module SqlTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as D
import Json.Encode as E
import Sql
import Sql.MsSql as MsSql
import Test exposing (..)


intColumn : Sql.ColumnType Int
intColumn =
    { decoder = D.int
    , encode = E.int
    }


stringColumn : Sql.ColumnType String
stringColumn =
    { decoder = D.string
    , encode = E.string
    }


idColumn : (Int -> id) -> (id -> Int) -> Sql.ColumnType id
idColumn toId fromId =
    { decoder = D.int |> D.map toId
    , encode = fromId >> E.int
    }


type PersonId
    = PersonId Int


personIdColumn : Sql.ColumnType PersonId
personIdColumn =
    idColumn PersonId (\(PersonId id) -> id)


type alias Person =
    { id : Sql.Column PersonId Sql.Default
    , name : Sql.Column String Sql.NoDefault
    , parent : Sql.Column PersonId Sql.NoDefault
    }


personTable =
    Sql.table "person" Person
        |> Sql.column "id" personIdColumn
        |> Sql.column "name" stringColumn
        |> Sql.column "parent" personIdColumn


type alias Result =
    { id : PersonId
    , name : String
    , parent : String
    }


suite : Test
suite =
    describe "SQL"
        [ test "select" <| \_ -> testSelect
        , test "left join" <| \_ -> testLeftJoin
        , test "multiple join" testMultipleJoin
        , test "update" <| \_ -> testUpdate
        , test "update with multiple wheres" <| \_ -> testUpdateMultipleWhere
        , test "create" <| \_ -> testCreate
        , test "delete" <| \_ -> testDelete
        ]


testSelect : Expectation
testSelect =
    let
        query : Sql.Query { name : String } Result
        query =
            Sql.from personTable
                |> Sql.innerJoin personTable .id Sql.Equals identity .parent
                |> Sql.where_ (\( person, _ ) -> person) .name Sql.Equals .name
                |> Sql.where_ (\( person, _ ) -> person) .name Sql.Equals .name
                |> Sql.orderBy (\( person, _ ) -> person) .name Sql.Asc
                |> Sql.select Result
                    (\( person, parent ) ->
                        Sql.field person .id
                            >> Sql.field person .name
                            >> Sql.field parent .name
                    )
    in
    query
        |> Expect.all
            [ MsSql.toString
                >> Expect.equal """SELECT
f.[id] r0,
f.[name] r1,
j0.[name] r2
FROM [person] f
INNER JOIN [person] j0 ON j0.id=f.[parent]
WHERE f.[name]=@p0
AND f.[name]=@p1
ORDER BY f.[name] ASC"""
            , .encodeParams
                >> (\p -> p { name = "foo" })
                >> E.encode 2
                >> Expect.equal """{
  "p1": "foo",
  "p0": "foo"
}"""
            ]


testLeftJoin : Expectation
testLeftJoin =
    let
        query : Sql.Query {} (Maybe String)
        query =
            Sql.from personTable
                |> Sql.leftJoin personTable .id Sql.Equals identity .parent
                |> Sql.select identity
                    (\( person, parent ) ->
                        Sql.maybeField parent .name
                    )
    in
    query
        |> Expect.all
            [ MsSql.toString
                >> Expect.equal """SELECT
j0.[name] r0
FROM [person] f
LEFT JOIN [person] j0 ON j0.id=f.[parent]"""
            , .encodeParams
                >> (\p -> p {})
                >> E.encode 2
                >> Expect.equal "{}"
            ]


testMultipleJoin : () -> Expectation
testMultipleJoin _ =
    let
        query : Sql.Query {} PersonId
        query =
            Sql.from personTable
                |> Sql.innerJoin personTable .id Sql.Equals identity .parent
                |> Sql.innerJoin personTable .id Sql.Equals (\( p, p1 ) -> p1) .parent
                |> Sql.select identity
                    (\( ( p, p1 ), p2 ) ->
                        Sql.field p2 .id
                    )
    in
    query
        |> Expect.all
            [ MsSql.toString
                >> Expect.equal """SELECT
j1.[id] r0
FROM [person] f
INNER JOIN [person] j0 ON j0.id=f.[parent]
INNER JOIN [person] j1 ON j1.id=j0.[parent]"""
            , .encodeParams
                >> (\p -> p {})
                >> E.encode 2
                >> Expect.equal "{}"
            ]


testUpdate : Expectation
testUpdate =
    let
        query : Sql.Query { id : PersonId, name : String } {}
        query =
            Sql.update personTable
                (Sql.updateField .name .name)
                (Sql.where_ identity .id Sql.Equals .id)
    in
    query
        |> Expect.all
            [ MsSql.toString
                >> Expect.equal """UPDATE [person] SET
[name]=@u0
WHERE [id]=@p0"""
            , .encodeParams
                >> (\p -> p { id = PersonId 0, name = "foo" })
                >> E.encode 2
                >> Expect.equal """{
  "p0": 0,
  "u0": "foo"
}"""
            ]


testUpdateMultipleWhere : Expectation
testUpdateMultipleWhere =
    let
        query : Sql.Query { id : PersonId, parent : PersonId, name : String } {}
        query =
            Sql.update personTable
                (Sql.updateField .name .name)
                (Sql.where_ identity .id Sql.Equals .id
                    >> Sql.where_ identity .parent Sql.Equals .parent
                )
    in
    query
        |> Expect.all
            [ MsSql.toString
                >> Expect.equal """UPDATE [person] SET
[name]=@u0
WHERE [id]=@p0
AND [parent]=@p1"""
            , .encodeParams
                >> (\p -> p { id = PersonId 0, parent = PersonId 2, name = "foo" })
                >> E.encode 2
                >> Expect.equal """{
  "p0": 0,
  "p1": 2,
  "u0": "foo"
}"""
            ]


testCreate : Expectation
testCreate =
    let
        query : Sql.Query { name : String, parent : PersonId } PersonId
        query =
            Sql.insert personTable
                (Sql.insertDefault .id
                    >> Sql.insertColumn .name .name
                    >> Sql.insertColumn .parent .parent
                )
                identity
                .id
    in
    query
        |> Expect.all
            [ MsSql.toString
                >> Expect.equal """INSERT [person] ([name],[parent])
OUTPUT INSERTED.[id]
VALUES (@p0, @p1)"""
            , .encodeParams
                >> (\p -> p { name = "foo", parent = PersonId 1 })
                >> E.encode 2
                >> Expect.equal """{
  "p1": 1,
  "p0": "foo"
}"""
            ]


testDelete : Expectation
testDelete =
    let
        query : Sql.Query PersonId {}
        query =
            Sql.delete personTable
                (Sql.where_ identity .id Sql.Equals identity)
    in
    query
        |> Expect.all
            [ MsSql.toString
                >> Expect.equal """DELETE [person]
WHERE [id]=@p0"""
            , .encodeParams
                >> (\p -> p (PersonId 1))
                >> E.encode 2
                >> Expect.equal """{
  "p0": 1
}"""
            ]
