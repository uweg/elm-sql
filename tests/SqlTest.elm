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


type alias Person =
    { id : Sql.Column Int Sql.Default
    , name : Sql.Column String Sql.NoDefault
    , parent : Sql.Column Int Sql.NoDefault
    }


personTable =
    Sql.table "person" Person
        |> Sql.column "id" intColumn
        |> Sql.column "name" stringColumn
        |> Sql.column "parent" intColumn


type alias Result =
    { id : Int
    , name : String
    , parent : String
    }


suite : Test
suite =
    describe "SQL"
        [ test "select" <|
            \_ ->
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
        , test "left join" <|
            \_ ->
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
        , test "update" <|
            \_ ->
                let
                    query : Sql.Query { id : Int, name : String } {}
                    query =
                        Sql.update personTable
                            (Sql.updateField .name .name)
                            (Sql.where_ identity .id Sql.Equals .id)
                in
                query
                    |> Expect.all
                        [ MsSql.toString
                            >> Expect.equal """UPDATE [person] SET
[name]=@p0
WHERE [id]=@v"""
                        , .encodeParams
                            >> (\p -> p { id = 0, name = "foo" })
                            >> E.encode 2
                            >> Expect.equal """{
  "v": 0,
  "p0": "foo"
}"""
                        ]
        , test "create" <|
            \_ ->
                let
                    query : Sql.Query { name : String, parent : Int } Int
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
                            >> (\p -> p { name = "foo", parent = 1 })
                            >> E.encode 2
                            >> Expect.equal """{
  "p1": 1,
  "p0": "foo"
}"""
                        ]
        , test "delete" <|
            \_ ->
                let
                    query : Sql.Query Int {}
                    query =
                        Sql.delete personTable
                            .id
                            Sql.Equals
                            identity
                in
                query
                    |> Expect.all
                        [ MsSql.toString
                            >> Expect.equal """DELETE [person] WHERE [id]=@p"""
                        , .encodeParams
                            >> (\p -> p 1)
                            >> E.encode 2
                            >> Expect.equal """{
  "p": 1
}"""
                        ]
        ]
