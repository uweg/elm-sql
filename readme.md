# elm-sql

This is an Elm package to work with SQL. It allows to create a model and type-checked queries from it. Those queries then can be serialized to a `String`. Right now, it is just a draft and needs some work to be published as an Elm package.

Schema

```elm
type alias Person =
    { id : Sql.Column Int Sql.Default
    , name : Sql.Column String Sql.NoDefault
    , parent : Sql.Column Int Sql.NoDefault
    }


personTable =
    Sql.table "person" Person
        |> Sql.column "id" .id intColumn
        |> Sql.column "name" .name stringColumn
        |> Sql.column "parent" .parent intColumn

```

Query

```elm
type alias Params =
    { name : String
    }

type alias Result =
    { id : Int
    , name : String
    , parent : String
    }

query : Sql.Query Params Result
query =
Sql.from personTable
    |> Sql.innerJoin personTable .id Sql.Equals identity .parent
    |> Sql.where_ (\( person, _ ) -> person) .name Sql.Equals .name
    |> Sql.where_ (\( person, _ ) -> person) .name Sql.Equals .name
    |> Sql.order (\( person, _ ) -> person) .name
    |> Sql.select Result
        (\( person, parent ) ->
            Sql.field person .id
                >> Sql.field person .name
                >> Sql.field parent .name
        )
```

toString

```elm
MsSql.toString query
    |> Expect.equal """SELECT
f.[id] r0,
f.[name] r1,
j0.[name] r2
FROM [person] f
INNER JOIN [person] j0 ON j0.id=f.[parent]
WHERE f.[name]=@p0
AND f.[name]=@p1
ORDER BY f.[name]"""
```