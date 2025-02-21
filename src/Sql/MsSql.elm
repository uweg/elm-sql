module Sql.MsSql exposing (toString)

import Sql


toString : Sql.Query t r -> String
toString q =
    case q.info of
        Sql.SelectQuery info ->
            selectToString info

        Sql.UpdateQuery info ->
            updateToString info

        Sql.CreateQuery info ->
            createToString info

        Sql.DeleteQuery info ->
            deleteToString info


deleteToString : Sql.DeleteQueryData p -> String
deleteToString data =
    [ "DELETE ["
    , data.table
    , "]"
    , whereToString data.where_
        |> List.map String.concat
        |> List.concatMap (\s -> [ "\n", s ])
        |> String.concat
    ]
        |> String.concat


updateToString : Sql.UpdateQueryData p -> String
updateToString q =
    [ "UPDATE ["
    , q.table
    , "] SET\n"
    , q.updates
        |> List.map
            (\u ->
                [ "["
                , u.column
                , "]=@"
                , u.name
                ]
                    |> String.concat
            )
        |> String.join ",\n"
    , whereToString q.where_
        |> List.map String.concat
        |> List.concatMap (\s -> [ "\n", s ])
        |> String.concat
    ]
        |> String.concat


createToString : Sql.InsertQueryData -> String
createToString q =
    [ "INSERT ["
    , q.table
    , "] ("
    , q.columns
        |> List.map (\( c, _ ) -> "[" ++ c ++ "]")
        |> String.join ","
    , ")\n"
    , "OUTPUT INSERTED.["
    , q.output
    , "]\n"
    , "VALUES ("
    , q.columns
        |> List.map (\( _, p ) -> "@" ++ p)
        |> String.join ", "
    , ")"
    ]
        |> String.concat


operatorToString : Sql.Operator -> String
operatorToString operator =
    case operator of
        Sql.Equals ->
            "="

        Sql.LessOrEquals ->
            "<="

        Sql.NotEquals ->
            "<>"


selectToString : Sql.SelectQueryData t -> String
selectToString q =
    [ [ [ "SELECT" ] ]
    , q.select
        |> List.map
            (\s ->
                [ s.table, ".[", s.column, "] ", s.name ]
                    |> String.concat
            )
        |> String.join ",\n"
        |> List.singleton
        |> List.singleton
    , [ [ "FROM [", q.from, "] ", q.fromAlias ] ]
    , q.join |> List.map joinToString
    , whereToString q.where_
    , case q.order of
        [] ->
            []

        order ->
            [ "ORDER BY "
            , order
                |> List.map
                    (\o ->
                        o.table ++ ".[" ++ o.column ++ "] " ++ directionToString o.direction
                    )
                |> String.join ", "
            ]
                |> List.singleton
    ]
        |> List.concatMap (List.map String.concat)
        |> String.join "\n"


toW : Sql.WhereInfo p -> List String
toW a =
    [ a.table
        |> Maybe.map (\t -> t ++ ".")
        |> Maybe.withDefault ""
    , "["
    , a.column
    , "]"
    , operatorToString a.operator
    , "@"
    , a.param
    ]


whereToString : List (Sql.WhereInfo p) -> List (List String)
whereToString where_ =
    case where_ of
        [] ->
            []

        first :: rest ->
            ("WHERE " :: toW first)
                :: List.map (\i -> "AND " :: toW i) rest


directionToString : Sql.Direction -> String
directionToString direction =
    case direction of
        Sql.Asc ->
            "ASC"

        Sql.Desc ->
            "DESC"


joinToString : Sql.JoinInfo -> List String
joinToString j =
    [ joinTypeToString j.type_
    , " JOIN ["
    , j.table
    , "] "
    , j.alias
    , " ON "
    , j.alias
    , "."
    , j.column
    , operatorToString j.operator
    , j.table2
    , ".["
    , j.column2
    , "]"
    ]


joinTypeToString : Sql.JoinType -> String
joinTypeToString t =
    case t of
        Sql.InnerJoin ->
            "INNER"

        Sql.LeftJoin ->
            "LEFT"
