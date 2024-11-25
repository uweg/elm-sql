module Sql.MsSql exposing (toString)

import Sql

toString : Sql.Query t r -> String
toString q =
  case q.info of
    Sql.SelectQuery info -> selectToString info
    Sql.UpdateQuery info -> updateToString info
    Sql.CreateQuery info -> createToString info
    Sql.DeleteQuery info -> deleteToString info

deleteToString : Sql.DeleteQueryData -> String
deleteToString data =
  [ "DELETE ["
  , data.table
  , "] WHERE ["
  , data.column
  , "]"
  , operatorToString data.operator
  , "@p"
  ]
  |> String.concat

updateToString : Sql.UpdateQueryData p -> String
updateToString q =
  [ "UPDATE ["
  , q.table
  , "] SET\n"
  , q.updates
      |> List.map (\u ->
          [ "["
          , u.column
          , "]=@", u.name
          ]
          |> String.concat
        )
      |> String.join ",\n"
  , "\nWHERE ["
  , q.column
  , "]"
  , operatorToString q.operator
  , "@v"
  ]
  |> String.concat

createToString : Sql.CreateQueryData -> String
createToString q =
  [ "INSERT ["
  , q.table
  , "] ("
  , q.columns |> List.map (\(c, _) -> "[" ++ c ++ "]") |> String.join ","
  , ")\n"
  , "OUTPUT INSERTED.["
  , q.output
  , "]\n"
  , "VALUES ("
  , q.columns
      |> List.map (\(_, p) -> "@" ++ p)
      |> String.join ", "
  , ")"
  ]
  |> String.concat


operatorToString : Sql.Operator -> String
operatorToString operator =
  case operator of
    Sql.Equals -> "="

selectToString : Sql.SelectQueryData t -> String
selectToString q =
  [ [ [ "SELECT" ] ]
  , q.select
    |> List.map (\s ->
      [ s.table
      , ".["
      , s.column
      , "] "
      , s.name
      ]
      |> String.join ""
    )
    |> String.join ",\n"
    |> List.singleton
    |> List.singleton
  , [ [ "FROM [", q.from, "] ", q.fromAlias ] ]
  , q.join
    |> List.map (\j ->
      [ "INNER JOIN ["
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
    )
  , case q.where_ of
      [] -> []
      first :: rest ->
        let
          toW a =
            [ a.table
            , ".["
            , a.column
            , "]"
            , operatorToString a.operator
            , "@"
            , a.param
            ]
        in
        ("WHERE " :: toW first)
        :: List.map (\i -> "AND " :: toW i) rest
  , case q.order of
      [] -> []
      order ->
        [ "ORDER BY "
        , (order 
            |> List.map (\o -> o.table ++ ".[" ++ o.column ++ "]") 
            |> String.join ", "
          )
        ]
        |> List.singleton
  ]
  |> List.map (List.map (String.join ""))
  |> List.concat
  |> String.join "\n"