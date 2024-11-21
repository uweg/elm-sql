module Sql exposing (..)

import Json.Decode as D
import Json.Decode.Extra as DE
import Json.Encode as E

type alias TableInfo t =
  { table : t
  , name : String
  , alias : String
  }

type Table current t = Table (TableInfo current)

type C result current = C current

type alias ColumnType t =
  { decoder : D.Decoder t
  , encode : t -> E.Value
  }

intColumn : ColumnType Int
intColumn =
  { decoder = D.int
  , encode = E.int
  }

stringColumn : ColumnType String
stringColumn =
  { decoder = D.string
  , encode = E.string
  }

column :
  String
  -> (result -> Column current default)
  -> ColumnType current
  -> Table (Column current default -> next) result
  -> Table next result
column name _ type_ (Table current) =
  let
    c : Column current default
    c =
      { name = name
      , decoder = type_.decoder
      , encode = type_.encode
      }
      |> Column
  in
  { name = current.name
  , alias = current.alias
  , table = current.table c
  }
  |> Table

table : String -> ctor -> Table ctor t
table name ctor =
  { table = ctor
  , name = name
  , alias = ""
  }
  |> Table

type alias ColumnInfo t =
  { name : String
  , decoder : D.Decoder t
  , encode : t -> E.Value
  }

type Column t default
 = Column (ColumnInfo t)

type Operator
  = Equals

type alias JoinInfo =
  { table : String
  , alias : String
  , column : String
  , operator : Operator
  , table2 : String
  , column2 : String
  }

type alias SelectInfo t params =
  { select : t
  , from : String
  , fromAlias : String
  , join : List JoinInfo
  , where_ : List (WhereInfo params)
  }

type Select p t = Select (SelectInfo t p)

from :
  Table t t
  -> Select p (Table t t)
from (Table t) =
  { select = Table {t | alias = "f"}
  , from = t.name
  , fromAlias = "f"
  , join = []
  , where_ = []
  }
  |> Select

innerJoin :
  Table j j
  -> (j -> Column a defaultJ)
  -> Operator
  -> (t -> Table t_ t_)
  -> (t_ -> Column a defaultT)
  -> Select p t
  -> Select p (t, Table j j)
innerJoin (Table j) ( col) operator toTable toColumn (Select t) =
  let
    (Table t2) = toTable t.select
    (Column c2) = toColumn t2.table
    i = List.length t.join
    alias = "j" ++ String.fromInt i
  in
  { select =
      ( t.select
      , Table {j | alias = alias}
      )
  , from = t.from
  , fromAlias = t.fromAlias
  , where_ = t.where_
  , join =
      { table = j.name
      , alias = alias
      , column = col j.table |> (\(Column info) -> info.name)
      , operator = operator
      , table2 = t2.alias
      , column2 = c2.name
      }
      :: t.join
  }
  |> Select

select :
  ctor
  -> (t -> Field ctor result -> Field result result)
  -> Select p t
  -> Query p result
select ctor toR (Select s) =
  let
    (Field f resultDecoder)
      = Field [] (D.succeed ctor) |> toR s.select

    info =
      { from = s.from
      , fromAlias = s.fromAlias
      , join = s.join
      , select = List.reverse f
      , where_ = List.reverse s.where_
      }
      |> SelectQuery
  in
  { info = info
  , resultDecoder = resultDecoder
  , encodeParams = \p ->
      s.where_
      |> List.map (\w -> (w.param, w.encode p))
      |> E.object
  }

type alias SelectQueryData params =
  { from : String
  , fromAlias : String
  , join : List JoinInfo
  , select : List FieldInfo
  , where_ : List (WhereInfo params)
  }

type alias Query params result =
  { info: QueryInfo params result
  , encodeParams : params -> E.Value
  , resultDecoder : D.Decoder result
  }

type QueryInfo params result
  = SelectQuery (SelectQueryData params)
  | UpdateQuery (UpdateQueryData params)
  | CreateQuery CreateQueryData
  | DeleteQuery DeleteQueryData

type alias FieldInfo =
  { table : String
  , column : String
  , name : String
  }

type Field current result
  = Field (List FieldInfo) (D.Decoder current)

field :
  Table t t
  -> (t -> Column current default)
  -> Field (current -> next) result
  -> Field next  result
field (Table t) toColumn (Field info decoder) =
  let
    i = List.length info

    (Column c) = toColumn t.table

    name = "r" ++ String.fromInt i

    f =
      { table = t.alias
      , column = c.name
      , name = name
      }
      :: info

    d =
      decoder
      |> DE.andMap (D.field name c.decoder)

  in
  Field f d

type alias WhereInfo params =
  { table : String
  , column : String
  , operator : Operator
  , param : String
  , encode : params -> E.Value
  }

where_ :
  (t -> Table t_ t_)
  -> (t_ -> Column a default)
  -> Operator
  -> (p -> a)
  -> Select p t
  -> Select p t
where_ toTable toColumn operator fromParam (Select s) =
  let
    (Table t) = toTable s.select
    (Column c) = toColumn t.table
    name = "p" ++ String.fromInt (List.length s.where_)
  in
  { s
  | where_ =
      { table = t.alias
      , column = c.name
      , operator = operator
      , param = name
      , encode = fromParam >> c.encode
      }
      :: s.where_
  }
  |> Select

operatorToString : Operator -> String
operatorToString operator =
  case operator of
    Equals -> "="

selectToString : SelectQueryData t -> String
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
      [] -> []
  ]
  |> List.map (List.map (String.join ""))
  |> List.concat
  |> String.join "\n"

toString : Query t r -> String
toString q =
  case q.info of
    SelectQuery info -> selectToString info
    UpdateQuery info -> updateToString info
    CreateQuery info -> createToString info
    DeleteQuery info -> deleteToString info

type Default = Default

type NoDefault = NoDefault

type alias Person =
  { id : Column Int Default
  , name : Column String NoDefault
  , parent : Column Int NoDefault
  }

personTable : Table Person Person
personTable =
  table "person" Person
  |> column "id" .id intColumn
  |> column "name" .name stringColumn
  |> column "parent" .parent intColumn

type alias TestResult =
  { id : Int
  , name : String
  , parent : String
  }

type alias TestParameter =
  { name : String
  }

query1 : Query TestParameter TestResult
query1 =
  from personTable
  |> innerJoin personTable .id Equals identity .parent
  |> where_ (\(person, _) -> person) .name Equals .name
  |> where_ (\(person, _) -> person) .name Equals .name
  |> select TestResult (\(person, parent) ->
      field person .id
      >> field person .name
      >> field parent .name
    )

test = toString query1


type alias UpdateInfo p =
  { encode : p -> E.Value
  , name : String
  , column : String
  }

type Update t p
  = Update (List (t -> UpdateInfo p))

update :
  Table t t
  -> (Update t params -> Update t params)
  -> (t -> Column a default)
  -> Operator
  -> (params -> a)
  -> Query params {}
update (Table t) u toColumn operator p =
  let
    (Update u_) = Update [] |> u
    u__ = u_ |> List.map (\u___ -> u___ t.table)
    (Column c) = toColumn t.table
  in
  { info =
      UpdateQuery
        { table = t.name
        , updates = u__
        , column = c.name
        , operator = operator
        }
  , resultDecoder = D.succeed {}
  , encodeParams = \p_ ->
      ("v", p p_ |> c.encode)
      :: (u__ |> List.map (\u___ -> (u___.name, u___.encode p_)))
      |> E.object
  }


updateField :
  (t -> Column a default) 
  -> (p -> a)
  -> Update t p 
  -> Update t p
updateField toColumn p (Update u) =
  let
    name = "p" ++ (List.length u |> String.fromInt)
  in
  (\t ->
    let
      (Column c) = toColumn t
    in
    { encode = \p_ -> p p_ |> c.encode
    , name = name
    , column = c.name
    }
  )
  :: u
  |> Update


type alias UpdateQueryData p =
  { table : String
  , updates : List (UpdateInfo p)
  , column : String
  , operator : Operator
  }

updateToString : UpdateQueryData p -> String
updateToString q =
  [ "UPDATE "
  , q.table
  , " SET\n"
  , q.updates
      |> List.map (\u ->
          [ "["
          , u.column
          , "]=@", u.name
          ]
          |> String.concat
        )
      |> String.join ",\n"
  , "\nWHERE "
  , q.column
  , operatorToString q.operator
  , "@v"
  ]
  |> String.concat

type alias UpdateParams =
  { id : Int
  , name : String
  }

testU =
  update personTable
    (updateField .name .name
    )
    .id
    Equals
    .id

type alias CreateQueryData =
  { table : String
  , columns : List (String, String)
  , output : String
  }

create :
  Table t t
  -> ctor
  -> (Create params ctor t -> Create params t t)
  -> (a -> result)
  -> (t -> Column a default)
  -> Query params result
create (Table t) ctor columns toOutput outputColumn =
  let
    (Create c) =
      { columns = []
      , result = t.table
      }
      |> Create
      |> columns
      
    (Column oc) = outputColumn t.table
  in
  { info =
      { table = t.name
      , columns =
          c.columns 
          |> List.reverse 
          |> List.map (\c_ -> (c_.column, c_.parameter))
      , output = oc.name
      }
      |> CreateQuery
  , encodeParams = \p -> 
      c.columns
      |> List.map (\c_ -> (c_.parameter, c_.encode p)) 
      |> E.object
  , resultDecoder =
      D.field oc.name oc.decoder 
      |> D.map toOutput
  }

type alias CreateColumn params =
  { column : String
  , parameter : String
  , encode : params -> E.Value
  }

type Create params current result
  = Create 
      { result : result
      , columns : List (CreateColumn params)
      }

createColumn :
  (result -> Column current NoDefault)
  -> (params -> current)
  -> Create params (Column current NoDefault -> next) result
  -> Create params next result
createColumn toColumn param (Create c) =
  let
    name = "p" ++ (List.length c.columns |> String.fromInt)
    (Column col) = toColumn c.result
  in
  Create
    { columns = 
        { parameter = name
        , encode = \p -> param p |> col.encode
        , column = col.name
        }
        :: c.columns
    , result = c.result
    }
    
createDefault :
  (result -> Column current Default)
  -> Create params (Column current Default -> next) result
  -> Create params next result
createDefault toColumn (Create c) =
  Create c

createToString : CreateQueryData -> String
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

type alias CreateOutput =
  { id : Int
  }

testC =
  create personTable Person (
    createDefault .id
    >> createColumn .name .name
    >> createColumn .parent .parent
  )
  CreateOutput
  .id
  |> toString
  
type alias DeleteQueryData =
  { table : String
  , column : String
  , operator : Operator
  }
  
deleteToString : DeleteQueryData -> String
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
  
delete : 
  Table t t
  -> (t -> Column a default)
  -> Operator
  -> (params -> a)
  -> Query params {}
delete (Table t) c o p =
  let
    (Column col) = c t.table
  in
  { info =
      { table = t.name
      , column = col.name
      , operator = o
      }
      |> DeleteQuery
  , encodeParams = \pa ->
      [ ("p", p pa |> col.encode)
      ] 
      |> E.object
  , resultDecoder = D.succeed {}
  }
  
testD =
  delete personTable
    .id
    Equals
    .id
  |> toString