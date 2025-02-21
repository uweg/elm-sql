module Sql exposing
    ( Column
    , ColumnType
    , Default
    , DeleteQueryData
    , Direction(..)
    , InsertQueryData
    , JoinInfo
    , JoinType(..)
    , NoDefault
    , Operator(..)
    , Query
    , QueryInfo(..)
    , SelectQueryData
    , Table
    , UpdateQueryData
    , WhereInfo
    , column
    , delete
    , field
    , from
    , innerJoin
    , innerJoinMaybe
    , insert
    , insertColumn
    , insertDefault
    , leftJoin
    , leftJoinMaybe
    , maybeField
    , orderBy
    , select
    , table
    , update
    , updateField
    , where_
    )

import Json.Decode as D
import Json.Encode as E


andMap : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
andMap =
    D.map2 (|>)


type alias TableInfo t =
    { table : t
    , name : String
    , alias : Maybe String
    }


type Table current t ctor table
    = Table (TableInfo current)


type NormalTable
    = NormalTable


type MaybeTable
    = MaybeTable


type alias ColumnType t =
    { decoder : D.Decoder t
    , encode : t -> E.Value
    }


column :
    String
    -> ColumnType current
    -> Table (Column current default -> next) result ctor NormalTable
    -> Table next result ctor NormalTable
column name type_ (Table current) =
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


table : String -> ctor -> Table ctor t ctor NormalTable
table name ctor =
    { table = ctor
    , name = name
    , alias = Nothing
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
    | LessOrEquals
    | NotEquals


type alias JoinInfo =
    { table : String
    , alias : String
    , column : String
    , operator : Operator
    , table2 : String
    , column2 : String
    , type_ : JoinType
    }


type JoinType
    = InnerJoin
    | LeftJoin


type alias SelectInfo params t =
    { select : t
    , from : String
    , fromAlias : String
    , join : List JoinInfo
    , where_ : List (WhereInfo params)
    , order : List OrderInfo
    }


from :
    Table t t ctor NormalTable
    -> SelectInfo p (Table t t ctor NormalTable)
from (Table t) =
    { select = Table { t | alias = Just "f" }
    , from = t.name
    , fromAlias = "f"
    , join = []
    , where_ = []
    , order = []
    }


innerJoin :
    Table j j ctor NormalTable
    -> (j -> Column a defaultJ)
    -> Operator
    -> (t -> Table t_ t_ ctor_ NormalTable)
    -> (t_ -> Column a defaultT)
    -> SelectInfo p t
    -> SelectInfo p ( t, Table j j ctor NormalTable )
innerJoin (Table j) col operator toTable toColumn t =
    let
        (Table t2) =
            toTable t.select

        (Column c2) =
            toColumn t2.table

        i =
            List.length t.join

        alias =
            "j" ++ String.fromInt i
    in
    { select =
        ( t.select
        , Table { j | alias = Just alias }
        )
    , from = t.from
    , fromAlias = t.fromAlias
    , where_ = t.where_
    , order = t.order
    , join =
        { table = j.name
        , alias = alias
        , column = col j.table |> (\(Column info) -> info.name)
        , operator = operator
        , table2 = t2.alias |> Maybe.withDefault ""
        , column2 = c2.name
        , type_ = InnerJoin
        }
            :: t.join
    }


innerJoinMaybe :
    Table j j ctor NormalTable
    -> (j -> Column a defaultJ)
    -> Operator
    -> (t -> Table t_ t_ ctor_ NormalTable)
    -> (t_ -> Column (Maybe a) defaultT)
    -> SelectInfo p t
    -> SelectInfo p ( t, Table j j ctor NormalTable )
innerJoinMaybe (Table j) col operator toTable toColumn t =
    let
        (Table t2) =
            toTable t.select

        (Column c2) =
            toColumn t2.table

        i =
            List.length t.join

        alias =
            "j" ++ String.fromInt i
    in
    { select =
        ( t.select
        , Table { j | alias = Just alias }
        )
    , from = t.from
    , fromAlias = t.fromAlias
    , where_ = t.where_
    , order = t.order
    , join =
        { table = j.name
        , alias = alias
        , column = col j.table |> (\(Column info) -> info.name)
        , operator = operator
        , table2 = t2.alias |> Maybe.withDefault ""
        , column2 = c2.name
        , type_ = InnerJoin
        }
            :: t.join
    }


leftJoin :
    Table j j ctor NormalTable
    -> (j -> Column a defaultJ)
    -> Operator
    -> (t -> Table t_ t_ ctor_ joinTable)
    -> (t_ -> Column a defaultT)
    -> SelectInfo p t
    -> SelectInfo p ( t, Table j j ctor MaybeTable )
leftJoin (Table j) col operator toTable toColumn t =
    let
        (Table t2) =
            toTable t.select

        (Column c2) =
            toColumn t2.table

        i =
            List.length t.join

        alias =
            "j" ++ String.fromInt i
    in
    { select =
        ( t.select
        , Table { j | alias = Just alias }
        )
    , from = t.from
    , fromAlias = t.fromAlias
    , where_ = t.where_
    , order = t.order
    , join =
        { table = j.name
        , alias = alias
        , column = col j.table |> (\(Column info) -> info.name)
        , operator = operator
        , table2 = t2.alias |> Maybe.withDefault ""
        , column2 = c2.name
        , type_ = LeftJoin
        }
            :: t.join
    }


leftJoinMaybe :
    Table j j ctor NormalTable
    -> (j -> Column a defaultJ)
    -> Operator
    -> (t -> Table t_ t_ ctor_ joinTable)
    -> (t_ -> Column (Maybe a) defaultT)
    -> SelectInfo p t
    -> SelectInfo p ( t, Table j j ctor MaybeTable )
leftJoinMaybe (Table j) col operator toTable toColumn t =
    let
        (Table t2) =
            toTable t.select

        (Column c2) =
            toColumn t2.table

        i =
            List.length t.join

        alias =
            "j" ++ String.fromInt i
    in
    { select =
        ( t.select
        , Table { j | alias = Just alias }
        )
    , from = t.from
    , fromAlias = t.fromAlias
    , where_ = t.where_
    , order = t.order
    , join =
        { table = j.name
        , alias = alias
        , column = col j.table |> (\(Column info) -> info.name)
        , operator = operator
        , table2 = t2.alias |> Maybe.withDefault ""
        , column2 = c2.name
        , type_ = LeftJoin
        }
            :: t.join
    }


select :
    ctor
    -> (t -> Field ctor result -> Field result result)
    -> SelectInfo p t
    -> Query p result
select ctor toR s =
    let
        (Field f resultDecoder) =
            Field [] (D.succeed ctor) |> toR s.select

        info =
            { from = s.from
            , fromAlias = s.fromAlias
            , join = List.reverse s.join
            , select = List.reverse f
            , where_ = List.reverse s.where_
            , order = List.reverse s.order
            }
                |> SelectQuery
    in
    { info = info
    , resultDecoder = resultDecoder
    , encodeParams =
        \p ->
            s.where_
                |> List.map (\w -> ( w.param, w.encode p ))
                |> E.object
    }


type alias SelectQueryData params =
    { from : String
    , fromAlias : String
    , join : List JoinInfo
    , select : List FieldInfo
    , where_ : List (WhereInfo params)
    , order : List OrderInfo
    }


type alias Query params result =
    { info : QueryInfo params result
    , encodeParams : params -> E.Value
    , resultDecoder : D.Decoder result
    }


type QueryInfo params result
    = SelectQuery (SelectQueryData params)
    | UpdateQuery (UpdateQueryData params)
    | CreateQuery InsertQueryData
    | DeleteQuery (DeleteQueryData params)


type alias FieldInfo =
    { table : String
    , column : String
    , name : String
    }


type Field current result
    = Field (List FieldInfo) (D.Decoder current)


field :
    Table t t ctor NormalTable
    -> (t -> Column current default)
    -> Field (current -> next) result
    -> Field next result
field (Table t) toColumn (Field info decoder) =
    let
        i =
            List.length info

        (Column c) =
            toColumn t.table

        name =
            "r" ++ String.fromInt i

        f =
            { table = t.alias |> Maybe.withDefault ""
            , column = c.name
            , name = name
            }
                :: info

        d =
            decoder
                |> andMap (D.field name c.decoder)
    in
    Field f d


maybeField :
    Table t t ctor MaybeTable
    -> (t -> Column current default)
    -> Field (Maybe current -> next) result
    -> Field next result
maybeField (Table t) toColumn (Field info decoder) =
    let
        i =
            List.length info

        (Column c) =
            toColumn t.table

        name =
            "r" ++ String.fromInt i

        f =
            { table = t.alias |> Maybe.withDefault ""
            , column = c.name
            , name = name
            }
                :: info

        d =
            decoder
                |> andMap (D.field name (D.maybe c.decoder))
    in
    Field f d


type alias WhereInfo params =
    { table : Maybe String
    , column : String
    , operator : Operator
    , param : String
    , encode : params -> E.Value
    }


where_ :
    (t -> Table t_ t_ ctor NormalTable)
    -> (t_ -> Column a default)
    -> Operator
    -> (p -> a)
    -> { s | select : t, where_ : List (WhereInfo p) }
    -> { s | select : t, where_ : List (WhereInfo p) }
where_ toTable toColumn operator fromParam s =
    let
        (Table t) =
            toTable s.select

        (Column c) =
            toColumn t.table

        name =
            "p" ++ String.fromInt (List.length s.where_)
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


type alias OrderInfo =
    { table : String
    , column : String
    , direction : Direction
    }


type Direction
    = Asc
    | Desc


orderBy :
    (t -> Table t_ t_ ctor table)
    -> (t_ -> Column c default)
    -> Direction
    -> SelectInfo p t
    -> SelectInfo p t
orderBy toTable toColumn direction s =
    let
        (Table t) =
            toTable s.select

        (Column c) =
            toColumn t.table
    in
    { s
        | order =
            { table = t.alias |> Maybe.withDefault ""
            , column = c.name
            , direction = direction
            }
                :: s.order
    }


type Default
    = Default


type NoDefault
    = NoDefault


type alias UpdateInfo p =
    { encode : p -> E.Value
    , name : String
    , column : String
    }


type Update t p
    = Update (List (t -> UpdateInfo p))


type alias UpdateWhere t params =
    { select : t
    , where_ : List (WhereInfo params)
    }


update :
    Table t t ctor NormalTable
    -> (Update t params -> Update t params)
    ->
        (UpdateWhere (Table t t ctor NormalTable) params
         -> UpdateWhere (Table t t ctor NormalTable) params
        )
    -> Query params {}
update (Table t) u where__ =
    let
        (Update u_) =
            Update [] |> u

        u__ =
            u_ |> List.map (\u___ -> u___ t.table)

        where___ : List (WhereInfo params)
        where___ =
            where__
                { select = Table t
                , where_ = []
                }
                |> .where_
                |> List.reverse
    in
    { info =
        UpdateQuery
            { table = t.name
            , updates = u__
            , where_ = where___
            }
    , resultDecoder = D.succeed {}
    , encodeParams =
        \p_ ->
            (List.map (\w -> ( w.param, w.encode p_ )) where___
                ++ List.map (\u___ -> ( u___.name, u___.encode p_ )) u__
            )
                |> E.object
    }



{-
   \p ->

           |> E.object

-}


updateField :
    (t -> Column a default)
    -> (p -> a)
    -> Update t p
    -> Update t p
updateField toColumn p (Update u) =
    let
        name =
            "u" ++ (List.length u |> String.fromInt)
    in
    (\t ->
        let
            (Column c) =
                toColumn t
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
    , where_ : List (WhereInfo p)
    }


type alias InsertQueryData =
    { table : String
    , columns : List ( String, String )
    , output : String
    }


insert :
    Table t t ctor NormalTable
    -> (Insert params ctor t -> Insert params t t)
    -> (a -> result)
    -> (t -> Column a default)
    -> Query params result
insert (Table t) columns toOutput outputColumn =
    let
        (Insert c) =
            { columns = []
            , result = t.table
            }
                |> Insert
                |> columns

        (Column oc) =
            outputColumn t.table
    in
    { info =
        { table = t.name
        , columns =
            c.columns
                |> List.reverse
                |> List.map (\c_ -> ( c_.column, c_.parameter ))
        , output = oc.name
        }
            |> CreateQuery
    , encodeParams =
        \p ->
            c.columns
                |> List.map (\c_ -> ( c_.parameter, c_.encode p ))
                |> E.object
    , resultDecoder =
        D.field oc.name oc.decoder
            |> D.map toOutput
    }


type alias InsertColumn params =
    { column : String
    , parameter : String
    , encode : params -> E.Value
    }


type Insert params current result
    = Insert
        { result : result
        , columns : List (InsertColumn params)
        }


insertColumn :
    (result -> Column current default)
    -> (params -> current)
    -> Insert params (Column current default -> next) result
    -> Insert params next result
insertColumn toColumn param (Insert c) =
    let
        name =
            "p" ++ (List.length c.columns |> String.fromInt)

        (Column col) =
            toColumn c.result
    in
    Insert
        { columns =
            { parameter = name
            , encode = \p -> param p |> col.encode
            , column = col.name
            }
                :: c.columns
        , result = c.result
        }


insertDefault :
    (result -> Column current Default)
    -> Insert params (Column current Default -> next) result
    -> Insert params next result
insertDefault toColumn (Insert c) =
    Insert c


type alias DeleteQueryData p =
    { table : String
    , where_ : List (WhereInfo p)
    }


delete :
    Table t t ctor NormalTable
    -> (UpdateWhere (Table t t ctor NormalTable) params -> UpdateWhere (Table t t ctor NormalTable) params)
    -> Query params {}
delete (Table t) where__ =
    let
        where___ : List (WhereInfo params)
        where___ =
            where__
                { select = Table t
                , where_ = []
                }
                |> .where_
                |> List.reverse
    in
    { info =
        { table = t.name
        , where_ = where___
        }
            |> DeleteQuery
    , encodeParams =
        \pa ->
            List.map (\w -> ( w.param, w.encode pa )) where___
                |> E.object
    , resultDecoder = D.succeed {}
    }
