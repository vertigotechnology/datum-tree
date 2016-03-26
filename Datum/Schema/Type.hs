
module Datum.Schema.Type 
        ( Shape (..)
        , TType (..)
        , PType (..)
        , Name) 
where


-- | Dimensions.
data Shape
        = Shape 
                Name            -- Name of this dimension.
                TType           -- Key type.
                [Shape]         -- Sub dimensions.
        deriving Show


-- | Named tuple types.
data TType
        = TType [(Name, PType)]
        deriving Show


-- | Primitive types.
data PType
        = PUnit
        | PBool
        | PInt
        | PFloat
        | PNat
        | PDecimal
        | PText
        | PTime
        deriving Show


-- | Field and dimension names.
type Name
        = String


exShares :: Shape
exShares  
 = Shape "root" 
        (TType [])
        [ Shape "company" 
                (TType  [ ("symbol", PText)
                        , ("name",   PText) ])

                [ Shape "transaction" 
                        (TType  [ ("price",     PDecimal)
                                , ("volume",    PNat)
                                , ("time",      PTime) ])
                        []

                , Shape "office"
                        (TType  [ ("address",   PText)])
                        []
                ]
        ]



