
module Datum.Schema.Type 
        ( Path          (..)
        , Ix            (..)
        , Shape         (..)
        , TypeTup       (..)
        , TypePrim      (..)
        , Name) 
where

-- | Path to a particular dimension.
type Path 
        = [Ix]

data Ix
        = IxSub   Name
        | IxField Name
        | IxElem  Int
        deriving Show
        

-- | Dimensions.
data Shape
        = TS    Name            -- Name of this dimension.
                TypeTup         -- Key type.
                [Shape]         -- Sub dimensions.
        deriving Show


-- | Named tuple types.
data TypeTup
        = TT [(Name, TypePrim)]
        deriving Show


-- | Primitive types.
data TypePrim
        = TPUnit
        | TPBool
        | TPInt
        | TPFloat
        | TPNat
        | TPDecimal
        | TPText
        | TPTime
        deriving Show


-- | Field and dimension names.
type Name
        = String


exShares :: Shape
exShares  
 = TS "" 
        (TT [])
        [ TS "company" 
                (TT [ ("symbol",        TPText)
                    , ("name",          TPText) ])

                [ TS "transaction" 
                        (TT [ ("price",         TPDecimal)
                            , ("volume",        TPNat)
                            , ("time",          TPTime) ])
                        []

                , TS "office"
                        (TT  [ ("address",      TPText)])
                        []
                ]
        ]



