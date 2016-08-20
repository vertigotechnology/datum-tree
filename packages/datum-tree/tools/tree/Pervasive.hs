
module Pervasive where


strPervasive :: String
strPervasive
        = unlines
        [ -- Application, which implements the '$' operator.
          "apply  f x = f x;" 

          -- Reverse application, which implements the '&' operator.
        , "applyr x f = f x;" 

          -- Reverse function composition
          -- TODO: there is a name clash somewhere in interpreter, 
          -- maybe merged environments in the wrong way.
        , "composer f g x  = g (f x);"

          -- Aliases for primitive operators.
        , "neg          = neg#;"
        , "add          = add#;"
        , "sub          = sub#;"
        , "mul          = mul#;"
        , "div          = div#;"

        , "eq           = eq#;"
        , "gt           = gt#;"
        , "ge           = ge#;"
        , "lt           = lt#;"
        , "le           = le#;"

        , "argument             = argument#;"
        , "load                 = load#;"
        , "store                = store#;"
        , "sample               = sample#;"
        , "group                = group#;"
        , "gather               = gather#;"
        , "rename-fields        = rename-fields#;"
        , "permute-fields       = permute-fields#;"
        , "flatten              = flatten#;"
        , "on                   = on#;"
        ]
