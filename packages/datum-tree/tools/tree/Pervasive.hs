
module Pervasive where


strPervasive :: String
strPervasive
        = unlines
        [ -- Application, which implements the '$' operator.
          "apply  f x           = f x" 

          -- Reverse application, which implements the '&' operator.
        , "applyr x f           = f x" 

          -- Reverse function composition
        , "composer f g x       = g (f x)"

          -- Aliases for primitive operators.
        , "neg                  = neg#"
        , "add                  = add#"
        , "sub                  = sub#"
        , "mul                  = mul#"
        , "div                  = div#"
        , "eq                   = eq#"
        , "gt                   = gt#"
        , "ge                   = ge#"
        , "lt                   = lt#"
        , "le                   = le#"

        -- Date operators.
        , "date-pack            = date-pack#"
        , "date-year            = date-year#"
        , "date-month           = date-month#"
        , "date-day             = date-day#"
        , "date-next            = date-next#"
        , "date-diff            = date-diff#"

        -- File system operators.
        , "load                 = load#"
        , "store                = store#"
        , "read                 = read#"

        -- Console operators.
        , "print                = print#"

        -- Pure tree operators.
        , "at                   = at#"
        , "append               = append#"
        , "argument             = argument#"
        , "concat               = concat#"
        , "count-as-field       = count-as-field#"
        , "drop-dim             = drop-dim#"
        , "dup-dim              = dup-dim#"
        , "final                = final#"
        , "filter-keys          = filter-keys#"
        , "flatten              = flatten#"
        , "gather               = gather#"
        , "group                = group#"
        , "initial              = initial#"
        , "map-keys             = map-keys#"
        , "on                   = on#"
        , "permute-fields       = permute-fields#"
        , "push-dim             = push-dim#"
        , "rename-fields        = rename-fields#"
        , "rename-dimension     = rename-dimension#"
        , "sample               = sample#"
        , "sortby-field         = sortby-field#"

        ]
