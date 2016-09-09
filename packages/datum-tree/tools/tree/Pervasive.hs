
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

        , "load                 = load#"
        , "store                = store#"
        , "read                 = read#"

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
        , "print                = print#"
        , "rename-fields        = rename-fields#"
        , "rename-dimension     = rename-dimension#"
        , "sample               = sample#"
        , "sortby-field         = sortby-field#"
        ]
