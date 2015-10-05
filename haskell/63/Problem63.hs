f = [ v
    | b <- [1..10]
    , p <- [1..30]
    , let v = b ^ p
    , (length . show $ v) == p
    ]

problem63 = length f
