{-
>>> putStrLn $ renderString $ layoutSmart defaultLayoutOptions $ prettyE sample
[ [ [ 0
    ]
  , [ 0 ]
  , [ 111111111111111111111111111111111111111111111111111111111111111111111111111
    ]
  ]
]

-}
module Lib where

import Numeric.Natural
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String -- for renderString

data E
  = L [E]
  | N Natural

sample :: E
sample =
  L [ L [ L [N 0]
        , L [N 0]
        , L [N 111111111111111111111111111111111111111111111111111111111111111111111111111]
        ]
    ]

prettyE :: E -> Doc a
prettyE (N n)  = pretty n
prettyE (L xs) = list' (map prettyE xs)

list' :: [Doc a] -> Doc a
list' docs =
    group
        (flatAlt
            (align docs'          <> rbracket)
            (      docs' <> space <> rbracket))
  where
    docs' =
        mconcat
            (zipWith
                combine
                (lbracket : repeat comma)
                docs)

    combine x y = x <> space <> y <> flatAlt hardline mempty
