{-
>>> putStrLn $ renderString $ layoutSmart defaultLayoutOptions $ prettyE sample
[ [ [ 0 ]
  , [0]
  , [ 111111111111111111111111111111111111111111111111111111111111111111111111111 ] ] ]

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
prettyE (L xs) = align (list (map prettyE xs))
