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
import Data.Text.Prettyprint.Doc.Render.String

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
list'   [] = lbracket <> rbracket
list' docs =
    enclose'
        (lbracket <> space)
        (lbracket <> space)
        (comma <> space)
        (comma <> space)
        (space <> rbracket)
        rbracket
        docs

{-| Format an expression that holds a variable number of elements, such as a
    list, record, or union
-}
enclose'
    :: Doc ann
    -- ^ Beginning document for compact representation
    -> Doc ann
    -- ^ Beginning document for multi-line representation
    -> Doc ann
    -- ^ Separator for compact representation
    -> Doc ann
    -- ^ Separator for multi-line representation
    -> Doc ann
    -- ^ Ending document for compact representation
    -> Doc ann
    -- ^ Ending document for multi-line representation
    -> [Doc ann]
    -- ^ Elements to format
    -> Doc ann
enclose' beginShort beginLong sepShort sepLong endShort endLong docs =
    group
        (flatAlt
            (align
                (mconcat (zipWith combineLong (beginLong : repeat sepLong) docs) <> endLong)
            )
            (mconcat (zipWith combineShort (beginShort : repeat sepShort) docs) <> endShort)
        )
  where
    combineLong x y = x <> y <> hardline

    combineShort x y = x <> y
