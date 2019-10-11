{-
>>> putStrLn $ render' $ prettyE sample
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
        (map (\x -> (x, x)) docs)

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
    -> [(Doc ann, Doc ann)]
    -- ^ Elements to format, each of which is a pair: @(compact, multi-line)@
    -> Doc ann
enclose' beginShort _         _        _       endShort _       []   =
    beginShort <> endShort
  where
enclose' beginShort beginLong sepShort sepLong endShort endLong docs =
    group
        (flatAlt
            (align
                (mconcat (zipWith combineLong (beginLong : repeat sepLong) docsLong) <> endLong)
            )
            (mconcat (zipWith combineShort (beginShort : repeat sepShort) docsShort) <> endShort)
        )
  where
    docsShort = fmap fst docs

    docsLong = fmap snd docs

    combineLong x y = x <> y <> hardline

    combineShort x y = x <> y

layoutOpts :: LayoutOptions
layoutOpts =
    defaultLayoutOptions
        { layoutPageWidth = AvailablePerLine 80 1.0 }

render' :: Doc a -> String
render' = renderString . layoutSmart layoutOpts
