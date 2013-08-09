-- Count \BV programs having a given size and number of operators.

nPrograms sizes size = nExps sizes (size - 1)

data Params = Size
  { nUnaryOps  :: Int  -- 0 .. 5
  , nBinaryOps :: Int  -- 0 .. 4
  , if0There   :: Bool
  , withinFold :: Bool
  , foldUsed   :: Bool
  }
    deriving Show

opt :: Num a => (Params -> Bool) -> Params -> a -> a
opt flag sizes sizeExp = if (flag sizes) then sizeExp else 0

nExps sizes 0 = 0
nExps sizes 1
  = 2 -- Constants
  + 1 -- Variable
  + opt withinFold sizes 2 -- Other variables
                
-- Should know whether we're in a fold
nExps sizes size
  = nUnaryOps sizes * (nExps sizes (size - 1))
  + nBinaryOps sizes * sum [nExps sizes i * nExps sizes (size - 1 - i) | i <- [1 .. size - 2]]
  + opt if0There sizes
       (sum
         [ nExps sizes condSize * nExps sizes thenSize * nExps sizes elseSize
         | condSize <- [ 1 .. size - 3 ]
         , thenSize <- [ 1 .. size - 2 - condSize ]
         , let elseSize = size - condSize - thenSize - 1 ])
  + opt (not . foldUsed) sizes
       (sum
         [ nExps sizesNoFold condSize * nExps sizesNoFold thenSize * nExps sizesInFold elseSize
         | condSize <- [ 1 .. size - 4 ]
         , thenSize <- [ 1 .. size - 3 - condSize ]
         , let elseSize = size - condSize - thenSize - 2 ])
       where
         sizesNoFold = sizes { foldUsed = True }
         sizesInFold = sizesNoFold { withinFold = True }

  -- nBinaryOps * 
  -- \sum_{i=1}^(size-2) nBinaryOps * nExps sizes i * nExps sizes (size - 1 - i)
