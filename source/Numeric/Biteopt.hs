module Numeric.Biteopt where

import Foreign
import Foreign.C

type Objective = CInt -> Ptr CDouble -> Ptr () -> IO CDouble
type RNG = Ptr () -> IO CUInt

foreign import ccall "wrapper" mkObjective :: Objective -> IO (FunPtr Objective)
foreign import ccall "minimize" minimize ::
    CInt -> FunPtr Objective -> Ptr () ->
    Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
    CInt -> CInt -> CInt ->
    CInt -> FunPtr RNG -> Ptr () -> IO CInt
