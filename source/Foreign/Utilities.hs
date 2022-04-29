module Foreign.Utilities (withWrapper) where

import Control.Monad.Cont
import Foreign

withWrapper :: (a -> IO (FunPtr a)) -> a -> ContT r IO (FunPtr a)
withWrapper wrapper f = ContT $ \ action -> do
    p <- wrapper f
    result <- action p
    freeHaskellFunPtr p
    return result
