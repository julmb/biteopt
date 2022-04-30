module Foreign.Utilities (Wrapper, withWrapper) where

import Control.Exception
import Control.Monad.Cont
import Foreign

type Wrapper a = a -> IO (FunPtr a)

withWrapper :: IO (FunPtr a) -> ContT r IO (FunPtr a)
withWrapper = lift
