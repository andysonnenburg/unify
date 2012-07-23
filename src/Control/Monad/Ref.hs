module Control.Monad.Ref
       ( module X
       , Ref
       , RefSupply
       , runRefSupply
       , RefSupplyT
       , runRefSupplyT
       ) where

import Control.Monad.Ref.Class as X
import Control.Monad.Trans.Ref.Int (Ref,
                                    RefSupply,
                                    runRefSupply,
                                    RefSupplyT,
                                    runRefSupplyT)