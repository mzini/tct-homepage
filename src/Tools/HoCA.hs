module Tools.HoCA where

import qualified Tct.Core.Data        as T
import Tct.Hoca.Config (hocaConfig, hocaDeclarations)
import Tools.TRS ()
import Tct.Hoca.Types

config = hocaConfig

instance T.Declared ML ML where
  decls = hocaDeclarations
