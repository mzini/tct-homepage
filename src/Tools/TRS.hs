module Tools.TRS where

import qualified Tct.Core.Data        as T
import Tct.Trs.Declarations (trsDeclarations)
import Tct.Trs.Data.Problem (TrsProblem)
import Tct.Trs.Config (trsConfig)

config = trsConfig

instance T.Declared TrsProblem TrsProblem where
  decls = trsDeclarations
