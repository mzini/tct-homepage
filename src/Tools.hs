module Tools where

import qualified Tct.Core.Data as TcT
import qualified Tct.Core.Main as TcT
import qualified Tools.HoCA as HoCA
import qualified Tools.TRS as TRS

data Tool a = Tool { tctConfig   :: TcT.TctConfig a
                   , toolName    :: String
                   , defaultInput :: FilePath }

data SomeTool where
  SomeTool :: TcT.Declared a a => Tool a -> SomeTool

tools :: [SomeTool]
tools = [ SomeTool $ Tool { tctConfig = HoCA.config, toolName = "HoCA", defaultInput = "rev-dl.fp" }
        , SomeTool $ Tool { tctConfig = TRS.config,  toolName = "TRS",  defaultInput = "square.trs" } ]
