{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad ((>=>), void,forM, msum, filterM)
import           Data.Monoid ((<>))
import Data.String (fromString)
import System.FilePath.Posix ((</>),takeBaseName)
import           Hakyll
import qualified Text.Pandoc as Pandoc
import System.Directory (getDirectoryContents)
import qualified Tct.Core.Main as TcT
import qualified Tct.Core.Data as TcT
import Tct.Hoca.Config (hocaConfig)
import Data.Maybe (fromMaybe,isNothing)

config :: Configuration
config = defaultConfiguration

----------------------------------------------------------------------
-- utils

withDefault :: Context String -> Item String -> Compiler (Item String)
withDefault ctx = 
  loadAndApplyTemplate "templates/content.html" ctx
  >=> loadAndApplyTemplate "templates/default.html" ctx
  -- >=> relativizeUrls

withDefaultPHP :: Context String -> Item String -> Compiler (Item String)
withDefaultPHP ctx =
  withDefault ctx
  >=> loadAndApplyTemplate "templates/php_session.php" ctx

----------------------------------------------------------------------
-- tools

data Tool a = Tool { tctConfig   :: TcT.TctConfig a
                   , toolName    :: String
                   , defaultInput :: FilePath }
              

hoca = Tool { tctConfig = hocaConfig, toolName = "HoCA", defaultInput = "rev-dl.fp" }

----------------------------------------------------------------------
-- examples

exampleDir :: Tool a -> FilePath
exampleDir tool = "examples" </> toolName tool

getExamples :: Tool a -> Compiler [Item CopyFile]
getExamples tool = loadAll (fromString (exampleDir tool </> "*"))

exampleCtx :: Context b
exampleCtx =
  field "name" (\ item -> return $ takeBaseName $ toFilePath $ itemIdentifier item)
  <> field "filepath" (\ item -> return $ toFilePath (itemIdentifier item))


----------------------------------------------------------------------
-- strategies

strategyDeclarations :: Tool a -> [TcT.StrategyDeclaration a a]
strategyDeclarations tool = defaultDecl : TcT.strategies (tctConfig tool) where
  defaultDecl = TcT.SD (TcT.strategy "default" () (TcT.defaultStrategy (tctConfig tool)))

-- strategyId :: Tool a -> TcT.StrategyDeclaration a a -> Identifier
-- strategyId tool (TcT.SD sd) = fromFilePath (toolName tool </> TcT.declName sd)

-- strategyIds :: Tool a -> [Identifier]
-- strategyIds tool = strategyId tool `map` strategyDeclarations tool

getStrategies :: Tool a -> Compiler [Item (TcT.StrategyDeclaration a a)]
getStrategies tool = return [Item (strategyId decl) decl | decl <- strategyDeclarations tool] where
  strategyId (TcT.SD sd) = fromFilePath ("strategy" </> TcT.declName sd)

-- data ArgType = ArgString | ArgStringOpt | ArgNat | ArgNatOpt

type ArgInfo = (String,String,[String],Maybe String)

argInfoCtx :: Context ArgInfo 
argInfoCtx =
  field "name" (return . name . itemBody)
  <> field "type" (return . tpe . itemBody)
  <> field "help" (return . help . itemBody)
  <> field "default" (return . def . itemBody)
  <> boolField "isOptional" (opt . itemBody)
  where
    name (n,_,_,_) = n
    tpe  (_,t,_,_) = t
    help (_,_,hlp,_) = concat hlp
    def  (_,_,_,md) = fromMaybe "" md
    opt  (_,_,_,md) = isNothing md


getArgInfos :: Item (TcT.StrategyDeclaration a a) -> Compiler [Item ArgInfo]
getArgInfos = return . toItem . argsInfo . itemBody where
  argsInfo (TcT.SD decl) = (TcT.declName decl, TcT.argsInfo (TcT.declArgs decl))
  toItem (declName,as) = [Item (fromFilePath ("option" </>  declName </> name)) a | a@(name,_,_,_) <- as ]


strategyCtx :: Context (TcT.StrategyDeclaration a a)
strategyCtx =
  field "id" (return . toFilePath . itemIdentifier)
  <> field "name" (return . name . itemBody)
  <> listFieldWith "options" argInfoCtx getArgInfos
  where name (TcT.SD sd) = TcT.declName sd


----------------------------------------------------------------------
-- tool

toolContext :: Tool a -> Context String
toolContext tool = 
  titleField (toolName tool ++ " web-interface")
  <> constField "defaultInput" (exampleDir tool </> defaultInput tool)
  <> listField  "examples"     exampleCtx (getExamples tool)
  <> listField  "strategies"   strategyCtx (getStrategies tool)
  <> defaultContext

----------------------------------------------------------------------
-- main rule for webif.php

webIF :: Tool a -> Rules ()
webIF tool = version name $ do
  route (constRoute name)
  compile $ do
    getResourceBody >>= applyAsTemplate ctx >>= withDefaultPHP ctx
    where
      ctx = toolContext tool
      name = toolName tool ++ ".php"

----------------------------------------------------------------------
-- main

main :: IO ()
main = hakyllWith config $ do
  
    match "javascript/*" $ route idRoute

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "examples/*/*" $ do
      route idRoute
      compile copyFileCompiler

    match "templates/*" $ compile templateCompiler

    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler >>= withDefault defaultContext

    match "webif.php" $ webIF hoca
