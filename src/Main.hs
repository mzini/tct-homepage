{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Control.Monad ((>=>), void,forM, msum, filterM,forM_)
import           Data.Monoid ((<>))
import Data.String (fromString)
import System.FilePath.Posix ((</>),takeBaseName,takeDirectory)
import           Hakyll
import qualified Text.Pandoc as Pandoc
import System.Directory (getDirectoryContents)
import qualified Tct.Core.Main as TcT
import qualified Tct.Core.Data as TcT
import Data.Maybe (fromMaybe,isNothing)
import Data.Typeable

import Tools

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


idField = field "id" (return . toFilePath . itemIdentifier)

parentIdField = field "parent-id" (return . takeDirectory . toFilePath . itemIdentifier)

(</+>) :: Item a -> String -> Identifier
i </+> n = fromFilePath (toFilePath (itemIdentifier i) </> n)

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
-- arguments and strategies

data SomeSD where
  SomeSD :: TcT.StrategyDeclaration i o -> SomeSD

argMeta :: TcT.SomeArgument -> TcT.ArgMeta
argMeta (TcT.SomeArgument a) = TcT.argMeta a

argName :: TcT.SomeArgument -> String
argName (TcT.SomeArgument a) = TcT.argName a

argIsOptional :: TcT.SomeArgument -> Bool
argIsOptional (TcT.SomeArgument TcT.OptArg {}) = True
argIsOptional _ = False

baseArg :: TcT.SomeArgument -> TcT.SomeArgument
baseArg (TcT.SomeArgument (TcT.OptArg a _)) = baseArg (TcT.SomeArgument a)
baseArg (TcT.SomeArgument (TcT.SomeArg a)) = baseArg (TcT.SomeArgument a)
baseArg a = a

argIsStrategy :: TcT.SomeArgument -> Bool
argIsStrategy a =
  case baseArg a of
  TcT.SomeArgument TcT.StrategyArg{} -> True
  _ -> False

argIsSimple :: TcT.SomeArgument -> Bool
argIsSimple a =
  case baseArg a of
  TcT.SomeArgument TcT.SimpleArg{} -> True
  _ -> False

argDomain :: TcT.SomeArgument -> String
argDomain (baseArg -> TcT.SomeArgument a) = TcT.argDomain a
  
argIsBool :: TcT.SomeArgument -> Bool
argIsBool a = argDomain a == "bool"

argIsNat :: TcT.SomeArgument -> Bool
argIsNat a = argDomain a == "nat"

getArgStrategies :: Item TcT.SomeArgument -> Compiler [Item SomeSD]
getArgStrategies i = return $ 
  case baseArg (itemBody i) of
    TcT.SomeArgument (TcT.StrategyArg _ decls) -> [Item (i </+> TcT.declName d') (SomeSD d) | d@(TcT.SD d') <- decls]
    _ -> []
  

argCtx :: Context TcT.SomeArgument 
argCtx = field' "name" argName
         <> idField
         <> field' "help" (unlines . TcT.argHelp_ . argMeta)
         <> boolField' "isOptional" argIsOptional
         <> boolField' "isStrategy" argIsStrategy
         <> boolField' "isSimple" argIsSimple
         <> boolField' "isBool" argIsBool
         <> boolField' "isNat" argIsNat
         <> listFieldWith "strategies" strategyCtx getArgStrategies
  where
    field' n f = field n (return . f . itemBody)
    boolField' n f = boolField n (f . itemBody)
  
getArgs :: Item SomeSD -> Compiler [Item TcT.SomeArgument]
getArgs i = return [ Item (i </+> argName a) a | a <- as ]
  where as = case itemBody i of {SomeSD (TcT.SD decl) -> TcT.toArgList (TcT.declArgs decl)}
  -- return . toItem . argsInfo . itemBody where
  -- argsInfo (SomeSD (TcT.SD decl)) = (TcT.declName decl, )
  -- toItem (declName,as) = [Item (fromFilePath (toFilePath (itemId "option" </>  declName </> argName a)) a | a <- as ]


strategyDeclarations :: TcT.Declared a a => Tool a -> [TcT.StrategyDeclaration a a]
strategyDeclarations tool =  defaultDecl : TcT.decls where
  defaultDecl = TcT.SD (TcT.strategy "default" () (TcT.defaultStrategy (tctConfig tool)))

getStrategies :: TcT.Declared a a => Tool a -> Compiler [Item SomeSD]
getStrategies tool = return [Item (strategyId decl) (SomeSD decl) | decl <- strategyDeclarations tool] where
  strategyId (TcT.SD sd) = fromFilePath (toolName tool </> TcT.declName sd)


strategyCtx :: Context SomeSD
strategyCtx =
  idField
  <> parentIdField
  <> field "name" (return . name . itemBody)
  <> listFieldWith "argument" argCtx getArgs
  where name (SomeSD (TcT.SD sd)) = TcT.declName sd


----------------------------------------------------------------------
-- tool

toolContext :: TcT.Declared a a => Tool a -> Context String
toolContext tool = 
  titleField (toolName tool ++ " web-interface")
  <> constField "id" (toolName tool)
  <> constField "defaultInput" (exampleDir tool </> defaultInput tool)
  <> listField  "examples"     exampleCtx (getExamples tool)
  <> listField  "strategies"   strategyCtx (getStrategies tool)
  <> defaultContext

----------------------------------------------------------------------
-- main rule for webif.php

webIF :: TcT.Declared a a => Tool a -> Rules ()
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
main = hakyllWith defaultConfiguration $ do
  
    match "javascript/*" $ route idRoute

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
      
    match "javascript/*" $ do
      route idRoute
      compile copyFileCompiler

    match "examples/*/*" $ do
      route idRoute
      compile copyFileCompiler

    match "templates/*" $ compile templateCompiler

    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler >>= withDefault defaultContext

    forM_ tools $ \ (SomeTool tool) -> match "webif.php" (webIF tool)
