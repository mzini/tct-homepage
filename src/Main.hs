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
import Tct.Hoca.Config (hocaConfig)


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

data Config a = Config { tctConfig   :: TcT.TctConfig a
                       , modeName    :: String
                       , defaultInput :: FilePath }


hoca = Config { tctConfig = hocaConfig
              , modeName = "HoCA"
              , defaultInput = "rev-dl.fp" }

----------------------------------------------------------------------
-- examples

exampleDir :: Config a -> FilePath
exampleDir cfg = "examples" </> modeName cfg

getExamples :: Config a -> Compiler [Item CopyFile]
getExamples cfg = loadAll (fromString (exampleDir cfg </> "*"))

exampleContext :: Config a -> Context b
exampleContext cfg = field "name" (\ item -> return $ takeBaseName $ toFilePath $ itemIdentifier item)
                 <> field "filepath" (\ item -> return $ exampleDir cfg </> toFilePath (itemIdentifier item))

----------------------------------------------------------------------
-- main rule for webif.php

webIF :: Config a -> Rules ()
webIF cfg = version name $ do
  route (constRoute name)
  compile $ do
    getResourceBody >>= applyAsTemplate ctx >>= withDefaultPHP ctx
    where
      ctx = titleField (name ++ " web-interface")
            <> constField "defaultInput" (defaultInput cfg)
            <> listField "examples" (exampleContext cfg) (getExamples cfg)
            <> defaultContext
      name = modeName cfg ++ ".php"

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
