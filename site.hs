{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.List (sortBy,isSuffixOf)
import Data.Typeable
import GHC.IO.Encoding
import Hakyll
import Hakyll.Favicon (faviconsRules, faviconsField)
import System.FilePath.Posix (takeBaseName,takeDirectory,(</>))
import Timestamp

main :: IO ()
main = do 
   
   -- Set the encoding so w3c doesnt complain
   setLocaleEncoding utf8
   hakyll $ do

      -- Generate the favicons
      --faviconsRules "icons/favicon.svg"

      -- Straight copying of files
      match (fromList ["humans.txt", "robots.txt"]) $ do
         route idRoute
         compile copyFileCompiler

      -- CSS needs to be compiled and minified
      match "css/*" $ do
         route   idRoute
         compile compressCssCompiler

      -- Load pages that need to be formatted
      match (fromList ["description.md", "about.md", "contact.md"]) $ do
         route $ cleanRoute
         compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
            >>= cleanIndexUrls

      -- Compile the templates
      match "templates/*" $ compile templateBodyCompiler

-- Our default context for pages
ctx :: Context String
ctx = defaultContext <>
      faviconsField

-- Default context for format
fmtCtx :: Integer -> Integer -> String -> Context String
fmtCtx m y u =
   (titleField mTitle) <>
   (field "nrdbUrl" u) <>
   (listFieldWith 
   ctx
   where
      mTitle = "Format for " ++ (showMonth m) ++ " " ++ y

-- Functions to convert pages to /name/index.html
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
   where
      createIndexRoute ident =
         takeDirectory p </> takeBaseName p </> "index.html"
            where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
   where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
   where idx = "index.html"
