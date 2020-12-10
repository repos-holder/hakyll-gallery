--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import Algorithms.NaturalSort
import System.FilePath
import Control.Applicative
import Data.List
import Hakyll.Images (loadImage, ensureFitCompiler)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            -- here we get listField for all gallery items
            ctx <- makeGalleryCtx
            getResourceString
              >>= renderPandoc
              -- apply twice first $gallery()$ into $for()$ template then $for()$ into html code
              >>= applyAsTemplate ctx
              >>= applyAsTemplate ctx
              >>= loadAndApplyTemplate "templates/post.html"    postCtx
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match galleryFiles $ do
        route   idRoute
        compile copyFileCompiler
    match galleryImages $ version "thumbnail" $ do
      route . customRoute $ (\x -> replaceExtension x (".thumb" ++ takeExtension x)) . toFilePath
      compile $ loadImage
        >>= ensureFitCompiler 200 72
    -- photo descriptions
    match (fromGlob $ folder ++ "/*/*.md") $ do
        route . customRoute $ (<.> "html") . toFilePath
        compile $ pandocCompiler
            >>= relativizeUrls
    -- actually when some page is modified we only need to update adjacent pages so they link to our page, but due
    -- hakyll limitation we rebuild all gallery pages :(
    galleryDependencies <- makePatternDependency galleryFiles
    rulesExtraDependencies [galleryDependencies] $ do
      match (fromGlob $ folder ++ "/index.html") $ do
        route $ setExtension "html"
        compile $ do
          -- here we get listField for all gallery items
          ctx <- makeGalleryCtx
          getResourceString
            >>= renderPandoc
            -- apply twice first $gallery()$ into $for()$ template then $for()$ into html code
            >>= applyAsTemplate ctx
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" (constField "title" "Gallery" `mappend` postCtx)
            >>= relativizeUrls
    rulesExtraDependencies [galleryDependencies] $ do
      match galleryFiles $ version "page" $ do
        route . customRoute $ (<.> "html") . toFilePath
        compile $ do
          -- here we find metadata for our item and its adjacent pages and put it into context
          path <- toFilePath <$> getUnderlying
          galleryUnboxed <- gallery
          let [(_,ctx)] = filter (\ (x,_) -> x `equalFilePath` takeDirectory path) galleryUnboxed
          let [item] = filter (\ x -> elem' x `equalFilePath` path) ctx
          let prevElm = maybe missingField (\ x -> ctxMaker "prev" (\ _ -> x)) $ prev' item
          let nextElm = maybe missingField (\ x -> ctxMaker "next" (\ _ -> x)) $ next' item
          let ctx' = ctxMaker "" (\ _ -> item) `mappend`
                prevElm `mappend`
                nextElm `mappend`
                constField "baseurl" ("/" ++ folder) `mappend`
                -- here we get description for gallery item
                (field "body" . return . loadBody . fromFilePath $ path <.> "md") `mappend`
                postCtx
          makeItem ""
            >>= loadAndApplyTemplate "templates/gallery.html" (ctx' `mappend` postCtx)
            >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

folder = "gallery"
galleryImages =
  fromGlob (folder ++ "/*/*.jpg")
  .||. fromGlob (folder ++ "/*/*.jpeg")
  .||. fromGlob (folder ++ "/*/*.png")
galleryVideos =
  fromGlob (folder ++ "/*/*.mp4")
galleryFiles = galleryImages .||. galleryVideos
-- https://www.reddit.com/r/haskell/comments/2nepr0/implementing_doubly_linked_lists_in_haskell/cmdadok?utm_source=share&utm_medium=web2x
data DList a = Empty | Cell { elem :: a, prev :: DList a, next :: DList a } deriving (Eq)
data GalleryItem a = GalleryItem { elem' :: String
                               , prev' :: Maybe (GalleryItem a)
                               , next' :: Maybe (GalleryItem a)
                               , url :: Compiler String
                               , page :: Compiler String
                               , thumbnail :: Compiler String
                               , video :: Bool
                               , previousPageNum :: Bool
                               , nextPageNum :: Bool
                               }
makeFromList :: [a] -> DList a
makeFromList = go Empty
    where go :: DList a -> [a] -> DList a
          go prev [] = Empty
          go prev (a:as) = head
              where head = Cell a prev tail
                    tail = go head as
toList :: DList a -> [a]
toList Empty = []
toList (Cell a _ next) = a : toList next
mapDList f = go
    where go Empty = Empty
          go item@Cell { Main.elem = e, prev = p, next = n } = Cell { Main.elem = f item, prev = go p, next = go n }
-- create metadata list for futher search: image or video, thumbnail, previous and next item
gallery = do
  recursiveContents <- unsafeCompiler $ getRecursiveContents (\_ -> return False) folder
  let contents = map (folder </>) recursiveContents

  -- sort by last time modified
  contentsTime <- sequence $ map (getItemModificationTime . fromFilePath) contents
  let contentsWithTime = zip contentsTime contents
  let timeSortedList = sortBy (\ x y -> Prelude.compare (fst x) (fst y)) contentsWithTime
  --let (_,sortedContents) = unzip timeSortedList

  -- sort by natural sort
  let sortedContents = sortBy (\ x y -> Algorithms.NaturalSort.compare x y) contents

  let filteredContents = filter (matches galleryFiles . fromFilePath) sortedContents
  let groupContents = groupBy (\ x y -> takeDirectory x == takeDirectory y) filteredContents
  let linkedContents = map (\ l -> (takeDirectory $ head l, makeFromList l)) groupContents
  let
    ctxMaker Cell { Main.elem = e, prev = p, next = n } =
      let versionUrl version path = fmap (maybe empty toUrl) . getRoute . setVersion version $ fromFilePath path
          prevElm = if p == Empty then empty else pure $ itemMaker (Main.elem p) p n empty empty
          nextElm = if n == Empty then empty else pure $ itemMaker (Main.elem n) p n empty empty
          itemMaker e p n prevElm nextElm = GalleryItem { elem' = e
                                                  , prev' = prevElm
                                                  , next' = nextElm
                                                  , url = versionUrl Nothing e
                                                  , page = versionUrl (Just "page") e
                                                  , thumbnail = versionUrl (Just "thumbnail") e
                                                  , video = matches galleryVideos $ fromFilePath e
                                                  , previousPageNum = p /= Empty
                                                  , nextPageNum = n /= Empty
                                                  }
      in itemMaker e p n prevElm nextElm
  return $ map (\ (dir,l) -> (dir, toList $ mapDList ctxMaker l)) linkedContents
-- build actual context with all needed fields
ctxMaker prefix f =
  field (prefix ++ "url") (url . f) `mappend`
  field (prefix ++ "page") (page . f) `mappend`
  field (prefix ++ "thumbnail") (thumbnail . f) `mappend`
  boolField (prefix ++ "video") (video . f) `mappend`
  if prefix == "" then missingField else boolField "previousPageNum" (previousPageNum . f) `mappend`
  if prefix == "" then missingField else boolField "nextPageNum" (nextPageNum . f)
-- here we build listField for all gallery items and also teaser variant where are only first 5 items available
makeGalleryCtx = do
  galleryUnboxed <- gallery
  let listfieldMaker (folder,items) =
        let items' = map makeItem items
        -- variable names are fragile
        in listField (takeFileName folder) (ctxMaker "" itemBody) (sequence items') `mappend`
           listField (takeFileName folder ++ "preview") (ctxMaker "" itemBody) (sequence $ take 5 items')
  let ctx = map listfieldMaker galleryUnboxed
  return $ foldl1 mappend ctx `mappend` galleryField
-- this is template used for gallery
galleryField = functionField "gallery" $ \[args] _ ->
  return $ unlines [
    "$for(" ++ args ++ ")$",
    "$if(video)$",
    "<a href=\"$page$\"><video width=\"128\" height=\"72\" preload=\"metadata\"><source src=\"$url$#t=1\"></video></a>",
    "$else$",
    "<a href=\"$page$\"><img src=\"$thumbnail$\"/></a>",
    "<link rel=\"prefetch\" href=\"$url$\">",
    "$endif$",
    "$endfor$"
  ]
