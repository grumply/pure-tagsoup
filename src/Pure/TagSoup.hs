{-# language CPP #-}
module Pure.TagSoup where

import Pure.Txt as T
import Pure.View

import Text.HTML.TagSoup.Tree as TS
import Text.HTML.TagSoup as TS
import Text.StringLike

import Data.Maybe

import qualified Data.Map as M

#ifdef __GHCJS__
instance StringLike Txt where
  empty = T.empty
  cons = T.cons
  uncons t =
    -- careful here because of a ghcjs bug
    if T.null t then
      Nothing
    else
      let ~(Just ~(c,rest)) = T.uncons t
      in (Just (c,rest))
  toString = T.unpack
  fromChar = T.singleton
  strConcat = T.concat
  strNull = T.null
  append = T.append
#endif

parseView :: Txt -> [View '[]]
parseView = fmap convertTree . parseTree
  where
    convertTree :: TagTree Txt -> View '[]
    convertTree tree =
      case tree of
        TagBranch t as cs ->
          if t == "svg" then
            SVGView Nothing t (fmap convertAttribute as) (fmap convertSVGTree cs)
          else
            HTMLView Nothing t (fmap convertAttribute as) (fmap convertTree cs)
        TagLeaf t ->
          case t of
            TagText tt   -> TextView Nothing tt
            TagOpen t as -> HTMLView Nothing t (fmap convertAttribute as) []
            _            -> NullView Nothing


    convertSVGTree :: TagTree Txt -> View '[]
    convertSVGTree tree =
      case tree of
        TagBranch t as cs ->
          if t == "foreignObject" then
            SVGView Nothing t (fmap convertAttribute as) (fmap convertTree cs)
          else
            SVGView Nothing t (fmap convertAttribute as) (fmap convertSVGTree cs)
        TagLeaf t ->
          case t of
            TagText tt   -> TextView Nothing tt
            TagOpen t as -> SVGView Nothing t (fmap convertAttribute as) []
            _            -> NullView Nothing

    convertAttribute :: TS.Attribute Txt -> Feature '[]
    convertAttribute (k,v) = let v' = T.toLower v in
      if v == "style" then
        let ss = T.splitOn ";" v
            brk t =
              let (pre,suf) = T.break (== ':') t
                 -- ghcjs bug requires this
              in if T.null suf then
                   Nothing
                 else
                   Just (pre,T.tail suf)
            kvs = mapMaybe brk ss
        in
          StyleList (M.fromList kvs)
      else if k == "href" then
        if dumbRelativeCheck v then
          Lref v
        else
          Href v
      else if T.isPrefixOf "xlink:" k then
          if k == "xlink:href" then
            SVGLink v
          else
            xlink k v
      else
        Attr k v

dumbRelativeCheck :: Txt -> Bool
dumbRelativeCheck t = T.isPrefixOf "/" t && not (T.isPrefixOf "//" t)
