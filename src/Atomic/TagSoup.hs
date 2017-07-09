{-# language CPP #-}
module Atomic.TagSoup where

import Atomic
import Component

import Text.HTML.TagSoup.Tree as TS
import Text.HTML.TagSoup as TS
import Text.StringLike

import qualified Data.Txt as T

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
            SVGHTML Nothing t (fmap convertAttribute as) (fmap convertSVGTree cs)
          else
            HTML Nothing t (fmap convertAttribute as) (fmap convertTree cs)
        TagLeaf t ->
          case t of
            TagText tt   -> TextHTML Nothing tt
            TagOpen t as -> HTML Nothing t (fmap convertAttribute as) []
            _            -> NullHTML Nothing


    convertSVGTree :: TagTree Txt -> View '[]
    convertSVGTree tree =
      case tree of
        TagBranch t as cs ->
          if t == "foreignObject" then
            SVGHTML Nothing t (fmap convertAttribute as) (fmap convertTree cs)
          else
            SVGHTML Nothing t (fmap convertAttribute as) (fmap convertSVGTree cs)
        TagLeaf t ->
          case t of
            TagText tt   -> TextHTML Nothing tt
            TagOpen t as -> SVGHTML Nothing t (fmap convertAttribute as) []
            _            -> NullHTML Nothing

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
          StyleList kvs
      else if k == "href" then
        if dumbRelativeCheck v then
          LinkTo v Nothing
        else
          Attribute "href" v
      else if T.isPrefixOf "xlink:" k then
          if k == "xlink:href" then
            SVGLinkTo v Nothing
          else
            XLink k v
      else
        Attribute k v

dumbRelativeCheck :: Txt -> Bool
dumbRelativeCheck t = T.isPrefixOf "/" t && not (T.isPrefixOf "//" t)
