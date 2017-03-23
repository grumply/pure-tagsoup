{-# language CPP #-}
module Atomic.TagSoup where

import Atomic

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

parseAtoms :: Txt -> [Atom Void]
parseAtoms = map convertTree . parseTree
  where
    convertTree :: TagTree Txt -> Atom Void
    convertTree tree =
      case tree of
        TagBranch t as cs ->
          if t == "svg" then
            SVGAtom Nothing t (map convertAttribute as) (map convertSVGTree cs)
          else
            Atom Nothing t (map convertAttribute as) (map convertTree cs)
        TagLeaf t ->
          case t of
            TagText tt   -> Text Nothing tt
            TagOpen t as -> Atom Nothing t (map convertAttribute as) []
            _            -> NullAtom Nothing


    convertSVGTree :: TagTree Txt -> Atom Void
    convertSVGTree tree =
      case tree of
        TagBranch t as cs ->
          if t == "foreignObject" then
            SVGAtom Nothing t (map convertAttribute as) (map convertTree cs)
          else
            SVGAtom Nothing t (map convertAttribute as) (map convertSVGTree cs)
        TagLeaf t ->
          case t of
            TagText tt   -> Text Nothing tt
            TagOpen t as -> SVGAtom Nothing t (map convertAttribute as) []
            _            -> NullAtom Nothing

    convertAttribute :: TS.Attribute Txt -> Feature Void
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
          Style kvs
      else if k == "href" then
        if dumbRelativeCheck v then
          Link v Nothing
        else
          Attribute "href" v
      else if T.isPrefixOf "xlink:" k then
          if k == "xlink:href" then
            SVGLink v Nothing
          else
            XLink k v
      else
        Attribute k v

dumbRelativeCheck :: Txt -> Bool
dumbRelativeCheck t = T.isPrefixOf "/" t && not (T.isPrefixOf "//" t)
