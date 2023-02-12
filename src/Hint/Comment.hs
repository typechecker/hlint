
{-
<TEST>
{- MISSING HASH #-} -- {-# MISSING HASH #-}
<COMMENT> {- INLINE X -}
{- INLINE Y -} -- {-# INLINE Y #-}
{- INLINE[~k] f -} -- {-# INLINE[~k] f #-}
{- NOINLINE Y -} -- {-# NOINLINE Y #-}
{- UNKNOWN Y -}
<COMMENT> INLINE X
</TEST>
-}

{-# LANGUAGE ViewPatterns #-}

module Hint.Comment(commentHint) where

import Hint.Type
import Data.Char
import Data.List.Extra
import Refact.Types(Refactoring(ModifyComment))
import GHC.Types.SrcLoc
import GHC.Parser.Annotation
import GHC.Util
import qualified GHC.Data.Strict

directives :: [String]
directives = words $
    "LANGUAGE OPTIONS_GHC INCLUDE WARNING DEPRECATED MINIMAL INLINE NOINLINE INLINABLE " ++
    "CONLIKE LINE SPECIALIZE SPECIALISE UNPACK NOUNPACK SOURCE"

commentHint :: ModuHint
commentHint _ m = concatMap chk (ghcComments m)

chk :: LEpaComment -> [Idea]
chk comm@(L{})
  | isHaddockWhitespace comm = [(if isMultiline then emptyHaddockMulti else emptyHaddockSingle) comm]
  | isCommentWhitespace comm = if isMultiline then [emptyCommentMulti comm] else []
  | isMultiline, null (commentText comm) = [emptyCommentMulti comm]
  | isMultiline, "#" `isSuffixOf` s && not ("#" `isPrefixOf` s) = [grab "Fix pragma markup" comm $ '#':s]
  | isMultiline, name `elem` directives = [grab "Use pragma syntax" comm $ "# " ++ trim s ++ " #"]
    where
      isMultiline = isCommentMultiline comm
      s = commentText comm
      name = takeWhile (\x -> isAlphaNum x || x == '_') $ trimStart s

chk _ = []

isHaddockWhitespace :: LEpaComment -> Bool
isHaddockWhitespace comm = isHaddock comm && isStringWhitespace (drop 2 $ commentText comm)

isHaddock :: LEpaComment -> Bool
isHaddock (take 2 . commentText -> s) = " |" == s || " ^" == s

isStringWhitespace :: String -> Bool
isStringWhitespace = not . any (`notElem` " \r\n")

isCommentWhitespace :: LEpaComment -> Bool
isCommentWhitespace comm@(L (anchor -> span) _ ) =
  not (isPointRealSpan span) && isStringWhitespace (commentText comm)

emptyHaddockSingle  :: LEpaComment -> Idea
emptyHaddockSingle = emptyComment ("--" ++) "Empty single-line haddock"

emptyCommentMulti, emptyHaddockMulti :: LEpaComment -> Idea
emptyCommentMulti = emptyComment (\s -> "{-" ++ s ++ "-}") "Empty multi-line comment"
emptyHaddockMulti = emptyComment (\s -> "{-" ++ s ++ "-}") "Empty multi-line haddock"

emptyComment :: (String -> String) -> String -> LEpaComment -> Idea
emptyComment f msg o@(L pos _) =
  let s1 = commentText o
      loc = RealSrcSpan (anchor pos) GHC.Data.Strict.Nothing
  in
  ideaRemove Suggestion msg loc (f s1) (refact loc)
    where refact loc = [ModifyComment (toRefactSrcSpan loc) ""]

grab :: String -> LEpaComment -> String -> Idea
grab msg o@(L pos _) s2 =
  let s1 = commentText o
      loc = RealSrcSpan (anchor pos) GHC.Data.Strict.Nothing
  in
  rawIdea Suggestion msg loc (f s1) (Just $ f s2) [] (refact loc)
    where f s = if isCommentMultiline o then "{-" ++ s ++ "-}" else "--" ++ s
          refact loc = [ModifyComment (toRefactSrcSpan loc) (f s2)]

-- | 'True' if the span is known to straddle only one line.
-- SEE: https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Types/SrcLoc.hs#L480-483
isOneLineRealSpan :: RealSrcSpan -> Bool
isOneLineRealSpan span = line1 == line2 where
  line1 = srcLocLine $ realSrcSpanStart span
  line2 = srcLocLine $ realSrcSpanEnd span

-- | 'True' if the span is a single point.
-- SEE: https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Types/SrcLoc.hs#L485-488
isPointRealSpan :: RealSrcSpan -> Bool
isPointRealSpan span = isOneLineRealSpan span && col1 == col2 where
  col1 = srcLocCol $ realSrcSpanStart span
  col2 = srcLocCol $ realSrcSpanEnd span
