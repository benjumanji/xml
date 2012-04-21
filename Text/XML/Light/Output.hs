--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Light.Output
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki <diatchki@galois.com>
-- Stability : provisional
-- Portability:
--
-- Output handling for the lightweight XML lib.
--

{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Light.Output
  ( showTopElement, showContent, showElement, showCData, showQName, showAttr
  , ppTopElement, ppContent, ppElement
  , ppcTopElement, ppcContent, ppcElement
  , ConfigPP
  , defaultConfigPP, prettyConfigPP
  , useShortEmptyTags, useExtraWhiteSpace
  , tagEnd, xml_header
  ) where

import Text.XML.Light.Types
import Data.Char
-- import Data.Monoid ( (<>) )
import Data.Monoid ( mappend
                   , Monoid
                   , mempty
                   )

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as BI

-- this is just to cover older versions of base
(<>) :: Monoid a => a -> a -> a
(<>) = mappend

-- | The XML 1.0 header
xml_header :: LT.Text
xml_header = "<?xml version='1.0' ?>"


--------------------------------------------------------------------------------
data ConfigPP = ConfigPP
  { shortEmptyTag :: QName -> Bool
  , prettify      :: Bool
  }

-- | Default pretty orinting configutaion.
--  * Always use abbreviate empty tags.
defaultConfigPP :: ConfigPP
defaultConfigPP = ConfigPP { shortEmptyTag = const True
                           , prettify      = False
                           }

-- | The predicate specifies for which empty tags we should use XML's
-- abbreviated notation <TAG />.  This is useful if we are working with
-- some XML-ish standards (such as certain versions of HTML) where some
-- empty tags should always be displayed in the <TAG></TAG> form.
useShortEmptyTags :: (QName -> Bool) -> ConfigPP -> ConfigPP
useShortEmptyTags p c = c { shortEmptyTag = p }


-- | Specify if we should use extra white-space to make document more readable.
-- WARNING: This adds additional white-space to text elements,
-- and so it may change the meaning of the document.
useExtraWhiteSpace :: Bool -> ConfigPP -> ConfigPP
useExtraWhiteSpace p c  = c { prettify = p }

-- | A configuration that tries to make things pretty
-- (possibly at the cost of changing the semantics a bit
-- through adding white space.)
prettyConfigPP     :: ConfigPP
prettyConfigPP      = useExtraWhiteSpace True defaultConfigPP


--------------------------------------------------------------------------------


-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppTopElement       :: Element -> LT.Text
ppTopElement        = ppcTopElement prettyConfigPP

-- | Pretty printing elements
ppElement          :: Element -> LT.Text
ppElement           = ppcElement prettyConfigPP

-- | Pretty printing content
ppContent          :: Content -> LT.Text
ppContent           = ppcContent prettyConfigPP



-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppcTopElement      :: ConfigPP -> Element -> LT.Text
ppcTopElement c e   = LT.unlines [xml_header, ppcElement c e]

-- | Pretty printing elements
ppcElement         :: ConfigPP -> Element -> LT.Text
ppcElement c e      = B.toLazyText $ ppElementB c mempty e

-- | Pretty printing content
ppcContent         :: ConfigPP -> Content -> LT.Text
ppcContent c x      = B.toLazyText $ ppContentB c mempty x





-- | Pretty printing content using ShowS
ppContentB         :: ConfigPP -> B.Builder -> Content -> B.Builder
ppContentB c i x = case x of
                        Elem e -> ppElementB c i e
                        Text t -> ppCDataB c i t
                        CRef r -> showCRefB r

ppElementB         :: ConfigPP -> B.Builder -> Element -> B.Builder
ppElementB c i e = tagStart (elName e) (elAttribs e) <>
  case elContent e of
    [] | "?" `T.isPrefixOf` qName name -> " ?>"
       | shortEmptyTag c name          -> " />"
    [Text t]                           -> ">" <> ppCDataB c i t <> (tagEndB name)
    cs -> ">" <> nl <> (foldr ppSub (i <> tagEndB name) cs)
      where ppSub e1 acc = sp <> ppContentB c i e1 <> nl <> acc
            (nl,sp)  = if prettify c then ("\n","  ") 
                                     else (mempty,mempty)
  where name = elName e

ppCDataB :: ConfigPP -> B.Builder -> CData -> B.Builder
ppCDataB c i t = i <> if cdVerbatim t /= CDataText || not (prettify c)
                             then showCDataB t
                             else LT.foldr cons mempty (showCData t)

  where nb = B.singleton '\n' 
        cons         :: Char -> B.Builder -> B.Builder
        cons '\n' ys  = nb <> i <> ys
        cons y ys     = B.singleton y <> ys



--------------------------------------------------------------------------------

-- | Adds the <?xml?> header.
showTopElement     :: Element -> LT.Text
showTopElement c    = xml_header <> showElement c

showContent        :: Content -> LT.Text
showContent c       = B.toLazyText $ ppContentB defaultConfigPP mempty c

showElement        :: Element -> LT.Text
showElement c       = B.toLazyText $ ppElementB defaultConfigPP mempty c

showCData          :: CData -> LT.Text
showCData c         = B.toLazyText $ ppCDataB defaultConfigPP mempty c 

-- Note: crefs should not contain '&', ';', etc.
showCRefB          :: T.Text -> B.Builder
showCRefB r         = B.fromText $ "&" <> r <> ";"

-- | Convert a text element to characters.
showCDataB         :: CData -> B.Builder
showCDataB cd =
 case cdVerbatim cd of
   CDataText     -> escStr (cdData cd)
   CDataVerbatim -> "<![CDATA[" <> escCData (cdData cd) <> "]]>"
   CDataRaw      -> B.fromText $ cdData cd

--------------------------------------------------------------------------------
escCData           :: T.Text -> B.Builder
escCData cs | "]]>" `T.isPrefixOf` cs = B.fromText "]]]]><![CDATA[>" <> escCData (T.drop 3 cs)
            | T.null cs = mempty
            | otherwise = B.singleton (T.head cs) <> escCData (T.tail cs)

escChar            :: Char -> B.Builder
escChar c = case c of
  '<'   -> "&lt;"
  '>'   -> "&gt;"
  '&'   -> "&amp;"
  '"'   -> "&quot;"
  -- we use &#39 instead of &apos; because IE apparently has difficulties
  -- rendering &apos; in xhtml.
  -- Reported by Rohan Drape <rohan.drape@gmail.com>.
  '\''  -> "&#39;"

  -- NOTE: We escape '\r' explicitly because otherwise they get lost
  -- when parsed back in because of then end-of-line normalization rules.
  _ | isPrint c || c == '\n' -> B.singleton c
    | otherwise -> "&#" <> oc <> ";"
      where oc =  BI.decimal $ ord c

escStr             :: T.Text -> B.Builder
escStr cs           = T.foldr (\c b -> escChar c <> b) mempty cs

tagEndB :: QName -> B.Builder
tagEndB qn = "</" <> showQNameB qn <> ">"

tagEnd             :: QName -> T.Text
tagEnd qn           = "</" <> showQName qn <> ">"

tagStart :: QName -> [Attr] -> B.Builder
tagStart qn attrs = B.singleton '<' <> showQNameB qn <> as_str
 where as_str = if null attrs then mempty 
                              else foldr (\a b -> b <> " " <> showAttr a) " " attrs

showAttr           :: Attr -> B.Builder
showAttr (Attr qn v) = showQNameB qn <> "=\"" <> escStr v <> "\""

showQNameB :: QName -> B.Builder
showQNameB = B.fromText . showQName 

showQName :: QName -> T.Text 
showQName q = qn
  where name = qName q
        qn   = case qPrefix q of
                Nothing -> name
                Just p  -> p <> ":" <> name