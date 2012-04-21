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
import Data.List ( isPrefixOf )
-- import Data.Monoid ( (<>) )
import Data.Monoid ( mappend
                   , Monoid()
                   )

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B

-- this is just to cover older versions of base
(<>) :: Monoid a => a -> a -> a
(<>) = mappend

-- | The XML 1.0 header
xml_header :: T.Text
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
ppTopElement       :: Element -> T.Text
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
ppcTopElement      :: ConfigPP -> Element -> T.Text
ppcTopElement c e   = T.unlines [xml_header,LT.toStrict $ ppcElement c e]

-- | Pretty printing elements
ppcElement         :: ConfigPP -> Element -> LT.Text
ppcElement c e      = B.toLazyText $ ppElementS c "" e

-- | Pretty printing content
ppcContent         :: ConfigPP -> Content -> LT.Text
ppcContent c x      = B.toLazyText $ ppContentS c "" x





-- | Pretty printing content using ShowS
ppContentS         :: ConfigPP -> T.Text -> Content -> B.Builder
ppContentS c i x = case x of
                        Elem e -> ppElementS c i e
                        Text t -> ppCDataS c i t
                        CRef r -> showCRefS r

ppElementS         :: ConfigPP -> T.Text -> Element -> B.Builder
ppElementS c i e = tagStart (elName e) (elAttribs e) <>
  case elContent e of
    [] | "?" `T.isPrefixOf` qName name -> B.fromText " ?>"
       | shortEmptyTag c name          -> B.fromText " />"
    [Text t]                           -> B.singleton '>' <> ppCDataS c i t <> (tagEnd name)
    cs -> B.singleton '>' <> nl <> (foldr ppSub (B.fromText i <> tagEnd name) cs)
      where ppSub e1 acc = sp <> ppContentS c i e1 <> nl <> acc
            (nl,sp)  = if prettify c then (B.singleton '\n',B.fromText "  ") 
                                     else (B.fromText T.empty, B.fromText T.empty)
  where name = elName e

ppCDataS :: ConfigPP -> T.Text -> CData -> B.Builder
ppCDataS c i t = ib <> if cdVerbatim t /= CDataText || not (prettify c)
                             then showCDataS t
                             else T.foldr cons (B.fromText T.empty) (showCData t)

  where ib = B.fromText i
        nb = B.singleton '\n' 
        cons         :: Char -> B.Builder -> B.Builder
        cons '\n' ys  = nb <> ib <> ys
        cons y ys     = B.singleton y <> ys



--------------------------------------------------------------------------------

-- | Adds the <?xml?> header.
showTopElement     :: Element -> T.Text
showTopElement c    = xml_header <> showElement c

showContent        :: Content -> T.Text
showContent c       = LT.toStrict . B.toLazyText $ ppContentS defaultConfigPP "" c

showElement        :: Element -> T.Text
showElement c       = LT.toStrict . B.toLazyText $ ppElementS defaultConfigPP "" c

showCData          :: CData -> T.Text
showCData c         = LT.toStrict . B.toLazyText $ ppCDataS defaultConfigPP "" c 

-- Note: crefs should not contain '&', ';', etc.
showCRefS          :: T.Text -> B.Builder
showCRefS r         = B.fromText $ "&" <> r <> ";"

-- | Convert a text element to characters.
showCDataS         :: CData -> B.Builder
showCDataS cd =
 case cdVerbatim cd of
   CDataText     -> escStr (cdData cd)
   CDataVerbatim -> B.fromText "<![CDATA[" <> escCData (cdData cd)
                                           <> B.fromText "]]>"
   CDataRaw      -> B.fromText $ cdData cd

--------------------------------------------------------------------------------
escCData           :: T.Text -> B.Builder
escCData cs | "]]>" `T.isPrefixOf` cs = B.fromText "]]]]><![CDATA[>" <> escCData (T.drop 3 cs)
            | T.null cs = B.fromText T.empty
            | otherwise = B.singleton (T.head cs) <> escCData (T.tail cs)

escChar            :: Char -> B.Builder
escChar c = case c of
  '<'   -> B.fromText "&lt;"
  '>'   -> B.fromText "&gt;"
  '&'   -> B.fromText "&amp;"
  '"'   -> B.fromText "&quot;"
  -- we use &#39 instead of &apos; because IE apparently has difficulties
  -- rendering &apos; in xhtml.
  -- Reported by Rohan Drape <rohan.drape@gmail.com>.
  '\''  -> B.fromText "&#39;"

  -- NOTE: We escape '\r' explicitly because otherwise they get lost
  -- when parsed back in because of then end-of-line normalization rules.
  _ | isPrint c || c == '\n' -> B.singleton c
    | otherwise -> B.fromText $ "&#" <> oc <> ";"
      where oc = T.pack $ show $ ord c

escStr             :: T.Text -> B.Builder
escStr cs           = T.foldr (\c b -> escChar c <> b) (B.fromText T.empty) cs

tagEnd             :: QName -> B.Builder
tagEnd qn           = B.fromText "</" <> showQName qn <> B.singleton '>'

tagStart           :: QName -> [Attr] -> B.Builder
tagStart qn as      = B.singleton '<' <> showQName qn <> as_str
 where as_str       = if null as then B.fromText "" 
                                 else let bspace = B.singleton ' '
                                      in foldr (\a b -> b <> bspace <> showAttr a) bspace as

showAttr           :: Attr -> B.Builder
showAttr (Attr qn v) = showQName qn <> B.fromText "=\"" <> escStr v <> B.singleton '"'

showQName          :: QName -> B.Builder
showQName q         = B.fromText qn
  where name = qName q
        qn   = case qPrefix q of
                Nothing -> name
                Just p  -> p <> ":" <> name
