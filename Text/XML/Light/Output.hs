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
import Data.Monoid ( (<>) )

import qualified Data.Text as T

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
ppElement          :: Element -> T.Text
ppElement           = ppcElement prettyConfigPP

-- | Pretty printing content
ppContent          :: Content -> T.Text
ppContent           = ppcContent prettyConfigPP



-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppcTopElement      :: ConfigPP -> Element -> T.Text
ppcTopElement c e   = T.unlines [xml_header,ppcElement c e]

-- | Pretty printing elements
ppcElement         :: ConfigPP -> Element -> T.Text
ppcElement c e      = ppElementS c "" e ""

-- | Pretty printing content
ppcContent         :: ConfigPP -> Content -> T.Text
ppcContent c x      = ppContentS c "" x ""





-- | Pretty printing content using ShowS
ppContentS         :: ConfigPP -> T.Text -> Content -> TextS
ppContentS c i x xs = case x of
                        Elem e -> ppElementS c i e xs
                        Text t -> ppCDataS c i t xs
                        CRef r -> showCRefS r xs

ppElementS         :: ConfigPP -> T.Text -> Element -> TextS
ppElementS c i e xs = i <> (tagStart (elName e) (elAttribs e) $
  case elContent e of
    [] | "?" `T.isPrefixOf` qName name -> " ?>" <> xs
       | shortEmptyTag c name  -> " />" <> xs
    [Text t] -> ">" <> ppCDataS c "" t (tagEnd name xs)
    cs -> ">" <> nl <> (foldr ppSub (i <> tagEnd name xs) cs)
      where ppSub e1 = ppContentS c (sp <> i) e1 . showText nl
            (nl,sp)  = if prettify c then ("\n","  ") else ("","")
  )
  where name = elName e

ppCDataS           :: ConfigPP -> T.Text -> CData -> TextS
ppCDataS c i t xs   = i <> if cdVerbatim t /= CDataText || not (prettify c)
                             then showCDataS t xs
                             else T.foldr cons xs (showCData t)

  where cons         :: Char -> T.Text -> T.Text
        cons '\n' ys  = "\n" <> i <> ys
        cons y ys     = y `T.cons` ys



--------------------------------------------------------------------------------

-- | Adds the <?xml?> header.
showTopElement     :: Element -> T.Text
showTopElement c    = xml_header <> showElement c

showContent        :: Content -> T.Text
showContent c       = ppContentS defaultConfigPP "" c ""

showElement        :: Element -> T.Text
showElement c       = ppElementS defaultConfigPP "" c ""

showCData          :: CData -> T.Text
showCData c         = ppCDataS defaultConfigPP "" c ""

-- Note: crefs should not contain '&', ';', etc.
showCRefS          :: T.Text -> TextS
showCRefS r xs      = "&" <> r <> ";" <> xs

-- | Convert a text element to characters.
showCDataS         :: CData -> TextS
showCDataS cd =
 case cdVerbatim cd of
   CDataText     -> escStr (cdData cd)
   CDataVerbatim -> showText "<![CDATA[" . escCData (cdData cd)
                                           . showText "]]>"
   CDataRaw      -> \ xs -> cdData cd <> xs

--------------------------------------------------------------------------------
escCData           :: T.Text -> TextS
escCData cs | "]]>" `T.isPrefixOf` cs = showText "]]]]><![CDATA[>" . escCData (T.drop 3 cs)
            | T.null cs = id
            | otherwise = showCharT (T.head cs) . escCData (T.tail cs)

escChar            :: Char -> TextS
escChar c = case c of
  '<'   -> showText "&lt;"
  '>'   -> showText "&gt;"
  '&'   -> showText "&amp;"
  '"'   -> showText "&quot;"
  -- we use &#39 instead of &apos; because IE apparently has difficulties
  -- rendering &apos; in xhtml.
  -- Reported by Rohan Drape <rohan.drape@gmail.com>.
  '\''  -> showText "&#39;"

  -- NOTE: We escape '\r' explicitly because otherwise they get lost
  -- when parsed back in because of then end-of-line normalization rules.
  _ | isPrint c || c == '\n' -> showCharT c
    | otherwise -> showText $ "&#" <> oc <> ";"
      where oc = T.pack $ show $ ord c

escStr             :: T.Text -> TextS
escStr cs rs        = T.foldr escChar rs cs

tagEnd             :: QName -> TextS
tagEnd qn rs        = "</" <> showQName qn <> ">" <> rs

tagStart           :: QName -> [Attr] -> TextS
tagStart qn as rs   = "<" <> showQName qn <> as_str <> rs
 where as_str       = if null as then "" else " " <> T.unwords (map showAttr as)

showAttr           :: Attr -> T.Text
showAttr (Attr qn v) = showQName qn <> "=\"" <> escStr v "\""

showQName          :: QName -> T.Text
showQName q         = pre <> qName q
  where pre = case qPrefix q of
                Nothing -> ""
                Just p  -> p <> ":"

type TextS = T.Text -> T.Text

showText :: T.Text -> TextS
showText = (<>)

showCharT :: Char -> TextS
showCharT x = (<>) (T.singleton x) 

showt :: Show a => a -> TextS
showt = showText . T.pack . show

