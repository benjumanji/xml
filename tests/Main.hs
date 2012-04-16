{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.XML.Light

main = do
        s <- TIO.readFile "rss.xml"
        case parseXMLDoc s of
                Nothing -> error "Failed to parse xml"
                Just doc  -> 
                        let _:_:elem:_ = findElements (unqual "description") $ doc
                            [Text title_content] = elContent elem
                            title_text = cdData title_content
                        in mapM_ TIO.putStrLn $ T.lines title_text

