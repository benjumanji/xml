{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.XML.Light

pp = useShortEmptyTags (const False) defaultConfigPP

main = TIO.putStrLn $ ppcTopElement pp
                    $ Element { elName = unqual (T.pack "test")
                              , elAttribs = []
                              , elContent = []
                              , elLine = Nothing
                              }

