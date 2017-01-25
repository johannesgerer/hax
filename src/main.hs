{-# LANGUAGE OverloadedStrings
 #-}

 
import HAX.Report
import HAX.Example (example1)



main = do
  writeHtml "html" example1
