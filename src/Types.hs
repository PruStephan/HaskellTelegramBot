{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Label

data Format = FB2 | PDF | TXT

data Book = Book { _authorId :: Int
                 , _title   :: String
                 , _format :: [Format]
                 , _properties :: Properties
                 }

data Properties = Properties { _link :: String
                             , _coverLink :: String
                             }
                             
data Author = Author { _Id :: Int 
                     , _name :: String
                     }         


mkLabels [''Book, ''Properties, ''Author]

makeFormat :: String -> Format
makeFormat s = case s of
               "fb2" -> FB2
               "pdf" -> PDF
               "txt" -> TXT
