
module Language.Processor
    ( findWords,
    ) where

import qualified Data.Text as T
import           Data.Text (Text)

import Data.List
import Glider.NLP.Statistics
import Glider.NLP.Tokenizer     
      
findWords :: [String] -> Text -> IO [String]
findWords cws t = do
    -- tgr <- defaultTagger
    -- chk <- defaultChunker
    -- r <- chunkText tgr chk t
    let ts = tokenize t
    let ws = foldl f [] ts   
    let es = map (T.toLower . T.pack) cws
    let xs = filter (\w -> any (\e -> T.isInfixOf e w) es) ws
    
    pure $ map T.unpack xs 

    where         
        f a c = case c of 
                    Word(w) -> a ++ [w] 
                    _ -> a
             


