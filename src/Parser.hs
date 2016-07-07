module Parser  where

import TreeSimple
import Text.ParserCombinators.Parsec
symbol :: Parser Char
symbol = oneOf "#!$%&|+-/*:<=>?@^_~"
------------------------------------------------------------------
-----------------------------------------------------------------

doParse :: String -> TreeS
doParse x = case  (parse parseFile "tree" x ) of
		Left err  -> (Leaf 0)
		Right xs  -> xs
{-
instance Read TreeS where
	read = doParse
-}

parseFile :: Parser TreeS
parseFile = do 
	tree <- parseTree
	anyToken 
	eof
	return tree

parseTree :: Parser TreeS
parseTree = parseNode <|> parseLeaf 

parseNode :: Parser TreeS
parseNode = do
	try (char '(')
	l<-parseTree 
	char ' '
	r<-parseTree 
	char ')'
	return (Node l r)

parseLeaf :: Parser TreeS
parseLeaf = do
	num <- many1 digit
	return (Leaf (read num))
	

