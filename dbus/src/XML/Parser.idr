module XML.Parser

import Parser

%access private

public record XMLNode : Type where
  MkXMLNode
    :  (nodeName : String)
    -> (nodeProperties : List (String, String))
    -> (nodeChildren : List XMLNode)
    -> XMLNode

public XML : Type
XML = List XMLNode

nodeNameParser : Parser String
nodeNameParser =
  (map pack $ many (alphaNum <|> anyOf ['_', '-'])) <$ whitespaces

nodePropertyValueParser : Parser String
nodePropertyValueParser =
  (return $ pack $ !(char '"') :: !(many (noneOf ['"'])) ++ [!(char '"')]) <$ whitespaces

nodePropertyNameParser : Parser String
nodePropertyNameParser = nodeNameParser

nodePropertyParser : Parser (String, String)
nodePropertyParser = [| (\n,_,v => (n, v)) nodePropertyNameParser (char '=' $> whitespaces) nodePropertyValueParser |]

commentParser : Parser ()
commentParser = do
  string "<!--"
  stuff
  whitespaces
 where stuff : Parser ()
       stuff = string "-->" <|> (anyChar $> stuff)

commentsParser : Parser ()
commentsParser = many commentParser $> return ()

nodeParser : Parser XMLNode
nodeParser = do
  commentsParser
  langle
  n <- nodeNameParser
  ps <- many nodePropertyParser
  children <-  (crangle $> pure (the (List XMLNode) []))
           <|> (rangle $> many nodeParser <$ clangle <$ string n <$ whitespaces <$ rangle)
  commentsParser
  return $ MkXMLNode n ps children
 where langle : Parser ()
       langle = char '<' $> whitespaces
       rangle : Parser ()
       rangle = char '>' $> whitespaces
       clangle : Parser ()
       clangle = string "</" $> whitespaces
       crangle : Parser ()
       crangle = string "/>" $> whitespaces

-- TODO: Hardcoded from dbus output
doctypeParser : Parser ()
doctypeParser = do
  string "<!DOCTYPE node PUBLIC"
  whitespaces
  nodePropertyValueParser
  nodePropertyValueParser
  char '>'
  whitespaces

xmlParser : Parser XML
xmlParser = whitespaces $> optional doctypeParser $> many nodeParser <$ eof

public parseXML : String -> Maybe XML
parseXML = parse xmlParser
