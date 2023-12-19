module Parser (parseInput) where

import Data.Void (Void)
import Data.Text (Text)
import Data.Functor
import Data.Bifunctor
import Control.Applicative hiding (some, many)
import qualified Data.Map as M
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import Types

type Parser = Parsec Void Text

workflowName :: Parser WorkflowName
workflowName = some lowerChar <&> T.pack <&> WorkflowName

var :: Parser Var
var = choice $ zipWith ($>) (char <$> "xmas") allVars

comparison :: Parser Comparison
comparison = (char '<' $> Lt) <|> (char '>' $> Gt)

condition :: Parser Condition
condition = liftA3 Condition var comparison decimal

action :: Parser Action
action = (char 'A' $> Accept) <|> (char 'R' $> Reject) <|> (workflowName <&> Goto)

kvPair :: Parser a -> Char -> Parser b -> (a -> b -> c) -> Parser c
kvPair key sep value f = [ f k v | k <- key, _ <- char sep, v <- value ]

conditionalRule :: Parser Rule
conditionalRule = kvPair condition ':' action (Rule . Just)

rule :: Parser Rule
rule = try conditionalRule <|> (action <&> Rule Nothing)

workflow :: Parser [Rule]
workflow = rule `sepBy` char ','

namedWorkflow :: Parser (WorkflowName, [Rule])
namedWorkflow = liftA2 (,) workflowName (between (char '{') (char '}') workflow)

partField :: Parser (Var, Int)
partField = kvPair var '=' decimal (,)

part :: Parser Part
part = between (char '{') (char '}') (partField `sepBy` char ',') <&> M.fromList <&> Part

input :: Parser (System, [Part])
input = do
  sys <- M.fromList <$> namedWorkflow `sepEndBy` eol <* eol
  parts <- part `sepEndBy` eol <* eof
  pure (sys, parts)

parseInput :: Text -> Either String (System, [Part])
parseInput = first errorBundlePretty . parse input "input"
