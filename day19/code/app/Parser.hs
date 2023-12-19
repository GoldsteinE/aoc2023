module Parser (parseInput) where

import Data.Void (Void)
import Data.Text (Text)
import Data.Functor
import Control.Applicative hiding (some, many)
import qualified Data.Map as M
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char

import Types

type Parser = Parsec Void Text

workflowName :: Parser WorkflowName
workflowName = some lowerChar <&> T.pack <&> WorkflowName

var :: Parser Var
var = (char 'x' $> X) <|> (char 'm' $> M) <|> (char 'a' $> A) <|> (char 's' $> S)

comparison :: Parser Comparison
comparison = (char '<' $> Lt) <|> (char '>' $> Gt)

int :: Parser Int
int = some digitChar <&> read

condition :: Parser Condition
condition = liftA3 Condition var comparison int

action :: Parser Action
action = (char 'A' $> Accept) <|> (char 'R' $> Reject) <|> (workflowName <&> Goto)

conditionalRule :: Parser Rule
conditionalRule = do
  cond <- condition
  _ <- char ':'
  act <- action
  pure $ Rule (Just cond) act

rule :: Parser Rule
rule = (try conditionalRule) <|> (action <&> Rule Nothing)

workflow :: Parser [Rule]
workflow = (rule `sepBy` char ',')

namedWorkflow :: Parser (WorkflowName, [Rule])
namedWorkflow = liftA2 (,) workflowName (between (char '{') (char '}') workflow)

system :: Parser System
system = namedWorkflow `sepEndBy` eol <&> System . M.fromList

partField :: Parser (Var, Int)
partField = do
  v <- var
  _ <- char '='
  num <- int
  pure (v, num)

part :: Parser Part
part = between (char '{') (char '}') (partField `sepBy` char ',') <&> M.fromList <&> Part

input :: Parser (System, [Part])
input = do
  sys <- system
  _ <- eol
  parts <- part `sepEndBy` eol
  _ <- eof
  pure (sys, parts)

parseInput :: Text -> Either String (System, [Part])
parseInput inp = case parse input "input" inp of
  Right val -> Right val
  Left err -> Left $ errorBundlePretty err
