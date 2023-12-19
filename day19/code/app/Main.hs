{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map hiding (empty, foldr, filter)
import System.Exit
import System.Environment
import Data.Functor
import Control.Applicative
import Control.Monad.Logic
import Control.Monad.Writer
import qualified Data.Text as T

import Parser
import Types

groupOnKey :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupOnKey f = toList . fromListWith (<>) . fmap (\x -> (f x, [x]))

checkWorkflow :: System -> [Rule] -> WriterT [Condition] Logic ()
checkWorkflow sys [] = pure ()
checkWorkflow sys@(System wfs) (rule:rest) = case rule of
    Rule Nothing Reject -> empty
    Rule Nothing Accept -> pure ()
    Rule (Just cond) Reject -> flipCond cond ->> checkWorkflow sys rest
    Rule (Just cond) Accept -> tell [cond] <|> flipCond cond ->> checkWorkflow sys rest
    Rule Nothing (Goto workflow) -> let (Workflow rules) = wfs ! workflow in checkWorkflow sys rules
    Rule (Just cond) (Goto workflow) -> let (Workflow rules) = wfs ! workflow in
      flipCond cond ->> checkWorkflow sys rest <|> cond ->> checkWorkflow sys rules
  where
    infixl 4 ->>
    x ->> n = tell [x] >> n

mergeConditions :: [Condition] -> Map Var Range
mergeConditions conditions =
  let
    perVar = groupOnKey (\(Condition var _ _) -> var) conditions
    mergeVarConditions (var, conditions) = (var, foldr applyCond fullRange conditions)
    merged = fromList $ mergeVarConditions <$> perVar
    defaults = fromList $ (,fullRange) <$> allVars
  in union merged defaults

groupOptions :: Map Var Range -> Int
groupOptions ranges = product $ (\(_, Range lo hi) -> hi - lo + 1) <$> toList ranges

checkPart :: Foldable m => m (Map Var Range) -> Part -> Bool
checkPart ranges (Part vars) = any check ranges
  where
    check ranges = all (\var -> inRange (ranges ! var) (vars ! var)) allVars

main :: IO ()
main = do
  isPart2 <- getArgs <&> (== ["2"])
  rawInput <- getContents
  (sys, parts) <- case parseInput (T.pack rawInput) of
    Right inp -> pure inp
    Left err -> die err
  let (System wfs) = sys
  let (Workflow firstWf) = wfs ! WorkflowName "in"
  let conditions = checkWorkflow sys firstWf
  let ranges = mergeConditions <$> execWriterT conditions
  if isPart2 then
    print . sum $ groupOptions <$> ranges
  else do
    print . sum $ partScore <$> filter (checkPart ranges) parts
