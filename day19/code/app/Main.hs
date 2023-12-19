{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

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

walkWorkflow :: (?sys :: System) => [Rule] -> WriterT [Condition] Logic ()
walkWorkflow [] = pure ()
walkWorkflow (rule:rest) = case rule of
    Rule Nothing Reject -> empty
    Rule Nothing Accept -> pure ()
    Rule (Just cond) Reject -> flipCond cond ->> walkRest
    Rule (Just cond) Accept -> tell [cond] <|> flipCond cond ->> walkRest
    Rule Nothing (Goto next) -> walk next
    Rule (Just cond) (Goto next) -> cond ->> walk next <|> flipCond cond ->> walkRest
  where
    infixl 4 ->>
    x ->> n = tell [x] >> n
    walk next = walkWorkflow (?sys ! next)
    walkRest = walkWorkflow rest

mergeConditions :: [Condition] -> Map Var Range
mergeConditions conditions =
  let
    perVar = fromListWith (<>) $ (\c@(Condition var _ _) -> (var, [c])) <$> conditions
    merged = foldr applyCond fullRange <$> perVar
    defaults = fromList $ (,fullRange) <$> allVars
  in merged `union` defaults

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
  let conditions = let ?sys = sys in walkWorkflow $ sys ! WorkflowName "in"
  let ranges = mergeConditions <$> execWriterT conditions
  if isPart2 then
    print . sum $ groupOptions <$> ranges
  else do
    print . sum $ partScore <$> filter (checkPart ranges) parts
