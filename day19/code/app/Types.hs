module Types 
  ( WorkflowName(..)
  , Var(..)
  , Comparison(..)
  , Condition(..)
  , Action(..)
  , Rule(..)
  , System
  , Part(..)
  , Range(..)
  , fullRange
  , inRange
  , allVars
  , flipOp
  , flipCond
  , applyCond
  , partScore
  ) where

import Data.Map
import qualified Data.Text as T

newtype WorkflowName = WorkflowName T.Text deriving (Show, Eq, Ord)
data Var = X | M | A | S deriving (Show, Eq, Ord)
data Comparison = Lt | Gt | Le | Ge deriving Show
data Condition = Condition Var Comparison Int deriving Show
data Action = Accept | Reject | Goto WorkflowName deriving Show
data Rule = Rule (Maybe Condition) Action deriving Show
type System = (Map WorkflowName [Rule])
newtype Part = Part (Map Var Int) deriving Show
data Range = Range Int Int deriving Show

fullRange :: Range
fullRange = Range 1 4000

inRange :: Range -> Int -> Bool
inRange (Range low high) x = x >= low && x <= high

allVars :: [Var]
allVars = [X, M, A, S]

flipOp :: Comparison -> Comparison
flipOp Lt = Ge
flipOp Gt = Le
flipOp Le = Gt
flipOp Ge = Lt

flipCond :: Condition -> Condition
flipCond (Condition var op int) = Condition var (flipOp op) int

applyCond :: Condition -> Range -> Range
applyCond (Condition _ Lt x) (Range low high) = Range low (high `min` (x - 1))
applyCond (Condition _ Le x) (Range low high) = Range low (high `min` x)
applyCond (Condition _ Gt x) (Range low high) = Range (low `max` (x + 1)) high
applyCond (Condition _ Ge x) (Range low high) = Range (low `max` x) high

partScore :: Part -> Int
partScore (Part vars) = sum $ (vars !) <$> allVars
