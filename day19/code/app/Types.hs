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

import Data.Map (Map, (!))
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
flipOp = \case { Lt -> Ge; Gt -> Le; Le -> Gt; Ge -> Lt }

flipCond :: Condition -> Condition
flipCond (Condition var op int) = Condition var (flipOp op) int

applyCond :: Condition -> Range -> Range
applyCond (Condition _ op x) (Range low high) = case op of
  Lt -> Range low (high `min` pred x)
  Le -> Range low (high `min` x)
  Gt -> Range (low `max` succ x) high
  Ge -> Range (low `max` x) high

partScore :: Part -> Int
partScore (Part vars) = sum $ (vars !) <$> allVars
