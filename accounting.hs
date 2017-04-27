module Accounting where

import Data.Number.Nat
import Data.Group

data TAccount = TAccount Nat Nat deriving Show

instance Eq TAccount where
  (TAccount leftDebit leftCredit) == (TAccount rightDebit rightCredit) = leftDebit + rightCredit == leftCredit + rightCredit

instance Monoid TAccount where
  (TAccount leftDebit leftCredit) `mappend` (TAccount rightDebit rightCredit) = reduce $ TAccount (leftDebit + rightDebit) (leftCredit + rightCredit)
  mempty = TAccount 0 0

instance Group TAccount where
  invert (TAccount debit credit) = TAccount credit debit

isInReducedForm (TAccount debit credit) = debit == 0 || credit == 0
isZeroTerm (TAccount debit credit) = debit == credit

reduce (TAccount debit credit) | debit < credit = (TAccount 0 (credit - debit))
                               | otherwise = (TAccount (debit - credit) 0)

isBalanced :: [TAccount] -> Bool
isBalanced = (== mempty) . mconcat
