\begin{code}
{-# LANGUAGE DerivingStrategies #-}

module Scratchpad where

data Plan = A | B | C | D | E | F
  deriving stock (Eq, Ord, Show, Read)

basicADDBenefit :: Num a => Plan -> a
basicADDBenefit = \case
  A -> 100000
  B -> 200000
  C -> 300000
  D -> 500000
  E -> 750000
  F -> 1000000

-- -- Table Sect. 11.1
-- medicalReimbursementBenefit :: Num a => Plan -> a
-- medicalReimbursementBenefit = \case
--   A -> 2000
--   B -> 2500e
--   C -> 3000
--   D -> 4000
--   E -> 5000

\end{code}

{-
The data schema:

Pre-form:
* dob
* nationality
* plan type
* number of policy step-ups
* policy ended as per clause 16
* past ADD payouts

1. death and non-death:
    SNG, plan d, 0 step-up, policy acitve, no past ADD payouts,
    death and loss of sight in both eyes and all hearing in one ear

\begin{code}
saADD :: Integer
saADD = (saPlan 
          * maximum [1]    -- only death benefit since die within 30 days
          )
  where saPlan = basicADDBenefit F 

{-
>>> saADD
> 1000000

-}


-- death-sight-hearing-die-after-month

saADD_afterm_mo = saPlan  * 1.5  
  where saPlan = basicADDBenefit F 

{-

>>> saADD_afterm_mo


2. teeth edge case

3. reductions



-}
\end{code}