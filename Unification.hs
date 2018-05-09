module Unification where
import Data.List
import System.IO
import Data.Typeable
import qualified Data.Map as Map


{-tests = 
 [unifyM ([(Composed "ikonws" [Variable "a2", Variable "a1"],Composed "ikonws" [Variable "a3", Variable "a4"])],Map.fromList []), 
  unifyM ([(Variable "a1",Variable "a2"),(Variable "b1",Variable "b2")],Map.fromList []) ]

main = do putStr $ concatMap show tests
-}

data Term = Variable String
           |Composed String [Term]
            deriving (Eq,Show)

type Variable = String
type Subst = Map.Map Variable Term


{- Comment Sebastian:

Still the replace?? Wouldn't we have gone to real substitution by now?

Also replace is a mess that does not work, e.g. 

replace (Variable "x", Variable "y", [(Variable "x", Composed "c" [])]) = [(Variable "x", Composed "c" [])]

i.e. failing to replace "x" by "y". 

That code looks really terrible!
-}

replace :: (Term, Term, [(Term,Term)]) -> [(Term,Term)]

replace (_, _, []) = []

replace (Variable s, Variable t, (Variable s1, t1):rest) = 
        if Variable s == t1 
             then (Variable s1, Variable t) : replace (Variable s, Variable t, rest)
             else if t == s1
                  then (Variable s, t1) : replace (Variable s, Variable t, rest)
                  else (Variable s1, t1) : replace (Variable s, Variable t, rest)

replace (Variable s, Composed t ts, (Variable s1, t1):rest) = 
        if s == s1 
             then (Composed t ts, t1) : replace (Variable s, Composed t ts, rest)
             else (Variable s1, t1) : replace (Variable s, Composed t ts, rest)

replace (Variable s, Composed t ts, (Composed s1 ss1, t1):rest) = 
            (Composed s1 (replaceList (Variable s, Composed t ts, ss1)),t1) : replace (Variable s, Composed t ts, rest)

replace (Variable s, Variable t, (Composed s1 ts, t1):rest) = 
        (Composed s1 (replaceList (Variable s, Variable t, ts)), t1) : replace ((Variable s, Variable t, rest))

replace (_,_,ss) = error "Wrong arguments ..."

--replace all elements in a Term list with another Term if those elements satisfy the condition
replaceList :: (Term, Term, [Term]) -> [Term]

replaceList (Variable s, _, []) = []

replaceList (Variable s, Variable t, (Variable ts1):rest) = 
        if s == ts1 
            then (Variable t): replaceList (Variable s, Variable t, rest )
            else (Variable ts1) : replaceList (Variable s, Variable t, rest )

replaceList (Variable s, Variable t, (Composed s1 ts):rest) = 
        (Composed s1 (replaceList (Variable s, Variable t, ts))): replaceList (Variable s, Variable t, rest)

replaceList (Variable s, Composed t tt, (Composed s1 ts):rest) = 
        (Composed s1 (replaceList (Variable s, Composed t tt, ts))): replaceList (Variable s, Composed t tt, rest)

replaceList (Variable s, Composed t tt, (Variable ts1):rest) = 
        if s == ts1 
            then (Composed t tt): replaceList (Variable s, Composed t tt, rest )
            else (Variable ts1) : replaceList (Variable s, Composed t tt, rest )

-- verify if a Term exists in a Term list
containsWith :: Term -> [Term] -> Bool

containsWith (Variable s) [] = False

containsWith (Variable s) (Variable s1 : rest) = 
             if s == s1
                then True
                else containsWith (Variable s) rest

containsWith (Variable s) (Composed t ts : rest) = 
             (containsWith (Variable s) ts) && (containsWith (Variable s) rest)


{-  Comment Sebastian:

containsWith does not seem to work, e.g.

containsWith (Variable s) [Composed "f" [Variable s], Composed "c" []]

would return False

Also programming this should have been much easier??

-}

toTermList :: [(Variable,Term)] -> [(Term, Term)]
toTermList [] = []
toTermList ((v,t):rest) = (Variable v, t): (toTermList rest)

subs :: [(Term, Term)] -> Subst
subs  ((Variable s, t):rest)= 
            Map.insert s t (subs rest)
subs [] = Map.empty
             
-- Main function
   
unifyM :: ([(Term, Term)], Subst) -> Maybe Subst

unifyM ([], subst) = Just subst

unifyM ((Variable s, Variable t):rest,subst) = 
        if s==t 
           then unifyM (rest, subst)
           else unifyM (replace (Variable s, Variable t, rest), subs ((Variable s, Variable t): replace (Variable s, Variable t, toTermList (Map.toList subst))) )

unifyM ((Composed s ss, Composed t ts):rest,subst) = 
        if s == t && (length ss) == (length ts)
           then unifyM ((zip ss ts) ++ rest, subst)
           else Nothing

unifyM ((Variable s, Composed t ts):rest,subst) = 
        if containsWith (Variable s) ts 
           then Nothing
           else unifyM (replace (Variable s, Composed t ts, rest),subs ( (Variable s, Composed t ts): replace (Variable s, Composed t ts, toTermList (Map.toList subst))) ) 

unifyM ((Composed s ss, Variable t):rest,subst) = 
        if containsWith (Variable t) ss 
           then Nothing
           else unifyM (replace (Variable t, Composed s ss, rest), subs ( (Variable t, Composed s ss): replace (Variable s, Composed s ss, toTermList (Map.toList subst))) )










