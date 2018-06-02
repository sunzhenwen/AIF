module Unification where
import Data.List
import System.IO
import Data.Typeable
import qualified Data.Map as Map


data Term = Variable String
           |Composed String [Term]
            deriving (Eq,Show)

type Variable = String
type Subst = Map.Map Variable Term

{--Function Delta--}
substitution :: (Term, Term, [(Term,Term)]) -> [(Term,Term)]

substitution (_, _, []) = []

substitution (Variable s, Variable t, (Variable s1, t1):rest) = 
         if s == s1 
            then (Variable s1, Variable t) : substitution (Variable s, Variable t, rest)
            else if (Variable s) == t1 
                    then (Variable s1, Variable t) : substitution (Variable s, Variable t, rest)
                    else (Variable s1, t1) : substitution (Variable s, Variable t, rest)
                  
substitution (Variable s, Composed t ts, (Variable s1, t1):rest) = 
        if s == s1 
             then (Composed t ts, t1) : substitution (Variable s, Composed t ts, rest)
             else (Variable s1, t1) : substitution (Variable s, Composed t ts, rest)

substitution (Variable s, Composed t ts, (Composed s1 ss1, t1):rest) = 
            (Composed s1 (substitutionList (Variable s, Composed t ts, ss1)),t1) : substitution (Variable s, Composed t ts, rest)

substitution (Variable s, Variable t, (Composed s1 ts, t1):rest) = 
        (Composed s1 (substitutionList (Variable s, Variable t, ts)), t1) : substitution ((Variable s, Variable t, rest))

substitution (_,_,ss) = error "Wrong arguments ..."

--substitution all elements in a Term list with another Term if those elements satisfy the condition
substitutionList :: (Term, Term, [Term]) -> [Term]

substitutionList (Variable s, _, []) = []

substitutionList (Variable s, Variable t, (Variable ts1):rest) = 
        if s == ts1 
            then (Variable t): substitutionList (Variable s, Variable t, rest )
            else (Variable ts1) : substitutionList (Variable s, Variable t, rest )

substitutionList (Variable s, Variable t, (Composed s1 ts):rest) = 
        (Composed s1 (substitutionList (Variable s, Variable t, ts))): substitutionList (Variable s, Variable t, rest)

substitutionList (Variable s, Composed t tt, (Composed s1 ts):rest) = 
        (Composed s1 (substitutionList (Variable s, Composed t tt, ts))): substitutionList (Variable s, Composed t tt, rest)

substitutionList (Variable s, Composed t tt, (Variable ts1):rest) = 
        if s == ts1 
            then (Composed t tt): substitutionList (Variable s, Composed t tt, rest )
            else (Variable ts1) : substitutionList (Variable s, Composed t tt, rest )

-- verify if a Term exists in a Term list
containsWith :: Term -> [Term] -> Bool

containsWith (Variable s) [] = False

containsWith (Variable s) (Variable s1 : rest) = 
             if s == s1
                then True
                else containsWith (Variable s) rest

containsWith (Variable s) (Composed t ts : rest) = 
             (containsWith (Variable s) ts) || (containsWith (Variable s) rest)

toTermList :: [(Variable,Term)] -> [(Term, Term)]
toTermList [] = []
toTermList ((v,t):rest) = (Variable v, t): (toTermList rest)

subs :: [(Term, Term)] -> Subst
subs  ((Variable s, t):rest)= 
       Map.insert s t (subs rest)
subs [] = Map.empty
             
-- The most general unification main function---
   
unifyM :: ([(Term, Term)], Subst) -> Maybe Subst

unifyM ([], subst) = Just subst

unifyM ((Variable s, Variable t):rest,subst) = 
        if s==t 
           then unifyM (rest, subst)
           else unifyM (substitution (Variable s, Variable t, rest), subs ((Variable s, Variable t): substitution (Variable s, Variable t, toTermList (Map.toList subst))) )

unifyM ((Composed s ss, Composed t ts):rest,subst) = 
        if s == t && (length ss) == (length ts)
           then unifyM ((zip ss ts) ++ rest, subst)
           else Nothing

unifyM ((Variable s, Composed t ts):rest,subst) = 
        if containsWith (Variable s) ts 
           then Nothing
           else unifyM (substitution (Variable s, Composed t ts, rest),subs ( (Variable s, Composed t ts): substitution (Variable s, Composed t ts, toTermList (Map.toList subst))) ) 

unifyM ((Composed s ss, Variable t):rest,subst) = 
        if containsWith (Variable t) ss 
           then Nothing
           else unifyM (substitution (Variable t, Composed s ss, rest), subs ( (Variable t, Composed s ss): substitution (Variable s, Composed s ss, toTermList (Map.toList subst))) )










