module FixedPoint where
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.IO
import Types
import TypeTree
import Unification
import Control.Monad

{----------------------  FixedPoints calculation  -------------------------------}                          

--Define fixedpoint data type. The fixedpoint is represented as a list of FP
data FP = FP {fpRuleName :: String,
              typeInfo :: Typing,
              identityNo :: Int,
              appliedFact :: AppliedFact,
              fact :: FPFact}
              deriving (Show)

type Typing = Map.Map Variable Typename
type Typename = ( String, TypeInfo ) --type name, a list of Integer
type TypeInfo = [Int]
type AppliedFact = [Int]
 

data FPFact = FPFact {fpFactName :: String, fpArgs :: [Term]}
                      deriving (Show)

data MRule = MRule {mRuleName :: String,
                    mtyping :: Typing,  --variables type
                    mlf :: [FPFact], -- left-hand side facts
                    mrf :: [FPFact] -- right-hand side facts
                    } deriving (Show)

{-  Arguments: ruls, fixedpoint, user type tree, serial number, rename counter number

    Traversing each rule and return entire fixedpoints.
    If the fixedpoint list contains nothing new comparing with the previous one,
    it means no fixedpoints can be derived.
    Otherwise, keep deriving.    
-}
fixedpoint :: [PRule] -> [FP] -> [TypeTree] -> Int -> Int -> [FP]
fixedpoint rules fp userTypes serialNo reNameCounter = 
        let newfp = deriveOneStep rules fp userTypes serialNo reNameCounter
            newfp' = fixedpointComparsion newfp fp
        in if null newfp'
              then fp
              else fixedpoint rules newfp userTypes serialNo reNameCounter
              {-let fp' = fixedpointComparsion fp newfp'
                   in fixedpoint rules (fp' ++ newfp') userTypes serialNo reNameCounter-}
  
{-
    Function Omega
	Arguments: rules, fixedpointed, user type tree, 
               serial number will be the identity No. for fixedpoint, 
               rename counter number is used for rename rules later
    In this function, each rule will be traversed to see if the right-hand side fact can be derived from the exist fixedpoints.
-}  
deriveOneStep :: [PRule] -> [FP] -> [TypeTree] -> Int -> Int -> [FP]
deriveOneStep [] fp userTypes serialNo reNameCounter = fp
deriveOneStep ( pRule : rest ) fp userTypes serialNo reNameCounter = 
               let extendedFP = extendOneRuleToFP pRule fp userTypes serialNo reNameCounter
                   extendedSerialNo = maxSerialNo extendedFP
               in deriveOneStep rest extendedFP userTypes (extendedSerialNo+1) (reNameCounter+1)

--Arguments : new fixedpoint, old fixedpoint, new' that are not coverd by old fixedPoint
fixedpointComparsion :: [FP] -> [FP] -> [FP]
fixedpointComparsion [] _ = []
fixedpointComparsion (newFP:rest) fps = 
                     if isCovered newFP fps 
                        then fixedpointComparsion rest fps 
                        else newFP : fixedpointComparsion rest fps 

isCovered :: FP -> [FP] -> Bool
isCovered fp [] = False
isCovered (FP rName typing identityNo appliedFact (FPFact factName terms)) ( (FP rName' typing' identityNo' appliedFact' (FPFact factName' terms')):rest ) = 
          let term = Composed "" terms
              fpTerm = Composed "" terms'
              comp = compareTerms term fpTerm ( Map.union typing typing' )
          in if comp /= Equal || factName /= factName'
                          then isCovered (FP rName typing identityNo appliedFact (FPFact factName terms)) rest
                          else True

--The max serial no in fixedpoints                           
maxSerialNo :: [FP] -> Int
maxSerialNo [] = 0
maxSerialNo ( (FP _ _ serialNo _ _):rest ) = 
              let nextSerialNo = maxSerialNo rest
              in if serialNo > nextSerialNo
                    then serialNo
                    else nextSerialNo

                              
extendOneRuleToFP :: PRule -> [FP] -> [TypeTree] -> Int -> Int -> [FP]
extendOneRuleToFP pRule fp userTypes serialNo reNameCounter = 
                  let mRule = convertToMRule pRule userTypes
                      (MRule ruleName typing mlf mrf) = rename mRule reNameCounter 
                  in unifyLHF ruleName typing mlf mrf fp serialNo
                  
--rename all the variables in MRule in a pattern like "A1,A2,...,An"                  
rename :: MRule -> Int -> MRule
rename (MRule ruleName typing mlf mrf) reNameCounter = 
         let mapList = Map.toList typing
             renamedTyping = Map.fromList (map (reNameTyping reNameCounter) mapList)
         in MRule ruleName renamedTyping (map (reNameFPFact reNameCounter) mlf) (map (reNameFPFact reNameCounter) mrf)

reNameTyping :: Int -> (Variable, Typename) -> (Variable, Typename)
reNameTyping reNameCounter (s, typeName) = ( (s++(show reNameCounter)), typeName )
         
reNameFPFact :: Int -> FPFact -> FPFact
reNameFPFact reNameCounter (FPFact fName terms) =
               (FPFact fName (map (reNameTerm reNameCounter) terms))

reNameTerm :: Int -> Term -> Term
reNameTerm reNameCounter (Variable v) = Variable (v ++ (show reNameCounter))
reNameTerm reNameCounter (Composed c cs) = Composed c (map (reNameTerm reNameCounter) cs)
        
--convert PRule to MRule so that the data becomes self defined data
convertToMRule :: PRule -> [TypeTree] -> MRule
convertToMRule (PRule rName _ lf _ _ _ rf _) userTypeTree = 
               MRule rName (getTyping lf userTypeTree) (convertToTerms lf) (convertToTerms rf) 

getTyping :: [PFact] -> [TypeTree] -> Typing
getTyping [] userTypeTree = Map.empty
getTyping ( (PFact fName [PComp cName cArgs]):rest ) userTypeTree = getTyping rest userTypeTree
getTyping ( (PFact fName fArgs):rest ) userTypeTree = 
           if fName /= "iknows" && fName /= "occurs" && fName /= "timplies"
              then let [PAtom aName] = fArgs 
                   in Map.insert aName (fName, (prefix fName userTypeTree 0 )) (getTyping rest userTypeTree)           
              else getTyping rest userTypeTree

convertToTerms :: [PFact] -> [FPFact]
convertToTerms [] = []
convertToTerms ( (PFact fName fArgs):rest ) = 
                 if fName == "iknows" || fName == "occurs" || fName == "timplies" || fName == "attack"
                    then (FPFact fName (map transferPTermToTerm fArgs)) : convertToTerms rest 
                    else convertToTerms rest                 
                 
transferPTermToTerm :: PTerm -> Term
transferPTermToTerm (PAtom aName) = (Variable aName)
transferPTermToTerm (PComp cName cArgs) = 
               (Composed cName (map transferPTermToTerm cArgs))               

{- Arguments: rule name, variables type information, left-hand side facts,
              right-hand side facts, existing fixedpoints, serial number
   
   The main functionality is unifying left-hand side facts with fixedPoint facts.
   If left-hand side facts can be unified or there are no fixedpoints had been derived, 
   the right-hand side facts will be substituted and add to current fixedpoints. 
   Otherwise,return current fixedpoints.
-}
unifyLHF :: String -> Typing -> [FPFact] -> [FPFact] -> [FP] -> Int -> [FP]
unifyLHF rName typing [] mrf fp serialNo = addFactToFP rName typing mrf fp serialNo []
unifyLHF rName typing mlf mrf fp serialNo = 
                let unifyResults = unifySubset typing mlf fp
                in if isNothing unifyResults
                      then fp 
                      --substitute right hand side facts
                      else  concatSubstitutionToLHSFact rName mrf fp serialNo (fromJust unifyResults)


{-
    Arguments: the rule name, right-hand side facts, current fixedPoints, serial No, 
               a list of 3-tuple contains substitution, variables type information, 
               and a list of identity numbers representing which fixedPoint facts the left-hand side facts unified with .
    
    The right-hand facts are substituted by each 3-tuple's substitution and unified with current fixedpoints facts.
    If they can not be unified, those substituted right-hand facts will be add to current fixedpoint facts.
    Otherwise, it means those facts had bee derived.     
           
-}
concatSubstitutionToLHSFact :: String -> [FPFact] -> [FP] -> Int -> [(Subst, Typing, AppliedFact)] -> [FP]
concatSubstitutionToLHSFact _ _ fp _ [] = fp
concatSubstitutionToLHSFact rName mrf fp serialNo ((subst, typing, appliedFact):rest) = 
                          let newFP = addFactToFP rName typing (map (substitute typing subst) mrf) fp serialNo appliedFact
                              newSerialNo = maxSerialNo newFP
                          in concatSubstitutionToLHSFact rName mrf newFP (newSerialNo+1) rest

{-
    Arguments: variables type information, substitution, self defined fact (FPFact)
    
    Substituting all variables occur in the fact based on the substitution and variables types.
    The untyped variables can be substituted by either variable or constants or terms.
    The typed variables can only be substituted by typed variables.        
-}
substitute :: Typing -> Subst -> FPFact -> FPFact
substitute typing subst (FPFact factName terms) = 
          FPFact factName (map (substituteTerms typing subst) terms)

substituteTerms :: Typing -> Subst -> Term -> Term     
substituteTerms typing subst (Variable v) = 
          let sub = Map.lookup v subst           
          in case sub of
             Nothing -> Variable v
             Just (Composed c cs) -> let typeInfo = Map.lookup v typing
                                     in case typeInfo of 
                                        Nothing -> Composed c cs
                                        Just typingName -> Variable v                                       
             Just (Variable s) -> Variable s             
substituteTerms typing subst (Composed cName cArgs) = 
              (Composed cName (map (substituteTerms typing subst) cArgs) ) 

---Unify left hand side facts
{-
    Arguments: variables type information, left-hand side facts, current fixedpoints
    
    This function unifies left-hand side fact with fixedpoint facts and return a Maybe 3-tuple.
    For example, the left-hand side fact is:
                 iknows("A1"), and  "A1" is type of Agent ("A1":Agent).
                 the fixedpoint fact is :
                 iknows("A0"), and identity number is 1, "A0":Honest.
                 the result will be Just [(Map.fromlis("A1",Variable "A0"), ("A0",[0,1,12]), [1] )],
                 it means variable "A1" can be substituted by variable "A0", and "A0"'s type is Honest, the left-hand fact is unified by identity No. 1 fixedpoint fact.
    A fact can be unified by several fixedpoint facts, hence we use a list to keep the unification results.
    If left-hand facts can not be unified, the right-hand side facts are underivable. The function will return Nothing.
-}
unifySubset :: Typing -> [FPFact] -> [FP] -> Maybe [(Subst, Typing, AppliedFact)]
unifySubset typing [] fp = Just []
unifySubset typing (fpFact:rest) fp = 
                     let substResult = unifyElement typing fp fpFact
                     in if null substResult
                           then Nothing
                           else let unificatonRestult = unifyRestLFSubSet typing rest fp substResult
                                in if null unificatonRestult
                                   then Nothing
                                   else Just unificatonRestult

{-
   Unifying the first fact of left-hand side fact with facts the fixedPoint contains.
   The result is a list of 3-tuple which contains a substitution, variables type information, 
   and a list of integers representing the identity numbers of facts the fixedpoint contains.
   This result will be used to substitute the rest of left-hand side facts and unify them with fixedpoint facts (this is what unifyRestLFSubSet do).    
-}
unifyElement :: Typing -> [FP] -> FPFact -> [(Subst, Typing, AppliedFact)]
unifyElement typing [] fpFact = []
unifyElement typing ((FP rName fpTyping serialNo fpAppliedFact (FPFact fpFactName fpTerms)):rest) (FPFact factName terms) = 
                     if factName == fpFactName
                        then let term = Composed "" terms
                                 fpTerm = Composed "" fpTerms
                                 comp = compareTerms term fpTerm (Map.union typing fpTyping)
                                 restSubResults = unifyElement typing rest (FPFact factName terms)
                             in case comp of 
                                Disjoint -> restSubResults
                                Greater -> let subsResult = unifyM ((zip terms fpTerms),Map.fromList [])
                                           in if fpFactName == "occurs"
                                                 then (fromJust subsResult, fpTyping, []) : restSubResults
                                                 else (fromJust subsResult, fpTyping, [serialNo]) : restSubResults
                                _ -> let subsResult = unifyM ((zip terms fpTerms),Map.fromList [])
                                     in if fpFactName == "occurs"
                                           then (fromJust subsResult, substituteType fpTyping typing (fromJust subsResult), []) : restSubResults
                                           else (fromJust subsResult, substituteType fpTyping typing (fromJust subsResult), [serialNo]) : restSubResults
                        else unifyElement typing rest (FPFact factName terms)

unifyRestLFSubSet :: Typing -> [FPFact] -> [FP] -> [(Subst, Typing, AppliedFact)] -> [(Subst, Typing, AppliedFact)]
unifyRestLFSubSet typing [] _ subsitutionInfo = subsitutionInfo
unifyRestLFSubSet typing _ _ [] = []
unifyRestLFSubSet typing fpFacts fp ( (subst, substTyping, appliedFact):rest)  =
                  let unionTyping = Map.union typing substTyping
                      substFPFact = map (substitute unionTyping subst) fpFacts
                      substFPFactResult = unifyEachRestLFSubSet typing (subst, substTyping, appliedFact) fp substFPFact 
                  in if isNothing substFPFactResult
                     then unifyRestLFSubSet typing fpFacts fp rest
                     else (fromJust substFPFactResult) : unifyRestLFSubSet typing fpFacts fp rest

unifyEachRestLFSubSet :: Typing -> (Subst, Typing, AppliedFact) -> [FP] -> [FPFact] -> Maybe (Subst, Typing, AppliedFact)
unifyEachRestLFSubSet typing substitutionInfo fp [] = Just substitutionInfo
unifyEachRestLFSubSet typing (subst, substTyping, appliedFact) fp ((fpFact):rest) = 
                      let newTyping = substituteType typing substTyping subst
                          substFPFactResults = unifyElement (Map.union substTyping newTyping) fp fpFact
                      in if null substFPFactResults
                            then Nothing
                            else let (substUnion, typingUnion, appliedFactUnion) = unionResults (subst, substTyping, appliedFact) substFPFactResults
                                 in  unifyEachRestLFSubSet typing (substUnion, typingUnion, appliedFactUnion) fp rest
                     
unionResults :: (Subst, Typing, AppliedFact) -> [(Subst, Typing, AppliedFact)] -> (Subst, Typing, AppliedFact)
unionResults subsitutionInfo [] = subsitutionInfo
unionResults (subst, typing, appliedFact) ( ((subst', typing', appliedFact')):rest ) = 
             unionResults (Map.union subst subst', Map.union typing typing', union appliedFact appliedFact') rest 

{-
    The substitution does not take type into account.
    For example, suppose Variable "A" can be substituted by Variable "B".
    We need compare the type of Variable "A" and Variable "B" and keep the less general one.
    Since the comparison has been done by previous computation (unifyElement and unifyEachRestLFSubSet), 
    the substituteType just replace the variable "B"'s type by Variable "A".
-}             
substituteType :: Typing -> Typing -> Subst -> Typing
substituteType fpTyping typing subst =  
              Map.fromList (map (substituteTermType (Map.toList typing) subst) (Map.toList fpTyping) )

substituteTermType :: [(Variable, Typename)] -> Subst -> (Variable, Typename) -> (Variable, Typename)
substituteTermType [] subst (fpS,fpTypeName) = (fpS,fpTypeName)
substituteTermType ((s,typeName):restTyping) subst (fpS,fpTypeName) = 
                  case Map.findWithDefault (Composed "" []) s subst of
                       Composed c cs -> substituteTermType restTyping subst (fpS,fpTypeName)                  
                       Variable v -> if v == fpS 
                                        then (fpS,typeName)
                                        else substituteTermType restTyping subst (fpS,fpTypeName)
                  
                  
-- Term comparison
{-
    compareTerms s t Γ =  Greater      if [|t|]Γ is subset of [|s|]Γ
                          Smaller      if [|s|]Γ is subset of [|t|]Γ
                          Euqal        if [|t|]Γ = [|s|]Γ
                          Disjoint     if [|t|]Γ ∩ [|s|]Γ = Ø
                          overlap      otherwise
-}
compareTerms :: Term -> Term -> Typing -> Comp
compareTerms s t typing = 
             case unifyM ([(s,t)],Map.fromList []) of 
                  Nothing -> Disjoint 
                  (Just substitution) -> comparison s t substitution typing 

{-
    The comparison takes two terms, substitution and variables type as arguments.
    It returns Greater, Smaller, Equal, Disjoint or Overlap.
    Typed variables comparison is based on variables type.
    Untyped variables are greater than term, constant, or variable.
    Typed variable cannot be compared with term or constant,it will give the result of Disjoint.    
-}
comparison :: Term -> Term -> Subst -> Typing -> Comp
comparison (Composed c cs) (Variable v) substitution typing = 
           case (Map.lookup v typing) of 
                Nothing -> Smaller
                Just typeName -> Disjoint
comparison (Variable v) (Composed c cs) substitution typing =
          case (Map.lookup v typing) of 
                Nothing -> Greater
                Just typeName -> Disjoint
comparison (Composed c1 cs1) (Composed c2 cs2) substitution typing = 
             if c1 == c2
                then compareTermList (zip cs1 cs2) substitution typing
                else Disjoint
comparison (Variable v1) (Variable v2) substitution typing = 
              if Map.size substitution == 0 || v1 == v2 --it is the identity 
                 then Equal
                 else let result1 = Map.findWithDefault (Composed "-1" []) v1 substitution
                          result2 = Map.findWithDefault (Composed "-1" []) v2 substitution
                      in if result1 == (Variable v2) || result2 == (Variable v1)
                         then compareVariableTypes typing (Variable v1) (Variable v2)
                         else if result1 == result2 && result1 /= (Composed "-1" []) && result2 /= (Composed "-1" [])
                              then compareVariableTypes typing (Variable v1) result1
                              else error ("This should not have happened " ++ v1 ++ " -> " ++ v2 ++ " \n subs: " ++ (show substitution) ++ " \n typing: " ++ (show typing) )
                 
compareVariableTypes :: Typing -> Term -> Term -> Comp
compareVariableTypes typing (Variable v1) (Variable v2) = 
                     let ( typeName1, typeList1 ) = Map.findWithDefault ("NotFind",[]) v1 typing
                         ( typeName2, typeList2 ) = Map.findWithDefault ("NotFind",[]) v2 typing
                     in if typeName1 /= "NotFind" && typeName2 /= "NotFind"
                           then compPos typeList1 typeList2
                           else Disjoint                   
{-
    If unifying two term lists, zip them to [(Term,Term)] and compare each term tuple.
-}           
compareTermList :: [(Term,Term)] -> Subst -> Typing -> Comp
compareTermList [] subsitution typing = Equal   
compareTermList ((s,t):rest) subsitution typing =   
                 let comp1 = comparison s t subsitution typing
                     comp2 = compareTermList rest subsitution typing
                 in compComparsion comp1 comp2

compComparsion :: Comp -> Comp -> Comp
compComparsion Disjoint _ = Disjoint
compComparsion _ Disjoint = Disjoint
compComparsion Greater Smaller = Overlap
compComparsion Smaller Greater = Overlap
compComparsion _ Overlap = Overlap
compComparsion Overlap _ = Overlap
compComparsion Greater _ = Greater
compComparsion _ Greater = Greater
compComparsion Smaller _ = Smaller
compComparsion _ Smaller = Smaller
compComparsion _ _ = Equal -- Tow arguments are the same Comp type

               
{- arguments: ruleName, variables type, right-hand side facts, fixedpoint facts , serial No, a list of identity number of fixedpoint facts.
   
   Variables in right-hand side facts have been substituted. 
   -If no facts contained in fixedpoint, add right-hand side fact to fixedpoint
   -Otherwise, unify those facts with fixedpoint facts. If they cannot be unified, add to fixedpoint.
-}
addFactToFP :: String -> Typing -> [FPFact] -> [FP] -> Int -> AppliedFact -> [FP]               
addFactToFP rName typing (fpFact : rest) [] identityNo appliedFact = 
            ( FP rName typing identityNo appliedFact fpFact ) : (addFactToFP rName typing rest [] (identityNo+1) appliedFact )
addFactToFP rName typing [] [] identityNo appliedFact = []
addFactToFP rName typing [] fp identityNo appliedFact = fp
addFactToFP rName typing mrf fp identityNo appliedFact = 
            let unifiedMRF = checkMRF typing fp mrf
            in (addFactToFP rName typing unifiedMRF [] identityNo appliedFact) ++ fp 
 
-- unify each right-hand side facts. 
checkMRF :: Typing -> [FP] -> [FPFact] -> [FPFact]
checkMRF typing fp [] = []
checkMRF typing fp (fpFact:rest) =         
        if isNothing (unifySubset typing [fpFact] fp) 
           then fpFact : checkMRF typing fp rest
           else checkMRF typing fp rest

---------------------------Compute general variables algorithm----------------------------------------------
data Comp = Greater
          | Equal
          | Smaller
          | Disjoint
          | Overlap
          deriving(Eq, Show)

--Is the first argument less general than the second argument.
compPos :: [Int] -> [Int] -> Comp
compPos [] [] = Equal 
compPos _ [] = Smaller 
compPos [] _ = Greater 
compPos (x:xs) (y:ys) = 
        if x == y
          then compPos xs ys
          else Disjoint

prefix :: String -> [TypeTree] -> Int -> [Int]
prefix _ [] _ = []
prefix typeName ((Node s ts):rest) node = 
                      if typeName == s
                         then [node]
                         else if treeBranch typeName ts
                                 then [node] ++ (prefix typeName ts (node*10+1) )
                                 else (prefix typeName rest (node+1) )
       
treeBranch :: String -> [TypeTree] -> Bool
treeBranch _ [] = False
treeBranch typeName ((Node s ts):rest) = 
            if typeName == s
               then True
               else treeBranch typeName ts || treeBranch typeName rest


{-variant unification-} 
varComparison :: Term -> Term -> Typing -> Bool
varComparison (Variable s) (Variable t) typing = 
               let ( typeName1, typeList1 ) = Map.findWithDefault ("NotFind",[]) s typing
                   ( typeName2, typeList2 ) = Map.findWithDefault ("NotFind",[]) t typing
               in if typeName1 /= "NotFind" && typeName2 /= "NotFind" --typed variable
                  then let comp = compPos typeList1 typeList2
                       in case comp of
                            Greater -> True
                            Smaller -> False
                            Equal -> True
                            Disjoint -> False
                  else if typeName1 == "NotFind" && typeName2 /= "NotFind"  --untyped vs typed
                       then True
                       else False -- otherwise typed vs untyped, untyped vs untyped
varComparison (Composed s ss) (Composed t ts) typing = 
               if s /= t
                  then False
                  else
               if length ss == 0 && length ts == 0 --constants
                  then if s == t
                       then True
                       else False
                  else if length ss /= 0 && length ts /=0 --constant vs composed
                       then listComparison ss ts typing
                       else False -- constant vs composed, composed vs constant
 
listComparison :: [Term] -> [Term] -> Typing -> Bool
listComparison [] [] typing = True
listComparison (s:ss) (t:ts) typing = 
                if length (s:ss) == length (t:ts)
                   then (varComparison s t typing) && (listComparison ss ts typing) 
                   else False
listComparison ((Variable s):[]) ts typing = 
                  let ( typeName, typeList ) = Map.findWithDefault ("NotFind",[]) s typing
                  in if typeName == "NotFind" && length ts > 1 --untyped vs terms
                        then True
                        else False


isGEQ :: FPFact -> FPFact -> Typing -> Bool
isGEQ (FPFact fName1 fArgs1) (FPFact fName2 fArgs2) typing = 
                if fName1 /= fName2
                   then False
                   else listComparison fArgs1 fArgs2 typing 
{-                   
typedUnification :: Term -> Term -> Typing -> Subst -> Subst
typedUnification (Variable s) (Variable t) typing subst = 
               let ( typeName1, typeList1 ) = Map.findWithDefault ("NotFind",[]) s typing
                   ( typeName2, typeList2 ) = Map.findWithDefault ("NotFind",[]) t typing
               in if typeName1 /= "NotFind" && typeName2 /= "NotFind" --typed variable
                  then let comp = compPos typeList1 typeList2
                       in case comp of
                            Greater -> True
                            Smaller -> False
                            Equal -> True
                            Disjoint -> False
                  else if typeName1 == "NotFind" && typeName2 /= "NotFind"  --untyped vs typed
                       then True
                       else False -- otherwise typed vs untyped, untyped vs untyped
typedUnification (Composed s ss) (Composed t ts) typing subst = 
               if s /= t
                  then False
                  else
               if length ss == 0 && length ts == 0 --constants
                  then if s == t
                       then True
                       else False
                  else if length ss /= 0 && length ts /=0 --constant vs composed
                       then listComparison ss ts typing
                       else False -- constant vs composed, composed vs constant

--type Subst = Map.Map Variable Term
substitutionTyping :: Term -> Term -> [(Variable, Term)] -> [(Variable, Term)]
substitutionTyping (Variable s) t [] = [(s,t)]
substitutionTyping (Composed s ss) (Variable t) [] = [(t,(Composed s ss))]
substitutionTyping (Variable s) (Variable t) ((Variable s1, t1):rest) = 
            if s == s1 
            then (s1, Variable t) : substitution (Variable s, Variable t, rest)
            else if (Variable s) == t1 
                    then (s1, Variable t) : substitution (Variable s) (Variable t) rest
                    else (s1, t1) : substitution (Variable s) (Variable t) rest          
substitutionTyping (Variable s) (Composed t ts) ((Variable s1, t1):rest) = 
        if s == s1 
             then (Composed t ts, t1) : substitution (Variable s, Composed t ts, rest)
             else (Variable s1, t1) : substitution (Variable s, Composed t ts, rest)
substitutionTyping (Variable s, Composed t ts, (Composed s1 ss1, t1):rest) = 
            (Composed s1 (substitutionList (Variable s, Composed t ts, ss1)),t1) : substitution (Variable s, Composed t ts, rest)
substitutionTyping (Variable s, Variable t, (Composed s1 ts, t1):rest) = 
        (Composed s1 (substitutionList (Variable s, Variable t, ts)), t1) : substitution ((Variable s, Variable t, rest))
substitutionTyping (_,_,ss) = error "Wrong arguments ..."

-}










