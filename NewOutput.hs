module NewOutput where
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.IO
import Types
import TypeTree
import Unification
import FixedPoint

newoutput :: PASLan -> String
newoutput (PASLan _ _ _ _ _ _ _  rules) = 
           let typeRules = filterRuls rules
               factRules = filterFacts rules
               tree = typeTree typeRules
               fps = fixedpoint factRules [] tree 1 0
           in printFixedPoint (orderFPs fps)
    
printFixedPoint :: [FP] -> String
printFixedPoint [] = "---end---"
printFixedPoint ( (FP rName typing identityNo appliedFact (FPFact factName terms)):rest ) =  
                "(" ++ show identityNo ++ ") " ++
                concatMap (\x -> (printTyping x) ) (Map.toList typing) ++
                factName ++ "(" ++ printTerms terms ++ ")" ++
                --factName ++ "(" ++ concatMap (\x -> (printTerm x) ) terms ++ ")" ++
                (printList appliedFact)  ++
                " <= " ++ rName ++ ";" ++ "\n" ++
                printFixedPoint rest

filterRuls :: [PRule] -> [PRule]
filterRuls [] = []
filterRuls ((PRule rName [] lf e1 e2 e3 rf e4):rest) =         
             if rName == "userType"
               then (PRule rName [] lf e1 e2 e3 rf e4) : filterRuls rest
               else filterRuls rest  
               
filterFacts :: [PRule] -> [PRule]
filterFacts [] = []
filterFacts ((PRule rName [] lf e1 e2 e3 rf e4):rest) =         
             if rName /= "userType"
               then (PRule rName [] lf e1 e2 e3 rf e4) : filterFacts rest
               else filterFacts rest 
                
printTyping :: (Variable, (String,[Int])) -> String
printTyping (v, (s,typeInfo)) = v ++ ":" ++ s ++ "."

printTerms :: [Term] -> String
printTerms [] = []
printTerms ((Variable v):rest) = 
           if null rest
              then v
              else v ++ "," ++ printTerms rest
printTerms ((Composed c cs):rest) =
          if null rest
             then if null cs 
                    then c 
                    else c ++ "(" ++ printTerms cs ++ ")"
             else if null cs
                     then c ++ "," ++ (printTerms rest)
                     else c ++ "(" ++ (printTerms cs) ++ ")" ++ "," ++ (printTerms rest)
             

printList :: [Int] -> String
printList [] = ""
printList list = printElement list

printElement :: [Int] -> String
printElement [i] = " (" ++ (show i) ++ ")"
printElement (i:rest) = " (" ++ (show i) ++ ") " ++ "+" ++ printElement rest
                  
orderFPs :: [FP] -> [FP]
orderFPs [] = []
orderFPs ((FP rName typing identityNo appliedFact fact):rest) = 
         if identityNo == maxSerialNo ((FP rName typing identityNo appliedFact fact):rest)
            then  orderFPs rest ++ [(FP rName typing identityNo appliedFact fact)]
            else orderFPs (rest++[(FP rName typing identityNo appliedFact fact)])

---------------testteste---------------------------
printRules :: [PRule] -> String
printRules [] = "---end---"
printRules ((PRule rName _ lf _ _ _ rf _):rest) = 
             rName ++ ";" ++
             printRules rest
             
             
             
writeNode :: [TypeTree] -> String
writeNode [] = ""
writeNode [Node n []] = "Node \"" ++ n ++ "\" []" 
writeNode ((Node n ns):rest) = 
          if null rest
             then "Node \""++ n ++"\" [" ++ (writeNode ns) ++ "]" ++ (writeNode rest)
             else "Node \""++ n ++"\" [" ++ (writeNode ns) ++ "], " ++ (writeNode rest)

writeToFile :: [FP] -> String
writeToFile [] = "----end----"
writeToFile (fp:rest) = 
            (show fp) ++ ",\n" ++
            writeToFile rest
