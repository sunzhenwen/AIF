import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.IO
import Types
import UserTypeTree
import Unification
import FixPoint


{-  Comment Sebastian: can this program work only on the given example? Couldn't you connect to an arbitrary input? (Compile time might reduce, runtime increase!)  -}

main = do  
       writeFile "newoutput.txt" $ (printFixedPoint ( orderFPs (fixpoint [PRule {ruleName = "iAgents", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isAgent", factArgs = [PAtom {atomName = "A"}]}], 
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "iknows", factArgs = [PAtom {atomName = "A"}]}], 
                   rp = []},
            PRule {ruleName = "iOpen", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isAgent",factArgs = [PAtom {atomName = "A"}]},
                        PFact {factName = "iknows", 
                               factArgs = [PComp {compName = "sign", 
                                                  compArgs = [PComp {compName = "inv", 
                                                                     compArgs = [PComp {compName = "val", 
                                                                                        compArgs = [PAtom {atomName = "E__ring__PK"},
                                                                                        PAtom {atomName = "E__db__valid__PK"},
                                                                                        PAtom {atomName = "E__db__revoked__PK"}]
                                                                                        }]},
                                                              PComp {compName = "pair", 
                                                                     compArgs = [PAtom {atomName = "A"},
                                                                                PComp {compName = "val", 
                                                                                       compArgs = [PAtom {atomName = "E__ring__NPK"},
                                                                                       PAtom {atomName = "E__db__valid__NPK"},
                                                                                       PAtom {atomName = "E__db__revoked__NPK"}]}
                                                                                ]}]}]},
                        PFact {factName = "occurs", 
                               factArgs = [PComp {compName = "val", 
                                                  compArgs = [PAtom {atomName = "E__ring__PK"},
                                                              PAtom {atomName = "E__db__valid__PK"},
                                                              PAtom {atomName = "E__db__revoked__PK"}]
                                                }]},
                        PFact {factName = "occurs", factArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__NPK"},PAtom {atomName = "E__db__valid__NPK"},PAtom {atomName = "E__db__revoked__NPK"}]}]}
                        ], 
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "iknows", 
                                factArgs = [PComp {compName = "val", 
                                                   compArgs = [PAtom {atomName = "E__ring__NPK"},
                                                               PAtom {atomName = "E__db__valid__NPK"},
                                                               PAtom {atomName = "E__db__revoked__NPK"}]
                                                   }]}], 
                   rp = []},
            PRule {ruleName = "timplies", 
                    ruleEnv = [], 
                    lf = [PFact {factName = "isAgent", factArgs = [PAtom {atomName = "A"}]},
                          PFact {factName = "iknows", factArgs = [PAtom {atomName = "Val_1"}]},
                          PFact {factName = "timplies", factArgs = [PAtom {atomName = "Val_1"},PAtom {atomName = "Val_2"}]}], 
                    lp = [], 
                    ln = [], 
                    fresh = [], 
                    rf = [PFact {factName = "iknows", factArgs = [PAtom {atomName = "Val_2"}]}], 
                    rp = []},
            PRule {ruleName = "keyReg", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isUser", factArgs = [PAtom {atomName = "A"}]},
                         PFact {factName = "isServer", factArgs = [PAtom {atomName = "S"}]}], 
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "iknows", factArgs = [PComp {compName = "val", compArgs = [PComp     {compName = "ring", compArgs = [PAtom {atomName = "A"}]},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "A"}]},PComp {compName = "0", compArgs = []}]}]},
                        PFact {factName = "occurs", factArgs = [PComp {compName = "val", compArgs = [PComp {compName = "ring", compArgs = [PAtom {atomName = "A"}]},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "A"}]},PComp {compName = "0", compArgs = []}]}]}],
                   rp = []},
            PRule {ruleName = "timplies", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isUser", factArgs = [PAtom {atomName = "A"}]},
                        PFact {factName = "isServer", factArgs = [PAtom {atomName = "S"}]},
                        PFact {factName = "iknows", factArgs = [PAtom {atomName = "Val_1"}]},
                        PFact {factName = "timplies", factArgs = [PAtom {atomName = "Val_1"},PAtom {atomName = "Val_2"}]}], 
                   lp = [],
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "iknows", factArgs = [PAtom {atomName = "Val_2"}]}], 
                   rp = []},
            PRule {ruleName = "timplies", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isUser", factArgs = [PAtom {atomName = "A"}]},
                        PFact {factName = "isServer", factArgs = [PAtom {atomName = "S"}]},
                        PFact {factName = "occurs", factArgs = [PAtom {atomName = "Val_1"}]},
                        PFact {factName = "timplies", factArgs = [PAtom {atomName = "Val_1"},PAtom {atomName = "Val_2"}]}], 
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "occurs", factArgs = [PAtom {atomName = "Val_2"}]}], 
                   rp = []},
            PRule {ruleName = "userUpdateKey", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isHonest", factArgs = [PAtom {atomName = "A"}]},
                         PFact {factName = "isServer", factArgs = [PAtom {atomName = "S"}]},
                         PFact {factName = "iknows", factArgs = [PComp {compName = "val", compArgs = [PComp {compName = "ring", compArgs = [PAtom {atomName = "A"}]},PAtom {atomName = "E__db__valid__PK"},PAtom {atomName = "E__db__revoked__PK"}]}]},
                         PFact {factName = "occurs", factArgs = [PComp {compName = "val", compArgs = [PComp {compName = "ring", compArgs = [PAtom {atomName = "A"}]},PAtom {atomName = "E__db__valid__PK"},PAtom {atomName = "E__db__revoked__PK"}]}]}], 
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "iknows", factArgs = [PComp {compName = "sign", compArgs = [PComp {compName = "inv", compArgs = [PComp {compName = "val", compArgs = [PComp {compName = "0", compArgs = []},PAtom {atomName = "E__db__valid__PK"},PAtom {atomName = "E__db__revoked__PK"}]}]},PComp {compName = "pair", compArgs = [PAtom {atomName = "A"},PComp {compName = "val", compArgs = [PComp {compName = "ring", compArgs = [PAtom {atomName = "A"}]},PComp {compName = "0", compArgs = []},PComp {compName = "0", compArgs = []}]}]}]}]},
                   PFact {factName = "occurs", factArgs = [PComp {compName = "val", compArgs = [PComp {compName = "ring", compArgs = [PAtom {atomName = "A"}]},PComp {compName = "0", compArgs = []},PComp {compName = "0", compArgs = []}]}]},
                   PFact {factName = "timplies", factArgs = [PComp {compName = "val", compArgs = [PComp {compName = "ring", compArgs = [PAtom {atomName = "A"}]},PAtom {atomName = "E__db__valid__PK"},PAtom {atomName = "E__db__revoked__PK"}]},PComp {compName = "val", compArgs = [PComp {compName = "0", compArgs = []},PAtom {atomName = "E__db__valid__PK"},PAtom {atomName = "E__db__revoked__PK"}]}]}], 
                   rp = []},
            PRule {ruleName = "timplies", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isHonest", factArgs = [PAtom {atomName = "A"}]},
                         PFact {factName = "isServer", factArgs = [PAtom {atomName = "S"}]},
                         PFact {factName = "iknows", factArgs = [PComp {compName = "sign", compArgs = [PComp {compName = "inv", compArgs = [PAtom {atomName = "Val_1"}]},PComp {compName = "pair", compArgs = [PAtom {atomName = "A"},PAtom {atomName = "NPK"}]}]}]},
                         PFact {factName = "timplies", factArgs = [PAtom {atomName = "Val_1"},PAtom {atomName = "Val_2"}]}], 
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "iknows", factArgs = [PComp {compName = "sign", compArgs = [PComp {compName = "inv", compArgs = [PAtom {atomName = "Val_2"}]},PComp {compName = "pair", compArgs = [PAtom {atomName = "A"},PAtom {atomName = "NPK"}]}]}]}], 
                   rp = []},
            PRule {ruleName = "timplies", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isHonest", factArgs = [PAtom {atomName = "A"}]},
                         PFact {factName = "isServer", factArgs = [PAtom {atomName = "S"}]},
                         PFact {factName = "iknows", factArgs = [PComp {compName = "sign", compArgs = [PComp {compName = "inv", compArgs = [PAtom {atomName = "PK"}]},PComp {compName = "pair", compArgs = [PAtom {atomName = "A"},PAtom {atomName = "Val_1"}]}]}]},
                         PFact {factName = "timplies", factArgs = [PAtom {atomName = "Val_1"},PAtom {atomName = "Val_2"}]}], 
                   lp = [], 
                   ln = [], 
                   fresh = [],
                   rf = [PFact {factName = "iknows", factArgs = [PComp {compName = "sign", compArgs = [PComp {compName = "inv", compArgs = [PAtom {atomName = "PK"}]},PComp {compName = "pair", compArgs = [PAtom {atomName = "A"},PAtom {atomName = "Val_2"}]}]}]}], 
                   rp = []},
            PRule {ruleName = "timplies", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isHonest", factArgs = [PAtom {atomName = "A"}]},
                         PFact {factName = "isServer", factArgs = [PAtom {atomName = "S"}]},
                         PFact {factName = "occurs", factArgs = [PAtom {atomName = "Val_1"}]},
                         PFact {factName = "timplies", factArgs = [PAtom {atomName = "Val_1"},PAtom {atomName = "Val_2"}]}],
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "occurs", factArgs = [PAtom {atomName = "Val_2"}]}], 
                   rp = []},
            PRule {ruleName = "serverUpdateKey", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isUser", factArgs = [PAtom {atomName = "A"}]},
                         PFact {factName = "isServer", factArgs = [PAtom {atomName = "S"}]},
                         PFact {factName = "iknows", factArgs = [PComp {compName = "sign", compArgs = [PComp {compName = "inv", compArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "A"}]},PComp {compName = "0", compArgs = []}]}]},PComp {compName = "pair", compArgs = [PAtom {atomName = "A"},PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__NPK"},PComp {compName = "0", compArgs = []},PComp {compName = "0", compArgs = []}]}]}]}]},
                         PFact {factName = "occurs", factArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "A"}]},PComp {compName = "0", compArgs = []}]}]},
                         PFact {factName = "occurs", factArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__NPK"},PComp {compName = "0", compArgs = []},PComp {compName = "0", compArgs = []}]}]}], 
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "iknows", factArgs = [PComp {compName = "inv", compArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "0", compArgs = []},PComp {compName = "db__revoked", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "A"}]}]}]}]},
                         PFact {factName = "timplies", factArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "A"}]},PComp {compName = "0", compArgs = []}]},PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "0", compArgs = []},PComp {compName = "db__revoked", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "A"}]}]}]},
                         PFact {factName = "timplies", factArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__NPK"},PComp {compName = "0", compArgs = []},PComp {compName = "0", compArgs = []}]},PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__NPK"},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "A"}]},PComp {compName = "0", compArgs = []}]}]}],
                   rp = []},
            PRule {ruleName = "timplies", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isUser", factArgs = [PAtom {atomName = "A"}]},
                         PFact {factName = "isServer", factArgs = [PAtom {atomName = "S"}]},
                         PFact {factName = "iknows", factArgs = [PComp {compName = "inv", compArgs = [PAtom {atomName = "Val_1"}]}]},
                         PFact {factName = "timplies", factArgs = [PAtom {atomName = "Val_1"},PAtom {atomName = "Val_2"}]}],
                   lp = [],
                   ln = [], 
                   fresh = [],
                   rf = [PFact {factName = "iknows", factArgs = [PComp {compName = "inv", compArgs = [PAtom {atomName = "Val_2"}]}]}],
                   rp = []},
            PRule {ruleName = "serverUpdateKey__multifamily", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isServer", factArgs = [PAtom {atomName = "PK__db__revoked__1"}]},
                         PFact {factName = "isUser", factArgs = [PAtom {atomName = "PK__db__revoked__2"}]},
                         PFact {factName = "isUser", factArgs = [PAtom {atomName = "A"}]},
                         PFact {factName = "isServer", factArgs = [PAtom {atomName = "S"}]},
                         PFact {factName = "iknows", factArgs = [PComp {compName = "sign", compArgs = [PComp {compName = "inv", compArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "A"}]},PComp {compName = "db__revoked", compArgs = [PAtom {atomName = "PK__db__revoked__1"},PAtom {atomName = "PK__db__revoked__2"}]}]}]},PComp {compName = "pair", compArgs = [PAtom {atomName = "A"},PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__NPK"},PComp {compName = "0", compArgs = []},PComp {compName = "0", compArgs = []}]}]}]}]},
                         PFact {factName = "occurs", factArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "A"}]},PComp {compName = "db__revoked", compArgs = [PAtom {atomName = "PK__db__revoked__1"},PAtom {atomName = "PK__db__revoked__2"}]}]}]},
                         PFact {factName = "occurs", factArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__NPK"},PComp {compName = "0", compArgs = []},PComp {compName = "0", compArgs = []}]}]}], 
                   lp = [], 
                   ln = [], 
                   fresh = [],
                   rf = [PFact {factName = "attack", factArgs = []},
                         PFact {factName = "timplies", factArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "A"}]},PComp {compName = "db__revoked", compArgs = [PAtom {atomName = "PK__db__revoked__1"},PAtom {atomName = "PK__db__revoked__2"}]}]},PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "0", compArgs = []},PComp {compName = "0", compArgs = []}]}]}],
                   rp = []},
            PRule {ruleName = "attackDef", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isServer", factArgs = [PAtom {atomName = "S"}]},
                         PFact {factName = "isHonest", factArgs = [PAtom {atomName = "H"}]},
                         PFact {factName = "iknows", factArgs = [PComp {compName = "inv", compArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "H"}]},PAtom {atomName = "E__db__revoked__PK"}]}]}]},
                         PFact {factName = "occurs", factArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "H"}]},PAtom {atomName = "E__db__revoked__PK"}]}]}], 
                   lp = [], 
                   ln = [], 
                   fresh = [],
                   rf = [PFact {factName = "attack", factArgs = []},
                         PFact {factName = "timplies", factArgs = [PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "db__valid", compArgs = [PAtom {atomName = "S"},PAtom {atomName = "H"}]},PAtom {atomName = "E__db__revoked__PK"}]},PComp {compName = "val", compArgs = [PAtom {atomName = "E__ring__PK"},PComp {compName = "0", compArgs = []},PAtom {atomName = "E__db__revoked__PK"}]}]}],
                   rp = []}] [] [Node "isAgent" [Node "isServer" [Node "s" []],Node "isUser" [Node "isDishon" [Node "i" []],Node "isHonest" [Node "A" []]]]] 0 0  )))

   
    
printFixedPoint :: [FP] -> String
printFixedPoint [] = "---end---"
printFixedPoint ( (FP rName typing identityNo appliedFact (FPFact factName terms)):rest ) =  
                "(" ++ show identityNo ++ ") " ++
                concatMap (\x -> (printTyping x) ) (Map.toList typing) ++
                factName ++ "(" ++ concatMap (\x -> (printTerm x) ) terms ++ ")" ++
                (printList appliedFact)  ++
                " <= " ++ rName ++ "; \n" ++
                printFixedPoint rest
                
printTyping :: (Variable, (String,[Int])) -> String
printTyping (v, (s,typeInfo)) = v ++ ":" ++ s ++ "."

printTerm :: Term -> String
printTerm (Variable v) = v
printTerm (Composed c cs) = 
          if null cs 
             then c 
             else c ++ "(" ++ printTermList cs ++ ")"
             
printTermList :: [Term] -> String
printTermList [] = ""
printTermList (t:rest) = 
               if null rest
                  then (printTerm t)
                  else (printTerm t) ++ "," ++ (printTermList rest)

printList :: [Int] -> String
printList [] = ""
printList list = printElement list

printElement :: [Int] -> String
printElement [i] = " (" ++ (show i) ++ ")"
printElement (i:rest) = " (" ++ (show i) ++ ") " ++ "+" ++ printElement rest
                  
orderFPs :: [FP] -> [FP]
orderFPs [] = []
orderFPs ((FP rName typing identityNo appliedFact fact):rest) = 
         if identityNo == checkSerialNo ((FP rName typing identityNo appliedFact fact):rest)
            then  orderFPs rest ++ [(FP rName typing identityNo appliedFact fact)]
            else orderFPs (rest++[(FP rName typing identityNo appliedFact fact)])

