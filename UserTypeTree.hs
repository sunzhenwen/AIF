module UserTypeTree where
import Data.List
import System.IO
import Types

{-

Some remarks by Sebastian

- this code is like this quite unreadable, because there are no
comments. Actually, this is what needs to be done for the thesis
anyway. For the main functions, one should have 
   * an intuitive explanation/idea what it does
   * an example (or several, if one example does not really cover everything the function does)
   * if possible, a formal description -- e.g. some key properties of the function

- several things seem very naive: e.g. comparing lists of things
   * there are library functions that do that, and more efficiently so

- There are long commented-out examples. Actually some could serve as test-examples (uncommented)?


-}

{-createUserTypeTree (addLeafToNode (divideNodeAndLeaf (getUserTypeTree [PRule {ruleName = "userType", 
                     ruleEnv = [], 
                     lf = [], 
                     lp = [], 
                     ln = [], 
                     fresh = [],
                     rf = [PFact {factName = "isHonest", factArgs = [PAtom {atomName = "a"}]},
                           PFact {factName = "isHonest", factArgs = [PAtom {atomName = "b"}]}], 
                     rp = []},
            PRule {ruleName = "userType", 
                     ruleEnv = [], 
                     lf = [], 
                     lp = [], 
                     ln = [], 
                     fresh = [], 
                     rf = [PFact {factName = "isDishon", factArgs = [PAtom {atomName = "i"}]}], 
                     rp = []},
            PRule {ruleName = "userType", 
                     ruleEnv = [], 
                     lf = [], 
                     lp = [], 
                     ln = [], 
                     fresh = [], 
                     rf = [PFact {factName = "isServer", factArgs = [PAtom {atomName = "s"}]}], 
                     rp = []},
            PRule {ruleName = "userType", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isHonest", factArgs = [PAtom {atomName = "X"}]}],
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "isUser", factArgs = [PAtom {atomName = "X"}]}], 
                   rp = []},
            PRule {ruleName = "userType", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isDishon", factArgs = [PAtom {atomName = "X"}]}], 
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "isUser", factArgs = [PAtom {atomName = "X"}]}], 
                   rp = []},
            PRule {ruleName = "userType", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isHonest", factArgs = [PAtom {atomName = "X"}]}],
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "isAgent", factArgs = [PAtom {atomName = "X"}]}], 
                   rp = []},
            PRule {ruleName = "userType", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isServer", factArgs = [PAtom {atomName = "X"}]}], 
                   lp = [],
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "isAgent", factArgs = [PAtom {atomName = "X"}]}], 
                   rp = []},
            PRule {ruleName = "userType", 
                   ruleEnv = [], 
                   lf = [PFact {factName = "isDishon", factArgs = [PAtom {atomName = "X"}]}], 
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "isAgent", factArgs = [PAtom {atomName = "X"}]}], 
                   rp = []},
            PRule {ruleName = "userType", 
                   ruleEnv = [], 
                   lf = [], 
                   lp = [], 
                   ln = [], 
                   fresh = [], 
                   rf = [PFact {factName = "isSts", factArgs = [PAtom {atomName = "valid"}]},
                         PFact {factName = "isSts", factArgs = [PAtom {atomName = "revoked"}]}], 
                   rp = []}] []) [] []) )-}

data UserTypeTree = Node String [UserTypeTree] deriving (Eq,Show)

createBranch :: [PFact] -> [PFact] -> [UserTypeTree] -> [UserTypeTree]
createBranch [] rf trees =  combineTrees ((getLeaf rf) ++ trees)
createBranch [PFact lff lfArgs] [PFact rff rfArgs] trees = combineTrees (Node rff [Node lff []] : trees)

getLeaf :: [PFact] -> [UserTypeTree]
getLeaf [] = []
getLeaf ((PFact f [PComp cName cArgs]) : rest) = (Node f [Node cName []]) : getLeaf rest
getLeaf ((PFact f [PAtom aName]) : rest) = ( Node f [Node aName []] ) : getLeaf rest

getUserTypeTree :: [PRule] -> [UserTypeTree] -> [UserTypeTree]
getUserTypeTree [] trees = trees
getUserTypeTree ((PRule _ [] lf _ _ _ rf _):rest) trees = 
                 getUserTypeTree rest (createBranch lf rf trees)

--combine all the leafs which have the same node
combineTrees :: [UserTypeTree] -> [UserTypeTree]
combineTrees [] = []
combineTrees [(Node s1 ts1)] = [(Node s1 ts1)]
combineTrees ((Node s1 ts1) : (Node s2 ts2) : rest) =  
                  if s1 == s2  
                     then combineTrees ((Node s1 (ts1++ts2)):rest)
                     else (Node s2 ts2) : combineTrees ((Node s1 ts1):rest)
              
{-[Node "isServer" [Node "server" []],Node "isDishon" [Node "dishon" []],Node "isH
onest" [Node "honest" []],Node "isUser" [Node "isDishon" [],Node "isHonest" []],
Node "isAgent" [Node "isDishon" [],Node "isServer" [],Node "isHonest" []]]

-}

divideNodeAndLeaf :: [UserTypeTree] -> [UserTypeTree] -> [UserTypeTree] -> ([UserTypeTree],[UserTypeTree])
divideNodeAndLeaf [] ns1 ns2 = (ns1,ns2)
divideNodeAndLeaf ((Node s [Node s1 []]) : rest) ns1 ns2 = 
                  divideNodeAndLeaf rest ((Node s [Node s1 []]) : ns1) ns2
divideNodeAndLeaf ((Node s ts) : rest) ns1 ns2 = 
                  divideNodeAndLeaf rest ns1 ((Node s ts) : ns2)

addLeafToNode :: ([UserTypeTree], [UserTypeTree]) -> [UserTypeTree]
addLeafToNode ([], ns) = ns
addLeafToNode (((Node s ts) : rest), ns) = addLeafToNode ( rest, (addLeaf (Node s ts) ns) )

addLeaf :: UserTypeTree -> [UserTypeTree] -> [UserTypeTree]
addLeaf (Node s ts) [] = []
addLeaf (Node s1 ts1) ((Node s2 ts2):rest) = 
                 if contains (Node s1 ts1) ts2
                    then (Node s2 (replaceNode (Node s1 ts1) ts2)) : (addLeaf (Node s1 ts1) rest)
                    else (Node s2 ts2) : (addLeaf (Node s1 ts1) rest)
        
{-
[Node "A2" [Node "B1" [Node "b1" []],Node "B2" [Node "b2" []],Node "B3" [Node "b3" []]],
Node "A1" [Node "B1" [Node "b1" []],Node "B2" [Node "b2" []],Node "B3" [Node "b3" []],Node "B4" [Node "b4" []]]],
Node "A3" [Node "B1" [Node "b1" []],Node "B2" [Node "b2" []]]
-}
                    
createUserTypeTree :: [UserTypeTree] -> [UserTypeTree]
createUserTypeTree [] = []
createUserTypeTree [(Node s ts)] = [(Node s ts)]
createUserTypeTree ((Node s1 ts1):(Node s2 ts2):rest) = 
                 if containsList (getAllLeaves ts1 []) (getAllLeaves ts2 [])
                    then createUserTypeTree ( (Node s2 (replaceNode (Node s1 ts1) ts2)) : replaceNode (Node s1 ts1) rest )
                    else if containsList (getAllLeaves ts2 []) (getAllLeaves ts1 [])
                            then createUserTypeTree ( (Node s1 (replaceNode (Node s2 ts2) ts1)) : replaceNode (Node s2 ts2) rest )
                            else (Node s2 ts2) : createUserTypeTree ((Node s1 ts1):rest)
                            
getAllLeaves :: [UserTypeTree] -> [UserTypeTree] -> [UserTypeTree]
getAllLeaves [] ns = ns
getAllLeaves ( (Node s []):rest ) ns = getAllLeaves rest ( (Node s []) : ns )
getAllLeaves ( (Node s ts):rest ) ns = getAllLeaves rest ( getAllLeaves ts ns )

containsList :: [UserTypeTree] -> [UserTypeTree] -> Bool
containsList [] _ = True
containsList ((Node s1 ts1):rest) xs = (contains (Node s1 ts1) xs) && containsList rest xs

contains :: UserTypeTree -> [UserTypeTree] -> Bool
contains (Node s1 ts1) [] = False 
contains (Node s1 ts1) ((Node s2 ts2):rest) = 
                if s1 == s2 
                   then True
                   else contains (Node s1 ts1) rest           

replaceNode :: UserTypeTree -> [UserTypeTree] -> [UserTypeTree]
replaceNode (Node s1 ts1) [] = []
replaceNode (Node s1 ts1) ((Node s2 ts2):rest) = 
                if s1 == s2 
                   then (Node s1 ts1) : replaceNode (Node s1 ts1) rest
                   else if containsList (getAllLeaves ts1 []) (getAllLeaves ((Node s2 ts2):rest) [] ) 
                           then (Node s1 ts1) : replaceSubNode (Node s1 ts1) ((Node s2 ts2):rest) 
                           else (Node s2 ts2) : replaceNode (Node s1 ts1) rest

--remove the compared node
replaceSubNode :: UserTypeTree -> [UserTypeTree] -> [UserTypeTree]
replaceSubNode (Node s1 []) ns = ns
replaceSubNode (Node s1 ((Node s11 ts11):rest)) ((Node s2 ts2):rest2) = 
                if s11 == s2 
                   then replaceSubNode (Node s1 rest) rest2
                   else replaceSubNode (Node s1 rest) ((Node s2 ts2) : replaceSubNode (Node s1 ((Node s11 ts11):rest)) rest2)
{-replaceSubNode (Node s1 ((Node s11 ts11):rest)) ((Node s2 ts2):rest2) = 
                if containsList (getAllLeaves ts11 []) (getAllLeaves ((Node s2 ts2):rest) [] ) 
                   then replaceSubNode (Node s1 rest) (replaceSubNode (Node s11 ts11) ((Node s2 ts2):rest2))
                   else replaceSubNode (Node s1 rest) rest2-}

                   
                   
                   
                 
                  
                 
                 
                 



