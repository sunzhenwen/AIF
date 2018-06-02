module TypeTree where
import Data.List
import System.IO
import Types

data TypeTree = Node String [TypeTree] deriving (Eq,Show)

{-
    typeTree function takes a list of rules which represent variable type as an argument and returns a type tree.
-}
typeTree :: [PRule] -> [TypeTree]
typeTree [] = []
typeTree r = 
        let nodesAndLeaves = rulesTypes r
            trees = typeTree nodesAndLeaves []
        in trees
         --typeTree (createNodeAndLeaf lf rf (typeTree rest)) []

rulesTypes :: [PRule] -> [TypeTree]
rulesTypes [] = []
rulesTypes ((PRule rName [] lf e1 e2 e3 rf e4):rest) = 
            createNodeAndLeaf lf rf (rulesTypes rest)

{-
If the TypeTree is a leaf, add it into current TypeTree list;
If it is a node, it has to be checked if there is a same node already in the TypeTree list.
-}
createNodeAndLeaf :: [PFact] -> [PFact]-> [TypeTree] -> [TypeTree]
createNodeAndLeaf [] rf typeTree = getLeaf typeTree rf
createNodeAndLeaf [PFact lff lfArgs] [PFact rff rfArgs] typeTree = 
                  combineNode (Node rff [Node lff []]) typeTree

{-
getLeaf takes a fact as an argument and return a TypeTree
-}
getLeaf :: [TypeTree] -> [PFact] -> [TypeTree]
getLeaf [] ((PFact f [PComp cName cArgs]):rest) = getLeaf [Node f [Node cName []]] rest
getLeaf [] ((PFact f [PAtom aName]):rest) = getLeaf [Node f [Node aName []]] rest
getLeaf nodes [] = nodes
getLeaf nodes ((PFact f [PComp cName cArgs]):rest) = 
               let extionedNode = combineNode (Node f [Node cName []]) nodes
               in getLeaf extionedNode rest
getLeaf nodes ((PFact f [PAtom aName]) : rest) = 
               let extionedNode = combineNode (Node f [Node aName []]) nodes
               in getLeaf extionedNode rest
{-
combineNode takes a TypeTree and the current TypeTree list as arguments.
If the TypeTree is a node and it does not in the current TypeTree, add it into current TypeTree list;
If there is a same node (has the same node name) in the TypeTree list, combine these two nodes as one node.
-}
combineNode :: TypeTree -> [TypeTree] -> [TypeTree]
combineNode typeTree [] = [typeTree]
combineNode (Node n ns) ((Node n1 ns1):rest) = 
            if n == n1
               then (Node n (ns++ns1)):rest
               else (Node n1 ns1): combineNode (Node n ns) rest

{-
    The idea is if a node is other nodes' sub node, 
    add this node to each parent node and remove it from the list.
-}
typeTree :: [TypeTree] -> [TypeTree] -> [TypeTree]
typeTree [] nodes = nodes
typeTree (node:rest) nodes =               
           if isSubTree node rest || isSubTree node nodes
              then typeTree (replaceSubTree node rest) (replaceSubTree node nodes)
              else typeTree rest (node:nodes)

{-
isSubTree (Node "n11" [Node "n12" [], Node "n13" []] ) [Node "n12" [], Node "n13" []],
isSubTree (Node "n11" [Node "n12" []]) [Node "n21" [Node "n11" []],Node "n22" [Node "n12" []]],
isSubTree (Node "a" [Node "a1" []]) [Node "b" [Node "c" [Node "a" []], Node "d" []]],
isSubTree (Node "a" [Node "a1"[], Node "a2" []]) [Node "b" [Node "a1"[], Node "a2" []], Node "c" []]   
-}              
isSubTree :: TypeTree -> [TypeTree] -> Bool
isSubTree node []  = False
isSubTree (Node n ns) ((Node n1 ns1):rest) = 
           if n == n1 -- && ns == ns1 -- (Node a [],  Node a [])
              then True
              else isSubTree (Node n ns) ns1 || isSubTree (Node n ns) rest
           {-if n /= n1 -- && null ns
                   then isSubTree (Node n ns) rest || isSubTree (Node n ns) ns1
                   else if isSubSet ns ns1 ||
                           n == n1 && null ns1 --([Node n [Node n' []]] , [Node n []]  )   -- one node's sub nodes are all contained in another node's sub node
                           then True
                           else isSubTree (Node n ns) ns1 || isSubTree (Node n ns) rest
-}

replaceSubTree :: TypeTree -> [TypeTree] -> [TypeTree]
replaceSubTree node [] = []
replaceSubTree (Node n ns) ((Node n1 ns1):rest) = 
                  if n == n1
                     then (Node n ns) : replaceSubTree (Node n ns) rest
                     else (Node n1 (replaceSubTree (Node n ns) ns1)): replaceSubTree (Node n ns) rest
                {-if n == n1 && ns == ns1 -- if two nodes are the same, remove it from the list
                   then replaceSubTree (Node n ns) rest
                   else if n == n1 -- && null ns1 --add leaf to the node
                           then (Node n ns) : replaceSubTree (Node n ns) rest
                           else if isSubSet ns ns1 -- this node is sub node
                                   then (Node n1 ((Node n ns) : removeIntersect ns ns1)) : replaceSubTree (Node n ns) rest
                                   else (Node n1 (replaceSubTree (Node n ns) ns1)) : replaceSubTree (Node n ns) rest-}
                                           {-if isInfixOf ns ((Node n1 ns1):rest)
                                           then (Node n1 ns) : removeIntersect ns ((Node n1 ns1):rest)
                                           else (Node n1 (replaceSubTree (Node n ns) ns1)) : replaceSubTree (Node n ns) rest                                           
                                    -}
isSubSet :: [TypeTree] -> [TypeTree] -> Bool
isSubSet [] nodes = True
isSubSet (n:rest) nodes = 
         isSubTree n nodes && isSubSet rest nodes
                                           
{-[Node "isHonest" [Node "honest" []],Node "isDishon" [Node "dishon" []],Node "isServer" [Node "server" []],Node "isSts" [Node "valid" []],Node "isSts" [Node "revoked" []],Node "isAgent" [Node "isHonest" [],Node "isServer" [],Node "isDishon"[]],Node "isUser" [Node "isHonest" [],Node "isDishon" []]] 

removeIntersect takes two TypeTree lists as arguments.
The purpose is removing all elements of the first list which are contained in the second list.
removeIntersect [Node "a1"[], Node "a2" []] [Node "a1"[], Node "a2" [], Node "a3" []]
-}
removeIntersect :: [TypeTree] -> [TypeTree] -> [TypeTree]
removeIntersect nodes [] = []
removeIntersect nodes (n:rest) = 
                if isInfixOf [n] nodes
                   then removeIntersect nodes rest
                   else n : removeIntersect nodes rest                   


                   
                 
                  
                 
                 
                 



