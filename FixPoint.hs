module FixPoint where
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.IO
import Types
import UserTypeTree
import Unification
import Control.Monad

{- Comment Sebastian: What are these huge test examples in comments? (And actually no real comments!) [See also remarks in UserTypeTrees!]
   one could outsource larger examples into a separat file "Test.hs" or so!
 -}

{-testFixPoint = [concatSubstutionToLHSFact "iSign" 
                             [FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Variable "E__ring__PK",Variable "E__db__valid__PK",Variable "E__db__revoked__PK"]],Composed "pair" [Variable "A",Composed "val" [Variable "E__ring__NPK",Variable "E__db__valid__NPK",Variable "E__db__revoked__NPK"]]]]}]
                             [FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isDishon",[0,2,21])),("S4",("isServer",[0,1]))], identityNo = 11, appliedFact = [3,6], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]]}},
                                    FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 8, appliedFact = [4,7,5], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]]]}},
                                    FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 9, appliedFact = [4,7,5], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []],Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]]}},
                                    FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 10, appliedFact = [4,7,5], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "ring" [Variable "A4"],Composed "0" [],Composed "0" []],Composed "val" [Composed "ring" [Variable "A4"],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isUser",[0,2])),("S4",("isServer",[0,1]))], identityNo = 7, appliedFact = [2,6], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]}},
                                    FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 4, appliedFact = [1,2], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]],Composed "pair" [Variable "A4",Composed "val" [Composed "ring" [Variable "A4"],Composed "0" [],Composed "0" []]]]]}},
                                    FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 5, appliedFact = [1,2], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "ring" [Variable "A4"],Composed "0" [],Composed "0" []]]}},
                                    FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 6, appliedFact = [1,2], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "ring" [Variable "A4"],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []],Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]}},
                                    FP {fpRuleName = "dishonKey", typeInfo = Map.fromList [("A4",("isDishon",[0,2,21])),("S4",("isServer",[0,1]))], identityNo = 3, appliedFact = [1,2], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "inv" [Composed "val" [Composed "ring" [Variable "A4"],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]]}},
                                    FP {fpRuleName = "keyReg", typeInfo = Map.fromList [("A4",("isUser",[0,2])),("S4",("isServer",[0,1]))], identityNo = 1, appliedFact = [], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "ring" [Variable "A4"],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]}},
                                    FP {fpRuleName = "keyReg", typeInfo = Map.fromList [("A4",("isUser",[0,2])),("S4",("isServer",[0,1]))], identityNo = 2, appliedFact = [], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "ring" [Variable "A4"],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]}},
                                    FP {fpRuleName = "iAgents", typeInfo = Map.fromList [("A0",("isAgent",[0]))], identityNo = 0, appliedFact = [], fact = FPFact {fpFactName = "iknows", fpArgs = [Variable "A0"]}},
                                    FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 30, appliedFact = [26,7,27], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []],Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 27, appliedFact = [18,19,10], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 26, appliedFact = [4,19,10], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]],Composed "pair" [Variable "A4",Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 25, appliedFact = [14,6], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]],Composed "pair" [Variable "A4",Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 24, appliedFact = [17,19,10], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A4",Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 23, appliedFact = [20,6], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]],Composed "pair" [Variable "A4",Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 22, appliedFact = [21,19,10], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]],Composed "pair" [Variable "A4",Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 21, appliedFact = [4,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]],Composed "pair" [Variable "A4",Composed "val" [Composed "ring" [Variable "A4"],Composed "0" [],Composed "0" []]]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 20, appliedFact = [14,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]],Composed "pair" [Variable "A4",Composed "val" [Composed "ring" [Variable "A4"],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]]]}},
                                    FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 17, appliedFact = [12,5], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A4",Composed "val" [Composed "ring" [Variable "A4"],Composed "0" [],Composed "0" []]]]]}},
                                    FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 19, appliedFact = [12,5], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "ring" [Variable "A4"],Composed "0" [],Composed "0" []],Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isUser",[0,2])),("S4",("isServer",[0,1]))], identityNo = 16, appliedFact = [7,9], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isUser",[0,2])),("S4",("isServer",[0,1]))], identityNo = 15, appliedFact = [13,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 14, appliedFact = [4,10], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]],Composed "pair" [Variable "A4",Composed "val" [Composed "ring" [Variable "A4"],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isUser",[0,2])),("S4",("isServer",[0,1]))], identityNo = 13, appliedFact = [1,6], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]}},
                                    FP {fpRuleName = "iOpen", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 12, appliedFact = [4,7,5], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "ring" [Variable "A4"],Composed "0" [],Composed "0" []]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 36, appliedFact = [33,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A4",Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 35, appliedFact = [17,10,19], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A4",Composed "val" [Composed "ring" [Variable "A4"],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 34, appliedFact = [23,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]],Composed "pair" [Variable "A4",Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 33, appliedFact = [24,30], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A4",Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]]]]}},
                                    FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 32, appliedFact = [25,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S4",Variable "A4"],Composed "0" []]],Composed "pair" [Variable "A4",Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S4",Variable "A4"]]]]]}},
                                    FP {fpRuleName = "iOpen", typeInfo = Map.fromList [("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], identityNo = 31, appliedFact = [26,7,27], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]}}]        
                                     100
                                    [(Map.fromList [("A",Variable "A0"),("E__db__revoked__NPK",Composed "0" []),("E__db__revoked__PK",Composed "0" []),("E__db__valid__NPK",Composed "db__valid" [Variable "S4",Variable "A4"]),("E__db__valid__PK",Composed "db__valid" [Variable "S4",Variable "A4"]),("E__ring__NPK",Composed "ring" [Variable "A4"]),("E__ring__PK",Composed "0" [])], Map.fromList [("A0",("isAgent",[0])),("A4",("isDishon",[0,2,21])),("S4",("isServer",[0,1]))], [11,0,1,15,13,12,31,7,5,2,27,16]), 
                                    (Map.fromList [("A",Variable "A0"),("E__db__revoked__NPK",Composed "0" []),("E__db__revoked__PK",Composed "db__revoked" [Variable "S4",Variable "A4"]),("E__db__valid__NPK",Composed "db__valid" [Variable "S4",Variable "A4"]),("E__db__valid__PK",Composed "0" []),("E__ring__NPK",Composed "ring" [Variable "A4"]),("E__ring__PK",Composed "0" [])], Map.fromList [("A0",("isAgent",[0])),("A4",("isHonest",[0,2,22])),("S4",("isServer",[0,1]))], [8,0,1,15,13,12,31,16,7,5,2,27]), 
                                    (Map.fromList [("A",Variable "A0"),("E__db__revoked__NPK",Composed "0" []),("E__db__revoked__PK",Composed "0" []),("E__db__valid__NPK",Composed "db__valid" [Variable "S4",Variable "A4"]),("E__db__valid__PK",Composed "db__valid" [Variable "S4",Variable "A4"]),("E__ring__NPK",Composed "ring" [Variable "A4"]),("E__ring__PK",Composed "ring" [Variable "A4"])], Map.fromList [("A0",("isAgent",[0])),("A4",("isDishon",[0,2,21])),("S4",("isServer",[0,1]))], [3,0,1,15,13,12,31,2,7,5,27,16])] 

                                ]

{-[unifyRestLFSubSet (Map.fromList [("A",("isAgent",[0]))]) 
                             [FPFact {fpFactName = "iknows", fpArgs = [Variable "A"]}]
                                   [FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 7, appliedFact = [3,6,4], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]]}},
                             FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 8, appliedFact = [3,6,4], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []],Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]}},
                             FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 9, appliedFact = [3,6,4], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []],Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 6, appliedFact = [2,5], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 3, appliedFact = [1,2], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 4, appliedFact = [1,2], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []]]}},
                             FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 5, appliedFact = [1,2], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []],Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "keyReg", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 1, appliedFact = [], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "keyReg", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 2, appliedFact = [], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "iAgents", typeInfo = Map.fromList [("A0",("isAgent",[0]))], identityNo = 0, appliedFact = [], fact = FPFact {fpFactName = "iknows", fpArgs = [Variable "A0"]}},
                             FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 24, appliedFact = [20,6,21], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []],Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 21, appliedFact = [15,16,9], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 20, appliedFact = [3,16,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 19, appliedFact = [14,16,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 18, appliedFact = [17,16,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 17, appliedFact = [3,8], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 14, appliedFact = [10,4], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 16, appliedFact = [10,4], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []],Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 13, appliedFact = [6,8], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 12, appliedFact = [11,8], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 11, appliedFact = [1,5], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "iOpen", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 10, appliedFact = [3,6,4], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 31, appliedFact = [14,9,16], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 30, appliedFact = [17,9,16], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 29, appliedFact = [18,24], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 28, appliedFact = [19,24], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 27, appliedFact = [20,24], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 26, appliedFact = [3,9,16], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "iOpen", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 25, appliedFact = [20,6,21], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 34, appliedFact = [27,8], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 33, appliedFact = [28,8], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 32, appliedFact = [29,8], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]]]}}]  
                              [(Map.fromList [("E__db__revoked__PK",Composed "db__revoked" [Variable "S3",Variable "A3"]),("E__db__valid__PK",Composed "0" []),("E__ring__PK",Composed "0" [])],Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))],[7])]]
-}
                              
{-[unifyElement (Map.fromList [("A",("isAgent",[0]))])
                             [FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 7, appliedFact = [3,6,4], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]]}},
                             FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 8, appliedFact = [3,6,4], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []],Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]}},
                             FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 9, appliedFact = [3,6,4], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []],Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 6, appliedFact = [2,5], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 3, appliedFact = [1,2], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 4, appliedFact = [1,2], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []]]}},
                             FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 5, appliedFact = [1,2], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []],Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "keyReg", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 1, appliedFact = [], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "keyReg", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 2, appliedFact = [], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "iAgents", typeInfo = Map.fromList [("A0",("isAgent",[0]))], identityNo = 0, appliedFact = [], fact = FPFact {fpFactName = "iknows", fpArgs = [Variable "A0"]}},
                             FP {fpRuleName = "serverUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 24, appliedFact = [20,6,21], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []],Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 21, appliedFact = [15,16,9], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 20, appliedFact = [3,16,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 19, appliedFact = [14,16,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 18, appliedFact = [17,16,9], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 17, appliedFact = [3,8], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 14, appliedFact = [10,4], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []]]]]}},
                             FP {fpRuleName = "userUpdateKey", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 16, appliedFact = [10,4], fact = FPFact {fpFactName = "timplies", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []],Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 13, appliedFact = [6,8], fact = FPFact {fpFactName = "occurs", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 12, appliedFact = [11,8], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isUser",[0,2])),("S3",("isServer",[0,1]))], identityNo = 11, appliedFact = [1,5], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]}},
                             FP {fpRuleName = "iOpen", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 10, appliedFact = [3,6,4], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "ring" [Variable "A3"],Composed "0" [],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 31, appliedFact = [14,9,16], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 30, appliedFact = [17,9,16], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 29, appliedFact = [18,24], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 28, appliedFact = [19,24], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 27, appliedFact = [20,24], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 26, appliedFact = [3,9,16], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "ring" [Variable "A3"],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]]]]}},
                             FP {fpRuleName = "iOpen", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 25, appliedFact = [20,6,21], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 34, appliedFact = [27,8], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "db__valid" [Variable "S3",Variable "A3"],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 33, appliedFact = [28,8], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "0" []]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]]]}},
                             FP {fpRuleName = "timplies", typeInfo = Map.fromList [("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))], identityNo = 32, appliedFact = [29,8], fact = FPFact {fpFactName = "iknows", fpArgs = [Composed "sign" [Composed "inv" [Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]],Composed "pair" [Variable "A3",Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]]]]]}}]  
                             (FPFact {fpFactName = "iknows", fpArgs = [Composed "inv" [Composed "val" [Variable "E__ring__PK",Variable "E__db__valid__PK",Variable "E__db__revoked__PK"]]]}) ]

-}                             
                             
{-[compareTerms (Variable "A") 
                             (Composed "val" [Composed "0" [],Composed "0" [],Composed "db__revoked" [Variable "S3",Variable "A3"]])
                             (Map.fromList [("A",("isAgent",[0])),("A3",("isHonest",[0,2,22])),("S3",("isServer",[0,1]))])                             ]
-}
                              
main = do putStr $ concatMap show testFixPoint
-}


{----------------------  Calculate FixPoints  -------------------------------}              
--rename all the variables in PRules in a pattern like "A1,A2,...,An"
rename :: PRule -> Int -> PRule
rename (PRule ruleName ruleEnv lf lp ln freash rf rp) count = 
       PRule ruleName ruleEnv (numberedFactArgs lf count) lp ln freash (numberedFactArgs rf count) rp

--travese the PFact list to get the variable and number it
numberedFactArgs :: [PFact] -> Int -> [PFact]
numberedFactArgs [] _ = []
numberedFactArgs ( (PFact fName [PAtom aName]) : rest ) count = 
             (PFact fName [PAtom (aName++(show count))]) : numberedFactArgs rest count      
numberedFactArgs ( (PFact fName fargs) : rest ) count = 
             (PFact fName (numberedPCompArgs fargs count)) : numberedFactArgs rest count


{--- Comment Sebastian: the renaming has a problem because, PAtom is not necessarily a variable (it could be a constant); please check, maybe use the same data structures as in unification!  

     Another thing: why is there a distinction between the case of   (PFact _ [PAtom _]) and the other (PFact _ _) cases?? 

 ---}

--rename all variables in PComp    
numberedPCompArgs :: [PTerm] -> Int -> [PTerm]
numberedPCompArgs [] _ = []
numberedPCompArgs (( PComp cName pAtoms ) : rest2) count = 
                  ( PComp cName (numberPAtoms pAtoms count) ) : numberedPCompArgs rest2 count
numberedPCompArgs pAtoms count = 
                  numberPAtoms pAtoms count

--rename all variables in PAtoms    
numberPAtoms :: [PTerm] -> Int -> [PTerm]
numberPAtoms [] _ = []
numberPAtoms ((PAtom aName):rest) count = 
             (PAtom (aName++(show count))) : numberPAtoms rest count 
numberPAtoms ( (PComp cName cArgs):rest ) count  = 
             (PComp cName (numberedPCompArgs cArgs count)) : numberPAtoms rest count     

--Define fixedpoint data type. The fixedpoint is represented as a list of FP
data FP = FP {fpRuleName :: String,
              typeInfo :: Typing,
              identityNo :: Int,
              appliedFact :: AppliedFact,
              fact :: FPFact}
              deriving (Show)

type Typing = Map.Map Variable Typename
type Typename = ( String, [Int] )
type TypeInfo = [Int]
type AppliedFact = [Int]
 

data FPFact = FPFact {fpFactName :: String,
                      fpArgs :: [Term]}
                      deriving (Show)

data MRule = MRule {mRuleName :: String,
                    mtyping :: Typing,
                    mlf :: [FPFact],
                    mrf :: [FPFact]}
                    deriving (Show)

-- Arguments: ruls, fixpoint, user type tree, serial number, rename counter
fixpoint :: [PRule] -> [FP] -> [UserTypeTree] -> Int -> Int -> [FP]
fixpoint rules fp userTypes serialNo reNameCounter = 
        let newfp = deriveOneStep rules fp userTypes serialNo reNameCounter
            newfp' = fixedpointComparsion newfp fp
        in if null newfp'
              then fp
              else let fp' = fixedpointComparsion fp newfp'
                   in fixpoint rules (fp' ++ newfp') userTypes serialNo reNameCounter
         
deriveOneStep :: [PRule] -> [FP] -> [UserTypeTree] -> Int -> Int -> [FP]
deriveOneStep [] fp userTypes serialNo reNameCounter = fp
deriveOneStep ( (PRule rName rEnv lf lp ln fresh rf rp) : rest ) fp userTypes serialNo reNameCounter = 
               -- rename the rule here
               let renamedRule = rename (PRule rName rEnv lf lp ln fresh rf rp) reNameCounter 
                   extendedFP = extendOneRuleToFP renamedRule fp userTypes serialNo
                   extendedSerialNo = checkSerialNo extendedFP
               in deriveOneStep rest extendedFP userTypes (extendedSerialNo+1) (reNameCounter+1)

{- Comment Sebastian: serialNo and reNameCounter unclear -}

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
              {-  Comment Sebastian: These terms of the form  (Composed "" _) seem like a hack!  -}

              comp = compareTerms term fpTerm ( Map.union typing typing' )
          in if comp /= Equal || rName /= rName' || factName /= factName'
          {- Comment Sebastian: I would put factName first!  Also why rName?? -}
                          then isCovered (FP rName typing identityNo appliedFact (FPFact factName terms)) rest
                          else True
               
checkSerialNo :: [FP] -> Int
checkSerialNo [] = 0
checkSerialNo ( (FP _ _ serialNo _ _):rest ) = 
              let nextSerialNo = checkSerialNo rest
              in if serialNo > nextSerialNo
                    then serialNo
                    else nextSerialNo
{- Comment Sebastian: isn't this just Max? -}
                              
extendOneRuleToFP :: PRule -> [FP] -> [UserTypeTree] -> Int -> [FP]
extendOneRuleToFP (PRule rName rEnv lf lp ln fresh rf rp) fp userTypes serialNo = 
                  let (MRule ruleName typing mlf mrf) = convertToMRule (PRule rName rEnv lf lp ln fresh rf rp) userTypes
                  in compareLHSwithFP rName typing mlf mrf fp serialNo

--convert PRule to MRule so that the data becomes self defined data
convertToMRule :: PRule -> [UserTypeTree] -> MRule
convertToMRule (PRule rName _ lf _ _ _ rf _) userTypeTree = 
               MRule rName (getTyping lf userTypeTree) (convertToTerms lf) (convertToTerms rf) 

getTyping :: [PFact] -> [UserTypeTree] -> Typing
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
                 if fName == "iknows" || fName == "occurs" || fName == "timplies"
                    then (FPFact fName (transferPTermToTerm fArgs)) : convertToTerms rest 
                    else convertToTerms rest                 
                 
transferPTermToTerm :: [PTerm] -> [Term]
transferPTermToTerm [] = []
transferPTermToTerm ((PAtom aName):rest) = (Variable aName) : transferPTermToTerm rest
transferPTermToTerm ((PComp cName cArgs):rest) = 
               (Composed cName (transferPTermToTerm cArgs)) : transferPTermToTerm rest                

-- Arguments: rule name, lfethandside user types, lefthandside facts, righthandside facts, fixpoints, usertypetree,serialno
-- compare if left hand side facts are contained in the fixpoint, as well as the use types are not disjoint.
compareLHSwithFP :: String -> Typing -> [FPFact] -> [FPFact] -> [FP] -> Int -> [FP]
compareLHSwithFP rName typing [] mrf fp serialNo = addFactToFP rName typing mrf fp serialNo []
compareLHSwithFP rName typing mlf mrf fp serialNo = 
                let unifyResults = unifySubset typing mlf fp
                --let unifyResult = unifySubset typing mlf fp
                in if isNothing unifyResults
                      then fp 
                      --subsitute right hand side facts
                      else  concatSubstutionToLHSFact rName mrf fp serialNo (fromJust unifyResults)
                      --concatMap (\(subst, subTyping, appliedFact) -> addFactToFP rName subTyping (map (subsitute subst) mrf) fp serialNo appliedFact ) (fromJust unifyResults)
                      {-let (subst, subTyping, appliedFact) = fromJust unifyResult
                           in addFactToFP rName subTyping (map (subsitute subst) mrf) fp serialNo appliedFact                    
                      -}

{- Comment Sebastian: why does this basically give the entire fixedpoint back, instead of just the new facts, if any?? -}

concatSubstutionToLHSFact :: String -> [FPFact] -> [FP] -> Int -> [(Subst, Typing, AppliedFact)] -> [FP]
concatSubstutionToLHSFact _ _ fp _ [] = fp
concatSubstutionToLHSFact rName mrf fp serialNo ((subst, typing, appliedFact):rest) = 
                          let newFP = addFactToFP rName typing (map (subsitute typing subst) mrf) fp serialNo appliedFact
                              newSerialNo = checkSerialNo newFP
                          in concatSubstutionToLHSFact rName mrf newFP (newSerialNo+1) rest

{- Comment Sebastian: What is appliedFact here? what does this really do?  -}

subsitute :: Typing -> Subst -> FPFact -> FPFact
subsitute typing subst (FPFact factName terms) = 
          FPFact factName (map (subsituteTerms typing subst) terms)

subsituteTerms :: Typing -> Subst -> Term -> Term     
subsituteTerms typing subst (Variable v) = 
          let sub = Map.lookup v subst           
          in case sub of
             Nothing -> Variable v
             Just (Composed c cs) -> let typeInfo = Map.lookup v typing
                                     in case typeInfo of 
                                        Nothing -> Composed c cs
                                        Just typingName -> Variable v                                       
             Just (Variable s) -> Variable s             
subsituteTerms typing subst (Composed cName cArgs) = 
              (Composed cName (map (subsituteTerms typing subst) cArgs) ) 

---Unify left hand side facts
unifySubset :: Typing -> [FPFact] -> [FP] -> Maybe [(Subst, Typing, AppliedFact)]
unifySubset typing [] fp = Just []
unifySubset typing (fpFact:rest) fp = 
                     let substResult = unifyElement typing fp fpFact
                     in if null substResult
                           then Nothing
                           else let unificatonRestult =  unifyRestLFSubSet typing rest fp substResult
                                in if null unificatonRestult
                                   then Nothing
                                   else Just unificatonRestult

unifyElement :: Typing -> [FP] -> FPFact -> [(Subst, Typing, AppliedFact)]
unifyElement typing [] fpFact = []
unifyElement typing ((FP rName fpTyping serialNo fpAppliedFact (FPFact fpFactName fpTerms)):rest) (FPFact factName terms) = 
                     if factName == fpFactName
                        then let term = Composed "" terms
                                 fpTerm = Composed "" fpTerms
                                 comp = compareTerms term fpTerm (Map.union typing fpTyping)
                                 restSubResults = unifyElement typing rest (FPFact factName terms)
{- comment Sebastian: 

   Why is this so complicated with "restSubResults"? You are adding this in each and every case, but so why not factor that out?

   Also you get several times into unification, once in the comparison and then again -- couldn't this code be written a bit clearer and more systematic?

   Also, do you know "case" ?   -}                              
                             in if Disjoint == comp
                                   then restSubResults
                                   else if Greater == comp
                                           then let subsResult = unifyM ((zip terms fpTerms),Map.fromList [])
                                                in (fromJust subsResult, fpTyping, [serialNo]) : restSubResults
                                           else let subsResult = unifyM ((zip terms fpTerms),Map.fromList [])
                                                in (fromJust subsResult, subsituteType fpTyping typing (fromJust subsResult), [serialNo]) : restSubResults
                        else unifyElement typing rest (FPFact factName terms)


unifyRestLFSubSet :: Typing -> [FPFact] -> [FP] -> [(Subst, Typing, AppliedFact)] -> [(Subst, Typing, AppliedFact)]
unifyRestLFSubSet typing [] _ subsitutionInfo = subsitutionInfo
unifyRestLFSubSet typing _ _ [] = []
unifyRestLFSubSet typing fpFacts fp ( ((subst, substTyping, appliedFact)):rest)  =
                  let unionTyping = Map.union typing substTyping
                      substFPFact = map (subsitute unionTyping subst) fpFacts
                      substFPFactResult = unifyEachRestLFSubSet typing (subst, substTyping, appliedFact) fp substFPFact   --problem
                  in if isNothing substFPFactResult
                     then unifyRestLFSubSet typing fpFacts fp rest
                     else (fromJust substFPFactResult) : unifyRestLFSubSet typing fpFacts fp rest

unifyEachRestLFSubSet :: Typing -> (Subst, Typing, AppliedFact) -> [FP] -> [FPFact] -> Maybe (Subst, Typing, AppliedFact)
unifyEachRestLFSubSet typing substitutionInfo fp [] = Just substitutionInfo
unifyEachRestLFSubSet typing (subst, substTyping, appliedFact) fp ((fpFact):rest) = 
                      let newTyping = subsituteType typing substTyping subst
                          substFPFactResults = unifyElement (Map.union substTyping newTyping) fp fpFact
                      in if null substFPFactResults
                            then Nothing
                            else let (substUnion, typingUnion, appliedFactUnion) = unionResults (subst, substTyping, appliedFact) substFPFactResults
                                 in  unifyEachRestLFSubSet typing (substUnion, typingUnion, appliedFactUnion) fp rest
                     
unionResults :: (Subst, Typing, AppliedFact) -> [(Subst, Typing, AppliedFact)] -> (Subst, Typing, AppliedFact)
unionResults subsitutionInfo [] = subsitutionInfo
unionResults (subst, typing, appliedFact) ( ((subst', typing', appliedFact')):rest ) = 
             unionResults (Map.union subst subst', Map.union typing typing', union appliedFact appliedFact') rest 
                     
subsituteType :: Typing -> Typing -> Subst -> Typing
subsituteType fpTyping typing subst =  
              Map.fromList (map (subsituteTermType (Map.toList typing) subst) (Map.toList fpTyping) )

subsituteTermType :: [(Variable, Typename)] -> Subst -> (Variable, Typename) -> (Variable, Typename)
subsituteTermType [] subst (fpS,fpTypeName) = (fpS,fpTypeName)
subsituteTermType ((s,typeName):restTyping) subst (fpS,fpTypeName) = 
                  case Map.findWithDefault (Composed "" []) s subst of
                       Composed c cs -> subsituteTermType restTyping subst (fpS,fpTypeName)                  
                       Variable v -> if v == fpS 
                                        then (fpS,typeName)
                                        else subsituteTermType restTyping subst (fpS,fpTypeName)
                
-- Term comparison
compareTerms :: Term -> Term -> Typing -> Comp
compareTerms s t typing = 
             case unifyM ([(s,t)],Map.fromList []) of 
                  Nothing -> Disjoint
                  (Just substitution) -> comparison s t substitution typing 

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
                 else if Map.findWithDefault (Composed "-1" []) v1 substitution == Variable v2 || 
                         Map.findWithDefault (Composed "-1" []) v2 substitution == Variable v1
                         then let ( typeName1, typeList1 ) = Map.findWithDefault ("NotFind",[]) v1 typing
                                  ( typeName2, typeList2 ) = Map.findWithDefault ("NotFind",[]) v2 typing
                             in if typeName1 /= "NotFind" && typeName2 /= "NotFind"
                                then compPos typeList1 typeList2
                                else Disjoint
                       else error ("This should not have happened " ++ v1 ++ " -> " ++ v2 ++ " \n subs: " ++ (show substitution) ++ " \n typing: " ++ (show typing) )


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

               
-- arguments: ruleName, user types (terms), rhFacts, fixpoint , serial no.
addFactToFP :: String -> Typing -> [FPFact] -> [FP] -> Int -> AppliedFact -> [FP]               
addFactToFP rName typing (fpFact : rest) [] identityNo appliedFact = 
            ( FP rName typing identityNo appliedFact fpFact ) : (addFactToFP rName typing rest [] (identityNo+1) appliedFact )
addFactToFP rName typing [] [] identityNo appliedFact = []
addFactToFP rName typing [] fp identityNo appliedFact = fp
addFactToFP rName typing mrf fp identityNo appliedFact = 
            let result = unifySubset typing mrf fp
            in if isNothing result 
                  then (addFactToFP rName typing mrf [] identityNo appliedFact) ++ fp 
                  else fp
                  {-let (subst, subTyping, appliedFact1) = fromJust result
                       in if "timplies" == rName && Map.empty /= subst
                             then (addFactToFP rName typing mrf [] identityNo appliedFact) ++ fp
                             else fp-}

---------------------------Compute general variables algorithm----------------------------------------------
data Comp = Greater
          | Equal
          | Smaller
          | Disjoint
          | Overlap
          deriving(Eq, Show)

--Is the first argument less general than the second argument.
compPos :: [Int] -> [Int] -> Comp
--compPos [-1] _ = Disjoint
--compPos _ [-1] = Disjoint
compPos [] [] = Equal 
compPos _ [] = Smaller 
compPos [] _ = Greater 
compPos (x:xs) (y:ys) = 
        if x == y
          then compPos xs ys
          else Disjoint

prefix :: String -> [UserTypeTree] -> Int -> [Int]
prefix _ [] _ = []
prefix typeName ((Node s ts):rest) node = 
                      if typeName == s
                         then [node]
                         else if treeBranch typeName ts
                                 then [node] ++ (prefix typeName ts (node*10+1) )
                                 else (prefix typeName rest (node+1) )
       
treeBranch :: String -> [UserTypeTree] -> Bool
treeBranch _ [] = False
treeBranch typeName ((Node s ts):rest) = 
            if typeName == s
               then True
               else treeBranch typeName ts || treeBranch typeName rest


{---Type subsitution
unifyTyped :: Term -> Term -> Typing -> Maybe Subst
unifyTyped s t typing = 
           case unifyM ([(s,t)],Map.fromList []) of 
                  Nothing -> Nothing
                  subsitution -> unifyTypedComparison s t subsitution typing             

unifyTypedComparison :: Term -> Term -> Maybe Subst -> Typing -> Maybe Subst
unifyTypedComparison (Variable v) (Composed c cs) (Just subsitution) typing = Just (Map.insert v (Composed c cs) Map.empty)
unifyTypedComparison (Composed c1 cs1) (Composed c2 cs2) (Just subsitution) typing = 
             unifyTypedComparisonList (zip cs1 cs2) (Just subsitution) typing
unifyTypedComparison (Variable v1) (Variable v2) (Just subsitution) typing = 
              if Map.findWithDefault (Composed "-1" []) v1 subsitution == Variable v2
                 then let ( typeName1, typeList1 ) = Map.findWithDefault ("", [-1]) v1 typing
                          ( typeName2, typeList2 ) = Map.findWithDefault ("", [-1]) v2 typing
                          comp = compPos typeList1 typeList2
                      in if comp == Greater
                            then Just (Map.insert v1 (Variable v2) Map.empty)
                            else Just (Map.insert v2 (Variable v1) Map.empty)--seems like useless
                 else Nothing

unifyTypedComparisonList :: [(Term,Term)] -> Maybe Subst -> Typing -> Maybe Subst
unifyTypedComparisonList [] (Just subsitution) typing = Just Map.empty
unifyTypedComparisonList ((s,t):rest) (Just subsitution) typing = 
                          let subst1 = unifyTypedComparison s t (Just subsitution) typing 
                              subst2 = unifyTypedComparisonList rest (Just subsitution) typing
                              in if (isJust subst1) && (isJust subst2)
                                    then Just (Map.union (fromJust subst1) (fromJust subst2))
                                    else Nothing
                                    
--compare if a set of facts are contained in the fixpedoints.  
--for all fact, fact belongs to the first arugment. If all fact also exists in fixpoint, then return a set of subsitutions. Otherwise false.                 
unifySubset :: Typing -> [FPFact] -> [FP] -> Maybe (Subst, Typing, AppliedFact)
unifySubset typing [] fp = Just (Map.fromList [],Map.fromList [], [])
unifySubset typing (fpFact:rest) fp = 
                     let substResult = unifyElement typing fpFact fp
                     in if isNothing substResult
                           then Nothing
                           else let (subst, typing1,appliedFact1) = fromJust substResult
                                    restSubstResult = unifySubset typing rest fp
                                in if isNothing restSubstResult
                                      then Nothing 
                                      else let (restSubst, typing2, appliedFact2) = fromJust restSubstResult
                                           in Just ( Map.union subst restSubst, Map.union typing1 typing2, appliedFact1 ++ appliedFact2 )

unifyElement :: Typing -> FPFact -> [FP] -> Maybe (Subst, Typing, AppliedFact)
unifyElement typing fpFact [] = Nothing
unifyElement typing (FPFact factName terms) ((FP rName fpTyping serialNo fpAppliedFact (FPFact fpFactName fpTerms)):rest) = 
                     if factName == fpFactName
                        then let term = Composed "" terms
                                 fpTerm = Composed "" fpTerms
                                 comp = compareTerms term fpTerm (Map.union typing fpTyping)
                             in if Disjoint == comp
                                   then unifyElement typing (FPFact factName terms) rest
                                   else if Greater == comp
                                           then let subsResult = unifyM ((zip terms fpTerms),Map.fromList [])
                                                in Just (fromJust subsResult, fpTyping, [serialNo])
                                           else let subsResult = unifyM ((zip terms fpTerms),Map.fromList [])
                                                in Just (fromJust subsResult, subsituteType fpTyping typing (fromJust subsResult), [serialNo])
                        else unifyElement typing (FPFact factName terms) rest                                     
                                    
                                    -}


















