---2 SATISFIABILITY PROBLEM---------------------------------------------------------------------------------------

--1 
data BExp = BConst Bool | Var String | And BExp BExp | Or BExp BExp |Not BExp deriving (Eq,Show)

--2 
data SatConfig = SatConfig  [(String, Bool)] [String] BExp deriving (Eq)

instance Show SatConfig where
	show (SatConfig tuple vars exp) = show tuple

instance Config SatConfig where
	successors (SatConfig list [] exp) = []
	successors (SatConfig list vars exp)=[ (SatConfig truthComb vars exp) | 			  								truthComb<-(combTruthVars vars) , truthComb /=list, 								(evaluate truthComb exp) == True ]
{-
 (SatConfig [("a",True),("b",False)] ["a","b"] (And (Var "a")(Var "b")))
[[("a",True),("b",True)]]
-}

evaluate::[(String,Bool)]->BExp->Bool
evaluate [] _ = False
evaluate list (And exp1 exp2) = (evaluate list exp1) && (evaluate list exp2)
evaluate list (Or exp1 exp2) =  (evaluate list exp1) || (evaluate list exp2)
evaluate list (Not exp1) = not (evaluate list exp1)
evaluate list (Var x) = getCorrespondingBoolValue x list
evaluate list (BConst x) = x

-- eg 1 evaluate [("a",True),("b",False)] (And (Var "a")(Var "b"))
--	True
-- eg 2 evaluate [("a",True)] (Not (Var "a"))
-- 	False

getCorrespondingBoolValue::String->[(String,Bool)]->Bool
getCorrespondingBoolValue _ [] = False
getCorrespondingBoolValue x (y:ys)
		| x == fst y = snd y
		|otherwise = getCorrespondingBoolValue x ys
-- eg 1 getCorrespondingBoolValue "a" [("a",True),("b",False)]
-- 	True
-- eg 2 getCorrespondingBoolValue "x" [("u",True),("x",False),("v",False)]
--	False 


combTruthVars::[String]->[[(String, Bool)]]
combTruthVars [] = []
combTruthVars x = (comb x (map (truefalseComb len) [0..2^len-1] ))
			where len = length(x)
--eg 1 combTruthVars ["x","y"] 
-- [[("x",False),("y",False)],[("x",True),("y",False)],[("x",False),("y",True)],[("x",True),("y",True)]]

-- eg 2 combTruthVars ["x","y","z"]
--[[("x",False),("y",False),("z",False)],[("x",True),("y",False),("z",False)],[("x",False),("y",True),("z",False)],[("x",True),("y",True),("z",False)],[("x",False),("y",False),("z",True)],[("x",True),("y",False),("z",True)],[("x",False),("y",True),("z",True)],[("x",True),("y",True),("z",True)]]


truefalseComb::Int->Int->[Bool]
truefalseComb sozeOfBit num = map (testBit num) [0..sozeOfBit-1]
-- eg 1  truefalseComb 5 1
-- [True,False,False,False,False]

-- eg 2  truefalseComb 5 2
-- [False,True,False,False,False]


--3 
isGoal::SatConfig-> Bool
isGoal (SatConfig tuple vars expression)
		| evaluate tuple expression == True = True
		| otherwise 			    = False
-- eg 1. isGoal (SatConfig [("a",True),("b",False)] ["a","b"] (And (Var "a")(Var "b")))
-- 	False
--eg 2. isGoal (SatConfig [("a",False)] ["a"] (Not (Var "a")))
--	True

						 
comb::[String]->[[Bool]]->[[(String,Bool)]]
comb x [] = []
comb x (y:ys) = [zip x y] ++ comb x ys
-- eg 1 comb ["a","b","c"] [[True, False, True], [False, True, True]]
-- [[("a",True),("b",False),("c",True)],[("a",False),("b",True),("c",True)]]

-- eg 2  comb ["a","b"] [[True, True], [False,  True]]
-- [[("a",True),("b",True)],[("a",False),("b",True)]] 


--4
satSolve::BExp-> (Maybe SatConfig)
satSolve expression = 
		let 	sigma = nub (getVariables expression)
			goal = (isGoal)
		    	initConfig = SatConfig [] (getVariables expression) expression
		in solve goal initConfig
-- eg 1. satSolve (Not ((Or (Not (Var "a")) (Not (Var "b")))))
--	Just [("a",True),("b",True)]

-- eg 2. satSolve (And ((Var "v"))  (And (Var "z") (Or (Var "x") (Var "y"))))
--	Just [("v",True),("z",True),("x",True),("y",False)]

getVariables::BExp->[String]
getVariables (And exp1 exp2) = (getVariables exp1) ++ (getVariables exp2)
getVariables (Or exp1 exp2) = (getVariables exp1) ++ (getVariables exp2)
getVariables (Not exp1) = getVariables exp1
getVariables (BConst _) = []
getVariables (Var variable) = variable: []
-- eg 1	getVariables (And (Var "a") (Var "b"))
--	["a","b"]
--eg 2 getVariables (And (And (Var "a") (Var "b")) (Not (Var "c")))
--	["a","b","c"]








