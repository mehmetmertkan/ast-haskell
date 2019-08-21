module Hw1 where

type Mapping = [(String, String, String)]
data AST = EmptyAST | ASTNode String AST AST deriving (Show, Read)

writeExpression :: (AST, Mapping) -> String
evaluateAST :: (AST, Mapping) -> (AST, String)
-- DO NOT MODIFY OR DELETE THE LINES ABOVE -- 
-- IMPLEMENT writeExpression and evaluateAST FUNCTION ACCORDING TO GIVEN SIGNATURES -- 
type MappingEle = (String,String,String)

writeExpression2Empty :: (AST, Mapping) -> String

getFirst :: (String, String, String) -> String
getFirst (frst, scnd, thrd) = frst 

getSecond :: (String, String, String) -> String
getSecond (frst, scnd, thrd) = scnd 

getThird :: (String, String, String) -> String
getThird (frst, scnd, thrd) = thrd 


writeExpression2Empty (ASTNode str a b,[x]) = if getSecond x == "str" then getFirst x ++ "=\"" ++ getThird x ++ "\" in " ++ writeExpression (ASTNode str a b,[])
    else getFirst x ++ "=" ++ getThird x ++ " in " ++ writeExpression (ASTNode str a b,[])
writeExpression2Empty (ASTNode str a b,(x:xs)) = if getSecond x == "str" then getFirst x ++ "=\"" ++ getThird x ++ "\";" ++ writeExpression2Empty (ASTNode str a b, xs)
    else getFirst x ++ "=" ++ getThird x ++ ";" ++ writeExpression2Empty (ASTNode str a b, xs)


writeExpression (ASTNode str a b,[x]) = if getSecond x == "str" then "let " ++ getFirst x ++ "=\"" ++ getThird x ++ "\" in " ++ writeExpression (ASTNode str a b,[])
    else "let " ++ getFirst x ++ "=" ++ getThird x ++ " in " ++ writeExpression (ASTNode str a b,[])
writeExpression (ASTNode str a b,(x:xs)) = if getSecond x == "str" then "let " ++ getFirst x ++ "=\"" ++ getThird x ++ "\";" ++ writeExpression2Empty (ASTNode str a b,xs)
    else "let " ++ getFirst x ++ "=" ++ getThird x ++ ";" ++ writeExpression2Empty (ASTNode str a b,xs)


writeExpression (ASTNode str EmptyAST EmptyAST,[]) = str


writeExpression (ASTNode str left EmptyAST,[]) 
    | str == "num" = writeExpression (left,[])
    | str == "str" = "\"" ++ writeExpression (left,[]) ++ "\""
--    | str == "plus" = "+" ++ writeExpression (left,[])
--    | str == "times" = "*" ++ writeExpression (left,[])
    | str == "negate" = "(" ++ "-" ++ writeExpression (left,[]) ++ ")"
--    | str == "cat" = "(" ++ "\"" ++  "++" ++ writeExpression (left,[]) ++ "\"" ++ ")"
    | str == "len" = "(" ++ "length " ++ writeExpression (left,[]) ++ ")"
    | otherwise = "(" ++ str ++ writeExpression (left,[]) ++ ")"


writeExpression (ASTNode str EmptyAST right,[])
    | str == "num" = writeExpression (right,[])
    | str == "str" = "\"" ++ writeExpression (right,[]) ++ "\""
--    | str == "plus" = "+" ++ writeExpression (right,[])
--    | str == "times" = "*" ++ writeExpression (right,[])
    | str == "negate" = "(" ++ "-" ++ writeExpression (right,[]) ++ ")"
--    | str == "cat" = "(" ++ "\"" ++ "++" ++ writeExpression (right,[]) ++ "\"" ++ ")"
    | str == "len" = "(" ++ "length " ++ writeExpression (right,[]) ++ ")"
    | otherwise = "(" ++ str ++ writeExpression (right,[]) ++ ")"


writeExpression (ASTNode str left right,[])
    | str == "num" = writeExpression (left,[]) ++ writeExpression (right,[])
    | str == "str" = writeExpression (left,[]) ++ writeExpression (right,[])
    | str == "plus" = "(" ++ writeExpression (left,[]) ++ "+" ++ writeExpression (right,[]) ++ ")"
    | str == "times" = "(" ++ writeExpression (left,[]) ++ "*" ++ writeExpression (right,[]) ++ ")"
--    | str == "negate" = writeExpression (left,[]) ++ "-" ++ writeExpression (right,[])
    | str == "cat" = "(" ++ writeExpression (left,[]) ++ "++" ++ writeExpression (right,[]) ++ ")"
--    | str == "len" = "(" ++ writeExpression (left,[]) ++ "length " ++ writeExpression (right,[]) ++ ")"
    | otherwise = "(" ++ writeExpression (left,[]) ++ str ++ writeExpression (right,[]) ++ ")"





mappingEleToAST :: (String, String, String) -> AST
mappingEleToAST (a,b,c) = (ASTNode b (ASTNode c EmptyAST EmptyAST) EmptyAST) 

numAndStrWriter :: (AST) -> String
numAndStrWriter (ASTNode str EmptyAST EmptyAST) = str

adder :: (String,String) -> String
adder (num1, num2) = show ((read num1) + (read num2))

multiplier :: (String,String) -> String
multiplier (num1, num2) = show ((read num1) * (read num2))

negater :: String -> String
negater num = show ((-1)*(read num))

evaluateNegate :: (AST, Mapping) -> String
evaluateNegate (ASTNode str left right, []) 
    | str == "num" = negater (numAndStrWriter left)
    | str == "negate" = negater(evaluateNegate(left, []))
    | str == "len" = negater(evaluateLen(left, []))
    | str == "times" = negater(evaluateTimes(left,right, []))
    | str == "plus" = negater(evaluatePlus(left,right, []))
    | otherwise = "error 1"

evaluateLen :: (AST, Mapping) -> String
evaluateLen (ASTNode str left right, []) 
    | str == "str" = show (length (numAndStrWriter left))
    | str == "cat" = show (length (evaluateCat(left,right,[])))
    | otherwise = "error 2"

evaluatePlus :: (AST, AST, Mapping) -> String
evaluatePlus (ASTNode strLeft leftLeft leftRight, ASTNode strRight rightLeft rightRight, [])
    | strLeft == "num" && strRight == "num" = adder(numAndStrWriter leftLeft, numAndStrWriter rightLeft)
    | strLeft == "num" && strRight == "negate" = adder(numAndStrWriter leftLeft, evaluateNegate(rightLeft, []))
    | strLeft == "negate" && strRight == "num" = adder(evaluateNegate(leftLeft, []), numAndStrWriter rightLeft)
    | strLeft == "negate" && strRight == "negate" = adder(evaluateNegate(leftLeft, []), evaluateNegate (rightLeft,[]))
    | strLeft == "plus" && strRight == "plus" = adder(evaluatePlus(leftLeft, leftRight, []), evaluatePlus(rightLeft,rightRight, []))
    | strLeft == "plus" && strRight == "negate" = adder(evaluatePlus(leftLeft, leftRight, []), evaluateNegate(rightLeft, []))
    | strLeft == "negate" && strRight == "plus" = adder(evaluateNegate(leftLeft, []), evaluatePlus(rightLeft,rightRight, []))
    | strLeft == "times" && strRight == "num" = adder(evaluateTimes(leftLeft, leftRight, []), numAndStrWriter rightLeft)
    | strLeft == "num" && strRight == "times" = adder(numAndStrWriter leftLeft, evaluateTimes(rightLeft, rightRight, []))
    | strLeft == "plus" && strRight == "num" = adder(evaluatePlus(leftLeft, leftRight, []), numAndStrWriter rightLeft)
    | strLeft == "num" && strRight == "plus" = adder(numAndStrWriter leftLeft, evaluatePlus(rightLeft, rightRight, []))
    | strLeft == "times" && strRight == "times" = adder(evaluateTimes(leftLeft, leftRight, []), evaluateTimes(rightLeft, rightRight, []))
    | strLeft == "times" && strRight == "negate" = adder(evaluateTimes(leftLeft, leftRight, []), evaluateNegate(rightLeft, []))
    | strLeft == "negate" && strRight == "times" = adder(evaluateNegate(leftLeft, []), evaluateTimes(rightLeft,rightRight, []))
    | strLeft == "plus" && strRight == "times" = adder(evaluatePlus(leftLeft, leftRight, []), evaluateTimes(rightLeft,rightRight, []))
    | strLeft == "times" && strRight == "plus" = adder(evaluateTimes(leftLeft, leftRight, []), evaluatePlus(rightLeft,rightRight, []))
    --
    | strLeft == "len" && strRight == "len" = adder(evaluateLen(leftLeft, []), evaluateLen (rightLeft,[]))
    | strLeft == "num" && strRight == "len" = adder(numAndStrWriter leftLeft, evaluateLen(rightLeft, []))
    | strLeft == "len" && strRight == "num" = adder(evaluateLen(leftLeft, []), numAndStrWriter rightLeft)
    | strLeft == "plus" && strRight == "len" = adder(evaluatePlus(leftLeft, leftRight, []), evaluateLen(rightLeft, []))
    | strLeft == "len" && strRight == "plus" = adder(evaluateLen(leftLeft, []), evaluatePlus(rightLeft,rightRight, []))
    | strLeft == "times" && strRight == "len" = adder(evaluateTimes(leftLeft, leftRight, []), evaluateLen(rightLeft, []))
    | strLeft == "len" && strRight == "times" = adder(evaluateLen(leftLeft, []), evaluateTimes(rightLeft,rightRight, []))
    | otherwise = "error 3"

evaluateTimes :: (AST, AST, Mapping) -> String
evaluateTimes (ASTNode strLeft leftLeft leftRight, ASTNode strRight rightLeft rightRight, [])
    | strLeft == "num" && strRight == "num" = multiplier(numAndStrWriter leftLeft, numAndStrWriter rightLeft)
    | strLeft == "num" && strRight == "negate" = multiplier(numAndStrWriter leftLeft, evaluateNegate(rightLeft, []))
    | strLeft == "negate" && strRight == "num" = multiplier(evaluateNegate(leftLeft, []), numAndStrWriter rightLeft)
    | strLeft == "negate" && strRight == "negate" = multiplier(evaluateNegate(leftLeft, []), evaluateNegate (rightLeft,[]))
    | strLeft == "plus" && strRight == "plus" = multiplier(evaluatePlus(leftLeft, leftRight, []), evaluatePlus(rightLeft,rightRight, []))
    | strLeft == "plus" && strRight == "negate" = multiplier(evaluatePlus(leftLeft, leftRight, []), evaluateNegate(rightLeft, []))
    | strLeft == "negate" && strRight == "plus" = multiplier(evaluateNegate(leftLeft, []), evaluatePlus(rightLeft,rightRight, []))
    | strLeft == "times" && strRight == "num" = multiplier(evaluateTimes(leftLeft, leftRight, []), numAndStrWriter rightLeft)
    | strLeft == "num" && strRight == "times" = multiplier(numAndStrWriter leftLeft, evaluateTimes(rightLeft, rightRight, []))
    | strLeft == "plus" && strRight == "num" = multiplier(evaluatePlus(leftLeft, leftRight, []), numAndStrWriter rightLeft)
    | strLeft == "num" && strRight == "plus" = multiplier(numAndStrWriter leftLeft, evaluatePlus(rightLeft, rightRight, []))
    | strLeft == "times" && strRight == "times" = multiplier(evaluateTimes(leftLeft, leftRight, []), evaluateTimes(rightLeft, rightRight, []))
    | strLeft == "times" && strRight == "negate" = multiplier(evaluateTimes(leftLeft, leftRight, []), evaluateNegate(rightLeft, []))
    | strLeft == "negate" && strRight == "times" = multiplier(evaluateNegate(leftLeft, []), evaluateTimes(rightLeft,rightRight, []))
    | strLeft == "plus" && strRight == "times" = multiplier(evaluatePlus(leftLeft, leftRight, []), evaluateTimes(rightLeft,rightRight, []))
    | strLeft == "times" && strRight == "plus" = multiplier(evaluateTimes(leftLeft, leftRight, []), evaluatePlus(rightLeft,rightRight, []))
    --
    | strLeft == "len" && strRight == "len" = multiplier(evaluateLen(leftLeft, []), evaluateLen (rightLeft,[]))
    | strLeft == "num" && strRight == "len" = multiplier(numAndStrWriter leftLeft, evaluateLen(rightLeft, []))
    | strLeft == "len" && strRight == "num" = multiplier(evaluateLen(leftLeft, []), numAndStrWriter rightLeft)
    | strLeft == "plus" && strRight == "len" = multiplier(evaluatePlus(leftLeft, leftRight, []), evaluateLen(rightLeft, []))
    | strLeft == "len" && strRight == "plus" = multiplier(evaluateLen(leftLeft, []), evaluatePlus(rightLeft,rightRight, []))
    | strLeft == "times" && strRight == "len" = multiplier(evaluateTimes(leftLeft, leftRight, []), evaluateLen(rightLeft, []))
    | strLeft == "len" && strRight == "times" = multiplier(evaluateLen(leftLeft, []), evaluateTimes(rightLeft,rightRight, []))
    | otherwise = "error 4"

evaluateCat :: (AST, AST, Mapping) -> String
evaluateCat (ASTNode strLeft leftLeft leftRight, ASTNode strRight rightLeft rightRight, [])
    | strLeft == "str" && strRight == "str" = numAndStrWriter(leftLeft) ++ numAndStrWriter(rightLeft)
    | strLeft == "cat" && strRight == "str" = evaluateCat(leftLeft,leftRight,[]) ++ numAndStrWriter(rightLeft)
    | strLeft == "str" && strRight == "cat" = numAndStrWriter(leftLeft) ++ evaluateCat(rightLeft,rightRight,[]) 
    | strLeft == "cat" && strRight == "cat" = evaluateCat(leftLeft,leftRight,[])  ++ evaluateCat(rightLeft,rightRight,[]) 
    | otherwise = "error 5"

createAST :: (AST, MappingEle) -> (AST)

createAST (ASTNode str EmptyAST EmptyAST,(varName, numOrStr, value)) 
    | str == varName = (ASTNode numOrStr (ASTNode value EmptyAST EmptyAST) EmptyAST)
    | otherwise = (ASTNode str EmptyAST EmptyAST)

createAST (ASTNode str left EmptyAST, (varName, numOrStr, value)) = (ASTNode str (createAST(left,(varName, numOrStr, value))) EmptyAST)


createAST (ASTNode str EmptyAST right,(varName, numOrStr, value)) = (ASTNode str EmptyAST (createAST(right,(varName, numOrStr, value))))

createAST (ASTNode str left right,(varName, numOrStr, value)) = (ASTNode str (createAST((left,(varName, numOrStr, value)))) (createAST(right,(varName, numOrStr, value))))


evaluateAST (ASTNode str left right,[x]) = evaluateAST(createAST(ASTNode str left right,(getFirst x, getSecond x, getThird x)),[])

evaluateAST (ASTNode str left right,(x:xs)) = evaluateAST(createAST(ASTNode str left right,(getFirst x, getSecond x, getThird x)),xs)


evaluateAST (ASTNode str left right,[])
    | str == "num" = (ASTNode str left EmptyAST,numAndStrWriter(left))
    | str == "str" = (ASTNode str left EmptyAST,numAndStrWriter(left))
    | str == "plus" = (ASTNode str left right,evaluatePlus(left,right,[]))
    | str == "times" = (ASTNode str left right,evaluateTimes(left,right,[]))
    | str == "cat" = (ASTNode str left right,evaluateCat(left,right,[]))
    | str == "len" = (ASTNode str left right, evaluateLen(left,[]))
    | str == "negate" = (ASTNode str left right, evaluateNegate(left,[]))
    | otherwise = (ASTNode str EmptyAST EmptyAST, "error 6")
