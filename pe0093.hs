import Data.Char

isNumStr :: String->Bool
isNumStr [] = True
isNumStr (c:str)
	|isNumber c = isNumStr str
	|otherwise = False

maybeRRPN' :: [Maybe String] -> [String] -> [(Maybe String)]
maybeRRPN' [] [] = []
maybeRRPN' stack [] = stack
maybeRRPN' [] (str0:lostr)
	|isNumStr str0 = maybeRRPN' ((Just str0):[]) lostr
maybeRRPN' [str1] (str0:lostr)
	|isNumStr str0 = maybeRRPN' ((Just str0):str1:[]) lostr
maybeRRPN' (str1:str2:stack) (str0:los)
	|isNumStr str0 = maybeRRPN' ((Just str0):stack) los
	|str0 == "+"= maybeRRPN' ((maybeShow (maybeAdd (maybeRead(str1)::Maybe Int) (maybeRead(str2)::Maybe Int))):stack) los
	|str0 == "-"= maybeRRPN' ((maybeShow (maybeSub (maybeRead(str1)::Maybe Int) (maybeRead(str2)::Maybe Int))):stack) los
	|str0 == "*"= maybeRRPN' ((maybeShow (maybeMult (maybeRead(str1)::Maybe Int) (maybeRead(str2)::Maybe Int))):stack) los
	|str0 == "/"= maybeRRPN' ((maybeShow (maybeDiv (maybeRead(str1)::Maybe Int) (maybeRead(str2)::Maybe Int))):stack) los	
maybeRRPN' _ _ =[]

--maybeRRPN :: Maybe [String] -> Maybe String
--maybeRRPN (Just [c]) = Just c
--maybeRRPN _ = Nothing

maybeOper oper Nothing _ = Nothing
maybeOper oper _ Nothing = Nothing
maybeOper oper (Just a) (Just b) = Just (a `oper` b)

maybeAdd= maybeOper (+)
maybeSub= maybeOper (-)
maybeMult= maybeOper (*)

maybeDiv Nothing _ =Nothing
maybeDiv _ Nothing =Nothing
maybeDiv (Just a) (Just b)
	|a `mod` b == 0 = Just (div a b)
	|otherwise = Nothing

maybeShow Nothing =Nothing
maybeShow (Just a) = Just (show a)

maybeRead Nothing =Nothing
maybeRead (Just a) = Just (read a)

main = do
	--print (((Just 3) `maybeDiv` (Just 1)) ::Maybe Int)
	print $ maybeRRPN' [] ["4","3","*","4","1"]