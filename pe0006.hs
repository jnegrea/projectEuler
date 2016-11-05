intToList 0 = [];
intToList n = (mod n 10):(intToList (div (n-(mod n 10)) 10))

maxProdList (x:xs) n currmax isStart
	|(length (x:xs)) <n =currmax
	|isStart == 1 = maxProdList xs n (x*(product (take (n-1) xs))) 0
	|otherwise =maxProdList xs n (max currmax (x*(product (take (n-1) xs)))) 0