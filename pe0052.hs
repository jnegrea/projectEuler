import Data.List
listOfDigsHelper n 
	|n<10 = [n]
	|otherwise = (mod n 10):(listOfDigsHelper (quot n 10))

listOfDigs n =sort $ listOfDigsHelper n 

test k n = listOfDigs (k*n) == listOfDigs n
testlist1 = [1..]
testlist2 = filter (test 6) testlist1
testlist3 = filter (test 5) testlist2
testlist4 = filter (test 4) testlist3
testlist5 = filter (test 3) testlist4
testlist6 = filter (test 2) testlist5

x=head testlist6

main= do
	print x
