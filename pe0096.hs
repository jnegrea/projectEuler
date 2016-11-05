updateSuDoKu_SimpleDeduction "row" n suDoKu
	|length (takeWhile (==0) (sort (suDoKu!!n)))==1 = let missingVal = head ([1..9]\\(suDoKu!!n)) in map (\x->if x==0 then missingVal else x) (suDoKu!!n)
	|otherwise suDoKu

updateSuDoKu_SimpleDeduction "col" n suDoKu = transpose (updateSuDoKu_SimpleDeduction "row" n (transpose suDoKu))

updateSuDoKu_SimpleDeduction "box" n suDoKu = let pseudoRow =[]