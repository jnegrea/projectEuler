test = [(ms,ns)| 
	m1<-[5..9],
	m2<-[4..(m1-1)],
	m3<-[3..(m2-1)],
	m4<-[2..(m3-1)],
	m5<-[1..(m4-1)],
	m6<-[0..(m5-1)],
	n1<-[5..9],
	n2<-[4..(n1-1)],
	n3<-[3..(n2-1)],
	n4<-[2..(n3-1)],
	n5<-[1..(n4-1)],
	n6<-[0..(n5-1)],
	let ms = [m1,m2,m3,m4,m5,m6],
	let ns = [n1,n2,n3,n4,n5,n6],
	ms>=ns,
	((0 `elem` ms)&&(1 `elem` ns))||((0 `elem` ns)&&(1 `elem` ms)),
	((0 `elem` ms)&&(4 `elem` ns))||((0 `elem` ns)&&(4 `elem` ms)),
	((0 `elem` ms)&&(6 `elem` ns))||((0 `elem` ns)&&(6 `elem` ms))||((0 `elem` ms)&&(9 `elem` ns))||((0 `elem` ns)&&(9 `elem` ms)),
	((1 `elem` ms)&&(6 `elem` ns))||((1 `elem` ns)&&(6 `elem` ms))||((1 `elem` ms)&&(9 `elem` ns))||((1 `elem` ns)&&(9 `elem` ms)),
	((2 `elem` ms)&&(5 `elem` ns))||((2 `elem` ns)&&(5 `elem` ms)),
	((3 `elem` ms)&&(6 `elem` ns))||((3 `elem` ns)&&(6 `elem` ms))||((3 `elem` ms)&&(9 `elem` ns))||((3 `elem` ns)&&(9 `elem` ms)),
	((4 `elem` ms)&&(6 `elem` ns))||((4 `elem` ns)&&(6 `elem` ms))||((4 `elem` ms)&&(9 `elem` ns))||((4 `elem` ns)&&(9 `elem` ms)),
	((8 `elem` ms)&&(1 `elem` ns))||((8 `elem` ns)&&(1 `elem` ms)),
	True
	 ]

main =do
		print (length(test))