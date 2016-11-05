charMap::Char -> Int
charMap c
	|c == 'I' = 1
	|c == 'V' = 5
	|c == 'X' = 10
	|c == 'L' = 50
	|c == 'C' = 100
	|c == 'D' = 500
	|c == 'M' = 1000
	|otherwise = 0

countMap':: String ->[(Char,Int)]->[(Char,Int)]
countMap' [] loci = loci
countMap' (c1:str) [] = countMap' str [(c1,1)]
countMap' (c1:str) ((c2,i):loci)
	|c1==c2 = countMap' str ((c2,i+1):loci)
	|otherwise = countMap' str ((c1,1):(c2,i):loci)

countMap:: String ->[(Char,Int)]
countMap s = countMap' s []

collapse:: [(Char,Int)]->[(Char,Int)]
collapse [] = []
collapse ((c1,i1):(c2,i2):loci) 
	|(c1=='I' && i1>=5 && c2=='V' && i1 `mod` 5 == 0 ) 
		= (collapse (('V',i2+(div i1 5)):loci))

	|(c1=='I' && i1>=5 && c2=='V')
		= ('I',i1 `mod` 5):(collapse (('V',i2+(div i1 5)):loci))

	|(c1=='I' && i1>=5 && i1 `mod` 5 == 0)
		= (collapse (('V',(div i1 5)):(c2,i2):loci))  

	|(c1=='I' && i1>=5)
		= ('I',i1 `mod` 5):(collapse (('V',(div i1 5)):(c2,i2):loci))  

	|(c1=='I' )
		= ('I',i1):(collapse ((c2,i2):loci))  

	|(c1=='V' && i1>=2 && c2=='X' && i1 `mod` 2 ==0 )
			= collapse (('X',i2+(div i1 2)):loci)  

	|(c1=='V' && i1>=2 && c2=='X')
		= ('V',1):(collapse (('X',i2+(div i1 2)):loci))

	|(c1=='V' && i1>=2 && i1 `mod` 2 ==0 )
		= collapse (('X',(div i1 2)):(c2,i2):loci)

	|(c1=='V' && i1>=2)
		= ('V',1):(collapse (('X',(div i1 2)):(c2,i2):loci))

	|(c1=='V') = ('V',i1):(collapse ((c2,i2):loci))

	|(c1=='X' && i1>=5 && c2=='L' && i1 `mod` 5 == 0 ) 
		= (collapse (('L',i2+(div i1 5)):loci))

	|(c1=='X' && i1>=5 && c2=='L')
		= ('X',i1 `mod` 5):(collapse (('L',i2+(div i1 5)):loci))

	|(c1=='X' && i1>=5 && i1 `mod` 5 == 0)
		= (collapse (('L',(div i1 5)):(c2,i2):loci))  

	|(c1=='X' && i1>=5)
		= ('X',i1 `mod` 5):(collapse (('L',(div i1 5)):(c2,i2):loci))

	|(c1=='X' && i1<5)
		= ('X',i1):(collapse ((c2,i2):loci)) 

	|(c1=='L' && i1>=2 && c2=='C' && i1 `mod` 2 ==0 )
			= collapse (('C',i2+(div i1 2)):loci)  

	|(c1=='L' && i1>=2 && c2=='C')
		= ('L',1):(collapse (('C',i2+(div i1 2)):loci))

	|(c1=='L' && i1>=2 && i1 `mod` 2 ==0 )
		= collapse (('C',(div i1 2)):(c2,i2):loci)

	|(c1=='L' && i1>=2)
		= ('L',1):(collapse (('C',(div i1 2)):(c2,i2):loci))

	|(c1=='L') = ('L',i1):(collapse ((c2,i2):loci))

	|(c1=='C' && i1>=5 && c2=='D' && i1 `mod` 5 == 0 ) 
		= (collapse (('D',i2+(div i1 5)):loci))

	|(c1=='C' && i1>=5 && c2=='D')
		= ('C',i1 `mod` 5):(collapse (('D',i2+(div i1 5)):loci))

	|(c1=='C' && i1>=5 && i1 `mod` 5 == 0)
		= (collapse (('D',(div i1 5)):(c2,i2):loci)) 

	|(c1=='C' && i1>=5)
		= ('C',i1 `mod` 5):(collapse (('D',(div i1 5)):(c2,i2):loci))

	|(c1=='C' && i1<5)
		= ('C',i1):(collapse ((c2,i2):loci)) 

	|(c1=='D' && i1>=2 && c2=='M' && i1 `mod` 2 ==0 )
			= collapse (('M',i2+(div i1 2)):loci) 

	|(c1=='D' && i1>=2 && c2=='M')
		= ('D',1):(collapse (('M',i2+(div i1 2)):loci))

	|(c1=='D' && i1>=2 && i1 `mod` 2 ==0 )
		= collapse (('M',(div i1 2)):(c2,i2):loci)

	|(c1=='D' && i1>=2)
		= ('D',1):(collapse (('M',(div i1 2)):(c2,i2):loci))

	|(c1=='D') = ('D',i1):(collapse ((c2,i2):loci))

	|otherwise = (c1,i1):(c2,i2):loci

collapse ((c1,i1):loci)
	|(c1=='I' && i1>=5 && i1 `mod` 5 == 0)
		= (collapse (('V',(div i1 5)):loci))  

	|(c1=='I' && i1>=5)
		= ('I',i1 `mod` 5):(collapse (('V',(div i1 5)):loci))  

	|(c1=='I' )
		= [('I',i1)]

	|(c1=='V' && i1>=2 && i1 `mod` 2 ==0 )
		= collapse (('X',(div i1 2)):loci)

	|(c1=='V' && i1>=2)
		= ('V',1):(collapse (('X',(div i1 2)):loci))

	|(c1=='V') = [('V',i1)]

	|(c1=='X' && i1>=5 && i1 `mod` 5 == 0)
		= (collapse (('L',(div i1 5)):loci)) 

	|(c1=='X' && i1>=5)
		= ('X',i1 `mod` 5):(collapse (('L',(div i1 5)):loci))  

	|(c1=='X' && i1<5)
		= [('X',i1)]

	|(c1=='L' && i1>=2 && i1 `mod` 2 ==0 )
		= collapse (('C',(div i1 2)):loci)

	|(c1=='L' && i1>=2)
		= ('L',1):(collapse (('C',(div i1 2)):loci))

	|(c1=='L') = [('L',i1)]


	|(c1=='C' && i1>=5 && i1 `mod` 5 == 0)
		= (collapse (('D',(div i1 5)):loci)) 

	|(c1=='C' && i1>=5)
		= ('C',i1 `mod` 5):(collapse (('D',(div i1 5)):loci)) 

	|(c1=='C' && i1<5)
		= ('C',i1):(collapse (loci)) 

	|(c1=='D' && i1>=2 && i1 `mod` 2 ==0 )
		= collapse (('M',(div i1 2)):loci)

	|(c1=='D' && i1>=2)
		= ('D',1):(collapse (('M',(div i1 2)):loci))

	|(c1=='D') = [('D',i1)]

	|otherwise = [(c1,i1)]


collapse2 [] =[]
collapse2 ((c1,i1):(c2,i2):loci)
	|(c1=='I' && i1==4 && c2=='V')
		=('X',1):('I',1):(collapse2 loci)

	|(c1=='I' && i1==4)
		=('V',1):('I',1):(collapse2 ((c2,i2):loci))

	|(c1=='X' && i1==4 && c2=='L')
		=('C',1):('X',1):(collapse2 loci)

	|(c1=='X' && i1==4)
		=('L',1):('X',1):(collapse2 ((c2,i2):loci))

	|(c1=='C' && i1==4 && c2=='D')
		=('M',1):('C',1):(collapse2 loci)

	|(c1=='C' && i1==4)
		=('D',1):('C',1):(collapse2 ((c2,i2):loci))

	|otherwise = (c1,i1):(collapse2 ((c2,i2):loci))

collapse2 ((c1,i1):loci)
	|(c1=='I' && i1==4)
		=('V',1):('I',1):(collapse2 loci)

	|(c1=='X' && i1==4)
		=('L',1):('X',1):(collapse2 loci)

	|(c1=='C' && i1==4)
		=('D',1):('C',1):(collapse2 loci)

	|otherwise = (c1,i1):(collapse2 loci)

imroveScore str = (length str) - (foldr (\(c,i) n->i+n) 0 $collapse2$collapse$countMap str)

--totalscore los= foldr (\s n-> (imroveScore s)+n) 0 los  

totalscore los= sum$map imroveScore los  


--test = ["MMMMDCLXXII","MMDCCCLXXXIII","MMMDLXVIIII","MMMMDXCV","DCCCLXXII","MMCCCVI","MMMCDLXXXVII","MMMMCCXXI","MMMCCXX","MMMMDCCCLXXIII","MMMCCXXXVII","MMCCCLXXXXIX","MDCCCXXIIII","MMCXCVI","CCXCVIII","MMMCCCXXXII","MDCCXXX","MMMDCCCL","MMMMCCLXXXVI","MMDCCCXCVI","MMMDCII","MMMCCXII","MMMMDCCCCI","MMDCCCXCII","MDCXX","CMLXXXVII","MMMXXI","MMMMCCCXIV","MLXXII","MCCLXXVIIII","MMMMCCXXXXI","MMDCCCLXXII","MMMMXXXI","MMMDCCLXXX","MMDCCCLXXIX","MMMMLXXXV","MCXXI","MDCCCXXXVII","MMCCCLXVII","MCDXXXV","CCXXXIII","CMXX","MMMCLXIV","MCCCLXXXVI","DCCCXCVIII","MMMDCCCCXXXIV","CDXVIIII","MMCCXXXV","MDCCCXXXII","MMMMD","MMDCCLXIX","MMMMCCCLXXXXVI","MMDCCXLII","MMMDCCCVIIII","DCCLXXXIIII","MDCCCCXXXII","MMCXXVII","DCCCXXX","CCLXIX","MMMXI","MMMMCMLXXXXVIII","MMMMDLXXXVII","MMMMDCCCLX","MMCCLIV","CMIX","MMDCCCLXXXIIII","CLXXXII","MMCCCCXXXXV","MMMMDLXXXVIIII","MMMDCCCXXI","MMDCCCCLXXVI","MCCCCLXX","MMCDLVIIII","MMMDCCCLIX","MMMMCCCCXIX","MMMDCCCLXXV","XXXI","CDLXXXIII","MMMCXV","MMDCCLXIII","MMDXXX","MMMMCCCLVII","MMMDCI","MMMMCDLXXXIIII","MMMMCCCXVI","CCCLXXXVIII","MMMMCML","MMMMXXIV","MMMCCCCXXX","DCCX","MMMCCLX","MMDXXXIII","CCCLXIII","MMDCCXIII","MMMCCCXLIV","CLXXXXI","CXVI","MMMMCXXXIII","CLXX","DCCCXVIII","MLXVII","DLXXXX","MMDXXI","MMMMDLXXXXVIII","MXXII","LXI","DCCCCXLIII","MMMMDV","MMMMXXXIV","MDCCCLVIII","MMMCCLXXII","MMMMDCCXXXVI","MMMMLXXXIX","MDCCCLXXXI","MMMMDCCCXV","MMMMCCCCXI","MMMMCCCLIII","MDCCCLXXI","MMCCCCXI","MLXV","MMCDLXII","MMMMDXXXXII","MMMMDCCCXL","MMMMCMLVI","CCLXXXIV","MMMDCCLXXXVI","MMCLII","MMMCCCCXV","MMLXXXIII","MMMV","MMMV","DCCLXII","MMDCCCCXVI","MMDCXLVIII","CCLIIII","CCCXXV","MMDCCLXXXVIIII","MMMMDCLXXVIII","MMMMDCCCXCI","MMMMCCCXX","MMCCXLV","MMMDCCCLXIX","MMCCLXIIII","MMMDCCCXLIX","MMMMCCCLXIX","CMLXXXXI","MCMLXXXIX","MMCDLXI","MMDCLXXVIII","MMMMDCCLXI","MCDXXV","DL","CCCLXXII","MXVIIII","MCCCCLXVIII","CIII","MMMDCCLXXIIII","MMMDVIII","MMMMCCCLXXXXVII","MMDXXVII","MMDCCLXXXXV","MMMMCXLVI","MMMDCCLXXXII","MMMDXXXVI","MCXXII","CLI","DCLXXXIX","MMMCLI","MDCLXIII","MMMMDCCXCVII","MMCCCLXXXV","MMMDCXXVIII","MMMCDLX","MMMCMLII","MMMIV","MMMMDCCCLVIII","MMMDLXXXVIII","MCXXIV","MMMMLXXVI","CLXXIX","MMMCCCCXXVIIII","DCCLXXXV","MMMDCCCVI","LI","CLXXXVI","MMMMCCCLXXVI","MCCCLXVI","CCXXXIX","MMDXXXXI","MMDCCCXLI","DCCCLXXXVIII","MMMMDCCCIV","MDCCCCXV","MMCMVI","MMMMCMLXXXXV","MMDCCLVI","MMMMCCXLVIII","DCCCCIIII","MMCCCCIII","MMMDCCLXXXVIIII","MDCCCLXXXXV","DVII","MMMV","DCXXV","MMDCCCXCV","DCVIII","MMCDLXVI","MCXXVIII","MDCCXCVIII","MMDCLX","MMMDCCLXIV","MMCDLXXVII","MMDLXXXIIII","MMMMCCCXXII","MMMDCCCXLIIII","DCCCCLXVII","MMMCLXXXXIII","MCCXV","MMMMDCXI","MMMMDCLXXXXV","MMMCCCLII","MMCMIX","MMDCCXXV","MMDLXXXVI","MMMMDCXXVIIII","DCCCCXXXVIIII","MMCCXXXIIII","MMDCCLXXVIII","MDCCLXVIIII","MMCCLXXXV","MMMMDCCCLXXXVIII","MMCMXCI","MDXLII","MMMMDCCXIV","MMMMLI","DXXXXIII","MMDCCXI","MMMMCCLXXXIII","MMMDCCCLXXIII","MDCLVII","MMCD","MCCCXXVII","MMMMDCCIIII","MMMDCCXLVI","MMMCLXXXVII","MMMCCVIIII","MCCCCLXXIX","DL","DCCCLXXVI","MMDXCI","MMMMDCCCCXXXVI","MMCII","MMMDCCCXXXXV","MMMCDXLV","MMDCXXXXIV","MMD","MDCCCLXXXX","MMDCXLIII","MMCCXXXII","MMDCXXXXVIIII","DCCCLXXI","MDXCVIIII","MMMMCCLXXVIII","MDCLVIIII","MMMCCCLXXXIX","MDCLXXXV","MDLVIII","MMMMCCVII","MMMMDCXIV","MMMCCCLXIIII","MMIIII","MMMMCCCLXXIII","CCIII","MMMCCLV","MMMDXIII","MMMCCCXC","MMMDCCCXXI","MMMMCCCCXXXII","CCCLVI","MMMCCCLXXXVI","MXVIIII","MMMCCCCXIIII","CLXVII","MMMCCLXX","CCCCLXIV","MMXXXXII","MMMMCCLXXXX","MXL","CCXVI","CCCCLVIIII","MMCCCII","MCCCLVIII","MMMMCCCX","MCDLXXXXIV","MDCCCXIII","MMDCCCXL","MMMMCCCXXIII","DXXXIV","CVI","MMMMDCLXXX","DCCCVII","MMCMLXIIII","MMMDCCCXXXIII","DCCC","MDIII","MMCCCLXVI","MMMCCCCLXXI","MMDCCCCXVIII","CCXXXVII","CCCXXV","MDCCCXII","MMMCMV","MMMMCMXV","MMMMDCXCI","DXXI","MMCCXLVIIII","MMMMCMLII","MDLXXX","MMDCLXVI","CXXI","MMMDCCCLIIII","MMMCXXI","MCCIII","MMDCXXXXI","CCXCII","MMMMDXXXV","MMMCCCLXV","MMMMDLXV","MMMCCCCXXXII","MMMCCCVIII","DCCCCLXXXXII","MMCLXIV","MMMMCXI","MLXXXXVII","MMMCDXXXVIII","MDXXII","MLV","MMMMDLXVI","MMMCXII","XXXIII","MMMMDCCCXXVI","MMMLXVIIII","MMMLX","MMMCDLXVII","MDCCCLVII","MMCXXXVII","MDCCCCXXX","MMDCCCLXIII","MMMMDCXLIX","MMMMCMXLVIII","DCCCLXXVIIII","MDCCCLIII","MMMCMLXI","MMMMCCLXI","MMDCCCLIII","MMMDCCCVI","MMDXXXXIX","MMCLXXXXV","MMDXXX","MMMXIII","DCLXXIX","DCCLXII","MMMMDCCLXVIII","MDCCXXXXIII","CCXXXII","MMMMDCXXV","MMMCCCXXVIII","MDCVIII","MMMCLXXXXIIII","CLXXXI","MDCCCCXXXIII","MMMMDCXXX","MMMDCXXIV","MMMCCXXXVII","MCCCXXXXIIII","CXVIII","MMDCCCCIV","MMMMCDLXXV","MMMDLXIV","MDXCIII","MCCLXXXI","MMMDCCCXXIV","MCXLIII","MMMDCCCI","MCCLXXX","CCXV","MMDCCLXXI","MMDLXXXIII","MMMMDCXVII","MMMCMLXV","MCLXVIII","MMMMCCLXXVI","MMMDCCLXVIIII","MMMMDCCCIX","DLXXXXIX","DCCCXXII","MMMMIII","MMMMCCCLXXVI","DCCCXCIII","DXXXI","MXXXIIII","CCXII","MMMDCCLXXXIIII","MMMCXX","MMMCMXXVII","DCCCXXXX","MMCDXXXVIIII","MMMMDCCXVIII","LV","MMMDCCCCVI","MCCCII","MMCMLXVIIII","MDCCXI","MMMMDLXVII","MMCCCCLXI","MMDCCV","MMMCCCXXXIIII","MMMMDI","MMMDCCCXCV","MMDCCLXXXXI","MMMDXXVI","MMMDCCCLVI","MMDCXXX","MCCCVII","MMMMCCCLXII","MMMMXXV","MMCMXXV","MMLVI","MMDXXX","MMMMCVII","MDC","MCCIII","MMMMDCC","MMCCLXXV","MMDCCCXXXXVI","MMMMCCCLXV","CDXIIII","MLXIIII","CCV","MMMCMXXXI","CCCCLXVI","MDXXXII","MMMMCCCLVIII","MMV","MMMCLII","MCMLI","MMDCCXX","MMMMCCCCXXXVI","MCCLXXXI","MMMCMVI","DCCXXX","MMMMCCCLXV","DCCCXI","MMMMDCCCXIV","CCCXXI","MMDLXXV","CCCCLXXXX","MCCCLXXXXII","MMDCIX","DCCXLIIII","DXIV","MMMMCLII","CDLXI","MMMCXXVII","MMMMDCCCCLXIII","MMMDCLIIII","MCCCCXXXXII","MMCCCLX","CCCCLIII","MDCCLXXVI","MCMXXIII","MMMMDLXXVIII","MMDCCCCLX","MMMCCCLXXXX","MMMCDXXVI","MMMDLVIII","CCCLXI","MMMMDCXXII","MMDCCCXXI","MMDCCXIII","MMMMCLXXXVI","MDCCCCXXVI","MDV","MMDCCCCLXXVI","MMMMCCXXXVII","MMMDCCLXXVIIII","MMMCCCCLXVII","DCCXLI","MMCLXXXVIII","MCCXXXVI","MMDCXLVIII","MMMMCXXXII","MMMMDCCLXVI","MMMMCMLI","MMMMCLXV","MMMMDCCCXCIV","MCCLXXVII","LXXVIIII","DCCLII","MMMCCCXCVI","MMMCLV","MMDCCCXXXXVIII","DCCCXV","MXC","MMDCCLXXXXVII","MMMMCML","MMDCCCLXXVIII","DXXI","MCCCXLI","DCLXXXXI","MMCCCLXXXXVIII","MDCCCCLXXVIII","MMMMDXXV","MMMDCXXXVI","MMMCMXCVII","MMXVIIII","MMMDCCLXXIV","MMMCXXV","DXXXVIII","MMMMCLXVI","MDXII","MMCCCLXX","CCLXXI","DXIV","MMMCLIII","DLII","MMMCCCXLIX","MMCCCCXXVI","MMDCXLIII","MXXXXII","CCCLXXXV","MDCLXXVI","MDCXII","MMMCCCLXXXIII","MMDCCCCLXXXII","MMMMCCCLXXXV","MMDCXXI","DCCCXXX","MMMDCCCCLII","MMMDCCXXII","MMMMCDXCVIII","MMMCCLXVIIII","MMXXV","MMMMCDXIX","MMMMCCCX","MMMCCCCLXVI","MMMMDCLXXVIIII","MMMMDCXXXXIV","MMMCMXII","MMMMXXXIII","MMMMDLXXXII","DCCCLIV","MDXVIIII","MMMCLXXXXV","CCCCXX","MMDIX","MMCMLXXXVIII","DCCXLIII","DCCLX","D","MCCCVII","MMMMCCCLXXXIII","MDCCCLXXIIII","MMMDCCCCLXXXVII","MMMMCCCVII","MMMDCCLXXXXVI","CDXXXIV","MCCLXVIII","MMMMDLX","MMMMDXII","MMMMCCCCLIIII","MCMLXXXXIII","MMMMDCCCIII","MMDCLXXXIII","MDCCCXXXXIV","XXXXVII","MMMDCCCXXXII","MMMDCCCXLII","MCXXXV","MDCXXVIIII","MMMCXXXXIIII","MMMMCDXVII","MMMDXXIII","MMMMCCCCLXI","DCLXXXXVIIII","LXXXXI","CXXXIII","MCDX","MCCLVII","MDCXXXXII","MMMCXXIV","MMMMLXXXX","MMDCCCCXLV","MLXXX","MMDCCCCLX","MCDLIII","MMMCCCLXVII","MMMMCCCLXXIV","MMMDCVIII","DCCCCXXIII","MMXCI","MMDCCIV","MMMMDCCCXXXIV","CCCLXXI","MCCLXXXII","MCMIII","CCXXXI","DCCXXXVIII","MMMMDCCXLVIIII","MMMMCMXXXV","DCCCLXXV","DCCXCI","MMMMDVII","MMMMDCCCLXVIIII","CCCXCV","MMMMDCCXX","MCCCCII","MMMCCCXC","MMMCCCII","MMDCCLXXVII","MMDCLIIII","CCXLIII","MMMDCXVIII","MMMCCCIX","MCXV","MMCCXXV","MLXXIIII","MDCCXXVI","MMMCCCXX","MMDLXX","MMCCCCVI","MMDCCXX","MMMMDCCCCXCV","MDCCCXXXII","MMMMDCCCCXXXX","XCIV","MMCCCCLX","MMXVII","MLXXI","MMMDXXVIII","MDCCCCII","MMMCMLVII","MMCLXXXXVIII","MDCCCCLV","MCCCCLXXIIII","MCCCLII","MCDXLVI","MMMMDXVIII","DCCLXXXIX","MMMDCCLXIV","MDCCCCXLIII","CLXXXXV","MMMMCCXXXVI","MMMDCCCXXI","MMMMCDLXXVII","MCDLIII","MMCCXLVI","DCCCLV","MCDLXX","DCLXXVIII","MMDCXXXIX","MMMMDCLX","MMDCCLI","MMCXXXV","MMMCCXII","MMMMCMLXII","MMMMCCV","MCCCCLXIX","MMMMCCIII","CLXVII","MCCCLXXXXIIII","MMMMDCVIII","MMDCCCLXI","MMLXXIX","CMLXIX","MMDCCCXLVIIII","DCLXII","MMMCCCXLVII","MDCCCXXXV","MMMMDCCXCVI","DCXXX","XXVI","MMLXIX","MMCXI","DCXXXVII","MMMMCCCXXXXVIII","MMMMDCLXI"]

--test1 = imroveScore "MMMMDCLXXII"
main :: IO()
main = do
		--contents :: [String]
        contents <- fmap lines $ readFile "p089_roman.txt"
        let ans = (totalscore contents)
        print ans

--main = do
--	print test1
