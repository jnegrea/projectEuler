
intToList :: Integer -> [Int]
intToList =map (read . (:[])) . show

x1=0:(concat $ map intToList [1..])

x=product [x1!!n | m<-[0..6], n<-[10^m]]