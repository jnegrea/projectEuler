import Data.List

list1=[(a,b)|b<-[4..12000],a<-(filter (\x->((gcd b x) ==1)) [(1+(div b 3))..(div b 2)]) ]

main = do
  print $length $list1