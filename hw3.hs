myremoveduplicates::(Eq mylist)=>[mylist]->[mylist]
myremoveduplicates mylist
  |null mylist = []
  |(head mylist) `elem` (tail mylist) = myremoveduplicates (tail mylist) 
  |otherwise = (head mylist):myremoveduplicates (tail mylist)

myremoveduplicates_pm::(Eq mylist)=>[mylist]->[mylist]
myremoveduplicates_pm []= []
myremoveduplicates_pm [x] = [x]
myremoveduplicates_pm(x:xs) | x `elem` xs = myremoveduplicates_pm xs
                            |otherwise = x : myremoveduplicates_pm xs

myintersection::(Eq mylist)=>[mylist]->[mylist]->[mylist]
myintersection list1 list2
  | null list2 = []
  | null list1 = []
  | (head list1) `elem` list2 = (head list1):myintersection (tail list1) list2
  | otherwise = myintersection (tail list1) list2

myintersection_pm::(Eq mylist)=>[mylist]->[mylist]->[mylist]
myintersection_pm[] list2= []
myintersection_pm(x:xs) list2 | x `elem` list2 = x:myintersection_pm xs list2
                            |otherwise = myintersection_pm xs list2

mylast::(Eq mylist)=>[mylist]->[mylist]
mylast mylist
  |null mylist = []
  |null (tail mylist) = (head mylist):mylast (tail mylist)
  |otherwise = mylast (tail mylist)

mylast_pm::(Eq mylist)=>[mylist]->[mylist]
mylast_pm []=[]
mylast_pm (x:xs) |null xs = x:mylast_pm xs
                       |otherwise = mylast_pm xs --works but the [] or "" doesn't

myreverse :: [a] -> [a]                      
myreverse l
  | null l = []
  | otherwise = _myreverse l []
    where
    _myreverse :: [a] -> [a] -> [a]
    _myreverse l1 l2
      |null l1 = l2
      |otherwise = _myreverse (tail l1) ((head l1):l2)
                       
myreverse_pm :: [a] -> [a]
myreverse_pm l = _myreverse_pm l []
    where
        _myreverse_pm :: [a] -> [a] -> [a]
        _myreverse_pm [] l = l
        _myreverse_pm (x:xs) l = _myreverse_pm xs (x:l) 


myreplaceall ::(Eq a) => a -> a -> [a] -> [a]          
myreplaceall a b mylist
  |null mylist = []
  |b == (head mylist) = a:myreplaceall a b (tail mylist)
  |otherwise = (head mylist):myreplaceall a b (tail mylist)
        
myreplaceall_pm ::(Eq a) => a -> a -> [a] -> [a]  
myreplaceall_pm a b []=[]
myreplaceall_pm a b (x:xs) |b == x = a:myreplaceall_pm a b xs
                            |otherwise = x:myreplaceall_pm a b xs
                            
main = do
  print(myremoveduplicates "abaaabbbababbab")
  print(myremoveduplicates_pm "abaaabbbababbab")
  print(myintersection [1,2,3] [1,2,3])
  print(myintersection_pm [1,2,3] [1,2,3])
  print(mylast [1,2,3,4])
  print(mylast_pm [1,2,3,4])
  print(myreverse "abc")
  print(myreverse_pm "abc")
  print(myreplaceall 'a' 'x' "xbxbxbxbxbxb")
  print(myreplaceall 'a' 'x' "xbxbxbxbxbxb")                            
                            


