-- reduplicates::(Eq mylist)=>[mylist]->[mylist]
-- reduplicates[]= []
-- reduplicates [x] = [x]
-- reduplicates(myhead:mytail) | myhead `elem` mytail = reduplicates mytail
--                             |otherwise = myhead : reduplicates mytail
                            
-- intersection::(Eq mylist)=>[mylist]->[mylist]->[mylist]
-- intersection[] list2= []
-- intersection(myhead:mytail) list2 | myhead `elem` list2 = myhead:intersection mytail list2
--                             |otherwise = intersection mytail list2
                            
-- mylast::(Eq mylist)=>[mylist]->[mylist]
-- mylast []=[]
-- mylast (myhead:mytail) |null mytail = myhead:mylast mytail
--                        |otherwise = mylast mytail --works but the [] or "" doesn't
                       
-- myreverse :: [a] -> [a]
-- myreverse l = _myreverse l []
--     where
--         _myreverse :: [a] -> [a] -> [a]
--         _myreverse [] l = l
--         _myreverse (x:xs) l = _myreverse xs (x:l) 
        
-- replace ::(Eq a) => a -> a -> [a] -> [a]  
-- replace a b []=[]
-- replace a b (myhead:mytail) |b == myhead = a:replace a b mytail
--                             |otherwise = myhead:replace a b mytail

-- myremoveduplicates_pm::(Eq mylist)=>[mylist]->[mylist]
-- myremoveduplicates_pm []= []
-- myremoveduplicates_pm [x] = [x]
-- myremoveduplicates_pm(myhead:mytail) | myhead `elem` mytail = myremoveduplicates_pm mytail
--                             |otherwise = myhead : myremoveduplicates_pm mytail
                            
-- myintersection_pm::(Eq mylist)=>[mylist]->[mylist]->[mylist]
-- myintersection_pm[] list2= []
-- myintersection_pm(myhead:mytail) list2 | myhead `elem` list2 = myhead:myintersection_pm mytail list2
--                             |otherwise = myintersection_pm mytail list2
                            
-- mylast_pm::(Eq mylist)=>[mylist]->[mylist]
-- mylast_pm []=[]
-- mylast_pm (myhead:mytail) |null mytail = myhead:mylast_pm mytail
--                        |otherwise = mylast_pm mytail --works but the [] or "" doesn't
                       
-- myreverse_pm :: [a] -> [a]
-- myreverse_pm l = _myreverse_pm l []
--     where
--         _myreverse_pm :: [a] -> [a] -> [a]
--         _myreverse_pm [] l = l
--         _myreverse_pm (x:xs) l = _myreverse_pm xs (x:l) 
        
-- myreplaceall_pm ::(Eq a) => a -> a -> [a] -> [a]  
-- myreplaceall_pm a b []=[]
-- myreplaceall_pm a b (myhead:mytail) |b == myhead = a:myreplaceall_pm a b mytail
--                             |otherwise = myhead:myreplaceall_pm a b mytail

myremoveduplicates mylist
  |null mylist = []
  |(head mylist) `elem` (tail mylist) = myremoveduplicates (tail mylist) 
  |otherwise = (head mylist):myremoveduplicates (tail mylist)

myremoveduplicates_pm::(Eq mylist)=>[mylist]->[mylist]
myremoveduplicates_pm []= []
myremoveduplicates_pm [x] = [x]
myremoveduplicates_pm(x:xs) | x `elem` xs = myremoveduplicates_pm xs
                            |otherwise = x : myremoveduplicates_pm xs

myintersection list1 list2
  | null list2 = []
  | null list1 = []
  | (head list1) `elem` list2 = (head list1):myintersection (tail list1) list2
  | otherwise = myintersection (tail list1) list2

myintersection_pm::(Eq mylist)=>[mylist]->[mylist]->[mylist]
myintersection_pm[] list2= []
myintersection_pm(x:xs) list2 | x `elem` list2 = x:myintersection_pm xs list2
                            |otherwise = myintersection_pm xs list2

mylast mylist
  |null mylist = []
  |null (tail mylist) = (head mylist):mylast (tail mylist)
  |otherwise = mylast (tail mylist)

mylast_pm::(Eq mylist)=>[mylist]->[mylist]
mylast_pm []=[]
mylast_pm (x:xs) |null xs = x:mylast_pm xs
                       |otherwise = mylast_pm xs --works but the [] or "" doesn't
                       
myreverse_pm :: [a] -> [a]
myreverse_pm l = _myreverse_pm l []
    where
        _myreverse_pm :: [a] -> [a] -> [a]
        _myreverse_pm [] l = l
        _myreverse_pm (x:xs) l = _myreverse_pm xs (x:l) 
        
myreplaceall a b mylist
  |null mylist = []
  |b == (head mylist) = a:myreplaceall a b (tail mylist)
  |otherwise = (head mylist):myreplaceall a b (tail mylist)
        
myreplaceall_pm ::(Eq a) => a -> a -> [a] -> [a]  
myreplaceall_pm a b []=[]
myreplaceall_pm a b (x:xs) |b == x = a:myreplaceall_pm a b xs
                            |otherwise = x:myreplaceall_pm a b xs

