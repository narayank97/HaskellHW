reduplicates::(Eq mylist)=>[mylist]->[mylist]
reduplicates[]= []
reduplicates [x] = [x]
reduplicates(myhead:mytail) | myhead `elem` mytail = reduplicates mytail
                            |otherwise = myhead : reduplicates mytail
                            
intersection::(Eq mylist)=>[mylist]->[mylist]->[mylist]
intersection[] list2= []
intersection(myhead:mytail) list2 | myhead `elem` list2 = myhead:intersection mytail list2
                            |otherwise = intersection mytail list2
                            
mylast::(Eq mylist)=>[mylist]->[mylist]
mylast []=[]
mylast (myhead:mytail) |null mytail = myhead:mylast mytail
                       |otherwise = mylast mytail --works but the [] or "" doesn't
                       
myreverse :: [a] -> [a]
myreverse l = _myreverse l []
    where
        _myreverse :: [a] -> [a] -> [a]
        _myreverse [] l = l
        _myreverse (x:xs) l = _myreverse xs (x:l) 
        
replace ::(Eq a) => a -> a -> [a] -> [a]  
replace a b []=[]
replace a b (myhead:mytail) |b == myhead = a:replace a b mytail
                            |otherwise = myhead:replace a b mytail

