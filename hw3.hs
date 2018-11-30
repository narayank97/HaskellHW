reduplicates::(Eq mylist)=>[mylist]->[mylist]
reduplicates[]= []
reduplicates [x] = [x]
reduplicates(myhead:mytail) | myhead `elem` mytail = reduplicates mytail
                            |otherwise = myhead : reduplicates mytail
                            
intersection::(Eq mylist)=>[mylist]->[mylist]->[mylist]
intersection[] list2= []
intersection(myhead:mytail) list2 | myhead `elem` list2 = myhead:intersection mytail list2
                            |otherwise = intersection mytail list2

