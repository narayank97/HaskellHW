reduplicates::(Eq mylist)=>[mylist]->[mylist]
reduplicates[]= []
reduplicates [x] = [x]
reduplicates(myhead:mytail) | myhead `elem` mytail = reduplicates mytail
                            |otherwise = myhead : reduplicates mytail
