module Eightqueens where
    valid::[Int]->[[Int]]
    valid lst = [h:lst | h<-[1..8], not $ h `elem` lst, not $ (h-(length lst)) `elem` [(lst!!i) + i + 1 - (length lst) | i<-[0..(length lst -1)]], not $ (h+(length lst)) `elem` [(lst!!i) - i - 1 + (length lst) | i<-[0..(length lst -1)]]]   

    queens''::[[Int]]->Int->[[Int]]
    queens'' lst 0 = lst
    queens'' lst n = do
        result <- map valid lst
        queens'' result (n-1)   

    queens::[[Int]]
    queens = queens'' [[n]|n<-[1..8]] 7
