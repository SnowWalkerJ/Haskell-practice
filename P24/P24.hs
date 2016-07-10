module P24 where
    import Control.Monad
    import Data.Function
    data Exp = Val Double
             | Add Exp Exp
             | Sub Exp Exp
             | Mul Exp Exp
             | Div Exp Exp
    
    instance Eq Exp where
        (Val a) == (Val b) = a == b
        (Add a1 b1) == (Add a2 b2) = ((a1 == a2) && (b1 == b2)) || ((a1 == b2) && (b1 == a2))
        (Mul a1 b1) == (Mul a2 b2) = ((a1 == a2) && (b1 == b2)) || ((a1 == b2) && (b1 == a2))
        (Sub a1 b1) == (Sub a2 b2) = (a1 == a2) && (b1 == b2)
        (Div a1 b1) == (Sub a2 b2) = (a1 == a2) && (b1 == b2)    
        _ == _ = False
    
    show_::Exp->Int->String
    show_ (Val n) _ = show n
    show_ (Mul a b) 3 = "(" ++ (show_ a 2) ++ "*" ++ (show_ b 2) ++ ")"
    show_ (Mul a b) _ = (show_ a 2) ++ "*" ++ (show_ b 2)
    show_ (Div a b) 3 = "(" ++ (show_ a 2) ++ "/" ++ (show_ b 3) ++ ")"
    show_ (Div a b) _ = (show_ a 2) ++ "/" ++ (show_ b 3)
    show_ (Add a b) n | n >= 1    = "(" ++ (show_ a 0) ++ "+" ++ (show_ b 0) ++ ")"
                      | otherwise = (show_ a 0) ++ "+" ++ (show_ b 0)
    show_ (Sub a b) n | n >= 1    = "(" ++ (show_ a 0) ++ "-" ++ (show_ b 1) ++ ")"
                      | otherwise = (show_ a 0) ++ "-" ++ (show_ b 1)
    instance Show Exp where
        show = flip show_ 0
        

    eval::Exp->Double
    eval (Val a) = a
    eval (Add op1 op2) = on (+) eval op1 op2
    eval (Sub op1 op2) = on (-) eval op1 op2
    eval (Mul op1 op2) = on (*) eval op1 op2
    eval (Div op1 op2) = on (/) eval op1 op2
    
    generateExp::[Double]->[Exp]
    generateExp numbers = concat $ reduceExps [Val <$> numbers]
    
    reduceExps::[[Exp]]->[[Exp]]
    reduceExps []    = []
    reduceExps [[exp]] = [[exp]]
    reduceExps expss = do
        exps <- expss
        let n = length exps
        if n > 1 then reduceExps [f (exps!!n1) (exps!!n2):[exps!!k|k<-[0..n-1], k/=n1, k/=n2] |
                                  n1 <- [0..n-1],
                                  n2 <- [0..n-1],
                                  f <- if (eval$exps!!n1)<(eval$exps!!n2) then [Add, Mul, Div] else [Add, Sub, Mul, Div],
                                  n1 /= n2] 
                 else return exps
         
    twentyfour::[Double]->[Exp]
    twentyfour = dropReplicate . filter ((==24).eval) . generateExp 
    
    dropReplicate::[Exp]->[Exp]
    dropReplicate []     = []
    dropReplicate (h:ls) = h:dropReplicate [x|x<-ls, x/=h]
    
    main::IO()
    main = mapM_ print $ twentyfour [3, 9, 5, 6]
    