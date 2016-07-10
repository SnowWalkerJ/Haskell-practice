module Main where
    import Prelude hiding (Left, Right)
    import Control.Monad
    import Control.Monad.ST
    import Data.STRef
    import System.Random
    import System.IO

    data Move = Left | Right | Up | Down
    type Grid = [[Int]]
    type Coordinate = (Int, Int)
    
    map_size = 5
    
    initialGame = replicate map_size $replicate map_size 0 :: Grid
        
    findSpaces::Grid->[Coordinate]
    findSpaces grid = filter (uncurry (\x y->grid!!x!!y==0)) [(x,y)|x<-[0..map_size-1],y<-[0..map_size-1]]
    
    addTile::Grid->StdGen->Grid
    addTile grid seed = 
        let spaces = findSpaces grid
            position = choose spaces
            val = choose [1,1,1,1,1,2]
            choose x = 
                let n = length x
                    (s, newSeed) = randomR (0, n-1) seed::(Int, StdGen)
                in x!!s
        in setValue grid position val
    
    setValue::Grid->Coordinate->Int->Grid
    setValue grid (x,y) val = 
        [[if xx==x && yy==y then val else grid!!xx!!yy|yy<-[0..map_size-1]]|xx<-[0..map_size-1]]
        
    combine::[Int]->[Int]
    combine [] = []
    combine [h] = [h]
    combine (h1:h2:xs) = if h1 == h2 && h1 /= 0 then h1+h2:combine xs ++ [0] else h1:combine (h2:xs)

    topping::[Int]->[Int]
    topping [] = []
    topping (0:xs) = topping xs ++ [0]
    topping (h:xs) = h:topping xs
    
    transpose::Grid->Grid
    transpose grid = [[grid!!y!!x | y<-[0..map_size-1]]|x<-[0..map_size-1]]

    move::Move->Grid->Grid
    move Left = map (combine.topping)
    move Right = map reverse . move Left . map reverse
    move Up = transpose . move Left . transpose
    move Down = transpose . move Right . transpose

    printGrid::Grid->IO()
    printGrid grid = do
        putStr "\ESC[2J\ESC[2J\n"
        forM_ grid print 
    
    check2048 ::Grid->Bool
    check2048 x = length (filter (==2048) $ concat x) > 0
    
    loopGame::StdGen->Grid->Bool->IO()
    loopGame seed grid addNew = do
        grid' <- return $ if addNew then addTile grid seed else grid
        printGrid grid'
        c <- getChar
        direction <- return $ case c of
                'w' -> Up
                's' -> Down
                'a' -> Left
                'd' -> Right

        new_grid <- return $ move direction grid'
        if check2048 new_grid 
        then print "You win"
        else loopGame seed new_grid (new_grid /= grid')
        
    main::IO()
    main = do
        hSetBuffering stdin NoBuffering
        seed <- newStdGen
        loopGame seed initialGame True
            
        
    
