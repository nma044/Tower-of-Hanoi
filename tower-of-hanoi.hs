import Data.Char
import Data.List
import Data.Maybe
import System.Process

--Printer en linje i et tårn
putLine :: Int -> Int -> IO ()
putLine 0 n = putEmpty n
putLine m n = putDisc m n

--Printer linje med tom stolpe hvis det ikke er disker på tårnet der
putEmpty :: Int -> IO ()
putEmpty 0 = putStrLn ""
putEmpty n = do
            putStr (replicate (n-1) ' ')
            putStr "| "
            putStr (replicate (n-1) ' ')

--Printer disker med disc størrelse og size total størrelse
putDisc :: Int -> Int -> IO ()
putDisc n 0 = putStrLn ""
putDisc disc size   | disc > size = putStrLn "Invalid input"
                    | otherwise = do
                let spaces = size - disc
                putStr (replicate spaces ' ')
                putStr (concat $ replicate disc "# ") 
                putStr (replicate spaces ' ')


type Tower = [Int]

--Printer tre tårn l(left) c(center) r(right) på linje
printTowers :: Tower -> Tower -> Tower -> IO ()
printTowers [] _ _ = putStrLn ""
printTowers l c r = printTowersHelp l c r (length l)

printTowersHelp :: Tower -> Tower -> Tower -> Int -> IO ()
printTowersHelp [] _ _ n = putStrLn ""
printTowersHelp _ [] _ n = putStrLn ""
printTowersHelp _ _ [] n = putStrLn ""
printTowersHelp (l:ls) (c:cs) (r:rs) n = do 
                                        putLine l n
                                        putLine c n
                                        putLine r n
                                        putStrLn ""
                                        printTowersHelp ls cs rs n

--Sjekker om spillet er ferdig
finished :: Tower -> Int -> Bool
finished tow num    | tow == [0..(num-1)] = True
                    | otherwise = False

--Starter nytt spill med n ringer
startGame :: Int -> IO ()
startGame 0 = putStrLn "invalid nr."
startGame n = do
            let left = [0..n]
            let center = replicate (n+1) 0
            let right = replicate (n+1) 0
            play [left] [center] [right] 0

--Får inn command fra brukeren
getCommand :: String -> IO (Char, Int)
getCommand prompt = do
                putStr prompt
                com <- getLine
                if (null com) then getCommand prompt
                else do
                let fir = head com
                if all isDigit (drop 2 com) && not (null (drop 2 com)) then do 
                                        let snd = read (drop 2 com)
                                        return (fir, snd)
                else return (fir, 0)

--Angrer spillet b antall ganger
goBack:: Int -> [Tower] -> [Tower] -> [Tower] -> Int -> IO ()
goBack b l c r n  = do 
                    let nl = back l b 
                    let nc = back c b
                    let nr = back r b
                    play nl nc nr (n-b)

--Går tilbake i listen med Tower for å gå n antall ganger tilbake i spillet
back :: [Tower] -> Int -> [Tower]
back x 0 = x
back (x:xs) n = back xs (n-1)

--Sjekker om gyldig move
validMove :: [Tower] -> Int -> Int -> Bool
validMove tow f t   | null (filter (/=0) (tow !! (f-1))) = False
                    | null (filter (/=0) (tow !! (t-1))) = True
                    | (foldr1 min (filter (/=0) (tow !! (f-1)))) < (foldr1 min (filter (/=0) (tow !! (t-1)))) = True
                    | otherwise = False

--Endrer element i index id til new.
changeTower :: Tower -> Int -> Int -> Tower
changeTower tow id new = let (x,_:ys) = splitAt id tow in x ++ [new] ++ ys

--Passer på at tårnene er gitt i riktig rekkefølge (l,c,r)                    
orderTower:: (Tower,Tower,Tower) -> (Tower,Tower,Tower)
orderTower (x,y,z) = let [(l:ls),(c:cs),(r:rs)] = sort [x,y,z] in (ls, cs, rs)

--Gjør et trekk, tow er liste med 3 towers, returnerer tuppel med 3 tårn (l,c,r)
move :: [Tower] -> Int -> Int -> (Tower,Tower,Tower)
move tow f t = do
                let ukjent = head (filter (/=t) (filter (/=f) [1..3]))
                let ft = (tow!!(f-1))
                let tt = (tow!!(t-1))
                let ut = (tow!!(ukjent-1))
                let fmin = (foldr1 min (filter (/=0) (ft)))
                let tmin = (foldr1 min (filter (/=0) (tt)))
                let nf =(changeTower (ft) (fromJust $ elemIndex fmin (ft)) 0)
                if (null (filter (/=0) (tt))) then do
                    let nt = (changeTower (tt) ((length (tt))-1) fmin)
                    orderTower (([f]++nf),([t]++nt),([ukjent]++ut))
                else do 
                    let nt = changeTower (tt) ((fromJust $ elemIndex tmin (tt))-1) fmin 
                    orderTower (([f]++nf),([t]++nt),([ukjent]++ut))

--Sjekker at en command er gyldig
isValid :: (Char, Int) -> Bool
isValid (com,num)   | com == 'q' = True
                    | com == 'b' && (20 >= num) && (num > 0) = True
                    | com == 'z' = True
                    | isDigit com && (0<num && num<4) && (digitToInt com > 0 && digitToInt com < 4) = True
                    | otherwise = False

--Selve spillet, spilles til spiller gir command 'q' eller vinner spillet.
play :: [Tower] -> [Tower] -> [Tower] -> Int -> IO ()
play (l:ls) (c:cs) (r:rs) n = do
                        system "clear" --Klarerer skjermen for hver tur, så tårn og command blir samme sted
                        printTowers l c r
                        putStr "Number of moves: "
                        putStrLn (show n)
                        if finished r (length r) then putStrLn "You win!"
                        else do
                        (com,num) <- getCommand "Enter command: "
                        if isValid (com,num) then do
                                        if com == 'b' then startGame num
                                        else if com == 'q' then putStrLn "Game closed"
                                        else if com == 'z' && num > 0 then do
                                                            if num >= n then startGame ((length l)-1)
                                                            else goBack num (l:ls) (c:cs) (r:rs) n
                                        else if (isDigit com) && (validMove [l,c,r] (digitToInt com) num) then do --Sjekker at gyldig move er gitt
                                                            let (nl,nc,nr) = move [l,c,r] (digitToInt com) num
                                                            play (nl:(l:ls)) (nc:(c:cs)) (nr:(r:rs)) (n+1)
                                        else play (l:ls) (c:cs) (r:rs) n
                        else play (l:ls) (c:cs) (r:rs) n --Prøver på nytt om det ikke er gitt gyldig command

--Spør spiller om å starte spillet
main :: IO ()
main = do
    (com,num) <- getCommand "Star a new game with: b <Nr. of rings> (max 20 rings), quit game with: q: "
    if not (isValid (com,num)) then do
                            putStrLn "Invalid input"
                            main
    else if com == 'q' then putStrLn "Game closed"
    else if com == 'b' then startGame num --starter spill med gitt antall ringer
    else do                                                    
    putStrLn "Invalid input"
    main

