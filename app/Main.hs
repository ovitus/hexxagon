module Main where

import Control.Monad.State
import System.Console.ANSI
import System.Console.Haskeline
import Data.Maybe
import Data.List (nub)

type Hex = (Char, [(String, Char)])

defBoard :: Hex
defBoard = 
  ( 'b'
  , [ ("dg",'w')
    , ("ds",'b')
    , ("hk",'x')
    , ("ho",'x')
    , ("il",'x')
    , ("in",'x')
    , ("ja",'b')
    , ("jy",'w')
    , ("kl",'x')
    , ("kn",'x')
    , ("lk",'x')
    , ("lo",'x')
    , ("pg",'w')
    , ("ps",'b')
    ]
  )

testBoard :: Hex
testBoard = 
  ( 'w'
  , [("jy",'x'),("ix",'x'),("kx",'x'),("hw",'x'),("jw",'x'),("lw",'x'),("gv",'x'),("iv",'x'),("kv",'x'),("mv",'x'),("fu",'x'),("hu",'x'),("ju",'x'),("lu",'x'),("nu",'x'),("et",'x'),("gt",'x'),("it",'x'),("kt",'x'),("mt",'x'),("ot",'x'),("ds",'x'),("fs",'x'),("hs",'x'),("js",'x'),("ls",'x'),("ns",'x'),("ps",'x'),("cr",'x'),("er",'x'),("gr",'x'),("ir",'x'),("kr",'x'),("mr",'x'),("or",'x'),("qr",'x'),("bq",'x'),("dq",'x'),("fq",'x'),("hq",'x'),("jq",'x'),("lq",'x'),("nq",'x'),("pq",'x'),("rq",'x'),("ap",'x'),("cp",'x'),("ep",'x'),("gp",'x'),("ip",'x'),("kp",'x'),("mp",'x'),("op",'x'),("qp",'x'),("sp",'x'),("bo",'x'),("do",'x'),("fo",'x'),("ho",'x'),("jo",'x'),("lo",'x'),("no",'x'),("po",'x'),("ro",'x'),("an",'x'),("cn",'x'),("en",'x'),("gn",'x'),("in",'x'),("kn",'x'),("mn",'x'),("on",'x'),("qn",'x'),("sn",'x'),("bm",'x'),("dm",'x'),("fm",'x'),("hm",'x'),("jm",'x'),("lm",'x'),("nm",'x'),("pm",'x'),("rm",'x'),("al",'x'),("cl",'x'),("el",'x'),("gl",'x'),("il",'x'),("kl",'x'),("ml",'x'),("ol",'x'),("ql",'x'),("sl",'x'),("bk",'x'),("dk",'x'),("fk",'x'),("hk",'x'),("jk",'w'),("lk",'x'),("nk",'x'),("pk",'x'),("rk",'x'),("aj",'x'),("cj",'x'),("ej",'x'),("gj",'x'),("ij",'x'),("kj",'x'),("mj",'x'),("oj",'x'),("qj",'x'),("sj",'x'),("bi",'x'),("di",'x'),("fi",'x'),("hi",'x'),("li",'x'),("ni",'x'),("pi",'x'),("ri",'x'),("ch",'x'),("eh",'x'),("gh",'x'),("ih",'x'),("kh",'x'),("mh",'x'),("oh",'x'),("qh",'x'),("dg",'x'),("fg",'x'),("hg",'x'),("lg",'x'),("ng",'x'),("pg",'x'),("ef",'x'),("gf",'x'),("if",'x'),("kf",'x'),("mf",'x'),("of",'x'),("fe",'x'),("he",'x'),("le",'x'),("ne",'x'),("gd",'x'),("id",'x'),("kd",'x'),("md",'x'),("hc",'x'),("jc",'b'),("lc",'x'),("ib",'x'),("kb",'x'),("ja",'x')]
  )

load [] (c1,s1) = (c1,s1)
load ((c0,s0):rest) (c1,s1)
  | c0 == c1 && s0 == 'b' = (c0,"_◯")
  | c0 == c1 && s0 == 'w' = (c0,"_●")
  | c0 == c1 && s0 == 'x' = ("  ","_×")
  | c0 == c1 && s0 == 'a' = ("  ","_◯")
  | c0 == c1 && s0 == 'v' = ("  ","_●")
  | otherwise = load rest (c1,s1)

bSrc (c,s) = if s == "_◯" then (c,s) else ("  ",s)
wSrc (c,s) = if s == "_●" then (c,s) else ("  ",s)
showDes src lst (c,s) = if s == "__" && elem c (ns src lst) then (c,s) else ("  ",s)
clean (c,s) = ("  ",s)

noMoveB t (x,y) = case y of
  'b' -> if foldr (\x y -> elem x (fst <$> t) && y) True $ (filter (\x -> not $ elem x outerLimit) $ ns x (jLst <> cLst)) then (x,'a') else (x,y)
  _   -> (x,y)

noMoveW t (x,y) = case y of
  'w' -> if foldr (\x y -> elem x (fst <$> t) && y) True $ (filter (\x -> not $ elem x outerLimit) $ ns x (jLst <> cLst)) then (x,'v') else (x,y)
  _   -> (x,y)

ns :: String -> [(Int, Int)] -> [String]
ns c lst = 
  filter (all $ flip elem ['a'..'z']) $ 
    (\(x,y) -> (toEnum x : toEnum y : [])) <$> 
      (\(x0,y0) (x1,y1) -> (x0 + x1, y0 + y1)) 
      (fromEnum $ head c, fromEnum $ last c) <$> lst

test = 
  [ ("dg",'w')
  , ("ds",'b')
  , ("hk",'x')
  , ("ho",'x')
  , ("il",'x')
  , ("in",'x')
  , ("ja",'b')
  , ("jy",'w')
  , ("kl",'x')
  , ("kn",'x')
  , ("lk",'x')
  , ("lo",'x')
  , ("pg",'w')
  , ("ps",'b')
  ]

jLst =
  [ ( 0, 4)
  , ( 0,-4)
  , ( 1, 3)
  , ( 1,-3)
  , ( 2, 0)
  , ( 2, 2)
  , ( 2,-2)
  , (-1, 3) 
  , (-1,-3)
  , (-2, 0)
  , (-2, 2)
  , (-2,-2)
  ]

cLst =
  [ ( 0, 2)
  , ( 0,-2)
  , ( 1, 1)
  , ( 1,-1)
  , (-1, 1)
  , (-1,-1)
  ]

outerLimit = filter (\x -> not $ elem x (fst <$> hexArgs)) <$> concatMap (zipWith (\x y -> x:y:[]) ['a'..'u'] . repeat) $ reverse ['a'..'z']

game :: InputT (StateT Hex IO) ()
game = do
  (turn, plot) <- lift get
  case turn of
    'b' -> 
      do let b p = length $ filter (\(_,y) -> y == 'b') p
             w p = length $ filter (\(_,y) -> y == 'w') p
             noMove = all (== True) $ foldr (\x y -> elem x (fst <$> plot) && y) True <$> filter (\x -> not $ elem x outerLimit) <$> flip ns (jLst <> cLst) <$> fst <$> filter (\(_,y) -> y == 'b') plot
             count = case (b plot == w plot, b plot < w plot) of
                       (True,_) -> do outputStrLn "   TIE!"; return ()
                       (_,True) -> do outputStrLn " ● WINNER!"; return ()
                       _        -> do outputStrLn " ◯ WINNER!"; return ()
             fill' p c = concat $ filter (\x -> not $ elem x (fst <$> p)) <$> filter (\x -> not $ elem x outerLimit) <$> flip ns cLst <$> fst <$> filter (\(_,y) -> y == c) p
             fill p c = case fill' p c of
                    [] -> count
                    _  -> do lift $ put (c, nub $ ((\x -> (x,c)) <$> fill' p c) <> p)
                             (_, p') <- lift get
                             _ <- getInputLine $ if c == 'w' then " ● " else " ◯ " 
                             outputStrLn . hex $ clean <$> load p' <$> hexArgs
                             outputStrLn $ replicate 52 ' ' <> " ◯ " <> (show $ b p')
                             outputStrLn $ replicate 52 ' ' <> " ● " <> (show $ w p')
                             outputStrLn $ show p'
                             fill p' c
         outputStrLn . hex $ clean <$> load plot <$> hexArgs
         outputStrLn $ replicate 52 ' ' <> " ◯ " <> (show $ b plot)
         outputStrLn $ replicate 52 ' ' <> " ● " <> (show $ w plot)
         case (length plot, b plot, w plot, noMove) of
           (157,_,_,_) -> count
           (_  ,0,_,_) -> fill plot 'w'
           (_  ,_,0,_) -> fill plot 'b'
           (_  ,_,_,True) -> fill plot 'w'
           _ ->
             do start <- getInputLine " ◯ " 
                case start of
                  Nothing -> return ()
                  Just s ->
                    do outputStrLn . hex $ bSrc <$> load (noMoveB plot <$> plot) <$> hexArgs
                       outputStrLn $ replicate 52 ' ' <> " ◯ " <> (show $ b plot)
                       outputStrLn $ replicate 52 ' ' <> " ● " <> (show $ w plot)
                       src <- getInputLine " ◯ "
                       case src of
                         Nothing -> return ()
                         Just s 
                          | elem (s,'b') plot && (not $ foldr (\x y -> elem x (fst <$> plot) && y) True $ (filter (\x -> not $ elem x outerLimit) $ ns s (jLst <> cLst))) ->
                            do outputStrLn . hex $ showDes s (cLst <> jLst) <$> load plot <$> hexArgs
                               outputStrLn $ replicate 52 ' ' <> " ◯ " <> (show $ b plot)
                               outputStrLn $ replicate 52 ' ' <> " ● " <> (show $ w plot)
                               des <- getInputLine " ◯ "
                               case des of
                                 Nothing -> return ()
                                 Just d 
                                   | elem d (ns s cLst) -> do lift $ put ('w', (d,'b'):plot)
                                                              (_, plot) <- lift get
                                                              lift $ put ('w', ((\(x,y) -> (x,'b')) <$> filter (\(x,y) -> elem x (ns d cLst) && y == 'w') plot) <> filter (not . (\(x,y) -> elem x (ns d cLst) && y == 'w')) plot)
                                                              game
                                   | elem d (ns s jLst) -> do lift $ put ('w', (d,'b'):filter (\(x,_) -> x /= s) plot)
                                                              (_, plot) <- lift get
                                                              lift $ put ('w', ((\(x,y) -> (x,'b')) <$> filter (\(x,y) -> elem x (ns d cLst) && y == 'w') plot) <> filter (not . (\(x,y) -> elem x (ns d cLst) && y == 'w')) plot)
                                                              game
                                   | otherwise -> game
                          | otherwise -> game
    'w' -> 
      do let b p = length $ filter (\(_,y) -> y == 'b') p
             w p = length $ filter (\(_,y) -> y == 'w') p
             noMove = all (== True) $ foldr (\x y -> elem x (fst <$> plot) && y) True <$> filter (\x -> not $ elem x outerLimit) <$> flip ns (jLst <> cLst) <$> fst <$> filter (\(_,y) -> y == 'w') plot
             count = case (b plot == w plot, b plot < w plot) of
                       (True,_) -> do outputStrLn "   TIE!"; return ()
                       (_,True) -> do outputStrLn " ● WINNER!"; return ()
                       _        -> do outputStrLn " ◯ WINNER!"; return ()
             fill' p c = concat $ filter (\x -> not $ elem x (fst <$> p)) <$> filter (\x -> not $ elem x outerLimit) <$> flip ns cLst <$> fst <$> filter (\(_,y) -> y == c) p
             fill p c = case fill' p c of
                    [] -> count
                    _  -> do lift $ put (c, nub $ ((\x -> (x,c)) <$> fill' p c) <> p)
                             (_, p') <- lift get
                             _ <- getInputLine $ if c == 'w' then " ● " else " ◯ " 
                             outputStrLn . hex $ clean <$> load p' <$> hexArgs
                             outputStrLn $ replicate 52 ' ' <> " ◯ " <> (show $ b p')
                             outputStrLn $ replicate 52 ' ' <> " ● " <> (show $ w p')
                             outputStrLn $ show p'
                             fill p' c
         outputStrLn . hex $ clean <$> load plot <$> hexArgs
         outputStrLn $ replicate 52 ' ' <> " ◯ " <> (show $ b plot)
         outputStrLn $ replicate 52 ' ' <> " ● " <> (show $ w plot)
         case (length plot, b plot, w plot, noMove) of
           (157,_,_,_) -> count
           (_  ,0,_,_) -> fill plot 'w'
           (_  ,_,0,_) -> fill plot 'b'
           (_  ,_,_,True) -> fill plot 'b'
           _ ->
             do start <- getInputLine " ● " 
                case start of
                  Nothing -> return ()
                  Just s ->
                    do outputStrLn . hex $ wSrc <$> load (noMoveW plot <$> plot) <$> hexArgs
                       outputStrLn $ replicate 52 ' ' <> " ◯ " <> (show $ b plot)
                       outputStrLn $ replicate 52 ' ' <> " ● " <> (show $ w plot)
                       src <- getInputLine " ● "
                       case src of
                         Nothing -> return ()
                         Just s 
                          | elem (s,'w') plot && (not $ foldr (\x y -> elem x (fst <$> plot) && y) True $ (filter (\x -> not $ elem x outerLimit) $ ns s (jLst <> cLst))) ->
                            do outputStrLn . hex $ showDes s (cLst <> jLst) <$> load plot <$> hexArgs
                               outputStrLn $ replicate 52 ' ' <> " ◯ " <> (show $ b plot)
                               outputStrLn $ replicate 52 ' ' <> " ● " <> (show $ w plot)
                               des <- getInputLine " ● "
                               case des of
                                 Nothing -> return ()
                                 Just d 
                                   | elem d (ns s cLst) -> do lift $ put ('b', (d,'w'):plot)
                                                              (_, plot) <- lift get
                                                              lift $ put ('b', ((\(x,y) -> (x,'w')) <$> filter (\(x,y) -> elem x (ns d cLst) && y == 'b') plot) <> filter (not . (\(x,y) -> elem x (ns d cLst) && y == 'b')) plot)
                                                              game
                                   | elem d (ns s jLst) -> do lift $ put ('b', (d,'w'):filter (\(x,_) -> x /= s) plot)
                                                              (_, plot) <- lift get
                                                              lift $ put ('b', ((\(x,y) -> (x,'w')) <$> filter (\(x,y) -> elem x (ns d cLst) && y == 'b') plot) <> filter (not . (\(x,y) -> elem x (ns d cLst) && y == 'b')) plot)
                                                              game
                                   | otherwise -> game
                          | otherwise -> game

main :: IO ()
main = do 
  putStrLn "HEXXAGŌN"
  runStateT (runInputT defaultSettings game) defBoard
  putStrLn ""

showBoard :: StateT Hex IO ()
showBoard = do
  (_, plot) <- get
  lift . putStrLn . hex $ load plot <$> hexArgs

showBoard' = do
  putStrLn "HEXXAGŌN"
  runStateT showBoard defBoard
  putStrLn ""

hexArgs :: [(String, String)]
hexArgs = 
  fmap (\x -> (x,"__")) . concatMap (\(x,y) -> (x !!) <$> y) $
    zip 
      (filter (not . null) . concat $ 
        [[x,y] | (x,y) <- zip 
          ((\x -> (ls !! x !!) <$> [1,3..17]) <$> [0,2..24]) $
          ((\x -> (ls !! x !!) <$> [0,2..18]) <$> [1,3..23]) <> [[]] 
        ]
      )
      (reverse (ix [0..9]) <> take 7 (cycle ([0..9]:[0..8]:[])) <> ix [0..9])
  where
    ls = zipWith (\x y -> x:y:[]) ['a'..'s'] . repeat <$> reverse ['a'..'y']
    ix [x] = []
    ix x 
      | even $ length x = init x : ix (init x)
      | odd  $ length x = tail x : ix (tail x) 

hex :: [(String, String)] -> String
hex [ a0
    , b0,b1
    , c0,c1,c2
    , d0,d1,d2,d3
    , e0,e1,e2,e3,e4
    , f0,f1,f2,f3,f4,f5
    , g0,g1,g2,g3,g4,g5,g6
    , h0,h1,h2,h3,h4,h5,h6,h7
    , i0,i1,i2,i3,i4,i5,i6,i7,i8
    , j0,j1,j2,j3,j4,j5,j6,j7,j8,j9
    , k0,k1,k2,k3,k4,k5,k6,k7,k8
    , l0,l1,l2,l3,l4,l5,l6,l7,l8,l9
    , m0,m1,m2,m3,m4,m5,m6,m7,m8
    , n0,n1,n2,n3,n4,n5,n6,n7,n8,n9
    , o0,o1,o2,o3,o4,o5,o6,o7,o8
    , p0,p1,p2,p3,p4,p5,p6,p7,p8,p9
    , q0,q1,q2,q3,q4,q5,q6,q7,q8
    , r0,r1,r2,r3,r4,r5,r6,r7
    , s0,s1,s2,s3,s4,s5,s6
    , t0,t1,t2,t3,t4,t5
    , u0,u1,u2,u3,u4
    , v0,v1,v2,v3
    , w0,w1,w2
    , x0,x1
    , y0
    ] = 
  "                            __\n\
  \                         __/"<>fst a0<>"\\__\n\
  \                      __/"<>fst b0<>"\\"<>snd a0<>"/"<>fst b1<>"\\__\n\
  \                   __/"<>fst c0<>"\\"<>snd b0<>"/"<>fst c1<>"\\"<>snd b1<>"/"<>fst c2<>"\\__\n\
  \                __/"<>fst d0<>"\\"<>snd c0<>"/"<>fst d1<>"\\"<>snd c1<>"/"<>fst d2<>"\\"<>snd c2<>"/"<>fst d3<>"\\__\n\
  \             __/"<>fst e0<>"\\"<>snd d0<>"/"<>fst e1<>"\\"<>snd d1<>"/"<>fst e2<>"\\"<>snd d2<>"/"<>fst e3<>"\\"<>snd d3<>"/"<>fst e4<>"\\__\n\
  \          __/"<>fst f0<>"\\"<>snd e0<>"/"<>fst f1<>"\\"<>snd e1<>"/"<>fst f2<>"\\"<>snd e2<>"/"<>fst f3<>"\\"<>snd e3<>"/"<>fst f4<>"\\"<>snd e4<>"/"<>fst f5<>"\\__\n\
  \       __/"<>fst g0<>"\\"<>snd f0<>"/"<>fst g1<>"\\"<>snd f1<>"/"<>fst g2<>"\\"<>snd f2<>"/"<>fst g3<>"\\"<>snd f3<>"/"<>fst g4<>"\\"<>snd f4<>"/"<>fst g5<>"\\"<>snd f5<>"/"<>fst g6<>"\\__\n\
  \    __/"<>fst h0<>"\\"<>snd g0<>"/"<>fst h1<>"\\"<>snd g1<>"/"<>fst h2<>"\\"<>snd g2<>"/"<>fst h3<>"\\"<>snd g3<>"/"<>fst h4<>"\\"<>snd g4<>"/"<>fst h5<>"\\"<>snd g5<>"/"<>fst h6<>"\\"<>snd g6<>"/"<>fst h7<>"\\__\n\
  \ __/"<>fst i0<>"\\"<>snd h0<>"/"<>fst i1<>"\\"<>snd h1<>"/"<>fst i2<>"\\"<>snd h2<>"/"<>fst i3<>"\\"<>snd h3<>"/"<>fst i4<>"\\"<>snd h4<>"/"<>fst i5<>"\\"<>snd h5<>"/"<>fst i6<>"\\"<>snd h6<>"/"<>fst i7<>"\\"<>snd h7<>"/"<>fst i8<>"\\__\n\
  \/"<>fst j0<>"\\"<>snd i0<>"/"<>fst j1<>"\\"<>snd i1<>"/"<>fst j2<>"\\"<>snd i2<>"/"<>fst j3<>"\\"<>snd i3<>"/"<>fst j4<>"\\"<>snd i4<>"/"<>fst j5<>"\\"<>snd i5<>"/"<>fst j6<>"\\"<>snd i6<>"/"<>fst j7<>"\\"<>snd i7<>"/"<>fst j8<>"\\"<>snd i8<>"/"<>fst j9<>"\\\n\
  \\\"<>snd j0<>"/"<>fst k0<>"\\"<>snd j1<>"/"<>fst k1<>"\\"<>snd j2<>"/"<>fst k2<>"\\"<>snd j3<>"/"<>fst k3<>"\\"<>snd j4<>"/"<>fst k4<>"\\"<>snd j5<>"/"<>fst k5<>"\\"<>snd j6<>"/"<>fst k6<>"\\"<>snd j7<>"/"<>fst k7<>"\\"<>snd j8<>"/"<>fst k8<>"\\"<>snd j9<>"/\n\
  \/"<>fst l0<>"\\"<>snd k0<>"/"<>fst l1<>"\\"<>snd k1<>"/"<>fst l2<>"\\"<>snd k2<>"/"<>fst l3<>"\\"<>snd k3<>"/"<>fst l4<>"\\"<>snd k4<>"/"<>fst l5<>"\\"<>snd k5<>"/"<>fst l6<>"\\"<>snd k6<>"/"<>fst l7<>"\\"<>snd k7<>"/"<>fst l8<>"\\"<>snd k8<>"/"<>fst l9<>"\\\n\
  \\\"<>snd l0<>"/"<>fst m0<>"\\"<>snd l1<>"/"<>fst m1<>"\\"<>snd l2<>"/"<>fst m2<>"\\"<>snd l3<>"/"<>fst m3<>"\\"<>snd l4<>"/"<>fst m4<>"\\"<>snd l5<>"/"<>fst m5<>"\\"<>snd l6<>"/"<>fst m6<>"\\"<>snd l7<>"/"<>fst m7<>"\\"<>snd l8<>"/"<>fst m8<>"\\"<>snd l9<>"/\n\
  \/"<>fst n0<>"\\"<>snd m0<>"/"<>fst n1<>"\\"<>snd m1<>"/"<>fst n2<>"\\"<>snd m2<>"/"<>fst n3<>"\\"<>snd m3<>"/"<>fst n4<>"\\"<>snd m4<>"/"<>fst n5<>"\\"<>snd m5<>"/"<>fst n6<>"\\"<>snd m6<>"/"<>fst n7<>"\\"<>snd m7<>"/"<>fst n8<>"\\"<>snd m8<>"/"<>fst n9<>"\\\n\
  \\\"<>snd n0<>"/"<>fst o0<>"\\"<>snd n1<>"/"<>fst o1<>"\\"<>snd n2<>"/"<>fst o2<>"\\"<>snd n3<>"/"<>fst o3<>"\\"<>snd n4<>"/"<>fst o4<>"\\"<>snd n5<>"/"<>fst o5<>"\\"<>snd n6<>"/"<>fst o6<>"\\"<>snd n7<>"/"<>fst o7<>"\\"<>snd n8<>"/"<>fst o8<>"\\"<>snd n9<>"/\n\
  \/"<>fst p0<>"\\"<>snd o0<>"/"<>fst p1<>"\\"<>snd o1<>"/"<>fst p2<>"\\"<>snd o2<>"/"<>fst p3<>"\\"<>snd o3<>"/"<>fst p4<>"\\"<>snd o4<>"/"<>fst p5<>"\\"<>snd o5<>"/"<>fst p6<>"\\"<>snd o6<>"/"<>fst p7<>"\\"<>snd o7<>"/"<>fst p8<>"\\"<>snd o8<>"/"<>fst p9<>"\\\n\
  \\\"<>snd p0<>"/"<>fst q0<>"\\"<>snd p1<>"/"<>fst q1<>"\\"<>snd p2<>"/"<>fst q2<>"\\"<>snd p3<>"/"<>fst q3<>"\\"<>snd p4<>"/"<>fst q4<>"\\"<>snd p5<>"/"<>fst q5<>"\\"<>snd p6<>"/"<>fst q6<>"\\"<>snd p7<>"/"<>fst q7<>"\\"<>snd p8<>"/"<>fst q8<>"\\"<>snd p9<>"/\n\
  \   \\"<>snd q0<>"/"<>fst r0<>"\\"<>snd q1<>"/"<>fst r1<>"\\"<>snd q2<>"/"<>fst r2<>"\\"<>snd q3<>"/"<>fst r3<>"\\"<>snd q4<>"/"<>fst r4<>"\\"<>snd q5<>"/"<>fst r5<>"\\"<>snd q6<>"/"<>fst r6<>"\\"<>snd q7<>"/"<>fst r7<>"\\"<>snd q8<>"/\n\
  \      \\"<>snd r0<>"/"<>fst s0<>"\\"<>snd r1<>"/"<>fst s1<>"\\"<>snd r2<>"/"<>fst s2<>"\\"<>snd r3<>"/"<>fst s3<>"\\"<>snd r4<>"/"<>fst s4<>"\\"<>snd r5<>"/"<>fst s5<>"\\"<>snd r6<>"/"<>fst s6<>"\\"<>snd r7<>"/\n\
  \         \\"<>snd s0<>"/"<>fst t0<>"\\"<>snd s1<>"/"<>fst t1<>"\\"<>snd s2<>"/"<>fst t2<>"\\"<>snd s3<>"/"<>fst t3<>"\\"<>snd s4<>"/"<>fst t4<>"\\"<>snd s5<>"/"<>fst t5<>"\\"<>snd s6<>"/\n\
  \            \\"<>snd t0<>"/"<>fst u0<>"\\"<>snd t1<>"/"<>fst u1<>"\\"<>snd t2<>"/"<>fst u2<>"\\"<>snd t3<>"/"<>fst u3<>"\\"<>snd t4<>"/"<>fst u4<>"\\"<>snd t5<>"/\n\
  \               \\"<>snd u0<>"/"<>fst v0<>"\\"<>snd u1<>"/"<>fst v1<>"\\"<>snd u2<>"/"<>fst v2<>"\\"<>snd u3<>"/"<>fst v3<>"\\"<>snd u4<>"/\n\
  \                  \\"<>snd v0<>"/"<>fst w0<>"\\"<>snd v1<>"/"<>fst w1<>"\\"<>snd v2<>"/"<>fst w2<>"\\"<>snd v3<>"/\n\
  \                     \\"<>snd w0<>"/"<>fst x0<>"\\"<>snd w1<>"/"<>fst x1<>"\\"<>snd w2<>"/\n\
  \                        \\"<>snd x0<>"/"<>fst y0<>"\\"<>snd x1<>"/\n\
  \                           \\"<>snd y0<>"/"
