newtype Talk = Talk Int
    deriving (Eq, Ord)

instance NFData Talk

instance Show Talk where 
    show (Talk t) = show t

data Person = Person {
    name :: String,
    talks :: [Talks]
} deriving (Show)

type TimeTable = [[Talk]]

timetable :: [Person] -> [Talk] -> Int -> Int -> [TimeTable]
timetable people talks maxTrack maxSlot = 

clashes :: Map Talk [Talk]
clashes = Map.fromListWith union [(t, ts) | p <- people, (t, ts) <- selects (talks p) ]

generate :: Int -> Int -> [[Talk]] -> [Talk] -> [Talk] -> [Talk] -> [TimeTable]
generate slotNo trackNo slots slot slotTalks talks
    | slotNo == maxSlot = [slots]
    | trackNo == maxTrack = 
        generate (slotNo + 1) 0 (slot:slots) [] talks talks
    | otherwise = concat
        [ generate slotNo (trackNo + 1) slots (t:slot) slotTalks' talks' 
        | (t, ts) <- selects slotTalks
        , let clashesWithT = Map.findWithDefault [] t clashes
        , let slotTalks' = filter (`notElem` clashesWithT) ts
        , let talks' = filter (/= t) talks
        ]


bench :: Int -> Int -> Int -> Int -> Int -> StdGen
      -> ([Person],[Talk],[TimeTable])

bench nslots ntracks ntalks npersons c_per_s gen =
  (persons,talks, timetable persons talks ntracks nslots)
 where
  total_talks = nslots * ntracks

  talks = map Talk [1..total_talks]
  persons = mkpersons npersons gen

  mkpersons :: Int -> StdGen -> [Person]
  mkpersons 0 g = []
  mkpersons n g = Person ('P':show n) (take c_per_s cs) : rest
        where
          (g1,g2) = split g
          rest = mkpersons (n-1) g2
          cs = nub [ talks !! n | n <- randomRs (0,ntalks-1) g ]

main = do
   [ a, b, c, d, e ] <- fmap (fmap read) getArgs
   let g = mkStdGen 1001
   let (ss,cs,ts) = bench a b c d e g
   print ss
   print (length ts)
