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


