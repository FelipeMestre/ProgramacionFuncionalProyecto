data HijaraPlayer = BluePlayer | YellowPlayer deriving (Eq, Show, Enum)

beginning :: HijaraGame

activePlayer :: HijaraGame -> HijaraPlayer

actions :: HijaraGame -> [(HijaraPlayer, [HijaraAction])] 

next :: HijaraGame -> (HijaraPlayer, HijaraAction) -> HijaraGame

result :: HijaraGame -> [(HijaraPlayer, Int)]

score :: HijaraGame -> [(HijaraPlayer, Int)]

showGame :: HijaraGame -> String

showAction :: HijaraAction -> String

readAction :: String -> HijaraAction