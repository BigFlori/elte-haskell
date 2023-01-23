module PlantsVsZombies where
import qualified Data.Map as Map
import Data.List

--Typedef
type Coordinate = (Int, Int)
type Sun = Int
type PlantMap = Map.Map Coordinate Plant
type ZombieMap = Map.Map Coordinate Zombie

--Osztályok

data Plant = Peashooter { plantHealth :: Int }
            | Sunflower { plantHealth :: Int }
            | Walnut { plantHealth :: Int }
            | CherryBomb { plantHealth :: Int }
            deriving (Show, Eq)

data Zombie = Basic { zombieHealth :: Int, movementSpeed :: Int }
            | Conehead { zombieHealth :: Int, movementSpeed :: Int }
            | Buckethead { zombieHealth :: Int, movementSpeed :: Int }
            | Vaulting { zombieHealth :: Int, movementSpeed :: Int }
            deriving (Show, Eq)

data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)] deriving (Show, Eq)

--Növények definiálása
defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

pricePeashooter :: Sun
pricePeashooter = 100

priceSunflower :: Sun
priceSunflower = 50

priceWalnut :: Sun
priceWalnut = 50

priceCherryBomb :: Sun
priceCherryBomb = 150

--Zombik definiálása
basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2

--Növények vásárlása 
isInTable :: Coordinate -> Bool
isInTable (y, x) = (x >= 0 && x <= 11) && (y >= 0 && y <= 4)

getPrice :: Plant -> Sun
getPrice plant
    | plant == defaultPeashooter = pricePeashooter
    | plant == defaultSunflower = priceSunflower
    | plant == defaultWalnut = priceWalnut
    | plant == defaultCherryBomb = priceCherryBomb
    | otherwise = 0

hasEnoughSun :: Sun -> Plant -> Bool
hasEnoughSun sun plant
    | plant == defaultPeashooter = sun >= pricePeashooter
    | plant == defaultSunflower = sun >= priceSunflower
    | plant == defaultWalnut = sun >= priceWalnut
    | plant == defaultCherryBomb = sun >= priceCherryBomb
    | otherwise = False

plants :: [(Coordinate, Plant)] -> PlantMap
plants plantList = Map.fromList plantList

isPlantCoordinate :: Coordinate -> PlantMap -> Bool
isPlantCoordinate (y, x) map =
    case Map.lookup (y, x) map of
        Nothing -> False
        Just plant -> True

--van elég nap? x
--táblán lévő koordináta? x
--foglalt a táblahely?
--  lookup felkeressük a coordináta alapján a coordinátát

--nap levonása
--növény hozzáadása a listához

-- ! Lehetséges hiba, hogy a plantList végére vagy az elejére kell hozzáfűzni a planteket
-- ! eset tesztelésére nincsen megadva teszteset a feladat leírásban!

tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel sun [] zombieList) coords plant = if (hasEnoughSun sun plant) && (isInTable coords) then Just (GameModel (sun - getPrice plant) [(coords, plant)] zombieList) else Nothing
tryPurchase (GameModel sun plantList zombieList) coords plant = if (hasEnoughSun sun plant) && (not (isPlantCoordinate coords (plants plantList))) && (isInTable coords) then Just (GameModel (sun - getPrice plant) (plantList ++ [(coords, plant)]) zombieList) else Nothing

--Zombi lerakása

--mindig 11-től indulnak (jobbról balra mennek)
--ha foglalt a (sáv, 11) akkor Nothing
--ha nem létezik a sáv akkor Nothing

zombies :: [(Coordinate, Zombie)] -> ZombieMap
zombies zombieList = Map.fromList zombieList

isZombieCoordinate :: Coordinate -> ZombieMap -> Bool
isZombieCoordinate (y, x) map =
    case Map.lookup (y, x) map of
        Nothing -> True
        Just zombie -> False

placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel sun plantList []) zombie y = if (isInTable (y, 11)) then Just (GameModel sun plantList [((y, 11), zombie)]) else Nothing
placeZombieInLane (GameModel sun plantList zombieList) zombie y = if (isInTable (y, 11)) && (isZombieCoordinate (y, 11) (zombies zombieList)) then Just (GameModel sun plantList ([((y, 11), zombie)] ++ zombieList)) else Nothing

--Zombik mozgása és támadása
--Zombik poziciójának ellenőrzéséhez szükséges függvények

isZombieOnPlant :: GameModel -> (Coordinate, Zombie) -> Bool
isZombieOnPlant (GameModel _ plantList _) zombieObj@((y, x), zombie) = isZombieOnPlantByList plantList zombieObj

isZombieOnPlantByList :: [(Coordinate, Plant)] -> (Coordinate, Zombie) -> Bool
isZombieOnPlantByList plantList ((y, x), _) = (isPlantCoordinate (y, x) (plants plantList))

isZombieVaulting :: Zombie -> Bool
isZombieVaulting (Vaulting _ _) = True
isZombieVaulting (Basic _ _) = False
isZombieVaulting (Conehead _ _) = False
isZombieVaulting (Buckethead _ _) = False

--Zombik támadásához szükséges függvények
-- getPlantAt :: Coordinate -> PlantMap -> Maybe Plant
-- getPlantAt (y, x) map =
--     case Map.lookup (y, x) map of
--         Nothing -> Nothing
--         Just plant -> Just plant

--Plant hp csökkentése
reducePlantHealth :: (Coordinate, Plant) -> (Coordinate, Plant)
reducePlantHealth (coords, (Peashooter plantHealth)) = (coords, ((Peashooter (plantHealth - 1))))
reducePlantHealth (coords, (Sunflower plantHealth)) = (coords, (Sunflower (plantHealth - 1)))
reducePlantHealth (coords, (Walnut plantHealth)) = (coords, (Walnut (plantHealth - 1)))
reducePlantHealth (coords, (CherryBomb plantHealth)) = (coords, (CherryBomb (plantHealth - 1)))

reducePlantHealthBy :: (Coordinate, Plant) -> Int -> (Coordinate, Plant)
reducePlantHealthBy (coords, (Peashooter plantHealth)) minusHp = (coords, ((Peashooter (plantHealth - minusHp))))
reducePlantHealthBy (coords, (Sunflower plantHealth)) minusHp = (coords, (Sunflower (plantHealth - minusHp)))
reducePlantHealthBy (coords, (Walnut plantHealth)) minusHp = (coords, (Walnut (plantHealth - minusHp)))
reducePlantHealthBy (coords, (CherryBomb plantHealth)) minusHp = (coords, (CherryBomb (plantHealth - minusHp)))

--Plant hp csökkentése koordináta alapján
-- reducePlantHealthAt :: Coordinate -> [(Coordinate, Plant)] -> [(Coordinate, Plant)]
-- reducePlantHealthAt _ [] = []
-- reducePlantHealthAt (y, x) (plantElement@((pY, pX), plant):xs)
--     | y == pY && x == pX = (reducePlantHealth plantElement) : reducePlantHealthAt (y, x) xs
--     | otherwise = reducePlantHealthAt (y, x) xs

--Vaulting zombihoz szükséges függvények
isVaultingPassingByPlant :: Coordinate -> PlantMap -> Bool
isVaultingPassingByPlant (y, x) map =
    case Map.lookup (y, (x - 1)) map of
        Nothing -> False
        Just plant -> True

--Nothing ha nem tud lépni és támadnia kell
moveVaulting :: GameModel -> (Coordinate, Zombie) -> (Coordinate, Zombie)
moveVaulting (GameModel _ [] _) vaulting@((y, x), (Vaulting zombieHealth movementSpeed)) = ((y, (x - movementSpeed)), (Vaulting zombieHealth movementSpeed))
moveVaulting (GameModel _ plantList _) vaulting@((y, x), (Vaulting zombieHealth movementSpeed))
    | (isZombieOnPlantByList plantList vaulting) && movementSpeed == 2 = ((y, (x - 1)), (Vaulting zombieHealth (movementSpeed - 1)))
    | (isVaultingPassingByPlant (y, x) (plants plantList)) && movementSpeed == 2 = ((y, (x - 2)), (Vaulting zombieHealth (movementSpeed - 1)))
    | not (isZombieOnPlantByList plantList vaulting) && movementSpeed == 1 = ((y, (x - movementSpeed)), (Vaulting zombieHealth movementSpeed))
    | otherwise = ((y, (x - movementSpeed)), (Vaulting zombieHealth movementSpeed))

moveZombie :: (Coordinate, Zombie) -> (Coordinate, Zombie)
moveZombie ((y, x), zombie) = ((y, (x - (movementSpeed zombie))), zombie)

consOnMaybe :: a -> Maybe [a] -> Maybe [a]
consOnMaybe _ Nothing   = Nothing
consOnMaybe x (Just xs) = Just (x : xs)

-- moveZombieAt :: GameModel -> Coordinate -> Maybe [(Coordinate, Zombie)]
-- moveZombieAt (GameModel _ _ []) _ = Just []
-- moveZombieAt model@(GameModel sun plantList (z@((zY, zX), zombie):zs)) (y, x)
--     | (zX - (movementSpeed zombie)) < 0 = Nothing
--     | isZombieVaulting zombie = consOnMaybe (moveVaulting model z) (moveZombieAt (GameModel sun plantList zs) (y, x))
--     | y == zY && x == zX = consOnMaybe (moveZombie z) (moveZombieAt (GameModel sun plantList zs) (y, x))
--     | otherwise = consOnMaybe z (moveZombieAt (GameModel sun plantList zs) (y, x))

moveAllZombies :: GameModel -> Maybe [(Coordinate, Zombie)]
moveAllZombies (GameModel _ _ []) = Just []
moveAllZombies model@(GameModel sun plantList (x@((zY, zX), zombie):xs))
    | (zX - 1) < 0 = Nothing
    | (isZombieOnPlant model x) && (movementSpeed zombie) == 1 = consOnMaybe x (moveAllZombies (GameModel sun plantList xs))
    | isZombieVaulting zombie = consOnMaybe (moveVaulting model x) (moveAllZombies (GameModel sun plantList xs))
    | otherwise = consOnMaybe (moveZombie x) (moveAllZombies (GameModel sun plantList xs))

--Mennyi zombi áll egy adott koordinátán ezáltal csökkentjük a növény hpját
zombiesStandingAt :: Coordinate -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
zombiesStandingAt _ [] = []
zombiesStandingAt coords@(y, x) (z@((zY, zX), zombie):zs)
    | y == zY && x == zX && (movementSpeed zombie) == 1 = z : zombiesStandingAt coords zs
    | otherwise = zombiesStandingAt coords zs

lengthOfZombiesStandingAt :: Coordinate -> [(Coordinate, Zombie)] -> Int
lengthOfZombiesStandingAt coords zombieList = length (zombiesStandingAt coords zombieList)

reduceAllPlantHealth :: GameModel -> [(Coordinate, Plant)]
reduceAllPlantHealth (GameModel _ [] _) = []
reduceAllPlantHealth model@(GameModel sun (x@((pY, pX), plant):xs) zombieList@(y@((zY, zX), zombie):ys))
    | (lengthOfZombiesStandingAt (pY, pX) zombieList) > 0 = (reducePlantHealthBy x (lengthOfZombiesStandingAt (pY, pX) zombieList)) : reduceAllPlantHealth (GameModel sun xs zombieList)
    | isZombieOnPlant model y && (movementSpeed zombie) == 2 = x : reduceAllPlantHealth (GameModel sun xs zombieList)
    | otherwise = x : reduceAllPlantHealth (GameModel sun xs zombieList)

    -- | isZombieOnPlant model y && (movementSpeed zombie) == 1 = reducePlantHealth x : reduceAllPlantHealth (GameModel sun xs ys)

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

hasValue :: Maybe a -> Bool
hasValue Nothing = False
hasValue (Just x) = True

--Just (GameModel sun (reduceAllPlantHealth model) (fromJust (moveAllZombies model)))
performZombieActions :: GameModel -> Maybe GameModel
performZombieActions (GameModel sun [] []) = Just (GameModel sun [] [])
performZombieActions model@(GameModel sun [] zombieList@(y@((zY, zX), zombie):ys)) = if hasValue (moveAllZombies model) then Just (GameModel sun [] (fromJust (moveAllZombies model))) else Nothing
performZombieActions (GameModel sun plantList []) = Just (GameModel sun plantList [])
performZombieActions model@(GameModel sun plantList@(x@((pY, pX), plant):xs) zombieList@(y@((zY, zX), zombie):ys)) = if hasValue (moveAllZombies model) then Just (GameModel sun (reduceAllPlantHealth model) (fromJust (moveAllZombies model))) else Nothing

--movementSpeed = hány mezőt lép egyszerre
--ha a zombi koordinátája megegyezik egy növénnyel akkor 1-el csökkenti a növény hpját
--ha a Vaulting zombi koordinátája meg egyezik egy növénnyel és a sebessége == 2 akkor továbbhalad a növényen
--ha egy zombi koordinátája == (y, 0) akkor Nothing a kimenet

--sorrendben:
--1. zombi koordi == növény kordi?
--2. ha nem tovább lép
--2. ha igen növény hp csökkentés

--Pályatisztítás

cleanPlants :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
cleanPlants [] = []
cleanPlants ((coords, plant):xs)
    | (plantHealth plant) <= 0 = cleanPlants xs
    | otherwise = (coords, plant) : cleanPlants xs

cleanZombies :: [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
cleanZombies [] = []
cleanZombies ((coords, zombie):xs)
    | (zombieHealth zombie) <= 0 = cleanZombies xs
    | otherwise = (coords, zombie) : cleanZombies xs

cleanBoard :: GameModel -> GameModel
cleanBoard (GameModel sun plantList zombieList) = (GameModel sun (cleanPlants plantList) (cleanZombies zombieList))

--Extra feladat
--Növények műveletei

isSunflower :: (Coordinate, Plant) -> Bool
isSunflower (_, (Sunflower _)) = True
isSunflower (_, (Peashooter _)) = False
isSunflower (_, (CherryBomb _)) = False
isSunflower (_, (Walnut _)) = False

isPeashooter :: (Coordinate, Plant) -> Bool
isPeashooter (_, (Sunflower _)) = False
isPeashooter (_, (Peashooter _)) = True
isPeashooter (_, (CherryBomb _)) = False
isPeashooter (_, (Walnut _)) = False

isCherryBomb :: (Coordinate, Plant) -> Bool
isCherryBomb (_, (Sunflower _)) = False
isCherryBomb (_, (Peashooter _)) = False
isCherryBomb (_, (CherryBomb _)) = True
isCherryBomb (_, (Walnut _)) = False

--Sunflower action
produceSun :: GameModel -> Sun
produceSun (GameModel sun plantList@(x@((pY, pX), plant):xs) _) = sun + (length (filter isSunflower plantList)) * 25

--Peashooter segédfüggvények
getZombiesInLane :: Int -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
getZombiesInLane _ [] = []
getZombiesInLane lane zombieList@(z@((zY, zX), zombie):zs)
    | lane == zY = z : getZombiesInLane lane zs
    | otherwise = getZombiesInLane lane zs

findMinInLane :: [(Coordinate, Zombie)] -> Int
findMinInLane [] = -1
findMinInLane [((zY, zX), zombie)] = zX
findMinInLane zombieList@(z@((zY1, zX1), zombie1):y@((zY2, zX2), zombie2):zs)
    | zX1 > zX2 = findMinInLane (y:zs)
    | zX1 < zX2 = findMinInLane (z:zs)
    | zX1 == zX2 = findMinInLane (z:zs)

findPeashooterTargetsInLane :: Int -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
findPeashooterTargetsInLane _ [] = []
findPeashooterTargetsInLane min zombieList@(z@((zY, zX), zombie):zs)
    | zX == min = z : findPeashooterTargetsInLane min zs
    | otherwise = findPeashooterTargetsInLane min zs

peashooterTargets :: GameModel -> [(Coordinate, Zombie)]
peashooterTargets (GameModel _ _ []) = []
peashooterTargets model@(GameModel _ _ zombieList@(z@((zY, zX), zombie):zs)) = ((findPeashooterTargetsInLane (findMinInLane (getZombiesInLane 0 zombieList)) (getZombiesInLane 0 zombieList)) ++ (findPeashooterTargetsInLane (findMinInLane (getZombiesInLane 1 zombieList)) (getZombiesInLane 1 zombieList)) ++ (findPeashooterTargetsInLane (findMinInLane (getZombiesInLane 2 zombieList)) (getZombiesInLane 2 zombieList)) ++ (findPeashooterTargetsInLane (findMinInLane (getZombiesInLane 3 zombieList)) (getZombiesInLane 3 zombieList)) ++ (findPeashooterTargetsInLane (findMinInLane (getZombiesInLane 4 zombieList)) (getZombiesInLane 4 zombieList)))

reduceZombieHealthBy :: Int -> (Coordinate, Zombie) -> (Coordinate, Zombie)
reduceZombieHealthBy minusHp (coords, (Basic zombieHealth movementSpeed)) = (coords, (Basic (zombieHealth - minusHp) movementSpeed))
reduceZombieHealthBy minusHp (coords, (Conehead zombieHealth movementSpeed)) = (coords, (Conehead (zombieHealth - minusHp) movementSpeed))
reduceZombieHealthBy minusHp (coords, (Buckethead zombieHealth movementSpeed)) = (coords, (Buckethead (zombieHealth - minusHp) movementSpeed))
reduceZombieHealthBy minusHp (coords, (Vaulting zombieHealth movementSpeed)) = (coords, (Vaulting (zombieHealth - minusHp) movementSpeed))

--első listában az összes zombi, második listában azok a zombik amiknek csökkenteni kell a hpját
reducePeashooterTargetsHealth :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
reducePeashooterTargetsHealth _ [] _ = []
reducePeashooterTargetsHealth plantList@(p:py) allZombies@(x@((zY, zX), zombie):xs) targetZombies@(y:ys)
    | x `elem` targetZombies = (reduceZombieHealthBy (damageInLane (findPeashootersInLane zY (findPeashooters plantList))) x) : reducePeashooterTargetsHealth plantList xs targetZombies
    | otherwise = x : reducePeashooterTargetsHealth plantList xs targetZombies

findPeashooters :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
findPeashooters [] = []
findPeashooters (x@(coords, CherryBomb plantHealth):xs) = findPeashooters xs
findPeashooters (x@(coords, Peashooter plantHealth):xs) = x : findPeashooters xs
findPeashooters (x@(coords, Walnut plantHealth):xs) = findPeashooters xs
findPeashooters (x@(coords, Sunflower plantHealth):xs) = findPeashooters xs

--findPeashooters kimeneti listáját kell átadni ennek
findPeashootersInLane :: Int -> [(Coordinate, Plant)] -> [(Coordinate, Plant)]
findPeashootersInLane _ [] = []
findPeashootersInLane lane plantList@(x@((pY, pX), plant):xs)
    | pY == lane = x : findPeashootersInLane lane xs
    | otherwise = findPeashootersInLane lane xs

--findPeashootersInLane kimeneti listáját kell átadni
damageInLane :: [(Coordinate, Plant)] -> Int
damageInLane peashooters = length peashooters

--Peashooter action
peashooterAction :: GameModel -> [(Coordinate, Zombie)]
peashooterAction (GameModel _ [] _) = []
peashooterAction model@(GameModel sun plantList zombieList) = (reducePeashooterTargetsHealth plantList zombieList (peashooterTargets model))

--CherryBomb segédfüggvények
zombiesAt :: Coordinate -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
zombiesAt _ [] = []
zombiesAt (y, x) zombieList@(z@((zY, zX), zombie):zs)
    | y == zY && x == zX = z : zombiesAt (y, x) zs
    | otherwise = zombiesAt (y, x) zs

findCherryBombTargets :: Coordinate -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
findCherryBombTargets _ [] = []
findCherryBombTargets (y, x) zombieList@(z@((zY, zX), zombie):zs) = ((zombiesAt (y, x) zombieList) ++ (zombiesAt (y, x - 1) zombieList) ++ (zombiesAt (y, x + 1) zombieList) ++ (zombiesAt (y + 1, x - 1) zombieList) ++ (zombiesAt (y + 1, x) zombieList) ++ (zombiesAt (y + 1, x + 1) zombieList) ++ (zombiesAt (y - 1, x - 1) zombieList) ++ (zombiesAt (y - 1, x) zombieList) ++ (zombiesAt (y - 1, x + 1) zombieList))

findCherryBombs :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
findCherryBombs [] = []
findCherryBombs (x@(coords, CherryBomb plantHealth):xs) = x : findCherryBombs xs
findCherryBombs (x@(coords, Peashooter plantHealth):xs) = findCherryBombs xs
findCherryBombs (x@(coords, Walnut plantHealth):xs) = findCherryBombs xs
findCherryBombs (x@(coords, Sunflower plantHealth):xs) = findCherryBombs xs

findAllCherryBombTargets :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
findAllCherryBombTargets [] _ = []
findAllCherryBombTargets plantList@(x@((pY, pX), plant):xs) zombieList@(y@((zY, zX), zombie):ys) = (findCherryBombTargets (pY, pX) zombieList) ++ (findAllCherryBombTargets xs zombieList)

killZombie :: (Coordinate, Zombie) -> (Coordinate, Zombie)
killZombie (coords, (Basic zombieHealth movementSpeed)) = (coords, (Basic 0 movementSpeed))
killZombie (coords, (Conehead zombieHealth movementSpeed)) = (coords, (Conehead 0 movementSpeed))
killZombie (coords, (Buckethead zombieHealth movementSpeed)) = (coords, (Buckethead 0 movementSpeed))
killZombie (coords, (Vaulting zombieHealth movementSpeed)) = (coords, (Vaulting 0 movementSpeed))

--első listában az összes zombi, második listában azok a zombik amiknek csökkenteni kell a hpját 0-ra
killCherryBombTargets :: [(Coordinate, Zombie)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
killCherryBombTargets allZombies [] = allZombies
killCherryBombTargets [] _ = []
killCherryBombTargets allZombies@(x:xs) targetZombies@(y:ys)
    | x `elem` targetZombies = (killZombie x) : killCherryBombTargets xs targetZombies
    | otherwise = x : killCherryBombTargets xs targetZombies

-- cherryBombAction :: GameModel -> [(Coordinate, Zombie)]
cherryBombAction :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
cherryBombAction _ [] = []
cherryBombAction plantList@(x@((pY, pX), plant):xs) zombieList@(y@((zY, zX), zombie):ys) = if length (findCherryBombs plantList) > 0 then (killCherryBombTargets zombieList (findAllCherryBombTargets (findCherryBombs plantList) zombieList)) else zombieList

killCherryBombs :: GameModel -> [(Coordinate, Plant)]
killCherryBombs (GameModel _ [] _) = []
killCherryBombs (GameModel sun ((coords, (CherryBomb plantHealth)):xs) zombieList) = (coords, (CherryBomb 0)) : killCherryBombs (GameModel sun xs zombieList)
killCherryBombs (GameModel sun (x@(coords, (Peashooter plantHealth)):xs) zombieList) = x : killCherryBombs (GameModel sun xs zombieList)
killCherryBombs (GameModel sun (x@(coords, (Walnut plantHealth)):xs) zombieList) = x : killCherryBombs (GameModel sun xs zombieList)
killCherryBombs (GameModel sun (x@(coords, (Sunflower plantHealth)):xs) zombieList) = x : killCherryBombs (GameModel sun xs zombieList)

--Feladat függvénye
performPlantActions :: GameModel -> GameModel
performPlantActions (GameModel sun [] []) = (GameModel sun [] [])
performPlantActions model@(GameModel sun plantList zombieList) = (GameModel (produceSun model) (killCherryBombs model) (cherryBombAction plantList (peashooterAction model)))