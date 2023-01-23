module Homework10 where

--1. feladat

data User = User { username :: String
                , password :: String
                , email :: String
                , balance :: Int
                , pin :: Int
                } deriving(Show, Eq)

--Teszt userek
user1 :: User
user1 = User { username = "user1", password = "pass123", email = "example@email.com", balance = 12500, pin = 1111}

user2 :: User
user2 = User { username = "user2", password = "123password", email = "example2@email.com", balance = 150000, pin = 4578}

user3 :: User
user3 = User { username = "user3", password = "123passw0rd", email = "example22@email.com", balance = 3150000, pin = 4578}

--2. feladat

transactionPossible :: User -> Int -> Bool
transactionPossible user price = (balance user) >= price

--3. feladat

login :: User -> String -> String -> Int -> Bool
login user userName psw pinCode = (username user) == userName && (password user) == psw && (pin user) == pinCode

--4. feladat

data Date = Date { year :: Int, month :: Int, day :: Int } deriving (Show, Ord, Eq)

--Teszt dátumok
date1 :: Date
date1 = Date { year = 2021, month = 9, day = 29 }

date2 :: Date
date2 = Date { year = 2001, month = 6, day = 11 }

date3 :: Date
date3 = Date { year = 2000, month = 1, day = 1 }

date4 :: Date
date4 = Date { year = 2001, month = 6, day = 12 }

--5. feladat

inSeptember :: Date -> Bool
inSeptember date = (month date) == 9

--6. feladat

mostRecent :: Date -> Date -> String
mostRecent date1 date2
    | date1 > date2 = "Az elso datum van kesobb"
    | date1 < date2 = "A masodik datum van kesobb"
    | otherwise = "A ket datum megegyezik"

--7. feladat

applyToEvens :: (Int -> Int) -> [Int] -> [Int]
applyToEvens _ [] = []
applyToEvens func (x:xs)
    | even x = func x : applyToEvens func xs
    | otherwise = x : applyToEvens func xs

--8. feladat

data Vehicle = Bike | Scooter | Car {consumption :: Float} | Motorcycle {consumption :: Float} deriving(Show, Eq)

v1 :: Vehicle
v1 = Scooter

v2 :: Vehicle
v2 = Bike

v3 :: Vehicle
v3 = Car 7.3

v4 :: Vehicle
v4 = Motorcycle 2.9

--9. feladat

costOfTravel :: Vehicle -> Float
costOfTravel vehicle
    | vehicle == Bike = 0.0
    | vehicle == Scooter = 0.0
    | otherwise = (consumption vehicle) * 480