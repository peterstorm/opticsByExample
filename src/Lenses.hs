{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, InstanceSigs, LambdaCase #-}

module Lenses (main) where

import Control.Lens
import Control.Lens.Unsound (lensProduct)
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- 3.3 Lenses and Records

data Ship = Ship
  { _name    :: String
  , _numCrew :: Int }
  deriving (Show)

makeLenses ''Ship
-- Building a lens for a Ship data structure

getNumCrew :: Ship -> Int
getNumCrew = _numCrew

setNumCrew :: Ship -> Int -> Ship
setNumCrew s i = s { _numCrew = i }

numCrew' :: Lens' Ship Int
numCrew' = lens getNumCrew setNumCrew

-- 'lens's is a helper function from the Lens library, to create a lens from a getter and a setter

-- [ Excersise ] - implement the lens name :: Lens' Ship String

getName :: Ship -> String
getName = _name

setName :: Ship -> String -> Ship
setName s n = s { _name = n }

name' :: Lens' Ship String
name' = lens getName setName

-- 3.4 Limitations
-- [ Excersise ] - Is it a Lens?

conditional :: Lens' (Bool, a, a) a
conditional = lens (\case
                      (True, a, _) -> a
                      (False, _, a) -> a
                   )
                   (\(bool, a, a') a'' -> if bool then (bool, a'', a') else (bool, a, a''))

-- 3.5 Lens Laws
-- Case Study: Lens Product

type UserName = String
type UserId   = String

data Session = Session
  { _userId      :: UserId
  , _userName    :: UserName
  , _createdTime :: String
  , _expiryTime  :: String
  } deriving (Show, Eq)

makeLenses ''Session

userInfo :: Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName

-- Write a lawful lens for the following:

data Builder = Builder
  { _context :: [String]
  , _build   :: [ String ] -> String
  }

builder :: Lens' Builder String
builder = lens getter setter
  where getter (Builder c f) = f c
        setter (Builder c f) newVal =
          Builder c $ \c' ->
            if c' == c
               then newVal
               else f c'

-- 3.6 Virual Fields

data Temperature = Temperature
  { _location :: String
  , _kelvin  :: Float
  } deriving (Show)

makeLenses ''Temperature

-- make our 'Murican colleagues able to understand this weird temperature

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = c * (9/5) + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5/9)

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
  where getter = celsiusToFahrenheit . view celsius
        setter t f = set celsius (fahrenheitToCelsius f) t

-- this fahrenheit lens we created is a virtual field that allows us to interact with our datastructure, in new ways.

-- Let us now imagine we wanted to change our Temperatur ADT to take kelvin instead of celcius, we do the following, pretty painless refactoring

celsius :: Lens' Temperature Float
celsius = lens getter setter
  where getter = subtract 273.15 . view kelvin
        setter t c = set kelvin (c + 273.15) t

-- all over previews code (with few modifications) still works as intended!

-- [ Excersise ] - Virtual Fields

data User = User
  { _uFirstName :: String
  , _uLastName  :: String
  , _uEmail     :: String
  } deriving (Show)

makeLenses ''User

-- delete the uUsername field from the User, but build a new lens so that the original code can still be used

uUsername :: Lens' User String
uUsername = lens getter setter
  where getter = view uEmail
        setter u s = set uEmail s u

-- apparently another solution is also this!
--
uUsername' :: Lens' User String
uUsername' = uEmail

-- write a fullName lens

fullName :: Lens' User String
fullName = lens getter setter
  where getter u = view uFirstName u <> " " <> view uLastName u
        setter u s =
          let userWithFirstName = set uFirstName (head $ words s) u
           in set uLastName (unwords . tail . words $ s) userWithFirstName

-- 3.7 Data correction and maintaining invariants

data Time = Time
  { _hours :: Int
  , _mins  :: Int
  } deriving (Show)

clamp :: Int -> Int -> Int -> Int
clamp minVal maxVal = min maxVal . max minVal

hours :: Lens' Time Int
hours = lens getter setter
  where getter (Time h _) = h
        setter (Time _ m) newHours = Time (clamp 0 23 newHours) m

mins :: Lens' Time Int
mins = lens getter setter
  where getter (Time _ m) = m
        setter (Time h _) newMins = Time h (clamp 0 59 newMins)

-- Theses a not lawful lenses as they do not obey the set/get law.

main :: IO ()
main = do
  putStrLn "Hello from Lenses.hs"
  let ship = Ship "BoatyMcBoatFace" 1
  let shipName = view name' ship
  let shipCrew = view numCrew' ship
  putStrLn "The name of the ship, after \"viewing\" it:"
  putStrLn shipName
  putStrLn "The number of crew in the ship:"
  print shipCrew
  let newShip = set numCrew' 42 ship
  putStrLn "The new ship after \"setting\" the number of crew with an operator:"
  print newShip
  let modifiedShip = over numCrew' (*2) newShip
  putStrLn "The modified ship, after using \"over\" to be able to run a function on the field we are modifying:"
  print modifiedShip
  putStrLn "Session from the 3.5 case study, lensProduct."
  let session = Session "1234" "Peter Storm" "290885" "290982"
  print session
  let info = view userInfo session
  putStrLn "The result of viewing the lensProduct of the session"
  print info
  putStrLn "Builder excersice:"
  let stringBuilder = Builder ["a", "b", "c"] concat
  print $ view builder stringBuilder
  let newBuilder = set builder "abcd" stringBuilder
  print $ view builder $ newBuilder{ _context = ["a", "b", "c"] }
  putStrLn "Temperature in celsius:"
  let temp = Temperature "Berlin" (7.0 + 273.15)
  print $ view celsius temp
  putStrLn $ "Temperature in kelvin: " <> show (view kelvin temp)
  putStrLn "Set temperature to x degrees:"
  print $ set celsius 14.5 temp
  putStrLn "Bump the origional temp by 10 degrees:"
  print $ over celsius (+10) temp
  putStrLn "Viewing temperature in fahrenheit with the new fahrenheit lens:"
  putStrLn $ "Temperature in celsius: " <> show (view celsius temp)
  print $ view fahrenheit temp
  putStrLn "Adding 18 to the amount of f to the temperature using over:"
  putStrLn $ "Temperature in celsius: " <> show (view celsius temp)
  print $ over fahrenheit (+18) temp
  putStrLn "User virtual fields excersice:"
  let user = User "Peter" "Hansen" "pkshdk@gmail.com"
  print user
  putStrLn $ "Users username: " <> show (view uUsername user)
  let newUser = set uUsername "johanna@gmail.com" user
  print newUser
  putStrLn $ "Users username: " <> show (view uUsername' user)
  let newUser' = set uUsername' "johanna@gmail.com" user
  print newUser'
  putStrLn $ "Users full name: " <> show (view fullName user)
  putStrLn $ "Setting a different full name on user: " <> show (set fullName "Johanna Tummasardottir" user)
  let time = Time 3 10
  putStrLn $ "Time: " <> show time
  putStrLn $ "New time using our clamped lens: " <> show (set mins (-10) $ set hours 40 time)

