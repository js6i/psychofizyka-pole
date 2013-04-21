{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import System.Random
import Control.Lens
import Control.Monad.Reader
import Data.ByteString
import Data.SafeCopy
import Data.Typeable
import Data.Int
import Data.Char
import Data.Aeson

import qualified Data.Text as T
import qualified Data.Map as M

import Snap
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.AcidState

------------------------------------------------------------------------------
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _ -> Nothing

capitalize (x:xs) = toUpper x : xs
capitalize x = Prelude.map toUpper x

------------------------------------------------------------------------------
type Pixels = Int

data Shape = Square | Circle
  deriving (Show, Eq, Ord, Typeable, Enum, Bounded, Read)

deriveSafeCopy 0 'base ''Shape

instance Random Shape where
  randomR (a,b) g = (\(i,x) -> (toEnum i, x)) $ randomR
                      (fromEnum a, fromEnum b) g
  random g        = randomR (minBound, maxBound) g

data FillColor = Red | Green | Blue | White | Black | Yellow | Cyan | Magenta
  deriving (Show, Eq, Ord, Typeable, Enum, Bounded, Read)

deriveSafeCopy 0 'base ''FillColor

instance Random FillColor where
  randomR (a,b) g = (\(i,x) -> (toEnum i, x)) $ randomR
                      (fromEnum a, fromEnum b) g
  random g        = randomR (minBound, maxBound) g

data Record = Record
  { _shape   :: Shape
  , _color   :: FillColor
  , _ratio   :: Pixels
  , _initial :: Pixels
  , _result  :: Pixels
  } deriving (Typeable, Show)

makeLenses ''Record

deriveSafeCopy 0 'base ''Record

type Records = M.Map T.Text [Record]

data PersistentState = PersistentState
  { _records :: Records
  } deriving (Typeable, Show)
  
makeLenses ''PersistentState

deriveSafeCopy 0 'base ''PersistentState

addUser :: T.Text -> Update PersistentState ()
addUser uid = modify (over records (M.insert uid []))

addRecord :: T.Text -> Record -> Update PersistentState ()
addRecord uid r = modify (over records (M.adjust (r :) uid))

getRecord :: T.Text -> Query PersistentState (Maybe [Record])
getRecord uid = asks (M.lookup uid . _records)

getRecords :: Query PersistentState Records
getRecords = asks _records

makeAcidic ''PersistentState ['addUser, 'getRecords, 'getRecord, 'addRecord]

------------------------------------------------------------------------------
instance ToJSON Shape where
  toJSON = toJSON . show

instance ToJSON FillColor where
  toJSON = toJSON . fmap toLower . show

instance ToJSON Record where
  toJSON r = toJSON . M.fromList $ [ ("shape",   toJSON $ _shape   r)
                                   , ("color",   toJSON $ _color   r)
                                   , ("ratio",   toJSON $ _ratio   r)
                                   , ("initial", toJSON $ _initial r)
                                   , ("result",  toJSON $ _result  r)
                                   ]

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _acid  :: Snaplet (Acid PersistentState)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasAcid App PersistentState where
    getAcidStore = view (acid . snapletValue)

------------------------------------------------------------------------------
type AppHandler = Handler App App


