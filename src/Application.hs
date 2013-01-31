{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Control.Monad.State (get)
import Data.Pool
import Database.HDBC.MySQL
import Snap.Snaplet.Hdbc
------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _hdbc :: Snaplet (HdbcSnaplet Connection Pool)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasHdbc (Handler b App) Connection Pool where
  getHdbcState = with hdbc get

------------------------------------------------------------------------------
type AppHandler = Handler App App


