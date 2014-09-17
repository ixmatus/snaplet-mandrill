{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Snap.Snaplet.Mandrill
  ( initMandrill
  , runMandrill
  , MandrillState (..)
  , HasMandrill   (..)
  ) where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Data.Configurator
import           Network.API.Mandrill       (MandrillKey)
import           Paths_snaplet_mandrill
import           Snap.Snaplet

-------------------------------------------------------------------------------
newtype MandrillState = MandrillState { token :: MandrillKey }

-------------------------------------------------------------------------------
class MonadIO m => HasMandrill m where
    getMandrill :: m MandrillKey

instance HasMandrill (Handler b MandrillState) where
    getMandrill = gets token

instance MonadIO m => HasMandrill (ReaderT MandrillKey m) where
    getMandrill = ask

-- | Initialize the Mandrill Snaplet.
initMandrill :: SnapletInit b MandrillState
initMandrill = makeSnaplet "mandrill" description datadir $ do
    conf <- getSnapletUserConfig
    return =<< MandrillState <$> (liftIO $ require conf "token")

  where
    description = "Snaplet for Mandrill library"
    datadir = Just $ liftM (++"/resources/mandrill") getDataDir

-------------------------------------------------------------------------------
-- | Runs an Mandrill action in any monad with a HasAmqpConn instance.
runMandrill :: (HasMandrill m) => (MandrillKey -> b) -> m b
runMandrill action = getMandrill >>= return . action
