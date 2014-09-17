{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Snap.Snaplet.Mandrill
  ( initMandrill
  , runMandrill
  , mkAmqpConn
  , Mandrill    (..)
  , HasMandrill (..)
  ) where

import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Data.Configurator
import           Data.Configurator.Types
import           Network.API.Mandrill
import           Paths_snaplet_mandrill
import           Snap.Snaplet

-------------------------------------------------------------------------------
newtype Mandrill = Mandrill { token :: MandrillKey }

-------------------------------------------------------------------------------
class MonadIO m => HasMandrill m where
    getMandrill :: m MandrillKey

instance HasAmqpConn (Handler b Mandrill) where
    getMandrill = get

instance MonadIO m => HasMandrill (ReaderT MandrillKey m) where
    getMandrill = ask

-- | Initialize the Mandrill Snaplet.
initMandrill :: SnapletInit b Mandrill
initMandrill = makeSnaplet "mandrill" description datadir $ do
    conf  <- getSnapletUserConfig
    token <- liftIO $ require conf "token"

    return $ Mandrill token

  where
    description = "Snaplet for Mandrill library"
    datadir = Just $ liftM (++"/resources/mandrill") getDataDir

-------------------------------------------------------------------------------
-- | Runs an Mandrill action in any monad with a HasAmqpConn instance.
-- runAmqp :: (HasAmqpConn m) => (AmqpC -> b) -> m b
-- runAmqp action = getAmqpConn >>= return . action
