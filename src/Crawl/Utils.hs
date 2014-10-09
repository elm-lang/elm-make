{-# LANGUAGE FlexibleContexts #-}
module Crawl.Utils where

import Control.Monad.Error (MonadIO, liftIO)
import System.Directory (getCurrentDirectory, setCurrentDirectory)


within :: (MonadIO m) => FilePath -> m a -> m a
within directory command =
    do  root <- liftIO getCurrentDirectory
        liftIO (setCurrentDirectory directory)
        result <- command
        liftIO (setCurrentDirectory root)
        return result
