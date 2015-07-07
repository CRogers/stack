{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Stack.Image
       (image, imgCmdName, imgDockerCmdName, imgOptsFromMonoid,
        imgDockerOptsFromMonoid, imgOptsParser, imgDockerOptsParser)
       where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Options.Applicative
import           Path
import           Path.IO
import           Stack.Build.Source
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import           System.Directory
import           System.IO.Temp
import           System.Process.Read

stageExesInDir config pkgs dir = do
    dirPath <- parseAbsDir dir
    let binPath = dirPath </>
            $(mkRelDir "usr") </>
            $(mkRelDir "local") </>
            $(mkRelDir "bin")
    liftIO
        (createDirectoryIfMissing
             True
             (toFilePath binPath))
    forM_
        (concatMap (Set.toList . packageExes) pkgs)
        (\exe ->
              do exePath <-
                     parseRelFile
                         (T.unpack exe)
                 copyFile
                     (toFilePath
                          (configLocalBin config </> exePath))
                     (toFilePath
                          (binPath </> exePath)))

syncAddContentToDir config dir = return ()
    -- TODO copy all the static content to the image staging area
    -- let imgCfg = imgDocker (configImage config)
    --     imgAdd = fmap imgDockerAdd imgCfg
    -- return ()

createDockerImage config dir = do
    let imgCfg = imgDocker (configImage config) :: Maybe ImageDockerOpts
        imgBase = maybe Nothing imgDockerBase imgCfg :: Maybe String
    case imgBase of
        Nothing -> throwM (StackImageException "nope")
        Just base -> do
            dirPath <- parseAbsDir dir
            writeFile
                (toFilePath
                     (dirPath </>
                      $(mkRelFile "Dockerfile")))
                (unlines ["FROM " ++ base, "ADD ./ /"])
            void
                (readProcess
                     "docker"
                     ["build", "-t", "stack-docker:latest", dir]
                     "")

extendDockerImageWithEntrypoint config dir = do
    -- mapM_ (\eps -> do return ()) <$> imgDockerEntrypoints <$> imgDocker (configImage config)
    return ()
    -- TODO extend the base imae for each entrypoint

image :: forall env (m :: * -> *).
         (HasConfig env, HasEnvConfig env, HasBuildConfig env, HasTerminal env, MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadLogger m, MonadReader env m)
      => ImageDockerOptsMonoid -> m ()
image _dockerOptsMonoid = do
    -- TODO factor in command line opts - look at cleanupOptions in Main.hs
    config <- asks getConfig
    econfig <- asks getEnvConfig
    bconfig <- asks getBuildConfig
    tempDirFP <- liftIO getTemporaryDirectory
    pkgs <- projectPkgs econfig bconfig
    liftIO
        (withTempDirectory
             tempDirFP
             "stack"
             (\dir ->
                   do stageExesInDir config pkgs dir
                      syncAddContentToDir config dir
                      createDockerImage config dir
                      extendDockerImageWithEntrypoint config dir))

projectPkgs :: forall (m :: * -> *).
               (MonadLogger m, MonadCatch m, MonadIO m)
            => EnvConfig -> BuildConfig -> m [Package]
projectPkgs econfig bconfig = forM
        (Map.toList
             (bcPackages bconfig))
        (\(dir,_wanted) ->
              do cabalfp <- getCabalFileName dir
                 name <- parsePackageNameFromFilePath cabalfp
                 let cfg = PackageConfig
                         { packageConfigEnableTests = True
                         , packageConfigEnableBenchmarks = True
                         , packageConfigFlags = localFlags mempty bconfig name
                         , packageConfigGhcVersion = envConfigGhcVersion econfig
                         , packageConfigPlatform = configPlatform
                               (getConfig bconfig)
                         }
                 readPackage cfg cabalfp)

imgCmdName :: String
imgCmdName = "image"

imgDockerCmdName :: String
imgDockerCmdName = "docker"

imgOptsParser :: Parser ImageOptsMonoid
imgOptsParser = ImageOptsMonoid <$>
    pure Nothing <*>
    optional
        (subparser
             (command
                  "docker"
                  (info imgDockerOptsParser (progDesc "Create a docker image"))))

imgDockerOptsParser :: Parser ImageDockerOptsMonoid
imgDockerOptsParser = ImageDockerOptsMonoid <$>
    pure Nothing <*>
    optional
        (option
             str
             (long (imgDockerCmdName ++ "-" ++ T.unpack imgDockerBaseArgName) <>
              metavar "NAME" <>
              help "Docker base image name")) <*>
    pure Nothing <*>
    pure Nothing

imgOptsFromMonoid :: ImageOptsMonoid -> ImageOpts
imgOptsFromMonoid ImageOptsMonoid{..} = ImageOpts
    { imgDocker = imgDockerOptsFromMonoid <$> imgMonoidDocker
    }

imgDockerOptsFromMonoid :: ImageDockerOptsMonoid -> ImageDockerOpts
imgDockerOptsFromMonoid ImageDockerOptsMonoid{..} = ImageDockerOpts
    { imgDockerBase = emptyToNothing imgDockerMonoidBase
    , imgDockerEntrypoints = emptyToNothing imgDockerMonoidEntrypoints
    , imgDockerAdd = fromMaybe Map.empty imgDockerMonoidAdd
    }
    where emptyToNothing Nothing = Nothing
          emptyToNothing (Just s)
              | null s =
                  Nothing
              | otherwise =
                  Just s

data StackImageException =
    StackImageException { err :: String }
    deriving (Typeable)

instance Exception StackImageException

instance Show StackImageException where
    show (StackImageException e) = show e
