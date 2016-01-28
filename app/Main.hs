{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Data
import Data.Typeable
import qualified DockerHub.Build as B
import qualified DockerHub.Config as C
import qualified DockerHub.Data as D
import qualified DockerHub.Pull as P
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)

-- Main entry point for the program.
-- Takes the path (folder) where the docker-hub.yml file is located and a list
-- of DockerHub repositories, and issues requests to DockerHub to build these
-- repositories.

main = do
    args <- getArgs
    cmd <- getCmd args
    case cmd of
        CmdBuild{} -> runBuild cmd
        CmdPull{} -> runPull cmd

-- Functions/types for internal use.

-- Run the "build" command.
-- Issues a request to DockerHub to build the requested repositories.
runBuild :: Cmd -> IO ()
runBuild cmd = do
    config <- C.load $ cmdConfigFile cmd
    let reposToBuild = reposFromConfig config $ cmdRepositories cmd
    responses <- B.build reposToBuild
    print responses

-- Run the "pull" command.
runPull :: Cmd -> IO ()
runPull cmd = do
    config <- C.load $ cmdConfigFile cmd
    let tag = cmdTag cmd
    let requestedRepos = cmdRepositories cmd
    let reposToPull = if (null requestedRepos) then error "You must specify at least one repository to pull."
                                               else reposFromConfig config requestedRepos
    if (null reposToPull) then error "None of the specified repositoried have a corresponding configuration."
                          else do
                               results <- P.pull tag reposToPull
                               print results

-- The following command line handling system has roughly been taken from HLint
-- library.
getCmd :: [String] -> IO Cmd
getCmd args = withArgs (map f args) $ cmdArgsRun mode
    where f x = if x == "-?" || x == "--help" then "--help=all" else x

data Cmd
    = CmdBuild
        { cmdConfigFile :: FilePath
        , cmdTag :: String
        , cmdRepositories :: [String] }
    | CmdPull
        { cmdConfigFile :: FilePath
        , cmdTag :: String
        , cmdRepositories :: [String] }
    deriving (Data, Typeable, Show)

mode = cmdArgsMode $ modes
    [ CmdBuild
        { cmdConfigFile = name'' "config-file" "~/.docker/docker-hub.yaml" &= typFile &= help "The configuration file that contains information about the repositories. It defaults to ~/.docker/docker-hub.yaml."
        , cmdTag = name'' "tag" "latest" &= typ "TAG" &= help "The docker tag to build. It defaults to 'latest'."
        , cmdRepositories = def &= args &= typ "REPOSITORIES"
        } &= explicit &= name "build"
    , CmdPull
        { cmdConfigFile = name'' "config-file" "~/.docker/docker-hub.yaml" &= typFile &= help "The configuration file that contains information about the repositories. It defaults to ~/.docker/docker-hub.yaml."
        , cmdTag = name'' "tag" "latest" &= typ "TAG" &= help "The docker tag to build. It defaults to 'latest'."
        , cmdRepositories = def &= args &= typ "REPOSITORIES"
        } &= explicit &= name "pull"
    ] &= program "docker-hub" &=verbosity &= summary "A command line utility that eases triggering builds and pulls of repositories in the DockerHub registry."
    where name' optionName = def &= explicit &= name optionName
          name'' optionName optionDefault = optionDefault &= explicit &= name optionName

-- Gets a Config value and a list of repository names and returns a list of
-- Repository values (as defined in the Config value) that exist in both the
-- Config value and the list of repository names.
reposFromConfig :: C.Config -> [String] -> [D.Repository]
reposFromConfig config requestedRepos = filter (isRequestedRepo requestedRepos) configRepos
    where configRepos = C.repositories config
          isRequestedRepo requestedRepos configRepo = (D.name configRepo) `elem` requestedRepos
