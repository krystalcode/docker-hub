module Main where

{-
  @Issue(
    "Check if we could limit what is imported from System.Environment"
    type="improvement"
    priority="low"
  )
-}
import DockerHub.Build
import DockerHub.Config
import DockerHub.Data
import System.Environment

-- Main entry point for the program.
-- Takes the path (folder) where the docker-hub.yml file is located and a list
-- of DockerHub repositories, and issues requests to DockerHub to build these
-- repositories.
main :: IO [()]
main = do
    (path:repos) <- getArgs
    config <- load $ path ++ "/docker-hub.yml"
    let validRepos = reposFromConfig config repos
    build validRepos

-- Functions/types for internal use.

-- Gets a Config value and a list of repository names and returns a list of
-- Repository values (as defined in the Config value) that exist in both the
-- Config value and the list of repository names.
reposFromConfig :: Config -> [String] -> [Repository]
reposFromConfig config requestedRepos = filter (isRequestedRepo requestedRepos) configRepos
                                        where configRepos = repositories config
                                              isRequestedRepo requestedRepos configRepo = (name configRepo) `elem` requestedRepos
