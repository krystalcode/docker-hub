module DockerHub.Pull ( pull ) where

import DockerHub.Data
import DockerHub.Process (lazyProcess)

-- Public API/types.

-- Trigger pull for a list of repositories.
pull :: String -> [Repository] -> IO [(String)]
pull tag repositories = mapM (pull' tag) repositories

-- Functions/types for internal use.

-- Trigger pull for an individual repository.
pull' :: String -> Repository -> IO (String)
pull' "" repository = lazyProcess command "."
    where command = "docker pull " ++ (name repository)

pull' tag repository = lazyProcess command "."
    where command = "docker pull " ++ (name repository) ++ ":" ++ tag
