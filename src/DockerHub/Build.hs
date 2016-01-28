module DockerHub.Build ( build ) where

import DockerHub.Data
import Network.Curl (curlPost)

-- Public API/types.

-- Trigger build for a list of repositories.
build :: [Repository] -> IO [()]
build repositories = mapM build' repositories

-- Functions/types for internal use.

-- Trigger build for an individual repository.
build' :: Repository -> IO ()
build' repository = curlPost requestUrl []
    where requestUrl = "https://registry.hub.docker.com/u/" ++ repoName ++ "/trigger/" ++ repoTriggerToken ++ "/"
          repoName = name repository
          repoTriggerToken = triggerToken repository
