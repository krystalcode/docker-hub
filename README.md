# Docker Hub client #

### What is this repository for? ###

This repository aims to function as a Docker Hub client that facilitates common tasks when working with images stored in the Docker Hub registry. At the moment all it does is to ease triggering image buidls without having to do it through a web browser. Say you develop the Docker image krystalcode/f_haskell_stack and you store your image's dockerfile in a git repo. You make changes to it and you want to manually trigger a rebuild the image after the changes.

```
git push
docker-hub --config-file="~/.docker/docker-hub.yaml" krystalcode/f_haskell_stack
```

This is useful when you store multiple dockerfiles in a single git repository, or in a repository that does not have as its single purpose to host a single dockefile. In such cases, you may disable automatic building in Docker Hub so that builds are not triggered due to unrelated changes.

### How do I get set up? ###

First, compile the project using Haskell Stack and place the executable in your path. Executables might be provided as releases, if there is interest.

Create a configuration file under ~/.docker/docker-hub.yaml. The file should have the following format:

```
# ~/.docker/docker-hub.yaml
base_url: https://registry.hub.docker.com/u/

repositories:
  - name:          namespace/image-A
    trigger_token: the-trigger-token-for-image-A
  - name:          namespace/image-B
    trigger_token: the-trigger-token-for-image-B
  - name:          namespace/image-C
    trigger_token: the-trigger-token-for-image-C
```

You may now trigger a build by issuing the following command:

```
docker-hub --config-file="~/.docker/docker-hub.yaml" krystalcode/f_haskell_stack
```
