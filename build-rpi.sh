#!/usr/bin/env bash
set -e

docker run \
  -v stack-root-cache:/root/.stack \
  -v "$(pwd)":/home/build \
  tgolson/rpi-haskell-with-deps:8.0.1 \
  /bin/sh -c '
    set -e
    cd /home/build

    echo "Building executable..."
    stack build

    mkdir -p out
    echo "Copying executables..."
    cp "$(stack path --local-install-root)"/bin/* out/

    echo "Build finished! Executables available in \"out/\""'
