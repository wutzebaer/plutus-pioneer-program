# Lecture 1

## Build Plutus

(Re)Install Ubuntu, use the one **without** version number

 If reinstallation fails, it helps to download the Ubuntu_2004.2020.424.0_x64.appx manually, install and uninstall it, after that installation via ms-store works again

 Open a Linux shell and create a folder in your linux-home folder, avoid /mnt/c because windows dirs are much slower under wsl and cause other problems while compiling

    mkdir ~/workspace
    cd ~/workspace

Clone pioneer-progam and plutus into your ~\workspace folder

    git clone https://github.com/input-output-hk/plutus-pioneer-program
    git clone https://github.com/input-output-hk/plutus

Go into plutus-pioneer-program and find commit correct commit id

    cd plutus-pioneer-program
    head -30 code/week01/cabal.project
    output: 3746610e53654a1167aeb4c6294c6096d16b0502
    cd ..

checkout the correct plutus version

    cd plutus
    git checkout 3746610e53654a1167aeb4c6294c6096d16b0502

install nix

    curl -L https://nixos.org/nix/install | sh
    . ~/.nix-profile/etc/profile.d/nix.sh

build plutus, most other errors occour by low memory, try stopping your windows docker etc.

    nix build -f default.nix plutus.haskell.packages.plutus-core.components.library

now wait some hours ....

## Build the week01 code

    nix-shell
    # wait again
    cd ~/workspace/plutus-pioneer-program/code/week01
    cabal update 
    cabal build

Open a second wsl-linux shell in the c:\workspace\plutus dir and start 
the playground backend

    cd ~/workspace/plutus
    nix-shell
    cd plutus-playground-client
    plutus-playground-server

Open a third wsl-linux shell in the c:\workspace\plutus dir and start the playground frontend

    cd ~/workspace/plutus
    nix-shell
    cd plutus-playground-client
    npm run start

open frontend in browser: https://localhost:8009/

## BONUS

Run the plutus compile environment in a docker container and use it for **VSCode** to compile things, requires that docker is installed on your system:

  * Get VSCode for Windows
    * Install the "Remote - Containers" extension
  * goto docker windows and able docker in ubuntu distri
    * settings -> 
    * resources -> 
    * wsl integration -> 
    * enable ubuntu -> 
    * click apply and restart

Open a fourth nix-shell shell and create the docker image

    cd ~/workspace/plutus
    sudo docker load < $(nix-build default.nix -A devcontainer)
    cd ~/workspace
    git clone https://github.com/input-output-hk/plutus-starter
    cd plutus-starter
    code .

  * edit devcontainer.json and change "remoteUser": "plutus", to "remoteUser": "root", --- somehow it is screwed dunno
  * click reopen in container on the bottom

open plutus-pioneer-program with devcontainer

    cp -r ~/workspace/plutus-starter/.devcontainer/ ~/workspace/plutus-pioneer-program/
    cd ~/workspace/plutus-pioneer-program/
    code .
    wait an hour until "Processing"-notification has finished

open console (strg+ö)

    cd code/week01/
    cabal update
    cabal build