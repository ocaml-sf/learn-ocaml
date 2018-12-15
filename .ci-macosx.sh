#!/bin/sh

curl -LO https://raw.githubusercontent.com/GiovanniBussi/macports-ci/master/macports-ci
sudo source ./macports-ci install
sudo port install opam
opam --version
