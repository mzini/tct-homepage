#!/bin/bash

WWW=/var/www/html/tct3/
HP=".cabal-sandbox/bin/homepage"

pushd `dirname $0`
  cabal install || exit 1
  $HP clean
  $HP build  
popd

sudo rm -r $WWW/*
sudo mkdir -p $WWW/session
sudo chown www-data:www-data $WWW/session
sudo cp -r _site/* $WWW/
