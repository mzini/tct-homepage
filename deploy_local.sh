#!/bin/bash

WWW=/var/www/html/tct3/
HP=~/.local/bin/homepage
stack install
$HP clean
$HP build

# sudo rm -r $WWW/*
sudo mkdir -p $WWW/session
sudo chown www-data:www-data $WWW/session
sudo cp -r _site/* $WWW/
