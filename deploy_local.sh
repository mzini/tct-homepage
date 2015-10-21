#!/bin/bash

stack install
~/.local/bin/homepage clean
~/.local/bin/homepage build
sudo cp -r _site/* /var/www/html/tct3/
