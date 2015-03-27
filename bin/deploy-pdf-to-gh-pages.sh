#!/bin/bash
set -e # exit with nonzero exit code if anything fails

# This script is inspired by the one at:
# https://gist.github.com/domenic/ec8b0fc8ab45f39403dd
mkdir temp
cd temp

git init
git config --global user.name 'Travis CI'
git config --global user.email 'niranjanv+github@gmail.com'
git config --global push.default simple

git clone https://github.com/niranjv/ucscamsms2015.git
cd ucscamsms2015
git checkout gh-pages
cp ../../output/report.pdf .
git commit report.pdf -m "Add report.PDF"
git push "https://${GH_TOKEN}@github.com/niranjv/ucscamsms2015.git"

cd ../..
