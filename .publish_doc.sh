#!/bin/sh

set -e

echo "Working on branch $TRAVIS_BRANCH"
echo "R version: $TRAVIS_R_VERSION"

[ -z "${GITHUB_PAT}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0
[ "${TRAVIS_R_VERSION}" != "release" ] && exit 0

git config user.name "rapporter-travis"
git config user.email "travis"

ls 

make pdf
make README.md
git add *.pdf README.md
git commit -m"Automatic deployment after $TRAVIS_COMMIT [ci skip]" || true
git push -q origin master

#~ git clone -b master https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output
#~ cd book-output
#~ cp -r ../_book/* ./
#~ git add --all *
#~ git commit -m"Automatic deployment after $TRAVIS_COMMIT [ci skip]" || true
#~ git push -q origin master
