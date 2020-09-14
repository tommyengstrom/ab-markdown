#! /bin/bash

stack test --fast
export TmpDir=$(mktemp -d)
export TmpTestDir=$TmpDir/tests
mkdir $TmpTestDir
echo --------------------------------------------------------------------------------
echo -- Running elm client tests in $TmpTestDir
echo --------------------------------------------------------------------------------
cp app/elm-client/* $TmpDir -R
stack exec gen-client -- --output $TmpTestDir --includeTests
cd $TmpDir
elm-test
