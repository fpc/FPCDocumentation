#!/usr/bin/env bash
#
# Script to generate CHM docs. 
# (C) 2009 Marco van de Voort Initial version
# 
# Needs more errorchecking.

export FPCSRCDIR=/fpc/fpctrunk
make HTMLFMT=chm html CSSFILE=$FPCSRCDIR/utils/fpdoc/fpdoc.css FPDOC=fpdoc FPCSRCDIR=$FPCSRCDIR 2>&1 |tee buildlog.txt

cd src
fpc relinkdocs.pp
fpc compilelatexchm.pp
fpc gentoc
cd ..

cd dist/html
../../src/relinkdocs
rm -rf prog-old
rm -rf ref-old
rm -rf user-old
mv prog prog-old
mv ref ref-old
mv user user-old
mv prog-fixed  prog
mv ref-fixed ref
mv user-fixed user

cp prog-old/*.png prog
cp prog-old/*.kwd prog
cp prog-old/*.css prog
cp user-old/*.png user
cp user-old/*.kwd user
cp user-old/*.css user
cp ref-old/*.png ref
cp ref-old/*.kwd ref
cp ref-old/*.css ref
cp -r ../../pics prog/
cp -r ../../pics user/
cp -r ../../pics .
../../src/compilelatexchm prog "Programmer's Guide"
../../src/compilelatexchm user "User's Guide"
../../src/compilelatexchm ref "Reference Guide"  ref/ref.kwd
../../src/compilelatexchm fpdoc "FPDoc documentation"
 
../../src/gentoc . .

rm fpdoc.kwd user.kwd prog.kwd
