#!/usr/bin/env bash
#
# Script to generate CHM docs. 
# (C) 2009 Marco van de Voort Initial version
# 
# Needs more errorchecking.

export FPCSRCDIR=/fpc/fpc
make HTMLFMT=chm html CSSFILE=$FPCSRCDIR/utils/fpdoc/fpdoc.css FPDOC=fpdoc FPCSRCDIR=$FPCSRCDIR |tee buildlog.txt

fpc relinkdocs.pp
fpc compilelatexchm.pp
fpc gentoc

./relinkdocs
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
./compilelatexchm prog "Programmer's Guide"
./compilelatexchm user "User's Guide"
./compilelatexchm ref "Reference Guide"  ref/ref.kwd
./compilelatexchm fpdoc "FPDoc documentation"
 
./gentoc . .
