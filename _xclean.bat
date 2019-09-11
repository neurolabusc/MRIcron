#!/bin/sh

find . -name \*.dcu -type f -delete
rm -rf lib
rm -rf backup
rm  -r .DS_Store
rm  -r *.a
rm  -r *.o
rm  -r *.ppu
rm -r *.bak
rm mricron

cd ./rgb
rm  -r *.a
rm  -r *.o
rm  -r *.ppu
rm -r *.bak
cd ..

cd ./fpmath
rm  -r *.a
rm  -r *.o
rm  -r *.ppu
rm -r *.bak
cd ..

cd ./common
rm  -r *.a
rm  -r *.o
rm  -r *.ppu
rm -r *.bak
cd ..
rm -rf ./common/backup


cd ./dcm2nii
rm ./dcm2niigui
rm ./dcm2nii
rm  -r *.a
rm  -r *.o
rm  -r *.ppu
rm -r *.bak
rm -rf dcm2niigui.app
cd ..

cd ./npm
rm ./npm
rm  -r *.o
rm  -r *.ppu
rm -r *.bak


