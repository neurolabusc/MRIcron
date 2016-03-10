#!/bin/sh





find . -name \*.dcu -type f -delete
rm  -r *.a
rm  -r *.o
rm  -r *.ppu
rm -r *.bak
rm mricron

cd ./common
rm  -r *.a
rm  -r *.o
rm  -r *.ppu
rm -r *.bak
cd ..


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


