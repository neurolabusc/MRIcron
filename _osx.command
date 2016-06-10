#!/bin/sh
#This script builds a distribution on Chris Rorden's personal computer.
#  to build your own version you typically run
#    lazbuild -B filename.lpr

cd /Users/rorden/Documents/pas/mricron

chmod 777 ./_xclean.bat
./_xclean.bat
cp ./common/notgui.inc ./common/isgui.inc
#lazbuild ./dcm2nii/dcm2nii.lpr --cpu=x86_64 --compiler="/usr/local/bin/ppcx64"
#Current FPC 3.0.0 can not compile on OSX 10.11 El Capitan, so use 3.1.1
lazbuild ./dcm2nii.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/lib/fpc/3.1.1/ppcx64"

cp ./dcm2nii/dcm2nii /Users/rorden/Documents/mricron/dcm2nii64

# lazbuild -B ./dcm2nii/dcm2nii.lpr
lazbuild -B dcm2nii.lpr --ws=cocoa --cpu=x86_64 --os=darwin --compiler=/usr/local/bin/ppcx64
cp ./dcm2nii/dcm2nii /Users/rorden/Documents/mricron/dcm2nii

./_xclean.bat
cp ./common/gui.inc ./common/isgui.inc

#compile MRIcron 64
#lazbuild ./mricron.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/bin/ppcx64"
#Current FPC 3.0.0 can not compile on OSX 10.11 El Capitan, so use 3.1.1
lazbuild ./mricron.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/lib/fpc/3.1.1/ppcx64"
strip ./mricron
cp ./mricron /Users/rorden/Documents/mricron/mricron64.app/Contents/MacOS/mricron


# lazbuild -B ./mricron.lpr --ws=carbon
lazbuild -B ./npm/npm.lpr --ws=carbon
lazbuild -B ./dcm2nii/dcm2niigui.lpr --ws=carbon
lazbuild -B ./mricron.lpr --ws=carbon
#lazbuild -B ./dcm2nii/dcm2niigui.lpr --ws=cocoa --cpu=x86_64 --os=darwin --compiler=/usr/local/bin/ppcx64

strip ./mricron
strip ./npm/npm
strip ./dcm2nii/dcm2niigui

cp ./mricron /Users/rorden/Documents/mricron/mricron.app/Contents/MacOS/mricron
cp ./npm/npm /Users/rorden/Documents/mricron/npm.app/Contents/MacOS/npm
cp ./dcm2nii/dcm2niigui /Users/rorden/Documents/mricron/dcm2niigui.app/Contents/MacOS/dcm2niigui

./_xclean.bat

cd /Users/rorden/Documents/pas/
zip -r /Users/rorden/Documents/mricron_source.zip mricron

cd /Users/rorden/Documents/
zip -r /Users/rorden/Documents/mricron_osx.zip mricron

