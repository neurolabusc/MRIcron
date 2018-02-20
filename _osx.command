#!/bin/sh
#This script builds a distribution on Chris Rorden's personal computer.
#  to build your own version you typically run
#    lazbuild -B filename.lpr

#compile dcm2niix
cd ~/Documents/cocoa/dcm2niix/console
g++ -O3 -dead_strip -I. main_console.cpp nii_foreign.cpp nii_dicom.cpp nifti1_io_core.cpp nii_ortho.cpp nii_dicom_batch.cpp jpg_0XC3.cpp ujpeg.cpp -o dcm2niix  -I/usr/local/lib -I/usr/local/include/openjpeg-2.1 /usr/local/lib/libopenjp2.a
cp dcm2niix /Users/rorden/Documents/mricron/MRIcron/dcm2niix
cp dcm2niix /Users/rorden/Documents/mricron/MRIcron/MRIcron.app/Contents/Resources/dcm2niix


cd /Users/rorden/Documents/pas/mricron

chmod 777 ./_xclean.bat
./_xclean.bat

: <<'SKIPDCM2NII'
	cp ./common/notgui.inc ./common/isgui.inc
	#lazbuild ./dcm2nii/dcm2nii.lpr --cpu=x86_64 --compiler="/usr/local/bin/ppcx64"
	#Current FPC 3.0.0 can not compile on OSX 10.11 El Capitan, so use 3.1.1
	lazbuild ./dcm2nii/dcm2nii.lpr --cpu=x86_64 --compiler="/usr/local/lib/fpc/3.1.1/ppcx64"
	cp ./dcm2nii/dcm2nii /Users/rorden/Documents/mricron/MRIcron/dcm2nii64
	# lazbuild -B ./dcm2nii/dcm2nii.lpr
	lazbuild -B ./dcm2nii/dcm2nii.lpr --ws=cocoa --cpu=x86_64 --os=darwin --compiler=/usr/local/bin/ppcx64
	cp ./dcm2nii/dcm2nii /Users/rorden/Documents/mricron/MRIcron/dcm2nii
	./_xclean.bat
	cp ./common/gui.inc ./common/isgui.inc
	/Users/rorden/lazarus/lazbuild ./dcm2nii/dcm2niigui.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/lib/fpc/3.1.1/ppcx64"
	strip ./dcm2nii/dcm2niigui
	cp ./dcm2nii/dcm2niigui /Users/rorden/Documents/mricron/MRIcron/dcm2niigui.app/Contents/MacOS/dcm2niigui

SKIPDCM2NII
: <<'SKIPNPM'
	/Users/rorden/lazarus/lazbuild ./npm/npm.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/lib/fpc/3.1.1/ppcx64"
	strip ./npm/npm
	cp ./npm/npm /Users/rorden/Documents/mricron/MRIcron/npm.app/Contents/MacOS/npm
SKIPNPM

#compile 32-bit
# lazbuild -B ./npm/npm.lpr --ws=carbon
# lazbuild -B ./dcm2nii/dcm2niigui.lpr --ws=carbon
# lazbuild -B ./mricron.lpr --ws=carbon

#compile 64-bit
#lazbuild ./mricron.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/bin/ppcx64"
#Current FPC 3.0.0 can not compile on OSX 10.11 El Capitan, so use 3.1.1
#/Users/rorden/lazarus/lazbuild ./mricron.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/lib/fpc/3.1.1/ppcx64"
/Users/rorden/lazarus/lazbuild ./mricron.lpr --cpu=x86_64 --ws=cocoa

strip ./mricron

cp ./mricron /Users/rorden/Documents/mricron/MRIcron/mricron.app/Contents/MacOS/mricron

awk '{gsub(/Active="MacOS"/,"Active=\"Default\"");}1' mricron.lps > mricron.tmp && mv mricron.tmp mricron.lps

./_xclean.bat

rm -rf lib
rm -rf backup

cd /Users/rorden/Documents/pas/
zip -r /Users/rorden/Documents/mricron_source.zip mricron

cd /Users/rorden/Documents/
zip -r /Users/rorden/Documents/mricron_osx.zip mricron



