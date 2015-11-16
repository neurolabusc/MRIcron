#!/bin/sh

cd ~/mricron

chmod 777 ./_xclean.bat
./_xclean.bat
cp ./common/notgui.inc ./common/isgui.inc
lazbuild -B ./dcm2nii/dcm2nii.lpr
cp ./dcm2nii/dcm2nii ~/mricron_lx

lazbuild --cpu=i386 -B ./dcm2nii/dcm2nii.lpr
cp ./dcm2nii/dcm2nii ~/mricron_lx/dcm2nii32



./_xclean.bat
cp ./common/gui.inc ./common/isgui.inc

lazbuild -B ./mricron.lpr
lazbuild -B ./npm/npm.lpr
lazbuild -B ./dcm2nii/dcm2niigui.lpr
cp ./mricron ~/mricron_lx
cp ./npm/npm ~/mricron_lx
cp ./dcm2nii/dcm2niigui ~/mricron_lx

lazbuild --cpu=i386 -B ./mricron.lpr
lazbuild --cpu=i386 -B ./npm/npm.lpr
lazbuild --cpu=i386 -B ./dcm2nii/dcm2niigui.lpr
cp ./mricron ~/mricron_lx/mricron32
cp ./npm/npm ~/mricron_lx/npm32
cp ./dcm2nii/dcm2niigui ~/mricron_lx/dcm2niigui32


./_xclean.bat

cd ~
zip -r ~/mricron_lx.zip mricron_lx

