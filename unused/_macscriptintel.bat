chmod 777 ./_xclean.bat
./_xclean.bat
cp ./common/notgui.inc ./common/isgui.inc
lazbuild ./dcm2nii/dcm2nii.lpr --cpu=x86_64 --compiler="/usr/local/bin/ppcx64"
mv ./dcm2nii/dcm2nii ../distro/dcm2nii64

lazbuild ./dcm2nii/dcm2nii.lpr
cp ./dcm2nii/dcm2nii ../distro/intel/dcm2nii
#assume we will not lipo PPC versions...
cp ./dcm2nii/dcm2nii ../distro/dcm2nii

./_xclean.bat
cp ./common/gui.inc ./common/isgui.inc

lazbuild ./mricron.lpr --ws=carbon
lazbuild ./npm/npm.lpr --ws=carbon
lazbuild ./dcm2nii/dcm2niigui.lpr --ws=carbon

cp ./mricron ../distro/intel/mricron
cp ./npm/npm ../distro/intel/npm
cp ./dcm2nii/dcm2niigui ../distro/intel/dcm2niigui

#assume we will not lipo PPC versions...
cp ./dcm2nii/dcm2nii ../distro/dcm2nii
cp ./mricron ../distro/mricron.app/contents/MacOS/mricron
cp ./npm/npm ../distro/npm.app/contents/MacOS/npm
cp ./dcm2nii/dcm2niigui ../distro/dcm2niigui.app/contents/MacOS/dcm2niigui
