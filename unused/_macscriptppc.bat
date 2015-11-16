chmod 777 ./_xclean.bat
./_xclean.bat
cp ./common/notgui.inc ./common/isgui.inc
lazbuild ./dcm2nii/dcm2nii.lpr
cp ./dcm2nii/dcm2nii ../distro/ppc/dcm2nii

./_xclean.bat
cp ./common/gui.inc ./common/isgui.inc

lazbuild ./mricron.lpr --ws=carbon
lazbuild ./npm/npm.lpr --ws=carbon
lazbuild ./dcm2nii/dcm2niigui.lpr --ws=carbon

cp ./mricron ../distro/ppc/mricron
cp ./npm/npm ../distro/ppc/npm
cp ./dcm2nii/dcm2niigui ../distro/ppc/dcm2niigui
