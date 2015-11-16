chmod 777 ./_xclean.bat
./_xclean.bat
cp ./common/notgui.inc ./common/isgui.inc
lazbuild ./dcm2nii/dcm2nii.lpr
cp ./dcm2nii/dcm2nii ../distro/dcm2nii

./_xclean.bat
cp ./common/gui.inc ./common/isgui.inc

lazbuild ./mricron.lpr --ws=carbon
lazbuild ./npm/npm.lpr --ws=carbon
lazbuild ./dcm2nii/dcm2niigui.lpr --ws=carbon

cp ./mricron ../distro/mricron.app/mricron
cp ./npm/npm ../distro/npm.app/npm
cp ./dcm2nii/dcm2niigui ../distro/dcm2niigui.app/dcm2niigui
