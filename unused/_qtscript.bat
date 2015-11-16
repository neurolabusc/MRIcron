chmod 777 ./_xclean.bat
./_xclean.bat
cp ./common/notgui.inc ./common/isgui.inc
lazbuild ./dcm2nii/dcm2nii.lpr
cp ./dcm2nii/dcm2nii ../gtk1/mricron/dcm2nii
cp ./dcm2nii/dcm2nii ../gtk2/mricron/dcm2nii
cp ./dcm2nii/dcm2nii ../qt/mricron/dcm2nii


cp ./common/gui.inc ./common/isgui.inc

./_xclean.bat
lazbuild ./mricron.lpr
lazbuild ./npm/npm.lpr
lazbuild ./dcm2nii/dcm2niigui.lpr
cp ./mricron ../gtk1/mricron/mricron
cp ./npm/npm ../gtk1/mricron/npm
cp ./dcm2nii/dcm2niigui ../gtk1/mricron/dcm2niigui

./_xclean.bat
lazbuild ./mricron.lpr --ws=gtk2
lazbuild ./npm/npm.lpr --ws=gtk2
lazbuild ./dcm2nii/dcm2niigui.lpr --ws=gtk2
cp ./mricron ../gtk2/mricron/mricron
cp ./npm/npm ../gtk2/mricron/npm
cp ./dcm2nii/dcm2niigui ../gtk2/mricron/dcm2niigui

./_xclean.bat
lazbuild ./mricron.lpr --ws=qt
lazbuild ./npm/npm.lpr --ws=qt
lazbuild ./dcm2nii/dcm2niigui.lpr --ws=qt
cp ./mricron ../qt/mricron/mricron
cp ./npm/npm ../qt/mricron/npm
cp ./dcm2nii/dcm2niigui ../qt/mricron/dcm2niigui
