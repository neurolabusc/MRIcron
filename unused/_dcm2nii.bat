chmod 777 ./_xclean.bat
./_xclean.bat
cp ./common/notgui.inc ./common/isgui.inc
lazbuild ./dcm2nii/dcm2nii.lpr --cpu=x86_64 --compiler="/usr/local/bin/ppcx64"
mv ./dcm2nii/dcm2nii ../distro/dcm2nii64

./_xclean.bat
cp ./common/gui.inc ./common/isgui.inc
