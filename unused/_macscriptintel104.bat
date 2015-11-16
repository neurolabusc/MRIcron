chmod 777 ./_xclean.bat
./_xclean.bat
cp ./common/notgui.inc ./common/isgui.inc
lazbuild ./dcm2nii/dcm2nii.lpr="-va -k-macosx_version_min -k10.4 -XR/Developer/SDKs/MacOSX10.4u.sdk/"
cp ./dcm2nii/dcm2nii ../distro/intel/dcm2nii

./_xclean.bat
cp ./common/gui.inc ./common/isgui.inc

lazbuild ./mricron.lpr="-va -k-macosx_version_min -k10.4 -XR/Developer/SDKs/MacOSX10.4u.sdk/" --ws=carbon
lazbuild ./npm/npm.lpr="-va -k-macosx_version_min -k10.4 -XR/Developer/SDKs/MacOSX10.4u.sdk/" --ws=carbon
lazbuild ./dcm2nii/dcm2niigui.lpr --ws=carbon

cp ./mricron ../distro/intel/mricron
cp ./npm/npm ../distro/intel/npm
cp ./dcm2nii/dcm2niigui ../distro/intel/dcm2niigui
