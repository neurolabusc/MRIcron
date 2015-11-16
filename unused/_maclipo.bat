# lipo -create ../distro/ppc/dcm2nii ../distro/intel/dcm2nii -output ../distro/dcm2nii
cp ../distro/intel/dcm2nii ../distro/dcm2nii
lipo -create ../distro/ppc/mricron ../distro/intel/mricron -output ../distro/mricron.app/Contents/MacOS/mricron
lipo -create ../distro/ppc/dcm2niigui ../distro/intel/dcm2niigui -output ../distro/dcm2niigui.app/Contents/MacOS/dcm2niigui
lipo -create ../distro/ppc/npm ../distro/intel/npm -output ../distro/npm.app/Contents/MacOS/npm
