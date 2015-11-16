C:\lazarus\pp\bin\i386-win32\strip --verbose --strip-all "C:\lazarus\mricron\mricron.exe"
copy "C:\lazarus\mricron\mricron.exe" C:\mricron\mricron.exe
copy "C:\lazarus\mricron\dcm2nii\dcm2nii.exe" C:\mricron\dcm2nii.exe
"C:\Program Files\NSIS2\makensis" "C:\Program Files\NSIS2\mricron.nsi"
