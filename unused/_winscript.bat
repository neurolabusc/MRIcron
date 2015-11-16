call _clean.bat
copy .\common\notgui.inc .\common\isgui.inc

c:\lazarus\lazbuild .\dcm2nii\dcm2nii.lpr

copy .\dcm2nii\dcm2nii.exe c:\mricron\dcm2nii.exe

call _clean.bat
copy .\common\gui.inc .\common\isgui.inc
c:\lazarus\lazbuild .\npm\npm.lpr
copy .\npm\npm.exe c:\mricron\npm.exe
c:\lazarus\lazbuild .\dcm2nii\dcm2niigui.lpr
copy .\dcm2nii\dcm2niigui.exe c:\mricron\dcm2niigui.exe
c:\lazarus\lazbuild .\mricron.lpr
copy .\mricron.exe c:\mricron\mricron.exe
call _clean.bat
