del   c:\mricron\*.ini

call _clean.bat
copy /Y .\common\notgui.inc .\common\isgui.inc

cd .\dcm2nii
c:\lazarus\lazbuild dcm2nii.lpr
c:\lazarus\fpc\3.0.0\bin\x86_64-win64\strip dcm2nii.exe
#C:\PROGRA~2\BORLAND\DELPHI7\BIN\dcc32   -CC -B dcm2nii.dpr
# c:\strip dcm2nii.exe
###copy /Y dcm2nii.exe c:\mricron
cd ..

call _clean.bat
copy  /Y .\common\gui.inc .\common\isgui.inc

cd .\npm
# C:\PROGRA~2\BORLAND\DELPHI7\BIN\dcc32 -U..\delphionly -B npm.dpr
# c:\strip npm.exe
c:\lazarus\lazbuild npm.lpr
c:\lazarus\fpc\3.0.0\bin\x86_64-win64\strip npm.exe
#####copy /Y npm.exe c:\mricron
cd ..

cd .\dcm2nii
# C:\PROGRA~2\BORLAND\DELPHI7\BIN\dcc32 -U..\delphionly;C:\pas\d7\rx275d7\Units -B dcm2niigui.dpr
# c:\strip dcm2niigui.exe
c:\lazarus\lazbuild dcm2niigui.lpr
c:\lazarus\fpc\3.0.0\bin\x86_64-win64\strip dcm2niigui.exe
###copy /Y dcm2niigui.exe c:\mricron
cd ..



#cd c:\pas\mricron\niftiview7
# C:\PROGRA~2\BORLAND\DELPHI7\BIN\dcc32 -UC:\pas\d7\rx275d7\Units;C:\PROGRA~2\PngComponents\Source -B mricron.dpr
# c:\strip c:\pas\mricron\niftiview7\mricron.exe
c:\lazarus\lazbuild mricron.lpr
c:\lazarus\fpc\3.0.0\bin\x86_64-win64\strip mricron.exe
copy /Y mricron.exe c:\mricron

call _clean.bat

REM compress MRIcron
c:\Progra~1\7-Zip\7z a -tzip c:\pas\wincron.zip c:\mricron
REM copy /Y c:\pas\wincron.zip Y:\mcbi\MCBI\CRNL\sw\mricron\win.zip

REM compress Source
c:\Progra~1\7-Zip\7z a -tzip c:\pas\srccron.zip c:\pas\mricron 
REM copy c:\pas\srccron.zip Y:\mcbi\MCBI\CRNL\sw\mricron\source.zip


