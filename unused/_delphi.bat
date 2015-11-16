del   c:\mricron\*.ini

call _clean.bat
copy /Y .\common\notgui.inc .\common\isgui.inc

cd .\dcm2nii
C:\PROGRA~2\BORLAND\DELPHI7\BIN\dcc32   -CC -B dcm2nii.dpr
c:\strip dcm2nii.exe
copy /Y dcm2nii.exe c:\mricron
cd ..

call _clean.bat
copy  /Y .\common\gui.inc .\common\isgui.inc

cd .\npm
C:\PROGRA~2\BORLAND\DELPHI7\BIN\dcc32 -U..\delphionly -B npm.dpr
c:\strip npm.exe
copy /Y npm.exe c:\mricron
cd ..

cd .\dcm2nii
C:\PROGRA~2\BORLAND\DELPHI7\BIN\dcc32 -U..\delphionly;C:\pas\d7\rx275d7\Units -B dcm2niigui.dpr
c:\strip dcm2niigui.exe
copy /Y dcm2niigui.exe c:\mricron
cd ..

call _clean.bat

cd c:\pas\delphi\niftiview7
C:\PROGRA~2\BORLAND\DELPHI7\BIN\dcc32 -UC:\pas\d7\rx275d7\Units;C:\PROGRA~2\PngComponents\Source -B mricron.dpr
c:\strip c:\pas\delphi\niftiview7\mricron.exe
copy /Y c:\pas\delphi\niftiview7\mricron.exe c:\mricron\


