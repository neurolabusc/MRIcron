REM COMPILE MRIcron
call _delphi.bat

REM compress MRIcron
c:\Progra~1\7-Zip\7z a -tzip c:\pas\wincron.zip c:\mricron
copy /Y c:\pas\wincron.zip Y:\mcbi\MCBI\CRNL\sw\mricron\win.zip

REM compress Source
c:\Progra~1\7-Zip\7z a -tzip c:\pas\srccron.zip c:\pas\mricron 
copy c:\pas\srccron.zip Y:\mcbi\MCBI\CRNL\sw\mricron\source.zip