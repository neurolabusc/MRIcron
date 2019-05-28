del   c:\mricron\*.ini



call _clean.bat
copy  /Y .\common\gui.inc .\common\isgui.inc

#cd c:\pas\mricron\niftiview7
# C:\PROGRA~2\BORLAND\DELPHI7\BIN\dcc32 -UC:\pas\d7\rx275d7\Units;C:\PROGRA~2\PngComponents\Source -B mricron.dpr
# c:\strip c:\pas\mricron\niftiview7\mricron.exe
d:\lazarus\lazbuild mricron.lpr
d:\lazarus\fpc\3.0.4\bin\x86_64-win64\strip mricron.exe
copy /Y mricron.exe d:\neuro\mricron

call _clean.bat
del d:\neuro\MRIcron\mricron.ini
REM compress MRIcron
c:\Progra~1\7-Zip\7z a -tzip d:\neuro\MRIcron_Windows.zip d:\neuro\MRIcron
REM copy /Y c:\pas\wincron.zip Y:\mcbi\MCBI\CRNL\sw\mricron\win.zip

REM compress Source
# c:\Progra~1\7-Zip\7z a -tzip c:\pas\mricron_windows.zip c:\pas\mricron 
REM copy c:\pas\srccron.zip Y:\mcbi\MCBI\CRNL\sw\mricron\source.zip


