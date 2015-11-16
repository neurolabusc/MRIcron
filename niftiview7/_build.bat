"C:\strip" "C:\pas\Delphi\niftiview7\MRIcroN.exe"
copy "C:\pas\Delphi\niftiview7\MRIcroN.exe" C:\mricron\mricron.exe
"C:\Program Files\NSIS2\makensis" "C:\Program Files\NSIS2\mricron.nsi"
del /S *.dcu
del /S *.~pa
del /S *.~df
del /S *.cfg
del /S *.obj
del /S *.hpp
del /S *.ddp
del /S *.mps
del /S *.mpt
del /S *.dsm
del /S *.bak


