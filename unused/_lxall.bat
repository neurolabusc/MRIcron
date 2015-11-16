
cd ~/mricron
chmod 777 ./_qtscript.bat
./_qtscript.bat

cd ~/qt
zip -r mricronqt mricron
mv mricronqt.zip ..

cd ~/gtk1
zip -r mricronlx mricron
mv mricronlx.zip ..

cd ~/gtk2
zip -r mricronlx2 mricron
mv mricronlx2.zip ..

