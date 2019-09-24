Easiest way to get MRIcron pre-compiled
   curl -fLO https://github.com/rordenlab/MRIcroGL12/releases/latest/download/MRIcroGL_linux.zip
   
   
Building MRIcron from source code using Docker 


docker pull jgoerzen/debian-base-standard
docker run -td --stop-signal=SIGPWR --name=name jgoerzen/debian-base-standard
docker run -it jgoerzen/debian-base-standard /bin/bash

apt-get update
apt-get -yq install lazarus
apt-get -yq install git
cd ~
git clone https://github.com/neurolabusc/MRIcron.git
cd MRIcron
lazbuild -B ./mricron.lpr
# create distribution
mkdir ~/distro  
mkdir ~/distro/MRIcron
cp ./MRIcron ~/distro/MRIcron/MRIcron
cp -a ./Resources/ ~/distro/MRIcron

Notes

1.) MRIcron works best if pigz and dcm2niix are installed.

2.) MRIcron does list depend on some common Linux packages, which you can find by typing "ldd MRIcron"


Run8

syntaxWM13.run08+orig.HEAD
syntaxWM13.run08+orig.BRIK

syntaxWM13.run08.nii
