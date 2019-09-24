# The Easy Way

You can always get the latest compiled version online

```
curl -fLO https://github.com/rordenlab/MRIcroGL12/releases/latest/download/MRIcroGL_linux.zip
```  
   

# Build Your Own

Building MRIcron from source code using Docker (builds directly from the Master branch).


```
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
# copy executable, sample images, and other resources to folder "distro"
mkdir ~/distro  
mkdir ~/distro/MRIcron
cp ./MRIcron ~/distro/MRIcron/MRIcron
cp -a ./Resources/ ~/distro/MRIcron
```

# Notes

1. MRIcron works best if pigz and dcm2niix are installed.
2. MRIcron does list depend on some common Linux packages, which you can find by typing "ldd MRIcron"


