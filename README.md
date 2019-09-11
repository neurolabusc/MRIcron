# MRIcron / NPM / dcm2nii

##### About

MRIcron is a viewer for brain imaging data. 

##### Installing

You can download the software from several locations:

 - Download the latest version from [NITRC](https://www.nitrc.org/projects/mricron).
 - Download the [Github](https://github.com/neurolabusc/MRIcroGL/releases).
 - Run the following command to get the latest version for Linux, Macintosh or Windows: 
   * `curl -fLO https://github.com/rordenlab/MRIcroGL12/releases/latest/download/MRIcroGL_linux.zip`
   * `curl -fLO https://github.com/rordenlab/MRIcroGL12/releases/latest/download/MRIcroGL_macOS.dmg`
   * `curl -fLO https://github.com/rordenlab/MRIcroGL12/releases/latest/download/MRIcroGL_windows.zip`
   
Once you have downloaded and extracted the software, you may want to visit the [wiki](https://www.nitrc.org/plugins/mwiki/index.php/mricron:MainPage) for usage advice.

##### License

This software includes a [BSD license](https://opensource.org/licenses/BSD-2-Clause)

##### Future

MRIcron is robust and stable, but development efforts have moved to MRIcroGL. The latest releases of MRIcron only include the MRIcron viewer and the dcm2niix image converter. Once upon a time, the software was distributed with statistics (NPM) and legacy image converter (dcm2nii). One can still download old releases of MRIcron from [NITRC](https://www.nitrc.org/projects/mricron) or compile these legacy tools (see next section).

NPM is a tool for non parametric analysis of neuroimaging lesion data. dcm2nii is designed for converting the complicated DICOM format used in medical imaging to the simple NIfTI format preferred by scientists. These tools are mature and hopefully robust but no longer in active development.

 - MRIcron development has moved to [MRIcroGL](https://github.com/rordenlab/MRIcroGL12/releases)
 - NPM development has been moved to [NiiStat](https://github.com/neurolabusc/NiiStat)
 - dcm2nii development has been moved to [dcm2niix](https://github.com/neurolabusc/dcm2niix)

##### Compiling your own software

You can build MRIcron using [Lazarus](https://www.lazarus-ide.org).
 - Launch Lazarus and open the project.
 - Choose Run/Run to build and execute the program
