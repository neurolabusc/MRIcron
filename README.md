# MRIcron / NPM / dcm2nii

##### About

MRIcron is a viewer for brain imaging data. NPM is a tool for non parametric analysis of neuroimaging lesion data. dcm2nii is designed for converting the complicated DICOM format used in medical imaging to the simple NIfTI format preferred by scientists. These tools are mature and hopefully robust but no longer in active development.
 - MRIcron development has moved to [MRIcroGL](https://github.com/neurolabusc/MRIcroGL)
 - NPM development has been moved to [NiiStat](https://github.com/neurolabusc/NiiStat)
 - dcm2nii development has been moved to [dcm2niix](https://github.com/neurolabusc/dcm2niix)

##### Downloading compiled software

 - The latest stable version is released at [NITRC](https://www.nitrc.org/projects/mricron)

##### Recent Versions

2-May-2016
 - MRIcron : Improved ability to copy images to clipboard
 - NPM : Permutation thresholds in previous versions were not completely random, while the influence of this was typically negligible when images were listed in random order, this could make the thresholds slightly more liberal or conservative if the images listed in a sorted manner (e.g. lesion size, behavioral deficit). This new version revamps the randomization process, including using the [random number generator described by Marsaglia Zaman](http://paulbourke.net/miscellaneous/random/). Permutation thresholds are now more robust, albeit slower. THe enhanced 64-bit version allows the user to select more threads, which can accelerate the software (assuming your computer has more CPUs).

##### License

This software includes a [BSD license](https://opensource.org/licenses/BSD-2-Clause)

##### Compiling your own software

This is a beta release of MRIcron. You can compile this using Lazarus. It has been compiled on Windows, Linux-x86, OSX-x86 and OSX-PPC. It requires builds of Lazarus and FreePascal created after October 7, 2007.
 http://www.hu.freepascal.org/lazarus/

To compile for OSX [Carbon] -
1.) Launch Lazarus and open the project.
2.) Select Project/CompilerOptions
   Paths tab: make sure the "LCL widget type" is set to "carbon"
   Linking tab: make sure the "Pass options to linker" checkbox is selected and
      set the text to "-framework carbon" (no quotes).
3.) Select Project/ProjectOptions and set "Use application bundle for running and debugging"
4.) If using OSX 10.5 or later, add to Project / Compiler options / Other / Custom options:
     -k-macosx_version_min -k10.4
     -XR/Developer/SDKs/MacOSX10.4u.sdk/
     Alternative: Project/ProjectOptions/Linking/ Check 'pass options to linker' and add this line -macosx_version_min 10.4
5.) For debugging, you will want to create an alias from the application folder to the compiled executable:
   The exact value will depend on your paths, but it will be similar to this:
      rm ~/Documents/mricron/mricron.app/mricron
      ln -s ~/Documents/mricron/mricron ~/Documents/mricron/mricron.app/mricron
      rm ~/Documents/mricron/npm/npm.app/npm
      ln -s ~/Documents/mricron/npm/npm ~/Documents/mricron/npm/npm.app/npm
      rm ~/Documents/mricron/dcm2nii/dcm2niigui.app/dcm2niigui
      ln -s ~/Documents/mricron/dcm2nii/dcm2niigui ~/Documents/mricron/dcm2nii/dcm2niigui.app/dcm2niigui

6.) Select Run/Run to build and execute your program
7.) For making an executable to distribute, control+click on the program's .app folder (e.g. the file named mricron that has a brain icon) and choose "show package contents" - move the executable generated with Lazarus into the folder, overwriting the symbolic link created in step 4.

-------------------------------------------
To compile for Linux GTK1 -
1.) Launch Lazarus and open the project.
2.) Select Project/CompilerOptions
   Paths tab: make sure the "LCL widget type" is set to "default [gtk]"
   Linking tab: make sure the "Pass options to linker" checkbox is UNCHECKED.
3.) Choose Run/Run to build and execute the program

-------------------------------------------
To compile for Linux GTK2 -
1.) Launch Lazarus and open the project.
2.) Select Project/CompilerOptions
   Paths tab: make sure the "LCL widget type" is set to "gtk2"
   Linking tab: make sure the "Pass options to linker" checkbox is UNCHECKED.
3.) Choose Run/Run to build and execute the program

-------------------------------------------
To compile for Windows -
1.) Launch Lazarus and open the project.
2.) Select Project/CompilerOptions
   Paths tab: make sure the "LCL widget type" is set to "default [Win API]"
   Linking tab: make sure the "Pass options to linker" checkbox is UNCHECKED.
3.) Choose Run/Run to build and execute the program
