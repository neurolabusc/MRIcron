unit nii_math;

interface
{$H+}
uses
  //nii_types,nii_write,
  niftiutil,dicomtypes,prefs,
  define_types, sysutils, dialogsx,GraphicsMathLibrary, nifti_types;

type
  TNIFTIimg =  record
   HdrName: string;
   Hdr: TNIFTIhdr;
   Opts: TNIIOpts;
   //ByteSwap: boolean;
   Offset: integer;
   Buffer,i8: bytep;
   f32: singlep;
   i32 : longintP;
   i16 : SmallIntP;
  end;
function RMS3d (lAName,lBName,lMaskName: string; lMaskThresh: single; lSaveOutput: boolean; lPrefs: TPrefs):double;
function AddSlices (lAName: string; lSlices: integer; lPrefs: TPrefs):boolean;
function ReportMinMax (lAName: string): boolean;
function Hounsfield2NormScale (lAName: string; lPrefs: TPrefs):boolean;
function ShrinkNII (lAName: string; lPrefs: TPrefs): boolean;

implementation

procedure CreateNII(var lNII: TNIfTIimg);
begin
     lNII.Buffer := nil;
end;

procedure FreeNII(var lNII: TNIfTIimg);
begin
     if lNII.Buffer <> nil then
        Freemem(lNII.Buffer);
end;

function LoadHdrNII(lFilename: string; var lNII: TNIfTIimg): boolean;
begin
     result := false;
     lNII.HdrName := lFilename;
     if not NIFTIhdr_LoadHdr (lNII.HdrName, lNII.Hdr, lNII.Opts) then begin;
        ShowMsg('Header load error '+lFilename);
        exit;
     end;
     result := true;
end;

function SubBound (lVal,lMin: integer): integer;
begin
    result := lVal;
    if result < lMin then
       result := lMin;
end;

function NonspatialDimensionsNII (lA: TNIFTIimg): integer;
//returns sum of 4th, 5th, 6th and 7th dimension...
begin
     result := SubBound(lA.Hdr.dim[4],1)*SubBound(lA.Hdr.dim[5],1)*SubBound(lA.Hdr.dim[6],1)*SubBound(lA.Hdr.dim[7],1);
end;

function LoadImgNII(lFilename: string; var lNII: TNIfTIimg): boolean;
begin
     result := false;
     if not LoadHdrNII(lFilename,lNII) then
        exit;
     if not NIFTIhdr_LoadImgRaw (False,lNII.HdrName, lNII.Hdr, lNII.Buffer, lNII.Offset,lNII.Opts) then begin
        ShowMsg('Image load error '+lFilename);
        exit;
     end;
     lNII.f32 := SingleP(@lNII.Buffer^[lNII.Offset+1]);
     lNII.i32 := LongintP(@lNII.Buffer^[lNII.Offset+1]);
     lNII.i16 := SmallIntP(@lNII.Buffer^[lNII.Offset+1]);
     lNII.i8 := ByteP(@lNII.Buffer^[lNII.Offset+1]);
     result := true;
end;

procedure Force3DNII (var lNII: TNIFTIimg);
begin
     lNII.Hdr.dim[0] := 3;
     lNII.Hdr.dim[4] := 1;
     lNII.Hdr.Dim[5] := 1;
     lNII.Hdr.Dim[6] := 1;
     lNII.Hdr.Dim[7] := 1;
end;

function CreateEmptyImgNII(lHdr: TNIFTIHdr; var lNII: TNIfTIimg): boolean;
var
   lVol,lImgBytes,lFileBytes: integer;
begin
     result := false;
     //FreeNII ???
     lNII.Hdr := lHdr;
     lNII.Offset := kNIIImgOffset;// (=352) bytes for creating .nii.gz files
     lVol := NonspatialDimensionsNII(lNiI);
     //lVol := lHdr.dim[4]+lHdr.dim[5]+lHdr.dim[6]+lHdr.dim[7]; //crepes
     if lVol < 1 then
      lVol := 1;
     lImgBytes := lHdr.dim[1]*lHdr.dim[2]*lHdr.dim[3]*lVol*(lHdr.bitpix div 8);
     if lImgBytes < 1 then
        exit;
     lFileBytes := lImgBytes+ kNIIImgOffset;
     GetMem(lNII.Buffer,lFileBytes);
     lNII.f32 := SingleP(@lNII.Buffer^[lNII.Offset+1]);
     lNII.i32 := LongintP(@lNII.Buffer^[lNII.Offset+1]);
     lNII.i16 := SmallIntP(@lNII.Buffer^[lNII.Offset+1]);
     lNII.i8 := ByteP(@lNII.Buffer^[lNII.Offset+1]);
     result := true;
end;


function MinMaxNII(var lNII: TNIfTIimg; lVol: integer; ApplyHdrScaling: boolean; var lMin,lMax: double): boolean;
//returns min and max intensity in as Volume.
//For 4D data, use lVol to specify the volume
//  if lVol < 1 then all volumes
var
   i,lnVol,lVox,lVoxOffset: integer;
begin
     result := false;
     if lNII.Buffer = nil then begin
        showmsg('MinMax Error: image not loaded.');
        exit;//image not loaded...
     end;
     lnVol := NonspatialDimensionsNII(lNiI);
     lVox := lNII.Hdr.dim[1]*lNII.Hdr.dim[2]*lNII.Hdr.dim[3];
     if (lnVol < 1) or (lVox < 1) then
        exit;
     lVoxOffset := 0;
     if (lVol < 1) or (lVol > lnVol) then
        lVox := lVox * lnVol
     else
         lVoxOffset := (lVol-1)*lVox;
     case lNII.Hdr.datatype of
          kDT_UNSIGNED_CHAR: begin
                     lMin := lNII.i8^[lVoxOffset+1];
                     lMax := lMin;
                     for i := 1 to lVox do
                         if lNII.i8^[lVoxOffset+i] > lMax then
                            lMax := lNII.i8^[lVoxOffset+i];
                     for i := 1 to lVox do
                         if lNII.i8^[lVoxOffset+i] < lMin then
                            lMin := lNII.i8^[lVoxOffset+i];
             end;//CHAR
          kDT_SIGNED_SHORT: begin
                     lMin := lNII.i16^[lVoxOffset+1];
                     lMax := lMin;
                     for i := 1 to lVox do
                         if lNII.i16^[lVoxOffset+i] > lMax then
                            lMax := lNII.i16^[lVoxOffset+i];
                     for i := 1 to lVox do
                         if lNII.i16^[lVoxOffset+i] < lMin then
                            lMin := lNII.i16^[lVoxOffset+i];
             end;//kDT_SIGNED_SHORT
          kDT_SIGNED_INT: begin
                     lMin := lNII.i32^[lVoxOffset+1];
                     lMax := lMin;
                     for i := 1 to lVox do
                         if lNII.i32^[lVoxOffset+i] > lMax then
                            lMax := lNII.i32^[lVoxOffset+i];
                     for i := 1 to lVox do
                         if lNII.i32^[lVoxOffset+i] < lMin then
                            lMin := lNII.i32^[lVoxOffset+i];
             end;//kDT_SIGNED_INT
          kDT_FLOAT: begin
                     lMin := lNII.f32^[lVoxOffset+1];
                     lMax := lMin;
                     for i := 1 to lVox do
                         if lNII.f32^[lVoxOffset+i] > lMax then
                            lMax := lNII.f32^[lVoxOffset+i];
                     for i := 1 to lVox do
                         if lNII.f32^[lVoxOffset+i] < lMin then
                            lMin := lNII.f32^[lVoxOffset+i];
             end;//float
     end;// datatype
     if ApplyHdrScaling then begin
        lMin := (lMin * lNII.hdr.scl_slope)+lNII.hdr.scl_inter;
        lMax := (lMax * lNII.hdr.scl_slope)+lNII.hdr.scl_inter;
     end;
     result := true;
end;

function SameHdrDimNII (lA,lB: TNIFTIimg; lCheck4D, lCheckDataType: boolean): boolean;
begin
     result := SameHdrDim (lA.Hdr,lB.Hdr, lCheck4D, lCheckDataType);
     if not result then
        ShowMsg('Dimensions differ '+lA.Hdrname+' <> '+lB.HdrName);
end;

function ReportMinMax (lAName: string): boolean;
label
     666;
var
   lA: TNIfTIimg;
   lMin,lMax: double;
begin
     result := false;
     CreateNII(lA);
     if not LoadImgNII(lAName,lA) then
        goto 666;
     if not MinMaxNII(lA,0,true,lMin,lMax) then
        goto 666;
     showmsg(lAName+kTab+'Min'+floattostr(lMin)+kTab+'Max:'+floattostr(lMax));
     result := true;
666:
     FreeNII(lA);
end;

function Hounsfield2NormScale (lAName: string;  lPrefs: TPrefs):boolean;
//Hounsfield scaled data in the range
//Air	-1000
//Fat	120
//Water	0
//Muscle	~40
//Contrast	+130
//Bone	>400 (typically ~1000)
//problem 1: SPM assume 0 is dark [zero fills edges] - so we need to make minimum 0
//note the contrast of interest is in the compressed range -100..+200
//http://en.wikipedia.org/wiki/Hounsfield_units
const
     kUninterestingDarkUnits = 900; // e.g. -1000..-100
     kInterestingMidUnits = 300; //e.g. -100..+300
     kScaleRatio = 2;// increase dynamic range of interesting voxels by 3
label
     666;
var
   lA,lOut: TNIfTIimg;
   lMin,lMax,lRange: double;
   i,lVox: integer;
   v16,lExtra,lMin16: SmallInt;
   //lPrefs: TPrefs;
   lOName: string;
begin
     result :=  false;
     CreateNII(lA);
     CreateNII(lOut);
     if not LoadImgNII(lAName,lA) then
        goto 666;
     if lA.Hdr.datatype <> kDT_SIGNED_SHORT then begin
        showmsg('Hounsfield2NormScale Error: Image datatype must be 16-bit integer : '+lAName);
        goto 666;
     end;
     lVox := lA.Hdr.dim[1]*lA.Hdr.dim[2]*lA.Hdr.dim[3]*NonspatialDimensionsNII(lA);
     if lVox < 1 then
        goto 666;
     if not MinMaxNII(lA,0,false,lMin,lMax) then
        goto 666;
     lRange := lMax-lMin;
     if lRange < 1800 then begin
        //note assume integer data type with scaling...
        showmsg('Hounsfield2NormScale Error: dark to bright regions of a Hounsfield calibrated CT scan of the brain should exceed 1800 (air=-1000,bone=1000) : '+lAName);
        goto 666;
     end;
     //create output
     lOut.Hdr := lA.Hdr;
     force3DNII(lOut);
     lOut.Hdr.datatype := kDT_SIGNED_SHORT;
     lOut.Hdr.scl_slope := 1;
     lOut.Hdr.scl_inter := 0;
     CreateEmptyImgNII(lOut.Hdr, lOut);
     //translate values
     lMin16 := round(lMin);
     case lA.Hdr.datatype of
          kDT_SIGNED_SHORT: begin
             for i := 1 to lVox do begin
                 v16 := lA.i16^[i]-lMin16;
                 lExtra := v16-kUninterestingDarkUnits;
                 if lExtra > kInterestingMidUnits then
                    lExtra := kInterestingMidUnits;
                 if lExtra > 0 then
                    lExtra := lExtra*kScaleRatio
                 else
                     lExtra := 0;
                 lOut.i16^[i] := v16+lExtra;
             end;
             lOut.i16^[1] := 0;//ANTS uses this voxel for background color
          end;//kDT_SIGNED_SHORT
          else begin
                  Showmsg('Unsupported datatype');
          end;//float
     end;// datatype
     //Save data
     lOName := ChangeFilePrefix(lAName,'x');
     //SetDefaultPrefs (lPrefs);
     //lOName := lAName;
     //lPrefs.gzip := true;
     SaveNIfTICore (lOName, lOut.Buffer, kNIIImgOffset+1, lOut.Hdr, lPrefs);
     result := true;
666:
     FreeNII(lA);
     FreeNII(lOut);
end;

function RMS3d (lAName,lBName,lMaskName: string; lMaskThresh: single; lSaveOutput: boolean; lPrefs: TPrefs):double;
//Determines Root Mean Square Error between A and B
// both A and B are 3D images
//   Mean for each voxel sqrt(X^2+Y^2+Z^2)
//OPTIONAL: Mask image (set name to '' to ignore)
const
NaN : double = 1/0;
kErrorStr = 'RMS';
label
     666;
var
   lA,lB,lMask,lOut: TNIfTIimg;
   lSum,lRMS: double;
   lV,lVox,lCount,lMaskCount: integer;
   lUseMask: boolean;
   //lPrefs: TPrefs;
   lOName: string;
begin
     result :=  0;
     lUseMask := false;
     CreateNII(lA);
     CreateNII(lB);
     CreateNII(lMask);
     CreateNII(lOut);
     if not LoadImgNII(lAName,lA) then
        goto 666;
     if not LoadImgNII(lBName,lB) then
        goto 666;
     if not SameHdrDimNII(lA,lB,true,true) then
        goto 666;
     if NonspatialDimensionsNII(lA) <> 3 then begin
      ShowMsg('Image must have 3 volumes [not '+inttostr(NonspatialDimensionsNII(lA))+'] ' +lAName);
      goto 666;
     end;
     lVox := lA.Hdr.Dim[1]*lA.Hdr.Dim[2] * lA.Hdr.Dim[3];
     if lVox < 1 then
      goto 666;
     case lA.Hdr.datatype of
          kDT_FLOAT:;//lBPP := 4;
         else begin
             ShowMsg(kErrorStr+' datatype not supported.');
             exit;
         end;
     end; //case
     //next lines: mask....
     if (lMaskName <> '') and (not fileexists(lMaskName)) then
        ShowMsg(kErrorStr+'unable to find mask '+lMaskName)
     else if (lMaskName <> '') and (fileexists(lMaskName)) then begin
        lUseMask := true;
        if not LoadImgNII(lMaskName,lMask) then
           goto 666;
        if lMask.Hdr.datatype <> kDT_FLOAT then begin
           ShowMsg(kErrorStr+'datatype not supported. '+lMaskName);
           goto 666;
        end;
        if not SameHdrDimNII(lA,lMask,true,true) then
           goto 666;
     end; //mask
     //output
     if lSaveOutput then begin
        lOut.Hdr := lA.Hdr;
        force3DNII(lOut);
        lOut.Hdr.datatype := kDT_FLOAT;
        CreateEmptyImgNII(lOut.Hdr, lOut);
        for lV := 1 to lVox do
            lOut.f32^[lV] := 0;
     end;
     lSum:= 0;
     lCount := 0;
     lMaskCount := 0;
     case lA.Hdr.datatype of
          kDT_FLOAT: begin
             for lV := 1 to lVox do begin
               if (not (lUseMask)) or ((not SpecialSingle(lMask.f32^[lV])) and (lMask.f32^[lV]> lMaskThresh)) then begin
                 inc(lMaskCount);
                 if (not SpecialSingle(lA.f32^[lV])) and (not SpecialSingle(lA.f32^[lV+lVox])) and (not SpecialSingle(lA.f32^[lV+lVox+lVox]))
                  and (not SpecialSingle(lB.f32^[lV])) and (not SpecialSingle(lB.f32^[lV+lVox])) and (not SpecialSingle(lB.f32^[lV+lVox+lVox])) then begin
                 //if true then begin
                      inc(lCount);
                     lRMS :=  sqrt(sqr(lA.f32^[lV]-lB.f32^[lV])+ sqr(lA.f32^[lV+lVox]-lB.f32^[lV+lVox])+sqr(lA.f32^[lV+lVox+lVox]-lB.f32^[lV+lVox+lVox]));
                      if (lSaveOutput)  then begin
                         try   //switch from double to single precision...
                            lOut.f32^[lV] := lRMS;
                         except
                             lOut.f32^[lV] := NAN;
                         end; //except
                      end;
                      lSum := lSum + lRMS;
                 end; //not special - i.e. NaN

              end;//in mask
             end;//each 3D voxel
          end; //kDT_FLOAT
     end;//case of datatype

     if lMaskCount = 0 then
        ShowMsg(kErrorStr+' No voxels greater than '+floattostr(lMaskThresh)+' in mask '+lMaskName)
     else if lCount = 0 then
        ShowMsg(kErrorStr+' No valid voxels. All NaN?')
     else if lSaveOutput then begin
          lOName := ChangeFilePrefix(lAName,'Xrms');
          //SetDefaultPrefs (lPrefs);
          SaveNIfTICore (lOName, lOut.Buffer, kNIIImgOffset+1, lOut.Hdr, lPrefs);
     end;
     if lCount > 0 then
      result := lSum/lCount;
666:
     FreeNII(lA);
     FreeNII(lB);
     FreeNII(lMask);
     FreeNII(lOut);
end;

function AddSlices (lAName: string; lSlices: integer; lPrefs: TPrefs):boolean;
const
NaN : double = 1/0;
kErrorStr = 'RMS';
label
     666;
var
   lA,lOut: TNIfTIimg;
   lOffset,lV,lS,lI,lVolBytes,lSliceBytes: integer;
   //lPrefs: TPrefs;
   lOName: string;
begin
     result :=  false;
     if lSlices < 1 then
        exit;
     CreateNII(lA);
     CreateNII(lOut);
     if not LoadImgNII(lAName,lA) then
        goto 666;
     lSliceBytes := lA.hdr.dim[1]*lA.hdr.dim[2]*trunc(((lA.Hdr.bitpix)+7)/8);
     lVolBytes := lSliceBytes * lA.hdr.dim[3];
     if (lSliceBytes < 1) or (lVolBytes < 1) then
        goto 666;
     lOut.Hdr := lA.Hdr;
     force3DNII(lOut);
     lOut.hdr.dim[3] := lOut.hdr.dim[3] + lSlices;
     lOut.Hdr.datatype := kDT_FLOAT;
     CreateEmptyImgNII(lOut.Hdr, lOut);
     lI := 0;
     //lOffset := 0;
     lOffset := lSliceBytes * 10;
     for lS :=1 to lSlices do
         for lV := 1 to (lSliceBytes) do begin
             inc(lI);
             //lOut.i8^[lI] := 0;
             lOut.i8^[lI] := lA.i8^[lV+lOffset]
         end;
     lSliceBytes := lSliceBytes * lSlices;
     for lV := 1 to lVolBytes do
         lOut.i8^[lV+lSliceBytes] := lA.i8^[lV];

     //lOffset := 0;
     lSliceBytes := lA.hdr.dim[1]*lA.hdr.dim[2]*trunc(((lA.Hdr.bitpix)+7)/8);
     lI := 0;
     lOffset := lSliceBytes * 10;
     for lS :=1 to 18 do
         for lV := 1 to (lSliceBytes) do begin
             inc(lI);
             //lOut.i8^[lI] := 0;
             lOut.i8^[lI] := lA.i8^[lV+lOffset]
         end;

     lOName := ChangeFilePrefix(lAName,'x');
     //SetDefaultPrefs (lPrefs);
     if SaveNIfTICore (lOName, lOut.Buffer, kNIIImgOffset+1, lOut.Hdr, lPrefs) <> '' then
        result := true;
666:
     FreeNII(lA);
     FreeNII(lOut);
end;

procedure RescaleHdr (var lHdr: TNIFTIHdr; lX,lY,lZ: double);
var
   lIn,lScale,lResidualMat: TMatrix;
      dx, dy, dz: single;
begin
    lIn := Matrix3D (
    lHdr.srow_x[0],lHdr.srow_x[1],lHdr.srow_x[2],lHdr.srow_x[3],
    lHdr.srow_y[0],lHdr.srow_y[1],lHdr.srow_y[2],lHdr.srow_y[3],
    lHdr.srow_z[0],lHdr.srow_z[1],lHdr.srow_z[2],lHdr.srow_z[3],
    0,0,0,1);
    lScale := Matrix3D   (lX,0,0,0,
                         0,lY,0,0,
                         0,0,lZ,0,
                         0,0,0,1);
    lResidualMat := MultiplyMatrices(lIn,lScale);
  lHdr.srow_x[0] := lResidualMat.Matrix[1,1];
  lHdr.srow_x[1] := lResidualMat.Matrix[1,2];
  lHdr.srow_x[2] := lResidualMat.Matrix[1,3];
  lHdr.srow_y[0] := lResidualMat.Matrix[2,1];
  lHdr.srow_y[1] := lResidualMat.Matrix[2,2];
  lHdr.srow_y[2] := lResidualMat.Matrix[2,3];
  lHdr.srow_z[0] := lResidualMat.Matrix[3,1];
  lHdr.srow_z[1] := lResidualMat.Matrix[3,2];
  lHdr.srow_z[2] := lResidualMat.Matrix[3,3];
  lHdr.srow_x[3] := lResidualMat.Matrix[1,4];
  lHdr.srow_y[3] := lResidualMat.Matrix[2,4];
  lHdr.srow_z[3] := lResidualMat.Matrix[3,4];
  nifti_mat44_to_quatern( lResidualMat,
   lHdr.quatern_b,lHdr.quatern_c,lHdr.quatern_d,
   lHdr.qoffset_x,lHdr.qoffset_y,lHdr.qoffset_z,
                             dx, dy, dz,lHdr.pixdim[0]  {QFac});
end;

function ShrinkNII(lAName: String; lPrefs: TPrefs): boolean;
//Halves X and Y dimensions
label
     666;
var
   lOName: string;
   lo,li,lx,lyz: integer;
   lA,lOut: TNIfTIimg;
begin
     result := false;
     CreateNII(lA);
     CreateNII(lOut);
     if not LoadImgNII(lAName,lA) then
        goto 666;
     if odd(lA.hdr.dim[1]) or odd(lA.hdr.dim[2]) then begin
        ShowMsg('ShrinkNII error X and Y must be divisible by 2 '+inttostr(lA.hdr.dim[1])+' '+inttostr(lA.hdr.dim[2]));
        goto 666;
     end;
     case lA.Hdr.datatype of
          kDT_UNSIGNED_CHAR,kDT_SIGNED_SHORT , kDT_SIGNED_INT, kDT_FLOAT:;//lBPP := 4;
         else begin
             ShowMsg('ShrinkNII datatype not supported.');
             exit;
         end;
     end; //case

     lOut.Hdr := lA.Hdr;
     force3DNII(lOut);
     lOut.hdr.dim[1] := lOut.hdr.dim[1] div 2;
     lOut.hdr.dim[2] := lOut.hdr.dim[2] div 2;
     lOut.hdr.pixdim[1] := lOut.hdr.pixdim[1] * 2;
     lOut.hdr.pixdim[2] := lOut.hdr.pixdim[2] * 2;
     RescaleHdr(lOut.hdr,2,2,1);
     CreateEmptyImgNII(lOut.Hdr, lOut);
     case lA.Hdr.datatype of
          kDT_UNSIGNED_CHAR: begin
              li := 1;
              lo := 1;
              for lyz := 1 to (lOut.hdr.dim[2]*lOut.hdr.dim[3]) do begin
                      for lx := 1 to lOut.hdr.dim[1] do begin
                          lOut.i8^[lo] := (lA.i8^[li]+lA.i8^[li+1]+lA.i8^[li]+lA.i8^[li+1]) div 4;
                          inc(li,2); //skip voxel
                          inc(lo);
                      end;//x
                      inc(li,lA.hdr.dim[1]); //skip line
              end;//yz
             end;//CHAR
          kDT_SIGNED_SHORT: begin
              li := 1;
              lo := 1;
              for lyz := 1 to (lOut.hdr.dim[2]*lOut.hdr.dim[3]) do begin
                      for lx := 1 to lOut.hdr.dim[1] do begin
                          lOut.i16^[lo] := (lA.i16^[li]+lA.i16^[li+1]+lA.i16^[li]+lA.i16^[li+1]) div 4;
                          inc(li,2); //skip voxel
                          inc(lo);
                      end;//x
                      inc(li,lA.hdr.dim[1]); //skip line
              end;//yz
             end;//kDT_SIGNED_SHORT
          kDT_SIGNED_INT: begin
              li := 1;
              lo := 1;
              for lyz := 1 to (lOut.hdr.dim[2]*lOut.hdr.dim[3]) do begin
                      for lx := 1 to lOut.hdr.dim[1] do begin
                          lOut.i32^[lo] := (lA.i32^[li]+lA.i32^[li+1]+lA.i32^[li]+lA.i32^[li+1]) div 4;
                          inc(li,2); //skip voxel
                          inc(lo);
                      end;//x
                      inc(li,lA.hdr.dim[1]); //skip line
              end;//yz
             end;//kDT_SIGNED_INT
          kDT_FLOAT: begin
              li := 1;
              lo := 1;
              for lyz := 1 to (lOut.hdr.dim[2]*lOut.hdr.dim[3]) do begin
                      for lx := 1 to lOut.hdr.dim[1] do begin
                          lOut.f32^[lo] := (lA.f32^[li]+lA.f32^[li+1]+lA.f32^[li]+lA.f32^[li+1]) / 4;
                          inc(li,2); //skip voxel
                          inc(lo);
                      end;//x
                      inc(li,lA.hdr.dim[1]); //skip line
              end;//yz
             end;//float
     end;// datatype
     lOName := ChangeFilePrefix(lAName,'d');
     if SaveNIfTICore (lOName, lOut.Buffer, kNIIImgOffset+1, lOut.Hdr, lPrefs) <> '' then
        result := true;
666:
     FreeNII(lA);
     FreeNII(lOut);
end;


end.
