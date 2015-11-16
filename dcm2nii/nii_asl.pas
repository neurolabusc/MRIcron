unit nii_asl;
//tools for Arterial spin labeling...
{$H+}


interface

uses
//{$IFDEF FPC}gzio2,{$ENDIF}
  SysUtils,define_types,dicomtypes,niftiutil,prefs,dialogs_msg, nifti_types;

function ASL_subtract(var lHdrName: string;  lOverwrite: boolean; lFunction : integer; lPrefs: TPrefs): boolean;
//ASL_subtract(var lHdrName: string;  lOverwrite: boolean; lFunction : integer; lPrefs: TPrefs)

implementation
uses gui,dialogs;

function Parse (var lNumStr: string; var Val1,Val2: integer): boolean;
var
   lS: string;
   lLen,lP: integer;
begin
    Val1 := 0;
    Val2 := 0;
    result := false;
    lLen := length(lNumStr);
    if lLen < 3 then exit;
    lS := '';
    lP := 1;
    while (lP <= lLen) and (lNumStr[lP] <> ',') do begin
          lS := lS + lNumStr[lP];
          inc(lP);
    end;
    if lS = '' then
       exit;
    try
       Val1 := strtoint(lS);
    except
          dcmMsg('Error converting text to number '+lS);
    end;
    //next number
    inc(lP);
    lS := '';
    while (lP <= lLen) and (lNumStr[lP] <> ',') do begin
          lS := lS + lNumStr[lP];
          inc(lP);
    end;
    if lS = '' then
       exit;
    try
       Val2 := strtoint(lS);
    except
          dcmMsg('Error converting text to number '+lS);
    end;
    result := true;
end;

function readCSV (lFilename: string;  var lnObservations: integer; var lPosRA,lNegRA: LongIntp): boolean;
var
   lNumStr: string;
   F: TextFile;
   R,A,B,C: integer;
begin
     lnObservations := 0;
     result := false;
     if not fileexists(lFilename) then exit;
     AssignFile(F, lFilename);
     FileMode := 0;  //Set file access to read only
     //First pass: determine column height/width
     Reset(F);
     R := 0;
     while (not Eof(F))  do begin
           Readln(F,lNumStr);
           if Parse(lNumStr,A,B) then
              inc(R);
     end;
     if (R < 1)  then begin
         dcmMsg('problems reading CSV: must have at least 2 columns and 1 row.');
         exit;
     end;
     lnObservations := R;
     Getmem(lPosRA,lnObservations*sizeof(integer));
     Getmem(lNegRA,lnObservations*sizeof(integer));
     for R := 1 to lnObservations do begin
         lPosRA^[R] := -1;
         lNegRA^[R] := -1;
     end;
     //second pass
     Reset(F);
     C := 0;
     while (not Eof(F)) and (C<lnObservations) do begin
           Readln(F,lNumStr);
           if Parse(lNumStr,A,B ) then begin
               inc(C);
               lPosRA^[C] := A;
               lNegRA^[C] := B;
           end;
     end;
     CloseFile(F);
	 FileMode := 2;  //Set file access to read/write
         result := true;
end;
//start 32-bit float versions
function SaveMean32 (var lHdrName: string;var lInHdr: TNIFTIhdr; lBuffAligned: bytep;lImgBuffer : SingleP ;lPrefs: TPrefs ): string;
var
   lOutHdr : TNIFTIhdr;
   lOutImgName: string;
   lVol,lOutVolOffset,lInc,lImgSamples: integer;
   lSum: double;
begin
     result := '';
     lOutHdr := lInHdr;
     lImgSamples := lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3];
     if (lImgSamples < 1) or (lInHdr.dim[4] < 1) then
        exit;
   //next make mean
   lOutImgName := ChangeFilePrefix (lHdrName,'mean');
   for lInc := 1 to lImgSamples do  begin
       lSum := 0;
       lOutVolOffset := 0;
       for lVol := 1 to  lInHdr.dim[4] do begin
           lSum := lSum+lImgBuffer^[lOutVolOffset+lInc];
           lOutVolOffset := lOutVolOffset+lImgSamples;
       end;
       lImgBuffer^[lInc] :=  (lSum /lOutHdr.dim[4]);
   end;

   lOutHdr.dim[4] := 1;
   result :=  SaveNIfTICore (lOutImgName, lBuffAligned, kNIIImgOffset+1, lOutHdr, lPrefs);
end; //SaveMean32


function ASL_subtract32(var lHdrName: string; lOverwrite: boolean; lFunction : integer; lPrefs: TPrefs): boolean;
//lFunction :
//0 = Subtract (even-odd)
//1= Subtract (odd-even)
//2= Subtract custom
//3= Add (odd+even) BOLD
//4= Parse OddEven  aka SplitOddEven
//for odd/even, first scan is odd (1), second scan is even (2) [indexed from 1]
label
 666;
var
   lPosRA,lNegRA: LongIntp;
   lImgOffset,lVol,lInVolOddOffset,lInVolEvenOffset,lOutVolOffset,
   lImgSamples,l4DVox,lInc: integer;
   lCSVname,lResultStr,lOutImgName: string;
   lInHdr, lOutHdr : TNIFTIhdr;
   lO: TNIIOpts;
   lSplitOddEven,lCustom,lSubtract,lOddMinusEven: boolean;
   lBuffer,lSrcBuffer, lBuffUnaligned,lBuffAligned: bytep;
   lOdd,lEven: single;
   lOddVol,lEvenVol: integer;
   lImgBuffer,l32Buf : SingleP;
   //lSum: double;
begin
     lSplitOddEven := lFunction = 4;
     lCustom := lFunction = 2;
     lOddMinusEven := lFunction = 1;
     lSubtract := (lFunction <> 3);
   result := false;
     if not NIFTIhdr_LoadHdr (lHdrName, lInHdr, lO) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lHdrName);
        exit;
     end;
   case lInHdr.datatype of
     {kDT_UNSIGNED_CHAR,kDT_SIGNED_SHORT,kDT_UINT16, kDT_SIGNED_INT,}kDT_FLOAT:;//Supported
     else begin
         dcmMsg('Error with ASL_subtract: image format must be 32-bit floating point values.');
         exit;
     end;
   end;//case headertype
   if (odd(lInHdr.dim[4])) or (lInHdr.dim[4] < 2) then begin
      dcmMsg('ASL routines require an even number of volumes');
      exit;
   end;

   if not NIFTIhdr_LoadImg (lHdrName, lInHdr, lSrcBuffer, lImgOffset,lO) then  exit;
   lBuffer := (@lSrcBuffer^[lImgOffset+1]);
   l32Buf := SingleP(lBuffer );
   if lOverwrite then
       lOutImgName := lHdrName
   else begin
        if lSplitOddEven then
          lOutImgName := ChangeFilePrefix (lHdrName,'odd')
       else if lSubtract then begin
          if lCustom then
              lOutImgName := ChangeFilePrefix (lHdrName,'subc')
          else if lOddMinusEven then
              lOutImgName := ChangeFilePrefix (lHdrName,'subome')
          else
              lOutImgName := ChangeFilePrefix (lHdrName,'subemo')
       end else
          lOutImgName := ChangeFilePrefix (lHdrName,'add');
   end;
   lOutHdr := lInHdr;
   lOutHdr.dim[4] := lInHdr.dim[4] div 2;
   lOutHdr.pixdim[4] := 2 * lInHdr.pixdim[4];
   lImgSamples := lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3];
   l4DVox := lImgSamples * lOutHdr.dim[4];
   //l3DBytes := l3DVox*(lOutHdr.bitpix div 8);
   //l4DBytes := l3DBytes*lOutHdr.dim[4];
   if lCustom then
         lResultStr := 'Custom subtraction'
   else if lSubtract then begin
      if lOddMinusEven then
         lResultStr := 'Subtract Odd-Even'
      else
         lResultStr := 'Subtract Even-Odd';
   end else
       lResultStr := 'Add Odd+Even';
   dcmMsg('Computing '+lResultStr+'  '+lHdrName);
   lResultStr := ''; //assume error
   GetMem(lBuffUnaligned ,(sizeof(single)*l4DVox) + 16+kNIIImgOffset);
   {$IFDEF FPC}
    lBuffAligned := Align(lBuffUnaligned,16); // not commented - check this
   {$ELSE}
   lBuffAligned := ByteP($fffffff0 and (integer(lBuffUnaligned)+15));
   {$ENDIF}
   lInc := 1;
   lImgBuffer := SingleP(@lBuffAligned^[kNIIImgOffset+lInc]);
   
   if lSplitOddEven then begin
   //compute odd
   for lVol := 1 to  lOutHdr.dim[4] do begin
       lOutVolOffset := (lVol -1) * lImgSamples;
       //lInVolEvenOffset := ((lVol*2) -1) * lImgSamples; //second, fourth
       lInVolOddOffset := ((lVol*2) -2) * lImgSamples; //first, thired
       for lInc := 1 to lImgSamples do  begin
               lOdd := l32Buf^[lInVolOddOffset+lInc];
               //lEven := l16Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lOdd;
       end; //for lImgSamples
   end; //for lvol
   lresultStr :=  SaveNIfTICore (lOutImgName, lBuffAligned, kNIIImgOffset+1, lOutHdr, lPrefs);
   lresultStr :=  SaveMean32 (lOutImgName, lOutHdr, lBuffAligned,lImgBuffer,lPrefs);
   //compute even
   lOutImgName := ChangeFilePrefix (lHdrName,'even');
   for lVol := 1 to  lOutHdr.dim[4] do begin
       lOutVolOffset := (lVol -1) * lImgSamples;
       lInVolEvenOffset := ((lVol*2) -1) * lImgSamples; //second, fourth
       //lInVolOddOffset := ((lVol*2) -2) * lImgSamples; //first, thired
       for lInc := 1 to lImgSamples do  begin
               //lOdd := l16Buf^[lInVolOddOffset+lInc];
               lEven := l32Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lEven;
       end; //for lImgSamples
   end; //for lvol
end else if lCustom then begin //not lSplitOddEven .. if Custom
   dcmMsg('Select a comma separated text file that describes how to subtract images.');
   dcmMsg('For example, to subtract a six volume dataset your file could be:');
   dcmMsg('1,6');
   dcmMsg('2,5');
   dcmMsg('3,4');
   dcmMsg('The first output volume would be the first input volume minus the sixth');
   dcmMsg('The second output volume would be the second input volume minus the fifth');
   dcmMsg('The final output volume would be the third input volume minus the fourth');
   dcmMsg('Your file should have '+inttostr(lOutHdr.dim[4])+' lines, one for each output volume');

   if not MainForm.OpenDialogExecute('Select NIfTI images you wish to modify)',true,false,kTxtFilter) then
    goto 666;
   lCSVName := MainForm.OpenHdrDlg.Filename;
   //MainForm.BrowseDialog('Choose collapase file
   if not readCSV (lCSVname, lVol, lPosRA,lNegRA) then
      goto 666;
   if lVol < lOutHdr.dim[4] then begin
      dcmMsg ('Only found '+inttostr(lVol)+' contrasts in '+ lCSVName+' a total of '+inttostr(lOutHdr.dim[4])+' required');
      freemem(lPosRA);
      freemem(lNegRA);
      goto 666;
   end;
   for lVol := 1 to  lOutHdr.dim[4] do begin
       lOutVolOffset := (lVol -1) * lImgSamples;
       lEvenVol := lNegRA^[lVol];
       lOddVol := lPosRA^[lVol];
       if (lEvenVol > 0) and (lOddVol > 0) and (lEvenVol <= lInHdr.dim[4]) and (lOddVol <= lInHdr.dim[4]) then begin
          dcmMsg (inttostr(lVol) +' = '+inttostr(lOddVol)+' - '+inttostr(lEvenVol) );
          lInVolEvenOffset := (lEvenVol -1) * lImgSamples;
          lInVolOddOffset := (lOddVol -1) * lImgSamples;
          for lInc := 1 to lImgSamples do  begin
               lOdd := l32Buf^[lInVolOddOffset+lInc];
               lEven := l32Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lOdd - lEven;
          end; //each voxel in volume
       end else begin
          dcmMsg('Error: volumes out of range '+inttostr(lVol) +' = '+inttostr(lOddVol)+' - '+inttostr(lEvenVol) );
       end;
   end; //for lvol
      freemem(lPosRA);
      freemem(lNegRA);
end else begin  //not custom  or lSplitOddEven
   for lVol := 1 to  lOutHdr.dim[4] do begin
       lOutVolOffset := (lVol -1) * lImgSamples;
       lInVolEvenOffset := ((lVol*2) -1) * lImgSamples; //second, fourth
       lInVolOddOffset := ((lVol*2) -2) * lImgSamples; //first, thired
       if lSubtract then begin
          if lOddMinusEven then begin
             for lInc := 1 to lImgSamples do  begin
               lOdd := l32Buf^[lInVolOddOffset+lInc];
               lEven := l32Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lOdd - lEven;
             end; //for lImgSamples
          end else begin //not lOddMinusEven
             for lInc := 1 to lImgSamples do  begin
               lOdd := l32Buf^[lInVolOddOffset+lInc];
               lEven := l32Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lEven-lOdd;
             end; //for lImgSamples
          end; //if else lOddMinusEven
       end else begin //not subtract... add
           for lInc := 1 to lImgSamples do  begin
               lOdd := l32Buf^[lInVolOddOffset+lInc];
               lEven := l32Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lOdd + lEven;
           end; //for lImgSamples
       end; //add
   end; //for lvol
end;
   lresultStr :=  SaveNIfTICore (lOutImgName, lBuffAligned, kNIIImgOffset+1, lOutHdr, lPrefs);
   //next make mean
   lresultStr :=  SaveMean32 (lOutImgName, lOutHdr, lBuffAligned,lImgBuffer,lPrefs);
666:
   Freemem(lSrcBuffer);
   Freemem(lBuffUnaligned);
   if lResultStr = '' then //error - do not report success
      exit;
   result := true;
end; //ASL_subtract32
//end 32bit versions


function SaveMean (var lHdrName: string;var lInHdr: TNIFTIhdr; lInBuffer : SmallIntP; lPrefs: TPrefs ): string;
var
   lOutHdr : TNIFTIhdr;
   lOutImgName: string;
   lVol,lOutVolOffset,lInc,lImgSamples: integer;
   lSum: double;
   lBuffUnaligned,lBuffAligned: bytep;
   lImgBuffer: Singlep;
begin
     result := '';
     lOutHdr := lInHdr;
     lImgSamples := lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3];
     if (lImgSamples < 1) or (lInHdr.dim[4] < 1) then
        exit;
   GetMem(lBuffUnaligned ,(sizeof(single)*lImgSamples) + 16+kNIIImgOffset);
   {$IFDEF FPC}
    lBuffAligned := Align(lBuffUnaligned,16); // not commented - check this
   {$ELSE}
   lBuffAligned := ByteP($fffffff0 and (integer(lBuffUnaligned)+15));
   {$ENDIF}
   lInc := 1;
   lImgBuffer := SingleP(@lBuffAligned^[kNIIImgOffset+lInc]);
   //next make mean
   lOutImgName := ChangeFilePrefix (lHdrName,'mean');
   for lInc := 1 to lImgSamples do  begin
       lSum := 0;
       lOutVolOffset := 0;
       for lVol := 1 to  lInHdr.dim[4] do begin
           lSum := lSum+lInBuffer^[lOutVolOffset+lInc];
           lOutVolOffset := lOutVolOffset+lImgSamples;
       end;
       lImgBuffer^[lInc] :=  (lSum /lOutHdr.dim[4]);
   end;

   lOutHdr.dim[4] := 1;
   lOutHdr.datatype := kDT_FLOAT;
   lOutHdr.bitpix := 32;
   result :=  SaveNIfTICore (lOutImgName, lBuffAligned, kNIIImgOffset+1, lOutHdr, lPrefs);
   freemem( lBuffUnaligned);
end; //SaveMean

(*function SaveMean (var lHdrName: string;var lInHdr: TNIFTIhdr; lBuffAligned: bytep;lImgBuffer : SmallIntP;lPrefs: TPrefs; lByteSwap:boolean ): string;
var
   lOutHdr : TNIFTIhdr;
   lOutImgName: string;
   lVol,lOutVolOffset,lInc,lImgSamples: integer;
   lSum: double;
begin
     result := '';
     lOutHdr := lInHdr;
     lImgSamples := lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3];
     if (lImgSamples < 1) or (lInHdr.dim[4] < 1) then
        exit;
   //next make mean
   lOutImgName := ChangeFilePrefix (lHdrName,'mean');
   for lInc := 1 to lImgSamples do  begin
       lSum := 0;
       lOutVolOffset := 0;
       for lVol := 1 to  lInHdr.dim[4] do begin
           lSum := lSum+lImgBuffer^[lOutVolOffset+lInc];
           lOutVolOffset := lOutVolOffset+lImgSamples;
       end;
       lImgBuffer^[lInc] :=  round(lSum /lOutHdr.dim[4]);
   end;

   lOutHdr.dim[4] := 1;
   result :=  SaveNIfTICore (lOutImgName, lBuffAligned, kNIIImgOffset+1, lOutHdr, lPrefs,lByteSwap);
end; //SaveMean*)

function ASL_subtract(var lHdrName: string; lOverwrite: boolean; lFunction : integer; lPrefs: TPrefs): boolean;
//lFunction :
//0 = Subtract (even-odd)
//1= Subtract (odd-even)
//2= Subtract custom
//3= Add (odd+even) BOLD
//4= Parse OddEven  aka SplitOddEven
//for odd/even, first scan is odd (1), second scan is even (2) [indexed from 1]
label
 666;
var
   lPosRA,lNegRA: LongIntp;
   lImgOffset,lVol,lInVolOddOffset,lInVolEvenOffset,lOutVolOffset,
   lOddVol,lEvenVol: integer;
   lImgSamples,l4DVox,lInc: integer;
   lCSVname,lResultStr,lOutImgName: string;
   lInHdr, lOutHdr : TNIFTIhdr;
   lO: TNIIOpts;
   lSplitOddEven,lCustom,lSubtract,lOddMinusEven: boolean;
   lBuffer,lSrcBuffer, lBuffUnaligned,lBuffAligned: bytep;
   lOdd,lEven: integer;
   lImgBuffer,l16Buf : SmallIntP;
   //lSum: double;
begin
     lSplitOddEven := lFunction = 4;
     lCustom := lFunction = 2;
     lOddMinusEven := lFunction = 1;
     lSubtract := (lFunction <> 3);
   result := false;
     if not NIFTIhdr_LoadHdr (lHdrName, lInHdr, lO) then begin
        dcmMsg('Unable to read as NifTI/Analyze' + lHdrName);
        exit;
     end;
   case lInHdr.datatype of
     {kDT_UNSIGNED_CHAR,}kDT_SIGNED_SHORT{,kDT_UINT16, kDT_SIGNED_INT,kDT_FLOAT}:;//Supported
     kDT_FLOAT: begin
                result := ASL_subtract32(lHdrName,lOverwrite,lFunction,lPrefs);
                exit;                   
     end;//kDT_FLOAT
     else begin
         dcmMsg('Error with ASL_subtract: image format must be 16-bit signed integers.');
         exit;
     end;
   end;//case headertype
   if (odd(lInHdr.dim[4])) or (lInHdr.dim[4] < 2) then begin
      dcmMsg('ASL routines require an even number of volumes');
      exit;
   end;

   if not NIFTIhdr_LoadImg (lHdrName, lInHdr, lSrcBuffer, lImgOffset,lO) then  exit;
   lBuffer := (@lSrcBuffer^[lImgOffset+1]);
   l16Buf := SmallIntP(lBuffer );
   if lOverwrite then
       lOutImgName := lHdrName
   else begin
        if lSplitOddEven then
          lOutImgName := ChangeFilePrefix (lHdrName,'odd')
       else if lSubtract then begin
          if lCustom then
              lOutImgName := ChangeFilePrefix (lHdrName,'subc')
          else if lOddMinusEven then
              lOutImgName := ChangeFilePrefix (lHdrName,'subome')
          else
              lOutImgName := ChangeFilePrefix (lHdrName,'subemo')
       end else
          lOutImgName := ChangeFilePrefix (lHdrName,'add');
   end;
   lOutHdr := lInHdr;
   lOutHdr.dim[4] := lInHdr.dim[4] div 2;
   lOutHdr.pixdim[4] := 2 * lInHdr.pixdim[4];
   lImgSamples := lOutHdr.dim[1]*lOutHdr.dim[2]*lOutHdr.dim[3];
   l4DVox := lImgSamples * lOutHdr.dim[4];
   //l3DBytes := l3DVox*(lOutHdr.bitpix div 8);
   //l4DBytes := l3DBytes*lOutHdr.dim[4];
   if lCustom then
         lResultStr := 'Custom subtraction'
   else if lSubtract then begin
      if lOddMinusEven then
         lResultStr := 'Subtract Odd-Even'
      else
         lResultStr := 'Subtract Even-Odd';
   end else
       lResultStr := 'Add Odd+Even';
   dcmMsg('Computing '+lResultStr+'  '+lHdrName);
   lResultStr := ''; //assume error
   GetMem(lBuffUnaligned ,(sizeof(smallint)*l4DVox) + 16+kNIIImgOffset);
   {$IFDEF FPC}
    lBuffAligned := Align(lBuffUnaligned,16); // not commented - check this
   {$ELSE}
   lBuffAligned := ByteP($fffffff0 and (integer(lBuffUnaligned)+15));
   {$ENDIF}
   lInc := 1;
   lImgBuffer := SmallIntP(@lBuffAligned^[kNIIImgOffset+lInc]);
if lSplitOddEven then begin
   //compute odd
   for lVol := 1 to  lOutHdr.dim[4] do begin
       lOutVolOffset := (lVol -1) * lImgSamples;
       //lInVolEvenOffset := ((lVol*2) -1) * lImgSamples; //second, fourth
       lInVolOddOffset := ((lVol*2) -2) * lImgSamples; //first, thired
       for lInc := 1 to lImgSamples do  begin
               lOdd := l16Buf^[lInVolOddOffset+lInc];
               //lEven := l16Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lOdd;
       end; //for lImgSamples
   end; //for lvol
   lresultStr :=  SaveNIfTICore (lOutImgName, lBuffAligned, kNIIImgOffset+1, lOutHdr, lPrefs);
   lresultStr :=  SaveMean (lOutImgName, lOutHdr, lImgBuffer,lPrefs);
   //compute even
   lOutImgName := ChangeFilePrefix (lHdrName,'even');
   for lVol := 1 to  lOutHdr.dim[4] do begin
       lOutVolOffset := (lVol -1) * lImgSamples;
       lInVolEvenOffset := ((lVol*2) -1) * lImgSamples; //second, fourth
       //lInVolOddOffset := ((lVol*2) -2) * lImgSamples; //first, thired
       for lInc := 1 to lImgSamples do  begin
               //lOdd := l16Buf^[lInVolOddOffset+lInc];
               lEven := l16Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lEven;
       end; //for lImgSamples
   end; //for lvol
end else if lCustom then begin //not lSplitOddEven .. if Custom
   dcmMsg('Select a comma separated text file that describes how to subtract images.');
   dcmMsg('For example, to subtract a six volume dataset your file could be:');
   dcmMsg('1,6');
   dcmMsg('2,5');
   dcmMsg('3,4');
   dcmMsg('The first output volume would be the first input volume minus the sixth');
   dcmMsg('The second output volume would be the second input volume minus the fifth');
   dcmMsg('The final output volume would be the third input volume minus the fourth');
   dcmMsg('Your file should have '+inttostr(lOutHdr.dim[4])+' lines, one for each output volume');

   if not MainForm.OpenDialogExecute('Select NIfTI images you wish to modify)',true,false,kTxtFilter) then
    goto 666;
   lCSVName := MainForm.OpenHdrDlg.Filename;
   //MainForm.BrowseDialog('Choose collapase file
   if not readCSV (lCSVname, lVol, lPosRA,lNegRA) then
      goto 666;
   if lVol < lOutHdr.dim[4] then begin
      dcmMsg('Only found '+inttostr(lVol)+' contrasts in '+ lCSVName+' a total of '+inttostr(lOutHdr.dim[4])+' required');
      freemem(lPosRA);
      freemem(lNegRA);
      goto 666;
   end;
   for lVol := 1 to  lOutHdr.dim[4] do begin
       lOutVolOffset := (lVol -1) * lImgSamples;
       lEvenVol := lNegRA^[lVol];
       lOddVol := lPosRA^[lVol];
       if (lEvenVol > 0) and (lOddVol > 0) and (lEvenVol <= lInHdr.dim[4]) and (lOddVol <= lInHdr.dim[4]) then begin
          dcmMsg(inttostr(lVol) +' = '+inttostr(lOddVol)+' - '+inttostr(lEvenVol) );
          lInVolEvenOffset := (lEvenVol -1) * lImgSamples; //second, fourth
          lInVolOddOffset := (lOddVol -1) * lImgSamples; //first, thired
          for lInc := 1 to lImgSamples do  begin
               lOdd := l16Buf^[lInVolOddOffset+lInc];
               lEven := l16Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lOdd - lEven;
          end; //each voxel in volume
       end else begin
          dcmMsg('Error: volumes out of range '+inttostr(lVol) +' = '+inttostr(lOddVol)+' - '+inttostr(lEvenVol) );
       end;
   end; //for lvol
      freemem(lPosRA);
      freemem(lNegRA);
end else begin  //not custom  or lSplitOddEven
   for lVol := 1 to  lOutHdr.dim[4] do begin
       lOutVolOffset := (lVol -1) * lImgSamples;
       lInVolEvenOffset := ((lVol*2) -1) * lImgSamples; //second, fourth
       lInVolOddOffset := ((lVol*2) -2) * lImgSamples; //first, thired
       if lSubtract then begin
          if lOddMinusEven then begin
             for lInc := 1 to lImgSamples do  begin
               lOdd := l16Buf^[lInVolOddOffset+lInc];
               lEven := l16Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lOdd - lEven;
             end; //for lImgSamples
          end else begin //not lOddMinusEven
             for lInc := 1 to lImgSamples do  begin
               lOdd := l16Buf^[lInVolOddOffset+lInc];
               lEven := l16Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lEven-lOdd;
             end; //for lImgSamples
          end; //if else lOddMinusEven
       end else begin //not subtract... add
           for lInc := 1 to lImgSamples do  begin
               lOdd := l16Buf^[lInVolOddOffset+lInc];
               lEven := l16Buf^[lInVolEvenOffset+lInc];
               lImgBuffer^[lOutVolOffset+lInc] := lOdd + lEven;
           end; //for lImgSamples
       end; //add
   end; //for lvol
end;
   lresultStr :=  SaveNIfTICore (lOutImgName, lBuffAligned, kNIIImgOffset+1, lOutHdr, lPrefs);
   //next make mean
   lresultStr :=  SaveMean (lOutImgName, lOutHdr, lImgBuffer,lPrefs);
666:
   Freemem(lSrcBuffer);
   Freemem(lBuffUnaligned);
   if lResultStr = '' then //error - do not report success
      exit;
   result := true;
end; //ASL_subtract

end.