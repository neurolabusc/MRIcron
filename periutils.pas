unit periutils;
interface
{$IFDEF FPC} {$H+} {$ENDIF}

uses metagraph, define_types, sysutils,nifti_hdr, classes;

function FSLMatName (lFeatDir: string): string;
function FSLFuncName (lFeatDir: string): string;//Given feat folder returns name of filtered data
function FSLReslicedVOIName (lFeatDir, lMNIVOIName: string): string;
procedure RegressTrace (var l4DTrace: T4DTrace);
function ConvertToTrace (var l4DHdr: TMRIcroHdr;var l4DTrace: T4DTrace; lX,lY,lZ: integer): boolean;
function ReadCond (l3ColTextFileName: string; var l4DTrace: T4DTrace; lCond: integer): boolean;
procedure FSLEVNames (lFeatDir: string; var lEVlist: TStringList);

implementation

uses nifti_img_view, text,dialogs,periplot;

function ReadCond (l3ColTextFileName: string; var l4DTrace: T4DTrace; lCond: integer): boolean;
var
   lOnsetText: TextFile;
   lnEvents: integer;
   lFloat,lFloat2,lFloat3: single;
begin
  result := false;
  if (lCond < 1) or (lCond > kMaxCond) then
     exit;
  CloseCond(l4DTrace,lCond);

  Filemode := 0;
  assignfile(lOnsetText,l3ColTextFileName);
  {I-}
  reset(lOnsetText);
  {$I+}
  if ioresult <> 0 then begin
	  Showmessage('Unable to read file [may be in use by another program '+ l3ColTextFileName);
          exit;
  end;
  lnEvents := 0;
  while not EOF(lOnsetText) do begin
	  {$I-}
      read(lOnsetText,lFloat,lFloat2,lFloat3); //read triplets instead of readln: this should load UNIX files
        {$I+}
      if (ioresult = 0) and (lFloat3 > 0) then
           inc(lnEvents);
  end;
  if lnEvents < 1 then begin
     closefile(lOnsetText);
     showmessage('No events detected. Is this really a FSL-style 3 Column format file? '+l3ColTextFileName);
      exit;
  end;
  InitCond (l4DTrace, lCond, lnEvents);
  reset(lOnsetText);
  lnEvents := 0;
  while not EOF(lOnsetText) do begin
      lFloat := 0;
	  {$I-}
      read(lOnsetText,lFloat,lFloat2,lFloat3); //read triplets instead of readln: this should load UNIX files
        {$I+}
      if (ioresult = 0) and (lFloat3 > 0) then begin
           inc(lnEvents);
           l4DTrace.Conditions[lCond].EventRA^[lnEvents] := lFloat;
           l4DTrace.Conditions[lCond].DurRA^[lnEvents] := lFloat2;
           //l4DTrace.DurRA[lCond]^[lnEvents] := lFloat2;
        end;
  end;
  closefile(lOnsetText);
  l4DTrace.Conditions[lCond].ELabel := parsefilename(extractfilename(l3ColTextFileName));
  result := true;
end;

function ConvertToTrace (var l4DHdr: TMRIcroHdr;var l4DTrace: T4DTrace; lX,lY,lZ: integer): boolean;
var
   lVol,lVolSz,lPos,lSamples,lLine,lnLines,lROI: integer;
   l16Buf : SmallIntP;
   l32Buf : SingleP;
begin
     result := false;
     lSamples := l4DHdr.NIFTIhdr.dim[4];
     lVolSz :=  l4DHdr.NIFTIhdr.dim[1]*l4DHdr.NIFTIhdr.dim[2]*l4DHdr.NIFTIhdr.dim[3];
     if lSamples < 2 then
        exit;

     lnLines := 0;
     for lVol := (kBGOverlayNum+1) to knMaxOverlay do
         if   gMRIcroOverlay[lVol].ScrnBufferItems > 0 then //for each ROI
              inc(lnLines);
     if lnLines = 0 then begin //no ROIs
        lLine := 1;
        lPos := lX + ((lY-1)*gBGImg.ScrnDim[1])+((lZ-1)*gBGImg.ScrnDim[1]*gBGImg.ScrnDim[2]);
        if (lPos > l4DHdr.ImgBufferItems) or (lPos < 1) then exit;
        Init4DTrace(lSamples, 1,l4DTrace,false);
        l4DTrace.Lines[1].ELabel := inttostr(lX)+'x'+inttostr(lY)+'x'+inttostr(lZ);
        if (l4DHdr.ImgBufferBPP  = 4) then begin
	   l32Buf := SingleP(l4DHdr.ImgBuffer );
           for lVol := 1 to lSamples do begin
	    l4DTrace.Lines[lLine].EventRA[lVol] := l32Buf[lPos];
            lPos := lPos + lVolSz;
           end;
        end else if (l4DHdr.ImgBufferBPP  = 2) then begin
	   l16Buf := SmallIntP(l4DHdr.ImgBuffer );
           for lVol := 1 to lSamples do begin
               l4DTrace.Lines[lLine].EventRA^[lVol] := l16Buf^[lPos];
               lPos := lPos + lVolSz;
           end;
        end else if l4DHdr.ImgBufferBPP  = 1 then begin
           for lVol := 1 to lSamples do begin
               l4DTrace.Lines[lLine].EventRA^[lVol] := l4DHdr.ImgBuffer^[lPos];
               lPos := lPos + lVolSz;
           end;
        end else
         showmessage('Serious error: unknown data size!');
     end else begin //>0 ROIS
        Init4DTrace(lSamples, lnLines,l4DTrace,false);
        for lLine := 1 to lnLines do begin
            lROI := ROIoverlayNum(lLine);
            l4DTrace.Lines[lLine].ELabel := ParseFileName(extractfilename(gMRIcroOverlay[lROI].HdrFileName));
            for lVol := 1 to lSamples do
	        l4DTrace.Lines[lLine].EventRA^[lVol] := ROImean(l4DHdr,lROI,lVol{,lVolSz});
        end;
     end;
     MinMax4DTrace(l4DTrace);
     result := true;
end;

function ComputeRegress (ldataRA: singlep; lndata: integer): string;
const
  //kMax = 1000;
  kCR = chr (13);
Var
  gx : Array[1..4] of extended;
  gy : Array[1..4] of extended;
  Exy : Array[1..4] of extended;
  Ex : Array[1..4] of extended;
  Ey : Array[1..4] of extended;
  Ex2 : Array[1..4] of extended;
  Ey2 : Array[1..4] of extended;
  a : Array[1..4] of extended;
  b : Array[1..4] of extended;
  r : Array[1..4] of extended;
  chtX: Array[1..4] of extended;
  chtY: Array[1..4] of extended;
  no : Integer;
  gInter, gSlope,gRSqr : extended;

function calcit: string;
Var
  q : Integer;
Begin

For q := 1 To 4 Do Begin
  b[q] := (no * Exy[q] - Ex[q] * Ey[q]) / (no * Ex2[q] - (Ex[q]*Ex[q]) );
  a[q] := (Ey[q] - b[q] * Ex[q]) / no;
  r[q] := (no * Exy[q] - Ex[q] * Ey[q]) / (Sqrt((no * Ex2[q] - (Ex[q]*Ex[q]) ) * (no * Ey2[q] - (Ey[q]*Ey[q]) ) ));
End; // for
a[2] := Exp(a[2]);
a[4] := Exp(a[4]);
result :=  ('  Linear  Y=' + RealToStr(a[1],8) + ' +' + RealToStr(b[1],8) + ' * X'+' R=' + RealToStr(r[1],8)+' R^2=' + RealToStr(r[1]*r[1],8));
gInter := a[1];
gSlope := b[1];
gRSqr := r[1];
result := result + (',  Exp  Y=' + RealToStr(a[2],8) + ' * e ^' + RealToStr(b[2],8) + ' * X'+' R=' + RealToStr(r[2],8)+' R^2=' + RealToStr(r[2]*r[2],8));
result := result + (',  Log  Y=' + RealToStr(a[3],8) + ' +' + RealToStr(b[3],8) + ' * LOG(X)'+' R=' + RealToStr(r[3],8)+' R^2=' + RealToStr(r[3]*r[3],8));
result := result +(',  Power  Y=' + RealToStr(a[4],8) + ' * X ^' + RealToStr(b[4],8)+' R=' + RealToStr(r[4],8)+' R^2=' + RealToStr(r[4]*r[4],8));
End; // nested calcit()
Procedure inpcalc (lX, lY: extended);
Var
  q : Integer;
Begin
gx[1] := lX;
gy[1] := lY;
//inc(gnVal);
inc(no);
   gx[2] := gx[1];
 gy[2] := Ln(gy[1]);  //  exp
gx[3] := Ln(gx[1]);
 gy[3] := gy[1];  //  log
gx[4] := Ln(gx[1]);
 gy[4] := Ln(gy[1]);  // power

For q := 1 To 4 Do Begin
  Exy[q] := Exy[q] + gx[q] * gy[q];
  Ex[q] := Ex[q] + gx[q];
  Ey[q] := Ey[q] + gy[q];
  Ex2[q] := Ex2[q] + (gx[q]*gx[q]);
  Ey2[q] := Ey2[q] + (gy[q]*gy[q]);
 End; // For
End; //nested  inpcalc
procedure initReg;
var lC: byte;
begin
for lC := 1 to 4 do begin
  gx [lC]:= 0;
  gy [lC]:= 0;
  Exy [lC]:= 0;
  Ex[lC]:= 0;
  Ey[lC]:= 0;
  Ex2[lC]:= 0;
  Ey2[lC]:= 0;
  a[lC]:= 0;
  b[lC]:= 0;
  r[lC]:= 0;
  chtX[lC]:= 0;
  chtY[lC]:= 0;
  end; //for lC
end;//nested inp calc
const
     kDeleteVols = 3;
var
   i: integer;
begin  //computeRegress
  result := '';
       no := 0;
  if lndata < (kDeleteVols+5) then exit;

  //gnVal := 0;
  initReg;
  for i := kDeleteVols to lndata do begin
      //fx(i,ldatara[i]);
      inpcalc (i, ldataRA^[i]);
  end;
  result := calcit;
end; //func ComputeRegress

procedure RegressTrace (var l4DTrace: T4DTrace);
var
   lStr: string;
   lE,lCond, lnCond,lnE: integer;
   lMean : double;
   ldataRA: singlep;
begin
     lncond := 0;
     for lCond := 1 to kMaxCond do
         if l4DTrace.Lines[lCond].Events > 0 then
            inc(lnCond);
     if lncond = 0 then
     exit;
     for lCond := 1 to kMaxCond do begin
         if l4DTrace.Lines[lCond].Events > 0 then begin
            lnE := l4DTrace.Lines[lCond].Events;
            getmem(ldataRA,lnE * sizeof(single));
            lStr := gMRIcroOverlay[kBGOverlayNum].HdrFileName+','+l4DTrace.Lines[lCond].ELabel;
            //load data
            lMean := 0;
            for lE := 1 to lnE do begin
                ldataRA[lE] := l4DTrace.Lines[lCond].EventRA[lE];
                lMean := ldataRA^[lE] + lMean; //sum
            end;

            lMean := lMean / lnE;
            //fx(lMean);
            //normalize data...
            for lE := 1 to lnE do
                ldataRA^[lE] := ldataRA^[lE]/lMean;
            //compute functions
            lStr := lStr +kTextSep+ (ComputeRegress (ldataRA, lnE) );
            TextForm.MemoT.lines.add(lStr);
            //TextForm.Memo1.lines.add(lStr);
            freemem(ldataRA);
         end;
     end;
     //TextForm.show;
end;

//NEXT SECTION - FSL UTILITIES
procedure FSLEVNames (lFeatDir: string; var lEVlist: TStringList);
//Given feat folder returns name of matrix to reorient MNI image to functional data
var
   lEVdir : string;
   lSearchRec: TSearchRec;
begin
     lEVList.clear;
     lEVdir := lFEATDir+pathdelim+'custom_timing_files';
//showmessage(lEVdir);
     if not DirExists(lEVdir) then
        exit;
//showmessage(lEVdir);
     if FindFirst(lEVdir+pathdelim+'*'+'.txt', faAnyFile, lSearchRec) = 0 then begin
	    repeat
		        lEVlist.Add(lEVdir+pathdelim+lSearchRec.Name)
        until (FindNext(lSearchRec) <> 0);
     end;
     FindClose(lSearchRec);
//fx(lEVlist.count);
     //result := lFeatDir+PathDelim+'reg'+PathDelim+'example_func2standard.mat';
end; //MatName



function FSLMatName (lFeatDir: string): string;
//Given feat folder returns name of matrix to reorient MNI image to functional data
begin
     result := lFeatDir+PathDelim+'reg'+PathDelim+'example_func2standard.mat';
end; //MatName

function FSLFuncName (lFeatDir: string): string;//Given feat folder returns name of filtered data
begin
     result := lFeatDir+PathDelim+'filtered_func_data.nii.gz';
end; //FuncName

function FSLReslicedVOIName (lFeatDir, lMNIVOIName: string): string;
//Given FSL .feat folder name and source MNI volume name retuns resliced VOI name
begin
     result := lFeatDir+PathDelim+extractfilename(lMNIVOIName);


     (*result := extractfilename(lMNIVOIName);
     result := lFeatDir+PathDelim+ChangeFileExtX(result,'.nii.gz'); //;
       *)
end; //ReslicedVOIName


end.
