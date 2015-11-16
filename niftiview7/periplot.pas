unit periplot;
//peristimulus plotting routines

interface
uses
    nifti_hdr,define_types,metagraph,sysutils;

function CreatePeristimulusPlot (var l4DHdr: TMRIcroHdr; var l4DTrace: T4DTrace;
var lPSPlot: TPSPlot): boolean;
function ROIoverlayNameShort(lROI: integer): string;
function numROI: integer;
function ROIoverlayNum(lROI: integer): integer;
function NCond ( var l4DTrace: T4DTrace): integer;
function ROImean (var l4DHdr: TMRIcroHdr; lROInum,lVol: integer): double;

implementation


{$IFNDEF FPC}
{$DEFINE REMOVEREGRESS}
{$ENDIF}
uses nifti_img_view,dialogs,nifti_img,text,graphx,math
{$IFDEF REMOVEREGRESS},fmath, hrf,
matrices,Regmult{$ENDIF}; //need to specify path, e.g. C:\pas\mricron\npm\math


//var   gOffsetError: array [1..kMaxCond] of double;

function numROI: integer;
var
   lR: integer;
begin
     result := 0;
     for lR := (kBGOverlayNum+1) to knMaxOverlay do
              if gMRIcroOverlay[lR].ScrnBufferItems > 0 then
                 inc(result);
end;

function ROIoverlayNum(lROI: integer): integer;
var
   lR,lN: integer;
begin
     result := 0;
     lN := 0;
     for lR := (kBGOverlayNum+1) to knMaxOverlay do begin
              //fx(lR,gMRIcroOverlay[lR].ScrnBufferItems);
              if gMRIcroOverlay[lR].ScrnBufferItems > 0 then begin
                 inc(lN);
                 if lROI = lN then begin
                     result := lR;
                     exit;
                 end;
              end; //if ROI has items
     end;
end;


function ROIoverlayNameShort(lROI: integer): string;
begin
     if ROIoverlayNum(lROI) = 0 then
{$IFDEF FPC}
        result := inttostr(ImgForm.XViewEdit.value)+'x'+inttostr(ImgForm.YViewEdit.value)+'x'+inttostr(ImgForm.ZViewEdit.value)
{$ELSE}
        result := inttostr(ImgForm.XViewEdit.asinteger)+'x'+inttostr(ImgForm.YViewEdit.asinteger)+'x'+inttostr(ImgForm.ZViewEdit.asinteger)
{$ENDIF}
     else
         result := parsefilename(extractfilename(gMRIcroOverlay[ROIoverlayNum(lROI)].HdrFileName));
end;

function StDev (lSum, lSumSqr: single; lN: integer): single;
begin
	result := 0;
	if lN < 2 then
		exit; //avoid divide by zero. We divide by N-1
	result:= (lSumSqr - ((Sqr(lSum))/lN));
	if  (result > 0) then
				result :=  Sqrt ( result/(lN-1))
end;


function ROIoverlayNameLong(lROI: integer): string;
begin
     if ROIoverlayNum(lROI) = 0 then
{$IFDEF FPC}
        result := inttostr(ImgForm.XViewEdit.value)+'x'+inttostr(ImgForm.YViewEdit.value)+'x'+inttostr(ImgForm.ZViewEdit.value)
{$ELSE}
        result := inttostr(ImgForm.XViewEdit.asinteger)+'x'+inttostr(ImgForm.YViewEdit.asinteger)+'x'+inttostr(ImgForm.ZViewEdit.asinteger)
{$ENDIF}
     else
         result := gMRIcroOverlay[ROIoverlayNum(lROI)].HdrFileName;
end;


function NCond ( var l4DTrace: T4DTrace): integer;
var
   lCond: integer;
begin
  result := 0;
  for lCond := 1 to kMaxCond do
      if l4DTrace.Conditions[lCond].Events > 0 then
         inc(result);
end;

function StError (lSum, lSumSqr: single; lN: integer): single;
//= STANDARD DEVIATION / SQUARE ROOT OF THE POPULATION SIZE
//= STDEV(range of values)/SQRT(lN)
begin
	 if lN > 1 then
		result := StDev (lSum, lSumSqr, lN)/ sqrt(lN)
	else
		result := 0;
end;

const
	 kMaxEvents = 2048;

procedure TimecourseVoxinten (var l4DHdr: TMRIcroHdr; lVoxel: integer; lTimeCourse: DoubleP);
//could also use periutil's VoxInten, but this is faster...
var
   lVol,lVolOffset,lImgVox,lMaxStatVol: integer;
   l32Buf: singleP;
   l16Buf: smallintp;
begin //if ROI else no ROI - single voxel
      lImgVox  :=  l4DHdr.NIFTIhdr.dim[1]*l4DHdr.NIFTIhdr.dim[2]*l4DHdr.NIFTIhdr.dim[3];
     lMaxStatVol := l4DHdr.NIFTIhdr.dim[4];
      if (l4DHdr.ImgBufferBPP  = 4) then begin
         l32Buf := SingleP(l4DHdr.ImgBuffer );
         for lVol := 1 to lMaxStatVol do begin
             lVolOffset := (lVol-1)*lImgVox;
             lTimeCourse^[lVol] :=  l32Buf^[lVoxel+lVolOffset]
         end;
      end else if l4DHdr.ImgBufferBPP  = 2 then begin
          l16Buf := SmallIntP(l4DHdr.ImgBuffer );
          for lVol := 1 to lMaxStatVol do begin
             lVolOffset := (lVol-1)*lImgVox;
             lTimeCourse^[lVol] :=  l16Buf^[lVoxel+lVolOffset]
          end;
      end else if l4DHdr.ImgBufferBPP  = 1 then begin
          for lVol := 1 to lMaxStatVol do begin
             lVolOffset := (lVol-1)*lImgVox;
             lTimeCourse^[lVol] :=  l4DHdr.ImgBuffer^[lVoxel+lVolOffset];
          end;
      end; //if 1 bpp
end; //GenerateVoxinten

function ROImean (var l4DHdr: TMRIcroHdr; lROInum,lVol: integer): double;
var
   l32Buf: singleP;
   l16Buf: smallintp;
   lSum: double;
   lMaskVox: int64;
   lInc,lVolOffset,lImgVox: integer;
begin
     result := 0;
     //compute number of voxels in mask
     lImgVox  :=  l4DHdr.NIFTIhdr.dim[1]*l4DHdr.NIFTIhdr.dim[2]*l4DHdr.NIFTIhdr.dim[3];

     lMaskVox := 0;
     for lInc := 1 to lImgVox do
         if gMRIcroOverlay[lROInum].ScrnBuffer^[lInc] > 0 then //in mask
            lMaskVox := lMaskVox + gMRIcroOverlay[lROInum].ScrnBuffer^[lInc];
     if lMaskVox < 1 then
        exit;
     lSum := 0;
     lVolOffset := (lVol-1)*lImgVox;
     if (l4DHdr.ImgBufferBPP  = 4) then begin
            l32Buf := SingleP(l4DHdr.ImgBuffer );
            for lInc := 1 to lImgVox do begin
                if gMRIcroOverlay[lROInum].ScrnBuffer^[lInc] > 0 then begin//in mask
                      lSum := lSum + (gMRIcroOverlay[lROInum].ScrnBuffer^[lInc]*l32Buf^[lInc+lVolOffset]);
                end; //in mask
            end; //for each vox
     end else if (l4DHdr.ImgBufferBPP  = 2) then begin
            l16Buf := SmallIntP(l4DHdr.ImgBuffer );
            for lInc := 1 to lImgVox do begin
                if gMRIcroOverlay[lROInum].ScrnBuffer^[lInc] > 0 then begin//in mask
                       lSum := lSum + (gMRIcroOverlay[lROInum].ScrnBuffer^[lInc]*l16Buf^[lInc+lVolOffset]);
                end; //in mask
            end; //for each vox
     end else if (l4DHdr.ImgBufferBPP  = 1) then begin
            for lInc := 1 to lImgVox do begin
                if gMRIcroOverlay[lROInum].ScrnBuffer^[lInc] > 0 then begin//in mask
                       lSum := lSum + (gMRIcroOverlay[lROInum].ScrnBuffer^[lInc]*l4DHdr.ImgBuffer^[lInc+lVolOffset]);
                end; //for each volume
            end; //for each vox
     end; //for image type
     result := lSum/lMaskVox;
end;

function TimecourseROIinten (var l4DHdr: TMRIcroHdr; lROInum: integer; lTimeCourse: DoubleP): boolean;
var
   lVol,lMaxStatVol: integer;
begin
     lMaxStatVol := l4DHdr.NIFTIhdr.dim[4];
     //result := false;
     for lVol := 1 to lMaxStatVol do
         lTimeCourse^[lVol] := ROImean (l4DHdr,lROInum,lVol);
     //compute mean for each volume
     result := true;
end;

function ComputeMeanSE (lCountBin: longintp; lMnBin,lSEBin,lSumBin,lSumSqrBin: doublep;
        lNegBins,lPosBins: integer): boolean;
var
   lBin: integer;
begin
     result := false;

        (*var
   lBins,lBin,lnBinsWithSamples: integer;
   lIntensitySum: double;
begin
     result := false;
     lIntensitySum := 0;
     lnBinsWithSamples := 0;
     lBins := lNegBins;
     if lBins < 1 then
        lBins := lNegBins+lPosBins;
     for lBin := lBins downto 1 do begin //new only base pct on baseline
         if lCountBin^[lBin] > 0 then begin
            lIntensitySum := lIntensitySum+lMnBin^[lBin];
            inc(lnBinsWithSamples);
         end; //samples in bin
     end; //for each bin
     if lnBinsWithSamples < 1 then
        exit;*)
     if (lNegBins + lPosBins) < 1 then
        exit;
     for lBin := (lNegBins + lPosBins) downto 1 do
             lSEBin^[lBin] := StError(lSumBin^[lBin],lSumSqrBin^[lBin],lCountBin^[lBin]);
     result := true;
end; //ifunc ComputeMeanSE

{$IFDEF REMOVEREGRESS}


function RemoveRegressors(lTimeCourseRaw,lTimeCourseFilt: DoubleP; var l4DTrace: T4DTrace;lCond,lnVol: integer;var lPSPlot: TPSPlot): boolean;
var
   lOK: boolean;
   lKernelBins,lncond,lC,lVol,lnCondincludeTD: integer;
   lHRFra, lTDra: doublep;
   lInputSum,lOutputSum : double;
   X: PMatrix;
   Y: PVector;
   //lDummy,lEstTimeCoursePrecise: DoubleP;
   lOutT,lOutSlope: DoubleP0;
begin
     result := false;
     lncond := NCond (l4DTrace);
     lnCondincludeTD := lnCond;
     if lPSPlot.TemporalDeriv then
        lnCondincludeTD := lnCondincludeTD * 2;
     if (lnCondincludeTD < 2) or (lPSPlot.SPMDefaultsStatsFmriT < 1) then begin
        Showmessage('You need at least two variables to remove regressors (you could add the temporal derivative)');
	    exit;
     end; //cond = 0
     if not CreateHRF (lPSPlot.TRsec, lKernelBins,lPSPlot.SPMDefaultsStatsFmriT, lHRFra, lTDra) then exit;
     //getmem(lTimeCourseRegress,lnVol*sizeof(double));
     for lVol := 1 to lnVol do
         lTimeCourseFilt^[lVol] := lTimeCourseRaw^[lVol];
     //compute sum intensity so we can adjust for shifts in the mean...
     lInputSum := 0;
     for lVol := 1 to lnVol do
         lInputSum := lInputSum+lTimeCourseRaw^[lVol];
     //convolve each condition...
     DimMatrix(X, lnCondincludeTD, lnVol);
     //lDummy := nil;
     //Getmem(lEstTimeCoursePrecise, lnVol *lPSPlot.SPMDefaultsStatsFmriT * sizeof(double));
     for lC := 1 to lnCond do begin
         (*if lC = lCond then
             ConvolveTimeCourse(X, lHRFra, lEstTimeCoursePrecise,l4DTrace, lC,lC,lnVol,lKernelBins,lPSPlot.SPMDefaultsStatsFmriT,lPSPlot.SPMDefaultsStatsFmriT0,lPSPlot.TRSec, lPSPlot.SliceTime)
         else*)
             ConvolveTimeCourse(X, lHRFra, l4DTrace, lC,lC,lnVol,lKernelBins,lPSPlot.SPMDefaultsStatsFmriT,lPSPlot.SPMDefaultsStatsFmriT0,lPSPlot.TRSec, lPSPlot.SliceTime);
     end;
     //convolve temporal derivatives for each condition
     if lPSPlot.TemporalDeriv then
        for lC := 1 to lnCond do
            ConvolveTimeCourse(X, lTDra, l4DTrace, lC,lC+lnCond,lnVol,lKernelBins,lPSPlot.SPMDefaultsStatsFmriT,lPSPlot.SPMDefaultsStatsFmriT0,lPSPlot.TRSec, lPSPlot.SliceTime);
     freemem(lHRFra);
     freemem(lTDra);
     DimVector(Y, lnVol);
     for lVol := 1 to lnVol do
         Y^[lVol] := lTimeCourseRaw^[lVol];
     getmem(lOutT, (lnCondincludeTD+1)* sizeof(double));
     getmem(lOutSlope, (lnCondincludeTD+1)* sizeof(double));
     lOK := MultipleRegressionVec (lnVol,lnCondincludeTD, X, Y, lOutT,lOutSlope);
     freemem(lOutT);
     DelVector(Y, lnVol);
     //begin test - show responses...
if lPSPlot.PlotModel then begin
     lC := lCond; //response for condition
     //if lTemporalDeriv then lC := lCond + lnCond; //lCond + lnCond = TD
      //if lPSPlot.TemporalDeriv then  fx( lC,lOutSlope^[lC-1],lOutSlope^[lnCond+lC-1] );
     for lVol := 1 to lnVol do
         lTimeCourseFilt^[lVol] := (X^[lC]^[lVol] *lOutSlope[lC-1]);
end else begin //not test
     if lOK then begin
        for lC := 1 to lnCondincludeTD do begin
            if lC <> lCond then begin
               for lVol := 1 to lnVol do
                   lTimeCourseFilt^[lVol] := lTimeCourseFilt^[lVol]- (X^[lC]^[lVol] *lOutSlope[lC-1]);

            end; //for each regressor
        end; //for lC
        result := true;//SUCCESS!
        //next - search for optimal fit of model to data..
        //if (lPSPlot.TextOutput) and (lCond > 0) and (lCond <= kMaxCond) then
        //  gOffsetError[lCond] := (OptimalOffset(lOutSlope^[lCond-1],lOutSlope^[lnCondincludeTD], lPSPlot.SPMDefaultsStatsFmriT0,lPSPlot.SPMDefaultsStatsFmriT,lnVol, lTimeCourseFilt,lEstTimeCoursePrecise)/ lPSPlot.SPMDefaultsStatsFmriT ) * lPSPlot.TRsec;
     end;//lOK
end;
     //Freemem(lEstTimeCoursePrecise);
     DelMatrix(X, lnCondincludeTD, lnVol);

     //adjust for shifts in the mean...
     lOutputSum := 0;
     for lVol := 1 to lnVol do
         lOutputSum := lOutputSum+lTimeCourseFilt^[lVol];
     if lOutputSum <> lInputsum then begin
        lOutputSum := (lOutputSum - lInputSum)/lnVol;
        for lVol := 1 to lnVol do
            lTimeCourseFilt^[lVol] := lTimeCourseFilt^[lVol] - lOutputSum;
     end; //correct for changes...
     freemem(lOutSlope);
end;

{$ENDIF} //IFDEF REMOVEREGRESS
//old TimeCourseToPSPlot - each event can contribute to several samples e.g. both before and after stimulus
(*function TimeCourseToPSPlot(lTimeCourse: DoubleP; var l4DTrace: T4DTrace;
         lCountBin: longintp; lMnBin,lSumBin,lSumSqrBin: doublep;
         var lTRsec,lBinWidthSec: single; lCond,lnNegBins,lnPosBins,lMaxStatVol: integer; lSliceTime: boolean): boolean;
var
   lOnsetRAx: doublep;
   lEvent,lnEvent,lBin,lVol: integer;
   lNegMS,lPosMS,lVolTime,lTRms,lHalfTRms,lPeristimulusTime,lmsPerBin: double;
begin
     result := false;
     if l4DTrace.Conditions[lCond].Events < 1 then exit;
     lmsPerBin := lBinWidthSec * 1000;
     lTRms := lTRsec * 1000;
     if lTRms = 0 then begin
        Showmessage('Unable to compute plots: You need to specify the TR in seconds.');
	    exit;
     end;
     lHalfTRms := lTRms/2;
     lNegMS := -lnNegBins * lmsPerBin;
     lPosMS := lnPosBins * lmsPerBin;
     lnEvent := l4DTrace.Conditions[lCond].Events;
     getmem(lOnsetRAx,lnEvent*sizeof(double) );
     if  lSliceTime then begin
         for lEvent := 1 to lnEvent do begin
             lOnsetRAx^[lEvent] := (l4DTrace.Conditions[lCond].EventRA^[lEvent]*1000)-lHalfTRms;
         end;
     end else
         for lEvent := 1 to lnEvent do
             lOnsetRAx^[lEvent] := (l4DTrace.Conditions[lCond].EventRA^[lEvent]*1000);
     //initialize bins
     for lBin := 1 to (lnNegBins + lnPosBins)  do  begin
         lMnBin^[lBin] := 0;
	lSumBin^[lBin] := 0;
         lSumSqrBin^[lBin] := 0;
         lCountBin^[lBin] := 0; //no samples in each cell
     end;
     for lVol := 1 to lMaxStatVol do begin
         lVolTime := (lVol-1) * lTRms;
         for lEvent := 1 to l4DTrace.Conditions[lCond].Events do begin
             lPeristimulusTime := lVolTime-lOnsetRAx^[lEvent];
             if (lPeristimulusTime >= lNegMS) and (lPeristimulusTime < lPosMS) then begin
			              lBin := trunc((lPeristimulusTime - lNegMS) / lmsPerBin)+1;
			              inc(lCountBin^[lBin]);
                          lSumBin^[lBin] := lSumBin^[lBin] + lTimeCourse^[lVol];
                          lSumSqrBin^[lBin] := lSumSqrBin^[lBin] + sqr(lTimeCourse^[lVol]);
             end; //if lPeristimulusTime within mix/max temporal window
         end; //for each event
     end; //for each vol
     //next compute mean
     for lBin := 1 to (lnNegBins + lnPosBins)  do
         if lCountBin^[lBin] > 0 then
            lMnBin^[lBin] := lSumBin^[lBin]/lCountBin^[lBin];
     freemem(lOnsetRAx);
     result := true;
end;//func TimeCourseToPS
*)
function TimeCourseToPSPlot(lTimeCourse: DoubleP; var l4DTrace: T4DTrace;
         lCountBin: longintp; lMnBin,lSumBin,lSumSqrBin: doublep;
         var lPSPlot: TPSPlot; lCond,lMaxStatVol: integer): boolean;
var
   lOnsetRAx: doublep;
   lEvent,lnEvent,lBin,lVol: integer;
   lNextEvent,lPrevEvent,lNegMS,lPosMS,lVolTime,lTRms,lHalfTRms,lPeristimulusTime,lmsPerBin: double;
begin
     result := false;
     if (l4DTrace.Conditions[lCond].Events < 1) or ((lPSPlot.nNegBins + lPSPlot.nPosBins)<1) then exit;
     lmsPerBin := lPSPlot.BinWidthSec * 1000;
     lTRms := lPSPlot.TRsec * 1000;
     if lTRms = 0 then begin
        Showmessage('Unable to compute plots: You need to specify the TR in seconds.');
	    exit;
     end;
     lHalfTRms := lTRms/2;
     lNegMS := -lPSPlot.nNegBins * lmsPerBin;
     lPosMS := lPSPlot.nPosBins * lmsPerBin;
     lnEvent := l4DTrace.Conditions[lCond].Events;
     getmem(lOnsetRAx,lnEvent*sizeof(double) );
     if  lPSPlot.SliceTime then begin
         for lEvent := 1 to lnEvent do begin
             lOnsetRAx^[lEvent] := (l4DTrace.Conditions[lCond].EventRA^[lEvent]*1000)-lHalfTRms;
         end;
     end else
         for lEvent := 1 to lnEvent do
             lOnsetRAx^[lEvent] := (l4DTrace.Conditions[lCond].EventRA^[lEvent]*1000);
     //initialize bins
     for lBin := 1 to (lPSPlot.nNegBins + lPSPlot.nPosBins)  do  begin
         lMnBin^[lBin] := 0;
	     lSumBin^[lBin] := 0;
         lSumSqrBin^[lBin] := 0;
         lCountBin^[lBin] := 0; //no samples in each cell
     end;
     //find volume's peristimulus time
     //note: we assume periutil's ReadCond ensures that Cond.Events are sorted in ascending order
     lEvent := 1;
     lPrevEvent := -MaxInt;
     lNextEvent := lOnsetRAx^[lEvent];
     for lVol := 1 to lMaxStatVol do begin
         lVolTime := (lVol-1) * lTRms;
         while lVolTime > lNextEvent do begin
             inc(lEvent);
             lPrevEvent := lNextEvent;
             if lEvent > lnEvent then
                lNextEvent := MaxInt
             else
                 lNextEvent := lOnsetRAx^[lEvent];
         end;
         lPeristimulusTime := lVolTime-lPrevEvent;
         if (lPeristimulusTime >= 0) and (lPeristimulusTime < lPosMS) then begin
			              lBin := trunc((lPeristimulusTime - lNegMS) / lmsPerBin)+1;
			              inc(lCountBin^[lBin]);
                          lSumBin^[lBin] := lSumBin^[lBin] + lTimeCourse^[lVol];
                          lSumSqrBin^[lBin] := lSumSqrBin^[lBin] + sqr(lTimeCourse^[lVol]);
         end else begin //if not after - check if before
             lPeristimulusTime := lVolTime-lNextEvent;
             if (lPeristimulusTime >= lNegMS) and (lPeristimulusTime < 0) then begin
			              lBin := trunc((lPeristimulusTime - lNegMS) / lmsPerBin)+1;
			              inc(lCountBin^[lBin]);
                          lSumBin^[lBin] := lSumBin^[lBin] + lTimeCourse^[lVol];
                          lSumSqrBin^[lBin] := lSumSqrBin^[lBin] + sqr(lTimeCourse^[lVol]);
             end; //if lPeristimulusTime within mix/max temporal window
         end; //if else... not after stimuli

         (*for lEvent := 1 to l4DTrace.Conditions[lCond].Events do begin
             lPeristimulusTime := lVolTime-lOnsetRAx^[lEvent];
             if (lPeristimulusTime >= lNegMS) and (lPeristimulusTime < lPosMS) then begin
			              lBin := trunc((lPeristimulusTime - lNegMS) / lmsPerBin)+1;
			              inc(lCountBin^[lBin]);
                          lSumBin^[lBin] := lSumBin^[lBin] + lTimeCourse^[lVol];
                          lSumSqrBin^[lBin] := lSumSqrBin^[lBin] + sqr(lTimeCourse^[lVol]);
             end; //if lPeristimulusTime within mix/max temporal window
         end; //for each event*)
     end; //for each vol
     //next compute mean
     for lBin := 1 to (lPSPlot.nNegBins + lPSPlot.nPosBins)  do
         if lCountBin^[lBin] > 0 then
            lMnBin^[lBin] := lSumBin^[lBin]/lCountBin^[lBin];
     freemem(lOnsetRAx);
     result := true;
end;//func TimeCourseToPS

function TextOutput (lROI,lCond: integer; var lPSPlot : TPSPlot; var l4DTrace: T4DTrace; lCountBin: longintp; lMnROI,lSEROI: doublep): boolean;
var
   lOutMnStr,lOutSDStr,lCondStr, lOutStr,lModelStr: string;
   lNegMS,lmsPerBin: double;
   lnBins,lBin,lMinBinCount,lMaxBinCount: integer;
begin
     result := false;
     lnBins := lPSPlot.nNegBins + lPSPlot.nPosBins;
     if lnBins < 1 then
        exit;
     lmsPerBin := lPSPlot.BinWidthSec * 1000;
     lNegMS := -lPSPlot.nNegBins * lmsPerBin;
     lMinBinCount :=  lCountBin^[1];
     lMaxBinCount := lCountBin^[1];
     for lBin := 1 to lnBins do begin
         if lCountBin^[lBin] < lMinBinCount then
            lMinBinCount := lCountBin^[lBin];
         if lCountBin^[lBin] > lMaxBinCount then
            lMaxBinCount := lCountBin^[lBin];
     end;
     lModelStr := kTextSep+'Processing='+kTextSep;
     if lPSPlot.RemoveRegressorVariability then begin
        if lPSPlot.PlotModel then
           lModelStr := lModelStr+'MODEL[hrf'
        else
            lModelStr := lModelStr+'observed[hrf';
        if lPSPlot.TemporalDeriv then
           lModelStr := lModelStr+'+TD';
        lModelStr := lModelStr+']';
        //if (lCond > 0) and (lCond <= kMaxCond) then  lModelStr := lModelStr+ floattostr(gOffsetError[lCond]);

     end else
         lModelStr := lModelStr+'observed[raw]';
     lModelStr := lModelStr+kTextSep;
     lCondStr := 'Image=,'+gMRIcroOverlay[kBGOverlayNum].HdrFileName+', '+inttostr(lCond)+',Condition=,'+l4DTrace.Conditions[lCond].ELabel+lModelStr+'Events=, '+inttostr(l4DTrace.Conditions[lCond].Events)+', samples per bin= '+inttostr(lMinBinCount)+'..'+inttostr(lMaxBinCount);
     lOutStr := kTextSep;
     for lBin := 1 to 11 do
      lOutStr := lOutStr+kTextSep;
     lOutStr := lOutStr+'Bin Starts At->';
     for lBin := 1 to lnBins do
         lOutStr := lOutStr+kTextSep+ RealToStr((lNegMS+ ((lBin-1)*  lmsPerBin)),0 );
     TextForm.MemoT.lines.add(lOutStr);
     TextForm.MemoT.Lines.add('samples per bin '+inttostr(lMinBinCount)+'..'+inttostr(lMaxBinCount));
     //next report number of samples averaged
     lOutStr := lCondStr+kTextSep+kTextSep+kTextSep+'samples in bin=';
     for lBin := 1 to lnBins do
         lOutStr := lOutStr+kTextSep+ inttostr(lCountBin^[lBin]  );
     TextForm.MemoT.lines.add(lOutStr);
     //next report mean signal
     lOutMnStr := lCondStr+kTextSep+'roiMn'+kTextSep+'MaskROI['+ROIoverlayNameShort(lROI)+']=,'+ROIoverlayNameLong(lROI);
     lOutSDStr := lCondStr+kTextSep+'roiSE'+kTextSep+'MaskROI['+ROIoverlayNameShort(lROI)+']=,'+ROIoverlayNameLong(lROI);
     for lBin := 1 to (lnBins) do begin
         lOutMnStr := lOutMnStr+kTextSep+ floattostr(lMnROI^[lBin]);//floattostr(lSumROI[lROI,lBin]/lBinCountRA[lBin]);
         lOutSDStr := lOutSDStr+kTextSep+ floattostr(lSEROI^[lBin]);//StDev(lSumROI[lROI,lBin],lSumSqrROI[lROI,lBin],lBinCountRA[lBin])  );
     end; //for each bin
     TextForm.MemoT.lines.add(lOutMnStr);
     TextForm.MemoT.lines.add(lOutSDStr);
     result := true;
end; //proc TextOutput

function CalcMean (lTimeCourse: DoubleP;lnVol: integer): double;
var
   lSum: double;
   lVol: integer;
begin
     result := 0;
     if lnVol < 1 then
        exit;
     lSum := 0;
     for lVol := 1 to lnVol do
         lSum := lSum + lTimeCourse^[lVol]; //Sum
     result := lSum / lnVol;
end;

procedure  PctSignal (lTimeCourse: DoubleP;lnVol: integer);
var
   lMean,lScale: double;
   lVol: integer;
begin
     if lnVol < 1 then
        exit;
     lMean := CalcMean (lTimeCourse,lnVol);
     if lMean = 0 then
        exit; //can't compute % signal change...
     lScale := abs(1/lMean);
     for lVol := 1 to lnVol do
         lTimeCourse^[lVol] := (lTimeCourse^[lVol]-lMean)*lScale; //Sum

end;

function CreatePeristimulusPlot (var l4DHdr: TMRIcroHdr; var l4DTrace: T4DTrace; var lPSPlot: TPSplot): boolean;
var
   lBinData:  T4DTrace;
   lTimeCourse,lTimeCourseFilt:  doublep;
   lCountBin: longintp;
   lMnBin,lSEBin,lSumBin,lSumSqrBin: doublep;
   lCond,lncond,lnVol,lnROI,lROI,lnROImin1,lLine,lBin: integer;
   lTR: double;
begin
     result := false;
     lncond := NCond (l4DTrace);
     if lncond = 0 then begin
        Showmessage('You need to specify event onset times before creating a peristimulus plot.');
	    exit;
     end; //cond = 0
     lnVol := l4DHdr.NIFTIhdr.dim[4];
     if lnVol < 3 then begin
        Showmessage('Unable to compute plots: You need to analyze a 4D image.');
	    exit;
     end;
     if (l4DHdr.ImgBufferItems = 0) then exit;

     lTR := lPSPlot.TRsec * 1000;
     if lTR = 0 then begin
        Showmessage('Unable to compute plots: You need to specify the TR in seconds.');
	    exit;
     end;
     lnROI := 0;
     for lROI := (kBGOverlayNum+1) to knMaxOverlay do
         if   gMRIcroOverlay[lROI].ScrnBufferItems > 0 then //current implementation only one ROI
           inc(lnROI);
     if lnROI < 1 then begin
        lnROImin1 := 1;
     end else begin
         lnROImin1 := lnROI;
     end;
     //allocate memory
     getmem(lTimeCourse,lnVol*sizeof(double));
     getmem(lTimeCourseFilt,lnVol*sizeof(double));
     getmem(lCountBin,(lPSPlot.nNegBins+lPSPlot.nPosBins)*sizeof(integer));
     getmem(lMnBin,(lPSPlot.nNegBins+lPSPlot.nPosBins)*sizeof(double));
     getmem(lSEBin,(lPSPlot.nNegBins+lPSPlot.nPosBins)*sizeof(double));
     getmem(lSumSqrBin,(lPSPlot.nNegBins+lPSPlot.nPosBins)*sizeof(double));
     getmem(lSumBin,(lPSPlot.nNegBins+lPSPlot.nPosBins)*sizeof(double));
     if lPSPlot.GraphOutput then  begin
        Create4DTrace (lBinData);
        Init4DTrace(lPSPlot.nNegBins + lPSPlot.nPosBins,lnROImin1*lnCond,lBinData,true);
        for lROI := 1 to lnROImin1 do
            lBinData.Lines[lROI].ELabel := ROIoverlayNameShort(lROI);
     end; //if graphoutput
     //repeat for each Region of interest
     for lROI := 1 to lnROImin1 do begin
         //compute complete timecourse for all volumes...
         if lnROI = 0 then begin
                {$IFDEF FPC}
                TimecourseVoxinten (l4DHdr, ImgForm.XViewEdit.value
                                 + ((ImgForm.YViewEdit.value-1)*gBGImg.ScrnDim[1])
                                 +((ImgForm.ZViewEdit.value-1)*gBGImg.ScrnDim[1]
                                 *gBGImg.ScrnDim[2]),lTimeCourse)
                {$ELSE}
                TimecourseVoxinten (l4DHdr, ImgForm.XViewEdit.asinteger
                                 + ((ImgForm.YViewEdit.asinteger-1)*gBGImg.ScrnDim[1])
                                 +((ImgForm.ZViewEdit.asinteger-1)*gBGImg.ScrnDim[1]
                                 *gBGImg.ScrnDim[2]),lTimeCourse)
                {$ENDIF}
         end else
                 TimecourseROIinten (l4DHdr, ROIoverlayNum(lROI), lTimeCourse);
         //next normalize signal
         if lPSPlot.PctSignal then
            PctSignal(lTimeCourse,lnVol);
         //next compute PSPlots
         for lCond := 1 to lnCond do begin
             //here is where we can remove variability predicted by regressors....
             {$IFDEF REMOVEREGRESS}
             if  lPSPlot.RemoveRegressorVariability then begin
                RemoveRegressors(lTimeCourse,lTimeCourseFilt,l4DTrace,lCond,lnVol,lPSPlot);
                TimeCourseToPSPlot(lTimeCourseFilt, l4DTrace,lCountBin, lMnBin,lSumBin,lSumSqrBin
               ,lPSPlot, lCond,lnVol);
             end else
             {$ENDIF}
                 TimeCourseToPSPlot(lTimeCourse, l4DTrace,lCountBin, lMnBin,lSumBin,lSumSqrBin
               ,lPSPlot,lCond,lnVol);
             //percent signal change and std error
             ComputeMeanSE (lCountBin, lMnBin,lSEBin,lSumBin,lSumSqrBin
                ,lPSPlot.nNegBins,lPSPlot.nPosBins);
             //report results
             if lPSPlot.TextOutput then
                TextOutput (lROI,lCond,lPSPlot, l4DTrace,lCountBin,lMnBin,lSEBin);
             if (lPSPlot.GraphOutput)  then begin
                lLine := lROI + ((lCond-1)* lnROImin1);
                for lBin := 1 to (lPSPlot.nNegBins + lPSPlot.nPosBins) do begin
                    lBinData.Lines[lLine].EventRA^[lBin] := lMnBin^[lBin];
                    lBinData.Conditions[lLine].EventRA^[lBin] := lSEBin^[lBin];
                end;//for each bin
             end; //if graphoutput
         end; //for each cond
     end; //for each ROI
     freemem(lCountBin); //12/2007
     freemem(lTimeCourse);
     freemem(lTimeCourseFilt);
     freemem(lMnBin);
     freemem(lSEBin);
     freemem(lSumSqrBin);
     freemem(lSumBin);
     if lPSPlot.TextOutput then
        TextForm.show;
     if (lPSPlot.GraphOutput)  then begin
        MinMax4DTrace(lBinData);
        for lCond := 1 to lnCond do
            lBinData.Conditions[lCond].eLabel:= l4DTrace.Conditions[lCond].eLabel;
        lBinData.HorzMin := (-lPSPlot.nNegBins+0.5)*lPSPlot.BinWidthSec;
        lBinData.HorzWidPerBin := lPSPlot.BinWidthSec;
        CorePlot4DTrace(lBinData,Graph4DForm.Image1,1,0,lnCond,lPSPlot.TRsec,Graph4DForm.MinEdit.value,Graph4DForm.MaxEdit.value,true);
        Close4DTrace(lBinData,true);
     end;//if graph
     result := true;
end;


end.
