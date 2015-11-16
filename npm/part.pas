unit part;
//Physiological Artifact Removal Tool
{$H+}
interface
uses
    define_types,dialogsx,SysUtils;

function ApplyPart( lFilename: string;lImgData: singleP; lBins,lVolVox,lSlices, lImgVol : integer; lTRsec: single): string;

implementation
type
    TPhysioT =  RECORD
      Triggers,InterpolatedTriggers: integer;
      TriggerMedian,TriggerQ1,TriggerQ3: Double;
      TriggerRA: singleP;
    END;
function SaveTriggersAs3ColumnFSL(lPhysioIn: TPhysioT; lOutName: string): boolean;
var
   lF: textfile;
   lPos: integer;
begin
   result := false;
   if (lPhysioIn.Triggers < 1) then
        exit;
   assignfile(lF,lOutName+'.txt');
   Filemode := 0;
   rewrite(lF);
   for lPos := 1 to lPhysioIn.Triggers do
       Writeln(lf,realtostr(lPhysioIn.TriggerRA^[lPos],3)+' 1 1');
   closefile(lF);
   Filemode := 2;
   result := true;
end;

procedure qsort(lower, upper : integer; var Data:SingleP);
//40ms - fast but very recursive...
var
       left, right : integer;
       pivot,lswap: single;
begin
     pivot:=Data^[(lower+upper) div 2];
     left:=lower;
     right:=upper;
     while left<=right do begin
             while Data^[left]  < pivot do left:=left+1;  { Parting for left }
             while Data^[right] > pivot do right:=right-1;{ Parting for right}
             if left<=right then begin   { Validate the change }
                 lswap := Data^[left];
                 Data^[left] := Data^[right];
                 Data^[right] := lswap;
                 left:=left+1;
                 right:=right-1;
             end; //validate
     end;//while left <=right
     if right>lower then qsort(lower,right,Data); { Sort the LEFT  part }
     if upper>left  then qsort(left ,upper,data); { Sort the RIGHT part }
end;

procedure QuartileTriggerSpacing(var lPhysio: TPhysioT);
var
   lTriggerDelayRA: singleP;
   lPos: integer;
begin
          lPhysio.TriggerQ1 := 0;
          lPhysio.TriggerMedian := 0;
          lPhysio.TriggerQ3 := 0;
          if lPhysio.Triggers < 4 then
             exit;
          getmem(lTriggerDelayRA,(lPhysio.Triggers-1)*sizeof(single));
          for lPos := 1 to (lPhysio.Triggers-1) do
              lTriggerDelayRA^[lPos] := abs(lPhysio.TriggerRA^[lPos]-lPhysio.TriggerRA^[lPos+1]);
          qsort(1,lPhysio.Triggers-1,lTriggerDelayRA);//-1 : fence post vs wire 
          lPos := lPhysio.Triggers div 2;
          lPhysio.TriggerMedian := lTriggerDelayRA^[lPos];
          lPos := lPhysio.Triggers div 4;
          lPhysio.TriggerQ1 := lTriggerDelayRA^[lPos];
          lPos := round(0.75*lPhysio.Triggers );
          lPhysio.TriggerQ3 := lTriggerDelayRA^[lPos];
          freemem(lTriggerDelayRA);
end;

function PARTool (var lPhysio: TPhysioT; lImgData: singleP; lTRsec: single; lnVolVox,lnSlices, lImgVol, lBinIn : integer): string;
const
     kMinSamplesPerBin = 4;
var
   lV,lSliceTime,lMeanSignal,lOnsetTime,lBinWidth,lBinMin,lBinMax,lTimeSinceTrigger,lPrevTriggerTime: double;
   lSlice,lSlicePos,lnSliceVox,lnSlicePos,lVoxel,lBin,lSample,lnBin,lnBinDiv2,lNextTrigger,lSamplesWithVariance,lCorrectedSamples,lVolOffset: integer;
   lBinCountRA,lVolBinRA: longintp;
   lVariance : boolean;
   lBinEstimateRA: doublep;
begin
     result := '';
     if (lPhysio.Triggers < 4) or (lnVolVox < 4) or (lImgVol < 4) then begin
        ShowMsg('PART requires at least 4 triggers and at least 4 volumes each with at least 4 voxels');
        exit;
     end;
     if (lBinIn < 4) then begin
        ShowMsg('PART requires at least 4 data bins');
        exit;
     end;
     lnSliceVox := lnVolVox div lnSlices;
     if (lnVolVox mod lnSlices) <> 0 then begin
         ShowMsg('PART requires volvox to be evenly divisible by number of slices.');
         exit;
     end;
     lSamplesWithVariance := 0;
     lCorrectedSamples := 0;
     QuartileTriggerSpacing(lPhysio);
     //find number bin range - this is median-1.5IQR..median+1.5IQR
     lBinMin := -lPhysio.TriggerMedian/2-(abs(lPhysio.TriggerQ1-lPhysio.TriggerQ3)*0.75);
     lBinMax := +lPhysio.TriggerMedian/2+abs(lPhysio.TriggerQ1-lPhysio.TriggerQ3)*0.75;
     //next - create bins
     lnBin := lBinIn;
     //could adjust number of bins and return here wth a label
    lBinWidth := abs((lBinMax-lBinMin)/(lnBin-1));//lnBin-1: fenceposts vs wire
    lnBinDiv2 := (lnBin div 2)+1;
    getmem(lBinCountRA,lnBin*sizeof(integer));
    getmem(lBinEstimateRA,lnBin*sizeof(double));
    getmem(lVolBinRA,lImgVol*sizeof(integer));
    lVoxel := 0;
    for lSlice := 1 to lnSlices do begin
        //adjust slices so slice 1 occurs at 0, slice 2 at 1/nslices...
        lSliceTime := ((lSlice-1)/lnSlices)-1; //-1 as 1st volume starts at zero, not 1
        //do next step for each slice - different slices have different bin distributions due to different slicetime
        //next count number of samples in each bin
        for lBin := 1 to lnBin do
            lBinCountRA^[lBin] := 0;
        lPrevTriggerTime := -MaxInt;
        lNextTrigger := 1;
        for lSample := 1 to lImgVol do begin
                   //for each sample, find nearest trigger
                   lOnsetTime := lSample+lSliceTime;
                   if lOnsetTime > lPhysio.TriggerRA^[lNextTrigger] then begin
                      while (lNextTrigger <= lPhysio.Triggers ) and (lOnsetTime > lPhysio.TriggerRA^[lNextTrigger]) do begin
                            lPrevTriggerTime := lPhysio.TriggerRA^[lNextTrigger];
                            inc(lNextTrigger);
                      end; //while
                   end;//if onset >
                   lTimeSinceTrigger :=  lOnsetTime-lPrevTriggerTime;
                   if lTimeSinceTrigger > abs(lPhysio.TriggerRA^[lNextTrigger]-lOnsetTime) then
                      lTimeSinceTrigger :=  -abs(lPhysio.TriggerRA^[lNextTrigger]-lOnsetTime);//use abs in case we are past final trigger
                   //now compute bin...
                   //inc(lCorrectedSamples);
                   if (lTimeSinceTrigger > lBinMin) and (lTimeSinceTrigger < lBinMax) then begin
                      lBin := round( (lTimeSinceTrigger)/ lBinWidth)+lnBinDiv2;
                      lVolBinRA^[lSample] := lBin;
                      if (lBin < 1) or (lBin > lnBin) then
                         fx(-661,lBin,lTimeSinceTrigger)
                      else
                          inc(lBinCountRA^[lBin]);
                   end else
                       lVolBinRA^[lSample] := 0;
        end; //for each volume
        for lSlicePos := 1 to lnSliceVox do begin
            inc(lVoxel);
            //first - only correct voxels with variability - do not waste time outside brain
            lVolOffset := lVoxel;
            lVariance := false;
            lSample := 1;
            lV := lImgData^[lVolOffset];
            while (not lVariance) and (lSample <= lImgVol) do begin
                  if lV <> lImgData^[lVolOffset] then
                     lVariance := true;
                  inc(lSample);
                  lVolOffset := lVolOffset+lnVolVox;
            end; //while no variance
            if lVariance then begin //voxel intensity varies accross time - attempt to remove artifact
               lSamplesWithVariance := lSamplesWithVariance +lImgVol;
               //1st - sum effects
               for lBin := 1 to lnBin do
                   lBinEstimateRA^[lBin] := 0;
               lMeanSignal := 0;
               lVolOffset := lVoxel;
               for lSample := 1 to lImgVol do begin
                   lMeanSignal := lImgData^[lVolOffset] + lMeanSignal;
                   lBin := lVolBinRA^[lSample];
                   if (lBin > 0) and (lBinCountRA^[lBin] > kMinSamplesPerBin) then
                         lBinEstimateRA^[lBin] := lBinEstimateRA^[lBin]+ lImgData^[lVolOffset];
                   lVolOffset := lVolOffset+lnVolVox;
               end; //for each volume
               lMeanSignal := lMeanSignal /lImgVol;
               //next compute correction... average signal in bin - average voxel intensity irrelevant of bin
               for lBin := 1 to lnBin do
                   if lBinCountRA^[lBin] > kMinSamplesPerBin then
                      lBinEstimateRA^[lBin] := (lBinEstimateRA^[lBin]/lBinCountRA^[lBin])-lMeanSignal;
                      //lBinEstimateRA[lBin] := lBinEstimateRA[lBin]-lBinMeanCount;
               //next  apply correction - inner loop complete for each voxel!
               lVolOffset := lVoxel;
               for lSample := 1 to lImgVol do begin
                   //for each sample, find nearest trigger
                   lBin := lVolBinRA^[lSample];
                   if (lBin > 0) and (lBinCountRA^[lBin] > kMinSamplesPerBin) then begin
                         lImgData^[lVolOffset] :=  (lImgData^[lVolOffset]-lBinEstimateRA^[lBin]);
                         inc(lCorrectedSamples)
                   end;
                   lVolOffset := lVolOffset+lnVolVox;
               end; //for each volume
            end; //if variance
        end;//for each voxel in slice
    end; //for slice
    //**INNER LOOP end -
    //next - report results
    result :=' Time per vol (TR) [sec] '+realtostr(lTRsec,4)+kCR;
    result :=result +' fMRI Volumes '+inttostr(lImgVol)+kCR;
    result :=result +' Triggers n/First...Last [vol] '+realtostr(lPhysio.Triggers,0)+'/'+realtostr(lPhysio.TriggerRA^[1],2)+'...'+realtostr(lPhysio.TriggerRA^[lPhysio.Triggers],2)+kCR;
    if abs(lImgVol-lPhysio.TriggerRA^[lPhysio.Triggers]) > 10 then begin
       result :=result +'******* WARNING: Duration of fMRI session and duration of triggers is very different *******';
       result :=result +'******* Please ensure specified TR is correct, files are correct and onset of fMRI was synchronized with physio data *******';
    end;
    result := result + '    Q1/Median/Q2 [sec] '+realtostr(lTRsec*lPhysio.TriggerQ1,2)+'/'+realtostr(lTRsec*lPhysio.TriggerMedian,2)+'/'+realtostr(lTRsec*lPhysio.TriggerQ3,2)+kCR;
    result  :=  result + ' Bin n/Range [sec] '+inttostr(lnBin)+'/'+realtostr(lTRsec*lBinMin,2)+ '...'+realtostr(lTRsec*lBinMax,2)+kCR;
    result := result+   '  voxels without variance (outside brain) %: '+realtostr(100*( (lnVolVox-(lSamplesWithVariance/lImgVol))/lnVolVox),2)+kCR;
    if lSamplesWithVariance > 0 then
       result := result+   '  voxels with variance which were corrected %: '+realtostr(100*(lCorrectedSamples/lSamplesWithVariance),2)+kCR;
    for lBin := 1 to lnBin do
        result := result+('  Bin '+inttostr(lBin)+ ' '+realtostr(lBin*lBinWidth+lBinMin ,2) +' '+inttostr(lBinCountRA^[lBin])  )+kCR;
    freemem(lBinCountRA);
    freemem(lBinEstimateRA);
    freemem(lVolBinRA);
end;  


function StrVal (var lNumStr: string): integer;
begin
     try
        result := strtoint(lNumStr);
     except
           on EConvertError do begin
              ShowMsg('StrVal Error - Unable to convert the string '+lNumStr+' to a number');
              result := MaxInt;
           end;
     end;
end;

procedure AddSample(var lNumStr: string; var lnTotal,lnSample, lnTrigger: integer; var lPhysio: TPhysioT);
var
   lVal: integer;
begin
     lVal := StrVal(lNumStr);
     if lVal = 5003 then
        exit;
     lNumStr := '';
     inc(lnTotal);
     if lnTotal < 5 then exit;
          if lVal > 4096 then begin
             if lVal <> 5000 then begin
                ShowMsg('Potentially serious error: unknown trigger type : '+inttostr(lVal));
             end;
             inc(lnTrigger);
             if (lPhysio.Triggers <> 0) then
                lPhysio.TriggerRA^[lnTrigger] := lnSample;
          end else begin
             inc(lnSample);
          end;
end;

function AdjustStartPos (var lStr: string; var lStartPos: integer): boolean;
//Some Siemens physio files appear to have nonsense characters befor real data<bh:ef><bh:bb><bh:bf>1
var
   lLen: integer;
begin
   lLen := length(lStr);
   result := false;
   if (lLen-lStartPos)<2 then
      exit;
   result := true;
   repeat
         if  lStr[lStartPos] in [ '0'..'9'] then
             exit;
         inc(lStartPos);
   until (lStartPos = lLen);
    result := false;
end;

procedure CountValidItems(var lStr: string; var lStartPos,lnSample, lnTrigger: integer; var lPhysio: TPhysioT);
label
     123;
var
   lPos,lnTotal: integer;
   lNumStr: string;
begin
   lnTotal:= 0;
   lnSample := 0;
   lnTrigger := 0;
   lNumStr := '';
   if length(lStr)<2 then exit;
      if not AdjustStartPos ( lStr, lStartPos) then exit; //Oct 2009
   for lPos := lStartPos to length(lStr) do begin
       if (lStr[lPos] = ' ') and (lNumStr <> '') then begin
          if lNumStr = '5003' then begin
              lNumStr := '';
              goto 123; //end of recording
          end else
              AddSample(lNumStr, lnTotal,lnSample, lnTrigger, lPhysio);
       end else begin
           if  lStr[lPos] in [ '0'..'9'] then
               lNumStr := lNumStr + lStr[lPos]
           else if lStr[lPos] in [' '] then

           else begin
                //Showmessage(lStr[lPos]);
                goto 123;
           end;
       end;
   end; //for length
123:
   if (lNumStr <> '') then
      AddSample(lNumStr, lnTotal,lnSample, lnTrigger, lPhysio);
   lStartPos := lPos;
   while (lStartPos < length(lStr)) and ( lStr[lStartPos] <> ' ') do begin
                      inc(lStartPos);
   end;
end;

procedure CreatePhysio (var lPhysio: TPhysioT);
begin
     lPhysio.Triggers := 0;

end;

procedure ClosePhysio (var lPhysio: TPhysioT);
begin
     with lPhysio do begin
          if Triggers > 0 then
             freemem(TriggerRA);
          Triggers := 0;
     end;
end;

procedure InitPhysio(lnTrigger: integer; var lPhysio: TPhysioT);
begin
     ClosePhysio (lPhysio);
     with lPhysio do begin
          Triggers := lnTrigger;
          InterpolatedTriggers := 0;
          if Triggers > 0 then
             getmem(TriggerRA,Triggers*sizeof(single));
     end;
end;

function load3ColTxtPhysio (lFilename: string; var lPhysio: TPhysioT): boolean;
var
   F: TextFile;
   lnTrigger: integer;
   lFloat,lFloat2,lFloat3: single;
begin
    result := false;
     if not fileexists(lFilename) then exit;
     ClosePhysio(lPhysio);
     AssignFile(F, lFilename);
     FileMode := 0;  //Set file access to read only
     //pass 1 - count number of triggers
     lnTrigger := 0;
     Reset(F);
     while not EOF(F) do begin
	  {$I-}
          read(F,lFloat,lFloat2,lFloat3); //read triplets instead of readln: this should load UNIX files
          {$I+}
          if (ioresult = 0) and (lFloat > 0) then
             inc(lnTrigger);
     end;
     //pass 2 - load array
     InitPhysio(lnTrigger, lPhysio);
     lnTrigger := 0;
     Reset(F);
     while not EOF(F) do begin
	  {$I-}
          read(F,lFloat,lFloat2,lFloat3); //read triplets instead of readln: this should load UNIX files
          {$I+}
          if (ioresult = 0) and (lFloat > 0) then begin
             inc(lnTrigger);
             lPhysio.TriggerRA^[lnTrigger] := lFloat;
          end;
     end;
     FileMode := 2;  //Set file access to read/write
     CloseFile(F);
    result := true;
end;

procedure ReadlnX (var F: TextFile; var lResult: string);
var
   lCh: char;
begin
     lResult := '';
     while not Eof(F) do begin
           Read(F, lCh);
           if (lCh in [#10,#13]) then begin
              if lResult <> '' then begin
                 //Showmessage(lResult);
                 exit;
              end;
           end else
               lResult := lResult + lCh;
     end;
end; //ReadlnX


function loadSiemensPhysio (lFilename: string; var lPhysio: TPhysioT): boolean;
var
   F: TextFile;
   lStr: string;
   lPos,lnSample,lnTrigger: integer;
begin
    result := false;
     if not fileexists(lFilename) then exit;
     ClosePhysio(lPhysio);
     AssignFile(F, lFilename);
     FileMode := 0;  //Set file access to read only
     Reset(F);
     ReadlnX(F,lStr);//ColNames
     if length(lStr) < 1 then begin
        CloseFile(F);
        exit;
     end;
     //first pass - count items
     lPos := 1;
     CountValidItems(lStr,lPos,lnSample,lnTrigger,lPhysio);
     //second pass - load array
     if (lnSample < 1) and (lnTrigger < 1) then begin
        CloseFile(F);
        exit;
     end;
     //2nd pass...
     InitPhysio(lnTrigger, lPhysio);
     lPos := 1;
     CountValidItems(lStr,lPos,lnSample,lnTrigger,lPhysio);
     FileMode := 2;  //Set file access to read/write
     CloseFile(F);
    result := true;
end;

function InterpolateGaps (var lPhysioIn: TPhysioT): boolean;
//attempts to fill missing trigger pulses
//you must call QuartileTriggerSpacing before this function!
//   it assumes q1/median/q3 are filled
var
   lGap,l2Min,l2Max,l3Min,l3Max: double;
   lnReplace,lTrigger,lTrigger2: integer;
   lTempPhysio: TPhysioT;
begin
     result := false;
     if (lPhysioIn.Triggers < 4) then begin
        ShowMsg('InterpolateGaps requires at least 4 triggers.');
        exit;
     end;
     l2Min := 2*lPhysioIn.TriggerMedian-(abs(lPhysioIn.TriggerQ1-lPhysioIn.TriggerQ3)*1.5);
     l2Max := 2*lPhysioIn.TriggerMedian+(abs(lPhysioIn.TriggerQ1-lPhysioIn.TriggerQ3)*1.5);

     l3Min := 3*lPhysioIn.TriggerMedian-(abs(lPhysioIn.TriggerQ1-lPhysioIn.TriggerQ3)*1.5);
     l3Max := 3*lPhysioIn.TriggerMedian+(abs(lPhysioIn.TriggerQ1-lPhysioIn.TriggerQ3)*1.5);
     if l2Max > l3Min then
        exit; //variability too high to determine gaps
     lnReplace := 0;
     for lTrigger := 2 to lPhysioIn.Triggers do begin
        lGap := lPhysioIn.TriggerRA^[lTrigger] - lPhysioIn.TriggerRA^[lTrigger-1];
        if (lGap > l2Min) and (lGap < l2Max) then
           inc(lnReplace);
        if (lGap > l3Min) and (lGap < l3Max) then
           inc(lnReplace,2);
     end;
     if lnReplace = 0 then begin
         result := true;
         exit;
     end;
     //create temp backup
     CreatePhysio(lTempPhysio);
     InitPhysio(lPhysioIn.Triggers, lTempPhysio);
     for lTrigger := 1 to lPhysioIn.Triggers do
         lTempPhysio.TriggerRA[lTrigger] := lPhysioIn.TriggerRA[lTrigger];
     //create resized array
     InitPhysio(lTempPhysio.Triggers+lnReplace, lPhysioIn);
     //fill gaps
     lPhysioIn.TriggerRA[1] := lTempPhysio.TriggerRA[1];
     lTrigger2 := 1;
     for lTrigger := 2 to lTempPhysio.Triggers do begin
        inc(lTrigger2);
        lGap := lTempPhysio.TriggerRA^[lTrigger] - lTempPhysio.TriggerRA^[lTrigger-1];
        if ((lGap > l2Min) and (lGap < l2Max)) then begin //1 beat
           lPhysioIn.TriggerRA^[lTrigger2] := lTempPhysio.TriggerRA^[lTrigger-1]+(lgap / 2);
           inc(lTrigger2);
        end;
        if ((lGap > l3Min) and (lGap < l3Max)) then begin //2 beats
           lPhysioIn.TriggerRA^[lTrigger2] := lTempPhysio.TriggerRA^[lTrigger-1]+(lgap / 3);
           inc(lTrigger2);
           lPhysioIn.TriggerRA^[lTrigger2] := lTempPhysio.TriggerRA^[lTrigger-1]+(2*lgap / 3);
           inc(lTrigger2);
        end;
        lPhysioIn.TriggerRA^[lTrigger2] := lTempPhysio.TriggerRA^[lTrigger];
     end;
     ClosePhysio (lTempPhysio);
     lPhysioIn.InterpolatedTriggers := lnReplace;
     result := true;
end;

function ScalePhysioToTime(lPhysio: TPhysioT; lSamplesPerUnit: single): boolean;
var
   lScale: single;
   lTrigger: integer;
begin
     result := false;
     if (lPhysio.Triggers < 4) then begin
        ShowMsg('ScalePhysioToTR requires at least 4 triggers.');
        exit;
     end;
     if (lSamplesPerUnit <= 0) then begin
        ShowMsg('ScalePhysioToTime requires TR(sec) and samples/sec >0.');
        exit;
     end;
     lScale := 1/(lSamplesPerUnit); //use reciprocal: mults faster than divides
     for lTrigger := 1 to lPhysio.Triggers do
        lPhysio.TriggerRA^[lTrigger] := lPhysio.TriggerRA^[lTrigger] * lScale;
    result := true;
end;

procedure EnsureAscending(lPhysio: TPhysioT);
//check if order is correct - if not the sort...
//an alternative is to always sort, but this method is faster and less resource intensive for sorted data
var
   lPos: integer;
begin
   if lPhysio.Triggers < 2 then exit;
   for lPos := 2 to lPhysio.Triggers do begin
       if lPhysio.TriggerRA^[lPos] < lPhysio.TriggerRA^[lPos-1] then begin
          ShowMsg('Warning: input times are not in ascending order - data will be sorted.');
          qsort(1,lPhysio.Triggers,lPhysio.TriggerRA); //ensure trigger timings are in order...
          exit;
       end;
   end;
end;
                   
function ApplyPart( lFilename: string;lImgData: singleP; lBins,lVolVox,lSlices, lImgVol : integer; lTRsec: single): string;
var
   lPhysio: TPhysioT;
begin
     result := '';
     if not fileexists (lFilename) then exit;
     CreatePhysio(lPhysio);
     if UpCaseExt(lFilename) = '.TXT' then begin
         if not load3ColTxtPhysio(lFilename,lPhysio) then exit;
     end else
         if not loadSiemensPhysio(lFilename,lPhysio) then exit;
     EnsureAscending(lPhysio);
     QuartileTriggerSpacing(lPhysio);
     if not InterpolateGaps (lPhysio) then
         exit;
     if UpCaseExt(lFilename) <> '.TXT' then begin//export Siemens file as 3-column text
        ScalePhysioToTime(lPhysio,50); //50: siemens files use 50 Hz sampling -> convert to sec
        SaveTriggersAs3ColumnFSL(lPhysio,lFilename); //do this before TR conversion...
     end;
     ScalePhysioToTime(lPhysio,lTRsec); //Convert sec to volumes
     result := PARTool (lPhysio,lImgData,lTRsec,lVolVox,lSlices, lImgVol, lBins);
     ClosePhysio(lPhysio);
end;

end.