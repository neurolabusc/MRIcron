unit LesionStatThds;
{$I options.inc} // {$IFDEF OLDSTATS}
{$Include ..\common\isgui.inc}

interface

uses
 //  ComCtrls,Classes, Graphics, ExtCtrls,
 {$IFDEF GUI}  ComCtrls,{$ENDIF}
 SysUtils, Classes, dialogsx, unpm,
  define_types,stats,StatThdsUtil,Brunner,lesion_pattern;



type

  TLesionStatThread = class(TThread)
  private
    lBarX: TProgressBar;
    lttestx,lBMx: boolean;
    lnCritx,lBarPosX,lnPermuteX,lThreadx,lThreadStartx,lThreadEndx,lStartVoxx,lVoxPerPlankx,
    lImagesCountx,lControlsx : integer;
    lPlankImgx:ByteP;
    lOutImgMnx,lOutImgBMx,lOutImgTx,lOutImgAUCX,lSymptomRAx: SingleP;
    //lBarX: TProgressBar;
        procedure DoVisualSwap;
  protected
    procedure Execute; override;
    procedure VisualProg(lPos: Integer);
    procedure Analyze(lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lControlsIn : integer; lPlankImg:bytep;lOutImgMn,lOutImgBM,lOutImgT,lOutImgAUC,lSymptomRA: SingleP); virtual; abstract;
  public
    property Terminated;
    constructor Create(lBar: TProgressBar;lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lControlsIn : integer; lPlankImg:ByteP;lOutImgMn,lOutImgBM,lOutImgT,lOutImgAUC,lSymptomRA: SingleP);
  end;

  { Lesion - image reveals value }

  TLesionContinuous = class(TLesionStatThread )
  protected
    procedure Analyze(lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lControlsIn : integer; lPlankImg: byteP;lOutImgMn,lOutImgBM,lOutImgT,lOutImgAUC,lSymptomRA: SingleP);  override;
  end;

  TLesionBinom = class(TLesionStatThread )
  protected
    procedure Analyze(lChi2,lLieber: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lControlsIn : integer; lPlankImg: byteP;lOutImgMn,lOutImgL,lOutImgX,lOutImgAUC,lSymptomRA: SingleP);  override;
  end;
  {$IFNDEF OLDSTATS}
  procedure ShowRandomizationOrder (lnSubj, lnPermute: integer);
  {$ENDIF}
implementation

{$IFDEF NEWRNG}
uses rng;

procedure GenPermuteThreaded (lnSubj: integer; var lOrigOrder,lRanOrder: DoubleP0; var rng: TRNG);
var
   lInc,lRand: integer;
   lSwap: double;
begin
      Move(lOrigOrder^,lRanOrder^,lnSubj*sizeof(double));
     for lInc := lnSubj downto 2 do begin
         lRand := RandomInt0(rng, lInc-1);
         lSwap := lRanOrder^[lRand];
         lRanOrder^[lRand] := lRanOrder^[lInc-1];
         lRanOrder^[lInc-1] := lSwap;
     end;
end;
{$ELSE}



function GenRandThreaded(lRange: integer; var lRandSeed:comp): integer;
//normal random function does not work well when threaded - randseed is changed by each thread
Const Two32 = 4294967296.0 ;
const lFactor = $08088405 ; lTerm = 1 ;
type lT = array [0..1] of longint ;
var
   lX: extended;
begin
        lRandSeed := lRandSeed*lFactor + lTerm;
        lT(lRandSeed)[1] := 0 ; // < May'04 was: RS := RS - Trunc(RS/Two32)*Two32 ;
        lX := lRandSeed/Two32 ;
        result := trunc((lRange)*lX);
end;

//var
//   g1st: integer = 1;
procedure GenPermuteThreaded (lnSubj: integer; var lOrigOrder,lRanOrder: DoubleP0; var lRandSeed:comp);
var
   lInc,lRand: integer;
   lSwap: double;
begin
      Move(lOrigOrder^,lRanOrder^,lnSubj*sizeof(double));
     for lInc := lnSubj downto 2 do begin
         lRand := GenRandThreaded(lInc,lRandSeed);
         lSwap := lRanOrder^[lRand];
         lRanOrder^[lRand] := lRanOrder^[lInc-1];
         lRanOrder^[lInc-1] := lSwap;
     end;
     (*if g1st  < 3 then
        for lInc := 0 to (lnSubj-1) do
            NPMMsg(inttostr(g1st)+chr(9)+floattostr(lRanOrder^[lInc]));
     g1st := g1st + 1;*)

end;
{$ENDIF}


{$IFDEF NEWRNG}
procedure GenPermuteThreadedBinom (lnSubj: integer; var lOrigOrder,lRanOrder: ByteP0; var rng: TRNG);
{$ELSE}
procedure GenPermuteThreadedBinom (lnSubj: integer; var lOrigOrder,lRanOrder: ByteP0; var lRandSeed:comp);
{$ENDIF}
var
   lInc,lRand: integer;
   lSwap: byte;
begin
      Move(lOrigOrder^,lRanOrder^,lnSubj);
     for lInc := lnSubj downto 2 do begin
         {$IFDEF NEWRNG}
         lRand := RandomInt0(rng, lInc-1);
         {$ELSE}
         lRand := GenRandThreaded(lInc,lRandSeed);
         {$ENDIF}
         lSwap := lRanOrder^[lRand];
         lRanOrder^[lRand] := lRanOrder^[lInc-1];
         lRanOrder^[lInc-1] := lSwap;
     end;
end;



procedure TLesionStatThread.DoVisualSwap;
begin
  lBarX.Position := lBarPosX;
end;

procedure TLesionStatThread .VisualProg(lPos: Integer);
begin
  lBarPosX := lPos;
  {$IFDEF FPC}Synchronize(@DoVisualSwap); {$ELSE} Synchronize(DoVisualSwap);{$ENDIF}
end;

constructor TLesionStatThread .Create(lBar: TProgressBar; lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lControlsIn : integer; lPlankImg: byteP;lOutImgMn,lOutImgBM,lOutImgT,lOutImgAUC,lSymptomRA: SingleP);
begin

   lBarX := lBar;
     lttestx := lttest;
     lBMx:= lBM;
     lThreadX := lThread;
     lThreadStartX := lThreadStart;
     lThreadEndX := lThreadEnd;
     lStartVoxx := lStartVox;
     lVoxPerPlankx := lVoxPerPlank;
     lImagesCountX := lImagesCount;
     lControlsX := lControlsIn;
     lPlankImgx := lPlankImg;
     lOutImgMnx := lOutImgMn;
     lOutImgBMx := lOutImgBM;
     lOutImgTx := lOutImgT;
     lOutImgAUCx := lOutImgAUC;
     lSymptomRAx := lSymptomRA;
     lnPermuteX := lnPermute;
     lnCritX := lnCrit;
  FreeOnTerminate := True;
  inherited Create(False);
  //inherited Create(CreateSuspended);
end;

{ The Execute method is called when the thread starts }

procedure TLesionStatThread .Execute;
begin
  Analyze(lttestx,lBMx, lnCritX,lnPermuteX,lThreadx,lThreadStartx,lThreadEndx,lStartVoxx,lVoxPerPlankx,lImagesCountx,lControlsx,lPlankImgX,lOutImgMnx,lOutImgBMx,lOutImgTx,lOutImgAUCx,lSymptomRAx);

end;

{$IFDEF OLDSTATS}
procedure StatPermuteThreaded (lttest,lBM: boolean; lnSubj, lnGroup0,lnPermute,lThread: integer;var lOrigOrder: DoubleP0);
var
   lInc: integer;
   lOutT,lDF,lBMz: double;
   {$IFDEF NEWRNG}
   rng: TRNG = (test: FALSE);
   {$ELSE}
   lRandSeed: Comp = 128;
   {$ENDIF}
   lRanOrderp: pointer;
   lRanOrder: Doublep0;
begin
     if (lnSubj < 1) or (lnPermute < 1) then
        exit;
     createArray64(lRanOrderp,lRanOrder,lnSubj);
     //lRandSeed := 128;
     for lInc := 1 to lnPermute do begin
         {$IFDEF NEWRNG}
         GenPermuteThreaded(lnSubj, lOrigOrder,lRanOrder,rng); //generate random order of participants
         {$ELSE}
         GenPermuteThreaded(lnSubj, lOrigOrder,lRanOrder,lRandSeed); //generate random order of participants
         {$ENDIF}
         if lttest then begin
             TStat2 (lnSubj, lnGroup0, lRanOrder, lOutT);
             if lOutT > gPermuteMaxT[lThread,lInc] then
                 gPermuteMaxT[lThread,lInc] := lOutT;
             if lOutT < gPermuteMinT[lThread,lInc] then
                 gPermuteMinT[lThread,lInc] := lOutT;

          end; //compute ttest
          if lBM then begin
             //BMTest (lnSubj, lnGroup0, lRanOrder,lOutT);
             tBM (lnSubj, lnGroup0, lRanOrder,lBMz,lDF);
             lBMz := BMzVal (lnSubj, lnGroup0,lBMz,lDF);

             if lBMz > gPermuteMaxBM[lThread,lInc] then
                gPermuteMaxBM[lThread,lInc] := lBMz;
             if lBMz < gPermuteMinBM[lThread,lInc] then
                gPermuteMinBM[lThread,lInc] := lBMz;
          end; //compute BM
     end;
     freemem(lRanOrderp);
end;

procedure TLesionContinuous.Analyze (lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lControlsIN : integer; lPlankImg:bytep;lOutImgMn,lOutImgBM,lOutImgT,lOutImgAUC,lSymptomRA: SingleP);
//pattern variables
const
     knPrevPattern = 20;
var
   lPrevPatternRA: array[1..knPrevPattern] of TLesionPattern;
   lPattern: TLesionPattern;
   lPrevZValsT,lPrevZValsBM,lPrevAUCVals: array [1..knPrevPattern] of Single;
   lPatternPos: integer;
   lLesionOrderp: bytep;
//standard variables
var

   lObsp: pointer;
   lObs: Doublep0;
   lT,lBMz,lDF: Double;
   lnLesion,lnNoLesion,lPosPct,lPos,lPos2,lPos2Offset,lnControl,
   lnControlsPlusLesion,lnControlsPlusPatients : integer;
begin //statthread
      //init patterns
      lnControl := abs(lControlsIn);
      if lnControl > 0 then begin
         NPMMSG('ANACOM no longer supported');
         exit;
      end;
      //if lControlsIn < 0 then begin //binomial
      //    getmem(lObsB, lImagesCount+lnControl);
      //end;
      lnControlsPlusPatients := lImagesCount+lnControl;
      for lPatternPos := 1 to knPrevPattern do
          lPrevPatternRA[lPatternPos] := EmptyOrder;
      lPatternPos := 1;
      //lMaxLesion := lImagesCount-lnCrit;
      getmem(lLesionOrderp, lImagesCount *sizeof(byte));
      //now init standard variables
    createArray64(lObsp,lObs,lnControlsPlusPatients);
    lPosPct := (lThreadEnd-lThreadStart) div 100;
    //if lThread = 1 then
    //   OutStr( inttostr(lThreadStart)+':'+inttostr(lThreadEnd));   //xxxxx
    for lPos2 := lThreadStart to lThreadEnd do begin
        if (lThread = 1) and ((lPos2 mod lPosPct) = 0) then
           VisualProg(round((lPos2/(lThreadEnd-lThreadStart))*100));
        if Terminated then exit; //goto 345;//abort
        lPos2Offset := lPos2+lStartVox-1;
        lnLesion := 0;
        lnNoLesion := 0;
        for lPos := 1 to lImagesCount do begin
            if lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2] = 0 then begin
               //no lesion
               inc(lnNoLesion);
               lLesionOrderp^[lPos] := 0;
               lObs^[lnNoLesion-1] := lSymptomRA^[lPos];
            end else begin
                //lesion
                inc(lnLesion);
                lLesionOrderp^[lPos] := 1;
                //lObs^[lImagesCount-lnLesion] := lSymptomRA^[lPos]; //note: lObs indexed from zero!
                //lObs^[lImagesCount-lPos+lnNoLesion] := lSymptomRA^[lPos]; //note: lObs indexed from zero!
                lObs^[lImagesCount-lnLesion] := lSymptomRA^[lPos]; //note: lObs indexed from zero!

            end;
        end;
        lOutImgMn^[lPos2Offset] := lnLesion;///lImages.Count;
        if (lnLesion >= lnCrit) and (lnLesion > 0)  and (lnLesion < lImagesCount) then begin
           //when there are 0 lesions or all lesions there is no variability!
           inc(gnVoxTestedRA[lThread]);
           //now check if we have seen this precise lesion order recently...
           lPattern := SetOrderX (lLesionOrderp,lImagesCount);
           lPos := 1;
           while (lPos <= knPrevPattern) and not (SameOrder(lPattern,lPrevPatternRA[lPos],lImagesCount)) do
                 inc(lPos);
           if SameOrder(lPattern,lPrevPatternRA[lPos],lImagesCount) then begin  //lesion pattern is not novel
             if lttest then
                lOutImgT^[lPos2Offset] := lPrevZvalsT[lPos];
             if lBM then
                 lOutImgBM^[lPos2Offset] := lPrevZvalsBM[lPos];
             if lOutImgAUC <> nil then
                lOutImgAUC^[lPos2Offset] := lPrevAUCvals[lPos];
           end  else  begin //lesion pattern is novel
               //record novel pattern
               inc(lPatternPos);
               if lPatternPos > knPrevPattern then
                  lPatternPos := 1;
               lPrevPatternRA[lPatternPos] := lPattern;
               lnControlsPlusLesion := lnControlsPlusPatients;
               (*if (lControlsIn > 0)  then begin //anaCOm
                  createArray64(lObstp,lObst,lImagesCount);
                  for lPos := 1 to lImagesCount do
                      lObst^[lPos-1] := lObs^[lPos-1];
                  for lPos := 1 to lnLesion do
                      lObs^[lPos-1+lnControl] := lObst^[lPos-1+lnNoLesion];
                  freemem(lObstP);
                  for lPos := 1 to lnControl do
                      lObs^[lPos-1] := lSymptomRA^[lPos+lImagesCount];
                  lnControlsPlusLesion := lnControl+lnLesion;
                  lnNoLesion := {lnNoLesion +} lnControl;
               end;//controls  *)

              if lttest then begin
                 (*if lControlsIn > 0 then begin//anacom
                    TStat2Z (lnControlsPlusLesion, lnControl,lObs,lT);
                 end else*)
                     TStat2 (lnControlsPlusLesion, lnNoLesion, lObs,lT);
                 lOutImgT^[lPos2Offset] := lT;
                 lPrevZValsT[lPatternPos] := lT;
              end;

              if lBM then begin
                      tBM (lnControlsPlusLesion, lnNoLesion, lObs,lBMz,lDF);
                      lBMz := BMzVal (lnControlsPlusPatients, lnNoLesion,lBMz,lDF);
                 lOutImgBM^[lPos2Offset] := lBMz;
                 lPrevZValsBM[lPatternPos] := lBMz;
              end;
             if lOutImgAUC <> nil then begin
                 lOutImgAUC^[lPos2Offset] := continROC (lnControlsPlusLesion, lnNoLesion, lObs);
                 lPrevAUCVals[lPatternPos] :=  lOutImgAUC^[lPos2Offset];
             end;
             StatPermuteThreaded (lttest,lBM,lImagesCount, lnNoLesion,lnPermute,lThread, lObs);
           end; //novel lesion pattern
        end; //in brain mask - compute
    end; //for each voxel
    freemem(lObsP);
    freemem(lLesionOrderp);
end;

procedure StatPermuteBinomialThreaded (lnSubj, lnGroup0,lnPermute,lThread: integer;var lOrigOrder: ByteP0);
var
   lInc: integer;
   lOutP: double;
   {$IFDEF NEWRNG}
   rng: TRNG = (test: FALSE);
   {$ELSE}
   lRS: Comp = 128;
   {$ENDIF}
   lRanOrder: byteP0;
begin
     if (lnSubj < 1) or (lnPermute < 1) then
        exit;
     getmem(lRanOrder,lnSubj);
     for lInc := 1 to lnPermute do begin
        {$IFDEF NEWRNG}
        GenPermuteThreadedBinom(lnSubj, lOrigOrder,lRanOrder,rng); //generate random order of participants
        {$ELSE}
        GenPermuteThreadedBinom(lnSubj, lOrigOrder,lRanOrder,lRS); //generate random order of participants
        {$ENDIF}
        Liebermeister2bP (lnSubj, lnGroup0, lRanOrder,lOutP);
        if {(lOutP > 0) and} (lOutP < gPermuteMinT[lThread,lInc])  then begin  //negative correlation
          gPermuteMinT[lThread,lInc] := lOutP;
        end;
        if {(lOutP < 0) and} ( lOutP > gPermuteMaxT[lThread,lInc]) then  //positive correlation
          gPermuteMaxT[lThread,lInc] := lOutP;
     end;
     freemem(lRanOrder);
end;

procedure TLesionBinom.Analyze (lChi2,lLieber: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lControlsIn : integer; lPlankImg: bytep;lOutImgMn,lOutImgL,lOutImgX,lOutImgAUC,lSymptomRA: SingleP);
//procedure TLesionBinomial.Analyze (lChi2,lLieber: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgL,lOutImgX,lSymptomRA: SingleP);
 //pattern variables
const
     knPrevPattern = 10;
var
   lPrevPatternRA: array[1..knPrevPattern] of TLesionPattern;
   lPattern: TLesionPattern;
   lPrevZValsL ,lPrevAUCVals: array [1..knPrevPattern] of Single;
   lPatternPos: integer;
   lLesionOrderp: bytep;
var
   lObs: ByteP0;
   lAUC,lZ: Double;
   lnLesion,lPosPct,lPos,lPos2,lPos2Offset,lnVoxTested: integer;
begin //Binomial StatThread
      //init patterns
      for lPatternPos := 1 to knPrevPattern do
          lPrevPatternRA[lPatternPos] := EmptyOrder;
      lPatternPos := 1;
      getmem(lLesionOrderp, lImagesCount *sizeof(byte));
      //now init standard variables
   //createArray64(lObsp,lObs,lImagesCount);
    getmem(lObs,lImagesCount);
    lPosPct := (lThreadEnd-lThreadStart) div 100;

   for lPos2 := lThreadStart to lThreadEnd do begin
       if (lThread = 1) and ((lPos2 mod lPosPct) = 0) then
           VisualProg(round((lPos2/(lThreadEnd-lThreadStart))*100));
        if Terminated then exit; //goto 345;//abort
        lPos2Offset := lPos2+lStartVox-1;
        lnLesion := 0;
        for lPos := 1 to lImagesCount do begin
            if ((gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos]) = 0 then begin
                                           //no lesion
                                           lObs^[lImagesCount-lPos+lnLesion] := round(lSymptomRA^[lPos]);
                                           lLesionOrderp^[lPos] := 0;
            end else begin
                                            //lesion
                                            inc(lnLesion);
                                            lLesionOrderp^[lPos] := 1;
                                            lObs^[lnLesion-1] := round(lSymptomRA^[lPos]); //note: lObs indexed from zero!
            end;
        end;
        lOutImgMn^[lPos2Offset] := lnLesion;///lImages.Count;
        if (lnLesion >= lnCrit) and (lnLesion > 0) and (lnLesion < lImagesCount)  then begin
           //when there are 0 lesions or all lesions there is no variability!
           inc(gnVoxTestedRA[lThread]);
           //next check patterns
           lPattern := SetOrderX (lLesionOrderp,lImagesCount);
           lPos := 1;
           while (lPos <= knPrevPattern) and not (SameOrder(lPattern,lPrevPatternRA[lPos],lImagesCount)) do
                 inc(lPos);
           if SameOrder(lPattern,lPrevPatternRA[lPos],lImagesCount) then begin  //lesion pattern is not novel
             //if lChi2 then
             //   lOutImgX^[lPos2Offset] := lPrevZvalsX[lPos];
             //if lLieber then
                 lOutImgL^[lPos2Offset] := lPrevZvalsL[lPos];
                 if lOutImgAUC <> nil then
                    lOutImgAUC^[lPos2Offset] := lPrevAUCvals[lPos];
           end else begin //lesion pattern is novel
               //record novel pattern
               inc(lPatternPos);
               if lPatternPos > knPrevPattern then
                  lPatternPos := 1;
               lPrevPatternRA[lPatternPos] := lPattern;
                  Liebermeister2b(lImagesCount, lnLesion, lObs,lAUC,lZ);
                  if lOutImgAUC <> nil then
                     lOutImgAUC^[lPos2Offset] := lAUC;
                  lPrevAUCVals[lPatternPos] := lAUC;
                  lOutImgL^[lPos2Offset] := lZ;
                  lPrevZValsL[lPatternPos] := lZ;
               //end;
               StatPermuteBinomialThreaded (lImagesCount, lnLesion,lnPermute,lThread, lObs);
           end;
        end; //in brain mask - compute
    end; //for each voxel
    freemem(lObs);
    freemem(lLesionOrderp)
end;

{$ELSE} //IFDEF OLDSTATS else NEWSTATS!

procedure GenPermuteThreadedX (lnSubj: integer; var lOrigOrder,lRanOrder: Singlep; var rng: TRNG);
var
   lInc,lRand: integer;
   lSwap: double;
begin
      Move(lOrigOrder^,lRanOrder^,lnSubj*sizeof(single));
     for lInc := lnSubj downto 2 do begin
         lRand := RandomInt(rng, 1, lInc);
         lSwap := lRanOrder^[lRand];
         lRanOrder^[lRand] := lRanOrder^[lInc];
         lRanOrder^[lInc] := lSwap;
     end;
end;

procedure StatPermuteThreaded (lttest,lBM: boolean; lnSubj, lnPermute,lThread: integer; var lGroup: Bytep; var lOrigOrder: Singlep);
var
   lInc, lnGroup0: integer;
   lOutT,lDF,lBMz: double;
   rng: TRNG = (test: FALSE);
   lRanOrder: Singlep;
begin
     if (lnSubj < 1) or (lnPermute < 1) then
        exit;
     getmem(lRanOrder, lnSubj*sizeof(single));
     for lInc := 1 to lnPermute do begin
         GenPermuteThreadedX(lnSubj, lOrigOrder,lRanOrder,rng); //generate random order of participants
         if lttest then begin
             lOutT := TStat3 (lnSubj, lGroup, lRanOrder);
             if lOutT > gPermuteMaxT[lThread,lInc] then
                 gPermuteMaxT[lThread,lInc] := lOutT;
             if lOutT < gPermuteMinT[lThread,lInc] then
                 gPermuteMinT[lThread,lInc] := lOutT;

          end; //compute ttest
          if lBM then begin
             //BMTest (lnSubj, lnGroup0, lRanOrder,lOutT);
             //tBM (lnSubj, lnGroup0, lRanOrder,lBMz,lDF);
             tBM3 (lnSubj, lGroup, lRanOrder,lBMz,lDF, lnGroup0);
             lBMz := BMzVal (lnSubj, lnGroup0,lBMz,lDF);
             if lBMz > gPermuteMaxBM[lThread,lInc] then
                gPermuteMaxBM[lThread,lInc] := lBMz;
             if lBMz < gPermuteMinBM[lThread,lInc] then
                gPermuteMinBM[lThread,lInc] := lBMz;
          end; //compute BM
     end;
     freemem(lRanOrder);
end;

procedure ShowRandomizationOrder (lnSubj, lnPermute: integer);
var
   rng: TRNG = (test: FALSE);
   lRanOrder, lOrigOrder: Singlep;
   i,j: integer;
   lCount: array of array of integer;
   s: string;
begin
     if (lnSubj < 2) or (lnPermute < 2) then exit;
     npmmsg(format('Randomization order for %d subjects and %d permutations', [lnSubj, lnPermute]) );
     getmem(lRanOrder, lnSubj*sizeof(single));
     getmem(lOrigOrder, lnSubj*sizeof(single));
     setlength(lCount, lnSubj+1, lnSubj+1);
     for i := 1 to lnSubj do begin
         lOrigOrder^[i] := i;
         for j := 1 to lnSubj do
             lCount[i,j] := 0;
     end;
     for i := 1 to lnPermute do begin
         GenPermuteThreadedX(lnSubj, lOrigOrder,lRanOrder,rng); //generate random order of participants
         for j := 1 to (lnSubj) do
             inc(lCount[round(lRanOrder^[j]),j]);
     end;
     for i := 1 to (lnSubj) do begin
         s := inttostr(i)+kTab;
         for j := 1 to (lnSubj) do
             s := s + inttostr(lCount[i,j])+kTab;
         npmmsg(s);
     end;
     freemem(lRanOrder);
     freemem(lOrigOrder);
end;

(*procedure txtFx (lnSubj: integer; var lGroup: Bytep; lInX: Singlep; var ltBM,lDF: double; ln0: integer);
var
   s: string;
  myFile : TextFile;
  i: integer;
begin
     AssignFile(myFile, 'TestStat.txt');
     ReWrite(myFile);
     //Append(myFile);
     WriteLn(myFile, format('%d %d %g %g',[lnSubj, ln0, ltBM, lDF]));
     s := kTab;
     for i := 1 to lnSubj do
         s := s + inttostr(lGroup^[i])+kTab;
     Writeln(myFile, s);
     s := kTab;

     for i := 1 to lnSubj do
         s := s + floattostr(lInX^[i])+kTab;
     Writeln(myFile, s);
     CloseFile(myFile);
end;*)

procedure TLesionContinuous.Analyze (lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lControlsIN : integer; lPlankImg:bytep;lOutImgMn,lOutImgBM,lOutImgT,lOutImgAUC,lSymptomRA: SingleP);
//pattern variables
const
     knPrevPattern = 20;
var
   lPrevPatternRA: array[1..knPrevPattern] of TLesionPattern;
   lPattern: TLesionPattern;
   lPrevZValsT,lPrevZValsBM,lPrevAUCVals: array [1..knPrevPattern] of Single;
   lPatternPos: integer;
   lLesionOrderp: bytep;
//standard variables
var

   //lObsp, lObsp1: pointer;
   //lObs, lObs1: Doublep0;
   lT,lBMz,lDF: Double;
   lnLesion,lnNoLesion,lPosPct,lPos,lPos2,lPos2Offset,lnControl,
   lnControlsPlusLesion,lnControlsPlusPatients, lnGroup0 : integer;
begin //statthread
      //init patterns
      lnControl := abs(lControlsIn);
      if lnControl > 0 then begin
         NPMMSG('ANACOM no longer supported');
         exit;
      end;
      //if lControlsIn < 0 then begin //binomial
      //    getmem(lObsB, lImagesCount+lnControl);
      //end;
      lnControlsPlusPatients := lImagesCount+lnControl;
      for lPatternPos := 1 to knPrevPattern do
          lPrevPatternRA[lPatternPos] := EmptyOrder;
      lPatternPos := 1;
      //lMaxLesion := lImagesCount-lnCrit;
      getmem(lLesionOrderp, lImagesCount *sizeof(byte));
      //now init standard variables
    //createArray64(lObsp,lObs,lnControlsPlusPatients);
    //createArray64(lObsp1,lObs1,lnControlsPlusPatients);
    lPosPct := (lThreadEnd-lThreadStart) div 100;
    //if lThread = 1 then
    //   OutStr( inttostr(lThreadStart)+':'+inttostr(lThreadEnd));   //xxxxx
    for lPos2 := lThreadStart to lThreadEnd do begin
        if (lThread = 1) and ((lPos2 mod lPosPct) = 0) then
           VisualProg(round((lPos2/(lThreadEnd-lThreadStart))*100));
        if Terminated then exit; //goto 345;//abort
        lPos2Offset := lPos2+lStartVox-1;
        lnLesion := 0;
        lnNoLesion := 0;
        for lPos := 1 to lImagesCount do begin
            if lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2] = 0 then begin
               //no lesion
               inc(lnNoLesion);
               lLesionOrderp^[lPos] := 0;
               //lObs^[lnNoLesion-1] := lSymptomRA^[lPos];
            end else begin
                //lesion
                inc(lnLesion);
                lLesionOrderp^[lPos] := 1;
                //lObs^[lImagesCount-lnLesion] := lSymptomRA^[lPos]; //note: lObs indexed from zero!
                //lObs^[lImagesCount-lPos+lnNoLesion] := lSymptomRA^[lPos]; //note: lObs indexed from zero!
                //lObs^[lImagesCount-lnLesion] := lSymptomRA^[lPos]; //note: lObs indexed from zero!

            end;
        end;
        lOutImgMn^[lPos2Offset] := lnLesion;///lImages.Count;
        if (lnLesion >= lnCrit) and (lnLesion > 0)  and (lnLesion < lImagesCount) then begin
           //when there are 0 lesions or all lesions there is no variability!
           inc(gnVoxTestedRA[lThread]);
           //now check if we have seen this precise lesion order recently...
           lPattern := SetOrderX (lLesionOrderp,lImagesCount);
           lPos := 1;
           while (lPos <= knPrevPattern) and not (SameOrder(lPattern,lPrevPatternRA[lPos],lImagesCount)) do
                 inc(lPos);
           if SameOrder(lPattern,lPrevPatternRA[lPos],lImagesCount) then begin  //lesion pattern is not novel
             if lttest then
                lOutImgT^[lPos2Offset] := lPrevZvalsT[lPos];
             if lBM then
                 lOutImgBM^[lPos2Offset] := lPrevZvalsBM[lPos];
             if lOutImgAUC <> nil then
                lOutImgAUC^[lPos2Offset] := lPrevAUCvals[lPos];
           end  else  begin //lesion pattern is novel
               //record novel pattern
               inc(lPatternPos);
               if lPatternPos > knPrevPattern then
                  lPatternPos := 1;
               lPrevPatternRA[lPatternPos] := lPattern;
               lnControlsPlusLesion := lnControlsPlusPatients;
              if lttest then begin
                 lT := TStat3 (lnControlsPlusLesion, lLesionOrderp, lSymptomRA);
                 lOutImgT^[lPos2Offset] := lT;
                 lPrevZValsT[lPatternPos] := lT;
              end;

              if lBM then begin
                      tBM3 (lnControlsPlusLesion, lLesionOrderp, lSymptomRA,lBMz,lDF, lnGroup0);
                      //if lBMz > 55 then
                      //   txtFx(lnControlsPlusLesion, lLesionOrderp, lSymptomRA,lBMz,lDF, lnGroup0);
                      lBMz := BMzVal (lnControlsPlusPatients, lnGroup0,lBMz,lDF);
                 lOutImgBM^[lPos2Offset] := lBMz;
                 lPrevZValsBM[lPatternPos] := lBMz;
              end;
             if lOutImgAUC <> nil then begin
                 //lOutImgAUC^[lPos2Offset] := continROC (lnControlsPlusLesion, lnNoLesion, lObs);
                 lOutImgAUC^[lPos2Offset] := continROC3 (lnControlsPlusLesion, lLesionOrderp, lSymptomRA);
                 lPrevAUCVals[lPatternPos] :=  lOutImgAUC^[lPos2Offset];
             end;
             StatPermuteThreaded (lttest,lBM, lImagesCount, lnPermute,lThread, lLesionOrderp, lSymptomRA);
             //StatPermuteThreaded (lttest,lBM,lImagesCount, lnNoLesion,lnPermute,lThread, lObs);
           end; //novel lesion pattern
        end; //in brain mask - compute
    end; //for each voxel
    //freemem(lObsP);
    freemem(lLesionOrderp);
end;

procedure StatPermuteBinomialThreaded (lnSubj, lnPermute,lThread: integer; var lGroup: Bytep; var lOrigOrder: Singlep);
var
   lInc: integer;
   lOutZ: double;
   rng: TRNG = (test: FALSE);
   lRanOrder: Singlep;
begin
     if (lnSubj < 1) or (lnPermute < 1) then
        exit;
     getmem(lRanOrder,lnSubj * sizeof(single));
     for lInc := 1 to lnPermute do begin
        GenPermuteThreadedX(lnSubj, lOrigOrder,lRanOrder,rng); //generate random order of participants
        //lOutP := Liebermeister3bP (lnSubj, lGroup, lRanOrder); // <- for p value (faster)
        lOutZ := Liebermeister3(lnSubj, lGroup, lRanOrder); //for z value, simpler
        if  (lOutZ < gPermuteMinT[lThread,lInc])  then  //negative correlation
          gPermuteMinT[lThread,lInc] := lOutZ;
        if {(lOutP < 0) and} ( lOutZ > gPermuteMaxT[lThread,lInc]) then  //positive correlation
          gPermuteMaxT[lThread,lInc] := lOutZ;
     end;
     freemem(lRanOrder);
end;

procedure TLesionBinom.Analyze (lChi2,lLieber: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lControlsIn : integer; lPlankImg: bytep;lOutImgMn,lOutImgL,lOutImgX,lOutImgAUC,lSymptomRA: SingleP);
//procedure TLesionBinomial.Analyze (lChi2,lLieber: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgL,lOutImgX,lSymptomRA: SingleP);
 //pattern variables
const
     knPrevPattern = 10;
var
   lPrevPatternRA: array[1..knPrevPattern] of TLesionPattern;
   lPattern: TLesionPattern;
   lPrevZValsL ,lPrevAUCVals: array [1..knPrevPattern] of Single;
   lPatternPos: integer;
   lLesionOrderp: bytep;
var
   lObs: ByteP0;
   lAUC,lZ: Double;
   lnLesion,lPosPct,lPos,lPos2,lPos2Offset,lnVoxTested: integer;
begin //Binomial StatThread
      //init patterns
      for lPatternPos := 1 to knPrevPattern do
          lPrevPatternRA[lPatternPos] := EmptyOrder;
      lPatternPos := 1;
      getmem(lLesionOrderp, lImagesCount *sizeof(byte));
      //now init standard variables
   //createArray64(lObsp,lObs,lImagesCount);
    getmem(lObs,lImagesCount);
    lPosPct := (lThreadEnd-lThreadStart) div 100;

   for lPos2 := lThreadStart to lThreadEnd do begin
       if (lThread = 1) and ((lPos2 mod lPosPct) = 0) then
           VisualProg(round((lPos2/(lThreadEnd-lThreadStart))*100));
        if Terminated then exit; //goto 345;//abort
        lPos2Offset := lPos2+lStartVox-1;
        lnLesion := 0;
        for lPos := 1 to lImagesCount do begin
            if ((gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos]) = 0 then begin
                                           //no lesion
                                           lObs^[lImagesCount-lPos+lnLesion] := round(lSymptomRA^[lPos]);
                                           lLesionOrderp^[lPos] := 0;
            end else begin
                                            //lesion
                                            inc(lnLesion);
                                            lLesionOrderp^[lPos] := 1;
                                            lObs^[lnLesion-1] := round(lSymptomRA^[lPos]); //note: lObs indexed from zero!
            end;
        end;
        lOutImgMn^[lPos2Offset] := lnLesion;///lImages.Count;
        if (lnLesion >= lnCrit) and (lnLesion > 0) and (lnLesion < lImagesCount)  then begin
           //when there are 0 lesions or all lesions there is no variability!
           inc(gnVoxTestedRA[lThread]);
           //next check patterns
           lPattern := SetOrderX (lLesionOrderp,lImagesCount);
           lPos := 1;
           while (lPos <= knPrevPattern) and not (SameOrder(lPattern,lPrevPatternRA[lPos],lImagesCount)) do
                 inc(lPos);
           if SameOrder(lPattern,lPrevPatternRA[lPos],lImagesCount) then begin  //lesion pattern is not novel
             //if lChi2 then
             //   lOutImgX^[lPos2Offset] := lPrevZvalsX[lPos];
             //if lLieber then
                 lOutImgL^[lPos2Offset] := lPrevZvalsL[lPos];
                 if lOutImgAUC <> nil then
                    lOutImgAUC^[lPos2Offset] := lPrevAUCvals[lPos];
           end else begin //lesion pattern is novel
               //record novel pattern
               inc(lPatternPos);
               if lPatternPos > knPrevPattern then
                  lPatternPos := 1;
               lPrevPatternRA[lPatternPos] := lPattern;
                  lZ := Liebermeister3b(lImagesCount, lLesionOrderp, lSymptomRA,lAUC);
                  if lOutImgAUC <> nil then
                     lOutImgAUC^[lPos2Offset] := lAUC;
                  lPrevAUCVals[lPatternPos] := lAUC;
                  lOutImgL^[lPos2Offset] := lZ;
                  lPrevZValsL[lPatternPos] := lZ;
               //end;
              // StatPermuteBinomialThreaded (lImagesCount, lnLesion,lnPermute,lThread, lObs);
               StatPermuteBinomialThreaded (lImagesCount, lnPermute,lThread, lLesionOrderp, lSymptomRA);
           end;
        end; //in brain mask - compute
    end; //for each voxel
    freemem(lObs);
    freemem(lLesionOrderp)
end;

{$ENDIF}



end.
