unit StatThds;
  {$Include ..\common\isgui.inc}
interface

uses
 {$IFDEF GUI} ComCtrls,{$ENDIF}
 //ComCtrls,   Graphics, ExtCtrls,
 Classes, define_types,stats,StatThdsUtil,Brunner,lesion_pattern, dialogsx;



type

  TStatThread = class(TThread)
  private
    lBarX: TProgressBar;
    lttestx,lBMx: boolean;
    lnCritx,lBarPosX,lnPermuteX,lThreadx,lThreadStartx,lThreadEndx,lStartVoxx,lVoxPerPlankx,lImagesCountx,lnGroup1x : integer;
    lMaskImgx,lPlankImgx,lOutImgMnx,lOutImgBMx,lOutImgTx,lSymptomRAx: SingleP;
        procedure DoVisualSwap;
  protected
    procedure Execute; override;
    //procedure Terminate;
    procedure VisualProg(lPos: Integer);
    procedure Analyze(lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lSymptomRA: SingleP); virtual; abstract;
  public
    constructor Create(lBar: TProgressBar;lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lSymptomRA: SingleP);
  end;

{ VBM - two groups }

  TNNStat = class(TStatThread)
  protected
    procedure Analyze(lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lSymptomRA: SingleP); override;
  end;


  TPairedTStat = class(TStatThread)
  protected
    procedure Analyze(lunused1,lunused2: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lSymptomRA: SingleP); override;
  end;

  { Lesion - image reveals value }

  TLesionStat = class(TStatThread)
  protected
    procedure Analyze(lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lSymptomRA: SingleP);  override;
  end;

  TLesionBinomial = class(TStatThread)
  protected
    procedure Analyze(lChi2,lLieber: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgL,lOutImgX,lSymptomRA: SingleP);  override;
  end;

implementation
uses unpm;
//uses Stat;


{ TSortThread }

(*tpIdle	The thread executes only when the system is idle. The system will not interrupt other threads to execute a thread with tpIdle priority.
tpLowest	The thread's priority is two points below normal.
tpLower	The thread's priority is one point below normal.
tpNormal	The thread has normal priority.
tpHigher	The thread's priority is one point above normal.
tpHighest	The thread's priority is two points above normal.
tpTimeCritical*)

Const Two32 = 4294967296.0 ;
function GenRandThreaded(lRange: integer; var lRandSeed:comp): integer;
//normal random function does not work well when threaded - randseed is changed by each thread
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
end;

procedure StatPermuteThreaded (lttest,lBM: boolean; lnSubj, lnGroup0,lnPermute,lThread: integer;var lOrigOrder: DoubleP0);
var
   lInc: integer;
   lOutT: double;
   lRS: Comp;
   lRanOrderp: pointer;
   lRanOrder: Doublep0;
begin
     if (lnSubj < 1) or (lnPermute < 1) then
        exit;
     createArray64(lRanOrderp,lRanOrder,lnSubj);
     lRS := 128;
     for lInc := 1 to lnPermute do begin
         GenPermuteThreaded(lnSubj, lOrigOrder,lRanOrder,lRS); //generate random order of participants
         if lttest then begin
             TStat2 (lnSubj, lnGroup0, lRanOrder, lOutT);
             if lOutT > gPermuteMaxT[lThread,lInc] then
                gPermuteMaxT[lThread,lInc] := lOutT;
             if lOutT < gPermuteMinT[lThread,lInc] then
                gPermuteMinT[lThread,lInc] := lOutT;
          end; //compute ttest
          if lBM then begin
             BMTest (lnSubj, lnGroup0, lRanOrder,lOutT);
             if lOutT > gPermuteMaxBM[lThread,lInc] then
                gPermuteMaxBM[lThread,lInc] := lOutT;
             if lOutT < gPermuteMinBM[lThread,lInc] then
                gPermuteMinBM[lThread,lInc] := lOutT;
          end; //compute BM
     end;
     freemem(lRanOrderp);
end;

procedure StatPermuteBinomialThreaded (lChi2,lLieber: boolean; lnSubj, lnGroup0,lnPermute,lThread: integer;var lOrigOrder: DoubleP0);
var
   lInc: integer;
   lOutT: double;
   lRS: Comp;
   lRanOrderp: pointer;
   lRanOrder: Doublep0;
begin
     if (lnSubj < 1) or (lnPermute < 1) then
        exit;
     createArray64(lRanOrderp,lRanOrder,lnSubj);
     lRS := 128;
     for lInc := 1 to lnPermute do begin
         GenPermuteThreaded(lnSubj, lOrigOrder,lRanOrder,lRS); //generate random order of participants
         if lChi2 then begin
             Chi2 (lnSubj, lnGroup0, lRanOrder, lOutT);
             if lOutT > gPermuteMaxT[lThread,lInc] then
                gPermuteMaxT[lThread,lInc] := lOutT;
             if lOutT < gPermuteMinT[lThread,lInc] then
                gPermuteMinT[lThread,lInc] := lOutT;
          end; //compute ttest
          if lLieber then begin
             Liebermeister2 (lnSubj, lnGroup0, lRanOrder,lOutT);
             if lOutT > gPermuteMaxBM[lThread,lInc] then
                gPermuteMaxBM[lThread,lInc] := lOutT;
             if lOutT < gPermuteMinBM[lThread,lInc] then
                gPermuteMinBM[lThread,lInc] := lOutT;
          end; //compute BM
     end;
     freemem(lRanOrderp);
end;


procedure TStatThread.DoVisualSwap;
begin
     {$IFDEF GUI}
  lBarX.Position := lBarPosX;
     {$ENDIF}
end;

procedure TStatThread.VisualProg(lPos: Integer);
begin
          {$IFDEF GUI}
  lBarPosX := lPos;
  {$IFDEF FPC}Synchronize(@DoVisualSwap); {$ELSE} Synchronize(DoVisualSwap);{$ENDIF}
       {$ENDIF}
end;

constructor TStatThread.Create(lBar: TProgressBar; lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lSymptomRA: SingleP);
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
     lnGroup1x := lnGroup1;
     lMaskImgx := lMaskImg;
     lPlankImgx := lPlankImg;
     lOutImgMnx := lOutImgMn;
     lOutImgBMx := lOutImgBM;
     lOutImgTx := lOutImgT;
     lSymptomRAx := lSymptomRA;
     lnPermuteX := lnPermute;
     lnCritX := lnCrit;
  FreeOnTerminate := True;
  inherited Create(False);
end;



{ The Execute method is called when the thread starts }

procedure TStatThread.Execute;
begin
  Analyze(lttestx,lBMx, lnCritX,lnPermuteX,lThreadx,lThreadStartx,lThreadEndx,lStartVoxx,lVoxPerPlankx,lImagesCountx,lnGroup1x,lMaskImgx,lPlankImgX,lOutImgMnx,lOutImgBMx,lOutImgTx,lSymptomRAx);
end;

(*procedure TStatThread.Terminate;
begin
       Dec(gThreadsRunning);
       NPMmsg('Thread done');
     inherited  Terminate;
end;  *)

{ Nearest Nighbor }
procedure TNNStat.Analyze(lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lSymptomRA: SingleP);
var
   lObsp: pointer;
   lObs: Doublep0;
   lT: Double;

   lPosPct,lPos,lPos2,lPos2Offset: integer;
   lSum: single;
begin //statthread
    createArray64(lObsp,lObs,lImagesCount);
    lPosPct := (lThreadEnd-lThreadStart) div 100;
    for lPos2 := lThreadStart to lThreadEnd do begin
        if (lThread = 1) and ((lPos2 mod lPosPct) = 0) then
           VisualProg(round((lPos2/(lThreadEnd-lThreadStart))*100));
        if Terminated then exit; //goto 345;//abort
        lPos2Offset := lPos2+lStartVox-1;
        if lMaskImg^[lPos2Offset] <> 0 then begin
           inc(gnVoxTestedRA[lThread]);
           lSum := 0;
           for lPos := 1 to lImagesCount do begin
               lObs^[lPos-1] := (gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos];
               lSum := lSum +  lObs^[lPos-1];
           end;
           lOutImgMn^[lPos2Offset] := lSum/lImagesCount;
           if lttest then begin
              TStat2 (lImagesCount, lnGroup1, lObs,lT);
              lOutImgT^[lPos2Offset] := lT;
           end;
           if lBM then begin
              BMTest(lImagesCount, lnGroup1, lObs,lT);
              lOutImgBM^[lPos2Offset] := lT;

              //TStatAbs (lImagesCount, lnGroup1, lObs,lT);
              //lOutImgBM[lPos2Offset] := lT;
           end;
           StatPermuteThreaded (lttest,lBM,lImagesCount, lnGroup1,lnPermute,lThread, lObs);
        end; //in brain mask - compute
    end; //for each voxel
    freemem(lObsP);
    Terminate;
end;


{ Paired T-Test}
(*procedure PairedTTest (N, SumOfDifSqrs, SumDif: double;var t, p,DF: double);
        var
			meanDif, SumDifSqr, temp: double;
	begin
		df := n - 1;
                t := 0;
                p := 1;

        if (SumOfDifSqrs <> 0)and (SumDif <> 0)and (df <> 0) and (N <> 0) then begin
           meanDif := SumDif / N;
           SumDifSqr := sqr(SumDif);
           temp := SumOfDifSqrs - (SumDifSqr / n);
           temp := temp / (n * df);
           temp := sqrt(temp);
           if temp <> 0 then begin
              t := meanDif / temp;
              p := betai(0.5 * df, 0.5, df / (df + sqr(t)))
           end else {t is infinitely big}
               p := -1.0;
        end;
end; {paired ttest}    *)


procedure TPairedTStat.Analyze(lUnused1,lUnused2: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lSymptomRA: SingleP);
var
   lObsp: pointer;
   lObs: Doublep0;
   lT: Double;
   lPosPct,lPos,lPos2,lPos2Offset: integer;
   lSum: single;
begin //statthread
    createArray64(lObsp,lObs,lImagesCount);
    lPosPct := (lThreadEnd-lThreadStart) div 100;
    for lPos2 := lThreadStart to lThreadEnd do begin
        if (lThread = 1) and ((lPos2 mod lPosPct) = 0) then
           VisualProg(round((lPos2/(lThreadEnd-lThreadStart))*100));
        if Terminated then exit; //goto 345;//abort
        lPos2Offset := lPos2+lStartVox-1;
        if lMaskImg^[lPos2Offset] <> 0 then begin
           inc(gnVoxTestedRA[lThread]);
           lSum := 0;
           for lPos := 1 to lImagesCount do begin
               lObs^[lPos-1] := (gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos];
               lSum := lSum +  lObs^[lPos-1];
           end;
           lOutImgMn^[lPos2Offset] := lSum/lImagesCount;
           PairedTStat (lImagesCount, lObs,lT);
           lOutImgT^[lPos2Offset] := lT;
           //StatPermuteThreaded (lttest,lBM,lImagesCount, lnGroup1,lnPermute,lThread, lObs);
        end; //in brain mask - compute
    end; //for each voxel
    freemem(lObsP);
end;

(*procedure TLesionStat.Analyze (lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lSymptomRA: SingleP);
var
   lObsp: pointer;
   lObs: Doublep0;
   lT,lBMz,lDF: Double;
   lnLesion,lnNoLesion,lPosPct,lPos,lPos2,lPos2Offset,lnVoxTested: integer;
begin //statthread
    createArray64(lObsp,lObs,lImagesCount);
    lPosPct := (lThreadEnd-lThreadStart) div 100;
    for lPos2 := lThreadStart to lThreadEnd do begin
        if (lThread = 1) and ((lPos2 mod lPosPct) = 0) then
           VisualProg(round((lPos2/(lThreadEnd-lThreadStart))*100));
        if Terminated then exit; //goto 345;//abort
        lPos2Offset := lPos2+lStartVox-1;
        lnLesion := 0;
        lnNoLesion := 0;
        for lPos := 1 to lImagesCount do begin
            if ((gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos]) = 0 then begin
                                           //no lesion
                                           inc(lnNoLesion);
                                           lObs^[lnNoLesion-1] := lSymptomRA^[lPos];

            end else begin
                                            //lesion
                                            inc(lnLesion);
                                            lObs^[lImagesCount-lPos+lnNoLesion] := lSymptomRA^[lPos]; //note: lObs indexed from zero!
            end;
        end;
        lOutImgMn^[lPos2Offset] := lnLesion;///lImages.Count;
           if (lnLesion >= lnCrit) and (lnLesion > 0) then begin
                                   inc(gnVoxTestedRA[lThread]);
           if lttest then begin
              TStat2 (lImagesCount, lnNoLesion, lObs,lT);
              lOutImgT^[lPos2Offset] := lT;
           end;
           if lBM then begin
              tBM (lImagesCount, lnNoLesion, lObs,lBMz,lDF);
              BMzVal (lImagesCount, lnNoLesion,lBMz,lDF);
              lOutImgBM^[lPos2Offset] := lBMz;
           end;
           StatPermuteThreaded (lttest,lBM,lImagesCount, lnNoLesion,lnPermute,lThread, lObs);
        end; //in brain mask - compute
    end; //for each voxel
    freemem(lObsP);
end;*)

procedure TLesionStat.Analyze (lttest,lBM: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgBM,lOutImgT,lSymptomRA: SingleP);
//pattern variables
const
     knPrevPattern = 10;
var
   lPrevPatternRA: array[1..knPrevPattern] of TLesionPattern;
   lPattern: TLesionPattern;
   lPrevZValsT,lPrevZValsBM: array [1..knPrevPattern] of Single;
   lPatternPos: integer;
   lLesionOrderp: bytep;
//standard variables
var

   lObsp: pointer;
   lObs: Doublep0;
   lT,lBMz,lDF: Double;
   lnLesion,lnNoLesion,lPosPct,lPos,lPos2,lPos2Offset,lnVoxTested: integer;
begin //statthread
      //init patterns
      for lPatternPos := 1 to knPrevPattern do
          lPrevPatternRA[lPatternPos] := EmptyOrder;
      lPatternPos := 1;
      getmem(lLesionOrderp, lImagesCount *sizeof(byte));
      //now init standard variables
    createArray64(lObsp,lObs,lImagesCount);
    lPosPct := (lThreadEnd-lThreadStart) div 100;
    for lPos2 := lThreadStart to lThreadEnd do begin
        if (lThread = 1) and ((lPos2 mod lPosPct) = 0) then
           VisualProg(round((lPos2/(lThreadEnd-lThreadStart))*100));
        if Terminated then exit; //goto 345;//abort
        lPos2Offset := lPos2+lStartVox-1;
        lnLesion := 0;
        lnNoLesion := 0;
        for lPos := 1 to lImagesCount do begin
            if ((gScaleRA[lPos]*lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2])+gInterceptRA[lPos]) = 0 then begin
               //no lesion
               inc(lnNoLesion);
               lLesionOrderp^[lPos] := 0;
               lObs^[lnNoLesion-1] := lSymptomRA^[lPos];
            end else begin
                //lesion
                inc(lnLesion);
                lLesionOrderp^[lPos] := 1;
                lObs^[lImagesCount-lPos+lnNoLesion] := lSymptomRA^[lPos]; //note: lObs indexed from zero!
            end;
        end;
        lOutImgMn^[lPos2Offset] := lnLesion;///lImages.Count;
        if (lnLesion >= lnCrit) and (lnLesion > 0) then begin
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
           end else begin //lesion pattern is novel
               //record novel pattern
               inc(lPatternPos);
               if lPatternPos > knPrevPattern then
                  lPatternPos := 1;
               lPrevPatternRA[lPatternPos] := lPattern;


              if lttest then begin
                 TStat2 (lImagesCount, lnNoLesion, lObs,lT);
                 lOutImgT^[lPos2Offset] := lT;
                 lPrevZValsT[lPatternPos] := lT;
              end;
              if lBM then begin
                 tBM (lImagesCount, lnNoLesion, lObs,lBMz,lDF);
                 BMzVal (lImagesCount, lnNoLesion,lBMz,lDF);
                 lOutImgBM^[lPos2Offset] := lBMz;
                 lPrevZValsBM[lPatternPos] := lBMz;
              end;
              StatPermuteThreaded (lttest,lBM,lImagesCount, lnNoLesion,lnPermute,lThread, lObs);
           end; //novel lesion pattern
        end; //in brain mask - compute
    end; //for each voxel
    freemem(lObsP);
    freemem(lLesionOrderp)

end;

procedure TLesionBinomial.Analyze (lChi2,lLieber: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgL,lOutImgX,lSymptomRA: SingleP);
 //pattern variables
const
     knPrevPattern = 10;
var
   lPrevPatternRA: array[1..knPrevPattern] of TLesionPattern;
   lPattern: TLesionPattern;
   lPrevZValsL,lPrevZValsX: array [1..knPrevPattern] of Single;
   lPatternPos: integer;
   lLesionOrderp: bytep;
//standard variables
var
   lObsp: pointer;
   lObs: Doublep0;
   lT: Double;
   lnLesion,lPosPct,lPos,lPos2,lPos2Offset,lnVoxTested: integer;
begin //Binomial StatThread
      //init patterns
      for lPatternPos := 1 to knPrevPattern do
          lPrevPatternRA[lPatternPos] := EmptyOrder;
      lPatternPos := 1;
      getmem(lLesionOrderp, lImagesCount *sizeof(byte));
      //now init standard variables
   createArray64(lObsp,lObs,lImagesCount);
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
                                           lObs^[lImagesCount-lPos+lnLesion] := lSymptomRA^[lPos];
                                           lLesionOrderp^[lPos] := 0;
            end else begin
                                            //lesion
                                            inc(lnLesion);
                                            lLesionOrderp^[lPos] := 1;
                                            lObs^[lnLesion-1] := lSymptomRA^[lPos]; //note: lObs indexed from zero!
            end;
        end;
        lOutImgMn^[lPos2Offset] := lnLesion;///lImages.Count;
        if (lnLesion >= lnCrit) and (lnLesion > 0) then begin
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
           end else begin //lesion pattern is novel
               //record novel pattern
               inc(lPatternPos);
               if lPatternPos > knPrevPattern then
                  lPatternPos := 1;
               lPrevPatternRA[lPatternPos] := lPattern;

               {if lChi2 then begin
                  Chi2 (lImagesCount, lnLesion, lObs,lT);
                  lOutImgX^[lPos2Offset] := lT;//lT;
                  lPrevZValsX[lPatternPos] := lT;
               end;
               if lLieber then begin}
                  Liebermeister2(lImagesCount, lnLesion, lObs,lT);
                  lOutImgL^[lPos2Offset] := lT;
                  lPrevZValsL[lPatternPos] := lT;
               //end;
               StatPermuteBinomialThreaded ({lChi2}false,lLieber,lImagesCount, lnLesion,lnPermute,lThread, lObs);
           end;
        end; //in brain mask - compute
    end; //for each voxel
    freemem(lObsP);
    freemem(lLesionOrderp)
end;

(*procedure TLesionBinomial.Analyze (lChi2,lLieber: boolean; lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount,lnGroup1 : integer; lMaskImg,lPlankImg,lOutImgMn,lOutImgL,lOutImgX,lSymptomRA: SingleP);
var
   lObsp: pointer;
   lObs: Doublep0;
   lT: Double;
   lnLesion,lPosPct,lPos,lPos2,lPos2Offset,lnVoxTested: integer;
begin //statthread
   createArray64(lObsp,lObs,lImagesCount);
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
                                           lObs^[lImagesCount-lPos+lnLesion] := lSymptomRA^[lPos];
            end else begin
                                            //lesion
                                            inc(lnLesion);
                                            lObs^[lnLesion-1] := lSymptomRA^[lPos]; //note: lObs indexed from zero!
            end;
        end;
        lOutImgMn^[lPos2Offset] := lnLesion;///lImages.Count;
           if (lnLesion >= lnCrit) and (lnLesion > 0) then begin
                                   inc(gnVoxTestedRA[lThread]);
           if lChi2 then begin
              Chi2 (lImagesCount, lnLesion, lObs,lT);
              lOutImgX^[lPos2Offset] := lT;//lT;
           end;
           if lLieber then begin
              Liebermeister2(lImagesCount, lnLesion, lObs,lT);
              lOutImgL^[lPos2Offset] := lT;
           end;
           StatPermuteBinomialThreaded (lChi2,lLieber,lImagesCount, lnLesion,lnPermute,lThread, lObs);

        end; //in brain mask - compute
    end; //for each voxel
    freemem(lObsP);

end;*)


end.