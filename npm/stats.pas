unit stats;


interface                                                                                                         
uses define_types,statcr,DISTR
,SysUtils,Dialogsx;
procedure TStat2 (lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lOutT: double);
//procedure TStatAbs (lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lOutT: double);
procedure PairedTStat (lnSubj: integer; var lIn: DoubleP0; var lOutT: double);
procedure TStatWelch (lnSubj, lnGroup0: integer; var lIn: DoubleP0; var lOutT: double);
procedure WilcoxonMW2 (lnSubj, lnGroup0: integer; var lIn: DoubleP0; var lOutT: double);
procedure MeanMedian(lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lMeanFX,lMedianFX: double);
procedure TStat2Z (lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lOutT: double);
procedure BMTest (lnSubj, lnGroup0: integer; var lIn: DoubleP0; var lOutT: double);
procedure Liebermeister2 (lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lOutZ: double);
procedure Liebermeister2b (lnSubj, lnGroupX: integer; var lIn: ByteP0; var lAUC,lOutZ: double);
procedure Liebermeister2bP (lnSubj, lnGroupX: integer; var lIn: ByteP0; var lOutP: double);
//procedure Liebermeister2bPlus (lnSubj, lnGroupX: integer; var lIn: ByteP0; var lAUC, lOutP: double);
//function Aprime (lHit,lFA: double): double;
//function AUC (lHit,lFA: double): double;
//function rocAUC (lHit,lFA: double): double;
function rocAUC (lnYesDeficitYesLesion,lnNoDeficitYesLesion,lnYesDeficitNoLesion,lnNoDeficitNoLesion: integer): double;

procedure Chi2 (lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lOutZ: double);

implementation

procedure Chi2 (lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lOutZ: double);
var
	lVal: double;
        // luChiP: double;
	i,lnYesDeficit1,lnYesDeficit0,lnNoDeficit1,lnNoDeficit0,
	lnYesDeficit,lnNoDeficit: integer;
begin
	lnYesDeficit0 := 0;
	lnYesDeficit1 := 0;
	lnNoDeficit0 := 0;
	lnNoDeficit1 := 0;
        for i := 0 to (lnGroupX-1) do begin //for each subject
		if lIn^[i] = 0 then
                   inc(lnYesDeficit0)
                else
                    inc(lnNoDeficit0);
        end;
	for i := lnGroupX to (lnSubj-1) do begin //for each subject
		if lIn^[i] = 0 then
                   inc(lnYesDeficit1)
                else
                    inc(lnNoDeficit1);
	end; //for each sub
	lnYesDeficit :=lnYesDeficit0+lnYesDeficit1;
	lnNoDeficit := lnNoDeficit0+lnNoDeficit1;
        if  (lnYesDeficit<1) or (lnNoDeficit<1) then
		lOutZ := 0
	else begin
		lVal := Fisher(lnYesDeficit0, lnYesDeficit1, lnNoDeficit0, lnNoDeficit1);
		if lVal < 0 then
			lOutZ := -pNormalInv(abs(lVal))
		else
			lOutZ := pNormalInv(lVal)
		(*Chi2x2 (lnYesDeficit0, lnYesDeficit1, lnNoDeficit0, lnNoDeficit1,lMinExp,lChi,lChip,luChi, luChiP);
		if (lnYesDeficit1/lnYesDeficit) > (lnNoDeficit1/lnNoDeficit) then
			lOutZ := -luChi//t = m / d;
		else
			lOutZ := luChi;//t = m / d; *)
	end; //compute chi
end;

function rocAz (lHit,lFA: double): double;
//see Zhang and Mueller, 2005, Psychometrika 70, 145-154
var
   lH,lF: double;
begin
    if (lHit = 1) and (lFA = 0) then begin
        result := 1;
        exit;
    end;
    if (lHit = 0) and (lFA = 1) then begin
        result := 0;
        exit;
    end;

    if lHit >= lFA then begin//normal: better than chance
        lH := lHit;
        lF := lFA;
    end else begin //..else worse than chance
        lF := lHit;
        lH := lFA;

    end;
    if (lF <= 0.5) and (0.5 <= lH) then
       result := 0.75+ ((lH-lF)*0.25)- lF*(1-lH)
    else if (lF <= lH) and (lH < 0.5) then begin
         if (4*lH) = 0 then
            result := 0.5
         else
             result := 0.75+ ((lH-lF)*0.25)- (lF/(4*lH))
    end else if (0.5 < lF) and (lF <= lH) then begin
         if (4*(1-lF)) = 0 then
            result := 0.5
         else
             result := 0.75 + ((lH-lF)*0.25) - ((1-lH)/(4*(1-lF)))
    end else
        ShowMsg('error in Zhang and Mueller, 2005 (func rocA)');

    if lHit < lFA then //worse than chance
       result := 1 - result;
end;

function rocAUC (lnYesDeficitYesLesion,lnNoDeficitYesLesion,lnYesDeficitNoLesion,lnNoDeficitNoLesion: integer): double;
var
   lHitRate,lFalseAlarmRate: double;
begin
     result := 0.5;
     if ((lnYesDeficitYesLesion+lnNoDeficitYesLesion)=0) or ((lnYesDeficitNoLesion+lnNoDeficitNoLesion)=0) then
        exit;
     lHitRate := lnYesDeficitYesLesion/(lnYesDeficitYesLesion+lnNoDeficitYesLesion);
     lFalseAlarmRate := lnYesDeficitNoLesion/(lnYesDeficitNoLesion+lnNoDeficitNoLesion);
     result := rocAz(lHitRate,lFalseAlarmRate);
end;

(*function Aprime (lHit,lFA: double): double;
//see Wickens Elementary Signal Detection, equation 4.11, page 71
//problem - not symetrical: values less than 0.5 extreme -
//  does not deal with lFA > lHit
begin
    if (lFA=1) or (lHit = 0) then
       result := 0.5 //avoid divide by zero
    else
        result := 1 - 0.25*( ((1-lHit)/(1-lFA)) + lFA/lHit);
end;*)

(*function AUC (lHit,lFA: double): double;
var
   lNum,lDenom: double;
begin

    if (lHit> lFA) then begin
        lNum := (lHit-lFA)*(1+lHit-lFA);
        lDenom := 4 * lHit * (1 - lFA);
        if lDenom = 0 then
           result := 0
        else
            result := 0.5+   (lNum/lDenom);
    end else begin
        lNum := (lFA-lHit)*(1+lFA-lHit);
        lDenom := 4 * lFA * (1 - lHit);
        if lDenom = 0 then
           result := 0
        else
            result := 0.5-   (lNum/lDenom);
    end;
end;    *)

procedure ROCbinomialAUC (lnSubj, lnGroupX: integer; var lIn: ByteP0; var lAUC: double);
//Receiver operating characteristic area under curve for binimial data
//Liebermeister QuasiExact - excellent power
var
	i,lnYesDeficit1,lnYesDeficit0,lnNoDeficit1,lnNoDeficit0,
	lnYesDeficit,lnNoDeficit: integer;
        //lHitRate,lFalseAlarmRate: double;
begin
	lnYesDeficit0 := 0;
	lnYesDeficit1 := 0;
	lnNoDeficit0 := 0;
	lnNoDeficit1 := 0;
        for i := 0 to (lnGroupX-1) do begin //for each subject
		if lIn^[i] = 0 then
                   inc(lnYesDeficit0)
                else
                    inc(lnNoDeficit0);
        end;
	for i := lnGroupX to (lnSubj-1) do begin //for each subject
		if lIn^[i] = 0 then
                   inc(lnYesDeficit1)
                else
                    inc(lnNoDeficit1);
	end; //for each sub
        lAUC := rocAUC (lnYesDeficit1,lnNoDeficit1,lnYesDeficit0,lnNoDeficit0);
        (*lHitRate := lnYesDeficit1/(lnYesDeficit1+lnNoDeficit1);
        lFalseAlarmRate := lnYesDeficit0/(lnYesDeficit0+lnNoDeficit0);
        lAUC := rocA {AUC} (lHitRate,lFalseAlarmRate);    *)
end;

procedure Liebermeister2bP (lnSubj, lnGroupX: integer; var lIn: ByteP0; var lOutP: double);
//Liebermeister QuasiExact - excellent power
var
	i,lnYesDeficit1,lnYesDeficit0,lnNoDeficit1,lnNoDeficit0,
	lnYesDeficit,lnNoDeficit: integer;
	//lMaxChi,lMinChi: single;
begin
	lnYesDeficit0 := 0;
	lnYesDeficit1 := 0;
	lnNoDeficit0 := 0;
	lnNoDeficit1 := 0;
        for i := 0 to (lnGroupX-1) do begin //for each subject
		if lIn^[i] = 0 then
                   inc(lnYesDeficit0)
                else
                    inc(lnNoDeficit0);
        end;
	for i := lnGroupX to (lnSubj-1) do begin //for each subject
		if lIn^[i] = 0 then
                   inc(lnYesDeficit1)
                else
                    inc(lnNoDeficit1);
	end; //for each sub
	lnYesDeficit :=lnYesDeficit0+lnYesDeficit1;
	lnNoDeficit := lnNoDeficit0+lnNoDeficit1;
	if  (lnYesDeficit<1) or (lnNoDeficit<1) then
		lOutP := 0
	else begin
		lOutP := Liebermeister(lnYesDeficit0, lnYesDeficit1, lnNoDeficit0, lnNoDeficit1);
	end; //compute chi
end;


procedure Liebermeister2b (lnSubj, lnGroupX: integer; var lIn: ByteP0; var lAUC,lOutZ: double);
//(lnRow,lnCol: integer; var lIn,lOutZ: DoubleP0);
//Liebermeister QuasiExact - excellent power
var
	lVal: double;
	i,lnYesDeficitNoLesion,lnYesDeficitYesLesion,lnNoDeficitNoLesion,lnNoDeficitYesLesion,
	lnYesDeficit,lnNoDeficit: integer;
        //lHitRate,lFalseAlarmRate: double;
	//lMaxChi,lMinChi: single;
begin
	lnYesDeficitYesLesion := 0;
	lnYesDeficitNoLesion := 0;
	lnNoDeficitYesLesion := 0;
	lnNoDeficitNoLesion := 0;
        for i := 0 to (lnGroupX-1) do begin //for each lesioned subject
		if lIn^[i] = 0 then
                   inc(lnYesDeficitYesLesion)
                else
                    inc(lnNoDeficitYesLesion);
        end;
	for i := lnGroupX to (lnSubj-1) do begin //for each unlesioned subject
		if lIn^[i] = 0 then
                   inc(lnYesDeficitNoLesion)
                else
                    inc(lnNoDeficitNoLesion);
	end; //for each sub
	lnYesDeficit :=lnYesDeficitYesLesion+lnYesDeficitNoLesion;
	lnNoDeficit := lnNoDeficitYesLesion+lnNoDeficitNoLesion;
	if  (lnYesDeficit<1) or (lnNoDeficit<1) then
		lOutZ := 0
	else begin
		lVal := Liebermeister(lnYesDeficitYesLesion, lnYesDeficitNoLesion, lnNoDeficitYesLesion, lnNoDeficitNoLesion);
		if lVal < 0 then
			lOutZ := -pNormalInv(abs(lVal))
		else
			lOutZ := pNormalInv(lVal)
	end; //compute chi
        lAUC := rocAUC (lnYesDeficitYesLesion,lnNoDeficitYesLesion,lnYesDeficitNoLesion,lnNoDeficitNoLesion);
        {lFalseAlarmRate := lnYesDeficitNoLesion/(lnYesDeficitNoLesion+lnNoDeficitNoLesion);
        lHitRate := lnYesDeficitYesLesion/(lnYesDeficitYesLesion+lnNoDeficitYesLesion);
        lAUC := rocAz (lHitRate,lFalseAlarmRate);
        }
        //if lOutZ > 4 then ax(lnYesDeficitYesLesion,lnNoDeficitYesLesion,lnYesDeficitNoLesion,lnNoDeficitNoLesion,lauc,lOutZ);
end;

procedure Liebermeister2 (lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lOutZ: double);
//(lnRow,lnCol: integer; var lIn,lOutZ: DoubleP0);
//Liebermeister QuasiExact - excellent power
var
	lVal: double;
	i,lnYesDeficit1,lnYesDeficit0,lnNoDeficit1,lnNoDeficit0,
	lnYesDeficit,lnNoDeficit: integer;
	//lMaxChi,lMinChi: single;
begin
	lnYesDeficit0 := 0;
	lnYesDeficit1 := 0;
	lnNoDeficit0 := 0;
	lnNoDeficit1 := 0;
        for i := 0 to (lnGroupX-1) do begin //for each subject
		if lIn^[i] = 0 then
                   inc(lnYesDeficit0)
                else
                    inc(lnNoDeficit0);
        end;
	for i := lnGroupX to (lnSubj-1) do begin //for each subject
		if lIn^[i] = 0 then
                   inc(lnYesDeficit1)
                else
                    inc(lnNoDeficit1);
	end; //for each sub
	lnYesDeficit :=lnYesDeficit0+lnYesDeficit1;
	lnNoDeficit := lnNoDeficit0+lnNoDeficit1;
	if  (lnYesDeficit<1) or (lnNoDeficit<1) then
		lOutZ := 0
	else begin
		lVal := Liebermeister(lnYesDeficit0, lnYesDeficit1, lnNoDeficit0, lnNoDeficit1);
		if lVal < 0 then
			lOutZ := -pNormalInv(abs(lVal))
		else
			lOutZ := pNormalInv(lVal)
	end; //compute chi
end;





procedure SortDouble (first, last: integer; var DynDataRA:DoubleP0; var lGroupRA: Bytep0);
{Shell sort chuck uses this- see 'Numerical Recipes in C' for similar sorts.}
{less memory intensive than recursive quicksort}
label
	 555;
const
	 tiny = 1.0e-5;
	 aln2i = 1.442695022;
var
   n, nn, m, lognb2, l, k, j, i: INTEGER;
   swap: Single;
   swapbyte: byte;
begin
	n := abs(last - first + 1);
	lognb2 := trunc(ln(n) * aln2i + tiny);
	m := last;
	for nn := 1 to lognb2 do begin
		m := m div 2;
		k := last - m;
		for j := 0 to k do begin
			i := j;
			555: {<- LABEL}
			l := i + m;
			if (DynDataRA^[l] < DynDataRA^[i]) then begin
				swap := DynDataRA^[i];
				DynDataRA^[i] := DynDataRA^[l];
				DynDataRA^[l] := swap;
				swapbyte := lGroupRA^[i];
				lGroupRA^[i] := lGroupRA^[l];
				lGroupRA^[l] := swapbyte;
				i := i - m;
				if (i >= 0) then
					goto 555;
			end
		end
	end
end;//sort

procedure RankArray (first, last: integer; var DynDataRA:DoubleP0; var lGSum: double);
var
	lnTies,lPos,lStartPos,lRankPos: integer;
	lScore,lTie : double;
begin
	lGSum := 0;
	lPos := first;
	while lPos <= last do begin
		lStartPos := lPos;
		lScore := DynDataRA^[lPos];
		while (lPos < last) and (lScore = DynDataRA^[lPos+1]) do
			inc(lPos); //count ties
		lnTies := lPos - lStartPos;
		lTie := (lnTies) *0.5;
		if lnTies > 0 then begin
			lnTies := lnTies+1;//tj on page 135 of Siegel
			lGSum := lGSum + (( (lnTies*lnTies*lnTies)  - lnTies)/12);
			//showmessage(inttostr(lnTies)+'   '+realtostr(lGSum,4));
		end;
		for lRankPos := lStartPos to lPos do
			DynDataRA^[lRankPos] := lStartPos+1+lTie;
		inc(lPos);//start with next value
	end;
end;

function k_out_n (k,n: integer): double; //total possible permutations
//k= smaller group, n=sum of both groups
var
	lVal: double;
begin

	if not gFactRAready then InitFact;
        if (k < 1) or (n <0) then begin
            result := 20000001;
            ShowMsg('error k_out_n: k and n must be positive '+inttostr(n)+':'+inttostr(k))
	end else if (n > kMaxFact) or (k > kMaxFact) then
		result := 20000001
	else begin
		lVal := gFactRA[n] / (gFactRA[k]*gFactRA[n-k]  );
		if lVal > 20000001 then
			result := 20000001
		else
			result := round(lVal);
		//result := round(gFactRA[n] / (gFactRA[k]*gFactRA[n-k]  )  );
	end;
// k out n = n!/(k!*(n-k)! which is equal to the PROD(i=k; 1){(n-i+1)/i}
end; //k_out_n
//http://www.fon.hum.uva.nl/rob/
//# samples for which the sum of the ranks in the smaller sample is smaller than or
//# equal to a given upper bound W.
//# $W = the bound, $Sum = the sum of ranks upto now, $m-1 = one less than the
//# number of elements in the smaller sample that still have to be done,
//# $Start = the current position in the ranks list, *RankList = the array
//# with all the ranks (this is NOT just the numbers from 1 - N because of ties).
//# The list with ranks MUST be sorted in INCREASING order.
function CountSmallerRanks(var W,Sum: double;  lm, Start,N: integer; var RankList: DoubleP0): integer;
var
  Temp: double;
  i, mminus1: integer;
begin
  Temp:= 0;
  result := 0;
  if(Sum > W) then
	exit;
  //Check all subsets of the remaining of RankList
  mminus1 := lm-1;
  if(mminus1 > 0) then begin
	for i := Start to (N-mminus1) do begin
	  Temp := Sum + RankList^[i];
	  if(Temp > W) then
		exit;// No smaller values expected anymore
	  result := result +CountSmallerRanks(W,Temp, mminus1, i+1, N, RankList);
	end;
  end else begin
	//If even adding the highest rank doesn't reach $W,
	//return the remaining number of items
	if( (Sum + N + 1) <= W) then begin
		result := N - Start + 1;
		exit;
	end;
	for i := Start to N do begin
	  Temp := Sum + RankList^[i];
	  if(Temp <= W) then
		inc(result)
	  else // No smaller values expected anymore
		exit;
	end; //for
  end; //m = 0
end;

procedure SortD (first, last: integer; var DynDataRA:DoubleP0);
{Shell sort chuck uses this- see 'Numerical Recipes in C' for similar sorts.}
{less memory intensive than recursive quicksort}
label
	 555;
const
	 tiny = 1.0e-5;
	 aln2i = 1.442695022;
var
   n, nn, m, lognb2, l, k, j, i: INTEGER;
   swap: Single;
begin
	n := abs(last - first + 1);
	lognb2 := trunc(ln(n) * aln2i + tiny);
	m := last;
	for nn := 1 to lognb2 do begin
		m := m div 2;
		k := last - m;
		for j := 1 to k do begin
			i := j;
			555: {<- LABEL}
			l := i + m;
			if (DynDataRA^[l] < DynDataRA^[i]) then begin
				swap := DynDataRA^[i];
				DynDataRA^[i] := DynDataRA^[l];
				DynDataRA^[l] := swap;
				i := i - m;
				if (i >= 0) then
					goto 555;
			end
		end
	end
end;//sort

function Median (var lObs: DoubleP0; lnSubj: integer): double;
begin
	SortD(0,lnSubj-1,lObs);
        if odd(lnSubj) then
           result := lObs^[lnSubj div 2]
        else
            result := 0.5* (lObs^[(lnSubj div 2)-1]+lObs^[lnSubj div 2]);
end;
(* getmem(lGroupRA,lnSubj*sizeof(Byte));
 createArray64(lObspX,lObsX,lnSubj);
 ln0 := 0;
 ln1 := 0;
	for i := 0 to (lnSubj-1) do begin //for each subject
		//lVal := lIn[i];
		lObs[i] := lIn[i];
		if i < lnGroup0 then //group0
			lGroupRA[i] := 0
		else
			lGroupRA[i] := 1;
	end; //for each sub
	for i := 0 to (lnSubj-1) do
		if lGroupRA[i] = 0 then
			inc(ln0) //number of observations in group zero
		else
			inc(ln1); //number of observations in group one
      if (ln0 > 1) and (ln1 > 1) then begin
	SortDouble(0,lnSubj-1,lObs,lGroupRA);
  *)

procedure MeanMedian(lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lMeanFX,lMedianFX: double);
//compute mean and median effect size
var
   i: integer;
   lMeanY,lMeanX,lMedianY,lMedianX: double;
   lObsp: pointer;
   lObs: Doublep0;

begin
     lMeanFX := 0;
     lMedianFX := 0;
     if (lnSubj=lnGroupX) or (lnSubj < 2) or (lnGroupX = 0) then
        exit;  //at least one empty group - no effect size
     //next compute mean/median for groupX
     lMeanX := 0;
     createArray64(lObsp,lObs,lnSubj);
     for i := 0 to (lnGroupX-1) do begin //for each subject
                lMeanX := lMeanX + lIn^[i];
                lObs[i] := lIn[i];
     end;
     lMeanX := lMeanX/lnGroupX;
     lMedianX := Median (lObs,lnGroupX);
     freemem(lObsp);
     //next compute mean/median for groupY
     lMeanY := 0;
     createArray64(lObsp,lObs,(lnSubj-lnGroupX));
     for i := lnGroupX to (lnSubj-1) do begin  //for each subject
         lMeanY := lMeany + lIn^[i];
         lObs^[i-lnGroupX] := lIn^[i];
     end;
     lMeanY := lMeanY/ (lnSubj-lnGroupX);
     lMedianY := Median (lObs,(lnSubj-lnGroupX));
     freemem(lObsp);
     //finally, compute effect sizes
     lMeanFX := lMeanX-lMeanY;
     lMedianFX := lMedianX-lMedianY;
end;

procedure PairedTStat (lnSubj: integer; var lIn: DoubleP0; var lOutT: double);
//lIn has data for controls 1...n followed by 1..n paired measures.
//e.g. if three observations, 1x,2x,3x,1c,2c,3c
var
	i,lnObs: integer;
	lSqrSumDif,lSumDif,lSumDifSqr,lDF,lDif,lmeanDif,lVar: double;
begin
        lOutT := 0;
        if (odd(lnSubj)) or (lnSubj < 4) then
           exit; //must have even number
        lnObs := lnSubj shr 1;
	lSumDif := 0;
	lSumDifSqr := 0;
	for i := 0 to (lnObs-1) do begin //for each subject
                lDif := lIn^[i]-(lIn^[lnObs+i]) ;
                lSumDif := lSumDif + lDif;
                lSumDifSqr := lSumDifSqr + sqr(lDif);
        end;
	lDF := lnObs - 1;

        if (lSumDifSqr <> 0)and (lSumDif <> 0){and (lDF <> 0) and (lnObs <> 0)} then begin
           lmeanDif := lSumDif / lnObs;
           lSqrSumDif := sqr(lSumDif);
           lVar := lSumDifSqr - (lSqrSumDif / lnObs);
           lVar := lVar / (lnObs * lDF);
           lVar := sqrt(lVar);
           if lVar <> 0 then
              lOutT := lmeanDif / lVar;
        end;

end;

(*procedure ReportError (lnSubj, lnGroupX: integer; var lIn: DoubleP0; lS: double);
var
  myFile : TextFile;
  text : string;
  i: integer;
begin
  AssignFile(myFile, 'c:\Test666.txt');
  ReWrite(myFile);
  WriteLn(myFile,'Subj = '+INTTOSTR(lnSubj));
  WriteLn(myFile,'Group1 = '+INTTOSTR(lnGroupX));
  WriteLn(myFile,'Var = '+FLOATTOSTR(lS));
  for i := 0 to (lnSubj-1) do
    WriteLn(myFile,floattostr(lIn^[i]));
  CloseFile(myFile);
end;*)

procedure TStat2 (lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lOutT: double);
//pooled variance t-test http://www.okstate.edu/ag/agedcm4h/academic/aged5980a/5980/newpage26.htm
const
	 tiny = 1.0e-5;
var
	i,lnGroupY: integer;
	lSumX,lSumY,lSumSqrx,lSumSqry,lVarx,lVary,lS: double;
begin
        lnGroupY := lnSubj - lnGroupX;
        lOutT := 0;
        if (lnGroupX < 1) or (lnGroupY < 1) or (lnSubj < 3) then   //need at least 1 subj in each group
            exit;
	lSumx := 0;
	lSumSqrX := 0;
	for i := 0 to (lnGroupX-1) do begin //for each subject
		//lVal := lIn[i];
                lsumx := lsumx + lIn^[i];
                lSumSqrX := lSumSqrX + sqr(lIn^[i]);
        end;
	lVarx := (lnGroupX*lSumSqrX) - Sqr(lsumx);
	if lnGroupX > 1 then
		lVarX := lVarX / (lnGroupX*(lnGroupX-1))
	else
		lVarx := 0;
	lSumy := 0;
	lSumSqry := 0;
	for i := lnGroupX to (lnSubj-1) do begin //for each subject
                lsumy := lsumy + lIn^[i];
                lSumSqry := lSumSqry + sqr(lIn^[i]);
	end; //for each sub
        //lMnY := lsumy/lnGroupY;
	lVary := (lnGroupY*lSumSqrY) - Sqr(lsumy);
	if lnGroupY > 1 then
           lVary := lVary / (lnGroupY*(lnGroupY-1))
        else
            lVary := 0;
	//lm := (lsumx/lnGroupX)-(lsumy/lnGroupY); //mean effect size lmnx - lmny;
	//ldf := lnSubj - 2;
  ls := (  ((lnGroupX - 1) * lvarx + (lnGroupY - 1) * lvary) / (lnSubj - 2){ldf}) ;
  if abs(ls) < tiny then
    exit;
  if ls < 0 then
    ShowMsg('Error: t-test variance should not be zero.');
    //deepshit (lnSubj, lnGroupX, lIn,lS);
  //if ls <= 0 then
  //  exit;       xxx
	ls := sqrt( ls) ;
  ls := ls * sqrt(1 / lnGroupX + 1 / lnGroupY); //note - to get here both lnx and lny > 0
	if ls = 0 then
		lOutT := 0
	else
		lOutT := ( ((lsumx/lnGroupX)-(lsumy/lnGroupY))/ls);//t = lm / ls;
end;              

(*procedure TStatAbs (lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lOutT: double);
var
	i,lnGroupY: integer;
	lSumX,lSumY,lSumSqrx,lSumSqry,lVarx,lVary,lS: double;
begin
        lnGroupY := lnSubj - lnGroupX;
        if (lnGroupX < 1) or (lnGroupY < 1) then begin  //need at least 1 subj in each group
            lOutT := 0;
            exit;
        end;
	lSumx := 0;
	lSumSqrX := 0;
	for i := 0 to (lnGroupX-1) do begin //for each subject
                lsumx := lsumx + lIn[i];
                lSumSqrX := lSumSqrX + sqr(lIn[i]);
        end;
	lVarx := (lnGroupX*lSumSqrX) - Sqr(lsumx);
	if lnGroupX > 1 then
		lVarX := lVarX / (lnGroupX*(lnGroupX-1))
	else
		lVarx := 0;
	lSumy := 0;
	lSumSqry := 0;
	for i := lnGroupX to (lnSubj-1) do begin //for each subject
		//lVal := lIn[i];
                lsumy := lsumy + lIn[i];
                lSumSqry := lSumSqry + sqr(lIn[i]);
	end; //for each sub
	lVary := (lnGroupY*lSumSqrY) - Sqr(lsumy);
	if lnGroupY > 1 then
           lVary := lVary / (lnGroupY*(lnGroupY-1))
        else
            lVary := 0;
	ls := sqrt( (  ((lnGroupX - 1) * lvarx + (lnGroupY - 1) * lvary) / (lnSubj - 2)) ) ;
        ls := ls * sqrt(1 / lnGroupX + 1 / lnGroupY); //note - to get here both lnx and lny > 0
	if ls = 0 then
		lOutT := 0
	else
		lOutT := ( ((lsumx/lnGroupX)-(lsumy/lnGroupY))/ls);//t = lm / ls;
        //next - create direction map
        if (abs(lOutT) >= 1.96) then begin
           if abs (lsumx/lnGroupX) > abs(lsumy/lnGroupY) then
              lOutT := 4
           else
               lOutT := -4

        end else
            lOutT := 0;
end;*)

procedure TStat2Z (lnSubj, lnGroupX: integer; var lIn: DoubleP0; var lOutT: double);
var
	i,lnGroupY: integer;
	lSumX,lSumY,lSumSqrx,lSumSqry,lVarx,lVary,lS: double;
begin
        lnGroupY := lnSubj - lnGroupX;
        if (lnGroupX < 1) or (lnGroupY < 1) then begin  //need at least 1 subj in each group
            lOutT := 0;
            exit;
        end;
	lSumx := 0;
	lSumSqrX := 0;
	for i := 0 to (lnGroupX-1) do begin //for each subject
		//lVal := lIn[i];
                lsumx := lsumx + lIn^[i];
                lSumSqrX := lSumSqrX + sqr(lIn^[i]);
        end;
	lVarx := (lnGroupX*lSumSqrX) - Sqr(lsumx);
	if lnGroupX > 1 then
		lVarX := lVarX / (lnGroupX*(lnGroupX-1))
	else
		lVarx := 0;
	lSumy := 0;
	lSumSqry := 0;
	for i := lnGroupX to (lnSubj-1) do begin //for each subject
		//lVal := lIn[i];
                lsumy := lsumy + lIn^[i];
                lSumSqry := lSumSqry + sqr(lIn^[i]);
	end; //for each sub
        //lMnY := lsumy/lnGroupY;
	lVary := (lnGroupY*lSumSqrY) - Sqr(lsumy);
	if lnGroupY > 1 then
           lVary := lVary / (lnGroupY*(lnGroupY-1))
        else
            lVary := 0;
	//lm := (lsumx/lnGroupX)-(lsumy/lnGroupY); //mean effect size lmnx - lmny;
	//ldf := lnSubj - 2;
	ls := sqrt( (  ((lnGroupX - 1) * lvarx + (lnGroupY - 1) * lvary) / (lnSubj - 2){ldf}) ) ;
        ls := ls * sqrt(1 / lnGroupX + 1 / lnGroupY); //note - to get here both lnx and lny > 0
	if ls = 0 then
		lOutT := 0
	else begin
             lOutT := ( ((lsumx/lnGroupX)-(lsumy/lnGroupY))/ls);//t = lm / ls;
            lOutT := TtoZ (lOutT,lnSubj-2);
            //fx((lsumx/lnGroupX),(lsumy/lnGroupY));
        end;
end;



procedure TStatWelch (lnSubj, lnGroup0: integer; var lIn: DoubleP0; var lOutT: double);
//see R. D. DeVeaux 'The t -test: Some details' for details
//uses Welch's Test to protect against unequal variances
//uses true [often fractional] Degrees of Freedom
label
	129;
var
	i,lNx,lNy: integer;
	lVal,lSumX,lSumY,lSumSqrx,lSumSqry,lVarx,lVary,lMnX,lMnY,lM,lDF,lDenom,lZ,lT: double;
begin
	lZ := 0;
	lNx := 0;
	lSumx := 0;
	lSumSqrX := 0;
	lNy := 0;
	lSumy := 0;
	lSumSqry := 0;
	for i := 0 to (lnSubj-1) do begin //for each subject
		lVal := lIn^[i];
		if i < lnGroup0  then begin //group0
			inc(lNx);
			lsumx := lsumx + lVal;
			lSumSqrX := lSumSqrX + sqr(lVal);
		end else begin //else group1
			inc(lNy);
			lsumy := lsumy + lVal;
			lSumSqry := lSumSqry + sqr(lVal);
		end;//group1
	end; //for each sub
	if (lNy < 2) or (lNx < 2) then
		goto 129;  //unable to calculate
	lVarX := (lNx*lSumSqrX) - Sqr(lSumX);
	lVarX := lVarX / (lNx*(lNx-1));
	lMnX := lSumX/lNx;
	lVary := (lNy*lSumSqrY) - Sqr(lsumy);
	lVary := lVary / (lNy*(lNy-1));
	lMnY := lSumY/lNy;
	lm := lMnX - lMnY; //difference between means = t-Numerator
	if (lm = 0) {or (lVarY=0) or (lVarX = 0)} then
		goto 129; //no difference in proportions - do not waste time computing DF
	//next compute true Degrees of Freedom
	lDF := sqr( (lVarX/lNx)+(lVarY/lNy));
	//lDF := lDF /( ((Sqr(lVarX/lNx)) / (lnx-1) ) + ((Sqr(lVarY/lNy)) / (lny-1) )     );
	if (lVarX=0) or (lVarY=0) then begin //forced to estimate based on pooled variance
                lDF := lnx+lny -2;
		lDenom:= (  ((lnx - 1) * lvarx + (lny - 1) * lvary) / (lNx+lNy-2));
		lDenom :=  sqrt(lDenom / lnx + lDenom / lny);
	end else begin
                lDF := lDF /( ((Sqr(lVarX/lNx)) / (lnx-1) ) + ((Sqr(lVarY/lNy)) / (lny-1) )     );
		lDenom := sqrt(lVarX/lNx + lVary/lNy);//assume Unequal variances "Welch's Test"
        end;
	if lDenom = 0 then
		goto 129;
	lT := ( lm/lDenom);//t = m / d;
	lZ := TtoZ(lT,lDF);       //az
	//lP :=  pNormal(TtoZ(lT,lDF));
 129:
	 lOutT := lZ;
	//vlsm compatible =	lOutT[lColX] := ( lm/lD);//t = m / d;
end;

FUNCTION specialdouble (d:double): boolean;
//returns true if s is Infinity, NAN or Indeterminate
//8byte IEEE: msb[63] = signbit, bits[52-62] exponent, bits[0..51] mantissa
//exponent of all 1s =   Infinity, NAN or Indeterminate
CONST kSpecialExponent = 2047 shl 20;
VAR Overlay: ARRAY[1..2] OF LongInt ABSOLUTE d;
BEGIN
     IF ((Overlay[2] AND kSpecialExponent) = kSpecialExponent) THEN
        RESULT := true
     ELSE
        RESULT := false;
END;

procedure LocalRank (first, last: integer; var DynDataRA,DynDataRAX:DoubleP0; var lGroupRA: Bytep0);
var
	lGroup,lnTies,lPos,lStartPos,lRankPos,lLocalRank: integer;
	lScore,lTie : double;
begin
        for lGroup := 0 to 1 do begin
	  lPos := first;
          lLocalRank := 0;
	  while lPos <= last do begin
            if lGroupRA^[lPos] = lGroup then begin//
                inc(lLocalRank);
		lStartPos := lPos;
		lScore := DynDataRA^[lPos];
                lnTies := 0;
		while (lPos < last) and (0.001 > abs (lScore - DynDataRA^[lPos+1]) ) do begin
			inc(lPos); //count ties
                        if lGroupRA^[lPos] = lGroup then
                           inc(lnTies);
		end;
		lTie := (lnTies) *0.5;
		for lRankPos := lStartPos to lPos do begin
                    if lGroupRA^[lRankPos] = lGroup then
			DynDataRAX^[lRankPos] := (lLocalRank+lTie);
                end;
                lLocalRank := lLocalRank + lnTies;
            end; //if in group
            inc(lPos);//start with next value
	 end; //while... for each observation
        end; //for each group
end;

procedure BMTest (lnSubj, lnGroup0: integer; var lIn: DoubleP0; var lOutT: double);
//procedure BMtest (lnRow,lnCol: integer; var lIn,lOutT: DoubleP0);
var
	lObspX,lObsp: pointer;
	lObsX,lObs: Doublep0;
	lGroupRA: Bytep0;
	i,ln0,ln1,lColX: integer;
	lDF,lZ,lGSum: double;
        lSum0,lSum1,lMean0,lMean1,lSqr0,lSqr1,lk0,lk1: double;
begin
 createArray64(lObsp,lObs,lnSubj);
 getmem(lGroupRA,lnSubj*sizeof(Byte));
 createArray64(lObspX,lObsX,lnSubj);
 ln0 := 0;
 ln1 := 0;
	for i := 0 to (lnSubj-1) do begin //for each subject
		//lVal := lIn[i];
		lObs^[i] := lIn^[i];
		if i < lnGroup0 then //group0
			lGroupRA^[i] := 0
		else
			lGroupRA^[i] := 1;
	end; //for each sub
	for i := 0 to (lnSubj-1) do
		if lGroupRA^[i] = 0 then
			inc(ln0) //number of observations in group zero
		else
			inc(ln1); //number of observations in group one
      if (ln0 > 1) and (ln1 > 1) then begin
	SortDouble(0,lnSubj-1,lObs,lGroupRA);
	RankArray(0,lnSubj-1,lObs,lGSum);
        lSum0 := 0;
        lSum1 := 0;
	for i := 0 to (lnSubj-1) do
          if lGroupRA^[i] = 0 then
             lSum0 := lSum0 + lObs^[i]
          else
             lSum1 := lSum1 + lObs^[i];
        lMean0 := lSum0 / ln0;
        lMean1 := lSum1 / ln1;
        //fx(lmean0,lMean1);
        lSqr0 := 0;
        lSqr1 := 1;
        lk0 := (ln0+1)/2;
        lk1 := (ln1+1)/2;
        LocalRank(0,lnSubj-1,lObs,lObsX,lGroupRA);
	for i := 0 to (lnSubj-1) do
          if lGroupRA^[i] = 0 then
             lSqr0 := lSqr0 + Sqr(lObs^[i]-lObsX^[i]-lMean0+lk0)
          else
             lSqr1 := lSqr1 + Sqr(lObs^[i]-lObsX^[i]-lMean1+lk1);
        lSqr0 := (1/(ln0-1))*lSqr0;
        lSqr1 := (1/(ln1-1))*lSqr1;

        lZ := -(ln0*ln1*(lMean1-lMean0))/((ln0+ln1)*sqrt((ln0*lSqr0)+(ln1*lSqr1)   ) );
        lDF := sqr(ln0*lSqr0+ln1*lSqr1) / ( (sqr(ln0*lSqr0)/(ln0-1)) + (sqr(ln1*lSqr1)/(ln1-1))  )   ;
        lZ := TtoZ(lZ,lDF);       //az
        lOutT := lZ;
        //fx(lZ,lDF);
      end else //>1
          lOutT := 0;
 freemem(lObsp);
 freemem(lObspX);
 freemem(lGroupRA);
end; //bmtest


procedure WilcoxonMW2 (lnSubj, lnGroup0: integer; var lIn: DoubleP0; var lOutT: double);
var
	lObsp: pointer;
	lObs: Doublep0;
	lGroupRA: Bytep0;
	m,n,i,ln0,ln1,mplusn: integer;
	lPermutations,lVal,lWsmalln,lZ,lZi,lTail,lGSum,lWTotal,lH0,lSum: double;

begin
 createArray64(lObsp,lObs,lnSubj);
 getmem(lGroupRA,lnSubj*sizeof(Byte));
	ln0 := 0;
	ln1 := 0;
	for i := 0 to (lnSubj-1) do begin //for each subject
		//lVal := lIn[i];
		lObs[i] := lIn[i];
		if i < lnGroup0 then //group0
			lGroupRA^[i] := 0
		else
			lGroupRA^[i] := 1;
	end; //for each sub
	for i := 0 to (lnSubj-1) do
		if lGroupRA^[i] = 0 then
			inc(ln0) //number of observations in group zero
		else
			inc(ln1); //number of observations in group one
	SortDouble(0,lnSubj-1,lObs,lGroupRA);
	RankArray(0,lnSubj-1,lObs,lGSum);

	lWsmalln := 0;
	if ln1 < ln0 then begin //Group1 smaller than Group0
		m := ln1;
		n := ln0;
		for i := 0 to (lnSubj-1) do
			if lGroupRA^[i] = 1 then
				lWsmalln := lWsmalln + lObs^[i];
	end else begin//Group0 smaller than Group1
		m := ln0;
		n := ln1;
		for i := 0 to (lnSubj-1) do
			if lGroupRA^[i] = 0 then
				lWsmalln := lWsmalln + lObs^[i];
	end;
        mplusn := m + n;
        lZ := 0;
	if lWsmalln > (mplusn*(mplusn+1)/4) then
		lTail := -0.5
	else
		lTail := 0.5;
	if m < 1 then
		lZ := 0
	else if lGSum = 0 then begin //no ties
		lZ := ( lWsmalln + lTail - m * ( m + n + 1 ) / 2 ) / sqrt( m * n * ( m + n + 1 ) / 12 );
	end else begin //correct for ties, see Siegel page 135
		if ((12-lGSum)<>0) and (((lnSubj*(lnSubj-1)) * (((lnSubj*lnSubj*lnSubj) -lnSubj)  /12-lGSum))<> 0) then begin
			lZ :=  lWsmalln + lTail - (m * ( lnSubj + 1 ) / 2 );
			lZ := lZ/sqrt (  (m*n)/ (lnSubj*(lnSubj-1)) * (((lnSubj*lnSubj*lnSubj) -lnSubj)  /12-lGSum));
		end else begin
                    lZ := ( lWsmalln + lTail - m * ( m + n + 1 ) / 2 ) / sqrt( m * n * ( m + n + 1 ) / 12 );
                end;
	end;
           {if lStr = '' then begin
		for i := 0 to (lnSubj-1) do
			lStr := lStr+inttostr(lGroupRA[i])+', '+floattostr( lObs[i])+';';
                lStr := ('w'+floattostr(lWsmalln)+'Z'+floattostr(lZ)+'ties'+floattostr(lgSum)+'m'+inttostr(m)+'n'+inttostr(n)+':'+lStr);
           end; }
           if m < 10 then
              lPermutations := k_out_n(m,mplusn);
           if  (m < 10) and  (lPermutations < 20000000) and (abs(lZ) > 1) {}then begin
		lWTotal :=mplusn*(mplusn+1)/2; //sum ranks for both groups m and n
		lH0 := lWTotal * (m/mplusn); //null hypothesis
		lSum := 0;
		//next - use smallest value of W
		if lWSmallN > lH0 then begin
			lWSmallN := lH0 - (lWSmallN-lH0);
			//Due to ties, we need to flip the order as well, as we are searching smaller
			for i := 0 to (lnSubj-1) do
				lObs^[i] :=  (lnSubj+1)-lObs^[i];
			for i := 0 to ((lnSubj-2) div 2) do begin //swap
				lVal := lObs^[i];
				lObs^[i] := lObs^[lnSubj-1-i];
				lObs^[lnSubj-1-i] := lVal;
			end;
		end;
		lVal := CountSmallerRanks(lWSmallN, lSum,   m, 0,(mplusn-1), lObs);
                lZi := lZ;
		lZ :=pNormalInvQuickApprox(lVal/lPermutations);
                if ((lZ > 0) and (lZi < -1)) or ((lZ < 0) and (lZi > 1)) then
                   lZ := -lZ;
	end;
	if ln1 < ln0 then //we computed unexpected tail
		lOutT := -lZ
	else
		lOutT := lZ;//t = m / d;
 freemem(lObsp);
 freemem(lGroupRA);
end;



end.
 