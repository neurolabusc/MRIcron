unit brunner;
{$I options.inc} // {$IFDEF OLDSTATS}
{$IFDEF FPC} {$mode Delphi}{$ENDIF}
interface
uses define_types,math,Distr;
{$IFDEF OLDSTATS}
procedure tBM (lnSubj, lnGroup0: integer; var lIn: DoubleP0; var ltBM,lDF: double);
{$ENDIF}
procedure genBMsim (lnSubj: integer; var lOrigOrder: DoubleP0);
function BMzVal(lnSubj, lnGroup0: integer; ltBM,lDF: double): double;
function continROC (lnSubj, lnGroup0: integer; var lIn: DoubleP0): single;
function continROC2 (lnSubj: integer; var lInIV, lInDV: DoubleP0): single;
function continROC3 (lnSubj: integer; var lGroup: Bytep; lIn: Singlep): single;
procedure tBM3 (lnSubj: integer; var lGroup: Bytep; lInX: Singlep; var ltBM,lDF: double; out ln0: integer);

const
      {$ifdef CPU32}
     knPermute= 20000;
     {$ELSE}
     knPermute= 40000;
     {$ENDIF}
     knSim = 15;
var
   gSimRA: array [1..knSim] of DoubleP;
   gSimRAp: array [1..knSim] of pointer;
implementation


{$IFDEF NEWRNG}
uses rng;

//rng: TRNG = (test: FALSE);

procedure GenPermute (lnSubj: integer; var lOrigOrder,lRanOrder: DoubleP0; var rng: TRNG);
var
   lInc,lRand: integer;
   lSwap: double;
begin
     //next lines commented out - this check should be done before inner loop
     //if lnSubj < 2 then //can not randomize order of single value
     //   exit;
     //Move(src,dest,count);
      Move(lOrigOrder^,lRanOrder^,lnSubj*sizeof(double));
     //for lInc := 1 to lnSubj do
     //    lRanOrder[lInc-1] := lOrigOrder[lInc-1];
     for lInc := lnSubj downto 2 do begin
         //lRand := Random(lInc);
         lRand := RandomInt0(rng, lInc-1);
         lSwap := lRanOrder^[lRand];
         lRanOrder^[lRand] := lRanOrder^[lInc-1];
         lRanOrder^[lInc-1] := lSwap;
     end;
end;
{$ELSE}
procedure GenPermute (lnSubj: integer; var lOrigOrder,lRanOrder: DoubleP0);
var
   lInc,lRand: integer;
   lSwap: double;
begin
     //next lines commented out - this check should be done before inner loop
     //if lnSubj < 2 then //can not randomize order of single value
     //   exit;
     //Move(src,dest,count);
      Move(lOrigOrder^,lRanOrder^,lnSubj*sizeof(double));
     //for lInc := 1 to lnSubj do
     //    lRanOrder[lInc-1] := lOrigOrder[lInc-1];
     for lInc := lnSubj downto 2 do begin
         lRand := Random(lInc);
         lSwap := lRanOrder^[lRand];
         lRanOrder^[lRand] := lRanOrder^[lInc-1];
         lRanOrder^[lInc-1] := lSwap;
     end;
end;
{$ENDIF}

function BMzVal(lnSubj,lnGroup0 : integer; ltBM,lDF: double): double;
//can be approximated by      result := TtoZ(ltBM,lDF);
var
   lnSmallGroup,lJump,lEstimate,i,tie: integer;
   ltBMs : double;
   lSwap: boolean;
begin
     //result := TtoZ(ltBM,lDF); exit;
     lSwap := false;
     ltBMs := ltBM;
     result := 0;
     tie := 0;
     if (lnSubj div 2) > lnGroup0 then
        lnSmallGroup := lnGroup0
     else
         lnSmallGroup := lnSubj-lnGroup0;
     if lnSmallGroup < 1 then exit;
     if lnSmallGroup > knSim then begin
         result := TtoZ(ltBMs,lDF);
         exit;
     end;
     if (lnSubj div 2) < lnGroup0 then begin
         ltBMs := -ltBMs;
         lSwap := not lSwap; //distributions are not symetrical
     end;
     lEstimate := knPermute div 2; //start half way through data
     lJump := lEstimate div 2;
     for i := 1 to 9 do begin
         if gSimRA[lnSmallGroup]^[lEstimate] > ltBMs then
            lEstimate := lEstimate - lJump
         else
            lEstimate := lEstimate + lJump;
         lJump := (lJump+1) div 2;
     end;
     if lEstimate < (knPermute div 2) then begin //p < 0.5 count up for less extreme
        i := lEstimate-lJump-lJump;
        if i < 1 then
           i := 1;
        while ltBMs > gSimRA[lnSmallGroup]^[i] do begin
              inc(i);
        end;
        if ltBMs = gSimRA[lnSmallGroup]^[i] then begin
           while ltBMs = gSimRA[lnSmallGroup]^[i] do begin
              inc(i);
              dec(tie);
           end;
           dec(tie);
        end;
     end else begin //p < 0.5 count down for less extreme
        i := lEstimate+lJump+lJump;
        if i >= knPermute then
           i := knPermute;
        while ltBMs < gSimRA[lnSmallGroup]^[i] do
              dec(i);
        if ltBMs = gSimRA[lnSmallGroup]^[i] then begin
           while ltBMs = gSimRA[lnSmallGroup]^[i] do begin
              dec(i);
              inc(tie);
           end;
           inc(tie);
        end;
         i := i - 1; //indexed from 1 not 0
     end;
     //result :=  (i+(tie/2));
     //result := (1-( (i+(tie/2))/knPermute));
     result := pNormalInv(1-( (i+(tie/2))/knPermute));
     if lSwap then
        result := -result;
end;

procedure Sort (lo, up: integer; var r:DoubleP);
//62ms Shell Sort http://www.dcc.uchile.cl/~rbaeza/handbook/algs/4/414.sort.p.html
label     999;
var
   d, i, j : integer;
   tempr : single;
begin
     d := up-lo+1;
     while d>1 do begin
          if d<5 then
             d := 1
          else
              d := trunc( 0.45454*d ); // Do linear insertion sort in steps size d
          for i:=up-d downto lo do begin
               tempr := r^[i];
               j := i+d;
               while j <= up do
                    if tempr > r^[j] then begin
                         r^[j-d] := r^[j];
                         j := j+d
                    end else
                        goto 999;  {*** break ***}
                    999:
                    r^[j-d] := tempr
          end
     end
end;  //sort




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

(*procedure tBM (lnSubj, lnGroup0: integer; var lIn: DoubleP0; var ltBM,lDF: double);
//this is a t-test - only use to test BM!!!
var
	i,lnGroupY,lnGroupX: integer;
	lSumX,lSumY,lSumSqrx,lSumSqry,lVarx,lVary,lS: double;
begin
        lnGroupX := lnGroup0;
        lnGroupY := lnSubj - lnGroupX;
        lDF := lnSubj -1;
        if (lnGroupX < 1) or (lnGroupY < 1) then begin  //need at least 1 subj in each group
            ltBM := 0;
            exit;
        end;
	lSumx := 0;
	lSumSqrX := 0;
	for i := 0 to (lnGroupX-1) do begin //for each subject
		//lVal := lIn[i];
                lsumx := lsumx + lIn[i];
                lSumSqrX := lSumSqrX + sqr(lIn[i]);
        end;
        //lMnX := lsumx/lnGroupX;
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
		ltBM := 0
	else
		ltBM := ( ((lsumx/lnGroupX)-(lsumy/lnGroupY))/ls);//t = lm / ls;
end; *)

procedure tBM (lnSubj, lnGroup0: integer; var lIn: DoubleP0; var ltBM,lDF: double);
var
	lObspX,lObsp: pointer;
	lObsX,lObs: Doublep0;
	lGroupRA: Bytep0;
	i,ln0,ln1: integer;
	lZ,lGSum: double;
        lSum0,lSum1,lMean0,lMean1,lSqr0,lSqr1,lk0,lk1: double;
begin
 createArray64(lObsp,lObs,lnSubj);
 getmem(lGroupRA,lnSubj*sizeof(Byte));
 createArray64(lObspX,lObsX,lnSubj);
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
        //lZ := TtoZ(lZ,lDF);
        ltBM := lZ;
        //fx(lZ,lDF);
      end else //>1
          ltBM := 0;
 freemem(lObsp);
 freemem(lObspX);
 freemem(lGroupRA);
end; //tBM    (**)

procedure SortDoubleP0 (first, last: integer; var DynDataRA:DoubleP0);
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
   //swapbyte: byte;
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
				i := i - m;
				if (i >= 0) then
					goto 555;
			end
		end
	end
end;//sort


function continROC (lnSubj, lnGroup0: integer; var lIn: DoubleP0): single;
//see equation 1 of Obuchiwski, Statistics in Medicine, 25: 481-493
var
   lSum,lV: double;
   linc0,linc1,lnGroup1,i: integer;
	lObsp0,lObsp1: pointer;
	lObs0,lObs1: Doublep0;

begin
 result := -1;
 lnGroup1 := lnSubj - lnGroup0;
 if (lnGroup1 < 1) or (lnGroup0 < 1) then exit;
 createArray64(lObsp1,lObs1,lnSubj);
 createArray64(lObsp0,lObs0,lnSubj);
 for i := 0 to (lnGroup0-1) do //for each subject without disease
     lObs0[i] := lIn[i];
 SortDoubleP0(0,lnGroup0-1,lObs0);

 for i := lnGroup0 to (lnSubj-1) do  //for each subject with disease
     lObs1[i-lnGroup0] := lIn[i];
 SortDoubleP0(0,lnGroup1-1,lObs1);
 lSum := 0;
 for linc0 := 0 to (lnGroup0-1) do begin
     for linc1 := 0 to (lnGroup1-1) do begin
         if (lObs0^[linc0]) > (lObs1^[linc1]) then
            lV := 1
         else if (lObs0^[linc0]) = (lObs1^[linc1]) then //tie
              lV := 0.5
         else
             lV := 0;

         lSum := lV + lSum;
     end;//for group1
 end;//for group0
 lSum := lSum * (1/ (lnGroup0*lnGroup1 ) );
 result := lSum;

 freemem(lObsp1);
 freemem(lObsp0);
end; //continROC

procedure SortDoubleDouble (first, last: integer; var DynDataRA, lGroupRA: DoubleP0);
{Shell sort chuck uses this- see 'Numerical Recipes in C' for similar sorts.}
{less memory intensive than recursive quicksort}
label
	 555;
const
	 tiny = 1.0e-5;
	 aln2i = 1.442695022;
var
   n, nn, m, lognb2, l, k, j, i: INTEGER;
   swap,swapbyte: double;
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


function continROC2 (lnSubj: integer; var lInIV, lInDV: DoubleP0): single;
//see equation 9 of Obuchiwski, Statistics in Medicine, 25: 481-493
var
   lSum,lV: double;
   linci,lincj,i: integer;
	lObspIV,lObspDV: pointer;
	lObsIV,lObsDV: Doublep0;

begin
 result := -1;
 if (lnSubj < 1) then exit;
 createArray64(lObspIV,lObsIV,lnSubj);
 createArray64(lObspDV,lObsDV,lnSubj);
 for i := 0 to (lnSubj-1) do //for each subject without disease
     lObsIV[i] := lInIV[i];
 for i := 0 to (lnSubj-1) do //for each subject without disease
     lObsDV[i] := lInDV[i];
 SortDoubleDouble(0,lnSubj-1,lObsIV,lObsDV);

 lSum := 0;
 for linci := 0 to (lnSubj-1) do begin
     for lincj := 0 to (lnSubj-1) do begin
         if lincj <> linci then begin
            if ((lObsDV^[linci] > lObsDV^[lincj]) and (lObsIV^[linci] > lObsIV^[lincj])) or
               ((lObsDV^[linci] < lObsDV^[lincj]) and (lObsIV^[linci] < lObsIV^[lincj])) then
               lV := 1
            else if (lObsDV^[linci] = lObsDV^[lincj]) or (lObsIV^[linci] = lObsIV^[lincj]) then //tie
              lV := 0.5
            else
             lV := 0;
            lSum := lV + lSum;
         end;

     end;//for group1
 end;//for group0
 lSum := lSum * (1/ (lnSubj* (lnSubj-1) ) );
 result := lSum;
 freemem(lObspDV);
 freemem(lObspIV);
end; //continROC2

function continROC3 (lnSubj: integer; var lGroup: Bytep; lIn: Singlep): single;
//see equation 9 of Obuchiwski, Statistics in Medicine, 25: 481-493
label
     666;
var
   lnGroup0 : integer = 0;
   lnGroup1 : integer = 0;
   lSum,lV: double;
   linc0,linc1,i: integer;
	lObsp0,lObsp1: pointer;
	lObs0,lObs1: Doublep0;

begin
 result := -1;
 createArray64(lObsp1,lObs1,lnSubj);
 createArray64(lObsp0,lObs0,lnSubj);
 for i := 1 to (lnSubj) do begin//for each subject without disease
     if lGroup^[i] = 0 then begin
        lObs0^[lnGroup0] := lIn^[i];
        inc(lnGroup0);
     end else begin
       lObs1^[lnGroup1] := lIn^[i];
       inc(lnGroup1);
     end;
 end;
 if (lnGroup1 < 1) or (lnGroup0 < 1) then goto 666;
 SortDoubleP0(0,lnGroup0-1,lObs0);
 SortDoubleP0(0,lnGroup1-1,lObs1);
 lSum := 0;
 for linc0 := 0 to (lnGroup0-1) do begin
     for linc1 := 0 to (lnGroup1-1) do begin
         if (lObs0^[linc0]) > (lObs1^[linc1]) then
            lV := 1
         else if (lObs0^[linc0]) = (lObs1^[linc1]) then //tie
              lV := 0.5
         else
             lV := 0;

         lSum := lV + lSum;
     end;//for group1
 end;//for group0
 lSum := lSum * (1/ (lnGroup0*lnGroup1 ) );
 result := lSum;
666:
 freemem(lObsp1);
 freemem(lObsp0);
end; //continROC3


//function TStat3 (lnSubj: integer; var lGroup: Bytep; var lIn: Singlep): double;
procedure tBM3 (lnSubj: integer; var lGroup: Bytep; lInX: Singlep; var ltBM,lDF: double; out ln0: integer);
var
	lObspX,lObsp: pointer;
	lObsX,lObs: Doublep0;
	lGroupRA: Bytep0;
	i,ln1: integer;
	lZ,lGSum: double;
        lSum0,lSum1,lMean0,lMean1,lSqr0,lSqr1,lk0,lk1: double;
begin
 createArray64(lObsp,lObs,lnSubj);
 getmem(lGroupRA,lnSubj*sizeof(Byte));
 createArray64(lObspX,lObsX,lnSubj);
 ln0 := 0;
 ln1 := 0;
	for i := 0 to (lnSubj-1) do begin //for each subject
		lObs^[i] := lInX^[i+1];
		if lGroup^[i+1] = 0 then //group0
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
        //lZ := TtoZ(lZ,lDF);
        ltBM := lZ;
        //fx(lZ,lDF);
      end else //>1
          ltBM := 0;
 freemem(lObsp);
 freemem(lObspX);
 freemem(lGroupRA);
end; //tBM3


procedure genBMsim (lnSubj: integer; var lOrigOrder: DoubleP0);
//1.) creates kSim random permutations of the data
//2.) sorts permutations
var
   lRanOrderp: pointer;
   lRanOrder: DoubleP0;
   lInc,lnSmallGroup: integer;
   lOutT,lDF: double;
   {$IFDEF NEWRNG}
   rng: TRNG = (test: FALSE);
   {$ENDIF}
begin
     if (lnSubj < 1) or (knPermute < 1) then
        exit;
     createArray64(lRanOrderp,lRanOrder,lnSubj);
     //lnSmallGroup := lnGroup0;
     //if lnSmallGroup > knSim then exit;
     RandSeed := 128; //same order for multiple sessions
     for lnSmallGroup := 1 to knSim do begin
       //RandSeed := 128; //same order for all voxels
       for lInc := 1 to knPermute do begin
         {$IFDEF NEWRNG}
         GenPermute(lnSubj, lOrigOrder,lRanOrder, rng); //generate random order of participants
         {$ELSE}
         GenPermute(lnSubj, lOrigOrder,lRanOrder); //generate random order of participants
         {$ENDIF}
         tBM (lnSubj, lnSmallGroup, lRanOrder,lOutT,lDF);
         gSimRA[lnSmallGroup]^[lInc] := lOutT;
       end;
       //next sort permutes...
       Sort(1,knPermute,gSimRA[lnSmallGroup]);
     end;
     freemem(lRanOrderp);
end;

var
   i: integer;
initialization
begin
   for i := 1 to knSim do
    createArray64(gSimRAp[i],gSimRA[i],knPermute);
end;

finalization
begin
   for i := 1 to knSim do
       freemem(gSimRAp[i]);
end;
end.
