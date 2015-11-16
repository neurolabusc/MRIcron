Unit statcr;
interface
uses dialogsx,define_types;

const
	ITMAX = 300;                        
	EPS = 3.0e-7;
	kMaxFact = 1700;  {<= 1754}
	gFactRAready : boolean = false;
type
	FactRA = array[0..kMaxFact] of extended;
var
   gFactRA : FactRA;
FUNCTION betai(a,b,x: double): double;
procedure AlertMsg (pWarningStr: String);
function gammq( a,x: real): real;
function Fisher (A,B,C,D: integer): double;
procedure Chi2x2 (A, B, C, D: integer; var pMinExp, pChi, p, puChi, pup: double);
function Liebermeister (A,B,C,D: integer): extended;
procedure EstimateFDR(lnTests: integer; Ps: SingleP; var lFDR05, lFDR01: double);
function Fisher1TailMidP (A,B,C,D: integer): double; { use instead of chi2x2: returns p-value}
procedure InitFact;
procedure EstimateFDR2(lnTests: integer; var Ps: SingleP; var lFDR05, lFDR01,lnegFDR05, lnegFDR01: double);
procedure Descriptive (nV, SumOfSqrs, Sum: double; var lMn,lSD,lSE: double);
procedure SuperDescriptive (var RA: SingleP; n: integer; var lMn,lSD,lSE,lSkew,lZSkew: double);

implementation
uses Math{power};
procedure Descriptive (nV, SumOfSqrs, Sum: double; var lMn,lSD,lSE: double);
//given nV,SumOfSqrs,and Sum, returns Mean, StandardDeviation,StandardError and Skew
begin
     //first: initialize values
     lSD := 0;
     lSE := 0;
     lMn := 0;
     if nV < 1 then
     	exit;
     //next: compute mean
     lMn := Sum / nV;
     if (nV < 2) then
        exit;
     lSD := SumOfSqrs-(Sum*Sum/nV);
     lSD := sqrt((lSD)/(nV-1) );
     lSE := lSD/ sqrt(nV);
end;

procedure SuperDescriptive (var RA: SingleP; n: integer; var lMn,lSD,lSE,lSkew,lZSkew: double);
var
   i: integer;
   SumOfSqrs,Sum,Sigma: double;
begin
     lMn:= 0;
     lSD := 0;
     lSE := 0;
     lSkew := 0;
     lZSkew := 0;
     if n < 1 then exit;
     Sum := 0;
     SumOfSqrs := 0;
     for i := 1 to n do begin
         Sum := Sum + RA^[i];
         SumOfSqrs := SumOfSqrs + sqr(RA^[i]);
     end;
     Descriptive (n, SumOfSqrs, Sum,lMn,lSD,lSE);
     if (n < 3) or (lSD = 0) then
        lSkew := 0
     else begin
          Sigma := 0;
          for i := 1 to n do
               Sigma := Sigma + Power( ((RA^[i]-lMn) / lSD)  ,3);
         lSkew := (n/ ( (n-1)*(n-2)  ) ) *  Sigma;
     end;
     lZSkew := lSkew/(sqrt(6/N));
end;

procedure InitFact;
var lX: word;
begin
	gFactRA[0]:= 1;
	gFactRA[1] := 1;
	for lx := 2 to kMaxFact do
		 gFactRA[lx] := lx * gFactRA[lx-1];
	gFactRAready := true;
end;

function FisherX (A,B,C,D: integer): double; {FisherExactTest, use instead of chi}
{FisherX computes odds for this specific config only, not more extreme cases}
{alternate to Chi Square, see Siegel & Castellan, Nonparametric Statistics}
{use instead of Chi when n <= 20}
{A= X hits, B= control hits, C = X misses, D = control misses}
var
   N: word;
begin
	 N := A+B+C+D;
	 if (N <= kMaxFact) and (A>=0) and (B>=0) and (C>=0) and (D>=0) and (N > 0) then begin
		FisherX := (
			(gFactRA[A+B]/gFactRA[A])*
			(gFactRA[B+D]/gFactRA[B])*
			(gFactRA[A+C]/gFactRA[C])*
			(gFactRA[C+D]/gFactRA[D])
			)/ gFactRA[N];
	 end else FisherX := 0;
end;
function MidPKingFisher (lSmal,lCross1,lCross2,lSmalDiag: integer): extended;
var
   lProb1, lProb2: extended;
   lA,lB,lC,lD,lCnt: integer;
   l1st : boolean;
begin
	 lA :=lSmal;
	 lB:=lCross1;
	 lC:=lCross2;
	 lD:=lSmalDiag;
	 lProb1:=0;
	 l1st := true; //set to true for midP
	 for lCnt := lA downto 0 do begin
		if l1st then
		 lProb1 := 0.5* FisherX(lA,lB,lC,lD)
		else
		 lProb1 := lProb1 + FisherX(lA,lB,lC,lD);
		 l1st := false;
		 dec(lA);
		 dec(lD);
		 inc(lB);
		 inc(lC);
	 end;
	 lA :=lSmal;
	 lB:=lCross1;
	 lC:=lCross2;
	 lD:=lSmalDiag;
	 lProb2:=0;
	 l1st := true;  //alfa -set to true for MidP
	 while (lB >= 0) and (lC >= 0) do begin
		if l1st then
			lProb2 := 0.5* FisherX(lA,lB,lC,lD)
		else
			lProb2 := lProb2 + FisherX(lA,lB,lC,lD);
		l1st := false;
		 inc(lA);
		 inc(lD);
		 dec(lB);
		 dec(lC);
	 end;
	 if lProb1 < lProb2 then
		result := lProb1
	 else
		 result := lProb2;
	 //result := lprob1;
end;

function KingFisher (lSmal,lCross1,lCross2,lSmalDiag: integer): double;
var
   lProb1, lProb2: double;
   lA,lB,lC,lD,lCnt: integer;

begin
	 lA :=lSmal;
	 lB:=lCross1;
	 lC:=lCross2;
	 lD:=lSmalDiag;
	 lProb1:=0;
	 for lCnt := lA downto 0 do begin
		 lProb1 := lProb1 + FisherX(lA,lB,lC,lD);
		 dec(lA);
		 dec(lD);
		 inc(lB);
		 inc(lC);
	 end;
	 lA :=lSmal;
	 lB:=lCross1;
	 lC:=lCross2;
	 lD:=lSmalDiag;
	 lProb2:=0;
	 while (lB >= 0) and (lC >= 0) do begin
		 lProb2 := lProb2 + FisherX(lA,lB,lC,lD);
		 inc(lA);
		 inc(lD);
		 dec(lB);
		 dec(lC);
	 end;
	 if lProb1 < lProb2 then
		result := lProb1
	 else
		 result := lProb2;
end;

function Lieber (lSmal,lCross1,lCross2,lSmalDiag: integer): extended;
var
   lA,lB,lC,lD,lCnt: integer;
begin
	 lA :=lSmal;
	 lB:=lCross1+1;
	 lC:=lCross2+1;
	 lD:=lSmalDiag;
	 result :=0;
	 for lCnt := lA downto 0 do begin
		 result := result + FisherX(lA,lB,lC,lD);
		 dec(lA);
		 dec(lD);
		 inc(lB);
		 inc(lC);
	 end;
	 //TabbedNotebookDlg.caption := realtostr(result,6) ;
	  //TabbedNotebookDlg.caption := realtostr(result,6) ;
	 if result <= 0.5 then
		exit;

	 lA :=lSmal+1;
	 lB:=lCross1;
	 lC:=lCross2;
	 lD:=lSmalDiag+1;
	 result:=0;
	 while (lB >= 0) and (lC >= 0) do begin
		 result := result + FisherX(lA,lB,lC,lD);
		 inc(lA);
		 inc(lD);
		 dec(lB);
		 dec(lC);
	 end;
end;

function Liebermeister (A,B,C,D: integer): extended;
{A= X hits, B= control hits, C = X misses, D = control misses}
begin
	 result := 1;
	 if (A+B+C+D)<1 then
		exit;
	 if not gFactRAready then InitFact;
	 if (A<=B) and (A<=C) and (A<=D) then {lA smallest}
		result :=Lieber(A,B,C,D)
	 else if (B<=C) and (B<=D) then {lB smallest}
		  result :=Lieber(B,A,D,C)
	 else if (C<=D) then {lC smallest}
		  result :=Lieber(C,D,A,B)
	 else {d smallest}
		  result :=Lieber(D,C,B,A);
	 if ((A+C)>0) and ((B+D)>0) then begin
		if (A/(A+C)) < (B/(B+D)) then
			result := -result;
	 end;
end;

(*function Liebermeister (Ain,Bin,Cin,Din: integer): extended;
var
	A,B,C,D: integer;
{A= X hits, B= control hits, C = X misses, D = control misses}
begin
	 A := Ain;
	 B := Bin;
	 C := Cin;
	 D := Din;
	 if (A+B+C+D)<1 then begin
		result := 1;
		exit;
	 end;
	 //easy way to calculate Lieberman - make more extreme, then calculate Fisher
	 if abs(A-D) > abs(B-C) then begin
		inc(A);
		inc(D);
	 end else begin
		inc(B);
		inc(C);
	 end;
	 if not gFactRAready then InitFact;
	 if (A<=B) and (A<=C) and (A<=D) then {lA smallest}
		result :=KingFisher(A,B,C,D)
	 else if (B<=C) and (B<=D) then {lB smallest}
		  result :=KingFisher(B,A,D,C)
	 else if (C<=D) then {lC smallest}
		  result :=KingFisher(C,D,A,B)
	 else {d smallest}
		  result :=KingFisher(D,C,B,A);
	 if ((A+C)>0) and ((B+D)>0) then begin
		if (A/(A+C)) < (B/(B+D)) then
			result := -result;
	 end;
end;*)
function Fisher (A,B,C,D: integer): double;
{A= X hits, B= control hits, C = X misses, D = control misses}
begin
	 if (A+B+C+D)<1 then begin
		result := 1;
		exit
	end;
	 if not gFactRAready then InitFact;
	 if (A<=B) and (A<=C) and (A<=D) then {lA smallest}
		result :=KingFisher(A,B,C,D)
	 else if (B<=C) and (B<=D) then {lB smallest}
		  result :=KingFisher(B,A,D,C)
	 else if (C<=D) then {lC smallest}
		  result :=KingFisher(C,D,A,B)
	 else {d smallest}
		  result :=KingFisher(D,C,B,A);
	 if ((A+C)>0) and ((B+D)>0) then begin
		if (A/(A+C)) < (B/(B+D)) then
			result := -result;
	 end;
end;


function Fisher1TailMidP (A,B,C,D: integer): double;
{A= X hits, B= control hits, C = X misses, D = control misses}
begin
	 if (A+B+C+D)<1 then begin
		result := 1;
		exit
	end;
	 if not gFactRAready then InitFact;
	 if (A<=B) and (A<=C) and (A<=D) then {lA smallest}
		result :=MidPKingFisher(A,B,C,D)
	 else if (B<=C) and (B<=D) then {lB smallest}
		  result :=MidPKingFisher(B,A,D,C)
	 else if (C<=D) then {lC smallest}
		  result :=MidPKingFisher(C,D,A,B)
	 else {d smallest}
		  result :=MidPKingFisher(D,C,B,A);
	 if ((A+C)>0) and ((B+D)>0) then begin
		if (A/(A+C)) < (B/(B+D)) then
			result := -result;
	 end;
end;

procedure Sort (first, last: integer; var DynDataRA:SingleP);
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
				if (i >= 1) then
					goto 555;
			end
		end
	end
end;//sort

procedure EstimateFDR(lnTests: integer; Ps: SingleP; var lFDR05, lFDR01: double);
var
	lInc: integer;
	Qs: SingleP;
begin
	//rank Pvalues
	Sort(1,lnTests,Ps);
	{lStr := 'sort=';
	for lInc := 1 to knTests do
		lStr := lStr+realtostr(Ps[lInc],4)+',';
		Memo1.Lines.Add(lStr); }
	GetMem(Qs,lnTests*sizeof(single));
	//next findcrit FDR05
	for lInc := 1 to lnTests do
		Qs^[lInc] := (0.05*lInc)/lnTests;
	lFDR05 := 0;
	for lInc := 1 to lnTests do
		if Ps^[lInc] <= Qs^[lInc] then
				lFDR05 := Ps^[lInc];
	//next findcrit FDR01
	for lInc := 1 to lnTests do
		Qs^[lInc] := (0.01*lInc)/lnTests;
	lFDR01 := 0;
	for lInc := 1 to lnTests do
		if Ps^[lInc] <= Qs^[lInc] then
				lFDR01 := Ps^[lInc];
	Freemem(Qs);
end;

procedure EstimateFDR2(lnTests: integer; var Ps: SingleP; var lFDR05, lFDR01,lnegFDR05, lnegFDR01: double);
var
	lInc: integer;
	lrPs,Qs: SingleP;
begin
	//rank Pvalues
	Sort(1,lnTests,Ps);
	{lStr := 'sort=';
	for lInc := 1 to knTests do
		lStr := lStr+realtostr(Ps[lInc],4)+',';
		Memo1.Lines.Add(lStr); }
	GetMem(Qs,lnTests*sizeof(single));
	//next findcrit FDR05
	for lInc := 1 to lnTests do
		Qs^[lInc] := (0.05*lInc)/lnTests;
	lFDR05 := 0;
	for lInc := 1 to lnTests do
		if Ps^[lInc] <= Qs^[lInc] then
				lFDR05 := Ps^[lInc];
	//next findcrit FDR01
	for lInc := 1 to lnTests do
		Qs^[lInc] := (0.01*lInc)/lnTests;
	lFDR01 := 0;
	for lInc := 1 to lnTests do
		if Ps^[lInc] <= Qs^[lInc] then
				lFDR01 := Ps^[lInc];

        //reverse
        GetMem(lrPs,lnTests*sizeof(single));
	for lInc := 1 to lnTests do
		lrPs^[lInc] := 1- Ps^[lnTests-lInc+1];
	//for lInc := 1 to lnTests do
	//	Ps[lInc] := lR[lnTests-lInc+1];

	for lInc := 1 to lnTests do
		Qs^[lInc] := (0.05*lInc)/lnTests;
	lnegFDR05 := 0;
	for lInc := 1 to lnTests do
		if lrPs^[lInc] <= Qs^[lInc] then
				lnegFDR05 := lrPs^[lInc];
	//next findcrit FDR01
	for lInc := 1 to lnTests do
		Qs^[lInc] := (0.01*lInc)/lnTests;
	lnegFDR01 := 0;
	for lInc := 1 to lnTests do
		if lrPs^[lInc] <= Qs^[lInc] then
				lnegFDR01 := lrPs^[lInc];
        FreeMem(lrPs);
	Freemem(Qs);
end;


procedure AlertMsg (pWarningStr: String);
begin
	 ShowMsg(pWarningStr);
end;

function gammln (xx: double): double;  {Numerical Recipes for Pascal, p 177}
		const
			stp = 2.50662827465;
		var
			x, tmp, ser: double;
begin
		x := xx - 1.0;
		tmp := x + 5.5;
		tmp := (x + 0.5) * ln(tmp) - tmp;
		ser := 1.0 + 76.18009173 / (x + 1.0) - 86.50532033 /
		 (x + 2.0) + 24.01409822 / (x + 3.0) - 1.231739516 / (x + 4.0) + 0.120858003e-2 / (x + 5.0) - 0.536382e-5 / (x + 6.0);
		gammln := tmp + ln(stp * ser)
end; {procedure gammln}

FUNCTION betacf(a,b,x: double): double;
LABEL 1;
CONST
   itmax=100;
   eps=3.0e-7;
VAR
   tem,qap,qam,qab,em,d: double;
   bz,bpp,bp,bm,az,app: double;
   am,aold,ap: double;
   m: integer;
BEGIN
   am := 1.0;
   bm := 1.0;
   az := 1.0;
   qab := a+b;
   qap := a+1.0;
   qam := a-1.0;
   bz := 1.0-qab*x/qap;
   FOR m := 1 TO itmax DO BEGIN
	  em := m;
	  tem := em+em;
	  d := em*(b-m)*x/((qam+tem)*(a+tem));
	  ap := az+d*am;
	  bp := bz+d*bm;
	  d := -(a+em)*(qab+em)*x/((a+tem)*(qap+tem));
	  app := ap+d*az;
	  bpp := bp+d*bz;
	  aold := az;
	  am := ap/bpp;
	  bm := bp/bpp;
	  az := app/bpp;
	  bz := 1.0;
	  IF ((abs(az-aold)) < (eps*abs(az))) THEN GOTO 1
   END;
   writeln('pause in BETACF');
   writeln('a or b too big, or itmax too small'); readln;
1:   betacf := az
END;


FUNCTION betai(a,b,x: double): double;
VAR
   bt: double;
BEGIN
   IF ((x < 0.0) OR (x > 1.0)) THEN BEGIN
      writeln('pause in routine BETAI'); readln
   END;
   IF ((x = 0.0) OR (x = 1.0)) THEN bt := 0.0
   ELSE bt := exp(gammln(a+b)-gammln(a)-gammln(b)
           +a*ln(x)+b*ln(1.0-x));
   IF (x < ((a+1.0)/(a+b+2.0))) THEN
      betai := bt*betacf(a,b,x)/a
   ELSE betai := 1.0-bt*betacf(b,a,1.0-x)/b
END;

procedure gser(var gamser, a,x, gln: real);
var n: integer;
	sum, del, ap: real;
begin
	gln := gammln(a);
	if x <= 0.0 then begin
		if x < 0.0 then AlertMsg('x less then 0 in routine GSER');
		gamser:= 0.0;
	end else begin
		ap := a;
		sum := 1.0/a;
		del := sum;
		for n := 1 to ITMAX do begin
			ap := ap + 1;
			del := del * (x/ap);
			sum := sum + del;
			if (abs(del) < abs((sum)*EPS) )then begin
				gamser := sum * exp(-x+a*ln(x)-gln);
				exit;
			end;
		end;
		Alertmsg('GSER error: ITMAX too small for requested a-value');
	end;
end;

procedure gcf(var gammcf: real; a,x, gln: real);
var n: integer;
	gold,fac,b1,b0,a0,g,ana,anf,an,a1: real;
begin
	fac := 1.0;
	b1 := 1.0;
	b0 := 0.0;
	a0 := 1.0;
	gold := 0.0;
	gln := gammln(a);
	a1 := x;
	for n := 1 to ITMAX do begin
		an :=(n);
		ana := an - a;
		a0 := (a1 + a0*ana)*fac;
		b0 := (b1 + b0*ana)*fac;
		anf := an * fac;
		a1 := x*a0+anf*a1;
		b1 := x*b0+anf*b1;
		if a1 <> 0 then begin
			fac := 1.0/a1;
			g := b1*fac;
			if (abs((g-gold)/g)<EPS) then begin
				gammcf := exp(-x+a*ln(x)-gln)*g;
				exit;
			end;
			gold := g;
		end;
	end;
	Alertmsg('GCF error: ITMAX too small for requested a-value');
end;

function gammq( a,x: real): real;
	var gamser, gammcf, gln: real;
begin
		gammq := 0;
	if (x < 0) or (a <= 0.0) then alertmsg('Invalid arguments in routine GAMMQ')
	else begin
		if (x < (a+1.0)) then begin
			gser(gamser,a,x,gln);
			gammq := 1.0 - gamser;
		end else begin
			gcf(gammcf,a,x,gln);
			gammq := gammcf;
		end;
	end;
end;


procedure Chi2x2 (A, B, C, D: integer; var pMinExp, pChi, p, puChi, pup: double);
 {A= X hits, B= control hits, C = X misses, D = control misses}
 var
	lA, lB, lC, lD, lN: extended; {AEXp, BExp, CExp, Dexp, }
	lSameOdds: boolean;
 begin
	  lA := A; {convert to extended}
	  lB := B;
	  lC := C;
	  lD := D;
	  ln := lA + lB + lC + lD;
	  if lN > 0 then begin {avoid divide by 0}
		 pMinExp := ((lA + lB) * (lA + lC)) / lN;
		 if (((lA + lB) * (lB + lD)) / lN) < pMinExp then
			pMinExp := ((lA + lB) * (lB + lD)) / lN;
		 if (((lC + lD) * (lA + lC)) / lN) < pMinExp then
			pMinExp := ((lC + lD) * (lA + lC)) / lN;
		 if (((lC + lD) * (lB + lD)) / lN) < pMinExp then
			pMinExp := ((lC + lD) * (lB + lD)) / lN;
	  end else
		  pMinExp := 0;
	  lSameOdds := false;
	  if (lC > 0) and (lD > 0) then begin
		 if (lA / lC) = (lB / lD) then
			lSameOdds := true;
	  end;
	  if (lC = 0) and (lD = 0) then
		 lSameOdds := true;
	  if ((lA+lC) = 0) or ((lB+lD) = 0) then
		 lSameOdds := true;
	  if (lSameOdds = true) then begin
		 pChi := 0;   {same odds}
		 p := 1.0;
		 puChi := 0;
		 pup := 1.0;
	  end else begin
		  puChi := ((sqr((lA * lD) - (lB * lC))) * lN) / ((la + lb) * (lc + ld) * (lb + ld) * (la + lc));
		  pup := gammq(0.5, 0.5 * puChi); {half df}
		  pChi := ((sqr(abs((lA * lD) - (lB * lC)) - (0.5 * lN))) * lN) / ((la + lb) * (lc + ld) * (lb + ld) * (la + lc));
		  p := gammq(0.5, 0.5 * pChi);
	  end;
 end;


END.
