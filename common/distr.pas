
unit distr;
interface
{$Include isgui.inc}
uses Math,
{$IFDEF GUI}dialogs;{$ELSE} dialogsx;{$ENDIF}
function TtoZ(t,df: extended): extended;

function lnGamma(f:longint):extended;

      { Computes the logarithm of the gamma function at f/2. }


function pGamma(f:longint;y:extended):extended;

      { Returns the right tail probability in the gamma
        distribution with lambda = f/2. }

function pNormal(z:extended):extended;

//function pNormalOld(z:extended):extended;
//function pNormalOrig(z:extended):extended; //old
//function pNormalOrig(u:extended):extended; //old

	  { Returns the right tail probability in the normal distribution. }


function pChi2(f:longint;y:extended):extended;

      { Returns the right tail probability in the chi square distribution
        with f degrees of freedom. }


function pBeta(f1,f2:longint;y:extended):extended;

      { Returns the LEFT tail probability in the beta distribution
        with paramters lambda1=f1/2 and lambda2=f2/2. Use only
        f1 and f2 < 1E6.  }


function pFdistr(f1,f2:longint;y:extended):extended;

      { Returns the right tail probability in the F distribution
        with (f1,f2) degrees of freedom.
        Use only f1 and f2 < 1E6. }


function pTdistr(f:longint;y:extended):extended;

      { Returns the right tail probability in the T distribution.
        Use only  f < 1E6. }


function pNormalInv(p:extended):extended;
function pNormalInvQuickApprox(p : extended) : extended;//errors rise with Z>7

//function pNormalInvOld(p:extended):extended;



      { Inverse of pNormal. }


function pGammaInv(f:longint;p:extended):extended;

      { Inverse of pGamma(f,*). }


function pChi2Inv(f:longint;p:extended):extended;

      { Inverse of pChi2(f,*). }


function pBetaInv(f1,f2:longint;p:extended):extended;

      { Inverse of pBeta(f1,f2,*) (notice: LEFT tail). }


function pFdistrInv(f1,f2:longint;p:extended):extended;

      { 1-p percentile of F distribution. }

function pTdistrInv(f:longint;p:extended):extended;

      { 1-p percentile of T distribution. }


function pPoiss(lambda:extended; n:longint): extended;

      { Returns the right tail probability in the Poisson distribution. }


function PoissCL(n:longint; p:extended): extended;

      { Lower 1-p confidence limits for lambda in Poisson distribution
		when n is observed. }


function pBin(n,x:longint; p:extended): extended;

      { Returns the binomial right tail probability. }


function BinCL(n,x:longint; pp:extended): extended;

      { Returns confidence limit for binomial probability parameter,
        i.e. inverse to pBin(n,*). }

function ChiSq(x: double; n: integer): double;

{---------------------------------------------------------------------------}
implementation
//uses stat;

function ChiSq(x: double; n: integer): double;
var
   p,t: double;
   var k,a: integer;
begin
     p := exp(-0.5*x);
     if odd(n) then
        p := p * sqrt(2*x/Pi);
     k := round(n);
     while k >= 2 do begin
           p := p *x/k;
           k := k-2;
     end;
     t := p;
     a := round(n);
     while (t > 0.000001*p) do begin
           a := a+2;
           t := t*x/a;
           p := p + t;
     end;
     result := 1-p;
end;


function TtoZ(t,df: extended): extended;
// Converts a t value to an approximate z value w.r.t the given df
// s.t. std.norm.(z) = t(z, df) at the two-tail probability level.
//from http://www.anu.edu.au/nceph/surfstat/surfstat-home/tables/t.php
	var
		A9,B9,T9,Z8, P7, B7: extended;
	begin
	A9 := df - 0.5;
	B9 := 48*A9*A9;
	T9 := t*t/df;
	if T9 >= 0.04 then
		Z8 :=A9*ln(1+T9)
	else
		Z8 := A9*(((1 - T9*0.75)*T9/3 - 0.5)*T9 + 1)*T9;
	P7 := ((0.4*Z8 + 3.3)*Z8 + 24)*Z8 + 85.5;
	B7 := 0.8*power(Z8, 2) + 100 + B9;
	result :=  (1 + (-P7/B7 + Z8 + 3)/B9)*sqrt(Z8);
	if t < 0 then
		result := -result;
end;

function lnGamma(f:longint):extended;
var sum,y   : extended;
    k           : longint;
begin   y:=f/2;
if f>500 then
    begin
    sum:= ln(2*pi)/2  + (y-1/2)*ln(y); sum:=sum -y + 1/(12*y);
    sum:=sum - 1/360/y/y/y;
    lnGamma:=sum;
    end
else
    begin
    k:=f; sum:=0;
    while k>2 do
        begin
        k:=k-2;
        sum:=sum+ln(k/2);
        end;
    if k=1 then sum:=sum+ln(pi)/2;
    lnGamma:=sum;
    end;
end;

function pGamma(f:longint;y:extended):extended;
var  term,sum: extended;
     k      : longint;
begin if (y<=0) then pGamma:=1 else
if (y<f/2) or (y<42) then
    begin
    term:=(f/2)*ln(y)-y-lnGamma(f+2);
    if term>-1000 then term:=exp(term) else term:=0;
    sum:=0; k:=0;
    while ((f+k)*term>(f+k-2*y)*1E-20) do
        begin
        sum:=sum+term;
        term:=2*term*y/(f+k+2);
        k:=k+2;
        end;
    pGamma:=abs(1-sum);
    end
else
    begin
    term:=(f/2-1)*ln(y)-y-lnGamma(f);
    if term>-1000 then term:=exp(term) else term:=0;
    sum:=0; k:=0;
    while (term*y > (2*y-f+k)*0.5E-20) and (f-k>1) do
        begin
        sum:=sum+term;
        k:=k+2;
        term:=term*(f-k)/2/y;
        end;
    pGamma:=abs(sum);
    end;
end;
{---------------------------------------------------------------------------}
function pNormal(z:extended):extended;
const
PiD2=Pi/2;
var q: extended;
begin
	q := z*z;
	if abs(z)>7.0 then
		result := (0.5)*(1-1/q+3/(q*q))*Exp(-q/2)/(Abs(z)*Sqrt(PiD2))
	else
		result := pGamma(1,q/2)/2;
	if z<0 then result:=1-result;
end;

function pChi2(f:longint;y:extended):extended;
begin
pChi2:= pGamma(f,y/2);
end;

function pBeta0(f1,f2:longint; y:extended): extended;
      { Returns the left tail probability of the beta distribution
        with paramters lambda1=f1/2 and lambda2=f2/2. Use only f1+f2<40.
        Accuracy around +/- 1E-16 . }
var sum,term             : extended;
    k                   : longint;

begin
sum:=0; k:=0;
term:=lnGamma(f1+f2)-lnGamma(f2);
term:=term-lnGamma(f1+2)+f1*ln(y)/2;
term:=exp(term);
while (k<f2) or (abs(term) > 1E-20) do
    begin
    sum:=sum+term;
    k:=k+2;
    term:=-term*y*(f2-k)*(f1+k-2)/k/(f1+k);
    end;
pBeta0:=sum;
end;

function pBeta(f1,f2:longint;y:extended):extended;
var sum,term             : extended;
    k                   : longint;
    intch               : boolean;

begin  if (f1=f2) and (y=0.5) then pBeta:=0.5 else
       if y<=0 then pBeta:=0 else
       if y>=1 then pBeta:=1 else
    begin
    intch:=false;
    if y>(1-y) then
        begin intch:=true;
        k:=f1; f1:=f2; f2:=k;
        y:=1-y;
        end;
    if f1+f2<41 then sum:=pBeta0(f1,f2,y) else
        begin
        term:= (f2/2-1)*ln(1-y) + (f1/2)*ln(y)
            + lnGamma(f1+f2) - lnGamma(f1+2);
        term:=term - lnGamma(f2);
        if term > -1000 then term:=exp(term) else term:=0;
        if (term<1E-35) and (y<f1/(f1+f2)) then sum:=0
        else if  (term<1E-35) and (y>f1/(f1+f2)) then sum:=1
        else
            begin
            k:=0; sum:=0;
            while (abs(term)>1E-25) or (y*(f2-k) > (1-y)*(f1+k)) do
                begin sum:=sum+term;
                k:=k+2;
                term:= term*y*(f2-k)/(1-y)/(f1+k);
				end;
            end;
        end;
    if intch then sum:=1-sum;
    pBeta:= abs(sum);
    end;
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
end;

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

function pFdistr(f1,f2:longint;y:extended):extended;
begin
pFdistr:=pBeta(f2,f1,f2/(f1*y+f2));
end;

function pTdistr(f:longint;y:extended):extended;
begin
	if y = 0 then
		result := 0.5
	else begin
		result := betai(0.5*f,0.5,f/(f+sqr(y)))/2;
		if y < 0 then
			result := 1-result;
	end;
end;//from numerical recipes
(*below x5 slower than numerical recipes!
function pTdistr;
//function pTdistr(f:longint;y:extended):extended;
var  p: extended;

begin

if y=0 then pTdistr:=0.5 else
	begin
	p:=f/(y*y+f);
	p:=pBeta(f,1,p);  p:=p/2;
	if y<0 then p:=1-p;
	pTdistr:=p;
	end;
end;*)
{---------------------------------------------------------------------------}
(*function pNormalInv(p:extended):extended;
var
	v,dv,z: extended;
begin
	v := 0.5;
	dv := 0.5;
	z := 0;
	while (dv>1e-15) do begin
		z:=1/v-1;
		dv:=dv/2;
		if(pNormal(z)>p) then
			v:=v-dv
		else
			v:=v+dv;
	end;
	result := z;
end; *)
function pNormalInv(p:extended):extended;
var
	v,dv,z,tailp: extended;
begin
	if p <= 0.5 then
		tailp := p
	else
		tailp := 1-p;
	if tailp = 0 then begin
		result := 9.2;//fails with Z<-9
		exit;
	end;
	//showmessage('error'+realtostr(tailp,10));
	//showmessage(realtostr(tailp,10));
	v := 0.5;
	dv := 0.5;
	z := 0;
	while (dv>1e-15) do begin
		z:=1/v-1;
		dv:=dv/2;
		if(pNormal(z)>tailp) then
			v:=v-dv
		else
			v:=v+dv;
	end;
	if p <= 0.5 then
		result := z
	else
		result := -z;
end;

function zprob(p : extended {; VAR errorstate : boolean}) : extended;
VAR
   z, xp, lim, p0, p1, p2, p3, p4, q0, q1, q2, q3, q4, Y : extended;
begin
	 // value of probability between approx. 0 and .5 entered in p and the
	 // z value is returned  z
	 //errorstate := true;
	 lim := 1E-19;
	 p0 := -0.322232431088;
	 p1 := -1.0;
	 p2 := -0.342242088547;
	 p3 := -0.0204231210245;
	 p4 := -4.53642210148E-05;
	 q0 := 0.099348462606;
	 q1 := 0.588581570495;
	 q2 := 0.531103462366;
	 q3 := 0.10353775285;
	 q4 := 0.0038560700634;
	 xp := 0.0;
	 if (p > 0.5) then
		p := 1 - p;
	 if (p < lim) then //Z>9.5 Z<-9.5
		z := -pNormalInv(p) //use slow method
		//z := xp
	 else if (p = 0.5) then
		z := xp
	 else begin
			   Y := sqrt(ln(1.0 / (p * p)));
			   xp := Y + ((((Y * p4 + p3) * Y + p2) * Y + p1) * Y + p0) /
					((((Y * q4 + q3) * Y + q2) * Y + q1) * Y + q0);
			   if (p < 0.5) then xp := -xp;
			   z := xp;
	 end;
	 zprob := z;
end;  // End function zprob

function pNormalInvQuickApprox(p	 : extended) : extended;
var
   z, px : extended;
//   flag : boolean;
begin
	// obtains the inverse of z, that is, the z for a probability associated
		// with a normally distributed z score.
		px := p;
		if (p > 0.5) then px := 1.0 - p;
		if px < 0.000000000000001 then
                   z := -8 //lPs[lInc] := 0.000000000000001;
                else
			z := zprob(px{,flag});
		if (p > 0.5) then z := abs(z);
		result := -z;
end;    //End of inversez Function

function pGammaInv(f:longint;p:extended):extended;
var  pp,y,y0,a,b,a0       :extended;

begin a0:=-lnGamma(f);
if f=1 then
	begin
	y:=pNormalInv(p/2); y:=y*y/2;
	end
else
    begin if f>100 then
        begin y:= sqrt(2*f-1)+pNormalInv(p); y:=y*y/4;
        end
    else y:=f/2;
    y0:=1;
    pp:=pGamma(f,y);
    while y0>1E-7 do
        begin y0:=y;
        a:=a0+(f/2-1)*ln(y)-y;
        b:=(f/2-1)/y-1;
        if abs(b*(pp-p)*exp(-a))<1E-5 then y:=y+(pp-p)*exp(-a)
        else y:=y+ln(1+b*(pp-p)*exp(-a))/b;
        pp:=pGamma(f,y);
        y0:=abs(y-y0);
        end;
    end;
pGammaInv:=y;
end;

function pChi2Inv(f:longint;p:extended):extended;
var y:extended;

begin
y:=pGammaInv(f,p);
pChi2Inv:=2*y;
end;
{---------------------------------------------------------------------------}
function pBetaInv1(f1,f2:longint;p:extended):extended;

var  pp,y,y0,a,b,a0       :extended;

begin
if p<=0 then y:=0
else if p>=1 then y:=1
else if (f1=1) and (f2=1) then y:=sin(p*pi/2)*sin(p*pi/2)
else if (f1=1) and (f2=2) then y:=p*p
else if (f1=2) and (f2=1) then y:=1-(1-p)*(1-p)
else if (f1=2) and (f2=2) then y:=p
else
    begin
    a0:=-lnGamma(f1)-lnGamma(f2); a0:=a0+lnGamma(f1+f2);
    y:=f1/(f1+f2);
    if f1=1 then
        begin
        y:= pGammaInv(1,1-p);
        y:= 2*y/(2*y+f2-1/2);
        end;
    y0:=1;
    pp:=pBeta(f1,f2,y);
    while y0>1E-8 do
        begin
        a:=a0+(f1/2-1)*ln(y)+(f2/2-1)*ln(1-y);
        b:=(f1/2-1)/y-(f2/2-1)/(1-y);
        if abs(b*(pp-p))*exp(-a)<1E-5 then y0:=-(pp-p)*exp(-a)
        else y0:=ln(1-b*(pp-p)*exp(-a))/b;
        y:=y+y0;
        pp:=pBeta(f1,f2,y);
        y0:=abs(y0)/y/(1-y);
        end;
    end;
pBetaInv1:=y;
end;
{---------------------------------------------------------------------------}
function pBetaInv(f1,f2:longint;p:extended):extended;

var y: extended;

begin if f1<=f2 then y:=pBetaInv1(f1,f2,p)
else y:= 1-pBetaInv1(f2,f1,1-p);
pBetaInv := y;
end;
{---------------------------------------------------------------------------}
function pFdistrInv(f1,f2:longint;p:extended):extended;

var  y : extended;

begin
y:=pBetaInv(f2,f1,p);
if y = 0 then
	pFdistrInv:= 0 //infinityINF
else
	pFdistrInv:=f2/f1*(1-y)/y;
end;
{---------------------------------------------------------------------------}
function pTdistrInv(f:longint;p:extended):extended;

var t:extended;

begin if p<=0.5 then t:=sqrt(pFdistrInv(1,f,2*p))
else t:=-sqrt(pFdistrInv(1,f,2*(1-p)));
pTdistrInv:=t;
end;
{---------------------------------------------------------------------------}
function pPoiss(lambda:extended; n:longint): extended;
begin pPoiss:= 1-pGamma(2*n,lambda);
end;
{---------------------------------------------------------------------------}

function pBin(n,x:longint; p:extended): extended;
begin pBin:= pBeta(2*x,2+2*(n-x),p);
end;
{---------------------------------------------------------------------------}
function PoissCL(n:longint; p:extended): extended;

begin PoissCL := pGammaInv(2*n,1-p);
end;
{---------------------------------------------------------------------------}
function BinCL(n,x:longint; pp:extended): extended;

begin BinCL:= pBetaInv(2*x,2+2*(n-x),pp);
end;
{---------------------------------------------------------------------------}

end.
