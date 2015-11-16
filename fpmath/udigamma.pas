{ ******************************************************************
  DiGamma and TriGamma functions.
  Contributed by Philip Fletcher (FLETCHP@WESTAT.com)
  ****************************************************************** }

unit udigamma;

interface

uses
  utypes;

function DiGamma(X : Float ) : Float;
function TriGamma(X : Float ) : Float;

implementation

function DiGamma(X : Float ) : Float;
{ ------------------------------------------------------------------

  Digamma calculates the Digamma or Psi function =
  d ( LOG ( GAMMA ( X ) ) ) / dX


   Reference:

     J Bernardo,
     Psi ( Digamma ) Function,
     Algorithm AS 103,
     Applied Statistics,
     Volume 25, Number 3, pages 315-317, 1976.

   Modified:

     03 January 2000

   Parameters:

     Input, real X, the argument of the Digamma function.
     0 < X.

     Output, real Digamma, the value of the Digamma function at X.
  ------------------------------------------------------------------ }

const
  c  = 20                     ;
  d1 = -0.57721566490153286061;  { DiGamma(1) }
  s  = 0.00001                ;

  { Sterling coefficient S(n) = B(n) / 2n
    where B(n) = Bernoulli number          }

const
  S2  =  0.08333333333333333333    ;  { B(2)/2   }
  S4  = -0.83333333333333333333E-2 ;  { B(4)/4   }
  S6  =  0.39682539682539682541E-2 ;  { B(6)/6   }
  S8  = -0.41666666666666666666E-2 ;  { B(8)/8   }
  S10 =  0.75757575757575757576E-2 ;  { B(10)/10 }
  S12 = -0.21092796092796092796E-1 ;  { B(12)/12 }
  S14 =  0.83333333333333333335E-1 ;  { B(14)/14 }
  S16 = -0.44325980392156862745    ;  { B(16)/16 }

var
  dg, p, r, y : Float ;

begin
  if X <= 0.0 then
    begin
      DiGamma := DefaultVal(FSing, MaxNum);
      Exit;
    end;

  SetErrCode(FOk);

  if X = 1.0 then
    begin
      DiGamma := D1;
      Exit;
    end;

  { Use approximation if argument <= S }

  if X <= s then
    dg := d1 - 1.0 / x
  else
    { Reduce the argument to dg(X + N) where (X + N) >= C }
    begin
      dg := 0.0;
      y := x ;

      while y < c do
        begin
          dg := dg - 1.0 / y;
          y := y + 1.0;
        end ;

    { Use Stirling's (actually de Moivre's) expansion if argument > C }

      r := 1.0 / sqr ( y ) ;
      p := (((((((S16 * r + S14) * r + S12) * r + S10) * r + S8) * r +
               S6) * r + S4) * r + S2) * r ;
      dg := dg + ln ( y ) - 0.5 / y - p ;
    end ;

  DiGamma := dg ;
end ;

function TriGamma(X : Float) : Float;
{ ------------------------------------------------------------------
   Trigamma calculates the Trigamma or Psi Prime function =
   d**2 ( LOG ( GAMMA ( X ) ) ) / dX**2


   Reference:

     Algorithm As121 Appl. Statist. (1978) vol 27, no. 1
 ******************************************************************** }

const
  a     = 1.0E-4 ;
  b     = 20 ;
  zero  = 0 ;
  one   = 1 ;
  half  = 0.5 ;

  { Bernoulli numbers }

const
  B2  =  0.1666666666666667     ;
  B4  = -3.333333333333333E-002 ;
  B6  =  2.380952380952381E-002 ;
  B8  = -3.333333333333333E-002 ;
  B10 =  7.575757575757576E-002 ;
  B12 = -0.2531135531135531     ;

var
  y, z, Res : Float ;

begin
  if X <= 0.0 then
    begin
      TriGamma := DefaultVal(FSing, MaxNum);
      Exit;
    end;

  SetErrCode(FOk);

  Res := 0 ;
  z := x ;

  if z <= a then  { Use small value approximation }
    begin
      TriGamma := one / sqr ( z ) ;
      Exit ;
    end ;

  while z < b do  { Increase argument to (x+i) >= b }
    begin
      Res := Res + one / sqr ( z ) ;
      z := z + one ;
    end ;

  { Apply asymptotic formula where argument >= b }
  y := one / sqr ( z ) ;
  Res := Res + Half * y + (One + y * (B2 + y * (B4 + y * (B6 + y *
                         (B8 + y* (B10 + y * B12)))))) / z;
  TriGamma := Res;
end ;

end.