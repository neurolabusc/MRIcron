{ ******************************************************************
  Minimum, maximum, sign and exchange
  ****************************************************************** }

unit uminmax;

interface

uses
  utypes;

function FMin(X, Y : Float) : Float;      { Minimum of 2 reals }
function FMax(X, Y : Float) : Float;      { Maximum of 2 reals }
function IMin(X, Y : Integer) : Integer;  { Minimum of 2 integers }
function IMax(X, Y : Integer) : Integer;  { Maximum of 2 integers }
function Sgn(X : Float) : Integer;        { Sign (returns 1 if X = 0) }
function Sgn0(X : Float) : Integer;       { Sign (returns 0 if X = 0) }
function DSgn(A, B : Float) : Float;      { Sgn(B) * |A| }

procedure FSwap(var X, Y : Float);        { Exchange 2 reals }
procedure ISwap(var X, Y : Integer);      { Exchange 2 integers }

implementation

  function FMin(X, Y : Float) : Float;
  begin
    if X <= Y then
      FMin := X
    else
      FMin := Y;
  end;

  function FMax(X, Y : Float) : Float;
  begin
    if X >= Y then
      FMax := X
    else
      FMax := Y;
  end;

  function IMin(X, Y : Integer) : Integer;
  begin
    if X <= Y then
      IMin := X
    else
      IMin := Y;
  end;

  function IMax(X, Y : Integer) : Integer;
  begin
    if X >= Y then
      IMax := X
    else
      IMax := Y;
  end;

  function Sgn(X : Float) : Integer;
  begin
    if X >= 0.0 then
      Sgn := 1
    else
      Sgn := - 1;
  end;

  function Sgn0(X : Float) : Integer;
  begin
    if X > 0.0 then
      Sgn0 := 1
    else if X = 0.0 then
      Sgn0 := 0
    else
      Sgn0 := - 1;
  end;

  function DSgn(A, B : Float) : Float;
  begin
    if B < 0.0 then DSgn := - Abs(A) else DSgn := Abs(A)
  end;

  procedure FSwap(var X, Y : Float);
  var
    Temp : Float;
  begin
    Temp := X;
    X := Y;
    Y := Temp;
  end;

  procedure ISwap(var X, Y : Integer);
  var
    Temp : Integer;
  begin
    Temp := X;
    X := Y;
    Y := Temp;
  end;

end.
