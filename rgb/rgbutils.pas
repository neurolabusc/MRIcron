{
 /***************************************************************************
                                  RGBUtils.pas


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author:  Tom Gregorovic (_tom_@centrum.cz)

  Abstract:
}
unit RGBUtils;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif} 

interface

uses
  Classes, SysUtils;
  
type
  TIntArray = array of Integer;
  
  function DivideTrunc(Src, Dest: Integer): TIntArray;
  function GetMidPoints(const A: TIntArray): TIntArray;
  function GetDifference(const A: TIntArray): TIntArray;
  
  procedure SwapInt(var A, B: Integer);
  procedure SwapPtr(var A, B: Pointer);
  procedure MinMax(var A, B: Integer);

  procedure SortRect(var X1, Y1, X2, Y2: Integer); overload;
  procedure SortRect(var R: TRect); overload;
  
  procedure ClipDimension(ClipMin, ClipMax: Integer;
    var DstPos, SrcPos, SrcSize: Integer);

  operator =(A, B: TPoint): Boolean;

implementation

(*
  DivideTrunc divides bigger value of Src and Dest into array of chunks.
  Length(Result) = Min(Src, Dest)
*)
function DivideTrunc(Src, Dest: Integer): TIntArray;
var
  I: Integer;
  VMax, VMin: Integer;
  P, D: Single;
begin
  if Dest > Src then
  begin
    VMax := Dest;
    VMin := Src;
  end
  else
  begin
    VMax := Src;
    VMin := Dest;
  end;
  SetLength(Result, VMin);

  P := 0;
  D := VMax / VMin;
  for I := 0 to High(Result) do
  begin
    Result[I] := Round(P + D) - Round(P);
    P := P + D;
  end;
end;

(*
  GetMidPoints returns array of absolute positions of the middle in each chunk.
*)
function GetMidPoints(const A: TIntArray): TIntArray;
var
  I, P, V: Integer;
begin
  SetLength(Result, Length(A));
  P := 0;
  for I := 0 to High(A) do
  begin
    V := A[I];
    Result[I] := V shr 1 + P;
    Inc(P, V);
  end;
end;

(*
  GetDifference returns array of diffences between positions.
*)
function GetDifference(const A: TIntArray): TIntArray;
var
  I, P: Integer;
begin
  SetLength(Result, Length(A));
  P := 0;
  for I := 0 to High(A) do
  begin
    Result[I] := A[I] - P;
    P := A[I];
  end;
end;

operator =(A, B: TPoint): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

procedure SwapInt(var A, B: Integer);
var
  C: Integer;
begin
  C := A;
  A := B;
  B := C;
end;

procedure SwapPtr(var A, B: Pointer);
var
  C: Pointer;
begin
  C := A;
  A := B;
  B := C;
end;

procedure MinMax(var A, B: Integer);
var
  T: Integer;
begin
  if A > B then
  begin
    T := A;
    A := B;
    B := T;
  end;
end;

procedure SortRect(var X1, Y1, X2, Y2: Integer);
begin
  MinMax(X1, X2);
  MinMax(Y1, Y2);
end;

procedure SortRect(var R: TRect);
begin
  MinMax(R.Left, R.Right);
  MinMax(R.Top, R.Bottom);
end;

procedure ClipDimension(ClipMin, ClipMax: Integer;
    var DstPos, SrcPos, SrcSize: Integer);
var
  C: Integer;
begin
  if ClipMin > DstPos then
  begin
    C := ClipMin - DstPos;
    Inc(SrcPos, C);
    Dec(SrcSize, C);
    DstPos := ClipMin;
  end;
  
  if ClipMax < DstPos + SrcSize then SrcSize := ClipMax - DstPos;
end;
  

end.

