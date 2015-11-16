{ ******************************************************************
  Types and constants - Error handling - Dynamic arrays
  ******************************************************************
  The default real type is DOUBLE (8-byte real).
  Other types may be selected by defining the symbols:

       SINGLEREAL   (Single precision, 4 bytes)
       EXTENDEDREAL (Extended precision, 12 bytes)
  ****************************************************************** }

unit utypes;

interface

{$i types.inc}
{$ifdef fpc} {$mode delphi} {$endif}

{ ------------------------------------------------------------------
  Error handling
  ------------------------------------------------------------------ }

procedure SetErrCode(ErrCode : Integer);
{ Sets the error code }

function DefaultVal(ErrCode : Integer; DefVal : Float) : Float;
{ Sets error code and default function value }

function MathErr : Integer;
{ Returns the error code }

{ ------------------------------------------------------------------
  Dynamic arrays
  ------------------------------------------------------------------ }

procedure SetAutoInit(AutoInit : Boolean);
{ Sets the auto-initialization of arrays }

procedure DimVector(var V : PVector; Ub : Integer);
{ Creates floating point vector V[0..Ub] }

procedure DimIntVector(var V : PIntVector; Ub : Integer);
{ Creates integer vector V[0..Ub] }

procedure DimCompVector(var V : PCompVector; Ub : Integer);
{ Creates complex vector V[0..Ub] }

procedure DimBoolVector(var V : PBoolVector; Ub : Integer);
{ Creates boolean vector V[0..Ub] }

procedure DimStrVector(var V : PStrVector; Ub : Integer);
{ Creates string vector V[0..Ub] }

procedure DimMatrix(var A : PMatrix; Ub1, Ub2 : Integer);
{ Creates floating point matrix A[0..Ub1, 0..Ub2] }

procedure DimIntMatrix(var A : PIntMatrix; Ub1, Ub2 : Integer);
{ Creates integer matrix A[0..Ub1, 0..Ub2] }

procedure DimCompMatrix(var A : PCompMatrix; Ub1, Ub2 : Integer);
{ Creates complex matrix A[0..Ub1, 0..Ub2] }

procedure DimBoolMatrix(var A : PBoolMatrix; Ub1, Ub2 : Integer);
{ Creates boolean matrix A[0..Ub1, 0..Ub2] }

procedure DimStrMatrix(var A : PStrMatrix; Ub1, Ub2 : Integer);
{ Creates string matrix A[0..Ub1, 0..Ub2] }

procedure DelVector(var V : PVector; Ub : Integer);
{ Deletes floating point vector V[0..Ub] }

procedure DelIntVector(var V : PIntVector; Ub : Integer);
{ Deletes integer vector V[0..Ub] }

procedure DelCompVector(var V : PCompVector; Ub : Integer);
{ Deletes complex vector V[0..Ub] }

procedure DelBoolVector(var V : PBoolVector; Ub : Integer);
{ Deletes boolean vector V[0..Ub] }

procedure DelStrVector(var V : PStrVector; Ub : Integer);
{ Deletes string vector V[0..Ub] }

procedure DelMatrix(var A : PMatrix; Ub1, Ub2 : Integer);
{ Deletes floating point matrix A[0..Ub1, 0..Ub2] }

procedure DelIntMatrix(var A : PIntMatrix; Ub1, Ub2 : Integer);
{ Deletes integer matrix A[0..Ub1, 0..Ub2] }

procedure DelCompMatrix(var A : PCompMatrix; Ub1, Ub2 : Integer);
{ Deletes complex matrix A[0..Ub1, 0..Ub2] }

procedure DelBoolMatrix(var A : PBoolMatrix; Ub1, Ub2 : Integer);
{ Deletes boolean matrix A[0..Ub1, 0..Ub2] }

procedure DelStrMatrix(var A : PStrMatrix; Ub1, Ub2 : Integer);
{ Deletes string matrix A[0..Ub1, 0..Ub2] }

implementation

const
  gAutoInit : Boolean = True;

var
  gErrCode : Integer;

procedure SetErrCode(ErrCode : Integer);
begin
  gErrCode := ErrCode;
end;

function DefaultVal(ErrCode : Integer; DefVal : Float) : Float;
begin
  SetErrCode(ErrCode);
  DefaultVal := DefVal;
end;

function MathErr : Integer;
begin
  MathErr := gErrCode;
end;

procedure SetAutoInit(AutoInit : Boolean);
begin
  gAutoInit := AutoInit;
end;

procedure DimVector(var V : PVector; Ub : Integer);
var
  I : Integer;
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MAX_FLT) then
    begin
      V := nil;
      Exit;
    end;

  { Allocate vector }
  GetMem(V, (Ub + 1) * FltSize);
  if V = nil then Exit;

  { Initialize vector }
  if gAutoInit then
    for I := 0 to Ub do
      V^[I] := 0.0;
end;

procedure DimIntVector(var V : PIntVector; Ub : Integer);
var
  I : Integer;
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MAX_INT) then
    begin
      V := nil;
      Exit;
    end;

  { Allocate vector }
  GetMem(V, (Ub + 1) * IntSize);
  if V = nil then Exit;

  { Initialize vector }
  if gAutoInit then
    for I := 0 to Ub do
      V^[I] := 0;
end;

procedure DimCompVector(var V : PCompVector; Ub : Integer);
var
  I : Integer;
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MAX_COMP) then
    begin
      V := nil;
      Exit;
    end;

  { Allocate vector }
  GetMem(V, (Ub + 1) * CompSize);
  if V = nil then Exit;

  { Initialize vector }
  if gAutoInit then
    for I := 0 to Ub do
      begin
        V^[I].X := 0.0;
        V^[I].Y := 0.0;
      end;
end;

procedure DimBoolVector(var V : PBoolVector; Ub : Integer);
var
  I : Integer;
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MAX_BOOL) then
    begin
      V := nil;
      Exit;
    end;

  { Allocate vector }
  GetMem(V, (Ub + 1) * BoolSize);
  if V = nil then Exit;

  { Initialize vector }
  if gAutoInit then
    for I := 0 to Ub do
      V^[I] := False;
end;

procedure DimStrVector(var V : PStrVector; Ub : Integer);
var
  I : Integer;
begin
  { Check bounds }
  if (Ub < 0) or (Ub > MAX_STR) then
    begin
      V := nil;
      Exit;
    end;

  { Allocate vector }
  GetMem(V, (Ub + 1) * StrSize);
  if V = nil then Exit;

  { Initialize vector }
  if gAutoInit then
    for I := 0 to Ub do
      V^[I] := '';
end;

procedure DimMatrix(var A : PMatrix; Ub1, Ub2 : Integer);
var
  I, J : Integer;
  RowSize : Word;
begin
  if (Ub1 < 0) or (Ub1 > MAX_VEC) or (Ub2 < 0) or (Ub2 > MAX_FLT) then
    begin
      A := nil;
      Exit;
    end;

  { Allocate matrix }
  GetMem(A, (Ub1 + 1) * PtrSize);
  if A = nil then Exit;

  { Size of a row }
  RowSize := (Ub2 + 1) * FltSize;

  { Allocate each row }
  for I := 0 to Ub1 do
    begin
      GetMem(A^[I], RowSize);
      if A^[I] = nil then
        begin
          A := nil;
          Exit;
        end;
    end;

  { Initialize matrix }
  if gAutoInit then
    for I := 0 to Ub1 do
      for J := 0 to Ub2 do
        A^[I]^[J] := 0.0;
end;

procedure DimIntMatrix(var A : PIntMatrix; Ub1, Ub2 : Integer);
var
  I, J : Integer;
  RowSize : Word;
begin
  { Check bounds }
  if (Ub1 < 0) or (Ub1 > MAX_VEC) or (Ub2 < 0) or (Ub2 > MAX_INT) then
    begin
      A := nil;
      Exit;
    end;

  { Allocate matrix }
  GetMem(A, (Ub1 + 1) * PtrSize);
  if A = nil then Exit;

  { Size of a row }
  RowSize := (Ub2 + 1) * IntSize;

  { Allocate each row }
  for I := 0 to Ub1 do
    begin
      GetMem(A^[I], RowSize);
      if A^[I] = nil then
        begin
          A := nil;
          Exit;
        end;
    end;

  { Initialize matrix }
  if gAutoInit then
    for I := 0 to Ub1 do
      for J := 0 to Ub2 do
        A^[I]^[J] := 0;
end;

procedure DimCompMatrix(var A : PCompMatrix; Ub1, Ub2 : Integer);
var
  I, J : Integer;
  RowSize : Word;
begin
  { Check bounds }
  if (Ub1 < 0) or (Ub1 > MAX_VEC) or (Ub2 < 0) or (Ub2 > MAX_COMP) then
       begin
         A := nil;
         Exit;
       end;

  { Allocate matrix }
  GetMem(A, (Ub1 + 1) * PtrSize);
  if A = nil then Exit;

  { Size of a row }
  RowSize := (Ub2 + 1) * CompSize;

  { Allocate each row }
  for I := 0 to Ub1 do
    begin
      GetMem(A^[I], RowSize);
      if A^[I] = nil then
        begin
          A := nil;
          Exit;
        end;
    end;

  { Initialize matrix }
  if gAutoInit then
    for I := 0 to Ub1 do
      for J := 0 to Ub2 do
        begin
          A^[I]^[J].X := 0.0;
          A^[I]^[J].Y := 0.0;
        end;
end;

procedure DimBoolMatrix(var A : PBoolMatrix; Ub1, Ub2 : Integer);
var
  I, J : Integer;
  RowSize : Word;
begin
  { Check bounds }
  if (Ub1 < 0) or (Ub1 > MAX_VEC) or (Ub2 < 0) or (Ub2 > MAX_BOOL) then
    begin
      A := nil;
      Exit;
    end;

  { Allocate matrix }
  GetMem(A, (Ub1 + 1) * PtrSize);
  if A = nil then Exit;

  { Size of a row }
  RowSize := (Ub2 + 1) * BoolSize;

  { Allocate each row }
  for I := 0 to Ub1 do
    begin
      GetMem(A^[I], RowSize);
      if A^[I] = nil then
        begin
          A := nil;
          Exit;
        end;
      end;

  { Initialize matrix }
  if gAutoInit then
    for I := 0 to Ub1 do
      for J := 0 to Ub2 do
        A^[I]^[J] := False;
end;

procedure DimStrMatrix(var A : PStrMatrix; Ub1, Ub2 : Integer);
var
  I, J : Integer;
  RowSize : Word;
begin
  { Check bounds }
  if (Ub1 < 0) or (Ub1 > MAX_VEC) or (Ub2 < 0) or (Ub2 > MAX_STR) then
    begin
      A := nil;
      Exit;
    end;

  { Allocate matrix }
  GetMem(A, (Ub1 + 1) * PtrSize);
  if A = nil then Exit;

  { Size of a row }
  RowSize := (Ub2 + 1) * StrSize;

  { Allocate each row }
  for I := 0 to Ub1 do
    begin
      GetMem(A^[I], RowSize);
      if A^[I] = nil then
        begin
          A := nil;
          Exit;
        end;
    end;

  { Initialize matrix }
  if gAutoInit then
    for I := 0 to Ub1 do
      for J := 0 to Ub2 do
        A^[I]^[J] := '';
end;

procedure DelVector(var V : PVector; Ub : Integer);
begin
  if V <> nil then
    begin
      FreeMem(V, (Ub + 1) * FltSize);
      V := nil;
    end;
end;

procedure DelIntVector(var V : PIntVector; Ub : Integer);
begin
  if V <> nil then
    begin
      FreeMem(V, (Ub + 1) * IntSize);
      V := nil;
    end;
end;

procedure DelCompVector(var V : PCompVector; Ub : Integer);
begin
  if V <> nil then
    begin
      FreeMem(V, (Ub + 1) * CompSize);
      V := nil;
    end;
end;

procedure DelBoolVector(var V : PBoolVector; Ub : Integer);
begin
  if V <> nil then
    begin
      FreeMem(V, (Ub + 1) * BoolSize);
      V := nil;
    end;
end;

procedure DelStrVector(var V : PStrVector; Ub : Integer);
begin
  if V <> nil then
    begin
      FreeMem(V, (Ub + 1) * StrSize);
      V := nil;
    end;
end;

procedure DelMatrix(var A : PMatrix; Ub1, Ub2 : Integer);
var
  I : Integer;
  RowSize : Word;
begin
  if A <> nil then
    begin
      RowSize := (Ub2 + 1) * FltSize;
      for I := Ub1 downto 0 do
        FreeMem(A^[I], RowSize);
      FreeMem(A, (Ub1 + 1) * PtrSize);
      A := nil;
    end;
end;

procedure DelIntMatrix(var A : PIntMatrix; Ub1, Ub2 : Integer);
var
  I : Integer;
  RowSize : Word;
begin
  if A <> nil then
    begin
      RowSize := (Ub2 + 1) * IntSize;
      for I := Ub1 downto 0 do
        FreeMem(A^[I], RowSize);
      FreeMem(A, (Ub1 + 1) * PtrSize);
      A := nil;
    end;
end;

procedure DelCompMatrix(var A : PCompMatrix; Ub1, Ub2 : Integer);
var
  I : Integer;
  RowSize : Word;
begin
  if A <> nil then
    begin
      RowSize := (Ub2 + 1) * CompSize;
      for I := Ub1 downto 0 do
        FreeMem(A^[I], RowSize);
      FreeMem(A, (Ub1 + 1) * PtrSize);
      A := nil;
    end;
end;

procedure DelBoolMatrix(var A : PBoolMatrix; Ub1, Ub2 : Integer);
var
  I : Integer;
  RowSize : Word;
begin
  if A <> nil then
    begin
      RowSize := (Ub2 + 1) * BoolSize;
      for I := Ub1 downto 0 do
        FreeMem(A^[I], RowSize);
      FreeMem(A, (Ub1 + 1) * PtrSize);
      A := nil;
    end;
end;

procedure DelStrMatrix(var A : PStrMatrix; Ub1, Ub2 : Integer);
var
  I : Integer;
  RowSize : Word;
begin
  if A <> nil then
    begin
      RowSize := (Ub2 + 1) * StrSize;
      for I := Ub1 downto 0 do
        FreeMem(A^[I], RowSize);
      FreeMem(A, (Ub1 + 1) * PtrSize);
      A := nil;
    end;
end;

end.
