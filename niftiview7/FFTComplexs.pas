{ Unit FFTComplexs

  This unit implements complex number arithmic, including the basic
  operations addition, substraction, multiplication, division,
  magnitude and phase.

  Copyright: Nils Haeck M.Sc. (email: n.haeck@simdesign.nl)
  For more information visit http://ww.simdesign.nl
  Original date of publication: 10 Mar 2003

  This unit requires these other units:
  - Math:  Delphi mathematics unit
  - Types: Additional mathematical variable types

  ****************************************************************

  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at:
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
}
unit FFTComplexs;

interface

uses
  FFTTypes, Math;

type

  // Complex numbers, with precision specified in TFloat (Types unit)
  TComplex = packed record
    Re: TFloat; // Real part
    Im: TFloat; // Imaginary part
  end;

const

  // Zero value
  ComplexZero: TComplex = (Re: 0.0; Im: 0.0);

// Set a complex number
function Complex(Re: TFloat; Im: TFloat): TComplex;

// Add complex numbers (Result = C1 + C2)
function ComplexAdd(const C1, C2: TComplex): TComplex;

// Substract complex numbers (Result = C1 - C2)
function ComplexSub(const C1, C2: TComplex): TComplex;

// Multiply complex numbers (Result = C1 * C2)
function ComplexMul(const C1, C2: TComplex): TComplex;

// Scale complex numbers (Result = Scale * C)
function ComplexScl(Scale: TFloat; const C: TComplex): TComplex;

// Get the magnitude of the complex number C
function ComplexMag(const C: TComplex): TFloat;

// Get the phase of the complex number C (in radians, between -pi and pi)
function ComplexPhase(const C: TComplex): TFloat;

implementation

function Complex(Re: TFloat; Im: TFloat): TComplex;
// Set a complex number
begin
  Result.Re := Re;
  Result.Im := Im;
end;

function ComplexAdd(const C1, C2: TComplex): TComplex;
// Add complex numbers (Result = C1 + C2)
begin
  Result.Re := C1.Re + C2.Re;
  Result.Im := C1.Im + C2.Im;
end;

function ComplexSub(const C1, C2: TComplex): TComplex;
// Substract complex numbers (Result = C1 - C2)
begin
  Result.Re := C1.Re - C2.Re;
  Result.Im := C1.Im - C2.Im;
end;

function ComplexMul(const C1, C2: TComplex): TComplex;
// Multiply complex numbers (Result = C1 * C2)
begin
  Result.Re := C1.Re * C2.Re - C1.Im * C2.Im;
  Result.Im := C1.Im * C2.Re + C1.Re * C2.Im;
end;

function ComplexScl(Scale: TFloat; const C: TComplex): TComplex;
// Scale complex numbers (Result = Scale * C)
begin
  Result.Re := Scale * C.Re;
  Result.Im := Scale * C.Im;
end;

function ComplexMag(const C: TComplex): TFloat;
// Get the magnitude of the complex number C
begin
  Result := sqrt(sqr(C.Re) + sqr(C.Im));
end;

function ComplexPhase(const C: TComplex): TFloat;
// Get the phase of the complex number C (in radians, between -pi and pi)
const
  c2Pi  = 2 * pi;
  cPid2 = 0.5 * pi;
begin
  // Both zero
  if (C.Re = 0) and (C.Im = 0) then begin
    Result := 0;
    exit;
  end;

  // Non-zero case
  if abs(C.Re) > abs(C.Im) then begin
    Result := ArcTan(C.Im / C.Re); {-45 to 45 deg, 135 to -135 deg}
    if C.Re < 0 then Result := Result + pi;
  end else begin
    Result := cPid2 - ArcTan(C.Re / C.Im); {45 to 135, -45 to -135}
    if C.Im < 0 then Result := Result + pi;
  end;
  if Result > pi then Result := Result - c2pi;
end;

end.
