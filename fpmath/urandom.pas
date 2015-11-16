{ ******************************************************************
  Random number generators
  ****************************************************************** }

unit urandom;

interface

uses
  utypes, uranmwc, uranmt, uranuvag;

procedure SetRNG(RNG : RNG_Type);
{ Select generator and set default initialization }

procedure InitGen(Seed : LongInt);
{ Initialize generator }

function IRanGen : LongInt;
{ 32-bit random integer in [-2^31 .. 2^31 - 1] }

function IRanGen31 : LongInt;
{ 31-bit random integer in [0 .. 2^31 - 1] }

function RanGen1 : Float;
{ 32-bit random real in [0,1] }

function RanGen2 : Float;
{ 32-bit random real in [0,1) }

function RanGen3 : Float;
{ 32-bit random real in (0,1) }

function RanGen53 : Float;
{ 53-bit random real in [0,1) }

implementation

const
  Z  = 1.0 / 4294967296.0;        { 1 / 2^32 }
  Z1 = 1.0 / 4294967295.0;        { 1 / (2^32 - 1) }
  Z2 = 1.0 / 9007199254740992.0;

var
  gRNG : RNG_Type;

procedure SetRNG(RNG : RNG_Type);
var
  InitMT : MTKeyArray;
begin
  gRNG := RNG;
  case gRNG of
    RNG_MWC  : InitMWC(118105245);
    RNG_MT   : begin
                 InitMT[0] := $123;
                 InitMT[1] := $234;
                 InitMT[2] := $345;
                 InitMT[3] := $456;
                 InitMTbyArray(InitMT, 4);
               end;
    RNG_UVAG : InitUVAGbyString('abcd');
  end;
end;

procedure InitGen(Seed : LongInt);
begin
  case gRNG of
    RNG_MWC  : InitMWC(Seed);
    RNG_MT   : InitMT(Seed);
    RNG_UVAG : InitUVAG(Seed);
  end;
end;

function IRanGen : LongInt;
begin
  case gRNG of
    RNG_MWC  : IRanGen := IRanMWC;
    RNG_MT   : IRanGen := IRanMT;
    RNG_UVAG : IRanGen := IRanUVAG;
  end;
end;

function IRanGen31 : LongInt;
begin
  IRanGen31 := IRanGen shr 1;
end;

function RanGen1 : Float;
begin
  RanGen1 := (IRanGen + 2147483648.0) * Z1
end;

function RanGen2 : Float;
begin
  RanGen2 := (IRanGen + 2147483648.0) * Z
end;

function RanGen3 : Float;
begin
  RanGen3 := (IRanGen + 2147483648.5) * Z
end;

function RanGen53 : Float;
var
  A, B : LongInt;
begin
  A := IRanGen shr 5;
  B := IRanGen shr 6;

  RanGen53 := (A * 67108864.0 + B) * Z2;
end;

end.
