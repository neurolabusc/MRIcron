unit lesion_pattern;

interface
uses define_types;

Type
  TLesionPattern =  RECORD
		lowest, lo,hi,highest : int64;
  end;

function SetOrderX (var  lObs: Bytep;  var  lObsCount: integer): TLesionPattern ;
function SameOrder(lO1,lO2: TLesionPattern; lObsCount: integer): boolean;
function EmptyOrder: TLesionPattern;
procedure SetBit(lPos: integer; var lVal: TLesionPattern);

const
     kMaxBit = 63;
     kMaxBitx2 = 2*kMaxBit;
     kMaxBitx3 = 3*kMaxBit;

     kMaxObs = {126}kMaxBit*4;
implementation



var
   lPowerRA: array [1..kMaxBit] of int64;



procedure SetBit(lPos: integer; var lVal: TLesionPattern);
begin
    if  (lPos <= kMaxBit) then
        lVal.Lowest := lVal.Lowest + lPowerRA[lPos]
    else if  (lPos <= kMaxBitx2) then
        lVal.Lo := lVal.Lo + lPowerRA[lPos-kMaxBit]
    else if  (lPos <= kMaxBitx3) then
        lVal.Hi := lVal.Hi + lPowerRA[lPos-kMaxBitx2]
    else
        lVal.Highest := lVal.Highest + lPowerRA[lPos-kMaxBitx3];
end;

function EmptyOrder: TLesionPattern;
begin
    result.lowest := 0;
    result.lo := 0;
    result.hi := 0;
    result.highest := 0;
end;

function SameOrder(lO1,lO2: TLesionPattern; lObsCount: integer): boolean;
begin
   result := false;
   if  lObsCount > kMaxObs then
   	exit;
    if (lO1.lowest = lo2.lowest) and (lO1.highest = lO2.highest) and (lO1.lo = lo2.lo) and (lO1.hi = lO2.hi) then
       result := true
    else
        result := false;
end;

(*function SetOrder (var  lObs: Singlep;  var  lObsCount: integer): TLesionPattern ;
var
	lPos: integer;
begin
	result := EmptyOrder;
	if ( lObsCount > kMaxObs) or (lObsCount < 1)  then
   		exit;
	for lPos := 1  to lObsCount do
		if lObs[lPos] <> 0 then
			SetBit(lPos,result);
end;

function SetOrderI (var  lObs: LongIntp;  var  lObsCount: integer): TLesionPattern ;
var
	lPos: integer;
begin
	result := EmptyOrder;
	if ( lObsCount > kMaxObs) or (lObsCount < 1)  then
   		exit;
	for lPos := 1  to lObsCount do
		if lObs[lPos] <> 0 then
			SetBit(lPos,result);
end;*)

function SetOrderX (var  lObs: Bytep;  var  lObsCount: integer): TLesionPattern ;
var
	lPos: integer;
begin
	result := EmptyOrder;
	if ( lObsCount > kMaxObs) or (lObsCount < 1)  then
   		exit;
	for lPos := 1  to (lObsCount) do
		if lObs^[lPos] <> 0 then
			SetBit(lPos,result);
end;



var
lPowerPos: integer;
initialization
        lPowerRA[1] := 1;
	for lPowerPos := 2 to kMaxBit do
                lPowerRA[lPowerPos] := lPowerRA[lPowerPos-1]*2;

end.
