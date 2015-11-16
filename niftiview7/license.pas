unit license;
interface
//uses Windows; //DWord definition
//uses definetypes;//dialogs,SysUtils;

 type
 TCPUIDARRAY=array[1..4] of Longint;
 //procedure Encrypt64(var lInt1,lInt2: longint);
 //procedure Decrypt64(var lInt1,lInt2: longint);
//procedure GetIDInfo64(var lInt64: Int64);
//procedure GetIDInfo(var lInt1,lInt2: longint);
function GetIDInfo32: longint;
function Encrypt32(var lIn: longint): longint;

implementation
const
  kStartKey	= 3891;  	{Start default key}
  kMultKey	  = 23272;	{Mult default key}
  kAddKey	  = 31198;	{Add default key}

function IsCPUID_Available : Boolean;assembler;register;
const
 ID_BIT=$200000; (** EFLAGS ID bit **)
asm
  PUSHFD							{direct access to flags no possible, only via stack}
  POP     EAX					{flags to EAX}
  MOV     EDX,EAX			{save current flags}
  XOR     EAX,ID_BIT	{not ID bit}
  PUSH    EAX					{onto stack}
  POPFD								{from stack to flags, with not ID bit}
  PUSHFD							{back to stack}
  POP     EAX					{get back to EAX}
  XOR     EAX,EDX			{check if ID bit affected}
  JZ      @exit				{no, CPUID not availavle}
  MOV     AL,True			{Result=True}
@exit:
end;

function GetCPUID : TCPUIDARRAY; assembler;register;
asm
  PUSH    EBX         	{Save affected register}
  PUSH    EDI
  MOV     EDI,EAX     	{@Resukt}
  MOV     EAX,1
  DW      $A20F       	{CPUID Command}
  STOSD			{CPUID[1]}
  MOV     EAX,EBX
  STOSD               	{CPUID[2]}
  MOV     EAX,ECX
  STOSD               	{CPUID[3]}
  MOV     EAX,EDX
  STOSD               	{CPUID[4]}
  POP     EDI					{Restore registers}
  POP     EBX
end;

function GetIDInfo32: longint;
var
  I     : Integer;
  CPUIDARRAY,ByteArray: TCPUIDARRAY;
begin
	result:=-1;
	//for I := Low(CPUIDARRAY) to High(CPUIDARRAY)  do CPUIDARRAY[I] := -1;
	if not IsCPUID_Available then exit;
	CPUIDARRAY:=GetCPUID;
	for I := Low(CPUIDARRAY) to High(CPUIDARRAY)  do begin
		ByteArray[I] := CPUIDARRAY[I] and 255;
		if ByteArray[I] = 0 then
			ByteArray[I] := (CPUIDARRAY[I] shr 8) and 255;
		if ByteArray[I] = 0 then
			ByteArray[I] := (CPUIDARRAY[I] shr 16) and 255;
		if ByteArray[I] = 0 then
			ByteArray[I] := (CPUIDARRAY[I] shr 24) and 255;
	end;
	result := (ByteArray[1] and 255)+((ByteArray[2] and 255) shl 8)
		+ ((ByteArray[3] and 255) shl 16)+((ByteArray[4] and 255) shl 24);
end;

(*procedure GetIDInfo(var lInt1,lInt2: longint);
 const
		 kMax16bit = (256*256)-1;
var
  I     : Integer;
  CPUIDARRAY,WordArray: TCPUIDARRAY;

begin
	lInt1:=-1;
	lInt2 := -1;
	//for I := Low(CPUIDARRAY) to High(CPUIDARRAY)  do CPUIDARRAY[I] := -1;
	if not IsCPUID_Available then exit;
	CPUIDARRAY:=GetCPUID;
	for I := Low(CPUIDARRAY) to High(CPUIDARRAY)  do begin
		WordArray[I] := CPUIDARRAY[I] and kMax16bit;
		if WordArray[I] = 0 then
			WordArray[I] := (CPUIDARRAY[I] shr 8) and kMax16bit;
		if WordArray[I] = 0 then
			WordArray[I] := (CPUIDARRAY[I] shr 16) and kMax16bit;
		if WordArray[I] = 0 then
			WordArray[I] := (CPUIDARRAY[I] shr 24) and kMax16bit;
	end;
	lInt1 := (WordArray[1] and kMax16bit)+((WordArray[2] and kMax16bit) shl 16);
	lInt2 := (WordArray[3] and kMax16bit)+((WordArray[4] and kMax16bit) shl 16);
end;
	   (**)
{$R-}
{$Q-}
function Encrypt32(var lIn: longint): longint;
type
  swaptype = packed record
	case byte of
	  0:(b1,b2,b3,b4 : byte); //byte is 8 bits
	  1:(Long:LongInt);  //long is 16 bits
  end;
  swaptypep = ^swaptype;
var
  inguy,outguy:swaptypep;
  lResult,StartKey:Longint;
begin
  StartKey := kStartKey;
  inguy := @lIn;
  outguy := @lResult;
  outguy.b1 := ((inguy.b1) xor (StartKey shr 8));
  StartKey := (outguy.b1 + StartKey) * kMultKey + kAddkey;
  outguy.b2 := ((inguy.b2) xor (StartKey shr 8));
  StartKey := (outguy.b2 + StartKey) * kMultKey + kAddkey;
  outguy.b3 := ((inguy.b3) xor (StartKey shr 8));
  StartKey := (outguy.b3 + StartKey) * kMultKey + kAddkey;
  outguy.b4 := ((inguy.b4) xor (StartKey shr 8));
	  //StartKey := (outguy.b4 + StartKey) * kMultKey + kAddkey;
	  //inguy.Long := outguy.long;
  Result := outguy.long;
end;

(*procedure Encrypt64(var lInt1,lInt2: longint);
type
  swaptype = packed record
	case byte of
	  0:(b1,b2,b3,b4 : byte); //byte is 8 bits
	  1:(Long:LongInt);  //long is 16 bits
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
  lLoop,StartKey:Integer;
begin
  StartKey := kStartKey;
  for lLoop := 1 to 2 do begin
      if lLoop = 1 then
		 inguy := @lInt1
      else
		  inguy := @lInt2; //assign address of s to inguy
      outguy.b1 := ((inguy.b1) xor (StartKey shr 8));
	  StartKey := (outguy.b1 + StartKey) * kMultKey + kAddkey;
      outguy.b2 := ((inguy.b2) xor (StartKey shr 8));
	  StartKey := (outguy.b2 + StartKey) * kMultKey + kAddkey;
      outguy.b3 := ((inguy.b3) xor (StartKey shr 8));
	  StartKey := (outguy.b3 + StartKey) * kMultKey + kAddkey;
      outguy.b4 := ((inguy.b4) xor (StartKey shr 8));
	  StartKey := (outguy.b4 + StartKey) * kMultKey + kAddkey;
	  inguy.Long := outguy.long;
  end;
end;

procedure Decrypt64(var lInt1,lInt2: longint);
type
  swaptype = packed record
    case byte of
      0:(b1,b2,b3,b4 : byte); //byte is 8 bits
      1:(Long:LongInt);  //long is 16 bits
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
  lLoop,StartKey:Integer;
begin
  StartKey := kStartKey;
  for lLoop := 1 to 2 do begin
      if lLoop = 1 then
         inguy := @lInt1
      else
          inguy := @lInt2; //assign address of s to inguy
      outguy.b1 := ((inguy.b1) xor (StartKey shr 8));
      StartKey := ((inguy.b1) + StartKey) * kMultKey + kAddkey;
      outguy.b2 := ((inguy.b2) xor (StartKey shr 8));
      StartKey := ((inguy.b2) + StartKey) * kMultKey + kAddkey;
      outguy.b3 := ((inguy.b3) xor (StartKey shr 8));
      StartKey := ((inguy.b3) + StartKey) * kMultKey + kAddkey;
      outguy.b4 := ((inguy.b4) xor (StartKey shr 8));
      StartKey := ((inguy.b4) + StartKey) * kMultKey + kAddkey;
      inguy.Long := outguy.long;
  end;
end;     (**)



end.
