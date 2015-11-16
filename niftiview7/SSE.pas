unit sse;
interface
uses define_types;

var gSSEenabled: boolean;
procedure SSEScale(var lMod,lMin,lMax : single; lMaxByte: byte; var lSz: integer; var lDataIn: SingleP; var lDataOut: ByteP);

implementation

procedure SSEScale(var lMod,lMin,lMax : single; lMaxByte: byte; var lSz: integer; var lDataIn: SingleP; var lDataOut: ByteP);
  var
     t : integer;
     tail : integer;
     parameters: singleP;
     paralign: singleP;
     localIn: singleP;
     localOut: byteP;
     localS: integer;
     fMaxByte: single;
  begin
    {Get a multiple-of-eight number of voxels}
    tail := lSz and 7;
    lSz := lSz and $fffffff8;
	GetMem(parameters, 80);
{	if tail <> 0 then begin
	  for t := 0 to (tail) do begin //1402
		  if lDataIn[1+t+lSz] > lMax then
			 lDataOut[1+t+lSz] := lMaxByte
		  else if lDataIn[1+t+lSz] < lMin then
			   lDataOut[1+t+lSz] := 0
		  else
			  lDataOut[1+t+lSz] := round((lDataIn[1+t+lSz]-lMin)* lMod);
	  end;
	end;}
	if tail <> 0 then begin
	  for t := 1 to (tail) do begin //1402
		  if lDataIn[t+lSz] > lMax then
			 lDataOut[t+lSz] := lMaxByte
		  else if lDataIn[t+lSz] < lMin then
			   lDataOut[t+lSz] := 0
		  else
			  lDataOut[t+lSz] := round((lDataIn[t+lSz]-lMin)* lMod);
	  end;
	end;
	paralign := singleP($fffffff0 and (integer(parameters)+15));
	fMaxByte := lMaxByte;
	for t := 1 to 4 do begin
	  paralign[t]    := lMod;//scale factors
	  paralign[t+4]  := lMin;//bias
	  paralign[t+8]  := 0; //zeroes
      paralign[t+12] := fMaxByte;//MaxByte
    end;

    localS := lSz;
    localIn := lDataIn;
    localOut := lDataOut;

   {Real problem here is getting the pointers into the right places
    It gives complete nonsense unless you copy the passed parameters
    into local variables before running}

    asm
       mov eax, (paralign);
       mov ebx, (localIn);
       mov ecx, (localS);
       mov edx, (localOut);

       db $0f, $28, $78, $00     // movaps xmm7, [eax]    - X7 is the scale factors
	   db $0f, $28, $70, $10     // movaps xmm6, [eax+16] - X6 is the bias
       db $0f, $28, $68, $20     // movaps xmm5, [eax+32] - X5 is zeroes
       db $0f, $28, $60, $30     // movaps xmm4, [eax+48] - X4 is MaxBytes

       @ProcessLoop:
       db $0f, $18, $83, $80, $00, $00, $00      // prefetchnta [ebx+128]
       db $0f, $18, $4a, $20                     // prefetcht0 [edx+32]
       db $0f, $28, $43, $00                     // movaps xmm0, [ebx]
       db $0f, $28, $4b, $10                     // movaps xmm1, [ebx+16]

       db $0f, $5c, $c6                          // subps xmm0, xmm6 -- bias
       db $0f, $59, $c7                          // mulps xmm0, xmm7 -- scale
       db $0f, $5c, $ce                          // subps xmm1, xmm6
       db $0f, $59, $cf                          // mulps xmm1, xmm7

       db $0f, $5d, $c4                          // minps xmm0, xmm4 -- chop left
       db $0f, $5f, $c5                          // maxps xmm0, xmm5 -- chop right
       db $0f, $5d, $cc                          // minps xmm1, xmm4
       db $0f, $5f, $cd                          // maxps xmm1, xmm5

       db $0f, $2c, $c0                          // cvttps2pi mm0,xmm0
       db $0f, $12, $c0                          // movhlps xmm0,xmm0
       db $0f, $2c, $c8                          // cvttps2pi mm1,xmm0
       db $0f, $2c, $d1                          // cvttps2pi mm2,xmm1
       db $0f, $12, $c9                          // movhlps xmm1,xmm1
       db $0f, $2c, $d9                          // cvttps2pi mm3,xmm1

       db $0f, $6b, $c1                          // packssdw mm0, mm1
       db $0f, $6b, $d3                          // packssdw mm2, mm3
       db $0f, $67, $c2                          // packuswb mm0, mm2

       db $0f, $7f, $42, $00                     // movq [edx],mm0
       add ebx, 32
       add edx, 8
       sub ecx, 8
       jne @ProcessLoop
       db $0f, $77                               // emms
    end;

    freemem(parameters);
  end;

initialization
  try
    gSSEenabled := false;
    asm
      mov eax, 1
      db $0F,$A2               /// cpuid
      test edx,(1 shl 25)
      jnz @SSEFound
      mov gSSEenabled,0
      jmp @END_SSE
    @SSEFound:
      mov gSSEenabled,1
    @END_SSE:
    end;
  except
    gSSEenabled := false;
  end;
(* gSSEenabled := true;
 try
    asm
    db $0F,$54,$C0 //andps xmm0,xmm0
    end;
 except
       gSSEenabled := false;
 end;(**)
end.
