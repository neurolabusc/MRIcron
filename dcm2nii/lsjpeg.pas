unit lsjpeg;
{*$DEFINE Stream}
//rev13: changes by CR and JGS
//rev19: uses Lookup table for decoding Huffman table: this doubles the speed
//rev26: fixed memory leak: FreeMem(lRawRA)
interface
{$IFDEF FPC} {$mode delphi}{$H+} {$ENDIF}

uses
dialogsx,dialogs_msg,
  sysutils,define_types,classes;
type
    HufRA = record
           HufSz,HufCode,HufVal: Integer;
    end;
{$IFDEF Stream}
  procedure DecodeJPEG(var lStream: TMemoryStream; var lOutSmallRA: SmallIntP0; var lImgRAz: ByteP0;lOutputSz,lCptPosition,lCptSize: integer; lVerbose: boolean);
{$ELSE}
  procedure DecodeJPEG(var infp: file; var lOutSmallRA: SmallIntP0; var lImgRAz: ByteP0;lOutputSz,lCptPosition,lCptSize: integer; lVerbose: boolean);
{$ENDIF}
implementation

{$IFDEF Stream}
  procedure DecodeJPEG(var lStream: TMemoryStream; var lOutSmallRA: SmallIntP0; var lImgRAz: ByteP0;lOutputSz,lCptPosition,lCptSize: integer; lVerbose: boolean);
{$ELSE}
procedure DecodeJPEG(var infp: file; var lOutSmallRA: SmallIntP0; var lImgRAz: ByteP0;lOutputSz,lCptPosition,lCptSize: integer; lVerbose: boolean);
{$ENDIF}
const
     kmaxFrames = 4;
label
     666 {EOF}, 123 {Freemem};
  var
    lRawRA: bytep;
    lImgRA: WordP;
    lHufVal,lAbba,lOffset,lLineStart,lPredicted,lPredictedG,lPredictedB,lRestartSegmentSz,
    lSz,k,Code,Si,lIncX,lIncY,lInc,lPredA,lPredB,lPredC,lCurrentBitPos,btS1,btS2, btMarkerType,
    DHTnLi,DHTtcth,SOFprecision,SOSpttrans, SOFnf,SOFarrayPos,SOSns,SOSarrayPos,SOSss,SOSse,SOSahal:integer;//byte;
    lHufTable,lnHufTables,{lDecode,}lImgStart,lRawSz,lRawPos,lItems,SOFydim, SOFxdim: integer;
    lMaxHufSi,lMaxHufVal: array [1..kmaxFrames] of integer;
    DHTLiRA,DHTstartRA: array [1..kmaxFrames,0..31] of integer;//byte;
    lBitMask: array [1..17] of integer;
    lSSSSszRA: array [1..kMaxFrames,0..17] of byte;
    lLookUpRA: array [1..kMaxFrames,0..255] of byte; //lists all possible SSSS with <= 8bits
    lHufRA: array [1..kMaxFrames,0..31] of HufRA;
    lFrameCount,lSegmentLength,lSegmentEnd,lI: integer;
    lImgTypeC3,lHdrOK: boolean;
function ReadBit: integer; //Read the next single bit
begin
     result := (lRawRA[lRawPos] shr (7-lCurrentBitPos)) and 1;
     lCurrentBitPos := lCurrentBitPos + 1;
     if (lCurrentBitPos = 8) then begin
        lRawPos := 1+lRawPos;
        lCurrentBitPos := 0;
     end;
end; //nested proc ReadBit

(*
START Disabled Procedures
// These functions are not used: these routines have been inlined (following VTune profiling)
// but they are useful utilities if you want to explore Huffman Tables
function ReadBits2_9 ( lNum: integer): integer; //lNum: bits to read, not to exceed 9
//wo Advance: does not increment the Byte/Bit position. Use AdvanceBitPos to do this
begin
     result := lRawRA[lRawPos];
     result := result shl 8 + lRawRA[lRawPos+1];
     //result := result shl 8 + lRawRA[lRawPos+2];
     result := (result shr (16-lCurrentBitPos-lNum)) and lBitMask[lNum]; //lCurrentBitPos is incremented from 1, so -1
     lCurrentBitPos := lCurrentBitPos + lNum;
     if (lCurrentBitPos > 7) then begin
        lRawPos := lRawPos+(lCurrentBitPos shr 3{div 8});
        lCurrentBitPos := (lCurrentBitPos and 7{mod 8});
     end;
end;
procedure RetractBitPos(lNum: integer);
begin
  lCurrentBitPos := lCurrentBitPos - lNum;
  while (lCurrentBitPos < 0) do begin
    lRawPos := lRawPos - 1;
    lCurrentBitPos := lCurrentBitPos + 8;
  end;
end;
procedure AdvanceBitPos(lNum: integer);
//Advances Bit/Byte counters
begin
     lCurrentBitPos := lCurrentBitPos + lNum;
     if (lCurrentBitPos > 7) then begin
        lRawPos := lRawPos+(lCurrentBitPos shr 3{div 8});
        lCurrentBitPos := (lCurrentBitPos and 7{mod 8});
     end;
end;
END Disabled Procedures*)

function ReadBits ( lNum: integer): integer; //lNum: bits to read, not to exceed 16
begin
     result := lRawRA[lRawPos];
     result := result shl 8 + lRawRA[lRawPos+1];
     result := result shl 8 + lRawRA[lRawPos+2];
     result := (result shr (24-lCurrentBitPos-lNum)) and lBitMask[lNum]; //lCurrentBitPos is incremented from 1, so -1
     lCurrentBitPos := lCurrentBitPos + lNum;
     if (lCurrentBitPos > 7) then begin
        lRawPos := lRawPos+(lCurrentBitPos shr 3{div 8});
        lCurrentBitPos := (lCurrentBitPos and 7{mod 8});
     end;
end; //nested proc ReadBits

function DecodePixelDifference( lFrame: integer): integer;//Red/Green/Blue each a separate 'Frame': can have unique huffman tables
var
   lByte,lHufValSSSS,lInput,lInputbits,lDiff,lI: integer;
begin
  // read one byte from the stream, without modifying the pointer
  lByte := (lRawRA[lRawPos] shl lCurrentBitPos) + (lRawRA[lRawPos+1] shr (8-lCurrentBitPos));
  lByte := lByte and 255;
  lHufValSSSS := lLookUpRA[lFrame,lByte];
  //lLookUpRA: array [1..kMaxFrames,0..255] of byte; //lists all possible SSSS with <= 8bits
  if lHufValSSSS < 255 then begin
     lCurrentBitPos := lSSSSszRA[lFrame,lHufValSSSS] + lCurrentBitPos;
     lRawPos := lRawPos + (lCurrentBitpos shr 3);
     lCurrentBitpos := lCurrentBitpos and 7;
     //AdvanceBitPos(lSSSSszRA[lFrame,lSSSS]), but inlined;
  end else begin //full SSSS is not in the first 8-bits
   //if (lByte < 0) or (lByte > 255) then showmessage('yikes: this is impossible');
   lInput := lByte;
   lInputBits := 8;
   inc(lRawPos); // forward 8 bits = precisely 1 byte
   repeat
      Inc(lInputBits);
      lInput := lInput shl 1 + ReadBit;
      if DHTLiRA[lFrame,lInputBits] <> 0 then begin //if any entires with this length
         for lI := DHTstartRA[lFrame,lInputBits] to (DHTstartRA[lFrame,lInputBits]+DHTLiRA[lFrame,lInputBits]-1) do begin
            if (lInput = lHufRA[lFrame,lI].HufCode) then
             lHufValSSSS := lHufRA[lFrame,lI].HufVal;
         end; //check each code
      end; //if any entires with this length
      if (lInputBits >= lMaxHufSi[lFrame]) and (lHufValSSSS > 254) then begin//exhausted options CR: added rev13
         lHufValSSSS := lMaxHufVal[lFrame]; 
      end;
   until (lHufValSSSS < 255){found};
  end; //answer in first 8 bits
  //The HufVal is referred to as the SSSS in the Codec, so it is called 'lHufValSSSS'
  case lHufValSSSS of
          0: result:= 0;
          1: if ReadBit = 0 then result := -1 else result := 1;
          (*BELOW only a tiny bit faster to separate 2..15 into 2..9 and 10..15, requires extra procedure and more
          2..9: begin //see 10..15 for explanation
                     lDiff := ReadBits2_9(lHufValSSSS);
                     if (lDiff > (lBitMask[lHufValSSSS-1])) then  //add
                        result := lDiff
                     else //negation
                          result := lDiff - lBitMask[lHufValSSSS];
                end; //2..9 *)
          2..15: begin
          //Osiris includes extra bits after SSSS=16...a violation of the standard See "TABLE H.2 - Difference categories for lossless Huffman coding" of the codec ITU-T81
          //According to the Codec H.1.2.2 "No extra bits are appended after SSSS = 16 is encoded."
          //To patch for Osiris Change case from 2..15 to 2..16
          //  This will work for Osiris images, but will break non-Osiris images
              lDiff := ReadBits(lHufValSSSS);
              if (lDiff > (lBitMask[lHufValSSSS-1])) then  //add
                  result := lDiff
                  // this is slightly unintuitive: the positive bit is identical to the offset shown in TABLE H.2, a slower but more intuitive way to do this is:
                  //result := (lDiff and lBitMask[lHufVal-1]) + (1 shl (lHufval-1));
                  //where you clip off the sign bit and then SHL appropriately
              else //negation
                 result := lDiff - lBitMask[lHufValSSSS];
                 //NEXT to lines are a bit more intuitive:
                 {lDiff := lBitMask[lHufVal-1]- lDiff;
                 result := -(lDiff + (1 shl (lHufval-1)));}//negation
          end; //10..15
          else {16, not osiris}
               result := 32768;
  end; //case HuffVal
end; //nested proc DecodePixelDifference

procedure ReadByte(var lByte: integer);
begin
     inc(lRawPos);
     lByte := lRawRA[lRawPos];
end;   //nested proc ReadByte

function ReadWord: word;
var
   lbtL1, lbtL2: byte;
begin
     inc(lRawPos);
     lbtL1 := lRawRA[lRawPos];
     inc(lRawPos);
     lbtL2 := lRawRA[lRawPos];
     result := (256 * lbtL1 + lbtL2)
end; //nested proc ReadWord
//NEXT: main procedure
  begin
    lAbba := 4;
    lnHufTables := 0;
    lRawSz := lCptSize;
    lRawPos := 0;
    lRestartSegmentSz := 0;
    lImgTypeC3 := false;
    SOFxdim:= 1;
    if lRawSz < 32 then goto 666;
    for lFrameCount := 1 to kMaxFrames do
      for lInc := 1 to 16 do
        DHTstartRA[lFrameCount,lInc] := 0;
    SOFydim := 1;
    SOSpttrans := 0;
    lHdrOK := false;
    SOFnf := 0;
    SOSns := 0;
    GetMem( lRawRA, lRawSz);
{$IFDEF Stream}
    lStream.Seek(lCptPosition, soFromBeginning);
    lStream.readBuffer(lRawRA^, lRawSz);
{$ELSE}
    Seek(infp,lCptPosition);
    BlockRead(infp, lRawRA^, lRawSz);
{$ENDIF}
    ReadByte(btS1);
    ReadByte(btS1);
    repeat
      repeat
            if lRawPos <= lRawSz then ReadByte(btS1);
            if btS1 <> $FF then begin
               goto 666;
            end;
            if lRawPos <= lRawSz then  ReadByte( btMarkerType);
            case btMarkerType of //only process segments with length fields
                 $0,$1,$D0..$D7,$FF: btMarkerType := 0; //0&FF = fillers, $1=TEM,$D0..D7=resync
            end;
      until (lRawPos >= lRawSz) or (btMarkerType <> 0);
      lSegmentLength := ReadWord;
      lSegmentEnd := lRawPos+(lSegmentLength - 2);
      if lSegmentEnd > lRawSz then goto 666;
      if (btMarkerType = $C3)  then
         lImgTypeC3 := true;
      if lverbose then dcmMsg( {result+}inttohex(btMarkerType,2){':'+inttostr( lSegmentLength )+'@'+inttostr(positon)+' '});
      case btMarkerType of
           $0: ; //filler - ignore
           $C0..$C3,$C5..$CB,$CD..$CF: begin //read SOF FrameHeader
             ReadByte(SOFprecision);
             SOFydim := ReadWord;
             SOFxdim:= ReadWord;
             ReadByte(SOFnf);
             if lverbose then dcmMsg('[precision:'+inttostr(SOFprecision)+' X*Y:'+inttostr(SOFxdim)+'*'+inttostr(SOFydim)+'nFrames:'+inttostr(SOFnf)+'] ');
             if (not lImgTypeC3) or ((SOFnf <> 1) and (SOFnf <> 3)) then begin
                 dcmMsg('Unable to extract this file format.');
             end;
             SOFarrayPos := lRawPos;
             lRawPos := (lSegmentEnd);
           end; //SOF FrameHeader
           $C4: begin //DHT Huffman
              if lverbose then dcmMsg( 'HuffmanLength'+inttostr(lSegmentLength)+':');
            //if SOFnf <1 then SOFnf := 1; //we may not know SOFnf yet!
            lFrameCount := 1;
            repeat
             ReadByte( DHTtcth);
             //showmessage(inttostr(lFrameCount)+'@'+inttostr(DHTtcth and 15)+'x'+inttostr(DHTtcth ));
             DHTnLi := 0;
             for lInc := 1 to 16 do begin
                 ReadByte(DHTliRA[lFrameCount,lInc]);
                 DHTnLi := DHTnLi +  DHTliRA[lFrameCount,lInc];
                 if DHTliRA[lFrameCount,lInc] <> 0 then lMaxHufSi[lFrameCount] := lInc;
                 //showmessage(inttostr(DHTliRA[lFrameCount,lInc])+'@'+inttostr(lMaxHufSi));
             end;
             if DHTnLi > 17 then begin
                dcmMsg('Huffman table corrupted.');
                goto 666;
             end;
             lIncY := 0; //frequency

             for lInc := 0 to 31 do begin
               lHufRA[lFrameCount, lInc].HufVal := -1;
               lHufRA[lFrameCount, lInc].HufSz := -1;
               lHufRA[lFrameCount, lInc].HufCode := -1;
             end;

             for lInc := 1 to 16 do begin //set the huffman size values
                 if DHTliRA[lFrameCount,lInc]> 0 then begin
                     DHTstartRA[lFrameCount,lInc] := lIncY+1;
                     for lIncX := 1 to DHTliRA[lFrameCount,lInc] do begin
                         inc(lIncY);
                         ReadByte(btS1);
                         lHufRA[lFrameCount,lIncY].HufVal := btS1;
                         lMaxHufVal[lFrameCount] := btS1;
                         if (btS1 >= 0) and (btS1 <= 16) then
                           lHufRA[lFrameCount,lIncY].HufSz := lInc
                         else begin
                            dcmMsg('Huffman size array corrupted.');
                            goto 666;
                         end; {}
                     end;
                 end; //Length of size lInc > 0
             end;
             //showmessage('Max bits:'+inttostr(lMaxHufSi)+' SSSS:'+inttostr(lMaxHufVal));
             K := 1;
             Code := 0;
             Si := lHufRA[lFrameCount,K].HufSz;//HuffSizeRA[1];
             repeat
                   while (Si = lHufRA[lFrameCount,K].HufSz) do begin
                         lHufRA[lFrameCount,K].HufCode := Code;
                         //showmessage('bits: '+inttostr(Si)+' NthEntry:'+inttostr(K)+' Code:'+inttostr(Code));
                         Code := Code + 1;
                         Inc(K);
                   end;
                   if K <= DHTnLi then begin
                      while lHufRA[lFrameCount,K].HufSz > Si do begin
                            Code := Code Shl 1;
                            Si := Si + 1;
                      end; //while Si
                   end; //K <= 17
             until K > DHTnLi;// JGS added rev13
             inc(lFrameCount);
            until (lSegmentEnd-lRawPos) < 18;
            lnHufTables := lFrameCount - 1;
            //showmessage(inttostr(lnHufTables));
            lRawPos := (lSegmentEnd);
           end; //$C4: DHT Huffman
            $DD: begin  //Define Restart
               lRestartSegmentSz := Readword;
               lRawPos := (lSegmentEnd);
           end;
           $DA: begin //read SOS Scan Header
             if SOSns > 0 then goto 666; //multiple SOS!
             ReadByte(SOSns);
             //if Ns = 1 then NOT interleaved, else interleaved: see B.2.3
             SOSarrayPos := lRawPos;
             if SOSns > 0 then begin
                 for lInc := 1 to SOSns do begin
                     ReadByte( btS1); //component identifier 1=Y,2=Cb,3=Cr,4=I,5=Q
                     ReadByte(btS2); //horizontal and vertical sampling factors
                 end;
             end;
             ReadByte(SOSss); //predictor selection B.3
             ReadByte( SOSse);
             ReadByte( SOSahal); //lower 4bits= pointtransform
             SOSpttrans := SOSahal and 16;
             if lverbose then
                dcmMsg('[Predictor: '+inttostr(SOSss)+' PointTransform:'+inttostr(SOSahal)+'] ');
             lRawPos := (lSegmentEnd);
           end; //$DA SOS - Scan Header
           else begin //skip marker segment;
                lRawPos := (lSegmentEnd);
           end;
      end; //case markertype
    until (lRawPos >= lRawSz) or (btMarkerType = $DA); {hexDA=Start of scan}
    lHdrOK := true; //errors goto label 666, so are NOT OK
    lImgStart := lRawPos;
666:
    if not lHdrOK then begin
       dcmMsg('Unable to read this file - is this really a JPEG image?');
       goto 123;
    end;
    if (not lImgTypeC3) then
      goto 123; //lossless compressed huffman tables
    //NEXT: unpad data - delete byte that follows $FF
    lINc := lRawPos;
    lIncX := lRawPos;
    repeat
          lRawRA[lIncX] := lRawRA[lInc];
          if lRawRA[lInc] = 255 then begin
             if (lRawRA[lInc+1] = $00) then
                 lInc := lInc+1
             else begin
                 //showmessage(inttostr(lRawRA[lInc+1]));
                 if (lRawRA[lInc+1] = $D9) then //end of image
                    lIncX := -666; //end of padding
             end;
          end;
          inc(lInc);
          inc(lIncX);
    until lIncX < 0;
    //End: Data unpadding
    //NEXT: Create Huffman LookupTable.
    //We will compute all possible outcomes for an 8-bit value, while less intuitive than
    //reading Huffman 1 bit at a time, it doubles the decompression speed
    lBitMask[1]:= 1;
    lBitMask[2]:= 3;
    lBitMask[3]:= 7;
    lBitMask[4]:= 15;
    lBitMask[5]:= 31;
    lBitMask[6]:= 63;
    lBitMask[7]:= 127;
    lBitMask[8]:= 255;
    lBitMask[9]:= 511;
    lBitMask[10]:= 1023;
    lBitMask[11]:= 2047;
    lBitMask[12]:= 4095;
    lBitMask[13]:= 8191;
    lBitMask[14]:= 16383;
    lBitMask[15]:= 32767;
    lBitMask[16]:= 65535;
    lBitMask[17]:= 131071; //ONLY required for Osiris corrupted images, see DecodePixelDifference for details
    //NEXT: some RGB images use only a single Huffman table for all 3 colour planes. In this case, replicate the correct values
    if (lnHufTables < SOFnf) then begin //use single Hufman table for each frame
       //showmessage('generating tables'+inttostr(SOFnf));
       if lnHufTables < 1 then begin
           dcmMsg('Lossless JPEG decoding error: no Huffman tables.');
           goto 123;
       end;
       for lFrameCount := 2 to SOFnf do begin
           for lInc := 1 to 16 do
               DHTstartRA[lFrameCount,lInc] := DHTstartRA[1,lInc];
           for lInc := 0 to 31 do begin
               lHufRA[lFrameCount,lInc].HufCode := lHufRA[1,lInc].HufCode;
               lHufRA[lFrameCount,lInc].HufVal := lHufRA[1,lInc].HufVal;
               lHufRA[lFrameCount,lInc].HufSz := lHufRA[1,lInc].HufSz;
               DHTliRA[lFrameCount,lInc] := DHTliRA[1,lInc];
           end; //for each table entry
       end; //for each frame                                           xx
    end;// if lnHufTables < SOFnf
    for lFrameCount := 1 to  kMaxFrames do
      for lInc := 0 to 17 do
          lSSSSszRA[lFrameCount,lInc] := 123; //Impossible value for SSSS, suggests 8-bits can not describe answer
    for lFrameCount := 1 to  kMaxFrames do
      for lInc := 0 to 255 do
          lLookUpRA[lFrameCount,lInc] := 255; //Impossible value for SSSS, suggests 8-bits can not describe answer
    //NEXT fill lookuptable
    for lFrameCount := 1 to  SOFnf do begin
      lIncY := 0;
      for lSz := 1 to 8 do begin //set the huffman lookup table for keys with lengths <=8
        if DHTliRA[lFrameCount,lSz]> 0 then begin
           for lIncX := 1 to DHTliRA[lFrameCount,lSz] do begin
                         inc(lIncY);
                         lHufVal := lHufRA[lFrameCount,lIncY].HufVal; //SSSS
                         {if (lHufVal < 0) or (lHufVal > 17) then begin
                             showmessage('Unknown SSSS =' +inttostr(lHufVal));
                             lHufVal := 16;
                         end; }
                         lSSSSszRA[lFrameCount,lHufVal] := lSz;
                         k := (lHufRA[lFrameCount,lIncY].HufCode shl (8-lSz )) and 255; //K= most sig bits for hufman table
                         if lSz < 8 then begin //fill in all possible bits that exceed the huffman table
                              lInc := lBitMask[8-lSz];
                              for lCurrentBitPos := 0 to lInc do begin
                                 lLookUpRA[lFrameCount,k+lCurrentBitPos] := lHufVal;
                              end;
                         end else
                             lLookUpRA[lFrameCount,k] := lHufVal; //SSSS

                         {Showmessage('Frame ' + inttostr(lFrameCount) + ' SSSS= '+inttostr(lHufRA[lFrameCount,lIncY].HufVal)+
                            '  Size= '+inttostr(lHufRA[1,lIncY].HufSz)+
                            '  Code= '+inttostr(lHufRA[1,lIncY].HufCode)+
                            '  SHL Code= '+inttostr(k)+
                            '  EmptyBits= '+inttostr(lInc)); {}
           end; //Set SSSS
        end; //Length of size lInc > 0
      end; //for lInc := 1 to 8
    end; //For each frame, e.g. once each for Red/Green/Blue
    //Next: uncompress data: different loops for different predictors
    SOFxdim:= SOFnf*SOFxdim;
    lItems :=  SOFxdim*SOFydim;
    //if lVerbose then showmessage('precision'+inttostr(SOFprecision));
    //for timing, multiple decoding loops lRawAbba := lRawPos;for lLoopsAbba := 1 to 100 do begin lRawPos := lRawAbba;
       //if (lRestartSegmentSz > 0) and ((SOFPrecision<> 8) or (SOSss = 7)) then //add restart support if we ever find any samples to test
       //   showmessage('This image uses restart markers. Please contact the author. Predictor:Precision '+inttostr(SOSss)+':'+inttostr(SOFPrecision));
       inc(lRawPos);//abbax
       lCurrentBitPos := 0; //read in a new byte
       //lCurrentBitPos := 1; //read in a new byte
       lItems :=  SOFxdim*SOFydim;
       lPredicted :=  1 shl (SOFPrecision-1-SOSpttrans);
       lInc := 0;
       if (SOFPrecision<> 8) then begin //start - 16 bit data
          lImgRA := @lOutSmallRA[0];{set to 1 for MRIcro, else 0}
          FillChar(lImgRA^,lItems*sizeof(word), 0); //zero array
          lPredB:= 0;
          lPredC := 0;
          case SOSss of //predictors 1,2,3 examine single previous pixel, here we set the relative location
                   2: lPredA:= SOFxDim-1; //Rb directly above
                   3: lPredA:= SOFxDim; //Rc UpperLeft:above and to the left
                   4,5: begin
                      lPredA := 0;
                      lPredB := SOFxDim-1; //Rb directly above
                      lPredC:= SOFxDim; //Rc UpperLeft:above and to the left
                   end;
                   6: begin
                      lPredB := 0;
                      lPredA := SOFxDim-1; //Rb directly above
                      lPredC:= SOFxDim; //Rc UpperLeft:above and to the left
                   end;
                   else lPredA := 0; //Ra: directly to left
          end; //case SOSss: predictor offset
          for lIncX := 1 to SOFxdim do begin
              inc(lInc); //writenext voxel
              if lInc > 1 then lPredicted := lImgRA[lInc-1];
              lImgRA[lInc] := lPredicted+DecodePixelDifference(1);
          end; //first line: use prev voxel prediction;
         if lRestartSegmentSz = 0 then begin
             for lIncY := 2 to SOFyDim do begin
                 inc(lInc); //write next voxel
                 lPredicted := lImgRA[lInc-SOFxdim];
                 lImgRA[lInc] := lPredicted+DecodePixelDifference(1);
                 if SOSss = 4 then begin
                    for lIncX := 2 to SOFxdim do begin
                         lPredicted := lImgRA[lInc-lPredA]+lImgRA[lInc-lPredB]-lImgRA[lInc-lPredC];
                         inc(lInc); //writenext voxel
                         lImgRA[lInc] := lPredicted+DecodePixelDifference(1);
                    end; //for lIncX
                 end else if (SOSss = 5) or (SOSss = 6) then begin
                    for lIncX := 2 to SOFxdim do begin
                         lPredicted := lImgRA[lInc-lPredA]+ ((lImgRA[lInc-lPredB]-lImgRA[lInc-lPredC]) shr 1);
                         inc(lInc); //writenext voxel
                         lImgRA[lInc] := lPredicted+DecodePixelDifference(1);
                    end; //for lIncX
                 end else if SOSss = 7 then begin
                    for lIncX := 2 to SOFxdim do begin
                        inc(lInc); //writenext voxel
                        lPredicted := (lImgRA[lInc-1]+lImgRA[lInc-SOFxdim]) shr 1;
                        lImgRA[lInc] := lPredicted+DecodePixelDifference(1);
                    end; //for lIncX
                 end else begin //SOSss 1,2,3 read single values
                     for lIncX := 2 to SOFxdim do begin
                         lPredicted := lImgRA[lInc-lPredA];
                         inc(lInc); //writenext voxel
                         lImgRA[lInc] := lPredicted+DecodePixelDifference(1);
                     end; //for lIncX
                 end;  //SOSss predictor


             end; //for lIncY
         end {RestartSegmentSz = 0} else begin {restartsegment}
             if SOSss > 3 then
                dcmMsg('Unusual 16-bit lossless JPEG with restart segments. Please contact the author:'+inttostr(SOSss));
             lSegmentEnd := lRestartSegmentSz;
             repeat
                    if lSegmentEnd > lItems then lSegmentEnd := lItems;
                    lLineStart := (((lInc div SOFxDim)+1)* SOFxDim){-1};
                    if lInc > (SOFxDim+1) then
                       lPredicted :=  1 shl (SOFPrecision-1-SOSpttrans)
                    else
                        lPredicted := lImgRA[lInc-SOFxdim];

                    for lInc := lInc to (lSegmentEnd-1) do begin
                      lImgRA[lInc] := lPredicted+DecodePixelDifference(1);
                      if lInc+1 = lLineStart then begin//newline
                        lPredicted := lImgRA[lInc+1-SOFxdim];
                        lLineStart := lLineStart + SOFxDim;
                      end else
                        lPredicted := lImgRA[lInc-lPredA];
                    end;
                    if (lSegmentEnd+1) < lItems then begin
                        dec(lRawPos);
                        repeat
                              while (lRawRA[lRawPos] <> 255) do
                                    inc(lRawPos);
                              inc(lRawPos);
                        until (lRawRA[lRawPos] >= $D0) and (lRawRA[lRawPos] <= $D7);
                        lCurrentBitPos := 0; //read in a new byte
                        inc(lRawPos);//abbax
                    end;
                    lSegmentEnd := lSegmentEnd + lRestartSegmentSz;
              until (lRestartSegmentSz < 1) or ((lSegmentEnd-2) > lItems);
             end; //restartsegments
       end else if SOFnf = 3 then begin //>8bit data; 8 bit follows
              //LOSSLESS JPEG: 7 possible predictors - we will handle all of them
              lPredB:= 0;
              lPredC := 0;
              case SOSss of //predictors 1,2,3 examine single previous pixel, here we set the relative location
                   2: lPredA:= SOFxDim-3; //Rb directly above
                   3: lPredA:= SOFxDim; //Rc UpperLeft:above and to the left
                   5: begin
                      lPredA := 0;
                      lPredB := SOFxDim-3; //Rb directly above
                      lPredC:= SOFxDim; //Rc UpperLeft:above and to the left
                   end;
                   6: begin
                      lPredB := 0;
                      lPredA := SOFxDim-3; //Rb directly above
                      lPredC:= SOFxDim; //Rc UpperLeft:above and to the left
                   end;
                   else lPredA := 0; //Ra: directly to left
              end; //case SOSss: predictor offset
              lPredictedG := lPredicted;
              lPredictedB := lPredicted;
              lOffset := 0;
              lInc := lOffset;
              for lIncX := 1 to (SOFxdim div 3) do begin //write first line
                  //DecodePixelDifference=RED
                  lImgRAz[lInc] := lPredicted+DecodePixelDifference(1);
                  lPredicted := lImgRAz[lInc];
                  inc(lInc); //writenext voxel
                  //DecodePixelDifference=GREEN
                  lImgRAz[lInc] := lPredictedG+DecodePixelDifference(2);
                  lPredictedG := lImgRAz[lInc];
                  inc(lInc); //writenext voxel
                  //DecodePixelDifference=BLUE
                  lImgRAz[lInc] := lPredictedB+DecodePixelDifference(3);
                  lPredictedB := lImgRAz[lInc];
                  inc(lInc); //writenext voxel
              end; //first line: use prev voxel prediction;
              if lRestartSegmentSz = 0 then lSegmentEnd := lItems
              else lSegmentEnd := lRestartSegmentSz;
              repeat
                    if lSegmentEnd > lItems then lSegmentEnd := lItems;
                    lLineStart := (((lInc div SOFxDim)+1)* SOFxDim)+lOffset{-1};
                     if lInc > (SOFxDim+1) then begin
                       lPredicted :=  1 shl (SOFPrecision-1-SOSpttrans);
                       lPredictedG :=  lPredicted;
                       lPredictedB :=  lPredicted;
                    end else begin
                        lPredicted := lImgRAz[lInc-SOFxdim+lOffset];
                        lPredictedG := lImgRAz[1+lInc-SOFxdim+lOffset];
                        lPredictedB := lImgRAz[2+lInc-SOFxdim+lOffset];
                    end;
               if SOSss = 4 then  begin //predictor = 4
                    //this is a 24-bit image, so for 512-pixel wid image, SOFxdim will be (3*512=) 1536
                    while lInc < (lSegmentEnd-1) do begin
                      lImgRAz[lInc] := lPredicted+DecodePixelDifference(1); //RED
                      inc(lInc);
                      lImgRAz[lInc] := lPredictedG+DecodePixelDifference(2); //GREEN
                      inc(lInc);
                      lImgRAz[lInc] := lPredictedB+DecodePixelDifference(3); //BLUE
                      inc(lInc);
                      if lInc = lLineStart then begin//newline
                         lPredicted := lImgRAz[lInc-SOFxdim];
                         lPredictedG := lImgRAz[lInc-SOFxdim+1];
                         lPredictedB := lImgRAz[lInc-SOFxdim+2];
                         lLineStart := lLineStart + (SOFxDim);
                      end else begin
                        lPredicted :=  lImgRAz[lInc-3]+lImgRAz[lInc-3-(SOFxDim-3)]-lImgRAz[lInc-3-SOFxDim];
                        lPredictedG := lImgRAz[lInc-2]+lImgRAz[lInc-2-(SOFxDim-3)]-lImgRAz[lInc-2-SOFxDim];
                        lPredictedB := lImgRAz[lInc-1]+lImgRAz[lInc-1-(SOFxDim-3)]-lImgRAz[lInc-1-SOFxDim];
                      end;
                    end;
               //xxx
               end else if (SOSss = 5) or (SOSss = 6) then  begin //predictor = 5 or 6
                    //this is a 24-bit image, so for 512-pixel wid image, SOFxdim will be (3*512=) 1536
                    while lInc < (lSegmentEnd-1) do begin
                      lImgRAz[lInc] := lPredicted+DecodePixelDifference(1); //RED
                      inc(lInc);
                      lImgRAz[lInc] := lPredictedG+DecodePixelDifference(2); //GREEN
                      inc(lInc);
                      lImgRAz[lInc] := lPredictedB+DecodePixelDifference(3); //BLUE
                      inc(lInc);
                      if lInc = lLineStart then begin//newline
                         lPredicted := lImgRAz[lInc-SOFxdim];
                         lPredictedG := lImgRAz[lInc-SOFxdim+1];
                         lPredictedB := lImgRAz[lInc-SOFxdim+2];
                         lLineStart := lLineStart + (SOFxDim);
                      end else begin
                        lPredicted :=  lImgRAz[lInc-3-lPredA]+((lImgRAz[lInc-3-lPredB]-lImgRAz[lInc-3-lPredC])shr 1);
                        lPredictedG := lImgRAz[lInc-2-lPredA]+((lImgRAz[lInc-2-lPredB]-lImgRAz[lInc-2-lPredC])shr 1);
                        lPredictedB := lImgRAz[lInc-1-lPredA]+((lImgRAz[lInc-1-lPredB]-lImgRAz[lInc-1-lPredC])shr 1);
                      end;
                    end;
               end else if SOSss = 7 then  begin //predictor = 7
                    while lInc < (lSegmentEnd-1) do begin
                      lImgRAz[lInc] := lPredicted+DecodePixelDifference(1); //RED
                      inc(lInc);
                      lImgRAz[lInc] := lPredictedG+DecodePixelDifference(2); //GREEN
                      inc(lInc);
                      lImgRAz[lInc] := lPredictedB+DecodePixelDifference(3); //BLUE
                      inc(lInc);
                      if lInc = lLineStart then begin//newline
                         lPredicted := lImgRAz[lInc-SOFxdim];
                         lPredictedG := lImgRAz[lInc-SOFxdim+1];
                         lPredictedB := lImgRAz[lInc-SOFxdim+2];
                         lLineStart := lLineStart + (SOFxDim);
                      end else begin
                        lPredicted :=  (lImgRAz[lInc-3]+lImgRAz[lInc-3-(SOFxDim-3)])shr 1;
                        lPredictedG := (lImgRAz[lInc-2]+lImgRAz[lInc-2-(SOFxDim-3)]) shr 1;
                        lPredictedB := (lImgRAz[lInc-1]+lImgRAz[lInc-1-(SOFxDim-3)]) shr 1;
                      end;
                    end;

               end else begin //predictor in range 1,2,3
                    //this is a 24-bit image, so for 512-pixel wid image, SOFxdim will be (3*512=) 1536
                    while lInc < (lSegmentEnd-1) do begin
                      lImgRAz[lInc] := lPredicted+DecodePixelDifference(1); //RED
                      inc(lInc);
                      lImgRAz[lInc] := lPredictedG+DecodePixelDifference(2); //GREEN
                      inc(lInc);
                      lImgRAz[lInc] := lPredictedB+DecodePixelDifference(3); //BLUE
                      inc(lInc);
                      if lInc = lLineStart then begin//newline
                         lPredicted := lImgRAz[lInc-SOFxdim];
                         lPredictedG := lImgRAz[lInc-SOFxdim+1];
                         lPredictedB := lImgRAz[lInc-SOFxdim+2];
                         lLineStart := lLineStart + (SOFxDim);
                      end else begin
                        lPredicted := lImgRAz[lInc-3-lPredA];
                        lPredictedG := lImgRAz[lInc-2-lPredA];
                        lPredictedB := lImgRAz[lInc-1-lPredA];
                      end;
                    end;
               end; //predictor <> 7
              until (lRestartSegmentSz < 1) or ((lSegmentEnd-2) > lItems);
          // end; //8<>15data type
       end else begin //previously 12/16/24bit data, 8 bit follows
              lInc := 0;
              //LOSSLESS JPEG: 7 possible predictors - we will handle all of them
              lPredB:= 0;
              lPredC := 0;
              case SOSss of //predictors 1,2,3 examine single previous pixel, here we set the relative location
                   2: lPredA:= SOFxDim-1; //Rb directly above
                   3: lPredA:= SOFxDim; //Rc UpperLeft:above and to the left
                   5: begin
                      lPredA := 0;
                      lPredB := SOFxDim-1; //Rb directly above
                      lPredC:= SOFxDim; //Rc UpperLeft:above and to the left
                   end;
                   6: begin
                      lPredB := 0;
                      lPredA := SOFxDim-1; //Rb directly above
                      lPredC:= SOFxDim; //Rc UpperLeft:above and to the left
                   end;
                   else lPredA := 0; //Ra: directly to left
              end; //case SOSss: predictor offset
              //lOffset := -1;
              for lIncX := 1 to SOFxdim do begin //write first line
                  lImgRAz[lInc] := lPredicted+DecodePixelDifference(1);
                  inc(lInc); //writenext voxel
                  lPredicted := lImgRAz[lInc-1];
              end; //first line: use prev voxel prediction;
              if lRestartSegmentSz = 0 then lSegmentEnd := lItems
              else lSegmentEnd := lRestartSegmentSz;
              repeat
                    if lSegmentEnd > lItems then lSegmentEnd := lItems;
                    lLineStart := (((lInc div SOFxDim)+1)* SOFxDim){-1};
                    if lInc > (SOFxDim+1) then
                       lPredicted :=  1 shl (SOFPrecision-1-SOSpttrans)
                    else
                        lPredicted := lImgRAz[lInc-SOFxdim];
                if SOSss = 4 then begin //predictor 4 : ABOVE+LEFT-(UPPERLEFT)
                    for lInc := lInc to (lSegmentEnd-1) do begin
                      lImgRAz[lInc] := lPredicted+DecodePixelDifference(1);
                      if lInc+1 = lLineStart then begin//newline
                        lPredicted := lImgRAz[lInc+1-SOFxdim];
                        lLineStart := lLineStart + SOFxDim;
                      end else
                        lPredicted := lImgRAz[lInc]+lImgRAz[lInc-(SOFxDim-1)] -lImgRAz[lInc-SOFxDim] ;
                    end;

                end else if (SOSss = 5) or (SOSss=6) then begin //predictor 5,6 : comparisons
                    for lInc := lInc to (lSegmentEnd-1) do begin
                      lImgRAz[lInc] := lPredicted+DecodePixelDifference(1);
                      if lInc+1 = lLineStart then begin//newline
                        lPredicted := lImgRAz[lInc+1-SOFxdim];
                        lLineStart := lLineStart + SOFxDim;
                      end else
                        lPredicted := lImgRAz[lInc-lPredA]+((lImgRAz[lInc-lPredB]-lImgRAz[lInc-lPredC]) shr 1) ;
                    end;
               end else if SOSss = 7 then begin //predictor 7: average above and left
                    for lInc := lInc to (lSegmentEnd-1) do begin
                      lImgRAz[lInc] := lPredicted+DecodePixelDifference(1);
                      if lInc+1 = lLineStart then begin//newline
                        lPredicted := lImgRAz[lInc+1-SOFxdim];
                        lLineStart := lLineStart + SOFxDim;
                      end else
                        lPredA := lImgRAz[lInc];
                        lPredB:= lImgRAz[lInc-SOFxDim+1];//correct
                        lPredicted := (lPredA+lPredB) shr 1;
                    end;
               end else begin //predictor <> 7 : assume SOSss=1: previous
                    for lInc := lInc to (lSegmentEnd-1) do begin
                      lImgRAz[lInc] := lPredicted+DecodePixelDifference(1);
                      if lInc+1 = lLineStart then begin//newline
                        lPredicted := lImgRAz[lInc+1-SOFxdim];
                        lLineStart := lLineStart + SOFxDim;
                      end else
                        lPredicted := lImgRAz[lInc-lPredA];
                    end;
               end; //predictor <> 7
                    if (lSegmentEnd+1) < lItems then begin
                        dec(lRawPos);
                        repeat
                              while (lRawRA[lRawPos] <> 255) do
                                    inc(lRawPos);
                              inc(lRawPos);
                        until (lRawRA[lRawPos] >= $D0) and (lRawRA[lRawPos] <= $D7);
                        lCurrentBitPos := 0; //read in a new byte
                        inc(lRawPos);
                        //lCurrentBitPos := 9; //read in a new byte
                    end;
                    lSegmentEnd := lSegmentEnd + lRestartSegmentSz;
              until (lRestartSegmentSz < 1) or ((lSegmentEnd-2) > lItems);
           end; //8<>15data type
123:
    FreeMem( lRawRA); //release memory buffer
end;

end.
