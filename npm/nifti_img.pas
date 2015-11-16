unit nifti_img;
interface
{$Include ..\common\isgui.inc}
uses hdr,define_types,Classes,nifti_hdr,sysutils,dialogsx, unpm, nifti_types
{$IFDEF FPC},gzio2
{$ELSE}
,gziod
{$ENDIF}
;

{$H+}
function LoadImg(lInName: string; lImgData: SingleP; lStart, lEnd,linvox_offset,lRApos,lDataType,lVolVox: integer): boolean;

function LoadImg8(lInName: string; lImgData: ByteP; lStart, lEnd,linvox_offset,lRApos,lDataType,lVolVox: integer): boolean;


implementation


function LoadImg(lInName: string; lImgData: SingleP; lStart, lEnd,linvox_offset,lRApos,lDataType,lVolVox: integer): boolean;
var
	lvox_offset,lInc,lFSize,lP2: integer;
	lFData: file;
	lImgName: string;
        lByteP: ByteP;
        lSmallIntP: SmallIntP;
        lV,lMin,lMax: single;
begin
	result := false;

	if (lStart >= lEnd) or (lStart < 1) or (lEnd < 1) then begin
		NPMmsg('Error: LoadImg '+inttostr(lStart)+'>='+inttostr(lEnd)+' or zero');
		exit;
	end;
        if Files4D(lInName) then begin
	    lImgName := Filename4D(lInName);
            lP2 := BPP (lDataType);
            if lP2 = 0 then begin
                ShowMsg(lImgName +' is an unsupported file type');
                exit;
            end;
            lvox_offset := linvox_offset+ ((Vol4D(lInName)-1)* (lP2 * lVolVox));
        end else begin
	    lImgName := lInName;
            lvox_offset := linvox_offset;
        end;
	if UpCaseExt(lImgName) = '.HDR' then
		lImgName := changefileext(lImgName,'.img');
	lFSize := FSize(lImgName);
	if (not GzExt(lImgName)) and (lFSize < (lEnd+ lvox_offset))  then begin
		NPMmsg('Error: LoadImg '+lImgName+' FSize = '+inttostr(lFSize)+'  Expected '+inttostr(lEnd+ lvox_offset));
		exit;
	end;
	filemode := 0;
        if GzExt(lImgName) then begin
          if lDataType = kDT_UNSIGNED_CHAR then begin
            getmem(lByteP, (lEnd+1)-lStart);
	    UnGZip(lImgName,lByteP,lvox_offset+lStart-1,(lEnd+1)-lStart);
            for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := lByteP^[lInc];
            freemem(lByteP);
          end else if (lDataType = kDT_SIGNED_SHORT) then begin
            //getmem(lSmallIntP, sizeof(smallint)* ((lEnd+1)-lStart));
            getmem(lByteP, ((lEnd+1)-lStart)*sizeof(smallint));
	    UnGZip(lImgName,lByteP,lvox_offset+((lStart-1)*sizeof(smallint)),((lEnd+1)-lStart)*sizeof(smallint));
            for lInc := 1 to ((lEnd+1)-lStart) do begin
                lP2 := ((lInc-1)*2)+1 ;
                lImgData^[lRApos+lInc-1] := makesmallint(lByteP^[lP2],lByteP^[lP2+1]);
            end;
            freemem(lByteP);
          end else if (lDataType = kDT_SIGNED_INT) or (lDataType = kDT_FLOAT) then begin
            	lByteP := ByteP(@lImgData^[lRApos]);
	        UnGZip(lImgName,lByteP,lvox_offset+((lStart-1)*sizeof(single)),((lEnd+1)-lStart)*sizeof(single));

            (*getmem(lByteP, ((lEnd+1)-lStart)*sizeof(single));
	    UnGZip(lImgName,lByteP,lvox_offset+((lStart-1)*sizeof(single)),((lEnd+1)-lStart)*sizeof(single));
            for lInc := 1 to ((lEnd+1)-lStart) do begin
                lP2 := ((lInc-1)*4)+1 ;
                lImgData^[lRApos+lInc-1] := makesingle(lByteP^[lP2],lByteP^[lP2+1],lByteP^[lP2+2],lByteP^[lP2+3]);
                //lImgData^[lRApos+lInc-1] := makesingle(lByteP^[lP2+3],lByteP^[lP2+2],lByteP^[lP2+1],lByteP^[lP2]);
            end;
            freemem(lByteP);  *)
            //test range
            (*lINc := 1;
            lMin :=  lImgData^[lRApos+lInc-1];
            lMax := lMin;
            for lInc := 1 to ((lEnd+1)-lStart) do begin
                lV := lImgData^[lRApos+lInc-1];
                if lV > lMax then
                   lMax := lV;
                if lV < lMin then
                   lMin := lMax;
            end;
            MainForm.NPMmsg(inttostr(lvox_offset)+'  '+realtostr(lMin,8)+'  '+realtostr(lMax,8));  *)


            //end
            if lDataType = kDT_SIGNED_INT  then begin
             for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := conv4r4i (lImgData^[lRApos+lInc-1]);
             end else begin
                for lInc := 1 to ((lEnd+1)-lStart) do
                    if specialsingle(lImgData^[lRApos+lInc-1])then
                    lImgData^[lRApos+lInc-1] := 0;
            end;
          end else begin
              ShowMsg(lImgName + ' is an unsupported compressed data type '+inttostr(lDataType));
              exit;
          end;
        end else begin
	  assignfile(lFdata,lImgName);
          if lDataType = kDT_UNSIGNED_CHAR then begin
            getmem(lByteP, (lEnd+1)-lStart);
 	    reset(lFdata,1); //12/2010
	    seek(lFdata,lvox_offset+lStart-1);
	    BlockRead(lFdata, lByteP^, (lEnd+1)-lStart);
            for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := lByteP^[lInc];
            freemem(lByteP);
          end else if (lDataType = kDT_SIGNED_SHORT) then begin
            getmem(lSmallIntP, sizeof(smallint)* ((lEnd+1)-lStart));
 	    reset(lFdata,2);
            if (lvox_offset mod 2) <> 0 then begin
                ShowMsg('Error: this software can only read headers with dataoffsets that are divisible by 4.');
            end;
	    seek(lFdata,(lvox_offset div 2)+ (lStart-1));
	    BlockRead(lFdata, lSmallIntP^, (lEnd+1)-lStart);
            for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := lSmallIntP^[lInc];
            freemem(lSmallIntP);
          end else if (lDataType = kDT_SIGNED_INT) or (lDataType = kDT_FLOAT) then begin
            //next: 4 byte data
            reset(lFdata,4);
            if (lvox_offset mod 4) <> 0 then begin
                ShowMsg('Error: this software can only read headers with dataoffsets that are divisible by 4.');
            end;
	    seek(lFdata,(lvox_offset div 4)+ (lStart-1) );
	    BlockRead(lFdata, lImgData[lRApos], (lEnd+1)-lStart);

            if lDataType = kDT_SIGNED_INT  then begin
               for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := conv4r4i (lImgData^[lRApos+lInc-1]);
            end else begin
                for lInc := 1 to ((lEnd+1)-lStart) do
                    if specialsingle(lImgData^[lRApos+lInc-1]) then
                    lImgData^[lRApos+lInc-1] := 0;
            end;
          end else
              ShowMsg('Unsupported COMPRESSED data type '+inttostr(lDataType));
          closefile(lFdata);
        end; //not gz
	result := true;
end;

(*function LoadImgx(lInName: string; lImgData: SingleP; lStart, lEnd,linvox_offset,lRApos,lDataType,lVolVox: integer): boolean;
var
	lvox_offset,lInc,lFSize,lP2: integer;
	lFData: file;
	lImgName: string;
        lByteP: ByteP;
        lSmallIntP: SmallIntP;
        lV,lMin,lMax: single;
begin

	result := false;
	if (lStart >= lEnd) or (lStart < 1) or (lEnd < 1) then begin
		MainForm.NPMmsg('Error: LoadImg '+inttostr(lStart)+'>='+inttostr(lEnd)+' or zero');
		exit;
	end;
        if Files4D(lInName) then begin
	    lImgName := Filename4D(lInName);
            lP2 := BPP (lDataType);
            if lP2 = 0 then begin
                ShowMsg(lImgName +' is an unsupported file type');
                exit;
            end;
            lvox_offset := linvox_offset+ ((Vol4D(lInName)-1)* (lP2 * lVolVox));
        end else begin
	    lImgName := lInName;
            lvox_offset := linvox_offset;
        end;
	if UpCaseExt(lImgName) = '.HDR' then
		lImgName := changefileext(lImgName,'.img');
	lFSize := FSize(lImgName);
	if (not GzExt(lImgName)) and (lFSize < (lEnd+ lvox_offset))  then begin
		MainForm.NPMmsg('Error: LoadImg '+lImgName+' FSize = '+inttostr(lFSize)+'  Expected '+inttostr(lEnd+ lvox_offset));
		exit;
	end;
	filemode := 0;
        if GzExt(lImgName) then begin
          if lDataType = kDT_UNSIGNED_CHAR then begin
            getmem(lByteP, (lEnd+1)-lStart);
	    UnGZip(lImgName,lByteP,lvox_offset+lStart-1,(lEnd+1)-lStart);
            for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := lByteP^[lInc];
            freemem(lByteP);
          end else if (lDataType = kDT_SIGNED_SHORT) then begin
            //getmem(lSmallIntP, sizeof(smallint)* ((lEnd+1)-lStart));
            getmem(lByteP, ((lEnd+1)-lStart)*sizeof(smallint));
	    UnGZip(lImgName,lByteP,lvox_offset+((lStart-1)*sizeof(smallint)),((lEnd+1)-lStart)*sizeof(smallint));
            for lInc := 1 to ((lEnd+1)-lStart) do begin
                lP2 := ((lInc-1)*2)+1 ;
                lImgData^[lRApos+lInc-1] := makesmallint(lByteP^[lP2],lByteP^[lP2+1]);
            end;
            freemem(lByteP);
          end else if (lDataType = kDT_SIGNED_INT) or (lDataType = kDT_FLOAT) then begin
            	lByteP := ByteP(@lImgData^[lRApos]);
	        UnGZip(lImgName,lByteP,lvox_offset+((lStart-1)*sizeof(single)),((lEnd+1)-lStart)*sizeof(single));

            {getmem(lByteP, ((lEnd+1)-lStart)*sizeof(single));
	    UnGZip(lImgName,lByteP,lvox_offset+((lStart-1)*sizeof(single)),((lEnd+1)-lStart)*sizeof(single));
            for lInc := 1 to ((lEnd+1)-lStart) do begin
                lP2 := ((lInc-1)*4)+1 ;
                lImgData^[lRApos+lInc-1] := makesingle(lByteP^[lP2],lByteP^[lP2+1],lByteP^[lP2+2],lByteP^[lP2+3]);
                //lImgData^[lRApos+lInc-1] := makesingle(lByteP^[lP2+3],lByteP^[lP2+2],lByteP^[lP2+1],lByteP^[lP2]);
            end;
            freemem(lByteP);}
            //test range
            {lINc := 1;
            lMin :=  lImgData^[lRApos+lInc-1];
            lMax := lMin;
            for lInc := 1 to ((lEnd+1)-lStart) do begin
                lV := lImgData^[lRApos+lInc-1];
                if lV > lMax then
                   lMax := lV;
                if lV < lMin then
                   lMin := lMax;
            end;
            MainForm.NPMmsg(inttostr(lvox_offset)+'  '+realtostr(lMin,8)+'  '+realtostr(lMax,8)); }


            //end
            if lDataType = kDT_SIGNED_INT  then begin
             for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := conv4r4i (lImgData^[lRApos+lInc-1]);
             end else begin
                for lInc := 1 to ((lEnd+1)-lStart) do
                    if specialsingle(lImgData^[lRApos+lInc-1])then
                    lImgData^[lRApos+lInc-1] := 0;
            end;
          end else begin
              ShowMsg(lImgName + ' is an unsupported compressed data type '+inttostr(lDataType));
              exit;
          end;
        end else begin
	  assignfile(lFdata,lImgName);
          if lDataType = kDT_UNSIGNED_CHAR then begin
            getmem(lByteP, (lEnd+1)-lStart);
 	    reset(lFdata,1);
	    seek(lFdata,lvox_offset+lStart-1);
	    BlockRead(lFdata, lByteP^, (lEnd+1)-lStart);
            for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := lByteP^[lInc];
            freemem(lByteP);
          end else if (lDataType = kDT_SIGNED_SHORT) then begin
            getmem(lSmallIntP, sizeof(smallint)* ((lEnd+1)-lStart));
 	    reset(lFdata,2);
            if (lvox_offset mod 2) <> 0 then begin
                ShowMsg('Error: this software can only read headers with dataoffsets that are divisible by 4.');
            end;
	    seek(lFdata,(lvox_offset div 2)+ (lStart-1));
	    BlockRead(lFdata, lSmallIntP^, (lEnd+1)-lStart);
            for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := lSmallIntP^[lInc];
            freemem(lSmallIntP);
          end else if (lDataType = kDT_SIGNED_INT) or (lDataType = kDT_FLOAT) then begin
            //next: 4 byte data
            reset(lFdata,4);
            if (lvox_offset mod 4) <> 0 then begin
                ShowMsg('Error: this software can only read headers with dataoffsets that are divisible by 4.');
            end;
	    seek(lFdata,(lvox_offset div 4)+ (lStart-1) );
	    BlockRead(lFdata, lImgData[lRApos], (lEnd+1)-lStart);

            if lDataType = kDT_SIGNED_INT  then begin
               for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := conv4r4i (lImgData^[lRApos+lInc-1]);
            end else begin
                for lInc := 1 to ((lEnd+1)-lStart) do
                    if specialsingle(lImgData^[lRApos+lInc-1]) then
                    lImgData^[lRApos+lInc-1] := 0;
            end;
          end else
              ShowMsg('Unsupported COMPRESSED data type '+inttostr(lDataType));
          closefile(lFdata);
        end; //not gz
	result := true;
end;  *)

function LoadImg8(lInName: string; lImgData: ByteP; lStart, lEnd,linvox_offset,lRApos,lDataType,lVolVox: integer): boolean;
//loads BINARY data - ignore scaling: zero or not zero
var
	lvox_offset,lInc,lFSize,lP2: integer;
	lFData: file;
	lImgName: string;
        lByteP: ByteP;
        lSmallIntP: SmallIntP;
        lSingle: single;
begin
	result := false;
	if (lStart >= lEnd) or (lStart < 1) or (lEnd < 1) then begin
		NPMmsg('Error: LoadImg '+inttostr(lStart)+'>='+inttostr(lEnd)+' or zero');
		exit;
	end;
        if Files4D(lInName) then begin
	    lImgName := Filename4D(lInName);
            lP2 := BPP (lDataType);
            if lP2 = 0 then begin
                ShowMsg(lImgName +' is an unsupported file type');
                exit;
            end;
            lvox_offset := linvox_offset+ ((Vol4D(lInName)-1)* (lP2 * lVolVox));
        end else begin
	    lImgName := lInName;
            lvox_offset := linvox_offset;
        end;
	if UpCaseExt(lImgName) = '.HDR' then
		lImgName := changefileext(lImgName,'.img');
	lFSize := FSize(lImgName);
	if (not GzExt(lImgName)) and (lFSize < (lEnd+ lvox_offset))  then begin
		NPMmsg('Error: LoadImg '+lImgName+' FSize = '+inttostr(lFSize)+'  Expected '+inttostr(lEnd+ lvox_offset));
		exit;
	end;
	filemode := 0;
        //zero array
        for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := 0;//makesingle(lByteP^[lP2],lByteP^[lP2+1],lByteP^[lP2+2],lByteP^[lP2+3]);

        if GzExt(lImgName) then begin
          if lDataType = kDT_UNSIGNED_CHAR then begin
            getmem(lByteP, (lEnd+1)-lStart);
	    UnGZip(lImgName,lByteP,lvox_offset+lStart-1,(lEnd+1)-lStart);
            for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := lByteP^[lInc];
            freemem(lByteP);
          end else if (lDataType = kDT_SIGNED_SHORT) then begin
            //getmem(lSmallIntP, sizeof(smallint)* ((lEnd+1)-lStart));
            getmem(lByteP, ((lEnd+1)-lStart)*sizeof(smallint));
	    UnGZip(lImgName,lByteP,lvox_offset+((lStart-1)*sizeof(smallint)),((lEnd+1)-lStart)*sizeof(smallint));
            for lInc := 1 to ((lEnd+1)-lStart) do begin
                lP2 := ((lInc-1)*2)+1 ;
                lImgData^[lRApos+lInc-1] := makesmallint(lByteP^[lP2],lByteP^[lP2+1]);
            end;
            freemem(lByteP);
          end else if (lDataType = kDT_SIGNED_INT) or (lDataType = kDT_FLOAT) then begin
            getmem(lByteP, ((lEnd+1)-lStart)*sizeof(single));
	    UnGZip(lImgName,lByteP,lvox_offset+((lStart-1)*sizeof(single)),((lEnd+1)-lStart)*sizeof(single));
            if lDataType = kDT_SIGNED_INT  then begin
               for lInc := 1 to ((lEnd+1)-lStart) do begin
                lP2 := ((lInc-1)*4)+1 ;
                lSingle := conv4r4i (makesingle(lByteP^[lP2],lByteP^[lP2+1],lByteP^[lP2+2],lByteP^[lP2+3]));
                if lSingle <> 0 then
                   lImgData^[lRApos+lInc-1] := 1;
               end;
            end else begin //32 bit float
               for lInc := 1 to ((lEnd+1)-lStart) do begin
                lP2 := ((lInc-1)*4)+1 ;
                lSingle := makesingle(lByteP^[lP2],lByteP^[lP2+1],lByteP^[lP2+2],lByteP^[lP2+3]);
                if (not specialsingle(lSingle)) and (lSingle <> 0) then
                   lImgData^[lRApos+lInc-1] := 1;
               end;
            end;
            freemem(lByteP);
          end else begin
              ShowMsg(lImgName + ' is an unsupported compressed data type '+inttostr(lDataType));
              exit;
          end;
        end else begin
	  assignfile(lFdata,lImgName);
          if lDataType = kDT_UNSIGNED_CHAR then begin
            getmem(lByteP, (lEnd+1)-lStart);
 	    reset(lFdata,1);
	    seek(lFdata,lvox_offset+lStart-1);
	    BlockRead(lFdata, lByteP^, (lEnd+1)-lStart);
            for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := lByteP^[lInc];
            freemem(lByteP);
          end else if (lDataType = kDT_SIGNED_SHORT) then begin
            getmem(lSmallIntP, sizeof(smallint)* ((lEnd+1)-lStart));
 	    reset(lFdata,2);
            if (lvox_offset mod 2) <> 0 then begin
                ShowMsg('Error: this software can only read headers with dataoffsets that are divisible by 4.');
            end;
	    seek(lFdata,(lvox_offset div 2)+ (lStart-1));
	    BlockRead(lFdata, lSmallIntP^, (lEnd+1)-lStart);
            for lInc := 1 to ((lEnd+1)-lStart) do
                lImgData^[lRApos+lInc-1] := lSmallIntP^[lInc];
            freemem(lSmallIntP);
          end else if (lDataType = kDT_SIGNED_INT) or (lDataType = kDT_FLOAT) then begin
            //next: 4 byte data
            reset(lFdata,4);
            if (lvox_offset mod 4) <> 0 then begin
                ShowMsg('Error: this software can only read headers with dataoffsets that are divisible by 4.');
            end;
	    seek(lFdata,(lvox_offset div 4)+ (lStart-1) );
            getmem(lByteP, ((lEnd+1)-lStart)*sizeof(single));
            //fx(((lEnd+1)-lStart)*sizeof(single));
            BlockRead(lFdata, lByteP^, ((lEnd+1)-lStart));
            //April 2009
	    //UnGZip(lImgName,lByteP,lvox_offset+((lStart-1)*sizeof(single)),((lEnd+1)-lStart)*sizeof(single));
            if lDataType = kDT_SIGNED_INT  then begin
               for lInc := 1 to ((lEnd+1)-lStart) do begin
                lP2 := ((lInc-1)*4)+1 ;
                lSingle := conv4r4i (makesingle(lByteP^[lP2],lByteP^[lP2+1],lByteP^[lP2+2],lByteP^[lP2+3]));
                if lSingle <> 0 then
                   lImgData^[lRApos+lInc-1] := 1;
               end;
            end else begin
               for lInc := 1 to ((lEnd+1)-lStart) do begin
                lP2 := ((lInc-1)*4)+1 ;
                lSingle := makesingle(lByteP^[lP2],lByteP^[lP2+1],lByteP^[lP2+2],lByteP^[lP2+3]);
                if (not specialsingle(lSingle)) and (lSingle <> 0) then
                   lImgData^[lRApos+lInc-1] := 1;
               end;
            end;
            freemem(lByteP);

          end else
              ShowMsg('Unsupported COMPRESSED data type '+inttostr(lDataType));
          closefile(lFdata);
        end; //not gz
	result := true;
end;

end.