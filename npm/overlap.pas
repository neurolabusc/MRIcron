unit overlap;
{$H+}
{$Include ..\common\isgui.inc}
interface
uses
//     Graphics, Controls, Forms, StdCtrls,  ComCtrls,ExtCtrls,
Classes,nifti_hdr,define_types,SysUtils,dialogsx,
StatThdsUtil,Brunner,nifti_img,lesion_pattern, unpm;


Type
  OverlapRA = array [1..1] of TLesionPattern;//Toverlap;
  Overlapp = ^OverlapRA;

function CountOverlap (var lImages: TStrings; lMinDeficits,lnVoxTested: integer): integer;
procedure EvaluatePower(var lFilenames:  TStrings; lOverlapInc,lOverlapMax,lReps,lPct: integer);
function CountOverlap2(var lImages: TStrings; lMinDeficits, lnVoxTested: integer; lPlankImg: bytep): integer;


implementation


function SelectFiles (var lIn,lOut: TStrings; lN: integer): boolean;
//select (without replacement) lN filenames from the population lIn
var
 lnFound,lTrial,lRan,lSwap: integer;
 lRandRA: longintP;
begin
     result := false;
     lnFound := lIn.count;

     if (lnFound < lN) then
        exit; //not enough items found
     getmem(lRandRA,lnFound*sizeof(longint));
     for lTrial := 1 to lnFound do
         lRandRA^[lTrial] := lTrial-1; //index to each strong
     for lTrial := lnFound downto 2 do begin
             //jumble order
             lRan := random(lTrial)+1;
             lSwap := lRandRA^[lTrial];
             lRandRA^[lTrial] := lRandRA^[lRan];
             lRandRA^[lRan] := lSwap;
     end;
     for lTrial := 1 to lN do
         lOut.Add(lIn[lRandRA^[lTrial]]);
     freemem(lRandRA);
     result := true;
end;

procedure EvaluatePower(var lFilenames:  TStrings; lOverlapInc,lOverlapMax,lReps,lPct: integer);
label
     666;
var
     lG:  TStrings;
        lSize,lRep: integer;
begin
     if (lReps < 1) or (lOverlapMax < 1) or (lOverlapInc < 1) or (lOverlapMax > lFilenames.count) or (lOverlapInc > lOverlapMax) then begin
        ShowMsg('Error with EvaluatePower inputs...');
        exit;
     end;

     NPMMsgClear;
     //MainForm.NPMmsg(kVers);
     randomize;
     NPMmsg('Power Analysis began = ' +TimeToStr(Now));
     lSize := lOverlapInc;
     while lSize <= lOverlapMax do begin
           for lRep := 1 to lReps do begin
               lG:= TStringList.Create;
               if not SelectFiles(lFilenames,lG,lSize) then begin
                  ShowMsg('Error selecting '+inttostr(lSize)+'files!');
                  goto 666;
               end;
               CountOverlap(lG, round((lPct/100)*lSize),-1 );
               lG.Free;
           end; //for lLoop
           lSize := lSize + lOverlapInc;
    end; //for lRep
     NPMmsg('Analysis finished = ' +TimeToStr(Now));
     exit;
     666: //there has been a critical failure!
     lG.Free;
end;

(*function SelectFiles (lN: integer; var lOut: TStrings): boolean;
var
 lnFound,lTrial,lRan,lSwap: integer;
 lMaskExt,lFilePath: string;
 lSearchRec: TSearchRec;
 lF:  TStrings;
 lRandRA: longintP;
begin
     result := false;
     lF:= TStringList.Create;
     lFilepath := 'C:\140\';
     lMaskExt := '*.voi';
     if FindFirst(lFilePath{+PathDelim}+lMaskExt, faAnyFile-faSysFile-faDirectory, lSearchRec) = 0 then begin
        repeat
	       lF.add(lFilePath+lSearchRec.Name);
        until (FindNext(lSearchRec) <> 0);
     end;
     lnFound := lF.count;
     if (lnFound < lN) then begin
        lF.free;
        exit;
     end; //not enough items found
     getmem(lRandRA,lnFound*sizeof(longint));
     for lTrial := 1 to lnFound do
         lRandRA[lTrial] := lTrial-1; //index to each strong
     for lTrial := lnFound downto 2 do begin
             //jumble order
             lRan := random(lTrial)+1;
             lSwap := lRandRA[lTrial];
             lRandRA[lTrial] := lRandRA[lRan];
             lRandRA[lRan] := lSwap;
     end;
     for lTrial := 1 to lN do
         lOut.Add(lF[lRandRA[lTrial]]);
     freemem(lRandRA);
     lF.Free;
     result := true;
end;

procedure EvaluatePower;
label
     666;
var
	lG:  TStrings;
	lMaskname: string;
        lMaskHdr: TMRIcroHdr;
        lMaskVoxels,lN,lLoop,lRep: integer;
begin
     MainForm.NPMMsgClear;
     //MainForm.NPMmsg(kVers);
     randomize;
     MainForm.NPMmsg('Power Analysis began = ' +TimeToStr(Now));
    for lRep := 7 to 10 do begin
     for lLoop := 1 to 10 do begin
         lN := lRep * 10;
           lG:= TStringList.Create;
           if not SelectFiles(lN,lG) then begin
              ShowMsg('Error selecting '+inttostr(lN)+'files!');
              goto 666;
           end;
           {if not OpenDialogExecute('Select images to average',true,true,kImgFilter) then begin
	      ShowMsg('NPM aborted: file selection failed.');
	      exit;
           end; //if not selected
           lG:= TStringList.Create;
           lG.addstrings(OpenHdrDlg.Files);}

           {$IFDEF FORMATVARIES}
           //this next bit allows different types of scans to be read, but it is slow....
           lMaskname := lG[0];
           if not NIFTIhdr_LoadHdr(lMaskname,lMaskHdr) then begin
	      ShowMsg('Error reading mask.');
	      goto 666;
           end;
           lMaskVoxels := ComputeImageDataBytes8bpp(lMaskHdr);
           if not MainForm.CheckVoxelsGroup(lG,lMaskVoxels) then begin
	      ShowMsg('File dimensions differ from mask.');
	      goto 666;
           end;
           {$ENDIF}
           CountOverlap(lG, {round(0.1*lN)} 0);
           lG.Free;
     end; //for lLoop
    end; //for lRep
     MainForm.NPMmsg('Analysis finished = ' +TimeToStr(Now));
     exit;
     666: //there has been a critical failure!
     lG.Free;
end;  *)




function CountOverlap2(var lImages: TStrings; lMinDeficits, lnVoxTested: integer; lPlankImg: bytep): integer;
label
     123,667;
const
     kMaxBit = 63;
var
	lMaskName: string;
	//lPlankImg: byteP;
        lDouble,lTotalMemory: double;
        lVoxPerPlankDiv10,lOffset,lnDeficits,lUniqueOrders,
        lVolVox,lPos,lPlank,lVox,lDataType,lnVoxels,lImagesCount: integer;
        lnPlanks,lVoxPerPlank,lStartVox,lEndVox,lPlankImgPos: int64;
        lOverlapRA: Overlapp;
        lOrder,lPrevOrder: TLesionPattern;//x TOverlap;
        lMaskHdr: TMRIcroHdr;
        //lPowerRA: array [1..kMaxBit] of int64;
procedure CheckOrder(var lObservedOrder: TLesionPattern);
var
   lInc: integer;
begin
     if lUniqueOrders > 0 then begin //see if this is unique
          for lInc := 1 to lUniqueOrders do
              if SameOrder(lObservedOrder,lOverlapRA^[lInc],lImagesCount) then
                 exit; //not unique
     end; //UniqueOrders > 0
     //if we have not exited yet, we have found a new ordering!
     lUniqueOrders := lUniqueOrders + 1;
     lOverlapRA^[lUniqueOrders] := lObservedOrder;
end;
begin
        result := -1;
	//MainForm.NPMmsg('Analysis began = ' +TimeToStr(Now));
        //lMinDeficits := 0;
        lUniqueOrders := 0;
	lTotalMemory := 0;
        lMaskName := lImages[0];
        lImagesCount := lImages.Count;
        if lImagesCount < 1 then
           goto 667;
        if lImages.Count > (kMaxObs) then begin
            NPMmsg('Only able to compute tests for <= '+inttostr(kMaxObs)+' overlays.');
           goto 667;
        end;
        if not NIFTIhdr_LoadHdr(lMaskName,lMaskHdr) then begin
	    NPMmsg('Error reading 1st image.');
	   goto 667;
        end;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
        lDataType := lMaskHdr.NIFTIhdr.datatype;
        lOffset := round(lMaskHdr.NIFTIhdr.vox_offset);
        //ShowMsg(inttostr(lVolVox));
	if (lVolVox < 1) then goto 667;
	lVoxPerPlank :=  kPlankSz div lImages.Count {div sizeof(single)} ;
	if (lVoxPerPlank = 0) then goto 667; //no data
        lDouble := lVolVox;//force floating point multiplication in next step...
	lTotalMemory := lDouble * lImages.Count;
	if (lTotalMemory = 0)  then goto 667; //no data
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lImages.Count) ) + 1;
        lnVoxels := 0;
	lStartVox := 1;
	lEndVox := 0;
        if lnVoxTested <= 0 then
	   getmem(lOverlapRA,lVolVox* sizeof(TLesionPattern))
        else
           getmem(lOverlapRA,lnVoxTested* sizeof(TLesionPattern));
	for lPlank := 1 to lnPlanks do begin
            NPMProgressBar(1);

		lEndVox := lEndVox + lVoxPerPlank;
		if lEndVox > lVolVox then begin
			lVoxPerPlank := lVoxPerPlank - (lEndVox-lVolVox);
			lEndVox := lVolVox;
		end;
                lVoxPerPlankDiv10 := lVoxPerPlank div 10;
		lPlankImgPos := 1;
		for lPos := 1 to lImages.Count do begin
                        {$IFDEF FORMATVARIES}
			if not LoadImg8(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,round(gOffsetRA[lPos]),lPlankImgPos,gDataTypeRA[lPos],lVolVox) then
                        {$ELSE}
                        if not LoadImg8(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,lOffset,lPlankImgPos,lDataType,lVolVox) then
                        {$ENDIF}
				goto 667;
			lPlankImgPos := lPlankImgPos + lVoxPerPlank;
		end;//for each image
                lPrevOrder := EmptyOrder;//impossible: forces first voxel of each order to be checked
                for lVox := 1 to lVoxPerPlank do begin
                    if (lVox mod lVoxPerPlankDiv10) = 0 then begin
                         NPMProgressBar((lVox div lVoxPerPlankDiv10)*10);

                    end;
                    lOrder := EmptyOrder;
                    lPlankImgPos := 0;
                    lnDeficits := 0;
                    for lPos := 1 to lImages.Count do begin
                        if (lPlankImg^[lPlankImgPos + lVox] > 0) then begin
                           inc(lnDeficits);
                           SetBit(lPos,lOrder);
                        end;
                        lPlankImgPos := lPlankImgPos + lVoxPerPlank;
                    end;
                    if  (lnDeficits >= lminDeficits) then begin //this is different from the last voxel: perhaps this is a new ordering
                       if (not SameOrder(lOrder,lPrevOrder,lImagesCount)) then
                          CheckOrder(lOrder);
                       inc(lnVoxels);
                    end;
                    lPrevOrder := lOrder;
                end;
		lStartVox := lEndVox + 1;
	end;
        NPMmsg('n=,'+inttostr( lImages.Count)+',minN=,'+inttostr(lMinDeficits) +',unique overlap patterns,' +Inttostr(lUniqueOrders) +',voxels tested,' +Inttostr(lnVoxels));
123:
//next: free dynamic memory

        freemem(lOverlapRA);
         NPMProgressBar(0);
        result := lUniqueOrders;
	exit;
667: //you only get here if you aborted ... free memory and report error
        if lTotalMemory > 0 then begin
           freemem(lPlankImg);
           freemem(lOverlapRA);
        end;
	NPMmsg('Unable to complete analysis.');
        NPMProgressBar( 0);
end;

function CountOverlap(var lImages: TStrings; lMinDeficits,lnVoxTested: integer): integer;
var
   lPlankImg: byteP;
begin
     getmem(lPlankImg,kPlankSz);
    result := CountOverlap2( lImages, lMinDeficits,lnVoxTested,lPlankImg);
    freemem(lPlankImg);
end;

(*function CountOverlap2(var lImages: TStrings; lMinDeficits: integer; lPlankImg: bytep): integer;
label
     123,667;

var
	lMaskName: string;
        lVoxPerPlankDiv10,lOffset,lnDeficits,lUniqueOrders,lTotalMemory,
        lImagesCount,lVolVox,lPos,lPlank,lVox,lDataType,lnVoxels: integer;
        lnPlanks,lVoxPerPlank,lStartVox,lEndVox,lPlankImgPos: int64;
        lOverlapRA: Overlapp;
        lOrder,lPrevOrder: TLesionPattern;
        lMaskHdr: TMRIcroHdr;
        //lPowerRA: array [1..kMaxBit] of int64;


procedure CheckOrder(var lObservedOrder: TLesionPattern);
var
   lInc: integer;
begin
     if lUniqueOrders > 0 then begin //see if this is unique
          for lInc := 1 to lUniqueOrders do
              if SameOrder(lObservedOrder,lOverlapRA^[lInc],lImagesCount) then
                 exit; //not unique
     end; //UniqueOrders > 0
     //if we have not exited yet, we have found a new ordering!
     lUniqueOrders := lUniqueOrders + 1;
     lOverlapRA^[lUniqueOrders] := lObservedOrder;
end;
begin
        result := -1;
	//NPMmsg('Analysis began = ' +TimeToStr(Now));
        //lMinDeficits := 0;
        lUniqueOrders := 0;
	lTotalMemory := 0;
        lImagesCount := lImages.Count;
        lMaskName := lImages[0];
        if lImages.Count < 1 then
           goto 667;
        if lImages.Count > (kMaxObs) then begin
            NPMmsg('Only able to compute tests for <= '+inttostr(kMaxObs)+' overlays.');
           goto 667;
        end;
        if not NIFTIhdr_LoadHdr(lMaskName,lMaskHdr) then begin
	    NPMmsg('Error reading 1st image.');
	   goto 667;
        end;
	lVolVox := lMaskHdr.NIFTIhdr.dim[1]*lMaskHdr.NIFTIhdr.dim[2]* lMaskHdr.NIFTIhdr.dim[3];
        lDataType := lMaskHdr.NIFTIhdr.datatype;
        lOffset := round(lMaskHdr.NIFTIhdr.vox_offset);
        //ShowMsg(inttostr(lVolVox));
	if (lVolVox < 1) then goto 667;
	lVoxPerPlank :=  kPlankSz div lImages.Count ;
	if (lVoxPerPlank = 0) then goto 667; //no data
	lTotalMemory := lVolVox * lImages.Count;
	if (lTotalMemory = 0)  then goto 667; //no data
        //ShowMsg('xx');
	lnPlanks := trunc(lTotalMemory/(lVoxPerPlank*lImages.Count) ) + 1;
	//NPMmsg('Memory planks = ' +Floattostr(lTotalMemory/(lVoxPerPlank*lImages.Count)));
	//NPMmsg('Max voxels per Plank = ' +Floattostr(lVoxPerPlank));
        //if lTotalMemory > kPLankSz then
        
	//   getmem(lPlankImg,kPlankSz);
        //else
        //    getmem(lPlankImg,lTotalMemory);
        lnVoxels := 0;
	lStartVox := 1;
	lEndVox := 0;
	getmem(lOverlapRA,lVolVox* sizeof(TLesionPattern));
	for lPlank := 1 to lnPlanks do begin
                MainForm.ProgressBar1.Position := 1;
                MainForm.Refresh;
                Application.processmessages;
		lEndVox := lEndVox + lVoxPerPlank;
		if lEndVox > lVolVox then begin
			lVoxPerPlank := lVoxPerPlank - (lEndVox-lVolVox);
			lEndVox := lVolVox;
		end;
                lVoxPerPlankDiv10 := lVoxPerPlank div 10;
		lPlankImgPos := 1;
		for lPos := 1 to lImages.Count do begin
                        {$IFDEF FORMATVARIES}
			if not LoadImg8(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,round(gOffsetRA[lPos]),lPlankImgPos,gDataTypeRA[lPos],lVolVox) then
                        {$ELSE}
                        if not LoadImg8(lImages[lPos-1], lPlankImg, lStartVox, lEndVox,lOffset,lPlankImgPos,lDataType,lVolVox) then
                        {$ENDIF}
				goto 667;
			lPlankImgPos := lPlankImgPos + lVoxPerPlank;
		end;//for each image
                lPrevOrder := EmptyOrder;//impossible: forces first voxel of each order to be checked
                for lVox := 1 to lVoxPerPlank do begin
                    if (lVox mod lVoxPerPlankDiv10) = 0 then begin
                       MainForm.ProgressBar1.Position := (lVox div lVoxPerPlankDiv10)*10;
                       MainForm.Refresh;
                       Application.processmessages;
                    end;
                    lOrder := EmptyOrder;
                    lPlankImgPos := 0;
                    lnDeficits := 0;
                    for lPos := 1 to lImages.Count do begin
                        if (lPlankImg^[lPlankImgPos + lVox] > 0) then begin
                           inc(lnDeficits);
                           SetBit(lPos,lOrder);
                        end;
                        lPlankImgPos := lPlankImgPos + lVoxPerPlank;
                    end;
                    if  (lnDeficits >= lminDeficits) then begin //this is different from the last voxel: perhaps this is a new ordering
                       if (not SameOrder(lOrder,lPrevOrder,lImagesCount)) then
                          CheckOrder(lOrder);
                       inc(lnVoxels);
                    end;
                    lPrevOrder := lOrder;
                end;
		lStartVox := lEndVox + 1;
	end;
        NPMmsg('n=,'+inttostr( lImagesCount)+',minN=,'+inttostr(lMinDeficits) +',unique overlap patterns,' +Inttostr(lUniqueOrders) +',voxels tested,' +Inttostr(lnVoxels));
123:
//next: free dynamic memory
	//freemem(lPlankImg);
        freemem(lOverlapRA);
	//NPMmsg('Analysis finished = ' +TimeToStr(Now));
        MainForm.ProgressBar1.Position := 0;
        result := lUniqueOrders;
	exit;
667: //you only get here if you aborted ... free memory and report error
        if lTotalMemory > 0 then begin
           freemem(lPlankImg);
           freemem(lOverlapRA);
        end;
	NPMmsg('Unable to complete analysis.');
        MainForm.ProgressBar1.Position := 0;
end;  *)


end.