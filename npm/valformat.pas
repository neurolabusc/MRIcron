unit valformat;
{$H+}
interface
uses
  {$IFNDEF UNIX} Windows,{Registry,ShlObj,}{$ENDIF}
  //Messages,  Graphics, Controls, Forms, Dialogs,
  SysUtils, Classes,dialogsx,
  //Grids, Menus, ToolWin, ComCtrls, Buttons,Clipbrd, StdCtrls,Spin, ,npmform
  define_types;
const
{$IFDEF FPC}
        kNaN = -maxint;
{$ELSE}
        kNaN :  double = 1/0;
{$ENDIF}
	 kVALNativeSignatureBase = '#Version:';
         kValMaxVers = 1; //version 0 = 3D, version 1 = 4D, version 2 not yet supported
	 kTxtExt = '.txt';
	 kVALNativeExt = '.val';
  kValFilter  = 'Text description (*.val)|*.val';
function RowColPos (lRow,lCol,lnCol: integer): integer;         
function OpenValFile (var lFilename,lTemplateName:string; var lnRow,lnCol,lnColWObs,lnCritPct: integer;
	var lDesignUnspecified : boolean; var lPredictorList,lFileList:TStringList; var lDoublePtr: Pointer): boolean;

function GetValCore (var lVALFilename:string; var lnSubj, lnFactors: integer; var lSymptomRA: singleP; var lImageNames:  TStrings; var lCrit,lCritPct: integer; {lBinomial : boolean;} var lPredictorList: TStringList):boolean;

implementation

procedure MsgX (lStr: string);
begin
    //output something here
    showmsg(lStr);
end;



function VALNativeSignature (lStr: string): boolean;
var
   lP,lLen: integer;
   lVers: string;
begin
    result := false;
    lLen := length(lStr);
    if lLen < (length(kVALNativeSignatureBase)+1) then
       exit;
    for lP := 1 to length(kVALNativeSignatureBase) do
        if lStr[lP] <> kVALNativeSignatureBase[lP] then
           exit;
    //VAL format, but can we read this version?
    for lP := (length(kVALNativeSignatureBase)+1) to lLen do
        lVers := lVers + lStr[lP];
    if strtoint(lVers) <= kValMaxVers then
       result := true;
end;

function ReadTabStr (var lStr: string; var lPos: integer): string;
var
   lLen: integer;
begin
	result := '';
	if lPos < 1 then lPos := 1;
	lLen := length(lStr);
	while (lPos <= lLen) and (lStr[lPos] <> kTab) do begin
		  result := result + lStr[lPos];
		  inc(lPos);
	end;
	inc(lPos);
end;

function RowColPos (lRow,lCol,lnCol: integer): integer;
begin
	 result := ((lRow-1{alfa})*lnCol)+lCol;
end;

//Replicates Readln, but works for Unix files... Delphi 4's readln fails for non-MSDOS EOLs
procedure ReadlnX (var F: TextFile; var lResult: string);
var
   lCh: char;
begin
     lResult := '';
     while not Eof(F) do begin
           Read(F, lCh);
           if (lCh in [#10,#13]) then begin
              if lResult <> '' then begin
                 //Showmessage(lResult);
                 exit;
              end;
           end else
               lResult := lResult + lCh;
     end;
end; //ReadlnX

function OpenValFile (var lFilename,lTemplateName:string; var lnRow,lnCol,lnColwObs,lnCritPct: integer;
	var lDesignUnspecified : boolean; var lPredictorList,lFileList:TStringList; var lDoublePtr: Pointer): boolean;
var
   lNumStr,lStr,lExt,lPrevNumStr,lCmdStr: string;
   F: TextFile;
   lTempFloat: double;
   lCh: char;
   lPos,MaxC,R,C:integer;
   lDoubleBuf: DoubleP;
   lError: boolean;
   lDecimalSep: char;
begin
         lnRow := 0;
         lnCol := 0;
         result := false;
	 if not fileexists(lFilename) then exit;
         lError:= false;
	 lnCritPct := 0;
	 lExt := StrLower(PChar(extractfileext(lFilename)));
	 if  (lExt = kTxtExt) or (lExt = kVALNativeExt) then
	 else begin
		ShowMsg('This version is unable to recognize the extension of the file: '+lFilename);
		 exit;
	 end;
         lDecimalSep := DecimalSeparator;
         DecimalSeparator := '.';
	 AssignFile(F, lFilename);
	 FileMode := 0;  //Set file access to read only
	 //First pass: determine column height/width
	 Reset(F);
	 C := 0;
	 MaxC := 0;
	 R := 0;
	 if lExt = kVALNativeExt then begin
		ReadlnX(F,lStr);//Version
		if not VALNativeSignature(lStr) then begin
			showMsg('This software can not read this file. Perhaps you need to upgrade your software. The first line should read "'+kVALNativeSignatureBase+'x" where "x" is <'+inttostr(kValMaxVers+1));
			CloseFile(F);
			FileMode := 2;  //Set file access to read/write
			exit;
		end;
                lDesignUnspecified := false;
                lStr := '#';
                while (length(lStr)> 0) and (lStr[1] = '#') and (not Eof(F)) do begin
		      ReadlnX(F,lStr);
		      lPos := 0; //start at beginning of line
                      lCmdStr := ReadTabStr(lStr,lPos);
                      if lCmdStr = '#Template' then
			 lTemplateName := ReadTabStr(lStr,lPos);
                      if lCmdStr = '#CritPct' then
			lnCritPct := StrToInt(ReadTabStr(lStr,lPos));
		end;
                if (length(lStr)> 0) and (lStr[1] = '#') then showmsg(lCmdStr);
	 end else begin
		lnCritPct := 0;
		lDesignUnspecified := true;
		lTemplateName := '-';
	 end;//Ext=native version
	 Reset(F);
	 while not Eof(F) do begin
		//read next line
		//read next line
		Read(F, lCh);
		if lCh = '#' then
		   while not (lCh in [#10,#13]) do
			   Read(F, lCh)
		else if not (lCh in [#10,#13,#9]) then begin
		   lNumStr := lNumStr + lCh;
		end else if lNumStr <> '' then begin
			lNumStr := '';
			inc(C);
			if C > MaxC then
				MaxC := C;
			if (lCh in [#10,#13]) then begin
				C := 0;
			   inc(R);
			end; //eoln
		end; //if lNumStr <> '' and not tab
	 end;
	 if lNumStr <> '' then  //july06- read data immediately prior to EOF
            inc(R);
lnRow:= R;
lnCol := MaxC-1;
		lnColWObs := lnCol+1;
	getmem(lDoublePtr,(lnRow*lnColWObs* sizeof(double))+16);
	   {$IFDEF FPC}
	lDoubleBuf := align(lDoublePtr,16);
   {$ELSE}
   //lDoubleBuf := DoubleP((integer(lDoublePtr) and $FFFFFFF0)+16);
   lDoubleBuf := DoubleP($fffffff0 and (integer(lDoublePtr)+15));
   {$ENDIF}
	for C := 1 to (lnRow*lnColWObs) do
		lDoubleBuf^[C] := 0;
	 //Second pass: fill values
	 Reset(F);
	 C := 0;
	 MaxC := 0;
	 R := 1;
	 lNumStr := '';
	 while not Eof(F) do begin
		//read next line
		Read(F, lCh);
		if lCh = '#' then
		   while not (lCh in [#10,#13]) do
			   Read(F, lCh)
		else if not (lCh in [#10,#13,#9]) then begin
		   lNumStr := lNumStr + lCh;
		end else if lNumStr <> '' then begin
			//read current entry
			if R = 1 then begin  //1st Row
			   if C > 0 then
				lPredictorList.Add( lNumStr)
			end else if C = 0 then begin  //1st Row
				//showmessage(lNumStr);
				lFileList.Add( lNumStr)
			end else begin  //note: below -1 as we strip first header row for predictor names
                           if lNumStr = '-' then begin
                              lDoubleBuf^[RowColPos (R-1{ july 06 alfa},C,lnColWObs)] := 0;
                           end else begin //number
                              try
                                 lTempFloat := strtofloat(lNumStr);

                              except
                                    on EConvertError do begin
                                       if not lError then
                                          showmsg('Empty cells? Error reading VAL file row:'+inttostr(R)+' col:'+inttostr(C)+' - Unable to convert the string '+lNumStr+' to a number');
                                       lError := true;
                                       lTempFloat := knan;
                                    end;
                              end;
				lDoubleBuf^[RowColPos (R-1{ july 06 alfa},C,lnColWObs)] := lTempFloat;//DataGrid.Cells[ C, kMaxFactors+R-1 ] := (lNumStr) ;
                           end;
                        end;
			lPrevNumStr := lNumStr;
			lNumStr := '';
			inc(C);
			if C > MaxC then
				MaxC := C;
			if (lCh in [#10,#13]) then begin
				C := 0;
			   inc(R);
			end; //eoln
		end; //if lNumStr <> '' and not tab
	 end;
	 if (lNumStr <> '') and (C>0) then //alfa read data immediately prior to EOF
		lDoubleBuf^[RowColPos (R-1{alfa},C,lnColWObs)] := strtofloat(lNumStr);
	 CloseFile(F);
	 FileMode := 2;  //Set file access to read/write
         result := true;
         DecimalSeparator := lDecimalSep;
     //fx(lPredictorList.Count,lnCol);
     if lPredictorList.Count < lnCol then begin
        for C := (lPredictorList.Count+1) to lnCol do
            lPredictorList.Add('Predictor'+inttostr(C));
     end;
end;


function GetValCore (var lVALFilename:string; var lnSubj, lnFactors: integer; var lSymptomRA: singleP; var lImageNames:  TStrings; var lCrit,lCritPct: integer; {lBinomial : boolean;} var lPredictorList: TStringList):boolean;
//warning: you MUST free lPredictorList
var
   lTemplateName: string;
   lnRow,lCol,lnColWObs,lInc,lRow: integer;
   lDesignUnspecified : boolean;
   lFileList:TStringList;
   lInRA: DoubleP0;
   lInP: Pointer;
begin
     lPredictorList := TStringList.Create;
     result := false;
     lnSubj := 0;
     if not Fileexists(lVALFilename) then begin

	   MsgX('NPM aborted: VAL file selection failed:' +lValFilename);
	   exit;
     end; //if not selected
     lFileList := TStringList.Create;
     //MsgX( 'VAL filename: '+lVALFilename);
     if not OpenValFile (lVALFilename,lTemplateName, lnRow,lnFactors,lnColWObs,lCritPct,
                 lDesignUnspecified,lPredictorList,lFileList, lInP) then exit;

     if lnRow > 1 then begin
        lnSubj := lnRow -1; //top row is predictor
           {$IFDEF FPC}
        lInRA := align(lInP,16);
   {$ELSE}
        lInRA := DoubleP0($fffffff0 and (integer(lInP)+15));
   {$ENDIF}

        getmem(lSymptomRA,lnSubj*lnFactors* sizeof(single));
        for lCol := 1 to lnFactors do begin
            for lRow := 1 to lnSubj do begin
              lSymptomRA^[lRow+ ((lCol-1)*lnSubj)] := lInRA^[(lRow*lnColWObs)-lnColWObs-1+lCol];
            end;
        end;
        for lInc := 1 to lnSubj do
            lImageNames.add(ExtractFileDirWithPathDelim(lVALFilename)+lFileList.Strings[lInc-1]);
        //end reverse
     end; //for lRow = each subject
     lFileList.free;
     Freemem(lInP);

     lCrit := round( (lnSubj*lCritPct)/100);
          result := true;
end;


end.
 