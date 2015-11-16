unit filename;
{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}

interface
                                        

uses
//{$IFNDEF FPC}FileCtrl,{$ENDIF}
  Classes, SysUtils,define_types,dicomtypes,prefs;
procedure ExtractFileParts (var lFileName, lNameWOExt,lExt: string);
function ExtractFileDirWithPathDelim2(lInFilename: string): string;
procedure AplhaNumericStr (var lStr: string);
procedure AplhaNumericStrExt (var lStr: string);
function OutputFilename(var lDicomImgName: string; var lDicomData: dicomData; lPrefs: TPrefs): string;
procedure StripNIIVOIExt (var lFilename: string);

procedure StripGZExt (var lFilename: string);
function FilenameWOExt( lFileName: string): string;
function UpcaseStr(lIn: string): string;

implementation
uses dialogsx;


function UpcaseStr(lIn: string): string;
var lI: integer;
begin
	 result := lIn;
	 if length(result) > 0 then
		for lI := 1 to length(result) do
			result[lI] := upcase(result[lI]);
end;

function FilenameWOExt( lFileName: string): string;
var  lNameWOExt,lExt : string;
begin
	ExtractFileParts (lFileName, lNameWOExt,lExt);
	result := extractfilename(lNameWOExt);
end;

procedure AplhaNumericStr (var lStr: string);
var
	S: integer;
	lOutStr: string;
begin
	if length(lStr) < 1 then exit;
	lOutStr := '';

	for S := 1 to length (lStr) do
		if lStr[S] in ['0'..'9','A'..'Z','a'..'z'] then
			lOutStr := lOutStr+ lStr[S];
	lStr := lOutStr;
end;

procedure AplhaNumericStrExt (var lStr: string);
var
	S: integer;
	lOutStr: string;
begin
	if length(lStr) < 1 then exit;
	lOutStr := '';

	for S := 1 to length (lStr) do
		if lStr[S] in ['0'..'9','A'..'Z','a'..'z','_','-','^'] then
			lOutStr := lOutStr+ lStr[S];
	lStr := lOutStr;
end;

procedure StripNIIVOIExt (var lFilename: string);
var
	lStr: string;
	lLen,lPos: integer;
begin
	lLen := length(lFilename);
	if lLen < 8 then exit;
	lStr := '';
	for lPos := (lLen-7) to (lLen) do
		lStr := lStr +UpCase(lFilename[lPos]);
	if lStr <> '.NII.VOI' then exit;
	lStr := '';
	for lPos := 1 to (lLen-8) do
		lStr := lStr + lFilename[lPos];
        lStr := lStr + '.VOI';
	lFilename := lStr;
end;

procedure StripGZExt (var lFilename: string);
var
	lStr: string;
	lLen,lPos: integer;
begin
	lLen := length(lFilename);
	if lLen < 4 then exit;
	lStr := '';
	for lPos := (lLen-2) to (lLen) do
		lStr := lStr +UpCase(lFilename[lPos]);
	if lStr <> '.GZ' then exit;
	//showmessage(lFilename +' ->'+lStr);
	lStr := '';
	for lPos := 1 to (lLen-3) do
		lStr := lStr + lFilename[lPos];
	lFilename := lStr;
	//showmessage(lStr);
end;

function OutputFilename(var lDicomImgName: string; var lDicomData: dicomData; lPrefs: TPrefs): string;
var lFile,lStr,lStr2,lExt: string;
 lAppendDate,lAppendAcqSeries,lAppendProtocolName,lAppendPatientName,lAppendFilename:boolean ;
begin

  lAppendDate := lPrefs.AppendDate;
  lAppendAcqSeries := lPrefs.AppendAcqSeries;
  lAppendProtocolName := lPrefs.AppendProtocolName;
  lAppendPatientName := lPrefs.AppendPatientName;
  lAppendFilename := lPrefs.AppendFilename;
  if (not lAppendDate) and (not lAppendAcqSeries) and (not lAppendProtocolName) and (not lAppendPatientName) 
     and (not lAppendFilename) then begin
        lAppendDate := true;
        lAppendAcqSeries := true;
        lAppendProtocolName := true;
  end;
  lStr := '';

  if lAppendPatientName then begin
	lStr2 := lDicomData.PatientName;
	AplhaNumericStrExt(lStr2);
	lStr := lStr2+lStr;
  end;
  if lAppendProtocolName then begin
	lStr2 := lDicomData.ProtocolName;
	AplhaNumericStrExt(lStr2);
	lStr := lStr2+lStr;
  end;
  if lAppendFilename then begin
	  lFile := ExtractFilename (lDicomImgName);
	  ExtractFileParts (lFile, lStr2,lExt);
	  AplhaNumericStrExt(lStr2);
	  lStr := lStr2+lStr;
  end;
  if lAppendAcqSeries then
		  lStr := lStr+'s'+PadStr(lDicomData.SeriesNum,3)+'a'+PadStr(lDicomData.AcquNum,3);
  if lAppendDate then
	lStr := StudyDateTime2Str(lDicomData.DateTime)+lStr;
  lStr :=  lPrefs.NameAppend + lStr;
  result := lStr;



end;



procedure ExtractFileParts (var lFileName, lNameWOExt,lExt: string);
var lI: integer;
l2ndExt : string;
begin
   lNameWOExt := '';
   lExt := ExtractFileExt(lFileName);
   if length(lExt) > 0 then
		for lI := 1 to length(lExt) do
			lExt[lI] := upcase(lExt[lI]);
   if (lExt = '.GZ') or (lExt = '.VOI') then begin
	  lI := length(lFileName) - 6;
	  if li < 1 then exit;
	  l2ndExt := upcase(lFileName[lI])+upcase(lFileName[lI+1])+upcase(lFileName[li+2])+upcase(lFileName[li+3]);
	  if (l2ndExt = '.TAR') or (l2ndExt = '.NII')  then
		lExt := l2ndExt+lExt;
   end;
   if length(lExt) >= length (lFilename) then exit;
   for lI := 1 to (length(lFilename)-length(lExt)) do
    lNameWOExt := lNameWOExt + lFileName[lI];
   //next for Unix do not use UpCase extension
   if length(lExt) >= 0 then begin
    l2ndExt := '';
    for lI := 1 to (length(lExt)) do
      l2ndExt :=  lFilename[length(lFilename)-lI+1]+l2ndExt;
    lExt := l2ndExt;
   end; //length > 0
   //showmessage(lNameWOExt+'    '+lExt);
end;

function ExtractFileDirWithPathDelim2(lInFilename: string): string;
//F:\filename.ext -> 'F:\' and F:\dir\filename.ext -> 'F:\dir\'
//Despite documentation, Delphi3's ExtractFileDir does not always retain final pathdelim
//ensures c:\temp is returned c:\temp\ not c:\
begin
  if DirExists(lInFilename) then begin
  	result :=(lInFilename);
	if result[length(result)]<> pathdelim then
		result := result + pathdelim;
	exit;
  end;
  result := ExtractFileDirWithPathDelim(lInFilename);
end;

end.
