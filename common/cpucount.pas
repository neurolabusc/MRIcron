unit cpucount;

interface
//returns number of cores: a computer with two dual cores will report 4
function GetLogicalCpuCount: Integer;

implementation
{$Include isgui.inc}
{$IFDEF UNIX}
{$IFDEF Darwin}
uses Process,SysUtils,Controls,classes,
{$IFDEF GUI}dialogs;{$ELSE} dialogsx;{$ENDIF}



function GetLogicalCpuCount: Integer;
//returns number of CPUs for MacOSX computer
//example - will return 4 if the computer has two dual core CPUs
//requires Process in Uses Clause
//see http://wiki.lazarus.freepascal.org/Executing_External_Programs
var
   lProcess: TProcess;
   lLen,lPos: integer;
   lStr: string;
   lStringList: TStringList;
begin
     Result := 1;
     lProcess := TProcess.Create(nil);
     lStringList := TStringList.Create;
     lProcess.CommandLine := 'sysctl hw.ncpu';
     lProcess.Options := lProcess.Options + [poWaitOnExit, poUsePipes];
     lProcess.Execute;
     lStringList.LoadFromStream(lProcess.Output);
     lLen := length(lStringList.Text);
     if lLen > 0 then begin
        lStr := '';
        for lPos := 1 to lLen do
            if lStringList.Text[lPos] in ['0'..'9'] then
               lStr := lStr + lStringList.Text[lPos];
        if length(lStr) > 0 then
           result := strtoint(lStr);
     end;//if at least one character returned
     if result < 1 then //just incase there is a horrible error, e.g. 0
        result := 1;
     lStringList.Free;
     lProcess.Free;
end;
{$ELSE} //Not Darwin ... Assume Linux
uses
    classes,sysutils;
(*function GetLogicalCpuCount: Integer;
var lS: TStringList;
    lFilename: string;
    lLine,lnLines: integer;
begin
     result := 1;
     lFilename := '/proc/cpuinfo';
     if not fileexists(lFilename) then exit;
     lS:= TStringList.Create;
     lS.LoadFromFile(lFilename);
     lnLines := lS.Count;
     if lnLines > 0 then begin
        result := 0;
        for lLine := 1 to lnLines do
            if lS[lLine-1] = '' then
               inc(result);
     end;
     if result < 1 then
        result := 1;
     lS.Free;
end;*)

function GetLogicalCpuCount: Integer;
var lS: TStringList;
    lFilename: string;
    lLine,lcpu: integer;
    sList: TStringList;
begin
     result := 1;
     lFilename := '/proc/cpuinfo';
     if not fileexists(lFilename) then exit;
     sList := TStringList.Create;
     lS:= TStringList.Create;
     lS.LoadFromFile(lFilename);
     for lLine := 0 to (lS.Count-1) do begin
         if pos('processor',lS[lLine]) = 1 then begin
            sList.DelimitedText := lS[lLine];
            lcpu := strtointdef(sList[sList.Count-1],0)+1;
            if lcpu > result then
               result := lcpu;
         end;
         //showmessage('"'+lS[lLine]+'"'+inttostr(pos(lS[lLine], 'processor')));
     end;
     (*lnLines := lS.Count;
     if lnLines > 0 then begin
        result := 0;
        for lLine := 1 to lnLines do
            if lS[lLine-1] = '' then
               inc(result);
     end;*)
     //if result > 3 then
     //   result := result div 2; //assume hyperthreading
     if result < 1 then
        result := 1;
     lS.Free;
     sList.Free;
end;
{$ENDIF} //If Darwin Else Linux

{$ELSE} //If UNIX ELSE NOT Unix -> windows
uses Windows;
function GetLogicalCpuCount: Integer;
var
  SystemInfo: _SYSTEM_INFO;
begin
  GetSystemInfo(SystemInfo);
  Result := SystemInfo.dwNumberOfProcessors;
end;

  {$ENDIF}


end.
