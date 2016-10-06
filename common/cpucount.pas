unit cpucount;

interface
//returns number of cores: a computer with two dual cores will report 4
function GetLogicalCpuCount: Integer;

implementation
{$Include isgui.inc}
{$IFDEF UNIX}

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
     {$IFDEF Darwin}
     lProcess.CommandLine := 'sysctl hw.ncpu';
     {$ELSE}
     lProcess.CommandLine := 'nproc --all';
     {$ENDIF}
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
