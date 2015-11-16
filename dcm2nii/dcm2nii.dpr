program dcm2nii;
{$IFDEF FPC}
{$mode objfpc}
{$ELSE}
 {$APPTYPE CONSOLE}
{$ENDIF}
{$H+}
uses
  {$IFDEF UNIX}
          {$IFDEF UseCThreads} cthreads, {$ENDIF}
  {$ENDIF}
  dialogsx,paramstrs,dicomtypes;
{$IFNDEF UNIX}
{$R *.res}
{$ENDIF}

begin
     ShowMsg(kVers);
     kUseDateTimeForID := true;
     {$IFNDEF TEST}
      ProcessParamStrs;
     {$ELSE}
         Testdcm2nii;
         readln;
     {$ENDIF}
end.







