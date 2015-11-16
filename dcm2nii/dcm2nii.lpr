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
{$ELSE}
  shellAPI,
  Windows,
  {$ENDIF}

  Classes,
  inifiles,
  SysUtils,
  convert,
  define_types,
  sortdicom,
  dicom,
  parconvert,
  filename,
  dicomtypes,
  nii_crop,
  nii_4dto3d,
  prefs,
  dialogsx,
  paramstrs;
{$IFNDEF UNIX}
//{$R laz.res}
{$ENDIF}


(*var lIn,lOut: string;
Start: dword;
lPrefs: TPrefs;
begin
Start := GetTickCount;
	kUseDateTimeForID := true;
        SetDefaultPrefs (lPrefs);
        lPrefs.Gzip := true;
        lPrefs.Anonymize := true;
        lPrefs.SingleNIIFile := true;
        lPrefs.everyfile := true;
        lPrefs.AppendDate := false;
        lPrefs.AppendAcqSeries := true;
         lPrefs.AppendProtocolName := true;
         lPrefs.AppendPatientName := false;
         lPrefs.fourD := true;
         lPrefs.AppendFilename := true;
lOut := '';
lIn := 'C:\dti64\rapid\IM_0001.dcm';

LoadFileList(lIn,lOut,lPrefs);
Msg('Finished. Elapsed time: '+inttostr(GetTickCount-Start));
 readln;

 end.  *)

begin
     ShowMsg(kVers);
     kUseDateTimeForID := true;
      ProcessParamStrs;
end.







