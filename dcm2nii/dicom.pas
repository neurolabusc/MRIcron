unit dicom;
{$H+}
interface
{$DEFINE COMPAT}
uses
   dialogsx,prefs,dicomtypes
   {$IFDEF COMPAT}
,dicomcompat{,dicomfast};
{$ELSE}
,dicomfast;
{$ENDIF}
procedure read_dicom_data(lReadJPEGtables,lVerboseRead,lAutoDECAT7,lReadECAToffsetTables,lAutodetectInterfile,lAutoDetectGenesis,lReadColorTables: boolean; var lDICOMdata: DICOMdata; var lDTIra: TDTIRA;  var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string; var lPrefs: TPrefs);

implementation

procedure read_dicom_data(lReadJPEGtables,lVerboseRead,lAutoDECAT7,lReadECAToffsetTables,lAutodetectInterfile,lAutoDetectGenesis,lReadColorTables: boolean; var lDICOMdata: DICOMdata; var lDTIra: TDTIRA;  var lHdrOK, lImageFormatOK: boolean; var lDynStr: string;var lFileName: string; var lPrefs: TPrefs);
begin
   lDICOMdata.Filename := lFilename;
   lHdrOK := true;
   lImageFormatOK:=true;
   {$IFDEF COMPAT}
   //if not fast_read_dicom_data(lDICOMdata,128, lFileName) then
     read_dicom_data_compat(lReadJPEGtables,lVerboseRead,lAutoDECAT7,lReadECAToffsetTables,lAutodetectInterfile,lAutoDetectGenesis,lReadColorTables, lDICOMdata, lDTIra, lHdrOK, lImageFormatOK, lDynStr, lFileName,lPrefs);
     {$ELSE}
    lHdrOK := fast_read_dicom_data(lDICOMdata,128, lFileName);
     {$ENDIF}
end;

end.



