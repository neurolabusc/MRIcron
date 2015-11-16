unit batchstatselect;
{$H+}
interface

uses
  Classes, SysUtils,StrUtils, define_types, Dialogs;

procedure GetFilesInDir (lDefaultFolder: string; var lFilenames: TStrings);

implementation

function IsStatHdr(lStr: string): boolean;
//detects 'spmT_000*.hdr and  zstat*.nii.gz
//requires StrUtils
var
   lExt: string;
begin
     result := false;
     if not IsExtNIFTIHdr(lStr) then
      exit;
     if AnsiContainsText(lStr, 'spmT_') or  AnsiContainsText(lStr, pathdelim+'zstat') then
      result := true;
end;

procedure FindNIIhdrRecursive (var lFolderNameIn: string; var lStringList : TStrings);
var
 len: integer;
 lFolderName,lNewDir,lNewName,lExt: String;
 lSearchRec: TSearchRec;
begin
 lFolderName := lFolderNameIn;
 if not DirExists (lFolderName) then begin
     lFolderName := ExtractFileDir(lFolderName);
 end;
 if (length(lFolderName) > 1) and (lFolderName[length(lFolderName)] <> PathDelim) then
    lNewDir := lFolderName+PathDelim;
 if DirExists (lNewDir) then begin
{$IFDEF UNIX}
 if FindFirst(lNewDir+'*',faAnyFile-faSysFile,lSearchRec) = 0 then begin
{$ELSE}
 if FindFirst(lNewDir+'*.*',faAnyFile-faSysFile,lSearchRec) = 0 then begin
{$ENDIF}
    repeat
      lNewName := lNewDir+lSearchRec.Name;
      if  (lSearchRec.Name = '.') or (lSearchRec.Name = '..') then
        //current or parent folder - do nothing
      else if DirExists(lNewName) then
              FindNIIhdrRecursive (lNewName, lStringList)
      else if  IsStatHdr(lNewName)  then
              lStringList.Add(lNewName);
    until (FindNext(lSearchRec) <> 0);
   end; //if findfirst
   FindClose(lSearchRec);
 end;//Direxists
end;

procedure FilterForText (lRequiredText: string; var lFilenames: TStrings);
var
  i,len: integer;
begin
  len := lFilenames.Count;
  if (length(lRequiredText) < 1) or (len < 1) then
    exit;
  for i := len-1 downto 0 do
    if not AnsiContainsText(lFilenames[i], lRequiredText)  then
      lFilenames.Delete(i);
end;

procedure GetFilesInDir (lDefaultFolder: string; var lFilenames: TStrings);
var
  lParentDir,lFilter : string;
begin
     lParentDir := GetDirPrompt (lDefaultFolder);
     FindNIIhdrRecursive(lParentDir,lFilenames);
     lFilter := '.gfeat';
     InputQuery('Filter data', 'Filter for statistical maps [e.g. ''.gfeat'' will only analyze files with this in their path. Set to blank to analyze all files',lFilter);
     FilterForText(lFilter,lFilenames);
end;


end.

