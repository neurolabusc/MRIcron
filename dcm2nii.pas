unit dcm2nii;

 {$IFDEF FPC} {$mode delphi}{$H+} {$ENDIF}
interface
uses
    LazUTF8, LazFileUtils,
    {$IFDEF FPC}
    FileUtil, Process,LResources,
    {$IFDEF UNIX} LCLIntf, {$ENDIF}
  {$ELSE}
  Windows, FileCtrl, shellAPI, Messages,
  {$ENDIF}
  {$IFNDEF UNIX} Registry, {$ENDIF}
    {$IFDEF Darwin} userdir, {$ENDIF}
  //
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus;  

type
  { Tdcm2niiForm }
  Tdcm2niiForm = class(TForm)
    compressCheck: TCheckBox;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    EditMenu: TMenuItem;
    CopyMenu: TMenuItem;
    DicomMenu: TMenuItem;
    ResetMenu: TMenuItem;
    ParRecMenu: TMenuItem;
    outputFolderName: TButton;
    //compressCheck: TCheckBox;
    Label2: TLabel;
    outputFolderLabel: TLabel;
    outnameLabel: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    outnameEdit: TEdit;
    VerboseCheck: TCheckBox;
    //bidsCheck: TCheckBox;
    VerboseLabel: TLabel;
    BIDSLabel: TLabel;
    bidsCheck: TCheckBox;
    //BIDSLabel: TLabel;
    //verboseCheck: TCheckBox;
    //VerboseLabel: TLabel;
    procedure compressCheckClick(Sender: TObject);
    procedure DicomMenuClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function getOutputFolder: string;
    procedure outnameEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ParRecMenuClick(Sender: TObject);
    procedure ProcessFile(infilename: string);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure CopyMenuClick(Sender: TObject);
    procedure outputFolderNameClick(Sender: TObject);
    procedure ResetMenuClick(Sender: TObject);
    procedure RunCmd (lCmd: string; isDemo: boolean; out line1: string);
    function getExeName : string; //return path for command line tool
    procedure readIni (ForceReset: boolean); //load preferences
    procedure writeIni; //save preferences
    function FindDicom2niixPath(const Executable: string): string;
   // procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  private
    { private declarations }
   {$IFNDEF FPC}    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES; {$ENDIF}
  public
    { public declarations }
  end;

var
  dcm2niiForm: Tdcm2niiForm;

implementation
 {$IFDEF FPC}
  {$R *.lfm}
 {$ELSE}
 {$R *.dfm}
 {$ENDIF}
//
 {$IFDEF CPU64}
 const kExeName = 'dcm2niix';
 {$ELSE}
        {$IFDEF Linux}
        const kExeName = 'dcm2niix32';
        {$ELSE}
        const kExeName = 'dcm2niix';
        {$ENDIF}
 {$ENDIF}

  var
    isAppDoneInitializing : boolean = false;

{$IFDEF FPC}
function Tdcm2niiForm.FindDicom2niixPath(const Executable: string): string;
//function FindDicom2niixPath(const Executable: string): string;
var
  s: string;
begin
     {$IFDEF Darwin}
     result := AppDir + kExeName;
     if fileexists(result) then exit;
     {$ENDIF}
     result := FindDefaultExecutablePath(kExeName);
     if result = '' then
        result := FindDefaultExecutablePath(ExtractFilePath(paramstr(0))+'Resources'+pathdelim+kExeName);
     if result = '' then
        result := FindDefaultExecutablePath(ExtractFilePath  (paramstr(0)) +kExeName);
     if result = '' then
        result := FindDefaultExecutablePath(kExeName);
     {$IFDEF Unix}
     if (result = '') and (fileexists('/usr/local/bin/'+kExeName)) then
        result := '/usr/local/bin/'+kExeName;
     {$ENDIF}
     //Env:=GetEnvironmentVariableUTF8('PATH');
     //if result = '' then
     //   showmessage('mango:'+GetEnvironmentVariableUTF8('HOME'));
end;
{$ELSE}
function Tdcm2niiForm.FindDicom2niixPath(const Executable: string): string;
begin
     result := extractfilepath(paramstr(0))+kExeName+'.exe';
end;
{$ENDIF}

function Tdcm2niiForm.getExeName : string;
var
  lF: string;
begin
     result := FindDicom2niixPath(kExeName);
     if not fileexists(result) then begin
        lF :=  ExtractFilePath (paramstr(0));
        result := lF+kExeName;
        if not fileexists(result) then begin
           Memo1.Lines.Clear;
           memo1.Lines.Add('Error: unable to find executable '+kExeName+' in path');
           memo1.Lines.Add(' Solution: copy '+kExeName+' to '+lF);
           result := '';
        end;  //not in same folder as GUI
     end; //not in path
     {$IFNDEF UNIX} //strip .exe for Windows
     result := ChangeFileExt(result, '');
     {$ENDIF}
end; //exeName()

{$IFDEF UNIX}
function iniName : string;
begin
     result := GetEnvironmentVariable ('HOME')+PathDelim+'.dcm2nii.ini';
end;

procedure Tdcm2niiForm.writeIni;
var
   iniFile : TextFile;
 begin
   AssignFile(iniFile, iniName);
   ReWrite(iniFile);
   if (compressCheck.checked) then
      WriteLn(iniFile, 'isGZ=1')
   else
       WriteLn(iniFile, 'isGZ=0');
   if (bidsCheck.checked) then
      WriteLn(iniFile, 'isBIDS=1')
   else
       WriteLn(iniFile, 'isBIDS=0');
   WriteLn(iniFile, 'filename='+outnameEdit.caption);
   CloseFile(iniFile);
end; //writeIni

procedure Tdcm2niiForm.readIni (ForceReset: boolean);
var
  fileData, rowData : TStringList;
  row, i: integer;
  opts_isGz, opts_isBids: boolean;
  opts_filename: string;
begin
     opts_isGz := true;
     opts_isBids := true;
     //opts_outdir := '';
     opts_filename := '%t_%p_%s';
     if FileExists( iniName) and (not (ForceReset )) then begin
        fileData := TStringList.Create;
        fileData.LoadFromFile(iniName);  // Load from Testing.txt file
        if (fileData.Count > 0) then begin
           rowData := TStringList.Create;
           rowData.Delimiter := '=';
           for row := 0 to (fileData.Count-1) do begin //for each row of file
               rowData.DelimitedText:=fileData[row];
               if ((rowData.Count > 1) and (CompareText(rowData[0] ,'isGZ')= 0)) then
                  opts_isGz := (CompareText(rowData[1],'1') = 0);
               if ((rowData.Count > 1) and (CompareText(rowData[0] ,'isBIDS')= 0)) then
                  opts_isBids := (CompareText(rowData[1],'1') = 0);
               if ((rowData.Count > 1) and (CompareText(rowData[0] ,'filename')= 0)) then begin
                  opts_filename := '';
                  if (rowData.Count > 2) then
                     for i := 1 to (rowData.Count-2) do
                         opts_filename := opts_filename+ rowData[i]+' ';
                  opts_filename := opts_filename+ rowData[rowData.Count-1];
               end;
           end;
          rowData.Free;
        end;
        fileData.Free;
     end else
         memo1.Lines.Add('Using default settings');
     compressCheck.Checked := opts_isGz;
     bidsCheck.Checked := opts_isBids;
     outnameEdit.Caption := opts_filename;
     //getExeName;
end; //readIni()
{$ELSE}
//For Windows we save preferences in the registry to ensure user has write access
procedure Tdcm2niiForm.writeIni;
var
  ARegistry: TRegistry;
begin
     ARegistry := TRegistry.Create;
     ARegistry.RootKey := HKEY_CURRENT_USER;//HKEY_LOCAL_MACHINE;
     if ARegistry.OpenKey ('\Software\dcm2nii',true) then begin
       	  ARegistry.WriteBool('isGZ', compressCheck.Checked );
          ARegistry.WriteBool('isBIDS', bidsCheck.Checked );
       	  ARegistry.WriteString('filename', outnameEdit.text );
     end;
     ARegistry.Free;
end; //writeIni()

procedure Tdcm2niiForm.readIni (ForceReset: boolean);
var
  ARegistry: TRegistry;
  opts_isGz, opts_isBids: boolean;
  opts_filename: string;
begin
     opts_isBids := true;
     opts_isGz := true;
     opts_filename := '%t_%p_%s';
     if not ForceReset then begin
       ARegistry := TRegistry.Create;
       ARegistry.RootKey := HKEY_CURRENT_USER;//HKEY_LOCAL_MACHINE;
       if ARegistry.OpenKey ('\Software\dcm2nii',true) then begin
       	    if ARegistry.ValueExists( 'isGZ' ) then
          	   opts_isGz := ARegistry.ReadBool( 'isGZ' );
       	    if ARegistry.ValueExists( 'isBIDS' ) then
          	   opts_isBids := ARegistry.ReadBool( 'isBIDS' );
            if ARegistry.ValueExists( 'isGZ' ) then
          	   opts_filename := ARegistry.ReadString( 'filename' );
       end;
       ARegistry.Free;
     end;
     bidsCheck.Checked := opts_isBids;
     compressCheck.Checked := opts_isGz;
     outnameEdit.text := opts_filename;
     //getExeName;
end; //readIni()
{$ENDIF}

{$IFDEF FPC}
procedure Tdcm2niiForm.RunCmd (lCmd: string; isDemo: boolean; out line1: string);
//http://wiki.freepascal.org/Executing_External_Programs
var
  OutputLines: TStringList;
  MemStream: TMemoryStream;
  OurProcess: TProcess;
  NumBytes: LongInt;
  BytesRead: LongInt;
const
  READ_BYTES = 2048;
begin
     line1 := '';
   if (not isAppDoneInitializing) then exit;
   if (getExeName = '') then exit;
   Memo1.Lines.Clear;
   dcm2niiForm.refresh; Memo1.refresh; Memo1.invalidate;
   MemStream := TMemoryStream.Create;
   BytesRead := 0;
   OurProcess := TProcess.Create(nil);
   {$IFDEF UNIX}
   OurProcess.Environment.Add(GetEnvironmentVariable('PATH'));
   {$ENDIF}
   OurProcess.CommandLine := lCmd;
  // We cannot use poWaitOnExit here since we don't
  // know the size of the output. On Linux the size of the
  // output pipe is 2 kB; if the output data is more, we
  // need to read the data. This isn't possible since we are
  // waiting. So we get a deadlock here if we use poWaitOnExit.
  OurProcess.Options := [poUsePipes, poNoConsole];
  OurProcess.Execute;
  while True do begin
    // make sure we have room
    MemStream.SetSize(BytesRead + READ_BYTES);
    // try reading it
    NumBytes := OurProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
    if NumBytes > 0 // All read() calls will block, except the final one.
    then begin
      Inc(BytesRead, NumBytes);
    end else
      BREAK // Program has finished execution.
  end;
  MemStream.SetSize(BytesRead);
  OutputLines := TStringList.Create;
  OutputLines.LoadFromStream(MemStream);
  if OutputLines.Count > 0 then begin
     Line1 := OutputLines[0];
     //skip if line is "Compression will be faster with 'pigz'"
     if (pos('Compression', Line1) = 1) and (OutputLines.Count > 0) then
        Line1 := OutputLines[1];
  end;
  Memo1.Lines.AddStrings(OutputLines);
  if isDemo then
     Memo1.Lines.Add(lCmd+' "MyDicomFolder"')
  else
      Memo1.Lines.Add(lCmd);
  OutputLines.Free;
  OurProcess.Free;
  MemStream.Free;
end;
{$ELSE} //if FPC else Delphi
procedure Tdcm2niiForm.RunCmd (lCmd: string; isDemo: boolean);
const
     ReadBuffer = 2400;
  var
   Security : TSecurityAttributes;
   ReadPipe,WritePipe : THandle;
   start : TStartUpInfo;
   ProcessInfo : TProcessInformation;
   Buffer : Pchar;
   BytesRead : DWord;
   Apprunning : DWord;
  begin
   if (not isAppDoneInitializing) then exit;
   if (getExeName = '') then exit;
   Memo1.Lines.Clear;
   With Security do begin
    nlength := SizeOf(TSecurityAttributes) ;
    binherithandle := true;
    lpsecuritydescriptor := nil;
   end;
   if Createpipe (ReadPipe, WritePipe,
                  @Security, 0) then begin
    Buffer := AllocMem(ReadBuffer + 1) ;
    FillChar(Start,Sizeof(Start),#0) ;
    start.cb := SizeOf(start) ;
    start.hStdOutput := WritePipe;
    start.hStdInput := ReadPipe;
    start.dwFlags := STARTF_USESTDHANDLES +
                         STARTF_USESHOWWINDOW;
    start.wShowWindow := SW_HIDE;

    if CreateProcess(nil,
           PChar(lCmd),
           @Security,
           @Security,
           true,
           NORMAL_PRIORITY_CLASS,
           nil,
           nil,
           start,
           ProcessInfo)
    then
    begin
     repeat
      Apprunning := WaitForSingleObject
                   (ProcessInfo.hProcess,100) ;
      Application.ProcessMessages;
     until (Apprunning <> WAIT_TIMEOUT) ;
      Repeat
        BytesRead := 0;
        ReadFile(ReadPipe,Buffer[0],
ReadBuffer,BytesRead,nil) ;
        Buffer[BytesRead]:= #0;
        OemToAnsi(Buffer,Buffer) ;
        Memo1.Text := Memo1.text + String(Buffer) ;
if isDemo then
     Memo1.Lines.Add(lCmd+' "MyDicomFolder"')
  else
      Memo1.Lines.Add(lCmd);
      until (BytesRead < ReadBuffer) ;
   end;
   FreeMem(Buffer) ;
   CloseHandle(ProcessInfo.hProcess) ;
   CloseHandle(ProcessInfo.hThread) ;
   CloseHandle(ReadPipe) ;
   CloseHandle(WritePipe) ;
   end;
  end;
{$ENDIF}

function Tdcm2niiForm.getOutputFolder: string;
begin
     if (outputFolderName.Tag > 0) then
        result := outputFolderName.Caption
     else
         result := '';
end; //getOutputFolder

procedure Tdcm2niiForm.ProcessFile(infilename: string);
var
  cmd, outputFolder, inFolder, line1: string;
begin
  inFolder := infilename;
  (*if isTGZ(inFolder) then begin
  	 infolder := deTGZ(infolder);
     if infolder = '' then exit; //error
 end;*)
 cmd := '"'+getExeName +'" ';
 if bidsCheck.checked then
    cmd := cmd + '-b y '
 else
     cmd := cmd + '-b n ';
 if compressCheck.checked then
    cmd := cmd + '-z y '
 else
     cmd := cmd + '-z n ';
 if verboseCheck.checked then
 cmd := cmd + '-v y ';
 outputFolder := getOutputFolder;
 if length(outputFolder) > 0 then
     cmd := cmd + '-o '+outputFolder+' ';
 cmd := cmd + '-f "'+outnameEdit.Text+'" ';
 if length(inFolder) > 0 then
     cmd := cmd +'"'+inFolder+'"';
     //Caption := inttostr(length(inFolder));
 RunCmd(cmd, length(inFolder) = 0, line1);
end; //ProcessFile()

procedure Tdcm2niiForm.outnameEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
      ProcessFile('');
end; //outnameEditKeyUp()

procedure Tdcm2niiForm.ParRecMenuClick(Sender: TObject);
var
  lI: integer;
begin
  if not OpenDialog1.execute then exit;
  //ProcessFile(OpenDialog1.filename);
  if OpenDialog1.Files.count < 1 then exit;
     for lI := 0 to (OpenDialog1.Files.count-1) do
         ProcessFile(OpenDialog1.Files[lI]);
end; //ParRecMenuClick()

{$IFDEF FPC}
function getDirPrompt (lDefault: string): string;
begin
  result := lDefault;  // Set the starting directory
  chdir(result); //start search from default dir...
  if SelectDirectory(result, [sdAllowCreate,sdPerformCreate,sdPrompt], 0) then
     chdir(result)
  else
      result := '';
end;  //getDirPrompt()
{$ELSE}
function DirExists(Name: string): Boolean;
{$IFDEF WIN32}
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
{$ELSE}
var
  SR: TSearchRec;
begin
  if Name[Length(Name)] = '\' then Dec(Name[0]);
  if (Length(Name) = 2) and (Name[2] = ':') then
    Name := Name + '\*.*';
  Result := FindFirst(Name, faDirectory, SR) = 0;
  Result := Result and (SR.Attr and faDirectory <> 0);
end;
{$ENDIF}

function getDirPrompt (lDefault: string): string;
var
  opts: TSelectDirOpts;
begin
  result := lDefault;  // Set the starting directory
  if direxists(result) then
    chdir(result); //start search from default dir...
  if SelectDirectory(result, opts, 0) then
     chdir(result)
  else
      result := '';
end;  //getDirPrompt()
{$ENDIF}
procedure Tdcm2niiForm.DicomMenuClick(Sender: TObject);
var
  dir: string;
begin
     dir := getDirPrompt('');
     ProcessFile( dir);
end; //DicomMenuClick()

procedure Tdcm2niiForm.compressCheckClick(Sender: TObject);
begin
  ProcessFile('');
end;

procedure Tdcm2niiForm.FormResize(Sender: TObject);
begin
  outputFolderName.width := dcm2niiForm.Width-outputFolderName.left-2;
end; //FormResize()

procedure Tdcm2niiForm.FormShow(Sender: TObject);
begin
       ProcessFile('');
end;

procedure Tdcm2niiForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
    ProcessFile( FileNames[0]);
end; //FormDropFiles()

{$IFNDEF FPC}
procedure Tdcm2niiForm.WMDropFiles(var Msg: TWMDropFiles);  //implement drag and drop
var  CFileName: array[0..MAX_PATH] of Char;
begin
  try
   if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then begin
      ProcessFile(CFilename);
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;//Proc WMDropFiles
{$ENDIF}

procedure Tdcm2niiForm.CopyMenuClick(Sender: TObject);
begin
     Memo1.SelectAll;
     Memo1.CopyToClipboard;
end; //CopyMenuClick()

procedure Tdcm2niiForm.outputFolderNameClick(Sender: TObject);
var
  lDir : string;
begin
     if (outputFolderName.Tag > 0) then //start search from prior location
        lDir := outputFolderName.Caption
     else
         lDir := '';
     lDir := getDirPrompt(lDir);
     outputFolderName.Tag := length(lDir);
     if length(lDir) > 0 then
        outputFolderName.Caption := lDir
     else
         outputFolderName.Caption := 'input folder';
end; //outputFolderNameClick()

procedure Tdcm2niiForm.ResetMenuClick(Sender: TObject);
begin
  isAppDoneInitializing := false;
     readIni(true);
     isAppDoneInitializing := true;
     ProcessFile('');
end;

procedure Tdcm2niiForm.FormCreate(Sender: TObject);
begin
     readIni(false);
     {$IFDEF FPC}
     application.ShowButtonGlyphs:= sbgNever;
     {$ELSE}//Delphi specific
     DragAcceptFiles(dcm2niiForm.Handle, True);
     {$ENDIF}
     isAppDoneInitializing := true;
     //ProcessFile('');
end; //FormCreate()

procedure Tdcm2niiForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
     writeIni;
     {$IFNDEF FPC}
     DragAcceptFiles(dcm2niiForm.Handle, False);
     {$ENDIF}
end; //FormClose()

initialization
 //{$IFDEF FPC}
 // {$I dcm2nii.lrs}
 // {$ENDIF}
end.

