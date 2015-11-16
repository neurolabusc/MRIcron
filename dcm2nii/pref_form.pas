unit pref_form;

interface

uses

{$IFDEF FPC}LResources,{$ENDIF}
  {$IFDEF UNIX}Process, {$ELSE}ShellApi, Windows,{$ENDIF}
//Messages,
SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,filename,define_types,dicomtypes,prefs, Spin, Buttons;

type
{$H+}
  { TPrefsForm }
  TPrefsForm = class(TForm)
   // TxtReportCheck: TCheckBox;
   // WritePrefsOnQuit: TCheckBox;
   // TextEditorBtn: TButton;
    Stack3DImagesWithSameAcqNum: TCheckBox;
    OutputCombo: TComboBox;
    FilenameBox: TGroupBox;
    DateCheck: TCheckBox;
    OutDirLabel: TLabel;
    CollapseCheck: TCheckBox;
    SeriesCheck: TCheckBox;
    ProtocolCheck: TCheckBox;
    PatientNameCheck: TCheckBox;
    InputNameCheck: TCheckBox;
    NotAnonymizeCheck: TCheckBox;
    ReorientCheck: TCheckBox;
    OKBtn: TButton;
    CancelBtn: TButton;
    RecursiveSpin: TSpinEdit;
    Label1: TLabel;
    TextEditorBtn: TButton;
    WritePrefsOnQuit: TCheckBox;
    TxtReportCheck: TCheckBox;
    //Stack3DImagesWithSameAcqNum: TCheckBox;
    //CollapseCheck: TCheckBox;
    procedure FilenameChecks(Sender: TObject);
    procedure OutputComboMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ReadPrefs(var lPrefs: TPrefs);
    procedure TextEditorBtnClick(Sender: TObject);
    procedure WritePrefs(var lPrefs: TPrefs);
    procedure SetOutDirLabel;
    procedure OutputComboChange(Sender: TObject);
    procedure SetOutput;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PrefsForm: TPrefsForm;

implementation
{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}
uses gui,userdir;
var
   gPrefs: TPrefs;
procedure TPrefsForm.SetOutDirLabel;
begin
     OutDirLabel.visible :=  (OutputCombo.ItemIndex = kOutDirModeOutDir);
end;

procedure TPrefsForm.WritePrefs(var lPrefs: TPrefs);
begin
     gPrefs := lPrefs;
     Stack3DImagesWithSameAcqNum.Checked := lPrefs.Stack3DImagesWithSameAcqNum;
     DateCheck.Checked := lPrefs.AppendDate;
     SeriesCheck.Checked := lPrefs.AppendAcqSeries;
     ProtocolCheck.Checked := lPrefs.AppendProtocolName;
     PatientNameCheck.Checked := lPrefs.AppendPatientName;
     InputNameCheck.checked := lPrefs.AppendFilename;
     CollapseCheck.checked := lPrefs.CollapseFolders;
     ReorientCheck.Checked := (lPrefs.MinReorientMatrix < 32000);
     NotAnonymizeCheck.Checked := not lPrefs.Anonymize;
     TxtReportCheck.Checked := lPrefs.TxtReport;
     RecursiveSpin.Value := lPrefs.RecursiveFolderDepth;
     OutDirLabel.Caption := lPrefs.OutDir;
     if (lPrefs.OutDirMode < 0) or (lPrefs.OutDirMode >= OutputCombo.Items.count) then
        lPrefs.OutDirMode := 0;
     OutputCombo.ItemIndex := lPrefs.OutDirMode;
     SetOutDirLabel;
end;

procedure TPrefsForm.ReadPrefs(var lPrefs: TPrefs);
begin
    lPrefs := gPrefs;
    lPrefs.Stack3DImagesWithSameAcqNum := Stack3DImagesWithSameAcqNum.checked;
    lPrefs.AppendDate := DateCheck.Checked;
    lPrefs.AppendAcqSeries := SeriesCheck.Checked;
    lPrefs.AppendProtocolName := ProtocolCheck.Checked;
    lPrefs.AppendPatientName := PatientNameCheck.Checked;
    lPrefs.AppendFilename := InputNameCheck.checked;
    //lPrefs.SaveToBaseFolder := SaveToBaseFolderCheck.Checked;
    lPrefs.CollapseFolders := CollapseCheck.checked;
    if ReorientCheck.Checked then begin
       if lPrefs.MinReorientMatrix = MaxInt then
          lPrefs.MinReorientMatrix := kMinReorientMatrix
    end else
        lPrefs.MinReorientMatrix := MaxInt;
    lPrefs.Anonymize := not NotAnonymizeCheck.Checked;
    lPrefs.TxtReport:= TxtReportCheck.Checked;
    lPrefs.RecursiveFolderDepth := RecursiveSpin.Value;
    lPrefs.OutDir := OutDirLabel.Caption;
    lPrefs.OutDirMode := OutputCombo.ItemIndex;
    lPrefs.WritePrefsOnQuit := WritePrefsOnQuit.Checked;
end;


procedure TPrefsForm.TextEditorBtnClick(Sender: TObject);
{$IFDEF UNIX}
var
  AProcess: TProcess;
begin
    Showmessage('Preferences will be opened in a text editor. The program '+ExtractFilename(paramstr(0))+' will now quit, so that the file will not be overwritten.');
    MainForm.SavePrefs;
    AProcess := TProcess.Create(nil);
    {$IFDEF UNIX}
            {$IFDEF Darwin}
            AProcess.CommandLine := 'open -a TextEdit '+IniName;
            {$ELSE}
            AProcess.CommandLine := 'open -a gedit '+IniName;
            {$ENDIF}
    {$ELSE}
          AProcess.CommandLine := 'notepad '+IniName;
    {$ENDIF}
  //AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  AProcess.Free;
  WritePrefsOnQuit.checked := false;
  MainForm.close;
end;
{$ELSE} //ShellExecute(Handle,'open', 'c:\windows\notepad.exe','c:\SomeText.txt', nil, SW_SHOWNORMAL) ;
begin
    Showmessage('Preferences will be opened in a text editor. The program '+ExtractFilename(paramstr(0))+' will now quit, so that the file will not be overwritten.');
   MainForm.SavePrefs;
    ShellExecute(Handle,'open', 'notepad.exe',PAnsiChar(AnsiString(IniName)), nil, SW_SHOWNORMAL) ;
  WritePrefsOnQuit.checked := false;
  MainForm.close;
end;
{$ENDIF}

procedure TPrefsForm.FilenameChecks(Sender: TObject);
var
   lDICOMImgName: string;
   lDicomData: DICOMdata;
   lPrefs: TPrefs;
begin
   Clear_Dicom_Data(lDicomData);
   SetDefaultPrefs (lPrefs);
   ReadPrefs(lPrefs);
  clear_dicom_data(lDicomData);
   lDICOMImgName:= 'IM60';
   lDicomData.PatientName := 'JOHN_DOE';
   lDicomData.ProtocolName := 'T1';
   //FilenameBox.Caption := 'Output Filename ('+  OutputFilename(lDicomImgName,lDicomData,lPrefs.AppendDate,lPrefs.AppendAcqSeries,lPrefs.AppendProtocolName,lPrefs.AppendPatientName,lPrefs.FourD,lPrefs.AppendFilename)+')';
   Caption := 'Output: '+  OutputFilename(lDicomImgName,lDicomData,lPrefs);
end;



procedure TPrefsForm.SetOutput;
begin
      SetOutDirLabel;
  if not (OutputCombo.ItemIndex = kOutDirModeOutDir) then
     exit;
  OutDirLabel.Caption := GetDirPrompt(OutDirLabel.Caption);
end;

procedure TPrefsForm.OutputComboMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//for OSX
begin
 {$IFDEF Darwin}
   SetOutput;
 {$ENDIF}
end;

procedure TPrefsForm.OutputComboChange(Sender: TObject);
//for all OSes except OSX...
begin
{$IFNDEF Darwin}
     SetOutput;
{$ENDIF}
end;

initialization
{$IFDEF FPC}
  {$I pref_form.lrs}
 {$ENDIF}

end.