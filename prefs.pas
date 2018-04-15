unit prefs;

{$mode objfpc}{$H+}

interface

uses
   {$IFDEF Windows} ShellAPI, Windows, {$ENDIF} //x18
    userdir, Process,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Buttons;

type

  { TPrefForm }

  TPrefForm = class(TForm)
    AdvancedBtn: TButton;
    DarkModeCheck: TCheckBox;
    Label4: TLabel;
    SingleRowCheck: TCheckBox;
    OrthoCheck: TCheckBox;
    FontEdit1: TSpinEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    ThinPenCheck: TCheckBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ResliceCheck: TCheckBox;
    GroupBox1: TGroupBox;
    MaxDimEdit: TSpinEdit;
    ThreadEdit: TSpinEdit;
    SigDigEdit: TSpinEdit;
    XBarClr: TButton;
    procedure Quit2TextEditor;
    procedure AdvancedBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure ResliceCheckClick(Sender: TObject);
    procedure SingleRowCheckChange(Sender: TObject);
    procedure XBarClrClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  PrefForm: TPrefForm;

implementation

uses
    nifti_img_view;
{ TPrefForm }

procedure TPrefForm.CancelBtnClick(Sender: TObject);
begin
       Close;
end;

procedure TPrefForm.Quit2TextEditor;
{$IFDEF UNIX}
var
  AProcess: TProcess;
  {$IFDEF LINUX} I: integer; EditorFName : string; {$ENDIF}
begin
    {$IFDEF LINUX}
    EditorFName := FindDefaultExecutablePath('gedit');
   if EditorFName = '' then
     EditorFName := FindDefaultExecutablePath('tea');
    if EditorFName = '' then
      EditorFName := FindDefaultExecutablePath('nano');
    if EditorFName = '' then
      EditorFName := FindDefaultExecutablePath('pico');
    if EditorFName = '' then begin
       Showmessage(ExtractFilename(paramstr(0))+' will now quit. You can then use a text editor to modify the file '+IniName);
       Clipboard.AsText := EditorFName;
    end else begin
      EditorFName := '"'+EditorFName +'" "'+IniName+'"';
      Showmessage(ExtractFilename(paramstr(0))+' will now quit. Modify the settings with the command "'+EditorFName+'"');
         AProcess := TProcess.Create(nil);
         AProcess.InheritHandles := False;
         AProcess.Options := [poNewProcessGroup, poNewConsole];
         AProcess.ShowWindow := swoShow;
        for I := 1 to GetEnvironmentVariableCount do
            AProcess.Environment.Add(GetEnvironmentString(I));
         AProcess.Executable := EditorFName;
         AProcess.Execute;
         AProcess.Free;
    end;
    Clipboard.AsText := EditorFName;
    GLForm1.close;
    exit;
    {$ENDIF}
    Showmessage('Preferences will be opened in a text editor. The program '+ExtractFilename(paramstr(0))+' will now quit, so that the file will not be overwritten.');
    AProcess := TProcess.Create(nil);
    {$IFDEF UNIX}
      //AProcess.CommandLine := 'open -a TextEdit '+IniName;
      AProcess.Executable := 'open';
      AProcess.Parameters.Add('-e');
      AProcess.Parameters.Add(IniName);
    {$ELSE}
      AProcess.CommandLine := 'notepad '+IniName;
    {$ENDIF}
   //Clipboard.AsText := AProcess.CommandLine;
  //AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  AProcess.Free;
  ImgForm.close;
end; //Quit2TextEditor()
{$ELSE}
begin
  gBGImg.SaveDefaultIni := false;
  //  Showmessage('Preferences will be opened in a text editor. The program '+ExtractFilename(paramstr(0))+' will now quit, so that the file will not be overwritten.');
   //GLForm1.SavePrefs;
    ShellExecute(Handle,'open', 'notepad.exe',PAnsiChar(AnsiString(IniName)), nil, SW_SHOWNORMAL) ;
  //WritePrefsOnQuit.checked := false;
  ImgForm.close; //x18
end;

{$ENDIF}

procedure TPrefForm.AdvancedBtnClick(Sender: TObject);
begin
	Quit2TextEditor;
end;

procedure TPrefForm.FormCreate(Sender: TObject);
begin

end;

procedure TPrefForm.FormShow(Sender: TObject);
begin
    //RGBPlanarCheck.checked := gBGImg.isPlanarRGB;
       ResliceCheck.checked := gBGImg.ResliceOnLoad;
       //OrthoCheck.Visible := not gBGImg.ResliceOnLoad;
       OrthoCheck.checked := gBGImg.OrthoReslice;

       MaxDimEdit.value := gBGImg.MaxDim;
     ThreadEdit.value := gnCPUThreads;
     //DrawCheck.checked := ImgForm.ToolPanel.Visible;
     ThinPenCheck.Checked := gBGImg.ThinPen;
     SigDigEdit.value := gBGImg.SigDig;
     SingleRowCheck.checked := gBGImg.SingleRow;
     FontEdit1.Value := gBGImg.FontSize;
     {$IFDEF LCLCocoa}
     DarkModeCheck.visible := true;
     DarkModeCheck.Checked := gBGImg.DarkMode;
     {$ENDIF}

end;

procedure TPrefForm.OKBtnClick(Sender: TObject);
begin
     OKBtn.SetFocus;
     //gBGImg.isPlanarRGB := RGBPlanarCheck.checked;
     gBGImg.ResliceOnLoad := ResliceCheck.checked;
          gBGImg.OrthoReslice := OrthoCheck.checked;
     gBGImg.MaxDim := MaxDimEdit.value;
     gnCPUThreads := ThreadEdit.value;
     //ImgForm.ToolPanel.Visible := DrawCheck.checked;
     //ImgForm.DrawMenu.Visible := DrawCheck.checked;
     gBGImg.ThinPen := ThinPenCheck.Checked;
     if (gBGImg.FontSize <> FontEdit1.Value) then begin
       gBGImg.FontSize := FontEdit1.Value;
       if gBGImg.FontSize < 6 then gBGImg.FontSize := 6;
       if gBGImg.FontSize > 128 then gBGImg.FontSize := 128;
       ImgForm.RefreshImagesTimer.enabled := true;

     end;
     gBGImg.SigDig := SigDigEdit.value;
     {$IFDEF LCLCocoa}
     if gBGImg.DarkMode <> DarkModeCheck.Checked then begin
        gBGImg.DarkMode := DarkModeCheck.Checked;
        ImgForm.SetDarkMode;
     end;
     {$ENDIF}
     if gBGImg.SingleRow <> SingleRowCheck.Checked then begin
        gBGImg.SingleRow := SingleRowCheck.Checked;
        ImgForm.DefaultControlPanel;
        ImgForm.RefreshImagesTimer.enabled := true;
     end;
     Close;
end;

procedure TPrefForm.ResliceCheckClick(Sender: TObject);
begin
    OrthoCheck.Visible := not ResliceCheck.checked;
end;

procedure TPrefForm.SingleRowCheckChange(Sender: TObject);
begin

end;

procedure TPrefForm.XBarClrClick(Sender: TObject);
begin
  ImgForm.XBarColor;
  PrefForm.BringToFront;
end;

initialization
  {$I prefs.lrs}

end.

