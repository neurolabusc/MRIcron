program dcm2niigui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, gui, nifti_form, pref_form, dialogs_msg;
  
  


//{$R dcm2niigui.res}

begin
  Application.Title:='dcm2niigui.exe';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TNIfTIForm, NIfTIForm);
  Application.CreateForm(TPrefsForm, PrefsForm);
  Application.Run;
end.

