program dcm2niigui;

uses
  Forms,
  gui in 'gui.pas' {MainForm},
  pref_form in 'pref_form.pas' {PrefsForm},
  nifti_form in 'nifti_form.pas' {NIfTIform},
  untar in 'untar.pas',
  convertsimple in 'convertsimple.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'dcm2niiGUI';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPrefsForm, PrefsForm);
  Application.CreateForm(TNIfTIform, NIfTIform);
  Application.Run;
end.
