program npm;

uses
  Forms,
  npmform in 'npmform.pas' {MainForm},
  stats in 'stats.pas',
  spread in 'spread.pas' {SpreadForm},
  design in 'design.pas' {DesignForm},
  valformat in 'valformat.pas',
  ReadInt in 'ReadInt.pas' {ReadIntForm},
  firth in 'firth.pas',
  roc in 'roc.pas',
  prefs in 'prefs.pas';

{$R *.RES}
{$IFNDEF FPC}
{$R windowsxp.res}

{$ENDIF}

begin
  Application.Initialize;
  Application.Title := 'NPM';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSpreadForm, SpreadForm);
  Application.CreateForm(TDesignForm, DesignForm);
  Application.CreateForm(TReadIntForm, ReadIntForm);
  Application.Run;
end.
