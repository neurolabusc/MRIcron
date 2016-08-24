program npm;

{$mode objfpc}{$H+}
{$I options.inc}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, npmform,stats
,nifti_hdr,valformat, part, gzio2, StatThds,
  StatThdsUtil, brunner, statcr, distr, GraphicsMathLibrary, define_types,
  ReadInt
  {$IFDEF SPREADSHEET}  ,design,spread{$ENDIF};
  {$IFNDEF FPC}
{$R npm.res} 
{$ENDIF}

begin
  Application.Title:='NPM';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
    {$IFDEF SPREADSHEET}
  Application.CreateForm(TSpreadForm, SpreadForm);
  Application.CreateForm(TDesignForm, DesignForm);
  {$ENDIF}
  Application.CreateForm(TReadIntForm, ReadIntForm);
  Application.Run;
end.

