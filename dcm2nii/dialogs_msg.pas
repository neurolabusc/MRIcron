unit dialogs_msg;
{$ifdef fpc}{$mode delphi}{$endif}
{$Include ..\common\isgui.inc}
interface
 //this wrapper sends text to the main form memo for GUI applications and to the terminal for console applications
uses
  Classes, SysUtils;

procedure dcmMsg (lStr: string);

implementation
{$IFDEF GUI}
uses gui;
{$ENDIF}

procedure dcmMsg (lStr: string);
begin
{$IFDEF GUI}
         MainForm.Memo1.Lines.Add(lStr);
         MainForm.refresh;
{$ELSE}
        writeln(lStr)
{$ENDIF}
end;

end.

