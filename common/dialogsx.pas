unit dialogsx;
{$Include isgui.inc}
{$H+}
interface
uses
        //,IniFiles
SysUtils;
                                      
type
  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TMsgDlgButtons = set of TMsgDlgBtn;
  TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);

//procedure Msg (lStr: string);
procedure ShowMsg (lStr: string);
procedure msgfx (a,b,c,d: double); overload; //fx used to help debugging - reports number values
function MsgDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Word;
function GetInt(lStr: string; lMin,lDefault,lMax: integer): integer;
procedure MyReadLn;//no GUI: waits for user
function GetStr(lPrompt: string): string;


 {$IFNDEF GUI}procedure ShowMessage (lStr: string); {$ENDIF}
//procedure vx (a,b,c,d: double); 

const
 mrCancel = 2;
 mrAbort = 1;//	idAbort
 mrNo = 0;
implementation
{$IFDEF GUI}uses readint,dialogs; {$ENDIF}

procedure Msg (lStr: string);
begin
{$IFDEF GUI}
         showmessage(lStr);
{$ELSE}
        writeln(lStr)
{$ENDIF}
end;

{$IFNDEF GUI}procedure ShowMessage (lStr: string);
begin
writeln(lStr) ;
end;

{$ENDIF}

procedure vx (a,b,c,d: double);  //vx used to help debugging - reports number values
begin
msg(floattostr(a)+':'+floattostr(b)+':'+floattostr(c)+':'+floattostr(d));
end;


procedure MyReadLn;
{$IFDEF GUI}
begin
  //do nothing
end;
{$ELSE}
begin
  {$IFNDEF UNIX}
 if IsConsole then
		ReadLn;
   {$ENDIF}
end;
{$ENDIF}

function MsgDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Word;
{$IFDEF GUI}
var
   lDlgType : Dialogs.TMsgDlgType;
   lButtons: Dialogs.TMsgDlgButtons;

begin
  lDlgType :=  Dialogs.TMsgDlgType(DlgType);
  lButtons:= Dialogs.TMsgDlgButtons(Buttons);
  result := MessageDlg(Msg, lDlgType, lButtons,HelpCtx);
{$ELSE}
begin
  result := 0;
  writeln('WARNING: dialogs not being used. Unabled to process this '+Msg);
{$ENDIF}
end;

procedure ShowMsg (lStr: string);
begin
{$IFDEF GUI}
         ShowMessage(lStr);
{$ELSE}
        writeln(lStr)
{$ENDIF}
end;
procedure msgfx (a,b,c,d: double); overload; //fx used to help debugging - reports number values
begin
    {$IFDEF GUI}
	msg(floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c)+'x'+floattostr(d));
    {$ELSE}
	msg(floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c)+'x'+floattostr(d));
    {$ENDIF}
end;



function GetStr(lPrompt: string): string;
{$IFDEF GUI}
var
   lOK: boolean;
begin
    lOK := InputQuery(lPrompt, lPrompt, result);
    if not lOK then
       result := '';
end;
{$ELSE}
var
   lS: string;
begin
      writeln ( lPrompt);
      readln(lS);
      result := lS;
end;
{$ENDIF}

function GetInt(lStr: string; lMin,lDefault,lMax: integer): integer;
{$IFDEF GUI}
begin
    //result := GetInt(lStr, lMin,lDefault,lMax);
    result := lDefault;
    Showmessage('Warning - unable to get values for '+lStr);
end;
{$ELSE}
var
   lS: string;
   lError,lI: integer;
begin
      writeln ( lStr+' ['+inttostr(lMin)+'..'+inttostr(lMax)+'], default '+inttostr(lDefault));
      readln(lS);
      Val(lS,lI,lError);
      if lError = 0 then
         result  := round(lI)
      else begin
          writeln(inttostr(lDefault));
          result := lDefault;
      end;
      if result < lMin then
         result := lMin;
      if result > lMax then
         result := lMax;
end;
{$ENDIF}


end.
 