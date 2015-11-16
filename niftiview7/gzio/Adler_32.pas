unit Adler_32;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DsgnIntf;

type
  TZeroHundred = 0..100;

  TAboutProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
	function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TAdler32 = class(TComponent)
  private
	{ Private declarations }
	FAbout : TAboutProperty;
	FAdler32FileName : String;
	FWindowOnError : boolean;
	FOnProgress : TNotifyEvent;
	FProgress : integer;
	FProgressStep : TZeroHundred;
  protected
	{ Protected declarations }
	procedure DoOnProgress; virtual;
  public
	{ Public declarations }
	constructor Create( AOwner: TComponent); override;
	function CalcAdler32 : longint;
	function CalcAdler32_hex : string;
	property Progress : integer
		 read FProgress write FProgress;
  published
	{ Published declarations }
	property About: TAboutProperty
		 read FAbout write FAbout;
	property Adler32FileName : String
		 read FAdler32FileName write FAdler32FileName;
	Property WindowOnError : boolean
		 read FWindowOnError write FWindowOnError;
	property ProgressStep : TZeroHundred
		 read FProgressStep write FProgressStep;
	property OnProgress : TNotifyEvent
		 read FOnProgress write FOnProgress;
  end;

procedure Register;

implementation

uses adler, zutil, utils;

constructor TAdler32.Create( AOwner: TComponent);
begin
   inherited Create( AOwner);
   WindowOnError := True;
   FProgressStep := 0
end;

procedure TAdler32.DoOnProgress;
begin
	if Assigned (FOnProgress) then
	   FOnProgress (self)
end;

function TAdler32.CalcAdler32 : longint;
var adler : uLong;
	len : integer;
	buffer : array [0..BUFLEN-1] of Byte;
	infile : file;
	fsize, lensize : LongInt{LongWord};
begin
   adler := 0;
   if FileExists( FAdler32Filename) then begin
	  AssignFile(infile, FAdler32FileName);
	  Reset(infile, 1);

	  adler := adler32(0, NIL, 0);

	  FProgress := 0;
	  fsize := FileSize(infile);
	  lensize := 0;
	  if FProgressStep > 0 then DoOnProgress;

	  while true do begin
		 blockread (infile, buffer, BUFLEN, len);
		 if len=0 then break;
		 adler := adler32(adler, @buffer, len);

		 if FProgressStep > 0 then begin
			{$WARNINGS OFF}
			lensize := lensize + len;
			if ((lensize / fsize) * 100 >= FProgress + FProgressStep)
							or (lensize = fsize) then begin
			   FProgress := Trunc((lensize / fsize) * 100);
			   DoOnProgress
			end
			{$WARNINGS ON}
		 end
	  end;
	  CloseFile(infile)
   end
   else if FWindowOnError then
		 MessageDlg('File '+FAdler32FileName+' does not exist.',
					mtError, [mbAbort], 0);
   CalcAdler32 := adler
end;

function TAdler32.CalcAdler32_hex : string;
var utils : TUtils;
begin
   CalcAdler32_hex := utils.HexToStr(CalcAdler32)
end;

procedure TAboutProperty.Edit;
var utils : TUtils;
begin
  ShowMessage(utils.CreateAboutMsg('DelphiAdler32'))
end;

function TAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paReadOnly];
end;

function TAboutProperty.GetValue: string;
begin
  Result := 'DelphiAdler32';
end;

procedure Register;
begin
  RegisterComponents('Samples', [TAdler32]);
  RegisterPropertyEditor(TypeInfo(TAboutProperty), TAdler32, 'ABOUT', TAboutProperty);
end;

end.
