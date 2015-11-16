unit Crc_32;

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

  TCrc32 = class(TComponent)
  private
	{ Private declarations }
	FAbout : TAboutProperty;
	FCrc32FileName : String;
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
	function CalcCrc32 : longint;
	function CalcCrc32_hex : string;
	property Progress : integer
		 read FProgress write FProgress;
  published
	{ Published declarations }
	property About: TAboutProperty
		 read FAbout write FAbout;
	property Crc32FileName : String
		 read FCrc32FileName write FCrc32FileName;
	Property WindowOnError : boolean
		 read FWindowOnError write FWindowOnError;
	property ProgressStep : TZeroHundred
		 read FProgressStep write FProgressStep;
	property OnProgress : TNotifyEvent
		 read FOnProgress write FOnProgress;
  end;

procedure Register;

implementation

uses crc, zutil, utils;

constructor TCrc32.Create( AOwner: TComponent);
begin
   inherited Create( AOwner);
   WindowOnError := True;
   FProgressStep := 0
end;

procedure TCrc32.DoOnProgress;
begin
	if Assigned (FOnProgress) then

	   FOnProgress (self)
end;

function TCrc32.CalcCrc32 : longint;
var crc : uLong;
	len : integer;
	buffer : array [0..BUFLEN-1] of Byte;
	infile : file;
	fsize, lensize : LongInt{LongWord};
begin
   crc := 0;
   if FileExists( FCrc32Filename) then begin
	  AssignFile(infile, FCrc32FileName);
	  Reset(infile, 1);

	  crc := crc32(0, NIL, 0);

	  FProgress := 0;
	  fsize := FileSize(infile);
	  lensize := 0;
	  if FProgressStep > 0 then DoOnProgress;

	  while true do begin
		 blockread (infile, buffer, BUFLEN, len);
		 if len=0 then break;
		 crc := crc32(crc, @buffer, len);

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
	 CloseFile(infile);
   end
   else if FWindowOnError then
		 MessageDlg('File '+FCrc32FileName+' does not exist.',
					mtError, [mbAbort], 0);
   CalcCrc32 := crc
end;

function TCrc32.CalcCrc32_hex : string;
var utils : TUtils;
begin
   CalcCrc32_hex := utils.HexToStr(CalcCrc32)
end;

procedure TAboutProperty.Edit;
var utils : TUtils;
begin
  ShowMessage(utils.CreateAboutMsg('DelphiCrc32'))
end;

function TAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paReadOnly];
end;

function TAboutProperty.GetValue: string;
begin
  Result := 'DelphiCrc32';
end;

procedure Register;
begin
  RegisterComponents('Samples', [TCrc32]);
  RegisterPropertyEditor(TypeInfo(TAboutProperty), TCrc32, 'ABOUT', TAboutProperty);
end;

end.
 