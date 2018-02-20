unit landmarks;

interface
{$H+}


uses
    {$IFDEF Win32}
  Windows,  Messages,
{$ELSE}
  LMessages, LCLType,
{$ENDIF}
   {$IFDEF FPC}LResources, {$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ToolWin, ComCtrls;

type

  { TAnatForm }

  TAnatForm = class(TForm)
    ToolBar1: TToolBar;
    SaveBtn: TSpeedButton;
    AddBtn: TSpeedButton;
    ComboBox1: TComboBox;
    UpdateBtn: TSpeedButton;
    OpenBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure UpdateCombo;
    procedure OpenBtnClick(Sender: TObject);
    procedure Update(lIndex: integer);
    procedure UpdateBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure OpenAnat(lFilename: string);
    procedure CloseAnat;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AnatForm: TAnatForm;

implementation

uses nifti_img_view, nifti_img, nifti_hdr_view, define_types;
 {$IFNDEF FPC} //Delphi
{$R *.dfm}
{$ENDIF}
type
  TLandmark = record 
    Name: string;
    X,Y,Z: single;
  end;
  TLandmarkRA = array of TLandmark;
const
kAnatFilter = 'AnatomyFile|*.anat';
var
  gLandmarks: TLandmarkRA;
procedure TAnatForm.CloseAnat;
begin
  if length(gLandmarks) < 1 then
     exit;
  SetLength(gLandmarks,0);
     UpdateCombo;
end;
  
procedure TAnatForm.SaveBtnClick(Sender: TObject);
const
  kSep = chr(9);
var
  i: integer;
  lF: TextFile;
begin
  if length(gLandmarks) < 1 then begin
    showmessage('No landmarks open - either open a file or create new landmarks');
    exit;
  end;
  ImgForm.SaveDialog1.Filter := kAnatFilter;
     ImgForm.SaveDialog1.DefaultExt := '.anat';
  ImgForm.SaveDialog1.Filename := ChangeFileExt(ImgForm.SaveDialog1.Filename, ImgForm.SaveDialog1.DefaultExt); //10102006
  if not ImgForm.SaveDialog1.Execute then exit;
  Filemode := 0;
   AssignFile(lF, ImgForm.SaveDialog1.Filename);
   rewrite(lF);
   for i := 0 to length(gLandmarks)-1 do
    Writeln(lF, gLandmarks[i].Name+kSep+floattostr(gLandmarks[i].X)+kSep+floattostr(gLandmarks[i].Y)+kSep+floattostr(gLandmarks[i].Z)  );
  CloseFile(lF);

end;

procedure TAnatForm.FormCreate(Sender: TObject);
begin

end;

procedure TAnatForm.UpdateCombo;
var
  i: integer;
begin
//xxx
  ComboBox1.Items.Clear;
  if length(gLandmarks) < 1 then
    exit;
  for i := 0 to length(gLandmarks)-1 do
    ComboBox1.Items.Add(gLandmarks[i].Name);
  ComboBox1.ItemIndex := length(gLandmarks)-1;
  ComboBox1Change(nil);
end;


procedure TAnatForm.AddBtnClick(Sender: TObject);
var
  s: string;
  i: integer;
  lOK: boolean;
begin
  i := length(gLandmarks)+1;
  s := 'A'+inttostr(i);
  lOK := InputQuery('Enter a name', 'region name', s);
  if not lOK then
    exit;
  setlength(gLandmarks,i);
  gLandmarks[i-1].Name := s;
  Update(i-1);
  UpdateCombo;
end;

(*
 MMToImgCoord(lX,lY,lZ,lXmm,lYmm,lZmm);
 if lX <> ImgForm.XViewEdit.value then ImgForm.XViewEdit.value := lX;
 if lY <> ImgForm.YViewEdit.value then ImgForm.YViewEdit.value := lY;
 if lZ <> ImgForm.ZViewEdit.value then ImgForm.ZViewEdit.value := lZ;
   *)
procedure SetLandmark(index: integer);//indexed from 0
var
//lXmm,lYmm,lZmm: single;
lX,lY,lZ: integer;
begin
  if (index < 0) or (index >= length(gLandmarks)) then
    exit;
 MMToImgCoord(lX,lY,lZ,gLandmarks[index].X,gLandmarks[index].Y,gLandmarks[index].Z);
 if lX <> ImgForm.XViewEdit.value then ImgForm.XViewEdit.value := lX;
 if lY <> ImgForm.YViewEdit.value then ImgForm.YViewEdit.value := lY;
 if lZ <> ImgForm.ZViewEdit.value then ImgForm.ZViewEdit.value := lZ;
 ImgForm.XViewEditChange(nil);
end;

procedure TAnatForm.ComboBox1Change(Sender: TObject);
begin
  SetLandmark(ComboBox1.ItemIndex);
end;

function NextTab(lStr: string; var lP: integer): string;
//reports text prior to comma...
var
 len: integer;
begin
  result := '';
  len := length(lStr);
  if len < lP then exit;
  repeat
    if (lStr[lP] = chr(9){','})   then begin
      lP := lP + 1;
      exit;
    end;
    //if lStr[lP] <> ' ' then
      result := result + lStr[lP];
    lP := lP + 1;
  until (lP > len);
end;

procedure TAnatForm.OpenAnat(lFilename: string);
var
  st: string;
   sl: TStringList;
  n, line, col : integer;
begin
  if not Fileexists(lFilename) then begin
    CloseAnat;
    exit;
  end;
   //will load the TAB delimited TXT here
   sl := TStringList.Create;
   try
     //load the tab delimited txt file
     sl.LoadFromFile(lFilename) ;
     //for each tab delimited line
     n := 0;
     setlength(gLandmarks,sl.Count);
     for line := 0 to sl.Count-1 do begin
       st := sl[line];
       col := 1;
       if (NextTab(st,col) <> '') and  (NextTab(st,col) <> '') and(NextTab(st,col) <> '') and(NextTab(st,col) <> '')  then begin
          inc(n);
          col := 1;
          gLandmarks[line].Name := NextTab(st,col);
          gLandmarks[line].X := strtofloat(NextTab(st,col));
          gLandmarks[line].Y := strtofloat(NextTab(st,col));
          gLandmarks[line].Z := strtofloat(NextTab(st,col));
       end;
     end;
     setlength(gLandmarks,n);
   finally
     sl.Free;
   end;
   UpdateCombo;
    AnatForm.show;
end;


procedure TAnatForm.OpenBtnClick(Sender: TObject);
begin
     if not OpenDialogExecute(kAnatFilter,'Select background image',false) then exit;
   OpenAnat(HdrForm.OpenHdrDlg.Filename) ;
end;

procedure TAnatForm.Update(lIndex: integer);
var
  X,Y,Z: integer;
begin
  if lIndex >= Length(gLandmarks) then
    exit;
  X := round(ImgForm.XViewEdit.value);
  Y := round(ImgForm.YViewEdit.value);
  Z := round(ImgForm.ZViewEdit.value);
  ImgCoordToMM(X,Y,Z, gLandmarks[lIndex].X,gLandmarks[lIndex].Y,gLandmarks[lIndex].Z);
  ComboBox1Change(nil);
end;

procedure TAnatForm.UpdateBtnClick(Sender: TObject);
begin
  Update(ComboBox1.ItemIndex);

end;

procedure TAnatForm.DeleteBtnClick(Sender: TObject);
var
  p,i,l: integer;
begin
  l := Length(gLandmarks);
  i := ComboBox1.ItemIndex;
  if (l < 1) or (i >= l) or (i < 0) then
    exit;
  if i < (l-1) then
    for p := i+1 to l-1 do
      gLandmarks[p-1] := gLandmarks[p];
  SetLength(gLandmarks,l-1);
     UpdateCombo;
end;

initialization
{$IFDEF FPC}
{$I landmarks.lrs}
{$ENDIF}

end.

