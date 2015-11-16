unit landmarks;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ToolWin, ComCtrls;

type
  TAnatForm = class(TForm)
    ToolBar1: TToolBar;
    SaveBtn: TSpeedButton;
    AddBtn: TSpeedButton;
    ComboBox1: TComboBox;
    UpdateBtn: TSpeedButton;
    OpenBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
    procedure SaveBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure UpdateCombo;
    procedure OpenBtnClick(Sender: TObject);
    procedure Update(lIndex: integer);
    procedure UpdateBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AnatForm: TAnatForm;

implementation

uses nifti_img_view, nifti_img, nifti_hdr_view, define_types;

{$R *.dfm}
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
  CloseFile(lF); (**)

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

procedure TAnatForm.OpenBtnClick(Sender: TObject);
var
  //lFilename: string;
  //i: integer;
  //lX,lY,lZ
  st: string;
   sl{, slRow} : TStringList;
  n, line, col : integer;
begin
	 if not OpenDialogExecute(kAnatFilter,'Select background image',false) then exit;
   //will load the TAB delimited TXT here
   sl := TStringList.Create;
   //will process each TAB delimited line here
   //slRow := TStringList.Create;
   //slRow.StrictDelimiter := true;
   //slRow.Delimiter := #9; //TAB
   try
     //load the tab delimited txt file
     sl.LoadFromFile(HdrForm.OpenHdrDlg.Filename) ;
     //StringGrid1.RowCount := sl.Count;

     //for each tab delimited line
     n := 0;
     setlength(gLandmarks,sl.Count);
     for line := 0 to sl.Count-1 do begin
       //"load" the line into a stringlist
       //slRow.DelimitedText := sl[line];
       st := sl[line];
       col := 1;
       if (NextTab(st,col) <> '') and  (NextTab(st,col) <> '') and(NextTab(st,col) <> '') and(NextTab(st,col) <> '')  then begin
       inc(n);
       col := 1;
       gLandmarks[line].Name := NextTab(st,col);
       //showmessage(gLandmarks[line].Name+' '+NextTab(st,col));
       gLandmarks[line].X := strtofloat(NextTab(st,col));
       gLandmarks[line].Y := strtofloat(NextTab(st,col));
       gLandmarks[line].Z := strtofloat(NextTab(st,col));
     end;
       //StringGrid1.Rows[line].Assign(slRow);
       {gLandmarks[line].Name := slRow.Strings[0];
       gLandmarks[line].X := strtofloat(slRow.Strings[1]);
       gLandmarks[line].Y := strtofloat(slRow.Strings[2]);
       gLandmarks[line].Z := strtofloat(slRow.Strings[3]); }
     end;
     setlength(gLandmarks,n);
   finally
     //slRow.Free;
     sl.Free;
   end;
   UpdateCombo;
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

end.

