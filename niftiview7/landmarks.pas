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
  TAnatForm = class(TForm)
    ToolBar1: TToolBar;
    SaveBtn: TSpeedButton;
    AddBtn: TSpeedButton;
    UpdateBtn: TSpeedButton;
    OpenBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
    AnatDrop: TComboBox;
    procedure SaveBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure AnatDropChange(Sender: TObject);
    procedure UpdateCombo;
    procedure OpenBtnClick(Sender: TObject);
    procedure Update(lIndex: integer);
    procedure UpdateBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure OpenAnat(lFilename: string);
    procedure CloseAnat;
    procedure ComputeRMS;
    procedure AcceptLandmark;
    procedure BatchLandmarks;
    procedure LoadLandmark;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AnatForm: TAnatForm;

implementation

uses nifti_img_view, nifti_img, nifti_hdr_view, define_types, Text;
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
  gBatchImg,gBatchMark: integer;
//  gBatchName: string;

function BatchName (lImg: integer): string;
begin
  result := ChangeFileExt(HdrForm.OpenHdrDlg.Files[lImg-1], '.anat'); //10102006
end;

procedure SaveLandmarks (Filename: string);
const
  kSep = chr(9);
var
  i: integer;
  lF: TextFile;
begin
  Filemode := 0;
  AssignFile(lF,Filename);
  rewrite(lF);
   for i := 0 to length(gLandmarks)-1 do
    Writeln(lF, gLandmarks[i].Name+kSep+floattostr(gLandmarks[i].X)+kSep+floattostr(gLandmarks[i].Y)+kSep+floattostr(gLandmarks[i].Z)  );
  CloseFile(lF);
end;

procedure TAnatForm.LoadLandmark;
var
  lI: string;
begin
  if (gBatchImg < 1) or (gBatchMark < 1) then exit;//or (gBatchImg > HdrForm.OpenHdrDlg.Files.Count) or (gBatchMark >  length(gLandmarks)) then exit;
  lI := HdrForm.OpenHdrDlg.Files[gBatchImg-1];
	 ImgForm.CloseImagesClick(nil);
	 ImgForm.OpenAndDisplayImg(lI,True);
  OpenAnat(BatchName(gBatchImg));
  AnatDrop.ItemIndex := gBatchMark-1;
  ImgForm.Caption := 'Img '+inttostr(gBatchImg)+'/'+inttostr(HdrForm.OpenHdrDlg.Files.Count)+' Landmark '+inttostr(gBatchMark)+'/'+inttostr( length(gLandmarks));
  AnatDropChange(nil);
end;

procedure TAnatForm.AcceptLandmark;
var
  lSaveName: string;
begin
  Update(AnatDrop.ItemIndex);
  lSaveName := ChangeFileExt(HdrForm.OpenHdrDlg.Files[gBatchImg-1], '.anat'); //10102006
  SaveLandmarks(lSaveName);
  if gBatchImg >=  HdrForm.OpenHdrDlg.Files.Count  then begin
    gBatchImg := 1;
    if gBatchMark = length(gLandmarks) then begin
      //ImgForm.AcceptLandmark1.Enabled := false;
      showmessage('Done with landmarks');
    end else
      inc(gBatchMark);
  end else
    inc(gBatchImg);
  LoadLandmark;
end;

procedure TAnatForm.BatchLandmarks;
var
  lTemplateName, lBatchName: string;
  lI,lPrevAnat: integer;
begin
  if not OpenDialogExecute(kAnatFilter,'Select landmark template',false) then exit;
  lTemplateName :=  HdrForm.OpenHdrDlg.Filename;
  // OpenAnat(HdrForm.OpenHdrDlg.Filename) ;
  if not OpenDialogExecute(kImgFilter,'Select image[s] to create landmarks',true) then exit;
  // length(gLandmarks)
  //for lInc := 1 to HdrForm.OpenHdrDlg.Files.Count do begin //vcx
  //  lFilename := HdrForm.OpenHdrDlg.Files[lInc-1];
  gBatchImg := 1;
  gBatchMark := 1;
  //make anatomy files for each image
  lPrevAnat := 0;
  for lI := 1 to HdrForm.OpenHdrDlg.Files.Count do begin
    lBatchName := BatchName(lI);
    if fileexists(lBatchName) then
      inc(lPrevAnat)
    else
      CopyFile(PChar(lTemplateName), PChar(lBatchName), False);//SaveLandmarks(lBatchName);
  end;
  if lPrevAnat > 0 then
    Showmessage('Warning: '+inttostr(lPrevAnat)+' pre-existing .anat files were not overwritten! Lets hope these have the correct order/number of landmarks.');
  LoadLandmark;
  if (length(gLandmarks)< 1) or (HdrForm.OpenHdrDlg.Files.Count < 1) then begin
    showmessage('Error loading template anatomy file!');
    exit;
  end;
  Showmessage('Press F10 to position the '+ inttostr(length(gLandmarks))+' landmarks to '+inttostr(HdrForm.OpenHdrDlg.Files.Count)+' images');


  //CR ImgForm.AcceptLandmark1.Enabled := true;
end;

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

procedure TAnatForm.UpdateCombo;
var
  i: integer;
begin
//xxx
  AnatDrop.Items.Clear;
  if length(gLandmarks) < 1 then
    exit;
  for i := 0 to length(gLandmarks)-1 do
    AnatDrop.Items.Add(gLandmarks[i].Name);
  AnatDrop.ItemIndex := length(gLandmarks)-1;
  AnatDropChange(nil);
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

procedure TAnatForm.AnatDropChange(Sender: TObject);
begin
  SetLandmark(AnatDrop.ItemIndex);
end;

function NextTab(lStr: string; var lP: integer): string;
//reports text prior to tab...
var
 len: integer;
begin
  result := '';
  len := length(lStr);
  if len < lP then exit;
  repeat
    if (lStr[lP] = chr(9))   then begin
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

procedure RMS (lS,lT: TLandmark;var nummatch: integer; var summatch: double);
var
  DX: double;
begin
  DX := sqrt(sqr(lS.X-lT.X)+sqr(lS.Y-lT.Y)+sqr(lS.Z-lT.Z));
  summatch := summatch + DX;
  nummatch := nummatch+ 1;
end;

procedure TAnatForm.ComputeRMS;
const
  kTab= chr(9);
var
  lSource: TLandmarkRA;
	lStrings: TStringList;
	lInc,l,p,t,s,nummatch:integer;
  meanRMS,summatch: double;
  lFilename: string;
begin
	lStrings := TStringList.Create;
  if not OpenDialogExecute(kAnatFilter,'Select template landmark file',false) then exit;
  OpenAnat(HdrForm.OpenHdrDlg.Filename) ;
  l := Length(gLandmarks);
  if l < 1 then exit;
  SetLength(lSource,l);
  for p := 0 to l-1 do
    lSource[p] := gLandmarks[p];
	lStrings := TStringList.Create;
  lStrings.Add('Source='+kTab+HdrForm.OpenHdrDlg.Filename);
  if not OpenDialogExecute(kAnatFilter,'Select test landmark file[s]',true) then exit;
  TextForm.MemoT.Lines.Clear;
  if HdrForm.OpenHdrDlg.Files.Count < 1 then
    exit;
  for lInc := 1 to HdrForm.OpenHdrDlg.Files.Count do begin //vcx
    lFilename := HdrForm.OpenHdrDlg.Files[lInc-1];
  OpenAnat(lFilename) ;
  nummatch := 0;
  summatch := 0;
  t := Length(gLandmarks);
  if t > 0 then begin
    for l := 0 to l-1 do begin
      for s := 0 to t-1 do begin
        if lSource[l].Name = gLandmarks[s].Name then
          RMS(lSource[l],gLandmarks[s],nummatch,summatch);
      end; //for each item in target
    end;//for each item in source
  end;
  if nummatch < 1 then
    meanRMS := 0
  else
    meanRMS := summatch/nummatch;
  lStrings.Add('Target='+kTab+lFilename+kTab+'N='+kTab+inttostr(nummatch)+kTab+'RMS'+kTab+floattostr(summatch)+kTab+'MeanRMS'+kTab+floattostr(meanRMS));

 end;
 TextForm.MemoT.Lines.AddStrings(lStrings);
  lStrings.Free;
  SetLength(lSource,0);
  TextForm.Show;
end;

procedure TAnatForm.OpenBtnClick(Sender: TObject);
begin
  if (ssShift in KeyDataToShiftState(vk_Shift))  then begin
    ComputeRMS;
    exit;
  end;
  if not OpenDialogExecute(kAnatFilter,'Select landmark file',false) then exit;
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
  AnatDropChange(nil);
end;

procedure TAnatForm.UpdateBtnClick(Sender: TObject);
begin
  Update(AnatDrop.ItemIndex);

end;

procedure TAnatForm.DeleteBtnClick(Sender: TObject);
var
  p,i,l: integer;
begin
  l := Length(gLandmarks);
  i := AnatDrop.ItemIndex;
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

