unit design;

interface

uses
{$IFNDEF FPC}
//Utils,
{$ELSE}
LResources,
{$ENDIF}
//{$IFNDEF Unix} Windows,{$ENDIF}

  Buttons, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, Grids,nifti_hdr;

type
  String10= String[10];

  { TDesignForm }

  TDesignForm = class(TForm)
    OKBtn: TButton;
    AVal: TSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    ALevelNames: TStringGrid;
    LesionCovaryCheck: TCheckBox;
    AddMRIBtn: TButton;
    Label1: TLabel;
    TemplateBtn: TButton;
    TemplateLabel: TLabel;
    CritPctEdit: TSpinEdit;
    Label2: TLabel;
	//procedure LRsetup (var NumColumns,Vars,L1,L2,L3: integer; var OK: boolean);
    procedure AValChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ALevelNamesEnter(Sender: TObject);
    procedure ALevelNamesExit(Sender: TObject);
    procedure AddMRIBtnClick(Sender: TObject);
    procedure TemplateBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DesignForm: TDesignForm;

implementation

uses npmform,spread,hdr;
{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

const
  kMaxColumns = 16; {for ANOVA}
  //maxElements = kMaxColumns; {ANOVA}
  MaxLen = 12;
  //kCR = chr (13);
  //kTab = chr(9);
  kVALImgFilter = 'Image (*.hdr;*.nii;*.voi)|*.hdr;*.nii;*.nii.gz;*.voi';

procedure TDesignForm.AValChange(Sender: TObject);
{$IFDEF FPC}
var
   lOrig,lP: integer;
begin
  lOrig := ALevelNames.ColCount;
  DesignForm.Caption := inttostr(AVal.Value);
  ALevelNames.ColCount := AVal.Value;
  if AVal.value > lOrig then
     for lP := lOrig to (AVal.value-1) do
         AlevelNames.Cells[lP,0] := 'Pred'+inttostr(lP+1);
end;

{$ELSE}
begin
  ALevelNames.ColCount := AVal.Value;
end;
{$ENDIF}

procedure TDesignForm.FormCreate(Sender: TObject);
var lC: integer;
begin
     ALevelNames.ColCount := 16 ;
	 AlevelNames.Selection:=TGridRect(Rect(-1,-1,-1,-1));
   //AlevelNames.Cells[8,0] := 'Pred';
	 for lC := 0 to 15 do begin
		 AlevelNames.Cells[lC,0] := 'Pred'+inttostr(lC+1);
	 end;
	 SpreadForm.UpdateLabels;
  AValChange(nil); 
end;

procedure TDesignForm.ALevelNamesEnter(Sender: TObject);
begin
     AlevelNames.Selection:=TGridRect(Rect(0,0,0,0));
end;

procedure TDesignForm.ALevelNamesExit(Sender: TObject);
begin
     AlevelNames.Selection:=TGridRect(Rect(-1,-1,-1,-1));
end;

function LeadingZeroFilename (lInX: string): string;
var
   lIn: string;
   lC,lnPad,lPos,lnDec,lExtPos,lLen: integer;
begin
     {$IFDEF Unix}
     lIn := lInX;
     {$ELSE}
     lIn := Lowercase(lInX);
     {$ENDIF}
     lnPad := 8;
     lLen := length(lIn);
     result := lIn;
     if lLen < 1 then exit;
     lExtPos := 1;
     while (lExtPos <= lLen) and (lIn[lExtPos] <> '.') do
           inc(lExtPos);
     if lExtPos <= 1 then
        exit;
     lnDec := 0;
     lPos := lExtPos -1;
     while (lPos > 0) and ( lIn[lPos] in ['0'..'9']) do
           dec(lPos);
     lnDec := (lExtPos-lPos)-1;
     if (lnDec = 0) or (lnDec >= lnPad) then
        exit;
     result := '';
     if lPos > 0 then
        for lC := 1 to lPos do
            result := result + lIn[lC];
     for lC := 1 to (lnPad-lnDec) do
         result := result + '0';
     for lC := (lPos+1) to lLen do
         result := result+lIn[lC];
end;

procedure SortStrPadded (var lStr: TStringList);
{file1,file2...file10  not file1,file10..file2}
var counter, look:integer; temp:Tstrings;
begin
   if lStr.Count < 2 then exit;
   temp := TStringList.Create;
   for counter:=0 to lStr.Count-1 do
          temp.Append(LeadingZeroFilename{LowerCase}(lStr[counter]));
          for counter:=0 to temp.Count-1 do
               for look:=counter+1 to temp.Count-1 do
               if temp[look]<temp[counter] then begin
               lStr.Exchange(look, counter);
               temp.Exchange(look,counter);
     end;
     temp.Free;
end;

procedure TDesignForm.AddMRIBtnClick(Sender: TObject);
var
  lNumberofFiles,lC: integer;
  lFileStrs: TStringList;
begin
	if not MainForm.OpenDialogExecute('Select VOIs you wish to analyze',true,false,kVALImgFilter) then exit;
	lNumberofFiles:= MainForm.OpenHdrDlg.Files.Count;
        if  lNumberofFiles < 2 then begin
                lNumberofFiles := NIFTIhdr_HdrVolumes(MainForm.OpenHdrDlg.Filename);
                if lNumberofFiles < 2 then begin
		   Showmessage('Error: This function is designed to overlay MULTIPLE images. You selected less than two images.');
		   exit;
                end;
                lFileStrs := TStringList.Create;
	        for lC:= 1 to lNumberofFiles do
		    lFileStrs.Add(extractfilename(MainForm.OpenHdrDlg.Filename)+':'+inttostr(lC));
	end else begin
	    lFileStrs := TStringList.Create;
	    for lC:= 1 to lNumberofFiles do
		lFileStrs.Add(extractfilename(MainForm.OpenHdrDlg.Files[lC-1]));
            SortStrPadded (lFileStrs);
        end;
        SpreadForm.DataGrid.RowCount := lNumberofFiles+1+kMaxFactors;  //10/10/2006 -must resize BEFORE to populating cells
        for lC:= 1 to lNumberofFiles do
		SpreadForm.DataGrid.Cells[0,kMaxFactors+lC] := lFileStrs[lC-1];
        lFileStrs.free;

end;

procedure TDesignForm.TemplateBtnClick(Sender: TObject);
begin
	if not MainForm.OpenDialogExecute('Select Template image [determines bounding box and dimensions]',false,false,kVALImgFilter) then exit;
	TemplateLabel.Caption := (MainForm.OpenHdrDlg.Filename);
end;

  {$IFDEF FPC}
initialization
  {$I design.lrs}
{$ENDIF}

end.
