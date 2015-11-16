unit spread;
interface
{$H+}
uses
{$IFNDEF FPC}
//Utils,
Toolwin,shlobj,Spin,ShellApi,windows,messages,
{$ELSE}
LResources,
{$ENDIF}
   SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, Menus,  ComCtrls, Buttons,Clipbrd,design, StdCtrls,Registry,
  define_types,valformat;

type

  { TSpreadForm }

  TSpreadForm = class(TForm)
	DataGrid: TStringGrid;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    //DescriptiveMenu: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Design1:TMenuItem;
    Quit1: TMenuItem;
    ToolBar1: TToolBar;
    Help1: TMenuItem;
    Aboutthissoftware1: TMenuItem;
	StatusBar1: TStatusBar;
    Save1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
	Selectall1: TMenuItem;
    View: TMenuItem;
    Font1: TMenuItem;
    N81: TMenuItem;
    N101: TMenuItem;
    N121: TMenuItem;
    N141: TMenuItem;
    DesignBtn: TSpeedButton;
	Clearallcells1: TMenuItem;
    DescriptiveMenu: TMenuItem;
    procedure UpdateLabels;
    function GetVal (lC,lR: integer; var lVal: double): boolean;
    procedure Quit1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Aboutthissoftware1Click(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure DataGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure NewBtnClick(Sender: TObject);
    procedure Save1Click(var NoCancel: boolean);
    procedure FormCreate(Sender: TObject);
    procedure DataGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
	procedure OpenTextFile (var lFilename:string);
    function CheckSave2Close (lAllowCancel: boolean): boolean;
    procedure DataGridKeyPress(Sender: TObject; var Key: Char);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
	procedure SaveBtnClick(Sender: TObject);
    procedure ShowStatus;
    procedure ReadCells2Buffer;
    procedure Selectall1Click(Sender: TObject);
    procedure FontSizeChange(Sender: TObject);
    procedure DataGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DataGridDrawCell(Sender: TObject; Col, Row: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure Clearallcells1Click(Sender: TObject);
    procedure DesignBtnClick(Sender: TObject);
    procedure AddMRIScansClick(Sender: TObject);
    procedure DescriptiveClick(Sender: TObject);
    function SOpenDialogExecute (lCaption: string;lAllowMultiSelect,lForceMultiSelect: boolean; lFilter: string): boolean;
{$IFNDEF FPC}
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
{$ELSE}
    procedure FormClose(Sender: TObject);
    //procedure SpeedButton3Click(Sender: TObject);
{$ENDIF}

  private
    //procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    { Private declarations }
  public
	{ Public declarations }
  end;

var
  SpreadForm: TSpreadForm;
const
	kRegressSWName = 'VAL';
 kRegressSWVers = kRegressSWName+' v1.0';
	  kMaxFactors = 1;
	  gVALChanges: boolean = false;
	  gDesignUnspecified : boolean = true;
	  gEnterCell : boolean= false;
	  gVALFontSize: integer = 8;
   kMagicDouble : double = -111666222;
   //kVALNativeSignature = 'abba';
	  //kTxtExt = '.txt';
	 //kVALNativeExt = '.val';
implementation
uses statcr,hdr;

{$IFNDEF FPC}

{$R *.DFM}
{$ENDIF}
function TSpreadForm.SOpenDialogExecute (lCaption: string;lAllowMultiSelect,lForceMultiSelect: boolean; lFilter: string): boolean;//; lAllowMultiSelect: boolean): boolean;
var
   lNumberofFiles: integer;
begin
	OpenDialog1.Filter := lFilter;//kAnaHdrFilter;//lFilter;
	OpenDialog1.FilterIndex := 1;
	OpenDialog1.Title := lCaption;
	if lAllowMultiSelect then
		OpenDialog1.Options := [ofAllowMultiSelect,ofFileMustExist]
	else
		OpenDialog1.Options := [ofFileMustExist];
	result := OpenDialog1.Execute;
	if not result then exit;
	if lForceMultiSelect then begin
		lNumberofFiles:= OpenDialog1.Files.Count;
		if  lNumberofFiles < 2 then begin
			Showmessage('Error: This function is designed to overlay MULTIPLE images. You selected less than two images.');
			result := false;
		end;
	end;
end;
(*procedure TSpreadForm.WMDropFiles(var Msg: TWMDropFiles);
var
  lStr: string;
  CFileName: array[0..MAX_PATH] of Char;
begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then //requires ShellAPI in 'uses' clause
	begin
         if gChanges then begin
            if not CheckSave2Close(true) then exit;
         end;
         lStr := CFilename;
			OpenTextFile(lStr);
            OpenDialog1.FileName := lStr;
      Msg.Result := 0;
	end;
  finally
    DragFinish(Msg.Drop);
  end;
end;   *)

procedure TSpreadForm.Quit1Click(Sender: TObject);
begin
     if not CheckSave2Close(true) then exit;
	 gVALChanges := false;
	 SpreadForm.Close;
end;



procedure TSpreadForm.FormResize(Sender: TObject);
var lClient,lWid,lCount: integer;
begin
  lCount := DataGrid.ColCount;
  lClient := DataGrid.ClientWidth;
  if lCount < 1 then begin
	DataGrid.ColWidths[0] := lClient;
	  exit;
  end;
  lWid := ((lClient) div lCount);
  DataGrid.DefaultColWidth := lWid-1;
  (*if lWid <> lCol1Wid then begin
	 lCol1Wid := (lClient-((lCount) * lWid))-lCount{-14};
	 lGrid.ColWidths[0] := lCol1Wid;
  end;*)
end;

function ColLabel (lCol: integer): string; //first column= A, 26th=Z,27th AA, etc...
var lColDiv,lColMod: integer;
begin
    result := '';
	lColDiv := lCol;
	repeat
		  lColMod := lColDiv mod 26;
		  if lColMod = 0 then lColMod := 26;
          result := chr(ord('A')+lColMod-1)+result;
          {if lColDiv = 26 then
             lColDiv := 0
          else}
              lColDiv := (lColDiv-1) div 26;
    until lColDiv <= 0;
end;

procedure UpdateGridLabels(lGrid: TStringGrid);
var
   lA,lInc,lInc2: integer;
begin
     if lGrid.RowCount < 2 then exit;
	 //for lInc := (lGrid.RowCount -1) downto 1 do
	 //    lGrid.Cells[0,kMaxFactors+lInc] := inttostr(lInc);
	 if (lGrid.ColCount) < 1 then exit;

     //Next enter ANOVA labels for each row
	 for lInc := (lGrid.ColCount-1 {-1 for Lazarus 999}) downto 0 do
		 for lInc2 := 0 to kMaxFactors do
             lGrid.Cells[lInc,lInc2] := '';
	 lA := DesignForm.AVal.value;
//999 showmessage(inttostr(lGrid.RowCount)+'x'+inttostr(lGrid.ColCount)+'alpha'+inttostr(lA));
	 //lGrid.Cells[0,0] := '';
	 for lInc := 1 to lA do
		  lGrid.Cells[lInc,0] := DesignForm.ALevelNames.Cells[lInc-1,0];
{$IFDEF FPC}
	 for lInc := (lGrid.ColCount -2) downto 0 do
		 lGrid.Cells[lInc +1 ,kMaxFactors] := ColLabel(lInc+1);
{$ELSE}
	 for lInc := (lGrid.ColCount -1) downto 0 do
		 lGrid.Cells[lInc+1,kMaxFactors] := ColLabel(lInc+1);//chr(ord('A')+lInc);
{$ENDIF}

end;

procedure TSpreadForm.UpdateLabels;
begin
 DataGrid.ColCount := DesignForm.AVal.value+1; //2007 For FPC
 UpdateGridLabels(DataGrid);
 DataGrid.ColCount := DesignForm.AVal.value+1;
end;

procedure TSpreadForm.Aboutthissoftware1Click(Sender: TObject);
begin
	Showmessage(kRegressSWVers); // AboutForm.Showmodal;
end;


procedure ClearDesignMatrix;
begin

	gDesignUnspecified := true;
	SpreadForm.DesignBtn.Caption := 'Design: not specified';
end;

procedure DesignBtnLabelUpdate;
begin
	 SpreadForm.DesignBtn.Caption := 'Design IVs: '+inttostr(DesignForm.AVal.Value)  ;
	 SpreadForm.UpdateLabels;
	 SpreadForm.FormResize(nil);
end;

{$ifdef fpc}
function alignx(addr : Pointer;alignment : PtrUInt) : Pointer;
begin

     result:=align(addr,alignment);
end;
{$endif}

procedure TSpreadForm.OpenTextFile (var lFilename:string);
var lTemplateName:string;
	lnCritPct,lnRow,lnCol,lnColWObs,lCol,lRow: integer;
	//lLesionCovary : boolean;
	lPredictorList,lFileList:TStringList;
	lDoublePtr: Pointer;
	lDoubleBuf : DoubleP;
begin
   Self.Caption := kRegressSWVers+': '+extractfilename(lFilename);
	ClearDesignMatrix;
	lPredictorList := TStringList.Create;
	lFileList := TStringList.Create;
	gVALChanges := false;
	OpenValFile (lFilename,lTemplateName, lnRow,lnCol,lnColWObs,lnCritPct,gDesignUnspecified,lPredictorList,lFileList,lDoublePtr);
 {$IFDEF FPC}
 DataGrid.RowCount := kMaxFactors+lnRow{-1};
  {$ELSE}
 DataGrid.RowCount := kMaxFactors+lnRow;
  {$ENDIF}
	DataGrid.ColCount := lnCol+1;
        DataGrid.refresh;
        {$IFDEF FPC}
        lDoubleBuf := alignx(lDoublePtr, 16); // note: lDoubleBuf > lDoublePtr always (VSDS);
        {$ELSE}
        lDoubleBuf := DoubleP($fffffff0 and (integer(lDoublePtr)+15));
        {$ENDIF}
	if lFileList.Count < lnRow then
		lnRow := lFileList.Count;
	for lRow := 1 to lnRow do begin

                 DataGrid.Cells[ 0, kMaxFactors+lRow ] := lFileList.Strings[lRow-1];
		for lCol := 1 to lnCol do begin
                    if lDoubleBuf^[RowColPos (lRow,lCol,lnColWObs)] = kMagicDouble then
                      DataGrid.Cells[ lCol, kMaxFactors+lRow ] := ''
                    else
   	            DataGrid.Cells[ lCol, kMaxFactors+lRow ] := floattostr((lDoubleBuf^[RowColPos (lRow,lCol,lnColWObs)]));
                end;

	end;

if lPredictorList.Count < lnRow then
		for lCol := (lPredictorList.Count+1) to lnRow do
			lPredictorList.Add( 'Pred'+inttostr(lCol) );
   DesignForm.ALevelNames.ColCount := lnCol;
	for lCol := 1 to lnCol do
	    DesignForm.ALevelNames.Cells[lCol-1,0] := lPredictorList.Strings[lCol-1];
	Freemem(lDoublePtr);
	lPredictorList.Free;
	lFileList.free;
	//DesignForm.LesionCovaryCheck.Checked := lLesionCovary;
    DesignForm.CritPctEdit.value := lnCritPct;
	DesignForm.TemplateLabel.Caption := lTemplateName;
	//Tidy Up...
	DesignForm.AVal.Value := lnCol;
	UpdateLabels;

 DesignBtnLabelUpdate;

	FormResize(nil);
	if gDesignUnspecified then
		Showmessage('You need to define the experiment design [press the ''Design'' button]');

end;

procedure TSpreadForm.OpenBtnClick(Sender: TObject);
var lFileName: string;
begin
     if gVALChanges then begin
		if not CheckSave2Close(true) then exit;
     end;
     if not SOpenDialogExecute('Select VAL design file',false,false, kValFilter) then exit;
     lFilename := OpenDialog1.filename;
     if not fileexists(lFilename) then exit;
     OpenTextFile(lFilename);
end;

procedure GridToStatusBar(lGrid: TStringGrid; lStatus: TStatusBar);
begin
{$IFDEF FPC}
          //SpreadForm.StatusBar1.Panels[1].Text := inttostr(random(888));
    if (lGrid.Selection.Top <= kMaxFactors) or (lGrid.Selection.Left <= 0) then begin
       lGrid.Selection:=TGridRect(Rect(-1,-1,-1,-1));
       SpreadForm.Caption := '';
       exit;
    end;
    if lGrid.Selection.Top < 0 then exit;
    if((lGrid.Selection.Top = lGrid.Selection.Bottom ) and ( lGrid.Selection.Left = lGrid.Selection.Right )) then begin
	  SpreadForm.Caption :=
    lGrid.Cells[0,lGrid.Selection.Top]+' = '+lGrid.Cells[lGrid.Selection.Left,lGrid.Selection.Top]+'   '+
	   lGrid.Cells[lGrid.Selection.Left,0]+' '+ lGrid.Cells[lGrid.Selection.Left,1]+inttostr(lGrid.Selection.Top-kMaxFactors);
    end else begin
        SpreadForm.Caption :=  inttostr(lGrid.Selection.Bottom-lGrid.Selection.Top + 1)+'R x '+ inttostr(lGrid.Selection.Right-lGrid.Selection.Left + 1)+'C';
    end;
    
    (*if((lGrid.Selection.Top <> lGrid.Selection.Bottom ) or ( lGrid.Selection.Left <> lGrid.Selection.Right )) then exit;

    if (lGrid.Selection.Top <= kMaxFactors) or (lGrid.Selection.Left <= 0) then begin
       lGrid.Selection:=TGridRect(Rect(-1,-1,-1,-1));
       lStatus.Panels[0].Text := '';
       exit;
    end;
    if (lGrid.Selection.Top < 0) then exit;

    //lStatus.Panels[1].Text := inttostr(lGrid.Selection.Bottom-lGrid.Selection.Top + 1)+'R x '+ inttostr(lGrid.Selection.Right-lGrid.Selection.Left + 1)+'C';
    //lStatus.Panels[1].Text := inttostr(lGrid.Selection.Top)+'R x '+ inttostr(lGrid.Selection.Bottom)+'C';
    SpreadForm.Caption := inttostr(lGrid.Selection.Top)+'R x '+ inttostr(lGrid.Selection.Left)+'C';
    exit;
    if((lGrid.Selection.Top = lGrid.Selection.Bottom ) and ( lGrid.Selection.Left = lGrid.Selection.Right )) then begin
	  lStatus.Panels[1].Text := {ColLabel(lGrid.Selection.Left)+}lGrid.Cells[0,lGrid.Selection.Top]{inttostr(lGrid.Selection.Top-kMaxFactors)}+' = '+lGrid.Cells[lGrid.Selection.Left,lGrid.Selection.Top];
//	  lStatus.Panels[0].Text := lGrid.Cells[lGrid.Selection.Left,0]+' '+ lGrid.Cells[lGrid.Selection.Left,1]+' '+lGrid.Cells[lGrid.Selection.Left,2];
//	  lStatus.Panels[0].Text :=  lGrid.Cells[lGrid.Selection.Left,0]+' '+ lGrid.Cells[lGrid.Selection.Left,1]+inttostr(lGrid.Selection.Top-kMaxFactors);
	  //lStatus.Panels[0].Text :=  lGrid.Cells[lGrid.Selection.Left,0]+' '+ lGrid.Cells[lGrid.Selection.Left,1]+inttostr(lGrid.Selection.Top-kMaxFactors);

    end else begin
        lStatus.Panels[0].Text := inttostr(lGrid.Selection.Bottom-lGrid.Selection.Top + 1)+'R x '+ inttostr(lGrid.Selection.Right-lGrid.Selection.Left + 1)+'C';
        lStatus.Panels[1].Text := '';
    end; *)
{$ELSE} //Delphi
    if (lGrid.Selection.Top <= kMaxFactors) or (lGrid.Selection.Left <= 0) then begin
       lGrid.Selection:=TGridRect(Rect(-1,-1,-1,-1));
       lStatus.Panels[0].Text := '';
       exit;
    end;
    if lGrid.Selection.Top < 0 then exit;
    if((lGrid.Selection.Top = lGrid.Selection.Bottom ) and ( lGrid.Selection.Left = lGrid.Selection.Right )) then begin
	  lStatus.Panels[1].Text := lGrid.Cells[0,lGrid.Selection.Top]+' = '+lGrid.Cells[lGrid.Selection.Left,lGrid.Selection.Top];
	  lStatus.Panels[0].Text :=  lGrid.Cells[lGrid.Selection.Left,0]+' '+ lGrid.Cells[lGrid.Selection.Left,1]+inttostr(lGrid.Selection.Top-kMaxFactors);
    end else begin
        lStatus.Panels[0].Text := inttostr(lGrid.Selection.Bottom-lGrid.Selection.Top + 1)+'R x '+ inttostr(lGrid.Selection.Right-lGrid.Selection.Left + 1)+'C';
        lStatus.Panels[1].Text := '';
    end;
{$ENDIF}
    
end;

procedure TSpreadForm.ShowStatus;
begin
//SpreadForm.Caption := inttostr(random(888));
    GridToStatusBar(DataGrid,StatusBar1);
end;

procedure TSpreadForm.DataGridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  //ShowStatus; //bxxx
    gEnterCell := true;
end;

procedure TSpreadForm.NewBtnClick(Sender: TObject);
begin
	 DesignForm.Showmodal;
	 gDesignUnspecified := false;
	 DesignBtnLabelUpdate;
end;

function RemoveColons( lStr: string): string;
var lLen,lPos: integer;
begin
	 result := lStr;
	 lLen := length(lStr);
	 if lLen < 1 then exit;
	 for lPos := 1 to lLen do
		 if result[lPos] = ':' then
			result[lPos] := ';';
end;

function Str2Float (var lStr: string; var lError: boolean): single;
begin
     lError := false;
     try
        result :=  Strtofloat(lStr);
     except
           on EConvertError do
              lError := true;
     end; //except
end;

procedure TSpreadForm.Save1Click(var NoCancel: boolean);
const
	 kNative = 1;
	 kTxt = 2;
var
  f: TextFile;
  lFormat,C, R,lLen,lPos,ColStart,ColEnd,RowStart,RowEnd : integer ;
  lLevelStr,lFilename,S,lCell,lExt : string ;
  kSpacer,lDecimalSep : char;
  lError: boolean;
begin
	NoCancel := false;
   if not SaveDialog1.Execute then exit;
   lFormat := SaveDialog1.FilterIndex;
   if (lFormat < kNative) or (lFormat > kTxt) then
	  lFormat := kNative;
   case lFormat of
		kTxt: lExt := kTXText;
		else lExt := kValNativeExt;
   end;
   if lFormat <> kNative then begin
	  case MessageDlg( 'Export file as a text format? Note you will lose information about the experiment design [save to Native format to preserve condition information]', mtWarning, [mbYes, mbCancel], 0 ) of
			mrCancel : exit ;
		end ;
   end; //not native
   if (lFormat = kNative) and (gDesignUnspecified) then begin
	   showmessage('Unable to save this data as '+kRegressSWVers+' format file until you have specified the conditions [press the ''Design'' button]');
	   exit;
   end;
   //lExt := StrUpper(PChar(extractfileext(SaveDialog1.Filename)));
   lFilename := SaveDialog1.Filename;
   lDecimalSep := DecimalSeparator;
         DecimalSeparator := '.';
   ChangeFileExt(lFilename,lExt);
	// Setup...
	kSpacer := #9; //tab
	S := '' ;
	RowStart := kMaxFactors+1 ;
	RowEnd := DataGrid.RowCount - 1;
	ColStart := 0 ;
	ColEnd := DataGrid.ColCount - 1;
	if (ColEnd < ColStart) or (RowEnd < RowStart) then exit;
	// Copy to string
	for R := RowStart to RowEnd do
	begin
		for C := ColStart to ColEnd do begin

                        lCell := DataGrid.Cells[ C, R ];
                        if C <> ColStart then begin
                           if lCell = '' then  //this simply prevents error reports when run from debugger
                              lError := true
                           else
                               Str2Float (lCell, lError);
                           if (lError) then
                              lCell := '-';
                        end;
			S := S +  lCell;
			if( C < DataGrid.ColCount - 1 ) then
				S := S + kSpacer{#9} ; // Tab
		end ;
		if R <> (DataGrid.RowCount - 1) then //all except last line
		   S := S + #13#10 ; // End line
	end ;
	AssignFile(f, lFileName);
	rewrite(f);
	if lFormat = kNative then begin
	   Self.Caption := kRegressSWVers+': '+extractfilename(SaveDialog1.Filename);//remove any previous filename
           if Files4D(DataGrid.Cells[ ColStart, RowStart ]) then
	      writeln(f,kVALNativeSignatureBase + '1')//version 1 supports 4D images
           else
	       writeln(f,kVALNativeSignatureBase + '0');//version 0 supports 3D images only

	   //Details for 1st factor
	   //writeln(f,'#Predictors:'+inttostr(lLen)+lLevelStr+lWithinSubjStr);
	   writeln(f,'#Covary Volume'+kSpacer+bool2char(DesignForm.LesionCovaryCheck.Checked));
	   writeln(f,'#Template'+kSpacer+DesignForm.TemplateLabel.Caption);
	   writeln(f,'#CritPct'+kSpacer+inttostr(DesignForm.CritPctEdit.value));
	   lLevelStr := 'ImageName';
	   lLen := DesignForm.AVal.value;
	   if lLen >= 1 then
	   	  for lPos := 1 to lLen do
	   		  lLevelStr := lLevelStr+kTab+(DesignForm.ALevelNames.Cells[lPos-1,0]);
	   writeln(f,lLevelStr);
	   gVALChanges := false;
	end;
	Writeln(f, S);
	Flush(f);  { ensures that the text was actually written to file }
	CloseFile(f);
    NoCancel := true;
    DecimalSeparator :=lDecimalSep;
end;


procedure registerfiletype(inft,inkey,desc,icon:string);
var myreg : treginifile;
    ct : integer;
    ft,key: string;
begin
     ft := inft;
     key := inkey;
     ct := pos('.',ft);
     while ct > 0 do begin
           delete(ft,ct,1);
           ct := pos('.',ft);
     end;
     if (ft = '') or (Application.ExeName = '') then exit; //not a valid file-ext or ass. app
     ft := '.'+ft;
	 myreg := treginifile.create('');
     try
        myreg.rootkey := hkey_classes_root; // where all file-types are described
        if key = '' then key := copy(ft,2,maxint)+'_auto_file'; // if no key-name is given, create one
        myreg.writestring(ft,'',key); // set a pointer to the description-key
        myreg.writestring(key,'',desc); // write the description
        myreg.writestring(key+'\DefaultIcon','',icon); // write the def-icon if given
        myreg.writestring(key+'\shell\open\command','',Application.ExeName+' %1'); //association
     finally
            myreg.free;
     end;
end;

procedure TSpreadForm.FormCreate(Sender: TObject);
begin
	SpreadForm.Caption := kRegressSWName;
(*	 registerfiletype(kNativeExt,kRegressSWName{key},kRegressSWName,Application.ExeName+',1');
	 DragAcceptFiles(Handle, True); *)
	 DataGrid.Selection:=TGridRect(Rect(-1,-1,-1,-1));
	 gVALFontSize := 8;
	 //DecSeparator := DecimalSeparator;
	 //l64rBufP := nil;
	 gEnterCell := false;
	 gVALChanges := false;
    DataGrid.ColCount := 9;
	DataGrid.RowCount := 15;
	FormResize(nil);
          {$IFDEF Darwin}
    {$IFNDEF LCLgtk} //only for Carbon compile

New1.ShortCut := ShortCut(Word('N'), [ssMeta]);
            Open1.ShortCut := ShortCut(Word('O'), [ssMeta]);
            Save1.ShortCut := ShortCut(Word('S'), [ssMeta]);
            Quit1.ShortCut := ShortCut(Word('W'), [ssMeta]);
            Copy1.ShortCut := ShortCut(Word('C'), [ssMeta]);
            Paste1.ShortCut := ShortCut(Word('V'), [ssMeta]);
            Selectall1.ShortCut := ShortCut(Word('A'), [ssMeta]);
            DescriptiveMenu.ShortCut := ShortCut(Word('L'), [ssMeta]);

    {$ENDIF}//Carbon
    {$ENDIF}//Darwin
end;

procedure TSpreadForm.DataGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var C, R : integer ;
    Rect : TGridRect ;

begin

	DataGrid.MouseToCell( X, Y, C, R ) ;
	Rect.Left := C ;
	Rect.Right := C ;
	Rect.Top := R ;
	Rect.Bottom := R ;
	DataGrid.Selection := Rect ;
end;

procedure TSpreadForm.DataGridKeyPress(Sender: TObject; var Key: Char);
var S : string ;

begin
      if (Key in ['0'..'9','.',kBS,kDel,kCR]) or ((Key='-') and (gEnterCell)) then
    else 
		exit;
	if(( DataGrid.Selection.Top = DataGrid.Selection.Bottom ) and
		( DataGrid.Selection.Left = DataGrid.Selection.Right )) then begin
        gVALChanges := true;
        if gEnterCell then begin
           S := ''
        end else
			S := DataGrid.Cells[ DataGrid.Selection.Left,DataGrid.Selection.Top ] ;
        gEnterCell := false;
        if ( ( Key = kDEL ) or ( Key = kBS ) )then
		begin
            if( length( S ) > 0 ) then
            begin
                setlength( S, length( S ) - 1 ) ;
            end ;
        end else
        if ( Key = kCR ) then
        begin
            //Edit_Box.Text := S ;
            exit ;
		end else
		begin
			S := S + Key ;
		end ;
		DataGrid.Cells[ DataGrid.Selection.Left, DataGrid.Selection.Top ] := S ;
		//Format_Grid.Cells[ DataGrid.Selection.Left, DataGrid.Selection.Top ] := '' ;
	end ;
end;

procedure TSpreadForm.Copy1Click(Sender: TObject);
var C, R : integer ;
	P: PChar;
 S : string ;
	RStart,CStart,REnd,CEnd : integer ;
begin
	// Setup...
	S := '' ;
	if (DataGrid.Selection.Left < 0) or (DataGrid.Selection.Top < 0) then begin
		DataGrid.Selection:= TGridRect(Rect(1,1+kMaxFactors,DataGrid.ColCount-1,DataGrid.RowCount-1));
	end;
	CStart := DataGrid.Selection.Left;
	CEnd := DataGrid.Selection.Right;
	RStart := DataGrid.Selection.Top;
	REnd := DataGrid.Selection.Bottom;
	// Copy to string
    for R := RStart to REnd do
    begin
        for C := CStart to CEnd do
		begin
			S := S + DataGrid.Cells[ C, R ] ;
			if( C < CEnd ) then begin
				S := S + #9 ; // Tab
			end ;
		end ;
		S := S + #13#10 ; // End line
	end ;
	// Set clipboard
 {$IFNDEF FPC}
	Clipboard.SetTextBuf( PChar( S ) ) ;
{$ELSE}
  p:=StrAlloc (length(S)+1);
  if StrPCopy (P,S)=P then
     Clipboard.SetTextBuf(P);

{$ENDIF}
end;

procedure TSpreadForm.Paste1Click(Sender: TObject);
const
      BS = #8 ; { Backspace }
      CR = #13 ; { Carriage return }
      DEL = #127 ; { Delete }
      //HT = #9 ; { Horizontal Tab }
      //LF = #10 ; { Line Feed }
      //VT = #11 ; { Vertical Tab }
var StartC,C, R,I : integer ;
    Dummy : integer ;
    lSciNotation,EOF: boolean;
    lValue: double;
    DecSeparator : char;
    Line, S, Work,WorkFilter : string ;
begin
    // Setup...
    DecSeparator := DecimalSeparator;
    S := Clipboard.AsText ;
    EOF:= false;
    if (DataGrid.Selection.Left < 0) or (DataGrid.Selection.Top < 0) then begin
        Selectall1Click(nil);
    end;
    //gChanges := true;
    StartC := DataGrid.Selection.Left;
    R := DataGrid.Selection.Top;
    C := StartC;
    while( length( S ) > 0 ) do begin
        // Extract next line...
        {$IFDEF UNIX}
        Dummy := pos( #13, S + #13 ) ;
        {$ELSE}
        Dummy := pos( #13#10, S + #13#10 ) ;
        {$ENDIF}
        Line := copy( S, 1, Dummy - 1 ) ;
        if (Dummy+1) < length(S) then //last line may not have eol
           S := copy( S, Dummy + 1, length( S ) )
        else
            EOF := true;
        while( length( Line ) > 0 ) do begin
            // Extract next cell...
            lSciNotation := false;
            Dummy := pos( #9, Line + #9 ) ;
            Work := copy( Line, 1, Dummy - 1 ) ;
            Line := copy( Line, Dummy + 1, length( S ) ) ;
            WorkFilter :=  '';
            if length(Work) > 0 then begin
                for I := length(Work) downto 1 do begin
                    if (Work[i] in ['-','0'..'9','E','e',DecSeparator,BS,DEL,CR]) then
                       WorkFilter := Work[i]+WorkFilter;
                    if (Work[i] in ['E','e']) then
                       lSciNotation := true;
                end;
            end;
            if lSciNotation then begin
               try
                  lValue := strtofloat(Workfilter);
               except
                     on EConvertError do
                        lValue := NaN
                     else
                         lValue := NaN;
               end; //try..except
               if lValue <> NaN then
                  DataGrid.Cells[ C, R ] :=(floattostr(lValue));
            end else if(length(WorkFilter) > 0) and ( C < DataGrid.ColCount ) then begin
                DataGrid.Cells[ C, R ] := WorkFilter ;
                //Format_Grid.Cells[ C, R ] := '' ;
            end ;
            inc( C ) ;
        end ;
        inc( R ) ; // Move to next row
        if( R >= DataGrid.RowCount ) or (EOF) then begin
            break ; // All done with paste
        end ;
        C := StartC;
    end ; // While length(S) > 0
end; //proc Paste1Click
(*var StartC,C, R,I : integer ;
	Dummy : integer ;
	lSciNotation,EOF,lData: boolean;
	lValue: double;
	Line, S, Work,WorkFilter : string ;
begin
	// Setup...
	lValue := 0; //only to prevent compiler warning...
	S := Clipboard.AsText ;
	EOF:= false;
	if (DataGrid.Selection.Left < 0) or (DataGrid.Selection.Top < 0) then begin
		Selectall1Click(nil);
	end;
    //R := 1 ;
    //StartC := 1 ;
    StartC := DataGrid.Selection.Left;
    //CEnd := DataGrid.Selection.Right;
    R := DataGrid.Selection.Top;
    //REnd := DataGrid.Selection.Bottom;
    // Do the paste
    C := StartC;
    while( length( S ) > 0 ) do begin
        // Extract next line...
        Dummy := pos( #13#10, S + #13#10 ) ;
        Line := copy( S, 1, Dummy - 1 ) ;
        if (Dummy+1) < length(S) then //last line may not have eol
           S := copy( S, Dummy + 1, length( S ) )
        else
            EOF := true;
		//showmessage(inttostr(C)+'x'+Line);
        while( length( Line ) > 0 ) do begin
            // Extract next cell...
            lSciNotation := false;
			//old
			//Dummy := pos( #9, Line + #9 ) ;
			//new - comma separated, etc
			lData := false;
			Dummy := length(line)+1;
			I := 1;
			repeat
				if (Line[i] in ['-','0'..'9','E','e']) then
					lData := true
				else begin
					if lData then Dummy := I;
				end;
				inc(I);
			until (I > length(Line)) or (Dummy = (I-1));
			//end new

			Work := copy( Line, 1, Dummy - 1 );
			//showmessage(inttostr(Dummy)+'x'+Work);

			Line := copy( Line, Dummy + 1, length( S ) ) ;
			//showmessage(Line);
			WorkFilter :=  '';
			if length(Work) > 0 then begin
				for I := length(Work) downto 1 do begin
					if (Work[i] in ['-','0'..'9','E','e','.',kBS,kDEL,kCR]) then
					   WorkFilter := Work[i]+WorkFilter;
                    if (Work[i] in ['E','e']) then
                       lSciNotation := true;
                end;
            end;
            if lSciNotation then begin
               try
				  lValue := strtofloat(Workfilter);
               except
					 on EConvertError do
						lValue := NaN;
               end; //try..except
			   if lValue <> NaN then
				  DataGrid.Cells[ C, R ] :=(floattostr(lValue));
            end else if(length(WorkFilter) > 0) and ( C < DataGrid.ColCount ) then begin
                DataGrid.Cells[ C, R ] := WorkFilter ;
                //Format_Grid.Cells[ C, R ] := '' ;
            end ;
            inc( C ) ;
        end ;
        inc( R ) ; // Move to next row
        if( R >= DataGrid.RowCount ) or (EOF) then begin
            break ; // All done with paste
		end ;
		//Showmessage(inttostr(StartC));
		C := StartC;
    end ; // TMainForm.Paste1Click
end;   *)


procedure TSpreadForm.SaveBtnClick(Sender: TObject);
var
   b: boolean;
begin
	 Save1Click(b);
end;

function TSpreadForm.CheckSave2Close (lAllowCancel: boolean): boolean;
begin
	 result := true;
	 if not gVALChanges then exit;
	 result := false;
	 if lAllowCancel then begin
	   case MessageDlg( 'Save changes?', mtWarning, [mbYes, mbNo, mbCancel], 0 ) of
			mrYes : begin
					Save1Click( result ) ;
				end ;
			mrCancel : exit ;
		end ;
	 end else
	   case MessageDlg( 'Save changes?', mtWarning, [mbYes, mbNo], 0 ) of
			mrYes : begin
					Save1Click( result ) ;
				end ;
		end;
	 result := true;
end;

procedure TSpreadForm.ReadCells2Buffer;
var
   lDbl: double;
   lRend,lRStart,lCStart,lCEnd,lC,lR,lPos: integer;
   lStr: string;
   l64rBufP: pointer;
   l64rBuf: DoubleP;
begin
	//if l64rBufP <> nil then
	//    freemem(l64rBufP);
	GetMem(l64rBufP,(DataGrid.ColCount*DataGrid.RowCount*sizeof(double))+16);
        {$IFDEF FPC}
	l64rBuf := alignx(l64rBufP, 16);
        {$ELSE}
	l64rBuf := DoubleP((integer(l64rBufP) and $FFFFFFF0)+16);
        {$ENDIF}
	lRStart := {1}kMaxFactors+1;
    lREnd := DataGrid.RowCount - 1;
    lCstart := 1;
    lCend := DataGrid.ColCount - 1;
	//gnCol := lCEnd;
    //gnRow := lREnd-lRStart+1;
    // Copy to string
	lPos := 0;
    for lR := lRStart to lREnd do begin
        for lC := lCStart to lCEnd do begin
            inc(lPos);
            lStr := (DataGrid.Cells[ lC, lR ]);
            lDbl := NaN;
            if length(lStr) > 0 then begin
				try
				   lDbl :=  Strtofloat(lStr);
                except
                      on EConvertError do begin
                         showmessage('Cell '+ColLabel(lC)+inttostr(lR-kMaxFactors)+ ': Unable to convert the string '+lStr+' to a number');
                         DataGrid.Cells[ lC, lR ] := '';
                         lDbl := NaN; //NAN? Not-A-Number
                      end; //Error
                end; //except
            end; //length > 0
            l64rBuf^[lPos] :=lDbl;
		end ; //for each col
	end ; //for each row
	freemem(l64rBufP);
end;

procedure TSpreadForm.Selectall1Click(Sender: TObject);
begin
		DataGrid.Selection:= TGridRect(Rect(1,1+kMaxFactors,DataGrid.ColCount-1,DataGrid.RowCount-1));
end;

procedure TSpreadForm.FontSizeChange(Sender: TObject);
begin
   (sender as TMenuItem).Checked := true;
   gVALFontSize := (sender as TMenuItem).tag;
   DataGrid.Font.Size := (sender as TMenuItem).tag;
   DataGrid.DefaultRowHeight := (sender as TMenuItem).tag+12;
   FormResize(nil);
end;

procedure TSpreadForm.DataGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
     ShowStatus;
end;

procedure TSpreadForm.DataGridDrawCell(Sender: TObject; Col, Row: Integer;
  Rect: TRect; State: TGridDrawState);
begin
         ShowStatus;
end;

procedure TSpreadForm.Clearallcells1Click(Sender: TObject);
var
 lR,lC,lRi,lCi: integer;
begin
	lR := DataGrid.RowCount-1;
	lC := DataGrid.ColCount-1;
	for lRi := 1 to lR do begin
        for lCi := 1 to lC do begin
           DataGrid.Cells[lCi,kMaxFactors+lRi] := '';
        end;//for cols
    end;//for rows
end;

procedure TSpreadForm.DesignBtnClick(Sender: TObject);
begin
	 DesignForm.Showmodal;
	 gDesignUnspecified := false;
	 DesignBtnLabelUpdate;
end;

procedure TSpreadForm.AddMRIScansClick(Sender: TObject);
begin
DesignForm.AddMRIBtnClick(nil);
end;

function TSpreadForm.GetVal (lC,lR: integer; var lVal: double): boolean;
var
   lStr: string;
   lDbl: double;
begin
     result := false;
     lVal := 0;
     lStr := (DataGrid.Cells[ lC, lR ]);
     if lStr = '' then
        exit;
     try
        lDbl :=  Strtofloat(lStr);
     except
       on EConvertError do begin
           showmessage('Cell '+ColLabel(lC)+inttostr(lR-kMaxFactors)+ ': Unable to convert the string '+lStr+' to a number');
           exit;
       end;
     end; //try..except
     lVal := lDbl;
     result := true;
end;//GetVal

procedure TSpreadForm.DescriptiveClick(Sender: TObject);
var
   lMn,lSD,lSE,lSkew,lZSkew: double;
 n,lR,lC,lRi,lCi: integer;
 lVal: double;
 RA: SingleP;
begin
     lR := DataGrid.RowCount-1;
     if (lR <= kMaxFactors+1)   then
        exit;
     lC := DataGrid.ColCount-1;
     Getmem(RA,lR * sizeof(single));
     for lCi := 1 to lC do begin
         n := 0;
         for lRi := (kMaxFactors+1) to lR do begin
             if GetVal (lCi,lRi,lVal) then begin
                 inc(n);
                 RA^[n] := lVal;
             end;

         end;//for rows
         if n > 0 then begin
            SuperDescriptive (RA, n, lMn,lSD,lSE,lSkew,lZSkew);
            Showmessage('"'+DataGrid.Cells[ lC, 0]+'" mean='+floattostr(lMn)+',StDev='+floattostr(lSD)+',StEr='+floattostr(lSE)+',Skew='+floattostr(lSkew)+',ZSkew='+floattostr(lZSkew));

         end; //n > 0
    end;//for cols
   Freemem(RA);
end;


{$IFNDEF FPC}
    procedure TSpreadForm.FormClose(Sender: TObject; var Action: TCloseAction);
{$ELSE}
    procedure TSpreadForm.FormClose(Sender: TObject);
{$ENDIF}
begin
     CheckSave2Close(false);
end;



{$IFDEF FPC}
initialization
  {$I spread.lrs}
{$ENDIF}

end.