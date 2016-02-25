unit nifti_hdr_view;
interface
{$H+}
{$MODE DELPHI}
uses
{$IFNDEF FPC}
  RXSpin,capmenu,
{$ELSE}
LResources, Spin,

{$ENDIF}
{$IFNDEF Unix} ShellAPI, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, nifti_hdr, Menus, ComCtrls, Buttons, define_types, nifti_types;
type
  { THdrForm }
  THdrForm = class(TForm)
    EcodeLabel: TLabel;
    EcodeMemo: TMemo;
    NoECodeLabel: TLabel;
    Ymm: TFloatSpinEdit;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    Save1: TMenuItem;
    OpenHdrDlg: TOpenDialog;
    SaveHdrDlg: TSaveDialog;
    PageControl1: TPageControl;
    TabRequired: TTabSheet;
    TabUnused: TTabSheet;
    intent_nameEdit: TEdit;
    data_typeEdit: TEdit;
    CommentEdit: TEdit;
    db_: TEdit;
    aux: TEdit;
    gmax: TSpinEdit;
    gmin: TSpinEdit;
    ses: TSpinEdit;
    ext: TSpinEdit;
    reg: TSpinEdit;
    Label34: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label26: TLabel;
    HeaderMagicDrop: TComboBox;
    Label21: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label7: TLabel;
    Endian: TComboBox;
    fTypeDrop: TComboBox;
    Label44: TLabel;
    xyzt_sizeDrop: TComboBox;
    xyzt_timeDrop: TComboBox;
    Xdim: TSpinEdit;
    Ydim: TSpinEdit;
    Zdim: TSpinEdit;
    Zmm: TFloatSpinEdit;
    OffsetEdit: TSpinEdit;
    TDim: TSpinEdit;
    Xmm: TFloatSpinEdit;
    TSec: TFloatSpinEdit;
    StatusBar1: TStatusBar;
    Label29: TLabel;
    Dim5Edit: TSpinEdit;
    TabSheet1: TTabSheet;
    Label35: TLabel;
    IntentCodeDrop: TComboBox;
    intent_p1Edit: TFloatSpinEdit;
    intent_p2Edit: TFloatSpinEdit;
    intent_p3Edit: TFloatSpinEdit;
    Label25: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    TabSheet2: TTabSheet;
    Label11: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label32: TLabel;
    slice_startEdit: TSpinEdit;
    Slice_durationEdit: TFloatSpinEdit;
    toffsetEdit: TFloatSpinEdit;
    TabSheet3: TTabSheet;
    cmax: TFloatSpinEdit;
    cmin: TFloatSpinEdit;
    Label12: TLabel;
    Label13: TLabel;
    Scale: TFloatSpinEdit;
    Label23: TLabel;
    Intercept: TFloatSpinEdit;
    Label22: TLabel;
    Label30: TLabel;
    Label33: TLabel;
    Page1: TMenuItem;
    Dimensions1: TMenuItem;
    ImageIntensity1: TMenuItem;
    Statistics1: TMenuItem;
    FunctionalMRI1: TMenuItem;
    Optional1: TMenuItem;
    TabSheet4: TTabSheet;
    Rotations1: TMenuItem;
    srow_x0Edit: TFloatSpinEdit;
    srow_x1Edit: TFloatSpinEdit;
    srow_x2Edit: TFloatSpinEdit;
    Label24: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    srow_y0Edit: TFloatSpinEdit;
    srow_y1Edit: TFloatSpinEdit;
    srow_y2Edit: TFloatSpinEdit;
    srow_z0Edit: TFloatSpinEdit;
    srow_z1Edit: TFloatSpinEdit;
    srow_z2Edit: TFloatSpinEdit;
    srow_x3Edit: TFloatSpinEdit;
    srow_y3Edit: TFloatSpinEdit;
    srow_z3Edit: TFloatSpinEdit;
    quatern_bEdit: TFloatSpinEdit;
    quatern_cEdit: TFloatSpinEdit;
    quatern_dEdit: TFloatSpinEdit;
    qoffset_x_Edit: TFloatSpinEdit;
    qoffset_y_Edit: TFloatSpinEdit;
    qoffset_z_Edit: TFloatSpinEdit;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Dim6Edit: TSpinEdit;
    Label42: TLabel;
    Dim7Edit: TSpinEdit;
    PixDim5: TFloatSpinEdit;
    PixDim6: TFloatSpinEdit;
    PixDim7: TFloatSpinEdit;
    SliceCodeDrop: TComboBox;
    Label20: TLabel;
    slice_endEdit: TSpinEdit;
    FreqDimDrop: TComboBox;
    PhaseDimDrop: TComboBox;
    SliceDimDrop: TComboBox;
    Label31: TLabel;
    Label43: TLabel;
    Label45: TLabel;
    QFacEdit: TFloatSpinEdit;
    Label46: TLabel;
    QFormDrop: TComboBox;
    SFormDrop: TComboBox;
    Label38: TLabel;
    Label47: TLabel;
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure SaveHdrDlgClose(Sender: TObject);
    procedure TabRequiredContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure WriteHdrForm (var lHdr: TMRIcroHdr);
    procedure ReadHdrDimensionsOnly (var lHdr: TMRIcroHdr); //reads only size dimensions: useful for computing estimated filesize
    procedure ReadHdrForm (var lHdr: TMRIcroHdr); //reads entire header
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure TabMenuClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageSzChange(Sender: TObject);
    procedure HeaderMagicDropSelect(Sender: TObject);
    function OpenAndDisplayHdr (var lFilename: string; var lHdr: TMRIcroHdr): boolean;
  private
	{ Private declarations }
{$IFNDEF FPC}	procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
{$ENDIF}
  public
	{ Public declarations }
  end;
	function OpenDialogExecute (lFilter,lCaption: string; lAllowMultiSelect: boolean): boolean;

var
  HdrForm: THdrForm;

implementation

uses nifti_img_view, render,nifti_img;
{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.DFM}
{$ENDIF}

function OpenDialogExecute (lFilter,lCaption: string; lAllowMultiSelect: boolean): boolean;
begin
	HdrForm.OpenHdrDlg.Filter := lFilter;
	HdrForm.OpenHdrDlg.FilterIndex := 1;
	HdrForm.OpenHdrDlg.Title := lCaption;
	if lAllowMultiSelect then
		HdrForm.OpenHdrDlg.Options := [ofAllowMultiSelect,ofFileMustExist];
	result := HdrForm.OpenHdrDlg.Execute;
	HdrForm.OpenHdrDlg.Options := [ofFileMustExist];
end;

function DropItem2DataType(lItemIndex: integer): integer; //returns NIfTI datatype number
begin
  case  lItemIndex of
     0: result :=1; //binary
     1 : result := 256; //8-bit S
     2 : result := 2; //8-bit int U*
     3 : result := 4; //16-bit int S*
     4 : result := 512; //16-bit int U
     5 : result := 8; //32-bit int S*
     6 : result := 768; //32-bit int U
     7: result := 1024; //64-bit int S
     8: result := 1280; //64-bit int U
     9: result := 16; //32-bit real*
     10: result := 64; //64-bit real*
     11: result := 1536; //128-bit real
     12: result := 128; //24-bit rgb
     13: result := 32; //64-bit complex
     14: result := 1792; //128-bit complex
     15: result := 2048; //256-bit complex
     else
       result := 0;
  end; //case
end; //func DropItem2DataType

function DataType2DropItem (lDataType: smallint): integer;
begin
  case  lDataType of
     1: result := 0; //binary
     256: result := 1; //8-bit S
     2: result := 2; //8-bit int U*
     4: result := 3; //16-bit int S*
     512: result := 4; //16-bit int U
     8: result := 5; //32-bit int S*
     768: result := 6; //32-bit int U
     1024: result := 7; //64-bit int S
     1280: result := 8; //64-bit int U
     16: result := 9; //32-bit real*
     64: result := 10; //64-bit real*
     1536: result := 11; //128-bit real
     128: result := 12; //24-bit rgb
     32: result := 13; //64-bit complex
     1792: result := 14; //128-bit complex
     2048: result := 15; //256-bit complex
     else
       result := 0;
  end; //case
end; //func DataType2DropItem

function DataType2BitsPerVoxel (lDataType: smallint): integer;
begin
  case  lDataType of
     1: result := 1; //binary
     256: result := 8; //8-bit S
     2: result := 8; //8-bit int U*
     4: result := 16; //16-bit int S*
     512: result := 16; //16-bit int U
     8: result := 32; //32-bit int S*
     768: result := 32; //32-bit int U
     1024: result := 64; //64-bit int S
     1280: result := 64; //64-bit int U
     16: result := 32; //32-bit real*
     64: result := 64; //64-bit real*
     1536: result := 128; //128-bit real
     128: result := 24; //24-bit rgb
     32: result := 64; //64-bit complex
     1792: result := 128; //128-bit complex
     2048: result := 256; //256-bit complex
     else
       result := 0;
  end; //case
end; //func DataType2BitsPerVoxel

function time_units2DropItem (lxyzt_units: byte): integer;
var lxyzt_unitsClipped: byte;
begin
     lxyzt_unitsClipped := lxyzt_units and 56;
     case lxyzt_unitsClipped of
          kNIFTI_UNITS_SEC  : result := 1;//= 8;
          kNIFTI_UNITS_MSEC : result := 2;//= 16;
          kNIFTI_UNITS_USEC : result := 3;//= 24;
          kNIFTI_UNITS_HZ   : result := 4;//= 32;
          kNIFTI_UNITS_PPM  : result := 5;//= 40;
          else result := 0; //unknown
     end; //case
end; //func time_units2DropItem

function DropItem2time_units (lDropItemIndex: byte): integer; //convert ComboBox index to NIFTI time units
begin
     case lDropItemIndex of
          1: result := kNIFTI_UNITS_SEC;
          2: result := kNIFTI_UNITS_MSEC;
          3: result := kNIFTI_UNITS_USEC;
          4: result := kNIFTI_UNITS_HZ;
          5: result := kNIFTI_UNITS_PPM;
          else result := 0; //unknown
     end; //case
end; //func DropItem2time_units

procedure THdrForm.WriteHdrForm (var lHdr: TMRIcroHdr); //writes a header to the various controls
var //lCStr: string[80];
    lInc: Integer;
    s: string;
begin
     //showmessage(format('%g %g %g', [lHdr.NIFTIhdr.qoffset_x, lHdr.NIFTIhdr.qoffset_y, lHdr.NIFTIhdr.qoffset_z]));

     with lHdr.NIFTIhdr do begin
		  //numDimEdit.value := dim[0];
          XDim.Value := dim[1];
          YDim.Value := dim[2];
          ZDim.Value := dim[3];
          TDim.Value := dim[4];
          Dim5Edit.value := dim[5];
          Dim6Edit.value := dim[6];
          Dim7Edit.value := dim[7];
          Xmm.Value := pixdim[1];
          Ymm.Value := pixdim[2];
          Zmm.Value := pixdim[3];
          TSec.Value := pixdim[4];
          PixDim5.value := pixdim[5];
          PixDim6.value := pixdim[6];
          PixDim7.value := pixdim[7];
          OffsetEdit.value := round(vox_offset);
          Scale.value := scl_slope;
          Intercept.value := scl_inter;
          {$IFNDEF FPC}
          fTypeDrop.SetItemIndex(  DataType2DropItem( datatype));
          if lHdr.NativeEndian then
             Endian.SetItemIndex(0)
          else
              Endian.SetItemIndex(1);
          //caption := inttohex(Magic);
          if Magic = kNIFTI_MAGIC_EMBEDDED_HDR then
			 HeaderMagicDrop.SetItemIndex(2)
		  else if Magic = kNIFTI_MAGIC_SEPARATE_HDR then
			 HeaderMagicDrop.SetItemIndex(1)
		  else if Magic = kswapNIFTI_MAGIC_EMBEDDED_HDR then
			 HeaderMagicDrop.SetItemIndex(2)
		  else if Magic = kswapNIFTI_MAGIC_SEPARATE_HDR then
			 HeaderMagicDrop.SetItemIndex(1)
		  else
             HeaderMagicDrop.SetItemIndex(0);
          xyzt_sizeDrop.SetItemIndex(xyzt_units and 3);
          xyzt_timeDrop.SetItemIndex(time_units2DropItem(xyzt_units));
          {$ELSE}
          fTypeDrop.ItemIndex := (  DataType2DropItem( datatype));
          if lHdr.DiskDataNativeEndian then
             Endian.ItemIndex:=(0)
          else
              Endian.ItemIndex:=(1);
          if Magic = kNIFTI_MAGIC_EMBEDDED_HDR then
			 HeaderMagicDrop.ItemIndex:=(2)
		  else if Magic = kNIFTI_MAGIC_SEPARATE_HDR then
			 HeaderMagicDrop.ItemIndex:=(1)
		  else if Magic = kswapNIFTI_MAGIC_EMBEDDED_HDR then
			 HeaderMagicDrop.ItemIndex:=(2)
		  else if Magic = kswapNIFTI_MAGIC_SEPARATE_HDR then
			 HeaderMagicDrop.ItemIndex:=(1)
		  else
             HeaderMagicDrop.ItemIndex:=(0);
          xyzt_sizeDrop.ItemIndex:=(xyzt_units and 3);
          xyzt_timeDrop.ItemIndex:=(time_units2DropItem(xyzt_units));
          {$ENDIF}

          CommentEdit.text := descrip;
          data_typeEdit.text := data_type;
          db_.text := db_name;
          aux.text := aux_file;
          intent_nameEdit.text := intent_name;
	  ext.value := extents;
	lInc := intent_code;
          if (intent_code > 1) and (intent_code <= kNIFTI_LAST_STATCODE) then
             lInc := lInc - 1 //intent_codes start from 2 not 1
          else if intent_code >= kNIFTI_FIRST_NONSTATCODE then //remove gap in numbers that follow final statcode
             lInc := (intent_code - kNIFTI_FIRST_NONSTATCODE)+kNIFTI_LAST_STATCODE
		  else begin
			  lInc := 0; //unknown
		  end;
      {$IFNDEF FPC}
		  IntentCodeDrop .SetItemIndex(lInc);
          SliceCodeDrop.SetItemIndex(slice_code);
          FreqDimDrop.SetItemIndex(dim_info and 3);
          PhaseDimDrop.SetItemIndex((dim_info shr 2) and 3);
          SliceDimDrop.SetItemIndex((dim_info shr 4) and 3);
  {$ELSE}
    IntentCodeDrop.ItemIndex:=lInc;
          SliceCodeDrop.ItemIndex:=(slice_code);
          FreqDimDrop.ItemIndex:=(dim_info and 3);
          PhaseDimDrop.ItemIndex:=((dim_info shr 2) and 3);
          SliceDimDrop.ItemIndex:=((dim_info shr 4) and 3);
  {$ENDIF}
          intent_p1Edit.value := intent_p1;
          intent_p2Edit.value := intent_p2;
          intent_p3Edit.value := intent_p3;
          ses.value := session_error;
          reg.value := ord(regular);
          slice_startEdit.value := slice_start;
          slice_endEdit.value := slice_end;
          cmax.value := cal_max;
          cmin.value := cal_min;
          slice_durationEdit.value := slice_duration;
          toffsetEdit.value := toffset;
          gmax.value := glmax;
          gmin.value := glmin;
          //Next: 3D orientation rotations
          QFacEdit.value := pixdim[0];
            {$IFNDEF FPC}
          QFormDrop.SetItemIndex(qform_code);
          SFormDrop.SetItemIndex(sform_code);
  {$ELSE}
          QFormDrop.ItemIndex:= (qform_code);
          SFormDrop.ItemIndex :=(sform_code);
  {$ENDIF}
          //showmessage(format('%g %g %g', [lHdr.NIFTIhdr.qoffset_x, lHdr.NIFTIhdr.qoffset_y, lHdr.NIFTIhdr.qoffset_z]));
          //showmessage(format('%g %g %g', [qoffset_x, qoffset_y, qoffset_z]));

          quatern_bEdit.value := quatern_b;
          quatern_cEdit.value := quatern_c;
          quatern_dEdit.value := quatern_d;
          qoffset_x_Edit.value := qoffset_x;
          qoffset_y_Edit.value := qoffset_y;
          qoffset_z_Edit.value := qoffset_z;
          //caption := format('%g %g %g', [srow_x[0], srow_x[1], srow_x[2]]);
		  srow_x0Edit.value := srow_x[0];//12 affine matrix values
          srow_x1Edit.value := srow_x[1];
          srow_x2Edit.value := srow_x[2];
          srow_x3Edit.value := srow_x[3];
          srow_y0Edit.value := srow_y[0];
          srow_y1Edit.value := srow_y[1];
          srow_y2Edit.value := srow_y[2];
          srow_y3Edit.value := srow_y[3];
          srow_z0Edit.value := srow_z[0];
          srow_z1Edit.value := srow_z[1];
          srow_z2Edit.value := srow_z[2];
          srow_z3Edit.value := srow_z[3];
          //Finally... check values
          HeaderMagicDropSelect(nil); //disable or enable offset based on image format
          //showmessage(lHdr.ECodeText);
          if length(lHdr.ECodeText) > 0 then begin
             s := lHdr.ECodeText;
             s  := StringReplace(s, chr (0), '',[rfReplaceAll, rfIgnoreCase]);
             s := AdjustLineBreaks(s);//, tlbsLF);
             EcodeMemo.Lines.Text:= s;
             EcodeMemo.Visible := true;
          end else
              EcodeMemo.Visible := false;

    end;  //with lHdr
end;

(*procedure ApplySaveDlgFilter (lSaveDlg: TSaveDialog);
var
   lLen,lPos,lPipes,lPipesReq: integer;
   lExt: string;
begin
     lPipesReq := (lSaveDlg.FilterIndex * 2)-1;
     if lPipesReq < 1 then exit;
     lLen := length(lSaveDlg.Filter);
     lPos := 1;
     lPipes := 0;
     while (lPos < lLen) and (lPipes < lPipesReq) do begin
           if lSaveDlg.Filter[lPos] = '|' then
              inc(lPipes);
           inc(lPos);
     end;
     if (lPos >= lLen) or (lPipes < lPipesReq) then
        exit;
     lExt := '';
     while (lPos <= lLen) and (lSaveDlg.Filter[lPos] <> '|') do begin
           if lSaveDlg.Filter[lPos] <> '*' then
              lExt := lExt + lSaveDlg.Filter[lPos];
           inc(lPos);
     end;
     if lExt <> '' then
        lSaveDlg.Filename := ChangeFileExt(lSaveDlg.Filename,lExt);
end;   *)

procedure THdrForm.SaveHdrDlgClose(Sender: TObject);
begin
  //ApplySaveDlgFilter(SaveHdrDlg);
end;



procedure THdrForm.TabRequiredContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure THdrForm.FormShow(Sender: TObject);
begin
      // ImgForm.OnLaunch;
end;


procedure THdrForm.PageControl1Change(Sender: TObject);
begin

end;

procedure THdrForm.ReadHdrDimensionsOnly (var lHdr: TMRIcroHdr); //reads only size dimensions: useful for computing estimated filesize
var
    lInc: Integer;
begin
     with lHdr.NIFTIhdr do begin
          dim[1] := round(XDim.Value);
          dim[2] := round(YDim.Value);
          dim[3] := round(ZDim.Value);
          dim[4] := round(TDim.Value);
          dim[5] := round(Dim5Edit.value);
          dim[6] := round(Dim6Edit.value);
          dim[7] := round(Dim7Edit.value);
          //Next: compute Dim[0]: compute number of dimensions by finding largest dimension with at least two samples
          lInc := 7;
          while dim[lInc] < 2 do
            dec(lInc);
          Dim[0] := lInc; //comp
          //showmessage(inttostr(Dim[0]));
          vox_offset := OffsetEdit.value;
          DataType := DropItem2DataType(FTypeDrop.ItemIndex);
          bitpix := DataType2BitsPerVoxel(DataType);
          if Endian.ItemIndex = 0 then
             lHdr.DiskDataNativeEndian := true
          else
             lHdr.DiskDataNativeEndian := false;
     end; //with NIfTIhdr
end; //proc ReadHdrDimensionsOnly

procedure THdrForm.ReadHdrForm (var lHdr: TMRIcroHdr); //read the values the user has entered
var
    lInc: Integer;
begin
     NIFTIhdr_ClearHdr(lHdr); //important: reset values like first 4 bytes = 348
     ReadHdrDimensionsOnly(lHdr);
     //StatusBar1.Panels[0].text := 'ImageData (bytes)= '+inttostr(ComputeImageDataBytes(lHdr));
     with lHdr.NIFTIhdr do begin
          pixdim[1] := Xmm.Value;
          pixdim[2] := Ymm.Value;
          pixdim[3] := Zmm.Value;
          pixdim[4] := TSec.Value;
          pixdim[5] := PixDim5.Value;
          pixdim[6] := PixDim6.Value;
          pixdim[7] := PixDim7.Value;
          scl_slope := Scale.value;
          scl_inter := Intercept.value;
          if HeaderMagicDrop.ItemIndex = 2 then
             Magic := kNIFTI_MAGIC_EMBEDDED_HDR
          else if HeaderMagicDrop.ItemIndex = 1 then
             Magic := kNIFTI_MAGIC_SEPARATE_HDR
          else
             Magic := 0; //not saed as NIFTI
          for lInc := 1 to 80 do
              descrip[lInc] := chr(0);
          for lInc := 1 to length(CommentEdit.text) do
              descrip[lInc]  := CommentEdit.text[lInc];
          for lInc := 1 to 10 do
              data_type[lInc] := chr(0);
          for lInc := 1 to length(data_typeEdit.text) do
              data_type[lInc] := data_typeEdit.text[lInc];
          for lInc := 1 to 18 do
              db_name[lInc] := chr(0);
          for lInc := 1 to length(db_.text) do
              db_name[lInc]  := db_.text[lInc];
          for lInc := 1 to 24 do
              aux_file[lInc] := chr(0);
          for lInc := 1 to length(aux.text) do
              aux_file[lInc]  := aux.text[lInc];
          for lInc := 1 to 16 do
              intent_name[lInc] := chr(0);
          for lInc := 1 to length(intent_nameEdit.text) do
              intent_name[lInc]  := intent_nameEdit.text[lInc];
          xyzt_units := xyzt_sizeDrop.ItemIndex;
          xyzt_units := xyzt_units+ (DropItem2time_units(xyzt_timeDrop.ItemIndex));
		  lInc := IntentCodeDrop.ItemIndex;
		  if (lInc > 0) and (lInc < kNIFTI_LAST_STATCODE) then
			 lInc := lInc + 1 //intent_codes start from 2 not 1
		  else if (lInc >= kNIFTI_LAST_STATCODE)  then //add gap in numbers between last stat code and misc codes
			 lInc := (lInc - kNIFTI_LAST_STATCODE)+kNIFTI_FIRST_NONSTATCODE
		  else
			  lInc := 0; //unknown
		  intent_code := lInc;
		  intent_p1 := intent_p1Edit.value;
          intent_p2 := intent_p2Edit.value;
          intent_p3 := intent_p3Edit.value;
          extents:= round(ext.value);
          session_error := round(ses.value);
          regular := chr(round(reg.value));
          dim_Info := FreqDimDrop.ItemIndex+(PhaseDimDrop.ItemIndex shl 2)+(SliceDimDrop.ItemIndex shl 4);
          slice_start := round(slice_startEdit.value);
          slice_end := round(slice_endEdit.value);
          slice_code := SliceCodeDrop.ItemIndex;
          Slice_duration := (Slice_DurationEdit.value);
          toffset := (toffsetEdit.value);
          cal_max := cmax.value;
          cal_min := cmin.value;
          glmax := round(gmax.value);
          glmin := round(gmin.value);
          //Next: 3D orientation rotations
          pixdim[0] := QFacEdit.value;
          qform_code := QFormDrop.ItemIndex;
          quatern_b := quatern_bEdit.value;
          quatern_c := quatern_cEdit.value;
          quatern_d := quatern_dEdit.value;
          qoffset_x := qoffset_x_Edit.value;
          qoffset_y := qoffset_y_Edit.value;
          qoffset_z := qoffset_z_Edit.value;
          sform_code  :=  SFormDrop.ItemIndex;
          srow_x[0] := srow_x0Edit.value;//12 affine matrix values
          srow_x[1] := srow_x1Edit.value;
          srow_x[2] := srow_x2Edit.value;
          srow_x[3] := srow_x3Edit.value;
          srow_y[0] := srow_y0Edit.value;
          srow_y[1] := srow_y1Edit.value;
          srow_y[2] := srow_y2Edit.value;
          srow_y[3] := srow_y3Edit.value;
          srow_z[0] := srow_z0Edit.value;
          srow_z[1] := srow_z1Edit.value;
          srow_z[2] := srow_z2Edit.value;
          srow_z[3] := srow_z3Edit.value;
     end; //with lHdr
     //zero_intercept := intercept.value;
end;

function THdrForm.OpenAndDisplayHdr (var lFilename: string; var lHdr: TMRIcroHdr): boolean;
var lFileDir: string;
begin
  FreeImgMemory(lHdr);
  result := false;
  NIFTIhdr_ClearHdr(lHdr);
  if not NIFTIhdr_LoadHdr(lFilename, lHdr) then exit;
  WriteHdrForm (lHdr);
  lFileDir := extractfiledir(lFilename);
  if lFileDir <> gTemplateDir then
     OpenHdrDlg.InitialDir := lFileDir;
  SaveHdrDlg.InitialDir := lFileDir;
  //999 ImgForm.SaveDialog1.InitialDir := lFileDir;
  SaveHdrDlg.FileName := lFilename; //make this default file to write
  StatusBar1.Panels[1].text := lFilename;
  StatusBar1.Panels[0].text := 'Img= '+inttostr(ComputeImageDataBytes(lHdr));
  result := true;
end;

procedure THdrForm.Open1Click(Sender: TObject);
var lHdr: TMRIcroHdr;
    lFilename: string;
begin
  //NIfTI (*.hdr;*.nii)|*.hdr; *.nii; *.nii.gz|NIfTI separate header (*.hdr)|*.hdr|NIfTI embedded header|*.nii|NIfTI compressed|*.nii.gz
  //if not OpenHdrDlg.Execute then exit;
  if not OpenDialogExecute(kImgFilter,'Select header',false) then exit;
  lFilename := OpenHdrDlg.Filename;
  OpenAndDisplayHdr(lFilename,lHdr);
end;

procedure THdrForm.Save1Click(Sender: TObject);
var lHdr: TMRIcroHdr;
    lFilename: string;
begin
  NIFTIhdr_ClearHdr(lHdr);
  if not SaveHdrDlg.Execute then exit;
  lFilename := SaveHdrDlg.Filename;
  OpenHdrDlg.InitialDir := extractfiledir(lFilename);
  //999 ImgForm.SaveDialog1.InitialDir := extractfiledir(lFilename);
  ReadHdrForm (lHdr);
  if not NIFTIhdr_SaveHdr (lFilename, lHdr,true) then exit;
  OpenHdrDlg.FileName := lFilename; //make this default file to open
  StatusBar1.Panels[1].text := 'wrote: '+lFilename;
end;

procedure THdrForm.TabMenuClick(Sender: TObject);
begin
     PageControl1.ActivePage := PageControl1.Pages[(Sender as TMenuItem).Tag];
end;

procedure THdrForm.Exit1Click(Sender: TObject); //Quit the program or form
begin
     Close;
end;

  {$IFNDEF FPC}
procedure THdrForm.WMDropFiles(var Msg: TWMDropFiles);  //implement drag and drop
//NOTE: requires 'ShellAPI' in uses clause
var lHdr: TMRIcroHdr;
  CFileName: array[0..MAX_PATH] of Char;
  lFilename: string;
begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then
    begin
         lFilename := CFilename;
         OpenAndDisplayHdr(lFileName, lHdr);
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;
  {$ENDIF}

procedure THdrForm.FormCreate(Sender: TObject);
var lHdr: TMRIcroHdr;
begin
  //DecimalSeparator := '.'; //important for reading DICOM data: e.g. Germans write '12,00' but DICOM is '12.00'
  {$IFNDEF Unix}	DragAcceptFiles(Handle, True); //engage drag and drop
  {$ENDIF}
  NIFTIhdr_ClearHdr(lHdr);
  HdrForm.WriteHdrForm (lHdr); //show default header
  {$IFDEF Darwin}
  {$IFNDEF LCLgtk} //only for Carbon compile
  Open1.ShortCut := ShortCut(Word('O'), [ssMeta]);
  Save1.ShortCut := ShortCut(Word('S'), [ssMeta]);
  Exit1.ShortCut := ShortCut(Word('W'), [ssMeta]);
  Dimensions1.ShortCut := ShortCut(Word('A'), [ssMeta]);
  Rotations1.ShortCut := ShortCut(Word('B'), [ssMeta]);
  ImageIntensity1.ShortCut := ShortCut(Word('I'), [ssMeta]);
  Statistics1.ShortCut := ShortCut(Word('D'), [ssMeta]);
  FunctionalMRI1.ShortCut := ShortCut(Word('E'), [ssMeta]);
  Optional1.ShortCut := ShortCut(Word('F'), [ssMeta]);
  {$ENDIF}
  {$ENDIF}
end;

procedure THdrForm.ImageSzChange(Sender: TObject); //report size of image data
var
	lHdr: TMRIcroHdr;
begin
	 NIFTIhdr_ClearHdr(lHdr); //important: reset values like first 4 bytes = 348
	 ReadHdrDimensionsOnly(lHdr);
	 StatusBar1.Panels[0].text := 'Img= '+inttostr(ComputeImageDataBytes(lHdr));
end;

procedure THdrForm.HeaderMagicDropSelect(Sender: TObject);
var lHdrIndex: integer;
begin
     lHdrIndex := HeaderMagicDrop.ItemIndex; //0=unkown, 1=nifti hdr+img, 2=nifti .nii embedded
     if lHdrIndex = 1 then begin//nifti hdr+img, offset must be = 0
        OffsetEdit.MinValue := 0;
        OffsetEdit.Enabled := false;
        OffsetEdit.value := 0;
     end else if lHdrIndex = 2 then begin//embedded header, offset must be at least 348
        OffsetEdit.Enabled := true;
        if OffsetEdit.value < sizeof(TNIFTIHdr) then
           OffsetEdit.value := sizeof(TNIFTIHdr);
        OffsetEdit.MinValue := sizeof(TNIFTIHdr);
     end else begin //no embedded header... therefore offset can be zero
        OffsetEdit.MinValue := 0;
        OffsetEdit.Enabled := true;

        if OffsetEdit.value = sizeof(TNIFTIHdr) then
		   OffsetEdit.value := 0;
     end;
end;


end.
