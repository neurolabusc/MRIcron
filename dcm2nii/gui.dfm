�
 TMAINFORM 0�	  TPF0	TMainFormMainFormLeftTop� Width�Height�Caption&Drag and Drop DICOM to NIfTI converterColor	clBtnFaceDragModedmAutomaticFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style Menu	MainMenu1OldCreateOrderPositionpoScreenCenterOnClose	FormCloseOnCreate
FormCreateOnShowFormShowPixelsPerInch`
TextHeight TMemoMemo1Left Top!Width�Height�AlignalClientReadOnly	
ScrollBars
ssVerticalTabOrder   TPanelPanel1Left Top Width�Height!AlignalTop
BevelOuterbvNoneTabOrder TLabelLabel1Left TopWidthOHeightCaption  Output Format: LayouttlCenter  	TComboBox	TypeComboLeftOTopWidth� HeightStylecsDropDownList
ItemHeightTabOrder OnChangeTypeComboChangeItems.StringsSPM2 (3D Anlyze hdr/img)SPM5 (3D NIfTI hdr/img)SPM8 (3D NIfTI nii)4D NIfTI hdr/imgFSL (4D NIfTI nii)Compressed FSL (4D NIfTI nii)    	TMainMenu	MainMenu1Left� TopP 	TMenuItemFile1CaptionFile 	TMenuItemDICOMtoNIfTI1CaptionDICOM to NIfTIShortCutD@OnClickdcm2niiBtnClick  	TMenuItemModifyNIfTI1CaptionModify NIfTIOnClickModifyNIfTI1Click  	TMenuItem
NIfTI3D4D1CaptionNIfTI 3D -> 4DOnClickNIfTI3D4D1Click  	TMenuItemAnonymizeDICOM1CaptionAnonymize DICOMOnClickAnonymizeDICOM1Click  	TMenuItemResliceNIfTI1CaptionReslice NIfTIOnClickResliceNIfTI1Click  	TMenuItemExit1CaptionExitOnClick
Exit1Click  	TMenuItemDeletenondcm1CaptionDelete non .dcmVisibleOnClickDeletenondcm1Click   	TMenuItemEdit1CaptionEdit 	TMenuItemCopy1CaptionCopyShortCutC@OnClick
Copy1Click   	TMenuItemUntestedMenuCaptionUntested 	TMenuItemMirrorXdimension1CaptionMirror X-dimensionOnClickMirrorXdimension1Click  	TMenuItemSumTPM1CaptionSum TPMOnClickSumTPM1Click  	TMenuItemExtractDICOMdims1CaptionExtract DICOM dimensionsOnClickExtractDICOMdims1Click  	TMenuItemExtractDICOMhdr1CaptionExtract DICOM headersOnClickExtractDICOMhdr1Click  	TMenuItemExtractNIfTIhdrs1CaptionExtract NIfTI headersOnClickExtractNIfTIhdrs1Click  	TMenuItem
HalveMenu1CaptionHalve dimensions inplaneOnClickHalveMenu1Click   	TMenuItemHelp1CaptionHelp 	TMenuItemPreferences1CaptionPreferences...ShortCutP@OnClickPreferences1Click  	TMenuItemAbout1CaptionAbout...ShortCutA@OnClickAbout1Click    TOpenDialog
OpenHdrDlgLeft� TopP   