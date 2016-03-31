object AnatForm: TAnatForm
  Left = 246
  Top = 878
  BorderStyle = bsDialog
  Caption = 'Landmarks'
  ClientHeight = 27
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 427
    Height = 29
    ButtonHeight = 21
    Caption = 'ToolBar1'
    TabOrder = 0
    object OpenBtn: TSpeedButton
      Left = 0
      Top = 2
      Width = 56
      Height = 21
      Caption = 'Open'
      OnClick = OpenBtnClick
    end
    object SaveBtn: TSpeedButton
      Left = 56
      Top = 2
      Width = 56
      Height = 21
      Caption = 'Save'
      OnClick = SaveBtnClick
    end
    object AnatDrop: TComboBox
      Left = 112
      Top = 2
      Width = 145
      Height = 21
      Style = csDropDownList
      DropDownCount = 24
      ItemHeight = 13
      TabOrder = 0
      OnChange = AnatDropChange
    end
    object AddBtn: TSpeedButton
      Left = 257
      Top = 2
      Width = 56
      Height = 21
      Caption = 'Add'
      OnClick = AddBtnClick
    end
    object UpdateBtn: TSpeedButton
      Left = 313
      Top = 2
      Width = 56
      Height = 21
      Caption = 'Update'
      OnClick = UpdateBtnClick
    end
    object DeleteBtn: TSpeedButton
      Left = 369
      Top = 2
      Width = 56
      Height = 21
      Caption = 'Delete'
      OnClick = DeleteBtnClick
    end
  end
end
