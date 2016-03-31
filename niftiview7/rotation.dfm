object RotationForm: TRotationForm
  Left = 327
  Top = 196
  BorderStyle = bsDialog
  Caption = 'Rotations'
  ClientHeight = 141
  ClientWidth = 134
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
  object LabelX: TLabel
    Left = 2
    Top = 8
    Width = 21
    Height = 13
    Caption = 'Yaw'
  end
  object Label1: TLabel
    Left = 2
    Top = 32
    Width = 24
    Height = 13
    Caption = 'Pitch'
  end
  object Label2: TLabel
    Left = 2
    Top = 56
    Width = 18
    Height = 13
    Caption = 'Roll'
  end
  object ResliceBtn: TSpeedButton
    Left = 2
    Top = 88
    Width = 129
    Height = 22
    Caption = 'Create resliced image'
    OnClick = ResliceBtnClick
  end
  object AdjustMatrixBtn: TSpeedButton
    Left = 2
    Top = 112
    Width = 129
    Height = 22
    Caption = 'Adjust matrix'
    OnClick = AdjustMatrixBtnClick
  end
  object YawEdit: TRxSpinEdit
    Left = 56
    Top = 4
    Width = 70
    Height = 21
    ButtonKind = bkStandard
    Decimal = 4
    ValueType = vtFloat
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = YawPitchRollChange
  end
  object PitchEdit: TRxSpinEdit
    Left = 56
    Top = 28
    Width = 70
    Height = 21
    ButtonKind = bkStandard
    Decimal = 4
    ValueType = vtFloat
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnChange = YawPitchRollChange
  end
  object ROllEdit: TRxSpinEdit
    Left = 56
    Top = 52
    Width = 70
    Height = 21
    ButtonKind = bkStandard
    Decimal = 4
    ValueType = vtFloat
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnChange = YawPitchRollChange
  end
end
