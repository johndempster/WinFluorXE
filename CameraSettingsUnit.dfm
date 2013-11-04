object CameraSettingsFrm: TCameraSettingsFrm
  Left = 778
  Top = 666
  Width = 371
  Height = 164
  Caption = 'Camera Settings'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CCDCoolingGrp: TGroupBox
    Left = 8
    Top = 4
    Width = 161
    Height = 93
    Caption = ' CCD Cooling  '
    TabOrder = 0
    object Label41: TLabel
      Left = 22
      Top = 65
      Width = 51
      Height = 15
      Alignment = taRightJustify
      Caption = 'Set Point'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 16
      Top = 38
      Width = 54
      Height = 15
      Caption = 'Fan Mode'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edTemperatureSetPoint: TValidatedEdit
      Left = 80
      Top = 65
      Width = 73
      Height = 20
      AutoSize = False
      Text = ' 0 DEGC'
      Scale = 1.000000000000000000
      Units = 'DEGC'
      NumberFormat = '%.4g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object ckCameraCooling: TCheckBox
      Left = 16
      Top = 16
      Width = 137
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Peltier Cooling On'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
    object cbFanMode: TComboBox
      Left = 88
      Top = 38
      Width = 65
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object bOK: TButton
    Left = 8
    Top = 104
    Width = 49
    Height = 20
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 64
    Top = 104
    Width = 49
    Height = 18
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = bCancelClick
  end
  object SpecialGrp: TGroupBox
    Left = 176
    Top = 4
    Width = 161
    Height = 93
    Caption = ' Special Modes '
    TabOrder = 3
    object ckDisableEMCCD: TCheckBox
      Left = 16
      Top = 16
      Width = 137
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Disable EMCCD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
  end
end
