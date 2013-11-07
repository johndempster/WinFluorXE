object CameraSettingsFrm: TCameraSettingsFrm
  Left = 670
  Top = 448
  Width = 349
  Height = 198
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
    Height = 125
    Caption = ' CCD Cooling  '
    TabOrder = 0
    object Label41: TLabel
      Left = 8
      Top = 76
      Width = 49
      Height = 14
      Alignment = taRightJustify
      Caption = 'Set Point'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 8
      Top = 34
      Width = 53
      Height = 14
      Caption = 'Fan Mode'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edTemperatureSetPoint: TValidatedEdit
      Left = 8
      Top = 92
      Width = 145
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
      Left = 8
      Top = 16
      Width = 145
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Peltier Cooling On'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
    object cbFanMode: TComboBox
      Left = 8
      Top = 50
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object bOK: TButton
    Left = 8
    Top = 136
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
    Top = 136
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
    Height = 125
    Caption = ' CCD Readout '
    TabOrder = 3
    object Label2: TLabel
      Left = 8
      Top = 16
      Width = 93
      Height = 14
      Caption = 'Readout A/D Gain'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 56
      Width = 128
      Height = 14
      Caption = 'Vertical Line Shift Time'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cbADCGain: TComboBox
      Left = 8
      Top = 32
      Width = 145
      Height = 21
      Hint = 'Voltage gain applied before A/D conversion'
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'cbADCGain'
    end
    object cbCCDVerticalShiftSpeed: TComboBox
      Left = 8
      Top = 72
      Width = 145
      Height = 21
      Hint = 
        'Reduce time to increase frame rate at expense of pixel well dept' +
        'h.'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
  end
end
