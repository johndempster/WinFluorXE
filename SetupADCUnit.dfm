object SetupADCFrm: TSetupADCFrm
  Left = 296
  Top = 333
  Width = 655
  Height = 457
  Caption = 'Setup Patch Clamp / Analogue Inputs'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object GroupBox3: TGroupBox
    Left = 8
    Top = 0
    Width = 205
    Height = 113
    Caption = ' Analogue Inputs '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 80
      Top = 16
      Width = 74
      Height = 15
      Alignment = taRightJustify
      Caption = 'No. Channels'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 15
      Top = 40
      Width = 98
      Height = 15
      Alignment = taRightJustify
      Caption = 'Sampling Interval'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object Label3: TLabel
      Left = 8
      Top = 68
      Width = 86
      Height = 37
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ADC Voltage Range'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object edNumChannels: TValidatedEdit
      Left = 157
      Top = 16
      Width = 34
      Height = 20
      OnKeyPress = edNumChannelsKeyPress
      AutoSize = False
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 16.000000000000000000
    end
    object edScanInterval: TValidatedEdit
      Left = 117
      Top = 42
      Width = 74
      Height = 20
      AutoSize = False
      Text = ' 0.100 ms'
      Value = 0.000100000004749745
      Scale = 1000.000000000000000000
      Units = 'ms'
      NumberFormat = '%.3f'
      LoLimit = 0.000009999999747379
      HiLimit = 1.000000015047466E30
    end
    object cbADCVoltageRange: TComboBox
      Left = 101
      Top = 68
      Width = 92
      Height = 23
      Style = csDropDownList
      ItemHeight = 15
      ParentShowHint = False
      ShowHint = False
      TabOrder = 2
    end
  end
  object ChannelsGrp: TGroupBox
    Left = 221
    Top = 0
    Width = 228
    Height = 425
    Caption = ' Channel calibration table '
    TabOrder = 1
    object ChannelTable: TStringGrid
      Left = 8
      Top = 16
      Width = 209
      Height = 401
      Hint = 'Input channel scaling factors and calibration units'
      ColCount = 4
      DefaultColWidth = 50
      DefaultRowHeight = 18
      RowCount = 9
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssNone
      TabOrder = 0
      RowHeights = (
        18
        18
        18
        18
        18
        18
        18
        18
        18)
    end
  end
  object GroupBox18: TGroupBox
    Left = 8
    Top = 114
    Width = 205
    Height = 103
    Caption = ' Amplifier #1'
    TabOrder = 2
    object Label15: TLabel
      Left = 37
      Top = 70
      Width = 112
      Height = 15
      Alignment = taRightJustify
      Caption = 'Vout 0 Divide Factor'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object cbAmplifier1: TComboBox
      Left = 8
      Top = 16
      Width = 185
      Height = 23
      ItemHeight = 15
      TabOrder = 0
      Text = 'cbAmplifier1'
      OnChange = cbAmplifier1Change
    end
    object edVDivide0: TValidatedEdit
      Left = 155
      Top = 70
      Width = 41
      Height = 20
      OnKeyPress = edVDivide0KeyPress
      AutoSize = False
      Text = ' 0 X'
      Scale = 1.000000000000000000
      Units = 'X'
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object GainTelPanel1: TPanel
      Left = 8
      Top = 40
      Width = 93
      Height = 25
      BevelOuter = bvNone
      Caption = 'l'
      TabOrder = 2
      object lbTelegraphChannel: TLabel
        Left = 17
        Top = 2
        Width = 47
        Height = 14
        Alignment = taRightJustify
        Caption = 'Gain Tel.'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object edGainTelegraphChannel1: TValidatedEdit
        Left = 66
        Top = 2
        Width = 22
        Height = 20
        AutoSize = False
        Text = ' 0 '
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = -1.000000015047466E30
        HiLimit = 15.000000000000000000
      end
    end
    object ModeTelPanel1: TPanel
      Left = 106
      Top = 40
      Width = 93
      Height = 25
      BevelOuter = bvNone
      TabOrder = 3
      object Label4: TLabel
        Left = 10
        Top = 2
        Width = 54
        Height = 14
        Alignment = taRightJustify
        Caption = 'Mode Tel.'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object edModeTelegraphChannel1: TValidatedEdit
        Left = 66
        Top = 2
        Width = 22
        Height = 20
        AutoSize = False
        Text = ' 0 '
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = -1.000000000000000000
        HiLimit = 15.000000000000000000
      end
    end
  end
  object bOK: TButton
    Left = 8
    Top = 329
    Width = 49
    Height = 20
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 3
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 64
    Top = 329
    Width = 49
    Height = 17
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 4
    OnClick = bCancelClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 218
    Width = 205
    Height = 103
    Caption = ' Amplifier #2'
    TabOrder = 5
    object Label5: TLabel
      Left = 37
      Top = 70
      Width = 112
      Height = 15
      Alignment = taRightJustify
      Caption = 'Vout 1 Divide Factor'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object cbAmplifier2: TComboBox
      Left = 8
      Top = 16
      Width = 185
      Height = 23
      ItemHeight = 15
      TabOrder = 0
      Text = 'cbAmplifiers'
      OnChange = cbAmplifier1Change
    end
    object edVDivide1: TValidatedEdit
      Left = 155
      Top = 70
      Width = 41
      Height = 20
      OnKeyPress = edVDivide0KeyPress
      AutoSize = False
      Text = ' 0 X'
      Scale = 1.000000000000000000
      Units = 'X'
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object GainTelPanel2: TPanel
      Left = 8
      Top = 40
      Width = 93
      Height = 25
      BevelOuter = bvNone
      Caption = 'l'
      TabOrder = 2
      object Label7: TLabel
        Left = 17
        Top = 2
        Width = 47
        Height = 14
        Alignment = taRightJustify
        Caption = 'Gain Tel.'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object edGainTelegraphChannel2: TValidatedEdit
        Left = 66
        Top = 2
        Width = 22
        Height = 20
        AutoSize = False
        Text = ' 0 '
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = -1.000000015047466E30
        HiLimit = 15.000000000000000000
      end
    end
    object ModeTelPanel2: TPanel
      Left = 106
      Top = 40
      Width = 93
      Height = 25
      BevelOuter = bvNone
      TabOrder = 3
      object Label8: TLabel
        Left = 10
        Top = 2
        Width = 54
        Height = 14
        Alignment = taRightJustify
        Caption = 'Mode Tel.'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object edModeTelegraphChannel2: TValidatedEdit
        Left = 66
        Top = 2
        Width = 22
        Height = 20
        AutoSize = False
        Text = ' 0 '
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = -1.000000000000000000
        HiLimit = 15.000000000000000000
      end
    end
  end
  object CapGrp: TGroupBox
    Left = 456
    Top = 0
    Width = 185
    Height = 305
    Caption = ' Cell Capacity '
    TabOrder = 6
    object GroupBox12: TGroupBox
      Left = 8
      Top = 104
      Width = 169
      Height = 89
      Caption = ' Capacity Compensation '
      TabOrder = 0
      object Label29: TLabel
        Left = 7
        Top = 37
        Width = 65
        Height = 15
        Caption = 'Series Res.'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label32: TLabel
        Left = 7
        Top = 61
        Width = 72
        Height = 15
        Caption = 'Cell Capacity'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object edCapRSeriesComp: TValidatedEdit
        Left = 96
        Top = 37
        Width = 67
        Height = 20
        AutoSize = False
        Text = ' 0 MOhm'
        Scale = 0.000000999999997475
        Units = 'MOhm'
        NumberFormat = '%.5g'
        HiLimit = 1.000000015047466E30
      end
      object edCapCellCapacityComp: TValidatedEdit
        Left = 96
        Top = 61
        Width = 67
        Height = 20
        AutoSize = False
        Text = ' 0 pF'
        Scale = 999999995904.000000000000000000
        Units = 'pF'
        NumberFormat = '%.4g'
        HiLimit = 1.000000015047466E30
      end
      object ckCapacityCompensationInUse: TCheckBox
        Left = 8
        Top = 16
        Width = 145
        Height = 17
        BiDiMode = bdLeftToRight
        Caption = 'Use compensation '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentBiDiMode = False
        ParentFont = False
        TabOrder = 2
      end
    end
    object GroupBox7: TGroupBox
      Left = 8
      Top = 32
      Width = 169
      Height = 65
      TabOrder = 1
      object Label19: TLabel
        Left = 7
        Top = 13
        Width = 79
        Height = 15
        Caption = 'Excitation freq.'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label20: TLabel
        Left = 7
        Top = 37
        Width = 73
        Height = 15
        Caption = 'Reversal Pot.'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object edCapFrequency: TValidatedEdit
        Left = 96
        Top = 13
        Width = 67
        Height = 20
        AutoSize = False
        Text = ' 10 Hz'
        Value = 10.000000000000000000
        Scale = 1.000000000000000000
        Units = 'Hz'
        NumberFormat = '%.5g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edCapVRev: TValidatedEdit
        Left = 96
        Top = 37
        Width = 67
        Height = 20
        AutoSize = False
        Text = ' 0 mV'
        Scale = 1000.000000000000000000
        Units = 'mV'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
    end
    object GroupBox6: TGroupBox
      Left = 8
      Top = 200
      Width = 169
      Height = 97
      Caption = ' Display ranges '
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object Label9: TLabel
        Left = 67
        Top = 72
        Width = 23
        Height = 15
        Alignment = taRightJustify
        Caption = 'C.m'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label17: TLabel
        Left = 67
        Top = 24
        Width = 23
        Height = 15
        Alignment = taRightJustify
        Caption = 'G.m'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object Label21: TLabel
        Left = 71
        Top = 48
        Width = 19
        Height = 15
        Alignment = taRightJustify
        Caption = 'G.s'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object edCapCmDisplayMax: TValidatedEdit
        Left = 96
        Top = 69
        Width = 67
        Height = 20
        AutoSize = False
        Text = ' 10 pF'
        Value = 10.000000000000000000
        Scale = 1.000000000000000000
        Units = 'pF'
        NumberFormat = '%.5g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edCapGmDisplayMax: TValidatedEdit
        Left = 96
        Top = 21
        Width = 67
        Height = 20
        AutoSize = False
        Text = ' 10 nS'
        Value = 10.000000000000000000
        Scale = 1.000000000000000000
        Units = 'nS'
        NumberFormat = '%.5g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edCapGsDisplayMax: TValidatedEdit
        Left = 96
        Top = 45
        Width = 67
        Height = 20
        AutoSize = False
        Text = ' 10 nS'
        Value = 10.000000000000000000
        Scale = 1.000000000000000000
        Units = 'nS'
        NumberFormat = '%.5g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
    end
    object ckCapEnabled: TCheckBox
      Left = 8
      Top = 16
      Width = 169
      Height = 17
      Caption = 'Enable'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = ckCapEnabledClick
    end
  end
end
