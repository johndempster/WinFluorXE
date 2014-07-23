object SealTestFrm: TSealTestFrm
  Left = 532
  Top = 298
  Caption = 'Signals Monitor (Seal Test)'
  ClientHeight = 678
  ClientWidth = 591
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  FormStyle = fsMDIChild
  KeyPreview = True
  OldCreateOrder = True
  Position = poMainFormCenter
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object scDisplay: TScopeDisplay
    Left = 160
    Top = 8
    Width = 393
    Height = 209
    CursorChangeInProgress = False
    NumChannels = 1
    NumPoints = 1024
    MaxPoints = 1024
    XMin = 0
    XMax = 1023
    XOffset = 0
    CursorsEnabled = True
    TScale = 1000.000000000000000000
    TUnits = 's'
    TCalBar = -1.000000000000000000
    ZoomDisableHorizontal = True
    ZoomDisableVertical = False
    DisableChannelVisibilityButton = False
    PrinterFontSize = 0
    PrinterPenWidth = 0
    PrinterLeftMargin = 0
    PrinterRightMargin = 0
    PrinterTopMargin = 0
    PrinterBottomMargin = 0
    PrinterDisableColor = False
    PrinterShowLabels = True
    PrinterShowZeroLevels = True
    MetafileWidth = 0
    MetafileHeight = 0
    StorageMode = False
    RecordNumber = -1
    DisplayGrid = True
    MaxADCValue = 2047
    MinADCValue = -2048
    NumBytesPerSample = 2
    FixZeroLevels = False
    DisplaySelected = False
    FontSize = 8
  end
  object ChannelsGrp: TGroupBox
    Left = 8
    Top = 0
    Width = 145
    Height = 97
    Caption = ' Channels '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 41
      Height = 15
      Caption = 'Current'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 40
      Height = 15
      Caption = 'Voltage'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object cbCurrentChannel: TComboBox
      Left = 56
      Top = 16
      Width = 73
      Height = 23
      Hint = 'Channel containing current signal'
      Style = csDropDownList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = cbCurrentChannelChange
    end
    object cbVoltageChannel: TComboBox
      Left = 56
      Top = 46
      Width = 73
      Height = 23
      Hint = 'Channel containing voltage signal'
      Style = csDropDownList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnChange = cbCurrentChannelChange
    end
    object rbAmplifier1: TRadioButton
      Left = 8
      Top = 72
      Width = 81
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Amplifier #1 '
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      TabStop = True
      OnClick = rbAmplifier1Click
    end
    object rbAmplifier2: TRadioButton
      Left = 96
      Top = 72
      Width = 33
      Height = 17
      Alignment = taLeftJustify
      Caption = '#2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = rbAmplifier2Click
    end
  end
  object VoltsGrp: TGroupBox
    Left = 160
    Top = 240
    Width = 113
    Height = 169
    Caption = ' Voltage '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label5: TLabel
      Left = 8
      Top = 16
      Width = 43
      Height = 15
      Caption = 'Holding'
    end
    object Label6: TLabel
      Left = 8
      Top = 56
      Width = 32
      Height = 15
      Caption = 'Pulse'
    end
    object edVHold: TValidatedEdit
      Left = 8
      Top = 34
      Width = 97
      Height = 20
      OnKeyPress = EdHoldingVoltage3KeyPress
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      Text = '   0.0 mV'
      Scale = 1.000000000000000000
      Units = 'mV'
      NumberFormat = '%5.1f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object edVPulse: TValidatedEdit
      Left = 8
      Top = 74
      Width = 97
      Height = 20
      OnKeyPress = EdHoldingVoltage3KeyPress
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      Text = '   0.0 mV'
      Scale = 1.000000000000000000
      Units = 'mV'
      NumberFormat = '%5.1f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
  end
  object CurrentGrp: TGroupBox
    Left = 280
    Top = 240
    Width = 113
    Height = 169
    Caption = ' Current '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object Label7: TLabel
      Left = 8
      Top = 16
      Width = 43
      Height = 15
      Caption = 'Holding'
    end
    object Label8: TLabel
      Left = 8
      Top = 56
      Width = 32
      Height = 15
      Caption = 'Pulse'
    end
    object edIHold: TValidatedEdit
      Left = 8
      Top = 34
      Width = 97
      Height = 20
      OnKeyPress = EdHoldingVoltage3KeyPress
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      Text = '   0.0 mV'
      Scale = 1.000000000000000000
      Units = 'mV'
      NumberFormat = '%5.1f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object edIPulse: TValidatedEdit
      Left = 8
      Top = 74
      Width = 97
      Height = 20
      OnKeyPress = EdHoldingVoltage3KeyPress
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      Text = '   0.0 mV'
      Scale = 1.000000000000000000
      Units = 'mV'
      NumberFormat = '%5.1f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
  end
  object CellGrp: TGroupBox
    Left = 400
    Top = 240
    Width = 153
    Height = 169
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object PageControl1: TPageControl
      Left = 4
      Top = 12
      Width = 141
      Height = 105
      ActivePage = PipetteTab
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object PipetteTab: TTabSheet
        Caption = 'Pipette'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        object Label9: TLabel
          Left = 8
          Top = 16
          Width = 64
          Height = 15
          Caption = 'Resistance'
        end
        object edResistance: TValidatedEdit
          Left = 4
          Top = 34
          Width = 117
          Height = 23
          OnKeyPress = EdHoldingVoltage3KeyPress
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -15
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Text = '     0.0 MOhm'
          Scale = 0.000000999999997475
          Units = 'MOhm'
          NumberFormat = '%7.1f'
          LoLimit = -1.000000015047466E29
          HiLimit = 1.000000015047466E29
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'Cell'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label15: TLabel
          Left = 10
          Top = 4
          Width = 16
          Height = 15
          Caption = 'Ga'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label17: TLabel
          Left = 7
          Top = 28
          Width = 20
          Height = 15
          Caption = 'Gm'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label16: TLabel
          Left = 7
          Top = 52
          Width = 20
          Height = 15
          Caption = 'Cm'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object edGaccess: TValidatedEdit
          Left = 30
          Top = 4
          Width = 91
          Height = 20
          OnKeyPress = EdHoldingVoltage3KeyPress
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          Text = ' 0 nS'
          Scale = 1000000000.000000000000000000
          Units = 'nS'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object edGmembrane: TValidatedEdit
          Left = 30
          Top = 28
          Width = 91
          Height = 20
          OnKeyPress = EdHoldingVoltage3KeyPress
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          Text = ' 0 nS'
          Scale = 1000000000.000000000000000000
          Units = 'nS'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object edCmembrane: TValidatedEdit
          Left = 30
          Top = 52
          Width = 91
          Height = 20
          OnKeyPress = EdHoldingVoltage3KeyPress
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          Text = ' 0 pF'
          Scale = 999999995904.000000000000000000
          Units = 'pF'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 120
      Width = 137
      Height = 41
      Caption = ' Ga estimate from'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object rbGaFromPeak: TRadioButton
        Left = 8
        Top = 16
        Width = 49
        Height = 17
        Caption = 'Peak'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object rbGaFromExp: TRadioButton
        Left = 56
        Top = 16
        Width = 73
        Height = 17
        Caption = 'Exp. Amp.'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        TabStop = True
      end
    end
  end
  object DACGrp: TGroupBox
    Left = 8
    Top = 100
    Width = 145
    Height = 317
    Caption = ' Test Pulse '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    object Label3: TLabel
      Left = 15
      Top = 60
      Width = 55
      Height = 15
      Alignment = taRightJustify
      Caption = 'Amplitude'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label11: TLabel
      Left = 34
      Top = 36
      Width = 36
      Height = 15
      Alignment = taRightJustify
      Caption = 'V Hold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label12: TLabel
      Left = 34
      Top = 114
      Width = 36
      Height = 15
      Alignment = taRightJustify
      Caption = 'V Hold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label14: TLabel
      Left = 15
      Top = 138
      Width = 55
      Height = 15
      Alignment = taRightJustify
      Caption = 'Amplitude'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 9
      Top = 250
      Width = 64
      Height = 15
      Alignment = taRightJustify
      Caption = 'Pulse width'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label13: TLabel
      Left = 34
      Top = 190
      Width = 36
      Height = 15
      Alignment = taRightJustify
      Caption = 'V Hold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Shape1: TShape
      Left = 8
      Top = 244
      Width = 130
      Height = 1
    end
    object Shape2: TShape
      Left = 7
      Top = 276
      Width = 130
      Height = 1
    end
    object Label10: TLabel
      Left = 13
      Top = 284
      Width = 56
      Height = 15
      Alignment = taRightJustify
      Caption = 'O/P Chan.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label20: TLabel
      Left = 15
      Top = 214
      Width = 55
      Height = 15
      Alignment = taRightJustify
      Caption = 'Amplitude'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object rbUseHoldingVoltage1: TRadioButton
      Left = 8
      Top = 16
      Width = 89
      Height = 17
      Hint = 'Select test pulse #1 (F3)'
      Caption = 'Pulse #1 (F3)'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TabStop = True
      OnClick = rbUseHoldingVoltage1Click
    end
    object rbUseHoldingVoltage2: TRadioButton
      Left = 8
      Top = 92
      Width = 89
      Height = 17
      Hint = 'Select test pulse #2 (F4)'
      Caption = 'Pulse #2 (F4)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = rbUseHoldingVoltage2Click
    end
    object rbUseHoldingVoltage3: TRadioButton
      Left = 8
      Top = 168
      Width = 89
      Height = 17
      Hint = 'Select test pulse #3 (No pulse) (F5)'
      Caption = 'Pulse #3 (F5)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = rbUseHoldingVoltage3Click
    end
    object edHoldingVoltage1: TValidatedEdit
      Left = 72
      Top = 36
      Width = 65
      Height = 20
      OnKeyPress = edHoldingVoltage1KeyPress
      AutoSize = False
      Text = ' 0 mV'
      Scale = 1000.000000000000000000
      Units = 'mV'
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object edPulseHeight1: TValidatedEdit
      Left = 72
      Top = 60
      Width = 65
      Height = 20
      OnKeyPress = edPulseHeight1KeyPress
      AutoSize = False
      Text = ' 0 mV'
      Scale = 1000.000000000000000000
      Units = 'mV'
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object edHoldingVoltage2: TValidatedEdit
      Left = 72
      Top = 114
      Width = 65
      Height = 20
      OnKeyPress = edHoldingVoltage2KeyPress
      AutoSize = False
      Text = ' 0 mV'
      Scale = 1000.000000000000000000
      Units = 'mV'
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object edPulseHeight2: TValidatedEdit
      Left = 72
      Top = 138
      Width = 65
      Height = 20
      OnKeyPress = edPulseheight2KeyPress
      AutoSize = False
      Text = ' 0 mV'
      Scale = 1000.000000000000000000
      Units = 'mV'
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object edHoldingVoltage3: TValidatedEdit
      Left = 72
      Top = 190
      Width = 65
      Height = 20
      OnKeyPress = EdHoldingVoltage3KeyPress
      AutoSize = False
      Text = ' 0 mV'
      Scale = 1000.000000000000000000
      Units = 'mV'
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object edPulseWidth: TValidatedEdit
      Left = 80
      Top = 250
      Width = 57
      Height = 20
      OnKeyPress = edPulseWidthKeyPress
      AutoSize = False
      Text = ' 0 ms'
      Scale = 1000.000000000000000000
      Units = 'ms'
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object cbDACOut: TComboBox
      Left = 72
      Top = 284
      Width = 65
      Height = 23
      Hint = 'Channel containing voltage signal'
      Style = csDropDownList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnChange = cbCurrentChannelChange
    end
    object edPulseHeight3: TValidatedEdit
      Left = 72
      Top = 214
      Width = 65
      Height = 20
      OnKeyPress = edPulseHeight3KeyPress
      AutoSize = False
      Text = ' 0 mV'
      Scale = 1000.000000000000000000
      Units = 'mV'
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
  end
  object TimerGrp: TGroupBox
    Left = 8
    Top = 592
    Width = 137
    Height = 49
    Caption = ' Timer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    object edTimer: TEdit
      Left = 72
      Top = 16
      Width = 57
      Height = 25
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      Text = 'edTimer'
    end
    object bResetTimer: TButton
      Left = 16
      Top = 16
      Width = 49
      Height = 17
      Caption = 'Reset'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = bResetTimerClick
    end
    object GroupBox1: TGroupBox
      Left = 0
      Top = 48
      Width = 185
      Height = 105
      Caption = 'GroupBox1'
      TabOrder = 2
    end
  end
  object ckAutoScale: TCheckBox
    Left = 160
    Top = 220
    Width = 89
    Height = 12
    Hint = 'Automatic display magification adjustment'
    Caption = ' Auto scale'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = ckAutoScaleClick
  end
  object AmplifierGrp: TGroupBox
    Left = 8
    Top = 418
    Width = 145
    Height = 175
    Caption = ' Amplifier '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    object lbAmplifier1: TLabel
      Left = 8
      Top = 15
      Width = 94
      Height = 15
      Caption = 'Amplifier #1 Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object lbAmplifier2: TLabel
      Left = 8
      Top = 53
      Width = 91
      Height = 15
      Caption = 'Amplifier#2 Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object edAmplifier1Gain: TValidatedEdit
      Left = 8
      Top = 30
      Width = 129
      Height = 20
      OnKeyPress = edAmplifier1GainKeyPress
      AutoSize = False
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.6g'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object DivideGrp: TGroupBox
      Left = 8
      Top = 94
      Width = 129
      Height = 73
      Caption = ' Divide Factors '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object Label18: TLabel
        Left = 43
        Top = 24
        Width = 35
        Height = 15
        Alignment = taRightJustify
        Caption = 'Vout 0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label19: TLabel
        Left = 43
        Top = 48
        Width = 35
        Height = 15
        Alignment = taRightJustify
        Caption = 'Vout 1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object edVDivide0: TValidatedEdit
        Left = 80
        Top = 21
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
      object edVDivide1: TValidatedEdit
        Left = 80
        Top = 45
        Width = 41
        Height = 20
        OnKeyPress = edVDivide1KeyPress
        AutoSize = False
        Text = ' 0 X'
        Scale = 1.000000000000000000
        Units = 'X'
        NumberFormat = '%.0f'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
    end
    object edAmplifier2Gain: TValidatedEdit
      Left = 8
      Top = 68
      Width = 129
      Height = 20
      OnKeyPress = edAmplifier2GainKeyPress
      AutoSize = False
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.6g'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 55
    OnTimer = TimerTimer
    Left = 168
    Top = 16
  end
end
