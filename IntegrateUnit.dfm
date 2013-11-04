object IntegrateFrm: TIntegrateFrm
  Left = 820
  Top = 267
  Width = 577
  Height = 603
  Caption = 'Average/Integrated Image'
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ImageGrp: TGroupBox
    Left = 160
    Top = 0
    Width = 401
    Height = 265
    Caption = ' Images '
    TabOrder = 0
    object Image: TImage
      Left = 8
      Top = 16
      Width = 369
      Height = 233
    end
  end
  object ControlsGrp: TGroupBox
    Left = 8
    Top = 0
    Width = 145
    Height = 553
    TabOrder = 1
    object GroupBox2: TGroupBox
      Left = 8
      Top = 246
      Width = 129
      Height = 106
      Caption = ' Capture Region '
      TabOrder = 0
      object Label2: TLabel
        Left = 26
        Top = 16
        Width = 7
        Height = 13
        Caption = 'X'
      end
      object Label3: TLabel
        Left = 26
        Top = 40
        Width = 7
        Height = 13
        Caption = 'Y'
      end
      object Label4: TLabel
        Left = 50
        Top = 64
        Width = 35
        Height = 13
        Caption = 'Binning'
      end
      object lbImageSize: TLabel
        Left = 64
        Top = 86
        Width = 57
        Height = 13
        Alignment = taRightJustify
        Caption = 'lbImageSize'
      end
      object erXRange: TRangeEdit
        Left = 40
        Top = 16
        Width = 81
        Height = 20
        OnKeyPress = erXRangeKeyPress
        AutoSize = False
        Text = ' 0 - 511 '
        HiValue = 511.000000000000000000
        HiLimit = 1.000000015047466E30
        Scale = 1.000000000000000000
        NumberFormat = '%.0f - %.0f'
      end
      object erYRange: TRangeEdit
        Left = 40
        Top = 40
        Width = 81
        Height = 20
        OnKeyPress = erXRangeKeyPress
        AutoSize = False
        Text = ' 0 - 511 '
        HiValue = 511.000000000000000000
        HiLimit = 1.000000015047466E30
        Scale = 1.000000000000000000
        NumberFormat = '%.0f - %.0f'
      end
      object edBinFactor: TValidatedEdit
        Left = 88
        Top = 64
        Width = 33
        Height = 20
        OnKeyPress = erXRangeKeyPress
        AutoSize = False
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%g'
        LoLimit = 1.000000000000000000
        HiLimit = 64.000000000000000000
      end
    end
    object GroupBox3: TGroupBox
      Left = 8
      Top = 156
      Width = 129
      Height = 89
      Caption = ' Capture Interval '
      TabOrder = 1
      object lbReadoutTime: TLabel
        Left = 48
        Top = 38
        Width = 72
        Height = 13
        Alignment = taRightJustify
        Caption = 'lbReadoutTime'
      end
      object Label6: TLabel
        Left = 26
        Top = 54
        Width = 58
        Height = 28
        Alignment = taRightJustify
        Caption = 'No. Frames  in Average'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object edFrameInterval: TValidatedEdit
        Left = 56
        Top = 16
        Width = 65
        Height = 20
        OnKeyPress = edFrameIntervalKeyPress
        AutoSize = False
        Text = ' 0 ms'
        Scale = 1000.000000000000000000
        Units = 'ms'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
      object edNumFramesToAverage: TValidatedEdit
        Left = 88
        Top = 54
        Width = 33
        Height = 20
        OnKeyPress = edNumFramesRequiredKeyPress
        AutoSize = False
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = 1.000000000000000000
        HiLimit = 1.000000015047466E30
      end
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 354
      Width = 129
      Height = 111
      Caption = ' Display '
      TabOrder = 2
      object Label5: TLabel
        Left = 8
        Top = 56
        Width = 32
        Height = 13
        Caption = 'Range'
      end
      object bFullScale: TButton
        Left = 8
        Top = 14
        Width = 95
        Height = 17
        Caption = 'Full Scale'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = bFullScaleClick
      end
      object bMaxContrast: TButton
        Left = 8
        Top = 34
        Width = 95
        Height = 17
        Caption = 'Max. Contrast'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bMaxContrastClick
      end
      object edDisplayIntensityRange: TRangeEdit
        Left = 48
        Top = 56
        Width = 73
        Height = 20
        OnKeyPress = edDisplayIntensityRangeKeyPress
        AutoSize = False
        Text = ' 4096 - 4096 '
        LoValue = 4096.000000000000000000
        HiValue = 4096.000000000000000000
        HiLimit = 1.000000015047466E30
        Scale = 1.000000000000000000
        NumberFormat = '%.f - %.f'
      end
      object cbPalette: TComboBox
        Left = 8
        Top = 82
        Width = 113
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
        OnChange = cbPaletteChange
      end
    end
    object GroupBox5: TGroupBox
      Left = 8
      Top = 8
      Width = 129
      Height = 41
      Caption = ' Single Image '
      TabOrder = 3
      object rbSingle: TRadioButton
        Left = 8
        Top = 16
        Width = 25
        Height = 17
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        TabStop = True
        OnClick = rbSingleClick
      end
      object bStart: TButton
        Left = 40
        Top = 18
        Width = 81
        Height = 17
        Caption = 'Start/Restart'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bStartClick
      end
    end
    object GroupBox6: TGroupBox
      Left = 8
      Top = 48
      Width = 129
      Height = 77
      Caption = ' Continuous  '
      TabOrder = 4
      object Label1: TLabel
        Left = 14
        Top = 38
        Width = 55
        Height = 14
        Caption = 'No. Frames'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lbDuration: TLabel
        Left = 66
        Top = 58
        Width = 55
        Height = 14
        Alignment = taRightJustify
        Caption = 'No. Frames'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object edNumFramesRequired: TValidatedEdit
        Left = 72
        Top = 38
        Width = 49
        Height = 20
        OnKeyPress = edNumFramesRequiredKeyPress
        AutoSize = False
        Text = ' 100 '
        Value = 100.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object rbSequence: TRadioButton
        Left = 8
        Top = 16
        Width = 25
        Height = 17
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = rbSequenceClick
      end
      object bStop: TButton
        Left = 88
        Top = 14
        Width = 35
        Height = 17
        Caption = 'Stop'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = bStopClick
      end
      object bRecord: TButton
        Left = 36
        Top = 14
        Width = 47
        Height = 17
        Caption = 'Record'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnClick = bRecordClick
      end
    end
    object GroupBox7: TGroupBox
      Left = 8
      Top = 124
      Width = 129
      Height = 30
      TabOrder = 5
      object rbIntegrate: TRadioButton
        Left = 8
        Top = 8
        Width = 73
        Height = 17
        Caption = 'Integrate'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object rbAverage: TRadioButton
        Left = 80
        Top = 8
        Width = 41
        Height = 17
        Caption = 'Avg.'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        TabStop = True
      end
    end
    object GroupBox8: TGroupBox
      Left = 8
      Top = 488
      Width = 129
      Height = 57
      Caption = ' Backg. Subtraction'
      TabOrder = 6
      object bLoadBackground: TButton
        Left = 8
        Top = 16
        Width = 73
        Height = 17
        Caption = 'Load Image'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = bLoadBackgroundClick
      end
      object ckSubtractBackground: TCheckBox
        Left = 8
        Top = 36
        Width = 73
        Height = 17
        Caption = 'Enabled'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = ckSubtractBackgroundClick
      end
    end
  end
  object Timer: TTimer
    Interval = 55
    OnTimer = TimerTimer
    Left = 168
    Top = 448
  end
end
