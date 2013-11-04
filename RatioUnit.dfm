object RatioFrm: TRatioFrm
  Left = 626
  Top = 349
  Width = 287
  Height = 302
  Caption = 'Create Ratio/Ion Conc. Image'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 8
    Top = 4
    Width = 121
    Height = 85
    Caption = ' Range '
    TabOrder = 0
    object rbAllFrames: TRadioButton
      Left = 8
      Top = 16
      Width = 89
      Height = 17
      Hint = 'Use all frames in file'
      Caption = 'All Frames'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton1: TRadioButton
      Left = 8
      Top = 32
      Width = 57
      Height = 17
      Hint = 'Use a sub-range of  frames'
      Caption = 'Range'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
    object edRange: TRangeEdit
      Left = 26
      Top = 52
      Width = 65
      Height = 20
      Hint = 'Range of frames to be  processed'
      AutoSize = False
      ShowHint = True
      Text = ' 1 - 1 '
      LoValue = 1.000000000000000000
      HiValue = 1.000000000000000000
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      NumberFormat = '%.0f - %.0f'
    end
  end
  object GroupBox4: TGroupBox
    Left = 136
    Top = 4
    Width = 137
    Height = 269
    TabOrder = 1
    object lbDeltaFPlot: THTMLLabel
      Left = 24
      Top = 16
      Width = 33
      Height = 17
      Hint = 'Create intensity as % df/F0 ratio images'
      Caption = '<font face=symbol>D</font>F/F0'
      Alignment = taCenter
      LineSpacing = 1.500000000000000000
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
    end
    object rbDeltaF: TRadioButton
      Left = 8
      Top = 16
      Width = 17
      Height = 17
      Hint = 'Plot intensity from a single frame type'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
      OnClick = rbDeltaFClick
    end
    object rbFrameRatio: TRadioButton
      Left = 64
      Top = 16
      Width = 49
      Height = 17
      Hint = 'Create intensity ratio or computed concentration images'
      Caption = 'Ratio'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = rbFrameRatioClick
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 180
      Width = 121
      Height = 79
      Caption = ' Backg. Subtraction '
      TabOrder = 2
      object Label3: TLabel
        Left = 32
        Top = 16
        Width = 19
        Height = 13
        Caption = 'ROI'
      end
      object Label4: TLabel
        Left = 16
        Top = 42
        Width = 47
        Height = 26
        Alignment = taRightJustify
        Caption = 'Inclusion Threshold'
        WordWrap = True
      end
      object cbBackgroundROI: TComboBox
        Left = 56
        Top = 16
        Width = 57
        Height = 21
        Hint = 'Region of interest used to compute background intensity level'
        Style = csDropDownList
        ItemHeight = 13
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object edInclusionThreshold: TValidatedEdit
        Left = 72
        Top = 46
        Width = 41
        Height = 20
        Hint = 
          'Minimum intensity (after backg. subtraction) for inclusion of pi' +
          'xel in image'
        AutoSize = False
        ShowHint = True
        Text = ' 0 '
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
    end
    object RatioGrp: TGroupBox
      Left = 8
      Top = 36
      Width = 121
      Height = 129
      TabOrder = 3
      object Shape1: TShape
        Left = 22
        Top = 48
        Width = 76
        Height = 2
      end
      object Label2: TLabel
        Left = 24
        Top = 8
        Width = 52
        Height = 13
        Caption = 'Frame type'
      end
      object cbNumerator: TComboBox
        Left = 24
        Top = 24
        Width = 73
        Height = 21
        Hint = 'Numerator frame type used in ratio image'
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
      object cbDenominator: TComboBox
        Left = 24
        Top = 54
        Width = 73
        Height = 21
        Hint = 'Denominator frame type'
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
      end
      object cbEquation: TComboBox
        Left = 24
        Top = 100
        Width = 73
        Height = 21
        Hint = 'Binding equation used to compute ion concentration from ratio'
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
      end
      object ckUseEquation: TCheckBox
        Left = 8
        Top = 82
        Width = 105
        Height = 17
        Hint = 'Plot ion concenration computed from ratio using binding equation'
        Caption = 'Ion Binding Eqn.'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
      end
    end
    object DeltaFGrp: TGroupBox
      Left = 8
      Top = 36
      Width = 121
      Height = 141
      TabOrder = 4
      object Label1: TLabel
        Left = 6
        Top = 8
        Width = 52
        Height = 13
        Caption = 'Frame type'
      end
      object cbFrameType: TComboBox
        Left = 6
        Top = 24
        Width = 107
        Height = 21
        Hint = 'Frame type to be '
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
      object GroupBox7: TGroupBox
        Left = 8
        Top = 46
        Width = 105
        Height = 85
        Caption = ' F0 '
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 1
        object rbF0FromFrames: TRadioButton
          Left = 8
          Top = 14
          Width = 73
          Height = 17
          Hint = 'Compute F0 from series of frames'
          Caption = 'F0 frames'
          Checked = True
          Color = clBtnFace
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          TabOrder = 0
          TabStop = True
        end
        object edF0Range: TRangeEdit
          Left = 24
          Top = 30
          Width = 73
          Height = 20
          Hint = 'Series of frames from which F0 is computed'
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ShowHint = True
          Text = ' 1 - 1 '
          LoValue = 1.000000000000000000
          HiValue = 1.000000000000000000
          LoLimit = 1.000000000000000000
          HiLimit = 1.000000015047466E30
          Scale = 1.000000000000000000
          NumberFormat = '%.f - %.f'
        end
        object rbF0Constant: TRadioButton
          Left = 8
          Top = 54
          Width = 49
          Height = 17
          Hint = 'Use constant F0 value'
          Caption = 'F0 ='
          Color = clBtnFace
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          TabOrder = 2
        end
        object edF0Constant: TValidatedEdit
          Left = 56
          Top = 54
          Width = 41
          Height = 20
          Hint = 'Constant F0 value '
          AutoSize = False
          ShowHint = True
          Text = ' 0.0 '
          Scale = 1.000000000000000000
          NumberFormat = '%.1f'
          LoLimit = -1.000000015047466E29
          HiLimit = 1000.000000000000000000
        end
      end
    end
  end
  object bOK: TButton
    Left = 8
    Top = 230
    Width = 57
    Height = 25
    Hint = 'Create computed images'
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 72
    Top = 230
    Width = 57
    Height = 17
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = bCancelClick
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 96
    Width = 121
    Height = 129
    Caption = ' Output Range '
    TabOrder = 4
    object Label5: TLabel
      Left = 19
      Top = 18
      Width = 20
      Height = 13
      Alignment = taRightJustify
      Caption = 'Min.'
      WordWrap = True
    end
    object Label6: TLabel
      Left = 16
      Top = 42
      Width = 23
      Height = 13
      Alignment = taRightJustify
      Caption = 'Max,'
      WordWrap = True
    end
    object edLowerLimit: TValidatedEdit
      Left = 48
      Top = 18
      Width = 65
      Height = 20
      Hint = 'Computed value mapped to zero intensity within computed image'
      AutoSize = False
      ShowHint = True
      Text = '    0 '
      Scale = 1.000000000000000000
      NumberFormat = '%4g'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
    object edUpperLimit: TValidatedEdit
      Left = 48
      Top = 42
      Width = 65
      Height = 20
      Hint = 'Computed value mapped to maximum intensity within computed image'
      AutoSize = False
      ShowHint = True
      Text = '    0 '
      Scale = 1.000000000000000000
      NumberFormat = '%4g'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
  end
  object IDROut: TIDRFile
    AsyncWriteBufSize = 0
    NumFrames = 0
    NumFrameTypes = 1
    FrameWidth = 0
    FrameHeight = 0
    PixelDepth = 1
    IntensityScale = 1.000000000000000000
    XResolution = 1.000000000000000000
    ADCNumScansInFile = 0
    ADCNumChannels = 1
    ADCNumScansPerFrame = 0
    ADCMaxValue = 0
    LineScan = False
    LSTimeCoursePixel = 0
    LSTimeCourseNumAvg = 1
    LSTimeCourseBackgroundPixel = 0
    LSSubtractBackground = False
    LineScanIntervalCorrectionFactor = 1.000000000000000000
    WriteEnabled = False
    SpectralDataFile = False
    EventDisplayDuration = 1.000000000000000000
    EventDeadTime = 1.000000000000000000
    EventDetectionThreshold = 1000.000000000000000000
    EventDetectionThresholdPolarity = 0
    EventDetectionSource = 0
    EventROI = 0
    EventBackgROI = 0
    EventFixedBaseline = True
    EventRollingBaselinePeriod = 1.000000000000000000
    EventBaselineLevel = 0
    EventRatioExclusionThreshold = 0
    EventRatioTop = 0
    EventRatioBottom = 1
    EventRatioDisplayMax = 10.000000000000000000
    EventRatioRMax = 1.000000000000000000
    EventFLWave = 0
    EventF0Wave = 0
    EventF0Start = 1
    EventF0End = 1
    EventF0UseConstant = False
    EventF0DisplayMax = 10.000000000000000000
    EventF0SubtractF0 = False
    Left = 96
    Top = 208
  end
end
