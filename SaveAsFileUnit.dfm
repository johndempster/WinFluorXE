object SaveAsFileFrm: TSaveAsFileFrm
  Left = 606
  Top = 313
  BorderStyle = bsDialog
  Caption = 'Save As Data File'
  ClientHeight = 247
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object GroupBox1: TGroupBox
    Left = 8
    Top = 0
    Width = 451
    Height = 65
    Caption = ' Save as File Name '
    TabOrder = 0
    object edFileName: TEdit
      Left = 8
      Top = 16
      Width = 433
      Height = 22
      ReadOnly = True
      TabOrder = 0
      Text = 'edFileName'
    end
    object bChangeName: TButton
      Left = 8
      Top = 40
      Width = 105
      Height = 17
      Caption = 'Change Name'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = bChangeNameClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 72
    Width = 117
    Height = 105
    Caption = ' Range '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 6
      Top = 80
      Width = 59
      Height = 14
      Alignment = taRightJustify
      Caption = 'Skip Frames'
    end
    object Shape1: TShape
      Left = 8
      Top = 74
      Width = 97
      Height = 1
    end
    object rbAllFrames: TRadioButton
      Left = 8
      Top = 16
      Width = 89
      Height = 17
      Caption = 'All Frames'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
      OnClick = rbAllFramesClick
    end
    object rbRange: TRadioButton
      Left = 8
      Top = 32
      Width = 57
      Height = 17
      Caption = 'Range'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = rbRangeClick
    end
    object edRange: TRangeEdit
      Left = 28
      Top = 48
      Width = 77
      Height = 20
      OnKeyPress = edRangeKeyPress
      AutoSize = False
      Text = ' 1 - 1 '
      LoValue = 1.000000000000000000
      HiValue = 1.000000000000000000
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      NumberFormat = '%.0f - %.0f'
    end
    object edSkip: TValidatedEdit
      Left = 72
      Top = 80
      Width = 33
      Height = 20
      OnKeyPress = edSkipKeyPress
      AutoSize = False
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
  end
  object GroupBox6: TGroupBox
    Left = 133
    Top = 72
    Width = 160
    Height = 169
    Caption = ' Frame Types '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object ckFrameType0: TCheckBox
      Left = 8
      Top = 16
      Width = 81
      Height = 17
      Caption = 'ckFrameType0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = ckFrameType0Click
    end
    object ckFrameType1: TCheckBox
      Left = 8
      Top = 32
      Width = 81
      Height = 17
      Caption = 'ckADC0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = ckFrameType0Click
    end
    object ckFrameType2: TCheckBox
      Left = 8
      Top = 48
      Width = 81
      Height = 17
      Caption = 'ckADC0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = ckFrameType0Click
    end
    object ckFrameType3: TCheckBox
      Left = 8
      Top = 64
      Width = 81
      Height = 17
      Caption = 'ckADC0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = ckFrameType0Click
    end
    object ckFrameType4: TCheckBox
      Left = 8
      Top = 80
      Width = 81
      Height = 17
      Caption = 'ckFrameType0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = ckFrameType0Click
    end
    object ckFrameType5: TCheckBox
      Left = 8
      Top = 96
      Width = 81
      Height = 17
      Caption = 'ckADC0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = ckFrameType0Click
    end
    object ckFrameType6: TCheckBox
      Left = 8
      Top = 112
      Width = 81
      Height = 17
      Caption = 'ckADC0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnClick = ckFrameType0Click
    end
    object ckFrameType7: TCheckBox
      Left = 8
      Top = 128
      Width = 81
      Height = 17
      Caption = 'ckADC0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnClick = ckFrameType0Click
    end
    object ckFrameType8: TCheckBox
      Left = 8
      Top = 144
      Width = 81
      Height = 17
      Caption = 'ckADC0'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      OnClick = ckFrameType0Click
    end
  end
  object bCancel: TButton
    Left = 72
    Top = 184
    Width = 49
    Height = 17
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 3
  end
  object bOK: TButton
    Left = 8
    Top = 184
    Width = 57
    Height = 25
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 4
    OnClick = bOKClick
  end
  object ChannelsGrp: TGroupBox
    Left = 300
    Top = 72
    Width = 160
    Height = 169
    Caption = ' Channels '
    TabOrder = 5
    object ckCh0: TCheckBox
      Left = 8
      Top = 16
      Width = 81
      Height = 17
      Caption = 'ckCh0'
      TabOrder = 0
    end
    object ckCh1: TCheckBox
      Left = 8
      Top = 32
      Width = 81
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 1
    end
    object ckCh2: TCheckBox
      Left = 8
      Top = 48
      Width = 81
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 2
    end
    object ckCh3: TCheckBox
      Left = 8
      Top = 64
      Width = 81
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 3
    end
    object ckCh4: TCheckBox
      Left = 8
      Top = 80
      Width = 89
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 4
    end
    object ckCh5: TCheckBox
      Left = 8
      Top = 96
      Width = 81
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 5
    end
    object ckCh6: TCheckBox
      Left = 8
      Top = 112
      Width = 81
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 6
    end
    object ckCh7: TCheckBox
      Left = 8
      Top = 128
      Width = 81
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 7
    end
  end
  object SaveDialog: TSaveDialog
    Left = 272
    Top = 424
  end
  object SaveIDRFile: TIDRFile
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
    Left = 88
    Top = 216
  end
end
