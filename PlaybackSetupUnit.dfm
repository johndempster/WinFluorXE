object PlaybackSetupFrm: TPlaybackSetupFrm
  Left = 873
  Top = 194
  BorderStyle = bsDialog
  Caption = 'Playback Setup'
  ClientHeight = 510
  ClientWidth = 842
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
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelChannel: TLabel
    Left = 8
    Top = 48
    Width = 42
    Height = 13
    Caption = 'Channel:'
  end
  object sdDisplay: TScopeDisplay
    Left = 144
    Top = 8
    Width = 689
    Height = 215
    OnCursorChange = sdDisplayCursorChange
    CursorChangeInProgress = False
    NumChannels = 1
    NumPoints = 1
    MaxPoints = 1024
    XMin = 0
    XMax = 1023
    XOffset = 0
    CursorsEnabled = True
    TScale = 1.000000000000000000
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
    DisplaySelected = True
  end
  object LabelStart: TLabel
    Left = 8
    Top = 96
    Width = 25
    Height = 13
    Caption = 'Start:'
  end
  object LabelStop: TLabel
    Left = 8
    Top = 144
    Width = 25
    Height = 13
    Caption = 'Stop:'
  end
  object Label1: TLabel
    Left = 8
    Top = 192
    Width = 98
    Height = 13
    Caption = 'Start photo-stimulus::'
  end
  object LabelVOut: TLabel
    Left = 8
    Top = 240
    Width = 76
    Height = 13
    Caption = 'Output channel:'
  end
  object lblV0: TLabel
    Left = 8
    Top = 391
    Width = 39
    Height = 13
    Caption = 'V Out 0:'
  end
  object lblStop0: TLabel
    Left = 8
    Top = 367
    Width = 25
    Height = 13
    Caption = 'Stop:'
  end
  object lblStart0: TLabel
    Left = 8
    Top = 343
    Width = 25
    Height = 13
    Caption = 'Start:'
  end
  object lblVOut0: TLabel
    Left = 8
    Top = 320
    Width = 57
    Height = 13
    Caption = 'Voltage out:'
  end
  object lblV1: TLabel
    Left = 8
    Top = 415
    Width = 39
    Height = 13
    Caption = 'V Out 1:'
  end
  object lblV2: TLabel
    Left = 8
    Top = 439
    Width = 39
    Height = 13
    Caption = 'V Out 2:'
  end
  object bCancel: TButton
    Left = 64
    Top = 478
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
    TabOrder = 1
    OnClick = bCancelClick
  end
  object bOK: TButton
    Left = 8
    Top = 478
    Width = 49
    Height = 25
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 0
    OnClick = bOKClick
  end
  object bOpenIDR: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Open IDR...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = bOpenIDRClick
  end
  object cbChannel: TComboBox
    Left = 8
    Top = 64
    Width = 113
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    OnChange = cbChannelChange
  end
  object sbDisplay: TScrollBar
    Left = 144
    Top = 224
    Width = 689
    Height = 17
    PageSize = 0
    TabOrder = 4
    OnChange = sbDisplayChange
  end
  object pnlDisplay: TPanel
    Left = 626
    Top = 250
    Width = 209
    Height = 23
    BevelOuter = bvNone
    TabOrder = 5
    object edDisplay: TValidatedEdit
      Left = 124
      Top = 1
      Width = 65
      Height = 20
      OnKeyPress = edDisplayKeyPress
      AutoSize = False
      Text = ' 0.01667 min'
      Value = 1.000198006629944000
      Scale = 0.016666699200868610
      Units = 'min'
      NumberFormat = '%.4g'
      LoLimit = 1.000000000000000000
      HiLimit = 2000.000000000000000000
    end
    object rbDisplayUnitMins: TRadioButton
      Left = 50
      Top = 1
      Width = 49
      Height = 17
      Caption = 'Mins'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = rbDisplayUnitMinsClick
    end
    object rbDisplayUnitsSecs: TRadioButton
      Left = 1
      Top = 1
      Width = 49
      Height = 17
      Caption = 'Secs'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      TabStop = True
      OnClick = rbDisplayUnitsSecsClick
    end
    object btnDisplayDouble: TButton
      Left = 190
      Top = 2
      Width = 16
      Height = 18
      Caption = '4'
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Webdings'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = btnDisplayDoubleClick
    end
    object btnDisplayHalf: TButton
      Left = 107
      Top = 2
      Width = 16
      Height = 18
      Caption = '3'
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Webdings'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      OnClick = btnDisplayHalfClick
    end
  end
  object edStart: TValidatedEdit
    Left = 8
    Top = 112
    Width = 113
    Height = 20
    OnKeyPress = edStartKeyPress
    AutoSize = False
    Text = ' 1.00000 '
    Value = 1.000000000000000000
    Scale = 1.000000000000000000
    NumberFormat = '%.5f'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object edStop: TValidatedEdit
    Left = 8
    Top = 160
    Width = 113
    Height = 20
    OnKeyPress = edStopKeyPress
    AutoSize = False
    Text = ' 1.00000 '
    Value = 1.000000000000000000
    Scale = 1.000000000000000000
    NumberFormat = '%.5f'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object edStartPS: TValidatedEdit
    Left = 8
    Top = 208
    Width = 113
    Height = 20
    OnKeyPress = edStartPSKeyPress
    AutoSize = False
    Text = ' 1.00000 '
    Value = 1.000000000000000000
    Scale = 1.000000000000000000
    NumberFormat = '%.5f'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object rbVOut0: TRadioButton
    Left = 8
    Top = 256
    Width = 105
    Height = 17
    Caption = 'VOut 0'
    Checked = True
    TabOrder = 9
    TabStop = True
    OnClick = rbVOut0Click
  end
  object rbVOut1: TRadioButton
    Left = 8
    Top = 272
    Width = 105
    Height = 17
    Caption = 'VOut 1'
    TabOrder = 10
    OnClick = rbVOut1Click
  end
  object rbVOut2: TRadioButton
    Left = 8
    Top = 288
    Width = 105
    Height = 17
    Caption = 'VOut 2'
    TabOrder = 11
    OnClick = rbVOut2Click
  end
  object edVOut0: TValidatedEdit
    Left = 56
    Top = 389
    Width = 65
    Height = 20
    OnKeyPress = edStartKeyPress
    AutoSize = False
    Text = ' 0 mV'
    Scale = 1000.000000000000000000
    Units = 'mV'
    NumberFormat = '%.4g'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object edVOutStop: TValidatedEdit
    Left = 56
    Top = 365
    Width = 65
    Height = 20
    OnKeyPress = edVOutStopKeyPress
    AutoSize = False
    Text = ' 0.00000 '
    Scale = 1.000000000000000000
    NumberFormat = '%.5f'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object edVOutStart: TValidatedEdit
    Left = 56
    Top = 341
    Width = 65
    Height = 20
    OnKeyPress = edVOutStartKeyPress
    AutoSize = False
    Text = ' 0.00000 '
    Scale = 1.000000000000000000
    NumberFormat = '%.5f'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object edVOut1: TValidatedEdit
    Left = 56
    Top = 413
    Width = 65
    Height = 20
    OnKeyPress = edStartKeyPress
    AutoSize = False
    Text = ' 0 mV'
    Scale = 1000.000000000000000000
    Units = 'mV'
    NumberFormat = '%.4g'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object edVOut2: TValidatedEdit
    Left = 56
    Top = 437
    Width = 65
    Height = 20
    OnKeyPress = edStartKeyPress
    AutoSize = False
    Text = ' 0 mV'
    Scale = 1000.000000000000000000
    Units = 'mV'
    NumberFormat = '%.4g'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object OpenDialog: TOpenDialog
    Left = 144
    Top = 472
  end
  object IDRFile: TIDRFile
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
    Left = 176
    Top = 472
  end
end
