object ExportROITimeCourseFrm: TExportROITimeCourseFrm
  Left = 681
  Top = 158
  BorderStyle = bsDialog
  Caption = 'Export ROI Time Course'
  ClientHeight = 221
  ClientWidth = 496
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox3: TGroupBox
    Left = 8
    Top = 0
    Width = 481
    Height = 65
    Caption = ' Output file '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object edFileName: TEdit
      Left = 8
      Top = 16
      Width = 465
      Height = 21
      AutoSize = False
      Color = clMenuBar
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      Text = 'edFileName'
    end
    object bChangeName: TButton
      Left = 8
      Top = 40
      Width = 121
      Height = 17
      Hint = 'Change name and/or destination folder of export file'
      Caption = 'Change Destination'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = bChangeNameClick
    end
  end
  object GroupBox8: TGroupBox
    Left = 8
    Top = 68
    Width = 129
    Height = 90
    Caption = ' Frames '
    TabOrder = 1
    object rbAllRecords: TRadioButton
      Left = 8
      Top = 16
      Width = 81
      Height = 18
      Hint = 'Export ROIs from all frame'
      Caption = 'All Frames'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TabStop = True
      OnClick = rbAllRecordsClick
    end
    object rbRange: TRadioButton
      Left = 8
      Top = 32
      Width = 57
      Height = 17
      Hint = 'Export selected range of frames'
      Caption = 'Range'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = rbRangeClick
    end
    object edRange: TRangeEdit
      Left = 24
      Top = 48
      Width = 97
      Height = 20
      OnKeyPress = edRangeKeyPress
      AutoSize = False
      Text = ' 1 - 1E030 '
      LoValue = 1.000000000000000000
      HiValue = 1.000000015047466E30
      LoLimit = 1.000000000000000000
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      NumberFormat = '%.5g - %.5g'
    end
  end
  object GroupBox2: TGroupBox
    Left = 392
    Top = 68
    Width = 97
    Height = 149
    Caption = '  Format '
    TabOrder = 2
    OnClick = GroupBox2Click
    object rbABF: TRadioButton
      Left = 8
      Top = 16
      Width = 57
      Height = 17
      Hint = 'Export to Axon ABF file'
      Caption = 'Axon'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      TabStop = True
      OnClick = rbABFClick
    end
    object rbASCII: TRadioButton
      Left = 8
      Top = 31
      Width = 57
      Height = 17
      Hint = 'Export to ASCII text file (columns separated by tabs)'
      Caption = 'Text'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = rbASCIIClick
    end
    object rbEDR: TRadioButton
      Left = 8
      Top = 46
      Width = 57
      Height = 17
      Hint = 'Export to Strathclyde Electrophysiology Software EDR data file'
      Caption = 'EDR'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = rbEDRClick
    end
    object rbMAT: TRadioButton
      Left = 8
      Top = 93
      Width = 57
      Height = 17
      Caption = 'MAT'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsItalic]
      ParentFont = False
      TabOrder = 3
    end
    object rbIBW: TRadioButton
      Left = 8
      Top = 63
      Width = 81
      Height = 15
      Hint = 'Export to IGOR Wave format file'
      Caption = 'Igor Wave'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = rbEDRClick
    end
    object rbCFS: TRadioButton
      Left = 8
      Top = 78
      Width = 65
      Height = 17
      Caption = 'CFS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = rbEDRClick
    end
  end
  object bOK: TButton
    Left = 8
    Top = 162
    Width = 50
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
    Top = 162
    Width = 50
    Height = 17
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 4
  end
  object FluorGrp: TGroupBox
    Left = 144
    Top = 68
    Width = 113
    Height = 149
    Caption = ' ROI '
    TabOrder = 5
    object ROIGrp: TGroupBox
      Left = 8
      Top = 80
      Width = 97
      Height = 45
      Caption = ' Subtract ROI '
      TabOrder = 0
      object Label2: TLabel
        Left = 6
        Top = 15
        Width = 11
        Height = 22
        Alignment = taRightJustify
        Caption = '-'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object cbSubROI: TComboBox
        Left = 24
        Top = 16
        Width = 65
        Height = 21
        Hint = 'Subtract this ROI from other ROI signals'
        ItemHeight = 13
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = 'cbROI'
        OnChange = cbSubROIChange
      end
    end
    object rbAllROIs: TRadioButton
      Left = 8
      Top = 16
      Width = 73
      Height = 17
      Hint = 'Export all ROIs (except subtraction ROI)'
      Caption = 'All'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = rbAllROIsClick
    end
    object RadioButton2: TRadioButton
      Left = 8
      Top = 32
      Width = 73
      Height = 17
      Hint = 'Export a single ROI, selected from the list below'
      Caption = 'ROI'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      TabStop = True
      OnClick = RadioButton2Click
    end
    object cbROI: TComboBox
      Left = 24
      Top = 48
      Width = 65
      Height = 21
      Hint = 'ROI to be exported'
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'cbROI'
      OnChange = cbROIChange
    end
  end
  object RatioGrp: TGroupBox
    Left = 264
    Top = 67
    Width = 121
    Height = 150
    Caption = ' Wavelength'
    TabOrder = 6
    object Shape2: TShape
      Left = 30
      Top = 87
      Width = 81
      Height = 2
    end
    object Label5: TLabel
      Left = 8
      Top = 114
      Width = 57
      Height = 28
      Alignment = taRightJustify
      Caption = 'Excl. Threshold'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object cbDenominator: TComboBox
      Left = 28
      Top = 92
      Width = 85
      Height = 21
      Hint = 'Wavelength channel in denominator of ratio'
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'cbROI'
      OnChange = cbDenominatorChange
    end
    object cbNumerator: TComboBox
      Left = 28
      Top = 62
      Width = 85
      Height = 21
      Hint = 'Wavelength channel in numerator of ratio'
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'cbROI'
      OnChange = cbNumeratorChange
    end
    object rbExportROI: TRadioButton
      Left = 8
      Top = 16
      Width = 17
      Height = 17
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      TabStop = True
      OnClick = rbExportROIClick
    end
    object rbExportRatio: TRadioButton
      Left = 8
      Top = 44
      Width = 73
      Height = 17
      Hint = 'Export wavelength channel ratio'
      Caption = 'Ratio'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = rbExportRatioClick
    end
    object cbFrameType: TComboBox
      Left = 28
      Top = 18
      Width = 85
      Height = 21
      Hint = 'Fluorescence wavelength channel to be exported'
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = 'cbROI'
      OnChange = cbFrameTypeChange
    end
    object edExclusionThreshold: TValidatedEdit
      Left = 72
      Top = 120
      Width = 41
      Height = 21
      Hint = 
        'Exclusion Threshold: Ratio set to zero if fluorescence signal fa' +
        'lls below this threshold.'
      Text = ' 1 '
      Value = 1.000000000000000000
      Scale = 1.000000000000000000
      NumberFormat = '%.4g'
      LoLimit = 1.000000000000000000
      HiLimit = 1.000000015047466E30
    end
  end
  object ExportFile: TADCDataFile
    NumChannelsPerScan = 1
    NumBytesPerSample = 2
    NumScansPerRecord = 512
    FloatingPointSamples = False
    MaxADCValue = 2047
    MinADCValue = -2048
    RecordNum = 0
    NumFileHeaderBytes = 0
    WCPNumZeroAvg = 0
    WCPRecordAccepted = False
    ABFAcquisitionMode = ftGapFree
    EDREventDetectorChannel = 0
    EDREventDetectorRecordSize = 0
    EDRVarianceRecordSize = 0
    EDRVarianceRecordOverlap = 0
    EDRBackedUp = False
    ASCIISeparator = #9
    ASCIITimeDataInCol0 = False
    ASCIITimeUnits = 's'
    ASCIITitleLines = 2
    ASCIIFixedRecordSize = False
    Left = 16
    Top = 184
  end
  object SaveDialog: TSaveDialog
    Left = 88
    Top = 184
  end
end
