object ExportAnalogueFrm: TExportAnalogueFrm
  Tag = 12
  Left = 195
  Top = 387
  BorderStyle = bsDialog
  Caption = ' Export Analogue Signals'
  ClientHeight = 252
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object GroupBox8: TGroupBox
    Left = 8
    Top = 130
    Width = 129
    Height = 90
    Caption = ' Range '
    TabOrder = 0
    object rbAllRecords: TRadioButton
      Left = 8
      Top = 16
      Width = 81
      Height = 18
      Hint = 'Analysis all record in the data file'
      Caption = 'Whole file'
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
    end
    object rbRange: TRadioButton
      Left = 8
      Top = 32
      Width = 57
      Height = 17
      Hint = 'Analysis a limited range of records within the data file'
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
    end
    object edRange: TRangeEdit
      Left = 24
      Top = 48
      Width = 97
      Height = 20
      AutoSize = False
      Text = ' 0 - 1E030 s '
      HiValue = 1.000000015047466E30
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      NumberFormat = '%.5g - %.5g s'
    end
  end
  object ChannelsGrp: TGroupBox
    Left = 144
    Top = 130
    Width = 225
    Height = 101
    Caption = ' Channels '
    TabOrder = 1
    object ckCh0: TCheckBox
      Left = 8
      Top = 16
      Width = 81
      Height = 17
      Caption = 'ckCh0'
      TabOrder = 0
      OnClick = ckCh0Click
    end
    object ckCh1: TCheckBox
      Tag = 1
      Left = 8
      Top = 32
      Width = 81
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 1
      OnClick = ckCh0Click
    end
    object ckCh2: TCheckBox
      Tag = 2
      Left = 8
      Top = 48
      Width = 65
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 2
      OnClick = ckCh0Click
    end
    object ckCh3: TCheckBox
      Tag = 3
      Left = 8
      Top = 64
      Width = 81
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 3
      OnClick = ckCh0Click
    end
    object ckCh4: TCheckBox
      Tag = 4
      Left = 112
      Top = 16
      Width = 89
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 4
      OnClick = ckCh0Click
    end
    object ckCh5: TCheckBox
      Tag = 5
      Left = 112
      Top = 32
      Width = 81
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 5
      OnClick = ckCh0Click
    end
    object ckCh6: TCheckBox
      Tag = 6
      Left = 112
      Top = 48
      Width = 81
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 6
      OnClick = ckCh0Click
    end
    object ckCh7: TCheckBox
      Tag = 7
      Left = 112
      Top = 64
      Width = 81
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 7
      OnClick = ckCh0Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 376
    Top = 130
    Width = 97
    Height = 101
    Caption = ' Format '
    TabOrder = 2
    object rbABF: TRadioButton
      Left = 8
      Top = 16
      Width = 57
      Height = 17
      Caption = 'Axon'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
    end
    object rbASCII: TRadioButton
      Left = 8
      Top = 31
      Width = 57
      Height = 17
      Caption = 'Text'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
    object rbEDR: TRadioButton
      Left = 8
      Top = 46
      Width = 57
      Height = 17
      Caption = 'EDR'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
    end
    object rbMAT: TRadioButton
      Left = 8
      Top = 77
      Width = 57
      Height = 17
      Caption = 'MAT'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
    end
    object rbIBW: TRadioButton
      Left = 8
      Top = 63
      Width = 81
      Height = 15
      Caption = 'Igor Wave'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
    end
  end
  object bOK: TButton
    Left = 8
    Top = 224
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
    Top = 224
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
    OnClick = bCancelClick
  end
  object FilesToExportGrp: TGroupBox
    Left = 8
    Top = 8
    Width = 529
    Height = 120
    Caption = ' File to be Exported '
    TabOrder = 5
    object lbExportDirectory: TLabel
      Left = 229
      Top = 98
      Width = 30
      Height = 14
      Caption = 'xxxxx'
    end
    object bSelectFilesToExport: TButton
      Left = 9
      Top = 78
      Width = 137
      Height = 17
      Caption = 'Select Files to Export'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = bSelectFilesToExportClick
    end
    object meFiles: TMemo
      Left = 8
      Top = 18
      Width = 511
      Height = 54
      Lines.Strings = (
        'Memo1')
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
    end
    object bClearList: TButton
      Left = 152
      Top = 78
      Width = 70
      Height = 17
      Caption = 'Clear List'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
    end
    object bSelectDestination: TButton
      Left = 229
      Top = 78
      Width = 125
      Height = 17
      Hint = 'Select folder to hold exported files'
      Caption = 'Select Export Folder'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = bSelectDestinationClick
    end
  end
  object SaveDialog: TSaveDialog
    Left = 192
    Top = 208
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
    ASCIISaveRecordsinColumns = False
    Left = 128
    Top = 208
  end
  object OpenDialog: TOpenDialog
    Left = 104
    Top = 240
  end
end
