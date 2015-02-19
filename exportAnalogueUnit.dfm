object ExportAnalogueFrm: TExportAnalogueFrm
  Left = 195
  Top = 387
  BorderStyle = bsDialog
  Caption = ' Export Analogue Signals'
  ClientHeight = 187
  ClientWidth = 481
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
    Top = 68
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
    Top = 68
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
  object GroupBox3: TGroupBox
    Left = 8
    Top = 0
    Width = 465
    Height = 65
    Caption = ' Output file '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object edFileName: TEdit
      Left = 8
      Top = 16
      Width = 449
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
      Caption = 'Change Destination'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = bChangeNameClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 376
    Top = 68
    Width = 97
    Height = 101
    Caption = ' Format '
    TabOrder = 3
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
      OnClick = rbABFClick
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
      OnClick = rbABFClick
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
      OnClick = rbABFClick
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
      OnClick = rbABFClick
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
      OnClick = rbABFClick
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
    TabOrder = 4
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
    TabOrder = 5
    OnClick = bCancelClick
  end
  object SaveDialog: TSaveDialog
    Left = 168
    Top = 152
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
    Left = 128
    Top = 152
  end
end
