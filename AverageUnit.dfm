object AverageFrm: TAverageFrm
  Left = 187
  Top = 222
  BorderStyle = bsDialog
  Caption = 'Average/Subtract Frames'
  ClientHeight = 176
  ClientWidth = 309
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
    Top = 2
    Width = 121
    Height = 85
    Caption = ' Range '
    TabOrder = 0
    object rbAllFrames: TRadioButton
      Left = 8
      Top = 16
      Width = 89
      Height = 17
      Caption = 'All Frames'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
    end
    object RadioButton1: TRadioButton
      Left = 8
      Top = 33
      Width = 57
      Height = 17
      Caption = 'Range'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
    object edRange: TRangeEdit
      Left = 24
      Top = 50
      Width = 81
      Height = 20
      AutoSize = False
      Text = ' 1 - 1 '
      LoValue = 1.000000000000000000
      HiValue = 1.000000000000000000
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      NumberFormat = '%.0f - %.0f'
    end
  end
  object bOK: TButton
    Left = 8
    Top = 96
    Width = 70
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
    Left = 8
    Top = 120
    Width = 57
    Height = 17
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = bCancelClick
  end
  object GroupBox3: TGroupBox
    Left = 136
    Top = 2
    Width = 169
    Height = 167
    TabOrder = 3
    object GroupBox1: TGroupBox
      Left = 8
      Top = 10
      Width = 153
      Height = 103
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 66
        Width = 85
        Height = 15
        Alignment = taRightJustify
        Caption = 'No. frames'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object rbAverage: TRadioButton
        Left = 8
        Top = 12
        Width = 113
        Height = 17
        Hint = 'Average series of adjacent frame'
        Caption = 'Average Frames'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        TabStop = True
      end
      object rbSkip: TRadioButton
        Left = 8
        Top = 47
        Width = 121
        Height = 17
        Hint = 'Create a file with fewer frames by skipping one or more frames'
        Caption = 'Skip Frames'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object edNumAverage: TValidatedEdit
        Left = 104
        Top = 66
        Width = 41
        Height = 21
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = 1.000000000000000000
        HiLimit = 1.000000015047466E29
      end
      object rbAdd: TRadioButton
        Left = 8
        Top = 29
        Width = 97
        Height = 17
        Hint = 'Summate series of adjacent frames'
        Caption = 'Sum Frames'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
    end
    object GroupBox4: TGroupBox
      Left = 8
      Top = 114
      Width = 153
      Height = 41
      TabOrder = 1
      object ckFrameDifferences: TRadioButton
        Left = 8
        Top = 16
        Width = 137
        Height = 17
        Hint = 
          'Create a file of pixel intensity differences between successive ' +
          'frames'
        Caption = 'Frame Differences'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
  end
  object IDRAvg: TIDRFile
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
    LineScanIntervalCorrectionFactor = 1.000000000000000000
    WriteEnabled = False
    SpectralDataFile = False
    EventDisplayDuration = 1.000000000000000000
    Left = 72
    Top = 144
  end
end
