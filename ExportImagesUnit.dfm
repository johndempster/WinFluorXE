object ExportImagesFrm: TExportImagesFrm
  Left = 271
  Top = 177
  BorderStyle = bsDialog
  Caption = 'Export Images'
  ClientHeight = 246
  ClientWidth = 544
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 529
    Height = 65
    Caption = ' Export File Name '
    TabOrder = 0
    object edFileName: TEdit
      Left = 8
      Top = 16
      Width = 513
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
    Top = 74
    Width = 117
    Height = 121
    Caption = ' Frames '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 23
      Top = 80
      Width = 42
      Height = 28
      Alignment = taRightJustify
      Caption = 'Skip Frames'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
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
      Hint = 'Export all frames in file'
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
    object rbRange: TRadioButton
      Left = 8
      Top = 32
      Width = 57
      Height = 17
      Hint = 'Export range of frames selected below'
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
      Hint = 'Export every Nth frame'
      AutoSize = False
      ShowHint = True
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
  end
  object FrameTypeGrp: TGroupBox
    Left = 245
    Top = 74
    Width = 164
    Height = 160
    Caption = ' Wavelengths  '
    TabOrder = 2
    object rbAllFrameTypes: TRadioButton
      Left = 8
      Top = 16
      Width = 113
      Height = 17
      Hint = 'Export all images from multi-wavelength files'
      Caption = 'All Wavelengths'
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
    end
    object GroupBox5: TGroupBox
      Left = 32
      Top = 32
      Width = 121
      Height = 49
      TabOrder = 1
      object rbInterleaved: TRadioButton
        Left = 8
        Top = 28
        Width = 105
        Height = 17
        Hint = 'Export wavelengths interleaved in a single file'
        Caption = 'Interleaved'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object rbSeparate: TRadioButton
        Left = 8
        Top = 12
        Width = 105
        Height = 17
        Hint = 'Export wavelengths into separate files'
        Caption = 'Separate files'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        TabStop = True
      end
    end
    object rbSingleFrameType: TRadioButton
      Left = 8
      Top = 88
      Width = 129
      Height = 17
      Hint = 
        'Export the image illuminated with wavelength selected from list ' +
        'below'
      Caption = 'Single Wavelength'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
    end
    object cbFrameType: TComboBox
      Left = 32
      Top = 108
      Width = 105
      Height = 22
      ItemHeight = 14
      TabOrder = 3
      Text = 'cbROI'
    end
  end
  object GroupBox3: TGroupBox
    Left = 414
    Top = 74
    Width = 121
    Height = 160
    Caption = ' Format '
    TabOrder = 3
    object rbBioRad: TRadioButton
      Left = 8
      Top = 16
      Width = 89
      Height = 17
      Hint = 'Export to BioRAD PIC format file'
      Caption = 'BioRad PIC'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbBioRadClick
    end
    object rbMetaMorph: TRadioButton
      Left = 8
      Top = 32
      Width = 105
      Height = 17
      Hint = 'Export to MetaMorph STK format file'
      Caption = 'MetaMorph STK'
      TabOrder = 1
      OnClick = rbBioRadClick
    end
    object rbTIFF: TRadioButton
      Left = 8
      Top = 48
      Width = 57
      Height = 17
      Hint = 'Export to TIFF format file'
      Caption = 'TIFF'
      TabOrder = 2
      OnClick = rbBioRadClick
    end
  end
  object bOK: TButton
    Left = 8
    Top = 200
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
    TabOrder = 4
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 64
    Top = 200
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
    TabOrder = 5
    OnClick = bCancelClick
  end
  object GroupBox4: TGroupBox
    Left = 128
    Top = 74
    Width = 113
    Height = 160
    Caption = ' Image '
    TabOrder = 6
    object rbWholeImage: TRadioButton
      Left = 8
      Top = 16
      Width = 94
      Height = 17
      Hint = 'Export whole image in  frame'
      Caption = 'Whole Image'
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
    object rbROI: TRadioButton
      Left = 8
      Top = 48
      Width = 49
      Height = 17
      Hint = 'Export image region within ROI selected from list below'
      Caption = 'ROI'
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
    object cbROINum: TComboBox
      Left = 24
      Top = 64
      Width = 81
      Height = 22
      Hint = 'ROI no. to be deleted'
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 2
    end
    object rbAllROIs: TRadioButton
      Left = 8
      Top = 32
      Width = 81
      Height = 17
      Hint = 'Export image areas defined by ROIs'
      Caption = 'All ROIs'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
  end
  object ImageFile: TImageFile
    XResolution = 1.000000000000000000
    YResolution = 1.000000000000000000
    ZResolution = 1.000000000000000000
    TResolution = 1.000000000000000000
    Left = 168
    Top = 224
  end
  object SaveDialog: TSaveDialog
    Left = 200
    Top = 224
  end
end
