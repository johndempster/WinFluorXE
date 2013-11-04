object PlotROIFrm: TPlotROIFrm
  Left = 260
  Top = 388
  BorderStyle = bsDialog
  Caption = 'Plot ROI'
  ClientHeight = 248
  ClientWidth = 495
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
  object SourceGrp: TGroupBox
    Left = 8
    Top = 0
    Width = 481
    Height = 105
    Caption = ' Source '
    TabOrder = 0
    object lbNumSection: TLabel
      Left = 389
      Top = 20
      Width = 41
      Height = 14
      Alignment = taRightJustify
      Caption = 'Section'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cbNumFile: TComboBox
      Left = 16
      Top = 14
      Width = 361
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 0
      OnChange = cbNumFileChange
    end
    object edNumSection: TValidatedEdit
      Left = 432
      Top = 14
      Width = 41
      Height = 22
      Text = ' 1 '
      Value = 1.000000000000000000
      LoLimit = 1.000000000000000000
      HiLimit = 2.000000000000000000
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
    end
    object RatioPanel: TPanel
      Left = 2
      Top = 39
      Width = 471
      Height = 42
      BevelOuter = bvNone
      Caption = 'RatioPanel'
      TabOrder = 2
      object lbDenSection: TLabel
        Left = 387
        Top = 20
        Width = 41
        Height = 14
        Alignment = taRightJustify
        Caption = 'Section'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Shape1: TShape
        Left = 14
        Top = 8
        Width = 470
        Height = 1
      end
      object cbDenFile: TComboBox
        Left = 14
        Top = 14
        Width = 363
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        TabOrder = 0
        OnChange = cbDenFileChange
      end
      object edDenSection: TValidatedEdit
        Left = 430
        Top = 14
        Width = 41
        Height = 22
        Text = ' 1 '
        Value = 1.000000000000000000
        LoLimit = 1.000000000000000000
        HiLimit = 2.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
      end
    end
    object ckRatio: TCheckBox
      Left = 8
      Top = 80
      Width = 49
      Height = 17
      Caption = 'Ratio'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = ckRatioClick
    end
  end
  object ROIGrp: TGroupBox
    Left = 232
    Top = 104
    Width = 257
    Height = 105
    Caption = ' Regions of Interest '
    TabOrder = 1
    object ckROI1: TCheckBox
      Left = 8
      Top = 16
      Width = 57
      Height = 25
      Caption = 'ROI #1'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = ckROI1Click
    end
    object CKROI2: TCheckBox
      Left = 8
      Top = 32
      Width = 57
      Height = 25
      Caption = 'ROI #2'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = ckROI1Click
    end
    object ckROI3: TCheckBox
      Left = 8
      Top = 48
      Width = 57
      Height = 25
      Caption = 'ROI #3'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = ckROI1Click
    end
    object ckROI4: TCheckBox
      Left = 8
      Top = 64
      Width = 57
      Height = 25
      Caption = 'ROI #4'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = ckROI1Click
    end
    object ckROI5: TCheckBox
      Left = 72
      Top = 16
      Width = 57
      Height = 25
      Caption = 'ROI #5'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      OnClick = ckROI1Click
    end
    object ckROI6: TCheckBox
      Left = 72
      Top = 32
      Width = 57
      Height = 25
      Caption = 'ROI #6'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = ckROI1Click
    end
    object ckROI7: TCheckBox
      Left = 72
      Top = 48
      Width = 57
      Height = 25
      Caption = 'ROI #7'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
      OnClick = ckROI1Click
    end
    object ckROI8: TCheckBox
      Left = 72
      Top = 64
      Width = 57
      Height = 25
      Caption = 'ROI #8'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 7
      OnClick = ckROI1Click
    end
    object GroupBox3: TGroupBox
      Left = 136
      Top = 8
      Width = 113
      Height = 65
      TabOrder = 8
      object cbSubtractROI: TComboBox
        Left = 26
        Top = 28
        Width = 71
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        TabOrder = 0
      end
      object ckSubtractROI: TCheckBox
        Left = 8
        Top = 8
        Width = 89
        Height = 20
        Caption = 'Subtract ROI'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
      end
    end
  end
  object RangeGrp: TGroupBox
    Left = 8
    Top = 104
    Width = 97
    Height = 105
    Caption = ' Plot Range '
    TabOrder = 2
    object rbAllFrames: TRadioButton
      Left = 8
      Top = 16
      Width = 81
      Height = 17
      Hint = 'Plot all frames in file'
      Caption = 'All Frames'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbRange: TRadioButton
      Left = 8
      Top = 32
      Width = 57
      Height = 17
      Hint = 'Plot sub-range of frames'
      Caption = 'Range'
      TabOrder = 1
    end
    object edRange: TRangeEdit
      Left = 24
      Top = 52
      Width = 65
      Height = 20
      Hint = 'Sub-range of frames to be plotted'
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
    Top = 216
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
    TabOrder = 3
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 64
    Top = 216
    Width = 49
    Height = 20
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
  object VariablesGrp: TGroupBox
    Left = 112
    Top = 104
    Width = 113
    Height = 105
    Caption = ' Plot Variables '
    TabOrder = 5
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 34
      Height = 14
      Caption = 'X Axis'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 56
      Width = 34
      Height = 14
      Caption = 'Y Axis'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cbXAxisVar: TComboBox
      Left = 8
      Top = 32
      Width = 97
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 0
    end
    object cbYAxisVar: TComboBox
      Left = 8
      Top = 72
      Width = 97
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 1
    end
  end
end
