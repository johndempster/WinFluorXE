object AVIFrm: TAVIFrm
  Left = 394
  Top = 289
  BorderStyle = bsDialog
  Caption = 'Create Movie'
  ClientHeight = 344
  ClientWidth = 679
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnClose = FormClose
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object bOK: TButton
    Left = 4
    Top = 296
    Width = 53
    Height = 25
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = bOKClick
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 50
    Width = 141
    Height = 119
    Caption = ' Range '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 32
      Top = 88
      Width = 66
      Height = 15
      Alignment = taRightJustify
      Caption = 'Skip frames'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Shape1: TShape
      Left = 12
      Top = 78
      Width = 120
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
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
    end
    object rbRange: TRadioButton
      Left = 8
      Top = 32
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
      Left = 28
      Top = 48
      Width = 105
      Height = 20
      AutoSize = False
      Text = ' 1 - 1 '
      LoValue = 1.000000000000000000
      HiValue = 1.000000000000000000
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      NumberFormat = '%.0f - %.0f'
    end
    object edFrameStep: TValidatedEdit
      Left = 104
      Top = 88
      Width = 29
      Height = 20
      AutoSize = False
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E29
      HiLimit = 1.000000015047466E29
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 170
    Width = 141
    Height = 119
    Caption = ' AVI  '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object Label2: TLabel
      Left = 11
      Top = 16
      Width = 78
      Height = 15
      Alignment = taRightJustify
      Caption = 'Playback Rate'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object edPlayBackRate: TValidatedEdit
      Left = 8
      Top = 34
      Width = 81
      Height = 20
      AutoSize = False
      Text = ' 20.0 fps'
      Value = 20.000000000000000000
      Scale = 1.000000000000000000
      Units = 'fps'
      NumberFormat = '%0.1f'
      LoLimit = 0.100000001490116100
      HiLimit = 20.000000000000000000
    end
  end
  object bCancel: TButton
    Left = 64
    Top = 296
    Width = 49
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
  object FilenameGrp: TGroupBox
    Left = 4
    Top = 0
    Width = 669
    Height = 49
    Caption = ' Export File Name '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    object edFileName: TEdit
      Left = 120
      Top = 16
      Width = 537
      Height = 23
      ReadOnly = True
      TabOrder = 0
      Text = 'edFileName'
    end
    object bChangeName: TButton
      Left = 8
      Top = 16
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
  object PlotChannelGrp: TGroupBox
    Left = 152
    Top = 50
    Width = 521
    Height = 287
    Caption = ' Plot Channels '
    TabOrder = 5
    object Label4: TLabel
      Left = 242
      Top = 252
      Width = 49
      Height = 15
      Alignment = taRightJustify
      Caption = 'Plot Area'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object GroupBox6: TGroupBox
      Left = 8
      Top = 18
      Width = 225
      Height = 263
      TabOrder = 0
      object bClearPlots: TButton
        Left = 112
        Top = 176
        Width = 105
        Height = 17
        Caption = 'Clear Plot'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = bClearPlotsClick
      end
      object GroupBox8: TGroupBox
        Left = 112
        Top = 200
        Width = 105
        Height = 49
        Caption = ' Subtract ROI '
        TabOrder = 1
        object cbSubtractROI: TComboBox
          Left = 8
          Top = 18
          Width = 89
          Height = 23
          Style = csDropDownList
          ItemHeight = 15
          TabOrder = 0
          OnChange = cbSubtractROIChange
        end
      end
      object bAddPlot: TButton
        Left = 8
        Top = 14
        Width = 97
        Height = 17
        Caption = 'Add To Plot'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = bAddPlotClick
      end
      object cbROI: TComboBox
        Left = 10
        Top = 36
        Width = 95
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        TabOrder = 3
      end
      object mePlotList: TMemo
        Left = 112
        Top = 16
        Width = 105
        Height = 153
        TabOrder = 4
      end
      object rbFLuorescence: TRadioButton
        Left = 8
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Fluorescence'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
        TabStop = True
        OnClick = rbFLuorescenceClick
      end
      object rbRatio: TRadioButton
        Left = 8
        Top = 80
        Width = 97
        Height = 17
        Caption = 'Ratio'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 6
        OnClick = rbRatioClick
      end
      object FLPanel: TPanel
        Left = 8
        Top = 106
        Width = 97
        Height = 145
        BevelOuter = bvNone
        TabOrder = 7
        object ckFrameType0: TCheckBox
          Left = 0
          Top = 0
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
        end
        object ckFrameType1: TCheckBox
          Left = 0
          Top = 16
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
        end
        object ckFrameType2: TCheckBox
          Left = 0
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
          TabOrder = 2
        end
        object ckFrameType3: TCheckBox
          Left = 0
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
          TabOrder = 3
        end
        object ckFrameType4: TCheckBox
          Left = 0
          Top = 64
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
        end
        object ckFrameType5: TCheckBox
          Left = 0
          Top = 80
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
        end
        object ckFrameType6: TCheckBox
          Left = 0
          Top = 98
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
        end
        object ckFrameType7: TCheckBox
          Left = 0
          Top = 114
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
        end
        object ckFrameType8: TCheckBox
          Left = 0
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
          TabOrder = 8
        end
      end
      object RatioPanel: TPanel
        Left = 8
        Top = 98
        Width = 97
        Height = 95
        BevelOuter = bvNone
        TabOrder = 8
        object Label5: TLabel
          Left = 7
          Top = 58
          Width = 43
          Height = 28
          Alignment = taRightJustify
          Caption = 'Excl. Thresh.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object Shape2: TShape
          Left = 4
          Top = 28
          Width = 81
          Height = 2
        end
        object cbNumWave: TComboBox
          Left = 2
          Top = 2
          Width = 85
          Height = 23
          Hint = 'Numerator frame type '
          Style = csDropDownList
          ItemHeight = 15
          TabOrder = 0
        end
        object cbDenWave: TComboBox
          Left = 2
          Top = 32
          Width = 85
          Height = 23
          Hint = 'Denominator frame type'
          Style = csDropDownList
          ItemHeight = 15
          TabOrder = 1
        end
        object edRatioExclusionThreshold: TValidatedEdit
          Left = 50
          Top = 58
          Width = 35
          Height = 18
          Hint = 'Constant F0 value'
          AutoSize = False
          Text = ' 0 '
          Scale = 1.000000000000000000
          NumberFormat = '%.0f'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
      end
    end
    object CalTableGrp: TGroupBox
      Left = 240
      Top = 12
      Width = 273
      Height = 181
      Caption = ' Calibration Bars '
      TabOrder = 1
      object sgCalTable: TStringGrid
        Left = 8
        Top = 16
        Width = 257
        Height = 153
        ColCount = 3
        DefaultColWidth = 80
        DefaultRowHeight = 20
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        ScrollBars = ssNone
        TabOrder = 0
        RowHeights = (
          20
          20
          20
          20
          20)
      end
    end
    object TextGrp: TGroupBox
      Left = 236
      Top = 198
      Width = 277
      Height = 49
      Hint = '4'
      Caption = ' Text  '
      TabOrder = 2
      object edFont: TEdit
        Left = 104
        Top = 16
        Width = 169
        Height = 21
        AutoSize = False
        ReadOnly = True
        TabOrder = 0
        Text = 'edFont'
      end
      object bSetFont: TButton
        Left = 8
        Top = 16
        Width = 89
        Height = 17
        Caption = 'Set Font/Size'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bSetFontClick
      end
    end
    object edPlotArea: TValidatedEdit
      Left = 296
      Top = 252
      Width = 49
      Height = 20
      AutoSize = False
      Text = ' 50 %'
      Value = 0.500000000000000000
      Scale = 100.000000000000000000
      Units = '%'
      NumberFormat = '%.4g'
      LoLimit = 0.100000001490116100
      HiLimit = 10.000000000000000000
    end
  end
  object SaveDialog: TSaveDialog
    Left = 72
    Top = 328
  end
  object FontDialog: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    Left = 104
    Top = 336
  end
end
