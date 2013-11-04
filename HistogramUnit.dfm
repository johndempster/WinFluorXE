object HistogramFrm: THistogramFrm
  Left = 678
  Top = 245
  Width = 578
  Height = 496
  Caption = 'Pixel Intensity Histogram'
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
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object plPlot: TXYPlotDisplay
    Left = 152
    Top = 8
    Width = 297
    Height = 361
    MaxPointsPerLine = 4096
    XAxisMax = 1.000000000000000000
    XAxisTick = 0.200000002980232200
    XAxisLaw = axLinear
    XAxisLabel = 'X Axis'
    XAxisAutoRange = False
    YAxisMax = 1.000000000000000000
    YAxisTick = 0.200000002980232200
    YAxisLaw = axLinear
    YAxisLabel = 'Y Axis'
    YAxisAutoRange = False
    YAxisLabelAtTop = False
    ScreenFontName = 'Arial'
    ScreenFontSize = 10
    LineWidth = 1
    MarkerSize = 10
    ShowLines = True
    ShowMarkers = True
    HistogramFullBorders = False
    HistogramFillColor = clWhite
    HistogramFillStyle = bsClear
    HistogramCumulative = False
    HistogramPercentage = False
    PrinterFontSize = 10
    PrinterFontName = 'Arial'
    PrinterLineWidth = 1
    PrinterMarkerSize = 5
    PrinterLeftMargin = 18
    PrinterRightMargin = 18
    PrinterTopMargin = 18
    PrinterBottomMargin = 18
    PrinterDisableColor = False
    MetafileWidth = 500
    MetafileHeight = 400
  end
  object HistGrp: TGroupBox
    Left = 8
    Top = 2
    Width = 137
    Height = 363
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 39
      Height = 15
      Caption = 'Source'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Top = 48
      Width = 65
      Height = 15
      Caption = 'Frame Type'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object cbSource: TComboBox
      Left = 8
      Top = 24
      Width = 121
      Height = 23
      Hint = 'Source of images for intensity histogram'
      Style = csDropDownList
      ItemHeight = 15
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      OnChange = cbSourceChange
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 160
      Width = 121
      Height = 81
      Caption = ' Histogram Range '
      TabOrder = 1
      object bFullRange: TButton
        Left = 8
        Top = 16
        Width = 105
        Height = 17
        Hint = 
          'Set histogram range to full range of intensities supported by ca' +
          'mera'
        Caption = 'Full Range'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = bFullRangeClick
      end
      object bAutoScale: TButton
        Left = 8
        Top = 36
        Width = 105
        Height = 17
        Hint = 'Set histogram to rmin.-max. range of intensities within image'
        Caption = 'Auto-Scale'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bAutoScaleClick
      end
      object bSetAxes: TButton
        Left = 8
        Top = 56
        Width = 105
        Height = 17
        Hint = 'Customise histogram axis range and labels'
        Caption = 'Set Axes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = bSetAxesClick
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 246
      Width = 121
      Height = 106
      Hint = 'Image intensity statistics'
      Caption = ' Statistics '
      TabOrder = 2
      object meStatistics: TMemo
        Left = 8
        Top = 16
        Width = 105
        Height = 82
        Lines.Strings = (
          '')
        ReadOnly = True
        TabOrder = 0
      end
    end
    object cbFrameType: TComboBox
      Left = 8
      Top = 64
      Width = 121
      Height = 23
      Hint = 'Type of image frame to be used'
      Style = csDropDownList
      ItemHeight = 15
      ParentShowHint = False
      ShowHint = False
      TabOrder = 3
      OnChange = cbFrameTypeChange
    end
    object ROIGrp: TGroupBox
      Left = 8
      Top = 88
      Width = 121
      Height = 65
      TabOrder = 4
      object rbWholeImage: TRadioButton
        Left = 8
        Top = 8
        Width = 105
        Height = 17
        Hint = 'Compute histogram from all pixels within image'
        Caption = 'Whole Image'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        TabStop = True
        OnClick = rbWholeImageClick
      end
      object rbROI: TRadioButton
        Left = 8
        Top = 26
        Width = 41
        Height = 17
        Hint = 'Compute histogram from a specific region of interest'
        Caption = 'ROI'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = rbROIClick
      end
      object cbROI: TComboBox
        Left = 48
        Top = 28
        Width = 65
        Height = 23
        Hint = 'Region of interest to be analysed'
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ItemHeight = 15
        ParentFont = False
        TabOrder = 2
        OnChange = cbROIChange
      end
    end
  end
  object Timer: TTimer
    Interval = 100
    OnTimer = TimerTimer
    Left = 224
    Top = 376
  end
end
