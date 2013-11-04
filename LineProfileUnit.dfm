object LineProfileFrm: TLineProfileFrm
  Left = 786
  Top = 157
  Width = 506
  Height = 469
  Caption = 'Line Profile'
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
  OnCloseQuery = FormCloseQuery
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Page: TPageControl
    Left = 8
    Top = 8
    Width = 449
    Height = 425
    ActivePage = EdgeTrackTab
    TabOrder = 0
    object LineProfileTab: TTabSheet
      Caption = 'Line Profile'
      object plPlot: TXYPlotDisplay
        Left = 152
        Top = 7
        Width = 265
        Height = 322
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
        PrinterLeftMargin = 12
        PrinterRightMargin = 12
        PrinterTopMargin = 12
        PrinterBottomMargin = 12
        PrinterDisableColor = False
        MetafileWidth = 500
        MetafileHeight = 400
      end
      object LPControlsGrp: TGroupBox
        Left = 2
        Top = 2
        Width = 137
        Height = 375
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label5: TLabel
          Left = 8
          Top = 8
          Width = 65
          Height = 14
          Caption = 'Frame Type'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label1: TLabel
          Left = 31
          Top = 50
          Width = 18
          Height = 14
          Alignment = taRightJustify
          Caption = 'ROI'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object cbFrameType: TComboBox
          Left = 8
          Top = 24
          Width = 121
          Height = 23
          Style = csDropDownList
          ItemHeight = 15
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          OnChange = cbFrameTypeChange
        end
        object cbROI: TComboBox
          Left = 56
          Top = 52
          Width = 70
          Height = 23
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ItemHeight = 15
          ParentFont = False
          TabOrder = 1
          OnChange = cbROIChange
        end
        object GroupBox1: TGroupBox
          Left = 8
          Top = 80
          Width = 121
          Height = 73
          Caption = ' Axes Range '
          TabOrder = 2
          object bSetAxes: TButton
            Left = 24
            Top = 50
            Width = 89
            Height = 17
            Caption = 'Set Axes'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            OnClick = bSetAxesClick
          end
          object rbAutoScale: TRadioButton
            Left = 8
            Top = 16
            Width = 89
            Height = 17
            Caption = 'Auto Scale'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            TabStop = True
          end
          object rbManual: TRadioButton
            Left = 8
            Top = 32
            Width = 89
            Height = 17
            Caption = 'Manual'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
          end
        end
        object GroupBox3: TGroupBox
          Left = 8
          Top = 152
          Width = 121
          Height = 65
          TabOrder = 3
          object Label3: TLabel
            Left = 8
            Top = 8
            Width = 58
            Height = 14
            Caption = 'Line width'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edLineWidth: TValidatedEdit
            Left = 8
            Top = 24
            Width = 105
            Height = 21
            OnKeyPress = edLineWidthKeyPress
            AutoSize = False
            Text = ' 1 pixels'
            Value = 1.000000000000000000
            Scale = 1.000000000000000000
            Units = 'pixels'
            NumberFormat = '%.0f'
            LoLimit = 1.000000000000000000
            HiLimit = 1.000000015047466E30
          end
        end
      end
    end
    object EdgeTrackTab: TTabSheet
      Caption = 'Edge Track'
      ImageIndex = 1
      object plEdgePlot: TXYPlotDisplay
        Left = 146
        Top = 7
        Width = 303
        Height = 322
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
        PrinterLeftMargin = 11
        PrinterRightMargin = 11
        PrinterTopMargin = 11
        PrinterBottomMargin = 11
        PrinterDisableColor = False
        MetafileWidth = 500
        MetafileHeight = 400
      end
      object EPControlsGrp: TGroupBox
        Left = 2
        Top = 2
        Width = 137
        Height = 351
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label2: TLabel
          Left = 8
          Top = 8
          Width = 31
          Height = 14
          Caption = 'Track'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Button1: TButton
          Left = 8
          Top = 212
          Width = 121
          Height = 17
          Caption = 'Set Axes'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = Button1Click
        end
        object cbEdge: TComboBox
          Left = 8
          Top = 24
          Width = 121
          Height = 23
          Style = csDropDownList
          ItemHeight = 15
          ParentShowHint = False
          ShowHint = False
          TabOrder = 1
          OnChange = cbFrameTypeChange
        end
        object bDoEdgePlot: TButton
          Left = 7
          Top = 52
          Width = 122
          Height = 17
          Caption = 'Plot Time Course'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          OnClick = bDoEdgePlotClick
        end
        object RangeGrp: TGroupBox
          Left = 6
          Top = 70
          Width = 121
          Height = 83
          Caption = ' Plot Range '
          TabOrder = 3
          object rbAllFrames: TRadioButton
            Left = 8
            Top = 16
            Width = 89
            Height = 17
            Hint = 'Plot all frames in file'
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
            Hint = 'Plot sub-range of frames'
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
            Top = 48
            Width = 89
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
        object GroupBox2: TGroupBox
          Left = 8
          Top = 152
          Width = 121
          Height = 57
          Caption = ' X Axis '
          TabOrder = 4
          object rbXAxisFrames: TRadioButton
            Left = 8
            Top = 32
            Width = 87
            Height = 17
            Caption = 'Frames'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
          end
          object rbXAxisTime: TRadioButton
            Left = 8
            Top = 16
            Width = 87
            Height = 17
            Caption = 'Time (s)'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            TabStop = True
          end
        end
      end
    end
  end
  object Timer: TTimer
    Interval = 55
    OnTimer = TimerTimer
    Left = 168
    Top = 384
  end
end
