object TimeCourseFrm: TTimeCourseFrm
  Left = 548
  Top = 61
  Width = 588
  Height = 644
  Caption = 'Time Course Analysis'
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
  OnCloseQuery = FormCloseQuery
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object plPlot: TXMultiYPlot
    Left = 203
    Top = 8
    Width = 370
    Height = 321
    PlotNum = 0
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
    PrinterLeftMargin = 0
    PrinterRightMargin = 0
    PrinterTopMargin = 0
    PrinterBottomMargin = 0
    PrinterDisableColor = False
    MetafileWidth = 500
    MetafileHeight = 400
  end
  object ControlsGrp: TGroupBox
    Left = 8
    Top = 4
    Width = 188
    Height = 605
    TabOrder = 0
    object RangeGrp: TGroupBox
      Left = 8
      Top = 488
      Width = 168
      Height = 65
      Caption = ' Plot Range '
      TabOrder = 0
      object rbAllFrames: TRadioButton
        Left = 8
        Top = 16
        Width = 89
        Height = 17
        Hint = 'Plot all frames in file'
        Caption = 'All Points'
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
      object rbRange: TRadioButton
        Left = 8
        Top = 32
        Width = 57
        Height = 17
        Hint = 'Plot sub-range of frames'
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
        Left = 64
        Top = 36
        Width = 93
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
    object ROIGrp: TGroupBox
      Left = 8
      Top = 8
      Width = 168
      Height = 473
      Caption = ' ROI / Channel '
      TabOrder = 1
      object Label1: TLabel
        Left = 45
        Top = 16
        Width = 34
        Height = 13
        Caption = 'Source'
      end
      object Label2: TLabel
        Left = 21
        Top = 40
        Width = 58
        Height = 13
        Caption = 'Background'
      end
      object cbSource: TComboBox
        Left = 85
        Top = 16
        Width = 73
        Height = 21
        Hint = 'Region of interest to be plotted'
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbSourceChange
      end
      object cbBackground: TComboBox
        Left = 85
        Top = 40
        Width = 73
        Height = 21
        Hint = 'Background region of interest to be subtracted '
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
      end
      object ExcludePanel: TPanel
        Left = 8
        Top = 68
        Width = 145
        Height = 41
        BevelOuter = bvNone
        TabOrder = 2
      end
      object PlotGrp: TGroupBox
        Left = 8
        Top = 316
        Width = 153
        Height = 145
        TabOrder = 3
        object Shape1: TShape
          Left = 8
          Top = 84
          Width = 137
          Height = 1
        end
        object bAddLineToPlot: TButton
          Left = 8
          Top = 36
          Width = 84
          Height = 17
          Hint = 'Add line to existing Y Axis'
          Caption = 'Add to Plot'
          Enabled = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = bAddLineToPlotClick
        end
        object cbPlotNum: TComboBox
          Left = 101
          Top = 34
          Width = 41
          Height = 21
          Hint = 'Y Axis number to be plotted on'
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
        object bAddAllROIs: TButton
          Left = 8
          Top = 60
          Width = 136
          Height = 17
          Hint = 'Add all available ROIs to plot'
          Caption = 'Add all ROIs'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = bAddAllROIsClick
        end
        object bNewPlot: TButton
          Left = 8
          Top = 12
          Width = 136
          Height = 17
          Hint = 'Plot line using new Y axis '
          Caption = 'Add to New Plot'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = bNewPlotClick
        end
        object bClearPlots: TButton
          Left = 8
          Top = 92
          Width = 89
          Height = 17
          Hint = 'Clear all lines from plot'
          Caption = 'Clear Plot'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = bClearPlotsClick
        end
        object cbClearPlot: TComboBox
          Left = 104
          Top = 92
          Width = 41
          Height = 21
          Hint = 'Y Axis number to be plotted on'
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 5
        end
        object bSetAxes: TButton
          Left = 8
          Top = 118
          Width = 136
          Height = 17
          Hint = 'Set X & Y axis range and labels'
          Caption = 'Set Axes'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          OnClick = bSetAxesClick
        end
      end
      object FluorescenceDisplayGrp: TGroupBox
        Left = 7
        Top = 66
        Width = 153
        Height = 249
        Caption = ' Fluorescence Display '
        TabOrder = 4
        object DisplayModePage: TPageControl
          Left = 2
          Top = 40
          Width = 145
          Height = 206
          ActivePage = dFTab
          Style = tsFlatButtons
          TabHeight = 18
          TabOrder = 0
          object FTab: TTabSheet
            Caption = 'Fluor'
            TabVisible = False
            object Label3: TLabel
              Left = 5
              Top = 2
              Width = 44
              Height = 13
              Alignment = taRightJustify
              Caption = 'Excit. ch.'
            end
            object cbWavelength: TComboBox
              Left = 57
              Top = 2
              Width = 79
              Height = 21
              Hint = 'Frame type to be displayed'
              Style = csDropDownList
              ItemHeight = 0
              TabOrder = 0
            end
          end
          object dFTab: TTabSheet
            Caption = 'F/F0'
            ImageIndex = 1
            TabVisible = False
            object Label6: TLabel
              Left = 5
              Top = 2
              Width = 44
              Height = 13
              Alignment = taRightJustify
              Caption = 'Excit. ch.'
            end
            object rbF0FromFrames: TRadioButton
              Left = 0
              Top = 26
              Width = 67
              Height = 15
              Hint = 'Compute F0 from series of frames'
              Caption = 'F0 points'
              Checked = True
              Color = clBtnFace
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentColor = False
              ParentFont = False
              TabOrder = 0
              TabStop = True
            end
            object edF0Range: TRangeEdit
              Left = 41
              Top = 43
              Width = 94
              Height = 20
              Hint = 'Ranges of frames averaged to compute F0.'
              AutoSize = False
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = []
              ShowHint = True
              Text = ' 1 - 1 '
              LoValue = 1.000000000000000000
              HiValue = 1.000000000000000000
              LoLimit = 1.000000000000000000
              HiLimit = 1.000000015047466E30
              Scale = 1.000000000000000000
              NumberFormat = '%.f - %.f'
            end
            object rbF0Constant: TRadioButton
              Left = 0
              Top = 70
              Width = 46
              Height = 15
              Hint = 'Use constant F0 value'
              Caption = 'F0 ='
              Color = clBtnFace
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentColor = False
              ParentFont = False
              TabOrder = 2
            end
            object edF0Constant: TValidatedEdit
              Left = 57
              Top = 70
              Width = 78
              Height = 20
              Hint = 'User entered F0 value'
              AutoSize = False
              ShowHint = True
              Text = ' 1.0 '
              Value = 1.000000000000000000
              Scale = 1.000000000000000000
              NumberFormat = '%.1f'
              LoLimit = 1.000000000000000000
              HiLimit = 1.000000015047466E30
            end
            object cbdFWavelength: TComboBox
              Left = 57
              Top = 2
              Width = 79
              Height = 21
              Hint = 'Frame type to be displayed'
              Style = csDropDownList
              ItemHeight = 13
              ParentShowHint = False
              ShowHint = True
              TabOrder = 4
            end
            object GroupBox1: TGroupBox
              Left = 0
              Top = 97
              Width = 136
              Height = 48
              TabOrder = 5
              object rbDFOverF0: TRadioButton
                Left = 8
                Top = 10
                Width = 74
                Height = 20
                Hint = 
                  'Plot difference in fractional change in fluorescence relative to' +
                  ' F0'
                Caption = '(F-F0)/F0'
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
              object rbFOverF0: TRadioButton
                Left = 8
                Top = 26
                Width = 52
                Height = 20
                Hint = 'Plot fluorescence as fraction of  F0'
                Caption = 'F/F0'
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentFont = False
                TabOrder = 1
              end
            end
          end
          object RatioTab: TTabSheet
            Caption = 'Ratio'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ImageIndex = 2
            ParentFont = False
            TabVisible = False
            object Shape2: TShape
              Left = 56
              Top = 27
              Width = 79
              Height = 2
            end
            object Label4: TLabel
              Left = 19
              Top = 0
              Width = 24
              Height = 42
              Alignment = taRightJustify
              Caption = 'Exc. ch. ratio'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              WordWrap = True
            end
            object Label5: TLabel
              Left = 16
              Top = 60
              Width = 49
              Height = 28
              Alignment = taRightJustify
              Caption = 'Exclusion Threshold'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              WordWrap = True
            end
            object cbNumWave: TComboBox
              Left = 58
              Top = 0
              Width = 79
              Height = 22
              Hint = 'Numerator frame type '
              Style = csDropDownList
              ItemHeight = 0
              TabOrder = 0
            end
            object cbDenomWave: TComboBox
              Left = 58
              Top = 32
              Width = 79
              Height = 22
              Hint = 'Denominator frame type'
              Style = csDropDownList
              ItemHeight = 0
              TabOrder = 1
            end
            object edRatioExclusionThreshold: TValidatedEdit
              Left = 80
              Top = 60
              Width = 56
              Height = 20
              Hint = 'Constant F0 value'
              AutoSize = False
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = []
              Text = ' 1 '
              Value = 1.000000000000000000
              Scale = 1.000000000000000000
              NumberFormat = '%.0f'
              LoLimit = 1.000000000000000000
              HiLimit = 1.000000015047466E30
            end
            object GroupBox8: TGroupBox
              Left = 1
              Top = 92
              Width = 136
              Height = 93
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 3
              object edRatioRMax: TValidatedEdit
                Left = 40
                Top = 29
                Width = 89
                Height = 20
                Hint = 'Maximum fluorescence ratio'
                AutoSize = False
                Text = ' 1 '
                Value = 1.000000000000000000
                Scale = 1.000000000000000000
                NumberFormat = '%.3g'
                LoLimit = 0.000000000100000001
                HiLimit = 1000.000000000000000000
              end
              object cbEquation: TComboBox
                Left = 52
                Top = 60
                Width = 77
                Height = 22
                Hint = 'Binding equation used to compute ion concentration from ratio'
                Style = csDropDownList
                ItemHeight = 0
                TabOrder = 1
              end
              object ckUseEquation: TCheckBox
                Left = 7
                Top = 60
                Width = 39
                Height = 20
                Caption = 'Eqn.'
                TabOrder = 2
                OnClick = ckUseEquationClick
              end
              object ckDivideByRMax: TCheckBox
                Left = 7
                Top = 13
                Width = 65
                Height = 14
                Caption = '/Rmax  ='
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentFont = False
                TabOrder = 3
                OnClick = ckDivideByRMaxClick
              end
            end
          end
        end
        object cbFluorescence: TComboBox
          Left = 8
          Top = 16
          Width = 134
          Height = 21
          ItemHeight = 13
          TabOrder = 1
          Text = 'cbFluorescence'
          OnChange = cbFluorescenceChange
        end
      end
    end
    object TUnitsGrp: TGroupBox
      Left = 8
      Top = 556
      Width = 168
      Height = 35
      Caption = ' Time Units '
      TabOrder = 2
      object rbSeconds: TRadioButton
        Left = 8
        Top = 14
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
        TabOrder = 0
        TabStop = True
      end
      object rbMinutes: TRadioButton
        Left = 64
        Top = 14
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
      end
    end
  end
end
