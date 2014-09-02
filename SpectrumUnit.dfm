object SpectrumFrm: TSpectrumFrm
  Left = 383
  Top = 168
  Caption = 'Spectral Analysis'
  ClientHeight = 422
  ClientWidth = 572
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
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Page: TPageControl
    Left = 0
    Top = 0
    Width = 577
    Height = 417
    ActivePage = SpectrumTab
    TabOrder = 0
    object SpectrumTab: TTabSheet
      Caption = 'Spectrum'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object plSpectrum: TXYPlotDisplay
        Left = 192
        Top = 8
        Width = 361
        Height = 273
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
        MarkerSize = 6
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
        PrinterLeftMargin = 3
        PrinterRightMargin = 3
        PrinterTopMargin = 3
        PrinterBottomMargin = 3
        PrinterDisableColor = False
        MetafileWidth = 500
        MetafileHeight = 400
      end
      object SpectrumGrp: TGroupBox
        Left = 8
        Top = 0
        Width = 177
        Height = 385
        TabOrder = 0
        object ControlGrp: TGroupBox
          Left = 8
          Top = 8
          Width = 161
          Height = 89
          Caption = ' Spectrum No. '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object lbSpectrumFrame: TLabel
            Left = 8
            Top = 58
            Width = 15
            Height = 15
            Caption = 'xxx'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object sbSpectrumNum: TScrollBar
            Left = 8
            Top = 38
            Width = 145
            Height = 18
            Hint = 'Select frame number to be displayed'
            PageSize = 0
            TabOrder = 0
            OnChange = sbSpectrumNumChange
          end
          object edSpectrumNum: TRangeEdit
            Left = 8
            Top = 16
            Width = 145
            Height = 20
            Hint = 'Frame number on display'
            AutoSize = False
            Text = ' 1 / 1 '
            LoValue = 1.000000000000000000
            HiValue = 1.000000000000000000
            HiLimit = 1.000000015047466E30
            Scale = 1.000000000000000000
            NumberFormat = '%.0f / %.0f'
          end
        end
        object GroupBox1: TGroupBox
          Left = 8
          Top = 160
          Width = 161
          Height = 97
          TabOrder = 1
          object Label2: TLabel
            Left = 8
            Top = 8
            Width = 114
            Height = 13
            Caption = ' Wavelength Range'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label3: TLabel
            Left = 8
            Top = 48
            Width = 55
            Height = 13
            Hint = '8'
            Alignment = taRightJustify
            Caption = 'Step Size'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label4: TLabel
            Left = 88
            Top = 48
            Width = 60
            Height = 13
            Alignment = taRightJustify
            Caption = 'Bandwidth'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edSpectrumRange: TRangeEdit
            Left = 8
            Top = 24
            Width = 145
            Height = 20
            Hint = '8'
            OnKeyPress = edSpectrumRangeKeyPress
            AutoSize = False
            ShowHint = True
            Text = ' 350.00 - 750.00 nm'
            LoValue = 350.000000000000000000
            HiValue = 750.000000000000000000
            LoLimit = 100.000000000000000000
            HiLimit = 2000.000000000000000000
            Scale = 1.000000000000000000
            Units = 'nm'
            NumberFormat = '%.f - %.f'
          end
          object edSpectrumStepSize: TValidatedEdit
            Left = 8
            Top = 64
            Width = 73
            Height = 20
            Hint = '8'
            OnKeyPress = edSpectrumStepSizeKeyPress
            AutoSize = False
            ShowHint = True
            Text = ' 10 nm'
            Value = 10.000000000000000000
            Scale = 1.000000000000000000
            Units = 'nm'
            NumberFormat = '%.4g'
            LoLimit = 5.000000000000000000
            HiLimit = 1000.000000000000000000
          end
          object edSpectrumBandwidth: TValidatedEdit
            Left = 88
            Top = 64
            Width = 65
            Height = 20
            Hint = 'Excitation spectrum bandwidth'
            OnKeyPress = edSpectrumBandwidthKeyPress
            AutoSize = False
            ShowHint = True
            Text = ' 10 nm'
            Value = 10.000000000000000000
            Scale = 1.000000000000000000
            Units = 'nm'
            NumberFormat = '%.4g'
            LoLimit = 1.000000000000000000
            HiLimit = 1000.000000000000000000
          end
        end
        object GroupBox2: TGroupBox
          Left = 8
          Top = 96
          Width = 161
          Height = 65
          TabOrder = 2
          object Label5: TLabel
            Left = 56
            Top = 12
            Width = 20
            Height = 15
            Alignment = taRightJustify
            Caption = 'ROI'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label7: TLabel
            Left = 63
            Top = 36
            Width = 13
            Height = 23
            Alignment = taRightJustify
            Caption = '-'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -21
            Font.Name = 'Courier New'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object cbROI: TComboBox
            Left = 80
            Top = 12
            Width = 73
            Height = 21
            TabOrder = 0
            Text = 'cbROI'
            OnChange = cbROIChange
          end
          object cbSubtractROI: TComboBox
            Left = 80
            Top = 36
            Width = 73
            Height = 21
            TabOrder = 1
            Text = 'cbROI'
            OnChange = cbSubtractROIChange
          end
        end
      end
    end
    object TimeCourseTab: TTabSheet
      Caption = 'Time Course'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object plTimeCourse: TXYPlotDisplay
        Left = 192
        Top = 8
        Width = 369
        Height = 217
        OnCursorChange = plTimeCourseCursorChange
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
        MarkerSize = 6
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
        PrinterLeftMargin = 2
        PrinterRightMargin = 2
        PrinterTopMargin = 2
        PrinterBottomMargin = 2
        PrinterDisableColor = False
        MetafileWidth = 500
        MetafileHeight = 400
      end
      object lbTimeCourseCursor: THTMLLabel
        Left = 304
        Top = 232
        Width = 113
        Height = 153
        Caption = 'Label'
        Alignment = taCenter
        LineSpacing = 1.000000000000000000
        Color = clWhite
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
      end
      object TimeCourseGrp: TGroupBox
        Left = 8
        Top = 0
        Width = 177
        Height = 385
        TabOrder = 0
        object GroupBox6: TGroupBox
          Left = 8
          Top = 8
          Width = 161
          Height = 345
          TabOrder = 0
          object GroupBox3: TGroupBox
            Left = 8
            Top = 158
            Width = 145
            Height = 83
            Caption = ' Range '
            TabOrder = 0
            object rbAllSpectra: TRadioButton
              Left = 8
              Top = 16
              Width = 89
              Height = 17
              Hint = 'Average all detected events'
              Caption = 'All Spectra'
              Checked = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
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
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 1
            end
            object edRange: TRangeEdit
              Left = 32
              Top = 52
              Width = 105
              Height = 20
              Hint = 'Sub-range of events to be averaged'
              AutoSize = False
              Text = ' 1 - 1 '
              LoValue = 1.000000000000000000
              HiValue = 1.000000000000000000
              HiLimit = 1.000000015047466E30
              Scale = 1.000000000000000000
              NumberFormat = '%.0f - %.0f'
            end
          end
          object bPlotTimeCourse: TButton
            Left = 8
            Top = 248
            Width = 145
            Height = 20
            Caption = 'Plot Graph'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = bPlotTimeCourseClick
          end
          object bSetAxes: TButton
            Left = 8
            Top = 272
            Width = 145
            Height = 20
            Caption = 'Set Axes'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
            OnClick = bSetAxesClick
          end
          object GroupBox4: TGroupBox
            Left = 8
            Top = 72
            Width = 145
            Height = 83
            Caption = ' Wavelengths  '
            TabOrder = 3
            object rbAllWavelengths: TRadioButton
              Left = 8
              Top = 16
              Width = 113
              Height = 17
              Hint = 'Average all detected events'
              Caption = 'All Wavelengths'
              Checked = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 0
              TabStop = True
            end
            object rbSingleWavelength: TRadioButton
              Left = 8
              Top = 32
              Width = 129
              Height = 17
              Hint = 'Plot sub-range of frames'
              Caption = 'Single Wavelength'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 1
            end
            object cbTimeCourseWavelength: TComboBox
              Left = 32
              Top = 52
              Width = 105
              Height = 21
              TabOrder = 2
              Text = 'cbROI'
            end
          end
          object GroupBox5: TGroupBox
            Left = 8
            Top = 8
            Width = 145
            Height = 65
            TabOrder = 4
            object Label10: TLabel
              Left = 40
              Top = 12
              Width = 20
              Height = 15
              Alignment = taRightJustify
              Caption = 'ROI'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label6: TLabel
              Left = 49
              Top = 36
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
            object cbTimeCourseROI: TComboBox
              Left = 64
              Top = 12
              Width = 73
              Height = 21
              TabOrder = 0
              Text = 'cbROI'
            end
            object cbTimeCourseSubtractROI: TComboBox
              Left = 64
              Top = 36
              Width = 73
              Height = 21
              TabOrder = 1
              Text = 'cbROI'
            end
          end
        end
      end
    end
  end
end
