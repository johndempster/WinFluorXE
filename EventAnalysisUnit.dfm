object EventAnalysisFrm: TEventAnalysisFrm
  Tag = 9
  Left = 695
  Top = 111
  Caption = 'Event Detection & Analysis'
  ClientHeight = 653
  ClientWidth = 692
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Page: TPageControl
    Left = 2
    Top = 2
    Width = 692
    Height = 643
    ActivePage = PlotTab
    TabOrder = 0
    OnChange = PageChange
    object DetectTab: TTabSheet
      Caption = 'Detect events'
      ImageIndex = 3
      object DetectGrp: TGroupBox
        Left = 0
        Top = 0
        Width = 169
        Height = 601
        TabOrder = 0
        object DetectCritGrp: TGroupBox
          Left = 8
          Top = 330
          Width = 153
          Height = 257
          Caption = ' Detection Criteria '
          TabOrder = 0
          object Label15: TLabel
            Left = 17
            Top = 44
            Width = 51
            Height = 28
            Alignment = taRightJustify
            Caption = 'Threshold Level'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object Label16: TLabel
            Left = 15
            Top = 76
            Width = 53
            Height = 14
            Alignment = taRightJustify
            Caption = 'Exceed for'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object Label7: TLabel
            Left = 27
            Top = 224
            Width = 50
            Height = 14
            Alignment = taRightJustify
            Caption = 'Dead Time'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object edThresholdLevel: TValidatedEdit
            Left = 72
            Top = 44
            Width = 73
            Height = 20
            Hint = 'Event detection threshold'
            OnKeyPress = edThresholdLevelKeyPress
            AutoSize = False
            ShowHint = True
            Text = ' 1 s'
            Value = 1.000000000000000000
            Scale = 1.000000000000000000
            Units = 's'
            NumberFormat = '%.5g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E29
          end
          object cbDetectionThresholdPolarity: TComboBox
            Left = 8
            Top = 18
            Width = 137
            Height = 22
            Style = csDropDownList
            TabOrder = 1
            OnChange = cbDetectionThresholdPolarityChange
            Items.Strings = (
              'Positive-going events'
              'Negative-going events'
              'Positive or Negative')
          end
          object edThresholdDuration: TValidatedEdit
            Left = 72
            Top = 76
            Width = 73
            Height = 20
            Hint = 'Period of time signal has to exceed threshold to be detected'
            AutoSize = False
            ShowHint = True
            Text = ' 0 s'
            Scale = 1.000000000000000000
            Units = 's'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E29
          end
          object GroupBox7: TGroupBox
            Left = 8
            Top = 104
            Width = 137
            Height = 113
            Caption = ' Baseline '
            TabOrder = 3
            object Label10: TLabel
              Left = 46
              Top = 34
              Width = 11
              Height = 14
              Alignment = taRightJustify
              Caption = 'At'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object Label11: TLabel
              Left = 35
              Top = 78
              Width = 30
              Height = 14
              Alignment = taRightJustify
              Caption = 'Period'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
            end
            object rbFixedBaseline: TRadioButton
              Left = 8
              Top = 16
              Width = 113
              Height = 17
              Hint = 'Keep detection threshold level fixed'
              Caption = 'Fixed Baseline'
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
            object edFixedBaselineLevel: TValidatedEdit
              Left = 64
              Top = 34
              Width = 65
              Height = 20
              Hint = 'Fixed detection threshold baseline level'
              OnKeyPress = edFixedBaselineLevelKeyPress
              AutoSize = False
              ShowHint = True
              Text = ' 1 s'
              Value = 1.000000000000000000
              Scale = 1.000000000000000000
              Units = 's'
              NumberFormat = '%.5g'
              LoLimit = -1.000000015047466E30
              HiLimit = 1.000000015047466E29
            end
            object rbRollingBaseline: TRadioButton
              Left = 8
              Top = 60
              Width = 113
              Height = 17
              Hint = 
                'Update detection threshold using rolling average to track change' +
                's in baseline'
              Caption = 'Rolling Average'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 2
            end
            object edRollingBaselinePeriod: TValidatedEdit
              Left = 72
              Top = 78
              Width = 57
              Height = 20
              Hint = 'Averaging period'
              AutoSize = False
              ShowHint = True
              Text = ' 1 s'
              Value = 1.000000000000000000
              Scale = 1.000000000000000000
              Units = 's'
              NumberFormat = '%.4g'
              LoLimit = -1.000000015047466E30
              HiLimit = 1.000000015047466E29
            end
          end
          object edDeadTime: TValidatedEdit
            Left = 80
            Top = 224
            Width = 65
            Height = 20
            Hint = 'Time period skipped without detection after an event is detected'
            AutoSize = False
            ShowHint = True
            Text = ' 1 s'
            Value = 1.000000000000000000
            Scale = 1.000000000000000000
            Units = 's'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E29
          end
        end
        object bDetectEvents: TButton
          Left = 8
          Top = 16
          Width = 153
          Height = 20
          Hint = 'Detect location of events on selected source channel'
          Caption = 'Detect Events'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnClick = bDetectEventsClick
        end
        object GroupBox10: TGroupBox
          Left = 8
          Top = 70
          Width = 153
          Height = 257
          Caption = ' Detection Channel '
          TabOrder = 2
          object cbDetectionSource: TComboBox
            Left = 4
            Top = 15
            Width = 141
            Height = 22
            Hint = 'Signal channel to be monitored for event'
            Style = csDropDownList
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = cbDetectionSourceChange
          end
          object DetSettingsPage: TPageControl
            Left = 8
            Top = 125
            Width = 140
            Height = 130
            ActivePage = detFluorSettingsTab
            Style = tsFlatButtons
            TabOrder = 1
            object detFluorSettingsTab: TTabSheet
              Caption = 'detFluorSettingsTab'
              TabVisible = False
              object Label2: TLabel
                Left = 4
                Top = 2
                Width = 45
                Height = 14
                Alignment = taRightJustify
                Caption = 'Excit. Ch.'
              end
              object cbDetFluor: TComboBox
                Left = 56
                Top = 2
                Width = 77
                Height = 22
                Hint = 'Frame type to be displayed'
                Style = csDropDownList
                TabOrder = 0
                OnChange = cbDetFluorChange
              end
            end
            object DetFluorRatioSettingsTab: TTabSheet
              Caption = 'DetFluorRatioSettingsTab'
              ImageIndex = 1
              TabVisible = False
              object Label14: TLabel
                Left = 25
                Top = 0
                Width = 24
                Height = 42
                Alignment = taRightJustify
                Caption = 'Exc. Ch. Ratio'
                WordWrap = True
              end
              object Shape1: TShape
                Left = 56
                Top = 28
                Width = 77
                Height = 2
              end
              object Label20: TLabel
                Left = 26
                Top = 62
                Width = 51
                Height = 28
                Alignment = taRightJustify
                Caption = 'Min. Signal Level'
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                WordWrap = True
              end
              object Label21: TLabel
                Left = 16
                Top = 94
                Width = 61
                Height = 14
                Alignment = taRightJustify
                Caption = 'Display Max.'
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                WordWrap = True
              end
              object cbDetFluorRatioTop: TComboBox
                Left = 56
                Top = 0
                Width = 77
                Height = 22
                Hint = 'Fluorescence ratio numerator'
                Style = csDropDownList
                ParentShowHint = False
                ShowHint = True
                TabOrder = 0
                OnChange = cbDetFluorRatioTopChange
              end
              object cbDetFluorRatioBottom: TComboBox
                Left = 56
                Top = 34
                Width = 77
                Height = 22
                Hint = 'Fluorescence ration denominator'
                Style = csDropDownList
                ParentShowHint = False
                ShowHint = True
                TabOrder = 1
                OnChange = cbDetFluorRatioBottomChange
              end
              object eddetRatioexclusionThreshold: TValidatedEdit
                Left = 80
                Top = 62
                Width = 52
                Height = 20
                Hint = 'Minimum signal level included in ratio'
                OnKeyPress = eddetRatioexclusionThresholdKeyPress
                AutoSize = False
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = []
                ShowHint = True
                Text = ' 0 '
                Scale = 1.000000000000000000
                NumberFormat = '%.0f'
                LoLimit = -1.000000015047466E30
                HiLimit = 1.000000015047466E30
              end
              object edDetRatioDisplayMax: TValidatedEdit
                Left = 80
                Top = 94
                Width = 52
                Height = 20
                Hint = 'Upper limit of display range'
                OnKeyPress = edDetRatioDisplayMaxKeyPress
                AutoSize = False
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = []
                ShowHint = True
                Text = ' 10 '
                Value = 10.000000000000000000
                Scale = 1.000000000000000000
                NumberFormat = '%.4g'
                LoLimit = -1.000000015047466E30
                HiLimit = 1.000000015047466E30
              end
            end
            object DetADCSettingsTab: TTabSheet
              Caption = 'DetADCSettingsTab'
              ImageIndex = 2
              TabVisible = False
            end
          end
          object DetROIGrp: TGroupBox
            Left = 6
            Top = 40
            Width = 138
            Height = 80
            Caption = ' Region of Interest'
            TabOrder = 2
            object Label13: TLabel
              Left = 17
              Top = 18
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
            object Label12: TLabel
              Left = 25
              Top = 46
              Width = 12
              Height = 23
              Alignment = taRightJustify
              Caption = '-'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -20
              Font.Name = 'Courier New'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbDetROI: TComboBox
              Left = 41
              Top = 18
              Width = 87
              Height = 22
              Hint = 'Region of interest to be displayed'
              Style = csDropDownList
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              OnChange = cbDetROIChange
            end
            object cbDetBackgROI: TComboBox
              Left = 41
              Top = 46
              Width = 87
              Height = 22
              Hint = 'Background region of interest to be subtracted '
              Style = csDropDownList
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              OnClick = cbDetBackgROIClick
            end
          end
        end
        object bStop: TButton
          Left = 125
          Top = 40
          Width = 36
          Height = 17
          Caption = 'Stop'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
          OnClick = bStopClick
        end
        object edDetectStatus: TEdit
          Left = 10
          Top = 40
          Width = 109
          Height = 22
          ReadOnly = True
          TabOrder = 4
        end
      end
      object detDisplayGrp: TGroupBox
        Left = 174
        Top = 0
        Width = 473
        Height = 553
        TabOrder = 1
        object scDetDisplay: TScopeDisplay
          Left = 13
          Top = 17
          Width = 457
          Height = 193
          OnCursorChange = scDetDisplayCursorChange
          CursorChangeInProgress = False
          NumChannels = 1
          NumPoints = 1
          MaxPoints = 1024
          XMin = 0
          XMax = 1023
          XOffset = 0
          CursorsEnabled = True
          TScale = 1.000000000000000000
          TUnits = 's'
          TCalBar = -1.000000000000000000
          ZoomDisableHorizontal = True
          ZoomDisableVertical = False
          DisableChannelVisibilityButton = False
          PrinterFontSize = 0
          PrinterPenWidth = 0
          PrinterLeftMargin = 0
          PrinterRightMargin = 0
          PrinterTopMargin = 0
          PrinterBottomMargin = 0
          PrinterDisableColor = False
          PrinterShowLabels = True
          PrinterShowZeroLevels = True
          MetafileWidth = 0
          MetafileHeight = 0
          StorageMode = False
          RecordNumber = -1
          DisplayGrid = True
          MaxADCValue = 2047
          MinADCValue = -2048
          NumBytesPerSample = 2
          FixZeroLevels = False
          DisplaySelected = True
          FontSize = 8
        end
        object panDetDisplay: TPanel
          Left = 352
          Top = 218
          Width = 113
          Height = 23
          BevelOuter = bvNone
          TabOrder = 0
          object edTDetDisplay: TValidatedEdit
            Left = 20
            Top = 1
            Width = 65
            Height = 19
            Hint = 'Display window duration'
            OnKeyPress = edTDetDisplayKeyPress
            AutoSize = False
            ShowHint = True
            Text = ' 5 s'
            Value = 5.000000000000000000
            Scale = 1.000000000000000000
            Units = 's'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
          object bDetDisplayDouble: TButton
            Left = 86
            Top = 2
            Width = 16
            Height = 18
            Caption = '4'
            Font.Charset = SYMBOL_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Webdings'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = bDetDisplayDoubleClick
          end
          object bTDetDisplayHalf: TButton
            Left = 8
            Top = 2
            Width = 11
            Height = 18
            Caption = '3'
            Font.Charset = SYMBOL_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Webdings'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
            OnClick = bTDetDisplayHalfClick
          end
        end
        object sbDetDisplay: TScrollBar
          Left = 8
          Top = 216
          Width = 337
          Height = 17
          PageSize = 0
          TabOrder = 1
          OnChange = sbDetDisplayChange
        end
      end
    end
    object ViewTab: TTabSheet
      Caption = 'View Events'
      object ViewGrp: TGroupBox
        Left = 0
        Top = 0
        Width = 169
        Height = 545
        TabOrder = 0
        object ControlGrp: TGroupBox
          Left = 8
          Top = 12
          Width = 153
          Height = 85
          Caption = ' Event No. '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object ckEventRejected: TCheckBox
            Left = 74
            Top = 60
            Width = 69
            Height = 12
            Hint = 'Accept/Reject event for averaging or plotting'
            Alignment = taLeftJustify
            Caption = 'Rejected'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 3
            OnClick = ckEventRejectedClick
          end
          object sbEventNum: TScrollBar
            Left = 8
            Top = 38
            Width = 137
            Height = 16
            Hint = 'Select frame number to be displayed'
            Min = 1
            PageSize = 0
            Position = 1
            TabOrder = 0
            OnChange = sbEventNumChange
          end
          object edEventNum: TRangeEdit
            Left = 8
            Top = 16
            Width = 65
            Height = 18
            Hint = 'Frame number on display'
            OnKeyPress = edEventNumKeyPress
            AutoSize = False
            Text = ' 1 / 1 '
            LoValue = 1.000000000000000000
            HiValue = 1.000000000000000000
            HiLimit = 1.000000015047466E30
            Scale = 1.000000000000000000
            NumberFormat = '%.0f / %.0f'
          end
          object edEventTime: TEdit
            Left = 76
            Top = 16
            Width = 69
            Height = 18
            Hint = 'Time of detection of displayed event'
            AutoSize = False
            ReadOnly = True
            TabOrder = 1
          end
        end
        object GroupBox3: TGroupBox
          Left = 8
          Top = 96
          Width = 153
          Height = 73
          Caption = ' Region of Interest  '
          TabOrder = 1
          object Label9: TLabel
            Left = 37
            Top = 42
            Width = 12
            Height = 23
            Caption = '-'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -20
            Font.Name = 'Courier New'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label17: TLabel
            Left = 33
            Top = 18
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
          object cbSource: TComboBox
            Left = 57
            Top = 18
            Width = 87
            Height = 22
            Hint = 'Region of interest to be displayed'
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbSourceChange
          end
          object cbBackground: TComboBox
            Left = 58
            Top = 42
            Width = 87
            Height = 22
            Hint = 'Background region of interest to be subtracted '
            Style = csDropDownList
            TabOrder = 1
            OnChange = cbBackgroundChange
          end
        end
        object FluorGrp: TGroupBox
          Left = 8
          Top = 174
          Width = 153
          Height = 275
          Caption = ' Fluorescence Display  '
          TabOrder = 2
          object DisplayModePage: TPageControl
            Left = 6
            Top = 41
            Width = 145
            Height = 232
            ActivePage = FTab
            Style = tsFlatButtons
            TabHeight = 18
            TabOrder = 0
            OnChange = DisplayModePageChange
            OnChanging = DisplayModePageChanging
            object FTab: TTabSheet
              Caption = 'Fluor'
              TabVisible = False
              object Label3: TLabel
                Left = 5
                Top = 2
                Width = 44
                Height = 14
                Alignment = taRightJustify
                Caption = 'Excit. ch.'
              end
              object cbWavelength: TComboBox
                Left = 56
                Top = 2
                Width = 81
                Height = 22
                Hint = 'Frame type to be displayed'
                Style = csDropDownList
                TabOrder = 0
                OnChange = cbWavelengthChange
              end
            end
            object dFTab: TTabSheet
              Caption = 'F/F0'
              ImageIndex = 1
              TabVisible = False
              object Label5: TLabel
                Left = 5
                Top = 2
                Width = 44
                Height = 14
                Alignment = taRightJustify
                Caption = 'Excit. ch.'
              end
              object Label19: TLabel
                Left = 12
                Top = 160
                Width = 61
                Height = 14
                Alignment = taRightJustify
                Caption = 'Display Max.'
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                WordWrap = True
              end
              object rbF0FromFrames: TRadioButton
                Left = 0
                Top = 28
                Width = 73
                Height = 17
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
                OnClick = rbF0FromFramesClick
              end
              object edF0Range: TRangeEdit
                Left = 39
                Top = 45
                Width = 95
                Height = 20
                Hint = 'Series of frames from which F0 is computed'
                OnKeyPress = edF0RangeKeyPress
                AutoSize = False
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = []
                ShowHint = True
                Text = ' 1.00 - 1.00 '
                LoValue = 1.000000000000000000
                HiValue = 1.000000000000000000
                LoLimit = 1.000000000000000000
                HiLimit = 1.000000015047466E30
                Scale = 1.000000000000000000
                NumberFormat = '%.f - %.f'
              end
              object rbF0Constant: TRadioButton
                Left = 0
                Top = 73
                Width = 49
                Height = 17
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
                OnClick = rbF0ConstantClick
              end
              object edF0Constant: TValidatedEdit
                Left = 48
                Top = 73
                Width = 87
                Height = 20
                Hint = 'Constant F0 value'
                OnKeyPress = s
                AutoSize = False
                ShowHint = True
                Text = ' 1.0 '
                Value = 1.000000000000000000
                Scale = 1.000000000000000000
                NumberFormat = '%.1f'
                LoLimit = 1.000000000000000000
                HiLimit = 10000000000.000000000000000000
              end
              object cbdFWavelength: TComboBox
                Left = 55
                Top = 2
                Width = 80
                Height = 22
                Hint = 'Frame type to be displayed'
                Style = csDropDownList
                TabOrder = 4
                OnChange = cbWavelengthChange
              end
              object GroupBox1: TGroupBox
                Left = 0
                Top = 102
                Width = 137
                Height = 51
                TabOrder = 5
                object rbDFOverF0: TRadioButton
                  Left = 7
                  Top = 11
                  Width = 66
                  Height = 22
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
                  OnClick = rbDFOverF0Click
                end
                object rbFOverF0: TRadioButton
                  Left = 7
                  Top = 27
                  Width = 57
                  Height = 21
                  Hint = 'Plot fluorescence as fraction of  F0'
                  Caption = 'F/F0'
                  Font.Charset = ANSI_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -12
                  Font.Name = 'Arial'
                  Font.Style = [fsBold]
                  ParentFont = False
                  ParentShowHint = False
                  ShowHint = True
                  TabOrder = 1
                  OnClick = rbFOverF0Click
                end
              end
              object edDFDisplayMax: TValidatedEdit
                Left = 80
                Top = 160
                Width = 56
                Height = 20
                Hint = 'Upper limit of display range'
                OnKeyPress = edTFLDisplayKeyPress
                AutoSize = False
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = []
                ShowHint = True
                Text = ' 2 '
                Value = 2.000000000000000000
                Scale = 1.000000000000000000
                NumberFormat = '%.4g'
                LoLimit = 0.000000000100000001
                HiLimit = 1.000000015047466E30
              end
            end
            object RatioTab: TTabSheet
              Caption = 'Ratio'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ImageIndex = 2
              ParentFont = False
              TabVisible = False
              object Label4: TLabel
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
              object Shape2: TShape
                Left = 56
                Top = 27
                Width = 79
                Height = 2
              end
              object Label1: TLabel
                Left = 27
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
              object Label18: TLabel
                Left = 12
                Top = 196
                Width = 61
                Height = 14
                Alignment = taRightJustify
                Caption = 'Display Max.'
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                WordWrap = True
              end
              object cbNumWave: TComboBox
                Left = 56
                Top = 0
                Width = 79
                Height = 23
                Hint = 'Numerator frame type '
                Style = csDropDownList
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
                OnChange = cbNumWaveChange
              end
              object cbDenomWave: TComboBox
                Left = 56
                Top = 32
                Width = 80
                Height = 23
                Hint = 'Denominator frame type'
                Style = csDropDownList
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
                OnChange = cbDenomWaveChange
              end
              object edRatioExclusionThreshold: TValidatedEdit
                Left = 80
                Top = 60
                Width = 55
                Height = 20
                Hint = 'Constant F0 value'
                OnKeyPress = edRatioExclusionThresholdKeyPress
                AutoSize = False
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = []
                Text = ' 0 '
                Scale = 1.000000000000000000
                NumberFormat = '%.0f'
                LoLimit = -1.000000015047466E30
                HiLimit = 1.000000015047466E30
              end
              object GroupBox8: TGroupBox
                Left = 1
                Top = 90
                Width = 136
                Height = 100
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentFont = False
                TabOrder = 3
                object edRatioRMax: TValidatedEdit
                  Left = 52
                  Top = 30
                  Width = 77
                  Height = 20
                  Hint = 'Constant F0 value'
                  OnKeyPress = edRatioRMaxKeyPress
                  AutoSize = False
                  Font.Charset = ANSI_CHARSET
                  Font.Color = clWindowText
                  Font.Height = -12
                  Font.Name = 'Arial'
                  Font.Style = []
                  Text = ' 1E-010 '
                  Value = 0.000000000100000001
                  Scale = 1.000000000000000000
                  NumberFormat = '%.3g'
                  LoLimit = 0.000000000100000001
                  HiLimit = 1000.000000000000000000
                end
                object cbEquation: TComboBox
                  Left = 56
                  Top = 60
                  Width = 73
                  Height = 23
                  Hint = 'Binding equation used to compute ion concentration from ratio'
                  Style = csDropDownList
                  TabOrder = 1
                end
                object ckUseEquation: TCheckBox
                  Left = 7
                  Top = 60
                  Width = 43
                  Height = 21
                  Caption = 'Eqn.'
                  TabOrder = 2
                  OnClick = ckUseEquationClick
                end
                object ckDivideByRMax: TCheckBox
                  Left = 7
                  Top = 14
                  Width = 71
                  Height = 15
                  Caption = '/ Rmax  ='
                  TabOrder = 3
                  OnClick = ckDivideByRMaxClick
                end
              end
              object edRatioDisplayMax: TValidatedEdit
                Left = 88
                Top = 196
                Width = 48
                Height = 20
                OnKeyPress = edRatioDisplayMaxKeyPress
                AutoSize = False
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = []
                Text = ' 2 '
                Value = 2.000000000000000000
                Scale = 1.000000000000000000
                NumberFormat = '%.4g'
                LoLimit = 0.000000000100000001
                HiLimit = 1.000000015047466E30
              end
            end
          end
          object cbFluorescence: TComboBox
            Left = 8
            Top = 16
            Width = 137
            Height = 22
            TabOrder = 1
            Text = 'cbFluorescence'
            OnChange = cbFluorescenceChange
          end
        end
        object ExportGrp: TGroupBox
          Left = 8
          Top = 452
          Width = 153
          Height = 65
          Caption = ' Export '
          TabOrder = 3
          object bExportADC: TButton
            Left = 8
            Top = 16
            Width = 137
            Height = 17
            Hint = 'Export analogue channels of displayed events'
            Caption = 'Export A/D Channels'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = bExportADCClick
          end
          object bExportFL: TButton
            Left = 8
            Top = 38
            Width = 137
            Height = 17
            Hint = 'Export displayed fluorescence signal of detected events'
            Caption = 'Export Fluorescence'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = bExportFLClick
          end
        end
      end
      object DisplayGrp: TGroupBox
        Left = 174
        Top = 0
        Width = 481
        Height = 497
        TabOrder = 1
        object scFLDisplay: TScopeDisplay
          Left = 8
          Top = 16
          Width = 465
          Height = 193
          OnMouseDown = scFLDisplayMouseDown
          OnCursorChange = scFLDisplayCursorChange
          CursorChangeInProgress = False
          NumChannels = 1
          NumPoints = 1
          MaxPoints = 1024
          XMin = 0
          XMax = 1023
          XOffset = 0
          CursorsEnabled = True
          TScale = 1.000000000000000000
          TUnits = 's'
          TCalBar = -1.000000000000000000
          ZoomDisableHorizontal = True
          ZoomDisableVertical = False
          DisableChannelVisibilityButton = False
          PrinterFontSize = 0
          PrinterPenWidth = 0
          PrinterLeftMargin = 0
          PrinterRightMargin = 0
          PrinterTopMargin = 0
          PrinterBottomMargin = 0
          PrinterDisableColor = False
          PrinterShowLabels = True
          PrinterShowZeroLevels = True
          MetafileWidth = 0
          MetafileHeight = 0
          StorageMode = False
          RecordNumber = -1
          DisplayGrid = True
          MaxADCValue = 2047
          MinADCValue = -2048
          NumBytesPerSample = 2
          FixZeroLevels = False
          DisplaySelected = True
          FontSize = 8
        end
        object scADCDisplay: TScopeDisplay
          Left = 13
          Top = 239
          Width = 465
          Height = 193
          OnMouseDown = scADCDisplayMouseDown
          OnCursorChange = scADCDisplayCursorChange
          CursorChangeInProgress = False
          NumChannels = 1
          NumPoints = 1
          MaxPoints = 1024
          XMin = 0
          XMax = 1023
          XOffset = 0
          CursorsEnabled = True
          TScale = 1.000000000000000000
          TUnits = 's'
          TCalBar = -1.000000000000000000
          ZoomDisableHorizontal = True
          ZoomDisableVertical = False
          DisableChannelVisibilityButton = False
          PrinterFontSize = 0
          PrinterPenWidth = 0
          PrinterLeftMargin = 0
          PrinterRightMargin = 0
          PrinterTopMargin = 0
          PrinterBottomMargin = 0
          PrinterDisableColor = False
          PrinterShowLabels = True
          PrinterShowZeroLevels = True
          MetafileWidth = 0
          MetafileHeight = 0
          StorageMode = False
          RecordNumber = -1
          DisplayGrid = True
          MaxADCValue = 2047
          MinADCValue = -2048
          NumBytesPerSample = 2
          FixZeroLevels = False
          DisplaySelected = True
          FontSize = 8
        end
        object ckSuperimposeEvents: TCheckBox
          Left = 8
          Top = 456
          Width = 129
          Height = 17
          Caption = 'Superimpose Events'
          TabOrder = 0
          OnClick = ckSuperimposeEventsClick
        end
        object panFLDisplay: TPanel
          Left = 160
          Top = 210
          Width = 313
          Height = 23
          BevelOuter = bvNone
          TabOrder = 1
          object edTFLDisplay: TValidatedEdit
            Left = 228
            Top = 1
            Width = 65
            Height = 19
            OnKeyPress = edTFLDisplayKeyPress
            AutoSize = False
            Text = ' 1 s'
            Value = 1.000000000000000000
            Scale = 1.000000000000000000
            Units = 's'
            NumberFormat = '%.4g'
            LoLimit = 1.000000000000000000
            HiLimit = 2000.000000000000000000
          end
          object bTFLDisplayDouble: TButton
            Left = 294
            Top = 2
            Width = 16
            Height = 18
            Caption = '4'
            Font.Charset = SYMBOL_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Webdings'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = bTFLDisplayDoubleClick
          end
          object bTFLDisplayHalf: TButton
            Left = 216
            Top = 2
            Width = 11
            Height = 18
            Caption = '3'
            Font.Charset = SYMBOL_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Webdings'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
            OnClick = bTFLDisplayHalfClick
          end
        end
        object panADCDisplay: TPanel
          Left = 304
          Top = 450
          Width = 169
          Height = 23
          BevelOuter = bvNone
          TabOrder = 2
          object edTADCDisplay: TValidatedEdit
            Left = 84
            Top = 1
            Width = 65
            Height = 19
            OnKeyPress = edTADCDisplayKeyPress
            AutoSize = False
            Text = ' 1 s'
            Value = 1.000000000000000000
            Scale = 1.000000000000000000
            Units = 's'
            NumberFormat = '%.4g'
            LoLimit = 1.000000000000000000
            HiLimit = 2000.000000000000000000
          end
          object bTADCDisplayDouble: TButton
            Left = 150
            Top = 2
            Width = 16
            Height = 18
            Caption = '4'
            Font.Charset = SYMBOL_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Webdings'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = bTADCDisplayDoubleClick
          end
          object bTADCDisplayHalf: TButton
            Left = 72
            Top = 2
            Width = 11
            Height = 18
            Caption = '3'
            Font.Charset = SYMBOL_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Webdings'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
            OnClick = bTADCDisplayHalfClick
          end
          object ckEnableSeparateADCDisplayDuration: TCheckBox
            Left = 6
            Top = 0
            Width = 57
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Enable'
            TabOrder = 3
            OnClick = ckEnableSeparateADCDisplayDurationClick
          end
        end
      end
    end
    object AverageTab: TTabSheet
      Caption = 'Average Events'
      ImageIndex = 1
      object AvgGrp: TGroupBox
        Left = 0
        Top = 0
        Width = 169
        Height = 481
        TabOrder = 0
        object GroupBox2: TGroupBox
          Left = 8
          Top = 38
          Width = 153
          Height = 83
          Caption = ' Range '
          TabOrder = 0
          object rbAvgAllEvents: TRadioButton
            Left = 8
            Top = 16
            Width = 89
            Height = 17
            Hint = 'Average all detected events'
            Caption = 'All Events'
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
          object rbAvgRange: TRadioButton
            Left = 8
            Top = 32
            Width = 89
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
          object edAvgRange: TRangeEdit
            Left = 24
            Top = 52
            Width = 121
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
        object bAverageEvents: TButton
          Left = 8
          Top = 16
          Width = 153
          Height = 20
          Hint = 'Display average of selected range of events'
          Caption = 'Average Events'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = bAverageEventsClick
        end
      end
      object AvgDisplayGrp: TGroupBox
        Left = 174
        Top = 0
        Width = 481
        Height = 529
        TabOrder = 1
        object scAvgFlDisplay: TScopeDisplay
          Left = 13
          Top = 16
          Width = 465
          Height = 193
          OnMouseDown = scAvgFlDisplayMouseDown
          OnCursorChange = scAvgFlDisplayCursorChange
          CursorChangeInProgress = False
          NumChannels = 1
          NumPoints = 1
          MaxPoints = 1024
          XMin = 0
          XMax = 1023
          XOffset = 0
          CursorsEnabled = True
          TScale = 1.000000000000000000
          TUnits = 's'
          TCalBar = -1.000000000000000000
          ZoomDisableHorizontal = True
          ZoomDisableVertical = False
          DisableChannelVisibilityButton = False
          PrinterFontSize = 0
          PrinterPenWidth = 0
          PrinterLeftMargin = 0
          PrinterRightMargin = 0
          PrinterTopMargin = 0
          PrinterBottomMargin = 0
          PrinterDisableColor = False
          PrinterShowLabels = True
          PrinterShowZeroLevels = True
          MetafileWidth = 0
          MetafileHeight = 0
          StorageMode = False
          RecordNumber = -1
          DisplayGrid = False
          MaxADCValue = 2047
          MinADCValue = -2048
          NumBytesPerSample = 2
          FixZeroLevels = False
          DisplaySelected = True
          FontSize = 8
        end
        object scAvgADCDisplay: TScopeDisplay
          Left = 8
          Top = 256
          Width = 465
          Height = 193
          OnMouseDown = scAvgADCDisplayMouseDown
          OnCursorChange = scAvgADCDisplayCursorChange
          CursorChangeInProgress = False
          NumChannels = 1
          NumPoints = 1
          MaxPoints = 1024
          XMin = 0
          XMax = 1023
          XOffset = 0
          CursorsEnabled = True
          TScale = 1.000000000000000000
          TUnits = 's'
          TCalBar = -1.000000000000000000
          ZoomDisableHorizontal = True
          ZoomDisableVertical = False
          DisableChannelVisibilityButton = False
          PrinterFontSize = 0
          PrinterPenWidth = 0
          PrinterLeftMargin = 0
          PrinterRightMargin = 0
          PrinterTopMargin = 0
          PrinterBottomMargin = 0
          PrinterDisableColor = False
          PrinterShowLabels = True
          PrinterShowZeroLevels = True
          MetafileWidth = 0
          MetafileHeight = 0
          StorageMode = False
          RecordNumber = -1
          DisplayGrid = False
          MaxADCValue = 2047
          MinADCValue = -2048
          NumBytesPerSample = 2
          FixZeroLevels = False
          DisplaySelected = True
          FontSize = 8
        end
      end
    end
    object PlotTab: TTabSheet
      Caption = 'Plot Graphs'
      ImageIndex = 2
      object PlotGrp: TGroupBox
        Left = -1
        Top = 0
        Width = 169
        Height = 481
        TabOrder = 0
        object GroupBox6: TGroupBox
          Left = 8
          Top = 94
          Width = 153
          Height = 83
          Caption = ' Range '
          TabOrder = 0
          object rbPlotAllEvents: TRadioButton
            Left = 8
            Top = 16
            Width = 89
            Height = 17
            Hint = 'Plot all events in file'
            Caption = 'All Events'
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
          object rbPlotRange: TRadioButton
            Left = 8
            Top = 32
            Width = 89
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
          object edPlotRange: TRangeEdit
            Left = 24
            Top = 52
            Width = 121
            Height = 20
            Hint = 'Sub-range of events to be plotted'
            AutoSize = False
            Text = ' 1 - 1 '
            LoValue = 1.000000000000000000
            HiValue = 1.000000000000000000
            HiLimit = 1.000000015047466E30
            Scale = 1.000000000000000000
            NumberFormat = '%.0f - %.0f'
          end
        end
        object bPlotGraph: TButton
          Left = 8
          Top = 16
          Width = 153
          Height = 20
          Hint = 'Plot X-Y graph of selected measurements from range of events'
          Caption = 'Plot Graph'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnClick = bPlotGraphClick
        end
        object bSetAxes: TButton
          Left = 8
          Top = 68
          Width = 153
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
        object XGrp: TGroupBox
          Left = 8
          Top = 180
          Width = 153
          Height = 145
          Caption = ' X Axis Variable '
          TabOrder = 3
          object Label6: TLabel
            Left = 8
            Top = 48
            Width = 24
            Height = 14
            Caption = 'From'
          end
          object cbXVar: TComboBox
            Left = 8
            Top = 19
            Width = 137
            Height = 22
            Hint = 'X Axis: Waveform measurement to be plotted'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            Text = 'cbXVar'
            OnChange = cbXVarChange
          end
          object cbXSource: TComboBox
            Left = 40
            Top = 45
            Width = 105
            Height = 22
            Hint = 'Signal channel to be measured'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            Text = 'cbXVar'
            OnChange = cbXSourceChange
          end
          object rgXPeakPolarity: TRadioGroup
            Left = 8
            Top = 66
            Width = 137
            Height = 67
            Caption = ' Polarity '
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ItemIndex = 0
            Items.Strings = (
              'Positive'
              'Negative'
              'Absolute')
            ParentFont = False
            TabOrder = 2
          end
          object panXNumAvg: TPanel
            Left = 8
            Top = 72
            Width = 123
            Height = 25
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = 'Points Avg'#39'd'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            object edXNumAvg: TValidatedEdit
              Left = 88
              Top = 2
              Width = 33
              Height = 20
              Hint = 'Number of points averaged'
              OnKeyPress = edTFLDisplayKeyPress
              AutoSize = False
              ShowHint = True
              Text = '  1 '
              Value = 1.000000000000000000
              Scale = 1.000000000000000000
              NumberFormat = '%2.0f'
              LoLimit = 1.000000000000000000
              HiLimit = 1.000000015047466E30
            end
          end
        end
        object GroupBox5: TGroupBox
          Left = 8
          Top = 330
          Width = 153
          Height = 145
          Caption = ' Y Axis Variable '
          TabOrder = 4
          object Label8: TLabel
            Left = 8
            Top = 48
            Width = 24
            Height = 14
            Caption = 'From'
          end
          object cbYVar: TComboBox
            Left = 8
            Top = 19
            Width = 137
            Height = 22
            TabOrder = 0
            Text = 'cbXVar'
            OnChange = cbYVarChange
          end
          object cbYSource: TComboBox
            Left = 40
            Top = 45
            Width = 105
            Height = 22
            TabOrder = 1
            Text = 'cbXVar'
            OnChange = cbYSourceChange
          end
          object rgYPeakPolarity: TRadioGroup
            Left = 8
            Top = 66
            Width = 137
            Height = 67
            Caption = ' Polarity '
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ItemIndex = 0
            Items.Strings = (
              'Positive'
              'Negative'
              'Absolute')
            ParentFont = False
            TabOrder = 2
          end
          object panYNumAvg: TPanel
            Left = 8
            Top = 72
            Width = 123
            Height = 25
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = 'Points Avg'#39'd'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            object edYNumAvg: TValidatedEdit
              Left = 88
              Top = 2
              Width = 33
              Height = 20
              Hint = 'Number of points averaged'
              OnKeyPress = edTFLDisplayKeyPress
              AutoSize = False
              ShowHint = True
              Text = '  1 '
              Value = 1.000000000000000000
              Scale = 1.000000000000000000
              NumberFormat = '%2.0f'
              LoLimit = 1.000000000000000000
              HiLimit = 1.000000015047466E30
            end
          end
        end
        object bPlotGraphStop: TButton
          Left = 110
          Top = 40
          Width = 50
          Height = 17
          Caption = 'Stop'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 5
          OnClick = bPlotGraphStopClick
        end
        object edPlotStatus: TEdit
          Left = 8
          Top = 40
          Width = 96
          Height = 22
          ReadOnly = True
          TabOrder = 6
        end
      end
      object PlotDisplayGrp: TGroupBox
        Left = 174
        Top = 0
        Width = 505
        Height = 481
        TabOrder = 1
        object plPlot: TXYPlotDisplay
          Left = 8
          Top = 16
          Width = 489
          Height = 417
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
      end
    end
  end
end
