object ViewLineFrm: TViewLineFrm
  Left = 364
  Top = 126
  Width = 840
  Height = 812
  Caption = 'View Line Scans'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object ControlsGrp: TGroupBox
    Left = 8
    Top = -8
    Width = 143
    Height = 641
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object ControlGrp: TGroupBox
      Left = 8
      Top = 8
      Width = 129
      Height = 153
      Caption = ' Line Scan  '
      TabOrder = 0
      object Label7: TLabel
        Left = 8
        Top = 44
        Width = 55
        Height = 15
        Caption = 'Start Line'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label1: TLabel
        Left = 8
        Top = 67
        Width = 101
        Height = 15
        Caption = 'Line Scan Interval'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label9: TLabel
        Left = 8
        Top = 104
        Width = 63
        Height = 15
        Caption = 'Initial delay'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object edStartLine: TValidatedEdit
        Left = 64
        Top = 44
        Width = 57
        Height = 18
        OnKeyPress = edStartLineKeyPress
        AutoSize = False
        Text = ' 0 '
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = 0.100000001490116100
        HiLimit = 1.000000015047466E29
      end
      object edLineScanInterval: TValidatedEdit
        Left = 8
        Top = 82
        Width = 113
        Height = 20
        OnKeyPress = edLineScanIntervalKeyPress
        AutoSize = False
        Text = ' 0.1 ms'
        Value = 0.000100000004749745
        Scale = 1000.000000000000000000
        Units = 'ms'
        NumberFormat = '%.5g'
        LoLimit = 0.000099999997473788
        HiLimit = 1.000000015047466E29
      end
      object edImageStartDelay: TValidatedEdit
        Left = 8
        Top = 120
        Width = 113
        Height = 20
        OnKeyPress = edImageStartDelayKeyPress
        AutoSize = False
        Text = ' 0 ms'
        Scale = 1000.000000000000000000
        Units = 'ms'
        NumberFormat = '%.5g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
      object cbFrameNum: TComboBox
        Left = 8
        Top = 16
        Width = 113
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        TabOrder = 3
        OnChange = cbFrameNumChange
      end
    end
    object DisplayGrp: TGroupBox
      Left = 8
      Top = 160
      Width = 129
      Height = 161
      Caption = 'Display  '
      TabOrder = 1
      object Label5: TLabel
        Left = 8
        Top = 73
        Width = 85
        Height = 15
        Caption = 'Intensity Range'
      end
      object Label6: TLabel
        Left = 21
        Top = 44
        Width = 32
        Height = 15
        Alignment = taRightJustify
        Caption = 'Zoom'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object bOptimiseContrast: TButton
        Left = 8
        Top = 108
        Width = 113
        Height = 17
        Hint = 'Set display look-up table for maximum contrast'
        Caption = 'Best Contrast'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = bOptimiseContrastClick
      end
      object bFullScale: TButton
        Left = 8
        Top = 88
        Width = 113
        Height = 17
        Hint = 'Set display look-up table to full range of camera'
        Caption = 'Full Range'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = bFullScaleClick
      end
      object edDisplayIntensityRange: TRangeEdit
        Left = 8
        Top = 129
        Width = 113
        Height = 18
        Hint = 'Set working range display look-up table range manually'
        OnKeyPress = edDisplayIntensityRangeKeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 4096 - 4096 '
        LoValue = 4096.000000000000000000
        HiValue = 4096.000000000000000000
        HiLimit = 1.000000015047466E30
        Scale = 1.000000000000000000
        NumberFormat = '%.f - %.f'
      end
      object cbPalette: TComboBox
        Left = 8
        Top = 18
        Width = 113
        Height = 23
        Hint = 'Display colour mapping'
        Style = csDropDownList
        ItemHeight = 15
        TabOrder = 3
        OnChange = cbPaletteChange
      end
      object cbDisplayZoom: TComboBox
        Left = 56
        Top = 44
        Width = 65
        Height = 23
        Hint = 'Set display magification '
        Style = csDropDownList
        ItemHeight = 15
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnChange = cbDisplayZoomChange
      end
    end
    object TimeCourseGrp: TGroupBox
      Left = 8
      Top = 320
      Width = 129
      Height = 121
      Caption = ' Time Course Pixel '
      TabOrder = 2
      object Label2: TLabel
        Left = 46
        Top = 16
        Width = 31
        Height = 15
        Alignment = taRightJustify
        Caption = ' Pixel'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label4: TLabel
        Left = 25
        Top = 40
        Width = 52
        Height = 15
        Alignment = taRightJustify
        Caption = 'No. Avgd.'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Shape1: TShape
        Left = 8
        Top = 66
        Width = 115
        Height = 1
      end
      object Label8: TLabel
        Left = 7
        Top = 92
        Width = 69
        Height = 15
        Alignment = taRightJustify
        Caption = 'Background'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object edTimeCoursePixel: TValidatedEdit
        Left = 80
        Top = 16
        Width = 41
        Height = 18
        Hint = 'Centre pixel of region selected for time course plot'
        OnKeyPress = edTimeCoursePixelKeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = 1.000000000000000000
        HiLimit = 1.000000015047466E29
      end
      object edNumPixelsAvg: TValidatedEdit
        Left = 80
        Top = 40
        Width = 41
        Height = 19
        Hint = 'No, of line pixels averaged to compute intensity time course'
        OnKeyPress = edNumPixelsAvgKeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = 1.000000000000000000
        HiLimit = 1.000000015047466E29
      end
      object ckSubtractBackground: TCheckBox
        Left = 8
        Top = 72
        Width = 105
        Height = 17
        Hint = 'Subtract background intensity from time course'
        Caption = 'Subtr. Backg.'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = ckSubtractBackgroundClick
      end
      object edBackgroundPixel: TValidatedEdit
        Left = 80
        Top = 92
        Width = 41
        Height = 18
        Hint = 
          'Centre pixel of line region selected as background intensity tim' +
          'e course'
        OnKeyPress = edTimeCoursePixelKeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = 1.000000000000000000
        HiLimit = 1.000000015047466E29
      end
    end
    object RatioGrp: TGroupBox
      Left = 8
      Top = 440
      Width = 129
      Height = 193
      Caption = ' Plot '
      TabOrder = 3
      object ckDisplayR: TCheckBox
        Left = 8
        Top = 56
        Width = 89
        Height = 20
        Hint = 'Plot wavelength ratio time course'
        Caption = 'Ratio'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        WordWrap = True
        OnClick = ckDisplayRClick
      end
      object ckDisplayFluorescence: TCheckBox
        Left = 8
        Top = 16
        Width = 97
        Height = 20
        Hint = 'Plot fluorescence intensity time course'
        Caption = 'Fluorescence '
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 1
        WordWrap = True
        OnClick = ckDisplayFluorescenceClick
      end
      object ckDisplayADC: TCheckBox
        Left = 8
        Top = 36
        Width = 100
        Height = 20
        Hint = 'Plot analogue channel time course'
        Caption = 'A/D Channels'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 2
        WordWrap = True
        OnClick = ckDisplayADCClick
      end
      object RatioPanel: TPanel
        Left = 2
        Top = 76
        Width = 121
        Height = 113
        BevelOuter = bvNone
        TabOrder = 3
        object Shape2: TShape
          Left = 18
          Top = 30
          Width = 94
          Height = 2
        end
        object Label10: TLabel
          Left = 9
          Top = 62
          Width = 68
          Height = 15
          Alignment = taRightJustify
          Caption = 'Display Max'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label11: TLabel
          Left = 7
          Top = 83
          Width = 70
          Height = 15
          Alignment = taRightJustify
          Caption = 'Exc. Thresh.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object cbNumerator: TComboBox
          Left = 16
          Top = 4
          Width = 100
          Height = 23
          Hint = 'Ratio numerator wavelength'
          Style = csDropDownList
          ItemHeight = 15
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnChange = cbNumeratorChange
        end
        object cbDenominator: TComboBox
          Left = 16
          Top = 34
          Width = 100
          Height = 23
          Hint = 'Ratio denominator wavelength'
          Style = csDropDownList
          ItemHeight = 15
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnChange = cbDenominatorChange
        end
        object edRDisplayMax: TValidatedEdit
          Left = 79
          Top = 61
          Width = 40
          Height = 19
          Hint = 'Upper limit of ratio plot display range'
          OnKeyPress = edRDisplayMaxKeyPress
          AutoSize = False
          ShowHint = True
          Text = ' 10 '
          Value = 10.000000000000000000
          Scale = 1.000000000000000000
          NumberFormat = '%.4g'
          LoLimit = 0.001000000047497451
          HiLimit = 1.000000015047466E30
        end
        object edRatioThreshold: TValidatedEdit
          Left = 79
          Top = 85
          Width = 40
          Height = 18
          Hint = 
            'Lowest accetable  fluorescence intensity level for denominator w' +
            'avelength'
          OnKeyPress = edRatioThresholdKeyPress
          AutoSize = False
          ShowHint = True
          Text = ' 10 '
          Value = 10.000000000000000000
          Scale = 1.000000000000000000
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
      end
    end
  end
  object IdentGrp: TGroupBox
    Left = 160
    Top = 0
    Width = 401
    Height = 35
    Hint = 'Experiment identification data line'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object Label3: TLabel
      Left = 8
      Top = 11
      Width = 24
      Height = 14
      Caption = 'Expt.'
    end
    object edIdent: TEdit
      Left = 40
      Top = 11
      Width = 353
      Height = 20
      AutoSize = False
      TabOrder = 0
      Text = 'edIdent'
      OnKeyUp = edIdentKeyUp
    end
  end
  object ImageGrp: TGroupBox
    Left = 160
    Top = 36
    Width = 409
    Height = 469
    TabOrder = 2
    object Image: TImage
      Left = 56
      Top = 16
      Width = 329
      Height = 97
      Cursor = crCross
      OnMouseDown = ImageMouseDown
      OnMouseMove = ImageMouseMove
      OnMouseUp = ImageMouseUp
    end
    object scADCDisplay: TScopeDisplay
      Left = 8
      Top = 280
      Width = 393
      Height = 113
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
    object scFLDisplay: TScopeDisplay
      Left = 8
      Top = 136
      Width = 393
      Height = 73
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
    object scRDisplay: TScopeDisplay
      Left = 8
      Top = 224
      Width = 393
      Height = 49
      OnMouseDown = scRDisplayMouseDown
      OnCursorChange = scRDisplayCursorChange
      CursorChangeInProgress = False
      NumChannels = 1
      NumPoints = 0
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
      MaxADCValue = 32768
      MinADCValue = -32768
      NumBytesPerSample = 4
      FixZeroLevels = False
      DisplaySelected = False
      FontSize = 8
    end
    object ckDisplayCalBar: TCheckBox
      Left = 56
      Top = 112
      Width = 154
      Height = 17
      Caption = 'Display calibration bar'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 0
    end
    object ckFixZeroLevels: TCheckBox
      Left = 8
      Top = 432
      Width = 105
      Height = 17
      Caption = 'Fix Zero Levels'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = ckFixZeroLevelsClick
    end
    object sbDisplay: TScrollBar
      Left = 8
      Top = 400
      Width = 393
      Height = 17
      PageSize = 0
      TabOrder = 2
      OnChange = sbDisplayChange
    end
    object TDisplayPanel: TPanel
      Left = 192
      Top = 426
      Width = 209
      Height = 23
      BevelOuter = bvNone
      TabOrder = 3
      object edTDisplay: TValidatedEdit
        Left = 124
        Top = 1
        Width = 65
        Height = 20
        OnKeyPress = edTDisplayKeyPress
        AutoSize = False
        Text = ' 0.01667 min'
        Value = 1.000198006629944000
        Scale = 0.016666699200868610
        Units = 'min'
        NumberFormat = '%.4g'
        LoLimit = 0.050000000745058060
        HiLimit = 2000.000000000000000000
      end
      object rbTDisplayUnitMins: TRadioButton
        Left = 50
        Top = 1
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
        OnClick = rbTDisplayUnitsSecsClick
      end
      object rbTDisplayUnitsSecs: TRadioButton
        Left = 1
        Top = 1
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
        TabOrder = 2
        TabStop = True
        OnClick = rbTDisplayUnitsSecsClick
      end
      object bTDisplayDouble: TButton
        Left = 190
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
        TabOrder = 3
        OnClick = bTDisplayDoubleClick
      end
      object bTDisplayHalf: TButton
        Left = 107
        Top = 2
        Width = 16
        Height = 18
        Caption = '3'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Webdings'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        OnClick = bTDisplayHalfClick
      end
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 55
    OnTimer = TimerTimer
    Left = 176
    Top = 560
  end
end
