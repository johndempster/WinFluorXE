object SnapFrm: TSnapFrm
  Tag = 43
  Left = 809
  Top = 531
  Caption = 'Record Image'
  ClientHeight = 746
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object ControlGrp: TGroupBox
    Left = 2
    Top = 0
    Width = 175
    Height = 745
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object ImageCaptureGrp: TGroupBox
      Left = 8
      Top = 64
      Width = 161
      Height = 169
      Caption = ' Image Capture '
      TabOrder = 0
      object lbReadoutTime: TLabel
        Left = 64
        Top = 38
        Width = 70
        Height = 14
        Caption = 'lbReadoutTime'
      end
      object Label3: TLabel
        Left = 5
        Top = 16
        Width = 52
        Height = 28
        Alignment = taRightJustify
        Caption = 'Exposure Interval'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object Label8: TLabel
        Left = 35
        Top = 140
        Width = 22
        Height = 14
        Alignment = taRightJustify
        Caption = 'Gain'
      end
      object edFrameInterval: TValidatedEdit
        Left = 64
        Top = 16
        Width = 89
        Height = 20
        Hint = 'Time interval between frames'
        OnKeyPress = edFrameIntervalKeyPress
        AutoSize = False
        Text = ' 0 ms'
        Scale = 1000.000000000000000000
        Units = 'ms'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E29
      end
      object cbCameraGain: TComboBox
        Left = 64
        Top = 140
        Width = 89
        Height = 22
        Hint = 'Selected single excitation wavelength'
        Style = csDropDownList
        TabOrder = 1
        OnChange = cbCameraGainChange
      end
      object CCDAreaGrp: TGroupBox
        Left = 8
        Top = 55
        Width = 145
        Height = 82
        Caption = 'CCD Area '
        TabOrder = 2
        object Label4: TLabel
          Left = 100
          Top = 20
          Width = 40
          Height = 29
          AutoSize = False
          Caption = 'Pixel Binning'
          WordWrap = True
        end
        object bFullFrame: TButton
          Left = 6
          Top = 16
          Width = 89
          Height = 17
          Hint = 'Set CCD readout area to whole of CCD'
          Caption = 'Full CCD area'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = bFullFrameClick
        end
        object bSelectedRegion: TButton
          Left = 6
          Top = 36
          Width = 89
          Height = 17
          Hint = 'Set CCD readout area to rectangle indicated on display '
          Caption = 'Use rectangle'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnClick = bSelectedRegionClick
        end
        object edBinFactor: TValidatedEdit
          Left = 100
          Top = 52
          Width = 33
          Height = 20
          Hint = 'Camera CCD binning factor'
          OnKeyPress = edBinFactorKeyPress
          AutoSize = False
          Text = ' 1 '
          Value = 1.000000000000000000
          Scale = 1.000000000000000000
          NumberFormat = '%.3g'
          LoLimit = 1.000000000000000000
          HiLimit = 64.000000000000000000
        end
        object bEnterCCDArea: TButton
          Left = 6
          Top = 56
          Width = 89
          Height = 17
          Hint = 'Specify coordinates of CCD readout area'
          Caption = 'Set CCD area'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
          OnClick = bEnterCCDAreaClick
        end
      end
    end
    object DisplayGrp: TGroupBox
      Left = 8
      Top = 232
      Width = 161
      Height = 193
      Caption = ' Display '
      TabOrder = 1
      object Splitter1: TSplitter
        Left = 2
        Top = 16
        Height = 175
      end
      object cbPalette: TComboBox
        Left = 8
        Top = 16
        Width = 145
        Height = 22
        Hint = 'Display colour palette'
        Style = csDropDownList
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnChange = cbPaletteChange
      end
      object ContrastPage: TPageControl
        Left = 5
        Top = 68
        Width = 150
        Height = 117
        ActivePage = RangeTab
        MultiLine = True
        TabOrder = 1
        object RangeTab: TTabSheet
          Caption = 'Display Contrast'
          object bFullScale: TButton
            Left = 2
            Top = 4
            Width = 71
            Height = 17
            Hint = 'Set display intensity range to cover full camera range'
            Caption = 'Full Range'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            OnClick = bFullScaleClick
          end
          object bMaxContrast: TButton
            Left = 78
            Top = 4
            Width = 59
            Height = 17
            Hint = 'Set display range to min. - max.  intensities within image'
            Caption = 'Best'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = bMaxContrastClick
          end
          object edDisplayIntensityRange: TRangeEdit
            Left = 2
            Top = 24
            Width = 135
            Height = 20
            Hint = 'Range of intensities displayed within image'
            OnKeyPress = edDisplayIntensityRangeKeyPress
            AutoSize = False
            ShowHint = True
            Text = ' 4096.00 - 4096.00 '
            LoValue = 4096.000000000000000000
            HiValue = 4096.000000000000000000
            HiLimit = 1.000000015047466E30
            Scale = 1.000000000000000000
            NumberFormat = '%.f - %.f'
          end
          object ckContrast6SDOnly: TCheckBox
            Left = 2
            Top = 61
            Width = 105
            Height = 17
            Hint = 
              'Set maximum contrast range to 6 standard deviations rather than ' +
              'min-max range'
            Caption = '6 x s.d. only'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
            OnClick = ckContrast6SDOnlyClick
          end
          object ckAutoOptimise: TCheckBox
            Left = 2
            Top = 46
            Width = 119
            Height = 17
            Caption = 'Auto adjust'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 4
            OnClick = ckAutoOptimiseClick
          end
        end
        object SlidersTab: TTabSheet
          Caption = 'Sliders'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label5: TLabel
            Left = 14
            Top = 0
            Width = 49
            Height = 15
            Caption = 'Contrast'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label10: TLabel
            Left = 14
            Top = 40
            Width = 62
            Height = 15
            Caption = 'Brightness'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label11: TLabel
            Left = 2
            Top = 18
            Width = 9
            Height = 19
            Caption = '+'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label13: TLabel
            Left = 2
            Top = 58
            Width = 9
            Height = 19
            Caption = '+'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label12: TLabel
            Left = 132
            Top = 16
            Width = 6
            Height = 19
            Caption = '-'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Bodoni MT Black'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label14: TLabel
            Left = 132
            Top = 56
            Width = 6
            Height = 19
            Caption = '-'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Bodoni MT Black'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object sbContrast: TScrollBar
            Left = 14
            Top = 19
            Width = 113
            Height = 17
            Hint = 'Display contrast control slider'
            LargeChange = 10
            PageSize = 0
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = sbContrastChange
          end
          object sbBrightness: TScrollBar
            Left = 14
            Top = 59
            Width = 113
            Height = 17
            Hint = 'Display brightness control slider'
            LargeChange = 10
            PageSize = 0
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnChange = sbContrastChange
          end
        end
      end
    end
    object ExcitationLightGrp: TGroupBox
      Left = 8
      Top = 576
      Width = 161
      Height = 113
      Caption = ' Excitation Light '
      TabOrder = 2
      object Label1: TLabel
        Left = 8
        Top = 40
        Width = 60
        Height = 14
        Caption = ' Wavelength'
      end
      object GroupBox6: TGroupBox
        Left = 8
        Top = 12
        Width = 145
        Height = 28
        TabOrder = 0
        object rbEXCShutterOpen: TRadioButton
          Left = 8
          Top = 8
          Width = 49
          Height = 17
          Hint = 'Turn excitation light source on'
          Caption = 'On'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = rbEXCShutterOpenClick
        end
        object rbEXCShutterClosed: TRadioButton
          Left = 56
          Top = 8
          Width = 43
          Height = 17
          Hint = 'Turn excitation light source off'
          Caption = 'Off'
          Checked = True
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          TabStop = True
          OnClick = rbEXCShutterClosedClick
        end
      end
      object cbWavelength: TComboBox
        Left = 8
        Top = 56
        Width = 145
        Height = 22
        Hint = 'Selected single excitation wavelength'
        TabOrder = 1
        Text = 'cbWavelength'
        OnChange = cbWavelengthChange
      end
      object bSetLaserIntensity: TButton
        Left = 8
        Top = 84
        Width = 145
        Height = 17
        Hint = 'Set intensity of LED/Laser light source'
        Caption = 'Set Light Intensity'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = bSetLaserIntensityClick
      end
    end
    object SnapGrp: TGroupBox
      Left = 8
      Top = 10
      Width = 161
      Height = 49
      TabOrder = 3
      object bSnapImage: TButton
        Left = 8
        Top = 16
        Width = 145
        Height = 17
        Caption = 'Save Image'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = bSnapImageClick
      end
    end
    object ShadingGrp: TGroupBox
      Left = 8
      Top = 428
      Width = 161
      Height = 146
      Caption = ' Shading Correction '
      TabOrder = 4
      object sbShadeCorShowSettings: TSpeedButton
        Left = 8
        Top = 56
        Width = 16
        Height = 16
        AllowAllUp = True
        GroupIndex = 1
        Flat = True
        Glyph.Data = {
          3E010000424D3E010000000000007600000028000000280000000A0000000100
          040000000000C800000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF00FFFFF00FFFF00FF00FFF
          F00FFFF0000FFFFFF0000FFFFF00FF00FFFF00FF00FFFF00FF00FFFF00FF00FF
          FFF0000FFFFFF0000FFFF00FFFF00FF00FFFF00FFFFF00FFFFFFFF00FFFFFFFF
          00FFFFFFFF00FFFFF00FFFF00FF00FFFF00FFFF0000FFFFFF0000FFFFF00FF00
          FFFF00FF00FFFF00FF00FFFF00FF00FFFFF0000FFFFFF0000FFFF00FFFF00FF0
          0FFFF00FFFFF00FFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF}
        NumGlyphs = 4
        OnClick = sbShadeCorShowSettingsClick
      end
      object ckBackgroundSubtraction: TCheckBox
        Left = 8
        Top = 16
        Width = 145
        Height = 17
        Caption = ' Correction Enabled'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = ckBackgroundSubtractionClick
      end
      object bAcquireBackground: TButton
        Left = 8
        Top = 36
        Width = 145
        Height = 17
        Caption = 'Acquire Backg. Image'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bAcquireBackgroundClick
      end
      object ShadeCorSettingsPanel: TPanel
        Left = 3
        Top = 74
        Width = 152
        Height = 74
        BevelOuter = bvNone
        TabOrder = 2
        object Label2: TLabel
          Left = 8
          Top = 0
          Width = 93
          Height = 14
          Caption = 'Image block size'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label7: TLabel
          Left = 8
          Top = 24
          Width = 95
          Height = 14
          Caption = 'Images averaged'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label15: TLabel
          Left = 8
          Top = 48
          Width = 70
          Height = 14
          Caption = 'Normalise to'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edShadeCorImageBlockSize: TValidatedEdit
          Left = 112
          Top = 0
          Width = 38
          Height = 20
          AutoSize = False
          Text = ' 10 '
          Value = 10.000000000000000000
          Scale = 1.000000000000000000
          NumberFormat = '%.0f'
          LoLimit = 1.000000000000000000
          HiLimit = 200.000000000000000000
        end
        object edShadeCorNumFramesAveraged: TValidatedEdit
          Left = 111
          Top = 24
          Width = 38
          Height = 20
          AutoSize = False
          Text = ' 1 '
          Value = 1.000000000000000000
          Scale = 1.000000000000000000
          NumberFormat = '%.0f'
          LoLimit = 1.000000000000000000
          HiLimit = 1.000000015047466E30
        end
        object cbShadeCorNormalisation: TComboBox
          Left = 96
          Top = 48
          Width = 55
          Height = 22
          TabOrder = 2
          Text = 'cbShadeCorNormalisation'
        end
      end
    end
    object ZStageGrp: TGroupBox
      Left = 8
      Top = 688
      Width = 161
      Height = 49
      Caption = 'Z Axis Position '
      TabOrder = 5
      object edZPosition: TValidatedEdit
        Left = 6
        Top = 16
        Width = 59
        Height = 22
        OnKeyPress = edZPositionKeyPress
        Text = ' 10000 um'
        Value = 10000.000000000000000000
        Scale = 1.000000000000000000
        Units = 'um'
        NumberFormat = '%.5g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object sbZPosition: TScrollBar
        Left = 72
        Top = 16
        Width = 81
        Height = 14
        LargeChange = 10
        PageSize = 0
        TabOrder = 1
        OnChange = sbZPositionChange
      end
    end
  end
  object ImageGrp: TGroupBox
    Left = 182
    Top = 0
    Width = 363
    Height = 339
    Caption = ' Images '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Image1: TImage
      Left = 8
      Top = 48
      Width = 329
      Height = 153
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Label6: TLabel
      Left = 13
      Top = 16
      Width = 27
      Height = 14
      Alignment = taRightJustify
      Caption = 'Zoom'
    end
    object sbXScroll: TScrollBar
      Left = 8
      Top = 210
      Width = 353
      Height = 15
      PageSize = 0
      TabOrder = 0
    end
    object sbYScroll: TScrollBar
      Left = 336
      Top = 24
      Width = 15
      Height = 153
      Kind = sbVertical
      PageSize = 0
      TabOrder = 1
    end
    object ROIPanel: TPanel
      Left = 8
      Top = 232
      Width = 345
      Height = 41
      BevelOuter = bvNone
      TabOrder = 2
      object lbReadout: TLabel
        Left = 0
        Top = 0
        Width = 55
        Height = 14
        Caption = 'lbReadout'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ckDisplayCalBar: TCheckBox
        Left = 0
        Top = 16
        Width = 145
        Height = 17
        Alignment = taLeftJustify
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
    end
    object cbDisplayZoom: TComboBox
      Left = 48
      Top = 16
      Width = 65
      Height = 22
      Hint = 'Display magnification'
      Style = csDropDownList
      TabOrder = 3
      OnChange = cbDisplayZoomChange
    end
  end
  object Timer: TTimer
    Interval = 50
    OnTimer = TimerTimer
    Left = 184
    Top = 352
  end
  object ImageFile: TImageFile
    XResolution = 1.000000000000000000
    YResolution = 1.000000000000000000
    ZResolution = 1.000000000000000000
    TResolution = 1.000000000000000000
    Left = 184
    Top = 392
  end
  object SaveDialog: TSaveDialog
    Left = 184
    Top = 432
  end
end
