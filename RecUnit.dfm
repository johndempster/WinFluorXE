object RecordFrm: TRecordFrm
  Left = 1124
  Top = 26
  Caption = 'Record Images & Signals'
  ClientHeight = 746
  ClientWidth = 811
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShortCut = FormShortCut
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object ImageGrp: TGroupBox
    Left = 246
    Top = 38
    Width = 363
    Height = 339
    Caption = ' Images '
    TabOrder = 0
    object Image1: TImage
      Left = 8
      Top = 44
      Width = 73
      Height = 57
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image2: TImage
      Tag = 1
      Left = 120
      Top = 16
      Width = 73
      Height = 57
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image3: TImage
      Tag = 2
      Left = 8
      Top = 80
      Width = 73
      Height = 49
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image4: TImage
      Tag = 3
      Left = 168
      Top = 16
      Width = 49
      Height = 57
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image5: TImage
      Tag = 1
      Left = 88
      Top = 80
      Width = 73
      Height = 57
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image6: TImage
      Tag = 3
      Left = 176
      Top = 80
      Width = 49
      Height = 57
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image7: TImage
      Tag = 2
      Left = 8
      Top = 144
      Width = 73
      Height = 49
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image8: TImage
      Tag = 1
      Left = 88
      Top = 144
      Width = 73
      Height = 57
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image9: TImage
      Tag = 3
      Left = 176
      Top = 144
      Width = 49
      Height = 57
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Label6: TLabel
      Left = 16
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
      Left = 264
      Top = 24
      Width = 15
      Height = 153
      Kind = sbVertical
      PageSize = 0
      TabOrder = 1
    end
    object ROIPanel: TPanel
      Left = 16
      Top = 264
      Width = 321
      Height = 21
      BevelOuter = bvNone
      TabOrder = 2
      object bAddROI: TButton
        Left = 2
        Top = 0
        Width = 73
        Height = 17
        Caption = 'Add ROI'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = bAddROIClick
      end
      object bDeleteROIs: TButton
        Left = 80
        Top = 0
        Width = 73
        Height = 17
        Caption = 'Delete ROIs'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bDeleteROIsClick
      end
      object ckDisplayCalBar: TCheckBox
        Left = 168
        Top = 0
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
        TabOrder = 2
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
  object ControlGrp: TGroupBox
    Left = 4
    Top = 0
    Width = 213
    Height = 937
    TabOrder = 1
    object LightStimGrp: TGroupBox
      Left = 5
      Top = 484
      Width = 204
      Height = 273
      Caption = ' Light / Stimulator  '
      TabOrder = 5
      object sbLightStimShowSettings: TSpeedButton
        Left = 8
        Top = 16
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
        OnClick = sbLightStimShowSettingsClick
      end
      object LightStimPage: TPageControl
        Left = 24
        Top = 16
        Width = 170
        Height = 250
        ActivePage = ExcitationLightTab
        MultiLine = True
        TabOrder = 0
        object ExcitationLightTab: TTabSheet
          Caption = 'Excitation Light'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object GroupBox6: TGroupBox
            Left = 2
            Top = 0
            Width = 154
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
          object WavelengthGrp: TGroupBox
            Left = 2
            Top = 52
            Width = 154
            Height = 124
            Caption = ' Wavelength '
            TabOrder = 1
            object rbSingleWavelength: TRadioButton
              Left = 6
              Top = 16
              Width = 73
              Height = 17
              Hint = 'Single excitation wavelength'
              Caption = 'Single '
              Checked = True
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 0
              TabStop = True
              OnClick = rbSingleWavelengthClick
            end
            object rbMultipleWavelengths: TRadioButton
              Left = 6
              Top = 58
              Width = 123
              Height = 17
              Hint = 'Multiple, sequential wavelength excitation'
              Caption = 'Multi-wavelength'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 1
              OnClick = rbMultipleWavelengthsClick
            end
            object cbWavelength: TComboBox
              Left = 25
              Top = 32
              Width = 124
              Height = 22
              Hint = 'Selected single excitation wavelength'
              TabOrder = 2
              Text = 'cbWavelength'
              OnChange = cbWavelengthChange
            end
            object rbSpectrum: TRadioButton
              Left = 6
              Top = 100
              Width = 91
              Height = 17
              Hint = 'Use sequence of excitatiion wavelengths'
              Caption = 'Spectrum'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 3
              OnClick = rbMultipleWavelengthsClick
            end
            object cbSequence: TComboBox
              Left = 25
              Top = 74
              Width = 124
              Height = 22
              Hint = 'Selected single excitation wavelength'
              TabOrder = 4
              Text = 'cbWavelength'
              OnChange = cbSequenceChange
            end
          end
          object bSetLaserIntensity: TButton
            Left = 2
            Top = 182
            Width = 153
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
          object ckExcitationOnWhenRecording: TCheckBox
            Left = 2
            Top = 32
            Width = 156
            Height = 16
            Hint = 
              'Turn on excitation during recording to file, turn off when recor' +
              'ding stops'
            Caption = 'Turn on when recording'
            Checked = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            State = cbChecked
            TabOrder = 3
            OnClick = ckExcitationOnWhenRecordingClick
          end
        end
        object StimulatorTab: TTabSheet
          Caption = 'Stimulator'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object cbStimProgram: TComboBox
            Left = 2
            Top = 0
            Width = 151
            Height = 22
            Hint = 'Stimulation program in current use'
            TabOrder = 0
            Text = 'cbStimProgram'
            OnChange = cbStimProgramChange
          end
          object bStartStimulus: TButton
            Left = 2
            Top = 26
            Width = 87
            Height = 17
            Hint = 'Start stimulation program'
            Caption = 'Start'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnClick = bStartStimulusClick
          end
          object bStopStimulus: TButton
            Left = 98
            Top = 26
            Width = 54
            Height = 17
            Hint = 'Stop stimulation program'
            Caption = 'Stop'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnClick = bStopStimulusClick
          end
          object VHoldGrp: TGroupBox
            Left = 2
            Top = 64
            Width = 151
            Height = 73
            Caption = ' V Hold (Default)'
            TabOrder = 3
            object VHold0Panel: TPanel
              Left = 11
              Top = 16
              Width = 134
              Height = 25
              BevelOuter = bvNone
              TabOrder = 0
              object lbVHold0: TLabel
                Left = 5
                Top = 2
                Width = 38
                Height = 15
                Caption = 'VOut 0'
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object edVHold0: TValidatedEdit
                Left = 60
                Top = 2
                Width = 69
                Height = 20
                Hint = 'Patch clamp cell holding voltage'
                OnKeyPress = edVHold0KeyPress
                AutoSize = False
                ShowHint = True
                Text = ' 0 mV'
                Scale = 1000.000000000000000000
                Units = 'mV'
                NumberFormat = '%.4g'
                LoLimit = -1.000000015047466E29
                HiLimit = 1.000000015047466E29
              end
            end
            object VHold1Panel: TPanel
              Left = 11
              Top = 42
              Width = 134
              Height = 25
              BevelOuter = bvNone
              TabOrder = 1
              object Label7: TLabel
                Left = 5
                Top = 2
                Width = 38
                Height = 15
                Caption = 'VOut 1'
                Font.Charset = ANSI_CHARSET
                Font.Color = clWindowText
                Font.Height = -12
                Font.Name = 'Arial'
                Font.Style = [fsBold]
                ParentFont = False
              end
              object edVHold1: TValidatedEdit
                Left = 60
                Top = 2
                Width = 69
                Height = 20
                Hint = 'Patch clamp cell holding voltage'
                OnKeyPress = edVHold0KeyPress
                AutoSize = False
                ShowHint = True
                Text = ' 0 mV'
                Scale = 1000.000000000000000000
                Units = 'mV'
                NumberFormat = '%.4g'
                LoLimit = -1.000000015047466E29
                HiLimit = 1.000000015047466E29
              end
            end
          end
          object bSetSubFolder: TButton
            Left = 2
            Top = 148
            Width = 151
            Height = 17
            Caption = 'Set Stim. Folder'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 4
            OnClick = bSetSubFolderClick
          end
          object ckStartStimOnRecord: TCheckBox
            Left = 0
            Top = 44
            Width = 113
            Height = 17
            Hint = 'Start stimulus program automatically when Record button pressed'
            Caption = 'Start on Record'
            Checked = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            State = cbChecked
            TabOrder = 5
            OnClick = ckStartStimOnRecordClick
          end
        end
        object PhotoStimTab: TTabSheet
          Caption = 'Photo Stim.'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object cbPhotoStimProgram: TComboBox
            Left = 2
            Top = 0
            Width = 151
            Height = 22
            Hint = 'Stimulation program in current use'
            TabOrder = 0
            Text = 'cbStimProgram'
            OnChange = cbPhotoStimProgramChange
          end
          object bStartPhotoStimulus: TButton
            Left = 2
            Top = 26
            Width = 87
            Height = 17
            Hint = 'Start stimulation program'
            Caption = 'Start'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnClick = bStartPhotoStimulusClick
          end
          object bStopPhotoStimulus: TButton
            Left = 98
            Top = 26
            Width = 54
            Height = 17
            Hint = 'Stop stimulation program'
            Caption = 'Stop'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnClick = bStopPhotoStimulusClick
          end
        end
      end
    end
    object DisplayGrp: TGroupBox
      Left = 5
      Top = 344
      Width = 204
      Height = 171
      Caption = ' Display '
      TabOrder = 0
      object sbDisplayShowSettings: TSpeedButton
        Left = 8
        Top = 16
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
        OnClick = sbDisplayShowSettingsClick
      end
      object cbPalette: TComboBox
        Left = 32
        Top = 16
        Width = 161
        Height = 22
        Hint = 'Display colour palette'
        Style = csDropDownList
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnChange = cbPaletteChange
      end
      object DisplaySettingsPanel: TPanel
        Left = 5
        Top = 42
        Width = 192
        Height = 122
        BevelOuter = bvNone
        Caption = 'DisplaySettingsPanel'
        TabOrder = 1
        object ContrastPage: TPageControl
          Left = 2
          Top = 0
          Width = 188
          Height = 120
          ActivePage = RangeTab
          MultiLine = True
          TabOrder = 0
          object RangeTab: TTabSheet
            Caption = 'Display Contrast'
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object bFullScale: TButton
              Left = 2
              Top = 4
              Width = 82
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
              Left = 94
              Top = 4
              Width = 82
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
              Width = 175
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
            object ckChangeAllFrameTypes: TCheckBox
              Left = 2
              Top = 59
              Width = 111
              Height = 19
              Hint = 'Update contrast on all image panels'
              Caption = 'All image panels'
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
              TabOrder = 3
              OnClick = ckChangeAllFrameTypesClick
            end
            object ckContrast6SDOnly: TCheckBox
              Left = 2
              Top = 75
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
              TabOrder = 4
              OnClick = ckContrast6SDOnlyClick
            end
            object ckAutoOptimise: TCheckBox
              Left = 2
              Top = 44
              Width = 119
              Height = 19
              Caption = 'Auto adjust'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              TabOrder = 5
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
              Width = 48
              Height = 14
              Caption = 'Contrast'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label10: TLabel
              Left = 14
              Top = 40
              Width = 61
              Height = 14
              Caption = 'Brightness'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
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
    end
    object RecordingGrp: TGroupBox
      Left = 5
      Top = 8
      Width = 204
      Height = 161
      Caption = ' Recording '
      TabOrder = 1
      object Label1: TLabel
        Left = 39
        Top = 62
        Width = 82
        Height = 14
        Alignment = taRightJustify
        Caption = 'Recording Period'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object Label18: TLabel
        Left = 61
        Top = 36
        Width = 26
        Height = 14
        Alignment = taRightJustify
        Caption = 'Mode'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object bRecord: TButton
        Left = 8
        Top = 16
        Width = 121
        Height = 17
        Caption = 'Record'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = bRecordClick
      end
      object bStop: TButton
        Left = 138
        Top = 16
        Width = 57
        Height = 17
        Caption = 'Stop'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bStopClick
      end
      object edRecordingPeriod: TValidatedEdit
        Left = 126
        Top = 62
        Width = 70
        Height = 20
        Hint = 'Period of time to record images to file'
        OnKeyPress = edRecordingPeriodKeyPress
        AutoSize = False
        Text = ' 1000 s'
        Value = 1000.000000000000000000
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.6g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object TimelapsePanel: TPanel
        Left = 22
        Top = 86
        Width = 180
        Height = 25
        BevelOuter = bvNone
        TabOrder = 3
        object Label9: TLabel
          Left = 8
          Top = 0
          Width = 93
          Height = 14
          Alignment = taRightJustify
          Caption = 'Time Lapse Interval'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object edTimeLapseInterval: TValidatedEdit
          Left = 104
          Top = 0
          Width = 70
          Height = 20
          Hint = 'Time lapse time interval'
          OnKeyPress = edTimeLapseIntervalKeyPress
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
      end
      object cbRecordingMode: TComboBox
        Left = 92
        Top = 36
        Width = 105
        Height = 22
        Style = csDropDownList
        ItemIndex = 2
        TabOrder = 4
        Text = 'T. Lapse + Burst'
        OnChange = cbRecordingModeChange
        Items.Strings = (
          'Continuous'
          'Time Lapse'
          'T. Lapse + Burst')
      end
      object BurstModePanel: TPanel
        Left = 22
        Top = 110
        Width = 180
        Height = 47
        BevelOuter = bvNone
        TabOrder = 5
        object Label19: TLabel
          Left = 32
          Top = 0
          Width = 69
          Height = 14
          Alignment = taRightJustify
          Caption = 'Burst Duration'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label20: TLabel
          Left = 37
          Top = 24
          Width = 64
          Height = 14
          Alignment = taRightJustify
          Caption = 'Burst Interval'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object edBurstDuration: TValidatedEdit
          Left = 104
          Top = 0
          Width = 70
          Height = 20
          Hint = 'Duration of high speed recording burst'
          OnKeyPress = edBurstDurationKeyPress
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
        object edBurstInterval: TValidatedEdit
          Left = 104
          Top = 24
          Width = 70
          Height = 20
          Hint = 'Interval between high speed recording bursts'
          OnKeyPress = edBurstIntervalKeyPress
          AutoSize = False
          ShowHint = True
          Text = ' 10 s'
          Value = 10.000000000000000000
          Scale = 1.000000000000000000
          Units = 's'
          NumberFormat = '%.6g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
      end
    end
    object MarkGrp: TGroupBox
      Left = 5
      Top = 873
      Width = 204
      Height = 42
      Caption = ' Markers '
      TabOrder = 2
      object edMarker: TEdit
        Left = 118
        Top = 14
        Width = 75
        Height = 20
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyPress = edMarkerKeyPress
      end
      object bMark: TButton
        Left = 6
        Top = 16
        Width = 107
        Height = 18
        Hint = 'Add marker text to bottom of display'
        Caption = 'Add Marker'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bMarkClick
      end
    end
    object ImageCaptureGrp: TGroupBox
      Left = 5
      Top = 176
      Width = 204
      Height = 162
      Caption = ' Image Capture '
      TabOrder = 3
      object lbReadoutTime: TLabel
        Left = 120
        Top = 38
        Width = 70
        Height = 14
        Caption = 'lbReadoutTime'
      end
      object Label3: TLabel
        Left = 44
        Top = 16
        Width = 84
        Height = 14
        Alignment = taRightJustify
        Caption = 'Exposure Interval'
      end
      object sbImageCaptureShowSettings: TSpeedButton
        Left = 4
        Top = 16
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
        OnClick = sbImageCaptureShowSettingsClick
      end
      object edFrameInterval: TValidatedEdit
        Left = 133
        Top = 16
        Width = 60
        Height = 20
        Hint = 'Camera exposure interval'
        OnKeyPress = edFrameIntervalKeyPress
        AutoSize = False
        Text = ' 0 ms'
        Scale = 1000.000000000000000000
        Units = 'ms'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E29
      end
      object ImageCaptureSettingsPanel: TPanel
        Left = 8
        Top = 54
        Width = 188
        Height = 100
        BevelOuter = bvNone
        TabOrder = 1
        object CCDAreaGrp: TGroupBox
          Left = 0
          Top = 0
          Width = 188
          Height = 98
          Caption = 'CCD Area '
          TabOrder = 0
          object Label4: TLabel
            Left = 102
            Top = 16
            Width = 35
            Height = 14
            Caption = 'Binning'
            WordWrap = True
          end
          object Label8: TLabel
            Left = 102
            Top = 34
            Width = 22
            Height = 14
            Alignment = taRightJustify
            Caption = 'Gain'
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
            Left = 146
            Top = 16
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
          object cbCameraGain: TComboBox
            Left = 102
            Top = 50
            Width = 80
            Height = 22
            Hint = 'Selected single excitation wavelength'
            Style = csDropDownList
            TabOrder = 4
            OnChange = cbCameraGainChange
          end
          object ckSplitCCDImage: TCheckBox
            Left = 8
            Top = 76
            Width = 169
            Height = 17
            Hint = 'Acquire upper and lower halves of CCD as separate frames'
            Caption = 'Split CCD image '
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 5
            OnClick = ckSplitCCDImageClick
          end
        end
      end
    end
    object ShadingGrp: TGroupBox
      Left = 5
      Top = 516
      Width = 204
      Height = 141
      Caption = ' Shading Correction '
      TabOrder = 4
      object sbShadeCorShowSettings: TSpeedButton
        Left = 8
        Top = 36
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
      end
      object bAcquireBackground: TButton
        Left = 32
        Top = 36
        Width = 161
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
        Top = 58
        Width = 190
        Height = 74
        BevelOuter = bvNone
        TabOrder = 2
        object Label15: TLabel
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
        object Label16: TLabel
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
        object Label17: TLabel
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
          Left = 152
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
          Left = 151
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
          Left = 136
          Top = 48
          Width = 55
          Height = 22
          TabOrder = 2
          Text = 'cbShadeCorNormalisation'
        end
      end
    end
    object ZStageGrp: TGroupBox
      Left = 5
      Top = 736
      Width = 204
      Height = 137
      Caption = 'Z Axis Position '
      TabOrder = 6
      object sbShowHideZStackSettings: TSpeedButton
        Left = 8
        Top = 16
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
        OnClick = sbShowHideZStackSettingsClick
      end
      object edZPosition: TValidatedEdit
        Left = 28
        Top = 16
        Width = 61
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
      object ZStackGrp: TGroupBox
        Left = 8
        Top = 40
        Width = 185
        Height = 89
        Caption = ' Z Stack'
        TabOrder = 1
        object Label21: TLabel
          Left = 77
          Top = 14
          Width = 36
          Height = 14
          Alignment = taRightJustify
          Caption = 'Start At'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label22: TLabel
          Left = 67
          Top = 38
          Width = 46
          Height = 14
          Alignment = taRightJustify
          Caption = 'Step Size'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label23: TLabel
          Left = 66
          Top = 62
          Width = 47
          Height = 14
          Alignment = taRightJustify
          Caption = 'No. Steps'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object edZStartPos: TValidatedEdit
          Left = 120
          Top = 13
          Width = 57
          Height = 20
          Hint = 'Period of time to record images to file'
          OnKeyPress = edZStartPosKeyPress
          AutoSize = False
          Text = ' 1000 um'
          Value = 1000.000000000000000000
          Scale = 1.000000000000000000
          Units = 'um'
          NumberFormat = '%.6g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object edZStepSize: TValidatedEdit
          Left = 120
          Top = 37
          Width = 57
          Height = 20
          Hint = 'Period of time to record images to file'
          OnKeyPress = edZStepSizeKeyPress
          AutoSize = False
          Text = ' 1000 um'
          Value = 1000.000000000000000000
          Scale = 1.000000000000000000
          Units = 'um'
          NumberFormat = '%.6g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object edZNumSteps: TValidatedEdit
          Left = 120
          Top = 61
          Width = 57
          Height = 20
          Hint = 'Period of time to record images to file'
          OnKeyPress = edZNumStepsKeyPress
          AutoSize = False
          Text = ' 20 '
          Value = 20.000000000000000000
          Scale = 1.000000000000000000
          NumberFormat = '%.6g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object ckZStackEnabled: TCheckBox
          Left = 8
          Top = 16
          Width = 57
          Height = 17
          Caption = 'Enable'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
          OnClick = ckZStackEnabledClick
        end
      end
      object sbZPosition: TScrollBar
        Left = 96
        Top = 16
        Width = 97
        Height = 14
        PageSize = 0
        TabOrder = 2
        OnChange = sbZPositionChange
      end
    end
  end
  object IdentGrp: TGroupBox
    Left = 238
    Top = 0
    Width = 401
    Height = 35
    TabOrder = 2
    object Label2: TLabel
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
      OnKeyPress = edIdentKeyPress
    end
  end
  object Timer: TTimer
    Interval = 55
    OnTimer = TimerTimer
    Left = 248
    Top = 560
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofNoValidate, ofEnableSizing]
    Left = 224
    Top = 624
  end
  object MainMenu1: TMainMenu
    Left = 344
    Top = 608
  end
  object IDRBackground: TIDRFile
    NumFrames = 0
    NumFrameTypes = 1
    FrameWidth = 0
    FrameHeight = 0
    PixelDepth = 1
    NumZSections = 1
    ZSpacing = 1.000000000000000000
    IntensityScale = 1.000000000000000000
    XResolution = 1.000000000000000000
    ADCNumScansInFile = 0
    ADCNumChannels = 0
    ADCNumScansPerFrame = 0
    ADCMaxValue = 0
    LineScan = False
    LSTimeCoursePixel = 0
    LSTimeCourseNumAvg = 1
    LSTimeCourseBackgroundPixel = 0
    LSSubtractBackground = False
    WriteEnabled = False
    SpectralDataFile = False
    EventDisplayDuration = 1.000000000000000000
    EventDeadTime = 1.000000000000000000
    EventDetectionThreshold = 1000.000000000000000000
    EventDetectionThresholdPolarity = 0
    EventDetectionSource = 0
    EventROI = 0
    EventBackgROI = 0
    EventFixedBaseline = True
    EventRollingBaselinePeriod = 1.000000000000000000
    EventBaselineLevel = 0
    EventRatioExclusionThreshold = 0
    EventRatioTop = 0
    EventRatioBottom = 1
    EventRatioDisplayMax = 10.000000000000000000
    EventRatioRMax = 1.000000000000000000
    EventFLWave = 0
    EventF0Wave = 0
    EventF0Start = 1
    EventF0End = 1
    EventF0UseConstant = False
    EventF0DisplayMax = 10.000000000000000000
    EventF0SubtractF0 = False
    Left = 424
    Top = 744
  end
  object IDRFileBurst: TIDRFile
    NumFrames = 0
    NumFrameTypes = 1
    FrameWidth = 0
    FrameHeight = 0
    PixelDepth = 1
    NumZSections = 1
    ZSpacing = 1.000000000000000000
    IntensityScale = 1.000000000000000000
    XResolution = 1.000000000000000000
    ADCNumScansInFile = 0
    ADCNumChannels = 0
    ADCNumScansPerFrame = 0
    ADCMaxValue = 0
    LineScan = False
    LSTimeCoursePixel = 0
    LSTimeCourseNumAvg = 1
    LSTimeCourseBackgroundPixel = 0
    LSSubtractBackground = False
    WriteEnabled = False
    SpectralDataFile = False
    EventDisplayDuration = 1.000000000000000000
    EventDeadTime = 1.000000000000000000
    EventDetectionThreshold = 1000.000000000000000000
    EventDetectionThresholdPolarity = 0
    EventDetectionSource = 0
    EventROI = 0
    EventBackgROI = 0
    EventFixedBaseline = True
    EventRollingBaselinePeriod = 1.000000000000000000
    EventBaselineLevel = 0
    EventRatioExclusionThreshold = 0
    EventRatioTop = 0
    EventRatioBottom = 1
    EventRatioDisplayMax = 10.000000000000000000
    EventRatioRMax = 1.000000000000000000
    EventFLWave = 0
    EventF0Wave = 0
    EventF0Start = 1
    EventF0End = 1
    EventF0UseConstant = False
    EventF0DisplayMax = 10.000000000000000000
    EventF0SubtractF0 = False
    Left = 248
    Top = 696
  end
  object IDRFileXY: TIDRFile
    NumFrames = 0
    NumFrameTypes = 1
    FrameWidth = 0
    FrameHeight = 0
    PixelDepth = 1
    NumZSections = 1
    ZSpacing = 1.000000000000000000
    IntensityScale = 1.000000000000000000
    XResolution = 1.000000000000000000
    ADCNumScansInFile = 0
    ADCNumChannels = 0
    ADCNumScansPerFrame = 0
    ADCMaxValue = 0
    LineScan = False
    LSTimeCoursePixel = 0
    LSTimeCourseNumAvg = 1
    LSTimeCourseBackgroundPixel = 0
    LSSubtractBackground = False
    WriteEnabled = False
    SpectralDataFile = False
    EventDisplayDuration = 1.000000000000000000
    EventDeadTime = 1.000000000000000000
    EventDetectionThreshold = 1000.000000000000000000
    EventDetectionThresholdPolarity = 0
    EventDetectionSource = 0
    EventROI = 0
    EventBackgROI = 0
    EventFixedBaseline = True
    EventRollingBaselinePeriod = 1.000000000000000000
    EventBaselineLevel = 0
    EventRatioExclusionThreshold = 0
    EventRatioTop = 0
    EventRatioBottom = 1
    EventRatioDisplayMax = 10.000000000000000000
    EventRatioRMax = 1.000000000000000000
    EventFLWave = 0
    EventF0Wave = 0
    EventF0Start = 1
    EventF0End = 1
    EventF0UseConstant = False
    EventF0DisplayMax = 10.000000000000000000
    EventF0SubtractF0 = False
    Left = 312
    Top = 696
  end
end
