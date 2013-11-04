object RecADCOnlyFrm: TRecADCOnlyFrm
  Left = 639
  Top = 8
  Width = 600
  Height = 897
  Caption = 'BioRad LaserSharp Image Capture'
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
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnDeactivate = FormDeactivate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ControlsGrp: TGroupBox
    Left = 2
    Top = 0
    Width = 175
    Height = 857
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 8
      Top = 8
      Width = 161
      Height = 81
      TabOrder = 0
      object Label1: TLabel
        Left = 47
        Top = 36
        Width = 46
        Height = 14
        Alignment = taRightJustify
        Caption = 'Duration'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object bRecord: TButton
        Left = 8
        Top = 10
        Width = 81
        Height = 22
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
        Left = 96
        Top = 10
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
      object edRecordingTime: TValidatedEdit
        Left = 96
        Top = 36
        Width = 57
        Height = 17
        Hint = 'No. of frames to be recorded to file'
        OnKeyPress = edRecordingTimeKeyPress
        AutoSize = False
        Text = ' 10 s'
        Value = 10.000000000000000000
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.0f'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object ckRecordADCSignalsOnly: TCheckBox
        Left = 10
        Top = 56
        Width = 111
        Height = 20
        Caption = 'A/D Signals Only'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnClick = ckRecordADCSignalsOnlyClick
      end
    end
    object MarkGrp: TGroupBox
      Left = 8
      Top = 686
      Width = 161
      Height = 61
      TabOrder = 1
      object edMarker: TEdit
        Left = 6
        Top = 34
        Width = 147
        Height = 20
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object bMark: TButton
        Left = 6
        Top = 12
        Width = 115
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
      object EdNumMarkers: TEdit
        Left = 130
        Top = 12
        Width = 25
        Height = 17
        Hint = 'Number of markers remaining'
        AutoSize = False
        ReadOnly = True
        TabOrder = 2
        Text = 'EdNumMarkers'
      end
    end
    object ImageCaptureGrp: TGroupBox
      Left = 8
      Top = 437
      Width = 161
      Height = 249
      Caption = ' Image Capture  '
      TabOrder = 2
      object ModeGrp: TGroupBox
        Left = 8
        Top = 23
        Width = 145
        Height = 122
        Caption = ' Mode '
        TabOrder = 0
        object lbInterval: TLabel
          Left = 35
          Top = 52
          Width = 41
          Height = 14
          Alignment = taRightJustify
          Caption = 'Interval'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object Label3: TLabel
          Left = 18
          Top = 84
          Width = 58
          Height = 14
          Alignment = taRightJustify
          Caption = 'Start delay'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object rbImage: TRadioButton
          Left = 8
          Top = 16
          Width = 105
          Height = 17
          Caption = 'Image Series'
          Checked = True
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          TabStop = True
          OnClick = rbImageClick
        end
        object rbLineScan: TRadioButton
          Left = 8
          Top = 32
          Width = 81
          Height = 17
          Caption = 'Line Scan'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnClick = rbLineScanClick
        end
        object edInterval: TValidatedEdit
          Left = 80
          Top = 53
          Width = 57
          Height = 17
          AutoSize = False
          Text = ' 2 ms'
          Value = 0.002000000094994903
          Scale = 1000.000000000000000000
          Units = 'ms'
          NumberFormat = '%.5g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edImageStartDelay: TValidatedEdit
          Left = 80
          Top = 85
          Width = 57
          Height = 17
          AutoSize = False
          Text = ' 0 ms'
          Scale = 1000.000000000000000000
          Units = 'ms'
          NumberFormat = '%.5g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
      end
      object TriggerGrp: TGroupBox
        Left = 8
        Top = 144
        Width = 145
        Height = 97
        Caption = ' Trigger '
        TabOrder = 1
        object GroupBox5: TGroupBox
          Left = 8
          Top = 50
          Width = 129
          Height = 39
          Caption = ' Polarity '
          TabOrder = 0
          object rbTriggerTTLHigh: TRadioButton
            Left = 8
            Top = 16
            Width = 49
            Height = 17
            Caption = '5V'
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
          object rbTriggerTTLLow: TRadioButton
            Left = 48
            Top = 16
            Width = 57
            Height = 17
            Caption = '0 V'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
          end
        end
        object rbFrameTrigger: TRadioButton
          Left = 8
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Each Frame '
          Enabled = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
        end
        object rbSequenceTrigger: TRadioButton
          Left = 8
          Top = 32
          Width = 97
          Height = 17
          Caption = 'First Frame '
          Checked = True
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          TabStop = True
        end
      end
    end
    object StimulatorGrp: TGroupBox
      Left = 8
      Top = 89
      Width = 161
      Height = 155
      Caption = ' Stimulator '
      TabOrder = 3
      object bStartStimulus: TButton
        Left = 8
        Top = 44
        Width = 81
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
        TabOrder = 0
        OnClick = bStartStimulusClick
      end
      object cbStimProgram: TComboBox
        Left = 8
        Top = 16
        Width = 145
        Height = 21
        Hint = 'Stimulation program in current use'
        ItemHeight = 13
        TabOrder = 1
        Text = 'cbStimProgram'
        OnChange = cbStimProgramChange
      end
      object bStopStimulus: TButton
        Left = 96
        Top = 44
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
        Left = 8
        Top = 64
        Width = 143
        Height = 84
        Caption = ' V Hold (Default)'
        TabOrder = 3
        object VHold0Panel: TPanel
          Left = 3
          Top = 16
          Width = 134
          Height = 38
          BevelOuter = bvNone
          TabOrder = 0
          object lbVHold0: TLabel
            Left = 24
            Top = 2
            Width = 35
            Height = 14
            Alignment = taRightJustify
            Caption = 'Vout 0'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edVHold0: TValidatedEdit
            Left = 60
            Top = 2
            Width = 69
            Height = 17
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
          Left = 3
          Top = 39
          Width = 134
          Height = 20
          BevelOuter = bvNone
          TabOrder = 1
          object Label7: TLabel
            Left = 24
            Top = 0
            Width = 35
            Height = 14
            Alignment = taRightJustify
            Caption = 'Vout 1'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edVHold1: TValidatedEdit
            Left = 60
            Top = 0
            Width = 69
            Height = 17
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
        object VHold2Panel: TPanel
          Left = 2
          Top = 60
          Width = 134
          Height = 20
          BevelOuter = bvNone
          TabOrder = 2
          object Label4: TLabel
            Left = 24
            Top = 0
            Width = 35
            Height = 14
            Alignment = taRightJustify
            Caption = 'Vout 2'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edVHold2: TValidatedEdit
            Left = 60
            Top = 0
            Width = 69
            Height = 17
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
    end
    object ReadoutGrp: TGroupBox
      Left = 8
      Top = 747
      Width = 161
      Height = 105
      TabOrder = 4
      object lbMeasurements: THTMLLabel
        Left = 8
        Top = 16
        Width = 145
        Height = 81
        Caption = 'Label'
        Alignment = taLeftJustify
        LineSpacing = 1.000000000000000000
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
      end
    end
    object PhotoStimulusGrp: TGroupBox
      Left = 8
      Top = 245
      Width = 161
      Height = 64
      Caption = ' Photo-stimulus '
      TabOrder = 5
      object cbPhotoStimProgram: TComboBox
        Left = 8
        Top = 16
        Width = 145
        Height = 21
        Hint = 'Stimulation program in current use'
        ItemHeight = 13
        TabOrder = 0
        Text = 'cbStimProgram'
        OnChange = cbPhotoStimProgramChange
      end
      object ckPhotoStimEnabled: TCheckBox
        Left = 8
        Top = 40
        Width = 145
        Height = 17
        Caption = 'Apply photo-stimulus'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = ckPhotoStimEnabledClick
      end
    end
    object DynamicProtocolGrp: TGroupBox
      Left = 8
      Top = 309
      Width = 161
      Height = 64
      Caption = ' Dynamic Protocol '
      TabOrder = 6
      object bDynamicProtocolSetup: TButton
        Left = 8
        Top = 17
        Width = 143
        Height = 17
        Caption = 'Dynamic Protocol Setup...'
        TabOrder = 0
        OnClick = bDynamicProtocolSetupClick
      end
      object ckDynamicProtocolEnabled: TCheckBox
        Left = 8
        Top = 39
        Width = 145
        Height = 17
        Caption = 'Apply dynamic protocol'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = ckDynamicProtocolEnabledClick
      end
    end
    object PlaybackGrp: TGroupBox
      Left = 8
      Top = 373
      Width = 161
      Height = 64
      Caption = ' Playback '
      TabOrder = 7
      object bPlaybackSetup: TButton
        Left = 8
        Top = 17
        Width = 143
        Height = 17
        Caption = 'Playback Setup...'
        TabOrder = 0
        OnClick = bPlaybackSetupClick
      end
      object ckPlaybackEnabled: TCheckBox
        Left = 8
        Top = 39
        Width = 145
        Height = 17
        Caption = 'Apply playback'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = ckPlaybackEnabledClick
      end
    end
  end
  object SignalsGrp: TGroupBox
    Left = 184
    Top = 40
    Width = 401
    Height = 257
    Caption = ' Signals '
    TabOrder = 1
    object scADCDisplay: TScopeDisplay
      Left = 8
      Top = 16
      Width = 385
      Height = 209
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
    object TDisplayPanel: TPanel
      Left = 152
      Top = 230
      Width = 209
      Height = 23
      BevelOuter = bvNone
      TabOrder = 0
      object edTDisplay: TValidatedEdit
        Left = 120
        Top = 1
        Width = 65
        Height = 19
        OnKeyPress = edTDisplayKeyPress
        AutoSize = False
        Text = ' 1 s'
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        Units = 's'
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
      object bTDisplayHalf: TButton
        Left = 102
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
        TabOrder = 3
        OnClick = bTDisplayHalfClick
      end
      object bTDisplayDouble: TButton
        Left = 188
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
        TabOrder = 4
        OnClick = bTDisplayDoubleClick
      end
    end
    object ckFixZeroLevels: TCheckBox
      Left = 8
      Top = 232
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
  end
  object IdentGrp: TGroupBox
    Left = 184
    Top = 0
    Width = 401
    Height = 35
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 11
      Width = 24
      Height = 13
      Caption = 'Expt.'
    end
    object edIdent: TEdit
      Left = 40
      Top = 11
      Width = 353
      Height = 17
      AutoSize = False
      TabOrder = 0
      Text = 'edIdent'
      OnKeyPress = edIdentKeyPress
    end
  end
  object Timer: TTimer
    Interval = 55
    OnTimer = TimerTimer
    Left = 208
    Top = 520
  end
  object OpenDialog: TOpenDialog
    Left = 208
    Top = 360
  end
  object ImageFile: TImageFile
    XResolution = 1.000000000000000000
    YResolution = 1.000000000000000000
    ZResolution = 1.000000000000000000
    TResolution = 1.000000000000000000
    Left = 472
    Top = 328
  end
end
