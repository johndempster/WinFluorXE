object StimSetupFrm: TStimSetupFrm
  Left = 324
  Top = 322
  Caption = 'Stimulus Setup'
  ClientHeight = 320
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  Position = poMainFormCenter
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object DigitalPulseGrp: TGroupBox
    Left = 7
    Top = 184
    Width = 475
    Height = 108
    Caption = ' Digital Pulse 1 '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Pulse1Grp: TGroupBox
      Left = 6
      Top = 31
      Width = 352
      Height = 66
      Caption = ' Timing '
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Label1: TLabel
        Left = 17
        Top = 40
        Width = 63
        Height = 15
        Alignment = taRightJustify
        Caption = 'Initial Delay'
      end
      object Label2: TLabel
        Left = 134
        Top = 18
        Width = 69
        Height = 15
        Alignment = taRightJustify
        Caption = ' Pulse Width'
      end
      object Label3: TLabel
        Left = 18
        Top = 18
        Width = 61
        Height = 15
        Alignment = taRightJustify
        Caption = 'No. Pulses'
      end
      object lbInterval1: TLabel
        Left = 151
        Top = 40
        Width = 52
        Height = 15
        Alignment = taRightJustify
        Caption = 'At Interval'
      end
      object edDelay1: TValidatedEdit
        Left = 84
        Top = 40
        Width = 45
        Height = 19
        AutoSize = False
        Text = ' 0 s'
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.3g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
      object edWidth1: TValidatedEdit
        Left = 207
        Top = 18
        Width = 46
        Height = 19
        AutoSize = False
        Text = ' 0 s'
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.3g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
      object edNumPulses1: TValidatedEdit
        Left = 85
        Top = 18
        Width = 44
        Height = 19
        OnKeyPress = edNumPulses1KeyPress
        AutoSize = False
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.0f'
        LoLimit = 1.000000000000000000
        HiLimit = 1.000000015047466E29
      end
      object edInterval1: TValidatedEdit
        Left = 207
        Top = 40
        Width = 46
        Height = 19
        AutoSize = False
        Text = ' 0 s'
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.3g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
    end
    object ckUsePulse1: TCheckBox
      Left = 7
      Top = 15
      Width = 76
      Height = 16
      Caption = 'Use Pulse'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = ckUsePulse1Click
    end
    object GroupBox12: TGroupBox
      Left = 364
      Top = 31
      Width = 105
      Height = 66
      Caption = ' Polarity '
      TabOrder = 2
      object rbActiveHigh1: TRadioButton
        Left = 7
        Top = 18
        Width = 90
        Height = 15
        Caption = 'Active High'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbActiveLow1: TRadioButton
        Left = 7
        Top = 34
        Width = 90
        Height = 17
        Caption = 'Active Low'
        TabOrder = 1
      end
    end
  end
  object bOK: TButton
    Left = 7
    Top = 295
    Width = 39
    Height = 23
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 52
    Top = 295
    Width = 45
    Height = 16
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = bCancelClick
  end
  object GroupBox7: TGroupBox
    Left = 320
    Top = 0
    Width = 162
    Height = 68
    Caption = ' Single/Repeated Stimulus '
    TabOrder = 3
    object Label21: TLabel
      Left = 90
      Top = 15
      Width = 36
      Height = 15
      Caption = 'Period'
    end
    object rbRepeat: TRadioButton
      Left = 7
      Top = 34
      Width = 65
      Height = 16
      Caption = 'Repeat'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object rbSingle: TRadioButton
      Left = 7
      Top = 18
      Width = 65
      Height = 16
      Caption = 'Single'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      TabStop = True
    end
    object edRepeatPeriod: TValidatedEdit
      Left = 89
      Top = 30
      Width = 51
      Height = 18
      OnKeyPress = edPrePulseAmplitudeKeyPress
      AutoSize = False
      Text = ' 0.01 s'
      Value = 0.009999999776482582
      Scale = 1.000000000000000000
      Units = 's'
      NumberFormat = '%.4g'
      LoLimit = 0.009999999776482582
      HiLimit = 1.000000015047466E29
    end
  end
  object GroupBox8: TGroupBox
    Left = 7
    Top = 0
    Width = 306
    Height = 68
    Caption = ' Stimulus File '
    TabOrder = 4
    object edFileName: TEdit
      Left = 7
      Top = 15
      Width = 293
      Height = 23
      ReadOnly = True
      TabOrder = 0
      Text = 'edFileName'
    end
    object bLoadFile: TButton
      Left = 7
      Top = 42
      Width = 74
      Height = 18
      Caption = 'Load File ..'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = bLoadFileClick
    end
    object bSaveFile: TButton
      Left = 90
      Top = 42
      Width = 74
      Height = 18
      Caption = 'Save File ..'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = bSaveFileClick
    end
  end
  object VoltagePulseGrp: TGroupBox
    Left = 7
    Top = 72
    Width = 475
    Height = 110
    Caption = ' Voltage Pulse (D/A 0) '
    TabOrder = 5
    object GroupBox9: TGroupBox
      Left = 128
      Top = 15
      Width = 126
      Height = 74
      Caption = 'Pre-Pulse '
      TabOrder = 0
      object Label4: TLabel
        Left = 8
        Top = 18
        Width = 57
        Height = 15
        Alignment = taRightJustify
        Caption = ' Amplitude'
      end
      object Label8: TLabel
        Left = 31
        Top = 40
        Width = 34
        Height = 15
        Alignment = taRightJustify
        Caption = ' Width'
      end
      object edPrePulseAmplitude: TValidatedEdit
        Left = 67
        Top = 18
        Width = 53
        Height = 19
        OnKeyPress = edPrePulseAmplitudeKeyPress
        AutoSize = False
        Text = ' 0 mV'
        Scale = 1000.000000000000000000
        Units = 'mV'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
      object edPrePulseWidth: TValidatedEdit
        Left = 67
        Top = 40
        Width = 53
        Height = 19
        OnKeyPress = edPrePulseAmplitudeKeyPress
        AutoSize = False
        Text = ' 0 s'
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
    end
    object GroupBox10: TGroupBox
      Left = 259
      Top = 15
      Width = 210
      Height = 74
      Caption = ' Primary Pulse(s) '
      TabOrder = 1
      object Label12: TLabel
        Left = 8
        Top = 18
        Width = 57
        Height = 15
        Alignment = taRightJustify
        Caption = ' Amplitude'
      end
      object Label16: TLabel
        Left = 31
        Top = 40
        Width = 34
        Height = 15
        Alignment = taRightJustify
        Caption = ' Width'
      end
      object Label17: TLabel
        Left = 132
        Top = 18
        Width = 22
        Height = 15
        Alignment = taRightJustify
        Caption = 'Incr.'
      end
      object Label18: TLabel
        Left = 132
        Top = 40
        Width = 22
        Height = 15
        Alignment = taRightJustify
        Caption = 'Incr.'
      end
      object edPrimaryPulseAmplitude: TValidatedEdit
        Left = 67
        Top = 18
        Width = 53
        Height = 19
        OnKeyPress = edPrePulseAmplitudeKeyPress
        AutoSize = False
        Text = ' 0 mV'
        Scale = 1000.000000000000000000
        Units = 'mV'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
      object edPrimaryPulseWidth: TValidatedEdit
        Left = 67
        Top = 40
        Width = 53
        Height = 19
        OnKeyPress = edPrePulseAmplitudeKeyPress
        AutoSize = False
        Text = ' 0 s'
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
      object edPrimaryPulseAmplitudeIncrement: TValidatedEdit
        Left = 154
        Top = 18
        Width = 46
        Height = 19
        OnKeyPress = edPrePulseAmplitudeKeyPress
        AutoSize = False
        Text = ' 0 mV'
        Scale = 1000.000000000000000000
        Units = 'mV'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
      object edPrimaryPulseWidthIncrement: TValidatedEdit
        Left = 154
        Top = 40
        Width = 46
        Height = 19
        OnKeyPress = edPrePulseAmplitudeKeyPress
        AutoSize = False
        Text = ' 0 s'
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
    end
    object GroupBox11: TGroupBox
      Left = 6
      Top = 15
      Width = 118
      Height = 90
      Caption = ' Timing '
      TabOrder = 2
      object Label19: TLabel
        Left = 8
        Top = 18
        Width = 61
        Height = 15
        Alignment = taRightJustify
        Caption = 'No. Pulses'
      end
      object Label20: TLabel
        Left = 17
        Top = 41
        Width = 52
        Height = 15
        Alignment = taRightJustify
        Caption = 'At Interval'
      end
      object Label22: TLabel
        Left = 6
        Top = 65
        Width = 63
        Height = 15
        Alignment = taRightJustify
        Caption = 'Initial Delay'
      end
      object edNumPulses: TValidatedEdit
        Left = 72
        Top = 18
        Width = 39
        Height = 19
        AutoSize = False
        Text = ' 0 '
        Scale = 1.000000000000000000
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
      object edPulseInterval: TValidatedEdit
        Left = 72
        Top = 41
        Width = 39
        Height = 19
        OnKeyPress = edPrePulseAmplitudeKeyPress
        AutoSize = False
        Text = ' 0 s'
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
      object edVDelay: TValidatedEdit
        Left = 72
        Top = 65
        Width = 39
        Height = 18
        OnKeyPress = edPrePulseAmplitudeKeyPress
        AutoSize = False
        Text = ' 0 s'
        Scale = 1.000000000000000000
        Units = 's'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E29
      end
    end
  end
  object OpenDialog: TOpenDialog
    Left = 120
    Top = 320
  end
  object SaveDialog: TSaveDialog
    Left = 152
    Top = 320
  end
end
