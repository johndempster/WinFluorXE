object DirectControlFrm: TDirectControlFrm
  Left = 668
  Top = 42
  Width = 436
  Height = 845
  Caption = 'Device Control Outputs (Direct Control)'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 209
    Width = 428
    Height = 4
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Beveled = True
    MinSize = 20
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 323
    Width = 428
    Height = 4
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Beveled = True
    MinSize = 20
  end
  object Splitter3: TSplitter
    Left = 0
    Top = 545
    Width = 428
    Height = 4
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Beveled = True
    MinSize = 20
  end
  object Splitter4: TSplitter
    Left = 0
    Top = 737
    Width = 428
    Height = 4
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Beveled = True
    MinSize = 20
  end
  object LSControlGrp: TGroupBox
    Left = 0
    Top = 0
    Width = 428
    Height = 209
    Align = alTop
    Caption = ' Light Source Control Voltages '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object LightSourcePanel0: TPanel
      Left = 4
      Top = 16
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Label0: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object Trackbar0: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = Trackbar0Change
      end
      object ValidatedEdit0: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit0KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object LightSourcePanel1: TPanel
      Left = 4
      Top = 42
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object Label1: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object TrackBar1: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar1Change
      end
      object ValidatedEdit1: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit1KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object LightSourcePanel2: TPanel
      Left = 4
      Top = 68
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object Label2: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object TrackBar2: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar2Change
      end
      object ValidatedEdit2: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit2KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object LightSourcePanel3: TPanel
      Left = 4
      Top = 94
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      object Label3: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object TrackBar3: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar3Change
      end
      object ValidatedEdit3: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit3KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object LightSourcePanel4: TPanel
      Left = 4
      Top = 120
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      object Label5: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object TrackBar4: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar4Change
      end
      object ValidatedEdit4: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit4KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object LightSourcePanel5: TPanel
      Left = 4
      Top = 146
      Width = 450
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      object Label6: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object TrackBar5: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar5Change
      end
      object ValidatedEdit5: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit5KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object ShutterPanel: TPanel
      Left = 4
      Top = 172
      Width = 450
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      object Label9: TLabel
        Left = 2
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object ComboBox1: TComboBox
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        ItemIndex = 0
        TabOrder = 0
        Text = 'Off (0V)'
        OnChange = ComboBox1Change
        Items.Strings = (
          'Off (0V)'
          'On (5V)')
      end
    end
  end
  object VStimGrp: TGroupBox
    Left = 0
    Top = 213
    Width = 428
    Height = 110
    Align = alTop
    Caption = ' Voltage Stimulus Outputs  '
    TabOrder = 1
    object VStimPanel0: TPanel
      Left = 4
      Top = 16
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Label20: TLabel
        Left = 8
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object Trackbar20: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = Trackbar20Change
      end
      object validatededit20: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = validatededit20KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object VStimPanel1: TPanel
      Left = 4
      Top = 42
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object Label7: TLabel
        Left = 8
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object TrackBar21: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar21Change
      end
      object ValidatedEdit21: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit21KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object VStimPanel2: TPanel
      Left = 4
      Top = 68
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object Label8: TLabel
        Left = 8
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object TrackBar22: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar22Change
      end
      object ValidatedEdit22: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit22KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
  end
  object DigStimGrp: TGroupBox
    Left = 0
    Top = 327
    Width = 428
    Height = 218
    Align = alTop
    Caption = ' Digital Stimulus Outputs '
    TabOrder = 2
    object DigStimPanel0: TPanel
      Left = 4
      Top = 16
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Label30: TLabel
        Left = 2
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object combobox30: TComboBox
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        ItemIndex = 0
        TabOrder = 0
        Text = 'Off (0V)'
        OnChange = combobox30Change
        Items.Strings = (
          'Off (0V)'
          'On (5V)')
      end
    end
    object DigStimPanel1: TPanel
      Left = 4
      Top = 40
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object Label31: TLabel
        Left = 2
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object combobox31: TComboBox
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        ItemIndex = 0
        TabOrder = 0
        Text = 'Off (0V)'
        OnChange = combobox31Change
        Items.Strings = (
          'Off (0V)'
          'On (5V)')
      end
    end
    object DigStimPanel2: TPanel
      Left = 4
      Top = 64
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object Label32: TLabel
        Left = 2
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object combobox32: TComboBox
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        ItemIndex = 0
        TabOrder = 0
        Text = 'Off (0V)'
        OnChange = combobox32Change
        Items.Strings = (
          'Off (0V)'
          'On (5V)')
      end
    end
    object DigStimPanel3: TPanel
      Left = 4
      Top = 88
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      object Label33: TLabel
        Left = 2
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object combobox33: TComboBox
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        ItemIndex = 0
        TabOrder = 0
        Text = 'Off (0V)'
        OnChange = combobox33Change
        Items.Strings = (
          'Off (0V)'
          'On (5V)')
      end
    end
    object DigStimPanel4: TPanel
      Left = 4
      Top = 112
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      object Label34: TLabel
        Left = 2
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object combobox34: TComboBox
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        ItemIndex = 0
        TabOrder = 0
        Text = 'Off (0V)'
        OnChange = combobox34Change
        Items.Strings = (
          'Off (0V)'
          'On (5V)')
      end
    end
    object DigStimPanel5: TPanel
      Left = 4
      Top = 136
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      object Label35: TLabel
        Left = 2
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object combobox35: TComboBox
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        ItemIndex = 0
        TabOrder = 0
        Text = 'Off (0V)'
        OnChange = combobox35Change
        Items.Strings = (
          'Off (0V)'
          'On (5V)')
      end
    end
    object DigStimPanel6: TPanel
      Left = 4
      Top = 160
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      object label36: TLabel
        Left = 2
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object combobox36: TComboBox
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        ItemIndex = 0
        TabOrder = 0
        Text = 'Off (0V)'
        OnChange = combobox36Change
        Items.Strings = (
          'Off (0V)'
          'On (5V)')
      end
    end
    object DigStimPanel7: TPanel
      Left = 4
      Top = 184
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      object Label37: TLabel
        Left = 2
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object combobox37: TComboBox
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        ItemIndex = 0
        TabOrder = 0
        Text = 'Off (0V)'
        OnChange = combobox37Change
        Items.Strings = (
          'Off (0V)'
          'On (5V)')
      end
    end
  end
  object PhotoStimGrp: TGroupBox
    Left = 0
    Top = 549
    Width = 428
    Height = 188
    Align = alTop
    Caption = 'Photo Stimulus Outputs '
    TabOrder = 3
    object PhotoStimPanel0: TPanel
      Left = 4
      Top = 17
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Label40: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object TrackBar40: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar40Change
      end
      object ValidatedEdit40: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit40KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object PhotoStimPanel1: TPanel
      Left = 4
      Top = 44
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object Label41: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object TrackBar41: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar41Change
      end
      object ValidatedEdit41: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit41KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object PhotoStimPanel2: TPanel
      Left = 4
      Top = 70
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object Label42: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object TrackBar42: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar42Change
      end
      object ValidatedEdit42: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit42KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object PhotoStimPanel4: TPanel
      Left = 4
      Top = 124
      Width = 425
      Height = 30
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      object Label144: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object TrackBar44: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar44Change
      end
      object ValidatedEdit44: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit44KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object PhotoStimPanel3: TPanel
      Left = 4
      Top = 96
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      object Label143: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object TrackBar43: TTrackBar
        Left = 260
        Top = 0
        Width = 89
        Height = 20
        Max = 10000
        Min = -10000
        TabOrder = 0
        ThumbLength = 12
        TickStyle = tsManual
        OnChange = TrackBar43Change
      end
      object ValidatedEdit43: TValidatedEdit
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        OnKeyPress = ValidatedEdit43KeyPress
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object PhotoStimPanel5: TPanel
      Left = 4
      Top = 152
      Width = 425
      Height = 25
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      object Label14: TLabel
        Left = 2
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object ComboBox2: TComboBox
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        ItemIndex = 0
        TabOrder = 0
        Text = 'Off (0V)'
        OnChange = ComboBox2Change
        Items.Strings = (
          'Off (0V)'
          'On (5V)')
      end
    end
  end
  object CameraTriggerGrp: TGroupBox
    Left = 0
    Top = 741
    Width = 428
    Height = 77
    Align = alClient
    Caption = ' Camera Trigger Output '
    TabOrder = 4
    object CameraTriggerPanel: TPanel
      Left = 4
      Top = 14
      Width = 425
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Label15: TLabel
        Left = 2
        Top = 0
        Width = 72
        Height = 15
        Caption = 'lbLSControl0'
      end
      object ComboBox3: TComboBox
        Left = 348
        Top = 0
        Width = 66
        Height = 23
        Style = csDropDownList
        ItemHeight = 15
        ItemIndex = 0
        TabOrder = 0
        Text = 'Off (0V)'
        OnChange = ComboBox3Change
        Items.Strings = (
          'Off (0V)'
          'On (5V)')
      end
    end
  end
end
