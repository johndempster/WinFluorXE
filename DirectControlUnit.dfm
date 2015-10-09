object DirectControlFrm: TDirectControlFrm
  Tag = 6
  Left = 668
  Top = 42
  Caption = 'Device Control Outputs (Direct Control)'
  ClientHeight = 824
  ClientWidth = 311
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
    Top = 258
    Width = 311
    Height = 15
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Beveled = True
    MinSize = 20
    ExplicitTop = 249
    ExplicitWidth = 428
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 383
    Width = 311
    Height = 4
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Beveled = True
    MinSize = 20
    ExplicitTop = 323
    ExplicitWidth = 428
  end
  object Splitter3: TSplitter
    Left = 0
    Top = 605
    Width = 311
    Height = 4
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Beveled = True
    MinSize = 20
    ExplicitTop = 545
    ExplicitWidth = 428
  end
  object Splitter4: TSplitter
    Left = 0
    Top = 753
    Width = 311
    Height = 4
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Beveled = True
    MinSize = 20
    ExplicitTop = 737
    ExplicitWidth = 428
  end
  object LSControlGrp: TGroupBox
    Left = 0
    Top = 0
    Width = 311
    Height = 258
    Align = alTop
    Caption = ' Light Source Control Voltages '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitWidth = 305
    object LightSourcePanel0: TPanel
      Left = 8
      Top = 16
      Width = 293
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
      object ValidatedEdit0: TValidatedEdit
        Left = 220
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
      Left = 8
      Top = 41
      Width = 294
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
      object ValidatedEdit1: TValidatedEdit
        Left = 220
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
    object LightSourcePanel2: TPanel
      Left = 8
      Top = 65
      Width = 293
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
      object ValidatedEdit2: TValidatedEdit
        Left = 220
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
    object LightSourcePanel3: TPanel
      Left = 8
      Top = 90
      Width = 293
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      object Label4: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit4: TValidatedEdit
        Left = 220
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
    object LightSourcePanel4: TPanel
      Left = 8
      Top = 116
      Width = 293
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
      object ValidatedEdit5: TValidatedEdit
        Left = 220
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
    object LightSourcePanel5: TPanel
      Left = 8
      Top = 140
      Width = 293
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
      object ValidatedEdit6: TValidatedEdit
        Left = 220
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
    object LightSourcePanel6: TPanel
      Left = 8
      Top = 165
      Width = 293
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      object Label3: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit3: TValidatedEdit
        Left = 220
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
    object LightSourcePanel7: TPanel
      Left = 8
      Top = 192
      Width = 293
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      object Label10: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit7: TValidatedEdit
        Left = 220
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
    object ShutterPanel: TPanel
      Left = 8
      Top = 221
      Width = 293
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      object Label9: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit8: TValidatedEdit
        Left = 220
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
  end
  object VStimGrp: TGroupBox
    Left = 0
    Top = 273
    Width = 311
    Height = 110
    Align = alTop
    Caption = ' Voltage Stimulus Outputs  '
    TabOrder = 1
    ExplicitWidth = 305
    object VStimPanel0: TPanel
      Left = 8
      Top = 16
      Width = 293
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
      object validatededit20: TValidatedEdit
        Left = 220
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
    object VStimPanel1: TPanel
      Left = 8
      Top = 42
      Width = 293
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
      object ValidatedEdit21: TValidatedEdit
        Left = 220
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
    object VStimPanel2: TPanel
      Left = 8
      Top = 68
      Width = 293
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
      object ValidatedEdit22: TValidatedEdit
        Left = 220
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
  end
  object DigStimGrp: TGroupBox
    Left = 0
    Top = 387
    Width = 311
    Height = 218
    Align = alTop
    Caption = ' Digital Stimulus Outputs '
    TabOrder = 2
    ExplicitWidth = 305
    object DigStimPanel0: TPanel
      Left = 8
      Top = 16
      Width = 293
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Label11: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit9: TValidatedEdit
        Left = 220
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
    object DigStimPanel1: TPanel
      Left = 8
      Top = 43
      Width = 294
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object Label12: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit10: TValidatedEdit
        Left = 220
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
    object DigStimPanel2: TPanel
      Left = 8
      Top = 73
      Width = 293
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object Label13: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit11: TValidatedEdit
        Left = 220
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
    object DigStimPanel3: TPanel
      Left = 8
      Top = 98
      Width = 293
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      object Label16: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit12: TValidatedEdit
        Left = 220
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
    object DigStimPanel4: TPanel
      Left = 8
      Top = 124
      Width = 293
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      object Label17: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit13: TValidatedEdit
        Left = 220
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
    object DigStimPanel5: TPanel
      Left = 8
      Top = 148
      Width = 293
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      object Label18: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit14: TValidatedEdit
        Left = 220
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
    object DigStimPanel6: TPanel
      Left = 8
      Top = 173
      Width = 293
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      object Label19: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit15: TValidatedEdit
        Left = 220
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
    object DigStimPanel7: TPanel
      Left = 8
      Top = 194
      Width = 293
      Height = 24
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      object Label21: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit16: TValidatedEdit
        Left = 220
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
  end
  object PhotoStimGrp: TGroupBox
    Left = 0
    Top = 609
    Width = 311
    Height = 144
    Align = alTop
    Caption = 'Photo Stimulus Outputs '
    TabOrder = 3
    ExplicitWidth = 305
    object PhotoStimPanel0: TPanel
      Left = 8
      Top = 17
      Width = 293
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
      object ValidatedEdit40: TValidatedEdit
        Left = 220
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
    object PhotoStimPanel1: TPanel
      Left = 8
      Top = 44
      Width = 293
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
      object ValidatedEdit41: TValidatedEdit
        Left = 220
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
    object PhotoStimPanel2: TPanel
      Left = 8
      Top = 70
      Width = 293
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
      object ValidatedEdit42: TValidatedEdit
        Left = 220
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
    object PhotoStimPanel4: TPanel
      Left = 8
      Top = 124
      Width = 293
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
      object ValidatedEdit44: TValidatedEdit
        Left = 220
        Top = 0
        Width = 66
        Height = 23
        Text = ' 0 V'
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -10.000000000000000000
        HiLimit = 10.000000000000000000
      end
    end
    object PhotoStimPanel3: TPanel
      Left = 8
      Top = 96
      Width = 293
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
      object ValidatedEdit43: TValidatedEdit
        Left = 220
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
    object PhotoStimPanel5: TPanel
      Left = 8
      Top = 152
      Width = 425
      Height = 30
      BevelOuter = bvNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      object Label22: TLabel
        Left = 2
        Top = 0
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit18: TValidatedEdit
        Left = 220
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
  end
  object CameraTriggerGrp: TGroupBox
    Left = 0
    Top = 757
    Width = 311
    Height = 67
    Align = alClient
    Caption = ' Camera Trigger Output '
    TabOrder = 4
    ExplicitLeft = 88
    ExplicitTop = 748
    ExplicitHeight = 15
    object CameraTriggerPanel: TPanel
      Left = 8
      Top = 12
      Width = 293
      Height = 30
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
        Width = 31
        Height = 15
        Caption = 'Label'
      end
      object ValidatedEdit19: TValidatedEdit
        Left = 220
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
      object Button1: TButton
        Left = 192
        Top = 32
        Width = 49
        Height = 1
        Caption = 'Button1'
        TabOrder = 1
      end
    end
    object bUpdateOutputs: TButton
      Left = 192
      Top = 40
      Width = 105
      Height = 17
      Caption = 'Update Outputs'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = bUpdateOutputsClick
    end
  end
end
