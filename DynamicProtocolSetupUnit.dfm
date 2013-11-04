object DynamicProtocolSetupFrm: TDynamicProtocolSetupFrm
  Left = 1291
  Top = 460
  BorderStyle = bsDialog
  Caption = 'Dynamic Protocol Setup'
  ClientHeight = 179
  ClientWidth = 472
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
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 18
    Top = 20
    Width = 47
    Height = 13
    Caption = 'If channel'
  end
  object Label2: TLabel
    Left = 162
    Top = 20
    Width = 36
    Height = 13
    Caption = 'remains'
  end
  object Label3: TLabel
    Left = 367
    Top = 20
    Width = 12
    Height = 13
    Caption = 'for'
  end
  object Label4: TLabel
    Left = 18
    Top = 52
    Width = 21
    Height = 13
    Caption = 'then'
  end
  object Label5: TLabel
    Left = 18
    Top = 95
    Width = 18
    Height = 13
    Caption = 'and'
  end
  object Label6: TLabel
    Left = 106
    Top = 52
    Width = 125
    Height = 13
    Caption = 'electro-physiology protocol'
  end
  object Label7: TLabel
    Left = 106
    Top = 95
    Width = 108
    Height = 13
    Caption = 'photo-stimulus protocol'
  end
  object bOK: TButton
    Left = 16
    Top = 142
    Width = 49
    Height = 25
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 0
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 72
    Top = 142
    Width = 49
    Height = 17
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 1
    OnClick = bCancelClick
  end
  object cbChannel: TComboBox
    Left = 70
    Top = 16
    Width = 89
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = 'cbChannel'
  end
  object cbDirection: TComboBox
    Left = 204
    Top = 16
    Width = 89
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    Text = 'cbDirection'
  end
  object edThreshold: TValidatedEdit
    Left = 298
    Top = 16
    Width = 65
    Height = 21
    Text = ' 0 mV'
    Scale = 1000.000000000000000000
    Units = 'mV'
    NumberFormat = '%.4g'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object edDuration: TValidatedEdit
    Left = 385
    Top = 16
    Width = 68
    Height = 21
    AutoSize = False
    Text = ' 100 ms'
    Value = 0.100000001490116100
    Scale = 1000.000000000000000000
    Units = 'ms'
    NumberFormat = '%.4g'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object Panel1: TPanel
    Left = 48
    Top = 47
    Width = 57
    Height = 41
    BevelOuter = bvNone
    TabOrder = 6
    object rbEPRestart: TRadioButton
      Left = 0
      Top = 21
      Width = 49
      Height = 17
      Caption = 'start'
      TabOrder = 0
      OnClick = rbEPRestartClick
    end
    object rbEPStop: TRadioButton
      Left = 0
      Top = 4
      Width = 49
      Height = 17
      Caption = 'stop'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = rbEPStopClick
    end
  end
  object Panel2: TPanel
    Left = 48
    Top = 90
    Width = 57
    Height = 41
    BevelOuter = bvNone
    TabOrder = 7
    object rbPSRestart: TRadioButton
      Left = 0
      Top = 21
      Width = 49
      Height = 17
      Caption = 'restart'
      TabOrder = 0
      OnClick = rbPSRestartClick
    end
    object rbPSStop: TRadioButton
      Left = 0
      Top = 4
      Width = 49
      Height = 17
      Caption = 'stop'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = rbPSStopClick
    end
  end
  object cbStimProgram: TComboBox
    Left = 104
    Top = 66
    Width = 145
    Height = 21
    Hint = 'Stimulation program in current use'
    ItemHeight = 13
    TabOrder = 8
    Text = 'cbStimProgram'
  end
  object cbPhotoStimProgram: TComboBox
    Left = 104
    Top = 109
    Width = 145
    Height = 21
    Hint = 'Stimulation program in current use'
    ItemHeight = 13
    TabOrder = 9
    Text = 'cbPhotoStimProgram'
  end
end
