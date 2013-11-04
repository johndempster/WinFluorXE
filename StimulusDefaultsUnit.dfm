object StimulusDefaultsFrm: TStimulusDefaultsFrm
  Left = 562
  Top = 291
  Width = 538
  Height = 244
  Caption = ' Stimulus Outputs (Default Settings)'
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
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object DIGGroup: TGroupBox
    Left = 8
    Top = 64
    Width = 513
    Height = 105
    Caption = ' Digital Output States '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label4: TLabel
      Left = 8
      Top = 32
      Width = 15
      Height = 14
      Caption = 'On'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Top = 50
      Width = 16
      Height = 14
      Caption = 'Off'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Dig1: TGroupBox
      Left = 64
      Top = 16
      Width = 30
      Height = 57
      Caption = ' 1 '
      TabOrder = 0
      object rbOn1: TRadioButton
        Left = 8
        Top = 16
        Width = 17
        Height = 17
        TabOrder = 0
      end
      object rbOff1: TRadioButton
        Left = 8
        Top = 32
        Width = 17
        Height = 17
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object Dig3: TGroupBox
      Left = 128
      Top = 16
      Width = 30
      Height = 57
      Caption = ' 3 '
      TabOrder = 1
      object rbOn3: TRadioButton
        Left = 8
        Top = 16
        Width = 17
        Height = 17
        Caption = 'rbOn3'
        TabOrder = 0
      end
      object rboff3: TRadioButton
        Left = 8
        Top = 32
        Width = 17
        Height = 17
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object Dig4: TGroupBox
      Left = 160
      Top = 16
      Width = 30
      Height = 57
      Caption = ' 4 '
      TabOrder = 2
      object rbon4: TRadioButton
        Left = 8
        Top = 16
        Width = 17
        Height = 17
        TabOrder = 0
      end
      object rboff4: TRadioButton
        Left = 8
        Top = 32
        Width = 17
        Height = 17
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object Dig5: TGroupBox
      Left = 192
      Top = 16
      Width = 30
      Height = 57
      Caption = ' 5 '
      TabOrder = 3
      object rbon5: TRadioButton
        Left = 8
        Top = 16
        Width = 17
        Height = 17
        TabOrder = 0
      end
      object rboff5: TRadioButton
        Left = 8
        Top = 32
        Width = 17
        Height = 17
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object Dig6: TGroupBox
      Left = 224
      Top = 16
      Width = 30
      Height = 57
      Caption = ' 6 '
      TabOrder = 4
      object rbon6: TRadioButton
        Left = 8
        Top = 16
        Width = 17
        Height = 17
        TabOrder = 0
      end
      object rboff6: TRadioButton
        Left = 8
        Top = 32
        Width = 17
        Height = 17
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object Dig7: TGroupBox
      Left = 256
      Top = 16
      Width = 30
      Height = 57
      Caption = ' 7 '
      TabOrder = 5
      object rbon7: TRadioButton
        Left = 8
        Top = 16
        Width = 17
        Height = 17
        TabOrder = 0
      end
      object rboff7: TRadioButton
        Left = 8
        Top = 32
        Width = 17
        Height = 17
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object Dig2: TGroupBox
      Left = 96
      Top = 16
      Width = 30
      Height = 57
      Caption = ' 2 '
      TabOrder = 6
      object rbOn2: TRadioButton
        Left = 8
        Top = 16
        Width = 17
        Height = 17
        Caption = 'rbOn2'
        TabOrder = 0
      end
      object rbOff2: TRadioButton
        Left = 8
        Top = 32
        Width = 17
        Height = 17
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
    object Dig0: TGroupBox
      Left = 32
      Top = 16
      Width = 30
      Height = 57
      Caption = ' 0 '
      TabOrder = 7
      object rbOn0: TRadioButton
        Left = 8
        Top = 16
        Width = 17
        Height = 17
        Caption = 'rbOn0'
        TabOrder = 0
      end
      object rbOff0: TRadioButton
        Left = 8
        Top = 32
        Width = 17
        Height = 17
        Checked = True
        TabOrder = 1
        TabStop = True
      end
    end
  end
  object bOK: TButton
    Left = 8
    Top = 176
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
    TabOrder = 1
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 64
    Top = 176
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
    TabOrder = 2
    OnClick = bCancelClick
  end
  object DACGroup: TGroupBox
    Left = 8
    Top = 0
    Width = 513
    Height = 57
    Caption = ' DAC Output Holding Voltages '
    TabOrder = 3
    object VCommand0Panel: TPanel
      Left = 8
      Top = 16
      Width = 161
      Height = 33
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 72
        Height = 14
        Caption = 'Voltage O/P 1'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object edVCommand0: TValidatedEdit
        Left = 88
        Top = 2
        Width = 65
        Height = 23
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        Text = ' 0 mV'
        Scale = 1000.000000000000000000
        Units = 'mV'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
    end
    object VCommand1Panel: TPanel
      Left = 176
      Top = 16
      Width = 161
      Height = 33
      BevelOuter = bvNone
      TabOrder = 1
      object Label2: TLabel
        Left = 8
        Top = 8
        Width = 72
        Height = 14
        Caption = 'Voltage O/P 2'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object edVCommand1: TValidatedEdit
        Left = 88
        Top = 2
        Width = 65
        Height = 23
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        Text = ' 0 mV'
        Scale = 1000.000000000000000000
        Units = 'mV'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
    end
    object VCommand2Panel: TPanel
      Left = 344
      Top = 16
      Width = 161
      Height = 33
      BevelOuter = bvNone
      TabOrder = 2
      object Label3: TLabel
        Left = 8
        Top = 8
        Width = 72
        Height = 14
        Caption = 'Voltage O/P 3'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object edVCommand2: TValidatedEdit
        Left = 88
        Top = 2
        Width = 65
        Height = 23
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        Text = ' 0 mV'
        Scale = 1000.000000000000000000
        Units = 'mV'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
    end
  end
end
