object DigDiagnosticsFrm: TDigDiagnosticsFrm
  Left = 333
  Top = 218
  Width = 388
  Height = 166
  Caption = 'Digital TTL Lines Diagnostics'
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
    Top = 0
    Width = 369
    Height = 105
    Caption = ' Digital Outputs '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
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
    object GroupBox5: TGroupBox
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
        TabOrder = 1
      end
    end
    object GroupBox6: TGroupBox
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
        TabOrder = 1
      end
    end
    object GroupBox7: TGroupBox
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
        TabOrder = 1
      end
    end
    object GroupBox8: TGroupBox
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
        TabOrder = 1
      end
    end
    object GroupBox9: TGroupBox
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
        TabOrder = 1
      end
    end
    object GroupBox10: TGroupBox
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
        TabOrder = 1
      end
    end
    object GroupBox11: TGroupBox
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
        TabOrder = 1
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
        TabOrder = 1
      end
    end
    object GroupBox12: TGroupBox
      Left = 288
      Top = 16
      Width = 30
      Height = 57
      Caption = ' 8 '
      TabOrder = 8
      object rbon8: TRadioButton
        Left = 8
        Top = 16
        Width = 17
        Height = 17
        TabOrder = 0
      end
      object rboff8: TRadioButton
        Left = 8
        Top = 32
        Width = 17
        Height = 17
        TabOrder = 1
      end
    end
    object GroupBox13: TGroupBox
      Left = 320
      Top = 16
      Width = 30
      Height = 57
      Caption = ' 9 '
      TabOrder = 9
      object rbon9: TRadioButton
        Left = 8
        Top = 16
        Width = 17
        Height = 17
        TabOrder = 0
      end
      object rboff9: TRadioButton
        Left = 8
        Top = 32
        Width = 17
        Height = 17
        TabOrder = 1
      end
    end
    object bSetDigitalOut: TButton
      Left = 32
      Top = 80
      Width = 57
      Height = 17
      Caption = 'Update'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 10
      OnClick = bSetDigitalOutClick
    end
  end
  object bOK: TButton
    Left = 8
    Top = 112
    Width = 49
    Height = 17
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
    Top = 112
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
end
