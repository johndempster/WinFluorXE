object ManualControlFrm: TManualControlFrm
  Left = 192
  Top = 111
  Width = 193
  Height = 288
  Caption = ' Manual Control'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Excitation: TGroupBox
    Left = 2
    Top = 0
    Width = 175
    Height = 65
    Caption = ' Excitation Light Source '
    TabOrder = 0
    object Label2: TLabel
      Left = 8
      Top = 16
      Width = 58
      Height = 13
      Caption = 'Wavelength'
      WordWrap = True
    end
    object rbEXCShutterOpen: TRadioButton
      Left = 96
      Top = 40
      Width = 33
      Height = 17
      Alignment = taLeftJustify
      Caption = 'On'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = rbEXCShutterOpenClick
    end
    object rbEXCShutterClosed: TRadioButton
      Left = 132
      Top = 40
      Width = 36
      Height = 17
      Alignment = taLeftJustify
      Caption = ' Off'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      TabStop = True
      OnClick = rbEXCShutterOpenClick
    end
    object cbWavelength: TComboBox
      Left = 72
      Top = 16
      Width = 97
      Height = 21
      ItemHeight = 13
      TabOrder = 2
      Text = 'cbWavelength'
      OnChange = rbEXCShutterOpenClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 2
    Top = 68
    Width = 175
    Height = 41
    Caption = ' Intensifier  Shutter '
    TabOrder = 1
    object rbIntensifierActive: TRadioButton
      Left = 40
      Top = 16
      Width = 53
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Active'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = rbEXCShutterOpenClick
    end
    object rbIntensifierInactive: TRadioButton
      Left = 104
      Top = 16
      Width = 63
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Inactive'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      TabStop = True
      OnClick = rbEXCShutterOpenClick
    end
  end
  object DigitalOutGrp: TGroupBox
    Left = 2
    Top = 110
    Width = 175
    Height = 145
    Caption = ' Digital Outputs '
    TabOrder = 2
    object Dig0: TGroupBox
      Left = 40
      Top = 16
      Width = 129
      Height = 30
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 10
        Width = 29
        Height = 13
        Caption = 'Line 0'
        WordWrap = True
      end
      object rbDig0On: TRadioButton
        Left = 50
        Top = 8
        Width = 35
        Height = 17
        Alignment = taLeftJustify
        Caption = ' On'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = rbEXCShutterOpenClick
      end
      object rbDig0Off: TRadioButton
        Left = 86
        Top = 8
        Width = 35
        Height = 17
        Alignment = taLeftJustify
        Caption = ' Off'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        TabStop = True
        OnClick = rbEXCShutterOpenClick
      end
    end
    object GroupBox3: TGroupBox
      Left = 40
      Top = 46
      Width = 129
      Height = 30
      TabOrder = 1
      object Label3: TLabel
        Left = 8
        Top = 10
        Width = 29
        Height = 13
        Caption = 'Line 1'
        WordWrap = True
      end
      object rbDig1On: TRadioButton
        Left = 50
        Top = 8
        Width = 35
        Height = 17
        Alignment = taLeftJustify
        Caption = ' On'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = rbEXCShutterOpenClick
      end
      object rbDig1Off: TRadioButton
        Left = 86
        Top = 8
        Width = 35
        Height = 17
        Alignment = taLeftJustify
        Caption = ' Off'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        TabStop = True
        OnClick = rbEXCShutterOpenClick
      end
    end
    object GroupBox4: TGroupBox
      Left = 40
      Top = 76
      Width = 129
      Height = 30
      TabOrder = 2
      object Label4: TLabel
        Left = 8
        Top = 10
        Width = 29
        Height = 13
        Caption = 'Line 2'
        WordWrap = True
      end
      object rbDig2On: TRadioButton
        Left = 50
        Top = 8
        Width = 35
        Height = 17
        Alignment = taLeftJustify
        Caption = ' On'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = rbEXCShutterOpenClick
      end
      object rbDig2Off: TRadioButton
        Left = 86
        Top = 8
        Width = 35
        Height = 17
        Alignment = taLeftJustify
        Caption = ' Off'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        TabStop = True
        OnClick = rbEXCShutterOpenClick
      end
    end
    object GroupBox5: TGroupBox
      Left = 40
      Top = 106
      Width = 129
      Height = 30
      TabOrder = 3
      object Label5: TLabel
        Left = 8
        Top = 10
        Width = 29
        Height = 13
        Caption = 'Line 3'
        WordWrap = True
      end
      object rbDig3On: TRadioButton
        Left = 50
        Top = 8
        Width = 35
        Height = 17
        Alignment = taLeftJustify
        Caption = ' On'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = rbEXCShutterOpenClick
      end
      object rbDig3Off: TRadioButton
        Left = 86
        Top = 8
        Width = 35
        Height = 17
        Alignment = taLeftJustify
        Caption = ' Off'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        TabStop = True
        OnClick = rbEXCShutterOpenClick
      end
    end
  end
end
