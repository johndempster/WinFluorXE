object SetLasersFrm: TSetLasersFrm
  Tag = 38
  Left = 590
  Top = 268
  Caption = 'Set Light Source Intensity'
  ClientHeight = 248
  ClientWidth = 274
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object LasersGrp: TGroupBox
    Left = 4
    Top = 0
    Width = 253
    Height = 216
    TabOrder = 0
    object panSource1: TPanel
      Left = 4
      Top = 12
      Width = 245
      Height = 24
      BevelOuter = bvNone
      TabOrder = 0
      object lbName1: TLabel
        Left = 27
        Top = 2
        Width = 50
        Height = 15
        Alignment = taRightJustify
        Caption = 'lbName1'
      end
      object sbIntensity1: TScrollBar
        Left = 82
        Top = 2
        Width = 100
        Height = 15
        PageSize = 0
        TabOrder = 0
        OnChange = sbIntensity1Change
      end
      object edIntensity1: TValidatedEdit
        Left = 186
        Top = 2
        Width = 49
        Height = 20
        Hint = '% Maximum Intensity'
        OnKeyPress = edIntensity1KeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 0 %'
        Scale = 1.000000000000000000
        Units = '%'
        NumberFormat = '%.4g'
        HiLimit = 100.000000000000000000
      end
    end
    object panSource2: TPanel
      Left = 4
      Top = 39
      Width = 245
      Height = 24
      BevelOuter = bvNone
      TabOrder = 1
      object lbName2: TLabel
        Left = 26
        Top = 2
        Width = 51
        Height = 15
        Alignment = taRightJustify
        Caption = 'Source 2'
      end
      object sbIntensity2: TScrollBar
        Left = 82
        Top = 2
        Width = 100
        Height = 15
        PageSize = 0
        TabOrder = 0
        OnChange = sbIntensity1Change
      end
      object edIntensity2: TValidatedEdit
        Left = 186
        Top = 2
        Width = 49
        Height = 20
        Hint = '% Maximum Intensity'
        OnKeyPress = edIntensity1KeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 0 %'
        Scale = 1.000000000000000000
        Units = '%'
        NumberFormat = '%.4g'
        HiLimit = 100.000000000000000000
      end
    end
    object panSource3: TPanel
      Left = 4
      Top = 63
      Width = 245
      Height = 24
      BevelOuter = bvNone
      TabOrder = 2
      object lbName3: TLabel
        Left = 26
        Top = 2
        Width = 51
        Height = 15
        Alignment = taRightJustify
        Caption = 'Source 3'
      end
      object sbIntensity3: TScrollBar
        Left = 82
        Top = 2
        Width = 100
        Height = 15
        PageSize = 0
        TabOrder = 0
        OnChange = sbIntensity1Change
      end
      object edIntensity3: TValidatedEdit
        Left = 186
        Top = 2
        Width = 49
        Height = 20
        Hint = '% Maximum Intensity'
        OnKeyPress = edIntensity1KeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 0 %'
        Scale = 1.000000000000000000
        Units = '%'
        NumberFormat = '%.4g'
        HiLimit = 100.000000000000000000
      end
    end
    object panSource4: TPanel
      Left = 4
      Top = 87
      Width = 245
      Height = 24
      BevelOuter = bvNone
      TabOrder = 3
      object lbName4: TLabel
        Left = 26
        Top = 2
        Width = 51
        Height = 15
        Alignment = taRightJustify
        Caption = 'Source 4'
      end
      object sbIntensity4: TScrollBar
        Left = 82
        Top = 2
        Width = 100
        Height = 15
        PageSize = 0
        TabOrder = 0
        OnChange = sbIntensity1Change
      end
      object edIntensity4: TValidatedEdit
        Left = 186
        Top = 2
        Width = 49
        Height = 20
        Hint = '% Maximum Intensity'
        OnKeyPress = edIntensity1KeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 0 %'
        Scale = 1.000000000000000000
        Units = '%'
        NumberFormat = '%.4g'
        HiLimit = 100.000000000000000000
      end
    end
    object panSource5: TPanel
      Left = 4
      Top = 111
      Width = 245
      Height = 24
      BevelOuter = bvNone
      TabOrder = 4
      object lbName5: TLabel
        Left = 26
        Top = 2
        Width = 51
        Height = 15
        Alignment = taRightJustify
        Caption = 'Source 5'
      end
      object sbIntensity5: TScrollBar
        Left = 82
        Top = 2
        Width = 100
        Height = 15
        PageSize = 0
        TabOrder = 0
        OnChange = sbIntensity1Change
      end
      object edIntensity5: TValidatedEdit
        Left = 186
        Top = 2
        Width = 49
        Height = 20
        Hint = '% Maximum Intensity'
        OnKeyPress = edIntensity1KeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 0 %'
        Scale = 1.000000000000000000
        Units = '%'
        NumberFormat = '%.4g'
        HiLimit = 100.000000000000000000
      end
    end
    object panSource6: TPanel
      Left = 4
      Top = 135
      Width = 245
      Height = 24
      BevelOuter = bvNone
      TabOrder = 5
      object lbName6: TLabel
        Left = 26
        Top = 2
        Width = 51
        Height = 15
        Alignment = taRightJustify
        Caption = 'Source 6'
      end
      object sbIntensity6: TScrollBar
        Left = 82
        Top = 2
        Width = 100
        Height = 15
        PageSize = 0
        TabOrder = 0
        OnChange = sbIntensity1Change
      end
      object edIntensity6: TValidatedEdit
        Left = 186
        Top = 2
        Width = 49
        Height = 20
        Hint = '% Maximum Intensity'
        OnKeyPress = edIntensity1KeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 0 %'
        Scale = 1.000000000000000000
        Units = '%'
        NumberFormat = '%.4g'
        HiLimit = 100.000000000000000000
      end
    end
    object panSource7: TPanel
      Left = 4
      Top = 159
      Width = 245
      Height = 24
      BevelOuter = bvNone
      TabOrder = 6
      object lbName7: TLabel
        Left = 26
        Top = 2
        Width = 51
        Height = 15
        Alignment = taRightJustify
        Caption = 'Source 7'
      end
      object sbIntensity7: TScrollBar
        Left = 82
        Top = 2
        Width = 100
        Height = 15
        PageSize = 0
        TabOrder = 0
        OnChange = sbIntensity1Change
      end
      object edIntensity7: TValidatedEdit
        Left = 186
        Top = 2
        Width = 49
        Height = 20
        Hint = '% Maximum Intensity'
        OnKeyPress = edIntensity1KeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 0 %'
        Scale = 1.000000000000000000
        Units = '%'
        NumberFormat = '%.4g'
        HiLimit = 100.000000000000000000
      end
    end
    object panSource8: TPanel
      Left = 4
      Top = 187
      Width = 245
      Height = 24
      BevelOuter = bvNone
      TabOrder = 7
      object lbName8: TLabel
        Left = 26
        Top = 2
        Width = 51
        Height = 15
        Alignment = taRightJustify
        Caption = 'Source 8'
      end
      object sbIntensity8: TScrollBar
        Left = 82
        Top = 2
        Width = 100
        Height = 15
        PageSize = 0
        TabOrder = 0
        OnChange = sbIntensity1Change
      end
      object edIntensity8: TValidatedEdit
        Left = 186
        Top = 2
        Width = 49
        Height = 20
        Hint = '% Maximum Intensity'
        OnKeyPress = edIntensity1KeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 0 %'
        Scale = 1.000000000000000000
        Units = '%'
        NumberFormat = '%.4g'
        HiLimit = 100.000000000000000000
      end
    end
  end
  object bOK: TButton
    Left = 8
    Top = 222
    Width = 49
    Height = 20
    Caption = 'OK'
    TabOrder = 1
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 70
    Top = 222
    Width = 48
    Height = 17
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
end
