object PhotoStimSetupFrm: TPhotoStimSetupFrm
  Left = 716
  Top = 422
  BorderStyle = bsDialog
  Caption = 'Photolysis Attenuation Setup'
  ClientHeight = 417
  ClientWidth = 497
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
  object bOK: TButton
    Left = 13
    Top = 376
    Width = 49
    Height = 26
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
    Left = 69
    Top = 377
    Width = 49
    Height = 17
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 1
    OnClick = bCancelClick
  end
  object SetupTab: TPageControl
    Left = 13
    Top = 10
    Width = 468
    Height = 359
    ActivePage = Attenuator1Tab
    TabOrder = 2
    object Attenuator1Tab: TTabSheet
      Caption = 'Attenuator 1'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ImageIndex = 1
      ParentFont = False
      object Label30: TLabel
        Left = 311
        Top = 10
        Width = 51
        Height = 14
        Caption = 'X Center:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label32: TLabel
        Left = 311
        Top = 34
        Width = 51
        Height = 14
        Caption = 'Y Center:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label33: TLabel
        Left = 320
        Top = 58
        Width = 42
        Height = 14
        Caption = 'X Scale:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label34: TLabel
        Left = 320
        Top = 82
        Width = 42
        Height = 14
        Caption = 'Y Scale:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Shape2: TShape
        Left = 296
        Top = 7
        Width = 1
        Height = 310
      end
      object rbPockelsCellA1: TRadioButton
        Left = 8
        Top = 9
        Width = 97
        Height = 17
        Caption = 'Pockels Cell'
        TabOrder = 0
        OnClick = rbPockelsCellA1Click
      end
      object Panel3: TPanel
        Left = 104
        Top = 0
        Width = 177
        Height = 185
        BevelOuter = bvNone
        TabOrder = 1
        object Label50: TLabel
          Left = 20
          Top = 144
          Width = 59
          Height = 14
          Caption = 'Conoptics:'
        end
        object Label49: TLabel
          Left = 12
          Top = 106
          Width = 67
          Height = 14
          Caption = 'Polarization:'
        end
        object Label45: TLabel
          Left = 17
          Top = 10
          Width = 62
          Height = 14
          Caption = 'Min Power:'
        end
        object Label46: TLabel
          Left = 15
          Top = 34
          Width = 64
          Height = 14
          Caption = 'Max Power:'
        end
        object Label48: TLabel
          Left = 21
          Top = 58
          Width = 58
          Height = 14
          Caption = 'Voltage Pi:'
        end
        object Label47: TLabel
          Left = 32
          Top = 82
          Width = 47
          Height = 14
          Caption = 'Net Bias:'
        end
        object Panel2: TPanel
          Left = 88
          Top = 144
          Width = 89
          Height = 35
          BevelOuter = bvNone
          ParentBackground = True
          TabOrder = 0
          object rbConoptics302A1: TRadioButton
            Left = 0
            Top = 0
            Width = 49
            Height = 17
            Caption = '302'
            Enabled = False
            TabOrder = 0
          end
          object rbConoptics302RMA1: TRadioButton
            Left = 0
            Top = 16
            Width = 57
            Height = 17
            Caption = '302 RM'
            Enabled = False
            TabOrder = 1
          end
        end
        object Panel1: TPanel
          Left = 88
          Top = 106
          Width = 89
          Height = 33
          BevelOuter = bvNone
          ParentBackground = True
          TabOrder = 1
          object rbPolarizationCrossA1: TRadioButton
            Left = 0
            Top = 16
            Width = 89
            Height = 17
            Caption = 'Cross (sin)'
            TabOrder = 0
          end
          object rbPolarizationParallelA1: TRadioButton
            Left = 0
            Top = 0
            Width = 89
            Height = 17
            Caption = 'Parallel (cos)'
            TabOrder = 1
          end
        end
        object edBiasSettingA1: TValidatedEdit
          Left = 88
          Top = 79
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edVoltagePiA1: TValidatedEdit
          Left = 88
          Top = 55
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edPowerMaximumA1: TValidatedEdit
          Left = 88
          Top = 31
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edPowerMinimumA1: TValidatedEdit
          Left = 88
          Top = 7
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
      end
      object rbLinearRampA1: TRadioButton
        Left = 8
        Top = 200
        Width = 97
        Height = 17
        Caption = 'Linear Ramp'
        TabOrder = 2
        OnClick = rbLinearRampA1Click
      end
      object Panel4: TPanel
        Left = 104
        Top = 192
        Width = 177
        Height = 105
        BevelOuter = bvNone
        TabOrder = 3
        object Label1: TLabel
          Left = 17
          Top = 10
          Width = 62
          Height = 14
          Caption = 'Min Power:'
        end
        object Label2: TLabel
          Left = 15
          Top = 34
          Width = 64
          Height = 14
          Caption = 'Max Power:'
        end
        object Label3: TLabel
          Left = 11
          Top = 58
          Width = 68
          Height = 14
          Caption = 'Min Voltage:'
        end
        object Label4: TLabel
          Left = 9
          Top = 82
          Width = 70
          Height = 14
          Caption = 'Max Voltage:'
        end
        object edLinearPowerMinimumA1: TValidatedEdit
          Left = 88
          Top = 7
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edLinearPowerMaximumA1: TValidatedEdit
          Left = 88
          Top = 31
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edLinearVoltageMinimumA1: TValidatedEdit
          Left = 88
          Top = 55
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edLinearVoltageMaximumA1: TValidatedEdit
          Left = 88
          Top = 79
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
      end
      object cbEnableShutterA1: TCheckBox
        Left = 9
        Top = 304
        Width = 104
        Height = 17
        Caption = 'Enable Shutter'
        TabOrder = 4
      end
      object edPhotoStimXCenter1: TValidatedEdit
        Left = 368
        Top = 7
        Width = 81
        Height = 20
        AutoSize = False
        Text = ' 0.1 V'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edPhotoStimYCenter1: TValidatedEdit
        Left = 368
        Top = 31
        Width = 81
        Height = 20
        AutoSize = False
        Text = ' 0.1 V'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edPhotoStimXScale1: TValidatedEdit
        Left = 368
        Top = 55
        Width = 81
        Height = 20
        AutoSize = False
        Text = ' 0.1 V/um'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V/um'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edPhotoStimYScale1: TValidatedEdit
        Left = 368
        Top = 79
        Width = 81
        Height = 20
        AutoSize = False
        Text = ' 0.1 V/um'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V/um'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
    end
    object Attenuator2Tab: TTabSheet
      Caption = 'Attenuator 2'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ImageIndex = 2
      ParentFont = False
      object Label35: TLabel
        Left = 311
        Top = 10
        Width = 51
        Height = 14
        Caption = 'X Center:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label36: TLabel
        Left = 311
        Top = 34
        Width = 51
        Height = 14
        Caption = 'Y Center:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label37: TLabel
        Left = 320
        Top = 58
        Width = 42
        Height = 14
        Caption = 'X Scale:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label38: TLabel
        Left = 320
        Top = 82
        Width = 42
        Height = 14
        Caption = 'Y Scale:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Shape3: TShape
        Left = 296
        Top = 7
        Width = 1
        Height = 310
      end
      object rbPockelsCellA2: TRadioButton
        Left = 8
        Top = 9
        Width = 97
        Height = 17
        Caption = 'Pockels Cell'
        TabOrder = 0
        OnClick = rbPockelsCellA2Click
      end
      object Panel5: TPanel
        Left = 104
        Top = 0
        Width = 177
        Height = 185
        BevelOuter = bvNone
        TabOrder = 1
        object Label5: TLabel
          Left = 20
          Top = 144
          Width = 59
          Height = 14
          Caption = 'Conoptics:'
        end
        object Label6: TLabel
          Left = 12
          Top = 106
          Width = 67
          Height = 14
          Caption = 'Polarization:'
        end
        object Label7: TLabel
          Left = 17
          Top = 10
          Width = 62
          Height = 14
          Caption = 'Min Power:'
        end
        object Label8: TLabel
          Left = 15
          Top = 34
          Width = 64
          Height = 14
          Caption = 'Max Power:'
        end
        object Label9: TLabel
          Left = 21
          Top = 58
          Width = 58
          Height = 14
          Caption = 'Voltage Pi:'
        end
        object Label14: TLabel
          Left = 32
          Top = 82
          Width = 47
          Height = 14
          Caption = 'Net Bias:'
        end
        object Panel6: TPanel
          Left = 88
          Top = 144
          Width = 89
          Height = 35
          BevelOuter = bvNone
          ParentBackground = True
          TabOrder = 0
          object rbConoptics302A2: TRadioButton
            Left = 0
            Top = 0
            Width = 49
            Height = 17
            Caption = '302'
            Enabled = False
            TabOrder = 0
          end
          object rbConoptics302RMA2: TRadioButton
            Left = 0
            Top = 16
            Width = 57
            Height = 17
            Caption = '302 RM'
            Enabled = False
            TabOrder = 1
          end
        end
        object Panel7: TPanel
          Left = 88
          Top = 106
          Width = 89
          Height = 33
          BevelOuter = bvNone
          ParentBackground = True
          TabOrder = 1
          object rbPolarizationCrossA2: TRadioButton
            Left = 0
            Top = 16
            Width = 89
            Height = 17
            Caption = 'Cross (sin)'
            TabOrder = 0
          end
          object rbPolarizationParallelA2: TRadioButton
            Left = 0
            Top = 0
            Width = 89
            Height = 17
            Caption = 'Parallel (cos)'
            TabOrder = 1
          end
        end
        object edBiasSettingA2: TValidatedEdit
          Left = 88
          Top = 79
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edVoltagePiA2: TValidatedEdit
          Left = 88
          Top = 55
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edPowerMaximumA2: TValidatedEdit
          Left = 88
          Top = 31
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edPowerMinimumA2: TValidatedEdit
          Left = 88
          Top = 7
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
      end
      object rbLinearRampA2: TRadioButton
        Left = 8
        Top = 200
        Width = 97
        Height = 17
        Caption = 'Linear Ramp'
        TabOrder = 2
        OnClick = rbLinearRampA2Click
      end
      object Panel8: TPanel
        Left = 104
        Top = 192
        Width = 177
        Height = 105
        BevelOuter = bvNone
        TabOrder = 3
        object Label15: TLabel
          Left = 17
          Top = 10
          Width = 62
          Height = 14
          Caption = 'Min Power:'
        end
        object Label16: TLabel
          Left = 15
          Top = 34
          Width = 64
          Height = 14
          Caption = 'Max Power:'
        end
        object Label17: TLabel
          Left = 11
          Top = 58
          Width = 68
          Height = 14
          Caption = 'Min Voltage:'
        end
        object Label18: TLabel
          Left = 9
          Top = 82
          Width = 70
          Height = 14
          Caption = 'Max Voltage:'
        end
        object edLinearPowerMinimumA2: TValidatedEdit
          Left = 88
          Top = 7
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edLinearPowerMaximumA2: TValidatedEdit
          Left = 88
          Top = 31
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edLinearVoltageMinimumA2: TValidatedEdit
          Left = 88
          Top = 55
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edLinearVoltageMaximumA2: TValidatedEdit
          Left = 88
          Top = 79
          Width = 89
          Height = 20
          AutoSize = False
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
      end
      object cbEnableShutterA2: TCheckBox
        Left = 9
        Top = 304
        Width = 104
        Height = 17
        Caption = 'Enable Shutter'
        TabOrder = 4
      end
      object edPhotoStimXCenter2: TValidatedEdit
        Left = 368
        Top = 7
        Width = 81
        Height = 20
        AutoSize = False
        Text = ' 0.1 V'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edPhotoStimYCenter2: TValidatedEdit
        Left = 368
        Top = 31
        Width = 81
        Height = 20
        AutoSize = False
        Text = ' 0.1 V'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edPhotoStimXScale2: TValidatedEdit
        Left = 368
        Top = 55
        Width = 81
        Height = 20
        AutoSize = False
        Text = ' 0.1 V/um'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V/um'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edPhotoStimYScale2: TValidatedEdit
        Left = 368
        Top = 79
        Width = 81
        Height = 20
        AutoSize = False
        Text = ' 0.1 V/um'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V/um'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
    end
    object Attenuator3Tab: TTabSheet
      Caption = 'Attenuator 3'
      ImageIndex = 3
      object Label19: TLabel
        Left = 17
        Top = 10
        Width = 53
        Height = 13
        Caption = 'Min Power:'
      end
      object Shape4: TShape
        Left = 296
        Top = 7
        Width = 1
        Height = 310
      end
      object Label39: TLabel
        Left = 311
        Top = 10
        Width = 51
        Height = 14
        Caption = 'X Center:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label40: TLabel
        Left = 311
        Top = 34
        Width = 51
        Height = 14
        Caption = 'Y Center:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label41: TLabel
        Left = 320
        Top = 58
        Width = 42
        Height = 14
        Caption = 'X Scale:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label42: TLabel
        Left = 320
        Top = 82
        Width = 42
        Height = 14
        Caption = 'Y Scale:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object rbPockelsCellA3: TRadioButton
        Left = 8
        Top = 9
        Width = 97
        Height = 17
        Caption = 'Pockels Cell'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = rbPockelsCellA3Click
      end
      object rbLinearRampA3: TRadioButton
        Left = 8
        Top = 200
        Width = 97
        Height = 17
        Caption = 'Linear Ramp'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = rbLinearRampA3Click
      end
      object Panel9: TPanel
        Left = 104
        Top = 0
        Width = 177
        Height = 185
        BevelOuter = bvNone
        TabOrder = 2
        object Label20: TLabel
          Left = 20
          Top = 144
          Width = 59
          Height = 14
          Caption = 'Conoptics:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label21: TLabel
          Left = 12
          Top = 106
          Width = 67
          Height = 14
          Caption = 'Polarization:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label22: TLabel
          Left = 17
          Top = 10
          Width = 62
          Height = 14
          Caption = 'Min Power:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label23: TLabel
          Left = 15
          Top = 34
          Width = 64
          Height = 14
          Caption = 'Max Power:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label24: TLabel
          Left = 21
          Top = 58
          Width = 58
          Height = 14
          Caption = 'Voltage Pi:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label25: TLabel
          Left = 32
          Top = 82
          Width = 47
          Height = 14
          Caption = 'Net Bias:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Panel10: TPanel
          Left = 88
          Top = 144
          Width = 89
          Height = 35
          BevelOuter = bvNone
          ParentBackground = True
          TabOrder = 0
          object rbConoptics302A3: TRadioButton
            Left = 0
            Top = 0
            Width = 49
            Height = 17
            Caption = '302'
            Enabled = False
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
          end
          object rbConoptics302RMA3: TRadioButton
            Left = 0
            Top = 16
            Width = 57
            Height = 17
            Caption = '302 RM'
            Enabled = False
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
          end
        end
        object Panel11: TPanel
          Left = 88
          Top = 106
          Width = 89
          Height = 33
          BevelOuter = bvNone
          ParentBackground = True
          TabOrder = 1
          object rbPolarizationCrossA3: TRadioButton
            Left = 0
            Top = 16
            Width = 89
            Height = 17
            Caption = 'Cross (sin)'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
          end
          object rbPolarizationParallelA3: TRadioButton
            Left = 0
            Top = 0
            Width = 89
            Height = 17
            Caption = 'Parallel (cos)'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
          end
        end
        object edBiasSettingA3: TValidatedEdit
          Left = 88
          Top = 79
          Width = 89
          Height = 20
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edVoltagePiA3: TValidatedEdit
          Left = 88
          Top = 55
          Width = 89
          Height = 20
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edPowerMaximumA3: TValidatedEdit
          Left = 88
          Top = 31
          Width = 89
          Height = 20
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edPowerMinimumA3: TValidatedEdit
          Left = 88
          Top = 7
          Width = 89
          Height = 20
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
      end
      object Panel12: TPanel
        Left = 104
        Top = 192
        Width = 177
        Height = 105
        BevelOuter = bvNone
        TabOrder = 3
        object Label26: TLabel
          Left = 17
          Top = 10
          Width = 62
          Height = 14
          Caption = 'Min Power:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label27: TLabel
          Left = 15
          Top = 34
          Width = 64
          Height = 14
          Caption = 'Max Power:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label28: TLabel
          Left = 11
          Top = 58
          Width = 68
          Height = 14
          Caption = 'Min Voltage:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label29: TLabel
          Left = 9
          Top = 82
          Width = 70
          Height = 14
          Caption = 'Max Voltage:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edLinearPowerMinimumA3: TValidatedEdit
          Left = 88
          Top = 7
          Width = 89
          Height = 20
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edLinearPowerMaximumA3: TValidatedEdit
          Left = 88
          Top = 31
          Width = 89
          Height = 20
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ShowHint = True
          Text = ' 0 mW'
          Scale = 1.000000000000000000
          Units = 'mW'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edLinearVoltageMinimumA3: TValidatedEdit
          Left = 88
          Top = 55
          Width = 89
          Height = 20
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edLinearVoltageMaximumA3: TValidatedEdit
          Left = 88
          Top = 79
          Width = 89
          Height = 20
          AutoSize = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ShowHint = True
          Text = ' 0 V'
          Scale = 1.000000000000000000
          Units = 'V'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
      end
      object cbEnableShutterA3: TCheckBox
        Left = 9
        Top = 304
        Width = 104
        Height = 17
        Caption = 'Enable Shutter'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
      end
      object edPhotoStimXCenter3: TValidatedEdit
        Left = 368
        Top = 7
        Width = 81
        Height = 20
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Text = ' 0.1 V'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edPhotoStimYCenter3: TValidatedEdit
        Left = 368
        Top = 31
        Width = 81
        Height = 20
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Text = ' 0.1 V'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edPhotoStimXScale3: TValidatedEdit
        Left = 368
        Top = 55
        Width = 81
        Height = 20
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Text = ' 0.1 V/um'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V/um'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edPhotoStimYScale3: TValidatedEdit
        Left = 368
        Top = 79
        Width = 81
        Height = 20
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Text = ' 0.1 V/um'
        Value = 0.100000001490116100
        Scale = 1.000000000000000000
        Units = 'V/um'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
    end
    object PrairieView: TTabSheet
      Caption = 'PrairieView'
      ImageIndex = 4
      object Label31: TLabel
        Left = 9
        Top = 10
        Width = 134
        Height = 14
        Caption = 'PrairieView log file path:'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblPath: TLabel
        Left = 9
        Top = 26
        Width = 31
        Height = 13
        Caption = 'Path...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object lblTerminator: TLabel
        Left = 9
        Top = 104
        Width = 119
        Height = 13
        Caption = 'Command terminator:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object btnBrowse: TButton
        Left = 8
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Browse...'
        TabOrder = 0
        OnClick = btnBrowseClick
      end
      object rbTerm0: TRadioButton
        Left = 16
        Top = 120
        Width = 41
        Height = 17
        Caption = '#0'
        TabOrder = 1
      end
      object rbTerm1: TRadioButton
        Left = 16
        Top = 136
        Width = 41
        Height = 17
        Caption = '#1'
        TabOrder = 2
      end
    end
  end
end
