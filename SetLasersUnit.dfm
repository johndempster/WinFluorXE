object SetLasersFrm: TSetLasersFrm
  Left = 590
  Top = 268
  Width = 270
  Height = 162
  Caption = 'Set LED/Laser Intensity'
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
    Width = 249
    Height = 105
    TabOrder = 0
    object panLaser1: TPanel
      Left = 8
      Top = 16
      Width = 233
      Height = 24
      BevelOuter = bvNone
      TabOrder = 0
      object Label31: TLabel
        Left = 1
        Top = 2
        Width = 43
        Height = 15
        Caption = 'Laser 1'
      end
      object sbLaser1: TScrollBar
        Left = 56
        Top = 2
        Width = 113
        Height = 17
        PageSize = 0
        TabOrder = 0
        OnChange = sbLaser1Change
      end
      object edLaser1Intensity: TValidatedEdit
        Left = 176
        Top = 2
        Width = 49
        Height = 20
        Hint = 'Laser #1 emission wavelength (nm)'
        OnKeyPress = edLaser1IntensityKeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 0 %'
        Scale = 1.000000000000000000
        Units = '%'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 100.000000000000000000
      end
    end
    object panLaser3: TPanel
      Left = 8
      Top = 68
      Width = 233
      Height = 24
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 1
        Top = 2
        Width = 43
        Height = 15
        Caption = 'Laser 2'
      end
      object sbLaser2: TScrollBar
        Left = 56
        Top = 0
        Width = 113
        Height = 17
        PageSize = 0
        TabOrder = 0
        OnChange = sbLaser2Change
      end
      object edLaser2Intensity: TValidatedEdit
        Left = 176
        Top = -3
        Width = 49
        Height = 20
        Hint = 'Laser #1 emission wavelength (nm)'
        OnKeyPress = edLaser2IntensityKeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 0 %'
        Scale = 1.000000000000000000
        Units = '%'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 100.000000000000000000
      end
    end
    object panLaser2: TPanel
      Left = 8
      Top = 42
      Width = 233
      Height = 24
      BevelOuter = bvNone
      TabOrder = 2
      object Label2: TLabel
        Left = 1
        Top = 2
        Width = 43
        Height = 15
        Caption = 'Laser 3'
      end
      object sbLaser3: TScrollBar
        Left = 56
        Top = 0
        Width = 113
        Height = 17
        PageSize = 0
        TabOrder = 0
        OnChange = sbLaser3Change
      end
      object edLaser3Intensity: TValidatedEdit
        Left = 176
        Top = -3
        Width = 49
        Height = 20
        Hint = 'Laser #1 emission wavelength (nm)'
        OnKeyPress = edLaser3IntensityKeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 0 %'
        Scale = 1.000000000000000000
        Units = '%'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 100.000000000000000000
      end
    end
  end
  object bOK: TButton
    Left = 4
    Top = 110
    Width = 49
    Height = 20
    Caption = 'OK'
    TabOrder = 1
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 66
    Top = 110
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
