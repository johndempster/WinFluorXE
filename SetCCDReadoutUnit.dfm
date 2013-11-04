object SetCCDReadoutFrm: TSetCCDReadoutFrm
  Left = 777
  Top = 526
  Width = 219
  Height = 152
  Caption = 'Set CCD Area'
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
  object AreaGrp: TGroupBox
    Left = 2
    Top = 0
    Width = 191
    Height = 73
    Caption = ' CCD readout area '
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 16
      Width = 61
      Height = 13
      Caption = 'X pixel range'
    end
    object Label2: TLabel
      Left = 16
      Top = 40
      Width = 61
      Height = 13
      Caption = 'Y pixel range'
    end
    object edXRange: TRangeEdit
      Left = 88
      Top = 16
      Width = 97
      Height = 21
      Text = ' 4096 - 4096 '
      LoValue = 4096.000000000000000000
      HiValue = 4096.000000000000000000
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      NumberFormat = '%.0f - %.0f'
    end
    object edYRange: TRangeEdit
      Left = 88
      Top = 40
      Width = 97
      Height = 21
      Text = ' 4096 - 4096 '
      LoValue = 4096.000000000000000000
      HiValue = 4096.000000000000000000
      HiLimit = 1.000000015047466E30
      Scale = 1.000000000000000000
      NumberFormat = '%.0f - %.0f'
    end
  end
  object bOK: TButton
    Left = 2
    Top = 80
    Width = 40
    Height = 25
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
    Left = 50
    Top = 80
    Width = 48
    Height = 17
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = bCancelClick
  end
end
