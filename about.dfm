object AboutBox: TAboutBox
  Left = 689
  Top = 186
  ActiveControl = OKButton
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 342
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 4
    Top = 4
    Width = 525
    Height = 301
    BevelOuter = bvLowered
    TabOrder = 0
    object ProductName: TLabel
      Left = 8
      Top = 8
      Width = 354
      Height = 16
      Caption = 'WinFluor - Fluorescence Image Capture Program V3.5.5'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      IsControl = True
    end
    object meCameraInfo: TMemo
      Left = 1
      Top = 40
      Width = 523
      Height = 260
      Align = alBottom
      Lines.Strings = (
        'meCameraInfo')
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object OKButton: TButton
    Left = 8
    Top = 314
    Width = 65
    Height = 23
    Caption = 'OK'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
    IsControl = True
  end
end
