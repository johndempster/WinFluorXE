object EditROIFrm: TEditROIFrm
  Tag = 8
  Left = 428
  Top = 110
  Caption = 'Edit Regions of Interest'
  ClientHeight = 240
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object sgTable: TStringGrid
    Left = 8
    Top = 8
    Width = 425
    Height = 193
    ColCount = 6
    DefaultColWidth = 80
    DefaultRowHeight = 20
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    ColWidths = (
      80
      80
      80
      80
      80
      80)
    RowHeights = (
      20
      20
      20
      20
      20)
  end
  object bOK: TButton
    Left = 4
    Top = 208
    Width = 53
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
    Top = 208
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
    TabOrder = 2
    OnClick = bCancelClick
  end
end
