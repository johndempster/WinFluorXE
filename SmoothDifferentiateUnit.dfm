object SmoothDifferentiateFrm: TSmoothDifferentiateFrm
  Left = 308
  Top = 171
  AutoScroll = False
  Caption = 'Smooth / Differentiate'
  ClientHeight = 694
  ClientWidth = 859
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 640
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
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object sdDisplay: TScopeDisplay
    Left = 152
    Top = 18
    Width = 689
    Height = 599
    CursorChangeInProgress = False
    NumChannels = 1
    NumPoints = 1
    MaxPoints = 1024
    XMin = 0
    XMax = 1023
    XOffset = 0
    CursorsEnabled = True
    TScale = 1.000000000000000000
    TUnits = 's'
    TCalBar = -1.000000000000000000
    ZoomDisableHorizontal = True
    ZoomDisableVertical = False
    DisableChannelVisibilityButton = False
    PrinterFontSize = 0
    PrinterPenWidth = 0
    PrinterLeftMargin = 0
    PrinterRightMargin = 0
    PrinterTopMargin = 0
    PrinterBottomMargin = 0
    PrinterDisableColor = False
    PrinterShowLabels = True
    PrinterShowZeroLevels = True
    MetafileWidth = 0
    MetafileHeight = 0
    StorageMode = False
    RecordNumber = -1
    DisplayGrid = True
    MaxADCValue = 2047
    MinADCValue = -2048
    NumBytesPerSample = 2
    FixZeroLevels = False
    DisplaySelected = True
  end
  object LabelChannel: TLabel
    Left = 16
    Top = 16
    Width = 42
    Height = 13
    Caption = 'Channel:'
  end
  object LabelDataMAWindow: TLabel
    Left = 16
    Top = 64
    Width = 90
    Height = 13
    Caption = 'MA(Data) Window:'
  end
  object LabelDXDTMAWindow: TLabel
    Left = 16
    Top = 112
    Width = 92
    Height = 13
    Caption = 'MA(dx/dt) Window:'
  end
  object cbChannel: TComboBox
    Left = 16
    Top = 32
    Width = 121
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object btnUpdate: TButton
    Left = 32
    Top = 168
    Width = 89
    Height = 25
    Caption = 'Update'
    TabOrder = 1
    OnClick = btnUpdateClick
  end
  object edDataMAWindow: TValidatedEdit
    Left = 16
    Top = 80
    Width = 121
    Height = 20
    AutoSize = False
    Text = ' 1 '
    Value = 1.000000000000000000
    Scale = 1.000000000000000000
    NumberFormat = '%.0f'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object edDXDTMAWindow: TValidatedEdit
    Left = 16
    Top = 128
    Width = 121
    Height = 20
    AutoSize = False
    Text = ' 1 '
    Value = 1.000000000000000000
    Scale = 1.000000000000000000
    NumberFormat = '%.0f'
    LoLimit = -1.000000015047466E30
    HiLimit = 1.000000015047466E30
  end
  object sbDisplay: TScrollBar
    Left = 152
    Top = 624
    Width = 689
    Height = 17
    PageSize = 0
    TabOrder = 4
    OnChange = sbDisplayChange
  end
  object pnlDisplay: TPanel
    Left = 634
    Top = 650
    Width = 209
    Height = 23
    BevelOuter = bvNone
    TabOrder = 5
    object edDisplay: TValidatedEdit
      Left = 124
      Top = 1
      Width = 65
      Height = 20
      OnKeyPress = edDisplayKeyPress
      AutoSize = False
      Text = ' 0.01667 min'
      Value = 1.000198006629944000
      Scale = 0.016666699200868610
      Units = 'min'
      NumberFormat = '%.4g'
      LoLimit = 1.000000000000000000
      HiLimit = 2000.000000000000000000
    end
    object rbDisplayUnitMins: TRadioButton
      Left = 50
      Top = 1
      Width = 49
      Height = 17
      Caption = 'Mins'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = rbDisplayUnitMinsClick
    end
    object rbDisplayUnitsSecs: TRadioButton
      Left = 1
      Top = 1
      Width = 49
      Height = 17
      Caption = 'Secs'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      TabStop = True
      OnClick = rbDisplayUnitsSecsClick
    end
    object btnDisplayDouble: TButton
      Left = 190
      Top = 2
      Width = 16
      Height = 18
      Caption = '4'
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Webdings'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = btnDisplayDoubleClick
    end
    object btnDisplayHalf: TButton
      Left = 107
      Top = 2
      Width = 16
      Height = 18
      Caption = '3'
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Webdings'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      OnClick = btnDisplayHalfClick
    end
  end
end
