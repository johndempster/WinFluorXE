object PrintGraphFrm: TPrintGraphFrm
  Left = 365
  Top = 277
  BorderStyle = bsDialog
  Caption = 'Print Graph'
  ClientHeight = 250
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object bOK: TButton
    Left = 4
    Top = 224
    Width = 53
    Height = 20
    Caption = 'OK'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 0
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 64
    Top = 224
    Width = 57
    Height = 17
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 1
  end
  object FontGrp: TGroupBox
    Left = 4
    Top = 66
    Width = 153
    Height = 69
    Caption = ' Typeface '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    object Label7: TLabel
      Left = 54
      Top = 40
      Width = 23
      Height = 14
      Alignment = taRightJustify
      Caption = 'Size'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cbFontName: TComboBox
      Left = 8
      Top = 16
      Width = 137
      Height = 23
      TabOrder = 0
      Text = 'cbFontName'
    end
    object edFontSize: TValidatedEdit
      Left = 80
      Top = 40
      Width = 65
      Height = 17
      AutoSize = False
      Text = ' 1.00  pts'
      Value = 1.000000000000000000
      Scale = 1.000000000000000000
      Units = 'pts'
      NumberFormat = '%.f '
      LoLimit = 1.000000000000000000
      HiLimit = 100.000000000000000000
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 136
    Width = 153
    Height = 81
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object Label5: TLabel
      Left = 17
      Top = 13
      Width = 58
      Height = 14
      Alignment = taRightJustify
      Caption = 'Line width'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label8: TLabel
      Left = 9
      Top = 37
      Width = 66
      Height = 14
      Alignment = taRightJustify
      Caption = 'Marker size'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edLineThickness: TValidatedEdit
      Left = 80
      Top = 13
      Width = 65
      Height = 17
      AutoSize = False
      Text = ' 1.00  pts'
      Value = 1.000000000000000000
      Scale = 1.000000000000000000
      Units = 'pts'
      NumberFormat = '%.f '
      LoLimit = 1.000000000000000000
      HiLimit = 100.000000000000000000
    end
    object edMarkerSize: TValidatedEdit
      Left = 80
      Top = 37
      Width = 65
      Height = 17
      AutoSize = False
      Text = ' 1.00  pts'
      Value = 1.000000000000000000
      Scale = 1.000000000000000000
      Units = 'pts'
      NumberFormat = '%.f '
      LoLimit = 1.000000000000000000
      HiLimit = 100.000000000000000000
    end
    object ckUseColor: TCheckBox
      Left = 64
      Top = 60
      Width = 81
      Height = 17
      Hint = 'Plot signal waveforms in colour'
      Alignment = taLeftJustify
      Caption = 'Use colour'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object Page: TNotebook
    Left = 160
    Top = 64
    Width = 129
    Height = 129
    TabOrder = 4
    object TPage
      Left = 0
      Top = 0
      Caption = 'Printer'
      object PrinterGrp: TGroupBox
        Left = 4
        Top = 0
        Width = 117
        Height = 121
        Caption = ' Page Margins '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label1: TLabel
          Left = 19
          Top = 16
          Width = 22
          Height = 14
          Alignment = taRightJustify
          Caption = 'Left'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label2: TLabel
          Left = 18
          Top = 64
          Width = 23
          Height = 14
          Alignment = taRightJustify
          Caption = 'Top '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label3: TLabel
          Left = 10
          Top = 40
          Width = 31
          Height = 14
          Alignment = taRightJustify
          Caption = 'Right '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label4: TLabel
          Left = 1
          Top = 88
          Width = 40
          Height = 14
          Alignment = taRightJustify
          Caption = 'Bottom'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edLeftMargin: TValidatedEdit
          Left = 44
          Top = 16
          Width = 65
          Height = 17
          AutoSize = False
          Text = ' 0.0 cm'
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object edRightMargin: TValidatedEdit
          Left = 44
          Top = 40
          Width = 65
          Height = 17
          AutoSize = False
          Text = ' 0.0 cm'
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object edTopMargin: TValidatedEdit
          Left = 44
          Top = 64
          Width = 65
          Height = 17
          AutoSize = False
          Text = ' 0.0 cm'
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object edBottomMargin: TValidatedEdit
          Left = 44
          Top = 88
          Width = 65
          Height = 17
          AutoSize = False
          Text = ' 0.0 cm'
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Metafile'
      object MetafileGrp: TGroupBox
        Left = 4
        Top = 0
        Width = 121
        Height = 121
        Caption = ' Image Size '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label6: TLabel
          Left = 8
          Top = 16
          Width = 34
          Height = 14
          Caption = ' Width'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label9: TLabel
          Left = 8
          Top = 56
          Width = 35
          Height = 14
          Caption = 'Height'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edBitmapWidth: TValidatedEdit
          Left = 8
          Top = 32
          Width = 81
          Height = 17
          AutoSize = False
          Text = ' 0 pixels'
          Scale = 1.000000000000000000
          Units = 'pixels'
          NumberFormat = '%.0f'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object edBitmapHeight: TValidatedEdit
          Left = 8
          Top = 72
          Width = 81
          Height = 21
          Text = ' 0 pixels'
          Scale = 1.000000000000000000
          Units = 'pixels'
          NumberFormat = '%.0f'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
      end
    end
  end
  object GroupBox6: TGroupBox
    Left = 4
    Top = 0
    Width = 280
    Height = 65
    Caption = ' Output to '
    TabOrder = 5
    object bPrinterSetup: TButton
      Left = 8
      Top = 40
      Width = 89
      Height = 17
      Caption = 'Printer Setup'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = bPrinterSetupClick
    end
    object edPrinterName: TEdit
      Left = 8
      Top = 16
      Width = 265
      Height = 20
      AutoSize = False
      Color = clGradientInactiveCaption
      ReadOnly = True
      TabOrder = 1
      Text = 'edPrinterName'
    end
  end
end
