object PrintRecFrm: TPrintRecFrm
  Left = 470
  Top = 256
  BorderStyle = bsDialog
  Caption = 'Print Record'
  ClientHeight = 353
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 12
    Top = 56
    Width = 165
    Height = 241
    Caption = ' Calibration Bars '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object CalibrationBarTable: TStringGrid
      Left = 8
      Top = 16
      Width = 145
      Height = 185
      Hint = 'Size of vertical and horizontal calibration bars'
      ColCount = 2
      DefaultColWidth = 45
      DefaultRowHeight = 18
      RowCount = 1
      FixedRows = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnKeyPress = CalibrationBarTableKeyPress
    end
    object bDefaultSettings: TButton
      Left = 8
      Top = 208
      Width = 105
      Height = 17
      Caption = 'Set to 10% range '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = bDefaultSettingsClick
    end
  end
  object GroupBox5: TGroupBox
    Left = 184
    Top = 136
    Width = 177
    Height = 97
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Label5: TLabel
      Left = 8
      Top = 13
      Width = 58
      Height = 15
      Alignment = taRightJustify
      Caption = 'Line Width'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object ckShowZeroLevels: TCheckBox
      Left = 56
      Top = 40
      Width = 113
      Height = 17
      Hint = 'Show signal zero levels as dotted line(s) on plots'
      Alignment = taLeftJustify
      Caption = 'Show zero levels'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 0
    end
    object ckShowLabels: TCheckBox
      Left = 56
      Top = 72
      Width = 113
      Height = 17
      Hint = 'Show channel names and calibration bar values on plot'
      Alignment = taLeftJustify
      Caption = 'Show labels'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 1
    end
    object ckUseColor: TCheckBox
      Left = 56
      Top = 56
      Width = 113
      Height = 17
      Hint = 'Plot signal waveforms in colour'
      Alignment = taLeftJustify
      Caption = 'Use colour'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 2
    end
    object edLineThickness: TValidatedEdit
      Left = 72
      Top = 13
      Width = 97
      Height = 20
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
  object bPrint: TButton
    Left = 12
    Top = 307
    Width = 57
    Height = 20
    Caption = 'OK'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 2
    OnClick = bPrintClick
  end
  object bCancel: TButton
    Left = 72
    Top = 307
    Width = 52
    Height = 17
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 3
  end
  object FontGrp: TGroupBox
    Left = 183
    Top = 56
    Width = 178
    Height = 73
    Caption = ' Typeface '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    object Label7: TLabel
      Left = 42
      Top = 45
      Width = 23
      Height = 15
      Alignment = taRightJustify
      Caption = 'Size'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object cbFontName: TComboBox
      Left = 8
      Top = 16
      Width = 161
      Height = 23
      TabOrder = 0
      Text = 'cbFontName'
    end
    object edFontSize: TValidatedEdit
      Left = 72
      Top = 45
      Width = 97
      Height = 20
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
  object Page: TNotebook
    Left = 181
    Top = 240
    Width = 180
    Height = 105
    TabOrder = 5
    object TPage
      Left = 0
      Top = 0
      Caption = 'Print'
      object GroupBox1: TGroupBox
        Left = 4
        Top = 2
        Width = 173
        Height = 95
        Caption = ' Page Margins '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 16
          Width = 20
          Height = 15
          Caption = 'Left'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label2: TLabel
          Left = 8
          Top = 52
          Width = 23
          Height = 15
          Caption = 'Top '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 90
          Top = 16
          Width = 32
          Height = 15
          Caption = 'Right '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 90
          Top = 52
          Width = 39
          Height = 15
          Caption = 'Bottom'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object edLeftMargin: TValidatedEdit
          Left = 8
          Top = 30
          Width = 65
          Height = 20
          AutoSize = False
          Text = ' 0.1 cm'
          Value = 1.000000000000000000
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
          LoLimit = 1.000000000000000000
          HiLimit = 100.000000000000000000
        end
        object edTopMargin: TValidatedEdit
          Left = 8
          Top = 66
          Width = 65
          Height = 20
          AutoSize = False
          Text = ' 0.1 cm'
          Value = 1.000000000000000000
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
          LoLimit = 1.000000000000000000
          HiLimit = 100.000000000000000000
        end
        object edRightMargin: TValidatedEdit
          Left = 90
          Top = 30
          Width = 63
          Height = 20
          AutoSize = False
          Text = ' 0.1 cm'
          Value = 1.000000000000000000
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
          LoLimit = 1.000000000000000000
          HiLimit = 200.000000000000000000
        end
        object edBottomMargin: TValidatedEdit
          Left = 90
          Top = 66
          Width = 63
          Height = 20
          AutoSize = False
          Text = ' 0.1 cm'
          Value = 1.000000000000000000
          Scale = 0.100000001490116100
          Units = 'cm'
          NumberFormat = '%.1f'
          LoLimit = 1.000000000000000000
          HiLimit = 200.000000000000000000
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Clipboard'
      object GroupBox4: TGroupBox
        Left = 4
        Top = 2
        Width = 149
        Height = 95
        Caption = ' Image size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label6: TLabel
          Left = 10
          Top = 16
          Width = 31
          Height = 15
          Caption = 'Width'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label8: TLabel
          Left = 10
          Top = 56
          Width = 36
          Height = 15
          Caption = 'Height'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object edWidth: TValidatedEdit
          Left = 10
          Top = 32
          Width = 90
          Height = 20
          AutoSize = False
          Text = ' 100.00  pixels'
          Value = 100.000000000000000000
          Scale = 1.000000000000000000
          Units = 'pixels'
          NumberFormat = '%.f '
          LoLimit = 100.000000000000000000
          HiLimit = 1000000.000000000000000000
        end
        object edHeight: TValidatedEdit
          Left = 10
          Top = 72
          Width = 90
          Height = 20
          AutoSize = False
          Text = ' 100.00 pixels'
          Value = 100.000000000000000000
          Scale = 1.000000000000000000
          Units = 'pixels'
          NumberFormat = '%.f'
          LoLimit = 100.000000000000000000
          HiLimit = 1000000.000000000000000000
        end
      end
    end
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 0
    Width = 353
    Height = 49
    Caption = ' Output to '
    TabOrder = 6
    object bPrinterSetup: TButton
      Left = 256
      Top = 16
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
      Width = 241
      Height = 20
      AutoSize = False
      Color = clGradientInactiveCaption
      ReadOnly = True
      TabOrder = 1
      Text = 'edPrinterName'
    end
  end
end
