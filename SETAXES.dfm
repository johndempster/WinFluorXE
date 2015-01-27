object SetAxesFrm: TSetAxesFrm
  Left = 475
  Top = 174
  BorderStyle = bsDialog
  Caption = 'Customise Graph'
  ClientHeight = 340
  ClientWidth = 259
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
  object XAxisGrp: TGroupBox
    Left = 8
    Top = 42
    Width = 121
    Height = 153
    Caption = ' X Axis '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lbXMin: TLabel
      Left = 8
      Top = 52
      Width = 19
      Height = 14
      Caption = 'Min.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object lbxMax: TLabel
      Left = 8
      Top = 76
      Width = 23
      Height = 14
      Caption = 'Max.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 8
      Top = 100
      Width = 22
      Height = 14
      Caption = 'Tick '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object TLabel
      Left = 8
      Top = 124
      Width = 27
      Height = 14
      Caption = 'Scale'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object cbXAxisType: TComboBox
      Left = 40
      Top = 124
      Width = 73
      Height = 23
      Hint = 'Linear or logarithmic axis scaling '
      Style = csDropDownList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Items.Strings = (
        'Linear'
        'Log.'
        'Square Root')
    end
    object rbXAutoRange: TRadioButton
      Left = 8
      Top = 16
      Width = 105
      Height = 17
      Caption = 'Automatic'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      TabStop = True
      OnClick = rbXAutoRangeClick
    end
    object rbXManual: TRadioButton
      Left = 8
      Top = 32
      Width = 65
      Height = 17
      Caption = 'Manual'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = rbXManualClick
    end
    object edXMin: TValidatedEdit
      Left = 40
      Top = 52
      Width = 73
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.4g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edXMax: TValidatedEdit
      Left = 40
      Top = 76
      Width = 73
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.4g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edXTick: TValidatedEdit
      Left = 40
      Top = 100
      Width = 73
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.4g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
  end
  object YAxisGrp: TGroupBox
    Left = 136
    Top = 42
    Width = 121
    Height = 153
    Caption = ' Y Axis '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lbYMin: TLabel
      Left = 8
      Top = 52
      Width = 19
      Height = 14
      Caption = 'Min.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object lbYMax: TLabel
      Left = 8
      Top = 76
      Width = 23
      Height = 14
      Caption = 'Max.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 8
      Top = 100
      Width = 19
      Height = 14
      Caption = 'Tick'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object TLabel
      Left = 8
      Top = 124
      Width = 27
      Height = 14
      Caption = 'Scale'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object cbYAxisType: TComboBox
      Left = 40
      Top = 124
      Width = 73
      Height = 23
      Hint = 'Linear or logarithmic axis scaling '
      Style = csDropDownList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Items.Strings = (
        'Linear'
        'Log.'
        'Square Root')
    end
    object rbYAutoRange: TRadioButton
      Left = 8
      Top = 16
      Width = 105
      Height = 17
      Caption = 'Automatic'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      TabStop = True
      OnClick = rbYAutoRangeClick
    end
    object rbYManual: TRadioButton
      Left = 8
      Top = 32
      Width = 65
      Height = 17
      Caption = 'Manual'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = rbYManualClick
    end
    object edYMin: TValidatedEdit
      Left = 40
      Top = 52
      Width = 73
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.4g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edYMax: TValidatedEdit
      Left = 40
      Top = 76
      Width = 73
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.4g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edYTick: TValidatedEdit
      Left = 40
      Top = 100
      Width = 73
      Height = 20
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.4g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
  end
  object bUpdate: TButton
    Left = 8
    Top = 272
    Width = 65
    Height = 17
    Caption = 'Update'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 2
    OnClick = bUpdateClick
  end
  object LabelsGrp: TGroupBox
    Left = 8
    Top = 194
    Width = 249
    Height = 73
    Caption = ' Labels '
    TabOrder = 3
    object Label5: TLabel
      Left = 8
      Top = 16
      Width = 31
      Height = 14
      Caption = 'X Axis'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 8
      Top = 40
      Width = 32
      Height = 14
      Caption = 'Y Axis'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object EdXLabel: TEdit
      Left = 48
      Top = 16
      Width = 193
      Height = 22
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      Text = 'EdXLabel'
    end
    object edYLabel: TEdit
      Left = 48
      Top = 40
      Width = 193
      Height = 21
      AutoSize = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      Text = 'edYLabel'
    end
  end
  object nbPlotType: TNotebook
    Left = 80
    Top = 268
    Width = 177
    Height = 69
    TabOrder = 4
    object TPage
      Left = 0
      Top = 0
      Caption = 'XYGraph'
      object TraceGrp: TGroupBox
        Left = 8
        Top = 0
        Width = 169
        Height = 57
        Caption = ' Line Style '
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object ckMarkers: TCheckBox
          Left = 88
          Top = 14
          Width = 73
          Height = 17
          Caption = ' Markers'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object ckLines: TCheckBox
          Left = 16
          Top = 14
          Width = 57
          Height = 17
          Caption = ' Lines'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object ckShowLineLabels: TCheckBox
          Left = 16
          Top = 32
          Width = 145
          Height = 17
          Caption = 'Show Line Labels'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Histogram'
      object GroupBox1: TGroupBox
        Left = 8
        Top = 0
        Width = 169
        Height = 65
        Caption = ' Bin Style '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object ColorBox: TShape
          Left = 112
          Top = 36
          Width = 25
          Height = 17
          OnMouseDown = ColorBoxMouseDown
        end
        object Label8: TLabel
          Left = 104
          Top = 16
          Width = 37
          Height = 14
          Caption = 'Colour'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object ckBorders: TCheckBox
          Left = 8
          Top = 40
          Width = 97
          Height = 17
          Caption = ' Full borders'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          State = cbChecked
          TabOrder = 0
        end
        object cbFillStyle: TComboBox
          Left = 8
          Top = 16
          Width = 89
          Height = 23
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          Items.Strings = (
            'No Fill'
            'Solid Fill'
            'Hatched Fill')
        end
      end
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 0
    Width = 249
    Height = 43
    Caption = ' Plot '
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    object cbPlot: TComboBox
      Left = 8
      Top = 14
      Width = 233
      Height = 22
      Style = csDropDownList
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnChange = cbPlotChange
    end
  end
  object bUpdateAll: TButton
    Left = 8
    Top = 294
    Width = 65
    Height = 17
    Caption = 'Update All'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = bUpdateAllClick
  end
  object ColorDialog: TColorDialog
    Left = 72
    Top = 280
  end
end
