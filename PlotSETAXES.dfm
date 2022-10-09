object PlotSetAxesFrm: TPlotSetAxesFrm
  Tag = 27
  Left = 376
  Top = 123
  BorderStyle = bsDialog
  Caption = 'Set Axes Range / Labels'
  ClientHeight = 301
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object XAxisGrp: TGroupBox
    Left = 8
    Top = 0
    Width = 121
    Height = 153
    Caption = ' X Axis '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
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
      Height = 22
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
      Height = 17
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
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
      Height = 17
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
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
      Height = 18
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
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
    Top = 0
    Width = 121
    Height = 153
    Caption = ' Y Axis '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
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
      Height = 22
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
      Height = 17
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
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
      Height = 17
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
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
      Height = 18
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Text = ' 0 '
      Scale = 1.000000000000000000
      NumberFormat = '%.4g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
  end
  object bOK: TButton
    Left = 8
    Top = 232
    Width = 57
    Height = 25
    Caption = 'OK'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 2
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 8
    Top = 262
    Width = 49
    Height = 16
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 3
  end
  object LabelsGrp: TGroupBox
    Left = 8
    Top = 152
    Width = 249
    Height = 73
    Caption = ' Labels '
    TabOrder = 4
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
      Height = 17
      AutoSize = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      Text = 'EdXLabel'
    end
    object edYLabel: TEdit
      Left = 48
      Top = 38
      Width = 193
      Height = 18
      AutoSize = False
      TabOrder = 1
      Text = 'edYLabel'
    end
  end
  object nbPlotType: TNotebook
    Left = 72
    Top = 228
    Width = 185
    Height = 69
    PageIndex = 1
    TabOrder = 5
    object TPage
      Left = 0
      Top = 0
      Caption = 'XYGraph'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TraceGrp: TGroupBox
        Left = 32
        Top = 0
        Width = 153
        Height = 57
        Caption = ' Trace '
        TabOrder = 0
        object ckMarkers: TCheckBox
          Left = 8
          Top = 32
          Width = 73
          Height = 17
          Caption = ' Markers'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object ckLines: TCheckBox
          Left = 8
          Top = 14
          Width = 57
          Height = 17
          Caption = ' Lines'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Histogram'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox1: TGroupBox
        Left = 16
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
          Height = 22
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
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
  object ColorDialog: TColorDialog
    Left = 56
    Top = 264
  end
end
