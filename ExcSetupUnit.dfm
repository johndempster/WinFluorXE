object ExcSetupFrm: TExcSetupFrm
  Tag = 10
  Left = 556
  Top = 115
  Caption = 'Excitation Light Setup'
  ClientHeight = 478
  ClientWidth = 603
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object TableGrp: TGroupBox
    Left = 192
    Top = 0
    Width = 393
    Height = 446
    Caption = ' Excitation / emission wavelength table '
    TabOrder = 0
    object WaveTable: TStringGrid
      Left = 8
      Top = 16
      Width = 377
      Height = 409
      Hint = 'Input channel scaling factors and calibration units'
      ColCount = 6
      DefaultColWidth = 50
      DefaultRowHeight = 18
      RowCount = 18
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
      OnKeyPress = WaveTableKeyPress
      ColWidths = (
        50
        50
        50
        50
        50
        50)
      RowHeights = (
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18
        18)
    end
  end
  object bOK: TButton
    Left = 8
    Top = 452
    Width = 57
    Height = 20
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
    Left = 72
    Top = 452
    Width = 49
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
  object GroupBox1: TGroupBox
    Left = 4
    Top = 0
    Width = 183
    Height = 313
    Caption = ' Multi-wavelength sequences '
    TabOrder = 3
    object meSequence: TMemo
      Left = 8
      Top = 72
      Width = 169
      Height = 145
      Lines.Strings = (
        'meCycle')
      ReadOnly = True
      TabOrder = 0
    end
    object SequenceGrp: TGroupBox
      Left = 8
      Top = 216
      Width = 169
      Height = 65
      TabOrder = 1
      object Label4: TLabel
        Left = 64
        Top = 40
        Width = 61
        Height = 14
        Alignment = taRightJustify
        Caption = 'Divide factor'
      end
      object bAddWavelength: TButton
        Left = 8
        Top = 14
        Width = 105
        Height = 18
        Hint = 'Add selected wavelength number to sequence'
        Caption = 'Add Wavelength'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = bAddWavelengthClick
      end
      object cbAddWavelength: TComboBox
        Left = 118
        Top = 14
        Width = 45
        Height = 22
        Hint = 'Wavelength number to be added to sequence'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = 'cbAddWavelength'
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8')
      end
      object edDivideFactor: TValidatedEdit
        Left = 128
        Top = 40
        Width = 33
        Height = 20
        AutoSize = False
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.4g'
        LoLimit = 1.000000000000000000
        HiLimit = 100.000000000000000000
      end
    end
    object ClearCycle: TButton
      Left = 8
      Top = 286
      Width = 169
      Height = 18
      Hint = 'Erase wavelength sequence list'
      Caption = 'Clear Sequence'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = ClearCycleClick
    end
    object edSequenceName: TEdit
      Left = 8
      Top = 42
      Width = 167
      Height = 22
      TabOrder = 3
      Text = 'edSequenceName'
      OnChange = edSequenceNameChange
    end
    object cbSequence: TComboBox
      Left = 8
      Top = 16
      Width = 169
      Height = 22
      Style = csDropDownList
      TabOrder = 4
      OnChange = cbSequenceChange
    end
  end
  object SpectrumGrp: TGroupBox
    Left = 4
    Top = 314
    Width = 181
    Height = 132
    Caption = ' Spectrum '
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 31
      Height = 14
      Caption = 'Range'
    end
    object Label2: TLabel
      Left = 68
      Top = 44
      Width = 46
      Height = 14
      Alignment = taRightJustify
      Caption = 'Step Size'
    end
    object Label3: TLabel
      Left = 62
      Top = 70
      Width = 52
      Height = 14
      Alignment = taRightJustify
      Caption = 'Bandwidth'
    end
    object Label5: TLabel
      Left = 71
      Top = 96
      Width = 43
      Height = 14
      Alignment = taRightJustify
      Caption = 'Em. Filter'
    end
    object edSpectrumRange: TRangeEdit
      Left = 46
      Top = 16
      Width = 129
      Height = 22
      Hint = 'Start and end wavelengths for excitation spectrum'
      ShowHint = True
      Text = ' 350.00 - 750.00 nm'
      LoValue = 350.000000000000000000
      HiValue = 750.000000000000000000
      LoLimit = 100.000000000000000000
      HiLimit = 2000.000000000000000000
      Scale = 1.000000000000000000
      Units = 'nm'
      NumberFormat = '%.f - %.f'
    end
    object edSpectrumStepSize: TValidatedEdit
      Left = 118
      Top = 44
      Width = 57
      Height = 20
      Hint = 'Excitation spectrum wavelength increment'
      AutoSize = False
      ShowHint = True
      Text = ' 10 nm'
      Value = 10.000000000000000000
      Scale = 1.000000000000000000
      Units = 'nm'
      NumberFormat = '%.4g'
      LoLimit = 1.000000000000000000
      HiLimit = 1000.000000000000000000
    end
    object edSpectrumBandwidth: TValidatedEdit
      Left = 118
      Top = 70
      Width = 57
      Height = 20
      Hint = 'Excitation spectrum bandwidth'
      AutoSize = False
      ShowHint = True
      Text = ' 10 nm'
      Value = 10.000000000000000000
      Scale = 1.000000000000000000
      Units = 'nm'
      NumberFormat = '%.4g'
      LoLimit = 1.000000000000000000
      HiLimit = 1000.000000000000000000
    end
    object edSpectrumEMFilter: TValidatedEdit
      Left = 118
      Top = 96
      Width = 57
      Height = 20
      Hint = 'Excitation spectrum bandwidth'
      AutoSize = False
      ShowHint = True
      Text = ' 1 '
      Value = 1.000000000000000000
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E30
      HiLimit = 15.000000000000000000
    end
  end
end
