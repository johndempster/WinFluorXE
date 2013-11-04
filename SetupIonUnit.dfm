object SetupIonFrm: TSetupIonFrm
  Left = 518
  Top = 317
  Width = 296
  Height = 278
  Caption = 'Ion Binding Equations'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poMainFormCenter
  Visible = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object GroupBox15: TGroupBox
    Left = 7
    Top = 0
    Width = 274
    Height = 217
    TabOrder = 0
    object GroupBox16: TGroupBox
      Left = 7
      Top = 8
      Width = 114
      Height = 201
      TabOrder = 0
      object Label12: TLabel
        Left = 9
        Top = 90
        Width = 35
        Height = 15
        Alignment = taRightJustify
        Caption = 'R.Max'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label13: TLabel
        Left = 13
        Top = 114
        Width = 31
        Height = 15
        Alignment = taRightJustify
        Caption = 'R.Min'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object Label14: TLabel
        Left = 18
        Top = 139
        Width = 26
        Height = 15
        Alignment = taRightJustify
        Caption = 'K.eff'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object Label16: TLabel
        Left = 27
        Top = 62
        Width = 17
        Height = 15
        Alignment = taRightJustify
        Caption = 'Ion'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label17: TLabel
        Left = 15
        Top = 166
        Width = 29
        Height = 15
        Alignment = taRightJustify
        Caption = 'Units'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label10: TLabel
        Left = 8
        Top = 12
        Width = 49
        Height = 15
        Alignment = taRightJustify
        Caption = 'Equation'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object edRMax: TValidatedEdit
        Left = 50
        Top = 90
        Width = 54
        Height = 20
        AutoSize = False
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.3g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edRMin: TValidatedEdit
        Left = 50
        Top = 115
        Width = 54
        Height = 20
        AutoSize = False
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.3g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object edKEff: TValidatedEdit
        Left = 50
        Top = 138
        Width = 54
        Height = 20
        AutoSize = False
        Text = ' 1 '
        Value = 1.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.3g'
        LoLimit = -1.000000015047466E29
        HiLimit = 1.000000015047466E30
      end
      object edIon: TEdit
        Left = 50
        Top = 62
        Width = 53
        Height = 20
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        Text = 'edIon'
      end
      object edUnits: TEdit
        Left = 50
        Top = 166
        Width = 53
        Height = 20
        AutoSize = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        Text = 'edIon'
      end
      object cbBindingEquation: TComboBox
        Left = 8
        Top = 29
        Width = 94
        Height = 19
        Style = csOwnerDrawFixed
        ItemHeight = 13
        ParentShowHint = False
        ShowHint = False
        TabOrder = 5
        OnChange = cbBindingEquationChange
        OnEnter = cbBindingEquationEnter
      end
    end
    object GroupBox17: TGroupBox
      Left = 127
      Top = 10
      Width = 135
      Height = 56
      TabOrder = 1
      object Label8: TLabel
        Left = 14
        Top = 30
        Width = 34
        Height = 15
        Caption = 'Name'
      end
      object edNewEqnName: TEdit
        Left = 56
        Top = 31
        Width = 72
        Height = 19
        AutoSize = False
        TabOrder = 0
      end
      object bNewTable: TButton
        Left = 8
        Top = 11
        Width = 120
        Height = 16
        Caption = 'Add New Equation'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bNewTableClick
      end
    end
    object bDeleteTable: TButton
      Left = 128
      Top = 71
      Width = 134
      Height = 17
      Caption = 'Delete Equation'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = bDeleteTableClick
    end
  end
  object bOK: TButton
    Left = 7
    Top = 221
    Width = 46
    Height = 20
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
    Left = 59
    Top = 221
    Width = 46
    Height = 16
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 2
    OnClick = bCancelClick
  end
end
