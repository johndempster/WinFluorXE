object XYStageFrm: TXYStageFrm
  Left = 0
  Top = 0
  Caption = 'XY Stage Control'
  ClientHeight = 352
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListGrp: TGroupBox
    Left = 175
    Top = 0
    Width = 186
    Height = 344
    Caption = ' Stage Position List '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object sgPositions: TStringGrid
      Left = 8
      Top = 18
      Width = 169
      Height = 204
      ColCount = 3
      DefaultRowHeight = 20
      FixedCols = 0
      RowCount = 3
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
      TabOrder = 0
    end
    object bAddPosition: TButton
      Left = 8
      Top = 228
      Width = 169
      Height = 17
      Hint = 'Add current stage position to list'
      Caption = 'Add Position'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = bAddPositionClick
    end
    object bMoveTo: TButton
      Left = 8
      Top = 274
      Width = 120
      Height = 17
      Hint = 'Move to selected stage position'
      Caption = 'Move To'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = bMoveToClick
    end
    object cbPosition: TComboBox
      Left = 133
      Top = 272
      Width = 45
      Height = 22
      Style = csDropDownList
      TabOrder = 3
    end
    object DeletePosition: TButton
      Left = 8
      Top = 249
      Width = 120
      Height = 17
      Hint = 'Delete selected stage position from list'
      Caption = 'Delete Position'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = DeletePositionClick
    end
    object cbDeletePosition: TComboBox
      Left = 133
      Top = 247
      Width = 45
      Height = 22
      Style = csDropDownList
      TabOrder = 5
    end
    object CycleGrp: TGroupBox
      Left = 8
      Top = 294
      Width = 169
      Height = 41
      Caption = ' Time Lapse Action'
      TabOrder = 6
      object ckIncrementStagePosition: TCheckBox
        Left = 8
        Top = 16
        Width = 158
        Height = 17
        Hint = 
          'Tick to increment stage position after each time lapse image seq' +
          'uence'
        Caption = 'Increment Stage Position'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
  end
  object ButtonsGrp: TGroupBox
    Left = 8
    Top = 156
    Width = 161
    Height = 188
    TabOrder = 1
    object bMoveUp: TButton
      Left = 65
      Top = 16
      Width = 26
      Height = 25
      Hint = 'Move stage up'
      Caption = #61552
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Wingdings 3'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = bMoveUpClick
    end
    object bMoveLeft: TButton
      Left = 34
      Top = 46
      Width = 26
      Height = 25
      Hint = 'Move stage left'
      Caption = #61556
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Wingdings 3'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = bMoveLeftClick
    end
    object bHome: TButton
      Left = 65
      Top = 46
      Width = 26
      Height = 25
      Hint = 'Move stage to home position'
      Caption = 'H'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = bHomeClick
    end
    object bMoveRight: TButton
      Left = 96
      Top = 46
      Width = 26
      Height = 25
      Hint = 'Move stage right'
      Caption = #61557
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Wingdings 3'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = bMoveRightClick
    end
    object bMoveDown: TButton
      Left = 65
      Top = 77
      Width = 26
      Height = 25
      Hint = 'Move stage down'
      Caption = #61553
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Wingdings 3'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = bMoveDownClick
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 114
      Width = 73
      Height = 65
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      object rbCoarse: TRadioButton
        Left = 4
        Top = 10
        Width = 60
        Height = 17
        Hint = 'Coarse steps (1 mm)'
        Caption = 'Coarse'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        TabStop = True
      end
      object rbFine: TRadioButton
        Left = 4
        Top = 43
        Width = 60
        Height = 17
        Hint = 'Fine steps (0.01 mm)'
        Caption = 'Fine'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object rbMedium: TRadioButton
        Left = 4
        Top = 26
        Width = 60
        Height = 17
        Hint = 'Medium steps (0.1 mm)'
        Caption = 'Medium'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
    end
  end
  object StagePositionGrp: TGroupBox
    Left = 8
    Top = 0
    Width = 161
    Height = 153
    TabOrder = 2
    object lbXPos: TLabel
      Left = 6
      Top = 45
      Width = 13
      Height = 22
      Caption = 'X'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbYPos: TLabel
      Left = 6
      Top = 95
      Width = 13
      Height = 22
      Caption = 'Y'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edStagePosition: TEdit
      Left = 25
      Top = 14
      Width = 121
      Height = 21
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
      Text = 'edStagePosition'
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Top = 176
  end
end
