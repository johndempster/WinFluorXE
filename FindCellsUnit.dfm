object FindCellsFrm: TFindCellsFrm
  Left = 0
  Top = 0
  Caption = 'FindCellsFrm'
  ClientHeight = 621
  ClientWidth = 913
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsMDIChild
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    913
    621)
  TextHeight = 15
  object Image1: TImage
    Left = 217
    Top = 8
    Width = 688
    Height = 605
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 201
    Height = 257
    Caption = ' Detection Thresholds '
    TabOrder = 0
    object lbBackgroundROI: TLabel
      Left = 3
      Top = 111
      Width = 67
      Height = 15
      Caption = 'Background'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbThresholdLow: TLabel
      Left = 37
      Top = 161
      Width = 55
      Height = 15
      Caption = 'Threshold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 64
      Height = 15
      Caption = 'Frame Type'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 53
      Width = 44
      Height = 15
      Caption = 'Cell ROI'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbBackgroundIntensity: TLabel
      Left = 84
      Top = 140
      Width = 119
      Height = 15
      Caption = 'lbBackgroundIntensity'
    end
    object lbCellIntensity: TLabel
      Left = 84
      Top = 82
      Width = 119
      Height = 15
      Caption = 'lbBackgroundIntensity'
    end
    object Label5: TLabel
      Left = 47
      Top = 193
      Width = 47
      Height = 15
      Alignment = taRightJustify
      Caption = 'ROI Size'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 26
      Top = 219
      Width = 68
      Height = 15
      Alignment = taRightJustify
      Caption = 'Erosion Dist.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object cbBackgroundROI: TComboBox
      Left = 84
      Top = 111
      Width = 97
      Height = 23
      Hint = 'ROI containing background levels to be subtracted'
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbBackgroundROIChange
    end
    object edThreshold: TValidatedEdit
      Left = 98
      Top = 161
      Width = 80
      Height = 23
      Hint = 'Cell vs background discrimination threshold (grey level units)'
      Text = ' 100 '
      Value = 100.000000000000000000
      Scale = 1.000000000000000000
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object cbFrameType: TComboBox
      Left = 84
      Top = 24
      Width = 101
      Height = 23
      Hint = 'Frame type within which cells are to be detected'
      Style = csDropDownList
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnChange = cbFrameTypeChange
    end
    object cbCellROI: TComboBox
      Left = 84
      Top = 53
      Width = 100
      Height = 23
      Hint = 'ROI containing a typical cell to be detected'
      Style = csDropDownList
      TabOrder = 3
      OnChange = cbCellROIChange
    end
    object edROISize: TValidatedEdit
      Left = 100
      Top = 190
      Width = 78
      Height = 23
      Hint = 'Size of n x n square ROI averaging box.'
      Text = ' 5 pixels'
      Value = 5.000000000000000000
      Scale = 1.000000000000000000
      Units = 'pixels'
      NumberFormat = '%.0f'
      LoLimit = 1.000000000000000000
      HiLimit = 21.000000000000000000
    end
    object edErosionRadius: TValidatedEdit
      Left = 100
      Top = 219
      Width = 78
      Height = 23
      Hint = 'No. of pixels to be removed from borders of detected objects'
      Text = ' 1 pixels'
      Value = 1.000000000000000000
      Scale = 1.000000000000000000
      Units = 'pixels'
      NumberFormat = '%.0f'
      LoLimit = -1.000000015047466E30
      HiLimit = 21.000000000000000000
    end
  end
  object bFIndCells: TButton
    Left = 8
    Top = 391
    Width = 97
    Height = 22
    Hint = 'Find cells in image'
    Caption = 'Find Cells'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = bFIndCellsClick
  end
  object bCancel: TButton
    Left = 111
    Top = 391
    Width = 58
    Height = 18
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
  end
  object gpCellImits: TGroupBox
    Left = 8
    Top = 271
    Width = 201
    Height = 113
    Caption = ' Cell Area Limits '
    TabOrder = 3
    object Label3: TLabel
      Left = 45
      Top = 18
      Width = 53
      Height = 15
      Alignment = taRightJustify
      Caption = 'Minimum'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 42
      Top = 47
      Width = 56
      Height = 15
      Alignment = taRightJustify
      Caption = 'Maximum'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edMinCellArea: TValidatedEdit
      Left = 101
      Top = 18
      Width = 97
      Height = 23
      Hint = 
        'Minimum valid cell area: Ignore cells with areas below this limi' +
        't'
      ShowHint = True
      Text = ' 10 '
      Value = 10.000000000000000000
      Scale = 1.000000000000000000
      NumberFormat = '%.4g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object edMaxCellArea: TValidatedEdit
      Left = 101
      Top = 47
      Width = 97
      Height = 23
      Hint = 
        'Maximum valid cell area: Ignore cells with areas above this limi' +
        't'
      ShowHint = True
      Text = ' 2000 '
      Value = 2000.000000000000000000
      Scale = 1.000000000000000000
      NumberFormat = '%.4g'
      LoLimit = -1.000000015047466E30
      HiLimit = 1.000000015047466E30
    end
    object rbMicrons: TRadioButton
      Left = 72
      Top = 76
      Width = 73
      Height = 25
      Caption = 'Microns'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = rbMicronsClick
    end
    object rbPixels: TRadioButton
      Left = 140
      Top = 76
      Width = 58
      Height = 25
      Caption = 'Pixels'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      TabStop = True
      OnClick = rbPixelsClick
    end
  end
  object edStatus: TEdit
    Left = 8
    Top = 446
    Width = 203
    Height = 23
    TabOrder = 4
  end
  object bSaveAsROIs: TButton
    Left = 8
    Top = 418
    Width = 97
    Height = 22
    Hint = 'Find cells in image'
    Caption = 'Save As ROIs'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = bSaveAsROIsClick
  end
end
