object PhotoStimFrm: TPhotoStimFrm
  Tag = 24
  Left = 513
  Top = 356
  Caption = 'Edit Photo-Stimulus'
  ClientHeight = 566
  ClientWidth = 918
  Color = clBtnFace
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
  object ControlGrp: TGroupBox
    Left = 2
    Top = 0
    Width = 357
    Height = 529
    TabOrder = 0
    object DisplayGrp: TGroupBox
      Left = 6
      Top = 8
      Width = 343
      Height = 97
      Caption = ' Display '
      TabOrder = 0
      object lblZoom: TLabel
        Left = 52
        Top = 46
        Width = 32
        Height = 14
        Alignment = taRightJustify
        Caption = 'Zoom'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblChannel: TLabel
        Left = 40
        Top = 20
        Width = 45
        Height = 14
        Alignment = taRightJustify
        Caption = 'Channel'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblRange: TLabel
        Left = 50
        Top = 72
        Width = 34
        Height = 14
        Alignment = taRightJustify
        Caption = 'Range'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object cbDisplayZoom: TComboBox
        Left = 90
        Top = 43
        Width = 75
        Height = 21
        Hint = 'Display magnification'
        Style = csDropDownList
        TabOrder = 0
        OnChange = cbDisplayZoomChange
      end
      object cbChannel: TComboBox
        Left = 90
        Top = 17
        Width = 75
        Height = 21
        Hint = 'Display magnification'
        Style = csDropDownList
        TabOrder = 1
        OnChange = cbChannelChange
      end
      object bGetImage: TButton
        Left = 184
        Top = 17
        Width = 150
        Height = 21
        Caption = 'SNAP'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = bGetImageClick
      end
      object bSaveImage: TButton
        Left = 184
        Top = 43
        Width = 150
        Height = 21
        Caption = 'Save Image As...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnClick = bSaveImageClick
      end
      object edDisplayIntensityRange: TRangeEdit
        Left = 90
        Top = 69
        Width = 75
        Height = 20
        Hint = 'Set working range display look-up table range manually'
        OnKeyPress = edDisplayIntensityRangeKeyPress
        AutoSize = False
        ShowHint = True
        Text = ' 4095.00 - 4095.00 '
        LoValue = 4095.000000000000000000
        HiValue = 4095.000000000000000000
        HiLimit = 4095.000000000000000000
        Scale = 1.000000000000000000
        NumberFormat = '%.f - %.f'
      end
    end
    object CalGrp: TGroupBox
      Left = 6
      Top = 481
      Width = 343
      Height = 42
      TabOrder = 1
      object bPowerCalibrate: TButton
        Left = 8
        Top = 13
        Width = 327
        Height = 21
        Caption = 'Calibrate...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = bPowerCalibrateClick
      end
    end
    object LocationGrp: TGroupBox
      Left = 6
      Top = 106
      Width = 343
      Height = 375
      Caption = 'Stimulus Protocol'
      TabOrder = 2
      object lblAttenuator: TLabel
        Left = 31
        Top = 85
        Width = 59
        Height = 14
        Caption = 'Attenuator'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblRepeatPeriod: TLabel
        Left = 24
        Top = 350
        Width = 77
        Height = 14
        Caption = 'Repeat Period'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Shape1: TShape
        Left = 8
        Top = 75
        Width = 325
        Height = 1
      end
      object lblTotalDuration: TLabel
        Left = 10
        Top = 301
        Width = 134
        Height = 13
        Caption = 'Total Protocol Duration (ms):'
      end
      object lblDuration: TLabel
        Left = 148
        Top = 301
        Width = 6
        Height = 13
        Caption = '0'
      end
      object Shape2: TShape
        Left = 8
        Top = 320
        Width = 325
        Height = 1
      end
      object bDeleteTargets: TButton
        Left = 184
        Top = 85
        Width = 150
        Height = 21
        Caption = 'Delete All'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = bDeleteTargetsClick
      end
      object sgTargets: TStringGrid
        Left = 8
        Top = 144
        Width = 328
        Height = 155
        ColCount = 100
        DefaultColWidth = 48
        DefaultRowHeight = 18
        RowCount = 7
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor, goThumbTracking]
        ParentFont = False
        ScrollBars = ssHorizontal
        TabOrder = 1
        OnKeyPress = sgTargetsKeyPress
        OnMouseDown = sgTargetsMouseDown
        OnSelectCell = sgTargetsSelectCell
        ColWidths = (
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48
          48)
        RowHeights = (
          18
          18
          18
          18
          18
          18
          18)
      end
      object cbAttenuator: TComboBox
        Left = 94
        Top = 85
        Width = 65
        Height = 21
        TabOrder = 2
        Text = 'cbAttenuator'
        OnChange = cbAttenuatorChange
      end
      object ckPhotoStimRepeat: TCheckBox
        Left = 8
        Top = 327
        Width = 169
        Height = 17
        Caption = 'Repeat Stimulus'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        OnClick = ckPhotoStimRepeatClick
      end
      object edPhotoStimPeriod: TValidatedEdit
        Left = 109
        Top = 347
        Width = 68
        Height = 20
        OnKeyPress = edPhotoStimPeriodKeyPress
        AutoSize = False
        Text = ' 100 ms'
        Value = 0.100000001490116100
        Scale = 1000.000000000000000000
        Units = 'ms'
        NumberFormat = '%.4g'
        LoLimit = -1.000000015047466E30
        HiLimit = 1.000000015047466E30
      end
      object bOpenProtocol: TButton
        Left = 8
        Top = 17
        Width = 150
        Height = 21
        Caption = 'Open Protocol...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
        OnClick = bOpenProtocolClick
      end
      object bSaveProtocolAs: TButton
        Left = 184
        Top = 17
        Width = 150
        Height = 21
        Caption = 'Save Protocol As...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 6
        OnClick = bSaveProtocolAsClick
      end
      object bSaveProtocol: TButton
        Left = 184
        Top = 46
        Width = 150
        Height = 21
        Caption = 'Save Protocol'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 7
        OnClick = bSaveProtocolClick
      end
      object bDuplicate: TButton
        Left = 184
        Top = 114
        Width = 150
        Height = 21
        Caption = 'Duplicate...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 8
        OnClick = bDuplicateClick
      end
    end
  end
  object ImageGrp: TGroupBox
    Left = 364
    Top = 0
    Width = 507
    Height = 529
    TabOrder = 1
    object Image: TImage
      Left = 8
      Top = 24
      Width = 489
      Height = 393
      OnDblClick = ImageDblClick
      OnMouseDown = ImageMouseDown
      OnMouseMove = ImageMouseMove
      OnMouseUp = ImageMouseUp
    end
    object pnlControls: TPanel
      Left = 8
      Top = 464
      Width = 185
      Height = 56
      BevelOuter = bvNone
      TabOrder = 0
      object lblCursorPosition: TLabel
        Left = 0
        Top = 4
        Width = 5
        Height = 13
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMenuHighlight
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object ckCalibrationBar: TCheckBox
        Left = 0
        Top = 20
        Width = 97
        Height = 17
        Caption = 'Calibration Bar'
        TabOrder = 0
        OnClick = ckCalibrationBarClick
      end
      object ckReferenceLine: TCheckBox
        Left = 0
        Top = 36
        Width = 97
        Height = 17
        Caption = 'Reference Line'
        TabOrder = 1
        OnClick = ckReferenceLineClick
      end
      object ckMoveAll: TCheckBox
        Left = 112
        Top = 20
        Width = 65
        Height = 17
        Caption = 'Move All'
        TabOrder = 2
      end
    end
  end
  object ImageFile: TImageFile
    XResolution = 1.000000000000000000
    YResolution = 1.000000000000000000
    ZResolution = 1.000000000000000000
    TResolution = 1.000000000000000000
    Left = 400
    Top = 536
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 432
    Top = 536
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'ppr'
    Filter = 'Photo-Stimulus Protocol (ppr)|*.ppr'
    Title = 'Save File As...'
    Left = 496
    Top = 536
  end
  object OpenDialog: TOpenDialog
    DefaultExt = ' '
    FileName = '*'
    Filter = ' '
    InitialDir = '\windows\ses'
    Options = [ofHideReadOnly, ofNoValidate, ofExtensionDifferent, ofEnableSizing]
    Left = 464
    Top = 536
  end
  object UltimaImageFile: TImageFile
    XResolution = 1.000000000000000000
    YResolution = 1.000000000000000000
    ZResolution = 1.000000000000000000
    TResolution = 1.000000000000000000
    Left = 368
    Top = 536
  end
  object menuTargets: TPopupMenu
    Left = 528
    Top = 536
    object menuDeleteTarget: TMenuItem
      Caption = 'Delete Target'
      OnClick = menuDeleteTargetClick
    end
    object menuSetAll: TMenuItem
      Caption = 'Set All'
      OnClick = menuSetAllClick
    end
  end
end
