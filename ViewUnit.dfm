object ViewFrm: TViewFrm
  Tag = 51
  Left = 361
  Top = 227
  Caption = 'Stored Images'
  ClientHeight = 621
  ClientWidth = 714
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object ControlsGrp: TGroupBox
    Left = 8
    Top = 0
    Width = 185
    Height = 617
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = ControlsGrpClick
    object ControlGrp: TGroupBox
      Left = 8
      Top = 8
      Width = 169
      Height = 121
      Caption = ' Frame No. '
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Label1: TLabel
        Left = 27
        Top = 94
        Width = 28
        Height = 15
        Alignment = taRightJustify
        Caption = 'Time'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
      end
      object sbFrameNum: TScrollBar
        Left = 8
        Top = 38
        Width = 153
        Height = 18
        Hint = 'Select frame number to be displayed'
        PageSize = 0
        TabOrder = 0
        OnChange = sbFrameNumChange
      end
      object edFrameNum: TRangeEdit
        Left = 8
        Top = 16
        Width = 153
        Height = 20
        Hint = 'Frame number on display'
        OnKeyPress = edFrameNumKeyPress
        AutoSize = False
        Text = ' 1 / 1 '
        LoValue = 1.000000000000000000
        HiValue = 1.000000000000000000
        HiLimit = 1.000000015047466E30
        Scale = 1.000000000000000000
        NumberFormat = '%.0f / %.0f'
      end
      object edFrameTime: TEdit
        Left = 64
        Top = 94
        Width = 97
        Height = 20
        Hint = 'Time of acquisition of displayed frame'
        AutoSize = False
        ReadOnly = True
        TabOrder = 2
        Text = 'edFrameTime'
      end
      object GroupBox2: TGroupBox
        Left = 10
        Top = 59
        Width = 151
        Height = 32
        Hint = 'Frame playback controls'
        TabOrder = 3
        object bGoToStart: TButton
          Left = 28
          Top = 10
          Width = 18
          Height = 17
          Hint = 'Start of file'
          Caption = '9'
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Webdings'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnMouseDown = bGoToStartMouseDown
        end
        object bGoToEnd: TButton
          Left = 101
          Top = 10
          Width = 18
          Height = 17
          Hint = 'Go to end'
          Caption = ':'
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Webdings'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnMouseDown = bGoToEndMouseDown
        end
        object bBackwards: TButton
          Left = 48
          Top = 10
          Width = 14
          Height = 17
          Hint = 'Play backwards'
          Caption = '3'
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Webdings'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnMouseDown = bBackwardsMouseDown
        end
        object bForwards: TButton
          Left = 85
          Top = 10
          Width = 14
          Height = 17
          Hint = 'Play forwards'
          Caption = '4'
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Webdings'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnMouseDown = bForwardsMouseDown
        end
        object bStop: TButton
          Left = 64
          Top = 10
          Width = 18
          Height = 17
          Hint = 'Stop'
          Caption = '<'
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Webdings'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnMouseDown = bStopMouseDown
        end
      end
    end
    object DisplayGrp: TGroupBox
      Left = 8
      Top = 128
      Width = 169
      Height = 185
      Caption = 'Display  '
      TabOrder = 1
      object cbPalette: TComboBox
        Left = 8
        Top = 18
        Width = 153
        Height = 23
        Hint = 'Display colour mapping'
        Style = csDropDownList
        TabOrder = 0
        OnChange = cbPaletteChange
      end
      object ContrastPage: TPageControl
        Left = 3
        Top = 48
        Width = 158
        Height = 125
        ActivePage = RangeTab
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object RangeTab: TTabSheet
          Caption = 'Display Contrast'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object bFullScale: TButton
            Left = 2
            Top = 4
            Width = 71
            Height = 17
            Hint = 'Set display intensity range to cover full camera range'
            Caption = 'Full Range'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            OnClick = bFullScaleClick
          end
          object bMaxContrast: TButton
            Left = 80
            Top = 4
            Width = 65
            Height = 17
            Hint = 'Set display range to min. - max.  intensities within image'
            Caption = 'Best'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = bMaxContrastClick
          end
          object edDisplayIntensityRange: TRangeEdit
            Left = 2
            Top = 24
            Width = 143
            Height = 20
            Hint = 'Range of intensities displayed within image'
            OnKeyPress = edDisplayIntensityRangeKeyPress
            AutoSize = False
            ShowHint = True
            Text = ' 4096.00 - 4096.00 '
            LoValue = 4096.000000000000000000
            HiValue = 4096.000000000000000000
            HiLimit = 1.000000015047466E30
            Scale = 1.000000000000000000
            NumberFormat = '%.f - %.f'
          end
          object ckChangeAllFrameTypes: TCheckBox
            Left = 2
            Top = 59
            Width = 111
            Height = 19
            Hint = 'Update contrast on all image panels'
            Caption = 'All image panels'
            Checked = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            State = cbChecked
            TabOrder = 3
          end
          object ckContrast6SDOnly: TCheckBox
            Left = 2
            Top = 77
            Width = 105
            Height = 17
            Hint = 
              'Set maximum contrast range to 6 standard deviations rather than ' +
              'min-max range'
            Caption = '6 x s.d. only'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
            OnClick = ckContrast6SDOnlyClick
          end
          object ckAutoOptimise: TCheckBox
            Left = 2
            Top = 44
            Width = 119
            Height = 19
            Caption = 'Auto adjust'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 5
            OnClick = ckAutoOptimiseClick
          end
        end
        object SlidersTab: TTabSheet
          Caption = 'Sliders'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ImageIndex = 1
          ParentFont = False
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label5: TLabel
            Left = 0
            Top = 0
            Width = 49
            Height = 15
            Caption = 'Contrast'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label10: TLabel
            Left = 0
            Top = 40
            Width = 62
            Height = 15
            Caption = 'Brightness'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object sbContrast: TScrollBar
            Left = 3
            Top = 19
            Width = 142
            Height = 17
            Hint = 'Display contrast control slider'
            LargeChange = 10
            PageSize = 0
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnChange = sbContrastChange
          end
          object sbBrightness: TScrollBar
            Left = 3
            Top = 59
            Width = 142
            Height = 17
            Hint = 'Display brightness control slider'
            LargeChange = 10
            PageSize = 0
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnChange = sbContrastChange
          end
        end
      end
    end
    object ROIGrp: TGroupBox
      Left = 8
      Top = 360
      Width = 169
      Height = 121
      Caption = ' Regions of Interest '
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object bSaveROIs: TButton
        Left = 8
        Top = 46
        Width = 153
        Height = 17
        Hint = 'Save Regions of Interest to file'
        Caption = 'Save ROIs to File'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = bSaveROIsClick
      end
      object bLoadROis: TButton
        Left = 8
        Top = 68
        Width = 153
        Height = 17
        Hint = 'Load Regions of Interest from file'
        Caption = 'Load ROIs from File'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = bLoadROisClick
      end
      object bEditROIs: TButton
        Left = 8
        Top = 90
        Width = 153
        Height = 17
        Caption = 'Edit ROI Table'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = bEditROIsClick
      end
      object bDeleteROI: TButton
        Left = 8
        Top = 18
        Width = 65
        Height = 17
        Hint = 'Delete selected region of interest'
        Caption = 'Delete'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = bDeleteROIClick
      end
      object cbDeleteROI: TComboBox
        Left = 80
        Top = 18
        Width = 81
        Height = 23
        Hint = 'ROI no. to be deleted'
        Style = csDropDownList
        TabOrder = 4
      end
    end
    object MarkGrp: TGroupBox
      Left = 8
      Top = 543
      Width = 169
      Height = 66
      Caption = ' Markers '
      TabOrder = 3
      object edMarker: TEdit
        Left = 8
        Top = 38
        Width = 153
        Height = 20
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object bMark: TButton
        Left = 6
        Top = 16
        Width = 155
        Height = 18
        Hint = 'Add marker text to bottom of display'
        Caption = 'Add Marker'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = bMarkClick
      end
    end
    object ShadingGrp: TGroupBox
      Left = 8
      Top = 317
      Width = 169
      Height = 40
      Caption = ' Shading Correction '
      TabOrder = 4
      object ckBackgroundSubtraction: TCheckBox
        Left = 8
        Top = 16
        Width = 145
        Height = 17
        Caption = ' Correction Enabled'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = ckBackgroundSubtractionClick
      end
      object ShadeCorSettingsPanel: TPanel
        Left = 3
        Top = 66
        Width = 152
        Height = 74
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
  end
  object ImageGrp: TGroupBox
    Left = 200
    Top = 38
    Width = 497
    Height = 379
    Caption = ' Images '
    TabOrder = 1
    object Image1: TImage
      Left = 8
      Top = 40
      Width = 65
      Height = 57
      Cursor = crCross
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnDragDrop = Image1DragDrop
      OnDragOver = Image1DragOver
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image2: TImage
      Tag = 1
      Left = 105
      Top = 16
      Width = 64
      Height = 57
      Cursor = crCross
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnDragDrop = Image1DragDrop
      OnDragOver = Image1DragOver
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image3: TImage
      Tag = 2
      Left = 153
      Top = 16
      Width = 80
      Height = 57
      Cursor = crCross
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnDragDrop = Image1DragDrop
      OnDragOver = Image1DragOver
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image4: TImage
      Tag = 3
      Left = 10
      Top = 80
      Width = 63
      Height = 57
      Cursor = crCross
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnDragDrop = Image1DragDrop
      OnDragOver = Image1DragOver
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image5: TImage
      Left = 88
      Top = 80
      Width = 65
      Height = 57
      Cursor = crCross
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnDragDrop = Image1DragDrop
      OnDragOver = Image1DragOver
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image6: TImage
      Tag = 1
      Left = 161
      Top = 80
      Width = 64
      Height = 57
      Cursor = crCross
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnDragDrop = Image1DragDrop
      OnDragOver = Image1DragOver
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image7: TImage
      Tag = 3
      Left = 10
      Top = 144
      Width = 63
      Height = 57
      Cursor = crCross
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnDragDrop = Image1DragDrop
      OnDragOver = Image1DragOver
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image8: TImage
      Left = 88
      Top = 144
      Width = 65
      Height = 57
      Cursor = crCross
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnDragDrop = Image1DragDrop
      OnDragOver = Image1DragOver
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Image9: TImage
      Tag = 1
      Left = 161
      Top = 144
      Width = 64
      Height = 57
      Cursor = crCross
      OnClick = Image1Click
      OnDblClick = Image1DblClick
      OnDragDrop = Image1DragDrop
      OnDragOver = Image1DragOver
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
    object Label6: TLabel
      Left = 14
      Top = 16
      Width = 27
      Height = 14
      Alignment = taRightJustify
      Caption = 'Zoom'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object sbXScroll: TScrollBar
      Left = 8
      Top = 264
      Width = 353
      Height = 15
      PageSize = 0
      TabOrder = 0
      OnChange = sbXScrollChange
    end
    object sbYScroll: TScrollBar
      Left = 344
      Top = 16
      Width = 15
      Height = 153
      Kind = sbVertical
      PageSize = 0
      TabOrder = 1
      OnChange = sbYScrollChange
    end
    object PanROITools: TPanel
      Left = 8
      Top = 282
      Width = 441
      Height = 25
      BevelOuter = bvNone
      TabOrder = 2
      object ROIellipse: TImage
        Left = 92
        Top = 2
        Width = 16
        Height = 16
        Hint = 'Ellipse'
        ParentShowHint = False
        Picture.Data = {
          07544269746D61707E000000424D7E000000000000003E000000280000001000
          0000100000000100010000000000400000000000000000000000020000000000
          000000000000FFFFFF00000000007FFE00007FFE00007C3E0000700E000073CE
          000067E6000067E6000067E6000067E6000073CE0000700E00007C3E00007FFE
          00007FFE000000000000}
        ShowHint = True
        Transparent = True
        OnMouseDown = ROIPointMouseDown
      end
      object ROIPolyline: TImage
        Left = 110
        Top = 2
        Width = 16
        Height = 16
        Hint = 'Multi-segment line'
        ParentShowHint = False
        Picture.Data = {
          07544269746D61707E000000424D7E000000000000003E000000280000001000
          0000100000000100010000000000400000000000000000000000020000000000
          000000000000FFFFFF00000000007FFE00007FFE000067FE000063FE000071FE
          000079E6000061E6000043E6000047E6000067E6000070060000700600007FFE
          00007FFE000000000000}
        ShowHint = True
        Transparent = True
        OnMouseDown = ROIPointMouseDown
      end
      object ROIPolygon: TImage
        Left = 130
        Top = 2
        Width = 16
        Height = 16
        Hint = 'Multi-sided shape'
        ParentShowHint = False
        Picture.Data = {
          07544269746D61707E000000424D7E000000000000003E000000280000001000
          0000100000000100010000000000400000000000000000000000020000000000
          000000000000FFFFFF00000000007FFE0000787E0000703E0000630E00006786
          000063E6000071E6000079E6000071CE000063CE0000619E0000701E00007C3E
          00007FFE000000000000}
        ShowHint = True
        Transparent = True
        OnMouseDown = ROIPointMouseDown
      end
      object ROILine: TImage
        Left = 52
        Top = 2
        Width = 16
        Height = 16
        Hint = 'Line'
        ParentShowHint = False
        Picture.Data = {
          07544269746D61707E000000424D7E000000000000003E000000280000001000
          0000100000000100010000000000400000000000000000000000020000000000
          000000000000FFFFFF00000000007FFE00007FFE00007FE600007FC600007F8E
          00007F1E00007E3E00007C7E000078FE000071FE000063FE000067FE00007FFE
          00007FFE000000000000}
        ShowHint = True
        Transparent = True
        OnMouseDown = ROIPointMouseDown
      end
      object ROIPoint: TImage
        Left = 32
        Top = 2
        Width = 16
        Height = 16
        Hint = 'Point'
        ParentShowHint = False
        Picture.Data = {
          07544269746D61707E000000424D7E000000000000003E000000280000001000
          0000100000000100010000000000400000000000000000000000020000000000
          000000000000FFFFFF00000000007FFE00007FFE00007E7E00007E7E00007E7E
          00007FFE000062460000624600007FFE00007E7E00007E7E00007E7E00007FFE
          00007FFE000000000000}
        ShowHint = True
        Transparent = True
        OnMouseDown = ROIPointMouseDown
      end
      object Label2: TLabel
        Left = 2
        Top = 2
        Width = 27
        Height = 15
        Hint = 'Regions of Interest (Select by dragging on to image)'
        Caption = 'ROIs'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
      end
      object ROIRectangle: TImage
        Left = 72
        Top = 2
        Width = 16
        Height = 16
        Hint = 'Rectangle'
        ParentShowHint = False
        Picture.Data = {
          07544269746D61707E000000424D7E000000000000003E000000280000001000
          0000100000000100010000000000400000000000000000000000020000000000
          000000000000FFFFFF00000000007FFE00007FFE0000600600006006000067E6
          000067E6000067E6000067E6000067E6000067E6000060060000600600007FFE
          00007FFE000000000000}
        ShowHint = True
        Transparent = True
        OnMouseDown = ROIPointMouseDown
      end
      object Label4: TLabel
        Left = 152
        Top = 6
        Width = 81
        Height = 14
        Caption = 'Drag on to image'
      end
      object ckDisplayCalBar: TCheckBox
        Left = 264
        Top = 0
        Width = 153
        Height = 25
        Caption = 'Display calibration bar'
        Checked = True
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 0
        OnClick = ckDisplayCalBarClick
      end
    end
    object cbDisplayZoom: TComboBox
      Left = 48
      Top = 16
      Width = 81
      Height = 22
      Hint = 'Set display magification '
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = 'cbDisplayZoom'
      OnChange = cbDisplayZoomChange
    end
  end
  object IdentGrp: TGroupBox
    Left = 200
    Top = 1
    Width = 401
    Height = 35
    Hint = 'Experiment identification data line'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    object Label3: TLabel
      Left = 8
      Top = 11
      Width = 24
      Height = 14
      Caption = 'Expt.'
    end
    object edIdent: TEdit
      Left = 40
      Top = 11
      Width = 353
      Height = 20
      AutoSize = False
      TabOrder = 0
      Text = 'edIdent'
      OnKeyUp = edIdentKeyUp
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 60
    OnTimer = TimerTimer
    Left = 184
    Top = 424
  end
  object SaveDialog: TSaveDialog
    Left = 208
    Top = 496
  end
  object OpenDialog: TOpenDialog
    Left = 216
    Top = 440
  end
  object IDRBackground: TIDRFile
    NumFrames = 0
    NumFrameTypes = 1
    FrameWidth = 0
    FrameHeight = 0
    PixelDepth = 1
    NumZSections = 1
    ZSpacing = 1.000000000000000000
    IntensityScale = 1.000000000000000000
    XResolution = 1.000000000000000000
    ADCNumScansInFile = 0
    ADCNumChannels = 0
    ADCNumScansPerFrame = 0
    ADCMaxValue = 0
    LineScan = False
    LSTimeCoursePixel = 0
    LSTimeCourseNumAvg = 1
    LSTimeCourseBackgroundPixel = 0
    LSSubtractBackground = False
    WriteEnabled = False
    SpectralDataFile = False
    EventDisplayDuration = 1.000000000000000000
    EventDeadTime = 1.000000000000000000
    EventDetectionThreshold = 1000.000000000000000000
    EventDetectionThresholdPolarity = 0
    EventDetectionSource = 0
    EventROI = 0
    EventBackgROI = 0
    EventFixedBaseline = True
    EventRollingBaselinePeriod = 1.000000000000000000
    EventBaselineLevel = 0
    EventRatioExclusionThreshold = 0
    EventRatioTop = 0
    EventRatioBottom = 1
    EventRatioDisplayMax = 10.000000000000000000
    EventRatioRMax = 1.000000000000000000
    EventFLWave = 0
    EventF0Wave = 0
    EventF0Start = 1
    EventF0End = 1
    EventF0UseConstant = False
    EventF0DisplayMax = 10.000000000000000000
    EventF0SubtractF0 = False
    Left = 288
    Top = 456
  end
end
