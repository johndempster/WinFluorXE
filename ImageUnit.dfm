object ImageFrm: TImageFrm
  Left = 192
  Top = 107
  Width = 563
  Height = 465
  Caption = 'ImageFrm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ControlGrp: TGroupBox
    Left = 2
    Top = 8
    Width = 175
    Height = 425
    TabOrder = 0
    object DisplayGrp: TGroupBox
      Left = 8
      Top = 64
      Width = 161
      Height = 193
      Caption = ' Display '
      TabOrder = 0
      object Label6: TLabel
        Left = 53
        Top = 42
        Width = 27
        Height = 13
        Alignment = taRightJustify
        Caption = 'Zoom'
      end
      object cbDisplayZoom: TComboBox
        Left = 88
        Top = 42
        Width = 65
        Height = 21
        Hint = 'Display magnification'
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
      object cbPalette: TComboBox
        Left = 8
        Top = 16
        Width = 145
        Height = 21
        Hint = 'Display colour palette'
        Style = csDropDownList
        ItemHeight = 13
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object ContrastPage: TPageControl
        Left = 5
        Top = 68
        Width = 150
        Height = 117
        ActivePage = SlidersTab
        MultiLine = True
        TabOrder = 2
        object RangeTab: TTabSheet
          Caption = 'Display Contrast'
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
          end
          object bMaxContrast: TButton
            Left = 78
            Top = 4
            Width = 59
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
          end
          object edDisplayIntensityRange: TRangeEdit
            Left = 2
            Top = 24
            Width = 135
            Height = 20
            Hint = 'Range of intensities displayed within image'
            AutoSize = False
            ShowHint = True
            Text = ' 4096 - 4096 '
            LoValue = 4096.000000000000000000
            HiValue = 4096.000000000000000000
            HiLimit = 1.000000015047466E30
            Scale = 1.000000000000000000
            NumberFormat = '%.f - %.f'
          end
          object ckContrast6SDOnly: TCheckBox
            Left = 2
            Top = 45
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
            TabOrder = 3
          end
        end
        object SlidersTab: TTabSheet
          Caption = 'Sliders'
          ImageIndex = 1
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
            Width = 135
            Height = 17
            Hint = 'Display contrast control slider'
            LargeChange = 10
            PageSize = 0
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
          object sbBrightness: TScrollBar
            Left = 3
            Top = 59
            Width = 137
            Height = 17
            Hint = 'Display brightness control slider'
            LargeChange = 10
            PageSize = 0
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
          end
        end
      end
    end
    object SnapGrp: TGroupBox
      Left = 8
      Top = 10
      Width = 161
      Height = 49
      Caption = ' Snap Image '
      TabOrder = 1
      object bSnapImage: TButton
        Left = 8
        Top = 16
        Width = 145
        Height = 17
        Caption = 'Snap Image'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
    end
  end
  object ImageGrp: TGroupBox
    Left = 182
    Top = 6
    Width = 363
    Height = 339
    Caption = ' Images '
    TabOrder = 1
    object Image1: TImage
      Left = 8
      Top = 16
      Width = 329
      Height = 185
    end
    object sbXScroll: TScrollBar
      Left = 8
      Top = 210
      Width = 353
      Height = 15
      PageSize = 0
      TabOrder = 0
    end
    object sbYScroll: TScrollBar
      Left = 336
      Top = 24
      Width = 15
      Height = 153
      Kind = sbVertical
      PageSize = 0
      TabOrder = 1
    end
    object ROIPanel: TPanel
      Left = 8
      Top = 232
      Width = 321
      Height = 21
      BevelOuter = bvNone
      TabOrder = 2
      object lbReadout: TLabel
        Left = 152
        Top = 0
        Width = 55
        Height = 14
        Caption = 'lbReadout'
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ckDisplayCalBar: TCheckBox
        Left = 0
        Top = 0
        Width = 145
        Height = 17
        Alignment = taLeftJustify
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
      end
    end
  end
end
