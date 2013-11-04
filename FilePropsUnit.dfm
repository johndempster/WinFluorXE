object FilePropsFrm: TFilePropsFrm
  Left = 342
  Top = 154
  Width = 452
  Height = 523
  Caption = 'File Properties'
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
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Page: TPageControl
    Left = 8
    Top = 8
    Width = 369
    Height = 393
    ActivePage = HeaderTab
    TabOrder = 0
    object PropTab: TTabSheet
      Caption = 'Properties'
      object meProperties: TMemo
        Left = 8
        Top = 8
        Width = 345
        Height = 201
        Lines.Strings = (
          'meProperties')
        TabOrder = 0
      end
      object EditablePropsGrp: TGroupBox
        Left = 8
        Top = 216
        Width = 345
        Height = 145
        Caption = ' Edit Properties '
        TabOrder = 1
        object panInterval: TPanel
          Left = 8
          Top = 16
          Width = 209
          Height = 25
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = ' Inter-frame Interval'
          TabOrder = 0
          object edInterval: TValidatedEdit
            Left = 104
            Top = 1
            Width = 90
            Height = 22
            Text = ' 0 ms'
            Scale = 1000.000000000000000000
            Units = 'ms'
            NumberFormat = '%.6g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
        end
        object Panel2: TPanel
          Left = 8
          Top = 64
          Width = 209
          Height = 25
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = ' Pixel size'
          TabOrder = 1
          object edPixelSize: TValidatedEdit
            Left = 104
            Top = 1
            Width = 90
            Height = 22
            Text = ' 0 um'
            Scale = 1.000000000000000000
            Units = 'um'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
        end
        object Panel3: TPanel
          Left = 8
          Top = 88
          Width = 209
          Height = 25
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = ' Pixel units'
          TabOrder = 2
          object edPixelUnits: TEdit
            Left = 104
            Top = 2
            Width = 90
            Height = 22
            TabOrder = 0
            Text = 'edPixelUnits'
            OnKeyPress = edPixelUnitsKeyPress
          end
        end
        object bUpdateFileProps: TButton
          Left = 8
          Top = 120
          Width = 113
          Height = 17
          Caption = 'Update Properties'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
          OnClick = bUpdateFilePropsClick
        end
        object Panel4: TPanel
          Left = 8
          Top = 40
          Width = 209
          Height = 25
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = 'Initial Delay'
          TabOrder = 4
          object edImageStartDelay: TValidatedEdit
            Left = 104
            Top = 1
            Width = 90
            Height = 22
            Text = ' 0 ms'
            Scale = 1000.000000000000000000
            Units = 'ms'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
        end
      end
    end
    object ADCTab: TTabSheet
      Caption = 'Analogue Channels'
      ImageIndex = 1
      object ChannelsGrp: TGroupBox
        Left = 8
        Top = 0
        Width = 257
        Height = 257
        Caption = ' Channel calibration table '
        TabOrder = 0
        object ChannelTable: TStringGrid
          Left = 8
          Top = 16
          Width = 241
          Height = 177
          Hint = 'Input channel scaling factors and calibration units'
          ColCount = 4
          DefaultColWidth = 50
          DefaultRowHeight = 18
          RowCount = 9
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssNone
          TabOrder = 0
          RowHeights = (
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
        object panADCScanInterval: TPanel
          Left = 8
          Top = 200
          Width = 193
          Height = 25
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = ' Sampling Interval'
          TabOrder = 1
          object edADCScanInterval: TValidatedEdit
            Left = 96
            Top = 1
            Width = 73
            Height = 22
            Text = ' 0 ms'
            Scale = 1000.000000000000000000
            Units = 'ms'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
        end
        object bUpdateChannelProps: TButton
          Left = 8
          Top = 232
          Width = 113
          Height = 17
          Caption = 'Update Properties'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          OnClick = bUpdateChannelPropsClick
        end
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 272
        Width = 1
        Height = 17
        Caption = 'GroupBox1'
        TabOrder = 1
      end
    end
    object MarkerTab: TTabSheet
      Caption = 'Markers'
      ImageIndex = 2
      object MarkersGrp: TGroupBox
        Left = 8
        Top = 8
        Width = 281
        Height = 233
        Caption = ' Markers '
        TabOrder = 0
        object MarkerTable: TStringGrid
          Left = 8
          Top = 16
          Width = 241
          Height = 177
          Hint = 'Input channel scaling factors and calibration units'
          ColCount = 2
          DefaultColWidth = 150
          DefaultRowHeight = 18
          FixedCols = 0
          RowCount = 9
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssNone
          TabOrder = 0
          RowHeights = (
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
        object bUpdateMarkers: TButton
          Left = 8
          Top = 200
          Width = 113
          Height = 17
          Caption = 'Update Properties'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnClick = bUpdateMarkersClick
        end
      end
    end
    object FrameTypesTab: TTabSheet
      Caption = 'Frame Types'
      ImageIndex = 3
      object FrameTypesGrp: TGroupBox
        Left = 8
        Top = 8
        Width = 345
        Height = 233
        Caption = ' Frame Types'
        TabOrder = 0
        object FrameTypeTable: TStringGrid
          Left = 8
          Top = 16
          Width = 329
          Height = 177
          Hint = 'Input channel scaling factors and calibration units'
          ColCount = 2
          DefaultColWidth = 50
          DefaultRowHeight = 18
          RowCount = 9
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssNone
          TabOrder = 0
          RowHeights = (
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
        object bUpdateFrameTypes: TButton
          Left = 8
          Top = 200
          Width = 113
          Height = 17
          Caption = 'Update Properties'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          OnClick = bUpdateFrameTypesClick
        end
        object ckSpectralDataFile: TCheckBox
          Left = 128
          Top = 200
          Width = 129
          Height = 17
          Caption = 'Spectral Data File'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
        end
      end
    end
    object HeaderTab: TTabSheet
      Caption = 'File Header'
      ImageIndex = 4
      object meFileHeader: TMemo
        Left = 8
        Top = 8
        Width = 345
        Height = 353
        Lines.Strings = (
          'meProperties')
        TabOrder = 0
      end
    end
  end
end
