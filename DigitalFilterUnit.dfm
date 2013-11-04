object DigitalFilterFrm: TDigitalFilterFrm
  Left = 272
  Top = 199
  Width = 276
  Height = 232
  Caption = 'Filter Analog Signals'
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
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object FilterGrp: TGroupBox
    Left = 8
    Top = 2
    Width = 153
    Height = 159
    Caption = ' Filter Operation  '
    TabOrder = 0
    object rbLowPass: TRadioButton
      Left = 8
      Top = 16
      Width = 81
      Height = 17
      Caption = 'Low Pass'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
      OnClick = rbLowPassClick
    end
    object rbHighPass: TRadioButton
      Left = 8
      Top = 32
      Width = 81
      Height = 17
      Caption = 'High Pass'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = rbHighPassClick
    end
    object Filter: TPageControl
      Left = 8
      Top = 96
      Width = 105
      Height = 57
      ActivePage = LPFilter
      Style = tsFlatButtons
      TabOrder = 2
      object LPFilter: TTabSheet
        Caption = 'LPFilter'
        TabVisible = False
        object Label1: TLabel
          Left = 1
          Top = 2
          Width = 81
          Height = 13
          Caption = 'Cut-off frequency'
        end
        object edLPCutOffFreq: TValidatedEdit
          Left = 0
          Top = 18
          Width = 81
          Height = 20
          AutoSize = False
          Text = ' 0 Hz'
          Scale = 1.000000000000000000
          Units = 'Hz'
          NumberFormat = '%.5g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
      end
      object HPFilter: TTabSheet
        Caption = 'HPFilter'
        ImageIndex = 1
        TabVisible = False
        object Label2: TLabel
          Left = 0
          Top = 0
          Width = 81
          Height = 13
          Caption = 'Cut-off frequency'
        end
        object cbHPFilter: TComboBox
          Left = 0
          Top = 18
          Width = 89
          Height = 21
          ItemHeight = 0
          TabOrder = 0
          Text = 'cbHPFilter'
        end
      end
      object NFFilter: TTabSheet
        ImageIndex = 2
        TabVisible = False
        object Label4: TLabel
          Left = 1
          Top = 2
          Width = 81
          Height = 13
          Caption = 'Cut-off frequency'
        end
        object edNFCutOffFreq: TValidatedEdit
          Left = 0
          Top = 18
          Width = 81
          Height = 20
          AutoSize = False
          Text = ' 0 Hz'
          Scale = 1.000000000000000000
          Units = 'Hz'
          NumberFormat = '%.5g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
      end
      object NoneTab: TTabSheet
        Caption = 'NoneTab'
        ImageIndex = 3
        TabVisible = False
      end
    end
    object rbNotchFilter: TRadioButton
      Left = 8
      Top = 48
      Width = 81
      Height = 17
      Caption = 'Notch'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = rbNotchFilterClick
    end
    object rbInvert: TRadioButton
      Left = 8
      Top = 64
      Width = 89
      Height = 17
      Caption = 'Invert'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      OnClick = rbInvertClick
    end
    object rbRestoreOriginal: TRadioButton
      Left = 8
      Top = 80
      Width = 121
      Height = 17
      Caption = 'Restore Original'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = rbRestoreOriginalClick
    end
  end
  object bOK: TButton
    Left = 8
    Top = 168
    Width = 65
    Height = 25
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
    Left = 80
    Top = 168
    Width = 57
    Height = 17
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
  object ChannelsGrp: TGroupBox
    Left = 168
    Top = 2
    Width = 97
    Height = 159
    Caption = ' Channels'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object ckInUse0: TCheckBox
      Left = 8
      Top = 16
      Width = 81
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Ch.0'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object ckInUse1: TCheckBox
      Left = 8
      Top = 32
      Width = 81
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Ch.0'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object ckInUse2: TCheckBox
      Left = 8
      Top = 48
      Width = 81
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Ch.0'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object ckInUse3: TCheckBox
      Left = 8
      Top = 64
      Width = 81
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Ch.0'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object ckInUse4: TCheckBox
      Left = 8
      Top = 80
      Width = 81
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Ch.0'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object ckInUse5: TCheckBox
      Left = 8
      Top = 96
      Width = 81
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Ch.0'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object ckInUse6: TCheckBox
      Left = 8
      Top = 112
      Width = 81
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Ch.0'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object ckInUse7: TCheckBox
      Left = 8
      Top = 128
      Width = 81
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Ch.0'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
  end
end
