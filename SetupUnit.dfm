object SetupFrm: TSetupFrm
  Tag = 41
  Left = 488
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Camera/System Setup'
  ClientHeight = 600
  ClientWidth = 582
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
  object bOK: TButton
    Left = 8
    Top = 567
    Width = 49
    Height = 24
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 0
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 64
    Top = 567
    Width = 49
    Height = 17
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 1
    OnClick = bCancelClick
  end
  object CardGrp: TGroupBox
    Left = 8
    Top = 464
    Width = 558
    Height = 97
    Caption = ' Interface Cards '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object rbNIDAQ: TRadioButton
      Left = 8
      Top = 30
      Width = 137
      Height = 17
      Caption = 'NI-DAQ (Traditional)'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
      OnClick = rbNIDAQClick
    end
    object rbNIDAQMX: TRadioButton
      Left = 8
      Top = 15
      Width = 81
      Height = 17
      Caption = 'NI-DAQ-MX'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = rbNIDAQMXClick
    end
    object meDeviceList: TMemo
      Left = 136
      Top = 13
      Width = 409
      Height = 76
      Lines.Strings = (
        'meDeviceList')
      ReadOnly = True
      TabOrder = 2
    end
    object bResetDevices: TButton
      Left = 24
      Top = 48
      Width = 89
      Height = 25
      Caption = 'Reset Devices'
      TabOrder = 3
      OnClick = bResetDevicesClick
    end
    object ckAutoReset: TCheckBox
      Left = 8
      Top = 75
      Width = 123
      Height = 17
      Caption = 'Auto reset devices'
      TabOrder = 4
    end
  end
  object TabPage: TPageControl
    Left = 8
    Top = 8
    Width = 561
    Height = 450
    ActivePage = CameraTab
    TabOrder = 3
    object CameraTab: TTabSheet
      Caption = 'Camera'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox4: TGroupBox
        Left = 8
        Top = 0
        Width = 225
        Height = 409
        Caption = ' Camera '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object Label41: TLabel
          Left = 42
          Top = 360
          Width = 90
          Height = 14
          Alignment = taRightJustify
          Caption = 'Tempr. Set Point'
        end
        object cbCamera: TComboBox
          Left = 8
          Top = 16
          Width = 209
          Height = 22
          Hint = 'Camera type in use'
          ParentCustomHint = False
          TabOrder = 0
          Text = 'cbCamera'
          OnChange = cbCameraChange
        end
        object ModePanel: TPanel
          Left = 8
          Top = 40
          Width = 210
          Height = 44
          BevelOuter = bvNone
          TabOrder = 1
          object lbCameraMode: TLabel
            Left = 0
            Top = 3
            Width = 31
            Height = 14
            Alignment = taRightJustify
            Caption = 'Mode'
          end
          object cbCameraMode: TComboBox
            Left = 0
            Top = 19
            Width = 209
            Height = 22
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbCameraModeChange
          end
        end
        object ComPanel: TPanel
          Left = 8
          Top = 124
          Width = 210
          Height = 30
          BevelOuter = bvNone
          TabOrder = 2
          object lbComPort: TLabel
            Left = 52
            Top = 3
            Width = 52
            Height = 14
            Alignment = taRightJustify
            Caption = 'COM Port'
          end
          object cbCameraPort: TComboBox
            Left = 112
            Top = 3
            Width = 97
            Height = 22
            Hint = 'Camera control port'
            ParentCustomHint = False
            TabOrder = 0
            Text = 'cbCameraGain'
            OnChange = cbCameraPortChange
            Items.Strings = (
              'COM1'
              'COM2'
              'COM3'
              'COM4')
          end
        end
        object ReadoutSpeedPanel: TPanel
          Left = 8
          Top = 94
          Width = 210
          Height = 30
          BevelOuter = bvNone
          TabOrder = 3
          object Label1: TLabel
            Left = 21
            Top = 3
            Width = 83
            Height = 14
            Alignment = taRightJustify
            Caption = 'Readout Speed'
          end
          object cbReadoutSpeed: TComboBox
            Left = 112
            Top = 3
            Width = 97
            Height = 22
            Hint = 'Camera readout speed'
            Style = csDropDownList
            TabOrder = 0
          end
        end
        object edTemperatureSetPoint: TValidatedEdit
          Left = 136
          Top = 360
          Width = 81
          Height = 20
          Hint = 'Camera cooler temperature set point'
          AutoSize = False
          ShowHint = True
          Text = ' 0 DEGC'
          Scale = 1.000000000000000000
          Units = 'DEGC'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object ADCPanel: TPanel
          Left = 7
          Top = 154
          Width = 210
          Height = 30
          BevelOuter = bvNone
          TabOrder = 5
          object Label48: TLabel
            Left = 3
            Top = 3
            Width = 77
            Height = 14
            Alignment = taRightJustify
            Caption = 'A/D Converter'
          end
          object cbCameraADC: TComboBox
            Left = 88
            Top = 3
            Width = 121
            Height = 22
            Hint = 'Camera readout channel'
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbCameraADCChange
          end
        end
        object AuxCameraPanel: TPanel
          Left = 7
          Top = 48
          Width = 210
          Height = 49
          BevelOuter = bvNone
          TabOrder = 6
          object Label37: TLabel
            Left = 1
            Top = 4
            Width = 96
            Height = 14
            Caption = 'Auxilliary Camera'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object cbAuxCamera: TComboBox
            Left = 1
            Top = 20
            Width = 209
            Height = 22
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            Text = 'cbCamera'
            OnChange = cbCameraChange
          end
        end
        object CameraPanel: TPanel
          Left = 8
          Top = 208
          Width = 209
          Height = 48
          BevelOuter = bvNone
          TabOrder = 7
          object Label77: TLabel
            Left = 4
            Top = 4
            Width = 102
            Height = 14
            Caption = 'Available Cameras'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object cbCameraNames: TComboBox
            Left = 0
            Top = 20
            Width = 209
            Height = 22
            Style = csDropDownList
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            OnChange = cbCameraNamesChange
          end
        end
        object ckDisableExposureIntervalLimit: TCheckBox
          Left = 12
          Top = 380
          Width = 205
          Height = 25
          Hint = 
            'Q Imaging cameras only: Tick to allow exposure intervals less th' +
            'an reported readout time of camera '
          Alignment = taLeftJustify
          Caption = 'Disable Exposure Time Checking'
          Ctl3D = True
          ParentCtl3D = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
        end
      end
      object GroupBox14: TGroupBox
        Left = 240
        Top = 170
        Width = 245
        Height = 121
        Caption = ' Calibration '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object Label19: TLabel
          Left = 59
          Top = 18
          Width = 105
          Height = 14
          Alignment = taRightJustify
          Caption = 'Lens magnification'
        end
        object Label20: TLabel
          Left = 112
          Top = 42
          Width = 52
          Height = 14
          Alignment = taRightJustify
          Caption = 'Pixel size'
        end
        object Label23: TLabel
          Left = 50
          Top = 66
          Width = 114
          Height = 14
          Alignment = taRightJustify
          Caption = 'Calibration bar (size)'
        end
        object Label61: TLabel
          Left = 42
          Top = 90
          Width = 122
          Height = 14
          Alignment = taRightJustify
          Caption = 'Calibration bar (width)'
        end
        object edLensMagnification: TValidatedEdit
          Left = 168
          Top = 18
          Width = 65
          Height = 20
          Hint = 'Microscope objective lens magnification'
          OnKeyPress = edLensMagnificationKeyPress
          AutoSize = False
          ShowHint = True
          Text = ' 0 X'
          Scale = 1.000000000000000000
          Units = 'X'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edPixelWidth: TValidatedEdit
          Left = 168
          Top = 42
          Width = 65
          Height = 20
          Hint = 'Camera pixel size'
          AutoSize = False
          ShowHint = True
          Text = ' 0 um'
          Scale = 1.000000000000000000
          Units = 'um'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edCalibrationBarSize: TValidatedEdit
          Left = 168
          Top = 66
          Width = 65
          Height = 20
          Hint = 'Calibration bar size (um)'
          AutoSize = False
          ShowHint = True
          Text = ' 0 um'
          Scale = 1.000000000000000000
          Units = 'um'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object edCalibrationBarThickness: TValidatedEdit
          Left = 168
          Top = 90
          Width = 65
          Height = 20
          Hint = 'Calibration bar width (pixels)'
          AutoSize = False
          ShowHint = True
          Text = ' 0 pixels'
          Scale = 1.000000000000000000
          Units = 'pixels'
          NumberFormat = '%.3g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
      end
      object GroupBox9: TGroupBox
        Left = 239
        Top = 0
        Width = 246
        Height = 166
        Caption = ' Exposure Trigger Output '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        object Label16: TLabel
          Left = 42
          Top = 116
          Width = 115
          Height = 14
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Trigger Delay'
        end
        object Label42: TLabel
          Left = 42
          Top = 140
          Width = 115
          Height = 14
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Extra Readout Time'
        end
        object cbCameraStart: TComboBox
          Left = 40
          Top = 18
          Width = 193
          Height = 22
          Hint = 'Camera exposure trigger output channel'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object rbCameraStartActiveHigh: TRadioButton
          Left = 40
          Top = 42
          Width = 81
          Height = 17
          Caption = 'Active High'
          Checked = True
          TabOrder = 1
          TabStop = True
        end
        object rbCameraStartActiveLow: TRadioButton
          Left = 128
          Top = 42
          Width = 89
          Height = 17
          Caption = 'Active Low'
          TabOrder = 2
        end
        object edCameraTriggerOffset: TValidatedEdit
          Left = 161
          Top = 116
          Width = 73
          Height = 20
          Hint = 
            'Delay between wavelength change and camera exposure trigger adde' +
            'd to camera trigger'
          AutoSize = False
          Text = ' 0 ms'
          Scale = 1000.000000000000000000
          Units = 'ms'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object edCameraReadoutTime: TValidatedEdit
          Left = 161
          Top = 140
          Width = 73
          Height = 20
          Hint = 'Extra readout camera readout time (subtracted from exposure)'
          AutoSize = False
          Text = ' 0 ms'
          Scale = 1000.000000000000000000
          Units = 'ms'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E30
        end
        object ckCCDClearPreExposure: TCheckBox
          Left = 40
          Top = 78
          Width = 194
          Height = 17
          Hint = 'Clear CCD image area before exposure (triggered exposures only)'
          Alignment = taLeftJustify
          Caption = 'Clear CCD before exposure'
          TabOrder = 5
        end
        object ckPostExposureReadout: TCheckBox
          Left = 40
          Top = 96
          Width = 194
          Height = 17
          Hint = 
            'Adjust exposure time to account for CCD readout time after expos' +
            'ure (triggered exposures only)'
          Alignment = taLeftJustify
          Caption = 'Post-exposure CCD Readout'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
        end
        object ckBulbExposureMode: TCheckBox
          Left = 40
          Top = 60
          Width = 194
          Height = 17
          Hint = 'Exposure time controlled by trigger output'
          Alignment = taLeftJustify
          Caption = 'Bulb exposure mode'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
        end
      end
      object SplitImageGrp: TGroupBox
        Left = 240
        Top = 296
        Width = 245
        Height = 49
        Caption = ' Split Image Labels '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        object Label75: TLabel
          Left = 6
          Top = 18
          Width = 33
          Height = 14
          Alignment = taRightJustify
          Caption = 'Upper'
        end
        object Label76: TLabel
          Left = 115
          Top = 18
          Width = 36
          Height = 14
          Alignment = taRightJustify
          Caption = 'Lower'
        end
        object edSplitImageUpper: TEdit
          Left = 45
          Top = 18
          Width = 60
          Height = 22
          Hint = 
            'Label of upper half of camera image when split image mode select' +
            'ed'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'A'
        end
        object edSplitImageLower: TEdit
          Left = 157
          Top = 18
          Width = 60
          Height = 22
          Hint = 
            'Label of lower half of camera image when split image mode select' +
            'ed'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = 'B'
        end
      end
      object GroupBox18: TGroupBox
        Left = 240
        Top = 352
        Width = 245
        Height = 57
        Caption = ' Camera Dark Level Range'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        object Label78: TLabel
          Left = 8
          Top = 16
          Width = 36
          Height = 14
          Alignment = taRightJustify
          Caption = 'Lower'
        end
        object Label79: TLabel
          Left = 120
          Top = 16
          Width = 33
          Height = 14
          Alignment = taRightJustify
          Caption = 'Upper'
        end
        object edDarkLevelLo: TValidatedEdit
          Left = 50
          Top = 16
          Width = 60
          Height = 20
          Hint = 'Lower limit of camera dark level range (grey levels)'
          ParentCustomHint = False
          AutoSize = False
          ShowHint = True
          Text = ' -1 '
          Value = -1.000000000000000000
          Scale = 1.000000000000000000
          NumberFormat = '%.0f'
          LoLimit = -1.000000000000000000
          HiLimit = 65000.000000000000000000
        end
        object edDarkLevelHi: TValidatedEdit
          Left = 157
          Top = 16
          Width = 60
          Height = 20
          Hint = 'Upper limit of camera dark level range (grey levels)'
          AutoSize = False
          Text = ' -1 '
          Value = -1.000000000000000000
          Scale = 1.000000000000000000
          NumberFormat = '%.0f'
          LoLimit = -1.000000000000000000
          HiLimit = 65000.000000000000000000
        end
      end
    end
    object LightSourceTab: TTabSheet
      Caption = 'Light Source'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LSShutterGrp: TGroupBox
        Left = 280
        Top = 0
        Width = 177
        Height = 217
        Caption = ' Shutter Control '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object Label43: TLabel
          Left = 8
          Top = 16
          Width = 89
          Height = 30
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Shutter closed wavelength'
          WordWrap = True
        end
        object Label40: TLabel
          Left = 8
          Top = 80
          Width = 89
          Height = 30
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Frame blanking period'
          WordWrap = True
        end
        object Label68: TLabel
          Left = 7
          Top = 49
          Width = 89
          Height = 30
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Opening time'
          WordWrap = True
        end
        object edShutterClosedWavelength: TValidatedEdit
          Left = 104
          Top = 16
          Width = 63
          Height = 20
          Hint = 'Laser #1 emission wavelength (nm)'
          AutoSize = False
          ShowHint = True
          Text = ' 10 nm'
          Value = 10.000000000000000000
          Scale = 1.000000000000000000
          Units = 'nm'
          NumberFormat = '%.4g'
          LoLimit = 1.000000000000000000
          HiLimit = 100000.000000000000000000
        end
        object edShutterBlankingPeriod: TValidatedEdit
          Left = 104
          Top = 80
          Width = 63
          Height = 20
          Hint = 
            'Closed shutter blanking period at end of exposure cycle for CCD ' +
            'readout'
          AutoSize = False
          ShowHint = True
          Text = ' 0 ms'
          Scale = 1000.000000000000000000
          Units = 'ms'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object GroupBox3: TGroupBox
          Left = 8
          Top = 116
          Width = 161
          Height = 93
          Caption = ' Shutter Control Output '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          object cbLSShutter: TComboBox
            Left = 8
            Top = 19
            Width = 145
            Height = 22
            Hint = 'Shutter AO/DO control output line.'
            Style = csDropDownList
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
          object rbLSShutterActiveHigh: TRadioButton
            Left = 8
            Top = 48
            Width = 89
            Height = 17
            Hint = 'Use TTL High (5V) to open shutter'
            Caption = 'Active High'
            Checked = True
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            TabStop = True
          end
          object rbLSShutterActiveLow: TRadioButton
            Left = 8
            Top = 64
            Width = 89
            Height = 17
            Hint = 'Use TTL Low (0V) to open shutter'
            Caption = 'Active Low'
            Color = clBtnFace
            ParentColor = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
          end
        end
        object edShutterChangeTime: TValidatedEdit
          Left = 103
          Top = 49
          Width = 63
          Height = 20
          Hint = 'Shutter open/close switching time'
          AutoSize = False
          ShowHint = True
          Text = ' 1000 ms'
          Value = 1.000000000000000000
          Scale = 1000.000000000000000000
          Units = 'ms'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
      end
      object EmFilterGrp: TGroupBox
        Left = 280
        Top = 224
        Width = 177
        Height = 169
        Caption = ' Emission Filter '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        object Label74: TLabel
          Left = 7
          Top = 95
          Width = 89
          Height = 18
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Change Time'
          WordWrap = True
        end
        object EmFilterControlGrp: TGroupBox
          Left = 8
          Top = 16
          Width = 161
          Height = 73
          Caption = ' Filter Control Outputs '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          object Label72: TLabel
            Left = 14
            Top = 18
            Width = 28
            Height = 13
            Alignment = taRightJustify
            Caption = 'Start'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label73: TLabel
            Left = 19
            Top = 44
            Width = 23
            Height = 13
            Alignment = taRightJustify
            Caption = 'End'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object cbEMFilterStart: TComboBox
            Left = 48
            Top = 16
            Width = 110
            Height = 22
            Hint = 'Start of emission filter DO output range'
            Style = csDropDownList
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
          object cbEmFilterEnd: TComboBox
            Left = 48
            Top = 44
            Width = 110
            Height = 22
            Hint = 'End of emission filter DO output range'
            Style = csDropDownList
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
          end
        end
        object edEmFilterChangeTime: TValidatedEdit
          Left = 106
          Top = 95
          Width = 63
          Height = 20
          Hint = 'Emission filter switching time'
          AutoSize = False
          ShowHint = True
          Text = ' 1000 ms'
          Value = 1.000000000000000000
          Scale = 1000.000000000000000000
          Units = 'ms'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
      end
      object LightSourceGrp: TGroupBox
        Left = 8
        Top = 0
        Width = 265
        Height = 418
        Caption = 'Light Source  '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object LSWaveGrp: TGroupBox
          Left = 8
          Top = 147
          Width = 249
          Height = 246
          Caption = ' Light Source Control Outputs  '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          object LSControl0: TPanel
            Left = 8
            Top = 20
            Width = 233
            Height = 28
            BevelOuter = bvNone
            TabOrder = 0
            object lbLSControl0: TLabel
              Left = 88
              Top = 1
              Width = 28
              Height = 13
              Alignment = taRightJustify
              Caption = 'Start'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbLSControl0: TComboBox
              Tag = 1
              Left = 123
              Top = 1
              Width = 110
              Height = 22
              Style = csDropDownList
              TabOrder = 0
              OnChange = cbLSControl0Change
            end
          end
          object lsControl1: TPanel
            Left = 8
            Top = 47
            Width = 233
            Height = 28
            BevelOuter = bvNone
            TabOrder = 1
            object lbLSControl1: TLabel
              Left = 88
              Top = 1
              Width = 28
              Height = 13
              Alignment = taRightJustify
              Caption = 'Start'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbLSControl1: TComboBox
              Tag = 1
              Left = 123
              Top = 1
              Width = 110
              Height = 22
              Style = csDropDownList
              TabOrder = 0
              OnChange = cbLSControl0Change
            end
          end
          object lsControl2: TPanel
            Left = 8
            Top = 74
            Width = 233
            Height = 28
            BevelOuter = bvNone
            TabOrder = 2
            object lbLSControl2: TLabel
              Left = 88
              Top = 1
              Width = 28
              Height = 13
              Alignment = taRightJustify
              Caption = 'Start'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbLSControl2: TComboBox
              Tag = 1
              Left = 123
              Top = 1
              Width = 110
              Height = 22
              Style = csDropDownList
              TabOrder = 0
              OnChange = cbLSControl0Change
            end
          end
          object lsControl3: TPanel
            Left = 8
            Top = 101
            Width = 233
            Height = 28
            BevelOuter = bvNone
            TabOrder = 3
            object lbLSControl3: TLabel
              Left = 88
              Top = 1
              Width = 28
              Height = 13
              Alignment = taRightJustify
              Caption = 'Start'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbLSControl03: TComboBox
              Tag = 1
              Left = 123
              Top = 1
              Width = 110
              Height = 22
              Style = csDropDownList
              TabOrder = 0
              OnChange = cbLSControl0Change
            end
          end
          object lsControl4: TPanel
            Left = 8
            Top = 128
            Width = 233
            Height = 28
            BevelOuter = bvNone
            TabOrder = 4
            object lbLSControl4: TLabel
              Left = 88
              Top = 1
              Width = 28
              Height = 13
              Alignment = taRightJustify
              Caption = 'Start'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbLSControl4: TComboBox
              Tag = 1
              Left = 123
              Top = 1
              Width = 110
              Height = 22
              Style = csDropDownList
              TabOrder = 0
              OnChange = cbLSControl0Change
            end
          end
          object lsControl5: TPanel
            Left = 8
            Top = 155
            Width = 233
            Height = 28
            BevelOuter = bvNone
            TabOrder = 5
            object lbLSControl5: TLabel
              Left = 88
              Top = 1
              Width = 28
              Height = 13
              Alignment = taRightJustify
              Caption = 'Start'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbLSControl05: TComboBox
              Tag = 1
              Left = 123
              Top = 1
              Width = 110
              Height = 22
              Style = csDropDownList
              TabOrder = 0
              OnChange = cbLSControl0Change
            end
          end
          object lsControl6: TPanel
            Left = 8
            Top = 180
            Width = 233
            Height = 28
            BevelOuter = bvNone
            TabOrder = 6
            object lbLSControl6: TLabel
              Left = 88
              Top = 1
              Width = 28
              Height = 13
              Alignment = taRightJustify
              Caption = 'Start'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbLSControl06: TComboBox
              Tag = 1
              Left = 123
              Top = 1
              Width = 110
              Height = 22
              Style = csDropDownList
              TabOrder = 0
              OnChange = cbLSControl0Change
            end
          end
          object lsControl7: TPanel
            Left = 8
            Top = 206
            Width = 233
            Height = 30
            BevelOuter = bvNone
            TabOrder = 7
            object lbLSControl7: TLabel
              Left = 89
              Top = 1
              Width = 28
              Height = 13
              Alignment = taRightJustify
              Caption = 'Start'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbLSControl7: TComboBox
              Tag = 1
              Left = 123
              Top = 1
              Width = 110
              Height = 22
              Style = csDropDownList
              TabOrder = 0
              OnChange = cbLSControl0Change
            end
          end
        end
        object cbLightSource: TComboBox
          Left = 8
          Top = 20
          Width = 249
          Height = 22
          Style = csDropDownList
          Sorted = True
          TabOrder = 0
          OnChange = cbLightSourceChange
        end
        object LSLEDGrp: TGroupBox
          Left = 7
          Top = 82
          Width = 250
          Height = 59
          Caption = ' LED O/P Voltages '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
          object Label35: TLabel
            Left = 11
            Top = 13
            Width = 60
            Height = 14
            Caption = 'Off Voltage'
          end
          object Label36: TLabel
            Left = 100
            Top = 13
            Width = 66
            Height = 14
            Caption = 'Max Voltage'
          end
          object edLEDOffVoltage: TValidatedEdit
            Left = 8
            Top = 29
            Width = 80
            Height = 20
            AutoSize = False
            Text = ' 0 V'
            Scale = 1.000000000000000000
            Units = 'V'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
          object edLEDMaxVoltage: TValidatedEdit
            Left = 100
            Top = 29
            Width = 80
            Height = 20
            AutoSize = False
            Text = ' 5 V'
            Value = 5.000000000000000000
            Scale = 1.000000000000000000
            Units = 'V'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
        end
        object LSCalGrp: TGroupBox
          Left = 7
          Top = 46
          Width = 250
          Height = 83
          Caption = ' Calibration '
          TabOrder = 1
          object Label12: TLabel
            Left = 59
            Top = 13
            Width = 64
            Height = 14
            Caption = 'Wavelength'
          end
          object Label13: TLabel
            Left = 146
            Top = 13
            Width = 41
            Height = 14
            Caption = 'Voltage'
          end
          object Label14: TLabel
            Left = 20
            Top = 29
            Width = 37
            Height = 14
            Alignment = taRightJustify
            Caption = 'Point 1'
          end
          object Label15: TLabel
            Left = 20
            Top = 55
            Width = 37
            Height = 14
            Alignment = taRightJustify
            Caption = 'Point 2'
          end
          object edWavelength1: TValidatedEdit
            Left = 59
            Top = 29
            Width = 70
            Height = 20
            Hint = 'Calibration wavelength #1'
            AutoSize = False
            ShowHint = True
            Text = ' 0 nm'
            Scale = 1.000000000000000000
            Units = 'nm'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
          object edVoltage1: TValidatedEdit
            Left = 148
            Top = 29
            Width = 69
            Height = 20
            Hint = 'Calibration voltage #1'
            AutoSize = False
            ShowHint = True
            Text = ' 0 V'
            Scale = 1.000000000000000000
            Units = 'V'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
          object edWavelength2: TValidatedEdit
            Left = 59
            Top = 55
            Width = 70
            Height = 20
            Hint = 'Calibration wavelength #2'
            AutoSize = False
            ShowHint = True
            Text = ' 0 nm'
            Scale = 1.000000000000000000
            Units = 'nm'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
          object edVoltage2: TValidatedEdit
            Left = 148
            Top = 55
            Width = 69
            Height = 20
            Hint = 'Calibration wavelength #2'
            AutoSize = False
            ShowHint = True
            Text = ' 0 V'
            Scale = 1.000000000000000000
            Units = 'V'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
        end
        object lsTIRFGrp: TGroupBox
          Left = 8
          Top = 96
          Width = 257
          Height = 113
          Caption = ' TIRF Angle Control Voltages '
          TabOrder = 4
          object GroupBox5: TGroupBox
            Left = 6
            Top = 16
            Width = 120
            Height = 89
            Caption = 'Laser 1 '
            TabOrder = 0
            object Label49: TLabel
              Left = 42
              Top = 16
              Width = 16
              Height = 14
              Alignment = taRightJustify
              Caption = 'Off'
            end
            object Label50: TLabel
              Left = 35
              Top = 38
              Width = 23
              Height = 14
              Alignment = taRightJustify
              Caption = 'TIRF'
            end
            object Label57: TLabel
              Left = 7
              Top = 61
              Width = 51
              Height = 14
              Alignment = taRightJustify
              Caption = 'Widefield'
            end
            object edTIRFOff1: TValidatedEdit
              Left = 60
              Top = 16
              Width = 54
              Height = 20
              Hint = 'Laser #1 control voltage for minimum intensity '
              AutoSize = False
              ShowHint = True
              Text = ' 0 V'
              Scale = 1.000000000000000000
              Units = 'V'
              NumberFormat = '%.4g'
              LoLimit = -10.000000000000000000
              HiLimit = 10.000000000000000000
            end
            object edTIRFOn1: TValidatedEdit
              Left = 60
              Top = 37
              Width = 54
              Height = 20
              Hint = 'Laser #1 control voltage for maximum intensity '
              AutoSize = False
              ShowHint = True
              Text = ' 0 V'
              Scale = 1.000000000000000000
              Units = 'V'
              NumberFormat = '%.4g'
              LoLimit = -10.000000000000000000
              HiLimit = 10.000000000000000000
            end
            object edTIRFWF1: TValidatedEdit
              Left = 60
              Top = 61
              Width = 54
              Height = 20
              Hint = 'Laser #1 on/off transition delay (ms)'
              AutoSize = False
              ShowHint = True
              Text = ' 0 V'
              Scale = 1.000000000000000000
              Units = 'V'
              NumberFormat = '%.4g'
              LoLimit = -10.000000000000000000
              HiLimit = 10.000000000000000000
            end
          end
          object GroupBox8: TGroupBox
            Left = 130
            Top = 16
            Width = 120
            Height = 89
            Caption = ' Laser 2 '
            TabOrder = 1
            object Label58: TLabel
              Left = 44
              Top = 16
              Width = 16
              Height = 14
              Alignment = taRightJustify
              Caption = 'Off'
            end
            object Label59: TLabel
              Left = 37
              Top = 38
              Width = 23
              Height = 14
              Alignment = taRightJustify
              Caption = 'TIRF'
            end
            object Label60: TLabel
              Left = 9
              Top = 61
              Width = 51
              Height = 14
              Alignment = taRightJustify
              Caption = 'Widefield'
            end
            object edTIRFOff2: TValidatedEdit
              Left = 62
              Top = 16
              Width = 54
              Height = 20
              Hint = 'Laser #1 control voltage for minimum intensity '
              AutoSize = False
              ShowHint = True
              Text = ' 0 V'
              Scale = 1.000000000000000000
              Units = 'V'
              NumberFormat = '%.4g'
              LoLimit = -10.000000000000000000
              HiLimit = 10.000000000000000000
            end
            object edTIRFOn2: TValidatedEdit
              Left = 62
              Top = 37
              Width = 54
              Height = 20
              Hint = 'Laser #1 control voltage for maximum intensity '
              AutoSize = False
              ShowHint = True
              Text = ' 0 V'
              Scale = 1.000000000000000000
              Units = 'V'
              NumberFormat = '%.4g'
              LoLimit = -10.000000000000000000
              HiLimit = 10.000000000000000000
            end
            object edTIRFWF2: TValidatedEdit
              Left = 62
              Top = 61
              Width = 54
              Height = 20
              Hint = 'Laser #1 on/off transition delay (ms)'
              AutoSize = False
              ShowHint = True
              Text = ' 0 V'
              Scale = 1.000000000000000000
              Units = 'V'
              NumberFormat = '%.4g'
              LoLimit = -10.000000000000000000
              HiLimit = 10.000000000000000000
            end
          end
        end
        object LSLaserGrp: TGroupBox
          Left = 15
          Top = 158
          Width = 250
          Height = 148
          Caption = ' LED/Lasers '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 5
          object LSLaserPage: TPageControl
            Left = 8
            Top = 16
            Width = 232
            Height = 121
            ActivePage = Laser1Tab
            TabOrder = 0
            object Laser1Tab: TTabSheet
              Caption = 'LED/Laser 1'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object Label7: TLabel
                Left = 9
                Top = 4
                Width = 64
                Height = 14
                Caption = 'Wavelength'
              end
              object Label26: TLabel
                Left = 9
                Top = 44
                Width = 72
                Height = 14
                Caption = 'Def. Intensity'
              end
              object edLaser1Wavelength: TValidatedEdit
                Left = 9
                Top = 18
                Width = 70
                Height = 20
                Hint = 'Laser #1 emission wavelength (nm)'
                AutoSize = False
                ShowHint = True
                Text = ' 0 nm'
                Scale = 1.000000000000000000
                Units = 'nm'
                NumberFormat = '%.4g'
                LoLimit = -1.000000015047466E29
                HiLimit = 1.000000015047466E29
              end
              object GroupBox2: TGroupBox
                Left = 96
                Top = 0
                Width = 121
                Height = 89
                Caption = ' Control Voltages '
                TabOrder = 1
                object Label24: TLabel
                  Left = 23
                  Top = 16
                  Width = 15
                  Height = 14
                  Alignment = taRightJustify
                  Caption = '0%'
                end
                object Label25: TLabel
                  Left = 11
                  Top = 38
                  Width = 27
                  Height = 14
                  Alignment = taRightJustify
                  Caption = '100%'
                end
                object Label8: TLabel
                  Left = 9
                  Top = 61
                  Width = 29
                  Height = 14
                  Alignment = taRightJustify
                  Caption = 'Delay'
                end
                object edLaser1OffVoltage: TValidatedEdit
                  Left = 40
                  Top = 16
                  Width = 60
                  Height = 20
                  Hint = 'Laser #1 control voltage for minimum intensity '
                  AutoSize = False
                  ShowHint = True
                  Text = ' 0 V'
                  Scale = 1.000000000000000000
                  Units = 'V'
                  NumberFormat = '%.3g'
                  LoLimit = -10.000000000000000000
                  HiLimit = 10.000000000000000000
                end
                object edLaser1OnVoltage: TValidatedEdit
                  Left = 40
                  Top = 37
                  Width = 60
                  Height = 20
                  Hint = 'Laser #1 control voltage for maximum intensity '
                  AutoSize = False
                  ShowHint = True
                  Text = ' 0 V'
                  Scale = 1.000000000000000000
                  Units = 'V'
                  NumberFormat = '%.3g'
                  LoLimit = -10.000000000000000000
                  HiLimit = 10.000000000000000000
                end
                object edLaser1Delay: TValidatedEdit
                  Left = 40
                  Top = 61
                  Width = 60
                  Height = 20
                  Hint = 'Laser #1 on/off transition delay (ms)'
                  AutoSize = False
                  ShowHint = True
                  Text = ' 0 ms'
                  Scale = 1000.000000000000000000
                  Units = 'ms'
                  NumberFormat = '%.3g'
                  LoLimit = -1.000000015047466E30
                  HiLimit = 1.000000015047466E29
                end
              end
              object edLaser1Intensity: TValidatedEdit
                Left = 9
                Top = 58
                Width = 70
                Height = 20
                Hint = 'Laser #1 emission wavelength (nm)'
                AutoSize = False
                ShowHint = True
                Text = ' 0 %'
                Scale = 1.000000000000000000
                Units = '%'
                NumberFormat = '%.4g'
                LoLimit = -1.000000015047466E30
                HiLimit = 100.000000000000000000
              end
            end
            object Laser2Tab: TTabSheet
              Caption = 'LED/Laser 2'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ImageIndex = 1
              ParentFont = False
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object Label9: TLabel
                Left = 9
                Top = 4
                Width = 64
                Height = 14
                Caption = 'Wavelength'
              end
              object Label22: TLabel
                Left = 9
                Top = 44
                Width = 72
                Height = 14
                Caption = 'Def. Intensity'
              end
              object edLaser2Wavelength: TValidatedEdit
                Left = 9
                Top = 18
                Width = 70
                Height = 20
                Hint = 'Laser #1 emission wavelength (nm)'
                AutoSize = False
                ShowHint = True
                Text = ' 0 nm'
                Scale = 1.000000000000000000
                Units = 'nm'
                NumberFormat = '%.4g'
                LoLimit = -1.000000015047466E29
                HiLimit = 1.000000015047466E29
              end
              object edLaser2Intensity: TValidatedEdit
                Left = 9
                Top = 58
                Width = 70
                Height = 20
                Hint = 'Laser #1 emission wavelength (nm)'
                AutoSize = False
                ShowHint = True
                Text = ' 0 %'
                Scale = 1.000000000000000000
                Units = '%'
                NumberFormat = '%.4g'
                LoLimit = -1.000000015047466E30
                HiLimit = 100.000000000000000000
              end
              object GroupBox10: TGroupBox
                Left = 96
                Top = 0
                Width = 121
                Height = 89
                Caption = ' Control Voltages '
                TabOrder = 2
                object Label27: TLabel
                  Left = 23
                  Top = 16
                  Width = 15
                  Height = 14
                  Alignment = taRightJustify
                  Caption = '0%'
                end
                object Label28: TLabel
                  Left = 11
                  Top = 38
                  Width = 27
                  Height = 14
                  Alignment = taRightJustify
                  Caption = '100%'
                end
                object Label29: TLabel
                  Left = 9
                  Top = 61
                  Width = 29
                  Height = 14
                  Alignment = taRightJustify
                  Caption = 'Delay'
                end
                object edlaser2OffVoltage: TValidatedEdit
                  Left = 40
                  Top = 16
                  Width = 60
                  Height = 20
                  Hint = 'Laser #1 control voltage for minimum intensity '
                  AutoSize = False
                  ShowHint = True
                  Text = ' 0 V'
                  Scale = 1.000000000000000000
                  Units = 'V'
                  NumberFormat = '%.3g'
                  LoLimit = -10.000000000000000000
                  HiLimit = 10.000000000000000000
                end
                object edLaser2OnVoltage: TValidatedEdit
                  Left = 40
                  Top = 37
                  Width = 60
                  Height = 20
                  Hint = 'Laser #1 control voltage for maximum intensity '
                  AutoSize = False
                  ShowHint = True
                  Text = ' 0 V'
                  Scale = 1.000000000000000000
                  Units = 'V'
                  NumberFormat = '%.3g'
                  LoLimit = -10.000000000000000000
                  HiLimit = 10.000000000000000000
                end
                object edLaser2Delay: TValidatedEdit
                  Left = 40
                  Top = 61
                  Width = 60
                  Height = 20
                  Hint = 'Laser #1 on/off transition delay (ms)'
                  AutoSize = False
                  ShowHint = True
                  Text = ' 0 ms'
                  Scale = 1000.000000000000000000
                  Units = 'ms'
                  NumberFormat = '%.3g'
                  LoLimit = -1.000000015047466E30
                  HiLimit = 1.000000015047466E29
                end
              end
            end
            object Laser3Tab: TTabSheet
              Caption = 'LED/Laser 3'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ImageIndex = 2
              ParentFont = False
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object Label30: TLabel
                Left = 9
                Top = 4
                Width = 64
                Height = 14
                Caption = 'Wavelength'
              end
              object Label31: TLabel
                Left = 9
                Top = 44
                Width = 72
                Height = 14
                Caption = 'Def. Intensity'
              end
              object edLaser3Wavelength: TValidatedEdit
                Left = 9
                Top = 18
                Width = 70
                Height = 20
                Hint = 'Laser #1 emission wavelength (nm)'
                AutoSize = False
                ShowHint = True
                Text = ' 0 nm'
                Scale = 1.000000000000000000
                Units = 'nm'
                NumberFormat = '%.4g'
                LoLimit = -1.000000015047466E29
                HiLimit = 1.000000015047466E29
              end
              object edLaser3Intensity: TValidatedEdit
                Left = 9
                Top = 58
                Width = 70
                Height = 20
                Hint = 'Laser #1 emission wavelength (nm)'
                AutoSize = False
                ShowHint = True
                Text = ' 0 %'
                Scale = 1.000000000000000000
                Units = '%'
                NumberFormat = '%.4g'
                LoLimit = -1.000000015047466E30
                HiLimit = 100.000000000000000000
              end
              object GroupBox12: TGroupBox
                Left = 96
                Top = 0
                Width = 121
                Height = 89
                Caption = ' Control Voltages '
                TabOrder = 2
                object Label32: TLabel
                  Left = 23
                  Top = 16
                  Width = 15
                  Height = 14
                  Alignment = taRightJustify
                  Caption = '0%'
                end
                object Label33: TLabel
                  Left = 11
                  Top = 38
                  Width = 27
                  Height = 14
                  Alignment = taRightJustify
                  Caption = '100%'
                end
                object Label34: TLabel
                  Left = 9
                  Top = 61
                  Width = 29
                  Height = 14
                  Alignment = taRightJustify
                  Caption = 'Delay'
                end
                object edLaser3OffVoltage: TValidatedEdit
                  Left = 40
                  Top = 16
                  Width = 60
                  Height = 20
                  Hint = 'Laser #1 control voltage for minimum intensity '
                  AutoSize = False
                  ShowHint = True
                  Text = ' 0 V'
                  Scale = 1.000000000000000000
                  Units = 'V'
                  NumberFormat = '%.3g'
                  LoLimit = -10.000000000000000000
                  HiLimit = 10.000000000000000000
                end
                object edLaser3OnVoltage: TValidatedEdit
                  Left = 40
                  Top = 37
                  Width = 60
                  Height = 20
                  Hint = 'Laser #1 control voltage for maximum intensity '
                  AutoSize = False
                  ShowHint = True
                  Text = ' 0 V'
                  Scale = 1.000000000000000000
                  Units = 'V'
                  NumberFormat = '%.3g'
                  LoLimit = -10.000000000000000000
                  HiLimit = 10.000000000000000000
                end
                object edLaser3Delay: TValidatedEdit
                  Left = 40
                  Top = 61
                  Width = 60
                  Height = 20
                  Hint = 'Laser #1 on/off transition delay (ms)'
                  AutoSize = False
                  ShowHint = True
                  Text = ' 0 ms'
                  Scale = 1000.000000000000000000
                  Units = 'ms'
                  NumberFormat = '%.3g'
                  LoLimit = -1.000000015047466E30
                  HiLimit = 1.000000015047466E29
                end
              end
            end
          end
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Stimulus Outputs'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox11: TGroupBox
        Left = 8
        Top = 0
        Width = 249
        Height = 90
        Caption = ' Voltage (D/A)  Outputs '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object Label3: TLabel
          Left = 86
          Top = 18
          Width = 38
          Height = 13
          Alignment = taRightJustify
          Caption = 'Vout 0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label17: TLabel
          Left = 86
          Top = 41
          Width = 38
          Height = 13
          Alignment = taRightJustify
          Caption = 'Vout 1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label47: TLabel
          Left = 86
          Top = 64
          Width = 38
          Height = 13
          Alignment = taRightJustify
          Caption = 'Vout 2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object cbVCommand0: TComboBox
          Left = 128
          Top = 14
          Width = 110
          Height = 22
          Hint = 'AO (analog output channel) for stimulus channel Vout 0'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object cbVCommand1: TComboBox
          Left = 128
          Top = 37
          Width = 110
          Height = 22
          Hint = 'AO (analog output channel) for stimulus channel Vout 1'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
        object cbVCommand2: TComboBox
          Left = 128
          Top = 60
          Width = 110
          Height = 22
          Hint = 'AO (analog output channel) for stimulus channel Vout 2'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
      end
      object GroupBox6: TGroupBox
        Left = 264
        Top = 0
        Width = 193
        Height = 90
        Caption = ' Digital  Outputs (Range) '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object Label4: TLabel
          Left = 42
          Top = 18
          Width = 28
          Height = 13
          Alignment = taRightJustify
          Caption = 'Start'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label6: TLabel
          Left = 47
          Top = 46
          Width = 23
          Height = 13
          Alignment = taRightJustify
          Caption = 'End'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object cbDigitalStimStart: TComboBox
          Left = 72
          Top = 18
          Width = 110
          Height = 22
          Hint = 'Lower limit of digital output channels (digital stimulus 0)'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object cbDigitalStimEnd: TComboBox
          Left = 72
          Top = 46
          Width = 110
          Height = 22
          Hint = 'Upper limit of digital stimulus output channels'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
        end
      end
      object PhotoStimGrp: TGroupBox
        Left = 8
        Top = 90
        Width = 249
        Height = 126
        Caption = ' Photo Stimulus'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        object PhotoStimPage: TPageControl
          Left = 5
          Top = 16
          Width = 239
          Height = 103
          ActivePage = MeterTab
          TabOrder = 0
          object GalvosTab: TTabSheet
            Caption = 'Galvos'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label55: TLabel
              Left = 26
              Top = 30
              Width = 84
              Height = 14
              Alignment = taRightJustify
              Caption = 'Y Galvo Control'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label56: TLabel
              Left = 26
              Top = 6
              Width = 84
              Height = 14
              Alignment = taRightJustify
              Caption = 'X Galvo Control'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbPhotoStimX: TComboBox
              Left = 115
              Top = 2
              Width = 113
              Height = 22
              Style = csDropDownList
              TabOrder = 0
            end
            object cbPhotoStimY: TComboBox
              Left = 115
              Top = 26
              Width = 113
              Height = 22
              Style = csDropDownList
              TabOrder = 1
            end
          end
          object AttenuatorsTab: TTabSheet
            Caption = 'Attenuators'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ImageIndex = 1
            ParentFont = False
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label51: TLabel
              Left = 11
              Top = 6
              Width = 98
              Height = 14
              Caption = 'Control Channel 1'
            end
            object Label45: TLabel
              Left = 11
              Top = 30
              Width = 98
              Height = 14
              Caption = 'Control Channel 2'
            end
            object Label46: TLabel
              Left = 11
              Top = 54
              Width = 98
              Height = 14
              Caption = 'Control Channel 3'
            end
            object cbPhotoStimIntensity1: TComboBox
              Left = 115
              Top = 2
              Width = 113
              Height = 22
              Style = csDropDownList
              TabOrder = 0
            end
            object cbPhotoStimIntensity2: TComboBox
              Left = 115
              Top = 26
              Width = 113
              Height = 22
              Style = csDropDownList
              TabOrder = 1
            end
            object cbPhotoStimIntensity3: TComboBox
              Left = 115
              Top = 50
              Width = 113
              Height = 22
              Style = csDropDownList
              TabOrder = 2
            end
          end
          object ShutterTab: TTabSheet
            Caption = 'Shutter'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ImageIndex = 2
            ParentFont = False
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label52: TLabel
              Left = 20
              Top = 6
              Width = 89
              Height = 14
              Alignment = taRightJustify
              Caption = 'Control Channel'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label53: TLabel
              Left = 22
              Top = 30
              Width = 87
              Height = 14
              Alignment = taRightJustify
              Caption = 'Shutter Latency'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label54: TLabel
              Left = 31
              Top = 53
              Width = 78
              Height = 14
              Alignment = taRightJustify
              Caption = 'Shutter Active'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbPhotoStimShutter: TComboBox
              Left = 115
              Top = 2
              Width = 113
              Height = 22
              Style = csDropDownList
              TabOrder = 0
            end
            object edPhotoStimShutterLatency: TValidatedEdit
              Left = 115
              Top = 26
              Width = 113
              Height = 20
              AutoSize = False
              ShowHint = True
              Text = ' 0 ms'
              Scale = 1000.000000000000000000
              Units = 'ms'
              NumberFormat = '%.3g'
              LoLimit = -1.000000015047466E30
              HiLimit = 1.000000015047466E29
            end
            object rbPhotoStimShutterActiveHigh: TRadioButton
              Left = 115
              Top = 52
              Width = 49
              Height = 17
              Caption = 'High'
              TabOrder = 2
            end
            object rbPhotoStimShutterActiveLow: TRadioButton
              Left = 180
              Top = 52
              Width = 45
              Height = 17
              Caption = 'Low'
              TabOrder = 3
            end
          end
          object MeterTab: TTabSheet
            Caption = 'Meter'
            ImageIndex = 3
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label44: TLabel
              Left = 44
              Top = 6
              Width = 64
              Height = 14
              Alignment = taRightJustify
              Caption = 'Meter Input'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object cbPhotoStimMeterInput: TComboBox
              Left = 115
              Top = 2
              Width = 113
              Height = 22
              Style = csDropDownList
              TabOrder = 0
            end
          end
        end
      end
    end
    object AnalogInputTab: TTabSheet
      Caption = 'Analog Inputs && Amplifiers'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ADCGrp: TGroupBox
        Left = 8
        Top = 0
        Width = 226
        Height = 129
        Caption = ' Analog Inputs '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object Label10: TLabel
          Left = 13
          Top = 16
          Width = 81
          Height = 14
          Alignment = taRightJustify
          Caption = 'Input channels'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label18: TLabel
          Left = 31
          Top = 42
          Width = 63
          Height = 14
          Alignment = taRightJustify
          Caption = 'Input mode'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label83: TLabel
          Left = 46
          Top = 96
          Width = 109
          Height = 14
          Alignment = taRightJustify
          Caption = 'No. Channels In Use'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label85: TLabel
          Left = 4
          Top = 69
          Width = 90
          Height = 21
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Voltage Range'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object cbADCIn: TComboBox
          Left = 97
          Top = 16
          Width = 120
          Height = 22
          Hint = 'Analogue input channel range'
          Style = csDropDownList
          Ctl3D = True
          ParentCtl3D = False
          TabOrder = 0
          OnChange = cbADCInChange
        end
        object cbADCInputMode: TComboBox
          Left = 97
          Top = 42
          Width = 120
          Height = 22
          Hint = 'Analogue input mode (differential, single-ended, etc.)'
          TabOrder = 1
          Text = 'cbADCInputMode'
        end
        object edNumChannels: TValidatedEdit
          Left = 161
          Top = 96
          Width = 56
          Height = 22
          Hint = 'No. of analogue input channels in use'
          OnKeyPress = edNumChannelsKeyPress
          AutoSize = False
          ShowHint = True
          Text = ' 0 '
          Scale = 1.000000000000000000
          NumberFormat = '%.0f'
          LoLimit = -1.000000015047466E29
          HiLimit = 8.000000000000000000
        end
        object cbADCVoltageRange: TComboBox
          Left = 97
          Top = 68
          Width = 120
          Height = 22
          Hint = 'Analogue input voltage range (+/-)'
          Style = csDropDownList
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = False
          TabOrder = 3
        end
      end
      object GroupBox7: TGroupBox
        Left = 8
        Top = 132
        Width = 226
        Height = 74
        Caption = ' Timing   '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object Label11: TLabel
          Left = 57
          Top = 18
          Width = 98
          Height = 13
          Alignment = taRightJustify
          Caption = 'Sampling interval'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label5: TLabel
          Left = 32
          Top = 44
          Width = 58
          Height = 13
          Alignment = taRightJustify
          Caption = 'Board link'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edADCInterval: TValidatedEdit
          Left = 161
          Top = 18
          Width = 56
          Height = 22
          Hint = 'Analogue output and input update and sampling interval (ms)'
          ShowHint = True
          Text = ' 0 ms'
          Scale = 1000.000000000000000000
          Units = 'ms'
          NumberFormat = '%.4g'
          LoLimit = -1.000000015047466E30
          HiLimit = 1.000000015047466E29
        end
        object cbClockSynchronisation: TComboBox
          Left = 97
          Top = 44
          Width = 120
          Height = 22
          Hint = 'Timing link between multiple interface boards'
          Style = csDropDownList
          DoubleBuffered = False
          ParentDoubleBuffered = False
          TabOrder = 1
        end
      end
      object GroupBox20: TGroupBox
        Left = 8
        Top = 211
        Width = 226
        Height = 103
        Caption = ' Amplifier #1'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        object Label84: TLabel
          Left = 64
          Top = 72
          Width = 107
          Height = 14
          Alignment = taRightJustify
          Caption = 'Vout 0 Divide Factor'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object cbAmplifier1: TComboBox
          Left = 8
          Top = 16
          Width = 210
          Height = 22
          Hint = '#1 Patch clamp amplifier in use'
          TabOrder = 0
          Text = 'cbAmplifier1'
        end
        object edVDivide0: TValidatedEdit
          Left = 177
          Top = 72
          Width = 41
          Height = 20
          Hint = 'Patch clamp command voltage divide factor'
          AutoSize = False
          ShowHint = True
          Text = ' 0 X'
          Scale = 1.000000000000000000
          Units = 'X'
          NumberFormat = '%.0f'
          LoLimit = -1.000000015047466E29
          HiLimit = 1.000000015047466E29
        end
        object GainTelPanel1: TPanel
          Left = 17
          Top = 44
          Width = 93
          Height = 25
          BevelOuter = bvNone
          Caption = 'l'
          TabOrder = 2
          object lbTelegraphChannel: TLabel
            Left = 18
            Top = 2
            Width = 46
            Height = 14
            Alignment = taRightJustify
            Caption = 'Gain Tel.'
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            WordWrap = True
          end
          object edGainTelegraphChannel1: TValidatedEdit
            Left = 66
            Top = 2
            Width = 22
            Height = 20
            Hint = 'Amplifier gain telegraph analogue input channel'
            AutoSize = False
            Text = ' 0 '
            Scale = 1.000000000000000000
            NumberFormat = '%.0f'
            LoLimit = -1.000000015047466E30
            HiLimit = 15.000000000000000000
          end
        end
        object ModeTelPanel1: TPanel
          Left = 130
          Top = 44
          Width = 93
          Height = 25
          BevelOuter = bvNone
          TabOrder = 3
          object Label86: TLabel
            Left = 11
            Top = 2
            Width = 53
            Height = 14
            Alignment = taRightJustify
            Caption = 'Mode Tel.'
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            WordWrap = True
          end
          object edModeTelegraphChannel1: TValidatedEdit
            Left = 66
            Top = 2
            Width = 22
            Height = 20
            Hint = 
              'Amplifier voltage/current-clamp mode telegraph analogue input ch' +
              'annel'
            AutoSize = False
            ShowHint = True
            Text = ' 0 '
            Scale = 1.000000000000000000
            NumberFormat = '%.0f'
            LoLimit = -1.000000000000000000
            HiLimit = 15.000000000000000000
          end
        end
      end
      object GroupBox21: TGroupBox
        Left = 8
        Top = 315
        Width = 226
        Height = 103
        Caption = ' Amplifier #2'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        object Label87: TLabel
          Left = 64
          Top = 72
          Width = 107
          Height = 14
          Alignment = taRightJustify
          Caption = 'Vout 1 Divide Factor'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object cbAmplifier2: TComboBox
          Left = 8
          Top = 16
          Width = 210
          Height = 22
          Hint = '#2 Patch clamp amplifier in use'
          Ctl3D = True
          ParentCtl3D = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'cbAmplifiers'
        end
        object edVDivide1: TValidatedEdit
          Left = 177
          Top = 72
          Width = 41
          Height = 20
          Hint = 'Patch clamp command voltage divide factor'
          AutoSize = False
          ShowHint = True
          Text = ' 0 X'
          Scale = 1.000000000000000000
          Units = 'X'
          NumberFormat = '%.0f'
          LoLimit = -1.000000015047466E29
          HiLimit = 1.000000015047466E29
        end
        object GainTelPanel2: TPanel
          Left = 17
          Top = 44
          Width = 93
          Height = 25
          BevelOuter = bvNone
          Caption = 'l'
          TabOrder = 2
          object Label88: TLabel
            Left = 14
            Top = 2
            Width = 46
            Height = 14
            Alignment = taRightJustify
            Caption = 'Gain Tel.'
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            WordWrap = True
          end
          object edGainTelegraphChannel2: TValidatedEdit
            Left = 66
            Top = 2
            Width = 22
            Height = 20
            Hint = 'Amplifier gain telegraph analogue input channel'
            AutoSize = False
            ShowHint = True
            Text = ' 0 '
            Scale = 1.000000000000000000
            NumberFormat = '%.0f'
            LoLimit = -1.000000015047466E30
            HiLimit = 15.000000000000000000
          end
        end
        object ModeTelPanel2: TPanel
          Left = 130
          Top = 44
          Width = 93
          Height = 25
          BevelOuter = bvNone
          TabOrder = 3
          object Label89: TLabel
            Left = 11
            Top = 2
            Width = 53
            Height = 14
            Alignment = taRightJustify
            Caption = 'Mode Tel.'
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            WordWrap = True
          end
          object edModeTelegraphChannel2: TValidatedEdit
            Left = 66
            Top = 2
            Width = 22
            Height = 20
            Hint = 
              'Amplifier voltage/current-clamp mode telegraph analogue input ch' +
              'annel'
            AutoSize = False
            ShowHint = True
            Text = ' 0 '
            Scale = 1.000000000000000000
            NumberFormat = '%.0f'
            LoLimit = -1.000000000000000000
            HiLimit = 15.000000000000000000
          end
        end
      end
      object ChannelsGrp: TGroupBox
        Left = 240
        Top = 0
        Width = 257
        Height = 418
        Caption = ' Channel calibration table '
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        object ChannelTable: TStringGrid
          Left = 13
          Top = 16
          Width = 241
          Height = 390
          Hint = 'Input channel scaling factors and calibration units'
          ColCount = 4
          DefaultColWidth = 58
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
          ColWidths = (
            58
            58
            58
            58)
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
      end
    end
    object ZStageTab: TTabSheet
      Caption = 'XY / Z Stage'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox13: TGroupBox
        Left = 3
        Top = 0
        Width = 458
        Height = 233
        Caption = ' Z Stage '
        TabOrder = 0
        object Label62: TLabel
          Left = 141
          Top = 16
          Width = 172
          Height = 14
          Alignment = taRightJustify
          Caption = ' Z Stage Control Voltage Output'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object cbZStageControl: TComboBox
          Left = 320
          Top = 16
          Width = 121
          Height = 22
          Hint = 'Analog output channel connected to Z stage control voltage input'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object GroupBox15: TGroupBox
          Left = 8
          Top = 40
          Width = 225
          Height = 82
          Caption = ' Calibration '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          object Label63: TLabel
            Left = 67
            Top = 13
            Width = 45
            Height = 14
            Caption = 'Position'
          end
          object Label64: TLabel
            Left = 144
            Top = 13
            Width = 41
            Height = 14
            Caption = 'Voltage'
          end
          object Label65: TLabel
            Left = 11
            Top = 29
            Width = 54
            Height = 14
            Alignment = taRightJustify
            Caption = 'Position 1'
          end
          object Label66: TLabel
            Left = 11
            Top = 55
            Width = 54
            Height = 14
            Alignment = taRightJustify
            Caption = 'Position 2'
          end
          object edZStagePos1: TValidatedEdit
            Left = 67
            Top = 29
            Width = 70
            Height = 20
            Hint = 'Calibration point #1: position'
            AutoSize = False
            ShowHint = True
            Text = ' 0 um'
            Scale = 1.000000000000000000
            Units = 'um'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
          object edZStageV1: TValidatedEdit
            Left = 144
            Top = 29
            Width = 69
            Height = 20
            Hint = 'Calibration point #1: voltage'
            AutoSize = False
            ShowHint = True
            Text = ' 0 V'
            Scale = 1.000000000000000000
            Units = 'V'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
          object edZstagePos2: TValidatedEdit
            Left = 67
            Top = 55
            Width = 70
            Height = 20
            Hint = 'Calibration point #2: position'
            AutoSize = False
            ShowHint = True
            Text = ' 0 um'
            Scale = 1.000000000000000000
            Units = 'um'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
          object edZStageV2: TValidatedEdit
            Left = 144
            Top = 55
            Width = 69
            Height = 20
            Hint = 'Calibration point #2: voltage'
            AutoSize = False
            ShowHint = True
            Text = ' 0 V'
            Scale = 1.000000000000000000
            Units = 'V'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
        end
        object GroupBox1: TGroupBox
          Left = 240
          Top = 40
          Width = 200
          Height = 82
          Caption = ' Voltage Range Limits '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          object Label69: TLabel
            Left = 69
            Top = 29
            Width = 52
            Height = 14
            Alignment = taRightJustify
            Caption = 'Minimum'
          end
          object Label70: TLabel
            Left = 67
            Top = 55
            Width = 54
            Height = 14
            Alignment = taRightJustify
            Caption = 'Maximum'
          end
          object edZStageVMin: TValidatedEdit
            Left = 124
            Top = 29
            Width = 69
            Height = 20
            Hint = 'Lower limit of Z stage voltage range'
            AutoSize = False
            ShowHint = True
            Text = ' -10 V'
            Value = -10.000000000000000000
            Scale = 1.000000000000000000
            Units = 'V'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
          object edZStageVMax: TValidatedEdit
            Left = 124
            Top = 55
            Width = 69
            Height = 20
            Hint = 'Upper limit of Z stage voltage range'
            AutoSize = False
            ShowHint = True
            Text = ' 10 V'
            Value = 10.000000000000000000
            Scale = 1.000000000000000000
            Units = 'V'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
        end
        object GroupBox16: TGroupBox
          Left = 239
          Top = 128
          Width = 200
          Height = 89
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
          object Label71: TLabel
            Left = 29
            Top = 14
            Width = 84
            Height = 14
            Alignment = taRightJustify
            Caption = 'Max. Step Time'
          end
          object edZStageStepTime: TValidatedEdit
            Left = 120
            Top = 14
            Width = 70
            Height = 20
            Hint = 'Maximum time taken to move to new position'
            AutoSize = False
            ShowHint = True
            Text = ' 0 ms'
            Scale = 1000.000000000000000000
            Units = 'ms'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E29
          end
          object ckZStepExcitationOffDuringStep: TCheckBox
            Left = 32
            Top = 40
            Width = 158
            Height = 17
            Hint = 'Turn excitation light off during change in Z position'
            Alignment = taLeftJustify
            Caption = 'Excitation off during step'
            DoubleBuffered = False
            ParentDoubleBuffered = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
          end
          object ckZStepEndExposureAtStep: TCheckBox
            Left = 48
            Top = 55
            Width = 142
            Height = 25
            Hint = 'Terminate exposure when Z position starts to change'
            Alignment = taLeftJustify
            Caption = 'End exposure at step'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
          end
        end
        object ckZStageEnabled: TCheckBox
          Left = 8
          Top = 16
          Width = 73
          Height = 17
          Hint = 'Enable operation of Z stage'
          Caption = 'Enabled'
          DoubleBuffered = False
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentDoubleBuffered = False
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
        end
        object GroupBox17: TGroupBox
          Left = 8
          Top = 128
          Width = 225
          Height = 65
          TabOrder = 5
          object Label67: TLabel
            Left = 60
            Top = 16
            Width = 77
            Height = 14
            Alignment = taRightJustify
            Caption = 'Min. Step Size'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object edZStageMinStepSize: TValidatedEdit
            Left = 144
            Top = 16
            Width = 70
            Height = 20
            Hint = 'Z stage precision'
            AutoSize = False
            ShowHint = True
            Text = ' 0.05 um'
            Value = 0.050000000745058060
            Scale = 1.000000000000000000
            Units = 'um'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E29
            HiLimit = 1.000000015047466E29
          end
        end
      end
      object XYStateGrp: TGroupBox
        Left = 3
        Top = 238
        Width = 458
        Height = 155
        Caption = ' XY Stage '
        TabOrder = 1
        object Label2: TLabel
          Left = 186
          Top = 20
          Width = 80
          Height = 14
          Alignment = taRightJustify
          Caption = ' XY Stage Type'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object cbXYStageType: TComboBox
          Left = 272
          Top = 20
          Width = 177
          Height = 22
          Hint = 'Analog output channel connected to Z stage control voltage input'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object GroupBox19: TGroupBox
          Left = 167
          Top = 44
          Width = 280
          Height = 101
          Caption = ' Motors '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 1
          object Label21: TLabel
            Left = 14
            Top = 16
            Width = 124
            Height = 14
            Caption = 'X Motor Serial Number'
          end
          object Label38: TLabel
            Left = 14
            Top = 44
            Width = 124
            Height = 14
            Caption = 'Y Motor Serial Number'
          end
          object edXMotorSerialNumber: TEdit
            Left = 144
            Top = 16
            Width = 105
            Height = 22
            TabOrder = 0
          end
          object edYMotorSerialNumber: TEdit
            Left = 144
            Top = 44
            Width = 105
            Height = 22
            TabOrder = 1
          end
        end
        object LimitsGrp: TGroupBox
          Left = 8
          Top = 16
          Width = 153
          Height = 129
          Caption = ' Limits '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          object Label39: TLabel
            Left = 40
            Top = 16
            Width = 34
            Height = 14
            Alignment = taRightJustify
            Caption = 'X min.'
          end
          object Label80: TLabel
            Left = 39
            Top = 40
            Width = 36
            Height = 14
            Alignment = taRightJustify
            Caption = 'X max.'
          end
          object Label81: TLabel
            Left = 40
            Top = 64
            Width = 34
            Height = 14
            Alignment = taRightJustify
            Caption = 'Y min.'
          end
          object Label82: TLabel
            Left = 41
            Top = 88
            Width = 33
            Height = 14
            Alignment = taRightJustify
            Caption = 'Y max'
          end
          object edXYStageXMin: TValidatedEdit
            Left = 81
            Top = 16
            Width = 65
            Height = 20
            Hint = 'Lower limit of Z stage voltage range'
            AutoSize = False
            ShowHint = True
            Text = ' 0 mm'
            Scale = 1.000000000000000000
            Units = 'mm'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E29
          end
          object edXYStageXMax: TValidatedEdit
            Left = 81
            Top = 40
            Width = 65
            Height = 20
            Hint = 'Lower limit of Z stage voltage range'
            AutoSize = False
            ShowHint = True
            Text = ' 100 mm'
            Value = 100.000000000000000000
            Scale = 1.000000000000000000
            Units = 'mm'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E29
          end
          object edXYStageYMin: TValidatedEdit
            Left = 81
            Top = 64
            Width = 65
            Height = 20
            Hint = 'Lower limit of Z stage voltage range'
            AutoSize = False
            ShowHint = True
            Text = ' 0 mm'
            Scale = 1.000000000000000000
            Units = 'mm'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E29
          end
          object edXYStageYMax: TValidatedEdit
            Left = 81
            Top = 88
            Width = 65
            Height = 20
            Hint = 'Lower limit of Z stage voltage range'
            AutoSize = False
            ShowHint = True
            Text = ' 75 mm'
            Value = 75.000000000000000000
            Scale = 1.000000000000000000
            Units = 'mm'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E29
          end
        end
      end
    end
    object CapacityTab: TTabSheet
      Caption = 'Computed Channels'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object CapGrp: TGroupBox
        Left = 3
        Top = 0
        Width = 185
        Height = 401
        Caption = ' Cell Capacity '
        TabOrder = 0
        object GroupBox22: TGroupBox
          Left = 8
          Top = 112
          Width = 169
          Height = 89
          Caption = ' Capacity Compensation '
          TabOrder = 0
          object Label90: TLabel
            Left = 7
            Top = 37
            Width = 65
            Height = 15
            Caption = 'Series Res.'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object Label91: TLabel
            Left = 7
            Top = 61
            Width = 72
            Height = 15
            Caption = 'Cell Capacity'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object edCapRSeriesComp: TValidatedEdit
            Left = 96
            Top = 37
            Width = 67
            Height = 20
            AutoSize = False
            Text = ' 0 MOhm'
            Scale = 0.000000999999997475
            Units = 'MOhm'
            NumberFormat = '%.5g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
          object edCapCellCapacityComp: TValidatedEdit
            Left = 96
            Top = 61
            Width = 67
            Height = 20
            AutoSize = False
            Text = ' 0 pF'
            Scale = 999999995904.000000000000000000
            Units = 'pF'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
          object ckCapacityCompensationInUse: TCheckBox
            Left = 8
            Top = 16
            Width = 145
            Height = 17
            BiDiMode = bdLeftToRight
            Caption = 'Use compensation '
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentBiDiMode = False
            ParentFont = False
            TabOrder = 2
          end
        end
        object GroupBox23: TGroupBox
          Left = 8
          Top = 40
          Width = 169
          Height = 65
          TabOrder = 1
          object Label92: TLabel
            Left = 7
            Top = 13
            Width = 79
            Height = 15
            Caption = 'Excitation freq.'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object Label93: TLabel
            Left = 7
            Top = 37
            Width = 73
            Height = 15
            Caption = 'Reversal Pot.'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object edCapFrequency: TValidatedEdit
            Left = 96
            Top = 13
            Width = 67
            Height = 20
            AutoSize = False
            Text = ' 10 Hz'
            Value = 10.000000000000000000
            Scale = 1.000000000000000000
            Units = 'Hz'
            NumberFormat = '%.5g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
          object edCapVRev: TValidatedEdit
            Left = 96
            Top = 37
            Width = 67
            Height = 20
            AutoSize = False
            Text = ' 0 mV'
            Scale = 1000.000000000000000000
            Units = 'mV'
            NumberFormat = '%.4g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
        end
        object GroupBox24: TGroupBox
          Left = 8
          Top = 208
          Width = 169
          Height = 97
          Caption = ' Display ranges '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          object Label94: TLabel
            Left = 67
            Top = 72
            Width = 23
            Height = 15
            Alignment = taRightJustify
            Caption = 'C.m'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object Label95: TLabel
            Left = 67
            Top = 24
            Width = 23
            Height = 15
            Alignment = taRightJustify
            Caption = 'G.m'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object Label96: TLabel
            Left = 71
            Top = 48
            Width = 19
            Height = 15
            Alignment = taRightJustify
            Caption = 'G.s'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object edCapCmDisplayMax: TValidatedEdit
            Left = 96
            Top = 69
            Width = 67
            Height = 20
            AutoSize = False
            Text = ' 10 pF'
            Value = 10.000000000000000000
            Scale = 1.000000000000000000
            Units = 'pF'
            NumberFormat = '%.5g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
          object edCapGmDisplayMax: TValidatedEdit
            Left = 96
            Top = 21
            Width = 67
            Height = 20
            AutoSize = False
            Text = ' 10 nS'
            Value = 10.000000000000000000
            Scale = 1.000000000000000000
            Units = 'nS'
            NumberFormat = '%.5g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
          object edCapGsDisplayMax: TValidatedEdit
            Left = 96
            Top = 45
            Width = 67
            Height = 20
            AutoSize = False
            Text = ' 10 nS'
            Value = 10.000000000000000000
            Scale = 1.000000000000000000
            Units = 'nS'
            NumberFormat = '%.5g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
        end
        object ckCapEnabled: TCheckBox
          Left = 8
          Top = 17
          Width = 169
          Height = 17
          Hint = 'Enable analogue capacity calculation'
          Caption = 'Enable'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = ckCapEnabledClick
        end
      end
      object RatioGrp: TGroupBox
        Left = 205
        Top = 0
        Width = 228
        Height = 401
        Caption = ' PMT Fluorescence Ratio '
        TabOrder = 1
        object GroupBox26: TGroupBox
          Left = 8
          Top = 34
          Width = 214
          Height = 79
          Caption = ' Input channels '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object Label97: TLabel
            Left = 151
            Top = 18
            Width = 48
            Height = 14
            Alignment = taRightJustify
            Caption = 'Threshold'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object Label98: TLabel
            Left = 8
            Top = 36
            Width = 25
            Height = 19
            Caption = 'R ='
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Shape1: TShape
            Left = 40
            Top = 44
            Width = 105
            Height = 1
          end
          object edThreshold: TValidatedEdit
            Left = 151
            Top = 34
            Width = 57
            Height = 20
            Hint = 'Minimum signal threhold'
            AutoSize = False
            ShowHint = True
            Text = ' 10 '
            Value = 10.000000000000000000
            Scale = 1.000000000000000000
            NumberFormat = '%.3g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
          object cbNumerChan: TComboBox
            Left = 40
            Top = 18
            Width = 105
            Height = 23
            Hint = 'Input channel used as numerator of ratio R'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            Text = 'cbNumerChan'
            OnChange = cbNumerChanChange
          end
          object cbDenomChan: TComboBox
            Left = 39
            Top = 47
            Width = 105
            Height = 23
            Hint = 'Input channel used as denominator of ratio R'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            Text = 'cbNumerator'
          end
        end
        object ConcResultsGrp: TGroupBox
          Left = 8
          Top = 125
          Width = 214
          Height = 66
          Caption = ' Ratio Result Channel'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          object Label99: TLabel
            Left = 133
            Top = 20
            Width = 69
            Height = 14
            Alignment = taRightJustify
            Caption = 'Display Range'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object edRatioMax: TValidatedEdit
            Left = 133
            Top = 36
            Width = 73
            Height = 20
            Hint = 'Ratio channel display range (+/-)'
            AutoSize = False
            ShowHint = True
            Text = ' 10 '
            Value = 10.000000000000000000
            Scale = 1.000000000000000000
            NumberFormat = '%.3g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
          object cbRatioChan: TComboBox
            Left = 7
            Top = 20
            Width = 120
            Height = 23
            Hint = 'Data channel to contain ratio R.'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            Text = 'cbNumerator'
          end
        end
        object GroupBox27: TGroupBox
          Left = 8
          Top = 197
          Width = 214
          Height = 185
          Caption = ' Ion Concentration Result Channel '
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          object Label100: TLabel
            Left = 127
            Top = 20
            Width = 69
            Height = 14
            Alignment = taRightJustify
            Caption = 'Display Range'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object Label104: TLabel
            Left = 17
            Top = 76
            Width = 17
            Height = 15
            Alignment = taRightJustify
            Caption = 'Ion'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object Label105: TLabel
            Left = 103
            Top = 76
            Width = 29
            Height = 15
            Alignment = taRightJustify
            Caption = 'Units'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object Label101: TLabel
            Left = 89
            Top = 105
            Width = 33
            Height = 15
            Alignment = taRightJustify
            Caption = 'R.Max'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object Label102: TLabel
            Left = 91
            Top = 130
            Width = 31
            Height = 15
            Alignment = taRightJustify
            Caption = 'R.Min'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object Label103: TLabel
            Left = 91
            Top = 155
            Width = 25
            Height = 15
            Alignment = taRightJustify
            Caption = 'K.Eff'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object edConcMax: TValidatedEdit
            Left = 127
            Top = 36
            Width = 73
            Height = 20
            Hint = 'Computed ion concentration channel display range (+/-)'
            AutoSize = False
            ShowHint = True
            Text = ' 10 '
            Value = 10.000000000000000000
            Scale = 1.000000000000000000
            NumberFormat = '%.3g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
          object cbConcChan: TComboBox
            Left = 8
            Top = 20
            Width = 113
            Height = 23
            Hint = 'Data channel to contain computed ion concentration '
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            Text = 'cbNumerator'
          end
          object ckConcEnabled: TCheckBox
            Left = 8
            Top = 45
            Width = 102
            Height = 21
            Hint = 'Enable calculation of ion concentration channel'
            Caption = 'Enable'
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
            TabOrder = 2
          end
          object edIonName: TEdit
            Left = 40
            Top = 76
            Width = 49
            Height = 23
            Hint = 'Name of ion being monitored'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
            Text = 'Ca'
          end
          object edConcUnits: TEdit
            Left = 138
            Top = 76
            Width = 33
            Height = 23
            Hint = 'Concentration units (uM,uM,M)'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
            Text = 'uM'
          end
          object edRMax: TValidatedEdit
            Left = 128
            Top = 104
            Width = 73
            Height = 20
            Hint = 'maximum ratio at saturating concentration of ion '
            AutoSize = False
            ShowHint = True
            Text = ' 1 '
            Value = 1.000000000000000000
            Scale = 1.000000000000000000
            NumberFormat = '%.3g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
          object edRMin: TValidatedEdit
            Left = 128
            Top = 130
            Width = 73
            Height = 20
            Hint = 'Minimum ratio at zero concentration of ion.'
            AutoSize = False
            ShowHint = True
            Text = ' 0 '
            Scale = 1.000000000000000000
            NumberFormat = '%.3g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
          object edKeff: TValidatedEdit
            Left = 128
            Top = 155
            Width = 73
            Height = 20
            Hint = 'K.Eff Ion concentration at which 50% are bound to fluorphore '
            AutoSize = False
            ShowHint = True
            Text = ' 1 '
            Value = 1.000000000000000000
            Scale = 1.000000000000000000
            NumberFormat = '%.3g'
            LoLimit = -1.000000015047466E30
            HiLimit = 1.000000015047466E30
          end
        end
        object ckPMTRatioEnabled: TCheckBox
          Left = 8
          Top = 16
          Width = 169
          Height = 17
          Caption = 'Enable'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 3
          OnClick = ckPMTRatioEnabledClick
        end
      end
    end
  end
end
