unit SetupUnit;
// ------------------------------------------
// WinFluor - Camera/Lab.Interface Setup Form
// ------------------------------------------
// (c) J. Dempster, University of Strathclyde 2002, All Rights Reserved
// 29.7.03 TIDRFile component added
// 1/8/03 .FrameInterval can now be edited
// 8/10/03 A/D input, ion binding & diagnostics moved to separate dialog forms
// 11/1/07 User-entered Laser calibration settings added for OptoScan + Lasers
// 31.01.07 Readout speed now set to maximum when camera selected
// 26.02.07 Third laser added to Optoscan + Lasers
// 16.01.08 LED Off/Max voltages added
// 25.01.08 Support for an auxiliary camera for use when main camera is an LSM added
// 21.01.09 User-set extra camera readout time box added
//          Shutter closed wavelength for monochromators added
//          Camer trigger offset now set to default value when light source selected
// 23.01.09 Nicholas Schwarz's modifications added
// 09.03.09 JD .. Separate laser control output range added to light source configuration
// 13.03.09 JD .. Memory violation which ocurred when switching to NIDAQ-MX with no NIDAQ-MX Dll
//                fixed. Card list now also cleared
// 04.10.09 NS .. Added third DAC channel, Vout2
// 01.02.11 JD Post-exposure readout check box added. Exposure time in ext. trigger mode
//             can now be shortened to account for post-exposure readout in camera which do not support
//             overlap readout mode.
// 30.04.12 JD Clocked digital outputs of X series devices (634X,635X,636X) now detected
//             Reset Boards no longer results in repeated digital stimulus output lists
// 03.04.13 JD Z stage control DAC channel can now be set to none.
// 31.05.13 JD Placement of light source settings panel tidied up
// 03.06.13 JD LED/laser 2 & 3 settings now only visible if enough DAC control lines selected
// 27.09.13 JD ShutterChangeTime field added to shutter group
// 25.10.13 JD Z stage step time added
// 16.12.13 JD CheckSettings added to check input and output channels for missing and multiple channels
// 27.02.13 JD Emission filter control settings added to Light Source page
// 22.07.14 JD Camera selection menu added (allows selection of camera when more than one available)
// 20.08.14 JD Default readout speed set when camera type changed now obtained from Cam1.DefaultReadoutSpeed
// 20.01.15 JD MainFrm.IOConfig.LSLaserStart/MainFrm.IOConfig.LSLaserEnd now set to None when
//             laser not required
// 23.01.15 JD DarkLevelLo and  DarkLeveHi added
// 14.5.15  JD Disable Exposure Interval Limit check box added
// 03.9.15  JD edADCInterval.LoLimit now set to LabIO.DACMinUpdateInterval
// 16.09.15 JD Form position/size saved by MainFrm.SaveFormPosition() when form closed
// 25.09.15 JD Analog Channels & Amplifiers setup transferred from setupadcfrm to
//             Analog Channels & Amplifiers page of this form. Capacity page added
// 20.11.15 JD Software-upated digital outputs now available for use a light source outputs
//             (but with timing precision limitations)

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Grids, ValEdit, ValidatedEdit, IDRFile, maths, math, system.StrUtils ;
const
     MaxADCChannel = 7 ;
type


  TSetupFrm = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    CardGrp: TGroupBox;
    rbNIDAQ: TRadioButton;
    rbNIDAQMX: TRadioButton;
    meDeviceList: TMemo;
    bResetDevices: TButton;
    ckAutoReset: TCheckBox;
    TabPage: TPageControl;
    CameraTab: TTabSheet;
    GroupBox4: TGroupBox;
    Label41: TLabel;
    cbCamera: TComboBox;
    ModePanel: TPanel;
    lbCameraMode: TLabel;
    cbCameraMode: TComboBox;
    ComPanel: TPanel;
    lbComPort: TLabel;
    cbCameraPort: TComboBox;
    ReadoutSpeedPanel: TPanel;
    Label1: TLabel;
    cbReadoutSpeed: TComboBox;
    edTemperatureSetPoint: TValidatedEdit;
    ADCPanel: TPanel;
    Label48: TLabel;
    cbCameraADC: TComboBox;
    AuxCameraPanel: TPanel;
    Label37: TLabel;
    cbAuxCamera: TComboBox;
    LightSourceTab: TTabSheet;
    LightSourceGrp: TGroupBox;
    cbLightSource: TComboBox;
    LSCalGrp: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    edWavelength1: TValidatedEdit;
    edVoltage1: TValidatedEdit;
    edWavelength2: TValidatedEdit;
    edVoltage2: TValidatedEdit;
    LSWaveGrp: TGroupBox;
    LSLEDGrp: TGroupBox;
    Label35: TLabel;
    Label36: TLabel;
    edLEDOffVoltage: TValidatedEdit;
    edLEDMaxVoltage: TValidatedEdit;
    lsTIRFGrp: TGroupBox;
    GroupBox5: TGroupBox;
    Label49: TLabel;
    Label50: TLabel;
    Label57: TLabel;
    edTIRFOff1: TValidatedEdit;
    edTIRFOn1: TValidatedEdit;
    edTIRFWF1: TValidatedEdit;
    GroupBox8: TGroupBox;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    edTIRFOff2: TValidatedEdit;
    edTIRFOn2: TValidatedEdit;
    edTIRFWF2: TValidatedEdit;
    TabSheet1: TTabSheet;
    AnalogInputTab: TTabSheet;
    ADCGrp: TGroupBox;
    Label10: TLabel;
    Label18: TLabel;
    cbADCIn: TComboBox;
    cbADCInputMode: TComboBox;
    ZStageTab: TTabSheet;
    GroupBox14: TGroupBox;
    Label19: TLabel;
    Label20: TLabel;
    Label23: TLabel;
    Label61: TLabel;
    edLensMagnification: TValidatedEdit;
    edPixelWidth: TValidatedEdit;
    edCalibrationBarSize: TValidatedEdit;
    edCalibrationBarThickness: TValidatedEdit;
    GroupBox7: TGroupBox;
    Label11: TLabel;
    Label5: TLabel;
    edADCInterval: TValidatedEdit;
    cbClockSynchronisation: TComboBox;
    GroupBox13: TGroupBox;
    cbZStageControl: TComboBox;
    Label62: TLabel;
    GroupBox15: TGroupBox;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    edZStagePos1: TValidatedEdit;
    edZStageV1: TValidatedEdit;
    edZstagePos2: TValidatedEdit;
    edZStageV2: TValidatedEdit;
    GroupBox9: TGroupBox;
    Label16: TLabel;
    Label42: TLabel;
    cbCameraStart: TComboBox;
    rbCameraStartActiveHigh: TRadioButton;
    rbCameraStartActiveLow: TRadioButton;
    edCameraTriggerOffset: TValidatedEdit;
    edCameraReadoutTime: TValidatedEdit;
    ckCCDClearPreExposure: TCheckBox;
    ckPostExposureReadout: TCheckBox;
    LSShutterGrp: TGroupBox;
    Label43: TLabel;
    Label40: TLabel;
    edShutterClosedWavelength: TValidatedEdit;
    edShutterBlankingPeriod: TValidatedEdit;
    GroupBox3: TGroupBox;
    cbLSShutter: TComboBox;
    rbLSShutterActiveHigh: TRadioButton;
    rbLSShutterActiveLow: TRadioButton;
    GroupBox11: TGroupBox;
    Label3: TLabel;
    Label17: TLabel;
    Label47: TLabel;
    cbVCommand0: TComboBox;
    cbVCommand1: TComboBox;
    cbVCommand2: TComboBox;
    GroupBox6: TGroupBox;
    Label4: TLabel;
    Label6: TLabel;
    cbDigitalStimStart: TComboBox;
    cbDigitalStimEnd: TComboBox;
    PhotoStimGrp: TGroupBox;
    PhotoStimPage: TPageControl;
    GalvosTab: TTabSheet;
    Label55: TLabel;
    Label56: TLabel;
    cbPhotoStimX: TComboBox;
    cbPhotoStimY: TComboBox;
    AttenuatorsTab: TTabSheet;
    Label51: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    cbPhotoStimIntensity1: TComboBox;
    cbPhotoStimIntensity2: TComboBox;
    cbPhotoStimIntensity3: TComboBox;
    ShutterTab: TTabSheet;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    cbPhotoStimShutter: TComboBox;
    edPhotoStimShutterLatency: TValidatedEdit;
    rbPhotoStimShutterActiveHigh: TRadioButton;
    rbPhotoStimShutterActiveLow: TRadioButton;
    MeterTab: TTabSheet;
    Label44: TLabel;
    cbPhotoStimMeterInput: TComboBox;
    GroupBox1: TGroupBox;
    Label69: TLabel;
    Label70: TLabel;
    edZStageVMin: TValidatedEdit;
    edZStageVMax: TValidatedEdit;
    GroupBox16: TGroupBox;
    ckZStageEnabled: TCheckBox;
    Label68: TLabel;
    edShutterChangeTime: TValidatedEdit;
    Label71: TLabel;
    edZStageStepTime: TValidatedEdit;
    GroupBox17: TGroupBox;
    Label67: TLabel;
    edZStageMinStepSize: TValidatedEdit;
    ckZStepExcitationOffDuringStep: TCheckBox;
    ckZStepEndExposureAtStep: TCheckBox;
    EmFilterGrp: TGroupBox;
    EmFilterControlGrp: TGroupBox;
    Label72: TLabel;
    cbEMFilterStart: TComboBox;
    Label73: TLabel;
    cbEmFilterEnd: TComboBox;
    Label74: TLabel;
    edEmFilterChangeTime: TValidatedEdit;
    SplitImageGrp: TGroupBox;
    edSplitImageUpper: TEdit;
    Label75: TLabel;
    Label76: TLabel;
    edSplitImageLower: TEdit;
    ckBulbExposureMode: TCheckBox;
    CameraPanel: TPanel;
    cbCameraNames: TComboBox;
    Label77: TLabel;
    GroupBox18: TGroupBox;
    edDarkLevelLo: TValidatedEdit;
    Label78: TLabel;
    Label79: TLabel;
    edDarkLevelHi: TValidatedEdit;
    LSControl0: TPanel;
    cbLSControl0: TComboBox;
    lbLSControl0: TLabel;
    lsControl1: TPanel;
    lbLSControl1: TLabel;
    cbLSControl1: TComboBox;
    lsControl2: TPanel;
    lbLSControl2: TLabel;
    cbLSControl2: TComboBox;
    lsControl3: TPanel;
    lbLSControl3: TLabel;
    cbLSControl03: TComboBox;
    lsControl4: TPanel;
    lbLSControl4: TLabel;
    cbLSControl4: TComboBox;
    lsControl5: TPanel;
    lbLSControl5: TLabel;
    cbLSControl05: TComboBox;
    lsControl6: TPanel;
    lbLSControl6: TLabel;
    cbLSControl06: TComboBox;
    lsControl7: TPanel;
    lbLSControl7: TLabel;
    cbLSControl7: TComboBox;
    LSLaserGrp: TGroupBox;
    LSLaserPage: TPageControl;
    Laser1Tab: TTabSheet;
    Label7: TLabel;
    Label26: TLabel;
    edLaser1Wavelength: TValidatedEdit;
    GroupBox2: TGroupBox;
    Label24: TLabel;
    Label25: TLabel;
    Label8: TLabel;
    edLaser1OffVoltage: TValidatedEdit;
    edLaser1OnVoltage: TValidatedEdit;
    edLaser1Delay: TValidatedEdit;
    edLaser1Intensity: TValidatedEdit;
    Laser2Tab: TTabSheet;
    Label9: TLabel;
    Label22: TLabel;
    edLaser2Wavelength: TValidatedEdit;
    edLaser2Intensity: TValidatedEdit;
    GroupBox10: TGroupBox;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    edlaser2OffVoltage: TValidatedEdit;
    edLaser2OnVoltage: TValidatedEdit;
    edLaser2Delay: TValidatedEdit;
    Laser3Tab: TTabSheet;
    Label30: TLabel;
    Label31: TLabel;
    edLaser3Wavelength: TValidatedEdit;
    edLaser3Intensity: TValidatedEdit;
    GroupBox12: TGroupBox;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    edLaser3OffVoltage: TValidatedEdit;
    edLaser3OnVoltage: TValidatedEdit;
    edLaser3Delay: TValidatedEdit;
    XYStateGrp: TGroupBox;
    Label2: TLabel;
    cbXYStageType: TComboBox;
    GroupBox19: TGroupBox;
    edXMotorSerialNumber: TEdit;
    Label21: TLabel;
    Label38: TLabel;
    edYMotorSerialNumber: TEdit;
    LimitsGrp: TGroupBox;
    edXYStageXMin: TValidatedEdit;
    Label39: TLabel;
    Label80: TLabel;
    edXYStageXMax: TValidatedEdit;
    Label81: TLabel;
    edXYStageYMin: TValidatedEdit;
    Label82: TLabel;
    edXYStageYMax: TValidatedEdit;
    ckDisableExposureIntervalLimit: TCheckBox;
    Label83: TLabel;
    edNumChannels: TValidatedEdit;
    Label85: TLabel;
    cbADCVoltageRange: TComboBox;
    GroupBox20: TGroupBox;
    Label84: TLabel;
    cbAmplifier1: TComboBox;
    edVDivide0: TValidatedEdit;
    GainTelPanel1: TPanel;
    lbTelegraphChannel: TLabel;
    edGainTelegraphChannel1: TValidatedEdit;
    ModeTelPanel1: TPanel;
    Label86: TLabel;
    edModeTelegraphChannel1: TValidatedEdit;
    GroupBox21: TGroupBox;
    Label87: TLabel;
    cbAmplifier2: TComboBox;
    edVDivide1: TValidatedEdit;
    GainTelPanel2: TPanel;
    Label88: TLabel;
    edGainTelegraphChannel2: TValidatedEdit;
    ModeTelPanel2: TPanel;
    Label89: TLabel;
    edModeTelegraphChannel2: TValidatedEdit;
    ChannelsGrp: TGroupBox;
    ChannelTable: TStringGrid;
    CapacityTab: TTabSheet;
    CapGrp: TGroupBox;
    GroupBox22: TGroupBox;
    Label90: TLabel;
    Label91: TLabel;
    edCapRSeriesComp: TValidatedEdit;
    edCapCellCapacityComp: TValidatedEdit;
    ckCapacityCompensationInUse: TCheckBox;
    GroupBox23: TGroupBox;
    Label92: TLabel;
    Label93: TLabel;
    edCapFrequency: TValidatedEdit;
    edCapVRev: TValidatedEdit;
    GroupBox24: TGroupBox;
    Label94: TLabel;
    Label95: TLabel;
    Label96: TLabel;
    edCapCmDisplayMax: TValidatedEdit;
    edCapGmDisplayMax: TValidatedEdit;
    edCapGsDisplayMax: TValidatedEdit;
    ckCapEnabled: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure cbCameraChange(Sender: TObject);
    procedure cbCameraPortChange(Sender: TObject);
    procedure cbLightSourceChange(Sender: TObject);
    procedure rbNIDAQClick(Sender: TObject);
    procedure rbNIDAQMXClick(Sender: TObject);
    procedure edLensMagnificationKeyPress(Sender: TObject; var Key: Char);
    procedure bResetDevicesClick(Sender: TObject);
    procedure cbCameraModeChange(Sender: TObject);
    procedure cbCameraADCChange(Sender: TObject);
    procedure cbCameraNamesChange(Sender: TObject);
    procedure cbLSControl0Change(Sender: TObject);
    procedure cbADCInChange(Sender: TObject);
    procedure edNumChannelsKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    CameraOpenRequired : Boolean ;
    procedure NewCamera ;
    procedure NewInterfaceCards ;
    procedure DisplayLightSourceSettingsPanels ;
    procedure ShowLaserSettings ;
    procedure CheckSettings ;
    procedure DisplayLSControl(
              Panel : TPanel ;
              LineName : string ;
              Index : Integer
              ) ;
   Function GetLSControlSetting(
            Panel : TPanel ) : Integer ;
   procedure GetAllLSControlSetting ;
   procedure UpdateChannelEditTable ;
   procedure GetChannelsFromEditTable ;
   procedure UpdateAmplifierSettings ;
   procedure GetADCVoltageRangeList ;
  public
    { Public declarations }
  end;

var
  SetupFrm: TSetupFrm;

implementation

uses Main, shared, LabIOUnit, TimeCourseUnit, AmpModule , LightSourceUnit,
  RecUnit, SnapUnit, RecADCOnlyUnit, Sealtest, LogUnit, ZStageUnit, XYStageUnit;

const
     // Channel calibration table column definitions
     ChNum = 0 ;
     ChName = 1 ;
     ChCal = 2 ;
     ChUnits = 3 ;

{$R *.DFM}

procedure TSetupFrm.FormShow(Sender: TObject);
// ---------------------------------------------
// Initial control settings when form is opened
// ---------------------------------------------
begin

     // Set NI API type
     if LabIO.NIDAQAPI = NIDAQ then rbNIDAQ.Checked := True
                               else rbNIDAQMX.Checked := True ;

     // Set up controls for new interface cards
     NewInterfaceCards ;

     CameraOpenRequired :=  False ;
     MainFrm.mnCameraSetup.Enabled := False ;

     // Close any form which might be acquiring images
     if MainFrm.FormExists('RecordFrm') then RecordFrm.Close ;
     if MainFrm.FormExists('SnapFrm') then SnapFrm.Close ;
     if MainFrm.FormExists('RecADCOnlyFrm') then RecADCOnlyFrm.Close ;
     if MainFrm.FormExists('SealTestFrm') then SealTestFrm.Close ;
     if XYStageFrm.Visible then XYStageFrm.Close ;

     // Get camera library list
     MainFrm.Cam1.GetCameraLibList( cbCamera.Items ) ;
     cbCamera.ItemIndex := MainFrm.CameraType ;

     // Get names of available cameras
     MainFrm.Cam1.GetCameraNameList( cbCameraNames.Items ) ;
     cbCameraNames.ItemIndex := MainFrm.Cam1.SelectedCamera ;

     // Set auxiliary camera
     MainFrm.Cam1.GetCameraNameList( cbAuxCamera.Items ) ;
     cbAuxCamera.ItemIndex := MainFrm.AuxCameraType ;

     // Get camera COM port
     cbCameraPort.ItemIndex := MainFrm.Cam1.ComPort - 1 ;

     // Reset camera
     NewCamera ;

     // Get CCD pre-exposure clear setting
     ckCCDClearPreExposure.Checked := MainFrm.Cam1.CCDClearPreExposure ;

     // Get CCD post-exposure readout setting
     ckPostExposureReadout.Checked := MainFrm.Cam1.CCDPostExposureReadout ;

     // Get bulb exposure mode (exposure trigger pulse duration determines camera exposure time)
     ckBulbExposureMode.Checked := MainFrm.BulbExposureMode ;

     // Hardware in use

     edCalibrationBarSize.Value := MainFrm.CalibrationBarSize ;
     edCalibrationBarThickness.Value := MainFrm.CalibrationBarThickness ;

     // Get list of light sources
     LightSource.GetList( cbLightSource.Items ) ;
     cbLightSource.ItemIndex := cbLightSource.Items.IndexOfObject(TObject(LightSource.DeviceType)) ;

     // Display light source settings panels
     DisplayLightSourceSettingsPanels ;

     // Set wavelength calibration values (for user-calibrated monochromators)
     edWavelength1.Value := LightSource.Wavelength1 ;
     edVoltage1.Value := LightSource.Voltage1 ;
     edWavelength2.Value := LightSource.Wavelength2 ;
     edVoltage2.Value := LightSource.Voltage2 ;

     // Laser calibration settings
     edLaser1Wavelength.Value := LightSource.LaserWavelength[0] ;
     edLaser1Delay.Value := LightSource.LaserDelay[0] ;
     edLaser1OffVoltage.Value := LightSource.LaserOffVoltage[0] ;
     edLaser1OnVoltage.Value := LightSource.LaserOnVoltage[0] ;
     edLaser1Intensity.Value := LightSource.LaserIntensity[0] ;

     edLaser2Wavelength.Value := LightSource.LaserWavelength[1] ;
     edLaser2Delay.Value := LightSource.LaserDelay[1] ;
     edLaser2OffVoltage.Value := LightSource.LaserOffVoltage[1] ;
     edLaser2OnVoltage.Value := LightSource.LaserOnVoltage[1] ;
     edLaser2Intensity.Value := LightSource.LaserIntensity[1] ;

     edLaser3Wavelength.Value := LightSource.LaserWavelength[2] ;
     edLaser3Delay.Value := LightSource.LaserDelay[2] ;
     edLaser3OffVoltage.Value := LightSource.LaserOffVoltage[2] ;
     edLaser3OnVoltage.Value := LightSource.LaserOnVoltage[2] ;
     edLaser3Intensity.Value := LightSource.LaserIntensity[2] ;

     ShowLaserSettings ;

     // LED control settings
     edLEDOffVoltage.Value := LightSource.LEDOffVoltage ;
     edLEDMaxVoltage.Value := LightSource.LEDMaxVoltage ;

     // TIRF control settings
     edTIRFOff1.Value := LightSource.TIRFOff[1] ;
     edTIRFOn1.Value := LightSource.TIRFOn[1] ;
     edTIRFWF1.Value := LightSource.TIRFWF[1] ;
     edTIRFOff2.Value := LightSource.TIRFOff[2] ;
     edTIRFOn2.Value := LightSource.TIRFOn[2] ;
     edTIRFWF2.Value := LightSource.TIRFWF[2] ;

     edShutterClosedWavelength.Value := LightSource.ShutterClosedWavelength ;
     edShutterBlankingPeriod.Value := LightSource.ShutterBlankingPeriod ;
     edShutterChangeTime.Value := LightSource.ShutterChangeTime ;

     edEmFilterChangeTime.Value := LightSource.EmFilterChangeTime ;

     edADCInterval.Value := MainFrm.ADCScanInterval ;

     // Calibration settings
     edLensMagnification.Value := MainFrm.Cam1.LensMagnification ;
     edPixelWidth.Value := MainFrm.Cam1.PixelWidth ;
     edPixelWidth.Units := MainFrm.Cam1.PixelUnits ;
     edCalibrationBarSize.Units := MainFrm.Cam1.PixelUnits ;
     edCalibrationBarSize.Value := MainFrm.CalibrationBarSize ;
     edCalibrationBarThickness.Value := MainFrm.CalibrationBarThickness ;

     edADCInterval.LoLimit := LabIO.DACMinUpdateInterval ;

     edTemperatureSetPoint.Value := MainFrm.Cam1.CameraTemperatureSetPoint ;
     ckDisableExposureIntervalLimit.Checked := MainFrm.Cam1.DisableExposureIntervalLimit ;

     // XY stage
     XYStageFrm.GetList ( cbXYStageType.Items ) ;
     cbXYStageType.ItemIndex := cbXYStageType.Items.IndexOfObject(TObject(XYStageFrm.StageType)) ;
     edXMotorSerialNumber.Text := format('%d',[XYStageFrm.XMotorID]) ;
     edYMotorSerialNumber.Text := format('%d',[XYStageFrm.YMotorID]) ;
     edXYStageXMin.Value := XYStageFrm.XMin ;
     edXYStageXMax.Value := XYStageFrm.XMax ;
     edXYStageYMin.Value := XYStageFrm.YMin ;
     edXYStageYMax.Value := XYStageFrm.YMax ;

     // Z Stage
     ckZStageEnabled.Checked := ZStage.Available ;
     edZStagePos1.Value := ZStage.CalPosition1 ;
     edZStagePos2.Value := ZStage.CalPosition2 ;
     edZStageV1.Value := ZStage.CalVoltage1 ;
     edZStageV2.Value := ZStage.CalVoltage2 ;
     edZStageVMin.Value := ZStage.VMin ;
     edZStageVMax.Value := ZStage.VMax ;
     edZStageMinStepSize.Value := ZStage.MinStepSize ;
     edZStageStepTime.Value := ZStage.StepTime ;
     ckZStepExcitationOffDuringStep.Checked := ZStage.ExcitationOffDuringStep ;
     ckZStepEndExposureAtStep.Checked := ZStage.EndExposureAtStep ;

     edSplitImageUpper.Text := MainFrm.SplitImageName[0] ;
     edSplitImageLower.Text := MainFrm.SplitImageName[1] ;

     // Camera dark level range (used in time lapse mode to detect images containing only dark level values)
     edDarkLevelLo.Value := MainFrm.DarkLevelLo ;
     edDarkLevelHi.Value := MainFrm.DarkLevelHi ;

     // Analogue input settings
     edNumChannels.Value := MainFrm.ADCNumChannels ;
     edADCInterval.Value := MainFrm.ADCScanInterval ;

     // Analog & Patch clamp page

     // Get available A/D input voltage ranges and add to list
     GetADCVoltageRangeList ;

     // Setup Amplifier #1 telegraph options
     Amplifier.GetList( cbAmplifier1.Items ) ;
     cbAmplifier1.ItemIndex := cbAmplifier1.Items.IndexofObject(TObject(Amplifier.AmplifierType[1])) ;
     Amplifier.GetList( cbAmplifier2.Items ) ;
     cbAmplifier2.ItemIndex := cbAmplifier2.Items.IndexofObject(TObject(Amplifier.AmplifierType[2])) ;

     edGainTelegraphChannel1.Value := Amplifier.GainTelegraphChannel[1] ;
     edModeTelegraphChannel1.Value := Amplifier.ModeTelegraphChannel[1] ;
     edGainTelegraphChannel2.Value := Amplifier.GainTelegraphChannel[2] ;
     edModeTelegraphChannel2.Value := Amplifier.ModeTelegraphChannel[2] ;

     // Command voltage divide factor
     edVDivide0.Value := MainFrm.VCommand[0].DivideFactor ;
     edVDivide1.Value := MainFrm.VCommand[1].DivideFactor ;

     // Get channel settings
     UpdateAmplifierSettings ;
     UpdateChannelEditTable ;

     // Capacity page

     ckCapEnabled.Checked := MainFrm.Cap.Enabled ;
     edCapFrequency.Value := MainFrm.Cap.Frequency ;
     edCapVRev.Value :=  MainFrm.Cap.VRev ;
     ckCapacityCompensationInUse.Checked := MainFrm.Cap.CompensationInUse ;
     edCapRSeriesComp.Value :=  MainFrm.Cap.RSeriesComp ;
     edCapCellCapacityComp.Value := MainFrm.Cap.CellCapacityComp ;
     edCapGmDisplayMax.Value :=  MainFrm.Cap.GmDisplayMax ;
     edCapGsDisplayMax.Value :=  MainFrm.Cap.GsDisplayMax ;
     edCapCmDisplayMax.Value :=  MainFrm.Cap.CmDisplayMax ;

     ClientWidth := TabPage.Left + TabPage.Width + 5 ;
     ClientHeight := bOk.Top + bOk.Height + 5 ;

     // Auto reset interface cards
     ckAutoReset.Checked := MainFrm.AutoResetInterfaceCards ;

     end ;

procedure TSetupFrm.GetADCVoltageRangeList ;
// ---------------------------------------------------------
// Get available A/D input voltage ranges and add to list
// ---------------------------------------------------------
var
    i,Device : Integer ;
begin
     { Set up A/D Converter voltage range selection box }
     // Get A/D converter device
     if (MainFrm.IOConfig.ADCIn >= 0) and
        (MainFrm.IOConfig.ADCIn <= MaxResources) then
        Device := LabIO.Resource[MainFrm.IOConfig.ADCIn].Device
     else Device := 0 ;

     cbADCVoltageRange.clear ;
     if Device > 0 then begin
        // Get ranges supported by interface
        for i := 0 to LabIO.NumADCVoltageRanges[Device]-1 do begin
            cbADCVoltageRange.items.add(
            format(' +/- %.3g V ',[LabIO.ADCVoltageRanges[Device,i]] )) ;
            if Abs((MainFrm.ADCVoltageRange/LabIO.ADCVoltageRanges[Device,i])-1.0)
               < 1E-2 then cbADCVoltageRange.ItemIndex := i ;
            if Abs((MainFrm.ADCVoltageRange/LabIO.ADCVoltageRanges[Device,i])-1.0)
               < 1E-2 then cbADCVoltageRange.ItemIndex := i ;
            end ;
        end
     else begin
        cbADCVoltageRange.items.add(
        format(' +/- %.3g V ',[MainFrm.ADCVoltageRange] )) ;
        cbADCVoltageRange.ItemIndex := 0 ;
        end ;
     end ;


procedure TSetupFrm.DisplayLightSourceSettingsPanels ;
// ------------------------------------------------------------
// Display appropriate settings panels for current light source
// ------------------------------------------------------------
var
    iTop,iLeft : Integer ;
begin

     LSCalGrp.Visible := LightSource.UsercalibrationRequired ;
//     LSLaserGrp.Visible := LightSource.LaserSettingsRequired ;
     LSLEDGrp.Visible := LightSource.LEDSettingsRequired ;
     LSTIRFGrp.Visible := LightSource.TIRFSettingsRequired ;

     iTop := cbLightSource.Top + cbLightSource.Height + 5 ;
     iLeft := cbLightSource.Left ;

     if LSCalGrp.Visible then begin
        LSCalGrp.Top := iTop ;
        LSCalGrp.Left := iLeft ;
        iTop := iTop + LSCalGrp.Height + 2 ;
        end ;
     if LSLEDGrp.Visible then begin
        LSLEDGrp.Top := iTop ;
        LSLEDGrp.Left := iLeft ;
        iTop := iTop + LSLEDGrp.Height + 2 ;
        end ;
     if LSTIRFGrp.Visible then begin
        LSTIRFGrp.Top := iTop ;
        LSTIRFGrp.Left := iLeft ;
        iTop := iTop + LSTIRFGrp.Height + 2 ;
        end ;

     LSWaveGrp.Top := iTop ;
     LSWaveGrp.Left := iLeft ;
     iTop := iTop + LSWaveGrp.Height + 2 ;

     if LSLaserGrp.Visible then begin
        LSLaserGrp.Top := iTop ;
        LSLaserGrp.Left := iLeft ;
        iTop := iTop + LSLaserGrp.Height + 2 ;
        end ;
     end ;


procedure TSetupFrm.NewInterfaceCards ;
// ---------------------------------------------
// Setup up dialog box for new interface cards
// ---------------------------------------------
var
     i :Integer ;
     s : string ;
     Device :Integer ;
begin

     // List available interface cards
     meDeviceList.Clear ;
     for Device := 1 to LabIO.NumDevices do begin
         s := format('Device %d: %s, %d ADC channels, %d DAC channels',
                     [Device,
                      LabIO.DeviceBoardName[Device],
                      LabIO.NumADCs[Device],
                      LabIO.NumDACs[Device]] ) ;
         meDeviceList.Lines.Add(s) ;
         end ;

     // Analogue input channels
     cbADCIn.Clear ;
     cbADCIn.Items.AddObject('None',TObject(MaxResources+1)) ;
     for i := 0 to LabIO.NumResources-1 do
         if LabIO.Resource[i].ResourceType = ADCIn then begin
         s := format('Device %d: AI%d-%d',
              [LabIO.Resource[i].Device,
               LabIO.Resource[i].StartChannel,
               LabIO.Resource[i].EndChannel]) ;
         cbADCIn.Items.AddObject(s,TObject(i))
         end ;
     cbADCIn.ItemIndex := Max( 0,
     cbADCIn.Items.IndexOfObject(TObject(MainFrm.IOConfig.ADCIn))) ;

     // A/D channel input mode
     LABIO.GetADCInputModes( cbADCInputMode.Items ) ;
     cbADCInputMode.ItemIndex := Min(LABIO.ADCInputMode,cbADCInputMode.Items.Count-1) ;

     // Get of A/D input voltage ranges
     GetADCVoltageRangeList ;

     // Camera start trigger line options
     // ---------------------------------

     cbCameraStart.Clear ;
     cbCameraStart.Items.AddObject('Internal',TObject(MaxResources+1)) ;
     for i := 0 to LabIO.NumResources-1 do begin
         if LabIO.Resource[i].ResourceType = DACOut then begin
            // DAC outputs
            s := format('Device %d: AO%d',
                 [LabIO.Resource[i].Device,
                  LabIO.Resource[i].StartChannel]) ;
            cbCameraStart.Items.AddObject(s,TObject(i))
            end
         else if (LabIO.Resource[i].ResourceType = DIGOut) and
                 LabIO.DigitalWaveFormCapable[LabIO.Resource[i].Device] then begin
            // Digital outputs
            s := format('Device %d: P0.%d',
                 [LabIO.Resource[i].Device,
                  LabIO.Resource[i].StartChannel]) ;
                cbCameraStart.Items.AddObject(s,TObject(i))
            end ;
         end ;

     cbCameraStart.ItemIndex := Max( 0,
     cbCameraStart.Items.IndexOfObject(TObject(MainFrm.IOConfig.CameraStart))) ;
     rbCameraStartActiveHigh.Checked := MainFrm.IOConfig.CameraStartActiveHigh ;
     rbCameraStartActiveLow.Checked := not rbCameraStartActiveHigh.Checked ;

     // Patch clamp command voltage
     // Light source wavelength control
     // Digital stimulus output

     // Voltage stimulus O/P lines 1 & 2

     cbVCommand0.Clear ;
     cbVCommand0.Items.AddObject('None',TObject(MaxResources+1)) ;
     for i := 0 to LabIO.NumResources-1 do
         if LabIO.Resource[i].ResourceType = DACOut then begin
         s := format('Device %d: AO%d',
              [LabIO.Resource[i].Device,
               LabIO.Resource[i].StartChannel]) ;
         cbVCommand0.Items.AddObject(s,TObject(i))
         end ;
    cbVCommand0.ItemIndex := Max( 0,
    cbVCommand0.Items.IndexOfObject(TObject(MainFrm.IOConfig.VCommand[0]))) ;

    cbVCommand1.Items.Assign(cbVCommand0.Items) ;
    cbVCommand1.ItemIndex := Max( 0,
    cbVCommand1.Items.IndexOfObject(TObject(MainFrm.IOConfig.VCommand[1]))) ;

    cbVCommand2.Items.Assign(cbVCommand0.Items) ;
    cbVCommand2.ItemIndex := Max( 0,
    cbVCommand2.Items.IndexOfObject(TObject(MainFrm.IOConfig.VCommand[2]))) ;

    // Light source control lines
    // --------------------------

    DisplayLSControl( lsControl0, LightSource.ControlLineName(0), MainFrm.IOConfig.LSControlLine[0]  ) ;
    DisplayLSControl( lsControl1, LightSource.ControlLineName(1), MainFrm.IOConfig.LSControlLine[1] ) ;
    DisplayLSControl( lsControl2, LightSource.ControlLineName(2),MainFrm.IOConfig.LSControlLine[2] ) ;
    DisplayLSControl( lsControl3, LightSource.ControlLineName(3),MainFrm.IOConfig.LSControlLine[3] ) ;
    DisplayLSControl( lsControl4, LightSource.ControlLineName(4),MainFrm.IOConfig.LSControlLine[4] ) ;
    DisplayLSControl( lsControl5, LightSource.ControlLineName(5),MainFrm.IOConfig.LSControlLine[5] ) ;
    DisplayLSControl( lsControl6, LightSource.ControlLineName(6),MainFrm.IOConfig.LSControlLine[6] ) ;
    DisplayLSControl( lsControl7, LightSource.ControlLineName(7),MainFrm.IOConfig.LSControlLine[7] ) ;

    // Emission filter control lines
    // -----------------------------

     cbEMFilterStart.Clear ;
     cbEMFilterStart.Items.AddObject('None',TObject(MaxResources+1)) ;
     for i := 0 to LabIO.NumResources-1 do begin
         if (LabIO.Resource[i].ResourceType = DIGOut) then begin
                 // Digital outputs
                 s := format('Device %d: P0.%d',
                     [LabIO.Resource[i].Device,
                      LabIO.Resource[i].StartChannel]) ;
                cbEMFilterStart.Items.AddObject(s,TObject(i))
            end ;
         end ;
    cbEMFilterEnd.Items.Assign(cbEMFilterStart.Items) ;

    cbEMFilterStart.ItemIndex := Max( 0,
        cbEMFilterStart.Items.IndexOfObject(TObject(MainFrm.IOConfig.EmFilterStart))) ;
    cbEMFilterEnd.ItemIndex := Max( 0,
        cbEMFilterEnd.Items.IndexOfObject(TObject(MainFrm.IOConfig.EmFilterEnd))) ;

    // Digital stimulus output channels (both digital and DAC)
    cbDigitalStimStart.Clear ;
    cbDigitalStimStart.Items.AddObject('None',TObject(MaxResources+1)) ;
    // Add digital outputs (if supported)
    for i := 0 to LabIO.NumResources-1 do
         if (LabIO.Resource[i].ResourceType = DIGOut) and
            LabIO.DigitalWaveFormCapable[LabIO.Resource[i].Device] then begin
         s := format('Device %d: P0.%d',
              [LabIO.Resource[i].Device,
               LabIO.Resource[i].StartChannel]) ;
         cbDigitalStimStart.Items.AddObject(s,TObject(i))
         end ;
    // Add D/A outputs
    for i := 0 to LabIO.NumResources-1 do
         if LabIO.Resource[i].ResourceType = DACOut then begin
            // DAC outputs
            s := format('Device %d: AO%d',
                 [LabIO.Resource[i].Device,
                  LabIO.Resource[i].StartChannel]) ;
            cbDigitalStimStart.Items.AddObject(s,TObject(i))
            end ;

    cbDigitalStimEnd.Items.Assign( cbDigitalStimStart.Items) ;

    // Digital stimulus output (start line)
    cbDigitalStimStart.ItemIndex := Max( 0,
    cbDigitalStimStart.Items.IndexOfObject(TObject(MainFrm.IOConfig.DigitalStimStart))) ;

    // Digital stimulus output (start line)
    cbDigitalStimEnd.ItemIndex := Max( 0,
    cbDigitalStimEnd.Items.IndexOfObject(TObject(MainFrm.IOConfig.DigitalStimEnd))) ;


     // Photo-stimulus control lines

     cbPhotoStimX.Clear ;
     cbPhotoStimX.Items.AddObject('None',TObject(MaxResources+1)) ;
     for i := 0 to LabIO.NumResources-1 do
         if LabIO.Resource[i].ResourceType = DACOut then begin
         s := format('Device %d: AO%d',
              [LabIO.Resource[i].Device,
               LabIO.Resource[i].StartChannel]) ;
         cbPhotoStimX.Items.AddObject(s,TObject(i))
         end ;
    cbPhotoStimX.ItemIndex := Max( 0,
                              cbPhotoStimX.Items.IndexOfObject(TObject(MainFrm.IOConfig.PhotoStimX))) ;

    cbPhotoStimY.Items.Assign(cbPhotoStimX.Items) ;
    cbPhotoStimY.ItemIndex := Max( 0,
                              cbPhotoStimY.Items.IndexOfObject(TObject(MainFrm.IOConfig.PhotoStimY))) ;

    cbPhotoStimIntensity1.Items.Assign(cbPhotoStimX.Items) ;
    cbPhotoStimIntensity1.ItemIndex := Max( 0,
                                      cbPhotoStimIntensity1.Items.IndexOfObject(TObject(MainFrm.IOConfig.PhotoStimI1))) ;

    cbPhotoStimIntensity2.Items.Assign(cbPhotoStimX.Items) ;
    cbPhotoStimIntensity2.ItemIndex := Max( 0,
                                      cbPhotoStimIntensity2.Items.IndexOfObject(TObject(MainFrm.IOConfig.PhotoStimI2))) ;

    cbPhotoStimIntensity3.Items.Assign(cbPhotoStimX.Items) ;
    cbPhotoStimIntensity3.ItemIndex := Max( 0,
                                      cbPhotoStimIntensity3.Items.IndexOfObject(TObject(MainFrm.IOConfig.PhotoStimI3))) ;


     // Analogue input channels for power meter
     cbPhotoStimMeterInput.Clear ;
     cbPhotoStimMeterInput.Items.AddObject('None',TObject(MaxResources+1)) ;
     for i := 0 to LabIO.NumResources-1 do
         if LabIO.Resource[i].ResourceType = ADCIn then begin
         s := format('Device %d: ADC%d',
              [LabIO.Resource[i].Device,
               LabIO.Resource[i].StartChannel]) ;
         cbPhotoStimMeterInput.Items.AddObject(s,TObject(i))
         end ;
     cbPhotoStimMeterInput.ItemIndex := Max( 0,
                                        cbPhotoStimMeterInput.Items.IndexOfObject(TObject(MainFrm.IOConfig.PhotoStimMeter))) ;

    // Photo-stimulus source shutter
    // Modified by NS 17 September 2008
    // Modified by NS 22 September 2008
    // Modified by NS 23 September 2008
    cbPhotoStimShutter.Clear ;
    cbPhotoStimShutter.Items.Assign(cbPhotoStimX.Items) ;
    cbPhotoStimShutter.ItemIndex := Max( 0,
                                    cbPhotoStimShutter.Items.IndexOfObject(TObject(MainFrm.IOConfig.PhotoStimShutter))) ;
    rbPhotoStimShutterActiveHigh.Checked := MainFrm.IOConfig.PhotoStimShutterActiveHigh ;
    rbPhotoStimShutterActiveLow.Checked := not rbPhotoStimShutterActiveHigh.Checked ;
    edPhotoStimShutterLatency.Value := MainFrm.IOConfig.PhotoStimShutterLatency ;

     // Light source shutter
     cbLSShutter.Clear ;
     cbLSShutter.Items.AddObject('None',TObject(MaxResources+1)) ;
     for i := 0 to LabIO.NumResources-1 do
         if (LabIO.Resource[i].ResourceType = DIGOut) then begin
         s := format('Device %d: P0.%d',
              [LabIO.Resource[i].Device,
               LabIO.Resource[i].StartChannel]) ;
         cbLSShutter.Items.AddObject(s,TObject(i))
         end ;
     cbLSShutter.ItemIndex := Max( 0,
     cbLSShutter.Items.IndexOfObject(TObject(MainFrm.IOConfig.LSShutter))) ;
     rbLSShutterActiveHigh.Checked := MainFrm.IOConfig.LSShutterActiveHigh ;
     rbLSShutterActiveLow.Checked := not rbLSShutterActiveHigh.Checked ;

     // Clock synchronisation line
     cbClockSynchronisation.Clear ;
     cbClockSynchronisation.Items.AddObject('RTSI 0',TObject(0)) ;
     cbClockSynchronisation.Items.AddObject('PFI 5',TObject(1)) ;
     cbClockSynchronisation.ItemIndex :=  Max( 0,MainFrm.IOConfig.ClockSyncLine ) ;

     // Z stage control

     cbZStageControl.Clear ;
     cbZStageControl.Items.AddObject('None',TObject(MaxResources+1)) ;
     for i := 0 to LabIO.NumResources-1 do
         if LabIO.Resource[i].ResourceType = DACOut then begin
         s := format('Device %d: AO%d',
              [LabIO.Resource[i].Device,
               LabIO.Resource[i].StartChannel]) ;
         cbZStageControl.Items.AddObject(s,TObject(i)) ;
         end ;
    cbZStageControl.ItemIndex := Max( 0,
        cbZStageControl.Items.IndexOfObject(TObject(MainFrm.IOConfig.ZStageControl))) ;

     end ;

procedure TSetupFrm.DisplayLSControl(
          Panel : TPanel ;
          LineName : string ;
          Index : Integer
          ) ;
// ---------------------------------------
// Display light source control line panel
// ---------------------------------------
var
    ComboBox : TComboBox ;
    Lab : TLabel ;
    i : Integer ;
    s : string ;
begin

    // If line has no name hide panel and exit
    if LineName = '' then begin
       Panel.Visible := False ;
       Exit ;
       end
    else Panel.Visible := True ;

    // Local components
    Lab := Nil ;
    ComboBox := Nil ;
    for i  := 0 to Panel.ControlCount-1 do begin
        if ANSIContainsText(Panel.Controls[i].Name,'cbLS') then ComboBox := TComboBox(Panel.Controls[i])
        else if ANSIContainsText(Panel.Controls[i].Name,'lbLS') then Lab := TLabel(Panel.Controls[i]) ;
        end;

    if (Lab = Nil) and (ComboBox = Nil) then Exit ;

    Lab.Caption := LineName ;

    ComboBox.Clear ;
    ComboBox.Items.AddObject('None',TObject(MaxResources+1)) ;
    for i := 0 to LabIO.NumResources-1 do begin
        if LabIO.Resource[i].ResourceType = DACOut then begin
            // DAC outputs
            s := format('Device %d: AO%d',
                 [LabIO.Resource[i].Device,
                  LabIO.Resource[i].StartChannel]) ;
            ComboBox.Items.AddObject(s,TObject(i))
            end
         else if (LabIO.Resource[i].ResourceType = DIGOut) and
                 (not LightSource.DACOutputsRequired) then begin
                 // Digital outputs
                 s := format('Device %d: P0.%d',
                     [LabIO.Resource[i].Device,
                      LabIO.Resource[i].StartChannel]) ;
                ComboBox.Items.AddObject(s,TObject(i))
            end ;
         end ;

    ComboBox.ItemIndex := Max( 0, ComboBox.Items.IndexOfObject(TObject(Index))) ;

    lsWaveGrp.Height := Panel.Top + Panel.Height + 10 ;


    end;


Function TSetupFrm.GetLSControlSetting(
         Panel : TPanel ) : Integer ;
// --------------------------------------------
// Return settings of light source control line
// --------------------------------------------
var
    ComboBox : TComboBox ;
    i : Integer ;
begin

    Result := 0 ;
    if not Panel.Visible then Exit ;

    // Local components
    ComboBox := Nil ;
    for i  := 0 to Panel.ControlCount-1 do begin
        if ANSIContainsText(Panel.Controls[i].Name,'cbLS') then ComboBox := TComboBox(Panel.Controls[i]) ;
        end;
    if ComboBox = Nil then Exit ;

    Result := Integer(ComboBox.Items.Objects[ComboBox.ItemIndex]) ;

    end;

procedure TSetupFrm.NewCamera ;
// ---------------------------------------------
// Setup up dialog box for newly selected camera
// ---------------------------------------------
var
    i : Integer ;
    iTop : Integer ;
begin

     // Close existing camera and re-open new if camera changed

     if (cbCamera.ItemIndex <> MainFrm.CameraType) or
        (cbCameraNames.ItemIndex <> MainFrm.Cam1.SelectedCamera) or
        (cbAuxCamera.ItemIndex <> MainFrm.AuxCameraType) or
        (cbCameraPort.ItemIndex <> (MainFrm.Cam1.ComPort-1)) then begin

        Screen.Cursor := crHourglass ;
        MainFrm.StatusBar.SimpleText := ' WAIT: Initialising camera ... ' ;
        MainFrm.Cam1.CloseCamera ;

        // Update main camera type
        MainFrm.Cam1.SelectedCamera := cbCameraNames.ItemIndex ;
        MainFrm.CameraType := cbCamera.ItemIndex ;
        MainFrm.AuxCameraType := cbAuxCamera.ItemIndex ;

        // Open new camera
        // (use auxiliary camera setting if main camera is an LSM)
        MainFrm.Cam1.ComPort :=  cbCameraPort.ItemIndex + 1 ;
        if MainFrm.Cam1.IsLSM( MainFrm.CameraType ) then begin
           MainFrm.Cam1.OpenCamera( MainFrm.AuxCameraType ) ;
           end
        else begin
           MainFrm.Cam1.OpenCamera( MainFrm.CameraType ) ;
           end ;

        MainFrm.Cam1.FrameInterval := 0.1 ;

        // Initialise look-up tables
        for i := 0 to MaxFrameType do begin
            MainFrm.GreyLo[i] := 0 ;
            MainFrm.GreyHi[i] := MainFrm.Cam1.GreyLevelMax ;
            MainFrm.UpdateLUT( i, MainFrm.Cam1.GreyLevelMax ) ;
            end ;

        MainFrm.StatusBar.SimpleText := ' Camera Initialised ' ;
        Screen.Cursor := crDefault ;

        end ;

     // Get list of available camera operating modes

     iTop := cbCamera.Top + cbCamera.Height + 5 ;

     // Show auxiliary camera panel if main camera is an LSM
     AuxCameraPanel.Top := iTop ;
     AuxCameraPanel.Visible := MainFrm.Cam1.IsLSM( MainFrm.CameraType ) ;
     if AuxCameraPanel.Visible then iTop := iTop + AuxCameraPanel.Height ;

     // Show camera selection panel if more than one camera available
     CameraPanel.Top := iTop ;
     if MainFrm.Cam1.NumCameras > 1 then begin
        CameraPanel.Visible := True ;
        iTop := iTop + CameraPanel.Height ;
        end
     else CameraPanel.Visible := False ;

     ModePanel.Top := iTop ;
     MainFrm.Cam1.GetCameraModeList( cbCameraMode.Items );

     if cbCameraMode.Items.Count > 1 then ModePanel.Visible := True
                                     else ModePanel.Visible := False ;
     cbCameraMode.ItemIndex := Min(MainFrm.Cam1.CameraMode,cbCameraMode.Items.Count-1) ;
     if ModePanel.Visible then iTop := iTop + ModePanel.Height ;

     // Get list of available camera A/D converters
     ADCPanel.Top := iTop ;
     MainFrm.Cam1.GetCameraADCList( cbCameraADC.Items );
     cbCameraADC.ItemIndex := MainFrm.Cam1.CameraADC ;
     if cbCameraADC.Items.Count > 1 then ADCPanel.Visible := True
                                    else ADCPanel.Visible := False ;
     if ADCPanel.Visible then iTop := iTop + ADCPanel.Height ;

     // Get list of available camera readout speeds
     ReadoutSpeedPanel.Top := iTop ;
     MainFrm.Cam1.GetCameraReadoutSpeedList( cbReadoutSpeed.Items );
     cbReadoutSpeed.ItemIndex := MainFrm.Cam1.ReadoutSpeed ;
     if cbReadoutSpeed.Items.Count > 0 then ReadoutSpeedPanel.Visible := True
                                       else ReadoutSpeedPanel.Visible := False ;
     if ReadoutSpeedPanel.Visible then iTop := iTop + ReadoutSpeedPanel.Height ;

     // Display camera control COM port (if present)
     ComPanel.Top := iTop ;
     if MainFrm.Cam1.ComPortUsed then begin
        ComPanel.Visible := True ;
        iTop := iTop + ComPanel.Height ;
        cbCameraPort.ItemIndex := MainFrm.Cam1.ComPort - 1 ;
        end
     else begin
        cbCameraPort.ItemIndex := -1 ;
        ComPanel.Visible := False ;
        end ;

     edCameraTriggerOffset.Value := MainFrm.CameraTriggerOffset ;
     edCameraReadoutTime.Value := MainFrm.Cam1.AdditionalReadoutTime ;

     end ;


procedure TSetupFrm.bOKClick(Sender: TObject);
// ------------------------------------
// Update settings and close setup form
// ------------------------------------
var
    BadChar,ADCDevice : Integer ;
begin

    Screen.Cursor := crDefault ;

    // Update I/O configuration
    MainFrm.IOConfig.ADCIn := Integer(cbADCIn.Items.Objects[cbADCIn.ItemIndex]) ;

    // A/D channel input mode
    LABIO.ADCInputMode := cbADCInputMode.ItemIndex ;

    if (MainFrm.IOConfig.ADCIn >= 0) and
        (MainFrm.IOConfig.ADCIn <= MaxResources) then begin
        ADCDevice := LabIO.Resource[MainFrm.IOConfig.ADCIn].Device ;
        MainFrm.ADCVoltageRange := LabIO.ADCVoltageRanges[ ADCDevice,cbADCVoltageRange.ItemIndex] ;
       MainFrm.ADCVoltageRange := LabIO.ADCVoltageRanges[ ADCDevice,cbADCVoltageRange.ItemIndex] ;
       end
     else MainFrm.ADCVoltageRange := 10.0 ;


    MainFrm.IOConfig.CameraStart :=
    Integer(cbCameraStart.Items.Objects[cbCameraStart.ItemIndex]) ;
    MainFrm.IOConfig.CameraStartActiveHigh := rbCameraStartActiveHigh.Checked ;

    // Get CCD pre-exposure clear setting
    MainFrm.Cam1.CCDClearPreExposure := ckCCDClearPreExposure.Checked ;

     // Get CCD post-exposure readout setting
    MainFrm.Cam1.CCDPostExposureReadout := ckPostExposureReadout.Checked ;

    // Get bulb exposure mode (exposure trigger pulse duration determines camera exposure time)
    MainFrm.BulbExposureMode := ckBulbExposureMode.Checked ;

    // Command voltage O/P lines 1 & 2

    MainFrm.IOConfig.VCommand[0] :=
    Integer(cbVCommand0.Items.Objects[cbVCommand0.ItemIndex]) ;

    MainFrm.IOConfig.VCommand[1] :=
    Integer(cbVCommand1.Items.Objects[cbVCommand1.ItemIndex]) ;

    MainFrm.IOConfig.VCommand[2] :=
    Integer(cbVCommand2.Items.Objects[cbVCommand2.ItemIndex]) ;

    MainFrm.IOConfig.LSShutter :=
    Integer(cbLSShutter.Items.Objects[cbLSShutter.ItemIndex]) ;
    MainFrm.IOConfig.LSShutterActiveHigh := rbLSShutterActiveHigh.Checked ;

    // Monochromator/filter wavelength control lines
    GetAllLSControlSetting ;

    MainFrm.IOConfig.DigitalStimStart :=
      Integer(cbDigitalStimStart.Items.Objects[cbDigitalStimStart.ItemIndex]) ;

    // Emission filter wavelength control lines
    MainFrm.IOConfig.EmFilterStart :=
      Integer(cbEmFilterStart.Items.Objects[cbEmFilterStart.ItemIndex]) ;
    MainFrm.IOConfig.EmFilterEnd :=
      Integer(cbEmFilterEnd.Items.Objects[cbEmFilterEnd.ItemIndex]) ;

    MainFrm.IOConfig.DigitalStimEnd :=
    Integer(cbDigitalStimEnd.Items.Objects[cbDigitalStimEnd.ItemIndex]) ;

    // Photo stimulator settings

    MainFrm.IOConfig.PhotoStimX :=
    Integer(cbPhotoStimX.Items.Objects[cbPhotoStimX.ItemIndex]) ;
    MainFrm.IOConfig.PhotoStimY :=
    Integer(cbPhotoStimY.Items.Objects[cbPhotoStimY.ItemIndex]) ;
    MainFrm.IOConfig.PhotoStimI1 :=
    Integer(cbPhotoStimIntensity1.Items.Objects[cbPhotoStimIntensity1.ItemIndex]) ;
    MainFrm.IOConfig.PhotoStimI2 :=
    Integer(cbPhotoStimIntensity2.Items.Objects[cbPhotoStimIntensity2.ItemIndex]) ;
    MainFrm.IOConfig.PhotoStimI3 :=
    Integer(cbPhotoStimIntensity3.Items.Objects[cbPhotoStimIntensity3.ItemIndex]) ;

    MainFrm.IOConfig.PhotoStimShutter :=
    Integer(cbPhotoStimShutter.Items.Objects[cbPhotoStimShutter.ItemIndex]) ;
    MainFrm.IOConfig.PhotoStimShutterActiveHigh := rbPhotoStimShutterActiveHigh.Checked ;
    MainFrm.IOConfig.PhotoStimShutterLatency := edPhotoStimShutterLatency.Value ;

    MainFrm.IOConfig.PhotoStimMeter :=
    Integer(cbPhotoStimMeterInput.Items.Objects[cbPhotoStimMeterInput.ItemIndex]) ;

    MainFrm.IOConfig.ClockSyncLine := Integer(cbClockSynchronisation.Items.Objects[
                                       cbClockSynchronisation.ItemIndex]) ;

    // User-calibrated monochromator settings
    LightSource.DeviceType := Integer(cbLightSource.Items.Objects[cbLightSource.ItemIndex]) ;
    LightSource.Wavelength1 := edWavelength1.Value ;
    LightSource.Voltage1 := edVoltage1.Value ;
    LightSource.Wavelength2 := edWavelength2.Value ;
    LightSource.Voltage2 := edVoltage2.Value ;

    // Laser calibration settings
    LightSource.LaserWavelength[0] := edLaser1Wavelength.Value ;
    LightSource.LaserDelay[0] := edLaser1Delay.Value ;
    LightSource.LaserOffVoltage[0] := edLaser1OffVoltage.Value ;
    LightSource.LaserOnVoltage[0] := edLaser1OnVoltage.Value ;
    LightSource.LaserIntensity[0] := edLaser1Intensity.Value ;

    LightSource.LaserWavelength[1] := edLaser2Wavelength.Value ;
    LightSource.LaserDelay[1] := edLaser2Delay.Value ;
    LightSource.LaserOffVoltage[1] := edLaser2OffVoltage.Value ;
    LightSource.LaserOnVoltage[1] := edLaser2OnVoltage.Value ;
    LightSource.LaserIntensity[1] := edLaser2Intensity.Value ;

    LightSource.LaserWavelength[2] := edLaser3Wavelength.Value ;
    LightSource.LaserDelay[2] := edLaser3Delay.Value ;
    LightSource.LaserOffVoltage[2] := edLaser3OffVoltage.Value ;
    LightSource.LaserOnVoltage[2] := edLaser3OnVoltage.Value ;
    LightSource.LaserIntensity[2] := edLaser3Intensity.Value ;

    LightSource.LEDOffVoltage := edLEDOffVoltage.Value ;
    LightSource.LEDMaxVoltage := edLEDMaxVoltage.Value ;

     // TIRF control settings
     LightSource.TIRFOff[1] := edTIRFOff1.Value ;
     LightSource.TIRFOn[1] := edTIRFOn1.Value ;
     LightSource.TIRFWF[1] := edTIRFWF1.Value;
     LightSource.TIRFOff[2] := edTIRFOff2.Value ;
     LightSource.TIRFOn[2] := edTIRFOn2.Value ;
     LightSource.TIRFWF[2] := edTIRFWF2.Value ;

    // Wavelength selected when light source shutters closed
    LightSource.ShutterClosedWavelength := edShutterClosedWavelength.Value ;

    // Shutter blanking period (at end of wavelength cycle
    LightSource.ShutterBlankingPeriod := edShutterBlankingPeriod.Value  ;
    LightSource.ShutterBlankingPeriod := Max(LightSource.ShutterBlankingPeriod,0.0) ;
    LightSource.ShutterChangeTime := edShutterChangeTime.Value ;

    LightSource.EMFilterChangeTime := edEMFilterChangeTime.Value ;

    MainFrm.ADCScanInterval := edADCInterval.Value ;

    // Camera video mode
    MainFrm.Cam1.CameraMode := cbCameraMode.ItemIndex ;

    // Camera CCD readout speed
    MainFrm.Cam1.ReadoutSpeed := cbReadoutSpeed.ItemIndex ;

    // Calibration settings
    MainFrm.Cam1.LensMagnification := edLensMagnification.Value ;
    MainFrm.CalibrationBarSize := edCalibrationBarSize.Value ;
    MainFrm.CalibrationBarThickness := edCalibrationBarThickness.Value ;

    MainFrm.Cam1.CameraTemperatureSetPoint := edTemperatureSetPoint.Value ;

    MainFrm.Cam1.DisableExposureIntervalLimit := ckDisableExposureIntervalLimit.Checked ;

    MainFrm.Cam1.AdditionalReadoutTime := edCameraReadoutTime.Value ;

     // XY stage
     Val(edXMotorSerialNumber.Text,XYStageFrm.XMotorID, BadChar ) ;
     Val(edYMotorSerialNumber.Text,XYStageFrm.YMotorID, BadChar );
     XYStageFrm.XMin := edXYStageXMin.Value ;
     XYStageFrm.XMax := edXYStageXMax.Value ;
     XYStageFrm.YMin := edXYStageYMin.Value ;
     XYStageFrm.YMax := edXYStageYMax.Value ;
     XYStageFrm.StageType := Integer(cbXYStageType.Items.Objects[cbXYStageType.ItemIndex]) ;

     // Z Stage
     ZStage.Available := ckZStageEnabled.Checked ;
     ZStage.CalPosition1 := edZStagePos1.Value ;
     ZStage.CalPosition2 := edZStagePos2.Value ;
     ZStage.CalVoltage1 := edZStageV1.Value ;
     ZStage.CalVoltage2 := edZStageV2.Value ;
     ZStage.VMin := edZStageVMin.Value ;
     ZStage.VMax := edZStageVMax.Value ;
     ZStage.MinStepSize := edZStageMinStepSize.Value ;
     ZStage.StepTime := edZStageStepTime.Value ;
     ZStage.ExcitationOffDuringStep := ckZStepExcitationOffDuringStep.Checked ;
     ZStage.EndExposureAtStep := ckZStepEndExposureAtStep.Checked ;

     MainFrm.IOConfig.ZStageControl :=
                         Integer(cbZStageControl.Items.Objects[cbZStageControl.ItemIndex]) ;

     MainFrm.SplitImageName[0] := edSplitImageUpper.Text ;
     MainFrm.SplitImageName[1] := edSplitImageLower.Text ;

     MainFrm.DarkLevelLo := Round(edDarkLevelLo.Value) ;
     MainFrm.DarkLevelHi := Round(edDarkLevelHi.Value) ;

     CheckSettings ;

    // Update relevant forms if they are active
    if MainFrm.FormExists('TimeCourseFrm') then TimeCourseFrm.UpdateSettings ;

    MainFrm.CameraTriggerOffset := edCameraTriggerOffset.Value ;

    // Auto reset interface cards
    MainFrm.AutoResetInterfaceCards := ckAutoReset.Checked ;

    // Get settings from channel table
    GetChannelsFromEditTable ;

    // Ensure amplifier settings are up to date
    UpdateAmplifierSettings ;

    // Analogue input settings
    MainFrm.ADCNumChannels := Round(edNumChannels.Value)  ;

    // Channel scanning interval
    MainFrm.ADCScanInterval := edADCInterval.Value ;


    // Setup Amplifier telegraph options
    Amplifier.AmplifierType[1] := Integer(cbAmplifier1.Items.Objects[cbAmplifier1.ItemIndex]) ;
    Amplifier.GainTelegraphChannel[1] := Round(edGainTelegraphChannel1.Value) ;
    Amplifier.ModeTelegraphChannel[1] := Round(edModeTelegraphChannel1.Value) ;
    Amplifier.AmplifierType[2] := Integer(cbAmplifier2.Items.Objects[cbAmplifier2.ItemIndex]) ;
    Amplifier.GainTelegraphChannel[2] := Round(edGainTelegraphChannel2.Value) ;
    Amplifier.ModeTelegraphChannel[2] := Round(edModeTelegraphChannel2.Value) ;

    // Get command voltage divide factor
    // (If an amplifier is defined, it over-rides user entered setting
    MainFrm.VCommand[0].DivideFactor := edVDivide0.Value ;
    Amplifier.GetCommandVoltageDivideFactor(1,MainFrm.VCommand[0].DivideFactor) ;
    MainFrm.VCommand[1].DivideFactor := edVDivide1.Value ;
    Amplifier.GetCommandVoltageDivideFactor(2,MainFrm.VCommand[1].DivideFactor) ;

    // Capacity calculation
    MainFrm.Cap.Enabled := ckCapEnabled.Checked ;
    MainFrm.Cap.Frequency := edCapFrequency.Value ;
    MainFrm.Cap.VRev := edCapVRev.Value ;
    MainFrm.Cap.CompensationInUse := ckCapacityCompensationInUse.Checked ;
    MainFrm.Cap.RSeriesComp := edCapRSeriesComp.Value ;
    MainFrm.Cap.CellCapacityComp := edCapCellCapacityComp.Value ;
    MainFrm.Cap.GmDisplayMax := edCapGmDisplayMax.Value ;
    MainFrm.Cap.GsDisplayMax := edCapGsDisplayMax.Value ;
    MainFrm.Cap.CmDisplayMax := edCapCmDisplayMax.Value ;

    Close ;

    end;

procedure TSetupFrm.GetAllLSControlSetting ;
// --------------------------------------------------
// Get all LS conntrol line settings from combo boxes
// --------------------------------------------------
begin
    // Monochromator/filter wavelength control lines
    MainFrm.IOConfig.LSControlLine[0] := GetLSControlSetting( LSControl0 ) ;
    MainFrm.IOConfig.LSControlLine[1] := GetLSControlSetting( LSControl1 ) ;
    MainFrm.IOConfig.LSControlLine[2] := GetLSControlSetting( LSControl2 ) ;
    MainFrm.IOConfig.LSControlLine[3] := GetLSControlSetting( LSControl3 ) ;
    MainFrm.IOConfig.LSControlLine[4] := GetLSControlSetting( LSControl4 ) ;
    MainFrm.IOConfig.LSControlLine[5] := GetLSControlSetting( LSControl5 ) ;
    MainFrm.IOConfig.LSControlLine[6] := GetLSControlSetting( LSControl6 ) ;
    MainFrm.IOConfig.LSControlLine[7] := GetLSControlSetting( LSControl7 ) ;
    end ;


procedure TSetupFrm.bCancelClick(Sender: TObject);
// -------------------------------------
// Exit without updating master settings
// -------------------------------------
begin
    Close ;
    end;


procedure TSetupFrm.FormClose(Sender: TObject; var Action: TCloseAction);
// -----------------------
// Close and destroy form
// -----------------------
begin
     MainFrm.mnCameraSetup.Enabled := True ;
     Action := caFree ;

     // Save position/size of form within parent window
     MainFrm.SaveFormPosition( Self ) ;

     end;


procedure TSetupFrm.cbCameraChange(Sender: TObject);
// --------------
// Camera changed
// --------------
begin

     CameraOpenRequired :=  True ;
     NewCamera ;

     // Set camera readout speed to maximum
     if cbReadoutSpeed.Visible then begin
        cbReadoutSpeed.ItemIndex := MainFrm.Cam1.DefaultReadoutSpeed ;
        end ;

     end;

procedure TSetupFrm.cbCameraPortChange(Sender: TObject);
begin
     CameraOpenRequired :=  True ;
     NewCamera ;
     end;

procedure TSetupFrm.cbLightSourceChange(Sender: TObject);
// --------------------
// Light source changed
// --------------------
begin

     LightSource.DeviceType := Integer(cbLightSource.Items.Objects[cbLightSource.ItemIndex]) ;

     // Display light source settings panels
     DisplayLightSourceSettingsPanels ;

     // Set camera trigger offset with default value for light source
     edCameraTriggerOffset.Value := LightSource.WavelengthChangeTime ;

     // Force updates to control line list
     NewInterfaceCards ;

     // Display light source settings panels
     GetAllLSControlSetting ;
     ShowLaserSettings ;
     DisplayLightSourceSettingsPanels ;

     end;

     
procedure TSetupFrm.rbNIDAQClick(Sender: TObject);
// ----------------------------
// Select Traditional NIDAQ API
// ----------------------------
var
    ch : Integer ;
begin
     if LabIO.NIDAQAPI <> NIDAQ then begin
         MainFrm.StatusBar.SimpleText := 'Wait ... switching to Traditional NIDAQ library' ;
         LabIO.Close ;
         LabIO.NIDAQAPI := NIDAQ ;
         NewInterfaceCards ;
         MainFrm.StatusBar.SimpleText := '' ;
         for ch := 0 to MaxADCChannels-1 do begin
            MainFrm.ADCChannel[ch].yMax := LabIO.ADCMaxValue[1] ;
            MainFrm.ADCChannel[ch].yMin := -LabIO.ADCMaxValue[1] - 1 ;
            end ;
         end ;
     end ;

procedure TSetupFrm.rbNIDAQMXClick(Sender: TObject);
// ----------------------------
// Select NIDAQ-MX API
// ----------------------------
var
    ch : Integer ;
begin
     if LabIO.NIDAQAPI <> NIDAQMX then begin
         MainFrm.StatusBar.SimpleText := 'Wait ... switching to NIDAQ-MX library' ;
         LabIO.Close ;
         LabIO.NIDAQAPI := NIDAQMX ;
         NewInterfaceCards ;
         MainFrm.StatusBar.SimpleText := '' ;
         for ch := 0 to MaxADCChannels-1 do begin
            MainFrm.ADCChannel[ch].yMax := LabIO.ADCMaxValue[1] ;
            MainFrm.ADCChannel[ch].yMin := -LabIO.ADCMaxValue[1] - 1 ;
            end ;
         end ;
     end;

procedure TSetupFrm.edLensMagnificationKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then begin
        MainFrm.Cam1.LensMagnification := edLensMagnification.Value ;
        edPixelWidth.Value := MainFrm.Cam1.PixelWidth ;
        end ;
     end;

procedure TSetupFrm.edNumChannelsKeyPress(Sender: TObject; var Key: Char);
// ---------------------------
// No. of A/D channels changed
// ---------------------------
begin
     if key = #13 then begin
        GetChannelsFromEditTable ;
        MainFrm.ADCNumChannels :=  Round(edNumChannels.Value) ;
        UpdateChannelEditTable ;
        end ;
     end ;

procedure TSetupFrm.bResetDevicesClick(Sender: TObject);
begin

  // Attempt to re-initialize NI cards
  LabIO.ResetNIBoards ;

  // Reload interface card properties
  NewInterfaceCards ;

end;

procedure TSetupFrm.cbCameraModeChange(Sender: TObject);
// -------------------
// Camera mode changed
// -------------------
begin

     // Set camera mode
     MainFrm.Cam1.CameraMode := cbCameraMode.ItemIndex ;

     // Update readout speed list
     MainFrm.Cam1.GetCameraReadoutSpeedList( cbReadoutSpeed.Items );
     cbReadoutSpeed.ItemIndex := Min(Max(MainFrm.Cam1.ReadoutSpeed,0),cbReadoutSpeed.Items.Count-1) ;
     if cbReadoutSpeed.Items.Count > 0 then ReadoutSpeedPanel.Visible := True
                                       else ReadoutSpeedPanel.Visible := False ;

     CameraOpenRequired :=  True ;

     end;

procedure TSetupFrm.cbCameraNamesChange(Sender: TObject);
// --------------
// Camera changed
// --------------
var
    i : Integer ;
    ReadSpeed, MaxSpeed : Single ;
begin

     //MainFrm.Cam1.SelectedCamera := cbCameraNames.ItemIndex ;
     CameraOpenRequired :=  True ;
     NewCamera ;

     // Set camera readout speed to maximum
     if cbReadoutSpeed.Visible then begin
        MaxSpeed := 0.0 ;
        for i := 0 to cbReadoutSpeed.Items.Count-1 do begin
            ReadSpeed := ExtractFloat( cbReadoutSpeed.Items.Strings[i], 0.0 ) ;
            if ReadSpeed > MaxSpeed then begin
               MaxSpeed := ReadSpeed ;
               cbReadoutSpeed.ItemIndex := i ;
               end ;
            end ;
        end ;

     end;

procedure TSetupFrm.cbADCInChange(Sender: TObject);
// --------------------------
// A/D input channels changed
// --------------------------
begin
    MainFrm.IOConfig.ADCIn := Integer(cbADCIn.Items.Objects[cbADCIn.ItemIndex]) ;
    GetADCVoltageRangeList ;
    end;

procedure TSetupFrm.cbCameraADCChange(Sender: TObject);
// -------------------
// Camera mode changed
// -------------------
begin

     // Set camera mode
     MainFrm.Cam1.CameraADC := cbCameraADC.ItemIndex ;

     // Update readout speed list
     MainFrm.Cam1.GetCameraReadoutSpeedList( cbReadoutSpeed.Items );
     cbReadoutSpeed.ItemIndex := Min(Max(MainFrm.Cam1.ReadoutSpeed,0),cbReadoutSpeed.Items.Count-1) ;
     if cbReadoutSpeed.Items.Count > 0 then ReadoutSpeedPanel.Visible := True
                                       else ReadoutSpeedPanel.Visible := False ;

     //CameraOpenRequired :=  True ;
     //NewCamera ;

     end;


procedure TSetupFrm.ShowLaserSettings ;
// -----------------------------------------------------
// Make laser control tabs visible if laser is available
// -----------------------------------------------------
var
    i,nLasers : Integer ;
begin
    nLasers := 0 ;
    Laser1Tab.TabVisible := False ;
    Laser1Tab.Caption := '' ;
    Laser2Tab.TabVisible := False ;
    Laser2Tab.Caption := '' ;
    Laser3Tab.TabVisible := False ;
    Laser3Tab.Caption := '' ;
    for i := 0 to MaxLSControlLine do begin
        outputdebugstring(pchar(LightSource.ControlLineName(i)+
        format(' %d',[MainFrm.IOConfig.LSControlLine[i]]) ));
        if ANSIContainsText( LightSource.ControlLineName(i), 'LED/Laser') and
           MainFrm.IOResourceAvailable(MainFrm.IOConfig.LSControlLine[i]) then begin
           Inc(nLasers) ;
           case nLasers of
              1 : begin
                Laser1Tab.TabVisible := True ;
                Laser1Tab.Caption := LightSource.ControlLineName(i) ;
                end;
              2 : begin
                Laser2Tab.TabVisible := True ;
                Laser2Tab.Caption := LightSource.ControlLineName(i) ;
                end;
              3 : begin
                Laser3Tab.TabVisible := True ;
                Laser3Tab.Caption := LightSource.ControlLineName(i) ;
                end;
              end;
           end;
        end;
    LSLaserPage.Invalidate ;
    if nLasers > 0 then LSLaserGrp.Visible := True
                   else  LSLaserGrp.Visible := False ;

    end ;


procedure TSetupFrm.cbLSControl0Change(Sender: TObject);
// ---------------------------------
// Light source control line changed
// ---------------------------------
begin
     GetAllLSControlSetting ;
     ShowLaserSettings ;
     DisplayLightSourceSettingsPanels ;

     end;


procedure TSetupFrm.CheckSettings ;
// -----------------------------------------------------
// Check I/O settings for conflicts and missing settings
// -----------------------------------------------------
var
    OK : Boolean ;
    i,j : Integer ;
    ResCount : Array[0..MaxResources-1] of Integer ;
begin
     OK := False ;
     if LabIO.NumDevices <= 0 then Exit ;

     for i := 0 to High(LabIO.Resource) do if LabIO.Resource[i].ResourceType = ADCIn then begin
         if MainFrm.IOConfig.ADCIn = i then OK := True ;
         end;
     if not OK then ShowMessage('SETUP: Warning! No Analog input channels assigned!');

     // Check analog outputs

     for i := 0 to High(LabIO.Resource) do ResCount[i] := 0 ;
     for i := 0 to High(LabIO.Resource) do if LabIO.Resource[i].ResourceType = DACOut then begin
         if MainFrm.IOConfig.CameraStart = i then  Inc(ResCount[i]);
         for j := 0 to High(MainFrm.IOConfig.VCommand) do
             if MainFrm.IOConfig.VCommand[j] = i then  Inc(ResCount[i]);

         for j := 0 to High(MainFrm.IOConfig.LSControlLine) do
             if MainFrm.IOConfig.LSControlLine[j] = i then Inc(ResCount[i]);

         for j := MainFrm.IOConfig.DigitalStimStart to MainFrm.IOConfig.DigitalStimEnd do
             if j = i then  Inc(ResCount[i]);
         if MainFrm.IOConfig.PhotoStimX = i then  Inc(ResCount[i]);
         if MainFrm.IOConfig.PhotoStimY = i then  Inc(ResCount[i]);
         if MainFrm.IOConfig.PhotoStimI1 = i then  Inc(ResCount[i]);
         if MainFrm.IOConfig.PhotoStimI2 = i then  Inc(ResCount[i]);
         if MainFrm.IOConfig.PhotoStimI3 = i then  Inc(ResCount[i]);
         if MainFrm.IOConfig.PhotoStimShutter = i then  Inc(ResCount[i]);
         if MainFrm.IOConfig.ZStageControl = i then  Inc(ResCount[i]);
         end;

      // At least one analog channel required
      OK := False ;
      for i := 0 to High(ResCount) do if ResCount[i] > 0 then OK := True ;
      if not OK then ShowMessage('SETUP: Warning! No analog output channels assigned! At least one is required to enable timing.');

      // Check for multiple analog output channel selection
      for i := 0 to High(ResCount) do if ResCount[i] >= 2 then begin
          ShowMessage(format('SETUP: Warning! Channel Device%d:AO%d selected more than once!',
                  [LabIO.Resource[i].Device,LabIO.Resource[i].StartChannel]));
          end;

     // Check digital outputs

     for i := 0 to High(LabIO.Resource) do ResCount[i] := 0 ;
     for i := 0 to High(LabIO.Resource) do if LabIO.Resource[i].ResourceType = DIGOut then begin
         if MainFrm.IOConfig.CameraStart = i then  Inc(ResCount[i]);
         if MainFrm.IOConfig.LSShutter = i then  Inc(ResCount[i]);
         for j := 0 to High(MainFrm.IOConfig.LSControlLine) do
             if MainFrm.IOConfig.LSControlLine[i] = i then Inc(ResCount[i]);
         for j := MainFrm.IOConfig.DigitalStimStart to MainFrm.IOConfig.DigitalStimEnd do
             if j = i then  Inc(ResCount[i]);
         if MainFrm.IOConfig.PhotoStimShutter = i then  Inc(ResCount[i]);
         end;

      // Check for multiple digital output channel selection
      for i := 0 to High(ResCount) do if ResCount[i] >= 2 then begin
          ShowMessage(format('SETUP: Warning! Channel Device%d:PO0.%d selected more than once!',
                  [LabIO.Resource[i].Device,LabIO.Resource[i].StartChannel]));
          end;


      end ;

procedure TSetupFrm.UpdateChannelEditTable ;
// ----------------------------
// Update channel editing table
// ----------------------------
var
     ch : Integer ;
begin

     { Set A/D input channel calibration table }
     ChannelTable.cells[ChNum,0] := 'Ch.' ;
     ChannelTable.colwidths[ChNum] := ChannelTable.DefaultColWidth div 2 ;
     ChannelTable.cells[ChName,0] := 'Name' ;
     ChannelTable.colwidths[ChName] := ChannelTable.DefaultColWidth ;
     ChannelTable.cells[ChCal,0] := 'V/Units' ;
     ChannelTable.colwidths[ChCal] := (5*ChannelTable.DefaultColWidth) div 4 ;
     ChannelTable.cells[ChUnits,0] := 'Units' ;
     ChannelTable.colwidths[ChUnits] := ChannelTable.DefaultColWidth ;
     ChannelTable.RowCount := MainFrm.ADCNumChannels + 1;
     ChannelTable.options := [goEditing,goHorzLine,goVertLine] ;

     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
         ChannelTable.cells[ChNum,ch+1] := IntToStr(ch) ;
         ChannelTable.cells[ChName,ch+1] := MainFrm.ADCChannel[ch].ADCName ;
         ChannelTable.cells[ChCal,ch+1] := Format( '%5.4g',
                                           [MainFrm.ADCChannel[ch].ADCCalibrationFactor] ) ;
         ChannelTable.cells[ChUnits,ch+1] := MainFrm.ADCChannel[ch].ADCUnits ;
         end ;

     end ;


procedure TSetupFrm.GetChannelsFromEditTable ;
// --------------------------------------------
// Get channel calibration data from edit table
// --------------------------------------------
var
    ch : Integer ;
begin

    for ch := 0 to MainFrm.ADCNumChannels-1 do begin
       MainFrm.ADCChannel[ch].ADCName := ChannelTable.cells[ChName,ch+1] ;
       MainFrm.ADCChannel[ch].ADCCalibrationFactor := ExtractFloat(
                                             ChannelTable.cells[ChCal,ch+1],
                                             MainFrm.ADCChannel[ch].ADCCalibrationFactor);
       MainFrm.ADCChannel[ch].ADCUnits := ChannelTable.cells[ChUnits,ch+1] ;
       end ;
    end ;


procedure TSetupFrm.UpdateAmplifierSettings ;
// ----------------------------------------------
// Update channel settings when amplifier changed
// ----------------------------------------------
var
    ch : Integer ;

begin

     Amplifier.AmplifierType[1] := Integer(cbAmplifier1.Items.Objects[cbAmplifier1.ItemIndex]) ;
     GainTelPanel1.Visible := Amplifier.NeedsGainTelegraphChannel[1] ;
     ModeTelPanel1.Visible := Amplifier.NeedsModeTelegraphChannel[1] ;

     Amplifier.AmplifierType[2] := Integer(cbAmplifier2.Items.Objects[cbAmplifier2.ItemIndex]) ;
     GainTelPanel2.Visible := Amplifier.NeedsGainTelegraphChannel[2] ;
     ModeTelPanel2.Visible := Amplifier.NeedsModeTelegraphChannel[2] ;

     // Update channels 0 & 1 settings which depend upon amplifier
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin

         Amplifier.GetChannelSettings( ch,
                                       MainFrm.ADCChannel[ch].ADCName,
                                       MainFrm.ADCChannel[ch].ADCUnits,
                                       MainFrm.ADCChannel[ch].ADCCalibrationFactor,
                                       MainFrm.ADCChannel[ch].ADCAmplifierGain ) ;

        end ;

     // Get command voltage divide factor
     // (If an amplifier is defined, it over-rides user entered setting
     MainFrm.VCommand[0].DivideFactor := edVDivide0.Value ;
     Amplifier.GetCommandVoltageDivideFactor(1,MainFrm.VCommand[0].DivideFactor) ;
     edVDivide0.Value := MainFrm.VCommand[0].DivideFactor  ;
     MainFrm.VCommand[1].DivideFactor := edVDivide1.Value ;
     Amplifier.GetCommandVoltageDivideFactor(2,MainFrm.VCommand[1].DivideFactor) ;
     edVDivide1.Value := MainFrm.VCommand[1].DivideFactor  ;

     end ;




end.
