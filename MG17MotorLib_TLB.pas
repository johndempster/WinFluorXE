unit MG17MotorLib_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 03/02/2015 15:53:37 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\PROGRA~1\Thorlabs\APT\APTSER~1\MG17MO~1.OCX (1)
// LIBID: {2A833923-9AA7-4C45-90AC-DA4F19DC24D1}
// LCID: 0
// Helpfile: C:\PROGRA~1\Thorlabs\APT\APTSER~1\APTServer.hlp 
// HelpString: MG17Motor ActiveX Control module
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleCtrls, Vcl.OleServer, Winapi.ActiveX;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  MG17MotorLibMajorVersion = 1;
  MG17MotorLibMinorVersion = 0;

  LIBID_MG17MotorLib: TGUID = '{2A833923-9AA7-4C45-90AC-DA4F19DC24D1}';

  DIID__DMG17Motor: TGUID = '{83D03B54-E1C4-466F-97A2-CBBF3330927A}';
  DIID__DMG17MotorEvents: TGUID = '{641DF21F-2956-4F93-B442-2CA591C73628}';
  CLASS_MG17Motor: TGUID = '{3CE35BF3-1E13-4D2C-8C0B-DEF6314420B3}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum LLMOTREQIDS
type
  LLMOTREQIDS = TOleEnum;
const
  DEV_PARAMS = $00000000;
  DIGOUTPUTS = $00000001;
  CHANENABLEDSTATE = $00000002;
  VEL_PARAMS = $00000003;
  JOG_PARAMS = $00000004;
  LIMSWITCH_PARAMS = $00000005;
  POWER_PARAMS = $00000006;
  GENMOVE_PARAMS = $00000007;
  HOME_PARAMS = $00000008;
  MOVEREL_PARAMS = $00000009;
  MOVEABS_PARAMS = $0000000A;
  MOVEVEL_PARAMS = $0000000B;
  TRIGBITS = $0000000C;
  HOSTDIGOUTPUTS = $0000000D;
  DCPID_PARAMS = $0000000E;
  DCADVANCED_PARAMS = $0000000F;
  POT_PARAMS = $00000010;
  AV_MODES = $00000011;
  BUTTON_PARAMS = $00000012;
  PMD_ILOOP_PARAMS = $00000013;
  PMD_PLOOP_PARAMS = $00000014;
  PMD_MOUTPUT_PARAMS = $00000015;
  PMD_TRACKSETTLE_PARAMS = $00000016;
  PMD_PROFILE_PARAMS = $00000017;
  PMD_JOYSTICK_PARAMS = $00000018;
  PMD_STAGEAXIS_PARAMS = $00000019;
  PMD_ILOOPSETTLED_PARAMS = $0000001A;

// Constants for enum HWCHANNEL
type
  HWCHANNEL = TOleEnum;
const
  CHAN1_ID = $00000000;
  CHAN2_ID = $00000001;
  CHANBOTH_ID = $0000000A;

// Constants for enum STAGETYPE
type
  STAGETYPE = TOleEnum;
const
  TYPE_UNKNOWN = $00000001;
  TYPE_17APT600R = $00000002;
  TYPE_17APT600L = $00000003;
  TYPE_NANOMAX300 = $00000010;
  TYPE_NST150 = $00000020;
  TYPE_NST25 = $00000021;
  TYPE_NST50 = $00000022;
  TYPE_NST75 = $00000023;
  TYPE_NST100 = $00000024;
  TYPE_NANOMAX600_DRV001 = $00000030;
  TYPE_DRV005 = $00000040;
  TYPE_DRV006 = $00000041;
  TYPE_DRV013 = $00000042;
  TYPE_DRV014 = $00000043;
  TYPE_DRV113 = $00000044;
  TYPE_DRV114 = $00000045;
  TYPE_AMR101 = $00000050;
  TYPE_ZST6 = $00000060;
  TYPE_ZST13 = $00000061;
  TYPE_ZST25 = $00000062;
  TYPE_Z606 = $00000063;
  TYPE_Z612 = $00000064;
  TYPE_Z625 = $00000065;
  TYPE_07EAS503 = $00000070;
  TYPE_07EAS504 = $00000071;

// Constants for enum STAGEAXIS
type
  STAGEAXIS = TOleEnum;
const
  AXIS_UNKNOWN = $00000001;
  AXIS_SINGLE = $00000002;
  AXIS_ROTARY = $00000003;
  AXIS_X = $00000010;
  AXIS_Y = $00000011;
  AXIS_Z = $00000012;
  AXIS_PITCH = $00000013;
  AXIS_ROLL = $00000014;
  AXIS_YAW = $00000015;

// Constants for enum STAGEUNITS
type
  STAGEUNITS = TOleEnum;
const
  UNITS_MM = $00000001;
  UNITS_DEG = $00000002;

// Constants for enum STOPMODE
type
  STOPMODE = TOleEnum;
const
  STOP_IMMEDIATE = $00000001;
  STOP_PROFILED = $00000002;

// Constants for enum JOGMODE
type
  JOGMODE = TOleEnum;
const
  JOG_CONTINUOUS = $00000001;
  JOG_SINGLESTEP = $00000002;

// Constants for enum SWLIMITMODE
type
  SWLIMITMODE = TOleEnum;
const
  SWLIMIT_IGNORE = $00000001;
  SWLIMIT_IMMEDIATESTOP = $00000002;
  SWLIMIT_PROFILEDSTOP = $00000003;

// Constants for enum HOMEDIR
type
  HOMEDIR = TOleEnum;
const
  HOME_FWD = $00000001;
  HOME_REV = $00000002;

// Constants for enum HOMELIMSWITCH
type
  HOMELIMSWITCH = TOleEnum;
const
  HOMELIMSW_FWD_HW = $00000004;
  HOMELIMSW_REV_HW = $00000001;

// Constants for enum HWLIMSWITCH
type
  HWLIMSWITCH = TOleEnum;
const
  HWLIMSW_IGNORE = $00000001;
  HWLIMSW_MAKES = $00000002;
  HWLIMSW_BREAKS = $00000003;
  HWLIMSW_MAKES_HOMEONLY = $00000004;
  HWLIMSW_BREAKS_HOMEONLY = $00000005;

// Constants for enum MOVEVELDIR
type
  MOVEVELDIR = TOleEnum;
const
  MOVE_FWD = $00000001;
  MOVE_REV = $00000002;

// Constants for enum MOVEJOGDIR
type
  MOVEJOGDIR = TOleEnum;
const
  JOG_FWD = $00000001;
  JOG_REV = $00000002;

// Constants for enum TRIGMODE
type
  TRIGMODE = TOleEnum;
const
  TRIGMODE_DISABLED = $00000001;
  TRIGMODE_IN = $00000002;
  TRIGMODE_INOUT = $00000003;
  TRIGMODE_OUT = $00000004;

// Constants for enum TRIGMOVE
type
  TRIGMOVE = TOleEnum;
const
  TRIGMOVE_REL = $00000001;
  TRIGMOVE_ABS = $00000002;
  TRIGMOVE_HOME = $00000003;

// Constants for enum ENCQEPSENSE
type
  ENCQEPSENSE = TOleEnum;
const
  ENC_QEP_POSITIVE = $00000001;
  ENC_QEP_NEGATIVE = $FFFFFFFF;

// Constants for enum ENCPOSSOURCEMODE
type
  ENCPOSSOURCEMODE = TOleEnum;
const
  ENC_POSSRC_MICROSTEP = $00000001;
  ENC_POSSRC_ENCODER = $00000002;

// Constants for enum ENCPOSCONTROLMODE
type
  ENCPOSCONTROLMODE = TOleEnum;
const
  ENC_POSCTRL_DISABLED = $00000001;
  ENC_POSCTRL_POSITION = $00000002;
  ENC_POSCTRL_POSITIONSTOPSHORT = $00000003;

// Constants for enum DISPLAYMODE
type
  DISPLAYMODE = TOleEnum;
const
  DISPMODE_PANEL = $00000001;
  DISPMODE_GRAPH = $00000002;
  DISPMODE_POSITION = $00000003;

// Constants for enum INDICATORLEDMODE
type
  INDICATORLEDMODE = TOleEnum;
const
  LED_IDENT = $00000001;
  LED_LIMITSWITCH = $00000002;
  LED_BUTTONMODECHANGE = $00000004;
  LED_MOVING = $00000008;

// Constants for enum BUTTONMODE
type
  BUTTONMODE = TOleEnum;
const
  BUTTON_MODEJOG = $00000001;
  BUTTON_MODEGOTO = $00000002;

// Constants for enum APT_PARENT_HWTYPES
type
  APT_PARENT_HWTYPES = TOleEnum;
const
  USB_BMC103 = $00000028;
  USB_BBD103 = $0000002D;
  ETHNET_MMR601 = $0000002A;
  USB_MMR601 = $0000002B;

// Constants for enum ROTMOVEMODE
type
  ROTMOVEMODE = TOleEnum;
const
  ROT_MOVE_POS = $00000001;
  ROT_MOVE_NEG = $00000002;
  ROT_MOVE_SHORT = $00000003;

// Constants for enum ROTPOSDISPMODE
type
  ROTPOSDISPMODE = TOleEnum;
const
  ROT_POSDISP_360 = $00000001;
  ROT_POSDISP_TOTAL = $00000002;

// Constants for enum DCPROFILEMODE
type
  DCPROFILEMODE = TOleEnum;
const
  PROFMODE_TRAPEZOIDAL = $00000001;
  PROFMODE_SCURVE = $00000003;

// Constants for enum SOLOUTPUTSTATE
type
  SOLOUTPUTSTATE = TOleEnum;
const
  OUTPUTSTATE_ON = $00000001;
  OUTPUTSTATE_OFF = $00000002;

// Constants for enum SOLOPERATINGMODE
type
  SOLOPERATINGMODE = TOleEnum;
const
  SOLENOID_MANUAL = $00000001;
  SOLENOID_SINGLE = $00000002;
  SOLENOID_AUTO = $00000003;
  SOLENOID_TRIGGER = $00000004;

// Constants for enum BRAKE_STATE
type
  BRAKE_STATE = TOleEnum;
const
  BRAKE_ON = $00000001;
  BRAKE_OFF = $00000002;

// Constants for enum DCTRIGINMODE
type
  DCTRIGINMODE = TOleEnum;
const
  DCTRIGIN_DISABLED = $00000001;
  DCTRIGINRISE_RELMOVE = $00000002;
  DCTRIGINFALL_RELMOVE = $00000003;
  DCTRIGINRISE_ABSMOVE = $00000004;
  DCTRIGINFALL_ABSMOVE = $00000005;
  DCTRIGINRISE_HOMEMOVE = $00000006;
  DCTRIGINFALL_HOMEMOVE = $00000007;

// Constants for enum DCTRIGOUTMODE
type
  DCTRIGOUTMODE = TOleEnum;
const
  DCTRIGOUT_DISABLED = $00000001;
  DCTRIGOUTHIGH_INMOTION = $00000002;
  DCTRIGOUTLOW_INMOTION = $00000003;
  DCTRIGOUTHIGH_MOTIONCOMPLETE = $00000004;
  DCTRIGOUTLOW_MOTIONCOMPLETE = $00000005;
  DCTRIGOUTHIGH_MAXVELOCITY = $00000006;
  DCTRIGOUTLOW_MAXVELOCITY = $00000007;

// Constants for enum MFFOPERMODE
type
  MFFOPERMODE = TOleEnum;
const
  MFFOPERMODE_IP_TOGGLEPOS = $00000001;
  MFFOPERMODE_IP_GOTOPOS = $00000002;
  MFFOPERMODE_OP_ATPOS = $00000003;
  MFFOPERMODE_OP_MOVING = $00000004;

// Constants for enum MFFSIGMODE
type
  MFFSIGMODE = TOleEnum;
const
  MFFSIGMODE_IP_BUTTON = $00000001;
  MFFSIGMODE_IP_LOGIC = $00000002;
  MFFSIGMODE_IP_SWAP = $00000004;
  MFFSIGMODE_OP_LEVEL = $00000010;
  MFFSIGMODE_OP_PULSE = $00000020;
  MFFSIGMODE_OP_INVERT = $00000040;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _DMG17Motor = dispinterface;
  _DMG17MotorEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  MG17Motor = _DMG17Motor;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PSingle1 = ^Single; {*}
  PInteger1 = ^Integer; {*}
  POleVariant1 = ^OleVariant; {*}
  PWideString1 = ^WideString; {*}
  PByte1 = ^Byte; {*}
  PWordBool1 = ^WordBool; {*}


// *********************************************************************//
// DispIntf:  _DMG17Motor
// Flags:     (4096) Dispatchable
// GUID:      {83D03B54-E1C4-466F-97A2-CBBF3330927A}
// *********************************************************************//
  _DMG17Motor = dispinterface
    ['{83D03B54-E1C4-466F-97A2-CBBF3330927A}']
    property HWSerialNum: Integer dispid 4;
    property APTHelp: WordBool dispid 69;
    property DISPLAYMODE: Integer dispid 97;
    procedure AboutBox; dispid -552;
    function StartCtrl: Integer; dispid 5;
    function StopCtrl: Integer; dispid 6;
    function GetPosition(lChanID: Integer; var pfPosition: Single): Integer; dispid 8;
    function LLSetGetPosParams(bSet: WordBool; lChanID: Integer; var plPosControlMode: Integer; 
                               var plMicroStepDivider: Integer; var plPosStepRes: Integer): Integer; dispid 10;
    function LLSetGetVelParams(bSet: WordBool; lChanID: Integer; var plMinVel: Integer; 
                               var plAccn: Integer; var plMaxVel: Integer): Integer; dispid 11;
    function LLSetGetJogParams(bSet: WordBool; lChanID: Integer; var plJogMode: Integer; 
                               var plJogStepSize: Integer; var plMinVel: Integer; 
                               var plAccn: Integer; var plMaxVel: Integer): Integer; dispid 13;
    function LLSetGetLimSwitchParams(bSet: WordBool; lChanID: Integer; var plCWHardLimit: Integer; 
                                     var plCCWHardLimit: Integer; var plCWSoftLimit: Integer; 
                                     var plCCWSoftLimit: Integer; var plSoftLimitMode: Integer): Integer; dispid 14;
    function LLSetGetPowerParams(bSet: WordBool; lChanID: Integer; var plRestFactor: Integer; 
                                 var plMoveFactor: Integer): Integer; dispid 15;
    function LLSetGetHomeParams(bSet: WordBool; lChanID: Integer; var plDirection: Integer; 
                                var plLimSwitch: Integer; var plHomeVel: Integer; 
                                var plOffsetDist: Integer): Integer; dispid 16;
    function LLSetGetMoveRelParams(bSet: WordBool; lChanID: Integer; var plRelDist: Integer): Integer; dispid 17;
    function LLSetGetMoveAbsParams(bSet: WordBool; lChanID: Integer; var plAbsPos: Integer): Integer; dispid 18;
    function MoveVelocity(lChanID: Integer; lDirection: Integer): Integer; dispid 22;
    function LLMoveStop(lChanID: Integer; lStopMode: Integer): Integer; dispid 23;
    function LLReqHWParams(lChanID: Integer; lParamID: Integer): Integer; dispid 24;
    function EnableHWChannel(lChanID: Integer): Integer; dispid 25;
    function DisableHWChannel(lChanID: Integer): Integer; dispid 26;
    function LLSetGetDevParams(bSet: WordBool; var plDevParams: Integer): Integer; dispid 27;
    function LLSetGetDevParamsEx(bSet: WordBool; const plDevParams: OleVariant): Integer; dispid 28;
    function MoveHome(lChanID: Integer; bWait: WordBool): Integer; dispid 29;
    function StopImmediate(lChanID: Integer): Integer; dispid 30;
    function StopProfiled(lChanID: Integer): Integer; dispid 31;
    function MoveRelativeEx(lChanID: Integer; fRelDistCh1: Single; fRelDistCh2: Single; 
                            bWait: WordBool): Integer; dispid 32;
    function MoveAbsoluteEx(lChanID: Integer; fAbsPosCh1: Single; fAbsPosCh2: Single; 
                            bWait: WordBool): Integer; dispid 33;
    function MoveRelative(lChanID: Integer; bWait: WordBool): Integer; dispid 34;
    function MoveAbsolute(lChanID: Integer; bWait: WordBool): Integer; dispid 35;
    function LLGetStatusBits(lChanID: Integer; var plStatusBits: Integer): Integer; dispid 36;
    function LLSetGetGenMoveParams(bSet: WordBool; lChanID: Integer; var plBLashDist: Integer): Integer; dispid 37;
    function SetVelParams(lChanID: Integer; fMinVel: Single; fAccn: Single; fMaxVel: Single): Integer; dispid 38;
    function GetVelParams(lChanID: Integer; var pfMinVel: Single; var pfAccn: Single; 
                          var pfMaxVel: Single): Integer; dispid 39;
    function GetStageAxis(lChanID: Integer; var pbstrStageAxisName: WideString; 
                          var plStageID: Integer; var plAxisID: Integer): Integer; dispid 40;
    function GetStageAxisInfo(lChanID: Integer; var pfMinPos: Single; var pfMaxPos: Single; 
                              var plUnits: Integer; var pfPitch: Single; var plDirSense: Integer): Integer; dispid 41;
    function GetVelParamLimits(lChanID: Integer; var pfMaxAccn: Single; var pfMaxVel: Single): Integer; dispid 42;
    function SetJogMode(lChanID: Integer; lMode: Integer; lStopMode: Integer): Integer; dispid 43;
    function GetJogMode(lChanID: Integer; var plMode: Integer; var plStopMode: Integer): Integer; dispid 44;
    function SetJogStepSize(lChanID: Integer; fStepSize: Single): Integer; dispid 45;
    function GetJogStepSize(lChanID: Integer; var pfStepSize: Single): Integer; dispid 46;
    function SetJogVelParams(lChanID: Integer; fMinVel: Single; fAccn: Single; fMaxVel: Single): Integer; dispid 47;
    function GetJogVelParams(lChanID: Integer; var pfMinVel: Single; var pfAccn: Single; 
                             var pfMaxVel: Single): Integer; dispid 48;
    function SetRelMoveDist(lChanID: Integer; fRelDist: Single): Integer; dispid 49;
    function GetRelMoveDist(lChanID: Integer; var pfRelDist: Single): Integer; dispid 50;
    function SetAbsMovePos(lChanID: Integer; fAbsPos: Single): Integer; dispid 51;
    function GetAbsMovePos(lChanID: Integer; var pfAbsPos: Single): Integer; dispid 52;
    function LLSetGetDigOPs(bSet: WordBool; var plBits: Integer): Integer; dispid 54;
    function LLGetDigIPs(var plBits: Integer): Integer; dispid 55;
    function LLGetADCInputs(var plADCVal1: Integer; var plADCVal2: Integer): Integer; dispid 56;
    function LLGetPosition(lChanID: Integer; var plPosition: Integer): Integer; dispid 57;
    function SetBLashDist(lChanID: Integer; fBLashDist: Single): Integer; dispid 58;
    function GetBLashDist(lChanID: Integer; var pfBLashDist: Single): Integer; dispid 59;
    function Identify: Integer; dispid 60;
    function SetSWPosLimits(lChanID: Integer; fRevPosLimit: Single; fFwdPosLimit: Single; 
                            lLimitMode: Integer): Integer; dispid 61;
    function GetSWPosLimits(lChanID: Integer; var pfRevPosLimit: Single; var pfFwdPosLimit: Single; 
                            var plLimitMode: Integer): Integer; dispid 62;
    function SetStageAxisInfo(lChanID: Integer; fMinPos: Single; fMaxPos: Single; lUnits: Integer; 
                              fPitch: Single; lDirSense: Integer): Integer; dispid 63;
    function LLSaveHWDefaults: Integer; dispid 64;
    function SetHomeParams(lChanID: Integer; lDirection: Integer; lLimSwitch: Integer; 
                           fHomeVel: Single; fZeroOffset: Single): Integer; dispid 65;
    function GetHomeParams(lChanID: Integer; var plDirection: Integer; var plLimSwitch: Integer; 
                           var pfHomeVel: Single; var pfZeroOffset: Single): Integer; dispid 66;
    function SetHWLimSwitches(lChanID: Integer; lRevLimSwitch: Integer; lFwdLimSwitch: Integer): Integer; dispid 67;
    function GetHWLimSwitches(lChanID: Integer; var plRevLimSwitch: Integer; 
                              var plFwdLimSwitch: Integer): Integer; dispid 68;
    function SetPhaseCurrents(lChanID: Integer; lRestVal: Integer; lMoveVal: Integer): Integer; dispid 71;
    function GetPhaseCurrents(lChanID: Integer; var plRestVal: Integer; var plMoveVal: Integer): Integer; dispid 72;
    function GetPositionEx(lChanID: Integer; var pfCalibPosition: Single; 
                           var pfUncalibPosition: Single): Integer; dispid 73;
    function DoEvents: Integer; dispid 74;
    function LLSetEncoderCount(lChanID: Integer; lEncCount: Integer): Integer; dispid 75;
    function LLGetEncoderCount(lChanID: Integer; var plEncCount: Integer): Integer; dispid 76;
    function LLSetGetTrigBits(bSet: WordBool; lChanID: Integer; var plBits: Integer): Integer; dispid 77;
    function MoveJog(lChanID: Integer; lJogDir: Integer): Integer; dispid 78;
    function SetMotorParams(lChanID: Integer; lStepsPerRev: Integer; lGearBoxRatio: Integer): Integer; dispid 79;
    function GetMotorParams(lChanID: Integer; var plStepsPerRev: Integer; 
                            var plGearBoxRatio: Integer): Integer; dispid 80;
    function SetTriggerParams(lChanID: Integer; lTrigMode: Integer; lTrigMove: Integer): Integer; dispid 81;
    function GetTriggerParams(lChanID: Integer; var plTrigMode: Integer; var plTrigMove: Integer): Integer; dispid 82;
    function SaveParamSet(const bstrName: WideString): Integer; dispid 83;
    function LoadParamSet(const bstrName: WideString): Integer; dispid 84;
    function DeleteParamSet(const btsrName: WideString): Integer; dispid 85;
    function LLSetDSPProgState(bProg: WordBool): Integer; dispid 86;
    function LLSendDSPProgData(lNumBytes: Integer; var lpbyData: Byte): Integer; dispid 87;
    function MoveAbsoluteEnc(lChanID: Integer; fAbsPosCh1: Single; fAbsPosCh2: Single; 
                             lTimeInc: Integer; bWait: WordBool): Integer; dispid 88;
    function MoveRelativeEnc(lChanID: Integer; fRelDistCh1: Single; fRelDistCh2: Single; 
                             lTimeInc: Integer; bWait: WordBool): Integer; dispid 89;
    function CalibrateEnc(lChanID: Integer; bWait: WordBool): Integer; dispid 90;
    function SetEncCalibTableParams(lChanID: Integer; lEncCalib: Integer; fCalibStep: Single; 
                                    lCalibDwell: Integer; lQEPSense: Integer): Integer; dispid 91;
    function GetEncCalibTableParams(lChanID: Integer; var plEncCalib: Integer; 
                                    var pfCalibStep: Single; var plCalibDwell: Integer; 
                                    var plQEPSense: Integer): Integer; dispid 92;
    function SetEncPosCorrectParams(lChanID: Integer; lPosSetPtWnd: Integer; lSnguStepWnd: Integer; 
                                    lStopShortDist: Integer; lCorrMoveStep: Integer): Integer; dispid 93;
    function GetEncPosCorrectParams(lChanID: Integer; var plPosSetPtWnd: Integer; 
                                    var plSnguStepWnd: Integer; var plStopShortDist: Integer; 
                                    var plCorrMoveStep: Integer): Integer; dispid 94;
    function SetEncPosControlParams(lChanID: Integer; lPosSrcMode: Integer; lPosCorrMode: Integer; 
                                    bUseCalib: WordBool): Integer; dispid 95;
    function GetEncPosControlParams(lChanID: Integer; var plPosSrcMode: Integer; 
                                    var plPosCorrMode: Integer; var pbUseCalib: WordBool): Integer; dispid 96;
    function LLSetGetHostDigOPs(bSet: WordBool; var plBits: Integer): Integer; dispid 98;
    function LLGetHostStatusBits(var plStatusBits: Integer): Integer; dispid 99;
    function LLSetGetPIDParams(bSet: WordBool; lChanID: Integer; var plProp: Integer; 
                               var plInt: Integer; var plDeriv: Integer; var plIntLimit: Integer; 
                               var plFilterCtrl: Integer): Integer; dispid 100;
    function LLGetAdvancedDCParams(lChanID: Integer; var plTargetPos: Integer; 
                                   var plTargetVel: Integer; var plIntSum: Integer): Integer; dispid 101;
    function LLSetGetPotParams(bSet: WordBool; lChanID: Integer; var plZeroWnd: Integer; 
                               var plVel1: Integer; var plWnd1: Integer; var plVel2: Integer; 
                               var plWnd2: Integer; var plVel3: Integer; var plWnd3: Integer; 
                               var plVel4: Integer): Integer; dispid 102;
    function SetIndicatorLEDMode(lChanID: Integer; lLEDMode: Integer): Integer; dispid 103;
    function GetIndicatorLEDMode(lChanID: Integer; var plLEDMode: Integer): Integer; dispid 104;
    function LLSetGetAVMode(bSet: WordBool; lChanID: Integer; var plModeBits: Integer): Integer; dispid 105;
    function LLSetGetButtonParams(bSet: WordBool; lChanID: Integer; var plMode: Integer; 
                                  var plPos1: Integer; var plPos2: Integer; 
                                  var plTimeout1: Integer; var plTimeout2: Integer): Integer; dispid 106;
    function LLGetOptoDCVelParams(lChanID: Integer; var plMinVel: Integer; var plAccn: Integer; 
                                  var plMaxVel: Integer): Integer; dispid 107;
    function LLGetOptoDCJogParams(lChanID: Integer; var plJogMode: Integer; 
                                  var plJogStepSize: Integer; var plMinVel: Integer; 
                                  var plAccn: Integer; var plMaxVel: Integer): Integer; dispid 108;
    function LLGetOptoDCHomeParams(lChanID: Integer; var plDirection: Integer; 
                                   var plLimSwitch: Integer; var plHomeVel: Integer; 
                                   var plOffsetDist: Integer): Integer; dispid 109;
    function GetParentHWInfo(var plHWSerialNum: Integer; var plHWType: Integer): Integer; dispid 110;
    function SetButtonParams(lChanID: Integer; lButMode: Integer; fLeftButPos: Single; 
                             fRightButPos: Single): Integer; dispid 111;
    function GetButtonParams(lChanID: Integer; var plButMode: Integer; var pfLeftButPos: Single; 
                             var pfRightButPos: Single): Integer; dispid 112;
    function SetPotParams(lChanID: Integer; lVel1PotVal: Integer; fVel1: Single; 
                          lVel2PotVal: Integer; fVel2: Single; lVel3PotVal: Integer; fVel3: Single; 
                          lVel4PotVal: Integer; fVel4: Single): Integer; dispid 113;
    function GetPotParams(lChanID: Integer; var plVel1PotVal: Integer; var pfVel1: Single; 
                          var plVel2PotVal: Integer; var pfVel2: Single; var plVel3PotVal: Integer; 
                          var pfVel3: Single; var plVel4PotVal: Integer; var pfVel4: Single): Integer; dispid 114;
    function LLSetEEPROMParams(lChanID: Integer; lMsgID: Integer): Integer; dispid 115;
    function ShowSettingsDlg: Integer; dispid 119;
    function GetCtrlStarted(var pbStarted: WordBool): Integer; dispid 120;
    function GetHWCommsOK(var pbCommsOK: WordBool): Integer; dispid 121;
    function SetPositionOffset(lChanID: Integer; fPosOffset: Single): Integer; dispid 126;
    function LLSetGUIEnable(bEnable: WordBool): Integer; dispid 127;
    function LLSetContextMenuEnable(bEnabled: WordBool): Integer; dispid 128;
    function LLSetPosDispClickEventEnable(bEnabled: WordBool): Integer; dispid 129;
    function LLSetPosDispDblClickEventEnable(bEnabled: WordBool): Integer; dispid 130;
    function GetPositionOffset(lChanID: Integer; var pfPosOffset: Single): Integer; dispid 131;
    function SetDispMode(lDispMode: Integer): Integer; dispid 132;
    function GetDispMode(var plDispMode: Integer): Integer; dispid 133;
    function SetChannelSwitch(lChanID: Integer): Integer; dispid 134;
    function GetChannelSwitch(var plChanID: Integer): Integer; dispid 135;
    function MoveAbsoluteRot(lChanID: Integer; fAnglePosCh1: Single; fAnglePosCh2: Single; 
                             lMoveMode: Integer; bWait: WordBool): Integer; dispid 136;
    function GetVelParams_Accn(lChanID: Integer): Single; dispid 137;
    function GetVelParams_MaxVel(lChanID: Integer): Single; dispid 138;
    function GetStageAxisInfo_MinPos(lChanID: Integer): Single; dispid 139;
    function GetStageAxisInfo_MaxPos(lChanID: Integer): Single; dispid 140;
    function GetRelMoveDist_RelDist(lChanID: Integer): Single; dispid 141;
    function GetAbsMovePos_AbsPos(lChanID: Integer): Single; dispid 142;
    function GetBLashDist_BLashDist(lChanID: Integer): Single; dispid 143;
    function GetPosition_Position(lChanID: Integer): Single; dispid 144;
    function GetPositionEx_UncalibPosition(lChanID: Integer): Single; dispid 145;
    function GetJogMode_Mode(lChanID: Integer): Integer; dispid 146;
    function GetJogMode_StopMode(lChanID: Integer): Integer; dispid 147;
    function GetJogStepSize_StepSize(lChanID: Integer): Single; dispid 148;
    function GetStatusBits_Bits(lChanID: Integer): Integer; dispid 149;
    function GetJogVelParams_Accn(lChanID: Integer): Single; dispid 150;
    function GetJogVelParams_MaxVel(lChanID: Integer): Single; dispid 151;
    function GetHomeParams_HomeVel(lChanID: Integer): Single; dispid 152;
    function GetHomeParams_ZeroOffset(lChanID: Integer): Single; dispid 153;
    function GetPIDParams_Prop(lChanID: Integer): Integer; dispid 154;
    function GetPIDParams_Int(lChanID: Integer): Integer; dispid 155;
    function GetPIDParams_Deriv(lChanID: Integer): Integer; dispid 156;
    function SC_SetOperatingMode(lChanID: Integer; lMode: Integer): Integer; dispid 157;
    function SC_GetOperatingMode(lChanID: Integer; var plMode: Integer): Integer; dispid 158;
    function SC_SetCycleParams(lChanID: Integer; fOnTime: Single; fOffTime: Single; 
                               lNumCycles: Integer): Integer; dispid 159;
    function SC_GetCycleParams(lChanID: Integer; var pfOnTime: Single; var pfOffTime: Single; 
                               var plNumCycles: Integer): Integer; dispid 160;
    function SC_Enable(lChanID: Integer): Integer; dispid 163;
    function SC_Disable(lChanID: Integer): Integer; dispid 164;
    function SC_GetOPState(lChanID: Integer; var plState: Integer): Integer; dispid 165;
    function SetBrakeState(lChanID: Integer; lState: Integer): Integer; dispid 166;
    function GetBrakeState(lChanID: Integer; var plState: Integer): Integer; dispid 167;
    function LLSuspendEndOfMoveMsgs: Integer; dispid 168;
    function LLResumeEndOfMoveMsgs: Integer; dispid 169;
    function EnableEventDlg(bEnable: WordBool): Integer; dispid 170;
    function ShowEventDlg: Integer; dispid 171;
    function SetRotStageModes(lChanID: Integer; lMoveMode: Integer; lPosReportMode: Integer): Integer; dispid 172;
    function GetRotStageModes(lChanID: Integer; var plMoveMode: Integer; 
                              var plPosReportMode: Integer): Integer; dispid 173;
    function LLSetPMDParam(lCommandCode: Integer; lParamCode: Integer; lVal: Integer): Integer; dispid 174;
    function LLGetPMDParam(lCommandCode: Integer; lParamCode: Integer; var plVal: Integer): Integer; dispid 175;
    function SetDCCurrentLoopParams(lChanID: Integer; lProp: Integer; lInt: Integer; 
                                    lIntLim: Integer; lIntDeadBand: Integer; lFFwd: Integer): Integer; dispid 176;
    function GetDCCurrentLoopParams(lChanID: Integer; var plProp: Integer; var plInt: Integer; 
                                    var plIntLim: Integer; var plIntDeadBand: Integer; 
                                    var plFFwd: Integer): Integer; dispid 177;
    function SetDCPositionLoopParams(lChanID: Integer; lProp: Integer; lInt: Integer; 
                                     lIntLim: Integer; lDeriv: Integer; lDerivTime: Integer; 
                                     lLoopGain: Integer; lVelFFwd: Integer; lAccFFwd: Integer; 
                                     lPosErrLim: Integer): Integer; dispid 178;
    function GetDCPositionLoopParams(lChanID: Integer; var plProp: Integer; var plInt: Integer; 
                                     var plIntLim: Integer; var plDeriv: Integer; 
                                     var plDerivTime: Integer; var plLoopGain: Integer; 
                                     var plVelFFwd: Integer; var plAccFFwd: Integer; 
                                     var plPosErrLim: Integer): Integer; dispid 179;
    function SetDCMotorOutputParams(lChanID: Integer; fContCurrLim: Single; fEnergyLim: Single; 
                                    fMotorLim: Single; fMotorBias: Single): Integer; dispid 180;
    function GetDCMotorOutputParams(lChanID: Integer; var pfContCurrLim: Single; 
                                    var pfEnergyLim: Single; var pfMotorLim: Single; 
                                    var pfMotorBias: Single): Integer; dispid 181;
    function SetDCTrackSettleParams(lChanID: Integer; lSettleTime: Integer; lSettleWnd: Integer; 
                                    lTrackWnd: Integer): Integer; dispid 182;
    function GetDCTrackSettleParams(lChanID: Integer; var plSettleTime: Integer; 
                                    var plSettleWnd: Integer; var plTrackWnd: Integer): Integer; dispid 183;
    function SetDCProfileModeParams(lChanID: Integer; lProfMode: Integer; fJerk: Single): Integer; dispid 184;
    function GetDCProfileModeParams(lChanID: Integer; var plProfMode: Integer; var pfJerk: Single): Integer; dispid 185;
    function SetDCJoystickParams(lChanID: Integer; fMaxVelLO: Single; fMaxVelHI: Single; 
                                 fAccnLO: Single; fAccnHI: Single; lDirSense: Integer): Integer; dispid 186;
    function GetDCJoystickParams(lChanID: Integer; var pfMaxVelLO: Single; var pfMaxVelHI: Single; 
                                 var pfAccnLO: Single; var pfAccnHI: Single; var plDirSense: Integer): Integer; dispid 187;
    function SetDCSettledCurrentLoopParams(lChanID: Integer; lSettledProp: Integer; 
                                           lSettledInt: Integer; lSettledIntLim: Integer; 
                                           lSettledIntDeadBand: Integer; lSettledFFwd: Integer): Integer; dispid 188;
    function GetDCSettledCurrentLoopParams(lChanID: Integer; var plSettledProp: Integer; 
                                           var plSettledInt: Integer; var plSettledIntLim: Integer; 
                                           var plSettledIntDeadBand: Integer; 
                                           var plSettledFFwd: Integer): Integer; dispid 189;
    function SetBowIndex(lChanID: Integer; lBowIndex: Integer): Integer; dispid 190;
    function GetBowIndex(lChanID: Integer; var plBowIndex: Integer): Integer; dispid 191;
    function SetDCTriggerParams(lChanID: Integer; lTrigInMode: Integer; lTrigOutMode: Integer): Integer; dispid 192;
    function GetDCTriggerParams(lChanID: Integer; var plTrigInMode: Integer; 
                                var plTrigOutMode: Integer): Integer; dispid 193;
    function SetMFFOperParams(lChanID: Integer; lTransitTime: Integer; lDigIO1OperMode: Integer; 
                              lDigIO1SigMode: Integer; lDigIO1PulseWidth: Integer; 
                              lDigIO2OperMode: Integer; lDigIO2SigMode: Integer; 
                              lDigIO2PulseWidth: Integer): Integer; dispid 194;
    function GetMFFOperParams(lChanID: Integer; var plTransitTime: Integer; 
                              var plDigIO1OperMode: Integer; var plDigIO1SigMode: Integer; 
                              var plDigIO1PulseWidth: Integer; var plDigIO2OperMode: Integer; 
                              var plDigIO2SigMode: Integer; var plDigIO2PulseWidth: Integer): Integer; dispid 195;
    function LLSetPosition(lChanID: Integer; lPosition: Integer): Integer; dispid 196;
  end;

// *********************************************************************//
// DispIntf:  _DMG17MotorEvents
// Flags:     (4096) Dispatchable
// GUID:      {641DF21F-2956-4F93-B442-2CA591C73628}
// *********************************************************************//
  _DMG17MotorEvents = dispinterface
    ['{641DF21F-2956-4F93-B442-2CA591C73628}']
    procedure MoveComplete(lChanID: Integer); dispid 2;
    procedure HomeComplete(lChanID: Integer); dispid 3;
    procedure MoveStopped(lChanID: Integer); dispid 4;
    procedure HWResponse(lHWCode: Integer; lMsgIdent: Integer); dispid 5;
    procedure SettingsChanged(lFlags: Integer); dispid 6;
    procedure EncCalibComplete(lChanID: Integer); dispid 9;
    procedure PositionClick(fCurrentPosition: Single); dispid 10;
    procedure PositionDblClick(fCurrentPosition: Single); dispid 11;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TMG17Motor
// Help String      : MGMotor Control
// Default Interface: _DMG17Motor
// Def. Intf. DISP? : Yes
// Event   Interface: _DMG17MotorEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TMG17MotorMoveComplete = procedure(ASender: TObject; lChanID: Integer) of object;
  TMG17MotorHomeComplete = procedure(ASender: TObject; lChanID: Integer) of object;
  TMG17MotorMoveStopped = procedure(ASender: TObject; lChanID: Integer) of object;
  TMG17MotorHWResponse = procedure(ASender: TObject; lHWCode: Integer; lMsgIdent: Integer) of object;
  TMG17MotorSettingsChanged = procedure(ASender: TObject; lFlags: Integer) of object;
  TMG17MotorEncCalibComplete = procedure(ASender: TObject; lChanID: Integer) of object;
  TMG17MotorPositionClick = procedure(ASender: TObject; fCurrentPosition: Single) of object;
  TMG17MotorPositionDblClick = procedure(ASender: TObject; fCurrentPosition: Single) of object;

  TMG17Motor = class(TOleControl)
  private
    FOnMoveComplete: TMG17MotorMoveComplete;
    FOnHomeComplete: TMG17MotorHomeComplete;
    FOnMoveStopped: TMG17MotorMoveStopped;
    FOnHWResponse: TMG17MotorHWResponse;
    FOnSettingsChanged: TMG17MotorSettingsChanged;
    FOnEncCalibComplete: TMG17MotorEncCalibComplete;
    FOnPositionClick: TMG17MotorPositionClick;
    FOnPositionDblClick: TMG17MotorPositionDblClick;
    FIntf: _DMG17Motor;
    function  GetControlInterface: _DMG17Motor;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    procedure AboutBox;
    function StartCtrl: Integer;
    function StopCtrl: Integer;
    function GetPosition(lChanID: Integer; var pfPosition: Single): Integer;
    function LLSetGetPosParams(bSet: WordBool; lChanID: Integer; var plPosControlMode: Integer; 
                               var plMicroStepDivider: Integer; var plPosStepRes: Integer): Integer;
    function LLSetGetVelParams(bSet: WordBool; lChanID: Integer; var plMinVel: Integer; 
                               var plAccn: Integer; var plMaxVel: Integer): Integer;
    function LLSetGetJogParams(bSet: WordBool; lChanID: Integer; var plJogMode: Integer; 
                               var plJogStepSize: Integer; var plMinVel: Integer; 
                               var plAccn: Integer; var plMaxVel: Integer): Integer;
    function LLSetGetLimSwitchParams(bSet: WordBool; lChanID: Integer; var plCWHardLimit: Integer; 
                                     var plCCWHardLimit: Integer; var plCWSoftLimit: Integer; 
                                     var plCCWSoftLimit: Integer; var plSoftLimitMode: Integer): Integer;
    function LLSetGetPowerParams(bSet: WordBool; lChanID: Integer; var plRestFactor: Integer; 
                                 var plMoveFactor: Integer): Integer;
    function LLSetGetHomeParams(bSet: WordBool; lChanID: Integer; var plDirection: Integer; 
                                var plLimSwitch: Integer; var plHomeVel: Integer; 
                                var plOffsetDist: Integer): Integer;
    function LLSetGetMoveRelParams(bSet: WordBool; lChanID: Integer; var plRelDist: Integer): Integer;
    function LLSetGetMoveAbsParams(bSet: WordBool; lChanID: Integer; var plAbsPos: Integer): Integer;
    function MoveVelocity(lChanID: Integer; lDirection: Integer): Integer;
    function LLMoveStop(lChanID: Integer; lStopMode: Integer): Integer;
    function LLReqHWParams(lChanID: Integer; lParamID: Integer): Integer;
    function EnableHWChannel(lChanID: Integer): Integer;
    function DisableHWChannel(lChanID: Integer): Integer;
    function LLSetGetDevParams(bSet: WordBool; var plDevParams: Integer): Integer;
    function LLSetGetDevParamsEx(bSet: WordBool; const plDevParams: OleVariant): Integer;
    function MoveHome(lChanID: Integer; bWait: WordBool): Integer;
    function StopImmediate(lChanID: Integer): Integer;
    function StopProfiled(lChanID: Integer): Integer;
    function MoveRelativeEx(lChanID: Integer; fRelDistCh1: Single; fRelDistCh2: Single; 
                            bWait: WordBool): Integer;
    function MoveAbsoluteEx(lChanID: Integer; fAbsPosCh1: Single; fAbsPosCh2: Single; 
                            bWait: WordBool): Integer;
    function MoveRelative(lChanID: Integer; bWait: WordBool): Integer;
    function MoveAbsolute(lChanID: Integer; bWait: WordBool): Integer;
    function LLGetStatusBits(lChanID: Integer; var plStatusBits: Integer): Integer;
    function LLSetGetGenMoveParams(bSet: WordBool; lChanID: Integer; var plBLashDist: Integer): Integer;
    function SetVelParams(lChanID: Integer; fMinVel: Single; fAccn: Single; fMaxVel: Single): Integer;
    function GetVelParams(lChanID: Integer; var pfMinVel: Single; var pfAccn: Single; 
                          var pfMaxVel: Single): Integer;
    function GetStageAxis(lChanID: Integer; var pbstrStageAxisName: WideString; 
                          var plStageID: Integer; var plAxisID: Integer): Integer;
    function GetStageAxisInfo(lChanID: Integer; var pfMinPos: Single; var pfMaxPos: Single; 
                              var plUnits: Integer; var pfPitch: Single; var plDirSense: Integer): Integer;
    function GetVelParamLimits(lChanID: Integer; var pfMaxAccn: Single; var pfMaxVel: Single): Integer;
    function SetJogMode(lChanID: Integer; lMode: Integer; lStopMode: Integer): Integer;
    function GetJogMode(lChanID: Integer; var plMode: Integer; var plStopMode: Integer): Integer;
    function SetJogStepSize(lChanID: Integer; fStepSize: Single): Integer;
    function GetJogStepSize(lChanID: Integer; var pfStepSize: Single): Integer;
    function SetJogVelParams(lChanID: Integer; fMinVel: Single; fAccn: Single; fMaxVel: Single): Integer;
    function GetJogVelParams(lChanID: Integer; var pfMinVel: Single; var pfAccn: Single; 
                             var pfMaxVel: Single): Integer;
    function SetRelMoveDist(lChanID: Integer; fRelDist: Single): Integer;
    function GetRelMoveDist(lChanID: Integer; var pfRelDist: Single): Integer;
    function SetAbsMovePos(lChanID: Integer; fAbsPos: Single): Integer;
    function GetAbsMovePos(lChanID: Integer; var pfAbsPos: Single): Integer;
    function LLSetGetDigOPs(bSet: WordBool; var plBits: Integer): Integer;
    function LLGetDigIPs(var plBits: Integer): Integer;
    function LLGetADCInputs(var plADCVal1: Integer; var plADCVal2: Integer): Integer;
    function LLGetPosition(lChanID: Integer; var plPosition: Integer): Integer;
    function SetBLashDist(lChanID: Integer; fBLashDist: Single): Integer;
    function GetBLashDist(lChanID: Integer; var pfBLashDist: Single): Integer;
    function Identify: Integer;
    function SetSWPosLimits(lChanID: Integer; fRevPosLimit: Single; fFwdPosLimit: Single; 
                            lLimitMode: Integer): Integer;
    function GetSWPosLimits(lChanID: Integer; var pfRevPosLimit: Single; var pfFwdPosLimit: Single; 
                            var plLimitMode: Integer): Integer;
    function SetStageAxisInfo(lChanID: Integer; fMinPos: Single; fMaxPos: Single; lUnits: Integer; 
                              fPitch: Single; lDirSense: Integer): Integer;
    function LLSaveHWDefaults: Integer;
    function SetHomeParams(lChanID: Integer; lDirection: Integer; lLimSwitch: Integer; 
                           fHomeVel: Single; fZeroOffset: Single): Integer;
    function GetHomeParams(lChanID: Integer; var plDirection: Integer; var plLimSwitch: Integer; 
                           var pfHomeVel: Single; var pfZeroOffset: Single): Integer;
    function SetHWLimSwitches(lChanID: Integer; lRevLimSwitch: Integer; lFwdLimSwitch: Integer): Integer;
    function GetHWLimSwitches(lChanID: Integer; var plRevLimSwitch: Integer; 
                              var plFwdLimSwitch: Integer): Integer;
    function SetPhaseCurrents(lChanID: Integer; lRestVal: Integer; lMoveVal: Integer): Integer;
    function GetPhaseCurrents(lChanID: Integer; var plRestVal: Integer; var plMoveVal: Integer): Integer;
    function GetPositionEx(lChanID: Integer; var pfCalibPosition: Single; 
                           var pfUncalibPosition: Single): Integer;
    function DoEvents: Integer;
    function LLSetEncoderCount(lChanID: Integer; lEncCount: Integer): Integer;
    function LLGetEncoderCount(lChanID: Integer; var plEncCount: Integer): Integer;
    function LLSetGetTrigBits(bSet: WordBool; lChanID: Integer; var plBits: Integer): Integer;
    function MoveJog(lChanID: Integer; lJogDir: Integer): Integer;
    function SetMotorParams(lChanID: Integer; lStepsPerRev: Integer; lGearBoxRatio: Integer): Integer;
    function GetMotorParams(lChanID: Integer; var plStepsPerRev: Integer; 
                            var plGearBoxRatio: Integer): Integer;
    function SetTriggerParams(lChanID: Integer; lTrigMode: Integer; lTrigMove: Integer): Integer;
    function GetTriggerParams(lChanID: Integer; var plTrigMode: Integer; var plTrigMove: Integer): Integer;
    function SaveParamSet(const bstrName: WideString): Integer;
    function LoadParamSet(const bstrName: WideString): Integer;
    function DeleteParamSet(const btsrName: WideString): Integer;
    function LLSetDSPProgState(bProg: WordBool): Integer;
    function LLSendDSPProgData(lNumBytes: Integer; var lpbyData: Byte): Integer;
    function MoveAbsoluteEnc(lChanID: Integer; fAbsPosCh1: Single; fAbsPosCh2: Single; 
                             lTimeInc: Integer; bWait: WordBool): Integer;
    function MoveRelativeEnc(lChanID: Integer; fRelDistCh1: Single; fRelDistCh2: Single; 
                             lTimeInc: Integer; bWait: WordBool): Integer;
    function CalibrateEnc(lChanID: Integer; bWait: WordBool): Integer;
    function SetEncCalibTableParams(lChanID: Integer; lEncCalib: Integer; fCalibStep: Single; 
                                    lCalibDwell: Integer; lQEPSense: Integer): Integer;
    function GetEncCalibTableParams(lChanID: Integer; var plEncCalib: Integer; 
                                    var pfCalibStep: Single; var plCalibDwell: Integer; 
                                    var plQEPSense: Integer): Integer;
    function SetEncPosCorrectParams(lChanID: Integer; lPosSetPtWnd: Integer; lSnguStepWnd: Integer; 
                                    lStopShortDist: Integer; lCorrMoveStep: Integer): Integer;
    function GetEncPosCorrectParams(lChanID: Integer; var plPosSetPtWnd: Integer; 
                                    var plSnguStepWnd: Integer; var plStopShortDist: Integer; 
                                    var plCorrMoveStep: Integer): Integer;
    function SetEncPosControlParams(lChanID: Integer; lPosSrcMode: Integer; lPosCorrMode: Integer; 
                                    bUseCalib: WordBool): Integer;
    function GetEncPosControlParams(lChanID: Integer; var plPosSrcMode: Integer; 
                                    var plPosCorrMode: Integer; var pbUseCalib: WordBool): Integer;
    function LLSetGetHostDigOPs(bSet: WordBool; var plBits: Integer): Integer;
    function LLGetHostStatusBits(var plStatusBits: Integer): Integer;
    function LLSetGetPIDParams(bSet: WordBool; lChanID: Integer; var plProp: Integer; 
                               var plInt: Integer; var plDeriv: Integer; var plIntLimit: Integer; 
                               var plFilterCtrl: Integer): Integer;
    function LLGetAdvancedDCParams(lChanID: Integer; var plTargetPos: Integer; 
                                   var plTargetVel: Integer; var plIntSum: Integer): Integer;
    function LLSetGetPotParams(bSet: WordBool; lChanID: Integer; var plZeroWnd: Integer; 
                               var plVel1: Integer; var plWnd1: Integer; var plVel2: Integer; 
                               var plWnd2: Integer; var plVel3: Integer; var plWnd3: Integer; 
                               var plVel4: Integer): Integer;
    function SetIndicatorLEDMode(lChanID: Integer; lLEDMode: Integer): Integer;
    function GetIndicatorLEDMode(lChanID: Integer; var plLEDMode: Integer): Integer;
    function LLSetGetAVMode(bSet: WordBool; lChanID: Integer; var plModeBits: Integer): Integer;
    function LLSetGetButtonParams(bSet: WordBool; lChanID: Integer; var plMode: Integer; 
                                  var plPos1: Integer; var plPos2: Integer; 
                                  var plTimeout1: Integer; var plTimeout2: Integer): Integer;
    function LLGetOptoDCVelParams(lChanID: Integer; var plMinVel: Integer; var plAccn: Integer; 
                                  var plMaxVel: Integer): Integer;
    function LLGetOptoDCJogParams(lChanID: Integer; var plJogMode: Integer; 
                                  var plJogStepSize: Integer; var plMinVel: Integer; 
                                  var plAccn: Integer; var plMaxVel: Integer): Integer;
    function LLGetOptoDCHomeParams(lChanID: Integer; var plDirection: Integer; 
                                   var plLimSwitch: Integer; var plHomeVel: Integer; 
                                   var plOffsetDist: Integer): Integer;
    function GetParentHWInfo(var plHWSerialNum: Integer; var plHWType: Integer): Integer;
    function SetButtonParams(lChanID: Integer; lButMode: Integer; fLeftButPos: Single; 
                             fRightButPos: Single): Integer;
    function GetButtonParams(lChanID: Integer; var plButMode: Integer; var pfLeftButPos: Single; 
                             var pfRightButPos: Single): Integer;
    function SetPotParams(lChanID: Integer; lVel1PotVal: Integer; fVel1: Single; 
                          lVel2PotVal: Integer; fVel2: Single; lVel3PotVal: Integer; fVel3: Single; 
                          lVel4PotVal: Integer; fVel4: Single): Integer;
    function GetPotParams(lChanID: Integer; var plVel1PotVal: Integer; var pfVel1: Single; 
                          var plVel2PotVal: Integer; var pfVel2: Single; var plVel3PotVal: Integer; 
                          var pfVel3: Single; var plVel4PotVal: Integer; var pfVel4: Single): Integer;
    function LLSetEEPROMParams(lChanID: Integer; lMsgID: Integer): Integer;
    function ShowSettingsDlg: Integer;
    function GetCtrlStarted(var pbStarted: WordBool): Integer;
    function GetHWCommsOK(var pbCommsOK: WordBool): Integer;
    function SetPositionOffset(lChanID: Integer; fPosOffset: Single): Integer;
    function LLSetGUIEnable(bEnable: WordBool): Integer;
    function LLSetContextMenuEnable(bEnabled: WordBool): Integer;
    function LLSetPosDispClickEventEnable(bEnabled: WordBool): Integer;
    function LLSetPosDispDblClickEventEnable(bEnabled: WordBool): Integer;
    function GetPositionOffset(lChanID: Integer; var pfPosOffset: Single): Integer;
    function SetDispMode(lDispMode: Integer): Integer;
    function GetDispMode(var plDispMode: Integer): Integer;
    function SetChannelSwitch(lChanID: Integer): Integer;
    function GetChannelSwitch(var plChanID: Integer): Integer;
    function MoveAbsoluteRot(lChanID: Integer; fAnglePosCh1: Single; fAnglePosCh2: Single; 
                             lMoveMode: Integer; bWait: WordBool): Integer;
    function GetVelParams_Accn(lChanID: Integer): Single;
    function GetVelParams_MaxVel(lChanID: Integer): Single;
    function GetStageAxisInfo_MinPos(lChanID: Integer): Single;
    function GetStageAxisInfo_MaxPos(lChanID: Integer): Single;
    function GetRelMoveDist_RelDist(lChanID: Integer): Single;
    function GetAbsMovePos_AbsPos(lChanID: Integer): Single;
    function GetBLashDist_BLashDist(lChanID: Integer): Single;
    function GetPosition_Position(lChanID: Integer): Single;
    function GetPositionEx_UncalibPosition(lChanID: Integer): Single;
    function GetJogMode_Mode(lChanID: Integer): Integer;
    function GetJogMode_StopMode(lChanID: Integer): Integer;
    function GetJogStepSize_StepSize(lChanID: Integer): Single;
    function GetStatusBits_Bits(lChanID: Integer): Integer;
    function GetJogVelParams_Accn(lChanID: Integer): Single;
    function GetJogVelParams_MaxVel(lChanID: Integer): Single;
    function GetHomeParams_HomeVel(lChanID: Integer): Single;
    function GetHomeParams_ZeroOffset(lChanID: Integer): Single;
    function GetPIDParams_Prop(lChanID: Integer): Integer;
    function GetPIDParams_Int(lChanID: Integer): Integer;
    function GetPIDParams_Deriv(lChanID: Integer): Integer;
    function SC_SetOperatingMode(lChanID: Integer; lMode: Integer): Integer;
    function SC_GetOperatingMode(lChanID: Integer; var plMode: Integer): Integer;
    function SC_SetCycleParams(lChanID: Integer; fOnTime: Single; fOffTime: Single; 
                               lNumCycles: Integer): Integer;
    function SC_GetCycleParams(lChanID: Integer; var pfOnTime: Single; var pfOffTime: Single; 
                               var plNumCycles: Integer): Integer;
    function SC_Enable(lChanID: Integer): Integer;
    function SC_Disable(lChanID: Integer): Integer;
    function SC_GetOPState(lChanID: Integer; var plState: Integer): Integer;
    function SetBrakeState(lChanID: Integer; lState: Integer): Integer;
    function GetBrakeState(lChanID: Integer; var plState: Integer): Integer;
    function LLSuspendEndOfMoveMsgs: Integer;
    function LLResumeEndOfMoveMsgs: Integer;
    function EnableEventDlg(bEnable: WordBool): Integer;
    function ShowEventDlg: Integer;
    function SetRotStageModes(lChanID: Integer; lMoveMode: Integer; lPosReportMode: Integer): Integer;
    function GetRotStageModes(lChanID: Integer; var plMoveMode: Integer; 
                              var plPosReportMode: Integer): Integer;
    function LLSetPMDParam(lCommandCode: Integer; lParamCode: Integer; lVal: Integer): Integer;
    function LLGetPMDParam(lCommandCode: Integer; lParamCode: Integer; var plVal: Integer): Integer;
    function SetDCCurrentLoopParams(lChanID: Integer; lProp: Integer; lInt: Integer; 
                                    lIntLim: Integer; lIntDeadBand: Integer; lFFwd: Integer): Integer;
    function GetDCCurrentLoopParams(lChanID: Integer; var plProp: Integer; var plInt: Integer; 
                                    var plIntLim: Integer; var plIntDeadBand: Integer; 
                                    var plFFwd: Integer): Integer;
    function SetDCPositionLoopParams(lChanID: Integer; lProp: Integer; lInt: Integer; 
                                     lIntLim: Integer; lDeriv: Integer; lDerivTime: Integer; 
                                     lLoopGain: Integer; lVelFFwd: Integer; lAccFFwd: Integer; 
                                     lPosErrLim: Integer): Integer;
    function GetDCPositionLoopParams(lChanID: Integer; var plProp: Integer; var plInt: Integer; 
                                     var plIntLim: Integer; var plDeriv: Integer; 
                                     var plDerivTime: Integer; var plLoopGain: Integer; 
                                     var plVelFFwd: Integer; var plAccFFwd: Integer; 
                                     var plPosErrLim: Integer): Integer;
    function SetDCMotorOutputParams(lChanID: Integer; fContCurrLim: Single; fEnergyLim: Single; 
                                    fMotorLim: Single; fMotorBias: Single): Integer;
    function GetDCMotorOutputParams(lChanID: Integer; var pfContCurrLim: Single; 
                                    var pfEnergyLim: Single; var pfMotorLim: Single; 
                                    var pfMotorBias: Single): Integer;
    function SetDCTrackSettleParams(lChanID: Integer; lSettleTime: Integer; lSettleWnd: Integer; 
                                    lTrackWnd: Integer): Integer;
    function GetDCTrackSettleParams(lChanID: Integer; var plSettleTime: Integer; 
                                    var plSettleWnd: Integer; var plTrackWnd: Integer): Integer;
    function SetDCProfileModeParams(lChanID: Integer; lProfMode: Integer; fJerk: Single): Integer;
    function GetDCProfileModeParams(lChanID: Integer; var plProfMode: Integer; var pfJerk: Single): Integer;
    function SetDCJoystickParams(lChanID: Integer; fMaxVelLO: Single; fMaxVelHI: Single; 
                                 fAccnLO: Single; fAccnHI: Single; lDirSense: Integer): Integer;
    function GetDCJoystickParams(lChanID: Integer; var pfMaxVelLO: Single; var pfMaxVelHI: Single; 
                                 var pfAccnLO: Single; var pfAccnHI: Single; var plDirSense: Integer): Integer;
    function SetDCSettledCurrentLoopParams(lChanID: Integer; lSettledProp: Integer; 
                                           lSettledInt: Integer; lSettledIntLim: Integer; 
                                           lSettledIntDeadBand: Integer; lSettledFFwd: Integer): Integer;
    function GetDCSettledCurrentLoopParams(lChanID: Integer; var plSettledProp: Integer; 
                                           var plSettledInt: Integer; var plSettledIntLim: Integer; 
                                           var plSettledIntDeadBand: Integer; 
                                           var plSettledFFwd: Integer): Integer;
    function SetBowIndex(lChanID: Integer; lBowIndex: Integer): Integer;
    function GetBowIndex(lChanID: Integer; var plBowIndex: Integer): Integer;
    function SetDCTriggerParams(lChanID: Integer; lTrigInMode: Integer; lTrigOutMode: Integer): Integer;
    function GetDCTriggerParams(lChanID: Integer; var plTrigInMode: Integer; 
                                var plTrigOutMode: Integer): Integer;
    function SetMFFOperParams(lChanID: Integer; lTransitTime: Integer; lDigIO1OperMode: Integer; 
                              lDigIO1SigMode: Integer; lDigIO1PulseWidth: Integer; 
                              lDigIO2OperMode: Integer; lDigIO2SigMode: Integer; 
                              lDigIO2PulseWidth: Integer): Integer;
    function GetMFFOperParams(lChanID: Integer; var plTransitTime: Integer; 
                              var plDigIO1OperMode: Integer; var plDigIO1SigMode: Integer; 
                              var plDigIO1PulseWidth: Integer; var plDigIO2OperMode: Integer; 
                              var plDigIO2SigMode: Integer; var plDigIO2PulseWidth: Integer): Integer;
    function LLSetPosition(lChanID: Integer; lPosition: Integer): Integer;
    property  ControlInterface: _DMG17Motor read GetControlInterface;
    property  DefaultInterface: _DMG17Motor read GetControlInterface;
  published
    property Anchors;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property HWSerialNum: Integer index 4 read GetIntegerProp write SetIntegerProp stored False;
    property APTHelp: WordBool index 69 read GetWordBoolProp write SetWordBoolProp stored False;
    property DISPLAYMODE: Integer index 97 read GetIntegerProp write SetIntegerProp stored False;
    property OnMoveComplete: TMG17MotorMoveComplete read FOnMoveComplete write FOnMoveComplete;
    property OnHomeComplete: TMG17MotorHomeComplete read FOnHomeComplete write FOnHomeComplete;
    property OnMoveStopped: TMG17MotorMoveStopped read FOnMoveStopped write FOnMoveStopped;
    property OnHWResponse: TMG17MotorHWResponse read FOnHWResponse write FOnHWResponse;
    property OnSettingsChanged: TMG17MotorSettingsChanged read FOnSettingsChanged write FOnSettingsChanged;
    property OnEncCalibComplete: TMG17MotorEncCalibComplete read FOnEncCalibComplete write FOnEncCalibComplete;
    property OnPositionClick: TMG17MotorPositionClick read FOnPositionClick write FOnPositionClick;
    property OnPositionDblClick: TMG17MotorPositionDblClick read FOnPositionDblClick write FOnPositionDblClick;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses System.Win.ComObj;

procedure TMG17Motor.InitControlData;
const
  CEventDispIDs: array [0..7] of DWORD = (
    $00000002, $00000003, $00000004, $00000005, $00000006, $00000009,
    $0000000A, $0000000B);
  CControlData: TControlData2 = (
    ClassID:      '{3CE35BF3-1E13-4D2C-8C0B-DEF6314420B3}';
    EventIID:     '{641DF21F-2956-4F93-B442-2CA591C73628}';
    EventCount:   8;
    EventDispIDs: @CEventDispIDs;
    LicenseKey:   nil (*HR:$80004005*);
    Flags:        $00000000;
    Version:      500);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := UIntPtr(@@FOnMoveComplete) - UIntPtr(Self);
end;

procedure TMG17Motor.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as _DMG17Motor;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TMG17Motor.GetControlInterface: _DMG17Motor;
begin
  CreateControl;
  Result := FIntf;
end;

procedure TMG17Motor.AboutBox;
begin
  DefaultInterface.AboutBox;
end;

function TMG17Motor.StartCtrl: Integer;
begin
  Result := DefaultInterface.StartCtrl;
end;

function TMG17Motor.StopCtrl: Integer;
begin
  Result := DefaultInterface.StopCtrl;
end;

function TMG17Motor.GetPosition(lChanID: Integer; var pfPosition: Single): Integer;
begin
  Result := DefaultInterface.GetPosition(lChanID, pfPosition);
end;

function TMG17Motor.LLSetGetPosParams(bSet: WordBool; lChanID: Integer; 
                                      var plPosControlMode: Integer; 
                                      var plMicroStepDivider: Integer; var plPosStepRes: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetPosParams(bSet, lChanID, plPosControlMode, plMicroStepDivider, 
                                               plPosStepRes);
end;

function TMG17Motor.LLSetGetVelParams(bSet: WordBool; lChanID: Integer; var plMinVel: Integer; 
                                      var plAccn: Integer; var plMaxVel: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetVelParams(bSet, lChanID, plMinVel, plAccn, plMaxVel);
end;

function TMG17Motor.LLSetGetJogParams(bSet: WordBool; lChanID: Integer; var plJogMode: Integer; 
                                      var plJogStepSize: Integer; var plMinVel: Integer; 
                                      var plAccn: Integer; var plMaxVel: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetJogParams(bSet, lChanID, plJogMode, plJogStepSize, plMinVel, 
                                               plAccn, plMaxVel);
end;

function TMG17Motor.LLSetGetLimSwitchParams(bSet: WordBool; lChanID: Integer; 
                                            var plCWHardLimit: Integer; 
                                            var plCCWHardLimit: Integer; 
                                            var plCWSoftLimit: Integer; 
                                            var plCCWSoftLimit: Integer; 
                                            var plSoftLimitMode: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetLimSwitchParams(bSet, lChanID, plCWHardLimit, plCCWHardLimit, 
                                                     plCWSoftLimit, plCCWSoftLimit, plSoftLimitMode);
end;

function TMG17Motor.LLSetGetPowerParams(bSet: WordBool; lChanID: Integer; 
                                        var plRestFactor: Integer; var plMoveFactor: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetPowerParams(bSet, lChanID, plRestFactor, plMoveFactor);
end;

function TMG17Motor.LLSetGetHomeParams(bSet: WordBool; lChanID: Integer; var plDirection: Integer; 
                                       var plLimSwitch: Integer; var plHomeVel: Integer; 
                                       var plOffsetDist: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetHomeParams(bSet, lChanID, plDirection, plLimSwitch, plHomeVel, 
                                                plOffsetDist);
end;

function TMG17Motor.LLSetGetMoveRelParams(bSet: WordBool; lChanID: Integer; var plRelDist: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetMoveRelParams(bSet, lChanID, plRelDist);
end;

function TMG17Motor.LLSetGetMoveAbsParams(bSet: WordBool; lChanID: Integer; var plAbsPos: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetMoveAbsParams(bSet, lChanID, plAbsPos);
end;

function TMG17Motor.MoveVelocity(lChanID: Integer; lDirection: Integer): Integer;
begin
  Result := DefaultInterface.MoveVelocity(lChanID, lDirection);
end;

function TMG17Motor.LLMoveStop(lChanID: Integer; lStopMode: Integer): Integer;
begin
  Result := DefaultInterface.LLMoveStop(lChanID, lStopMode);
end;

function TMG17Motor.LLReqHWParams(lChanID: Integer; lParamID: Integer): Integer;
begin
  Result := DefaultInterface.LLReqHWParams(lChanID, lParamID);
end;

function TMG17Motor.EnableHWChannel(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.EnableHWChannel(lChanID);
end;

function TMG17Motor.DisableHWChannel(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.DisableHWChannel(lChanID);
end;

function TMG17Motor.LLSetGetDevParams(bSet: WordBool; var plDevParams: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetDevParams(bSet, plDevParams);
end;

function TMG17Motor.LLSetGetDevParamsEx(bSet: WordBool; const plDevParams: OleVariant): Integer;
begin
  Result := DefaultInterface.LLSetGetDevParamsEx(bSet, plDevParams);
end;

function TMG17Motor.MoveHome(lChanID: Integer; bWait: WordBool): Integer;
begin
  Result := DefaultInterface.MoveHome(lChanID, bWait);
end;

function TMG17Motor.StopImmediate(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.StopImmediate(lChanID);
end;

function TMG17Motor.StopProfiled(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.StopProfiled(lChanID);
end;

function TMG17Motor.MoveRelativeEx(lChanID: Integer; fRelDistCh1: Single; fRelDistCh2: Single; 
                                   bWait: WordBool): Integer;
begin
  Result := DefaultInterface.MoveRelativeEx(lChanID, fRelDistCh1, fRelDistCh2, bWait);
end;

function TMG17Motor.MoveAbsoluteEx(lChanID: Integer; fAbsPosCh1: Single; fAbsPosCh2: Single; 
                                   bWait: WordBool): Integer;
begin
  Result := DefaultInterface.MoveAbsoluteEx(lChanID, fAbsPosCh1, fAbsPosCh2, bWait);
end;

function TMG17Motor.MoveRelative(lChanID: Integer; bWait: WordBool): Integer;
begin
  Result := DefaultInterface.MoveRelative(lChanID, bWait);
end;

function TMG17Motor.MoveAbsolute(lChanID: Integer; bWait: WordBool): Integer;
begin
  Result := DefaultInterface.MoveAbsolute(lChanID, bWait);
end;

function TMG17Motor.LLGetStatusBits(lChanID: Integer; var plStatusBits: Integer): Integer;
begin
  Result := DefaultInterface.LLGetStatusBits(lChanID, plStatusBits);
end;

function TMG17Motor.LLSetGetGenMoveParams(bSet: WordBool; lChanID: Integer; var plBLashDist: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetGenMoveParams(bSet, lChanID, plBLashDist);
end;

function TMG17Motor.SetVelParams(lChanID: Integer; fMinVel: Single; fAccn: Single; fMaxVel: Single): Integer;
begin
  Result := DefaultInterface.SetVelParams(lChanID, fMinVel, fAccn, fMaxVel);
end;

function TMG17Motor.GetVelParams(lChanID: Integer; var pfMinVel: Single; var pfAccn: Single; 
                                 var pfMaxVel: Single): Integer;
begin
  Result := DefaultInterface.GetVelParams(lChanID, pfMinVel, pfAccn, pfMaxVel);
end;

function TMG17Motor.GetStageAxis(lChanID: Integer; var pbstrStageAxisName: WideString; 
                                 var plStageID: Integer; var plAxisID: Integer): Integer;
begin
  Result := DefaultInterface.GetStageAxis(lChanID, pbstrStageAxisName, plStageID, plAxisID);
end;

function TMG17Motor.GetStageAxisInfo(lChanID: Integer; var pfMinPos: Single; var pfMaxPos: Single; 
                                     var plUnits: Integer; var pfPitch: Single; 
                                     var plDirSense: Integer): Integer;
begin
  Result := DefaultInterface.GetStageAxisInfo(lChanID, pfMinPos, pfMaxPos, plUnits, pfPitch, 
                                              plDirSense);
end;

function TMG17Motor.GetVelParamLimits(lChanID: Integer; var pfMaxAccn: Single; var pfMaxVel: Single): Integer;
begin
  Result := DefaultInterface.GetVelParamLimits(lChanID, pfMaxAccn, pfMaxVel);
end;

function TMG17Motor.SetJogMode(lChanID: Integer; lMode: Integer; lStopMode: Integer): Integer;
begin
  Result := DefaultInterface.SetJogMode(lChanID, lMode, lStopMode);
end;

function TMG17Motor.GetJogMode(lChanID: Integer; var plMode: Integer; var plStopMode: Integer): Integer;
begin
  Result := DefaultInterface.GetJogMode(lChanID, plMode, plStopMode);
end;

function TMG17Motor.SetJogStepSize(lChanID: Integer; fStepSize: Single): Integer;
begin
  Result := DefaultInterface.SetJogStepSize(lChanID, fStepSize);
end;

function TMG17Motor.GetJogStepSize(lChanID: Integer; var pfStepSize: Single): Integer;
begin
  Result := DefaultInterface.GetJogStepSize(lChanID, pfStepSize);
end;

function TMG17Motor.SetJogVelParams(lChanID: Integer; fMinVel: Single; fAccn: Single; 
                                    fMaxVel: Single): Integer;
begin
  Result := DefaultInterface.SetJogVelParams(lChanID, fMinVel, fAccn, fMaxVel);
end;

function TMG17Motor.GetJogVelParams(lChanID: Integer; var pfMinVel: Single; var pfAccn: Single; 
                                    var pfMaxVel: Single): Integer;
begin
  Result := DefaultInterface.GetJogVelParams(lChanID, pfMinVel, pfAccn, pfMaxVel);
end;

function TMG17Motor.SetRelMoveDist(lChanID: Integer; fRelDist: Single): Integer;
begin
  Result := DefaultInterface.SetRelMoveDist(lChanID, fRelDist);
end;

function TMG17Motor.GetRelMoveDist(lChanID: Integer; var pfRelDist: Single): Integer;
begin
  Result := DefaultInterface.GetRelMoveDist(lChanID, pfRelDist);
end;

function TMG17Motor.SetAbsMovePos(lChanID: Integer; fAbsPos: Single): Integer;
begin
  Result := DefaultInterface.SetAbsMovePos(lChanID, fAbsPos);
end;

function TMG17Motor.GetAbsMovePos(lChanID: Integer; var pfAbsPos: Single): Integer;
begin
  Result := DefaultInterface.GetAbsMovePos(lChanID, pfAbsPos);
end;

function TMG17Motor.LLSetGetDigOPs(bSet: WordBool; var plBits: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetDigOPs(bSet, plBits);
end;

function TMG17Motor.LLGetDigIPs(var plBits: Integer): Integer;
begin
  Result := DefaultInterface.LLGetDigIPs(plBits);
end;

function TMG17Motor.LLGetADCInputs(var plADCVal1: Integer; var plADCVal2: Integer): Integer;
begin
  Result := DefaultInterface.LLGetADCInputs(plADCVal1, plADCVal2);
end;

function TMG17Motor.LLGetPosition(lChanID: Integer; var plPosition: Integer): Integer;
begin
  Result := DefaultInterface.LLGetPosition(lChanID, plPosition);
end;

function TMG17Motor.SetBLashDist(lChanID: Integer; fBLashDist: Single): Integer;
begin
  Result := DefaultInterface.SetBLashDist(lChanID, fBLashDist);
end;

function TMG17Motor.GetBLashDist(lChanID: Integer; var pfBLashDist: Single): Integer;
begin
  Result := DefaultInterface.GetBLashDist(lChanID, pfBLashDist);
end;

function TMG17Motor.Identify: Integer;
begin
  Result := DefaultInterface.Identify;
end;

function TMG17Motor.SetSWPosLimits(lChanID: Integer; fRevPosLimit: Single; fFwdPosLimit: Single; 
                                   lLimitMode: Integer): Integer;
begin
  Result := DefaultInterface.SetSWPosLimits(lChanID, fRevPosLimit, fFwdPosLimit, lLimitMode);
end;

function TMG17Motor.GetSWPosLimits(lChanID: Integer; var pfRevPosLimit: Single; 
                                   var pfFwdPosLimit: Single; var plLimitMode: Integer): Integer;
begin
  Result := DefaultInterface.GetSWPosLimits(lChanID, pfRevPosLimit, pfFwdPosLimit, plLimitMode);
end;

function TMG17Motor.SetStageAxisInfo(lChanID: Integer; fMinPos: Single; fMaxPos: Single; 
                                     lUnits: Integer; fPitch: Single; lDirSense: Integer): Integer;
begin
  Result := DefaultInterface.SetStageAxisInfo(lChanID, fMinPos, fMaxPos, lUnits, fPitch, lDirSense);
end;

function TMG17Motor.LLSaveHWDefaults: Integer;
begin
  Result := DefaultInterface.LLSaveHWDefaults;
end;

function TMG17Motor.SetHomeParams(lChanID: Integer; lDirection: Integer; lLimSwitch: Integer; 
                                  fHomeVel: Single; fZeroOffset: Single): Integer;
begin
  Result := DefaultInterface.SetHomeParams(lChanID, lDirection, lLimSwitch, fHomeVel, fZeroOffset);
end;

function TMG17Motor.GetHomeParams(lChanID: Integer; var plDirection: Integer; 
                                  var plLimSwitch: Integer; var pfHomeVel: Single; 
                                  var pfZeroOffset: Single): Integer;
begin
  Result := DefaultInterface.GetHomeParams(lChanID, plDirection, plLimSwitch, pfHomeVel, 
                                           pfZeroOffset);
end;

function TMG17Motor.SetHWLimSwitches(lChanID: Integer; lRevLimSwitch: Integer; 
                                     lFwdLimSwitch: Integer): Integer;
begin
  Result := DefaultInterface.SetHWLimSwitches(lChanID, lRevLimSwitch, lFwdLimSwitch);
end;

function TMG17Motor.GetHWLimSwitches(lChanID: Integer; var plRevLimSwitch: Integer; 
                                     var plFwdLimSwitch: Integer): Integer;
begin
  Result := DefaultInterface.GetHWLimSwitches(lChanID, plRevLimSwitch, plFwdLimSwitch);
end;

function TMG17Motor.SetPhaseCurrents(lChanID: Integer; lRestVal: Integer; lMoveVal: Integer): Integer;
begin
  Result := DefaultInterface.SetPhaseCurrents(lChanID, lRestVal, lMoveVal);
end;

function TMG17Motor.GetPhaseCurrents(lChanID: Integer; var plRestVal: Integer; 
                                     var plMoveVal: Integer): Integer;
begin
  Result := DefaultInterface.GetPhaseCurrents(lChanID, plRestVal, plMoveVal);
end;

function TMG17Motor.GetPositionEx(lChanID: Integer; var pfCalibPosition: Single; 
                                  var pfUncalibPosition: Single): Integer;
begin
  Result := DefaultInterface.GetPositionEx(lChanID, pfCalibPosition, pfUncalibPosition);
end;

function TMG17Motor.DoEvents: Integer;
begin
  Result := DefaultInterface.DoEvents;
end;

function TMG17Motor.LLSetEncoderCount(lChanID: Integer; lEncCount: Integer): Integer;
begin
  Result := DefaultInterface.LLSetEncoderCount(lChanID, lEncCount);
end;

function TMG17Motor.LLGetEncoderCount(lChanID: Integer; var plEncCount: Integer): Integer;
begin
  Result := DefaultInterface.LLGetEncoderCount(lChanID, plEncCount);
end;

function TMG17Motor.LLSetGetTrigBits(bSet: WordBool; lChanID: Integer; var plBits: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetTrigBits(bSet, lChanID, plBits);
end;

function TMG17Motor.MoveJog(lChanID: Integer; lJogDir: Integer): Integer;
begin
  Result := DefaultInterface.MoveJog(lChanID, lJogDir);
end;

function TMG17Motor.SetMotorParams(lChanID: Integer; lStepsPerRev: Integer; lGearBoxRatio: Integer): Integer;
begin
  Result := DefaultInterface.SetMotorParams(lChanID, lStepsPerRev, lGearBoxRatio);
end;

function TMG17Motor.GetMotorParams(lChanID: Integer; var plStepsPerRev: Integer; 
                                   var plGearBoxRatio: Integer): Integer;
begin
  Result := DefaultInterface.GetMotorParams(lChanID, plStepsPerRev, plGearBoxRatio);
end;

function TMG17Motor.SetTriggerParams(lChanID: Integer; lTrigMode: Integer; lTrigMove: Integer): Integer;
begin
  Result := DefaultInterface.SetTriggerParams(lChanID, lTrigMode, lTrigMove);
end;

function TMG17Motor.GetTriggerParams(lChanID: Integer; var plTrigMode: Integer; 
                                     var plTrigMove: Integer): Integer;
begin
  Result := DefaultInterface.GetTriggerParams(lChanID, plTrigMode, plTrigMove);
end;

function TMG17Motor.SaveParamSet(const bstrName: WideString): Integer;
begin
  Result := DefaultInterface.SaveParamSet(bstrName);
end;

function TMG17Motor.LoadParamSet(const bstrName: WideString): Integer;
begin
  Result := DefaultInterface.LoadParamSet(bstrName);
end;

function TMG17Motor.DeleteParamSet(const btsrName: WideString): Integer;
begin
  Result := DefaultInterface.DeleteParamSet(btsrName);
end;

function TMG17Motor.LLSetDSPProgState(bProg: WordBool): Integer;
begin
  Result := DefaultInterface.LLSetDSPProgState(bProg);
end;

function TMG17Motor.LLSendDSPProgData(lNumBytes: Integer; var lpbyData: Byte): Integer;
begin
  Result := DefaultInterface.LLSendDSPProgData(lNumBytes, lpbyData);
end;

function TMG17Motor.MoveAbsoluteEnc(lChanID: Integer; fAbsPosCh1: Single; fAbsPosCh2: Single; 
                                    lTimeInc: Integer; bWait: WordBool): Integer;
begin
  Result := DefaultInterface.MoveAbsoluteEnc(lChanID, fAbsPosCh1, fAbsPosCh2, lTimeInc, bWait);
end;

function TMG17Motor.MoveRelativeEnc(lChanID: Integer; fRelDistCh1: Single; fRelDistCh2: Single; 
                                    lTimeInc: Integer; bWait: WordBool): Integer;
begin
  Result := DefaultInterface.MoveRelativeEnc(lChanID, fRelDistCh1, fRelDistCh2, lTimeInc, bWait);
end;

function TMG17Motor.CalibrateEnc(lChanID: Integer; bWait: WordBool): Integer;
begin
  Result := DefaultInterface.CalibrateEnc(lChanID, bWait);
end;

function TMG17Motor.SetEncCalibTableParams(lChanID: Integer; lEncCalib: Integer; 
                                           fCalibStep: Single; lCalibDwell: Integer; 
                                           lQEPSense: Integer): Integer;
begin
  Result := DefaultInterface.SetEncCalibTableParams(lChanID, lEncCalib, fCalibStep, lCalibDwell, 
                                                    lQEPSense);
end;

function TMG17Motor.GetEncCalibTableParams(lChanID: Integer; var plEncCalib: Integer; 
                                           var pfCalibStep: Single; var plCalibDwell: Integer; 
                                           var plQEPSense: Integer): Integer;
begin
  Result := DefaultInterface.GetEncCalibTableParams(lChanID, plEncCalib, pfCalibStep, plCalibDwell, 
                                                    plQEPSense);
end;

function TMG17Motor.SetEncPosCorrectParams(lChanID: Integer; lPosSetPtWnd: Integer; 
                                           lSnguStepWnd: Integer; lStopShortDist: Integer; 
                                           lCorrMoveStep: Integer): Integer;
begin
  Result := DefaultInterface.SetEncPosCorrectParams(lChanID, lPosSetPtWnd, lSnguStepWnd, 
                                                    lStopShortDist, lCorrMoveStep);
end;

function TMG17Motor.GetEncPosCorrectParams(lChanID: Integer; var plPosSetPtWnd: Integer; 
                                           var plSnguStepWnd: Integer; 
                                           var plStopShortDist: Integer; var plCorrMoveStep: Integer): Integer;
begin
  Result := DefaultInterface.GetEncPosCorrectParams(lChanID, plPosSetPtWnd, plSnguStepWnd, 
                                                    plStopShortDist, plCorrMoveStep);
end;

function TMG17Motor.SetEncPosControlParams(lChanID: Integer; lPosSrcMode: Integer; 
                                           lPosCorrMode: Integer; bUseCalib: WordBool): Integer;
begin
  Result := DefaultInterface.SetEncPosControlParams(lChanID, lPosSrcMode, lPosCorrMode, bUseCalib);
end;

function TMG17Motor.GetEncPosControlParams(lChanID: Integer; var plPosSrcMode: Integer; 
                                           var plPosCorrMode: Integer; var pbUseCalib: WordBool): Integer;
begin
  Result := DefaultInterface.GetEncPosControlParams(lChanID, plPosSrcMode, plPosCorrMode, pbUseCalib);
end;

function TMG17Motor.LLSetGetHostDigOPs(bSet: WordBool; var plBits: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetHostDigOPs(bSet, plBits);
end;

function TMG17Motor.LLGetHostStatusBits(var plStatusBits: Integer): Integer;
begin
  Result := DefaultInterface.LLGetHostStatusBits(plStatusBits);
end;

function TMG17Motor.LLSetGetPIDParams(bSet: WordBool; lChanID: Integer; var plProp: Integer; 
                                      var plInt: Integer; var plDeriv: Integer; 
                                      var plIntLimit: Integer; var plFilterCtrl: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetPIDParams(bSet, lChanID, plProp, plInt, plDeriv, plIntLimit, 
                                               plFilterCtrl);
end;

function TMG17Motor.LLGetAdvancedDCParams(lChanID: Integer; var plTargetPos: Integer; 
                                          var plTargetVel: Integer; var plIntSum: Integer): Integer;
begin
  Result := DefaultInterface.LLGetAdvancedDCParams(lChanID, plTargetPos, plTargetVel, plIntSum);
end;

function TMG17Motor.LLSetGetPotParams(bSet: WordBool; lChanID: Integer; var plZeroWnd: Integer; 
                                      var plVel1: Integer; var plWnd1: Integer; 
                                      var plVel2: Integer; var plWnd2: Integer; 
                                      var plVel3: Integer; var plWnd3: Integer; var plVel4: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetPotParams(bSet, lChanID, plZeroWnd, plVel1, plWnd1, plVel2, 
                                               plWnd2, plVel3, plWnd3, plVel4);
end;

function TMG17Motor.SetIndicatorLEDMode(lChanID: Integer; lLEDMode: Integer): Integer;
begin
  Result := DefaultInterface.SetIndicatorLEDMode(lChanID, lLEDMode);
end;

function TMG17Motor.GetIndicatorLEDMode(lChanID: Integer; var plLEDMode: Integer): Integer;
begin
  Result := DefaultInterface.GetIndicatorLEDMode(lChanID, plLEDMode);
end;

function TMG17Motor.LLSetGetAVMode(bSet: WordBool; lChanID: Integer; var plModeBits: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetAVMode(bSet, lChanID, plModeBits);
end;

function TMG17Motor.LLSetGetButtonParams(bSet: WordBool; lChanID: Integer; var plMode: Integer; 
                                         var plPos1: Integer; var plPos2: Integer; 
                                         var plTimeout1: Integer; var plTimeout2: Integer): Integer;
begin
  Result := DefaultInterface.LLSetGetButtonParams(bSet, lChanID, plMode, plPos1, plPos2, 
                                                  plTimeout1, plTimeout2);
end;

function TMG17Motor.LLGetOptoDCVelParams(lChanID: Integer; var plMinVel: Integer; 
                                         var plAccn: Integer; var plMaxVel: Integer): Integer;
begin
  Result := DefaultInterface.LLGetOptoDCVelParams(lChanID, plMinVel, plAccn, plMaxVel);
end;

function TMG17Motor.LLGetOptoDCJogParams(lChanID: Integer; var plJogMode: Integer; 
                                         var plJogStepSize: Integer; var plMinVel: Integer; 
                                         var plAccn: Integer; var plMaxVel: Integer): Integer;
begin
  Result := DefaultInterface.LLGetOptoDCJogParams(lChanID, plJogMode, plJogStepSize, plMinVel, 
                                                  plAccn, plMaxVel);
end;

function TMG17Motor.LLGetOptoDCHomeParams(lChanID: Integer; var plDirection: Integer; 
                                          var plLimSwitch: Integer; var plHomeVel: Integer; 
                                          var plOffsetDist: Integer): Integer;
begin
  Result := DefaultInterface.LLGetOptoDCHomeParams(lChanID, plDirection, plLimSwitch, plHomeVel, 
                                                   plOffsetDist);
end;

function TMG17Motor.GetParentHWInfo(var plHWSerialNum: Integer; var plHWType: Integer): Integer;
begin
  Result := DefaultInterface.GetParentHWInfo(plHWSerialNum, plHWType);
end;

function TMG17Motor.SetButtonParams(lChanID: Integer; lButMode: Integer; fLeftButPos: Single; 
                                    fRightButPos: Single): Integer;
begin
  Result := DefaultInterface.SetButtonParams(lChanID, lButMode, fLeftButPos, fRightButPos);
end;

function TMG17Motor.GetButtonParams(lChanID: Integer; var plButMode: Integer; 
                                    var pfLeftButPos: Single; var pfRightButPos: Single): Integer;
begin
  Result := DefaultInterface.GetButtonParams(lChanID, plButMode, pfLeftButPos, pfRightButPos);
end;

function TMG17Motor.SetPotParams(lChanID: Integer; lVel1PotVal: Integer; fVel1: Single; 
                                 lVel2PotVal: Integer; fVel2: Single; lVel3PotVal: Integer; 
                                 fVel3: Single; lVel4PotVal: Integer; fVel4: Single): Integer;
begin
  Result := DefaultInterface.SetPotParams(lChanID, lVel1PotVal, fVel1, lVel2PotVal, fVel2, 
                                          lVel3PotVal, fVel3, lVel4PotVal, fVel4);
end;

function TMG17Motor.GetPotParams(lChanID: Integer; var plVel1PotVal: Integer; var pfVel1: Single; 
                                 var plVel2PotVal: Integer; var pfVel2: Single; 
                                 var plVel3PotVal: Integer; var pfVel3: Single; 
                                 var plVel4PotVal: Integer; var pfVel4: Single): Integer;
begin
  Result := DefaultInterface.GetPotParams(lChanID, plVel1PotVal, pfVel1, plVel2PotVal, pfVel2, 
                                          plVel3PotVal, pfVel3, plVel4PotVal, pfVel4);
end;

function TMG17Motor.LLSetEEPROMParams(lChanID: Integer; lMsgID: Integer): Integer;
begin
  Result := DefaultInterface.LLSetEEPROMParams(lChanID, lMsgID);
end;

function TMG17Motor.ShowSettingsDlg: Integer;
begin
  Result := DefaultInterface.ShowSettingsDlg;
end;

function TMG17Motor.GetCtrlStarted(var pbStarted: WordBool): Integer;
begin
  Result := DefaultInterface.GetCtrlStarted(pbStarted);
end;

function TMG17Motor.GetHWCommsOK(var pbCommsOK: WordBool): Integer;
begin
  Result := DefaultInterface.GetHWCommsOK(pbCommsOK);
end;

function TMG17Motor.SetPositionOffset(lChanID: Integer; fPosOffset: Single): Integer;
begin
  Result := DefaultInterface.SetPositionOffset(lChanID, fPosOffset);
end;

function TMG17Motor.LLSetGUIEnable(bEnable: WordBool): Integer;
begin
  Result := DefaultInterface.LLSetGUIEnable(bEnable);
end;

function TMG17Motor.LLSetContextMenuEnable(bEnabled: WordBool): Integer;
begin
  Result := DefaultInterface.LLSetContextMenuEnable(bEnabled);
end;

function TMG17Motor.LLSetPosDispClickEventEnable(bEnabled: WordBool): Integer;
begin
  Result := DefaultInterface.LLSetPosDispClickEventEnable(bEnabled);
end;

function TMG17Motor.LLSetPosDispDblClickEventEnable(bEnabled: WordBool): Integer;
begin
  Result := DefaultInterface.LLSetPosDispDblClickEventEnable(bEnabled);
end;

function TMG17Motor.GetPositionOffset(lChanID: Integer; var pfPosOffset: Single): Integer;
begin
  Result := DefaultInterface.GetPositionOffset(lChanID, pfPosOffset);
end;

function TMG17Motor.SetDispMode(lDispMode: Integer): Integer;
begin
  Result := DefaultInterface.SetDispMode(lDispMode);
end;

function TMG17Motor.GetDispMode(var plDispMode: Integer): Integer;
begin
  Result := DefaultInterface.GetDispMode(plDispMode);
end;

function TMG17Motor.SetChannelSwitch(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.SetChannelSwitch(lChanID);
end;

function TMG17Motor.GetChannelSwitch(var plChanID: Integer): Integer;
begin
  Result := DefaultInterface.GetChannelSwitch(plChanID);
end;

function TMG17Motor.MoveAbsoluteRot(lChanID: Integer; fAnglePosCh1: Single; fAnglePosCh2: Single; 
                                    lMoveMode: Integer; bWait: WordBool): Integer;
begin
  Result := DefaultInterface.MoveAbsoluteRot(lChanID, fAnglePosCh1, fAnglePosCh2, lMoveMode, bWait);
end;

function TMG17Motor.GetVelParams_Accn(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetVelParams_Accn(lChanID);
end;

function TMG17Motor.GetVelParams_MaxVel(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetVelParams_MaxVel(lChanID);
end;

function TMG17Motor.GetStageAxisInfo_MinPos(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetStageAxisInfo_MinPos(lChanID);
end;

function TMG17Motor.GetStageAxisInfo_MaxPos(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetStageAxisInfo_MaxPos(lChanID);
end;

function TMG17Motor.GetRelMoveDist_RelDist(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetRelMoveDist_RelDist(lChanID);
end;

function TMG17Motor.GetAbsMovePos_AbsPos(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetAbsMovePos_AbsPos(lChanID);
end;

function TMG17Motor.GetBLashDist_BLashDist(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetBLashDist_BLashDist(lChanID);
end;

function TMG17Motor.GetPosition_Position(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetPosition_Position(lChanID);
end;

function TMG17Motor.GetPositionEx_UncalibPosition(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetPositionEx_UncalibPosition(lChanID);
end;

function TMG17Motor.GetJogMode_Mode(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.GetJogMode_Mode(lChanID);
end;

function TMG17Motor.GetJogMode_StopMode(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.GetJogMode_StopMode(lChanID);
end;

function TMG17Motor.GetJogStepSize_StepSize(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetJogStepSize_StepSize(lChanID);
end;

function TMG17Motor.GetStatusBits_Bits(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.GetStatusBits_Bits(lChanID);
end;

function TMG17Motor.GetJogVelParams_Accn(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetJogVelParams_Accn(lChanID);
end;

function TMG17Motor.GetJogVelParams_MaxVel(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetJogVelParams_MaxVel(lChanID);
end;

function TMG17Motor.GetHomeParams_HomeVel(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetHomeParams_HomeVel(lChanID);
end;

function TMG17Motor.GetHomeParams_ZeroOffset(lChanID: Integer): Single;
begin
  Result := DefaultInterface.GetHomeParams_ZeroOffset(lChanID);
end;

function TMG17Motor.GetPIDParams_Prop(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.GetPIDParams_Prop(lChanID);
end;

function TMG17Motor.GetPIDParams_Int(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.GetPIDParams_Int(lChanID);
end;

function TMG17Motor.GetPIDParams_Deriv(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.GetPIDParams_Deriv(lChanID);
end;

function TMG17Motor.SC_SetOperatingMode(lChanID: Integer; lMode: Integer): Integer;
begin
  Result := DefaultInterface.SC_SetOperatingMode(lChanID, lMode);
end;

function TMG17Motor.SC_GetOperatingMode(lChanID: Integer; var plMode: Integer): Integer;
begin
  Result := DefaultInterface.SC_GetOperatingMode(lChanID, plMode);
end;

function TMG17Motor.SC_SetCycleParams(lChanID: Integer; fOnTime: Single; fOffTime: Single; 
                                      lNumCycles: Integer): Integer;
begin
  Result := DefaultInterface.SC_SetCycleParams(lChanID, fOnTime, fOffTime, lNumCycles);
end;

function TMG17Motor.SC_GetCycleParams(lChanID: Integer; var pfOnTime: Single; 
                                      var pfOffTime: Single; var plNumCycles: Integer): Integer;
begin
  Result := DefaultInterface.SC_GetCycleParams(lChanID, pfOnTime, pfOffTime, plNumCycles);
end;

function TMG17Motor.SC_Enable(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.SC_Enable(lChanID);
end;

function TMG17Motor.SC_Disable(lChanID: Integer): Integer;
begin
  Result := DefaultInterface.SC_Disable(lChanID);
end;

function TMG17Motor.SC_GetOPState(lChanID: Integer; var plState: Integer): Integer;
begin
  Result := DefaultInterface.SC_GetOPState(lChanID, plState);
end;

function TMG17Motor.SetBrakeState(lChanID: Integer; lState: Integer): Integer;
begin
  Result := DefaultInterface.SetBrakeState(lChanID, lState);
end;

function TMG17Motor.GetBrakeState(lChanID: Integer; var plState: Integer): Integer;
begin
  Result := DefaultInterface.GetBrakeState(lChanID, plState);
end;

function TMG17Motor.LLSuspendEndOfMoveMsgs: Integer;
begin
  Result := DefaultInterface.LLSuspendEndOfMoveMsgs;
end;

function TMG17Motor.LLResumeEndOfMoveMsgs: Integer;
begin
  Result := DefaultInterface.LLResumeEndOfMoveMsgs;
end;

function TMG17Motor.EnableEventDlg(bEnable: WordBool): Integer;
begin
  Result := DefaultInterface.EnableEventDlg(bEnable);
end;

function TMG17Motor.ShowEventDlg: Integer;
begin
  Result := DefaultInterface.ShowEventDlg;
end;

function TMG17Motor.SetRotStageModes(lChanID: Integer; lMoveMode: Integer; lPosReportMode: Integer): Integer;
begin
  Result := DefaultInterface.SetRotStageModes(lChanID, lMoveMode, lPosReportMode);
end;

function TMG17Motor.GetRotStageModes(lChanID: Integer; var plMoveMode: Integer; 
                                     var plPosReportMode: Integer): Integer;
begin
  Result := DefaultInterface.GetRotStageModes(lChanID, plMoveMode, plPosReportMode);
end;

function TMG17Motor.LLSetPMDParam(lCommandCode: Integer; lParamCode: Integer; lVal: Integer): Integer;
begin
  Result := DefaultInterface.LLSetPMDParam(lCommandCode, lParamCode, lVal);
end;

function TMG17Motor.LLGetPMDParam(lCommandCode: Integer; lParamCode: Integer; var plVal: Integer): Integer;
begin
  Result := DefaultInterface.LLGetPMDParam(lCommandCode, lParamCode, plVal);
end;

function TMG17Motor.SetDCCurrentLoopParams(lChanID: Integer; lProp: Integer; lInt: Integer; 
                                           lIntLim: Integer; lIntDeadBand: Integer; lFFwd: Integer): Integer;
begin
  Result := DefaultInterface.SetDCCurrentLoopParams(lChanID, lProp, lInt, lIntLim, lIntDeadBand, 
                                                    lFFwd);
end;

function TMG17Motor.GetDCCurrentLoopParams(lChanID: Integer; var plProp: Integer; 
                                           var plInt: Integer; var plIntLim: Integer; 
                                           var plIntDeadBand: Integer; var plFFwd: Integer): Integer;
begin
  Result := DefaultInterface.GetDCCurrentLoopParams(lChanID, plProp, plInt, plIntLim, 
                                                    plIntDeadBand, plFFwd);
end;

function TMG17Motor.SetDCPositionLoopParams(lChanID: Integer; lProp: Integer; lInt: Integer; 
                                            lIntLim: Integer; lDeriv: Integer; lDerivTime: Integer; 
                                            lLoopGain: Integer; lVelFFwd: Integer; 
                                            lAccFFwd: Integer; lPosErrLim: Integer): Integer;
begin
  Result := DefaultInterface.SetDCPositionLoopParams(lChanID, lProp, lInt, lIntLim, lDeriv, 
                                                     lDerivTime, lLoopGain, lVelFFwd, lAccFFwd, 
                                                     lPosErrLim);
end;

function TMG17Motor.GetDCPositionLoopParams(lChanID: Integer; var plProp: Integer; 
                                            var plInt: Integer; var plIntLim: Integer; 
                                            var plDeriv: Integer; var plDerivTime: Integer; 
                                            var plLoopGain: Integer; var plVelFFwd: Integer; 
                                            var plAccFFwd: Integer; var plPosErrLim: Integer): Integer;
begin
  Result := DefaultInterface.GetDCPositionLoopParams(lChanID, plProp, plInt, plIntLim, plDeriv, 
                                                     plDerivTime, plLoopGain, plVelFFwd, plAccFFwd, 
                                                     plPosErrLim);
end;

function TMG17Motor.SetDCMotorOutputParams(lChanID: Integer; fContCurrLim: Single; 
                                           fEnergyLim: Single; fMotorLim: Single; fMotorBias: Single): Integer;
begin
  Result := DefaultInterface.SetDCMotorOutputParams(lChanID, fContCurrLim, fEnergyLim, fMotorLim, 
                                                    fMotorBias);
end;

function TMG17Motor.GetDCMotorOutputParams(lChanID: Integer; var pfContCurrLim: Single; 
                                           var pfEnergyLim: Single; var pfMotorLim: Single; 
                                           var pfMotorBias: Single): Integer;
begin
  Result := DefaultInterface.GetDCMotorOutputParams(lChanID, pfContCurrLim, pfEnergyLim, 
                                                    pfMotorLim, pfMotorBias);
end;

function TMG17Motor.SetDCTrackSettleParams(lChanID: Integer; lSettleTime: Integer; 
                                           lSettleWnd: Integer; lTrackWnd: Integer): Integer;
begin
  Result := DefaultInterface.SetDCTrackSettleParams(lChanID, lSettleTime, lSettleWnd, lTrackWnd);
end;

function TMG17Motor.GetDCTrackSettleParams(lChanID: Integer; var plSettleTime: Integer; 
                                           var plSettleWnd: Integer; var plTrackWnd: Integer): Integer;
begin
  Result := DefaultInterface.GetDCTrackSettleParams(lChanID, plSettleTime, plSettleWnd, plTrackWnd);
end;

function TMG17Motor.SetDCProfileModeParams(lChanID: Integer; lProfMode: Integer; fJerk: Single): Integer;
begin
  Result := DefaultInterface.SetDCProfileModeParams(lChanID, lProfMode, fJerk);
end;

function TMG17Motor.GetDCProfileModeParams(lChanID: Integer; var plProfMode: Integer; 
                                           var pfJerk: Single): Integer;
begin
  Result := DefaultInterface.GetDCProfileModeParams(lChanID, plProfMode, pfJerk);
end;

function TMG17Motor.SetDCJoystickParams(lChanID: Integer; fMaxVelLO: Single; fMaxVelHI: Single; 
                                        fAccnLO: Single; fAccnHI: Single; lDirSense: Integer): Integer;
begin
  Result := DefaultInterface.SetDCJoystickParams(lChanID, fMaxVelLO, fMaxVelHI, fAccnLO, fAccnHI, 
                                                 lDirSense);
end;

function TMG17Motor.GetDCJoystickParams(lChanID: Integer; var pfMaxVelLO: Single; 
                                        var pfMaxVelHI: Single; var pfAccnLO: Single; 
                                        var pfAccnHI: Single; var plDirSense: Integer): Integer;
begin
  Result := DefaultInterface.GetDCJoystickParams(lChanID, pfMaxVelLO, pfMaxVelHI, pfAccnLO, 
                                                 pfAccnHI, plDirSense);
end;

function TMG17Motor.SetDCSettledCurrentLoopParams(lChanID: Integer; lSettledProp: Integer; 
                                                  lSettledInt: Integer; lSettledIntLim: Integer; 
                                                  lSettledIntDeadBand: Integer; 
                                                  lSettledFFwd: Integer): Integer;
begin
  Result := DefaultInterface.SetDCSettledCurrentLoopParams(lChanID, lSettledProp, lSettledInt, 
                                                           lSettledIntLim, lSettledIntDeadBand, 
                                                           lSettledFFwd);
end;

function TMG17Motor.GetDCSettledCurrentLoopParams(lChanID: Integer; var plSettledProp: Integer; 
                                                  var plSettledInt: Integer; 
                                                  var plSettledIntLim: Integer; 
                                                  var plSettledIntDeadBand: Integer; 
                                                  var plSettledFFwd: Integer): Integer;
begin
  Result := DefaultInterface.GetDCSettledCurrentLoopParams(lChanID, plSettledProp, plSettledInt, 
                                                           plSettledIntLim, plSettledIntDeadBand, 
                                                           plSettledFFwd);
end;

function TMG17Motor.SetBowIndex(lChanID: Integer; lBowIndex: Integer): Integer;
begin
  Result := DefaultInterface.SetBowIndex(lChanID, lBowIndex);
end;

function TMG17Motor.GetBowIndex(lChanID: Integer; var plBowIndex: Integer): Integer;
begin
  Result := DefaultInterface.GetBowIndex(lChanID, plBowIndex);
end;

function TMG17Motor.SetDCTriggerParams(lChanID: Integer; lTrigInMode: Integer; lTrigOutMode: Integer): Integer;
begin
  Result := DefaultInterface.SetDCTriggerParams(lChanID, lTrigInMode, lTrigOutMode);
end;

function TMG17Motor.GetDCTriggerParams(lChanID: Integer; var plTrigInMode: Integer; 
                                       var plTrigOutMode: Integer): Integer;
begin
  Result := DefaultInterface.GetDCTriggerParams(lChanID, plTrigInMode, plTrigOutMode);
end;

function TMG17Motor.SetMFFOperParams(lChanID: Integer; lTransitTime: Integer; 
                                     lDigIO1OperMode: Integer; lDigIO1SigMode: Integer; 
                                     lDigIO1PulseWidth: Integer; lDigIO2OperMode: Integer; 
                                     lDigIO2SigMode: Integer; lDigIO2PulseWidth: Integer): Integer;
begin
  Result := DefaultInterface.SetMFFOperParams(lChanID, lTransitTime, lDigIO1OperMode, 
                                              lDigIO1SigMode, lDigIO1PulseWidth, lDigIO2OperMode, 
                                              lDigIO2SigMode, lDigIO2PulseWidth);
end;

function TMG17Motor.GetMFFOperParams(lChanID: Integer; var plTransitTime: Integer; 
                                     var plDigIO1OperMode: Integer; var plDigIO1SigMode: Integer; 
                                     var plDigIO1PulseWidth: Integer; 
                                     var plDigIO2OperMode: Integer; var plDigIO2SigMode: Integer; 
                                     var plDigIO2PulseWidth: Integer): Integer;
begin
  Result := DefaultInterface.GetMFFOperParams(lChanID, plTransitTime, plDigIO1OperMode, 
                                              plDigIO1SigMode, plDigIO1PulseWidth, 
                                              plDigIO2OperMode, plDigIO2SigMode, plDigIO2PulseWidth);
end;

function TMG17Motor.LLSetPosition(lChanID: Integer; lPosition: Integer): Integer;
begin
  Result := DefaultInterface.LLSetPosition(lChanID, lPosition);
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TMG17Motor]);
end;

end.
