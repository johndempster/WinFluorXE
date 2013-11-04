unit AmpModule;
// -------------------------------------------------------------
// Patch clamp and computer-controlled amplifier support module
// -------------------------------------------------------------
// (c) J. Dempster, University of Strathclyde, 2001, All rights reserved.
// USERS OF THIS SOFTWARE MAY MODIFY IT FOR PERSONAL PURPOSES ASSOCIATED
// WITH ACADEMIC RESEARCH. ITS SALE OR INCORPORATION INTO COMMERCIALLY
// DISTRIBUTED PRODUCTS IN WHOLE OR IN PART IS FORBIDDEN WITHOUT THE
// THE EXPRESS PERMISSION OF THE AUTHOR OR THE UNIVERSITY OF STRATHCLYDE
// 20/2/02 Axopatch 200 added
// 26/02/02 Axopatch 200 tested and working correctly
// NOTE Axopatch 200 gain telegraph values X100 or larger require +/-10V ADC range
// 26/2/01 CED 1902 support moved to this module
// 7/8/01 Integers changed to DWORD to make it compile under Delphi 5
// 17.5.02 Communications with CED 1902 via COM port now works under Windows NT/2000
// 24.6.03 Support for WPC-100 patch clamp added
// 25.8.03 CED 1902 queried after each command sent
//         (fixes failure to set AC coupling with 29xx s/n 1902s)
// 24.2.04 VP500 amplifier gain added
// 8.3.04 Errors in WCP-100 gain telegraph corrected
//        Biologic RK400 added
//        All gain telegraphs now use same signal reading algorithm
//        Cairn Optopatch added
//        26.05.04 Axon MultiClamp 700 support added
// 10.08.04 Biologic RK400 gain readout fixed
// 15.09.04 Support for Heka EPC-8 amplifier added (not yet tested)
// 16.09.04 Support for NPI Turbo Tec 03 added
// 06.11.04 Support for A-M Systems 2400 added
// 08.11.04 Two amplifiers now supported
// 12.01.05 Optopatch gain now read correctly
// 13.02.05 AMS-2400 gain corrected
// 14.02.05 Support for NPI Turbo Tec 10C added
// 18.07.05 Support for NPI SEC 05LX switch clamp added
// 29.07.05 NPI SEC 05LX gain corrected
// 01.08.05 NPI SEC 05LX gain corrected (finally?)
// 05.09.05 CED 1902 gain now reading properly
// 12.09.05 EPC-8 digital gain telegraph now supported
//          (Tested with CED1401 at Plymouth CPW)
// 12.13.05 Dagan PC-One patch clamp added
// 11.1.06  CED 1902 settings now downloaded before gain is read
//          if it has not been initialised
// 12.04.06 Support for Dagan 3900A added
// 20.06.06 Multiclamp 700A now supported correctly
//          Primary & Secondary Multiclamp channels can now be changed
// 28.07.06 Warner OC725C, PC501A and PC505B support added
//          Axopatch 200 voltage- current- clamp switching now supported
//          via mode telegraph
// 17.08.06 Telegraph voltage reading now done by GetTelegraphVoltage
// 03.09.06 Mode telegraph support for Cairn Optopatch added
//          Full range of Optopatch telegraph (Big cell-Patch) now covered
// 18.08.07 Default gain telegraph now 7 and mode 6 making it same as user gude
// 18.03.08 Axoclamp 2 and Dagan TEV200A support added
// 28.05.08 Support for Tecella Triton added
// 03.06.08 Amplifiers can now be addressed independently
//          .Number property removed .SetAmpType GetAmpType removed
// 04.06.08 Support for Axoclamp 900A added
// 14.07.08 Support for NPI Turbo TEC-05, TEC-10C, TEC-10CX, TEC-20, TEC-30 added
//          (consoldated into single TurboTec function
//          Amplifier number now stored as object in amplifier list rather than index
//          Allowing amplifiers to be listed in a better order.
// 05.08.08 Optopatch checked now working correctly

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math ;

const

     NumAmplifiers = 29 ;

     amNone = 0 ;
     amAxopatch1d = 1 ;
     amAxopatch200 = 2 ;
     amCED1902 = 3 ;
     amWPC100 = 4 ;
     amVP500 = 5 ;
     amRK400 = 6 ;
     amOptopatch = 7 ;
     amMultiClamp700A = 8 ;
     amMultiClamp700B = 9 ;
     amEPC8 = 10 ;
     amTurboTec03 = 11 ;
     amAMS2400 = 12 ;
     amTurboTec10C = 13 ;
     amSEC05LX = 14 ;
     amManual = 15 ;
     amDaganPCOne10M = 16 ;
     amDaganPCOne100M = 17 ;
     amDaganPCOne1G = 18 ;
     amDaganPCOne10G = 19 ;
     amDagan3900A10nA = 20 ;
     amDagan3900A100nA = 21 ;
     amWarnerPC501A = 22 ;
     amWarnerPC505B = 23 ;
     amWarnerOC725C = 24 ;
     amAxoclamp2 = 25 ;
     amDaganTEV200A = 26 ;
     amTriton = 27 ;
     amAxoClamp900A = 28 ;
     amTurboTec05 = 29 ;
     amTurboTec10CX = 30 ;
     amTurboTec20 = 31 ;
     amTurboTec30 = 32 ;


     // Patch clamp mode flags
     VClampMode = 0 ;
     IClampMode = 1 ;

     // Axon Multi-Clamp constants
     MCTG_API_VERSION = 5;
     MCTG_OPEN_MESSAGE_STR : PChar = 'MultiClampTelegraphOpenMsg';
     MCTG_CLOSE_MESSAGE_STR : PChar = 'MultiClampTelegraphCloseMsg';
     MCTG_REQUEST_MESSAGE_STR : PChar = 'MultiClampTelegraphRequestMsg';
     MCTG_RECONNECT_MESSAGE_STR : PChar = 'MultiClampTelegraphReconnectMsg';
     MCTG_BROADCAST_MESSAGE_STR : PChar = 'MultiClampTelegraphBroadcastMsg';
     MCTG_ID_MESSAGE_STR : PChar = 'MultiClampTelegraphIdMsg';

     MCTG_MAX_CLIENTS = 16;

     MCTG_MODE_VCLAMP = 0;
     MCTG_MODE_ICLAMP = 1;
     MCTG_MODE_ICLAMPZERO = 2;
     MCTG_MODE_NUMCHOICES = 3;

     MCTG_OUT_MUX_COMMAND         = 0;
     MCTG_OUT_MUX_I_MEMBRANE      = 1;
     MCTG_OUT_MUX_V_MEMBRANE      = 2;
     MCTG_OUT_MUX_V_MEMBRANEx100  = 3;
     MCTG_OUT_MUX_I_BATH          = 4;
     MCTG_OUT_MUX_V_BATH          = 5;

     MCTG_OUT_MUX_NUMCHOICES_700A      = 6;
     MCTG_ChanNames700A_VC : Array[0..MCTG_OUT_MUX_NUMCHOICES_700A-1] of String =
     ('Vm','Im','Vp','Vm(AC)','Ib','Vb') ;
     MCTG_ChanNames700A_CC : Array[0..MCTG_OUT_MUX_NUMCHOICES_700A-1] of String =
     ('Icom','Im','Vm','Vm(AC)','Ib','Vb') ;

     MCTG_OUT_MUX_NUMCHOICES_700B = 50 ;
     MCTG_ChanNames700B : Array[0..MCTG_OUT_MUX_NUMCHOICES_700B-1] of String = (
     'Im','Vm','Vp','Vm(AC)','Vcom','5','6','7','8','9',
     'Im','Vm','Vp','Vm(AC)','Vcom','15','16','17','18','19',
     'Vm','Im','Icom','Vm(AC)','Icom','25','26','27','28','29',
     'Vm','Im','Vp','Vm(AC)','Icom','35','36','37','38','39',
     'AUX1','41','42','AUX2','44','45','46','47','48','49' ) ;
     MCTG_OUT_MUX_MAXCHOICES = MCTG_OUT_MUX_NUMCHOICES_700B ;

     MCTG_UNITS_VOLTS_PER_VOLT      = 0;
     MCTG_UNITS_VOLTS_PER_MILLIVOLT = 1;
     MCTG_UNITS_VOLTS_PER_MICROVOLT = 2;
     MCTG_UNITS_VOLTS_PER_AMP       = 3;
     MCTG_UNITS_VOLTS_PER_MILLIAMP  = 4;
     MCTG_UNITS_VOLTS_PER_MICROAMP  = 5;
     MCTG_UNITS_VOLTS_PER_NANOAMP   = 6;
     MCTG_UNITS_VOLTS_PER_PICOAMP   = 7;
     MCTG_ChannelUnits : Array[0..7] of String =
     ('V','mV','uV','A','mA','uA', 'nA', 'pA' ) ;

     MCTG_LPF_BYPASS         = 1.0e+5;
     MCTG_NOMEMBRANECAP      = 0.0e+0;

     MCTG_HW_TYPE_MC700A      = 0;
     MCTG_HW_TYPE_NUMCHOICES  = 1;


     AXC_CHAN_1                 = 0;
     AXC_CHAN_2                 = 1;
     AXC_MAX_CHANNELS           = 2;

     AXC_CHAN_ALL               = 2;
     AXC_CHAN_NONE              = 3;

// Axoclamp 900A
     AXC_SOCHAN_1               = 0;
     AXC_SOCHAN_2               = 1;
     AXC_MAX_SOCHANNELS         = 2;
     AXC_MODE_IZERO             = 0;
     AXC_MODE_ICLAMP            = 1;
     AXC_MODE_DCC               = 2;
     AXC_MODE_HVIC              = 3;
     AXC_MODE_DSEVC             = 4;
     AXC_MODE_TEVC              = 5;
     AXC_MAX_MODES              = 6;
     AXC_MODE_NONE              = 6;
     AXC_MODE_ALL               = 7;
     ACX_HW_TYPE_AC900A      = 0;
     ACX_HW_TYPE_NUMCHOICES  = 1;
     ACX_HW_TYPE_CURRENT     = ACX_HW_TYPE_AC900A;
     AXC_AUTO_TEVC              = 0;
     AXC_AUTO_DSEVC             = 1;
     AXC_AUTO_ICRETURN_MANUAL   = 0;
     AXC_AUTO_ICRETURN_EXTERNAL = 1;
     AXC_AUTO_ICRETURN_DELAY    = 2;
     AXC_AUTO_POLARITY_NTOP     = 0;
     AXC_AUTO_POLARITY_PTON     = 1;
     AXC_AUTO_SOURCE_EXTERNAL   = 0;
     AXC_AUTO_SOURCE_INTERNAL   = 1;
     AXC_SIGNAL_ID_XICMD1      = 0;
     AXC_SIGNAL_ID_ICMD1       = 1;
     AXC_SIGNAL_ID_10V1        = 2;
     AXC_SIGNAL_ID_I1          = 3;
     AXC_SIGNAL_ID_MON         = 4;
     AXC_SIGNAL_ID_RMP         = 5;
     AXC_SIGNAL_ID_XICMD2      = 6;
     AXC_SIGNAL_ID_ICMD2       = 7;
     AXC_SIGNAL_ID_10V2        = 8;
     AXC_SIGNAL_ID_I2          = 9;
     AXC_SIGNAL_ID_DIV10V2     = 10;
     AXC_SIGNAL_ID_DIV10I2     = 11;
     AXC_SIGNAL_ID_XVCMD       = 12;
     AXC_SIGNAL_ID_VCMD        = 13;
     AXC_SIGNAL_ID_10AUX1      = 14;
     AXC_SIGNAL_ID_10AUX2      = 15;
     AXC_SIGNAL_ID_10mV        = 16;
     AXC_SIGNAL_ID_GND         = 17;

//==============================================================================================
// Headstage types - 6 to 19 reserved for future headstages
//==============================================================================================
  AXC_HEADSTAGE_TYPE_HS9_ADAPTER          = 0;   // custom headstage, user entered Rf and Ci value
  AXC_HEADSTAGE_TYPE_HS9_x10uA            = 1;   // Rf = 1M
  AXC_HEADSTAGE_TYPE_HS9_x1uA             = 2;   // Rf = 10M
  AXC_HEADSTAGE_TYPE_HS9_x100nA           = 3;   // Rf = 100M
  AXC_HEADSTAGE_TYPE_VG9_x10uA            = 4;   // Rf = 1M
  AXC_HEADSTAGE_TYPE_VG9_x100uA           = 5;   // Rf = 0.1M
  AXC_HEADSTAGE_TYPE_NONE                 = 20;  // headstage not connected


    AXC_SignalName : Array[0..17] of String = (
    'Vc1',
    'Ic1',
    'Vm1',
    'Im1',
    'DCC1',
    'Vrmp',
    'Ic2x',
    'Ic2',
    'Vm2',
    'Im2',
    'Vm2/10',
    'Im2/10',
    'Vex',
    'Vc',
    'Aux1',
    'Aux2',
    'Test',
    'Gnd'
    ) ;

    AXC_SignalUnits : Array[0..17] of String = (
    'mV', {Vc1}
    'nA', {Ic1}
    'mV', {Vm1}
    'nA', {Im1}
    'mV', {DCC1}
    'mV', {Vrmp}
    'nA', {Ic2x}
    'nA', {Ic2}
    'mV', {Vm2}
    'nA', {Im2}
    'mV', {Vm2/10}
    'nA', {Im2/10}
    'mV', {Vex}
    'mV', {Vc}
    'mV', {Aux1}
    'nA', {Aux2}
    'mV', {Test}
    'mV'  {Gnd}
    ) ;

    AXC_ChanCalFactors : Array[0..17] of Single = (
    0.01, {Vc1}
    0.01, {Ic1}
    0.01, {Vm1}
    0.01, {Im1}
    0.01, {DCC1}
    0.01, {Vrmp}
    0.1,  {Ic2x}
    0.1,  {Ic2}
    0.01,  {Vm2}
    0.1, {Im2}
    0.001, {Vm2/10}
    0.01, {Im2/10}
    0.01, {Vex}
    0.01, {Vc}
    0.01, {Aux1}
    0.01, {Aux2}
    0.01, {Test}
    0.01  {Gnd}
    ) ;

type

TCopyDataStruct = record
    dwData : Cardinal ;
    cbData : Cardinal ;
    lpData : Pointer ;
    end ;
PCopyDataStruct = ^TCopyDataStruct ;

// Axon Multiclamp Commander data record
TMC_TELEGRAPH_DATA = packed record
   Version : Cardinal ;            // must be set to MCTG_API_VERSION
   StructSize : Cardinal ;         // currently 128 bytes
   ComPortID : Cardinal ;          // ( one-based  counting ) 1 -> 8
   AxoBusID : Cardinal ;           // ( zero-based counting ) 0 -> 9
   ChannelID : Cardinal ;          // ( one-based  counting ) 1 -> 2
   OperatingMode : Cardinal ;      // use constants defined above
   PrimaryScaledOutSignal : Cardinal ;    // use constants defined above
   PrimaryAlpha : Double ;              // scaled output gain (dimensionless)
   PrimaryScaleFactor : Double ;        // gain scale factor ( for dAlpha == 1 )
   PrimaryScaleFactorUnits : Cardinal ;   // use constants defined above
   LPFCutoff : Double ;          // ( Hz ) , ( MCTG_LPF_BYPASS indicates Bypass )
   MembraneCap : Double ;        // ( F  )
   ExtCmdSens : Double ;         // external command sensitivity
   SecondaryOutSignal : Cardinal ;       // use constants defined above
   SecondaryScaleFactor : Double ;     // gain scale factor ( for dAlpha == 1 )
   SecondaryScaleFactorUnits : Cardinal ;// use constants defined above
   HardwareType : Cardinal ;       // use constants defined above
   SecondaryAlpha : Double ;
   pcPadding : Array[1..28] of Byte ;
   pcPadding1 : Array[1..128] of Byte ;
   end ;
PMC_TELEGRAPH_DATA = ^TMC_TELEGRAPH_DATA ;

TCED1902 = record
           ComPort : LongInt ;
           ComHandle : Integer ;
           Input : LongInt ;
           InputName : string[16] ;
           Gain : LongInt ;
           GainValue : Single ;
           HPFilter : LongInt ;
           HPFilterValue : Single ;
           LPFilter : LongInt ;
           LPFilterValue : Single ;
           NotchFilter : LongInt ;
           ACCoupled : LongInt ;
           DCOffset : LongInt ;
           DCOffsetVMax : single ;
           OverLapStructure : POverlapped ;
           AmplifierSet : Boolean ;
           end ;

// Axoclamp 900A

TAXC_Signal = record
    Channel : Integer ;
    ID : Integer ;
    Name : PChar ;
    Valid : Boolean ;
    end ;

// Create the Axoclamp device object and return a handle.

TAXC_CheckAPIVersion = function(QueryVersion : PChar ) : Boolean ;

TAXC_CreateHandle = function(
                    bDemo : Boolean ;
                    var Error : Integer ) : Integer ; stdcall ;

// Destroy the Axoclamp device object specified by handle.
TAXC_DestroyHandle = procedure(
                     AxHandle : Integer
                     ) ; stdcall ;


// Find the first Axoclamp 900x and return device info.
TAXC_FindFirstDevice = function(
                       AxHandle : Integer ;
                       pszSerialNum : PChar ;
                       uBufSize : Integer ;
                       var Error : Integer ) : Boolean ; stdcall ;

// Find next Axoclamp 900x and return device info, returns FALSE when all Axoclamp 900x devices have been found.
TAXC_FindNextDevice = function(
                       AxHandle : Integer ;
                       pszSerialNum : PChar ;
                       uBufSize : Integer ;
                       var Error : Integer ) : Boolean ; stdcall ;

// Open Axoclamp 900x device.
TAXC_OpenDevice = function(
                  AxHandle : Integer ;
                  pszSerialNum : PChar ;
                  bReadHardware : Boolean ;
                  var Error : Integer ) : Boolean  ; stdcall ;

// Close Axoclamp 900x device.
TAXC_CloseDevice = function(
                   AxHandle : Integer ;
                   var Error : Integer ) : Boolean  ; stdcall ;

// Get Axoclamp 900x device serial number.
TAXC_GetSerialNumber = function(
                       AxHandle : Integer ;
                       pszSerialNum : PChar ;
                       uBufSize : Integer ;
                       var Error : Integer ) : Boolean  ; stdcall ;

// Is an Axoclamp 900x device open?
TAXC_IsDeviceOpen = function(
                     AxHandle : Integer ;
                     var pbOpen : Boolean ;
                     var Error : Integer ) : Boolean  ; stdcall ;

// Get scaled output signal
TAXC_GetScaledOutputSignal = function(
                             HAxHandle : Integer ;
                             var Signal : Integer ;
                             Channel : Integer ;
                             Mode : Integer ;
                             var Error : Integer) : Boolean  ; stdcall ;

// Set scaled output signal
TAXC_SetScaledOutputSignal = function(
                             HAxHandle : Integer ;
                             Signal : Integer ;
                             Channel : Integer ;
                             Mode : Integer ;
                             var Error : Integer) : Boolean  ; stdcall ;


// Get scaled output gain
TAXC_GetScaledOutputGain = function(
                           AxHandle : Integer ;
                           var Gain : Double ;
                           Channel : Integer ;
                           Mode : Integer ;
                           var Error : Integer) : Boolean  ; stdcall ;

// Get scaled output gain
TAXC_SetScaledOutputGain = function(
                           AxHandle : Integer ;
                           Gain : Double ;
                           Channel : Integer ;
                           Mode : Integer ;
                           var Error : Integer) : Boolean  ; stdcall ;


TAXC_GetMode = function(
               AxHandle : Integer ;
               Channel : Integer ;
               var Mode : Integer ;
               var Error : Integer) : Boolean  ; stdcall ;

TAXC_GetDeviceName = function(
                     AxHandle : Integer ;
                     Name : PChar ;
                     BufSize : Integer ;
                     var Error : Integer ) : Boolean  ; stdcall ;

TAXC_SetMode = function(
               AxHandle : Integer ;
               Channel : Integer ;
               Mode : Integer ;
               var Error : Integer ) : Boolean  ; stdcall ;

TAXC_BuildErrorText = function(
                  AxHandle : Integer ;
                  ErrorNum : Integer ;
                  ErrorText : PChar ;
                  MaxLen : Integer ) : Boolean  ; stdcall ;

TAXC_GetSignalScaleFactor = function(
                            AxHandle : Integer ;
                            var ScaleFactor : Double ;
                            Signal : Integer ;
                            var Error : Integer) : Boolean  ; stdcall ;

TAXC_GetHeadstageType = function(
                        AxHandle : Integer ;
                        var HeadstageType : Integer ;
                        Channel : Integer ;
                        Auxiliary : Boolean ;
                        var Error : Integer ) : Boolean  ; stdcall ;

  TAmplifier = class(TDataModule)
    procedure AmplifierCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FAmpType : Array[1..NumAmplifiers] of Integer ;

    FGainTelegraphChannel : Array[1..NumAmplifiers] of Integer ;
    LastGain : Array[1..NumAmplifiers] of Single ;  // Last telegraph gain value

    FModeTelegraphChannel : Array[1..NumAmplifiers] of Integer ;
    LastMode : Array[1..NumAmplifiers] of Single ; // Last telegraph mode value

    FAmpCurrentChannel : Integer ;
    FAmpVoltageChannel : Integer ;

    // Multiclamp Commander
    MCConnectionOpen : Boolean ;
    MCNumChannels : Cardinal ;
    MCChannels: Array[0..16] of Cardinal ;
    MCOpenMessageID : Cardinal ;
    MCCloseMessageID : Cardinal ;
    MCRequestMessageID : Cardinal ;
    MCReconnectMessageID : Cardinal ;
    MCBroadcastMessageID : Cardinal ;
    MCIDMessageID : Cardinal ;

    // Axoclamp 900A
    Axoclamp900ALibPath : String ;
    Axoclamp900ALibLoaded : Boolean ;
    Axoclamp900AOpen : Boolean ;
    Axoclamp900ALibHnd : Integer ;
    Axoclamp900AHIDHnd : Integer ;
    Axoclamp900AHnd : Integer ;

    procedure GetNoneChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;

    procedure GetCED1902ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;


    function GetAxopatch1DGain(
         AmpNumber : Integer ) : single ;
    procedure GetAxopatch1DChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;

    function GetAxopatch200Gain(
         AmpNumber : Integer ) : single ;
    procedure GetAxopatch200ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;

    function GetWPC100Gain(
         AmpNumber : Integer ) : single ;
    procedure GetWPC100ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;

    function GetVP500Gain(
         AmpNumber : Integer ) : single ;
    procedure GetVP500ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;

    function GetRK400Gain(
         AmpNumber : Integer ) : single ;
    procedure GetRK400ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;

    function GetOptopatchGain(
         AmpNumber : Integer ) : single ;
    procedure GetOptopatchChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;

    function GetMultiClampGain(
         AmpNumber : Integer ) : single ;
    procedure GetMultiClampChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetTurboTecGain(
         AmpNumber : Integer ) : single ;
    procedure GetTurboTecChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetAMS2400Gain(
         AmpNumber : Integer ) : single ;
    procedure GetAMS2400ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetEPC8Gain(
         AmpNumber : Integer ) : single ;
    procedure GetEPC8ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetSEC05LXGain(
         AmpNumber : Integer ) : single ;
    procedure GetSEC05LXChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetDaganPCOneGain(
         AmpNumber : Integer ) : single ;
    procedure GetDaganPCOneChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetDagan3900AGain(
         AmpNumber : Integer ) : single ;
    procedure GetDagan3900AChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetWarnerPC501AGain(
         AmpNumber : Integer ) : single ;
    procedure GetWarnerPC501AChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetWarnerPC505BGain(
         AmpNumber : Integer ) : single ;
    procedure GetWarnerPC505BChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetWarnerOC725CGain(
         AmpNumber : Integer ) : single ;
    procedure GetWarnerOC725CChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetAxoClamp2Gain(
         AmpNumber : Integer ) : single ;
    procedure GetAxoClamp2ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetDaganTEV200AGain(
         AmpNumber : Integer ) : single ;
    procedure GetDaganTEV200AChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetTritonGain(
         AmpNumber : Integer ) : single ;
    procedure GetTritonChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;

    function GetAxoclamp900AGain(
         ChanNumber : Integer ) : single ;

    procedure GetAxoclamp900AChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : Single ;
          var ChanScale : Single
          ) ;
    procedure OpenAxoclamp900A ;
    procedure CloseAxoclamp900A ;
    procedure CheckErrorAxoclamp900A( Err : Integer ) ;

    function LoadProcedure(
         Hnd : THandle ;       { Library DLL handle }
         Name : string         { Procedure name within DLL }
         ) : Pointer ;         { Return pointer to procedure }

    function AppHookFunc(var Message : TMessage) : Boolean;
    function GetNeedsGainTelegraphChannel(  AmpNumber : Integer ) : Boolean ;
    function GetNeedsModeTelegraphChannel(  AmpNumber : Integer ) : Boolean ;

    procedure SetGainTelegraphChannel( AmpNumber : Integer ; Value : Integer ) ;
    function  GetGainTelegraphChannel(  AmpNumber : Integer ) : Integer  ;

    procedure SetModeTelegraphChannel( AmpNumber : Integer ; Value : Integer ) ;
    function  GetModeTelegraphChannel( AmpNumber : Integer ) : Integer  ;

    procedure SetAmplifierType( AmpNumber : Integer ; Value : Integer ) ;
    function GetAmplifierType( AmpNumber : Integer ) : Integer ;

    function GetModelName( AmpNumber : Integer ) : String ;
    function GetGainTelegraphAvailable( AmpNumber : Integer ) : Boolean ;
  public
    { Public declarations }

    CED1902 : TCED1902 ;        // CED 1902 Amplifier settings
    MCTelegraphData : Array[0..15] of TMC_TELEGRAPH_DATA ; // Multiclamp 700 settings

    procedure GetList( List : TStrings ) ;
    function GetGain( AmpNumber : Integer ) : single ;
    // CED 1902 amplifier procedures/functions
    procedure TransmitLine( const Line : string ) ;
    function  ReceiveLine : string ;
    function Check1902Error : string ;
    procedure SetCED1902 ;
    function GetCED1902DCOffsetRange : single ;
    procedure GetCED1902List( Command : string ; List : TStrings ) ;
    function QueryCED1902( Request : string ) : string ;
    function OpenCED1902 : Boolean ;
    procedure CloseCED1902 ;

    procedure GetChannelSettings(
              iChan : Integer ;
              var ChanName : String ;
              var ChanUnits : String ;
              var ChanCalFactor : Single ;
              var ChanScale : Single
              ) ;

    function GetTelegraphVoltage(
             ChannelNum : Integer
             ) : Single ;

    function ADCInUse : Boolean ;

    function CurrentChannel( AmpNum : Integer ) : Integer ;
    function VoltageChannel( AmpNum : Integer ) : Integer ;
    function ChanAmpNumber( iChan : Integer ) : Integer ;

    procedure GetCommandVoltageDivideFactor(  AmpNumber : Integer ;
                                              var DivFactor : Single ) ;

    Property AmplifierType[AmpNumber : Integer] : Integer read GetAmplifierType write SetAmplifierType ;
    Property ModelName[AmpNumber : Integer] : String read GetModelName ;
    Property GainTelegraphChannel[AmpNumber : Integer] : Integer read GetGainTelegraphChannel
                                            write SetGainTelegraphChannel ;
    Property ModeTelegraphChannel[AmpNumber : Integer] : Integer read GetModeTelegraphChannel
                                            write SetModeTelegraphChannel ;
    Property NeedsGainTelegraphChannel[AmpNumber : Integer] : Boolean read GetNeedsGainTelegraphChannel ;
    Property NeedsModeTelegraphChannel[AmpNumber : Integer] : Boolean read GetNeedsModeTelegraphChannel ;
    Property GainTelegraphAvailable[AmpNumber : Integer] : Boolean read GetGainTelegraphAvailable ;
  end;

var
  Amplifier: TAmplifier;

implementation

uses Main, maths, shared, VP500Unit,VP500Lib , LabIOUnit;

{$R *.DFM}

var
  AXC_CheckAPIVersion : TAXC_CheckAPIVersion ;
  AXC_CreateHandle : TAXC_CreateHandle ;
  AXC_DestroyHandle : TAXC_DestroyHandle ;
  AXC_FindFirstDevice : TAXC_FindFirstDevice ;
  AXC_FindNextDevice : TAXC_FindNextDevice ;
  AXC_OpenDevice : TAXC_OpenDevice ;
  AXC_CloseDevice : TAXC_CloseDevice ;
  AXC_GetSerialNumber : TAXC_GetSerialNumber ;
  AXC_IsDeviceOpen : TAXC_IsDeviceOpen ;
  AXC_GetScaledOutputSignal : TAXC_GetScaledOutputSignal ;
  AXC_SetScaledOutputSignal : TAXC_SetScaledOutputSignal ;
  AXC_GetScaledOutputGain : TAXC_GetScaledOutputGain ;
  AXC_SetScaledOutputGain : TAXC_SetScaledOutputGain ;
  AXC_GetMode : TAXC_GetMode ;
  AXC_GetDeviceName : TAXC_GetDeviceName ;
  AXC_SetMode : TAXC_SetMode ;
  AXC_BuildErrorText : TAXC_BuildErrorText ;
  AXC_GetSignalScaleFactor : TAXC_GetSignalScaleFactor ;
  AXC_GetHeadstageType : TAXC_GetHeadstageType ;

procedure TAmplifier.AmplifierCreate(Sender: TObject);
// ---------------------------------------------
// Initialisations when amplifier module created
// ---------------------------------------------
var
    i : Integer ;
begin
     // Default settings

     FAmpCurrentChannel := 0 ;
     FAmpVoltageChannel := 1 ;

     for i := 1 to NumAmplifiers do begin
         FGainTelegraphChannel[i] := 7 ;
         FModeTelegraphChannel[i] := 6 ;
         FAmpType[i] := amNone ;
         LastGain[i] := 1.0 ;
         LastMode[i] := VClampMode ;
         end ;

     // Default CED 1902 settings
     CED1902.Input := 1 ;
     CED1902.Gain := 1 ;
     CED1902.GainValue := 1. ;
     CED1902.LPFilter := 0 ;
     CED1902.HPFilter := 0 ;
     CED1902.ACCoupled := 0 ;
     CED1902.NotchFilter := 0 ;
     CED1902.ComPort := 1 ;
     CEd1902.ComHandle := -1 ;
     CED1902.DCOffset := 0 ;
     CED1902.AmplifierSet := False ;

     // Hook main window message handler
     Application.HookMainWindow(AppHookFunc);

     // No Multi-Clamp connection
     MCConnectionOpen := False ;
     // Clear Multi-Clamp channel list
     for i := 0 to High(MCChannels) do MCChannels[i] := 0 ;

     // Axoclamp 900A
     Axoclamp900ALibPath := 'C:\Program Files\Molecular Devices\Axoclamp 900A Commander\' ;
     Axoclamp900ALibLoaded := False ;
     Axoclamp900AOpen := False ;
     Axoclamp900ALibHnd := -1 ;
     Axoclamp900AHIDHnd := -1 ;
     Axoclamp900AHnd := -1 ;

     end;


function TAmplifier.AppHookFunc(var Message : TMessage)  : Boolean;
// ---------------
// Message handler
// ---------------
var
    AddChannel : Boolean ;
    MCTelegraphDataIn : TMC_TELEGRAPH_DATA ;
    i : Integer ;
begin
  Result := False; //I just do this by default

   // MultiClamp settings message
   if Message.Msg = WM_COPYDATA then begin

      if ((PCopyDataStruct(Message.lParam)^.cbData = 128) or
          (PCopyDataStruct(Message.lParam)^.cbData = 256)) and
         (PCopyDataStruct(Message.lParam)^.dwData = MCRequestMessageID) then begin
         // Copy telegraph data into record
         MCTelegraphDataIn := PMC_TELEGRAPH_DATA(PCopyDataStruct(Message.lParam)^.lpData)^;
         if (MCTelegraphDataIn.ChannelID >= 0) and
            (MCTelegraphDataIn.ChannelID <= High(MCTelegraphData)) then begin
           MCTelegraphData[MCTelegraphDataIn.ChannelID] :=  MCTelegraphDataIn ;
           end ;
         //Main.StatusBar.SimpleText := 'WM_COPYDATA received' ;
         Result := True ;
         end ;
      end ;

    // MultiClamp server responses to broadcast identification request
    if (Message.Msg = MCIDMessageID) or (Message.Msg = MCReconnectMessageID) then begin
         AddChannel := True ;
         for i := 0 to MCNumChannels-1 do if MCChannels[i] = Message.lParam then AddChannel := False ;
         if AddChannel then begin
             // Store server device/channel ID in list
             MCChannels[MCNumChannels] := Message.lParam ;
             // Open connection to this device/channel
             if not PostMessage(HWND_BROADCAST,MCOpenMessageID,Application.Handle,MCChannels[MCNumChannels] ) then
                ShowMessage( 'MultiClamp Commander (Open Message Failed)' ) ;
             MainFrm.StatusBar.SimpleText := format('MCOpenMessageID broadcast to device %x',
             [MCChannels[MCNumChannels]]) ;
             Inc(MCNumChannels) ;
             end ;
         Result := True ;
         end ;

    end;


procedure TAmplifier.GetList( List : TStrings ) ;
// ---------------------------------
// Get list of supported amplifiers
// ---------------------------------
begin

     List.AddObject('None',Tobject(amNone)) ;
     List.AddObject('Manual gain entry',TObject(amManual)) ;

     List.AddObject('Axopatch 1D',TObject(amAxopatch1d)) ;
     List.AddObject('Axopatch 200',TObject(amAxopatch200)) ;
     List.AddObject('Axon MultiClamp 700A',TObject(amMultiClamp700A)) ;
     List.AddObject('Axon MultiClamp 700B',TObject(amMultiClamp700B)) ;
     List.AddObject('Axon Axoclamp 2 (X1 HS',TObject(amAxoclamp2)) ;
     List.AddObject('Axon Axoclamp 900A',TObject(amAxoClamp900A)) ;

     List.AddObject('CED 1902',TObject(amCED1902)) ;
     List.AddObject('WPC-100',TObject(amWPC100)) ;

     List.AddObject('Biologic VP500',TObject(amVP500)) ;
     List.AddObject('Biologic RK400',TObject(amRK400)) ;

     List.AddObject('Cairn Optopatch',TObject(amOptopatch)) ;
     List.AddObject('Heka EPC-8',TObject(amEPC8)) ;

     List.AddObject('NPI Turbo Tec-03',TObject(amTurboTEC03)) ;
     List.AddObject('A-M Systems 2400',TObject(amAMS2400)) ;

     List.AddObject('NPI SEC 05-LX',TObject(amSEC05LX)) ;
     List.AddObject('NPI Turbo Tec-05',TObject(amTurboTec05)) ;
     List.AddObject('NPI Turbo Tec-10C',TObject(amTurboTEC10C)) ;
     List.AddObject('NPI Turbo Tec-10CX',TObject(amTurboTec10CX)) ;
     List.AddObject('NPI Turbo Tec-20',TObject(amTurboTec20)) ;
     List.AddObject('NPI Turbo Tec-30',TObject(amTurboTec30)) ;

     List.AddObject('Dagan TEV-200',TObject(amDaganTEV200A)) ;
     List.AddObject('Dagan PC-ONE-10 (10M)',TObject(amDaganPCOne10M)) ;
     List.AddObject('Dagan PC-ONE-20 (100M)',TObject(amDaganPCOne100M)) ;
     List.AddObject('Dagan PC-ONE-30 (1G)',TObject(amDaganPCOne1G)) ;
     List.AddObject('Dagan PC-ONE-40 (10G)',TObject(amDaganPCOne10G)) ;
     List.AddObject('Dagan 3900A (H/S 10nA)',TObject(amDagan3900A10nA)) ;
     List.AddObject('Dagan 3900A (H/S 100nA)',TObject(amDagan3900A100nA)) ;

     List.AddObject('Warner PC501A',TObject(amWarnerPC501A)) ;
     List.AddObject('Warner PC505B',TObject(amWarnerPC505B)) ;
     List.AddObject('Warner OC725C',TObject(amWarnerOC725C)) ;

     List.AddObject('Tecella Triton',TObject(amTriton)) ;


     end ;


procedure TAmplifier.SetGainTelegraphChannel(
          AmpNumber : Integer ;
          Value : Integer ) ;
// -----------------------
// Set Gain telegraph channel #
// -----------------------
begin
    FGainTelegraphChannel[AmpNumber] := Value ;
    end ;


function  TAmplifier.GetGainTelegraphChannel(
          AmpNumber : Integer
          ) : Integer  ;
// -----------------------
// Get gain telegraph channel #
// -----------------------
begin
     Result :=  FGainTelegraphChannel[AmpNumber] ;
     end ;


procedure TAmplifier.SetModeTelegraphChannel(
          AmpNumber : Integer ;
          Value : Integer ) ;
// -----------------------
// Set Mode telegraph channel #
// -----------------------
begin
    FModeTelegraphChannel[AmpNumber] := Value ;
    end ;


function  TAmplifier.GetModeTelegraphChannel(
          AmpNumber : Integer )
           : Integer  ;
// -----------------------
// Get Mode telegraph channel #
// -----------------------
begin
     Result :=  FModeTelegraphChannel[AmpNumber] ;
     end ;


procedure TAmplifier.SetAmplifierType(
          AmpNumber : Integer ;
          Value : Integer ) ;
// ----------------------
// Set type of amplifier
// ----------------------
begin
    if (AmpNumber < Low(FAmpType)) or (AmpNumber > High(FAmpType)) then Exit ;
    FAmpType[AmpNumber] := Value ;
    end ;


function TAmplifier.GetAmplifierType(AmpNumber : Integer ) : Integer ;
// ------------------------
// Return type of amplifier
// ------------------------
begin
     AmpNumber := Min(Max(AmpNumber,Low(FAmpType)),High(FAmpType)) ;
     Result := FAmpType[AmpNumber] ;
     end ;




function  TAmplifier.GetModelName(
          AmpNumber : Integer ) : String  ;
// -----------------------
// Get amplifier name
// -----------------------
var
    List : TStringList ;
begin

     List := TStringList.Create ;
     GetList(List) ;
     AmpNumber := Min(Max(AmpNumber,Low(FAmpType)),High(FAmpType)) ;
     Result :=  List[FAmpType[AmpNumber]] ;
     List.Free ;
     end ;


function TAmplifier.GetGain(
         AmpNumber : Integer
         ) : single ;
// ---------------------------------------
// Get current gain setting from amplifier
// ---------------------------------------
begin

     case GetAmplifierType(AmpNumber) of
          amNone : Result := 1.0 ;
          amCED1902 : Result := CED1902.GainValue ;
          amAxopatch1d : Result := GetAxopatch1dGain(AmpNumber) ;
          amAxopatch200 : Result := GetAxopatch200Gain(AmpNumber) ;
          amWPC100 : Result :=  GetWPC100Gain(AmpNumber) ;
          amVP500 : Result := GetVP500Gain(AmpNumber) ;
          amRK400 : Result := GetRK400Gain(AmpNumber) ;
          amOptopatch : Result := GetOptopatchGain(AmpNumber) ;
          amMultiClamp700A : Result := GetMultiClampGain(AmpNumber)  ;
          amMultiClamp700B : Result := GetMultiClampGain(AmpNumber)  ;
          amEPC8 : Result := GetEPC8Gain(AmpNumber) ;
          amTurboTec03,
          amTurboTec05,
          amTurboTec10C,
          amTurboTec10CX,
          amTurboTec20,
          amTurboTec30 : Result := GetTurboTecGain(AmpNumber) ;
          amAMS2400 : Result := GetAMS2400Gain(AmpNumber) ;
          amSEC05LX : Result := GetSEC05LXGain(AmpNumber) ;
          amDaganPCOne10M,
          amDaganPCOne100M,
          amDaganPCOne1G,
          amDaganPCOne10G : Result :=  GetDaganPCOneGain(AmpNumber) ;
          amDagan3900A10nA,amDagan3900A100nA : Result := GetDagan3900AGain(AmpNumber) ;
          amWarnerPC501A : Result := GetWarnerPC501AGain(AmpNumber) ;
          amWarnerPC505B : Result := GetWarnerPC505BGain(AmpNumber) ;
          amWarnerOC725C : Result := GetWarnerOC725CGain(AmpNumber) ;
          amAxoclamp2 : Result := GetAxoclamp2Gain(AmpNumber) ;
          amDaganTEV200A : Result := GetDaganTEV200AGain(AmpNumber) ;
          amTriton : Result := GetTritonGain(AmpNumber) ;
          amAxoclamp900A : Result := GetAxoclamp900AGain(AmpNumber) ;
          else Result := 1.0 ;
          end ;
     end ;


function TAmplifier.GetNeedsGainTelegraphChannel(
          AmpNumber : Integer )
           : Boolean ;
// ------------------------------------------------------------------
// Returns TRUE if amplifier needs an analogue gain telegraph channel
// ------------------------------------------------------------------
begin
     case GetAmplifierType(AmpNumber) of
          amNone : Result := False ;
          amCED1902 : Result := False ;
          amAxopatch1d : Result := True ;
          amAxopatch200 : Result := True ;
          amWPC100 : Result :=  True ;
          amVP500 : Result := False ;
          amRK400 : Result := True ;
          amOptopatch : Result := True ;
          amMultiClamp700A : Result := False  ;
          amMultiClamp700B : Result := False  ;
          amEPC8 : Result := False ;
          amTurboTec03,
          amTurboTec05,
          amTurboTec10C,
          amTurboTec10CX,
          amTurboTec20,
          amTurboTec30 : Result := True ;
          amAMS2400 : Result := True ;
          amSEC05LX : Result := True ;
          amDaganPCOne10M,
          amDaganPCOne100M,
          amDaganPCOne1G,
          amDaganPCOne10G : Result := True ;
          amDagan3900A10nA,amDagan3900A100nA : Result := True ;
          amWarnerPC501A : Result := True ;
          amWarnerPC505B : Result := True ;
          amWarnerOC725C : Result := True ;
          amAxoclamp2 : Result := False ;
          amDaganTEV200A : Result := True ;
          amTriton : Result := False ;
          amAxoclamp900A : Result := False ;
          else Result := False ;
          end ;
     end ;


function TAmplifier.GetNeedsModeTelegraphChannel(
          AmpNumber : Integer
          ) : Boolean ;
// ------------------------------------------------------------------
// Returns TRUE if amplifier needs a mode telegraph channel
// ------------------------------------------------------------------
begin
     case GetAmplifierType(AmpNumber) of
          amNone : Result := False ;
          amCED1902 : Result := False ;
          amAxopatch1d : Result := False ;
          amAxopatch200 : Result := True ;
          amWPC100 : Result :=  False ;
          amVP500 : Result := False ;
          amRK400 : Result := False ;
          amOptopatch : Result := True ;
          amMultiClamp700A : Result := False  ;
          amMultiClamp700B : Result := False  ;
          amEPC8 : Result := False ;
          amTurboTec03,
          amTurboTec05,
          amTurboTec10C,
          amTurboTec10CX,
          amTurboTec20,
          amTurboTec30 : Result := False ;
          amAMS2400 : Result := False ;
          amSEC05LX : Result := False ;
          amDaganPCOne10M,
          amDaganPCOne100M,
          amDaganPCOne1G,
          amDaganPCOne10G : Result := False ;
          amDagan3900A10nA,amDagan3900A100nA : Result := False ;
          amWarnerPC501A : Result := False ;
          amWarnerPC505B : Result := False ;
          amWarnerOC725C : Result := False ;
          amAxoclamp2 : Result := False ;
          amDaganTEV200A : Result := False ;
          amTriton : Result := False ;
          amAxoclamp900A : Result := False ;
          else Result := False ;

          end ;
     end ;


function TAmplifier.GetGainTelegraphAvailable(
         AmpNumber : Integer ) : Boolean ;
// ------------------------------------------------------------------
// Returns TRUE if amplifier needs an analogue gain telegraph channel
// ------------------------------------------------------------------
begin

     case GetAmplifierType(AmpNumber)  of
          amNone : Result := False ;
          amManual : Result := False ;
          amCED1902 : Result := True ;
          amAxopatch1d : Result := True ;
          amAxopatch200 : Result := True ;
          amWPC100 : Result :=  True ;
          amVP500 : Result := True ;
          amRK400 : Result := True ;
          amOptopatch : Result := True ;
          amMultiClamp700A : Result := True  ;
          amMultiClamp700B : Result := True  ;
          amEPC8 : Result := True ;
          amTurboTec03,
          amTurboTec05,
          amTurboTec10C,
          amTurboTec10CX,
          amTurboTec20,
          amTurboTec30 : Result := True ;
          amAMS2400 : Result := True ;
          amSEC05LX : Result := True ;
          amDaganPCOne10M,
          amDaganPCOne100M,
          amDaganPCOne1G,
          amDaganPCOne10G : Result := True ;
          amDagan3900A10nA,amDagan3900A100nA : Result := True ;
          amWarnerPC501A : Result := True ;
          amWarnerPC505B : Result := True ;
          amWarnerOC725C : Result := True ;
          amAxoclamp2 : Result := False ;
          amDaganTEV200A : Result := True ;
          amTriton : Result := True ;
          amAxoclamp900A : Result := True ;
          else Result := False ;
          end ;
     end ;


procedure TAmplifier.GetCommandVoltageDivideFactor(
          AmpNumber : Integer ;
          var DivFactor : Single ) ;
// ------------------------------------------------------------------
// Returns patch clamp command voltage divide factor for amplifier
// ------------------------------------------------------------------
begin
     case GetAmplifierType(AmpNumber)  of
          amAxopatch1d : DivFactor := 50.0 ;
          amAxopatch200 : DivFactor := 50.0 ;
          amWPC100 : DivFactor :=  10.0 ;
          amVP500 : DivFactor := 50.0 ;
          amRK400 : DivFactor := 50.0 ;
          amOptopatch : DivFactor := 10.0 ;
          amMultiClamp700A : DivFactor := 50.0  ;
          amMultiClamp700B : DivFactor := 50.0  ;
          amEPC8 : DivFactor := 10.0 ;
          amTurboTec03,
          amTurboTec05,
          amTurboTec10C,
          amTurboTec10CX,
          amTurboTec20,
          amTurboTec30 : DivFactor := 10.0 ;
          amAMS2400 : DivFactor := 50.0 ;
          amSEC05LX : DivFactor := 10.0 ;
          amDaganPCOne10M,
          amDaganPCOne100M,
          amDaganPCOne1G,
          amDaganPCOne10G : DivFactor := 50.0 ;
          amDagan3900A10nA,amDagan3900A100nA : DivFactor := 50.0 ;
          amWarnerPC501A : DivFactor := 10.0 ;
          amWarnerPC505B : DivFactor := 10.0  ;
          amWarnerOC725C : DivFactor := 10.0  ;
          amAxoclamp2 : DivFactor := 50.0 ;
          amDaganTEV200A : DivFactor := 10.0 ;
          amTriton : DivFactor := 1.0 ;
          amAxoclamp900A : DivFactor := 50.0 ;
          end ;

     if DivFactor = 0.0 then DivFactor := 1.0 ;

     end ;


procedure TAmplifier.GetChannelSettings(
          iChan : Integer ;
          var ChanName : String ;         // Returns name of channel
          var ChanUnits : String ;        // Returns units of channel
          var ChanCalFactor : Single ;    // Returns V/Units calibration factor
          var ChanScale : Single          // Returns channel gain factor
          ) ;
// ------------------------------------------
// Get current channel settings for amplifier
// ------------------------------------------
var
   AmplifierType : Integer ;
begin

     if iChan < 2 then AmplifierType := FAmpType[1]
                  else AmplifierType := FAmpType[2] ;

     case AmplifierType of

          amNone,amManual : GetNoneChannelSettings( iChan,
                                                    ChanName,
                                                    ChanUnits,
                                                    ChanCalFactor,
                                                    ChanScale ) ;

          amCED1902 : GetCED1902ChannelSettings( iChan,
                                                 ChanName,
                                                 ChanUnits,
                                                 ChanCalFactor,
                                                 ChanScale ) ;

          amAxoPatch1D : GetAxoPatch1DChannelSettings( iChan,
                                                           ChanName,
                                                           ChanUnits,
                                                           ChanCalFactor,
                                                           ChanScale ) ;

          amAxoPatch200 : GetAxoPatch200ChannelSettings( iChan,
                                                           ChanName,
                                                           ChanUnits,
                                                           ChanCalFactor,
                                                           ChanScale ) ;

          amWPC100 : GetWPC100ChannelSettings( iChan,
                                                           ChanName,
                                                           ChanUnits,
                                                           ChanCalFactor,
                                                           ChanScale ) ;

          amRK400 : GetRK400ChannelSettings( iChan,
                                                           ChanName,
                                                           ChanUnits,
                                                           ChanCalFactor,
                                                           ChanScale ) ;

          amVP500 : GetVP500ChannelSettings( iChan,
                                                           ChanName,
                                                           ChanUnits,
                                                           ChanCalFactor,
                                                           ChanScale ) ;

          amOptoPatch : GetOptoPatchChannelSettings( iChan,
                                                           ChanName,
                                                           ChanUnits,
                                                           ChanCalFactor,
                                                           ChanScale ) ;

          amMultiClamp700A : GetMultiClampChannelSettings( iChan,
                                                           ChanName,
                                                           ChanUnits,
                                                           ChanCalFactor,
                                                           ChanScale ) ;
          amMultiClamp700B :  GetMultiClampChannelSettings( iChan,
                                                            ChanName,
                                                            ChanUnits,
                                                            ChanCalFactor,
                                                            ChanScale ) ;
          amEPC8 :  GetEPC8ChannelSettings( iChan,
                                            ChanName,
                                            ChanUnits,
                                            ChanCalFactor,
                                            ChanScale ) ;

          amTurboTec03,
          amTurboTec05,
          amTurboTec10C,
          amTurboTec10CX,
          amTurboTec20,
          amTurboTec30 : GetTurboTecChannelSettings( iChan,
                                                        ChanName,
                                                        ChanUnits,
                                                        ChanCalFactor,
                                                        ChanScale ) ;

          amAMS2400 :  GetAMS2400ChannelSettings( iChan,
                                                  ChanName,
                                                  ChanUnits,
                                                  ChanCalFactor,
                                                  ChanScale ) ;

          amSEC05LX :  GetSEC05LXChannelSettings( iChan,
                                                  ChanName,
                                                  ChanUnits,
                                                  ChanCalFactor,
                                                  ChanScale ) ;

          amDaganPCOne10M,
          amDaganPCOne100M,
          amDaganPCOne1G,
          amDaganPCOne10G : GetDaganPCOneChannelSettings( iChan,
                                                       ChanName,
                                                       ChanUnits,
                                                       ChanCalFactor,
                                                       ChanScale ) ;

          amDagan3900A10nA,
          amDagan3900A100nA : GetDagan3900AChannelSettings( iChan,
                                                       ChanName,
                                                       ChanUnits,
                                                       ChanCalFactor,
                                                       ChanScale ) ;

          amWarnerPC501A :  GetWarnerPC501AChannelSettings( iChan,
                                                  ChanName,
                                                  ChanUnits,
                                                  ChanCalFactor,
                                                  ChanScale ) ;

          amWarnerPC505B :  GetWarnerPC505BChannelSettings( iChan,
                                                  ChanName,
                                                  ChanUnits,
                                                  ChanCalFactor,
                                                  ChanScale ) ;

          amWarnerOC725C :  GetWarnerOC725CChannelSettings( iChan,
                                                  ChanName,
                                                  ChanUnits,
                                                  ChanCalFactor,
                                                  ChanScale ) ;

          amAxoclamp2 :  GetAxoclamp2ChannelSettings( iChan,
                                                  ChanName,
                                                  ChanUnits,
                                                  ChanCalFactor,
                                                  ChanScale ) ;

          amDaganTEV200A :  GetDaganTEV200AChannelSettings( iChan,
                                                  ChanName,
                                                  ChanUnits,
                                                  ChanCalFactor,
                                                  ChanScale ) ;

          amTriton :  GetTritonChannelSettings( iChan,
                                                ChanName,
                                                ChanUnits,
                                                ChanCalFactor,
                                                ChanScale ) ;

          amAxoclamp900A :  GetAxoclamp900AChannelSettings( iChan,
                                                ChanName,
                                                ChanUnits,
                                                ChanCalFactor,
                                                ChanScale ) ;

          end ;

    if ChanCalFactor = 0.0 then ChanCalFactor := 1.0 ;
    if ChanScale = 0.0 then ChanScale := 1.0 ;

    end ;


function TAmplifier.CurrentChannel(
         AmpNum : Integer
         ) : Integer ;
begin
    case GetAmplifierType(AmpNum) of
        amTriton : Result := 1 ;
        else Result := (AmpNum-1)*2 ;
        end ;
    end ;


function TAmplifier.VoltageChannel(
         AmpNum : Integer
         ) : Integer ;
begin
    case GetAmplifierType(AmpNum) of
        amTriton : Result := 0 ;
        else Result := (AmpNum-1)*2 + 1 ;
        end ;
    end ;

function TAmplifier.ChanAmpNumber( iChan : Integer ) : Integer ;
begin

    case FAmpType[1] of
        amTriton : Result := 1 ;
        else Result := (iChan div 2) + 1 ;
        end ;
    end ;




procedure TAmplifier.GetNoneChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -------------------------------------
// Get No amplifier channel settings
// -------------------------------------
begin

    ChanScale := 1.0 ;

    end ;


procedure TAmplifier.GetCED1902ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -------------------------------------
// Get CED 1902 channel settings
// -------------------------------------
begin

    // Update amplifier settings
    if not CED1902.AmplifierSet then SetCED1902 ;

    if iChan = 0 then begin
       ChanScale := CED1902.GainValue ;
       end ;

    end ;


function TAmplifier.GetAxopatch1DGain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Axopatch current gain from telegraph output
// (See Axopatch manual p.45)
// ---------------------------------------------------
const
     NumGains = 8 ;
     VGainSpacing = 0.4 ;
     VStart = 0.4 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V,AdditionalGain : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        // Axopatch gain settings
        Gains[0] := 1.0 ;
        Gains[1] := 2.0 ;
        Gains[2] := 4.0 ;
        Gains[3] := 10.0 ;
        Gains[4] := 20.0 ;
        Gains[5] := 40.0 ;
        Gains[6] := 100.0 ;
        Gains[7] := 200.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // If negative gain is X100 larger
        if V < 0.0 then AdditionalGain := 100.0
                   else AdditionalGain := 1.0 ;

        // Extract gain associated with telegraph voltage
        V := Abs(V) ;
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Min(Max(iGain,0),High(Gains)) ;
        LastGain[AmpNumber] := Gains[iGain]*AdditionalGain ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetAxopatch1DChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -------------------------------------
// Get Axon Axopatch 1D channel settings
// -------------------------------------
begin

    if (iChan = 0) or (iChan = 2)then begin
       ChanName := 'Im' ;
       ChanUnits := 'pA' ;
       ChanCalFactor := 0.0005 ;
       ChanScale := GetAxopatch1DGain( (iChan div 2) + 1 ) ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ; //
       ChanScale := 1.0 ;
       end ;

    end ;


function TAmplifier.GetAxopatch200Gain(
         AmpNumber : Integer ) : single ;
// ------------------------------------------------------
// Decode Axopatch 200 current gain from telegraph output
// (See Axopatch manual p.45)
// ------------------------------------------------------
const
     NumGains = 13 ;
     VGainSpacing = 0.5 ;
     VStart = 0.5 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        // Axopatch gain settings
        Gains[0] := 0.1 ;
        Gains[1] := 0.2 ;
        Gains[2] := 0.4 ;
        Gains[3] := 1.0 ;
        Gains[4] := 2.0 ;
        Gains[5] := 4.0 ;
        Gains[6] := 10.0 ;
        Gains[7] := 20.0 ;
        Gains[8] := 40.0 ;
        Gains[9] := 100.0 ;
        Gains[10] := 200.0 ;
        Gains[11] := 400.0 ;
        Gains[12] := 1000.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        V := Abs(V) ;
        iGain := Trunc( (V - VStart + (VGainSpacing*0.4))/VGainSpacing ) ;
        iGain := Min(Max(iGain,0),High(Gains)) ;
        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetAxopatch200ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -------------------------------------
// Get Axon Axopatch 200 channel settings
// -------------------------------------
const
    VTrack = 4 ;
    VClamp = 6 ;
    IClampZero = 3 ;
    IClampNormal = 2 ;
    IClampFast = 1 ;
var
   V : single ;
begin

    // Get mode telegraph value
    if (not ADCInUse) then begin
       if FModeTelegraphChannel[ChanAmpNumber(iChan)] > 0 then begin
          // Get voltage from telegraph channel
          V := GetTelegraphVoltage( FModeTelegraphChannel[ChanAmpNumber(iChan)] ) ;
          if V >= (IClampZero + 0.5) then LastMode[ChanAmpNumber(iChan)] := VClampMode
                                     else LastMode[ChanAmpNumber(iChan)] := IClampMode ;
          end
        else LastMode[ChanAmpNumber(iChan)] := VClampMode ;
        end ;

    if (iChan = 0) or (iChan = 2) then begin
       // Channel 0 or 2
       if LastMode[ChanAmpNumber(iChan)] = VClampMode then begin
          // Voltage-clamp mode
          ChanName := 'Im' ;
          ChanUnits := 'pA' ;
          ChanCalFactor := 0.0005 ;
          end
       else begin
          // Current-clamp mode
          ChanName := 'Vm' ;
          ChanUnits := 'mV' ;
          ChanCalFactor := 0.0005 ;
          end ;
       ChanScale := GetAxopatch200Gain((iChan div 2)+1) ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       if LastMode[ChanAmpNumber(iChan)] = VClampMode then begin
          // Voltage-clamp mode
          ChanName := 'Vm' ;
          ChanUnits := 'mV' ;
          ChanCalFactor := 0.01 ; //
          end
       else begin
          // Current-clamp mode
          ChanName := 'Im' ;
          ChanUnits := 'pA' ;
          ChanCalFactor := 0.001 ;
          end ;
       ChanScale := 1.0 ;
       end ;

    end ;


function TAmplifier.GetWPC100Gain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode WPC-100 current gain from telegraph output
// (See WPC-100 manual p.11)
// ---------------------------------------------------
const
     NumGains = 10 ;
     VGainSpacing = 0.5 ;
     VStart = 2.0 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        // Gain settings
        Gains[0] := 1.0 ;
        Gains[1] := 2.0 ;
        Gains[2] := 4.0 ;
        Gains[3] := 10.0 ;
        Gains[4] := 20.0 ;
        Gains[5] := 40.0 ;
        Gains[6] := 100.0 ;
        Gains[7] := 200.0 ;
        Gains[8] := 400.0 ;
        Gains[9] := 1000.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Min(Max(iGain,0),High(Gains)) ;
        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


function TAmplifier.GetTelegraphVoltage(
         ChannelNum : Integer
          ) : Single ;
// ------------------------------------------
// Return amplifier telegraph channel voltage
// ------------------------------------------
var
    ADCValue : Integer ;
    Device : Integer ;
begin

     Result := 0.0 ;

     if (MainFrm.IOConfig.ADCIn < 0) or
        (MainFrm.IOConfig.ADCIn > MaxResources) then Exit ;

     Device := LabIO.Resource[MainFrm.IOConfig.ADCIn].Device ;
     if LabIO.ADCMaxValue[Device] <= 0.0 then Exit ;

     ADCValue := LabIO.ReadADC( Device,
                                ChannelNum,
                                LabIO.ADCVoltageRanges[Device,0] ) ;

     Result := (ADCValue/LabIO.ADCMaxValue[Device])* LabIO.ADCVoltageRanges[Device,0] ;

     end ;


function TAmplifier.ADCInUse : Boolean ;
// -----------------------------------
// Return TRUE if A/D converter in use
// -----------------------------------
begin

     if (MainFrm.IOConfig.ADCIn < 0) or
        (MainFrm.IOConfig.ADCIn > MaxResources) then begin
        Result := True ;
        Exit ;
        end ;
     Result := LabIO.ADCActive[LabIO.Resource[MainFrm.IOConfig.ADCIn].Device] ;
     end ;


procedure TAmplifier.GetWPC100ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------
// Get WPC 100 channel settings
// -----------------------------
begin

    if (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'pA' ;
       ChanCalFactor := 0.0005 ;
       ChanScale := GetWPC100Gain(ChanAmpNumber(iChan)) ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ; //
       ChanScale := 1.0 ;
       end ;
    end ;


function TAmplifier.GetRK400Gain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Biologic RK400 current gain from telegraph output
// ---------------------------------------------------
const
     NumGains = 10 ;
     VGainSpacing = 0.4 ;
     VStart = 0.4 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   BaseGain : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        // Gain settings
        Gains[0] := 1.0 ;
        Gains[1] := 2.0 ;
        Gains[2] := 5.0 ;
        Gains[3] := 10.0 ;
        Gains[4] := 20.0 ;
        Gains[5] := 50.0 ;
        Gains[6] := 100.0 ;
        Gains[7] := 200.0 ;
        Gains[8] := 500.0 ;
        Gains[9] := 1000.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Negative telegraph voltages indicated X100 gain boost
        if V < 0.0 then BaseGain := 100.0
                   else BaseGain := 1.0 ;
        V := Abs(V) ;

        // Extract gain associated with telegraph voltage
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Min(Max(iGain,0),High(Gains)) ;
        LastGain[AmpNumber] := BaseGain*Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetRK400ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------
// Get RK400 channel settings
// -----------------------------
begin

    if (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'pA' ;
       ChanCalFactor := 0.0001 ;
       ChanScale := GetRK400Gain(ChanAmpNumber(iChan)) ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ; //
       ChanScale := 1.0 ;
       end ;
    end ;


function TAmplifier.GetOptopatchGain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Cairn Optopatch current gain from telegraph output
// ---------------------------------------------------
const
     NumGains = 18 ;
     VGainSpacing = 0.5 ;
     VStart = 1.0 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        // Gain settings
        Gains[0] := 1.0 ;
        Gains[1] := 2.0 ;
        Gains[2] := 5.0 ;
        Gains[3] := 10.0 ;
        Gains[4] := 20.0 ;
        Gains[5] := 50.0 ;
        Gains[6] := 100.0 ;
        Gains[7] := 200.0 ;
        Gains[8] := 500.0 ;
        Gains[9] := 1000.0 ;
        Gains[10] := 2000.0 ;
        Gains[11] := 5000.0 ;
        Gains[12] := 10000.0 ;
        Gains[13] := 20000.0 ;
        Gains[14] := 50000.0 ;
        Gains[15] := 100000.0 ;
        Gains[16] := 200000.0 ;
        Gains[17] := 500000.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Min(Max(iGain,0),High(Gains)) ;
        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetOptoPatchChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------
// Get Cairn OptoPatch channel settings
// -----------------------------
var
   V : single ;
begin

    // Get mode telegraph value
    if (not ADCInUse) then begin
       if FModeTelegraphChannel[ChanAmpNumber(iChan)] > 0 then begin
          // Get voltage from telegraph channel
          V := GetTelegraphVoltage( FModeTelegraphChannel[ChanAmpNumber(iChan)] ) ;
          if V >= 2.5 then LastMode[ChanAmpNumber(iChan)] := VClampMode
                      else LastMode[ChanAmpNumber(iChan)] := IClampMode ;
          end
        else LastMode[ChanAmpNumber(iChan)] := VClampMode ;
        end ;

    if (iChan = 0) or (iChan = 2) then begin
       // Channel 0 or 2
       if LastMode[ChanAmpNumber(iChan)] = VClampMode then begin
          // Voltage-clamp mode
          ChanName := 'Im' ;
          ChanUnits := 'pA' ;
          ChanCalFactor := 0.0001 ;
          end
       else begin
          // Current-clamp mode
          ChanName := 'Vm' ;
          ChanUnits := 'mV' ;
          ChanCalFactor := 0.0001 ;
          end ;
       ChanScale := GetOptopatchGain(ChanAmpNumber(iChan))  ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       if LastMode[ChanAmpNumber(iChan)] = VClampMode then begin
          // Voltage-clamp mode
          ChanName := 'Vm' ;
          ChanUnits := 'mV' ;
          ChanCalFactor := 0.01 ; //
          end
       else begin
          // Current-clamp mode
          ChanName := 'Im' ;
          ChanUnits := 'pA' ;
          ChanCalFactor := 0.01 ;
          end ;
       ChanScale := 1.0 ;
       end ;

    end ;


function TAmplifier.GetVP500Gain(
         AmpNumber : Integer ) : single ;
// -------------------------------------
// Get current gain from VP500 amplifier
// -------------------------------------
var
     VP500 : THardwareConf ;
     Err : Integer ;
begin
     Err := VP500_GetHardwareConf( @VP500 ) ;
     if Err >= 0 then Result := VP500.TotalGain
                 else Result := 1.0 ;
     end ;


procedure TAmplifier.GetVP500ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------
// Get Biologic VP500 channel settings
// -----------------------------
begin

    if  (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'pA' ;
       ChanCalFactor := 0.001 ;
       ChanScale := GetVP500Gain(ChanAmpNumber(iChan)) ;
       end
    else if  (iChan = 1) or (iChan = 3)  then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.05 ; //
       ChanScale := 1.0 ;
       end ;

    end ;


function TAmplifier.GetMultiClampGain(
         AmpNumber : Integer ) : single ;
// --------------------------
// Get Axon Multi-Clamp gain
// --------------------------
var
    lParam : Cardinal ;
    i : Integer ;
begin

    if not MCConnectionOpen then begin
       // Register messages
       MCOpenMessageID := RegisterWindowMessage(MCTG_OPEN_MESSAGE_STR) ;
       MCCloseMessageID := RegisterWindowMessage(MCTG_CLOSE_MESSAGE_STR) ;
       MCRequestMessageID := RegisterWindowMessage(MCTG_REQUEST_MESSAGE_STR) ;
       MCReconnectMessageID := RegisterWindowMessage(MCTG_RECONNECT_MESSAGE_STR) ;
       MCBroadcastMessageID := RegisterWindowMessage(MCTG_BROADCAST_MESSAGE_STR) ;
       MCIDMessageID := RegisterWindowMessage(MCTG_ID_MESSAGE_STR) ;
       // Request MultiClamps to identify themselves

       // Clear available channel list
       for i := 0 to High(MCChannels) do MCChannels[i] := 0 ;
       MCNumChannels := 0 ;
       lParam := 0 ;
       if not PostMessage( HWND_BROADCAST, MCBroadcastMessageID, Application.Handle, lParam )
       then ShowMessage( 'MultiClamp Commander (Broadcast Message Failed)' ) ;
       MCConnectionOpen := True ;
       end ;

    Result := MCTelegraphData[AmpNumber].PrimaryAlpha ;
    if Result = 0.0 then Result := 1.0 ;
    end ;


procedure TAmplifier.GetMultiClampChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// --------------------------
// Get Axon Multi-Clamp channel settings
// --------------------------
var
    lParam : Cardinal ;
    i : Integer ;
    Units : Integer ;
    ChanNames : Array[0..MCTG_OUT_MUX_MAXCHOICES-1] of String ;
begin

    // Exit if not one patch clamp channels
    if iChan > 3 then Exit ;

    if MCConnectionOpen then begin

       for i := 0 to High(ChanNames) do begin

           ChanNames[i] := format('%d',[i]) ;

           if MCTelegraphData[ChanAmpNumber(iChan)].HardwareType = 1 then begin
              if i < High(MCTG_ChanNames700B) then ChanNames[i] := MCTG_ChanNames700B[i] ;
              end
           else if i <= High(MCTG_ChanNames700A_VC) then begin
              if MCTelegraphData[ChanAmpNumber(iChan)].OperatingMode = MCTG_MODE_VCLAMP then begin
                 ChanNames[i] := MCTG_ChanNames700A_VC[i] ;
                 end
              else begin
                 ChanNames[i] := MCTG_ChanNames700A_CC[i] ;
                 end ;
              end ;
           end ;


       // Voltage clamp mode
       if (iChan = 0) or (iChan = 2) then begin

          if (MCTelegraphData[ChanAmpNumber(iChan)].PrimaryScaledOutSignal >= 0) and
             (MCTelegraphData[ChanAmpNumber(iChan)].PrimaryScaledOutSignal < High(ChanNames)) then
             ChanName := ChanNames[MCTelegraphData[ChanAmpNumber(iChan)].PrimaryScaledOutSignal] ;

          Units := MCTelegraphData[ChanAmpNumber(iChan)].PrimaryScaleFactorUnits ;
          ChanCalFactor := MCTelegraphData[ChanAmpNumber(iChan)].PrimaryScaleFactor ;
          ChanScale := MCTelegraphData[ChanAmpNumber(iChan)].PrimaryAlpha ;
          end
       else if (iChan = 1) or (iChan = 3) then begin

          if (MCTelegraphData[ChanAmpNumber(iChan)].SecondaryOutSignal >= 0) and
             (MCTelegraphData[ChanAmpNumber(iChan)].SecondaryOutSignal < High(ChanNames)) then
             ChanName := ChanNames[MCTelegraphData[ChanAmpNumber(iChan)].SecondaryOutSignal] ;

          Units := MCTelegraphData[ChanAmpNumber(iChan)].SecondaryScaleFactorUnits ;
          ChanCalFactor := MCTelegraphData[ChanAmpNumber(iChan)].SecondaryScaleFactor ;
          ChanScale := MCTelegraphData[ChanAmpNumber(iChan)].SecondaryAlpha ;
          end ;

       // Convert to WinWCP preferred units (mV, pA)

       case Units of
           MCTG_UNITS_VOLTS_PER_VOLT : Begin
              ChanCalFactor := ChanCalFactor*0.001 ;
              ChanUnits := 'mV' ;
              end ;
           MCTG_UNITS_VOLTS_PER_MILLIVOLT : Begin
              ChanUnits := 'mV' ;
              end ;
           MCTG_UNITS_VOLTS_PER_MICROVOLT : Begin
              ChanUnits := 'uV' ;
              end ;
           MCTG_UNITS_VOLTS_PER_AMP : Begin
              ChanCalFactor := ChanCalFactor*1E-12 ;
              ChanUnits := 'pA' ;
              end ;
           MCTG_UNITS_VOLTS_PER_MILLIAMP : Begin
              ChanCalFactor := ChanCalFactor*1E-9 ;
              ChanUnits := 'pA' ;
              end ;
           MCTG_UNITS_VOLTS_PER_MICROAMP : Begin
              ChanCalFactor := ChanCalFactor*1E-6 ;
              ChanUnits := 'pA' ;
              end ;
           MCTG_UNITS_VOLTS_PER_NANOAMP : Begin
              ChanCalFactor := ChanCalFactor*1E-3 ;
              ChanUnits := 'pA' ;
              end ;
           MCTG_UNITS_VOLTS_PER_PICOAMP : Begin
              ChanCalFactor := ChanCalFactor*1E-3 ;
              ChanUnits := 'pA' ;
              end ;
           else begin
              ChanUnits := '?' ;
              end ;
           end ;

       if ChanScale = 0.0 then ChanScale := 1.0 ;
       if ChanCalFactor = 0.0 then ChanCalFactor := 1.0 ;
       end
    else begin
       // Open connection \

       // Register messages
       MCOpenMessageID := RegisterWindowMessage(MCTG_OPEN_MESSAGE_STR) ;
       MCCloseMessageID := RegisterWindowMessage(MCTG_CLOSE_MESSAGE_STR) ;
       MCRequestMessageID := RegisterWindowMessage(MCTG_REQUEST_MESSAGE_STR) ;
       MCReconnectMessageID := RegisterWindowMessage(MCTG_RECONNECT_MESSAGE_STR) ;
       MCBroadcastMessageID := RegisterWindowMessage(MCTG_BROADCAST_MESSAGE_STR) ;
       MCIDMessageID := RegisterWindowMessage(MCTG_ID_MESSAGE_STR) ;
       // Request MultiClamps to identify themselves

       // Clear available channel list
       for i := 0 to High(MCChannels) do MCChannels[i] := 0 ;
       MCNumChannels := 0 ;
       lParam := 0 ;
       if not PostMessage( HWND_BROADCAST, MCBroadcastMessageID, Application.Handle, lParam )
       then ShowMessage( 'MultiClamp Commander (Broadcast Message Failed)' ) ;
       MCConnectionOpen := True ;

       end ;

    end ;


function TAmplifier.GetTurboTecGain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode NPI TurboTec current gain from telegraph output
// ---------------------------------------------------
const
     MaxGains = 8 ;
     VGainSpacing = 1.0 ;
var
   Gains : Array[0..MaxGains-1] of single ;
   NumGains : Integer ;
   VStart : Single ;
   V : single ;
   iGain : Integer ;
begin

     case FAmpType[AmpNumber] of

        amTurboTec03,amTurboTec05,amTurboTec10CX,amTurboTec10C : Begin
           NumGains := 8 ;
           VStart := 0.0 ;
           Gains[0] := 1.0 ;
           Gains[1] := 1.25 ;
           Gains[2] := 2.0 ;
           Gains[3] := 5.0 ;
           Gains[4] := 10.0 ;
           Gains[5] := 20.0 ;
           Gains[6] := 50.0 ;
           Gains[7] := 100.0 ;
           end ;
        else begin
           // TurboTec 10C, 20, 30
           VStart := 1.0 ;
           NumGains := 6 ;
           Gains[0] := 1.0 ;
           Gains[1] := 2.0 ;
           Gains[2] := 5.0 ;
           Gains[3] := 10.0 ;
           Gains[4] := 50.0 ;
           Gains[5] := 100.0 ;
           end ;
        end ;

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        // Gain settings

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Min(Max(iGain,0),High(Gains)) ;
        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetTurboTecChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get NPI TurboTec10C channel settings
// -----------------------------------
begin

    if (iChan = 0) or (iChan = 2) then begin

       ChanName := 'Im' ;
       ChanUnits := 'uA' ;

       case FAmpType[ChanAmpNumber(iChan)] of
          amTurboTEC03 : ChanCalFactor := 0.1 ;
          amTurboTEC05 : ChanCalFactor := 0.1 ;
          amTurboTEC10C : ChanCalFactor := 0.1 ;
          amTurboTEC10CX : ChanCalFactor := 0.1 ;
          amTurboTEC20 : ChanCalFactor := 1.0 ;
          amTurboTEC30 : ChanCalFactor := 0.01 ;
          end ;

       ChanScale := GetTurboTecGain(ChanAmpNumber(iChan)) ;

       end
    else if (iChan = 1) or (iChan = 3)  then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ; //
       ChanScale := 1.0 ;
       end ;
    end ;




function TAmplifier.GetAMS2400Gain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode A-M Systems 2400 current gain from telegraph output
// ---------------------------------------------------
const
     NumGains = 16 ;
     VGainSpacing = 0.5 ;
     VStart = 0.3 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        // Gain settings
        Gains[0] := 1.0 ;
        Gains[1] := 2.0 ;
        Gains[2] := 5.0 ;
        Gains[3] := 10.0 ;
        Gains[4] := 20.0 ;
        Gains[5] := 50.0 ;
        Gains[6] := 100.0 ;
        Gains[7] := 200.0 ;
        Gains[8] := 500.0 ;
        Gains[9] := 1000.0 ;
        Gains[10] := 2000.0 ;
        Gains[11] := 5000.0 ;
        Gains[12] := 10000.0 ;
        Gains[13] := 20000.0 ;
        Gains[14] := 50000.0 ;
        Gains[15] := 100000.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        if V < (VStart-0.1) then iGain := 0
        else begin
           iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) + 1 ;
           end ;
        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetAMS2400ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get A-M Systems 2400  channel settings
// -----------------------------------
begin

    if (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'pA' ;
       ChanCalFactor := 0.00001 ;
       ChanScale := GetAMS2400Gain(ChanAmpNumber(iChan)) ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ; //
       ChanScale := 1.0 ;
       end ;
    end ;


function TAmplifier.GetEPC8Gain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Biologic RK400 current gain from telegraph output
// ---------------------------------------------------
const
     NumGains = 6 ;
     NumRanges = 3 ;
var
   Gains : Array[0..NumGains-1] of single ;
   RangeMultiplier : Array[0..NumRanges-1] of single ;
   iGain,iRange : Integer ;
   Bits : Integer ;
begin

     // Gain settings
     Gains[0] := 0.5 ;
     Gains[1] := 1.0 ;
     Gains[2] := 2.0 ;
     Gains[3] := 5.0 ;
     Gains[4] := 10.0 ;
     Gains[5] := 20.0 ;

     RangeMultiplier[0] := 100.0 ; // Medium Gain
     RangeMultiplier[1] := 10000.0 ; // High Gain
     RangeMultiplier[2] := 1.0 ; // Low Gain

     // Read digital I/P lines
     //Bits := Main.SESLabIO.DIGInputs ;

     // Decode gain
     iGain := Bits and $7 ;
     iRange := (Bits shr 3) and $3 ;
     iRange := Min( iRange, 2 ) ;

     Result := Gains[iGain]*RangeMultiplier[iRange] ;

     end ;


procedure TAmplifier.GetEPC8ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -------------------------------------
// Get Heka EPC-8 channel settings
// -------------------------------------
begin

    if  (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'pA' ;
       ChanCalFactor := 0.00001 ;
       ChanScale := GetEPC8Gain(ChanAmpNumber(iChan)) ;
       end
    else if  (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ; //
       ChanScale := 1.0 ;
       end ;
    end ;


function TAmplifier.GetSEC05LXGain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode NPI SEC05LX current gain from telegraph output
// ---------------------------------------------------
const
     NumGains = 7 ;
     VGainSpacing = 1.0 ;
     VStart = 1.0 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        // Gain settings
        Gains[0] := 1.0 ;
        Gains[1] := 2.0 ;
        Gains[2] := 5.0 ;
        Gains[3] := 10.0 ;
        Gains[4] := 20.0 ;
        Gains[5] := 50.0 ;
        Gains[6] := 100.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Max(Min(iGain,NumGains-1),0);

        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetSEC05LXChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get NPI SEC05LX channel settings
// -----------------------------------
begin

    if  (iChan = 0) or (iChan = 2)  then begin
       ChanName := 'Im' ;
       ChanUnits := 'nA' ;
       ChanCalFactor := 0.1 ;
       ChanScale := GetSEC05LXGain(ChanAmpNumber(iChan)) ;
       end
    else if  (iChan = 1) or (iChan = 3)  then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ; //
       ChanScale := 1.0 ;
       end ;
    end ;


function TAmplifier.GetDaganPCOneGain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Dagan PC-One current gain from telegraph output
// ---------------------------------------------------
const
     NumGains = 8 ;
     VGainSpacing = 0.4 ;
     VStart = 0.2 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        // Gain settings
        Gains[0] := 1 ;
        Gains[1] := 2 ;
        Gains[2] := 5 ;
        Gains[3] := 10 ;
        Gains[4] := 20 ;
        Gains[5] := 50.0 ;
        Gains[6] := 100.0 ;
        Gains[7] := 200.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Max(Min(iGain,NumGains-1),0);

        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetDaganPCOneChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get Dagan PC-One channel settings
// -----------------------------------
begin

    if (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'pA' ;
       case GetAmplifierType(ChanAmpNumber(iChan)) of
            amDaganPCOne10M : ChanCalFactor := 0.00001 ;
            amDaganPCOne100M : ChanCalFactor := 0.0001 ;
            amDaganPCOne1G : ChanCalFactor := 0.001 ;
            amDaganPCOne10G : ChanCalFactor := 0.01 ;
            else ChanCalFactor := 0.0001 ;
            end ;
       ChanScale := GetDaganPCOneGain(ChanAmpNumber(iChan)) ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ;
       ChanScale := 1.0 ;
       end ;
    end ;


function TAmplifier.GetDagan3900AGain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Dagan 3900A current gain from telegraph output
// ---------------------------------------------------
const
     NumGains = 8 ;
     VGainSpacing = 0.4 ;
     VStart = 0.4 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        // Gain settings
        Gains[0] := 1 ;
        Gains[1] := 2 ;
        Gains[2] := 5 ;
        Gains[3] := 10 ;
        Gains[4] := 20 ;
        Gains[5] := 50.0 ;
        Gains[6] := 100.0 ;
        Gains[7] := 500.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Max(Min(iGain,NumGains-1),0);

        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetDagan3900AChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get Dagan PC-One channel settings
// -----------------------------------
begin

    if (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'pA' ;
       case GetAmplifierType(ChanAmpNumber(iChan)) of
            amDagan3900A10nA : ChanCalFactor := 0.001 ;
            amDagan3900A100nA : ChanCalFactor := 0.0001 ;
            end ;
       ChanScale := GetDagan3900AGain(ChanAmpNumber(iChan)) ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ;
       ChanScale := 1.0 ;
       end ;
    end ;


function TAmplifier.GetWarnerPC501AGain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Warner PC501A current gain from telegraph output
// ---------------------------------------------------
const
     NumGains = 21 ;
     VGainSpacing = 0.2 ;
     VStart = 0.2 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        // 100 MOhm HS Gain settings
        Gains[0] := 1.0  ;
        Gains[1] := 2.0  ;
        Gains[2] := 5.0  ;
        Gains[3] := 10.0  ;
        Gains[4] := 20.0  ;
        Gains[5] := 50.0 ;
        Gains[6] := 100.0 ;

        // 1 GOhm HS Gain settings
        Gains[7] := 10.0  ;
        Gains[8] := 20.0  ;
        Gains[9] := 50.0  ;
        Gains[10] := 100.0  ;
        Gains[11] := 200.0  ;
        Gains[12] := 500.0 ;
        Gains[13] := 1000.0 ;

        // 10 GOhm HS Gain settings
        Gains[14] := 100.0 ;
        Gains[15] := 200.0 ;
        Gains[16] := 500.0 ;
        Gains[17] := 1000.0 ;
        Gains[18] := 2000.0 ;
        Gains[19] := 5000.0 ;
        Gains[20] := 10000.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Max(Min(iGain,NumGains-1),0);

        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;

     end ;


procedure TAmplifier.GetWarnerPC501AChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get Warner PC501A channel settings
// -----------------------------------
begin

    if (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'pA' ;
       ChanCalFactor := 0.0001 ;
       ChanScale := GetWarnerPC501AGain(ChanAmpNumber(iChan)) ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ;
       ChanScale := 1.0 ;
       end ;
    end ;


function TAmplifier.GetWarnerPC505BGain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Warner PC505B current gain from telegraph output
// ---------------------------------------------------
const
     NumGains = 14 ;
     VGainSpacing = 0.5 ;
     VStart = 0.5 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        Gains[0] := 1.0  ;
        Gains[1] := 2.0  ;
        Gains[2] := 4.0  ;
        Gains[3] := 10.0  ;
        Gains[4] := 20.0  ;
        Gains[5] := 40.0 ;
        Gains[6] := 100.0 ;
        Gains[7] := 200.0  ;
        Gains[8] := 400.0  ;
        Gains[9] := 1000.0  ;
        Gains[10] := 2000.0  ;
        Gains[11] := 4000.0  ;
        Gains[12] := 10000.0 ;
        Gains[13] := 20000.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Max(Min(iGain,NumGains-1),0);

        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetWarnerPC505BChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get Warner PC505B channel settings
// -----------------------------------
begin

    if (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'pA' ;
       ChanCalFactor := 0.00005 ;
       ChanScale := GetWarnerPC505BGain(ChanAmpNumber(iChan)) ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ;
       ChanScale := 1.0 ;
       end ;
    end ;


function TAmplifier.GetWarnerOC725CGain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Warner OC725C current gain from telegraph output
// ---------------------------------------------------
const
     NumGains = 13 ;
     VGainSpacing = 0.2 ;
     VStart = 0.2 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        Gains[0] := 1.0  ;
        Gains[1] := 2.0  ;
        Gains[2] := 5.0  ;
        Gains[3] := 10.0  ;
        Gains[4] := 20.0  ;
        Gains[5] := 50.0 ;
        Gains[6] := 100.0 ;
        Gains[7] := 200.0  ;
        Gains[8] := 500.0  ;
        Gains[9] := 1000.0  ;
        Gains[10] := 2000.0  ;
        Gains[11] := 5000.0  ;
        Gains[12] := 10000.0 ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Max(Min(iGain,NumGains-1),0);

        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetWarnerOC725CChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get Warner OC725C voltage clamp channel settings
// -----------------------------------
begin

    if (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'uA' ;
       ChanCalFactor := 0.01 ;
       ChanScale := GetWarnerOC725CGain(ChanAmpNumber(iChan)) ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ;
       ChanScale := 1.0 ;
       end ;
    end ;

function TAmplifier.GetAxoclamp2Gain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Axoclamp current gain
// ---------------------------------------------------
begin

     Result := 1.0 ;

     end ;


procedure TAmplifier.GetAxoclamp2ChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get Axoclamp 2 voltage clamp channel settings
// -----------------------------------
begin

    if (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'nA' ;
       ChanCalFactor := 0.01 ;
       ChanScale := 1.0 ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ;
       ChanScale := 1.0 ;
       end ;
    end ;


function TAmplifier.GetDaganTEV200AGain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Dagan TEV200A current gain from telegraph output
// ---------------------------------------------------
const
     NumGains = 8 ;
     VGainSpacing = 0.4 ;
     VStart = 0.4 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not ADCInUse then begin

        Gains[0] := 1.0  ;
        Gains[1] := 2.0  ;
        Gains[2] := 4.0  ;
        Gains[3] := 10.0  ;
        Gains[4] := 20.0  ;
        Gains[5] := 40.0 ;
        Gains[6] := 100.0 ;
        Gains[7] := 200.0  ;

        // Get voltage from telegraph channel
        V := GetTelegraphVoltage( FGainTelegraphChannel[AmpNumber] ) ;

        // Extract gain associated with telegraph voltage
        iGain := Trunc( (V - VStart + 0.1)/VGainSpacing ) ;
        iGain := Max(Min(iGain,NumGains-1),0);

        LastGain[AmpNumber] := Gains[iGain] ;

        end ;

     Result := LastGain[AmpNumber] ;
     end ;


procedure TAmplifier.GetDaganTEV200AChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get Warner OC725C voltage clamp channel settings
// -----------------------------------
begin

    if (iChan = 0) or (iChan = 2) then begin
       ChanName := 'Im' ;
       ChanUnits := 'uA' ;
       ChanCalFactor := 0.05 ;
       ChanScale := GetDaganTEV200AGain(ChanAmpNumber(iChan)) ;
       end
    else if (iChan = 1) or (iChan = 3) then begin
       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.01 ;
       ChanScale := 1.0 ;
       end ;
    end ;


function TAmplifier.GetTritonGain(
         AmpNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Tecella Triton current gain
// ---------------------------------------------------
const
     NumGains = 8 ;
     VGainSpacing = 0.4 ;
     VStart = 0.4 ;
var
   Gains : Array[0..NumGains-1] of single ;
   V : single ;
   iGain : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead


     Result := 1.0 ;
     end ;


procedure TAmplifier.GetTritonChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get Tecella Triton  voltage clamp channel settings
// -----------------------------------
begin

    if iChan = 0 then begin

       ChanName := 'Vm' ;
       ChanUnits := 'mV' ;
       ChanCalFactor := 0.001 ;
       ChanScale := 1.0 ;
       end
    else begin
       ChanName := format('Im%d',[iChan-1]) ;
       ChanUnits := 'pA' ;
       ChanCalFactor := 0.001 ;
       ChanScale := 1.0 ;
       end ;

    end ;

function TAmplifier.GetAxoclamp900AGain(
         ChanNumber : Integer ) : single ;
// ---------------------------------------------------
// Decode Axoclamp 900A current gain
// ---------------------------------------------------
const
     NumGains = 8 ;
     VGainSpacing = 0.4 ;
     VStart = 0.4 ;
var
   Gain : Double ;
   Mode,Err : Integer ;
begin

     // Note. Don't interrupt A/D sampling if it in progress.
     // Use most recent gain setting instead

     if not Axoclamp900AOpen then OpenAxoclamp900A ;
     if not Axoclamp900AOpen then Exit ;

     ChanNumber := Min(Max(ChanNumber,0),1) ;
     AXC_GetMode( Axoclamp900AHnd, ChanNumber, Mode, Err ) ;
     Gain := -1 ;
     AXC_GetScaledOutputGain( Axoclamp900AHnd, Gain, ChanNumber, Mode, Err ) ;

     Result := Gain ;
     end ;


procedure TAmplifier.GetAxoclamp900AChannelSettings(
          iChan : Integer ;
          var ChanName : String ;
          var ChanUnits : String ;
          var ChanCalFactor : single ;
          var ChanScale : Single
          ) ;
// -----------------------------------
// Get Axoclamp900A voltage clamp channel settings
// -----------------------------------
var
    Err,iSignal,iMode : Integer ;
    ScaleFactor,ISCale : Double ;
    HeadstageType : Integer ;
begin

    if not Axoclamp900AOpen then OpenAxoclamp900A ;
    if not Axoclamp900AOpen then Exit ;

    if (iChan mod 2) = 0 then begin

       AXC_GetMode( Axoclamp900AHnd, iChan mod 2, iMode, Err ) ;
       AXC_GetScaledOutputSignal( Axoclamp900AHnd, iSignal, iChan mod 2, iMode, Err ) ;
       ChanName := AXC_SignalName[iSignal] ;
       ChanUnits := AXC_SignalUnits[iSignal] ;

       iSignal := Min(Max(iSignal,0),High(AXC_ChanCalFactors)) ;

       //AXC_GetSignalScaleFactor( Axoclamp900AHnd, ScaleFactor, iSignal, Err ) ;
       // Get type of headstage and calculate calibration factor
       AXC_GetHeadstageType( Axoclamp900AHnd, HeadstageType, iChan mod 2, False, Err ) ;
       case HeadstageType of
          AXC_HEADSTAGE_TYPE_HS9_x10uA : IScale := 0.001 ;
          AXC_HEADSTAGE_TYPE_HS9_x1uA : IScale := 0.01 ;
          AXC_HEADSTAGE_TYPE_HS9_x100nA : IScale := 0.1 ;
          AXC_HEADSTAGE_TYPE_VG9_x10uA : IScale := 0.001 ;
          AXC_HEADSTAGE_TYPE_VG9_x100uA : IScale := 0.0001 ;
          else IScale := 0.01 ;
          end ;
       if ChanUnits = 'mV' then ChanCalFactor := 0.01
                           else ChanCalFactor := IScale ;

       if (iSignal = AXC_SIGNAL_ID_DIV10V2) or (iSignal = AXC_SIGNAL_ID_DIV10I2) then
          ChanCalFactor := ChanCalFactor*0.1 ;

       ChanScale := GetAxoclamp900AGain(iChan mod 2) ;

       end
    else begin

       AXC_GetMode( Axoclamp900AHnd, iChan mod 2, iMode, Err ) ;
       AXC_GetScaledOutputSignal( Axoclamp900AHnd, iSignal, iChan mod 2, iMode, Err ) ;

       iSignal := Min(Max(iSignal,0),High(AXC_ChanCalFactors)) ;

       ChanName := AXC_SignalName[iSignal] ;
       ChanUnits := AXC_SignalUnits[iSignal] ;

       // Get type of headstage and calculate calibration factor
       AXC_GetHeadstageType( Axoclamp900AHnd, HeadstageType, iChan mod 2, False, Err ) ;
       case HeadstageType of
          AXC_HEADSTAGE_TYPE_HS9_x10uA : IScale := 0.001 ;
          AXC_HEADSTAGE_TYPE_HS9_x1uA : IScale := 0.01 ;
          AXC_HEADSTAGE_TYPE_HS9_x100nA : IScale := 0.1 ;
          AXC_HEADSTAGE_TYPE_VG9_x10uA : IScale := 0.001 ;
          AXC_HEADSTAGE_TYPE_VG9_x100uA : IScale := 0.0001 ;
          else IScale := 0.01 ;
          end ;

       if ChanUnits = 'mV' then ChanCalFactor := 0.01
                           else ChanCalFactor := IScale ;

       if (iSignal = AXC_SIGNAL_ID_DIV10V2) or (iSignal = AXC_SIGNAL_ID_DIV10I2) then
          ChanCalFactor := ChanCalFactor*0.1 ;

       ChanScale := GetAxoclamp900AGain(iChan mod 2) ;

       end ;

    end ;


procedure TAmplifier.OpenAxoclamp900A ;
// ------------------------------------
// Open link to Axoclamp 900A commander
// ------------------------------------
var
    Err,iMode : Integer ;
    SerialNum : Array[0..15] of Char ;
    DemoMode : Boolean ;
    Devicename : Array[0..32] of Char ;
begin

     if Axoclamp900AOpen then Exit ;

     if not Axoclamp900ALibLoaded then begin
        // Load main library
        if FileExists(Axoclamp900ALibPath + 'axoclampdriver.dll') then begin
           // Look for DLLs in Axoclamp 900A folder
           Axoclamp900AHIDHnd := LoadLibrary(PChar(Axoclamp900ALibPath + 'axHIDManager.dll')) ;
           Axoclamp900ALibHnd := LoadLibrary(PChar(Axoclamp900ALibPath + 'axoclampdriver.dll')) ;
           end
        else begin
           // Look for DLLs elsewhere
           Axoclamp900AHIDHnd := LoadLibrary(PChar('axHIDManager.dll')) ;
           Axoclamp900ALibHnd := LoadLibrary(PChar('axoclampdriver.dll')) ;
           end ;
        if Axoclamp900ALibHnd <= 0 then begin
           ShowMessage( format('%s library not found',[Axoclamp900ALibPath])) ;
           Exit ;
           end ;
        end ;

     Axoclamp900ALibLoaded := True ;

     @AXC_CheckAPIVersion := LoadProcedure( Axoclamp900ALibHnd, 'AXC_CheckAPIVersion' ) ;
     @AXC_CreateHandle := LoadProcedure( Axoclamp900ALibHnd, 'AXC_CreateHandle' ) ;
     @AXC_DestroyHandle := LoadProcedure( Axoclamp900ALibHnd, 'AXC_DestroyHandle' ) ;
     @AXC_FindFirstDevice := LoadProcedure( Axoclamp900ALibHnd, 'AXC_FindFirstDevice' ) ;
     @AXC_FindNextDevice := LoadProcedure( Axoclamp900ALibHnd, 'AXC_FindNextDevice' ) ;
     @AXC_OpenDevice := LoadProcedure( Axoclamp900ALibHnd, 'AXC_OpenDevice' ) ;
     @AXC_CloseDevice := LoadProcedure( Axoclamp900ALibHnd, 'AXC_CloseDevice' ) ;
     @AXC_GetSerialNumber := LoadProcedure( Axoclamp900ALibHnd, '_AXC_GetSerialNumber@16' ) ;
     @AXC_IsDeviceOpen := LoadProcedure( Axoclamp900ALibHnd, '_AXC_IsDeviceOpen@12' ) ;
     @AXC_GetScaledOutputSignal := LoadProcedure( Axoclamp900ALibHnd, '_AXC_GetScaledOutputSignal@20' ) ;
     AXC_SetScaledOutputSignal := LoadProcedure( Axoclamp900ALibHnd, '_AXC_SetScaledOutputSignal@20' ) ;
     @AXC_GetScaledOutputGain := LoadProcedure( Axoclamp900ALibHnd, '_AXC_GetScaledOutputGain@20' ) ;
     @AXC_SetScaledOutputGain := LoadProcedure( Axoclamp900ALibHnd, '_AXC_SetScaledOutputGain@24' ) ;
     @AXC_GetMode := LoadProcedure( Axoclamp900ALibHnd, '_AXC_GetMode@16' ) ;
     @AXC_GetDeviceName := LoadProcedure( Axoclamp900ALibHnd, '_AXC_GetDeviceName@16' ) ;
     @AXC_SetMode := LoadProcedure( Axoclamp900ALibHnd, '_AXC_SetMode@16' ) ;
     @AXC_BuildErrorText := LoadProcedure( Axoclamp900ALibHnd, 'AXC_BuildErrorText' ) ;
     @AXC_GetSignalScaleFactor := LoadProcedure( Axoclamp900ALibHnd, '_AXC_GetSignalScaleFactor@16' ) ;
     @AXC_GetHeadstageType := LoadProcedure( Axoclamp900ALibHnd, '_AXC_GetHeadstageType@20' ) ;

     DemoMode := False ;
     Err := 0 ;
     Axoclamp900AHnd := AXC_CreateHandle( DemoMode, Err ) ;
     AXC_FindFirstDevice( Axoclamp900AHnd, SerialNum, High(SerialNum), Err ) ;
     if Err <> 0 then begin
        ShowMessage('ERROR! Unable to find Axoclamp 900A') ;
        end ;
//     CheckErrorAxoclamp900A(Err) ;

     if Err = 0 then begin
        AXC_OpenDevice( Axoclamp900AHnd, SerialNum, True, Err ) ;
        if Err <> 0 then begin
           ShowMessage('ERROR! Unable to open Axoclamp 900A') ;
           end ;
        end ;

     //CheckErrorAxoclamp900A(Err) ;


{     const UINT AXC_MODE_IZERO             = 0;
const UINT AXC_MODE_ICLAMP            = 1;
const UINT AXC_MODE_DCC               = 2;
const UINT AXC_MODE_HVIC              = 3;
const UINT AXC_MODE_DSEVC             = 4;
const UINT AXC_MODE_TEVC              = 5;
const UINT AXC_MAX_MODES              = 6;

const UINT AXC_MODE_NONE              = 6;
const UINT AXC_MODE_ALL               = 7;

     AXC_SetMode( Axoclamp900AHnd, 0, 2, Err ) ;
     AXC_SetScaledOutputSignal( Axoclamp900AHnd, AXC_SIGNAL_ID_I2, 0, AXC_MODE_ICLAMP, Err ) ;
     AXC_SetScaledOutputGain( Axoclamp900AHnd, 1.0, 0, 2, Err ) ;
     AXC_SetScaledOutputSignal( Axoclamp900AHnd, AXC_SIGNAL_ID_10V1, 1, AXC_MODE_ICLAMP, Err ) ;
     AXC_SetScaledOutputGain( Axoclamp900AHnd, 2.0, 1, 2, Err ) ; }

     //CheckErrorAxoclamp900A(Err) ;

     Axoclamp900AOpen := True ;

     end ;

procedure TAmplifier.CloseAxoclamp900A ;
// --------------------
// Close Axoclamp 900A
// --------------------
var

    Err : Integer ;
begin

    if Axoclamp900AOpen then begin
       AXC_CloseDevice( Axoclamp900AHnd, Err ) ;
       CheckErrorAxoclamp900A(Err) ;
       AXC_DestroyHandle(Axoclamp900AHnd) ;
       Axoclamp900AHnd := -1 ;
       Axoclamp900AOpen := False ;
       end ;

    if Axoclamp900ALibLoaded then begin
       FreeLibrary( Axoclamp900ALibHnd ) ;
       FreeLibrary( Axoclamp900AHIDHnd ) ;
       Axoclamp900ALibLoaded := False ;
       end ;

    end ;


procedure TAmplifier.CheckErrorAxoclamp900A(
          Err : Integer
          ) ;
// -------------
// Report error
// -------------
var
    ErrText : Array[0..255] of Char ;
begin
     if Err <> 0 then begin
        AXC_BuildErrorText( Axoclamp900AHnd, Err, ErrText, High(ErrText)) ;
        ShowMessage(ErrText) ;
        end ;
     end ;


// *** CED 1902 Amplifier methods ***

procedure TAmplifier.TransmitLine(
          const Line : string   { Text to be sent to Com port }
          ) ;
{ --------------------------------------
  Write a line of ASCII text to Com port
  --------------------------------------}
var
   i,nC : Integer ;
   nWritten : DWORD ;
   xBuf : array[0..258] of char ;
   Overlapped : Pointer ; //POverlapped ;
   OK : Boolean ;
begin
     { Copy command line to be sent to xMit buffer and and a CR character }
     nC := Length(Line) ;
     for i := 1 to nC do xBuf[i-1] := Line[i] ;
     xBuf[nC] := chr(13) ;
     Inc(nC) ;

    Overlapped := Nil ;
    OK := WriteFile( CED1902.ComHandle, xBuf, nC, nWritten, Overlapped ) ;
    if (not OK) or (nWRitten <> nC) then
        ShowMessage( ' Error writing to COM port ' ) ;
     end ;


function TAmplifier.Check1902Error : string ;         { Error flag returned  }
{ --------------------------------------
  Retrieve error information from 1902
  --------------------------------------}
var
   i,nC : Integer ;
   xBuf : array[0..258] of char ;
   Line : string ;
begin


     Line := '?ER;' ;
     nC := Length(Line) ;
     for i := 1 to nC do xBuf[i-1] := Line[i] ;
     xBuf[nC] := chr(13) ;
     Inc(nC) ;
     if FileWrite( CED1902.ComHandle, xBuf, nC ) = nC then begin
        Result := ReceiveLine ;
        end
     else begin
        Result := ' Error writing to COM port ' ;
        end ;
     end ;


function TAmplifier.ReceiveLine : string ;          { Return line of bytes received }
{ -------------------------------------------------------
  Read bytes from Com port until a line has been received
  -------------------------------------------------------}
const
     TimeOut = 500 ;
var
   Line : string ;
   rBuf : array[0..1] of char ;
   ComState : TComStat ;
   PComState : PComStat ;
   TimeOutTickCount : LongInt ;
   ComError,NumBytesRead : DWORD ;
   //OverLapStructure : POVERLAPPED ;
begin
     { Set time that ReceiveLine will give up at if a full line has not
       been received }
     TimeOutTickCount := GetTickCount + TimeOut ;

     PComState := @ComState ;
     Line := '' ;
     repeat
        rBuf[0] := ' ' ;
        { Find out if there are any characters in receive buffer }
        ClearCommError( CED1902.ComHandle, ComError, PComState )  ;
        NumBytesRead := 0 ;
        if ComState.cbInQue > 0 then begin
           ReadFile( CED1902.ComHandle,
                     rBuf,
                     1,
                     NumBytesRead,
                     CED1902.OverlapStructure ) ;
           end ;

        if NumBytesRead > 0 then begin
           if (rBuf[0] <> chr(13)) and (rBuf[0]<>chr(10)) then
              Line := Line + rBuf[0] ;
           end ;
        until (rBuf[0] = chr(13)) or (GetTickCount >= TimeOutTickCount) ;
     Result := Line ;
     end ;


procedure TAmplifier.SetCED1902 ;
{ ---------------------------------------------------
  Transmit gain/filter settings to CED 1902 amplifier
  ---------------------------------------------------}
var
   OK : Boolean ;
   Status : String ;
   GainList : TStringList ;
begin

     //   Ensure CED 1902 COM link is open
     if CED1902.ComHandle < 0 then OK := Amplifier.OpenCED1902
                              else OK := True ;

     // If open successful, send commands
     if OK then begin
        TransmitLine( format('IP%d;',[CED1902.Input]));
        Status := QueryCED1902('?IP') ;
        TransmitLine( format('GN%d;',[CED1902.Gain]));
        Status := QueryCED1902('?GN') ;
        TransmitLine( format('LP%d;',[CED1902.LPFilter]));
        Status := QueryCED1902('?LP') ;
        TransmitLine( format('HP%d;',[CED1902.HPFilter]));
        Status := QueryCED1902('?HP') ;
        TransmitLine( format('AC%d;',[CED1902.ACCoupled]));
        Status := QueryCED1902('?AC') ;
        TransmitLine( format('NF%d;',[CED1902.NotchFilter]));
        Status := QueryCED1902('?NF') ;

        { Set DC Offset }
        TransmitLine( 'OR1;' );
        TransmitLine( format('OF%d;',[CED1902.DCOffset]));
        Status := QueryCED1902('?OF') ;
        TransmitLine( 'OR1;') ;
        TransmitLine( format('OF%d;',[CED1902.DCOffset]));
        Status := QueryCED1902('?OF') ;


        GainList := TStringList.Create ;
        GetCED1902List( '?GS;', GainList ) ;
        // Gain value
        if GainList.Count >= CED1902.Gain then begin
           CED1902.GainValue := ExtractFloat( GainList[CED1902.Gain-1],
                                              CED1902.GainValue ) ;
           end ;                                   
        GainList.Free ;

        CloseCED1902 ;

        CED1902.AmplifierSet := True ;

        end ;
     end ;


function TAmplifier.OpenCED1902 ;
// ---------------------------------------------------
// Establish communications with CED 1902 via COM port
// ---------------------------------------------------
var
   DCB : TDCB ;           { Device control block for COM port }
   CommTimeouts : TCommTimeouts ;
begin

     if CED1902.ComPort <= 1 then CED1902.ComPort := 1 ;
     if CED1902.ComPort >= 2 then CED1902.ComPort := 2 ;

     { Open com port  }
     CED1902.ComHandle :=  CreateFile( PCHar(format('COM%d',[CED1902.ComPort])),
                                       GENERIC_READ or GENERIC_WRITE,
                                       0,
                                       Nil,
                                       OPEN_EXISTING,
                                       FILE_ATTRIBUTE_NORMAL,
                                       0) ;

     if CED1902.ComHandle >= 0 then begin

        { Get current state of COM port and fill device control block }
        GetCommState( CED1902.ComHandle, DCB ) ;
        { Change settings to those required for 1902 }
        DCB.BaudRate := CBR_9600 ;
        DCB.ByteSize := 7 ;
        DCB.Parity := EVENPARITY ;
        DCB.StopBits := ONESTOPBIT ;

        { Update COM port }
        SetCommState( CED1902.ComHandle, DCB ) ;

        { Initialise Com port and set size of transmit/receive buffers }
        SetupComm( CED1902.ComHandle, 4096, 4096 ) ;

        { Set Com port timeouts }
        GetCommTimeouts( CED1902.ComHandle, CommTimeouts ) ;
        CommTimeouts.ReadIntervalTimeout := $FFFFFFFF ;
        CommTimeouts.ReadTotalTimeoutMultiplier := 0 ;
        CommTimeouts.ReadTotalTimeoutConstant := 0 ;
        CommTimeouts.WriteTotalTimeoutMultiplier := 0 ;
        CommTimeouts.WriteTotalTimeoutConstant := 5000 ;
        SetCommTimeouts( CED1902.ComHandle, CommTimeouts ) ;
        Result := True ;
        end
     Else Result := False ;
     end ;


procedure TAmplifier.CloseCED1902 ;
//
// Close serial COM linke to CED 1902
//
begin
//   Ensure CED 1902 COM link is open
     if CED1902.ComHandle >= 0 then CloseHandle( CED1902.ComHandle ) ;
     CED1902.ComHandle := -1 ;
     end ;


 function TAmplifier.GetCED1902DCOffsetRange : single ;
 begin
      if CED1902.Input = 1 then GetCED1902DCOffsetRange := 0.0005
      else if CED1902.Input = 2 then GetCED1902DCOffsetRange := 0.5
      else if CED1902.Input = 3 then GetCED1902DCOffsetRange := 0.0005
      else if CED1902.Input = 4 then GetCED1902DCOffsetRange := 0.0005
      else if CED1902.Input = 5 then GetCED1902DCOffsetRange := 0.0001
      else GetCED1902DCOffsetRange := 0.0001 ;
      end ;


procedure TAmplifier.GetCED1902List(
           Command : string ;       { Command requesting list }
           List : TStrings   { List of strings returned from 1902 }
           ) ;
var
   NumItems,i : Integer ;
   IOBuf : string ;
begin

//   Ensure CED 1902 COM link is open
     if CED1902.ComHandle < 0 then Amplifier.OpenCED1902 ;

     { Request list of gains }
     TransmitLine( Command ) ;
     IOBuf := ReceiveLine ;
     if IOBuf = '' then begin
        TransmitLine( Command ) ;
        IOBuf := ReceiveLine ;
        end ;
     { Read  list back from 1902 }

     NumItems := ExtractInt( IOBuf ) ;
     //List.Clear ;
     for i := 0 to NumItems-1 do begin
         List.Add( ReceiveLine ) ;
         end ;

     end ;


function TAmplifier.QueryCED1902(
         Request : string         { Request 1902 command string }
         ) : string ;
// ---------------------------------
// Request information from CED 1902
// ---------------------------------
var
   IOBuf : string ;
begin

//   Ensure CED 1902 COM link is open
     if CED1902.ComHandle < 0 then Amplifier.OpenCED1902 ;

     IOBuf := '' ;

     { Request information }
     TransmitLine( Request ) ;

     // Wait for it to come back
     IOBuf := ReceiveLine ;
     if IOBuf = '' then begin
        TransmitLine( Request ) ;
        IOBuf := ReceiveLine ;
        end ;
     Result := IOBuf ;

     end ;


procedure TAmplifier.DataModuleDestroy(Sender: TObject);
// -------------------------------------
// Tidy up when data module is destroyed
// -------------------------------------
var
    i : Cardinal ;
begin
    // Close an open Multiclamp 700 telegraph connection
    if MCConnectionOpen then begin
       if MCNumChannels > 0 then begin
       for i := 0 to MCNumChannels-1 do begin
           if not PostMessage( HWND_BROADCAST, MCCloseMessageID, Application.Handle, MCChannels[i] )
           then ShowMessage( 'Multi-Clamp Commander(Failed to close channel)' ) ;
           end ;
       end ;
       MCConnectionOpen := False ;
       end ;

    // Un-hook special message handle
    Application.UnHookMainWindow(AppHookFunc);

    // Close axoclamp 900A (if it is open)
    CloseAxoclamp900A ;

    end;

function TAmplifier.LoadProcedure(
         Hnd : THandle ;       { Library DLL handle }
         Name : string         { Procedure name within DLL }
         ) : Pointer ;         { Return pointer to procedure }
var
   P : Pointer ;

begin
     P := GetProcAddress(Hnd,PChar(Name)) ;
     if {Integer(P) = Null} P = Nil then begin
        SHowMessage(format('DLL- %s not found',[Name])) ;
        end ;
     Result := P ;
     end ;



end.
