unit LabIOUnit;
{$O-}
// ---------------------------------------------------
// National Instruments NIDAQ library interface module
// ---------------------------------------------------
// (c) John Dempster, University of Strathclyde, 2002
// All Rights Reserved
// 28/2/2002
// 26.8.2002 ... Multi-channel scans now triggered from PFI_7 (ADCToMemoryExtScan)
// 27.3.2003 ... Functions call now be called without hardware being available
//               without generating errors
// 21.9.2004 ... Bug in WriteDACS fixed (was outputting large glitches when windows closed
// 14.03.2005 .. PCI 6014 and other boards with only one DMA channel now supported
// 24.03.2005 .. Multi-board ADC & DAC clocks can now be synchronised either by RTSI0 or PFI5
// 19.04.2005 ..
// 06.05.2005 .. Bugs where Device=1 constant used instead of DeviceNum fixed
//               (Caused problem with Tom Carter's system)
// 31.08.05   .. Both Traditional NIDAQ and NIDAQ-MX now supported
//               Last device doing DAC now used as master ADC/DAC timing source
//               NIDAQMX_UpdateDACBuffer disabled (DAC restart needed for DAC buffer updates
// 23.11.05 .... MemoryToDAC now has PFI0 external trigger mode (added for Ultima support)
// 23.06.06 .... DACMinValue array now set correctly (not 0) for Traditional NI-DAQ
// 28.08.06 .... DigitalWaveCapable public array now reports if devices supports
//               timed digital output waveforms
// 26.07.07 .... SamplingRate adjusted in ADCToMemoryExtScan to avoid exceeding 200 kHZ aggregate
// 17.02.08 .... Digital waveform capability now recognised in 625X boards
// 04.03.09 JD .... DACOutState and DigOutState now saved whenever static DAC or Dig write takes place
// 11.03.09 JD .... FillDIGBufWithDefaultValues added.
// 13.03.09 JD .. Memory violation which ocurred when switching to NIDAQ-MX with no NIDAQ-MX Dll
//                fixed. Card list now also cleared
// 15.06.09 NS .... NIDAQ_InitialiseNIBoards no longer re-initializes NI devices.
// 04.08.11 JD .... PCI-6733 board name now returned
// 27.01.12 JD .... LibraryHnd now THandle
// 30.04.12 JD .... Digital stimulus outputs of X series devices now detected
// 30.07.12 JD .... NIDAQMAXUpdateDACOutputBuffer & NIDAQ_UpdateDACOutputBuffer added
//                  NIDAQmx D/A outputs can now be updated without restarting MemoryToDAC
//                  Strings converted to ANSISTring, Char to ANSIChar
// 03.04.13 JD .... NIDAQMX_MemoryToDIG() Digital outputs now clocked by digital sample clock
//                  when no D/A channels in use ion timing device
// 23.04.13 JD .... Bug fix in NIDAQMX_MemoryToDAC() Data written to DAC from internal IOBuf
//                  not DACBuf. Fixes access violation when startadc called twice in RecADCOnlyUnit.pas
//                  Similar change to NIDAQMX_MemoryToDIG()
// 02.05.13 JD .... InternalBufferDuration added defining duration of internal NIDAQmx DAC and digital waveform buffer
// 09.07.13 JD .... PCI-6035E and PCI-6036E and now identified as having only one DMA channel (for A/D)
//                  PCI-6052E identified as having 2 DMA channels.
// 03.02.14 JD .... ClearNIBoardSettings added to _InitialiseBoards
// 02.09.15 JD .... USB-6002 - USB-6005 devices supported.
//                    DAQmx_Val_Falling changed to DAQmx_Val_Rising in DAQmxCfgSampClkTiming()
//                    A/D and D/A timed by own on-board clocks and synchronised by pulse P.1.0 -> PFI0+PFI1
// 11.03.16 JD .... Single Ended (RSE) analogue input mode now correctly selected
// 31.03.16 JD .... PCIe-632X boards now recognised as DigitalWaveformCapable
// 10.05.16 JD .... Digital outputs not supported by USB 621X boards
// 21.11.17 JD ... USB-600X devices D/A update interval limited to 2ms or longer to avoid
//                 intermittent 5s delays when .ADCStop called.
// 04.01.18 JD ... NIDAQMXMemoryToDAC now writes D/A values as DOUBLE voltages to avoid calibration errors
//                 with USB-6002/3
// 23.04.18 JD ... digital waveform output now supported on 628x devices
//                 NIDAQmx getadcsamples() A/D samples now read as double precision and converted to 16 bit integer
// 02.12.19 JD ... TaskHandle now defined as NativeInt rather than Integer to fix problem with NIDAQmx 19.X and later in 64 bit compiles.
// 09.12.19 JD ... NIDAQ_GetDeviceDACChannelProperties() NIDAQ_GetDeviceDACChannelProperties() now use property functions calls to determine
//                 nos. of available channels rather than incrementing through channels until errors occur.
// 03.02.20 JD ... ChannelList size increased to 10000 characters because array too small error occurs on some systems
// 13.10.21 JD ... +/-10V D/A output voltage range of devices which support more than one range (e.g. USB 6363) now correctly identified as available for use.

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, nidaqcns, math, mmsystem, strutils ;
const

    NIDAQ = 0 ;
    NIDAQMX = 1 ;
    MaxDevices = 10 ;
    MaxDACs = 16 ;
    MaxVRanges = 16 ;
    MaxDACChannels = 16 ;
    MaxResources = 200 ;
    ClockSync_RTSI0 = 0 ;
    ClockSync_PFI5 = 1 ;
    DefaultTimeOut = 1.0 ;
    MaxADCSamples = 1000000 ;

    imSingleEndedNRSE = 0  ; // Single ended A/D input mode
    imDifferential = 1 ; // Differential input mode
    imBNC2110 = 2 ;      // Standard mode for BNC-2110 panel (differential)
    imBNC2090 = 3 ;      // Standard mode for BNC 2090 panel (SE)
    imSingleEndedRSE = 4 ;          // Single Ended (grounded)

type
  TSmallIntArray = Array[0..MaxADCSamples-1] of SmallInt ;
  PSmallIntArray = ^TSmallIntArray ;
  TDoubleArray = Array[0..MaxADCSamples-1] of Double ;
  PDoubleArray = ^TDoubleArray ;


  TResourceType = (ADCIn,DACOut,DigOut,None) ;
  TResource = record
        Device : Integer ;
        ResourceType : TResourceType ;
        StartChannel : Integer ;
        EndChannel : Integer ;
        V : Single ;
        Delay : Single ;
        end ;

  TBig16bitArray =  Array[0..$1FFFFFFF] of SmallInt ;
  PBig16bitArray =  ^TBig16bitArray ;
  TBig8bitArray =   Array[0..$1FFFFFFF] of Byte ;
  PBig8bitArray =   ^TBig8bitArray ;
  TBig32bitArray =  Array[0..$1FFFFFF] of Integer ;
  PBig32bitArray =  ^TBig32bitArray ;
  TBigSingleArray = Array[0..$1FFFFFF] of Single ;
  PBigSingleArray = ^TBigSingleArray ;
  TBigDoubleArray = Array[0..$1FFFFFF] of double ;
  PBigDoubleArray = ^TBigdoubleArray ;

  TDAC = record
      Buf : PBig16bitArray ;
      Pointer : Integer ;
      NumChannels : Integer ;
      EndofBuf : Integer ;
      CircularBuffer : Boolean ;
      SpaceAvailable : Cardinal ;
      end ;

  TDIG = record
      Buf : PBig32bitArray ;
      Pointer : Integer ;
      NumChannels : Integer ;
      EndofBuf : Integer ;
      CircularBuffer : Boolean ;
      SpaceAvailable : Cardinal ;
      end ;


  TLabIO = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }

    FNIDAQAPI : Integer ;       // NI-DAQ library (NIDAQ,NIDAQMX)
    LibraryHnd : THandle ;
    LibraryLoaded : Boolean ;
    BoardInitialised : Boolean ;

//    InBuf : PSmallIntArray ; // A/D input buffer pointer
    DBuf : PBigDoubleArray ; // Double precision working array

    // NIDAQ-MX task objects
    DACTask : Array[1..MaxDevices] of NativeInt ;
    ADCTask : Array[1..MaxDevices] of NativeInt ;
    DIGTask : Array[1..MaxDevices] of NativeInt ;

    ADCVScale : Array[1..MaxDevices] of Single ;
    ADCVOffset : Array[1..MaxDevices] of Single ;
    ADCVoltageRangeAtX1Gain : Array[1..MaxDevices] of Single ;

    FADCCircularBuffer : Boolean ;

    FADCPointer : Integer ;
    FADCNumChannels : Integer ;
    FADCNumSamples : Integer ;
    FADCNumSamplesAcquired : Integer ;
    FADCVoltageRange : Array[1..MaxDevices] of Single ;                   // A/D voltage range in use

    FPUExceptionMask : Set of TFPUException ;
    FPUExceptionMaskSet : Boolean ;

    procedure DisableFPUExceptions ;
    procedure EnableFPUExceptions ;
    function  PCharArrayToString( CBuf : Array of ANSIChar ) : ANSIString ;

    // NI-DAQ MX methods
    function NIDAQMX_InitialiseNIBoards : Boolean ;
    function NIDAQMX_ResetNIBoards : Boolean ;
    procedure NIDAQMX_GetDeviceADCChannelProperties( DeviceNum : Integer ) ;
    procedure NIDAQMX_GetDeviceDACChannelProperties( DeviceNum : Integer ) ;
    function NIDAQMX_ADCToMemoryExtScan(
             Device : SmallInt ;
             var ADCBuf : Array of SmallInt  ;
             nChannels : Integer ;
             nSamples : Integer ;
             ADCVoltageRange : Single ;
             CircularBuffer : Boolean ;
             SamplingInterval : Double ;
             TimingDevice : SmallInt ;
             UseTimingDeviceDACClock : Boolean
             ) : Boolean ;
    function NIDAQMX_StopADC( Device : SmallInt ) : Boolean ;
    function NIDAQMX_ADCInputModeCode(
             Device : Integer ;
             InputMode : Integer ) : Integer ;
    procedure NIDAQMX_GetADCSamples(
              Device : Integer ;
              var ADCBuf : Array of SmallInt
              ) ;
    function NIDAQMX_MemoryToDAC(
             Device : SmallInt ;
             var DACBuf : Array of SmallInt  ;
             nChannels : SmallInt ;
             nPoints : Integer ;
             UpdateInterval : Double ;
             CircularBufferMode : Boolean ;
             ExternalTrigger : Boolean ;
             TimingDevice : SmallInt
             ) : Boolean ;

    procedure NIDAQMX_UpdateDACOutputBuffer ;

    function NIDAQMX_StopDAC( Device : SmallInt ) : Boolean ;

    procedure NIDAQMX_WriteDACs(
              Device : Integer ;
              DACVolts : array of Single ;
              nChannels : Integer ) ;

    procedure NIDAQMX_WriteDAC(
              Device : Integer ;
              DACVolts : Single ;
              iChannel : Integer ) ;

   function NIDAQMX_MemoryToDIG(
            Device : SmallInt ;
            var DIGBuf : Array of Integer  ;     { D/A output data buffer (IN) }
            nPoints : Integer ;               { No. of D/A output values (IN) }
            UpdateInterval : Double ;          { D/A output interval (s) (IN) }
            CircularBufferMode : Boolean ;     { TRUE = continuous update from circular buffer }
            ExternalTrigger : Boolean ;
            TimingDevice : SmallInt ;          // Device providing ADC/DAC timing pulse
            UseTimingDeviceDACClock : Boolean
            ): Boolean ;                      { Returns TRUE=D/A active }

    procedure NIDAQMX_UpdateDIGOutputBuffer ;

    function NIDAQMX_StopDIG( Device : SmallInt ) : Boolean ;

     function NIDAQMX_ReadADC(
              Device : Integer ;
              Channel : Integer ;
              ADCVoltageRange : Single
              ) : Integer ;
    procedure NIDAQMX_WriteToDigitalOutPutPort(
              Device : Integer ;
              Pattern : Integer
              ) ;
    procedure NIDAQMX_CheckError(
              Err : Integer
              ) ;
    procedure NIDAQMX_CheckSamplingInterval(
              DeviceNum : Integer ;
              var SamplingInterval : double ;
               NumADCChannels : Integer
              ) ;

    // Traditional NI-DAQ methods
    function NIDAQ_InitialiseNIBoards : Boolean ;
    function NIDAQ_ResetNIBoards : Boolean ;
    procedure NIDAQ_GetDeviceADCChannelProperties( DeviceNum : Integer ) ;
    procedure NIDAQ_GetDeviceDACChannelProperties( DeviceNum : Integer ) ;
    function NIDAQ_ADCToMemoryExtScan(
             Device : SmallInt ;
             var ADCBuf : Array of SmallInt  ;
             nChannels : Integer ;
             nSamples : Integer ;
             ADCVoltageRange : Single ;
             CircularBuffer : Boolean ;
             TimingDevice : SmallInt
             ) : Boolean ;
    function NIDAQ_StopADC( Device : SmallInt ) : Boolean ;
    //function NIDAQ_ADCInputModeCode : Integer ;
    function NIDAQ_MemoryToDAC(
             Device : SmallInt ;
             var DACBuf : Array of SmallInt  ;
             nChannels : SmallInt ;
             nPoints : Integer ;
             UpdateInterval : Double ;
             CircularBufferMode : Boolean ;
             ExternalTrigger : Boolean ;
             TimingDevice : SmallInt
             ) : Boolean ;
    procedure NIDAQ_UpdateDACOutputBuffer ;
    function NIDAQ_StopDAC( Device : SmallInt ) : Boolean ;
    procedure NIDAQ_WriteDACs(
              Device : Integer ;
              DACVolts : array of Single ;
              nChannels : Integer ) ;
    procedure NIDAQ_WriteDAC(
              Device : Integer ;
              DACVolts : Single ;
              iChannel : Integer ) ;
     function NIDAQ_ReadADC(
              Device : Integer ;
              Channel : Integer ;
              ADCVoltageRange : Single
              ) : Integer ;
    procedure NIDAQ_WriteToDigitalOutPutPort(
              Device : Integer ;
              Pattern : Integer
              ) ;
    procedure NIDAQ_CheckError(
              Err : Integer
              ) ;
    procedure NIDAQ_CheckSamplingInterval(
          var SamplingInterval : double ;  { Sampling interval (IN/OUT) }
          var TimeBase : SmallInt ;        { Clock timebase code (OUT) }
          var ClockTicks : Word            { No. clock ticks (OUT) }
          )  ;

  function IsLabInterfaceAvailable : boolean ;

  function  IntLimit( Value : Integer ; LoLimit : Integer ; HiLimit : Integer
          ) : Integer ;

  procedure SetNIDAQAPI( Value : Integer ) ;
  procedure ClearNIBoardSettings ;

  procedure Wait( Delay : Single ) ;

  public
    { Public declarations }

    NumDevices : Integer ;
    DeviceName : Array[1..MaxDevices] of ANSIstring ;
    DeviceBoardName : Array[1..MaxDevices] of ANSIstring ;
    DeviceBoardType : Array[1..MaxDevices] of SmallInt ;
    DeviceNumDMAChannels : Array[1..MaxDevices] of Integer ;
    NumDACs : Array[1..MaxDevices] of SmallInt ;
    NumADCs : Array[1..MaxDevices] of SmallInt ;
    DACMaxValue : Array[1..MaxDevices] of Integer ;
    DACMinValue : Array[1..MaxDevices] of Integer ;
    DACResolution : Array[1..MaxDevices] of Integer ;
    DACMaxVolts  : Array[1..MaxDevices] of Single ;
    DACMinVolts  : Array[1..MaxDevices] of Single ;
    DACScale : Array[1..MaxDevices] of Single ;

    // DAC output voltage states
    DACOutState : Array[1..MaxDevices,0..MaxDACs-1] of Single ;

    // DAC output buffer state
    DAC : Array[1..MaxDevices] of TDAC ;

    ADCInputMode : Integer ;
    ADCMaxValue : Array[1..MaxDevices] of Integer ;
    ADCMinValue : Array[1..MaxDevices] of Integer ;
    ADCResolution : Array[1..MaxDevices] of Integer ;
    ADCScale : Array[1..MaxDevices] of Single ;
    ADCVoltageRanges : Array[1..MaxDevices,0..MaxVRanges] of Single ;
    NumADCVoltageRanges : Array[1..MaxDevices] of Integer ;

    DigitalWaveformCapable : Array[1..MaxDevices] of Boolean ;
    DACClockTriggerSupported : Array[1..MaxDevices] of Boolean ;
    DigitalOutputsSupported : Array[1..MaxDevices] of Boolean ;

    DigOutState : Array[1..MaxDevices] of Integer ;
    // Digital output buffer state
    DIG : Array[1..MaxDevices] of TDIG ;

    Resource : Array[0..MaxResources-1] of TResource ;
    NumResources : Integer ;

    BoardsInitialised : Boolean ;    // A/D hardware available
    TimerAvailable : Boolean ;  // Timer hardware available

    ADCActive : Array[1..MaxDevices] of Boolean ;     { A/D sampling in progress flag }
    DACActive : Array[1..MaxDevices] of Boolean ;     { D/A output in progress flag }
    DIGActive : Array[1..MaxDevices] of Boolean ;     { D/A output in progress flag }

    ADCVoltageRangeMax : single ;  { Max. positive A/D input voltage range}
    ADCChannelOffsets : Array[0..15] of Integer ;
    ADCMinSamplingInterval : single ;
    ADCMaxSamplingInterval : single ;

    DACMinUpdateInterval : Double ;
    InternalBufferDuration : Double ;             // Duration of time held in internal NIDAQmx buffer

    t0 : Integer ;

    function Open : Boolean ;

    function InitialiseNIBoards : Boolean ;
    function ResetNIBoards : Boolean ;
    procedure GetDeviceADCChannelProperties( DeviceNum : Integer ) ;
    procedure GetDeviceDACChannelProperties( DeviceNum : Integer ) ;

    procedure Close ;

    procedure CheckError( Err : Integer ) ;


    function ADCToMemoryExtScan(
             Device : SmallInt ;
             var ADCBuf : Array of SmallInt  ;
             nChannels : Integer ;
             nSamples : Integer ;
             ADCVoltageRange : Single ;
             CircularBuffer : Boolean ;
             SamplingInterval : Double ;
             TimingDevice : SmallInt ;
             UseTimingDeviceDACClock : Boolean
             ) : Boolean ;

    procedure CheckSamplingInterval(
              DeviceNum : Integer ;
              var SamplingInterval : double ;
               NumADCChannels : Integer
              ) ;


    function StopADC( Device : SmallInt ) : Boolean ;

    function ADCInputModeCode(
             Device : Integer ;
             InputMode : Integer ) : Integer ;
    procedure GetADCInputModes( InputModes :TStrings ) ;

    procedure GetADCSamples(
          Device : Integer ;
          var ADCBuf : Array of SmallInt
          ) ;

    function MemoryToDAC(
             Device : SmallInt ;
             var DACBuf : Array of SmallInt  ;
             nChannels : SmallInt ;
             nPoints : Integer ;
             UpdateInterval : Double ;
             CircularBufferMode : Boolean ;
             ExternalTrigger : Boolean ;
             TimingDevice : SmallInt
             ) : Boolean ;

    procedure UpdateDACOutputBuffer ;

    function StopDAC( Device : SmallInt ) : Boolean ;

    procedure WriteDACs(
              Device : Integer ;
              DACVolts : array of Single ;
              nChannels : Integer ) ;

    procedure WriteDAC(
              Device : Integer ;
              DACVolts : Single ;
              iChannel : Integer ) ;

    function MemoryToDIG(
             Device : SmallInt ;
             var DIGBuf : Array of Integer  ;
             nPoints : Integer ;
             UpdateInterval : Double ;
             CircularBufferMode : Boolean ;
             ExternalTrigger : Boolean ;
             TimingDevice : SmallInt ;
             UseTimingDeviceDACClock : Boolean
             ) : Boolean ;

    procedure UpdateDIGOutputBuffer ;

    function StopDIG( Device : SmallInt ) : Boolean ;

     function ReadADC(
              Device : Integer ;
              Channel : Integer ;
              ADCVoltageRange : Single
              ) : Integer ;

   procedure WriteToDigitalOutPutPort(
             Device : Integer ;
             Pattern : Integer
             ) ;

   procedure FillDACBufWithDefaultValues(
         Dev : Integer ;            // Device #
         DACBuf : PBig16bitArray ;  // Buffer to be updated
         NumPoints : Integer // No. of points in buffer
         ) ;

   procedure FillDIGBufWithDefaultValues(
         Dev : Integer ;            // Device #
         DIGBuf : PBig32bitArray ;  // Buffer to be updated
         NumPoints : Integer // No. of points in buffer
         ) ;
         
    function GetChannelOffset(
             Chan : Integer ;
             NumChannels : Integer ) : Integer ;

    function BitMask( BitNumber : Integer ) : Integer ;

    property NIDAQAPI : Integer read FNIDAQAPI write SetNIDAQAPI ;

  end;

var
  LabIO: TLabIO;

implementation

uses Main, logunit, nidaqmxlib, nidaqlib ;

//uses Main;

{$R *.DFM}

const
   InternalClock = 0 ;
   Timebase_Ext = 0 ;
   Timebase_1us = 1 ;
   Timebase_10us = 2 ;
   Timebase_100us = 3 ;
   Timebase_1ms = 4 ;
   Timebase_10ms = 5 ;
   TimeBasePeriod : Array[-3..5] of Single = (5E-8,0.0,2E-7,0.0,1E-6,1E-5,1E-4,1E-3,1E-2) ;

type

   { NIDAQ.DLL procedure  variables }
    pi16 = ^SmallInt ;
    PSmallInt = ^SmallInt ;
    PLongInt = ^LongInt ;


procedure TLabIO.DataModuleCreate(Sender: TObject);
// --------------------------------------
// Initialisations when module is created
// --------------------------------------
begin

    BoardsInitialised := False ;
    LibraryLoaded := False ;
    FPUExceptionMaskSet := False ;

    FNIDAQAPI := NIDAQ ;

    // Clear A/D, D/A channels available
    ClearNiBoardSettings ;
    {$ifdef win32} InternalBufferDuration := 2.0 ;
    {$ELSE} InternalBufferDuration := 5.0;
    {$IFEND}

    end;

procedure TLabIO.ClearNiBoardSettings ;
// -----------------------------------------
// Clear board A/D, D/A and digital channels
// ------------------------------------------
var
    i,j : Integer ;
begin
    for i := 1 to MaxDevices do DeviceBoardName[i] := '' ;
    for i := 1 to MaxDevices do DigitalWaveformCapable[i] := False ;
    for i := 1 to MaxDevices do DigitalOutputsSupported[i] := False ;
    for i := 1 to MaxDevices do DeviceName[i] := '' ;
    for i := 1 to MaxDevices do NumDACs[i] := 0 ;
    for i := 1 to MaxDevices do NumADCs[i] := 0 ;
    for i := 1 to MaxDevices do ADCMaxValue[i] := 0 ;
    for i := 1 to MaxDevices do DACMaxVolts[i] := 0. ;
    for i := 1 to MaxDevices do NumADCVoltageRanges[i] := 0 ;
    for i := 1 to MaxDevices do ADCVoltageRangeAtX1Gain[i] := 1.0 ;
    for i := 1 to MaxDevices do for j := 0 to MaxDACs-1 do DACOutState[i,j] := 0.0 ;
    NumResources := 0 ;
    NumDevices := 0 ;

    end;


procedure TLabIO.SetNIDAQAPI( Value : Integer ) ;
// ------------------------------------------
// Set National Instrument interface API type
// ------------------------------------------
begin

    // Shut down system to allow change of API
    if BoardsInitialised then Close ;

    // Reset API
    if Value = NIDAQ then FNIDAQAPI := NIDAQ
                     else FNIDAQAPI := NIDAQMX ;

    // Re-open interface
    Open ;

    end ;


function TLabIO.Open : Boolean ;
// ---------------------------------
// Open laboratory interface for use
// ---------------------------------
var
   MinInterval : array[0..370] of single ;
   MinDACInterval : array[0..370] of single ;
   i : Integer ;
begin

     Result := False ;
     if BoardsInitialised then Exit ;

     for i := 0 to High(MinInterval) do begin
         MinInterval[i] := 1E-5 ;
         MinDACInterval[i] := 1E-4 ;
         end ;

     BoardsInitialised := InitialiseNIBoards ;

     // Create input/output buffer
//     GetMem(InBuf,MaxADCSamples*2) ;
     GetMem(DBuf,MaxADCSamples*Sizeof(Double)) ;

     Result := BoardsInitialised ;

     end ;


function TLabIO.InitialiseNIBoards : Boolean ;
// ---------------------------
// Initialise interface boards
// ---------------------------
begin
    Result := False ;
    case FNIDAQAPI of
        NIDAQMX : Result := NIDAQMX_InitialiseNIBoards ;
        NIDAQ : Result := NIDAQ_InitialiseNIBoards ;
        end ;
    end ;


function TLabIO.ResetNIBoards : Boolean ;
// ----------------------
// Reset interface boards
// ----------------------
begin
    Result := False ;
    case FNIDAQAPI of
        NIDAQMX : Result := NIDAQMX_ResetNIBoards ;
        NIDAQ : Result := NIDAQ_ResetNIBoards ;
        end ;
    end ;


procedure TLabIO.GetDeviceADCChannelProperties(
          DeviceNum : Integer
          ) ;
// -------------------------
// Get device A/D properties
// -------------------------
begin
    case FNIDAQAPI of
        NIDAQMX : NIDAQMX_GetDeviceADCChannelProperties( DeviceNum ) ;
        NIDAQ : NIDAQ_GetDeviceADCChannelProperties( DeviceNum ) ;
        end ;
    end ;


procedure TLabIO.GetDeviceDACChannelProperties( DeviceNum : Integer ) ;
// -------------------------
// Get device D/A properties
// -------------------------
begin
    case FNIDAQAPI of
        NIDAQMX : NIDAQMX_GetDeviceDACChannelProperties( DeviceNum ) ;
        NIDAQ : NIDAQ_GetDeviceDACChannelProperties( DeviceNum ) ;
        end ;
    end ;


procedure TLabIO.CheckError(
          Err : Integer
          ) ;
// ---------------------------------------
// Report information on NIDAQ error flags
// ---------------------------------------
begin
    case FNIDAQAPI of
        NIDAQMX : NIDAQMX_CheckError( Err ) ;
        NIDAQ : NIDAQ_CheckError( Err ) ;
        end ;
    end ;


procedure TLabIO.CheckSamplingInterval(
          DeviceNum : Integer ;
          var SamplingInterval : double ;
          NumADCChannels : Integer
          ) ;
// -------------------------------------------------------------
// EnsureSamplingInterval is a valid interval supported by board
// -------------------------------------------------------------
begin
    case FNIDAQAPI of
        NIDAQMX : NIDAQMX_CheckSamplingInterval( DeviceNum,SamplingInterval,NumADCChannels ) ;
//        NIDAQ : NIDAQ_CheckSamplingInterval( DeviceNum,TimeBase,ClockTicks ) ;
        end ;
    end ;


function TLabIO.ADCToMemoryExtScan(
         Device : SmallInt ;
         var ADCBuf : Array of SmallInt  ;
         nChannels : Integer ;
         nSamples : Integer ;
         ADCVoltageRange : Single ;
         CircularBuffer : Boolean ;
         SamplingInterval : Double ;
         TimingDevice : SmallInt ;
         UseTimingDeviceDACClock : Boolean
         ) : Boolean ;
// ------------------
// Start A/D sampling
// ------------------
begin
    Result := False ;
    case FNIDAQAPI of
        NIDAQMX : Result := NIDAQMX_ADCToMemoryExtScan(
                            Device,
                            ADCBuf,
                            nChannels,
                            nSamples,
                            ADCVoltageRange,
                            CircularBuffer,
                            SamplingInterval,
                            TimingDevice,
                            UseTimingDeviceDACClock ) ;
                            
        NIDAQ : Result := NIDAQ_ADCToMemoryExtScan(
                            Device,
                            ADCBuf,
                            nChannels,
                            nSamples,
                            ADCVoltageRange,
                            CircularBuffer,
                            TimingDevice ) ;
        end ;
    end ;


function TLabIO.StopADC(
         Device : SmallInt
         ) : Boolean ;
// ------------------------------------
// Stop A/D sampling on selected device
// ------------------------------------
begin
    Result := False ;
    case FNIDAQAPI of
        NIDAQMX : Result := NIDAQMX_StopADC( Device ) ;
        NIDAQ : Result := NIDAQ_StopADC( Device ) ;
        end ;
    end ;


function TLabIO.ADCInputModeCode(
         Device : Integer ;
         InputMode : Integer ) : Integer ;
// ------------------------------------
// Return code for selected A/D input mode
// ------------------------------------
begin
    Result := 0 ;
    case FNIDAQAPI of
        NIDAQMX : Result := NIDAQMX_ADCInputModeCode(Device,InputMode) ;
        //NIDAQ : Result := NIDAQ_ADCInputModeCode ;
        end ;
    end ;


procedure TLabIO.GetADCSamples(
          Device : Integer ;
          var ADCBuf : Array of SmallInt
          ) ;
// -------------------------------------
// Read A/D samples from internal buffer
// -------------------------------------
begin
    case FNIDAQAPI of
        NIDAQMX : NIDAQMX_GetADCSamples( Device, ADCBuf ) ;
        end ;
    end ;


function TLabIO.MemoryToDAC(
             Device : SmallInt ;
             var DACBuf : Array of SmallInt  ;
             nChannels : SmallInt ;
             nPoints : Integer ;
             UpdateInterval : Double ;
             CircularBufferMode : Boolean ;
             ExternalTrigger : Boolean ;
             TimingDevice : SmallInt
             ) : Boolean ;
// --------------------------------------------
// Start D/A waveform output on selected device
// --------------------------------------------
begin
    Result := False ;
    case FNIDAQAPI of
        NIDAQMX : Result := NIDAQMX_MemoryToDAC(
                  Device,
                  DACBuf,
                  nChannels,
                  nPoints,
                  UpdateInterval,
                  CircularBufferMode,
                  ExternalTrigger,
                  TimingDevice ) ;
        NIDAQ : Result := NIDAQ_MemoryToDAC(
                  Device,
                  DACBuf,
                  nChannels,
                  nPoints,
                  UpdateInterval,
                  CircularBufferMode,
                  ExternalTrigger,
                  TimingDevice ) ;
        end ;
    end ;


procedure TLabIO.UpdateDACOutputBuffer ;
// ------------------------
// Update D/A output buffer
// ------------------------
begin

    case FNIDAQAPI of
        NIDAQMX : NIDAQMX_UpdateDACOutputBuffer ;
        NIDAQ : NIDAQ_UpdateDACOutputBuffer ;
        end ;
    end ;


function TLabIO.StopDAC( Device : SmallInt ) : Boolean ;
// ------------------------------------
// Stop D/A sampling on selected device
// ------------------------------------
begin
    Result := False ;
    case FNIDAQAPI of
        NIDAQMX : Result := NIDAQMX_StopDAC( Device ) ;
        NIDAQ : Result := NIDAQ_StopDAC( Device ) ;
        end ;
    end ;


procedure TLabIO.WriteDACs(
          Device : Integer ;
          DACVolts : array of Single ;
          nChannels : Integer ) ;
// --------------------
// Write to D/A outputs
// --------------------
var
    ch : Integer ;
begin

    // Update DAC default output state
    for ch := 0 to nChannels-1 do DACOutState[Device,ch] := DACVolts[ch] ;

    case FNIDAQAPI of
        NIDAQMX : NIDAQMX_WriteDACs( Device,
                                     DACVolts,
                                     nChannels ) ;
        NIDAQ : NIDAQ_WriteDACs( Device,
                                 DACVolts,
                                 nChannels ) ;
        end ;
    end ;


procedure TLabIO.WriteDAC(
          Device : Integer ;
          DACVolts : Single ;
          iChannel : Integer ) ;
// ----------------------------
// Write to a single D/A output
// ----------------------------
begin

    // Update DAC default output state
    DACOutState[Device,iChannel] := DACVolts ;

    case FNIDAQAPI of
        NIDAQMX : NIDAQMX_WriteDAC( Device,
                                    DACVolts,
                                    iChannel ) ;
        NIDAQ : NIDAQ_WriteDAC( Device,
                                DACVolts,
                                iChannel ) ;
        end ;
    end ;


function TLabIO.MemoryToDIG(
             Device : SmallInt ;
             var DIGBuf : Array of Integer  ;
             nPoints : Integer ;
             UpdateInterval : Double ;
             CircularBufferMode : Boolean ;
             ExternalTrigger : Boolean ;
             TimingDevice : SmallInt ;
             UseTimingDeviceDACClock : Boolean
             ) : Boolean ;
// --------------------------------------------
// Start digital waveform output on selected device
// --------------------------------------------
begin
    Result := False ;
    case FNIDAQAPI of
        NIDAQMX : Result := NIDAQMX_MemoryToDIG(
                  Device,
                  DIGBuf,
                  nPoints,
                  UpdateInterval,
                  CircularBufferMode,
                  ExternalTrigger,
                  TimingDevice,
                  UseTimingDeviceDACClock ) ;
        end ;
    end ;


procedure TLabIO.UpdateDIGOutputBuffer ;
// ------------------------
// Update digital output buffer
// ------------------------
begin

    case FNIDAQAPI of
        NIDAQMX : NIDAQMX_UpdateDIGOutputBuffer ;
        end ;
        end ;


function TLabIO.StopDIG( Device : SmallInt ) : Boolean ;
// ------------------------------------
// Stop digital output on selected device
// ------------------------------------
begin
    Result := False ;
    case FNIDAQAPI of
        NIDAQMX : Result := NIDAQMX_StopDIG( Device ) ;
        end ;
    end ;



function TLabIO.ReadADC(
         Device : Integer ;
         Channel : Integer ;
         ADCVoltageRange : Single
         ) : Integer ;
// ---------------
// Read A/D inputs
// ---------------
begin
    FADCVoltageRange[Device] := ADCVoltageRange ;
    Result := 0 ;
    case FNIDAQAPI of
        NIDAQMX : Result := NIDAQMX_ReadADC( Device,
                                             Channel,
                                             ADCVoltageRange ) ;
        NIDAQ : Result := NIDAQ_ReadADC( Device,
                                         Channel,
                                         ADCVoltageRange ) ;
        end ;
    end ;


procedure TLabIO.WriteToDigitalOutPutPort(
          Device : Integer ;
          Pattern : Integer
          ) ;
// -------------------------
// Write to digital outputs
// -------------------------
begin

    // Save default digital output state
    DigOutState[Device] := Pattern ;

    if not DigitalOutputsSupported[Device] then Exit ;

    case FNIDAQAPI of
        NIDAQMX : NIDAQMX_WriteToDigitalOutPutPort( Device,Pattern ) ;
        NIDAQ : NIDAQ_WriteToDigitalOutPutPort( Device,Pattern ) ;
        end ;
    end ;

procedure TLabIO.FillDACBufWithDefaultValues(
         Dev : Integer ;            // Device #
         DACBuf : PBig16bitArray ;  // Buffer to be updated
         NumPoints : Integer // No. of points in buffer
         ) ;
// ---------------------------------------
// Update DAC buffer with default voltages
// ---------------------------------------
var
  ch,i,j,DACValue : Integer ;
begin

     if DACBuf = nil then Exit ;

     // Initialise DAC outputs to current default DAC states
     for ch := 0 to NumDACS[Dev]-1 do begin
         j := ch ;
         DACValue := Round( DACOutState[Dev,ch]*DACScale[Dev] ) ;
         for i := 0 to NumPoints-1 do begin
             DACBuf^[j] := DACValue ;
             j := j + NumDACS[Dev] ;
            end ;
         end ;

     end ;


procedure TLabIO.FillDIGBufWithDefaultValues(
         Dev : Integer ;            // Device #
         DIGBuf : PBig32bitArray ;  // Buffer to be updated
         NumPoints : Integer // No. of points in buffer
         ) ;
// ---------------------------------------
// Update DAC buffer with default voltages
// ---------------------------------------
var
  i : Integer ;
begin

     if DIGBuf = nil then Exit ;

     // Initialise DAC outputs to current default DAC states
     for i := 0 to NumPoints-1 do DIGBuf^[i] := DigOutState[Dev] ;

     end ;



// *******************************
// NIDAQ-MX functions & procedures
// *******************************

function TLabIO.NIDAQMX_InitialiseNIBoards : Boolean ;
{ ----------------------------------------------------
  Initialise A/D converter hardware and NI-DAQ library
  ---------------------------------------------------- }
var
    Done : Boolean ;
    i : Integer ;
    CBuf : Array[0..500] of ANSIChar ;
    DeviceNum : Integer ;
begin

   // Clear number of devices
   NumDevices := 0 ;
   ClearNIBoardSettings ;

   { Clear A/D and D/A in progress flags }

   for i := 1 to MaxDevices do ADCActive[i] := False ;
   for i := 1 to MaxDevices do DACActive[i] := False ;
   for i := 1 to MaxDevices do DIGActive[i] := False ;
   Result := False ;

   // Load API function library
   if not LibraryLoaded then LibraryLoaded := NIDAQMX_LoadLibrary( LibraryHnd ) ;
   if not LibraryLoaded then begin
      LogFrm.AddLine('NIDAQ-MX library (nicaiu.dll) not found!') ;
      Exit ;
      end ;

   // Get list of devices available
   CheckError(DAQmxGetSysDevNames( CBuf, High(CBuf)+1 )) ;

   NumDevices := 0 ;
   for i := 0 to High(DeviceName) do DeviceName[i] := '' ;
   i := 0 ;
   Done := False ;
   repeat
        if (CBuf[i] <> ',') and (CBuf[i] <> #0) then begin
           if CBuf[i] <> ' ' then
              DeviceName[NumDevices+1] := DeviceName[NumDevices+1] + CBuf[i] ;
           end
        else begin
           Inc(NumDevices) ;
           if CBuf[i] = #0 then Done := True ;
           end ;
        Inc(i) ;
        if i >= High(CBuf) then Done := True ;
        until Done ;


   if NumDevices <= 0 then Exit ;

   // Get device board name
   for i := 1 to NumDevices do begin
       CheckError(DAQmxGetDevProductType(PANSIChar(DeviceName[i]),CBuf,High(CBuf)+1)) ;
       DeviceBoardName[i] := PCharArrayToString(CBuf) ;
       if AnsiContainsText(DeviceBoardName[i],'622') or
          AnsiContainsText(DeviceBoardName[i],'625') or
          AnsiContainsText(DeviceBoardName[i],'628') or
          AnsiContainsText(DeviceBoardName[i],'632') or
          AnsiContainsText(DeviceBoardName[i],'634') or
          AnsiContainsText(DeviceBoardName[i],'635') or
          AnsiContainsText(DeviceBoardName[i],'636') then DigitalWaveformCapable[i] := True
                                                     else DigitalWaveformCapable[i] := False ;
       // 6002-6005 devices do not support A/D triggering from DAC/DIG clock.
       if AnsiContainsText(DeviceBoardName[i],'600') then DACClockTriggerSupported[i] := False
                                                     else DACClockTriggerSupported[i] := True ;

       // Digital outputs supported (all except USB-621X devices)
       if AnsiContainsText(DeviceBoardName[i],'621') then DigitalOutputsSupported[i] := False
                                                     else DigitalOutputsSupported[i] := True ;

       end ;

   for i := 1 to NumDevices do DeviceNumDMAChannels[i] := 2 ;

   // List available device resources

   NumResources := 0 ;
   for DeviceNum := 1 to NumDevices do
       begin

       // Determine number of A/D channels per board
       GetDeviceADCChannelProperties( DeviceNum ) ;
       // Add to resource list
       if NumADCs[DeviceNum] > 0 then
          begin
          Resource[NumResources].Device := DeviceNum ;
          Resource[NumResources].ResourceType := ADCIn ;
          Resource[NumResources].StartChannel := 0 ;
          Resource[NumResources].EndChannel := NumADCs[DeviceNum]-1 ;
          Inc(NumResources) ;
          end ;

       // Determine number of D/A channels per board
       GetDeviceDACChannelProperties( DeviceNum ) ;
       // Set analogue output DMA transfer mode
       if NumDACs[DeviceNum] > 0 then
          begin
          // Add to resource list
          for i := 0 to NumDACs[DeviceNum]-1 do
              begin
              Resource[NumResources].Device := DeviceNum ;
              Resource[NumResources].ResourceType := DACOut ;
              Resource[NumResources].StartChannel := i ;
              Resource[NumResources].EndChannel := i ;
              Inc(NumResources) ;
              end ;
          end ;

        if DACMaxVolts[DeviceNum] > 0.0 then
           begin
           DACScale[DeviceNum] := DACMaxValue[DeviceNum] / DACMaxVolts[DeviceNum] ;
           end
        else DACScale[DeviceNum] := 1.0 ;

        // Digital O/P ports (Except 621X series USB devices)
        if DigitalOutputsSupported[DeviceNum] then
           begin
           for i := 0 to 7 do
               begin
               Resource[NumResources].Device := DeviceNum ;
               Resource[NumResources].ResourceType := DIGOut ;
               Resource[NumResources].StartChannel := i ;
               Resource[NumResources].EndChannel := i ;
               Inc(NumResources) ;
               end ;
           end;
        end ;

   // Set minimum DAC update interval
   DACMinUpdateInterval := 5E-5 ;
   for i := 1 to NumDevices do
       begin
       if AnsiContainsText(DeviceBoardName[i],'600') then DACMinUpdateInterval := 2E-4 ;
       end;
   Result := True ;

   end ;


function TLabIO.NIDAQMX_ResetNIBoards : Boolean ;
{ ----------------------------------------------------
  Reset A/D converter hardware and NI-DAQ library
  ---------------------------------------------------- }
var
    Done : Boolean ;
    i : Integer ;
    CBuf : Array[0..500] of ANSIChar ;
    DeviceNum : Integer ;
begin

   // Clear number of devices
   NumDevices := 0 ;

   { Clear A/D and D/A in progress flags }

   for i := 1 to MaxDevices do ADCActive[i] := False ;
   for i := 1 to MaxDevices do DACActive[i] := False ;
   for i := 1 to MaxDevices do DIGActive[i] := False ;
   Result := False ;

   // Load API function library
   if not LibraryLoaded then LibraryLoaded := NIDAQMX_LoadLibrary( LibraryHnd ) ;
   if not LibraryLoaded then begin
      LogFrm.AddLine('NIDAQ-MX library (nicaiu.dll) not found!') ;
      Exit ;
      end ;

   // Get list of devices available
   CheckError(DAQmxGetSysDevNames( CBuf, High(CBuf)+1 )) ;

   NumDevices := 0 ;
   for i := 0 to High(DeviceName) do DeviceName[i] := '' ;
   i := 0 ;
   Done := False ;
   repeat
        if (CBuf[i] <> ',') and (CBuf[i] <> #0) then
           begin
           if CBuf[i] <> ' ' then
              DeviceName[NumDevices+1] := DeviceName[NumDevices+1] + CBuf[i] ;
           end
        else
           begin
           Inc(NumDevices) ;
           if CBuf[i] = #0 then Done := True ;
           end ;
        Inc(i) ;
        if i >= High(CBuf) then Done := True ;
        until Done ;


   if NumDevices <= 0 then Exit ;

   // Get device board name
   for i := 1 to NumDevices do
       begin
       CheckError(DAQmxGetDevProductType(PANSIChar(DeviceName[i]),CBuf,High(CBuf)+1)) ;
       DeviceBoardName[i] := PCharArrayToString(CBuf) ;
       if AnsiContainsText(DeviceBoardName[i],'622') or
          AnsiContainsText(DeviceBoardName[i],'625') or
          AnsiContainsText(DeviceBoardName[i],'634') or
          AnsiContainsText(DeviceBoardName[i],'635') or
          AnsiContainsText(DeviceBoardName[i],'636') then DigitalWaveformCapable[i] := True
                                                     else DigitalWaveformCapable[i] := False ;
       end ;

   for i := 1 to NumDevices do DeviceNumDMAChannels[i] := 2 ;

   // List available device resources

   NumResources := 0 ;
   for DeviceNum := 1 to NumDevices do
       begin

       // Determine number of A/D channels per board
       GetDeviceADCChannelProperties( DeviceNum ) ;
       // Add to resource list
       if NumADCs[DeviceNum] > 0 then
          begin
          Resource[NumResources].Device := DeviceNum ;
          Resource[NumResources].ResourceType := ADCIn ;
          Resource[NumResources].StartChannel := 0 ;
          Resource[NumResources].EndChannel := NumADCs[DeviceNum]-1 ;
          Inc(NumResources) ;
          end ;

       // Determine number of D/A channels per board
       GetDeviceDACChannelProperties( DeviceNum ) ;
       // Set analogue output DMA transfer mode
       if NumDACs[DeviceNum] > 0 then
          begin
          // Add to resource list
          for i := 0 to NumDACs[DeviceNum]-1 do
              begin
              Resource[NumResources].Device := DeviceNum ;
              Resource[NumResources].ResourceType := DACOut ;
              Resource[NumResources].StartChannel := i ;
              Resource[NumResources].EndChannel := i ;
              Inc(NumResources) ;
              end ;
          end ;

       if DACMaxVolts[DeviceNum] > 0.0 then
           begin
           DACScale[DeviceNum] := DACMaxValue[DeviceNum] / DACMaxVolts[DeviceNum] ;
           end
       else DACScale[DeviceNum] := 1.0 ;

        // Digital O/P ports
       for i := 0 to 7 do
            begin
            Resource[NumResources].Device := DeviceNum ;
            Resource[NumResources].ResourceType := DIGOut ;
            Resource[NumResources].StartChannel := i ;
            Resource[NumResources].EndChannel := i ;
            Inc(NumResources) ;
            end ;
       end ;


   Result := True ;

   end ;


procedure TLabIO.NIDAQMX_GetDeviceADCChannelProperties(
          DeviceNum : Integer
          ) ;
// ------------------------------------------------
// Get number of device A/D channels and properties
// ------------------------------------------------
const
    NumVRanges = 9 ;
var
    DValue : Double ;
    ADCScaleFactors : Array[0..20] of Double ;
    i : Integer ;
    ChannelName : ANSIString ;
    NumChannels : Integer ;
    ChannelList : Array[0..9999] of ANSICHar ;
    VRanges : Array[0..31] of Double ;
    SimultaneousSampling : LongBool ;
begin

    DisableFPUExceptions ;

    // Determine number of available AI channels
    CheckError( DAQmxGetDevAIPhysicalChans( PANSIChar(DeviceName[DeviceNum]), ChannelList, High(ChannelList)) ) ;
    NumChannels := 0 ;
    while ContainsText(String(ChannelList),format('ai%d',[NumChannels])) do Inc(NumChannels) ;
    CheckError( DAQmxGetDevAISimultaneousSamplingSupported( PANSIChar(DeviceName[DeviceNum]),SimultaneousSampling));
    if not SimultaneousSampling then NumChannels := NumChannels div 2 ;
    NumADCs[DeviceNum] := NumChannels ;

    // Determine channel input voltage ranges
    for i := 0 to High(VRanges) do VRanges[i] := 0.0 ;
    CheckError( DAQmxGetDevAIVoltageRngs( PANSIChar(DeviceName[DeviceNum]), VRanges, High(VRanges) ) );
    NumChannels := 0 ;
    for i := High(VRanges) downto 0 do if VRanges[i] > 0.0 then
        begin
        ADCVoltageRanges[DeviceNum,NumChannels] := VRanges[i] ;
        Inc(NumChannels) ;
        end;
    NumADCVoltageRanges[DeviceNum]  := NumChannels ;

    // Create A/D task for selected channel
    CheckError( DAQmxCreateTask( '', ADCTask[DeviceNum] ) ) ;

    ChannelName := format('%s/ai0',[DeviceName[DeviceNum]]) ;
    CheckError( DAQmxCreateAIVoltageChan( ADCTask[DeviceNum],
                                      PANSIChar(ChannelName),
                                      nil ,
                                      Integer(DAQmx_Val_Diff),
                                      -10.0,
                                      10.0,
                                      Integer(DAQmx_Val_Volts),
                                      nil));

    // Get A/D converter resolution
    CheckError( DAQmxGetAIResolution( ADCTask[DeviceNum],
                                      PANSIChar(ChannelName),
                                      DValue)) ;

    // Channel scaling factors
    CheckError( DAQmxGetAIDevScalingCoeff( ADCTask[DeviceNum],
                                           PANSIChar(ChannelName),
                                           @ADCScaleFactors,
                                           4 )) ;

    ADCVScale[DeviceNum] := ADCScaleFactors[1] ;
    ADCVOffset[DeviceNum] := ADCScaleFactors[0] ;
    ADCResolution[DeviceNum] := Round(DValue) ;
    ADCMaxValue[DeviceNum] := Round(Power(2.0,DValue-1.0)) - 1 ;
    ADCMinValue[DeviceNum] := -ADCMaxValue[DeviceNum] - 1 ;

    CheckError( DAQmxClearTask( ADCTask[DeviceNum] ) ) ;


    EnableFPUExceptions ;

    end ;

procedure TLabIO.NIDAQMX_GetDeviceDACChannelProperties(
          DeviceNum : Integer
          ) ;
// ------------------------------------------------
// Get number of device D/A channels and properties
// ------------------------------------------------
const
    MaxVRanges = 9 ;
    VRangeSize = MaxVRanges*2 ;

var
    DValue : Double ;
    i,j : Integer ;
    ChannelName : ANSIString ;
    NumChannels : Integer ;
    ChannelList : Array[0..9999] of ANSICHar ;
    VRanges : Array[0..VRangeSize-1] of Double ;
begin

    DisableFPUExceptions ;

    // Determine number of available AO channels
    CheckError( DAQmxGetDevAOPhysicalChans( PANSIChar(DeviceName[DeviceNum]), ChannelList, High(ChannelList)) ) ;
    NumChannels := 0 ;
    while ContainsText(String(ChannelList),format('ao%d',[NumChannels])) do Inc(NumChannels) ;
    NumDACs[DeviceNum] := NumChannels ;

    // Get output voltage ranges available
    for i := 0 to High(VRanges) do VRanges[i] := 0.0 ;
    CheckError( DAQmxGetDevAOVoltageRngs( PANSIChar(DeviceName[DeviceNum]), VRanges, High(VRanges) ) );

    // Find largest available DAC voltage range <= +/- 10V
    DACMaxVolts[DeviceNum] := 0.0 ;
    DACMinVolts[DeviceNum] := 0.0 ;
    for i := 0 to MaxVRanges-1 do
        begin
        j := i*2 ;
        if DACMinVolts[DeviceNum] > VRanges[j] then DACMinVolts[DeviceNum] := Max(VRanges[j],-10.0) ;
        if DACMaxVolts[DeviceNum] < VRanges[j+1] then DACMaxVolts[DeviceNum] := Min(VRanges[j+1],10.0) ;
        end;

    // Create D/A task
    CheckError( DAQmxCreateTask( '', DACTask[DeviceNum] ) ) ;
    ChannelName := format('%s/ao0',[DeviceName[DeviceNum]]) ;

    CheckError( DAQmxCreateAOVoltageChan( DACTask[DeviceNum],
                                          PANSIChar(ChannelName),
                                          nil,
                                          DACMinVolts[DeviceNum],
                                          DACMaxVolts[DeviceNum],
                                          DAQmx_Val_Volts,
                                          nil) );

     // Get D/A converter resolution
     CheckError( DAQmxGetAOResolution(DACTask[DeviceNum],PANSIChar(ChannelName),DValue)) ;
     DACResolution[DeviceNum] := Round(DValue) ;

     if DACMinVolts[DeviceNum] = 0.0 then
        begin
        // Unipolar DACs
        DACMaxValue[DeviceNum] := Round(Power(2.0,DValue)) - 1 ;
        DACMinValue[DeviceNum] := 0 ;
        end
     else
        begin
        // Bipolar DACs
        DACMaxValue[DeviceNum] := Round(Power(2.0,DValue-1.0)) - 1 ;
        DACMinValue[DeviceNum] := -DACMaxValue[DeviceNum] - 1 ;
        end ;

    EnableFPUExceptions ;

    end ;


procedure  TLabIO.NIDAQMX_CheckError( Err : Integer ) ;
var
   ErrString : Array[0..511] of ANSIChar ;
begin
     if Err = 0 then Exit ;
     DAQmxGetErrorString( Err, ErrString, 511 ) ;
     ShowMessage( String(ErrString) ) ;
     end ;


procedure TLabIO.DisableFPUExceptions ;
// ----------------------
// Disable FPU exceptions
// ----------------------
var
    FPUNoExceptions : Set of TFPUException ;
begin

     if not FPUExceptionMaskSet then FPUExceptionMask := GetExceptionMask ;
     FPUExceptionMaskSet := True ;

     Include(FPUNoExceptions, exInvalidOp );
     Include(FPUNoExceptions, exDenormalized );
     Include(FPUNoExceptions, exZeroDivide );
     Include(FPUNoExceptions, exOverflow );
     Include(FPUNoExceptions, exUnderflow );
     Include(FPUNoExceptions, exPrecision );
     SetExceptionMask( FPUNoExceptions ) ;

     end ;


procedure TLabIO.EnableFPUExceptions ;
// ----------------------
// Disable FPU exceptions
// ----------------------
begin
     SetExceptionMask( FPUExceptionMask ) ;
     end ;


function TLabIO.IsLabInterfaceAvailable : boolean ;
{ ------------------------------------------------------------
  Check to see if a lab. interface library is available
  ------------------------------------------------------------}
begin
     IsLabInterfaceAvailable := InitialiseNIBoards ;
     end ;


function TLabIO.NIDAQMX_ADCInputModeCode(
         Device : Integer ;
         InputMode : Integer                // Device in use
         ) : Integer ;
// ---------------------------------------------------------
// Return A/D input mode code for input mode in ADCInputMode
// ---------------------------------------------------------
begin
     // Set A/D input mode
     if (InputMode = imDifferential) or (InputMode = imBNC2110) then
        begin
        if ANSIContainsText(DeviceBoardName[Device],'61') then Result := DAQmx_Val_PseudoDiff
                                                          else Result := DAQmx_Val_Diff ;
        end
     else if InputMode = imSingleEndedRSE then Result := DAQmx_Val_RSE
     else Result := DAQmx_Val_NRSE ;

     end ;


procedure TLabIO.GetADCInputModes( InputModes :TStrings ) ;
// ----------------------------------------
// Return list of available A/D input modes
// ----------------------------------------
begin
    InputModes.Clear ;
    InputModes.Add( 'Single Ended (NRSE)') ;
    InputModes.Add( 'Differential') ;
    InputModes.Add( 'BNC 2110 ') ;
    InputModes.Add( 'BNC 2090 ') ;
    InputModes.Add('Single Ended (RSE)') ;
    end ;


function TLabIO.NIDAQMX_ADCToMemoryExtScan(
          Device : SmallInt ;
          var ADCBuf : Array of SmallInt  ;  { A/D sample buffer (OUT) }
          nChannels : Integer ;              { Number of A/D channels (IN) }
          nSamples : Integer ;               { Number of A/D samples ( per channel) (IN) }
          ADCVoltageRange : Single ;         { A/D input voltage range (V) (IN) }
          CircularBuffer : Boolean ;          { Repeated sampling into buffer (IN) }
          SamplingInterval : Double ;
          TimingDevice : SmallInt ;           // Device supply ADC/DAC timing pulses
          UseTimingDeviceDACClock : Boolean
          ) : Boolean ;                      { Returns TRUE indicating A/D started }
{ ----------------------------
  Start A/D converter sampling
  ----------------------------}
var
   ChannelList : ANSIstring ;
   ClockSource : ANSIstring ;
   TriggerSource : ANSIstring ;
   ch : Integer ;
   SampleMode : Integer ;
   SamplingRate : Double ;
begin

     Result := False ;
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumADCs[Device] <= 0 then Exit ;

     // Stop any running A/D task
     StopADC(Device) ;

     DisableFPUExceptions ;

     // Create A/D task
     CheckError( DAQmxCreateTask( '', ADCTask[Device] ) ) ;

     // Select A/D input channels
     ChannelList := format( DeviceName[Device] + '/AI0:%d', [nChannels-1] ) ;

     FADCVoltageRange[Device] := ADCVoltageRange ;
     CheckError( DAQmxCreateAIVoltageChan( ADCTask[Device],
                                           PANSIChar(ChannelList),
                                           nil ,
                                           ADCInputModeCode(Device,ADCInputMode),
                                           -ADCVoltageRange,
                                           ADCVoltageRange,
                                           DAQmx_Val_Volts,
                                           nil));

     // Select continuous sampling if circular buffer selected
     FADCCircularBuffer := CircularBuffer ;
     if CircularBuffer then SampleMode := DAQmx_Val_ContSamps
                       else SampleMode := DAQmx_Val_FiniteSamps ;

     if DACClockTriggerSupported[TimingDevice] then begin
        // Trigger A/D sampling from DAC or DIG update clock
        if UseTimingDeviceDACClock then begin
           ClockSource := '/' + DeviceName[TimingDevice] + '/ao/sampleclock' ;
           end
        else begin
           ClockSource := '/' + DeviceName[TimingDevice] + '/do/sampleclock' ;
           end ;
        SamplingRate := 200000.0 / (nChannels+1) ;

        CheckError( DAQmxCfgSampClkTiming( ADCTask[Device],
                                             PANSIChar(ClockSource),
                                             SamplingRate,
                                             DAQmx_Val_Rising,
                                             SampleMode,
                                             nSamples));

        // Request immediate start
        CheckError(DAQmxDisableStartTrig(ADCTask[Device]));

        end
     else begin
        // Time A/D sampling from on-board clock, start trigger by PFI1
        SamplingRate := 1.0/Max(SamplingInterval,DACMinUpdateInterval) ;

        CheckError( DAQmxCfgSampClkTiming( ADCTask[Device],
                                             PANSIChar('onboardclock'),
                                             SamplingRate,
                                             DAQmx_Val_Rising,
                                             SampleMode,
                                             nSamples));

       // Use PFI0 as shared trigger for AI and AO when AO/sampleclock not available
       TriggerSource := '/' + DeviceName[Device] + '/pfi0' ;
       CheckError( DAQmxCfgDigEdgeStartTrig( ADCTask[Device],
                                             PANSIChar(TriggerSource),
                                             DAQmx_Val_Rising )) ;

        end;

     // Get clock rate set
     CheckError( DAQmxGetSampClkRate( ADCTask[Device], SamplingRate )) ;
//     SamplingInterval := 1.0 / SamplingRate ;

     // Configure buffer
     CheckError( DAQmxCfgInputBuffer ( ADCTask[Device], nSamples )) ;
     // Enable immediate return with any available data within buffer
     CheckError( DAQmxSetReadReadAllAvailSamp( ADCTask[Device], True )) ;

     // Set triggering
     // --------------

     // Start A/D task
     CheckError( DAQmxStartTask(ADCTask[Device])) ;

     // Restore FPU exceptions
     EnableFPUExceptions ;

     FADCPointer := 0 ;
     FADCNumChannels := nChannels ;
     FADCNumSamples := nSamples ;
     FADCNumSamplesAcquired := 0 ;

     // Define A/D channel offset sequence within A/D sample buffer
     for ch := 0 to nChannels-1 do ADCChannelOffsets[ch] := ch ;

     ADCActive[Device] := True ;
     Result := ADCActive[Device] ;

     end ;


function TLabIO.NIDAQMX_StopADC(
         Device : SmallInt
         ) : Boolean ;      { Returns FALSE = A/D stopped }
{ -------------------------------
  Reset A/D conversion sub-system
  -------------------------------}
var
    t0 : Integer ;
begin
     Result := False ;
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumADCs[Device] <= 0 then Exit ;
     if not ADCActive[Device] then Exit ;

     DisableFPUExceptions ;

     // Stop running A/D task
     T0 := timegettime ;
     CheckError( DAQmxClearTask(ADCTask[Device])) ;
     if (timegettime - t0) > 1000 then
        outputdebugstring(pchar(format('NIMX_StopADC: %d ms Delay in DAQmxClearTask call. ',[timegettime - t0])));


     EnableFPUExceptions ;

     ADCActive[Device] := False ;
     Result := ADCActive[Device] ;

     end ;


procedure TLabIO.NIDAQMX_GetADCSamples(
          Device : Integer ;
          var ADCBuf : Array of SmallInt
          ) ;
// -------------------------------------------------------------
// Get latest A/D samples acquired and transfer to memory buffer
// -------------------------------------------------------------
var
    i,j,ch : Integer ;
    ADCBufNumSamples : Integer ;
    ADCBufEnd : Integer ;
    NumSamplesRead : Integer ;
    VScale : Single ;
    ScaledValue,ADCMaxVal,ADCMinVal : SmallInt ;

begin

    if not ADCActive[Device] then Exit ;

    ADCBufNumSamples := FADCNumSamples*FADCNumChannels ;
    ADCBufEnd := FADCNumSamples*FADCNuMChannels - 1 ;

    if (not FADCCircularBuffer) and (FADCNumSamplesAcquired >= ADCBufNumSamples) then Exit ;

    // Read data from A/D converter
    DAQmxReadAnalogF64( ADCTask[Device],
                                  -1,
                                  DefaultTimeOut,
                                  DAQmx_Val_GroupByScanNumber,
                                  DBuf,
                                  ADCBufNumSamples,
                                  NumSamplesRead,
                                  Nil) ;

    // Apply calibration factors and copy to output buffer

    ADCMaxVal := ADCMaxValue[Device] - 1 ;
    ADCMinVal := ADCMinValue[Device] + 1 ;
    VScale := ADCMaxValue[Device]/FADCVoltageRange[Device] ;
    j := 0 ;
    for i := 0 to NumSamplesRead-1 do
        begin
        for ch := 0 to FADCNumChannels-1 do
            begin
            ScaledValue := Round(DBuf[j]*VScale) ;
            if ScaledValue < ADCMinVal then ScaledValue := ADCMinVal ;
            if ScaledValue > ADCMaxVal then ScaledValue := ADCMaxVal ;
            ADCBuf[FADCPointer] := ScaledValue ;
            Inc(FADCPointer) ;
            Inc(j) ;
            end ;
        FADCNumSamplesAcquired := FADCPointer ;
        if (FADCPointer > ADCBufEnd) and FADCCircularBuffer then FADCPointer := 0 ;
        end ;

    end ;



procedure TLabIO.NIDAQMX_CheckSamplingInterval(
               DeviceNum : Integer ;
               var SamplingInterval : double ;
               NumADCChannels : Integer
               ) ;
// ------------------------------------------------
// Set sampling interval to nearest supported value
// ------------------------------------------------
var
    ChannelList : ANSIstring ;
    SamplingRate : Double ;
    ActualSamplingRate : Double ;
    Err : Integer ;
begin

     if ADCActive[DeviceNum] then Exit ;
     if not BoardInitialised then InitialiseNIBoards ;
     if not BoardInitialised then Exit ;

     DisableFPUExceptions ;

     // Create A/D task
     CheckError( DAQmxCreateTask( '', ADCTask[DeviceNum] ) ) ;

     // Select A/D input channels
     ChannelList := format( DeviceName[DeviceNum] + '/AI0:%d', [NumADCChannels-1] ) ;

     CheckError( DAQmxCreateAIVoltageChan( ADCTask[DeviceNum],
                                                PANSIChar(ChannelList),
                                                nil ,
                                                ADCInputModeCode(DeviceNum,ADCInputMode),
                                                -10.0,
                                                10.0,
                                                DAQmx_Val_Volts,
                                                nil));

     // Set sampling rate (ensuring that it can be supported by board)

     SamplingRate := 1.0 / Max(SamplingInterval,1E-7) ;
     Repeat
        Err := DAQmxCfgSampClkTiming( ADCTask[DeviceNum],
                                      nil,
                                      SamplingRate,
                                      DAQmx_Val_Rising,
                                      DAQmx_Val_FiniteSamps,
                                      2);

        // Read rate back from board
        Err := DAQmxGetSampClkRate( ADCTask[DeviceNum], ActualSamplingRate ) ;
        if Err <> 0 then SamplingRate := SamplingRate*0.75 ;
        Until Err = 0 ;

     // Return actual sampling interval
     SamplingInterval := 1.0 / Max(ActualSamplingRate,1E-7) ;

     // Clear task
     CheckError( DAQmxClearTask(ADCTask[DeviceNum])) ;

     EnableFPUExceptions ;

     end ;


function TLabIO.NIDAQMX_MemoryToDAC(
          Device : SmallInt ;
          var DACBuf : Array of SmallInt  ; { D/A output data buffer (IN) }
          nChannels : SmallInt ;            { No. of D/A channels (IN) }
          nPoints : Integer ;               { No. of D/A output values (IN) }
          UpdateInterval : Double ;          { D/A output interval (s) (IN) }
          CircularBufferMode : Boolean ;     { TRUE = continuous update from circular buffer }
          ExternalTrigger : Boolean ;
          TimingDevice : SmallInt           // Device providing ADC/DAC timing pulse
          ): Boolean ;                      { Returns TRUE=D/A active }
{ --------------------------
  Start D/A converter output
  --------------------------}
var
    SampleMode : Integer ;
    ClockSource : ANSIString ;
    TriggerSource : ANSIString ;
    ChannelList : ANSIString ;
    i,iFrom,iTo,nCopy,nSource,EndOffset : Integer ;
    NumSamplesWritten : Integer ;
    UpdateRate : Double ;
    NumPointsInBuffer : Integer ;
    VBuf : PBigDoubleArray ;
    VScale : Double ;
    TaskHandle : NativeInt ;
    Err : Integer ;
begin
     Result := False ;
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumDACs[Device] <= 0 then Exit ;

     if Device > TimingDevice then begin
        ShowMessage('ERROR! Timing device must be last device number started.)') ;
        Exit ;
        end ;

    // Stop any running D/A task
     StopDAC(Device) ;

     DisableFPUExceptions ;

     // Create D/A task
     CheckError( DAQmxCreateTask( '', DACTask[Device] ) ) ;

     // Select D/A output channels
     ChannelList := format( DeviceName[Device] + '/AO0:%d', [nChannels-1] ) ;
     CheckError( DAQmxCreateAOVoltageChan( DACTask[Device],
                                           PANSIChar(ChannelList),
                                           nil ,
                                           -DACMaxVolts[Device],
                                           DACMaxVolts[Device],
                                           DAQmx_Val_Volts,
                                           nil));


     // Configure buffer size
     if CircularBufferMode then begin
        NumPointsInBuffer := round(InternalBufferDuration/UpdateInterval) ;
        SampleMode := DAQmx_Val_ContSamps ;
        end
     else begin
        NumPointsInBuffer := nPoints ;
        SampleMode := DAQmx_Val_FiniteSamps ;

        end ;

     CheckError( DAQmxCfgOutputBuffer ( DACTask[Device], NumPointsInBuffer )) ;

     if DACClockTriggerSupported[Device] then begin
        // Set D/A clock source
        if TimingDevice <> Device then ClockSource := '/' + DeviceName[TimingDevice] + '/ao/sampleclock'
                                  else ClockSource := 'onboardclock' ;

        // Set timing
        UpdateRate := 1.0 / Max(UpdateInterval,1E-8) ;
        CheckError( DAQmxCfgSampClkTiming( DACTask[Device],
                                           PANSIChar(ClockSource),
                                           UpdateRate,
                                           DAQmx_Val_Rising,
                                           SampleMode,
                                           Int64(NumPointsInBuffer) )) ;

        // Request immediate start
        CheckError(DAQmxDisableStartTrig(DACTask[Device]));

        end
     else begin

        // Set timing
        UpdateRate := 1.0 / Max(UpdateInterval,DACMinUpdateInterval) ;
        CheckError( DAQmxCfgSampClkTiming( DACTask[Device],
                                           PANSIChar('onboardclock'),
                                           UpdateRate,
                                           DAQmx_Val_Rising,
                                           SampleMode,
                                           Int64(NumPointsInBuffer) )) ;

        // Start when pulse received on PFI1
        TriggerSource := '/' + DeviceName[Device] + '/PFI1' ;
        CheckError( DAQmxCfgDigEdgeStartTrig( DACTask[Device],
                                              PANSIChar(TriggerSource),
                                              DAQmx_Val_Rising )) ;

        end ;

     // Disable buffer regeneration, so waveform update is possible
     CheckError( DAQmxSetWriteRegenMode( DACTask[Device], DAQmx_Val_DoNotAllowRegen ));

     // Copy into output buffer
     nCopy := NumPointsInBuffer*nChannels ;
     nSource := nPoints*nChannels ;
     if CircularBufferMode then EndOffset := nSource
                           else EndOffset := nChannels ;
     iTo := 0 ;
     iFrom := 0 ;
     GetMem(VBuf,nCopy*Sizeof(Double)) ;
     VScale := 1.0/DACScale[Device] ;
     for i := 0 to nCopy-1 do begin
         VBuf[iTo] := DACBuf[iFrom]*VScale ;
         Inc(iFrom) ;
         Inc(iTo) ;
         if iFrom >= nSource then iFrom := iFrom - EndOffset
        end ;

     // Write data to buffer
     DAC[Device].EndOfBuf := nPoints-1 ;
     DAC[Device].NumChannels := nChannels ;
     DAC[Device].Buf := @DACBuf ;
     DAC[Device].CircularBuffer := CircularBufferMode ;
     DAC[Device].Pointer := iFrom div nChannels ;
     CheckError( DAQmxWriteAnalogF64( DACTask[Device],
                                      NumPointsInBuffer,
                                      False,
                                      DefaultTimeOut,
                                      DAQmx_Val_GroupByScanNumber,
                                      @VBuf^,
                                      NumSamplesWritten,
                                      Nil
                                      )) ;

      FreeMem(VBuf) ;

     // Start D/A task
     CheckError( DAQmxStartTask(DACTask[Device])) ;

     // Restore FPU exceptions
     EnableFPUExceptions ;

     if not DACClockTriggerSupported[Device] then begin
        // Generate trigger pulse on P1.0 going to PFI0 and PFI1
        CheckError( DAQmxCreateTask( '', TaskHandle ) ) ;
        Err := DAQmxCreateDOChan( TaskHandle,
                                  PANSIChar(DeviceName[Device] + '/port1/Line0'),
                                  nil,
                                  DAQmx_Val_ChanPerLine );
        if Err = 0 then DAQmxWriteDigitalScalarU32( TaskHandle,     // Set = 0
                                                    True,
                                                    DefaultTimeOut,
                                                    0,
                                                    Nil ) ;
        if Err = 0 then DAQmxWriteDigitalScalarU32( TaskHandle,    // Set = 1
                                                    True,
                                                    DefaultTimeOut,
                                                    1,
                                                    Nil ) ;
        // Clear task
        CheckError( DAQmxClearTask ( TaskHandle ) ) ;

        end;

     DACActive[Device] := True ;
     Result := DACActive[Device] ;

     end ;


procedure TLabIO.NIDAQMX_UpdateDACOutputBuffer ;
// -------------------------------------------------------------
// Update internal NIDAQmx DAC output buffer
// -------------------------------------------------------------
var
    i,ch,iDev,iFrom,iTo,iPointer,EndOfBuf,nChannels,NumSamplesWritten : Integer ;

    NumPointsToWrite : Cardinal ;
    CircularBuffer : Boolean ;
    pBuf : PBig16bitArray ;
    VScale : Double ;
begin


    for iDev := 1 to NumDevices do if DACActive[iDev] then begin ;

        // Update D/A output buffer with XY scan waveform
        DAQmxGetWriteSpaceAvail( DACTask[iDev], NumPointsToWrite ) ;
        if NumPointsToWrite = 0 then continue ;

        // Copy DAC data into output buffer
        iPointer := DAC[iDev].Pointer ;

        nChannels := DAC[iDev].NumChannels ;
        CircularBuffer := DAC[iDev].CircularBuffer ;
        if not CircularBuffer then continue ;
        EndOfBuf := DAC[iDev].EndOfBuf ;
        iFrom := DAC[iDev].Pointer*nChannels ;
        pBuf := DAC[iDev].Buf ;
        iTo := 0 ;
        VScale := 1.0/DACScale[iDev] ;
        for i := 0 to NumPointsToWrite-1 do begin
            for ch := 0 to nChannels-1 do begin
               DBuf[iTo] := pBuf^[iFrom]*VScale ;
               Inc(iFrom) ;
               Inc(iTo) ;
               end ;
            Inc(iPointer) ;
            if iPointer > EndOfBuf then begin
               if CircularBuffer then iPointer := 0
                                 else iPointer := EndOfBuf - nChannels + 1 ;
               iFrom := iPointer*nChannels ;
               end ;
            end ;
        DAC[iDev].Pointer := iPointer ;

        CheckError( DAQmxWriteAnalogF64( DACTask[iDev],
                           NumPointsToWrite,
                           False,
                           DefaultTimeOut,
                           DAQmx_Val_GroupByScanNumber,
                           DBuf,
                           NumSamplesWritten,
                           Nil
                           )) ;

        end ;

    end ;




function TLabIO.NIDAQMX_StopDAC(
         Device : SmallInt
         ) : Boolean ;    { Returns FALSE = D/A stopped }
{ ---------------
  Stop D/A output
  --------------- }
begin
     Result := False ;
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumDACs[Device] <= 0 then Exit ;
     if not DACActive[Device] then Exit ;

     DisableFPUExceptions ;

     // Stop running D/A task
     CheckError( DAQmxClearTask(DACTask[Device])) ;

     EnableFPUExceptions ;

     DACActive[Device] := False ;
     Result := DACActive[Device] ;

     end ;


procedure TLabIO.NIDAQMX_WriteDACs(
          Device : Integer ;
          DACVolts : array of Single ;
          nChannels : Integer ) ;
{ --------------------------------------
  Write values to D/A converter outputs
  -------------------------------------}
var
   DBuf : Array[0..MaxDACChannels-1] of Double ;
   ChannelList : ANSIstring ;
   NumSamplesWritten : Integer ;
   i : Integer ;

begin

     // Quit if device does not exist or have DACs
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumDACs[Device] <= 0 then Exit ;

     // Stop any running D/A task
     StopDAC(Device) ;

     DisableFPUExceptions ;

     // Create task
     CheckError( DAQmxCreateTask( '', DACTask[Device] ) ) ;

     // Select D/A output channels
     ChannelList := format( DeviceName[Device] + '/AO0:%d', [nChannels-1] ) ;
     CheckError( DAQmxCreateAOVoltageChan( DACTask[Device],
                                           PANSIChar(ChannelList),
                                           nil ,
                                           -DACMaxVolts[Device],
                                           DACMaxVolts[Device],
                                           DAQmx_Val_Volts,
                                           nil)) ;

     // Copy into double array
     for i := 0 to nChannels-1 do begin
          DBuf[i] := Max(Min(DACVolts[i],DACMaxVolts[Device]),-DACMaxVolts[Device]) ;
          end ;

     // Write data to buffer
     DAQmxWriteAnalogF64 ( DACTask[Device],
                           1,
                           True,
                           DefaultTimeOut,
                           DAQmx_Val_GroupByScanNumber,
                           @DBuf,
                           NumSamplesWritten,
                           Nil
                           ) ;

     // Clear task
     CheckError( DAQmxClearTask ( DACTask[Device] ) ) ;

     EnableFPUExceptions ;

     end ;


procedure TLabIO.NIDAQMX_WriteDAC(
          Device : Integer ;
          DACVolts : Single ;
          iChannel : Integer ) ;
{ --------------------------------------
  Write values to D/A converter outputs
  -------------------------------------}
var
   DBuf : Array[0..MaxDACChannels-1] of Double ;
   ChannelList : ANSIstring ;
   NumSamplesWritten : Integer ;

begin

     // Quit if device does not exist or have DACs
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if iChannel >= NumDACs[Device] then Exit ;

     // Stop any running D/A task
     StopDAC(Device) ;

     DisableFPUExceptions ;

     // Create task
     CheckError( DAQmxCreateTask( '', DACTask[Device] ) ) ;

     // Select D/A output channels
     ChannelList := format( DeviceName[Device] + '/AO%d', [iChannel] ) ;
     CheckError( DAQmxCreateAOVoltageChan( DACTask[Device],
                                           PANSIChar(ChannelList),
                                           nil ,
                                           -DACMaxVolts[Device],
                                           DACMaxVolts[Device],
                                           DAQmx_Val_Volts,
                                           nil)) ;

     // Copy into double array
      DBuf[0] := Max(Min(DACVolts,DACMaxVolts[Device]),-DACMaxVolts[Device]) ;

     // Write data to buffer
     DAQmxWriteAnalogF64 ( DACTask[Device],
                           1,
                           True,
                           DefaultTimeOut,
                           DAQmx_Val_GroupByScanNumber,
                           @DBuf,
                           NumSamplesWritten,
                           Nil
                           ) ;

     // Clear task
     CheckError( DAQmxClearTask ( DACTask[Device] ) ) ;

     EnableFPUExceptions ;

     end ;


function TLabIO.NIDAQMX_MemoryToDIG(
          Device : SmallInt ;
          var DIGBuf : Array of Integer  ;  { Digital output data buffer (IN) }
          nPoints : Integer ;               { No. of digital output values (IN) }
          UpdateInterval : Double ;          { Digital output interval (s) (IN) }
          CircularBufferMode : Boolean ;     { TRUE = continuous update from circular buffer }
          ExternalTrigger : Boolean ;
          TimingDevice : SmallInt ;          // Device providing ADC/DAC timing pulse
          UseTimingDeviceDACClock : Boolean // DAC Clock available for user on timing device
          ): Boolean ;                      { Returns TRUE=D/A active }
{ --------------------------
  Start digital output
  --------------------------}
var
    PortList : ANSIString ;
    ClockSource : ANSIString ;
    NumPointsInBuffer,NumSamplesWritten : Integer ;
    iFrom,iTo,nSource,nCopy,EndOffset : Integer ;
    IOBuf : PBig32bitArray ;
begin

     Result := False ;
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if not DigitalWaveformCapable[Device] then Exit ;

     if Device > TimingDevice then begin
        ShowMessage('ERROR! Timing device must be last device number started.)') ;
        Exit ;
        end ;

    // Stop any running D/A task
     StopDIG(Device) ;

     DisableFPUExceptions ;

     // Create D/A task
     CheckError( DAQmxCreateTask( '', DIGTask[Device] ) ) ;

     // Select digital port 0 for output
     PortList := DeviceName[Device] + '/port0' ;
     CheckError( DAQmxCreateDOChan( DIGTask[Device],
                                    PANSIChar(PortList),
                                    nil,
                                    DAQmx_Val_ChanForAllLines ));

     // Set D/A clock source
     if UseTimingDeviceDACClock then begin
        // Use DAC clock to time digital updates
        ClockSource := '/' + DeviceName[TimingDevice] + '/ao/sampleclock' ;
        end
     else begin
        // No DACs in use on timing device, use own digital clock
        ClockSource := 'onboardclock' ;
        end ;

     // Configure buffer size
     NumPointsInBuffer := round(InternalBufferDuration/UpdateInterval) ;
     CheckError( DAQmxCfgOutputBuffer ( DIGTask[Device], NumPointsInBuffer )) ;

     // Set digital output timing
     CheckError( DAQmxCfgSampClkTiming( DIGTask[Device],
                                        PANSIChar(ClockSource),
                                        1.0/UpdateInterval,
                                        DAQmx_Val_Rising ,
                                        DAQmx_Val_ContSamps,
                                        Int64(NumPointsInBuffer))) ;

     // Disable buffer regeneration, so waveform update is possible
     CheckError( DAQmxSetWriteRegenMode( DIGTask[Device], DAQmx_Val_DoNotAllowRegen ));

     // Copy into output buffer
     nCopy := NumPointsInBuffer ;
     nSource := nPoints ;
     if CircularBufferMode then EndOffset := nSource
                           else EndOffset := 1 ;
     iFrom := 0 ;
     GetMem(IOBuf,nCopy*4) ;
     for iTo := 0 to nCopy-1 do begin
         IOBuf^[iTo] := DigBuf[iFrom] ;
         Inc(iFrom) ;
         if iFrom >= nSource then iFrom := iFrom - EndOffset
        end ;

     // Write data to buffer
     DIG[Device].EndOfBuf := nPoints-1 ;
     DIG[Device].NumChannels := 1 ;
     DIG[Device].Buf := @DIGBuf ;
     DIG[Device].CircularBuffer := CircularBufferMode ;
     DIG[Device].Pointer := iFrom ;
     CheckError( DAQmxWriteDigitalU32( DIGTask[Device],
                                       NumPointsInBuffer,
                                       False,
                                       DefaultTimeOut,
                                       DAQmx_Val_GroupByScanNumber ,
                                       @IOBuf^,
                                       NumSamplesWritten,
                                       Nil)) ;

     FreeMem(IOBuf) ;

     // Start DIG task
     CheckError( DAQmxStartTask(DIGTask[Device])) ;

     // Restore FPU exceptions
     EnableFPUExceptions ;

     DIGActive[Device] := True ;
     Result := DIGActive[Device] ;

     end ;


procedure TLabIO.NIDAQMX_UpdateDIGOutputBuffer ;
// -------------------------------------------------------------
// Update internal NIDAQmx digital output buffer
// -------------------------------------------------------------
var
    i,iDev,iFrom,iTo,iPointer,EndOfBuf,NumSamplesWritten : Integer ;
    //tt,t1 : Integer ;
    NumPointsToWrite : Cardinal ;
    CircularBuffer : Boolean ;
    pBuf : PBig32bitArray ;
    IOBuf : pBig32bitArray ;
begin

    for iDev := 1 to NumDevices do if DIGActive[iDev] then begin ;

        // Update digital output buffer
        DAQmxGetWriteSpaceAvail( DIGTask[iDev], NumPointsToWrite ) ;
        if NumPointsToWrite = 0 then continue ;

        GetMem( IOBuf, NumPointsToWrite*SizeOf(Integer)) ;

        // Copy DAC data into output buffer
        iPointer := DIG[iDev].Pointer ;

        CircularBuffer := DIG[iDev].CircularBuffer ;
        EndOfBuf := DIG[iDev].EndOfBuf ;
        iFrom := DIG[iDev].Pointer ;
        pBuf := DIG[iDev].Buf ;
        iTo := 0 ;
        for i := 0 to NumPointsToWrite-1 do begin
            IOBuf[iTo] := pBuf^[iFrom] ;
            Inc(iFrom) ;
            Inc(iTo) ;
            Inc(iPointer) ;
            if iPointer > EndOfBuf then begin
               if CircularBuffer then iPointer := 0
                                 else iPointer := EndOfBuf ;
               iFrom := iPointer ;
               end ;
            end ;
        DIG[iDev].Pointer := iPointer ;

        CheckError( DAQmxWriteDigitalU32( DIGTask[iDev],
                                          NumPointsToWrite,
                                          False,
                                          DefaultTimeOut,
                                          DAQmx_Val_GroupByScanNumber ,
                                          IOBuf,
                                          NumSamplesWritten,
                                          Nil)) ;

        FreeMem(IOBuf) ;
        end ;

    end ;


function TLabIO.NIDAQMX_StopDIG(
         Device : SmallInt
         ) : Boolean ;    { Returns FALSE = D/A stopped }
{ -------------------
  Stop digital output
  ------------------- }
begin
     Result := False ;
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if not DIGActive[Device] then Exit ;

     DisableFPUExceptions ;

     // Stop running D/A task
     CheckError( DAQmxClearTask(DIGTask[Device])) ;

     EnableFPUExceptions ;

     DIGActive[Device] := False ;
     Result := DIGActive[Device] ;

     end ;


procedure TLabIO.NIDAQMX_WriteToDigitalOutPutPort(
          Device : Integer ;
          Pattern : Integer
          ) ;
{ ----------------------
  Update digital port 0
  ---------------------}
var
   PortList : ANSIstring ;
begin

     if Device > NumDevices then Exit ;
     //Exit ;

     DisableFPUExceptions ;

     // Create task
     CheckError( DAQmxCreateTask( '', DigTask[Device] ) ) ;

     // Select digital port 0 for output
     PortList := DeviceName[Device] + '/port0' ;
     CheckError( DAQmxCreateDOChan( DigTask[Device],
                                    PANSIChar(PortList),
                                    nil,
                                    DAQmx_Val_ChanForAllLines ));

     // Write bit pattern to output (first 8 bits only used)
     CheckError( DAQmxWriteDigitalScalarU32( DigTask[Device],
                                             True,
                                             DefaultTimeOut,
                                             Pattern,
                                             Nil )) ;

     // Clear task
     CheckError( DAQmxClearTask ( DigTask[Device] ) ) ;

     EnableFPUExceptions ;

     end ;


function TLabIO.NIDAQMX_ReadADC(
         Device : Integer ;
         Channel : Integer ;       { A/D channel to be read (IN) }
         ADCVoltageRange : Single  { A/D converter input voltage range (V) (IN) }
         ) : Integer ;
// ----------------------------------------------
// Single read of selected A/D converter channel
// ----------------------------------------------
var
    ChannelList : ANSIstring ;
    Voltage : Double ;
begin
     Result := 0 ;
     // Quit if device does not exist or have ADCs
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumADCs[Device] <= 0 then Exit ;

     // Stop any running A/D task
     StopADC(Device) ;

     DisableFPUExceptions ;

     // Create task
     CheckError( DAQmxCreateTask( '', ADCTask[Device] ) ) ;

     // Select A/D input channel
     ChannelList := format( DeviceName[Device] + '/AI%d', [Channel] ) ;


     FADCVoltageRange[Device] := ADCVoltageRange ;
     CheckError( DAQmxCreateAIVoltageChan( ADCTask[Device],
                                           PANSIChar(ChannelList),
                                           nil ,
                                           ADCInputModeCode(Device,ADCInputMode),
                                           -ADCVoltageRange,
                                           ADCVoltageRange,
                                           DAQmx_Val_Volts,
                                           nil));

     // Read voltage
     CheckError( DAQmxReadAnalogScalarF64( ADCTask[Device],
                                           DefaultTimeOut,
                                           Voltage,
                                           Nil )) ;

     // Clear task
     CheckError( DAQmxClearTask ( ADCTask[Device] ) ) ;

     EnableFPUExceptions ;

     Result := Round( (Voltage/ADCVoltageRange)*ADCMaxValue[Device] ) ;

     end ;


function TLabIO.GetChannelOffset(
         Chan : Integer ;
         NumChannels : Integer ) : Integer ;
begin
     Result := Chan ;
     end ;


procedure TLabIO.Close ;
{ -----------------------------------
  Shut down lab. interface operations
  ----------------------------------- }
var
    Device : SmallInt ;
begin

     for Device := 1 to NumDevices do begin
         if DACActive[Device] then StopDAC(Device) ;
         if ADCActive[Device] then StopADC(Device) ;
         end ;

     BoardsInitialised := False ;

//     FreeMem(InBuf) ;
     FreeMem(DBuf) ;

     if LibraryLoaded then FreeLibrary( LibraryHnd ) ;
     LibraryLoaded := False ;

     end ;


function  TLabIO.IntLimit(
          Value : Integer ;
          LoLimit : Integer ;
          HiLimit : Integer
          ) : Integer ;
{ -------------------------------------------------------------
  Return integer Value constrained within range LoLimit-HiLimit
  ------------------------------------------------------------- }
begin
     if Value > HiLimit then Value := HiLimit ;
     if Value < LoLimit then Value := LoLimit ;
     Result := Value ;
     end ;

function  TLabIO.PCharArrayToString(
          CBuf : Array of ANSIChar
          ) : ANSIstring ;
var
  i : Integer ;
  s : ANSIstring ;
begin

    i := 0 ;
    s := '' ;
    repeat
        if Cbuf[i] <> #0 then s := s + CBuf[i] ;
        Inc(i) ;
        until (CBuf[i] = #0) or (i >= High(CBuf)) ;
    Result := s ;
    end ;


// **********************************
// Traditional NI-DAQ library methods
// **********************************

function TLabIO.NIDAQ_InitialiseNIBoards : Boolean ;
{ ----------------------------------------------------
  Initialise A/D converter hardware and NI-DAQ library
  ---------------------------------------------------- }
const
     DigPortGroup = 1 ;
     AsDigOutputPort = 1 ;
var
    BoardName : array[0..370] of string[16] ;
    Err : SmallInt ;
    Done : Boolean ;
    DeviceNum : SmallInt ;
    i : Integer ;
    BrdType : Integer;
begin

        ClearNIBoardSettings ;

        BoardName[0] :=  'AT-MIO-16L-9' ;
        BoardName[1] :=  'AT-MIO-16L-15' ;
        BoardName[2] :=  'AT-MIO-16L-25' ;
        BoardName[3] := '?' ;
        BoardName[4] :=  'AT-MIO-16H-9' ;
        BoardName[5] :=  'AT-MIO-16H-15' ;
        BoardName[6] :=  'AT-MIO-16H-25' ;
        BoardName[7] :=  'PC-DIO-24' ;
        BoardName[8] :=  'AT-DIO-32F' ;
        BoardName[9] := '?' ;
        BoardName[10] :=  'EISA-A2000' ;
        BoardName[11] :=  'AT-MIO-16F-5' ;
        BoardName[12] :=  'PC-DIO-96/PnP' ;
        BoardName[13] :=  'PC-LPM-16' ;
        BoardName[14] :=  'PC-TIO-10' ;
        BoardName[15] :=  'AT-AO-6' ;
        BoardName[16] :=  'AT-A2150S' ;
        BoardName[17] :=  'AT-DSP2200 ' ;
        BoardName[18] :=  'AT-DSP2200 ' ;
        BoardName[19] :=  'AT-MIO-16X' ;
        BoardName[20] :=  'AT-MIO-64F-5' ;
        BoardName[21] :=  'AT-MIO-16DL-9' ;
        BoardName[22] :=  'AT-MIO-16DL-25' ;
        BoardName[23] :=  'AT-MIO-16DH-9' ;
        BoardName[24] :=  'AT-MIO-16DH-25' ;
        BoardName[25] :=  'AT-MIO-16E-2' ;
        BoardName[26] :=  'AT-AO-10' ;
        BoardName[27] :=  'AT-A2150C' ;
        BoardName[28] :=  'Lab-PC+' ;
        BoardName[29] := '?' ;
        BoardName[30] :=  'SCXI-1200' ;
        BoardName[31] :=  'DAQCard-700' ;
        BoardName[32] :=  'NEC-MIO-16E-4' ;
        BoardName[33] :=  'DAQPad-1200' ;
        BoardName[34] :=  'DAQCard-DIO-24' ;
        BoardName[36] :=  'AT-MIO-16E-10' ;
        BoardName[37] :=  'AT-MIO-16DE-10' ;
        BoardName[38] :=  'AT-MIO-64E-3' ;
        BoardName[39] :=  'AT-MIO-16XE-50' ;
        BoardName[40] :=  'NEC-AI-16E-4' ;
        BoardName[41] :=  'NEC-MIO-16XE-50' ;
        BoardName[42] :=  'NEC-AI-16XE-50' ;
        BoardName[43] :=  'DAQPad-MIO-16XE-50' ;
        BoardName[44] :=  'AT-MIO-16E-1' ;
        BoardName[45] :=  'PC-OPDIO-16' ;
        BoardName[46] :=  'PC-AO-2DC' ;
        BoardName[47] :=  'DAQCard-AO-2DC' ;
        BoardName[48] :=  'DAQCard-1200' ;
        BoardName[49] :=  'DAQCard-500' ;
        BoardName[50] :=  'AT-MIO-16XE-10' ;
        BoardName[51] :=  'AT-AI-16XE-10' ;
        BoardName[52] :=  'DAQCard-AI-16XE-50' ;
        BoardName[53] :=  'DAQCard-AI-16E-4' ;
        BoardName[54] :=  'DAQCard-516' ;
        BoardName[55] :=  'PC-516' ;
        BoardName[56] :=  'PC-LPM-16PnP' ;
        BoardName[57] :=  'Lab-PC-1200' ;
        BoardName[58] :=  'Lab-PC-1200/AI' ;
        BoardName[59] :=  'Unknown' ;
        BoardName[60] :=  'Unknown' ;
        BoardName[61] :=  'VXI-AO-48XDC' ;
        BoardName[62] :=  'VXI-DIO-128' ;
        BoardName[65] :=  'PC-DIO-24/PnP' ;
        BoardName[66] :=  'PC-DIO-96/PnP' ;
        BoardName[67] :=  'AT-DIO-32HS' ;
        BoardName[69] :=  'DAQArb AT-5411' ;
        BoardName[75] :=  'DAQPad-6507/8.' ;
        BoardName[76] :=  'DAQPad-6020E for USB' ;
        BoardName[88] :=  'DAQCard-6062E' ;
        BoardName[89] :=  'DAQCard-6715' ;
        BoardName[90] :=  'DAQCard-6023E' ;
        BoardName[91] :=  'DAQCard-6024E' ;
        BoardName[200] :=  'PCI-DIO-96' ;
        BoardName[201] :=  'PCI-1200' ;
        BoardName[202] :=  'PCI-MIO-16XE-50' ;
        BoardName[203] :=  'PCI-5102' ;
        BoardName[204] :=  'PCI-MIO-16E-1' ;
        BoardName[205] :=  'PCI-MIO-16E-1' ;
        BoardName[206] :=  'PCI-MIO-16E-4' ;
        BoardName[207] :=  'PXI-6070E' ;
        BoardName[208] :=  'PXI-6040E' ;
        BoardName[211] :=  'PCI-DIO-32HS' ;
        BoardName[212] :=  'DAQArb PCI-5411' ;
        BoardName[220] :=  'PCI-6031E (MIO-64XE-10)' ;
        BoardName[221] :=  'PCI-6032E (AI-16XE-10)' ;
        BoardName[222] :=  'PCI-6033E (AI-64XE-10)' ;
        BoardName[223] :=  'PCI-6071E (MIO-64E-1)' ;
        BoardName[233] :=  'PCI-4451' ;
        BoardName[234] :=  'PCI-4452' ;
        BoardName[235] :=  'PCI-4551' ;
        BoardName[236] :=  'PPCI-4552' ;
        BoardName[240] :=  'PXI-6508' ;
        BoardName[241] :=  'PCI-6110E' ;
        BoardName[244] :=  'PCI-6110E' ;
        BoardName[256] :=  'PCI-650' ;
        BoardName[257] :=  'PXI-6503' ;
        BoardName[258] :=  'PXI-6071E' ;
        BoardName[259] :=  'PXI-6031E' ;
        BoardName[261] :=  'PCI-6711' ;
        BoardName[262] :=  'PCI-6711' ;
        BoardName[263] :=  'PCI-6713' ;
        BoardName[264] :=  'PXI-6713' ;
        BoardName[265] :=  'PCI-6704' ;
        BoardName[266] :=  'PXI-6704' ;
        BoardName[267] :=  'PCI-6023E' ;
        BoardName[268] :=  'PXI-6023E' ;
        BoardName[269] :=  'PCI-6024E' ;
        BoardName[270] :=  'PXI-6024E' ;
        BoardName[271] :=  'PCI-6025E' ;
        BoardName[272] :=  'PXI-6025E' ;
        BoardName[273] :=  'PCI-6052E' ;
        BoardName[274] :=  'PXI-6052E' ;
        BoardName[275] :=  'DAQPad-6070E' ;
        BoardName[276] :=  'DAQPad-6052E' ;
        BoardName[285] :=  'PCI-6527' ;
        BoardName[286] :=  'PXI-6527' ;
        BoardName[308] :=  'PCI-6601' ;
        BoardName[311] :=  'PCI-6703' ;
        BoardName[314] :=  'PCI-6034E' ;
        BoardName[315] :=  'PXI-6034E' ;
        BoardName[316] :=  'PCI-6035E' ;
        BoardName[317] :=  'PXI-6035E' ;
        BoardName[318] :=  'PXI-6703' ;
        BoardName[319] :=  'PXI-6608' ;
        BoardName[320] :=  'PCI-4453' ;
        BoardName[321] :=  'PCI-4454' ;
        BoardName[327] :=  'PCI-6608' ;
        BoardName[329] :=  'NI 6222(PCI)' ;
        BoardName[330] :=  'NI 6222(PXI)' ;
        BoardName[331] :=  'NI 6224 (Ethernet)' ;
        BoardName[332] :=  'DAQPad-6052E (USB)' ;
        BoardName[335] :=  'NI 4472 (PXI/CompactPCI)' ;
        BoardName[338] :=  'PCI-6115' ;
        BoardName[339] :=  'PXI-6115' ;
        BoardName[340] :=  'PCI-6120' ;
        BoardName[341] :=  'PXI-6120' ;
        BoardName[342] :=  'NI 4472 (PCI)' ;
        BoardName[347] :=  'NI 4472 (IEEE-1394)' ;
        BoardName[348] :=  'DAQCard 6036E ' ;
        BoardName[349] :=  'PCI 6036E ' ;
        BoardName[350] := 'PCI-6733' ;
        BoardName[367] :=  'PCI 6014E' ;


   { Clear A/D and D/A in progress flags }
   for i := 1 to MaxDevices do ADCActive[i] := False ;
   for i := 1 to MaxDevices do DACActive[i] := False ;
   Result := False ;

   LibraryLoaded := NIDAQ_LoadLibrary( LibraryHnd ) ;
   if not LibraryLoaded then begin ;
         LogFrm.AddLine('NIDAQ library (nidaq32.dll) not found!') ;
         Exit ;
      end ;

   // Determine number of boards installed
   Done := False ;
   NumDevices := 0 ;
   DeviceNum := 1 ;
   While (not Done) and (NumDevices < MaxDevices) do begin
       if MainFrm.AutoResetInterfaceCards then begin
        Err := Init_DA_Brds( DeviceNum, DeviceBoardType[DeviceNum] ) ;
       end
       else begin
        Err := Get_DAQ_Device_Info( DeviceNum, ND_DEVICE_TYPE_CODE, BrdType ) ;
        DeviceBoardType[DeviceNum] := SmallInt(BrdType);
       end;
       if Err = 0 then begin
          DeviceBoardName[DeviceNum] := BoardName[DeviceBoardType[DeviceNum]] ;
          DigitalWaveformCapable[DeviceNum] := False ;
          // Determine no. of DMA channels
          case DeviceBoardType[DeviceNum] of
          90..91,267..272,314..317,348..349,367 : DeviceNumDMAChannels[DeviceNum] := 1 ;
          else DeviceNumDMAChannels[DeviceNum] := 2 ;
          end ;
          Inc(NumDevices) ;
          end
       else Done := True ;
       Inc(DeviceNum) ;
       end ;
   if NumDevices <= 0 then Exit ;

   NumResources := 0 ;
   for DeviceNum := 1 to NumDevices do begin

       // Get number and properties of device A/D channels
       NIDAQ_GetDeviceADCChannelProperties( DeviceNum ) ;

       // Set analogue input DMA transfer mode
       if NumADCs[DeviceNum] > 0 then begin
          // Select DMA channel (if one is available)
          if DeviceNumDMAChannels[DeviceNum] > 1 then begin
             CheckError( Set_DAQ_Device_Info( DeviceNum,
                                              ND_DATA_XFER_MODE_AI,
                                              ND_UP_TO_1_DMA_CHANNEL ) ) ;
             end
          else begin
             CheckError( Set_DAQ_Device_Info( DeviceNum,
                                              ND_DATA_XFER_MODE_AI,
                                              ND_INTERRUPTS ) ) ;
             end ;

          // Add to resource list
          Resource[NumResources].Device := DeviceNum ;
          Resource[NumResources].ResourceType := ADCIn ;
          Resource[NumResources].StartChannel := 0 ;
          Resource[NumResources].EndChannel := NumADCs[DeviceNum]-1 ;
          Inc(NumResources) ;
          end ;

       // Get number and properties of device D/A channels
       NIDAQ_GetDeviceDACChannelProperties( DeviceNum ) ;

       // Set analogue output DMA transfer mode
       if NumDACs[DeviceNum] > 0 then begin
          CheckError( Set_DAQ_Device_Info( DeviceNum,
                                           ND_DATA_XFER_MODE_AO_GR1,
                                           ND_UP_TO_1_DMA_CHANNEL ) ) ;

          // Add to resource list
          for i := 0 to NumDACs[DeviceNum]-1 do begin
              Resource[NumResources].Device := DeviceNum ;
              Resource[NumResources].ResourceType := DACOut ;
              Resource[NumResources].StartChannel := i ;
              Resource[NumResources].EndChannel := i ;
              Inc(NumResources) ;
              end ;
          end ;

        { Set port 0 to output, mode 0 }
        CheckError( DIG_Prt_Config( DeviceNum, 0, 0, AsDigOutputPort )) ;

        // Digital O/P ports
        for i := 0 to 7 do begin
            Resource[NumResources].Device := DeviceNum ;
            Resource[NumResources].ResourceType := DIGOut ;
            Resource[NumResources].StartChannel := i ;
            Resource[NumResources].EndChannel := i ;
            Inc(NumResources) ;
            end ;

        end ;

   Result := True ;

   end ;


function TLabIO.NIDAQ_ResetNIBoards : Boolean ;
{ ----------------------------------------------------
  Reset A/D converter hardware and NI-DAQ library
  ---------------------------------------------------- }
const
     DigPortGroup = 1 ;
     AsDigOutputPort = 1 ;
var
    BoardName : array[0..370] of string[16] ;
    Err : SmallInt ;
    Done : Boolean ;
    DeviceNum : SmallInt ;
    i : Integer ;
begin

        BoardName[0] :=  'AT-MIO-16L-9' ;
        BoardName[1] :=  'AT-MIO-16L-15' ;
        BoardName[2] :=  'AT-MIO-16L-25' ;
        BoardName[3] := '?' ;
        BoardName[4] :=  'AT-MIO-16H-9' ;
        BoardName[5] :=  'AT-MIO-16H-15' ;
        BoardName[6] :=  'AT-MIO-16H-25' ;
        BoardName[7] :=  'PC-DIO-24' ;
        BoardName[8] :=  'AT-DIO-32F' ;
        BoardName[9] := '?' ;
        BoardName[10] :=  'EISA-A2000' ;
        BoardName[11] :=  'AT-MIO-16F-5' ;
        BoardName[12] :=  'PC-DIO-96/PnP' ;
        BoardName[13] :=  'PC-LPM-16' ;
        BoardName[14] :=  'PC-TIO-10' ;
        BoardName[15] :=  'AT-AO-6' ;
        BoardName[16] :=  'AT-A2150S' ;
        BoardName[17] :=  'AT-DSP2200 ' ;
        BoardName[18] :=  'AT-DSP2200 ' ;
        BoardName[19] :=  'AT-MIO-16X' ;
        BoardName[20] :=  'AT-MIO-64F-5' ;
        BoardName[21] :=  'AT-MIO-16DL-9' ;
        BoardName[22] :=  'AT-MIO-16DL-25' ;
        BoardName[23] :=  'AT-MIO-16DH-9' ;
        BoardName[24] :=  'AT-MIO-16DH-25' ;
        BoardName[25] :=  'AT-MIO-16E-2' ;
        BoardName[26] :=  'AT-AO-10' ;
        BoardName[27] :=  'AT-A2150C' ;
        BoardName[28] :=  'Lab-PC+' ;
        BoardName[29] := '?' ;
        BoardName[30] :=  'SCXI-1200' ;
        BoardName[31] :=  'DAQCard-700' ;
        BoardName[32] :=  'NEC-MIO-16E-4' ;
        BoardName[33] :=  'DAQPad-1200' ;
        BoardName[34] :=  'DAQCard-DIO-24' ;
        BoardName[36] :=  'AT-MIO-16E-10' ;
        BoardName[37] :=  'AT-MIO-16DE-10' ;
        BoardName[38] :=  'AT-MIO-64E-3' ;
        BoardName[39] :=  'AT-MIO-16XE-50' ;
        BoardName[40] :=  'NEC-AI-16E-4' ;
        BoardName[41] :=  'NEC-MIO-16XE-50' ;
        BoardName[42] :=  'NEC-AI-16XE-50' ;
        BoardName[43] :=  'DAQPad-MIO-16XE-50' ;
        BoardName[44] :=  'AT-MIO-16E-1' ;
        BoardName[45] :=  'PC-OPDIO-16' ;
        BoardName[46] :=  'PC-AO-2DC' ;
        BoardName[47] :=  'DAQCard-AO-2DC' ;
        BoardName[48] :=  'DAQCard-1200' ;
        BoardName[49] :=  'DAQCard-500' ;
        BoardName[50] :=  'AT-MIO-16XE-10' ;
        BoardName[51] :=  'AT-AI-16XE-10' ;
        BoardName[52] :=  'DAQCard-AI-16XE-50' ;
        BoardName[53] :=  'DAQCard-AI-16E-4' ;
        BoardName[54] :=  'DAQCard-516' ;
        BoardName[55] :=  'PC-516' ;
        BoardName[56] :=  'PC-LPM-16PnP' ;
        BoardName[57] :=  'Lab-PC-1200' ;
        BoardName[58] :=  'Lab-PC-1200/AI' ;
        BoardName[59] :=  'Unknown' ;
        BoardName[60] :=  'Unknown' ;
        BoardName[61] :=  'VXI-AO-48XDC' ;
        BoardName[62] :=  'VXI-DIO-128' ;
        BoardName[65] :=  'PC-DIO-24/PnP' ;
        BoardName[66] :=  'PC-DIO-96/PnP' ;
        BoardName[67] :=  'AT-DIO-32HS' ;
        BoardName[69] :=  'DAQArb AT-5411' ;
        BoardName[75] :=  'DAQPad-6507/8.' ;
        BoardName[76] :=  'DAQPad-6020E for USB' ;
        BoardName[88] :=  'DAQCard-6062E' ;
        BoardName[89] :=  'DAQCard-6715' ;
        BoardName[90] :=  'DAQCard-6023E' ;
        BoardName[91] :=  'DAQCard-6024E' ;
        BoardName[200] :=  'PCI-DIO-96' ;
        BoardName[201] :=  'PCI-1200' ;
        BoardName[202] :=  'PCI-MIO-16XE-50' ;
        BoardName[203] :=  'PCI-5102' ;
        BoardName[204] :=  'PCI-MIO-16E-1' ;
        BoardName[205] :=  'PCI-MIO-16E-1' ;
        BoardName[206] :=  'PCI-MIO-16E-4' ;
        BoardName[207] :=  'PXI-6070E' ;
        BoardName[208] :=  'PXI-6040E' ;
        BoardName[211] :=  'PCI-DIO-32HS' ;
        BoardName[212] :=  'DAQArb PCI-5411' ;
        BoardName[220] :=  'PCI-6031E (MIO-64XE-10)' ;
        BoardName[221] :=  'PCI-6032E (AI-16XE-10)' ;
        BoardName[222] :=  'PCI-6033E (AI-64XE-10)' ;
        BoardName[223] :=  'PCI-6071E (MIO-64E-1)' ;
        BoardName[233] :=  'PCI-4451' ;
        BoardName[234] :=  'PCI-4452' ;
        BoardName[235] :=  'PCI-4551' ;
        BoardName[236] :=  'PPCI-4552' ;
        BoardName[240] :=  'PXI-6508' ;
        BoardName[241] :=  'PCI-6110E' ;
        BoardName[244] :=  'PCI-6110E' ;
        BoardName[256] :=  'PCI-650' ;
        BoardName[257] :=  'PXI-6503' ;
        BoardName[258] :=  'PXI-6071E' ;
        BoardName[259] :=  'PXI-6031E' ;
        BoardName[261] :=  'PCI-6711' ;
        BoardName[262] :=  'PCI-6711' ;
        BoardName[263] :=  'PCI-6713' ;
        BoardName[264] :=  'PXI-6713' ;
        BoardName[265] :=  'PCI-6704' ;
        BoardName[266] :=  'PXI-6704' ;
        BoardName[267] :=  'PCI-6023E' ;
        BoardName[268] :=  'PXI-6023E' ;
        BoardName[269] :=  'PCI-6024E' ;
        BoardName[270] :=  'PXI-6024E' ;
        BoardName[271] :=  'PCI-6025E' ;
        BoardName[272] :=  'PXI-6025E' ;
        BoardName[273] :=  'PCI-6052E' ;
        BoardName[274] :=  'PXI-6052E' ;
        BoardName[275] :=  'DAQPad-6070E' ;
        BoardName[276] :=  'DAQPad-6052E' ;
        BoardName[285] :=  'PCI-6527' ;
        BoardName[286] :=  'PXI-6527' ;
        BoardName[308] :=  'PCI-6601' ;
        BoardName[311] :=  'PCI-6703' ;
        BoardName[314] :=  'PCI-6034E' ;
        BoardName[315] :=  'PXI-6034E' ;
        BoardName[316] :=  'PCI-6035E' ;
        BoardName[317] :=  'PXI-6035E' ;
        BoardName[318] :=  'PXI-6703' ;
        BoardName[319] :=  'PXI-6608' ;
        BoardName[320] :=  'PCI-4453' ;
        BoardName[321] :=  'PCI-4454' ;
        BoardName[327] :=  'PCI-6608' ;
        BoardName[329] :=  'NI 6222(PCI)' ;
        BoardName[330] :=  'NI 6222(PXI)' ;
        BoardName[331] :=  'NI 6224 (Ethernet)' ;
        BoardName[332] :=  'DAQPad-6052E (USB)' ;
        BoardName[335] :=  'NI 4472 (PXI/CompactPCI)' ;
        BoardName[338] :=  'PCI-6115' ;
        BoardName[339] :=  'PXI-6115' ;
        BoardName[340] :=  'PCI-6120' ;
        BoardName[341] :=  'PXI-6120' ;
        BoardName[342] :=  'NI 4472 (PCI)' ;
        BoardName[347] :=  'NI 4472 (IEEE-1394)' ;
        BoardName[348] :=  'DAQCard 6036E ' ;
        BoardName[367] :=  'PCI 6014E' ;


   { Clear A/D and D/A in progress flags }
   for i := 1 to MaxDevices do ADCActive[i] := False ;
   for i := 1 to MaxDevices do DACActive[i] := False ;
   Result := False ;

   LibraryLoaded := NIDAQ_LoadLibrary( LibraryHnd ) ;
   if not LibraryLoaded then begin ;
         LogFrm.AddLine('NIDAQ library (nidaq32.dll) not found!') ;
         Exit ;
      end ;

   // Determine number of boards installed
   Done := False ;
   NumDevices := 0 ;
   DeviceNum := 1 ;
   While (not Done) and (NumDevices < MaxDevices) do begin
       Err := Init_DA_Brds( DeviceNum, DeviceBoardType[DeviceNum] ) ;
       if Err = 0 then begin
          DeviceBoardName[DeviceNum] := BoardName[DeviceBoardType[DeviceNum]] ;
          DigitalWaveformCapable[DeviceNum] := False ;
          // Determine no. of DMA channels
          case DeviceBoardType[DeviceNum] of
          90..91,267..272,314..317,348..349,367 : DeviceNumDMAChannels[DeviceNum] := 1 ;
          else DeviceNumDMAChannels[DeviceNum] := 2 ;
          end ;
          Inc(NumDevices) ;
          end
       else Done := True ;
       Inc(DeviceNum) ;
       end ;
   if NumDevices <= 0 then Exit ;

   NumResources := 0 ;
   for DeviceNum := 1 to NumDevices do begin

       // Get number and properties of device A/D channels
       NIDAQ_GetDeviceADCChannelProperties( DeviceNum ) ;

       // Set analogue input DMA transfer mode
       if NumADCs[DeviceNum] > 0 then begin
          // Select DMA channel (if one is available)
          if DeviceNumDMAChannels[DeviceNum] > 1 then begin
             CheckError( Set_DAQ_Device_Info( DeviceNum,
                                              ND_DATA_XFER_MODE_AI,
                                              ND_UP_TO_1_DMA_CHANNEL ) ) ;
             end
          else begin
             CheckError( Set_DAQ_Device_Info( DeviceNum,
                                              ND_DATA_XFER_MODE_AI,
                                              ND_INTERRUPTS ) ) ;
             end ;

          // Add to resource list
          Resource[NumResources].Device := DeviceNum ;
          Resource[NumResources].ResourceType := ADCIn ;
          Resource[NumResources].StartChannel := 0 ;
          Resource[NumResources].EndChannel := NumADCs[DeviceNum]-1 ;
          Inc(NumResources) ;
          end ;

       // Get number and properties of device D/A channels
       NIDAQ_GetDeviceDACChannelProperties( DeviceNum ) ;

       // Set analogue output DMA transfer mode
       if NumDACs[DeviceNum] > 0 then begin
          CheckError( Set_DAQ_Device_Info( DeviceNum,
                                           ND_DATA_XFER_MODE_AO_GR1,
                                           ND_UP_TO_1_DMA_CHANNEL ) ) ;

          // Add to resource list
          for i := 0 to NumDACs[DeviceNum]-1 do begin
              Resource[NumResources].Device := DeviceNum ;
              Resource[NumResources].ResourceType := DACOut ;
              Resource[NumResources].StartChannel := i ;
              Resource[NumResources].EndChannel := i ;
              Inc(NumResources) ;
              end ;
          end ;

        { Set port 0 to output, mode 0 }
        CheckError( DIG_Prt_Config( DeviceNum, 0, 0, AsDigOutputPort )) ;

        // Digital O/P ports
        for i := 0 to 7 do begin
            Resource[NumResources].Device := DeviceNum ;
            Resource[NumResources].ResourceType := DIGOut ;
            Resource[NumResources].StartChannel := i ;
            Resource[NumResources].EndChannel := i ;
            Inc(NumResources) ;
            end ;

        end ;

   Result := True ;

   end ;


procedure TLabIO.NIDAQ_GetDeviceDACChannelProperties(
          DeviceNum : Integer
          ) ;
// ------------------------------------------------
// Get number of device D/A channels and properties
// ------------------------------------------------
var
   NumChannels : Integer ;
   Err : Integer ;
   iValue16 : SmallInt ;
   Done : Boolean ;
begin

     // Quit if device does not exist
     if (DeviceNum < 1) or (DeviceNum > NumDevices) then Exit ;

     Done := False ;
     NumChannels := 0 ;
     while (not Done) and (NumChannels < 16) do begin
         Err := AO_Configure( DeviceNum, NumChannels, 0, 0, 10., 1 ) ;
         if Err = 0 then Inc(NumChannels)
                    else Done := True ;
         end ;
     NumDACs[DeviceNum] := NumChannels ;

     // Quit here if no D/A channels
     if NumDACs[DeviceNum] <= 0 then Exit ;

     // Determine limits of DAC binary integer values (12/16 bit) }
     AO_VScale ( DeviceNum, 0, 5.0, iValue16 ) ;
     if iValue16 > 2047 then DACMaxValue[DeviceNum] := 32767
                        else DACMaxValue[DeviceNum] := 2047 ;
     DACMinValue[DeviceNum] := -DACMaxValue[DeviceNum] - 1 ;

     // Determine upper limit of bipolar D/A voltage range }
     CheckError( AO_VScale( DeviceNum, 0, 4.9, iValue16 ) ) ;
     if iValue16 > (DACMaxValue[DeviceNum] div 2) then DACMaxVolts[DeviceNum] := 5.0
                                                  else DACMaxVolts[DeviceNum] := 10.0 ;
     DACScale[DeviceNum] := DACMaxValue[DeviceNum] / Max(DACMaxVolts[DeviceNum],1E-6) ;

     end ;


procedure TLabIO.NIDAQ_GetDeviceADCChannelProperties(
          DeviceNum : Integer
          ) ;
// ------------------------------------------------
// Get number of device A/D channels and properties
// ------------------------------------------------
const
    Gains : Array[0..9] of Integer = (-1,1,2,4,5,8,10,20,50,100) ;
var
   Err : Integer ;
   iValue : SmallInt ;
   iGain : Integer ;
   Done : Boolean ;
   V,Voltage : Double ;
   NumChannels : Integer ;
   NumVRanges : Integer ;
begin

     // Quit if device does not exist
     if (DeviceNum < 1) or (DeviceNum > NumDevices) then Exit ;

     Done := False ;
     NumChannels := 0 ;
     while (not Done) and (NumChannels < 128) do begin
         Err := AI_Read( DeviceNum, NumChannels, 1, iValue ) ;
         if Err = 0 then Inc(NumChannels)
                    else Done := True ;
         end ;
     NumADCs[DeviceNum] := NumChannels ;

     // Quit here if no A/D channels
     if NumADCs[DeviceNum] <= 0 then Exit ;

     // Determine voltage range at X1 gain
     V := 1.0 ;
     Err := AI_VScale( DeviceNum,0,1,1.0,0.0,2047, V ) ;
     if V < 4.9 then begin
        Err := AI_VScale( DeviceNum,0,1,1.0,0.0,32767,V ) ;
        end ;
     ADCVoltageRangeAtX1Gain[DeviceNum] := V ;

     // Determine voltage range of valid gains
     NumVRanges := 0 ;
     for iGain := 0 to High(Gains) do begin
         Err := AI_Read( DeviceNum, 0, Gains[iGain], iValue ) ;
         if Err = 0 then begin
            if Gains[iGain] >= 1 then
               ADCVoltageRanges[DeviceNum,NumVRanges] := ADCVoltageRangeAtX1Gain[DeviceNum]/Gains[iGain]
            else ADCVoltageRanges[DeviceNum,NumVRanges] := ADCVoltageRangeAtX1Gain[DeviceNum] /0.5 ;
            Inc(NumVRanges) ;
            end ;
         end ;

     NumADCVoltageRanges[DeviceNum] := NumVRanges ;

     // Determine limits of ADC binary integer values (12/16 bit)
     AI_VScale ( DeviceNum, 0, 1, 1.0, 0.0, 2047, Voltage) ;
     if Voltage < (0.5*ADCVoltageRangeAtX1Gain[DeviceNum]) then ADCMaxValue[DeviceNum] := 32767
                                                           else ADCMaxValue[DeviceNum] := 2047 ;
     ADCMinValue[DeviceNum] := -ADCMaxValue[DeviceNum] -1 ;

     end ;


function TLabIO.NIDAQ_ADCToMemoryExtScan(
          Device : SmallInt ;
          var ADCBuf : Array of SmallInt  ;  { A/D sample buffer (OUT) }
          nChannels : Integer ;              { Number of A/D channels (IN) }
          nSamples : Integer ;               { Number of A/D samples ( per channel) (IN) }
          ADCVoltageRange : Single ;         { A/D input voltage range (V) (IN) }
          CircularBuffer : Boolean ;          { Repeated sampling into buffer (IN) }
          TimingDevice : SmallInt            // Device supplying timing pulses
          ) : Boolean ;                      { Returns TRUE indicating A/D started }
{ ----------------------------
  Start A/D converter sampling
  ----------------------------}
var
   ch : Integer ;
   Gain : SmallInt ;
   ChanVector : array[0..15] of SmallInt ;
   GainVector : array[0..15] of SmallInt ;
   SyncLine : Integer ;
   ADCModeCode : SmallInt ;
begin
     Result := False ;
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumADCs[Device] <= 0 then Exit ;

     // Reset Data acquisition sub-system
     DAQ_Clear(Device) ;

     // Set A/D input mode
     if (ADCInputMode = imDifferential) or
        (ADCInputMode = imBNC2110) then ADCModeCode := 0     // Differential
     else if ADCInputMode = imSingleEndedRSE then ADCModeCode := 1
     else ADCModeCode := 2 ; // NRSE

     CheckError(AI_Configure( Device,
                              -1,
                              ADCModeCode,
                              0,
                              0,
                              0)) ;

     // Set acquisition to external scan trigger from PFI_7
     CheckError(DAQ_Config( Device,0,2)) ;

     {If in 'CircularBuffer' mode set A/D converter to continue
      indefinitely filling ADC buffer }
     if CircularBuffer then CheckError(DAQ_DB_Config(Device, 1))
                       else CheckError(DAQ_DB_Config(Device, 0)) ;

     { Set internal gain for A/D converter's programmable amplifier }
     FADCVoltageRange[Device] := ADCVoltageRange ;
     Gain := Trunc( (ADCVoltageRangeAtX1Gain[Device] + 0.001) / Max(ADCVoltageRange,1E-6) ) ;
     if Gain < 1 then Gain := -1 ;

     // Define A/D channel offset sequence within A/D sample buffer
     for ch := 0 to nChannels-1 do ADCChannelOffsets[ch] := ch ;

     { Multi-channel A/D conversion for cards with Channel/Gain lists }
     { Note ... channels are acquired in descending order }
     for ch := 0 to nChannels-1 do begin
         ChanVector[ch] := ADCChannelOffsets[ch] ;
         GainVector[ch] := Gain ;
         end ;

     CheckError(SCAN_Setup( Device,
                            nChannels,
                            @ChanVector,
                            @GainVector ) );

     // Configure A/D scans to be triggered by D/A update signal on clock sync. line
     if MainFrm.IOConfig.ClockSyncLine = ClockSync_RTSI0 then begin
        case TimingDevice of
           1 : SyncLine := ND_RTSI_0 ;
           2 : SyncLine := ND_RTSI_1 ;
           3 : SyncLine := ND_RTSI_1 ;
           else SyncLine := ND_RTSI_1 ;
           end ;
        CheckError( Select_Signal (Device, ND_IN_SCAN_START, SyncLine, ND_HIGH_TO_LOW)) ;
        end
     else begin
        SyncLine := ND_PFI_5 ;
        CheckError( Select_Signal (Device, ND_IN_SCAN_START, ND_PFI_5,ND_HIGH_TO_LOW)) ;
        end ;

     CheckError(SCAN_Start( Device,
                            @ADCBuf,
                            nSamples*nChannels,
                            TimeBase_1us,5,
                            TimeBase_1us,0)) ;

     ADCActive[Device] := True ;
     Result := ADCActive[Device] ;
     end ;



function TLabIO.NIDAQ_StopADC(
         Device : SmallInt
         ) : Boolean ;      { Returns FALSE = A/D stopped }
{ -------------------------------
  Reset A/D conversion sub-system
  -------------------------------}
begin
     Result := False ;
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumADCs[Device] <= 0 then Exit ;
     if not ADCActive[Device] then Exit ;

     DAQ_Clear(Device) ;

     ADCActive[Device] := False ;
     Result := ADCActive[Device] ;

     end ;


{ ---------------------------------------------------------
  Check sampling interval to make sure it lies within valid
  range and adjust it to match sample clock settings
  ---------------------------------------------------------}
procedure TLabIO.NIDAQ_CheckSamplingInterval(
          var SamplingInterval : double ;  { Sampling interval (IN/OUT) }
          var TimeBase : SmallInt ;        { Clock timebase code (OUT) }
          var ClockTicks : Word            { No. clock ticks (OUT) }
          )  ;
var
   ClockInterval : array[-3..5] of Single ;
begin

     { Determine sampling clock time-base and number of ticks }
     DAQ_Rate (SamplingInterval,1,Timebase,ClockTicks ) ;
     ClockInterval[-3] := 5E-8 ;
     ClockInterval[-1] := 2E-7 ;
     ClockInterval[0] := 1E-6 ;
     ClockInterval[1] := 1E-6 ;
     ClockInterval[2] := 1E-5 ;
     ClockInterval[3] := 1E-4 ;
     ClockInterval[4] := 1E-3 ;
     ClockInterval[5] := 1E-2 ;

     // (7/3/01) Timebase shifted to next lower frequency when Clockticks greater
     // than 2000 to fix -10697 error with E-Series boards when Scan_Start called
     // with ClockTicks over 3000
     if (ClockTicks > 2000) and (Timebase < 5) then begin
        Inc(Timebase) ;
        ClockTicks := ClockTicks div 10 ;
        end ;

     SamplingInterval := ClockTicks * ClockInterval[TimeBase] ;
     end ;


procedure TLabIO.NIDAQ_CheckError(
          Err : Integer
          ) ;
{ --------------------------------------------------------------
  Warn User if the NIDAQ Lab. interface library returns an error
  --------------------------------------------------------------}
begin

     if Err <> 0 then ShowMessage(' Lab. Interface Error = ' +
                                   format('%d',[Err]));
     end ;


function TLabIO.NIDAQ_MemoryToDAC(
          Device : SmallInt ;
          var DACBuf : Array of SmallInt  ; { D/A output data buffer (IN) }
          nChannels : SmallInt ;            { No. of D/A channels (IN) }
          nPoints : Integer ;               { No. of D/A output values (IN) }
          UpdateInterval : Double ;          { D/A output interval (s) (IN) }
          CircularBufferMode : Boolean ;     { TRUE = continuous update from circular buffer }
          ExternalTrigger : Boolean ;        // TRUE = Wait for external trigger
          TimingDevice : SmallInt            // Device suppling ADC/DAC timing pulses
          ): Boolean ;                      { Returns TRUE=D/A active }
{ --------------------------
  Start D/A converter output
  --------------------------}
var
   TimeBase : SmallInt ;
   ClockTicks,nDACValues : Cardinal ;
   Channels : array[0..8] of SmallInt ;
   NumIterations : Cardinal ;
   i : Integer ;
   SyncLine : Integer ;
begin

     Result := False ;
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumDACs[Device] <= 0 then Exit ;

     if DACActive[Device] then StopDAC(Device) ;

     { Set D/A update clock }
     CheckError(WFM_Rate( UpdateInterval, 1,Timebase,ClockTicks));

     // Configure A/D scans to be triggered by D/A update signal on clock sync. line
     if MainFrm.IOConfig.ClockSyncLine = ClockSync_RTSI0 then begin
        case TimingDevice of
           1 : SyncLine := ND_RTSI_0 ;
           2 : SyncLine := ND_RTSI_1 ;
           3 : SyncLine := ND_RTSI_1 ;
           else SyncLine := ND_RTSI_1 ;
           end ;
        end
     else begin
        SyncLine := ND_PFI_5 ;
        end ;

     // Set D/A clock source
     if TimingDevice <> Device then begin
        Timebase := 0 ;
        CheckError( Select_Signal (Device, ND_OUT_UPDATE, SyncLine,ND_HIGH_TO_LOW)) ;
        end
     else begin
        CheckError( Select_Signal (Device, ND_OUT_UPDATE, ND_INTERNAL_TIMER , ND_LOW_TO_HIGH)) ;
        CheckError( Select_Signal (Device, SyncLine, ND_OUT_UPDATE, ND_HIGH_TO_LOW)) ;
        end ;

     CheckError(WFM_ClockRate(Device,1,0,TimeBase,ClockTicks,0));

     { Set up D/A channel selection array }
     for i := 0 to High(Channels) do Channels[i] := i ;

     if CircularBufferMode then NumIterations := 0
                           else  NumIterations := 1 ;
     { Load D/A data into output buffer }
     nDACValues := nPoints*nChannels ;
     CheckError( WFM_Load( Device,
                           nChannels,
                           @Channels[0],
                           @DACBuf[0],
                           nDACValues,
                           NumIterations,
                           0)) ;

     if ExternalTrigger then begin
       // Wait for active-high trigger on PFI0
       CheckError( Select_Signal (Device, ND_OUT_START_TRIGGER, ND_PFI_0,ND_LOW_TO_HIGH)) ;
       end
     else begin
       // Start immediately
       CheckError( Select_Signal (Device, ND_OUT_START_TRIGGER, ND_AUTOMATIC,ND_LOW_TO_HIGH)) ;
       end ;

     { Begin D/A output sequence }
     CheckError(WFM_Group_Control(Device,1,1)) ;
     DACActive[Device] := True ;

     // Update output buffer state info
     DAC[Device].EndOfBuf := nPoints-1 ;
     DAC[Device].NumChannels := nChannels ;
     DAC[Device].Buf := @DACBuf ;
     DAC[Device].CircularBuffer := CircularBufferMode ;
     DAC[Device].Pointer := 0 ;

     Result := DACActive[Device] ;
     end ;


procedure TLabIO.NIDAQ_UpdateDACOutputBuffer ;
// -------------------------------------------------
// Update internal NIDAQ DAC output buffer position
// -------------------------------------------------
var
    iDev,iStopped : SmallInt ;
    PointsDone,ItersDone : Cardinal ;
begin

     for iDev := 1 to NumDevices do begin
         if NumDACs[iDev] <= 0 then Continue ;
         WFM_Check (iDev, 0, iStopped, itersDone, pointsDone) ;
         DAC[iDev].Pointer := pointsDone ;
         end ;
     end ;


function TLabIO.NIDAQ_StopDAC(
         Device : SmallInt
         ) : Boolean ;    { Returns FALSE = D/A stopped }
{ ---------------
  Stop D/A output
  --------------- }
begin
     Result := False ;
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumDACs[Device] <= 0 then Exit ;
     if not DACActive[Device] then Exit ;

     WFM_Group_Control(Device,1,0) ;

     DACActive[Device] := False ;
     Result := DACActive[Device] ;

     end ;


procedure TLabIO.NIDAQ_WriteDACs(
          Device : Integer ;
          DACVolts : array of Single ;
          nChannels : Integer ) ;
{ --------------------------------------
  Write values to D/A converter outputs
  -------------------------------------}
var
   iDACValue,ch : Integer ;
   iDACValue16 : SmallInt ;
   DACMinValue : Integer ;
begin

     // Quit if device does not exist or have DACs
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumDACs[Device] <= 0 then Exit ;

     { Output the final D/A values }
     for ch := 0 to nChannels-1 do begin
         CheckError( AO_Configure( Device, ch, 0, 0, 10., 1 ) ) ;
         iDACValue := Round((DACVolts[ch]*DACMaxValue[Device])/DACMaxVolts[Device]) ;
         DACMinValue := -DACMaxValue[Device] - 1 ;
         iDACValue16 := IntLimit( iDACValue, DACMinValue, DACMaxValue[Device] ) ;
         CheckError(AO_Write(Device,ch,iDACValue16)) ;
         end ;
     CheckError( AO_Update(Device) ) ;
     end ;


procedure TLabIO.NIDAQ_WriteDAC(
          Device : Integer ;
          DACVolts : Single ;
          iChannel : Integer ) ;
{ ----------------------------------------------
  Write values to a D/A converter output channel
  ----------------------------------------------}
var
   iDACValue : Integer ;
   iDACValue16 : SmallInt ;
   DACMinValue : Integer ;
begin

     // Quit if device does not exist or have DACs
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if iChannel >= NumDACs[Device] then Exit ;

     { Output the final D/A values }
     CheckError( AO_Configure( Device, iChannel, 0, 0, 10., 1 ) ) ;
     iDACValue := Round((DACVolts*DACMaxValue[Device])/DACMaxVolts[Device]) ;
     DACMinValue := -DACMaxValue[Device] - 1 ;
     iDACValue16 := IntLimit( iDACValue, DACMinValue, DACMaxValue[Device] ) ;
     CheckError(AO_Write(Device,iChannel,iDACValue16)) ;
     CheckError( AO_Update(Device) ) ;

     end ;


procedure TLabIO.NIDAQ_WriteToDigitalOutPutPort(
          Device : Integer ;
          Pattern : Integer
          ) ;
{ ----------------------
  Update digital port 0
  ---------------------}
const
     DigPortGroup = 1 ;
     AsDigOutputPort = 1 ;

var
   PortList : Array[0..4] of Integer ;
begin

     if (Device < 1) or (Device > NumDevices) then Exit ;

     { Clear any existing block transfers }
     //if DigActive then Dig_Block_Clear( Device, DigPortGroup ) ;

     { Clear port assignments to group 0 }
     PortList[0] := 0 ;
     Dig_SCAN_Setup( Device, DigPortGroup, 0, @PortList, AsDigOutputPort ) ;
     { Note ... No CheckError because an error occurs when group
       is cleared and none has been assigned 4.9.0 }

     { Set port 0 to output, mode 0 }
     DIG_Prt_Config( Device, 0, 0, AsDigOutputPort ) ;
     { NOTE No CheckError because an error occurs here but doesn't
       seem to affect operation 24/8/99 }

     { Send the byte pattern }
     CheckError(DIG_Out_Port( Device, 0, Pattern )) ;

     end ;


function TLabIO.NIDAQ_ReadADC(
         Device : Integer ;
         Channel : Integer ;       { A/D channel to be read (IN) }
         ADCVoltageRange : Single  { A/D converter input voltage range (V) (IN) }
         ) : Integer ;
// ----------------------------------------------
// Single read of selected A/D converter channel
// ----------------------------------------------
var
   ADCREading, Gain: SmallInt ;
   ADCModeCode : SmallInt ;
begin
     Result := 0 ;
     // Quit if device does not exist or have ADCs
     if (Device < 1) or (Device > NumDevices) then Exit ;
     if NumADCs[Device] <= 0 then Exit ;

     // Set A/D input mode
     if (ADCInputMode = imDifferential) or
        (ADCInputMode = imBNC2110) then ADCModeCode := 0     // Differential
     else if ADCInputMode = imSingleEndedRSE then ADCModeCode := 1
     else ADCModeCode := 2 ; // NRSE

     CheckError(AI_Configure( Device,
                              -1,
                              ADCModeCode,
                              0,
                              0,
                              0)) ;

     { Set internal gain for A/D converter's programmable amplifier }

     FADCVoltageRange[Device] := ADCVoltageRange ;
     Gain := Trunc( (ADCVoltageRangeAtX1Gain[Device]/ADCVoltageRange) + 0.001 ) ;
     if Gain < 1 then Gain := -1 ;

     CheckError( AI_Read( 1, Channel, Gain, ADCReading ) ) ;
     Result := ADCReading ;

     end ;


procedure TLabIO.Wait( Delay : Single ) ;
var
  T : Integer ;
  TExit : Integer ;
begin
    T := TimeGetTime ;
    TExit := T + Round(Delay*1E3) ;
    while T < TExit do begin
       T := TimeGetTime ;
       Application.ProcessMessages ;
       end ;
    end ;

function TLabIO.BitMask( BitNumber : Integer ) : Integer ;
// ----------------------------------------------
// Return bit mask from number of bit (0..15) within word
// ----------------------------------------------
var
    i, Bit : Integer ;

begin
    Bit := 1 ;
    for i := 1 to BitNumber do Bit := Bit*2 ;
    Result := Bit ;
    end ;



end.
