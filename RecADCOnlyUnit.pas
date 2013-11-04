unit RecADCOnlyUnit;
// =====================================================================================
// WinFluor - A/D Signals Recording Module (c) J. Dempster, University of Strathclyde 2001-03
// =====================================================================================
// All Rights Reserved
// 28.05.04
// 02.09.04 Digital pulse stimulus added
// 31.05.05 VPR voltage program stimulus generator added
// 23.06.05 Files now switched to writeenabled mode for recording
// 01.09.05 A/D channel settings now stored in MainFrm.ADCChannel
// 16.11.05 Support for Praire Ultima added
//          FirstCall flag removed and initial setting of ADCOldScan tided up
// 12.12.05 Recording Time setting now retained between form closures
// 30.01.08 Empty flag changed to 32766
// 27.03.08 ImportImageFile can now be called multiple times to import images
//          from multiple files.
// 31.03.08 NumFrameTypes now set to NumFrames when line scans imported
// 23.01.09 .... Nicholas Schwarz's version
// 11.03.08 JD .. Digital trigger output outputs now supported with M series cards
// 04.10.09 NS .. Added third DAC channel, Vout2
// 15.11.12 DE Added MainFrm.LastRecADCOnly* variables to allow Record Signals
//          window to remember its size and location after being dismissed
//          (requested by M. Day)
// 19.11.12 DE Modified FormActivate so that if no new file has been opened,
//          the Record form will auotmatically open the next default file
//          (feature request from M. Day)
// 11.12.12 DE Added brief delay to allow photostimulus shutter to close
// 08.01.13 DE Voltage stimulus would not start on some rigs
//
// 23.04.13 JD .. Special StartADC() calls when running with NIDAQmx library removed.
// 24.04.13 JD .. Real time display sweep now correctly synchronised to incoming signal
//                by using ADCMaxBlocksDisplayed rather than scADCDisplay.Maxpoints to detect end of sweep
//                Single stimulus D/A buffer padding increased from 1s to 2s to prevent
//                stimulus repetition due to 2 second internal NIDAQmx buffering
// 02.05.13 JD .. Single stimulus D/A buffer padding set to LabIO.InternalBufferDuration + 0.5s a
//                to prevent stimulus repetition in single stimulus mode.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ScopeDisplay, StdCtrls, ValidatedEdit, IDRFile,
  ImageFile, HTMLLabel, LabIOUnit, StrUtils, math, SESCam ;

const
    cMaxSamplesInADCBuffer = 1000000 ;
    MaxADCChannels = 8 ;
    MaxDisplayScans = 2000 ;
    cADCWriteBufferDuration = 2.0 ;
    DACCameraStartChannel = 1 ;

    WriteBufferDuration = 2.0 ;    // Duration of A/D write to file buffer (s)
    MinWriteBufferScans = 16 ;     // Min. no. of samples in write buffer
    ADCNumWriteBuffers = 4 ;       // No. of write buffers in ring buffer

    LSMLineScanMode = 1 ;
    LSMXYSeriesSequenceMode = 2 ;
    LSMXYSeriesFrameTriggerMode = 3 ;


type

  TRecADCOnlyFrm = class(TForm)
    ControlsGrp: TGroupBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    bRecord: TButton;
    bStop: TButton;
    edRecordingTime: TValidatedEdit;
    MarkGrp: TGroupBox;
    edMarker: TEdit;
    bMark: TButton;
    EdNumMarkers: TEdit;
    SignalsGrp: TGroupBox;
    Timer: TTimer;
    IdentGrp: TGroupBox;
    Label2: TLabel;
    edIdent: TEdit;
    ImageCaptureGrp: TGroupBox;
    OpenDialog: TOpenDialog;
    ImageFile: TImageFile;
    TDisplayPanel: TPanel;
    edTDisplay: TValidatedEdit;
    rbTDisplayUnitMins: TRadioButton;
    rbTDisplayUnitsSecs: TRadioButton;
    ckFixZeroLevels: TCheckBox;
    ModeGrp: TGroupBox;
    rbImage: TRadioButton;
    rbLineScan: TRadioButton;
    lbInterval: TLabel;
    edInterval: TValidatedEdit;
    TriggerGrp: TGroupBox;
    GroupBox5: TGroupBox;
    rbTriggerTTLHigh: TRadioButton;
    rbTriggerTTLLow: TRadioButton;
    rbFrameTrigger: TRadioButton;
    rbSequenceTrigger: TRadioButton;
    StimulatorGrp: TGroupBox;
    bStartStimulus: TButton;
    cbStimProgram: TComboBox;
    bStopStimulus: TButton;
    VHoldGrp: TGroupBox;
    VHold0Panel: TPanel;
    lbVHold0: TLabel;
    edVHold0: TValidatedEdit;
    VHold1Panel: TPanel;
    Label7: TLabel;
    edVHold1: TValidatedEdit;
    ReadoutGrp: TGroupBox;
    lbMeasurements: THTMLLabel;
    ckRecordADCSignalsOnly: TCheckBox;
    Label3: TLabel;
    edImageStartDelay: TValidatedEdit;
    bTDisplayHalf: TButton;
    bTDisplayDouble: TButton;
    VHold2Panel: TPanel;
    Label4: TLabel;
    edVHold2: TValidatedEdit;
    PhotoStimulusGrp: TGroupBox;
    cbPhotoStimProgram: TComboBox;
    ckPhotoStimEnabled: TCheckBox;
    DynamicProtocolGrp: TGroupBox;
    bDynamicProtocolSetup: TButton;
    ckDynamicProtocolEnabled: TCheckBox;
    PlaybackGrp: TGroupBox;
    bPlaybackSetup: TButton;
    ckPlaybackEnabled: TCheckBox;
    scADCDisplay: TScopeDisplay;
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure bMarkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bStartStimulusClick(Sender: TObject);
    procedure bStopStimulusClick(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure bRecordClick(Sender: TObject);
    procedure rbTDisplayUnitsSecsClick(Sender: TObject);
    procedure scADCDisplayCursorChange(Sender: TObject);
    procedure edTDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure cbStimProgramChange(Sender: TObject);
    procedure rbImageClick(Sender: TObject);
    procedure rbLineScanClick(Sender: TObject);
    procedure edIdentKeyPress(Sender: TObject; var Key: Char);
    procedure edVHold0KeyPress(Sender: TObject; var Key: Char);
    procedure bTDisplayHalfClick(Sender: TObject);
    procedure bTDisplayDoubleClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure edRecordingTimeKeyPress(Sender: TObject; var Key: Char);
    procedure ckFixZeroLevelsClick(Sender: TObject);
    procedure ckPhotoStimEnabledClick(Sender: TObject);
    procedure ckDynamicProtocolEnabledClick(Sender: TObject);
    procedure bDynamicProtocolSetupClick(Sender: TObject);
    procedure ckPlaybackEnabledClick(Sender: TObject);
    procedure bPlaybackSetupClick(Sender: TObject);
    procedure cbPhotoStimProgramChange(Sender: TObject);
    procedure ckRecordADCSignalsOnlyClick(Sender: TObject);
  private
    { Private declarations }
    ADCDevice : SmallInt ;                    // Device # of A/D Converter
    DeviceInUse : Array[1..MaxDevices] of Boolean ; // I/O devices in use
    DeviceDigInUse : Array[1..MaxDevices] of Boolean ; // I/O devices digital O/P in use
    TimerProcBusy : Boolean ;              // TRUE = scheduled timer code executing
    ResetDisplays : Boolean ;              // TRUE = reset all time course displays
    InitialisationComplete : Boolean ;     // TRUE = formshow initialisations done
    IDRFilePointer : Int64 ;               // IDR file pointer
    EDRFilePointer : Int64 ;               // EDR file pointer

    RecordingTimeElapsed : Single ;        // Recording time acquired
    RecordingTimeRequired : Single ;       // Recording time required

    // D/A output buffer
    DACBufs : Array[0..MaxDevices] of PBig16bitArray ; // D/A ring buffer pointer
    DACNumScansInBuffer : Integer ;           // No. D/A channel scans in ring buffer
    DACNumScansPerFrame : Integer ;           // No. D/A scans per image frame
    DACNumFramesPerBuf : Integer ;            // No. frames per buffer
    PDACBuf : PBig16bitArray ;

    ClearStimulusAfterScanNum : Integer ;
    SingleStimulusInProgress : Boolean ;

    DigBufs : Array[0..MaxDevices] of PBig32bitArray ; // D/A ring buffer pointer

    // A/D input buffer
    ADCBuf : PBig16bitArray ;                 // A/D ring buffer pointer
    ADCNumScansInBuffer : Integer ;           // No. of A/D scans per buffer cycle
    ADCNumSamplesInBuffer : Integer ;         // Total no. of samples in circular A/D buffer ADCBuf
    ADCNumSamplesInWriteBuffer : Integer ;    // No. of A/D samples in write buffer
    ADCWriteBuffer : Integer ;                // No. of next buffer to be written to file

    ADCActiveBuffer : Integer ;               // No. of sub-buffer being filled
    ADCRunning : Boolean ;                    // TRUE = A/D converter acquiring samples
    ADCPointer : Integer ;                    // Buffer index of latest A/D sample acquired
    ADCNumScansCumulative : Integer ;         // Cumulative number of A/D scans

    // Display buffer counters
    ADCNumScansPerBlock : Integer ;          // No. of A/D channel scans per display block
    ADCNumPointsPerBlock : Integer ;         // No. of A/D samples per display block
    ADCMaxBlocksDisplayed : Integer ;        // Max. no. of blocks in display
    ADCNumBlocksDisplayed : Integer ;
    ADCBlockCount : Integer ;
    ADCEmptyPointer : Integer ;
    ADCDispPointer : Integer ;
    ADCOldestScan : Integer ;               // Index of oldest scan processed
    ADCLatestScan : Integer ;               // Index of latest available scan
    ADCStartDisplayScan : Integer ;         // Index of scan at start of current display
    ADCyMin : Array[0..MaxADCChannels-1] of Integer ;
    ADCyMax : Array[0..MaxADCChannels-1] of Integer ;
    ADCyMinAt : Array[0..MaxADCChannels-1] of Integer ;
    ADCyMaxAt : Array[0..MaxADCChannels-1] of Integer ;
    ADCMaxValue : Integer ;
    ADCMinValue : Integer ;

    ADCDisplayBuf : Array[0..(MaxADCChannels*MaxDisplayScans*4)-1] of SmallInt ;

    TimeCourseCursor : TPoint ;
    MousePosition : TPoint ;
    ReadOutValues : Array[0..7] of SmallInt ;     // Latest cursor readout values


    ADCDisplayPointer : Integer ;                 // Current A/D sample pointer

    StimulusRequired : Boolean ;

    DynamicProtocolCounter : Integer ;        // Dynamic protocol scan counter
    DynamicProtocolEnabled : Boolean ;        // Dyanmic protocol enabled flag

    RecordingFirstTrace: Boolean;
    OldTScale: Single;

    procedure UpdateCameraStartWaveform ;
    procedure UpdateCameraStartWaveformDAC ;
    procedure UpdateCameraStartWaveformDIG ;
    procedure UpdateStimulusWaveforms(
              StimulusEnabled : Boolean ;  // TRUE = Generate stimulus
              InitialiseBuffer : Boolean   // TRUE = initialise LABIO.DAC
              )   ;
    procedure UpdatePhotoStimulus( StimulusEnabled : Boolean )   ;
    procedure UpdatePlaybackWaveform( StimulusEnabled : Boolean )   ;    
    procedure UpdateHoldingVoltage ;
    procedure UpdateADCDisplay ;
    procedure NewDisplaySetup ;
    procedure SetDisplayUnits ;
    procedure SetInterval( Value : Single ) ;
    function GetInterval : Single ;
    procedure SetImageStartDelay( Value : Single ) ;
    function GetImageStartDelay : Single ;
    procedure SetDisplayGrid( Value : Boolean ) ;
    function GetDisplayGrid : Boolean ;
    procedure SetCaptureMode ;
    procedure CreateEmptyImageFile ;
    function FindTimingDevice : SmallInt ;
    function CalculatePCStimulusDriveVoltage(
             Min : Single;                             // Minimum power
             Max : Single;                             // Maximum power
             VBias : Single;                           // Pockels cell bias setting
             VPi : Single;                             // VPi constant for Pockels cell
             Amplitude : Single;                       // Desired amplitude in mW
             PolarizationCross : Boolean) : Single ;   // Cross-polarization (sin)
    function CalculateLinearStimulusDriveVoltage(
             Min : Single;                             // Minimum power
             Max : Single;                             // Maximum power
             VMin : Single;                            // Minimum voltage
             VMax : Single;                            // Maximum voltage
             Amplitude : Single) : Single ;            // Desired amplitude in mW
    procedure ToggleVoltageStimulus;  // To address failure of stimulus
                                      // protocol to start on some rigs

    procedure Wait( Delay : Single ) ;
  public
    { Public declarations }
    AutoRecordingMode : Boolean ;
    Ident : String ;               // Expt. notes lines

    procedure StartADC ;
    procedure ImportImageFile( ImportFileName : String ) ;
    procedure UpdateStimProgramList ;
    procedure UpdatePhotoStimProgramList ;
    procedure StartRecordingToDisk(
              RecordingTime : Single ;             // Time to record for (s)
              Num : Integer ;                      // No. if frames/scans to record
              RecordingMode : Integer  ) ;         // Line scan/XY Series recording mode
    procedure StopADC ;

    procedure MagnifyChannelDisplay( ChanNum : Integer ) ;
    procedure ReduceChannelDisplay( ChanNum : Integer ) ;
    procedure ZoomOutAll ;

    Property Interval : Single
             read GetInterval Write SetInterval ;
    Property ImageStartDelay : Single
             read GetImageStartDelay Write SetImageStartDelay ;
    Property DisplayGrid : Boolean read GetDisplayGrid write SetDisplayGrid ;

  end;

var
  RecADCOnlyFrm: TRecADCOnlyFrm;

implementation

uses Main, AmpModule, ViewUnit, SealTest , LogUnit, StimModule, UltimaUnit,
     DynamicProtocolSetupUnit, PhotoStimModule, PlaybackSetupUnit,
     PlaybackStimModule, mmsystem ;

{$R *.dfm}

const
    EmptyFlag = 32766 ;



procedure TRecADCOnlyFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
begin

     Top := 30 ;
     Left := 30 ;

     InitialisationComplete := False ;

     // Set form at top left of MDI window
     Top := 20 ;
     Left := 20 ;

     // Disable A/D input channels if no ADC
     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.ADCIn) then begin
        ShowMessage( 'RECORD: No A/D converter available or configured!' ) ;
        Close ;
        Exit ;
        end
     else begin
        ADCDevice := LabIO.Resource[MainFrm.IOConfig.ADCIn].Device ;
        end ;

     // Customise for LSM type
     if MainFrm.CameraType = BioRad then begin
        Caption := 'BioRad Radiance/1024 Image Capture' ;
        TriggerGrp.Visible := false ;
        ImageCaptureGrp.Height := ImageCaptureGrp.Height - TriggerGrp.Height - 30;
        MarkGrp.Top := ImageCaptureGrp.Top + ImageCaptureGrp.Height - 7;
        ReadoutGrp.Top := MarkGrp.Top + MarkGrp.Height - 7;
        ControlsGrp.Height := ControlsGrp.Height - TriggerGrp.Height;
        ModeGrp.Visible := True ;
        end
     else if MainFrm.CameraType = UltimaLSM then begin
        Caption := 'Prairie Technology Ultima Image Capture' ;
        TriggerGrp.Visible := False ;
        ImageCaptureGrp.Height := ImageCaptureGrp.Height - TriggerGrp.Height
                                - ModeGrp.Height - 30;
        MarkGrp.Top := ImageCaptureGrp.Top + ImageCaptureGrp.Height - 7;
        ReadoutGrp.Top := MarkGrp.Top + MarkGrp.Height - 7;
        ControlsGrp.Height := ControlsGrp.Height - TriggerGrp.Height
                           - ModeGrp.Height - 8;
        ModeGrp.Visible := False ;
        end ;

     ClientHeight := ControlsGrp.Height + ControlsGrp.Top + 10 ;

     // Duration of analogue signals display window
     edTDisplay.Value := MainFrm.ADCDisplayWindow ;

     // Default recording time
     edRecordingTime.Value := MainFrm.ADCRecordingTime ;

     // Experiment ident line
     Ident := '' ;
     edIdent.Text := Ident ;

     // Set dynamic protocol scan counter
     DynamicProtocolCounter := 0 ;

     // Set dyanmic protocol enabled flag
     DynamicProtocolEnabled := False ;

     // Load list of stimulus programs
     UpdateStimProgramList ;

     // Load list of photo-stimulus programs
     UpdatePhotoStimProgramList ;

     // Set holding voltage
     edVHold0.Value := MainFrm.VCommand[0].HoldingVoltage ;
     edVHold1.Value := MainFrm.VCommand[1].HoldingVoltage ;
     edVHold2.Value := MainFrm.VCommand[2].HoldingVoltage ;

     VHold0Panel.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[0]) ;
     VHold1Panel.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[1]) ;
     VHold2Panel.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[2]) ;

     MainFrm.Recording := False ;
     RecordingFirstTrace := False;
     OldTScale := 1.0;
     bRecord.Enabled := True ;
     bStop.Enabled := False ;
     TimerProcBusy := False ;
     ResetDisplays := False ;
     bMark.Enabled := False ;
     cbStimProgram.Enabled := True ;
     VHoldGrp.Enabled := True ;
     SingleStimulusInProgress := False ;
     ckPhotoStimEnabled.Checked := MainFrm.PhotoStim.Enabled ;
     ckDynamicProtocolEnabled.Checked := MainFrm.DynamicProtocol.Enabled ;
     ckPlaybackEnabled.Checked := MainFrm.Playback.Enabled ;

     // Make AD recording choice persistent
     ckRecordADCSignalsOnly.Checked := MainFrm.RecordADCSignalsOnly;

     // Delay between start of A/D sampling and image acquisition
     edImageStartDelay.Value := 0.0 ;

     MainFrm.StatusBar.SimpleText := ' A/D Converter initialised' ;

     // Start acquiring samples
     ADCBuf := Nil ;
     // D/A & dig waveform buffers
     for i := 0 to High(DACBufs) do DACBufs[i] := Nil ;
     for i := 0 to High(DIGBufs) do DIGBufs[i] := Nil ;

     // Initialise A/D displays
     scADCDisplay.MaxADCValue := LabIO.ADCMaxValue[ADCDevice] ;
     scADCDisplay.MinADCValue := -LabIO.ADCMaxValue[ADCDevice]-1 ;
     scADCDisplay.NumChannels := MainFrm.ADCNumChannels ;
     for i := 0 to MainFrm.ADCNumChannels-1 do begin
         scADCDisplay.YMax[i] := MainFrm.ADCChannel[i].YMax ;
         scADCDisplay.YMin[i] := MainFrm.ADCChannel[i].YMin ;
         scADCDisplay.ChanVisible[i] := MainFrm.ADCChannelRecADCOnlyUnitVisible[i] ; // Modified by NS 19 March 2009
         end ;

     scADCDisplay.FixZeroLevels := ckFixZeroLevels.Checked ;

     NewDisplaySetup ;

     // Set display controls for capture mode
     SetCaptureMode ;

     Resize ;

     StartADC ;
     // To address failure of stimulus protocol to start on some rigs
     ToggleVoltageStimulus;

     // Start schedule events timer (runs at 50 ms intervals)
     InitialisationComplete := True ;
     Timer.Enabled := True ;

     end;


procedure TRecADCOnlyFrm.UpdateStimProgramList ;
// --------------------------------
// Update list of stimulus programs
// --------------------------------
var
    NamePart : String ;
begin

     // Load list of stimulus programs
     Stimulator.CreateProgramList( cbStimProgram ) ;
     if MainFrm.StimFileName <> '' then begin
        // Find name within list
        NamePart := AnsiReplaceText(ExtractFileName(MainFrm.StimFileName),'.vpr', '' ) ;
        cbStimProgram.ItemIndex := Min(Max(cbStimProgram.Items.IndexOf(NamePart),
                                   0),cbStimProgram.Items.Count-1) ;
        end
     else begin
        cbStimProgram.ItemIndex := 0 ;
        end ;

     end ;


procedure TRecADCOnlyFrm.UpdatePhotoStimProgramList ;
// --------------------------------------
// Update list of photo-stimulus programs
// --------------------------------------
var
    NamePart : String ;
begin

     // Load list of stimulus programs
     PhotoStimulator.CreateProgramList( cbPhotoStimProgram ) ;
     if MainFrm.PhotoStimFileName <> '' then begin
        // Find name within list
        NamePart := AnsiReplaceText(ExtractFileName(MainFrm.PhotoStimFileName),'.ppr', '' ) ;
        cbPhotoStimProgram.ItemIndex := Min(Max(cbPhotoStimProgram.Items.IndexOf(NamePart),
                                            0),cbPhotoStimProgram.Items.Count-1) ;
        end
     else begin
        cbPhotoStimProgram.ItemIndex := 0 ;
        end ;

     end ;


procedure TRecADCOnlyFrm.StartADC ;
// ---------------------------------------
// Start A/D sampling into circular buffer
// ---------------------------------------
var
     i,j,ch : Integer ;
     tStim : Single ;
     NumScansInWriteBuffer : Integer ;
     DACValue : SmallInt ;
     VHoldDACValue : SmallInt ;
     Device : SmallInt ;
     Dev : Integer ;
     TimingDevice : SmallInt ;
     ExtUpdate : Boolean ;
     WaitForExtTrigger : Boolean ;  // TRUE = Wait for external trigger before starting A/D
     DynamicScans : Integer ;       // Number of scans required for dynamic protocol
     DACNumScansInPlayback : Integer; // Number of scans in playback buffer
begin

     // Stop A/D and D/A if it is running
     for Device := 1 to LabIO.NumDevices do LabIO.StopDAC(Device) ;
     LabIO.StopADC(ADCDevice) ;
     // Wait(0.1) ;

     // Update private variables
     ADCMaxValue := LabIO.ADCMaxValue[ADCDevice] ;
     ADCMinValue := -LabIO.ADCMaxValue[ADCDevice] -1 ;

     // Update channel scaling factors in case amplifier gain has changed
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
         MainFrm.ADCChannel[ch].ChannelOffset := LabIO.GetChannelOffset(ch,MainFrm.ADCNumChannels) ;
         MainFrm.ADCChannel[ch].ADCAmplifierGain := 1.0 ;
         end ;

     // Update channel scaling factors in case amplifier gain has changed
     Amplifier.GetChannelSettings( 0, MainFrm.ADCChannel[0].ADCName,
                                      MainFrm.ADCChannel[0].ADCUnits,
                                      MainFrm.ADCChannel[0].ADCCalibrationFactor,
                                      MainFrm.ADCChannel[0].ADCAmplifierGain ) ;

     Amplifier.GetChannelSettings( 1, MainFrm.ADCChannel[1].ADCName,
                                      MainFrm.ADCChannel[1].ADCUnits,
                                      MainFrm.ADCChannel[1].ADCCalibrationFactor,
                                      MainFrm.ADCChannel[1].ADCAmplifierGain ) ;

     MainFrm.IDRFile.UpdateChannelScalingFactors( MainFrm.ADCChannel,
                                                  MainFrm.ADCNumChannels,
                                                  MainFrm.ADCVoltageRange,
                                                  LabIO.ADCMaxValue[ADCDevice] ) ;

     { Set channel display information }
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
           scADCDisplay.ChanOffsets[ch] := MainFrm.ADCChannel[Ch].ChannelOffset ;
           scADCDisplay.ChanUnits[ch] := MainFrm.ADCChannel[Ch].ADCUnits ;
           scADCDisplay.ChanName[ch] := MainFrm.ADCChannel[Ch].ADCName ;
           scADCDisplay.ChanScale[ch] := MainFrm.ADCChannel[ch].ADCScale ;
           end ;

     // Determine size and number of A/D write buffers within circular ADC buffer
     ADCNumSamplesInWriteBuffer := Max(
                                   Round(WriteBufferDuration/
                                         MainFrm.ADCScanInterval)*MainFrm.ADCNumChannels,
                                   MinWriteBufferScans*MainFrm.ADCNumChannels ) ;
     ADCNumSamplesInBuffer := ADCNumWriteBuffers*ADCNumSamplesInWriteBuffer ;
     NumScansInWriteBuffer := ADCNumSamplesInWriteBuffer div MainFrm.ADCNumChannels ;
     ADCNumScansInBuffer := ADCNumWriteBuffers*NumScansInWriteBuffer ;

     // Allocate A/D ring buffer
     if ADCBuf <> Nil then FreeMem(ADCBuf) ;
     GetMem(ADCBuf, ADCNumSamplesInBuffer*2 ) ;

     // Fill circular buffer with empty flags
     i := 0 ;
     while i < ADCNumSamplesInBuffer do begin
         ADCBuf^[i] := EmptyFlag ;
         Inc(i) ;
         ADCBuf^[i] := -EmptyFlag ;
         Inc(i) ;
         end ;

     // Set clear stimulus time to very high value
     ClearStimulusAfterScanNum := High(ClearStimulusAfterScanNum) ;
     SingleStimulusInProgress := False ;
     DACNumScansInBuffer := 512 ;
     DynamicScans := 512 ;

     if StimulusRequired then begin

        // Load dyanmic ephys protocol first in order to compare which buffer is bigger
        if (ckDynamicProtocolEnabled.Checked) then begin
          if (MainFrm.DynamicProtocol.EPStimFileName <> '') and (MainFrm.DynamicProtocol.EPStimIndex > 0) then begin
             // Load program
             Stimulator.LoadProgram( MainFrm.DynamicProtocol.EPStimFileName ) ;
             // Set buffer size to stimulus period
             DynamicScans := Round(Stimulator.ProtocolDuration/MainFrm.ADCScanInterval) ;
             if Stimulator.Prog.NumRepeats <= 1 then begin
              ClearStimulusAfterScanNum := DynamicScans ;
              DynamicScans := DynamicScans + Round(1.0/MainFrm.ADCScanInterval) ;
              SingleStimulusInProgress := True ;
              end ;
          end ;
        end ;

        // Analog/digital stimulus outputs
        if (cbStimProgram.ItemIndex > 0)  and (MainFrm.Playback.Enabled = False) then begin
           // Load program
           Stimulator.LoadProgram( MainFrm.StimFileName ) ;
           // Set buffer size to stimulus period
           DACNumScansInBuffer := Round(Stimulator.ProtocolDuration/MainFrm.ADCScanInterval) ;
           // Increase buffer size for single stimulus (to allow erasure of stimulus)
           if Stimulator.Prog.NumRepeats <= 1 then begin
              ClearStimulusAfterScanNum := DACNumScansInBuffer ;
              DACNumScansInBuffer := DACNumScansInBuffer + Round((LabIO.InternalBufferDuration+0.5)/MainFrm.ADCScanInterval) ;
              SingleStimulusInProgress := True ;
              end ;
           end ;

        // Playback waveform buffer size
        if (MainFrm.Playback.Enabled) then
        begin
          DACNumScansInBuffer := Round(PlaybackStimulator.ProtocolDuration /
                                       MainFrm.ADCScanInterval);
          ClearStimulusAfterScanNum := DACNumScansInBuffer;
          DACNumScansInBuffer := DACNumScansInBuffer + Round(1.0/MainFrm.ADCScanInterval);
          SingleStimulusInProgress := True;
        end;

        // Set number of scans in buffer to greater of first protocol or dynamic protocol
        DACNumScansInBuffer := Max(DynamicScans, DACNumScansInBuffer) ;
        DACNumScansInPlayback := DACNumScansInBuffer;

        // Photo stimulus
        DynamicScans := 512;
        if ckPhotoStimEnabled.Checked then begin

          if (ckDynamicProtocolEnabled.Checked) then begin
            if (MainFrm.DynamicProtocol.PSStimFileName <> '') and (MainFrm.DynamicProtocol.PSStimIndex > 0) then begin
            PhotoStimulator.LoadProgram(MainFrm.DynamicProtocol.PSStimFileName);
            if MainFrm.PhotoStim.RepeatedStim then begin
              // Repeated stimulus (photo-stim repeat period overrides voltage stim)
              //DynamicScans := Round(PhotoStimulator.ProtocolDuration/MainFrm.ADCScanInterval) ;
              DynamicScans := Round((PhotoStimulator.ProtocolDuration+(MainFrm.PhotoStim.Period-PhotoStimulator.ProtocolDuration))/MainFrm.ADCScanInterval) ;
            end
            else begin
             DynamicScans := Max(DACNumScansInBuffer,
                                        Round(PhotoStimulator.ProtocolDuration/MainFrm.ADCScanInterval)) ;
             ClearStimulusAfterScanNum := DynamicScans ;
             DynamicScans := DynamicScans + Round(1.0/MainFrm.ADCScanInterval) ;
             SingleStimulusInProgress := True ;
            end;
            end;
          end;

          if cbPhotoStimProgram.ItemIndex > 0 then begin
          PhotoStimulator.LoadProgram(MainFrm.PhotoStimFileName);
          if MainFrm.PhotoStim.RepeatedStim then begin
              // Repeated stimulus (photo-stim repeat period overrides voltage stim)
             //DACNumScansInBuffer := Round((PhotoStimulator.ProtocolDuration)/MainFrm.ADCScanInterval) ;
             DACNumScansInBuffer := Round((PhotoStimulator.ProtocolDuration+(MainFrm.PhotoStim.Period-PhotoStimulator.ProtocolDuration))/MainFrm.ADCScanInterval) ;
             end
          else begin

             // If photo-stimulus and playback enabled
             if ckPlaybackEnabled.Enabled then
             begin

               // Size of buffer including photo-stimulus offset
               DACNumScansInBuffer := Round((MainFrm.Playback.StartPS-MainFrm.Playback.Start)/MainFrm.ADCScanInterval) + Round(PhotoStimulator.ProtocolDuration/MainFrm.ADCScanInterval);
               DACNumScansInBuffer := Max(DACNumScansInBuffer, DACNumScansInPlayback);
               ClearStimulusAfterScanNum := DACNumScansInBuffer ;
               DACNumScansInBuffer := DACNumScansInBuffer + Round(1.0/MainFrm.ADCScanInterval) ;
               SingleStimulusInProgress := True ;

             end

             // Only photo-stimulus enabled
             else
             begin
               DACNumScansInBuffer := Max(DACNumScansInBuffer,
                                          Round(PhotoStimulator.ProtocolDuration/MainFrm.ADCScanInterval)) ;
               ClearStimulusAfterScanNum := DACNumScansInBuffer ;
               DACNumScansInBuffer := DACNumScansInBuffer + Round(1.0/MainFrm.ADCScanInterval) ;
               SingleStimulusInProgress := True ;
             end;

          end;
          end;
          // Set number of scans in buffer to greater of first protocol or dynamic protocol
          DACNumScansInBuffer := Max(DynamicScans, DACNumScansInBuffer) ;
        end;

        // Disable changes to stimulus & holding voltages
        bStopStimulus.Enabled := True ;
        bStartStimulus.Enabled := False ;
        cbStimProgram.Enabled := False ;
        VHoldGrp.Enabled := False ;

        end
     else DACNumScansInBuffer := 512 ;

     // --- End if StimulusRequired ---

     // Ensure DAC buffer is multiple of frame acquisition interval
     // when in frame trigger mode
     if rbFrameTrigger.Checked and MainFrm.Recording then begin
        DACNumScansPerFrame := Max(Round(edInterval.Value/MainFrm.ADCScanInterval),2) ;
        DACNumFramesPerBuf := Max(DACNumScansInBuffer div DACNumScansPerFrame,2) ;
        DACNumScansInBuffer := DACNumScansPerFrame*DACNumFramesPerBuf ;
        end
     else begin
        DACNumScansPerFrame := DACNumScansInBuffer ;
        DACNumFramesPerBuf := 1 ;
        end ;

     // Allocate D/A & digital waveform buffers
     for Device := 1 to LabIO.NumDevices do begin
         if DACBufs[Device] <> Nil then FreeMem(DACBufs[Device]) ;
         if DIGBufs[Device] <> Nil then FreeMem(DIGBufs[Device]) ;
         DACBufs[Device] := Nil ;
         DIGBufs[Device] := Nil ;
         if LabIO.NumDACs[Device] > 0 then begin
            GetMem( DACBufs[Device],DACNumScansInBuffer*LabIO.NumDACs[Device]*2 ) ;
            end ;
         GetMem( DIGBufs[Device],DACNumScansInBuffer*4 ) ;
         end ;

     // Fill DAC & DIG buffer with default values
     for Device := 1 to LabIO.NumDevices do begin
         DeviceInUse[Device] := False ;
         LabIO.FillDACBufWithDefaultValues( Device, DACBufs[Device],DACNumScansInBuffer ) ;
         LabIO.FillDIGBufWithDefaultValues( Device, DIGBufs[Device],DACNumScansInBuffer ) ;
         end ;

     // Update exposure trigger timing waveform
     UpdateCameraStartWaveform ;

     // Update command voltages for ananlog and digital outputs
     if (MainFrm.Playback.Enabled) then
     begin

        // Update comand voltage and digital stimulus waveform
        UpdateStimulusWaveforms( false, true ) ;

        // Update command voltage for playback waveform
        UpdatePlaybackWaveform( StimulusRequired );

     end
     else
     begin

        // Update comand voltage and digital stimulus waveform
        UpdateStimulusWaveforms( StimulusRequired, true ) ;

     end;

     // Update photo stimulus waveform
     UpdatePhotoStimulus( StimulusRequired and ckPhotoStimEnabled.Checked ) ;

     // Start A/D sampling
     LabIO.ADCToMemoryExtScan( ADCDevice,
                               ADCBuf^,
                               MainFrm.ADCNumChannels,
                               ADCNumSamplesInBuffer div MainFrm.ADCNumChannels,
                               MainFrm.ADCVoltageRange,
                               True,
                               FindTimingDevice,
                               True ) ;

     // Initialise circular A/D and display buffers
     ADCPointer := MainFrm.ADCNumChannels ;
     ADCActiveBuffer := 0 ;
     ADCWriteBuffer := 0 ;
     ADCDisplayPointer := ADCPointer ;

     // Ignore 1st 4 channel groups in buffer
     ADCEmptyPointer := MainFrm.ADCNumChannels*4 ;
     ADCOldestScan := ADCEmptyPointer - MainFrm.ADCNumChannels*4 ;

     ADCNumScansCumulative := 0 ;

     // Erase display
     scADCDisplay.NumPoints := 0 ;
     scADCDisplay.Invalidate ;

     // Clear status panels
     MainFrm.StatusBar.SimpleText := '' ;


     if (MainFrm.CameraType = UltimaLSM) and MainFrm.Recording and
        (not ckRecordADCSignalsOnly.Checked) then begin
        // Ultima LSM
        // Wait for external trigger (active-high on PFI0) to synchronise
        // A/D recording with start of line scan
        WaitForExtTrigger := True ;
        end
     else WaitForExtTrigger := False ;

     // Start D/A waveform output
     for Device := 1 to LabIO.NumDevices do begin

         // Set up digital output sweep
         if DeviceDIGInUse[Device] then begin
            LabIO.MemoryToDIG( Device,
                               DIGBufs[Device]^,
                               DACNumScansInBuffer,
                               MainFrm.ADCScanInterval,
                               True,
                               WaitForExtTrigger,
                               FindTimingDevice,
                               True ) ;
            end ;

         // Set up D/A output sweep
         if DeviceInUse[Device] then begin
            LabIO.MemoryToDAC( Device,
                               DACBufs[Device]^,
                               LabIO.NumDACs[Device],
                               DACNumScansInBuffer,
                               MainFrm.ADCScanInterval,
                               True,
                               WaitForExtTrigger,
                               FindTimingDevice ) ;
            end ;

         end ;

     ADCRunning := True ;

     end ;


function TRecADCOnlyFrm.FindTimingDevice : Smallint ;
// ----------------------------------------------
// Find NI device to use as ADC/DAC timing source
// ----------------------------------------------
var
    Dev : SmallInt ;
begin
     // Use last DAC device in use as timing device
     Result := 0 ;
     for Dev := 1 to LabIO.NumDevices do if DeviceInUse[Dev] then Result := Dev ;
     end ;


procedure TRecADCOnlyFrm.NewDisplaySetup ;
// --------------------------
// Set up A/D signals display
// --------------------------
begin

    // Set up A/D signals display window

    // No. of points in A/D window
    MainFrm.ADCDisplayWindow := edTDisplay.Value ;
    edTDisplay.Value := MainFrm.ADCDisplayWindow ;

    edNumMarkers.text := format('%d',[MaxMarker-MainFrm.IDRFile.NumMarkers]);

    end ;


function TRecADCOnlyFrm.CalculatePCStimulusDriveVoltage(
         Min : Single;                                  // Minimum power
         Max : Single;                                  // Maximum power
         VBias : Single;                                // Pockels cell bias setting
         VPi : Single;                                  // VPi constant for Pockels cell
         Amplitude : Single;                            // Desired amplitude in mW
         PolarizationCross : Boolean) : Single ;        // Cross-polarization (sin)
// -------------------------------------------------------------------
// Calculate appropriate drive voltage for photo-stimulus Pockels cell
// This is very much the same as PowerCal
// Modified by NS 14 October 2008
// -------------------------------------------------------------------
var
  Modulation : Single;
begin

  // Determine the term in the square root below
  if Max <> Min then Modulation := (Amplitude - Min) / (Max - Min)
                else Modulation := 0.0 ;

  // Assign minimum power if the power is set too low
  if Modulation < 0.0 then
    Modulation := 0.0;

  // Calculate the final drive voltage
  if PolarizationCross then
    Result := (Vpi * (2/Pi) * ArcSin(sqrt(Modulation))) + ((Pi/2) * (VBias / 375.0));
  if Not PolarizationCross then
    Result := (Vpi * (2/Pi) * ArcCos(sqrt(Modulation))) + ((Pi/2) * (VBias / 375.0));

end;


function TRecADCOnlyFrm.CalculateLinearStimulusDriveVoltage(
         Min : Single;                             // Minimum power
         Max : Single;                             // Maximum power
         VMin : Single;                            // Minimum voltage
         VMax : Single;                            // Maximum voltage
         Amplitude : Single) : Single ;            // Desired amplitude in mW
// -------------------------------------------------------------------
// Calculate appropriate drive voltage for a linear ramp
// -------------------------------------------------------------------
begin
end;


procedure TRecADCOnlyFrm.UpdateCameraStartWaveform ;
// ----------------------------------------
// Update camera start timing pulse pattern
// ----------------------------------------
begin

     // No exposure trigger pulse when not recording
     if not MainFrm.Recording then Exit ;

     // Exit if no D/A channel configured
     if (MainFrm.IOConfig.CameraStart < 0) or
        (MainFrm.IOConfig.CameraStart > MaxResources) then Exit ;

     if LabIO.Resource[MainFrm.IOConfig.CameraStart].ResourceType = DACOut then
        UpdateCameraStartWaveformDAC
     else UpdateCameraStartWaveformDIG ;

     end ;


procedure TRecADCOnlyFrm.UpdateCameraStartWaveformDAC ;
// ----------------------------------------
// Update camera start timing pulse pattern (DAC O/P)
// ----------------------------------------
const
    TriggerPulseDuration = 0.001 ; // 1ms trigger pulse
var
     Device : Integer ;
     OnState : SmallInt ;
     OffState : SmallInt ;
     DACChannel : Integer ;
     NumDACChannels : Integer ;
     i,j,iPulse : Integer ;
     DACScale : Single ;
     NumPointsPerPulse : Integer ;
begin

     // Create camera exposure start pattern
     Device :=  LabIO.Resource[MainFrm.IOConfig.CameraStart].Device ;
     if DACBufs[Device] = Nil Then Exit ;
     DeviceInUse[Device] := True ;

     DACChannel := LabIO.Resource[MainFrm.IOConfig.CameraStart].StartChannel ;
     NumDACChannels := LabIO.NumDACs[Device] ;

     // D/A scaling factor
     DACScale := LabIO.DACMaxValue[Device]/LabIO.DACMaxVolts[Device] ;

     if rbTriggerTTLHigh.Checked then begin
        // 5V Active High trigger
        OnState := Round(4.9*DACScale) ;
        OffState := 0 ;
        end
     else begin
        // Active Low (0V) trigger
        OffState := Round(4.9*DACScale) ;
        OnState := 0 ;
        end ;

     // Clear channel
        j := DACChannel ;
        for i := 0 to DACNumScansInBuffer-1 do begin
            DACBufs[Device]^[j] := OffState ;
            j := j + NumDACChannels ;
            end ;

     NumPointsPerPulse := Max(Round(TriggerPulseDuration/MainFrm.ADCScanInterval),1) ;

     if rbSequenceTrigger.Checked then begin
        // Trigger pulses starts capture of series of frames
        j := DACChannel ;
        for i := 1 to NumPointsPerPulse do begin
            DACBufs[Device]^[j] := OnState ;
            j := j + NumDACChannels ;
            end ;
        end
     else begin
        // Trigger pulse starts each frame
        for iPulse := 0 to DACNumFramesPerBuf-1 do begin
            j := iPulse*DACNumScansPerFrame + DACChannel ;
            for i := 1 to NumPointsPerPulse do begin
                DACBufs[Device]^[j] := OnState ;
                j := j + NumDACChannels ;
                end ;
            end ;
        end ;

     // Update default values for DAC channel
     LabIO.DACOutState[Device][DACChannel] := OffState / DACScale ;

     end ;


procedure TRecADCOnlyFrm.UpdateCameraStartWaveformDIG ;
// ------------------------------------------------------
// Update camera start timing pulse pattern (digital O/P)
// ------------------------------------------------------
const
    TriggerPulseDuration = 0.001 ; // 1ms trigger pulse
var
     Device : Integer ;
     OnState : SmallInt ;
     OffState : SmallInt ;
     DACChannel : Integer ;
     NumDACChannels : Integer ;
     i,j,iPulse : Integer ;
     DACScale : Single ;
     NumPointsPerPulse : Integer ;
     BitMask : Byte ;

begin

    // Create camera exposure start pattern
    Device :=  LabIO.Resource[MainFrm.IOConfig.CameraStart].Device ;
    if DIGBufs[Device] = Nil Then Exit ;
    DeviceDIGInUse[Device] := True ;

    // Bit mask to isolate bit within digital O/P byte
    BitMask := not LabIO.BitMask(LabIO.Resource[MainFrm.IOConfig.CameraStart].StartChannel) ;

    if rbTriggerTTLHigh.Checked then begin
       // 5V Active High trigger
       OnState := LabIO.BitMask(LabIO.Resource[MainFrm.IOConfig.CameraStart].StartChannel) ;
       OffState := 0 ;
       end
    else begin
       // Active Low (0V) trigger
       OffState := LabIO.BitMask(LabIO.Resource[MainFrm.IOConfig.CameraStart].StartChannel) ;
       OnState := 0 ;
       end ;

     // Clear output bit to off-state
     for i := 0 to DACNumScansInBuffer-1 do begin
         DIGBufs[Device]^[i] := (DIGBufs[Device]^[i] and BitMask) or OffState ;
         end ;

     NumPointsPerPulse := Max(Round(TriggerPulseDuration/MainFrm.ADCScanInterval),1) ;

     if rbSequenceTrigger.Checked then begin
        // Trigger pulses starts capture of series of frames
        for i := 0 to NumPointsPerPulse-1 do begin
            DIGBufs[Device]^[i] := (DIGBufs[Device]^[i] and BitMask) or OnState ;
            end ;
        end
     else begin
        // Trigger pulse starts each frame
        for iPulse := 0 to DACNumFramesPerBuf-1 do begin
            j := iPulse*DACNumScansPerFrame ;
            for i := 0 to NumPointsPerPulse-1 do begin
                DIGBufs[Device]^[i+j] := (DIGBufs[Device]^[i+j] and BitMask) or OnState ;
                end ;
            end ;
        end ;

     // Update default values for digital O/P
     LabIO.DigOutState[Device] := (LabIO.DigOutState[Device] and BitMask) or OffState ;

     end ;


procedure TRecADCOnlyFrm.UpdatePlaybackWaveform(StimulusEnabled: Boolean);
// -----------------------------------------------------------------------------
// Create analogue & digital D/A output waveform for patch clamp playback
// -----------------------------------------------------------------------------
var
    Device : Integer ;          // Hardware device #
    VChan : Integer ;           // Voltage O/P channel #
    DACChannel : Integer ;      // DAC output channel #
    DACNumChannels : Integer ;  // No. of DAC channels on current device
    VHoldDACValue : Integer ;   // DAC default holding value
    DStimCh : Integer ;         // Digital stimulus pulse bit
    DChan : Integer ;           // Digital output bit
    DStartChan : Integer ;
    DNumChans : Integer ;
    DACLevel : Integer ;
    i,j : Integer ;
    DACScale : Single ;
begin

  // Set default voltage stimulus outputs
  for VChan := 0 to NumCommandVoltageChannels-1 do
  begin

    // Skip if channel is not configured
    if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[VChan]) then
      Continue;

    // Get I/O device properties (skip if not set up)
    Device := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device;
    if DACBufs[Device] = Nil then Continue;
    DeviceInUse[Device] := True;

    // DAC output channel and no. of channels on device
    DACChannel := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].StartChannel;
    DACNumChannels := LabIO.NumDACs[Device];

    // D/A scaling factor
    DACScale := (LabIO.DACScale[Device]*MainFrm.VCommand[VChan].DivideFactor);

    // New holding voltage
    if VChan = 0 then MainFrm.VCommand[VChan].HoldingVoltage := edVHold0.Value
    else if VChan = 1 then MainFrm.VCommand[VChan].HoldingVoltage := edVHold1.Value
    else if VChan = 2 then MainFrm.VCommand[VChan].HoldingVoltage := edVHold2.Value;

    // Initialise buffer to holding potential
    VHoldDACValue := Min(Max(Round(MainFrm.VCommand[VChan].HoldingVoltage*DACScale),
                         LabIO.DACMinValue[Device]),LabIO.DACMaxValue[Device]);
    j := DACChannel;
    for i := 0 to DACNumScansInBuffer-1 do begin
      DACBufs[Device]^[j] := VHoldDACValue;
      j := j + DACNumChannels ;
    end;

    // Set default DAC channel value
    LabIO.DACOutState[Device,DACChannel] := VHoldDACValue / DACSCale;

  end;

  // Set default digital stimulus outputs
  for DStimCh := 0 to NumDigitalStimulusChannels-1 do begin

    // Skip if channel is not configured
    if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) then
      Continue;
    if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then
      Continue;

    // Get I/O device properties (skip if not set up)
    Device := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device;
    if DigBufs[Device] = Nil then Continue;
    DeviceDigInUse[Device] := True;

    // Get starting output bit and number of bits available
    DStartChan := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel;
    DNumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel - DStartChan + 1;

    if DStimCh >= DNumChans then Continue;

    // Update DAC output buffer (if a DAC channel is being used for digital output)
    if LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].ResourceType = DACOut then begin

      // No. of DAC channels
      DACNumChannels := LabIO.NumDACs[Device];
      DChan := DStimCh + DStartChan;
      if DChan >= DACNumChannels then Break;

      // Calculate DAC output level value (5V or 0V)
      if (LabIO.DigOutState[Device] and LabIO.BitMask(DChan)) <> 0 then begin
        DACLevel := Round((4.99*LabIO.DACMaxValue[Device])/LabIO.DACMaxVolts[Device]);
      end
      else DACLevel := 0;

      // Update DAC buffer
      j := DChan;
      PDACBuf := DACBufs[Device];
      for i := 0 to DACNumScansInBuffer-1 do begin
        PDACBuf^[j] := DACLevel;
        j := j + DACNumChannels;
      end;

      // Set default DAC channel value
      LabIO.DACOutState[Device,DChan] := DACLevel/LabIO.DACSCale[Device];

    end;

  end;


  // Create playback stimulus protocol if required
  if StimulusEnabled and MainFrm.Playback.Enabled then
  begin

    // Change this
    VChan := MainFrm.Playback.VOutChannel;

    // Get I/O device properties (skip if not set up)
    Device := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device;
    if DACBufs[Device] = Nil then Exit;
    DeviceInUse[Device] := True;

    // DAC output channel and no. of channels on device
    DACChannel := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].StartChannel;
    DACNumChannels := LabIO.NumDACs[Device];

    // Create waveform for playback
    PlaybackStimulator.CreateWaveform(DACBufs,
                                      DACChannel,
                                      VChan,
                                      DACNumScansInBuffer,
                                      Device,
                                      StimulusEnabled,
                                      DACNumChannels);

  end;

end;


procedure TRecADCOnlyFrm.UpdateStimulusWaveforms(
          StimulusEnabled : Boolean ;  // TRUE = Generate stimulus
          InitialiseBuffer : Boolean   // TRUE = initialise LABIO.DAC
          )   ;
// -----------------------------------------------------------------------------
// Create analogue & digital D/A output waveform for patch clamp command voltage
// -----------------------------------------------------------------------------
var
    Device : Integer ;          // Hardware device #
    VChan : Integer ;           // Voltage O/P channel #
    DACChannel : Integer ;      // DAC output channel #
    DACNumChannels : Integer ;  // No. of DAC channels on current device
    VHoldDACValue : Integer ;   // DAC default holding value
    DStimCh : Integer ;         // Digital stimulus pulse bit
    DChan : Integer ;           // Digital output bit
    DStartChan : Integer ;
    DNumChans : Integer ;
    DACLevel : Integer ;
    i,j : Integer ;
    DACScale : Single ;
begin

     // Set default voltage stimulus outputs
     for VChan := 0 to NumCommandVoltageChannels-1 do begin

         // Skip if channel is not configured
         if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[VChan]) then Continue ;

         // Get I/O device properties (skip if not set up)
         Device := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device ;
         if DACBufs[Device] = Nil then Continue ;
         DeviceInUse[Device] := True ;

         // DAC output channel and no. of channels on device
         DACChannel := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].StartChannel ;
         DACNumChannels := LabIO.NumDACs[Device] ;

         // D/A scaling factor
         DACScale := (LabIO.DACScale[Device]*MainFrm.VCommand[VChan].DivideFactor) ;

        // New holding voltage
        if VChan = 0 then MainFrm.VCommand[VChan].HoldingVoltage := edVHold0.Value
        else if VChan = 1 then MainFrm.VCommand[VChan].HoldingVoltage := edVHold1.Value
        else if VChan = 2 then MainFrm.VCommand[VChan].HoldingVoltage := edVHold2.Value ;

         // Initialise buffer to holding potential
         VHoldDACValue := Min( Max( Round(MainFrm.VCommand[VChan].HoldingVoltage*DACScale),
                                    LabIO.DACMinValue[Device]),LabIO.DACMaxValue[Device]) ;
         j := DACChannel ;
         for i := 0 to DACNumScansInBuffer-1 do begin
             DACBufs[Device]^[j] := VHoldDACValue ;
             j := j + DACNumChannels ;
             end ;

         // Set default DAC channel value
         LabIO.DACOutState[Device,DACChannel] := VHoldDACValue/DACSCale ;

         end ;

     // Set default digital stimulus outputs

     for DStimCh := 0 to NumDigitalStimulusChannels-1 do begin

         // Skip if channel is not configured
         if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) then Continue ;
         if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then Continue ;

         // Get I/O device properties (skip if not set up)
         Device := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device ;
         if DigBufs[Device] = Nil then Continue ;
         DeviceDigInUse[Device] := True ;

         // Get starting output bit and number of bits available
         DStartChan := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel ;
         DNumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel
                     -DStartChan + 1 ;

         if DStimCh >= DNumChans then Continue ;

         // Update DAC output buffer (if a DAC channel is being used for digital output)
         if LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].ResourceType = DACOut then begin

            // No. of DAC channels
            DACNumChannels := LabIO.NumDACs[Device] ;
            DChan := DStimCh + DStartChan ;
            if DChan >= DACNumChannels then Break ;

            // Calculate DAC output level value (5V or 0V)
            if (LabIO.DigOutState[Device] and LabIO.BitMask(DChan)) <> 0 then begin
               DACLevel := Round((4.99*LabIO.DACMaxValue[Device])/LabIO.DACMaxVolts[Device]) ;
               end
            else DACLevel := 0 ;

            // Update DAC buffer
            j := DChan ;
            PDACBuf := DACBufs[Device] ;
            for i := 0 to DACNumScansInBuffer-1 do begin
                PDACBuf^[j] := DACLevel ;
                j := j + DACNumChannels ;
                end ;

            // Set default DAC channel value
            LabIO.DACOutState[Device,DChan] := DACLevel/LabIO.DACSCale[Device] ;

            end ;

         end ;

     // Create stimulus if required
     if  StimulusEnabled and (cbStimProgram.ItemIndex > 0) then
         Stimulator.CreateWaveform( DACBufs,DigBufs,DACNumScansInBuffer, InitialiseBuffer ) ;

     end ;


procedure TRecADCOnlyFrm.UpdatePhotoStimulus(StimulusEnabled : Boolean);
// ----------------------------------------------------------
// Create photo stimulus X/Y galvo control and intensity waveforms
// ----------------------------------------------------------
var
  Device : Integer;                  // Hardware device number
  DACNumChannels : Integer;          // Number of DAC channels on current device
  DACX, DACY, DACI, DACS : Integer;  // Control channels
  DACScale : Single;                 // DAC scale factor
  Offset : Integer;                  // Number of samples to offset protocol
begin

  // Skip if control channels are not configured
  if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimX) then
    Exit;
  if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimY) then
    Exit;
  if ((not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI1)) and
      (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI2)) and
      (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI3))) then
    Exit;
  if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimShutter) then
    Exit;

  // Get I/O device properties (skip if not set up)
  Device := LabIO.Resource[MainFrm.IOConfig.PhotoStimX].Device;
  if DACBufs[Device] = Nil then Exit;
  DeviceInUse[Device] := True;

  // Get DAC output channels
  DACX := LabIO.Resource[MainFrm.IOConfig.PhotoStimX].StartChannel;
  DACY := LabIO.Resource[MainFrm.IOConfig.PhotoStimY].StartChannel;
  if MainFrm.PhotoStim.Attenuator = 1 then
    DACI := LabIO.Resource[MainFrm.IOConfig.PhotoStimI1].StartChannel;
  if MainFrm.PhotoStim.Attenuator = 2 then
    DACI := LabIO.Resource[MainFrm.IOConfig.PhotoStimI2].StartChannel;
  if MainFrm.PhotoStim.Attenuator = 3 then
    DACI := LabIO.Resource[MainFrm.IOConfig.PhotoStimI3].StartChannel;
  DACS := LabIO.Resource[MainFrm.IOConfig.PhotoStimShutter].StartChannel;

  // Number of channels on device
  DACNumChannels := LabIO.NumDACs[Device];

  // D/A scaling factor
  DACScale := LabIO.DACMaxValue[Device] / LabIO.DACMaxVolts[Device];

  // Calcualte sample offset when protocol playback is enabled
  if ckPlaybackEnabled.Checked then
    Offset :=
      Round((MainFrm.Playback.StartPS-MainFrm.Playback.Start)/MainFrm.ADCScanInterval)
  else
    Offset := 0;

  // Use PhotoStimulator at this point...
  PhotoStimulator.CreateWaveform(DACBufs,
                                 DACX, DACY, DACI, DACS,
                                 DACScale,
                                 DACNumScansInBuffer,
                                 Device,
                                 StimulusEnabled,
                                 DACNumChannels,
                                 Offset);

end;


procedure TRecADCOnlyFrm.UpdateHoldingVoltage ;
// ----------------------
// Update holding voltage
// ----------------------
var
    DACScale : Single ;
    OldDACValue : SmallInt ;
    NewDACValue : SmallInt ;
    Device : SmallInt ;
    DACChannel : Integer ;
    DACNumChannels : Integer ;
    VHoldDACValue : Integer ;
    i,j : Integer ;
    VChan : Integer ;
begin

     for VChan := 0 to NumCommandVoltageChannels-1 do begin

        // Exit if no D/A channel configured
        if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[VChan]) then Continue ;

        Device := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device ;
        if DACBufs[Device] = Nil Then Continue ;

        DACChannel := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].StartChannel ;
        DACNumChannels := LabIO.NumDACs[Device] ;

        // Set D/A voltage->integer scaling factor
        DACScale := (LabIO.DACMaxValue[Device]*MainFrm.VCommand[VChan].DivideFactor)/
                     LabIO.DACMaxVolts[Device] ;

        // Old holding voltage
        OldDACValue := Round(MainFrm.VCommand[VChan].HoldingVoltage*DACScale) ;

        // New holding voltage
        if VChan = 0 then MainFrm.VCommand[VChan].HoldingVoltage := edVHold0.Value
        else if VChan = 1 then MainFrm.VCommand[VChan].HoldingVoltage := edVHold1.Value
        else if VChan = 2 then MainFrm.VCommand[VChan].HoldingVoltage := edVHold2.Value ;
        NewDACValue := Round(MainFrm.VCommand[VChan].HoldingVoltage*DACScale) ;

        j := DACChannel ;
        for i := 0 to DACNumScansInBuffer-1 do begin
            DACBufs[Device]^[j] := DACBufs[Device]^[j] - OldDACValue + NewDACValue ;
            j := j + DACNumChannels ;
            end ;

        end ;

     // A/D restart needed with NIDAQ-MX to force DAC updates
     //if LABIO.NIDAQAPI = NIDAQMX then StartADC ;

     end ;


procedure TRecADCOnlyFrm.UpdateADCDisplay ;
// ------------------------------
// Update analogue inputs display
// ------------------------------
var
     i,MaxDisplayPoints : Integer ;
     NumBytesWritten : Integer ;     // No. bytes written to file
     BufStart : Integer ;
     NumScans : Integer ;
     NumSamplesPerBlock : Integer ;
     nCount : Integer ;
     ch : Integer ;
     Done : Boolean ;
     y : Integer ;
     s : String ;
     DynamicProtocolCh : Integer ;  // Dynamic protocol channel
     DACNumScansInBuffer : Integer ;
     Device : Integer ;
     WaitForExtTrigger : Boolean ;
begin

     if (not ADCRunning) or (not InitialisationComplete) then Exit ;

     // Erase display and initialise live display counters
     // --------------------------------------------------
     if ResetDisplays or (ADCNumBlocksDisplayed >= ADCMaxBlocksDisplayed) then begin

       // Set up A/D signals display window
       scADCDisplay.MaxADCValue := LabIO.ADCMaxValue[ADCDevice] ;
       scADCDisplay.MinADCValue := -LabIO.ADCMaxValue[ADCDevice] -1 ;

       scADCDisplay.NumChannels := MainFrm.ADCNumChannels ;

       // Add zero level cursors
       scADCDisplay.ClearHorizontalCursors ;
       for ch :=  0 to scADCDisplay.NumChannels-1 do
           scADCDisplay.AddHorizontalCursor( ch, clBlue, True, 'z' ) ;

       // Update channel scale factors
       for ch := 0 to MainFrm.ADCNumChannels-1 do begin
          Amplifier.GetChannelSettings( ch, MainFrm.ADCChannel[ch].ADCName,
                                           MainFrm.ADCChannel[ch].ADCUnits,
                                           MainFrm.ADCChannel[ch].ADCCalibrationFactor,
                                           MainFrm.ADCChannel[ch].ADCAmplifierGain ) ;
          end ;

       MainFrm.IDRFile.UpdateChannelScalingFactors( MainFrm.ADCChannel,
                                                       MainFrm.ADCNumChannels,
                                                       MainFrm.ADCVoltageRange,
                                                       LabIO.ADCMaxValue[ADCDevice] ) ;

       { Set channel information }
       for ch := 0 to MainFrm.ADCNumChannels-1 do begin
           scADCDisplay.ChanOffsets[ch] := MainFrm.ADCChannel[Ch].ChannelOffset ;
           scADCDisplay.ChanUnits[ch] := MainFrm.ADCChannel[Ch].ADCUnits ;
           scADCDisplay.ChanName[ch] := MainFrm.ADCChannel[Ch].ADCName ;
           scADCDisplay.ChanScale[ch] := MainFrm.ADCChannel[ch].ADCScale ;
           scADCDisplay.yMin[ch] := MainFrm.ADCChannel[Ch].yMin ;
           scADCDisplay.yMax[ch] := MainFrm.ADCChannel[Ch].yMax ;
           scADCDisplay.HorizontalCursors[ch] := MainFrm.ADCChannel[Ch].ADCZero ;
           scADCDisplay.ChanVisible[ch] := MainFrm.ADCChannel[ch].InUse ;
           end ;

       // No. of multi-channel scans to be displayed
       NumScans := Max( Round(edTDisplay.Value/MainFrm.ADCScanInterval),2 ) ;
       edTDisplay.Value := NumScans*MainFrm.ADCScanInterval ;

       // Size of display compression block
       ADCNumScansPerBlock := Max( NumScans div MaxDisplayScans,1) ;
       NumSamplesPerBlock := ADCNumScansPerBlock*MainFrm.ADCNumChannels ;

       // No. of displayed points per block
       ADCNumPointsPerBlock := Min(ADCNumScansPerBlock,2) ;

       // Max. number of points in display
       ADCMaxBlocksDisplayed := (NumScans div ADCNumScansPerBlock) ;
       scADCDisplay.MaxPoints := ADCMaxBlocksDisplayed*ADCNumPointsPerBlock ;

       // Set display time units
       SetDisplayUnits ;

       if MainFrm.Recording then
       begin
         if not RecordingFirstTrace then
         begin
           scADCDisplay.XOffset := scADCDisplay.XOffset + scADCDisplay.MaxPoints;
         end else
         begin
           scADCDisplay.XOffset := Round((scADCDisplay.XOffset +
                                          scADCDisplay.NumPoints) * OldTScale /
                                          scADCDisplay.TScale);
           // scADCDisplay.XOffset := 0;
           RecordingFirstTrace := False;
         end;
       end else
         scADCDisplay.XOffset := 0;
       scADCDisplay.xMin := 0 ;
       scADCDisplay.xMax := scADCDisplay.MaxPoints-1 ;
       // Enable/disable display calibration grid
       //scADCDisplay.DisplayGrid := MainFrm.mnDisplayGrid.Checked ;
       scADCDisplay.SetDataBuf( @ADCDisplayBuf ) ;

//       scADCDisplay.XOffset := (CDRFH.NumSamplesInFile*ADCNumPointsPerBlock)
//                            div (ADCNumScansPerBlock*ADCNumChannels) ;

       scADCDisplay.NumPoints := 0 ;
       scADCDisplay.Invalidate ;

       // Clear markers on display
       scADCDisplay.ClearMarkers ;

       // Initialise counters
       ADCBlockCount := ADCNumScansPerBlock ;
       ADCDispPointer := 0 ;
       ADCNumBlocksDisplayed := 0 ;
       ADCStartDisplayScan := ADCOldestScan ;
       ResetDisplays := False ;


       end ;

     // Load latest A/D samples into ADCBuf
     LabIO.GetADCSamples( ADCDevice, ADCBuf^ ) ;

     // Find latest A/D samples

     nCount := 0 ;
     While ((ADCBuf^[ADCEmptyPointer] <> EmptyFlag) or
            (ADCBuf^[ADCEmptyPointer+1] <> -EmptyFlag)) and
           (nCount < ADCNumSamplesInWriteBuffer) do begin
           ADCEmptyPointer := ADCEmptyPointer + MainFrm.ADCNumChannels*2 ;
           if ADCEmptyPointer >= ADCNumSamplesInBuffer then
              ADCEmptyPointer := ADCEmptyPointer - ADCNumSamplesInBuffer ;
           Inc(nCount) ;
           end ;
     ADCLatestScan := ADCEmptyPointer - MainFrm.ADCNumChannels*4 ;
     if ADCLatestScan < 0 then ADCLatestScan := ADCLatestScan + ADCNumSamplesInBuffer ;

     // Cumulative number of A/D scans
     ADCNumScansCumulative := ADCNumScansCumulative + (nCount*2) ;

     // Update numerical readout of A/D channel signals
     s := '' ;
     for i := 0 to MainFrm.ADCNumChannels-1 do begin
         if i > 0 then s := s + '<br>' ;
         s := s + format('%s = %.4g %s',
                  [MainFrm.ADCChannel[i].ADCName,
                  ((ADCBuf^[ADCLatestScan + MainFrm.ADCChannel[i].ChannelOffset])-MainFrm.ADCChannel[i].ADCZero)*MainFrm.ADCChannel[i].ADCScale,
                  MainFrm.ADCChannel[i].ADCUnits]) ;
         end ;

     // Read and display samples from A/D sample buffer
     // -----------------------------------------------

     if ADCOldestScan = ADCLatestScan then Done := True
                                      else Done := False ;
     While not Done do begin

        // Initialise block
        if ADCBlockCount >= ADCNumScansPerBlock then begin
           for ch := 0 to MainFrm.ADCNumChannels-1 do begin
               ADCyMin[ch] := ADCMaxValue ;
               ADCyMax[ch] := ADCMinValue ;
               end ;
           ADCBlockCount := 0 ;
           end ;

        // Determine min. / max. value & order of samples within compression block
        for ch := 0 to MainFrm.ADCNumChannels-1 do begin

            y := ADCBuf^[ADCOldestScan+ch] ;

            if y < ADCyMin[ch] then begin
               ADCyMin[ch] := y ;
               ADCyMinAt[ch] := ADCBlockCount ;
               end ;
            if y > ADCyMax[ch] then begin
               ADCyMax[ch] := y ;
               ADCyMaxAt[ch] := ADCBlockCount ;
               end ;
            end ;
        Inc(ADCBlockCount) ;

        // When block complete ... write min./max. to display buffer
        if ADCBlockCount >= ADCNumScansPerBlock then begin

           // First point
           for ch := 0 to MainFrm.ADCNumChannels-1 do begin
               if ADCyMaxAt[ch] <= ADCyMinAt[ch] then
                  ADCDisplayBuf[ADCDispPointer] := ADCyMax[ch]
               else ADCDisplayBuf[ADCDispPointer] := ADCyMin[ch] ;
               ADCDispPointer := ADCDispPointer + 1 ;
               end ;

           // Second point
           if ADCBlockCount > 1 then begin
              for ch := 0 to MainFrm.ADCNumChannels-1 do begin
                  if ADCyMaxAt[ch] >= ADCyMinAt[ch] then
                     ADCDisplayBuf[ADCDispPointer] := ADCyMax[ch]
                  else ADCDisplayBuf[ADCDispPointer] := ADCyMin[ch] ;
                  ADCDispPointer := ADCDispPointer + 1 ;
                  end ;
              end ;

           Inc(ADCNumBlocksDisplayed) ;
           end ;


        // Handle Dynamic Protocol
        if DynamicProtocolEnabled and
           StimulusRequired and
           ckDynamicProtocolEnabled.Checked then
        begin

          // Get input channel for dyanmic protocol
          DynamicProtocolCh := MainFrm.DynamicProtocol.SelectedChannel;

          // Read scan and check against threshold
          if ((MainFrm.DynamicProtocol.Direction = 0) and ((MainFrm.ADCChannel[DynamicProtocolCh].ADCScale * (ADCBuf^[ADCOldestScan+MainFrm.ADCChannel[DynamicProtocolCh].ChannelOffset] - MainFrm.ADCChannel[DynamicProtocolCh].ADCZero)) > MainFrm.DynamicProtocol.Threshold)) or
             ((MainFrm.DynamicProtocol.Direction = 1) and ((MainFrm.ADCChannel[DynamicProtocolCh].ADCScale * (ADCBuf^[ADCOldestScan+MainFrm.ADCChannel[DynamicProtocolCh].ChannelOffset] - MainFrm.ADCChannel[DynamicProtocolCh].ADCZero)) < MainFrm.DynamicProtocol.Threshold)) then
          begin

            // Increment number of scans that meet threshold criteria
            Inc(DynamicProtocolCounter);

            // Check if number of scans exceeds duration and
            // execute code for triggering event
            if (DynamicProtocolCounter > (MainFrm.DynamicProtocol.Duration / MainFrm.ADCScanInterval)) then
            begin

              // Place code to execute when event occurs here.

              // Initialize buffer size
              DACNumScansInBuffer := 512;

              // Copied (almost) exactly from StartADC
              if MainFrm.DynamicProtocol.EPStimIndex > 0 then begin
                // Load program
                //Stimulator.LoadProgram( MainFrm.StimFileName ) ;
                Stimulator.LoadProgram( MainFrm.DynamicProtocol.EPStimFileName ) ;
                // Set buffer size to stimulus period
                DACNumScansInBuffer := Round(Stimulator.ProtocolDuration/MainFrm.ADCScanInterval) ;
                // Increase buffer size for single stimulus (to allow erasure of stimulus)
                if Stimulator.Prog.NumRepeats <= 1 then begin
                  ClearStimulusAfterScanNum := DACNumScansInBuffer ;
                  DACNumScansInBuffer := DACNumScansInBuffer + Round(1.0/MainFrm.ADCScanInterval) ;
                  SingleStimulusInProgress := True ;
                end ;
              end ;

              // Photo-stimulus
              // Copied (almost) exactly from StartADC
              if ckPhotoStimEnabled.Checked then begin
                if (ckDynamicProtocolEnabled.Checked) then begin
                  if (MainFrm.DynamicProtocol.PSStimFileName <> '') and (MainFrm.DynamicProtocol.PSStimIndex > 0) then begin
                    PhotoStimulator.LoadProgram(MainFrm.DynamicProtocol.PSStimFileName);
                    if MainFrm.PhotoStim.RepeatedStim then begin
                      //DACNumScansInBuffer := Round(PhotoStimulator.ProtocolDuration/MainFrm.ADCScanInterval) ;
                      DACNumScansInBuffer := Round((PhotoStimulator.ProtocolDuration+(MainFrm.PhotoStim.Period-PhotoStimulator.ProtocolDuration))/MainFrm.ADCScanInterval) ;
                    end
                    else begin
                      DACNumScansInBuffer := Max(DACNumScansInBuffer,
                                                 Round(PhotoStimulator.ProtocolDuration/MainFrm.ADCScanInterval)) ;
                      ClearStimulusAfterScanNum := DACNumScansInBuffer ;
                      DACNumScansInBuffer := DACNumScansInBuffer + Round(1.0/MainFrm.ADCScanInterval) ;
                      SingleStimulusInProgress := True ;
                    end;
                  end;
                end;
              end;

              // Reset to zero so timer function does not stop pre-maturely
              ADCNumScansCumulative := 0;

              // Update electro-physiology waveforms if required
              // by dynamic protocol
              if MainFrm.DynamicProtocol.EPRestart then
              begin
                UpdateStimulusWaveforms(True,false);
              end
              else
              begin
                UpdateStimulusWaveforms(False,false);
              end;

              // Update photo-stimulus waveforms if required
              // by dynamic protocol
              if MainFrm.DynamicProtocol.PSRestart then
              begin
                UpdatePhotoStimulus(ckPhotoStimEnabled.Checked);
              end
              else
              begin
                UpdatePhotoStimulus(False);
              end;

              // Update protocols on LabIO devices
              for Device := 1 to LabIO.NumDevices do
              begin

                // Set up digital output sweep
                if DeviceDIGInUse[Device] then
                begin
                LabIO.MemoryToDIG(Device,
                                  DIGBufs[Device]^,
                                  DACNumScansInBuffer,
                                  MainFrm.ADCScanInterval,
                                  True,
                                  False,
                                  FindTimingDevice,
                                  True ) ;
                end ;

                // Set up D/A output sweep
                if DeviceInUse[Device] then
                begin
                  LabIO.MemoryToDAC(Device,
                                    DACBufs[Device]^,
                                    LabIO.NumDACs[Device],
                                    DACNumScansInBuffer,
                                    MainFrm.ADCScanInterval,
                                    True,
                                    False,
                                    FindTimingDevice) ;
                end ;

              end ;

              // Reset dynamic protocol scan counter
              DynamicProtocolCounter := 0;

              // Disable dyanmic protocol handling after the event occurs
              DynamicProtocolEnabled := False;

            end;

          end

          // Scan does not meet criteria
          else
          begin

            // Reset dynamic protocol scan counter
            DynamicProtocolCounter := 0;

          end;

        end;  // End of dynamic protocol


        // Increment pointer to next available scan
        ADCOldestScan := ADCOldestScan + MainFrm.ADCNumChannels ;
        // Keep within cyclic buffer
        if ADCOldestScan >= ADCNumSamplesInBuffer then
           ADCOldestScan := ADCOldestScan - ADCNumSamplesInBuffer ;
        // Terminate if all points processed or at end of display   
        if ADCOldestScan = ADCLatestScan then Done := True ;
        if ADCNumBlocksDisplayed >= ADCMaxBlocksDisplayed then Done := True ;

        end ;

     // Display latest points added to display buffer
     scADCDisplay.DisplayNewPoints( ADCNumBlocksDisplayed*ADCNumPointsPerBlock );
     //scADCDisplay.Invalidate ;

     // Update cursor readout
     s := '' ;
     for ch := 0 to scADCDisplay.NumChannels-1 do begin
         MainFrm.ADCChannel[ch].CursorValue := MainFrm.ADCChannel[ch].ADCScale *
                                       (ADCBuf^[ADCLatestScan+MainFrm.ADCChannel[ch].ChannelOffset]
                                        - MainFrm.ADCChannel[ch].ADCZero) ;
         s := s + format( ' %s= %9.3f %s<br>',
                            [MainFrm.ADCChannel[ch].ADCName,
                             MainFrm.ADCChannel[ch].CursorValue ,
                             MainFrm.ADCChannel[ch].ADCUnits] ) ;
         end ;
    lbMeasurements.Caption := s ;


    // Determine A/D write buffer being currently filled
    if ADCOldestScan > MainFrm.ADCNumChannels then begin
       ADCActiveBuffer := ADCOldestScan div ADCNumSamplesInWriteBuffer
       end ;
//    ADCActiveBuffer := ADCOldestScan - 1 ;
//    if ADCActiveBuffer < 0 then ADCActiveBuffer := ADCNumSamplesInBuffer - 1 ;
//    ADCActiveBuffer := ADCActiveBuffer div ADCNumSamplesInWriteBuffer ;
  //   outputdebugString(PChar(format('%d ',[ADCOldestScan]))) ;

    // Write buffer changed
    // --------------------
//MainFrm.StatusBar.SimpleText := format('%d %d %d',[ADCActiveBuffer,ADCWriteBuffer,ADCEmptyPointer]) ;

    if ADCActiveBuffer <> ADCWriteBuffer then begin

       // Write buffer to file (if recording)
       BufStart := ADCWriteBuffer*ADCNumSamplesInWriteBuffer ;
       if MainFrm.Recording then begin
       //outputdebugString(PChar(format('%d %d',[ADCActiveBuffer,ADCWriteBuffer]))) ;
          FileSeek( MainFrm.IDRFile.EDRFileHandle,EDRFilePointer,0) ;
          NumBytesWritten := FileWrite( MainFrm.IDRFile.EDRFileHandle,
                                        ADCBuf^[BufStart],
                                        ADCNumSamplesInWriteBuffer*2 ) ;
          EDRFilePointer := EDRFilePointer +  Int64(NumBytesWritten) ;
          MainFrm.IDRFile.ADCNumScansInFile := MainFrm.IDRFile.ADCNumScansInFile +
                                               (ADCNumSamplesInWriteBuffer div MainFrm.ADCNumChannels) ;

          // Report progress
          RecordingTimeElapsed := ((EDRFilePointer - cNumEDRHeaderBytes)*MainFrm.ADCScanInterval)/
                                    (2*MainFrm.ADCNumChannels) ;
          MainFrm.StatusBar.SimpleText := format(
          'RECORDING: %.1f/%.1f',
          [RecordingTimeElapsed,RecordingTimeRequired]);
          Application.ProcessMessages ;

          // Terminate when required recording time achieved
          if RecordingTimeElapsed >= RecordingTimeRequired then begin
             MainFrm.Recording := False ;
             bStop.Click ;
             Exit ;
             end ;
          end ;
       // Re-fill buffer with empty flag
       i := BufStart ;
       while i < BufStart+ADCNumSamplesInWriteBuffer do begin
         ADCBuf^[i] := EmptyFlag ;
         Inc(i) ;
         ADCBuf^[i] := -EmptyFlag ;
         Inc(i) ;
         end ;

       // Increment to next write buffer
       Inc(ADCWriteBuffer) ;
       if ADCWriteBuffer >= ADCNumWriteBuffers then ADCWriteBuffer := 0 ;
       end ;

    end ;


procedure TRecADCOnlyFrm.TimerTimer(Sender: TObject);
// ---------------------------------------------------
// Scheduled timer event supervising recording/display
// ---------------------------------------------------

begin

    // Prevent more than one instance running
    if TimerProcBusy then Exit ;
    TimerProcBusy := True ;

    // Disable recording file contains A/D samples
    if (MainFrm.IDRFile.ADCNumScansInFile > 0) or (not MainFrm.IDRFile.Open) then
       bRecord.Enabled := False
    else if Not MainFrm.Recording then bRecord.Enabled := True ;

    // If a single stimulus program is in progress clear D/A buffer
    // and re-enable stimulus button
    if SingleStimulusInProgress then begin
       if ADCNumScansCumulative > ClearStimulusAfterScanNum then begin
          //ClearStimulusPulses ;
          UpdateStimulusWaveforms( False, false ) ;
          UpdatePhotoStimulus( False ) ;
          ClearStimulusAfterScanNum := High(ClearStimulusAfterScanNum) ;
          StimulusRequired := False ;
          // Reset stimulus program control buttons
          bStartStimulus.Enabled := True ;
          bStopStimulus.Enabled := False ;
          if not ckPlaybackEnabled.Checked then
            cbStimProgram.Enabled := True;
          VHoldGrp.Enabled := True ;
          end ;
       end ;

     // Update D/A output buffer
     LabIO.UpdateDACOutputBuffer ;
     //LabIO.UpdateDIGOutputBuffer ;

    // Update A/D signal display
    UpdateADCDisplay ;

    TimerProcBusy := False ;

    end;


procedure TRecADCOnlyFrm.FormResize(Sender: TObject);
// ------------------------------------------------
// Adjust control sizes when window size is changed
// ------------------------------------------------
begin

    // Controls group
    ControlsGrp.Height := ClientHeight - ControlsGrp.Top - 5 ;

    lbMeasurements.Height := (MainFrm.ADCNumChannels + 2)*15 ;
    ReadoutGrp.ClientHeight := lbMeasurements.Top + lbMeasurements.Height + 5 ;
    ReadoutGrp.Top := ControlsGrp.ClientHeight - ReadoutGrp.Height - 5 ;
    MarkGrp.Top := ReadoutGrp.Top - MarkGrp.Height - 5 ;
    ImageCaptureGrp.Height := Max(MarkGrp.Top - ImageCaptureGrp.Top - 5,2) ;

    // Set size of signal time course display area
    SignalsGrp.Height :=  ClientHeight - SignalsGrp.Top - 5 ;
    SignalsGrp.Width := ClientWidth - SignalsGrp.Left - 5 ;
    TDisplayPanel.Top := SignalsGrp.Height - TDisplayPanel.Height - 2 ;
    ckFixZeroLevels.Top := TDisplayPanel.Top ;

    // Set width of time course display components
    scADCDisplay.Width := Max(SignalsGrp.Width - scADCDisplay.Left*2,2) ;
    scADCDisplay.Height := Max( 2,TDisplayPanel.Top - scADCDisplay.Top - 1) ;

    TDisplayPanel.Left := scADCDisplay.Left + scADCDisplay.Width - TDisplayPanel.Width ;

    // Identification field
    IdentGrp.Width := Max( ClientWidth - IdentGrp.Left - 5,2 );
    edIdent.Width := Max( IdentGrp.Width - edIdent.Left - 5,2 );

    end;


procedure TRecADCOnlyFrm.bMarkClick(Sender: TObject);
// ------------------------------
//  Add a text marker to the chart
// ------------------------------
var
     MarkerTime : Single ;
     TimeScale : Single ;
begin

     if scADCDisplay.TScale*edTDisplay.Scale > 0.0 then begin
        TimeScale := (scADCDisplay.TScale*edTDisplay.Scale) ;
        end
     else TimeScale := 1.0 ;

     MarkerTime := (scADCDisplay.NumPoints + scADCDisplay.XOffset)*TimeScale ;

     // Plot marker on chart
     MainFrm.IDRFile.AddMarker( MarkerTime, EdMarker.text ) ;
     scADCDisplay.AddMarker( Round(MarkerTime/TimeScale) - scADCDisplay.XOffset,
                          EdMarker.text );

     edNumMarkers.text := format('%d',[MaxMarker-MainFrm.IDRFile.NumMarkers]);
     end;


procedure TRecADCOnlyFrm.FormActivate(Sender: TObject);
// ---------------------------
// Form has become active form
// ---------------------------
var
    i : Integer ;
    NewFile : String;
begin

     // Stop seal test (if it is running)
     for i := 0 to MainFrm.MDIChildCount-1 do begin
         if (MainFrm.MDIChildren[i].Name = 'SealTestFrm') then begin
            TSealTestFrm(MainFrm.MDIChildren[i]).StopSealTest ;
            end ;
         end ;

     // If no new file is ready for writing, open a default one
     if (MainFrm.IDRFile.ADCNumScansInFile > 0) or
        (not MainFrm.IDRFile.Open) then
     begin
       MainFrm.mnCloseFile.Click;
       NewFile := MainFrm.CreateIndexedFileName(MainFrm.IDRFile.FileName);
       NewFile := ChangeFileExt(NewFile, '.'+DataFileExtension);
       MainFrm.IDRFile.CreateNewFile(NewFile);
       LogFrm.AddLine('New file created: ' + MainFrm.IDRFile.FileName);
       MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName;
       MainFrm.UpdateRecentFilesList;
     end;

     // Reload stimuli programs so someone can define a new one
     // without closing + reopening Record Signals window
     // Load list of stimulus programs
     UpdateStimProgramList ;

     // Load list of photo-stimulus programs
     UpdatePhotoStimProgramList ;

     if not LabIO.ADCActive[ADCDevice] then begin
        NewDisplaySetup ;
        StartADC ;
        // To address failure of stimulus protocol to start on some rigs
        ToggleVoltageStimulus;
        end ;

     end;


procedure TRecADCOnlyFrm.StopADC ;
// -------------------------
// Stop A/D and D/A activity
// -------------------------
var
    Device : SmallInt ;
begin
     // Stop A/D converter(s)
     for Device := 1 to LabIO.NumDevices do LabIO.StopADC(Device) ;

     // Stop D/A converter(s)
     for Device := 1 to LabIO.NumDevices do LabIO.StopDAC(Device) ;

     ADCRunning := False ;

     end ;


procedure TRecADCOnlyFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
var
    ch : Integer ;
    Device : SmallInt ;
begin

     Timer.Enabled := False ;

     // Stop A/D converter(s)
     for Device := 1 to LabIO.NumDevices do LabIO.StopADC(Device) ;

     // Stop D/A converter(s)
     for Device := 1 to LabIO.NumDevices do LabIO.StopDAC(Device) ;

     // Free buffers
     // Dispose of A/D & D/A buffers
     if ADCBuf <> Nil then FreeMem(ADCBuf) ;
     for Device := 1 to LabIO.NumDevices do begin
         if DACBufs[Device] <> Nil then FreeMem(DACBufs[Device]) ;
         DACBufs[Device] := Nil ;
         if DIGBufs[Device] <> Nil then FreeMem(DIGBufs[Device]) ;
         DIGBufs[Device] := Nil ;
         end ;

     // Save visibility state for each channel
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
         MainFrm.ADCChannelRecADCOnlyUnitVisible[ch] := scADCDisplay.ChanVisible[ch];
     end;

     MainFrm.LastRecADCOnlyWidth := Width;
     MainFrm.LastRecADCOnlyHeight := Height;
     MainFrm.LastRecADCOnlyTop := Top;
     MainFrm.LastRecADCOnlyLeft := Left;

     // Request destruction of form
     Action := caFree ;

     end;


procedure TRecADCOnlyFrm.bStartStimulusClick(Sender: TObject);
// -----------------------
// Start stimulus program
// -----------------------
begin
     if (cbStimProgram.ItemIndex > 0) or
        ckPhotoStimEnabled.Checked or
        ckDynamicProtocolEnabled.Checked or
        ckPlaybackEnabled.Checked then
     begin

        // Enable dynamic protocol handling
        if ckDynamicProtocolEnabled.Checked then DynamicProtocolEnabled := True ;

        StimulusRequired := True ;
        StartADC ;
        end ;
     end;


procedure TRecADCOnlyFrm.bStopStimulusClick(Sender: TObject);
// ---------------------
// Stop stimulus program
// ---------------------
var
    Device : Integer ;
begin

     StimulusRequired := False ;

     // Set voltage and digital stimulus channels to default (off) settings
     UpdateStimulusWaveforms( StimulusRequired, false ) ;

     // Update photo-stimulus
     UpdatePhotoStimulus( StimulusRequired and ckPhotoStimEnabled.Checked ) ;

     // A/D restart needed with NIDAQ-MX to force DAC updates
     //if LABIO.NIDAQAPI = NIDAQMX then StartADC ;

     bStartStimulus.Enabled := True ;
     bStopStimulus.Enabled := False ;
     if not ckPlaybackEnabled.Checked then
        cbStimProgram.Enabled := True ;
     VHoldGrp.Enabled := True ;

     end;


procedure TRecADCOnlyFrm.bStopClick(Sender: TObject);
// -----------------------------
// Stop recording images to disk
// -----------------------------
var
     i : Integer ;
begin

     MainFrm.Recording := False ;
     bStop.Enabled := False ;
     bMark.Enabled := False ;
     ckRecordADCSignalsOnly.Enabled := True ;

     MainFrm.IDRFile.ADCMaxValue := ADCMaxValue ;

     // Shut down stimulus if one is in use
     bStopStimulus.Click;
     Wait(0.25); // Delay to prevent shutter from failing to close
     {if cbStimProgram.ItemIndex > 0 then begin
        // Set voltage and digital stimulus channels to default (off) settings
        StimulusRequired := False ;
        UpdateStimulusWaveforms( StimulusRequired, false ) ;
        // A/D restart needed with NIDAQ-MX to force DAC updates
        //if LABIO.NIDAQAPI = NIDAQMX then StartADC ;
        bStopStimulus.Enabled := False ;
        bStartStimulus.Enabled := True ;
        if not ckPlaybackEnabled.Checked then
           cbStimProgram.Enabled := True ;
        VHoldGrp.Enabled := True ;
        end ;}

     if ckRecordADCSignalsOnly.Checked then begin
        // If recording analogue signals only, create an empty image
        CreateEmptyImageFile ;
        LogFrm.AddLine( 'Recording stopped' ) ;
        MainFrm.mnViewImages.Click ;
        Close ;
        Exit ;
        end
     else if not AutoRecordingMode then begin
        // Let user select and import an external image file into IDR file
        // if not under control of external program via AutoUnit

        // Get file containing images
        ImportImageFile( '' ) ;

        MainFrm.IDRFile.CloseFile ;
        MainFrm.IDRFile.OpenFile( MainFrm.IDRFile.FileName ) ;
        MainFrm.DataDirectory := ExtractFilePath(MainFrm.IDRFile.FileName) ;
        MainFrm.UpdateRecentFilesList ;
        // Display in program title bar
        MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;
        LogFrm.AddLine( 'File opened: ' + MainFrm.IDRFile.FileName );

        // Update Viewing window if it is open
        for i := 0 to MainFrm.MDIChildCount-1 do begin
            if MainFrm.MDIChildren[I].Name = 'ViewFrm' then TViewFrm(MainFrm.MDIChildren[I]).NewFile ;
            end ;

        NewDisplaySetup ;

        end ;

     LogFrm.AddLine( 'Recording stopped' ) ;

     // Re-start A/D
     StartADC ;

     end;


procedure TRecADCOnlyFrm.bRecordClick(Sender: TObject);
// ------------------------
// Recording button clicked
// ------------------------
var
    RecordingMode : Integer ;
begin

     // Set recording mode
     if rbLineScan.Checked or ckRecordADCSignalsOnly.Checked then
        RecordingMode := LSMLineScanMode
     else if rbSequenceTrigger.Checked then
        RecordingMode := LSMXYSeriesSequenceMode
     else RecordingMode := LSMXYSeriesFrameTriggerMode ;

     // Start recording
     AutoRecordingMode := False ;
     RecordingFirstTrace := True;
     scADCDisplay.XOffset := 0;
     StartRecordingToDisk( edRecordingTime.Value,
                           1,
                           RecordingMode ) ;



     end ;


procedure TRecADCOnlyFrm.StartRecordingToDisk(
          RecordingTime : Single ;             // Time to record for (s)
          Num : Integer ;                      // No. of frames/scans to record
          RecordingMode : Integer ) ;          // Recording mode
// ---------------------------------
// Starting recording images to disk
// ---------------------------------
var
     i : Integer ;
begin

     // Close seal test form to avoid possible interruption of recording
     for i := 0 to MainFrm.MDIChildCount-1 do begin
         if MainFrm.MDIChildren[i].Name = 'SealTestFrm' then
            TSealTestFrm(MainFrm.MDIChildren[i]).Close ;
         end ;

     // Recording not allowed if no data file open
     if not MainFrm.IDRFile.Open then begin
        // Report failure to record
        MainFrm.StatusBar.SimpleText :=
        ' Record (LSM) : Unable to record! No data file open! ' ;
        exit ;
        end ;

     // Enable file for writing
     if not MainFrm.IDRFile.WriteEnabled then MainFrm.IDRFile.WriteEnabled := True ;


     MainFrm.Recording := True ;
     bRecord.Enabled := False ;
     bStop.Enabled := True ;
     bMark.Enabled := True ;
     ckRecordADCSignalsOnly.Enabled := False ;

     // Set image capture mode controls
     SetCaptureMode ;

     // Set dimensions of frame stored in file
     MainFrm.IDRFile.IntensityScale := 1.0 ;
     MainFrm.IDRFile.IntensityOffset := 1.0 ;

     // Delay between start of A/D sampling and image acquisition
     MainFrm.IDRFile.ImageStartDelay := edImageStartDelay.Value ;

     // Line scan mode
     if RecordingMode = LSMLineScanMode then begin
        // Line scan mode
        rbLineScan.Checked := True ;
        MainFrm.IDRFile.LineScan := True ;
        end
      else if RecordingMode = LSMXYSeriesSequenceMode then begin
        // XY Series (First frame trigger mode)
        rbImage.Checked := True ;
        MainFrm.IDRFile.LineScan := False ;
        rbSequenceTrigger.Checked := True ;
        end
      else begin
        // XY Series (Each frame trigger mode)
        rbImage.Checked := True ;
        MainFrm.IDRFile.LineScan := False ;
        rbFrameTrigger.Checked := True ;
        end ;

     // Duration of single image frame
     //MainFrm.IDRFile.FrameInterval := RecordingTime / Num ;

     // Update file channels with current settings
     MainFrm.IDRFile.ADCNumChannels := MainFrm.ADCNumChannels ;
     MainFrm.IDRFile.ADCScanInterval := MainFrm.ADCScanInterval ;
     for i := 0 to MainFrm.ADCNumChannels-1 do
         MainFrm.IDRFile.ADCChannel[i] := MainFrm.ADCChannel[i] ;
     MainFrm.IDRFile.ADCNumScansInFile := 0 ;

     // Request stimulus if a stimulus program is in use
     if (cbStimProgram.ItemIndex > 0) or ckPhotoStimEnabled.Checked or ckDynamicProtocolEnabled.Checked then begin
        if ckDynamicProtocolEnabled.Checked then DynamicProtocolEnabled := True ;
        StimulusRequired := True ;
        bStopStimulus.Enabled := True ;
        bStartStimulus.Enabled := False ;
        end ;

     // Move file pointer to start of A/D data file
     EDRFilePointer := cNumEDRHeaderBytes ;

     // Recording period
     // Note. If a valid (>0.0) recording time is supplied as an
     // argument to this method, over-ride existing setting with it
     if RecordingTime > 0.0 then edRecordingTime.Value := RecordingTime ;
     RecordingTimeRequired := edRecordingTime.Value ;
     MainFrm.ADCRecordingTime := edRecordingTime.Value ;

     // Reset chart displays
     ResetDisplays := True ;

     // Update log file
     if rbLineScan.Checked then begin
        LogFrm.AddLine( format(
                     'LSM-Line Scan: Recording Started T=%.4gs %d lines',
                     [RecordingTime,Num] )) ;
        end
     else begin
        LogFrm.AddLine( format(
                     'LSM-XY Scan: Recording Started T=%.4gs %d lines',
                     [RecordingTime,Num] )) ;
        end ;

     // Start A/D sampling
     StartADC ;

     MainFrm.StatusBar.SimpleText := 'RECORDING:' ;

     end;


procedure TRecADCOnlyFrm.ImportImageFile(
          ImportFileName : String               // Name of file to import from
          ) ;
// -----------------------------
// Import images from image file
// -----------------------------
var
    OK : Boolean ;
    FrameNum : Integer ; // Frame counter
    PFrameBuf : Pointer ; // Image frame buffer pointer
    IDRFileName : String ; // Name of .IDR file to hold imported images
    i : Integer ;
    FileNum : Integer ;    // Index into OpenDialog.Files list
    iFrame : Integer ;     // Frame counter
    NumFramesImported : Integer ;
    NumFiles : Integer ;
begin

    if ImportFileName = '' then  begin

       // If file name is undefined, ask user

       OpenDialog.options := [ofPathMustExist,ofAllowMultiSelect] ;
       if MainFrm.DataDirectory <> '' then OpenDialog.InitialDir := MainFrm.DataDirectory ;

       OpenDialog.Filter :=
       'BIORad PIC Files (*.PIC)|*.PIC|' +
       'TIFF Files (*.TIF)|*.TIF|' +
       'STK Files (*.STK)|*.STK';
       OpenDialog.Title := 'Import Image File ' ;

       // Exit if no file name available
       if (not OpenDialog.Execute) or (OpenDialog.Files.Count <= 0) then Exit ;
       NumFiles := OpenDialog.Files.Count ;
       ImportFileName := OpenDialog.Files[FileNum] ;
       end
    else NumFiles := 1 ;

    // Ensure file is write enabled
    if not MainFrm.IDRFile.WriteEnabled then MainFrm.IDRFile.WriteEnabled := True ;

    // Import frames from all files in list
    NumFramesImported := MainFrm.IDRFile.NumFrames ;
    for FileNum := 0 to NumFiles-1 do begin

        // Open file

        if not ImageFile.OpenFile( ImportFileName ) then begin
           MainFrm.StatusBar.SimpleText := 'Unable to open : ' + ImportFileName ;
           Exit ;
           end ;

        // Get image properties from first import file
        if NumFramesImported = 0 then begin
           MainFrm.IDRFile.FrameWidth := ImageFile.FrameWidth ;
           MainFrm.IDRFile.FrameHeight := ImageFile.FrameHeight ;
           MainFrm.IDRFile.PixelDepth := ImageFile.PixelDepth ;
           // Get camera pixel size
           MainFrm.IDRFile.XResolution := ImageFile.XResolution ;
           MainFrm.IDRFile.ResolutionUnits := ImageFile.ResolutionUnit ;

           MainFrm.IDRFile.NumFrameTypes := 1 ;
           MainFrm.IDRFile.FrameInterval := ImageFile.TResolution ;
           MainFrm.IDRFile.ADCScanInterval := MainFrm.ADCScanInterval ;
           MainFrm.IDRFile.ADCNumChannels := MainFrm.ADCNumChannels ;
           end ;

        // Terminate import if frame size changed
        if (MainFrm.IDRFile.FrameWidth <> ImageFile.FrameWidth) or
           (MainFrm.IDRFile.FrameHeight <> ImageFile.FrameHeight) or
           (MainFrm.IDRFile.PixelDepth <> ImageFile.PixelDepth) then begin
           MainFrm.StatusBar.SimpleText := 'Import: Aborted at ' + ImportFileName ;
           LogFrm.AddLine( 'Import: Aborted at ' + ImportFileName ) ;
           ImageFile.CloseFile ;
           Break ;
           end ;

        // Allocate frame buffer
        GetMem( PFrameBuf,MainFrm.IDRFile.NumBytesPerFrame ) ;

        // Import frames from this file
        for iFrame := 1 to ImageFile.NumFrames do begin
           if ImageFile.LoadFrame( iFrame, PFrameBuf ) then begin
              Inc(NumFramesImported) ;
              MainFrm.IDRFile.SaveFrame( NumFramesImported, PFrameBuf ) ;
              MainFrm.StatusBar.SimpleText := format(
              'Importing %d/%d frames from %s',
              [iFrame,ImageFile.NumFrames,ImportFileName]) ;
              //Application.ProcessMessages ;
              end ;
           end ;

        // Set inter-frame interval
        MainFrm.IDRFile.FrameInterval := edInterval.Value ;
        if rbLineScan.Checked then begin
           MainFrm.IDRFile.FrameInterval := ImageFile.FrameHeight*edInterval.Value ;
           MainFrm.IDRFile.NumFrameTypes := MainFrm.IDRFile.NumFrames ;
           end ;

        // Close this import file
        ImageFile.CloseFile ;
        // Free buffer
        FreeMem( PFrameBuf ) ;

        end ;

    // Display in program title bar
    MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;

    // Close and re-open IDR file
    MainFrm.IDRFile.CloseFile ;
    MainFrm.IDRFile.OpenFile( MainFrm.IDRFile.FileName ) ;
    MainFrm.DataDirectory := ExtractFilePath(MainFrm.IDRFile.FileName) ;

    if MainFrm.IDRFile.NumFrames > 0 then MainFrm.mnViewImages.Click ;

    LogFrm.AddLine( ' Image imported from ' + ImportFileName ) ;

    end ;


procedure TRecADCOnlyFrm.CreateEmptyImageFile ;
// ---------------------------
// Create an empty image file
// ---------------------------
var
    PFrameBuf : Pointer ; // Image frame buffer pointer
    i : Integer ;
begin

      // Create small image 32x1000 8 bit image
      if rbLineScan.Checked then begin
         // Dummy image for line scan
         MainFrm.IDRFile.FrameWidth := 32 ;
         MainFrm.IDRFile.FrameHeight := 1000 ;
         end
      else begin
         // Dummy image for XY
         MainFrm.IDRFile.FrameWidth := 1000 ;
         MainFrm.IDRFile.FrameHeight := 32 ;
         end ;

      MainFrm.IDRFile.PixelDepth := 8 ;
      MainFrm.IDRFile.XResolution := 1 ;
      MainFrm.IDRFile.ResolutionUnits := 'um' ;

      MainFrm.IDRFile.NumFrameTypes := 1 ;
      MainFrm.IDRFile.FrameInterval := MainFrm.ADCScanInterval*MainFrm.IDRFile.ADCNumScansInFile*2.0 ;

      // Allocate frame buffer
      GetMem( PFrameBuf,MainFrm.IDRFile.NumBytesPerFrame ) ;

      // Set image to zero
      for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do PByteArray(PFrameBuf)^[i] := 0 ;
      // Save to file
      MainFrm.IDRFile.SaveFrame( 1, PFrameBuf ) ;
      MainFrm.IDRFile.NumFrames := 1 ;

      MainFrm.IDRFile.LineScan := rbLineScan.Checked ;

      // Close and re-open file to update MainFrm.IDRFile.ADCScanInterval and MainFrm.IDRFile.ADCNumScansInFile
      MainFrm.IDRFile.CloseFile ;
      MainFrm.IDRFile.OpenFile( MainFrm.IDRFile.FileName ) ;
      MainFrm.DataDirectory := ExtractFilePath(MainFrm.IDRFile.FileName) ;

      // Set FrameInterval
      MainFrm.IDRFile.FrameInterval := MainFrm.IDRFile.ADCScanInterval*MainFrm.IDRFile.ADCNumScansInFile ;

      // Display in program title bar
      MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;

      end ;


procedure TRecADCOnlyFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
     // Prevent form closure if recording
     if MainFrm.Recording then CanClose := False
                          else CanCLose := True ;

     end;


procedure TRecADCOnlyFrm.rbTDisplayUnitsSecsClick(Sender: TObject);
// ------------------------------
// Set Display time units to secs
// ------------------------------
begin
  SetDisplayUnits ; // Do this first, because for scale change we want
                    // OldTScale = scADCDisplay.TScale
  if MainFrm.Recording then
  begin
    OldTScale := scADCDisplay.TScale;
    RecordingFirstTrace := True;
  end;
  ResetDisplays := True ;
  //   SetDisplayUnits ;
     end;


procedure TRecADCOnlyFrm.SetDisplayUnits ;
// ----------------------
// Set display time units
// ----------------------
begin
    if rbTDisplayUnitsSecs.Checked then begin
       edTDisplay.Units := 's' ;
       edTDisplay.Scale := 1.0 ;

       end
    else begin
       edTDisplay.Units := 'm' ;
       edTDisplay.Scale := 1.0/60.0 ;
       end ;

    scADCDisplay.TScale := (MainFrm.ADCScanInterval*ADCNumScansPerBlock*edTDisplay.Scale)
                              /ADCNumPointsPerBlock ;
    scADCDisplay.TUnits := edTDisplay.Units ;

    end ;


procedure TRecADCOnlyFrm.scADCDisplayCursorChange(Sender: TObject);
// ----------------------
// Display cursor changed
// ----------------------
var
    ch : Integer ;
begin
     if not InitialisationComplete then Exit ;

     for ch := 0 to scADCDisplay.NumChannels-1 do begin
         // Update zero level
         MainFrm.ADCChannel[ch].ADCZero := scADCDisplay.HorizontalCursors[ch] ;
         // Update vertical display region
         MainFrm.ADCChannel[ch].yMin := scADCDisplay.yMin[ch] ;
         MainFrm.ADCChannel[ch].yMax := scADCDisplay.yMax[ch] ;
         MainFrm.ADCChannel[ch].InUse := scADCDisplay.ChanVisible[ch] ;
         end ;
     end ;


procedure TRecADCOnlyFrm.edTDisplayKeyPress(Sender: TObject;
  var Key: Char);
// --------------------------------------------------------
// Request display update when A/D display duration changed
// --------------------------------------------------------
begin
     if key = #13 then begin
        MainFrm.ADCDisplayWindow := edTDisplay.Value ;
        if MainFrm.Recording then
        begin
          OldTScale := scADCDisplay.TScale;
          RecordingFirstTrace := True;
        end;
        SetDisplayUnits ;
        ResetDisplays := True ;
        end ;
     end;

procedure TRecADCOnlyFrm.cbStimProgramChange(Sender: TObject);
// ---------------------------------
// Stimulus program has been changed
// ---------------------------------
begin

     // Voltage protocol file name
     MainFrm.StimFileName := Mainfrm.VProtDirectory + cbStimProgram.Text + '.vpr' ;

     end;


procedure TRecADCOnlyFrm.SetInterval(
          Value : Single ) ;
// ----------------------
// Set line/frame interval
// ----------------------
begin
    edInterval.Value := Value ;
    end ;


function TRecADCOnlyFrm.GetInterval : Single ;
// ----------------------
// Get line/frame interval
// ----------------------
begin
    Result := edInterval.Value ;
    end ;


procedure TRecADCOnlyFrm.SetImageStartDelay(
          Value : Single ) ;
// ----------------------
// Set Delay between start of A/D sampling and image acquisition
// ----------------------
begin
    edImageStartDelay.Value := Value ;
    end ;


function TRecADCOnlyFrm.GetImageStartDelay : Single ;
// ----------------------
// Get Delay between start of A/D sampling and image acquisition
// ----------------------
begin
    Result := edImageStartDelay.Value ;
    end ;


procedure TRecADCOnlyFrm.SetDisplayGrid(
          Value : Boolean ) ;
// --------------------------------
// Add grid lines to chart displays
// --------------------------------
begin
    scADCDisplay.DisplayGrid := Value ;
    end ;


function TRecADCOnlyFrm.GetDisplayGrid : Boolean ;
// --------------------------------
// Remove grid lines from chart displays
// --------------------------------
begin
    Result := scADCDisplay.DisplayGrid ;
    end ;


procedure TRecADCOnlyFrm.rbImageClick(Sender: TObject);
// ----------------------------------
// XY series capture mode selected
// ----------------------------------
begin
     SetCaptureMode ;
     end;


procedure TRecADCOnlyFrm.rbLineScanClick(Sender: TObject);
// ----------------------------------
// Line scan capture mode selected
// ----------------------------------
begin
     SetCaptureMode ;
     end;


procedure TRecADCOnlyFrm.SetCaptureMode ;
// --------------------------------------
// Set display controls for capture mode
// --------------------------------------
begin
    if rbImage.Checked then begin
       // XY Series image capture mode
       lbInterval.Caption := 'Frame Interval' ;
       rbFrameTrigger.Enabled := True ;
       end
    else begin
       // Line scan capture mode
       lbInterval.Caption := 'Line Interval' ;
       rbFrameTrigger.Enabled := False ;
       rbFrameTrigger.Checked := False ;
       end ;
    end ;

procedure TRecADCOnlyFrm.edIdentKeyPress(Sender: TObject; var Key: Char);
// ------------------------------------------
// Update ident field when ident text changed
// ------------------------------------------
begin
     // Save to ident field
     Ident := edIdent.Text ;
     if Key = #13 then LogFrm.AddLine(  Ident ) ;
     end;

procedure TRecADCOnlyFrm.edVHold0KeyPress(Sender: TObject; var Key: Char);
// -----------------------
// Holding voltage changed
// -----------------------
begin
     if Key = #13 then UpdateHoldingVoltage ;
     end;


procedure TRecADCOnlyFrm.MagnifyChannelDisplay(
          ChanNum : Integer ) ;
// ------------------------------------
// Magnify selected A/D channel display
// ------------------------------------
begin
     if ChanNum < MainFrm.ADCNumChannels then scADCDisplay.YZoom(ChanNum, -50.0);
     end ;


procedure TRecADCOnlyFrm.ReduceChannelDisplay( ChanNum : Integer ) ;
// ------------------------------------
// Reduce selected A/D channel display
// ------------------------------------
begin
     if ChanNum < MainFrm.ADCNumChannels then scADCDisplay.YZoom(ChanNum, 50.0);
     end ;



procedure TRecADCOnlyFrm.bTDisplayHalfClick(Sender: TObject);
// ---------------------------
// Halve A/D display duration
// ---------------------------
begin
     edTDisplay.Value := edTDisplay.Value*0.5 ;
     MainFrm.ADCDisplayWindow := edTDisplay.Value ;
     if MainFrm.Recording then
     begin
       OldTScale := scADCDisplay.TScale;
       RecordingFirstTrace := True;
     end;
     SetDisplayUnits ;
     ResetDisplays := True ;
     end;

procedure TRecADCOnlyFrm.bTDisplayDoubleClick(Sender: TObject);
// ---------------------------
// Double A/D display duration
// ---------------------------
begin
     edTDisplay.Value := edTDisplay.Value*2.0 ;
     MainFrm.ADCDisplayWindow := edTDisplay.Value ;
     if MainFrm.Recording then
     begin
       OldTScale := scADCDisplay.TScale;
       RecordingFirstTrace := True;
     end;
     SetDisplayUnits ;
     ResetDisplays := True ;
     end;


procedure TRecADCOnlyFrm.FormDeactivate(Sender: TObject);
// -------------------
// Form has lost focus
// -------------------
begin
     if not Mainfrm.Recording then StopADC ;
     end;


procedure TRecADCOnlyFrm.edRecordingTimeKeyPress(Sender: TObject;
  var Key: Char);
// --------------------------
// Recording time key pressed
// --------------------------
begin
     if Key = #13 then begin
        MainFrm.ADCRecordingTime := edRecordingTime.Value ;
        startadc ;
        end ;
     end;

procedure TRecADCOnlyFrm.ckFixZeroLevelsClick(Sender: TObject);
// -------------------------------------
// Fix display zero level option changed
// -------------------------------------
begin
     scADCDisplay.FixZeroLevels := ckFixZeroLevels.Checked ;
     scADCDisplay.Invalidate ;
     end;


procedure TRecADCOnlyFrm.ZoomOutAll ;
// --------------------------------------
// Set all plots to minimum magnification
// --------------------------------------
begin

    // Set A/D plots
    scADCDisplay.ZoomOut ;

    end ;

procedure TRecADCOnlyFrm.ckPhotoStimEnabledClick(Sender: TObject);
begin
      MainFrm.PhotoStim.Enabled := ckPhotoStimEnabled.Checked ;
      end;


procedure TRecADCOnlyFrm.ckDynamicProtocolEnabledClick(Sender: TObject);
begin
      MainFrm.DynamicProtocol.Enabled := ckDynamicProtocolEnabled.Checked ;
      end;

procedure TRecADCOnlyFrm.bDynamicProtocolSetupClick(Sender: TObject);
begin

    if MainFrm.FormExists( 'DynamicProtocolSetupFrm' ) then DynamicProtocolSetupFrm.SetFocus
    else begin
       DynamicProtocolSetupFrm := TDynamicProtocolSetupFrm.Create(Self) ;
       DynamicProtocolSetupFrm.Left := 10 ;
       DynamicProtocolSetupFrm.Top := 10 ;
       end ;

end;

procedure TRecADCOnlyFrm.ckPlaybackEnabledClick(Sender: TObject);
begin

      MainFrm.Playback.Enabled := ckPlaybackEnabled.Checked;

      if ckPlaybackEnabled.Checked then
      begin
        ckDynamicProtocolEnabled.Checked := False;
        ckDynamicProtocolEnabled.Enabled := False;
        MainFrm.DynamicProtocol.Enabled := False;
        bDynamicProtocolSetup.Enabled := False;
        cbStimProgram.Enabled := False;
      end
      else
      begin
        ckDynamicProtocolEnabled.Enabled := True;
        bDynamicProtocolSetup.Enabled := True;
        cbStimProgram.Enabled := True;
      end;

      end;

procedure TRecADCOnlyFrm.bPlaybackSetupClick(Sender: TObject);
begin

    if MainFrm.FormExists( 'PlaybackSetupFrm' ) then PlaybackSetupFrm.SetFocus
    else begin
       PlaybackSetupFrm := TPlaybackSetupFrm.Create(Self) ;
       PlaybackSetupFrm.Left := 10 ;
       PlaybackSetupFrm.Top := 10 ;
       end ;

end;

procedure TRecADCOnlyFrm.cbPhotoStimProgramChange(Sender: TObject);
begin

  // Photo-stimulus protocol file name
  MainFrm.PhotoStimFileName :=
    Mainfrm.PProtDirectory + cbPhotoStimProgram.Text + '.ppr';

end;

procedure TRecADCOnlyFrm.Wait( Delay : Single ) ;
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



procedure TRecADCOnlyFrm.ckRecordADCSignalsOnlyClick(Sender: TObject);
begin
  MainFrm.RecordADCSignalsOnly := ckRecordADCSignalsOnly.Checked;
end;

procedure TRecADCOnlyFrm.ToggleVoltageStimulus;
begin
  if MainFrm.PhotoStim.Enabled then
  begin
    ckPhotoStimEnabled.Checked := False;
    bStartStimulus.Click;
    bStopStimulus.Click;
    ckPhotoStimEnabled.Checked := True;
  end else
  begin
    bStartStimulus.Click;
    bStopStimulus.Click;
  end;
end;

end.
