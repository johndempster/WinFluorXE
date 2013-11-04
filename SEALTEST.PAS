unit Sealtest;
{ ==================================================
  WinFluor - Patch clamp pipette seal test module
  (c) J. Dempster, University of Strathclyde 1996-97
  3/7/02 ... Modified from WinEDR V2.3.2
  19/6/03 ... Amplifier gain changes during seal test no longer
              affect ADCAmplifierGain settings of sdata files
  23/03/04 .. Floating point exceptions now longer halt execution
  25/05/04 .. No longer needs DAC1 -> Ext Trigger link (free run option removed)
              voltage now automatically starts when seal test window displayed
  19/07/04 .. Current and voltage channel selections now set automatically
  10/08/04 .. Channel labels now updated correctly when current/voltage clamp mode changes
  16/08/04 .. StopDAC commands all now refer to DACDevice
  18/02/05 .. SealTestRunning flag added
  17/03/05 .. Now works with more than 2 channels
  01.09.05 ..
  09.09.05 .. User can enter amplifier gain when Amplifier = None
  05.12.05 .. Capacity now computed using initial amplitude of fitted exponential
  24.05.06 .. Display autoscale now works correctly when Gm=0
  15.06.06 .. End of sweep detection now improved to handel off-limits channels better
  20.06.06 .. Command voltage 1 channel now maintained at holding voltage
  21.06.06 .. Automatic display scaling now has independent magnifications for each channel
  23.06.06 .. All DAC output channels on all boards now updated by seal test
              (Avoids loss of interboard timing on DW's Radiance with PCI-6014 board)
  17.08.06 .. Seal test holding and pulse voltage settings now preserved
              Seal trace now not erased until next waveform available
  18.07.07 .. Display can now be scaled using zoom buttons
              Pulse is now only applied to select Vout channel
  15.08.07 .. Divide factor for selected voltage output now shown correctly
  11.01.08 .. DAC output voltages on channels not used for stimulus
              now maintained at existing values
  20.06.08 .. Support for 2 amplifiers added
  04.03.09 .. Command voltage outputs now updated correctly when seal test window
              deactivated. 0V pulse no longer produced on VC 0 when VC 1 allocated
  16.03.09 .. JD FP error when 2 amplifiers but only 2 A/D channels selected now avoided
              by increasing min. required channels in seal test window for 2 amplifiers
  15.09.09 .. JD Timeout inserted which restarts test pulse if sweep fails to complete
              Temporary fix for problem at Sept. Plymouth 2009
  08.07.10 .. JD Empty flag samples remaining at end of buffer when buffer timeout occurs
              now set to last valid value for each channel in buffer (fixes problem with PCI-61XX
              cards which collect 4 samples too few when using external clock triggering)
  ==================================================}

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls, shared, SysUtils, Spin, maths, dialogs,
  ValEdit, ScopeDisplay, ComCtrls, ValidatedEdit, IDRFile, LabIOUnit, Math ;

type

  TSealTest = record
        Use : Integer ;
        AutoScale : Boolean ;
        FreeRun : Boolean ;
        FirstSweep : Boolean ;
        DisplayScale : Integer ;
        PulseHeight1 : Single ;
        PulseHeight2 : Single ;
        PulseHeight3 : Single ;
        PulseHeight : Single ;
        PulseWidth : Single ;
        HoldingVoltage2 : Single ;
        HoldingVoltage3 : Single ;
        HoldingVoltage1 : Single  ;
        VoltageChannel : Integer ;
        CurrentChannel : Integer ;
        end ;

  TState = ( Idle, StartSweep, SweepInProgress, EndofSweep ) ;
  TSealTestFrm = class(TForm)
    ChannelsGrp: TGroupBox;
    cbCurrentChannel: TComboBox;
    Label1: TLabel;
    cbVoltageChannel: TComboBox;
    Label2: TLabel;
    VoltsGrp: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    CurrentGrp: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    CellGrp: TGroupBox;
    Timer: TTimer;
    DACGrp: TGroupBox;
    rbUseHoldingVoltage1: TRadioButton;
    rbUseHoldingVoltage2: TRadioButton;
    Label3: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label4: TLabel;
    rbUseHoldingVoltage3: TRadioButton;
    Label13: TLabel;
    Shape1: TShape;
    edHoldingVoltage1: TValidatedEdit;
    edPulseHeight1: TValidatedEdit;
    edHoldingVoltage2: TValidatedEdit;
    edPulseHeight2: TValidatedEdit;
    edHoldingVoltage3: TValidatedEdit;
    edPulseWidth: TValidatedEdit;
    edVHold: TValidatedEdit;
    edVPulse: TValidatedEdit;
    edIHold: TValidatedEdit;
    edIPulse: TValidatedEdit;
    PageControl1: TPageControl;
    PipetteTab: TTabSheet;
    Label9: TLabel;
    edResistance: TValidatedEdit;
    TabSheet1: TTabSheet;
    Label15: TLabel;
    Label17: TLabel;
    Label16: TLabel;
    edGaccess: TValidatedEdit;
    edGmembrane: TValidatedEdit;
    edCmembrane: TValidatedEdit;
    TimerGrp: TGroupBox;
    edTimer: TEdit;
    bResetTimer: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    rbGaFromPeak: TRadioButton;
    rbGaFromExp: TRadioButton;
    ckAutoScale: TCheckBox;
    AmplifierGrp: TGroupBox;
    lbAmplifier1: TLabel;
    lbAmplifier2: TLabel;
    edAmplifier1Gain: TValidatedEdit;
    DivideGrp: TGroupBox;
    Label18: TLabel;
    Label19: TLabel;
    edVDivide0: TValidatedEdit;
    edVDivide1: TValidatedEdit;
    edAmplifier2Gain: TValidatedEdit;
    Shape2: TShape;
    Label10: TLabel;
    cbDACOut: TComboBox;
    rbAmplifier1: TRadioButton;
    rbAmplifier2: TRadioButton;
    Label20: TLabel;
    edPulseHeight3: TValidatedEdit;
    scDisplay: TScopeDisplay;
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edHoldingVoltage1KeyPress(Sender: TObject; var Key: Char);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edHoldingVoltage2KeyPress(Sender: TObject; var Key: Char);
    procedure rbUseHoldingVoltage1Click(Sender: TObject);
    procedure rbUseHoldingVoltage2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure edPulseHeight1KeyPress(Sender: TObject; var Key: Char);
    procedure edPulseheight2KeyPress(Sender: TObject; var Key: Char);
    procedure edPulseWidthKeyPress(Sender: TObject; var Key: Char);
    procedure rbUseHoldingVoltage3Click(Sender: TObject);
    procedure EdHoldingVoltage3KeyPress(Sender: TObject; var Key: Char);
    procedure ckAutoScaleClick(Sender: TObject);
    procedure bResetTimerClick(Sender: TObject);
    procedure cbCurrentChannelChange(Sender: TObject);
    procedure edVDivide0KeyPress(Sender: TObject; var Key: Char);
    procedure edVDivide1KeyPress(Sender: TObject; var Key: Char);
    procedure rbAmplifier1Click(Sender: TObject);
    procedure rbAmplifier2Click(Sender: TObject);
    procedure edAmplifier1GainKeyPress(Sender: TObject; var Key: Char);
    procedure edAmplifier2GainKeyPress(Sender: TObject; var Key: Char);
    procedure edPulseHeight3KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    ADCDevice : SmallInt ;                    // Device # of A/D Converter
    DeviceInUse : Array[1..MaxDevices] of Boolean ; // I/O devices in use
    ADC : Array[0..1024*8-1] of SmallInt ;
    DACBufs : Array[0..MaxDevices] of PBig16bitArray ;
    ADCSamplingInterval : Double ;
    ResetReadouts : Boolean ;
    EndOfSweepCount : Integer ;
    TTimeOut : Integer ;
    EmptyFlags : Array[0..1] of Integer ;

    NewAmplifier1GainValue : Boolean ;
    NewAmplifier2GainValue : Boolean ;
    SetAmplifier1GainReadout : Boolean ;
    SetAmplifier2GainReadout : Boolean ;

    ReadPulseSettings : Boolean ;

    procedure CreateTestPulse ;
    procedure AnalyseTestPulse ;
    procedure UpdateChannelScalingFactors ;
    procedure UpdateChannelLists ;
    function FindTimingDevice : Smallint ;
    procedure StopADCandDAC ;
    procedure InitialiseDisplay ;
    function GetDisplayGrid : Boolean ;
    procedure SetDisplayGrid( Value : Boolean ) ;
    procedure SelectAmplifier( AmpNumber : Integer ) ;


  public
    { Public declarations }
    State : TState ;
    SealTestRunning : Boolean ;
    procedure StopSealTest ;
    procedure ChangeDisplayGrid ;
    procedure ZoomOutAll ;
    Property DisplayGrid : Boolean read GetDisplayGrid write SetDisplayGrid ;
  end;

var
  SealTestFrm: TSealTestFrm;

implementation

{$R *.DFM}

uses Main, AmpModule, mmsystem, SetupUnit, RecUnit, RecADCOnlyUnit ;

const
     StimDACChannel = 0 ;
     SyncDACChannel = 1 ;
     NumDACChannels = 2 ;
     NumTestSamples = 512 ;
     MinDACInterval = 0.0002 ;
     MinPulseWidth = 0.0001 ;
     MaxPulseWidth = 1.0 ;
     MinTestChannels = 2 ;
     ADCMinSamplingInterval = 2E-5 ;
type
    TTestPulse = record
               Duration : single ;
               TStart : single ;
               TEnd : Single ;
               RecordLength : single ;
               RepeatPeriod : single ;
               end ;
var
  EndOfBuf : Integer ;
  MinFormHeight : Integer ; { Minimum height allowed for form }
  MinFormWidth : Integer ;  { Minimum width allowed for form }

  NumTestChannels : Integer ;
  TestChannel : array[0..7] of TChannel ;
  TestPulse : TTestPulse ;
  TimerBusy : boolean ;
  TimerStarted : Integer ;


procedure TSealTestFrm.FormShow(Sender: TObject);
{ ----------------
  Initialise form
  ---------------}
var
   ch,iDev,VChan : Integer ;
begin

     Timer.Enabled := False ;
     SealTestRunning := False ;
     EmptyFlags[0] := 32767 ;
     EmptyFlags[1] := -EmptyFlags[0] ;

     { Disable Record to disk option in Record menu }
     MainFrm.mnSealTest.enabled := false ;

     // Exit if no A/D converter hardware
     if (MainFrm.IOConfig.ADCIn < 0) or
        (MainFrm.IOConfig.ADCIn > MaxResources) then begin
        ShowMessage( 'No analogue input channels defined (see Camera Setup)!') ;
        Close ;
        Exit ;
        end ;
     // Get A/D converter device #
     ADCDevice := LabIO.Resource[MainFrm.IOConfig.ADCIn].Device ;

     // Exit if no D/A channel configured
     if (MainFrm.IOConfig.VCommand[0] < 0) or
        (MainFrm.IOConfig.VCommand[0] > MaxResources) then begin
        ShowMessage('No patch clamp voltage output defined (see Camera Setup)!') ;
        Close ;
        Exit ;
        end ;

     { Stop laboratory interface activity }
     for iDev := 1 to LabIO.NumDevices do begin
         LabIO.StopADC(iDev) ;
         LabIO.StopDAC(iDev) ;
         end ;

     // Allocate D/A waveform buffers
     for iDev := 1 to LabIO.NumDevices do begin
         DACBufs[iDev] := Nil ;
         if LabIO.NumDACs[iDev] > 0 then begin
            GetMem( DACBufs[iDev],NumTestSamples*LabIO.NumDACs[iDev]*2 ) ;
            end ;
         end ;

     // Determine available voltage output channels
     cbDACOut.Clear ;
     for VChan := 0 to NumCommandVoltageChannels-1 do begin
         if MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[VChan]) then begin
            cbDACOut.Items.Add(format('VOut %d',[VChan])) ;

            // Get I/O device properties (skip if not set up)
            iDev := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device ;
            DeviceInUse[iDev] := True ;
            end ;
         end ;
     if cbDACOut.Items.Count >0 then cbDACOut.ItemIndex := 0 ;

     MinFormHeight := Height ;
     MinFormWidth := Width ;

     if Amplifier.GainTelegraphAvailable[2] then begin
        NumTestChannels := Max( MainFrm.ADCNumChannels,MinTestChannels*2 ) ;
        end
     else begin
        NumTestChannels := Max( MainFrm.ADCNumChannels,MinTestChannels ) ;
        end ;
     scDisplay.ClearHorizontalCursors ;
     scDisplay.MaxADCValue := LabIO.ADCMaxValue[ADCDevice] ;
     scDisplay.MinADCValue := -LabIO.ADCMaxValue[ADCDevice] -1 ;
     scDisplay.NumChannels := NumTestChannels ;
     for ch := 0 to NumTestChannels-1 do begin
         TestChannel[ch] := MainFrm.ADCChannel[ch] ;
         TestChannel[ch].xMin := 0. ;
         TestChannel[ch].xMax := NumTestSamples ;
         TestChannel[ch].yMin := scDisplay.MinADCValue ;
         TestChannel[ch].yMax := scDisplay.MaxADCValue ;
         TestChannel[ch].InUse := True ;
         TestChannel[ch].ADCZero := 0 ;
         { Create horizontal cursors }
         scDisplay.AddHorizontalCursor(ch,clBlue,True, 'z') ;
         end ;

{     edAmplifier1Gain.Units := 'V/' + TestChannel[1].ADCUnits ;
     edAmplifier1Gain.Value := TestChannel[1].ADCCalibrationFactor ;
     edAmplifier2Gain.Units := 'V/' + TestChannel[1].ADCUnits ;
     edAmplifier2Gain.Value := TestChannel[1].ADCCalibrationFactor ;}

     scDisplay.xMin := 0 ;
     scDisplay.xMax := NumTestSamples-1  ;

     scDisplay.TUnits := 'ms' ;

     { Set current channel selection combo boxes }
     UpdateChannelLists ;

     { Test pulse amplitude & width }
     edPulseHeight1.Value := MainFrm.SealTest.PulseHeight1 ;
     edPulseHeight2.Value := MainFrm.SealTest.PulseHeight2 ;
     edPulseHeight3.Value := MainFrm.SealTest.PulseHeight3 ;
     edPulseWidth.Value := MainFrm.SealTest.PulseWidth ;

     { Update holding potential text boxes }
     edHoldingVoltage1.Value := MainFrm.SealTest.HoldingVoltage1 ;
     edHoldingVoltage2.Value := MainFrm.SealTest.HoldingVoltage2 ;
     edHoldingVoltage3.Value := MainFrm.SealTest.HoldingVoltage3 ;
     edVDivide0.Value := Mainfrm.VCommand[0].DivideFactor ;
     edVDivide1.Value := Mainfrm.VCommand[1].DivideFactor ;

     ckAutoScale.checked := MainFrm.SealTest.AutoScale ;

     // Initialise elapsed time readout
     TimerStarted := TimeGetTime ;

     Timer.Enabled := True ;
     TimerBusy := False ;
     ResetReadouts := True ;
     MainFrm.SealTest.FirstSweep := True ;
     SetAmplifier1GainReadout := True ;
     SetAmplifier2GainReadout := True ;

     if Amplifier.AmplifierType[2] = amNone then begin
        lbAmplifier2.Visible := False ;
        edAmplifier2Gain.Visible := False ;
        end
     else begin
        lbAmplifier2.Visible := True ;
        edAmplifier2Gain.Visible := True ;
        end ;

     ReadPulseSettings := True ;
     State := StartSweep ;

     // Clear display
     InitialiseDisplay ;

     // Make all channels visible
     // Modified by NS 24 March 2009
     for ch := 0 to scDisplay.NumChannels-1 do
        scDisplay.ChanVisible[ch] := MainFrm.ADCChannelSealtestVisible[ch] ;


     ClientWidth := CellGrp.Left + CellGrp.Width + 5 ;
     ClientHeight := CellGrp.Top + CellGrp.Height + 5 ;

     Resize ;

     end ;


procedure TSealTestFrm.UpdateChannelLists ;
// --------------------------------------
// Update current & voltage channel lists
// --------------------------------------
var
    ch : Integer ;
begin

     { Fill current & voltage channel selection combo boxes }
     cbCurrentChannel.Clear ;
     cbVoltageChannel.Clear ;
     for ch := 0 to NumTestChannels-1 do begin
         cbCurrentChannel.Items.Add(format('Ch.%d %s',[ch,TestChannel[ch].ADCName]));
         cbVoltageChannel.Items.Add(format('Ch.%d %s',[ch,TestChannel[ch].ADCName]));
         end ;
     cbCurrentChannel.ItemIndex := 0 ;
     cbVoltageChannel.ItemIndex := Max(1,NumTestChannels-1) ;

     // Set
     for ch := NumTestChannels-1 downto 0 do begin
         // Current channel (units = pA,nA,uA)
         if Pos('A',UpperCase(TestChannel[ch].ADCUnits)) > 0 then
            cbCurrentChannel.ItemIndex := ch ;
         // Voltage channel (units= mV)
         if Pos('V',UpperCase(TestChannel[ch].ADCUnits)) > 0 then
            cbVoltageChannel.ItemIndex := ch ;
         end ;

     end ;


procedure TSealTestFrm.TimerTimer(Sender: TObject);
{ ---------------------
  Timed Event scheduler
  ---------------------}

var
   i,j,ch,T : Integer ;
   OldADCUnits : Array[0..ChannelLimit] of String ;
   Changed : Boolean ;
   Dev : Integer ;
   IChannel : Integer ;
   TimedOut : Boolean ;
begin
     if not TimerBusy then begin
          TimerBusy := True ;
          case State of

               StartSweep : Begin

                   { Start recording sweep(s) }

                   if ReadPulseSettings then begin
                      if rbUseHoldingVoltage1.checked then begin
                         Mainfrm.VCommand[cbDACOut.ItemIndex].HoldingVoltage := edHoldingVoltage1.Value ;
                         MainFrm.SealTest.PulseHeight := edPulseHeight1.Value ;
                         end
                      else if rbUseHoldingVoltage2.checked then begin
                         Mainfrm.VCommand[cbDACOut.ItemIndex].HoldingVoltage := edHoldingVoltage2.Value ;
                         MainFrm.SealTest.PulseHeight := edPulseHeight2.Value ;
                         end
                      else if rbUseHoldingVoltage3.checked then begin
                         Mainfrm.VCommand[cbDACOut.ItemIndex].HoldingVoltage := edHoldingVoltage3.Value ;
                         MainFrm.SealTest.PulseHeight := edPulseHeight3.Value ;
                         end ;
                      MainFrm.SealTest.PulseWidth := edPulseWidth.Value ;
                      ReadPulseSettings := False ;
                      ResetReadouts := True ;
                      end ;

                   ResetReadouts := True ;


                   // Save old channel units
                   for ch := 0 to NumTestChannels-1 do
                       OldADCUnits[ch] := TestChannel[ch].ADCUnits ;

                   // Update channel scaling factors in case amplifier gain has changed
                   for ch := 0 to NumTestChannels-1 do begin
                       Amplifier.GetChannelSettings( ch,
                                                     TestChannel[ch].ADCName,
                                                     TestChannel[ch].ADCUnits,
                                                     TestChannel[ch].ADCCalibrationFactor,
                                                     TestChannel[ch].ADCAmplifierGain ) ;
                       end ;

                   UpdateChannelScalingFactors ;

                   // If units have changed, update current/voltage channels
                   Changed := False ;
                   for ch := 0 to NumTestChannels-1 do
                       if OldADCUnits[ch] <> TestChannel[ch].ADCUnits then Changed := True ;
                   if Changed then UpdateChannelLists ;

                   if cbCurrentChannel.ItemIndex >= 0 then
                      MainFrm.SealTest.CurrentChannel := cbCurrentChannel.ItemIndex ;
                   if cbVoltageChannel.ItemIndex >= 0 then
                      MainFrm.SealTest.VoltageChannel := cbVoltageChannel.ItemIndex ;

                   // Set patch clamp command voltage divide factor
                   Amplifier.GetCommandVoltageDivideFactor(1,Mainfrm.VCommand[0].DivideFactor) ;
                   Amplifier.GetCommandVoltageDivideFactor(2,Mainfrm.VCommand[1].DivideFactor) ;

                   // Update Amplifier #1 gain display

                   IChannel := Amplifier.CurrentChannel( 1 ) ;
                   lbAmplifier1.Caption := 'Amp#1 Gain (Ch.0)' ;
                   if Amplifier.GainTelegraphAvailable[1] then begin
                      // Display current amplifier gain

                      edAmplifier1Gain.Units := 'V/' + TestChannel[IChannel].ADCUnits ;
                      edAmplifier1Gain.Value := Abs(MainFrm.ADCVoltageRange) /
                                                 (TestChannel[IChannel].ADCScale
                                                *(LabIO.ADCMaxValue[ADCDevice]+1) ) ;
                      end
                   else begin
                      // Let user type in gain
                     if SetAmplifier1GainReadout then begin
                        edAmplifier1Gain.Units := 'V/' + TestChannel[IChannel].ADCUnits ;
                        edAmplifier1Gain.Value := TestChannel[IChannel].ADCCalibrationFactor ;
                        SetAmplifier1GainReadout := False ;
                        end ;
                     if NewAmplifier1GainValue then begin
                        // Gain factor cannot be manually changed if records have been collected
                        // or zero or negative value entered
                        if edAmplifier1Gain.Value <= 0.0 then begin
                           edAmplifier1Gain.Value := TestChannel[IChannel].ADCCalibrationFactor ;
                           end ;
                        TestChannel[IChannel].ADCCalibrationFactor := edAmplifier1Gain.Value ;
                        MainFrm.ADCChannel[IChannel].ADCCalibrationFactor := edAmplifier1Gain.Value ;
                        NewAmplifier1GainValue := False ;
                        end ;
                     end ;

                   // Update Amplifier #2 gain display

                   IChannel := Amplifier.CurrentChannel( 2 ) ;
                   lbAmplifier2.Caption := 'Amp#2 Gain (Ch.2)' ;
                   if Amplifier.GainTelegraphAvailable[2] then begin
                      // Display current amplifier gain

                      edAmplifier2Gain.Units := 'V/' + TestChannel[IChannel].ADCUnits ;
                      edAmplifier2Gain.Value := Abs(MainFrm.ADCVoltageRange) /
                                                 (TestChannel[IChannel].ADCScale
                                                *(LabIO.ADCMaxValue[ADCDevice]+1) ) ;
                      end
                   else begin
                      // Let user type in gain
                     if SetAmplifier2GainReadout then begin
                        edAmplifier2Gain.Units := 'V/' + TestChannel[IChannel].ADCUnits ;
                        edAmplifier2Gain.Value := TestChannel[IChannel].ADCCalibrationFactor ;
                        SetAmplifier2GainReadout := False ;
                        end ;
                     if NewAmplifier2GainValue then begin
                        // Gain factor cannot be manually changed if records have been collected
                        // or zero or negative value entered
                        if edAmplifier2Gain.Value <= 0.0 then begin
                           edAmplifier2Gain.Value := TestChannel[IChannel].ADCCalibrationFactor ;
                           end ;
                        TestChannel[IChannel].ADCCalibrationFactor := edAmplifier2Gain.Value ;
                        MainFrm.ADCChannel[IChannel].ADCCalibrationFactor := edAmplifier2Gain.Value ;
                        NewAmplifier2GainValue := False ;
                        end ;
                     end ;

                   { A/D sampling interval }
                   EndofBuf := (NumTestChannels*NumTestSamples) - 1;
                   // Stop any existing A/D sweep
                   LabIO.StopADC(ADCDevice) ;

                   // Create test pulse waveform
                   CreateTestPulse ;

                   // Fill circular buffer with empty flags
                   i := 0 ;
                   for i := 0 to EndofBuf do begin
                      ADC[i] := EmptyFlags[i and 1] ;
                      end ;

                   // Start A/D sampling (triggered by DAC updates)
                   LabIO.ADCToMemoryExtScan( ADCDevice,
                                       ADC,
                                       NumTestChannels,
                                       NumTestSamples,
                                       MainFrm.ADCVoltageRange,
                                       False,
                                       FindTimingDevice,
                                       True ) ;

                   // Set up D/A output sweep
                   for Dev := 1 to LabIO.NumDevices do if DeviceInUse[Dev] then begin
                       LabIO.StopDAC(Dev) ;
                       LabIO.MemoryToDAC( Dev,
                                          DACBufs[Dev]^,
                                          LabIO.NumDACs[Dev],
                                          NumTestSamples,
                                          ADCSamplingInterval,
                                          False,
                                          False,
                                          FindTimingDevice ) ;
                       end ;

                   State := SweepInProgress ;
                   SealTestRunning := True ;
                   EndOfSweepCount := 0 ;
                   // Set time out (to restart if sweep not completed)
                   TTimeOut := TimeGetTime +
                               Max(Round(NumTestSamples*ADCSamplingInterval*2000),100) ;
                   End ;

               SweepInProgress : Begin
                   { Erase display & Draw horizontal zero level cursor(s) }
                   LabIO.UpdateDACOutputBuffer ;
                   LabIO.GetADCSamples( ADCDevice, ADC ) ;
                   outputdebugstring(pchar(format('%d %d',[TimeGetTime,TTimeOut]))) ;
                   // if all samples have been acquired, display on screen
                   if ADC[EndOfBuf] <> EmptyFlags[EndOfBuf and 1] then Inc(EndOfSweepCount) ;
                   if ADC[EndOfBuf-1] <> EmptyFlags[(EndOfBuf-1) and 1] then Inc(EndOfSweepCount) ;
                   if TimeGetTime > TTimeOut then TimedOut := True
                                             else TimedOut := False ;
                   if (EndOfSweepCount >= 2) or TimedOut then begin
                      if TimedOut then begin
                         // If timed out clear empty flags left in buffer
                         for i := 1 to NumTestSamples-1 do begin
                             for ch := 0 to NumTestChannels-1 do begin
                                j := i*NumTestChannels + ch ;
                                if ADC[j] = EmptyFlags[j and 1] then begin
                                   ADC[j] := ADC[j- NumTestChannels]  ;
                                   end ;
                                end ;
                             end ;
                         end ;
                      // Clear display
                      InitialiseDisplay ;
                      scDisplay.DisplayNewPoints( NumTestSamples ) ;
                      StopADCandDAC ;
                      State := EndOfSweep ;
                      end ;

                   { Procedure to be done when recording sweep completes }
                   if State = EndOfSweep then begin
                       StopADCandDAC ;
                       { Analyse and display test pulse results }
                       AnalyseTestPulse ;
                       { Do another sweep }
                       State := StartSweep ;
                       MainFrm.SealTest.FirstSweep := False ;
                       end ;

                   end ;

               Idle : begin
                    { Procedures when recording is in idle mode }
                    end ;
               end ;

          //Update elapsed time readout
          T := (TimeGetTime - TimerStarted) div 1000 ;
          edTimer.Text := format( ' %.2d:%.2d',[T div 60, T mod 60]) ;


          TimerBusy := False ;
          end ;
     end ;


procedure TSealTestFrm.InitialiseDisplay ;
// ---------------------------------
// Setup oscilloscope display window
// ---------------------------------
var
    ch : Integer ;
begin

    scDisplay.MaxPoints := NumTestSamples ;
    scDisplay.NumPoints := 0 ;
    scDisplay.NumChannels := NumTestChannels ;
    if ADCSamplingInterval > 0.0 then
       scDisplay.xMax := Round(TestPulse.RecordLength/ADCSamplingInterval)-1
    else scDisplay.xMax := NumTestSamples-1 ;
    scDisplay.xMin := 0 ;

    { Set channel information }
    for ch := 0 to NumTestChannels-1 do begin
        TestChannel[ch].ChannelOffset := LabIO.ADCChannelOffsets[ch] ;
        scDisplay.ChanOffsets[ch] := LabIO.ADCChannelOffsets[ch] ;
        scDisplay.ChanUnits[ch] := TestChannel[Ch].ADCUnits ;
        scDisplay.ChanScale[ch] := TestChannel[Ch].ADCScale ;
        scDisplay.ChanName[ch] := TestChannel[Ch].ADCName ;
        if ckAutoScale.Checked or MainFrm.SealTest.FirstSweep then begin
           scDisplay.yMin[ch] := TestChannel[Ch].yMin ;
           scDisplay.yMax[ch] := TestChannel[Ch].yMax ;
           end ;
        scDisplay.HorizontalCursors[ch] := TestChannel[ch].ADCZero ;

        end ;
    scDisplay.TScale := ADCSamplingInterval*1000.0 ;
    scDisplay.SetDataBuf( @ADC ) ;

    end ;


procedure TSealTestFrm.CreateTestPulse ;
{ ----------------------------
  Create test pulse waveform
  ----------------------------}
var
   i,j,ch,iStart,iEnd,VChan : Integer ;
   DACChannel,DACNumChannels : Integer ;
   DACScale : Single ;
   DACValue : Integer ;
   Dev : Integer ;
begin

     { Test pulse duration and recording sweep length }
     TestPulse.Duration := MainFrm.SealTest.PulseWidth ;
     TestPulse.TStart := Max(TestPulse.Duration/6.0,0.005) ;
     TestPulse.TEnd := TestPulse.TStart + TestPulse.Duration ;
     TestPulse.RecordLength := TestPulse.TEnd + TestPulse.TStart ;

     { Test pulse repeat period }
     TestPulse.RepeatPeriod := MaxFlt([TestPulse.RecordLength,0.1] ) ;

     ADCSamplingInterval := TestPulse.RecordLength / NumTestSamples ;
     ADCSamplingInterval := Max(ADCSamplingInterval,(NumTestChannels+2)*ADCMinSamplingInterval) ;

     { Create test pulse waveform }
     iStart := Max(Round(TestPulse.TStart/ADCSamplingInterval),1) ;
     iEnd := Max(Round(TestPulse.TEnd/ADCSamplingInterval),1) ;

     // Initialise DAC outputs to current default DAC states
     for Dev := 1 to LabIO.NumDevices do begin
         DeviceInUse[Dev] := False ;
         for ch := 0 to LabIO.NumDACS[Dev]-1 do begin
             j := ch ;
             DACValue := Round( LabIO.DACOutState[Dev,ch]*LabIO.DACScale[Dev] ) ;
             for i := 0 to NumTestSamples-1 do begin
                 DACBufs[Dev]^[j] := DACValue ;
                 j := j + LabIO.NumDACS[Dev] ;
                 end ;
            end ;
         end ;

     // Update command voltage channels
     // (Command voltage channel used for patch clamp)

     for VChan := 0 to NumCommandVoltageChannels-1 do begin

         // Skip if channel is not configured
         if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[VChan]) then Continue ;

         // Get I/O device properties (skip if not set up)
         Dev := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device ;
         if DACBufs[Dev] = Nil then Continue ;
         DeviceInUse[Dev] := True ;

         // DAC output channel and no. of channels on device
         DACChannel := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].StartChannel ;
         DACNumChannels := LabIO.NumDACs[Dev] ;

         // D/A scaling factor
         DACScale := (LabIO.DACMaxValue[Dev]*MainFrm.VCommand[VChan].DivideFactor)/
                      LabIO.DACMaxVolts[Dev] ;

         // Initialise buffer to holding potential
         j := DACChannel ;
         DACValue := Min(LabIO.DACMaxValue[Dev], Max(LabIO.DACMinValue[Dev],
                     Round(MainFrm.VCommand[VChan].HoldingVoltage*DACScale))) ;
         for i := 0 to NumTestSamples-1 do begin
             DACBufs[Dev]^[j] := DACValue ;
             j := j + DACNumChannels ;
             end ;

         // Apply test pulse for selected channel
         if VChan = cbDACOut.ItemIndex then begin
            DACValue := Min(LabIO.DACMaxValue[Dev], Max(LabIO.DACMinValue[Dev],
                        Round( (MainFrm.VCommand[VChan].HoldingVoltage +
                                MainFrm.SealTest.PulseHeight)*DACScale) )) ;
            j := iStart*DACNumChannels + DACChannel ;
            for i := iStart to iEnd do begin
                DACBufs[Dev]^[j] := DACValue ;
                j := j + DACNumChannels ;
                end ;
            end ;

         end ;

     end ;


procedure TSealTestFrm.AnalyseTestPulse ;
{ --------------------------------------------------------------------
  Calculate and display seal test pulse amplitudes and seal properties
  --------------------------------------------------------------------}
const
    LoLimit = 1E-15 ;
    LoVoltageLimit = 1E-6 ;  // Smallest valid test voltage (1uV)
    LoCurrentLimit = 1E-13 ; // Smallest valid current (0.1 pA)

var
   i,j,ch,iStart,iEnd,n : Integer ;
   iAvgStart,iAvgEnd,nAvg,EndofVHold,NumSamplesIn : Integer ;
   ChIm,ChVm,ChOffset,yADC : Integer ;
   HoldLevel,VThreshold,iOffLevel,iOnLevel,PeakAt : Integer ;
   YMin,YMax : Array[0..ChannelLimit] of Integer ;
   VHold,VPulse,IHold,IPulse,Avg,Sum,IPeak,IValue : single ;
   Voltage,SteadyCurrent,PeakCurrent,VScale,IScale : single ;
   TauC,Slope,YIntercept,SteadyStateLevel,GMembrane,GAccess,Capacity,dt : single ;
   Resistance : Single ;
   x,y : Array[0..NumTestSamples] of Single ;
   OK,Done : Boolean ;
   YAdd,YMid,YUpperLimit,YLowerLimit,YHalfRange : Integer ;
begin

     try

     ChIm := cbCurrentChannel.ItemIndex ;
     ChVm := cbVoltageChannel.ItemIndex ;

     { Find Min./Max. limits of voltage and current }
     for ch := 0 to NumTestChannels-1 do begin
         YMax[ch] := -LabIO.ADCMaxValue[ADCDevice] -1 ;
         YMin[ch] := LabIO.ADCMaxValue[ADCDevice] ;
         j := TestChannel[ch].ChannelOffset ;
         // Last few samples in data buffer collected with 6110 device have
         // EmptyFlag values, which disturb auto-scaling and pulse average.
         // 7/1/10
         //for i := 0 to NumTestSamples-1 do begin
         for i := 0 to NumTestSamples-1 do begin
             yADC := ADC[j] ;
             if YMax[ch] <= yADC then YMax[ch] := yADC ;
             if YMin[ch] >= yADC then YMin[ch] := yADC ;
             j := j + NumTestChannels ;
             end ;
         end ;

     { Calculate holding voltage }

     Sum := 0. ;
     EndofVHold := Max(Round(TestPulse.TStart/(2.0*ADCSamplingInterval)),1) ;
     for i := 0 to EndofVHold do begin
         j := i*NumTestChannels + TestChannel[chVm].ChannelOffset ;
         Sum := Sum + ADC[j] ;
         end ;
     Avg := Sum/(EndofVHold+1) ;
     HoldLevel := Round(Avg) ;
     VHold := (Avg - TestChannel[ChVm].ADCZero)*TestChannel[ChVm].ADCScale ;

     { Calculate holding current }
     Sum := 0. ;
     for i := 0 to EndofVHold do begin
         j := i*NumTestChannels + TestChannel[chIm].ChannelOffset ;
         Sum := Sum + ADC[j] ;
         end ;
     Avg := Sum/(EndofVHold+1) ;
     IHold := (Avg - TestChannel[ChIm].ADCZero)*TestChannel[ChIm].ADCScale ;

     { Find start / end of test pulse }
     VThreshold := Abs( YMax[ChVm] - YMin[ChVm] ) div 2 ;
     iStart := 0 ;
     iEnd := 0 ;
     for i := 0 to NumTestSamples-1 do begin
         j := i*NumTestChannels + TestChannel[chVm].ChannelOffset ;
         yADC := Abs(ADC[j] - HoldLevel) ;
         if (yADC >= VThreshold) and (iStart = 0) then iStart := i ;
         if (iStart <> 0) and (iEnd=0) and (yADC < VThreshold) then iEnd := i ;
         end ;

     { Calculate amplitude of voltage pulse (from last half of pulse) }
     nAvg := Max( (iEnd - iStart + 1) div 2, 1 ) ;
     iAvgStart := Max( iEnd - nAvg + 1,iStart ) ;
     iAvgEnd := Max( iEnd - 2, iAvgStart ) ;
     Sum := 0. ;
     for i := iAvgStart to iAvgEnd do begin
         j := i*NumTestChannels + TestChannel[chVm].ChannelOffset ;
         Sum := Sum + ADC[j] ;
         end ;
     if (iAvgEnd-iAvgStart+1) <> 0 then Avg := Sum / (iAvgEnd-iAvgStart+1)
                                   else Avg := 0.0 ;

     VPulse := (Avg - TestChannel[ChVm].ADCZero)*TestChannel[ChVm].ADCScale
                - VHold ;

     { Calculate steady-state amplitude of current pulse (from last half of pulse) }
     Sum := 0. ;
     for i := iAvgStart to iAvgEnd do begin
         j := i*NumTestChannels + TestChannel[chIm].ChannelOffset ;
         Sum := Sum + ADC[j] ;
         end ;
     if (iAvgEnd-iAvgStart+1) <> 0 then Avg := Sum / (iAvgEnd-iAvgStart+1)
                                   else Avg := 0.0 ;

     SteadyStateLevel := Avg ;
     IPulse := (Avg - TestChannel[ChIm].ADCZero)*TestChannel[ChIm].ADCScale ;
     IPulse := IPulse - IHold ;

     edVHold.Units := TestChannel[ChVm].ADCUnits ;
     edVHold.Value := VHold ;
     edVPulse.Units := TestChannel[ChVm].ADCUnits ;
     EdVPulse.Value := VPulse ;
     edIHold.Units := TestChannel[ChIm].ADCUnits ;
     edIHold.Value := IHold ;
     edIPulse.Units := TestChannel[ChIm].ADCUnits ;
     EdIPulse.Value := IPulse ;

     { Set scaling factors to convert current and voltage from user's
                     units to Volts and Amps }
     if Contains('mV',TestChannel[ChVm].ADCUnits) then VScale := 1E-3
                                                  else VScale := 1. ;
     if Contains('nA',TestChannel[ChIm].ADCUnits) then IScale := 1E-9
     else if Contains('pA',TestChannel[ChIm].ADCUnits) then IScale := 1E-12
     else if Contains('uA',TestChannel[ChIm].ADCUnits) then IScale := 1E-6
     else if Contains('mA',TestChannel[ChIm].ADCUnits) then IScale := 1E-3
     else IScale := 1. ;

     // Voltage and current pulse amplitude in Volts and Amps
     Voltage := VPulse*VScale ;
     SteadyCurrent := IPulse*IScale ;

     // Set display limits

     YAdd := LabIO.ADCMaxValue[ADCDevice] div 40 ;
     YUpperLimit := LabIO.ADCMaxValue[ADCDevice] ;
     YLowerLimit := LabIO.ADCMinValue[ADCDevice] ;
     for ch := 0 to NumTestChannels-1 do begin

         if ckAutoScale.checked or MainFrm.SealTest.FirstSweep then begin
            // Display auto-scaling
            // Lower display limit
            if TestChannel[ch].yMin > yMin[ch] then
               TestChannel[ch].yMin := Max(yMin[ch]-YAdd,YLowerLimit) ;
            if TestChannel[ch].yMin < (yMin[ch] - (YAdd)) then
               TestChannel[ch].yMin := Max(yMin[ch]-YAdd,YLowerLimit) ;
            // Upper display limit
            if TestChannel[ch].yMax < yMax[ch] then
               TestChannel[ch].yMax := Min(yMax[ch]+YAdd,YUpperLimit) ;
            if TestChannel[ch].yMax > (yMax[ch] + (YAdd)) then
               TestChannel[ch].yMax := Min(yMax[ch]+YAdd,YUpperLimit) ;
            scDisplay.yMin[ch] := TestChannel[ch].yMin ;
            scDisplay.yMax[ch] := TestChannel[ch].yMax  ;

            end
         else begin
            // Fixed magnification display
            TestChannel[ch].yMin := scDisplay.yMin[ch] ;
            TestChannel[ch].yMax := scDisplay.yMax[ch] ;
            end ;

         // Ensure a usable diplay range
         if TestChannel[ch].yMin = TestChannel[ch].yMax then begin
            TestChannel[ch].yMax := Min(TestChannel[ch].yMax + YAdd,YUpperLimit)  ;
            TestChannel[ch].yMin := Min(TestChannel[ch].yMin - YAdd,YLowerLimit)  ;
            end ;

         end ;

     // Calculate seal resistance }
     if Abs(SteadyCurrent) > LoLimit then begin
        Resistance := Voltage/SteadyCurrent ;
        // Note the use of low pass filter to smooth reading
        if ResetReadouts then EdResistance.Value := Resistance ;
        EdResistance.Value := 0.1*Resistance + 0.9*EdResistance.Value ;
        end ;

     { Calculate peak current from first half of pulse }
     PeakCurrent := 0. ;
     PeakAt := 0 ;
     for i := 0 to iAvgStart do begin
         j := i*NumTestChannels + TestChannel[chIm].ChannelOffset ;
         IValue := ((ADC[j] - TestChannel[ChIm].ADCZero)*TestChannel[ChIm].ADCScale
                    - IHold)*IScale ;
         if Abs(IValue) > Abs(PeakCurrent) then begin
            PeakCurrent := IValue ;
            PeakAt := i ;
            end ;
         end ;

     // Calculate Access conductance (Ga)
     // See Gillis K.D. p. 161 Single-channel recording (Neher & Sakmann)
     // Calculate decay time constant of capacity current
     dt := ADCSamplingInterval ;
     Done := False ;
     i := PeakAt ;
     n := 0 ;
     while not Done do begin
         j := i*NumTestChannels + TestChannel[chIm].ChannelOffset ;
         IValue := ((ADC[j] - TestChannel[ChIm].ADCZero)*TestChannel[ChIm].ADCScale
                    - IHold)*IScale - SteadyCurrent ;
         // Invert value if negative peak
         if PeakCurrent < 0.0 then IValue := -IValue ;
         if (IValue > LoCurrentLimit) and (i < iAvgStart) then begin
            x[n] := (i-((iStart + PeakAt) div 2))*dt ;
            y[n] := Ln(IValue) ;
            Inc(n) ;
            Inc(i) ;
            end
         else Done := True ;
         end ;
     OK := LinearRegression( x, y, n, Slope, YIntercept ) ;
     if (not OK) or (Abs(Slope) <= LoLimit) or (Abs(Slope) > 1E30) then Exit ;

     // Calculate Access conductance (Ga)
     // See Gillis K.D. p. 161 Single-channel recording (Neher & Sakmann)
     if rbGaFromPeak.Checked then begin
        if Abs(PeakCurrent) < LoCurrentLimit then Exit ;
        GAccess := PeakCurrent/Voltage ;
        end
     else begin
        if Abs(YIntercept) > 30 then Exit ;
        if Abs(Voltage) < 1E-6 then Exit ;
        GAccess := exp(YIntercept)*sign(PeakCurrent)/Voltage ;
        end ;

     if GAccess > 1E-14 then begin
        if ResetReadouts then edGAccess.Value := GAccess ;
        edGAccess.Value := 0.1*GAccess + 0.9*edGAccess.Value
        end
     else edGAccess.Value := 0.0 ;
     if edGAccess.Value < 1E-14 then Exit ;

     // Calculate membrane conductance (Gm)
     if Abs(Voltage - (SteadyCurrent/edGAccess.Value)) > LoVoltageLimit then begin
        GMembrane := SteadyCurrent/(Voltage - (SteadyCurrent/edGAccess.Value)) ;
        if ResetReadouts then edGmembrane.Value := GMembrane ;
        edGmembrane.Value := 0.1*GMembrane + 0.9*edGmembrane.Value ;
        end
     else edGmembrane.Value := 0.0 ;
     if edGmembrane.Value < 1E-14 then Exit ;

     // Calculate membrane capacity
     if OK and (Abs(Slope) > LoLimit) and (Abs(Slope) < 1E30)then begin
        TauC := -1.0 / Slope ;
        Capacity := TauC*(edGAccess.Value + edGmembrane.Value) ;
        if ResetReadouts then EdCmembrane.Value := Capacity ;
        EdCmembrane.Value := 0.1*Capacity + 0.9*EdCmembrane.Value ;
        end
     else EdCmembrane.Value := 0.0 ;

     except
          end ;

     ResetReadouts := False ;
     end ;


procedure TSealTestFrm.StopSealTest ;
{ ---------------------------------
  Shut down A/D and D/A sub-systems
  ---------------------------------}
var
    VChan,iResource,DACChannel,Dev : Integer ;
begin

    if not SealTestRunning then Exit ;

    // Stop A/D and D/A sweeps
    StopADCandDAC ;

    // Set command voltage outputs to holding values
    for VChan := 0 to  NumCommandVoltageChannels-1 do begin
        iResource := MainFrm.IOConfig.VCommand[VChan] ;
        if iResource <= High(LabIO.Resource) then begin
           DACChannel := LabIO.Resource[iResource].StartChannel ;
           Dev := LabIO.Resource[iResource].Device ;
           LabIO.DACOutState[Dev,DACChannel] :=
               Mainfrm.VCommand[VChan].HoldingVoltage*Mainfrm.VCommand[VChan].DivideFactor ;
           LabIO.WriteDAC( Dev,
                           LabIO.DACOutState[Dev,DACChannel],
                           DACChannel ) ;
           end ;
        end ;

    Timer.Enabled := False ;
    State := Idle ;
    SealTestRunning := False ;

    end ;


procedure TSealTestFrm.edHoldingVoltage1KeyPress(Sender: TObject; var Key: Char);
{ ------------------------------------
  Set Voltage clamp holding voltage #1
  ------------------------------------}
begin
     if key = #13 then ReadPulseSettings := True ;
     end;


procedure TSealTestFrm.edHoldingVoltage2KeyPress(Sender: TObject; var Key: Char);
{ ------------------------------------
  Set Voltage clamp holding voltage #2
  ------------------------------------}
begin
     if key = #13 then ReadPulseSettings := True ;
     end;


procedure TSealTestFrm.EdHoldingVoltage3KeyPress(Sender: TObject;
  var Key: Char);
{ ------------------------------------
  Set Voltage clamp holding voltage #3
  ------------------------------------}
begin
     if key = #13 then ReadPulseSettings := True ;
     end;

procedure TSealTestFrm.rbUseHoldingVoltage1Click(Sender: TObject);
{ ---------------------------------
  Set holding voltage to voltage #1
  ---------------------------------}
begin
     { Set holding voltage and pulse height to group #1 }
     MainFrm.SealTest.Use := 1 ;
     ReadPulseSettings := True ;
     end;


procedure TSealTestFrm.rbUseHoldingVoltage2Click(Sender: TObject);
{ ---------------------------------
  Set holding voltage to voltage #2
  ---------------------------------}
begin
     { Set holding voltage and pulse height to group #2 }
     MainFrm.SealTest.Use := 2 ;
     ReadPulseSettings := True ;
     end;


procedure TSealTestFrm.rbUseHoldingVoltage3Click(Sender: TObject);
{ ---------------------------------
  Set holding voltage to voltage #3
  Note. No pulse with this option
  ---------------------------------}
begin
     { Set holding voltage and pulse height to group #3 }
     MainFrm.SealTest.Use := 3 ;
     ReadPulseSettings := True ;
     end;


procedure TSealTestFrm.edPulseHeight1KeyPress(Sender: TObject;
  var Key: Char);
{ -------------------------------
  Set test pulse amplitude #1
  -------------------------------}
begin
     if key = #13 then ReadPulseSettings := True ;
     end;


procedure TSealTestFrm.edPulseheight2KeyPress(Sender: TObject;
  var Key: Char);
  { -----------------------------
    Set test pulse amplitude #2
    -----------------------------}
begin
     if key = #13 then ReadPulseSettings := True ;
     end;


procedure TSealTestFrm.edPulseWidthKeyPress(Sender: TObject; var Key: Char);
{ ----------------------------
  Set width of seal test pulse
  ----------------------------}
begin
     if key = #13 then begin
        ReadPulseSettings := True ;
        MainFrm.SealTest.PulseWidth := edPulseWidth.Value ;
        { Note. Force a re-start of D/A waveform so that D/A cycle
          time is updated }
        StopADCandDAC ;
        State := StartSweep ;
        end ;
     end ;


procedure TSealTestFrm.FormDeactivate(Sender: TObject);
{ -----------------------------------------
  Called when focus moves to another window
  -----------------------------------------}
begin
     { Terminate seal test pulses if form loses focus }
//     Timer.enabled := false ;
//     ResetReadouts := True ;
//     StopSealTest ;

     end;


procedure TSealTestFrm.FormActivate(Sender: TObject);
{ -------------------------------------------
  Called when focus moves to seal test window
  -------------------------------------------}
begin

     // Stop camera (if it is running)
     if MainFrm.FormExists('RecordFrm') then begin
        RecordFrm.StopCameraAndAnalogIO ;
        end ;
     if MainFrm.FormExists('RecADCOnlyFrm') then begin
        RecADCOnlyFrm.StopADC ;
        end ;

     { Start seal test pulses when form gains focus }
     Timer.enabled := True ;
     ResetReadouts := True ;
     ReadPulseSettings := True ;
     TimerBusy := False ;
     State := StartSweep ;

     end;


procedure TSealTestFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
{ -----------------------------
  Called when window is closed
  -----------------------------}
var
    ch : Integer ;
    Device : Integer ;
begin


     { Shut down A/D and D/A sub-systems }
     StopSealTest ;
     { Note. If A/D and D/A sub-systems are still active when the
      form is closed the program will crash. }

     { Test pulse amplitude & width }
     MainFrm.SealTest.PulseHeight1 := edPulseHeight1.Value ;
     MainFrm.SealTest.PulseHeight2 := edPulseHeight2.Value ;
     MainFrm.SealTest.PulseHeight3 := edPulseHeight3.Value ;
     MainFrm.SealTest.PulseWidth := edPulseWidth.Value ;
     MainFrm.SealTest.HoldingVoltage1 := edHoldingVoltage1.Value ;
     MainFrm.SealTest.HoldingVoltage2 := edHoldingVoltage2.Value ;
     MainFrm.SealTest.HoldingVoltage3 := edHoldingVoltage3.Value ;


     // Free DAC buffers
     for Device := 1 to LabIO.NumDevices do begin
         if DACBufs[Device] <> Nil then FreeMem( DACBufs[Device] ) ;
         DACBufs[Device] := Nil ;
         end ;

     // Save chart visiblity state
     // Modified by NS on 24 March 2009
     MainFrm.ADCChannelSealtestNumberOfChannels := scDisplay.NumChannels ;
     for ch := 0 to scDisplay.NumChannels-1 do begin
         MainFrm.ADCChannelSealtestVisible[ch] := scDisplay.ChanVisible[ch] ;
     end ;

     { Close the form }
     Action := caFree ;
     MainFrm.mnSealTest.enabled := True ;

     end;


procedure TSealTestFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{ --------------
  Function keys
  -------------}
begin
     case Key of
          { F3 selects holding voltage #1 }
          VK_F3 : begin
               MainFrm.SealTest.Use := 1 ;
               Mainfrm.VCommand[cbDACOut.ItemIndex].HoldingVoltage := edHoldingVoltage1.Value ;
               MainFrm.SealTest.PulseHeight := edPulseHeight1.Value ;
               rbUseHoldingVoltage1.checked := True ;
               ResetReadouts := True ;
               end ;
          { F4 selects holding voltage #2 }
          VK_F4 : begin
               MainFrm.SealTest.Use := 2 ;
               Mainfrm.VCommand[cbDACOut.ItemIndex].HoldingVoltage := edHoldingVoltage2.value ;
               MainFrm.SealTest.PulseHeight := edPulseHeight2.Value ;
               rbUseHoldingVoltage2.checked := True ;
               ResetReadouts := True ;
               end ;
          { F5 selects holding voltage #3 }
          VK_F5 : begin
               rbUseHoldingVoltage3.checked := True ;
               MainFrm.SealTest.Use := 3 ;
               Mainfrm.VCommand[cbDACOut.ItemIndex].HoldingVoltage := edHoldingVoltage3.Value ;
               MainFrm.SealTest.PulseHeight := edPulseHeight3.Value ;
               ResetReadouts := True ;
               end ;
          end ;
     end;


procedure TSealTestFrm.FormResize(Sender: TObject);
{ ----------------------------------
  Called when window size is changed
  ----------------------------------}
begin

     Height := Max( Height,MinFormHeight ) ;
     Width := Max( Width,MinFormWidth ) ;
     TimerGrp.Top := ClientHeight - TimerGrp.Height - 5 ;
     AmplifierGrp.Top := Timergrp.Top - AmplifierGrp.Height - 5 ;
     DACGrp.Height := AmplifierGrp.Top - DACGrp.Top - 5 ;

     VoltsGrp.Top := ClientHeight - VoltsGrp.Height - 5 ;
     CurrentGrp.Top := VoltsGrp.Top ;
     CellGrp.Top := VoltsGrp.Top ;

     ckAutoScale.Top := VoltsGrp.Top - ckAutoScale.Height - 5 ;

     scDisplay.Width := Width - scDisplay.Left - 15 ;
     scDisplay.Height := ckAutoScale.Top - scDisplay.Top -2 ;

     end;


procedure TSealTestFrm.ckAutoScaleClick(Sender: TObject);
{ ------------------------------
  Enable display auto-scale mode
  ------------------------------}
begin
     MainFrm.SealTest.AutoScale := ckAutoScale.checked ;
     end;


procedure TSealTestFrm.ChangeDisplayGrid ;
{ --------------------------------------------
  Update grid pattern on oscilloscope display
  -------------------------------------------- }
begin

     if ADCDevice < 1 then Exit ;
     scDisplay.MaxADCValue := LabIO.ADCMaxValue[ADCDevice] ;
     scDisplay.MinADCValue := LabIO.ADCMinValue[ADCDevice] ;
     scDisplay.DisplayGrid := True ;
     scDisplay.Invalidate ;
     end ;


procedure  TSealTestFrm.ZoomOutAll ;
{ ---------------------------------
  Set minimum display magnification
  --------------------------------- }
begin
     if ADCDevice < 1 then Exit ;
     scDisplay.MaxADCValue := LabIO.ADCMaxValue[ADCDevice] ;
     scDisplay.MinADCValue := LabIO.ADCMinValue[ADCDevice] ;
     scDisplay.ZoomOut ;
     end ;


procedure TSealTestFrm.bResetTimerClick(Sender: TObject);
begin
    // Initialise elapsed time readout
    TimerStarted := TimeGetTime ;
    end;

procedure TSealTestFrm.UpdateChannelScalingFactors ;
// ------------------------------
// Update channel scaling factors
// ------------------------------
var
   ch : Integer ;
begin
     for ch := 0 to NumTestChannels-1 do begin

         // Ensure that calibration factor is non-zero
         if TestChannel[ch].ADCCalibrationFactor = 0.0 then
            TestChannel[ch].ADCCalibrationFactor := 0.001 ;

         // Ensure that amplifier gain is non-zero
         if TestChannel[ch].ADCAmplifierGain = 0.0 then
            TestChannel[ch].ADCAmplifierGain := 1.0 ;

         // Calculate bits->units scaling factor
         TestChannel[ch].ADCScale := MainFrm.ADCVoltageRange /
                                     (TestChannel[ch].ADCCalibrationFactor*
                                     TestChannel[ch].ADCAmplifierGain
                                     *(LabIO.ADCMaxValue[ADCDevice]+1) ) ;
         end ;
     end ;


function TSealTestFrm.FindTimingDevice : Smallint ;
// ----------------------------------------------
// Find NI device to use as ADC/DAC timing source
// ----------------------------------------------
var
    Dev : Integer ;
begin
     // Use last DAC device in use as timing device
     Result := 1 ;
     for Dev := 1 to LabIO.NumDevices do if DeviceInUse[Dev] then Result := Dev ;
     end ;


procedure TSealTestFrm.StopADCandDAC ;
// ---------------------------------------------
// Stop timed input/output on all ADC and DAC channels
// ---------------------------------------------
var
    iDev : Integer ;
begin
     { Stop laboratory interface activity }
     LabIO.StopADC(ADCDevice) ;
     for iDev := 1 to LabIO.NumDevices do LabIO.StopDAC(iDev) ;
     end ;


procedure TSealTestFrm.cbCurrentChannelChange(Sender: TObject);
begin
     ResetReadouts := True ;
     end;

procedure TSealTestFrm.SetDisplayGrid( Value : Boolean ) ;
// ------------------------------------
// Set chart display grid on/off
// ------------------------------------
begin
    scDisplay.DisplayGrid := Value ;
    end ;


function TSealTestFrm.GetDisplayGrid : Boolean ;
// ------------------------------------
// Get chart display grid on/off state
// ------------------------------------
begin
    Result := scDisplay.DisplayGrid ;
    end ;




procedure TSealTestFrm.edVDivide0KeyPress(Sender: TObject; var Key: Char);
// -----------------
// Set divide factor
// -----------------
begin
     if (key = #13) then begin
        Mainfrm.VCommand[0].DivideFactor := edVDivide0.Value ;
        end ;
     end;


procedure TSealTestFrm.edVDivide1KeyPress(Sender: TObject; var Key: Char);
// -----------------
// Set divide factor
// -----------------
begin
     if (key = #13) then begin
        Mainfrm.VCommand[1].DivideFactor := edVDivide1.Value ;
        end ;
     end;


procedure TSealTestFrm.rbAmplifier1Click(Sender: TObject);
// -------------------------------
// Select patch clamp amplifier #1
// -------------------------------
begin
     SelectAmplifier(1) ;
     end;

procedure TSealTestFrm.rbAmplifier2Click(Sender: TObject);
// -------------------------------
// Select patch clamp amplifier #1
// -------------------------------
begin
     SelectAmplifier(2) ;
     end;


procedure TSealTestFrm.SelectAmplifier(
          AmpNumber : Integer
          ) ;
// ------------------------------
// Select amplifier for seal test
// ------------------------------
begin

     cbCurrentChannel.ItemIndex := 2*(AmpNumber-1)  ;
     cbVoltageChannel.ItemIndex := 2*(AmpNumber-1) + 1 ;

     cbDACOut.ItemIndex := AmpNumber - 1 ;

     end ;




procedure TSealTestFrm.edAmplifier1GainKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then NewAmplifier1GainValue := True ;
     end;

procedure TSealTestFrm.edAmplifier2GainKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then NewAmplifier2GainValue := True ;
     end;


procedure TSealTestFrm.edPulseHeight3KeyPress(Sender: TObject;
  var Key: Char);
  { -----------------------------
    Set test pulse amplitude #3
    -----------------------------}
begin
     if key = char(13) then begin
        MainFrm.SealTest.PulseHeight3 := edPulseHeight3.Value ;
        if rbUseHoldingVoltage3.checked then begin
           MainFrm.SealTest.PulseHeight := edPulseHeight3.Value ;
           ResetReadouts := True ;
           end ;
        end ;
     end;


end.
