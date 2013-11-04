unit StimModule;
{ =============================================
  WinFluor -Analogue & Digital Waveform Generator
  27.05.05 Based upon WinWCP stimModule.pas
  =============================================
  05.09.05 .. MainFrm.ADCScanInterval now used
  20.07.06 .. Spurious pulse to holding potential at end of stimulus pulse fixed
              (for Chicago)
  11.10.06 .. Series of pulse trains protocol added.
  15.08.07 .. Digital pulse with incremented delay added
  28.08.07 .. Digital pulses now kept within limits of DigBuf
              (fixes memory exception when using long digital pulse stimuli)
              Ptrain1 (Incrementing series of pulse trains removed
  04.10.09 .. NS Added third DAC channel, Vout2
  23.07.12 .. JD 
  }

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  stdctrls, maths, winprocs, LabIOUnit, math ;

const
     DigShapes = 10 ; { First digital waveform shape }
     MaxWaveShapes = 38 ;
     MaxV3216WaveShapes = 28 ;
     MaxOldWaveShapes = 18 ;
     MaxProtocolShapes = 38 ;
     MaxV3216ProtocolShapes = 28 ;
     MaxDACPointsPerProtocol = 8000 ;

     // DAC0 shape #s
     V0Start = 0 ;
     V0End = 9 ;
     // Digital shape #s
     DigStart = 10 ;
     DigEnd = 17 ;
     // DAC1 shape #s
     V1Start = 18 ;
     V1End = 27 ;
     // DAC2 shape #s
     V2Start = 28 ;
     V2End = 37 ;
     NumDigChannels = 8 ;
     NumVChannels = 3 ;
     DAC0 = 0 ;
     DAC1 = 1 ;
     DAC2 = 2 ;

     //WaveformCreationTime = 0.05 ;

type

// Stimulator waveform types
TWaveShape = ( wvNone, wvStep0, wvStep1, wvStep2, wvRamp, wvpTrain, wvWave,
               wvDigStep0,wvDigStep1, wvDigTrain, wvDigNone, wvpTrain1, wvDigDelay ) ;
// Old waveform definition record (pre-WinWCP V3.5.2)
TWaveformOld = packed record
          Shape : Array[0..17] of TWaveShape ;
          Amplitude :     Array[0..17] of Single ;
          Duration :      Array[0..17] of Single ;
          Increment :     Array[0..17] of Single ;
          RampStart :     Array[0..17] of Single ;
          RampEnd :       Array[0..17] of Single ;
          PulseInterval : Array[0..17] of Single ;
          NumPulses : Array[0..17] of LongInt ;
          Period :        Array[0..17] of Single ;
          Delay :         Array[0..17] of Single ;
          Invert :        Array[0..17] of Boolean ;
          HoldingVoltage : Single ;
          VMin : Single ;
          VMax : Single ;
          RecordInterval : Single ;
          RecordDuration : Single ;
          RecordDelay : Single ;
          RecordDelayIncrement : Single ;
          NumSteps : LongInt ;
          NumRepeats : LongInt ;
          dt : Single ;
          DACdt : Single ;
          FileName : string[255] ;
          ExtWaveData : Array[0..1023] of single ;
          ExtEndOfData : SmallInt ;
          ExtDACdt : single ;
          NextFile : string[255] ;
          StepCounter : LongInt ;
          RepeatCounter : LongInt ;
          NumDACPoints : LongInt ;
          NotInUse : LongInt ;
          LeakCounter : longint ;
          NumLeaks : LongInt ;
          DigitalInUse : Boolean ;
          DigitalPortValue : LongInt ;
          RecordingStart : LongInt ;
          InUse : Boolean ;
          Saved : Boolean ;
          RecordIntervalChanged : boolean ;
          LeakScale : LongInt ;
          NextProtocolFileName : string[255] ;
          end ;

// Waveform definition record
TWaveformV3216 = packed record
          Shape : Array[0..MaxV3216WaveShapes-1] of TWaveShape ;
          Amplitude :     Array[0..MaxV3216WaveShapes-1] of Single ;
          Duration :      Array[0..MaxV3216WaveShapes-1] of Single ;
          Increment :     Array[0..MaxV3216WaveShapes-1] of Single ;
          RampStart :     Array[0..MaxV3216WaveShapes-1] of Single ;
          RampEnd :       Array[0..MaxV3216WaveShapes-1] of Single ;
          PulseInterval : Array[0..MaxV3216WaveShapes-1] of Single ;
          NumPulses : Array[0..MaxV3216WaveShapes-1] of LongInt ;
          Period :        Array[0..MaxV3216WaveShapes-1] of Single ;
          Delay :         Array[0..MaxV3216WaveShapes-1] of Single ;
          Invert :        Array[0..MaxV3216WaveShapes-1] of Boolean ;
          HoldingVoltage : Array[DAC0..DAC1] of Single ;
          VMin : Single ;
          VMax : Single ;
          RecordInterval : Single ;
          RecordDuration : Single ;
          NumSteps : LongInt ;
          NumRepeats : LongInt ;
          dt : Single ;
          DACdt : Single ;
          FileName : string[255] ;
          ExtWaveData : Array[0..1023] of single ;
          ExtEndOfData : SmallInt ;
          ExtDACdt : single ;
          NextFile : string[255] ;
          StepCounter : LongInt ;
          RepeatCounter : LongInt ;
          NumDACPoints : LongInt ;
          NotInUse : LongInt ;
          LeakCounter : longint ;
          NumLeaks : LongInt ;
          DigitalInUse : Boolean ;
          DigitalPortValue : LongInt ;
          RecordingStart : LongInt ;
          InUse : Boolean ;
          Saved : Boolean ;
          RecordIntervalChanged : boolean ;
          LeakScale : LongInt ;
          NextProtocolFileName : string[255] ;
          end ;

// Waveform definition record
TWaveform = packed record
          Shape : Array[0..MaxWaveShapes-1] of TWaveShape ;
          Amplitude :     Array[0..MaxWaveShapes-1] of Single ;
          Duration :      Array[0..MaxWaveShapes-1] of Single ;
          Increment :     Array[0..MaxWaveShapes-1] of Single ;
          RampStart :     Array[0..MaxWaveShapes-1] of Single ;
          RampEnd :       Array[0..MaxWaveShapes-1] of Single ;
          PulseInterval : Array[0..MaxWaveShapes-1] of Single ;
          NumPulses : Array[0..MaxWaveShapes-1] of LongInt ;
          Period :        Array[0..MaxWaveShapes-1] of Single ;
          Delay :         Array[0..MaxWaveShapes-1] of Single ;
          Invert :        Array[0..MaxWaveShapes-1] of Boolean ;
          HoldingVoltage : Array[DAC0..DAC2] of Single ;
          VMin : Single ;
          VMax : Single ;
          RecordInterval : Single ;
          RecordDuration : Single ;
          NumSteps : LongInt ;
          NumRepeats : LongInt ;
          dt : Single ;
          DACdt : Single ;
          FileName : string[255] ;
          ExtWaveData : Array[0..1023] of single ;
          ExtEndOfData : SmallInt ;
          ExtDACdt : single ;
          NextFile : string[255] ;
          StepCounter : LongInt ;
          RepeatCounter : LongInt ;
          NumDACPoints : LongInt ;
          NotInUse : LongInt ;
          LeakCounter : longint ;
          NumLeaks : LongInt ;
          DigitalInUse : Boolean ;
          DigitalPortValue : LongInt ;
          RecordingStart : LongInt ;
          InUse : Boolean ;
          Saved : Boolean ;
          RecordIntervalChanged : boolean ;
          LeakScale : LongInt ;
          NextProtocolFileName : string[255] ;
          end ;



  TStimulator = class(TDataModule)
  private
    { Private declarations }
    VChannel : Array[0..MaxProtocolShapes-1] of Integer ;
    DACPointer : Array[0..NumVChannels-1] of Integer ;
    DigChannel : Array[0..MaxProtocolShapes-1] of Integer ;
    DigPointer : Array[0..NumDigChannels-1] of Integer ;
    NumDACPoints : Integer ;

    procedure FillDACBuf(
          VChan : Integer ;
          Time : Single ;
          DACVoltsStart : Single ;                  // Voltage level
          DACVoltsEnd : Single ;                  // Voltage level
          DACBufs : Array of PBig16bitArray
          ) ;
    procedure FillDigBuf(
              DigChannel : Integer ;
              Time : Single ;
              BitOn : Boolean ;
              DigBufs : Array of PBig32bitArray    // Pointers to digital buffers
              ) ;


  public
    { Public declarations }
    FileName : String ;
    Prog : TWaveform ;

    procedure CreateWaveform(
              DACBufs : Array of PBig16bitArray ;
              DigBufs : Array of PBig32bitArray ;
              NumDACPointsIn : Integer ;
              InitialiseDACPointers : Boolean
              )  ;

    procedure CreateProgramList(
              var cbList : TComboBox ) ;
    procedure LoadProgram(
              const Name : string ) ;
    procedure SaveProgram(
              const Name : string ) ;
    procedure ClearWaveformElements ;
    function ProtocolDuration : single ;

  end;

var
  Stimulator: TStimulator;

implementation

uses Main, shared, Wavgen ;

{$R *.DFM}


procedure TStimulator.CreateWaveform(
         DACBufs : Array of PBig16bitArray ;
         DigBufs : Array of PBig32bitArray ;
         NumDACPointsIn : Integer ;
         InitialiseDACPointers : Boolean
         )  ;
{ -----------------------------------------------------
  Create command voltage waveforms defined by protocol
  ----------------------------------------------------}
var
   TProtocol,V,dV,NumPoints : Single ;
   NumRamp,i,j,Shape,NumExtra,iEnd,iStep : Integer ;

   Device : Integer ;           // Hardware device
   VChan : Integer ;            // Voltage stimulus channel
   DACChan : Integer ;          // DAC channel to output on
   DACNumChannels : Integer ;   // No. of DAC channels in device
   TInterval : Single ;
   BitWord : SmallInt ;
   BitMask : SmallInt ;
   Bit : SmallInt ;
   DACValue : Integer ;
   DACScale : Single ;
   PDACBuf : PBig16bitArray ;
   iStart : Integer ;
   Step : Integer ;
   OnLevel : Integer ;
   DACch : Integer ;
   PDigBuf : PBig32bitArray ;
   DigDevice : Integer ;      // Digital output interface card device #
   iRep,NumRepeats,NumPointsPerRepeat : Integer ;
begin

     // Define shape-> plot channel mappings
     for i := V0Start to V0End do VChannel[i] := 0 ;
     for i := V1Start to V1End do VChannel[i] := 1 ;
     for i := V2Start to V2End do VChannel[i] := 2 ;
     for i := DigStart to DigEnd do DigChannel[i] := i - DigStart ;

     // Calculate total duration of voltage protocol
     TProtocol := Prog.NumSteps*Prog.RecordInterval ;
     NumDACPoints := NumDACPointsIn ;

     { Determine if a digital channel is in use }
     Prog.DigitalInUse := False ;
     NumExtra := Prog.NumSteps - 1 ;
     for i := 0 to High(Prog.Shape) do begin
         case Prog.Shape[i] of
              wvDigStep0 : Prog.DigitalInUse := True ;
              wvDigStep1 : Prog.DigitalInUse := True ;
              wvDigTrain : Prog.DigitalInUse := True ;
              wvDigDelay : Prog.DigitalInUse := True ;
              end ;
         end ;

     // Fill DAC buffers with holding potential
     // ---------------------------------------

     for VChan := DAC0 to DAC2 do begin

        // Exit if no D/A channel configured
        if (MainFrm.IOConfig.VCommand[VChan] < 0) or
           (MainFrm.IOConfig.VCommand[VChan] > MaxResources) then Continue ;

        // Get hardware device for this channel
        Device := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device ;
        if DACBufs[Device] = Nil Then Continue ;

        // Pointer to device DAC buffer
        PDACBuf := DACBufs[Device] ;

        if InitialiseDACPointers then begin
           LabIO.DAC[Device].Pointer := 0 ;
           LabIO.DAC[Device].EndofBuf := NumDACPoints ;
           end ;

        DACChan := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].StartChannel ;
        DACNumChannels := LabIO.NumDACs[Device] ;

        // Set V->DAC scaling factor
        if MainFrm.VCommand[VChan].DivideFactor <= 0. then MainFrm.VCommand[VChan].DivideFactor := 1. ;
        DACScale := (LabIO.DACMaxValue[Device]*MainFrm.VCommand[VChan].DivideFactor) / LabIO.DACMaxVolts[Device] ;

        // Calculate DAC value
        DACValue := Round( DACScale*MainFrm.VCommand[VChan].HoldingVoltage ) ;
        DACValue := Min(DACValue,LabIO.DACMaxValue[Device]) ;
        DACValue := Max(DACValue,-LabIO.DACMaxValue[Device]-1) ;

        // Update buffer
        j := DACChan ;
        for i := 1 to NumDACPoints do begin
            PDACBuf^[j] := DACValue ;
            j := j + DACNumChannels ;
            end ;
        end ;

     // Update digital stimulus bit of digital o/p buffer
     // with off-state settings. (Normal=0V, Inverted=5V)
     // ---------------------------------------------

     PDigBuf := Nil ;
     DigDevice := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device ;
     if (MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart)) and
        (DigBufs[DigDevice] <> Nil) then begin

        PDigBuf := DigBufs[DigDevice] ;

        if InitialiseDACPointers then begin
           LabIO.DIG[Device].Pointer := 0 ;
           LabIO.DIG[Device].EndofBuf := NumDACPoints ;
           end ;

        // Create bit and bit mask values for off values of digital stimulus channel
        BitMask := 0 ;
        BitWord := 0 ;
        Bit := LabIO.BitMask(LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel) ;
        for Shape := DigStart to DigEnd do begin
            if Bit <=
               LabIO.BitMask(LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel) then begin
               BitMask := BitMask or Bit ;
               if Prog.Invert[Shape] then BitWord := BitWord or Bit ;
               end ;
            Bit := Bit*2 ;
            end ;

        BitMask := not BitMask ;
        for i := 0 to NumDACPoints-1 do begin
            PDigBuf^[i] := (PDigBuf^[i] and BitMask) or BitWord ;
            end ;

        end ;

     // Fill DAC and digital buffers with protocols shapes
     // --------------------------------------------------

     if Prog.NumRepeats > 1 then begin
        // Repeated stimulus
        NumPointsPerRepeat := Round((Prog.RecordInterval*Prog.NumSteps)/MainFrm.ADCScanInterval) ;
        NumRepeats := Max(NumDACPoints div Max(NumPointsPerRepeat,1),1) ;
        // adjust interval to fit buffer
        NumPointsPerRepeat := NumDACPoints div NumRepeats ;
        end
     else  begin
        NumRepeats := 1 ;
        NumPointsPerRepeat := NumDACPoints ;
        end ;

     for iRep := 0 to NumRepeats-1 do begin
       for Step := 0 to Prog.NumSteps-1 do begin

         // Initialise buffer pointers
         // (leaving first point always set to holding voltages)
         iStart := Trunc((Prog.RecordInterval*Step)/MainFrm.ADCSCanInterval)
                   + iRep*NumPointsPerRepeat ;
         for i := 0 to High(DACPointer) do DACPointer[i] := iStart ;
         for i := 0 to High(DigPointer) do DigPointer[i] := iStart ;

         for Shape := 0 to High(Prog.Shape) do begin

             case Prog.Shape[Shape] of
              wvStep0 : Begin
                  { Fixed size step }
                  FillDACBuf( VChannel[Shape],
                              Prog.Delay[Shape],
                              0.0,
                              0.0,
                              DACBufs) ;
                  FillDACBuf( VChannel[Shape],
                              Prog.Duration[Shape],
                              Prog.Amplitude[Shape],
                              Prog.Amplitude[Shape],
                              DACBufs) ;
                  end ;

              wvStep1 : Begin
                  { Family of steps incrementing in amplitude }
                  FillDACBuf( VChannel[Shape],
                              Prog.Delay[Shape],
                              0.0,
                              0.0,
                              DACBufs) ;
                  FillDACBuf( VChannel[Shape],
                              Prog.Duration[Shape],
                              Prog.Amplitude[Shape] + Prog.Increment[Shape]*Step,
                              Prog.Amplitude[Shape] + Prog.Increment[Shape]*Step,
                              DACBufs) ;
                  end ;
              wvStep2 : Begin
                  { Family of steps incrementing in duration }
                  FillDACBuf( VChannel[Shape],
                              Prog.Delay[Shape],
                              0.0,
                              0.0,
                              DACBufs) ;
                  FillDACBuf( VChannel[Shape],
                              (Prog.Duration[Shape]
                              + Prog.Increment[Shape]*Step ),
                              Prog.Amplitude[Shape],
                              Prog.Amplitude[Shape],
                              DACBufs) ;
                  end ;

              wvRamp : Begin
                  { Voltage ramp }
                  FillDACBuf( VChannel[Shape],
                              Prog.Delay[Shape],
                              0.0,
                              0.0,
                              DACBufs
                              ) ;
                  FillDACBuf( VChannel[Shape],
                                  Prog.Duration[Shape],
                                  Prog.RampStart[Shape],
                                  Prog.RampEnd[Shape],
                                  DACBufs) ;
                  end ;

              wvPTrain : Begin
                  { Pulse train }
                  FillDACBuf( VChannel[Shape],
                              Prog.Delay[Shape],
                              0.0,
                              0.0,
                              DACBufs) ;
                  for i := 1 to Prog.NumPulses[Shape] do begin
                      { Pulse duration }
                      FillDACBuf( VChannel[Shape],
                                  Prog.Duration[Shape],
                                  Prog.Amplitude[Shape],
                                  Prog.Amplitude[Shape],
                                  DACBufs) ;
                      { Inter pulse interval }
                      { Note ... Train ends at last pulse }
                      TInterval := Max(
                                   (Prog.PulseInterval[Shape] - Prog.Duration[Shape]),
                                   MainFrm.ADCScanInterval
                                   ) ;
                      if i = Prog.NumPulses[Shape] then TInterval := MainFrm.ADCScanInterval ;
                      FillDACBuf( VChannel[Shape],
                                  TInterval,
                                  0.0,
                                  0.0,
                                  DACBufs) ;
                      end ;
                  end ;

              wvWave : Begin
                  { Waveform defined from an external file }
                  FillDACBuf( VChannel[Shape],
                              Prog.Delay[Shape],
                              0.0,
                              0.0,
                              DACBufs
                              ) ;
                  for i := 0 to Prog.ExtEndofData do
                      FillDACBuf( VChannel[Shape],
                                  MainFrm.ADCScanInterval,
                                  Prog.ExtWaveData[i],
                                  Prog.ExtWaveData[i],
                                  DACBufs ) ;
                  end ;

              wvDigStep0 : Begin
                  { Digital pulse }
                  FillDigBuf( DigChannel[Shape],
                              Prog.Delay[Shape],
                              Prog.Invert[Shape],
                              DigBufs) ;
                  FillDigBuf( DigChannel[Shape],
                              Prog.Duration[Shape],
                              not Prog.Invert[Shape],
                              DigBufs) ;
                  Prog.DigitalInUse := True ;
                  end ;

              wvDigStep1 : Begin
                  { Digital pulse (incrementing width)}
                  FillDigBuf( DigChannel[Shape],
                              (Prog.Delay[Shape]),
                              Prog.Invert[Shape],
                              DigBufs) ;
                  FillDigBuf( DigChannel[Shape],
                              (Prog.Duration[Shape] +
                              (Prog.Increment[Shape]*Step)),
                              not Prog.Invert[Shape],
                              DigBufs) ;
                  Prog.DigitalInUse := True ;
                  end ;

              wvDigTrain : begin
                  { Pulse train }
                  FillDigBuf( DigChannel[Shape],
                              Prog.Delay[Shape],
                              Prog.Invert[Shape],
                              DigBufs) ;
                  for i := 1 to Prog.NumPulses[Shape] do begin
                      { Pulse duration }
                      FillDigBuf( DigChannel[Shape],
                                  Prog.Duration[Shape],
                                  not Prog.Invert[Shape],
                                  DigBufs) ;
                      { Inter pulse interval }
                      { Note ... Train ends at last pulse }
                      TInterval := Prog.PulseInterval[Shape] - Prog.Duration[Shape] ;
                      if i = Prog.NumPulses[Shape] then TInterval := MainFrm.ADCScanInterval ;
                      FillDigBuf( DigChannel[Shape],
                                  TInterval,
                                  Prog.Invert[Shape],
                                  DigBufs) ;
                      end ;
                  Prog.DigitalInUse := True ;
                  end ;

              wvDigDelay : Begin
                  { Digital pulse (incrementing width)}
                  FillDigBuf( DigChannel[Shape],
                              Prog.Delay[Shape] +
                              (Prog.Increment[Shape]*Step),
                              Prog.Invert[Shape],
                              DigBufs) ;
                  FillDigBuf( DigChannel[Shape],
                              Prog.Duration[Shape],
                              not Prog.Invert[Shape],
                              DigBufs) ;
                  Prog.DigitalInUse := True ;
                  end ;
              end ;
             end ;
         end ;
       end ;

     {Set default value for digital ports }
     if PDigBuf <> Nil then
        Prog.DigitalPortValue := PDigBuf^[0]
     else Prog.DigitalPortValue := 0 ;

     // If DAC channels are being used to support digital outputs
     // copy digital states to DAC buffer

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) and
        (LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].ResourceType = DACOut) then begin

        // Get hardware device for this channel
        Device := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device ;
        if DACBufs[Device] = Nil Then Exit ;

        // Pointer to device DAC buffer
        PDACBuf := DACBufs[Device] ;
        PDigBuf := DigBufs[Device] ;

        DACNumChannels := LabIO.NumDACs[Device] ;

        // Calculate 5V on level value
        OnLevel := Round((4.99*LabIO.DACMaxValue[Device])/LabIO.DACMaxVolts[Device]) ;

        // Update DAC buffer
        for DACch := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel to
                     LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel do begin
            Bit := LabIO.BitMask(DACch) ;
            j := DACch ;
            for i := 0 to NumDACPointsIn-1 do begin
                if (Bit and PDigBuf^[i]) <> 0 then PDacBuf^[j] := OnLevel
                                              else PDacBuf^[j] := 0 ;
                j := j + DACNumChannels ;
                end ;
            end ;
        end ;


     end ;


procedure TStimulator.FillDACBuf(
          VChan : Integer ;                    // Voltage O/P channel to be updated
          Time : Single ;                      // Time at voltage level
          DACVoltsStart : Single ;                  // Voltage level
          DACVoltsEnd : Single ;                  // Voltage level
          DACBufs : Array of PBig16bitArray    // Pointers to DAC buffers
          ) ;
{ -------------------------------------------------------------
  Copy a series of NumPoints D/A values of amplitude Value into
  channel Chan of a D/A output buffer Buf, starting at point BufPointer
  ---------------------------------------------------------------------
  23/11/01 DAC buffer binary wrap-round error fixed}
var
   i,j,jEnd,jShift : Integer ;
   iDACValue,IDACMax,iDACMin : Integer ;
   DACScale,V,DV : Single ;
   DACChan : Integer ;
   DACNumChannels : Integer ;
   Device : Integer ;
   NumPoints : Integer ;
   PDACBuf : PBig16bitArray ;
begin

     if Time <= 0.0 then Exit ;

     // Exit if no hardware DAC channel configured for this voltage channel
     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[VChan]) then Exit ;

     // Get hardware device for channel
     Device := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device ;
     if DACBufs[Device] = Nil Then Exit ;

     // Pointer to device DAC buffer
     PDACBuf := DACBufs[Device] ;

     // Hardware DAC output channel
     DACChan := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].StartChannel ;
     DACNumChannels := LabIO.NumDACs[Device] ;

     // Set V->DAC scaling factor
     if MainFrm.VCommand[VChan].DivideFactor <= 0. then MainFrm.VCommand[VChan].DivideFactor := 1. ;
     DACScale := (LabIO.DACMaxValue[Device]*MainFrm.VCommand[VChan].DivideFactor) / LabIO.DACMaxVolts[Device] ;
     iDACMax := LabIO.DACMaxValue[Device] ;
     iDACMin := LabIO.DACMinValue[Device] ;

     NumPoints := Max( Round(Time/MainFrm.ADCScanInterval),1) ;

     j := DACPointer[VChan]*DACNumChannels + DACChan ;
     jEnd := (LabIO.DAC[Device].EndOfBuf+1)*DACNumChannels -1 ;
     jShift := j + LabIO.DAC[Device].Pointer*DACNumChannels ;
     DACVoltsStart :=  DACScale*(DACVoltsStart + MainFrm.VCommand[VChan].HoldingVoltage) ;
     DACVoltsEnd := DACScale*(DACVoltsEnd + MainFrm.VCommand[VChan].HoldingVoltage) ;
     DV := (DACVoltsEnd - DACVoltsStart)/NumPoints ;
     V := DACVoltsStart ;
     for i := 1 to NumPoints do begin
         if j <= jEnd then begin
            iDACValue := Round( V ) ;
            if iDACValue > iDACMax then iDACValue := iDACMax ;
            if iDACValue < iDACMin then iDACValue := iDACMin ;
            if jShift > jEnd then jShift := jShift - jEnd - 1 ;
            PDACBuf^[jShift] := iDACValue ;
            end ;
         j := j + DACNumChannels ;
         jShift := jShift + DACNumChannels ;
         V := V + DV ;
         end ;

     DACPointer[VChan] := DACPointer[VChan] + NumPoints ;

     end ;


procedure TStimulator.FillDigBuf(
          DigChannel : Integer ;
          Time : Single ;
          BitOn : Boolean ;
          DigBufs : Array of PBig32bitArray    // Pointers to digital buffers
          ) ;
{ -------------------------------------------------------------
  Set or clear the bit defined by "BitMask" in the series of "NumPoints"
  16 bit integer in the array "Buf", starting at "BPointer"
  Note. If a bit-set operation is required, BitMask should have that bit
  set to 1 (all others 0). For bit-clear operations, the required bit is
  set to 0, (all other bits to 1).
  ---------------------------------------------------------------------}
var
   Device : Integer ;
   i,j,jShift,jEnd : Integer ;
   Bit : SmallInt ;
   BitMask : SmallInt ;
   BitValue : SmallInt ;
   NumPoints : Integer ;
   PDigBuf : PBig32bitArray ;
begin

     if Time <= 0.0 then Exit ;

     // Exit if no hardware DAC channel configured for this voltage channel
     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) then Exit ;

     // Get hardware device for channel
     Device := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device ;
     if DigBufs[Device] = Nil Then Exit ;

     // Pointer to device DAC buffer
     PDigBuf := DigBufs[Device] ;

     // Create bit and bit mask values for digital channel
     Bit := LabIO.BitMask(LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel) ;
     for i := 0 to DigChannel-1 do Bit := Bit*2 ;

     // Quit if no output bit available for digital channel
     if Bit > LabIO.BitMask(LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel) then Exit ;

     // Set bit mask and bit value to be set
     BitMask := not Bit ;
     if BitOn then BitValue := Bit
              else BitValue := 0 ;

     NumPoints := Max( Trunc(Time/MainFrm.ADCScanInterval ),0 ) ;

     { Clear the bit defined by BitMask, all other bits left untouched }
     j := DigPointer[DigChannel] ;
     jEnd := LabIO.DIG[Device].EndOfBuf ;
     jShift := j + LabIO.DAC[Device].Pointer ;
     for i := 1 to NumPoints do begin
         if j <= jEnd then begin
            if jShift > jEnd then jShift := jShift - jEnd - 1 ;
            PDigBuf^[jShift] := (PDigBuf^[jShift] and BitMask) or BitValue ;
            end ;
         j := j + 1 ;
         jShift := jShift + 1 ;
         end ;

     j := DigPointer[DigChannel] ;
     jEnd := LabIO.DIG[Device].EndOfBuf ;
     jShift := j + LabIO.DAC[Device].Pointer ;
     for i := 1 to NumPoints do begin
         if j <= jEnd then begin
            if jShift > jEnd then jShift := jShift - jEnd - 1 ;
            PDigBuf^[jShift] := (PDigBuf^[jShift] and BitMask) or BitValue ;
            end ;
         j := j + 1 ;
         jShift := jShift + 1 ;
         end ;

    DigPointer[DigChannel] := DigPointer[DigChannel] + NumPoints ;

    end ;


procedure TStimulator.LoadProgram(
          const Name : string     { Voltage program file path }
          ) ;
{ --------------------------------
  Load a voltage program from file
  --------------------------------}
var
   FileHandle : Integer ;
   OldProg : TWaveformOld ;
   V3216Prog : TWaveformV3216 ;
   i, j : Integer ;
   NumBytes : Integer ;
begin

    // Open stimulus protocol file
    FileName := ChangeFileExt( Name, '.vpr' ) ;
    Prog.FileName := FileName ;
    FileHandle := FileOpen(  FileName, fmOpenReadWrite ) ;

    NumBytes := FileSeek( FileHandle, 0, 2 ) ;

    if NumBytes = SizeOf(Prog) then begin
       // Load protocol file
       // ------------------
       FileSeek( FileHandle, 0, 0 ) ;
       if FileRead(FileHandle,Prog,Sizeof(Prog)) <> Sizeof(Prog) then
          MessageDlg(format( 'LoadProgram : Unable to read %s ',
                             [FileName]), mtWarning,[mbOK],0) ;
    end

    else if NumBytes = SizeOf(V3216Prog) then begin
       // Load V3.2.16 and previous format protocol file
       // -----------------------------
       FileSeek( FileHandle, 0, 0 ) ;
       if FileRead(FileHandle,V3216Prog,Sizeof(V3216Prog)) <> Sizeof(V3216Prog) then
          MessageDlg(format( 'LoadProgram : Unable to read %s ',
                             [FileName]), mtWarning,[mbOK],0) ;

       // Clear all waveshapes
       ClearWaveformElements ;

       // Copy to new format
       for i := 0 to MaxV3216WaveShapes-1 do begin
           // Swap wvDigNone for wvNone in digital elements
           Prog.Shape[i] := V3216Prog.Shape[i] ;
           Prog.Amplitude[i] := V3216Prog.Amplitude[i] ;
           Prog.Duration[i] := V3216Prog.Duration[i] ;
           Prog.Increment[i] := V3216Prog.Increment[i] ;
           Prog.RampStart[i] := V3216Prog.RampStart[i] ;
           Prog.RampEnd[i] := V3216Prog.RampEnd[i] ;
           Prog.PulseInterval[i] := V3216Prog.PulseInterval[i] ;
           Prog.NumPulses[i] := V3216Prog.NumPulses[i] ;
           Prog.Period[i] := V3216Prog.Period[i] ;
           Prog.Delay[i] := V3216Prog.Delay[i] ;
           Prog.Invert[i] := V3216Prog.Invert[i] ;
           end ;
       Prog.HoldingVoltage[0] := V3216Prog.HoldingVoltage[0] ;
       Prog.HoldingVoltage[1] := V3216Prog.HoldingVoltage[1] ;
       Prog.HoldingVoltage[2] := 0.0 ;
       Prog.RecordInterval := V3216Prog.RecordInterval ;
       Prog.RecordDuration := V3216Prog.RecordDuration ;
       Prog.NumSteps := V3216Prog.NumSteps ;
       Prog.NumRepeats := V3216Prog.NumRepeats ;
       Prog.dt := V3216Prog.dt ;
       Prog.DACdt := V3216Prog.DACdt ;

       for j := 0 to High(Prog.ExtWaveData) do
           Prog.ExtWaveData[j] := V3216Prog.ExtWaveData[j] ;
       Prog.ExtEndOfData := V3216Prog.ExtEndOfData ;
       Prog.ExtDACdt := V3216Prog.ExtDACdt ;
       Prog.FileName := V3216Prog.FileName ;
       Prog.NextFile := V3216Prog.NextFile ;
       Prog.NumDACPoints := V3216Prog.NumDACPoints ;
       Prog.NotInUse := V3216Prog.NotInUse ;
       Prog.NumLeaks := V3216Prog.NumLeaks ;
       Prog.DigitalInUse := V3216Prog.DigitalInUse ;
       Prog.DigitalPortValue := V3216Prog.DigitalPortValue ;
       Prog.RecordingStart := V3216Prog.RecordingStart ;
       Prog.InUse := V3216Prog.InUse ;
       Prog.Saved := V3216Prog.Saved ;
       Prog.RecordIntervalChanged := V3216Prog.RecordIntervalChanged ;
       Prog.LeakScale := V3216Prog.LeakScale ;
       Prog.NextProtocolFileName := V3216Prog.NextProtocolFileName ;

       // Save as new format
       FileSeek( FileHandle, 0, 0 ) ;
       if FileWrite(FileHandle,V3216Prog,Sizeof(V3216Prog)) <> Sizeof(V3216Prog) then
          MessageDlg(format( 'LoadProgram : Unable to write to %s ',
                             [FileName]), mtWarning,[mbOK],0) ;

    end

    else begin
       // Load old format protocol file
       // -----------------------------
       FileSeek( FileHandle, 0, 0 ) ;
       if FileRead(FileHandle,OldProg,Sizeof(OldProg)) <> Sizeof(OldProg) then
          MessageDlg(format( 'LoadProgram : Unable to read %s ',
                             [FileName]), mtWarning,[mbOK],0) ;

       // Clear all waveshapes
       ClearWaveformElements ;

       // Copy to new format
       for i := 0 to MaxOldWaveShapes-1 do begin
           // Swap wvDigNone for wvNone in digital elements
           Prog.Shape[i] := OldProg.Shape[i] ;
           if (i >= 10) and (OldProg.Shape[i] = wvNone) then
              Prog.Shape[i] := wvDigNone ;
           Prog.Amplitude[i] := OldProg.Amplitude[i] ;
           Prog.Duration[i] := OldProg.Duration[i] ;
           Prog.Increment[i] := OldProg.Increment[i] ;
           Prog.RampStart[i] := OldProg.RampStart[i] ;
           Prog.RampEnd[i] := OldProg.RampEnd[i] ;
           Prog.PulseInterval[i] := OldProg.PulseInterval[i] ;
           Prog.NumPulses[i] := OldProg.NumPulses[i] ;
           Prog.Period[i] := OldProg.Period[i] ;
           Prog.Delay[i] := OldProg.Delay[i] ;
           Prog.Invert[i] := OldProg.Invert[i] ;
           end ;
       Prog.HoldingVoltage[0] := 0.0 ;
       Prog.HoldingVoltage[1] := 0.0 ;
       Prog.HoldingVoltage[2] := 0.0 ;
       Prog.RecordInterval := OldProg.RecordInterval ;
       Prog.RecordDuration := OldProg.RecordDuration ;
       Prog.NumSteps := OldProg.NumSteps ;
       Prog.NumRepeats := OldProg.NumRepeats ;
       Prog.dt := OldProg.dt ;
       Prog.DACdt := OldProg.DACdt ;

       for j := 0 to High(Prog.ExtWaveData) do
           Prog.ExtWaveData[j] := OldProg.ExtWaveData[j] ;
       Prog.ExtEndOfData := OldProg.ExtEndOfData ;
       Prog.ExtDACdt := OldProg.ExtDACdt ;
       Prog.FileName := OldProg.FileName ;
       Prog.NextFile := OldProg.NextFile ;
       Prog.NumDACPoints := OldProg.NumDACPoints ;
       Prog.NotInUse := OldProg.NotInUse ;
       Prog.NumLeaks := OldProg.NumLeaks ;
       Prog.DigitalInUse := OldProg.DigitalInUse ;
       Prog.DigitalPortValue := OldProg.DigitalPortValue ;
       Prog.RecordingStart := OldProg.RecordingStart ;
       Prog.InUse := OldProg.InUse ;
       Prog.Saved := OldProg.Saved ;
       Prog.RecordIntervalChanged := OldProg.RecordIntervalChanged ;
       Prog.LeakScale := OldProg.LeakScale ;
       Prog.NextProtocolFileName := OldProg.NextProtocolFileName ;

       // Save as new format
       FileSeek( FileHandle, 0, 0 ) ;
       if FileWrite(FileHandle,OldProg,Sizeof(OldProg)) <> Sizeof(OldProg) then
          MessageDlg(format( 'LoadProgram : Unable to write to %s ',
                             [FileName]), mtWarning,[mbOK],0) ;

    end ;

    // Close file
    if FileHandle >=0 then FileClose( FileHandle ) ;

    Prog.Saved := True ;
    Prog.RecordIntervalChanged := False ;

    end ;


procedure TStimulator.SaveProgram(
          const Name : string     { Voltage program file path }
          ) ;
{ --------------------------------
  Save voltage program to file
  --------------------------------}
var
   FileHandle : Integer ;
begin

    // Create file
    FileName := ChangeFileExt( Name, '.vpr' ) ;
    Prog.FileName := FileName ;
    FileHandle := FileCreate( FileName ) ;

    if FileWrite(FileHandle,Prog,Sizeof(Prog)) <> Sizeof(Prog) then
       MessageDlg('SaveProgram : File Write - Failed ',mtWarning,[mbOK],0) ;

    // Close file
    if FileHandle >=0 then FileClose( FileHandle ) ;

    Prog.Saved := True ;
    Prog.RecordIntervalChanged := False ;

    end ;


procedure TStimulator.CreateProgramList(
          var cbList : TComboBox
          ) ;
{ --------------------------------------------
  Compile a list of protocol files in the directory \winwcp\vprot
  and put the file names into a combo box
  --------------------------------------------------------------}
var
   SearchRec : TSearchRec ;
   First : Boolean ;
   FileFound : Integer ;
begin
     First := True ;
     cbList.Clear ;
     cbList.items.add( 'None' ) ;
     repeat
        { Find file }
        if First then
           FileFound := FindFirst( MainFrm.VProtDirectory + '*.vpr',
                                   faAnyFile,
                                   SearchRec )
        else
           FileFound := FindNext( SearchRec ) ;

        { Add file name (no extension or path) to list }
        if FileFound = 0 then cbList.items.Add(ExtractFileNameOnly(SearchRec.Name))
                        else FindClose(SearchRec.FindHandle) ;
        First := False ;
        Until FileFound <> 0 ;

     end ;


function TStimulator.ProtocolDuration : Single ;
{ --------------------------------------------
  Calculate total duration of voltage protocol
  --------------------------------------------}
const
  DigSumStart = 2 ;
var
   i,j,NumExtra : Integer ;
   TSum : Array[0..MaxProtocolShapes-1] of Single ;
   TMax : Single ;
   VSum : Array[0..MaxProtocolShapes-1] of Integer ;
   DigSum : Array[0..MaxProtocolShapes-1] of Integer ;

begin

     Result := Prog.NumSteps*Prog.RecordInterval ;
     Exit ;

     // Define shape-> plot channel mappings
     for i := V0Start to V0End do VSum[i] := 0 ;
     for i := V1Start to V1End do VSum[i] := 1 ;
     for i := V2Start to V2End do VSum[i] := 2 ;
     for i := DigStart to DigEnd do DigSum[i] := i - DigSumStart ;

     for i := 0 to MaxProtocolShapes-1 do TSum[i] := 0.0 ;

     { Add up time taken by all active waveform shapes }
     NumExtra := Stimulator.Prog.NumSteps - 1 ;

     for i := 0 to High(Stimulator.Prog.Shape) do begin
         case Stimulator.Prog.Shape[i] of
              { Analogue voltage waveforms. Occur one after the other
                   on DAC O/P channel 0) }
              wvStep0 :   begin
                  TSum[VSum[i]] := TSum[VSum[i]]
                                   + Stimulator.Prog.Duration[i]
                                   + Stimulator.Prog.Delay[i] ;
                  end ;
              wvStep1 : begin
                  TSum[VSum[i]] := TSum[VSum[i]]
                                   + Stimulator.Prog.Duration[i]
                                   + Stimulator.Prog.Delay[i] ;
                  end ;
              wvStep2 : begin
                  TSum[VSum[i]] := TSum[VSum[i]]
                                    + Stimulator.Prog.Duration[i]
                                    + Stimulator.Prog.Delay[i]
                                    + Max(Stimulator.Prog.Increment[i]*NumExtra,0. ) ;
                  end ;
              wvRamp : begin
                  TSum[VSum[i]] := TSum[VSum[i]]
                                   + Stimulator.Prog.Duration[i]
                                   + Stimulator.Prog.Delay[i] ;
                  end ;
              wvPTrain : begin
                 TSum[VSum[i]] := TSum[VSum[i]]
                                  + (Stimulator.Prog.Delay[i] +
                                  + Stimulator.Prog.NumPulses[i]*Stimulator.Prog.PulseInterval[i]) ;
                 end ;
              wvWave : begin
                 TSum[VSum[i]] := TSum[VSum[i]]
                                  + (Stimulator.Prog.ExtEndOfData+1)*MainFrm.ADCScanInterval ;
                 end ;

              { Digital waveforms. Each waveform output simultaneously (on bits 0..7) }
              wvDigStep0 : begin
                 TSum[DigSum[i]] := TSum[DigSum[i]] +
                                    Stimulator.Prog.Delay[i]
                                    + Stimulator.Prog.Duration[i] ;
                 end ;
              wvDigStep1 : begin
                 TSum[DigSum[i]] := TSum[DigSum[i]] +
                                    Stimulator.Prog.Delay[i]
                                    + Stimulator.Prog.Duration[i] +
                                    (Stimulator.Prog.Increment[i]*NumExtra) ;
                end ;
              wvDigTrain : begin
                 TSum[DigSum[i]] := TSum[DigSum[i]] +
                                    Stimulator.Prog.Delay[i] +
                                    (Stimulator.Prog.NumPulses[i]*Stimulator.Prog.PulseInterval[i]) ;
                end ;
              wvDigDelay : begin
                 TSum[DigSum[i]] := TSum[DigSum[i]] +
                                    Stimulator.Prog.Delay[i]
                                    + Stimulator.Prog.Duration[i] +
                                    (Stimulator.Prog.Increment[i]*NumExtra) ;
                end ;
              end ;
         end ;

     TMax := Stimulator.Prog.RecordInterval ;
     for i := 0 to NumDisplayChannels-1 do if TMax < TSum[i] then TMax := TSum[i] ;

     Result := TMax ;

     end ;


procedure TStimulator.ClearWaveformElements ;
// ---------------------------
// Clear all waveform elements
// ---------------------------
var
    i : Integer ;
begin
     { Set DAC0 elements to none }
     for i := V0Start to V0End do begin
         Stimulator.Prog.Shape[i] := wvNone ;
         Stimulator.Prog.Delay[i] := 0. ;
         Stimulator.Prog.Duration[i] := 0. ;
         end ;

     { Set DAC1 elements to none }
     for i := V1Start to V1End do begin
         Stimulator.Prog.Shape[i] := wvNone ;
         Stimulator.Prog.Delay[i] := 0. ;
         Stimulator.Prog.Duration[i] := 0. ;
         end ;

     { Set DAC2 elements to none }
     for i := V2Start to V2End do begin
         Stimulator.Prog.Shape[i] := wvNone ;
         Stimulator.Prog.Delay[i] := 0. ;
         Stimulator.Prog.Duration[i] := 0. ;
         end ;

     { Set digital elements to none }
     for i := DigStart to DigEnd do begin
         Stimulator.Prog.Shape[i] := wvDigNone ;
         Stimulator.Prog.Delay[i] := 0. ;
         Stimulator.Prog.Duration[i] := 0. ;
         end ;

     end ;

end.
