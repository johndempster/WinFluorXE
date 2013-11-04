unit PlaybackStimModule;

//------------------------------------------------------------------------------
// PlaybackStimulator
// by Nicholas Schwarz
//------------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, Dialogs, Math, LabIOUnit, StdCtrls, SHared;

type

  // Double array type
  TDoubleArray = Array[0..$FFFFFF] of Double;
  PDoubleArray = ^TDoubleArray;

  // Playback stimulator class
  TPlaybackStimulator = class(TDataModule)

  private

    // Data array
    m_Waveform: TDoubleArray;

    // Number of samples in data array
    m_NumSamples: Integer;

    // Voltage output settings
    // Used to control Pockels Cell during playback
    m_VoltageOffset: Integer;
    m_VoltageNumSamples: Integer;
    m_VOut : Array[0..2] of Single;

  public

    // Create waveform in given array
    procedure CreateWaveform(DACBufs : Array of PBig16bitArray;
                             DACCh : Integer;
                             VChan : Integer;
                             DACNumScansInBuffer : Integer;
                             Device : Integer;
                             StimulusEnabled : Boolean;
                             DACNumChannels : Integer);

    // Return protocol duration in ms
    function ProtocolDuration : Single;

    // Set protocol parameters
    procedure SetProtocol(
      Data : Pointer;               // Pointer to data
      StartSample : Integer;        // Start sample
      NumSamples : Integer;         // Number of samples in array
      ADCScale : Double;            // ADCScale for converting to real units
      Units: String;                // Units, e.g. mV
      VoltageStartSample : Integer; // Start sample for voltage output
      VoltageNumSamples : Integer;  // Number of samples for voltage output
      Voltage0 : Single;            // Voltage output values in volts
      Voltage1 : Single;
      Voltage2 : Single);

  end;

var
  PlaybackStimulator: TPlaybackStimulator;

implementation

uses Main, LogUnit;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TPlaybackStimulator.CreateWaveform(DACBufs : Array of PBig16bitArray;
                                             DACCh : Integer;
                                             VChan : Integer;
                                             DACNumScansInBuffer : Integer;
                                             Device : Integer;
                                             StimulusEnabled : Boolean;
                                             DACNumChannels : Integer);
var
  ch, i, j : Integer;
  DACValue : Integer;
  DACScale : Single;
begin

  // Set V->DAC scaling factor
  if MainFrm.VCommand[VChan].DivideFactor <= 0.0
    then MainFrm.VCommand[VChan].DivideFactor := 1.0 ;
  DACScale := (LabIO.DACMaxValue[Device]*MainFrm.VCommand[VChan].DivideFactor) /
    LabIO.DACMaxVolts[Device];

  // Fill DAC buffer with playback protocol
  for i := 0 to Min(DACNumScansInBuffer, m_NumSamples) - 1 do
  begin

    // Calculate DAC value and apply holding voltage
    DACValue := Round(DACScale *
                      (m_Waveform[i] + MainFrm.VCommand[VChan].HoldingVoltage));
    DACValue := Min(DACValue, LabIO.DACMaxValue[Device]);
    DACValue := Max(DACValue, -LabIO.DACMaxValue[Device] - 1);

    // Assign value
    j := i * DACNumChannels;
    DACBufs[Device]^[j+DACCh] := DACValue;

  end;

  // Fill other command voltage channels with user specified values
  for ch := 0 to 2 do
  begin

    // Fill buffers for channels other than playback channel
    if not (ch = VChan) then
    begin

      // Skip if channel is not configured
      if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[ch]) then
        Continue;

      // Set V->DAC scaling factor
      if MainFrm.VCommand[ch].DivideFactor <= 0.0
        then MainFrm.VCommand[ch].DivideFactor := 1.0 ;
      DACScale :=
        (LabIO.DACMaxValue[Device]*MainFrm.VCommand[ch].DivideFactor) /
        LabIO.DACMaxVolts[Device];
      DACCh := LabIO.Resource[MainFrm.IOConfig.VCommand[ch]].StartChannel;


      // Fill DAC buffer with playback protocol
      for i := m_VoltageOffset to
               Min(DACNumScansInBuffer, m_VoltageOffset + m_VoltageNumSamples) - 1 do
      begin

        // Calculate DAC value and apply holding voltage
        DACValue := Round(DACScale *
           (m_VOut[ch] + MainFrm.VCommand[ch].HoldingVoltage));
        DACValue := Min(DACValue, LabIO.DACMaxValue[Device]);
        DACValue := Max(DACValue, -LabIO.DACMaxValue[Device] - 1);

        // Assign value
        j := i * DACNumChannels;
        DACBufs[Device]^[j+DACCh] := DACValue;

      end;

    end;

  end;

end;

//------------------------------------------------------------------------------

function TPlaybackStimulator.ProtocolDuration : Single;
begin

  // Caclulate total duration
  Result := m_NumSamples * MainFrm.ADCScanInterval;

end;

//------------------------------------------------------------------------------

procedure TPlaybackStimulator.SetProtocol(Data : Pointer;
                                          StartSample : Integer;
                                          NumSamples : Integer;
                                          ADCScale : Double;
                                          Units : String;
                                          VoltageStartSample : Integer;
                                          VoltageNumSamples : Integer;
                                          Voltage0 : Single;
                                          Voltage1 : Single;
                                          Voltage2 : Single);
var
  i, j : Integer;    // Loop indices
  p : PDoubleArray;  // Pointer to data
begin

  // Cast pointer to PDoublArray
  p := PDoubleArray(Data);

  // Assign number of samples
  m_NumSamples := NumSamples;

  // Convert to Volts or Amps
  if Units = 'pV' then ADCScale := ADCScale / 1E+12
  else if Units = 'nV' then ADCScale := ADCScale / 1E+9
  else if Units = 'uV' then ADCScale := ADCScale / 1E+6
  else if Units = 'mV' then ADCScale := ADCScale / 1000.0
  else if Units = 'cV' then ADCScale := ADCScale / 100.0
  else if Units = 'dV' then ADCScale := ADCScale / 10.0
  else if Units = 'pA' then ADCScale := ADCScale / 1E+12
  else if Units = 'nA' then ADCScale := ADCScale / 1E+9
  else if Units = 'uA' then ADCScale := ADCScale / 1E+6
  else if Units = 'mA' then ADCScale := ADCScale / 1000.0
  else if Units = 'cA' then ADCScale := ADCScale / 100.0
  else if Units = 'dA' then ADCScale := ADCScale / 10.0;

  // Copy waveform to local storage making first value 0 volts
  j := 0;
  for i := StartSample to ((StartSample + NumSamples) - 1) do
  begin
    m_Waveform[j] := (p^[i] * ADCScale) - (p^[StartSample] * ADCScale);
    Inc(j);
  end;

  // Voltage output settings
  m_VoltageOffset := VoltageStartSample - StartSample;
  m_VoltageNumSamples := VoltageNumSamples;
  m_VOut[0] := Voltage0;
  m_VOut[1] := Voltage1;
  m_VOut[2] := Voltage2;

end;

//------------------------------------------------------------------------------

end.

//------------------------------------------------------------------------------
