unit PhotoStimModule;

//------------------------------------------------------------------------------
// PhotoStimModule
// by Nicholas Schwarz
//------------------------------------------------------------------------------
// JD 21.12.10 .CreateWaveform() now ignores D/A outputs DACI and DACS if they
//              are not defined (DACI or DACS <0)


interface

uses
  SysUtils, Classes, Dialogs, Math, LabIOUnit, StdCtrls, SHared;

const
  MaxStimPoints = 100;                          // Max number of stimulus points

type

  // Packed record of stimulus protocol
  TWaveform = packed record
    X : Array[0..MaxStimPoints-1] of Single;    // X location in um
    Y : Array[0..MaxStimPoints-1] of Single;    // Y location in um
    PRE : Array[0..MaxStimPoints-1] of Single;  // Pre-delay in ms
    D : Array[0..MaxStimPoints-1] of Single;    // Duration in ms
    A : Array[0..MaxStimPoints-1] of Single;    // Amplitude in mW
    POST : Array[0..MaxStimPoints-1] of Single; // Post-delay in ms
    NumStimPoints : Integer;                    // Number of stim points
    FileName : String[255];                     // File name
    Saved : Boolean;                            // Saved flag
  end;

  // PhotoStimulator class
  TPhotoStimulator = class(TDataModule)

    private

      Prog : TWaveform;                            // Packed wavefrom record

    public

      FileName : String;                           // File name

      // Calculate linear stimulus drive voltage
      function CalculateLinearStimulusDriveVoltage(
         Min : Single;                             // Minimum power
         Max : Single;                             // Maximum power
         VMin : Single;                            // Minimum voltage
         VMax : Single;                            // Maximum voltage
         Amplitude : Single) : Single ;            // Desired amplitude in mW

      // Calculate Pockels cell stimulus drive voltage
      function CalculatePCStimulusDriveVoltage(
         Min : Single;                             // Minimum power
         Max : Single;                             // Maximum power
         VBias : Single;                           // Pockels cell bias setting
         VPi : Single;                             // VPi for Pockels cell
         Amplitude : Single;                       // Desired amplitude in mW
         PolarizationCross : Boolean) : Single ;   // Cross-polarization (sin)

      // Clear protocol
      procedure ClearWaveformElements;

      // Fill combo box with list of protocols in pprot directory
      procedure CreateProgramList(var cbList : TComboBox);

      // Create waveform in given array
      procedure CreateWaveform(DACBufs : Array of PBig16bitArray;
                               DACX : Integer;
                               DACY : Integer;
                               DACI : Integer;
                               DACS : Integer;
                               DACScale : Single;
                               DACNumScansInBuffer : Integer;
                               Device : Integer;
                               StimulusEnabled : Boolean;
                               DACNumChannels : Integer;
                               SampleOffset : Integer);

      // Get protocol parameters
      procedure GetProtocol(
        var X : Array of Single;                   // X location in um
        var Y : Array of Single;                   // Y location in um
        var PRE : Array of Single;                 // Pre-delay in ms
        var D : Array of Single;                   // Duration in ms
        var A : Array of Single;                   // Amplitude in mW
        var POST : Array of Single;                // Post-delay in ms
        var NumStimPoints : Integer);              // Number of stim points

      // Is the protocol saved
      function IsSaved : Boolean;

      // Load protocol from file
      procedure LoadProgram(const Name : String);

      // Return protocol duration in ms
      function ProtocolDuration : Single;

      // Save protocol to file
      procedure SaveProgram(const Name : String);

      // Set protocol parameters
      procedure SetProtocol(
        X : Array of Single;                       // X location in um
        Y : Array of Single;                       // Y location in um
        PRE : Array of Single;                     // Pre-delay in ms
        D : Array of Single;                       // Duration in ms
        A : Array of Single;                       // Amplitude in mW
        POST : Array of Single;                    // Post-delay in ms
        NumStimPoints : Integer);                  // Number of stim points

  end;

var
  PhotoStimulator: TPhotoStimulator;

implementation

uses Main, LogUnit;

{$R *.dfm}

//------------------------------------------------------------------------------

function TPhotoStimulator.CalculateLinearStimulusDriveVoltage(
         Min : Single;                             // Minimum power
         Max : Single;                             // Maximum power
         VMin : Single;                            // Minimum voltage
         VMax : Single;                            // Maximum voltage
         Amplitude : Single) : Single ;            // Desired amplitude in mW
begin

  // Initialize result
  Result := 0.0;

  // Exit function if Min = Max
  if (Min = Max) then Exit;

  // Calculate linear ramp
  Result := ((Amplitude - Min) / (Max - Min)) * (VMax - VMin) + VMin;

end;

//------------------------------------------------------------------------------

function TPhotoStimulator.CalculatePCStimulusDriveVoltage(
         Min : Single;                             // Minimum power
         Max : Single;                             // Maximum power
         VBias : Single;                           // Pockels cell bias setting
         VPi : Single;                             // VPi for Pockels cell
         Amplitude : Single;                       // Desired amplitude in mW
         PolarizationCross : Boolean) : Single ;   // Cross-polarization (sin)
var
  Modulation : Single;
begin

  // Initialize result
  Result := 0.0;

  // Determine the term in the square root below
  if Max <> Min then Modulation := (Amplitude - Min) / (Max - Min)
                else Modulation := 0.0 ;

  // Assign minimum power if the power is set too low
  if Modulation < 0.0 then
    Modulation := 0.0;

  // Calculate the final drive voltage
  if PolarizationCross then
    Result :=
      (Vpi * (2/Pi) * ArcSin(sqrt(Modulation))) + ((Pi/2) * (VBias / 375.0));
  if Not PolarizationCross then
    Result :=
      (Vpi * (2/Pi) * ArcCos(sqrt(Modulation))) + ((Pi/2) * (VBias / 375.0));

end;

//------------------------------------------------------------------------------

procedure TPhotoStimulator.ClearWaveformElements;
var
  i : Integer;
begin

  // Set values to 0.0
  for i := 0 to MaxStimPoints-1 do
  begin
    Prog.X[i] := 0.0;
    Prog.Y[i] := 0.0;
    Prog.PRE[i] := 0.0;
    Prog.D[i] := 0.0;
    Prog.A[i] := 0.0;
    Prog.POST[i] := 0.0;
  end;

  Prog.NumStimPoints := 0;
  Prog.FileName := '';

  // Update saved flag
  Prog.Saved := False;  

end;

//------------------------------------------------------------------------------

procedure TPhotoStimulator.CreateProgramList(var cbList : TComboBox);
var
  SearchRec : TSearchRec;
  First : Boolean;
  FileFound : Integer;
begin

  First := True;
  cbList.Clear;
  cbList.Items.Add('None');

  repeat

    // Find file
    if First then
      FileFound := FindFirst(MainFrm.PProtDirectory + '*.ppr',
                             faAnyFile,
                             SearchRec)
    else
      FileFound := FindNext(SearchRec);

    //Add file name (no extension or path) to list
    if FileFound = 0 then cbList.Items.Add(ExtractFileNameOnly(SearchRec.Name))
    else FindClose(SearchRec);

    First := False;

  until FileFound <> 0;

end;

//------------------------------------------------------------------------------

procedure TPhotoStimulator.CreateWaveform(DACBufs : Array of PBig16bitArray;
                                          DACX : Integer; // X galvo DAC channel
                                          DACY : Integer; // Y galvo DAC channel
                                          DACI : Integer; // Attenuator DAC (<0 = unused)
                                          DACS : Integer; // Shutter DAC (<0 = unused)
                                          DACScale : Single;
                                          DACNumScansInBuffer : Integer;
                                          Device : Integer;
                                          StimulusEnabled : Boolean;
                                          DACNumChannels : Integer;
                                          SampleOffset : Integer);


var

  iPreDelay : Integer;     // Pre-delay in samples
  iDuration : Integer;     // Duration in samples
  iPostDelay : Integer;    // Post-delay in samples

  iStim : Integer;         // Stimulus point
  iPos : Integer;          // Position in DAC buffer in samples

  i, j : Integer;          // Counters

  DACXVolts, DACYVolts, DACIVolts, DACSVolts : Single;
  iDACXValue, iDACYValue, iDACIValue, iDACSValue : Integer;

  XRotated, YRotated : Single; // Rotated coordinates

  // Attenuator settings
  pcpmin, pcpmax, lpmin, lpmax, vmin, vmax, bias, pi : Single;
  cross, pcenable : Boolean;

begin

LogFrm.AddLine('Sample Offset: ' + IntToStr(SampleOffset));
  // Determine attenuator settings for active attenuator
  pcpmin := MainFrm.PhotoStim.PCPowerMin[MainFrm.PhotoStim.Attenuator];
  pcpmax := MainFrm.PhotoStim.PCPowerMax[MainFrm.PhotoStim.Attenuator];
  lpmin := MainFrm.PhotoStim.LinearPowerMin[MainFrm.PhotoStim.Attenuator];
  lpmax := MainFrm.PhotoStim.LinearPowerMax[MainFrm.PhotoStim.Attenuator];
  vmin := MainFrm.PhotoStim.LinearVoltageMin[MainFrm.PhotoStim.Attenuator];
  vmax := MainFrm.PhotoStim.LinearVoltageMax[MainFrm.PhotoStim.Attenuator];
  bias := MainFrm.PhotoStim.PCBias[MainFrm.PhotoStim.Attenuator];
  pi := MainFrm.PhotoStim.PCVoltagePi[MainFrm.PhotoStim.Attenuator];
  cross := MainFrm.PhotoStim.PCPolarizationCross[MainFrm.PhotoStim.Attenuator];
  pcenable := MainFrm.PhotoStim.PCEnable[MainFrm.PhotoStim.Attenuator];


  // Initial X/Y galvo positions
  iDACXValue := Round((MainFrm.PhotoStim.XCenter[MainFrm.PhotoStim.Attenuator] +
    MainFrm.PhotoStim.ROIXVoltageOffset) * DACScale);
  iDACYValue := Round((MainFrm.PhotoStim.YCenter[MainFrm.PhotoStim.Attenuator] +
    MainFrm.PhotoStim.ROIYVoltageOffset) * DACScale);

  // Calculate initial (off) attenuator drive voltage
  if pcenable then
  begin
    DACIVolts := CalculatePCStimulusDriveVoltage(pcpmin,
                                                 pcpmax,
                                                 bias,
                                                 pi,
                                                 0.0,
                                                 cross);
  end
  else
  begin
    DACIVolts := CalculateLinearStimulusDriveVoltage(lpmin,
                                                     lpmax,
                                                     vmin,
                                                     vmax,
                                                     0.0);
  end;

  // Scale and convert voltage from Single to 16-bit Integer
  iDACIValue := Round(DACScale * DACIVolts);

  // Open the shutter for the duration if stimulus is enabled
  if StimulusEnabled then
  begin
    // Determine open shutter signal
    if ((MainFrm.PhotoStim.Attenuator = 1) and
        MainFrm.PhotoStim.EnableShutter[1]) or
       ((MainFrm.PhotoStim.Attenuator = 2) and
        MainFrm.PhotoStim.EnableShutter[2]) or
       ((MainFrm.PhotoStim.Attenuator = 3) and
        MainFrm.PhotoStim.EnableShutter[3]) then
    begin
      if MainFrm.IOConfig.PhotoStimShutterActiveHigh then
        iDACSValue := Round(5.0 * DACScale)
      else
        iDACSValue := Round(0.0 * DACScale);
    end;
  end
  else
  begin
    // Determine closed shutter signal
    if MainFrm.IOConfig.PhotoStimShutterActiveHigh then
      iDACSValue := Round(0.0 * DACScale)
    else
      iDACSValue := Round(5.0 * DACScale);
  end;

  // Fill buffer with initial values
  for i := 0 to DACNumScansInBuffer - 1 do
  begin
    j := i*DACNumChannels;
    DACBufs[Device]^[j+DACX] := iDACXValue;
    DACBufs[Device]^[j+DACY] := iDACYValue;
    if DACI >= 0 then DACBufs[Device]^[j+DACI] := iDACIValue;
    if DACS >= 0 then DACBufs[Device]^[j+DACS] := iDACSValue;
  end;

  // Set default DAC channel values
  LabIO.DACOutState[Device, DACX] :=
    MainFrm.PhotoStim.XCenter[MainFrm.PhotoStim.Attenuator] +
    MainFrm.PhotoStim.ROIXVoltageOffset;
  LabIO.DACOutState[Device, DACY] :=
    MainFrm.PhotoStim.YCenter[MainFrm.PhotoStim.Attenuator] +
    MainFrm.PhotoStim.ROIYVoltageOffset;
  if DACI >= 0 then LabIO.DACOutState[Device, DACI] := DACIVolts;
  if DACS >= 0 then begin
     if MainFrm.IOConfig.PhotoStimShutterActiveHigh then
        LabIO.DACOutState[Device, DACS] := 0.0
     else
        LabIO.DACOutState[Device, DACS] := 5.0;
  end ;

  // Stop here if stimulus is not enabled
  if not StimulusEnabled then Exit;


  // Create values for each photo-stimulus point
  iPos := 0 ;
  for iStim := 0 to Prog.NumStimPoints-1 do
  begin

    // Convert pre-delay from ms to number of samples
    iPreDelay :=
      Max(Round((Prog.PRE[iStim] / 1000.0) / MainFrm.ADCScanInterval), 1);

    // Convert duration from ms to number of samples
    iDuration :=
      Max(Round((Prog.D[iStim] / 1000.0) / MainFrm.ADCScanInterval), 1);

    // Convert post-delay from ms to number of samples
    iPostDelay :=
      Max(Round((Prog.POST[iStim] / 1000.0) / MainFrm.ADCScanInterval), 1);

    // Rotate coordinate to account for image rotation
    XRotated :=
      Cos(-MainFrm.PhotoStim.ImageRotation) * Prog.X[iStim] -
      Sin(-MainFrm.PhotoStim.ImageRotation) * Prog.Y[iStim];
    YRotated :=
      Sin(-MainFrm.PhotoStim.ImageRotation) * Prog.X[iStim] +
      Cos(-MainFrm.PhotoStim.ImageRotation) * Prog.Y[iStim];

    // Apply coordinate calibration
    DACXVolts :=
      (XRotated * MainFrm.PhotoStim.XScale[MainFrm.PhotoStim.Attenuator]) +
      MainFrm.PhotoStim.XCenter[MainFrm.PhotoStim.Attenuator] -
      MainFrm.PhotoStim.ROIXVoltageOffset;
    iDACXValue := Round(DACScale * DACXVolts);
    DACYVolts :=
      (YRotated * MainFrm.PhotoStim.YScale[MainFrm.PhotoStim.Attenuator]) +
      MainFrm.PhotoStim.YCenter[MainFrm.PhotoStim.Attenuator] -
      MainFrm.PhotoStim.ROIYVoltageOffset;
    iDACYValue := Round(DACScale * DACYVolts);

    // Calculate attenuator drive voltage
    if pcenable then
    begin
      DACIVolts := CalculatePCStimulusDriveVoltage(pcpmin,
                                                   pcpmax,
                                                   bias,
                                                   pi,
                                                   Prog.A[iStim],
                                                   cross);
    end
    else
    begin
      DACIVolts := CalculateLinearStimulusDriveVoltage(lpmin,
                                                       lpmax,
                                                       vmin,
                                                       vmax,
                                                       Prog.A[iStim]);
    end;

    // Scale and convert voltage from Single to 16-bit Integer
    iDACIValue := Round(DACScale * DACIVolts);


    // Pre-delay
    for i := 1 to iPreDelay do
    begin
      j := (iPos + SampleOffset) * DACNumChannels;
      DACBufs[Device]^[j+DACX] := iDACXValue;
      DACBufs[Device]^[j+DACY] := iDACYValue;
      if DACS >= 0 then DACBufs[Device]^[j+DACS] := iDACSValue;
      iPos := Min(iPos + 1, DACNumScansInBuffer - 1);
    end;

    // Apply photo-stimulation pulse for duration
    for i := 1 to iDuration do
    begin
      j := (iPos + SampleOffset) * DACNumChannels;
      DACBufs[Device]^[j+DACX] := iDACXValue;
      DACBufs[Device]^[j+DACY] := iDACYValue;
      if DACI >= 0 then DACBufs[Device]^[j+DACI] := iDACIValue;
      if DACS >= 0 then DACBufs[Device]^[j+DACS] := iDACSValue;
      iPos := Min(iPos + 1, DACNumScansInBuffer - 1);
    end;

    // Post-delay
    for i := 1 to iPostDelay do
    begin
      j := (iPos + SampleOffset) * DACNumChannels;
      DACBufs[Device]^[j+DACX] := iDACXValue;
      DACBufs[Device]^[j+DACY] := iDACYValue;
      if DACS >= 0 then DACBufs[Device]^[j+DACS] := iDACSValue;
      iPos := Min(iPos + 1, DACNumScansInBuffer - 1);
    end;

  end;  // End for-loop for each stimulus point


end;

//------------------------------------------------------------------------------

procedure TPhotoStimulator.GetProtocol(
          var X : Array of Single;
          var Y : Array of Single;
          var PRE : Array of Single;
          var D : Array of Single;
          var A : Array of Single;
          var POST : Array of Single;
          var NumStimPoints : Integer);
var
  i : Integer;
begin

  NumStimPoints := Prog.NumStimPoints;

  for i := 0 to NumStimPoints-1 do
  begin
    X[i] := Prog.X[i];
    Y[i] := Prog.Y[i];
    PRE[i] := Prog.PRE[i];
    D[i] := Prog.D[i];
    A[i] := Prog.A[i];
    POST[i] := Prog.POST[i];
  end;

end;

//------------------------------------------------------------------------------

function TPhotoStimulator.IsSaved : Boolean;
begin

  // Return program saved flag
  Result := Prog.Saved;

end;

//------------------------------------------------------------------------------

procedure TPhotoStimulator.LoadProgram(const Name : String);
var
  FileHandle : Integer;
  FileName : String;
  NumBytes : Integer;
begin

  // Open photo-stimulus protocol file
  FileName := ChangeFileExt(Name, '.ppr');
  Prog.FileName := FileName;
  FileHandle := FileOpen(FileName, fmOpenReadWrite);

  // Get size of data
  NumBytes := FileSeek(FileHandle, 0, 2);

  // Check that size matches what it should be
  if NumBytes = SizeOf(Prog) then
  begin

    // Read data
    FileSeek(FileHandle, 0, 0);
    if FileRead(FileHandle, Prog, Sizeof(Prog)) <> Sizeof(Prog) then
    begin
      MessageDlg(format('LoadProgram: Unable to read %s ', [FileName]),
                 mtWarning, [mbOK], 0);
    end;

  end;

  // Close file
  if FileHandle >= 0 then FileClose(FileHandle);

  // Update saved flag
  Prog.Saved := True;

end;

//------------------------------------------------------------------------------

function TPhotoStimulator.ProtocolDuration : Single;
var
  i : Integer;
begin

  // Initialize total duration
  Result := 0.0;

  // Loop through points summing duration
  for i := 0 to Prog.NumStimPoints - 1 do
  begin
    Result := Result + Prog.PRE[i] + Prog.D[i] + Prog.POST[i];
  end;

  // Covert to seconds from ms
  Result := Result / 1000.0;

end;

//------------------------------------------------------------------------------

procedure TPhotoStimulator.SaveProgram(const Name : String);
var
  FileHandle : Integer;
begin

  // Create file
  FileName := ChangeFileExt(Name, '.ppr');
  Prog.FileName := FileName;
  FileHandle := FileCreate(FileName);

  // Write
  if FileWrite(FileHandle, Prog, Sizeof(Prog)) <> Sizeof(Prog) then
    MessageDlg('SaveProgram : File write failed', mtWarning, [mbOK], 0);

  // Close file
  if FileHandle >=0 then
    FileClose(FileHandle);

  // Update saved flag
  Prog.Saved := True;

end;

//------------------------------------------------------------------------------

procedure TPhotoStimulator.SetProtocol(
          X : Array of Single;
          Y : Array of Single;
          PRE : Array of Single;
          D : Array of Single;
          A : Array of Single;
          POST : Array of Single;
          NumStimPoints : Integer);
var
  i : Integer;
begin

  Prog.NumStimPoints := NumStimPoints;

  for i := 0 to NumStimPoints-1 do
  begin
    Prog.X[i] := X[i];
    Prog.Y[i] := Y[i];
    Prog.PRE[i] := PRE[i];
    Prog.D[i] := D[i];
    Prog.A[i] := A[i];
    Prog.POST[i] := POST[i];
  end;

  // Update saved flag
  Prog.Saved := False;

end;

//------------------------------------------------------------------------------

end.
