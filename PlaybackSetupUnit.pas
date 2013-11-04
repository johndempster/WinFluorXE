unit PlaybackSetupUnit;

//------------------------------------------------------------------------------
// PlaybackSetupUnit
// by Nicholas Schwarz
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ExtCtrls, ScopeDisplay, IDRFile, Math;

type

  TSmallIntArray = Array[0..$FFFFFF] of SmallInt;
  PSmallIntArray = ^TSmallIntArray;

  TDoubleArray = Array[0..$FFFFFF] of Double;
  PDoubleArray = ^TDoubleArray;

  TPlaybackSetupFrm = class(TForm)
    bCancel: TButton;
    bOK: TButton;
    bOpenIDR: TButton;
    cbChannel: TComboBox;
    LabelChannel: TLabel;
    sdDisplay: TScopeDisplay;
    sbDisplay: TScrollBar;
    pnlDisplay: TPanel;
    edDisplay: TValidatedEdit;
    rbDisplayUnitMins: TRadioButton;
    rbDisplayUnitsSecs: TRadioButton;
    btnDisplayDouble: TButton;
    btnDisplayHalf: TButton;
    LabelStart: TLabel;
    LabelStop: TLabel;
    edStart: TValidatedEdit;
    edStop: TValidatedEdit;
    OpenDialog: TOpenDialog;
    IDRFile: TIDRFile;
    Label1: TLabel;
    edStartPS: TValidatedEdit;
    rbVOut0: TRadioButton;
    rbVOut1: TRadioButton;
    rbVOut2: TRadioButton;
    LabelVOut: TLabel;
    edVOut0: TValidatedEdit;
    edVOutStop: TValidatedEdit;
    edVOutStart: TValidatedEdit;
    lblV0: TLabel;
    lblStop0: TLabel;
    lblStart0: TLabel;
    lblVOut0: TLabel;
    edVOut1: TValidatedEdit;
    lblV1: TLabel;
    edVOut2: TValidatedEdit;
    lblV2: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCancelClick(Sender: TObject);
    procedure cbChannelChange(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bOpenIDRClick(Sender: TObject);
    procedure btnDisplayDoubleClick(Sender: TObject);
    procedure btnDisplayHalfClick(Sender: TObject);
    procedure edDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbDisplayUnitMinsClick(Sender: TObject);
    procedure rbDisplayUnitsSecsClick(Sender: TObject);
    procedure sbDisplayChange(Sender: TObject);
    procedure sdDisplayCursorChange(Sender: TObject);
    procedure edStartKeyPress(Sender: TObject; var Key: Char);
    procedure edStopKeyPress(Sender: TObject; var Key: Char);
    procedure edStartPSKeyPress(Sender: TObject; var Key: Char);
    procedure edVOutStartKeyPress(Sender: TObject; var Key: Char);
    procedure edVOutStopKeyPress(Sender: TObject; var Key: Char);    
    procedure rbVOut0Click(Sender: TObject);
    procedure rbVOut1Click(Sender: TObject);
    procedure rbVOut2Click(Sender: TObject);

  private

    // Sample buffer
    ADCBuf: Array[0..4096] of SmallInt;

    // Sample step size
    m_SampleStep: Integer;

    // Start and stop times shown in scope display
    m_StartTime: Single;
    m_StopTime: Single;

    // Return bounded display time
    function BoundDisplayTime(v: Single): Single;

    // Disable window controls
    procedure DisableControls();

    // Enable window controls
    procedure EnableControls();

    // Resize widgets
    procedure Resize();

    // Set units for time
    procedure SetDisplayUnits();

    // Update scope display
    procedure UpdateDisplay();

  public

  end;

var

  PlaybackSetupFrm: TPlaybackSetupFrm;

  // Data as a double
  m_Data: PDoubleArray;

implementation

uses Main, LogUnit, PlaybackStimModule;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin

  // Deallocate memory
  if m_Data <> Nil then begin
    FreeMem(m_Data);
    m_Data := Nil;
  end;

  // Close IDR file
  IDRFile.CloseFile;

  // Close window
  Action := caFree;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.FormResize(Sender: TObject);
begin

  // Resize window
  Resize();

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.FormShow(Sender: TObject);
var
  ch : Integer;  // Channel index
begin

  // Allocate memory for data as double
  GetMem(m_Data, SizeOf(TDoubleArray));

  // Set up A/D signals display window
  sdDisplay.DisableChannelVisibilityButton := True;  
  sdDisplay.NumChannels := 1;
  sdDisplay.MaxADCValue := IDRFile.ADCMaxValue;
  sdDisplay.MinADCValue := -IDRFile.ADCMaxValue - 1;
  sdDisplay.ClearHorizontalCursors;
  sdDisplay.ClearVerticalCursors;
  sdDisplay.ChanName[0] := 'Data';
  sdDisplay.Visible := True;

  // Add vertical cursors
  sdDisplay.ClearVerticalCursors;
  sdDisplay.AddVerticalCursor(-1, clGreen, '?t');    // Start, 0
  sdDisplay.AddVerticalCursor(-1, clGreen, '?t');    // Stop, 1
  sdDisplay.AddVerticalCursor(-1, clRed, '?t');      // Start photo-stimulus, 2
  sdDisplay.AddVerticalCursor(-1, clFuchsia, '?t');  // Start vout, 3
  sdDisplay.AddVerticalCursor(-1, clFuchsia, '?t');  // Stop vout, 4
  sdDisplay.LinkVerticalCursors(0, 1);
  sdDisplay.LinkVerticalCursors(3, 4);  

  // Display interval
  edDisplay.Value := MainFrm.Playback.DisplayInterval;

  // Display output channels
  rbVOut0.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[0]);
  rbVOut1.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[1]);
  rbVOut2.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[2]);

  // Restore output channel selection
  if MainFrm.Playback.VOutChannel = 0 then rbVOut0.Checked := True
  else if MainFrm.Playback.VOutChannel = 1 then rbVOut1.Checked := True
  else if MainFrm.Playback.VOutChannel = 2 then rbVOut2.Checked := True;

  // Display other output channels
  lblV0.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[0]);
  edVOut0.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[0]);
  lblV1.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[1]);
  edVOut1.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[1]);
  lblV2.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[2]);
  edVOut2.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[2]);

  // Disable controls until IDR is opened
  DisableControls();

  // Resize
  Resize();


  // If previous file exists, open it and restore window state
  if FileExists(MainFrm.Playback.FileName) then
  begin

    // Close file if already open
    if IDRFile.Open then IDRFile.CloseFile;

    // Open IDR file
    IDRFile.OpenFile(MainFrm.Playback.FileName);

    // Fill in the channel selector
    cbChannel.Clear;
    for ch := 0 to IDRFile.ADCNumChannels - 1 do
    begin
      cbChannel.Items.Add(IDRFile.ADCChannel[ch].ADCName);
    end;
    cbChannel.ItemIndex := MainFrm.Playback.SelectedChannel;

    // Change channel callback
    cbChannelChange(cbChannel);

    // Restore start and stop position
    sdDisplay.VerticalCursors[0] :=
      Round(MainFrm.Playback.Start / sdDisplay.TScale);
    sdDisplay.VerticalCursors[1] :=
      Round(MainFrm.Playback.Stop / sdDisplay.TScale);
    sdDisplay.VerticalCursors[2] :=
      Round(MainFrm.Playback.StartPS / sdDisplay.TScale);
    sdDisplay.VerticalCursors[3] :=
      Round(MainFrm.Playback.VOutStart / sdDisplay.TScale);
    sdDisplay.VerticalCursors[4] :=
      Round(MainFrm.Playback.VOutStop / sdDisplay.TScale);

    // Restore output channel values
    edVOutStart.Value := MainFrm.Playback.VOutStart;
    edVOutStop.Value := MainFrm.Playback.VOutStop;
    edVOut0.Value := MainFrm.Playback.VOutVoltage[0];
    edVOut1.Value := MainFrm.Playback.VOutVoltage[1];
    edVOut2.Value := MainFrm.Playback.VOutVoltage[2];

    // Enable controls
    EnableControls();

    // Enable/disable voltage output channels
    if rbVOut0.Checked then rbVOut0Click(Nil)
    else if rbVOut1.Checked then rbVOut1Click(Nil)
    else if rbVOut2.Checked then rbVOut2Click(Nil);

  end;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.bCancelClick(Sender: TObject);
begin

  // Close window without saving changes
  Close;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.bOKClick(Sender: TObject);
var
  i, j : Integer;
begin

  // Save form state
  MainFrm.Playback.SelectedChannel := cbChannel.ItemIndex;
  MainFrm.Playback.Start := edStart.Value;
  MainFrm.Playback.Stop := edStop.Value;
  MainFrm.Playback.StartPS := edStartPS.Value;
  MainFrm.Playback.DisplayInterval := edDisplay.Value;

  // Save output channel
  if rbVOut0.Checked then MainFrm.Playback.VOutChannel := 0;
  if rbVOut1.Checked then MainFrm.Playback.VOutChannel := 1;
  if rbVOut2.Checked then MainFrm.Playback.VOutChannel := 2;

  // Save output voltage settings
  MainFrm.Playback.VOutStart := edVOutStart.Value;
  MainFrm.Playback.VOutStop := edVOutStop.Value;
  MainFrm.Playback.VOutVoltage[0] := edVOut0.Value;
  MainFrm.Playback.VOutVoltage[1] := edVOut1.Value;
  MainFrm.Playback.VOutVoltage[2] := edVOut2.Value;

  // Set protocol in PlaybackStimulator
  PlaybackStimulator.SetProtocol(
    m_Data,
    Round(MainFrm.Playback.Start / MainFrm.ADCScanInterval),
    Round((MainFrm.Playback.Stop - MainFrm.Playback.Start) /
          MainFrm.ADCScanInterval),
    IDRFile.ADCChannel[cbChannel.ItemIndex].ADCScale,
    IDRFile.ADCChannel[cbChannel.ItemIndex].ADCUnits,
    Round(MainFrm.Playback.VOutStart / MainFrm.ADCScanInterval),
    Round((MainFrm.Playback.VOutStop - MainFrm.Playback.VOutStart) /
          MainFrm.ADCScanInterval),
    MainFrm.Playback.VOutVoltage[0],
    MainFrm.Playback.VOutVoltage[1],
    MainFrm.Playback.VOutVoltage[2]);

  // Close window
  Close;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.bOpenIDRClick(Sender: TObject);
var
  ch : Integer;
begin

  // Setup dialog
  OpenDialog.options := [ofPathMustExist];
  OpenDialog.DefaultExt := DataFileExtension;

  // Open last used data directory
  if DirectoryExists(MainFrm.DataDirectory) and
     (MainFrm.DataDirectory <> '') then
  begin
    OpenDialog.InitialDir := MainFrm.DataDirectory;
  end
  else
    OpenDialog.InitialDir := MainFrm.DefaultDataDirectory;

  // Setup dialog
  OpenDialog.Filter := format(' %s Files (*.%s)|*.%s',
    [DataFileExtension, DataFileExtension, DataFileExtension]);
  OpenDialog.Title := 'Open Data File for Playback';

  // Show dialog
  if OpenDialog.execute then
  begin

    // Check if file is already open in a different part of WinFluor
    if (AnsiCompareStr(OpenDialog.FileName, MainFrm.IDRFile.FileName) = 0) then
    begin

      // Show error message
      MessageDlg('IDR file is already open. Please close it and try again.',
                 mtError, [mbOk], 0);

      // Disable controls
      DisableControls();                 

    end
    else
    begin

      // Store filename
      MainFrm.Playback.FileName := OpenDialog.FileName;

      // Close file if already open
      if IDRFile.Open then IDRFile.CloseFile;

      // Open IDR file
      IDRFile.OpenFile(OpenDialog.FileName);

      // Fill in the channel selector
      cbChannel.Clear;
      for ch := 0 to IDRFile.ADCNumChannels - 1 do
      begin
        cbChannel.Items.Add(IDRFile.ADCChannel[ch].ADCName);
      end;
      cbChannel.ItemIndex := 0;

      // Change channel callback
      cbChannelChange(cbChannel);

      // Set initial cursor locations if not already
      if (edStart.Value = edStop.Value) then
      begin
        sdDisplay.VerticalCursors[1] := Round(0.50 / sdDisplay.TScale);
        sdDisplay.VerticalCursors[0] := Round(0.00 / sdDisplay.TScale);
        sdDisplay.VerticalCursors[2] := Round(0.25 / sdDisplay.TScale);
        sdDisplay.VerticalCursors[3] := Round(0.10 / sdDisplay.TScale);
        sdDisplay.VerticalCursors[4] := Round(0.20 / sdDisplay.TScale);
      end;

      // Enable controls
      EnableControls();

      // Enable/disable voltage output channels
      if rbVOut0.Checked then rbVOut0Click(Nil)
      else if rbVOut1.Checked then rbVOut1Click(Nil)
      else if rbVOut2.Checked then rbVOut2Click(Nil);

    end;

  end;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.btnDisplayHalfClick(Sender: TObject);
begin

  edDisplay.Value := 0.5 * edDisplay.Value;

  UpdateDisplay();

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.btnDisplayDoubleClick(Sender: TObject);
begin

  edDisplay.Value := 2.0 * edDisplay.Value;

  UpdateDisplay();

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.cbChannelChange(Sender: TObject);
var
  ch : Integer;                     // Selected channel
  i : Integer;                      // Loop index
  sample: Array[0..7] of SmallInt;  // Array of one sample
begin

  // Get channel selection
  ch := cbChannel.ItemIndex;

  // Load sample data for channel 0
  for i := 0 to IDRFile.ADCNumScansInFile - 1 do
  begin
    IDRFile.LoadADC(i, 1, sample);
    m_Data[i] := Round(sample[ch]);
  end;

  // Set up channel in A/D signals display window
  sdDisplay.MaxADCValue := IDRFile.ADCMaxValue;
  sdDisplay.MinADCValue := -IDRFile.ADCMaxValue - 1;
  sdDisplay.ChanOffsets[0] := 0;
  sdDisplay.ChanUnits[0] := IDRFile.ADCChannel[ch].ADCUnits;
  sdDisplay.ChanName[0] := IDRFile.ADCChannel[ch].ADCName;
  sdDisplay.ChanScale[0] := IDRFile.ADCChannel[ch].ADCScale;
  sdDisplay.yMin[0] := IDRFile.ADCChannel[ch].yMin;
  sdDisplay.yMax[0] := IDRFile.ADCChannel[ch].yMax;
  sdDisplay.ChanVisible[0] := True;

  // Update display
  UpdateDisplay();

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.edDisplayKeyPress(Sender: TObject;
  var Key: Char);
begin

  if Key = #13 then begin
    UpdateDisplay();
  end;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.rbDisplayUnitMinsClick(Sender: TObject);
begin
  SetDisplayUnits();
end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.rbDisplayUnitsSecsClick(Sender: TObject);
begin
  SetDisplayUnits();
end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.sbDisplayChange(Sender: TObject);
begin
  UpdateDisplay();
end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.sdDisplayCursorChange(Sender: TObject);
var
  PSStart: Single;
  Start : Single;
  Stop : Single;
  VStart : Single;
  VStop : Single;
  Temp : Single;
begin

  // Get vertical cursor times
  Start := sdDisplay.VerticalCursors[0] * sdDisplay.TScale;
  Stop := sdDisplay.VerticalCursors[1] * sdDisplay.TScale;
  PSStart := sdDisplay.VerticalCursors[2] * sdDisplay.TScale;

  // Set start and stop to min and max times
  Temp := Start;
  Start := Min(Start, Stop);
  Stop := Max(Temp, Stop);

  // Update edit boxes
  edStart.Value := Start;
  edStop.Value := Stop;

  // Place bound on uncaging start time
  if PSStart < Start then
  begin
    PSStart := Start;
    edStartPS.Value := PSStart;
    sdDisplay.VerticalCursors[2] :=
      Round(edStartPS.Value / sdDisplay.TScale);
  end
  else if PSStart > Stop then
  begin
    PSStart := Stop;
    edStartPS.Value := PSStart;
    sdDisplay.VerticalCursors[2] :=
      Round(edStartPS.Value / sdDisplay.TScale);
  end
  else edStartPS.Value := PSStart;


  // Get vertical cursor times for output voltage
  VStart := sdDisplay.VerticalCursors[3] * sdDisplay.TScale;
  VStop := sdDisplay.VerticalCursors[4] * sdDisplay.TScale;

  // Place bound on voltage output start time
  if VStart < Start then
  begin
    VStart := Start;
    edVOutStart.Value := VStart;
    sdDisplay.VerticalCursors[3] :=
      Round(edVOutStart.Value / sdDisplay.TScale);
  end
  else if VStart > Stop then
  begin
    VStart := Stop;
    edVOutStart.Value := VStart;
    sdDisplay.VerticalCursors[3] :=
      Round(edVOutStart.Value / sdDisplay.TScale);
  end
  else edVOutStart.Value := VStart;

  // Place bound on voltage output start time
  if VStop < Start then
  begin
    VStop := Start;
    edVOutStop.Value := VStop;
    sdDisplay.VerticalCursors[4] :=
      Round(edVOutStop.Value / sdDisplay.TScale);
  end
  else if VStop > Stop then
  begin
    VStop := Stop;
    edVOutStop.Value := VStop;
    sdDisplay.VerticalCursors[4] :=
      Round(edVOutStop.Value / sdDisplay.TScale);
  end
  else edVOutStop.Value := VStop;

  // Set start and stop to min and max times
  Temp := VStart;
  VStart := Min(VStart, VStop);
  VStop := Max(Temp, VStop);

  // Update edit boxes
  edVOutStart.Value := VStart;
  edVOutStop.Value := VStop;

end;

// -----------------------------------------------------------------------------

function TPlaybackSetupFrm.BoundDisplayTime(v: Single): Single;
begin

  if (v < m_StartTime) then Result := m_StartTime
  else if (v > m_StopTime) then Result := m_StopTime
  else Result := v;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.DisableControls();
begin

  // Disable controls
  cbChannel.Enabled := False;
  edStart.Enabled := False;
  edStop.Enabled := False;
  edStartPS.Enabled := False;
  pnlDisplay.Enabled := False;
  sdDisplay.Enabled := False;
  sbDisplay.Enabled := False;
  rbVOut0.Enabled := False;
  rbVOut1.Enabled := False;
  rbVOut2.Enabled := False;
  edVOutStart.Enabled := False;
  edVOutStop.Enabled := False;
  edVOut0.Enabled := False;
  edVOut1.Enabled := False;
  edVOut2.Enabled := False;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.EnableControls();
begin

  // Enable controls
  cbChannel.Enabled := True;
  edStart.Enabled := True;
  edStop.Enabled := True;
  edStartPS.Enabled := True;
  pnlDisplay.Enabled := True;
  sdDisplay.Enabled := True;
  sbDisplay.Enabled := True;
  rbVOut0.Enabled := True;
  rbVOut1.Enabled := True;
  rbVOut2.Enabled := True;
  edVOutStart.Enabled := True;
  edVOutStop.Enabled := True;
  edVOut0.Enabled := True;
  edVOut1.Enabled := True;
  edVOut2.Enabled := True;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.Resize();
begin

  // Position scope display
  sdDisplay.Width := ClientWidth - sdDisplay.Left - 8;
  sdDisplay.Height :=
    ClientHeight - sdDisplay.Top - bOK.Height - 23 - 17 - (8 * 3);

  // Position scroll bar
  sbDisplay.Width := ClientWidth - sbDisplay.Left - 8;
  sbDisplay.Height := 17;
  sbDisplay.Top := sdDisplay.Top + sdDisplay.Height + 8;

  // Position time panel
  pnlDisplay.Width := 209;
  pnlDisplay.Height := 23;
  pnlDisplay.Left := sbDisplay.Left + 3 + (sbDisplay.Width - pnlDisplay.Width);
  pnlDisplay.Top := sbDisplay.Top + sbDisplay.Height + 8;

  // Position OK/Cancel buttons
  bOK.Top := ClientHeight - bOK.Height - 8;
  bCancel.Top := bOK.Top;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.SetDisplayUnits ;
begin

  if rbDisplayUnitsSecs.Checked then begin
    edDisplay.Units := 's';
    edDisplay.Scale := 1.0;
  end
  else begin
    edDisplay.Units := 'm';
    edDisplay.Scale := 1.0 / 60.0;
  end;

  if (MainFrm.IDRFile.ADCNumChannels > 0) then begin
    sdDisplay.TScale :=
      IDRFile.ADCScanInterval * m_SampleStep * edDisplay.Scale;
    sdDisplay.TUnits := edDisplay.Units ;
    sdDisplay.Invalidate;
  end;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.UpdateDisplay();
var
  BlockCount: Integer;
  ch: Integer;
  i: Integer;
  j: Integer;
  k: Integer;
  y: Integer;
  DisplayDuration: Single;
  TotalDuration: Single;
  DisplayScans: Integer;
  SampleStep: Integer;
  StartTime: Single;
  StartSample: Integer;
  Done: Boolean;
  yMin: Integer;
  yMax: Integer;
  yMinAt: Integer;
  yMaxAt: Integer;
begin

  TotalDuration := IDRFile.ADCScanInterval *
                   IDRFile.ADCNumScansInFile;
  DisplayDuration := edDisplay.Value;
  StartTime := (sbDisplay.Position / 100) * TotalDuration;
  
  m_StartTime := StartTime;
  m_StopTime := StartTime + DisplayDuration;

  DisplayScans := Trunc(DisplayDuration / IDRFile.ADCScanInterval);
  SampleStep := DisplayScans div 1024;
  m_SampleStep := SampleStep;

  StartSample := Trunc(StartTime / IDRFile.ADCScanInterval);

  i := StartSample;
  j := 0;
  while (i < (StartSample + DisplayScans - 1)) and
        (i < IDRFile.ADCNumScansInFile) and
        (j < 1024) do
  begin

    k := i;
    BlockCount := 0;

    yMin := IDRFile.ADCMaxValue ;
    yMax := -yMin - 1 ;

    while (k < (i + SampleStep - 1)) and
          (k < IDRFile.ADCNumScansInFile) do
    begin

      y := Trunc(m_Data[k]);
      if y < yMin then begin
        yMin := y;
        yMinAt := BlockCount;
      end;
      if y > yMax then begin
        yMax := y;
        yMaxAt := BlockCount;
      end;

      k := k + 1;
      BlockCount := BlockCount + 1;

    end;

    // First point
    if yMaxAt <= yMinAt then ADCBuf[j] := yMax
                        else ADCBuf[j] := yMin;

    // Second point
    if BlockCount > 1 then
    begin
      if yMaxAt >= yMinAt then ADCBuf[j] := yMax
                          else ADCBuf[j] := yMin;
    end ;

    i := i + SampleStep;
    j := j + 1;

  end;

  sdDisplay.XOffset := Round(StartSample / SampleStep);

  sdDisplay.MaxPoints := 1024;
  sdDisplay.NumPoints := j;
  sdDisplay.SetDataBuf(@ADCBuf);

  sdDisplay.xMin := 0;
  sdDisplay.xMax := sdDisplay.MaxPoints - 1;

  SetDisplayUnits();

  sdDisplay.Invalidate();

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.edStartKeyPress(Sender: TObject; var Key: Char);
begin

  // Enter/Return pressed
  if Key = #13 then
  begin

    // Set start and stop position
    sdDisplay.VerticalCursors[0] :=
      Round(BoundDisplayTime(edStart.Value) / sdDisplay.TScale);

    // Update scope display
    UpdateDisplay();

  end;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.edStopKeyPress(Sender: TObject; var Key: Char);
begin

  // Enter/Return pressed
  if Key = #13 then
  begin

    // Set start and stop position
    sdDisplay.VerticalCursors[1] :=
      Round(BoundDisplayTime(edStop.Value) / sdDisplay.TScale);

    // Update scope display
    UpdateDisplay();

  end;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.edStartPSKeyPress(Sender: TObject; var Key: Char);
begin

  // Enter/Return pressed
  if Key = #13 then
  begin

    // Set start and stop position
    sdDisplay.VerticalCursors[2] :=
      Round(BoundDisplayTime(edStartPS.Value) / sdDisplay.TScale);

    // Update scope display
    UpdateDisplay();

  end;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.edVOutStartKeyPress(Sender: TObject; var Key: Char);
begin

  // Enter/Return pressed
  if Key = #13 then
  begin

    // Set start and stop position
    sdDisplay.VerticalCursors[3] :=
      Round(BoundDisplayTime(edVOutStart.Value) / sdDisplay.TScale);

    // Update scope display
    UpdateDisplay();

  end;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.edVOutStopKeyPress(Sender: TObject; var Key: Char);
begin

  // Enter/Return pressed
  if Key = #13 then
  begin

    // Set start and stop position
    sdDisplay.VerticalCursors[4] :=
      Round(BoundDisplayTime(edVOutStop.Value) / sdDisplay.TScale);

    // Update scope display
    UpdateDisplay();

  end;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.rbVOut0Click(Sender: TObject);
begin

  // Enable appropriate panels
  edVOut0.Enabled := False;
  edVOut1.Enabled := True;
  edVOut2.Enabled := True;
  
end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.rbVOut1Click(Sender: TObject);
begin

  // Enable appropriate panels
  edVOut0.Enabled := True;
  edVOut1.Enabled := False;
  edVOut2.Enabled := True;

end;

// -----------------------------------------------------------------------------

procedure TPlaybackSetupFrm.rbVOut2Click(Sender: TObject);
begin

  // Enable appropriate panels
  edVOut0.Enabled := True;
  edVOut1.Enabled := True;
  edVOut2.Enabled := False;

end;

// -----------------------------------------------------------------------------

end.

// -----------------------------------------------------------------------------
