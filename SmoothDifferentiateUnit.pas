unit SmoothDifferentiateUnit;

//----------------------------------------------------------------------------//
// SmoothDifferentiateUnit
// by Nicholas Schwarz
//----------------------------------------------------------------------------//

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ScopeDisplay, StdCtrls, ValidatedEdit, Math, ExtCtrls;

type

  TSmallIntArray = Array[0..$FFFFFF] of SmallInt;
  PSmallIntArray = ^TSmallIntArray;

  TDoubleArray = Array[0..$FFFFFF] of Double;
  PDoubleArray = ^TDoubleArray;

  TSmoothDifferentiateFrm = class(TForm)
    sdDisplay: TScopeDisplay;
    LabelChannel: TLabel;
    cbChannel: TComboBox;
    LabelDataMAWindow: TLabel;
    LabelDXDTMAWindow: TLabel;
    btnUpdate: TButton;
    edDataMAWindow: TValidatedEdit;
    edDXDTMAWindow: TValidatedEdit;
    sbDisplay: TScrollBar;
    pnlDisplay: TPanel;
    edDisplay: TValidatedEdit;
    rbDisplayUnitMins: TRadioButton;
    rbDisplayUnitsSecs: TRadioButton;
    btnDisplayDouble: TButton;
    btnDisplayHalf: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure btnDisplayDoubleClick(Sender: TObject);
    procedure btnDisplayHalfClick(Sender: TObject);
    procedure rbDisplayUnitsSecsClick(Sender: TObject);
    procedure rbDisplayUnitMinsClick(Sender: TObject);
    procedure sbDisplayChange(Sender: TObject);
    procedure edDisplayKeyPress(Sender: TObject; var Key: Char);

  private

    ADCBuf: Array[0..4096] of SmallInt;
    ChnVisible: Array[0..3] of Boolean;
    m_SampleStep: Integer;
    m_Max: Double;
    m_Min: Double;

    procedure CalculateMovingAverage(InBuf: PDoubleArray;
                                     OutBuf: PDoubleArray;
                                     BufSize: Integer;
                                     WindowSize: Integer);
    procedure CalculateGradient(InBuf: PDoubleArray;
                                OutBuf: PDoubleArray;
                                BufSize: Integer);
    function FindMax(Buf: PDoubleArray; BufSize: Integer): Double;
    function FindMin(Buf: PDoubleArray; BufSize: Integer): Double;
    function MakeOdd(value: Integer): Integer;
    function Resample(Buf: PDoubleArray; BufSize: Integer): Double;
    procedure Resize();
    procedure SetDisplayUnits();
    procedure UpdateDisplay();

  public
    procedure CopyPlotImageToClipboard();
    procedure CopyDataToClipboard();
    function PlotAvailable(): Boolean;

  end;



var
  SmoothDifferentiateFrm: TSmoothDifferentiateFrm;

  // Data as a double
  m_Data: PDoubleArray;

  // Moving average of data
  m_MovingAverage: PDoubleArray;

  // Gradient (derivative) of moving average of data
  m_Gradient: PDoubleArray;

  // Moving average of gradient
  m_Result: PDoubleArray;

implementation

uses Main, PrintRec, Printgra;

{$R *.dfm}

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.btnUpdateClick(Sender: TObject);
var
  ch: Integer;                      // Channel index
  i: Integer;                       // Loop index
  sample: Array[0..7] of SmallInt;  // Array of one sample
  scaleFactorG: Double;
  scaleFactorR: Double;
  WindowData: Integer;              // Data window
  WindowGradient: Integer;          // Gradient window
  smax: Double;
  smin: Double;
begin

  // Get channel index
  ch := cbChannel.ItemIndex;

  // Check that channel index is valid
  if (ch < 0) or (ch > MainFrm.IDRFile.ADCNumChannels) then
  begin
    MessageDlg('Invalid channel selection.', mtInformation, [mbOk], 0);
    Exit;
  end;


  // Change cursor
  Screen.Cursor := crHourGlass;


  // Disable controls while computing
  btnUpdate.Enabled := False;
  cbChannel.Enabled := False;
  edDataMAWindow.Enabled := False;
  edDXDTMAWindow.Enabled := False;

  // Save channel visible state
  for i := 0 to 4 - 1 do
  begin
    ChnVisible[i] := sdDisplay.ChanVisible[i];
  end;


  // Make sure window sizes are odd
  edDataMAWindow.Value := MakeOdd(Floor(edDataMAWindow.Value));
  edDXDTMAWindow.Value := MakeOdd(Floor(edDXDTMAWindow.Value));

  // Get window sizes
  WindowData := Floor(edDataMAWindow.Value);
  WindowGradient := Floor(edDXDTMAWindow.Value);


  // Load sample data for selected channel
  for i := 0 to MainFrm.IDRFile.ADCNumScansInFile - 1 do
  begin
    MainFrm.IDRFile.LoadADC(i, 1, sample);
    m_Data[i] := Round(sample[ch]);
  end;


  // Calculate moving average of original data
  CalculateMovingAverage(m_Data,
                         m_MovingAverage,
                         MainFrm.IDRFile.ADCNumScansInFile,
                         WindowData);

  // Calculate gradient
  CalculateGradient(m_MovingAverage,
                    m_Gradient,
                    MainFrm.IDRFile.ADCNumScansInFile);

  // Calculate moving average of gradient
  CalculateMovingAverage(m_Gradient,
                         m_Result,
                         MainFrm.IDRFile.ADCNumScansInFile,
                         WindowGradient);


  // Resample gradient to fill chart range
  scaleFactorG := Resample(m_Gradient, MainFrm.IDRFile.ADCNumScansInFile);

  // Resample moving average of gradient to fill chart range
  scaleFactorR := Resample(m_Result, MainFrm.IDRFile.ADCNumScansInFile);

  // Add vertical cursor
  sdDisplay.ClearVerticalCursors;
  sdDisplay.AddVerticalCursor(-1, clGreen, '?r');

  // Set up first channel in A/D signals display window
  sdDisplay.ChanOffsets[0] := 0;
  sdDisplay.ChanUnits[0] := MainFrm.IDRFile.ADCChannel[ch].ADCUnits;
  sdDisplay.ChanName[0] := MainFrm.IDRFile.ADCChannel[ch].ADCName;
  sdDisplay.ChanScale[0] := MainFrm.IDRFile.ADCChannel[ch].ADCScale;
  sdDisplay.yMin[0] := MainFrm.IDRFile.ADCChannel[ch].yMin;
  sdDisplay.yMax[0] := MainFrm.IDRFile.ADCChannel[ch].yMax;
  sdDisplay.ChanVisible[0] := ChnVisible[0];

  // Set up second channel in A/D signals display window
  sdDisplay.ChanOffsets[1] := 1;
  sdDisplay.ChanUnits[1] := MainFrm.IDRFile.ADCChannel[ch].ADCUnits;
  sdDisplay.ChanName[1] := 'MA';
  sdDisplay.ChanScale[1] := MainFrm.IDRFile.ADCChannel[ch].ADCScale;
  sdDisplay.yMin[1] := MainFrm.IDRFile.ADCChannel[ch].yMin;
  sdDisplay.yMax[1] := MainFrm.IDRFile.ADCChannel[ch].yMax;
  sdDisplay.ChanVisible[1] := ChnVisible[1];

  // Set up third channel in A/D signals display window
  sdDisplay.ChanOffsets[2] := 2;
  sdDisplay.ChanUnits[2] := MainFrm.IDRFile.ADCChannel[ch].ADCUnits;
  sdDisplay.ChanName[2] := 'dx';
  sdDisplay.ChanScale[2] := MainFrm.IDRFile.ADCChannel[ch].ADCScale / scaleFactorG;
  sdDisplay.yMin[2] := MainFrm.IDRFile.ADCChannel[ch].yMin;
  sdDisplay.yMax[2] := MainFrm.IDRFile.ADCChannel[ch].yMax;
  sdDisplay.ChanVisible[2] := ChnVisible[2];

  // Set up fourth channel in A/D signals display window
  sdDisplay.ChanOffsets[3] := 3;
  sdDisplay.ChanUnits[3] := MainFrm.IDRFile.ADCChannel[ch].ADCUnits;
  sdDisplay.ChanName[3] := 'MA(dx)';
  sdDisplay.ChanScale[3] := MainFrm.IDRFile.ADCChannel[ch].ADCScale / scaleFactorR;
  sdDisplay.yMin[3] := MainFrm.IDRFile.ADCChannel[ch].yMin;
  sdDisplay.yMax[3] := MainFrm.IDRFile.ADCChannel[ch].yMax;
  sdDisplay.ChanVisible[3] := ChnVisible[3];


  // Enable controls
  btnUpdate.Enabled := True;
  cbChannel.Enabled := True;
  edDataMAWindow.Enabled := True;
  edDXDTMAWindow.Enabled := True;


  // Change cursor
  Screen.Cursor := crDefault;


  // Update display
  UpdateDisplay();

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.CopyDataToClipboard();
begin

  sdDisplay.CopyDataToClipboard();

end ;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.CopyPlotImageToClipboard();
begin

  // Copy record on display
  PrintRecFrm.Destination := deClipboard;
  PrintRecFrm.DisplayObj :=  sdDisplay;
  PrintRecFrm.ShowModal;
  if PrintRecFrm.ModalResult = mrOK then begin
    sdDisplay.ClearPrinterTitle;
    sdDisplay.AddPrinterTitleLine('File : ' + MainFrm.IDRFile.FileName);
    sdDisplay.AddPrinterTitleLine(MainFrm.IDRFile.Ident);
    sdDisplay.CopyImageToClipboard;
  end;

end ;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.FormShow(Sender: TObject);
var
  ch: Integer;  // Channel index
begin

  // Allocate memory for data as double
  GetMem(m_Data, SizeOf(TDoubleArray));

  // Allocate memory for moving averagae of data
  GetMem(m_MovingAverage, SizeOf(TDoubleArray));

  // Allocate memory for gradient of moving average of data
  GetMem(m_Gradient, SizeOf(TDoubleArray));

  // Allocate memory for moving average of gradient
  GetMem(m_Result, SizeOf(TDoubleArray));


  // Fill in the channel selector
  for ch := 0 to MainFrm.IDRFile.ADCNumChannels - 1 do
  begin
    cbChannel.Items.Add(MainFrm.IDRFile.ADCChannel[ch].ADCName);
  end ;


  // Set up channel visible state
  for ch := 0 to 4 - 1 do
  begin
    sdDisplay.ChanVisible[ch] := MainFrm.SmoothDifferentiateUnitVisible[ch];
  end;
  edDataMAWindow.Value := MainFrm.SmoothDifferentiateUnitMADataWindow;
  edDXDTMAWindow.Value := MainFrm.SmoothDifferentiateUnitMADXWindow;


  // Set up A/D signals display window
  sdDisplay.NumChannels := 4;
  sdDisplay.MaxADCValue := MainFrm.IDRFile.ADCMaxValue;
  sdDisplay.MinADCValue := -MainFrm.IDRFile.ADCMaxValue - 1;
  sdDisplay.ClearHorizontalCursors;
  sdDisplay.ClearVerticalCursors;
  sdDisplay.ChanName[0] := 'Data';
  sdDisplay.ChanName[1] := 'MA';
  sdDisplay.ChanName[2] := 'dx';
  sdDisplay.ChanName[3] := 'MA(dx)';
  sdDisplay.Visible := True;


  // Get window to size correctly
  Resize();

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  ch: Integer;
begin

  // Deallocate memory
  if m_Data <> Nil then begin
    FreeMem(m_Data);
    m_Data := Nil;
  end;
  if m_MovingAverage <> Nil then begin
    FreeMem(m_MovingAverage);
    m_MovingAverage := Nil;
  end;
  if m_Gradient <> Nil then begin
    FreeMem(m_Gradient);
    m_Gradient := Nil;
  end;
  if m_Result <> Nil then begin
    FreeMem(m_Result);
    m_Result := Nil;
  end;

  // Save window state
  for ch := 0 to (4 - 1) do
  begin
    MainFrm.SmoothDifferentiateUnitVisible[ch] := sdDisplay.ChanVisible[ch];
  end;
  MainFrm.SmoothDifferentiateUnitMADataWindow :=
    MakeOdd(Floor(edDataMAWindow.Value));
  MainFrm.SmoothDifferentiateUnitMADXWindow :=
    MakeOdd(Floor(edDXDTMAWindow.Value));

  // Signal window to close completely
  Action := caFree;

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.FormResize(Sender: TObject);
begin

  // Resize window
  Resize();

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.CalculateGradient(InBuf: PDoubleArray;
                                                    OutBuf: PDoubleArray;
                                                    BufSize: Integer);
var
  i: Integer;
begin

  OutBuf[0] := (InBuf[1] - InBuf[0]) / 1.0;
  OutBuf[BufSize - 1] := (InBuf[BufSize - 1] - InBuf[BufSize - 2]) / 1.0;

  for i := 1 to BufSize - 2 do
  begin
    OutBuf[i] := (InBuf[i + 1] - InBuf[i - 1]) / 2.0;
  end;

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.CalculateMovingAverage(InBuf: PDoubleArray;
                                                         OutBuf: PDoubleArray;
                                                         BufSize: Integer;
                                                         WindowSize: Integer);
var
  i: Integer;
  j: Integer;
  HalfWindow: Integer;
  MovingAverage: Single;
  Step: Integer;
begin

  WindowSize := Min(BufSize, WindowSize);
  WindowSize := MakeOdd(WindowSize);

  HalfWindow := Floor(WindowSize / 2);

  for i := 0 to BufSize - 1 do
  begin

    MovingAverage := 0.0;

    if (i < HalfWindow) then begin
      Step := HalfWindow - (HalfWindow - i);
    end
    else if ((i + HalfWindow) > (BufSize - 1)) then begin
      Step := HalfWindow - ((i + HalfWindow) - (BufSize - 1));
    end
    else
      Step := HalfWindow;

    for j := i - Step to i + Step do
    begin
      MovingAverage := MovingAverage + InBuf[j];
    end;

    OutBuf[i] := MovingAverage / (Step * 2 + 1);

  end;

end;

//----------------------------------------------------------------------------//

function TSmoothDifferentiateFrm.FindMax(Buf: PDoubleArray;
                                         BufSize: Integer): Double;
var
  i: Integer;
  value: Double;
begin

  value := Buf[0];

  for i := 0 to BufSize - 1 do
  begin
    if Buf[i] > value then value := Buf[i];
  end;

  Result := value;

end;

//----------------------------------------------------------------------------//

function TSmoothDifferentiateFrm.FindMin(Buf: PDoubleArray;
                                         BufSize: Integer): Double;
var
  i: Integer;
  value: Double;
begin

  value := Buf[0];

  for i := 0 to BufSize - 1 do
  begin
    if Buf[i] < value then value := Buf[i];
  end;

  Result := value;

end;

//----------------------------------------------------------------------------//

function TSmoothDifferentiateFrm.MakeOdd(value: Integer): Integer;
begin

  Result := value - 1 + (value mod 2);

end;

//----------------------------------------------------------------------------//

function TSmoothDifferentiateFrm.PlotAvailable(): Boolean ;
begin

    Result := False ;
    
    if (sdDisplay.NumPoints > 0) then Result := True;

end;

//----------------------------------------------------------------------------//

function TSmoothDifferentiateFrm.Resample(Buf: PDoubleArray;
                                          BufSize: Integer): Double;
var
  i: Integer;
  scaleFactor: Double;
  vMin: Double;
  vMax: Double;
begin

  vMin := FindMin(Buf, BufSize);
  vMax := FindMax(Buf, BufSize);

  if abs(vMax) > abs(vMin) then
    scaleFactor := abs(vMax)
  else
    scaleFactor := abs(vMin);

  scaleFactor := 32767 / scaleFactor;

  for i := 0 to BufSize - 1 do
  begin
    Buf[i] := Buf[i] * scaleFactor;
  end;

  Result := ScaleFactor;

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.Resize();
begin

  sdDisplay.Width := ClientWidth - sdDisplay.Left - 16;
  sdDisplay.Height := ClientHeight - (sdDisplay.Top * 5);

  sbDisplay.Width := ClientWidth - sbDisplay.Left - 16;
  sbDisplay.Height := 17;
  sbDisplay.Top := sdDisplay.Top + sdDisplay.Height + 8;

  pnlDisplay.Width := 209;
  pnlDisplay.Height := 23;
  pnlDisplay.Left := sbDisplay.Left + 3 + (sbDisplay.Width - pnlDisplay.Width);
  pnlDisplay.Top := sbDisplay.Top + sbDisplay.Height + 8;

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.UpdateDisplay();
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
  yMin: Array[0..3] of Integer;
  yMax: Array[0..3] of Integer;
  yMinAt: Array[0..3] of Integer;
  yMaxAt: Array[0..3] of Integer;
begin

  TotalDuration := MainFrm.IDRFile.ADCScanInterval *
                   MainFrm.IDRFile.ADCNumScansInFile;
  DisplayDuration := edDisplay.Value;
  StartTime := (sbDisplay.Position / 100) * TotalDuration;

  DisplayScans := Trunc(DisplayDuration / MainFrm.IDRFile.ADCScanInterval);
  SampleStep := DisplayScans div 1024;
  m_SampleStep := SampleStep;

  StartSample := Trunc(StartTime / MainFrm.IDRFile.ADCScanInterval);

  i := StartSample;
  j := 0;
  while (i < (StartSample + DisplayScans - 1)) and
        (i < MainFrm.IDRFile.ADCNumScansInFile) and
        (j < 1024) do
  begin
    //Label1.Caption := 'j: ' + IntToStr(j);
    //Label2.Caption := 'DisplayScans: ' + FloatToStr(DisplayScans);
    //Label3.Caption := 'SampleStep: ' + IntToStr(SampleStep);
    //Label4.Caption := 'NumInFile: ' + IntToStr(MainFrm.IDRFile.ADCNumScansInFile);
    //Label5.Caption := 'Gra: ' + IntToStr(Round(m_Result[i])) + ' ' + FloatToStr(m_Result[i]);
    //Label6.Caption := 'StartSample: ' + IntToStr(StartSample);
    //Label7.Caption := 'DisplayScans: ' + IntToStr(DisplayScans);
    //ADCBuf[(j*4)] := Round(m_Data[i]);
    //ADCBuf[(j*4)+1] := Round(m_MovingAverage[i]);
    //ADCBuf[(j*4)+2] := Round(m_Gradient[i]);
    //ADCBuf[(j*4)+3] := Round(m_Result[i]);

    k := i;
    BlockCount := 0;

    for ch := 0 to 4 - 1 do
    begin
      yMin[ch] := MainFrm.IDRFile.ADCMaxValue ;
      yMax[ch] := -yMin[ch] - 1 ;
    end;

    while (k < (i + SampleStep - 1)) and
          (k < MainFrm.IDRFile.ADCNumScansInFile) do
    begin

      y := Trunc(m_Data[k]);
      if y < yMin[0] then begin
        yMin[0] := y;
        yMinAt[0] := BlockCount;
      end;
      if y > yMax[0] then begin
        yMax[0] := y;
        yMaxAt[0] := BlockCount;
      end;

      y := Trunc(m_MovingAverage[k]);
      if y < yMin[1] then begin
        yMin[1] := y;
        yMinAt[1] := BlockCount;
      end;
      if y > yMax[1] then begin
        yMax[1] := y;
        yMaxAt[1] := BlockCount;
      end;

      y := Trunc(m_Gradient[k]);
      if y < yMin[2] then begin
        yMin[2] := y;
        yMinAt[2] := BlockCount;
      end;
      if y > yMax[2] then begin
        yMax[2] := y;
        yMaxAt[2] := BlockCount;
      end;

      y := Trunc(m_Result[k]);
      if y < yMin[3] then begin
        yMin[3] := y;
        yMinAt[3] := BlockCount;
      end;
      if y > yMax[3] then begin
        yMax[3] := y;
        yMaxAt[3] := BlockCount;
      end;

      k := k + 1;
      BlockCount := BlockCount + 1;

    end;

    // First point
    for ch := 0 to 4 - 1 do
    begin
      if yMaxAt[ch] <= yMinAt[ch] then ADCBuf[(j*4)+ch] := yMax[ch]
                                  else ADCBuf[(j*4)+ch] := yMin[ch];
    end;

    // Second point
    if BlockCount > 1 then
    begin
      for ch := 0 to 4 - 1 do
      begin
        if yMaxAt[ch] >= yMinAt[ch] then ADCBuf[(j*4)+ch] := yMax[ch]
                                    else ADCBuf[(j*4)+ch] := yMin[ch] ;
      end;
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

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.SetDisplayUnits ;
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
      sdDisplay.TScale := (MainFrm.IDRFile.ADCScanInterval*m_SampleStep*edDisplay.Scale);
      sdDisplay.TUnits := edDisplay.Units ;
      sdDisplay.Invalidate;
    end;

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.btnDisplayDoubleClick(Sender: TObject);
begin

  edDisplay.Value := 2.0 * edDisplay.Value;

  UpdateDisplay();

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.btnDisplayHalfClick(Sender: TObject);
begin

  edDisplay.Value := 0.5 * edDisplay.Value;

  UpdateDisplay();

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.rbDisplayUnitsSecsClick(
  Sender: TObject);
begin

  SetDisplayUnits();

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.rbDisplayUnitMinsClick(Sender: TObject);
begin

  SetDisplayUnits();

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.sbDisplayChange(Sender: TObject);
begin

  UpdateDisplay();

end;

//----------------------------------------------------------------------------//

procedure TSmoothDifferentiateFrm.edDisplayKeyPress(Sender: TObject;
  var Key: Char);
begin

  if Key = #13 then begin
    UpdateDisplay();
  end;

end;

//----------------------------------------------------------------------------//

end.

//----------------------------------------------------------------------------//
