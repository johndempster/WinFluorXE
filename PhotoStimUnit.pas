unit PhotoStimUnit;
// ------------------------------
// Photo stimulus protocol editor
// ------------------------------

interface

uses
  SysUtils, Windows, WinTypes, WinProcs, Messages, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, RangeEdit, ComCtrls, IDRFile,
  math, ValidatedEdit, ImageFile, {GraphicEx,} Grids, strutils, mmsystem,
  FileCtrl, SHared, SESCam, Menus;

const
  MaxStimPoints = 100;

type
  TPhotoStimFrm = class(TForm)

    CalGrp: TGroupBox;
    ControlGrp: TGroupBox;
    DisplayGrp: TGroupBox;
    Image: TImage;
    ImageFile: TImageFile;
    ImageGrp: TGroupBox;
    LocationGrp: TGroupBox;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Timer: TTimer;    
    UltimaImageFile: TImageFile;
    bDeleteTargets: TButton;
    bDuplicate: TButton;    
    bGetImage: TButton;
    bOpenProtocol: TButton;
    bSaveImage: TButton;
    bSaveProtocol: TButton;
    bSaveProtocolAs: TButton;
    bPowerCalibrate: TButton;
    cbAttenuator: TComboBox;
    cbChannel: TComboBox;
    cbDisplayZoom: TComboBox;
    ckCalibrationBar: TCheckBox;
    ckMoveAll: TCheckBox;    
    ckPhotoStimRepeat: TCheckBox;
    ckReferenceLine: TCheckBox;
    edDisplayIntensityRange: TRangeEdit;
    edPhotoStimPeriod: TValidatedEdit;
    lblAttenuator: TLabel;
    lblChannel: TLabel;
    lblCursorPosition: TLabel;
    lblDuration: TLabel;
    lblTotalDuration: TLabel;
    lblRange: TLabel;
    lblRepeatPeriod: TLabel;
    lblZoom: TLabel;
    menuDeleteTarget: TMenuItem;
    menuSetAll: TMenuItem;
    menuTargets: TPopupMenu;
    pnlControls: TPanel;
    sgTargets: TStringGrid;
    Shape1: TShape;
    Shape2: TShape;

    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);    
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImageDblClick(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject;
                             Button: TMouseButton;
                             Shift: TShiftState;
                             X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject;
                             Shift: TShiftState;
                             X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject;
                           Button: TMouseButton;
                           Shift: TShiftState;
                           X, Y: Integer);
    procedure TimerTimer(Sender: TObject);
    procedure bDeleteTargetsClick(Sender: TObject);
    procedure bDuplicateClick(Sender: TObject);    
    procedure bGetImageClick(Sender: TObject);
    procedure bOpenProtocolClick(Sender: TObject);
    procedure bPowerCalibrateClick(Sender: TObject);
    procedure bSaveImageClick(Sender: TObject);
    procedure bSaveProtocolAsClick(Sender: TObject);
    procedure bSaveProtocolClick(Sender: TObject);
    procedure cbAttenuatorChange(Sender: TObject);
    procedure ckCalibrationBarClick(Sender: TObject);
    procedure cbChannelChange(Sender: TObject);
    procedure cbDisplayZoomChange(Sender: TObject);
    procedure ckPhotoStimRepeatClick(Sender: TObject);
    procedure ckReferenceLineClick(Sender: TObject);
    procedure edDisplayIntensityRangeKeyPress(Sender: TObject; var Key: Char);
    procedure edPhotoStimPeriodKeyPress(Sender: TObject; var Key: Char);
    procedure menuDeleteTargetClick(Sender: TObject);    
    procedure menuSetAllClick(Sender: TObject);
    procedure sgTargetsKeyPress(Sender: TObject; var Key: Char);
    procedure sgTargetsMouseDown(Sender: TObject; Button: TMouseButton;
                                 Shift: TShiftState; X, Y: Integer);
    procedure sgTargetsSelectCell(Sender: TObject; ACol, ARow: Integer;
                                  var CanSelect: Boolean);

  private

    // Local copy of protocol
    m_X : Array[0..MaxStimPoints-1] of Single ;     // X location in um
    m_Y : Array[0..MaxStimPoints-1] of Single ;     // Y location in um
    m_A : Array[0..MaxStimPoints-1] of Single ;     // Amplitude in mW
    m_D : Array[0..MaxStimPoints-1] of Single ;     // Duration in ms
    m_PRE : Array[0..MaxStimPoints-1] of Single ;   // Pre-delay in ms
    m_POST : Array[0..MaxStimPoints-1] of Single ;  // Post-delay in ms

    DisplayPic : TPicture;
    DisplayZoom : Single;      // Display zoom factor (0.5,1.0.2.0)
    Initialised : Boolean;     // TRUE = formshow initialisations done

    // Unit variables
    m_MouseClickPos : TPoint;              // Mouse click position
    m_MouseMoveRefLine : Boolean;          // Move ref line with mouse
    m_MouseMoveTarget : Boolean;           // Move target with mouse
    m_MouseRefLineSel : Boolean;           // Is ref line selected
    m_MouseTargetSel : Integer;            // Selected target index
    m_NumChannels : Integer;               // Number of channels
    m_OrigFrameHeight : Integer;           // Height in pixels
    m_OrigFrameWidth : Integer;            // Width in pixels
    m_OrigFrameBPP : Integer;              // Bytes per pixel
    p_OrigFrame : Array[0..4] of Pointer;  // Image frames from acquisition
    m_TargetsColRClk : Integer;            // Targets grid right click column
    m_TargetsRowRClk : Integer;            // Targets grid right click row    

    function CalculateDuration : Single;
    procedure CameraSnap();
    procedure ConvertToPixelCoord(
              DisplayRect : TRect;
              XMicron : Single;
              YMicron : Single;
              var XPixel : Integer;
              var YPixel : Integer);
    procedure ConvertToTargetCoord(
              DisplayRect : TRect;
              XPixel : Integer;
              YPixel : Integer;
              var XMicron : Single;
              var YMicron : Single);
    procedure DisplayCalibrationBar(Const Pic : TPicture);
    procedure DisplayRefLine(Const Pic : TPicture);
    procedure DisplayTargets(Const Pic : TPicture);
    procedure LoadWaveform(FileName : String);
    procedure NewWaveform();
    procedure SaveWaveform(FileName : String);
    procedure SetImagePanels();    
    procedure UltimaSnap();
    procedure UpdateDisplay();
    procedure UpdateDuration();
    procedure UpdateSettings();
    procedure UpdateTargetTable();
    function ValidateNumericEntry(input: String; var output: Single) : Boolean;
    procedure ValidateStimulusProtocol();
    procedure ValidateStimulusProtocolBounds();
    procedure ZoomDisplay();

end;

var
  PhotoStimFrm: TPhotoStimFrm;

implementation

uses Main, UltimaUnit, PhotoStimSetupUnit, LogUnit, PhotoStimModule,
     RecUnit, RecADCOnlyUnit;

const
    CrossHalfWidth = 10 ;

{$R *.dfm}


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
  i : Integer;
begin

  // Not yet initialized
  Initialised := False;


  // Initialize mouse flags
  m_MouseMoveRefLine := False;  
  m_MouseRefLineSel := False;
  m_MouseTargetSel := -1;

  // Initialize right click positions
  m_TargetsColRClk := -1;
  m_TargetsRowRClk := -1;


  // Set form at top left of MDI window
  Top := 20;
  Left := 20;


  // Check that attenuators are configured
  if (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI1)) and
     (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI2)) and
     (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI3)) then begin
    MessageDlg('No photo-stimulus attenuators configured.', mtError, [mbOK], 0);
    Close;
    Exit;
  end;

  // Load configured attenuator channels
  cbAttenuator.Clear ;
  if MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI1) then
  begin
    cbAttenuator.Items.AddObject( ' 1 ', Tobject(1));
  end;
  if MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI2) then
  begin
    cbAttenuator.Items.AddObject( ' 2 ', Tobject(2));
  end;
  if MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI3) then
  begin
    cbAttenuator.Items.AddObject( ' 3 ', Tobject(3));
  end;
  cbAttenuator.ItemIndex := Max(0,
    cbAttenuator.Items.IndexOfObject(TObject(MainFrm.PhotoStim.Attenuator)));


  // Load settings (must be done first)
  edPhotoStimPeriod.Value := MainFrm.PhotoStim.Period;
  ckPhotoStimRepeat.Checked := MainFrm.PhotoStim.RepeatedStim;


  // Display magnification factor
  cbDisplayZoom.Clear;
  cbDisplayZoom.Items.AddObject( '  25% ', Tobject(25));
  cbDisplayZoom.Items.AddObject( '  50% ', Tobject(50));
  cbDisplayZoom.Items.AddObject( ' 100% ', Tobject(100));
  cbDisplayZoom.Items.AddObject( ' 200% ', Tobject(200));
  cbDisplayZoom.Items.AddObject( ' 300% ', Tobject(300));
  cbDisplayZoom.Items.AddObject( ' 400% ', Tobject(400));
  cbDisplayZoom.Items.AddObject( ' 500% ', Tobject(500));
  cbDisplayZoom.Items.AddObject( ' 600% ', Tobject(600));
  cbDisplayZoom.Items.AddObject( ' 700% ', Tobject(700));
  cbDisplayZoom.Items.AddObject( ' 800% ', Tobject(800));
  cbDisplayZoom.ItemIndex := 2;
  DisplayZoom :=
    Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex]) * 0.01;


  // Contrast range
  edDisplayIntensityRange.LoValue := MainFrm.PhotoStim.GreyLo;
  edDisplayIntensityRange.HiValue := MainFrm.PhotoStim.GreyHi;


  // Create picture holders
  DisplayPic := TPicture.Create;


  ClientHeight := ControlGrp.Top + ControlGrp.Height + 5;


  // Set target table headers
  sgTargets.Cells[0,0] := 'No.';
  sgTargets.Cells[0,1] := 'Pre (ms)';
  sgTargets.Cells[0,2] := 'Duration (ms)';
  sgTargets.Cells[0,3] := 'Post (ms)';
  sgTargets.Cells[0,4] := 'Amp (mW)';
  sgTargets.Cells[0,5] := 'X (um)';
  sgTargets.Cells[0,6] := 'Y (um)';
  sgTargets.Colwidths[0] := sgTargets.Canvas.TextWidth('XXXXXXXXXX');


  // Load the protocol in current use (if any)
  if (MainFrm.PhotoStimFileName <> '') and
    FileExists(MainFrm.PhotoStimFileName) then
  begin
    LoadWaveform(MainFrm.PhotoStimFileName);
    UpdateTargetTable();
    UpdateDisplay();
  end
  else
    NewWaveform();


  // Display reference line
  ckReferenceLine.Checked := MainFrm.PhotoStim.RefLineEnabled;


  // Load image from Camera / PrairieView Ultima LSM
  bGetImage.Click;


  // Initialized
  Initialised := True;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.FormResize(Sender: TObject);
// --------------------
// Form resize callback
// --------------------
begin

  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.FormClose(Sender: TObject;
                                  var Action: TCloseAction);
// -------------------
// Form close callback
// -------------------
var
  i : Integer;  // Loop index
begin

  // Update settings
  UpdateSettings();

  // Clear buffers
  for i := 0 to 4 do
  begin
    if p_OrigFrame[i] = Nil then
    begin
      FreeMem(p_OrigFrame[i]);
      p_OrigFrame[i] := Nil;
    end;
  end;

  // Free display picture
  DisplayPic.Free;

  // Update protocol box in RecADCOnlyFrm
  for i := 0 to MainFrm.MDIChildCount - 1 do
  begin
    if (MainFrm.MDIChildren[i].Name = 'RecADCOnlyFrm') then
      TRecADCOnlyFrm(MainFrm.MDIChildren[i]).UpdatePhotoStimProgramList;
  end;

  // Make the window disappear
  Action := caFree;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ImageDblClick(Sender: TObject);
// ------------------------------------------
// Handle mouse double-click (add new target)
// ------------------------------------------
var
  DisplayRect : TRect;      // Display rectangle
  XValue, YValue : Single;  // Target position in um
  TargetPos : TPoint;       // Zoom adjusted position in pixels
begin

  // Check that max number of points is not exceeded
  if MainFrm.PhotoStim.NumStimPoints >= MaxStimPoints then Exit;

  // Display rectangle offset
  DisplayRect := Rect(0,
                      0,
                      Image.Width - 1,
                      Image.Height - 1);

  // Convert mouse click pixel location to target coordinates and store
  ConvertToTargetCoord(DisplayRect,
                       m_MouseClickPos.X,
                       m_MouseClickPos.Y,
                       m_X[MainFrm.PhotoStim.NumStimPoints],
                       m_Y[MainFrm.PhotoStim.NumStimPoints]);

  // Store point
  // First point, then set values to zero
  if (MainFrm.PhotoStim.NumStimPoints = 0) then
  begin
    m_PRE[MainFrm.PhotoStim.NumStimPoints] := 0.0;
    m_POST[MainFrm.PhotoStim.NumStimPoints] := 0.0;
    m_D[MainFrm.PhotoStim.NumStimPoints] := 0.0;
    m_A[MainFrm.PhotoStim.NumStimPoints] := 0.0;
    Inc(MainFrm.PhotoStim.NumStimPoints);
  end

  // Not first point, then set values to previous point's values
  else if (MainFrm.PhotoStim.NumStimPoints > 0) then
  begin
    m_PRE[MainFrm.PhotoStim.NumStimPoints] :=
      m_PRE[MainFrm.PhotoStim.NumStimPoints - 1];
    m_POST[MainFrm.PhotoStim.NumStimPoints] :=
      m_POST[MainFrm.PhotoStim.NumStimPoints - 1];
    m_D[MainFrm.PhotoStim.NumStimPoints] :=
      m_D[MainFrm.PhotoStim.NumStimPoints - 1];
    m_A[MainFrm.PhotoStim.NumStimPoints] :=
      m_A[MainFrm.PhotoStim.NumStimPoints - 1];
    Inc(MainFrm.PhotoStim.NumStimPoints);
  end;

  // Update display
  UpdateTargetTable();  
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ImageMouseDown(Sender: TObject;
                                       Button: TMouseButton;
                                       Shift: TShiftState;
                                       X, Y: Integer);
// -----------------
// Handle mouse down
// -----------------
var
  DisplayRect : TRect;
  XPix, YPix : Integer;
begin

  DisplayRect := Rect(0,
                      0,
                      Image.Width - 1,
                      Image.Height - 1);

  // If over a target, select move mode and record offset
  if m_MouseTargetSel >= 0 then
  begin
    ConvertToPixelCoord(DisplayRect,
                        m_X[m_MouseTargetSel],
                        m_Y[m_MouseTargetSel],
                        XPix, YPix);
    m_MouseMoveTarget := True;
  end

  // Mouse down and over reference line handle
  else if m_MouseRefLineSel then
  begin
    m_MouseMoveRefLine := True;
  end

  // Nothing to move
  else
  begin
    m_MouseMoveTarget := False;
    m_MouseMoveRefLine := False;
  end;

  // Store mouse position for use by ImageDblClick
  m_MouseClickPos.X := X;
  m_MouseClickPos.Y := Y;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ImageMouseMove(Sender: TObject;
                                       Shift: TShiftState;
                                       X, Y: Integer);
// -----------------
// Handle mouse move
// -----------------
var
    DisplayRect: TRect;
    i, XPix, YPix : Integer;
    XPos, YPos : Single;
    preX, preY : Single;        // Previous location for moving all points
    offsetX, offsetY : Single;  // Offset for moving all points
    tmp : Single;
begin

  // Display rectangle offset
  DisplayRect := Rect(0,
                      0,
                      Image.Width - 1,
                      Image.Height - 1);
                      

  // Calculate cursor position in target coordinates
  ConvertToTargetCoord(DisplayRect,
                       X, Y,
                       XPos, YPos);

  // Display cursor position in um
  lblCursorPosition.Caption := format('X=%.4g Y=%.4g um', [XPos, YPos]);


  // Determine target selected
  if (not m_MouseMoveTarget) or (m_MouseTargetSel < 0) then
  begin

    // Reset selected target
    m_MouseTargetSel := -1;

    // Find target
    for i := 0 to MainFrm.PhotoStim.NumStimPoints - 1 do
    begin

      // Convert to pixel coordinates
      ConvertToPixelCoord(DisplayRect, m_X[i], m_Y[i], XPix, YPix);

      // Test if mouse is in range
      if (Abs(X - XPix) < CrossHalfWidth) and
         (Abs(Y - YPix) < CrossHalfWidth) then
         m_MouseTargetSel := i;

    end;

  end

  // Move selected target
  else
  begin

    // Move all targets
    if (ckMoveAll.Checked) then
    begin

      // Get previous location for clicked point
      preX := m_X[m_MouseTargetSel];
      preY := m_Y[m_MouseTargetSel];

      // Convert pixel location to target coordinates
      // Move clicked point
      ConvertToTargetCoord(DisplayRect,
                           X, Y,
                           m_X[m_MouseTargetSel], m_Y[m_MouseTargetSel]);

      // Calculate offset for clicked point
      offsetX := m_X[m_MouseTargetSel] - preX;
      offsetY := m_Y[m_MouseTargetSel] - preY;

      for i := 0 to MainFrm.PhotoStim.NumStimPoints - 1 do
      begin

        // Skip clicked point: move all others
        if i <> m_MouseTargetSel then
        begin
          m_X[i] := m_X[i] + offsetX;
          m_Y[i] := m_Y[i] + offsetY;
        end;

      end;

      // Convert pixel location to target coordinates
      ConvertToTargetCoord(DisplayRect,
                           X, Y,
                           m_X[m_MouseTargetSel], m_Y[m_MouseTargetSel]);

    end

    // Move one target
    else
    begin

      // Convert pixel location to target coordinates
      ConvertToTargetCoord(DisplayRect,
                           X, Y,
                           m_X[m_MouseTargetSel], m_Y[m_MouseTargetSel]);

    end;

    // Update display
    UpdateDisplay();
    
  end;


  // Reference line in pixel coordinates
  ConvertToPixelCoord(DisplayRect,
                      0.0,
                      MainFrm.PhotoStim.RefLinePos,
                      XPix, YPix);

  // Check if over reference line handles
  if ((X < 10) or (X > (Image.Width * DisplayZoom) - 10)) then
  begin

    if ((Y > (YPix - 5)) and (Y < (YPix + 5))) then
    begin
      m_MouseRefLineSel := True;
    end
    else
    begin
      m_MouseRefLineSel := False;
    end;

  end
  else
  begin
    m_MouseRefLineSel := False;
  end;

  // Move reference line
  if m_MouseMoveRefLine then
  begin

    // Keep reference line within image
    if (Y < 0) then Y := 0;
    if (Y > Image.Height) then Y := Image.Height;

    // Convert pixel location to target coordinates
    ConvertToTargetCoord(DisplayRect,
                         X,
                         Y,
                         tmp,
                         MainFrm.PhotoStim.RefLinePos);

    // Update display
    UpdateDisplay();

  end;


  // Set cursor
  if m_MouseRefLineSel  or (m_MouseTargetSel >= 0) then
  begin
    Image.Cursor := crDrag;
  end
  else
  begin
    Image.Cursor := crCross;
  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ImageMouseUp(Sender: TObject;
                                      Button: TMouseButton;
                                      Shift: TShiftState;
                                      X, Y: Integer);
// ---------------
// Handle mouse up
// ---------------
begin

  // Reset mouse cursor
  Image.Cursor := crCross;

  // Reset mouse movement flags
  //m_MouseTargetSel := -1;
  m_MouseMoveRefLine := False;
  m_MouseMoveTarget := False;

  // Update positions in table when mouse released
  UpdateTargetTable();  

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.bDeleteTargetsClick(Sender: TObject);
// ------------------------
// Delete all target points
// ------------------------
var
  i : Integer;
  r, c : Integer;
begin

  // Reset protocol values
  for i := 0 to MainFrm.PhotoStim.NumStimPoints - 1 do
  begin
    m_X[i] := 0.0;
    m_Y[i] := 0.0;
    m_PRE[i] := 0.0;
    m_POST[i] := 0.0;
    m_D[i] := 0.0;
    m_A[i] := 0.0;
  end;

  // Clear targets in table
  MainFrm.PhotoStim.NumStimPoints := 0;
  for r := 0 to sgTargets.RowCount-1 do
    for c := 1 to sgTargets.ColCount-1 do sgTargets.Cells[c,r] := '';

  // Update display
  UpdateTargetTable();
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.bDuplicateClick(Sender: TObject);
// ---------------------------
// Duplicate all target points
// ---------------------------
var
  i : Integer;         // Loop index
  inputVal : String;   // InputBox string
  pointVal : String;   // InputBox string
  num : Integer;       // Original number of points
  value : Single;      // Delay between runs of points
begin

  // Get input from user
  inputVal := InputBox('Duplicate Points', 'Enter PRE delay (ms):', '1');

  // User canceled operation
  if inputVal = '' then Exit;

  // Get input as valid Single
  if ValidateNumericEntry(inputVal, value) = False then
  begin
    ShowMessage('Invalid input to Duplicate Points: ' + inputVal);
    Exit;
  end;


  // Get input from user
  pointVal := InputBox('Duplicate Points', 'Duplicate up to point:', '1');

  // User canceled operation
  if pointVal = '' then Exit;

  // Get input as valid Single
  if ValidateNumericEntry(pointVal, value) = False then
  begin
    ShowMessage('Invalid input to Duplicate Points: ' + pointVal);
    Exit;
  end;

  // Check against zero
  if (StrToInt(pointVal) < 0) then
  begin
    ShowMessage('Invalid input to Duplicate Points: ' + pointVal);
    Exit;
  end;

  // Check against maximum number of points
  if (StrToInt(pointVal) > (MainFrm.PhotoStim.NumStimPoints - 1)) then
  begin
    ShowMessage('Invalid input to Duplicate Points: ' + pointVal);
    Exit;
  end;


  // Copy points
  num := StrToInt(pointVal);
  for i := 0 to num do
  begin
    m_X[MainFrm.PhotoStim.NumStimPoints] := m_X[i];
    m_Y[MainFrm.PhotoStim.NumStimPoints] := m_Y[i];
    if i = 0 then
      m_PRE[MainFrm.PhotoStim.NumStimPoints] := value
    else
      m_PRE[MainFrm.PhotoStim.NumStimPoints] := m_PRE[i];
    m_POST[MainFrm.PhotoStim.NumStimPoints] := m_POST[i];
    m_D[MainFrm.PhotoStim.NumStimPoints] := m_D[i];
    m_A[MainFrm.PhotoStim.NumStimPoints] := m_A[i];
    Inc(MainFrm.PhotoStim.NumStimPoints);
  end;

  // Update display
  UpdateTargetTable();
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.bGetImageClick(Sender: TObject);
// -----------------------------------------------------------
// Get images from acquisition system when SNAP button clicked
// -----------------------------------------------------------
var
  i : Integer;                   // Loop index
  isImageAvailable : Boolean;    // Camera image is available flag
  isRecordFrmOpen : Boolean;     // RecordFrm window is open flag
begin

  // Initialize flag
  isRecordFrmOpen := False;
  

  // Check for no camera
  if (MainFrm.CameraType = NoCamera8) or
     (MainFrm.CameraType = NoCamera16) then
  begin
      MessageDlg('There is no camera setup.', mtError, [mbOK], 0);
      Close;
      Exit;
  end


  // Check for unsupported BioRad Radiance/MRC 1024
  else if MainFrm.CameraType = BioRad then
  begin
      MessageDlg('This feature is not supported on a BioRad Radiance/MRC 1024.',
                 mtError, [mbOK], 0);
      Close;
      Exit;
  end


  // SNAP image from Ultima/PrairieView
  else if MainFrm.CameraType = UltimaLSM then
  begin

    // Number of bytes per pixel is always 2 for PriarieView
    m_OrigFrameBPP := 2;

    // Enable channel combo box
    cbChannel.Enabled := True;

    // Enable intensity range edit box
    edDisplayIntensityRange.Enabled := True;

    // Send SNAP command to Ultima
    Ultima.SendSnap;

    // Check if Ultima is active
    if not Ultima.IsUltimaActive then
    begin
      MessageDlg('There is no connection to the Ultima.', mtError, [mbOK], 0);
      Exit;
    end;

    // Update status bar
    MainFrm.StatusBar.SimpleText := ' Waiting for NEWIMAGE command... ';

  end


  // All other non-LSM imaging cameras
  else
  begin

    // Check that RecordFrm (RecUnit) is open
    for i := 0 to Mainfrm.MDIChildCount-1 do
    begin
      if ANSIContainsText(Mainfrm.MDIChildren[i].Name, 'RecordFrm') then
      begin
        if TRecordFrm(MainFrm.MDIChildren[i]).ImageAvailable then
        begin
          isRecordFrmOpen := True;
        end;
      end;
    end;

    // Show error if Record Images & Signals form is not open
    if not isRecordFrmOpen then
    begin
      MessageDlg('Record Images & Signals is not open.', mtError, [mbOK], 0);
      Close;
      Exit;
    end;

    // Check for image
    isImageAvailable := RecordFrm.ImageAvailable;
    if not isImageAvailable then
    begin
      MessageDlg('No image is available.', mtError, [mbOK], 0);
      Close;
      Exit;
    end;

    // Update status bar
    MainFrm.StatusBar.SimpleText := ' Acquiring new image(s)... ';

    // Get images from camera
    CameraSnap();

    // Update status bar
    MainFrm.StatusBar.SimpleText := ' Received new image(s). ';    


    // Update display
    FormResize(nil);

  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.bOpenProtocolClick(Sender: TObject);
// -----------------------------
// Open waveform button callback
// -----------------------------
begin

  // Get filename
  OpenDialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist];
  OpenDialog.FileName := ExtractFileName(SaveDialog.FileName);
  OpenDialog.InitialDir := MainFrm.PProtDirectory;
  OpenDialog.Title := 'Load Photo-Stimulus Protocol';

  // Open file
  if OpenDialog.Execute then
  begin

    // Load wave form
    LoadWaveform(OpenDialog.FileName);

    // Update display and tables
    UpdateTargetTable();
    UpdateDisplay();
    
  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.bPowerCalibrateClick(Sender: TObject);
// -----------------------
// Show calibration dialog
// -----------------------
begin

  if MainFrm.FormExists('PhotoStimSetupFrm') then
    PhotoStimSetupFrm.SetFocus
  else
  begin
    PhotoStimSetupFrm := TPhotoStimSetupFrm.Create(Self);
    PhotoStimSetupFrm.Left := 10;
    PhotoStimSetupFrm.Top := 10;
  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.bSaveImageClick(Sender: TObject);
var
  FileName : String;
  Row,i,j,k,NumComponentsPerPixel,NumBytesPerLine : Integer;
  Buf, pLine : PByteArray;
  SaveDialog : TSaveDialog;
  SavePic : TPicture;
begin

  // Create the save dialog object
  saveDialog := TSaveDialog.Create(self);

  // Give the dialog a title
  saveDialog.Title := 'Save image as...';

  // Set up the starting directory to be the current one
  saveDialog.InitialDir := GetCurrentDir;

  // Allow only .txt and .doc file types to be saved
  saveDialog.Filter := 'TIFF file|*.tif';

  // Set the default extension
  saveDialog.DefaultExt := 'tif';

  // Select text files as the starting filter type
  saveDialog.FilterIndex := 1;

  // Display the save file dialog
  if saveDialog.Execute then
  begin

    // Determine pixel format
    case Image.Picture.Bitmap.PixelFormat of
      pf8bit : NumComponentsPerPixel := 1;
      pf16bit : NumComponentsPerPixel := 2;
      pf24bit : NumComponentsPerPixel := 3;
      pf32bit : NumComponentsPerPixel := 4;
      else NumComponentsPerPixel := 1;
    end ;

    // Calculate scan line size
    NumBytesPerLine := NumComponentsPerPixel * Image.Picture.Bitmap.Width;

    // Allocate memory
    GetMem(Buf, NumBytesPerLine * Image.Picture.Bitmap.Height);

    // Create a temporary picture for drawing
    SavePic := TPicture.Create;

    // Draw the camera/LSM image
    SavePic.Assign(DisplayPic);

    // Draw targets on picture
    DisplayTargets(SavePic);

    // Draw calibration bar
    if ckCalibrationBar.Checked then DisplayCalibrationBar(SavePic);

    // Draw reference line
    if ckReferenceLine.Checked then DisplayRefLine(SavePic);

    // Copy image from temporary picture to allocated buffer
    j := 0;
    for Row := 0 to SavePic.Height-1 do begin
      pLine := SavePic.Bitmap.ScanLine[Row];
      for i := 0 to SavePic.Bitmap.Width-1 do begin
        for k := NumComponentsPerPixel-1 downto 0 do begin
          Buf^[j] := pLine^[i * NumComponentsPerPixel + k];
          Inc(j);
        end;
      end;
    end;

    // Create image file
    ImageFile.CreateFile(saveDialog.FileName,
                         SavePic.Bitmap.Width,
                         SavePic.Bitmap.Height,
                         8, NumComponentsPerPixel,
                         True);

    // Save buffer to file
    ImageFile.SaveFrame(1, Buf);

    // Close file
    ImageFile.CloseFile;

    // Free memory
    FreeMem(Buf);

    // Free temporary picture
    SavePic.Free;

  end;

  // Free the dialog
  saveDialog.Free;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.bSaveProtocolAsClick(Sender: TObject);
// ----------------------
// SaveAs button callback
// ----------------------
begin

  // Set-up dialog box
  SaveDialog.options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist];
  SaveDialog.InitialDir := MainFrm.PProtDirectory ;
  SaveDialog.Title := 'Save Photo-Stimulus Protocol';

  // Set file name
  if FileExists(PhotoStimulator.FileName) then
    SaveDialog.FileName := ExtractFileName(PhotoStimulator.FileName)
  else SaveDialog.FileName := '*.ppr';

  // Save protocol if users clicks OK
  if SaveDialog.execute then SaveWaveform(SaveDialog.FileName);

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.bSaveProtocolClick(Sender: TObject);
// --------------------
// Save button callback
// --------------------
begin

  // Save waveform
  SaveWaveform(PhotoStimulator.FileName);

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.cbAttenuatorChange(Sender: TObject);
begin

  // Validate protocol
  ValidateStimulusProtocol();

  // Update display
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.cbChannelChange(Sender: TObject);
// --------------
// Change channel
// --------------
begin

  // Update display
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.cbDisplayZoomChange(Sender: TObject);
// ------------------------------
// Change zoom combo box callback
// ------------------------------
begin

  // Update display
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ckCalibrationBarClick(Sender: TObject);
// ----------------------
// Toggle calibration bar
// ----------------------
begin

  // Update display
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ckPhotoStimRepeatClick(Sender: TObject);
// ----------------------------------
// Repeat stimulus check box callback
// ----------------------------------
begin

  // Update settings in main form
  UpdateSettings();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ckReferenceLineClick(Sender: TObject);
// ---------------------
// Toggle reference line
// ---------------------
begin

  // Update display
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.edDisplayIntensityRangeKeyPress(Sender: TObject;
                                                        var Key: Char);
// --------------------------------
// Intensity range changed callback
// --------------------------------
begin

  if Key = #13 then
  begin

    // Set low and hi values in MainFrm
    MainFrm.PhotoStim.GreyLo := Round(edDisplayIntensityRange.LoValue);
    MainFrm.PhotoStim.GreyHi := Round(edDisplayIntensityRange.HiValue);

    // Update display
    UpdateDisplay();
    
  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.edPhotoStimPeriodKeyPress(Sender: TObject;
                                                  var Key: Char);
// -------------------------------------
// Validate repeat period edit box value
// -------------------------------------
var
  duration : Single;
begin

  if Key = #13 then
  begin

    // Calcualte protocol duration
    duration := CalculateDuration;

    // Check period edit field
    if ((edPhotoStimPeriod.Value * 1000.0) < duration) then
    begin
      edPhotoStimPeriod.Value := (duration / 1000.0);
    end;

  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.menuDeleteTargetClick(Sender: TObject);
// --------------------------------
// Delete Target menu item callback
// --------------------------------
var
  i: Integer;
begin

  // Not over grid or over a row/column header
  if (m_TargetsColRClk < 0) or (m_TargetsRowRClk < 0) then Exit;

  // Column value bigger than number of photo stim points
  if ((m_TargetsColRClk - 1) > MainFrm.PhotoStim.NumStimPoints - 1) then Exit;

  // Shift points left by one
  for i := m_TargetsColRClk - 1 to MainFrm.PhotoStim.NumStimPoints - 1 do
  begin
    if i = MainFrm.PhotoStim.NumStimPoints - 1 then
    begin
      m_PRE[i] := 0;
      m_POST[i] := 0;
      m_D[i] := 0;
      m_A[i] := 0;
      m_X[i] := 0;
      m_Y[i] := 0;
    end
    else
    begin
      m_PRE[i] := m_PRE[i + 1];
      m_POST[i] := m_POST[i + 1];
      m_D[i] := m_D[i + 1];
      m_A[i] := m_A[i + 1];
      m_X[i] := m_X[i + 1];
      m_Y[i] := m_Y[i + 1];
    end;
  end;

  // Decrement number of stim points
  MainFrm.PhotoStim.NumStimPoints := MainFrm.PhotoStim.NumStimPoints - 1;  

  // Update display
  UpdateTargetTable();  
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.menuSetAllClick(Sender: TObject);
// --------------------------
// Set All menu item callback
// --------------------------
var
  i : Integer;   // Loop index
  val : String;  // Copy of grid cell
begin

  // Not over grid or over a row/column header
  if (m_TargetsColRClk < 0) or (m_TargetsRowRClk < 0) then Exit;

  // Get entry to copy
  val := sgTargets.Cells[m_TargetsColRClk, m_TargetsRowRClk];

  // Set protocol values
  for i := 0 to MainFrm.PhotoStim.NumStimPoints - 1 do
  begin
    sgTargets.Cells[i+1, m_TargetsRowRClk] := val;
  end;

  // Validate protocol
  ValidateStimulusProtocol();

  // Update display
  UpdateTargetTable();
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.sgTargetsKeyPress(Sender: TObject; var Key: Char);
// ------------------------------
// String grid key press callback
// ------------------------------
begin
            
  if Key = #13 then
  begin

    // Validate protocol
    ValidateStimulusProtocol();

    // Update display
    UpdateDisplay();

  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.sgTargetsMouseDown(Sender: TObject;
                                           Button: TMouseButton;
                                           Shift: TShiftState;
                                           X, Y: Integer);
// --------------------------------
// Targets grid mouse down callback
// --------------------------------
var
  RClkPoint: TPoint;             // X, Y click position in scrren coordinates
begin

  // Exit if not right button
  if (Button <> mbRight) then
    Exit;

  // Store the Column and Row the user right-clicked on
  sgTargets.MouseToCell(X, Y, m_TargetsColRClk, m_TargetsRowRClk);

  // The user's right-click was not within a cell
  if (m_TargetsColRClk < 0) or (m_TargetsRowRClk < 0) then
    Exit;

  // The user clicked on a header
  if ((m_TargetsColRClk < sgTargets.FixedCols) or
      (m_TargetsRowRClk < sgTargets.FixedRows)) then
    Exit;

  // X, Y contains coordinates within the grid sgTargets.
  // Convert them to screen coordinates.
  RClkPoint := sgTargets.ClientToScreen(Point(X, Y));

  //  Display the popup
  menuTargets.Popup(RClkPoint.X, RClkPoint.Y);

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.sgTargetsSelectCell(Sender: TObject; ACol,
                                            ARow: Integer;
                                            var CanSelect: Boolean);
// --------------------------------
// String grid select cell callback
// --------------------------------
begin


  // Validate protocol
  ValidateStimulusProtocol();

  // Update display
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.TimerTimer(Sender: TObject);
// --------------------------------------------------------------------
// Timer callback used to wait for images from PrairieView / Ultima LSM
// --------------------------------------------------------------------
begin

  // Exit timer if the camera is not UltimaLSM
  if MainFrm.CameraType <> UltimaLSM then
  begin
    Exit;
  end;

  // If Ultima is ready with a new image from NEWIMAGE command
  if Ultima.NewImageCompleted then
  begin

    // Get images from PrairieView / Ultima LSM
    UltimaSnap();

    // Update status bar
    MainFrm.StatusBar.SimpleText := ' Received new image(s). ';

    // Reset completed flag in Ultima unit
    Ultima.NewImageCompleted := False;

  end;

end;


// -----------------------------------------------------------------------------


function TPhotoStimFrm.CalculateDuration : Single;
// ---------------------------
// Calculate protocol duration
// ---------------------------
var
  duration : Single;  // Duration
  i : Integer;        // Loop index
begin

  // Initialize total duration
  duration := 0.0;

  // Save to MainFrm.PhotoStim
  for i := 0 to MainFrm.PhotoStim.NumStimPoints - 1 do
  begin
    duration := duration + m_PRE[i] + m_D[i] + m_POST[i];
  end;

  // Return result
  Result := duration;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.CameraSnap();
// -----------------------
// Get images from non-LSM
// -----------------------
var
  FrameHeight : Integer;         // Frame height in pixels
  FrameWidth : Integer;          // Frame width in pixels
  i, j : Integer;                // Loop indices
  PImageSource : Pointer;        // Pointer to source camera image
  NumBytesPerFrame : Integer;    // Number of bytes per frame
  NumBytesPerPixel : Integer;    // Number of bytes per pixel  
  NumPixelsPerFrame : Integer;   // Number of pixels in image
begin

  // Disable channel combo box
  cbChannel.Enabled := False;

  // Reset ROI voltage offsets to zero just in case...
  MainFrm.PhotoStim.ROIXVoltageOffset := 0.0;
  MainFrm.PhotoStim.ROIYVoltageOffset := 0.0;

  // Only one channel from camera image. Maybe more in the future...
  m_NumChannels := 1;

  // Get image source
  PImageSource := RecordFrm.PDisplayBufs[0];
  if PImageSource = Nil then
  begin
    MessageDlg('Error getting camera image.', mtError, [mbOK], 0);
    Close;
    Exit;
  end;

  // Get camera image properties
  NumBytesPerPixel := RecordFrm.NumBytesPerPixel;
  NumPixelsPerFrame := RecordFrm.NumPixelsPerFrame;
  NumBytesPerFrame := NumBytesPerPixel * NumPixelsPerFrame;
  FrameWidth := RecordFrm.FrameWidth;
  FrameHeight := NumPixelsPerFrame div FrameWidth;


  // Enable/disable intensity range edit box
  if NumBytesPerPixel <> 2 then
  begin
    edDisplayIntensityRange.Enabled := False;
  end
  else
  begin
    edDisplayIntensityRange.Enabled := True;
  end;


  // Free previous camera images
  for i := 0 to 4 do
  begin
    if p_OrigFrame[i] = Nil then
    begin
      FreeMem(p_OrigFrame[i]);
      p_OrigFrame[i] := Nil;
    end;
  end;

  // Allocate buffer for original camera image.
  for i := 0 to m_NumChannels-1 do
  begin
    GetMem(p_OrigFrame[i], NumBytesPerFrame);
  end;

  // Copy frame buffers
  for i := 0 to m_NumChannels-1 do
  begin
    for j := 0 to NumBytesPerFrame-1 do
    begin
      PByteArray(p_OrigFrame[i])^[j] := PIntArray(PImageSource)^[j];
    end;
  end;


  // Assign unit/object variables from local copies
  m_OrigFrameHeight := FrameHeight;
  m_OrigFrameWidth := FrameWidth;
  m_OrigFrameBPP := NumBytesPerPixel;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ConvertToPixelCoord(
          DisplayRect : TRect;
          XMicron : Single;
          YMicron : Single;
          var XPixel : Integer;
          var YPixel : Integer);
// ----------------------------
// Convert to pixel coordinates
// ----------------------------
var
  XTemp, YTemp : Single;
begin

  XTemp :=
    (XMicron / MainFrm.PhotoStim.XMicronsPerPixel) + (m_OrigFrameWidth div 2);
  XPixel := Round(XTemp * DisplayZoom) - DisplayRect.Left;

  YTemp :=
    (YMicron / MainFrm.PhotoStim.YMicronsPerPixel) + (m_OrigFrameHeight div 2);
  YPixel := Round(YTemp * DisplayZoom) - DisplayRect.Top;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ConvertToTargetCoord(
          DisplayRect : TRect;
          XPixel : Integer;
          YPixel : Integer;
          var XMicron : Single;
          var YMicron : Single);
// --------------------------------------
// Convert to absolute target coordinates
// --------------------------------------
var
  XTemp, YTemp : Single;
begin

  XTemp := (XPixel + DisplayRect.Left) / DisplayZoom;
  XMicron :=
    (XTemp - (m_OrigFrameWidth div 2)) * MainFrm.PhotoStim.XMicronsPerPixel;

  YTemp := (YPixel + DisplayRect.Top) / DisplayZoom;
  YMicron :=
    (YTemp - (m_OrigFrameHeight div 2)) * MainFrm.PhotoStim.YMicronsPerPixel;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.DisplayCalibrationBar(Const Pic : TPicture);
// ---------------------------------------
// Add a spatial calibration bar to bitmap
// ---------------------------------------
var
  barSize : Integer ;
begin

  // Check microns per pixel values
  if MainFrm.PhotoStim.XMicronsPerPixel = 0.0 then
    MainFrm.PhotoStim.XMicronsPerPixel := 1.0;
  if MainFrm.PhotoStim.YMicronsPerPixel = 0.0 then
    MainFrm.PhotoStim.YMicronsPerPixel := 1.0;

  // Determine size of calibration bar
  barSize := Round((MainFrm.CalibrationBarSize * DisplayZoom) /
    MainFrm.PhotoStim.XMicronsPerPixel);

  // Set pen and text properties
  Pic.Bitmap.Canvas.Pen.Color := clAqua;
  Pic.Bitmap.Canvas.Pen.Width := 2;
  Pic.Bitmap.Canvas.Brush.Style := bsClear;
  Pic.Bitmap.Canvas.TextFlags := 0;
  Pic.Bitmap.Canvas.Pen.Mode := pmCopy;
  Pic.Bitmap.Canvas.Font.Name := 'Arial';
  Pic.Bitmap.Canvas.Font.Size := 8;
  Pic.Bitmap.Canvas.Font.Color := clAqua;

  // Draw bar
  Pic.Bitmap.Canvas.Polyline(
    [Point(2, Pic.Bitmap.Height - 1),
     Point(2 + barSize, Pic.Bitmap.Height - 1)]);

  // Display calibration bar size
  Pic.Bitmap.Canvas.TextOut(2,
    Pic.Bitmap.Height - 3 - Pic.Bitmap.Canvas.TextHeight('X'),
    format('%.3g um', [MainFrm.CalibrationBarSize]));

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.DisplayRefLine(Const Pic : TPicture);
// ----------------------------
// Add reference line to bitmap
// ----------------------------
var
  X, Y : Integer;         // X, Y pixel location of reference line
  DisplayRect : TRect;    // Display rectangle
begin

  // Set pen properties
  Pic.Bitmap.Canvas.Pen.Color := clAqua	;
  Pic.Bitmap.Canvas.Pen.Width := 1;
  Pic.Bitmap.Canvas.font.Color := clAqua	;
  Pic.Bitmap.Canvas.Brush.Style := bsClear;
  Pic.Bitmap.Canvas.Pen.Mode := pmCopy;

  // Display rectangle offset
  DisplayRect := Rect(0,
                      0,
                      Image.Width - 1,
                      Image.Height - 1);

  // Get line position in pixels
  ConvertToPixelCoord(DisplayRect,
                      0.0,
                      MainFrm.PhotoStim.RefLinePos,
                      X, Y);

  // Check bounds                      
  if (Y < 0) then Y := 0;
  if (Y > Pic.Height) then Y := Pic.Height;

  // Draw line
  Pic.Bitmap.Canvas.Polyline([Point(0, Y),
                              Point(Pic.Bitmap.Width, Y)]);

  // Draw left handle
  Pic.Bitmap.Canvas.Rectangle(0, Y-5,
                              10, Y+6);

  // Draw right handle
  Pic.Bitmap.Canvas.Rectangle(Pic.Bitmap.Width, Y - 5,
                              Pic.Bitmap.Width - 10, Y + 6);

end ;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.DisplayTargets(const Pic : TPicture);
// ------------
// Draw targets
// ------------
const
  nLineSize = 6;
  nSpace = 5;
var
  DisplayRect : TRect;
  i : Integer;
  X, Y : Integer;
begin

  // Display rectangle offset
  DisplayRect := Rect(0,
                      0,
                      Image.Width - 1,
                      Image.Height - 1);

  // Draw each target
  for i := 0 to MainFrm.PhotoStim.NumSTimPoints - 1 do
  begin

    // Get pixel position
    ConvertToPixelCoord(DisplayRect,
                        m_X[i],
                        m_Y[i],
                        X, Y);

    // Target background
    Pic.Bitmap.Canvas.Pen.Color := $000050FF;
    Pic.Bitmap.Canvas.Pen.Width := 3;
    Pic.Bitmap.Canvas.Brush.Style := bsClear;
    Pic.Bitmap.Canvas.Pen.Mode := pmCopy;

    // Horizontal line
    Pic.Bitmap.Canvas.Polyline(
      [Point(X - nSpace, Y), Point(X - nSpace - nLineSize, Y)]);
    Pic.Bitmap.Canvas.Polyline(
      [Point(X + nSpace, Y), Point(X + nSpace + nLineSize, Y)]);

    // Vertical line
    Pic.Bitmap.Canvas.Polyline(
      [Point(X, Y - nSpace), Point(X, Y - nSpace - nLineSize)]);
    Pic.Bitmap.Canvas.Polyline(
      [Point(X, Y + nSpace), Point(X, Y + nSpace + nLineSize)]);


    // Target foreground for contrast
    Pic.Bitmap.Canvas.Pen.Color := clBlack;
    Pic.Bitmap.Canvas.Pen.Width := 1;
    Pic.Bitmap.Canvas.Brush.Style := bsClear;
    Pic.Bitmap.Canvas.Pen.Mode := pmCopy;

    // Horizontal line
    Pic.Bitmap.Canvas.Polyline(
      [Point(X - nSpace,Y), Point(X - nSpace - nLineSize,Y)]);
    Pic.Bitmap.Canvas.Polyline(
      [Point(X + nSpace,Y), Point(X + nSpace + nLineSize,Y)]);

    // Vertical line
    Pic.Bitmap.Canvas.Polyline(
      [Point(X, Y - nSpace), Point(X, Y - nSpace - nLineSize)]);
    Pic.Bitmap.Canvas.Polyline(
      [Point(X, Y + nSpace), Point(X, Y + nSpace + nLineSize)]);


    // Text label
    Pic.Bitmap.Canvas.Font.Color := $000050FF;
    Pic.Bitmap.Canvas.TextOut(X + nSpace + nLineSize + 1,
                              Y,
                              format(' %d', [i]));

  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.LoadWaveform(FileName : String);
// ------------------------------------------
// Load a photo-stimulus waveform from a file
// ------------------------------------------
var
  i, c, r : Integer;
begin

  // Clear all waveform elements in protocol
  PhotoStimulator.ClearWaveformElements;

  // Clear string grid
  bDeleteTargetsClick(nil);

  FileName := ChangeFileExt(FileName, '.ppr');
  Caption := 'Photo-Stimulus Protocol: ' + ExtractFileNameOnly(FileName);

  // Get voltage program from PPR file
  PhotoStimulator.LoadProgram(FileName);
  PhotoStimulator.FileName := FileName;

  // Load protocol into local buffers
  PhotoStimulator.GetProtocol(m_X,
                              m_Y,
                              m_PRE,
                              m_D,
                              m_A,
                              m_POST,
                              MainFrm.PhotoStim.NumStimPoints);

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.NewWaveform();
// -------------------------------------------
// Create a new, blank photo-stimulus waveform
// -------------------------------------------
var
  FileName : String;
begin

  // Set file name to Untitled.ppr
  FileName := 'Untitled.ppr';

  // Clear string grid
  bDeleteTargetsClick(nil);

  // Clear all waveform elements in protocol
  PhotoStimulator.ClearWaveformElements;

  // Set filename to untitled
  PhotoStimulator.FileName := MainFrm.PProtDirectory + FileName;

  // Load protocol into local buffers
  PhotoStimulator.GetProtocol(m_X,
                              m_Y,
                              m_PRE,
                              m_D,
                              m_A,
                              m_POST,
                              MainFrm.PhotoStim.NumStimPoints);

  // Set window caption
  FileName := ChangeFileExt(FileName, '.ppr');
  Caption := 'Photo-Stimulus Protocol: ' + ExtractFileNameOnly(FileName);

  // Update display
  UpdateDisplay();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.SaveWaveform(FileName : String);
// ------------------------------------
// Save photo-stimulus protocol to file
// ------------------------------------
begin

  // Set values in PhotoStimulator
  PhotoStimulator.SetProtocol(m_X,
                              m_Y,
                              m_PRE,
                              m_D,
                              m_A,
                              m_POST,
                              MainFrm.PhotoStim.NumStimPoints);

  // Set file name and save protocol
  FileName := ChangeFileExt(FileName, '.ppr');
  Caption := 'Photo-stimulus Protocol : ' + ExtractFileNameOnly(FileName);
  PhotoStimulator.SaveProgram(FileName);

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.SetImagePanels();
// -------------------------------------------
// Set size and number of image display panels
// -------------------------------------------
const
  MarginPixels = 16 ;
var
  ImageAreaHeight : Integer ;
  ImageAreaWidth : Integer ;
  RightEdge : Integer ;
  BottomEdge : Integer ;
begin

  // Resize control group height
  ControlGrp.Height := ClientHeight - ControlGrp.Top - 5;

  // Get zoom factor
  DisplayZoom :=
    Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex]) * 0.01;

  // Resize image group
  ImageGrp.ClientWidth := Max(ClientWidth - ImageGrp.Left - 5, 2);
  ImageGrp.ClientHeight := Max(ClientHeight - ImageGrp.Top - 5, 2);

  // Resize image
  Image.Width := Round(m_OrigFrameWidth * DisplayZoom);
  Image.Height := Round(m_OrigFrameHeight * DisplayZoom);

  // Set control panel location
  pnlControls.Top := Image.Top + Image.Height;
  pnlControls.Left := Image.Left;



  // Set top left location of image
  Image.Left := MarginPixels;
  Image.Top := MarginPixels;

  Image.Canvas.Pen.Color := clWhite;
  Image.Canvas.Brush.Style := bsClear;
  Image.Canvas.Font.Color := clWhite;
  Image.Canvas.TextFlags := 0;
  Image.Canvas.Pen.Mode := pmCopy;
  Image.Canvas.Font.Name := 'Arial';
  Image.Canvas.Font.Size := 8;
  Image.Canvas.Font.Color := clBlue;
  Image.Cursor := crCross;

  // Determine right/bottom edge of image area
  RightEdge := Image.Left + Image.Width + 1;
  BottomEdge := Image.Top + Image.Height + 1;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.UltimaSnap();
// -----------------------------------------
// Load images from PrairieView / Ultima LSM
// -----------------------------------------
var
  chName : String;               // Channel name
  i : Integer;                   // Loop index
begin

  // Number of PMT channels
  m_NumChannels := MainFrm.PhotoStim.NumPMTFiles;

  // Enable channel combo box
  cbChannel.Enabled := True;

  // Populate channel combo box
  cbChannel.Clear;
  for i := 0 to m_NumChannels - 1 do
  begin
    if MainFrm.PhotoStim.PMTFileNames[i] <> '' then begin
      chName :=
        MainFrm.PhotoStim.PMTChannelNames[MainFrm.PhotoStim.PMTChannels[i]];
      cbChannel.Items.AddObject(chName, Tobject(i));
    end;
  end;

  // Set initial channel combo box index
  if MainFrm.PhotoStim.NumPMTFiles > 0 then cbChannel.ItemIndex := 0;


  // Free previous camera images
  for i := 0 to 4 do
  begin
    if p_OrigFrame[i] = Nil then
    begin
      FreeMem(p_OrigFrame[i]);
      p_OrigFrame[i] := Nil;
    end;
  end;


  // Load image for each channel
  for i := 0 to m_NumChannels - 1 do
  begin

    // Check for file
    if MainFrm.PhotoStim.PMTFileNames[i] = '' then
    begin
      MessageDlg('No file associated with channel.', mtError, [mbOK], 0);
      Exit;
    end;

    // Attempt to open image
    if not UltimaImageFile.OpenFile(MainFrm.PhotoStim.PMTFileNames[i]) then
    begin
      MessageDlg('Error loading image file.', mtError, [mbOK], 0);
      Exit;
    end;

    // Allocate frame buffer for image
    GetMem(p_OrigFrame[i], UltimaImageFile.NumBytesPerFrame);

    // Get image buffer
    UltimaImageFile.LoadFrame(1, p_OrigFrame[i]);

    // Set image properties
    m_OrigFrameHeight := UltimaImageFile.FrameHeight;
    m_OrigFrameWidth := UltimaImageFile.FrameWidth;
    m_OrigFrameBPP := UltimaImageFile.NumBytesPerFrame div
      (m_OrigFrameHeight * m_OrigFrameWidth);

    // Close image file
    UltimaImageFile.CloseFile;

    // Update display
    UpdateDisplay();

  end;


end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.UpdateDisplay();
// --------------
// Redraw display
// --------------
begin

  SetImagePanels();
  ZoomDisplay();
  DisplayTargets(Image.Picture);
  if ckCalibrationBar.Checked then DisplayCalibrationBar(Image.Picture);
  if ckReferenceLine.Checked then DisplayRefLine(Image.Picture);
  UpdateDuration();

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.UpdateDuration();
// ----------------------------------------------------------------
// Calcualte protocol duration and update label and repeat edit box
// ----------------------------------------------------------------
var
  duration : Single;  // Duration
  i : Integer;        // Loop index
begin

  // Calcualte protocol duration
  duration := CalculateDuration;

  // Update duration label
  lblDuration.Caption := FloatToStr(duration);

  // Check period edit field
  if ((edPhotoStimPeriod.Value * 1000.0) < duration) then
  begin
    edPhotoStimPeriod.Value := (duration / 1000.0);
  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.UpdateSettings;
// --------------------------------------------
// Update PhotoStim master record from controls
// --------------------------------------------
begin

  if not Initialised then Exit;

  // Save repeat period
  MainFrm.PhotoStim.Period := edPhotoStimPeriod.Value;

  // Save repeat period enabled
  MainFrm.PhotoStim.RepeatedStim := ckPhotoStimRepeat.Checked;

  // Save reference line enabled (line position saved when line is moved)
  MainFrm.PhotoStim.RefLineEnabled := ckReferenceLine.Checked;

  // Avoid list out of range error is no attenuators
  if cbAttenuator.Items.Count > 0 then begin
    MainFrm.PhotoStim.Attenuator :=
      Integer(cbAttenuator.Items.Objects[Max(cbAttenuator.ItemIndex, 0)]);
  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.UpdateTargetTable();
// -----------------------------------------
// Load local protocol copy into string grid
// -----------------------------------------
var
  r, c : Integer;
begin

  // Clear string grid
  for c := 1 to sgTargets.ColCount - 1 do
    sgTargets.Cols[c].Clear;

  // Load protocol into string grid
  for r := 0 to MainFrm.PhotoStim.NumStimPoints-1 do
  begin
    for c := 0 to 6 do
    begin
        if c = 0 then sgTargets.Cells[r+1, c] := Format('%d', [r]);
        if c = 1 then sgTargets.Cells[r+1, c] := Format('%.4g', [m_PRE[r]]);
        if c = 2 then sgTargets.Cells[r+1, c] := Format('%.4g', [m_D[r]]);
        if c = 3 then sgTargets.Cells[r+1, c] := Format('%.4g', [m_POST[r]]);
        if c = 4 then sgTargets.Cells[r+1, c] := Format('%.4g', [m_A[r]]);
        if c = 5 then sgTargets.Cells[r+1, c] := Format('%.4g', [m_X[r]]);
        if c = 6 then sgTargets.Cells[r+1, c] := Format('%.4g', [m_Y[r]]);
    end;
  end;

end;


// -----------------------------------------------------------------------------


function TPhotoStimFrm.ValidateNumericEntry(input: String;
                                            var output: Single) : Boolean;
// ---------------------------
// Validate string as a number
// ---------------------------
var
  i : Integer;
  dsep,NumAsString : String;
begin

  // Assign empty string
  NumAsString := '';

  // Check length
  if Length(input) < 1 then begin
    Result := False;
    Exit;
  end;

  // Only keep 0-9, +, -, ., ,
  for i := 1 to Length(input) do
  begin
    if input[i] in ['0'..'9', '+', '-', '.', ','] then begin
      NumAsString := NumAsString + input[i];
    end;
  end;

     { Correct for use of comma/period as decimal separator }
     {$IF CompilerVersion > 7.0} dsep := formatsettings.DECIMALSEPARATOR ;
     {$ELSE} dsep := DECIMALSEPARATOR ;
     {$IFEND}
     if dsep = '.' then NumAsString := ANSIReplaceText(NumAsString ,',',dsep);
     if dsep = ',' then NumAsString := ANSIReplaceText(NumAsString, '.',dsep);

  // Check length
  if Length(NumAsString) < 1 then begin
    Result := False;
    Exit;
  end;

  // Check for only +, -, .
  if Length(NumAsString) = 1 then
  begin
    if (NumAsString = '+') or (NumAsString = '-') or (NumAsString = '.') then
    begin
      Result := False;
      Exit;
    end;
  end;

  // Assign value
  output := StrToFloat(NumAsString);

  // Return success
  Result := True;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ValidateStimulusProtocol;
// -----------------------------------------
// Validate protocol entries for correctness
// -----------------------------------------
var
  c : Integer;
  r : Integer;
  cur : String;
  val : Single;
begin

  // Validate every entry in list. Restore old values if new values are invalid.
  for r := 0 to MainFrm.PhotoStim.NumStimPoints-1 do
  begin
    for c := 1 to 6 do
    begin
      cur := sgTargets.Cells[r+1, c];
      if ValidateNumericEntry(cur, val) then
      begin
        sgTargets.Cells[r+1, c] := Format('%.4g',[val]);
      end
      else
      begin
        if c = 1 then sgTargets.Cells[r+1, c] := Format('%.4g',[m_PRE[r]]);
        if c = 2 then sgTargets.Cells[r+1, c] := Format('%.4g',[m_D[r]]);
        if c = 3 then sgTargets.Cells[r+1, c] := Format('%.4g',[m_POST[r]]);
        if c = 4 then sgTargets.Cells[r+1, c] := Format('%.4g',[m_A[r]]);
        if c = 5 then sgTargets.Cells[r+1, c] := Format('%.4g',[m_X[r]]);
        if c = 6 then sgTargets.Cells[r+1, c] := Format('%.4g',[m_Y[r]]);
      end;
    end;
  end;

  // Validate protocol bounds, i.e. amplitudes and durations are within ranges.
  ValidateStimulusProtocolBounds();

  // Save to PhotoStimUnit
  for c := 1 to 6 do
  begin
    for r := 0 to MainFrm.PhotoStim.NumStimPoints-1 do
    begin
      if c = 1 then m_PRE[r] := StrToFloat(sgTargets.Cells[r+1, c]);
      if c = 2 then m_D[r] := StrToFloat(sgTargets.Cells[r+1, c]);
      if c = 3 then m_POST[r] := StrToFloat(sgTargets.Cells[r+1, c]);
      if c = 4 then m_A[r] := StrToFloat(sgTargets.Cells[r+1, c]);
      if c = 5 then m_X[r] := StrToFloat(sgTargets.Cells[r+1, c]);
      if c = 6 then m_Y[r] := StrToFloat(sgTargets.Cells[r+1, c]);
    end;
  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ValidateStimulusProtocolBounds();
// -----------------------------------------
// Validate protocol entries for correctness
// -----------------------------------------
var
  attenuator : Integer;
  c : Integer;
  cur : Single;
  i : Integer;
  r : Integer;
  maxPower : Single;        // Maximum power in mW
  minPower : Single;        // Minimum power in mW
  shutterLatency : Single;  // Shutter latency in ms
begin

  maxPower := 0.0;
  minPower := 0.0;
  shutterLatency := 0.0;

  // Selected attenuator
  attenuator := Integer(cbAttenuator.Items.Objects[cbAttenuator.ItemIndex]);

  // Get min and max powers                            
  if (attenuator = 1) or (attenuator = 2) or (attenuator = 3) then
  begin
    if MainFrm.PhotoStim.PCEnable[attenuator] then
    begin
      maxPower := MainFrm.PhotoStim.PCPowerMax[attenuator];
      minPower := MainFrm.PhotoStim.PCPowerMin[attenuator];
      if MainFrm.PhotoStim.EnableShutter[attenuator] then
      begin
        shutterLatency := MainFrm.IOConfig.PhotoStimShutterLatency * 1000.0;
      end
      else
      begin
        shutterLatency := 0.0;
      end;
    end
    else
    begin
      maxPower := MainFrm.PhotoStim.LinearPowerMax[attenuator];
      minPower := MainFrm.PhotoStim.LinearPowerMin[attenuator];
      if MainFrm.PhotoStim.EnableShutter[attenuator] then
      begin
        shutterLatency := MainFrm.IOConfig.PhotoStimShutterLatency * 1000.0;
      end
      else
      begin
        shutterLatency := 0.0;
      end;
    end;
  end;

  // Validate bounds of every entry in list
  for c := 0 to MainFrm.PhotoStim.NumStimPoints-1 do
  begin
    for r := 1 to 4 do
    begin
      cur := StrToFloat(sgTargets.Cells[c+1, r]);

      // Check PRE (pre-delay) for first photo-stimulus location
      if r = 1 then
      begin
        if (c = 0) and (cur < shutterLatency) then
        begin
          cur := shutterLatency;
        end
        else if cur < 0.0 then
        begin
          cur := 0.0;
        end;
      end

      // Check D (duration) / POST (post-delay)
      else if (r = 2) or (r = 3) then
      begin
        if cur < 0.0 then
        begin
          cur := 0.0;
        end;
      end

      // Check A (amplitude)
      else if r = 4 then
      begin
        if cur < minPower then
        begin
          cur := minPower;
        end;
        if cur > maxPower then
        begin
          cur := maxPower;
        end;
      end;

      // Write to string grid
      sgTargets.Cells[c+1, r] := Format('%.4g', [cur]);

    end;
  end;

end;


// -----------------------------------------------------------------------------


procedure TPhotoStimFrm.ZoomDisplay();
// ------------------
// Zoom display image
// ------------------
var
  XMult, YMult, nMult, nDiv : Integer;  // Zoom factors
  Xfrom, Yfrom, Xto, Yto : Integer;     // Buffer indices
  TempBitmap : TBitmap;                 // Temp TBitmap for resized/scaled image
  pTo : pByteArray;                     // Scan line in Temp bitmap
  BP : PByteArray;                      // Pointer to original image array
  ChannelIndex : Integer;               // Channel to zoom
  LBits, RBits : Integer;               // Left and right bits of 16-bit pixel
  p : Byte;                             // Pixel value as byte
  pixel : Integer;                      // Pixel value with low and high bits
  scale : Integer;                      // Pixel value [0 255]
  value : Single;                       // Normalized pixel value
begin

  // Determine zoom factor
  DisplayZoom :=
    Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex]) * 0.01;


  // PrairieView / Ultima LSM
  if MainFrm.CameraType = UltimaLSM then
  begin

    // Check if there are items in the combo box list
    if cbChannel.Items.Count <= 0 then Exit;

    // Check for out of bounds (-1) errors
    if cbChannel.ItemIndex < 0 then Exit;

    // Determine channel to display
    ChannelIndex := Integer(cbChannel.Items.Objects[cbChannel.ItemIndex]);

  end

  // Camera image
  else
  begin

    // Set channel
    ChannelIndex := 0;

  end;


  // Cast p_OrigFrame from Pointer to PByteArray
  BP := PByteArray(p_OrigFrame[ChannelIndex]);


  // Create temporary bitmap
  TempBitmap := TBitmap.Create;


  // Magnify display
  if DisplayZoom >= 1.0 then
  begin

    // Determine zoom factor
    nMult := Round(DisplayZoom);

    // Set properties for temporary bitmap
    TempBitmap.Width := m_OrigFrameWidth * nMult;
    TempBitmap.Height := m_OrigFrameHeight * nMult;
    TempBitmap.PixelFormat := pf24bit;

    // Copy pixels
    Yto := 0;
    for Yfrom := 0 to m_OrigFrameHeight - 1 do
    begin
      for YMult := 0 to nMult-1 do
      begin
        pTo := TempBitmap.ScanLine[Yto];
        Xto := 0;
        for XFrom := 0 to m_OrigFrameWidth - 1 do
        begin
          for Xmult := 0 to nMult - 1 do
          begin
            if (m_OrigFrameBPP = 2) then
            begin
              LBits := BP[(Yfrom * m_OrigFrameWidth * 2) + (XFrom * 2)];
              RBits := BP[(Yfrom * m_OrigFrameWidth * 2) + (XFrom * 2) + 1];
              RBits := RBits shl 8;
              pixel := LBits or RBits;
              value := (pixel - MainFrm.PhotoStim.GreyLo) /
                       (MainFrm.PhotoStim.GreyHi - MainFrm.PhotoStim.GreyLo);
              if value > 1.0 then scale := 255
              else if value < 0.0 then scale := 0
              else scale := Trunc(value * 255.0);
              p := Byte(scale);
            end
            else
            begin
              p := BP[(Yfrom * m_OrigFrameWidth) + XFrom];
            end;
            pTo[(Xto*3) + 0] := p; // B
            pTo[(Xto*3) + 1] := p; // G
            pTo[(Xto*3) + 2] := p; // R
            Inc(Xto);
          end;
        end;
        Inc(Yto);
      end;
    end;

  end

  // Reduce display
  else
  begin

    // Determine zoom factor
    nDiv := Round(1.0 / DisplayZoom) ;

    // Set properties for temporary bitmap
    TempBitmap.Width := m_OrigFrameWidth div nDiv;
    TempBitmap.Height := m_OrigFrameHeight div nDiv;
    TempBitmap.PixelFormat := pf24bit;

    // Copy pixels
    Yto := 0;
    for Yfrom := 0 to (m_OrigFrameHeight div nDiv) - 1 do
    begin
      pTo := TempBitmap.ScanLine[Yto];
      Xto := 0;
      for XFrom := 0 to (m_OrigFrameWidth div nDiv) - 1 do
      begin
        if (m_OrigFrameBPP = 2) then
        begin
          LBits := BP[((Yfrom*m_OrigFrameWidth*2)*nDiv) + (XFrom*nDiv*2)];
          RBits := BP[((Yfrom*m_OrigFrameWidth*2)*nDiv) + (XFrom*nDiv*2)+1];
          RBits := RBits shl 8;
          pixel := LBits or RBits;
          value := (pixel - MainFrm.PhotoStim.GreyLo) /
                   (MainFrm.PhotoStim.GreyHi - MainFrm.PhotoStim.GreyLo);
          if value > 1.0 then scale := 255
          else if value < 0.0 then scale := 0
          else scale := Trunc(value * 255.0);
          p := Byte(scale);
        end
        else
        begin
          p := BP[(Yfrom * m_OrigFrameWidth * nDiv) + (XFrom * nDiv)];
        end;
        pTo[(Xto * 3) + 0] := p; // B
        pTo[(Xto * 3) + 1] := p; // G
        pTo[(Xto * 3) + 2] := p; // R
        Inc(Xto);
      end;
      Inc(Yto);
    end;

  end;


  // Assign temp bitmap to DisplayPic
  DisplayPic.Assign(TempBitmap);

  // Set image
  Image.Picture.Assign(DisplayPic);

  // Free bitmap
  TempBitmap.Free;

end;


// -----------------------------------------------------------------------------


end.
