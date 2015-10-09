unit XYStageUnit;
// ----------------------
// XY Stage control panel
// ----------------------
// 04.02.15 Started
// 16.02.15 Supports Thorlabs MLS203 stage
//          TMG17Motor controls created dynamically to avoid OLESYSERROR on  systems without MLS203s
// 07.04.15 3 levels (coarse/medium/fine) of XY position control now available
//          XY Position table can be directly edited by user
// 30.06.15 Now reads actual stage position allowing positions set by joystick to be saved.
// 16.09.15 .. JD Form position/size saved by MainFrm.SaveFormPosition() when form closed

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.OleCtrls,
  MG17MotorLib_TLB, Vcl.ExtCtrls;

const
  MaxXYStage = 1 ;
  MaxXYStagePositions = 100 ;

type
  TXYStageFrm = class(TForm)
    ListGrp: TGroupBox;
    sgPositions: TStringGrid;
    bAddPosition: TButton;
    bMoveTo: TButton;
    cbPosition: TComboBox;
    DeletePosition: TButton;
    cbDeletePosition: TComboBox;
    CycleGrp: TGroupBox;
    ckIncrementStagePosition: TCheckBox;
    ButtonsGrp: TGroupBox;
    bMoveUp: TButton;
    bMoveLeft: TButton;
    bHome: TButton;
    bMoveRight: TButton;
    bMoveDown: TButton;
    GroupBox2: TGroupBox;
    rbCoarse: TRadioButton;
    rbFine: TRadioButton;
    StagePositionGrp: TGroupBox;
    lbXPos: TLabel;
    lbYPos: TLabel;
    edStagePosition: TEdit;
    Timer: TTimer;
    rbMedium: TRadioButton;

    procedure FormCreate(Sender: TObject);
    procedure bMoveLeftClick(Sender: TObject);
    procedure bMoveRightClick(Sender: TObject);
    procedure bMoveUpClick(Sender: TObject);
    procedure bMoveDownClick(Sender: TObject);
    procedure bMoveToClick(Sender: TObject);
    procedure bAddPositionClick(Sender: TObject);
    procedure bHomeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DeletePositionClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure XMG17MotorHomeComplete(ASender: TObject; lChanID: Integer);
    procedure YMG17MotorHomeComplete(ASender: TObject; lChanID: Integer);
    procedure XMG17MotorMoveComplete(ASender: TObject; lChanID: Integer);
    procedure YMG17MotorMoveComplete(ASender: TObject; lChanID: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    { Private declarations }
    FStageType : Integer ;
    FActive : Boolean ;             // TRUE = XY stage active
    XHomed : Boolean ;              // X home completed
    YHomed : Boolean ;              // Y home completed

    FXPos : Double ;
    FYPos : Double ;
    FXMin : Double ;              // Lower limit of X range (mm)
    FXMax : Double ;              // Upper limit of X range (mm)
    FYMin : Double ;              // Lower limit of Y range (mm)
    FYMax : Double ;              // Upper limit of Y range (mm)


    FNumPositions : Integer ;                        // No. of stored positions
    XPosition : Array[0..MaxXYStagePositions] of Double ;  // X Position list
    YPosition : Array[0..MaxXYStagePositions] of  Double ; // Y position list
    FPosition : Integer ;                           // Currently selected position from list

    XHomeAtTick : Integer ;
    YHomeAtTick : Integer ;
    CentreStageAtTick : Integer ;
    TickCount : Integer ;

    XMG17Motor : TMG17Motor;          // X MG17 ActiveX Motor control component
    YMG17Motor : TMG17Motor;          // Y MG17 ActiveX Motor control component

    procedure SetStageType( Value : Integer  ) ;
    procedure SetXPosition( Value : Double  ) ;
    function GetXPosition : Double ;
    procedure SetYPosition( Value : Double  ) ;
    function GetYPosition : Double ;
    procedure FillPositionTable ;
    procedure ReadPositionTable ;
    function GetIncrementStagePosition : Boolean ;
    procedure SetPosition( Value : Integer ) ;
    function GetPosition : Integer ;
    function GetAvailable : Boolean ;
    procedure OpenStage ;
    procedure CloseStage ;
    procedure EnableXMove( Enable : Boolean ) ;
    procedure EnableYMove( Enable : Boolean ) ;

  public
    { Public declarations }
    XMotorID : Integer ;
    YMotorID : Integer ;

    procedure GetList( List : TStrings ) ;
    procedure ReadSettings( var Header : Array of ANSIChar ) ;
    procedure SaveSettings( var Header : Array of ANSIChar ) ;


    property StageType : Integer read FStageType write SetStageType ;
    property XMin : Double Read FXMin write FXMin ;
    property XMax : Double Read FXMax write FXMax ;
    property YMin : Double Read FYMin write FYMin ;
    property YMax : Double Read FYMax write FYMax ;
    property XPos : Double read GetXPosition write SetXPosition ;
    property YPos : Double read GetYPosition write SetYPosition ;
    property IncrementStagePosition : Boolean read GetIncrementStagePosition ;
    property Position : Integer read GetPosition write SetPosition ;
    property NumPositions : Integer read FNumPositions ;
    property Available : Boolean read GetAvailable ;
  end;

var
  XYStageFrm: TXYStageFrm;

const
    xyNone = 0 ;
    xyThorlabsMLS203 = 1 ;

implementation

uses shared,math , MAIN;

{$R *.dfm}

procedure TXYStageFrm.FormClose(Sender: TObject; var Action: TCloseAction);
// --------------
// Form is closed
// --------------
begin
    ReadPositionTable ;

     // Save position/size of form within parent window
     MainFrm.SaveFormPosition( Self ) ;

    end;

procedure TXYStageFrm.FormCreate(Sender: TObject);
// --------------------------------------
// Initialisations when module is created
// --------------------------------------
var
    i : Integer ;
begin

    FStageType := xyNone ;
    FXPos := 0.0 ;
    FYPos := 0.0 ;
    FXMin := 0.0 ;
    FYMax := 110.0 ;
    FYMin := 0.0 ;
    FYMax := 75.0 ;
    FActive := False ;
    TickCount := 0 ;

    FNumPositions := 0 ;
    for i  := 0 to MaxXYStagePositions do begin
      XPosition[i] := 0.0 ;
      YPosition[i] := 0.0 ;
      end;

    XHomed := False ;
    YHomed := False ;

    end;


procedure TXYStageFrm.FormDestroy(Sender: TObject);
// ------------------------------
// Tidy up when form is destroyed
// ------------------------------
begin

    CloseStage ;
    end;


procedure TXYStageFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed.
// --------------------------------------
begin

    // Open communications with XY stage (if necessary)
    OpenStage ;

    cbPosition.Clear ;
    cbDeletePosition.Clear ;
    cbDeletePosition.Items.Add('All') ;
    cbDeletePosition.ItemIndex := 0 ;

    // Fill position table string grid
    FillPositionTable ;
    if FNumPositions > 0 then
       edStagePosition.Text := format('Stage Position = %d',[FPosition+1])
    else  edStagePosition.Text := '' ;

    end;


procedure TXYStageFrm.OpenStage ;
// --------------------------------------
// Establish communications with XY stage
// --------------------------------------
begin

    if FActive then Exit ;

    case FStageType of
      xyThorlabsMLS203 : begin

        XMG17motor := TMG17Motor.Create(Self);
        XMG17motor.Parent := Self ;
        XMG17motor.Height := 48 ;
        XMG17motor.HWSerialNum := XMotorID ;
        XMG17motor.DisplayMode := 3 ;
        XMG17motor.OnMoveComplete := XMG17MotorMoveComplete ;
        XMG17motor.OnHomeComplete := XMG17MotorHomeComplete ;
        XMG17motor.StartCtrl ;
        XMG17motor.EnableHWChannel(0);
        XMG17motor.Visible := True ;
        XMG17motor.Top := lbXPos.Top ;
        XMG17motor.Left := edStagePosition.Left + 10 ;
        XMG17motor.Width := edStagePosition.Width ;

        YMG17motor := TMG17Motor.Create(Self);
        YMG17motor.Parent := Self ;
        YMG17motor.Height := 48 ;
        YMG17motor.HWSerialNum := YMotorID ;
        YMG17motor.DisplayMode := 3 ;
        YMG17motor.OnMoveComplete := YMG17MotorMoveComplete ;
        YMG17motor.OnHomeComplete := YMG17MotorHomeComplete ;
        YMG17motor.StartCtrl ;
        YMG17motor.EnableHWChannel(0);
        YMG17motor.Visible := True ;
        YMG17motor.Top := lbYPos.Top ;
        YMG17motor.Left := edStagePosition.Left + 10 ;
        YMG17motor.Width := edStagePosition.Width ;

        XHomeAtTick := TickCount + 30 ;
        YHomeAtTick := High(YHomeAtTick) ;
        CentreStageAtTick := High(CentreStageAtTick) ;
        XHomed := False ;
        YHomed := False ;
        EnableXMove( False ) ;
        EnableYMove( False ) ;
        Timer.Enabled := True ;
        end;
       end;

    FActive := True ;

    end;


procedure TXYStageFrm.CloseStage ;
// --------------------------------------
// Close communications with XY stage
// --------------------------------------
begin
    if not FActive then Exit ;
    case FStageType of
      xyThorlabsMLS203 : begin
        XMG17motor.StopCtrl ;
        YMG17motor.StopCtrl ;
        end ;
      end;
    FActive := False ;
    end;


procedure TXYStageFrm.FillPositionTable ;
// ------------------------------
// Fill table of stored positions
// ------------------------------
var
  i: Integer;
begin

     sgPositions.RowCount := 1 ;

     sgPositions.ColWidths[0] :=  sgPositions.Canvas.TextWidth('XXX') ;
     sgPositions.ColWidths[1] :=  sgPositions.Canvas.TextWidth('XXXXXXXXX') ;
     sgPositions.ColWidths[2] :=  sgPositions.Canvas.TextWidth('XXXXXXXXX') ;

     sgPositions.RowCount := 1 ;
     sgPositions.Cells[0,0] := '#' ;
     sgPositions.Cells[1,0] := 'X (mm)' ;
     sgPositions.Cells[2,0] := 'Y (mm)' ;

     cbPosition.Clear ;
     cbDeletePosition.Clear ;
     cbDeletePosition.Items.Add('All') ;

     for i := 0 to FNumPositions-1 do begin
        sgPositions.RowCount := sgPositions.RowCount + 1 ;
        sgPositions.Cells[0,sgPositions.RowCount-1] := format('%d',[sgPositions.RowCount-1]);
        sgPositions.Cells[1,sgPositions.RowCount-1] := format('%.3f',[XPosition[i]]);
        sgPositions.Cells[2,sgPositions.RowCount-1] := format('%.3f',[YPosition[i]]);
        cbPosition.Items.Add(format('%d',[i+1])) ;
        cbDeletePosition.Items.Add(format('%d',[i+1])) ;
        end ;

     cbDeletePosition.ItemIndex := 0 ;
     cbPosition.ItemIndex := FPosition ;

     end;


procedure TXYStageFrm.ReadPositionTable ;
// ------------------------------
// Read table of stored positions
// ------------------------------
var
  i,ichar: Integer;
begin

     FNumPositions := 0 ;
     for i := 1 to Min(sgPositions.RowCount-1,MaxXYStagePositions) do begin
        Val( sgPositions.Cells[1,i],XPosition[i-1],iChar) ;
        Val( sgPositions.Cells[2,i],YPosition[i-1],iChar) ;
        Inc(FNumPositions) ;
        end ;

     end;


procedure TXYStageFrm.bAddPositionClick(Sender: TObject);
// -------------------------------------
// Add current XY stage position to list
// -------------------------------------
var
    X,Y : single ;
begin

    // Open communications with XY stage (if necessary)
    OpenStage ;

    if FNumPositions >= MaxXYStagePositions then Exit ;

    case FStageType of
      xyThorlabsMLS203 : begin
        XMG17motor.GetPosition(0,X) ;
        YMG17motor.GetPosition(0,Y) ;
        end;
      end;

    XPosition[FNumPositions] := X ;
    YPosition[FNumPositions] := Y ;
    Inc(FNumPositions) ;

    FillPositionTable ;

    end;


procedure TXYStageFrm.bHomeClick(Sender: TObject);
// ---------------------
// Move to home position
// ---------------------
begin
    exit ;
    XHomeAtTick := TickCount + 1 ;
    YHomeAtTick := High(Integer) ;
    CentreStageAtTick := High(Integer) ;
    XHomed := False ;
    YHomed := False ;
    EnableXMove( False ) ;
    EnableYMove( False ) ;
    Timer.Enabled := True ;
    end;


procedure TXYStageFrm.bMoveDownClick(Sender: TObject);
// -----------------
// Move XY stage down
// -----------------
var
    Pos,Step : single ;
begin

    if rbCoarse.Checked then Step := 1.0
    else if rbMedium.Checked then Step := 0.1
    else Step := 0.01 ;

    case FStageType of
      xyThorlabsMLS203 : begin
        YMG17motor.GetPosition(0,Pos) ;
        Pos := Min(Max(Pos + Step,FYMin),FYMax) ;
        YMG17motor.SetAbsMovePos(0,Pos) ;
        YMG17motor.MoveAbsolute(0,false);
        EnableYMove( False ) ;
        end;
      end;
    end;


procedure TXYStageFrm.bMoveLeftClick(Sender: TObject);
// ---------------
// Move stage left
// ---------------
var
    Pos,Step : single ;
begin

    if rbCoarse.Checked then Step := 1.0
    else if rbMedium.Checked then Step := 0.1
    else Step := 0.01 ;

    case FStageType of
      xyThorlabsMLS203 : begin
        XMG17motor.GetPosition(0,Pos) ;
        Pos := Min(Max(Pos - Step,FXMin),FXMax) ;
        XMG17motor.SetAbsMovePos(0,Pos) ;
        XMG17motor.MoveAbsolute(0,false);
        EnableXMove( False ) ;
        end;
      end;
    end;


procedure TXYStageFrm.bMoveRightClick(Sender: TObject);
// ---------------
// Move stage right
// ---------------
var
    Pos,Step : single ;
begin

    if rbCoarse.Checked then Step := 1.0
    else if rbMedium.Checked then Step := 0.1
    else Step := 0.01 ;

    case FStageType of
      xyThorlabsMLS203 : begin
        XMG17motor.GetPosition(0,Pos) ;
        Pos := Min(Max(Pos + Step,FXMin),FXMax) ;
        XMG17motor.SetAbsMovePos(0,Pos) ;
        XMG17motor.MoveAbsolute(0,false);
        EnableXMove( False ) ;
        end;
      end;
    end;

procedure TXYStageFrm.EnableXMove( Enable : Boolean ) ;
// -------------------------
// Enable/disable X movement
// -------------------------
begin
      bMoveRight.Enabled := Enable ;
      bMoveLeft.Enabled := Enable ;
      ListGrp.Enabled := Enable ;
      end;


procedure TXYStageFrm.EnableYMove( Enable : Boolean ) ;
// -------------------------
// Enable/disable Y movement
// -------------------------
begin
      bMoveUp.Enabled := Enable ;
      bMoveDown.Enabled := Enable ;
      ListGrp.Enabled := Enable ;
      end;


procedure TXYStageFrm.bMoveToClick(Sender: TObject);
// ----------------------
// Move to saved position
// ----------------------
begin

    if cbPosition.Items.Count > 0 then begin
       SetPosition( cbPosition.ItemIndex ) ;
       end;

    end;


procedure TXYStageFrm.bMoveUpClick(Sender: TObject);
// -----------------
// Move XY stage up
// -----------------
var
    Pos,Step : single ;
begin

    if rbCoarse.Checked then Step := 1.0
    else if rbMedium.Checked then Step := 0.1
    else Step := 0.01 ;

    case FStageType of
      xyThorlabsMLS203 : begin
        YMG17motor.GetPosition(0,Pos) ;
        Pos := Min(Max(Pos - Step,FYMin),FYMax) ;
        YMG17motor.SetAbsMovePos(0,Pos) ;
        YMG17motor.MoveAbsolute(0,false);
        EnableYMove( False ) ;
        end;
      end;
    end;


procedure TXYStageFrm.DeletePositionClick(Sender: TObject);
// -----------------------------
// Delete position(s) from table
// -----------------------------
var
  i,j: Integer;
begin

    if cbDeletePosition.ItemIndex = 0 then begin
       // Delete all
       FNumPositions := 0 ;
       end
    else begin
       // Delete selected position
       for i := 0 to FNumPositions-1 do begin
           if i <= (cbDeletePosition.ItemIndex-1) then j := i
                                                  else j := i - 1 ;
           XPosition[j] := XPosition[i] ;
           YPosition[j] := YPosition[i] ;
           end ;
       FNumPositions := Max(FNumPositions - 1,0);
       end;

    FillPositionTable ;

    end;


procedure TXYStageFrm.GetList( List : TStrings ) ;
// -----------------------------------
// Get list of supported light sources
// -----------------------------------
begin

     List.Clear ;
     List.AddObject('None',TObject(xyNone)) ;
     List.AddObject('Thorlabs MLS203',TObject(xyThorlabsMLS203)) ;

     end ;


procedure TXYStageFrm.SetStageType( Value : Integer  ) ;
// ---------------------------
// Set type of XY stage in use
// ---------------------------
begin

    if (FStageType <> Value) then begin
        if FActive then CloseStage ;
        FStageType := Value ;
        FActive := False ;
        end;

    end;


procedure TXYStageFrm.SetXPosition( Value : Double ) ;
// -------------------------
// Set stage X axis position
// -------------------------
var
    Pos : single ;
begin
    // Open communications with XY stage (if necessary)
    OpenStage ;

    Pos := Min(Max(Value,FXMin),FXMax) ;

    case FStageType of
      xyThorlabsMLS203 : begin
        XMG17motor.SetAbsMovePos(0,Pos) ;
        XMG17motor.MoveAbsolute(0,false);
        end;
      end;

end;

function TXYStageFrm.GetXPosition : Double ;
// -------------------------
// Return stage X axis position
// -------------------------
var
    Pos : single ;
begin
    // Open communications with XY stage (if necessary)
    OpenStage ;

    XMG17motor.GetPosition(0,Pos) ;
    Result := Pos ;
    end;


procedure TXYStageFrm.SetYPosition( Value : Double ) ;
// -------------------------
// Set stage Y axis position
// -------------------------
var
    Pos : single ;
begin
    // Open communications with XY stage (if necessary)
    OpenStage ;

    Pos := Min(Max(Value,FYMin),FYMax) ;

    case FStageType of
      xyThorlabsMLS203 : begin
        YMG17motor.SetAbsMovePos(0,Pos) ;
        YMG17motor.MoveAbsolute(0,false);
        end;
      end;
    end ;


procedure TXYStageFrm.TimerTimer(Sender: TObject);
// ---------------------
// Scheduled event timer
// ---------------------
begin
    Inc(TickCount);
    if TickCount = XHomeAtTick then begin
       XHomed := False ;
       XMG17motor.MoveHome(0,False) ;
       end
    else if TickCount = YHomeAtTick then begin
       YHomed := False ;
       YMG17motor.MoveHome(0,False) ;
       end
     else if (TickCount = CentreStageAtTick) and XHomed and YHomed then begin
       XMG17motor.SetAbsMovePos(0,(FXMax + FXMin)*0.5) ;
       XMG17motor.MoveAbsolute(0,False) ;
       YMG17motor.SetAbsMovePos(0,(FYMax + FYMin)*0.5) ;
       YMG17motor.MoveAbsolute(0,False) ;
       EnableXMove(True) ;
       EnableYMove(True) ;
       Timer.Enabled := False  ;
       end ;

    end;


procedure TXYStageFrm.XMG17MotorHomeComplete(ASender: TObject;
  lChanID: Integer);
// ----------------------
// X motor home completed
// ----------------------
begin
    XHomed := True ;
    YHomeAtTick := TickCount + 1 ;
    outputdebugstring(pchar('X home complete'));
    end;

procedure TXYStageFrm.XMG17MotorMoveComplete(ASender: TObject;
  lChanID: Integer);
// ----------------------
// X motor move completed
// ----------------------
begin
    EnableXMove( TRue ) ;
    end;

procedure TXYStageFrm.YMG17MotorHomeComplete(ASender: TObject;
  lChanID: Integer);
// ----------------------
// Y motor home completed
// ----------------------
begin
    YHomed := True ;
    CentreStageAtTick := TickCount + 1 ;
    outputdebugstring(pchar('Y home complete'));
    end;

procedure TXYStageFrm.YMG17MotorMoveComplete(ASender: TObject;
  lChanID: Integer);
// ----------------------
// Y motor move completed
// ----------------------
begin
    EnableYMove( TRue ) ;
    end;


function TXYStageFrm.GetYPosition : Double ;
// -------------------------
// Return stage Y axis position
// -------------------------
var
    Pos : single ;
begin
    // Open communications with XY stage (if necessary)
    OpenStage ;

    YMG17motor.GetPosition(0,Pos) ;
    Result := Pos ;
    end;


procedure TXYStageFrm.SetPosition( Value : Integer ) ;
// ------------------------------------------
// Move to position # in stored position list
// ------------------------------------------
var
    NPos,ichar : Integer ;
    XPos,YPos : Single ;
begin
    // Open communications with XY stage (if necessary)
    OpenStage ;

    // Get data from table
    ReadPositionTable ;

    NPos := sgPositions.RowCount -1 ;
    if NPos < 1 then begin
       FPosition := 0 ;
       edStagePosition.Text := '' ;
       Exit ;
       end;
    FPosition := Value mod NPos ;
    edStagePosition.Text := format('Stage Position = %d',[FPosition+1]);

    Val( sgPositions.Cells[1,FPosition+1],XPos,iChar) ;
    Val( sgPositions.Cells[2,FPosition+1],YPos,iChar) ;
    cbPosition.ItemIndex := FPosition ;

    case FStageType of
      xyThorlabsMLS203 : begin
        XMG17motor.SetAbsMovePos(0,XPos) ;
        XMG17motor.MoveAbsolute(0,false);
        YMG17motor.SetAbsMovePos(0,YPos) ;
        YMG17motor.MoveAbsolute(0,false);
        end ;
      end;

    end ;

function TXYStageFrm.GetPosition : Integer ;
// --------------------------------------
// Return current selected stage position
// --------------------------------------
begin
      Result := FPosition ;
      end;


function TXYStageFrm.GetAvailable : Boolean ;
// -------------------------------
// Return TRUE if stage available
// -------------------------------
begin
    if FStageType <> xyNone then Result := True
                            else Result := False ;
    end;


procedure TXYStageFrm.ReadSettings(
          var Header : Array of ANSIChar
          ) ;
// -----------------------------------
// Read XY stage settings from INI text
// -----------------------------------
var
    i : Integer ;
    bValue : Boolean ;
begin
      // Settings
      ReadInt( Header, 'XYSType=', FStageType ) ;

      ReadDouble( Header, 'XYSXMin=', FXMin ) ;
      ReadDouble( Header, 'XYSXMax=', FXMax ) ;
      ReadDouble( Header, 'XYSYMin=', FYMin ) ;
      ReadDouble( Header, 'XYSYMaz=', FYMax ) ;
      ReadInt( Header, 'XYXMSN=', XMotorID ) ;
      ReadInt( Header, 'XYYMSN=', YMotorID ) ;

      ReadPositionTable ;
      ReadInt( Header, 'XYNPOS=', FNumPositions ) ;
      for i := 0 to FNumPositions-1 do begin
        ReadDouble( Header, format('XYXP%d=',[i]), XPosition[i] ) ;
        ReadDouble( Header, format('XYYP%d=',[i]), YPosition[i] ) ;
        end;
      ReadInt( Header, 'XYPOS=',FPosition) ;

      bValue := False ;
      ReadLogical( Header, 'XYISP=', bValue ) ;
      ckIncrementStagePosition.Checked := bValue ;

      end ;


procedure TXYStageFrm.SaveSettings(
          var Header : Array of ANSIChar
          ) ;
// -----------------------------------
// Read XY stage settings from INI text
// -----------------------------------
var
    i : Integer ;
begin
      // Settings
      AppendInt( Header, 'XYSType=', FStageType ) ;

      AppendDouble( Header, 'XYSXMin=', FXMin ) ;
      AppendDouble( Header, 'XYSXMax=', FXMax ) ;
      AppendDouble( Header, 'XYSYMin=', FYMin ) ;
      AppendDouble( Header, 'XYSYMaz=', FYMax ) ;
      AppendInt( Header, 'XYXMSN=', XMotorID ) ;
      AppendInt( Header, 'XYYMSN=', YMotorID ) ;

      AppendInt( Header, 'XYNPOS=', FNumPositions ) ;
      for i := 0 to FNumPositions-1 do begin
        AppendDouble( Header, format('XYXP%d=',[i]), XPosition[i] ) ;
        AppendDouble( Header, format('XYYP%d=',[i]), YPosition[i] ) ;
        end;
      AppendInt( Header, 'XYPOS=',FPosition) ;

      AppendLogical( Header, 'XYISP=', ckIncrementStagePosition.Checked ) ;

      end ;

function TXYStageFrm.GetIncrementStagePosition : Boolean ;
// ---------------------------------------
// Return increment stage position setting
// ---------------------------------------
begin
    Result := ckIncrementStagePosition.Checked ;
    end;


end.
