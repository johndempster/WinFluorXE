unit ZStageUnit;
// ------------------------------
// Z stage position/focus control
// ------------------------------
// 25.01.13 Started
// 25.10.13 .UpdateDACBuffer() modified. Z stage now moved after end wavelength sequence
//          StepTime added ... Max. Time taken to move to Z position.

interface

uses
  SysUtils, Classes, math, LabIOUnit, Windows ;

type
  TZStage = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FPosition : double ;
    procedure SetPosition( Value : double ) ;
    function GetMinPosition : Double ;
    function GetMaxPosition : Double ;
    function GetShortenExcitationBy : Double ;
    function GetShortenExposureBy : Double ;
  public
    { Public declarations }
      Available : Boolean ;       // TRUE = Z stage hardware available
      CalPosition1 : double ;     // Calibration point 1 position (um)
      CalVoltage1 : double ;      // Calibration point 1 voltage (V)
      CalPosition2 : double ;     // Calibration point 2 position (um)
      CalVoltage2 : double ;      // Calibration point 2 voltage (V)
      VMin : double ;             // Lower limit of Z control voltage range (V)
      VMax : double ;             // Upper limit of Z control voltage range (V)
      MinStepSize : double ;      // Minimum Z position step size (um)
      StackEnabled : Boolean ;    // TRUE = Z stack enabled
      StartAt : double ;          // Starting position of Z stack (um)
      StepSize : double ;         // Z stack step size (um)
      NumSteps : Integer ;        // No. of steps in Z stack
      StepTime : double ;         // Maximum time to move to Z position (s).
      ExcitationOffDuringStep : Boolean ; // Turn excitation light off during step
      EndExposureAtStep : Boolean ;       // End camera exposure when step starts

     procedure ReadSettings(
               var Header : Array of ANSIChar
               ) ;
     procedure SaveSettings(
               var Header : Array of ANSIChar
               ) ;

      property Position : Double read FPosition write SetPosition ;

      procedure UpdateDACBuffer(
          DoZStack : Boolean ;
          NewPosition : Double ;
          NumFramesInBuffer : Integer ;
          NumFramesPerZStep : Integer ;
          NumFramesPerWavelengthCycle : Integer ;
          NumDACPointsPerFrame : Integer ;
          DACUpdateInterval : Single ;
          var DACBufs : Array of PBig16bitArray
          ) ;

      property MinPosition : Double read GetMinPosition ;
      property MaxPosition : Double read GetMaxPosition ;
      property ShortenExcitationBy : Double read GetShortenExcitationBy ;
      property ShortenExposureBy : Double read GetShortenExposureBy ;

  end;

var
  ZStage: TZStage;

implementation

uses Main, shared ;

{$R *.dfm}

procedure TZStage.DataModuleCreate(Sender: TObject);
//  -----------------------------------
//  Initialisations when module created
// ------------------------------------
begin

      Available := False ;
      CalPosition1 := 0.0 ;
      CalVoltage1 := 0.0 ;
      CalPosition2 := 10.0 ;
      CalVoltage2 := 10.0 ;
      VMin := -10.0 ;
      VMax := 10.0 ;
      MinStepSize := 0.05 ;
      StackEnabled := False ;
      StartAt := 0.0 ;
      StepSize := 1.0 ;
      NumSteps := 10 ;
      Position := 0.0 ;
      StepTime := 0.0 ;
      ExcitationOffDuringStep := False ;
      EndExposureAtStep := False ;

      end;

procedure TZStage.SetPosition( Value : double ) ;
// ------------------------------------
// Move focus to to new Z position (um)
// ------------------------------------
var
    V,VScale : double ;
    Dev,DACChannel : Integer ;
begin

    if not Available then Exit ;
    if CalPosition2 = CalPosition1 then Exit ;
    if CalVoltage2 = CalVoltage1 then Exit ;

    // Exit if no Z stage control channel configured
    if (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.ZStageControl)) then Exit ;
    Dev := LabIO.Resource[MainFrm.IOConfig.ZStageControl].Device ;
    if Dev <= 0 then Exit ;

    DACChannel := LabIO.Resource[MainFrm.IOConfig.ZStageControl].StartChannel ;
    VScale := (CalVoltage2 - CalVoltage1)/(CalPosition2 - CalPosition1) ;

    // Keep as multiple of step size
    Value := Round(Value/MinStepSize)*MinStepSize ;

    // Update piezo control voltage
    V := ((Value - CalPosition1)*VScale) + CalVoltage1 ;
    V := Max(Min(V,VMax),VMin) ;
    LabIO.WriteDAC( Dev, V, DACChannel ) ;

    // Set default Z stage DAC value
    LabIO.DACOutState[Dev][DACChannel] := V ;

    FPosition := ((V - CalVoltage1)/VScale) + CalPosition1 ;

    end ;

procedure TZStage.UpdateDACBuffer(
          DoZStack : Boolean ;
          NewPosition : Double ;
          NumFramesInBuffer : Integer ;
          NumFramesPerZStep : Integer ;
          NumFramesPerWavelengthCycle : Integer ;
          NumDACPointsPerFrame : Integer ;
          DACUpdateInterval : Single ;
          var DACBufs : Array of PBig16bitArray
          ) ;
// ------------------------------------------------
// Create D/A buffer waveform for Z control channel
// ------------------------------------------------
var
    VScale,VStart,VStep : double ;
    DACValue,iStep,iFrame,i,j,Dev,DACChannel,NumDACChannels,NumPrePoints : Integer ;
begin

    if not Available then Exit ;
    if CalPosition2 = CalPosition1 then Exit ;
    if CalVoltage2 = CalVoltage1 then Exit ;

    // Exit if no Z stage control channel configured
    if (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.ZStageControl)) then Exit ;
    Dev := LabIO.Resource[MainFrm.IOConfig.ZStageControl].Device ;
    if Dev <= 0 then Exit ;

    NumDACChannels := LabIO.NumDACs[Dev] ;
    DACChannel := LabIO.Resource[MainFrm.IOConfig.ZStageControl].StartChannel ;
    VScale := (CalVoltage2 - CalVoltage1)/(CalPosition2 - CalPosition1) ;

    if StackEnabled and DoZStack then begin
       VStart := ((StartAt - CalPosition1)*VScale) + CalVoltage1 ;
       VStep :=  StepSize*VScale ;
       end
    else begin
       FPosition := Round(NewPosition/MinStepSize)*MinStepSize ;
       VStart := ((FPosition - CalPosition1)*VScale) + CalVoltage1 ;
       VStep :=  0.0 ;
       end ;


    // For continuous recording, initiate Z change StepTime (s) before start of frame
    // For time lapse record Z change occurs after last frame in wavelength sequence
    // Note. StepTime limited to less than half of frame interval
    if NumFramesPerZStep = NumFramesPerWavelengthCycle then begin
       NumPrePoints := Min(Round(StepTime/DACUpdateInterval),NumDACPointsPerFrame div 2) ;
       end
    else NumPrePoints := 0 ;

    // Update Z stage channel with new position after end of wavelength cycle
    iFrame := 0 ;
    iStep := 0 ;
    NumFramesPerZStep := Max(NumFramesPerZStep,1) ;
    while iFrame < NumFramesInBuffer do begin

       // Set Z level for this frame
       DACValue := Round((VStart+(VStep*iStep))*LabIO.DACScale[Dev]) ;

       // Set Z level for frame
       j := (iFrame*NumDACPointsPerFrame*NumDACChannels) + DACChannel ;
       for i := 1 to NumDACPointsPerFrame do begin
           DACBufs[Dev]^[j] := DACValue ;
           j := j + NumDACChannels ;
           end ;

       // Initiate Z step StepTime seconds before start of frame
       j := (((iFrame*NumDACPointsPerFrame)-1)*NumDACChannels) + DACChannel ;
       if j < 0 then j := j + (NumDACPointsPerFrame*NumFramesInBuffer*NumDACChannels) ;
       for i := 1 to NumPrePoints do begin
           DACBufs[Dev]^[j] := DACValue ;
           j := j - NumDACChannels ;
           end ;

       // Move to next Z level after last frame in wavelength sequence
       if (iFrame mod NumFramesPerZStep) = (NumFramesPerWavelengthCycle-1) then Inc(iStep) ;
       if iStep >= NumSteps then iStep := 0 ;

       // Next frame
       Inc(iFrame) ;

       end ;

    // Set default Z stage value
    LabIO.DACOutState[Dev][DACChannel] := ((FPosition - CalPosition1)*VScale) + CalVoltage1 ;

    end ;


function TZStage.GetMinPosition : double ;
// ------------------------------------
// Return minimum Z stage position (um)
// ------------------------------------
var
    PScale : double ;
begin
    Result := 0.0 ;
    if CalVoltage2 = CalVoltage1 then Exit ;
    PScale := (CalPosition2 - CalPosition1) / (CalVoltage2 - CalVoltage1) ;
    Result := CalPosition1 + (VMin - CalVoltage1)*PScale ;
    Result := Round(Result/MinStepSize)* MinStepSize ;
    end ;


function TZStage.GetMaxPosition : double ;
// ------------------------------------
// Return maximum Z stage position (um)
// ------------------------------------
var
    PScale : double ;
begin
    Result := 0.0 ;
    if CalVoltage2 = CalVoltage1 then Exit ;
    PScale := (CalPosition2 - CalPosition1) / (CalVoltage2 - CalVoltage1) ;
    Result := CalPosition1 + (VMax - CalVoltage1)*PScale ;
    Result := Round(Result/MinStepSize)* MinStepSize ;
    end ;

function TZStage.GetShortenExcitationBy : double ;
// --------------------------------------------------------------------------
// Return time period to turn excitation light off at end of exposure
// --------------------------------------------------------------------------
begin
    if ExcitationOffDuringStep then Result := StepTime
                               else Result := 0.0 ;
    end;

function TZStage.GetShortenExposureBy : double ;
// -----------------------------------------------
// Return time period to shorten exposure time by
// -----------------------------------------------
begin
    if EndExposureAtStep then Result := StepTime
                         else Result := 0.0 ;
    end;


procedure TZStage.ReadSettings(
          var Header : Array of ANSIChar
          ) ;
// -----------------------------------
// Read Z stage settings from INI text
// -----------------------------------
begin
      // Settings
      ReadLogical( Header, 'ZSAVAIL=', Available ) ;
      ReadDouble( Header, 'ZSCALP1=', CalPosition1 ) ;
      ReadDouble( Header, 'ZSCALV1=', CalVoltage1 ) ;
      ReadDouble( Header, 'ZSCALP2=', CalPosition2 ) ;
      ReadDouble( Header, 'ZSCALV2=', CalVoltage2 ) ;
      ReadDouble( Header, 'ZSVMIN=', VMin ) ;
      ReadDouble( Header, 'ZSVMAX=', VMax ) ;
      ReadDouble( Header, 'ZSMINST=', MinStepSize ) ;
      ReadLogical( Header, 'ZSSTEN=', StackEnabled ) ;
      ReadDouble( Header, 'ZSTEP=', StepSize ) ;
      ReadDouble( Header, 'ZSPOS=', FPosition ) ;
      ReadDouble( Header, 'ZSTIME=', StepTime ) ;
      ReadDouble( Header, 'ZSTART=', StartAt ) ;
      ReadInt( Header, 'ZSNSTEPS=', NumSteps ) ;
      ReadLogical( Header, 'ZSEXOFF=', ExcitationOffDuringStep ) ;
      ReadLogical( Header, 'ZSENDEX=', EndExposureAtStep ) ;

      // Z stage control line
      ReadInt( Header, 'IOZSTAGE=', MainFrm.IOConfig.ZStageControl ) ;

      end ;

procedure TZStage.SaveSettings(
          var Header : Array of ANSIChar
          ) ;
// -----------------------------------
// Read Z stage settings from INI text
// -----------------------------------
begin
      // Settings
      AppendLogical( Header, 'ZSAVAIL=', Available ) ;
      AppendDouble( Header, 'ZSCALP1=', CalPosition1 ) ;
      AppendDouble( Header, 'ZSCALV1=', CalVoltage1 ) ;
      AppendDouble( Header, 'ZSCALP2=', CalPosition2 ) ;
      AppendDouble( Header, 'ZSCALV2=', CalVoltage2 ) ;
      AppendDouble( Header, 'ZSVMIN=', VMin ) ;
      AppendDouble( Header, 'ZSVMAX=', VMax ) ;
      AppendDouble( Header, 'ZSMINST=', MinStepSize ) ;
      AppendLogical( Header, 'ZSSTEN=', StackEnabled ) ;
      AppendDouble( Header, 'ZSTEP=', StepSize ) ;
      AppendDouble( Header, 'ZSPOS=', FPosition ) ;
      AppendDouble( Header, 'ZSTIME=', StepTime ) ;
      AppendDouble( Header, 'ZSTART=', StartAt ) ;
      AppendInt( Header, 'ZSNSTEPS=', NumSteps ) ;
      AppendLogical( Header, 'ZSEXOFF=', ExcitationOffDuringStep ) ;
      AppendLogical( Header, 'ZSENDEX=', EndExposureAtStep ) ;

      // Z stage control line
      AppendInt( Header, 'IOZSTAGE=', MainFrm.IOConfig.ZStageControl ) ;

      end ;


end.
