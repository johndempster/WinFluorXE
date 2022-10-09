unit ZStageUnit;
// ------------------------------
// Z stage position/focus control
// ------------------------------
// 25.01.13 Started
// 25.10.13 .UpdateDACBuffer() modified. Z stage now moved after end wavelength sequence
//          StepTime added ... Max. Time taken to move to Z position.
// 30.08.22 ReadSettings() SaveSettings() now has TStringList argument

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
               Header : TStringList
               ) ;
     procedure SaveSettings(
               Header : TStringList
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

uses Main, FileIOUnit ;

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
          Header : TStringList
          ) ;
// -----------------------------------
// Read Z stage settings from INI text
// -----------------------------------
begin
      // Settings
      Available := FileIO.GetKeyValue( Header, 'ZSAVAIL', Available ) ;
      CalPosition1 := FileIO.GetKeyValue( Header, 'ZSCALP1', CalPosition1 ) ;
      CalVoltage1 := FileIO.GetKeyValue( Header, 'ZSCALV1', CalVoltage1 ) ;
      CalPosition2 := FileIO.GetKeyValue( Header, 'ZSCALP2', CalPosition2 ) ;
      CalVoltage2 := FileIO.GetKeyValue( Header, 'ZSCALV2', CalVoltage2 ) ;
      VMin := FileIO.GetKeyValue( Header, 'ZSVMIN', VMin ) ;
      VMax := FileIO.GetKeyValue( Header, 'ZSVMAX', VMax ) ;
      MinStepSize := FileIO.GetKeyValue( Header, 'ZSMINST', MinStepSize ) ;
      StackEnabled := FileIO.GetKeyValue( Header, 'ZSSTEN', StackEnabled ) ;
      StepSize := FileIO.GetKeyValue( Header, 'ZSTEP', StepSize ) ;
      FPosition := FileIO.GetKeyValue( Header, 'ZSPOS', FPosition ) ;
      StepTime := FileIO.GetKeyValue( Header, 'ZSTIME', StepTime ) ;
      StartAt := FileIO.GetKeyValue( Header, 'ZSTART', StartAt ) ;
      NumSteps := FileIO.GetKeyValue( Header, 'ZSNSTEPS', NumSteps ) ;
      ExcitationOffDuringStep := FileIO.GetKeyValue( Header, 'ZSEXOFF', ExcitationOffDuringStep ) ;
      EndExposureAtStep := FileIO.GetKeyValue( Header, 'ZSENDEX', EndExposureAtStep ) ;

      // Z stage control line
      MainFrm.IOConfig.ZStageControl := FileIO.GetKeyValue( Header, 'IOZSTAGE', MainFrm.IOConfig.ZStageControl ) ;

      end ;

procedure TZStage.SaveSettings(
          Header : TStringList
          ) ;
// -----------------------------------
// Read Z stage settings from INI text
// -----------------------------------
begin
      // Settings
      FileIO.AddKeyValue( Header, 'ZSAVAIL', Available ) ;
      FileIO.AddKeyValue( Header, 'ZSCALP1', CalPosition1 ) ;
      FileIO.AddKeyValue( Header, 'ZSCALV1', CalVoltage1 ) ;
      FileIO.AddKeyValue( Header, 'ZSCALP2', CalPosition2 ) ;
      FileIO.AddKeyValue( Header, 'ZSCALV2', CalVoltage2 ) ;
      FileIO.AddKeyValue( Header, 'ZSVMIN', VMin ) ;
      FileIO.AddKeyValue( Header, 'ZSVMAX', VMax ) ;
      FileIO.AddKeyValue( Header, 'ZSMINST', MinStepSize ) ;
      FileIO.AddKeyValue( Header, 'ZSSTEN', StackEnabled ) ;
      FileIO.AddKeyValue( Header, 'ZSTEP', StepSize ) ;
      FileIO.AddKeyValue( Header, 'ZSPOS', FPosition ) ;
      FileIO.AddKeyValue( Header, 'ZSTIME', StepTime ) ;
      FileIO.AddKeyValue( Header, 'ZSTART', StartAt ) ;
      FileIO.AddKeyValue( Header, 'ZSNSTEPS', NumSteps ) ;
      FileIO.AddKeyValue( Header, 'ZSEXOFF', ExcitationOffDuringStep ) ;
      FileIO.AddKeyValue( Header, 'ZSENDEX', EndExposureAtStep ) ;

      // Z stage control line
      FileIO.AddKeyValue( Header, 'IOZSTAGE', MainFrm.IOConfig.ZStageControl ) ;

      end ;


end.
