unit EventAnalysisUnit;
// =============================================================================
// WinFluor - Event Analysis Module
// (c) J. Dempster, University of Strathclyde, 2001-2005, All Rights Reserved
// =============================================================================
// 30.05.05 Started
// 11.10.05 Display duration control buttons added
// 22.05.06 Max. display points increased to 8192
// 21.06.06 dF/F0 now works correctly with line scan mode
// 20.07.06 Window now handles no detected events correctly
//          EventAnalysisUnit settings added to INI file
// 25.09.06 Signals now averaged before dF/F0 calculation
// 22.07.07 Vertical cursors can now be moved using left/right arrow keys
// 10.09.09 JD "Buf" AnalyseEvent now allocated from heap using New(Buf) because
//          MaxDisplayPoints = 262144 caused stack overflow when defined as local procedure variable
//          Lots of LogFrm.Addlines (placed there for debugging?) removed because they were causing
//          opening failure on log file
// 04.10.09 NS Eliminated extraneous LogFrm.Addlines
// 09.07.10 JD Updating event detection
// 06.08.10 JD Updates to make new event detection work with line scan files
//             Event detection parameters now stored in INI file
// 20.10.10 JD Display buffer size increased to 1048576 points
//             Bug in export to WCP files fixed
// 29.11.12 JD TDecay,T90,Frequency variables now plotted correctly
//             Cursor readout on plot now shows values
//
// 13.11.12 ... .LOADADC() now uses 64 bit scan counter
// 29.01.14 Updated to Compile under both 32/64 bits (File handle now THandle)
// 07.10.15 JD Many minor event detection bugs fixed. Display screens can now be expanded to display whole record
//             Display vertical ranges updated when detection or fluorescence channels changed.
//             Rolling baseline now initialised to baseline cursor position

interface


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ScopeDisplay, HTMLLabel, ExtCtrls,
  ValidatedEdit, RangeEdit, Math, LabIOUnit, XYPlotDisplay, UITYpes ;

const
    EventListFileExt = '.evl' ;
    MaxEvents = 30000 ;
    MaxDisplayPoints = 16777216;
    DetFluor = 0 ;
    DetFluorRatio = 1 ;
    ADCChannelLimit = 7 ;
    ADCChannelSources = 2 ;
    FDFluorescence = 1 ;
    FDRelativeChange = 2 ;
    FDRatio = 3 ;
    FDIonConc = 4 ;
    FDRatioRMax = 5 ;

    XAxis = 0 ;
    YAxis = 1 ;
    vEventNum = 0 ;
    vTime = 1 ;
    vInterval = 2 ;
    vFrequency = 3 ;
    vCursor = 4 ;
    vPeak = 5 ;
    vArea = 6 ;
    vTRise = 7 ;
    vTDecay90 = 8 ;
    vTauDecay = 9 ;
    vDuration = 10 ;
    vBaseline = 11 ;
    MaxVar = 11 ;

    pPositivePeak = 0 ;
    pNegativePeak = 1 ;
    pAbsolutePeak = 2 ;

    PositiveGoing = 0 ;
    NegativeGoing = 1 ;
    PosNegGoing = 2 ;
type

  TEventListElement = packed record
      Time : Single ;
      Accepted : Boolean ;
      end ;

  TEventAnalysisFrm = class(TForm)
    Page: TPageControl;
    ViewTab: TTabSheet;
    AverageTab: TTabSheet;
    ViewGrp: TGroupBox;
    ControlGrp: TGroupBox;
    sbEventNum: TScrollBar;
    edEventNum: TRangeEdit;
    edEventTime: TEdit;
    GroupBox3: TGroupBox;
    cbSource: TComboBox;
    cbBackground: TComboBox;
    FluorGrp: TGroupBox;
    AvgGrp: TGroupBox;
    DisplayGrp: TGroupBox;
    ckEventRejected: TCheckBox;
    ExportGrp: TGroupBox;
    bExportADC: TButton;
    bExportFL: TButton;
    AvgDisplayGrp: TGroupBox;
    GroupBox2: TGroupBox;
    rbAvgAllEvents: TRadioButton;
    rbAvgRange: TRadioButton;
    edAvgRange: TRangeEdit;
    bAverageEvents: TButton;
    PlotTab: TTabSheet;
    PlotGrp: TGroupBox;
    GroupBox6: TGroupBox;
    rbPlotAllEvents: TRadioButton;
    rbPlotRange: TRadioButton;
    edPlotRange: TRangeEdit;
    bPlotGraph: TButton;
    PlotDisplayGrp: TGroupBox;
    plPlot: TXYPlotDisplay;
    bSetAxes: TButton;
    XGrp: TGroupBox;
    cbXVar: TComboBox;
    cbXSource: TComboBox;
    Label6: TLabel;
    GroupBox5: TGroupBox;
    Label8: TLabel;
    cbYVar: TComboBox;
    cbYSource: TComboBox;
    rgXPeakPolarity: TRadioGroup;
    rgYPeakPolarity: TRadioGroup;
    panXNumAvg: TPanel;
    edXNumAvg: TValidatedEdit;
    panYNumAvg: TPanel;
    edYNumAvg: TValidatedEdit;
    ckSuperimposeEvents: TCheckBox;
    panFLDisplay: TPanel;
    edTFLDisplay: TValidatedEdit;
    bTFLDisplayDouble: TButton;
    bTFLDisplayHalf: TButton;
    panADCDisplay: TPanel;
    edTADCDisplay: TValidatedEdit;
    bTADCDisplayDouble: TButton;
    bTADCDisplayHalf: TButton;
    ckEnableSeparateADCDisplayDuration: TCheckBox;
    DisplayModePage: TPageControl;
    FTab: TTabSheet;
    cbWavelength: TComboBox;
    dFTab: TTabSheet;
    rbF0FromFrames: TRadioButton;
    edF0Range: TRangeEdit;
    rbF0Constant: TRadioButton;
    edF0Constant: TValidatedEdit;
    cbdFWavelength: TComboBox;
    RatioTab: TTabSheet;
    cbNumWave: TComboBox;
    cbDenomWave: TComboBox;
    edRatioExclusionThreshold: TValidatedEdit;
    Label4: TLabel;
    GroupBox8: TGroupBox;
    edRatioRMax: TValidatedEdit;
    cbEquation: TComboBox;
    ckUseEquation: TCheckBox;
    ckDivideByRMax: TCheckBox;
    GroupBox1: TGroupBox;
    rbDFOverF0: TRadioButton;
    rbFOverF0: TRadioButton;
    Label9: TLabel;
    DetectTab: TTabSheet;
    DetectGrp: TGroupBox;
    DetectCritGrp: TGroupBox;
    detDisplayGrp: TGroupBox;
    panDetDisplay: TPanel;
    edTDetDisplay: TValidatedEdit;
    bDetDisplayDouble: TButton;
    bTDetDisplayHalf: TButton;
    sbDetDisplay: TScrollBar;
    bDetectEvents: TButton;
    GroupBox10: TGroupBox;
    cbDetectionSource: TComboBox;
    DetSettingsPage: TPageControl;
    detFluorSettingsTab: TTabSheet;
    DetFluorRatioSettingsTab: TTabSheet;
    DetADCSettingsTab: TTabSheet;
    cbDetFluor: TComboBox;
    Label2: TLabel;
    Label14: TLabel;
    cbDetFluorRatioTop: TComboBox;
    cbDetFluorRatioBottom: TComboBox;
    Shape1: TShape;
    edThresholdLevel: TValidatedEdit;
    Label15: TLabel;
    cbDetectionThresholdPolarity: TComboBox;
    Label16: TLabel;
    edThresholdDuration: TValidatedEdit;
    GroupBox7: TGroupBox;
    rbFixedBaseline: TRadioButton;
    Label10: TLabel;
    edFixedBaselineLevel: TValidatedEdit;
    rbRollingBaseline: TRadioButton;
    Label11: TLabel;
    edRollingBaselinePeriod: TValidatedEdit;
    Label7: TLabel;
    edDeadTime: TValidatedEdit;
    Label17: TLabel;
    cbFluorescence: TComboBox;
    Label3: TLabel;
    Label5: TLabel;
    Shape2: TShape;
    Label1: TLabel;
    edRatioDisplayMax: TValidatedEdit;
    Label18: TLabel;
    edDFDisplayMax: TValidatedEdit;
    Label19: TLabel;
    DetROIGrp: TGroupBox;
    Label13: TLabel;
    Label12: TLabel;
    cbDetROI: TComboBox;
    cbDetBackgROI: TComboBox;
    Label20: TLabel;
    eddetRatioexclusionThreshold: TValidatedEdit;
    Label21: TLabel;
    edDetRatioDisplayMax: TValidatedEdit;
    scDetDisplay: TScopeDisplay;
    scFLDisplay: TScopeDisplay;
    scADCDisplay: TScopeDisplay;
    scAvgFlDisplay: TScopeDisplay;
    scAvgADCDisplay: TScopeDisplay;
    bStop: TButton;
    bPlotGraphStop: TButton;
    edPlotStatus: TEdit;
    edDetectStatus: TEdit;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bDetectEventsClick(Sender: TObject);
    procedure cbDetectionSourceChange(Sender: TObject);
    procedure sbEventNumChange(Sender: TObject);
    procedure edEventNumKeyPress(Sender: TObject; var Key: Char);
    procedure edTADCDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure edTFLDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure cbFluorescenceDisplayChange(Sender: TObject);
    procedure scADCDisplayCursorChange(Sender: TObject);
    procedure scFLDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scADCDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bExportADCClick(Sender: TObject);
    procedure bExportFLClick(Sender: TObject);
    procedure bAverageEventsClick(Sender: TObject);
    procedure ckEventRejectedClick(Sender: TObject);
    procedure cbSourceClick(Sender: TObject);
    procedure cbBackgroundClick(Sender: TObject);
    procedure scAvgFlDisplayMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure scAvgADCDisplayMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure bPlotGraphClick(Sender: TObject);
    procedure cbXVarChange(Sender: TObject);
    procedure cbYVarChange(Sender: TObject);
    procedure cbXSourceChange(Sender: TObject);
    procedure cbYSourceChange(Sender: TObject);
    procedure PageChange(Sender: TObject);
    procedure bSetAxesClick(Sender: TObject);
    procedure cbWavelengthChange(Sender: TObject);
    procedure ckSuperimposeEventsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edTDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure bTFLDisplayHalfClick(Sender: TObject);
    procedure bTFLDisplayDoubleClick(Sender: TObject);
    procedure bTADCDisplayDoubleClick(Sender: TObject);
    procedure bTADCDisplayHalfClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure s(Sender: TObject; var Key: Char);
    procedure edF0RangeKeyPress(Sender: TObject; var Key: Char);
    procedure rbF0FromFramesClick(Sender: TObject);
    procedure rbF0ConstantClick(Sender: TObject);
    procedure DisplayModePageChange(Sender: TObject);
    procedure DisplayModePageChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure cbNumWaveChange(Sender: TObject);
    procedure cbDenomWaveChange(Sender: TObject);
    procedure ckUseEquationClick(Sender: TObject);
    procedure ckDivideByRMaxClick(Sender: TObject);
    procedure edRatioRMaxKeyPress(Sender: TObject; var Key: Char);
    procedure rbDFOverF0Click(Sender: TObject);
    procedure rbFOverF0Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure scFLDisplayCursorChange(Sender: TObject);
    procedure scAvgFlDisplayCursorChange(Sender: TObject);
    procedure scAvgADCDisplayCursorChange(Sender: TObject);
    procedure ResetCursors ;
    procedure sbDetDisplayChange(Sender: TObject);
    procedure edTDetDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure scDetDisplayCursorChange(Sender: TObject);
    procedure cbDetROIChange(Sender: TObject);
    procedure cbDetBackgROIClick(Sender: TObject);
    procedure cbDetFluorChange(Sender: TObject);
    procedure cbDetectionThresholdPolarityChange(Sender: TObject);
    procedure cbFluorescenceChange(Sender: TObject);
    procedure bTDetDisplayHalfClick(Sender: TObject);
    procedure bDetDisplayDoubleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ckEnableSeparateADCDisplayDurationClick(Sender: TObject);
    procedure edDetRatioDisplayMaxKeyPress(Sender: TObject; var Key: Char);
    procedure cbDetFluorRatioTopChange(Sender: TObject);
    procedure cbDetFluorRatioBottomChange(Sender: TObject);
    procedure eddetRatioexclusionThresholdKeyPress(Sender: TObject;
      var Key: Char);
    procedure edThresholdLevelKeyPress(Sender: TObject; var Key: Char);
    procedure edRatioDisplayMaxKeyPress(Sender: TObject; var Key: Char);
    procedure edRatioExclusionThresholdKeyPress(Sender: TObject;
      var Key: Char);
    procedure edFixedBaselineLevelKeyPress(Sender: TObject; var Key: Char);
    procedure cbSourceChange(Sender: TObject);
    procedure cbBackgroundChange(Sender: TObject);
    procedure cbEquationChange(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure bPlotGraphStopClick(Sender: TObject);
  private
    { Private declarations }
    MaxDisplayScans : Integer ;

    // Detection display cursors
    DetDisplayInitialised : Boolean ;
    DetBaseLineCursor : Integer ;  // Baseline cursor index
    DetBaselineCursorY : Integer ; // Baseline cursor position
    DetPosThresholdCursor : Integer ;  // Positive threshold cursor
    DetPosThresholdCursorY : Integer ;
    DetNegThresholdCursor : Integer ; // Negative threshold cursor
    DetNegThresholdCursorY : Integer ;


    ADCReadoutCursor : Integer ; // A/D readout cursor index on scADCDisplay
    ADCC0Cursor : Integer ;
    ADCC1Cursor : Integer ;
    FLReadoutCursor : Integer ; // A/D readout cursor index on scADCDisplay
    FLC0Cursor : Integer ;
    FLC1Cursor : Integer ;

    AvgADCReadoutCursor : Integer ;
    AvgFLReadoutCursor : Integer ;

    // X-Y Plot current settings
    PlotXVar : Integer ;         // X`axis variable
    PlotXSource : Integer ;      // X axis source
    PlotYVar : Integer ;         // Y axis variable
    PlotYSource : Integer ;      // Y axis source

    VarNames : Array[0..MaxVar] of String ; // Plot variable names
    Variables : Array[0..MaxVar,0..1] of Single ;

    DetectEventsRunning : Boolean ;
    StopDetectEventsRunning : Boolean ;

    procedure SetupFluorescenceDisplay ;

    procedure DisplayADCChannels(
              StartEvent : Integer ;
              EndEvent : Integer ;
              var DisplayBuf : Array of SmallInt ;
              scDisplay : TScopeDisplay ;
              ResetYRange : Boolean
              ) ;

    procedure DisplayFLIntensity(
              StartEvent : Integer ;
              EndEvent : Integer ;
              var DisplayBuf : Array of SmallInt ;
              scDisplay : TScopeDisplay ;
              ResetYRange : Boolean
              ) ;

    procedure UpdateYRange(
         Chan : Integer ;
         Ymin : single ;
         YMax : single ;
         scDisplay : TScopeDisplay ) ;

    procedure DisplayADCChannelAverages ;
    procedure DisplayFLIntensityAverages ;

    procedure DisplayDetChannel ;

    function F0ReferenceIntensity(
             StartGroup : Integer ;
             EndGroup : Integer
             ) : Single ;

    procedure SaveEventList ;
    procedure LoadEventList ;
    procedure UpdatePlotVariables ;
    procedure PlotGraph ;

    Function PlotAxisLabel(
             cbVar : TComboBox ;
             cbSource : TComboBox
             ) : String ;

    Function AnalyseEvent(
              SelectedAxis : Integer ;    // Plot Axis
              iVar : Integer ;            // Variable #
              iSource : Integer ;         // Signal source
              EventNum : Integer ;        // Event #
              NumAvgPoints : Integer ;
              PeakPolarity : Integer
              ) : Boolean ;

    function Intensity(
             iROI : Integer ;
             FrameNum : Integer ;
             FrameType : Integer
             ) : Single ;

    function GetDisplayGrid : Boolean ;
    procedure SetDisplayGrid( Value : Boolean ) ;
    procedure UpdateDetDisplay( ResetYRange : Boolean ) ;
    procedure UpdateDetCursors( CreateCursors : Boolean );

    procedure SetEventDisplayLimit ;
    
  public
    { Public declarations }
    EventTime : Single ;
    EventList : Array[0..MaxEvents-1] of TEventListElement ;
    NumEvents : Integer ;
    NumADCDisplayScans : Integer ;
    ADCBuf : Array[0..MaxDisplayPoints-1] of SmallInt ;
    AvgADCBuf : Array[0..MaxDisplayPoints-1] of SmallInt ;
    NumFLDisplayScans : Integer ;
    FLBuf : Array[0..MaxDisplayPoints-1] of SmallInt ;
    DetBuf : Array[0..MaxDisplayPoints-1] of Integer ;
    AvgFLBuf : Array[0..MaxDisplayPoints-1] of SmallInt ;

    procedure UpdateSettings ;
    procedure DisplayEvent( EventNum : Integer ;
                            ResetYRange : Boolean
                            ) ;
    procedure CopyImageToClipboard ;
    procedure CopyDataToClipboard ;
    procedure Print ;
    Function PlotAvailable : Boolean ;
    procedure MagnifyChannelDisplay( ChanNum : Integer ) ;
    procedure ReduceChannelDisplay( ChanNum : Integer ) ;
    procedure ZoomOutAll ;

    Property DisplayGrid : Boolean read GetDisplayGrid write SetDisplayGrid ;

  end;

var
  EventAnalysisFrm: TEventAnalysisFrm;

implementation

uses Main , ViewUnit, ViewLineUnit, PrintRec, ExportADCEventsUnit,
  Printgra, PlotSETAXES, IDRFile , ViewPlotUnit, LogUnit;

{$R *.dfm}

type
    TSingleArray = array[0..MaxDisplayPoints*2-1] of Single ;



procedure TEventAnalysisFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
    i : Integer ;
begin

    DetDisplayInitialised := False ;

    // Update ROI and frame type settings
    UpdateSettings ;

    edTDetDisplay.Value := MainFrm.IDRFile.EventDisplayDuration ;
    edDeadTime.Value := MainFrm.IDRFile.EventDeadTime ;


    edThresholdDuration.Value := MainFrm.IDRFile.EventThresholdDuration ;
    cbDetectionThresholdPolarity.ItemIndex := Min(Max(MainFrm.IDRFile.EventDetectionThresholdPolarity,0),
                                              cbDetectionThresholdPolarity.Items.Count-1) ;

    cbDetectionSource.ItemIndex := Min(Max(MainFrm.IDRFile.EventDetectionSource,0),
                                   cbDetectionSource.Items.Count-1);

    cbDetROI.ItemIndex := Min(Max(MainFrm.IDRFile.EventROI,0),
                                   cbDetROI.Items.Count-1);
    cbSource.ItemIndex := cbDetROI.ItemIndex ;
    cbDetBackgROI.ItemIndex := Min(Max(MainFrm.IDRFile.EventBackgROI,0),
                                   cbDetBackgROI.Items.Count-1);
    cbBackground.ItemIndex := cbDetROI.ItemIndex ;

    if cbDetectionSource.ItemIndex < MainFrm.IDRFile.NumFrameTypes then edThresholdLevel.Units := ''
    else edThresholdLevel.Units :=
         MainFrm.IDRFile.ADCChannel[cbDetectionSource.ItemIndex - MainFrm.IDRFile.NumFrameTypes].ADCUnits ;

    edThresholdLevel.Scale := 1.0 ;
    edThresholdLevel.Value := Abs(MainFrm.IDRFile.EventDetectionThreshold) ;

    rbFixedBaseline.Checked := MainFrm.IDRFile.EventFixedBaseline ;
    rbRollingBaseline.Checked := not MainFrm.IDRFile.EventFixedBaseline ;

    edRollingBaselinePeriod.Value := MainFrm.IDRFile.EventRollingBaselinePeriod ;
    edFixedBaselineLevel.Scale := 1.0 ;
    edFixedBaselineLevel.Value := MainFrm.IDRFile.EventBaselineLevel ;

    edRatioExclusionThreshold.Value := MainFrm.IDRFile.EventRatioExclusionThreshold ;
    edDetRatioExclusionThreshold.Value := MainFrm.IDRFile.EventRatioExclusionThreshold ;

    cbWavelength.ItemIndex := Min(Max(MainFrm.IDRFile.EventFLWave,0),cbWavelength.Items.Count-1) ;
    cbdFWavelength.ItemIndex := Min(Max(MainFrm.IDRFile.EventF0Wave,0),cbdFWavelength.Items.Count-1) ;

    cbNumWave.ItemIndex := Min(Max(MainFrm.IDRFile.EventRatioTop,0),cbNumWave.Items.Count-1) ;
    cbDetFluorRatioTop.ItemIndex := cbNumWave.ItemIndex ;

    cbDenomWave.ItemIndex := Min(Max(MainFrm.IDRFile.EventRatioBottom,0),cbDenomWave.Items.Count-1) ;
    cbDetFluorRatioBottom.ItemIndex := cbDenomWave.ItemIndex ;

    edRatioDisplayMax.Value := MainFrm.IDRFile.EventRatioDisplayMax ;
    edDetRatioDisplayMax.Value := MainFrm.IDRFile.EventRatioDisplayMax ;

    edRatioRMax.Value := MainFrm.IDRFile.EventRatioRMax ;
    edF0Range.LoValue := MainFrm.IDRFile.EventF0Start ;
    edF0Range.HiValue := MainFrm.IDRFile.EventF0End ;

    edF0Constant.Value := MainFrm.IDRFile.EventF0Constant ;
    rbF0Constant.Checked := MainFrm.IDRFile.EventF0UseConstant ;
    rbF0FromFrames.Checked := not MainFrm.IDRFile.EventF0UseConstant ;

    edDFDisplayMax.Value := MainFrm.IDRFile.EventF0DisplayMax ;

    rbDFOverF0.Checked := MainFrm.IDRFile.EventF0SubtractF0 ;
    rbFOverF0.Checked := not MainFrm.IDRFile.EventF0SubtractF0 ;

    edThresholdLevel.Value := Abs(MainFrm.IDRFile.EventDetectionThreshold) ;
    DetBaselineCursorY := MainFrm.IDRFile.EventBaselineLevel ;
    DetPosThresholdCursorY := DetBaselineCursorY + Round(edThresholdLevel.Value) ;
    DetNegThresholdCursorY := DetBaselineCursorY - Round(edThresholdLevel.Value) ;

    SetEventDisplayLimit ;

    edXNumAvg.Value := 1 ;
    edYNumAvg.Value := 1 ;

    // Initialise X-Y plot variables
    PlotXVar := 0 ;
    PlotXSource := 0 ;
    PlotYVar := 0 ;
    PlotYSource := 0 ;

    // Add readout cursor to plot
    plPlot.ClearVerticalCursors ;
    plPlot.AddVerticalCursor( clGreen, '?r', 0 ) ;

    scADCDisplay.StorageMode := ckSuperimposeEvents.Checked ;
    scFLDisplay.StorageMode := ckSuperimposeEvents.Checked ;

    // Add frame types
    if MainFrm.IDRFile.LineScan then begin
       if (MainFrm.IDRFile.NumFrameTypes = 1) and
          (MainFrm.IDRFile.NumFrames > 1) then
           MainFrm.IDRFile.NumFrameTypes := Min( MainFrm.IDRFile.NumFrames,
                                                 MaxFrameType+1 ) ;

       for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
           MainFrm.IDRFile.FrameType[i] := format('Frame %d',[i]) ;
       end ;

    scFLDisplay.FixZeroLevels := True ;
    scADCDisplay.FixZeroLevels := True ;
    scAvgFLDisplay.FixZeroLevels := True ;
    scAvgADCDisplay.FixZeroLevels := True ;

    scFLDisplay.DisplaySelected := True ;
    scADCDisplay.DisplaySelected := False ;
    scAvgFLDisplay.DisplaySelected := True ;
    scAvgADCDisplay.DisplaySelected := False ;

    ClientHeight := ViewGrp.Top + ViewGrp.Height + 5 ;
    // Set control sizes on form
    Resize ;

    // Load event list from file
    LoadEventList ;

    if NumEvents > 0 then Page.ActivePage := ViewTab
                     else Page.ActivePage := DetectTab ;
    DisplayModePage.ActivePage := FTab ;

    // Display detection channel
    UpdateDetDisplay( True ) ;
    DisplayDetChannel ;

    StopDetectEventsRunning := False ;
    DetectEventsRunning := False ;

    bStop.Enabled := False ;

    end;


procedure TEventAnalysisFrm.SetEventDisplayLimit ;
// ---------------------------------
// Set upper limit of event displays
// ---------------------------------
begin

    edTFLDisplay.LoLimit := 2.0*scFLDisplay.TScale ;
    edTFLDisplay.HiLimit := MaxDisplayScans*MainFrm.IDRFile.FrameInterval ;
    edTFLDisplay.Value := MainFrm.IDRFile.EventDisplayDuration ;

    edTADCDisplay.LoLimit := 2*MainFrm.IDRFile.ADCScanInterval ;
    edTADCDisplay.HiLimit := MaxDisplayScans*MainFrm.IDRFile.ADCScanInterval ;
    edTADCDisplay.Value := MainFrm.IDRFile.EventDisplayDuration ;

    // If displays locked together, set least upper limit
    if ckEnableSeparateADCDisplayDuration.Checked then begin
       edTFLDisplay.HiLimit := Min(edTFLDisplay.HiLimit,edTADCDisplay.HiLimit) ;
       edTADCDisplay.HiLimit := Min(edTFLDisplay.HiLimit,edTADCDisplay.HiLimit) ;
       end ;

    end ;


procedure TEventAnalysisFrm.FormResize(Sender: TObject);
// ----------------------------------------
// Adjust controls sizes when form re-sized
// ----------------------------------------
begin

    Page.Width := ClientWidth - Page.Left - 5 ;
    Page.Height := ClientHeight - Page.Top - 5 ;

    ViewGrp.Height := ViewTab.ClientHeight - ViewGrp.Top - 5 ;
    AvgGrp.Height := AverageTab.ClientHeight - AvgGrp.Top - 5 ;

    // Detection display

    Detectgrp.Height := DetectTab.ClientHeight - Detectgrp.Top - 5 ;
    DetDisplayGrp.Width := Max(DetectTab.ClientWidth - DetDisplayGrp.Left - 5,2) ;
    DetDisplayGrp.Height := Max(DetectTab.ClientHeight - DetDisplayGrp.Top - 5,2) ;

    scDetDisplay.Width := Max(DetDisplayGrp.ClientWidth - scDetDisplay.Left - 5,2) ;
    scDetDisplay.Height := Max(DetDisplayGrp.ClientHeight
                               - scDetDisplay.Top
                               - panDetDisplay.Height
                               - 5,2) ;
    sbDetDisplay.Top := scDetDisplay.Top + scDetDisplay.Height + 1 ;
    panDetDisplay.Top := sbDetDisplay.Top ;
    panDetDisplay.Left := scDetDisplay.Left + scDetDisplay.Width - panDetDisplay.Width ;
    sbDetDisplay.Width := scDetDisplay.Left + scDetDisplay.Width - panDetDisplay.Width - 2 ;


    DisplayGrp.Width := Max(ViewTab.ClientWidth - DisplayGrp.Left - 5,2) ;
    DisplayGrp.Height := Max(ViewTab.ClientHeight - DisplayGrp.Top - 5,2) ;

    scFLDisplay.Width := Max(DisplayGrp.ClientWidth - scFLDisplay.Left - 5,2) ;
    scADCDisplay.Width := scFLDisplay.Width ;

    if MainFrm.IDRFile.ADCNumChannels > 0 then begin
       // A/D channels available
       scFLDisplay.Height := Max( (DisplayGrp.ClientHeight
                                 - scFLDisplay.Top
                                - (panFLDisplay.Height + 1)
                                - (panADCDisplay.Height +1)
                                - 10) div 2, 2) ;

       scADCDisplay.Height := scFLDisplay.Height ;
       scADCDisplay.Visible := True ;
       panADCDisplay.Visible := True ;
       bExportADC.Enabled := True ;
       end
    else begin
       // No A/D channels to display
       scFLDisplay.Height := Max( (DisplayGrp.ClientHeight
                                 - scFLDisplay.Top
                                - (panFLDisplay.Height + 1)
                                - (panADCDisplay.Height +1)
                                - 10), 2) ;
       scADCDisplay.Height := 2 ;
       scADCDisplay.Visible := False ;
       panADCDisplay.Visible := False ;
       bExportADC.Enabled := false ;
       end ;

    PanFLDisplay.Top := scFLDisplay.Top + scFLDisplay.Height + 2 ;

    scADCDisplay.Top := PanFLDisplay.Top + PanFLDisplay.Height + 2 ;

    panADCDisplay.Left := DisplayGrp.ClientWidth -
                                  panADCDisplay.Width - 5 ;
    PanFLDisplay.Left := DisplayGrp.ClientWidth -
                          PanFLDisplay.Width - 5 ;

    panADCDisplay.Top := scADCDisplay.Top + scADCDisplay.Height + 2 ;
    ckSuperimposeEvents.Top := panADCDisplay.Top ;

    AvgDisplayGrp.Width := Max(ViewTab.ClientWidth - AvgDisplayGrp.Left - 5,2) ;
    AvgDisplayGrp.Height := Max(ViewTab.ClientHeight - AvgDisplayGrp.Top - 5,2) ;

    scAvgFLDisplay.Top := scFLDisplay.Top ;

    scAvgFLDisplay.Width := Max(AvgDisplayGrp.ClientWidth - scAvgFLDisplay.Left - 5,2) ;
    scAvgADCDisplay.Width := scAvgFLDisplay.Width ;

    scAvgFLDisplay.Height := Max( (AvgDisplayGrp.ClientHeight
                                - scAvgFLDisplay.Top
                                - 10) div 2, 2) ;
    scAvgADCDisplay.Height := scAvgFLDisplay.Height ;

    scAvgADCDisplay.Top := scAvgFLDisplay.Top + scAvgFLDisplay.Height + 5 ;

    // X/Y Plot page

    PlotGrp.Height := PlotTab.ClientHeight - PlotGrp.Top - 5 ;
    PlotDisplayGrp.Width := PlotTab.ClientWidth -PlotDisplayGrp.Left - 5 ;
    PlotDisplayGrp.Height := PlotTab.ClientHeight -PlotDisplayGrp.Top - 5 ;

    plPlot.Width := Max(PlotDisplayGrp.ClientWidth - plPlot.Left - 5,2) ;
    plPlot.Height := Max(PlotDisplayGrp.ClientHeight - plPlot.Top - 5, 2) ;

    end ;


procedure TEventAnalysisFrm.UpdateSettings ;
// -------------------------------------------------------
// Update available regions of interest and other settings
// -------------------------------------------------------
var
    ch,i,iOld : Integer ;
begin

    // Calculate max. number of A/D scans (must be multiple of 256)
    MaxDisplayScans := MaxDisplayPoints div Max(MainFrm.IDRFile.ADCNumChannels,1) ;

    // Create list of sources
    cbSource.Clear ;

    // Regions of interest
    if MainFrm.IDRFile.LineScan then begin
       // Line scan data file
       cbSource.Items.AddObject( 'TC',TObject(1)) ;
       cbDetROI.Items.AddObject( 'TC',TObject(1)) ;
       cbDetBackgROI.Items.AddObject( ' ', TObject(MainFrm.IDRFile.MaxROI+1)) ;
       end
    else begin
       // Image data files : list regions of interest
       cbDetBackgROI.Items.AddObject( ' ', TObject(MainFrm.IDRFile.MaxROI+1)) ;
       for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then begin
           cbSource.Items.AddObject( format('ROI.%d',[i]),TObject(i)) ;
           cbDetROI.Items.AddObject( format('ROI.%d',[i]),TObject(i)) ;
           cbDetBackgROI.Items.AddObject( format('ROI.%d',[i]),TObject(i)) ;
           end ;
       end ;

    cbSource.ItemIndex := 0 ;
    cbDetROI.ItemIndex := 0 ;
    cbDetBackgROI.ItemIndex := 0 ;

    // Create list of background subtraction sources
    cbBackground.Clear ;
    cbBackground.Items.AddObject( ' ',TObject(MainFrm.IDRFile.MaxROI+1)) ;
    if MainFrm.IDRFile.LineScan then begin
       cbBackground.Enabled := False ;
       end
    else begin
       // Image series data file
       cbBackground.Enabled := True ;
       for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then
           cbBackground.Items.AddObject( format('ROI.%d',[i]),TObject(i)) ;
       end ;
    cbBackground.ItemIndex := 0 ;

    // Create list of event detection channels
    cbDetectionSource.Clear ;
    // Add fluorescence options
    cbDetectionSource.Items.Add('Fluorescence') ;
    cbDetectionSource.Items.Add('Fluorescence ratio') ;
    // Add A/D channels
    for i := 0 to MainFrm.IDRFile.ADCNumChannels-1 do
        cbDetectionSource.Items.AddObject( MainFrm.IDRFile.ADCChannel[i].ADCName,
                                           TObject(i+MainFrm.IDRFile.MaxROI+1)) ;
    cbDetectionSource.ItemIndex := 0 ;

    // Add frame types
    for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do begin
        cbDetFluor.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
        cbDetFluorRatioTop.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
        cbDetFluorRatioBottom.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
        end ;
    cbDetFluor.ItemIndex := 0 ;
    cbDetFluorRatioTop.ItemIndex := 0 ;
    cbDetFluorRatioBottom.ItemIndex := Min(1,cbDetFluorRatioBottom.Items.Count-1) ;

    // Enabled plot creation buttons if sources available
    if cbSource.Items.Count > 0 then begin
       bDetectEvents.Enabled := True ;
       end
    else begin
       bDetectEvents.Enabled := False ;
       end ;

    // Create list of event detection channels
    cbFluorescence.Clear ;
    // Add fluorescence options
    cbFluorescence.Items.Add('Fluorescence') ;
    cbFluorescence.Items.Add('Fluorescence ratio') ;
    cbFluorescence.Items.Add('Fluorescence dF/F0') ;
    cbFluorescence.ItemIndex := 0 ;
    DisplayModePage.ActivePage := FTab ;

    // Raw intensity display
    cbWavelength.Clear ;
    for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
        cbWavelength.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
    cbWavelength.ItemIndex := Min(Max(MainFrm.IDRFile.EventFLWave,0),cbWavelength.Items.Count-1) ;

    // dF/F0 display
    cbdFWavelength.Clear ;
    for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
        cbdFWavelength.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
    cbdFWavelength.ItemIndex := Min(Max(MainFrm.IDRFile.EventF0Wave,0),cbdFWavelength.Items.Count-1) ;

    // Ratio display
    cbNumWave.Clear ;
    iOld := cbNumWave.ItemIndex ;
    for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
        cbNumWave.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
    cbNumWave.ItemIndex := Min(Max(MainFrm.IDRFile.EventRatioTop,0),cbNumWave.Items.Count-1) ;

    cbDenomWave.Clear ;
    for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
        cbDenomWave.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
    cbDenomWave.ItemIndex := Min(Max(MainFrm.IDRFile.EventRatioBottom,0),cbDenomWave.Items.Count-1) ;

    // Ensure numerator and denomintor are different
    if (cbDenomWave.ItemIndex = cbNumWave.ItemIndex) then begin
       for i := 0 to cbNumWave.Items.Count-1 do
           if i <> cbNumWave.ItemIndex then cbDenomWave.ItemIndex := i ;
       end ;

    // Update available ion binding equations
    iOld := cbEquation.ItemIndex ;
    cbEquation.Clear ;
    for i := 0 to MainFrm.IDRFile.MaxEquations-1 do
        if MainFrm.IDRFile.Equation[i].InUse then
           cbEquation.Items.AddObject( MainFrm.IDRFile.Equation[i].Name,
                                       TObject(i) ) ;
    if cbEquation.Items.Count > 0 then cbEquation.Enabled := True
                                   else cbEquation.Enabled := False ;
    if iOld < cbEquation.Items.Count then cbEquation.ItemIndex := iOld ;
    if (iOld < 0) and (cbEquation.Items.Count > 0) then cbEquation.ItemIndex := 0 ;

    SetupFluorescenceDisplay ;

    // Set up ROI fluorescence display
    // ------------------------------

    //scFLDisplay.edT
    scFLDisplay.MinADCValue := -MainFrm.IDRFile.GreyMax ;
    scFLDisplay.MaxADCValue := MainFrm.IDRFile.GreyMax ;
    scFLDisplay.NumChannels := 1 ;
    // Get fluorescence display inter-point interval
    if MainFrm.IDRFile.LineScan then begin
       scFLDisplay.TScale := MainFrm.IDRFile.FrameInterval/MainFrm.IDRFile.FrameHeight ;
       end
    else begin
       scFLDisplay.TScale := MainFrm.IDRFile.FrameInterval*MainFrm.IDRFile.NumFrameTypes ;
       end ;

    scFLDisplay.TUnits := 's' ;

    // Add zero level cursors
    scFLDisplay.ClearHorizontalCursors ;
    for ch :=  0 to scFLDisplay.NumChannels-1 do
        scFLDisplay.AddHorizontalCursor( ch, clBlue, True, 'z' ) ;

    // Set No. of points in A/D window (multiple of 256)
    scFLDisplay.MaxPoints := Min( Round(edTFLDisplay.Value/scFLDisplay.TScale),
                                  MaxDisplayScans) ;

    scFLDisplay.NumPoints := 0 ;

     scFLDisplay.xMax := scFLDisplay.MaxPoints-1 ;
     scFLDisplay.xMin := 0 ;

     { Set channel information }
     scFLDisplay.ChanOffsets[0] := 0 ;
     scFLDisplay.ChanUnits[0] := '';
     scFLDisplay.ChanName[0] := MainFrm.IDRFile.FrameType[cbWavelength.ItemIndex] ;
     scFLDisplay.ChanScale[0] := 1.0 ;
     UpdateYRange( 0, 0, MainFrm.GreyHi[cbWavelength.ItemIndex], scFLDisplay ) ;
     scFLDisplay.HorizontalCursors[0] := 0 ;
     scFLDisplay.ChanVisible[0] := True ;

    scFLDisplay.SetDataBuf( @FLBuf ) ;

    scFLDisplay.ClearVerticalCursors ;
    FLC0Cursor := scFLDisplay.AddVerticalCursor(-1,clGray,'a') ;
    scFLDisplay.VerticalCursors[FLC0Cursor] := -1 ;
    FLC1Cursor := scFLDisplay.AddVerticalCursor(-1,clGray,'a') ;
    scFLDisplay.VerticalCursors[FLC1Cursor] := -1 ;
    // Draw horizontal link between cursors
    scFLDisplay.LinkVerticalCursors( FLC0Cursor, FLC1Cursor ) ;

    FLReadoutCursor := scFLDisplay.AddVerticalCursor(-1,clGreen, '?i?y' ) ;
    scFLDisplay.VerticalCursors[FLReadoutCursor] := -1 ;

    // Set up A/D channel display
    // --------------------------

    if MainFrm.IDRFile.ADCNumChannels > 0 then begin

       scADCDisplay.MaxADCValue := MainFrm.IDRFile.ADCMaxValue ;
       scADCDisplay.MinADCValue := -MainFrm.IDRFile.ADCMaxValue - 1 ;

       scADCDisplay.ZoomDisableHorizontal := True ;

       // Add zero level cursors
       scADCDisplay.ClearHorizontalCursors ;
       for ch :=  0 to MainFrm.IDRFile.ADCNumChannels-1 do
           scADCDisplay.AddHorizontalCursor( ch, clBlue, True, 'z' ) ;

       // No. of points in A/D window
       scADCDisplay.MaxPoints := Min( Round(edTADCDisplay.Value/MainFrm.IDRFile.ADCSCanInterval),
                                      MaxDisplayScans ) ;

       scADCDisplay.xMax := scADCDisplay.MaxPoints-1 ;
       scADCDisplay.NumPoints := 0 ;
       scADCDisplay.NumChannels := MainFrm.IDRFile.ADCNumChannels ;

       scADCDisplay.xMin := 0 ;
       scADCDisplay.TScale := MainFrm.IDRFile.ADCScanInterval ;
       scADCDisplay.TUnits := 's' ;

       { Set channel information }
       for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
           scADCDisplay.ChanOffsets[ch] := MainFrm.IDRFile.ADCChannel[ch].ChannelOffset ;
           //MainFrm.IDRFile.ADCChannel[ch].ChannelOffset := scADCDisplay.ChanOffsets[ch] ;
           scADCDisplay.ChanUnits[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCUnits ;
           scADCDisplay.ChanName[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCName ;
           scADCDisplay.ChanScale[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCScale ;
           scADCDisplay.yMin[ch] := MainFrm.IDRFile.ADCChannel[ch].yMin ;
           scADCDisplay.yMax[ch] := MainFrm.IDRFile.ADCChannel[ch].yMax ;
           scADCDisplay.HorizontalCursors[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCZero ;
           scADCDisplay.ChanVisible[ch] := MainFrm.IDRFile.ADCChannel[ch].InUse ;
           end ;

       // Allocate A/D buffer
       scADCDisplay.SetDataBuf( @ADCBuf ) ;

       scADCDisplay.ClearVerticalCursors ;
       ADCC0Cursor := scADCDisplay.AddVerticalCursor(-1,clGray,'a') ;
       scADCDisplay.VerticalCursors[ADCC0Cursor] := -1 ;
       ADCC1Cursor := scADCDisplay.AddVerticalCursor(-1,clGray,'a') ;
       scADCDisplay.VerticalCursors[ADCC1Cursor] := -1 ;
       // Draw horizontal link between cursors
       scADCDisplay.LinkVerticalCursors( ADCC0Cursor, ADCC1Cursor ) ;
       ADCReadoutCursor := scADCDisplay.AddVerticalCursor(-1,clGreen,'?y') ;
       scADCDisplay.VerticalCursors[ADCReadoutCursor] := -1 ;

       end ;

    scFLDisplay.DisplaySelected := True ;
    scAvgFLDisplay.DisplaySelected := True ;

    Resize ;

    if NumEvents > 0 then DisplayEvent( sbEventNum.Position, True ) ;

    // Update detection display
    UpdateDetDisplay( True ) ;

    end;


procedure TEventAnalysisFrm.UpdateDetDisplay(
          ResetYRange : Boolean ) ;
// --------------------------------------
// Update event detection channel display
// --------------------------------------
var
    ADCChan : Integer ;
    Temp : Single ;
begin

    if cbDetectionSource.ItemIndex < ADCChannelSources then begin
       // Fluorescence channels
       scDetDisplay.MinADCValue := -MainFrm.IDRFile.GreyMax ;
       scDetDisplay.MaxADCValue := MainFrm.IDRFile.GreyMax ;

       case cbDetectionSource.ItemIndex of

          detFluor : Begin
             scDetDisplay.ChanName[0] := MainFrm.IDRFile.FrameType[cbDetFluor.ItemIndex] ;
             scDetDisplay.ChanUnits[0] := '';
             scDetDisplay.ChanScale[0] := 1.0 ;
             if ResetYRange then UpdateYRange(0,0,MainFrm.GreyHi[cbDetFluor.ItemIndex],scDetDisplay) ;
             DetSettingsPage.ActivePage := DetFluorSettingsTab ;
             end ;

          detFluorRatio : begin
             scDetDisplay.ChanName[0] := MainFrm.IDRFile.FrameType[cbDetFluorRatioTop.ItemIndex]
                                         + '/' +
                                         MainFrm.IDRFile.FrameType[cbDetFluorRatioBottom.ItemIndex] ;
             scDetDisplay.ChanUnits[0] := '';
             scDetDisplay.ChanScale[0] := edDetRatioDisplayMax.Value/scDetDisplay.MaxADCValue ;
             if ResetYRange then UpdateYRange(0,0,scDetDisplay.MaxADCValue,scDetDisplay) ;
             DetSettingsPage.ActivePage := DetFluorRatioSettingsTab ;
             end ;

          end ;

       // Get fluorescence display inter-point interval
       if MainFrm.IDRFile.LineScan then begin
          // Line scan files
          scDetDisplay.TScale := MainFrm.IDRFile.FrameInterval/MainFrm.IDRFile.FrameHeight ;
          scDetDisplay.MaxPoints := Min( Round(edTDetDisplay.Value/scDetDisplay.TScale),
                                         MaxDisplayScans) ;
          sbDetDisplay.Min := 1 ;
          sbDetDisplay.Max := Max(MainFrm.IDRFile.FrameHeight-scDetDisplay.MaxPoints,2) ;
          edTDetDisplay.Value := scDetDisplay.MaxPoints*scDetDisplay.TScale ;
          MainFrm.IDRFile.EventDisplayDuration := edTDetDisplay.Value ;
          end
       else begin
          scDetDisplay.TScale := MainFrm.IDRFile.FrameInterval{*MainFrm.IDRFile.NumFrameTypes} ;
          scDetDisplay.MaxPoints := Min( Round(edTDetDisplay.Value/scDetDisplay.TScale),
                                         MaxDisplayScans) ;
          sbDetDisplay.Min := 1 ;
          sbDetDisplay.Max := Max(MainFrm.IDRFile.NumFrames,2) ;
          edTDetDisplay.Value := scDetDisplay.MaxPoints*scDetDisplay.TScale ;
          MainFrm.IDRFile.EventDisplayDuration := edTDetDisplay.Value ;
          end ;
       DetROIGrp.Visible := True ;
       end
    else begin
       // Analog channels
       ADCChan := cbDetectionSource.ItemIndex - ADCChannelSources ;
       scDetDisplay.MinADCValue := -MainFrm.IDRFile.ADCMaxValue ;
       scDetDisplay.MaxADCValue := MainFrm.IDRFile.ADCMaxValue ;
       scDetDisplay.TScale := MainFrm.IDRFile.ADCSCanInterval ;
      // Set No. of points in A/D window (multiple of 256)
       scDetDisplay.MaxPoints := Min( Round(edTDetDisplay.Value/scDetDisplay.TScale),
                                      MaxDisplayScans) ;
       edTDetDisplay.Value := scDetDisplay.MaxPoints*scDetDisplay.TScale ;
       MainFrm.IDRFile.EventDisplayDuration := edTDetDisplay.Value ;
       scDetDisplay.ChanName[0] := MainFrm.IDRFile.ADCChannel[ADCChan].ADCName ;
       scDetDisplay.ChanUnits[0] := MainFrm.IDRFile.ADCChannel[ADCChan].ADCUnits ;
       scDetDisplay.ChanScale[0] := MainFrm.IDRFile.ADCChannel[ADCChan].ADCSCale ;
       if ResetYRange then begin
          UpdateYRange(0,MainFrm.IDRFile.ADCChannel[ADCChan].YMin,
                       MainFrm.IDRFile.ADCChannel[ADCChan].YMax,scDetDisplay) ;
          end;
       sbDetDisplay.Min := 0 ;
       sbDetDisplay.Max := Max(MainFrm.IDRFile.ADCNumScansInFile,1) ;
       DetSettingsPage.ActivePage := DetADCSettingsTab ;
       DetROIGrp.Visible := False ;

       end ;

    scDetDisplay.NumChannels := 1 ;

    scDetDisplay.TUnits := 's' ;

    Temp := edThresholdLevel.Value ;
    edThresholdLevel.Units := scDetDisplay.ChanUnits[0] ;
    edThresholdLevel.Scale := scDetDisplay.ChanScale[0] ;
    edThresholdLevel.Value :=  Temp ;

    Temp := edFixedBaselineLevel.Value ;
    edFixedBaselineLevel.Units := scDetDisplay.ChanUnits[0] ;
    edFixedBaselineLevel.Scale := scDetDisplay.ChanScale[0] ;
    edFixedBaselineLevel.Value := Temp ;

    Temp := edThresholdDuration.Value ;
    edThresholdDuration.Scale := scDetDisplay.TScale ;
    edThresholdDuration.LoLimit := 1.0 ;
    edThresholdDuration.Value := Temp ;

    if ResetYRange then begin
       DetBaseLineCursorY := Round((scDetDisplay.YMin[0] + scDetDisplay.YMax[0])*0.5) ;
       edThresholdLevel.Value := (scDetDisplay.YMax[0] - scDetDisplay.YMin[0])*0.05 ;
       end;
    DetPosThresholdCursorY := DetBaseLineCursorY + Abs(Round(edThresholdLevel.Value));
    DetNegThresholdCursorY := DetBaseLineCursorY - Abs(Round(edThresholdLevel.Value));

    // Create baseline & threshold cursors
    UpdateDetCursors( True ) ;

    scDetDisplay.NumPoints := 0 ;

    scDetDisplay.xMax := scDetDisplay.MaxPoints-1 ;
    scDetDisplay.xMin := 0 ;

    { Set channel information }
    scDetDisplay.ChanOffsets[0] := 0 ;
    scDetDisplay.ChanVisible[0] := True ;

    scDetDisplay.SetDataBuf( @DetBuf ) ;
    scDetDisplay.NumBytesPerSample := 4 ;

    scDetDisplay.ClearVerticalCursors ;
    scDetDisplay.AddVerticalCursor(-1,clGreen, '?y' ) ;
    scDetDisplay.VerticalCursors[0] := scDetDisplay.MaxPoints div 2 ;

    DetDisplayInitialised := True ;

    // Redisplay channel
    DisplayDetChannel ;

    end ;


procedure TEventAnalysisFrm.UpdateDetCursors(
          CreateCursors : Boolean );            // Create new set cursors if TRUE
// -------------------------------
// Update detection window cursors
// -------------------------------
begin

    if CreateCursors then scDetDisplay.ClearHorizontalCursors ;

    if CreateCursors then DetBaseLineCursor := scDetDisplay.AddHorizontalCursor(0,clRed,False,'Baseline' ) ;
    scDetDisplay.HorizontalCursors[DetBaseLineCursor] := DetBaseLineCursorY ;

    case cbDetectionThresholdPolarity.ItemIndex of
         PositiveGoing : Begin
            if CreateCursors then
               DetPosThresholdCursor := scDetDisplay.AddHorizontalCursor( 0, clRed, False, 'Threshold(+)' ) ;
            scDetDisplay.HorizontalCursors[DetPosThresholdCursor] := DetPosThresholdCursorY ;
            end ;
         NegativeGoing : Begin
            if CreateCursors then
               DetNegThresholdCursor := scDetDisplay.AddHorizontalCursor( 0, clRed, False, 'Threshold(-)' ) ;
            scDetDisplay.HorizontalCursors[DetNegThresholdCursor] := DetNegThresholdCursorY ;
            end ;
         PosNegGoing : Begin
            if CreateCursors then begin
               DetPosThresholdCursor := scDetDisplay.AddHorizontalCursor( 0, clRed, False, 'Threshold(+)' ) ;
               DetNegThresholdCursor := scDetDisplay.AddHorizontalCursor( 0, clRed, False, 'Threshold(-)' ) ;
               end;
            scDetDisplay.HorizontalCursors[DetPosThresholdCursor] := DetPosThresholdCursorY ;
            scDetDisplay.HorizontalCursors[DetNegThresholdCursor] := DetNegThresholdCursorY ;
            end ;
         end ;

    scDetDisplay.invalidate ;

    end;


procedure TEventAnalysisFrm.DisplayDetChannel ;
// ----------------------------------
// Display selected detection channel
// ----------------------------------
var
    i,j,iFrame,ADCChan,IROI,iBackg : Integer ;
    FrameType, FTTop, FTBottom : Integer ;
    InBuf : PSmallIntArray ;
    SubtractBackground : Boolean ;
    y,yTop,yBottom : single ;
    ExclusionThreshold : Integer ;
begin

    if not DetDisplayInitialised Then Exit ;

    case cbDetectionSource.ItemIndex of

        // Display fluorescence channel
        // ----------------------------
        DetFluor : begin
          // ROI# to be displayed
          iROI := Integer(cbDetROI.Items.Objects[cbDetROI.ItemIndex]) ;

          // Background ROI# to be subtracted (if any)
          iBackg := Integer(cbDetBackgROI.Items.Objects[cbDetBackgROI.ItemIndex]) ;
          if (iBackg >= 0) and
             (iBackg <= MainFrm.IDRFile.MaxROI) and
             (not MainFrm.IDRFile.LineScan)
             then SubtractBackground := True
             else SubtractBackground := False ;

           FrameType := cbDetFluor.ItemIndex ;

           j := 0 ;
           for iFrame := sbDetDisplay.Position to
               Min(sbDetDisplay.Position + scDetDisplay.MaxPoints,MainFrm.IDRFile.NumFrames) do begin
               y := Intensity( iROI, iFrame, FrameType ) ;
               if SubtractBackground then y := y - Intensity( iBackg, iFrame, FrameType ) ;
               DetBuf[j] := Round(y) ;
               Inc(j) ;
               end ;
           scDetDisplay.NumPoints := j ;
           end ;

        // Display fluorescence ratio
        // ----------------------------
        DetFluorRatio : begin

          // ROI# to be displayed
          iROI := Integer(cbDetROI.Items.Objects[cbDetROI.ItemIndex]) ;

          // Background ROI# to be subtracted (if any)
          iBackg := Integer(cbDetBackgROI.Items.Objects[cbDetBackgROI.ItemIndex]) ;
          if (iBackg >= 0) and
             (iBackg <= MainFrm.IDRFile.MaxROI) and
             (not MainFrm.IDRFile.LineScan)
             then SubtractBackground := True
             else SubtractBackground := False ;

           FTTop := cbDetFluorRatioTop.ItemIndex ;
           FTBottom := cbDetFluorRatioBottom.ItemIndex ;
           ExclusionThreshold := Round(edDetRatioExclusionThreshold.Value) ;
           j := 0 ;
           for iFrame := sbDetDisplay.Position to
               Min(sbDetDisplay.Position + scDetDisplay.MaxPoints,MainFrm.IDRFile.NumFrames) do begin

               yTop := Intensity( iROI, iFrame, FTTop ) ;
               if SubtractBackground then yTop := yTop - Intensity( iBackg, iFrame, FTTop ) ;
               yBottom := Intensity( iROI, iFrame, FTBottom ) ;
               if SubtractBackground then yBottom := yBottom - Intensity( iBackg, iFrame, FTBottom ) ;

               if (yBottom > ExclusionThreshold) and (yTop > ExclusionThreshold) then begin
                  DetBuf[j] := Round( (YTop/YBottom)/scDetDisplay.ChanScale[0]) ;
                  end
               else DetBuf[j] := 0 ;

               Inc(j) ;
               end ;
           scDetDisplay.NumPoints := j ;
           end ;

        else begin

           // Display analogue channel
           // ------------------------

            // Read data for A/D channel into display buffer
           ADCChan := cbDetectionSource.ItemIndex - ADCChannelSources ;
           GetMem( InBuf, scDetDisplay.MaxPoints*MainFrm.IDRFile.ADCNumChannels*2) ;
           scDetDisplay.NumPoints := Min(scDetDisplay.MaxPoints,
                                      MainFrm.IDRFile.ADCNumScansInFile-sbDetDisplay.Position);
           MainFrm.IDRFile.LoadADC( Int64(sbDetDisplay.Position),scDetDisplay.NumPoints,InBuf^ ) ;
           j := MainFrm.IDRFile.ADCChannel[ADCChan].ChannelOffset ;
           for i := 0 to scDetDisplay.NumPoints-1 do begin
               DetBuf[i] := InBuf[j] ;
               j := j + MainFrm.IDRFile.ADCNumChannels ;
               end ;
           FreeMem(InBuf) ;
           end ;

        end ;

    scDetDisplay.XOffset := sbDetDisplay.Position ;
    scDetDisplay.Invalidate ;

    end ;


procedure TEventAnalysisFrm.SaveEventList ;
// ------------------------------------
// Save list of detected events to file
// ------------------------------------
var
   EventListFileName : String ;
   i : Integer ;
   FileHandle : THandle ;
begin

     // Create event list file
     EventListFileName := ChangeFileExt( MainFrm.IDRFile.FileName, EventListFileExt ) ;
     FileHandle := FileCreate( EventListFileName ) ;
     if NativeInt(FileHandle) < 0 then begin
        Exit ;
        end ;

     // Move to start of file
     FileSeek( FileHandle, 0, 0 ) ;

     // Write no. of events
     FileWrite( FileHandle, NumEvents, SizeOf(NumEvents) ) ;
     // Write event data
     for i := 0 to NumEvents-1 do begin
         FileWrite( FileHandle, EventList[i], SizeOf(TEventListElement) ) ;
         end ;

     FileClose( FileHandle ) ;

     // Update available variables on X-Y plot page
     UpdatePlotVariables ;

     end ;


procedure TEventAnalysisFrm.LoadEventList ;
// ------------------------------------
// Save list of detected events to file
// ------------------------------------
var
   EventListFileName : String ;
   i : Integer ;
   FileHandle : THandle ;
begin

     // Create event list file
     EventListFileName := ChangeFileExt( MainFrm.IDRFile.FileName, EventListFileExt ) ;

     // Set no. events to zero if no file
     if FileExists(EventListFileName) then begin
        // Open file
        FileHandle := FileOpen( EventListFileName, fmOpenRead ) ;

        if NativeInt(FileHandle) >= 0 then begin

           // Move to start of file
           FileSeek( FileHandle, 0, 0 ) ;

           // Read no. of events
           FileRead( FileHandle, NumEvents, SizeOf(NumEvents) ) ;
           // Read event data
           for i := 0 to NumEvents-1 do begin
               FileRead( FileHandle, EventList[i], SizeOf(TEventListElement) ) ;
               end ;

           // Close file
           FileClose( FileHandle ) ;

           end ;
        end ;

     sbEventNum.Min := Min(NumEvents,1) ;
     sbEventNum.Max := Max(NumEvents,0) ;
     sbEventNum.Position := Min(1,sbEventNum.Max) ;
     edEventNum.LoLimit := Min(NumEvents,1) ;
     edEventNum.LoValue := Min(NumEvents,1) ;
     edEventNum.HiLimit := sbEventNum.Max ;
     edEventNum.HiValue := sbEventNum.Max ;

     edAvgRange.HiLimit := sbEventNum.Max ;
     edAvgRange.LoValue := sbEventNum.Min ;
     edAvgRange.HiValue := sbEventNum.Max ;

     edPlotRange.HiLimit := sbEventNum.Max ;
     edPlotRange.LoValue := sbEventNum.Min ;
     edPlotRange.HiValue := sbEventNum.Max ;

     if NumEvents > 0 then begin
        DisplayEvent( sbEventNum.Position, True ) ;
        // Update available variables on X-Y plot page
        UpdatePlotVariables ;
        end ;

     end ;


procedure TEventAnalysisFrm.DisplayEvent(
          EventNum : Integer ;
          ResetYRange : Boolean
          ) ;
// ----------------------
// Display selected event
// ----------------------
begin

    bExportADC.Enabled := False ;
    bExportFL.Enabled := False ;

    if NumEvents <= 0 then begin
       DisplayGrp.Visible := False ;
       Exit ;
       end
    else begin
       DisplayGrp.Visible := True ;
       end ;

    if not ckEnableSeparateADCDisplayDuration.Checked then begin
       edTADCDisplay.Enabled := False ;
       bTADCDisplayDouble.Enabled := False ;
       bTADCDisplayHalf.Enabled := False ;
       edTADCDisplay.Value := edTFLDisplay.Value ;
       end
    else begin
       edTADCDisplay.Enabled := True ;
       bTADCDisplayDouble.Enabled := True ;
       bTADCDisplayHalf.Enabled := True ;
       //edTADCDisplay.Value := edTFLDisplay.Value ;
    end ;

    MainFrm.IDRFile.EventDisplayDuration := edTFLDisplay.Value ;

    MainFrm.IDRFile.EventRatioExclusionThreshold := Round(edRatioExclusionThreshold.Value) ;
    MainFrm.IDRFile.EventRatioTop := cbNumWave.ItemIndex ;
    MainFrm.IDRFile.EventRatioBottom := cbDenomWave.ItemIndex ;
    MainFrm.IDRFile.EventRatioDisplayMax := edRatioDisplayMax.Value ;
    MainFrm.IDRFile.EventRatioRMax := edRatioRMax.Value ;
    MainFrm.IDRFile.EventFLWave := cbWavelength.ItemIndex ;
    MainFrm.IDRFile.EventF0Wave := cbdFWavelength.ItemIndex ;
    MainFrm.IDRFile.EventF0Start := Round(edF0Range.LoValue) ;
    MainFrm.IDRFile.EventF0End := Round(edF0Range.HiValue) ;
    MainFrm.IDRFile.EventF0Constant := edF0Constant.Value ;
    MainFrm.IDRFile.EventF0UseConstant := rbF0Constant.Checked ;
    MainFrm.IDRFile.EventF0DisplayMax := edDFDisplayMax.Value ;
    MainFrm.IDRFile.EventF0SubtractF0 := rbDFOverF0.Checked ;

    DisplayADCChannels(EventNum,-1,ADCBuf,scADCDisplay,ResetYRange) ;

    DisplayFLIntensity(EventNum,-1,FLBuf,scFLDisplay,ResetYRange) ;

    ckEventRejected.Checked := not EventList[EventNum-1].Accepted ;

    bExportADC.Enabled := True ;
    bExportFL.Enabled := True ;

    // Set event selector
    edEventNum.LoValue := EventNum ;
    edEventTime.Text := format('%.4g s',[EventList[EventNum-1].Time]) ;
    EventTime := EventList[EventNum-1].Time ;

    end ;


procedure TEventAnalysisFrm.DisplayADCChannels(
              StartEvent : Integer ;            // Starting event
              EndEvent : Integer ;              // End event
              var DisplayBuf : Array of SmallInt ;  // Display buffer
              scDisplay : TScopeDisplay ;        // Display plotting area
              ResetYRange : boolean               // Update Y range
              ) ;
// --------------------------------------
// Display A/D channels of detected event
// --------------------------------------
var
   iEventStart : Integer ;
   iDisplayStart : Int64 ;
   NumAvg : Integer ;
   NumSamples : Integer ;
   EventNum : Integer ;
   i,ch : Integer ;
   ySum : ^TSingleArray ;
   AverageMode : Boolean ;
begin

     if NumEvents <= 0 then Exit ;
     if MainFrm.IDRFile.ADCNumChannels <= 0 then Exit ;

     // If EndEvent < 0 then display only first event
     if EndEvent < 0 then begin
        AverageMode := False ;
        EndEvent := StartEvent ;
        end
     else AverageMode := True ;

     New(ySum) ;

     // No. of time points in A/D display window
     NumADCDisplayScans := Min( Round(edTADCDisplay.Value/MainFrm.IDRFile.ADCSCanInterval),
                                MaxDisplayScans) ;
     edTADCDisplay.Value := NumADCDisplayScans*MainFrm.IDRFile.ADCSCanInterval ;
     NumSamples := NumADCDisplayScans*MainFrm.IDRFile.ADCNumChannels ;

     scDisplay.MaxPoints := NumADCDisplayScans ;
     scDisplay.xMin := 0 ;
     scDisplay.xMax := scDisplay.MaxPoints-1 ;
     scDisplay.RecordNumber := StartEvent ;

     if ResetYRange then begin
        for ch := 0 to scDisplay.NumChannels-1 do begin
            UpdateYRange( ch, MainFrm.IDRFile.ADCChannel[ch].yMin,MainFrm.IDRFile.ADCChannel[ch].yMax,scDisplay);
            end;
        end ;

     // Allocate display data buffer
     scDisplay.SetDataBuf( @DisplayBuf ) ;

     // Clear averaging buffer and counter
     for i := 0 to NumSamples-1 do ySum[i] := 0.0 ;
     NumAvg := 0 ;

     // Calculate average
     iDisplayStart := 0 ;
     for EventNum := StartEvent to EndEvent do
         if EventList[EventNum-1].Accepted or (not AverageMode) then begin

         // Starting point of event and windows (20% pre-event)
         iEventStart := Round( EventList[EventNum-1].Time / MainFrm.IDRFile.ADCScanInterval ) ;
         iDisplayStart := iEventStart - (NumADCDisplayScans div 5) ;

         // Read A/D data into buffer
         scDisplay.NumPoints := Min(MainFrm.IDRFile.ADCNumScansInFile - iDisplayStart,NumADCDisplayScans) ;
         MainFrm.IDRFile.LoadADC( iDisplayStart,scDisplay.NumPoints,DisplayBuf);

         // Add to average buffer
         for i := 0 to NumSamples-1 do ySum[i] := ySum[i] + DisplayBuf[i] ;
         Inc(NumAvg) ;
         end ;

     // Calculate average
     if NumAvg > 0 then begin
        for i := 0 to NumSamples-1 do DisplayBuf[i] := Round(ySum[i]/NumAvg) ;
        end ;

     if (scDisplay.VerticalCursors[ADCReadoutCursor] < 0) or
        (scDisplay.VerticalCursors[ADCReadoutCursor] > scDisplay.MaxPoints) then
        scDisplay.VerticalCursors[ADCReadoutCursor] := scDisplay.MaxPoints div 5 ;

     if (scDisplay.VerticalCursors[ADCC0Cursor] < 0) or
        (scDisplay.VerticalCursors[ADCC0Cursor] > scDisplay.MaxPoints) then
        scDisplay.VerticalCursors[ADCC0Cursor] := 2 ;//Max((scDisplay.MaxPoints -2) -10,0) ;

     if (scDisplay.VerticalCursors[ADCC1Cursor] < 0) or
        (scDisplay.VerticalCursors[ADCC1Cursor] > scDisplay.MaxPoints) then
        scDisplay.VerticalCursors[ADCC1Cursor] := scDisplay.MaxPoints - 2 ;

     if AverageMode then
       scDisplay.XOffset := -(NumADCDisplayScans div 5)
     else
       scDisplay.XOffset := iDisplayStart;

     scDisplay.Invalidate ;

     if not AverageMode then begin
        edEventNum.LoValue := StartEvent ;
        edEventTime.Text := format('%.4g s',[EventList[StartEvent-1].Time]) ;
        end ;
     Dispose(ySum) ;

     end ;


procedure TEventAnalysisFrm.DisplayFLIntensity(
          StartEvent : Integer ;
          EndEvent : Integer ;
          var DisplayBuf : Array of SmallInt ;
          scDisplay : TScopeDisplay ;
          ResetYRange : boolean
          ) ;
// ----------------------------------
// Display section of ROI time course
// ----------------------------------
var
    NumPoints : Integer ;
    i : Integer ;
    y : Single ;
    F0 : Single ;
    iROI : Integer ;
    iBackg : Integer ;
    FDMode : Integer ;
    Group : Integer ;
    StartGroup : Integer ;
    EndGroup : Integer ;
    EventNum : Integer ;
    Frame : Integer ;
    NumGroupsInFile : Integer ;
    NumDisplayGroups : Integer ;
    MarkerAt : Integer ;
    TMarkerScale : Single ;
    EventAtTime : Single ;
    EventAtGroup : Integer ;
    SubtractBackground : Boolean ;
    RMin : Single ;
    RMax : Single ;
    KEff : Single ;
    yFL : ^TSingleArray ;
    NumAvg : Integer ; // No. of events averaged
    NumF0Avg : Integer ;
    F0Sum : Single ;
    F0Offset : Single ;
    AverageMode : Boolean ;
begin

    // Exit if no events or ROIs
    if NumEvents <= 0 then Exit ;
    if cbSource.Items.Count < 1 then Exit ;

     // If EndEvent < 0 then display only first event
     if EndEvent < 0 then begin
        AverageMode := False ;
        EndEvent := StartEvent ;
        end
     else AverageMode := True ;

    // Create internal buffer
    New(yFL) ;

    // Set max. number of points in display
    scDisplay.MaxPoints := Min( Round(edTFLDisplay.Value/scDisplay.TScale),
                                  MaxDisplayScans) ;
    // Force to multiple of 256
    edTFLDisplay.Value := scDisplay.MaxPoints*scDisplay.TScale ;

    NumFLDisplayScans := scDisplay.MaxPoints ;
    scDisplay.XMin := 0 ;
    scDisplay.xMax := scDisplay.MaxPoints - 1 ;
    scDisplay.RecordNumber := StartEvent ;

    // Allocate display data buffer
    scDisplay.SetDataBuf( @DisplayBuf ) ;

    // Find starting intensity point
    if MainFrm.IDRFile.LineScan then begin
       // Line scan files
       NumGroupsInFile := MainFrm.IDRFile.FrameHeight ;
       EventAtTime := EventList[StartEvent-1].Time ;
       EventAtGroup := Max(Round((EventAtTime-MainFrm.IDRFile.ImageStartDelay)/scDisplay.TScale),1) ;
       end
    else begin
       // Image files
       NumGroupsInFile := MainFrm.IDRFile.NumFrames div MainFrm.IDRFile.NumFrameTypes ;
       EventAtTime := EventList[StartEvent-1].Time ;
       EventAtGroup := Max(Round((EventAtTime-MainFrm.IDRFile.ImageStartDelay)/scDisplay.TScale),1) ;
       end ;
    NumDisplayGroups := Max(Min(scDisplay.MaxPoints,NumGroupsInFile),1) ;

    // Background ROI# to be subtracted (if any)
    iBackg := Integer(cbBackground.Items.Objects[cbBackground.ItemIndex]) ;
    if (iBackg >= 0) and (iBackg <= MainFrm.IDRFile.MaxROI) then SubtractBackground := True
                                                            else SubtractBackground := False ;

    // Fluorescence display mode
    if DisplayModePage.ActivePage = DFTAB then FDMode := FDRelativeChange
    else if DisplayModePage.ActivePage = RATIOTAB then begin
        if ckUseEquation.Checked then FDMode := FDIonConc
        else if ckDivideByRMax.Checked then FDMode := FDRatioRMax
        else FDMode := FDRatio ;
        end
    else FDMode := FDFluorescence ;

    // Calculate reference intensity for dF/F0 plot
    case FDMode of
       FDFluorescence : begin
          scDisplay.ChanScale[0] := 1.0 ;
          scDisplay.ChanName[0] := cbWavelength.Text ;
          if ResetYRange then UpdateYRange( 0, 0, Mainfrm.GreyHi[cbWavelength.ItemIndex], scDisplay ) ;
          end ;
       FDRelativeChange : begin
          if rbDFOverF0.Checked then begin
             // Plot change in fractional intensity relative to F0
             scDisplay.ChanName[0] := 'dF/F0' ;
             F0Offset := 1.0 ;
             end
          else begin
             // Plot fractional intensity relative to F0
             scDisplay.ChanName[0] := 'F/F0' ;
             F0Offset := 0.0 ;
             end ;
          scDisplay.ChanScale[0] := edDFDisplayMax.Value / scDisplay.MaxADCValue ;
          edRatioDisplayMax.Value := edRatioDisplayMax.Value ;
          if ResetYRange then UpdateYRange( 0, 0, scDisplay.MaxADCValue, scDisplay ) ;
          end ;
       FDRatio : begin
          scDisplay.ChanName[0] := cbNumWave.Text + '/' + cbDenomWave.Text ;
          scDisplay.ChanScale[0] := edRatioDisplayMax.Value / scDisplay.MaxADCValue ;
          edDFDisplayMax.Value := edRatioDisplayMax.Value ;
          if ResetYRange then UpdateYRange( 0, 0, scDisplay.MaxADCValue, scDisplay ) ;
          end ;
       FDRatioRMax : begin
          scDisplay.ChanName[0] := cbNumWave.Text + '/' + cbDenomWave.Text + '/RMax' ;
          scDisplay.ChanScale[0] := edRatioDisplayMax.Value / scDisplay.MaxADCValue ;
          edDFDisplayMax.Value := edRatioDisplayMax.Value ;
          if ResetYRange then UpdateYRange( 0, 0, scDisplay.MaxADCValue, scDisplay ) ;
          end ;
       FDIonConc : begin
          RMax := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].RMax ;
          RMin := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].RMin ;
          KEff := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Keff ;
          scDisplay.ChanName[0] := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Ion ;
          scDisplay.ChanScale[0] := edRatioDisplayMax.Value / scDisplay.MaxADCValue ;
          edDfDisplayMax.Value := edRatioDisplayMax.Value ;
          if ResetYRange then UpdateYRange( 0, 0, scDisplay.MaxADCValue, scDisplay ) ;
          end ;
       else begin
          scDisplay.ChanScale[0] := edRatioDisplayMax.Value / scDisplay.MaxADCValue ;
          end ;
       end ;

    // Clear summation buffer
    for i := 0 to High(yFL^) do yFL[i] := 0.0 ;

    // Calculate average fluorescence time course
    // ------------------------------------------
    NumAvg := 0 ;

    NumF0Avg := 0 ;
    F0Sum := 0.0 ;
    NumPoints := 0 ;
    for EventNum := StartEvent to EndEvent do
        if EventList[EventNum-1].Accepted or (not AverageMode) then begin

        // Get start/end of event
        EventAtTime := EventList[EventNum-1].Time ;
        EventAtGroup := Max(Round((EventAtTime-MainFrm.IDRFile.ImageStartDelay)
                                   /scDisplay.TScale),1) ;
        StartGroup := EventAtGroup - (NumDisplayGroups div 5) ;
        EndGroup := StartGroup + NumDisplayGroups - 1 ;

        // Calculate reference intensity
        if FDMode = FDRelativeChange then begin
           F0Sum := F0Sum + F0ReferenceIntensity( StartGroup, EndGroup ) ;
           Inc(NumF0Avg) ;
           end ;

        // Add fluorescence time course to average
        NumPoints := 0 ;
        for Group := StartGroup to EndGroup do begin

           if MainFrm.IDRFile.LineScan then begin
              Frame := Group ;
              iROI := Integer(cbSource.Items.Objects[cbSource.ItemIndex]) ;
              SubtractBackground := False ;
              end
           else begin
              Frame :=(Group-1)*MainFrm.IDRFile.NumFrameTypes + 1 ;
              Frame := Min(Max(Frame,1),MainFrm.IDRFile.NumFrames) ;
              iROI := Integer(cbSource.Items.Objects[cbSource.ItemIndex]) ;
              end ;

           case FDMode of
              FDFluorescence : begin
                 y := Intensity( iROI, Frame, cbWavelength.ItemIndex ) ;
                 if SubtractBackground then y := y - Intensity( iBackg, Frame, cbWavelength.ItemIndex ) ;
                 yFL[NumPoints] := yFL[NumPoints] + y ;
                 Inc(NumPoints) ;
                 end ;
              FDRatio,FDRatioRMax,FDIonConc : begin
                 // Ratio and ion concentration from images
                 // Numerator
                 y := Intensity( iROI, Frame, cbNumWave.ItemIndex ) ;
                 if SubtractBackground then y := y - Intensity( iBackg, Frame, cbNumWave.ItemIndex ) ;
                 yFL[NumPoints] := yFL[NumPoints] + y ;
                 Inc(NumPoints) ;
                 // Denominator
                 y := Intensity( iROI, Frame, cbDenomWave.ItemIndex ) ;
                 if SubtractBackground then y := y - Intensity( iBackg, Frame, cbDenomWave.ItemIndex ) ;
                 yFL[NumPoints] := yFL[NumPoints] + y ;
                 Inc(NumPoints) ;
                 end ;
              else begin
                 // Intensity and relative change from images
                 y := Intensity( iROI, Frame, cbDFWavelength.ItemIndex ) ;
                 if SubtractBackground then y := y - Intensity( iBackg, Frame, cbDFWavelength.ItemIndex ) ;
                 yFL[NumPoints] := yFL[NumPoints] + y ;
                 Inc(NumPoints) ;
                 end ;
              end ;
           end ;
        Inc(NumAvg) ;
        end ;

    // Calculate averages
    if NumAvg > 0 then for i := 0 to NumPoints-1 do yFL[i] := yFL[i] / NumAvg ;
    if NumF0Avg > 0 then F0 := F0Sum / NumF0Avg
                    else F0 := 0.0 ;

    case FDMode of
       FDRelativeChange : begin
          // Relative change in fluorescence (dF/F0)
          if F0 <> 0.0 then begin
             for i := 0 to NumPoints-1 do yFL[i] := (yFL[i]/F0) - F0Offset ;
             end ;
          end ;
       FDRatio,FDRatioRMax,FDIonConc : begin
          // Display fluorescence ratio (image file)
          for i := 0 to (NumPoints div 2)-1 do begin
              // Calculate ratio
              if yFL[i*2+1] > edRatioExclusionThreshold.Value then
                 yFL[i] := yFL[i*2] / yFL[i*2+1]
              else yFL[i] := 0.0 ;
              // Calculate ion conc.
              if FDMode = FDIonConc then begin
                 if yFL[i] < (RMax*0.99) then begin
                    yFL[i] := ((yFL[i] - RMin) / (RMax - yFL[i]))*KEff ;
                    end
                 else yFL[i] := 0.0 ;
                 end
              else if FDMode = FDRatioRMax then begin
                 if edRatioRMax.Value > 0.0 then begin
                    yFL[i] := yFL[i] / edRatioRMax.Value ;
                    end
                 else yFL[i] := 0.0 ;
                 end ;
              end ;
          NumPoints := NumPoints div 2 ;
          end ;
       end ;

    // Copy in display buffer
    for i := 0 to NumPoints-1 do begin
        if i <= High(DisplayBuf) then begin
           DisplayBuf[i] := Round(yFL[i]/scDisplay.ChanScale[0]) ;
           end ;
        end ;

     // Add markers (if any appear on display
     scDisplay.ClearMarkers ;
     TMarkerScale := 1.0/scDisplay.TScale ;
     for i := 0 to MainFrm.IDRFile.NumMarkers-1 do begin
         MarkerAt := Round(MainFrm.IDRFile.MarkerTime[i]*TMarkerScale)
                     - scDisplay.XOffset ;
         if (MarkerAt >= 0) and (MarkerAt < scDisplay.MaxPoints) then
            scDisplay.AddMarker( MarkerAt, MainFrm.IDRFile.MarkerText[i] );
         end ;

    scDisplay.NumPoints := NumPoints ;

    // Set cursors to default values if out of range

    if (scDisplay.VerticalCursors[FLReadoutCursor] < 0) or
        (scDisplay.VerticalCursors[FLReadoutCursor] > scDisplay.MaxPoints) then
        scDisplay.VerticalCursors[FLReadoutCursor] := EventAtGroup - StartGroup ;
    //    scDisplay.MaxPoints div 5 ;

    if (scDisplay.VerticalCursors[FLC0Cursor] < 0) or
        (scDisplay.VerticalCursors[FLC0Cursor] > scDisplay.MaxPoints) then
        scDisplay.VerticalCursors[FLC0Cursor] := 2 ; //Max((scDisplay.MaxPoints div 5) -10,0) ;

    if (scDisplay.VerticalCursors[FLC1Cursor] < 0) or
        (scDisplay.VerticalCursors[FLC1Cursor] > scDisplay.MaxPoints) then
        scDisplay.VerticalCursors[FLC1Cursor] := scDisplay.MaxPoints - 2 ;

    if AverageMode then
      scDisplay.XOffset := -(NumDisplayGroups div 5)
    else
      scDisplay.XOffset := StartGroup;

    scDisplay.Invalidate ;

    Dispose(yFL) ;

    end ;

procedure TEventAnalysisFrm.UpdateYRange(
         Chan : Integer ;                 // Channel to update
         Ymin : single ;                  // Lower limit of Y range
         YMax : single ;                  // Upper limit of Y range
         scDisplay : TScopeDisplay ) ;    // Display control to update
// -----------------------------
// Update display vertical range
// -----------------------------
begin
    scDisplay.YMin[Chan] := Ymin ;
    scDisplay.YMax[Chan] := Ymax ;
    end;


function TEventAnalysisFrm.Intensity(
         iROI : Integer ;
         FrameNum : Integer ;
         FrameType : Integer
          ) : Single ;
// ---------------------------------------------------
// Get ROI/line intensity from image or line scan file
// ---------------------------------------------------
begin

    if MainFrm.IDRFile.LineScan then begin
       Result := ViewLineFrm.TimeCourseIntensity( FrameNum,FrameType+1 ) ;
       end
    else begin
       Result := ViewPlotFrm.ROIIntensity( iROI, FrameNum, FrameType ) ;
       end ;
    end ;


function TEventAnalysisFrm.F0ReferenceIntensity(
          StartGroup : Integer ;
          EndGroup : Integer
          ) : Single ;
//
// Return reference fluorescence intensity (F0)
// --------------------------------------------
var
    iROI : Integer ;
    iBackg : Integer ;
    Group : Integer ;
    y : Single ;
    Sum : Single ;
    nAvg : Integer ;
    StartF0 : Integer ;
    EndF0 : Integer ;
    SubtractBackground : Boolean ;
    FrameNum : Integer ;
begin

    if rbF0FromFrames.Checked then begin

       // Calculate F0 reference intensity from select series of frames
       iROI := Integer(cbSource.Items.Objects[cbSource.ItemIndex]) ;
       iBackg := Integer(cbBackground.Items.Objects[cbBackground.ItemIndex]) ;
       if (iBackg >= 0) and (iBackg <= MainFrm.IDRFile.MaxROI) then SubtractBackground := True
                                                               else SubtractBackground := False ;

       Sum := 0.0 ;
       nAvg := 0 ;
       StartF0 := Round(edF0Range.LoValue) + StartGroup ;
       EndF0 := Round(edF0Range.HiValue) + StartGroup ;

       for Group := StartF0 to EndF0 do begin

           if MainFrm.IDRFile.LineScan then begin
              FrameNum := Group ;
//              iROI := Integer(cbSource.Items.Objects[cbSource.ItemIndex]) ;
              SubtractBackground := False ;
              end
           else begin
              FrameNum := (Group-1)*MainFrm.IDRFile.NumFrameTypes + 1 ;
              FrameNum := Min(Max(FrameNum,1),MainFrm.IDRFile.NumFrames-1) ;
//              iROI := Integer(cbSource.Items.Objects[cbSource.ItemIndex]) ;
              end ;

           y := Intensity( iROI, FrameNum, cbDFWavelength.ItemIndex  ) ;
           if SubtractBackground then y := y - Intensity( iBackg, FrameNum, cbDFWavelength.ItemIndex ) ;

           // Add to summation
           Sum := Sum + y ;

           Inc(nAvg) ;
           end ;

        Result := Round( Sum/Max(nAvg,1) ) ;

        end
    else begin
         // Constant F0 value
         Result := Round(edF0Constant.Value) ;
         end ;

    end ;


procedure TEventAnalysisFrm.bDetectEventsClick(Sender: TObject);
// --------------------------------------------------------
// Locate position of events within selected signal channel
// --------------------------------------------------------
var
    i : Integer ;
    iPolarity : Integer ;
    ExceedCount : Integer ;
    yDiff : Integer ;
    yThreshold : Integer ;
    Done : Boolean ;
    yBaseline : Integer ;
    iDetectedAt : Integer ;
    NumExceedCountsRequired : Integer ;
    RBAddFraction : Single ;
    RollingBaseline : Single ;
    StatusUpdateCounter : Integer ;
    EventDetected : Boolean ;
begin

     // Allow user to clear event list
     if NumEvents > 0 then begin
        if MessageDlg( 'Clear event list? ', mtConfirmation,
        [mbYes,mbNo], 0 ) = mrYes then NumEvents := 0 ;
        end ;

     bDetectEvents.Enabled := False ;
     bStop.Enabled := True ;

     if DetectEventsRunning then Exit ;

     DetectEventsRunning := True ;
     StopDetectEventsRunning := False ;

     scADCDisplay.StorageMode := False ;
     scFLDisplay.StorageMode := False ;

     sbDetDisplay.Position := sbDetDisplay.Min ;

     iPolarity := cbDetectionThresholdPolarity.ItemIndex ;
     yThreshold := Abs(Round(edThresholdLevel.Value)) ;
     NumExceedCountsRequired := Max(Round(edThresholdDuration.Value),1) ;
     yBaseline := Round(edFixedBaselineLevel.Value) + scDetDisplay.ChanZero[0] ;

     i := 0 ;
     ExceedCount := 0 ;
     NumEvents := 0 ;
     sbDetDisplay.Position := sbDetDisplay.Min ;
     RollingBaseline := DetBaseLineCursorY ;
     iDetectedAt := 0 ;
     Done := False ;
     DisplayDetChannel ;
     StatusUpdateCounter := 0 ;
     EventDetected := False ;
     while not Done do begin

         // Update buffer and display
         if i >= scDetDisplay.MaxPoints then begin
            sbDetDisplay.Position := sbDetDisplay.Position + scDetDisplay.MaxPoints ;
            i := i - scDetDisplay.MaxPoints ;
            DisplayDetChannel ;
            end ;

         // Update rolling average baseline
         if rbRollingBaseline.Checked then begin
            RBAddFraction := scDetDisplay.TScale/edRollingBaselinePeriod.Value ;
            RollingBaseline := (1.0-RBAddFraction)*RollingBaseline +  RBAddFraction*DetBuf[i] ;
            yBaseline := Round(RollingBaseline) ;
            end ;

         // Has signal exceeded threshold ?
         yDiff := DetBuf[i] - yBaseline ;
         case iPolarity of
              PositiveGoing : begin
                  if yDiff >= yThreshold then Inc(ExceedCount)
                                         else ExceedCount := 0 ;
                  end ;
              NegativeGoing : begin
                  if yDiff <= (-yThreshold) then Inc(ExceedCount)
                                         else ExceedCount := 0 ;
                  end ;
              else begin
                  if Abs(yDiff) >= Abs(yThreshold) then Inc(ExceedCount)
                                                   else ExceedCount := 0 ;
                  end ;
              end ;

        //
        if ExceedCount = 1 then iDetectedAt := i ;
        if ExceedCount >= NumExceedCountsRequired then begin
           // Event detected
           EventList[NumEvents].Time := (iDetectedAt + sbDetDisplay.Position)*scDetDisplay.TScale ;
           EventList[NumEvents].Accepted := True ;
           if NumEvents < High(EventList) then Inc(NumEvents) ;
           sbEventNum.Max := Max(NumEvents,1) ;
           sbEventNum.Position :=  sbEventNum.Max ;
           i := iDetectedAt + Round(edDeadTime.Value/scDetDisplay.TScale) ;
           // Set pointer to start detecting again after dead time
           ExceedCount := 0 ;
           Application.ProcessMessages ;
           EventDetected := True ;
           end ;

        // Next point
        Inc(i) ;

        Inc(StatusUpdateCounter) ;
        if (StatusUpdateCounter >= 1000) or EventDetected then begin
           edDetectStatus.Text := format('%.0f/%.0fs Events %d',
                                  [(i + sbDetDisplay.Position)*scDetDisplay.TScale,
                                   sbDetDisplay.Max*scDetDisplay.TScale,
                                   NumEvents]) ;
           // Update baseline/threshold cursors
           DetBaseLineCursorY := yBaseline ;
           DetPosThresholdCursorY :=  yBaseline + yThreshold ;
           DetNegThresholdCursorY :=  yBaseline - yThreshold ;
           UpdateDetCursors(False) ;
           StatusUpdateCounter := 0 ;
           EventDetected := False ;
        end;


        if (sbDetDisplay.Position + i) >= sbDetDisplay.Max then Done := True ;
        if not bStop.Enabled then Done := True ;

        end ;

     sbEventNum.Max := Max(NumEvents,1) ;
     edEventNum.HiLimit := sbEventNum.Max ;
     edAvgRange.HiLimit := sbEventNum.Max ;
     edAvgRange.LoValue := 1 ;
     edEventNum.HiValue := sbEventNum.Max ;
     edAvgRange.HiValue := sbEventNum.Max ;

     edPlotRange.HiValue := sbEventNum.Max ;
     edPlotRange.HiLimit := sbEventNum.Max ;
     edPlotRange.LoValue := 1 ;

     // Save event list to file
     SaveEventList ;
     LoadEventList ;

     bDetectEvents.Enabled := True ;
     bStop.Enabled := False ;
     DetectEventsRunning := False ;

     scADCDisplay.StorageMode := ckSuperimposeEvents.Checked ;
     scFLDisplay.StorageMode := ckSuperimposeEvents.Checked ;

     // Display first event
     if NumEvents > 0 then begin
        Page.ActivePage := ViewTab ;
        sbEventNum.Position := 1 ;
        end ;

     end;


procedure TEventAnalysisFrm.cbDetectionSourceChange(Sender: TObject);
// ------------------------
// Detection source changed
// ------------------------
begin
     //Update detection display settings
     UpdateDetDisplay( True ) ;
     end ;


procedure TEventAnalysisFrm.sbEventNumChange(Sender: TObject);
// ------------------------------
// Event selection slider changed
// ------------------------------
begin
     ResetCursors ;
     DisplayEvent( sbEventNum.Position, False ) ;
     end;


procedure TEventAnalysisFrm.edEventNumKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------
// Displayed event number changed
// ------------------------------
begin
     if Key = #13 then begin
        sbEventNum.Position := Round(EdEventNum.LoValue) ;
        DisplayEvent( sbEventNum.Position, False ) ;
        end ;
     end;


procedure TEventAnalysisFrm.edTADCDisplayKeyPress(Sender: TObject;
  var Key: Char);
// -------------------------------
// ADC Display duration changed
// -------------------------------
begin
     if Key = #13 then begin
        ResetCursors ;
        ckSuperimposeEvents.Checked := False ;
        DisplayEvent( sbEventNum.Position, False ) ;

        end ;
     end;

     
procedure TEventAnalysisFrm.edTFLDisplayKeyPress(Sender: TObject;
  var Key: Char);
// -------------------------------
// ROI Display duration changed
// -------------------------------
begin
     if Key = #13 then begin
        ResetCursors ;
        ckSuperimposeEvents.Checked := False ;
        DisplayEvent( sbEventNum.Position, False ) ;
        end ;
     end;


procedure TEventAnalysisFrm.SetupFluorescenceDisplay ;
// --------------------------------------------
// Set up for selected fluoresence display mode
// --------------------------------------------
var
    FDMode : Integer ;
begin

    // Fluorescence display mode
    if DisplayModePage.ActivePage = FTAB then FDMode := FDFluorescence
    else if DisplayModePage.ActivePage = DFTAB then FDMode := FDRelativeChange
    else if DisplayModePage.ActivePage = RATIOTAB then begin
        if ckUseEquation.Checked then FDMode := FDIonConc
                                 else FDMode := FDRatio ;
        end ;

     // Display ratio and ion conc. modes if only one wavelength available
     if (MainFrm.IDRFile.NumFrameTypes < 2) then begin
        if (FDMode = FDRatio) or (FDMode = FDIonConc) then begin
           FDMode := FDFluorescence ;
//           cbFluorescenceDisplay.ItemIndex := cbFluorescenceDisplay.Items.IndexOfObject(
//                                              TObject(FDMode)) ;
           end ;
        end ;

     // Update variables/channels available on X-Y plot page
     UpdatePlotVariables ;

     end;


procedure TEventAnalysisFrm.cbFluorescenceDisplayChange(Sender: TObject);
//  ---------------------------------
// Fluorescence display mode changed
// ---------------------------------
begin
     SetupFluorescenceDisplay ;
     DisplayEvent( sbEventNum.Position, True ) ;
    end;


procedure TEventAnalysisFrm.scADCDisplayCursorChange(Sender: TObject);
// --------------------------------
// A/D channel display cursor moved
// --------------------------------
var
    ch : Integer ;
    ADCChannel : TChannel ;
begin

    // Update main display Y limits
    for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
        ADCChannel := MainFrm.IDRFile.ADCChannel[ch] ;
        ADCChannel.YMax :=  scADCDisplay.YMax[ch] ;
        ADCChannel.YMin :=  scADCDisplay.YMin[ch] ;
        ADCChannel.InUse := scADCDisplay.ChanVisible[ch] ;
        MainFrm.IDRFile.ADCChannel[ch] := ADCChannel ;
        end ;

     // Give this control focus to avoid left/right cursor control arrow keys
     // from changing other controls
     edEventTime.SetFocus ;

     end ;


procedure TEventAnalysisFrm.CopyImageToClipboard ;
{ -------------------------------------------
  Copy image to clipboard as Windows metafile
  ------------------------------------------- }
var
      ScopeDisp : TScopeDisplay ;
begin

     if Page.ActivePage = PlotTab then begin
        // Copy X-Y graph to clipboard
        PrintGraphFrm.Plot := plPlot ;
        PrintGraphFrm.MultiYPlot := False ;
        PrintGraphFrm.ToPrinter := False ;
        PrintGraphFrm.ShowModal ;
        if PrintGraphFrm.ModalResult = mrOK then plPlot.CopyImageToClipboard ;
        end
     else begin
        PrintRecFrm.Destination := deClipboard ;
        if (Page.ActivePage = AverageTab) then begin
           // Copy average record
           if scAvgFLDisplay.DisplaySelected then ScopeDisp := scAvgFLDisplay
                                             else ScopeDisp := scAvgADCDisplay ;
           end
        else begin
           // Copy record
           if scFLDisplay.DisplaySelected then ScopeDisp := scFLDisplay
                                          else ScopeDisp := scADCDisplay ;
           end ;

        PrintRecFrm.DisplayObj :=  ScopeDisp ;
        PrintRecFrm.ShowModal ;
        if PrintRecFrm.ModalResult = mrOK then begin
           ScopeDisp.ClearPrinterTitle ;
           ScopeDisp.AddPrinterTitleLine( 'File : ' + MainFrm.IDRFile.FileName ) ;
           ScopeDisp.AddPrinterTitleLine( MainFrm.IDRFile.Ident ) ;
           ScopeDisp.CopyImageToClipboard ;
           end ;
        end ;

     end ;


procedure TEventAnalysisFrm.Print ;
{ -------------------------------------------
  Print image
  ------------------------------------------- }
var
      ScopeDisp : TScopeDisplay ;
begin

     if Page.ActivePage = PlotTab then begin
        // Copy X-Y graph to clipboard
        PrintGraphFrm.Plot := plPlot ;
        PrintGraphFrm.MultiYPlot := False ;
        PrintGraphFrm.ToPrinter := True ;
        PrintGraphFrm.ShowModal ;
        if PrintGraphFrm.ModalResult = mrOK then begin
           plPlot.ClearPrinterTitle ;
           plPlot.AddPrinterTitleLine( 'File : ' + MainFrm.IDRFile.FileName ) ;
           plPlot.AddPrinterTitleLine( MainFrm.IDRFile.Ident ) ;
           plPlot.Print ;
           end ;
        end
     else begin
        PrintRecFrm.Destination := dePrinter ;
        if (Page.ActivePage = AverageTab) then begin
           // Copy average record
           if scAvgFLDisplay.DisplaySelected then ScopeDisp := scAvgFLDisplay
                                             else ScopeDisp := scAvgADCDisplay ;
           end
        else begin
           // Copy record
           if scFLDisplay.DisplaySelected then ScopeDisp := scFLDisplay
                                          else ScopeDisp := scADCDisplay ;
           end ;

        PrintRecFrm.DisplayObj :=  ScopeDisp ;
        PrintRecFrm.ShowModal ;
        if PrintRecFrm.ModalResult = mrOK then begin
           ScopeDisp.ClearPrinterTitle ;
           ScopeDisp.AddPrinterTitleLine( 'File : ' + MainFrm.IDRFile.FileName ) ;
           ScopeDisp.AddPrinterTitleLine( MainFrm.IDRFile.Ident ) ;
           ScopeDisp.Print ;
           end ;
        end ;

     end ;


procedure TEventAnalysisFrm.scFLDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ---------------------------------
// Mouse button depressed over graph
// ---------------------------------
begin

     // Select ADC display for copy/print (deselect others)
     scFLDisplay.DisplaySelected := True ;
     scFLDisplay.Invalidate ;
     scADCDisplay.DisplaySelected := False ;
     scADCDisplay.Invalidate ;

     end;


procedure TEventAnalysisFrm.scADCDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ---------------------------------
// Mouse button depressed over graph
// ---------------------------------
begin
     // Select ADC display for copy/print (deselect others)
     scFLDisplay.DisplaySelected := False ;
     scFLDisplay.Invalidate ;
     scADCDisplay.DisplaySelected := True ;
     scADCDisplay.Invalidate ;

   end;


function TEventAnalysisFrm.PlotAvailable : Boolean ;
// --------------------------------------------------------------
// Return TRUE if selected graph is available for copying/printing
// ---------------------------------------------------------------
begin

    Result := False ;
    if Page.ActivePage = PlotTab then Result := plPlot.Available
    else begin
        if scFLDisplay.DisplaySelected
           and (scFLDisplay.NumPoints > 0) then Result := True
        else if scADCDisplay.DisplaySelected
           and (scADCDisplay.NumPoints > 0) then Result := True ;
        end ;

    end ;


procedure TEventAnalysisFrm.CopyDataToClipboard ;
// -----------------------------------------------------------------
// Copy the data in currently displayed graph to the clipboard
// -----------------------------------------------------------------
begin

     if Page.ActivePage = ViewTab then begin
           if scFLDisplay.DisplaySelected then scFLDisplay.CopyDataToClipboard
                                          else scADCDisplay.CopyDataToClipboard ;
        end;

     if Page.ActivePage = AverageTab then begin
           if scAvgFLDisplay.DisplaySelected then scAvgFlDisplay.CopyDataToClipboard
                                          else scAvgADCDisplay.CopyDataToClipboard ;
        end;

     if Page.ActivePage = PlotTab then begin
        plPlot.CopyDataToClipboard ;
        end ;

     end ;


procedure TEventAnalysisFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ----------
// Close form
// ----------
begin
     Action := caFree ;

    // Save stored settings
    MainFrm.IDRFile.EventDisplayDuration := edTDetDisplay.Value ;

    MainFrm.IDRFile.EventDeadTime := edDeadTime.Value ;
    MainFrm.IDRFile.EventDetectionThreshold := edThresholdLevel.Value ;
    MainFrm.IDRFile.EventThresholdDuration := edThresholdDuration.Value ;
    MainFrm.IDRFile.EventDetectionThresholdPolarity := cbDetectionThresholdPolarity.ItemIndex ;
    MainFrm.IDRFile.EventDetectionSource := cbDetectionSource.ItemIndex ;
    MainFrm.IDRFile.EventROI := cbSource.ItemIndex ;
    MainFrm.IDRFile.EventBackgROI := cbBackground.ItemIndex ;
    MainFrm.IDRFile.EventFixedBaseline := rbFixedBaseline.Checked ;
    MainFrm.IDRFile.EventRollingBaselinePeriod := edRollingBaselinePeriod.Value ;
    MainFrm.IDRFile.EventBaselineLevel := Round(edFixedBaselineLevel.Value) ;
    MainFrm.IDRFile.EventRatioDisplayMax := edDetRatioDisplayMax.Value ;

    MainFrm.IDRFile.EventRatioExclusionThreshold := Round(edRatioExclusionThreshold.Value) ;
    MainFrm.IDRFile.EventRatioTop := cbNumWave.ItemIndex ;
    MainFrm.IDRFile.EventRatioBottom := cbDenomWave.ItemIndex ;
    MainFrm.IDRFile.EventRatioDisplayMax := edRatioDisplayMax.Value ;
    MainFrm.IDRFile.EventRatioRMax := edRatioRMax.Value ;
    MainFrm.IDRFile.EventFLWave := cbWavelength.ItemIndex ;
    MainFrm.IDRFile.EventF0Wave := cbdFWavelength.ItemIndex ;
    MainFrm.IDRFile.EventF0Start := Round(edF0Range.LoValue) ;
    MainFrm.IDRFile.EventF0End := Round(edF0Range.HiValue) ;
    MainFrm.IDRFile.EventF0Constant := edF0Constant.Value ;
    MainFrm.IDRFile.EventF0UseConstant := rbF0Constant.Checked ;
    MainFrm.IDRFile.EventF0DisplayMax := edDFDisplayMax.Value ;
    MainFrm.IDRFile.EventF0SubtractF0 := rbDFOverF0.Checked ;

     end;

procedure TEventAnalysisFrm.bExportADCClick(Sender: TObject);
// --------------------------------------
// Export A/D channels of detected events
// --------------------------------------
begin
     ExportEventsFrm.ExportADC := True ;
     ExportEventsFrm.Display := scADCDisplay ;
     ExportEventsFrm.ShowModal ;

     end;

procedure TEventAnalysisFrm.bExportFLClick(Sender: TObject);
// ----------------------------------------------
// Export fluorescence channel of detected events
// ----------------------------------------------
begin
     ExportEventsFrm.ExportADC := False ;
     ExportEventsFrm.Display := scFLDisplay ;
     ExportEventsFrm.ShowModal ;
     end;


procedure TEventAnalysisFrm.DisplayADCChannelAverages ;
// ----------------------------------------------------------
// Display A/D channel averages for selected range of events
// ----------------------------------------------------------
var
   StartAtEvent : Integer ;
   EndAtEvent : Integer ;
   ch : Integer ;

begin

     if NumEvents <= 0 then Exit ;
     if MainFrm.IDRFile.ADCNumChannels <= 0 then Exit ;

     if rbAvgAllEvents.Checked then begin
        StartAtEvent := 1 ;
        EndAtEvent := NumEvents ;
        end
     else begin
        StartAtEvent := Round(edAvgRange.LoValue) ;
        EndAtEvent := Round(edAvgRange.HiValue) ;
        end ;

     scAvgADCDisplay.MaxADCValue := scADCDisplay.MaxADCValue ;
     scAvgADCDisplay.MinADCValue := scADCDisplay.MinADCValue ;

     // Add zero level cursors
     scAvgADCDisplay.ClearHorizontalCursors ;
     for ch :=  0 to MainFrm.IDRFile.ADCNumChannels-1 do
           scAvgADCDisplay.AddHorizontalCursor( ch, clBlue, True, 'z' ) ;

     // No. of time points in A/D display window
     scAvgADCDisplay.MaxPoints := scADCDisplay.MaxPoints ;
     scAvgADCDisplay.NumPoints := scADCDisplay.NumPoints ;
     scAvgADCDisplay.xMin := scADCDisplay.xMin ;
     scAvgADCDisplay.xMax := scADCDisplay.xMax ;

     scAvgADCDisplay.NumChannels := scADCDisplay.NumChannels ;

     scAvgADCDisplay.xMin := 0 ;
     scAvgADCDisplay.TScale := scADCDisplay.TScale ;
     scAvgADCDisplay.TUnits := scADCDisplay.TUnits ;

     { Set channel information }
     for ch := 0 to scADCDisplay.NumChannels-1 do begin
         scAvgADCDisplay.ChanOffsets[ch] := scADCDisplay.ChanOffsets[ch] ;
         scAvgADCDisplay.ChanUnits[ch] := scADCDisplay.ChanUnits[ch] ;
         scAvgADCDisplay.ChanName[ch] := scADCDisplay.ChanName[ch] ;
         scAvgADCDisplay.ChanScale[ch] := scADCDisplay.ChanScale[ch] ;
         scAvgADCDisplay.yMin[ch] := scADCDisplay.yMin[ch] ;
         scAvgADCDisplay.yMax[ch] := scADCDisplay.yMax[ch] ;
         scAvgADCDisplay.HorizontalCursors[ch] := scADCDisplay.HorizontalCursors[ch] ;
         scAvgADCDisplay.ChanVisible[ch] := scADCDisplay.ChanVisible[ch] ;
         end ;

     scAvgADCDisplay.ClearVerticalCursors ;
     AvgADCReadoutCursor := scAvgADCDisplay.AddVerticalCursor(-1,clGreen,'?y') ;
     scAvgADCDisplay.VerticalCursors[AvgADCReadoutCursor] := scAvgADCDisplay.MaxPoints div 5 ;

     // Get A/D channel data for this event (held in ADCBuf)
     DisplayADCChannels(StartAtEvent,EndAtEvent,AvgADCBuf,scAvgADCDisplay,false) ;

     scAvgADCDisplay.VerticalCursors[0] := scADCDisplay.VerticalCursors[0] ;
     scAvgADCDisplay.Invalidate ;

     end ;


procedure TEventAnalysisFrm.DisplayFLIntensityAverages ;
// --------------------------------------------------------------------
// Display fluorescence intensity averages for selected range of events
// --------------------------------------------------------------------
var
   StartAtEvent : Integer ;
   EndAtEvent : Integer ;
   ch : Integer ;

begin

     if NumEvents <= 0 then Exit ;

     if rbAvgAllEvents.Checked then begin
        StartAtEvent := 1 ;
        EndAtEvent := NumEvents ;
        end
     else begin
        StartAtEvent := Round(edAvgRange.LoValue) ;
        EndAtEvent := Round(edAvgRange.HiValue) ;
        end ;

     scAvgFLDisplay.MaxADCValue := scFLDisplay.MaxADCValue ;
     scAvgFLDisplay.MinADCValue := scFLDisplay.MinADCValue ;

     // Add zero level cursors
     scAvgFLDisplay.ClearHorizontalCursors ;
     scAvgFLDisplay.AddHorizontalCursor( 0, clBlue, True, 'z' ) ;

     // No. of time points in A/D display window
     scAvgFLDisplay.MaxPoints := scFLDisplay.MaxPoints ;
     scAvgFLDisplay.NumPoints := scFLDisplay.NumPoints ;
     scAvgFLDisplay.xMin := scFLDisplay.xMin ;
     scAvgFLDisplay.xMax := scFLDisplay.xMax ;

     scAvgFLDisplay.NumChannels := scFLDisplay.NumChannels ;

     scAvgFLDisplay.xMin := 0 ;
     scAvgFLDisplay.TScale := scFLDisplay.TScale ;
     scAvgFLDisplay.TUnits := scFLDisplay.TUnits ;

     { Set channel information }
     for ch := 0 to scFLDisplay.NumChannels-1 do begin
         scAvgFLDisplay.ChanOffsets[ch] := scFLDisplay.ChanOffsets[ch] ;
         scAvgFLDisplay.ChanUnits[ch] := scFLDisplay.ChanUnits[ch] ;
         scAvgFLDisplay.ChanName[ch] := scFLDisplay.ChanName[ch] ;
         scAvgFLDisplay.ChanScale[ch] := scFLDisplay.ChanScale[ch] ;
         scAvgFLDisplay.yMin[ch] := scFLDisplay.yMin[ch] ;
         scAvgFLDisplay.yMax[ch] := scFLDisplay.yMax[ch] ;
         scAvgFLDisplay.HorizontalCursors[ch] := scFLDisplay.HorizontalCursors[ch] ;
         scAvgFLDisplay.ChanVisible[ch] := scFLDisplay.ChanVisible[ch] ;
         end ;

     scAvgFLDisplay.ClearVerticalCursors ;
     AvgFLReadoutCursor := scAvgFLDisplay.AddVerticalCursor(-1,clGreen,'?y') ;
     scAvgFLDisplay.VerticalCursors[AvgFLReadoutCursor] := scFLDisplay.VerticalCursors[FLReadoutCursor] ;

     DisplayFLIntensity(StartAtEvent,EndAtEvent,AvgFLBuf,scAvgFLDisplay,true) ;

     scAvgFLDisplay.VerticalCursors[0] := scFLDisplay.VerticalCursors[0] ;
     scAvgFLDisplay.Invalidate ;

     end ;


procedure TEventAnalysisFrm.bAverageEventsClick(Sender: TObject);
// -----------------------------------------------------
// Display averages of A/D channel and fluorescence data
// -----------------------------------------------------
begin

   DisplayADCChannelAverages ;
   DisplayFLIntensityAverages ;

   end;


procedure TEventAnalysisFrm.ckEventRejectedClick(Sender: TObject);
// --------------------------------------
// Event accepted/rejected status changed
// --------------------------------------
begin
     EventList[sbEventNum.Position-1].Accepted := not ckEventRejected.Checked ;

     // Save event list to file
     SaveEventList ;

     end;


procedure TEventAnalysisFrm.cbSourceClick(Sender: TObject);
// ------------------
// Source ROI changed
// ------------------
begin
     DisplayEvent( sbEventNum.Position, false ) ;
     end;


procedure TEventAnalysisFrm.cbBackgroundClick(Sender: TObject);
// ----------------------
// Background ROI changed
// -----------------------
begin
     DisplayEvent( sbEventNum.Position, false ) ;
     end;

procedure TEventAnalysisFrm.scAvgFlDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ---------------------------------
// Mouse button depressed over graph
// ---------------------------------
begin

     // Select fluorescence display for copy/print (deselect others)
     scAvgFLDisplay.DisplaySelected := True ;
     scAvgFLDisplay.Invalidate ;
     scAvgADCDisplay.DisplaySelected := False ;
     scAvgADCDisplay.Invalidate ;

     end;

procedure TEventAnalysisFrm.scAvgADCDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ---------------------------------
// Mouse button depressed over graph
// ---------------------------------
begin
        // Select ADC display for copy/print (deselect others)
     scAvgFLDisplay.DisplaySelected := False ;
     scAvgFLDisplay.Invalidate ;
     scAvgADCDisplay.DisplaySelected := True ;
     scAvgADCDisplay.Invalidate ;

   end;


procedure TEventAnalysisFrm.UpdatePlotVariables ;
// ---------------------------------------
// Update list of available plot variables
// ---------------------------------------
var
    i : Integer ;
begin

    // Waveform measurement variables
    VarNames[vEventNum] := 'Event No.' ;
    VarNames[vTime] := 'Time' ;
    VarNames[vInterval] := 'Interval ' ;
    VarNames[vFrequency] := 'Frequency' ;
    VarNames[vPeak] := 'Peak' ;
    VarNames[vArea] := 'Area' ;
    VarNames[vCursor] := 'Cursor' ;
    VarNames[vTRise] := 'T.rise' ;
    VarNames[vTDecay90] := 'T(90%)' ;
    VarNames[vTauDecay] := 'Tau(decay)' ;
    VarNames[vDuration] := 'Duration' ;
    VarNames[vBaseline] := 'Baseline' ;

    // Create X & Y variable lists
    cbXVar.Clear ;
    for i := 0 to MaxVar do cbXVar.Items.Add( VarNames[i] ) ;
    cbYVar.Items.Assign( cbXVar.Items ) ;
    cbXVar.ItemIndex := PlotXVar ;
    cbYVar.ItemIndex := PlotYVar ;

    // X & Y variable source lists
    cbXSource.Clear ;
    cbXSource.Items.Add( scFLDisplay.ChanName[0] ) ;

    // Add A/D channels
    for i := 0 to scADCDisplay.NumChannels-1 do
        cbXSource.Items.Add( scADCDisplay.ChanName[i] ) ;
    cbYSource.Items.Assign( cbXSource.Items ) ;
    cbXSource.ItemIndex := PlotXSource ;
    cbYSource.ItemIndex := PlotYSource ;

    // Displayconfiguration controls appropriate to variable
    if PlotXVar = vPeak then rgXPeakPolarity.Visible := True
                        else rgXPeakPolarity.Visible := False ;
    panXNumAvg.Visible := not rgXPeakPolarity.Visible ;

    case PlotXVar of
         vPeak : begin
            rgXPeakPolarity.Visible := True ;
            panXNumAvg.Visible := False ;
            end ;
         vCursor,vBaseline : Begin
            rgXPeakPolarity.Visible := False ;
            panXNumAvg.Visible := True ;
            end ;
         else begin
            rgXPeakPolarity.Visible := False ;
            panXNumAvg.Visible := False ;
            end ;
         end ;

    case PlotYVar of
         vPeak : begin
            rgYPeakPolarity.Visible := True ;
            panYNumAvg.Visible := False ;
            end ;
         vCursor,vBaseline : Begin
            rgYPeakPolarity.Visible := False ;
            panYNumAvg.Visible := True ;
            end ;
         else begin
            rgYPeakPolarity.Visible := False ;
            panYNumAvg.Visible := False ;
            end ;
         end ;

    end ;


procedure TEventAnalysisFrm.PlotGraph ;
// ------------------------------------
// Plot X-Y graph of selected variables
// ------------------------------------
var
     StartAtEvent : Integer ;
     EndAtEvent : Integer ;
     EventNum : Integer ;
     XOK,YOK : Boolean ;
begin

     if NumEvents <= 0 then Exit ;

     bPlotGraph.Enabled := False ;
     bPlotGraphStop.Enabled := True ;

     // Select range of events to be plotted
     if rbPlotAllEvents.Checked then begin
        StartAtEvent := 1 ;
        EndAtEvent := NumEvents ;
        end
     else begin
        StartAtEvent := Round(edPlotRange.LoValue) ;
        EndAtEvent := Round(edPlotRange.HiValue) ;
        end ;

     plPlot.MaxPointsPerLine := MaxDisplayScans ;
     plPlot.ClearAllLines ;
     plPlot.CreateLine( 0 , clBlue, msOpenSquare, psSolid ) ;

     { Plot graph of currently selected variables }

     plPlot.xAxisAutoRange := True ;
     plPlot.yAxisAutoRange := True ;

     // Set axis labels
     plPlot.XAxisLabel := PlotAxisLabel( cbXVar, cbXSource ) ;
     plPlot.YAxisLabel := PlotAxisLabel( cbYVar, cbYSource ) ;

     for EventNum := StartAtEvent to EndAtEvent do
         if EventList[EventNum-1].Accepted and bPlotGraphStop.Enabled then begin

         // Read in sample data for event
         sbEventNum.Position :=  EventNum ;
         DisplayEvent( sbEventNum.Position, false ) ;

         // Calculate X axis variables
         XOK := AnalyseEvent( XAxis,
                       cbXVar.ItemIndex,
                       cbXSource.ItemIndex,
                       EventNum,
                       Round(edXNumAvg.Value),
                       rgXPeakPolarity.ItemIndex ) ;

         // Calculate Y axis variables
         YOK := AnalyseEvent( YAxis,
                       cbYVar.ItemIndex,
                       cbYSource.ItemIndex,
                       EventNum,
                       Round(edYNumAvg.Value),
                       rgYPeakPolarity.ItemIndex ) ;

         if XOK and YOK then
            plPlot.AddPoint( 0,
                             Variables[cbXVar.ItemIndex,XAxis],
                             Variables[cbYVar.ItemIndex,YAxis] ) ;

         edPlotStatus.Text := format('Events %d/%d',[EventNum,EndAtEvent]);
         Application.ProcessMessages ;

         end ;

     plPlot.VerticalCursors[0] := (plPlot.XAxisMin + plPlot.XAxisMax)*0.5 ;

     bPlotGraph.Enabled := True ;
     bPlotGraphStop.Enabled := False ;

     end ;


Function TEventAnalysisFrm.PlotAxisLabel(
         cbVar : TComboBox ;
         cbSource : TComboBox
         ) : String ;
// ---------------------------------------------
// Return label for selected variable and source
// ---------------------------------------------
var
    s : string ;
begin

    s := cbVar.Text ;

    case cbVar.ItemIndex of
         vTRise,vTDecay90,vTauDecay,vDuration,vCursor,vPeak,vBaseline,vArea : begin
             s := s + ' ' + cbSource.Text ;
             end ;
         end ;

    case cbVar.ItemIndex of
         vInterval,vTRise,vTDecay90,vTauDecay,vDuration : begin
            s := s + ' s' ;
            end ;
         vFrequency : begin
            s := s + ' Hz' ;
            end ;
         vCursor,vPeak,vBaseline : begin
            if cbSource.ItemIndex > 0 then
               s := s + ' ' + scADCDisplay.ChanUnits[cbSource.ItemIndex-1] ;
            end ;
         vArea : begin
            if cbSource.ItemIndex > 0 then
               s := s + ' ' + scADCDisplay.ChanUnits[cbSource.ItemIndex-1] + '.s' ;
            end ;
         end ;
    Result := s ;
    end ;


Function TEventAnalysisFrm.AnalyseEvent(
          SelectedAxis : Integer ;    // Plot Axis
          iVar : Integer ;            // Variable no.
          iSource : Integer ;         // Signal source
          EventNum : Integer ;        // Event #
          NumAvgPoints : Integer ;     // No. of points in averages
          PeakPolarity : Integer     // Peak detection polarity
                                      // 0=Pos,1=Neg,2=Absolute
          ) : Boolean ;
// -----------------------------------
// Calculate event waveform parameters
// -----------------------------------
var
     i,j : Integer ;        // Counters
     YScale : Single ;      // Signal scale factor
     YZero : Integer ;      // Signal zero level
     iChan : Integer ;      // Selected channel
     NumPoints : Integer ;  // No. of points in analysis buffer
     dt : Single ;          // Inter-point time interval (s)
     ReadoutPoint : Integer ;    // Readout cursor point
     StartPoint : Integer ;      // Start of analysis area
     EndPoint : Integer ;        // End of analysis area

     Polarity : Integer ;   // Event polarity (1=positive-going,-1=negative-)
     Sum : Single ;         // Summation variable
     nAvg : Integer ;       // No. samples actually averaged
     Y : Single ;           // A/D sample value
     YBaseline : Single ;   // Signal baseline level before event
     YPeak : Single ;       // Peak amplitide
     PeakAt : Integer ;     // Scan at which peak occurs
     Y90 : Single ;         // 90% of peak
     Y50 : Single ;         // 50% of peak
     Y10 : Single ;         // 10% of peak
     Num10to90PercentPeak : Integer ;    // No. of scans within event rising phase
     Num10toPeak : Integer ;             // No. rising phase scans from 10% to peak
     EventTime : Single ;                // Event detection time
     Interval : Single ;                 // Time since last event
     Frequency : Single ;                // Event frequency
     T : Single ;          //
     SumT : Single ;        // Summation variables for time constant fit
     SumT2 : Single ;         //
     SumY : Single ;         //
     SumYT : Single ;         //
     nPoints : Integer ;
     Slope : Single ;
     TauDecay : Single ;     // Decay time constant

     Buf : ^TSingleArray ;

     Done : Boolean ;
begin

   New(Buf) ;

   Result := True ;

   // Load selected signal into work buffer

   iChan := iSource - 1 ;
   if iChan < 0 then begin
      // Load fluorescence signal into buffer
      YScale := scFLDisplay.ChanScale[0] ;
      YZero := scFLDisplay.ChanZero[0] ;
      NumPoints := scFLDisplay.NumPoints ;
      ReadoutPoint := scFLDisplay.VerticalCursors[FLReadOutCursor] ;
      StartPoint := scFLDisplay.VerticalCursors[FLC0Cursor] ;
      EndPoint := scFLDisplay.VerticalCursors[FLC1Cursor] ;
      dt := scFLDisplay.TScale ;
      for i := 0 to NumPoints-1 do begin
          Buf[i] := (FLBuf[i] - YZero)*YScale;
          end ;
      end
   else begin
      // Load selected A/D signal into buffer
      YScale := scADCDisplay.ChanScale[iChan] ;
      YZero := scADCDisplay.ChanZero[iChan] ;
      dt := scADCDisplay.TScale ;
      NumPoints := scADCDisplay.NumPoints ;
      ReadoutPoint := scADCDisplay.VerticalCursors[ADCReadOutCursor] ;
      StartPoint := scADCDisplay.VerticalCursors[ADCC0Cursor] ;
      EndPoint := scADCDisplay.VerticalCursors[ADCC1Cursor] ;
      j := scADCDisplay.ChanOffsets[iChan] ;
      for i := 0 to scADCDisplay.NumPoints-1 do begin
          Buf[i] := (ADCBuf[j] - YZero)*YScale ;
          j := j + scADCDisplay.NumChannels ;
          end ;
      end ;

   // Event number
   Variables[vEventNum,SelectedAxis] := EventNum ;

   // Time of detection
   EventTime := EventList[EventNum-1].Time  ;
   Variables[vTime,SelectedAxis] := EventTime ;

   // Time interval since previous event
   if EventNum > 1 then Interval := EventTime - EventList[EventNum-2].Time
                   else Interval := -1.0 ;
   Variables[vInterval,SelectedAxis] := Interval  ;
   if (iVar = vInterval) and (Interval < 0.0) then Result := False ;

   // Instantaneous frequency of event occurence
   if Interval > 0.0 then Frequency := 1.0 / Interval
                     else Frequency := -1.0 ;
   Variables[vFrequency,SelectedAxis] := Frequency  ;
   if (iVar = vFrequency) and (Frequency < 0.0) then Result := False ;

   // Pre-event baseline level
   Sum := 0.0 ;
   for i := 0 to NumAvgPoints-1 do Sum := Sum + Buf[i] ;
   YBaseline := Sum / Max(NumAvgPoints,1) ;
   Variables[vBaseline,SelectedAxis] := YBaseline ;

   // Peak amplitude
   case PeakPolarity of
        pPositivePeak : YPeak := -1E30 ;
        pNegativePeak : YPeak := 1E30 ;
        else YPeak := 0.0 ;
        end ;
   PeakAt := 0 ;

   for i := Min(StartPoint,EndPoint) to Max(StartPoint,EndPoint) do begin

       Y := Buf[i] ;

       // Find peak value
       case PeakPolarity of
            pPositivePeak : begin
               if Y > YPeak then begin
                  YPeak := Y ;
                  PeakAt := i ;
                  end ;
               end ;
            pNegativePeak : begin
               if Y < YPeak then begin
                  YPeak := Y ;
                  PeakAt := i ;
                  end ;
               end ;
            else begin
               if Abs(Y) > Abs(YPeak) then begin
                  YPeak := Y ;
                  PeakAt := i ;
                  end ;
               end ;
            end ;
       end ;

   Variables[vPeak,SelectedAxis] := YPeak ;

   // Integral of signal within analysis area
   for i := StartPoint to EndPoint do Sum := Sum + Buf[i] ;
   Variables[vArea,SelectedAxis] := Sum ;

   // Average of points around readout cursor
   nAvg := 0 ;
   Sum := 0.0 ;
   for i := Max(ReadoutPoint-NumAvgPoints,0) to
            Min(ReadoutPoint+NumAvgPoints,NumPoints-1) do begin
       Sum := Sum + Buf[i] ;
       Inc(nAvg) ;
       end ;
   Variables[vCursor,SelectedAxis] := Sum / Max(nAvg,1) ;

   // Find rise time
   i := PeakAt ;
   Y10 := Abs(YPeak - YBaseline)*0.1 ;
   Y90 := Abs(YPeak - YBaseline)*0.9 ;
   Polarity := Sign(YPeak - YBaseline) ;
   Num10to90PercentPeak := 0 ;
   Num10toPeak := 0 ;
   Done := False ;
   while not Done do begin
       Y := Polarity*(Buf[i] - YBaseline) ;
       if Y <= Y90 then Inc(Num10to90PercentPeak) ;
       Inc(Num10toPeak) ;
       Dec(i) ;
       if (Y < Y10) or (i<=0) then Done := True ;
       end ;
   Variables[vTRise,SelectedAxis] := Num10to90PercentPeak*dt ;

   // Find time of 90% decay
   i := PeakAt ;
   Y10 := Abs(YPeak - YBaseline)*0.1 ;
   Polarity := Sign(YPeak - YBaseline) ;
   Done := False ;
   while not Done do begin
       Y := Polarity*(Buf[i] - YBaseline) ;
       Inc(i) ;
       if (Y <= Y10) or (i>=NumPoints) then Done := True ;
       end ;
   Variables[vTDecay90,SelectedAxis] := (i-PeakAt)*dt ;

   // Calculate decay time constant
   i := PeakAt ;
   Y10 := Abs(YPeak - YBaseline)*0.1 ;
   T := 0.0 ;
   SumT := 0.0 ;
   SumT2 := 0.0 ;
   SumY := 0.0 ;
   SumYT := 0.0 ;
   nPoints := 0 ;
   Done := False ;
   while not Done do begin
      Y := Polarity*(Buf[i] - YBaseline) ;
      if Y >= Y10 then begin
         Y := Ln(Y) ;
         SumT := SumT + T ;
         SumT2 := SumT2 + T*T ;
         SumY := SumY + Y ;
         SumYT := SumYT + Y*T ;
         Inc(nPoints) ;
         end
      else Done := True ;
      Inc(i) ;
      T := T + dt ;
      if i >= NumPoints then Done := True ;
      end ;
   if nPoints > 1 then begin
      Slope := ((nPoints*SumYT) - (SumT*SumY)) /
               ((nPoints*SumT2) - (SumT*SumT)) ;
      if Slope < 0.0 then TauDecay := (-1.0/Slope)
                     else TauDecay := 0.0 ;
      end
   else TauDecay := 0.0 ;
   Variables[vTauDecay,SelectedAxis] := TauDecay ;

   Variables[vDuration,SelectedAxis] := Variables[vTDecay90,SelectedAxis] +
                                        Variables[vTRise,SelectedAxis] ;

   Dispose(Buf) ;

   end ;


procedure TEventAnalysisFrm.bPlotGraphClick(Sender: TObject);
// --------------
// Plot X-Y Graph
// --------------
begin
     if NumEvents > 0 then PlotGraph ;
     end;


procedure TEventAnalysisFrm.bPlotGraphStopClick(Sender: TObject);
begin
     bPlotGraphStop.Enabled := False ;
     end;

procedure TEventAnalysisFrm.cbXVarChange(Sender: TObject);
begin
     PlotXVar := cbXVar.ItemIndex ;
     UpdatePlotVariables ;
     end;


procedure TEventAnalysisFrm.cbYVarChange(Sender: TObject);
begin
     PlotYVar := cbYVar.ItemIndex ;
     UpdatePlotVariables ;
     end;


procedure TEventAnalysisFrm.cbXSourceChange(Sender: TObject);
begin
     PlotXSource := cbXSource.ItemIndex ;
     end;


procedure TEventAnalysisFrm.cbYSourceChange(Sender: TObject);
begin
     PlotYSource := cbYSource.ItemIndex ;
     end;


procedure TEventAnalysisFrm.PageChange(Sender: TObject);
// --------------------
// Page tab has changed
// --------------------
begin

     if Page.ActivePage = DetectTab then begin
        DisplayDetChannel ;
        end
     else if Page.ActivePage = AverageTab then begin
        scAvgFLDisplay.DisplaySelected := True ;
        end
     else if Page.ActivePage = ViewTab then begin
        scFLDisplay.DisplaySelected := True ;
        DisplayEvent( sbEventNum.Position, true ) ;
        end
     else if Page.ActivePage = PlotTab then begin
        UpdatePlotVariables ;
        end ;

     end;


procedure TEventAnalysisFrm.bSetAxesClick(Sender: TObject);
//  --------------------------------------------
// Customise plot axes range, labels and styles
// --------------------------------------------
begin
     PlotSetAxesFrm.Plot := plPlot ;
     PlotSetAxesFrm.ShowModal ;
     end;


procedure TEventAnalysisFrm.bStopClick(Sender: TObject);
begin
     bStop.Enabled := False ;
     end;

procedure TEventAnalysisFrm.cbWavelengthChange(Sender: TObject);
// --------------------------
// Display wavelength changed
// --------------------------
begin
    DisplayEvent( sbEventNum.Position, true ) ;
    end ;


procedure TEventAnalysisFrm.ckSuperimposeEventsClick(Sender: TObject);
// -------------------------------
// Superimpose event option on/off
// -------------------------------
begin
     scADCDisplay.StorageMode := ckSuperimposeEvents.Checked ;
     scFLDisplay.StorageMode := ckSuperimposeEvents.Checked ;
     end;

procedure TEventAnalysisFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
// -----------------------------------
// Check to see if form can be closed
// -----------------------------------
begin
     if DetectEventsRunning then begin
        StopDetectEventsRunning := True ;
        CanClose := False ;
        end
     else CanClose := True ;
     end;


procedure TEventAnalysisFrm.edTDisplayKeyPress(Sender: TObject;
  var Key: Char);
// -------------------------------------
// Fluorescence Display duration changed
// -------------------------------------
var
    i : Integer ;
begin
     if Key = #13 then begin
        // Set cursor to -1 to force reset to defaults
        for i := 0 to scFLDisplay.NumVerticalCursors-1 do
            scFLDisplay.VerticalCursors[i] := -1 ;
        DisplayEvent( sbEventNum.Position, false ) ;
        end ;
     end;


procedure TEventAnalysisFrm.bTFLDisplayHalfClick(Sender: TObject);
// -------------------------------------
// Halve Fluorescence Display duration
// -------------------------------------
begin
     edTFLDisplay.Value := edTFLDisplay.Value*0.5 ;
     ResetCursors ;
     ckSuperimposeEvents.Checked := False ;
     DisplayEvent( sbEventNum.Position, false ) ;
     end;

procedure TEventAnalysisFrm.bTFLDisplayDoubleClick(Sender: TObject);
// -------------------------------------
// Double Fluorescence Display duration
// -------------------------------------
begin
     edTFLDisplay.Value := edTFLDisplay.Value*2.0 ;
     ResetCursors ;
     ckSuperimposeEvents.Checked := False ;
     DisplayEvent( sbEventNum.Position, false ) ;
     end;


procedure TEventAnalysisFrm.ResetCursors ;
// Set cursor to -1 to force reset to defaults
// -------------------------------------------
var
    i : Integer ;
begin

     for i := 0 to scFLDisplay.NumVerticalCursors-1 do
         scFLDisplay.VerticalCursors[i] := -1 ;
     for i := 0 to scADCDisplay.NumVerticalCursors-1 do
         scADCDisplay.VerticalCursors[i] := -1 ;

     end ;

procedure TEventAnalysisFrm.bTADCDisplayDoubleClick(Sender: TObject);
// -------------------------------------
// Double A/D Display duration
// -------------------------------------
begin
     edTADCDisplay.Value := edTADCDisplay.Value*2.0 ;
     ResetCursors ;
     ckSuperimposeEvents.Checked := False ;
     DisplayEvent( sbEventNum.Position, false ) ;

     end;

procedure TEventAnalysisFrm.bTADCDisplayHalfClick(Sender: TObject);
// -------------------------------------
// Halve A/D Display duration
// -------------------------------------
begin
     edTADCDisplay.Value := edTADCDisplay.Value*0.5 ;
     ResetCursors ;
     ckSuperimposeEvents.Checked := False ;
     DisplayEvent( sbEventNum.Position, false ) ;

     end;


procedure TEventAnalysisFrm.MagnifyChannelDisplay(
          ChanNum : Integer ) ;
// ------------------------------------
// Magnify selected A/D channel display
// ------------------------------------
begin
     if ChanNum >= MainFrm.IDRFile.ADCNumChannels then begin
       scFLDisplay.YZoom(ChanNum - MainFrm.IDRFile.ADCNumChannels, -50.0) ;
       scAvgFLDisplay.YZoom(ChanNum - MainFrm.IDRFile.ADCNumChannels, -50.0) ;
       end
     else begin
       scADCDisplay.YZoom(ChanNum, -50.0);
       scAvgADCDisplay.YZoom(ChanNum, -50.0);
       end ;
     end ;


procedure TEventAnalysisFrm.ReduceChannelDisplay( ChanNum : Integer ) ;
// ------------------------------------
// Reduce selected A/D channel display
// ------------------------------------
begin
     if ChanNum >= MainFrm.IDRFile.ADCNumChannels then begin
       scFLDisplay.YZoom(ChanNum - MainFrm.IDRFile.ADCNumChannels, 50.0) ;
       scAvgFLDisplay.YZoom(ChanNum - MainFrm.IDRFile.ADCNumChannels, 50.0) ;
       end
     else begin
       scADCDisplay.YZoom(ChanNum, 50.0);
       scAvgADCDisplay.YZoom(ChanNum, 50.0);
       end ;
     end ;



procedure TEventAnalysisFrm.FormActivate(Sender: TObject);
// -----------------------
// Form is now active form
// -----------------------
begin
     DisplayEvent( sbEventNum.Position, false ) ;
     end;

procedure TEventAnalysisFrm.s(Sender: TObject;
  var Key: Char);
// ----------------------
// dF/F0 constant changed
// ----------------------
var
    i : Integer ;
begin
     if Key = #13 then begin
        // Set cursor to -1 to force reset to defaults
        for i := 0 to scFLDisplay.NumVerticalCursors-1 do
            scFLDisplay.VerticalCursors[i] := -1 ;
        DisplayEvent( sbEventNum.Position, false ) ;
        end ;
     end;

procedure TEventAnalysisFrm.edF0RangeKeyPress(Sender: TObject;
  var Key: Char);
// ----------------------
// F0 range changed
// ----------------------
var
    i : Integer ;
begin
     if Key = #13 then begin
        // Set cursor to -1 to force reset to defaults
        for i := 0 to scFLDisplay.NumVerticalCursors-1 do
            scFLDisplay.VerticalCursors[i] := -1 ;
        DisplayEvent( sbEventNum.Position, true ) ;
        end ;
     end;


procedure TEventAnalysisFrm.rbF0FromFramesClick(Sender: TObject);
// -----------------------
// F0 from frames selected
// -----------------------
var
    i : Integer ;
begin
     // Set cursor to -1 to force reset to defaults
     for i := 0 to scFLDisplay.NumVerticalCursors-1 do
         scFLDisplay.VerticalCursors[i] := -1 ;
     DisplayEvent( sbEventNum.Position, false ) ;
     end;


procedure TEventAnalysisFrm.rbF0ConstantClick(Sender: TObject);
// ----------------------
// dF/F0 constant changed
// ----------------------
var
    i : Integer ;
begin
     // Set cursor to -1 to force reset to defaults
     for i := 0 to scFLDisplay.NumVerticalCursors-1 do
         scFLDisplay.VerticalCursors[i] := -1 ;
     DisplayEvent( sbEventNum.Position, false ) ;
     end;


procedure TEventAnalysisFrm.DisplayModePageChange(Sender: TObject);
begin
     DisplayEvent( sbEventNum.Position, true ) ;
     end;

procedure TEventAnalysisFrm.DisplayModePageChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
     if DisplayModePage.ActivePage = FTab then
        cbdFWavelength.ItemIndex := cbWavelength.ItemIndex
     else if DisplayModePage.ActivePage = DFTab then
        cbWavelength.ItemIndex := cbDFWavelength.ItemIndex ;
     end;

procedure TEventAnalysisFrm.cbNumWaveChange(Sender: TObject);
begin
     cbDetFluorRatioTop.ItemIndex := cbNumWave.ItemIndex ;
     DisplayEvent( sbEventNum.Position, false ) ;
     end;

procedure TEventAnalysisFrm.cbDenomWaveChange(Sender: TObject);
begin
     cbDetFluorRatioBottom.ItemIndex := cbDenomWave.ItemIndex ;
     DisplayEvent( sbEventNum.Position, false ) ;
     end;

procedure TEventAnalysisFrm.ckUseEquationClick(Sender: TObject);
begin
     if ckUseEquation.Checked then ckDivideByRmax.Checked := False ;
     DisplayEvent( sbEventNum.Position, false ) ;
     end;

procedure TEventAnalysisFrm.ckDivideByRMaxClick(Sender: TObject);
begin
     if ckDivideByRmax.Checked then ckUseEquation.Checked := False ;
     DisplayEvent( sbEventNum.Position, false ) ;
     end;


procedure TEventAnalysisFrm.edRatioRMaxKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then DisplayEvent( sbEventNum.Position, false ) ;
     end;

procedure TEventAnalysisFrm.rbDFOverF0Click(Sender: TObject);
begin
     DisplayEvent( sbEventNum.Position, false ) ;
     end;

procedure TEventAnalysisFrm.rbFOverF0Click(Sender: TObject);
begin
     DisplayEvent( sbEventNum.Position, false ) ;
     end;

procedure TEventAnalysisFrm.SetDisplayGrid( Value : Boolean ) ;
// ------------------------------------
// Set chart display grid on/off
// ------------------------------------
begin
    scFLDisplay.DisplayGrid := Value ;
    scADCDisplay.DisplayGrid := Value ;
    scDetDisplay.DisplayGrid := Value ;
    scAvgFLDisplay.DisplayGrid := Value ;
    scAvgADCDisplay.DisplayGrid := Value ;
    end ;


function TEventAnalysisFrm.GetDisplayGrid : Boolean ;
// ------------------------------------
// Get chart display grid on/off state
// ------------------------------------
begin
    Result := scFLDisplay.DisplayGrid ;
    end ;


procedure TEventAnalysisFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
// --------------------
//  Process key presses
// --------------------
var
    ScopeDisp : TScopeDisplay ;
begin

     // Get selected display
     if Page.ActivePage = ViewTab then begin
        if scFLDisplay.DisplaySelected then ScopeDisp := scFLDisplay
                                       else  ScopeDisp := scADCDisplay ;
        end
     else begin
        if scAvgFLDisplay.DisplaySelected then ScopeDisp := scAvgFLDisplay
                                       else  ScopeDisp := scAvgADCDisplay ;
        end ;

     case key of
          VK_LEFT : ScopeDisp.MoveActiveVerticalCursor(-1) ;
          VK_RIGHT : ScopeDisp.MoveActiveVerticalCursor(1) ;
          end ;

     end;

procedure TEventAnalysisFrm.scFLDisplayCursorChange(Sender: TObject);
begin

     // Give this control focus to avoid left/right cursor control arrow keys
     // from changing other controls
     edEventTime.SetFocus ;

     end ;

procedure TEventAnalysisFrm.scAvgFlDisplayCursorChange(Sender: TObject);
begin

     // Give this control focus to avoid left/right cursor control arrow keys
     // from changing other controls
     edAvgRange.SetFocus ;

     end ;


procedure TEventAnalysisFrm.scAvgADCDisplayCursorChange(Sender: TObject);
begin

     // Give this control focus to avoid left/right cursor control arrow keys
     // from changing other controls
     edAvgRange.SetFocus ;

     end ;


procedure TEventAnalysisFrm.ZoomOutAll ;
// --------------------------------------
// Set all plots to minimum magnification
// --------------------------------------
var
    i : Integer ;
begin

    // Set fluorescence plots
    for i := 0 to scFLDisplay.NumChannels-1 do begin
        scFLDisplay.YMax[i] := scFLDisplay.MaxADCValue ;
        scFLDisplay.YMin[i] := 0.0 ;
        scAvgFLDisplay.YMax[i] := scFLDisplay.MaxADCValue ;
        scAvgFLDisplay.YMin[i] := 0.0 ;
        end ;
    scFLDisplay.Invalidate ;
    scAvgFLDisplay.Invalidate ;

    scADCDisplay.ZoomOut ;
    scAvgADCDisplay.ZoomOut ;

    end ;


procedure TEventAnalysisFrm.sbDetDisplayChange(Sender: TObject);
// --------------------------------------------
// Detection channel display scroll bar changed
// --------------------------------------------
begin
    DisplayDetChannel ;
    end;

procedure TEventAnalysisFrm.edTDetDisplayKeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------------------
// Detection display window duration changed
// -----------------------------------------
begin

    if Key = #13 then begin
       MainFrm.IDRFile.EventDisplayDuration := edTDetDisplay.Value ;
       UpdateDetDisplay( False ) ;
       end ;
    end;

procedure TEventAnalysisFrm.scDetDisplayCursorChange(Sender: TObject);
// -----------------------------------
// Cursor changed on detection display
// -----------------------------------
begin

      // If baseline cursor changed ... move thresholds
      if DetBaselineCursorY <> scdetDisplay.HorizontalCursors[DetBaselineCursor] then begin
         DetBaselineCursorY := scdetDisplay.HorizontalCursors[DetBaselineCursor] ;
         case cbDetectionThresholdPolarity.ItemIndex of
             PositiveGoing : begin
                DetPosThresholdCursorY := DetBaselineCursorY + Abs(Round(edThresholdLevel.Value)) ;
                scdetDisplay.HorizontalCursors[DetPosThresholdCursor] := DetPosThresholdCursorY ;
                end ;
             NegativeGoing : begin
                DetNegThresholdCursorY := DetBaselineCursorY - Abs(Round(edThresholdLevel.Value)) ;
                scdetDisplay.HorizontalCursors[DetNegThresholdCursor] := detNegThresholdCursorY ;
                end ;
             PosNegGoing : begin
                DetPosThresholdCursorY := DetBaselineCursorY + Abs(Round(edThresholdLevel.Value)) ;
                scdetDisplay.HorizontalCursors[DetPosThresholdCursor] := DetPosThresholdCursorY ;
                DetNegThresholdCursorY := DetBaselineCursorY - Abs(Round(edThresholdLevel.Value)) ;
                scdetDisplay.HorizontalCursors[DetNegThresholdCursor] := detNegThresholdCursorY ;
                end ;
             end ;

         edFixedBaselineLevel.value := (scdetDisplay.HorizontalCursors[DetBaselineCursor]
                                       - scDetDisplay.ChanZero[0]);

         end ;

      // If positive threshold cursor changed ...
      case cbDetectionThresholdPolarity.ItemIndex of
           PositiveGoing,PosNegGoing : begin
              if DetPosThresholdCursorY <> scdetDisplay.HorizontalCursors[DetPosThresholdCursor] then begin
                 DetPosThresholdCursorY := scdetDisplay.HorizontalCursors[DetPosThresholdCursor] ;
                 edThresholdLevel.Value := Abs(Max(DetPosThresholdCursorY - DetBaselineCursorY,0)) ;
                 DetPosThresholdCursorY := DetBaselineCursorY + Abs(Round(edThresholdLevel.Value)) ;
                 scdetDisplay.HorizontalCursors[DetPosThresholdCursor] := DetPosThresholdCursorY ;
                 if cbDetectionThresholdPolarity.ItemIndex = PosNegGoing then begin
                    DetNegThresholdCursorY := DetBaselineCursorY - Round(edThresholdLevel.Value) ;
                    scdetDisplay.HorizontalCursors[DetNegThresholdCursor] := DetNegThresholdCursorY ;
                    end ;
                 end ;
              end ;
           end ;

      // If Negative threshold cursor changed ...
      case cbDetectionThresholdPolarity.ItemIndex of
           NegativeGoing,PosNegGoing : begin
              if DetNegThresholdCursorY <> scdetDisplay.HorizontalCursors[DetNegThresholdCursor] then begin
                 DetNegThresholdCursorY := scdetDisplay.HorizontalCursors[DetNegThresholdCursor] ;
                 edThresholdLevel.Value := Abs(Min(DetNegThresholdCursorY - DetBaselineCursorY,0)) ;
                 DetNegThresholdCursorY := DetBaselineCursorY - Abs(Round(edThresholdLevel.Value)) ;
                 scdetDisplay.HorizontalCursors[DetNegThresholdCursor] := DetNegThresholdCursorY ;
                 if cbDetectionThresholdPolarity.ItemIndex = PosNegGoing then begin
                    DetPosThresholdCursorY := DetBaselineCursorY  + Round(edThresholdLevel.Value);
                    scdetDisplay.HorizontalCursors[DetPosThresholdCursor] := DetPosThresholdCursorY ;
                    end ;
                end ;
             end ;
           end ;

      end;


procedure TEventAnalysisFrm.cbDetROIChange(Sender: TObject);
// ---------------------
// Detection ROI changed
// ---------------------
begin
     cbSource.ItemIndex := cbDetROI.ItemIndex ;
     UpdateDetDisplay( False ) ;
     end;

procedure TEventAnalysisFrm.cbDetBackgROIClick(Sender: TObject);
// ---------------------
// Background ROI changed
// ---------------------
begin
     cbBackground.ItemIndex := cbDetBackgROI.ItemIndex ;
     UpdateDetDisplay( False ) ;
     end;


procedure TEventAnalysisFrm.cbDetFluorChange(Sender: TObject);
// -------------------------------------
// Excitation wavelength channel changed
// -------------------------------------
begin
     UpdateDetDisplay( True ) ;
     end;


procedure TEventAnalysisFrm.cbDetectionThresholdPolarityChange(
  Sender: TObject);
// ---------------------------
// Threshold polarity changed
// ---------------------------
begin
     UpdateDetDisplay( False ) ;
     end;

procedure TEventAnalysisFrm.cbFluorescenceChange(Sender: TObject);
// ---------------------------------------
// Event fluorescence display mode changed
// ---------------------------------------
begin
      case cbFluorescence.ItemIndex of
          0 : DisplayModePage.ActivePage := FTab ;
          1 : DisplayModePage.ActivePage := RatioTab ;
          2 : DisplayModePage.ActivePage := dFTab ;
          end ;

       DisplayEvent( sbEventNum.Position, true ) ;

       end;

procedure TEventAnalysisFrm.bTDetDisplayHalfClick(Sender: TObject);
// -----------------------------------
// Halve duration of detection display
// -----------------------------------
begin
     edTDetDisplay.Value := 0.5*edTDetDisplay.Value ;
     MainFrm.IDRFile.EventDisplayDuration := edTDetDisplay.Value ;
     UpdateDetDisplay( False ) ;
     end;

procedure TEventAnalysisFrm.bDetDisplayDoubleClick(Sender: TObject);
// -----------------------------------
// Double duration of detection display
// -----------------------------------
begin
     edTDetDisplay.Value := 2.0*edTDetDisplay.Value ;
     MainFrm.IDRFile.EventDisplayDuration := edTDetDisplay.Value ;
     UpdateDetDisplay( False ) ;
     end;


procedure TEventAnalysisFrm.FormCreate(Sender: TObject);
begin
    DetDisplayInitialised := False ;
    end;


procedure TEventAnalysisFrm.ckEnableSeparateADCDisplayDurationClick(
  Sender: TObject);
// --------------------------------------
// Event display duration locking changed
// --------------------------------------
begin

    SetEventDisplayLimit ;
    DisplayEvent( sbEventNum.Position, false ) ;
    
    end;

procedure TEventAnalysisFrm.edDetRatioDisplayMaxKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------------
// Detection ratio display maximum changed
// ---------------------------------------
begin
    if Key = #13 then begin
       edRatioDisplayMax.Value := edDetRatioDisplayMax.Value ;
       UpdateDetDisplay( True ) ;
       end ;
    end ;

    
procedure TEventAnalysisFrm.cbDetFluorRatioTopChange(Sender: TObject);
// ------------------------------------------
// Detection : Fluorescence ratio top changed
// ------------------------------------------
begin
    cbNumWave.ItemIndex := cbDetFluorRatioTop.ItemIndex ;
    UpdateDetDisplay( True ) ;
    end;

    
procedure TEventAnalysisFrm.cbDetFluorRatioBottomChange(Sender: TObject);
// ------------------------------------------
// Detection : Fluorescence ratio bottom changed
// ------------------------------------------
begin
    cbDenomWave.ItemIndex := cbDetFluorRatioBottom.ItemIndex ;
    UpdateDetDisplay( True ) ;
    end;

procedure TEventAnalysisFrm.eddetRatioexclusionThresholdKeyPress(
  Sender: TObject; var Key: Char);
// ------------------------------------------
// Detection : Fluorescence ratio min. signal level changed
// ------------------------------------------
begin
    if Key = #13 then begin
       edRatioExclusionThreshold.Value := edDetRatioExclusionThreshold.Value ;
       UpdateDetDisplay( False ) ;
       end ;
    end;


procedure TEventAnalysisFrm.edThresholdLevelKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then begin
        case cbDetectionThresholdPolarity.ItemIndex of
             PositiveGoing : begin
                DetPosThresholdCursorY := DetBaselineCursorY + Abs(Round(edThresholdLevel.Value)) ;
                scdetDisplay.HorizontalCursors[DetPosThresholdCursor] := DetPosThresholdCursorY ;
                end ;
             NegativeGoing : Begin
                DetNegThresholdCursorY := DetBaselineCursorY - Abs(Round(edThresholdLevel.Value)) ;
                scdetDisplay.HorizontalCursors[DetNegThresholdCursor] := DetNegThresholdCursorY ;
                end ;
             PosNegGoing : Begin
                DetPosThresholdCursorY := DetBaselineCursorY + Abs(Round(edThresholdLevel.Value)) ;
                DetNegThresholdCursorY := DetBaselineCursorY - Abs(Round(edThresholdLevel.Value)) ;
                scdetDisplay.HorizontalCursors[DetPosThresholdCursor] := DetPosThresholdCursorY ;
                scdetDisplay.HorizontalCursors[DetNegThresholdCursor] := DetNegThresholdCursorY ;
                end ;
             end ;
        end ;
     end;

procedure TEventAnalysisFrm.edRatioDisplayMaxKeyPress(Sender: TObject;
  var Key: Char);
// -------------------------------
// ROI Display duration changed
// -------------------------------
begin
     if Key = #13 then begin
        edDetRatioDisplayMax.Value := edRatioDisplayMax.Value ;
        ResetCursors ;
        ckSuperimposeEvents.Checked := False ;
        DisplayEvent( sbEventNum.Position, false ) ;
        end ;
     end;

procedure TEventAnalysisFrm.edRatioExclusionThresholdKeyPress(
  Sender: TObject; var Key: Char);
begin
     if Key = #13 then begin
        edDetRatioExclusionThreshold.Value := edRatioExclusionThreshold.Value ;
        DisplayEvent( sbEventNum.Position, false ) ;
        end ;
     end;

procedure TEventAnalysisFrm.edFixedBaselineLevelKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then begin
        scdetDisplay.HorizontalCursors[DetBaselineCursor] := Round(edFixedBaselineLevel.Value) ;
        end ;
     end;

procedure TEventAnalysisFrm.cbSourceChange(Sender: TObject);
// ------------------
// Source ROI changed
// ------------------
begin
     cbDetROI.ItemIndex := cbSource.ItemIndex ;
     DisplayEvent( sbEventNum.Position, false ) ;
     end;

procedure TEventAnalysisFrm.cbBackgroundChange(Sender: TObject);
// ----------------------
// Background ROI changed
// -----------------------
begin
     cbDetBackgROI.ItemIndex := cbBackground.ItemIndex ;
     DisplayEvent( sbEventNum.Position, false ) ;
     end;
procedure TEventAnalysisFrm.cbEquationChange(Sender: TObject);
// ----------------------------
// Ion-binding Equation changed
// ----------------------------
begin
     DisplayEvent( sbEventNum.Position, false ) ;
     end;

end.
