unit ViewPlotUnit;
// =======================================================
// WinFluor - Stored image ROI time course plotting module
// =======================================================
// 14.03.06
// 29.01.07 DisplayGrid property added
// 31.01.07 Buffer overwrite bug when 8 A/D channels used fixed
// 20.02.07 ZoomOutAll method added
// 01.04.07 Internal NumFrameTypes now used
// 19.07.07 Ratio display added
//          Zoom display magnification buttons added
// 22.07.07 Vertical cursors can now be moved using left/right arrow keys
// 10.06.08 Buffer pointers now all set Nil after FreeMem to fix memory allocation
//          error when form is closed
// 08.09.09 JD User-defined polyline and polygon ROIs added
// 10.09.09 JD Pixel Exclusion facility removed from MeanROIIntensity
//          JD ROITimeCourseBufEmptyFlag changed from 0 to -(High(Integer)-1) ;
//          MeanROIIntensity Now handles negative fluorescence values and implements intensity
//          scaling factors and offset
// 18.09.09 JD Fluorescence time course buffer now correctly scales
//          ratio intensities from ratio images
// 02.09.10 JD Cursors now show signal levels correctly (after earlier changed to AddVerticalCursors)
// 20.10.10 JD Display max. duration limit now 20000s
// 30.07.12 JD Size of ROITimeCourseBuf now allocated to number of ROIs in use
// 17.09.12 JD ROITimeCourseBuf size increased to hold MainFrm.IDRFile.MaxROIInUse+1 ROIs
//             (rather than MainFrm.IDRFile.MaxROIInUse) to avoid memory access violations
// 13.11.12 ... .LOADADC() now uses 64 bit scan counter
// 16.01.15 ... ROI time calculation skipped if no ROIs defined
// 16.09.15 .. JD Form position/size saved by MainFrm.SaveFormPosition() when form closed
// 24.09.15 .. JD Min/Max display compression now implemented in scADCDisplay component rather than this form.
//             ADCBuf,FLDisplayBuf & RDisplayBuf now allocated dynamically
// 24.08.16 .. JD Time course plot data now stored in <filename>.TCB file to avoid
//             recomputing plot every time window is opened.
// 27.10.16 .. JD ROI time course now stored and display in Single floating point.
// 31.10.16 .. JD Mean intensity now computes fractional grey level units
// 07.12.16 .. JD Whole of time course empty flag now filled with empty flag
//             which has been chznged to a floating point -65536.0. Multi-waavelength
//             time courses now calculated correctly again.
// 04.01.18 .. JD Heap memory now allocated with AllocMem instead to GetMem to initialise to zero
// 20.03.21 .. JD A/D display start and readout cursor now made same as fluorescence display when displayed frame changed
// 22.03.21 .. JD Time course computations now carried out by thread rather than Timer component
//                CSV data table containing ROI time courses automatically created
// 10.10.22 .. JD NewFile now called when ROI time couse computation completed to reload timw course buffer

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ExtCtrls, ScopeDisplay, HTMLLabel,IDRFile,
  math, strutils, mmsystem, excsetupunit, labiounit, ViewPlotThread ;

Const
    GreyLevelLimit = $FFFF ;
    MaxADCChannels = 8 ;
    MaxFrames = 100000 ;

type

  TROIPixelList = Array[0..4096*4096] of TPoint ;
  PROIPixelList = ^TROIPixelList ;

  TViewPlotFrm = class(TForm)
    ckFixZeroLevels: TCheckBox;
    TDisplayPanel: TPanel;
    edTDisplay: TValidatedEdit;
    rbTDisplayUnitMins: TRadioButton;
    rbTDisplayUnitsSecs: TRadioButton;
    bTDisplayDouble: TButton;
    bTDisplayHalf: TButton;
    sbDisplay: TScrollBar;
    FluorGrp: TGroupBox;
    cbROI: TComboBox;
    RatioGrp: TGroupBox;
    Label3: TLabel;
    ckDisplayR: TCheckBox;
    cbDenominator: TComboBox;
    cbNumerator: TComboBox;
    edRDisplayMax: TValidatedEdit;
    ADCGrp: TGroupBox;
    ckDisplayADC: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    cbSubROI: TComboBox;
    Shape1: TShape;
    Timer: TTimer;
    edRatioExclusionThreshold: TValidatedEdit;
    Label5: TLabel;
    scFLDisplay: TScopeDisplay;
    scRDisplay: TScopeDisplay;
    scADCDisplay: TScopeDisplay;
    ckDisplayFluorescence: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure bTDisplayDoubleClick(Sender: TObject);
    procedure bTDisplayHalfClick(Sender: TObject);
    procedure ckDisplayADCClick(Sender: TObject);
    procedure ckFixZeroLevelsClick(Sender: TObject);
    procedure scFLDisplayCursorChange(Sender: TObject);
    procedure scADCDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scFLDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scADCDisplayCursorChange(Sender: TObject);
    procedure edTDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure rbTDisplayUnitsSecsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbDisplayChange(Sender: TObject);
    procedure ckDisplayFluorescenceClick(Sender: TObject);
    procedure cbROIChange(Sender: TObject);
    procedure cbSubROIChange(Sender: TObject);
    procedure ckDisplayRClick(Sender: TObject);
    procedure cbNumeratorChange(Sender: TObject);
    procedure cbDenominatorChange(Sender: TObject);
    procedure edRDisplayMaxKeyPress(Sender: TObject; var Key: Char);
    procedure scRDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scRDisplayCursorChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TimerTimer(Sender: TObject);
    procedure edRatioExclusionThresholdKeyPress(Sender: TObject;
      var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    ADCBuf : PBig16BitArray ;
    FLDisplayBuf : PSingleArray ;
    RDisplayBuf : PSingleArray ;
    ROITimeCourseBuf : PSingleArray ;

    NumROIsPlotted : Integer ;

    ADCReadoutCursor : Integer ; // A/D readout cursor index on scADCDisplay
    FLReadoutCursor : Integer ;  // Fluorescence readout cursor index on scFLDisplay
    RReadoutCursor : Integer ;  // Fluorescence readout cursor index on scRDisplay

    ROIPixelList : Array[0..cMaxROIs] of PROIPixelList ;
    ROINumPixels : Array[0..cMaxROIs] of Integer ;

    CompThread : TViewPlotThread ;   // Time course computation thread


    function GetFLYMax( i : Integer ) : Single ;
    procedure SetFLYMax( i : Integer ; Value : single ) ;
    function GetFLYMin( i : Integer ) : Single ;
    procedure SetFLYMin( i : Integer ; Value : single ) ;
    function GetADCYMax( i : Integer ) : Single ;
    procedure SetADCYMax( i : Integer ; Value : single ) ;
    function GetADCYMin( i : Integer ) : Single ;
    procedure SetADCYMin( i : Integer ; Value : single ) ;
    function GetDisplayGrid : Boolean ;
    procedure SetDisplayGrid( Value : Boolean ) ;
    procedure DisplayADCChannels ;
    procedure SetDisplayUnits ;
    function GetFLTimeCourseAvailable : Boolean ;
    procedure StopCompThread ;

  public
    { Public declarations }
    PlotAvailable : Boolean ;
    CurrentFrame : Integer ;       // Current frame on display
    TCBFileName : string ;

    ROITCSPacing : Integer ;       // No. of points in an ROI time course
    ROITCNumFrames : Integer ;     // No. of points in time course buffer
    ROITCAvailable : Boolean ;     // Tiem course data available flag
    ROITCNumBytes : Integer ;      // No. of bytes in TC buffer
    CompDone : Boolean ;         // Computation done flag

    AllowClose : Boolean ;           // Set TRUE to allow window to close

    procedure NewFile ;
    procedure DisplayTimeCourse( AtFrame : Integer ) ;
    procedure DisplayRatio( AtFrame : Integer ) ;
    function MeanROIIntensity(
             iROI : Integer ;                 // ROI #
             FrameBuf : PIntArray           // Pointer to frame data (in)
             //zExclusionThreshold : Integer    // Inclusion threshold (in)
             ) : Single ;

    function ROIIntensity(
             ROINum : Integer ;               // ROI#
             FrameNum : Integer ;             // Frame #
             FrameType : Integer            // Frame type
             //zExclusionThreshold : Integer    // Inclusion threshold (in)
             ) : Single ;

    procedure MagnifyChannelDisplay( ChanNum : Integer ) ;
    procedure ReduceChannelDisplay( ChanNum : Integer ) ;
    procedure CopyPlotImageToClipboard ;
    procedure CopyPlotDataToClipboard ;
    procedure PrintPlot ;
    procedure UpdateROIList ;
    procedure ZoomOutAll ;
    procedure NewFLTimeCourseRequired ;

    Property FLYMax[ i : Integer ] : Single read GetFLYMax write SetFLYMax ;
    Property FLYMin[ i : Integer ] : Single read GetFLYMin write SetFLYMin ;
    Property ADCYMax[ i : Integer ] : Single read GetADCYMax write SetADCYMax ;
    Property ADCYMin[ i : Integer ] : Single read GetADCYMin write SetADCYMin ;
    Property DisplayGrid : Boolean read GetDisplayGrid write SetDisplayGrid ;
    Property FLTimeCourseAvailable : Boolean read GetFLTimeCourseAvailable ;
  end;

var
  ViewPlotFrm: TViewPlotFrm;

implementation

uses Main , PrintRec, LightSourceUnit ;

{$R *.dfm}

procedure TViewPlotFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
begin

     // Duration of analogue signals display window
     edTDisplay.Value := MainFrm.ADCDisplayWindow ;

     NumROIsPlotted := 0 ;
     PlotAvailable := False ;
     AllowClose := False ;

     NewFile ;

     Resize ;

     end;


procedure TViewPlotFrm.UpdateROIList ;
// ------------------------------------------
// Update list of ROIs available for plotting
// ------------------------------------------
var
    i,iDisplayROI,iSubROI : Integer ;
begin

     // Keep existing ROI
     if (cbROI.Items.Count > 0) and (cbROI.ItemIndex >= 0) then
        begin
        iDisplayROI := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
        end
     else iDisplayROI := -1 ;

     // Keep existing subtraction ROI
     if (cbSubROI.Items.Count > 0) and (cbSubROI.ItemIndex >= 0) then
        begin
        iSubROI := Integer(cbSubROI.Items.Objects[cbSubROI.ItemIndex]) ;
        end
     else iSubROI := -1 ;

     // Update ROI list
     cbROI.Clear ;
     cbSubROI.Clear ;
     cbSubROI.Items.AddObject(' ',TObject(MainFrm.IDRFile.MaxROI+1)) ;
     for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then
        begin
        cbROI.Items.AddObject(format('ROI %d',[i]),TObject(i)) ;
        cbSubROI.Items.AddObject(format('ROI %d',[i]),TObject(i)) ;
        if i = iDisplayROI then cbROI.ItemIndex := cbROI.Items.Count-1 ;
        if i = iSubROI then cbROI.ItemIndex := cbSubROI.Items.Count-1 ;
        end ;

     if (cbROI.Items.Count > 0) and
        ((iDisplayROI < 0) or (iDisplayROI > MainFrm.IDRFile.MaxROI)) then
         begin
         cbROI.ItemIndex := 0 ;
         end ;

     if (cbSubROI.Items.Count > 0) and
        ((iSubROI < 0) or (iSubROI > MainFrm.IDRFile.MaxROI)) then
         begin
         cbSubROI.ItemIndex := 0 ;
         end ;

     // Clear fluorescence and ratio displays in no ROIs
     if cbROI.Items.Count <= 0 then
        begin
        ckDisplayFluorescence.Checked := False ;
        ckDisplayR.Checked := False ;
        end ;

     end ;


procedure TViewPlotFrm.NewFile ;
// -----------------------------------------------
// Update controls when data file has been changed
// -----------------------------------------------
var
     ch,i : Integer ;
     FileHandle : THandle ;
begin

     Caption := 'Time Course: ' + MainFrm.IDRFile.FileName ;
     CurrentFrame := 1 ;

     // Set numerator and denominator wavelength lists
     cbNumerator.Clear ;
     for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do begin
          cbNumerator.Items.Add( MainFrm.IDRFile.FrameType[i] ) ;
          end ;
     cbDenominator.Items.Assign( cbNumerator.Items ) ;

     cbNumerator.ItemIndex := Min(MainFrm.TimeCourseRatioNumerator,
                                  cbNumerator.Items.Count-1) ;
     cbDenominator.ItemIndex := Min(MainFrm.TimeCourseRatioDenominator,
                                    cbDenominator.Items.Count-1) ;
     edRDisplayMax.Value := MainFrm.TimeCourseRatioDisplayMax ;
     edRatioExclusionThreshold.Value := MainFrm.TimeCourseRatioThreshold ;

     // Set regions of interest list
     UpdateROIList ;

    // Create time course buffer point -> -> frame # pointer list
    if ROITimeCourseBuf <> Nil then FreeMem( ROITimeCourseBuf ) ;
    ROITCNumBytes := MainFrm.IDRFile.NumFrames*MainFrm.IDRFile.NumFrameTypes*Max(MainFrm.IDRFile.MaxROIInUse+1,1)*SizeOf(Single) ;
    ROITimeCourseBuf := AllocMem( ROITCNumBytes ) ;
    ROITCNumFrames := MainFrm.IDRFile.NumFrames ;

    sbDisplay.Max := MainFrm.IDRFile.NumFrames - 1 ;

     // Set up ROI time course display
     // ------------------------------

     // Get number and types of frames in use
     if MainFrm.IDRFile.SpectralDataFile then scFLDisplay.NumChannels := 1
     else scFLDisplay.NumChannels := MainFrm.IDRFile.NumFrameTypes ;
     ROITCSpacing := MainFrm.IDRFile.NumFrames*scFLDisplay.NumChannels ;

     scFLDisplay.MinADCValue := -MainFrm.IDRFile.GreyMax ;
     scFLDisplay.MaxADCValue := MainFrm.IDRFile.GreyMax ;
     scFLDisplay.NumChannels := scFLDisplay.NumChannels ;

     // Add zero level cursors
     scFLDisplay.ClearHorizontalCursors ;
     for ch :=  0 to scFLDisplay.NumChannels-1 do
         scFLDisplay.AddHorizontalCursor( ch, clGreen, True, 'z' ) ;

     // No. of points in A/D window
     MainFrm.ADCDisplayWindow := edTDisplay.Value ;
     scFLDisplay.MaxPoints := Round( MainFrm.ADCDisplayWindow/
                                   (scFLDisplay.NumChannels*MainFrm.IDRFile.FrameInterval)) ;
     if MainFrm.IDRFile.ADCNumChannels > 0 then begin
        edTDisplay.HiLimit := MainFrm.IDRFile.ADCSCanInterval*1000000 ;
        end
     else edTDisplay.HiLimit := MainFrm.IDRFile.FrameInterval*1000000 ;

     edTDisplay.Value := MainFrm.ADCDisplayWindow ;
     scFLDisplay.NumPoints := 0 ;

     scFLDisplay.xMax := scFLDisplay.MaxPoints-1 ;
     scFLDisplay.xMin := 0 ;

     { Set channel information }
     for ch := 0 to scFLDisplay.NumChannels-1 do
         begin
         scFLDisplay.ChanOffsets[ch] := ch ;
         scFLDisplay.ChanUnits[ch] := '';
         scFLDisplay.ChanName[ch] := MainFrm.IDRFile.FrameType[ch] ;
         scFLDisplay.ChanScale[ch] := MainFrm.IDRFile.IntensityScale ;
         scFLDisplay.yMin[ch] := 0 ;
         scFLDisplay.yMax[ch] := MainFrm.GreyHi[ch] ;
         scFLDisplay.HorizontalCursors[ch] := 0 ;
         scFLDisplay.ChanVisible[ch] := True ;
         end ;

    if FLDisplayBuf <> Nil then FreeMem(FLDisplayBuf) ;
    FLDisplayBuf := AllocMem( scFLDisplay.MaxPoints*scFLDisplay.NumChannels*SizeOf(Single));
    scFLDisplay.SetDataBuf( FLDisplayBuf ) ;
    scFLDisplay.NumBytesPerSample := SizeOf(Single) ;
    scFLDisplay.FloatingPointSamples := True ;

    scFLDisplay.ClearVerticalCursors ;
    FLReadoutCursor := scFLDisplay.AddVerticalCursor(-1,clGreen,'?t?y') ;
    scFLDisplay.VerticalCursors[FLReadoutCursor] := scFLDisplay.MaxPoints div 2 ;
    scFLDisplay.DisplaySelected := True ;

    // Set up ratio time course display
    // ------------------------------

    scRDisplay.MinADCValue := -MainFrm.IDRFile.GreyMax ;
    scRDisplay.MaxADCValue := MainFrm.IDRFile.GreyMax ;
    scRDisplay.NumChannels := 1 ;

    // Add zero level cursors
    scRDisplay.ClearHorizontalCursors ;
    for ch :=  0 to scRDisplay.NumChannels-1 do
        scRDisplay.AddHorizontalCursor( ch, clGreen, True, 'z' ) ;

    // No. of points in ratio display
    scRDisplay.MaxPoints := scFLDisplay.MaxPoints ;
    scRDisplay.NumPoints := 0 ;

    scRDisplay.xMax := scRDisplay.MaxPoints-1 ;
    scRDisplay.xMin := 0 ;

    { Set channel information }
    scRDisplay.ChanOffsets[0] := 0 ;
    scRDisplay.ChanUnits[0] := '';
    scRDisplay.ChanName[0] := 'R' ;
    scRDisplay.ChanScale[0] := edRDisplayMax.Value / scRDisplay.MaxADCValue ;
    scRDisplay.yMin[0] := 0 ;
    scRDisplay.yMax[0] := scRDisplay.MaxADCValue ;
    scRDisplay.HorizontalCursors[0] := 0 ;
    scRDisplay.ChanVisible[0] := True ;

    if RDisplayBuf <> Nil then FreeMem(RDisplayBuf) ;
    RDisplayBuf := AllocMem( Max(scRDisplay.MaxPoints*scRDisplay.NumChannels,1)*SizeOf(Single));
    scRDisplay.NumBytesPerSample := SizeOf(Single) ;
    scRDisplay.FloatingPointSamples := True ;
    scRDisplay.SetDataBuf( RDisplayBuf ) ;

    scRDisplay.ClearVerticalCursors ;
    RReadoutCursor := scRDisplay.AddVerticalCursor(-1,clGreen,'?t?y') ;
    scRDisplay.VerticalCursors[RReadoutCursor] := scRDisplay.MaxPoints div 2 ;
    scRDisplay.DisplaySelected := False ;

    // Set up A/D channel display
    // --------------------------

    if MainFrm.IDRFile.ADCNumChannels > 0 then begin

       scADCDisplay.MaxADCValue := MainFrm.IDRFile.ADCMaxValue ;
       scADCDisplay.MinADCValue := -MainFrm.IDRFile.ADCMaxValue - 1 ;

       // Add zero level cursors
       scADCDisplay.ClearHorizontalCursors ;
       for ch :=  0 to MainFrm.IDRFile.ADCNumChannels-1 do
           scADCDisplay.AddHorizontalCursor( ch, clGreen, True, 'z' ) ;

       // No. of points in A/D window
       MainFrm.ADCDisplayWindow := edTDisplay.Value ;
       scADCDisplay.MaxPoints := Round(edTDisplay.Value/MainFrm.IDRFile.ADCScanInterval);

       scADCDisplay.NumPoints := 0 ;
       scADCDisplay.NumChannels := MainFrm.IDRFile.ADCNumChannels ;
       scADCDisplay.xMax := scADCDisplay.MaxPoints-1 ;
       scADCDisplay.xMin := 0 ;
       scADCDisplay.TScale := MainFrm.IDRFile.ADCSCanInterval ;
       { Set channel information }
       for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do
           begin
           scADCDisplay.ChanOffsets[ch] := MainFrm.IDRFile.ADCChannel[ch].ChannelOffset ;
           scADCDisplay.ChanUnits[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCUnits ;
           scADCDisplay.ChanName[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCName ;
           scADCDisplay.ChanScale[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCScale ;
           scADCDisplay.yMin[ch] := MainFrm.IDRFile.ADCChannel[ch].yMin ;
           scADCDisplay.yMax[ch] := MainFrm.IDRFile.ADCChannel[ch].yMax ;
           scADCDisplay.HorizontalCursors[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCZero ;
           scADCDisplay.ChanVisible[ch] := MainFrm.IDRFile.ADCChannel[ch].InUse ;
           end ;

       // Allocate A/D buffer
       if ADCBuf <> Nil then FreeMem( ADCBuf ) ;
       ADCBuf := AllocMem( scADCDisplay.MaxPoints*MainFrm.IDRFile.ADCNumChannels*2 ) ;
       scADCDisplay.SetDataBuf( ADCBuf ) ;

       scADCDisplay.ClearVerticalCursors ;
       ADCReadoutCursor := scADCDisplay.AddVerticalCursor(-1,clGreen,'?t?y') ;
       scADCDisplay.VerticalCursors[ADCReadoutCursor] := scADCDisplay.MaxPoints div 2 ;

       scFLDisplay.DisplaySelected := False ;

       end ;

    // Set display time units
    SetDisplayUnits ;

    // Load plot from .TCB storage file (if it exists)
    TCBFileName :=  ChangeFileExt(Mainfrm.IDRFile.FileName,'.TCB') ;
    ROITCAVailable := False ;
    if FileExists(TCBFileName) then
       begin
       FileHandle := FileOpen( TCBFileName, fmOpenRead );
       if NativeInt(FileHandle) <> -1 then
          begin
          FileRead( FileHandle,ROITimeCourseBuf^,ROITCNumBytes);
          FileClose( FileHandle) ;
          ROITCAVailable := True ;
          end
       else NewFLTimeCourseRequired ;
       end
    else NewFLTimeCourseRequired ;

    PlotAvailable := True ;

    end ;



procedure TViewPlotFrm.FormResize(Sender: TObject);
// -----------------------------------------------
// Re-size/locate controls when form size changes
// -----------------------------------------------
var
    DisplayHeight : Integer ;
    iTop : Integer ;
    NumDisplays : Integer ;
begin

     // Scroll bar and control panel
     TDisplayPanel.Top := ClientHeight
                           - TDisplayPanel.Height - 5 ;
     ckFixZeroLevels.Top := TDisplayPanel.Top ;
     ckFixZeroLevels.Left := sbDisplay.Left ;
     sbDisplay.Top := TDisplayPanel.Top - sbDisplay.Height - 1 ;

     // Set size & location of time course displays

     //DisplayHeight := Max( sbDisplay.Top - scFLDisplay.Top - 1, 2 ) ;

     if MainFrm.IDRFile.ADCNumChannels <= 0 then begin
        ckDisplayADC.Visible := False ;
        ckDisplayADC.Checked := False ;
        end ;

     scFLDisplay.Visible := ckDisplayFluorescence.Checked ;
     scADCDisplay.Visible := ckDisplayADC.Checked ;
     scRDisplay.Visible := ckDisplayR.Checked ;

     NumDisplays := 0 ;
     if scFLDisplay.Visible then Inc(NumDisplays) ;
     if scADCDisplay.Visible then Inc(NumDisplays) ;
     if scRDisplay.Visible then Inc(NumDisplays) ;
     DisplayHeight := Max( sbDisplay.Top - scFLDisplay.Top - 1, 2 ) div Max(NumDisplays,1) ;

     iTop := scFLDisplay.Top ;

     // Fluorescence display
     if scFLDisplay.Visible then begin
        scFLDisplay.Height := Max( DisplayHeight - 5, 2) ;
        iTop := iTop + DisplayHeight ;
        end
     else scFLDisplay.Height := 2 ;

     // Ratio display
     scRDisplay.Top := iTop ;
     if scRDisplay.Visible then begin
        scRDisplay.Height := Max( DisplayHeight - 5, 2) ;
        iTop := iTop + DisplayHeight ;
        end
     else scRDisplay.Height := 2 ;

     // A/D display
     scADCDisplay.Top := iTop ;
     if scADCDisplay.Visible then begin
        scADCDisplay.Height := Max( DisplayHeight - 5, 2) ;
        iTop := iTop + DisplayHeight ;
        end
     else scADCDisplay.Height := 2 ;

     sbDisplay.Top := iTop - 4 ;

     scFLDisplay.Width := Max( ClientWidth - scADCDisplay.Left - 5,2 ) ;
     scRDisplay.Width := scFLDisplay.Width ;
     scADCDisplay.Width := scFLDisplay.Width ;
     sbDisplay.Width := scFLDisplay.Width ;

     TDisplayPanel.Left := scADCDisplay.Left + scADCDisplay.Width - TDisplayPanel.Width ;

     end;


procedure TViewPlotFrm.PrintPlot ;
{ ----------
  Print plot
  ---------- }
var
    ScopeDisp : TScopeDisplay ;
begin

     // Print record on display
     PrintRecFrm.Destination := dePrinter ;
     if scFLDisplay.DisplaySelected then ScopeDisp := scFLDisplay
     else if scRDisplay.DisplaySelected then ScopeDisp := scRDisplay
                                    else  ScopeDisp := scADCDisplay ;

     PrintRecFrm.DisplayObj := ScopeDisp ;
     PrintRecFrm.ShowModal ;
     if PrintRecFrm.ModalResult = mrOK then begin
        ScopeDisp.ClearPrinterTitle ;
        ScopeDisp.AddPrinterTitleLine( 'File : ' + MainFrm.IDRFile.FileName ) ;
        ScopeDisp.AddPrinterTitleLine( MainFrm.IDRFile.Ident ) ;
        ScopeDisp.Print ;
        end ;

     end ;


procedure TViewPlotFrm.SetDisplayUnits ;
// ----------------------
// Set display time units
// ----------------------
begin

    if rbTDisplayUnitsSecs.Checked then
       begin
       edTDisplay.Units := 's' ;
       edTDisplay.Scale := 1.0 ;

       end
    else
       begin
       edTDisplay.Units := 'm' ;
       edTDisplay.Scale := 1.0/60.0 ;
       end ;

    scFLDisplay.TScale := MainFrm.IDRFile.FrameInterval*edTDisplay.Scale ;
    scFLDisplay.TUnits := edTDisplay.Units ;

    // Determine no. of points in display
    scFLDisplay.MaxPoints := Round(edTDisplay.Value/MainFrm.IDRFile.FrameInterval) + 1 ;
    scFLDisplay.XMax := scFLDisplay.MaxPoints ;
    scFLDisplay.Invalidate ;

    // Set ratio display
    if MainFrm.IDRFile.SpectralDataFile then
       begin
       scRDisplay.MaxPoints := scFLDisplay.MaxPoints ;
       scRDisplay.TScale := scFLDisplay.TScale ;
       end
    else
       begin
       scRDisplay.MaxPoints := scFLDisplay.MaxPoints ;
       scRDisplay.TScale := scFLDisplay.TScale ;
       end ;

    scRDisplay.TUnits := scFLDisplay.TUnits ;
    scRDisplay.XMax := scFLDisplay.XMax ;
    scRDisplay.Invalidate ;

    end ;


function TViewPlotFrm.GetFLTimeCourseAvailable : Boolean ;
// ------------------------------------------
// Return TRUE if ROI time course buffer available
// ------------------------------------------
begin
      Result := ROITCAvailable ;
      end ;


procedure TViewPlotFrm.NewFLTimeCourseRequired ;
// ------------------------------------------
// Start another ROI time course computation
// ------------------------------------------
var
    i : Integer ;
begin

     ROITCAVailable := False ;

     // If computation thread is running, stop it
     StopCompThread ;

     for i:= 0 to High(ROIPixelList) do
         begin
         if ROIPixelList[i] <> Nil then
            begin
            FreeMem(ROIPixelList[i]) ;
            ROIPixelList[i] := Nil ;
            ROINumPixels[i] := 0 ;
            end ;
         end ;

     CompDone := False ;

     // Create and run computation thread
     CompThread := TViewPlotThread.Create ;

     end ;


procedure TViewPlotFrm.DisplayTimeCourse(
          AtFrame : Integer
          ) ;
// -------------------------------------------
// Display section of fluorescence time course
// -------------------------------------------
var
    i,j : Integer ;
    iROI,iSubROI : Integer ;
    StartGroup : Integer ;
    CursorGroup : Integer ;
    NumDisplayFrames : Integer ;
    MarkerAt : Integer ;
    TMarkerScale : Single ;
begin

    if AtFrame < 1 then Exit ;
    if not ROITCAvailable then Exit ;

    // Find starting frame group
    NumDisplayFrames := scFLDisplay.MaxPoints ;
    CurrentFrame := AtFrame ;
    sbDisplay.Position := AtFrame ;

    CursorGroup := AtFrame ;
    StartGroup := Max( CursorGroup - (NumDisplayFrames div 2), 1 ) ;
    scFLDisplay.XOffset := StartGroup ;
    NumDisplayFrames := Min( NumDisplayFrames, MainFrm.IDRFile.NumFrames-StartGroup );

    // Allocate buffer
    if FLDisplayBuf <> Nil then FreeMem(FLDisplayBuf) ;
    FLDisplayBuf := AllocMem( scFLDisplay.MaxPoints*scFLDisplay.NumChannels*SizeOf(Integer));
    scFLDisplay.SetDataBuf( FLDisplayBuf ) ;
    scFLDisplay.NumBytesPerSample := SizeOf(Integer) ;

    scFLDisplay.VerticalCursors[FLReadoutCursor] := CursorGroup - StartGroup - 1 ;

    // Plot selected fluorescence ROI

    if (cbROI.ItemIndex >= 0) and ckDisplayFluorescence.Checked then begin

        iROI := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
        if cbSubROI.ItemIndex > 0 then
           iSubROI := Integer(cbSubROI.Items.Objects[cbSubROI.ItemIndex])
        else iSubROI := 0 ;

        // Copy time course data into display buffer
        j := ((iROI-1)*ROITCSpacing) + (StartGroup*scFLDisplay.NumChannels)  ;
        for i := 0 to NumDisplayFrames*scFLDisplay.NumChannels-1 do
            begin
            FLDisplayBuf^[i] := ROITimeCourseBuf^[j] ;
            Inc(j) ;
            end ;

        // Subtract background ROI
        if iSubROI > 0 then
           begin
           j := ((iSubROI-1)*ROITCSpacing) + (StartGroup*scFLDisplay.NumChannels)  ;
           for i := 0 to NumDisplayFrames*scFLDisplay.NumChannels-1 do
               begin
               FLDisplayBuf^[i] := FLDisplayBuf^[i] - ROITimeCourseBuf^[j] ;
               Inc(j) ;
               end ;
           end ;
        end ;

     // Add markers (if any appear on display
     scFLDisplay.ClearMarkers ;
     if rbTDisplayUnitMins.Checked then TMarkerScale := 60.0/scFLDisplay.TScale
                                   else TMarkerScale := 1.0/scFLDisplay.TScale ;
     for i := 0 to MainFrm.IDRFile.NumMarkers-1 do
         begin
         MarkerAt := Round(MainFrm.IDRFile.MarkerTime[i]*TMarkerScale) - scFLDisplay.XOffset ;
         if (MarkerAt >= 0) and (MarkerAt < scFLDisplay.MaxPoints) then
            scFLDisplay.AddMarker( MarkerAt, MainFrm.IDRFile.MarkerText[i] );
         end ;

    scFLDisplay.NumPoints := NumDisplayFrames ;
    scFLDisplay.Invalidate ;

    // Display wavelength ratio time course
    if MainFrm.IDRFile.SpectralDataFile and ckDisplayR.Checked then ckDisplayR.Checked := False ;

    if ckDisplayR.Checked then DisplayRatio( AtFrame ) ;

    // Update A/D signals display
    if MainFrm.IDRFile.ADCNumChannels > 0 then DisplayADCChannels ;

    end ;


procedure TViewPlotFrm.DisplayADCChannels ;
// ------------------------------------------
// Display analogue signals stored on file
// ------------------------------------------
var
    StartScan : Int64 ;
    CursorScan : Integer ;
    i : Integer ;
    MarkerAt : Integer ;
    TMarkerScale : Single ;
    CursorTime,StartTime : Single ;
    NumScans : Integer ;
begin

     // No. of multi-channel scans to be displayed
     NumScans := Max( Round(edTDisplay.Value/MainFrm.IDRFile.ADCScanInterval),1 ) ;
     scADCDisplay.MaxPoints := NumScans ;

     // Find starting scan number
     CursorTime := CurrentFrame*MainFrm.IDRFile.FrameInterval ;
     CursorScan := Round( CursorTime / MainFrm.IDRFile.ADCScanInterval ) ;

     // Make start time of A/D display same as fluorescence display
     StartTime := scFLDisplay.XOffset*MainFrm.IDRFile.FrameInterval ;
     StartScan := Round( StartTime/MainFrm.IDRFile.ADCScanInterval ) ;
     scADCDisplay.XOffset := Cardinal(StartScan) ;

     // Make A/D readout cursor same as fluorescence readout cursor
     scADCDisplay.VerticalCursors[ADCReadoutCursor] := Round( scFLDisplay.VerticalCursors[FLReadoutCursor]*
                                                             (scFLDisplay.TScale / scADCDisplay.TScale) ) ;
     // Set display time units
     SetDisplayUnits ;

     // Read data from file
     if ADCBuf <> Nil then FreeMem( ADCBuf ) ;
     ADCBuf := AllocMem( scADCDisplay.MaxPoints*MainFrm.IDRFile.ADCNumChannels*2 ) ;
     scADCDisplay.SetDataBuf( ADCBuf ) ;
     scADCDisplay.NumPoints := MainFrm.IDRFile.LoadADC( StartScan,
                                                        Min(NumScans,MainFrm.IDRFile.ADCNumScansInFile-StartScan),
                                                        ADCBuf^ ) ;
     scADCDisplay.xMin := 0 ;
     scADCDisplay.xMax := scADCDisplay.MaxPoints-1 ;

     // Add markers (if any appear on display)
     scadcDisplay.ClearMarkers ;
     if rbTDisplayUnitMins.Checked then TMarkerScale := 60.0/scADCDisplay.TScale
                                   else TMarkerScale := 1.0/scADCDisplay.TScale ;
     for i := 0 to MainFrm.IDRFile.NumMarkers-1 do
         begin
         MarkerAt := Round(MainFrm.IDRFile.MarkerTime[i]*TMarkerScale)
                     - scADCDisplay.XOffset ;
         if (MarkerAt >= 0) and (MarkerAt < scadcDisplay.MaxPoints) then
            scadcDisplay.AddMarker( MarkerAt, MainFrm.IDRFile.MarkerText[i] );
         end ;

     scADCDisplay.Invalidate ;

     end ;




procedure TViewPlotFrm.DisplayRatio(
          AtFrame : Integer
          ) ;
// ---------------------------------------
// Display fluorescence ratio time course
// ---------------------------------------
var
    i : Integer ;
    yNum,yDen,YScale : Single ;
    iROI,iSubROI : Integer ;
    jROINum,jSubNum, jROIDen, jSubDen : Integer ;
    StartGroup : Integer ;
    CursorGroup : Integer ;
    NumDisplayGroups : Integer ;
    MarkerAt : Integer ;
    TMarkerScale : Single ;
    SubtractROI : Boolean ;
    NumFrameTypes : Integer ;
    yThreshold : Single ;
begin

    if cbROI.ItemIndex < 0 then Exit ; // Quit if no ROIs defined
    if not ROITCAvailable then Exit ;

    // Find starting frame group
    NumFrameTypes := MainFrm.IDRFile.NumFrameTypes ;
    NumDisplayGroups := scRDisplay.MaxPoints ;
    CurrentFrame := AtFrame ;
    sbDisplay.Position := AtFrame ;

    CursorGroup := CurrentFrame ;
    StartGroup := Max( CursorGroup - (NumDisplayGroups div 2), 1 ) ;
    scRDisplay.XOffset := StartGroup ;
    NumDisplayGroups := Min( NumDisplayGroups, MainFrm.IDRFile.NumFrames-StartGroup );
    scRDisplay.VerticalCursors[RReadoutCursor] := CursorGroup - StartGroup ;

    if RDisplayBuf <> Nil then FreeMem(RDisplayBuf) ;
    RDisplayBuf := AllocMem( Max(scRDisplay.MaxPoints*scRDisplay.NumChannels,1)*SizeOf(Integer));
    scRDisplay.NumBytesPerSample := SizeOf(Integer) ;
    scRDisplay.SetDataBuf( RDisplayBuf ) ;

    // Frame #1 is at normally at index=NumFrameTypes in time course buffer
    // In spectral data files index=1 for frame #1
    //if MainFrm.IDRFile.SpectralDataFile then jOffset := -NumFrameTypes + 1
    //                                    else jOffset := 0 ;

    // Copy time course data into display buffer
    iROI := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
    jROINum := ((iROI-1)*ROITCSpacing) + (StartGroup*NumFrameTypes)
               + cbNumerator.ItemIndex ;
    jROIDen := ((iROI-1)*ROITCSpacing) + (StartGroup*NumFrameTypes)
               + cbDenominator.ItemIndex ;

    iSubROI := Integer(cbSubROI.Items.Objects[cbSubROI.ItemIndex]) ;
    if iSubROI < (MainFrm.IDRFile.MaxROI+1) then begin
       jSubNum := ((iSubROI-1)*ROITCSpacing) + (StartGroup*NumFrameTypes)
                  + cbNumerator.ItemIndex ;
       jSubDen := ((iSubROI-1)*ROITCSpacing) + (StartGroup*NumFrameTypes)
                  + cbDenominator.ItemIndex ;
       SubtractROI := True ;
       end
    else begin
       SubtractROI := False ;
       jSubNum := 0 ;
       jSubDen := 0 ;
       end;
    YScale := scRDisplay.MaxADCValue / edRDisplayMax.Value ;
    scRDisplay.ChanScale[0] := 1.0 / YScale ;

    // Channel name
    scRDisplay.ChanName[0] :=
       MainFrm.IDRFile.FrameType[cbNumerator.ItemIndex]
       + '/' +
       MainFrm.IDRFile.FrameType[cbDenominator.ItemIndex] ;

    yThreshold := edRatioExclusionThreshold.Value ;
    for i := 0 to NumDisplayGroups-1 do begin

        // Calculate ratio
        yNum := ROITimeCourseBuf^[jROINum] ;
        if SubtractROI then yNum := yNum - ROITimeCourseBuf[jSubNum] ;
        yDen := ROITimeCourseBuf^[jROIDen] ;
        if SubtractROI then yDen := yDen - ROITimeCourseBuf^[jSubDen] ;
        if yDen >= yThreshold then RDisplayBuf^[i] := Round((yNum/yDen)*YScale)
                              else RDisplayBuf^[i] := 0 ;

        // Increment pointers
        jROINum := jROINum + NumFrameTypes ;
        jSubNum := jSubNum + NumFrameTypes ;
        jROIDen := jROIDen + NumFrameTypes ;
        jSubDen := jSubDen + NumFrameTypes ;

        end ;

     // Add markers (if any appear on display
     scRDisplay.ClearMarkers ;
     if rbTDisplayUnitMins.Checked then TMarkerScale := 60.0/scRDisplay.TScale
                                   else TMarkerScale := 1.0/scRDisplay.TScale ;
     for i := 0 to MainFrm.IDRFile.NumMarkers-1 do begin
         MarkerAt := Round(MainFrm.IDRFile.MarkerTime[i]*TMarkerScale)
                     - scRDisplay.XOffset ;
         if (MarkerAt >= 0) and (MarkerAt < scRDisplay.MaxPoints) then
            scRDisplay.AddMarker( MarkerAt, MainFrm.IDRFile.MarkerText[i] );
         end ;

    scRDisplay.NumPoints := NumDisplayGroups ;
    scRDisplay.Invalidate ;

    end ;


function TViewPlotFrm.MeanROIIntensity(
         iROI : Integer ;                 // ROI # (in)
         FrameBuf : PIntArray            // Pointer to frame data (in)
//         zExclusionThreshold : Integer    // Inclusion threshold (in)
          ) : Single ;
// --------------------------------------------------------
// Calculate mean pixel intensity within region of interest
// and exceeding exclusion limit
// --------------------------------------------------------
var
     xPix,yPix,i,ix,iy,NumPixels : Integer ;
     //z : Integer ;
     z,Sum : Single ;
     nSum : Integer ;
     r,Asq,BSq : Single ;
     XStart, XEnd : Single ; // X limits of line
     YStart, YEnd : single ; // Y limits of line
     XStep, YStep : Single ;
     X, Y : Single ; // line coordinates
     D : Single ;    // Distance along line
     DStart : Single ; // Start of line segment
     DEnd : Single ;   // Length of line segment

     LeftEdge : Integer ;  // Rectangular bounds of current ROI
     RightEdge : Integer ;
     TopEdge : Integer ;
     BottomEdge : Integer ;
     iLine,nLines : Integer ;
     SlopeAngle : Single ;

     P : PByteArray ;
     ROIBitMap : TBitMap ;
     XY : Array[0..ROIMaxPoints-1] of TPoint ;
     ROI : TROI ;
     PixelList : PROIPixelList ;

begin
     Result := 0.0 ;

     // Get ROI record
     ROI := MainFrm.IDRFile.ROI[iROI] ;

     LeftEdge := Min(ROI.TopLeft.X,ROI.BottomRight.X) ;
     RightEdge := Max(ROI.TopLeft.X,ROI.BottomRight.X) ;
     TopEdge := Min(ROI.TopLeft.Y,ROI.BottomRight.y) ;
     BottomEdge := Max(ROI.TopLeft.Y,ROI.BottomRight.y) ;

     case ROI.Shape of

         // Cross-hairs region of interest
         PointROI : begin
            i := ROI.Centre.Y*MainFrm.IDRFile.FrameWidth + ROI.Centre.X ;
            Result := FrameBuf[i] ;
            end ;

         // Rectangular region of interest
         RectangleROI : begin
            Sum := 0.0 ;
            NumPixels := 0 ;
            for xPix := LeftEdge to RightEdge do
                for yPix := TopEdge to BottomEdge do begin
                    i := yPix*MainFrm.IDRFile.FrameWidth + xPix ;
                    //if z >= zExclusionThreshold then begin
                    Sum := Sum + FrameBuf[i] ; ;
                    Inc(NumPixels) ;
                    //   end ;
                    end ;

           if NumPixels > 0 then Result := Sum/NumPixels
                            else Result := 0.0 ;

           end ;

         // Elliptical region of interest
         EllipseROI : begin
            Sum := 0.0 ;
            aSq := (LeftEdge - ROI.Centre.x)*(LeftEdge - ROI.Centre.x) ;
            bSq := (TopEdge - ROI.Centre.y)*(TopEdge - ROI.Centre.y) ;
            NumPixels := 0 ;
            for xPix := LeftEdge to RightEdge do
                for yPix := TopEdge to BottomEdge do begin
                    r := ((xPix-ROI.Centre.x)*(xPix-ROI.Centre.x)/aSq) +
                         ((yPix-ROI.Centre.y)*(yPix-ROI.Centre.y)/bSq) ;
                    if  r <= 1.0 then begin
                        i := yPix*MainFrm.IDRFile.FrameWidth + xPix ;
                       //if z >= zExclusionThreshold then begin
                        Sum := Sum + FrameBuf[i] ; ;
                        Inc(NumPixels) ;
                       //   end ;
                        end ;
                    end ;

           if NumPixels > 0 then Result := Sum/NumPixels
                            else Result := 0.0 ;

           end ;

         // Line and polyline regions
         LineROI,PolylineROI : Begin

            case ROI.Shape of
               PolylineROI : nLines := ROI.NumPoints-1 ;
               else nLines := 1 ;
               end ;

            DStart := 0.0 ;
            NumPixels := 0 ;
            Sum := 0.0 ;
            for iLine := 0 to nLines-1 do begin

               // Get line segment
               case ROI.Shape of
                  PolylineROI : begin
                  XStart := ROI.XY[iLine].X ;
                  YStart := ROI.XY[iLine].Y ;
                  XEnd := ROI.XY[iLine+1].X ;
                  YEnd := ROI.XY[iLine+1].Y ;
                  end ;
               else begin
                  XStart := ROI.TopLeft.X ;
                  YStart := ROI.TopLeft.Y ;
                  XEnd := ROI.BottomRight.X ;
                  YEnd := ROI.BottomRight.Y ;
                  end ;
               end ;

               if XEnd <> XStart then SlopeAngle := ArcTan2( YEnd - YStart, XEnd - XStart )
                                 else  SlopeAngle := 0.5*Pi ;

               // Add pixels on line segment to average

               X := XStart ;
               Y := YStart ;
               XStep := Cos(SlopeAngle) ;
               YStep := Sin(SlopeAngle) ;
               DEnd :=  Sqrt( (XEnd - XStart)*(XEnd - XStart)
                        + (YEnd - YStart)*(YEnd - YStart)) ;
               Repeat
                  // Distance along line profile
                  D :=  Sqrt((X - XStart)*(X - XStart) + (Y - YStart)*(Y - YStart)) ;
                  //if z >= zExclusionThreshold then begin
                  Sum := Sum + FrameBuf[Round(X) + Round(Y)*MainFrm.IDRFile.FrameWidth] ;
                  Inc(NumPixels) ;
                  //   end ;

                  // Increment to next pixel on line
                  X := X + XStep ;
                  Y := Y + YStep ;

                  Until Round(D) >= Round(DEnd) ;

               DStart := DStart + DEnd ;

               end ;

           if NumPixels > 0 then Result := Sum / NumPixels
                            else Result := 0.0 ;

           end ;

         // Polygon regions
         PolygonROI : Begin

           PixelList := ROIPixelList[iROI] ;
           NumPixels := ROINumPixels[iROI] ;

           if PixelList = Nil then begin

              // Determine rectangular region of ROI
              ROI.TopLeft.X := MainFrm.IDRFile.FrameWidth ;
              ROI.TopLeft.Y := MainFrm.IDRFile.FrameHeight ;
              ROI.BottomRight.X  := 0 ;
              ROI.BottomRight.Y  := 0 ;
              for i := 0 to ROI.NumPoints-1 do begin
                  ROI.TopLeft.X := Min(ROI.TopLeft.X,ROI.XY[i].X) ;
                  ROI.TopLeft.Y := Min(ROI.TopLeft.Y,ROI.XY[i].Y) ;
                  ROI.BottomRight.X  := Max(ROI.BottomRight.X,ROI.XY[i].X) ;
                  ROI.BottomRight.Y  := Max(ROI.BottomRight.Y,ROI.XY[i].Y) ;
                  end ;
              ROI.Width := (ROI.BottomRight.X - ROI.TopLeft.X + 1) ;
              ROI.Height := (ROI.BottomRight.Y - ROI.TopLeft.Y + 1) ;

              // Create bitmap and draw outline of ROI on it
              ROIBitMap := TBitmap.Create ;
              ROIBitMap.PixelFormat := pf1Bit ;
              ROIBitMap.Width := Max(ROI.Width,2) ;
              ROIBitMap.Height := Max(ROI.Height,2) ;
              ROIBitMap.Canvas.Brush.Color := clBlack ;
              ROIBitMap.Canvas.Pen.Color := clWhite ;
              for i := 0 to ROI.NumPoints-1 do begin
                  XY[i].X := ROI.XY[i].X - ROI.TopLeft.X ;
                  XY[i].Y := ROI.XY[i].Y - ROI.TopLeft.Y ;
                  end ;
              ROIBitMap.Canvas.Polygon(Slice(XY,ROI.NumPoints));

              // Create list of pixels enclosed by ROI
              NumPixels := 0 ;
              PixelList := AllocMem( 2*SizeOf(TPoint)*(ROIBitMap.Width*ROIBitMap.Height)) ;

              for iY := 0 to ROI.Height-1 do begin
                  P := ROIBitMap.ScanLine[iY] ;
                  for iX := 0 to ROI.Width-1 do if P^[iX] =0 then begin
                      PixelList^[NumPixels].X := ROI.TopLeft.X + iX ;
                      PixelList^[NumPixels].Y := ROI.TopLeft.Y + iY ;
                      Inc(NumPixels) ;
                      end ;
                  end ;
              ROIBitMap.Free ;

              ROIPixelList[iROI] := PixelList;
              ROINumPixels[iROI] := NumPixels ;

              end ;

           nSum := 0 ;
           Sum := 0.0 ;
           for i := 0 to NumPixels-1 do begin
               //if z >= zExclusionThreshold then begin
               Sum := Sum + FrameBuf[PixelList[i].X +
                            PixelList[i].Y*MainFrm.IDRFile.FrameWidth] ; ;
               Inc(nSum) ;
              //    end ;
               end ;
           if nSum > 0 then Result := Sum / nSum
                       else Result := 0.0 ;

           end ;

         end ;

     Result := MainFrm.IDRFile.IntensityScale*
               (Result - MainFrm.IDRFile.IntensityOffset) ;

     end ;


function TViewPlotFrm.ROIIntensity(
             ROINum : Integer ;               // ROI#
             FrameNum : Integer ;             // Frame #
             FrameType : Integer             // Frame type
             //zExclusionThreshold : Integer    // Inclusion threshold (in)
             ) : Single ;
// -----------------------------------------
// Return intensity value for ROI# in Frame#
// -----------------------------------------
var
    j : Integer ;
begin

    // Exit if invalid parameters
    if (FrameNum < 1) or
       (FrameNum > MainFrm.IDRFile.NumFrames) or
       (ROINum < 1) or
       (ROINum > MainFrm.IDRFile.MaxROI) then begin
       Result := 0.0 ;
       Exit ;
       end ;

    // Return selected result

    if MainFrm.IDRFile.SpectralDataFile then begin
       // Spectral data
       j := ROITCSPacing*(ROINum-1) +
            (((FrameNum-1) div MainFrm.IDRFile.NumFramesPerSpectrum)*
            MainFrm.IDRFile.NumFramesPerSpectrum) +
            FrameType ;
       end
    else begin
       // Single & multi-wavelength data
       j := ROITCSPacing*(ROINum-1) +
            ((FrameNum-1)*MainFrm.IDRFile.NumFrameTypes) +
            FrameType ;
       end ;
    Result := ROITimeCourseBuf^[j] ;

    end ;


function TViewPlotFrm.GetFLYMax( i : Integer ) : Single ;
// ------------------------------------
// Get upper limit of flourescence plot
// ------------------------------------
begin
    Result := scFLDisplay.YMax[i] ;
    end ;

procedure TViewPlotFrm.SetFLYMax( i : Integer ; Value : single ) ;
// ------------------------------------
// Set upper limit of flourescence plot
// ------------------------------------
begin
    scFLDisplay.YMax[i] := Value ;
    end ;

function TViewPlotFrm.GetFLYMin( i : Integer ) : Single ;
// ------------------------------------
// Get lower limit of flourescence plot
// ------------------------------------

begin
    Result := scFLDisplay.YMin[i] ;
    end ;


procedure TViewPlotFrm.SetFLYMin( i : Integer ; Value : single ) ;
// ------------------------------------
// Set lower limit of flourescence plot
// ------------------------------------

begin
    scFLDisplay.YMin[i] := Value ;
    end ;


function TViewPlotFrm.GetADCYMax( i : Integer ) : Single ;
// ------------------------------------
// Get upper limit of analogue channel plot
// ------------------------------------

begin
    Result := scADCDisplay.YMax[i] ;
    end ;


procedure TViewPlotFrm.SetADCYMax( i : Integer ; Value : single ) ;
// ------------------------------------
// Set upper limit of analogue channel plot
// ------------------------------------
begin
    scADCDisplay.YMax[i] := Value ;
    end ;

function TViewPlotFrm.GetADCYMin( i : Integer ) : Single ;
// ------------------------------------
// Get lower limit of analogue channel plot
// ------------------------------------
begin
    Result := scADCDisplay.YMin[i] ;
    end ;


procedure TViewPlotFrm.SetADCYMin( i : Integer ; Value : single ) ;
// ------------------------------------
// Set lower limit of analogue channel plot
// ------------------------------------
begin
    scADCDisplay.YMin[i] := Value ;
    end ;


procedure TViewPlotFrm.SetDisplayGrid( Value : Boolean ) ;
// ------------------------------------
// Set chart display grid on/off
// ------------------------------------
begin
    scFLDisplay.DisplayGrid := Value ;
    scADCDisplay.DisplayGrid := Value ;
    scRDisplay.DisplayGrid := Value ;
    end ;


function TViewPlotFrm.GetDisplayGrid : Boolean ;
// ------------------------------------
// Get chart display grid on/off state
// ------------------------------------
begin
    Result := scFLDisplay.DisplayGrid ;
    end ;


procedure TViewPlotFrm.bTDisplayDoubleClick(Sender: TObject);
// ----------------------------------------
// Double the duration of the display window
// ----------------------------------------
begin
     edTDisplay.Value := 2.0*edTDisplay.Value ;
     MainFrm.ADCDisplayWindow := edTDisplay.Value ;
     SetDisplayUnits ;
     DisplayTimeCourse( CurrentFrame )

     end;

procedure TViewPlotFrm.bTDisplayHalfClick(Sender: TObject);
// ----------------------------------------
// Halve the duration of the display window
// ----------------------------------------
begin
     edTDisplay.Value := 0.5*edTDisplay.Value ;
     MainFrm.ADCDisplayWindow := edTDisplay.Value ;
     SetDisplayUnits ;
     DisplayTimeCourse( CurrentFrame )
     end;


procedure TViewPlotFrm.MagnifyChannelDisplay(
          ChanNum : Integer ) ;
// ------------------------------------
// Magnify selected A/D channel display
// ------------------------------------
begin
     if ChanNum >= MainFrm.IDRFile.ADCNumChannels then begin
       scFLDisplay.YZoom(ChanNum - MainFrm.IDRFile.ADCNumChannels, -50.0) ;
       end
     else scADCDisplay.YZoom(ChanNum, -50.0) ;
     end ;


procedure TViewPlotFrm.ReduceChannelDisplay( ChanNum : Integer ) ;
// ------------------------------------
// Reduce selected A/D channel display
// ------------------------------------
begin
     if ChanNum >= MainFrm.IDRFile.ADCNumChannels then begin
       scFLDisplay.YZoom(ChanNum - MainFrm.IDRFile.ADCNumChannels, 50.0) ;
       end
     else scADCDisplay.YZoom(ChanNum, 50.0) ;
     end ;


procedure TViewPlotFrm.ckDisplayADCClick(Sender: TObject);
// --------------------------------------------------
// Display analogue tim course checked/unchecked
// --------------------------------------------------
begin

     Resize ;

     DisplayTimeCourse( sbDisplay.Position ) ;

     if ckDisplayADC.Checked then begin
        scADCDisplay.DisplaySelected := True ;
        scFLDisplay.DisplaySelected := False ;
        scRDisplay.DisplaySelected := False ;
        end ;
        
     end;


procedure TViewPlotFrm.ckFixZeroLevelsClick(Sender: TObject);
// ---------------------------------
// Fix/unfix measurements zero level
// ---------------------------------
begin
     scADCDisplay.FixZeroLevels := ckFixZeroLevels.Checked ;
     scADCDisplay.Invalidate ;
     scFLDisplay.FixZeroLevels := ckFixZeroLevels.Checked ;
     scFLDisplay.Invalidate ;
     end;


procedure TViewPlotFrm.scFLDisplayCursorChange(Sender: TObject);
// -------------------------------
// Display cursor position changed
// -------------------------------
var
    ch : Integer ;
begin

    // ROI intensity values
    for ch := 0 to scFLDisplay.NumChannels-1 do begin

        // Fix zero levels at 0 (if required)
        if ckFixZeroLevels.Checked and
           (scFLDisplay.HorizontalCursors[ch] <> 0) then begin
           scFLDisplay.HorizontalCursors[ch] := 0 ;
           end ;

        end ;

    end;


procedure TViewPlotFrm.scADCDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ---------------------------------
// Mouse button depressed over graph
// ---------------------------------
begin

     // Select ADC display for copy/print (deselect others)
     scFLDisplay.DisplaySelected := False ;
     scFLDisplay.Invalidate ;
     scRDisplay.DisplaySelected := False ;
     scRDisplay.Invalidate ;
     scADCDisplay.DisplaySelected := True ;
     scADCDisplay.Invalidate ;

     end;


procedure TViewPlotFrm.scFLDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ---------------------------------
// Mouse button depressed over graph
// ---------------------------------
begin

     // Select fluorescence display for copy/print (deselect others)
     scFLDisplay.DisplaySelected := True ;
     scFLDisplay.Invalidate ;
     scRDisplay.DisplaySelected := False ;
     scRDisplay.Invalidate ;
     scADCDisplay.DisplaySelected := False ;
     scADCDisplay.Invalidate ;

     end;


procedure TViewPlotFrm.CopyPlotImageToClipboard ;
{ -------------------------------------------
  Copy plot image to clipboard as Windows metafile
  ------------------------------------------- }
var
    ScopeDisp : TScopeDisplay ;
begin

     // Copy record on display
     PrintRecFrm.Destination := deClipboard ;
     if scFLDisplay.DisplaySelected then ScopeDisp := scFLDisplay
     else if scRDisplay.DisplaySelected then ScopeDisp := scRDisplay
     else ScopeDisp := scADCDisplay ;

     PrintRecFrm.DisplayObj :=  ScopeDisp ;
     PrintRecFrm.ShowModal ;
     if PrintRecFrm.ModalResult = mrOK then begin
        ScopeDisp.ClearPrinterTitle ;
        ScopeDisp.AddPrinterTitleLine( 'File : ' + MainFrm.IDRFile.FileName ) ;
        ScopeDisp.AddPrinterTitleLine( MainFrm.IDRFile.Ident ) ;
        ScopeDisp.CopyImageToClipboard ;
        end ;

     end ;


procedure TViewPlotFrm.CopyPlotDataToClipboard ;
// -----------------------------------------------------------------
// Copy the data in currently displayed graph to the clipboard
// -----------------------------------------------------------------
begin

     if scFLDisplay.DisplaySelected then scFLDisplay.CopyDataToClipboard
     else if scRDisplay.DisplaySelected then scRDisplay.CopyDataToClipboard
     else scADCDisplay.CopyDataToClipboard ;

     end ;


procedure TViewPlotFrm.scADCDisplayCursorChange(Sender: TObject);
// ----------------------------------------------
// Update cursor labels when readout cursor moved
// ----------------------------------------------
var
    ch : Integer ;
    Channel : TChannel ;
begin

     if TScopeDisplay(Sender).CursorChangeInProgress then Exit ;
     TScopeDisplay(Sender).CursorChangeInProgress := True ;

     // A/D channel values
     for ch := 0 to scADCDisplay.NumChannels-1 do begin

         // Fix zero levels at 0 (if required)
         if ckFixZeroLevels.Checked and
            (scADCDisplay.HorizontalCursors[ch] <> 0) then begin
             scADCDisplay.HorizontalCursors[ch] := 0 ;
            end ;

         // Update channel vertical display limits
         Channel := MainFrm.IDRFile.ADCChannel[ch] ;
         Channel.YMin := scADCDisplay.YMin[ch] ;
         Channel.YMax := scADCDisplay.YMax[ch] ;
         Channel.InUse := scADCDisplay.ChanVisible[ch] ;
         MainFrm.IDRFile.ADCChannel[ch] := Channel ;

         end ;

     // Give this control focus to avoid left/right cursor control arrow keys
     // from changing other controls
     //edRDisplayMax.Setfocus ;

     TScopeDisplay(Sender).CursorChangeInProgress := False ;

     end ;


procedure TViewPlotFrm.edTDisplayKeyPress(Sender: TObject; var Key: Char);
// --------------------------------------------------------
// Request display update when A/D display duration changed
// --------------------------------------------------------
begin
     if key = #13 then begin
        MainFrm.ADCDisplayWindow := edTDisplay.Value ;
        SetDisplayUnits ;
        DisplayTimeCourse( CurrentFrame )
        end ;
     end;


procedure TViewPlotFrm.rbTDisplayUnitsSecsClick(Sender: TObject);
// ------------------------------
// Set Display time units to secs
// ------------------------------
begin
     SetDisplayUnits ;
     end;


procedure TViewPlotFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
begin

     if AllowClose then begin
        NewFLTimeCourseRequired ; // Clears ROI pixel lists
        Action := caFree ;
        end
     else Action := caMinimize ;

     // Save position/size of form within parent window
     MainFrm.SaveFormPosition( Self ) ;

     // Close computation thread (if active)
     StopCompThread ;

     end;


procedure TViewPlotFrm.sbDisplayChange(Sender: TObject);
// ----------------------------------
// Frame selection slider bar changed
// ----------------------------------
begin
     if CurrentFrame <> sbDisplay.Position then
        DisplayTimeCourse( sbDisplay.Position ) ;
     end;

     
procedure TViewPlotFrm.ckDisplayFluorescenceClick(Sender: TObject);
// -------------------------------------------
// Display fluorescence plot check box clicked
// -------------------------------------------
begin

     if cbROI.Items.Count <= 0 then ckDisplayFluorescence.Checked := False ;

     Resize ;

     DisplayTimeCourse( sbDisplay.Position ) ;

     if ckDisplayFluorescence.Checked then begin
        scFLDisplay.DisplaySelected := True ;
        scADCDisplay.DisplaySelected := False ;
        scRDisplay.DisplaySelected := False ;
        end ;

     end;


procedure TViewPlotFrm.cbROIChange(Sender: TObject);
// ------------------------
// Fluorescence ROI changed
// ------------------------
begin
     DisplayTimeCourse( sbDisplay.Position ) ;
     end;


procedure TViewPlotFrm.ZoomOutAll ;
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
        scRDisplay.YMax[i] := scFLDisplay.MaxADCValue ;
        scRDisplay.YMin[i] := 0.0 ;
        end ;
    scFLDisplay.Invalidate ;
    scRDisplay.Invalidate ;

    // Set A/D plots
    scADCDisplay.ZoomOut ;

    end ;


procedure TViewPlotFrm.cbSubROIChange(Sender: TObject);
// ------------------------------------
// Subtraction fluorescence ROI changed
// ------------------------------------
begin
     DisplayTimeCourse( sbDisplay.Position ) ;
     end;

procedure TViewPlotFrm.ckDisplayRClick(Sender: TObject);
// -------------------------------------------
// Display ratio plot check box clicked
// -------------------------------------------
begin

     if cbROI.Items.Count <= 0 then ckDisplayR.Checked := False ;
     if MainFrm.IDRFile.SpectralDataFile then ckDisplayR.Checked := False ;

     Resize ;

     DisplayTimeCourse( sbDisplay.Position ) ;

     if ckDisplayR.Checked then begin
        scFLDisplay.DisplaySelected := False ;
        scADCDisplay.DisplaySelected := False ;
        scRDisplay.DisplaySelected := True ;
        end ;

     end;


procedure TViewPlotFrm.cbNumeratorChange(Sender: TObject);
// ------------------------
// Ratio numerator changed
// ------------------------
begin
     MainFrm.TimeCourseRatioNumerator := cbNumerator.ItemIndex ;
     DisplayTimeCourse( sbDisplay.Position ) ;
     end;


procedure TViewPlotFrm.cbDenominatorChange(Sender: TObject);
// ------------------------
// Ratio denominator changed
// ------------------------
begin
     MainFrm.TimeCourseRatioDenominator := cbDenominator.ItemIndex ;
     DisplayTimeCourse( sbDisplay.Position ) ;
     end;


procedure TViewPlotFrm.edRDisplayMaxKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------
// Ratio display maximum changed
// ------------------------
begin
     if Key = #13 then begin
        MainFrm.TimeCourseRatioDisplayMax := edRDisplayMax.Value ;
        DisplayTimeCourse( sbDisplay.Position ) ;
        end ;
     end;


procedure TViewPlotFrm.scRDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ---------------------------------
// Mouse button depressed over graph
// ---------------------------------
begin

     // Select ratio display for copy/print (deselect others)
     scFLDisplay.DisplaySelected := False ;
     scFLDisplay.Invalidate ;
     scRDisplay.DisplaySelected := True ;
     scRDisplay.Invalidate ;
     scADCDisplay.DisplaySelected := False ;
     scADCDisplay.Invalidate ;

     end;

procedure TViewPlotFrm.scRDisplayCursorChange(Sender: TObject);
// -------------------------------
// Display cursor position changed
// -------------------------------
var
    ch : Integer ;
begin

    // ROI intensity values
    for ch := 0 to scRDisplay.NumChannels-1 do begin

        // Fix zero levels at 0 (if required)
        if ckFixZeroLevels.Checked and
           (scRDisplay.HorizontalCursors[ch] <> 0) then begin
           scRDisplay.HorizontalCursors[ch] := 0 ;
           end ;

        end ;

    // Give this control focus to avoid left/right cursor control arrow keys
    // from changing other controls
    //edRDisplayMax.Setfocus ;

    end;


procedure TViewPlotFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
// --------------------
//  Process key presses
// --------------------
var
    ScopeDisp : TScopeDisplay ;
begin

     // Get selected display
     if scFLDisplay.DisplaySelected then ScopeDisp := scFLDisplay
     else if scRDisplay.DisplaySelected then ScopeDisp := scRDisplay
                                    else  ScopeDisp := scADCDisplay ;

     case key of
          VK_LEFT : ScopeDisp.MoveActiveVerticalCursor(-1) ;
          VK_RIGHT : ScopeDisp.MoveActiveVerticalCursor(1) ;
          end ;

     end;

procedure TViewPlotFrm.TimerTimer(Sender: TObject);
// -------------------------
// Plot time course of ROIs
// -------------------------
begin

    if CompDone then
       begin
       // Reload from file and update display if time course computation complete
       NewFile ;
       sbDisplay.Position := 1 ;
       DisplayTimeCourse( sbDisplay.Position ) ;
       CompDone := False ;
       end ;

    end ;


procedure TViewPlotFrm.edRatioExclusionThresholdKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------
// Exclusion threshold changed
// ------------------------
begin
     if Key = #13 then begin
        MainFrm.TimeCourseRatioThreshold := edRatioExclusionThreshold.Value ;
        DisplayTimeCourse( sbDisplay.Position ) ;
        end ;
     end;


procedure TViewPlotFrm.FormDestroy(Sender: TObject);
// ------------------------------
// Tidy up when form is destroyed
// ------------------------------
var
    i : Integer ;
begin

     if ROITimeCourseBuf <> Nil then FreeMem( ROITimeCourseBuf ) ;
     ROITimeCourseBuf := Nil ;
     if FLDisplayBuf <> Nil then Freemem(FLDisplayBuf) ;
     FLDisplayBuf := Nil ;
     if RDisplayBuf <> Nil then Freemem(RDisplayBuf) ;
     RDisplayBuf := Nil ;
     if ADCBuf <> Nil then FreeMem(ADCBuf) ;
     ADCBuf := Nil ;

     for i := 0 to High(ROIPixelList) do
         begin
         if ROIPixelList[i] <> Nil then FreeMem(ROIPixelList[i]) ;
         ROIPixelList[i] := Nil ;
         end ;

     end;


procedure TViewPlotFrm.FormCreate(Sender: TObject);
// -------------------------
// Initialise memory buffers
// -------------------------
var
    i : Integer ;
begin

    ROITimeCourseBuf := Nil ;
    ADCBuf := Nil ;
    FLDisplayBuf := Nil ;
    RDisplayBuf := Nil ;
    CompThread := Nil ;
    CompDone := False ;
    ROITCAvailable := False ;

    for i := 0 to High(ROIPixelList) do ROIPixelList[i] := Nil ;
    for i := 0 to High(ROINumPixels) do ROINumPixels[i] := 0 ;

    end;


procedure TViewPlotFrm.StopCompThread ;
// ------------------------
// Stop and free computation thread
// ------------------------
begin
     if CompThread = Nil Then Exit ;
     CompThread.Terminate ;
     CompThread.WaitFor ;
     FreeAndNil(CompThread);
end;


end.
