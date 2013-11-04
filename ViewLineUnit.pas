unit ViewLineUnit;
// =============================================================================
// WinFluor - Windows Fluoresence Program - Line scan display module
// (c) J. Dempster, University of Strathclyde, 2001-4, All Rights Reserved
// =============================================================================
// 9.06.04 Started (In O'Hare airport waiting for a delayed plane)
// 17.07.04 Completed (sent to David Wokosin)
// 20.12.04 Time course background subtraction option added
// 10.06.05 Image start delay offset added to line scan image
// 14.09.05 .MagnifyADCChannelDisplay and .ReduceADCChannelDisplay added
//          Display duration Double/Half buttons added
// 11.10.05 .MagnifyChannelDisplay and .ReduceChannelDisplay now also work on fluor. channels
// 03.12.05 Display updates now run from timer routine
// 19.09.06 Plot/display alignment no longer disturbed when cursor moved
// 06.07.07 ScopeDisplay updated. Fix Zero Levels now works
//          Cursor readout now integrated into display
// 22.07.07 Vertical cursors can now be moved using left/right arrow keys
// 25.07.07 Ratio display added, Frames now selected using drop-down list
// 26.01.09 Line scan magnification off 25% added
//          Line scan & ratio time course display now has minimum height ensured
// 05.08.10 JD LoadLineScan now fills up display buffer to end
// 07.09.10 JD List "out of range" exception when % display zoom exceeds line scan zoom prevented 
// 06.03.13 DE Made FL and R displays behave more like ADC channels (times would
//          not agree, and half-range button would stop at a limit based on
//          window width)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ExtCtrls, ScopeDisplay, RangeEdit,
  HTMLLabel, ClipBrd, Printers, Math, strutils ;

type
  TSmallIntArray = Array[0..9999999] of SmallInt ;
  PSmallIntArray = ^TSmallIntArray ;
  TIntArray = Array[0..9999999] of Integer ;
  PIntArray = ^TIntArray ;
  TImageCursorInUse = (imcNone,imcPixelTimeCourse,imcBackgroundTimeCourse,imcTimeCursor) ;

  TViewLineFrm = class(TForm)
    ControlsGrp: TGroupBox;
    ControlGrp: TGroupBox;
    DisplayGrp: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    bOptimiseContrast: TButton;
    bFullScale: TButton;
    edDisplayIntensityRange: TRangeEdit;
    cbPalette: TComboBox;
    cbDisplayZoom: TComboBox;
    IdentGrp: TGroupBox;
    Label3: TLabel;
    edIdent: TEdit;
    ImageGrp: TGroupBox;
    Image: TImage;
    ckDisplayCalBar: TCheckBox;
    scADCDisplay: TScopeDisplay;
    ckFixZeroLevels: TCheckBox;
    scFLDisplay: TScopeDisplay;
    sbDisplay: TScrollBar;
    TimeCourseGrp: TGroupBox;
    edTimeCoursePixel: TValidatedEdit;
    Label2: TLabel;
    Label4: TLabel;
    edNumPixelsAvg: TValidatedEdit;
    edStartLine: TValidatedEdit;
    Label7: TLabel;
    Label1: TLabel;
    edLineScanInterval: TValidatedEdit;
    Shape1: TShape;
    ckSubtractBackground: TCheckBox;
    Label8: TLabel;
    edBackgroundPixel: TValidatedEdit;
    Label9: TLabel;
    edImageStartDelay: TValidatedEdit;
    TDisplayPanel: TPanel;
    edTDisplay: TValidatedEdit;
    rbTDisplayUnitMins: TRadioButton;
    rbTDisplayUnitsSecs: TRadioButton;
    bTDisplayDouble: TButton;
    bTDisplayHalf: TButton;
    Timer: TTimer;
    scRDisplay: TScopeDisplay;
    RatioGrp: TGroupBox;
    ckDisplayR: TCheckBox;
    ckDisplayFluorescence: TCheckBox;
    ckDisplayADC: TCheckBox;
    cbFrameNum: TComboBox;
    RatioPanel: TPanel;
    Shape2: TShape;
    Label10: TLabel;
    Label11: TLabel;
    cbNumerator: TComboBox;
    cbDenominator: TComboBox;
    edRDisplayMax: TValidatedEdit;
    edRatioThreshold: TValidatedEdit;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure rbTDisplayUnitsSecsClick(Sender: TObject);
    procedure edTDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure sbDisplayChange(Sender: TObject);
    procedure edTimeCoursePixelKeyPress(Sender: TObject; var Key: Char);
    procedure edNumPixelsAvgKeyPress(Sender: TObject; var Key: Char);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scFLDisplayCursorChange(Sender: TObject);
    procedure bFullScaleClick(Sender: TObject);
    procedure bOptimiseContrastClick(Sender: TObject);
    procedure cbPaletteChange(Sender: TObject);
    procedure cbDisplayZoomChange(Sender: TObject);
    procedure scFLDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scADCDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure scADCDisplayCursorChange(Sender: TObject);
    procedure edLineScanIntervalKeyPress(Sender: TObject; var Key: Char);
    procedure edStartLineKeyPress(Sender: TObject; var Key: Char);
    procedure edIdentKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ckSubtractBackgroundClick(Sender: TObject);
    procedure edDisplayIntensityRangeKeyPress(Sender: TObject;
      var Key: Char);
    procedure edImageStartDelayKeyPress(Sender: TObject; var Key: Char);
    procedure bTDisplayHalfClick(Sender: TObject);
    procedure bTDisplayDoubleClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ckFixZeroLevelsClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ckDisplayFluorescenceClick(Sender: TObject);
    procedure ckDisplayADCClick(Sender: TObject);
    procedure ckDisplayRClick(Sender: TObject);
    procedure cbFrameNumChange(Sender: TObject);
    procedure cbNumeratorChange(Sender: TObject);
    procedure cbDenominatorChange(Sender: TObject);
    procedure edRDisplayMaxKeyPress(Sender: TObject; var Key: Char);
    procedure edRatioThresholdKeyPress(Sender: TObject; var Key: Char);
    procedure scRDisplayCursorChange(Sender: TObject);
    procedure scRDisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    BitMap : TBitMap ;  // Image internal bitmap
    //ADCBuf : Array[0..99999] of SmallInt ;
    ADCBuf : PSmallIntArray ;
    TCCursor : Integer ;    // Fluoresence time course cursor
    TCCursorPos : Integer ; // Position of cursor
    RCursor : Integer ;
    ImageCursorInUse : TImageCursorInUse ;
    ADCReadoutCursor : Integer ; // A/D readout cursor index on scADCDisplay

    NumDisplayLines : Integer ;
    LSDisplayMaxBlocks : Integer ;  // Max. no. of min./max. blocks in display
    LSDisplayNumLinesPerPoint : Single ; // No. of scans per point displayed
    LSDisplayNumPointsPerBlock : Integer ; // No. display points per block
    NumDisplayBlocks : Integer ;  // No. of min./max. blocks in display
    LSDisplayBlockSize : Integer ;  // No. scan lines / block
    NumDisplayPoints : Integer ;  // No. of time points in display
    NumPixelsInImageBuf : Integer ; // No. of pixels in displayed line scan image
    LSDisplayCursorUpdated : Boolean ;
    //NumPointsPerBlock : Integer ;

    // A/D signal display variables
    ADCDisplayScansPerPoint : Single ;    // No. of A/D channel scans / display point
    ADCDisplayBlockSize : Integer ;       // No. of A/D channel scans / min-max block
    ADCDisplayPointsPerBlock : Integer ;  // No. of displayed points / min-max block
    MaxADCDisplayPoints : Integer ;       // No. of pixels in displayed line scan image
    ADCDisplayCursorUpdated : Boolean ;

    TimeCoursePixel : Integer ;
    BackgroundPixel : Integer ;

    MouseDown : Boolean ;
    DisplayUpdateRequired : Boolean ;   // TRUE = Update of display required
    TimerInProgress : Boolean ;         // TRUE = Timer code running

    PImageBuf : PIntArray ;         // Line scan compressed image buffer
    pImageN : PIntArray ;
    pImageD : PIntArray ;

    DisplayBuf : PIntArray ;   // Pixel time course buffer
    RDisplayBuf : PIntArray ;   // Pixel time course buffer
    IMin : PIntArray ;              // Min. value time course plot
    IMinAt : PIntArray ;            // Min. sample point
    IMax : PIntArray ;              // Max. value time course plot
    IMaxAt : PIntArray ;            // Max. sample point

    // Clipboard image data
    ClipboardImageFormat : Word ;
    ClipboardImageData: THandle ;
    ClipboardPalette : HPalette ;

    procedure LoadImage ;
    procedure DisplayLineScanImage ;
    procedure LoadLineScanImage(
              FrameNum : Integer ;
              var pImageBuf : PIntArray
              ) ;

    procedure UpdateImageCursors( CursorPos : Integer ) ;
    procedure DisplayPixelIntensityTimeCourse ;
    procedure DisplayRatioTimeCourse ;
    procedure SetDisplayUnits ;
    procedure UpdateADCDisplayCursor ;
    procedure UpdateLSDisplayCursor ;
    procedure DisplayADCChannels ;
    function GetDisplayGrid : Boolean ;
    procedure SetDisplayGrid( Value : Boolean ) ;

  public
    { Public declarations }
    FrameWidth : Integer ;
    FrameHeight : Integer ;
    NumLinesInFile : Integer ;   // No. of scan lines in file

    NumPixelsPerFrame : Integer ; // No. of pixels in image
    NumBytesPerFrame : Integer ;
    NumBytesPerPixel : Integer ;  // No. of bytes per pixel in PDisplayBuf

    DisplayZoom : Single ;         // Display zoom factor (0.5,1.0,2.0)
    FrameCounter : Integer ;   // Frames displayed counter
    SelectedFrameType : Integer ;                        // Type selected by user
    FrameIndex : Integer ;        // Index of current frame

    PLineScanBuf : Pointer ;      // Line scan data buffer

    ImageAvailable : Boolean ;    // True if an image is available
    PlotAvailable : Boolean ;     // True if plot is selected for copying

    procedure NewFile ;
    procedure CopyImageToClipboard ;
    procedure CopyPlotImageToClipboard ;
    procedure CopyPlotDataToClipboard ;
    procedure PrintImage ;
    procedure PrintPlot ;
    function TimeCourseIntensity (
             LineNum : Integer ;
             FrameNum : Integer
              ) : Single ;
    procedure MagnifyChannelDisplay( ChanNum : Integer ) ;
    procedure ReduceChannelDisplay( ChanNum : Integer ) ;
    procedure ZoomOutAll ;

    Property DisplayGrid : Boolean read GetDisplayGrid write SetDisplayGrid ;

  end;

var
  ViewLineFrm: TViewLineFrm;

implementation

uses Main, PrintRec, IDRFile, TimeCourseUnit, EventAnalysisUnit, SmoothDifferentiateUnit ;

{$R *.dfm}


procedure TViewLineFrm.FormCreate(Sender: TObject);
// ------------------------------------
// Initialisations when form is created
// ------------------------------------
begin

     ADCBuf := Nil ;
     PLineScanBuf := Nil ;
     PImageBuf := Nil ;

     pImageN := Nil ;
     pImageD := Nil ;

     BitMap := Nil ;

     DisplayBuf := Nil ;
     RDisplayBuf := Nil ;

     IMin := Nil ;
     IMinAt := Nil ;
     IMax := Nil ;
     IMaxAt := Nil ;
     DisplayUpdateRequired := False ;
     TimerInProgress := False ;

     end;


procedure TViewLineFrm.FormResize(Sender: TObject);
// -------------------------------------------------
// Adjust control sizes/positions when form re-sized
// -------------------------------------------------
const
    MinDisplayHeight = 100 ;
var
    NumDisplayChannels : Integer ;
    NumDisplays : Integer ;
    DisplayHeight : Integer ;
    iTop : Integer ;
begin
     // Group size
     ImageGrp.Width := Max( ClientWidth - ImageGrp.Left - 5,10 ) ;
     ImageGrp.Height := Max( ClientHeight - ImageGrp.Top - 5,10) ;
     ControlsGrp.Height := Max( ClientHeight - ControlsGrp.Top - 5,10) ;

     // Ident text group
     IdentGrp.Width := ImageGrp.Width ;
     edIdent.Width := IdentGrp.ClientWidth - edIdent.Left - 5 ;

     // Image/display widths

     // Make width of chart displays same as image
     scFLDisplay.Width := Max( ImageGrp.ClientWidth - scFLDisplay.Left - 5,2) ;
     scRDisplay.Width := scFLDisplay.Width ;
     scADCDisplay.Width := scFLDisplay.Width ;
     sbDisplay.Width := scFLDisplay.Width ;

     Image.Width := Max( scFLDisplay.Width + scFLDisplay.Left - Image.Left - 15,2) ;
     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])/100.0 ;

     Image.Height := Round(FrameWidth*DisplayZoom) ;

     // Controls at bottom of display
     TDisplayPanel.Top := ImageGrp.ClientHeight - TDisplayPanel.Height - 2 ;
     TDisplayPanel.Left := scADCDisplay.Left
                           + scADCDisplay.Width
                           - TDisplayPanel.Width ;

     ckFixZeroLevels.Top := TDisplayPanel.Top ;

     // Scroll bar determines bottom of plot area
     sbDisplay.Top := ckFixZeroLevels.Top - sbDisplay.Height - 2 ;

     // Top of plot area
     ckDisplayCalBar.Top := Image.Top + Image.Height + 1 ;
     scFLDisplay.Top := ckDisplayCalBar.Top + ckDisplayCalBar.Height + 2 ;

     if MainFrm.IDRFile.ADCNumChannels <= 0 then begin
        ckDisplayADC.Visible := False ;
        ckDisplayADC.Checked := False ;
        end ;

     scFLDisplay.Visible := ckDisplayFluorescence.Checked ;
     scADCDisplay.Visible := ckDisplayADC.Checked ;
     scRDisplay.Visible := ckDisplayR.Checked ;

     NumDisplays := 0 ;
     if scFLDisplay.Visible then Inc(NumDisplays) ;
     if scADCDisplay.Visible then NumDisplays := NumDisplays + scADCDisplay.NumChannels ;
     if scRDisplay.Visible then Inc(NumDisplays) ;
     DisplayHeight := Max( sbDisplay.Top - scFLDisplay.Top - 1, 2 ) div Max(NumDisplays,1) ;
     DisplayHeight := Max(DisplayHeight,MinDisplayHeight) ;

     iTop := scFLDisplay.Top ;

     // Fluorescence display
     if scFLDisplay.Visible then begin
        scFLDisplay.Height := DisplayHeight - 5 ;
        iTop := iTop + DisplayHeight + 5 ;
        end
     else scFLDisplay.Height := 2 ;

     // Ratio display
     scRDisplay.Top := iTop ;
     if scRDisplay.Visible then begin
        scRDisplay.Height := DisplayHeight - 5 ;
        iTop := iTop + DisplayHeight ;
        end
     else scRDisplay.Height := 2 ;

     // A/D display
     scADCDisplay.Top := iTop ;
     if scADCDisplay.Visible then begin
        scADCDisplay.Height := sbDisplay.Top - scADCDisplay.Top - 1 ;
        iTop := iTop + scADCDisplay.Height + 5 ;
        end
     else scADCDisplay.Height := 2 ;

     sbDisplay.Top := iTop - 4 ;

     // Show ratio panel if ratio selected
     RatioPanel.Visible := ckDisplayR.Checked ;

     DisplayUpdateRequired := True ;

     end;


procedure TViewLineFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
begin

     MainFrm.IDRFile.LineScan := True ;
     ImageAvailable := False ;

     // Display magnification factor
     cbDisplayZoom.Clear ;
     cbDisplayZoom.Items.AddObject( '  25% ',TObject(25) ) ;
     cbDisplayZoom.Items.AddObject( '  50% ',TObject(50) ) ;
     cbDisplayZoom.Items.AddObject( ' 100% ',TObject(100) ) ;
     cbDisplayZoom.Items.AddObject( ' 200% ',TObject(200) ) ;
     if MainFrm.DisplayZoomIndex < 0 then MainFrm.DisplayZoomIndex := 0;
     cbDisplayZoom.ItemIndex := Min(Max(MainFrm.DisplayZoomIndex,0),cbDisplayZoom.Items.Count-1) ;
     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])/100.0 ;

     // Intensity display palette
     cbPalette.Clear ;
     cbPalette.Items.AddObject(' Grey scale', TObject(palGrey)) ;
     cbPalette.Items.AddObject(' False colour', TObject(palFalseColor)) ;
     cbPalette.Items.AddObject(' Red scale', TObject(palRed)) ;
     cbPalette.Items.AddObject(' Green scale', TObject(palGreen)) ;
     cbPalette.Items.AddObject(' Blue scale', TObject(palBlue)) ;
     cbPalette.ItemIndex := cbPalette.Items.IndexOfObject(TObject(MainFrm.PaletteType)) ;

     SelectedFrameType := 0 ;

     // Duration of analogue signals display window
     edTDisplay.Value := MainFrm.ADCDisplayWindow ;

     // Create line scan display bit map
     BitMap := TBitMap.Create ;

     // Initialise/update controls
     NewFile ;

     ImageCursorInUse := imcNone ;

     // FrameIndex always zero, since only one frame in buffer
     FrameIndex := 0 ;

     // Adjust form to accommodate size of image
     ClientWidth := ImageGrp.Left + ImageGrp.Width + 5 ;
     ClientHeight := Max( ImageGrp.Top + ImageGrp.Height + 100,
                              ControlsGrp.Top + ControlsGrp.Height + 5 ) ;
     Top := 10 ;
     Left := 10 ;
     ClientHeight := MainFrm.ClientHeight - Top - 50 ;

     ControlsGrp.Height := ClientHeight - ControlsGrp.Top - 5 ;

     // Set display window duration units
     SetDisplayUnits ;

     MouseDown := False ;

     LSDisplayCursorUpdated := False ;
     ADCDisplayCursorUpdated := False ;

     scFLDisplay.DisplaySelected := True ;
     scADCDisplay.DisplaySelected := False ;

     Timer.Enabled := True ;
     //NewFileNeeded := True ;
     //DisplayUpdateRequired := True ;

     end;


procedure TViewLineFrm.NewFile ;
// -----------------------------------------------
// Update controls when data file has been changed
// -----------------------------------------------
var
     ch,i,iKeep : Integer ;
begin

     FrameWidth := MainFrm.IDRFile.FrameWidth ;
     FrameHeight := MainFrm.IDRFile.FrameHeight ;
     NumPixelsPerFrame := MainFrm.IDRFile.NumPixelsPerFrame ;
     NumBytesPerPixel := MainFrm.IDRFile.NumBytesPerPixel ;
     NumBytesPerFrame := NumBytesPerPixel*NumPixelsPerFrame ;

     // Set up display starting line selector
     NumLinesInFile := MainFrm.IDRFile.FrameHeight ;

     // Time interval between line scans
     edLineScanInterval.Value := (MainFrm.IDRFile.FrameInterval/NumLinesInFile) ;

     // Delay between start of A/D sampling and image acquisition
     edImageStartDelay.Value := MainFrm.IDRFile.ImageStartDelay ;

     // Set line scan display slider bar range
     sbDisplay.Max := NumLinesInFile - 1 ;
     sbDisplay.Position := 0 ;
     edStartLine.HiLimit := sbDisplay.Max ;

     if ADCBuf <> Nil then FreeMem(ADCBuf) ;
     GetMem( ADCBuf, 100000*2) ;

     // Allocate line scan data buffer
     if PLineScanBuf <> Nil then FreeMem(PLineScanBuf) ;
     GetMem( PLineScanBuf, NumBytesPerFrame*MainFrm.IDRFile.NumFrames ) ;

     // Frame display selection list
     cbFrameNum.Clear ;
     for i := 0 to MainFrm.IDRFile.NumFrames-1 do begin
         if MainFrm.IDRFile.FrameType[i] = '' then
            MainFrm.IDRFile.FrameType[i] := format('Fr%d',[i+1]) ;
         cbFrameNum.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
         end ;
     cbFrameNum.ItemIndex := 0 ;

     // Ratio selection lists
     iKeep := cbNumerator.ItemIndex ;
     cbNumerator.Items.Assign(cbFrameNum.Items) ;
     cbNumerator.ItemIndex := Max(Min(iKeep,cbNumerator.Items.Count-1),0) ;
     iKeep := cbDenominator.ItemIndex ;
     cbDenominator.Items.Assign(cbFrameNum.Items) ;
     cbDenominator.ItemIndex := Max(Min(iKeep,cbDenominator.Items.Count-1),0) ;

     // Load line scan images
     LoadImage ;

     // Experiment ident line
     edIdent.Text := MainFrm.IDRFile.Ident ;

     // Set size and pen/brush characteristics of images in use

     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])/100.0 ;

     Image.Canvas.Pen.Color := clWhite ;
     Image.Canvas.Brush.Style := bsClear ;
     Image.Canvas.Font.Color := clWhite ;
     Image.Canvas.TextFlags := 0 ;
     Image.Canvas.Pen.Mode := pmXOR ;
     Image.Canvas.Font.Name := 'Arial' ;
     Image.Canvas.Font.Size := 8 ;
     Image.Canvas.Font.Color := clBlue ;

     // Set colour palette
     if BitMap <> Nil then begin
        MainFrm.SetPalette( BitMap, MainFrm.PaletteType ) ;
        end ;

     // Update component sizes on form
     //Resize ;

     // Set display intensity range and update look-up table
     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[SelectedFrameType] ;
     edDisplayIntensityRange.HiLimit := MainFrm.IDRFile.GreyMax ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[SelectedFrameType]  ;
     MainFrm.GreyHi[SelectedFrameType]  := Round(edDisplayIntensityRange.HiValue) ;

     MainFrm.UpdateLUT( 0, MainFrm.IDRFile.GreyMax ) ;

     // Set limits of frame select slider and edit box

     scFLDisplay.NumChannels := 1 ;
     scFLDisplay.MaxADCValue := MainFrm.IDRFile.GreyMax ;
     scFLDisplay.MinADCValue := -MainFrm.IDRFile.GreyMax ;
     scFLDisplay.NumBytesPerSample := 4 ;

     scFLDisplay.ChanOffsets[0] := 0 ;
     scFLDisplay.ChanUnits[0] := '' ;
     scFLDisplay.ChanName[0] := cbFrameNum.Text ;
     scFLDisplay.ChanScale[0] := 1.0 ;
     scFLDisplay.yMin[0] := 0 ;
     scFLDisplay.yMax[0] := MainFrm.IDRFile.GreyMax ;

     scFLDisplay.ChanVisible[0] := True ;

     // Create readout cursor
     scFLDisplay.ClearVerticalCursors ;
     TCCursor := scFLDisplay.AddVerticalCursor(-1,clGreen,'?y?t') ;
     scFLDisplay.VerticalCursors[TCCursor] := 10 ;
     // Zero level cursor
     scFLDisplay.AddHorizontalCursor( 0, clBlue, True, 'z' ) ;
     scFLDisplay.HorizontalCursors[0] := 0 ;

     // Set up ratio time course display
     // ------------------------------

     scRDisplay.MinADCValue := -MainFrm.IDRFile.GreyMax ;
     scRDisplay.MaxADCValue := MainFrm.IDRFile.GreyMax ;
     scRDisplay.NumChannels := 1 ;

     // Add zero level cursors
     scRDisplay.ClearHorizontalCursors ;
     for ch :=  0 to scRDisplay.NumChannels-1 do
         scRDisplay.AddHorizontalCursor( ch, clGreen, True, 'z' ) ;

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

    scRDisplay.SetDataBuf( @RDisplayBuf ) ;
    scRDisplay.NumBytesPerSample := 4 ;

    scRDisplay.ClearVerticalCursors ;
    RCursor := scRDisplay.AddVerticalCursor(-1,clGreen,'?r') ;
    scRDisplay.VerticalCursors[RCursor] := scRDisplay.MaxPoints div 2 ;
    scRDisplay.DisplaySelected := False ;

    // Set up analogue display channels
    // --------------------------------

    if MainFrm.IDRFile.ADCNumChannels > 0 then begin

       scADCDisplay.MaxADCValue := MainFrm.IDRFile.ADCMaxValue ;
       scADCDisplay.MinADCValue := -MainFrm.IDRFile.ADCMaxValue - 1 ;

       // Add zero level cursors
       scADCDisplay.ClearHorizontalCursors ;
       for ch :=  0 to MainFrm.IDRFile.ADCNumChannels-1 do
           scADCDisplay.AddHorizontalCursor( ch, clGReen, True, 'z' ) ;

       scADCDisplay.NumPoints := 0 ;
       scADCDisplay.NumChannels := MainFrm.IDRFile.ADCNumChannels ;

       { Set channel information }
       for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
           scADCDisplay.ChanOffsets[ch] := MainFrm.IDRFile.ADCChannel[ch].ChannelOffset ;
           scADCDisplay.ChanUnits[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCUnits ;
           scADCDisplay.ChanName[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCName ;
           scADCDisplay.ChanScale[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCScale ;
           scADCDisplay.yMin[ch] := MainFrm.IDRFile.ADCChannel[ch].yMin ;
           scADCDisplay.yMax[ch] := MainFrm.IDRFile.ADCChannel[ch].yMax ;
           scADCDisplay.HorizontalCursors[ch] := MainFrm.IDRFile.ADCChannel[ch].ADCZero ;
           scADCDisplay.ChanVisible[ch] := MainFrm.IDRFile.ADCChannel[ch].InUse ;
           end ;

       // Set display time units
       //SetDisplayUnits ;

       scADCDisplay.SetDataBuf( ADCBuf ) ;

       scADCDisplay.ClearVerticalCursors ;
       ADCReadoutCursor := scADCDisplay.AddVerticalCursor(-1,clGreen,'?y') ;
       scADCDisplay.VerticalCursors[ADCReadoutCursor] := scADCDisplay.MaxPoints div 2 ;
       scADCDisplay.Visible := True ;

       end
    else scADCDisplay.Visible := False ;

    edTimeCoursePixel.Value := Max(Min(MainFrm.IDRFile.LSTimeCoursePixel,FrameWidth-1),0) ;
    edNumPixelsAvg.Value  := Min(Max(MainFrm.IDRFile.LSTimeCourseNumAvg,1),FrameWidth-1) ;
    edBackgroundPixel.Value := Max(Min(MainFrm.IDRFile.LSTimeCourseBackgroundPixel,FrameWidth-1),0) ;
    ckSubtractBackground.Checked := MainFrm.IDRFile.LSSubtractBackground ;

    PlotAvailable := True ;
    DisplayUpdateRequired := True ;

    end ;


procedure TViewLineFrm.LoadImage ;
// ------------------------------
// Load line scan image from file
// ------------------------------
var
    i : Integer ;
begin
     FrameCounter := cbFrameNum.ItemIndex ;
     for i := 1 to MainFrm.IDRFile.NumFrames do begin
         MainFrm.IDRFile.LoadFrame( i, Pointer(
                                       Cardinal(PLineScanBuf)
                                       + (i-1)*NumBytesPerFrame)) ;
         end ;

     ImageAvailable := True ;

     end ;


procedure TViewLineFrm.DisplayLineScanImage ;
// ----------------------------------
// Display section of line scan image
// ----------------------------------
var
     StartLine : Integer ;
     NumDisplayLines : Integer ;
     i,j,x,y,Line : Integer ;
     PBMLine : PByteArray ;
     IPix : Integer ;
     BlockCount : Integer ;
     BlockNum : Integer ;
     iFrom : Integer ;
     iTo : Integer ;
     DisplayLine : Single ;
     Done : Boolean ;
     pBufStart : Pointer ;
begin

     // No. of scans lines displayed
     NumDisplayLines := Round(edTDisplay.Value/edLineScanInterval.Value) ;
     // No. of min./max. lines pairs within image
     LSDisplayMaxBlocks := Max( ImageGrp.ClientWidth - Image.Left - 25,4) div 2 ;
     // No. of lines per min./max. block

     LSDisplayBlockSize := Max( NumDisplayLines div LSDisplayMaxBlocks,1) ;
     LSDisplayNumPointsPerBlock := Min(LSDisplayBlockSize,2) ;
     if LSDisplayBlockSize = 1 then LSDisplayMaxBlocks := LSDisplayMaxBlocks*2 ;

     // Make no. of lines displayed multiple of block size
     NumDisplayLines := LSDisplayMaxBlocks*LSDisplayBlockSize ;
     // Update display duration
     // edTDisplay.Value := NumDisplayLines*edLineScanInterval.Value ;
     edTDisplay.Value := Min(NumDisplayLines*edLineScanInterval.Value,
                             edTDisplay.Value) ;

     // Set large change increment of line selection slider bar
     sbDisplay.LargeChange := NumDisplayLines div 4 ;

     // Set image & bitmap size
     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])/100.0 ;
     Image.Width := LSDisplayMaxBlocks*Min(LSDisplayBlockSize,2) ;
     Image.Height := Round(FrameWidth*DisplayZoom) ;
     BitMap.Width := Image.Width ;
     BitMap.Height := Image.Height ;

     // Create compress/rotated image
     LoadLineScanImage( cbFrameNum.ItemIndex, pImageBuf ) ;

     // Copy image data buffer into display bitmap
     DisplayLine := 0.0 ;
     for y := 0 to BitMap.Height-1 do begin
         PBMLine := BitMap.ScanLine[y] ;
         StartLine := Trunc(DisplayLine)*BitMap.Width ;
         for x := 0 to BitMap.Width-1 do begin
             PBMLine[x] := MainFrm.LUTs[PImageBuf^[StartLine + x]]
             end ;
         DisplayLine := DisplayLine + (1.0/DisplayZoom) ;
         end ;

     // Update line scan image
     Image.Picture.Assign(BitMap) ;

     // Was being called twice, here and in TimerTimer
     // DisplayPixelIntensityTimeCourse ;

     ImageAvailable := True ;

     end ;


procedure TViewLineFrm.LoadLineScanImage(
          FrameNum : Integer ;
          var pImageBuf : PIntArray
          ) ;
// ----------------------------------------------
// Display section of line scan image into buffer
// ----------------------------------------------
var
     StartLine : Integer ;
     NumDisplayLines : Integer ;
     i,j,x,y,Line : Integer ;
     PBMLine : PByteArray ;
     IPix : Integer ;
     BlockCount : Integer ;
     BlockNum : Integer ;
     iFrom : Integer ;
     iTo : Integer ;
     DisplayLine : Single ;
     Done : Boolean ;
     pBufStart : Pointer ;
begin

     if PLineScanBuf = Nil then Exit ;

     // Allocate line scan image buffer
     if PImageBuf <> Nil then FreeMem(PImageBuf) ;
     GetMem( PImageBuf, LSDisplayMaxBlocks*FrameWidth*8 ) ;

     // Allocate min./max. work buffers
     if IMin <> Nil then FreeMem( IMin ) ;
     GetMem( IMin, LSDisplayMaxBlocks*FrameWidth*4 ) ;
     if IMinAt <> Nil then FreeMem( IMinAt ) ;
     GetMem( IMinAt, LSDisplayMaxBlocks*FrameWidth*4 ) ;
     if IMax <> Nil then FreeMem( IMax ) ;
     GetMem( IMax, LSDisplayMaxBlocks*FrameWidth*4 ) ;
     if IMaxAt <> Nil then FreeMem( IMaxAt ) ;
     GetMem( IMaxAt, LSDisplayMaxBlocks*FrameWidth*4 ) ;

     StartLine := sbDisplay.Position ;
     edStartLine.Value := sbDisplay.Position ;

     // Apply image start delay offset
     StartLine := StartLine - Round(edImageStartDelay.Value/edLineScanInterval.Value) ;
     //StartLine := Min(Max(0,StartLine),FrameHeight-1) ;

     // Initialise min./max. block array
     Line := StartLine ;
     BlockCount := 0 ;
     BlockNum := 0 ;
     for i := 0 to (LSDisplayMaxBlocks*FrameWidth)-1 do begin
         IMin[i] := High(IPix) ;
         IMax[i] := Low(IPix) ;
         end ;

     // Compress line scan image into blocks of min./max pairs
     Done := False ;
     pBufStart := Pointer( Cardinal(PLineScanBuf) + (FrameNum*NumBytesPerFrame) ) ;
     While Not Done do begin

         // Compile min./max. values within block
         j := BlockNum*FrameWidth ;
         if (Line >= 0) and (Line < NumLinesInFile) then begin
            i := Min(Max(0,Line),NumLinesInFile-1)*FrameWidth ;
            for x := 0 to FrameWidth-1 do begin
               if NumBytesPerPixel = 1 then IPix := PByteArray(pBufStart)^[i+x]
                                       else IPix := PWordArray(pBufStart)^[i+x] ;
               if IPix < IMin[j] then begin
                  IMin[j] := IPix ;
                  IMinAt[j] := Line ;
                  end ;
               if IPix > IMax[j] then begin
                  IMax[j] := IPix ;
                  IMaxAt[j] := Line ;
                  end ;
               Inc(j) ;
               end ;
            end
         else begin
            // Set pixels outside of image to zero
            IMin[j] := 0 ;
            IMinAt[j] := Line ;
            IMax[j] := 0 ;
            IMaxAt[j] := Line ;
            end ;

         // Increment counters

         Inc(BlockCount) ;
         Inc(Line) ;
         if BlockCount >= LSDisplayBlockSize then begin
            Inc(BlockNum) ;
            BlockCount := 0 ;
            end ;

         // Terminate when all blocks done
         if BlockNum >= LSDisplayMaxBlocks then Done := True ;

         end ;
     NumDisplayBlocks := BlockNum ;
     NumDisplayPoints := BlockNum*LSDisplayNumPointsPerBlock ;

     LSDisplayNumLinesPerPoint := LSDisplayBlockSize / LSDisplayNumPointsPerBlock ;
     NumPixelsInImageBuf := LSDisplayMaxBlocks*LSDisplayNumPointsPerBlock*FrameWidth ;

     // Clear buffer
     for i := 0 to NumPixelsInImageBuf-1 do PImageBuf[i] := 0 ;

     // Copy min./max. blocks into displayed image data buffer
     // (Note 90 degree rotation of image data)
     for BlockNum := 0 to NumDisplayBlocks-1 do begin
         for x := 0 to FrameWidth-1 do begin
            iFrom := BlockNum*FrameWidth + x ;
            iTo := LSDisplayNumPointsPerBlock*(BlockNum + x*LSDisplayMaxBlocks) ;
            if IMinAt[iFrom] <= IMaxAt[iFrom] then begin
               PImageBuf[iTo] := IMin[iFrom] ;
               PImageBuf[iTo+LSDisplayNumPointsPerBlock-1] := IMax[iFrom] ;
               end
            else begin
               PImageBuf[iTo] := IMax[iFrom] ;
               PImageBuf[iTo+LSDisplayNumPointsPerBlock-1] := IMin[iFrom] ;
               end ;
            end ;
         end ;

     end ;



procedure TViewLineFrm.DisplayPixelIntensityTimeCourse ;
// --------------------------------------------------------------
// Display time course of intensity at selected pixel within line
// --------------------------------------------------------------
var
    i,j,x,y,y0 : Integer ;
    BlockNum : Integer ;
    NumPointsPerRow : Integer ;
    NumPixelsAvg : Integer ;
    HalfNumPixelsAvg : Integer ;
    NumLinesPerPoint : Integer ;
    Row : Integer ;
    RowStart : Integer ;
    RowEnd : Integer ;
    BackgRowStart : Integer ;
    BackgRowEnd : Integer ;
    Sum : Single ;
    BackgroundIntensity : Single ;
    NumFLScans: Integer;
    MaxFLDisplayBlocks: Integer;
    FLDisplayBlockSize: Integer;
    FLDisplayPointsPerBlock: Integer;
    MaxFLDisplayPoints: Integer;
begin

     if not ckDisplayFluorescence.Checked then Exit ;

    MainFrm.IDRFile.LSTimeCoursePixel := Round(edTimeCoursePixel.Value) ;
    MainFrm.IDRFile.LSTimeCourseNumAvg := Round(edNumPixelsAvg.Value)  ;
    MainFrm.IDRFile.LSTimeCourseBackgroundPixel := Round(edBackgroundPixel.Value);
    MainFrm.IDRFile.LSSubtractBackground := ckSubtractBackground.Checked ;

     // Make time course and A/D display sizes same as image bitmap
     scFLDisplay.MaxPoints := LSDisplayMaxBlocks ;
     scFLDisplay.XMax := scFLDisplay.MaxPoints - 1 ;
     scADCDisplay.MaxPoints := LSDisplayMaxBlocks ;
     scADCDisplay.XMax := scADCDisplay.MaxPoints - 1 ;

     // Pixel intensity time course display
     if DisplayBuf <> Nil then FreeMem(DisplayBuf) ;
     GetMem( DisplayBuf, LSDisplayMaxBlocks*2*4 ) ;
     scFLDisplay.SetDataBuf( DisplayBuf ) ;

     NumPixelsAvg := Round(edNumPixelsAvg.Value) ;
     HalfNumPixelsAvg := NumPixelsAvg div 2 ;

     // Centre line pixel to be plotted
     TimeCoursePixel := FrameWidth - Round(edTimeCoursePixel.Value) ;
     TimeCoursePixel := Max( TimeCoursePixel,HalfNumPixelsAvg ) ;
     TimeCoursePixel := Min( TimeCoursePixel,
                             FrameWidth - (HalfNumPixelsAvg -(NumPixelsAvg mod 2) ) ) ;

     // Centre line pixel to be plotted
     BackgroundPixel := FrameWidth - Round(edBackgroundPixel.Value) ;
     BackgroundPixel := Max( BackgroundPixel,HalfNumPixelsAvg ) ;
     BackgroundPixel := Min( BackgroundPixel,
                             FrameWidth - (HalfNumPixelsAvg -(NumPixelsAvg mod 2) ) ) ;

     NumFLScans := Round(edTDisplay.Value/edLineScanInterval.Value);
     MaxFLDisplayBlocks := LSDisplayMaxBlocks;
     FLDisplayBlockSize := NumFLScans div MaxFLDisplayBlocks;
     if FLDisplayBlockSize <= 1 then
       FLDisplayBlockSize := 1;
     MaxFLDisplayBlocks := NumFLScans div FLDisplayBlockSize;
     FLDisplayPointsPerBlock := Min(FLDisplayBlockSize, 2);
     MaxFLDisplayPoints := MaxFLDisplayBlocks * FLDisplayPointsPerBlock;
     {scADCDisplay.MaxPoints := MaxADCDisplayPoints ;
     scADCDisplay.xMin := 0 ;
     scADCDisplay.xMax := MaxADCDisplayPoints-1 ;}
     // NumPointsPerRow := LSDisplayMaxBlocks*Min(LSDisplayBlockSize,2) ;
     NumPointsPerRow := MaxFLDisplayPoints;

     RowStart := TimeCoursePixel  - HalfNumPixelsAvg ;
     RowEnd := RowStart + NumPixelsAvg - 1 ;
     BackgRowStart := BackgroundPixel  - HalfNumPixelsAvg ;
     BackgRowEnd := BackgRowStart + NumPixelsAvg - 1 ;

     for x := 0 to NumDisplayPoints-1 do begin

        // Calculate average background band intensity
        if ckSubtractBackground.Checked then begin
           Sum := 0.0 ;
           for Row := BackgRowStart to BackgRowEnd do begin
               i := Row*NumPointsPerRow + x ;
               Sum := Sum + PImageBuf[i] ;
               end ;
           BackGroundIntensity := Sum / NumPixelsAvg ;
           end
        else  BackGroundIntensity := 0.0 ;

        // Calculate average band intensity
        Sum := 0.0 ;
        for Row := RowStart to RowEnd do begin
            i := Row*NumPointsPerRow + x ;
            Sum := Sum + PImageBuf[i] ;
            end ;
        DisplayBuf[x] := Round( Sum/NumPixelsAvg - BackGroundIntensity) ;

        end ;

     scFLDisplay.MaxPoints := NumPointsPerRow ;
     scFLDisplay.NumPoints := NumDisplayPoints ;
     scFLDisplay.XMin := 0 ;
     scFLDisplay.XMax := scFLDisplay.MaxPoints-1 ;
     scFLDisplay.ChanName[0] := cbFrameNum.Text ;

     NumLinesPerPoint := LSDisplayBlockSize div Min(LSDisplayBlockSize,2) ;

     // Set display time units
     SetDisplayUnits ;

     scFLDisplay.XOffset := sbDisplay.Position div NumLinesPerPoint ;
     scFLDisplay.Invalidate ;

     // Update cursors on line scan image
     UpdateImageCursors( scFLDisplay.VerticalCursors[TCCursor] ) ;

     end ;


procedure TViewLineFrm.DisplayRatioTimeCourse ;
// --------------------------------------------------------------
// Display time course of intensity ratio at selected pixel within line
// --------------------------------------------------------------
var
    i,j,x,y,y0 : Integer ;
    BlockNum : Integer ;
    NumPointsPerRow : Integer ;
    NumPixelsAvg : Integer ;
    HalfNumPixelsAvg : Integer ;
    NumLinesPerPoint : Integer ;
    Row : Integer ;
    RowStart : Integer ;
    RowEnd : Integer ;
    BackgRowStart : Integer ;
    BackgRowEnd : Integer ;
    SumN,SumD : Single ;
    YN, YD, YScale, YThreshold : Single ;
    BackgroundIntensityN,BackgroundIntensityD : Single ;
    NumRScans: Integer;
    MaxRDisplayBlocks: Integer;
    RDisplayBlockSize: Integer;
    RDisplayPointsPerBlock: Integer;
    MaxRDisplayPoints: Integer;
begin

     if not ckDisplayR.Checked then Exit ;

     // Load images
     LoadLineScanImage( cbNumerator.ItemIndex, pImageN ) ;
     LoadLineScanImage( cbDenominator.ItemIndex, pImageD ) ;

     // Make time course and A/D display sizes same as image bitmap
     scRDisplay.MaxPoints := LSDisplayMaxBlocks ;
     scRDisplay.XMax := scRDisplay.MaxPoints - 1 ;
     scADCDisplay.MaxPoints := LSDisplayMaxBlocks ;
     scADCDisplay.XMax := scADCDisplay.MaxPoints - 1 ;


     // Pixel intensity time course display
     if RDisplayBuf <> Nil then FreeMem(RDisplayBuf) ;
     GetMem( RDisplayBuf, LSDisplayMaxBlocks*2*4 ) ;
     scRDisplay.SetDataBuf( RDisplayBuf ) ;

     NumPixelsAvg := Round(edNumPixelsAvg.Value) ;
     HalfNumPixelsAvg := NumPixelsAvg div 2 ;

     // Centre line pixel to be plotted
     TimeCoursePixel := FrameWidth - Round(edTimeCoursePixel.Value) ;
     TimeCoursePixel := Max( TimeCoursePixel,HalfNumPixelsAvg ) ;
     TimeCoursePixel := Min( TimeCoursePixel,
                             FrameWidth - (HalfNumPixelsAvg -(NumPixelsAvg mod 2) ) ) ;

     // Centre line pixel to be plotted
     BackgroundPixel := FrameWidth - Round(edBackgroundPixel.Value) ;
     BackgroundPixel := Max( BackgroundPixel,HalfNumPixelsAvg ) ;
     BackgroundPixel := Min( BackgroundPixel,
                             FrameWidth - (HalfNumPixelsAvg -(NumPixelsAvg mod 2) ) ) ;

     NumRScans := Round(edTDisplay.Value/edLineScanInterval.Value);
     MaxRDisplayBlocks := LSDisplayMaxBlocks;
     RDisplayBlockSize := NumRScans div MaxRDisplayBlocks;
     if RDisplayBlockSize <= 1 then
       RDisplayBlockSize := 1;
     MaxRDisplayBlocks := NumRScans div RDisplayBlockSize;
     RDisplayPointsPerBlock := Min(RDisplayBlockSize, 2);
     MaxRDisplayPoints := MaxRDisplayBlocks * RDisplayPointsPerBlock;
     // NumPointsPerRow := LSDisplayMaxBlocks*Min(LSDisplayBlockSize,2) ;
     NumPointsPerRow := MaxRDisplayPoints;

     RowStart := TimeCoursePixel  - HalfNumPixelsAvg ;
     RowEnd := RowStart + NumPixelsAvg - 1 ;
     BackgRowStart := BackgroundPixel  - HalfNumPixelsAvg ;
     BackgRowEnd := BackgRowStart + NumPixelsAvg - 1 ;

     YScale := scRDisplay.MaxADCValue / edRDisplayMax.Value ;
     scRDisplay.ChanScale[0] := 1.0 / YScale ;
     scRDisplay.ChanName[0] := LeftStr(MainFrm.IDRFile.FrameType[cbNumerator.ItemIndex],3)
                               + '/' +
                               LeftStr(MainFrm.IDRFile.FrameType[cbDenominator.ItemIndex],3) ;
     YThreshold := edRatioThreshold.Value ;

     for x := 0 to NumDisplayPoints-1 do begin

         // Calculate average background band intensity
         if ckSubtractBackground.Checked then begin
            SumN := 0.0 ;
            SumD := 0.0 ;
            for Row := BackgRowStart to BackgRowEnd do begin
                i := Row*NumPointsPerRow + x ;
                SumN := SumN + PImageN^[i] ;
                SumD := SumD + PImageD^[i] ;
               end ;
            BackGroundIntensityN := SumN / NumPixelsAvg ;
            BackGroundIntensityD := SumD / NumPixelsAvg ;
            end
         else begin
            BackGroundIntensityN := 0.0 ;
            BackGroundIntensityD := 0.0 ;
            end ;

        // Calculate average band intensity
        SumN := 0.0 ;
        SumD := 0.0 ;
        for Row := RowStart to RowEnd do begin
            i := Row*NumPointsPerRow + x ;
            SumN := SumN + PImageN^[i] ;
            SumD := SumD + PImageD^[i] ;
            end ;
        YN := Round( SumN/NumPixelsAvg - BackGroundIntensityN) ;
        YD := Round( SumD/NumPixelsAvg - BackGroundIntensityD) ;
        if YD > YThreshold then RDisplayBuf[x] := Round((YN/YD)*YScale)
                           else  RDisplayBuf[x] := 0 ;

        end ;

     scRDisplay.MaxPoints := NumPointsPerRow ;
     scRDisplay.NumPoints := NumDisplayPoints ;
     scRDisplay.XMin := 0 ;
     scRDisplay.XMax := scRDisplay.MaxPoints-1 ;

     NumLinesPerPoint := LSDisplayBlockSize div Min(LSDisplayBlockSize,2) ;

     // Set display time units
     SetDisplayUnits ;

     scRDisplay.XOffset := sbDisplay.Position div NumLinesPerPoint ;
     scRDisplay.Invalidate ;

     // Update cursors on line scan image
     UpdateImageCursors( scRDisplay.VerticalCursors[RCursor] ) ; ;

     end ;


procedure TViewLineFrm.UpdateImageCursors(
          CursorPos : Integer ) ;
// ---------------------------------
// Update cursors on line scan image
// ---------------------------------
var
    OldPen : TPen ;
    NumPixelsAvg,YTop,YBottom : Integer ;
begin

     OldPen := TPen.Create ;
     OldPen.Assign(Image.Canvas.Pen) ;

     // Update line scan image
     Image.Picture.Assign(BitMap) ;

     NumPixelsAvg := Round(edNumPixelsAvg.Value) ;
     YBottom := Round(FrameWidth*DisplayZoom) - NumPixelsAvg div 2 ;
     YTop := NumPixelsAvg div 2 ;

     // Draw pixel time course cursor
     TimeCoursePixel := Round((FrameWidth-edTimeCoursePixel.Value)*DisplayZoom) ;
     TimeCoursePixel := Max(Min(TimeCoursePixel,YBottom),YTop) ;

     Image.Canvas.Pen.Width := NumPixelsAvg ;
     Image.Canvas.Pen.Color := clWhite ;
     Image.Canvas.Polyline( [Point(0,TimeCoursePixel),
                             Point(Image.Width-1,TimeCoursePixel)]) ;
     Image.Canvas.TextOut( 0, TimeCoursePixel, 'TC' ) ;

     // Draw background time course cursor
     BackgroundPixel := Round((FrameWidth-edBackgroundPixel.Value)*DisplayZoom) ;
     BackgroundPixel := Max(Min( BackgroundPixel,YBottom),YTop) ;

     Image.Canvas.Pen.Width := NumPixelsAvg ;
     Image.Canvas.Pen.Color := clWhite ;
     Image.Canvas.Polyline( [Point(0,BackgroundPixel),
                             Point(Image.Width-1,BackgroundPixel)]) ;
     Image.Canvas.TextOut( 0, BackgroundPixel, 'Bg' ) ;

     // Draw time course readout cursor on image
     TCCursorPos := CursorPos ;
     Image.Canvas.Pen.Width := 1 ;
     Image.Canvas.Pen.Color := clWhite ;
     Image.Canvas.Polyline( [Point(TCCursorPos,0),
                             Point(TCCursorPos,Image.Height-1)]) ;

     // Restore pen settings
     Image.Canvas.Pen.Assign(OldPen) ;
     OldPen.Free ;

     end ;


procedure TViewLineFrm.DisplayADCChannels ;
// ------------------------------------------
// Display analogue signals stored on file
// ------------------------------------------
const
     MaxADCDisplayScans = 2000 ;
     NumADCScansPerBuf = 1024 ;
var

    NumADCSamplesPerBuf : Integer ;
    NumADCScansRead : Integer ;
    BufStartScan : Integer ;
    NumSamplesRead : Integer ;
    StartScan : Integer ;
    CursorScan : Integer ;
    BlockCount : Integer ;
    NumPoints : Integer ;
    FilePointer : Integer ;
    iDisp : Integer ;
    Done : Boolean ;
    yMin : Array[0..ADCChannelLimit] of Integer ;
    yMax : Array[0..ADCChannelLimit] of Integer ;
    yMinAt : Array[0..ADCChannelLimit] of Integer ;
    yMaxAt : Array[0..ADCChannelLimit] of Integer ;
    Buf : Array[0..(NumADCScansPerBuf*(ADCChannelLimit+1)-1)] of SmallInt ;
    i,ch,y : Integer ;
    MarkerTime : Single ;
    MarkerAt : Integer ;
    TimeScale : Single ;
    TCCursorTime : Single ;
    NumADCScans : Integer ;
    NumADCScansPerBlock : Integer ;
    NumADCSamplesPerBlock : Integer ;
    NumADCPointsPerBlock : Integer ;
    MaxADCDisplayBlocks : Integer ;
    TMarkerScale : Single ;
begin

     if not ckDisplayADC.Checked then Exit ;
     if MainFrm.IDRFile.ADCNumChannels <= 0 then Exit ;

     NumADCScans := Round(edTDisplay.Value/MainFrm.IDRFile.ADCScanInterval) ;
     MaxADCDisplayBlocks := LSDisplayMaxBlocks ;
     ADCDisplayBlockSize := NumADCScans div MaxADCDisplayBlocks ;
     if ADCDisplayBlockSize <= 1 then ADCDisplayBlockSize := 1 ;
     MaxADCDisplayBlocks := NumADCScans div ADCDisplayBlockSize ;
     ADCDisplayPointsPerBlock := Min(ADCDisplayBlockSize,2) ;
     ADCDisplayScansPerPoint := ADCDisplayBlockSize / ADCDisplayPointsPerBlock ;
     MaxADCDisplayPoints := MaxADCDisplayBlocks*ADCDisplayPointsPerBlock ;
     scADCDisplay.MaxPoints := MaxADCDisplayPoints ;
     scADCDisplay.xMin := 0 ;
     scADCDisplay.xMax := MaxADCDisplayPoints-1 ;

     // No. of samples in file I/O buffer
     NumADCSamplesPerBuf := MainFrm.IDRFile.ADCNumChannels*NumADCScansPerBuf ;

     // Find starting scan number
     StartScan := Round ( (sbDisplay.Position*edLineScanInterval.Value)/
                           MainFrm.IDRFile.ADCScanInterval ) ;

     TCCursorTime := ((LSDisplayBlockSize/LSDisplayNumPointsPerBlock)
                     *edLineScanInterval.Value*TCCursorPos)
                     + sbDisplay.Position*edLineScanInterval.Value ;

     CursorScan := Round( TCCursorTime/MainFrm.IDRFile.ADCScanInterval ) ;


     scADCDisplay.XOffset := Round( StartScan/ADCDisplayScansPerPoint ) ;

     // Place readout cursor at frame capture time on A/D display
     scADCDisplay.VerticalCursors[ADCReadoutCursor] :=
          Round(CursorScan/ADCDisplayScansPerPoint) - scADCDisplay.XOffset;

     // Set display time units
     SetDisplayUnits ;

     // Initialise counters
     BlockCount := ADCDisplayBlockSize ;
     NumSamplesRead := NumADCSamplesPerBuf ;
     i := NumSamplesRead ;
     BufStartScan := StartScan ;
     iDisp := 0 ;
     NumPoints := 0 ;
     Done := False ;

     // Read samples from file
     While not Done do begin

        // Initialise block
        if BlockCount >= ADCDisplayBlockSize then begin
           for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
               yMin[ch] := MainFrm.IDRFile.ADCMaxValue ;
               yMax[ch] := -yMin[ch] -1 ;
               end ;
           BlockCount := 0 ;
           end ;

        // Load new buffer
        if i >= NumSamplesRead then begin
           NumADCScansRead := MainFrm.IDRFile.LoadADC( BufStartScan,
                                                       NumADCScansPerBuf,
                                                       Buf ) ;

           NumSamplesRead := NumADCScansRead*MainFrm.IDRFile.ADCNumChannels ;
           BufStartScan := BufStartScan + NumADCScansPerBuf ;
           i := 0 ;
           if NumSamplesRead <= 0 then Break ;
           end ;

        // Determine min. / max. value & order of samples within compression block
        for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin

            // Get A/D sample
            y := Buf[i] ;

            if y < yMin[ch] then begin
               yMin[ch] := y ;
               yMinAt[ch] := BlockCount ;
               end ;
            if y > yMax[ch] then begin
               yMax[ch] := y ;
               yMaxAt[ch] := BlockCount ;
               end ;
            Inc(i) ;
            end ;
        Inc(BlockCount) ;

        // When block complete ... write min./max. to display buffer
        if BlockCount >= ADCDisplayBlockSize then begin

           // First point
           for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
               if yMaxAt[ch] <= yMinAt[ch] then ADCBuf[iDisp] := yMax[ch]
                                           else ADCBuf[iDisp] := yMin[ch] ;
               Inc(iDisp) ;
               end ;
           Inc(NumPoints) ;

           // Second point
           if BlockCount > 1 then begin
              for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
                  if yMaxAt[ch] >= yMinAt[ch] then ADCBuf[iDisp] := yMax[ch]
                                              else ADCBuf[iDisp] := yMin[ch] ;
                  Inc(iDisp) ;
                  end ;
              Inc(NumPoints) ;
              end ;

           end ;

        if (NumPoints >= scADCDisplay.MaxPoints) {or
           ( NumSamplesRead <= 0)} then Done := True ;

        end ;

     scADCDisplay.NumPoints := NumPoints ;

     // Enable/disable display calibration grid
     //scADCDisplay.DisplayGrid := Main.mnDisplayGrid.Checked ;
     //scADCDisplay.SetDataBuf( ADCBuf ) ;

     // Add markers (if any appear on display
     scadcDisplay.ClearMarkers ;
     if rbTDisplayUnitMins.Checked then TMarkerScale := 60.0/scADCDisplay.TScale
                                   else TMarkerScale := 1.0/scADCDisplay.TScale ;
     for i := 0 to MainFrm.IDRFile.NumMarkers-1 do begin
         MarkerAt := Round(MainFrm.IDRFile.MarkerTime[i]*TMarkerScale)
                     - scADCDisplay.XOffset ;
         if (MarkerAt >= 0) and (MarkerAt < scadcDisplay.MaxPoints) then
            scadcDisplay.AddMarker( MarkerAt, MainFrm.IDRFile.MarkerText[i] );
         end ;


     SetDisplayUnits ;
     scADCDisplay.Invalidate ;

     end ;



procedure TViewLineFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
var
    i : Integer ;
begin

     // Close forms which depend upon this one
      for i := 0 to MainFrm.MDIChildCount-1 do begin
        if MainFrm.MDIChildren[i].Name = 'TimeCourseFrm' then
           TTimeCourseFrm(MainFrm.MDIChildren[i]).Close ;
        if MainFrm.MDIChildren[i].Name = 'EventAnalysisFrm' then
           TEventAnalysisFrm(MainFrm.MDIChildren[i]).Close ;
        if MainFrm.MDIChildren[i].Name = 'SmoothDifferentiateFrm' then
           TSmoothDifferentiateFrm(MainFrm.MDIChildren[i]).Close ;
        end ;

     if ADCBuf <> Nil then FreeMem(ADCBuf) ;
     if PLineScanBuf <> Nil then FreeMem(PLineScanBuf) ;
     if PImageBuf <> Nil then FreeMem(PImageBuf) ;
     if PImageN <> Nil then FreeMem(PImageN) ;
     if PImageD <> Nil then FreeMem(PImageD) ;
     if BitMap <> Nil then BitMap.Free ;
     if DisplayBuf <> Nil then FreeMem(DisplayBuf) ;
     if RDisplayBuf <> Nil then FreeMem(RDisplayBuf) ;
     if IMin <> Nil then FreeMem( IMin ) ;
     if IMinAt <> Nil then FreeMem( IMinAt ) ;
     if IMax <> Nil then FreeMem( IMax ) ;
     if IMaxAt <> Nil then FreeMem( IMaxAt ) ;

     Action := caFree ;

     end;


procedure TViewLineFrm.SetDisplayUnits ;
// ----------------------
// Set display time units
// ----------------------
begin

    if rbTDisplayUnitsSecs.Checked then begin
       edTDisplay.Units := 's' ;
       edTDisplay.Scale := 1.0 ;

       end
    else begin
       edTDisplay.Units := 'm' ;
       edTDisplay.Scale := 1.0/60.0 ;
       end ;

    // Line scan display
    scFLDisplay.TScale := edLineScanInterval.Value*LSDisplayNumLinesPerPoint*edTDisplay.Scale ;
    scFLDisplay.TUnits := edTDisplay.Units ;
    scRDisplay.TScale :=  scFLDisplay.TScale ;
    scRDisplay.TUnits :=  scFLDisplay.TUnits ;
    scFLDisplay.Invalidate ;
    scRDisplay.Invalidate ;

    // A/D channel display
    if MainFrm.IDRFile.ADCNumChannels > 0 then begin
       scADCDisplay.TScale := ADCDisplayScansPerPoint*
                              MainFrm.IDRFile.ADCSCanInterval*
                              edTDisplay.Scale  ;
       scADCDisplay.TUnits := edTDisplay.Units ;
       scADCDisplay.Invalidate ;
       end ;

    end ;



procedure TViewLineFrm.rbTDisplayUnitsSecsClick(Sender: TObject);
// ------------------------------
// Set Display time units to secs
// ------------------------------
begin
     SetDisplayUnits ;
     end;

procedure TViewLineFrm.edTDisplayKeyPress(Sender: TObject; var Key: Char);
// -------------------------------
// Display window duration changed
// -------------------------------
begin
     if Key = #13 then begin
        Resize ;
        DisplayUpdateRequired := True ;
        end ;
     end;

procedure TViewLineFrm.sbDisplayChange(Sender: TObject);
// ------------------------------------------
// Starting line of line scan display changed
// ------------------------------------------
begin
     DisplayUpdateRequired := True ;
     end;


procedure TViewLineFrm.edTimeCoursePixelKeyPress(Sender: TObject;
  var Key: Char);
// -------------------------------------------
// Pixel selected for time course plot changed
// -------------------------------------------
begin
     if Key = #13 then DisplayUpdateRequired := True ;
     end;

procedure TViewLineFrm.edNumPixelsAvgKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------------------------
// No. of pixels averaged for time course plot changed
// ---------------------------------------------------
begin
     if Key = #13 then DisplayUpdateRequired := True ;
     end;


procedure TViewLineFrm.ImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
// --------------------------------
// Mouse moved over line scan image
// --------------------------------

var
   YPixelCursor : Integer ;
   YBackgCursor : Integer ;
   HalfNumPixelsAvg : Integer ;
   NumPixelsAvg : Integer ;
   CursorScan : Single ;
begin

     YPixelCursor := Round((FrameWidth-edTimeCoursePixel.Value)*DisplayZoom) ;
     YBackgCursor := Round((FrameWidth-edBackgroundPixel.Value)*DisplayZoom) ;

     if (not MouseDown) then begin
        // Change mouse cursor image when over hor/vert cursor
        if Abs(y - YPixelCursor) <= 2 then begin
           Image.Cursor := crSizeNS ;
           ImageCursorInUse := imcPixelTimeCourse ;
           end
        else if Abs(y - YBackgCursor) <= 2 then begin
           Image.Cursor := crSizeNS ;
           ImageCursorInUse := imcBackgroundTimeCourse ;
           end
        else if Abs(x - TCCursorPos) <= 2 then begin
           Image.Cursor := crSizeWE ;
           ImageCursorInUse := imcTimeCursor ;
           end
        else begin
           Image.Cursor := crDefault ;
           ImageCursorInUse := imcNone ;
           end ;
        end
     else if ImageCursorInUse = imcPixelTimeCourse then begin
        // Pixel time course cursor selected
        if YPixelCursor <> y then begin
           NumPixelsAvg := Round(edNumPixelsAvg.Value) ;
           HalfNumPixelsAvg := NumPixelsAvg div 2 ;
           YPixelCursor := Min( Max(y,HalfNumPixelsAvg),
                                Image.Height-HalfNumPixelsAvg -(NumPixelsAvg mod 2)) ;
           edTimeCoursePixel.Value := FrameWidth - (YPixelCursor/DisplayZoom) ;
           DisplayUpdateRequired := True ;
           end ;
        end
     else if ImageCursorInUse = imcBackgroundTimeCourse then begin
        // Background time course cursor selected
        if YBackgCursor <> y then begin
           NumPixelsAvg := Round(edNumPixelsAvg.Value) ;
           HalfNumPixelsAvg := NumPixelsAvg div 2 ;
           YBackgCursor := Min( Max(y,HalfNumPixelsAvg),
                                Image.Height-HalfNumPixelsAvg -(NumPixelsAvg mod 2)) ;
           edBackgroundPixel.Value := FrameWidth - (YBackgCursor/DisplayZoom) ;
           DisplayUpdateRequired := True ;
           end ;
        end
     else if ImageCursorInUse = imcTimeCursor then begin
        // Vertical cursor selected
        if TCCursorPos <> x then begin
           TCCursorPos := Min(Max(x,0),Image.Width-1) ;
           scFLDisplay.VerticalCursors[TCCursor] := TCCursorPos ;
           scRDisplay.VerticalCursors[RCursor] := TCCursorPos ;
           UpdateADCDisplayCursor ;
           UpdateImageCursors(scFLDisplay.VerticalCursors[TCCursor]) ;
           end ;
        end

     end;


procedure TViewLineFrm.UpdateADCDisplayCursor ;
// ----------------------------------
// Update A/D channels display cursor
// ----------------------------------
var
    CursorScan : Single ;
begin

     if MainFrm.IDRFile.ADCNumChannels <= 0 then Exit ;
     if MainFrm.IDRFile.ADCScanInterval <= 0.0 then Exit ;
     if ADCDisplayScansPerPoint <= 0 then Exit ;

     CursorScan := ((LSDisplayNumLinesPerPoint*edLineScanInterval.Value*TCCursorPos)
                    + sbDisplay.Position*edLineScanInterval.Value )
                    /MainFrm.IDRFile.ADCScanInterval ;
     scADCDisplay.VerticalCursors[ADCReadoutCursor] :=
          Round(CursorScan/ADCDisplayScansPerPoint) - scADCDisplay.XOffset;

     end ;


procedure TViewLineFrm.UpdateLSDisplayCursor ;
// -------------------------------
// Update line scan display cursor
// -------------------------------
var
    CursorTime : Single ;
    CursorLine : Single ;
    ADCChannel : TChannel ;
    ch : Integer ;
begin


//    CursorLine := ADCDisplayScansPerPoint*
//                  (scADCDisplay.VerticalCursors[ADCReadoutCursor] + scADCDisplay.XOffset)*
//                  (MainFrm.IDRFile.ADCScanInterval/edLineScanInterval.Value) ;

    if edLineScanInterval.Value <= 0.0 then Exit ;
    CursorTime := ADCDisplayScansPerPoint*(scADCDisplay.VerticalCursors[ADCReadoutCursor] + scADCDisplay.XOffset)
                  * MainFrm.IDRFile.ADCScanInterval ;
    CursorLine := (CursorTime ) / edLineScanInterval.Value ;

    if LSDisplayNumLinesPerPoint <= 0 then Exit ;
    TCCursorPos := Round( (CursorLine - sbDisplay.Position)/LSDisplayNumLinesPerPoint) ;

    ADCDisplayCursorUpdated := True ;

    scFLDisplay.VerticalCursors[TCCursor] := TCCursorPos ;
    scRDisplay.VerticalCursors[RCursor] := TCCursorPos ;

    UpdateImageCursors(scFLDisplay.VerticalCursors[TCCursor]) ;

    // Update main display Y limits
    for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
        ADCChannel := MainFrm.IDRFile.ADCChannel[ch] ;
        ADCChannel.YMax :=  scADCDisplay.YMax[ch] ;
        ADCChannel.YMin :=  scADCDisplay.YMin[ch] ;
        MainFrm.IDRFile.ADCChannel[ch] := ADCChannel ;
        end ;

     end ;



procedure TViewLineFrm.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// ----------------------
// Mouse button depressed
// ----------------------
begin
     MouseDown := True ;
     end;


procedure TViewLineFrm.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
// ----------------------
// Mouse button released
// ----------------------
begin
     MouseDown := False ;
     end;


procedure TViewLineFrm.scFLDisplayCursorChange(Sender: TObject);
// ----------------------------------------------
// Update cursor labels when readout cursor moved
// ----------------------------------------------
var
    CursorScan : Single ;
begin

     if not Timer.Enabled then Exit ;
     if TScopeDisplay(Sender).CursorChangeInProgress then Exit ;

     TScopeDisplay(Sender).CursorChangeInProgress := True ;

     if ckFixZeroLevels.Checked then begin
        if scFLDisplay.HorizontalCursors[0] <> 0 then scFLDisplay.HorizontalCursors[0] := 0 ;
        end ;

     UpdateImageCursors(scFLDisplay.VerticalCursors[TCCursor]) ;

     if scFLDisplay.VerticalCursors[TCCursor] <> scRDisplay.VerticalCursors[RCursor] then
        scRDisplay.VerticalCursors[RCursor] := scFLDisplay.VerticalCursors[TCCursor] ;

     if not ADCDisplayCursorUpdated then UpdateADCDisplayCursor
                                    else ADCDisplayCursorUpdated := False ;

     LSDisplayCursorUpdated := True ;

     // Give this control focus to avoid left/right cursor control arrow keys
     // from changing other controls
     edStartLine.SetFocus ;

     TScopeDisplay(Sender).CursorChangeInProgress := False ;

     end ;


procedure TViewLineFrm.bFullScaleClick(Sender: TObject);
// ------------------------------------------------------
// Set display look-up table to full range of grey levels
// ------------------------------------------------------
begin

     edDisplayIntensityRange.LoValue := 0 ;
     MainFrm.GreyLo[SelectedFrameType] := Round(edDisplayIntensityRange.LoValue) ;
     edDisplayIntensityRange.HiValue := MainFrm.IDRFile.GreyMax ;
     MainFrm.GreyHi[SelectedFrameType] := Round(edDisplayIntensityRange.HiValue) ;

     MainFrm.UpdateLUT( SelectedFrameType, MainFrm.IDRFile.GreyMax ) ;

     // Update image
     DisplayUpdateRequired := True  ;

     end;


procedure TViewLineFrm.bOptimiseContrastClick(Sender: TObject);
// ------------------------------------------------------
// Set display look-up table to range of levels in image
// ------------------------------------------------------
var
     yMin,yMax : Integer ;
     i : Integer ;
begin

     // Find range of pixel intensity values within displayed line scan image
     yMin := High(yMin) ;
     yMax := Low(yMin) ;
     for i := 0 to NumPixelsInImageBuf-1 do begin
         if yMin > PImageBuf[i] then yMin := PImageBuf[i] ;
         if yMax < PImageBuf[i] then yMax := PImageBuf[i] ;
         end ;

     // Set upper/lower limits of display intensity range
     MainFrm.GreyLo[0] := yMin ;
     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[0]  ;
     MainFrm.GreyHi[0] := yMax ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[0]  ;

     MainFrm.UpdateLUT( 0, MainFrm.IDRFile.GreyMax ) ;

     DisplayUpdateRequired := True  ;

     end;


procedure TViewLineFrm.cbPaletteChange(Sender: TObject);
// ------------------------------
// Display colour palette changed
// ------------------------------
var
     i : Integer ;
begin
     MainFrm.PaletteType := TPaletteType(cbPalette.Items.Objects[cbPalette.ItemIndex]) ;
     if BitMap <> Nil then begin
        MainFrm.SetPalette( BitMap, MainFrm.PaletteType ) ;
        end ;
     DisplayUpdateRequired := True  ;
     end;


procedure TViewLineFrm.cbDisplayZoomChange(Sender: TObject);
// -----------------------------
// New image display zoom factor
// -----------------------------
var
     Scale, OldDisplayZoom : Single ;
     i : Integer ;
begin

     OldDisplayZoom := DisplayZoom ;
     MainFrm.DisplayZoomIndex := cbDisplayZoom.ItemIndex ;
     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])/100.0 ;
     Scale := DisplayZoom / OldDisplayZoom ;

     Resize ;

     end;


procedure TViewLineFrm.CopyImageToClipboard ;
// -----------------------------------------------------
// Copy line scan image to clipboard as Windows metafile
//------------------------------------------------------
begin

    // Copy bitmap image
    BitMap.SaveToClipboardFormat( ClipboardImageFormat,
                                  ClipboardImageData,
                                  ClipboardPalette ) ;
    Clipboard.SetAsHandle( ClipboardImageFormat,
                             ClipboardImageData ) ;

    end ;


procedure TViewLineFrm.CopyPlotImageToClipboard ;
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



procedure TViewLineFrm.CopyPlotDataToClipboard ;
// -----------------------------------------------------------
// Copy the data in currently selected graph to the clipboard
// -----------------------------------------------------------
begin

     if scFLDisplay.DisplaySelected then scFLDisplay.CopyDataToClipboard
     else if scRDisplay.DisplaySelected then scRDisplay.CopyDataToClipboard
                                        else scADCDisplay.CopyDataToClipboard ;

     end ;


procedure TViewLineFrm.PrintImage ;
{ ------------------
  Print image
  ------------------ }
const
     Margin = 250 ;
var
     SRect : TRect ;
     Scale : Single ;
begin

      // Print image
      Printer.BeginDoc ;
      Printer.Canvas.TextOut(Margin,Margin,MainFrm.IDRFile.FileName) ;

      Scale := (Printer.Canvas.ClipRect.Right
               - Printer.Canvas.ClipRect.Left - 2*Margin) /
               BitMap.Width ;

      SRect.Left := Margin ;
      SRect.Right := Round(BitMap.Width*Scale) + Margin ;
      SRect.Top := Margin + 100 ;
      SRect.Bottom := SRect.Top + Round(BitMap.Height*Scale) ;
      Printer.Canvas.StretchDraw(SRect,BitMap);
      Printer.EndDoc ;

      end ;


procedure TViewLineFrm.PrintPlot ;
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


procedure TViewLineFrm.scFLDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// --------------------
// Mouse button pressed
// --------------------
begin

     // Select ADC display for copy/print (deselect others)
     scFLDisplay.DisplaySelected := True ;
     scFLDisplay.Invalidate ;
     scRDisplay.DisplaySelected := False ;
     scRDisplay.Invalidate ;
     scADCDisplay.DisplaySelected := False ;
     scADCDisplay.Invalidate ;

     end;


procedure TViewLineFrm.scADCDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// --------------------
// Mouse button pressed
// --------------------
begin

     // Select ADC display for copy/print (deselect others)
     scFLDisplay.DisplaySelected := False ;
     scFLDisplay.Invalidate ;
     scRDisplay.DisplaySelected := False ;
     scRDisplay.Invalidate ;
     scADCDisplay.DisplaySelected := True ;
     scADCDisplay.Invalidate ;

     end;


procedure TViewLineFrm.scADCDisplayCursorChange(Sender: TObject);
// ----------------------------------------------
// Update cursor labels when readout cursor moved
// ----------------------------------------------
var
    CursorScan : Single ;
    ch : Integer ;
    Channel : TChannel ;
begin

     if not Timer.Enabled then Exit ;
     if TScopeDisplay(Sender).CursorChangeInProgress then Exit ;

     TScopeDisplay(Sender).CursorChangeInProgress := True ;

     if not LSDisplayCursorUpdated then UpdateLSDisplayCursor
                                   else LSDisplayCursorUpdated := False ;

     ADCDisplayCursorUpdated := True ;

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
     edStartLine.SetFocus ;

     TScopeDisplay(Sender).CursorChangeInProgress := False ;

     end ;


procedure TViewLineFrm.edLineScanIntervalKeyPress(Sender: TObject;
  var Key: Char);
// --------------------------
// Line scan interval changed
// --------------------------
begin
     if Key = #13 then begin
        MainFrm.IDRFile.FrameInterval := edLineScanInterval.Value*NumLinesInFile ;
        DisplayUpdateRequired := True ;
        end ;
     end;


procedure TViewLineFrm.edStartLineKeyPress(Sender: TObject; var Key: Char);
// ------------------------------------------
// First scan line in display changed by user
// ------------------------------------------
begin
     if Key = #13 then begin
        sbDisplay.Position := Round(edStartLine.Value) ;
        DisplayUpdateRequired := True ;
        end;
     end;


function TViewLineFrm.TimeCourseIntensity (
             LineNum : Integer ;
             FrameNum : Integer
             ) : Single ;
// ---------------------------------------------------------
// Return intensity of selected time course pixel at LineNum
// ---------------------------------------------------------
var
    i : Integer ;
    NumPixelsAvg : Integer ;
    HalfNumPixelsAvg : Integer ;
    LineStart : Integer ;
    iStart : Integer ;
    iEnd : Integer ;
    iBackgStart : Integer ;
    iBackgEnd : Integer ;
    Sum : Single ;
    BackgroundIntensity : Single ;
    pBufStart : Pointer ;
begin

     NumPixelsAvg := Round(edNumPixelsAvg.Value) ;
     HalfNumPixelsAvg := NumPixelsAvg div 2 ;

     // Start of line
     LineNum := Min(Max(LineNum,1),FrameHeight) ;
     LineStart := (LineNum-1)*FrameWidth ;

     // Time course pixel band
     TimeCoursePixel := FrameWidth - Round(edTimeCoursePixel.Value) ;
     iStart := Max( (TimeCoursePixel  - HalfNumPixelsAvg) + LineStart, 0) ;
     iEnd := Min( iStart + NumPixelsAvg - 1, NumPixelsPerFrame ) ;

     // Background pixel band
     BackgroundPixel := FrameWidth - Round(edBackgroundPixel.Value) ;
     iBackgStart := Max( (BackgroundPixel  - HalfNumPixelsAvg) + LineStart, 0) ;
     iBackgEnd := Min( iBackgStart + NumPixelsAvg - 1,NumPixelsPerFrame ) ;

     // Start of selected frame in buffer
     pBufStart := Pointer( Cardinal(PLineScanBuf)
                           + (FrameNum-1)*NumBytesPerFrame ) ;

     // Calculate average within subtraction pixel band
     if ckSubtractBackground.Checked then begin
        Sum := 0.0 ;
        if NumBytesPerPixel = 1 then begin
           // 1 byte pixels
           for i := iBackgStart to iBackgEnd do
               Sum := Sum + PByteArray(pBufStart)^[i] ;
           end
        else begin
           // 2 byte pixels
           for i := iBackgStart to iBackgEnd do
               Sum := Sum + PWordArray(pBufStart)^[i] ;
           end ;
        BackgroundIntensity := Sum / Max(NumPixelsAvg,1) ;
        end
     else BackgroundIntensity := 0.0 ;

     // Calculate average within pixel band
     Sum := 0.0 ;
     if NumBytesPerPixel = 1 then begin
        // 1 byte pixels
        for i := iStart to iEnd do Sum := Sum + PByteArray(pBufStart)^[i] ;
        end
     else begin
        // 2 byte pixels
        for i := iStart to iEnd do Sum := Sum + PWordArray(pBufStart)^[i] ;
        end ;

     Result := (Sum / Max(NumPixelsAvg,1)) - BackgroundIntensity ;

     end ;


procedure TViewLineFrm.edIdentKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
// ------------------------------------------
// Update ident field when ident text changed
// ------------------------------------------
begin
     MainFrm.IDRFile.Ident := edIdent.Text ;
     end;


procedure TViewLineFrm.ckSubtractBackgroundClick(Sender: TObject);
// ------------------------------------------------------
// Background pixel time course subtraction toggled on/off
// -------------------------------------------------------
begin
     DisplayUpdateRequired := True ;
     end;


procedure TViewLineFrm.edDisplayIntensityRangeKeyPress(Sender: TObject;
  var Key: Char);
// ----------------------------
// Update display look-up table
// ----------------------------
begin
     if Key = #13 then begin
        MainFrm.GreyLo[SelectedFrameType] := Round(edDisplayIntensityRange.LoValue) ;
        MainFrm.GreyHi[SelectedFrameType] := Round(edDisplayIntensityRange.HiValue) ;
        MainFrm.UpdateLUT( SelectedFrameType, MainFrm.IDRFile.GreyMax ) ;
        DisplayUpdateRequired := True  ;
        end ;
     end;


procedure TViewLineFrm.MagnifyChannelDisplay(
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


procedure TViewLineFrm.ReduceChannelDisplay( ChanNum : Integer ) ;
// ------------------------------------
// Reduce selected A/D channel display
// ------------------------------------
begin
     if ChanNum >= MainFrm.IDRFile.ADCNumChannels then begin
        scFLDisplay.YZoom(ChanNum - MainFrm.IDRFile.ADCNumChannels, 50.0) ;
        end
     else scADCDisplay.YZoom(ChanNum, 50.0);

     end ;


procedure TViewLineFrm.edImageStartDelayKeyPress(Sender: TObject;
  var Key: Char);
// -------------------
// Start delay changed
// -------------------
begin
     if Key = #13 then begin
        MainFrm.IDRFile.ImageStartDelay := edImageStartDelay.Value ;
        DisplayUpdateRequired := True ;
        end ;
     end;


procedure TViewLineFrm.bTDisplayHalfClick(Sender: TObject);
// ----------------------------------------
// Halve the duration of the display window
// ----------------------------------------
begin
     edTDisplay.Value := 0.5*edTDisplay.Value ;
     Resize ;
     DisplayUpdateRequired := True ;
     end;


procedure TViewLineFrm.bTDisplayDoubleClick(Sender: TObject);
// ----------------------------------------
// Double the duration of the display window
// ----------------------------------------
begin
     edTDisplay.Value := 2.0*edTDisplay.Value ;
     Resize ;
     DisplayUpdateRequired := True ;
     end;


procedure TViewLineFrm.TimerTimer(Sender: TObject);
// ------------------------------------
// Timer execution routine (every 55ms)
// ------------------------------------
begin

     if TimerInProgress then Exit ;
     TimerInProgress := True ;

     //if NewFileNeeded then NewFile ;

     // Display images
     if DisplayUpdateRequired then begin
        // Line scan image
        DisplayLineScanImage ;
        // intensity time course line
        DisplayPixelIntensityTimeCourse ;
        // intensity time course line
        DisplayRatioTimeCourse ;
        // A/D channels
        DisplayADCChannels ;
        DisplayUpdateRequired := False ;
        end ;

     TimerInProgress := False ;

     end;

procedure TViewLineFrm.SetDisplayGrid( Value : Boolean ) ;
// ------------------------------------
// Set chart display grid on/off
// ------------------------------------
begin
    scFLDisplay.DisplayGrid := Value ;
    scRDisplay.DisplayGrid := Value ;
    scADCDisplay.DisplayGrid := Value ;
    end ;


function TViewLineFrm.GetDisplayGrid : Boolean ;
// ------------------------------------
// Get chart display grid on/off state
// ------------------------------------
begin
    Result := scFLDisplay.DisplayGrid ;
    end ;





procedure TViewLineFrm.ckFixZeroLevelsClick(Sender: TObject);
// ---------------------------------
// Fix/unfix measurements zero level
// ---------------------------------
begin
     scADCDisplay.Invalidate ;
     scFLDisplay.Invalidate ;
     end;

procedure TViewLineFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
// --------------------
//  Process key presses
// --------------------
var
    ScopeDisp : TScopeDisplay ;
begin

     // Get selected display
     if scFLDisplay.DisplaySelected then ScopeDisp := scFLDisplay
                                    else  ScopeDisp := scADCDisplay ;

     case key of
          VK_LEFT : ScopeDisp.MoveActiveVerticalCursor(-1) ;
          VK_RIGHT : ScopeDisp.MoveActiveVerticalCursor(1) ;
          end ;

     end;


procedure TViewLineFrm.ZoomOutAll ;
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
        end ;
    scFLDisplay.Invalidate ;

    // Set A/D plots
    scADCDisplay.ZoomOut ;

    end ;




procedure TViewLineFrm.ckDisplayFluorescenceClick(Sender: TObject);
// ----------------------------------------------
// Display fluorescence time course plot changed
// ----------------------------------------------
begin
     Resize ;
     DisplayUpdateRequired := True  ;
     end;

procedure TViewLineFrm.ckDisplayADCClick(Sender: TObject);
// ----------------------------------------------
// Display analogue time course plot changed
// ----------------------------------------------
begin
     if MainFrm.IDRFile.ADCNumChannels <= 0 then ckDisplayADC.Checked := False ;
     Resize ;
     DisplayUpdateRequired := True  ;
     end;


procedure TViewLineFrm.ckDisplayRClick(Sender: TObject);
// ----------------------------------------------
// Display ratio time course plot changed
// ----------------------------------------------
begin
     Resize ;
     DisplayUpdateRequired := True  ;
     end;

procedure TViewLineFrm.cbFrameNumChange(Sender: TObject);
// -------------------------------
// Display frame selection changed
// -------------------------------
begin
     DisplayUpdateRequired := True ;
     end;

procedure TViewLineFrm.cbNumeratorChange(Sender: TObject);
// -------------------------------
// Display frame selection changed
// -------------------------------
begin
     DisplayUpdateRequired := True ;
     end;

procedure TViewLineFrm.cbDenominatorChange(Sender: TObject);
// -------------------------------
// Display frame selection changed
// -------------------------------
begin
     DisplayUpdateRequired := True ;
     end;

procedure TViewLineFrm.edRDisplayMaxKeyPress(Sender: TObject;
  var Key: Char);
// -------------------------------
// Ratio Display  max. changed
// -------------------------------
begin
     if Key = #13 then DisplayUpdateRequired := True ;
     end;

procedure TViewLineFrm.edRatioThresholdKeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------------
// Ratio calculation threshold changed
// -----------------------------------
begin
     if Key = #13 then DisplayUpdateRequired := True ;
     end;


procedure TViewLineFrm.scRDisplayCursorChange(Sender: TObject);
// ----------------------------------------------
// Update cursor labels when readout cursor moved
// ----------------------------------------------
var
    CursorScan : Single ;
begin

     if not Timer.Enabled then Exit ;
     if TScopeDisplay(Sender).CursorChangeInProgress then Exit ;

     TScopeDisplay(Sender).CursorChangeInProgress := True ;

     if ckFixZeroLevels.Checked then begin
        if scRDisplay.HorizontalCursors[0] <> 0 then scRDisplay.HorizontalCursors[0] := 0 ;
        end ;

     UpdateImageCursors(scFLDisplay.VerticalCursors[RCursor]) ;

     if scFLDisplay.VerticalCursors[TCCursor] <> scRDisplay.VerticalCursors[RCursor] then
        scFLDisplay.VerticalCursors[TCCursor] := scRDisplay.VerticalCursors[RCursor] ;

     if not ADCDisplayCursorUpdated then UpdateADCDisplayCursor
                                    else ADCDisplayCursorUpdated := False ;

     LSDisplayCursorUpdated := True ;

     // Give this control focus to avoid left/right cursor control arrow keys
     // from changing other controls
     edStartLine.SetFocus ;

     TScopeDisplay(Sender).CursorChangeInProgress := False ;

     end ;


procedure TViewLineFrm.scRDisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// --------------------
// Mouse button pressed
// --------------------
begin

     // Select ADC display for copy/print (deselect others)
     scFLDisplay.DisplaySelected := False ;
     scFLDisplay.Invalidate ;
     scRDisplay.DisplaySelected := True ;
     scRDisplay.Invalidate ;
     scADCDisplay.DisplaySelected := False ;
     scADCDisplay.Invalidate ;

     end;



end.
