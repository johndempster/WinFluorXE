unit ViewUnit;
// =============================================================================
// WinFluor - Windows Fluoresence Program - Image display module
// (c) J. Dempster, University of Strathclyde, 2001-3, All Rights Reserved
// =============================================================================
// 2-8-01 Started
// 7-2-02
// 13.6.02 ... Multi-wavelength images now displayed simultaneously
// 37.3.03 ... Now supports max. of 4 different frame types.
// 18.6.03 ... ROIs can now be saved to file
//             ROI boxes can now be dragged from corners
// 7.7.03 .... Red,green,blue colour palettes added
// 9.7.03 .... Each frame type display now as its own LUT
// 29.7.03 ... Images now stored as 32 bit frames
// 10.8.03 ... Marker text now appears at bottom-right of frame 0 images
// 21.10.03 .. Now uses min/max display point compression for analogue signal display
// 5.11.03 ... Out of Resource error when A/D display windows of zero size fixed
// 16.11.03 .. Zero level cursor added to A/D display
// 5.12.03 ... Images can now be copied to clipboard
// 12.3.03 ... Forwards/Backwards playback button now go grey when playing
// 28.05.04 .. Cursor readouts now displayed within window instead of main status panel
//             Time units can be switched from secs to minutes
// 09.09.04 .. Markers now appear on time course and image
// 22.02.05 .. Max. Contrast now sets chart range 0-max rather than min-max
// 23.02.05 .. Display vertical magnification now retained when form closed
//             Form now opens with Display ROI disabled to avoid delays computing time course
// 23.03.05 .. ROI Load/Save now works correctly
// 24.03.05 .. Out of frame ROIs now deleted when new file created
//             300% & 400% display magnification added
// 13.07.05 .. Time course traces now correct when more than one frame type
// 05.08.05 .. First point in ROI trace now same as second to avoid zero at start
// 11.09.05 .MagnifyADCChannelDisplay and .ReduceADCChannelDisplay added
// 12.09.05 Display duration Double/Half buttons added
// 14.09.05 ComputeROITimeCourse now stopped when form closed
//          (avoids memory allocation error)
// 11.10.05 .MagnifyChannelDisplay and .ReduceChannelDisplay now also work on fluor. channels
// 22.05.06  Fluorescence time course computation required flag now set when
//           new set of ROIs loaded from file
// 16.07.06 25% display zoom factor added
// 11.08.06 Optimised contrast now uses +/-3SD range rather than min/max
//          ROIs now correctly restricted to display image
// 23.01.07 Magnified area within within displayed imagescan now be scrolled within image
//          Number of frame types increased from 4 to 9
// 20.02.07 DrawROI and ScaleROI now public methods (used by AVIUnit)
// 28.02.07 Regions of interest now loaded from file correctly
// 05.03.07 LiveROI now displayed correctly
// 19.03.07 ScrollBar out of range error when 50% magnification selected with small images fixed
// 01.04.07 Internal NumFrameTypes now used
// 02.05.08 100% and higher zooms now shows correct magnification
//          Multi-rate frames now supported
//          Cross-hairs cursors now positioned correctly when X Y scroll bars used
// 10.06.08 Buffer pointers now all set Nil after FreeMem to fix memory allocation
//          error when form is closed
//          LiveROI initialised to crosshair to prevent intermittent
//          list out of bounds error when form opened
// 16.07.08 Delete ROI button replaced with All ROIs option in menu
//          combo box itemindex checked for validity before setting
//          in attempt to fix "list out of bounds" error
// 10.09.09 JD ROIs added by drag and drop. Polyline and polygon ROIs added
// 17.09.09 JD Double images now panelled vertically
// 26.01.10 JD Auto contrast adjustment option added
// 05.02.10 JD Contrast settings stored in INI
// 01.11.10 JD CheckViewFrmExists now correctly returns TRUE
//             ROI time courses now recalculated when ROIs changed
// 02.02.11 JD Thickness of calibration bar can now be set by user
// 06.02.12 JD ROI can now be dropped on to any wavelength image (not just top-left one)
// 17.09.12 JD Contrast Auto adjust setting now saved when window closed
// 26.11.12 JD Zoom combo box moved to top of image area
// 28.11.12 JD Contrast adjustment now only uses sample of 2000 pixels in image
// 02.05.13 JD Upper limit of best contrast now correctly constrained to GreyMax of
//             of image data file, not grey max of camera
// 04.11.13 JD ROIs now saved to tab-text format .ROI file
// 29.05.14 JD FileHandle now THandle rather than Integer
// 13.06.14 JD Error in horizontal display scroll position with display zooms <100% fixed
// 17.06.14 12.5% display zoom added

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, RangeEdit, StdCtrls, ValEdit, ScopeDisplay, ComCtrls, FileIOUnit,
  ValidatedEdit, IDRFile, ClipBrd, Printers, HTMLLabel, math, strutils, UITYpes, TYpes ;

Const
    GreyLevelLimit = $FFFF ;
    MaxDisplayScans = 2000 ;

type
    TSmallIntArray = Array[0..9999999] of SmallInt ;
    PSmallIntArray = ^ TSmallIntArray ;

  TViewFrm = class(TForm)
    ControlsGrp: TGroupBox;
    ControlGrp: TGroupBox;
    sbFrameNum: TScrollBar;
    edFrameNum: TRangeEdit;
    Timer: TTimer;
    edFrameTime: TEdit;
    Label1: TLabel;
    DisplayGrp: TGroupBox;
    ImageGrp: TGroupBox;
    Image1: TImage;
    ROIGrp: TGroupBox;
    Image2: TImage;
    IdentGrp: TGroupBox;
    Label3: TLabel;
    edIdent: TEdit;
    GroupBox2: TGroupBox;
    bGoToStart: TButton;
    bGoToEnd: TButton;
    bBackwards: TButton;
    bForwards: TButton;
    bStop: TButton;
    Image3: TImage;
    Image4: TImage;
    bSaveROIs: TButton;
    bLoadROis: TButton;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    cbPalette: TComboBox;
    MarkGrp: TGroupBox;
    edMarker: TEdit;
    bMark: TButton;
    bEditROIs: TButton;
    ContrastPage: TPageControl;
    RangeTab: TTabSheet;
    bFullScale: TButton;
    bMaxContrast: TButton;
    edDisplayIntensityRange: TRangeEdit;
    ckChangeAllFrameTypes: TCheckBox;
    ckContrast6SDOnly: TCheckBox;
    SlidersTab: TTabSheet;
    Label5: TLabel;
    Label10: TLabel;
    sbContrast: TScrollBar;
    sbBrightness: TScrollBar;
    sbXScroll: TScrollBar;
    sbYScroll: TScrollBar;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    ShadingGrp: TGroupBox;
    ckBackgroundSubtraction: TCheckBox;
    ShadeCorSettingsPanel: TPanel;
    PanROITools: TPanel;
    ROIellipse: TImage;
    ROIPolyline: TImage;
    ROIPolygon: TImage;
    ROILine: TImage;
    ROIPoint: TImage;
    Label2: TLabel;
    ckDisplayCalBar: TCheckBox;
    ROIRectangle: TImage;
    bDeleteROI: TButton;
    cbDeleteROI: TComboBox;
    Label4: TLabel;
    ckAutoOptimise: TCheckBox;
    Label6: TLabel;
    cbDisplayZoom: TComboBox;
    IDRBackground: TIDRFile;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbFrameNumChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure edFrameNumKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bFullScaleClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edDisplayIntensityRangeKeyPress(Sender: TObject;
      var Key: Char);
    procedure edSubtractFrameKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure bDeleteAllROIClick(Sender: TObject);
    procedure bDeleteROIClick(Sender: TObject);
    procedure cbDisplayZoomChange(Sender: TObject);
    procedure bLoadROisClick(Sender: TObject);
    procedure bSaveROIsClick(Sender: TObject);
    procedure cbPaletteChange(Sender: TObject);
    procedure ckDisplayCalBarClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure edIdentKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bMarkClick(Sender: TObject);
    procedure bLUTIncreaseRangeClick(Sender: TObject);
    procedure bLUTDecreaseRangeClick(Sender: TObject);
    procedure bMaxContrastClick(Sender: TObject);
    procedure sbContrastChange(Sender: TObject);
    procedure sbXScrollChange(Sender: TObject);
    procedure sbYScrollChange(Sender: TObject);
    procedure bEditROIsClick(Sender: TObject);
    procedure bStopMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bForwardsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bBackwardsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bGoToStartMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bGoToEndMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ckBackgroundSubtractionClick(Sender: TObject);
    procedure ROIPointMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Image1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Image1DblClick(Sender: TObject);
    procedure ckAutoOptimiseClick(Sender: TObject);
    procedure ControlsGrpClick(Sender: TObject);
    procedure ckContrast6SDOnlyClick(Sender: TObject);
  private
    { Private declarations }

    Images : Array[0..MaxFrameType] of TImage ;    // Image display area list
    ImageSelected : Boolean ;                         // TImage selected for copying

    FrameList : pIntArray ;                        // Frame pointer array

    PBackgroundBufs : Array[0..MaxFrameType] of PIntArray ; // Pointer to background image buffers

    TimerBusy : Boolean ;
    MouseDown : Boolean ;
    FormClosing : Boolean ;

    ImageCursor : TPoint       ; // Image readout cursor position
    bmADCCursor : TBitmap ;

    // Regions of interest
    SelectedROIType : Integer ;
    LiveROINum : Integer ;
    LiveROIChanged : Boolean ;
    ROIAddPolyLineMode : Boolean ;

    TimerCount : Integer ;

    NewFileNeeded : Boolean ;               // Call to NewFile required
    OptimiseContrastNeeded : Boolean ;      // Display contrast optimisation required

    // Clipboard image data

    ClipboardImageFormat : Word ;
    ClipboardImageData: THandle ;
    ClipboardPalette : HPalette ;

    procedure UpdateROIs ;
    procedure AdjustROI( X : Integer ; Y : Integer ) ;

    procedure UpdateStatusBar ;
    procedure UpdateOtherForms ;
    procedure DisplayImageZoom50( var LUT : Array of Word ;
                                  pImageBuf : PIntarray ; BitMap :
                                  TBitMap ; Image :
                                  TImage ) ;
    procedure DisplayImage( var LUT : Array of Word ;
                            var pImageBuf : PIntarray ;
                            var BitMap : TBitMap ;
                            var Image : TImage ) ;
    procedure DisplayCalibrationBar( BitMap : TBitMap ) ;
    function CheckViewPlotFrmExists : Boolean ;

    procedure AddMarker(
              MarkerAtFrame : Integer ;
              MarkerText : String
              ) ;
    procedure CalculateMaxContrast( FrameType : Integer ) ;

    procedure SetDisplayIntensityRange(
              LoValue : Integer ;
              HiValue : Integer
              ) ;
   procedure SetImagePanels ;

   procedure SetFrameNumber( Value : Integer ) ;
   function GetFrameNumber : Integer ;
   procedure UpdateDeleteROIList ;

   procedure AddROI(
             X : Integer ;    // Current mouse X
             Y : Integer      // current mouse Y
             ) ;
  function GetInt( var s : ANSIstring ) : Integer ;

  public
    { Public declarations }
    FrameWidth : Integer ;
    FrameHeight : Integer ;
    CurrentPosition : Integer ;       // Current frame on display
    DisplayZoom : Single ;         // Display zoom factor (0.5,1.0,2.0)
    BitMaps : Array[0..MaxFrameType] of TBitMap ;  // Image bitmaps
    pImageBufs : Array[0..MaxFrameType] of PIntArray ;  // Pointer to image buffers
    FrameTypes : Array[0..MaxFrameType] of string ;      // Frame labels
    NumFrameTypes : Integer ;                 // No. of frame types in use
    FrameCounter : Array[0..MaxFrameType] of Integer ;   // Frames displayed counter
    SelectedFrameType : Integer ;                        // Type selected by user
    FrameIndex : Integer ;        // Index of current frame
    NumBytesPerPixel : Integer ;  // No. of bytes per pixel in pImageBuf
    ImageAvailable : Boolean ;    // True if an image is available
    PlotAvailable : Boolean ;     // True if plot is selected for copying
    NumPixelsPerFrame : Integer ; // No. of pixels in image

    procedure NewFile ;
    procedure DrawROI(
              ROI : TROI ;
              ROINum : Integer ;
              Canvas : TCanvas
              ) ;
    procedure ScaleROI(
              SrcROI : TROI ; // Source ROI (IN)
              var DestROI : TROI   // Destination ROI (OUT)
              ) ;
    procedure UnScaleROI(
              SrcROI : TROI ; // Source ROI (IN)
              var DestROI : TROI   // Destination ROI (OUT)
               ) ;              // Scaling factor (IN)
    procedure LoadROIsFromFile( FileName : String ) ;
    procedure SaveROIsToFile( FileName : String ) ;
    procedure CopyImageToClipboard ;
    procedure PrintImage ;

    Property FrameNumber : Integer read GetFrameNumber write SetFrameNumber ;
  end;

var
  ViewFrm: TViewFrm;

implementation

uses Main, maths, LabIOUnit, TimeCourseUnit, EventAnalysisUnit, PrintRec, LogUnit,
  ViewPlotUnit, RecPlotUnit, EditROIUnit, SpectrumUnit;

{$R *.DFM}
type
    TMoveMode = (mvNone,mvLeftEdge,mvRightEdge,mvTopEdge,mvBottomEdge,
                 mvTopLeft,mvBottomLeft,mvTopRight,mvBottomRight,mvAll) ;

var
    MoveMode : TMoveMode ;


procedure TViewFrm.FormCreate(Sender: TObject);
// ------------------------------------
// Initialisations when form is created
// ------------------------------------
var
     i : Integer ;
begin
     // Raw image data storage buffers
     for i := 0 to High(pImageBufs) do pImageBufs[i] := Nil ;

     // Internal image bitmaps
     for i := 0 to High(BitMaps) do BitMaps[i] := Nil ;

     // Background subtraction buffer
     for i := 0 to High(PBackgroundBufs) do PBackgroundBufs[i] := Nil ;

     FrameList := Nil ;

     bmADCCursor := Nil ;
     Timer.Enabled := False ;
     end;


procedure TViewFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
begin

     Timer.Enabled := False ;

     // Open time course plotting window
     CheckViewPlotFrmExists ;

     // Disable menu item which called this form
     MainFrm.mnViewImages.Enabled := False ;

     // Available region of interest shapes
     ROIPoint.Tag := Integer(PointROI) ;
     ROILine.Tag := Integer(LineROI) ;
     ROIRectangle.Tag := Integer(RectangleROI) ;
     ROIEllipse.Tag := Integer(EllipseROI) ;
     ROIPolyline.Tag := Integer(PolylineROI) ;
     ROIPolygon.Tag := Integer(PolygonROI) ;
     // Default selection
     SelectedROIType := -1 ;
     LiveROINum := -1 ;
     LiveROIChanged := False ;

     // Display magnification factor
     cbDisplayZoom.Clear ;
     cbDisplayZoom.Items.AddObject( '12.5% ', Tobject(125) ) ;
     cbDisplayZoom.Items.AddObject( '  25% ', Tobject(250) ) ;
     cbDisplayZoom.Items.AddObject( '  50% ', Tobject(500) ) ;
     cbDisplayZoom.Items.AddObject( ' 100% ', Tobject(1000)) ;
     cbDisplayZoom.Items.AddObject( ' 200% ', Tobject(2000)) ;
     cbDisplayZoom.Items.AddObject( ' 300% ', Tobject(3000)) ;
     cbDisplayZoom.Items.AddObject( ' 400% ', Tobject(4000)) ;
     cbDisplayZoom.Items.AddObject( ' 500% ', Tobject(5000)) ;
     cbDisplayZoom.Items.AddObject( ' 600% ', Tobject(6000)) ;
     cbDisplayZoom.Items.AddObject( ' 700% ', Tobject(7000)) ;
     cbDisplayZoom.Items.AddObject( ' 800% ', Tobject(8000)) ;

     cbDisplayZoom.ItemIndex := Min(Max(MainFrm.DisplayZoomIndex,0),cbDisplayZoom.Items.Count-1) ;
     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])*0.001 ;

     // Initialise frames counter
     for i := 0 to NumFrameTypes-1 do FrameCounter[i] := 0 ;

     // Intensity display palette
     cbPalette.Clear ;
     cbPalette.Items.AddObject(' Grey scale', TObject(palGrey)) ;
     cbPalette.Items.AddObject(' False colour', TObject(palFalseColor)) ;
     cbPalette.Items.AddObject(' Red scale', TObject(palRed)) ;
     cbPalette.Items.AddObject(' Green scale', TObject(palGreen)) ;
     cbPalette.Items.AddObject(' Blue scale', TObject(palBlue)) ;
     cbPalette.ItemIndex := Max(cbPalette.Items.IndexOfObject(TObject(MainFrm.PaletteType)),0) ;

     if MainFrm.IDRFile.FrameInterval <= 0.0 then begin
        MainFrm.IDRFile.FrameInterval := 1.0 ;
        end ;

     // Display contrast optimisation
     OptimiseContrastNeeded := True ;
     ckAutoOptimise.Checked := MainFrm.ContrastAutoOptimise ;
     ckChangeAllFrameTypes.Checked := MainFrm.ContrastChangeAllFrameTypes ;
     ckContrast6SDOnly.Checked := MainFrm.Contrast6SD ;

     // Initialise/update controls
     NewFile ;

     // FrameIndex always zero, since only one frame in buffer
     FrameIndex := 0 ;

     // Adjust form to accommodate size of image
     ClientWidth := Min( ImageGrp.Left + ImageGrp.Width + 5,
                         MainFrm.ClientWidth - Left - 50 ) ;

     ControlsGrp.Height := MarkGrp.Top + MarkGrp.Height + 5 ;
     ClientHeight := Max( ImageGrp.Top + ImageGrp.Height + 20,
                          ControlsGrp.Top + ControlsGrp.Height + 5 ) ;
     Top := 10 ;
     Left := 10 ;

     // Forces a display update
     CurrentPosition := -1 ;

     Resize ;

     // Update other forms with new ViewFrm settings
     UpdateOtherForms ;

     TimerBusy := False ;
     MouseDown := False ;
     MoveMode := mvNone ;
     ImageAvailable := False ;
     bForwards.Enabled := True ;
     bBackwards.Enabled := True ;
     FormClosing := False ;
     TimerCount := 0 ;
     Timer.Enabled := True ;
     ckAutoOptimise.Checked := MainFrm.ContrastAutoOptimise ;

     end;


procedure TViewFrm.FormResize(Sender: TObject);
// -----------------------------------------------
// Re-size/locate controls when form size changes
// -----------------------------------------------
begin

    // Set image panel dimensions
    SetImagePanels ;

    // Identification field
    IdentGrp.Width := Max( ClientWidth - IdentGrp.Left - 5,2 ) ;
    edIdent.Width := Max( IdentGrp.Width - edIdent.Left - 5,2 ) ;

     // Forces a display update
     CurrentPosition := -1 ;

     end;


procedure TViewFrm.NewFile ;
// -----------------------------------------------
// Update controls when data file has been changed
// -----------------------------------------------
var
     i,FT : Integer ;
     ROI : TROI ;
     BackgroundFileName : String ;
begin

     Caption := 'Images: ' + MainFrm.IDRFile.FileName ;

     FrameWidth := MainFrm.IDRFile.FrameWidth ;
     FrameHeight := MainFrm.IDRFile.FrameHeight ;

     // Experiment ident line
     edIdent.Text := MainFrm.IDRFile.Ident ;

     // Get number and types of frames in use
     if MainFrm.IDRFile.SpectralDataFile then NumFrameTypes := 1
     else NumFrameTypes := MainFrm.IDRFile.NumFrameTypes ;

     for i := 0 to NumFrameTypes-1 do
         FrameTypes[i] := MainFrm.IDRFile.FrameType[i] ;
     DisplayGrp.Caption := ' Display ' + FrameTypes[SelectedFrameType] + ' ' ;

    // Create frame pointer list
    if FrameList <> Nil then FreeMem( FrameList ) ;
    GetMem( FrameList,MainFrm.IDRFile.NumFrames*MainFrm.IDRFile.NumFrameTypes*4 ) ;
    MainFrm.IDRFile.CreateFramePointerList(FrameList);

     // Load internal ROI records from master records
     // scaled by display zoom factor
     for i := 0 to MainFrm.IDRFile.MaxROI do begin
         ROI := MainFrm.IDRFile.ROI[i] ;
         // Delete ROIs which are out of frame
         if (ROI.TopLeft.X >= MainFrm.IDRFile.FrameWidth) or
            (ROI.TopLeft.Y >= MainFrm.IDRFile.FrameHeight) or
            (ROI.BottomRight.X >= MainFrm.IDRFile.FrameWidth) or
            (ROI.BottomRight.Y >= MainFrm.IDRFile.FrameHeight) then
            ROI.InUse := False ;
         MainFrm.IDRFile.ROI[i] := ROI ;
         end ;

     UpdateDeleteROIList ;

     NumPixelsPerFrame := MainFrm.IDRFile.NumPixelsPerFrame ;
     NumBytesPerPixel := 4 ;

     // Set display intensity range and update look-up table
     SelectedFrameType := 0 ;
     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[SelectedFrameType] ;
     edDisplayIntensityRange.HiLimit := MainFrm.IDRFile.GreyMax ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[SelectedFrameType]  ;
     MainFrm.GreyHi[SelectedFrameType]  := Round(edDisplayIntensityRange.HiValue) ;

     // Set brightness & contrast slider range and position
     sbContrast.Min := 0 ;
     sbContrast.Max := MainFrm.IDRFile.GreyMax ;
     sbContrast.SmallChange := 1 ;
     sbContrast.LargeChange := Max(sbContrast.Max div 50,1) ;
     sbBrightness.Min := 0 ;
     sbBrightness.Max := MainFrm.IDRFile.GreyMax ;
     sbBrightness.LargeChange := Max(sbBrightness.Max div 50,1) ;

     // Set intensity range and sliders

     SetDisplayIntensityRange( MainFrm.GreyLo[SelectedFrameType],
                               MainFrm.GreyHi[SelectedFrameType] ) ;

     for i := 0 to NumFrameTypes-1 do begin
         MainFrm.UpdateLUT( i, MainFrm.IDRFile.GreyMax ) ;
         end ;

     // Allocate storage for image raw data buffers
     for i := 0 to High(pImageBufs) do
         if pImageBufs[i] <> Nil then begin
            FreeMem(pImageBufs[i]) ;
            pImageBufs[i] := Nil ;
            end ;
     for i := 0 to NumFrameTypes-1 do
         GetMem( pImageBufs[i], MainFrm.IDRFile.NumPixelsPerFrame*SizeOf(Integer) ) ;

     // Dispose of existing background buffers and create new ones
     for i := 0 to High(PBackgroundBufs) do if PBackgroundBufs[i] <> Nil then begin
         Try
            FreeMem(PBackgroundBufs[i]) ;
            PBackgroundBufs[i] := Nil ;
          except
            PBackgroundBufs[i] := Nil ;
            end ;
          end ;

     for FT := 0 to NumFrameTypes-1 do begin
         GetMem( PBackgroundBufs[FT],NumPixelsPerFrame*SizeOf(Integer) ) ;
         for i := 0 to NumPixelsPerFrame-1 do PBackgroundBufs[FT]^[i] := 0 ;
         end ;

     // Load background file (if it exists)
     BackgroundFileName := ANSIReplaceText(MainFrm.IDRFile.FileName,'.idr','[BACKG].idr') ;
     if FileExists(BackgroundFileName) then begin
        IDRBackground.OpenFile(BackgroundFileName) ;
        for i := 0 to NumFrameTypes-1 do begin
            IDRBackground.LoadFrame32( i+1, pBackGroundBufs[i] ) ;
            end ;
        IDRBackground.CloseFile ;
        end ;

     // Set limits of frame select slider and edit box
     sbFrameNum.SmallChange := 1 ;
     sbFrameNum.LargeChange := Max(MainFrm.IDRFile.NumFrames div 100,1) ;
     sbFrameNum.Min := 1 ;
     sbFrameNum.Min := 1 ;
     sbFrameNum.Max := MainFrm.IDRFile.NumFrames ;
     sbFrameNum.Position := 1 ;
     edFrameNum.LoLimit :=  sbFrameNum.Min ;
     edFrameNum.HiLimit :=  sbFrameNum.Max ;
     edFrameNum.LoValue :=  sbFrameNum.Min ;
     edFrameNum.HiValue :=  sbFrameNum.Max ;

     // Resize display components
     SetImagePanels ;
     sbXScroll.Position := 0 ;
     sbYScroll.Position := 0 ;

     // Update plotting window settings
     if CheckViewPlotFrmExists then ViewPlotFrm.NewFile ;

     NewFileNeeded := False ;
     OptimiseContrastNeeded := True ;
     CurrentPosition := -1 ;

     end ;


procedure TViewFrm.UpdateDeleteROIList ;
// -------------------------
// Update Delete ROI options
// -------------------------
var
    i : Integer ;
begin

     // Set all regions of interest deletion list
     cbDeleteROI.Clear ;
     cbDeleteROI.Items.AddObject('All ROIs',TObject(0)) ;
     for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then begin
        cbDeleteROI.Items.AddObject(format('ROI.%d',[i]),TObject(i)) ;
        end ;
     cbDeleteROI.ItemIndex := 0 ;

     end ;


procedure TViewFrm.SetImagePanels ;
// -------------------------------------------
// Set size and number of image display panels
// -------------------------------------------
const
    MaxFrames = 9 ;
    MarginPixels = 16 ;
var
     i : Integer ;
    ImageAreaHeight : Integer ;
    ImageAreaWidth : Integer ;
    ImageColumns : Integer ;
    ImageRows : Integer ;
    RightEdge : Integer ;
    BottomEdge : Integer ;
begin

     // Get number and types of frames in use
     if MainFrm.IDRFile.SpectralDataFile then NumFrameTypes := 1
     else NumFrameTypes := MainFrm.IDRFile.NumFrameTypes ;

     // Dispose of existing bit maps
     for i := 0 to High(BitMaps) do if BitMaps[i] <> Nil then begin
         BitMaps[i].Free  ;

         BitMaps[i] := Nil ;
         end ;
     // Create new ones
     for i := 0 to NumFrameTypes-1 do begin
         BitMaps[i] := TBitMap.Create ;
         end ;

     // Create list of image areas on display
     Images[0] := Image1 ;
     Images[1] := Image2 ;
     Images[2] := Image3 ;
     Images[3] := Image4 ;
     Images[4] := Image5 ;
     Images[5] := Image6 ;
     Images[6] := Image7 ;
     Images[7] := Image8 ;
     Images[8] := Image9 ;

     // Initialise images as tiny and invisible
     for i := 0 to High(Images) do begin
         Images[i].Visible := False ;
         Images[i].Width := 1 ;
         Images[i].Height := 1 ;
         end ;

     // Set size and pen/brush characteristics of images in use

     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])*0.001 ;

     ImageGrp.ClientWidth :=  Max( ClientWidth - ImageGrp.Left - 5, 2) ;
     ImageGrp.ClientHeight :=  Max( ClientHeight - ImageGrp.Top - 5, 2) ;

     PanROITools.Top := ImageGrp.ClientHeight - PanROITools.Height - 2 ;

     // Determine number of image columns and rows
     ImageRows := Round(Sqrt(NumFrameTypes)) ;
     ImageColumns := ImageRows ;
     if (ImageRows*ImageColumns) < NumFrameTypes then Inc(ImageRows) ;

     ImageAreaWidth := Max( ImageGrp.ClientWidth - sbYScroll.Width
                            - ((ImageColumns+1)*MarginPixels),2) ;
     ImageAreaHeight := Max( PanROITools.Top - sbXScroll.Height - cbDisplayZoom.Height - cbDisplayZoom.Top
                             - ((ImageRows+1)*MarginPixels),2) ;

     BitMaps[0].Width := Max(Min( ImageAreaWidth div ImageColumns,
                                  Round(MainFrm.IDRFile.FrameWidth*DisplayZoom)),2) ;
     BitMaps[0].Height := Max(Min( ImageAreaHeight div ImageRows,
                                   Round(MainFrm.IDRFile.FrameHeight*DisplayZoom)),2) ;

     RightEdge := 0 ;
     BottomEdge := 0 ;
     for i := 0 to NumFrameTypes-1 do begin

         Images[i].Visible := True ;
         BitMaps[i].Width := BitMaps[0].Width ;
         BitMaps[i].Height := BitMaps[0].Height ;

         MainFrm.SetPalette( BitMaps[i], MainFrm.PaletteType ) ;

         Images[i].Width := BitMaps[0].Width ;
         Images[i].Height := BitMaps[0].Height ;

         Images[i].Left := (i mod ImageColumns)*
                           (BitMaps[0].Width + MarginPixels)
                           + MarginPixels ;
         Images[i].Top := (i div ImageColumns)*
                          (BitMaps[0].Height + MarginPixels)
                           + cbDisplayZoom.Top + cbDisplayZoom.Height + 2 ;

         Images[i].Canvas.Pen.Color := clWhite ;
         Images[i].Canvas.Brush.Style := bsClear ;
         Images[i].Canvas.Font.Color := clWhite ;
         Images[i].Canvas.TextFlags := 0 ;
         Images[i].Canvas.Pen.Mode := pmXOR ;
         Images[i].Canvas.Font.Name := 'Arial' ;
         Images[i].Canvas.Font.Size := 8 ;
         Images[i].Canvas.Font.Color := clBlue ;

         // Determine right/bottom edge of image area
         RightEdge := Max(Images[i].Left + Images[i].Width + 1,RightEdge) ;
         BottomEdge := Max(Images[i].Top + Images[i].Height + 1,BottomEdge) ;

         end ;

     // Position image scroll bars at right and bottom edges of image area
     sbXScroll.Top := BottomEdge ;
     sbXScroll.Left :=  Images[0].Left ;
     sbXScroll.Width := RightEdge - sbXScroll.Left ;

     sbYScroll.Left := RightEdge ;
     sbYScroll.Top := Images[0].Top ;
     sbYScroll.Height := BottomEdge - sbYScroll.Top ;

     // Image scroll bar range
     sbXScroll.Max := Max( MainFrm.IDRFile.FrameWidth - Round(Images[0].Width/DisplayZoom),1) ;
     sbYScroll.Max := Max( MainFrm.IDRFile.FrameHeight - Round(Images[0].Height/DisplayZoom),1) ;

     end ;


procedure TViewFrm.SetDisplayIntensityRange(
          LoValue : Integer ;
          HiValue : Integer
          ) ;
// --------------------------------------
// Set display contrast range and sliders
// --------------------------------------
begin

     edDisplayIntensityRange.LoValue := LoValue  ;
     edDisplayIntensityRange.HiValue := HiValue  ;
     sbBrightness.Position := (LoValue + HiValue) div 2 ;
     sbContrast.Position := HiValue - LoValue ;

     end ;


function TViewFrm.CheckViewPlotFrmExists : Boolean ;
// -------------------------------------------
// Check that time course plotting form exists
// -------------------------------------------
begin

     // Is there a ViewPlotFrm window open?
     // If not open it
     if not MainFrm.FormExists( 'ViewPlotFrm') then begin
        ViewPlotFrm := TViewPlotFrm.Create(Self) ;
        ViewPlotFrm.Top := Top ;
        ViewPlotFrm.Left := Left + Width + 10 ;
        ViewPlotFrm.Width := Max( MainFrm.ClientWidth - (Width + Left) - 10, 50 ) ;
        ViewPlotFrm.DisplayGrid := MainFrm.mnDisplayGrid.Checked ;
        Application.ProcessMessages ;
        end ;
     Result := True ;

     end ;


procedure TViewFrm.sbFrameNumChange(Sender: TObject);
// ---------------
// Frame # changed
// ---------------
begin
     // Update frame number readout/edit box
     edFrameNum.LoValue := sbFrameNum.Position ;
     CurrentPosition := -1 ;
     OptimiseContrastNeeded := ckAutoOptimise.Checked ;

     end;


procedure TViewFrm.TimerTimer(Sender: TObject);
// --------------------------------
// Timed procedure to display image
// --------------------------------
var
     iFrameType,i : Integer ;
     OK : Boolean ;
     FrameTime : Single ;     // Acquisition time of frame (s)
begin

     // Skip if procedure is already executing
     if TimerBusy then exit ;

     TimerBusy := True ;

     // Play frame forwards
     if not bForwards.Enabled then begin
        if sbFrameNum.Position < sbFrameNum.Max then begin
           sbFrameNum.Position := sbFrameNum.Position + 1 ;
           end
        else bForwards.Enabled := True ;
        end ;

     // Play frame forwards
     if not bBackwards.Enabled then begin
        if sbFrameNum.Position > 0 then
           sbFrameNum.Position := sbFrameNum.Position - 1
        else bBackwards.Enabled := True ;
        end ;

     if NewFileNeeded then NewFile ;

     if CurrentPosition <> sbFrameNum.Position then begin

        CurrentPosition := sbFrameNum.Position ;

        // Time of acquisition
        FrameTime := (CurrentPosition + (NumFrameTypes-1) )*MainFrm.IDRFile.FrameInterval ;
        edFrameTime.Text := format( ' %.5g %s',[FrameTime*MainFrm.TScale,MainFrm.TUnits]) ;

        for iFrameType := 0 to NumFrameTypes - 1 do begin

            if (pImageBufs[iFrameType] = Nil) then Break ;

            // Get frame from data file
            OK := MainFrm.IDRFile.LoadFrame32( FrameList^[(CurrentPosition-1)*NumFrameTypes+iFrameType],
                                               pImageBufs[iFrameType] ) ;

            // Subtract shading correction
            if ckBackgroundSubtraction.Checked then begin
               for i := 0 to NumPixelsPerFrame-1 do begin
                   pImageBufs[iFrameType]^[i] := pImageBufs[iFrameType]^[i] - PBackgroundBufs[iFrameType]^[i] ;
                   end ;
               end ;

            if OK then begin

               // Display frame in appropriate area
               if DisplayZoom < 1.0 then
                  DisplayImageZoom50( MainFrm.LUTs[iFrameType*LutSize],
                                      pImageBufs[iFrameType],
                                      BitMaps[iFrameType],
                                      Images[iFrameType] )
               else
                  DisplayImage( MainFrm.LUTs[iFrameType*LutSize],
                                pImageBufs[iFrameType],
                                BitMaps[iFrameType],
                                Images[iFrameType] ) ;

               FrameCounter[iFrameType] := CurrentPosition ;
               end ;

            end ;

        // Update regions of interest
        UpdateROIs ;

        // Place any marker text at bottom-right of frame type 0
        Images[0].Canvas.Pen.Color := clWhite ;
        Images[0].Canvas.Pen.Width := 2 ;
        Images[0].Canvas.Brush.Style := bsClear ;
        Images[0].Canvas.Font.Color := clWhite ;
        for i := 0 to MainFrm.IDRFile.NumMarkers-1 do
            if Abs(MainFrm.IDRFile.MarkerTime[i] - FrameTime)
               < (MainFrm.IDRFile.FrameInterval*NumFrameTypes) then
               Images[0].Canvas.TextOut(
               Images[0].Width - Images[0].Canvas.TextWidth(MainFrm.IDRFile.MarkerText[i]),
               Images[0].Height - 3 - Images[0].Canvas.TextHeight('X'),
               MainFrm.IDRFile.MarkerText[i] ) ;

        // Display ROI and A/D time courses
        if CheckViewPlotFrmExists then ViewPlotFrm.DisplayTimeCourse( CurrentPosition ) ;

        end ;

     if OptimiseContrastNeeded then bMaxContrast.Click ;

     TimerBusy := False ;

     end ;


procedure TViewFrm.DisplayImageZoom50(
          var LUT : Array of Word ; // Display look-up table
          pImageBuf : PIntArray ; // Pointer to raw image data buffer (IN)
          BitMap : TBitMap ;        // Bit map containing image to be displayed (IN)
          Image : TImage ) ;        // Image to be updated (OUT)
// ----------------------------------
// Display image at 50% magnification
// ----------------------------------
var
    i,k : Integer ;
    Xim,Yim,Xbm,Ybm : Integer ;
    iStep : Integer ;
    PScanLine : PByteArray ;
    FrameTime : Single ;
begin

    if (pImageBuf = Nil) or (BitMap = Nil) then Exit ;

    Ybm := 0 ;
    Yim := sbYScroll.Position ;
    iStep := Round(1.0/DisplayZoom) ;
    while (Ybm < BitMap.Height) and (Yim < MainFrm.IDRFile.FrameHeight) do begin

      // Get scan line array pointer
      PScanLine := BitMap.ScanLine[Ybm] ;

      // Copy line to bitmap
      xBm := 0 ;
      XIm := sbXScroll.Position ;
      i := (Yim*MainFrm.IDRFile.FrameWidth) + XIm ;
      while (Xbm < BitMap.Width) and
            (XIm < MainFrm.IDRFile.FrameWidth) and
            (i < NumPixelsPerFrame) do begin
          k := Max(Min(pImageBuf^[i],LUTSize-1),0) ;
          PScanLine[Xbm] := LUT[k] ;
          Inc(Xbm) ;
          Xim := Xim + iStep ;
          i := i + iStep ;
          end ;

      Inc(Ybm) ;
      Yim := Yim + iStep

      end ;

    // Add calibration bar
    if ckDisplayCalBar.Checked then DisplayCalibrationBar( BitMap ) ;

    Image.Picture.Assign(BitMap) ;

    // Display regions of interest
    Image.Canvas.Pen.Color := clWhite ;
    Image.Canvas.Brush.Style := bsClear ;
    Image.Canvas.Font.Color := clWhite ;

    // Place any marker text at bottom-right of frame type 0
    BitMap.Canvas.Pen.Color := clWhite ;
    BitMap.Canvas.Pen.Width := 2 ;
    BitMap.Canvas.Brush.Style := bsClear ;
    BitMap.Canvas.Font.Color := clWhite ;
    FrameTime := (CurrentPosition)*MainFrm.IDRFile.FrameInterval ;
    for i := 0 to MainFrm.IDRFile.NumMarkers-1 do
        if Abs(MainFrm.IDRFile.MarkerTime[i] - FrameTime)
           < (MainFrm.IDRFile.FrameInterval*NumFrameTypes) then
           BitMap.Canvas.TextOut(
           BitMap.Width - BitMap.Canvas.TextWidth(MainFrm.IDRFile.MarkerText[i]),
           BitMap.Height - 3 - BitMap.Canvas.TextHeight('X'),
           MainFrm.IDRFile.MarkerText[i] ) ;

    ImageAvailable := True ;

    end ;


procedure TViewFrm.DisplayImage(
          var LUT : Array of Word ;  // Display look-up table
          var pImageBuf : PIntArray ;  // Pointer to raw image data buffer (IN)
          var BitMap : TBitMap ;         // Bit map containing image to be displayed (IN)
          var Image : TImage ) ;         // Image to be updated (OUT)
// --------------
// Display image
// --------------
var
    Ybm,Yim,i,j,k,Xbm,StartOfLine : Integer ;
     PScanLine,PScanLine1 : PByteArray ;
    FrameTime : Single ;
begin

    if (pImageBuf = Nil) or (BitMap = Nil) then Exit ;

    // Index to start of image line in circular frame buffer
    Ybm := 0 ;
    StartOfLine := (sbYScroll.Position*MainFrm.IDRFile.FrameWidth)
                   + sbXScroll.Position ;

    for Yim := sbYScroll.Position to MainFrm.IDRFile.FrameHeight-1 do begin

        // Create line
        PScanLine := BitMap.ScanLine[Ybm] ;
        Xbm := 0 ;
        for i := StartOfLine to StartOfLine + MainFrm.IDRFile.FrameWidth-1 do
            if (i < NumPixelsPerFrame) then begin
            // Read pixel value
            k := Max(Min(pImageBuf^[i],LUTSize-1),0) ;
            // Create output pixel(s)
            for j := 1 to Round(DisplayZoom) do begin
                PScanLine[Xbm] := LUT[k] ;
                Inc(Xbm) ;
                end ;
            if Xbm >= BitMap.Width then break ;
            end ;

        // Create additional lines
        for i := 1 to Round(DisplayZoom)-1 do begin
            Inc(Ybm) ;
            if Ybm >= Bitmap.Height then break ;
            PScanLine1 := BitMap.ScanLine[Ybm] ;
            for Xbm := 0 to Bitmap.Width-1 do PScanLine1[Xbm] := PScanLine[Xbm] ;
            end ;

       StartOfLine := StartOfLine + MainFrm.IDRFile.FrameWidth ;

       Inc(Ybm) ;
       if Ybm >= Bitmap.Height then break ;

       end ;

    // Add calibration bar
    if ckDisplayCalBar.Checked then DisplayCalibrationBar( BitMap ) ;

    // Place any marker text at bottom-right of frame type 0
    BitMap.Canvas.Pen.Color := clWhite ;
    BitMap.Canvas.Pen.Width := 2 ;
    BitMap.Canvas.Brush.Style := bsClear ;
    BitMap.Canvas.Font.Color := clWhite ;
    FrameTime := (CurrentPosition)*MainFrm.IDRFile.FrameInterval ;
    for i := 0 to MainFrm.IDRFile.NumMarkers-1 do
        if Abs(MainFrm.IDRFile.MarkerTime[i] - FrameTime)
           < (MainFrm.IDRFile.FrameInterval*NumFrameTypes) then
           BitMap.Canvas.TextOut(
           BitMap.Width - BitMap.Canvas.TextWidth(MainFrm.IDRFile.MarkerText[i]),
           BitMap.Height - 3 - BitMap.Canvas.TextHeight('X'),
           MainFrm.IDRFile.MarkerText[i] ) ;

    Image.Picture.Assign(BitMap) ;
    Canvas.Pen.Mode := pmXOR ;
    ImageAvailable := True ;

    end ;


procedure TViewFrm.DisplayCalibrationBar(
          BitMap : TBitMap  // Bit map to be written to
          ) ;
// ---------------------------------------
// Add a spatial calibration bar to bitmap
// ---------------------------------------
var
    iCalBarThickness,iCalBarSize,iTop : Integer ;
    KeepPen : TPen ;
    KeepBrush : TBrush ;
begin

     if (BitMap = Nil) then Exit ;

     // Keep existing settings
     KeepPen := TPen.Create ;
     KeepPen.Assign(Canvas.Pen) ;
     KeepBrush := TBrush.Create ;
     KeepBrush.Assign(Canvas.Brush) ;

     iCalBarSize := Round((MainFrm.CalibrationBarSize*DisplayZoom)/MainFrm.IDRFile.XResolution) ;
     iCalBarThickness := Max( 1, Round(MainFrm.CalibrationBarThickness) ) ;

     Bitmap.Canvas.Pen.Color := clWhite ;
     Canvas.Pen.Mode := pmCopy ;
     Bitmap.Canvas.Pen.Width := 1 ;
     Bitmap.Canvas.Brush.Style := bsClear ;

     Bitmap.Canvas.Font.Color := clWhite ;

     iTop := Bitmap.Height - Bitmap.Canvas.TextHeight('X') ;
     Bitmap.Canvas.TextOut( 2,
                            iTop,
                            format('%.4g %s',[MainFrm.CalibrationBarSize,MainFrm.IDRFile.ResolutionUnits])) ;

     Bitmap.Canvas.Brush.Color := clWhite ;
     Bitmap.Canvas.Brush.Style := bsSolid ;
     iTop := iTop - 2 ;
     Bitmap.Canvas.Rectangle( 2,Max(0,iTop-iCalBarThickness),
                              Min(2+iCalBarSize,Bitmap.Width-1),iTop) ;

     // Restore settings
     Canvas.Pen.Assign(KeepPen) ;
     KeepPen.Free ;
     Canvas.Brush.Assign(KeepBrush) ;
     KeepBrush.Free ;


     end ;


procedure TViewFrm.edFrameNumKeyPress(Sender: TObject; var Key: Char);
// -------------------------------------------------------
// Update frame slider bar position with entered frame no.
// -------------------------------------------------------
begin
     if key = #13 then begin
        // Note. Ensure selected frame number is ALWAYS first frame in sequence
        sbFrameNum.Position := Round(edFrameNum.LoValue) ;
        edFrameNum.HiValue :=  sbFrameNum.Max ;
        // Note This triggers display of new image
        end ;
     end;


procedure TViewFrm.bFullScaleClick(Sender: TObject);
// ------------------------------------------------------
// Set display look-up table to full range of grey levels
// ------------------------------------------------------
var
    FrameType : Integer ;
begin

     for FrameType := 0 to NumFrameTypes-1 do
         if ckChangeAllFrameTypes.Checked or (FrameType = SelectedFrameType) then begin

         edDisplayIntensityRange.LoValue := 0 ;
         MainFrm.GreyLo[FrameType] := Round(edDisplayIntensityRange.LoValue) ;
         edDisplayIntensityRange.HiValue := MainFrm.IDRFile.GreyMax ;
         MainFrm.GreyHi[FrameType] := Round(edDisplayIntensityRange.HiValue) ;

         MainFrm.UpdateLUT( FrameType, MainFrm.IDRFile.GreyMax ) ;

         // Set upper/lower limits of time course display
         if CheckViewPlotFrmExists then begin
            ViewPlotFrm.FLYMax[FrameType] := MainFrm.GreyHi[FrameType] ;
            ViewPlotFrm.FLYMin[FrameType] := 0 ;
            end ;

         end ;

     CurrentPosition := -1 ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[SelectedFrameType],
                               MainFrm.GreyHi[SelectedFrameType] ) ;

     end;


procedure TViewFrm.CalculateMaxContrast(
          FrameType : Integer ) ; // Frame type
// ---------------------------------------------------------
// Calculate and set display for maximum grey scale contrast
// ---------------------------------------------------------
const
    PixelSampleSize = 10000 ;
var
     i,NumPixels,NAvg,Istep : Integer ;
     z,zMean,zSD,zSum : Single ;
     iz,ZMin,ZMax,ZLo,ZHi : Integer ;
begin

    if pImageBufs[FrameType] = Nil then Exit ;

    NumPixels := (MainFrm.IDRFile.FrameHeight*MainFrm.IDRFile.FrameWidth - 4) ;
    iStep := Max(NumPixels div PixelSampleSize,1) ;

    if NumPixels < 2 then Exit ;

    if ckContrast6SDOnly.Checked then begin
       ZSum := 0.0 ;
       nAvg := 0 ;
       i := 0 ;
       while i < NumPixels do begin
          ZSum := ZSum + pImageBufs[FrameType]^[i] ;
          i := i + iStep ;
          Inc(NAvg) ;
          end ;
       ZMean := ZSum / nAvg ;

       ZSum := 0.0 ;
       nAvg := 0 ;
       i := 0 ;
       while i < NumPixels do begin
          Z := pImageBufs[FrameType]^[i] ;
          ZSum := ZSum + (Z - ZMean)*(Z - ZMean) ;
          i := i + iStep ;
          Inc(NAvg) ;
          end ;
       ZSD := Sqrt( ZSum / (NumPixels-1) ) ;

       ZLo := Max( Round(ZMean - 3*ZSD),0) ;
       ZHi := Min( Round(ZMean + 3*ZSD), MainFrm.IDRFile.GreyMax );

       end
    else begin
       // Set contrast range to min-max
       ZMin := MainFrm.IDRFile.GreyMax ;
       ZMax := 0 ;
       ZSum := 0.0 ;
       nAvg := 0 ;
       i := 0 ;
       while i < NumPixels do begin
          iz := pImageBufs[FrameType]^[i] ;
          if iz < ZMin then ZMin := iz ;
          if iz > ZMax then ZMax := iz ;
          i := i + iStep ;
          end ;

       ZLo := ZMin ;
       ZHi := ZMax ;

       end ;

    ZLo := Max(Round(0.9*ZLo),0) ;
    ZHi := Min(Round(1.1*ZHi),MainFrm.IDRFile.GreyMax) ;

    // Update contrast
    if (not ckAutoOptimise.Checked) or
       (Abs(MainFrm.GreyLo[FrameType]- ZLo) > 10) then MainFrm.GreyLo[FrameType] := ZLo ;
    if (not ckAutoOptimise.Checked) or
       (Abs(MainFrm.GreyHi[FrameType]- ZHi) > 10) then MainFrm.GreyHi[FrameType] := ZHi ;

    SetDisplayIntensityRange( MainFrm.GreyLo[FrameType],
                              MainFrm.GreyHi[FrameType] ) ;

    MainFrm.UpdateLUT(FrameType, MainFrm.IDRFile.GreyMax ) ;

    end ;


procedure TViewFrm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
// ----------------------------------
// Update readout when mouse is moved
// ----------------------------------
begin

     // Save image readout cursor position
     ImageCursor.X := Round(x/DisplayZoom) + sbXScroll.Position ;
     ImageCursor.Y := Round(y/DisplayZoom) + sbYScroll.Position ;

     // Update status bar with new readout values
     UpdateStatusBar ;

     AdjustROI( ImageCursor.X, ImageCursor.Y ) ;

     end;


procedure TViewFrm.AdjustROI(
          X : Integer ;         // Mouse X position (IN)
          Y : Integer ) ;       // Mouse Y position (IN)
// ----------------------------------------------------------
// Update regions of interest and cursor when mouse is moved
// ----------------------------------------------------------
const
    Margin = 2 ;
var
    NewCursor : TCursor ;
    i,j : Integer ;
    LeftEdge : Integer ;  // Rectangular bounds of current ROI
    RightEdge : Integer ;
    TopEdge : Integer ;
    BottomEdge : Integer ;

    ImageWidth : Integer ;
    ImageHeight : Integer ;
    PointShift : TPoint ;
    LiveROI : TROI ;
    OverLeftEdge : Boolean ;
    OverRightEdge : Boolean ;
    OverTopEdge : Boolean ;
    OverBottomEdge : Boolean ;
    Slope,YLine,dX,dY,XLine : Single ;
    XMin,XMax,YMin,YMax : Single ;

begin

     ImageWidth := MainFrm.IDRFile.FrameWidth ;
     ImageHeight := MainFrm.IDRFile.FrameHeight ;

     // Keep within image limits
     X := Max(Min(X,ImageWidth-1),0) ;
     Y := Max(Min(Y,ImageHeight-1),0) ;

     // Scale active ROI into bitmap coordinates
     LiveROI := MainFrm.IDRFile.ROI[0] ;

     if (not MouseDown) then begin

         // Not in move mode - determine whether mouse is over ROI

         NewCursor := Images[0].Cursor ;

         LeftEdge := Min(LiveROI.TopLeft.X,LiveROI.BottomRight.X) ;
         RightEdge := Max(LiveROI.TopLeft.X,LiveROI.BottomRight.X) ;
         TopEdge := Min(LiveROI.TopLeft.Y,LiveROI.BottomRight.y) ;
         BottomEdge := Max(LiveROI.TopLeft.Y,LiveROI.BottomRight.y) ;

         NewCursor := crDefault ;
         MoveMode := mvNone ;

         case LiveROI.Shape of

              PolylineROI,PolygonROI : begin
                  for j:= 1 to LiveROI.NumPoints-1 do begin
                      XMin :=  Min(LiveROI.XY[j].X,LiveROI.XY[j-1].X) ;
                      XMax :=  Max(LiveROI.XY[j].X,LiveROI.XY[j-1].X) ;
                      YMin :=  Min(LiveROI.XY[j].Y,LiveROI.XY[j-1].Y) ;
                      YMax :=  Max(LiveROI.XY[j].Y,LiveROI.XY[j-1].Y) ;
                      dX := LiveROI.XY[j].X - LiveROI.XY[j-1].X ;
                      dY := LiveROI.XY[j].Y - LiveROI.XY[j-1].Y ;
                      if Abs(dX) > Abs(dY) then begin
                         YLine := ((ImageCursor.X - LiveROI.XY[j-1].X)*(dY/dX)) + LiveROI.XY[j-1].Y ;
                         YLine := Min(Max(YLine,Ymin),YMax) ;
                         if (Abs(ImageCursor.Y - YLine) < Margin) and
                            (ImageCursor.X >= XMin) and (ImageCursor.X <= XMax) then begin
                            NewCursor := crDrag ;
                            MoveMode := mvAll ;
                            end ;
                         end
                      else if Abs(dY) > Abs(dX) then begin
                         XLine := ((ImageCursor.Y - LiveROI.XY[j-1].Y)*(dX/dY)) + LiveROI.XY[j-1].X ;
                         XLine := Min(Max(XLine,Xmin),XMax) ;
                         if (Abs(ImageCursor.X - XLine) < Margin) and
                            (ImageCursor.Y >= YMin) and (ImageCursor.Y <= YMax) then begin
                            NewCursor := crDrag ;
                            MoveMode := mvAll ;
                            end ;
                         end ;
                      end ;
                  end ;

              PointROI : begin
                   if (Abs(LiveROI.Centre.X - ImageCursor.X) <= Margin) and
                      (Abs(LiveROI.Centre.Y - ImageCursor.Y) <= Margin) then begin
                      NewCursor := crDrag ;
                      MoveMode := mvAll ;
                      end ;
                   end ;

              LineROI : begin
                  if (Abs(X-LiveROI.TopLeft.X) <= Margin) and
                     (Abs(Y-LiveROI.TopLeft.Y) <= Margin) then begin
                     // Top/left point
                     NewCursor := crSizeAll ;
                     MoveMode := mvTopLeft ;
                     end
                  else if (Abs(X-LiveROI.BottomRight.X) <= Margin) and
                          (Abs(Y-LiveROI.BottomRight.Y) <= Margin) then begin
                     // Bottom-right point
                     NewCursor := crSizeAll ;
                     MoveMode := mvBottomRight ;
                     end
                  else begin
                     XMin :=  Min(LiveROI.BottomRight.X,LiveROI.TopLeft.X) ;
                     XMax :=  Max(LiveROI.BottomRight.X,LiveROI.TopLeft.X) ;
                     YMin :=  Min(LiveROI.BottomRight.Y,LiveROI.TopLeft.Y) ;
                     YMax :=  Max(LiveROI.BottomRight.Y,LiveROI.TopLeft.Y) ;
                     dX := LiveROI.BottomRight.X - LiveROI.TopLeft.X ;
                     dY := LiveROI.BottomRight.Y - LiveROI.TopLeft.Y ;
                     if Abs(dX) > Abs(dY) then begin
                        YLine := ((ImageCursor.X - LiveROI.TopLeft.X)*(dY/dX)) + LiveROI.TopLeft.Y ;
                        YLine := Min(Max(YLine,Ymin),YMax) ;
                        if (Abs(ImageCursor.Y - YLine) < Margin) and
                           (ImageCursor.X >= XMin) and (ImageCursor.X <= XMax) then begin
                            NewCursor := crDrag ;
                            MoveMode := mvAll ;
                            end ;
                        end
                     else if Abs(dX) < Abs(dY) then begin
                        XLine := ((ImageCursor.Y - LiveROI.TopLeft.Y)*(dX/dY)) + LiveROI.TopLeft.X ;
                        XLine := Min(Max(XLine,Xmin),XMax) ;
                        if (Abs(ImageCursor.X - XLine) < Margin) and
                           (ImageCursor.Y >= YMin) and (ImageCursor.Y <= YMax) then begin
                           NewCursor := crDrag ;
                           MoveMode := mvAll ;
                           end ;
                        end ;
                     end ;

                  end ;

              else begin
                  // Other ROI types
                   if (TopEdge <= Y) and (Y <= BottomEdge) and
                      (LeftEdge <= X) and (X <= RightEdge) then begin

                      OverLeftEdge := False ;
                      OverRightEdge := False ;
                      OverTopEdge := False ;
                      OverBottomEdge := False ;
                      if Abs(X-LiveROI.TopLeft.X) <= Margin then OverLeftEdge := True
                      else if Abs(X-LiveROI.BottomRight.X) <= Margin then OverRightEdge := True ;
                      if Abs(Y-LiveROI.TopLeft.Y) <= Margin then OverTopEdge := True
                      else if Abs(Y-LiveROI.BottomRight.Y) <= Margin then OverBottomEdge := True ;

                      if OverLeftEdge then begin
                         if OverTopEdge then begin
                            NewCursor := crSizeNWSE ;
                            MoveMode := mvTopLeft ;
                            end
                         else if OverBottomEdge then begin
                            NewCursor := crSizeNESW ;
                            MoveMode := mvBottomLeft ;
                            end
                         else begin
                            NewCursor := crSizeWE ;
                            MoveMode := mvLeftEdge ;
                            end ;
                         end
                      else if OverRightEdge then begin
                         if OverTopEdge then begin
                            NewCursor := crSizeNESW ;
                            MoveMode := mvTopRight ;
                            end
                         else if OverBottomEdge then begin
                            NewCursor := crSizeNWSE ;
                            MoveMode := mvBottomRight ;
                            end
                         else begin
                            NewCursor := crSizeWE ;
                            MoveMode := mvRightEdge ;
                            end ;
                         end
                      else if OverBottomEdge then begin
                         NewCursor := crSizeNS ;
                         MoveMode := mvBottomEdge ;
                         end
                      else if OverTopEdge then begin
                         NewCursor := crSizeNS ;
                         MoveMode := mvTopEdge ;
                         end
                      else begin
                         NewCursor := crDrag ;
                         MoveMode := mvAll ;
                         end ;

                      end ;
                   end ;
              end ;

        // Update image cursors
        for i := 0 to High(Images) do Images[i].Cursor := NewCursor ;

        end
     else if MouseDown then begin

        // Mouse button is down - adjust ROI if in one of the move modes

        case MoveMode of

             mvAll : begin
                PointShift.X := X - LiveROI.Centre.X ;
                PointShift.Y := Y - LiveROI.Centre.Y ;
                LiveROI.Centre := Point(X,Y) ;
                LiveROI.TopLeft.X := LiveROI.Centre.X - LiveROI.Width div 2 ;
                LiveROI.TopLeft.Y := LiveROI.Centre.Y - LiveROI.Height div 2 ;
                LiveROI.BottomRight.X := LiveROI.TopLeft.X + LiveROI.Width - 1 ;
                LiveROI.BottomRight.Y := LiveROI.TopLeft.Y + LiveROI.Height - 1 ;
                for i := 0 to LiveROI.NumPoints-1 do begin
                    LiveROI.XY[i].X := LiveROI.XY[i].X + PointShift.X ;
                    LiveROI.XY[i].Y := LiveROI.XY[i].Y + PointShift.Y ;
                    end ;
                end ;

             mvLeftEdge : LiveROI.TopLeft.X := X  ;
             mvRightEdge : LiveROI.BottomRight.X := X  ;
             mvTopEdge : LiveROI.TopLeft.Y := Y ;
             mvBottomEdge : LiveROI.BottomRight.Y := Y ;
             mvTopLeft : Begin
               LiveROI.TopLeft.X := X  ;
               LiveROI.TopLeft.Y := Y  ;
               end ;
             mvTopRight : Begin
               LiveROI.BottomRight.X := X  ;
               LiveROI.TopLeft.Y := Y  ;
               end ;
             mvBottomLeft : Begin
               LiveROI.TopLeft.X := X  ;
               LiveROI.BottomRight.Y := Y  ;
               end ;
             mvBottomRight : Begin
               LiveROI.BottomRight.X := X  ;
               LiveROI.BottomRight.Y := Y  ;
               end ;

             end ;

        // Computer width/centre
        LiveROI.Width := LiveROI.BottomRight.X - LiveROI.TopLeft.X + 1 ;
        LiveROI.Height := LiveROI.BottomRight.Y - LiveROI.TopLeft.Y + 1 ;
        LiveROI.Centre.X := LiveROI.TopLeft.X + LiveROI.Width div 2 ;
        LiveROI.Centre.Y := LiveROI.TopLeft.Y + LiveROI.Height div 2 ;

        // Keep within image limits
        LiveROI.TopLeft.X := Max(Min(LiveROI.TopLeft.X,ImageWidth - LiveROI.Width),0) ;
        LiveROI.BottomRight.X := LiveROI.TopLeft.X + LiveROI.Width -1 ;

        LiveROI.TopLeft.Y := Max(Min(LiveROI.TopLeft.Y,ImageHeight - LiveROI.Height),0) ;
        LiveROI.BottomRight.Y := LiveROI.TopLeft.Y + LiveROI.Height - 1 ;

        LiveROIChanged := True ;

        end ;

     // Copy back to live ROI 
     MainFrm.IDRFile.ROI[0] := LiveROI  ;

     // Update regions of interest
     UpdateROIs ;

     end ;


procedure TViewFrm.UpdateStatusBar ;
// ---------------------------------------------------
// Update status bar panels with cursor readout values
// ---------------------------------------------------
var
    i,iFrameType : Integer ;
    s : String ;
begin

     // x,y position of readout cursor
     s := format(' x= %6.3g %s y= %6.3g %s',
         [ImageCursor.X*MainFrm.IDRFile.XResolution,
          MainFrm.IDRFile.ResolutionUnits,
          (MainFrm.IDRFile.FrameHeight-ImageCursor.Y)*MainFrm.IDRFile.XResolution,
           MainFrm.IDRFile.ResolutionUnits] ) ;

     // Display Intensity
      i := ImageCursor.X + MainFrm.IDRFile.FrameWidth*ImageCursor.Y ;
     for iFrameType := 0 to NumFrameTypes-1 do
         if (pImageBufs[iFrameType] <> Nil) and
            (i < MainFrm.IDRFile.FrameWidth*MainFrm.IDRFile.FrameHeight) and
            (i>=0) then begin
         s := s + format(' %s = %7.5g',
         [FrameTypes[iFrameType],
          MainFrm.IDRFile.IntensityScale*
          (pImageBufs[iFrameType]^[i] - MainFrm.IDRFile.IntensityOffset)]) ;
         end ;

     MainFrm.StatusBar.SimpleText := s ;

     end ;


procedure TViewFrm.UpdateROIs ;
// ----------------------------------------------
// Update regions of interest on displayed images
// ----------------------------------------------
var
    iFrameType,i : Integer ;
    ROI : TROI ;
begin

    for iFrameType := 0 to NumFrameTypes-1 do if BitMaps[iFrameType] <> Nil then begin

        // Copy image from internal bitmap to image control
        Images[iFrameType].Picture.Assign(BitMaps[iFrameType]) ;

        // Set pen characteristics
        Images[iFrameType].Canvas.Pen.Color := clWhite ;
        Images[iFrameType].Canvas.Brush.Style := bsClear ;
        Images[iFrameType].Canvas.Font.Color := clWhite ;

        // Display frame type at top-left of image
        Images[iFrameType].Canvas.TextOut( 0,0, FrameTypes[iFrameType] ) ;

        // Draw ROI live cursor
        if LiveROINum > 0 then begin
           if MainFrm.IDRFile.ROI[LiveROINum].InUse then begin
              Images[iFrameType].Canvas.Pen.Width := 2 ;
              ScaleROI( MainFrm.IDRFile.ROI[0], ROI ) ;
              DrawROI(ROI,0,Images[iFrameType].Canvas) ;
              end ;
           end ;

        // Display regions of interest
        Images[iFrameType].Canvas.Pen.Width := 1 ;
        for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then begin
            Images[iFrameType].Canvas.Pen.Width := 1 ;
            ScaleROI( MainFrm.IDRFile.ROI[i], ROI ) ;
            DrawROI(ROI,i,Images[iFrameType].Canvas) ;
            end ;

        Images[iFrameType].Invalidate ;

        end ;

    end ;


procedure TViewFrm.DrawROI(
          ROI : TROI ;         // Region of interest to be drawn (IN)
          ROINum : Integer ;   // ROI index number (IN)
          Canvas : TCanvas     // Image canvas to be drawn on (OUT)
          ) ;
// -----------------------------------
// Draw region of interest on to image
// -----------------------------------
const
    CrossHairSize = 6 ;
begin

//     OldColor := Canvas.Pen.Color ;
//     if ROINum = 0 then Canvas.Pen.Color := clWhite
//                   else Canvas.Pen.Color := clWhite ;

     Case ROI.Shape of

          PointROI : begin
             // Draw cross-hairs
             Canvas.PolyLine( [Point(ROI.Centre.X, ROI.Centre.Y + CrossHairSize ),
                               Point(ROI.Centre.X, ROI.Centre.Y - CrossHairSize) ] ) ;
             Canvas.PolyLine( [Point(ROI.Centre.X + CrossHairSize, ROI.Centre.Y),
                               Point(ROI.Centre.X - CrossHairSize, ROI.Centre.Y) ] ) ;
             // Display index number
             if ROINum > 0 then Canvas.TextOut( ROI.Centre.X,
                                                 ROI.Centre.Y + CrossHairSize,
                                                 format('%d',[ROINum])) ;

             end ;

          RectangleROI : begin
             // Draw rectangle
             Canvas.Rectangle( ROI.TopLeft.X,     ROI.TopLeft.Y,
                               ROI.BottomRight.X, ROI.BottomRight.Y ) ;
             // Display index number
             if ROINum > 0 then Canvas.TextOut( ROI.BottomRight.X,
                                                 ROI.BottomRight.Y,
                                                 format('%d',[ROINum])) ;
             end ;

          EllipseROI : begin
             // Draw ellipse
             Canvas.Ellipse( ROI.TopLeft.X,     ROI.TopLeft.Y,
                             ROI.BottomRight.X, ROI.BottomRight.Y ) ;
             // Display index number
             if ROINum > 0 then Canvas.TextOut( ROI.Centre.X,
                                                 ROI.BottomRight.Y,
                                                 format('%d',[ROINum])) ;
             end ;

          LineROI : begin
             // Draw line
             Canvas.Polyline( [ROI.TopLeft,ROI.BottomRight] ) ;

             // Display index number
             if ROINum > 0 then Canvas.TextOut( ROI.BottomRight.X,
                                                 ROI.BottomRight.Y,
                                                 format('%d',[ROINum])) ;
             end ;

          PolylineROI,PolygonROI : begin
             Canvas.Polyline(Slice(ROI.XY, ROI.NumPoints));
             if ROINum > 0 then Canvas.TextOut( ROI.XY[0].X,
                                                ROI.XY[0].Y,
                                                format('%d',[ROINum])) ;
             end ;

          end ;

//     Canvas.Pen.Color := OldColor ;
     // Display ROI index number

     end ;


procedure TViewFrm.ScaleROI(
          SrcROI : TROI ;      // Source ROI (IN)
          var DestROI : TROI   // Destination ROI (OUT)
          ) ;
// --------------------------------------------
// Make scaled copy of region of interest array
// --------------------------------------------
var
    i : Integer ;
begin

         DestROI.InUse := SrcROI.InUse ;
         DestROI.Shape := SrcROI.Shape ;

         DestROI.NumPoints := SrcROI.NumPoints ;
         for i := 0 to SrcROI.NumPoints-1 do begin
             DestROI.XY[i].X := Round((SrcROI.XY[i].X- sbXScroll.Position)*DisplayZoom) ;
             DestROI.XY[i].Y := Round((SrcROI.XY[i].Y- sbYScroll.Position)*DisplayZoom) ;
            end ;

         DestROI.TopLeft.x := Round((SrcROI.TopLeft.x - sbXScroll.Position)*DisplayZoom) ;
         DestROI.TopLeft.y := Round((SrcROI.TopLeft.y - sbYScroll.Position)*DisplayZoom) ;

         DestROI.BottomRight.x := Round((SrcROI.BottomRight.x - sbXScroll.Position)*DisplayZoom) ;
         DestROI.BottomRight.y := Round((SrcROI.BottomRight.y - sbYScroll.Position)*DisplayZoom) ;

         DestROI.Centre.x := Round((SrcROI.Centre.x - sbXScroll.Position)*DisplayZoom) ;
         DestROI.Centre.y := Round((SrcROI.Centre.y - sbYScroll.Position)*DisplayZoom) ;

         DestROI.Width := Round(SrcROI.Width*DisplayZoom) ;
         DestROI.Height := Round(SrcROI.Height*DisplayZoom) ;

         end ;


procedure TViewFrm.UnScaleROI(
          SrcROI : TROI ;      // Source ROI (IN)
          var DestROI : TROI   // Destination ROI (OUT)
          ) ;
// --------------------------------------------
// Make unscaled copy of region of interest array
// --------------------------------------------
var
    i : Integer ;
begin

         DestROI.InUse := SrcROI.InUse ;
         DestROI.Shape := SrcROI.Shape ;

         DestROI.TopLeft.x := Round((SrcROI.TopLeft.x/DisplayZoom) + sbXScroll.Position) ;
         DestROI.TopLeft.y := Round((SrcROI.TopLeft.y/DisplayZoom) + sbYScroll.Position) ;

         DestROI.BottomRight.x := Round((SrcROI.BottomRight.x/DisplayZoom) + sbXScroll.Position) ;
         DestROI.BottomRight.y := Round((SrcROI.BottomRight.y/DisplayZoom) + sbYScroll.Position) ;

         DestROI.Centre.x := Round((SrcROI.Centre.x/DisplayZoom) + sbXScroll.Position) ;
         DestROI.Centre.y := Round( (SrcROI.Centre.y/DisplayZoom) + sbYScroll.Position) ;

         DestROI.Width := Round(SrcROI.Width/DisplayZoom) ;
         DestROI.Height := Round(SrcROI.Height/DisplayZoom) ;

         DestROI.NumPoints := SrcROI.NumPoints ;
         for i := 0 to SrcROI.NumPoints-1 do begin
             DestROI.XY[i].X := Round(SrcROI.XY[i].X/DisplayZoom) + sbXScroll.Position ;
             DestROI.XY[i].Y := Round(SrcROI.XY[i].Y/DisplayZoom) + sbYScroll.Position ;
             end ;


     end ;


procedure TViewFrm.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     MouseDown := True ;

     end;

procedure TViewFrm.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
// ---------------------
// Mouse button released
// ---------------------
const
    Margin = 4 ;
var
    LiveROI : TROI ;
    i,j : Integer ;
    YLine,dX,dY,XLine : Single ;
    XMin,XMax,YMin,YMax : Single ;
    ROIFound : Boolean ;
begin

     MouseDown := False ;

     // Save image cursor position
     ImageCursor.X := Round(x/DisplayZoom) + sbXScroll.Position ;
     ImageCursor.Y := Round(y/DisplayZoom) + sbYScroll.Position ;

     if LiveROIChanged and (LiveROINum >= 0) and (TImage(Sender).Cursor <> crDefault) then begin
        // Resize / relocate selected ROI
        // ------------------------------
        // Save to IDR file
        MainFrm.IDRFile.ROI[LiveROINum] := MainFrm.IDRFile.ROI[0] ;
        // Update regions of interest
        UpdateROIs ;
        // Recalculate time course
        if CheckViewPlotFrmExists then begin
           ViewPlotFrm.NewFLTimeCourseRequired ;
           ViewPlotFrm.UpdateROIList ;
           ViewPlotFrm.DisplayTimeCourse( CurrentPosition ) ;
           end ;
        LiveROIChanged := False ;
        end

     else if ROIAddPolyLineMode then begin

        LiveROI := MainFrm.IDRFile.ROI[0] ;
        LiveROI.XY[LiveROI.NumPoints].X := ImageCursor.X ;
        LiveROI.XY[LiveROI.NumPoints].Y := ImageCursor.Y ;
        Inc(LiveROI.NumPoints) ;

        // Determine bounds of irregular ROI region

        LiveROI.BottomRight.y := -High(LiveROI.BottomRight.y) ;
        LiveROI.BottomRight.y := -High(LiveROI.BottomRight.y) ;
        LiveROI.TopLeft.x := High(LiveROI.BottomRight.x) ;
        LiveROI.TopLeft.y := High(LiveROI.BottomRight.x) ;
        for i := 0 to LiveROI.NumPoints-1 do begin
            LiveROI.BottomRight.x := Max(LiveROI.BottomRight.x,LiveROI.XY[i].X) ;
            LiveROI.BottomRight.y := Max(LiveROI.BottomRight.y,LiveROI.XY[i].Y) ;
            LiveROI.TopLeft.x := Min(LiveROI.TopLeft.x,LiveROI.XY[i].X) ;
            LiveROI.TopLeft.y := Min(LiveROI.TopLeft.y,LiveROI.XY[i].Y) ;
            end ;
        // Computer width/centre
        LiveROI.Width := LiveROI.BottomRight.X - LiveROI.TopLeft.X + 1 ;
        LiveROI.Height := LiveROI.BottomRight.Y - LiveROI.TopLeft.Y + 1 ;
        LiveROI.Centre.X := LiveROI.TopLeft.X + LiveROI.Width div 2 ;
        LiveROI.Centre.Y := LiveROI.TopLeft.Y + LiveROI.Height div 2 ;

        //UnscaleROI( LiveROI, ROI ) ;
        MainFrm.IDRFile.ROI[0] := LiveROI ;
        // Save to IDR file
        MainFrm.IDRFile.ROI[LiveROINum] := MainFrm.IDRFile.ROI[0] ;
        // Update regions of interest
        UpdateROIs ;
        LiveROIChanged := False ;

        end
     else begin

          // Select different ROI
          // --------------------
          for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then begin

              LiveROI := MainFrm.IDRFile.ROI[i] ;
              ROIFound := False ;

              case LiveROI.Shape of

                   PolylineROI,PolygonROI : begin
                      for j:= 1 to LiveROI.NumPoints-1 do begin
                          XMin :=  Min(LiveROI.XY[j].X,LiveROI.XY[j-1].X) ;
                          XMax :=  Max(LiveROI.XY[j].X,LiveROI.XY[j-1].X) ;
                          YMin :=  Min(LiveROI.XY[j].Y,LiveROI.XY[j-1].Y) ;
                          YMax :=  Max(LiveROI.XY[j].Y,LiveROI.XY[j-1].Y) ;
                          dX := LiveROI.XY[j].X - LiveROI.XY[j-1].X ;
                          dY := LiveROI.XY[j].Y - LiveROI.XY[j-1].Y ;
                          if Abs(dX) > Abs(dY) then begin
                             YLine := ((ImageCursor.X - LiveROI.XY[j-1].X)*(dY/dX)) + LiveROI.XY[j-1].Y ;
                             YLine := Min(Max(YLine,Ymin),YMax) ;
                             if (Abs(ImageCursor.Y - YLine) < Margin) and
                                (ImageCursor.X >= XMin) and (ImageCursor.X <= XMax) then ROIFound := True ;
                             end
                          else if Abs(dY) > Abs(dX) then begin
                             XLine := ((ImageCursor.Y - LiveROI.XY[j-1].Y)*(dX/dY)) + LiveROI.XY[j-1].X ;
                             XLine := Min(Max(XLine,Xmin),XMax) ;
                             if (Abs(ImageCursor.X - XLine) < Margin) and
                                (ImageCursor.Y >= YMin) and (ImageCursor.Y <= YMax) then ROIFound := True ;
                             end ;
                         end ;
                      end ;

                   LineROI : begin
                          XMin :=  Min(LiveROI.BottomRight.X,LiveROI.TopLeft.X) ;
                          XMax :=  Max(LiveROI.BottomRight.X,LiveROI.TopLeft.X) ;
                          YMin :=  Min(LiveROI.BottomRight.Y,LiveROI.TopLeft.Y) ;
                          YMax :=  Max(LiveROI.BottomRight.Y,LiveROI.TopLeft.Y) ;
                          dX := LiveROI.BottomRight.X - LiveROI.TopLeft.X ;
                          dY := LiveROI.BottomRight.Y - LiveROI.TopLeft.Y ;
                          if Abs(dX) > Abs(dY) then begin
                             YLine := ((ImageCursor.X - LiveROI.TopLeft.X)*(dY/dX)) + LiveROI.TopLeft.Y ;
                             YLine := Min(Max(YLine,Ymin),YMax) ;
                             if (Abs(ImageCursor.Y - YLine) < Margin) and
                                (ImageCursor.X >= XMin) and (ImageCursor.X <= XMax) then ROIFound := True ;
                             end
                          else if Abs(dY) > Abs(dX) then begin
                             XLine := ((ImageCursor.Y - LiveROI.TopLeft.Y)*(dX/dY)) + LiveROI.TopLeft.X ;
                             XLine := Min(Max(XLine,Xmin),XMax) ;
                             if (Abs(ImageCursor.X - XLine) < Margin) and
                                (ImageCursor.Y >= YMin) and (ImageCursor.Y <= YMax) then ROIFound := True ;
                             end ;

                      end ;

                   else begin
                      // All other ROIs
                     if (Min(LiveROI.TopLeft.Y,LiveROI.BottomRight.y) <= ImageCursor.Y) and
                        (ImageCursor.Y <= Max(LiveROI.TopLeft.Y,LiveROI.BottomRight.y)) and
                        (Min(LiveROI.TopLeft.X,LiveROI.BottomRight.X) <= ImageCursor.X) and
                        (ImageCursor.X <= Max(LiveROI.TopLeft.X,LiveROI.BottomRight.X)) then ROIFound := True ;
                      end ;
                   end ;

              if ROIFound then begin
                 MainFrm.IDRFile.ROI[0] := LiveROI ;
                 LiveROINum := i ;
                 UpdateROIs ;
                 Break ;
                 end ;

              end ;

          end ;

     end;


procedure TViewFrm.AddROI(
          X : Integer ;    // Current mouse X
          Y : Integer      // current mouse Y
          ) ;
// -----------------------------
// Create new region of interest
// -----------------------------
var
     i,iNewROI : Integer ;
     ROI : TROI ;
begin

     // Convert to image coordinates
     X := Round(x/DisplayZoom) + sbXScroll.Position ;
     Y := Round(y/DisplayZoom) + sbYScroll.Position ;

     // Get next free ROI array element
     iNewROI := -1 ;
     for i :=  1 to MainFrm.IDRFile.MaxROI do if not MainFrm.IDRFile.ROI[i].InUse then begin
        iNewROI := i ;
        Break ;
        end ;
     if iNewROI < 0 then Exit ;

     // Initial position of live region of interest
     ROI.Centre.X := X ;
     ROI.Centre.Y := Y ;

     // Initial size of live region of interest
     ROI.Width :=  Round(Images[0].Width/(DisplayZoom*20.0)) ;
     ROI.Height := Round(Images[0].Width/(DisplayZoom*20.0)) ;
      for i :=  1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then begin
          case MainFrm.IDRFile.ROI[i].Shape of
              LineROI,RectangleROI,EllipseROI : begin
                 ROI.Width := MainFrm.IDRFile.ROI[i].Width ;
                 ROI.Height := MainFrm.IDRFile.ROI[i].Height ;
                 end ;
              end ;
          end ;

     ROI.TopLeft.X := ROI.Centre.X - ROI.Width div 2 ;
     ROI.BottomRight.X := ROI.TopLeft.X + ROI.Width - 1 ;
     ROI.TopLeft.Y := ROI.Centre.Y - ROI.Height div 2 ;
     ROI.BottomRight.Y := ROI.TopLeft.Y + ROI.Height - 1 ;

     ROI.NumPoints := 0 ;
     ROI.ZoomFactor := 1.0 ;

     ROI.Shape := SelectedROIType ;

     case ROI.Shape of
        PolylineROI,PolygonROI : begin
          ROI.XY[0].X := X ;
          ROI.XY[0].Y := Y ;
          ROI.NumPoints := 1 ;
          ROIAddPolyLineMode := True ;
          end ;
        else ROIAddPolyLineMode := False ;
        end ;

     // Save to IDR file
     ROI.InUse := True ;
     MainFrm.IDRFile.ROI[0] := ROI ;
     MainFrm.IDRFile.ROI[iNewROI] := MainFrm.IDRFile.ROI[0] ;
     LiveROINum := iNewROI ;

     // Add ROI to deletion list
     UpdateDeleteROIList ;

     // Update regions of interest
     UpdateROIs ;

     if CheckViewPlotFrmExists and (not roiaddPolyLineMode) then begin
        ViewPlotFrm.NewFLTimeCourseRequired ;
        ViewPlotFrm.UpdateROIList ;
        ViewPlotFrm.DisplayTimeCourse( CurrentPosition ) ;
        end ;

     UpdateOtherForms ;

     SelectedROIType := -1 ;

     end;


procedure TViewFrm.edDisplayIntensityRangeKeyPress(Sender: TObject;
  var Key: Char);
// -------------------------------------------
// Set upper/lower limit of display grey scale
// -------------------------------------------
var
    FrameType : Integer ;
begin

     if key <> #13 then Exit ;

     for FrameType := 0 to NumFrameTypes-1 do
         if ckChangeAllFrameTypes.Checked or (FrameType = SelectedFrameType) then begin

        MainFrm.GreyLo[FrameType] := Round(edDisplayIntensityRange.LoValue) ;
        MainFrm.GreyHi[FrameType] := Round(edDisplayIntensityRange.HiValue) ;

        MainFrm.UpdateLUT( FrameType, MainFrm.IDRFile.GreyMax ) ;

        // Set upper/lower limits of time course display
        if CheckViewPlotFrmExists then begin
           ViewPlotFrm.FLYMax[FrameType] := MainFrm.GreyHi[SelectedFrameType] ;
           ViewPlotFrm.FLYMin[FrameType] := 0 ;
           end ;
        end ;

     CurrentPosition := -1 ;

     SetDisplayIntensityRange( MainFrm.GreyLo[SelectedFrameType],
                               MainFrm.GreyHi[SelectedFrameType] ) ;

     end;


procedure TViewFrm.edSubtractFrameKeyPress(Sender: TObject;
  var Key: Char);
// --------------------------------
// Subtraction frame number changed
// --------------------------------
begin

     if key = #13 then begin
        // .. and update of displayed frame
        CurrentPosition := -1 ;
        end
     end;


procedure TViewFrm.bDeleteROIClick(Sender: TObject);
// --------------------------------------
// Delete ROI selected from deletion list
// --------------------------------------
var
     ROINum : Integer ;
     ROI : TROI ;
     i : Integer ;
begin

     if cbDeleteROI.ItemIndex = 0 then begin
        // Delete all ROIs
        if MessageDlg( 'Delete all ROI! Are you Sure? ', mtConfirmation,
           [mbYes,mbNo], 0 ) = mrYes then begin
           for i := 1 to MainFrm.IDRFile.MaxROI do begin
               ROI := MainFrm.IDRFile.ROI[i] ;
               ROI.InUse := False ;
               MainFrm.IDRFile.ROI[i] := ROI ;
               end ;
           end ;
        end
     else begin
        // Delete selected ROI
        ROINum := Integer(cbDeleteROI.Items.Objects[cbDeleteROI.ItemIndex]) ;
        ROI := MainFrm.IDRFile.ROI[ROINum] ;
        ROI.InUse := False ;
        MainFrm.IDRFile.ROI[ROINum] := ROI ;
        end ;

     // Update delete list
     UpdateDeleteROIList ;

     // Update regions of interest
     UpdateROIs ;

     if CheckViewPlotFrmExists then begin
        ViewPlotFrm.NewFLTimeCourseRequired ;
        ViewPlotFrm.UpdateROIList ;
        ViewPlotFrm.DisplayTimeCourse( CurrentPosition ) ;
        end ;

     UpdateOtherForms ;

     end;


procedure TViewFrm.bDeleteAllROIClick(Sender: TObject);
// ----------------------------------
// Delete all currently defined ROIs
// ----------------------------------
var
     i : Integer ;
     ROI : TROI ;
begin

     for i := 1 to MainFrm.IDRFile.MaxROI do begin
        ROI := MainFrm.IDRFile.ROI[i] ;
        ROI.InUse := False ;
        MainFrm.IDRFile.ROI[i] := ROI ;
        end ;

     // Update ROI delete list
     UpdateDeleteROIList ;

     // Update regions of interest
     UpdateROIs ;

     if CheckViewPlotFrmExists then begin
        ViewPlotFrm.NewFLTimeCourseRequired ;
        ViewPlotFrm.UpdateROIList ;
        ViewPlotFrm.DisplayTimeCourse( CurrentPosition ) ;
        end ;

     UpdateOtherForms ;

     end;


procedure TViewFrm.UpdateOtherForms ;
//  ------------------------------------
// Update other forms which use ViewFrm
// ------------------------------------
var
    MDIChild : Integer ;
begin

    for MDIChild := 0 to MainFrm.MDIChildCount-1 do begin
        if MainFrm.MDIChildren[MDIChild].Name = 'TimeCourseFrm' then
           TTimeCourseFrm(MainFrm.MDIChildren[MDIChild]).UpdateSettings ;
        if MainFrm.MDIChildren[MDIChild].Name = 'EventAnalysisFrm' then
           TEventAnalysisFrm(MainFrm.MDIChildren[MDIChild]).UpdateSettings ;
        end ;
    end ;


procedure TViewFrm.cbDisplayZoomChange(Sender: TObject);
// -----------------------------
// New image display zoom factor
// -----------------------------
begin

     MainFrm.DisplayZoomIndex := cbDisplayZoom.ItemIndex ;
     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])*0.001 ;

     // Update image panel sizes
     SetImagePanels ;

     CurrentPosition := -1 ;

     end;


procedure TViewFrm.FormClose(Sender: TObject; var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
begin

     UpdateOtherForms ;

     if MainFrm.FormExists('TimeCourseFrm') then TimeCourseFrm.Close ;
     if MainFrm.FormExists('EventAnalysisFrm') then EventAnalysisFrm.Close ;
     if MainFrm.FormExists('SpectrumFrm') then SpectrumFrm.Close ;
     if MainFrm.FormExists('ViewPlotFrm') then begin
        ViewPlotFrm.AllowClose := True ;
        ViewPlotFrm.Close ;
        end ;

     // Enable menu item associated with this form
     MainFrm.mnViewImages.Enabled := True ;

     MainFrm.DisplayZoomIndex := cbDisplayZoom.ItemIndex ;

     Action := caFree ;

     end;


procedure TViewFrm.FormDestroy(Sender: TObject);
// --------------------------------------------
// Free allocated resources when form destroyed
// --------------------------------------------
var
     i : Integer ;
begin

     for i := 0 to High(BitMaps) do if BitMaps[i] <> Nil then begin
         BitMaps[i].Free  ;
         BitMaps[i] := Nil ;
         end ;

     if bmADCCursor <> Nil then begin
        bmADCCursor.Free ;
        bmADCCursor := Nil ;
        end ;

     for i := 0 to High(pImageBufs) do if pImageBufs[i] <> Nil then begin
         FreeMem(pImageBufs[i]) ;
         pImageBufs[i] := Nil ;
         end ;

     for i := 0 to High(pImageBufs) do if pImageBufs[i] <> Nil then begin
         FreeMem(pImageBufs[i]) ;
         pImageBufs[i] := Nil ;
         end ;

     if FrameList <> Nil then begin
        FreeMem(FrameList) ;
        FrameList := Nil ;
        end ;

     end;


procedure TViewFrm.LoadROIsFromFile(
          FileName : String
          ) ;
// ---------------------------------------
// Load regions of interest from .ROI file
// ---------------------------------------
var
    FileHandle : THandle ;
    i : Integer ;
    ROIs : Array[0..cMaxROIs] of TROI ;         // Regions of interest list (scaled by zoom)
    InF : TextFile ;
    s : ANSIstring ;
begin

    // Open file
    FileHandle := FileOpen( FileName, fmOpenRead ) ;
    if FileHandle = INVALID_HANDLE_VALUE then begin
       MainFrm.StatusBar.SimpleText := ' Unable to load : ' + FileName ;
       Exit ;
       end ;

    if FileSeek( FileHandle, 0, 2 ) = SizeOf(ROIs) then begin
       // If this binary format ROI file (file size same as ROI array) then load direct
       FileSeek( FileHandle, 0, 0 ) ;
       FileRead( FileHandle, ROIs, SizeOf(ROIs)) ;
       FileClose( FileHandle ) ;
       end
    else begin
       // Load from ASCII text format ROI file
       FileClose( FileHandle ) ;
       AssignFile( InF, FileName ) ;
       Reset( InF ) ;
       for i := 0 to High(ROIs) do ROIs[i].InUse := False ;
       i := 0 ;
       while not EOF(InF) do begin
           ReadLn( InF, s ) ;
           ROIs[i].InUse := True ;
           ROIs[i].Shape := GetInt(s) ;
           ROIs[i].Centre.X := GetInt(s) ;
           ROIs[i].Centre.Y := GetInt(s) ;
           ROIs[i].Width := GetInt(s) ;
           ROIs[i].Height := GetInt(s) ;
           ROIs[i].TopLeft.X := ROIs[i].Centre.X - ROIs[i].Width div 2 ;
           ROIs[i].TopLeft.Y := ROIs[i].Centre.Y - ROIs[i].Height div 2 ;
           ROIs[i].BottomRight.X := ROIs[i].TopLeft.X + ROIs[i].Width - 1 ;
           ROIs[i].BottomRight.Y := ROIs[i].TopLeft.Y + ROIs[i].Height - 1 ;
           ROIs[i].ZoomFactor := 1 ;

           ROIs[i].NumPoints := 0 ;
           while Length(s) > 0 do begin
               ROIs[i].XY[ROIs[i].NumPoints].X := GetInt(s) ;
               ROIs[i].XY[ROIs[i].NumPoints].Y := GetInt(s) ;
               Inc(ROIs[i].NumPoints) ;
               end;
           Inc(i) ;
           end;
       CloseFile( InF ) ;
       end;

    // Disable any ROIs which lie outside current frame
    for i := 1 to MainFrm.IDRFile.MaxROI do if ROIs[i].InUse then begin
        if (ROIs[i].TopLeft.x >= MainFrm.IDRFile.FrameWidth) or
           (ROIs[i].TopLeft.y >= MainFrm.IDRFile.FrameHeight) or
           (ROIs[i].BottomRight.x >= MainFrm.IDRFile.FrameWidth) or
           (ROIs[i].BottomRight.y >= MainFrm.IDRFile.FrameHeight) then
           ROIs[i].InUse := False ;
        end ;

    // Load internal ROI records from master records
    // scaled by display zoom factor
    for i := 1 to MainFrm.IDRFile.MaxROI do begin
        MainFrm.IDRFile.ROI[i] := ROIs[i] ;
        end ;

    // Update ROI delete list
    UpdateDeleteROIList ;

    // Force display update
    CurrentPosition :=  -1 ;

    LogFrm.AddLine( 'ROIs loaded from ' + FileName ) ;

    end ;

function TViewFrm.GetInt( var s : ANSIstring ) : Integer ;
// -------------------------------------------------------
// Extract and return a tab-delimited integer value from s
// -------------------------------------------------------
var
    sNum : ANSIstring ;
    i,iNum, iErr : Integer ;
begin

    sNum := '' ;
    i := 1 ;
    while (i <= Length(s)) and (s[i] <> #9) do begin
        sNum := sNum + s[i] ;
        Inc(i) ;
        end ;
    s := ANSIRightStr(s,Max(Length(s)-i,0)) ;
    if Length(sNum) > 0 then begin
       Val( sNum, iNum, iErr ) ;
       Result := iNum ;
       end
    else Result := 0 ;
    end;


procedure TViewFrm.SaveROIsToFile(
          FileName : String
          ) ;
// ---------------------------------------
// Save regions of interest from .ROI file
// ---------------------------------------
var
     i,j : Integer ;
     OutF : TextFile ;
     s : ANSIString ;
begin

    AssignFile( OutF, FileName ) ;
    ReWrite(OutF) ;

    for i := 0 to MainFrm.IDRFile.MaxROI do begin
        if MainFrm.IDRFile.ROI[i].InUse then begin
           s := format('%d',[MainFrm.IDRFile.ROI[i].Shape]) ;
           s := s + #9 + format('%d',[MainFrm.IDRFile.ROI[i].Centre.X]) ;
           s := s + #9 + format('%d',[MainFrm.IDRFile.ROI[i].Centre.Y]) ;
           s := s + #9 + format('%d',[MainFrm.IDRFile.ROI[i].Width]) ;
           s := s + #9 + format('%d',[MainFrm.IDRFile.ROI[i].Height]) ;
           for j := 0 to MainFrm.IDRFile.ROI[i].NumPoints-1 do begin
               s := s + #9 + format('%d',[MainFrm.IDRFile.ROI[i].XY[j].X]);
               s := s + #9 + format('%d',[MainFrm.IDRFile.ROI[i].XY[j].Y]);
               end ;
           WriteLn( OutF, s ) ;
           end;
        end ;

     CloseFile( OutF ) ;

     LogFrm.AddLine( 'ROIs saved to ' + FileName ) ;

     end ;


procedure TViewFrm.bLoadROisClick(Sender: TObject);
// ----------------------------------------
// Load region of interests definition file
// ----------------------------------------
begin

     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := ROIFileExtension ;

     if MainFrm.DataDirectory <> '' then
        OpenDialog.InitialDir := MainFrm.DataDirectory ;

     OpenDialog.Filter := format(' %s Files (*.%s)|*.%s',
                          [ROIFileExtension,ROIFileExtension,ROIFileExtension]);
     OpenDialog.Title := 'Load ROIs from File' ;

     if not OpenDialog.Execute then Exit ;

     LoadROIsFromFile( OpenDialog.FileName ) ;

     if CheckViewPlotFrmExists then begin
        ViewPlotFrm.NewFLTimeCourseRequired ;
        ViewPlotFrm.UpdateROIList ;
        ViewPlotFrm.DisplayTimeCourse( CurrentPosition ) ;
        end ;

     UpdateDeleteROIList ;

     end;


procedure TViewFrm.bSaveROIsClick(Sender: TObject);
// ----------------------------------------
// Save region of interests definition file
// ----------------------------------------
begin

     //SaveDialog.options := [ofPathMustExist] ;
     SaveDialog.DefaultExt := ROIFileExtension ;

     if MainFrm.DataDirectory <> '' then
        SaveDialog.InitialDir := MainFrm.DataDirectory ;

     SaveDialog.Filter := format(' %s Files (*.%s)|*.%s',
                          [ROIFileExtension,ROIFileExtension,ROIFileExtension]);
     SaveDialog.Title := 'Save ROIs to File ' ;
     if SaveDialog.Execute then SaveROIsToFile( SaveDialog.FileName ) ;

     end;


procedure TViewFrm.cbPaletteChange(Sender: TObject);
// ------------------------------
// Display colour palette changed
// ------------------------------
var
     i : Integer ;
begin
     MainFrm.PaletteType := TPaletteType(cbPalette.Items.Objects[cbPalette.ItemIndex]) ;
     for i := 0 to NumFrameTypes-1 do if BitMaps[i] <> Nil then begin
         MainFrm.SetPalette( BitMaps[i], MainFrm.PaletteType ) ;
         end ;
     CurrentPosition := -1 ;
     end;


procedure TViewFrm.ckDisplayCalBarClick(Sender: TObject);
// -----------------------------------------
// Display calibration bar check box changed
// -----------------------------------------
begin
     // Force a display update
     CurrentPosition := -1 ;
     end;


procedure TViewFrm.Image1Click(Sender: TObject);
// -------------------------------------------
// Image clicked - changed frame type selected
// -------------------------------------------
var
    ROI : TROI ;
    i : Integer ;
begin

     if LiveROIChanged then Exit ;

     // Select frame type
     SelectedFrameType := TImage(Sender).Tag ;
     DisplayGrp.Caption := 'Contrast ' + FrameTypes[SelectedFrameType] + ' ' ;
     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[SelectedFrameType]  ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[SelectedFrameType]  ;
     ImageSelected := True ;

     Exit ;

     // Select ROI
     for i := 1 to cMaxROIs do if MainFrm.IDRFile.ROI[i].InUse then begin
        // If mouse is over ROI, sl
        if (Min(ROI.TopLeft.Y,ROI.BottomRight.y) <= ImageCursor.Y) and
           (ImageCursor.Y <= Max(ROI.TopLeft.Y,ROI.BottomRight.y)) and
           (Min(ROI.TopLeft.X,ROI.BottomRight.X) <= ImageCursor.X) and
           (ImageCursor.X <= Max(ROI.TopLeft.X,ROI.BottomRight.X)) then begin
           LiveROINum := i ;
           MainFrm.IDRFile.ROI[0] := MainFrm.IDRFile.ROI[LiveROINum] ;
           UpdateROIs ;
           Break ;
           end ;
        end ;

     end;


procedure TViewFrm.CopyImageToClipboard ;
{ -------------------------------------------
  Copy image to clipboard as Windows metafile
  ------------------------------------------- }
begin

    if BitMaps[SelectedFrameType] = Nil then Exit ;

    // Copy bitmap image
    BitMaps[SelectedFrameType].SaveToClipboardFormat( ClipboardImageFormat,
                                                      ClipboardImageData,
                                                      ClipboardPalette ) ;
    Clipboard.SetAsHandle( ClipboardImageFormat,
                             ClipboardImageData ) ;

    end ;




procedure TViewFrm.PrintImage ;
{ ------------------
  Print image
  ------------------ }
const
     Margin = 250 ;
var
     SRect : TRect ;
     Scale : Single ;
begin

      if BitMaps[SelectedFrameType] = Nil then Exit ;

      // Print image
      Printer.BeginDoc ;
      Printer.Canvas.TextOut(Margin,Margin,MainFrm.IDRFile.FileName) ;

      Scale := (Printer.Canvas.ClipRect.Right
               - Printer.Canvas.ClipRect.Left - 2*Margin) /
               BitMaps[SelectedFrameType].Width ;

      SRect.Left := Margin ;
      SRect.Right := Round(BitMaps[SelectedFrameType].Width*Scale) + Margin ;
      SRect.Top := Margin + 100 ;
      SRect.Bottom := SRect.Top + Round(BitMaps[SelectedFrameType].Height*Scale) ;
      Printer.Canvas.StretchDraw(SRect,BitMaps[SelectedFrameType]);
      Printer.EndDoc ;

      end ;



procedure TViewFrm.edIdentKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
// ------------------------------------------
// Update ident field when ident text changed
// ------------------------------------------
begin
     MainFrm.IDRFile.Ident := edIdent.Text ;
     end;


procedure TViewFrm.AddMarker(
          MarkerAtFrame : Integer ;
          MarkerText : String
          ) ;
// ------------------
// Add marker to file
// ------------------
var
     MarkerTime : Single ;
begin

     // If marker text is blank, add marker number
     if MarkerText = '' then MarkerText := format('%d',[MainFrm.IDRFile.NumMarkers+1]) ;

     MarkerTime := MainFrm.IDRFile.FrameInterval*MarkerAtFrame ;

     // Plot marker on chart
     MainFrm.IDRFile.AddMarker( MarkerTime, MarkerText ) ;

     //scDisplay.AddMarker( DisplayPointer div scDisplay.NumChannels, MarkerText );

     LogFrm.AddLine( format('Marker (off-line) at %.4gs %s',[MarkerTime, MarkerText]));

     end;




procedure TViewFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
// -----------------------------------
// Check to see if form can be closed
// -----------------------------------
begin

 {    if ViewPlotFrm.ComputeFLTimeCourseRunning then begin
        ViewPlotFrm.StopComputeFLTimeCourseRunning := True ;
        CanClose := False ;
        end
     else CanClose := True ;
     FormClosing := True ;}

     end;


procedure TViewFrm.bMarkClick(Sender: TObject);
// ------------------------------
//  Add a text marker to the chart
// ------------------------------
begin

     AddMarker( CurrentPosition, edMarker.Text ) ;

     CurrentPosition := -1 ;

     end ;


procedure TViewFrm.bLUTIncreaseRangeClick(Sender: TObject);
// --------------------------
// Increase range of LUT table
// --------------------------
var
    FrameType : Integer ;
begin

     for FrameType := 0 to NumFrameTypes-1 do
         if ckChangeAllFrameTypes.Checked or (FrameType = SelectedFrameType) then begin

         edDisplayIntensityRange.LoValue := 0.95*MainFrm.GreyLo[FrameType] ;
         MainFrm.GreyLo[FrameType] := Round(edDisplayIntensityRange.LoValue) ;
         edDisplayIntensityRange.HiValue := 1.05*MainFrm.GreyHi[FrameType] ;
         MainFrm.GreyHi[FrameType] := Round(edDisplayIntensityRange.HiValue) ;

         MainFrm.UpdateLUT( FrameType, MainFrm.IDRFile.GreyMax ) ;

         // Set upper/lower limits of time course display
         if CheckViewPlotFrmExists then begin
            ViewPlotFrm.FLYMax[FrameType] := MainFrm.GreyHi[FrameType] ;
            ViewPlotFrm.FLYMin[FrameType] := 0 ;
            end ;

         end ;

     // Force image update
     CurrentPosition := -1 ;
     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[SelectedFrameType]  ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[SelectedFrameType]  ;

     end ;


procedure TViewFrm.bLUTDecreaseRangeClick(Sender: TObject);
// ----------------------------------------------
// Decrease range of LUT table (increase contrast
// ----------------------------------------------
var
    FrameType : Integer ;
begin

     for FrameType := 0 to NumFrameTypes-1 do
         if ckChangeAllFrameTypes.Checked or (FrameType = SelectedFrameType) then begin

         edDisplayIntensityRange.LoValue := 1.05*MainFrm.GreyLo[FrameType] ;
        if edDisplayIntensityRange.LoValue < edDisplayIntensityRange.HiValue then begin
           MainFrm.GreyLo[FrameType] := Round(edDisplayIntensityRange.LoValue) ;
           edDisplayIntensityRange.HiValue := 0.95*MainFrm.GreyHi[FrameType] ;
           MainFrm.GreyHi[FrameType] := Round(edDisplayIntensityRange.HiValue) ;

           MainFrm.UpdateLUT( FrameType, MainFrm.IDRFile.GreyMax ) ;
           end ;

        // Set upper/lower limits of time course display
        if CheckViewPlotFrmExists then begin
           ViewPlotFrm.FLYMax[FrameType] := MainFrm.GreyHi[FrameType] ;
           ViewPlotFrm.FLYMin[FrameType] := 0 ;
           end ;

        end ;

     // Force image update
     CurrentPosition := -1 ;
     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[SelectedFrameType]  ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[SelectedFrameType]  ;

     end ;


procedure TViewFrm.bMaxContrastClick(Sender: TObject);
// ------------------------------------------------------
// Set display look-up table to range of levels in image
// ------------------------------------------------------
var
   FT : Integer ;
begin

     for FT := 0 to NumFrameTypes -1 do
         if ckChangeAllFrameTypes.Checked or OptimiseContrastNeeded or
           (FT = SelectedFrameType) then begin
           CalculateMaxContrast( FT ) ;

           // Set upper/lower limits of time course display
           if CheckViewPlotFrmExists then begin
              ViewPlotFrm.FLYMax[FT] := MainFrm.GreyHi[FT] ;
              ViewPlotFrm.FLYMin[FT] := 0 ;
              end ;

           end ;

     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[SelectedFrameType]  ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[SelectedFrameType]  ;

     OptimiseContrastNeeded := False ;

     // Force image update
     CurrentPosition := -1 ;

     end;


procedure TViewFrm.sbContrastChange(Sender: TObject);
// --------------------------------------------------------
// Set display grey scale to new contrast slider setting
// --------------------------------------------------------
var
    FT : Integer ;
begin

     if ContrastPage.ActivePage <> SlidersTab then Exit ;

     for FT := 0 to NumFrameTypes-1 do
         if ckChangeAllFrameTypes.Checked or (FT = SelectedFrameType) then begin

         edDisplayIntensityRange.LoValue := sbBrightness.Position -
                                            (sbContrast.Position div 2) ;
         edDisplayIntensityRange.HiValue := sbBrightness.Position +
                                            (sbContrast.Position div 2) ;

         if edDisplayIntensityRange.LoValue = edDisplayIntensityRange.HiValue then begin
            edDisplayIntensityRange.LoValue := edDisplayIntensityRange.LoValue - 1.0 ;
            edDisplayIntensityRange.HiValue := edDisplayIntensityRange.HiValue + 1.0 ;
            end ;

         MainFrm.GreyLo[FT] := Round(edDisplayIntensityRange.LoValue) ;
         MainFrm.GreyHi[FT] := Round(edDisplayIntensityRange.HiValue) ;

         MainFrm.UpdateLUT( FT, Mainfrm.IDRFile.GreyMax ) ;

         // Set upper/lower limits of time course display
         ViewPlotFrm.FLYMax[FT] := MainFrm.GreyHi[FT] ;
         ViewPlotFrm.FLYMin[FT] := 0 ;

         end ;

     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[SelectedFrameType]  ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[SelectedFrameType]  ;
     CurrentPosition := -1 ;

     end;


procedure TViewFrm.sbXScrollChange(Sender: TObject);
// ----------------------------------
// Image left edge scroll bar changed
// ----------------------------------
begin
     CurrentPosition := -1 ;
     end;

procedure TViewFrm.sbYScrollChange(Sender: TObject);
// ----------------------------------
// Image top edge scroll bar changed
// ----------------------------------
begin
     CurrentPosition := -1 ;
     end;

procedure TViewFrm.SetFrameNumber( Value : Integer ) ;
// ----------------
// Set frame number
// ----------------
begin
    sbFrameNum.Position := Min(Max(Value,sbFrameNum.Min),sbFrameNum.Max) ;
    end ;


function TViewFrm.GetFrameNumber : Integer ;
// ------------------------
// Set current frame number
// ------------------------
begin
     Result := CurrentPosition ;
     end ;
     
procedure TViewFrm.bEditROIsClick(Sender: TObject);
// -------------------------
// Display ROI editing table
// -------------------------
begin
     EditROIFrm.Left := MainFrm.Left + 20 ;
     EditROIFrm.Top := MainFrm.Top + 20 ;

     EditROIFrm.Show ;

     end;

procedure TViewFrm.bStopMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
// -------------------------------
// Stop movie
// -------------------------------
begin
     // NOTE. MouseDown used because Click does not respond during movie playing
     bForwards.Enabled := True ;
     bBackwards.Enabled := True ;
     end;

procedure TViewFrm.bForwardsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// -------------------------------
// Play movie in forward direction
// -------------------------------
begin
     bForwards.Enabled := False ;
     bBackwards.Enabled := True ;
     end;

procedure TViewFrm.bBackwardsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// -------------------------------
// Play movie in backward direction
// -------------------------------
begin
     bBackwards.Enabled := False ;
     bForwards.Enabled := True ;
     end;

procedure TViewFrm.bGoToStartMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// -----------------------------------
// Move display to first frame in file
// -----------------------------------
begin
     edFrameNum.LoValue := 1 ;
     sbFrameNum.Position := 1 ;
     edFrameNum.HiValue :=  sbFrameNum.Max ;
     bForwards.Enabled := True ;
     bBackwards.Enabled := True ;
     end;


procedure TViewFrm.bGoToEndMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
// -----------------------------------
// Move display to last frame in file
// -----------------------------------
begin
     sbFrameNum.Position := sbFrameNum.Max ;
     edFrameNum.LoValue  := sbFrameNum.Position ;
     edFrameNum.HiValue :=  sbFrameNum.Max ;
     bForwards.Enabled := True ;
     bBackwards.Enabled := True ;
     end;

procedure TViewFrm.ckBackgroundSubtractionClick(Sender: TObject);
// ---------------------------------
// Enable/disable shading correction
// ---------------------------------
begin
     CurrentPosition := -1 ;
     end;

procedure TViewFrm.ROIPointMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
// ------------------------------------
// Mouse button depressed over ROI icon
// ------------------------------------
begin

     TIMage(Sender).BeginDrag( False ) ;
     SelectedROIType := TIMage(Sender).Tag ;

     end;

procedure TViewFrm.Image1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
{ -----------------------------------------------------------------
  If the dragged object is compatible with this icon
  change the cursor to acceptable
  ----------------------------------------------------------------}
begin
     if (Source is TBitmap) then accept := True ;
     
     end;



procedure TViewFrm.Image1DragDrop(Sender, Source: TObject; X, Y: Integer);
// ---------------------------------------------
// End of drag operation - icon has been dropped
// ---------------------------------------------
begin
      if SelectedROIType >= 0 then AddROI( X, Y ) ;
      end;

procedure TViewFrm.Image1DblClick(Sender: TObject);
// -----------------------
// Double-click over image
// -----------------------
var
    ROI : TROI ;
begin

  // Double-clicking terminates addition of polyline and polygon ROIs
  // (If a polygon is being added, close bounding line
  if ROIAddPolyLineMode and (MainFrm.IDRFile.ROI[0].Shape = PolygonROI) then begin
     ROI := MainFrm.IDRFile.ROI[0] ;
     ROI.InUse := True ;
     if ROI.NumPoints <= High(ROI.XY) then begin
        ROI.XY[ROI.NumPoints] := ROI.XY[0];
        ROI.NumPoints := ROI.NumPoints + 1 ;
        MainFrm.IDRFile.ROI[0] := ROI ;
        MainFrm.IDRFile.ROI[LiveROINum] := ROI ;
        end ;

     // Recalculate time course
     if CheckViewPlotFrmExists then begin
        ViewPlotFrm.NewFLTimeCourseRequired ;
        ViewPlotFrm.UpdateROIList ;
        ViewPlotFrm.DisplayTimeCourse( CurrentPosition ) ;
        end ;

     end ;

  ROIAddPolyLineMode := False ;
  LiveROINum := -1 ;

  end;


procedure TViewFrm.ckAutoOptimiseClick(Sender: TObject);
// --------------------------------------------------
// 6 standard deviation contrast optimisation changed
// --------------------------------------------------
begin
    MainFrm.ContrastAutoOptimise := ckAutoOptimise.Checked ;
    end;

procedure TViewFrm.ControlsGrpClick(Sender: TObject);
// -----------------------------------
// Change contrast for all frame types
// -----------------------------------
Begin
    MainFrm.ContrastChangeAllFrameTypes := ckChangeAllFrameTypes.Checked ;
    end ;

procedure TViewFrm.ckContrast6SDOnlyClick(Sender: TObject);
// --------------------------------------------------
// 6 standard deviation contrast optimisation changed
// --------------------------------------------------
begin
    MainFrm.Contrast6SD := ckContrast6SDOnly.Checked ;
    end;

end.


