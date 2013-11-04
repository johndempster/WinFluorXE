unit SnapUnit;
// =============================================================================                                
// WinFluor - Windows Fluoresence Program - Live image display module
// (c) J. Dempster, University of Strathclyde, 2001-8, All Rights Reserved
// =============================================================================
// 13.01.08
// 07.08.08 Set BINFACTOR removed from zoom changes
// 30.04.08 Display no longer over-magnifies at 100% zoom and above settings
// 12.05.08 Lights sources controlled by digital lines now controllable
// 20.05.08 Word() added to LUT index in X0.5 and X0.25 display to avoid
//                 memory violation
// 08.09.08 Shading correction function added
// 17.09.08 Light wavelength now stored in INI file
// 22.09.08 Memory violation when changing zoom fixed (PBackgroundBuf)
// 18.03.09 JD Frame acquisition now only checked in one place
// 15.04.09 JD Double frame display glitch fixed
//          Image area selection no longer progressively shrinks image
// 07.09.09 JD Camera restarted when MainFrm.Cam1.CameraRestartRequired set
// 26.01.10 JD Auto contrast adjustment option added
// 05.02.10 JD Auto contrast now applied at 1s intervals
//             Contrast settings stored in INI
// 27.07.10 JD No. of frames in buffer increased for DCAM and IMAQ
// 03.09.10 JD PentaMax NumFramesInBuffer limited to max of 36
//             (same as recordfrm) to avoid system hanging up.
// 02.02.11 JD Thickness of calibration bar can now be set by user
// 18.07.12 JD Empty flags placed at beginning and end of each frame
//             to handle cameras that update end of frame first (intended to fix
//             intermittent blank frames produced by QImaging Bolt, but not checked yet)
//             QCAM NumFramesInBuffer now defined by MainFrm.Cam1.MaxFramesInBuffer (increased to 64)
// 26.11.12 JD Zoom combo box moved to top of image area

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, RangeEdit, ComCtrls, ValidatedEdit, IDRFile,
  math, SESCam, mmsystem, ImageFile, clipbrd, Buttons ;

type
  TSnapFrm = class(TForm)
    ControlGrp: TGroupBox;
    ImageCaptureGrp: TGroupBox;
    lbReadoutTime: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    edFrameInterval: TValidatedEdit;
    cbCameraGain: TComboBox;
    DisplayGrp: TGroupBox;
    cbPalette: TComboBox;
    ContrastPage: TPageControl;
    RangeTab: TTabSheet;
    bFullScale: TButton;
    bMaxContrast: TButton;
    edDisplayIntensityRange: TRangeEdit;
    ckContrast6SDOnly: TCheckBox;
    SlidersTab: TTabSheet;
    Label5: TLabel;
    Label10: TLabel;
    sbContrast: TScrollBar;
    sbBrightness: TScrollBar;
    CCDAreaGrp: TGroupBox;
    Label4: TLabel;
    bFullFrame: TButton;
    bSelectedRegion: TButton;
    edBinFactor: TValidatedEdit;
    bEnterCCDArea: TButton;
    ExcitationLightGrp: TGroupBox;
    GroupBox6: TGroupBox;
    rbEXCShutterOpen: TRadioButton;
    rbEXCShutterClosed: TRadioButton;
    cbWavelength: TComboBox;
    Label1: TLabel;
    ImageGrp: TGroupBox;
    Image1: TImage;
    sbXScroll: TScrollBar;
    sbYScroll: TScrollBar;
    ROIPanel: TPanel;
    ckDisplayCalBar: TCheckBox;
    Timer: TTimer;
    SnapGrp: TGroupBox;
    bSnapImage: TButton;
    lbReadout: TLabel;
    ImageFile: TImageFile;
    SaveDialog: TSaveDialog;
    Label11: TLabel;
    Label13: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    ShadingGrp: TGroupBox;
    ckBackgroundSubtraction: TCheckBox;
    bAcquireBackground: TButton;
    ShadeCorSettingsPanel: TPanel;
    edShadeCorImageBlockSize: TValidatedEdit;
    Label2: TLabel;
    Label7: TLabel;
    edShadeCorNumFramesAveraged: TValidatedEdit;
    Splitter1: TSplitter;
    Label9: TLabel;
    cbShadeCorNormalisation: TComboBox;
    Label15: TLabel;
    sbShadeCorShowSettings: TSpeedButton;
    bSetLaserIntensity: TButton;
    ckAutoOptimise: TCheckBox;
    Label6: TLabel;
    cbDisplayZoom: TComboBox;
    ZStageGrp: TGroupBox;
    edZPosition: TValidatedEdit;
    sbZPosition: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edFrameIntervalKeyPress(Sender: TObject; var Key: Char);
    procedure bFullScaleClick(Sender: TObject);
    procedure bMaxContrastClick(Sender: TObject);
    procedure edDisplayIntensityRangeKeyPress(Sender: TObject;
      var Key: Char);
    procedure cbDisplayZoomChange(Sender: TObject);
    procedure rbEXCShutterOpenClick(Sender: TObject);
    procedure bFullFrameClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bSelectedRegionClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edBinFactorKeyPress(Sender: TObject; var Key: Char);
    procedure cbPaletteChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cbCameraGainChange(Sender: TObject);
    procedure sbContrastChange(Sender: TObject);
    procedure bEnterCCDAreaClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure bSnapImageClick(Sender: TObject);
    procedure rbEXCShutterClosedClick(Sender: TObject);
    procedure cbWavelengthChange(Sender: TObject);
    procedure bAcquireBackgroundClick(Sender: TObject);
    procedure ckBackgroundSubtractionClick(Sender: TObject);
    procedure lbShadeCorShowSettingsClick(Sender: TObject);
    procedure sbShadeCorShowSettingsClick(Sender: TObject);
    procedure bSetLaserIntensityClick(Sender: TObject);
    procedure ckAutoOptimiseClick(Sender: TObject);
    procedure ckContrast6SDOnlyClick(Sender: TObject);
    procedure sbZPositionChange(Sender: TObject);
    procedure edZPositionKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }

    ii : Integer ;

    BitMap : TBitMap ;  // Image internal bitmaps

    // Image capture double buffer
    FirstFrameInLowerBuffer : Integer ;  // Start of lower half of buffer
    LastFrameInLowerBuffer : Integer ;   // End of lower half of buffer
    FirstFrameInUpperBuffer : Integer ;  // Start of upper half of buffer
    LastFrameInUpperBuffer : Integer ;   // End of upper half of buffer
    NumBytesPerHalfFrameBuffer : Integer ; // No. of bytes of half of buffer
    NumBytesPerFrame : Integer ;           // No. of bytes per image frame
    LowerBufferFilling : Boolean ;         // TRUE = Lower half of buffer filling
    LastFrameDisplayed : Integer ;            // Last frame # displayed

    NumFramesTotal : Integer ;             // Total no. of frames since start of file

    TimerProcBusy : Boolean ;              // TRUE = scheduled timer code executing
    InitialisationComplete : Boolean ;     // TRUE = formshow initialisations done
    FormClosing : Boolean ;                // TRUE = form is closing
    OptimiseContrastNeeded : Boolean ;     // TRUE = display contrast needs to be updated

    FrameTypeToBeDisplayed : Integer ;    // Next type of frame to be displayed

    DisplayZoom : Single ;                // Display zoom factor (0.5,1.0.2.0)
    XResolution : Single ;                // Image pixel width
    ResolutionUnits : String ;            // Image pixel width units

    FirstResize : Boolean ;
    FormResizeCounter : Integer ;

    MousePosition : TPoint ;

    TStart : Cardinal ;
    FrameRateCounter : Integer ;
    LastFrameNum : Integer ;

    CaptureRegion : TRect ;
    FXOld : Integer ;
    FYOld : Integer ;
    FMoveCaptureRegion : Boolean ;

    SnapNum : Integer ;

    ShadeCorNumFramesAveraged : Integer ; // No. of background frames averaged

    OptimiseContrastAtFrameNum : Integer ;  // Next frame # when contrast is to be optimised
    OptimiseContrastInterval : Integer ;    // Contrast optimisation interval (frames)

    // Clipboard image data

    ClipboardImageFormat : Word ;
    ClipboardImageData: THandle ;
    ClipboardPalette : HPalette ;

    procedure FillBufferWithEmptyFlags( StartAt : Integer ; EndAt : Integer ) ;
    procedure SetDisplayIntensityRange(
              LoValue : Integer ;
              HiValue : Integer
              ) ;

    procedure DisplayImage(
          StartAt : Integer ;          // Index to first pixel of frame in circular buffer [In]
          PCurrentFrame : PIntArray ;  // Display buffer
          var LUT : Array of Word ;    // Display look-up table [IN]
          BitMap : TBitMap ;           // Bit map to hold image (OUT)
          Image : TImage               // Image display component (OUT)
          ) ;

    procedure UpdateImage(
              PCurrentFrame : PIntArray ;  // Display buffer
              var LUT : Array of Word ;    // Display look-up table [IN]
              BitMap : TBitMap ;           // Bit map to hold image (OUT)
              Image : TImage               // Image display component (OUT)
              ) ;

    procedure CalculateMaxContrast ;
    procedure DisplayCalibrationBar(
              BitMap : TBitMap ) ; // Bit map to be written to

    procedure UpdateLightSource ;
    procedure UpdateLightSourceDAC ;
    procedure UpdateLightSourceDIG ;
    procedure UpdateLightSourceShutter ;

    procedure ReadCursor(
          FrameNum : Integer ;
          pBuf : Pointer ) ;

    procedure SmoothImage(
          PBuf : PIntArray ;  // Imagebuffer
          FrameWidth : Integer ; // Width of frame
          FrameHeight : Integer ; // Height of frame
          BlockSize : Integer    // Smoothing block size
          ) ;

    procedure ShowHideShadeCorSettingsPanel ;
    procedure DoShadingCorrection ;

    procedure DrawCaptureRegion(
            Canvas : TCanvas ;
            SelectedRect : TRect
            ) ;
    procedure DrawSquare(
            Canvas : TCanvas ;
            X : Integer ;
            Y : Integer ) ;


  public
    { Public declarations }
    { Public declarations }

    CameraRunning : Boolean ;              // TRUE = Camera is acquiring images

    FrameWidth : Integer ;                    // Frame width in use
    FrameHeight : Integer ;                   // Frame height in use
    PFrameBuf : Pointer ;                     // Pointer to circular image capture buffer
    PDisplayBuf : PIntArray ; // Pointer to displayed image buffers
    PBackGroundBuf : PIntArray ; // Background image buffer
    PSumBuf : PSingleArray ;      // Summation buffer
    FrameCounter : Integer ;  // Frames acquired counter
    PWorkBuf : Pointer ;                      // Pointer to work buffer
    SelectedFrameType : Integer ;                        // Frame selected by user
    NumFramesInBuffer : Integer ;             // No. of frames in circular capture buffer
    NumPixelsPerFrame : Integer ;             // No. of pixels in image frame
    NumBytesPerPixel : Integer ;              // No. of bytes per image pixel
    ByteImage : Boolean ;                     // TRUE = 1 byte/pixel image

    ImageLabel : string ;
    NumFrameTypes : Integer ;                        // No. of frame types in use
    ImageAvailable : Boolean ;                       // Image is available in display buffer

    ttest : Integer ;

    procedure StartCamera ;
    procedure StopCamera ;
    procedure RestartCamera ;
    procedure InitialiseImage ;
    procedure SetImagePanels ;
    procedure StopLiveImaging ;
    procedure CopyImageToClipboard ;

  end;

var
  SnapFrm: TSnapFrm;

implementation

uses Main, maths , SetCCDReadoutUnit, LightSourceUnit, LabIOUnit, RecUnit,
  LogUnit, SetLasersUnit, ZStageUnit;

{$R *.dfm}

const
    ByteLoValue = 0 ;
    ByteHiValue = 255 ;
    WordLoValue = 0 ;
    WordHiValue = $FFFF ;
    //EmptyFlag = 32767 ;
    NormaliseToMean = 0 ;
    NormaliseToMin = 1 ;
    NormaliseToMax = 2 ;
    NormaliseToZero = 3 ;

type
    TMoveMode = (mvNone,mvLeftEdge,mvRightEdge,mvTopEdge,mvBottomEdge,
                 mvTopLeft,mvBottomLeft,mvTopRight,mvBottomRight,mvAll) ;

var
    MoveMode : TMoveMode ;


procedure TSnapFrm.FormCreate(Sender: TObject);
// ------------------------------------
// Initialisations when form is created
// ------------------------------------
begin

     ii := 0 ;
     CameraRunning := False ;

     // Displayed image storage buffer pointers
     PDisplayBuf := Nil ;
     PBackgroundBuf := Nil ;
     PSumBuf := Nil ;
     PWorkBuf := Nil ;

     // Internal image bitmaps
     BitMap := Nil ;

     Timer.Enabled := False ;

     ttest := timegettime + 3000 ;

     end;


procedure TSnapFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
begin

     FirstResize := True ;
     FormResizeCounter := 1 ;
     InitialisationComplete := False ;
     FormClosing := False ;
     if not MainFrm.Cam1.CameraAvailable then begin
        Close ;
        Exit ;
        end ;

     // Set form at top left of MDI window
     Top := 20 ;
     Left := 20 ;

     // Inter-frame capture interval
     MainFrm.Cam1.ShortenExposureBy := 0.0 ;
     edFrameInterval.Value := MainFrm.Cam1.FrameInterval ;
     lbReadoutTime.Caption := format('Min.= %.3g ms',
                                      [SecsToMs*MainFrm.Cam1.ReadoutTime]) ;

     // Get list of available camera gains
     MainFrm.Cam1.GetCameraGainList( cbCameraGain.Items ) ;
     cbCameraGain.ItemIndex := Min(Max(MainFrm.Cam1.AmpGain,0),
                                       cbCameraGain.Items.Count-1) ;

     bSelectedRegion.Enabled := MainFrm.Cam1.CCDRegionReadoutAvailable ;

     // Set limits of display intensity range
     SelectedFrameType := 0 ;
     edDisplayIntensityRange.LoLimit := 0 ;
     edDisplayIntensityRange.HiLimit := MainFrm.Cam1.GreyLevelMax ;

     // Set brightness & contrast slider range and position
     sbContrast.Min := 0 ;
     sbContrast.Max := MainFrm.Cam1.GreyLevelMax ;
     sbContrast.SmallChange := 1 ;
     sbContrast.LargeChange := Max(sbContrast.Max div 50,1) ;
     sbBrightness.Min := 0 ;
     sbBrightness.Max := MainFrm.Cam1.GreyLevelMax ;
     sbBrightness.LargeChange := Max(sbBrightness.Max div 50,1) ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[SelectedFrameType],
                               MainFrm.GreyHi[SelectedFrameType] ) ;

    // Number of excitation wavelengths in use
    // (Each displayed as a separate image)
    NumFrameTypes := 1 ;

     // Intensity display palette
     cbPalette.Clear ;
     cbPalette.Items.AddObject(' Grey scale', TObject(palGrey)) ;
     cbPalette.Items.AddObject(' False colour', TObject(palFalseColor)) ;
     cbPalette.Items.AddObject(' Red scale', TObject(palRed)) ;
     cbPalette.Items.AddObject(' Green scale', TObject(palGreen)) ;
     cbPalette.Items.AddObject(' Blue scale', TObject(palBlue)) ;
     cbPalette.ItemIndex := cbPalette.Items.IndexOfObject(TObject(MainFrm.PaletteType)) ;

     // Display magnification factor
     cbDisplayZoom.Clear ;
     cbDisplayZoom.Items.AddObject( '  25% ', Tobject(25) ) ;
     cbDisplayZoom.Items.AddObject( '  50% ', Tobject(50) ) ;
     cbDisplayZoom.Items.AddObject( ' 100% ', Tobject(100)) ;
     cbDisplayZoom.Items.AddObject( ' 200% ', Tobject(200)) ;
     cbDisplayZoom.Items.AddObject( ' 300% ', Tobject(300)) ;
     cbDisplayZoom.Items.AddObject( ' 400% ', Tobject(400)) ;
     cbDisplayZoom.Items.AddObject( ' 500% ', Tobject(500)) ;
     cbDisplayZoom.Items.AddObject( ' 600% ', Tobject(600)) ;
     cbDisplayZoom.Items.AddObject( ' 700% ', Tobject(700)) ;
     cbDisplayZoom.Items.AddObject( ' 800% ', Tobject(800)) ;

     cbDisplayZoom.ItemIndex := Min(Max(MainFrm.DisplayZoomIndex,
                                0),cbDisplayZoom.Items.Count-1) ;
     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])*0.01 ;

     sbXScroll.Position := 0 ;
     sbYScroll.Position := 0 ;

     // Create list of excitation wavelengths
     cbWaveLength.Clear ;
     for i := 0 to High(MainFrm.EXCWavelengths) do begin
         cbWaveLength.Items.Add( format('%d: %d (%d)',
         [i,
          MainFrm.EXCWavelengths[i].Centre,
          MainFrm.EXCWavelengths[i].Width] )) ;
         end ;
     cbWaveLength.ItemIndex := Min(Max(MainFrm.LiveWindowWavelength,0),
                               cbWaveLength.Items.Count-1);

     // Excitation light shutter state closed
     rbEXCShutterOpen.Checked := False ;

     MainFrm.Recording := False ;
     TimerProcBusy := False ;
     ImageAvailable := False ;
     MoveMode := mvNone ;
     OptimiseContrastNeeded := True ;
     ImageLabel := '' ;

     // Save image file dialog
     SaveDialog.InitialDir := MainFrm.DataDirectory ;
     SaveDialog.Title := 'Save Image ' ;
     SaveDialog.options := [ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.DefaultExt := '.tif' ;
     SaveDialog.Filter := ' BioRad PIC (*.pic)|*.pic|' +
                          ' MetaMorph STK (*.stk)|*.stk|' +
                          ' TIFF (*.tif)|*.tif' ;
     SaveDialog.FilterIndex := 3 ;
     SnapNum := 1 ;

     MainFrm.StatusBar.SimpleText := ' Camera initialised' ;

     // Shading correction
     ckBackgroundSubtraction.Checked := False ;
     sbShadeCorShowSettings.Down := False ;
     ShowHideShadeCorSettingsPanel ;

     cbShadeCorNormalisation.Clear ;
     cbShadeCorNormalisation.Items.Add('Mean') ;
     cbShadeCorNormalisation.Items.Add('Min.') ;
     cbShadeCorNormalisation.Items.Add('Max.') ;
     cbShadeCorNormalisation.Items.Add('Zero') ;
     cbShadeCorNormalisation.ItemIndex := 0 ;

     // Display contrast optimisation
     OptimiseContrastNeeded := True ;
     ckAutoOptimise.Checked := MainFrm.ContrastAutoOptimise ;
     ckContrast6SDOnly.Checked := MainFrm.Contrast6SD ;

     InitialisationComplete := True ;

     ClientHeight := ControlGrp.Top + ControlGrp.Height + 5 ;

     // Start schedule events timer (runs at 50 ms intervals)
     Timer.Enabled := True ;

     end;


procedure TSnapFrm.StartCamera ;
// -------------------
// Start image capture
// -------------------
begin

   // Don't start if called before initialisations in FormShow complete
   if not InitialisationComplete then Exit ;
   if FormClosing then Exit ;

   // Stop camera (if it is running)
   StopCamera ;

   Timer.Enabled := False ;

   MainFrm.StatusBar.SimpleText := 'Wait ... Starting camera' ;

//   outputdebugString(PChar(format('camera started %d',[Numframesdone]))) ;
   CameraRunning := False ;

   // Set camera trigger mode
   MainFrm.Cam1.TriggerMode := CamFreeRun ;

   // Set camera gain
   MainFrm.Cam1.AmpGain := cbCameraGain.ItemIndex ;

   // Get current spatial resolution
   XResolution := MainFrm.Cam1.PixelWidth ;
   ResolutionUnits := MainFrm.Cam1.PixelUnits ;

   // Set exposure interval
   MainFrm.Cam1.FrameInterval := edFrameInterval.Value ;

   // Set image/display panels
   MainFrm.StatusBar.SimpleText := 'Wait ... Initialising image' ;
   InitialiseImage ;

   // Start frame capture
   MainFrm.StatusBar.SimpleText := 'Wait ... Starting camera' ;
   MainFrm.Cam1.StartCapture ;


   // Update exposure interval in case camera has changed it
   edFrameInterval.Value := MainFrm.Cam1.FrameInterval ;

   TStart := TimeGetTime ;
   FrameRateCounter := 0 ;
   LastFrameNum := 0 ;
   OptimiseContrastAtFrameNum := 0 ;
   OptimiseContrastInterval := Round( 1.0 / MainFrm.Cam1.FrameInterval ) ;

   Timer.Enabled := True ;

   // Report minimum readout time
   lbReadoutTime.Caption := format('Min.= %.3g ms',
                            [SecsToMs*MainFrm.Cam1.ReadoutTime]) ;

   // Initialise frames counter
   FrameCounter := 0 ;

   MainFrm.StatusBar.SimpleText := 'Camera started ' ;

   CameraRunning := True ;

   UpdateLightSource ;
   UpdateLightSourceShutter ;

   end ;


procedure TSnapFrm.StopCamera ;
// -------------------
// Stop image capture
// -------------------
begin

     // Exit if camera not running
     if not CameraRunning then Exit ;

     // Stop camera
     MainFrm.Cam1.StopCapture ;
     CameraRunning := False ;
     Timer.Enabled := False ;

     MainFrm.StatusBar.SimpleText := ' Camera stopped ' ;

     CameraRunning := False ;
     Timer.Enabled := True ;
     ImageAvailable := False ;

     end ;


procedure TSnapFrm.RestartCamera ;

var
    i,OldIndex : Integer ;
begin

    StopCamera ;

    // Update excitation wavelength list
    OldIndex := cbWaveLength.ItemIndex ;
    cbWaveLength.Clear ;
    for i := 0 to High(MainFrm.EXCWavelengths) do begin
        cbWaveLength.Items.Add( format('%d: %d (%d)',
           [i+1,
            MainFrm.EXCWavelengths[i].Centre,
            MainFrm.EXCWavelengths[i].Width] )) ;
           end ;
    cbWaveLength.ItemIndex := OldIndex ;

    StartCamera ;

    end ;


procedure TSnapFrm.InitialiseImage ;
// ------------------------------------------------------
// Re-initialise size of memory buffers and image bitmaps
// ------------------------------------------------------
var
     i : Integer ;
     MaxBuffers : Integer ;
begin

    // No. of pixels per frame
    FrameWidth := MainFrm.Cam1.FrameWidth ;
    FrameHeight := MainFrm.Cam1.FrameHeight ;
    NumPixelsPerFrame := FrameWidth*FrameHeight ;

    // Number of excitation wavelengths in use
    NumFrameTypes := 1 ;

     // Dispose of existing display buffers and create new ones
     if PDisplayBuf <> Nil then begin
         Try
           FreeMem(PDisplayBuf) ;
           PDisplayBuf := Nil ;
         except
           outputdebugString(PChar('Error FreeMem(PDisplayBufs[i]')) ;
           PDisplayBuf := Nil ;
           end ;
         end ;
     GetMem( PDisplayBuf,NumPixelsPerFrame*SizeOf(Integer) ) ;

     // Create background buffer
     if PBackgroundBuf <> Nil then begin
         Try
           FreeMem(PBackgroundBuf) ;
           PBackgroundBuf := Nil ;
         except
           outputdebugString(PChar('Error FreeMem(PBackgroundBuf[i]')) ;
           PBackgroundBuf := Nil ;
           end ;
         end ;
     GetMem( PBackgroundBuf,NumPixelsPerFrame*SizeOf(Integer) ) ;

     // Create summation buffer
     if PSumBuf <> Nil then begin
         Try
           FreeMem(PSumBuf) ;
           PSumBuf := Nil ;
         except
           outputdebugString(PChar('Error FreeMem(PSumBuf[i]')) ;
           PSumBuf := Nil ;
           end ;
         end ;
     GetMem( PSumBuf,NumPixelsPerFrame*SizeOf(Single) ) ;

     ckBackgroundSubtraction.Checked := False ;
     bAcquireBackground.Enabled := True ;
     for i := 0 to NumPixelsPerFrame-1 do begin
         PBackgroundBuf^[i] := 0 ;
         PSumBuf^[i] := 0.0 ;
         end ;

     // Create work buffer

     try
     if PWorkBuf <> Nil then FreeMem( PWorkBuf ) ;
     except
           outputdebugString(PChar('Error FreeMem( PWorkBuf )')) ;
           PWorkBuf := Nil ;
           end ;
     GetMem( PWorkBuf, NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel ) ;
     // Set size and location of image display panels
     SetImagePanels ;

     CCDAreaGrp.Caption := format( ' CCD Area (%d x %d) ',
                         [MainFrm.Cam1.FrameWidth,MainFrm.Cam1.FrameHeight] ) ;

     // Indicate selected frame type selected for contrast update
     DisplayGrp.Caption := ' Contrast ' ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[0],MainFrm.GreyHi[0] ) ;

     // Update display look up tables
     MainFrm.UpdateLUT( 0, MainFrm.Cam1.GreyLevelMax );

     // Determine number of frame within circular buffer

     case MainFrm.CameraType of
        RS_PVCAM_PENTAMAX : Begin
          // Pentamax has limited buffer size
          NumFramesInBuffer :=  (4194304 div
                                        (NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel))-1 ;
          NumFramesInBuffer := Min( NumFramesInBuffer, 36) ;
          end ;

        PIXELFLY : begin
          NumFramesInBuffer := 8 ;
          end ;

        Andor : begin
           NumFramesInBuffer :=  (20000000 div
                                        (NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel))-1 ;
           if NumFramesInBuffer > 36 then NumFramesInBuffer := 36 ;
           end ;

        AndorSDK3 : begin
           NumFramesInBuffer :=  (20000000 div
                                        (NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel))-1 ;
           if NumFramesInBuffer > 36 then NumFramesInBuffer := 36 ;
           NumFramesInBuffer := 16 ;
           NumFramesInBuffer := Min( (Round(2.0/edFrameInterval.Value) div 2)*2,
                                      (200000000 div (NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel))-1) ;

           end ;

        RS_PVCAM : begin
           NumFramesInBuffer :=  (20000000 div
                                        (NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel))-1 ;
           end ;

        DCAM : begin
           NumFramesInBuffer :=  (40000000 div
                                        (NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel))-1 ;
           end ;

        IMAQ : begin
           NumFramesInBuffer :=  (20000000 div
                                        (NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel))-1 ;
           end ;

        IMAQDX : begin
           NumFramesInBuffer := MainFrm.Cam1.MaxFramesInBuffer ;
           end ;

        DTOL : begin
           NumFramesInBuffer := MainFrm.Cam1.MaxFramesInBuffer ;
           end ;

        QCAM : begin
           NumFramesInBuffer := MainFrm.Cam1.MaxFramesInBuffer ;
           end ;

        else begin
           NumFramesInBuffer := 32 ;
           end ;
        end ;

     MaxBuffers := NumFramesInBuffer ;
     NumFramesInBuffer := Max(MaxBuffers div 2,1) ;
     NumFramesInBuffer := Max((NumFramesInBuffer div NumFrameTypes),1)*NumFrameTypes*2 ;
     if NumFramesInBuffer > MaxBuffers then NumFramesInBuffer := NumFramesInBuffer div 2 ;
     //NumFramesInBuffer := Min( NumFramesInBuffer, 36) ;
     MainFrm.Cam1.NumFramesInBuffer := NumFramesInBuffer ;
     NumFramesInBuffer := MainFrm.Cam1.NumFramesInBuffer ;

     FirstFrameInLowerBuffer := 0 ;
     LastFrameInLowerBuffer := (NumFramesInBuffer div 2) - 1 ;
     FirstFrameInUpperBuffer := LastFrameInLowerBuffer + 1 ;
     LastFrameInUpperBuffer := NumFramesInBuffer  - 1 ;

     NumPixelsPerFrame := MainFrm.Cam1.FrameWidth*MainFrm.Cam1.FrameHeight ;
     NumBytesPerFrame := NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel ;
     NumBytesPerHalfFrameBuffer := (NumFramesInBuffer div 2)*MainFrm.Cam1.NumBytesPerPixel*NumPixelsPerFrame ;

     MainFrm.Cam1.GetFrameBufferPointer( PFrameBuf ) ;

     // Set byte/word image flag
     NumBytesPerPixel := MainFrm.Cam1.NumBytesPerPixel ;
     if MainFrm.Cam1.NumBytesPerPixel = 1 then ByteImage := True
                                          else ByteImage := False ;

     // Add empty flag value to end of each frame
     FillBufferWithEmptyFlags( 0, NumFramesInBuffer-1 ) ;
     LowerBufferFilling := True ;

     // Initial setting of last frame displayed
     LastFrameDisplayed := NumFramesInBuffer - 1 ;
     // Type of frame to be displayed
     FrameTypeToBeDisplayed := 0 ;

     edBinFactor.Value := MainFrm.Cam1.BinFactor ;

     end ;


procedure TSnapFrm.SetImagePanels ;
// -------------------------------------------
// Set size and number of image display panels
// -------------------------------------------
const
    MarginPixels = 16 ;
var
    ImageAreaHeight : Integer ;
    ImageAreaWidth : Integer ;
    ImageColumns : Integer ;
    ImageRows : Integer ;
    RightEdge : Integer ;
    BottomEdge : Integer ;
begin

     // Dispose of existing bit maps
     if BitMap <> Nil then begin
         BitMap.Free  ;
         BitMap := Nil ;
         end ;
     BitMap := TBitMap.Create ;

     // Set size and pen/brush characteristics of images in use
     cbDisplayZoom.ItemIndex := Min(Max(cbDisplayZoom.ItemIndex,0),cbDisplayZoom.Items.Count-1) ;
     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])*0.01 ;

     // Determine number of image columns and rows
     ImageRows := 1 ;
     ImageColumns := 1 ;

     ImageGrp.ClientWidth :=  Max( ClientWidth - ImageGrp.Left - 5, 2) ;
     ImageGrp.ClientHeight :=  Max( ClientHeight - ImageGrp.Top - 5, 2) ;
     ControlGrp.ClientHeight := ImageGrp.ClientHeight ;

     ROIPanel.Top := ImageGrp.ClientHeight - ROIPanel.Height - 2 ;
     ROIPanel.Width := ImageGrp.ClientWidth - ROIPanel.Left - 5 ;

     ImageAreaWidth := Max( ImageGrp.ClientWidth - sbYScroll.Width - (2*MarginPixels),2) ;
     ImageAreaHeight := Max( ROIPanel.Top - sbXScroll.Height - cbDisplayZoom.Height - cbDisplayZoom.Top - (2*MarginPixels),2) ;

     BitMap.Width := Max(Min( ImageAreaWidth ,
                              Round(MainFrm.Cam1.FrameWidth*DisplayZoom)),2) ;
     BitMap.Height := Max(Min( ImageAreaHeight,
                               Round(MainFrm.Cam1.FrameHeight*DisplayZoom)),2) ;

     RightEdge := 0 ;
     BottomEdge := 0 ;

     MainFrm.SetPalette( BitMap, MainFrm.PaletteType ) ;

     Image1.Width := BitMap.Width ;
     Image1.Height := BitMap.Height ;

     Image1.Left := MarginPixels ;
     Image1.Top := cbDisplayZoom.Top + cbDisplayZoom.Height + 2  ;

     Image1.Canvas.Pen.Color := clWhite ;
     Image1.Canvas.Brush.Style := bsClear ;
     Image1.Canvas.Font.Color := clWhite ;
     Image1.Canvas.TextFlags := 0 ;
     Image1.Canvas.Pen.Mode := pmXOR ;
     Image1.Canvas.Font.Name := 'Arial' ;
     Image1.Canvas.Font.Size := 8 ;
     Image1.Canvas.Font.Color := clBlue ;

     // Determine right/bottom edge of image area
     RightEdge := Image1.Left + Image1.Width + 1 ;
     BottomEdge := Image1.Top + Image1.Height + 1 ;

     // Position image scroll bars at right and bottom edges of image area
     sbXScroll.Top := BottomEdge ;
     sbXScroll.Left :=  Image1.Left ;
     sbXScroll.Width := RightEdge - sbXScroll.Left ;
     ROIPanel.Top := sbXScroll.Top + sbXScroll.Height + 2 ;

     sbYScroll.Left := RightEdge ;
     sbYScroll.Top := Image1.Top ;
     sbYScroll.Height := BottomEdge - sbYScroll.Top ;

     // Image scroll bar range
     sbXScroll.Max := Max(MainFrm.Cam1.FrameWidth - Round(Image1.Width/DisplayZoom),1);
     sbYScroll.Max := Max(MainFrm.Cam1.FrameHeight - Round(Image1.Height/DisplayZoom),1);

     CaptureRegion.Left := 0 ;
     CaptureRegion.Right := BitMap.Width - 1 ;
     CaptureRegion.Top := 0 ;
     CaptureRegion.Bottom := BitMap.Height - 1 ;
     FMoveCaptureRegion := False ;

     end ;


procedure TSnapFrm.SetDisplayIntensityRange(
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


procedure TSnapFrm.FillBufferWithEmptyFlags(
          StartAt : Integer ;
          EndAt : Integer ) ;
// ----------------------------------------------
// Add empty flags to end of each frame in buffer
// ----------------------------------------------
var
    i,iFlag : Integer ;
begin

     if PFrameBuf = Nil then Exit ;

     for i := StartAt to EndAt do begin
         iFlag := i*NumPixelsPerFrame ;
         if ByteImage then begin
            PByteArray(PFrameBuf)^[iFlag] := ByteLoValue ;
            PByteArray(PFrameBuf)^[iFlag+1] := ByteHiValue ;
            PByteArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-2] := ByteLoValue ;
            PByteArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-1] := ByteHiValue ;
            end
         else begin
            PWordArray(PFrameBuf)^[iFlag] := WordLoValue ;
            PWordArray(PFrameBuf)^[iFlag+1] := WordHiValue ;
            PWordArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-2] := WordLoValue ;
            PWordArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-1] := WordHiValue ;
            end ;
         end ;

     end ;


procedure TSnapFrm.TimerTimer(Sender: TObject);
// ---------------------------------------------------
// Scheduled timer event supervising recording/display
// ---------------------------------------------------
var
     FrameNum,FrameCount,
     NextFrameToDisplay : Integer ;  // Next frame to be displayed
     iFlag : Integer ;               // Empty flag pixel offset within frame
     FirstFrame,LastFrame : Integer ;
     Done,BufferFull : Boolean ;
     LatestFrame : Pointer ;
     FrameRate : Single ;
     FrameType : Integer ;
     BufferOverFlowMessage : String ;
     RecordingStatus : String ;

begin

    if PFrameBuf = Nil then Exit ;

    if TimerProcBusy then Exit ;

    TimerProcBusy := True ;
    LatestFrame := Nil ;

    // Restart camera if a change to camera settings elsewhere requires is
    if MainFrm.Cam1.CameraRestartRequired then begin
       StopCamera ;
       StartCamera ;
       end ;

 {   if timegettime > ttest then begin
        StopCamera ;
        MainFrm.Cam1.SetCCDArea( MainFrm.Cam1.FrameLeft,
                                 MainFrm.Cam1.FrameTop,
                                 Max(MainFrm.Cam1.FrameRight-10,10),
                                 Max(MainFrm.Cam1.FrameBottom-10,10));
        StartCamera ;
        ttest := timegettime + 5000 ;
        end ;}

    // Resize controls (if required)
    if FormResizeCounter > 1 then Dec(FormResizeCounter) ;
    if FormResizeCounter = 1 then begin
       SetImagePanels ;
       ShowHideShadeCorSettingsPanel ;
       FormResizeCounter := 0 ;
       end ;

    if {MainFrm.Cam1.CameraActive}True then begin

        // Initiate transfer from frame grabber
        // (for boards which lack host DMA transfer)
        MainFrm.Cam1.ReadCamera ;

        // Find latest frame that has been acquired
        // ----------------------------------------

        FrameNum := LastFrameDisplayed + 1 ;

        NextFrameToDisplay := -1 ;
        FrameCount := 0 ;
        BufferFull := False ;
        Done := False ;
        while not Done do begin

           // Keep frame within buffer
           if FrameNum >= NumFramesInBuffer then FrameNum := 0 ;

           // Set pointer to flag pixel
           iFlag := FrameNum*NumPixelsPerFrame ;

           // Type of frame
           FrameType := 0 ;

           // If image available for this frame, get frame#
           if ByteImage then begin
              // 8 bit pixel frames
              if (PByteArray(PFrameBuf)^[iFlag+1] = ByteHiValue) and
                 (PByteArray(PFrameBuf)^[iFlag] = ByteLoValue) then Done := True
              else if (PByteArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-1] = ByteHiValue) and
                      (PByteArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-2] = ByteLoValue) then Done := True
              else begin
                  NextFrameToDisplay := FrameNum ;
                  ReadCursor( FrameNum, pFrameBuf ) ;
                  Inc(NumFramesTotal) ;
                  end ;
              end
           else begin
              // 16 bit pixel frames
              if (PWordArray(PFrameBuf)^[iFlag+1] = WordHiValue) and
                 (PWordArray(PFrameBuf)^[iFlag] = WordLoValue) then Done := True
              else if (PWordArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-1] = WordHiValue) and
                      (PWordArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-2] = WordLoValue) then Done := True
              else begin
                  NextFrameToDisplay := FrameNum ;
                  ReadCursor( FrameNum, pFrameBuf ) ;
                  Inc(NumFramesTotal) ;
                  end ;
              end ;

           // Is half-buffer full ?
           if not Done then begin
              BufferFull := False ;
              if LowerBufferFilling then begin
                 if FrameNum >= LastFrameInLowerBuffer then BufferFull := True ;
                 end
              else begin
                 if FrameNum < FirstFrameInUpperBuffer then BufferFull := True ;
                 end ;

              // Increment frame pointer
              Inc(FrameNum) ;
              if FrameNum >= NumFramesInBuffer then FrameNum := 0 ;

              // Emergency exit when buffer overflow occurs
              Inc(FrameCount) ;
              if FrameCount >= NumFramesInBuffer then begin
                 Done := True ;
                 end ;
              end ;


           end ;

        // Display latest frame
        // --------------------

        if NextFrameToDisplay >= 0 then begin

           if FrameNum > LastFrameNum then begin
              FrameRateCounter :=FrameRateCounter + FrameNum - LastFrameNum ;
              end
           else if FrameNum < LastFrameNum then begin
              FrameRateCounter := FrameRateCounter
                                + FrameNum - LastFrameNum + NumFramesInBuffer ;
              end ;

           // Calculate and display frame acquisition rate
           FrameRate := FrameRateCounter/((TimeGetTime-TStart)*0.001) ;
           LastFrameNum := FrameNum ;

           BufferOverFlowMessage := '' ;
           RecordingStatus := format( ' %8.2f fps F=%3.3d/%3.3d (%3.3d%%) Temp=%5.0f C ',
                                 [FrameRate,
                                  FrameNum+1,
                                  NumFramesInBuffer,
                                  Round(Ceil(FrameRate*edFrameInterval.value*100.0)),
                                  MainFrm.Cam1.CameraTemperature
                                  ]) ;

           MainFrm.StatusBar.SimpleText := RecordingStatus + BufferOverFlowMessage ;

           if not CameraRunning then begin
              MainFrm.StatusBar.SimpleText := ' Camera Initialised' ;
              end ;

           // Display frame
           DisplayImage( NextFrameToDisplay*NumPixelsPerFrame,
                         PDisplayBuf,
                         MainFrm.LUTs,
                         BitMap,
                         Image1 ) ;

           // Apply shading correction
           DoShadingCorrection ;

           LastFrameDisplayed := NextFrameToDisplay ;

           end ;

        // Restore empty frame flags / write to file
        // -----------------------------------------

        // If half-buffer is full ...
        // update empty flags and write to disk if necessary
        //
        If BufferFull then begin

           // Get position of frame empty flag at end of half-buffer
           if LowerBufferFilling then begin
              FirstFrame := FirstFrameInLowerBuffer ;
              LastFrame := LastFrameInLowerBuffer ;
              end
           else begin
              FirstFrame := FirstFrameInUpperBuffer ;
              LastFrame := LastFrameInUpperBuffer ;
              end ;

           // Reset empty frame flags
           FillBufferWithEmptyFlags( FirstFrame, LastFrame ) ;

           // Toggle buffer in use flag
           LowerBufferFilling := not LowerBufferFilling ;

           end ;

        // Optimise contrast if required

        if ckAutoOptimise.Checked then begin
           if FrameRateCounter >= OptimiseContrastAtFrameNum then OptimiseContrastNeeded := True ;
           end ;

        if OptimiseContrastNeeded and (FrameRateCounter > NumFrameTypes) then begin
           bMaxContrast.Click ;
           OptimiseContrastAtFrameNum := FrameRateCounter + OptimiseContrastInterval ;
           end ;

        end ;

     TimerProcBusy := False ;

     end;


procedure TSnapFrm.ReadCursor(
          FrameNum : Integer ;
          pBuf : Pointer ) ;
var
     xPix,yPix, iReadout, IPix : Integer ;
begin

    // Set pointer to cursor readout pixel
    xPix := Round(MousePosition.X/DisplayZoom) + sbXScroll.Position ;
    yPix := MainFrm.Cam1.FrameHeight -
           Round(MousePosition.Y/DisplayZoom) - sbYScroll.Position ;

    // Read pixel intensity
    iReadout :=  (FrameNum*NumPixelsPerFrame) +
                 (Round(MousePosition.Y/DisplayZoom) + sbYScroll.Position)*MainFrm.Cam1.FrameWidth
                 + Round(MousePosition.X/DisplayZoom) + sbXScroll.Position ;


    if iReadout >= (MainFrm.Cam1.NumFramesInBuffer*MainFrm.Cam1.FrameWidth
                    *MainFrm.Cam1.FrameHeight) then Exit ;

    if ByteImage then IPix := PByteArray(PBuf)^[iReadout]
                 else IPix := PWordArray(PBuf)^[iReadout] ;

    lbReadout.Caption := format(' X= %.4g um (%d), Y=%.4g um (%d), I=%d',
                         [xPix*MainFrm.Cam1.PixelWidth,xPix,
                          yPix*MainFrm.Cam1.PixelWidth,yPix,IPix]) ;

    end ;


procedure TSnapFrm.DisplayImage(
          StartAt : Integer ;          // Index to first pixel of frame in circular buffer [In]
          PCurrentFrame : PIntArray ;  // Display buffer
          var LUT : Array of Word ;    // Display look-up table [IN]
          BitMap : TBitMap ;           // Bit map to hold image (OUT)
          Image : TImage               // Image display component (OUT)
          ) ;
// --------------
// Display image
// --------------
const
    ROICrossSize = 8 ;
var
    Ybm,Yim,Xbm,Xim,i,j,StartOfLine : Integer ;
    iStep : Integer ;
    iDisplayZoom,iz : Integer ;
    iEnd : Integer ;
    PScanLine1 : PByteArray ;    // Bitmap line buffer pointer
    PScanLine : PByteArray ;    // Bitmap line buffer pointer
begin

    if PFrameBuf = Nil then Exit ;

    // Copy image from circular buffer into 32 bit display buffer

    j := StartAt ;
    if ByteImage then begin
       // 8 bit images
       for i := 0 to NumPixelsPerFrame-1 do begin
           PCurrentFrame^[i] := PByteArray(PFrameBuf)^[j] ;
           Inc(j) ;
           end ;
       end
    else begin
       // 16 bits images
       for i := 0 to NumPixelsPerFrame-1 do begin
           PCurrentFrame^[i] := PWordArray(PFrameBuf)^[j] ;
           Inc(j) ;
           end ;
       end ;

    if ckBackgroundSubtraction.Checked and bAcquireBackground.Enabled then begin
       for i := 0 to NumPixelsPerFrame-1 do begin
           PCurrentFrame^[i] := PCurrentFrame^[i] - PBackgroundBuf^[i] ;
           end ;
       end ;

    // Copy image to display bitmap

    if DisplayZoom >= 1.0 then begin
       // ------------------------------
       // X1 and above display zoom factors
       // ------------------------------
       Ybm := 0 ;
       Yim := 0 ;
       StartOfLine := (sbYScroll.Position*MainFrm.Cam1.FrameWidth)
                      + sbXScroll.Position ;
       for Yim := sbYScroll.Position to MainFrm.Cam1.FrameHeight-1 do begin

           // Create line
           PScanLine := BitMap.ScanLine[Ybm] ;
           iDisplayZoom := Round(DisplayZoom) ;
           iz := 0 ;
           i := StartOfLine ;
           iEnd := Min( StartOfLine + MainFrm.Cam1.FrameWidth -1, NumPixelsPerFrame ) ;
           Xbm := 0 ;
           while (i < iEnd) and (Xbm < BitMap.Width) do begin
              PScanLine[Xbm] := LUT[Word(PCurrentFrame^[i])] ;
              Inc(Xbm) ;
              Inc(iz) ;
              if iz >= iDisplayZoom then begin
                 inc(i) ;
                 iz := 0 ;
                 end
              end ;

           // Create additional lines
           for i := 1 to Round(DisplayZoom)-1 do begin
               Inc(Ybm) ;
               if Ybm >= Bitmap.Height then break ;
               PScanLine1 := BitMap.ScanLine[Ybm] ;
               for Xbm := 0 to Bitmap.Width-1 do PScanLine1[Xbm] := PScanLine[Xbm] ;
               end ;

           StartOfLine := StartOfLine + MainFrm.Cam1.FrameWidth ;

           Inc(Ybm) ;
           if Ybm >= Bitmap.Height then break ;

           end ;

       end
    else begin
       // ------------------------
       // X0.25 & 0.5 display zoom factor
       // ------------------------
       Ybm := 0 ;
       Yim := sbYScroll.Position ;
       iStep := Round(1.0/DisplayZoom) ;
       while (Ybm < BitMap.Height) and (Yim < MainFrm.Cam1.FrameHeight) do begin

          // Get scan line array pointer
          PScanLine := BitMap.ScanLine[Ybm] ;

          // Copy line to bitmap
          xBm := 0 ;
          XIm := sbXScroll.Position ;
          i := (Yim*MainFrm.Cam1.FrameWidth) + XIm*iStep ;
          while (Xbm < BitMap.Width) and
                (Xim < MainFrm.Cam1.FrameWidth) and
                (i < NumPixelsPerFrame) do begin
             PScanLine[Xbm] := LUT[Word(PCurrentFrame^[i])] ;
             Inc(Xbm) ;
             Xim := Xim + iStep ;
             i := i + iStep ;
             end ;

          Inc(Ybm) ;
          Yim := Yim + iStep

          end ;

       end ;

    // Add calibration bar
    if ckDisplayCalBar.Checked then DisplayCalibrationBar( BitMap ) ;

    Image.Picture.Assign(BitMap) ;

    // Draw rectangle round selected capture region
    DrawCaptureRegion( Image.Canvas, CaptureRegion ) ;

    ImageAvailable := True ;

    // Display frame type at top-left of image
    Image.Canvas.TextOut( 0,0,ImageLabel ) ;

    // Increment frames acquired counter
    Inc(FrameCounter) ;

    end ;


procedure TSnapFrm.DrawCaptureRegion(
          Canvas : TCanvas ;
          SelectedRect : TRect
          ) ;
// ------------------------------------------------------
// Display selected scanning region in full field of view
// ------------------------------------------------------
var
    KeepPenColor,KeepFontColor : Integer ;
    KeepStyle : TBrushStyle ;
begin

     KeepPenColor := Canvas.Font.Color ;
     KeepStyle := Canvas.Brush.Style ;
     KeepFontColor := Canvas.Font.Color ;

     Canvas.Pen.Color := clwhite ;
     Canvas.Brush.Style := bsClear ;
     Canvas.Font.Color := clWhite ;

     // Display zomm area selection rectangle
     Canvas.Rectangle(SelectedRect);

     // Display square corner and mid-point tags
     DrawSquare( Canvas, SelectedRect.Left, SelectedRect.Top ) ;
     DrawSquare( Canvas, (SelectedRect.Left + SelectedRect.Right) div 2, SelectedRect.Top ) ;
     DrawSquare( Canvas, SelectedRect.Right, SelectedRect.Top ) ;
     DrawSquare( Canvas, SelectedRect.Left, (SelectedRect.Top + SelectedRect.Bottom) div 2) ;
     DrawSquare( Canvas, SelectedRect.Right, (SelectedRect.Top + SelectedRect.Bottom) div 2) ;
     DrawSquare( Canvas, SelectedRect.Left, SelectedRect.Bottom ) ;
     DrawSquare( Canvas, (SelectedRect.Left + SelectedRect.Right) div 2, SelectedRect.Bottom ) ;
     DrawSquare( Canvas, SelectedRect.Right, SelectedRect.Bottom ) ;

     Canvas.Font.Color := KeepPenColor ;
     Canvas.Brush.Style := KeepStyle ;
     Canvas.Font.Color := KeepFontColor ;

     end ;


procedure TSnapFrm.DrawSquare(
          Canvas : TCanvas ;
          X : Integer ;
          Y : Integer ) ;
var
    Square : TRect ;
begin
     Square.Left := X - 3 ;
     Square.Right := X + 3 ;
     Square.Top := Y - 3 ;
     Square.Bottom := Y + 3 ;
     //Bitmap.Canvas.Pen.Color := clwhite ;
     Canvas.Brush.Style := bsSolid ;
     Canvas.Rectangle(Square);

     end ;


procedure TSnapFrm.UpdateImage(
          PCurrentFrame : PIntArray ;  // Display buffer
          var LUT : Array of Word ;    // Display look-up table [IN]
          BitMap : TBitMap ;           // Bit map to hold image (OUT)
          Image : TImage               // Image display component (OUT)
          ) ;

// --------------
// Display image
// --------------
const
    ROICrossSize = 8 ;
var
    Ybm,Yim,Xbm,Xim,StartOfLine,i : Integer ;
    iStep : Integer ;
    iDisplayZoom,iz : Integer ;
    iEnd : Integer ;
    PScanLine1 : PByteArray ;    // Bitmap line buffer pointer
    PScanLine : PByteArray ;    // Bitmap line buffer pointer
begin

    if not ImageAvailable then Exit ;
    if PFrameBuf = Nil then Exit ;

    // Copy image to display bitmap

    if DisplayZoom >= 1.0 then begin
       // ------------------------------
       // X1 and above display zoom factors
       // ------------------------------
       Ybm := 0 ;
       Yim := 0 ;
       StartOfLine := (sbYScroll.Position*MainFrm.Cam1.FrameWidth)
                      + sbXScroll.Position ;
       for Yim := sbYScroll.Position to MainFrm.Cam1.FrameHeight-1 do begin

           // Create line
           PScanLine := BitMap.ScanLine[Ybm] ;
           iDisplayZoom := Round(DisplayZoom) ;
           iz := 0 ;
           i := StartOfLine ;
           iEnd := Min( StartOfLine + MainFrm.Cam1.FrameWidth -1, NumPixelsPerFrame ) ;
           XBm := 0 ;
           while (i < iEnd) and (Xbm < BitMap.Width) do begin
              PScanLine[Xbm] := LUT[Word(PCurrentFrame^[i])] ;
              Inc(Xbm) ;
              Inc(iz) ;
              if iz >= iDisplayZoom then begin
                 inc(i) ;
                 iz := 0 ;
                 end
              end ;

           // Create additional lines
           for i := 1 to Round(DisplayZoom)-1 do begin
               Inc(Ybm) ;
               if Ybm >= Bitmap.Height then break ;
               PScanLine1 := BitMap.ScanLine[Ybm] ;
               for Xbm := 0 to Bitmap.Width-1 do PScanLine1[Xbm] := PScanLine[Xbm] ;
               end ;

           StartOfLine := StartOfLine + MainFrm.Cam1.FrameWidth ;

           Inc(Ybm) ;
           if Ybm >= Bitmap.Height then break ;

           end ;

       end
    else begin
       // ------------------------
       // X0.25 & 0.5 display zoom factor
       // ------------------------
       Ybm := 0 ;
       Yim := sbYScroll.Position ;
       iStep := Round(1.0/DisplayZoom) ;
       while (Ybm < BitMap.Height) and (Yim < MainFrm.Cam1.FrameHeight) do begin

          // Get scan line array pointer
          PScanLine := BitMap.ScanLine[Ybm] ;

          // Copy line to bitmap
          xBm := 0 ;
          XIm := sbXScroll.Position ;
          i := (Yim*MainFrm.Cam1.FrameWidth) + XIm*iStep ;
          while (Xbm < BitMap.Width) and
                (Xim < MainFrm.Cam1.FrameWidth) and
                (i < NumPixelsPerFrame) do begin
             PScanLine[Xbm] := LUT[Word(PCurrentFrame^[i])] ;
             Inc(Xbm) ;
             Xim := Xim + iStep ;
             i := i + iStep ;
             end ;

          Inc(Ybm) ;
          Yim := Yim + iStep

          end ;

       end ;

    // Add calibration bar
    if ckDisplayCalBar.Checked then DisplayCalibrationBar( BitMap ) ;

    Image.Picture.Assign(BitMap) ;

    // Draw rectangle round selected capture region
    Image.Canvas.FrameRect(CaptureRegion) ;

    // Display frame type at top-left of image
    Image.Canvas.TextOut( 0,0,ImageLabel ) ;

    end ;


procedure TSnapFrm.DisplayCalibrationBar(
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

procedure TSnapFrm.CalculateMaxContrast ;
// ---------------------------------------------------------
// Calculate and set display for maximum grey scale contrast
// ---------------------------------------------------------
const
    PixelSampleSize = 2000 ;
var
     i,NumPixels,NAvg,Istep : Integer ;
     z,zMean,zSD,zSum : Single ;
     iz,ZMin,ZMax,ZLo,ZHi,ZThreshold : Integer ;
     FrameType : Integer ;
begin

    if PDisplayBuf = Nil then Exit ;

    NumPixels := (MainFrm.Cam1.FrameHeight*MainFrm.Cam1.FrameWidth - 4) ;
    iStep := Max(NumPixels div PixelSampleSize,1) ;
    FrameType := 0 ;
    if NumPixels < 2 then Exit ;

    if ckContrast6SDOnly.Checked then begin
       // Set contrast range to +/- 3 x standard deviation
       ZSum := 0.0 ;
       nAvg := 0 ;
       i := 0 ;
       while i < NumPixels do begin
          ZSum := ZSum + PDisplayBuf^[i] ;
          i := i + iStep ;
          Inc(NAvg) ;
          end ;
       ZMean := ZSum / nAvg ;

       ZSum := 0.0 ;
       nAvg := 0 ;
       i := 0 ;
       while i < NumPixels do begin
          Z := PDisplayBuf^[i] ;
          ZSum := ZSum + (Z - ZMean)*(Z - ZMean) ;
          i := i + iStep ;
          Inc(NAvg) ;
          end ;
       ZSD := Sqrt( ZSum / (NumPixels-1) ) ;

       ZLo := Max( Round(ZMean - 3*ZSD),0) ;
       ZHi := Min( Round(ZMean + 3*ZSD), MainFrm.Cam1.GreyLevelMax );

       end
    else begin
       // Set contrast range to min-max
       ZMin := MainFrm.Cam1.GreyLevelMax ;
       ZMax := 0 ;
       ZSum := 0.0 ;
       nAvg := 0 ;
       i := 0 ;
       while i < NumPixels do begin
          iz := PDisplayBuf^[i]  ;
          if iz < ZMin then ZMin := iz ;
          if iz > ZMax then ZMax := iz ;
          i := i + iStep ;
          end ;
       ZLo := ZMin ;
       ZHi := ZMax ;
       end ;

    ZLo := Max(Round(0.9*ZLo),0) ;
    ZHi := Min(Round(1.1*ZHi),MainFrm.Cam1.GreyLevelMax) ;

    // Update contrast
    ZThreshold := Max((MainFrm.Cam1.GreyLevelMax div 50),2) ;
    if (not ckAutoOptimise.Checked) or
       (Abs(MainFrm.GreyLo[FrameType]- ZLo) > 10) then MainFrm.GreyLo[FrameType] := ZLo ;
    if (not ckAutoOptimise.Checked) or
       (Abs(MainFrm.GreyHi[FrameType]- ZHi) > 10) then MainFrm.GreyHi[FrameType] := ZHi ;

    // Ensure a non-zero LUT range
    if MainFrm.GreyLo[FrameType] = MainFrm.GreyHi[FrameType] then begin
       MainFrm.GreyLo[FrameType] := MainFrm.GreyLo[FrameType] - 1 ;
       MainFrm.GreyHi[FrameType] := MainFrm.GreyHi[FrameType] + 1 ;
       end ;

    MainFrm.UpdateLUT(0, MainFrm.Cam1.GreyLevelMax ) ;

    end ;


procedure TSnapFrm.FormClose(Sender: TObject; var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
begin

     FormClosing := True ;

     Timer.Enabled := False ;

     // Close shutter
     rbEXCShutterOpen.Checked := False ;
     rbEXCShutterClosed.Checked := True ;
     //UpdateLightSourceShutter ;

     // Stop acquiring images
     StopCamera ;

     // Save display zoom setting
     MainFrm.DisplayZoomIndex := cbDisplayZoom.ItemIndex ;

     // Request destruction of form
     Action := caFree ;

     end;


procedure TSnapFrm.edFrameIntervalKeyPress(Sender: TObject; var Key: Char);
// --------------------------------
// Inter-frame capture time updated
// --------------------------------
begin
     if key = #13 then begin
         OptimiseContrastNeeded := True ;
         StopCamera ;
         StartCamera ;
         // Report minimum readout time
        lbReadoutTime.Caption := format('Min.= %.3g ms',
                              [SecsToMs*MainFrm.Cam1.ReadoutTime]) ;
         end ;
     end;


procedure TSnapFrm.bFullScaleClick(Sender: TObject);
// --------------------------------------------------------
// Set display grey scale to full intensity range of camera
// --------------------------------------------------------
var
    FT : Integer ;
begin

     FT := 0 ;
     edDisplayIntensityRange.LoValue := 0 ;
     MainFrm.GreyLo[FT] := Round(edDisplayIntensityRange.LoValue) ;
     edDisplayIntensityRange.HiValue := MainFrm.Cam1.GreyLevelMax ;
     MainFrm.GreyHi[FT] := Round(edDisplayIntensityRange.HiValue) ;

     MainFrm.UpdateLUT( FT, MainFrm.Cam1.GreyLevelMax ) ;
     UpdateImage( pDisplayBuf, MainFrm.LUTs[FT*LUTSize],BitMap,Image1 ) ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[FT],
                               MainFrm.GreyHi[FT] ) ;

     end ;

procedure TSnapFrm.bMaxContrastClick(Sender: TObject);
// -------------------------------------------------------------
// Request display intensity range to be set for maximum contrast
// -------------------------------------------------------------
var
   FT : Integer ;
begin

    FT := 0 ;
    CalculateMaxContrast ;

    UpdateImage( pDisplayBuf, MainFrm.LUTs[FT*LUTSize],BitMap,Image1 ) ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[FT],
                               MainFrm.GreyHi[FT] ) ;

     OptimiseContrastNeeded := False ;

     end;


procedure TSnapFrm.edDisplayIntensityRangeKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------
// Update display intensity range
// ------------------------------
var
    FT : Integer ;
begin

     if key <> #13 then Exit ;

     FT := 0 ;

     if edDisplayIntensityRange.LoValue = edDisplayIntensityRange.HiValue then begin
        edDisplayIntensityRange.LoValue := edDisplayIntensityRange.LoValue - 1.0 ;
        edDisplayIntensityRange.HiValue := edDisplayIntensityRange.HiValue + 1.0 ;
        end ;

     MainFrm.GreyLo[FT] := Round(edDisplayIntensityRange.LoValue) ;
     MainFrm.GreyHi[FT] := Round(edDisplayIntensityRange.HiValue) ;

     MainFrm.UpdateLUT( FT, MainFrm.Cam1.GreyLevelMax ) ;

     MainFrm.UpdateLUT( FT, MainFrm.Cam1.GreyLevelMax ) ;
     UpdateImage( pDisplayBuf, MainFrm.LUTs[FT*LUTSize],BitMap,Image1 ) ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[FT],
                               MainFrm.GreyHi[FT] ) ;

     end;



procedure TSnapFrm.cbDisplayZoomChange(Sender: TObject);
// ---------------------------
// Display zoom factor changed
// ---------------------------
begin

    MainFrm.StatusBar.SimpleText := ' Wait ... Initialising Camera ' ;

    MainFrm.DisplayZoomIndex := cbDisplayZoom.ItemIndex ;
    StopCamera ;
    //MainFrm.Cam1.BinFactor := Round(edBinFactor.Value) ;
    DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])*0.01 ;
    StartCamera ;

    end;


procedure TSnapFrm.rbEXCShutterOpenClick(Sender: TObject);
// -----------------------------
// Open excitation light shutter
// -----------------------------
begin
   //outputdebugString(PChar(format('shutter open click %d',[Numframesdone]))) ;
   UpdateLightSource ;
   UpdateLightSourceShutter ;
   end;


procedure TSnapFrm.bFullFrameClick(Sender: TObject);
// ------------------------------------
// Set frame capture area to full frame
// ------------------------------------
begin

     bFullFrame.Enabled := False ;

     StopCamera ;
     // Set to full frame
     MainFrm.Cam1.SetCCDArea( 0,
                              0,
                              MainFrm.Cam1.FrameWidthMax-1,
                              MainFrm.Cam1.FrameHeightMax-1);


     // Report minimum readout time
     lbReadoutTime.Caption := format('Min.= %.3g ms',
                              [SecsToMs*MainFrm.Cam1.ReadoutTime]) ;

     StartCamera ;
     bFullFrame.Enabled := True ;

     end;


procedure TSnapFrm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
{ ------------------------------------------------------------
  Update size/location of display magnification adjustment box
  ------------------------------------------------------------}
const
     Margin = 2 ;
     ZoomMin = 0 ;
var
   BoxWidth,BoxHeight : Integer ;
   NewCursor :TCursor ;
begin

     MousePosition.X := X ;
     MousePosition.Y := Y ;

     if FMoveCaptureRegion then begin

        Canvas.Pen.Mode := pmXOR ;
        Canvas.FrameRect( CaptureRegion ) ;

        { Move the part of the zoom box which is under the mouse }
        case MoveMode of

             mvAll : begin
                { Move whole box }
                BoxWidth := CaptureRegion.Right - CaptureRegion.Left ;
                BoxHeight := CaptureRegion.Bottom - CaptureRegion.Top ;
                CaptureRegion.Left := IntLimitTo( CaptureRegion.Left + (X - FXOld),
                                                0,
                                                Image1.Width-1 - BoxWidth ) ;
                CaptureRegion.Right := CaptureRegion.Left + BoxWidth ;
                CaptureRegion.Top := IntLimitTo( CaptureRegion.Top + (Y - FYOld),
                                               0,
                                               Image1.Height-1 - BoxHeight ) ;
                CaptureRegion.Bottom := CaptureRegion.Top + BoxHeight ;
                FXOld := X ;
                FYOld := Y ;
                end ;

             mvLeftEdge : CaptureRegion.TopLeft.X := X  ;
             mvRightEdge : CaptureRegion.BottomRight.X := X  ;
             mvTopEdge : CaptureRegion.TopLeft.Y := Y ;
             mvBottomEdge : CaptureRegion.BottomRight.Y := Y ;
             mvTopLeft : Begin
               CaptureRegion.TopLeft.X := X  ;
               CaptureRegion.TopLeft.Y := Y  ;
               end ;
             mvTopRight : Begin
               CaptureRegion.BottomRight.X := X  ;
               CaptureRegion.TopLeft.Y := Y  ;
               end ;
             mvBottomLeft : Begin
               CaptureRegion.TopLeft.X := X  ;
               CaptureRegion.BottomRight.Y := Y  ;
               end ;
             mvBottomRight : Begin
               CaptureRegion.BottomRight.X := X  ;
               CaptureRegion.BottomRight.Y := Y  ;
               end ;
          else
          end ;

          { Keep within bounds }

        CaptureRegion.Left :=      IntLimitTo(CaptureRegion.Left,
                                              0,
                                              Image1.Width-1 ) ;
        CaptureRegion.Right :=     IntLimitTo(CaptureRegion.Right,
                                              0,
                                              Image1.Width-1 ) ;
        CaptureRegion.Top :=       IntLimitTo(CaptureRegion.Top,
                                              0,
                                              Image1.Height-1 ) ;
        CaptureRegion.Bottom :=    IntLimitTo(CaptureRegion.Bottom,
                                              0,
                                              Image1.Height-1 ) ;

        Canvas.FrameRect( CaptureRegion ) ;

        end
     else begin

         { *** Determine if the mouse is over part of the zoom box *** }

         // Get existing cursor
         NewCursor := Image1.Cursor ;

         if (CaptureRegion.TopLeft.Y <= Y) and (Y <= CaptureRegion.BottomRight.Y) and
            (CaptureRegion.TopLeft.X <= X) and (X <= CaptureRegion.BottomRight.X) then begin

            if (Abs(X-CaptureRegion.TopLeft.X) <= Margin) then begin
               if (Abs(Y-CaptureRegion.TopLeft.Y) <= Margin) then begin
                    NewCursor := crSizeNWSE ;
                    MoveMode := mvTopLeft ;
                    end
               else if (Abs(Y-CaptureRegion.BottomRight.Y) <= Margin) then begin
                    NewCursor := crSizeNESW ;
                    MoveMode := mvBottomLeft ;
                    end
               else begin
                    NewCursor := crSizeWE ;
                    MoveMode := mvLeftEdge ;
                    end ;
               end
            else if Abs(X-CaptureRegion.BottomRight.X) <= Margin then begin
               if Abs(Y-CaptureRegion.BottomRight.Y) <= Margin then begin
                    NewCursor := crSizeNWSE ;
                    MoveMode := mvBottomRight ;
                    end
               else if Abs(Y-CaptureRegion.TopLeft.Y) <= Margin then begin
                    NewCursor := crSizeNESW ;
                    MoveMode := mvTopRight ;
                    end
               else begin
                    NewCursor := crSizeWE ;
                    MoveMode := mvRightEdge ;
                    end ;
               end
            else if Abs(Y-CaptureRegion.TopLeft.Y) <= Margin then begin
               NewCursor := crSizeNS ;
               MoveMode := mvTopEdge ;
               end
            else if Abs(Y-CaptureRegion.BottomRight.Y) <= Margin then begin
               NewCursor := crSizeNS ;
               MoveMode := mvBottomEdge ;
               end
            else begin
               NewCursor := crDrag ;
               MoveMode := mvAll ;
               FXOld := X ;
               FYOld := Y ;
               end ;

            end
         else begin
            NewCursor := crDefault ;
            MoveMode := mvNone ;
            end ;

         // Update cursor on all images
         Image1.Cursor := NewCursor ;

         end ;

     end ;



procedure TSnapFrm.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
// ----------------------------------
// Mouse button depressed over images
// ----------------------------------
begin
     FMoveCaptureRegion := True ;
     MousePosition.X := X ;
     MousePosition.Y := Y ;
     end;

procedure TSnapFrm.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
// ---------------------------------
// Mouse button released over images
// ---------------------------------
begin
     FMoveCaptureRegion := False ;
     end;

procedure TSnapFrm.bSelectedRegionClick(Sender: TObject);
// ---------------------------
// Set new frame capture area
// ---------------------------
var
     OldLeft,OldTop : Integer ;
     FrameLeft,FrameTop,FrameBottom,FrameRight : Integer ;
     ImgLeft,ImgRight,ImgTop,ImgBottom : Integer ;     
begin

     bSelectedRegion.Enabled := False ;

     // Stop image capture
     StopCamera ;

     OldLeft := MainFrm.Cam1.FrameLeft ;
     OldTop := MainFrm.Cam1.FrameTop ;

     // Left edge of CCD frame
     ImgLeft := Round(CaptureRegion.Left/DisplayZoom) + sbXScroll.Position ;
     FrameLeft := OldLeft + ImgLeft*MainFrm.Cam1.BinFactor ;

     // Right edge of CCD frame
     ImgRight := Round((CaptureRegion.Right+1.0)/DisplayZoom) + sbXScroll.Position - 1;
     FrameRight := OldLeft + (ImgRight+1)*MainFrm.Cam1.BinFactor -1 ;

     // Top edge of CCD frame
     ImgTop := Round(CaptureRegion.Top/DisplayZoom) + sbYScroll.Position ;
     FrameTop := OldTop + ImgTop*MainFrm.Cam1.BinFactor ;

     // Bottom edge of CCD frame
     ImgBottom := Round((CaptureRegion.Bottom+1.0)/DisplayZoom) + sbYScroll.Position - 1;
     FrameBottom := OldTop + (ImgBottom+1)*MainFrm.Cam1.BinFactor -1 ;

     MainFrm.Cam1.SetCCDArea( FrameLeft,
                              FrameTop,
                              FrameRight,
                              FrameBottom);

     // Report minimum readout time
     lbReadoutTime.Caption := format('Min.= %.3g ms',
                              [SecsToMs*MainFrm.Cam1.ReadoutTime]) ;

     StartCamera ;

     bSelectedRegion.Enabled := True  ;

     end;

procedure TSnapFrm.FormDestroy(Sender: TObject);
// --------------------------------------------
// Free allocated resources when form destroyed
// --------------------------------------------
begin

     // Free display buffers
     if PDisplayBuf <> Nil then begin
        FreeMem(PDisplayBuf) ;
        PDisplayBuf := Nil ;
        end ;

     if PBackgroundBuf <> Nil then begin
        FreeMem(PBackgroundBuf) ;
        PBackgroundBuf := Nil ;
        end ;

     if PSumBuf <> Nil then begin
        FreeMem(PSumBuf) ;
        PSumBuf := Nil ;
        end ;


     FreeMem( PWorkBuf ) ;
     PWorkBuf := Nil ;

    // Free bitmaps
    BitMap.Free ;
    BitMap := Nil ;

    end ;

procedure TSnapFrm.edBinFactorKeyPress(Sender: TObject; var Key: Char);
// ----------------------------------
// Image pixel binning factor changed
// ----------------------------------
begin
     if Key = #13 then begin
         StopCamera ;
         MainFrm.Cam1.BinFactor := Round(edBinFactor.Value) ;
         StartCamera ;
         edBinFactor.Value := MainFrm.Cam1.BinFactor ;
         // Report minimum readout time
         lbReadoutTime.Caption := format('Min.= %.3g ms',
                              [SecsToMs*MainFrm.Cam1.ReadoutTime]) ;
         end ;
     end;


procedure TSnapFrm.cbPaletteChange(Sender: TObject);
// ------------------------------
// Display colour palette changed
// ------------------------------
begin

     MainFrm.PaletteType := TPaletteType(cbPalette.Items.Objects[cbPalette.ItemIndex]) ;
     if BitMap <> Nil then begin
        MainFrm.SetPalette( BitMap, MainFrm.PaletteType ) ;
        end ;

     end;

procedure TSnapFrm.FormActivate(Sender: TObject);
// -----------------------
// Form has become active
// -----------------------
begin

     // Disable imaging in recording form
     if MainFrm.FormExists('RecordFrm') then begin
        if not MainFrm.Recording then RecordFrm.StopCameraAndAnalogIO ;
        end ;

     // Start camera
     if not CameraRunning then StartCamera ;

     // Set Z stage control
     ZStageGrp.Visible := ZStage.Available ;
     edZPosition.Value := ZStage.Position ;
     edZPosition.LoLimit := ZStage.MinPosition ;
     edZPosition.HiLimit := ZStage.MaxPosition ;
     sbZPosition.Min := Round(ZStage.MinPosition/ZStage.MinStepSize) ;
     sbZPosition.Max := Round(ZStage.MaxPosition/ZStage.MinStepSize) ;
     sbZPosition.Position := Round(ZStage.Position/ZStage.MinStepSize) ;

     Timer.Enabled := True ;

     end;

procedure TSnapFrm.cbCameraGainChange(Sender: TObject);
// -------------------
// Camera gain changed
// -------------------
begin
     StopCamera ;
     StartCamera ;
     end ;


procedure TSnapFrm.sbContrastChange(Sender: TObject);
// --------------------------------------------------------
// Set display grey scale to new contrast slider setting
// --------------------------------------------------------
var
    FT : Integer ;
begin

     if ContrastPage.ActivePage <> SlidersTab then Exit ;

     FT := 0 ;

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

     MainFrm.UpdateLUT( FT, MainFrm.Cam1.GreyLevelMax ) ;
     UpdateImage( PDisplayBuf, MainFrm.LUTs[FT*LUTSize],BitMap,Image1) ;

     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[FT]  ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[FT]  ;

     end;



procedure TSnapFrm.bEnterCCDAreaClick(Sender: TObject);
begin
     SetCCDReadoutFrm := TSetCCDReadoutFrm.Create(Self) ;
     SetCCDReadoutFrm.CalledBy := 'SnapFrm' ;
     SetCCDReadoutFrm.Show ;
     SetCCDReadoutFrm.Left := 20 ;
     SetCCDReadoutFrm.Top := 20 ;
     end;


procedure TSnapFrm.FormResize(Sender: TObject);
// ------------------------------------------------
// Adjust control sizes when window size is changed
// ------------------------------------------------
begin

    FormResizeCounter := 5 ;

    end;


procedure TSnapFrm.UpdateLightSource ;
// -------------------------------------
// Update light source control waveforms
// -------------------------------------
begin

     // Exit if no light source or D/A channels configured
     if (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.LSWavelengthStart)) or
        (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.LSWavelengthEnd)) or
        (LightSource.DeviceType = lsNone) then Exit ;

     if LabIO.Resource[MainFrm.IOConfig.LSWavelengthStart].ResourceType = DACOut then begin
         // DAC output lines
        UpdateLightSourceDAC ;
        end
     else begin
        // Digital output lines
        UpdateLightSourceDIG ;
        end ;

     ImageLabel := format( '%d (%d)',
                   [MainFrm.EXCWavelengths[Max(cbWavelength.ItemIndex,0)].Centre,
                    MainFrm.EXCWavelengths[Max(cbWavelength.ItemIndex,0)].Width]) ;

     end ;


procedure TSnapFrm.UpdateLightSourceDAC ;
// ----------------------------------------
// Update light source DAC control pattern
// ----------------------------------------
var
     Dev : Integer ;
     iV : Integer ;
     FilterNums : Array[0..lsMaxVControl-1] of Integer ;
     Wavelengths : Array[0..lsMaxVControl-1] of Single ;
     Bandwidths : Array[0..lsMaxVControl-1] of Single ;
     VControl : Array[0..lsMaxVControl] of TLSVControl ;
     NumVControl : Integer ;
     NumWavelengths : Integer ;
begin

     // Exit if no light source or D/A channels configured
     if (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.LSWavelengthStart)) or
        (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.LSWavelengthEnd)) or
        (LightSource.DeviceType = lsNone) then Exit ;

     // Single wavelength excitatiom
     FilterNums[0] := Max(cbWavelength.ItemIndex,0) ;
     Wavelengths[0] :=MainFrm.EXCWavelengths[Max(cbWavelength.ItemIndex,0)].Centre ;
     Bandwidths[0] :=  MainFrm.EXCWavelengths[Max(cbWavelength.ItemIndex,0)].Width ;
     NumWavelengths := 1 ;

     // Fill D/A channel buffers with excitation light control voltages
     // for each frame type in use

     if rbEXCShutterOpen.Checked then begin
        // Get voltages for selected wavelength
        LightSource.WavelengthToVoltage( FilterNums[0],
                                         Wavelengths[0],
                                         Bandwidths[0],
                                         VControl,
                                         NumVControl );
        end
     else begin
        // Get voltages for shutters closed condition
        LightSource.ShutterClosedVoltages( VControl,
                                           NumVControl ) ;
        end ;

     // Update default values for DAC channels in use
     for iV := 0 to NumVControl-1 do begin
         Dev := VControl[iV].Device ;
         LabIO.DACOutState[Dev][VControl[iV].Chan] := VControl[iV].V ;
         // Output to DAC channel (if it is not in use)
         if not LabIO.DACActive[Dev] then begin
            LabIO.WriteDAC( Dev, VControl[iV].V, VControl[iV].Chan) ;
            end ;
         end ;

     end ;


procedure TSnapFrm.UpdateLightSourceDIG ;
// ----------------------------------------
// Update light source digital output waveform
// ----------------------------------------
var
     Device : Integer ;
     iV : Integer ;
     FilterNums : Array[0..lsMaxVControl-1] of Integer ;
     Wavelengths : Array[0..lsMaxVControl-1] of Single ;
     Bandwidths : Array[0..lsMaxVControl-1] of Single ;
     VControl : Array[0..lsMaxVControl-1] of TLSVControl ;
     NumVControl : Integer ;
     NumWavelengths : Integer ;
     Bit : Word ;
     BitWord  : Word ;
     BitMask  : Word ;
begin

     // Single wavelength excitatiom
     FilterNums[0] := Max(cbWavelength.ItemIndex,0) ;
     Wavelengths[0] :=MainFrm.EXCWavelengths[Max(cbWavelength.ItemIndex,0)].Centre ;
     Bandwidths[0] :=  MainFrm.EXCWavelengths[Max(cbWavelength.ItemIndex,0)].Width ;
     NumWavelengths := 1 ;

     if rbEXCShutterOpen.Checked then begin
        // Get voltages for selected wavelength
        LightSource.WavelengthToVoltage( FilterNums[0],
                                         Wavelengths[0],
                                         Bandwidths[0],
                                         VControl,
                                         NumVControl );
        end
     else begin
        // Get voltages for shutters closed condition
        LightSource.ShutterClosedVoltages( VControl,
                                           NumVControl ) ;
        end ;

     // Update binary word
     BitWord := 0 ;
     BitMask := 0 ;
     for iV := 0 to NumVControl-1 do begin
         Bit := LabIO.BitMask(VControl[iV].Chan) ;
         BitMask := BitMask or Bit ;
         if VControl[iV].V <> 0.0 then BitWord := BitWord or Bit ;
         Bit := Bit*2 ;
         end ;
     BitMask := not BitMask ;

     // Update digital output port state
     // (If in use, leave update to existing process)
     Device :=  VControl[0].Device ;
     LabIO.DigOutState[Device] := (LabIO.DigOutState[Device] and BitMask) or BitWord ;
     if not LabIO.DIGActive[Device] then begin
        LabIO.WriteToDigitalOutPutPort( Device, LabIO.DigOutState[Device] ) ;
        end ;

     end ;


procedure TSnapFrm.UpdateLightSourceShutter ;
// --------------------------------
// Turn light source shutter on/off
// --------------------------------
var
    ShutterBit : Integer ;
    ShutterOpen,ShutterClosed : Integer ;
    ShutterBitMask : Integer ;
    Device : Integer ;
begin

     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.LSShutter) then Exit ;

        // Set shutter controlled by digital O/P
        // -------------------------------------

     Device := LabIO.Resource[MainFrm.IOConfig.LSShutter].Device ;
     ShutterBit := LabIO.BitMask(LabIO.Resource[MainFrm.IOConfig.LSShutter].StartChannel) ;
     ShutterBitMask := not ShutterBit ;

     // Set polarity of shutter bit
     if MainFrm.IOConfig.LSShutterActiveHigh then begin
        ShutterOpen := ShutterBit ;
        ShutterClosed := 0 ;
        end
     else begin
        ShutterOpen := 0 ;
        ShutterClosed := ShutterBit ;
        end ;

     // Determine when shutter is open
     if rbEXCShutterOpen.Checked then begin
        LabIO.DigOutState[Device] := (LabIO.DigOutState[Device] and ShutterBitMask) or ShutterOpen ;
        end
     else begin
        LabIO.DigOutState[Device] := (LabIO.DigOutState[Device] and ShutterBitMask) or ShutterClosed ;
        end ;

     if not LabIO.DIGActive[Device] then begin
        LabIO.WriteToDigitalOutPutPort( Device, LabIO.DigOutState[Device] ) ;
        end ;

     end ;


procedure TSnapFrm.StopLiveImaging ;
// -----------------
// Stop live imaging
// -----------------

begin
    // Stop camera acquisition
    StopCamera ;

    // Disable timer-scheduled operations
    Timer.Enabled := False ;

    end ;


procedure TSnapFrm.bSnapImageClick(Sender: TObject);
// --------------------------
// Save current image to file
// --------------------------
var
    FileName : String ;
begin

     // Create a file name
     repeat
        FileName := SaveDialog.InitialDir + format('\snap_%d',[SnapNum]) ;
        case SaveDialog.FilterIndex of
           1 : FileName := ChangeFileExt( FileName, '.pic' ) ;
           2 : FileName := ChangeFileExt( FileName, '.stk' ) ;
           3 : FileName := ChangeFileExt( FileName, '.tif' ) ;
           end ;
        Inc(SnapNum) ;
        until not FileExists(FileName) ;

     // Open save file dialog
     SaveDialog.FileName := FileName ;
     if not SaveDialog.Execute then Exit ;

     // Ensure extension is set
     FileName := SaveDialog.FileName ;
     case SaveDialog.FilterIndex of
        1 : FileName := ChangeFileExt( FileName, '.pic' ) ;
        2 : FileName := ChangeFileExt( FileName, '.stk' ) ;
        3 : FileName := ChangeFileExt( FileName, '.tif' ) ;
        end ;

     // Check if file exists already
     if FileExists( FileName ) then begin
        if MessageDlg( format(
           'File %s already exists! Do you want to overwrite it? ',[FileName]),
           mtWarning,[mbYes,mbNo], 0 ) = mrNo then Exit ;
        end ;

     // Create file
     if not ImageFile.CreateFile( FileName,
                                  MainFrm.Cam1.FrameWidth,
                                  MainFrm.Cam1.FrameHeight,
                                  MainFrm.Cam1.NumBytesPerPixel*8,
                                  1,
                                  False ) then Exit ;

     // Save image
     ImageFile.SaveFrame32( 1, PDisplayBuf ) ;

     // Close file
     ImageFile.CloseFile ;

     // Log event
     LogFrm.AddLine( 'Live image saved to ' + FileName ) ;

     end;


procedure TSnapFrm.CopyImageToClipboard ;
{ -------------------------------------------
  Copy image to clipboard as Windows metafile
  ------------------------------------------- }
begin

    if BitMap = Nil then Exit ;

    // Copy bitmap image
    BitMap.SaveToClipboardFormat( ClipboardImageFormat,
                                  ClipboardImageData,
                                  ClipboardPalette ) ;
    Clipboard.SetAsHandle( ClipboardImageFormat,
                           ClipboardImageData ) ;

    end ;


procedure TSnapFrm.rbEXCShutterClosedClick(Sender: TObject);
// -----------------------------
// Close excitation light shutter
// -----------------------------
begin
   //outputdebugString(PChar(format('shutter open click %d',[Numframesdone]))) ;
   UpdateLightSource ;
   UpdateLightSourceShutter ;
   end;


procedure TSnapFrm.cbWavelengthChange(Sender: TObject);
// ----------------------------
// Change excitation wavelength
// ----------------------------
begin
     MainFrm.LiveWindowWavelength := cbWaveLength.ItemIndex ;
     UpdateLightSource ;
     end;


procedure TSnapFrm.DoShadingCorrection ;
// -------------------------------
// Do shading correction operation
// -------------------------------
var
    i,iSub : Integer ;
    Sum : Single ;
begin

     if not bAcquireBackground.Enabled then begin

        // Copy latest frame to background
        for i := 0 to NumPixelsPerFrame-1 do begin
            PSumBuf^[i] := PSumBuf^[i] + PDisplayBuf^[i] ;
            end ;
        Inc(ShadeCorNumFramesAveraged) ;

        if ShadeCorNumFramesAveraged >=
           Round(EdShadeCorNumFramesAveraged.Value) then begin

           // Average background frame
           for i := 0 to NumPixelsPerFrame-1 do begin
               PBackgroundBuf^[i] := Round(PSumBuf^[i]/ShadeCorNumFramesAveraged) ;
               end ;

           // Smooth background frame
           if Round(edShadeCorImageBlockSize.Value) > 1 then begin
              SmoothImage( PBackgroundBuf,
                           MainFrm.Cam1.FrameWidth,
                           MainFrm.Cam1.FrameHeight,
                           Round(edShadeCorImageBlockSize.Value)) ;
              end ;

           // Calculate shading (differences) image
           case cbShadeCorNormalisation.ItemIndex of

              NormaliseToMean : begin
                // Difference around mean intensity
                Sum := 0.0 ;
                for i := 0 to NumPixelsPerFrame-1 do Sum := Sum + PBackgroundBuf^[i] ;
                iSub := Round(Sum/NumPixelsPerFrame) ;
                end ;

              NormaliseToMin : Begin
                // Difference around min. intensity
                iSub := High(iSub) ;
                for i := 0 to NumPixelsPerFrame-1 do begin
                    if PBackgroundBuf^[i] < iSub then iSub := PBackgroundBuf^[i] ;
                    end ;
                end ;

              NormaliseToMax : Begin
                // Difference around min. intensity
                iSub := 0 ;
                for i := 0 to NumPixelsPerFrame-1 do begin
                    if PBackgroundBuf^[i] > iSub then iSub := PBackgroundBuf^[i] ;
                    end ;
                end ;

              else begin
                iSub := 0 ;
                end ;

              end ;

           for i := 0 to NumPixelsPerFrame-1 do begin
               PBackgroundBuf^[i] := PBackgroundBuf^[i] - iSub ;
               end ;

           // End of backround acquisition - re-enable button
           bAcquireBackground.Enabled := True ;

           end ;

        end ;

     end ;


procedure TSnapFrm.SmoothImage(
          PBuf : PIntArray ;  // Imagebuffer
          FrameWidth : Integer ; // Width of frame
          FrameHeight : Integer ; // Height of frame
          BlockSize : Integer    // Smoothing block size
          ) ;
//
// Smooth image using n x n pixel averaging block
// --------------------------------------------
var
    x,y,x0,y0,x1,y1,ix,iy,ix1,iy1 : Integer ;
    i : Integer ;
    NumPixels,Sum,nSum : Integer ;
    PTemp : PIntArray ;
begin

    NumPixels := FrameWidth*FrameHeight ;

    GetMem( PTemp, NumPixels*4 ) ;

    BlockSize := Max(Min(BlockSize,FrameWidth) div 2,1) ;

    nSum := (2*BlockSize + 1)*(2*BlockSize + 1) ;
    Sum := 0 ;
    for y := 0 to FrameHeight-1 do begin

        // Keep within image
        y0 := Max(y-BlockSize,0) ;
        y1 := Min(y+BlockSize,FrameHeight-1) ;

        for x := 0 to FrameWidth-1 do begin

            if x = 0 then begin
               // First pixel in row
               Sum := 0 ;
               nSum := 0 ;
               for ix := x-BlockSize to x+BlockSize do begin
                   if ix < 0 then ix1 := 0
                   else if ix >= FrameWidth then ix1 := FrameWidth-1
                   else ix1 := ix ;
                   for iy := y-BlockSize to y+BlockSize do begin
                       if iy < 0 then iy1 := 0
                       else if iy >= FrameHeight then iy1 := FrameHeight-1
                       else iy1 := iy ;
                       i := ix1 + iy1*FrameWidth ;
                       Sum := Sum + PBuf^[i] ;
                       Inc(nSum) ;
                       end ;
                   end ;
               end
            else begin
               // All other pixels
               x0 := Max(x-BlockSize-1,0) ;
               x1 := Min(x+BlockSize,FrameWidth-1) ;
               for iy := y-BlockSize to y+BlockSize do begin
                    if iy < 0 then iy1 := 0
                    else if iy >= FrameHeight then iy1 := FrameHeight-1
                    else iy1 := iy ;
                    i := x0 + iy1*FrameWidth ;
                    Sum := Sum - PBuf^[i] ;
                    i := x1 + iy1*FrameWidth ;
                    Sum := Sum + PBuf^[i] ;
                    end ;
               end ;

            i := x + y*FrameWidth ;
            PTemp^[i] := Sum div nSum ;

            end ;
        end ;

    for i := 0 to NumPixels-1 do PBuf^[i] := PTemp^[i] ;

    FreeMem( PTemp ) ;

    end ;

procedure TSnapFrm.bAcquireBackgroundClick(Sender: TObject);
// ---------------------------------------
// Start acquisition of a background image
// ---------------------------------------
var
    i : Integer ;
begin

    bAcquireBackground.Enabled := False ;

    // Clear summation buffer
    for i := 0 to NumPixelsPerFrame-1 do begin
        PSumBuf^[i] := 0.0 ;
        PBackgroundBuf^[i] := 0 ;
        end ;

   ShadeCorNumFramesAveraged := 0 ;

   end;


procedure TSnapFrm.ckBackgroundSubtractionClick(Sender: TObject);
// -----------------------------------
// Enabled/disable shading correction
// -----------------------------------
begin
//     bAcquireBackground.Enabled := not ckBackgroundSubtraction.Checked ;
     end;

procedure TSnapFrm.lbShadeCorShowSettingsClick(Sender: TObject);
// -------------------------------------
// Show/Hide shading correction settings
// -------------------------------------
begin

     ShadeCorSettingsPanel.Visible := not ShadeCorSettingsPanel.Visible ;
     ShowHideShadeCorSettingsPanel ;

     end;

procedure TSnapFrm.ShowHideShadeCorSettingsPanel ;
// -------------------------------------
// Show/Hide shading correction settings
// -------------------------------------
begin

     ShadeCorSettingsPanel.Visible := sbShadeCorShowSettings.Down ;

     if not ShadeCorSettingsPanel.Visible then begin
        // Hide settings
        ShadingGrp.Height := sbShadeCorShowSettings.Top +
                             sbShadeCorShowSettings.Height + 5 ;
        end
     else begin
        // Show settings
        ShadingGrp.Height := ShadeCorSettingsPanel.Top +
                             ShadeCorSettingsPanel.Height + 5 ;
        end ;

     ExcitationLightGrp.Top := ShadingGrp.Top + ShadingGrp.Height + 2 ;
     ZStageGrp.Top := ExcitationLightGrp.Top + ExcitationLightGrp.Height + 2 ;

     end;


procedure TSnapFrm.sbShadeCorShowSettingsClick(Sender: TObject);
// -------------------------------------
// Show/Hide shading correction settings
// -------------------------------------
begin

     ShowHideShadeCorSettingsPanel ;

     end;

procedure TSnapFrm.bSetLaserIntensityClick(Sender: TObject);
// -------------------
// Set laser intensity
// -------------------
begin

     SetLasersFrm.Left := MainFrm.Left + 20 ;
     SetLasersFrm.Top := Mainfrm.Top + 20 ;
     SetLasersFrm.Show ;


     end;


procedure TSnapFrm.ckAutoOptimiseClick(Sender: TObject);
// ----------------------------------
// Auto contrast optimisation changed
// ----------------------------------
begin
    MainFrm.ContrastAutoOptimise := ckAutoOptimise.Checked ;
    end;

procedure TSnapFrm.ckContrast6SDOnlyClick(Sender: TObject);
// --------------------------------------------------
// 6 standard deviation contrast optimisation changed
// --------------------------------------------------
begin
    MainFrm.Contrast6SD := ckContrast6SDOnly.Checked ;
    end;

procedure TSnapFrm.sbZPositionChange(Sender: TObject);
// ------------------------
// Z Stage position changed
// ------------------------
begin
    ZStage.Position := sbZPosition.Position*ZStage.MinStepSize ;
    edZPosition.Value := ZStage.Position ;
    end;

procedure TSnapFrm.edZPositionKeyPress(Sender: TObject; var Key: Char);
// ------------------
// Z position changed
// ------------------
begin
      if Key = #13 then begin
         ZStage.Position := edZPosition.Value ;
         sbZPosition.Position := Round(ZStage.Position/ZStage.MinStepSize) ;
         edZPosition.Value := ZStage.Position ;
         end ;
      end;

end.
