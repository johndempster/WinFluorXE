unit RecUnit;
// =====================================================================================
// WinFluor - Image Recording Module (c) J. Dempster, University of Strathclydcbsee 2001-03
// =====================================================================================
// All Rights Reserved
// 1/8/01
// 27.8.02
// 30.8.02 Start/Stop stimulusadcwritef now forces camera restart to keep
//         multiple frame types in correct order.
// 27.3.03 Can now acquire up to 4 frame types
// 3.7.03  StopCamera method added
// 7.7.03 .... Red,green,blue colour palettes added
// 9.7.03 .... Each frame type display now has its own LUT
// 30.7.03 ... Displayed image now held in 32 bit display buffers
//             (I-PentaMax) Intensifier now shuttered by LOGIC OUT pulse from camera
// 18.8.03 ... Time lapse recording mode added
// 21.8.03 ... FileSeek to end of file before all FileWrites
// 26.9.03 ... FileSeeks to end too slow. Replaced with FileSeek to internally maintained pointer.
// 21.10.03 .. Now uses min/max display point compression for analogue signal display
// 13.11.03 .. No. A/D channels set to zero if no A/D converter available
// 14.11.03 .. Disk space check now made before recording begins
// 16.11.03 .. Zero level cursors added to A/D display
// 4.12.03 ... Zero divide error when display duration=1 sample fixed
//             -10412 error when clicking record immediately after stop fixed
// 23.3.04 ... VHold box now works
//             Open SealTest window now closed when recording starts                    fra
//             A/D sampling is re-started when switching from seal test to record window
// 02.09.04 Digital pulse stimulus added
// 09.09.04 Pauses within recording now indicated by // markers
// 15.12.04 External trigger pulse now adjusted to take account of whether
//          exposure or frame readout is triggered
// 11.02.05 Add Marker function fixed
// 22.03.05 Monochromators now set to max. wavelength when shutter is closed
//          and no digital shutter control line exists
// 21.04.05 Max Contrast now set time course chart range to 0-2*max pixel intensity
// 17.05.05 VPR Voltage program stimulator added
// 23.06.05 Files now switched to writeenabled mode for recording
// 05.08.05 Selected Wavelength sequences now start automatically when form displayed
// 31.08.05 Disk write performance monitor added
// 01.09.05 A/D channel settings now stored in MainFrm.ADCChannel
// 06.09.05 Andor frame buffer size increased to 32
// 11.09.05 .MagnifyADCChannelDisplay and .ReduceADCChannelDisplay added
// 27.02.06 CameraTriggerOffsetInterval can now be negative
// 21.03.06 Time course plots now in separate window
//          Excitation light & stimulator now on tab panel
// 04.04.06 Timing problems with PCI-6229 fixed
// 23.05.06 Stimulus protocols cannot be changed during recording
// 16.07.06 25% display zoom factor added
// 17.07.06 Max. contrast now calculated as mean +/- 3 s.d.
//          Incr./Decr. contrast buttons now update display when pressed
// 11.08.06 No. of frame recorded to file now constrained to multiple of no. frame types
// 17.10.06 Display contrast and brightness control sliders added instead of control
//          increase/decrease buttons.
// 24.01.07 Magnified displays can now be scrolled within image
// 26.01.07 Stimulus protocols can now be linked together
//          Changing protocols, or stopping a stimulus program causes
//          recording to be restarted.
// 25.02.07 Spectrum data parameters now stored in IDR file
// 03.03.07 Laser wavelengths now have "L" suffix
// 07.03.07 Now avoids pauses in display by using asynchronous writes to IDR file
// 20.03.07 Scroll bars cannot now be set with negative maxs.
//          Intermittent memory protection fault when X/Y scrolling display fixed
// 20.07.07 Additional ROIs can now be added to plot
// 17.08.07 Time course displays now cleared used .ClearDisplays
// 29.08.07 ClearStimulus and optimisedisplay no longer waits for buffer-full
// 13.09.07 Excitation light now shut off during time lapse intervals
// 21.12.07 UpdateStimulusWaveforms replaces UpdateVCommandWaveform & UpdateDigitalWaveform
// 14.01.08 Live regions of interest can now be averaged as square blocks of pixels
// 04.04.08 Out of memory error when no voltage stimulus channels available fixed
// 07.08.08 Set BINFACTOR removed from zoom changes
// 30.04.08 Display no longer over-magnifies at 100% zoom and above settings
// 07.05.08 Dual-rate sequences now supported
// 12.05.08
// 20.05.08 Word() added to LUT index in X0.5 and X0.25 display to avoid
//                 memory violation
// 17.07.08 Marker text time and location now correct in time lapse mode
// 08.09.08 Shading correction function added
// 23.09.08 Speedbutton component now used for show/hide settings buttons
// 16.03.09 JD No. of frame buffers can now exceed 32
// 18.03.09 JD Frame buffer size limited to less than 8s in duration
//             Frame acquisition now only checked in one place (fixes
//             intermittent frame cycle stoppages with PVCAM
//             FC=xx% frame capture % debug data added to status line
// 15.04.09 JD Display jump when setting contrast fixed
//             Image area selection no longer progressively shrinks image
// 20.05.09 JD Shutters can now be closed during blanking period at
//             end of each frame, duration determined by LightSource.shutterblankingperiod
// 07.09.09 JD Change made in Paris (3/7/9) to fix premature addition of empty A/D data buffer
//             at beginning of file (Pedro Limas, Paris)
//             Camera re-started if MainFrm.Cam1.CameraRestartRequired flag is raised
//             unless recording is in progress
// 15.09.09 JD Elapsed time FP zero divide error prevented
// 19.01.10 JD Time lapse shuttering of light source now works with digital control. (in progress)
// 21.01.10 JD Up to 10 multi-wavelength sequences can be selected by list
// 26.01.10 JD Auto contrast adjustment option added
// 05.02.10 JD Auto contrast now applied at 1s intervals
//             Contrast settings stored in INI
// 01.09.10 JD Live ROI position now stored in INI file
// 09.09.10 JD Cell capacity calculation setup added (not functional yet)
// 21.12.10 JD Photostimulus feature added
//             Start Stimulus and Start PhotoStimulus buttons now re-enable after stimulus is complete
// 02.02.11 JD Thickness of calibration bar can now be set by user
// 04.08.11 JD Laboratory interface device now checked if >0 and DAC/Dig update skipped if not
//             to avoid memory access violation when lasers are not defined in OptoScanWithLasers
// 27.08.12 JD FrameTypeCycleLength now correctly set to 1 when no light source defined
//             User defined number of records now collected (rather than multiple of half buffer size)
//             Time lapse interval now correctly limited to no longer the duration of the timing waveform buffer
// 01.02.12 JD Form will now always close after 3 clicks
// 06.02.12 JD Minimum time lapse interval now reduced to twice frame interval x wavelength cycle length
//             Time lapse now acquires exact number of frames specified
// 18.07.12 JD Empty flags placed at beginning and end of each frame
//             to handle cameras that update end of frame first (intended to fix
//             intermittent blank frames produced by QImaging Bolt, but not checked yet)
//             QCAM NumFramesInBuffer now defined by MainFrm.Cam1.MaxFramesInBuffer (increased to 64)
// 19.07.12 JD No. of frames required now adjusted to multiple of frame capture cycle
// 23.07.12 JD Stimuli can be started and stopped without restarting camera
// 26.11.12 JD Zoom combo box moved to top of image area
//             Time lapse + burst mode added
//             Recording modes now selected by combo box
//             Recording period now specified as time rather than number of frames
// 28.11.12 JD Contrast adjustment now only uses sample of 2000 pixels in image
//             Excitation On/Off no longer restarts camera
// 04.12.12 JD Option added to turn on excitation when recording starts and off when it ends
// 14.12.12 V3.4.3 Turn on when recording option added. When selected, excitation light is turned on
//                 when recording starts and off at which it stops.
// 28.01.13 V3.3.4 JD Piezo Z focus control added
//                 FrameDisplayed array removed. Could have been causing random memory access violations
//                 when overwriting other variables since was being written beyond end of array.
// 03.04.13 JD Imaging area selection rectangle now made more visible by small squares at corners and middle.
//             LabIO Analog input and digital outputs now clocked by digital sample clock when no D/A channels in use on timing device
//             Excitation on/off now restarts camera when not recording to reduce delay on light changing
// 04.06.13 JD Stimulus no longer initiated on start of recording when Stimulus Start on Record is ticked
//             but no stimulus is available
//             Voltage protocol folder now selected using SelectDirectory
// 30.10.13 JD In continuous recording mode, if Zstack enabled, excitation now turned off at end of frame
//             during movement of Z positioner. Z positioner command voltage updated ZStage.StepTime
//             BEFORE end of frame. In time lapse mode, Z positioner update at end of wavelength seqence.
// 12.13.13 JD NumADCScans made Int64
// 13.12.13 JD DCAM NumFramesInBuffer set to hold 1 second history or are minimum of 8
// 16.12.13 JD .StartCapture Now returns False if unable to allocate enough frame buffer memory
// 29.01.14 Updated to Compile under both 32/64 bits (File handle now THandle)
// 18.02.14 DCAM NumFramesInBuffer now same as Andor SDK3
// 28.02.14 Emission filter control added and position of shading group moved to below light source group
// 05.03.14 50/50 top/bottom split Image mode added
// 03.04.14 Bulb exposure mode added.
// 11.06.14 Recording duration now correct when Split Image selected
// 13.06.14 StopCamera/StartCamera removed from UpdateExcitationWavelength and
//          replaced with RestartCamera() in radio button click handlers to
//          avoid premature start of camera during bRecord.click and unnecessary
//          camera stops and starts. Fixes hang ups and blue screens with OptiMOS camera
//          Error in horizontal display scroll position with display zooms <100% fixed
// 16.06.14 flDisplayBuf moved to RecPlotUnit.pas
// 17.06.14 12.5% display zoom added
// 18.06.14 burst illumination delay increased to 10 seconds to allow sufficient time
//          for waveform to be changed to ensure illumination on at start of burst
// 09.07.14 Calibration bar now sized correctly from Cam1.PixelWidth
// 05.09.14 exposure time no longer shortened by .EmissionFilterChangeTime when no
//          emission filters in use. Was causing filter wavelength sequence to be shifted
//          with non-zero exchange times.
// 02.12.14 Set Laser Intensity button name changed to Set Light Intensity
// 22.01.15 Restart if camera stops sending frames
// 26.01.15 Buffer overflow message written to log file
//          Andor frame buffer increased to MaxBufferSize
// 27.01.15 .IDR data file closed and re-opened after each time lapse frame
//          to preserve file directory entry
// 08.04.15 Display now updated at rate of fastest frame type when split rate multi-wavelength sequences in use
// 09.04.15 Not enough disk space check no longer stops camera and is reported in window title bar

{$DEFINE USECONT}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ValEdit, RangeEdit, ScopeDisplay, Spin, shared,
  ValidatedEdit, IDRFile, LabIOUnit, math, StrUtils, ComCtrls, LightSourceUnit, excsetupunit,
  Menus, Buttons, filectrl, Types ;

const
    cMaxBytesInDACBuffer = 10000000 ;
    cADCWriteBufferDuration = 10.0 ; //5.0 ;
    MaxDisplayScans = 2000 ;
    MaxDACChannels = 16 ;
    ADCChannelLimit = 7 ;
    MaxLiveROIs = 10 ;
    rmRecordingInProgress = 0 ;
    rmStopFrameRecording = 1 ;
    rmFrameRecordingStopped = 2 ;
    rmStopADCRecording = 3 ;
    rmRecordingStopped = 4 ;

    rmContinuous = 0 ;
    rmTimeLapse = 1 ;
    rmTimelapseBurst = 2 ;

type

  TLiveROI = record
      X : Integer ;
      Y : Integer ;
      end ;

  TRecordFrm = class(TForm)
    Timer: TTimer;
    ImageGrp: TGroupBox;
    Image1: TImage;
    ControlGrp: TGroupBox;
    Image2: TImage;
    IdentGrp: TGroupBox;
    Label2: TLabel;
    edIdent: TEdit;
    Image3: TImage;
    Image4: TImage;
    DisplayGrp: TGroupBox;
    cbPalette: TComboBox;
    RecordingGrp: TGroupBox;
    bRecord: TButton;
    bStop: TButton;
    Label1: TLabel;
    edRecordingPeriod: TValidatedEdit;
    MarkGrp: TGroupBox;
    edMarker: TEdit;
    bMark: TButton;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    sbXScroll: TScrollBar;
    sbYScroll: TScrollBar;
    OpenDialog: TOpenDialog;
    ROIPanel: TPanel;
    bAddROI: TButton;
    bDeleteROIs: TButton;
    ckDisplayCalBar: TCheckBox;
    TimelapsePanel: TPanel;
    Label9: TLabel;
    edTimeLapseInterval: TValidatedEdit;
    MainMenu1: TMainMenu;
    ImageCaptureGrp: TGroupBox;
    lbReadoutTime: TLabel;
    Label3: TLabel;
    edFrameInterval: TValidatedEdit;
    ImageCaptureSettingsPanel: TPanel;
    CCDAreaGrp: TGroupBox;
    Label4: TLabel;
    bFullFrame: TButton;
    bSelectedRegion: TButton;
    edBinFactor: TValidatedEdit;
    bEnterCCDArea: TButton;
    DisplaySettingsPanel: TPanel;
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
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    sbContrast: TScrollBar;
    sbBrightness: TScrollBar;
    ShadingGrp: TGroupBox;
    ckBackgroundSubtraction: TCheckBox;
    bAcquireBackground: TButton;
    ShadeCorSettingsPanel: TPanel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    edShadeCorImageBlockSize: TValidatedEdit;
    edShadeCorNumFramesAveraged: TValidatedEdit;
    cbShadeCorNormalisation: TComboBox;
    LightStimGrp: TGroupBox;
    LightStimPage: TPageControl;
    ExcitationLightTab: TTabSheet;
    GroupBox6: TGroupBox;
    rbEXCShutterOpen: TRadioButton;
    rbEXCShutterClosed: TRadioButton;
    WavelengthGrp: TGroupBox;
    rbSingleWavelength: TRadioButton;
    rbMultipleWavelengths: TRadioButton;
    cbWavelength: TComboBox;
    rbSpectrum: TRadioButton;
    bSetLaserIntensity: TButton;
    StimulatorTab: TTabSheet;
    cbStimProgram: TComboBox;
    bStartStimulus: TButton;
    bStopStimulus: TButton;
    VHoldGrp: TGroupBox;
    VHold0Panel: TPanel;
    lbVHold0: TLabel;
    edVHold0: TValidatedEdit;
    VHold1Panel: TPanel;
    Label7: TLabel;
    edVHold1: TValidatedEdit;
    bSetSubFolder: TButton;
    sbImageCaptureShowSettings: TSpeedButton;
    sbDisplayShowSettings: TSpeedButton;
    sbShadeCorShowSettings: TSpeedButton;
    sbLightStimShowSettings: TSpeedButton;
    cbSequence: TComboBox;
    ckAutoOptimise: TCheckBox;
    PhotoStimTab: TTabSheet;
    cbPhotoStimProgram: TComboBox;
    bStartPhotoStimulus: TButton;
    bStopPhotoStimulus: TButton;
    ckStartStimOnRecord: TCheckBox;
    cbRecordingMode: TComboBox;
    Label18: TLabel;
    BurstModePanel: TPanel;
    Label19: TLabel;
    edBurstDuration: TValidatedEdit;
    edBurstInterval: TValidatedEdit;
    Label20: TLabel;
    Label6: TLabel;
    cbDisplayZoom: TComboBox;
    ckExcitationOnWhenRecording: TCheckBox;
    ZStageGrp: TGroupBox;
    edZPosition: TValidatedEdit;
    sbShowHideZStackSettings: TSpeedButton;
    ZStackGrp: TGroupBox;
    Label21: TLabel;
    edZStartPos: TValidatedEdit;
    Label22: TLabel;
    edZStepSize: TValidatedEdit;
    Label23: TLabel;
    edZNumSteps: TValidatedEdit;
    ckZStackEnabled: TCheckBox;
    sbZPosition: TScrollBar;
    cbCameraGain: TComboBox;
    Label8: TLabel;
    ckSplitCCDImage: TCheckBox;
    IDRBackground: TIDRFile;
    IDRFileBurst: TIDRFile;
    IDRFileXY: TIDRFile;
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure bRecordClick(Sender: TObject);
    procedure edRecordingPeriodKeyPress(Sender: TObject; var Key: Char);
    procedure bStopClick(Sender: TObject);
    procedure edFrameIntervalKeyPress(Sender: TObject; var Key: Char);
    procedure bFullScaleClick(Sender: TObject);
    procedure bMaxContrastClick(Sender: TObject);
    procedure edDisplayIntensityRangeKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure cbDisplayZoomChange(Sender: TObject);
    procedure bChangeExcitationClick(Sender: TObject);
    procedure rbEXCShutterOpenClick(Sender: TObject);
    procedure bFullFrameClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bSelectedRegionClick(Sender: TObject);
    procedure rbSingleWavelengthClick(Sender: TObject);
    procedure rbMultipleWavelengthsClick(Sender: TObject);
    procedure bStartStimulusClick(Sender: TObject);
    procedure edIdentKeyPress(Sender: TObject; var Key: Char);
    procedure cbStimProgramChange(Sender: TObject);
    procedure rbStimOffClick(Sender: TObject);
    procedure bStopStimulusClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure edBinFactorKeyPress(Sender: TObject; var Key: Char);
    procedure cbPaletteChange(Sender: TObject);
    procedure bUpdateWavelengthsClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure cbWavelengthChange(Sender: TObject);
    procedure rbContinuousRecordingClick(Sender: TObject);
    procedure rbTimeLapseClick(Sender: TObject);
    procedure bMarkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edTimeLapseIntervalKeyPress(Sender: TObject; var Key: Char);
    procedure edVHold0KeyPress(Sender: TObject; var Key: Char);
    procedure FormActivate(Sender: TObject);
    procedure rbEXCShutterClosedClick(Sender: TObject);
    procedure edMarkerKeyPress(Sender: TObject; var Key: Char);
    procedure cbCameraGainChange(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure sbContrastChange(Sender: TObject);
    procedure bEnterCCDAreaClick(Sender: TObject);
    procedure bSetLaserIntensityClick(Sender: TObject);
    procedure bSetSubFolderClick(Sender: TObject);
    procedure bAddROIClick(Sender: TObject);
    procedure bDeleteROIsClick(Sender: TObject);
    procedure lbImageCaptureShowSettingsClick(Sender: TObject);
    procedure lbDisplayShowSettingsClick(Sender: TObject);
    procedure lbShadeCorShowSettingsClick(Sender: TObject);
    procedure lbLightStimShowSettingsClick(Sender: TObject);
    procedure bAcquireBackgroundClick(Sender: TObject);
    procedure sbImageCaptureShowSettingsClick(Sender: TObject);
    procedure sbDisplayShowSettingsClick(Sender: TObject);
    procedure sbShadeCorShowSettingsClick(Sender: TObject);
    procedure sbLightStimShowSettingsClick(Sender: TObject);
    procedure cbSequenceChange(Sender: TObject);
    procedure ckAutoOptimiseClick(Sender: TObject);
    procedure ckContrast6SDOnlyClick(Sender: TObject);
    procedure ckChangeAllFrameTypesClick(Sender: TObject);
    procedure bStartPhotoStimulusClick(Sender: TObject);
    procedure bStopPhotoStimulusClick(Sender: TObject);
    procedure cbPhotoStimProgramChange(Sender: TObject);
    procedure ckStartStimOnRecordClick(Sender: TObject);
    procedure cbRecordingModeChange(Sender: TObject);
    procedure edBurstDurationKeyPress(Sender: TObject; var Key: Char);
    procedure edBurstIntervalKeyPress(Sender: TObject; var Key: Char);
    procedure ckExcitationOnWhenRecordingClick(Sender: TObject);
    procedure sbZPositionChange(Sender: TObject);
    procedure edZPositionKeyPress(Sender: TObject; var Key: Char);
    procedure ckZStackEnabledClick(Sender: TObject);
    procedure sbShowHideZStackSettingsClick(Sender: TObject);
    procedure edZStartPosKeyPress(Sender: TObject; var Key: Char);
    procedure edZStepSizeKeyPress(Sender: TObject; var Key: Char);
    procedure edZNumStepsKeyPress(Sender: TObject; var Key: Char);
    procedure ckSplitCCDImageClick(Sender: TObject);

  private
    { Private declarations }

    ADCDevice : SmallInt ;              // Analogue input interface device #1
    DeviceDACsInUse : Array[1..MaxDevices] of Boolean ; // DAC outputs of devices in use
    DeviceDIGsInUse : Array[1..MaxDevices] of Boolean ; // Digital outputs of devices in use

    BitMaps : Array[0..MaxFrameType] of TBitMap ;  // Image internal bitmaps
    Images : Array[0..MaxFrameType] of TImage ;    // Image display area list

    PBackgroundBufs : Array[0..MaxFrameType] of PIntArray ; // Pointer to background image buffers
    PSumBufs : Array[0..MaxFrameType] of PSingleArray ; // Pointer to background summation buffers
    ShadeCorNumFramesAveraged : Array[0..MaxFrameType] of Integer ;  // Background average counter
    KeepBackgroundBufs : Boolean ;

    FrameNum : Integer ;
    CameraStartFrame : Integer ;                       // First frame after camera start

    // Contains FrameCounter number of frame type on display
    //FrameDisplayed : Array[0..2*MaxFrameType] of Integer ;

    // Multi-wavelength sequence
    NumFramesPerWavelengthCycle : Integer ;
    FilterNums : Array[0..MaxLightSourceCycleLength-1] of Integer ;
    Wavelengths : Array[0..MaxLightSourceCycleLength-1] of Single ;
    Bandwidths : Array[0..MaxLightSourceCycleLength-1] of Single ;
    FractionalExposure : Array[0..MaxLightSourceCycleLength-1] of Single ;
    EMFilters : Array[0..MaxLightSourceCycleLength-1] of Integer ;
    FrameTypeCycleLength : Integer ;
    FrameTypeCycle : Array[0..MaxLightSourceCycleLength-1] of Integer ;
    FrameTypeCycleCounter : Integer ;
    LatestFrames : Array[0..MaxFrameType] of Integer ;
    //LatestFramesDisplayed : Array[0..MaxFrameType] of Boolean ;
    MaxFramesPerCycle : Integer ;  // Max. no. of frames per D/A update cycle
    NumFramesPerCCD : Integer ;
    CameraRestartRequired : Boolean ;    // Restart camera if TRUE

    // Image capture double buffer
    FirstFrameInLowerBuffer : Integer ;  // Start of lower half of buffer
    LastFrameInLowerBuffer : Integer ;   // End of lower half of buffer
    FirstFrameInUpperBuffer : Integer ;  // Start of upper half of buffer
    LastFrameInUpperBuffer : Integer ;   // End of upper half of buffer
    NumBytesPerHalfFrameBuffer : Integer ; // No. of bytes of half of buffer
    NumBytesPerFrame : Integer ;           // No. of bytes per image frame
    LowerBufferFilling : Boolean ;         // TRUE = Lower half of buffer filling
    LastFrameDisplayed : Integer ;            // Last frame # displayed
    ReplaceEmptyFlags : Boolean ;          // Buffer empty flag replacement request
    EmptyFlagsFirst : Integer ;            // First frame to have empty flag restored
    EmptyFlagsLast : Integer ;             // Last frame to have empty flag restored
    BufferOverFlowMessage : String ;       // Buffer overflow warning message
    TNoFrames : Single ;                   // Time with no frames from camera (s)
    TimeLapsePreserveFile : Boolean ;      // TRUE = close/open IDR file to preserve directory settings
    EDRFilePointer : Int64 ;               // EDR file pointer

    NumFramesDone : Integer ;              // No. of frames recorded so far in this rec. session
    NumFramesRequired : Integer ;          // No. of frames still to be recorded
    NumFramesTotal : Integer ;             // Total no. of frames since start of file
    FrameInterval : Single ;               // Selected inter-frame interval
    FrameRate : Single ;                   // Actual rate of frame acquisition

    // T.lapse + burst
    NumBurstFramesRequired : Integer ;     // No. of frames per burst
    NumBurstFramesDone : Integer ;         // No. of frames acquired in current burst
    StartBurstAtFrame : Integer ;          // Start burst at frame number
    BurstCounter : Integer ;               // No. of bursts collected so far
    BurstFileName : string ;
    BurstIlluminationOn : Boolean ;        // Burst illumination on flag

    RecordingMode : Integer ;              // Recording mode flag

    TimerProcBusy : Boolean ;              // TRUE = scheduled timer code executing
    InitialisationComplete : Boolean ;     // TRUE = formshow initialisations done
    FormClosing : Boolean ;                // TRUE = form is closing

    ContinuedRecording : Boolean ;         // TRUE = Continuation of previous recording

    FrameTypeToBeDisplayed : Integer ;    // Next type of frame to be displayed

    DisplayZoom : Single ;                // Display zoom factor (0.5,1.0.2.0)
    ADCRunning : Boolean ;                    // TRUE = A/D converter acquiring samples
    ADCNumSamplesInBuffer : Integer ;         // Total no. of samples in circular A/D buffer ADCBuf
    ADCNumSamplesInWriteBuffer : Integer ;    // No. of A/D samples in write buffer
    ADCNumWriteBuffers : Integer ;            // Total no. of write buffers in ADCBuf
    ADCWriteBuffer : Integer ;                // No. of next buffer to be written to file
    ADCActiveBuffer : Integer ;               // No. of sub-buffer being filled
    ADCBuf : PBig16BitArray ;                 // A/D sample input buffer pointer

    ADCEmptyPointer : Integer ;
    ADCOldestScan : Integer ;               //
    ADCLatestScan : Integer ;               // Index of latest available scan
    ADCMaxValue : Integer ;
    ADCMinValue : Integer ;

    FirstCall : Boolean ;
    FirstResize : Boolean ;
    FormResizeCounter : Integer ;

    DACUpdateInterval : Single ;               // D/A timing update interval (s)
    NumDACPointsPerCycle : Integer ;           // No. of D/A output time points in timing cycle buffer
    NumDACPointsPerFrame : Integer ;           // No. of D/A output time points per frame
    NumFramesPerCycle : Integer ;              // No. of camera frames in timing buffer cycle
    NumFramesPerZStep : Integer ;              // No. of camera frames per Z axis step
    NumFramesPerZStack : Integer ;             // No. of camera frams in Z axis stack
    NumFramesPerTimeLapseInterval : Integer ;  // No. frames per time lapse interval
    NumTimeLapseIntervalsPerCycle : Integer ;  // No. time lapse intervals in timing cycle buffer
    TimeLapseOpenShutterAtFrame : Integer ;    // Frame # within time lapse cycle to open shutter at

    // DAC output buffers
    DACBufs : Array[0..MaxDevices] of PBig16bitArray ;  // D/A output waveform buffer pointers

    // Digital output buffers
    DigBufs : Array[0..MaxDevices] of PBig32bitArray ;  // Digital output buffer pointers

    LoadNextProtocolFile : Boolean ;         // Load next protocol file from existing stim program
    RestartRecording : Boolean ;

    // Time lapse control variables
    pTimeLapseBuf : Pointer ;
    TimeLapseFrameCounter : Integer ;
    TimeLapseFrameInterval : Integer ;

    MoveXYStageAtFrame : Integer ;              // Move XY stage at the frame
    XYStageFileNames : Array[0..99] of String ; // XY stage file names
    XYStageLastFrame : Integer ;                // Last frame written to an XY file

    MousePosition : TPoint ;

    StimulusRequired : Boolean ;                 // V/Dig/ stimulus needed
    TReEnableStartStimulusButton : Single ;      // Re-enable Start Stimulus button at this time
    PhotoStimulusRequired :Boolean ;             // Photo-stimulus needed flag
    TReEnableStartPhotoStimulusButton : Single ; // Re-enable Start PhotoStim button at this time

    TStart : Cardinal ;
    FrameRateCounter : Integer ;
    TimerTickCount : Cardinal ;

    CaptureRegion : TRect ;
    FXOld : Integer ;
    FYOld : Integer ;
    FMoveCaptureRegion : Boolean ;

    ROIs : Array[0..MaxLiveROIs-1] of TLiveROI ;
    NumROIs : Integer ;
    LatestROIValue : Array[0..MaxFrameType] of Integer ;

    TimeNow : Integer ;
    TimeLast : Integer ;
    TimeDiff : Integer ;
    TimeDiffMax : Integer ;

    NumCloseClicks : Integer ;
    OptimiseContrastCount : Integer ;

    procedure StopRecording ;
    procedure FillBufferWithEmptyFlags( StartAt : Integer ; EndAt : Integer ) ;
    procedure InitialiseImage ;
    procedure SetImagePanels ;
    procedure NewDisplaySetup ;
    procedure StartADC ;
    procedure StartTimingCycle ;
    procedure StopTimingCycle ;
    Procedure UpdateADCDisplay ;

    procedure DisplayImage(
              StartAt : Integer ;
              FrameType : Integer ;
              var LUT : Array of Word ;
              BitMap : TBitMap ;
              Image : TImage
              ) ;
    procedure UpdateImage(
          FrameType : Integer ;        // Type of frame/excitation wavelength being displayed [In]
          var LUT : Array of Word ;    // Display look-up table [IN]
          BitMap : TBitMap ;           // Bit map to hold image (OUT)
          Image : TImage               // Image display component (OUT)
          ) ;


    procedure ReadROI(
              FrameNum : Integer ;
              FrameType : Integer ;
              pImageBuf : Pointer ) ;

    procedure DisplayCalibrationBar( BitMap : TBitMap ) ;
    procedure CalculateMaxContrast( FrameType : Integer ) ;

    procedure UpdateCameraStartWaveform ;
    procedure UpdateLightSource ;
    procedure UpdateStimulusWaveforms( StimulusEnabled : Boolean ;
                                       InitialiseBuffer : Boolean ) ;
    procedure UpdateVCommandWaveform ;
    procedure UpdateDigitalWaveform ;
    procedure UpdateExcitationWavelengths ;
    procedure UpdateLightSourceShutter ;
    procedure UpdatePhotoStimulus(StimulusEnabled : Boolean);
    procedure UpdateEMFilter ;

    procedure AddMarker( MarkerText : String ) ;
    procedure Wait( Delay : Single ) ;
    function FindTimingDevice : SmallInt ;
    function CheckRecPlotFrmExists : Boolean ;

    procedure SetDisplayIntensityRange(
              LoValue : Integer ;
              HiValue : Integer
              ) ;

  procedure ShowHideImageCaptureSettingsPanel ;
  procedure ShowHideDisplaySettingsPanel ;
  procedure ShowHideShadeCorSettingsPanel ;
  procedure ShowHideLightStimPage ;
  procedure ShowHideZStackSettings ;
  procedure ResizeControlPanel ;
  procedure DoShadingCorrection( FrameType : Integer ) ;
  procedure SmoothImage(
            PBuf : PIntArray ;  // Imagebuffer
            FrameWidth : Integer ; // Width of frame
            FrameHeight : Integer ; // Height of frame
            BlockSize : Integer    // Smoothing block size
            ) ;

  procedure CalculateCapacity ;
  function GetTimeLapseFrameInterval : Integer ;
  function GetNumFramesRequired : Integer ;
  procedure UpdateRecordingModePanels ;

  procedure DrawCaptureRegion(
            Canvas : TCanvas ;
            SelectedRect : TRect
            ) ;
  procedure DrawSquare(
            Canvas : TCanvas ;
            X : Integer ;
            Y : Integer ) ;
  function ExcitationOn( iFrame : Integer ) : boolean ;

  function AreImagesDark(
           pBuf : Pointer ;
           NumPixels : Integer ) : Boolean ;

  public
    { Public declarations }

    CameraRunning : Boolean ;              // TRUE = Camera is acquiring images
    TimeLapseMode : Boolean ;              // TRUE = recording in time lapse mode

    FrameWidth : Integer ;                    // Frame width in use
    FrameHeight : Integer ;                   // Frame height in use
    PFrameBuf : Pointer ;                     // Pointer to circular image capture buffer
    PDisplayBufs : Array[0..MaxFrameType] of PIntArray ; // Pointer to displayed image buffers
    FrameTypeCounter : Array[0..MaxFrameType] of Integer ;  // Frames acquired counter

    PWorkBuf : Pointer ;                      // Pointer to work buffer
    SelectedFrameType : Integer ;                        // Frame selected by user
    NumFramesInBuffer : Integer ;             // No. of frames in circular capture buffer
    NumPixelsPerFrame : Integer ;             // No. of pixels per frame
    NumBytesPerPixel : Integer ;              // No. of bytes per image pixel
    ByteImage : Boolean ;                     // TRUE = 1 byte/pixel image

    FrameTypes : Array[0..MaxFrameType] of string ;  // Frame labels
    NumFrameTypes : Integer ;                        // No. of frame types in use
    ImageAvailable : Boolean ;                       // Image is available in display buffer

    procedure UpdateHoldingVoltage ;
    procedure StartCamera ;
    procedure StopCamera ;
    procedure RestartCamera ;
    procedure UpdateStimProgramList ;
    procedure UpdatePhotoStimProgramList ;
    procedure StopCameraAndAnalogIO ;

  end;

var
  RecordFrm: TRecordFrm;

implementation

uses Main, mmsystem, pvcam, maths, SESCam, FileIOUnit,
  ViewUnit, AmpModule, SealTest , LogUnit,
  StimModule, RecPlotUnit, SetCCDReadoutUnit, SetLasersUnit , SnapUnit,
  PhotoStimModule, ZStageUnit, XYStageUnit;

const
    ByteLoValue = 0 ;
    ByteHiValue = 255 ;
    WordLoValue = 0 ;
    WordHiValue = 32767 ;
    EmptyFlag = 32767 ;
    NormaliseToMean = 0 ;
    NormaliseToMin = 1 ;
    NormaliseToMax = 2 ;
    NormaliseToZero = 3 ;
    BurstIlluminationDelay = 10.0 ;

type
    TMoveMode = (mvNone,mvLeftEdge,mvRightEdge,mvTopEdge,mvBottomEdge,
                 mvTopLeft,mvBottomLeft,mvTopRight,mvBottomRight,mvAll) ;

var
    MoveMode : TMoveMode ;


{$R *.DFM}

procedure TRecordFrm.FormCreate(Sender: TObject);
// ------------------------------------
// Initialisations when form is created
// ------------------------------------
var
     i : Integer ;
begin

     FrameInterval := 0.0 ;
     FrameRate := 0.0 ;
     ADCRunning := False ;
     CameraRunning := False ;
     RecordingMode := rmRecordingStopped ;

     // Displayed image storage buffer pointers
     for i := 0 to High(PDisplayBufs) do PDisplayBufs[i] := Nil ;
     for i := 0 to High(PBackgroundBufs) do PBackgroundBufs[i] := Nil ;
     for i := 0 to High(PSumBufs) do PSumBufs[i] := Nil ;

     PWorkBuf := Nil ;
     PTimeLapseBuf := Nil ;
     ADCBuf := Nil ;

     // D/A waveform buffers
     for i := 0 to High(DACBufs) do DACBufs[i] := Nil ;

     // Digital waveform buffers
     for i := 0 to High(DigBufs) do DigBufs[i] := Nil ;

     // Internal image bitmaps
     for i := 0 to High(BitMaps) do BitMaps[i] := Nil ;

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

     Timer.Enabled := False ;
     LoadNextProtocolFile := False ;
     RestartRecording := False ;
     ContinuedRecording := False ;

     end;


procedure TRecordFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
     s : string ;
begin

     CameraRestartRequired := False ;
     NumCloseClicks := 0 ;
     FirstResize := True ;
     FormResizeCounter := 1 ;
     InitialisationComplete := False ;
     FormClosing := False ;
     if not MainFrm.Cam1.CameraAvailable then begin
        Close ;
        Exit ;
        end ;

     // Display XY stage control
     if XYStageFrm.Available and (not XYStageFrm.Visible) then XYStageFrm.Show ;

     // Stop seal test (if it is running)
     if MainFrm.FormExists( 'SealTestFrm' ) then SealTestFrm.StopSealTest ;

     // Stop live imaging form (if it is running)
     if MainFrm.FormExists('SnapFrm')then SnapFrm.StopLiveImaging ;

     ckSplitCCDImage.Checked := MainFrm.SplitImage ;

     // Set form at top left of MDI window
     Top := 20 ;
     Left := 20 ;

     // Open time course plotting window
     CheckRecPlotFrmExists ;

     // Disable A/D input channels if no ADC
     if (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.ADCIn)) and
        (MainFrm.ADCNumChannels > 0) then begin
        ShowMessage( 'RECORD: A/D channel recording selected but no A/D converter available!') ;
        MainFrm.ADCNumChannels := 0 ;
        ADCDevice := 0 ;
        end
     else begin
        ADCDevice := LabIO.Resource[MainFrm.IOConfig.ADCIn].Device ;
        end ;

     // Inter-frame capture interval
     edFrameInterval.Value := MainFrm.Cam1.FrameInterval ;
     lbReadoutTime.Caption := format('Min.= %.3g ms',
                                      [SecsToMs*MainFrm.Cam1.ReadoutTime]) ;

     // Get list of available camera gains
     MainFrm.Cam1.GetCameraGainList( cbCameraGain.Items ) ;
     cbCameraGain.ItemIndex := Min(Max(MainFrm.Cam1.AmpGain,
                               0),cbCameraGain.Items.Count-1) ;

     bSelectedRegion.Enabled := MainFrm.Cam1.CCDRegionReadoutAvailable ;

     // Interval between time lapse snaps

     cbRecordingMode.Clear ;
     cbRecordingMode.Items.Add('Continuous') ;
     cbRecordingMode.Items.Add('Time Lapse') ;
     cbRecordingMode.Items.Add('T. Lapse + Burst') ;
     cbRecordingMode.ItemIndex := MainFrm.RecordingMode ;
     UpdateRecordingModePanels ;

     edTimeLapseInterval.Value :=  MainFrm.TimeLapseInterval ;
     edBurstDuration.Value := MainFrm.BurstDuration ;
     edBurstInterval.Value := MainFrm.BurstInterval ;

     // No.frames per recording sequence
     edRecordingPeriod.Value :=  MainFrm.RecordingPeriod ;

     // Set check box for automatically starting stimulation when recording started
     ckStartStimOnRecord.Checked := MainFrm.StartStimOnRecord ;

     // Set check box for automatically turning excitation on during record and off on completion.
     ckExcitationOnWhenRecording.Checked := MainFrm.ExcitationOnWhenRecording ;

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

     // Experiment ident line
     edIdent.Text := MainFrm.IDRFile.Ident ;

    // Number of excitation wavelengths in use
    // (Each displayed as a separate image)
    if MainFrm.EXCSingleWaveLength then begin
       NumFrameTypes := 1 ;
       end
    else begin
       NumFrameTypes := Max(MainFrm.EXCNumWavelengths[MainFrm.EXCSequenceNum],1) ;
       end ;

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

     cbDisplayZoom.ItemIndex := Min(Max(MainFrm.DisplayZoomIndex,
                                 0),cbDisplayZoom.Items.Count-1) ; ;
     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])*0.001 ;

     sbXScroll.Position := 0 ;
     sbYScroll.Position := 0 ;

     // Excitation wavelength
     rbSingleWavelength.Checked := MainFrm.EXCSingleWavelength ;
     rbMultipleWavelengths.Checked := not rbSingleWavelength.Checked ;
     rbSpectrum.Checked := False ;

     // Create list of excitation wavelengths
     cbWaveLength.Clear ;

     for i := 0 to High(MainFrm.EXCWavelengths) do begin
         s := format('EX%d ',[MainFrm.EXCWavelengths[i].Centre]) ;
         if MainFrm.EXCWavelengths[i].Width <> 0.0 then begin
            s := s + format('(%d) ',[MainFrm.EXCWavelengths[i].Width]) ;
            end;
         if MainFrm.EXCWavelengths[i].EMName <> '' then begin
            s := s + 'EM' + MainFrm.EXCWavelengths[i].EMName ;
            end;
         cbWaveLength.Items.Add( s ) ;
         end ;
     cbWaveLength.ItemIndex := Min(Max(MainFrm.EXCSingleWavelengthNum,
                               0),cbWaveLength.Items.Count-1) ;

     // Create list of multi-wavelength sequences
     cbSequence.Clear ;
     for i := 0 to High(MainFrm.EXCSequenceName) do begin
         cbSequence.Items.Add( MainFrm.EXCSequenceName[i] ) ;
         end ;
     cbSequence.ItemIndex := Min( Max(MainFrm.EXCSequenceNum,
                                  0),cbSequence.Items.Count-1) ;

     // Excitation light shutter state closed
     rbEXCShutterOpen.Checked := False ;
     rbEXCShutterClosed.Checked := True ;

     // Default frame type cycle (if no light source)
     FrameTypeCycle[0] := 0 ;
     FrameTypeCycleLength := 1 ;
     FrameTypeCycleCounter := 0 ;

     // Load list of stimulus programs
     UpdateStimProgramList ;

     // Set holding voltage
     edVHold0.Value := MainFrm.VCommand[0].HoldingVoltage ;
     edVHold1.Value := MainFrm.VCommand[1].HoldingVoltage ;

     VHold0Panel.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[0]) ;
     VHold1Panel.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[1]) ;

     // Load list of photo-stimulus programs
     UpdatePhotoStimProgramList ;

     if ((MainFrm.ROIX = 0) and (MainFrm.ROIY = 0)) then begin
        ROIs[0].X := MainFrm.Cam1.FrameWidth div 2 ;
        ROIs[0].Y := MainFrm.Cam1.FrameHeight div 2 ;
        end
     else begin
        ROIs[0].X := Max(Min(MainFrm.ROIX,MainFrm.Cam1.FrameWidth-1),0) ;
        ROIs[0].Y := Max(Min(MainFrm.ROIY,MainFrm.Cam1.FrameHeight-1),0) ;
        end ;
     NumROIs := 1 ;

     MainFrm.Recording := False ;
     bRecord.Enabled := True ;
     bStop.Enabled := False ;
     TimerProcBusy := False ;
     ImageAvailable := False ;
     MoveMode := mvNone ;
     TimeLapseMode := False ;
     bMark.Enabled := False ;

     // Display contrast optimisation
     OptimiseContrastCount := NumFrameTypes*2 ;
     ckAutoOptimise.Checked := MainFrm.ContrastAutoOptimise ;
     ckChangeAllFrameTypes.Checked := MainFrm.ContrastChangeAllFrameTypes ;
     ckContrast6SDOnly.Checked := MainFrm.Contrast6SD ;

     // Shading correction
     cbShadeCorNormalisation.Clear ;
     cbShadeCorNormalisation.Items.Add('Mean') ;
     cbShadeCorNormalisation.Items.Add('Min.') ;
     cbShadeCorNormalisation.Items.Add('Max.') ;
     cbShadeCorNormalisation.Items.Add('Zero') ;
     cbShadeCorNormalisation.ItemIndex := 0 ;
     KeepBackgroundBufs := False ;

     // ZStage
     ZStageGrp.Visible := ZStage.Available ;
     edZPosition.Value := ZStage.Position ;
     edZStartPos.Value := ZStage.StartAt ;
     edZStepSize.Value := ZStage.StepSize ;
     edZNumSteps.Value := ZStage.NumSteps ;
     ckZStackEnabled.Checked := ZStage.StackEnabled and ZStage.Available ;

     // Select which panel settings are to be visible when form opens
     sbImageCaptureShowSettings.Down := False ;
     ShowHideImageCaptureSettingsPanel ;
     sbDisplayShowSettings.Down := True ;
     ShowHideDisplaySettingsPanel ;
     sbShadeCorShowSettings.Down := False ;
     ShowHideShadeCorSettingsPanel ;
     sbLightStimShowSettings.Down := True ;
     ShowHideLightStimPage ;
     sbShowHideZStackSettings.Down := False ;
     ShowHideZStackSettings ;

     ResizeControlPanel ;

     MainFrm.StatusBar.SimpleText := ' Camera initialised' ;

     InitialisationComplete := True ;

     ClientHeight := ControlGrp.Top + ControlGrp.Height + 5 ;

     // Initialise time course display window
     if CheckRecPlotFrmExists then begin
        NewDisplaySetup ;
        end ;

     // Set wavelength control pattern
     UpdateExcitationWavelengths ;

     // Start acquiring images
     StartCamera ;

     // Start schedule events timer (runs at 50 ms intervals)
     Timer.Enabled := True ;

     end;


procedure TRecordFrm.StartADC ;
// ---------------------------------------
// Start A/D sampling into circular buffer
// ---------------------------------------
var
     i,ch : Integer ;
begin

     // Exit if no A/D converter hardware or not required
     if (ADCDevice < 1) or (MainFrm.ADCNumChannels <= 0) then Exit ;

     // Stop A/D if it is running
     LabIO.StopADC(ADCDevice) ;

     // Update private variables
     ADCMaxValue := LabIO.ADCMaxValue[ADCDevice] ;
     ADCMinValue := LabIO.ADCMinValue[ADCDevice] ;

     // Update channel scale factors
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin

         MainFrm.ADCChannel[ch].ChannelOffset := LabIO.GetChannelOffset(ch,MainFrm.ADCNumChannels) ;

         Amplifier.GetChannelSettings( ch, MainFrm.ADCChannel[ch].ADCName,
                                           MainFrm.ADCChannel[ch].ADCUnits,
                                           MainFrm.ADCChannel[ch].ADCCalibrationFactor,
                                           MainFrm.ADCChannel[ch].ADCAmplifierGain ) ;
          end ;

     MainFrm.IDRFile.UpdateChannelScalingFactors( MainFrm.ADCChannel,
                                                  MainFrm.ADCNumChannels,
                                                  MainFrm.ADCVoltageRange,
                                                  LabIO.ADCMaxValue[ADCDevice] ) ;

     // Determine size and number of A/D write buffers within circular ADC buffer

     ADCNumSamplesInWriteBuffer := Round( cADCWriteBufferDuration /
                                          DACUpdateInterval)*MainFrm.ADCNumChannels ;
     ADCNumSamplesInWriteBuffer := Max( ADCNumSamplesInWriteBuffer,MainFrm.ADCNumChannels*8 ) ;
     //ADCNumSamplesInWriteBuffer := Min( ADCNumSamplesInWriteBuffer,cMaxSamplesInADCBuffer div 2 ) ;
     ADCNumSamplesInWriteBuffer := (ADCNumSamplesInWriteBuffer div MainFrm.ADCNumChannels)
                                   * MainFrm.ADCNumChannels ;
     ADCNumWriteBuffers := 2 ;
     ADCNumSamplesInBuffer := ADCNumWriteBuffers*ADCNumSamplesInWriteBuffer ;
     if ADCBuf <> Nil then FreeMem(ADCBuf) ;
     GetMem(ADCBuf,ADCNumSamplesInBuffer*2) ;

     // Fill circular buffer with empty flags
     i := 0 ;
     while i < ADCNumSamplesInBuffer do begin
         ADCBuf^[i] := EmptyFlag ;
         Inc(i) ;
         ADCBuf^[i] := -EmptyFlag ;
         Inc(i) ;
         end ;

     // Start A/D sampling
     LabIO.ADCToMemoryExtScan( ADCDevice,
                               ADCBuf^,
                               MainFrm.ADCNumChannels,
                               ADCNumSamplesInBuffer div MainFrm.ADCNumChannels,
                               MainFrm.ADCVoltageRange,
                               True,
                               DACUpdateInterval,
                               FindTimingDevice,
                               DeviceDACsInUse[FindTimingDevice] ) ;

     // Initialise circular A/D and display buffers
     //ADCPointer := MainFrm.ADCNumChannels ;
     ADCActiveBuffer := 0 ;
     ADCWriteBuffer := 0 ;
     ADCEmptyPointer := MainFrm.ADCNumChannels*4 ;
     ADCOldestScan := MainFrm.ADCNumChannels*3 ;

     // Initialise (and erase) display
     if (NumFramesDone = 0) then begin
        RecPlotFrm.ADCInitialiseDisplay( ADCBuf,
                                         LabIO.ADCMaxValue[ADCDevice],
                                         DACUpdateInterval
                                         ) ;
        end ;

     ADCRunning := True ;
     FirstCall := True ;

     end ;


function TRecordFrm.FindTimingDevice : Smallint ;
// ----------------------------------------------
// Find NI device to use as ADC/DAC timing source
// ----------------------------------------------
var
    Dev : SmallInt ;
begin
     Result := 1 ;
     // Use last DAC device in use as timing device
     for Dev := 1 to LabIO.NumDevices do
         if DeviceDACsInUse[Dev] or DeviceDIGsInUse[Dev] then Result := Dev ;

     end ;


procedure TRecordFrm.StartCamera ;
// -------------------
// Start image capture
// -------------------
var
     i : Integer ;
begin

   outputdebugstring(pchar('camera started'));

   // Don't start if called before initialisations in FormShow complete
   if not InitialisationComplete then Exit ;
   if FormClosing then Exit ;

   // Stop camera (if it is running)
   StopCamera ;

   // If in split image mode, ensure full height of CCD in use and is divisable by 2
   if ckSplitCCDImage.Checked then begin
      i := 1 ;
      repeat
         MainFrm.Cam1.SetCCDArea( MainFrm.Cam1.FrameLeft,
                                  0,
                                  MainFrm.Cam1.FrameRight,
                                  MainFrm.Cam1.FrameHeightMax-i);
         Inc(i) ;
         until ((MainFrm.Cam1.FrameHeight mod 2) = 0) or (i>16) ;
      end;

   Timer.Enabled := False ;

   MainFrm.StatusBar.SimpleText := 'Wait ... Starting camera' ;

   CameraRunning := False ;

   // Set camera trigger mode
   if MainFrm.IOConfig.CameraStart > MaxResources then begin
      MainFrm.Cam1.TriggerMode := CamFreeRun ;
      end
   else begin
      if MainFrm.BulbExposureMode then MainFrm.Cam1.TriggerMode := CamBulbMode
                                  else MainFrm.Cam1.TriggerMode := CamExtTrigger ;
   end;

   // Set camera gain
   MainFrm.Cam1.AmpGain := cbCameraGain.ItemIndex ;

   // In continuous mode if Z stack enabled and exposure to be terminated when Z step starts
   // add additional camera reaout time
   if (cbRecordingMode.ItemIndex = rmContinuous) and
      ZStage.EndExposureAtStep and ZStage.Available and ckZStackEnabled.Checked then begin
      MainFrm.Cam1.ShortenExposureBy := ZStage.ShortenExposureBy ;
      end
   else begin
      MainFrm.Cam1.ShortenExposureBy := 0.0 ;
      end;

   // If emission filter in use shorten exposure by emission filter change time
   if MainFrm.IOResourceAvailable(MainFrm.IOConfig.EMFilterStart) then begin
      MainFrm.Cam1.ShortenExposureBy := Max(MainFrm.Cam1.ShortenExposureBy,LightSource.EMFilterChangeTime) ;
      end;

   // Set exposure interval (ensure it is a multiple of A/D & D/A timing interval)
   MainFrm.Cam1.FrameInterval := Round(edFrameInterval.Value/MainFrm.ADCScanInterval)*MainFrm.ADCScanInterval ;
   edFrameInterval.Value := MainFrm.Cam1.FrameInterval ;

   // Set image/display panels
   MainFrm.StatusBar.SimpleText := 'Wait ... Initialising image' ;
   InitialiseImage ;

   // Start frame capture
   MainFrm.StatusBar.SimpleText := 'Wait ... Starting camera' ;
   if not MainFrm.Cam1.StartCapture then begin
      MainFrm.StatusBar.SimpleText := 'Aborted ... Unable to start camera (not enough memory)!' ;
      Exit ;
      end;

   // Ensure buffer is filled with empty flags
   FillBufferWithEmptyFlags( 0, NumFramesInBuffer-1 ) ;

   // Update exposure interval in case camera has changed it
   edFrameInterval.Value := MainFrm.Cam1.FrameInterval ;
   FrameInterval := MainFrm.Cam1.FrameInterval ;

   // Start timing pulses cycle
   MainFrm.StatusBar.SimpleText := 'Wait ... Starting timing' ;
   StartTimingCycle ;

   // Initialise A/D display
   NewDisplaySetup ;

   TStart := TimeGetTime ;
   FrameRateCounter := 0 ;
   TimeLapseFrameCounter := 0 ;
   TNoFrames := 0.0 ;
   TimeLapsePreserveFile := False ;

   Timer.Enabled := True ;
   TimerTickCount := 0 ;

   // Report minimum readout time
   lbReadoutTime.Caption := format('Min.= %.3g ms',
                            [SecsToMs*MainFrm.Cam1.ReadoutTime]) ;

   // Initialise frames counters
   for i := 0 to High(LatestFrames) do begin
       FrameTypeCounter[i] := 0 ;
       LatestFrames[i] := -1 ;             // Index into frame buffer
       LatestROIValue[i] := 0 ;
       end ;
   FrameTypeToBeDisplayed := 0 ;

   MainFrm.StatusBar.SimpleText := 'Camera started ' ;

   CameraRestartRequired := False ;
   CameraRunning := True ;
   ReplaceEmptyFlags := False ;
   BufferOverFlowMessage := '' ;

    TimeNow := 0 ;
    TimeLast := 0 ;
    TimeDiff := 0 ;
    TimeDiffMax := 0 ;

   end ;


procedure TRecordFrm.StopCamera ;
// -------------------
// Start image capture
// -------------------
begin

     // Exit if camera not running
     if not CameraRunning then Exit ;
   outputdebugstring(pchar('camera stopped'));

     // Stop camera
     MainFrm.Cam1.StopCapture ;
     CameraRunning := False ;
     Timer.Enabled := False ;

     // Stop timing pulses
     StopTimingCycle ;

     MainFrm.StatusBar.SimpleText := ' Camera stopped ' ;

     CameraRunning := False ;
     Timer.Enabled := True ;
     ImageAvailable := False ;

     end ;


procedure TRecordFrm.RestartCamera ;
// ----------------
// Re-start camera
// ----------------
var
    i, OldIndex : Integer ;
    s : string ;
begin

    // If camera is not running then leave it inactive
    if not MainFrm.Cam1.CameraActive then Exit ;

    StopCamera ;

    // Update excitation wavelength list
    OldIndex := cbWaveLength.ItemIndex ;
    cbWaveLength.Clear ;
    for i := 0 to High(MainFrm.EXCWavelengths) do begin
        s := format('EX%d ',[MainFrm.EXCWavelengths[i].Centre]) ;
        if MainFrm.EXCWavelengths[i].Width <> 0.0 then begin
            s := s + format('(%d) ',[MainFrm.EXCWavelengths[i].Width]) ;
            end;
        if MainFrm.EXCWavelengths[i].EMName <> '' then begin
            s := s + 'EM' + MainFrm.EXCWavelengths[i].EMName ;
            end;
        cbWaveLength.Items.Add( s ) ;
        end ;
    cbWaveLength.ItemIndex := OldIndex ;

    // Update multi-wavelength sequence list
    OldIndex := cbSequence.ItemIndex ;
       cbSequence.Clear ;
       for i := 0 to High(MainFrm.EXCSequenceName) do begin
           cbSequence.Items.Add( MainFrm.EXCSequenceName[i] ) ;
           end ;
    cbSequence.ItemIndex := OldIndex ;

    UpdateExcitationWavelengths ;

    StartCamera ;
    end ;


procedure TRecordFrm.InitialiseImage ;
// ------------------------------------------------------
// Re-initialise size of memory buffers and image bitmaps
// ------------------------------------------------------
const
    {$IFDEF WIN32}
      MaxBufferSize = 500000000 ;
    {$ELSE}
      MaxBufferSize = 2000000000 ;
    {$IFEND}

var
     i,FT : Integer ;
     WVNum : Integer ;
     CCDName : Array[0..1] of string ;
     FrameBufferMultiple : Integer ;
begin

    // Select split CCD option
    if MainFrm.SplitImage then begin
       NumFramesPerCCD := 2 ;
       CCDName[0] := MainFrm.SplitImageName[0] ;
       CCDName[1] := MainFrm.SplitImageName[1] ;
       end
    else begin
       NumFramesPerCCD := 1 ;
       CCDName[0] := '' ;
       CCDName[1] := '' ;
       end;

    // No. of pixels per frame
    FrameWidth := MainFrm.Cam1.FrameWidth;
    FrameHeight := MainFrm.Cam1.FrameHeight  div NumFramesPerCCD ;
    NumPixelsPerFrame := FrameWidth*FrameHeight ;

    // Number of excitation wavelengths in use
    // (Each displayed as a separate image)
    // When split CCD selected, no. of frames is doubled
    if MainFrm.EXCSingleWaveLength then NumFrameTypes := 1
    else NumFrameTypes := Max(MainFrm.EXCNumWavelengths[MainFrm.EXCSequenceNum],1) ;
    NumFrameTypes := NumFrameTypes*NumFramesPerCCD ;

     // Dispose of existing display buffers and create new ones
     for i := 0 to High(PDisplayBufs) do if PDisplayBufs[i] <> Nil then begin
         Try
           FreeMem(PDisplayBufs[i]) ;
           PDisplayBufs[i] := Nil ;
         except
           PDisplayBufs[i] := Nil ;
           end ;
         end ;
     for i := 0 to NumFrameTypes-1 do
         GetMem( PDisplayBufs[i],NumPixelsPerFrame*SizeOf(Integer) ) ;

     // Dispose of existing background buffers and create new ones
     if not KeepBackgroundBufs then begin
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
        KeepBackgroundBufs := False ;
        end ;

     // Dispose of existing summation buffers and create new ones
     for i := 0 to High(PSumBufs) do if PSumBufs[i] <> Nil then begin
         Try
           FreeMem(PSumBufs[i]) ;
           PSumBufs[i] := Nil ;
         except
           PSumBufs[i] := Nil ;
           end ;
         end ;
     for i := 0 to NumFrameTypes-1 do
         GetMem( PSumBufs[i],NumPixelsPerFrame*SizeOf(Single) ) ;

     // Create work buffer

     try
     if PWorkBuf <> Nil then begin
        FreeMem( PWorkBuf ) ;
        PWorkBuf := Nil ;
        end
     except
        PWorkBuf := Nil ;
        end ;
     GetMem( PWorkBuf, NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel ) ;

     // Set size and location of image display panels
     SetImagePanels ;

     CCDAreaGrp.Caption := format( ' CCD Area (%d x %d) ',
                         [MainFrm.Cam1.FrameWidth,MainFrm.Cam1.FrameHeight] ) ;

     // Create display labels for each frame
     if MainFrm.EXCSingleWaveLength then begin
         for i := 0 to NumFrameTypes-1 do begin
             FrameTypes[i] := CCDName[i mod NumFramesPerCCD] + ' ' + cbWavelength.Items[MainFrm.EXCSingleWavelengthNum] ;
             end ;
         end
     else begin
         for i := 0 to NumFrameTypes-1 do begin
             WVNum := MainFrm.EXCSequence[i div NumFramesPerCCD,MainFrm.EXCSequenceNum].WavelengthNum ;
             FrameTypes[i] := CCDName[i mod NumFramesPerCCD] + ' ' + cbWavelength.Items[WvNum] ;
             end ;
         end ;

     // Indicate selected frame type selected for contrast update
     DisplayGrp.Caption := ' Contrast ' + FrameTypes[SelectedFrameType] + ' ' ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[SelectedFrameType],
                               MainFrm.GreyHi[SelectedFrameType] ) ;

     // Update display look up tables
     for i := 0 to NumFrameTypes-1 do MainFrm.UpdateLUT( i, MainFrm.Cam1.GreyLevelMax );

     // Determine number of frame within circular buffer
     FrameBufferMultiple := 2 ;

     case MainFrm.CameraType of
        RS_PVCAM_PENTAMAX : Begin
          // Pentamax has limited buffer size
          NumFramesInBuffer :=  (4194304 div MainFrm.Cam1.NumBytesPerFrame)-1 ;
          NumFramesInBuffer := Min( NumFramesInBuffer, 36) ;
          FrameBufferMultiple := 2 ;
          end ;

        PIXELFLY : begin
          NumFramesInBuffer := 8 ;
          FrameBufferMultiple := 2 ;
          end ;

        Andor : begin
           NumFramesInBuffer := Max(Round(5.0/edFrameInterval.Value),8) ;
           NumFramesInBuffer := Min( NumFramesInBuffer,
                                     MaxBufferSize div Int64(MainFrm.Cam1.NumBytesPerFrame));
           FrameBufferMultiple := 2 ;
           end ;

        AndorSDK3 : begin
           NumFramesInBuffer := Round(5.0/edFrameInterval.Value) ;
           NumFramesInBuffer := Min( NumFramesInBuffer,
                                     MaxBufferSize div Int64(MainFrm.Cam1.NumBytesPerFrame));
           NumFramesInBuffer := Max(NumFramesInBuffer,8) ;
           FrameBufferMultiple := 2 ;
           end ;

        RS_PVCAM : begin
           NumFramesInBuffer := Round(5.0/edFrameInterval.Value) ;
           NumFramesInBuffer := Min( NumFramesInBuffer,
                                     MaxBufferSize div Int64(MainFrm.Cam1.NumBytesPerFrame));
           NumFramesInBuffer := Max(NumFramesInBuffer,8) ;
           FrameBufferMultiple := 2*MainFrm.Cam1.BinFactor*MainFrm.Cam1.BinFactor ;
           end ;

        DCAM : begin
           NumFramesInBuffer := Round(5.0/edFrameInterval.Value) ;
           NumFramesInBuffer := Min( NumFramesInBuffer,
                                     MaxBufferSize div Int64(MainFrm.Cam1.NumBytesPerFrame));
           NumFramesInBuffer := Max(NumFramesInBuffer,8) ;
           FrameBufferMultiple := 2 ;
           end ;

        IMAQ : begin
           NumFramesInBuffer :=  (20000000 div MainFrm.Cam1.NumBytesPerFrame)-1 ;
           FrameBufferMultiple := 2 ;
           end ;

        IMAQDX : begin
           NumFramesInBuffer := Round(5.0/edFrameInterval.Value) ;
           NumFramesInBuffer := Min( NumFramesInBuffer,
                                     MaxBufferSize div Int64(MainFrm.Cam1.NumBytesPerFrame));
           NumFramesInBuffer := Max(NumFramesInBuffer,8) ;
           FrameBufferMultiple := 2 ;
           end ;

        DTOL : begin
           NumFramesInBuffer := MainFrm.Cam1.MaxFramesInBuffer ;
           FrameBufferMultiple := 2 ;
           end ;

        QCAM : begin
           NumFramesInBuffer := MainFrm.Cam1.MaxFramesInBuffer ;
           FrameBufferMultiple := 2 ;
           end ;

        Thorlabs : begin
           NumFramesInBuffer := Min( (Round(5.0/edFrameInterval.Value) div 2)*2,
                                      (MaxBufferSize div (NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel))-1) ;
           FrameBufferMultiple := 2 ;
           end ;


        else begin
           NumFramesInBuffer := 32 ;
           FrameBufferMultiple := 2 ;
           end ;
        end ;

     // Keep duration of frames in buffer to less than 8s
     NumFramesInBuffer := Min( NumFramesInBuffer, Round(8.0/MainFrm.Cam1.FrameInterval) ) ;
     // Ensure buffer is divisible by required factor
     NumFramesInBuffer := Max(NumFramesInBuffer div FrameBufferMultiple,1)*FrameBufferMultiple ;

     // Set camera buffer
     MainFrm.Cam1.NumFramesInBuffer := NumFramesInBuffer ;
     NumFramesInBuffer := MainFrm.Cam1.NumFramesInBuffer*NumFramesPerCCD ;

     FirstFrameInLowerBuffer := 0 ;
     LastFrameInLowerBuffer := (NumFramesInBuffer div 2) - 1 ;
     FirstFrameInUpperBuffer := LastFrameInLowerBuffer + 1 ;
     LastFrameInUpperBuffer := NumFramesInBuffer  - 1 ;

//     NumPixelsPerFrame := MainFrm.Cam1.FrameWidth*MainFrm.Cam1.FrameHeight ;
     NumBytesPerFrame := NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel ;
     NumBytesPerHalfFrameBuffer := (NumFramesInBuffer div 2)*MainFrm.Cam1.NumBytesPerPixel*NumPixelsPerFrame ;

     //MainFrm.Cam1.FrameRight := MainFrm.Cam1.FrameRight ;
     MainFrm.Cam1.GetFrameBufferPointer( PFrameBuf ) ;

     // Set byte/word image flag
     NumBytesPerPixel := MainFrm.Cam1.NumBytesPerPixel ;
     if MainFrm.Cam1.NumBytesPerPixel = 1 then ByteImage := True
                                          else ByteImage := False ;

     // Add empty flag values to each frame
     FrameTypeCycleCounter := 0 ;
     FillBufferWithEmptyFlags( 0, NumFramesInBuffer-1 ) ;
     LowerBufferFilling := True ;
     ReplaceEmptyFlags := False ;

     // Initial setting of last frame displayed
     FrameNum := 0 ; // NumFramesInBuffer - 1 ;
     // Type of frame to be displayed
     FrameTypeToBeDisplayed := 0 ;

     edBinFactor.Value := MainFrm.Cam1.BinFactor ;

     // Pixel intensity readout cursor
     ROIs[0].X := Min(Max(ROIs[0].X,0),FrameWidth-1) ;
     ROIs[0].Y := Min(Max(ROIs[0].Y,0),FrameHeight-1) ;

     end ;


procedure TRecordFrm.SetImagePanels ;
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

     if NumFrameTypes <= 0 then Exit ;
     if cbDisplayZoom.Items.Count <= 0 then Exit ;

     // Dispose of existing bit maps
     for i := 0 to High(BitMaps) do if BitMaps[i] <> Nil then begin
         BitMaps[i].Free  ;
         BitMaps[i] := Nil ;
         end ;
     // Create new ones
     for i := 0 to NumFrameTypes-1 do BitMaps[i] := TBitMap.Create ;

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
     if cbDisplayZoom.ItemIndex < 0 then cbDisplayZoom.ItemIndex := 0 ;
     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])*0.001 ;

     // Determine number of image columns and rows
     ImageRows := Round(Sqrt(NumFrameTypes)) ;
     ImageColumns := ImageRows ;
     if (ImageRows*ImageColumns) < NumFrameTypes then Inc(ImageRows) ;

     ImageGrp.ClientWidth :=  Max( ClientWidth - ImageGrp.Left - 5, 2) ;
     ImageGrp.ClientHeight :=  Max( ClientHeight - ImageGrp.Top - 5, 2) ;

     ROIPanel.Top := ImageGrp.ClientHeight - ROIPanel.Height - 2 ;

     ImageAreaWidth := Max( ImageGrp.ClientWidth - sbYScroll.Width
                            - ((ImageColumns+1)*MarginPixels),2) ;
     ImageAreaHeight := Max( ROIPanel.Top - sbXScroll.Height - cbDisplayZoom.Height - cbDisplayZoom.Top - 2
                             - ((ImageRows+1)*MarginPixels),2) ;

     BitMaps[0].Width := Max(Min( ImageAreaWidth div ImageColumns,
                                  Round(FrameWidth*DisplayZoom)),2) ;
     BitMaps[0].Height := Max(Min( ImageAreaHeight div ImageRows,
                                   Round(FrameHeight*DisplayZoom)),2) ;

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
     ROIPanel.Top := sbXScroll.Top + sbXScroll.Height + 2 ;

     sbYScroll.Left := RightEdge ;
     sbYScroll.Top := Images[0].Top ;
     sbYScroll.Height := BottomEdge - sbYScroll.Top ;

     // Image scroll bar range
     sbXScroll.Max := Max(FrameWidth - Round(Images[0].Width/DisplayZoom),1);
     sbYScroll.Max := Max(FrameHeight - Round(Images[0].Height/DisplayZoom),1);

     CaptureRegion.Left := 0 ;
     CaptureRegion.Right := BitMaps[0].Width - 1 ;
     CaptureRegion.Top := 0 ;
     CaptureRegion.Bottom := BitMaps[0].Height - 1 ;
     FMoveCaptureRegion := False ;

     end ;


procedure TRecordFrm.SetDisplayIntensityRange(
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


procedure TRecordFrm.FillBufferWithEmptyFlags(
          StartAt : Integer ;
          EndAt : Integer ) ;
// ----------------------------------------------
// Add empty flags to start/end of each frame in buffer
// ----------------------------------------------
var
    i,iFlag : Integer ;
begin

     // Get buffer pointer
     MainFrm.Cam1.GetFrameBufferPointer( PFrameBuf ) ;
     if PFrameBuf = Nil then Exit ;

     for i := StartAt to EndAt do begin
         iFlag := i*NumPixelsPerFrame ;
         if ByteImage then begin
            pByteArray(PFrameBuf)^[iFlag] := ByteLoValue ;
            pByteArray(PFrameBuf)^[iFlag+1] := ByteHiValue ;
            pByteArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-2] := ByteLoValue ;
            pByteArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-1] := ByteHiValue ;
            end
         else begin
            pBigWordArray(PFrameBuf)^[iFlag] := WordLoValue ;
            pBigWordArray(PFrameBuf)^[iFlag+1] := WordHiValue ;
            pBigWordArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-2] := WordLoValue ;
            pBigWordArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-1] := WordHiValue ;
            end ;
         end ;

     end ;


procedure TRecordFrm.StartTimingCycle ;
// ----------------------------------------------------------
// Start digital pulse cycle which controls instrument timing
// ----------------------------------------------------------

var

    Device : SmallInt ;          // Interface device #
    DACValue : Array[0..MaxDACs-1] of Integer ;
    MaxDACs : Integer ;            // Max. no. of DAC outputs per board in use
    i,j,ch : Integer ;
    NumFramesPerSpectrum : Integer ;
    FramesDivFactor : Integer ;
    NumSlowFrames : Integer ;
    StimDuration : Single ;
    RepeatedStimulus : Boolean ;
    EnableZStack : Boolean ;
begin

    // Default frame type cycle (if no light source)
    FrameTypeCycleLength := NumFramesPerCCD ;
    for i := 0 to FrameTypeCycleLength-1 do FrameTypeCycle[i] := i ;
    FrameTypeCycleCounter := 0 ;

    if LabIO.NumDevices <= 0 then Exit ;

    // Stop all DAC output
    for Device := 1 to LabIO.NumDevices do begin
        // Clear device in use flag
        DeviceDACsInUse[Device] := False ;
        DeviceDIGsInUse[Device] := False ;
        // Stop D/A output on device
        LabIO.StopDAC(Device) ;
        LabIO.StopDIG(Device) ;
        end ;

    // Delay needed to ensure DACs are stopped before starting
    // (Don't know exactly why!!!)
    Wait(0.1) ;

     // Time interval between DAC updates
     DACUpdateInterval := Max(MainFrm.ADCScanInterval,LabIO.DACMinUpdateInterval) ;

     // No. of DAC time points per camera frame
     NumDACPointsPerFrame := Round(MainFrm.Cam1.FrameInterval/DACUpdateInterval) ;

     // Max. number of frames which can fit into allowed DAC buffer
     MaxDACs := 1 ;
     for Device := 1 to LabIO.NumDevices do MaxDACs := Max(LabIO.NumDACs[Device],MaxDACs) ;
     MaxFramesPerCycle := cMaxBytesInDACBuffer div (2*MaxDACs*NumDACPointsPerFrame) ;

     // No. of frames per timing cycle to be used
     if rbSingleWavelength.Checked then NumFramesPerCycle := 2
     else if rbMultipleWavelengths.Checked then NumFramesPerCycle := Max(NumFrameTypes,2)
     else begin
        // Spectrum mode
        NumFramesPerCycle := Round( (MainFrm.EXCSpectrumEndWavelength -
                                     MainFrm.EXCSpectrumStartWavelength) /
                                     MainFrm.EXCSpectrumStepSize ) + 1 ;
        NumFramesPerSpectrum := NumFramesPerCycle ;
        end ;

     // Photostimulus pulse program
     // ---------------------------
     bStopPhotoStimulus.Enabled := PhotoStimulusRequired ;
     bStartPhotoStimulus.Enabled := not PhotoStimulusRequired ;
     TReEnableStartPhotoStimulusButton := 1E30 ;
     if PhotoStimulusRequired then begin
        // Load photostimulus program
        PhotoStimulator.LoadProgram( MainFrm.PhotoStimFileName ) ;
        LogFrm.AddLine( 'PhotoStim Prog: ' + ExtractFileName(PhotoStimulator.FileName) + ' started' ) ;
        if Mainfrm.PhotoStim.RepeatedStim then begin
           StimDuration := Max( StimDuration, MainFrm.PhotoStim.Period ) ;
           RepeatedStimulus := True ;
           end
        else begin
           StimDuration := Max( StimDuration, PhotoStimulator.ProtocolDuration ) ;
           TReEnableStartPhotoStimulusButton := TimeGetTime + StimDuration*1000.0 ;
           RepeatedStimulus := False ;
           end ;
        end ;

     NumFramesPerCycle := MaxFramesPerCycle ;

     // Ensure number of frames per timing cycle is
     // multiple of number of frame types in use

     if rbSingleWavelength.Checked then begin
        // Single wavelength
        FramesDivFactor := 1 ;
        end
     else if rbSpectrum.Checked then begin
        // Wavelength cycle = no. of wavelengths in spectrum
        FramesDivFactor := NumFramesPerSpectrum ;
        end
     else begin
        // Wavelength cycle for multi-wavelength, dual-rate imaging

        // Dual rate not allowed in time lapse mode
        // (Set all divide factors to 1.0)
        if cbRecordingMode.ItemIndex <> rmContinuous then begin
           for i := 0 to MainFrm.EXCNumWavelengths[MainFrm.EXCSequenceNum]-1 do begin
               MainFrm.EXCSequence[i,MainFrm.EXCSequenceNum].DivideFactor := 1 ;
               end ;
           end ;

         NumSlowFrames := 0 ;
         while (MainFrm.EXCSequence[NumSlowFrames,MainFrm.EXCSequenceNum].DivideFactor > 1) and
               (NumSlowFrames < MainFrm.EXCNumWavelengths[MainFrm.EXCSequenceNum]) do inc(NumSlowFrames) ;
        FramesDivFactor := NumSlowFrames +
                           MainFrm.EXCSequence[0,MainFrm.EXCSequenceNum].DivideFactor *
                           (MainFrm.EXCNumWavelengths[MainFrm.EXCSequenceNum]-NumSlowFrames) ;
        end ;

     FramesDivFactor := Max(FramesDivFactor,1) ;
     if ckZStackEnabled.Checked and ZStageGrp.Visible then begin
        ZStage.NumSteps := Round(edZNumSteps.Value) ;
        NumFramesPerZStep := FramesDivFactor*2 ;
        NumFramesPerZStack := NumFramesPerZStep*ZStage.NumSteps ;
        end
     else begin
        NumFramesPerZStep := FramesDivFactor ;
        NumFramesPerZStack := FramesDivFactor ;
        end ;
     NumFramesPerCycle := Max( NumFramesPerCycle div NumFramesPerZStack,1)*NumFramesPerZStack ;

     // Set frame cycle time to time lapse interval
     if (cbRecordingMode.ItemIndex = rmTimeLapse) or (cbRecordingMode.ItemIndex = rmTimeLapseBurst)then begin
        NumFramesPerTimeLapseInterval := Max(Round(EdTimeLapseInterval.Value/MainFrm.Cam1.FrameInterval),1) ;
        NumFramesPerCycle := Max(NumFramesPerCycle div NumFramesPerTimeLapseInterval,1)*NumFramesPerTimeLapseInterval ;
        NumFramesPerCycle := Min(NumFramesPerCycle,MaxFramesPerCycle) ;
        NumFramesPerCycle := (Max(NumFramesPerCycle div FramesDivFactor,1))*FramesDivFactor ;
        NumTimeLapseIntervalsPerCycle := Max(NumFramesPerCycle div NumFramesPerTimeLapseInterval,1) ;
        NumFramesPerTimeLapseInterval := NumFramesPerCycle div NumTimeLapseIntervalsPerCycle ;
        TimeLapseOpenShutterAtFrame := NumFramesPerTimeLapseInterval - Round(Ceil(LightSource.ShutterChangeTime/MainFrm.Cam1.FrameInterval));
        EdTimeLapseInterval.Value := NumFramesPerTimeLapseInterval*MainFrm.Cam1.FrameInterval ;
        TimeLapseFrameInterval := GetTimeLapseFrameInterval ;
        end ;

     // Total number of DAC updates in timing cycle
     NumFramesPerCycle := Max(Round(5.0/MainFrm.Cam1.FrameInterval) div NumFramesPerCycle,1)*NumFramesPerCycle ;
     NumDACPointsPerCycle := NumDACPointsPerFrame*NumFramesPerCycle ;

     // Allocate D/A waveform buffers
     for Device := 1 to LabIO.NumDevices do begin
         if DACBufs[Device] <> Nil then FreeMem(DACBufs[Device]) ;
         DACBufs[Device] := Nil ;
         if LabIO.NumDACs[Device] > 0 then begin
            GetMem( DACBufs[Device],NumDACPointsPerCycle*LabIO.NumDACs[Device]*2 ) ;
            // Initialise to default values
            for ch := 0 to LabIO.NumDACs[Device]-1 do
                DACValue[ch] := Round(LabIO.DACOutState[Device][ch]*LabIO.DACScale[Device]) ;
            j := 0 ;
            for i := 0 to NumDACPointsPerCycle-1 do
                for ch := 0 to LabIO.NumDACs[Device]-1 do begin
                    DACBufs[Device]^[j] := DACValue[ch] ;
                    Inc(j) ;
                    end ;
            end ;
         end ;

     // Allocate and clear digital waveform buffers
     for Device := 1 to LabIO.NumDevices do begin
         if DigBufs[Device] <> Nil then FreeMem(DigBufs[Device]) ;
         DigBufs[Device] := Nil ;
         GetMem( DigBufs[Device],NumDACPointsPerCycle*4 ) ;
         for i := 0 to NumDACPointsPerCycle-1 do DigBufs[Device]^[i] := LabIO.DigOutState[Device] ;
         end ;

     // Update excitation light source wavelength
     UpdateLightSource ;

     // Create camera exposure start pulse waveform
     // (Must come after UpdateLightSource since it now uses FractionalExposure defined in UpdateLightSource)
     UpdateCameraStartWaveform ;

     // Update emission filters
     UpdateEMFilter ;

     // Update light source shutter
     UpdateLightSourceShutter ;

     // Update Z position
     if RecordingMode = rmRecordingInProgress then EnableZStack := True
                                              else EnableZStack := False ;

     if cbRecordingMode.ItemIndex = rmTimeLapse then begin
        NumFramesPerZStep := NumFramesPerTimeLapseInterval ;
        end
     else begin
        NumFramesPerZStep := NumFramesPerWavelengthCycle ;
        end;

     ZStage.UpdateDACBuffer( EnableZStack,
                             ZStage.Position,
                             NumFramesPerCycle,
                             NumFramesPerZStep,
                             NumFramesPerWavelengthCycle,
                             NumDACPointsPerFrame,
                             DACUpdateInterval,
                             DACBufs ) ;

     // Update photostimulus waveforms
     UpdatePhotoStimulus( PhotoStimulusRequired ) ;

     // Update voltage & digital stimulus waveforms (and initialise DAC buffer pointers)
     UpdateStimulusWaveforms( StimulusRequired, True ) ;

     // Start A/D conversion (samples triggered by DAC updates)
     if (MainFrm.ADCNumChannels > 0) and (ADCDevice > 0) then StartADC ;

     // Start D/A and digital waveform output
     for Device := 1 to LabIO.NumDevices do begin
         // Set up digital output sweep (note must be done before DAC)
         if DeviceDIGsInUse[Device] then begin
            LabIO.MemoryToDIG( Device,
                               DIGBufs[Device]^,
                               NumDACPointsPerCycle,
                               DACUpdateInterval,
                               True,
                               False,
                               FindTimingDevice,
                               DeviceDACsInUse[FindTimingDevice] ) ;
            end ;

         // Set up D/A output sweep
         if DeviceDACsInUse[Device] then begin
            LabIO.MemoryToDAC( Device,
                               DACBufs[Device]^,
                               LabIO.NumDACs[Device],
                               NumDACPointsPerCycle,
                               DACUpdateInterval,
                               True,
                               False,
                               FindTimingDevice ) ;
            end ;
         end ;

    if FrameTypeCycleLength > 1 then begin
       MainFrm.NumFramesRequired := GetNumFramesRequired ;
       NumFramesRequired := MainFrm.NumFramesRequired ;
       end ;

    // Update voltage & digital stimulus waveforms
//    UpdateStimulusWaveforms( StimulusRequired ) ;

    end ;


procedure TRecordFrm.StopTimingCycle ;
// ------------------------------
// Stop hardware timing waveforms
// ------------------------------
var
    Device : SmallInt ;
    i : Integer ;
begin

    if LabIO.NumDevices <= 0 then Exit ;

    // Stop D/A outputs
    for Device := 1 to LabIO.NumDevices do begin

        // Stop D/A output on device
        LabIO.StopDAC(Device) ;
        if DeviceDACsInUse[Device] then begin
           // Set to default off voltages
           for i := 0 to LabIO.NumDACs[Device]-1 do
               LabIO.WriteDAC( Device, LabIO.DACOutState[Device][i], i ) ;
           DeviceDACsInUse[Device] := False ;
           end ;

        // Stop digital output on device
        LabIO.StopDIG(Device) ;
        if DeviceDIGsInUse[Device] then begin
           // Set default off state
           LabIO.WriteToDigitalOutPutPort( Device, LabIO.DigOutState[Device] ) ;
           DeviceDIGsInUse[Device] := False ;
           end ;

        end ;

    // Stop A/D device
    if ADCDevice > 0 then LabIO.StopADC(ADCDevice) ;


    // Free buffers
    for Device := 1 to LabIO.NumDevices do begin
         if DACBufs[Device] <> Nil then begin
            FreeMem(DACBufs[Device]) ;
            DACBufs[Device] := Nil ;
            end ;
         if DIGBufs[Device] <> Nil then begin
            FreeMem(DIGBufs[Device]) ;
            DIGBufs[Device] := Nil ;
            end ;
         end ;

    end ;


procedure TRecordFrm.UpdateCameraStartWaveform ;
// ----------------------------------------
// Update camera start timing pulse pattern
// ----------------------------------------
var
     Device : Integer ;
     OnState : SmallInt ;
     OffState : SmallInt ;
     BitMask : Byte ;
     NumCamTriggerOffsetIntervals : Integer ;
     DACChannel : Integer ;
     NumDACChannels : Integer ;
     i,j,k,iStart,iEnd,iWavelength : Integer ;
     ExpTime : Double ;
begin

     // Exit if no output channelconfigured
     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.CameraStart)  then Exit ;

     // Get output device
     Device := LabIO.Resource[MainFrm.IOConfig.CameraStart].Device ;

     // Set camera start pulses
     // If camera uses a frame readout trigger at end of frame rather than an exposure start trigger
     // trigger offset is interpreted as a negative shift
     NumCamTriggerOffsetIntervals := Round(MainFrm.CameraTriggerOffset/DACUpdateInterval) ;
     NumCamTriggerOffsetIntervals := Max( Min(NumCamTriggerOffsetIntervals,NumDACPointsPerFrame-1),
                                          -NumDACPointsPerFrame+1 ) ;

     // Set camera trigger at end of frame if camera uses CCD readout trigger
     if MainFrm.Cam1.TriggerType = CamReadoutTrigger then begin
        NumCamTriggerOffsetIntervals := NumDACPointsPerFrame - NumCamTriggerOffsetIntervals - 1 ;
        end ;

     if (NumDACPointsPerCycle mod NumFramesPerCycle) <> 0 then
        ShowMessage( 'Cycle period is not a multiple of frame duration' ) ;

     // Create camera exposure start waveform
     // -------------------------------------

     if LabIO.Resource[MainFrm.IOConfig.CameraStart].ResourceType = DACOut then begin

        // DAC Output channel
        // ------------------

        if DACBufs[Device] = Nil Then Exit ;
        DeviceDACsInUse[Device] := True ;

        // DAC channel and scaling factors
        DACChannel := LabIO.Resource[MainFrm.IOConfig.CameraStart].StartChannel ;
        NumDACChannels := LabIO.NumDACs[Device] ;

        if MainFrm.IOConfig.CameraStartActiveHigh then begin
           // 5V Active High trigger
           OnState := Round(4.5*LabIO.DACScale[Device]) ;
           OffState := 0 ;
           end
        else begin
           // Active Low (0V) trigger
           OffState := Round(4.5*LabIO.DACScale[Device]) ;
           OnState := 0 ;
           end ;

        // Clear channel
        j := DACChannel ;
        for i := 0 to NumDACPointsPerCycle-1 do begin
            DACBufs[Device]^[j] := OffState ;
            j := j + NumDACChannels ;
            end ;

        if not MainFrm.BulbExposureMode then begin
           // Exposure start mode: DACUpdateInterval pulse triggers start of exposure
           j := NumCamTriggerOffsetIntervals*NumDACChannels + DACChannel ;
           for i := 0 to NumFramesPerCycle-1 do begin
               DACBufs[Device]^[j] := OnState ;
               j := j + NumDACPointsPerFrame*NumDACChannels ;
               end ;
           end
        else begin
           // Bulb mode: Pulse duration defines exposure time
           iStart := Round(MainFrm.CameraTriggerOffset/DACUpdateInterval) ;
           iStart := Max( Min(iStart,NumDACPointsPerFrame-1),0);
           iWavelength := 0 ;
           for i := 0 to NumFramesPerCycle-1 do begin
               // Reduce exposure time to fraction request for selected wavelength
               ExpTime := FractionalExposure[iWavelength]*MainFrm.Cam1.ExposureTime ;
               iEnd := iStart + Max(Round(ExpTime/DACUpdateInterval),0) ;
               k := (i*NumDACPointsPerFrame + iStart)*NumDACChannels + DACChannel ;
               for j := iStart to iEnd do begin
                  DACBufs[Device]^[k] := OnState ;
                  k := k + NumDACChannels ;
                  end;

               // Next frame type
               Inc(iWaveLength) ;
               if iWaveLength = NumFramesPerWavelengthCycle then iWaveLength := 0 ;

               end ;
           end ;

        // Set off-state value for this channel
        LabIO.DACOutState[Device][DACChannel] := OffState/LabIO.DACScale[Device] ;

        end
     else if LabIO.Resource[MainFrm.IOConfig.CameraStart].ResourceType = DIGOut then begin

        // Digital Output channel
        // ----------------------

        if DIGBufs[Device] = Nil Then Exit ;
        DeviceDIGsInUse[Device] := True ;

        // Bit mask to isolate bit within digital O/P byte
        BitMask := not LabIO.BitMask(LabIO.Resource[MainFrm.IOConfig.CameraStart].StartChannel) ;

        if MainFrm.IOConfig.CameraStartActiveHigh then begin
           // 5V Active High trigger
           OnState := LabIO.BitMask(LabIO.Resource[MainFrm.IOConfig.CameraStart].StartChannel) ;
           OffState := 0 ;
           end
        else begin
           // Active Low (0V) trigger
           OffState := LabIO.BitMask(LabIO.Resource[MainFrm.IOConfig.CameraStart].StartChannel) ;
           OnState := 0 ;
           end ;

        // Clear output bit to off-state
        for i := 0 to NumDACPointsPerCycle-1 do begin
            DIGBufs[Device]^[i] := (DIGBufs[Device]^[i] and BitMask) or OffState ;
            end ;

        if not MainFrm.BulbExposureMode then begin
           // Exposure start mode: DACUpdateInterval pulse triggers start of exposure
           j := NumCamTriggerOffsetIntervals ;
           for i := 0 to NumFramesPerCycle-1 do begin
               if j < NumDacPointsPerCycle then begin
                  DIGBufs[Device]^[j] := (DIGBufs[Device]^[j] and BitMask) or OnState ;
                  end ;
               j := j + NumDACPointsPerFrame ;
               end ;
            end
        else begin
           // Bulb mode: Pulse duration defines exposure time
           iStart := Round(MainFrm.CameraTriggerOffset/DACUpdateInterval) ;
           iStart := Max( Min(iStart,NumDACPointsPerFrame-1),0);
           iWavelength := 0 ;
           for i := 0 to NumFramesPerCycle-1 do begin
               ExpTime := FractionalExposure[iWavelength]*MainFrm.Cam1.ExposureTime ;
               iEnd := iStart + Max(Round(ExpTime/DACUpdateInterval),0) ;
               k := (i*NumDACPointsPerFrame + iStart) ;
               for j := iStart to iEnd do begin
                  DIGBufs[Device]^[k] := (DIGBufs[Device]^[k] and BitMask) or OnState ;
                  k := k + 1 ;
                  end;

               // Next frame type
               Inc(iWaveLength) ;
               if iWaveLength = NumFramesPerWavelengthCycle then iWaveLength := 0 ;

               end ;

            end;
        // Set off-state value for this channel
        LabIO.DigOutState[Device] := (LabIO.DigOutState[Device] and BitMask) or OffState ;
        end ;

     end ;


procedure TRecordFrm.UpdateLightSource ;
// -------------------------------------
// Update light source control waveforms
// -------------------------------------
var
     Dev : Integer ;
     iChan : Integer ;
     iFrame : Integer ;
     iV : Integer ;
     iStart : Integer ;
     NumDACChannels : Integer ;
     i,j,iResource : Integer ;
     DACValue : SmallInt ;
     VControl : Array[0..MaxLightSourceCycleLength] of TLSVControl ;
     VControlClosed : Array[0..MaxLightSourceCycleLength] of TLSVControl ;
     NumVControl : Integer ;
     DACBufSize : Integer ;
     iWaveLength : Integer ;
     V,Delay : single ;
     BlankingStartAt,iShorten : Integer ;
     VClosed : Array[0..MaxLSControlLine] of Single ;
     LastSlow,NumFramesPerSpectrum,WVNum : Integer ;
     ActualDivideFactor : Integer ;
     W : Single ;
     Bit : Word ;
     BitMask  : Word ;
     StartBit  : Word ;
     DigBufSize : Integer ;
     PDigBuf : PBig32bitArray ;
     Temp : Array[0..MaxLightSourceCycleLength-1] of Integer ;
begin

     // Default frame type cycle (if no light source)
     FrameTypeCycleLength := NumFramesPerCCD ;
     for i := 0 to FrameTypeCycleLength-1 do FrameTypeCycle[i] := i ;
     FrameTypeCycleCounter := 0 ;

     // Exit if no light source or D/A channels configured
     if LightSource.DeviceType = lsNone then Exit ;

     // Create wavelength/filter cycle patterns

     if rbSingleWavelength.Checked then begin
         // Single wavelength excitatiom
         FilterNums[0] := MainFrm.EXCSingleWavelengthNum ;
         Wavelengths[0] :=MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].Centre ;
         Bandwidths[0] :=  MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].Width ;
         FractionalExposure[0] :=  MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].FractionalExposure ;
         EMFilters[0] :=  MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].EMFilter ;
         NumFramesPerWavelengthCycle := 1 ;
         FrameTypeCycleLength := NumFramesPerCCD ;
         for i := 0 to FrameTypeCycleLength-1 do FrameTypeCycle[i] := i ;
         FrameTypeCycleCounter := 0 ;
         end
     else if rbSpectrum.Checked then begin
         // Spectrum mode
         NumFramesPerWavelengthCycle := 0 ;
         NumFramesPerSpectrum := Round( (MainFrm.EXCSpectrumEndWavelength -
                                        MainFrm.EXCSpectrumStartWavelength) /
                                        MainFrm.EXCSpectrumStepSize ) + 1 ;
         W := MainFrm.EXCSpectrumStartWavelength ;
         repeat
            FilterNums[NumFramesPerWavelengthCycle] :=
                  MainFrm.EXCSequence[NumFramesPerWavelengthCycle,MainFrm.EXCSequenceNum].WavelengthNum ;
            Wavelengths[NumFramesPerWavelengthCycle] := W ;
            Bandwidths[NumFramesPerWavelengthCycle] := MainFrm.EXCSpectrumBandwidth ;
            FractionalExposure[NumFramesPerWavelengthCycle] :=  1.0 ;
            EMFilters[NumFramesPerWavelengthCycle] := MainFrm.EXCSpectrumEMFilter ;
            Inc(NumFramesPerWavelengthCycle) ;
            W := W + MainFrm.EXCSpectrumStepSize ;
            until (NumFramesPerWavelengthCycle >= NumFramesPerSpectrum) ;

         FrameTypeCycleLength := NumFramesPerCCD ;
         for i := 0 to FrameTypeCycleLength-1 do FrameTypeCycle[i] := i ;
         FrameTypeCycleCounter := 0 ;
         end
     else begin
         // Sequential multi-wavelength excitation
         // Frame cycle = <Slow wavelengths (one cycle)><Fast wavelengths (DivideFactor cycles)>

         NumFramesPerWavelengthCycle := 0 ;
         // Determine last slow frame
         i := 0 ;
         LastSlow := 0 ;
         while (MainFrm.EXCSequence[i,MainFrm.EXCSequenceNum].DivideFactor > 1) and
               (i < MainFrm.EXCNumWavelengths[MainFrm.EXCSequenceNum]) do begin
               LastSlow := i ;
               Inc(i) ;
               end ;

         // Add one cycle of slow rate wavelengths
         for i := 0 to LastSlow do begin
             WVNum := MainFrm.EXCSequence[i,MainFrm.EXCSequenceNum].WavelengthNum ;
             FilterNums[NumFramesPerWavelengthCycle] := WVNum ;
             FrameTypeCycle[NumFramesPerWavelengthCycle] := i ;
             Wavelengths[NumFramesPerWavelengthCycle] := MainFrm.EXCWavelengths[WVNum].Centre ;
             Bandwidths[NumFramesPerWavelengthCycle] :=  MainFrm.EXCWavelengths[WVNum].Width ;
             FractionalExposure[NumFramesPerWavelengthCycle] :=  MainFrm.EXCWavelengths[WVNum].FractionalExposure ;
             EMFilters[NumFramesPerWavelengthCycle] := MainFrm.EXCWavelengths[WVNum].EMFilter ;
             Inc(NumFramesPerWavelengthCycle) ;
             end ;

         // Add DivideFactor cycle of fast frames
         ActualDivideFactor := 0 ;
         for j := 1 to MainFrm.EXCSequence[0,MainFrm.EXCSequenceNum].DivideFactor do begin
             for i := LastSlow+1 to MainFrm.EXCNumWavelengths[MainFrm.EXCSequenceNum]-1 do begin
                 WVNum := MainFrm.EXCSequence[i,MainFrm.EXCSequenceNum].WavelengthNum ;
                 FilterNums[NumFramesPerWavelengthCycle] := WVNum ;
                 FrameTypeCycle[NumFramesPerWavelengthCycle] := i ;
                 Wavelengths[NumFramesPerWavelengthCycle] := MainFrm.EXCWavelengths[WVNum].Centre ;
                 Bandwidths[NumFramesPerWavelengthCycle] :=  MainFrm.EXCWavelengths[WVNum].Width ;
                 FractionalExposure[NumFramesPerWavelengthCycle] :=  MainFrm.EXCWavelengths[WVNum].FractionalExposure ;
                 EMFilters[NumFramesPerWavelengthCycle] :=  MainFrm.EXCWavelengths[WVNum].EMFilter ;
                 Inc(NumFramesPerWavelengthCycle) ;
                 end ;
             Inc(ActualDivideFactor) ;
             // Prevent cycle exceeding available buffer
             if NumFramesPerWavelengthCycle >
                ((MaxFramesPerCycle div 2) - MainFrm.EXCNumWavelengths[MainFrm.EXCSequenceNum]) then break ;
            end ;

         MainFrm.EXCSequence[0,MainFrm.EXCSequenceNum].DivideFactor := ActualDivideFactor ;

         // Add CCD sub-frames to frame type cycle
         for j := 0 to NumFramesPerWavelengthCycle-1 do Temp[j] := FrameTypeCycle[j] ;
         FrameTypeCycleLength := 0 ;
         for j := 0 to NumFramesPerWavelengthCycle-1 do
             for i := 0 to NumFramesPerCCD-1 do begin
                 FrameTypeCycle[FrameTypeCycleLength] := (Temp[j]*NumFramesPerCCD)+i ;
                 Inc(FrameTypeCycleLength);
                 end;
         FrameTypeCycleCounter := 0 ;

         end ;


     if (NumDACPointsPerCycle mod NumDACPointsPerFrame) <> 0 then
        ShowMessage( 'Cycle period is not a multiple of frame duration') ;

     // Determine start of period of shutter blanking
     iShorten := Round(LightSource.ShutterBlankingPeriod/DACUpdateInterval) ;
     if (cbRecordingMode.ItemIndex = rmContinuous) and ckZStackEnabled.Checked then begin
        // If in continuous recording mode, and Z stack enabled set frame blanking to turn off
        // excitation during Z step
        iShorten := Max(iShorten,Round(ZStage.ShortenExcitationBy/DACUpdateInterval));
        end ;
     BlankingStartAt := Min(Max(NumDACPointsPerFrame - iShorten,0),NumDACPointsPerFrame) ;

     // Get voltages for shutters closed condition
     LightSource.ShutterClosedVoltages ;
     for i := 0 to LightSource.NumControlLines-1 do begin
         iResource := MainFrm.IOConfig.LSControlLine[i] ;
         if MainFRm.IOResourceAvailable(iResource) then VClosed[i] := LabIO.Resource[iResource].V ;
         end ;

     iWaveLength := 0 ;
     for iFrame := 0 to NumFramesPerCycle-1 do begin

         // Get control voltages for wavelength
         if ExcitationOn( iFrame ) then begin
            // Get voltages for selected wavelength
            LightSource.WavelengthToVoltage( FilterNums[iWaveLength],
                                             Wavelengths[iWaveLength],
                                             Bandwidths[iWaveLength] );
            end
         else begin
            // Get voltages for shutters closed condition
            LightSource.ShutterClosedVoltages ;
            end ;

         // Start of frame in cycle buffer
         iStart := iFrame*NumDACPointsPerFrame ;

         // Set D/A channel with excitation voltage

         for iV := 0 to LightSource.NumControlLines-1 do begin

             iResource := MainFrm.IOConfig.LSControlLine[iV] ;
             if not MainFRm.IOResourceAvailable(iResource) then Continue ;

             Dev := LabIO.Resource[iResource].Device ;
             iChan := LabIO.Resource[iResource].StartChannel ;
             Delay := LabIO.Resource[iResource].Delay ;
             V := LabIO.Resource[iResource].V ;

             if LabIO.Resource[iResource].ResourceType = DACOut then begin

                // *** Update DAC channel***

                DeviceDACsInUse[Dev] := True ;
                NumDACChannels := LabIO.NumDACs[Dev] ;
                DACBufSize := NumDACPointsPerFrame*NumDACChannels*NumFramesPerCycle ;

                j := (iStart + Round(Delay/DACUpdateInterval))*NumDACChannels + iChan ;

                // Open shutter period
                DACValue := Round(V*LabIO.DACScale[Dev]) ;

                for i := 0 to BlankingStartAt-1 do begin
                    // Keep within circular buffer
                    if j >= DACBufSize then j := j - DACBufSize
                    else if j < 0 then j := j + DACBufSize ;

                    DACBufs[Dev]^[j] := DACValue ;
                    j := j + NumDACChannels ;
                    end ;

                // Shutter blanking period (set to closed wavelength & slits)
                DACValue := Round(VClosed[iV]*LabIO.DACScale[Dev]) ;
                for i := BlankingStartAt to NumDACPointsPerFrame-1 do begin
                    // Keep within circular buffer
                   if j >= DACBufSize then j := j - DACBufSize
                   else if j < 0 then j := j + DACBufSize ;
                   DACBufs[Dev]^[j] := DACValue ;
                   j := j + NumDACChannels ;
                   end ;

                // Set off-state value for this channel
                LabIO.DACOutState[Dev][iChan] := VClosed[iV] ;

                end
             else begin

                // *** Update digital channel***

                // Start of frame in cycle buffer
                iStart := iFrame*NumDACPointsPerFrame ;
                j := (iStart + Round(Delay/DACUpdateInterval)) ;

                PDigBuf := DigBufs[Dev] ;      // Digital O/P buffer
                DeviceDIGsInUse[Dev] := True ;

                // Create pre-blanking data
                Bit := LabIO.BitMask(iChan) ;
                BitMask := not Bit ;
                if V = 0.0 then Bit := 0 ;
                for i := 0 to BlankingStartAt-1 do begin
                    // Keep within circular buffer
                    if j >= DIGBufSize then j := j - DIGBufSize
                    else if j < 0 then j := j + DIGBufSize ;
                    pDigBuf^[j] := (pDigBuf^[j] and BitMask) or Bit ;
                    Inc(j) ;
                    end ;

                // Create post-blanking data (closed setting)
                Bit := LabIO.BitMask(iChan) ;
                BitMask := not Bit ;
                if VClosed[iV] = 0.0 then Bit := 0 ;
                for i := BlankingStartAt to NumDACPointsPerFrame-1 do begin
                    // Keep within circular buffer
                    if j >= DIGBufSize then j := j - DIGBufSize
                    else if j < 0 then j := j + DIGBufSize ;
                    pDigBuf^[j] := (pDigBuf^[j] and BitMask) or Bit ;
                    Inc(j) ;
                    end ;

                // Set off state
                LabIO.DigOutState[Dev] := (LabIO.DigOutState[Dev] and BitMask) or Bit ;

                end;

             end ;

         // Next frame type
         Inc(iWaveLength) ;
         if iWaveLength = NumFramesPerWavelengthCycle then iWaveLength := 0 ;

         end ;

     end ;


function TRecordFrm.ExcitationOn( iFrame : Integer ) : boolean ;
// -------------------------------------------
// Return TRUE if excitation on at this frame
// -------------------------------------------
begin

     if cbRecordingMode.ItemIndex <> rmTimeLapseBurst then BurstIlluminationOn := False ;
     if (cbRecordingMode.ItemIndex <> rmContinuous) and
        (RecordingMode = rmRecordingInProgress) and
        (not BurstIlluminationOn) then begin
          // time lapse recording - excitation turned on for one wavelength sequence per time lapse period
          iFrame := iFrame mod NumFramesPerTimeLapseInterval ;
          if (iFrame >= NumFramesPerWavelengthCycle) and
             (iFrame < TimeLapseOpenShutterAtFrame) then Result := False
                                                    else Result := True ;
          end
     else begin
          Result := True ;
          end ;

     if not rbEXCShutterOpen.Checked then Result := False ;

     end ;


procedure TRecordFrm.UpdateEMFilter ;
// ------------------------
// Update emission filter
// ------------------------
var
     Device : Integer ;
     iFrame : Integer ;
     i,j,iBit : Integer ;
     NumPointsPerCycle : Integer ;
     Bit : Word ;
     BitMask  : Word ;
     iStartBit,iEndBit  : Word ;
     EMFilterBits : Integer ;
     PDigBuf : PBig32bitArray ;
     iWavelength : Integer ;
begin

     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.EMFilterStart) then Exit ;
     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.EMFilterEnd) then Exit ;

     // D/A output device
     Device :=  LabIO.Resource[MainFrm.IOConfig.EMFilterStart].Device ;
     if Device <= 0 then Exit ;
     Device :=  LabIO.Resource[MainFrm.IOConfig.EMFilterEnd].Device ;
     if Device <= 0 then Exit ;
     if DigBufs[Device] = Nil Then Exit ;

     PDigBuf := DigBufs[Device] ;      // Digital O/P buffer
     DeviceDIGsInUse[Device] := True ;
     NumPointsPerCycle := NumDACPointsPerFrame*NumFramesPerCycle ;

     if (NumDACPointsPerCycle mod NumDACPointsPerFrame) <> 0 then
        ShowMessage( 'Cycle period is not a multiple of frame duration') ;

     iStartBit := LabIO.Resource[MainFrm.IOConfig.EmFilterStart].StartChannel ;
     iEndBit := LabIO.Resource[MainFrm.IOConfig.EmFilterEnd].EndChannel ;

     // Create bit mask for EMFilter control line bits
     BitMask := 0 ;
     for iBit := iStartBit to iEndBit do begin
         Bit := LabIO.BitMask(iBit) ;
         BitMask := BitMask or Bit ;
         end ;
     BitMask := not BitMask ;

     // Update filter bits in digital output buffer

     iWaveLength := 0 ;
     for iFrame := 0 to NumFramesPerCycle-1 do begin

         EMFilterBits := EMFilters[iWaveLength] shl iStartBit ;

         // Start of frame in cycle buffer
         j := iFrame*NumDACPointsPerFrame - Max(Round(LightSource.EMFilterChangeTime/DACUpdateInterval),0) ;
         if j < 0 then j := j + NumPointsPerCycle ;
         for i := 1 to NumDACPointsPerFrame do begin
             pDigBuf^[j] := (pDigBuf^[j] and BitMask) or EMFilterBits ;
             Inc(j) ;
             if j >= NumPointsPerCycle then j := 0 ;
             end ;

         // Next frame type
         Inc(iWaveLength) ;
         if iWaveLength = NumFramesPerWavelengthCycle then iWaveLength := 0 ;

         end ;

     // Set off-state value for this channel (first filter setting)
     EMFilterBits := EMFilters[0] shl iStartBit ;
     LabIO.DigOutState[Device] := (LabIO.DigOutState[Device] and BitMask) or EMFilterBits ;

     end ;


procedure TRecordFrm.UpdateStimulusWaveforms(
          StimulusEnabled : Boolean ;
          InitialiseBuffer : Boolean   // TRUE = initialise LABIO.DAC
          )   ;
// ----------------------------------------------------------
// Create analogue & digital D/A output waveform for patch clamp command voltage
// ----------------------------------------------------------
var
    Device : Integer ;          // Hardware device #
    VChan : Integer ;           // Voltage O/P channel #
    DACChannel : Integer ;      // DAC output channel #
    DACNumChannels : Integer ;  // No. of DAC channels on current device
    VHoldDACValue : Integer ;   // DAC default holding value
    DChan : Integer ;           // Digital stimulus bit
    DStartChan : Integer ;
    DNumChans : Integer ;
    DigMask, DigWord : Integer ; // Digital output word and mask
    StimCh : Integer ;
    DACLevel : Integer ;
    i,j : Integer ;
    PDACBuf : PBig16bitArray ;
    StimDuration : Single ;
    RepeatedStimulus : Boolean ;
begin

     // Set default voltage stimulus outputs

     for VChan := 0 to NumCommandVoltageChannels-1 do begin

         // Skip if channel is not configured
         if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[VChan]) then Continue ;

         // Get I/O device properties (skip if not set up)
         Device := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device ;
         if Device <= 0 then Continue ;
         if DACBufs[Device] = Nil then Continue ;
         DeviceDACsInUse[Device] := True ;

         // DAC output channel and no. of channels on device
         DACChannel := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].StartChannel ;
         DACNumChannels := LabIO.NumDACs[Device] ;

         // Initialise buffer to holding potential
         VHoldDACValue := Min( Max( Round(MainFrm.VCommand[VChan].HoldingVoltage
                                    * MainFrm.VCommand[VChan].DivideFactor
                                    * LabIO.DACScale[Device]),
                                    LabIO.DACMinValue[Device]),LabIO.DACMaxValue[Device]) ;
         j := DACChannel ;
         for i := 0 to NumDACPointsPerCycle-1 do begin
             DACBufs[Device]^[j] := VHoldDACValue ;
             j := j + DACNumChannels ;
             end ;

         // Set default DAC channel value
         LabIO.DACOutState[Device,DACChannel] := VHoldDACValue/LabIO.DACScale[Device] ;

         end ;

     // Set default digital stimulus outputs

     DigMask := 0 ;
     for StimCh := 0 to NumDigitalStimulusChannels-1 do begin

         // Skip if channel is not configured
         if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) then Continue ;
         if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then Continue ;

         // Get I/O device properties (skip if not set up)
         Device := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device ;
         if Device <= 0 then Continue ;
         if DigBufs[Device] = Nil then Continue ;

         DeviceDIGsInUse[Device] := True ;

         // Get starting output bit and number of bits available
         DStartChan := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel ;
         DNumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel
                     -DStartChan + 1 ;

         if StimCh >= DNumChans then Continue ;

         // Get digital output bit
         DChan := DStartChan + StimCh ;
         DigMask := DigMask or LabIO.BitMask(DChan) ;

         // Update DAC output buffer (if a DAC channel is being used for digital output)
         if LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].ResourceType = DACOut then begin

            // No. of DAC channels
            DACNumChannels := LabIO.NumDACs[Device] ;
            if DChan >= DACNumChannels then Break ;

            // Calculate DAC output level value (5V or 0V)
            if (LabIO.DigOutState[Device] and LabIO.BitMask(DChan)) <> 0 then begin
               DACLevel := Round(4.99*LabIO.DACScale[Device]) ;
               end
            else DACLevel := 0 ;

            // Update DAC buffer
            j := DChan ;
            PDACBuf := DACBufs[Device] ;
            for i := 0 to NumDACPointsPerCycle-1 do begin
                PDACBuf^[j] := DACLevel ;
                j := j + DACNumChannels ;
                end ;

            // Set default DAC channel value
            LabIO.DACOutState[Device,DChan] := DACLevel/LabIO.DACSCale[Device] ;

            end ;

         end ;

     // Update digital output buffer with default settings
     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) then begin
        Device := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device ;
        if (DigBufs[Device] <> Nil) and (Device >0) then begin
           // Stimulus bits setting
           DigWord := DigMask and LabIO.DigOutState[Device] ;
           for i := 0 to NumDACPointsPerCycle-1 do begin
               DigBufs[Device]^[i] := (DigBufs[Device]^[i] and (not DigMask)) or DigWord ;
               end ;
           end ;
        end ;

     // Create stimulus if required
     if  StimulusEnabled then begin

         bStopStimulus.Enabled := StimulusRequired ;
         bStartStimulus.Enabled := not StimulusRequired ;
         TReEnableStartStimulusButton := 1E30 ;
        // Load Stimulus program
        if LoadNextProtocolFile and
           (Stimulator.Prog.NextProtocolFileName <> '') then begin
           Stimulator.LoadProgram( Stimulator.Prog.NextProtocolFileName ) ;
           end
        else Stimulator.LoadProgram( MainFrm.StimFileName ) ;
        LoadNextProtocolFile := False ;

        LogFrm.AddLine( 'Stim Prog: ' + ExtractFileName(Stimulator.Prog.FileName) + ' started' ) ;
        StimDuration := Stimulator.ProtocolDuration ;
        if Stimulator.Prog.NumRepeats <= 1 then begin
           TReEnableStartStimulusButton := TimeGetTime + StimDuration*1000.0 ;
           RepeatedStimulus := False ;
           end
        else RepeatedStimulus := True ;
        Stimulator.CreateWaveform( DACBufs,DigBufs,NumDACPointsPerCycle, InitialiseBuffer ) ;
        end ;

     end ;


procedure TRecordFrm.UpdateVCommandWaveform ;
// ----------------------------------------------------------
// Create D/A output waveform for patch clamp command voltage
// ----------------------------------------------------------
var
    Device : Integer ;          // Hardware device #
    VChan : Integer ;           // Voltage O/P channel #
    DACChannel : Integer ;      // DAC output channel #
    DACNumChannels : Integer ;  // No. of DAC channels on current device
    VHoldDACValue : Integer ;   // DAC default holding value
    i,j : Integer ;
    DACScale : Single ;
begin

     for VChan := 0 to NumCommandVoltageChannels-1 do begin

         // Skip if channel is not configured
         if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[VChan]) then Continue ;

         // Get I/O device properties (skip if not set up)
         Device := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device ;
         if Device <= 0 then Continue ;
         if DACBufs[Device] = Nil then Continue ;
         DeviceDACsInUse[Device] := True ;

         // DAC output channel and no. of channels on device
         DACChannel := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].StartChannel ;
         DACNumChannels := LabIO.NumDACs[Device] ;

         // D/A scaling factor
         DACScale := (LabIO.DACMaxValue[Device]*MainFrm.VCommand[VChan].DivideFactor)/
                      LabIO.DACMaxVolts[Device] ;


         // Initialise buffer to holding potential
         VHoldDACValue := Round(MainFrm.VCommand[VChan].HoldingVoltage*DACScale) ;
         j := DACChannel ;
         for i := 0 to NumDACPointsPerCycle-1 do begin
             DACBufs[Device]^[j] := VHoldDACValue ;
             j := j + DACNumChannels ;
             end ;

         LabIO.DACOutState[Device][DACChannel] := MainFrm.VCommand[VChan].HoldingVoltage ;

         end ;

     // Create stimulus if required
     if  StimulusRequired then begin
         Stimulator.CreateWaveform( DACBufs,DigBufs,NumDACPointsPerCycle, False ) ;
         end ;

     end ;


procedure TRecordFrm.UpdateDigitalWaveform ;
// ----------------------------------------------------------
// Create D/A output waveform for digital pulse output
// ----------------------------------------------------------
var
    Device : Integer ;
begin

     // Exit if no D/A channel configured
     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) then Exit ;

     // Get I/O device properties
     Device := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device ;
     if DigBufs[Device] = Nil Then Exit ;
     DeviceDIGsInUse[Device] := True ;

     // Set digital outputs to default settings
 //    for i := 0 to NumDACPointsPerCycle-1 do begin
 //        DigBufs[Device]^[i] := MainFrm.DefaultDigitalOutputs ;
 //        end ;

     // Create stimulus if required
     if  StimulusRequired then begin
         // Create analog & digital stimulus buffers
         Stimulator.CreateWaveform( DACBufs,DigBufs,NumDACPointsPerCycle, False ) ;

         end ;

     end ;


procedure TRecordFrm.UpdateLightSourceShutter ;
// --------------------------------
// Turn light source shutter on/off
// --------------------------------
var
    ShutterBit : Integer ;
    ShutterOpen,ShutterClosed : Integer ;
    ShutterBitMask : Integer ;
    Device : Integer ;
    i,j : Integer ;
    iFrame : Integer ;
    NumWavelengths : Integer ;
    FilterNums : Array[0..MaxLightSourceCycleLength-1] of Integer ;
    Wavelengths : Array[0..MaxLightSourceCycleLength-1] of Single ;
    Bandwidths : Array[0..MaxLightSourceCycleLength-1] of Single ;
    WVNum : Integer ;
    DACBlankingStart : Integer ;
begin

     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.LSShutter) then Exit ;

     // Set shutter controlled by digital O/P
     // -------------------------------------

     Device := LabIO.Resource[MainFrm.IOConfig.LSShutter].Device ;
     if Device <= 0 then Exit ;
     if DigBufs[Device] = Nil then Exit ;

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

     if MainFrm.EXCSingleWaveLength then begin
         // Single wavelength excitatiom
         FilterNums[0] := MainFrm.EXCSingleWavelengthNum ;
         Wavelengths[0] :=MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].Centre ;
         Bandwidths[0] :=  MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].Width ;
         NumWavelengths := 1 ;
         end
     else begin
         // Sequential multi-wavelength excitation
         for i := 0 to NumFrameTypes-1 do begin
            WVNum := MainFrm.EXCSequence[i,MainFrm.EXCSequenceNum].WavelengthNum ;
            FilterNums[i] := WVNum ;
            Wavelengths[i] := MainFrm.EXCWavelengths[WVNum].Centre ;
            Bandwidths[i] :=  MainFrm.EXCWavelengths[WVNum].Width ;
            end ;
         NumWavelengths := NumFrameTypes ;
         end ;

     // Determine start of period of shutter blanking
     DACBlankingStart := NumDACPointsPerFrame -
                         Round(LightSource.ShutterBlankingPeriod/DACUpdateInterval) ;
     DACBlankingStart := Min(Max(DACBlankingStart,0),NumDACPointsPerFrame) ;

     j := 0 ;
     for iFrame := 0 to NumFramesPerCycle-1 do begin

         if ExcitationOn(iFrame) then begin
            // Shutter open
            for i := 0 to DACBlankingStart-1 do begin
               DigBufs[Device]^[j] := (DigBufs[Device]^[j] and ShutterBitMask) or ShutterOpen ;
               Inc(j) ;
               end ;
            // Shutter closed during blanking period
            for i := DACBlankingStart to NumDACPointsPerFrame-1 do begin
               DigBufs[Device]^[j] := (DigBufs[Device]^[j] and ShutterBitMask) or ShutterClosed ;
               Inc(j) ;
               end ;
            end
         else begin
            // Shutter closed
            for i := 0 to NumDACPointsPerFrame-1 do begin
                DigBufs[Device]^[j] := (DigBufs[Device]^[j] and ShutterBitMask) or ShutterClosed ;
                Inc(j) ;
                end;
            end ;

         end ;

     // Set default digital output state
     LabIO.DigOutState[Device] := (LabIO.DigOutState[Device] and ShutterBitMask) or ShutterClosed ;

     end ;


procedure TRecordFrm.UpdateHoldingVoltage ;
// ----------------------
// Update holding voltage
// ----------------------
var
    DACScale : Single ;
    OldDACValue : SmallInt ;
    NewDACValue : SmallInt ;
    Device : SmallInt ;
    DACChannel : Integer ;
    DACNumChannels : Integer ;
    i,j : Integer ;
    VChan : Integer ;
begin

     for VChan := 0 to NumCommandVoltageChannels-1 do begin

        // Exit if no D/A channel configured
        if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[VChan]) then Continue ;

        Device := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device ;
        if Device <= 0 then Continue ;
        if DACBufs[Device] = Nil Then Continue ;

        DACChannel := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].StartChannel ;
        DACNumChannels := LabIO.NumDACs[Device] ;

        // Set D/A voltage->integer scaling factor
        DACScale := (LabIO.DACScale[Device]*MainFrm.VCommand[VChan].DivideFactor) ;

        // Old holding voltage
        OldDACValue := Round(MainFrm.VCommand[VChan].HoldingVoltage*DACScale) ;

        // New holding voltage
        if VChan = 0 then MainFrm.VCommand[VChan].HoldingVoltage := edVHold0.Value
        else if VChan = 1 then MainFrm.VCommand[VChan].HoldingVoltage := edVHold1.Value ;
        NewDACValue := Round(MainFrm.VCommand[VChan].HoldingVoltage*DACScale) ;

        j := DACChannel ;
        for i := 0 to NumDACPointsPerCycle-1 do begin
            DACBufs[Device]^[j] := DACBufs[Device]^[j] - OldDACValue + NewDACValue ;
            j := j + DACNumChannels ;
            end ;

        // Save as default value
        LabIO.DACOutState[Device][DACChannel] := DACBufs[Device]^[DACChannel]
                                                 / LabIO.DACScale[Device] ;

        end ;

     // A/D restart needed with NIDAQ-MX to force DAC updates
     //StopCamera ;
     //StartCamera ;

     end ;


procedure TRecordFrm.UpdatePhotoStimulus(StimulusEnabled : Boolean);
// ----------------------------------------------------------
// Create photo stimulus X/Y galvo control and intensity waveforms
// ----------------------------------------------------------
var
  Device : Integer;                  // Hardware device number
  DACNumChannels : Integer;          // Number of DAC channels on current device
  DACX, DACY, DACI, DACS : Integer;  // Control channels
  DACScale : Single;                 // DAC scale factor
  Offset : Integer;                  // Number of samples to offset protocol
begin

  // Skip if control channels are not configured
  if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimX) then Exit;
  DACX := LabIO.Resource[MainFrm.IOConfig.PhotoStimX].StartChannel;

  if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimY) then Exit;
  DACY := LabIO.Resource[MainFrm.IOConfig.PhotoStimY].StartChannel;

  if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI1) then Exit;
  DACI := LabIO.Resource[MainFrm.IOConfig.PhotoStimI1].StartChannel;

  if MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimShutter) then begin
     DACS := LabIO.Resource[MainFrm.IOConfig.PhotoStimShutter].StartChannel;
     end
  else DACS := -1 ;

  // Get I/O device properties (skip if not set up)
  Device := LabIO.Resource[MainFrm.IOConfig.PhotoStimX].Device;
  if Device <= 0 then Exit ;
  if DACBufs[Device] = Nil then Exit;
  DeviceDACsInUse[Device] := True ;

  // Number of channels on device
  DACNumChannels := LabIO.NumDACs[Device];

  // D/A scaling factor
  DACScale := LabIO.DACMaxValue[Device] / LabIO.DACMaxVolts[Device];

  Offset := 0;

  // Use PhotoStimulator at this point...
  PhotoStimulator.CreateWaveform(DACBufs,
                                 DACX, DACY, DACI, DACS,
                                 DACScale,
                                 NumDACPointsPerCycle,
                                 Device,
                                 StimulusEnabled,
                                 DACNumChannels,
                                 Offset);

  end;


procedure TRecordFrm.UpdateADCDisplay ;
// ------------------------------
// Update analogue inputs display
// ------------------------------
var
     i : Integer ;
     NumBytesWritten : Integer ;     // No. bytes written to file
     BufStart : Integer ;
     nCount : Integer ;
     Device : SmallInt ;
begin

     if not ADCRunning then Exit ;

     Device := LabIO.Resource[MainFrm.IOConfig.ADCIn].Device ;

     // Erase display and initialise live display counters
     // --------------------------------------------------
     if RecPlotFrm.ADCDisplayFull then begin
        RecPlotFrm.ADCInitialiseDisplay( ADCBuf,
                                         LabIO.ADCMaxValue[Device],
                                         DACUpdateInterval
                                         ) ;
        end ;

     // Load latest A/D samples into ADCBuf
     LabIO.GetADCSamples( ADCDevice, ADCBuf^ ) ;

     // Find latest A/D samples
     nCount := 0 ;
     While ((ADCBuf^[ADCEmptyPointer] <> EmptyFlag) or
            (ADCBuf^[ADCEmptyPointer+1] <> -EmptyFlag)) and
           (nCount < ADCNumSamplesInWriteBuffer) do begin
           ADCEmptyPointer := ADCEmptyPointer + MainFrm.ADCNumChannels*2 ;
           if ADCEmptyPointer >= ADCNumSamplesInBuffer then
              ADCEmptyPointer := ADCEmptyPointer - ADCNumSamplesInBuffer ;
           Inc(nCount) ;
           end ;
     ADCLatestScan := ADCEmptyPointer - MainFrm.ADCNumChannels*4 ;
     if ADCLatestScan < 0 then ADCLatestScan := ADCLatestScan + ADCNumSamplesInBuffer ;

     if FirstCall then begin
        ADCOldestScan := ADCLatestScan ;
        //ADCWriteBuffer := ADCOldestScan - 1 ;
        //if ADCWriteBuffer < 0 then ADCWriteBuffer := ADCNumSamplesInBuffer - 1 ;
        //ADCWriteBuffer := ADCWriteBuffer div ADCNumSamplesInWriteBuffer ;
        ADCWriteBuffer := 0 ;
        ADCActiveBuffer := 0 ;
         FirstCall := False ;
        end
     else ADCActiveBuffer := ADCOldestScan ;//- 1 ;

     // Capacity calculation
     if MainFrm.Cap.Enabled then CalculateCapacity ;

     // Determine A/D write buffer being currently filled

     if ADCActiveBuffer < 0 then ADCActiveBuffer := ADCNumSamplesInBuffer - 1 ;
     ADCActiveBuffer := ADCActiveBuffer div ADCNumSamplesInWriteBuffer ;
//      if RecordingMode <= rmStopADCRecording then
     RecPlotFrm.ADCUpdateDisplay( ADCBuf, ADCNumSamplesInBuffer, ADCOldestScan, ADCLatestScan ) ;

     // Write buffer changed
     // --------------------

     if ADCActiveBuffer <> ADCWriteBuffer then begin

        // Write buffer to file if recording
        BufStart := ADCWriteBuffer*ADCNumSamplesInWriteBuffer ;
        if RecordingMode <= rmStopADCRecording then begin
           FileSeek( MainFrm.IDRFile.EDRFileHandle,EDRFilePointer,0) ;
           NumBytesWritten := FileWrite( MainFrm.IDRFile.EDRFileHandle,
                                         ADCBuf^[BufStart],
                                         ADCNumSamplesInWriteBuffer*2 ) ;
           EDRFilePointer := EDRFilePointer +  Int64(NumBytesWritten) ;
           end ;
        // Re-fill buffer with empty flag
        i := BufStart ;
        while i < BufStart+ADCNumSamplesInWriteBuffer do begin
          ADCBuf^[i] := EmptyFlag ;
          Inc(i) ;
          ADCBuf^[i] := -EmptyFlag ;
          Inc(i) ;
          end ;

        // Increment to next write buffer
        Inc(ADCWriteBuffer) ;
        if ADCWriteBuffer >= ADCNumWriteBuffers then ADCWriteBuffer := 0 ;

        // Terminate recording after final buffer has been collected
        if RecordingMode = rmFrameRecordingStopped then begin
           RecordingMode := rmStopADCRecording ;
           end
        else if RecordingMode = rmStopADCRecording then begin
           StopRecording ;
           end ;

        end ;

    end ;


procedure TRecordFrm.TimerTimer(Sender: TObject);
// ---------------------------------------------------
// Scheduled timer event supervising recording/display
// ---------------------------------------------------
var
     i,j,OldFrameNum,FrameCount,
     iFlag : Integer ;               // Empty flag pixel offset within frame
     FirstFrame,LastFrame,FirstFrameToSave,iStart,StartIllumination : Integer ;
     iFrame,jFrame : Integer ;
     Done,BufferFull : Boolean ;
     FrameType : Integer ;

     StimulusStatus : String ;
     RecordingStatus : String ;
     DebugMessage : String ;
     NumFramesInHalfBuffer : Integer ;
     NumFramesToSave : Integer ;
     iType,iFrameType : Integer ;
     NextAvailableFrame : Integer ;
     NextAvailableType : Integer ;
     OldPosition : Integer ;
     iBuf : Pointer ;
begin

    if PFrameBuf = Nil then Exit ;

    if TimerProcBusy then Exit ;

    TimerProcBusy := True ;
    BufferFull := False ;

    // Restart camera if a change to camera settings elsewhere requires it
    if MainFrm.Cam1.CameraRestartRequired and
       (RecordingMode <> rmRecordingInProgress) then begin
       StopCamera ;
       StartCamera ;
       TimerProcBusy := False ;
       end ;

    // Restart camera
    if CameraRestartRequired then begin
       // Close and re-open file to make directory entry permanent
       RestartCamera ;
       TimerProcBusy := False ;
       LowerBufferFilling := True ;
       MainFrm.IDRFile.NumFrames := MainFrm.IDRFile.NumFrames
                                    - (MainFrm.IDRFile.NumFrames mod Max(FrameTypeCycleLength,1)) ;
       LogFrm.AddLine( format('Restart at %d',[MainFrm.IDRFile.NumFrames]));
       Exit ;
       end;

    // Calculate and display frame acquisition rate
    TimeLast := TimeNow ;
    TimeNow := TimeGetTime ;
    TimeDiff :=  TimeNow - TimeLast ;

    // Resize controls on form

    if FormResizeCounter > 1 then Dec(FormResizeCounter) ;
    if FormResizeCounter = 1 then begin
       // Identification field
       IdentGrp.Width := Max( 2,ClientWidth - IdentGrp.Left - 5 );
       edIdent.Width := Max( 2,IdentGrp.Width - edIdent.Left - 5 );

       SetImagePanels ;

       ShowHideImageCaptureSettingsPanel ;
       ShowHideDisplaySettingsPanel ;
       ShowHideShadeCorSettingsPanel ;
       ShowHideLightStimPage ;
       ShowHideZStackSettings ;
       ResizeControlPanel ;
       FormResizeCounter := 0 ;
       end ;


    if RecordingMode <> rmRecordingInProgress then begin
       // Determine if recording to file is possible
       bRecord.Enabled := True ;
       if not MainFrm.IDRFile.Open then bRecord.Enabled := False
       else if MainFrm.IDRFile.NumFrames > 0 then begin
          // Frame sizes differ
          if (MainFrm.IDRFile.FrameWidth <> FrameWidth) or
             (MainFrm.IDRFile.FrameHeight <> FrameHeight) or
             ((MainFrm.IDRFile.NumFrameTypes <> NumFrameTypes) and not MainFrm.IDRFile.SpectralDataFile)
             then  bRecord.Enabled := False ;

          if (cbRecordingMode.ItemIndex <> rmContinuous) then begin
             if Abs(MainFrm.IDRFile.FrameInterval-(edTimeLapseInterval.Value/NumFrameTypes)) > 1E-4 then
                bRecord.Enabled := False ;
             end
           else begin
             if Abs(MainFrm.IDRFile.FrameInterval-MainFrm.Cam1.FrameInterval) > 1E-4 then
                bRecord.Enabled := False ;
             end ;
          end ;

       if MainFrm.IdentChanged then begin
          edIdent.Text := MainFrm.IDRFile.Ident ;
          MainFrm.IdentChanged := False ;
          end ;

       end ;

    StartIllumination := StartBurstAtFrame - Round(BurstIlluminationDelay/MainFrm.Cam1.FrameInterval) ;

    if (RecordingMode = rmRecordingInProgress) and
       (cbRecordingMode.ItemIndex = rmTimelapseBurst) and
       (not BurstIlluminationOn) and
       (FrameRateCounter >= StartIllumination) then begin
       BurstIlluminationOn := True ;
       UpdateLightSource ;
       // Update emission filters
       UpdateEMFilter ;
       end ;

     // Update D/A output buffer
     LabIO.UpdateDACOutputBuffer ;
     LabIO.UpdateDIGOutputBuffer ;

     if {MainFrm.Cam1.CameraActive}True then begin

        // Keep frame count
        OldFrameNum := FrameNum ;

        // Initiate transfer from frame grabber
        // (for boards which lack host DMA transfer)
        MainFrm.Cam1.ReadCamera ;

        // Find latest frame that has been acquired
        // ----------------------------------------

        FrameCount := 0 ;
        Done := False ;
        while not Done do begin

           // Keep frame within buffer
           if FrameNum >= NumFramesInBuffer then FrameNum := 0 ;

           // Set pointer to flag pixel
           iFlag :=  FrameNum*NumPixelsPerFrame ;

           // If image available for this frame, get frame#
           if ByteImage then begin
              // 8 bit pixel frames
              if (pByteArray(PFrameBuf)^[iFlag+1] = ByteHiValue) and
                 (pByteArray(PFrameBuf)^[iFlag] = ByteLoValue) then Done := True ;
              if (pByteArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-1] = ByteHiValue) and
                 (pByteArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-2] = ByteLoValue) then Done := True ;
              end
           else begin
              // 16 bit pixel frames
              if (pBigWordArray(PFrameBuf)^[iFlag+1] = WordHiValue) and
                 (pBigWordArray(PFrameBuf)^[iFlag] = WordLoValue) then Done := True ;
              if (pBigWordArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-1] = WordHiValue) and
                 (pBigWordArray(PFrameBuf)^[iFlag+NumPixelsPerFrame-2] = WordLoValue) then Done := True ;
              end ;

           if not Done then begin

              // Is half-buffer full ?
              BufferFull := False ;
              if LowerBufferFilling then begin
                 if FrameNum >= LastFrameInLowerBuffer then BufferFull := True ;
                 end
              else begin
                 if FrameNum < FirstFrameInUpperBuffer then BufferFull := True ;
                 end ;

              // Get type of this frame
              FrameType := FrameTypeCycle[FrameRateCounter mod Max(FrameTypeCycleLength,1)] ;

              // Update to ROI time course
              if (cbRecordingMode.ItemIndex = rmContinuous) or
                 ((cbRecordingMode.ItemIndex = rmTimeLapse) and (RecordingMode <> rmRecordingInProgress)) or
                 ((cbRecordingMode.ItemIndex = rmTimeLapseBurst) and (RecordingMode <> rmRecordingInProgress)) then begin
                 ReadROI( FrameNum, FrameType, pFrameBuf ) ;
                 end ;

              Inc(NumFramesTotal) ;
              // Most recent available frames (indexed by type)
              LatestFrames[FrameType] := FrameNum  ;

              // Increment frame pointer
              Inc(FrameNum) ;
              if FrameNum >= NumFramesInBuffer then FrameNum := 0 ;

              // Emergency exit when buffer overflow occurs
              Inc(FrameCount) ;
              if FrameCount >= (NumFramesInBuffer div 2) then Done := True ;

              // Increment total no. of frames acquired since camera start
              Inc(FrameRateCounter) ;

              end ;

           end ;

        // Update time course
        if (cbRecordingMode.ItemIndex = rmContinuous) then RecPlotFrm.FLUpdateDisplay( rbSpectrum.Checked );

        // Display latest frame
        // --------------------

        if OldFrameNum <> FrameNum then begin

           if TimeNow <> TStart then
              FrameRate := FrameRateCounter/((TimeNow-TStart)*0.001)
           else FrameRate := 0.0 ;

           if TimeLast > 0 then TimeDiffMax := Max(TimeDiff,TimeDiffMax) ;

           RecordingStatus := '' ;

           RecordingStatus := format( ' %8.2f fps F=%3.3d/%3.3d (%3.3d%%)',
                                 [FrameRate,
                                  FrameNum+1,
                                  NumFramesInBuffer,
                                  Round(Ceil(FrameRate*FrameInterval*100.0/NumFramesPerCCD))
                                  ]) ;

           if RecordingMode = rmRecordingInProgress then begin
              RecordingStatus := RecordingStatus + 'Recording: ' ;
              if TimeLapseMode then  RecordingStatus := RecordingStatus + '[Time lapse] ' ;

              RecordingStatus := RecordingStatus + format( 'Frames %5d/%5d (%.2fs) %s',
                                                  [NumFramesDone,
                                                   NumFramesRequired,
                                                   NumFramesDone*MainFrm.IDRFile.FrameInterval,
                                                   BufferOverFlowMessage]) ;

              if cbRecordingMode.ItemIndex = rmTimeLapseBurst then
                 RecordingStatus := RecordingStatus + format('Burst at %.2fs: ',[StartBurstAtFrame*IDRFileBurst.FrameInterval]) + BurstFileName ;

              end ;

           if StimulusRequired then begin
              StimulusStatus := 'Stim: ' + ExtractFileName(Stimulator.Prog.FileName) ;
              end
           else StimulusStatus := '' ;

           DebugMessage := format(' Temp=%5.0f C',
                          [MainFrm.Cam1.CameraTemperature]) ;

           RecordingStatus :=RecordingStatus + StimulusStatus ;
           RecordingStatus :=RecordingStatus ;
           if RecordingMode = rmRecordingInProgress then  RecordingStatus :=RecordingStatus + DebugMessage ;

           if (TimerTickCount mod 2) = 0 then MainFrm.StatusBar.SimpleText := RecordingStatus ;
           Inc(TimerTickCount) ;

           // Display frame
           if (cbRecordingMode.ItemIndex = rmContinuous) or (RecordingMode <> rmRecordingInProgress) then begin
              // Find next available frame
              NextAvailableFrame := -1 ;
              for i  := FrameTypeToBeDisplayed to FrameTypeToBeDisplayed+NumFrameTypes-1 do begin
                  iType := i mod NumFrameTypes ;
                  if LatestFrames[iType] >= 0 then begin
                     NextAvailableFrame := LatestFrames[iType] ;
                     NextAvailableType := iType ;
                     Break ;
                     end;
                  end;
              // Display (if available)
              if NextAvailableFrame >= 0 then begin
                 DisplayImage( NextAvailableFrame*NumPixelsPerFrame,
                               NextAvailableType,
                               MainFrm.LUTs[NextAvailableType*LUTSize],
                               BitMaps[NextAvailableType],
                               Images[NextAvailableType] ) ;
                 LatestFrames[NextAvailableType] := -1 ;

                 Dec(OptimiseContrastCount) ;
                 // Update next priority frame to be displayed
                 Inc(FrameTypeToBeDisplayed) ;
                 if FrameTypeToBeDisplayed >= NumFrameTypes then FrameTypeToBeDisplayed := 0 ;
                 end ;

              // Select next frame type to display
              RecPlotFrm.FLUpdateDisplay( rbSpectrum.Checked ) ;

              end ;
           TNoFrames := 0.0 ;
           end
        else begin
           // If camera has stopped supplying frames, restart camera
           TNoFrames := TNoFrames + Timer.Interval*0.001 ;
           if TNoFrames > (MainFrm.Cam1.FrameInterval*NumFramesInBuffer*0.5) then CameraRestartRequired := True ;
           end ;

        // Restore empty frame flags / write to file
        // -----------------------------------------

        // If half-buffer is full ...
        // update empty flags and write to disk if necessary
        //
        If BufferFull then begin

           // Get frame range of full buffer
           if LowerBufferFilling then begin
              FirstFrame := FirstFrameInLowerBuffer ;
              LastFrame := LastFrameInLowerBuffer ;
              end
           else begin
              FirstFrame := FirstFrameInUpperBuffer ;
              LastFrame := LastFrameInUpperBuffer ;
              end ;

           // If buffer being written to disk has not had empty flags reset do it now
           if ReplaceEmptyFlags then begin
              FillBufferWithEmptyFlags( EmptyFlagsFirst, EmptyFlagsLast ) ;
              if BufferOverFlowMessage = '' then begin
                 BufferOverFlowMessage := ' [BUFFER OVERFLOW!]' ;
                 LogFrm.AddLine(BufferOverFlowMessage);
                 end;
              end;

           EmptyFlagsFirst := FirstFrame ;
           EmptyFlagsLast := LastFrame ;
           ReplaceEmptyFlags := True ;

            // Save to file, if in recording mode
           if (RecordingMode = rmRecordingInProgress) or
              (RecordingMode = rmStopFrameRecording) then begin

              NumFramesInHalfBuffer := NumFramesInBuffer div 2 ;

              if cbRecordingMode.ItemIndex = rmContinuous then begin
                 // Continuous recording - save all frames
                 iStart := FirstFrame*NumBytesPerFrame ;
                 NumFramesToSave := Min(NumFramesRequired - NumFramesDone,NumFramesInHalfBuffer) ;
                 MainFrm.IDRFile.AsyncSaveFrames( MainFrm.IDRFile.NumFrames + 1,
                                                  NumFramesToSave,
                                                  @pByteArray(PFrameBuf)[iStart]) ;
                 NumFramesDone :=  NumFramesDone + NumFramesToSave ;
                 if NumFramesDone >= NumFramesRequired then RecordingMode := rmStopFrameRecording ;
                 end
              else begin

                 // Time lapse recording mode

                 // Save burst (if in burst mode)
                 if (cbRecordingMode.ItemIndex = rmTimelapseBurst) and
                    (TimeLapseFrameCounter >= StartBurstAtFrame) then begin

                    // Start burst with type 0 frame.
                    FirstFrameToSave := FirstFrame ;
                    if NumBurstFramesDone = 0 then begin
                       while ((TimeLapseFrameCounter + (FirstFrameToSave-FirstFrame)) mod NumFrameTypes) <> 0 do
                          Inc(FirstFrameToSave) ;
                       end ;

                    if FirstFrameToSave <= LastFrame then begin
                       iStart := FirstFrameToSave*NumBytesPerFrame ;
                       NumFramesToSave := Min( NumBurstFramesRequired - NumBurstFramesDone,
                                               LastFrame - FirstFrameToSave + 1) ;
                       IDRFileBurst.AsyncSaveFrames( IDRFileBurst.NumFrames + 1,
                                                     NumFramesToSave,
                                                     @pByteArray(PFrameBuf)[iStart]) ;
                       NumBurstFramesDone :=  NumBurstFramesDone + NumFramesToSave ;
                       end ;

                    if NumBurstFramesDone >= NumBurstFramesRequired then begin
                       IDRFileBurst.CloseFile ;
                       Inc(BurstCounter) ;
                       BurstFileName := ANSIReplaceText(
                                        MainFrm.IDRFile.FileName,'.idr',format('.%d.idr',[BurstCounter])) ;
                       IDRFileBurst.CreateFileFrom( BurstFileName,MainFrm.IDRFile,False ) ;
                       IDRFileBurst.FrameInterval := MainFrm.Cam1.FrameInterval ;
                       NumBurstFramesDone := 0 ;
                       NumBurstFramesRequired := Max( Round(edBurstDuration.Value/MainFrm.Cam1.FrameInterval),
                                                      NumFrameTypes)*NumFramesPerCCD ; ;
                       StartBurstAtFrame := StartBurstAtFrame +
                                            Max( Round(edBurstInterval.Value/MainFrm.Cam1.FrameInterval)*NumFramesPerCCD,
                                                 NumFramesInHalfBuffer ) ;

                       IDRFileBurst.Ident := MainFrm.IDRFile.Ident +
                                             format('Burst T=%.2fs Frame=%d ',[StartBurstAtFrame*IDRFileBurst.FrameInterval,StartBurstAtFrame]);
                       LogFrm.AddLine( MainFrm.IDRFile.Ident + BurstFileName ) ;
                       BurstIlluminationOn := False ;
                       UpdateLightSource ;
                       // Update emission filters
                       UpdateEMFilter ;
                       end ;

                    end ;

                 // Extract selected time lapse frames
                 j := 0 ;
                 NumFramesToSave := 0 ;
                 TimeLapseFrameInterval := Max(TimeLapseFrameInterval,1) ;
                 for iFrame := 0 to NumFramesInHalfBuffer-1 do begin
                     jFrame := TimeLapseFrameCounter + iFrame ;
                     if (jFrame mod TimeLapseFrameInterval) < NumFrameTypes then begin
                        iStart := (FirstFrame + iFrame)*NumBytesPerFrame ;
                        for i := iStart to iStart + NumBytesPerFrame-1 do begin
                            pByteArray(PTimeLapseBuf)^[j] := pByteArray(PFrameBuf)^[i] ;
                            Inc(j) ;
                            end ;
                        Inc(NumFramesToSave) ;

                        iFrameType := jFrame mod NumFrameTypes ;
                        DisplayImage( (FirstFrame + iFrame)*NumPixelsPerFrame,
                                      iFrameType,
                                      MainFrm.LUTs[iFrameType*LUTSize],
                                      BitMaps[iFrameType],
                                      Images[iFrameType] ) ;

                        // Update time course display
                        ReadROI( FirstFrame+iFrame,iFrameType,pFrameBuf ) ;

                        end ;
                     end ;
                 TimeLapseFrameCounter := TimeLapseFrameCounter + NumFramesInHalfBuffer ;

                 // Save time lapse frames
                 if (NumFramesToSave > 0) and (NumFramesDone < NumFramesRequired) then begin
                    NumFramesToSave := Min(NumFramesRequired - NumFramesDone,NumFramesToSave) ;
                    MainFrm.IDRFile.AsyncSaveFrames( MainFrm.IDRFile.NumFrames + 1,
                                                     NumFramesToSave,
                                                     PTimeLapseBuf) ;
                    NumFramesDone :=  NumFramesDone + NumFramesToSave ;
                    if NumFramesDone >= NumFramesRequired then RecordingMode := rmStopFrameRecording ;

                    // Restart camera if time lapse images are all at dark level
                    CameraRestartRequired := AreImagesDark( PTimeLapseBuf, NumFramesToSave*NumPixelsPerFrame) ;
                    TimeLapsePreserveFile := True ;
                    end
                 else if (MainFrm.TimeLapseInterval > 2.0) and
                      (not MainFrm.IDRFile.AsyncWriteInProgress) and TimeLapsePreserveFile then begin
                       // Close and re-open file to make directory entry permanent (for intervals > 2s)
                       MainFrm.IDRFile.CloseFile ;
                       MainFrm.IDRFile.OpenFile( MainFrm.IDRFile.FileName ) ;
                       if not MainFrm.IDRFile.WriteEnabled then MainFrm.IDRFile.WriteEnabled := True ;
                       TimeLapsePreserveFile := False ;
                       end;

                 RecPlotFrm.FLUpdateDisplay( rbSpectrum.Checked );

                 end ;

              // Stop recording if flag set
              if RecordingMode = rmStopFrameRecording then begin
                 RecordingMode := rmStopADCRecording ;
                 end ;

              end ;

           // Force last frame displayed to end of buffer
           if (LastFrameDisplayed >= FirstFrame) and
              (LastFrameDisplayed < LastFrame) then LastFrameDisplayed := LastFrame ;
           // Toggle buffer in use flag
           LowerBufferFilling := not LowerBufferFilling ;

           end ;

        // Optimise contrast if required
        if OptimiseContrastCount = 0 then begin
           bMaxContrast.Click ;
           if ckAutoOptimise.Checked then OptimiseContrastCount := Max(Round(2.0/FrameInterval),2*NumFrameTypes) ;
           end ;

        if (RecordingMode = rmRecordingInProgress) and
           (NumFramesDone >= NumFramesRequired) then begin
           MainFrm.StatusBar.SimpleText := format( 'Recording Complete: %d Frames collected',
                                                       [NumFramesDone]) ;
           //MainFrm.Recording := False ;
           bStop.Click ;
           end ;

        // Stop recording if flag set
        if RecordingMode = rmStopADCRecording then begin
           if not ADCRunning then StopRecording ;
           end ;

        end ;

     // Add empty frame flags to buffer (if required)
     if ReplaceEmptyFlags and (not MainFrm.IDRFile.AsyncWriteInProgress) then begin
        FillBufferWithEmptyFlags( EmptyFlagsFirst, EmptyFlagsLast ) ;
        ReplaceEmptyFlags := False ;
        end;

    // Re-enable Start Stimulus button after stimulus is complete
    if (not bStartStimulus.Enabled) and
       (TimeNow >= TReEnableStartStimulusButton) then begin
       StimulusRequired := False ;
       if Stimulator.Prog.NextProtocolFileName <> '' then begin
          LoadNextProtocolFile := True ;
          StimulusRequired := True ;
          end
       else begin
          LoadNextProtocolFile := False ;
          StimulusRequired := False ;
          bStartStimulus.Enabled := True ;
          bStopStimulus.Enabled := False ;
          end ;
       UpdateStimulusWaveforms( StimulusRequired, False ) ;

       end ;

    // Re-enable Start Photostimulus button after stimulus is complete
    if (not bStartPhotoStimulus.Enabled) and
       (TimeNow >= TReEnableStartPhotoStimulusButton) then bStartPhotoStimulus.Enabled := True ;

    // XY Stage increment (in time lapse modes only)
    if (RecordingMode = rmRecordingInProgress) and
       (cbRecordingMode.ItemIndex <> rmContinuous) and
        XYStageFrm.IncrementStagePosition then begin
        // Set initial move
       if TimeLapseFrameCounter <= 0 then begin
          MoveXYStageAtFrame := TimeLapseFrameInterval div 2 ;
          end;
       // Write frames acquired at this stage position to position sub-file
       if TimeLapseFrameCounter >= MoveXYStageAtFrame then begin
          IDRFileXY.OpenFile( XYStageFileNames[XYStageFrm.Position]) ;
          IDRFileXY.WriteEnabled := True ;
          GetMem( iBuf, NumPixelsPerFrame*SizeOf(Integer) ) ;
          outputdebugstring(pchar(format('%s %d %d',[XYStageFileNames[XYStageFrm.Position],XYStageLastFrame,MainFrm.IDRFile.NumFrames])));
          while XYStageLastFrame <= MainFrm.IDRFile.NumFrames do begin
               MainFrm.IDRFile.LoadFrame( XYStageLastFrame, iBuf ) ;
               IDRFileXY.SaveFrame( IDRFileXY.NumFrames + 1, iBuf ) ;
               Inc(XYStageLastFrame) ;
               end;
          IDRFileXY.CloseFile ;
          FreeMem( iBuf ) ;

          XYStageFrm.Position := XYStageFrm.Position + 1 ;
          MoveXYStageAtFrame := MoveXYStageAtFrame + TimeLapseFrameInterval ;
          end;

       end;


    // Update A/D signal display
    if MainFrm.ADCNumChannels > 0 then UpdateADCDisplay ;

     TimerProcBusy := False ;

     end;

function TRecordFrm.AreImagesDark(
          pBuf : Pointer ;
          NumPixels : Integer ) : Boolean ;
// ------------------------------------------------------------------------
// Return TRUE if all pixels within image buffer are a dark level of camera
// (dark level defined by DarkLevelLo and DarkLevelHi limits)
// ------------------------------------------------------------------------
var
    i,Y,YMax,YMin : Integer ;
begin

     YMax := -1 ;
     YMin := MainFrm.Cam1.GreyLevelMax ;
     if MainFrm.Cam1.NumBytesPerPixel > 1 then begin
        for i := 0 to NumPixels-1 do begin
            y := PWordArray(pBuf)^[i] ;
            if Y > YMax then YMax := Y ;
            if Y < YMin then YMin := Y ;
            end ;
        end
     else begin
        for i := 0 to NumPixels-1 do begin
            y := PByteArray(pBuf)^[i] ;
            if Y > YMax then YMax := Y ;
            if Y < YMin then YMin := Y ;
            end ;
        end;

    if (YMin >= MainFrm.DarkLevelLo) and (YMax <= MainFrm.DarkLevelHi ) then Result := True
                                                                        else Result := False ;

    end;


procedure TRecordFrm.ReadROI(
          FrameNum : Integer ;
          FrameType : Integer ;
          pImageBuf : Pointer ) ;
// ----------------------------
// Read selected ROI on display
// ----------------------------
var
    i,iFT : Integer ;
    iX,iY,iX0,iY0,iX1,iY1 : Integer ;
    y : Integer ;
    iStart : Integer ;
    Sum : Single ;
    nAvg : Integer ;
    ROISize : Integer ;
begin

     // Get no. of pixels in ROI square
     ROISize := RecPlotFrm.ROISize ;

     // ROI position
     iX0 := Max(ROIs[RecPlotFrm.SelectedROI].X - ROISize div 2,0) ;
     iX1 := Min(iX0 + ROISize - 1,MainFrm.Cam1.FrameWidth-1) ;
     iY0 := Max(ROIs[RecPlotFrm.SelectedROI].Y - ROISize div 2,0) ;
     iY1 := Min(iY0 + ROISize - 1,FrameHeight-1) ;
     iStart := (FrameNum*NumPixelsPerFrame) + iY0*FrameWidth + iX0 ;

     // Average ROI
     Sum := 0.0 ;
     nAvg := 0 ;
     for iY := 0 to iY1-iY0 do begin
         i := iStart + iY*MainFrm.Cam1.FrameWidth ;
         for iX := 0 to iX1-iX0 do begin
             if ByteImage then Sum := Sum + pByteArray(pImageBuf)^[i]
                          else Sum := Sum + pBigWordArray(pImageBuf)^[i] ;
             Inc(i) ;
             Inc(nAvg) ;
             end ;
         end ;

     Y := Round( Sum/Max(nAvg,1) ) ;
                                            // Subtract background ROI
     if RecPlotFrm.SelectedSubROI >= 0 then begin

        iX0 := Max(ROIs[RecPlotFrm.SelectedSubROI].X - ROISize div 2,0) ;
        iX1 := Min(iX0 + ROISize - 1,MainFrm.Cam1.FrameWidth-1) ;
        iY0 := Max(ROIs[RecPlotFrm.SelectedSubROI].Y - ROISize div 2,0) ;
        iY1 := Min(iY0 + ROISize - 1,FrameHeight-1) ;
        iStart := (FrameNum*NumPixelsPerFrame) + iY0*FrameWidth + iX0 ;

        Sum := 0.0 ;
        nAvg := 0 ;
        for iY := 0 to iY1-iY0 do begin
            i := iStart + iY*MainFrm.Cam1.FrameWidth ;
            for iX := 0 to iX1-iX0 do begin
             if ByteImage then Sum := Sum + pByteArray(pImageBuf)^[i]
                          else Sum := Sum + pBigWordArray(pImageBuf)^[i] ;
               Inc(i) ;
               Inc(nAvg) ;
               end ;
            end ;

        Y := Y - Round( Sum/Max(nAvg,1) ) ;
        end ;

     // Update ROI time course buffer
     LatestROIValue[FrameType] := y ;
     RecPlotFrm.FLDisplayAddPoints( LatestROIValue, NumFrameTypes ) ;

     end ;


procedure TRecordFrm.DisplayImage(
          StartAt : Integer ;          // Index to first pixel of frame in circular buffer [In]
          FrameType : Integer ;        // Type of frame/excitation wavelength being displayed [In]
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
    Ybm,Yim,Xbm,Xim,i,j,StartOfLine,x,y : Integer ;
    iStep : Integer ;
    iEnd : Integer ;
    iDisplayZoom,iz : Integer ;
    PScanLine1 : pByteArray ;    // Bitmap line buffer pointer
    PScanLine : pByteArray ;    // Bitmap line buffer pointer
    PCurrentFrame : PIntArray ; // Pointer to current display frame
begin

    if PFrameBuf = Nil then Exit ;

    // Copy image from circular buffer into 32 bit display buffer

    j := StartAt ;
    PCurrentFrame := PDisplayBufs[FrameType] ;
    if PCurrentFrame = Nil then Exit ;

    if ByteImage then begin
       // 8 bit images
       for i := 0 to NumPixelsPerFrame-1 do begin
           PCurrentFrame^[i] := pByteArray(PFrameBuf)^[j] ;
           Inc(j) ;
           end ;
       end
    else begin
       // 16 bits images
       for i := 0 to NumPixelsPerFrame-1 do begin
           PCurrentFrame^[i] := pBigWordArray(PFrameBuf)^[j] ;
           Inc(j) ;
           end ;
       end ;

    // Do shading correct operations
    DoShadingCorrection( FrameType ) ;

    // Copy image to display bitmap

    if DisplayZoom >= 1.0 then begin
       // ------------------------------
       // X1 and above display zoom factors
       // ------------------------------
       Ybm := 0 ;
       StartOfLine := (sbYScroll.Position*FrameWidth) + sbXScroll.Position ;
       for Yim := sbYScroll.Position to FrameHeight-1 do begin

           // Create line
           PScanLine := BitMap.ScanLine[Ybm] ;
           Xbm := 0 ;
           iDisplayZoom := Round(DisplayZoom) ;
           iz := 0 ;
           i := StartOfLine ;
           iEnd := Min( StartOfLine + MainFrm.Cam1.FrameWidth -1, NumPixelsPerFrame ) ;
           while (i < iEnd) and (Xbm < BitMap.Width) do begin
              PScanLine[Xbm] := LUT[Word(PCurrentFrame^[i])] ;
              Inc(Xbm) ;
              inc(iz) ;
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
       while (Ybm < BitMap.Height) and (Yim < FrameHeight) do begin

          // Get scan line array pointer
          PScanLine := BitMap.ScanLine[Ybm] ;

          // Copy line to bitmap
          xBm := 0 ;
          XIm := sbXScroll.Position ;
          i := (Yim*MainFrm.Cam1.FrameWidth) + XIm ;
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
    //Image.Canvas.FrameRect(CaptureRegion) ;
    DrawCaptureRegion( Image.Canvas, CaptureRegion ) ;

    // Draw ROIs
    Image.Canvas.Pen.Color := clWhite ;
    for i := 0 to NumROIs-1 do begin
       X := Round((ROIs[i].X - sbXScroll.Position)*DisplayZoom) ;
       Y := Round((ROIs[i].Y - sbYScroll.Position)*DisplayZoom) ;
       Image.Canvas.Polyline( [Point(X-ROICrossSize,Y),Point(X+ROICrossSize,Y)]);
       Image.Canvas.Polyline( [Point(X,Y+ROICrossSize),Point(X,Y-ROICrossSize)]);
       Image.Canvas.TextOut( X + (ROICrossSize div 2),
                             Y + (ROICrossSize div 2), format('%d',[i+1])) ;
       end ;

    ImageAvailable := True ;

    // Display frame type at top-left of image
    Images[FrameType].Canvas.TextOut( 0,0,FrameTypes[FrameType]) ;

    // Increment frames acquired counter
    Inc(FrameTypeCounter[FrameType]) ;

    end ;


procedure TRecordFrm.UpdateImage(
          FrameType : Integer ;        // Type of frame/excitation wavelength being displayed [In]
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
    Ybm,Yim,Xbm,Xim,i,StartOfLine,x,y : Integer ;
    iStep : Integer ;
    iEnd : Integer ;
    iDisplayZoom,iz : Integer ;
    PScanLine1 : pByteArray ;    // Bitmap line buffer pointer
    PScanLine : pByteArray ;    // Bitmap line buffer pointer
    PCurrentFrame : PIntArray ; // Pointer to current display frame
begin

    if not ImageAvailable then Exit ;
    if PFrameBuf = Nil then Exit ;

    // Copy image from circular buffer into 32 bit display buffer

    PCurrentFrame := PDisplayBufs[FrameType] ;

    // Copy image to display bitmap

    if DisplayZoom >= 1.0 then begin
       // ------------------------------
       // X1 and above display zoom factors
       // ------------------------------
       Ybm := 0 ;
       StartOfLine := (sbYScroll.Position*MainFrm.Cam1.FrameWidth)
                      + sbXScroll.Position ;
       for Yim := sbYScroll.Position to FrameHeight-1 do begin

           // Create line
           PScanLine := BitMap.ScanLine[Ybm] ;
           Xbm := 0 ;
           iDisplayZoom := Round(DisplayZoom) ;
           iz := 0 ;
           i := StartOfLine ;
           iEnd := Min( StartOfLine + MainFrm.Cam1.FrameWidth -1, NumPixelsPerFrame ) ;
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
           for i := 1 to iDisplayZoom-1 do begin
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
       while (Ybm < BitMap.Height) and (Yim < FrameHeight) do begin

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

    // Draw ROIs
    Image.Canvas.Pen.Color := clWhite ;
    for i := 0 to NumROIs-1 do begin
       X := Round((ROIs[i].X - sbXScroll.Position)*DisplayZoom) ;
       Y := Round((ROIs[i].Y - sbYScroll.Position)*DisplayZoom) ;
       Image.Canvas.Polyline( [Point(X-ROICrossSize,Y),Point(X+ROICrossSize,Y)]);
       Image.Canvas.Polyline( [Point(X,Y+ROICrossSize),Point(X,Y-ROICrossSize)]);
       Image.Canvas.TextOut( X + (ROICrossSize div 2),
                             Y + (ROICrossSize div 2), format('%d',[i+1])) ;
       end ;

    // Display frame type at top-left of image
    Images[FrameType].Canvas.TextOut( 0,0,FrameTypes[FrameType]) ;

    // Increment frames acquired counter
    Inc(FrameTypeCounter[FrameType]) ;

    end ;


procedure TRecordFrm.DrawCaptureRegion(
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


procedure TRecordFrm.DrawSquare(
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


procedure TRecordFrm.DisplayCalibrationBar(
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

     iCalBarSize := Round((MainFrm.CalibrationBarSize*DisplayZoom)/MainFrm.Cam1.PixelWidth) ;
     iCalBarThickness := Max( 1, Round(MainFrm.CalibrationBarThickness) ) ;

     Bitmap.Canvas.Pen.Color := clWhite ;
     Canvas.Pen.Mode := pmCopy ;
     Bitmap.Canvas.Pen.Width := 1 ;
     Bitmap.Canvas.Brush.Style := bsClear ;

     Bitmap.Canvas.Font.Color := clWhite ;

     iTop := Bitmap.Height - Bitmap.Canvas.TextHeight('X') ;
     Bitmap.Canvas.TextOut( 2,
                            iTop,
                            format('%.4g %s',[MainFrm.CalibrationBarSize,MainFrm.Cam1.PixelUnits])) ;

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


procedure TRecordFrm.CalculateMaxContrast(
          FrameType : Integer ) ; // Frame type
// ---------------------------------------------------------
// Calculate and set display for maximum grey scale contrast
// ---------------------------------------------------------
const
    PixelSampleSize = 2000 ;
var
     i,NumPixels,NAvg,Istep : Integer ;
     z,zMean,zSD,zSum : Single ;
     iz,ZMin,ZMax,ZLo,ZHi,ZThreshold : Integer ;
begin

    if PDisplayBufs[FrameType] = Nil then Exit ;

    NumPixels := (FrameHeight*FrameWidth - 4) ;
    iStep := Max(NumPixels div PixelSampleSize,1) ;
    if NumPixels < 2 then Exit ;

    if ckContrast6SDOnly.Checked then begin
       // Set contrast range to +/- 3 x standard deviation
       ZSum := 0.0 ;
       nAvg := 0 ;
       i := 0 ;
       while i < NumPixels do begin
          ZSum := ZSum + PDisplayBufs[FrameType]^[i] ;
          i := i + iStep ;
          Inc(NAvg) ;
          end ;
       ZMean := ZSum / nAvg ;

       ZSum := 0.0 ;
       nAvg := 0 ;
       i := 0 ;
       while i < NumPixels do begin
          Z := PDisplayBufs[FrameType]^[i] ;
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
          iz := PDisplayBufs[FrameType]^[i] ;
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

     MainFrm.UpdateLUT(FrameType, MainFrm.Cam1.GreyLevelMax ) ;

     end ;


procedure TRecordFrm.FormClose(Sender: TObject; var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
var
    Device : SmallInt ;
begin

     FormClosing := True ;

     Timer.Enabled := False ;

     // Close shutter
     rbEXCShutterOpen.Checked := False ;
     rbEXCShutterClosed.Checked := True ;
     UpdateLightSourceShutter ;

     // Stop acquiring images
     StopCameraAndAnalogIO ;

     for Device := 1 to LabIO.NumDevices do begin
         if DACBufs[Device] <> Nil then begin
            FreeMem(DACBufs[Device]) ;
            DACBufs[Device] := Nil ;
            end ;
         if DIGBufs[Device] <> Nil then begin
            FreeMem(DIGBufs[Device]) ;
            DIGBufs[Device] := Nil ;
            end ;
         end ;

     if PTimeLapseBuf <> Nil then begin
        FreeMem(PTimeLapseBuf) ;
        PTimeLapseBuf := Nil ;
        end ;

     // Save display zoom setting
     MainFrm.DisplayZoomIndex := cbDisplayZoom.ItemIndex ;

     if MainFrm.FormExists( 'RecPlotFrm' ) then begin
        RecPlotFrm.AllowClose := True ;
        RecPlotFrm.Close ;
        end ;

     // Request destruction of form
     Action := caFree ;

     end;


procedure TRecordFrm.bRecordClick(Sender: TObject);
// ---------------------------------
// Starting recording images to disk
// ---------------------------------
var
     i : Integer ;
     NumADCScans : Int64 ;
begin

     // Close other forms to avoid possible interruption of recording
     if MainFrm.FormExists('SealTestFrm') then SealTestFrm.Close ;

     // Recording not allowed if no data file open
     if not MainFrm.IDRFile.Open then begin
        // Report failure to record
        Caption := 'Record Images & Signals: Unable to record! No data file open!' ;
        exit ;
        end ;

     // Recording not allowed if data contains frames of a different size
     if MainFrm.IDRFile.NumFrames > 0 then begin
        if (MainFrm.IDRFile.FrameWidth <> FrameWidth) and
           (MainFrm.IDRFile.FrameHeight <> FrameHeight) then begin
           Caption := 'Record Images & Signals: Unable to record! Camera frame size does not match existing frames in file!' ;
           Exit ;
           end ;
        end ;

     // Check for available disk space
     if not MainFrm.IDRFile.DiskSpaceAvailable( MainFrm.NumFramesRequired ) then begin
        Caption := 'Record Images & Signals: Not enough disk space!' ;
        exit ;
        end ;

     Caption := 'Record Images & Signals: RECORDING' ;

     // Stop image capture
     StopCamera ;

     // Enable file for writing
     if not MainFrm.IDRFile.WriteEnabled then MainFrm.IDRFile.WriteEnabled := True ;

     // Set dimensions of frame stored in file
     MainFrm.IDRFile.FrameWidth := FrameWidth ;
     MainFrm.IDRFile.FrameHeight := FrameHeight ;
     if ckZStackEnabled.Checked and ZStageGrp.Visible then begin
        MainFrm.IDRFile.NumZSections := Round(edZNumSteps.Value) ;
        MainFrm.IDRFile.ZStart := edZStartPos.Value ;
        MainFrm.IDRFile.ZSpacing := edZStepSize.Value ;
        end
     else begin
        MainFrm.IDRFile.NumZSections := 1 ;
        MainFrm.IDRFile.ZStart := 0.0 ;
        MainFrm.IDRFile.ZSpacing := 1.0 ;
        end ;

     // Turn on excitation if required while recording
     if ckExcitationOnWhenRecording.Checked then begin
        rbEXCShutterOpen.Checked := True ;
        rbEXCShutterClosed.Checked := False ;
        end ;

     MainFrm.Recording := True ;
     bRecord.Enabled := False ;
     bStop.Enabled := True ;
     ImageCaptureGrp.Enabled := False ;
     cbDisplayZoom.Enabled := False ;
     WavelengthGrp.Enabled := False ;
     //StimulatorTab.Enabled := False ;
     TimeLapsePanel.Enabled := False ;
     BurstModePanel.Enabled := False ;
     ckAutoOptimise.Checked := False ;       // Turn off auto-contrast while recording

     if MainFrm.IDRFile.NumMarkers < MaxMarker then bMark.Enabled := True ;
     RecordingMode := rmRecordingInProgress ;

     // if this is not a continuation of an existing recording sequence
     if not ContinuedRecording then begin
        // Set no. of frames required and collected
        NumFramesRequired := GetNumFramesRequired ;
        NumFramesDone := 0 ;
        // Request stimulus if a program has been selected
        if cbStimProgram.ItemIndex > 0 then StimulusRequired := True ;
        end ;
     ContinuedRecording := False ;

     LowerBufferFilling := True ;

     // Set dimensions of frame stored in file
     MainFrm.IDRFile.FrameWidth := FrameWidth ;
     MainFrm.IDRFile.FrameHeight := FrameHeight ;
     MainFrm.IDRFile.PixelDepth := MainFrm.Cam1.PixelDepth ;

     // (Ensure that additions to file starts on a multiple of frame types in file)
     MainFrm.IDRFile.NumFrames := MainFrm.IDRFile.NumFrames
                                  - (MainFrm.IDRFile.NumFrames mod Max(FrameTypeCycleLength,1)) ;

     MainFrm.IDRFile.IntensityScale := 1.0 ;
     MainFrm.IDRFile.IntensityOffset := 1.0 ;

     MainFrm.IDRFile.ADCScanInterval := DACUpdateInterval ;
     // Get camera pixel size
     MainFrm.IDRFile.XResolution := MainFrm.Cam1.PixelWidth ;
     MainFrm.IDRFile.ResolutionUnits := MainFrm.Cam1.PixelUnits ;

     // Update IDR file channels with current settings
     MainFrm.IDRFile.ADCNumChannels := MainFrm.ADCNumChannels ;
     for i := 0 to MainFrm.ADCNumChannels-1 do
         MainFrm.IDRFile.ADCChannel[i] := MainFrm.ADCChannel[i] ;
     MainFrm.IDRFile.ADCVoltageRange := MainFrm.ADCVoltageRange ;
     MainFrm.IDRFile.ADCMaxValue := LabIO.ADCMaxValue[ADCDevice] ;

     // Save frame types
     MainFrm.IDRFile.NumFrameTypes := NumFrameTypes ;
     for i := 0 to NumFrameTypes-1 do begin
         MainFrm.IDRFile.FrameType[i] := FrameTypes[i] ;
         MainFrm.IDRFile.FrameTypeDivideFactor[i] := MainFrm.EXCSequence[i,MainFrm.EXCSequenceNum].DivideFactor ;
         end ;

     // Write ident info to log
     MainFrm.IDRFile.Ident := edIdent.Text ;
     LogFrm.AddLine( MainFrm.IDRFile.Ident ) ;

     if rbSpectrum.Checked then begin
        // Spectral data file
        MainFrm.IDRFile.SpectralDataFile := True ;
        MainFrm.IDRFile.SpectrumStartWavelength := MainFrm.EXCSpectrumStartWavelength ;
        MainFrm.IDRFile.SpectrumEndWavelength := MainFrm.EXCSpectrumEndWavelength ;
        MainFrm.IDRFile.SpectrumStepSize := MainFrm.EXCSpectrumStepSize ;
        MainFrm.IDRFile.SpectrumBandwidth := MainFrm.EXCSpectrumBandwidth ;

        LogFrm.AddLine( format( 'Spectrum: %.0f - %.0f nm, %.0f nm steps (%.0f nm bandwidth)',
                                [MainFrm.EXCSpectrumStartWavelength,
                                 MainFrm.EXCSpectrumEndWavelength,
                                 MainFrm.EXCSpectrumStepSize,
                                 MainFrm.EXCSpectrumBandwidth])) ;
        end
     else MainFrm.IDRFile.SpectralDataFile := False ;

     // Setup for continuous or time lapse recording
     if cbRecordingMode.ItemIndex = rmContinuous then begin
        // ** Continuous recording mode
        MainFrm.IDRFile.FrameInterval := MainFrm.Cam1.FrameInterval / Max(NumFramesPerCCD,1) ;
        TimeLapseMode := False ;
        LogFrm.AddLine( format(
        'Continuous recording started at frame %d (%d frames@%.4gs intervals, %.4gs)',
        [MainFrm.IDRFile.NumFrames+1,
         NumFramesRequired,
         MainFrm.IDRFile.FrameInterval,
         NumFramesRequired*MainFrm.IDRFile.FrameInterval] )) ;

        // Ensure display window has sufficiently long duration
        MainFrm.ADCDisplayWindow := Max( MainFrm.ADCDisplayWindow,10*MainFrm.IDRFile.FrameInterval ) ;

        end
     else begin
        // ** Time lapse recording mode
        // No stimulus programs allowed in time lapse mode
        //cbStimProgram.ItemIndex := 0 ;

        // Interval Must be a multiple of exposure time x no. of frametypes
        // and no shorter than 5 x no, frames types
        TimeLapseFrameInterval := GetTimeLapseFrameInterval ;

        // Create time lapse image buffer
        if PTimeLapseBuf <> Nil then begin
           FreeMem( PTimeLapseBuf ) ;
           PTimeLapseBuf := Nil ;
           end ;
        GetMem( PTimeLapseBuf,
                NumPixelsPerFrame*(NumFramesInBuffer div 2)*MainFrm.Cam1.NumBytesPerPixel ) ;

        TimeLapseFrameCounter := 0 ;

        TimeLapseMode := True ;
        LogFrm.AddLine( format(
        'Record: Time-lapse Recording started at frame %d (%d frames@%.4gs intervals, %.2fs)',
                     [MainFrm.IDRFile.NumFrames+1,
                      NumFramesRequired,
                      MainFrm.IDRFile.FrameInterval,
                      NumFramesRequired*MainFrm.IDRFile.FrameInterval] )) ;

        // Ensure display window has sufficiently long duration
        MainFrm.ADCDisplayWindow := Max( MainFrm.ADCDisplayWindow,10*edTimeLapseInterval.Value ) ;

        // Initialise burst counter and file if in time lapse + burst mode
        if cbRecordingMode.ItemIndex = rmTimelapseBurst then begin
           BurstFileName := ANSIReplaceText(
                            MainFrm.IDRFile.FileName,'.idr',format('.%d.idr',[BurstCounter])) ;
           IDRFileBurst.CreateFileFrom( BurstFileName,MainFrm.IDRFile,False ) ;
           IDRFileBurst.FrameInterval := MainFrm.Cam1.FrameInterval / Max(NumFramesPerCCD,1);
           BurstCounter := 0 ;
           NumBurstFramesDone := 0 ;
           NumBurstFramesRequired := NumFramesPerCCD*Max(Round(edBurstDuration.Value/MainFrm.Cam1.FrameInterval),NumFrameTypes) ;
           StartBurstAtFrame := NumFramesPerCCD*Max(Round(edBurstInterval.Value/MainFrm.Cam1.FrameInterval),1) ;
           IDRFileBurst.Ident := MainFrm.IDRFile.Ident +
                                 format('Burst T=%.2fs Frame=%d',[StartBurstAtFrame*IDRFileBurst.FrameInterval,StartBurstAtFrame]);
           LogFrm.AddLine( IDRFileBurst.Ident + BurstFileName ) ;
           end
        else BurstFileName := '' ;

        end ;

     NumFramesTotal := MainFrm.IDRFile.NumFrames ;

     // Move file pointer to end of A/D data file (if in use)
     if MainFrm.ADCNumChannels > 0 then begin
        NumADCScans := Round( (MainFrm.IDRFile.NumFrames*MainFrm.IDRFile.FrameInterval) /
                               MainFrm.IDRFile.ADCScanInterval ) ;
        EDRFilePointer := cNumEDRHeaderBytes +
                          (NumADCScans*MainFrm.ADCNumChannels*2) ;
        FileSeek(MainFrm.IDRFile.EDRFileHandle, EDRFilePointer, 0 ) ;
        end ;

     // Keep background images intact during StartCamera
     if ckBackgroundSubtraction.Enabled then KeepBackgroundBufs := True
                                        else KeepBackgroundBufs := False ;

     // Start stimulus (if required and available)
     if ckStartStimOnRecord.Checked and (cbStimProgram.ItemIndex > 0) then begin
        StimulusRequired := True ;
        bStartStimulus.Enabled := False ;
        bStopStimulus.Enabled := True ;
        end
     else begin
        StimulusRequired := False ;
        bStartStimulus.Enabled := True ;
        bStopStimulus.Enabled := False ;
        end ;

     // Get Z stage settings
     ZStage.StartAt := edZStartPos.Value ;
     ZStage.StepSize := edZStepSize.Value ;
     ZStage.NumSteps := Round(edZNumSteps.Value) ;

     // Create XY Stage files (if required)
     if (cbRecordingMode.ItemIndex <> rmContinuous) and
        XYStageFrm.IncrementStagePosition then begin
        for i := 0 to XYStageFrm.NumPositions-1 do begin
            XYStageFileNames[i] := ANSIReplaceText( MainFrm.IDRFile.FileName,'.idr',
                                                    format('.XY%d.idr',[i+1]));
            if not FileExists(XYStageFileNames[i]) then begin
               IDRFileXY.CreateFileFrom( XYStageFileNames[i], MainFrm.IDRFile, false ) ;
               IDRFileXY.FrameInterval := MainFrm.IDRFile.FrameInterval*XYStageFrm.NumPositions ;
               IDRFileXY.CloseFile ;
               end ;
            end;
        XYStageLastFrame := MainFrm.IDRFile.NumFrames + 1 ;
        end;

     // Start image capture
     StartCamera ;
     CameraStartFrame :=  MainFrm.IDRFile.NumFrames + 1 ;

     end;


function TRecordFrm.GetNumFramesRequired : Integer ;
// --------------------------------------------------------
// Get valid number of frame required from recording period
// --------------------------------------------------------
var
    FrameInterval : Single ;
begin
      if cbRecordingMode.ItemIndex = rmContinuous then FrameInterval := edFrameInterval.Value
                                                  else FrameInterval := edTimeLapseInterval.Value ;
      Result := Round(edRecordingPeriod.Value/FrameInterval) ;
      Result := Max(Result div FrameTypeCycleLength,1)*FrameTypeCycleLength ;
      edRecordingPeriod.Value := FrameInterval*Result ;
      if cbRecordingMode.ItemIndex <> rmContinuous then Result := Result*FrameTypeCycleLength ;
      MainFrm.RecordingPeriod := edRecordingPeriod.Value ;
      Result := Result*NumFramesPerCCD ;
      end ;


function TRecordFrm.GetTimeLapseFrameInterval : Integer ;
// -----------------------------------
// Get valid time lapse frame interval
// -----------------------------------
begin
     Result := Max( Round( edTimeLapseInterval.Value/
                           (edFrameInterval.Value*NumFrameTypes)),2)*NumFrameTypes ;
     edTimeLapseInterval.Value := edFrameInterval.Value*Result ;
     Result := Result*NumFramesPerCCD ;
     MainFrm.IDRFile.FrameInterval := edTimeLapseInterval.Value/NumFrameTypes ;
     Mainfrm.TimeLapseInterval :=  edTimeLapseInterval.Value ;

     end ;


procedure TRecordFrm.edRecordingPeriodKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------
// No. of frames required updated
// ------------------------------
begin
     if Key = #13 then begin
        MainFrm.NumFramesRequired := GetNumFramesRequired ;
        NumFramesRequired := MainFrm.NumFramesRequired ;
        end ;
     end;


procedure TRecordFrm.bStopClick(Sender: TObject);
// -----------------------------
// Stop recording images to disk
// -----------------------------
begin
      // Request recording to stop
      if {TimeLapseMode} false then begin
         RecordingMode := rmStopADCRecording ;
         if not ADCRunning then StopRecording ;
         end
      else RecordingMode := rmStopFrameRecording ;
      bStop.Enabled := False ;

      end ;


procedure TRecordFrm.StopRecording ;
// -----------------------------
// Stop recording images to disk
// -----------------------------

var
     i : Integer ;
     FramesLost : Integer ;
begin

     MainFrm.Recording := False ;
     bRecord.Enabled := True ;
     bStop.Enabled := False ;
     ImageCaptureGrp.Enabled := True ;
     cbDisplayZoom.Enabled := True ;
     WavelengthGrp.Enabled := True ;
     StimulatorTab.Enabled := True ;
     TimeLapseMode := False ;
     bMark.Enabled := False ;
     TimeLapseMode := False ;
     RecordingMode := rmRecordingStopped ;
     TimeLapsePanel.Enabled := True ;
     BurstModePanel.Enabled := True ;
     Caption := 'Record Images & Signals' ;

     // Ensure that no. of frames in file is a multiple of no. of frame type cycle
     MainFrm.IDRFile.NumFrames := MainFrm.IDRFile.NumFrames
                                  - (MainFrm.IDRFile.NumFrames mod Max(FrameTypeCycleLength,1)) ;

     // Set number of A/D scans in file equal to duration of frames
     if (MainFrm.IDRFile.ADCNumChannels > 0) then begin
        MainFrm.IDRFile.ADCNumScansInFile := Round( (MainFrm.IDRFile.NumFrames*MainFrm.IDRFile.FrameInterval) /
                                                  MainFrm.IDRFile.ADCScanInterval ) ;
        end ;

     // Save background image to .IDS file
     if ckBackgroundSubtraction.Checked then begin
        IDRBackground.CreateFileFrom( ANSIReplaceText(MainFrm.IDRFile.FileName,'.idr','[BACKG].idr'),
                                      MainFrm.IDRFile,
                                      False) ;
        IDRBackground.PixelDepth := 32 ;
        for i := 0 to NumFrameTypes-1 do begin
            IDRBackground.SaveFrame32( i+1, pBackGroundBufs[i] ) ;
            end ;
        IDRBackground.CloseFile ;
        end ;

     // Remove blank camera start frame by replacing with next available frame of same type
     if (CameraStartFrame+FrameTypeCycleLength) < MainFrm.IDRFile.NumFrames then begin
        MainFrm.IDRFile.LoadFrame( CameraStartFrame+FrameTypeCycleLength, pFrameBuf ) ;
        MainFrm.IDRFile.SaveFrame( CameraStartFrame, pFrameBuf ) ;
        end ;

     // Close and re-open file to make directory entry permanent
     MainFrm.IDRFile.CloseFile ;
     MainFrm.IDRFile.OpenFile( MainFrm.IDRFile.FileName ) ;

     IDRFileBurst.CloseFile ;
     // Delete file if empty
     if IDRFileBurst.NumFrames < 1 then DeleteFile( Pchar(IDRFileBurst.FileName)) ;

     // Update Viewing window if it is open
     if MainFrm.FormExists('ViewFrm') then ViewFrm.NewFile ;

     FramesLost := Round(Floor(100.0*(1.0 - FrameRate*FrameInterval))) ;
     if FramesLost < 2 then FramesLost := 0 ;
     LogFrm.AddLine( format('Recording stopped at frame %d (lost frames %d%%)',
                     [MainFrm.IDRFile.NumFrames,FramesLost])) ;

     // Plot marker on chart
     AddMarker( '//' ) ;

     // Clear time course displays
     NewDisplaySetup ;

     // If a protocol file is waiting to be loaded, starting recording again
     if RestartRecording then begin
        RestartRecording := False ;
        bRecord.Click ;
        end
     else begin
        //Beep ;
        if StimulusRequired then StimulusRequired := False ;
        StopCamera ;
        if ckExcitationOnWhenRecording.Checked then begin
           rbEXCShutterOpen.Checked := False ;
           rbEXCShutterClosed.Checked := True ;
           end ;
        StartCamera ;
        end ;

     end;


procedure TRecordFrm.edFrameIntervalKeyPress(Sender: TObject;
  var Key: Char);
// --------------------------------
// Inter-frame capture time updated
// --------------------------------
begin
     if key = #13 then begin
         OptimiseContrastCount := NumFrameTypes*2 ;
         RestartCamera ;
         // Report minimum readout time
        lbReadoutTime.Caption := format('Min.= %.4g ms',
                              [SecsToMs*MainFrm.Cam1.ReadoutTime]) ;
         end ;
     end;


procedure TRecordFrm.bFullScaleClick(Sender: TObject);
// --------------------------------------------------------
// Set display grey scale to full intensity range of camera
// --------------------------------------------------------
var
    FT : Integer ;
begin

     for FT := 0 to NumFrameTypes-1 do
         if ckChangeAllFrameTypes.Checked or (FT = SelectedFrameType) then begin

         edDisplayIntensityRange.LoValue := 0 ;
         MainFrm.GreyLo[FT] := Round(edDisplayIntensityRange.LoValue) ;
         edDisplayIntensityRange.HiValue := MainFrm.Cam1.GreyLevelMax ;
         MainFrm.GreyHi[FT] := Round(edDisplayIntensityRange.HiValue) ;

         MainFrm.UpdateLUT( FT, MainFrm.Cam1.GreyLevelMax ) ;
         UpdateImage( FT, MainFrm.LUTs[FT*LUTSize],BitMaps[FT],Images[FT]) ;

         // Set upper/lower limits of time course display
         RecPlotFrm.FLDisplayYMax[FT] := MainFrm.GreyHi[FT] ;
         RecPlotFrm.FLDisplayYMin[FT] := 0 ;

         end ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[SelectedFrameType],
                               MainFrm.GreyHi[SelectedFrameType] ) ;

     end;


procedure TRecordFrm.bMaxContrastClick(Sender: TObject);
// -------------------------------------------------------------
// Request display intensity range to be set for maximum contrast
// -------------------------------------------------------------
var
   FT : Integer ;
begin

     for FT := 0 to NumFrameTypes -1 do
         if ckChangeAllFrameTypes.Checked or (OptimiseContrastCount = 0) or
           (FT = SelectedFrameType) then begin
           CalculateMaxContrast( FT ) ;

           // Set upper/lower limits of time course display
           RecPlotFrm.FLDisplayYMax[FT] := MainFrm.GreyHi[FT] ;
           RecPlotFrm.FLDisplayYMin[FT] := 0 ;

           UpdateImage( FT, MainFrm.LUTs[FT*LUTSize],BitMaps[FT],Images[FT]) ;

           end ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[SelectedFrameType],
                               MainFrm.GreyHi[SelectedFrameType] ) ;

     OptimiseContrastCount := -1 ;

     end;


procedure TRecordFrm.edDisplayIntensityRangeKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------
// Update display intensity range
// ------------------------------
var
    FT : Integer ;
begin

     if key <> #13 then Exit ;

     for FT := 0 to NumFrameTypes-1 do
         if ckChangeAllFrameTypes.Checked or (FT = SelectedFrameType) then begin

         if edDisplayIntensityRange.LoValue = edDisplayIntensityRange.HiValue then begin
            edDisplayIntensityRange.LoValue := edDisplayIntensityRange.LoValue - 1.0 ;
            edDisplayIntensityRange.HiValue := edDisplayIntensityRange.HiValue + 1.0 ;
            end ;

         MainFrm.GreyLo[FT] := Round(edDisplayIntensityRange.LoValue) ;
         MainFrm.GreyHi[FT] := Round(edDisplayIntensityRange.HiValue) ;

         MainFrm.UpdateLUT( FT, MainFrm.Cam1.GreyLevelMax ) ;

         MainFrm.UpdateLUT( FT, MainFrm.Cam1.GreyLevelMax ) ;
         UpdateImage( FT, MainFrm.LUTs[FT*LUTSize],BitMaps[FT],Images[FT]) ;

         // Set upper/lower limits of time course display
         RecPlotFrm.FLDisplayYMax[FT] := MainFrm.GreyHi[FT]*1.2 ;
         RecPlotFrm.FLDisplayYMin[FT] := 0 ;

         end ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[SelectedFrameType],
                               MainFrm.GreyHi[SelectedFrameType] ) ;

     end;


procedure TRecordFrm.FormResize(Sender: TObject);
// ------------------------------------------------
// Request a form resize
// ------------------------------------------------
begin

    FormResizeCounter := 5 ;

    end;


procedure TRecordFrm.NewDisplaySetup ;
// --------------------------
// Set up A/D signals display
// --------------------------
begin

    // Set up A/D signals display window

    RecPlotFrm.FLInitialiseDisplay( TimeLapseMode,
                                    FrameTypes,
                                    NumFrameTypes,
                                    MainFrm.Cam1.FrameInterval/NumFramesPerCCD,
                                    MainFrm.TimeLapseInterval,
                                    True ) ;

    RecPlotFrm.ClearDisplays ;

    end ;


procedure TRecordFrm.cbDisplayZoomChange(Sender: TObject);
// ---------------------------
// Display zoom factor changed
// ---------------------------
begin

    MainFrm.StatusBar.SimpleText := ' Wait ... Initialising Camera ' ;

    MainFrm.DisplayZoomIndex := cbDisplayZoom.ItemIndex ;
    DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])*0.001 ;
    RestartCamera ;
    end;


procedure TRecordFrm.bChangeExcitationClick(Sender: TObject);
begin

   if MainFrm.FormExists('EXCSetupFrm') then begin
      EXCSetupFrm.Show ;
      end
   else ExcSetupFrm := TExcSetupFrm.Create(Self) ;

   end;


procedure TRecordFrm.rbEXCShutterOpenClick(Sender: TObject);
// -----------------------------
// Open excitation light shutter
// -----------------------------
begin
   if MainFrm.Recording then begin
      UpdateLightSource ;
      UpdateEMFilter ;
      UpdateLightSourceShutter ;
      end
   else begin
      RestartCamera ;
      end ;
   end;


procedure TRecordFrm.bFullFrameClick(Sender: TObject);
// ------------------------------------
// Set frame capture area to full frame
// ------------------------------------
begin

     bFullFrame.Enabled := False ;

     // Delete ROIs
     bDeleteROIs.Click ;

     StopCamera ;

     // Set to full frame
     MainFrm.Cam1.SetCCDArea( 0,
                              0,
                              MainFrm.Cam1.FrameWidthMax-1,
                              MainFrm.Cam1.FrameHeightMax-1);
     StartCamera ;
     // Report minimum readout time
     lbReadoutTime.Caption := format('Min.= %.3g ms',
                              [SecsToMs*MainFrm.Cam1.ReadoutTime]) ;

     bFullFrame.Enabled := True ;

     end;


procedure TRecordFrm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
{ ------------------------------------------------------------
  Update size/location of display magnification adjustment box
  ------------------------------------------------------------}
const
     Margin = 2 ;
     ZoomMin = 0 ;
var
   BoxWidth,BoxHeight,i : Integer ;
   NewCursor :TCursor ;
begin

     if FMoveCaptureRegion then begin

        Canvas.Pen.Mode := pmXOR ;
        //Canvas.FrameRect( CaptureRegion ) ;
        DrawCaptureRegion( Canvas, CaptureRegion ) ;

        { Move the part of the zoom box which is under the mouse }
        case MoveMode of

             mvAll : begin
                { Move whole box }
                BoxWidth := CaptureRegion.Right - CaptureRegion.Left ;
                BoxHeight := CaptureRegion.Bottom - CaptureRegion.Top ;
                CaptureRegion.Left := IntLimitTo( CaptureRegion.Left + (X - FXOld),
                                                0,
                                                Images[0].Width-1 - BoxWidth ) ;
                CaptureRegion.Right := CaptureRegion.Left + BoxWidth ;
                CaptureRegion.Top := IntLimitTo( CaptureRegion.Top + (Y - FYOld),
                                               0,
                                               Images[0].Height-1 - BoxHeight ) ;
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
                                              Images[0].Width-1 ) ;
        CaptureRegion.Right :=     IntLimitTo(CaptureRegion.Right,
                                              0,
                                              Images[0].Width-1 ) ;
        CaptureRegion.Top :=       IntLimitTo(CaptureRegion.Top,
                                              0,
                                              Images[0].Height-1 ) ;
        CaptureRegion.Bottom :=    IntLimitTo(CaptureRegion.Bottom,
                                              0,
                                              Images[0].Height-1 ) ;

        //Canvas.FrameRect( CaptureRegion ) ;
        DrawCaptureRegion( Canvas, CaptureRegion ) ;

        end
     else begin

         { *** Determine if the mouse is over part of the zoom box *** }

         // Get existing cursor
         NewCursor := Images[0].Cursor ;

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
         for i := 0 to NumFrameTypes-1 do Images[i].Cursor := NewCursor ;

         end ;

     end ;


procedure TRecordFrm.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
// ----------------------------------
// Mouse button depressed over images
// ----------------------------------
begin
     FMoveCaptureRegion := True ;
     MousePosition.X := X ;
     MousePosition.Y := Y ;
     end;


procedure TRecordFrm.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
// ---------------------------------
// Mouse button released over images
// ---------------------------------
begin
     FMoveCaptureRegion := False ;
     end;


procedure TRecordFrm.bSelectedRegionClick(Sender: TObject);
// ---------------------------
// Set new frame capture area
// ---------------------------
var
     OldLeft,OldTop : Integer ;
     FrameLeft,FrameTop,FrameBottom,FrameRight : Integer ;
     ImgLeft,ImgRight,ImgTop,ImgBottom : Integer ;
begin

     bSelectedRegion.Enabled := False ;

     // Delete ROIs
     bDeleteROIs.Click ;

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


procedure TRecordFrm.UpdateExcitationWavelengths ;
// ----------------------------------------
// Update excitation wavelength(s) selected
// ----------------------------------------
var
     i,WVNum : Integer ;
begin

     // Stop camera
     //StopCamera ;

     // Prevent multiple wavelengths if no wavelength sequence defined
     if rbMultipleWavelengths.Checked and (MainFrm.EXCNumWavelengths[MainFrm.EXCSequenceNum] <= 0) then begin
        rbMultipleWavelengths.Checked := False ;
        rbSingleWavelength.Checked := TRue ;
        end ;

     if rbSingleWavelength.Checked then begin
        // Single wavelength excitation selected
        rbMultipleWavelengths.Checked := False ;
        rbSpectrum.Checked := False ;
        MainFrm.EXCSingleWavelength := True ;
        // Reset number of wavelengths in use
        NumFrameTypes := 1 ;
        end
     else if rbSpectrum.Checked then begin
        // Single wavelength excitation selected
        rbMultipleWavelengths.Checked := False ;
        rbSingleWavelength.Checked := False ;
        MainFrm.EXCSingleWavelength := True ;
        NumFrameTypes := 1 ;
        end
     else begin
        // Multiple wavelength excitation selected
        rbSingleWavelength.Checked := False ;
        MainFrm.EXCSingleWavelength := False ;
        rbSpectrum.Checked := False ;
        // Reset number of wavelengths in use
        NumFrameTypes := Max(MainFrm.EXCNumWavelengths[MainFrm.EXCSequenceNum],1) ;
        end ;
     MainFrm.EXCSingleWavelengthNum := MaxInt([cbWavelength.ItemIndex,0]) ;

     // Create display labels for each frame
     if MainFrm.EXCSingleWaveLength then begin
         FrameTypes[0] := format('%d (%d)',
         [MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].Centre,
          MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].Width]) ;
         end
     else begin
         for i := 0 to NumFrameTypes-1 do begin
             WVNum := MainFrm.EXCSequence[i,MainFrm.EXCSequenceNum].WavelengthNum ;
             FrameTypes[i] := format('%d (%d)',
                           [MainFrm.EXCWavelengths[WVNum].Centre,
                            MainFrm.EXCWavelengths[WVNum].Width] ) ;
             end ;
         end ;

     // Set lower limit for time lapse
     //edTimeLapseInterval.LoLimit := edFrameInterval.Value*(NumFrameTypes*4) ;

     // Restart camera
     //StartCamera ;

     end ;


procedure TRecordFrm.rbSingleWavelengthClick(Sender: TObject);
// --------------------------------------
// Switch to single wavelength excitation
// --------------------------------------
begin
     // Ensure selected frame type is set to zero when single wavelengths in use
     SelectedFrameType := 0 ;
     DisplayGrp.Caption := ' Contrast ' + FrameTypes[SelectedFrameType] + ' ' ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[SelectedFrameType],
                               MainFrm.GreyHi[SelectedFrameType] ) ;

     RestartCamera ;

     end;


procedure TRecordFrm.rbMultipleWavelengthsClick(Sender: TObject);
// ----------------------------------------
// Switch to multiple wavelength excitation
// ----------------------------------------
begin
     RestartCamera ;
     end;


procedure TRecordFrm.FormDestroy(Sender: TObject);
// --------------------------------------------
// Free allocated resources when form destroyed
// --------------------------------------------
var
     i : Integer ;
begin

     // Free display buffers
     for i := 0 to High(PDisplayBufs) do
         if PDisplayBufs[i] <> Nil then begin
            FreeMem(PDisplayBufs[i]) ;
            PDisplayBufs[i] := Nil ;
            end ;

     FreeMem( PWorkBuf ) ;
     PWorkBuf := Nil ;
     if ADCBuf <> Nil then FreeMem(ADCBuf) ;

    // Free bitmaps
    for i := 0 to High(BitMaps) do if BitMaps[i] = Nil then begin
        BitMaps[i].Free ;
        BitMaps[i] := Nil ;
        end ;

    end ;


procedure TRecordFrm.bStartStimulusClick(Sender: TObject);
// ----------------------
// Start stimulus program
// ----------------------
begin
     if cbStimProgram.ItemIndex > 0 then begin
        StimulusRequired := True ;
        UpdateStimulusWaveforms( StimulusRequired, False ) ;
        LogFrm.AddLine( 'Stim Prog: Started' ) ;
        bStopStimulus.Enabled := StimulusRequired ;
        bStartStimulus.Enabled := not StimulusRequired ;
        end ;
     end;


procedure TRecordFrm.edIdentKeyPress(Sender: TObject; var Key: Char);
// ------------------------------------------
// Update ident field when ident text changed
// ------------------------------------------
begin
     // Save to ident field
     MainFrm.IDRFile.Ident := edIdent.Text ;
     if Key = #13 then LogFrm.AddLine(  MainFrm.IDRFile.Ident ) ;
     end;


procedure TRecordFrm.cbStimProgramChange(Sender: TObject);
// ---------------------------------
// Stimulus program has been changed
// ---------------------------------
begin
     // Voltage protocol file name
     MainFrm.StimFileName := Mainfrm.VProtDirectory + cbStimProgram.Text + '.vpr' ;
     end;


procedure TRecordFrm.rbStimOffClick(Sender: TObject);
// ----------------------------------
// Stimulus Off radio button clicked
// ----------------------------------
begin
     // Re-start timing cycle with stimulus disabled
     RestartCamera ;
     end;


procedure TRecordFrm.bStopStimulusClick(Sender: TObject);
// ----------------------
// Stop stimulus program
// ----------------------
begin
     StimulusRequired := False ;
     LoadNextProtocolFile := False ;
     UpdateStimulusWaveforms( StimulusRequired, False ) ;
     LogFrm.AddLine( 'Stim Prog: Stopped' ) ;
     bStopStimulus.Enabled := StimulusRequired ;
     bStartStimulus.Enabled := not StimulusRequired ;
     end;


procedure TRecordFrm.Image1DblClick(Sender: TObject);
// ----------------------------------------------------
// Update image intensity time course measurement point
// when image double-clicked
// ----------------------------------------------------
begin
     ROIs[0].X := Round(MousePosition.X/DisplayZoom) + sbXScroll.Position ;
     ROIs[0].Y := Round(MousePosition.Y/DisplayZoom) + sbYScroll.Position ;
     MainFrm.ROIX := ROIs[0].X ;
     MainFrm.ROIY := ROIs[0].Y ;
     end;


procedure TRecordFrm.edBinFactorKeyPress(Sender: TObject; var Key: Char);
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


procedure TRecordFrm.cbPaletteChange(Sender: TObject);
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

     end;


procedure TRecordFrm.bUpdateWavelengthsClick(Sender: TObject);
begin
    RestartCamera ;
    end;


procedure TRecordFrm.Image1Click(Sender: TObject);
// -------------------------------------------
// Image clicked - changed frame type selected
// -------------------------------------------
begin
     SelectedFrameType := TImage(Sender).Tag ;
     DisplayGrp.Caption := ' Contrast ' + FrameTypes[SelectedFrameType] + ' ' ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[SelectedFrameType],
                               MainFrm.GreyHi[SelectedFrameType] ) ;

     end;


procedure TRecordFrm.cbWavelengthChange(Sender: TObject);
// --------------------------------------
// Single excitation wavelength # changed
// --------------------------------------
begin
     if rbSingleWavelength.Checked then begin
        RestartCamera ;
        end ;

     end;


procedure TRecordFrm.rbContinuousRecordingClick(Sender: TObject);
begin
//     DisplayRecordingDuration ;
     end;


procedure TRecordFrm.rbTimeLapseClick(Sender: TObject);
begin
//    DisplayRecordingDuration ;
    // No stimulus programs allowed in time lapse mode
    cbStimProgram.ItemIndex := 0 ;
    end;


procedure TRecordFrm.bMarkClick(Sender: TObject);
// ------------------------------
//  Add a text marker to the chart
// ------------------------------
begin

     AddMarker( edMarker.Text ) ;

     end ;


procedure TRecordFrm.AddMarker(
          MarkerText : String
          ) ;
// ------------------
// Add marker to file
// ------------------
var
     MarkerTime : Single ;
begin

     if MainFrm.IDRFile.NumMarkers >= MaxMarker then begin
        bMark.Enabled := False ;
        Exit ;
        end ;

     // If marker text is blank, add marker number
     if MarkerText = '' then MarkerText := format('%d',[MainFrm.IDRFile.NumMarkers+1]) ;

     MarkerTime := MainFrm.Cam1.FrameInterval*NumFramesTotal ;

     // Plot marker on chart
     MainFrm.IDRFile.AddMarker( MarkerTime, MarkerText ) ;
     RecPlotFrm.AddMarker( MarkerText );

     LogFrm.AddLine( format('Marker at %.2fs %s',[MarkerTime, MarkerText]));

     end;


procedure TRecordFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
// ---------------------------
// Check if form can be closed
// ---------------------------
begin
     // Prevent form closure if recording
     // or if engaged in time-critical activity with timer turned off
     if (RecordingMode = rmRecordingStopped) and Timer.Enabled then CanClose := True
                                                               else CanCLose := False ;
     // Always allow form to be closed if initialisation not complete
     if not InitialisationComplete then CanClose := True ;

     Inc(NumCloseClicks) ;
     if NumCloseClicks > 2 then CanClose := True ;

     end;


procedure TRecordFrm.edTimeLapseIntervalKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------
// Time lapse interval changed
// ---------------------------
begin
     if Key = #13 then begin
         StopCamera ;
         GetTimeLapseFrameInterval ;
         StartCamera ;
         end ;
     end;


procedure TRecordFrm.edVHold0KeyPress(Sender: TObject; var Key: Char);
// -----------------------
// Holding voltage changed
// -----------------------
begin
     if Key = #13 then UpdateHoldingVoltage ;
     end;


procedure TRecordFrm.FormActivate(Sender: TObject);
// -----------------------
// Form has become active
// -----------------------
begin

     // Stop seal test (if it is running)
     if MainFrm.FormExists('SealTestFrm') then SealTestFrm.StopSealTest ;

     // Stop seal test (if it is running)
     if MainFrm.FormExists('SnapFrm')then SnapFrm.StopLiveImaging ;

     // Set Z stage control
     ZStageGrp.Visible := ZStage.Available ;
     edZPosition.Value := ZStage.Position ;
     edZPosition.LoLimit := ZStage.MinPosition ;
     edZPosition.HiLimit := ZStage.MaxPosition ;
     sbZPosition.Min := Round(ZStage.MinPosition/ZStage.MinStepSize) ;
     sbZPosition.Max := Round(ZStage.MaxPosition/ZStage.MinStepSize) ;
     sbZPosition.Position := Round(ZStage.Position/ZStage.MinStepSize) ;

     // Start camera
     if not CameraRunning then StartCamera ;

     end;


procedure TRecordFrm.rbEXCShutterClosedClick(Sender: TObject);
// -----------------------------
// Close excitation light shutter
// -----------------------------
begin
   if MainFrm.Recording then begin
      UpdateLightSource ;
      UpdateEMFilter ;
      UpdateLightSourceShutter ;
      end
   else begin
      RestartCamera ;
      end ;
   end ;


procedure TRecordFrm.edMarkerKeyPress(Sender: TObject; var Key: Char);
// ---------------------------------------
// Add marker text when return key pressed
// ---------------------------------------
begin
     if Key = #13 then AddMarker( edMarker.Text ) ;
     end;


procedure TRecordFrm.cbCameraGainChange(Sender: TObject);
// -------------------
// Camera gain changed
// -------------------
begin
     RestartCamera ;
     end ;


procedure TRecordFrm.Wait( Delay : Single ) ;
var
  T : Integer ;
  TExit : Integer ;
begin
    T := TimeGetTime ;
    TExit := T + Round(Delay*1E3) ;
    while T < TExit do begin
       T := TimeGetTime ;
       Application.ProcessMessages ;
       end ;
    end ;


procedure TRecordFrm.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
     //outputdebugString(PChar(format('View %d',[Msg.CharCode] ))) ;
end;


function TRecordFrm.CheckRecPlotFrmExists : Boolean ;
// -------------------------------------------
// Check that time course plotting form exists
// -------------------------------------------
var
    i : Integer ;
    Exists : Boolean ;
begin
     // Is there a RecPlotFrm window open?
     Result := False ;
     Exists := False ;
     for i := 0 to MainFrm.MDIChildCount-1 do
         if MainFrm.MDIChildren[i].Name = 'RecPlotFrm' then Exists := True ;

     // If not open it
     if not Exists then begin
        RecPlotFrm := TRecPlotFrm.Create(Self) ;
        RecPlotFrm.DisplayGrid := MainFrm.mnDisplayGrid.Checked ;
        RecPlotFrm.Left := Left + Width + 10 ;
        RecPlotFrm.Width := MainFrm.ClientWidth - RecPlotFrm.Left - 10 ;
        Application.ProcessMessages ;
        Result := True ;
        end ;

     end ;


procedure TRecordFrm.UpdateStimProgramList ;
// --------------------------------
// Update list of stimulus programs
// --------------------------------
var
    NamePart : String ;
begin

     // Load list of stimulus programs
     Stimulator.CreateProgramList( cbStimProgram ) ;
     if MainFrm.StimFileName <> '' then begin
        // Find name within list
        NamePart := AnsiReplaceStr(LowerCase(ExtractFileName(MainFrm.StimFileName)),'.vpr', '' ) ;
        cbStimProgram.ItemIndex := Min(Max(cbStimProgram.Items.IndexOf(NamePart),
                                   0),cbStimProgram.Items.Count-1) ;
        end
     else begin
        cbStimProgram.ItemIndex := 0 ;
        end ;

     end ;


procedure TRecordFrm.sbContrastChange(Sender: TObject);
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

         MainFrm.UpdateLUT( FT, MainFrm.Cam1.GreyLevelMax ) ;
         UpdateImage( FT, MainFrm.LUTs[FT*LUTSize],BitMaps[FT],Images[FT]) ;

         // Set upper/lower limits of time course display
         RecPlotFrm.FLDisplayYMax[FT] := Round(MainFrm.GreyHi[FT]*1.2) ;
         RecPlotFrm.FLDisplayYMin[FT] := 0 ;

         end ;

     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[SelectedFrameType]  ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[SelectedFrameType]  ;

     end;


procedure TRecordFrm.bEnterCCDAreaClick(Sender: TObject);
begin
     
     SetCCDReadoutFrm := TSetCCDReadoutFrm.Create(Self) ;
     SetCCDReadoutFrm.CalledBy := 'RecordFrm' ;
     SetCCDReadoutFrm.Show ;
     SetCCDReadoutFrm.Left := 20 ;
     SetCCDReadoutFrm.Top := 20 ;
     end;

procedure TRecordFrm.bSetLaserIntensityClick(Sender: TObject);
// -------------------
// Set laser intensity
// -------------------
begin

     SetLasersFrm.Left := MainFrm.Left + 20 ;
     SetLasersFrm.Top := Mainfrm.Top + 20 ;
     SetLasersFrm.Show ;

     end;


procedure TRecordFrm.bSetSubFolderClick(Sender: TObject);
// ---------------------------------
//  Set voltage protocol file folder
// ---------------------------------
begin

    SelectDirectory(MainFrm.VProtDirectory, [sdAllowCreate, sdPerformCreate, sdPrompt],0) ;
    MainFrm.VProtDirectory := MainFrm.VProtDirectory + '\' ;
    UpdateStimProgramList ;

    end ;

procedure TRecordFrm.bAddROIClick(Sender: TObject);
// ------------------------------
// Add additional ROIs to display
// ------------------------------
begin
     if NumROIs < MaxLiveROIs then begin
        ROIs[NumROIs].X := ROIs[0].X ;
        ROIs[NumROIs].Y := ROIs[0].Y ;
        RecPlotFrm.AddToROILists ;
        Inc(NumROIs) ;
        end ;
     end;



procedure TRecordFrm.bDeleteROIsClick(Sender: TObject);
// ----------------------
// Delete additional ROIs
// ----------------------
begin
     RecPlotFrm.ClearROILists ;
     NumROIs := 1 ;
     end;


procedure TRecordFrm.StopCameraAndAnalogIO ;
// ----------------------------------------
// Stop camera, and all ADC/DAC and timing
// ----------------------------------------
var
    Device : Integer ;
begin

     // Stop acquiring images
     StopCamera ;

     // Stop A/D converter(s)
     for Device := 1 to LabIO.NumDevices do LabIO.StopADC(Device) ;

     // Stop D/A converter(s)
     for Device := 1 to LabIO.NumDevices do begin
         LabIO.StopDAC(Device) ;
         LabIO.StopDIG(Device) ;
         end ;

    Timer.Enabled := False ;

    end ;


procedure TRecordFrm.ShowHideImageCaptureSettingsPanel ;
// -------------------------------------
// Show/Hide image capture settings
// -------------------------------------
begin

     ImageCaptureSettingsPanel.Visible := sbImageCaptureShowSettings.Down ;

     if not ImageCaptureSettingsPanel.Visible then begin
        // Hide settings
        ImageCaptureGrp.Height := lbReadoutTime.Top +
                                  lbReadoutTime.Height + 5 ;
        end
     else begin
        // Show settings
        ImageCaptureGrp.Height := ImageCaptureSettingsPanel.Top +
                                  ImageCaptureSettingsPanel.Height + 5 ;
        end ;

     end;


procedure TRecordFrm.ShowHideDisplaySettingsPanel ;
// -------------------------------------
// Show/Hide image capture settings
// -------------------------------------
begin

     DisplaySettingsPanel.Visible := sbDisplayShowSettings.Down ;
     if not DisplaySettingsPanel.Visible then begin
        // Hide settings
        DisplayGrp.Height := cbPalette.Top +
                             cbPalette.Height + 5 ;
        end
     else begin
        // Show settings
        DisplayGrp.Height := DisplaySettingsPanel.Top +
                             DisplaySettingsPanel.Height + 5 ;
        end ;

     end;


procedure TRecordFrm.ShowHideShadeCorSettingsPanel ;
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

     end;


procedure TRecordFrm.ShowHideLightStimPage ;
// -------------------------------------
// Show/Hide shading correction settings
// -------------------------------------
begin

     LightStimPage.Visible := sbLightStimShowSettings.Down ;

     if not LightStimPage.Visible then begin
        // Hide settings
        LightStimGrp.Height := sbLightStimShowSettings.Top +
                             sbLightStimShowSettings.Height + 5 ;
        end
     else begin
        // Show settings
        LightStimGrp.Height := LightStimPage.Top +
                             LightStimPage.Height + 5 ;
        end ;

     end;


procedure TRecordFrm.ShowHideZStackSettings ;
// ---------------------------
// Show/Hide Z stage settings
// ---------------------------
begin

     ZStackGrp.Visible := sbShowHideZStackSettings.Down ;

     if not ZStackGrp.Visible then begin
        // Hide settings
        ZStageGrp.Height := edZPosition.Top + edZPosition.Height + 5 ;
        end
     else begin
        // Show settings
        ZStageGrp.Height := ZStackGrp.Top + ZStackGrp.Height + 5 ;
        end ;

     MarkGrp.Top := ZStageGrp.Top + ZStageGrp.Height + 5 ;
     end;



procedure TRecordFrm.lbImageCaptureShowSettingsClick(Sender: TObject);
begin
    ShowHideImageCaptureSettingsPanel ;
    ResizeControlPanel ;
    end;

procedure TRecordFrm.lbDisplayShowSettingsClick(Sender: TObject);
begin
    ShowHideDisplaySettingsPanel ;
    ResizeControlPanel ;
    end;

procedure TRecordFrm.lbShadeCorShowSettingsClick(Sender: TObject);
// -------------------------------------
// Show/Hide shading correction settings
// -------------------------------------
begin
     ShowHideShadeCorSettingsPanel ;
     ResizeControlPanel ;
     end;

procedure TRecordFrm.lbLightStimShowSettingsClick(Sender: TObject);
// -------------------------------------
// Show/Hide light/stimulus settings
// -------------------------------------
begin
     ShowHideLightStimPage ;
     ResizeControlPanel ;
     end;

procedure TRecordFrm.ResizeControlPanel ;
begin
     ImageCaptureGrp.Top := RecordingGrp.Top + RecordingGrp.Height + 5 ;
     DisplayGrp.Top := ImageCaptureGrp.Top + ImageCaptureGrp.Height + 5 ;
     //ShadingGrp.Top := DisplayGrp.Top + DisplayGrp.Height + 5 ;
     LightStimGrp.Top := DisplayGrp.Top + DisplayGrp.Height + 5 ;

//     MarkGrp.Top := LightStimGrp.Top + LightStimGrp.Height + 5 ;
     if ZStageGrp.Visible then begin
        ZStageGrp.Top := LightStimGrp.Top + LightStimGrp.Height + 5 ;
        ShadingGrp.Top := ZStageGrp.Top + ZStageGrp.Height + 5 ;
        end
     else ShadingGrp.Top := LightStimGrp.Top + LightStimGrp.Height + 5 ;

     MarkGrp.Top := ShadingGrp.Top + ShadingGrp.Height + 5 ;
     ControlGrp.Height := ClientHeight - ControlGrp.Top - 5 ;
     end ;


procedure TRecordFrm.DoShadingCorrection( FrameType : Integer ) ;
// -------------------------------
// Do shading correction operation
// -------------------------------
var
    i,iSub,FT : Integer ;
    Sum : Single ;
    AllDone : Boolean ;
begin

     // Subtract shading correction
     if ckBackgroundSubtraction.Checked then begin
        for i := 0 to NumPixelsPerFrame-1 do begin
            PDisplayBufs[FrameType]^[i] := PDisplayBufs[FrameType]^[i] - PBackgroundBufs[FrameType]^[i] ;
            end ;
        end ;

     if not bAcquireBackground.Enabled then begin

        // Copy latest frame to summation buffer
        if ShadeCorNumFramesAveraged[FrameType] <
           Round(EdShadeCorNumFramesAveraged.Value) then begin
           for i := 0 to NumPixelsPerFrame-1 do begin
               PSumBufs[FrameType]^[i] := PSumBufs[FrameType]^[i] + PDisplayBufs[FrameType]^[i] ;
               end ;
           Inc(ShadeCorNumFramesAveraged[FrameType]) ;
           end ;

        // Check to see if averages acquired for all frame types
        AllDone := True ;
        for FT := 0 to NumFrameTypes-1 do begin
            if ShadeCorNumFramesAveraged[FT]
               < Round(EdShadeCorNumFramesAveraged.Value) then AllDone := False ;
            end ;

        if AllDone then begin

           for FT := 0 to NumFrameTypes-1 do begin

                   // Average background frame
               for i := 0 to NumPixelsPerFrame-1 do begin
                   PBackgroundBufs[FT]^[i] := Round(PSumBufs[FT]^[i]/ShadeCorNumFramesAveraged[FT]) ;
                   end ;

              // Smooth background frame
              if Round(edShadeCorImageBlockSize.Value) > 1 then begin
                 SmoothImage( PBackgroundBufs[FT],
                              FrameWidth,
                              FrameHeight,
                              Round(edShadeCorImageBlockSize.Value)) ;
                 end ;

              // Calculate shading (differences) image
              case cbShadeCorNormalisation.ItemIndex of

                 NormaliseToMean : begin
                    // Difference around mean intensity
                   Sum := 0.0 ;
                   for i := 0 to NumPixelsPerFrame-1 do Sum := Sum + PBackgroundBufs[FT]^[i] ;
                   iSub := Round(Sum/NumPixelsPerFrame) ;
                   end ;

                 NormaliseToMin : Begin
                   // Difference around min. intensity
                   iSub := High(iSub) ;
                   for i := 0 to NumPixelsPerFrame-1 do begin
                       if PBackgroundBufs[FT]^[i] < iSub then iSub := PBackgroundBufs[FT]^[i] ;
                       end ;
                   end ;

                 NormaliseToMax : Begin
                   // Difference around min. intensity
                   iSub := 0 ;
                   for i := 0 to NumPixelsPerFrame-1 do begin
                       if PBackgroundBufs[FT]^[i] > iSub then iSub := PBackgroundBufs[FT]^[i] ;
                       end ;
                   end ;

                 else begin
                   iSub := 0 ;
                   end ;
                 end ;

              for i := 0 to NumPixelsPerFrame-1 do begin
                  PBackgroundBufs[FT]^[i] := PBackgroundBufs[FT]^[i] - iSub ;
                  end ;
              end ;

           // End of backround acquisition - re-enable button
           bAcquireBackground.Enabled := True ;

           end ;

        end ;

     end ;


procedure TRecordFrm.SmoothImage(
          PBuf : PIntArray ;  // Imagebuffer
          FrameWidth : Integer ; // Width of frame
          FrameHeight : Integer ; // Height of frame
          BlockSize : Integer    // Smoothing block size
          ) ;
// ----------------------------------------------
// Smooth image using n x n pixel averaging block
// --------------------------------------------
var
    x,y,x0,x1,ix,iy,ix1,iy1 : Integer ;
    i : Integer ;
    NumPixels,Sum,nSum : Integer ;
    PTemp : PIntArray ;
begin

    NumPixels := FrameWidth*FrameHeight ;

    GetMem( PTemp, NumPixels*4 ) ;

    BlockSize := Max(Min(BlockSize,FrameWidth) div 2,1) ;

    //nSum := (2*BlockSize + 1)*(2*BlockSize + 1) ;
    Sum := 0 ;
    nSum := 0 ;

    for y := 0 to FrameHeight-1 do begin

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



procedure TRecordFrm.bAcquireBackgroundClick(Sender: TObject);
// ---------------------------------------
// Start acquisition of a background image
// ---------------------------------------
var
    i,FT : Integer ;
begin

    bAcquireBackground.Enabled := False ;

    // Clear buffers
    for FT := 0 to NumFrameTypes-1 do begin
        for i := 0 to NumPixelsPerFrame-1 do begin
            PSumBufs[FT]^[i] := 0.0 ;
            PBackgroundBufs[FT]^[i] := 0 ;
            end ;
        ShadeCorNumFramesAveraged[FT] := 0 ;
        end ;
    end;


procedure TRecordFrm.sbImageCaptureShowSettingsClick(Sender: TObject);
begin
    ImageCaptureSettingsPanel.Visible := sbImageCaptureShowSettings.Down ;
    ShowHideImageCaptureSettingsPanel ;
    ResizeControlPanel ;
    end;


procedure TRecordFrm.sbDisplayShowSettingsClick(Sender: TObject);
begin
    DisplaySettingsPanel.Visible := sbDisplayShowSettings.Down ;
    ShowHideDisplaySettingsPanel ;
    ResizeControlPanel ;
    end;


procedure TRecordFrm.sbShadeCorShowSettingsClick(Sender: TObject);
// -------------------------------------
// Show/Hide shading correction settings
// -------------------------------------
begin
     ShadeCorSettingsPanel.Visible := sbShadeCorShowSettings.Down ;
     ShowHideShadeCorSettingsPanel ;
     ResizeControlPanel ;
     end;

procedure TRecordFrm.sbLightStimShowSettingsClick(Sender: TObject);
// -------------------------------------
// Show/Hide light/stimulus settings
// -------------------------------------
begin
     LightStimPage.Visible := sbLightStimShowSettings.Down ;
     ShowHideLightStimPage ;
     ResizeControlPanel ;
     end;


procedure TRecordFrm.cbSequenceChange(Sender: TObject);
// ------------------------------------------------
// Multiple wavelength excitation sequence changed
// ------------------------------------------------
begin
     MainFrm.EXCSequenceNum := cbSequence.ItemIndex ;
     RestartCamera ;
     end;

procedure TRecordFrm.ckAutoOptimiseClick(Sender: TObject);
// ----------------------------------
// Auto contrast optimisation changed
// ----------------------------------
begin
    MainFrm.ContrastAutoOptimise := ckAutoOptimise.Checked ;
    end;

procedure TRecordFrm.ckContrast6SDOnlyClick(Sender: TObject);
// --------------------------------------------------
// 6 standard deviation contrast optimisation changed
// --------------------------------------------------
begin
    MainFrm.Contrast6SD := ckContrast6SDOnly.Checked ;
    end;


procedure TRecordFrm.ckChangeAllFrameTypesClick(Sender: TObject);
// -----------------------------------
// Change contrast for all frame types
// -----------------------------------
Begin
    MainFrm.ContrastChangeAllFrameTypes := ckChangeAllFrameTypes.Checked ;
    end ;


procedure TRecordFrm.CalculateCapacity ;
// ----------------------------
// On-line capacity calculation
// ----------------------------
const
     mVToVolt = 1E-3 ;
     pFToFarad = 1E-12 ;
     nSToSiemen = 1E-9 ;
     pAToAmp = 1E-12 ;
     TwoPi = 2*pi ;
var
   Rm,A,GI,GR,Gm,Gs,Vm,Im,Cm : single ;
   iScan : Integer ;
   OK : Boolean ;
   Gdc : single ;
   Omega,Denom : Single ;
begin

     if MainFrm.Cap.NewCalculation then begin
        // Set channel names and units }

        MainFrm.Cap.CmScale := MainFrm.ADCChannel[MainFrm.Cap.CmChan].ADCScale*pFToFarad ;
        MainFrm.Cap.CmOffset :=  MainFrm.ADCChannel[MainFrm.Cap.CmChan].ChannelOffset ;

        MainFrm.Cap.GmScale := MainFrm.ADCChannel[MainFrm.Cap.GmChan].ADCScale*nSToSiemen ;
        MainFrm.Cap.GmOffset :=  MainFrm.ADCChannel[MainFrm.Cap.GmChan].ChannelOffset ;

        MainFrm.Cap.GsScale := MainFrm.ADCChannel[MainFrm.Cap.GsChan].ADCScale*nSToSiemen ;
        MainFrm.Cap.GsOffset :=  MainFrm.ADCChannel[MainFrm.Cap.GsChan].ChannelOffset ;

        { Cell membrane current }
        MainFrm.Cap.ImZero := MainFrm.ADCChannel[MainFrm.Cap.ImChan].ADCZero ;
        MainFrm.Cap.ImOffset :=  MainFrm.ADCChannel[MainFrm.Cap.ImChan].ChannelOffset ;
        MainFrm.Cap.ImScale := MainFrm.ADCChannel[MainFrm.Cap.ImChan].ADCScale*pAToAmp ;

        { Cell membrane potential }
        MainFrm.Cap.VmZero := MainFrm.ADCChannel[MainFrm.Cap.VmChan].ADCZero ;
        MainFrm.Cap.VmOffset :=  MainFrm.ADCChannel[MainFrm.Cap.VmChan].ChannelOffset ;
        MainFrm.Cap.VmScale := MainFrm.ADCChannel[MainFrm.Cap.VmChan].ADCScale*mVToVolt ;

        { Real component of cell membrane impedance }
        MainFrm.Cap.GRZero := MainFrm.ADCChannel[MainFrm.Cap.GRChan].ADCZero ;
        MainFrm.Cap.GROffset :=  MainFrm.ADCChannel[MainFrm.Cap.GRChan].ChannelOffset ;

        // Calculate scaling factor
        MainFrm.ADCChannel[MainFrm.Cap.GRChan].ADCScale :=
            Abs(MainFrm.ADCVoltageRange) /
            (MainFrm.ADCChannel[MainFrm.Cap.GRChan].ADCCalibrationFactor
             *(ADCMaxValue+1) ) ;
        if MainFrm.Cap.GChannelsUseGainTelegraph then begin
           // Apply current gain factor from telegraph (if in use)
           MainFrm.ADCChannel[MainFrm.Cap.GRChan].ADCScale :=
               MainFrm.ADCChannel[MainFrm.Cap.GRChan].ADCScale /
               MainFrm.ADCChannel[MainFrm.Cap.ImChan].ADCAmplifierGain ;
           end ;
        MainFrm.Cap.GRScale := MainFrm.ADCChannel[MainFrm.Cap.GRChan].ADCScale*nSToSiemen ;
        //if MainFrm.Cap.InvertGR then MainFrm.Cap.GRScale := -MainFrm.Cap.GRScale ;

        { Imaginary component of cell membrane impedance }
        MainFrm.Cap.GIZero := MainFrm.ADCChannel[MainFrm.Cap.GIChan].ADCZero ;
        MainFrm.Cap.GIOffset :=  MainFrm.ADCChannel[MainFrm.Cap.GIChan].ChannelOffset ;
        // Calculate scaling factor (note uses telegraph gain from current channel)
        MainFrm.ADCChannel[MainFrm.Cap.GIChan].ADCScale :=
            Abs(MainFrm.ADCVoltageRange) /
            (MainFrm.ADCChannel[MainFrm.Cap.GIChan].ADCCalibrationFactor
             *(ADCMaxValue+1) ) ;
        if MainFrm.Cap.GChannelsUseGainTelegraph then begin
           // Apply current gain factor from telegraph (if in use)
           MainFrm.ADCChannel[MainFrm.Cap.GIChan].ADCScale :=
               MainFrm.ADCChannel[MainFrm.Cap.GIChan].ADCScale /
               MainFrm.ADCChannel[MainFrm.Cap.ImChan].ADCAmplifierGain ;
           end ;
        MainFrm.Cap.GIScale := MainFrm.ADCChannel[MainFrm.Cap.GIChan].ADCScale*nSToSiemen ;
        //if MainFrm.Cap.InvertGI then MainFrm.Cap.GIScale := -MainFrm.Cap.GIScale ;



        end ;

     iScan := ADCOldestScan ;

     while iScan <> ADCLatestScan do begin

            {- Convert the input signals from binary to SI units - }
            Im := (ADCBuf^[iScan+MainFrm.Cap.ImOffset] - MainFrm.Cap.ImZero)*MainFrm.Cap.ImScale ;
            if MainFrm.Cap.NewCalculation then MainFrm.Cap.ImSmoothed := Im ;
            MainFrm.Cap.ImSmoothed := 0.02*Im + (1.0 - 0.02)*MainFrm.Cap.ImSmoothed ;
            MainFrm.Cap.ImSmoothed := Im ;

            // Real conductance
            if MainFrm.Cap.GRInvert then
               ADCBuf^[iScan+MainFrm.Cap.GROffset] := -(ADCBuf^[iScan+MainFrm.Cap.GROffset] - MainFrm.Cap.GRZero)
                                               + MainFrm.Cap.GRZero ;
            GR := (ADCBuf^[iScan+MainFrm.Cap.GROffset] - MainFrm.Cap.GRZero)*MainFrm.Cap.GRScale ;

            // Imaginary conductance
            if MainFrm.Cap.GIInvert then
               ADCBuf^[iScan+MainFrm.Cap.GIOffset] := -(ADCBuf^[iScan+MainFrm.Cap.GIOffset] - MainFrm.Cap.GIZero)
                                               + MainFrm.Cap.GIZero ;
            GI := (ADCBuf^[iScan+MainFrm.Cap.GIOffset] - MainFrm.Cap.GIZero)*MainFrm.Cap.GIScale ;

            if MainFrm.Cap.CompensationInUse then begin
               if Gdc = MainFrm.Cap.RSeriesComp then Break ;
               Rm := 1.0 /(Gdc - MainFrm.Cap.RSeriesComp) ;
               A := Gdc*MainFrm.Cap.RSeriesComp ;
               Omega := TwoPi*MainFrm.Cap.Frequency ;
//                    *MainFrm.Cap.CellCapacityComp*Rm ;
               //GR := GR + (Gdc*(1.0 + X*X*A))/(1.0 + X*X*A*A) ;
               //GI := GI + (Gdc*X*(1.0 - A))/(1.0 + X*X*A*A) ;

               A := Omega*MainFrm.Cap.CellCapacityComp*MainFrm.Cap.RSeriesComp ;
               Denom := 1. + A*A ;
               GR := GR +
                        ((Omega*Omega*MainFrm.Cap.CellCapacityComp*
                        MainFrm.Cap.CellCapacityComp*
                         MainFrm.Cap.RSeriesComp ) / Denom ) ;

               GI := GI +
                        ((Omega*MainFrm.Cap.CellCapacityComp) / Denom ) ;

               end ;


            Vm := (ADCBuf^[iScan+MainFrm.Cap.VmOffset] - MainFrm.Cap.VmZero)*MainFrm.Cap.VmScale ;
            MainFrm.Cap.VmSmoothed := 0.02*Vm + (1.0 - 0.02)*MainFrm.Cap.VmSmoothed ;
            if MainFrm.Cap.NewCalculation then MainFrm.Cap.VmSmoothed := Vm ;
            //MainFrm.Cap.VmSmoothed := -0.05 ;

            Gm := 0.0 ;
            Gs := 0.0 ;
            Cm := 0.0 ;

            OK := True ;
	          if MainFrm.Cap.VmSmoothed <> MainFrm.Cap.Vrev then begin
               Gdc := ABS(MainFrm.Cap.ImSmoothed/(MainFrm.Cap.VmSmoothed - MainFrm.Cap.Vrev)) ;
               if GDC = 0.0 then OK := False ;
               end
            else OK := False ;

            if GR <> Gdc then Gs := GR + ((GI*GI)/(GR-Gdc))
                        else OK := False ;

            if Gs <> Gdc then Gm := (Gdc*Gs)/(Gs - Gdc)
                         else OK := False ;

            if (GI <> 0.0) and OK then
               Cm := (Gs*Gs*(GR - Gdc))/((Gs - Gdc)*GI*TwoPi*MainFrm.Cap.Frequency) ;

            if MainFrm.Cap.GsOffset >= 0 then
               ADCBuf^[iScan+MainFrm.Cap.GsOffset] := Round(Min(Max(Gs/MainFrm.Cap.GsScale,
                                           0.0),ADCMaxValue-1)) ;
            if MainFrm.Cap.GmOffset >= 0 then
               ADCBuf^[iScan+MainFrm.Cap.GmOffset] := Round(Min(Max(Gm/MainFrm.Cap.GmScale,
                                           0.0),ADCMaxValue-1)) ;
            if MainFrm.Cap.CmOffset >= 0 then
               ADCBuf^[iScan+MainFrm.Cap.CmOffset] := Round(Min(Max(Cm/MainFrm.Cap.CmScale,
                                           -ADCMaxValue),ADCMaxValue-1)) ;
            { Next block of channels }
            iScan := iScan + MainFrm.ADCNumChannels ;
           if iScan >= ADCNumSamplesInBuffer then iScan := iScan - ADCNumSamplesInBuffer ;

            MainFrm.Cap.NewCalculation := False ;

            MainFrm.StatusBar.SimpleText := format('Vm(smoothed)=%.4g mV, G(dc)=%.4g nS',
            [MainFrm.Cap.VmSmoothed*1E3,Gdc*1E9])  ;

            end ;
     end ;




procedure TRecordFrm.bStartPhotoStimulusClick(Sender: TObject);
// ----------------------------
// Start photo-stimulus program
// ----------------------------
begin
     if cbPhotoStimProgram.ItemIndex > 0 then begin
        PhotoStimulusRequired := True ;
        // Camera re-start needed
        RestartCamera ;
        end ;
     end;

procedure TRecordFrm.bStopPhotoStimulusClick(Sender: TObject);
// ---------------------------
// Stop photo-stimulus program
// ---------------------------
begin
     PhotoStimulusRequired := False ;
     // A/D restart needed with NIDAQ-MX to force DAC updates
     RestartCamera ;

     LogFrm.AddLine( 'Stim Prog: Stopped' ) ;

     end;


procedure TRecordFrm.UpdatePhotoStimProgramList ;
// --------------------------------------
// Update list of photo-stimulus programs
// --------------------------------------
var
    NamePart : String ;
begin

     // Load list of stimulus programs
     PhotoStimulator.CreateProgramList( cbPhotoStimProgram ) ;
     if MainFrm.PhotoStimFileName <> '' then begin
        // Find name within list
        NamePart := AnsiReplaceText(ExtractFileName(MainFrm.PhotoStimFileName),'.ppr', '' ) ;
        cbPhotoStimProgram.ItemIndex := Min(Max(cbPhotoStimProgram.Items.IndexOf(NamePart),
                                            0),cbPhotoStimProgram.Items.Count-1) ;
        end
     else begin
        cbPhotoStimProgram.ItemIndex := 0 ;
        end ;

     end ;



procedure TRecordFrm.cbPhotoStimProgramChange(Sender: TObject);
// --------------------------------------
// Photostimulus program has been changed
// --------------------------------------
begin
     // Photo stimulus protocol file name
     MainFrm.PhotoStimFileName := Mainfrm.PProtDirectory + cbPhotoStimProgram.Text + '.ppr' ;

     end;


procedure TRecordFrm.ckSplitCCDImageClick(Sender: TObject);
// --------------------------
// Image split option changed
// --------------------------
begin
      MainFrm.SplitImage := ckSplitCCDImage.Checked ;
      OptimiseContrastCount := NumFrameTypes*2 ;
      RestartCamera ;
      end;


procedure TRecordFrm.ckStartStimOnRecordClick(Sender: TObject);
//
// Start stimulus when record button pressed option changed
// ---------------------------------------------------------
begin
     MainFrm.StartStimOnRecord := ckStartStimOnRecord.Checked ;
     end;

procedure TRecordFrm.cbRecordingModeChange(Sender: TObject);
begin
   UpdateRecordingModePanels ;
   ResizeControlPanel ;
   end ;

procedure TRecordFrm.UpdateRecordingModePanels ;
// -------------------------------------
// Update recording mode settings panels
// -------------------------------------
begin

     MainFrm.RecordingMode := cbRecordingMode.ItemIndex ;

     TimelapsePanel.Visible := False ;
     BurstModePanel.Visible := False ;
     if cbRecordingMode.ItemIndex = rmTimeLapse then begin
        TimelapsePanel.Visible := True ;
        BurstModePanel.Visible := False ;
        end
     else if cbRecordingMode.ItemIndex = rmTimeLapseBurst then begin
        TimelapsePanel.Visible := True ;
        BurstModePanel.Visible := True ;
        end ;

     RecordingGrp.Height := edRecordingPeriod.Top + edRecordingPeriod.Height + 5 ;
     if TimeLapsePanel.Visible then RecordingGrp.Height := TimeLapsePanel.Top + TimeLapsePanel.Height + 5 ;
     if BurstModePanel.Visible then RecordingGrp.Height := BurstModePanel.Top + BurstModePanel.Height + 5 ;

     end;



procedure TRecordFrm.edBurstDurationKeyPress(Sender: TObject;
  var Key: Char);
begin
    if Key = #13 then MainFrm.BurstDuration := edBurstDuration.Value ;
    end;

procedure TRecordFrm.edBurstIntervalKeyPress(Sender: TObject;
  var Key: Char);
begin
    if Key = #13 then MainFrm.BurstInterval := edBurstInterval.Value ;
    end;


procedure TRecordFrm.ckExcitationOnWhenRecordingClick(Sender: TObject);
begin
     MainFrm.ExcitationOnWhenRecording := ckExcitationOnWhenRecording.Checked ;
     end;

procedure TRecordFrm.sbZPositionChange(Sender: TObject);
// ------------------------
// Z stage position changed
// ------------------------
begin
    if not InitialisationComplete then Exit ;
    if RecordingMode <> rmRecordingInProgress then begin
       ZStage.UpdateDACBuffer( False,
                               sbZPosition.Position*ZStage.MinStepSize,
                               NumFramesPerCycle,
                               NumFramesPerZStep,
                               NumFramesPerWavelengthCycle,
                               NumDACPointsPerFrame,
                               DACUpdateInterval,
                               DACBufs ) ;
       edZPosition.Value := ZStage.Position ;
       end;
    end ;

procedure TRecordFrm.edZNumStepsKeyPress(Sender: TObject; var Key: Char);
begin
     if Key = #13 then ZStage.NumSteps := Round(edZNumSteps.Value) ;
     end;

procedure TRecordFrm.edZPositionKeyPress(Sender: TObject; var Key: Char);
// ------------------
// Z position changed
// ------------------
begin
      if (Key = #13) and (RecordingMode <> rmRecordingInProgress) then begin
       ZStage.UpdateDACBuffer( False,
                               edZPosition.Value,
                               NumFramesPerCycle,
                               NumFramesPerZStep,
                               NumFramesPerWavelengthCycle,
                               NumDACPointsPerFrame,
                               DACUpdateInterval,
                               DACBufs ) ;
         sbZPosition.Position := Round(ZStage.Position/ZStage.MinStepSize) ;
         edZPosition.Value := ZStage.Position ;
         end ;
      end;



procedure TRecordFrm.edZStartPosKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then ZStage.StartAt := edZStartPos.Value ;
    end;

procedure TRecordFrm.edZStepSizeKeyPress(Sender: TObject; var Key: Char);
begin
   if Key = #13 then ZStage.StepSize := edZStepSize.Value ;
   end;

procedure TRecordFrm.ckZStackEnabledClick(Sender: TObject);
// -----------------------
// Enable/disable Z stack
// -----------------------
begin
     ZStage.StackEnabled := ckZStackEnabled.Checked ;
     end;

procedure TRecordFrm.sbShowHideZStackSettingsClick(Sender: TObject);
// --------------------------
// Show/Hide Z stack settings
// --------------------------
begin
     ShowHideZStackSettings ;
     end;

end.
