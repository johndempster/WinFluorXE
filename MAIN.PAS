unit Main;
// ================================================                                                        average
// WinFluor - Fluorescence Image Capture Program
// (c) J. Dempster, University of Strathclyde, 2001-8
// All Rights Reserved
// ================================================
//
// 19.7.02
// 22.10.02 V1.1 Both image and A/D channels can be written to AVI file
// 7.3.03 V1.2 BioRad PIC files can now be imported
// 27.3.03 V1.3 Can now acquire up to 4 frame types (RecUnit.pas)
// 18.6.03 V1.4 Support for Hamamatsu C4880 added
// 2.7.03 V1.5
// 9.7.03 Frame types now have individual look-up table
// 29.7.03 TIDRFile introduced
// 6.8.03 Create Ratio/Ion module added
// 11.8.03 V1.8.2 2X ratio error in timecourseunit fixed
//                Minimum of display palette is now black (rather than blue)
// 18.8.05 V1.9 Time lapse recording mode added
// 7.9.03 V2.0.0 Multiple PIC, TIF & STK files can now be imported (FileIOUnit)
// 22.10.03 Min/max plot compression now implemented on all time course plots
// 13.11.03 Recording now prevented if A/D channels selected but no A/D converter available
// 04.03.04 Bugs in TimeCourseUnit.pas fixed
// 07.05.04 New file names now have index # appended
// 11.09.04 V2.3.3 Sent to David Wokosin 11/9/4
// 16.09.04 V2.3.4 update sent to DW
// 21.09.04 V2.3.5 Command voltage glitch when seal test window closed now fixed
// 20.12.04 V2.3.6 Background subtraction added to line scan time course
// 06.05.05 V2.4.3 Problems with Tom Carter's system fixed (labiounit.pas)
// 15.09.05 Camera trigger offset can be set manually
// 30.10.05 V2.6.3
// 16.11.05 V2.6.4 Support for Praire Technology Ultima added
// 28.02.06 V2.6.5 Mill Hill TIRF laser support added
// 26.04.06 V2.6.6 Overload of contrast range now distinctive colour
// 04.05.06 V2.7.0 Invalid memory access problem with histogram fixed
//                 Switching between seal test and record window now works
// 16.06.06 V2.7.5 2006 Chicago visit
// 17.07.06 V2.7.9 Support for Hamamatsu DCAM-API added
// 18.07.06 V2.8.0 PVCAM test with CoolSNAP fx
// 20.07.06 V2.8.1 Event detection window hang up when no events detected fixed
//                 Event detection parameters saved in INI file
//                 Forced step to holding potential at end of stimulus removed
// 11.08.06 V2.8.2
// 24.09.06 V2.8.6
// 25.09.06 V2.8.7 Event Analysis - dF/F0 and ratio-ing now done after averaging
//                 records. Record size of WCP export forced to multiple of 256
// 11.10.06 V2.8.8 Series of pulse trains protocol added
// 14.02.07 V2.9.1 Support for Photometrics Cascade added
// 25.04.07 V3.0.1 Digital outputs on PCI-6229 board now usable
// 08.05.07 V3.0.2 A/D channels settings and data now re-acquired from EDR
//                 if a mismatch exists between no. of A/D channels in IDR and EDR file
// 17.08.07 V3.0.8
// 29.08.07 V3.0.9 Digital pulses now kept within limits of DigBuf (stimmodule.pas)
//                 No. of increments in protocols no longer set to 1 when single steps updated
//                 Multiple-rate pulse train stimulus removed
// 25.01.08 V3.1.3 Support for an auxiliary camera for use when main camera is an LSM added
//                Live images window added
//                Now supports Prairie-View V2.4
// 14.05.08 V
//                New file now always adds sequence numbers to end of name
// 05.08.08 V3.2.4 Two amplifiers now supported
//                 Prairie-View V3.0 supported
//                 Photo stimulator added
//                 Camera temperature can be set by user (Andor only)
// 26.08.08 V3.2.5 Live Image window now has prototype shading correction
// 21.01.09 V3.2.6 
//                 Additional CameraReadoutTime property to CAM component
//                 Camera trigger delay now set to default value for light source
//                 when light source selected (can be changed by user later)
// 23.01.09 V3.2.6 Nicholas Schwarz's modifications added
// 15.04.09 V3.2.14 JD Display jump when setting contrast fixed
//                  Image area selection no longer progressively shrinks image
//                  External triggering with Hamamatsu Image-EM now works
// 21.05.09 V3.2.15 JD Ratio images can now be exported as TIF files
//                     Shutters can now be closed during blanking period at
//                     end of each frame, duration determined by LightSource.shutterblankingperiod
//                     Additional Camera Settings window added. Andor Ixon cooling, fan and EMCCD can be turned off
//                            6
// 07.09.09 V3.2.16 JD
// 17.09.09 V3.2.17 JD
// 23.01.10 V3.2.19 JD
// 27.01.10 V3.2.19 JD Automatic contrast adjustment added to recording and display windows
//                     Up to 10 multi-wavelength sequences can be defined and selected for menu
//                     ROI time course can be exported
//                     Export to Igor IBW files supported
// 05.02.10 V3.2.20 JD Auto contrast now applied at 1s intervals. Contrast settings stored in INI
//                     External triggering now works correctly with PVCAM cameras
// 28.07.10 V3.2.21 JD Event detector updated. Detectio now has separate tabe page.
//                      External triggering now supported with IMAQ
// 06.08.10 V3.2.22 JD Updates to event detector to work with line scan files
// 07.09.10 
// 21.10.10 V3.2.23 JD Analog signal filter module added (DigitalFilterFrm)
//                     Event Analysis: display buffer size increased to 1048576 points
//                     Event Analysis: Bug in export to WCP files fixed
//                     ExportROITimeCourse: Time course for ROI#s > ROI1 no longer set to zero
//                     ViewPlot/RecPlot: Max. display duration increased to 20000s
// 21.12.10 V3.2.26 JD Photostimulus module now works with camera-based systen
//                     Support for Cairn TIRF light source added
//                     Direct control form for control voltage outputs added (DirectControlFrm)
// 02.02.11 V3.2.27 JD CCDPostExposureReadout property added. Exposure time in ext. trigger mode can
//                     now be shortened to account for post-exposure readout in cameras which do not support
//                     overlapped readout mode. (Current only supported in PVCAM)
//                     QImaging camera support updated. Newer cameras now supported
//                     Width of calibration bar can now be set.
// 11.02.11 V3.2.3  JD 'camera busy' error when some QImaging cameras restarted fixed.
// 27.07.11 V3.3.0 JD Support for Data Translation frame grabbers added
//                 All ROIs can now be exported to file simulatenously
//                 File size of >2GB files now computed correctly (idrfile.pas)
//                 avoiding NumFrames mismatch warning when files loaded
// 30.08.11 V3.3.4 Hamamatsu DCAM: Higher scan speeds now set correctly when camera has more than
//                 one scan speed.
// 12.09.11 V3.3.5 Use Rectangle now working in Record to Disk window
// 27.02.12 V3.3.6 JD RecUnit.pas FrameTypeCycleLength now correctly set to 1 when no light source defined
//                 User defined number of records now collected (rather than multiple of half buffer size)
//                 Time lapse interval now correctly limited to no longer the duration of the timing waveform buffer
//                 IDRFile .. umFrames comparison with file size now only applied when NumFrames=0
//                 Read time out increased 1000 ms to avoid timing out during reads from slow disks
// 23.07.23 V3.3.8 JD Stimuli can now be started and stopped without a camera restart
// 17.09.12 V3.4.0 JD UpdateLUT: LUT buffer now completely filled (not just up to GreyMax)
//                 because Ixon cameras grey levels exceed stated maximum.
// 21.11.12 V3.4.1 JD UltimaUnit modified to work with Prairie View V4.3
// 15.11.12        DE Added LastRecADCOnly* variables to allow Record Signals
//                 window to remember its size and location after being
//                 dismissed (requested by M. Day)
// 30.11.12 V3.4.2 Time Lapse + Burst recording mode added (recunit.pas)
//                 T90 and T(decay) and frequency plots now calculated correctly in event detection & analysis
//                 No. points in zero level average now set correctly in WCP file export
// 14.12.12 V3.4.3 Turn on when recording option added. When selected, excitation light is turned on
//                 when recording starts and off at which it stops.
// 28.1.13 V3.4.4 Piezo Z focus and Z stack control added (ZStageUnit.pas)
// 03.04.13 V3.4.5 Imaging area selection rectangle now made more visible by small squares at corners and middle.
// 24.04.13 V3.4.6 LabIOUnit.pas Bug fix in NIDAQMX_MemoryToDAC() fixing access violations in RecADCOnlyUnit.pas
//                 when A/D restarted.
//                 RecADCOnlyUnit.pas: Real time display sweep now correctly synchronised to incoming signal
//                 FileIOUnit.pas: Default protocol folder now selected in folder in INI file not found
//                 wavgen.pas: SelectDirectory() dialog now used to set voltage protocol folder
// 15.05.13        NIDAQmx cameras: Mis-setting of CCD maximum width and height fixed.
// 03.06/13 V3.4.7 Support for Andor SDK 3 cameras addded (tested with Zyla)
//                 PTI/Till monochromator + LED/Laser light source added
//                 Memory violation when starting recording with light off and Turn on when recording enabled fixed
// 04.06.13 V3.4.8 In Record Images & Signals window, stimulus no longer initiated on start of recording when
//                 Stimulus Start on Record is ticked but no stimulus is available. Voltage protocol folder now selected using SelectDirectory
// 09.07.13 V3.4.9 NI cards: Number of DMA channels now correctly identified with PCI-6035E cards
// 24.07-13 V3.5.0 JD Now compiles unders Delphi XE2/3 as well as 7.
// 07-11-13 V3.52  JD Light source shutter: Shutter opening time can now be defined in setup allowing
//                   shutter to be opened at preset time before exposure in time lapse mode.
//                   LED Light Source: LED on/off voltages can now be set when using digital outputs (On=5V, Off=0V for active high LEDS, On=0V, Off=5V for active low LEDS).
//                   Z stage: Z stage movements now take place AFTER image (or series of images in
//                   multi-wavelength mode) are acquired in time lapse recording mode. In continuous recording mode, Z stage is moved at preset time before end of image acquisition frame. Illumination can be turned off or exposure terminated during Z stage movement period. Z step time defined in Z stage setup.
//                  Andor Ixon Cameras: Camera readout noise can now be optimised by
//                  adjusting Readout A/D gain and vertical shift timing in Setup/Extra Camera Settings.
// 09-12-13 V3.5.4 JD ITEX.PAS updated to compiled under XE3. Hamamatsu C4880 camera should now work again (since V3.5.0)
// 16-12-13 V3.5.5 JD Support for Hamamatsu FLASH 4.0 added. Setup now checks for missing multiple control outputs
// 31.01.14 V3.5.6 JD Updated to Compile under both 32/64 bits (File handle now THandle)
// 18.02.14 V3.5.7 JD Problems with Create Movie and Time Course window fixed
//                    now separate 32 and 64 bit versions of oslibrary.dll
// 27.02.14 V3.5.8 JD Emission filter control added to light control
//                    Bulb exposure mode added and % exposure in emission light control
// 15.04.14 V3.5.9 JD 64 bit version tested and working with FLASH 4.0 and PVCAM V3.0.1
// 16.05.14 V3.6.0 JD PVCAM_GetDLLAddress: in PVCAM.PAS (and other camera libraries)
//                    Handle now defined as THandle rather than Integer. Possible fix
//                    for unexplained failures to detect DLL procedures with 64 bit version
//                    of PVCAM
// 17.06.14 V3.6.1 JD Support for OptiMOS added
//                    Disk file writing speed now improved.
// 23.06.14 V3.6.1 JD Support for OptiMOS checked and working (no 3x3, 5x5 binning)
//                    Andor Now loads atmcd64d.dll in 64 bit version / unloads DLL when finished
// 09.07.14 V3.6.2 JD Calibration bar now sized correctly in Record Image & Record Image & Signals window
//                    NIMAQDXUnit.pas IMAQDX_SetVideoMode() and IMAQDX_SetPixelFormat() now trap Session.CameraOpen = false
//                    .CFG Settings now saved to settings directory rather than program directory
//                    Now detects QCAMDriverx64.dll correctly
// 22.07.14 V3.6.3 JD PVCAM & NIMAQdx: Camera can be selected when more than one is available
// 21.08.14 V3.6.5 JD Support for Thorlabs DCx cameras added
// 11.09.14 V3.6.6 JD 64 bit Andor SDK2 camera support now works.
//                    Bug in 64 bit Optoscan control fixed.
// 24.09.14 V3.6.7 JD Image files with long file names can now be exported again (since V3.5.9)
//                    (Word Wrap = False in meFiles memo box)
// 10.10.14 V3.6.8 JD OptiMOS minimum frame interval now 1.4 x readout time (not 2X)
//                    Support for Orca-100 with NI IMAQ interface
// 03.12.14 V3.6.9 JD Up to 8 LEDs can now be supported using digital control lines
//                    Intensity of LEDs controlled by analogue outputs can be set using Set Light Intensity dialog box
// 17.12.14 V3.7.0 JD QCAMUnit ReadoutTime lower limit reduced from 20ms to 10ms in free run mode
// 27.01.15 V3.7.1 JD Recording now restarted in time lapse mode when camera hangs up or
//                    images are a black level. File is closed/reopened after each time lapse interval (>2s)
//                    to ensure file directory info is preserved.
// 18.02.15 V3.7.2 JD Default file name prefix is date
//                    Support for Thorlabs MLS203 stage added
//                    Time courses can now be exported to MAT files
// 09.04.15 V3.7.3 JD RecUnit: real time image display now updated at correct rates
//                    when split rate multi-wavelength and split-CCD in use.
//                    ExportImagesUnit: Match frame count option added to separate file export
//                    No. of frames in separate wavelength files now matched to total no. of frames in file
// 14.05.15 V3.7.4 JD FrameInterval checking against readout time can be disabled for QImaging cameras
// 30.06.15 V3.7.5 JD XYStage: Now reads actual stage position allowing positions set by joystick to be saved.
// 25.09.15 V3.7.6 JD LED light source: Inverted TTL LED control (0=ON,5V=OFF) now works correctly
//                    USB-6002-5 interface cards now supported
//                    Analog channel and amplifier setup now on Analog Channel & Amplifiers page of
//                    Camera / System setup.
//                    Display time course trace compression now handled by Scopedisplay component.
//                    Auto contrast adjustment now works.

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, StdActns,
  ActnList, ToolWin, ImgList, SESCam, shared, ValEdit, SetupUnit, SealTest,
  StimSetupUnit, ImageFile, IDRFile, Sockets, UltimaUnit, ViewPlotUnit,
  EventAnalysisUnit, ExcSetupUnit, math, shlobj, UITypes, VCL.HTMLHelpViewer,
  Vcl.AppEvnts ;

const
    InitialisationFileName = 'WinFluor.ini' ;
    DataFileExtension = 'IDR' ;
    PICFileExtension = 'PIC' ;
    ROIFileExtension = 'ROI' ;
    ConfigFileExtension = 'CFG' ;
    MinPaletteColor = 11 ;
    MaxPaletteColor = 255 ;
    GreyScalePalette = 0 ;
    LUTSize = $10000 ;
    GreyLevelLimit = $FFFF ;
    FalseColourPalette = 1 ;
    SecsToMs = 1000.0 ;
    MsToSecs = 0.001 ;
    VoltsTomV = 1E3 ;
    mVtoVolts = 1E-3 ;
    NumStimulusPulses = 4 ;
    MaxEXCWavelengths = 20 ;
    MaxEXCSequences = 10 ;    
    ADCChannelLimit = 7 ;
    MaxADCChannels = 8 ;
    ADCMinInterval = 5E-5 ;
    NumCommandVoltageChannels = 3 ;
    NumDigitalStimulusChannels = 8 ;
    VProtSubFolderName = 'vprot\' ;
    PProtSubFolderName = 'pprot\' ;
    MaxFrameDivideFactor = 100 ;
    MaxStimPoints = 100 ;
    MaxLSControlLine = 7 ;
type

 TSmallIntArray = Array[0..4095*4095] of SmallInt ;
 PSmallIntArray = ^TSmallIntArray ;

 TPaletteType = (palGrey,palGreen,palRed,palBlue,palFalseColor) ;

  TEXCWavelength = record
         Centre : Integer ;
         Width : Integer ;
         EmFilter : Integer ;
         EmName : string ;
         Bits : Word ;
         FractionalExposure : single ;
         end ;

  TIOConfig = record
      ADCIn : Integer ;
      CameraStart : Integer ;
      CameraStartActiveHigh : Boolean ;
      VCommand : Array[0..NumCommandVoltageChannels-1] of Integer ;
      LSShutter : Integer ;
      LSShutterActiveHigh : Boolean ;
      LSControlLine : Array[0..MaxLSControlLine] of Integer ;
      //LSWavelengthStart : Integer ;
      //LSWavelengthEnd : Integer ;
      //LSLaserStart : Integer ; // 9/3/7 Separate laser control line range
      //LSLaserEnd : Integer ;   // 9/3/7
      EmFilterStart : Integer ;   // 27.2.14 Emission filter control lines
      EmFilterEnd : Integer ;     // 27.2.14
      DigitalStimStart : Integer ;
      DigitalStimEnd : Integer ;
      PhotoStimX : Integer ;
      PhotoStimY : Integer ;
      PhotoStimI1 : Integer ;
      PhotoStimI2 : Integer ;
      PhotoStimI3 : Integer ;
      PhotoStimMeter : Integer ;             // Modified by NS 21 May 2009
      PhotoStimMeterRange : Single ;
      PhotoStimMeterScale : Single ;
      PhotoStimShutter : Integer ;           // Modified by NS 17 September 2008
      PhotoStimShutterActiveHigh : Boolean ; // Modified by NS 17 September 2008
      PhotoStimShutterLatency : Single ;     // Modified by NS 23 September 2008
      PhotoStimPowerCalManual : Boolean ;    // Modified by NS 21 May 2009
      ZStageControl : Integer ;              // Z Stage control line JD 23.1.13
      ClockSyncLine : Integer ;
      end ;

  TVCommand = Record
      DivideFactor : Single ;
      HoldingVoltage : Single ;
      end ;

  TPhotoStim = Record
      // Ultima command terminator
      CmdTermZero : Boolean;
      // Used by RecADCOnly for running protocol
      Enabled : Boolean ;                 // Photo-stimulus protocol enabled
      RepeatedStim : Boolean ;            // Repeat stimulus flag
      Period : Single ;                   // Repeat stimulus period
      Attenuator : Integer ;              // Seleteced attenuator/galvo pair
      NumStimPoints : Integer ;           // Num targets in current protocol
      // Modified/used by PhotoStimUnit for protocol setup
      XMicronsPerPixel : Single ;         // X um per pixel in reference image
      YMicronsPerPixel : Single ;         // Y um per pixel in reference image
      GreyLo : Integer;                   // Low contrast setting
      GreyHi : Integer;                   // High contrast setting
      RefLinePos : Single ;               // Position of reference line
      RefLineEnabled : Boolean ;          // Show reference line
      // PrairieView/Ultima LSM specific variables modified by UltimaUnit
      PMTFileNames : Array[0..4] of String ;
      PMTChannels : Array[0..4] of Integer ;
      PMTChannelNames : Array[0..4] of String ;
      NumPMTFiles : Integer ;
      PVLogFile : String ;                // PrairieView log file path and name
      ImageRotation : Single ;            // Image rotation
      ROIXVoltageOffset : Double ;        // X voltage offset when in ROI mode
      ROIYVoltageOffset : Double ;        // Y voltage offset when in ROI mode
      // Calibration
      XCenter : Array[1..3] of Single ;                // X center
      YCenter : Array[1..3] of Single ;                // Y center
      XScale : Array[1..3] of Single ;                 // X scale
      YScale : Array[1..3] of Single ;                 // Y scale
      PCEnable : Array[1..3] of Boolean ;              // Use Pockels cell
      PCPowerMin : Array[1..3] of Single ;             // Min power in mW
      PCPowerMax : Array[1..3] of Single ;             // Max power in mW
      PCBias : Array[1..3] of Single ;                 // Net bias
      PCVoltagePi : Array[1..3] of Single ;            // VPi
      PCPolarizationCross : Array[1..3] of Boolean ;   // Cross polarization
      PCConoptics302 : Array[1..3] of Boolean ;        // 302 driver
      LinearPowerMin : Array[1..3] of Single ;         // Min power in mW
      LinearPowerMax : Array[1..3] of Single ;         // Max power im mW
      LinearVoltageMin : Array[1..3] of Single ;       // Voltage for min power
      LinearVoltageMax : Array[1..3] of Single ;       // Voltage for max power
      EnableShutter : Array[1..3] of Boolean ;         // Enable hard shutter
      end ;

  TDynamicProtocol = Record
      SelectedChannel : Integer ;  // Analogue input channel
      Direction : Integer ;        // Above = 0 ; Below = 1
      Threshold : Single ;         // Threshold value
      Duration : Single ;          // Duration threshold must be maintained
      EPRestart : Boolean ;        // Restart electro-physiology if true
      PSRestart : Boolean ;        // Restart photo-stimulus if true
      Enabled : Boolean ;          // Check box checked on RecADCOnlyUnit form
      EPStimFileName : String ;    // Stimulus file to start at threshold
      EPStimIndex : Integer ;      // Index of stimulus selection in combo box
      PSStimFileName : String ;    // Photo-timulus file to start at threshold
      PSStimIndex : Integer ;      // Index of photo-stim selection in combo box
      end;

  TPlayback = Record
      SelectedChannel : Integer ;  // Analogue channel to playback
      Enabled : Boolean ;          // Check box checked on RecADCOnlyUnit form
      FileName : String ;          // IDR file for playback
      Start : Single ;             // Start time
      Stop : Single ;              // Stop time
      StartPS : Single ;           // Start time for photo-stimulus protocol
      DisplayInterval : Single ;   // Display interval
      VOutChannel : Integer ;      // Voltage output channel for playbakc
      VOutStart : Single ;         // Start time - command voltage control
      VOutStop : Single;           // Stop time - command voltage control
      VOutVoltage : Array[0..2] of Single ;  // Voltage values for each output
      end;

  TCapacity = record
      Enabled : Boolean ;
      Frequency : Single ;
      VRev : Single ;
      CompensationInUse : Boolean ;
      RSeriesComp : Single ;
      CellCapacityComp : Single ;
      GmDisplayMax : Single ;
      GmScale : Single ;
      GmOffset : Integer ;
      GsDisplayMax : Single ;
      GsScale : Single ;
      GsOffset : Integer ;
      CmDisplayMax : Single ;
      CmScale : Single ;
      CmOffset : Integer ;
      ImChan : Integer ;
      ImZero : Integer ;
      ImOffset : Integer ;
      ImScale : Single ;
      ImSmoothed : Single ;
      VmChan : Integer ;
      VmZero : Integer ;
      VmOffset : Integer ;
      VmScale : Single ;
      VmSmoothed : Single ;
      GRChan : Integer ;
      GRZero : Integer ;
      GROffset : Integer ;
      GRScale : Single ;
      GRInvert : Boolean ;
      GIChan : Integer ;
      GIZero : Integer ;
      GIOffset : Integer ;
      GIScale : Single ;
      GIInvert : Boolean ;
      GMChan : Integer ;
      GsChan : Integer ;
      CMChan : Integer ;
      NewCalculation : Boolean ;
      GChannelsUseGainTelegraph : Boolean ;
      end ;


  TMainFrm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnNewFile: TMenuItem;
    mnOpenFile: TMenuItem;
    mnCloseFile: TMenuItem;
    Windows: TMenuItem;
    Help1: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    Edit1: TMenuItem;
    StatusBar: TStatusBar;
    ImageList1: TImageList;
    mnView: TMenuItem;
    mnRecordMenu: TMenuItem;
    mnRecord: TMenuItem;
    SaveDialog: TSaveDialog;
    mnViewImages: TMenuItem;
    mnAnalysis: TMenuItem;
    mnTimeCourse: TMenuItem;
    Setup: TMenuItem;
    mnCameraSetup: TMenuItem;
    mnHistogram: TMenuItem;
    mnCopyPlotData: TMenuItem;
    mnCopyImage: TMenuItem;
    mnSaveAsTIFF: TMenuItem;
    mnPrintGraph: TMenuItem;
    mnPrintSetup: TMenuItem;
    mnStimSetup: TMenuItem;
    mnEXCSetup: TMenuItem;
    mnSealTest: TMenuItem;
    PrinterSetupDialog: TPrinterSetupDialog;
    mnRecentFileSeparator: TMenuItem;
    mnRecentFile0: TMenuItem;
    mnRecentFile1: TMenuItem;
    mnRecentFile2: TMenuItem;
    mnRecentFile3: TMenuItem;
    mnCreateAVI: TMenuItem;
    mnImport: TMenuItem;
    mnExport: TMenuItem;
    mnDummy: TMenuItem;
    mnAverage: TMenuItem;
    mnLineProfile: TMenuItem;
    mnComputedImage: TMenuItem;
    mnSetupIon: TMenuItem;
    mnCopyPlotImage: TMenuItem;
    mnPrintImage: TMenuItem;
    mnExportImages: TMenuItem;
    mnExportAnalogue: TMenuItem;
    mnSaveSettings: TMenuItem;
    mnLoadSettings: TMenuItem;
    N2: TMenuItem;
    mnContents: TMenuItem;
    mnEventAnalysis: TMenuItem;
    mnProperties: TMenuItem;
    mnSaveAs: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    mnStimulusDefaultSettings: TMenuItem;
    mnDisplayGrid: TMenuItem;
    mnZoomOutAll: TMenuItem;
    mnSpectrumAnalysis: TMenuItem;
    mnSnapImage: TMenuItem;
    mePhotoStim: TMenuItem;
    mnSmoothDifferentiate: TMenuItem;
    mnCameraSettings: TMenuItem;
    mnExportROITimeCourse: TMenuItem;
    mnDigitalFilter: TMenuItem;
    mnDirectControl: TMenuItem;
    IDRFile: TIDRFile;
    mnXYStage: TMenuItem;
    ApplicationEvents: TApplicationEvents;
    Cam1: TSESCam;
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mnRecordClick(Sender: TObject);
    procedure mnViewImagesClick(Sender: TObject);
    procedure mnTimeCourseClick(Sender: TObject);
    procedure mnCameraSetupClick(Sender: TObject);
    procedure mnHistogramClick(Sender: TObject);
    procedure mnCopyPlotDataClick(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure mnCopyImageClick(Sender: TObject);
    procedure mnSaveAsTIFFClick(Sender: TObject);
    procedure mnSaveToTIFFClick(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure mnPrintGraphClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnStimSetupClick(Sender: TObject);
    procedure mnEXCSetupClick(Sender: TObject);
    procedure mnSealTestClick(Sender: TObject);
    procedure mnPrintSetupClick(Sender: TObject);
    procedure mnRecentFile0Click(Sender: TObject);
    procedure mnCreateAVIClick(Sender: TObject);
    procedure mnImportClick(Sender: TObject);
    procedure mnExportClick(Sender: TObject);
    procedure mnAnalysisClick(Sender: TObject);
    procedure mnAverageClick(Sender: TObject);
    procedure mnLineProfileClick(Sender: TObject);
    procedure mnComputedImageFrmClick(Sender: TObject);
    procedure mnSetupIonClick(Sender: TObject);
    procedure mnRecordMenuClick(Sender: TObject);
    procedure mnCopyPlotImageClick(Sender: TObject);
    procedure mnPrintImageClick(Sender: TObject);
    procedure mnRecordADCOnlyClick(Sender: TObject);
    procedure mnExportImagesClick(Sender: TObject);
    procedure mnExportAnalogueClick(Sender: TObject);
    procedure mnViewLineScansClick(Sender: TObject);
    procedure mnViewClick(Sender: TObject);
    procedure mnLoadSettingsClick(Sender: TObject);
    procedure mnSaveSettingsClick(Sender: TObject);
    procedure mnContentsClick(Sender: TObject);
    procedure SetupClick(Sender: TObject);
    procedure mnEventAnalysisClick(Sender: TObject);
    procedure mnPropertiesClick(Sender: TObject);
    procedure mnSaveAsClick(Sender: TObject);
    procedure mnCloseFileClick(Sender: TObject);
    procedure mnStimulusDefaultSettingsClick(Sender: TObject);
    procedure mnDisplayGridClick(Sender: TObject);
    procedure mnZoomOutAllClick(Sender: TObject);
    procedure mnSpectrumAnalysisClick(Sender: TObject);
    procedure mnSnapImageClick(Sender: TObject);
    procedure mePhotoStimClick(Sender: TObject);
    procedure mnSmoothDifferentiateClick(Sender: TObject);
    procedure mnCameraSettingsClick(Sender: TObject);
    procedure mnExportROITimeCourseClick(Sender: TObject);
    procedure mnDigitalFilterClick(Sender: TObject);
    procedure mnDirectControlClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnXYStageClick(Sender: TObject);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
  private

    procedure SetRecentFileItem(
              MenuItem : TMenuItem ;
              FileName : string ) ;

    function GetSpecialFolder(const ASpecialFolderID: Integer): string;

  public
    { Public declarations }

    CameraType : Integer ;   // Camera hardware type
    AuxCameraType : Integer ;   // Auxiliary camera hardware type
                                // (Used only when main camera is laser scanning microscope)
    CameraTriggerOffset : Single ;  // Camera trigger offset (s)
    VideoMode : Integer ;

    IOConfig : TIOConfig ;   // Hardware configuration
      // Digital output default settings

    Recording : Boolean ;         // True = recording to disk in progress

    // Display look-up tables
    GreyLo : Array[0..MaxFrameType] of Integer ; // Lower limit of display grey scale
    GreyHi : Array[0..MaxFrameType] of Integer ; // Upper limit of display grey scale
    LUTs : Array[0..((MaxFrameType+1)*LUTSize-1)] of Word ;    // Display look-up tables
    PaletteType : TPaletteType ;  // Display colour palette

    DarkLevelLo : Integer ;
    DarkLevelHi : Integer ;

    TScale : Single ;        // Time scaling factor
    TUnits : String ;        // Time units (s/ms)

    SplitImageName : Array[0..1] of string ;  // CCD name
    SplitImage : Boolean ;

    CalibrationBarSize : Single ; // Spatial calibration bar size
    CalibrationBarThickness : Single ; // Spatial calibration bar thickness
    
    ROISize : Integer ;           // Size (n x n pixels) of live ROI box
    ROIX : Integer ;              // X coord
    ROIY : Integer ;              // Y coord

    LiveWindowWavelength : Integer ; // Default light wavelength for live window

    // Data file fields
    NumFramesRequired : Integer ;    // Default no. frames per recording sequence
    RecordingPeriod : Single ;       // Default recording time (s)
    ADCRecordingTime : Single ;  // Default recording time (A/D only recording)
    RecordingMode : Integer ;        // Recording mode (Continuous,Time Lapse, Time Lapse + Burst)
    TimeLapseInterval : Single ;     // Time lapse interval
    BurstDuration : Single ;         // Duration of high speed bursts (T. Lapse + Burst mode)
    BurstInterval : Single ;         // Interval between high speed bursts (T. Lapse + Burst mode)
    BulbExposureMode : Boolean ;     // Bulb exposure mode: exposure trigger pulse duration determines camera exposure time

    ProgramDirectory : String ;      // Program directory
    DataDirectory : string ;         // Current data directory
    DefaultDataDirectory : string ;  // Default data directory
    SettingsDirectory : string ;     // Settings directory
    SettingsFileName : string ;      // Full path to winfluor.ini settings file

    // Current A/D channel settings
    ADCNumChannels : Integer ;       // No. of A/D channels
    ADCScanInterval : Double ;       // Sampling interval (s) (channel groups)
    ADCDisplayWindow : Single ;      // Duration of A/D signals display window (s)
    ADCVoltageRange : Single ;
    ADCChannel : Array[0..MaxADCChannels-1] of TChannel ; // A/D channel definitions

    // Current visibility state of charts
    // Modified by NS 19 March 2009
    ADCChannelRecADCOnlyUnitVisible : Array[0..MaxADCChannels-1] of Boolean ;

    // Current visibility state of charts in Sealtest
    // Modified by NS 24 March 2009
    ADCChannelSealtestNumberOfChannels : Integer ;
    ADCChannelSealtestVisible : Array[0..MaxADCChannels-1] of Boolean ;

    // State of SmoothDifferentiateFrm
    // Modified by NS 10 April 2009
    SmoothDifferentiateUnitVisible : Array[0..(4-1)] of Boolean;
    SmoothDifferentiateUnitMADataWindow : Integer;
    SmoothDifferentiateUnitMADXWindow : Integer;

    // Printer page settings
    PrinterTopMargin : Integer ;
    PrinterBottomMargin : Integer ;
    PrinterLeftMargin : Integer ;
    PrinterRightMargin : Integer ;
    PrinterLineThickness : Integer ;
    PrinterMarkerSize : Integer ;
    PrinterUseColor : Boolean ;
    PrinterFontName : string ;
    PrinterFontSize : Integer ;
    ClipboardBitMapWidth : Integer ;
    ClipboardBitMapHeight : Integer ;
    PrinterShowLabels : Boolean ;
    PrinterShowZeroLevels : Boolean ;
    PrinterTimeBarValue : Single ;
    PrinterCalibrationBar : Array[0..ADCChannelLimit] of Single ;
    PrinterTimeCalibrationBar : Single ;

    // Stimulus pulse settings
    VProtDirectory : String ;    // Stimulus voltage protocol folder
    DefVProtDirectory : String ; // Default stimulus voltage protocol folder
    StimFileName : String ;    // Stimulus file in current use
    //StimPulses : Array[0..NumStimulusPulses-1] of TStimulus ;
    //StimProgram : TStimProgram ;

    // Photo-stimulus settings
    PProtDirectory : String ;    // Photo-stimulus voltage protocol folder
    DefPProtDirectory : String ; // Default photo-stimulus voltage protocol folder
    PhotoStimFileName : String ; // Photo-stimulus file in current use

    // Excitation light control settings
    ExcitationOnWhenRecording : Boolean ; // Turn on excitation when recording
    EXCAvailable : Boolean ;           // Excitation light source available
    EXCShutterOpen : Boolean ;         // Excitation light shutter open/close
    EXCSingleWaveLength : Boolean ;    // Single/multiple wavelength excitation
    EXCSingleWavelengthNum : Integer ; // Single exc. wavelength no.
    EXCSequenceNum : Integer ;         // Selected sequence no.
    EXCNumWavelengths : Array[0..MaxEXCSequences-1] of Integer ; // No. of wavelengths in excitation sequence
    EXCSequenceName : Array[0..MaxEXCSequences-1] of String ;    // Sequence names
    EXCSequence : Array[0..MaxFrameType,0..MaxEXCSequences-1] of TEXCSequence ;  // Excitation sequence list
    EXCWavelengths : Array[0..MaxEXCWavelengths-1] of TEXCWavelength ;

    EXCSpectrumStartWavelength : Single ; // Start wavelength
    EXCSpectrumEndWavelength : Single ;   // End Wavelength
    EXCSpectrumStepSize : Single ;        // Number of steps
    EXCSpectrumBandwidth : Single ;       // Bandwidth (nm)
    EXCSpectrumEMFilter : Integer ;               // Emission filter #

    //EXCNewSettings :Boolean ;         // Flag indicating that settings have been changed

    SealTest : TSealTest ;            // Seal test module (sealtest.pas) settings
    VCommand : Array[0..NumCommandVoltageChannels-1] of TVCommand ;

    PhotoStim : TPhotoStim ; // Photo stimulus parameters

    DynamicProtocol : TDynamicProtocol ; // Dynamic protocol parameters

    Playback : TPlayback ; // Playback protocol parameters

    Cap : TCapacity ;

    //EventAnalysis : TEventAnalysisSettings ;   // Event analysis parameter

    // Ion binding equations
    BindingEquations : Array[0..MaxEqn] of TBindingEquation ;

    DisplayZoomIndex : Integer ;      // Display zoom menu choice

    TimeCourseRatioNumerator : Integer ;
    TimeCourseRatioDenominator : Integer ;
    TimeCourseRatioDisplayMax : Single ;
    TimeCourseRatioThreshold : Single ;

    ContrastChangeAllFrameTypes : Boolean ;
    ContrastAutoOptimise : Boolean ;
    Contrast6SD : Boolean ;

    IdentChanged : Boolean ;
    RecentFiles : Array[0..3] of String ;  // Recently used data files

    // SVN revisions
    WinFluorSVNRev : Integer ;
    ComponentsSVNRev : Integer ;

    AutoResetInterfaceCards : Boolean ;

    StartStimOnRecord : Boolean ;

    RecordADCSignalsOnly: Boolean;

    ProgramName : string ;

    FormPos : Array[0..99] of TRect ;   // form positions

    procedure SetPalette( BitMap : TBitMap ; PaletteType : TPaletteType ) ;

    procedure UpdateLUT( iFrameType : Integer ;
                         GreyMax : Integer ) ;

    procedure CloseWindows ;
    procedure UpdateRecentFilesList ;
    function CreateIndexedFileName( FileName : String ) : String ;
    function IOResourceAvailable( ResourceNum : Integer ) : Boolean ;
    procedure SetZoomChannel(
          MenuItem : TMenuItem ;
          ChanNum : Integer ;
          NumChannels : Integer ;
          const ADCChannel : TChannel
          ) ;
    function FormExists( FormName : String ) : Boolean ;
    function LSControlLinesAvailable :Boolean ;
    function LSNumControlLinesAvailable : Integer ;
    procedure SetFormPosition(
          Form : TForm ;             // Form
          Left : Integer ;           // Left edge of form
          Top : Integer ;            // Top edge
          Width : Integer ;          // Form width
          Height : Integer           // Form height
          ) ;
    procedure SaveFormPosition(
          Form : TForm ) ;
  end;

// Corrected system function call template
function GetSystemPaletteEntries(
         DC : HDC ;
         StartIndex : Cardinal ;
         NumEntries : Cardinal ;
         PaletteEntries : Pointer ) : Cardinal ; stdcall ;

var
  MainFrm: TMainFrm;

implementation

{$R *.DFM}


uses About, RecUnit, ViewUnit, TimeCourseUnit,
  LabIOUnit, HistogramUnit, FileIOUnit,
  mmsystem, AVIUnit , AverageUnit,
  LineProfileUnit, RatioUnit, StrUtils , SetupIonUnit,
  RecADCOnlyUnit, exportAnalogueUnit, ViewLineUnit,
  LogUnit, LightSourceUnit, ExportImagesUnit, Wavgen,
  FilePropsUnit, SaveAsFileUnit, RecPlotUnit, StimulusDefaultsUnit,
  SpectrumUnit, SnapUnit, PhotoStimUnit, SmoothDifferentiateUnit,
  CameraSettingsUnit, ExportROITImeCourseUnit, DynamicProtocolSetupUnit,
  DigitalFilterUnit, DirectControlUnit, XYStageUnit;



function GetSystemPaletteEntries ; external gdi32 name 'GetSystemPaletteEntries' ;


procedure TMainFrm.FormShow(Sender: TObject);
// --------------------------------------------
// Initialisations when form is first displayed
// --------------------------------------------
var
     i,ch : Integer ;
begin

    Caption := 'WinFluor : Fluorescence Image Capture & Analysis Program V3.7.6 12-10-15 ' ;
{$IFDEF WIN64}
    Caption := Caption + '(64 bit) ';
{$ELSE}
    Caption := Caption + '(32 bit) ';
{$IFEND}
    ProgramName := Caption ;

    EXCAvailable := True ;

    Left := 20 ;
    Top := 20 ;

    CameraType := RS_PVCAM ; {ITEX_CCIR}
    CameraType :=  NoCamera16 ;

    // Initial frame capture settings
    Cam1.FrameLeft := 0 ;
    Cam1.FrameRight := 255 ;
    Cam1.FrameTop := 0 ;
    Cam1.FrameBottom := 255 ;
    Cam1.FrameInterval := 0.1 ;
    Cam1.CameraTemperatureSetPoint := -50.0 ;
    CameraTriggerOffset := 0.0 ;
    Cam1.AdditionalReadoutTime := 0.0 ;

    DarkLevelLo := -1 ;
    DarkLevelHi := -1 ;

    // Default number of frames per recording sequence
    NumFramesRequired := 100 ;
    RecordingPeriod := 60.0 ;
    RecordingMode := rmContinuous ;

    // Default recording time (A/D only recording)
    ADCRecordingTime := 10.0 ;

    // Start stimulus when record button pressed
    StartStimOnRecord := True ;

    // Default display palette
    PaletteType := palGrey ;  // Grey scale

    ROISize := 1 ;
    ROIX := 0 ;
    ROIX := 0 ;
    LiveWindowWavelength := 0 ;

    // Split image settings
    SplitImageName[0] := 'A' ;
    SplitImageName[1] := 'B' ;
    SplitImage := False ;

    BulbExposureMode := False ;

    // Time display units/scaling factor
    TScale := 1.0 ;
    TUnits := 's' ;

    CalibrationBarSize := 10.0 ;
    CalibrationBarThickness := 1.0 ;

    TimeLapseInterval := 60 ;  // 60 seconds
    BurstDuration := 1.0 ;
    BurstInterval := 10.0 ;

    // Initial ADC input channel settings
    ADCNumChannels := 2 ;
    ADCVoltageRange := 10.0 ;
    ADCScanInterval := 1E-4 ;
    ADCDisplayWindow := 10.0 ;
    IDRFile.ADCMaxValue := 2047 ;

    EXCSequenceNum := 0 ;
    for i := 0 to MaxEXCSequences-1 do begin
        EXCSequenceName[i] := Format('Sequence %d',[i+1]) ;
        EXCNumWavelengths[i] := 0 ;
        end ;

    for i := 0 to High(EXCWavelengths) do begin
        EXCWavelengths[i].Centre := 0 ;
        EXCWavelengths[i].Width := 0 ;
        EXCWavelengths[i].EmFilter := 0 ;
        EXCWavelengths[i].EmName := '' ;
        EXCWavelengths[i].FractionalExposure := 1.0 ;
        end ;

    EXCSpectrumStartWavelength := 400 ;
    EXCSpectrumEndWavelength := 500 ;
    EXCSpectrumBandwidth := 10.0 ;
    EXCSpectrumStepSize := 10.0 ;
    EXCSpectrumEMFilter := 0 ;
    ExcitationOnWhenRecording := True ;

    // Interface hardware I/O configuration
    IOConfig.ADCIn := -1 ;
    IOConfig.CameraStart := -1 ;
    IOConfig.CameraStartActiveHigh := True ;
    for i := 0 to High(IOConfig.VCommand) do IOConfig.VCommand[i] := -1 ;
    IOConfig.LSShutter := -1 ;
    IOConfig.LSShutterActiveHigh := True ;
//    IOConfig.LSLaserStart := -1 ;
//    IOConfig.LSLaserEnd := -1 ;
    IOConfig.EmFilterStart := -1 ;
    IOConfig.EmFilterEnd := -1 ;
    IOConfig.DigitalStimStart := -1 ;
    IOConfig.DigitalStimEnd := -1 ;
    IOConfig.PhotoStimX := -1 ;
    IOConfig.PhotoStimY := -1 ;
    IOConfig.PhotoStimI1 := -1 ;
    IOConfig.PhotoStimI2 := -1;
    IOConfig.PhotoStimI3 := -1;
    IOConfig.PhotoStimShutter := -1 ;             // Modified by NS 17 September 2008
    IOConfig.PhotoStimShutterActiveHigh := True ; // Modified by NS 17 September 2008
    IOConfig.PhotoStimShutterLatency := 0.0 ;     // Modified by NS 23 September 2008
    IOconfig.PhotoStimMeter := -1 ;               // Modified by NS 21 May 2009
    IOconfig.PhotoStimMeterRange := 0.0 ;
    IOconfig.PhotoStimMeterScale := 0.0 ;
    IOconfig.PhotoStimPowerCalManual := True ;    // Modified by NS 21 May 2009
    IOConfig.ClockSyncLine := ClockSync_RTSI0 ;

    ADCVoltageRange := 10.0 ;

    // Command voltage O/P settings
    MainFrm.VCommand[0].DivideFactor := 1.0 ;    // Command voltage divide factor
    MainFrm.VCommand[0].HoldingVoltage := 0.0 ;  // Holding voltage
    MainFrm.VCommand[1].DivideFactor := 1.0 ;
    MainFrm.VCommand[1].HoldingVoltage := 0.0 ;
    MainFrm.VCommand[2].DivideFactor := 1.0 ;
    MainFrm.VCommand[2].HoldingVoltage := 0.0 ;

    LightSource.Wavelength1 := 340.0 ;
    LightSource.Voltage1 := -2.25 ;
    LightSource.Wavelength2 := 495.0 ;
    LightSource.Voltage2 := 0.2 ;
    LightSource.ShutterClosedWavelength := 550.0 ;
    LightSource.ShutterBlankingPeriod := 0.0 ;

    PhotoStim.CmdTermZero := False;
    PhotoStim.Period := 1.0 ;
    PhotoStim.Attenuator := 0 ;
    PhotoStim.RepeatedStim := False ;
    PhotoStim.NumStimPoints := 0 ;
    for i := 1 to 3 do
    begin
      PhotoStim.XCenter[i] := 0.0 ;
      PhotoStim.YCenter[i] := 0.0 ;
      PhotoStim.XScale[i] := 0.0 ;
      PhotoStim.YScale[i] := 0.0 ;
      PhotoStim.PCEnable[i] := False ;
      PhotoStim.PCPowerMin[i] := 0.0 ;
      PhotoStim.PCPowerMax[i] := 0.0 ;
      PhotoStim.PCBias[i] := 0.0 ;
      PhotoStim.PCVoltagePi[i] := 0.0 ;
      PhotoStim.PCPolarizationCross[i] := False ;
      PhotoStim.PCConoptics302[i] := True ;
      PhotoStim.LinearPowerMin[i] := 0.0 ;
      PhotoStim.LinearPowerMax[i] := 0.0 ;
      PhotoStim.LinearVoltageMin[i] := 0.0 ;
      PhotoStim.LinearVoltageMax[i] := 0.0 ;
      PhotoStim.EnableShutter[i] := False ;
    end;
    PhotoStim.ImageRotation := 0.0 ;
    PhotoStim.XMicronsPerPixel := 0.0 ;
    PhotoStim.Enabled := False ;
    PhotoStim.GreyLo := 0 ;
    PhotoStim.GreyHi := 4095 ;
    PhotoStim.RefLinePos := 0.0 ;
    PhotoStim.RefLineEnabled := True ;
    PhotoStim.ROIXVoltageOffset := 0.0;
    PhotoStim.ROIYVoltageOffset := 0.0;

    // Default dynamic protocol parameters
    DynamicProtocol.SelectedChannel := 0 ;
    DynamicProtocol.Direction := 0 ;
    DynamicProtocol.Threshold := 1.0 ;
    DynamicProtocol.Duration := 1.0 ;
    DynamicProtocol.EPRestart := False ;
    DynamicProtocol.PSRestart := False ;
    DynamicProtocol.Enabled := False ;
    DynamicProtocol.EPStimFileName := '' ;
    DynamicProtocol.EPStimIndex := 0 ;

    // Default playback parameters
    Playback.SelectedChannel := 0;
    Playback.Enabled := False;
    Playback.FileName := '';
    Playback.Start := 0.0;
    Playback.Stop := 0.5;
    Playback.StartPS := 0.25;
    Playback.DisplayInterval := 1.0;
    Playback.VOutChannel := 0;
    Playback.VOutStart := 0.0;
    Playback.VOutStop := 0.0;
    Playback.VOutVoltage[0] := 0.0;
    Playback.VOutVoltage[1] := 0.0;
    Playback.VOutVoltage[2] := 0.0;

    DisplayZoomIndex := 1 ;       // Display zoom menu choice

     { Seal Test/Monitor module default settings }
     SealTest.Use := 1 ;
     SealTest.PulseHeight1 := 0.01 ;
     SealTest.HoldingVoltage1 := 0. ;
     SealTest.PulseHeight2 := 0.01 ;
     SealTest.HoldingVoltage2 := 0. ;
     SealTest.PulseHeight3 := 0.0 ;
     SealTest.HoldingVoltage3 := 0. ;

     SealTest.PulseWidth:= 0.01 ;
     SealTest.CurrentChannel := 0 ;
     SealTest.VoltageChannel := 1 ;
     SealTest.AutoScale := True ;
     SealTest.DisplayScale := 1 ;
     { Set flag indicating this is the first sweep, to force an autoscale }
     SealTest.FirstSweep := True ;

     // Printer page settings
     PrinterTopMargin := 50 ;
     PrinterBottomMargin := 50 ;
     PrinterLeftMargin := 50 ;
     PrinterRightMargin := 50 ;
     PrinterLineThickness  := 1 ;
     PrinterMarkerSize := 10 ;
     PrinterUseColor := False ;
     PrinterFontName := 'Arial' ;
     PrinterFontSize := 10 ;
     PrinterShowLabels := True ;
     PrinterShowZeroLevels := True ;
     PrinterUseColor := True ;
     PrinterTimeCalibrationBar := 0.0 ;
     for ch := 0 to High(MainFrm.PrinterCalibrationBar) do
         MainFrm.PrinterCalibrationBar[ch] := 0.0 ;

     // Auto reset interface cards
     AutoResetInterfaceCards := False ;

     TimeCourseRatioNumerator := 0 ;
     TimeCourseRatioDenominator := 1 ;
     TimeCourseRatioDisplayMax := 2.0 ;
     TimeCourseRatioThreshold := 1 ;

     // Capacity default settings
      Cap.Enabled := False ;
      Cap.Frequency := 1000.0 ;
      Cap.VRev := -0.09 ;
      Cap.CompensationInUse := False ;
      Cap.RSeriesComp := 0.0 ;
      Cap.CellCapacityComp := 0.0 ;
      Cap.GmDisplayMax := 100 ;
      Cap.GmScale := 1.0 ;
      Cap.GmOffset := 0 ;
      Cap.GsDisplayMax := 1000.0 ;
      Cap.GsScale := 1.0 ;
      Cap.GsOffset := 0 ;
      Cap.CmDisplayMax := 100.0 ;
      Cap.CmScale := 1.0 ;
      Cap.CmOffset := 0 ;
      Cap.ImChan := 0 ;
      Cap.ImZero := 0 ;
      Cap.ImOffset := 0 ;
      Cap.ImScale := 1.0 ;
      Cap.ImSmoothed := 0.0 ;
      Cap.VmChan := 1 ;
      Cap.VmZero := 0 ;
      Cap.VmOffset := 0 ;
      Cap.VmScale := 1.0 ;
      Cap.VmSmoothed := 0. ;
      Cap.GRChan := 2 ;
      Cap.GRZero := 0 ;
      Cap.GROffset := 0 ;
      Cap.GRScale := 1.0 ;
      Cap.GRInvert := False ;
      Cap.GIChan := 3 ;
      Cap.GIZero := 0 ;
      Cap.GIOffset := 0 ;
      Cap.GIScale := 1.0  ;
      Cap.GIInvert := False ;
      Cap.GMChan := 4 ;
      Cap.GsChan := 5 ;
      Cap.CMChan := 6 ;
      Cap.NewCalculation := True ;
      Cap.GChannelsUseGainTelegraph := False ;


     // Clipboard image settings
     ClipboardBitMapWidth := 600 ;
     ClipboardBitMapHeight := 400 ;

     // Default contrast optimisation settings
     ContrastChangeAllFrameTypes := True ;
     ContrastAutoOptimise := True ;
     Contrast6SD := False ;

     // Program folder

     // Photo-stimulus protocol file folder
     DefPProtDirectory := ProgramDirectory + PProtSubFolderName ;
     if not DirectoryExists(DefPProtDirectory) then CreateDir(DefPProtDirectory) ;
     PProtDirectory := DefPProtDirectory ;

     // Clear recent files opened list
     for i := 0 to High(RecentFiles) do RecentFiles[i] := '' ;

    // Load settings from initialisation file (to get camera type in use)
    FileIO.LoadInitialisationFile( SettingsDirectory + InitialisationFileName ) ;

    // Open camera (use auxiliary camera if main camera is an LSM)
    if Cam1.IsLSM(CameraType) then Cam1.OpenCamera(AuxCameraType)
                              else Cam1.OpenCamera(CameraType) ;

    // Open TCP/IP connection with Ultima LSM program
    if CameraType = UltimaLSM then Ultima.Connect ;

    for ch := 0 to MaxADCChannels-1 do begin
        ADCChannel[ch].ChannelOffset := ch ;
        ADCChannel[ch].TimeZero := 1. ;
        ADCChannel[ch].ADCScale := 1. ;
        ADCChannel[ch].CursorIndex := 128 ;
        ADCChannel[ch].ZeroIndex := 0 ;
        ADCChannel[ch].Cursor0 := 0 ;
        ADCChannel[ch].Cursor1 := 0 ;
        { Zero levels fixed at hardware zero }
        ADCChannel[ch].ADCZero := 0 ;
        ADCChannel[ch].ADCZeroAt := -1 ;
        ADCChannel[ch].ADCCalibrationFactor := 0.001 ;
        ADCChannel[ch].ADCAmplifierGain := 1. ;
        ADCChannel[ch].ADCUnits := 'mV' ;
        ADCChannel[ch].ADCName := format('Ch.%d',[ch]);
        ADCChannel[ch].color := clBlue ;
        ADCChannel[ch].yMax := LabIO.ADCMaxValue[1] ;
        ADCChannel[ch].yMin := -LabIO.ADCMaxValue[1] - 1 ;
        ADCChannel[ch].InUse := True ;
        end ;

    // Initialize chart visiblity states
    // Modified by NS 19 March 2009
    for ch := 0 to MaxADCChannels-1 do begin
      ADCChannelRecADCOnlyUnitVisible[ch] := True ;
    end ;

    // Initialize chart visiblity states for Sealtest
    // Modified by NS 24 March 2009
    for ch := 0 to MaxADCChannels-1 do begin
      ADCChannelSealtestVisible[ch] := True ;
    end ;
    ADCChannelSealtestNumberOfChannels := 0 ;

    for ch := 0 to (4 - 1) do begin
      SmoothDifferentiateUnitVisible[ch] := True ;
    end ;
    SmoothDifferentiateUnitMADataWindow := 1 ;
    SmoothDifferentiateUnitMADXWindow := 1 ;

    // Load INI file again to ensure open camera  settings are updated
    FileIO.LoadInitialisationFile( SettingsDirectory + InitialisationFileName ) ;

    // Open National Instruments laboratory interfaces
    LabIO.Open ;

    for ch := 0 to MaxADCChannels-1 do begin
        ADCChannel[ch].yMax := LabIO.ADCMaxValue[1] ;
        ADCChannel[ch].yMin := -LabIO.ADCMaxValue[1] - 1 ;
        end ;

    Recording := False ;

    IdentChanged := False ;
    // Initialise look-up tables
    for i := 0 to MaxFrameType do begin
        GreyLo[i] := 0 ;
        GreyHi[i] := Cam1.GreyLevelMax ;
        UpdateLUT( i, Cam1.GreyLevelMax ) ;
        end ;

    // Add names of recently accessed data files to Files menu
    mnRecentFileSeparator.Visible := False ;
    SetRecentFileItem( mnRecentFile0, RecentFiles[0] ) ;
    SetRecentFileItem( mnRecentFile1, RecentFiles[1] ) ;
    SetRecentFileItem( mnRecentFile2, RecentFiles[2] ) ;
    SetRecentFileItem( mnRecentFile3, RecentFiles[3] ) ;

    // Set SVN revisions
    MainFrm.WinFluorSVNRev := 142 ;
    MainFrm.ComponentsSVNRev := 36 ;

    WindowMenu := Windows ;

    // Start with log form closed
    LogFrm.Close ;
    if FileExists( ParamStr(1)) then begin
       IDRFile.OpenFile( ParamStr(1) ) ;
       DataDirectory := ExtractFilePath(IDRFile.FileName) ;
       LogFrm.AddLine( 'File opened: ' + IDRFile.FileName );
       // Display in program title bar
       Caption := 'WinFluor : ' + IDRFile.FileName ;
       if IDRFile.NumFrames > 0 then begin
          UpdateRecentFilesList ;
          mnViewImages.Click ;
          end ;
       end ;

    RecordADCSignalsOnly := False;

    // Set main form position
    SetFormPosition( MainFrm,
                     20,
                     20,
                     Screen.Width - 40,
                     Screen.Height - 80 );

    end;


procedure TMainFrm.FileNew1Execute(Sender: TObject);
// -------------------------------
// Create new .IDR image data file
// -------------------------------
begin

     // Close existing file
     mnCloseFile.Click ;

     // Present user with standard Save File dialog box
     SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;

     // Open last used data directory
     if DirectoryExists(DataDirectory) and (DataDirectory <> '') then begin
        SaveDialog.InitialDir := DataDirectory ;
        end
     else SaveDialog.InitialDir := DefaultDataDirectory ;

     SaveDialog.DefaultExt := DataFileExtension ;
     SaveDialog.FileName := CreateIndexedFileName( IDRFile.FileName ) ;
     SaveDialog.Filter := format(' %s Files (*.%s)|*.%s',
                          [DataFileExtension,DataFileExtension,DataFileExtension]);
     SaveDialog.Title := 'New Data File' ;

     if SaveDialog.execute then begin
        SaveDialog.FileName := ChangeFileExt( SaveDialog.FileName, '.'+DataFileExtension ) ;
        IDRFile.CreateNewFile( SaveDialog.FileName ) ;
        DataDirectory := ExtractFilePath(SaveDialog.FileName) ;
        LogFrm.AddLine( 'New file created: ' + SaveDialog.FileName );
        Caption := 'WinFluor : ' + IDRFile.FileName ;
        UpdateRecentFilesList ;
        end ;

     // Restart camera (to ensure image capture and timing remain synchronised due to delays in creating file)
     if FormExists( 'RecordFrm' ) then RecordFrm.RestartCamera ;

     end;


procedure TMainFrm.ApplicationEventsException(Sender: TObject; E: Exception);
begin
      ShowMessage(E.ClassName+' error raised, with message : '+E.Message);
end;

procedure TMainFrm.CloseWindows ;
// ----------------------------------------
// Close all windows (except record window)
// ----------------------------------------
var
     i : Integer ;
begin

    for i := 0 to MDIChildCount-1 do
     if (MDIChildren[i].Name <> 'RecordFrm') and
        (MDIChildren[i].Name <> 'RecADCOnlyFrm') and
        (MDIChildren[i].Name <> 'RecPlotFrm') and
        (MDIChildren[i].Name <> 'ViewPlotFrm') then MDIChildren[i].Close ;

    Application.ProcessMessages ;

    end ;


procedure TMainFrm.FileOpen1Execute(Sender: TObject);
// -------------------------------
// Open an existing .IDR data file
// -------------------------------
begin

     // Close existing file
     mnCloseFile.Click ;

     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := DataFileExtension ;

     // Open last used data directory
     if DirectoryExists(DataDirectory) and (DataDirectory <> '') then begin
        OpenDialog.InitialDir := DataDirectory ;
        end
     else OpenDialog.InitialDir := DefaultDataDirectory ;

     OpenDialog.Filter := format(' %s Files (*.%s)|*.%s',
                          [DataFileExtension,DataFileExtension,DataFileExtension]);
     OpenDialog.Title := 'Open Data File ' ;

     if OpenDialog.execute then begin
        SaveDialog.FileName := ChangeFileExt( OpenDialog.FileName, '.'+DataFileExtension ) ;
        IDRFile.OpenFile( OpenDialog.FileName ) ;
        DataDirectory := ExtractFilePath(OpenDialog.FileName) ;
        LogFrm.AddLine( 'File opened: ' + OpenDialog.FileName );
        // Display in program title bar
        Caption := 'WinFluor : ' + IDRFile.FileName ;
        if IDRFile.NumFrames > 0 then begin
           UpdateRecentFilesList ;
           mnViewImages.Click ;
           end ;
        end ;

     end;


procedure TMainFrm.UpdateRecentFilesList ;
// ---------------------------
// Update recent files list
// ---------------------------
var
     i : Integer ;
begin


     if RecentFiles[0] <> IDRFile.FileName then begin

        // Shift names up one
        for i := High(RecentFiles) DownTo 1 do RecentFiles[i] := RecentFiles[i-1] ;
        RecentFiles[0] := IDRFile.FileName ;

        { Update list in Files menu }
        mnRecentFileSeparator.Visible := False ;
        SetRecentFileItem( mnRecentFile0, RecentFiles[0] ) ;
        SetRecentFileItem( mnRecentFile1, RecentFiles[1] ) ;
        SetRecentFileItem( mnRecentFile2, RecentFiles[2] ) ;
        SetRecentFileItem( mnRecentFile3, RecentFiles[3] ) ;
        end ;

     end ;


procedure TMainFrm.HelpAbout1Execute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;


procedure TMainFrm.FileExit1Execute(Sender: TObject);
// -------------
// Stop Program
// -------------
begin
   Close;
   end;


procedure TMainFrm.FormClose(Sender: TObject; var Action: TCloseAction);
// ------------------
// Close down program
// ------------------
begin

     CloseWindows ;

     // Close data file
     IDRFile.CloseFile ;

     MainFrm.StatusBar.SimpleText := 'WAIT ...Camera shutting down.' ;

     // Shut down camera
     Cam1.CloseCamera ;

     // Close TCP/IP connection (if it is open)
     Ultima.Disconnect ;

     // Shut down N.I. laboratory interfaces
     LabIO.Close ;

     // Save position/size of form
     SaveFormPosition( Self ) ;

     // Save settings to ini file
     FileIO.SaveInitialisationFile( SettingsDirectory + InitialisationFileName ) ;

     end;


procedure TMainFrm.mnRecordClick(Sender: TObject);
// ----------------------
// Open recording window
// ----------------------
begin

     // If camera type is BioRad use analogue only recording module
     if (CameraType = BioRad) or (CameraType = UltimaLSM) then begin
         // Analogue-only recording module
         if FormExists( 'RecADCOnlyFrm' ) then begin
            if RecADCOnlyFrm.WindowState = wsMinimized then RecADCOnlyFrm.WindowState := wsNormal ;
            RecADCOnlyFrm.BringToFront ;
            RecADCOnlyFrm.SetFocus ;
            end
         else begin
            RecADCOnlyFrm := TRecADCOnlyFrm.Create(Self) ;
            RecADCOnlyFrm.DisplayGrid := mnDisplayGrid.Checked ;
            SetFormPosition( RecADCOnlyFrm,20,20,RecADCOnlyFrm.Width,RecADCOnlyFrm.Height);
          end ;
        end
     else begin
         // Image & analogue recording module
         if FormExists('RecordFrm') then begin
            if RecordFrm.WindowState = wsMinimized then RecordFrm.WindowState := wsNormal ;
            RecordFrm.BringToFront ;
            RecordFrm.SetFocus ;
            end
         else begin
            RecordFrm := TRecordFrm.Create(Self) ;
            SetFormPosition( RecordFrm,20,20,
                             (3*ClientWidth) div 5,
                             ClientHeight - RecordFrm.Top - 40);
            // Update location of plotting form
            if FormExists( 'RecPlotFrm' ) then begin
                RecPlotFrm.DisplayGrid := mnDisplayGrid.Checked ;
                SetFormPosition( RecPlotFrm,
                                 RecordFrm.Left + RecordFrm.Width + 10,RecordFrm.Top,
                                 ClientWidth - RecordFrm.Left - RecordFrm.Width - 20,
                                 RecPlotFrm.Height);
                end ;
            end ;
        end ;

     end;


procedure TMainFrm.SetPalette(
          BitMap : TBitMap ;              // Bitmap to set (IN)
          PaletteType : TPaletteType ) ;  // Type of palette required (IN)
// ------------------------------------------------------
// Set bitmap palette to 8 bit grey or false colour scale
// ------------------------------------------------------
var
  Pal: PLogPalette;
  hpal: HPALETTE;
  i: Integer;
begin

  // Exit if bitmap does not exist
  if BitMap = Nil then Exit ;

  BitMap.PixelFormat := pf8bit ;

  //pal := nil;
  GetMem(pal, sizeof(TLogPalette) + sizeof(TPaletteEntry) * 256);

  try

    // Get existing 10 system colours
    GetSystemPaletteEntries( Canvas.Handle, 0, 10, @(Pal^.palPalEntry)) ;

    // Set remaining 246 as shades of grey
    Pal^.palVersion := $300;
    Pal^.palNumEntries := 256;

    case PaletteType of

       // Grey scale
       PalGrey : Begin
           for i := MinPaletteColor to MaxPaletteColor do begin
               Pal^.palPalEntry[i].peRed := i ;
               Pal^.palPalEntry[i].peGreen := i ;
               Pal^.palPalEntry[i].peBlue := i ;
               end;
           end ;

       // Green scale
       PalGreen : Begin
           for i := MinPaletteColor to MaxPaletteColor do begin
               Pal^.palPalEntry[i].peRed := 0;
               Pal^.palPalEntry[i].peGreen := i;
               Pal^.palPalEntry[i].peBlue := 0 ;
               end;
           end ;

       // Red scale
       PalRed : Begin
           for i := MinPaletteColor to MaxPaletteColor do begin
               Pal^.palPalEntry[i].peRed := i;
               Pal^.palPalEntry[i].peGreen := 0;
               Pal^.palPalEntry[i].peBlue := 0 ;
               end;
           end ;

       // Blue scale
       PalBlue : Begin
           for i := MinPaletteColor to MaxPaletteColor do begin
               Pal^.palPalEntry[i].peRed := 0;
               Pal^.palPalEntry[i].peGreen := 0;
               Pal^.palPalEntry[i].peBlue := i ;
               end;
           end ;

       // False colour
       PalFalseColor : begin
           for i := MinPaletteColor to MaxPaletteColor do begin
               if i <= 63 then begin
                  Pal^.palPalEntry[i].peRed := 0 ;
                  Pal^.palPalEntry[i].peGreen := 254 - 4*i ;
                  Pal^.palPalEntry[i].peBlue := 255 ;
                  end
               else if i <= 127 then begin
                  Pal^.palPalEntry[i].peRed := 0 ;
                  Pal^.palPalEntry[i].peGreen := 4*i - 254 ;
                  Pal^.palPalEntry[i].peBlue := 510 - 4*i ;
                  end
               else if i <= 191 then begin
                  Pal^.palPalEntry[i].peRed := 4*i - 510 ;
                  Pal^.palPalEntry[i].peGreen := 255 ;
                  Pal^.palPalEntry[i].peBlue := 0 ;
                  end
               else begin
                  Pal^.palPalEntry[i].peRed := 255 ;
                  Pal^.palPalEntry[i].peGreen := 1022 - 4*i ;
                  Pal^.palPalEntry[i].peBlue := 0 ;
                  end ;
               end;
           // Overload colour = white
           //i := MaxPaletteColor ;
           end ;
       end ;

    // Zero entry set to black for all palettes
    i := MinPaletteColor ;
    Pal^.palPalEntry[i].peRed := 0 ;
    Pal^.palPalEntry[i].peGreen := 0 ;
    Pal^.palPalEntry[i].peBlue := 0 ;

    hpal := CreatePalette(Pal^);
    if hpal <> 0 then Bitmap.Palette := hpal;

  finally
    FreeMem(Pal);
    end;
  end ;


procedure TMainFrm.UpdateLUT(
          iFrameType : Integer ;
          GreyMax : Integer ) ;
// ----------------------------
// Create display look-up table
// ----------------------------
var
    y : Integer ;
    i,j : Integer ;
    GreyScale : Single ;
begin

     if GreyHi[iFrameType] <> GreyLo[iFrameType] then
        GreyScale := (MaxPaletteColor - MinPaletteColor)/ (GreyHi[iFrameType] - GreyLo[iFrameType])
     else GreyScale := 1.0 ;

     j := iFrameType*LUTSize ;
     for i := 0 to LUTSize-1 do begin
         y := MinPaletteColor + Round((i-GreyLo[iFrameType])*GreyScale) ;
         if y < MinPaletteColor then y := MinPaletteColor ;
         if y > MaxPaletteColor then y := MaxPaletteColor ;
         LUTs[j] := y ;
         Inc(j) ;
         end ;

     end ;


procedure TMainFrm.mnViewImagesClick(Sender: TObject);
// --------------------------
// Open image viewing module
// --------------------------
begin

      if (not IDRFile.Open) or (IDRFile.NumFrames < 0) then Exit ;

      if IDRFile.LineScan then begin
         // Line scan file
         if FormExists('ViewLineFrm') then begin
            if ViewLineFrm.WindowState = wsMinimized then ViewLineFrm.WindowState := wsNormal ;
            ViewLineFrm.BringToFront ;
            ViewLineFrm.SetFocus ;
            end
         else begin
            ViewLineFrm := TViewLineFrm.Create(Self) ;
            ViewLineFrm.DisplayGrid := mnDisplayGrid.Checked ;
            SetFormPosition( ViewLineFrm,10,10,ViewLineFrm.Width,ViewLineFrm.Height);
            end ;
         end
      else begin
         // image file
         if FormExists('ViewFrm') then begin
            if ViewFrm.WindowState = wsMinimized then ViewFrm.WindowState := wsNormal ;
            ViewFrm.BringToFront ;
            ViewFrm.SetFocus ;
            end
         else begin
            ViewFrm := TViewFrm.Create(Self) ;
            SetFormPosition( ViewFrm,10,10,
                             (3*ClientWidth) div 5,
                             ClientHeight - ViewFrm.Top - 40);
            // Update location of plotting form
            if FormExists( 'ViewPlotFrm' ) then begin
               ViewPlotFrm.DisplayGrid := mnDisplayGrid.Checked ;
               SetFormPosition( ViewPlotFrm,
                                ViewFrm.Left + ViewFrm.Width + 10,
                                ViewFrm.Top,
                                ClientWidth - ViewFrm.Left - ViewFrm.Width - 20,
                                ViewPlotFrm.Height);
               end ;
            end ;

         end ;

      end;


procedure TMainFrm.mnTimeCourseClick(Sender: TObject);
//  -------------------------------
// Open time course analysis module
// -------------------------------
begin

    if FormExists('TimeCourseFrm') then begin
       if TimeCourseFrm.WindowState = wsMinimized then TimeCourseFrm.WindowState := wsNormal ;
       TimeCourseFrm.BringToFront ;
       TimeCourseFrm.SetFocus ;
       end
    else begin
       TimeCourseFrm := TTimeCourseFrm.Create(Self) ;
       SetFormPosition( TimeCourseFrm,10,10,TimeCourseFrm.Width,TimeCourseFrm.Height);
       end ;

    end ;


procedure TMainFrm.mnCameraSetupClick(Sender: TObject);
//  ---------------------------------
// Open recording settings setup box
// ---------------------------------
begin
    if FormExists( 'SetupFrm' ) then SetupFrm.SetFocus
    else begin
       SetupFrm := TSetupFrm.Create(Self) ;
       SetFormPosition( SetupFrm,10,10,SetupFrm.Width,SetupFrm.Height);
       end ;
    end ;


procedure TMainFrm.mnHistogramClick(Sender: TObject);
// -------------------------------------
// Open image intensity histogram window
// -------------------------------------
begin

   if FormExists('HistogramFrm') then begin
       if HistogramFrm.WindowState = wsMinimized then HistogramFrm.WindowState := wsNormal ;
       HistogramFrm.BringToFront ;
       HistogramFrm.SetFocus ;
       end
    else begin
       HistogramFrm := THistogramFrm.Create(Self) ;
       SetFormPosition( HistogramFrm,10,10,HistogramFrm.Width,HistogramFrm.Height);
       end ;

    end ;


procedure TMainFrm.Edit1Click(Sender: TObject);
// ----------------------------------------------------
// Enable copy & print functions if source is available
// ----------------------------------------------------
var
     PlotAvailable : Boolean ;
     ImageAvailable : Boolean ;
begin

     PlotAvailable := False ;
     ImageAvailable := False ;
     if MDIChildCount > 0 then begin
        if ActiveMDIChild.Name = 'HistogramFrm' then begin
           PlotAvailable := THistogramFrm(ActiveMDIChild).PlotAvailable ;
           end
        else if ActiveMDIChild.Name = 'TimeCourseFrm' then begin
           PlotAvailable := TTimeCourseFrm(ActiveMDIChild).PlotAvailable ;
           end
        else if ActiveMDIChild.Name = 'LineProfileFrm' then begin
           PlotAvailable := TLineProfileFrm(ActiveMDIChild).PlotAvailable ;
           end
        else if ActiveMDIChild.Name = 'ViewFrm' then begin
           ImageAvailable := TViewFrm(ActiveMDIChild).ImageAvailable ;
           end
        else if ActiveMDIChild.Name = 'ViewPlotFrm' then begin
           PlotAvailable := TViewPlotFrm(ActiveMDIChild).PlotAvailable ;
           end
        else if ActiveMDIChild.Name = 'EventAnalysisFrm' then begin
           PlotAvailable := TEventAnalysisFrm(ActiveMDIChild).PlotAvailable ;
           end
        else if ActiveMDIChild.Name = 'SpectrumFrm' then begin
           PlotAvailable := TSpectrumFrm(ActiveMDIChild).PlotAvailable ;
           end
        else if ActiveMDIChild.Name = 'ViewLineFrm' then begin
           PlotAvailable := TViewLineFrm(ActiveMDIChild).PlotAvailable ;
           ImageAvailable := TViewLineFrm(ActiveMDIChild).ImageAvailable ;
           end
        else if ActiveMDIChild.Name = 'SnapFrm' then begin
           ImageAvailable := TSnapFrm(ActiveMDIChild).ImageAvailable ;
           end
        else if ActiveMDIChild.Name = 'SmoothDifferentiateFrm' then begin
           PlotAvailable := TSmoothDifferentiateFrm(ActiveMDIChild).PlotAvailable ;
           end ;

        end ;

     mnCopyPlotData.Enabled := PlotAvailable ;
     mnCopyPlotImage.Enabled := PlotAvailable ;
     mnCopyImage.Enabled := ImageAvailable ;

     end;


procedure TMainFrm.mnCopyPlotDataClick(Sender: TObject);
// ------------------------------------
// Copy data points from graph or table
// ------------------------------------
begin

     if ActiveMDIChild.Name = 'HistogramFrm' then
        THistogramFrm(ActiveMDIChild).CopyDataToClipboard
     else if ActiveMDIChild.Name = 'TimeCourseFrm' then
        TTimeCourseFrm(ActiveMDIChild).CopyDataToClipboard
     else if ActiveMDIChild.Name = 'LineProfileFrm' then
        TLineProfileFrm(ActiveMDIChild).CopyDataToClipboard
     else if ActiveMDIChild.Name = 'ViewPlotFrm' then
        TViewPlotFrm(ActiveMDIChild).CopyPlotDataToClipboard
     else if ActiveMDIChild.Name = 'ViewLineFrm' then
        TViewLineFrm(ActiveMDIChild).CopyPlotDataToClipboard
     else if ActiveMDIChild.Name = 'SpectrumFrm' then
        TSpectrumFrm(ActiveMDIChild).CopyDataToClipboard
     else if ActiveMDIChild.Name = 'EventAnalysisFrm' then
        TEventAnalysisFrm(ActiveMDIChild).CopyDataToClipboard
     else if ActiveMDIChild.Name = 'SmoothDifferentiateFrm' then
        TSmoothDifferentiateFrm(ActiveMDIChild).CopyDataToClipboard ;

     end;


procedure TMainFrm.mnCopyImageClick(Sender: TObject);
// ------------------------------------
// Copy image or graph to clipboard
// ------------------------------------
begin

     if ActiveMDIChild.Name = 'ViewFrm' then
        TViewFrm(ActiveMDIChild).CopyImageToClipboard
     else if ActiveMDIChild.Name = 'ViewLineFrm' then
        TViewLineFrm(ActiveMDIChild).CopyImageToClipboard
     else if ActiveMDIChild.Name = 'SnapFrm' then
        TSnapFrm(ActiveMDIChild).CopyImageToClipboard ;

     end;


procedure TMainFrm.mnSaveAsTIFFClick(Sender: TObject);
// ----------------------------------------
// Save currently active image to TIFF file
// ----------------------------------------
begin

     FileIO.SaveFrameAsTIFF ;

     end;


procedure TMainFrm.mnSaveToTIFFClick(Sender: TObject);
//  -----------------------------------------
// Enable/disable image copying and printing
// -----------------------------------------
var
    ImageAvailable : Boolean ;
begin

     // Any images available for writing ?
     ImageAvailable := False ;
     if MDIChildCount > 0 then begin
        if ActiveMDIChild.Name = 'RecordFrm' then
           if RecordFrm.ImageAvailable then ImageAvailable := True ;
        end ;

     mnSaveAsTiff.Enabled := ImageAvailable ;

     end;


procedure TMainFrm.File1Click(Sender: TObject);
//  -----------------------------------------
// Enable/disable image copying and printing
// -----------------------------------------
var
    ImageAvailable,PlotAvailable,FramesAvailable : Boolean ;
    i : Integer ;
begin

     ImageAvailable := False ;
     PlotAvailable := False ;
     mnCreateAVI.Enabled := False ;

     if MDIChildCount > 0 then begin

        // Any images available for writing ?
        if ActiveMDIChild.Name = 'RecordFrm' then
           ImageAvailable := RecordFrm.ImageAvailable
        else if ActiveMDIChild.Name = 'ViewFrm' then
           ImageAvailable := ViewFrm.ImageAvailable
        else if ActiveMDIChild.Name = 'ViewLineFrm' then
           ImageAvailable := ViewLineFrm.ImageAvailable ;

        // Any plots available for printing
        if ActiveMDIChild.Name = 'HistogramFrm' then
           PlotAvailable := HistogramFrm.PlotAvailable
        else if ActiveMDIChild.Name = 'TimeCourseFrm' then
           PlotAvailable := TimeCourseFrm.PlotAvailable
        else if ActiveMDIChild.Name = 'LineProfileFrm' then
           PlotAvailable := LineProfileFrm.PlotAvailable
        else if ActiveMDIChild.Name = 'ViewPlotFrm' then
           PlotAvailable := ViewPlotFrm.PlotAvailable
        else if ActiveMDIChild.Name = 'ViewLineFrm' then
           PlotAvailable := ViewLineFrm.PlotAvailable
        else if ActiveMDIChild.Name = 'EventAnalysisFrm' then
           PlotAvailable := EventAnalysisFrm.PlotAvailable ;

        // Enable AVI creation if view form open
        mnCreateAVI.Enabled := False ;
        for i := 0 to MDIChildCount-1 do
            if MDIChildren[I].Name = 'ViewFrm' then mnCreateAVI.Enabled := True ;

        end ;

     // Enable/disable menu items
     mnSaveAsTiff.Enabled := ImageAvailable ;
     mnPrintImage.Enabled := ImageAvailable ;
     mnPrintGraph.Enabled := PlotAvailable ;

     if (IDRFile.Open) and (IDRFile.NumFrames > 0) then FramesAvailable := True
                                                   else FramesAvailable := False ;

     mnProperties.Enabled := FramesAvailable ;
     mnExport.Enabled := FramesAvailable ;
     mnSaveAs.Enabled := FramesAvailable ;
     mnCreateAVI.Enabled := FramesAvailable ;

     end;


procedure TMainFrm.mnPrintGraphClick(Sender: TObject);
// ---------------------
// Print image or graph
// ---------------------
begin

     if ActiveMDIChild.Name = 'HistogramFrm' then begin
        THistogramFrm(ActiveMDIChild).Print ;
        end
     else if ActiveMDIChild.Name = 'TimeCourseFrm' then begin
        TTimeCourseFrm(ActiveMDIChild).Print ;
        end 
     else if ActiveMDIChild.Name = 'LineProfileFrm' then begin
        TLineProfileFrm(ActiveMDIChild).Print ;
        end
     else if ActiveMDIChild.Name = 'ViewPlotFrm' then begin
        TViewPlotFrm(ActiveMDIChild).PrintPlot ;
        end 
     else if ActiveMDIChild.Name = 'ViewLineFrm' then begin
        TViewLineFrm(ActiveMDIChild).PrintPlot ;
        end
     else if ActiveMDIChild.Name = 'SpectrumFrm' then begin
        TSpectrumFrm(ActiveMDIChild).Print ;
        end
     else if ActiveMDIChild.Name = 'EventAnalysisFrm' then begin
        TEventAnalysisFrm(ActiveMDIChild).Print ;
        end ;

     end;


procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
// --------------------------------------------
// Give user a chance not to close down program
// --------------------------------------------
begin
     if MessageDlg( 'Exit Program! Are you Sure? ', mtConfirmation,
        [mbYes,mbNo], 0 ) = mrYes then CanClose := True
                                  else CanClose := False ;
     end;


procedure TMainFrm.FormCreate(Sender: TObject);
// ------------------------------------
// Initialisations when form is created
// ------------------------------------
var
    i : Integer ;
begin

    // Create program folder path
    ProgramDirectory := ExtractFilePath(ParamStr(0)) ;

     // Create settings directory path
     SettingsDirectory := GetSpecialFolder(CSIDL_COMMON_DOCUMENTS) + '\WinFluor\';
     if not SysUtils.DirectoryExists(SettingsDirectory) then begin
        if not SysUtils.ForceDirectories(SettingsDirectory) then
           ShowMessage( 'Unable to create settings folder' + SettingsDirectory) ;
        end ;
     SettingsFileName := SettingsDirectory + 'winfluor.ini' ;

     // Help file
     Application.HelpFile := ProgramDirectory + 'Winfluor.chm';

     DataDirectory := SettingsDirectory + 'Data\' ;
     if not SysUtils.DirectoryExists(DataDirectory) then begin
         if not SysUtils.ForceDirectories(DataDirectory) then
            ShowMessage( 'Unable to create protocols folder' + DataDirectory) ;
         end ;

     VProtDirectory :=  SettingsDirectory + 'vprot\' ;
     if not SysUtils.DirectoryExists(VProtDirectory) then begin
         if not SysUtils.ForceDirectories(VProtDirectory) then
            ShowMessage( 'Unable to create protocols folder' + VProtDirectory) ;
         end ;

     PProtDirectory :=  SettingsDirectory + 'pprot\' ;
     if not SysUtils.DirectoryExists(PProtDirectory) then begin
         if not SysUtils.ForceDirectories(PProtDirectory) then
            ShowMessage( 'Unable to create protocols folder' + PProtDirectory) ;
         end ;

     // Initialise form positions
     for i := 0 to High(FormPos) do begin
         FormPos[i].Left := 0 ;
         FormPos[i].Top := 0 ;
         FormPos[i].Width := 0 ;
         FormPos[i].Height := 0 ;
         end;

     end;


procedure TMainFrm.mnStimSetupClick(Sender: TObject);
//  ---------------------------------
// Open stimulus settings setup box
// ---------------------------------
begin

    if FormExists( 'WavGenFrm' ) then begin
       if WavGenFrm.WindowState = wsMinimized then WavGenFrm.WindowState := wsNormal ;
       WavGenFrm.BringToFront ;
       WavGenFrm.SetFocus ;
       end
    else begin
       WavGenFrm := TWavGenFrm.Create(Self) ;
       SetFormPosition( WavGenFrm,40,40,WavGenFrm.Width,WavGenFrm.Height);
       end ;

    end ;


procedure TMainFrm.mnEXCSetupClick(Sender: TObject);
//  --------------------------------------------
// Open excitation wavelength settings setup box
// ---------------------------------------------
begin
    if FormExists( 'ExcSetupFrm' ) then ExcSetupFrm.SetFocus
    else begin
       ExcSetupFrm := TExcSetupFrm.Create(Self) ;
       SetFormPosition( ExcSetupFrm,10,10,ExcSetupFrm.Width,ExcSetupFrm.Height);
       end ;
    end ;


procedure TMainFrm.mnSealTestClick(Sender: TObject);
// -----------------------------
// Run pipette seal test/monitor
// -----------------------------
begin

    if FormExists( 'SealTestFrm' ) then begin
       if SealTestFrm.WindowState = wsMinimized then SealTestFrm.WindowState := wsNormal ;
       SealTestFrm.BringToFront ;
       SealTestFrm.SetFocus ;
       end
    else begin
       SealTestFrm := TSealTestFrm.Create(Self) ;
       SetFormPosition( SealTestFrm,20,20,SealTestFrm.Width,SealTestFrm.Height);
       end ;
    end ;


procedure TMainFrm.mnPrintSetupClick(Sender: TObject);
{ - Menu Item ------------------
  Standard printer setup dialog
  ------------------------------}
begin
     PrinterSetupDialog.Execute ;
     end;

procedure TMainFrm.SetRecentFileItem(
          MenuItem : TMenuItem ;    { Menu item to be updated }
          FileName : string         { Name of data file to be added }
          ) ;
{ ------------------------------------------------------
  Add the name of a recently used data file to File menu
  ------------------------------------------------------ }
begin
     if FileName <> '' then begin
        mnRecentFileSeparator.Visible := True ;
        MenuItem.Caption := FileName ;
        MenuItem.Visible := True ;
        end
     Else MenuItem.Visible := False ;
     end ;



procedure TMainFrm.mnRecentFile0Click(Sender: TObject);
{ - Menu Item -------------------
  Load a recently used data file
  ------------------------------- }
var
   FileName : string ;
begin
     { Get file name. Note, the .Tag property of the menu item was set
       at design time to point to the appropriate RecentFiles array item }
     FileName := RecentFiles[TMenuItem(Sender).Tag] ;
     if (FileName <> '') and FileExists(FileName) then begin
        if (FileGetAttr(FileName) AND faReadOnly) = 0 then begin
           // Close any existing file
           mnCloseFile.Click ;
           // Open selected file
           IDRFile.OpenFile( FileName ) ;
           LogFrm.AddLine( 'File opened: ' + FileName );
           // Display in program title bar
           Caption := 'WinFluor : ' + IDRFile.FileName ;
           if IDRFile.NumFrames > 0 then mnViewImages.Click ;
           end
        else
           ShowMessage( FileName + ' is READ-ONLY. Unable to open!' ) ;
        end ;
     end;


procedure TMainFrm.mnCreateAVIClick(Sender: TObject);
// -------------------------
// Create an AVI movie file
// -------------------------
begin
      if FormExists( 'AVIFrm' ) then AVIFrm.SetFocus
      else begin
         AVIFrm := TAVIFrm.Create(Self) ;
         SetFormPosition( AVIFrm,10,10,AVIFrm.Width,AVIFrm.Height);
         end ;
      end;


procedure TMainFrm.mnImportClick(Sender: TObject);
// ------------------------------------------
// Import images from an external file format
// ------------------------------------------
begin

     // Close existing file
     mnCloseFile.Click ;

     // Import file
     FileIO.ImportFile ;

     end;


procedure TMainFrm.mnExportClick(Sender: TObject);
// --------------
// Export options
// --------------
begin
     if IDRFile.ADCNumScansInFile > 0 then mnExportAnalogue.Enabled := True
                                      else mnExportAnalogue.Enabled := False ;
     end;


procedure TMainFrm.mnAnalysisClick(Sender: TObject);
// --------------------------------------------------
// Enable/disable options when Analysis menu selected
// --------------------------------------------------
var
    i : Integer ;
begin

    mnHistogram.Enabled := False ;
    mnTimeCourse.Enabled := False ;
    mnEventAnalysis.Enabled := False ;
    mnLineProfile.Enabled := False ;
    mnAverage.Enabled := False ;
    mnComputedImage.Enabled := False ;
    mnSpectrumAnalysis.Enabled := False ;
    mnSmoothDifferentiate.Enabled := False ;
    mnDigitalFilter.Enabled := False ;

    for i := 0 to Mainfrm.MDIChildCount-1 do begin
        if MainFrm.MDIChildren[i].Name = 'RecordFrm' then begin
           if TRecordFrm(MainFrm.MDIChildren[i]).ImageAvailable then begin
              mnHistogram.Enabled := True ;
              end ;
           end
        else if MainFrm.MDIChildren[i].Name = 'SnapFrm' then begin
           if TSnapFrm(MainFrm.MDIChildren[i]).ImageAvailable then begin
              mnHistogram.Enabled := True ;
              end ;
           end
        else if LeftStr(MainFrm.MDIChildren[i].Name,7) = 'ViewFrm' then begin
           if TViewFrm(MainFrm.MDIChildren[i]).ImageAvailable then begin
              mnHistogram.Enabled := True ;
              mnTimeCourse.Enabled := True ;
              mnEventAnalysis.Enabled := True ;
              mnLineProfile.Enabled := True ;
              mnAverage.Enabled := True ;
              mnComputedImage.Enabled := True ;
              mnSpectrumAnalysis.Enabled := True ;
              mnDigitalFilter.Enabled := True ;
              end ;
           end
        else if LeftStr(MainFrm.MDIChildren[i].Name,11) = 'ViewLineFrm' then begin
           if TViewLineFrm(MainFrm.MDIChildren[i]).ImageAvailable then begin
              mnHistogram.Enabled := True ;
              mnTimeCourse.Enabled := True ;
              mnEventAnalysis.Enabled := True ;
              mnSmoothDifferentiate.Enabled := True ;
              end ;
           end ;
        end ;

    end;

procedure TMainFrm.mnAverageClick(Sender: TObject);
// --------------------------
// Open frame averaging form
// --------------------------
begin

   if FormExists('AverageFrm') then begin
       if AverageFrm.WindowState = wsMinimized then AverageFrm.WindowState := wsNormal ;
       AverageFrm.BringToFront ;
       AverageFrm.SetFocus ;
       end
    else begin
       AverageFrm := TAverageFrm.Create(Self) ;
       SetFormPosition( AverageFrm,20,20,AverageFrm.Width,AverageFrm.Height);
       end ;

    end ;


procedure TMainFrm.mnLineProfileClick(Sender: TObject);
// -------------------------
// Open line profile window
// --------------------------
begin

   if FormExists('LineProfileFrm') then begin
       if LineProfileFrm.WindowState = wsMinimized then LineProfileFrm.WindowState := wsNormal ;
       LineProfileFrm.BringToFront ;
       LineProfileFrm.SetFocus ;
       end
    else begin
       LineProfileFrm := TLineProfileFrm.Create(Self) ;
       SetFormPosition( LineProfileFrm,40,40,LineProfileFrm.Width,LineProfileFrm.Height);
       end ;

    end ;


procedure TMainFrm.mnComputedImageFrmClick(Sender: TObject);
// ----------------------------------
// Open computed image creation form
// ----------------------------------
begin

   if FormExists('RatioFrm') then begin
       if RatioFrm.WindowState = wsMinimized then RatioFrm.WindowState := wsNormal ;
       RatioFrm.BringToFront ;
       RatioFrm.SetFocus ;
       end
    else begin
       RatioFrm := TRatioFrm.Create(Self) ;
       SetFormPosition( RatioFrm,20,20,RatioFrm.Width,RatioFrm.Height);
       end ;

    end ;


procedure TMainFrm.mnSetupIonClick(Sender: TObject);
// -----------------------------------
// Open ion binding equation setup box
// ------------------------------------
begin
    if FormExists( 'SetupIonFrm' ) then SetupIonFrm.SetFocus
    else begin
       SetupIonFrm := TSetupIonFrm.Create(Self) ;
       SetFormPosition( SetupIonFrm,10,10,SetupIonFrm.Width,SetupIonFrm.Height);
       end ;
    end ;


procedure TMainFrm.mnRecordMenuClick(Sender: TObject);
// -----------
// Record menu
// -----------
begin

     mnRecord.Enabled := True ;
     mnSealTest.Enabled := True ;

     if (CameraType = NoCamera8) or (CameraType = NoCamera16) then begin
        mnRecord.Enabled := False ;
        end ;

     end;


procedure TMainFrm.mnCopyPlotImageClick(Sender: TObject);
// ------------------------------------
// Copy graph plot image toclipboard
// ------------------------------------
begin

     if ActiveMDIChild.Name = 'HistogramFrm' then
        THistogramFrm(ActiveMDIChild).CopyImageToClipboard
     else if ActiveMDIChild.Name = 'TimeCourseFrm' then
        TTimeCourseFrm(ActiveMDIChild).CopyImageToClipboard
     else if ActiveMDIChild.Name = 'LineProfileFrm' then
        TLineProfileFrm(ActiveMDIChild).CopyImageToClipboard
     else if ActiveMDIChild.Name = 'ViewPlotFrm' then
        TViewPlotFrm(ActiveMDIChild).CopyPlotImageToClipboard
     else if ActiveMDIChild.Name = 'ViewLineFrm' then
        TViewLineFrm(ActiveMDIChild).CopyPlotImageToClipboard
     else if ActiveMDIChild.Name = 'SpectrumFrm' then
        TSpectrumFrm(ActiveMDIChild).CopyImageToClipboard
     else if ActiveMDIChild.Name = 'EventAnalysisFrm' then
        TEventAnalysisFrm(ActiveMDIChild).CopyImageToClipboard
     else if ActiveMDIChild.Name = 'SmoothDifferentiateFrm' then
        TSmoothDifferentiateFrm(ActiveMDIChild).CopyPlotImageToClipboard ;

     end;


procedure TMainFrm.mnPrintImageClick(Sender: TObject);
// ------------------
// Print bitmap image
// ------------------
begin
     if ActiveMDIChild.Name = 'ViewFrm' then
        TViewFrm(ActiveMDIChild).PrintImage
     else if ActiveMDIChild.Name = 'ViewLineFrm' then
        TViewLineFrm(ActiveMDIChild).PrintImage ;

     end;


procedure TMainFrm.mnRecordADCOnlyClick(Sender: TObject);
// -------------------------------
// Open A/D only recording window
// ------------------------------
begin

    if FormExists( 'RecADCOnlyFrm' ) then RecADCOnlyFrm.SetFocus
    else begin
       RecADCOnlyFrm := TRecADCOnlyFrm.Create(Self) ;
       SetFormPosition( RecADCOnlyFrm,20,20,RecADCOnlyFrm.Width,RecADCOnlyFrm.Height);
       end ;
    end ;


procedure TMainFrm.mnExportImagesClick(Sender: TObject);
// -----------------------------
// Export images to another file
// -----------------------------
begin
     ExportImagesFrm.Left := Left + 40 ;
     ExportImagesFrm.Top := Top + 50 ;
     ExportImagesFrm.ShowModal ;
     end;


procedure TMainFrm.mnExportAnalogueClick(Sender: TObject);
// -----------------------------------------
// Export analogue signals in another format
// -----------------------------------------
begin
     ExportAnalogueFrm.Left := Left + 40 ;
     ExportAnalogueFrm.Top := Top + 50 ;
     ExportAnalogueFrm.ShowModal ;
     end;


procedure TMainFrm.mnViewLineScansClick(Sender: TObject);
// ------------------------------
// Open line scan viewing module
// ------------------------------
begin
      if (IDRFile.Open) and (IDRFile.NumFrames > 0) then
         ViewLineFrm := TViewLineFrm.Create(Self) ;
      end;


procedure TMainFrm.mnXYStageClick(Sender: TObject);
// ------------------------------
// Display XY stage control panel
// ------------------------------
begin
     XYStageFrm.Left := Left + 40 ;
     XYStageFrm.Top := Top + 50 ;
     XYStageFrm.Show ;
     end;


procedure TMainFrm.mnViewClick(Sender: TObject);
// --------------------------------
// Enable/disable View menu options
// --------------------------------
begin

     mnViewImages.Enabled := not Recording ;
     if FormExists('RecordFrm') then begin
           if RecordFrm.TimeLapseMode then mnViewImages.Enabled := True ;
        end ;
     if MainFrm.IDRFile.NumFrames <= 0 then mnViewImages.Enabled := False ;

     end;

procedure TMainFrm.SetZoomChannel(
          MenuItem : TMenuItem ;
          ChanNum : Integer ;
          NumChannels : Integer ;
          const ADCChannel : TChannel
          ) ;
begin
     if ChanNum < NumChannels then begin
        MenuItem.Visible := True ;
        MenuItem.Caption := ADCChannel.ADCName ;
        MenuItem.Tag := ChanNum ;
        end
     else begin
        MenuItem.Visible := False ;
        end ;
     end ;

procedure TMainFrm.mnLoadSettingsClick(Sender: TObject);
// ----------------------------------
// Load system configuration settings
// ----------------------------------
var
    i : Integer ;
begin

     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := ConfigFileExtension ;
     OpenDialog.InitialDir := SettingsDirectory ;

     OpenDialog.Filter := format(' Settings Files (*.%s)|*.%s',
                          [ConfigFileExtension,ConfigFileExtension]);
     OpenDialog.Title := 'Load Settings' ;

     if not OpenDialog.execute then Exit ;

     // Close camera
     MainFrm.StatusBar.SimpleText := ' WAIT: Initialising camera ... ' ;
     MainFrm.Cam1.CloseCamera ;

     FileIO.LoadInitialisationFile( OpenDialog.FileName ) ;

     // Open new camera
     MainFrm.Cam1.OpenCamera( MainFrm.CameraType ) ;

     // Initialise look-up tables
     for i := 0 to MaxFrameType do begin
         MainFrm.GreyLo[i] := 0 ;
         MainFrm.GreyHi[i] := MainFrm.Cam1.GreyLevelMax ;
         MainFrm.UpdateLUT( i, MainFrm.Cam1.GreyLevelMax ) ;
         end ;

     MainFrm.StatusBar.SimpleText := ' Camera Initialised ' ;

     end;

procedure TMainFrm.mnSaveSettingsClick(Sender: TObject);
// --------------------------------------------
// Save hardware configuration settings to file
// --------------------------------------------
begin
     // Present user with standard Save File dialog box
     SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.DefaultExt := ConfigFileExtension ;
     SaveDialog.InitialDir := SettingsDirectory ;
     SaveDialog.Filter := format(' Settings Files (*.%s)|*.%s',
                          [ConfigFileExtension,ConfigFileExtension]);
     SaveDialog.Title := 'Save Settings' ;

     if SaveDialog.execute then FileIO.SaveInitialisationFile( SaveDialog.FileName ) ;

     end;

procedure TMainFrm.mnContentsClick(Sender: TObject);
// ---------------------
// Display help contents
// ---------------------
begin
     application.helpcontext( 10 ) ;
     end;


procedure TMainFrm.SetupClick(Sender: TObject);
// ----------
// Setup menu
// ----------
var
    i : Integer ;
begin

     // Stimulus waveform editor
     mnStimSetup.Enabled := True ;
     for i := 0 to MDIChildCount-1 do if MDIChildren[i].Name = 'WavGenFrm' then mnStimSetup.Enabled := False ;

     if XYStageFrm.StageType <> xyNone then mnXYStage.Enabled := True
                                       else mnXYStage.Enabled := False ;
     end;


function TMainFrm.IOResourceAvailable(
          ResourceNum : Integer
          ) : Boolean ;
// ------------------------------------------------
// Return TRUE if Resource # ResourceNum is defined
// ------------------------------------------------
begin

    if (ResourceNum < 0) or (ResourceNum > MaxResources) then Result := False
                                                         else Result := True ;
    end ;


procedure TMainFrm.mnEventAnalysisClick(Sender: TObject);
//  -------------------------------
// Open event analysis module
// -------------------------------
begin

    if FormExists('EventAnalysisFrm') then begin
       if EventAnalysisFrm.WindowState = wsMinimized then EventAnalysisFrm.WindowState := wsNormal ;
       EventAnalysisFrm.BringToFront ;
       EventAnalysisFrm.SetFocus ;
       end
    else begin
       EventAnalysisFrm := TEventAnalysisFrm.Create(Self) ;
       EventAnalysisFrm.Left := 10 ;
       EventAnalysisFrm.Top := 10 ;
       end ;
    EventAnalysisFrm.DisplayGrid := mnDisplayGrid.Checked ;

    end ;


procedure TMainFrm.mnPropertiesClick(Sender: TObject);
// -----------------------
// Display file properties
// -----------------------
begin

    if FormExists('FilePropsFrm') then begin
       if FilePropsFrm.WindowState = wsMinimized then FilePropsFrm.WindowState := wsNormal ;
       FilePropsFrm.BringToFront ;
       FilePropsFrm.SetFocus ;
       end
    else begin
       FilePropsFrm := TFilePropsFrm.Create(Self) ;
       SetFormPosition( FilePropsFrm,10,10,FilePropsFrm.Width,FilePropsFrm.Height);
       end ;

     end;

procedure TMainFrm.mnSaveAsClick(Sender: TObject);
// --------------------------------------------
// Save all or part of data to another IDR file
// --------------------------------------------
begin
     SaveAsFileFrm.ShowModal ;
     end;


procedure TMainFrm.mnCloseFileClick(Sender: TObject);
// --------------------
// Close IDR data file
// --------------------
begin

     // Close all windows (except record window)
     CloseWindows ;

     // Close existing data files
     IDRFile.CloseFile ;

     Caption := 'WinFluor : ' ;

     end;


procedure TMainFrm.mnStimulusDefaultSettingsClick(Sender: TObject);
//  --------------------------------------
// Open stimulus default outputs setup box
// ---------------------------------------
begin
    if FormExists( 'StimulusDefaultsFrm' ) then StimulusDefaultsFrm.SetFocus
    else begin
       StimulusDefaultsFrm := TStimulusDefaultsFrm.Create(Self) ;
       SetFormPosition( StimulusDefaultsFrm,50,50,StimulusDefaultsFrm.Width,StimulusDefaultsFrm.Height);
       end ;
    end ;


procedure TMainFrm.mnDisplayGridClick(Sender: TObject);
// -------------------------------
// Toggle Display Grid menu option
// -------------------------------
begin

    mnDisplayGrid.Checked := not mnDisplayGrid.Checked  ;

    mnViewImages.Enabled := not Recording ;

    if FormExists( 'RecPlotFrm' )then RecPlotFrm.DisplayGrid := mnDisplayGrid.Checked ;
    if FormExists( 'ViewPlotFrm' )then ViewPlotFrm.DisplayGrid := mnDisplayGrid.Checked ;
    if FormExists( 'ViewLineFrm' )then ViewLineFrm.DisplayGrid := mnDisplayGrid.Checked ;
    if FormExists( 'RecADCOnlyFrm' )then RecADCOnlyFrm.DisplayGrid := mnDisplayGrid.Checked ;
    if FormExists( 'EventAnalysisFrm' )then EventAnalysisFrm.DisplayGrid := mnDisplayGrid.Checked ;
    if FormExists( 'SealTestFrm' )then SealTestFrm.DisplayGrid := mnDisplayGrid.Checked ;

    end;


procedure TMainFrm.mnZoomOutAllClick(Sender: TObject);
// --------------------------------------------------
// Set all time course plots to minimum magnification
// --------------------------------------------------
begin

    if FormExists( 'RecPlotFrm' )then RecPlotFrm.ZoomOutAll ;
    if FormExists( 'ViewPlotFrm' )then ViewPlotFrm.ZoomOutAll ;
    if FormExists( 'ViewLineFrm' )then ViewLineFrm.ZoomOutAll ;
    if FormExists( 'RecADCOnlyFrm' )then RecADCOnlyFrm.ZoomOutAll ;
    if FormExists( 'EventAnalysisFrm' )then EventAnalysisFrm.ZoomOutAll ;
    if FormExists( 'SealTestFrm' )then SealTestFrm.ZoomOutAll ;

    end;

procedure TMainFrm.mnSpectrumAnalysisClick(Sender: TObject);
//  -------------------------------
// Open spectrum analysis module
// -------------------------------
begin

    if FormExists('SpectrumFrm') then begin
       if SpectrumFrm.WindowState = wsMinimized then SpectrumFrm.WindowState := wsNormal ;
       SpectrumFrm.BringToFront ;
       SpectrumFrm.SetFocus ;
       end
    else begin
       SpectrumFrm := TSpectrumFrm.Create(Self) ;
       SpectrumFrm.Left := 10 ;
       SpectrumFrm.Top := 10 ;
       end ;

    end ;

    
function TMainFrm.FormExists( FormName : String ) : Boolean ;
// --------------------------
// Return TRUE if form exists
// --------------------------
var
    i : Integer ;
begin
     Result := False ;
     for i := 0 to MDIChildCount-1 do
         if ANSIContainsText(MDIChildren[i].Name,FormName) then Result := True ;
     end ;


procedure TMainFrm.mnSnapImageClick(Sender: TObject);
// -----------------------------
// Open live image window
// -----------------------------
begin

    if FormExists( 'SnapFrm' ) then begin
       if SnapFrm.WindowState = wsMinimized then SnapFrm.WindowState := wsNormal ;
       SnapFrm.BringToFront ;
       SnapFrm.SetFocus ;
       end
    else begin
       SnapFrm := TSnapFrm.Create(Self) ;
       SetFormPosition( SnapFrm,20,20,
                        ClientWidth - SnapFrm.Left - 40,
                        ClientHeight - SnapFrm.Top - 40);
       end ;

    end ;


procedure TMainFrm.mePhotoStimClick(Sender: TObject);
//  --------------------------------------
// Open photo stimulus settings setup box
// --------------------------------------
begin

    if FormExists( 'PhotoStimFrm' ) then begin
       if PhotoStimFrm.WindowState = wsMinimized then PhotoStimFrm.WindowState := wsNormal ;
       PhotoStimFrm.BringToFront ;
       PhotoStimFrm.SetFocus ;
       end
    else begin
       PhotoStimFrm := TPhotoStimFrm.Create(Self) ;
       SetFormPosition( PhotoStimFrm,20,20,PhotoStimFrm.Width,PhotoStimFrm.Height);
       end ;

    end ;


procedure TMainFrm.mnSmoothDifferentiateClick(Sender: TObject);
// ----------------------------------
// Create smooth differentiation form
// ----------------------------------
begin

    if FormExists( 'SmoothDifferentiateFrm' ) then begin
       if SmoothDifferentiateFrm.WindowState = wsMinimized then SmoothDifferentiateFrm.WindowState := wsNormal ;
       SmoothDifferentiateFrm.BringToFront ;
       SmoothDifferentiateFrm.SetFocus ;
       end
    else begin
       SmoothDifferentiateFrm := TSmoothDifferentiateFrm.Create(Self) ;
       SmoothDifferentiateFrm.Left := 20 ;
       SmoothDifferentiateFrm.Top := 20 ;
       end ;

    end;


procedure TMainFrm.mnCameraSettingsClick(Sender: TObject);
//  -------------------------
// Open camera settings box
// --------------------------
begin

    if FormExists( 'CameraSettingsFrm' ) then begin
       if CameraSettingsFrm.WindowState = wsMinimized then CameraSettingsFrm.WindowState := wsNormal ;
       CameraSettingsFrm.BringToFront ;
       CameraSettingsFrm.SetFocus ;
       end
    else begin
       CameraSettingsFrm := TCameraSettingsFrm.Create(Self) ;
       CameraSettingsFrm.Left := 20 ;
       CameraSettingsFrm.Top := 20 ;
       end ;

    if FormExists( 'CameraSettingsFrm' ) then CameraSettingsFrm.SetFocus
    else begin
       CameraSettingsFrm := TCameraSettingsFrm.Create(Self) ;
       CameraSettingsFrm.Left := 20 ;
       CameraSettingsFrm.Top := 20 ;
       end ;
    end ;

procedure TMainFrm.mnExportROITimeCourseClick(Sender: TObject);
// -----------------------------------------
// Export ROI time course in another format
// -----------------------------------------
begin
     ExportROITimeCourseFrm.Left := Left + 40 ;
     ExportROITimeCourseFrm.Top := Top + 50 ;
     ExportROITimeCourseFrm.ShowModal ;
     end;


procedure TMainFrm.mnDigitalFilterClick(Sender: TObject);
// ----------------------------------
// Open analog filter form
// ----------------------------------
begin

   if FormExists('DigitalFilterFrm') then begin
       if DigitalFilterFrm.WindowState = wsMinimized then DigitalFilterFrm.WindowState := wsNormal ;
       DigitalFilterFrm.BringToFront ;
       DigitalFilterFrm.SetFocus ;
       end
    else begin
       DigitalFilterFrm := TDigitalFilterFrm.Create(Self) ;
       DigitalFilterFrm.Left := 20 ;
       DigitalFilterFrm.Top := 20 ;
       end ;

    end ;


procedure TMainFrm.mnDirectControlClick(Sender: TObject);
//  -----------------------------------------------
// Open voltage outputs direct control form
// ------------------------------------------------
begin

    if FormExists( 'DirectControlFrm' ) then begin
       if DirectControlFrm.WindowState = wsMinimized then DirectControlFrm.WindowState := wsNormal ;
       DirectControlFrm.BringToFront ;
       DirectControlFrm.SetFocus ;
       end
    else begin
       DirectControlFrm := TDirectControlFrm.Create(Self) ;
       DirectControlFrm.Left := 20 ;
       DirectControlFrm.Top := 20 ;
       end ;

    end ;

function TMainFrm.GetSpecialFolder(const ASpecialFolderID: Integer): string;
// --------------------------
// Get Windows special folder
// --------------------------
var
  vSpecialPath : array[0..MAX_PATH] of Char;
begin

    SHGetFolderPath( 0, ASpecialFolderID, 0,0,vSpecialPath) ;
    Result := StrPas(vSpecialPath);

    end;

function TMainFrm.LSControlLinesAvailable :Boolean ;
// -------------------------------------------------------
// Return TRUE if light source control lines are available
// -------------------------------------------------------
var
    i : Integer ;
begin
     // Exit if no light source or D/A channels configured
     Result := False ;
     for i := 0 to High(MainFrm.IOConfig.LSControlLine) do
         if MainFrm.IOResourceAvailable(MainFrm.IOConfig.LSControlLine[i]) then Result := True ;
     end ;

function TMainFrm.LSNumControlLinesAvailable : Integer ;
// -------------------------------------------------------
// Return TRUE if light source control lines are available
// -------------------------------------------------------
var
    NumLines : Integer ;
    Done : Boolean ;
begin
     NumLines := 0 ;
     Done := False ;
     while not Done do begin
         if MainFrm.IOResourceAvailable(MainFrm.IOConfig.LSControlLine[NumLines]) then Inc(NumLines)
                                                                                  else Done := True ;
         if NumLines > High(MainFrm.IOConfig.LSControlLine) then Done := True ;
         end;
     Result := NumLines ;
     end ;


function TMainFrm.CreateIndexedFileName(
         FileName : String ) : String ;
// ---------------------------------------------------
// Append an (incremented) index number to end of file
// ---------------------------------------------------
var
     i : Integer ;
     ExtensionStart : Integer ;
     IndexNumberStart : Integer ;
     IndexNum : Integer ;
     sDate,NewFileName : String ;
     FileStem : String ;
begin

     // Create new file name based on date

     DateTimeToString( sDate, 'yymmdd', Date() ) ;
     FileName := DataDirectory  + sDate + '.idr' ;

     // Find '_nnn' index number (if it exists)
     i := Length(FileName) ;
     ExtensionStart := Length(FileName)+1 ;
     IndexNumberStart := -1 ;
     While (i > 0) do begin
         if FileName[i] = '.' then ExtensionStart := i ;
         if FileName[i] = '_' then begin
            IndexNumberStart := i ;
            Break ;
            end ;
         Dec(i) ;
         end ;

     if ((ExtensionStart - IndexNumberStart) > 4)
        or (ExtensionStart < 5) then IndexNumberStart := -1 ;

     // Find next available (lowest) index number for this file name

     FileStem :=  '' ;
     if IndexNumberStart > 0 then begin
        for i := 1 to IndexNumberStart-1 do
            FileStem :=  FileStem + FileName[i] ;
        end
     else begin
        for i := 1 to ExtensionStart-1 do
            FileStem :=  FileStem + FileName[i] ;
        end ;

     IndexNum := 0 ;
     repeat
          Inc(IndexNum) ;
          NewFileName := FileStem + format('_%.3d',[IndexNum]) ;
          for i := ExtensionStart to Length(FileName) do
              NewFileName := NewFileName + FileName[i] ;
          until not FileExists(NewFileName) ;

     // Return name
     Result := NewFileName ;

     end ;

procedure TMainFrm.SetFormPosition(
          Form : TForm ;             // Form
          Left : Integer ;           // Left edge of form
          Top : Integer ;            // Top edge
          Width : Integer ;          // Form width
          Height : Integer           // Form height
          ) ;
// -----------------
// Set form position
// -----------------
begin

     if FormPos[Form.Tag].Width = 0 then begin
        FormPos[Form.Tag].Left := Left ;
        FormPos[Form.Tag].Top := Top ;
        FormPos[Form.Tag].Width := Width ;
        FormPos[Form.Tag].Height := Height ;
        end;

     Form.Left := FormPos[Form.Tag].Left ;
     Form.Top := FormPos[Form.Tag].Top ;
     Form.Width := FormPos[Form.Tag].Width ;
     Form.Height := FormPos[Form.Tag].Height ;
     end ;

procedure TMainFrm.SaveFormPosition(
          Form : TForm               // Form
          ) ;
// ------------------
// Save form position
// ------------------
begin

    FormPos[Form.Tag].Left := Form.Left ;
    FormPos[Form.Tag].Top := Form.Top ;
    FormPos[Form.Tag].Width := Form.Width ;
    FormPos[Form.Tag].Height := Form.Height ;
    end ;


end.

