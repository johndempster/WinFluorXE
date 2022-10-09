unit FileIOUnit;
// =============================================================================
// WinFluor - Windows Fluorescence Imaging Program - Data file input/output methods
// (c) J. Dempster, University of Strathclyde, 2001-2003, All Rights Reserved
// =============================================================================
// 26.2.2002
// 29.6.2003 ... ROI shape added
// 7.7.2003 .... Import File added
// 31.7.2003 ... SaveFrameAsTIFF added
// 7.9.2003 .... Multiple PIC,TIF & STK files can now be imported
// 8.10.2003 ... MainFrm.ADCScanInterval now saved to INI file instead of ADCNumScansPerFrame
// 13.11.2003 .. Cam1.AmpGain added to initialisation list
// 14.03.2005 .. Lens magnification now stored in Cam1.LensMagnification
// 30.06.05 .... Save directories now checked for existence
// 01.09.05 .... Live A/D channel data now stored in MainFrm.ADCChannel
// 29.11.05 .... MainFrm.ADCRecordingTime added to ini file
// 12.05.06 .... LoadInitialisationFile now reads any size of INI file
// 20.07.06 .... EventAnalysisUnit settings added to INI file
// 17.08.06 .... Seal test added to INI file
// 08.08.06 .... EXCSPECTRUM fields added to INI file
// 11.01.07 .... LightSource laser settings added to INI file
// 29.01.07 .... DISPLAYGRID= added to INI file
// 09.07.07 .... Event detection parameters updated
// 18.07.07 .... VPDIR= now stores VProtDirectory
// 16.01.08 .... LSLEDOFFV= and LSLEDOFFV= added
// 21.04.08 .... EXCSEQDF%x= (excitation sequence divide factor) added
// 16.09.08 ..... ROISIZE= Live regions of interest size now added
// 17.09.08 ..... LIVEWVL= Live window wavelength added to INI
// 19.01.09 .... LSSCWAVEL= shutter closed wavelength added to INI
// 23.01.09 .... Nicholas Schwarz's modifications added
// 09.03.09 .... IOLSLS= and IOLSLE= added (Laser control DAC output range)
// 20.05.09 .... LSSCBLANK=', LightSource.ShutterBlankingPeriod added to INI file
// 04.10.09 .... NS ... VCDIV2=, VCHOLD2=, IOVCOM2=, Third voltage command channel, Vout2
// 13.07.10 .... JD Changes to Event Analysis settings
// 01.09.10 .... JD Mainfrm.ROIX Mainfrm.ROIY saved to INI file
// 06.10.10 .... JD MainFrm.Cam1.VideoMode (CAMVM=) saved in INI file
// 07.10.10 .... JD MainFrm.Cam1.CCDClearPreExposure 'CAMCCDCLR=' saved to INI file ;
// 29.10.10 .... JD MainFrm.Cam1.CameraADC 'CAMADC=' saved to INI file
// 15.12.10 .... JD LightSource TIRF settings added to INI file
// 21.12.10 .... JD PHOTOSTIMFIL= added, MainFrm.PhotoStimFileName ;
// 01.02.11 .... JD CAMCCDPERO= added, MainFrm.Cam1.CCDPostExposureReadout
// 02.02.11 .... JD 'CALBARSZ=', MainFrm.CalibrationBarSize added
//                  CALBARTH=', MainFrm.CalibrationBarThickness added
// 23.07.12 .... JD 'STARTSTIMONREC=', MainFrm.StartStimOnRecord added
// 04.12.12 .... JD 'EXCONREC=', MainFrm.ExcitationOnWhenRecording added
// 29.01.13 .... JD Z stage settings added to INI file ZStage.SaveSettings(), ZStage.ReadSettings();
// 24.04.13 .... JD If VProtDirectory in INI file does not exist, use default directory (\winfluor\vprot
// 27.09.13 .... JD 'LSSCHTIME=', LightSource.ShutterChangeTime added to INI file
// 07.11.13 .... JD 'CAMADCGN=', MainFrm.Cam1.ADCGain and 'CAMVSS=', MainFrm.Cam1.CCDVerticalShiftSpeed added
// 29.01.14 Updated to Compile under both 32/64 bits (File handle now THandle)
// 27.02.14 .... JD  'EMFCHTIME=', LightSource.EMFilterChangeTime added
//                    'IOEMFS=', MainFrm.IOConfig.EMFilterStart added
//                    'IOEMFE=', MainFrm.IOConfig.EMFilterEnd added
//                    'EXCWEMF%d='MainFrm.EXCWavelengths[iWav].EmFilter added
//                    'EXCWEMN%d=' MainFrm.EXCWavelengths[iWav].EmName added
//                    'EXCSPEMFILT=', MainFrm.EXCSpectrumEMFilter added
// 03.03.14 ......... 'SPLIM0=',MainFrm.SplitImageName[0]
//                    'SPLIM1=',MainFrm.SplitImageName[1]
//                    Header, 'SPLIM=',MainFrm.SplitImage
// 05.03.14 ......... 'BULBEXP=', MainFrm.BulbExposureMode
//                    'EXCWFEX%d=',[iWav]), MainFrm.EXCWavelengths[iWav].FractionalExposure
// 22.07.14 ......... 'CAMSEL=', MainFrm.Cam1.SelectedCamera
// 23.10.14 ......... LightSource.DeviceType now a property rather than public variable
// 02.12.14 ......... LightSource.LaserWavelength etc. now supports 8 light sources 0..7
//                    Keywords numbers 1..8 (LSLAS1WAV= .. LSLAS8WAV=
// 23.01.14 ......... DARKLEVLO= and DARKLEVHI= added.
// 3.2.15 ........... XYStage.Save/ReadSettings added
// 14.5.15 .......... CAMDEIL= MainFrm.Cam1.DisableExposureIntervalLimit added
// 24.9.15 .......... FPLEFTi= MainFrm.FormPos[i].left etc added
// 18.01.16 ......... ImageFile.CreateFile() now uses .NumFrames rather than single frames flag
// 06.06.17 ......... PMTRatio calculation settings now added
// 04.07.17 ......... 'CAMSPNR=', MainFrm.Cam1.SpotNoiseReduction added
// 31.07.17 ......... 'CAMSPNR=', MainFrm.Cam1.SpotNoiseReduction added
// 18.01.18 ......... CAMLIGHTSPEEDMODE= MainFr.Cam1.LIGHTSPEEDMODE added
// 20.11.19 ......... 'EXPDIR=', MainFrm.ExportDirectory added
// 01.09.22 ......... TStringList now used to hold INI file KEY=Value settings

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImageFile, IDRFile, StrUtils, system.UITypes ;

type

 // Region of interest definition record

TOVERLAPPED = Record
    Internal : DWORD;
    InternalHigh : DWORD;
    Offset : DWORD ;
    OffsetHigh :DWORD;
    hEvent : THANDLE ;
    end ;


  TFileIO = class(TDataModule)
    ImageFile: TImageFile;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    procedure SaveDialogTypeChange(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure OpenDialogTypeChange(Sender: TObject);
  private
    { Private declarations }
    FileTypeNames : Array[1..10] of String ;
    FileTypeExts : Array[1..10] of String ;
    NumFileTypes : Integer ;

    Overlap : _Overlapped ;

  public
    { Public declarations }
    procedure SaveInitialisationFile( FileName : String ) ;
    procedure LoadInitialisationFile( FileName : String ) ;
    procedure ImportFile ;
    procedure ExportFile ;
    procedure SaveFrameAsTIFF ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : single        // Value
                           ) ; Overload ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : Integer        // Value
                           ) ; Overload ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : NativeInt        // Value
                           ) ; Overload ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : String        // Value
                           ) ; Overload ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : Boolean        // Value
                           ) ; Overload ;


   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : single       // Value
                         ) : Single ; Overload ;        // Return value

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : Integer       // Value
                         ) : Integer ; Overload ;        // Return value

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : NativeInt       // Value
                         ) : NativeInt ; Overload ;        // Return value

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : string       // Value
                         ) : string ; Overload ;        // Return value

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : Boolean       // Value
                         ) : Boolean ; Overload ;        // Return value

  function ExtractFileNameOnly( FilePath : string ) : string ;

  end;

var
  FileIO: TFileIO;

implementation

uses Main, LabIOUnit, maths, AmpModule , RecUnit, ViewUnit,
  LightSourceUnit, ZStageUnit, XYStageUnit;

{$R *.DFM}


procedure TFileIO.DataModuleCreate(Sender: TObject);
// -----------------------------------
// Initialisations when module created
// -----------------------------------
var
     i : Integer ;
begin

     FileTypeNames[1] := 'BIORad PIC Files' ;
     FileTypeExts[1] := '.PIC' ;
     FileTypeNames[2] := 'TIFF Files' ;
     FileTypeExts[2] := '.TIF' ;
     FileTypeNames[3] := 'STK Files' ;
     FileTypeExts[3] := '.STK' ;
     FileTypeNames[4] := 'ICS Files' ;
     FileTypeExts[4] := '.ICS' ;
     NumFileTypes := 4 ;

     SaveDialog.Filter := '' ;
     for i := 1 to NumFileTypes do begin
         if i <> 1 then SaveDialog.Filter := SaveDialog.Filter + '|' ;
         SaveDialog.Filter := SaveDialog.Filter +
                              format( '%s (*%s)|*%s',
                                      [FileTypeNames[i],
                                       FileTypeExts[i],
                                       FileTypeExts[i]]) ;
         end ;
     SaveDialog.FilterIndex := 1 ;
     SaveDialog.DefaultExt := RightStr(FileTypeExts[SaveDialog.FilterIndex],3) ;
     SaveDialog.Options := [ofHideReadOnly,ofPathMustExist] ;

     OpenDialog.Filter := SaveDialog.Filter ;
     OpenDialog.FilterIndex := SaveDialog.FilterIndex ;
     OpenDialog.DefaultExt := SaveDialog.DefaultExt ;

     end;



procedure TFileIO.SaveInitialisationFile(
          FileName : String
          ) ;
// ------------------------
// Save initialisation file
// ------------------------
var
   Header : TStringList ;
   i,iSeq,iWav,ch,nWritten : Integer ;
   Dev : Integer ;
begin

{     INIFileHandle := FileCreate( FileName ) ;

     if Integer(INIFileHandle) < 0 then begin
        MainFrm.StatusBar.SimpleText := 'Unable to create : ' + FileName ;
        ShowMessage(MainFrm.StatusBar.SimpleText);
        Exit ;
        end ;

     // Initialise empty header buffer with zero bytes
     for i := 1 to sizeof(Header) do Header[i] := #0 ;}

     // Create string list to hold header key=values
     Header := TStringList.Create ;

     // Camera settings
     AddKeyValue( Header, 'CAMTYPE', MainFrm.CameraType ) ;

     // Auxiliary camera settings
     AddKeyValue( Header, 'CAMTYPEAUX', MainFrm.AuxCameraType ) ;

     AddKeyValue( Header, 'CAMSEL', MainFrm.Cam1.SelectedCamera ) ;

     AddKeyValue( Header, 'CAMFI', MainFrm.Cam1.FrameInterval ) ;
     AddKeyValue( Header, 'CAMFL', MainFrm.Cam1.FrameLeft ) ;
     AddKeyValue( Header, 'CAMFR', MainFrm.Cam1.FrameRight ) ;
     AddKeyValue( Header, 'CAMFT', MainFrm.Cam1.FrameTop ) ;
     AddKeyValue( Header, 'CAMFB', MainFrm.Cam1.FrameBottom ) ;
     AddKeyValue( Header, 'CAMBIN', MainFrm.Cam1.BinFactor ) ;
     AddKeyValue( Header, 'CAMRS', MainFrm.Cam1.ReadoutSpeed ) ;
     AddKeyValue( Header, 'CAMVM', MainFrm.Cam1.CameraMode ) ;
     AddKeyValue( Header, 'CAMADC', MainFrm.Cam1.CameraADC ) ;
     AddKeyValue( Header, 'CAMCCDCLR', MainFrm.Cam1.CCDClearPreExposure ) ;
     AddKeyValue( Header, 'CAMCCDPERO', MainFrm.Cam1.CCDPostExposureReadout ) ;
     AddKeyValue( Header, 'CAMLIGHTSPEEDMODE', MainFrm.Cam1.LightSpeedMode ) ;

     AddKeyValue( Header, 'NFREQ', MainFrm.NumFramesRequired ) ;
     AddKeyValue( Header, 'NRECPER', MainFrm.RecordingPeriod ) ;
     AddKeyValue( Header, 'ADCRECTIME', MainFrm.ADCRecordingTime ) ;
     AddKeyValue( Header, 'CAMCOM', MainFrm.Cam1.ComPort ) ;
     AddKeyValue( Header, 'CAMGN', MainFrm.Cam1.AmpGain ) ;
     // Camera exposure/readout trigger offset
     AddKeyValue( Header, 'CAMTRIGOFFSET', MainFrm.CameraTriggerOffset ) ;

     AddKeyValue( Header, 'CAMTEMPSET', MainFrm.Cam1.CameraTemperatureSetPoint ) ;

     AddKeyValue( Header, 'CAMADDRT', MainFrm.Cam1.AdditionalReadoutTime ) ;

     AddKeyValue( Header, 'CAMDEIL',  MainFrm.Cam1.DisableExposureIntervalLimit ) ;

     AddKeyValue( Header, 'BULBEXP', MainFrm.BulbExposureMode ) ;

     // Camera readout A/D converter gain
     AddKeyValue( Header, 'CAMADCGN', MainFrm.Cam1.ADCGain ) ;

     // Camera CCD vertical line shift speed
     AddKeyValue( Header, 'CAMVSS', MainFrm.Cam1.CCDVerticalShiftSpeed ) ;

     AddKeyValue( Header, 'LENSMAG', MainFrm.Cam1.LensMagnification ) ;

     AddKeyValue( Header, 'CALBARSZ', MainFrm.CalibrationBarSize ) ;
     AddKeyValue( Header, 'CALBARTH', MainFrm.CalibrationBarThickness ) ;

     AddKeyValue(  Header, 'SPLIM0',MainFrm.SplitImageName[0]);
     AddKeyValue(  Header, 'SPLIM1',MainFrm.SplitImageName[1]);
     AddKeyValue( Header, 'SPLIM',MainFrm.SplitImage) ;

     AddKeyValue( Header, 'RECMODE', MainFrm.RecordingMode ) ;
     AddKeyValue( Header, 'RECPER', MainFrm.RecordingPeriod ) ;
     AddKeyValue( Header, 'TLAPINT', MainFrm.TimeLapseInterval ) ;
     AddKeyValue( Header, 'BURDUR', MainFrm.BurstDuration ) ;
     AddKeyValue( Header, 'BURINT', MainFrm.BurstInterval ) ;

     AddKeyValue( Header, 'PAL', Integer(MainFrm.PaletteType) ) ;
     AddKeyValue( Header, 'DZOOM', MainFrm.DisplayZoomIndex ) ;

     AddKeyValue( Header, 'ROISIZE', MainFrm.ROISize ) ;
     AddKeyValue( Header, 'ROIX', MainFrm.ROIX ) ;
     AddKeyValue( Header, 'ROIY', MainFrm.ROIY ) ;
     AddKeyValue( Header, 'LIVEWVL', MainFrm.LiveWindowWavelength ) ;

     // A/D channel settings
     AddKeyValue( Header, 'ADCNC', MainFrm.ADCNumChannels ) ;
     AddKeyValue( Header, 'ADCINPUTMODE', LabIO.ADCInputMode ) ;
     // NI interface API
     AddKeyValue( Header, 'NIDAQAPI', LabIO.NIDAQAPI ) ;

     AddKeyValue( Header, 'ADCSI', MainFrm.ADCScanInterval ) ;
     AddKeyValue( Header, 'ADCVR', MainFrm.ADCVoltageRange ) ;
     AddKeyValue( Header, 'ADCDW', MainFrm.ADCDisplayWindow ) ;
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
         AddKeyValue( Header, format('CIN%d',[ch]), MainFrm.ADCChannel[ch].ChannelOffset) ;
         AddKeyValue( Header, format('CU%d',[ch]), MainFrm.ADCChannel[ch].ADCUnits ) ;
         AddKeyValue( Header, format('CN%d',[ch]), MainFrm.ADCChannel[ch].ADCName ) ;
         AddKeyValue( Header, format('CCF%d',[ch]), MainFrm.ADCChannel[ch].ADCCalibrationFactor ) ;
         AddKeyValue( Header, format('CSC%d',[ch]), MainFrm.ADCChannel[ch].ADCScale) ;
         end ;

     // Patch clamp amplifier data
     for i := 1 to 2 do begin
         AddKeyValue( Header, format('AMP%d',[i]),Amplifier.AmplifierType[i] ) ;
         AddKeyValue( Header, format('AMPCH%d',[i]),Amplifier.GainTelegraphChannel[i] ) ;
         AddKeyValue( Header, format('AMPGAINCH%d',[i]),Amplifier.GainTelegraphChannel[i] ) ;
         AddKeyValue( Header, format('AMPMODECH%d',[i]),Amplifier.ModeTelegraphChannel[i] ) ;
         end ;

     // Command voltage settings
     AddKeyValue( Header, 'VCDIV0', MainFrm.VCommand[0].DivideFactor ) ;
     AddKeyValue( Header, 'VCHOLD0', MainFrm.VCommand[0].HoldingVoltage ) ;
     AddKeyValue( Header, 'VCDIV1', MainFrm.VCommand[1].DivideFactor ) ;
     AddKeyValue( Header, 'VCHOLD1', MainFrm.VCommand[1].HoldingVoltage ) ;
     AddKeyValue( Header, 'VCDIV2', MainFrm.VCommand[2].DivideFactor ) ;
     AddKeyValue( Header, 'VCHOLD2', MainFrm.VCommand[2].HoldingVoltage ) ;

     // Default digital output port states
     for Dev := 1 to MaxDevices do begin
         AddKeyValue( Header, format('DIGOUTSTATE%d',[Dev]), LabIO.DigOutState[Dev] ) ;
         end ;

     // Light source
     AddKeyValue( Header, 'LSDEV', LightSource.DeviceType ) ;
     AddKeyValue( Header, 'LSW1', LightSource.Wavelength1 ) ;
     AddKeyValue( Header, 'LSV1', LightSource.Voltage1 ) ;
     AddKeyValue( Header, 'LSW2', LightSource.Wavelength2 ) ;
     AddKeyValue( Header, 'LSV2', LightSource.Voltage2 ) ;

     // Laser settings
     for i := 0 to lsMaxLightSources-1 do begin
        AddKeyValue( Header, format('LSLAS%dWAV',[i+1]), LightSource.LaserWavelength[i] ) ;
        AddKeyValue( Header, format('LSLAS%dDEL',[i+1]), LightSource.LaserDelay[i] ) ;
        AddKeyValue( Header, format('LSLAS%dVOFF',[i+1]), LightSource.LaserOffVoltage[i] ) ;
        AddKeyValue( Header, format('LSLAS%dVON',[i+1]), LightSource.LaserOnVoltage[i] ) ;
        AddKeyValue( Header, format('LSLAS%dINT',[i+1]), LightSource.LaserIntensity[i] ) ;
        end ;

     // LED settings
     AddKeyValue( Header, 'LSLEDOFFV', LightSource.LEDOffVoltage ) ;
     AddKeyValue( Header, 'LSLEDMAXV', LightSource.LEDMaxVoltage ) ;

     // TIRF settings
     for i := 1 to lsMaxTIRFGalvos do begin
        AddKeyValue( Header, format('LSTIRFOFF%d',[i]), LightSource.TIRFOff[i] ) ;
        AddKeyValue( Header, format('LSTIRFON%d',[i]), LightSource.TIRFOn[i] ) ;
        AddKeyValue( Header, format('LSTIRFWF%d',[i]), LightSource.TIRFWF[i] ) ;
        end ;

     // Shutter closed wavelength
     AddKeyValue( Header, 'LSSCWAVEL', LightSource.ShutterClosedWavelength ) ;

     // Shutter blanking period
     AddKeyValue( Header, 'LSSCBLANK', LightSource.ShutterBlankingPeriod ) ;

     // Shutter open/close change time
     AddKeyValue( Header, 'LSSCHTIME', LightSource.ShutterChangeTime ) ;

     // Emission filter
     AddKeyValue( Header, 'EMFCHTIME', LightSource.EMFilterChangeTime ) ;

     // Excitation wavelength settings
     AddKeyValue( Header, 'EXCSW', MainFrm.EXCSingleWavelength ) ;

     // Single wavelength selected
     AddKeyValue( Header, 'EXCSWN', MainFrm.EXCSingleWavelengthNum ) ;

     AddKeyValue( Header, 'EXCONREC', MainFrm.ExcitationOnWhenRecording ) ;

     // Excitation multi-wavelength sequence settings
     AddKeyValue( Header, 'EXCSEQNUM', MainFrm.EXCSequenceNum ) ;
     for iSeq := 0 to MaxEXCSequences-1 do begin
         AddKeyValue( Header, format('EXCNW%d',[iSeq]), MainFrm.EXCNumWavelengths[iSeq] ) ;
         AddKeyValue( Header, format('EXCSEQNAM%d',[iSeq]), MainFrm.EXCSequenceName[iSeq] ) ;
         for iWav := 0 to MainFrm.EXCNumWavelengths[iSeq] do begin
             AddKeyValue( Header, format('EXCSEQ%dW%d',[iSeq,iWav]), MainFrm.EXCSequence[iWav,iSeq].WavelengthNum) ;
             AddKeyValue( Header, format('EXCSEQ%dDF%d',[iSeq,iWav]), MainFrm.EXCSequence[iWav,iSeq].DivideFactor) ;
             end ;
         end ;

     // Save sequence 0 for compatibility with older versions of WinFluor
     AddKeyValue( Header, 'EXCNW', MainFrm.EXCNumWavelengths[0] ) ;
     for iWav := 0 to MainFrm.EXCNumWavelengths[0] do begin
         AddKeyValue( Header, format('EXCSEQW%d',[iWav]), MainFrm.EXCSequence[iWav,0].WavelengthNum) ;
         AddKeyValue( Header, format('EXCSEQDF%d',[iWav]), MainFrm.EXCSequence[iWav,0].DivideFactor) ;
         end ;

     // Save wavelengths list
     for iWav := 0 to High(MainFrm.EXCWavelengths) do begin
         AddKeyValue( Header, format('EXCWC%d',[iWav]), MainFrm.EXCWavelengths[iWav].Centre) ;
         AddKeyValue( Header, format('EXCWW%d',[iWav]), MainFrm.EXCWavelengths[iWav].Width) ;
         AddKeyValue( Header, format('EXCWEMF%d',[iWav]), MainFrm.EXCWavelengths[iWav].EmFilter) ;
         AddKeyValue( Header, format('EXCWEMN%d',[iWav]), MainFrm.EXCWavelengths[iWav].EmName) ;
         AddKeyValue( Header, format('EXCWFEX%d',[iWav]), MainFrm.EXCWavelengths[iWav].FractionalExposure) ;
         end ;

     AddKeyValue( Header, 'EXCSPSTARTW', MainFrm.EXCSpectrumStartWavelength ) ;
     AddKeyValue( Header, 'EXCSPENDW', MainFrm.EXCSpectrumEndWavelength ) ;
     AddKeyValue( Header, 'EXCSPBANDW', MainFrm.EXCSpectrumBandwidth ) ;
     AddKeyValue( Header, 'EXCSPEMFILT', MainFrm.EXCSpectrumEMFilter ) ;
     AddKeyValue( Header, 'EXCSPSTEPS', MainFrm.EXCSpectrumStepSize ) ;

     // Stimulus program file
     AddKeyValue( Header, 'STIMFILE', MainFrm.StimFileName) ;

     // Photo stimulus program file
     AddKeyValue( Header, 'PHOTOSTIMFIL', MainFrm.PhotoStimFileName) ;

     // Fluophore mainFrm.Binding mainFrm.BindingEquations table
     for i := 0 to MainFrm.IDRFile.MaxEquations-1 do begin
         AddKeyValue( Header, format('EQNUSE%d',[i]), mainFrm.BindingEquations[i].InUse ) ;
         AddKeyValue( Header, format('EQNION%d',[i]), mainFrm.BindingEquations[i].Ion) ;
         AddKeyValue( Header, format('EQNUN%d',[i]), mainFrm.BindingEquations[i].Units) ;
         AddKeyValue( Header, format('EQNNAM%d',[i]), mainFrm.BindingEquations[i].Name) ;
         AddKeyValue( Header, format('EQNRMAX%d',[i]), mainFrm.BindingEquations[i].RMax) ;
         AddKeyValue( Header, format('EQNRMIN%d',[i]), mainFrm.BindingEquations[i].RMin) ;
         AddKeyValue( Header, format('EQNKEFF%d',[i]), mainFrm.BindingEquations[i].KEff) ;
         end ;

     AddKeyValue( Header, 'DDIR', MainFrm.DataDirectory ) ;

     AddKeyValue( Header, 'VPDIR', MainFrm.VProtDirectory ) ;

     AddKeyValue( Header, 'EXPDIR', MainFrm.ExportDirectory ) ;

     // Printer page settings
     AddKeyValue( Header, 'PRTM', MainFrm.PrinterTopMargin )  ;
     AddKeyValue( Header, 'PRBM', MainFrm.PrinterBottomMargin )  ;
     AddKeyValue( Header, 'PRLM', MainFrm.PrinterLeftMargin )  ;
     AddKeyValue( Header, 'PRRM', MainFrm.PrinterRightMargin )  ;
     AddKeyValue( Header, 'PRLT', MainFrm.PrinterLineThickness )  ;
     AddKeyValue( Header, 'PRMS', MainFrm.PrinterMarkerSize )  ;
     AddKeyValue( Header, 'PRUC', MainFrm.PrinterUseColor ) ;
     AddKeyValue( Header, 'PRFN', MainFrm.PrinterFontName ) ;
     AddKeyValue( Header, 'PRFS', MainFrm.PrinterFontSize )  ;

     for i := 0 to High(MainFrm.RecentFiles) do
         AddKeyValue(Header,format('FILE%d',[i]),MainFrm.RecentFiles[i]) ;

     // Input/output/control line configuration
     AddKeyValue( Header, 'IOADCI', MainFrm.IOConfig.ADCIn ) ;
     AddKeyValue( Header, 'IOCAMS', MainFrm.IOConfig.CameraStart ) ;
     AddKeyValue( Header, 'IOCAMSAH', MainFrm.IOConfig.CameraStartActiveHigh ) ;
     AddKeyValue( Header, 'IOVCOM0', MainFrm.IOConfig.VCommand[0] ) ;
     AddKeyValue( Header, 'IOVCOM1', MainFrm.IOConfig.VCommand[1] ) ;
     AddKeyValue( Header, 'IOVCOM2', MainFrm.IOConfig.VCommand[2] ) ;
     AddKeyValue( Header, 'IOLSSU', MainFrm.IOConfig.LSShutter ) ;
     AddKeyValue( Header, 'IOLSSUAH', MainFrm.IOConfig.LSShutterActiveHigh ) ;

     // Light source control outputs
     for i := 0 to MaxLSControlLine do begin
        AddKeyValue( Header, format('IOLSCON%d',[i]), MainFrm.IOConfig.LSControlLine[i] ) ;
        end ;

     // Emission filter control lines
     AddKeyValue( Header, 'IOEMFS', MainFrm.IOConfig.EMFilterStart ) ;
     AddKeyValue( Header, 'IOEMFE', MainFrm.IOConfig.EMFilterEnd ) ;

     AddKeyValue( Header, 'IODSTA', MainFrm.IOConfig.DigitalStimStart ) ;
     AddKeyValue( Header, 'IODEND', MainFrm.IOConfig.DigitalStimEnd ) ;
     AddKeyValue( Header, 'IOPSX', MainFrm.IOConfig.PhotoStimX ) ;
     AddKeyValue( Header, 'IOPSY', MainFrm.IOConfig.PhotoStimY ) ;
     AddKeyValue( Header, 'IOPSI1', MainFrm.IOConfig.PhotoStimI1 ) ;
     AddKeyValue( Header, 'IOPSI2', MainFrm.IOConfig.PhotoStimI2 ) ;
     AddKeyValue( Header, 'IOPSI3', MainFrm.IOConfig.PhotoStimI3 ) ;

     // Photo-stimulus Pockels cell and shutter configuration (Added by NS)
     AddKeyValue( Header, 'IOPSMETER', MainFrm.IOConfig.PhotoStimMeter ) ;
     AddKeyValue( Header, 'IOPSSU', MainFrm.IOConfig.PhotoStimShutter ) ;
     AddKeyValue( Header, 'IOPSSLA', MainFrm.IOConfig.PhotoStimShutterLatency ) ;
     AddKeyValue( Header, 'IOPSSUAH', MainFrm.IOConfig.PhotoStimShutterActiveHigh ) ;
     AddKeyValue( Header, 'IOPSPCMAN', MainFrm.IOconfig.PhotoStimPowerCalManual ) ;
     AddKeyValue( Header, 'IOPSMETERR', MainFrm.IOConfig.PhotoStimMeterRange ) ;
     AddKeyValue( Header, 'IOPSMETERS', MainFrm.IOconfig.PhotoStimMeterScale ) ;


     AddKeyValue( Header, 'IOCLKSYNC', MainFrm.IOConfig.ClockSyncLine ) ;

     // Event detection settings
{     AddKeyValue( Header, 'EVANDEADTIME', MainFrm.EventAnalysis.DeadTime ) ;
     AddKeyValue( Header, 'EVANTHRESHOLD', MainFrm.EventAnalysis.DetectionThreshold ) ;
     AddKeyValue( Header, 'EVANTHRESHDUR', MainFrm.EventAnalysis.ThresholdDuration ) ;
     AddKeyValue( Header, 'EVANPOLARITY', MainFrm.EventAnalysis.DetectionThresholdPolarity ) ;
     AddKeyValue( Header, 'EVANFIXBASE', MainFrm.EventAnalysis.FixedBaseline ) ;
     AddKeyValue( Header, 'EVANSOURCE', MainFrm.EventAnalysis.DetectionSource ) ;
     AddKeyValue( Header, 'EVANDISPLAYMAX', MainFrm.EventAnalysis.DisplayMax ) ;
     AddKeyValue( Header, 'EVANROLLBASEPERIOD', MainFrm.EventAnalysis.RollingBaselinePeriod ) ;}

     // Seal test settings
     AddKeyValue( Header, 'SEALTUSE',MainFrm.SealTest.Use ) ;
     AddKeyValue( Header, 'SEALTPH1',MainFrm.SealTest.PulseHeight1 ) ;
     AddKeyValue( Header, 'SEALTHV1',MainFrm.SealTest.HoldingVoltage1 ) ;
     AddKeyValue( Header, 'SEALTPH2',MainFrm.SealTest.PulseHeight2 ) ;
     AddKeyValue( Header, 'SEALTHV2',MainFrm.SealTest.HoldingVoltage2 ) ;
     AddKeyValue( Header, 'SEALTPH3',MainFrm.SealTest.PulseHeight3 ) ;
     AddKeyValue( Header, 'SEALTHV3',MainFrm.SealTest.HoldingVoltage3 ) ;
     AddKeyValue( Header, 'SEALTPW',MainFrm.SealTest.PulseWidth ) ;

     // Photo-stimulus settings
     AddKeyValue( Header, 'PSPER',MainFrm.PhotoStim.Period ) ;
     AddKeyValue( Header, 'PSREP',MainFrm.PhotoStim.RepeatedStim ) ;
     AddKeyValue( Header, 'PSNSTIMP',MainFrm.PhotoStim.NumStimPoints ) ;
     AddKeyValue( Header, 'PSATTEN',MainFrm.PhotoStim.Attenuator ) ;
     for i := 1 to 3 do
     begin
       AddKeyValue( Header, format('PSXC%d',[i]), MainFrm.PhotoStim.XCenter[i]) ;
       AddKeyValue( Header, format('PSYC%d',[i]), MainFrm.PhotoStim.YCenter[i]) ;
       AddKeyValue( Header, format('PSXS%d',[i]), MainFrm.PhotoStim.XScale[i]) ;
       AddKeyValue( Header, format('PSYS%d',[i]), MainFrm.PhotoStim.YScale[i]) ;
       AddKeyValue( Header, format('PSPCPENA%d',[i]), MainFrm.PhotoStim.PCEnable[i] ) ;
       AddKeyValue( Header, format('PSPCPMIN%d',[i]), MainFrm.PhotoStim.PCPowerMin[i] ) ;
       AddKeyValue( Header, format('PSPCPMAX%d',[i]), MainFrm.PhotoStim.PCPowerMax[i] ) ;
       AddKeyValue( Header, format('PSPCBIAS%d',[i]), MainFrm.PhotoStim.PCBias[i] ) ;
       AddKeyValue( Header, format('PSPCVPI%d',[i]), MainFrm.PhotoStim.PCVoltagePi[i] ) ;
       AddKeyValue( Header, format('PSPCPC%d',[i]), MainFrm.PhotoStim.PCPolarizationCross[i] ) ;
       AddKeyValue( Header, format('PSPC302%d',[i]), MainFrm.PhotoStim.PCConoptics302[i] ) ;
       AddKeyValue( Header, format('PSLPMIN%d',[i]), MainFrm.PhotoStim.LinearPowerMin[i] ) ;
       AddKeyValue( Header, format('PSLPMAX%d',[i]), MainFrm.PhotoStim.LinearPowerMax[i] ) ;
       AddKeyValue( Header, format('PSLVMIN%d',[i]), MainFrm.PhotoStim.LinearVoltageMin[i] ) ;
       AddKeyValue( Header, format('PSLVMAX%d',[i]), MainFrm.PhotoStim.LinearVoltageMax[i] );
       AddKeyValue( Header, format('PSPCSHU%d',[i]), MainFrm.PhotoStim.EnableShutter[i] ) ;
     end;
     AddKeyValue( Header, 'PSROTAT',MainFrm.PhotoStim.ImageRotation ) ;
     AddKeyValue( Header, 'PSMICPERPX',MainFrm.PhotoStim.XMicronsPerPixel ) ;
     AddKeyValue( Header, 'PSMICPERPY',MainFrm.PhotoStim.YMicronsPerPixel ) ;
     AddKeyValue( Header, 'PSPVLOG',MainFrm.PhotoStim.PVLogFile ) ;
     AddKeyValue( Header, 'PSREFLE',MainFrm.PhotoStim.RefLineEnabled ) ;
     AddKeyValue( Header, 'PSCMDTERM',MainFrm.PhotoStim.CmdTermZero ) ;

     AddKeyValue( Header, 'DISPLAYGRID', MainFrm.mnDisplayGrid.Checked ) ;

     // Append visibility state of channels in RecADCOnlyUnit
     // Modified by NS 19 March 2009
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
        AddKeyValue( Header, format('CVRADCO%d',[ch]), MainFrm.ADCChannelRecADCOnlyUnitVisible[ch] ) ;
     end ;

     // Append visibility state of channels in Sealtest
     // Modified by NS 24 March 2009
     AddKeyValue( Header, 'CVSTNUM', MainFrm.ADCChannelSealtestNumberOfChannels ) ;
     for ch := 0 to MainFrm.ADCChannelSealtestNumberOfChannels-1 do begin
        AddKeyValue( Header, format('CVST%d',[ch]), MainFrm.ADCChannelSealtestVisible[ch] ) ;
     end ;

     // Append state of SmoothDifferentiate window
     // Modified by NS 10 April 2009
     for ch := 0 to (4 - 1) do begin
        AddKeyValue( Header, format('SDWCH%d',[ch]), MainFrm.SmoothDifferentiateUnitVisible[ch] ) ;
     end;
     AddKeyValue( Header, 'SDWMA', MainFrm.SmoothDifferentiateUnitMADataWindow ) ;
     AddKeyValue( Header, 'SDWDX', MainFrm.SmoothDifferentiateUnitMADXWindow ) ;

     // Display contrast settings optimisation
     AddKeyValue( Header, 'CNCAFT', MainFrm.ContrastChangeAllFrameTypes ) ;
     AddKeyValue( Header, 'CNAUTOOP', MainFrm.ContrastAutoOptimise ) ;
     AddKeyValue( Header, 'CN6SD', MainFrm.Contrast6SD ) ;

     // Append state of DynamicProtocol window
     // Modified by NS 22 December 2009
     AddKeyValue( Header, 'DPCH', MainFrm.DynamicProtocol.SelectedChannel ) ;
     AddKeyValue( Header, 'DPDIR', MainFrm.DynamicProtocol.Direction ) ;
     AddKeyValue( Header, 'DPTHRES', MainFrm.DynamicProtocol.Threshold ) ;
     AddKeyValue( Header, 'DPDUR', MainFrm.DynamicProtocol.Duration ) ;
     AddKeyValue( Header, 'DPEP', MainFrm.DynamicProtocol.EPRestart ) ;
     AddKeyValue( Header, 'DPPS', MainFrm.DynamicProtocol.PSRestart ) ;
     AddKeyValue( Header, 'DPEPF',MainFrm.DynamicProtocol.EPStimFileName ) ;
     AddKeyValue( Header, 'DPEPI', MainFrm.DynamicProtocol.EPStimIndex ) ;
     AddKeyValue( Header, 'DPPSF',MainFrm.DynamicProtocol.PSStimFileName ) ;
     AddKeyValue( Header, 'DPPSI', MainFrm.DynamicProtocol.PSStimIndex ) ;

     // Auto reset interface cards
     AddKeyValue( Header, 'ARI', MainFrm.AutoResetInterfaceCards ) ;

     AddKeyValue( Header, 'STARTSTIMONREC', MainFrm.StartStimOnRecord ) ;

     // Camera dark level detection range
     AddKeyValue( Header, 'DARKLEVLO',MainFrm.DarkLevelLo ) ;
     AddKeyValue( Header, 'DARKLEVHI',MainFrm.DarkLevelHi ) ;

     // Save Z Stage control settings
     ZStage.SaveSettings( Header ) ;

     // Save XY Stage control settings
     XYStageFrm.SaveSettings( Header ) ;

     // Save PMT Ratio calculation settings
     AddKeyValue( Header, 'PMTRATIOEN',MainFrm.PMTRatio.Enabled ) ;
     AddKeyValue( Header, 'PMTRATIONUM',MainFrm.PMTRatio.NumerChan ) ;
     AddKeyValue( Header, 'PMTRATIODEN',MainFrm.PMTRatio.DenomChan ) ;
     AddKeyValue( Header, 'PMTRATIORAT',MainFrm.PMTRatio.RatioChan ) ;
     AddKeyValue( Header, 'PMTRATIOCON',MainFrm.PMTRatio.ConcChan ) ;
     AddKeyValue( Header, 'PMTRATIOTHR',MainFrm.PMTRatio.Threshold ) ;
     AddKeyValue( Header, 'PMTRATIORATMAX',MainFrm.PMTRatio.RatioMax ) ;
     AddKeyValue( Header, 'PMTRATIOCEN',MainFrm.PMTRatio.ConcEnabled ) ;
     AddKeyValue( Header, 'PMTRATIOCONCMAX',MainFrm.PMTRatio.ConcMax ) ;
     AddKeyValue( Header, 'PMTRATIOINM',MainFrm.PMTRatio.IonName ) ;
     AddKeyValue( Header, 'PMTRATIOUNI',MainFrm.PMTRatio.ConcUnits ) ;
     AddKeyValue( Header, 'PMTRATIORMAX',MainFrm.PMTRatio.RMax ) ;
     AddKeyValue( Header, 'PMTRATIORMIN',MainFrm.PMTRatio.RMin ) ;
     AddKeyValue( Header, 'PMTRATIOKEFF',MainFrm.PMTRatio.Keff ) ;

     // Save form positions
     for I := 0 to High(MainFrm.FormPos) do begin
         AddKeyValue( Header, format('FPTOP%d',[i]), MainFrm.FormPos[i].Top ) ;
         AddKeyValue( Header, format('FPLEFT%d',[i]), MainFrm.FormPos[i].Left) ;
         AddKeyValue( Header, format('FPWIDTH%d',[i]), MainFrm.FormPos[i].Width ) ;
         AddKeyValue( Header, format('FPHEIGHT%d',[i]), MainFrm.FormPos[i].Height) ;
         end;

     Header.SaveToFile( FileName ) ;

     // Dispose of list
     Header.Free ;

     end ;


procedure TFileIO.LoadInitialisationFile(
          FileName : String
          ) ;
// ------------------------
// Load initialisation file
// ------------------------
var
   Header : TStringList ;
   i,iStart,iEnd,iLine,iWav,iSeq,ch : Integer ;
   iValue : Integer ;
   fValue : Single ;
   ADCChannel : TChannel ;
   Dev : Integer ;
begin

     if not FileExists( FileName ) then Exit ;

     // Create and load list of key=value settings

     Header := TStringList.Create ;
     Header.LoadFromFile( FileName ) ;

     // Camera settings
     MainFrm.CameraType := GetKeyValue( Header, 'CAMTYPE', MainFrm.CameraType ) ;

     // Auxiliary camera settings
     MainFrm.AuxCameraType := GetKeyValue( Header, 'CAMTYPEAUX', MainFrm.AuxCameraType ) ;

     MainFrm.Cam1.SelectedCamera := GetKeyValue( Header, 'CAMSEL', MainFrm.Cam1.SelectedCamera ) ;

     MainFrm.Cam1.BinFactor := GetKeyValue( Header, 'CAMBIN', MainFrm.Cam1.BinFactor ) ;

     MainFrm.Cam1.ReadoutSpeed := GetKeyValue( Header, 'CAMRS', MainFrm.Cam1.ReadoutSpeed ) ;

      MainFrm.Cam1.CameraMode := GetKeyValue( Header, 'CAMVM',  MainFrm.Cam1.CameraMode ) ;

     MainFrm.Cam1.CameraADC := GetKeyValue( Header, 'CAMADC', MainFrm.Cam1.CameraADC ) ;

     MainFrm.Cam1.CCDClearPreExposure := GetKeyValue( Header, 'CAMCCDCLR', MainFrm.Cam1.CCDClearPreExposure ) ;

     MainFrm.Cam1.CCDPostExposureReadout := GetKeyValue( Header, 'CAMCCDPERO', MainFrm.Cam1.CCDPostExposureReadout ) ;

     MainFrm.Cam1.LightSpeedMode := GetKeyValue( Header, 'CAMLIGHTSPEEDMODE', MainFrm.Cam1.LightSpeedMode ) ;

     MainFrm.Cam1.ComPort := GetKeyValue( Header, 'CAMCOM', MainFrm.Cam1.ComPort ) ;

     MainFrm.Cam1.AmpGain := GetKeyValue( Header, 'CAMGN', MainFrm.Cam1.AmpGain ) ;

     MainFrm.Cam1.FrameLeft := GetKeyValue( Header, 'CAMFL', MainFrm.Cam1.FrameLeft ) ;

     MainFrm.Cam1.FrameRight := GetKeyValue( Header, 'CAMFR', MainFrm.Cam1.FrameRight ) ;

     MainFrm.Cam1.FrameTop := GetKeyValue( Header, 'CAMFT', MainFrm.Cam1.FrameTop ) ;

     MainFrm.Cam1.FrameBottom := GetKeyValue( Header, 'CAMFB', MainFrm.Cam1.FrameBottom ) ;

     // NOTE. Bin factor, readout speed and imaging area need to be set before
     // .FrameInterval to ensure camera will accept short intervals

     MainFrm.Cam1.FrameInterval := GetKeyValue( Header, 'CAMFI', MainFrm.Cam1.FrameInterval ) ;

     // Camera exposure/readout trigger offset
     MainFrm.CameraTriggerOffset := GetKeyValue( Header, 'CAMTRIGOFFSET', MainFrm.CameraTriggerOffset ) ;

     // Camera temperature set point
     fValue := -50.0 ;
     MainFrm.Cam1.CameraTemperatureSetPoint := GetKeyValue( Header, 'CAMTEMPSET', fValue ) ;

     // Camera additional readout time (s)
     MainFrm.Cam1.AdditionalReadoutTime := GetKeyValue( Header, 'CAMADDRT', MainFrm.Cam1.AdditionalReadoutTime ) ;

     // Disable exposure interval limit checking
     MainFrm.Cam1.DisableExposureIntervalLimit := GetKeyValue( Header, 'CAMDEIL', MainFrm.Cam1.DisableExposureIntervalLimit ) ;

     // Camera CCD readout A/D convert gain
     iValue := 0 ;
     MainFrm.Cam1.ADCGain := GetKeyValue( Header, 'CAMADCGN', iValue ) ;

     // Camera CCD vertical line shift speed
     iValue := -1 ;
     MainFrm.Cam1.CCDVerticalShiftSpeed := GetKeyValue( Header, 'CAMVSS', iValue ) ;

     fValue := 0.0 ;
     fValue := GetKeyValue( Header, 'LENSMAG', fValue ) ;
     if fValue <> 0.0 then MainFrm.Cam1.LensMagnification := fValue ;

     MainFrm.CalibrationBarSize := GetKeyValue( Header, 'CALBARSZ', MainFrm.CalibrationBarSize ) ;
     MainFrm.CalibrationBarThickness := GetKeyValue( Header, 'CALBARTH', MainFrm.CalibrationBarThickness ) ;

     MainFrm.SplitImageName[0] := GetKeyValue(  Header, 'SPLIM0',MainFrm.SplitImageName[0]);
     MainFrm.SplitImageName[1] := GetKeyValue(  Header, 'SPLIM1',MainFrm.SplitImageName[1]);
     MainFrm.SplitImage := GetKeyValue( Header, 'SPLIM',MainFrm.SplitImage) ;

     MainFrm.BulbExposureMode := GetKeyValue( Header, 'BULBEXP', MainFrm.BulbExposureMode ) ;

     iValue := Integer(MainFrm.PaletteType) ;
     iValue := GetKeyValue( Header, 'PAL', iValue ) ;
     MainFrm.PaletteType := TPaletteType(iValue) ;

     MainFrm.DisplayZoomIndex:= GetKeyValue( Header, 'DZOOM', MainFrm.DisplayZoomIndex ) ;

     MainFrm.NumFramesRequired := GetKeyValue( Header, 'NFREQ', MainFrm.NumFramesRequired ) ;

     MainFrm.RecordingMode := GetKeyValue( Header, 'RECMODE', MainFrm.RecordingMode ) ;
     MainFrm.RecordingPeriod := GetKeyValue( Header, 'RECPER', MainFrm.RecordingPeriod ) ;
     MainFrm.TimeLapseInterval := GetKeyValue( Header, 'TLAPINT', MainFrm.TimeLapseInterval ) ;
     MainFrm.BurstDuration := GetKeyValue( Header, 'BURDUR', MainFrm.BurstDuration ) ;
     MainFrm.BurstInterval := GetKeyValue( Header, 'BURINT', MainFrm.BurstInterval ) ;

     MainFrm.ADCRecordingTime := GetKeyValue( Header, 'ADCRECTIME', MainFrm.ADCRecordingTime ) ;

     MainFrm.ROISize := GetKeyValue( Header, 'ROISIZE', MainFrm.ROISize ) ;
     MainFrm.ROIX := GetKeyValue( Header, 'ROIX', MainFrm.ROIX ) ;
     MainFrm.ROIY := GetKeyValue( Header, 'ROIY', MainFrm.ROIY ) ;
     MainFrm.LiveWindowWavelength := GetKeyValue( Header, 'LIVEWVL', MainFrm.LiveWindowWavelength ) ;

     // National Instruments interface library
     iValue := 0 ;
     LabIO.NIDAQAPI := GetKeyValue( Header, 'NIDAQAPI', iValue ) ;

     // A/D input mode (NRSE/Differential)
     iValue := 0 ;
     LabIO.ADCInputMode := GetKeyValue( Header, 'ADCINPUTMODE', iValue ) ;

     // A/D channel settings
     MainFrm.ADCNumChannels := GetKeyValue( Header, 'ADCNC', MainFrm.ADCNumChannels  ) ;

     // A/D channel scanning interval
     MainFrm.ADCScanInterval := GetKeyValue( Header, 'ADCSI', MainFrm.ADCScanInterval ) ;

     MainFrm.ADCVoltageRange := GetKeyValue( Header, 'ADCVR', MainFrm.ADCVoltageRange ) ;

     MainFrm.ADCDisplayWindow := GetKeyValue( Header, 'ADCDW', MainFrm.ADCDisplayWindow ) ;

     for ch := 0 to MainFrm.ADCNumChannels-1 do
         begin
         // Get current channel settings
         ADCChannel := MainFrm.ADCChannel[ch] ;

         ADCChannel.ChannelOffset := MainFrm.IDRFile.ADCNumChannels - ch - 1 ;
         // Load parameters from INI file
         ADCChannel.ChannelOffset := GetKeyValue( Header, format('CIN%d',[ch]), ADCChannel.ChannelOffset ) ;

         ADCChannel.ADCUnits := GetKeyValue( Header, format('CU%d',[ch]), ADCChannel.ADCUnits ) ;
         ADCChannel.ADCName := GetKeyValue( Header, format('CN%d',[ch]), ADCChannel.ADCName ) ;
         ADCChannel.ADCCalibrationFactor := GetKeyValue( Header, format('CCF%d',[ch]), ADCChannel.ADCCalibrationFactor ) ;
         ADCChannel.ADCScale := GetKeyValue( Header, format('CSC%d',[ch]), ADCChannel.ADCScale) ;
         // Update channel
         MainFrm.ADCChannel[ch] := ADCChannel ;
         end ;

     // Patch clamp amplifier data

     // Patch clamp amplifier data
     for i := 1 to 2 do begin
         Amplifier.AmplifierType[i] := GetKeyValue( Header, format('AMP',[i]),Amplifier.AmplifierType[i] ) ;
         Amplifier.AmplifierType[i] := GetKeyValue( Header, format('AMP%d',[i]),Amplifier.AmplifierType[i] ) ;
         Amplifier.GainTelegraphChannel[i] := GetKeyValue( Header, format('AMPCH%d',[i]),Amplifier.GainTelegraphChannel[i] ) ;
         Amplifier.GainTelegraphChannel[i] := GetKeyValue( Header, format('AMPGAINCH%d',[i]),Amplifier.GainTelegraphChannel[i] ) ;
         Amplifier.ModeTelegraphChannel[i] := GetKeyValue( Header, format('AMPMODECH%d',[i]),Amplifier.ModeTelegraphChannel[i] ) ;
         end ;

     // Patch clamp command voltage divide factor
     MainFrm.VCommand[0].DivideFactor := GetKeyValue( Header, 'VCDIV', MainFrm.VCommand[0].DivideFactor ) ;
     // Patch clamp holding potential
      MainFrm.VCommand[0].HoldingVoltage := GetKeyValue( Header, 'VCHOLD', MainFrm.VCommand[0].HoldingVoltage ) ;

     MainFrm.VCommand[0].DivideFactor := GetKeyValue( Header, 'VCDIV0', MainFrm.VCommand[0].DivideFactor ) ;
     MainFrm.VCommand[0].HoldingVoltage := GetKeyValue( Header, 'VCHOLD0', MainFrm.VCommand[0].HoldingVoltage ) ;
     MainFrm.VCommand[1].DivideFactor := GetKeyValue( Header, 'VCDIV1', MainFrm.VCommand[1].DivideFactor ) ;
     MainFrm.VCommand[1].HoldingVoltage := GetKeyValue( Header, 'VCHOLD1', MainFrm.VCommand[1].HoldingVoltage ) ;
     MainFrm.VCommand[2].DivideFactor := GetKeyValue( Header, 'VCDIV2', MainFrm.VCommand[2].DivideFactor ) ;
     MainFrm.VCommand[2].HoldingVoltage:= GetKeyValue( Header, 'VCHOLD2', MainFrm.VCommand[2].HoldingVoltage ) ;

     // Default digital output port states
     for Dev := 1 to MaxDevices do begin
         LabIO.DigOutState[Dev] := GetKeyValue( Header, format('DIGOUTSTATE%d',[Dev]), LabIO.DigOutState[Dev] ) ;
         end ;

     // Light source
     LightSource.DeviceType := GetKeyValue( Header, 'LSDEV', LightSource.DeviceType ) ;
     LightSource.Wavelength1 := GetKeyValue( Header, 'LSW1', LightSource.Wavelength1 ) ;
     LightSource.Voltage1 := GetKeyValue( Header, 'LSV1', LightSource.Voltage1 ) ;
     LightSource.Wavelength2 := GetKeyValue( Header, 'LSW2', LightSource.Wavelength2 ) ;
     LightSource.Voltage2 := GetKeyValue( Header, 'LSV2', LightSource.Voltage2 ) ;

     // Laser settings
     for i := 0 to lsMaxLightSources-1 do begin
        LightSource.LaserWavelength[i] := GetKeyValue( Header, format('LSLAS%dWAV',[i+1]), LightSource.LaserWavelength[i] ) ;
        LightSource.LaserDelay[i] := GetKeyValue( Header, format('LSLAS%dDEL',[i+1]), LightSource.LaserDelay[i] ) ;
        LightSource.LaserOffVoltage[i] := GetKeyValue( Header, format('LSLAS%dVOFF',[i+1]), LightSource.LaserOffVoltage[i] ) ;
        LightSource.LaserOnVoltage[i] := GetKeyValue( Header, format('LSLAS%dVON',[i+1]), LightSource.LaserOnVoltage[i] ) ;
        LightSource.LaserIntensity[i] := GetKeyValue( Header, format('LSLAS%dINT',[i+1]), LightSource.LaserIntensity[i] ) ;
        end ;

     // LED settings
     LightSource.LEDOffVoltage  := GetKeyValue( Header, 'LSLEDOFFV', LightSource.LEDOffVoltage ) ;
     LightSource.LEDMaxVoltage := GetKeyValue( Header, 'LSLEDMAXV', LightSource.LEDMaxVoltage ) ;

     // TIRF settings
     for i := 1 to lsMaxTIRFGalvos do begin
        LightSource.TIRFOff[i] := GetKeyValue( Header, format('LSTIRFOFF%d',[i]), LightSource.TIRFOff[i] ) ;
        LightSource.TIRFOn[i] := GetKeyValue( Header, format('LSTIRFON%d',[i]), LightSource.TIRFOn[i] ) ;
        LightSource.TIRFWF[i] := GetKeyValue( Header, format('LSTIRFWF%d',[i]), LightSource.TIRFWF[i] ) ;
        end ;

     // Shutter closed wavelength
     LightSource.ShutterClosedWavelength := GetKeyValue( Header, 'LSSCWAVEL', LightSource.ShutterClosedWavelength ) ;

     // Shutter blanking period
     LightSource.ShutterBlankingPeriod := GetKeyValue( Header, 'LSSCBLANK', LightSource.ShutterBlankingPeriod ) ;

     // Shutter open/close change time
     LightSource.ShutterChangeTime := GetKeyValue( Header, 'LSSCHTIME', LightSource.ShutterChangeTime ) ;

     // Emission filter
     LightSource.EMFilterChangeTime := GetKeyValue( Header, 'EMFCHTIME', LightSource.EMFilterChangeTime ) ;

     // Excitation wavelength settings
     MainFrm.EXCSingleWavelength := GetKeyValue( Header, 'EXCSW', MainFrm.EXCSingleWavelength ) ;

     MainFrm.ExcitationOnWhenRecording := GetKeyValue( Header, 'EXCONREC', MainFrm.ExcitationOnWhenRecording ) ;

     MainFrm.EXCSingleWavelengthNum:= GetKeyValue( Header, 'EXCSWN', MainFrm.EXCSingleWavelengthNum ) ;

     // Read sequence 0 (for compatibility with older versions
      MainFrm.EXCNumWavelengths[0] := GetKeyValue( Header, 'EXCNW', MainFrm.EXCNumWavelengths[0] ) ;
     for iWav := 0 to MainFrm.EXCNumWavelengths[0] do
         begin
         MainFrm.EXCSequence[iWav,0].WavelengthNum:= GetKeyValue( Header, format('EXCSEQ%d',[iWav]), MainFrm.EXCSequence[iWav,0].WavelengthNum) ;
         MainFrm.EXCSequence[iWav,0].DivideFactor := 1 ;
         MainFrm.EXCSequence[iWav,0].DivideFactor := GetKeyValue( Header, format('EXCSEQDF%d',[iWav]), MainFrm.EXCSequence[iWav,0].DivideFactor) ;
         MainFrm.EXCWavelengths[iWav].EmFilter := GetKeyValue( Header, format('EXCWEMF%d',[iWav]), MainFrm.EXCWavelengths[iWav].EmFilter) ;
         MainFrm.EXCWavelengths[iWav].EmName := GetKeyValue( Header, format('EXCWEMN%d',[iWav]), MainFrm.EXCWavelengths[iWav].EmName) ;
         MainFrm.EXCWavelengths[iWav].FractionalExposure := GetKeyValue( Header, format('EXCWFEX%d',[iWav]), MainFrm.EXCWavelengths[iWav].FractionalExposure) ;
         end ;

     // Excitation multi-wavelength sequence settings
     MainFrm.EXCSequenceNum := 0 ;
     MainFrm.EXCSequenceNum := GetKeyValue( Header, 'EXCSEQNUM', MainFrm.EXCSequenceNum ) ;
     for iSeq := 0 to MaxEXCSequences-1 do
         begin
         MainFrm.EXCNumWavelengths[iSeq] := GetKeyValue( Header, format('EXCNW%d',[iSeq]), MainFrm.EXCNumWavelengths[iSeq] ) ;
         MainFrm.EXCSequenceName[iSeq] := GetKeyValue( Header, format('EXCSEQNAM%d',[iSeq]), MainFrm.EXCSequenceName[iSeq] ) ;
         for iWav := 0 to MainFrm.EXCNumWavelengths[iSeq] do
             begin
             MainFrm.EXCSequence[iWav,iSeq].WavelengthNum := GetKeyValue( Header, format('EXCSEQ%dW%d',[iSeq,iWav]), MainFrm.EXCSequence[iWav,iSeq].WavelengthNum) ;
             MainFrm.EXCSequence[iWav,iSeq].DivideFactor := 1 ;
             MainFrm.EXCSequence[iWav,iSeq].DivideFactor := GetKeyValue( Header, format('EXCSEQ%dDF%d',[iSeq,iWav]), MainFrm.EXCSequence[iWav,iSeq].DivideFactor) ;
             end ;
         end ;

     // Read wavelengths table
     for i := 0 to High(MainFrm.EXCWavelengths) do begin
         MainFrm.EXCWavelengths[i].Centre := GetKeyValue( Header, format('EXCWC%d',[i]), MainFrm.EXCWavelengths[i].Centre) ;
         MainFrm.EXCWavelengths[i].Width := GetKeyValue( Header, format('EXCWW%d',[i]), MainFrm.EXCWavelengths[i].Width) ;
         end ;

     MainFrm.EXCSpectrumStartWavelength := GetKeyValue( Header, 'EXCSPSTARTW', MainFrm.EXCSpectrumStartWavelength ) ;
     MainFrm.EXCSpectrumEndWavelength := GetKeyValue( Header, 'EXCSPENDW', MainFrm.EXCSpectrumEndWavelength ) ;
     MainFrm.EXCSpectrumBandwidth := GetKeyValue( Header, 'EXCSBANDW', MainFrm.EXCSpectrumBandwidth ) ;
     MainFrm.EXCSpectrumEMFilter := GetKeyValue( Header, 'EXCSPEMFILT', MainFrm.EXCSpectrumEMFilter ) ;
     MainFrm.EXCSpectrumStepSize := GetKeyValue( Header, 'EXCSPSTEPS', MainFrm.EXCSpectrumStepSize ) ;

     // Stimulus program file
     MainFrm.StimFileName := GetKeyValue( Header, 'STIMFILE', MainFrm.StimFileName) ;

     // Photo stimulus program file
     MainFrm.PhotoStimFileName := GetKeyValue( Header, 'PHOTOSTIMFIL', MainFrm.PhotoStimFileName) ;

     // Fluophore binding equations table
     for i := 0 to MainFrm.IDRFile.MaxEquations-1 do
         begin
         mainFrm.BindingEquations[i].InUse := GetKeyValue( Header, format('EQNUSE%d',[i]), mainFrm.BindingEquations[i].InUse ) ;
         mainFrm.BindingEquations[i].Ion := GetKeyValue( Header, format('EQNION%d',[i]), mainFrm.BindingEquations[i].Ion) ;
          mainFrm.BindingEquations[i].Units := GetKeyValue( Header, format('EQNUN%d',[i]), mainFrm.BindingEquations[i].Units) ;
         mainFrm.BindingEquations[i].Name := GetKeyValue( Header, format('EQNNAM%d',[i]), mainFrm.BindingEquations[i].Name) ;
         mainFrm.BindingEquations[i].RMax := GetKeyValue( Header, format('EQNRMAX%d',[i]), mainFrm.BindingEquations[i].RMax) ;
         mainFrm.BindingEquations[i].RMin := GetKeyValue( Header, format('EQNRMIN%d',[i]), mainFrm.BindingEquations[i].RMin) ;
         mainFrm.BindingEquations[i].KEff := GetKeyValue( Header, format('EQNKEFF%d',[i]), mainFrm.BindingEquations[i].KEff) ;
         end ;

     // Printer page settings
     MainFrm.PrinterTopMargin := GetKeyValue( Header, 'PRTM', MainFrm.PrinterTopMargin )  ;
     MainFrm.PrinterBottomMargin := GetKeyValue( Header, 'PRBM', MainFrm.PrinterBottomMargin )  ;
     MainFrm.PrinterLeftMargin := GetKeyValue( Header, 'PRLM', MainFrm.PrinterLeftMargin )  ;
     MainFrm.PrinterRightMargin := GetKeyValue( Header, 'PRRM', MainFrm.PrinterRightMargin )  ;
     MainFrm.PrinterLineThickness := GetKeyValue( Header, 'PRLT', MainFrm.PrinterLineThickness )  ;
     MainFrm.PrinterMarkerSize := GetKeyValue( Header, 'PRMS', MainFrm.PrinterMarkerSize )  ;
     MainFrm.PrinterUseColor := GetKeyValue( Header, 'PRUC', MainFrm.PrinterUseColor ) ;
     MainFrm.PrinterFontName := GetKeyValue( Header, 'PRFN', MainFrm.PrinterFontName ) ;
     MainFrm.PrinterFontSize := GetKeyValue( Header, 'PRFS', MainFrm.PrinterFontSize )  ;

     MainFrm.DataDirectory := GetKeyValue( Header, 'DDIR', MainFrm.DataDirectory ) ;

     MainFrm.VProtDirectory := GetKeyValue( Header, 'VPDIR', MainFrm.VProtDirectory ) ;
     // Use default if voltage protocol directory does not exist
     if not DirectoryExists(MainFrm.VProtDirectory) then begin
        MainFrm.VProtDirectory := MainFrm.DefVProtDirectory ;
        end ;

     MainFrm.ExportDirectory := GetKeyValue( Header, 'EXPDIR', MainFrm.ExportDirectory ) ;

     for i := 0 to High(MainFrm.RecentFiles) do
         MainFrm.RecentFiles[i] := GetKeyValue(Header,format('FILE%d',[i]),MainFrm.RecentFiles[i]) ;

     // Input/output/control line configuration
     MainFrm.IOConfig.ADCIn := GetKeyValue( Header, 'IOADCI', MainFrm.IOConfig.ADCIn ) ;
     MainFrm.IOConfig.CameraStart := GetKeyValue( Header, 'IOCAMS', MainFrm.IOConfig.CameraStart ) ;
     MainFrm.IOConfig.CameraStartActiveHigh := GetKeyValue( Header, 'IOCAMSAH', MainFrm.IOConfig.CameraStartActiveHigh ) ;

     // Command voltage O/P lines 1 & 2
     MainFrm.IOConfig.VCommand[0] := GetKeyValue( Header, 'IOVCOM', MainFrm.IOConfig.VCommand[0] ) ;
     // Above line for compatibility with versions earlier than V2.4.3
     MainFrm.IOConfig.VCommand[0] := GetKeyValue( Header, 'IOVCOM0', MainFrm.IOConfig.VCommand[0] ) ;
     MainFrm.IOConfig.VCommand[1] := GetKeyValue( Header, 'IOVCOM1', MainFrm.IOConfig.VCommand[1] ) ;
     MainFrm.IOConfig.VCommand[2] := GetKeyValue( Header, 'IOVCOM2', MainFrm.IOConfig.VCommand[2] ) ;

     MainFrm.IOConfig.LSShutter := GetKeyValue( Header, 'IOLSSU', MainFrm.IOConfig.LSShutter ) ;
     MainFrm.IOConfig.LSShutterActiveHigh := GetKeyValue( Header, 'IOLSSUAH', MainFrm.IOConfig.LSShutterActiveHigh ) ;

     // Read pre V3.7.2 light source control lines info
     iStart := 0 ;
     iEnd := 0 ;
     iStart := GetKeyValue( Header, 'IOLSWS', iStart ) ;
     iEnd := GetKeyValue( Header, 'IOLSWE', iEnd ) ;
     iLine := 0 ;
     for i := iStart to iEnd do if iLine <= MaxLSControlLine then begin
         MainFrm.IOConfig.LSControlLine[iLine] := i ;
         Inc(iLine) ;
         end;
     iStart := GetKeyValue( Header, 'IOLSLS', iStart ) ;
     iEnd := GetKeyValue( Header, 'IOLSLE', iEnd ) ;
     for i := iStart to iEnd do if iLine <= MaxLSControlLine then begin
         MainFrm.IOConfig.LSControlLine[iLine] := i ;
         Inc(iLine) ;
         end;

     // Light source control outputs (post V3.7.2)
     for i := 0 to MaxLSControlLine do begin
        MainFrm.IOConfig.LSControlLine[i] := GetKeyValue( Header, format('IOLSCON%d',[i]), MainFrm.IOConfig.LSControlLine[i] ) ;
        end ;

     // Emission filter control lines
     MainFrm.IOConfig.EMFilterStart := GetKeyValue( Header, 'IOEMFS', MainFrm.IOConfig.EMFilterStart ) ;
     MainFrm.IOConfig.EMFilterEnd  := GetKeyValue( Header, 'IOEMFE', MainFrm.IOConfig.EMFilterEnd ) ;

     MainFrm.IOConfig.DigitalStimStart := GetKeyValue( Header, 'IODSTA', MainFrm.IOConfig.DigitalStimStart ) ;
     MainFrm.IOConfig.DigitalStimEnd := GetKeyValue( Header, 'IODEND', MainFrm.IOConfig.DigitalStimEnd ) ;
     MainFrm.IOConfig.PhotoStimX := GetKeyValue( Header, 'IOPSX', MainFrm.IOConfig.PhotoStimX ) ;
     MainFrm.IOConfig.PhotoStimY := GetKeyValue( Header, 'IOPSY', MainFrm.IOConfig.PhotoStimY ) ;
     MainFrm.IOConfig.PhotoStimI1 := GetKeyValue( Header, 'IOPSI1', MainFrm.IOConfig.PhotoStimI1 ) ;
     MainFrm.IOConfig.PhotoStimI2 := GetKeyValue( Header, 'IOPSI2', MainFrm.IOConfig.PhotoStimI2 ) ;
     MainFrm.IOConfig.PhotoStimI3 := GetKeyValue( Header, 'IOPSI3', MainFrm.IOConfig.PhotoStimI3 ) ;

     // Photo-stimulus Pockels cell and shutter configuration
     MainFrm.IOConfig.PhotoStimMeter := GetKeyValue( Header, 'IOPSMETER', MainFrm.IOConfig.PhotoStimMeter ) ;
     MainFrm.IOConfig.PhotoStimShutter := GetKeyValue( Header, 'IOPSSU', MainFrm.IOConfig.PhotoStimShutter ) ;
     MainFrm.IOConfig.PhotoStimShutterLatency := GetKeyValue( Header, 'IOPSSLA', MainFrm.IOConfig.PhotoStimShutterLatency ) ;
     MainFrm.IOConfig.PhotoStimShutterActiveHigh := GetKeyValue( Header, 'IOPSSUAH', MainFrm.IOConfig.PhotoStimShutterActiveHigh ) ;
     MainFrm.IOconfig.PhotoStimPowerCalManual:= GetKeyValue( Header, 'IOPSPCMAN', MainFrm.IOconfig.PhotoStimPowerCalManual ) ;
     MainFrm.IOConfig.PhotoStimMeterRange := GetKeyValue( Header, 'IOPSMETERR', MainFrm.IOConfig.PhotoStimMeterRange ) ;
     MainFrm.IOconfig.PhotoStimMeterScale := GetKeyValue( Header, 'IOPSMETERS', MainFrm.IOconfig.PhotoStimMeterScale ) ;

     MainFrm.IOConfig.ClockSyncLine := GetKeyValue( Header, 'IOCLKSYNC', MainFrm.IOConfig.ClockSyncLine ) ;


     // Event detection settings
{     := GetKeyValue( Header, 'EVANDEADTIME', MainFrm.EventAnalysis.DeadTime ) ;
     := GetKeyValue( Header, 'EVANTHRESHOLD', MainFrm.EventAnalysis.DetectionThreshold ) ;
     := GetKeyValue( Header, 'EVANTHRESHDUR', MainFrm.EventAnalysis.ThresholdDuration ) ;
     := GetKeyValue( Header, 'EVANPOLARITY', MainFrm.EventAnalysis.DetectionThresholdPolarity ) ;
     := GetKeyValue( Header, 'EVANFIXBASE', MainFrm.EventAnalysis.FixedBaseline ) ;
     := GetKeyValue( Header, 'EVANSOURCE', MainFrm.EventAnalysis.DetectionSource ) ;
     := GetKeyValue( Header, 'EVANDISPLAYMAX', MainFrm.EventAnalysis.DisplayMax ) ;
     := GetKeyValue( Header, 'EVANROLLBASEPERIOD', MainFrm.EventAnalysis.RollingBaselinePeriod ) ;}

     // Seal test settings
     MainFrm.SealTest.Use := GetKeyValue( Header, 'SEALTUSE',MainFrm.SealTest.Use ) ;
     MainFrm.SealTest.PulseHeight1 := GetKeyValue( Header, 'SEALTPH1',MainFrm.SealTest.PulseHeight1 ) ;
     MainFrm.SealTest.HoldingVoltage1 := GetKeyValue( Header, 'SEALTHV1',MainFrm.SealTest.HoldingVoltage1 ) ;
     MainFrm.SealTest.PulseHeight2 := GetKeyValue( Header, 'SEALTPH2',MainFrm.SealTest.PulseHeight2 ) ;
     MainFrm.SealTest.HoldingVoltage2 := GetKeyValue( Header, 'SEALTHV2',MainFrm.SealTest.HoldingVoltage2 ) ;
     MainFrm.SealTest.PulseHeight3 := GetKeyValue( Header, 'SEALTPH3',MainFrm.SealTest.PulseHeight3 ) ;
     MainFrm.SealTest.HoldingVoltage3 := GetKeyValue( Header, 'SEALTHV3',MainFrm.SealTest.HoldingVoltage3 ) ;
     MainFrm.SealTest.PulseWidth := GetKeyValue( Header, 'SEALTPW',MainFrm.SealTest.PulseWidth ) ;

     // Photo-stimulus settings
     MainFrm.PhotoStim.Period := GetKeyValue( Header, 'PSPER',MainFrm.PhotoStim.Period ) ;
     MainFrm.PhotoStim.RepeatedStim := GetKeyValue( Header, 'PSREP',MainFrm.PhotoStim.RepeatedStim ) ;
     MainFrm.PhotoStim.NumStimPoints := GetKeyValue( Header, 'PSNSTIMP',MainFrm.PhotoStim.NumStimPoints ) ;
     MainFrm.PhotoStim.Attenuator := GetKeyValue( Header, 'PSATTEN',MainFrm.PhotoStim.Attenuator ) ;
     MainFrm.PhotoStim.ImageRotation := GetKeyValue( Header, 'PSROTAT',MainFrm.PhotoStim.ImageRotation ) ;
     MainFrm.PhotoStim.XMicronsPerPixel := GetKeyValue( Header, 'PSMICPERPX',MainFrm.PhotoStim.XMicronsPerPixel ) ;
     MainFrm.PhotoStim.YMicronsPerPixel  := GetKeyValue( Header, 'PSMICPERPY',MainFrm.PhotoStim.YMicronsPerPixel ) ;
     MainFrm.PhotoStim.PVLogFile := GetKeyValue( Header, 'PSPVLOG',MainFrm.PhotoStim.PVLogFile ) ;
     MainFrm.PhotoStim.RefLineEnabled := GetKeyValue( Header, 'PSREFLE',MainFrm.PhotoStim.RefLineEnabled ) ;
     MainFrm.PhotoStim.CmdTermZero := GetKeyValue( Header, 'PSCMDTERM',MainFrm.PhotoStim.CmdTermZero ) ;

     for i := 1 to 3 do
     begin
       MainFrm.PhotoStim.XCenter[i] := GetKeyValue( Header, format('PSXC%d',[i]), MainFrm.PhotoStim.XCenter[i] ) ;
        MainFrm.PhotoStim.XScale[i] := GetKeyValue( Header, format('PSXS%d',[i]), MainFrm.PhotoStim.XScale[i] ) ;
       MainFrm.PhotoStim.YCenter[i] := GetKeyValue( Header, format('PSYC%d',[i]), MainFrm.PhotoStim.YCenter[i] ) ;
       MainFrm.PhotoStim.YScale[i] := GetKeyValue( Header, format('PSYS%d',[i]), MainFrm.PhotoStim.YScale[i] ) ;
       MainFrm.PhotoStim.PCEnable[i] := GetKeyValue( Header, format('PSPCPENA%d',[i]), MainFrm.PhotoStim.PCEnable[i] ) ;
       MainFrm.PhotoStim.PCPowerMin[i] := GetKeyValue( Header, format('PSPCPMIN%d',[i]), MainFrm.PhotoStim.PCPowerMin[i] ) ;
       MainFrm.PhotoStim.PCPowerMax[i] := GetKeyValue( Header, format('PSPCPMAX%d',[i]), MainFrm.PhotoStim.PCPowerMax[i] ) ;
       MainFrm.PhotoStim.PCBias[i] := GetKeyValue( Header, format('PSPCBIAS%d',[i]), MainFrm.PhotoStim.PCBias[i] ) ;
       MainFrm.PhotoStim.PCVoltagePi[i] := GetKeyValue( Header, format('PSPCVPI%d',[i]), MainFrm.PhotoStim.PCVoltagePi[i] ) ;
       MainFrm.PhotoStim.PCPolarizationCross[i] := GetKeyValue( Header, format('PSPCPC%d',[i]), MainFrm.PhotoStim.PCPolarizationCross[i] ) ;
       MainFrm.PhotoStim.PCConoptics302[i] := GetKeyValue( Header, format('PSPC302%d',[i]), MainFrm.PhotoStim.PCConoptics302[i] ) ;
       MainFrm.PhotoStim.LinearPowerMin[i] := GetKeyValue( Header, format('PSLPMIN%d',[i]), MainFrm.PhotoStim.LinearPowerMin[i] ) ;
       MainFrm.PhotoStim.LinearPowerMax[i] := GetKeyValue( Header, format('PSLPMAX%d',[i]), MainFrm.PhotoStim.LinearPowerMax[i] ) ;
       MainFrm.PhotoStim.LinearVoltageMin[i] := GetKeyValue( Header, format('PSLVMIN%d',[i]), MainFrm.PhotoStim.LinearVoltageMin[i] ) ;
       MainFrm.PhotoStim.LinearVoltageMax[i] := GetKeyValue( Header, format('PSLVMAX%d',[i]), MainFrm.PhotoStim.LinearVoltageMax[i] );
       MainFrm.PhotoStim.EnableShutter[i] := GetKeyValue( Header, format('PSPCSHU%d',[i]), MainFrm.PhotoStim.EnableShutter[i] ) ;
     end;

     MainFrm.mnDisplayGrid.Checked := GetKeyValue( Header, 'DISPLAYGRID', MainFrm.mnDisplayGrid.Checked ) ;

     // Display contrast settings optimisation
     MainFrm.ContrastChangeAllFrameTypes := GetKeyValue( Header, 'CNCAFT', MainFrm.ContrastChangeAllFrameTypes ) ;
     MainFrm.ContrastAutoOptimise := GetKeyValue( Header, 'CNAUTOOP', MainFrm.ContrastAutoOptimise ) ;
     MainFrm.Contrast6SD := GetKeyValue( Header, 'CN6SD', MainFrm.Contrast6SD ) ;

     // Read visibility state of channels in RecADCOnlyUnit
     // Modified by NS 19 March 2009
     for ch := 0 to MainFrm.ADCNumChannels-1 do
        begin
        MainFrm.ADCChannelRecADCOnlyUnitVisible[ch] := GetKeyValue( Header, format('CVRADCO%d',[ch]), MainFrm.ADCChannelRecADCOnlyUnitVisible[ch]) ;
     end ;

     // Read visibility state of channels in Sealtest
     // Modified by NS 24 March 2009
     MainFrm.ADCChannelSealtestNumberOfChannels := 0 ;
     MainFrm.ADCChannelSealtestNumberOfChannels := GetKeyValue( Header, 'CVSTNUM', MainFrm.ADCChannelSealtestNumberOfChannels ) ;
     for ch := 0 to MainFrm.ADCChannelSealtestNumberOfChannels-1 do
        begin
        MainFrm.ADCChannelSealtestVisible[ch] := GetKeyValue( Header, format('CVST%d',[ch]), MainFrm.ADCChannelSealtestVisible[ch] ) ;
     end ;

     // Read state of SmoothDifferentiate window
     // Modified by NS 10 April 2009
     for ch := 0 to (4 - 1) do
        begin
        MainFrm.SmoothDifferentiateUnitVisible[ch] := GetKeyValue( Header, format('SDWCH%d',[ch]), MainFrm.SmoothDifferentiateUnitVisible[ch] ) ;
     end;
     MainFrm.SmoothDifferentiateUnitMADataWindow := GetKeyValue( Header, 'SDWMA', MainFrm.SmoothDifferentiateUnitMADataWindow ) ;
     MainFrm.SmoothDifferentiateUnitMADXWindow := GetKeyValue( Header, 'SDWDX', MainFrm.SmoothDifferentiateUnitMADXWindow ) ;

     // Read state of DynamicProtocol window
     // Modified by NS 22 December 2009
     MainFrm.DynamicProtocol.SelectedChannel := GetKeyValue( Header, 'DPCH', MainFrm.DynamicProtocol.SelectedChannel ) ;
     MainFrm.DynamicProtocol.Direction := GetKeyValue( Header, 'DPDIR', MainFrm.DynamicProtocol.Direction ) ;
     MainFrm.DynamicProtocol.Threshold := GetKeyValue( Header, 'DPTHRES', MainFrm.DynamicProtocol.Threshold ) ;
     MainFrm.DynamicProtocol.Duration := GetKeyValue( Header, 'DPDUR', MainFrm.DynamicProtocol.Duration ) ;
     MainFrm.DynamicProtocol.EPRestart := GetKeyValue( Header, 'DPEP', MainFrm.DynamicProtocol.EPRestart ) ;
     MainFrm.DynamicProtocol.PSRestart := GetKeyValue( Header, 'DPPS', MainFrm.DynamicProtocol.PSRestart ) ;
     MainFrm.DynamicProtocol.EPStimFileName := GetKeyValue( Header, 'DPEPF',MainFrm.DynamicProtocol.EPStimFileName ) ;
     MainFrm.DynamicProtocol.EPStimIndex := GetKeyValue( Header, 'DPEPI', MainFrm.DynamicProtocol.EPStimIndex ) ;
     MainFrm.DynamicProtocol.PSStimFileName := GetKeyValue( Header, 'DPPSF',MainFrm.DynamicProtocol.PSStimFileName ) ;
     MainFrm.DynamicProtocol.PSStimIndex:= GetKeyValue( Header, 'DPPSI', MainFrm.DynamicProtocol.PSStimIndex ) ;

     // Auto reset interface cards
     MainFrm.AutoResetInterfaceCards := GetKeyValue( Header, 'ARI', MainFrm.AutoResetInterfaceCards ) ;

     MainFrm.StartStimOnRecord := GetKeyValue( Header, 'STARTSTIMONREC', MainFrm.StartStimOnRecord ) ;

     // Camera dark level detection range
     MainFrm.DarkLevelLo := GetKeyValue( Header, 'DARKLEVLO',MainFrm.DarkLevelLo ) ;
     MainFrm.DarkLevelHi := GetKeyValue( Header, 'DARKLEVHI',MainFrm.DarkLevelHi ) ;

     // Read Z Stage control settings
     ZStage.ReadSettings( Header ) ;

     // Read XY Stage control settings
     XYStageFrm.ReadSettings( Header ) ;

     // Read PMT Ratio calculation settings
     MainFrm.PMTRatio.Enabled := GetKeyValue( Header, 'PMTRATIOEN',MainFrm.PMTRatio.Enabled ) ;
     MainFrm.PMTRatio.NumerChan := GetKeyValue( Header, 'PMTRATIONUM',MainFrm.PMTRatio.NumerChan ) ;
     MainFrm.PMTRatio.DenomChan := GetKeyValue( Header, 'PMTRATIODEN',MainFrm.PMTRatio.DenomChan ) ;
     MainFrm.PMTRatio.RatioChan := GetKeyValue( Header, 'PMTRATIORAT',MainFrm.PMTRatio.RatioChan ) ;
     MainFrm.PMTRatio.ConcChan := GetKeyValue( Header, 'PMTRATIOCON',MainFrm.PMTRatio.ConcChan ) ;
     MainFrm.PMTRatio.Threshold := GetKeyValue( Header, 'PMTRATIOTHR',MainFrm.PMTRatio.Threshold ) ;
     MainFrm.PMTRatio.RatioMax := GetKeyValue( Header, 'PMTRATIORATMAX',MainFrm.PMTRatio.RatioMax ) ;
     MainFrm.PMTRatio.ConcEnabled := GetKeyValue( Header, 'PMTRATIOCEN',MainFrm.PMTRatio.ConcEnabled ) ;
     MainFrm.PMTRatio.ConcMax := GetKeyValue( Header, 'PMTRATIOCONCMAX',MainFrm.PMTRatio.ConcMax ) ;
     MainFrm.PMTRatio.IonName := GetKeyValue( Header, 'PMTRATIOINM',MainFrm.PMTRatio.IonName ) ;
     MainFrm.PMTRatio.ConcUnits := GetKeyValue( Header, 'PMTRATIOUNI',MainFrm.PMTRatio.ConcUnits ) ;
     MainFrm.PMTRatio.RMax := GetKeyValue( Header, 'PMTRATIORMAX',MainFrm.PMTRatio.RMax ) ;
     MainFrm.PMTRatio.RMin := GetKeyValue( Header, 'PMTRATIORMIN',MainFrm.PMTRatio.RMin ) ;
     MainFrm.PMTRatio.Keff := GetKeyValue( Header, 'PMTRATIOKEFF',MainFrm.PMTRatio.Keff ) ;

     for I := 0 to High(MainFrm.FormPos) do
         begin
         MainFrm.FormPos[i].Top := GetKeyValue( Header, format('FPTOP%d',[i]), MainFrm.FormPos[i].Top ) ;
         MainFrm.FormPos[i].Left := GetKeyValue( Header, format('FPLEFT%d',[i]), MainFrm.FormPos[i].Left) ;
         MainFrm.FormPos[i].Width := GetKeyValue( Header, format('FPWIDTH%d',[i]), MainFrm.FormPos[i].Width ) ;
         MainFrm.FormPos[i].Height := GetKeyValue( Header, format('FPHEIGHT%d',[i]), MainFrm.FormPos[i].Height ) ;
         end;

     // Dispose of list
     Header.Free ;

     end ;


procedure TFileIO.ImportFile ;
// --------------------------------------------
// Import images from another image file format
// --------------------------------------------
var
    PFrameBuf : Pointer ; // Image frame buffer pointer
    ImportFileName : String ;
    IDRFileName : String ; // Name of .IDR file to hold imported images
    FileNum : Integer ;    // Index into OpenDialog.Files list
    iFrame : Integer ;     // Frame counter
    NumFramesImported : Integer ;
begin


    // Setup Open Files dialog box
    OpenDialog.options := [ofPathMustExist,ofAllowMultiSelect] ;

    // Open last used data directory
     if DirectoryExists(MainFrm.DataDirectory) and
        (MainFrm.DataDirectory <> '') then begin
        OpenDialog.InitialDir := MainFrm.DataDirectory ;
        end
     else OpenDialog.InitialDir := MainFrm.DefaultDataDirectory ;

    OpenDialog.Title := 'Import File ' ;

    // Exit if no file name available
    if (not OpenDialog.Execute) or (OpenDialog.Files.Count <= 0) then Exit ;

    // Import frames from all files in list
    NumFramesImported := 0 ;
    for FileNum := 0 to OpenDialog.Files.Count-1 do begin

        // Open file
        ImportFileName := OpenDialog.Files[FileNum] ;
        if not ImageFile.OpenFile( ImportFileName ) then begin
           MainFrm.StatusBar.SimpleText := 'Unable to open : ' + ImportFileName ;
           Exit ;
           end ;

        // Create IDR file to hold images
        if FileNum = 0 then begin
           IDRFileName := ChangeFileExt( ImportFileName, '.IDR' ) ;
           if FileExists(IDRFileName) then begin
              if MessageDlg( 'File ' + IDRFileName +' already exists! Overwrite it?',
                 mtWarning,[mbYes,mbNo], 0 ) = mrYes then begin
                 DeleteFile(IDRFileName) ;
                 end
              else begin
                 // Present user with standard Save File dialog box
                 MainFrm.SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
                 MainFrm.SaveDialog.DefaultExt := DataFileExtension ;
                 MainFrm.SaveDialog.FileName := '' ;
                 MainFrm.SaveDialog.Filter := format(' %s Files (*.%s)|*.%s',
                             [DataFileExtension,DataFileExtension,DataFileExtension]);
                 MainFrm.SaveDialog.Title := 'Name for imported file' ;
                 MainFrm.SaveDialog.InitialDir := OpenDialog.InitialDir ;
                 if MainFrm.SaveDialog.execute then begin
                    IDRFileName := MainFrm.SaveDialog.FileName ;
                    end
                 else begin
                    ImageFile.CloseFile ;
                    Exit ;
                    end ;
                 end ;
              end ;

           // Create empty IDR data file to hold imported images
           MainFrm.IDRFile.CreateNewFile( IDRFileName) ;

           // Get image properties from import file
           MainFrm.IDRFile.FrameWidth := ImageFile.FrameWidth ;
           MainFrm.IDRFile.FrameHeight := ImageFile.FrameHeight ;
           MainFrm.IDRFile.PixelDepth := ImageFile.PixelDepth ;
           MainFrm.IDRFile.XResolution := ImageFile.XResolution ;
           MainFrm.IDRFile.ResolutionUnits := ImageFile.ResolutionUnit ;

           MainFrm.IDRFile.NumFrameTypes := ImageFile.ComponentsPerPixel ;
           MainFrm.IDRFile.FrameInterval := ImageFile.TResolution ;
           MainFrm.IDRFile.ADCScanInterval := 1.0 ;
           MainFrm.IDRFile.ADCNumChannels := 0 ;

           // Allocate frame buffer
           GetMem( PFrameBuf,MainFrm.IDRFile.NumBytesPerFrame ) ;
           end
        else begin
           // Terminate import if frame size changed
           if (MainFrm.IDRFile.FrameWidth <> ImageFile.FrameWidth) or
              (MainFrm.IDRFile.FrameHeight <> ImageFile.FrameHeight) or
              (MainFrm.IDRFile.PixelDepth <> ImageFile.PixelDepth) then begin
              MainFrm.StatusBar.SimpleText := 'Import: Aborted at ' + ImportFileName ;
              ImageFile.CloseFile ;
              Break ;
              end ;
           end ;

        // Import frames from this file
        for iFrame := 1 to ImageFile.NumFrames do begin
           if ImageFile.LoadFrame( iFrame, PFrameBuf ) then begin
              Inc(NumFramesImported) ;
              MainFrm.IDRFile.SaveFrame( NumFramesImported, PFrameBuf ) ;
              MainFrm.StatusBar.SimpleText := format(
              'Importing %d/%d frames from %s',
              [iFrame,ImageFile.NumFrames,ImportFileName]) ;
              Application.ProcessMessages ;
              end ;
           end ;

        // Close this import file
        ImageFile.CloseFile ;

        end ;

    // Display in program title bar
    MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;
    if MainFrm.IDRFile.NumFrames > 0 then MainFrm.mnViewImages.Click ;

{    edPixelWidth.Value := ImageFile.XResolution ;
    edPixelWidth.Units := ImageFile.ResolutionUnit ;
    edPixelUnits.Text := ImageFile.ResolutionUnit ;
    edPixelAspectRatio.Value := ImageFile.YResolution / ImageFile.XResolution ;
    edCalBarWidth.Value := 0.2*FrameWidth*edPixelWidth.Value ;
    edCalBarWidth.Units := ImageFile.ResolutionUnit ;}

     FreeMem( PFrameBuf ) ;
     end ;


procedure TFileIO.ExportFile ;
// --------------------------------------------
// Export images to another image file format
// --------------------------------------------
var
    FrameNum : Integer ; // Frame counter
    PFrameBuf : Pointer ; // Image frame buffer pointer
    Done : Boolean ;
    OK : Boolean ;
begin

     // Open last used data directory
     if DirectoryExists(MainFrm.DataDirectory) and (MainFrm.DataDirectory <> '') then begin
        SaveDialog.InitialDir := MainFrm.DataDirectory ;
        end
     else SaveDialog.InitialDir := MainFrm.DefaultDataDirectory ;

     SaveDialog.Title := 'Export to File' ;
     SaveDialog.FileName := ChangeFileExt( MainFrm.IDRFile.FileName,
                                           FileTypeExts[ SaveDialog.FilterIndex] ) ;

     Done := not SaveDialog.Execute ;
     OK := False ;
     while not Done do begin
        // Ensure that file has selected extension
        SaveDialog.FileName := ChangeFileExt( SaveDialog.FileName,
                                              FileTypeExts[ SaveDialog.FilterIndex] ) ;
        // Check if file exists
        if not FileExists(SaveDialog.FileName) then OK := True
        else if MessageDlg( format(
                'File %s already exists! DO you want to overwrite it? ',
                [SaveDialog.FileName]),mtWarning,[mbYes,mbNo], 0 )
                 = mrYes then OK := True ;

        // Let user choose another name
        if OK then Done := True
              else Done := not SaveDialog.Execute ;

        end ;

     // Exit if a suitable file name is not available
     if not OK then Exit ;

     OK := ImageFile.CreateFile( SaveDialog.FileName,
                                 MainFrm.IDRFile.FrameWidth,
                                 MainFrm.IDRFile.FrameHeight,
                                 MainFrm.IDRFile.NumBytesPerPixel*8,
                                 1,
                                 MainFrm.IDRFile.NumFrames ) ;

     if not OK then begin
        MainFrm.StatusBar.SimpleText := 'Unable to create : ' + SaveDialog.FileName ;
        Exit ;
        end ;

    // Allocate frame buffer
    GetMem( PFrameBuf, MainFrm.IDRFile.NumBytesPerFrame ) ;

    try
       for FrameNum := 1 to MainFrm.IDRFile.NumFrames do begin
           // Read frame from file
           OK := MainFrm.IDRFile.LoadFrame( FrameNum, PFrameBuf ) ;
           // Save file to export file
           if OK then begin
              ImageFile.SaveFrame( FrameNum, PFrameBuf ) ;
              MainFrm.StatusBar.SimpleText := format(
              'Exporting %d/%d frames',[FrameNum,MainFrm.IDRFile.NumFrames]) ;
              Application.ProcessMessages ;

              end ;
           end ;
    finally
       FreeMem( PFrameBuf ) ;
       ImageFile.CloseFile ;

       MainFrm.StatusBar.SimpleText := format(
       'Export: %d frames exported to %s',[MainFrm.IDRFile.NumFrames,SaveDialog.FileName]) ;

       end ;

{    edPixelWidth.Value := ImageFile.XResolution ;
    edPixelWidth.Units := ImageFile.ResolutionUnit ;
    edPixelUnits.Text := ImageFile.ResolutionUnit ;
    edPixelAspectRatio.Value := ImageFile.YResolution / ImageFile.XResolution ;
    edCalBarWidth.Value := 0.2*FrameWidth*edPixelWidth.Value ;
    edCalBarWidth.Units := ImageFile.ResolutionUnit ;}

       end ;


procedure TFileIO.SaveFrameAsTIFF ;
// ----------------------------------------
// Save currently active image to TIFF file
// ----------------------------------------
var
     PInFrameBuf : PIntArray ; // Pointer to image source buffer
     POutFrameBuf : Pointer ;  // Pointer to image save buffer
     TIFPixelDepth : Integer ;
     OK : Boolean ;
     i : Integer ;
begin

     // Present user with standard Save File dialog box
     SaveDialog.FilterIndex := 2 ;
     SaveDialog.DefaultExt := RightStr(FileTypeExts[SaveDialog.FilterIndex],3) ;
     SaveDialog.FileName := ChangeFileExt( MainFrm.IDRFile.FileName,
                                           FileTypeExts[SaveDialog.FilterIndex] ) ;
     SaveDialog.Title := 'Save as TIFF File' ;

     // Open last used data directory
     if DirectoryExists(MainFrm.DataDirectory) and (MainFrm.DataDirectory <> '') then begin
        SaveDialog.InitialDir := MainFrm.DataDirectory ;
        end
     else SaveDialog.InitialDir := MainFrm.DefaultDataDirectory ;

     if not SaveDialog.Execute then Exit ;

     PInFrameBuf := Nil ;
     OK := False ;
     if MainFrm.ActiveMDIChild.Name = 'IntegrateFrm' then begin
        // Save current image from integrated image module
        //if IntegrateFrm.ImageAvailable then
        end
     else if MainFrm.ActiveMDIChild.Name = 'RecordFrm' then begin
        // Save current image from Record Sequence module
        if RecordFrm.ImageAvailable then begin
           PInFrameBuf := RecordFrm.PDisplayBufs[RecordFrm.SelectedFrameType] ;
           if MainFrm.Cam1.PixelDepth > 8 then TIFPixelDepth := 16
                                          else TIFPixelDepth := 8 ;
           OK := ImageFile.CreateFile( SaveDialog.FileName,
                                       MainFrm.Cam1.FrameWidth,
                                       MainFrm.Cam1.FrameHeight,
                                       TIFPixelDepth,
                                       1,
                                       1 ) ;
           end ;
        end
     else if MainFrm.ActiveMDIChild.Name = 'ViewFrm' then begin
        // Save current image from View Frames module
        if ViewFrm.ImageAvailable then begin
           PInFrameBuf := ViewFrm.PImageBufs[ViewFrm.SelectedFrameType] ;
           OK := ImageFile.CreateFile( SaveDialog.FileName,
                                       MainFrm.IDRFile.FrameWidth,
                                       MainFrm.IDRFile.FrameHeight,
                                       MainFrm.IDRFile.NumBytesPerPixel*8,
                                       1,
                                       1 ) ;
           end ;
        end ;

     // Save image to TIFF file
     if OK then begin

        // Allocate buffer
        GetMem( POutFrameBuf, ImageFile.NumBytesPerFrame ) ;

        if ImageFile.PixelDepth <= 8 then begin
           // 8 bit image
           for i := 0 to ImageFile.NumPixelsPerFrame-1 do
               PByteArray(POutFrameBuf)^[i] := Byte(PInFrameBuf^[i]) ;
           end
        else begin
           // 16 bit image
           for i := 0 to ImageFile.NumPixelsPerFrame-1 do
               PWordArray(POutFrameBuf)^[i] := Word(PInFrameBuf^[i]) ;
           end ;
        ImageFile.SaveFrame( 1, POutFrameBuf ) ;
        ImageFile.CloseFile ;

        FreeMem( POutFrameBuf ) ;

        end ;

     end;


procedure TFileIO.SaveDialogTypeChange(Sender: TObject);
// -------------------------------------------------
// Update default extension when filter type changed
// -------------------------------------------------
begin
     SaveDialog.FileName := ChangeFileExt(SaveDialog.FileName,FileTypeExts[SaveDialog.FilterIndex]) ;
     SaveDialog.DefaultExt := RightStr(FileTypeExts[SaveDialog.FilterIndex],3) ;
     end;


procedure TFileIO.OpenDialogTypeChange(Sender: TObject);
// -------------------------------------------------
// Update default extension when filter type changed
// -------------------------------------------------
begin
     OpenDialog.DefaultExt := RightStr(FileTypeExts[OpenDialog.FilterIndex],3) ;
     end;

procedure TFileIO.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : single        // Value
                                 ) ;
// ---------------------
// Add Key=Single Value to List
// ---------------------
begin

     List.Add( ReplaceText(Keyword + format('=%.4g',[Value]),'==','=') ) ;
end;


procedure TFileIO.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : Integer        // Value
                                 ) ;
// ---------------------
// Add Key=Integer Value to List
// ---------------------
begin
     List.Add( ReplaceText( Keyword + format('=%d',[Value]),'==','=') ) ;
end;

procedure TFileIO.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : NativeInt        // Value
                                 ) ;
// ---------------------
// Add Key=NativeInt Value to List
// ---------------------
begin
     List.Add( ReplaceText(Keyword + format('=%d',[Value] ),'==','=') ) ;
end;


procedure TFileIO.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : string        // Value
                                 ) ;
// ---------------------
// Add Key=string Value to List
// ---------------------
begin
     List.Add( ReplaceText( Keyword + '=' + Value,'==','=') ) ;
end;


procedure TFileIO.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : Boolean        // Value
                                 ) ;
// ---------------------
// Add Key=boolean Value to List
// ---------------------
begin
     if Value then List.Add(  ReplaceText( Keyword + '= T','==','=') )
              else List.Add(  ReplaceText( Keyword + '= F','==','=') ) ;
end;


function TFileIO.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : single       // Value
                               ) : Single ;         // Return value
// ------------------------------
// Get Key=Single Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin

     idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := ExtractFloat( s, Value ) ;
        end
     else Result := Value ;

end;


function TFileIO.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : Integer       // Value
                               ) : Integer ;        // Return value
// ------------------------------
// Get Key=Integer Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin

     idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := STrToInt( s ) ;
        end
     else Result := Value ;

end;


function TFileIO.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : NativeInt       // Value
                               ) : NativeInt ;        // Return value
// ------------------------------
// Get Key=Integer Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin

     idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := STrToInt( s ) ;
        end
     else Result := Value ;

end;


function TFileIO.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : string       // Value
                               ) : string ;        // Return value
// ------------------------------
// Get Key=Integer Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin

      idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := s ;
        end
     else Result := Value ;

end;


function TFileIO.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : Boolean       // Value
                               ) : Boolean ;        // Return value
// ------------------------------
// Get Key=Boolean Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin

     idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        if ContainsText(s,'T') then Result := True
                               else Result := False ;
        end
     else Result := Value ;

end;


function TFileIO.ExtractFileNameOnly( FilePath : string ) : string ;
{ -----------------------------------------------------
  Extract file name (without extension) from file path
  ----------------------------------------------------}
var
   FileName : string ;
   FileExt : string[6] ;
begin
     FileName := ExtractFileName(FilePath) ;
     FileExt := ExtractFileExt(FileName) ;
     Delete( FileName,Pos(FileExt,FileName),Length(FileExt) ) ;
     ExtractFileNameOnly := FileName ;
     end ;




end.


