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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImageFile, IDRFile, StrUtils ;



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
    pWriteBuf : PByteArray ;
    FileTransferCount : Cardinal ;

  public
    { Public declarations }
    procedure SaveInitialisationFile( FileName : String ) ;
    procedure LoadInitialisationFile( FileName : String ) ;
    procedure ImportFile ;
    procedure ExportFile ;
    procedure SaveFrameAsTIFF ;

  end;

var
  FileIO: TFileIO;

implementation

uses Main, shared, LabIOUnit, maths, AmpModule , RecUnit, ViewUnit,
  LightSourceUnit, ZStageUnit;

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
   Header : array[1..cNumIDRHeaderBytes] of ANSIchar ;
   i,iSeq,iWav,ch,INIFileHandle : Integer ;
   Dev : Integer ;
begin

     INIFileHandle := FileCreate( FileName ) ;

     if INIFileHandle < 0 then begin
        MainFrm.StatusBar.SimpleText := 'Unable to create : ' + FileName ;
        Exit ;
        end ;

     // Initialise empty header buffer with zero bytes
     for i := 1 to sizeof(Header) do Header[i] := #0 ;

     // Camera settings
     AppendInt( Header, 'CAMTYPE=', MainFrm.CameraType ) ;

     // Auxiliary camera settings
     AppendInt( Header, 'CAMTYPEAUX=', MainFrm.AuxCameraType ) ;

     AppendFloat( Header, 'CAMFI=', MainFrm.Cam1.FrameInterval ) ;
     AppendInt( Header, 'CAMFL=', MainFrm.Cam1.FrameLeft ) ;
     AppendInt( Header, 'CAMFR=', MainFrm.Cam1.FrameRight ) ;
     AppendInt( Header, 'CAMFT=', MainFrm.Cam1.FrameTop ) ;
     AppendInt( Header, 'CAMFB=', MainFrm.Cam1.FrameBottom ) ;
     AppendInt( Header, 'CAMBIN=', MainFrm.Cam1.BinFactor ) ;
     AppendInt( Header, 'CAMRS=', MainFrm.Cam1.ReadoutSpeed ) ;
     AppendInt( Header, 'CAMVM=', MainFrm.Cam1.CameraMode ) ;
     AppendInt( Header, 'CAMADC=', MainFrm.Cam1.CameraADC ) ;
     AppendLogical( Header, 'CAMCCDCLR=', MainFrm.Cam1.CCDClearPreExposure ) ;
     AppendLogical( Header, 'CAMCCDPERO=', MainFrm.Cam1.CCDPostExposureReadout ) ;
     AppendInt( Header, 'NFREQ=', MainFrm.NumFramesRequired ) ;
     AppendFloat( Header, 'NRECPER=', MainFrm.RecordingPeriod ) ;
     AppendFloat( Header, 'ADCRECTIME=', MainFrm.ADCRecordingTime ) ;
     AppendInt( Header, 'CAMCOM=', MainFrm.Cam1.ComPort ) ;
     AppendInt( Header, 'CAMGN=', MainFrm.Cam1.AmpGain ) ;
     // Camera exposure/readout trigger offset
     AppendFloat( Header, 'CAMTRIGOFFSET=', MainFrm.CameraTriggerOffset ) ;

     AppendFloat( Header, 'CAMTEMPSET=', MainFrm.Cam1.CameraTemperatureSetPoint ) ;

     AppendFloat( Header, 'CAMADDRT=', MainFrm.Cam1.AdditionalReadoutTime ) ;

     // Camera readout A/D converter gain
     AppendINT( Header, 'CAMADCGN=', MainFrm.Cam1.ADCGain ) ;

     // Camera CCD vertical line shift speed
     AppendINT( Header, 'CAMVSS=', MainFrm.Cam1.CCDVerticalShiftSpeed ) ;

     AppendFloat( Header, 'LENSMAG=', MainFrm.Cam1.LensMagnification ) ;

     AppendFloat( Header, 'CALBARSZ=', MainFrm.CalibrationBarSize ) ;
     AppendFloat( Header, 'CALBARTH=', MainFrm.CalibrationBarThickness ) ;

     AppendInt( Header, 'RECMODE=', MainFrm.RecordingMode ) ;
     AppendFloat( Header, 'RECPER=', MainFrm.RecordingPeriod ) ;
     AppendFloat( Header, 'TLAPINT=', MainFrm.TimeLapseInterval ) ;
     AppendFloat( Header, 'BURDUR=', MainFrm.BurstDuration ) ;
     AppendFloat( Header, 'BURINT=', MainFrm.BurstInterval ) ;

     AppendInt( Header, 'PAL=', Integer(MainFrm.PaletteType) ) ;
     AppendInt( Header, 'DZOOM=', MainFrm.DisplayZoomIndex ) ;

     AppendInt( Header, 'ROISIZE=', MainFrm.ROISize ) ;
     AppendInt( Header, 'ROIX=', MainFrm.ROIX ) ;
     AppendInt( Header, 'ROIY=', MainFrm.ROIY ) ;
     AppendInt( Header, 'LIVEWVL=', MainFrm.LiveWindowWavelength ) ;

     // A/D channel settings
     AppendInt( Header, 'ADCNC=', MainFrm.ADCNumChannels ) ;
     AppendInt( Header, 'ADCINPUTMODE=', LabIO.ADCInputMode ) ;
     // NI interface API
     AppendInt( Header, 'NIDAQAPI=', LabIO.NIDAQAPI ) ;

     AppendFloat( Header, 'ADCSI=', MainFrm.ADCScanInterval ) ;
     AppendFloat( Header, 'ADCVR=', MainFrm.ADCVoltageRange ) ;
     AppendFloat( Header, 'ADCDW=', MainFrm.ADCDisplayWindow ) ;
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
         AppendInt( Header, format('CIN%d=',[ch]), MainFrm.ADCChannel[ch].ChannelOffset) ;
         AppendString( Header, format('CU%d=',[ch]), MainFrm.ADCChannel[ch].ADCUnits ) ;
         AppendString( Header, format('CN%d=',[ch]), MainFrm.ADCChannel[ch].ADCName ) ;
         AppendFloat( Header, format('CCF%d=',[ch]), MainFrm.ADCChannel[ch].ADCCalibrationFactor ) ;
         AppendFloat( Header, format('CSC%d=',[ch]), MainFrm.ADCChannel[ch].ADCScale) ;
         end ;


     // Patch clamp amplifier data
     for i := 1 to 2 do begin
         AppendInt( Header, format('AMP%d=',[i]),Amplifier.AmplifierType[i] ) ;
         AppendInt( Header, format('AMPCH%d=',[i]),Amplifier.GainTelegraphChannel[i] ) ;
         AppendInt( Header, format('AMPGAINCH%d=',[i]),Amplifier.GainTelegraphChannel[i] ) ;
         AppendInt( Header, format('AMPMODECH%d=',[i]),Amplifier.ModeTelegraphChannel[i] ) ;
         end ;

     // Command voltage settings
     AppendFloat( Header, 'VCDIV0=', MainFrm.VCommand[0].DivideFactor ) ;
     AppendFloat( Header, 'VCHOLD0=', MainFrm.VCommand[0].HoldingVoltage ) ;
     AppendFloat( Header, 'VCDIV1=', MainFrm.VCommand[1].DivideFactor ) ;
     AppendFloat( Header, 'VCHOLD1=', MainFrm.VCommand[1].HoldingVoltage ) ;
     AppendFloat( Header, 'VCDIV2=', MainFrm.VCommand[2].DivideFactor ) ;
     AppendFloat( Header, 'VCHOLD2=', MainFrm.VCommand[2].HoldingVoltage ) ;

     // Default digital output port states
     for Dev := 1 to MaxDevices do begin
         AppendInt( Header, format('DIGOUTSTATE%d=',[Dev]), LabIO.DigOutState[Dev] ) ;
         end ;

     // Light source
     AppendInt( Header, 'LSDEV=', LightSource.DeviceType ) ;
     AppendFloat( Header, 'LSW1=', LightSource.Wavelength1 ) ;
     AppendFloat( Header, 'LSV1=', LightSource.Voltage1 ) ;
     AppendFloat( Header, 'LSW2=', LightSource.Wavelength2 ) ;
     AppendFloat( Header, 'LSV2=', LightSource.Voltage2 ) ;

     // Laser settings
     for i := 1 to lsMaxLasers do begin
        AppendFloat( Header, format('LSLAS%dWAV=',[i]), LightSource.LaserWavelength[i] ) ;
        AppendFloat( Header, format('LSLAS%dDEL=',[i]), LightSource.LaserDelay[i] ) ;
        AppendFloat( Header, format('LSLAS%dVOFF=',[i]), LightSource.LaserOffVoltage[i] ) ;
        AppendFloat( Header, format('LSLAS%dVON=',[i]), LightSource.LaserOnVoltage[i] ) ;
        AppendFloat( Header, format('LSLAS%dINT=',[i]), LightSource.LaserIntensity[i] ) ;
        end ;

     // LED settings
     AppendFloat( Header, 'LSLEDOFFV=', LightSource.LEDOffVoltage ) ;
     AppendFloat( Header, 'LSLEDMAXV=', LightSource.LEDMaxVoltage ) ;

     // TIRF settings
     for i := 1 to lsMaxTIRFGalvos do begin
        AppendFloat( Header, format('LSTIRFOFF%d=',[i]), LightSource.TIRFOff[i] ) ;
        AppendFloat( Header, format('LSTIRFON%d=',[i]), LightSource.TIRFOn[i] ) ;
        AppendFloat( Header, format('LSTIRFWF%d=',[i]), LightSource.TIRFWF[i] ) ;
        end ;

     // Shutter closed wavelength
     AppendFloat( Header, 'LSSCWAVEL=', LightSource.ShutterClosedWavelength ) ;

     // Shutter blanking period
     AppendFloat( Header, 'LSSCBLANK=', LightSource.ShutterBlankingPeriod ) ;

     // Shutter open/close change time
     AppendFloat( Header, 'LSSCHTIME=', LightSource.ShutterChangeTime ) ;

     // Excitation wavelength settings
     AppendLogical( Header, 'EXCSW=', MainFrm.EXCSingleWavelength ) ;

     // Single wavelength selected
     AppendInt( Header, 'EXCSWN=', MainFrm.EXCSingleWavelengthNum ) ;

     AppendLogical( Header, 'EXCONREC=', MainFrm.ExcitationOnWhenRecording ) ;

     // Excitation multi-wavelength sequence settings
     AppendInt( Header, 'EXCSEQNUM=', MainFrm.EXCSequenceNum ) ;
     for iSeq := 0 to MaxEXCSequences-1 do begin
         AppendInt( Header, format('EXCNW%d=',[iSeq]), MainFrm.EXCNumWavelengths[iSeq] ) ;
         AppendString( Header, format('EXCSEQNAM%d=',[iSeq]), MainFrm.EXCSequenceName[iSeq] ) ;
         for iWav := 0 to MainFrm.EXCNumWavelengths[iSeq] do begin
             AppendInt( Header, format('EXCSEQ%dW%d=',[iSeq,iWav]), MainFrm.EXCSequence[iWav,iSeq].WavelengthNum) ;
             AppendInt( Header, format('EXCSEQ%dDF%d=',[iSeq,iWav]), MainFrm.EXCSequence[iWav,iSeq].DivideFactor) ;
             end ;
         end ;

     // Save sequence 0 for compatibility with older versions of WinFluor
     AppendInt( Header, 'EXCNW=', MainFrm.EXCNumWavelengths[0] ) ;
     for iWav := 0 to MainFrm.EXCNumWavelengths[0] do begin
         AppendInt( Header, format('EXCSEQW%d=',[iWav]), MainFrm.EXCSequence[iWav,0].WavelengthNum) ;
         AppendInt( Header, format('EXCSEQDF%d=',[iWav]), MainFrm.EXCSequence[iWav,0].DivideFactor) ;
         end ;

     // Save wavelengths list
     for iWav := 0 to High(MainFrm.EXCWavelengths) do begin
         AppendInt( Header, format('EXCWC%d=',[iWav]), MainFrm.EXCWavelengths[iWav].Centre) ;
         AppendInt( Header, format('EXCWW%d=',[iWav]), MainFrm.EXCWavelengths[iWav].Width) ;
         end ;

     AppendFloat( Header, 'EXCSPSTARTW=', MainFrm.EXCSpectrumStartWavelength ) ;
     AppendFloat( Header, 'EXCSPENDW=', MainFrm.EXCSpectrumEndWavelength ) ;
     AppendFloat( Header, 'EXCSPBANDW=', MainFrm.EXCSpectrumBandwidth ) ;
     AppendFloat( Header, 'EXCSPSTEPS=', MainFrm.EXCSpectrumStepSize ) ;

     // Stimulus program file
     AppendString( Header, 'STIMFILE=', MainFrm.StimFileName) ;

     // Photo stimulus program file
     AppendString( Header, 'PHOTOSTIMFIL=', MainFrm.PhotoStimFileName) ;

     // Fluophore mainFrm.Binding mainFrm.BindingEquations table
     for i := 0 to MainFrm.IDRFile.MaxEquations-1 do begin
         AppendLogical( Header, format('EQNUSE%d=',[i]), mainFrm.BindingEquations[i].InUse ) ;
         AppendString( Header, format('EQNION%d=',[i]), mainFrm.BindingEquations[i].Ion) ;
         AppendString( Header, format('EQNUN%d=',[i]), mainFrm.BindingEquations[i].Units) ;
         AppendString( Header, format('EQNNAM%d=',[i]), mainFrm.BindingEquations[i].Name) ;
         AppendFloat( Header, format('EQNRMAX%d=',[i]), mainFrm.BindingEquations[i].RMax) ;
         AppendFloat( Header, format('EQNRMIN%d=',[i]), mainFrm.BindingEquations[i].RMin) ;
         AppendFloat( Header, format('EQNKEFF%d=',[i]), mainFrm.BindingEquations[i].KEff) ;
         end ;

     AppendString( Header, 'DDIR=', MainFrm.DataDirectory ) ;

     AppendString( Header, 'VPDIR=', MainFrm.VProtDirectory ) ;

     // Printer page settings
     AppendInt( Header, 'PRTM=', MainFrm.PrinterTopMargin )  ;
     AppendInt( Header, 'PRBM=', MainFrm.PrinterBottomMargin )  ;
     AppendInt( Header, 'PRLM=', MainFrm.PrinterLeftMargin )  ;
     AppendInt( Header, 'PRRM=', MainFrm.PrinterRightMargin )  ;
     AppendInt( Header, 'PRLT=', MainFrm.PrinterLineThickness )  ;
     AppendInt( Header, 'PRMS=', MainFrm.PrinterMarkerSize )  ;
     AppendLogical( Header, 'PRUC=', MainFrm.PrinterUseColor ) ;
     AppendString( Header, 'PRFN=', MainFrm.PrinterFontName ) ;
     AppendInt( Header, 'PRFS=', MainFrm.PrinterFontSize )  ;

     for i := 0 to High(MainFrm.RecentFiles) do
         AppendString(Header,format('FILE%d=',[i]),MainFrm.RecentFiles[i]) ;

     // Input/output/control line configuration
     AppendInt( Header, 'IOADCI=', MainFrm.IOConfig.ADCIn ) ;
     AppendInt( Header, 'IOCAMS=', MainFrm.IOConfig.CameraStart ) ;
     AppendLogical( Header, 'IOCAMSAH=', MainFrm.IOConfig.CameraStartActiveHigh ) ;
     AppendInt( Header, 'IOVCOM0=', MainFrm.IOConfig.VCommand[0] ) ;
     AppendInt( Header, 'IOVCOM1=', MainFrm.IOConfig.VCommand[1] ) ;
     AppendInt( Header, 'IOVCOM2=', MainFrm.IOConfig.VCommand[2] ) ;
     AppendInt( Header, 'IOLSSU=', MainFrm.IOConfig.LSShutter ) ;
     AppendLogical( Header, 'IOLSSUAH=', MainFrm.IOConfig.LSShutterActiveHigh ) ;
     AppendInt( Header, 'IOLSWS=', MainFrm.IOConfig.LSWavelengthStart ) ;
     AppendInt( Header, 'IOLSWE=', MainFrm.IOConfig.LSWavelengthEnd ) ;
     AppendInt( Header, 'IOLSLS=', MainFrm.IOConfig.LSLaserStart ) ;
     AppendInt( Header, 'IOLSLE=', MainFrm.IOConfig.LSLaserEnd ) ;
     AppendInt( Header, 'IODSTA=', MainFrm.IOConfig.DigitalStimStart ) ;
     AppendInt( Header, 'IODEND=', MainFrm.IOConfig.DigitalStimEnd ) ;
     AppendInt( Header, 'IOPSX=', MainFrm.IOConfig.PhotoStimX ) ;
     AppendInt( Header, 'IOPSY=', MainFrm.IOConfig.PhotoStimY ) ;
     AppendInt( Header, 'IOPSI1=', MainFrm.IOConfig.PhotoStimI1 ) ;
     AppendInt( Header, 'IOPSI2=', MainFrm.IOConfig.PhotoStimI2 ) ;
     AppendInt( Header, 'IOPSI3=', MainFrm.IOConfig.PhotoStimI3 ) ;

     // Photo-stimulus Pockels cell and shutter configuration (Added by NS)
     AppendInt( Header, 'IOPSMETER=', MainFrm.IOConfig.PhotoStimMeter ) ;
     AppendInt( Header, 'IOPSSU=', MainFrm.IOConfig.PhotoStimShutter ) ;
     AppendFloat( Header, 'IOPSSLA=', MainFrm.IOConfig.PhotoStimShutterLatency ) ;
     AppendLogical( Header, 'IOPSSUAH=', MainFrm.IOConfig.PhotoStimShutterActiveHigh ) ;
     AppendLogical( Header, 'IOPSPCMAN=', MainFrm.IOconfig.PhotoStimPowerCalManual ) ;
     AppendFloat( Header, 'IOPSMETERR=', MainFrm.IOConfig.PhotoStimMeterRange ) ;
     AppendFloat( Header, 'IOPSMETERS=', MainFrm.IOconfig.PhotoStimMeterScale ) ;


     AppendInt( Header, 'IOCLKSYNC=', MainFrm.IOConfig.ClockSyncLine ) ;

     // Event detection settings
{     AppendFloat( Header, 'EVANDEADTIME=', MainFrm.EventAnalysis.DeadTime ) ;
     AppendFloat( Header, 'EVANTHRESHOLD=', MainFrm.EventAnalysis.DetectionThreshold ) ;
     AppendFloat( Header, 'EVANTHRESHDUR=', MainFrm.EventAnalysis.ThresholdDuration ) ;
     AppendInt( Header, 'EVANPOLARITY=', MainFrm.EventAnalysis.DetectionThresholdPolarity ) ;
     AppendLogical( Header, 'EVANFIXBASE=', MainFrm.EventAnalysis.FixedBaseline ) ;
     AppendInt( Header, 'EVANSOURCE=', MainFrm.EventAnalysis.DetectionSource ) ;
     AppendFloat( Header, 'EVANDISPLAYMAX=', MainFrm.EventAnalysis.DisplayMax ) ;
     AppendFloat( Header, 'EVANROLLBASEPERIOD=', MainFrm.EventAnalysis.RollingBaselinePeriod ) ;}

     // Seal test settings
     AppendInt( Header, 'SEALTUSE=',MainFrm.SealTest.Use ) ;
     AppendFloat( Header, 'SEALTPH1=',MainFrm.SealTest.PulseHeight1 ) ;
     AppendFloat( Header, 'SEALTHV1=',MainFrm.SealTest.HoldingVoltage1 ) ;
     AppendFloat( Header, 'SEALTPH2=',MainFrm.SealTest.PulseHeight2 ) ;
     AppendFloat( Header, 'SEALTHV2=',MainFrm.SealTest.HoldingVoltage2 ) ;
     AppendFloat( Header, 'SEALTPH3=',MainFrm.SealTest.PulseHeight3 ) ;
     AppendFloat( Header, 'SEALTHV3=',MainFrm.SealTest.HoldingVoltage3 ) ;
     AppendFloat( Header, 'SEALTPW=',MainFrm.SealTest.PulseWidth ) ;

     // Photo-stimulus settings
     AppendFloat( Header, 'PSPER=',MainFrm.PhotoStim.Period ) ;
     AppendLogical( Header, 'PSREP=',MainFrm.PhotoStim.RepeatedStim ) ;
     AppendInt( Header, 'PSNSTIMP=',MainFrm.PhotoStim.NumStimPoints ) ;
     AppendInt( Header, 'PSATTEN=',MainFrm.PhotoStim.Attenuator ) ;
     for i := 1 to 3 do
     begin
       AppendFloat( Header, format('PSXC%d=',[i]), MainFrm.PhotoStim.XCenter[i]) ;
       AppendFloat( Header, format('PSYC%d=',[i]), MainFrm.PhotoStim.YCenter[i]) ;
       AppendFloat( Header, format('PSXS%d=',[i]), MainFrm.PhotoStim.XScale[i]) ;
       AppendFloat( Header, format('PSYS%d=',[i]), MainFrm.PhotoStim.YScale[i]) ;
       AppendLogical( Header, format('PSPCPENA%d=',[i]), MainFrm.PhotoStim.PCEnable[i] ) ;
       AppendFloat( Header, format('PSPCPMIN%d=',[i]), MainFrm.PhotoStim.PCPowerMin[i] ) ;
       AppendFloat( Header, format('PSPCPMAX%d=',[i]), MainFrm.PhotoStim.PCPowerMax[i] ) ;
       AppendFloat( Header, format('PSPCBIAS%d=',[i]), MainFrm.PhotoStim.PCBias[i] ) ;
       AppendFloat( Header, format('PSPCVPI%d=',[i]), MainFrm.PhotoStim.PCVoltagePi[i] ) ;
       AppendLogical( Header, format('PSPCPC%d=',[i]), MainFrm.PhotoStim.PCPolarizationCross[i] ) ;
       AppendLogical( Header, format('PSPC302%d=',[i]), MainFrm.PhotoStim.PCConoptics302[i] ) ;
       AppendFloat( Header, format('PSLPMIN%d=',[i]), MainFrm.PhotoStim.LinearPowerMin[i] ) ;
       AppendFloat( Header, format('PSLPMAX%d=',[i]), MainFrm.PhotoStim.LinearPowerMax[i] ) ;
       AppendFloat( Header, format('PSLVMIN%d=',[i]), MainFrm.PhotoStim.LinearVoltageMin[i] ) ;
       AppendFloat( Header, format('PSLVMAX%d=',[i]), MainFrm.PhotoStim.LinearVoltageMax[i] );
       AppendLogical( Header, format('PSPCSHU%d=',[i]), MainFrm.PhotoStim.EnableShutter[i] ) ;
     end;
     AppendFloat( Header, 'PSROTAT=',MainFrm.PhotoStim.ImageRotation ) ;
     AppendFloat( Header, 'PSMICPERPX=',MainFrm.PhotoStim.XMicronsPerPixel ) ;
     AppendFloat( Header, 'PSMICPERPY=',MainFrm.PhotoStim.YMicronsPerPixel ) ;
     AppendString( Header, 'PSPVLOG=',MainFrm.PhotoStim.PVLogFile ) ;
     AppendLogical( Header, 'PSREFLE=',MainFrm.PhotoStim.RefLineEnabled ) ;
     AppendLogical( Header, 'PSCMDTERM=',MainFrm.PhotoStim.CmdTermZero ) ;

     AppendLogical( Header, 'DISPLAYGRID=', MainFrm.mnDisplayGrid.Checked ) ;

     // Append visibility state of channels in RecADCOnlyUnit
     // Modified by NS 19 March 2009
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
        AppendLogical( Header, format('CVRADCO%d=',[ch]), MainFrm.ADCChannelRecADCOnlyUnitVisible[ch] ) ;
     end ;

     // Append visibility state of channels in Sealtest
     // Modified by NS 24 March 2009
     AppendInt( Header, 'CVSTNUM=', MainFrm.ADCChannelSealtestNumberOfChannels ) ;
     for ch := 0 to MainFrm.ADCChannelSealtestNumberOfChannels-1 do begin
        AppendLogical( Header, format('CVST%d=',[ch]), MainFrm.ADCChannelSealtestVisible[ch] ) ;
     end ;

     // Append state of SmoothDifferentiate window
     // Modified by NS 10 April 2009
     for ch := 0 to (4 - 1) do begin
        AppendLogical( Header, format('SDWCH%d=',[ch]), MainFrm.SmoothDifferentiateUnitVisible[ch] ) ;
     end;
     AppendInt( Header, 'SDWMA=', MainFrm.SmoothDifferentiateUnitMADataWindow ) ;
     AppendInt( Header, 'SDWDX=', MainFrm.SmoothDifferentiateUnitMADXWindow ) ;

     // Display contrast settings optimisation
     AppendLogical( Header, 'CNCAFT=', MainFrm.ContrastChangeAllFrameTypes ) ;
     AppendLogical( Header, 'CNAUTOOP=', MainFrm.ContrastAutoOptimise ) ;
     AppendLogical( Header, 'CN6SD=', MainFrm.Contrast6SD ) ;

     // Append state of DynamicProtocol window
     // Modified by NS 22 December 2009
     AppendInt( Header, 'DPCH=', MainFrm.DynamicProtocol.SelectedChannel ) ;
     AppendInt( Header, 'DPDIR=', MainFrm.DynamicProtocol.Direction ) ;
     AppendFloat( Header, 'DPTHRES=', MainFrm.DynamicProtocol.Threshold ) ;
     AppendFloat( Header, 'DPDUR=', MainFrm.DynamicProtocol.Duration ) ;
     AppendLogical( Header, 'DPEP=', MainFrm.DynamicProtocol.EPRestart ) ;
     AppendLogical( Header, 'DPPS=', MainFrm.DynamicProtocol.PSRestart ) ;
     AppendString( Header, 'DPEPF=',MainFrm.DynamicProtocol.EPStimFileName ) ;
     AppendInt( Header, 'DPEPI=', MainFrm.DynamicProtocol.EPStimIndex ) ;
     AppendString( Header, 'DPPSF=',MainFrm.DynamicProtocol.PSStimFileName ) ;
     AppendInt( Header, 'DPPSI=', MainFrm.DynamicProtocol.PSStimIndex ) ;

     // Auto reset interface cards
     AppendLogical( Header, 'ARI=', MainFrm.AutoResetInterfaceCards ) ;

     AppendLogical( Header, 'STARTSTIMONREC=', MainFrm.StartStimOnRecord ) ;

     // Save Z Stage control settings
     ZStage.SaveSettings( Header ) ;

     if FileWrite( INIFileHandle, Header, Sizeof(Header) ) <> Sizeof(Header) then
        ShowMessage( ' Initialisation file write failed ' ) ;

     if INIFileHandle >= 0 then FileClose( INIFileHandle ) ;

 //    n := 0 ;
 //    for i:= 1 to High(Header) do if Header[i] <> #0 then Inc(n) ;
 //    OutputdebugString(pchar(format('Header size=%d',[n])));
 //    ShowMessage(format('Header size=%d',[n]));
     end ;


procedure TFileIO.LoadInitialisationFile(
          FileName : String
          ) ;
// ------------------------
// Load initialisation file
// ------------------------
var
   Header : array[1..cNumIDRHeaderBytes] of ANSIchar ;
   i,iWav,iSeq,ch,INIFileHandle : Integer ;
   iValue : Integer ;
   fValue : Single ;
   bValue : Boolean ;
   ADCChannel : TChannel ;
   Dev : Integer ;
begin

     if not FileExists( FileName ) then Exit ;

     INIFileHandle := FileOpen( FileName, fmOpenReadWrite ) ;

     if INIFileHandle < 0 then begin
        ShowMessage('Unable to open ' +  FileName ) ;
        Exit ;
        end ;

     // Fill with 0s
     for i := 0 to High(Header) do Header[i] := #0 ;

     // Read data from INI file
     FileRead( INIFileHandle, Header, Sizeof(Header) ) ;

     // Camera settings
     ReadInt( Header, 'CAMTYPE=', MainFrm.CameraType ) ;

     // Auxiliary camera settings
     ReadInt( Header, 'CAMTYPEAUX=', MainFrm.AuxCameraType ) ;

     ReadInt( Header, 'CAMBIN=', iValue ) ;
     MainFrm.Cam1.BinFactor := iValue ;

     ReadInt( Header, 'CAMRS=', iValue ) ;
     MainFrm.Cam1.ReadoutSpeed := iValue ;

     ReadInt( Header, 'CAMVM=', iValue ) ;
     MainFrm.Cam1.CameraMode := iValue ;

     ReadInt( Header, 'CAMADC=', iValue ) ;
     MainFrm.Cam1.CameraADC := iValue ;

     bValue := False ;
     ReadLogical( Header, 'CAMCCDCLR=', bValue ) ;
     MainFrm.Cam1.CCDClearPreExposure := bValue ;

     bValue := False ;
     ReadLogical( Header, 'CAMCCDPERO=', bValue ) ;
     MainFrm.Cam1.CCDPostExposureReadout := bValue ;

     ReadInt( Header, 'CAMCOM=', iValue ) ;
     MainFrm.Cam1.ComPort := iValue ;
     ReadInt( Header, 'CAMGN=', iValue ) ;
     MainFrm.Cam1.AmpGain := iValue ;

     ReadInt( Header, 'CAMFL=', iValue ) ;
     MainFrm.Cam1.FrameLeft := iValue ;
     ReadInt( Header, 'CAMFR=', iValue ) ;
     MainFrm.Cam1.FrameRight := iValue ;
     ReadInt( Header, 'CAMFT=', iValue ) ;
     MainFrm.Cam1.FrameTop := iValue ;
     ReadInt( Header, 'CAMFB=', iValue ) ;
     MainFrm.Cam1.FrameBottom := iValue ;

     // NOTE. Bin factor, readout speed and imaging area need to be set before
     // .FrameInterval to ensure camera will accept short intervals

     ReadFloat( Header, 'CAMFI=', fValue ) ;
     MainFrm.Cam1.FrameInterval := fValue ;

     // Camera exposure/readout trigger offset
     ReadFloat( Header, 'CAMTRIGOFFSET=', MainFrm.CameraTriggerOffset ) ;

     // Camera temperature set point
     fValue := -50.0 ;
     ReadFloat( Header, 'CAMTEMPSET=', fValue ) ;
     MainFrm.Cam1.CameraTemperatureSetPoint := fValue ;

     // Camera additional readout time (s)
     fValue := MainFrm.Cam1.AdditionalReadoutTime ;
     ReadFloat( Header, 'CAMADDRT=', fValue ) ;
     MainFrm.Cam1.AdditionalReadoutTime := fValue ;

     // Camera CCD readout A/D convert gain
     iValue := 0 ;
     ReadINT( Header, 'CAMADCGN=', iValue ) ;
     MainFrm.Cam1.ADCGain := iValue ;

     // Camera CCD vertical line shift speed
     iValue := -1 ;
     ReadINT( Header, 'CAMVSS=', iValue ) ;
     MainFrm.Cam1.CCDVerticalShiftSpeed := iValue ;

     fValue := 0.0 ;
     ReadFloat( Header, 'LENSMAG=', fValue ) ;
     if fValue <> 0.0 then MainFrm.Cam1.LensMagnification := fValue ;

     ReadFloat( Header, 'CALBARSZ=', MainFrm.CalibrationBarSize ) ;
     ReadFloat( Header, 'CALBARTH=', MainFrm.CalibrationBarThickness ) ;


     iValue := Integer(MainFrm.PaletteType) ;
     ReadInt( Header, 'PAL=', iValue ) ;
     MainFrm.PaletteType := TPaletteType(iValue) ;

     ReadInt( Header, 'DZOOM=', MainFrm.DisplayZoomIndex ) ;

     ReadInt( Header, 'NFREQ=', MainFrm.NumFramesRequired ) ;

     ReadInt( Header, 'RECMODE=', MainFrm.RecordingMode ) ;
     ReadFloat( Header, 'RECPER=', MainFrm.RecordingPeriod ) ;
     ReadFloat( Header, 'TLAPINT=', MainFrm.TimeLapseInterval ) ;
     ReadFloat( Header, 'BURDUR=', MainFrm.BurstDuration ) ;
     ReadFloat( Header, 'BURINT=', MainFrm.BurstInterval ) ;

     ReadFloat( Header, 'ADCRECTIME=', MainFrm.ADCRecordingTime ) ;

     ReadInt( Header, 'ROISIZE=', MainFrm.ROISize ) ;
     ReadInt( Header, 'ROIX=', MainFrm.ROIX ) ;
     ReadInt( Header, 'ROIY=', MainFrm.ROIY ) ;
     ReadInt( Header, 'LIVEWVL=', MainFrm.LiveWindowWavelength ) ;

     // National Instruments interface library
     iValue := 0 ;
     ReadInt( Header, 'NIDAQAPI=', iValue ) ;
     LabIO.NIDAQAPI := iValue ;

     // A/D input mode (NRSE/Differential)
     iValue := 0 ;
     ReadInt( Header, 'ADCINPUTMODE=', iValue ) ;
     LabIO.ADCInputMode := iValue ;

     // A/D channel settings
     ReadInt( Header, 'ADCNC=', iValue  ) ;
     MainFrm.ADCNumChannels := iValue ;

     // A/D channel scanning interval
     ReadFloat( Header, 'ADCSI=', fValue ) ;
     MainFrm.ADCScanInterval := fValue ;

     ReadFloat( Header, 'ADCVR=', fValue ) ;
     MainFrm.ADCVoltageRange := fValue ;

     ReadFloat( Header, 'ADCDW=', MainFrm.ADCDisplayWindow ) ;

     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
         // Get current channel settings
         ADCChannel := MainFrm.ADCChannel[ch] ;
         // Load parameters from INI file
         ReadInt( Header, format('CIN%d=',[ch]), ADCChannel.ChannelOffset ) ;
         ADCChannel.ChannelOffset := MainFrm.IDRFile.ADCNumChannels - ch - 1 ;
         ReadString( Header, format('CU%d=',[ch]), ADCChannel.ADCUnits ) ;
         ReadString( Header, format('CN%d=',[ch]), ADCChannel.ADCName ) ;
         ReadFloat( Header, format('CCF%d=',[ch]), ADCChannel.ADCCalibrationFactor ) ;
         ReadFloat( Header, format('CSC%d=',[ch]), ADCChannel.ADCScale) ;
         // Update channel
         MainFrm.ADCChannel[ch] := ADCChannel ;
         end ;

     // Patch clamp amplifier data

     // Patch clamp amplifier data
     for i := 1 to 2 do begin
         ReadInt( Header, format('AMP=',[i]),iValue ) ;
         ReadInt( Header, format('AMP%d=',[i]),iValue ) ;
         Amplifier.AmplifierType[i] := iValue ;
         ReadInt( Header, format('AMPCH%d=',[i]),iValue ) ;
         ReadInt( Header, format('AMPGAINCH%d=',[i]),iValue ) ;
         Amplifier.GainTelegraphChannel[i] := iValue ;
         ReadInt( Header, format('AMPMODECH%d=',[i]),iValue ) ;
         Amplifier.ModeTelegraphChannel[i] := iValue ;
         end ;

     // Patch clamp command voltage divide factor
     ReadFloat( Header, 'VCDIV=', MainFrm.VCommand[0].DivideFactor ) ;
     // Patch clamp holding potential
     ReadFloat( Header, 'VCHOLD=', MainFrm.VCommand[0].HoldingVoltage ) ;

     ReadFloat( Header, 'VCDIV0=', MainFrm.VCommand[0].DivideFactor ) ;
     ReadFloat( Header, 'VCHOLD0=', MainFrm.VCommand[0].HoldingVoltage ) ;
     ReadFloat( Header, 'VCDIV1=', MainFrm.VCommand[1].DivideFactor ) ;
     ReadFloat( Header, 'VCHOLD1=', MainFrm.VCommand[1].HoldingVoltage ) ;
     ReadFloat( Header, 'VCDIV2=', MainFrm.VCommand[2].DivideFactor ) ;
     ReadFloat( Header, 'VCHOLD2=', MainFrm.VCommand[2].HoldingVoltage ) ;

     // Default digital output port states
     for Dev := 1 to MaxDevices do begin
         ReadInt( Header, format('DIGOUTSTATE%d=',[Dev]), LabIO.DigOutState[Dev] ) ;
         end ;

     // Light source
     ReadInt( Header, 'LSDEV=', LightSource.DeviceType ) ;
     ReadFloat( Header, 'LSW1=', LightSource.Wavelength1 ) ;
     ReadFloat( Header, 'LSV1=', LightSource.Voltage1 ) ;
     ReadFloat( Header, 'LSW2=', LightSource.Wavelength2 ) ;
     ReadFloat( Header, 'LSV2=', LightSource.Voltage2 ) ;

     // Laser settings
     for i := 1 to lsMaxLasers do begin
        ReadFloat( Header, format('LSLAS%dWAV=',[i]), LightSource.LaserWavelength[i] ) ;
        ReadFloat( Header, format('LSLAS%dDEL=',[i]), LightSource.LaserDelay[i] ) ;
        ReadFloat( Header, format('LSLAS%dVOFF=',[i]), LightSource.LaserOffVoltage[i] ) ;
        ReadFloat( Header, format('LSLAS%dVON=',[i]), LightSource.LaserOnVoltage[i] ) ;
        ReadFloat( Header, format('LSLAS%dINT=',[i]), LightSource.LaserIntensity[i] ) ;
        end ;

     // LED settings
     ReadFloat( Header, 'LSLEDOFFV=', LightSource.LEDOffVoltage ) ;
     ReadFloat( Header, 'LSLEDMAXV=', LightSource.LEDMaxVoltage ) ;

     // TIRF settings
     for i := 1 to lsMaxTIRFGalvos do begin
        ReadFloat( Header, format('LSTIRFOFF%d=',[i]), LightSource.TIRFOff[i] ) ;
        ReadFloat( Header, format('LSTIRFON%d=',[i]), LightSource.TIRFOn[i] ) ;
        ReadFloat( Header, format('LSTIRFWF%d=',[i]), LightSource.TIRFWF[i] ) ;
        end ;

     // Shutter closed wavelength
     ReadFloat( Header, 'LSSCWAVEL=', LightSource.ShutterClosedWavelength ) ;

     // Shutter blanking period
     ReadFloat( Header, 'LSSCBLANK=', LightSource.ShutterBlankingPeriod ) ;

     // Shutter open/close change time
     ReadFloat( Header, 'LSSCHTIME=', LightSource.ShutterChangeTime ) ;

     // Excitation wavelength settings
     ReadLogical( Header, 'EXCSW=', MainFrm.EXCSingleWavelength ) ;

     ReadLogical( Header, 'EXCONREC=', MainFrm.ExcitationOnWhenRecording ) ;

     ReadInt( Header, 'EXCSWN=', MainFrm.EXCSingleWavelengthNum ) ;

     // Read sequence 0 (for compatibility with older versions
     ReadInt( Header, 'EXCNW=', MainFrm.EXCNumWavelengths[0] ) ;
     for iWav := 0 to MainFrm.EXCNumWavelengths[0] do begin
         ReadInt( Header, format('EXCSEQ%d=',[iWav]), MainFrm.EXCSequence[iWav,0].WavelengthNum) ;
         MainFrm.EXCSequence[iWav,0].DivideFactor := 1 ;
         ReadInt( Header, format('EXCSEQDF%d=',[iWav]), MainFrm.EXCSequence[iWav,0].DivideFactor) ;
         end ;

     // Excitation multi-wavelength sequence settings
     MainFrm.EXCSequenceNum := 0 ;
     ReadInt( Header, 'EXCSEQNUM=', MainFrm.EXCSequenceNum ) ;
     for iSeq := 0 to MaxEXCSequences-1 do begin
         ReadInt( Header, format('EXCNW%d=',[iSeq]), MainFrm.EXCNumWavelengths[iSeq] ) ;
         ReadString( Header, format('EXCSEQNAM%d=',[iSeq]), MainFrm.EXCSequenceName[iSeq] ) ;
         for iWav := 0 to MainFrm.EXCNumWavelengths[iSeq] do begin
             ReadInt( Header, format('EXCSEQ%dW%d=',[iSeq,iWav]), MainFrm.EXCSequence[iWav,iSeq].WavelengthNum) ;
             MainFrm.EXCSequence[iWav,iSeq].DivideFactor := 1 ;
             ReadInt( Header, format('EXCSEQ%dDF%d=',[iSeq,iWav]), MainFrm.EXCSequence[iWav,iSeq].DivideFactor) ;
             end ;
         end ;

     // Read wavelengths table
     for i := 0 to High(MainFrm.EXCWavelengths) do begin
         ReadInt( Header, format('EXCWC%d=',[i]), MainFrm.EXCWavelengths[i].Centre) ;
         ReadInt( Header, format('EXCWW%d=',[i]), MainFrm.EXCWavelengths[i].Width) ;
         end ;

     ReadFloat( Header, 'EXCSPSTARTW=', MainFrm.EXCSpectrumStartWavelength ) ;
     ReadFloat( Header, 'EXCSPENDW=', MainFrm.EXCSpectrumEndWavelength ) ;
     ReadFloat( Header, 'EXCSBANDW=', MainFrm.EXCSpectrumBandwidth ) ;
     ReadFloat( Header, 'EXCSPSTEPS=', MainFrm.EXCSpectrumStepSize ) ;

     // Stimulus program file
     ReadString( Header, 'STIMFILE=', MainFrm.StimFileName) ;

     // Photo stimulus program file
     ReadString( Header, 'PHOTOSTIMFIL=', MainFrm.PhotoStimFileName) ;

     // Fluophore binding equations table
     for i := 0 to MainFrm.IDRFile.MaxEquations-1 do begin
         ReadLogical( Header, format('EQNUSE%d=',[i]), mainFrm.BindingEquations[i].InUse ) ;
         ReadString( Header, format('EQNION%d=',[i]), mainFrm.BindingEquations[i].Ion) ;
         ReadString( Header, format('EQNUN%d=',[i]), mainFrm.BindingEquations[i].Units) ;
         ReadString( Header, format('EQNNAM%d=',[i]), mainFrm.BindingEquations[i].Name) ;
         ReadFloat( Header, format('EQNRMAX%d=',[i]), mainFrm.BindingEquations[i].RMax) ;
         ReadFloat( Header, format('EQNRMIN%d=',[i]), mainFrm.BindingEquations[i].RMin) ;
         ReadFloat( Header, format('EQNKEFF%d=',[i]), mainFrm.BindingEquations[i].KEff) ;
         end ;

     // Printer page settings
     ReadInt( Header, 'PRTM=', MainFrm.PrinterTopMargin )  ;
     ReadInt( Header, 'PRBM=', MainFrm.PrinterBottomMargin )  ;
     ReadInt( Header, 'PRLM=', MainFrm.PrinterLeftMargin )  ;
     ReadInt( Header, 'PRRM=', MainFrm.PrinterRightMargin )  ;
     ReadInt( Header, 'PRLT=', MainFrm.PrinterLineThickness )  ;
     ReadInt( Header, 'PRMS=', MainFrm.PrinterMarkerSize )  ;
     ReadLogical( Header, 'PRUC=', MainFrm.PrinterUseColor ) ;
     ReadString( Header, 'PRFN=', MainFrm.PrinterFontName ) ;
     ReadInt( Header, 'PRFS=', MainFrm.PrinterFontSize )  ;

     ReadString( Header, 'DDIR=', MainFrm.DataDirectory ) ;

     ReadString( Header, 'VPDIR=', MainFrm.VProtDirectory ) ;
     // Use default if voltage protocol directory does not exist
     if not DirectoryExists(MainFrm.VProtDirectory) then begin
        MainFrm.VProtDirectory := MainFrm.DefVProtDirectory ;
        end ;

     for i := 0 to High(MainFrm.RecentFiles) do
         ReadString(Header,format('FILE%d=',[i]),MainFrm.RecentFiles[i]) ;

     // Input/output/control line configuration
     ReadInt( Header, 'IOADCI=', MainFrm.IOConfig.ADCIn ) ;
     ReadInt( Header, 'IOCAMS=', MainFrm.IOConfig.CameraStart ) ;
     ReadLogical( Header, 'IOCAMSAH=', MainFrm.IOConfig.CameraStartActiveHigh ) ;

     // Command voltage O/P lines 1 & 2
     ReadInt( Header, 'IOVCOM=', MainFrm.IOConfig.VCommand[0] ) ;
     // Above line for compatibility with versions earlier than V2.4.3
     ReadInt( Header, 'IOVCOM0=', MainFrm.IOConfig.VCommand[0] ) ;
     ReadInt( Header, 'IOVCOM1=', MainFrm.IOConfig.VCommand[1] ) ;
     ReadInt( Header, 'IOVCOM2=', MainFrm.IOConfig.VCommand[2] ) ;

     ReadInt( Header, 'IOLSSU=', MainFrm.IOConfig.LSShutter ) ;
     ReadLogical( Header, 'IOLSSUAH=', MainFrm.IOConfig.LSShutterActiveHigh ) ;
     ReadInt( Header, 'IOLSWS=', MainFrm.IOConfig.LSWavelengthStart ) ;
     ReadInt( Header, 'IOLSWE=', MainFrm.IOConfig.LSWavelengthEnd ) ;
     ReadInt( Header, 'IOLSLS=', MainFrm.IOConfig.LSLaserStart ) ;
     ReadInt( Header, 'IOLSLE=', MainFrm.IOConfig.LSLaserEnd ) ;
     ReadInt( Header, 'IODSTA=', MainFrm.IOConfig.DigitalStimStart ) ;
     ReadInt( Header, 'IODEND=', MainFrm.IOConfig.DigitalStimEnd ) ;
     ReadInt( Header, 'IOPSX=', MainFrm.IOConfig.PhotoStimX ) ;
     ReadInt( Header, 'IOPSY=', MainFrm.IOConfig.PhotoStimY ) ;
     ReadInt( Header, 'IOPSI1=', MainFrm.IOConfig.PhotoStimI1 ) ;
     ReadInt( Header, 'IOPSI2=', MainFrm.IOConfig.PhotoStimI2 ) ;
     ReadInt( Header, 'IOPSI3=', MainFrm.IOConfig.PhotoStimI3 ) ;

     // Photo-stimulus Pockels cell and shutter configuration
     ReadInt( Header, 'IOPSMETER=', MainFrm.IOConfig.PhotoStimMeter ) ;
     ReadInt( Header, 'IOPSSU=', MainFrm.IOConfig.PhotoStimShutter ) ;
     ReadFloat( Header, 'IOPSSLA=', MainFrm.IOConfig.PhotoStimShutterLatency ) ;
     ReadLogical( Header, 'IOPSSUAH=', MainFrm.IOConfig.PhotoStimShutterActiveHigh ) ;
     ReadLogical( Header, 'IOPSPCMAN=', MainFrm.IOconfig.PhotoStimPowerCalManual ) ;
     ReadFloat( Header, 'IOPSMETERR=', MainFrm.IOConfig.PhotoStimMeterRange ) ;
     ReadFloat( Header, 'IOPSMETERS=', MainFrm.IOconfig.PhotoStimMeterScale ) ;

     ReadInt( Header, 'IOCLKSYNC=', MainFrm.IOConfig.ClockSyncLine ) ;


     // Event detection settings
{     ReadFloat( Header, 'EVANDEADTIME=', MainFrm.EventAnalysis.DeadTime ) ;
     ReadFloat( Header, 'EVANTHRESHOLD=', MainFrm.EventAnalysis.DetectionThreshold ) ;
     ReadFloat( Header, 'EVANTHRESHDUR=', MainFrm.EventAnalysis.ThresholdDuration ) ;
     ReadInt( Header, 'EVANPOLARITY=', MainFrm.EventAnalysis.DetectionThresholdPolarity ) ;
     ReadLogical( Header, 'EVANFIXBASE=', MainFrm.EventAnalysis.FixedBaseline ) ;
     ReadInt( Header, 'EVANSOURCE=', MainFrm.EventAnalysis.DetectionSource ) ;
     ReadFloat( Header, 'EVANDISPLAYMAX=', MainFrm.EventAnalysis.DisplayMax ) ;
     ReadFloat( Header, 'EVANROLLBASEPERIOD=', MainFrm.EventAnalysis.RollingBaselinePeriod ) ;}

     // Seal test settings
     ReadInt( Header, 'SEALTUSE=',MainFrm.SealTest.Use ) ;
     ReadFloat( Header, 'SEALTPH1=',MainFrm.SealTest.PulseHeight1 ) ;
     ReadFloat( Header, 'SEALTHV1=',MainFrm.SealTest.HoldingVoltage1 ) ;
     ReadFloat( Header, 'SEALTPH2=',MainFrm.SealTest.PulseHeight2 ) ;
     ReadFloat( Header, 'SEALTHV2=',MainFrm.SealTest.HoldingVoltage2 ) ;
     ReadFloat( Header, 'SEALTPH3=',MainFrm.SealTest.PulseHeight3 ) ;
     ReadFloat( Header, 'SEALTHV3=',MainFrm.SealTest.HoldingVoltage3 ) ;
     ReadFloat( Header, 'SEALTPW=',MainFrm.SealTest.PulseWidth ) ;

     // Photo-stimulus settings
     ReadFloat( Header, 'PSPER=',MainFrm.PhotoStim.Period ) ;
     ReadLogical( Header, 'PSREP=',MainFrm.PhotoStim.RepeatedStim ) ;
     ReadInt( Header, 'PSNSTIMP=',MainFrm.PhotoStim.NumStimPoints ) ;
     ReadInt( Header, 'PSATTEN=',MainFrm.PhotoStim.Attenuator ) ;
     ReadFloat( Header, 'PSROTAT=',MainFrm.PhotoStim.ImageRotation ) ;
     ReadFloat( Header, 'PSMICPERPX=',MainFrm.PhotoStim.XMicronsPerPixel ) ;
     ReadFloat( Header, 'PSMICPERPY=',MainFrm.PhotoStim.YMicronsPerPixel ) ;
     ReadString( Header, 'PSPVLOG=',MainFrm.PhotoStim.PVLogFile ) ;
     ReadLogical( Header, 'PSREFLE=',MainFrm.PhotoStim.RefLineEnabled ) ;
     ReadLogical( Header, 'PSCMDTERM=',MainFrm.PhotoStim.CmdTermZero ) ;
     for i := 1 to 3 do
     begin
       ReadFloat( Header, format('PSXC%d=',[i]), MainFrm.PhotoStim.XCenter[i] ) ;
       ReadFloat( Header, format('PSXS%d=',[i]), MainFrm.PhotoStim.XScale[i] ) ;
       ReadFloat( Header, format('PSYC%d=',[i]), MainFrm.PhotoStim.YCenter[i] ) ;
       ReadFloat( Header, format('PSYS%d=',[i]), MainFrm.PhotoStim.YScale[i] ) ;
       ReadLogical( Header, format('PSPCPENA%d=',[i]), MainFrm.PhotoStim.PCEnable[i] ) ;
       ReadFloat( Header, format('PSPCPMIN%d=',[i]), MainFrm.PhotoStim.PCPowerMin[i] ) ;
       ReadFloat( Header, format('PSPCPMAX%d=',[i]), MainFrm.PhotoStim.PCPowerMax[i] ) ;
       ReadFloat( Header, format('PSPCBIAS%d=',[i]), MainFrm.PhotoStim.PCBias[i] ) ;
       ReadFloat( Header, format('PSPCVPI%d=',[i]), MainFrm.PhotoStim.PCVoltagePi[i] ) ;
       ReadLogical( Header, format('PSPCPC%d=',[i]), MainFrm.PhotoStim.PCPolarizationCross[i] ) ;
       ReadLogical( Header, format('PSPC302%d=',[i]), MainFrm.PhotoStim.PCConoptics302[i] ) ;
       ReadFloat( Header, format('PSLPMIN%d=',[i]), MainFrm.PhotoStim.LinearPowerMin[i] ) ;
       ReadFloat( Header, format('PSLPMAX%d=',[i]), MainFrm.PhotoStim.LinearPowerMax[i] ) ;
       ReadFloat( Header, format('PSLVMIN%d=',[i]), MainFrm.PhotoStim.LinearVoltageMin[i] ) ;
       ReadFloat( Header, format('PSLVMAX%d=',[i]), MainFrm.PhotoStim.LinearVoltageMax[i] );
       ReadLogical( Header, format('PSPCSHU%d=',[i]), MainFrm.PhotoStim.EnableShutter[i] ) ;
     end;

     bValue := MainFrm.mnDisplayGrid.Checked ;
     ReadLogical( Header, 'DISPLAYGRID=', bValue) ;
     MainFrm.mnDisplayGrid.Checked := bValue ;

     // Display contrast settings optimisation
     ReadLogical( Header, 'CNCAFT=', MainFrm.ContrastChangeAllFrameTypes ) ;
     ReadLogical( Header, 'CNAUTOOP=', MainFrm.ContrastAutoOptimise ) ;
     ReadLogical( Header, 'CN6SD=', MainFrm.Contrast6SD ) ;

     // Read visibility state of channels in RecADCOnlyUnit
     // Modified by NS 19 March 2009
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
        ReadLogical( Header, format('CVRADCO%d=',[ch]), MainFrm.ADCChannelRecADCOnlyUnitVisible[ch]) ;
     end ;

     // Read visibility state of channels in Sealtest
     // Modified by NS 24 March 2009
     MainFrm.ADCChannelSealtestNumberOfChannels := 0 ;
     ReadInt( Header, 'CVSTNUM=', MainFrm.ADCChannelSealtestNumberOfChannels ) ;
     for ch := 0 to MainFrm.ADCChannelSealtestNumberOfChannels-1 do begin
        ReadLogical( Header, format('CVST%d=',[ch]), MainFrm.ADCChannelSealtestVisible[ch] ) ;
     end ;

     // Read state of SmoothDifferentiate window
     // Modified by NS 10 April 2009
     for ch := 0 to (4 - 1) do begin
        ReadLogical( Header, format('SDWCH%d=',[ch]), MainFrm.SmoothDifferentiateUnitVisible[ch] ) ;
     end;
     ReadInt( Header, 'SDWMA=', MainFrm.SmoothDifferentiateUnitMADataWindow ) ;
     ReadInt( Header, 'SDWDX=', MainFrm.SmoothDifferentiateUnitMADXWindow ) ;

     // Read state of DynamicProtocol window
     // Modified by NS 22 December 2009
     ReadInt( Header, 'DPCH=', MainFrm.DynamicProtocol.SelectedChannel ) ;
     ReadInt( Header, 'DPDIR=', MainFrm.DynamicProtocol.Direction ) ;
     ReadFloat( Header, 'DPTHRES=', MainFrm.DynamicProtocol.Threshold ) ;
     ReadFloat( Header, 'DPDUR=', MainFrm.DynamicProtocol.Duration ) ;
     ReadLogical( Header, 'DPEP=', MainFrm.DynamicProtocol.EPRestart ) ;
     ReadLogical( Header, 'DPPS=', MainFrm.DynamicProtocol.PSRestart ) ;
     ReadString( Header, 'DPEPF=',MainFrm.DynamicProtocol.EPStimFileName ) ;
     ReadInt( Header, 'DPEPI=', MainFrm.DynamicProtocol.EPStimIndex ) ;
     ReadString( Header, 'DPPSF=',MainFrm.DynamicProtocol.PSStimFileName ) ;
     ReadInt( Header, 'DPPSI=', MainFrm.DynamicProtocol.PSStimIndex ) ;

     // Auto reset interface cards
     ReadLogical( Header, 'ARI=', MainFrm.AutoResetInterfaceCards ) ;

     ReadLogical( Header, 'STARTSTIMONREC=', MainFrm.StartStimOnRecord ) ;

     // Read Z Stage control settings
     ZStage.ReadSettings( Header ) ;

     FileClose( INIFileHandle ) ;

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
                                 False ) ;

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
                                       True ) ;
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
                                       True ) ;
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


end.
