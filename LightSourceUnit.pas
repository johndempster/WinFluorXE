unit LightSourceUnit;
// -----------------------------------------------
// Excitation Light Source Module
// -----------------------------------------------
// (c) J. Dempster, University of Strathclyde 2004
// 18.08.04 V1.0
// 07.09.04 V1.1 User wavelength calibration added
// 15.12.04 V1.2 Simpled voltage-driven LED source added
// 22.03.05 .WavelengthMin and .WavelengthMax functions added
// 11.09.05 Now supports Optoscans with 1800 and 2000 lpm gratings
// 17.02.06 Optoscan + lasers supported added for Tom Carter TIRF systems
// 04.04.06 Now works with 2000 line Optoscan (voltage offset added)
// 29.08.06 ShutterBlankingTime added
// 10.01.07 Optoscan + lasers updated to support user configurable lasers
//          Laser intensity no longer derived from bandwidth
// 16.01.08 Upper wavelength limit for Till monochromators now 693.5 nm
//          LED light source updated, can now output user-defined values for LED on and off
// 24.01.08 Coding for "Optoscan + Lasers" modified to allow monochromator wavelength combined with
//          any combination of lasers
// 04.04.08 Cairn Optoscan 1800 mono now has correct voltage offset
// 16.04.08 Sutter DG4 fast filter changer added
// 12.05.08 NumVControl lines now returned consistently for all light sources
// 19.01.09 ShutterClosedWavelength added. Shutter closed wavelength can now be set by user.
// 09.03.09 VControl array now a record which includes DAC/DIG channel, delay and device
// 19.03.09 JD OptoScanWithLasers, Optoscan bandwidth kept constant when lasers selected
//             Laser 1 shutter now works
// 15.12.10 JD Support for Cairn TIRF system (supplied to Nigel Emptage) added
// 23.05.13 JD Till monochromator wavelength range now 0-10000
// 31.05.13 JD Monochromator + Laser/LED light source added
// 03.06.13 JD Monochromator + Laser/LED light source now working
// 27.09.13 JD .ShutterChangeTime property added
// 18.02.14 JD Now used osLibrary64.dll and osLibrary32.dll
// 11.09.14 JD os_In_Slit_Bandwidth_To_Width and os_Out_Slit_Bandwidth_To_Width
//             now includes Wavelength to fix crashes with 64 versions and Optoscan
// 16.09.14 JD osLibrary.inc now incorporated in to this unit.
// 23.10.14 JD LightSource.DeviceType now a property rather than public variable
//             Allows default settings of Optoscan to be loaded by SetDeviceType
// 02.12.14 JD LEDFilterNumToVoltage() No. LEDS no longer restricted to 4.
//             lsMaxLasers increased from 3 to 8
//             Light source intensities default to 50%
// 17.02.15 JD Eight light source control lines can now be set individually to DAC or DIG outputs

interface

uses
  Windows,SysUtils, Classes, math, dialogs, strutils ;

{$IFDEF WIN64}

// Calls to 64 bit DLL
procedure os_Set_Log(Strings: TStrings);stdcall; external 'osLibrary64.dll';

//General osLibrary functions
function os_Library_Version: Double;stdcall; external 'osLibrary64.dll';
function os_Library_Version_Str: PANSIChar;stdcall; external 'osLibrary64.dll';
function os_Wide_Error_String(ECode: DWord): lpWStr;stdcall; external 'osLibrary64.dll';
function os_Char_Error_String(ECode: DWord): lpStr;stdcall; external 'osLibrary64.dll';
function os_Wavelength_To_RGB(const Wavelength: Double; var R,G,B: Byte): DWord;stdcall; external 'osLibrary64.dll';
function os_Wavelength_To_Voltage(Wavelength: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_In_Slit_Bandwidth_To_Width(Wavelength: Double;Slit: PDouble): DWord;stdcall;  external 'osLibrary64.dll';
function os_Out_Slit_Bandwidth_To_Width(Wavelength: Double;Slit: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_In_Slit_Width_To_Voltage(Slit: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_Out_Slit_Width_To_Voltage(Slit: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_In_Slit_Width_To_Bandwidth(Wavelength: Double;Slit: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_Out_Slit_Width_To_Bandwidth(Wavelength: Double;Slit: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_Query_IO(Reserved: DWord; Data: lpStr; Size: DWord): DWord;stdcall; external 'osLibrary64.dll';
function os_Open_IO(Reserved: DWord; Data: lpStr): DWord;stdcall; external 'osLibrary64.dll';
function os_Switch_Control(Value: Bool): DWord;stdcall; external 'osLibrary64.dll';
function os_Close_IO: DWord;stdcall; external 'osLibrary64.dll';
function os_Valid_Params(Wavelength,InSlit,OutSlit: Double): DWord;stdcall; external 'osLibrary64.dll';
function os_Set_Params(Wavelength,InSlit,OutSlit: Double): DWord;stdcall; external 'osLibrary64.dll';
function os_Get_Ready(Wavelength,InSlit,OutSlit: PBool): DWord;stdcall; external 'osLibrary64.dll';
function os_Shut_In_Slit(Value: Bool): DWord;stdcall; external 'osLibrary64.dll';
function os_Shut_Grating(Value: Bool): DWord;stdcall; external 'osLibrary64.dll';
function os_Shut_Out_Slit(Value: Bool): DWord;stdcall; external 'osLibrary64.dll';
function os_Shut_Both_Slits(Value: Bool): DWord;stdcall; external 'osLibrary64.dll';
function os_Num_Driver_Errors: DWord;stdcall; external 'osLibrary64.dll';
function os_Last_Driver_Error: lpStr;stdcall; external 'osLibrary64.dll';
function os_Slits_Driven(InSlit,OutSlit: PBool): DWord;stdcall;  external 'osLibrary64.dll';
function os_Max_Wavelength(Wavelength: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_Min_Wavelength(Wavelength: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_Max_InSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_Min_InSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_Max_OutSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_Min_OutSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary64.dll';
function os_Grating_Lines(Lines: PDWord): DWord;stdcall; external 'osLibrary64.dll';
function os_Set_Grating_Lines(Lines: DWord): DWord;stdcall; external 'osLibrary64.dll';
function os_Load_Interface_Defaults(FileName: PANSIChar): DWord;stdcall; external 'osLibrary64.dll';
//function os_Load_DAC_Defaults(FileName: PANSIChar): DWord;stdcall; external 'osLibrary64.dll';
function os_Get_Handle(Handle: PDWord): DWord;stdcall; external 'osLibrary64.dll';
function os_Set_Handle(Handle: DWord): DWord;stdcall; external 'osLibrary64.dll';

{$ELSE}

// Calls to 32 bit DLL
procedure os_Set_Log(Strings: TStrings);stdcall; external 'osLibrary32.dll';

//General osLibrary functions
function os_Library_Version: Double;stdcall; external 'osLibrary32.dll';
function os_Library_Version_Str: PANSIChar;stdcall; external 'osLibrary32.dll';
function os_Wide_Error_String(ECode: DWord): lpWStr;stdcall; external 'osLibrary32.dll';
function os_Char_Error_String(ECode: DWord): lpStr;stdcall; external 'osLibrary32.dll';
function os_Wavelength_To_RGB(const Wavelength: Double; var R,G,B: Byte): DWord;stdcall; external 'osLibrary32.dll';
function os_Wavelength_To_Voltage(Wavelength: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_In_Slit_Bandwidth_To_Width(Wavelength: Double;Slit: PDouble): DWord;stdcall;  external 'osLibrary32.dll';
function os_Out_Slit_Bandwidth_To_Width(Wavelength: Double;Slit: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_In_Slit_Width_To_Voltage(Slit: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_Out_Slit_Width_To_Voltage(Slit: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_In_Slit_Width_To_Bandwidth(Wavelength: Double;Slit: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_Out_Slit_Width_To_Bandwidth(Wavelength: Double;Slit: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_Query_IO(Reserved: DWord; Data: lpStr; Size: DWord): DWord;stdcall; external 'osLibrary32.dll';
function os_Open_IO(Reserved: DWord; Data: lpStr): DWord;stdcall; external 'osLibrary32.dll';
function os_Switch_Control(Value: Bool): DWord;stdcall; external 'osLibrary32.dll';
function os_Close_IO: DWord;stdcall; external 'osLibrary32.dll';
function os_Valid_Params(Wavelength,InSlit,OutSlit: Double): DWord;stdcall; external 'osLibrary32.dll';
function os_Set_Params(Wavelength,InSlit,OutSlit: Double): DWord;stdcall; external 'osLibrary32.dll';
function os_Get_Ready(Wavelength,InSlit,OutSlit: PBool): DWord;stdcall; external 'osLibrary32.dll';
function os_Shut_In_Slit(Value: Bool): DWord;stdcall; external 'osLibrary32.dll';
function os_Shut_Grating(Value: Bool): DWord;stdcall; external 'osLibrary32.dll';
function os_Shut_Out_Slit(Value: Bool): DWord;stdcall; external 'osLibrary32.dll';
function os_Shut_Both_Slits(Value: Bool): DWord;stdcall; external 'osLibrary32.dll';
function os_Num_Driver_Errors: DWord;stdcall; external 'osLibrary32.dll';
function os_Last_Driver_Error: lpStr;stdcall; external 'osLibrary32.dll';
function os_Slits_Driven(InSlit,OutSlit: PBool): DWord;stdcall;  external 'osLibrary32.dll';
function os_Max_Wavelength(Wavelength: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_Min_Wavelength(Wavelength: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_Max_InSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_Min_InSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_Max_OutSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_Min_OutSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary32.dll';
function os_Grating_Lines(Lines: PDWord): DWord;stdcall; external 'osLibrary32.dll';
function os_Set_Grating_Lines(Lines: DWord): DWord;stdcall; external 'osLibrary32.dll';
function os_Load_Interface_Defaults(FileName: PANSIChar): DWord;stdcall; external 'osLibrary32.dll';
//function os_Load_DAC_Defaults(FileName: PANSIChar): DWord;stdcall; external 'osLibrary32.dll';
function os_Get_Handle(Handle: PDWord): DWord;stdcall; external 'osLibrary32.dll';
function os_Set_Handle(Handle: DWord): DWord;stdcall; external 'osLibrary32.dll';
{$ENDIF}


const
    lsNone = 0 ;
    lsOptoScan1200 = 1 ;
    lsOptoScan1800 = 2 ;
    lsOptoScan2000 = 3 ;
    lsTill = 4 ;
    lsLambda10 = 5 ;
    lsLED = 6 ;
    lsOptoscanWithLasers = 7 ;
    lsSutterDG4 = 8 ;
    lsCairnTIRF = 9 ;
    lsTillWithLasers = 10 ;
    lsMaxVControl = 199 ;

    lsMaxLightSources = 8 ;
    lsMaxTIRFGalvos = 2 ;

type

  TLSVControl = record
      V : Single ;
      Delay : Single ;
      Device : Integer ;
      Chan : Integer ;
      Name : String ;
      ResourceType : Integer ;
      end ;

  TLightSource = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FDeviceType : Integer ;        // Light source device in use
//    NumVControlInUse : Integer ;
    VControlInUse : Array[0..lsMaxVControl] of TLSVcontrol ;
    LastOptoscanBandwidth : Single ;

    procedure OptoScanWavelengthToVoltage( Wavelength : Single ;
                                           BandWidth : Single
                                           ) ;

    procedure TillWavelengthToVoltage(Wavelength : Single ) ;

    procedure TillWithLasersWavelengthToVoltage(Wavelength : Single) ;

    procedure Lambda10FilterNumToVoltage(FilterNum : Integer) ;

    procedure LEDFilterNumToVoltage( FilterNum : Integer) ;

    procedure OptoScanWithLasersWavelengthToVoltage(
              Wavelength : Single ;
              BandWidth : Single
              ) ;

    function LaserVoltage(
         LaserNum : Integer ;         // Laser # 1 or 2
         PercentageIntensity : Single // % of maximum intensity
         ) : Single ;

    procedure SutterDG4FilterNumToVoltage(
              FilterNum : Integer
              ) ;

    procedure CairnTIRFPositionToVoltage(
              GalvoPosition : Single ) ;

    procedure SetDeviceType( Value : Integer ) ;


  public
    { Public declarations }

    Wavelength1 : Single ;
    Voltage1 : Single ;
    Wavelength2 : Single ;
    Voltage2 : Single ;

    LEDOffVoltage : Single ; // Voltage at which LED is off
    LEDMaxVoltage : Single ; // Voltaged at which LED output is maximal

    ShutterClosedWavelength : Single ; // Wavelength selected when shutter is closed
    ShutterBlankingPeriod : Single ;   // Closed period at end of wavelength cycle
    ShutterChangeTime : Single ;       // Shutter open/close change time (s)

    // User defined laser settings (for lsOptoScanWithLasers2)
    LaserWavelength: Array[0..lsMaxLightSources-1] of Single ;
    LaserDelay: Array[0..lsMaxLightSources-1] of Single ;
    LaserOffVoltage: Array[0..lsMaxLightSources-1] of Single ;
    LaserOnVoltage: Array[0..lsMaxLightSources-1] of Single ;
    LaserIntensity: Array[0..lsMaxLightSources-1] of Single ;

    // TIRF angle galvo control voltages
    TIRFOff: Array[1..lsMaxTIRFGalvos] of Single ; // Off position
    TIRFOn: Array[1..lsMaxTIRFGalvos] of Single ;  // TIRF position
    TIRFWF: Array[1..lsMaxTIRFGalvos] of Single ;  // Wide field position

    EMFilterChangeTime : single ;                 // Emission filter change time (s)

    // Get list of supported light source devices
    procedure GetList( List : TStrings ) ;

    // Get control voltages for selected wavelength/bandwidth
    procedure WavelengthToVoltage(
              FilterNum : Integer ;
              Wavelength : Single ;
              BandWidth : Single
              ) ;

    procedure ShutterClosedVoltages ;

    function WavelengthChangeTime : Single ;
    function WavelengthMin : Single ;
    function WavelengthMax : Single ;
    function UserCalibrationRequired : Boolean ;
    //function LaserSettingsRequired : Boolean ;
    function LEDSettingsRequired : Boolean ;
    function TIRFSettingsRequired : Boolean ;
    function ShutterControlRequired : Boolean ;
    function DACOutputsRequired : Boolean ;
    function ShutterClosedWavelengthRequired : Boolean ;
    function ShutterBlankingTime : Single ;
    function ControlLineName( Num : Integer ) : String ;
    function VariableIntensitySource( iLine : Integer ) : Boolean ;
    function NumControlLines : Integer ;

    Property DeviceType : Integer read FDeviceType write SetDeviceType ;

  end;

var
  LightSource : TLightSource ;

implementation

uses Main, LabIOUnit;

{$R *.dfm}

procedure TLightSource.GetList( List : TStrings ) ;
// -----------------------------------
// Get list of supported light sources
// -----------------------------------
begin

     List.Clear ;
     List.AddObject('None',TObject(lsNone)) ;
     List.AddObject('Cairn Optoscan (1200)',TObject(lsOptoScan1200)) ;
     List.AddObject('Cairn Optoscan (1800)',TObject(lsOptoScan1800 )) ;
     List.AddObject('Cairn Optoscan (2000)',TObject(lsOptoScan2000)) ;
     List.AddObject('PTI/Till monochromator',TObject(lsTill)) ;
     List.AddObject('Sutter Lambda-10',TObject(lsLambda10)) ;
     List.AddObject('LED',TObject(lsLED)) ;
     List.AddObject('Cairn OptoScan + LED/Lasers',TObject(lsOptoscanWithLasers)) ;
     List.AddObject('Sutter DG4',TObject(lsSutterDG4)) ;
     List.AddObject('Cairn TIRF',TObject(lsCairnTIRF)) ;
     List.AddObject('PTI/Till monochromator + LED/Laser',TObject(lsTillWithLasers)) ;

     end ;

function TLightSource.ControlLineName( Num : Integer ) : String ;
// ----------------------------------------
// Return name of light source control line
// ----------------------------------------
var
    i : Integer ;
    Names : Array[0..7] of string ;
begin

   for i := 0 to High(Names) do Names[i] := '' ;

   case FDeviceType of
     lsOptoScan1200,
     lsOptoScan1800,
     lsOptoScan2000 : begin
        Names[0] := 'Input Slit' ;
        Names[1] := 'Wavelength' ;
        Names[2] := 'Output Slit' ;
        end ;
     lsOptoscanWithLasers : begin
        Names[0] := 'Input Slit' ;
        Names[1] := 'Wavelength' ;
        Names[2] := 'Output Slit' ;
        Names[3] := 'LED/Laser1' ;
        Names[4] := 'LED/Laser2' ;
        Names[5] := 'LED/Laser3' ;
        Names[6] := 'Laser #1 Shutter:' ;
        end ;
     lsTill : begin
        Names[0] := 'Wavelength:' ;
        end ;
     lsTillWithLasers : begin
        Names[0] := 'Wavelength:' ;
        Names[1] := 'LED/Laser1' ;
        end ;
     lsLambda10 : begin
       for i := 0 to High(Names) do Names[i] := format('Line%d:',[i]);
       end ;
     lsSutterDG4 : begin
        Names[0] := 'Line 0' ;
        Names[1] := 'Line 1' ;
        Names[2] := 'Line 2' ;
       end ;
     lsLED : begin
        for i := 0 to High(Names) do Names[i] := format('LED%d',[i]);
        end ;
     lsCairnTIRF : begin
        Names[0] := 'Galvo 1' ;
        Names[1] := 'Galvo 2' ;
       end ;
    end;
   if (Num >=0) and (Num <= High(Names)) then Result := Names[Num]
                                         else Result := '' ;
   end;


function TLightSource.WavelengthChangeTime : Single ;
// -------------------------------
// Time taken to change wavelength
// -------------------------------
begin
    case FDeviceType of
        lsOptoScan1200,
        lsOptoScan1800,
        lsOptoScan2000,
        lsOptoscanWithLasers : Result := 0.001 ;
        lsTill : Result := 0.001 ;
        lsTillWithLasers : Result := 0.001 ;
        lsLambda10 : Result := 0.1 ;
        lsLED : Result := 0.0 ;
        lsSutterDG4 : Result := 0.001 ;
        lsCairnTIRF : Result := 0.001 ;
        else begin
             Result := 0 ;
             end ;
        end ;
    end ;


function TLightSource.NumControlLines : Integer ;
// -----------------------------------------
// No. of control lines used by light source
// ------------------------------------------
begin
    case FDeviceType of
        lsOptoScan1200,
        lsOptoScan1800,
        lsOptoScan2000,
        lsOptoscanWithLasers : Result := 7 ;
        lsTill : Result := 1 ;
        lsTillWithLasers : Result := 2 ;
        lsLambda10 : Result := 3 ;
        lsLED : Result := 8 ;
        lsSutterDG4 : Result := 3 ;
        lsCairnTIRF : Result := 2 ;
        else Result := 0 ;
        end ;
    end ;



function TLightSource.WavelengthMin : Single ;
// ------------------
// Minimum wavelength
// -------------------
begin
    case FDeviceType of

        lsOptoScan1200,lsOptoscanWithLasers : begin
            os_Set_Grating_Lines( 1200 ) ;
            //os_Min_Wavelength(@MinWavelength) ;
            Result := 300 ; //MinWavelength ;
            end ;

        lsOptoScan1800 : begin
            os_Set_Grating_Lines( 1800 ) ;
            //os_Min_Wavelength(@MinWavelength) ;
            Result := 300 ; //MinWavelength ;
            end ;

        lsOptoScan2000 : begin
            os_Set_Grating_Lines( 2000 ) ;
            //os_Min_Wavelength(@MinWavelength) ;
            Result := 300 ; //MinWavelength ;
            end ;

        lsTill : Result := 0.0 ;
        lsTillWithLasers : Result := 0.0 ;
        lsLambda10 : Result := 0.0 ;
        lsLED : Result := 0.0 ;
        lsSutterDG4 : Result := 0.0 ;
        lsCairnTIRF : Result := 0.0 ;
        else begin
             Result := 0 ;
             end ;
        end ;
    end ;


Function TLightSource.WavelengthMax : Single ;
// ------------------
// Maximum wavelength
// -------------------
begin
    case FDeviceType of

        lsOptoScan1200 : begin
            os_Set_Grating_Lines( 1200 ) ;
            //os_Max_Wavelength(@MaxWavelength) ;
            Result := 750 ; //MaxWavelength ;
            end ;

        lsOptoScan1800 : begin
            os_Set_Grating_Lines( 1800 ) ;
            //os_Max_Wavelength(@MaxWavelength) ;
            Result := 750 ; //MaxWavelength ;
            end ;

        lsOptoScan2000 : begin
            os_Set_Grating_Lines( 2000 ) ;
            //os_Max_Wavelength(@MaxWavelength) ;
            Result := 750 ; //MaxWavelength ;
            end ;

        lsOptoscanWithLasers : begin
            os_Set_Grating_Lines( 1200 ) ;
            //os_Max_Wavelength(@MaxWavelength) ;
            Result := 3000 ; //MaxWavelength ;
            end ;

        lsTill : Result := 10000.0 ; // 23/5/13 widened to allow other monos original value 550; changed 14.12.2007 M.Ascherl
        lsTillWithLasers : Result := 10000.0 ;
        lsLambda10 : Result := 0.0 ;
        lsLED : Result := 0.0 ;
        lsSutterDG4 : Result := 0.0 ;
        lsCairnTIRF : Result := 0.0 ;
        else begin
             Result := 0 ;
             end ;
        end ;
    end ;


function TLightSource.UserCalibrationRequired : Boolean ;
// ---------------------------------------------------------
// Returns TRUE if user calibration of light source required
// ---------------------------------------------------------
begin
    case FDeviceType of
        lsTill : Result := True ;
        lsTillWithLasers : Result := True ;
        else Result := False ;
        end ;
    end ;


function TLightSource.LEDSettingsRequired : Boolean ;
// ---------------------------------------------------------
// Returns TRUE if used must supply LED on/off voltages
// ---------------------------------------------------------
begin
    case FDeviceType of
        lsLED : Result := True ;
        else Result := False ;
        end ;
    end ;


function TLightSource.TIRFSettingsRequired : Boolean ;
// ---------------------------------------------------------
// Returns TRUE if used must TIRF galvo voltage settings
// ---------------------------------------------------------
begin
    case FDeviceType of
        lsCairnTIRF : Result := True ;
        else Result := False ;
        end ;
    end ;


function TLightSource.ShutterControlRequired : Boolean ;
// ----------------------------------------------------------------
// Returns TRUE if shutter control line required by light source
// ----------------------------------------------------------------
begin
    case FDeviceType of
        lsOptoScan1200,lsOptoScan1800,lsOptoScan2000 : Result := false ;
        lsOptoscanWithLasers : Result := false ;
        lsTill : Result := false ;
        lsTillWithLasers : Result := False ;
        lsLambda10 : Result := True ;
        lsLED : Result := false ;
        lsSutterDG4 : Result := False ;
        else Result := False ;
        end ;
    end ;


function TLightSource.DACOutputsRequired : Boolean ;
// ----------------------------------------------------------------
// Returns TRUE if DAC outputs required for controlling light source
// ----------------------------------------------------------------
begin
    case FDeviceType of
        lsOptoScan1200,lsOptoScan1800,lsOptoScan2000 : Result := True ;
        lsOptoscanWithLasers : Result := True ;
        lsTill : Result := True ;
        lsTillWithLasers : Result := True ;
        lsLambda10 : Result := False ;
        lsLED : Result := false ;
        lsCairnTIRF : Result := True ;
        else Result := False ;
        end ;
    end ;


function TLightSource.ShutterClosedWavelengthRequired : Boolean ;
// ----------------------------------------------------------------
// Returns TRUE if shutter closed wavelength supported by light source
// ----------------------------------------------------------------
begin
    case FDeviceType of
        lsOptoScan1200,lsOptoScan1800,lsOptoScan2000 : Result := True ;
        lsOptoscanWithLasers : Result := True ;
        lsTill : Result := True ;
        lsTillWithLasers : Result := True ;
        lsLambda10 : Result := False ;
        lsLED : Result := False ;
        lsSutterDG4 : Result := False ;
        lsCairnTIRF : Result := True ;
        else Result := False ;
        end ;
    end ;


function TLightSource.ShutterBlankingTime : Single ;
// ----------------------------------------------------------------
// Returns time to hold shutter closed at being of a frame
// ----------------------------------------------------------------
begin
    case FDeviceType of
        lsOptoScan1200,lsOptoScan1800,lsOptoScan2000 : Result := 0.0 ;
        lsOptoscanWithLasers : Result := 0.0 ;
        lsTill : Result := 0.0 ;
        lsTillWithLasers : Result := 0.0 ;
        lsLambda10 : Result := 0.0 ;
        lsLED : Result := 0.0 ;
        lsSutterDG4 : Result := 0.0 ;
        lsCairnTIRF : Result := 0.0 ;
        else begin
             Result := 0.0 ;
             end ;
        end ;
    end ;


procedure TLightSource.WavelengthToVoltage(
          FilterNum : Integer ;
          Wavelength : Single ;
          BandWidth : Single
          ) ;
// --------------------------------------------------------
// Set control voltages for selected wavelength & bandwidth
// --------------------------------------------------------
begin

    case FDeviceType of
        lsOptoScan1200,lsOptoScan1800,lsOptoScan2000 : begin
            OptoScanWavelengthToVoltage( Wavelength,
                                         BandWidth ) ;
            end ;

        lsTill : begin
            TillWavelengthToVoltage( Wavelength ) ;
            end ;

        lsTillWithLasers : begin
            TillWithLasersWavelengthToVoltage( Wavelength ) ;
            end ;

        lsLambda10 : Begin
            Lambda10FilterNumToVoltage( FilterNum ) ;
            end ;
        lsLED : Begin
            LEDFilterNumToVoltage( FilterNum ) ;
            end ;
        lsOptoScanWithLasers : begin
            OptoScanWithLasersWavelengthToVoltage( Wavelength, BandWidth ) ;
            end ;

        lsSutterDG4 : Begin
            SutterDG4FilterNumToVoltage( FilterNum ) ;
            end ;

        lsCairnTIRF : Begin
            CairnTIRFPositionToVoltage(  Wavelength ) ;
            end ;

        end ;

    end ;


procedure TLightSource.ShutterClosedVoltages ;
// -------------------------------------------------
// Return Dig/DAC voltages which close all light sources
// -------------------------------------------------
begin

    case FDeviceType of
        lsOptoScan1200,lsOptoScan1800,lsOptoScan2000 : begin
            OptoScanWavelengthToVoltage( LightSource.ShutterClosedWavelength,
                                         0.0 ) ;
            end ;

        lsTill : begin
            TillWavelengthToVoltage( LightSource.ShutterClosedWavelength ) ;
            end ;

        lsTillWithLasers : begin
            TillWithLasersWavelengthToVoltage( LightSource.ShutterClosedWavelength ) ;
            end ;

        lsLambda10 : Begin
            Lambda10FilterNumToVoltage( 0 ) ;
            end ;

        lsLED : Begin
            LEDFilterNumToVoltage( -1 {-1 forces off setting} ) ;
            end ;

        lsOptoScanWithLasers : begin
            OptoScanWithLasersWavelengthToVoltage( LightSource.ShutterClosedWavelength, 0.0 ) ;
            end ;

        lsSutterDG4 : Begin
            SutterDG4FilterNumToVoltage( -1 ) ;{-1 forces off setting}
            end ;

        lsCairnTIRF : Begin
            CairnTIRFPositionToVoltage(  0  ) ;       // Both TIRF galvos in off position

            end ;

        end ;

    end ;


procedure TLightSource.OptoScanWavelengthToVoltage(
          Wavelength : Single ;                      // Selected wavelength
          BandWidth : Single                        // Selected bandwidth
          ) ;
// -----------------------------------------------------------------
// Get OptoScan control voltages for selected wavelength & bandwidth
// -----------------------------------------------------------------
const
    InputSlitDAC = 0 ;      // Optoscan
    GratingDAC = 1 ;        // Optoscan
    OutputSlitDAC = 2 ;      // Optoscan

var
     DValue,DWavelength : Double ;
     NumLines : DWord ;
     VOffset : Double ;
     iResource : Integer ;
begin

     // Set lines/mm of grating
     case FDeviceType of
          lsOptoScan1200 : begin
              NumLines := 1200 ;
              VOffset := 0.0 ;
              end ;
          lsOptoScan1800 : begin
              NumLines := 1800 ;
              VOffset := -1.25 ;
              end ;
          else begin // lsOptoScan2000
              NumLines := 2000 ;
              VOffset := -1.8 ;
              end ;
          end ;
     os_Set_Grating_Lines( NumLines ) ;

     // Set input slit bandwidth
     iResource := MainFrm.IOConfig.LSControlLine[InputSlitDAC] ;
     if MainFRm.IOResourceAvailable(iResource) then begin
        DValue := Bandwidth ;
        DWavelength := Wavelength ;
        os_In_Slit_Bandwidth_To_Width( DWavelength, @DValue ) ;
        os_In_Slit_Width_To_Voltage( @DValue ) ;
        LabIO.Resource[iResource].V := DValue ;
        LabIO.Resource[iResource].Delay := 0.0 ;
        end ;


     // Set centre wavelength
     iResource := MainFrm.IOConfig.LSControlLine[GratingDAC] ;
     if MainFRm.IOResourceAvailable(iResource) then begin
        DValue := Wavelength ;
        os_Wavelength_To_Voltage( @DValue ) ;
        DValue := DValue + VOffset ;
        LabIO.Resource[iResource].V := DValue ;
        LabIO.Resource[iResource].Delay := 0.0 ;
        end ;

     // Set output slit bandwidth
     iResource := MainFrm.IOConfig.LSControlLine[OutputSlitDAC] ;
     if MainFRm.IOResourceAvailable(iResource) then begin
        DValue := Bandwidth ;
        DWavelength := Wavelength ;
        os_Out_Slit_Bandwidth_To_Width( DWavelength, @DValue ) ;
        os_Out_Slit_Width_To_Voltage( @DValue ) ;
        LabIO.Resource[iResource].V := DValue ;
        LabIO.Resource[iResource].Delay := 0.0 ;
        end ;

     end;


procedure TLightSource.TillWavelengthToVoltage(
          Wavelength : Single ) ;
// ---------------------------------------------------
// Get Till control voltages for selected wavelength
// ---------------------------------------------------
const
    WavelengthMin = 0.0 ;
    WavelengthMax = 10000.0 ;
    GratingDAC = 0 ;
var
     VScale,VMono : Single ;
     iResource : Integer ;
begin

     // Set wavelength
     if Wavelength < WavelengthMin then Wavelength := WavelengthMin ;
     if Wavelength > WavelengthMax then Wavelength := WavelengthMax ;
     if Abs(Wavelength2 - Wavelength1) > 0.1 then begin
        VScale := (Voltage2 - Voltage1) / (Wavelength2 - Wavelength1) ;
        VMono := (Wavelength - Wavelength1)*VScale + Voltage1 ;
        end
     else VMono := 0.0 ;

     // Set wavelength
     iResource := MainFrm.IOConfig.LSControlLine[GratingDAC] ;
     if MainFRm.IOResourceAvailable(iResource) then begin
        LabIO.Resource[iResource].V := VMono ;
        LabIO.Resource[iResource].Delay := 0.0 ;
        end ;

     end ;


procedure TLightSource.TillWithLasersWavelengthToVoltage(
          Wavelength : Single
          ) ;
// ------------------------------------------------------------------------------
// Return PTI/Till monochromator & laser control voltages for selected wavelength
// ------------------------------------------------------------------------------
const
    GratingDAC = 0 ;        // Till
    Laser1DAC = 1 ;          // Laser #1 Intensity control
    Laser2DAC = 2 ;          // Laser #2 Intensity control
    Laser3DAC = 3 ;          // Laser #3 Intensity control

    // Values added to wavelength to select laser
    Laser1Value = 1000.0 ;
    Laser2Value = 10000.0 ;
    Laser3Value = 100000.0 ;
    TillWavelengthMin = 100.0 ;

var
//     LaserDAC : Integer ;
     i,iResource : Integer ;
     MonoWavelength,VScale,VMono : Double ;
     LaserOn : Array[0..lsMaxLightSources-1] of Boolean ;
begin

     // Determine which lasers are on
     // Wavelength = Till + Laser 1 + Laser 2 + Laser 3
     // Laser 1 = 1000
     // Laser 2 = 10000
     // Laser 3 = 100000

     MonoWavelength := Wavelength ;
     for i := 0 to High(LaserOn) do LaserOn[i] := False ;
     if MonoWavelength >= Laser3Value then begin
        LaserOn[2] := True ;
        MonoWavelength := MonoWavelength - Laser3Value ;
        end ;
     if MonoWavelength >= Laser2Value then begin
        LaserOn[1] := True ;
        MonoWavelength := MonoWavelength - Laser2Value ;
        end ;
     if MonoWavelength >= Laser1Value then begin
        LaserOn[0] := True ;
        MonoWavelength := MonoWavelength - Laser1Value ;
        end ;

     // If monochromator wavelength < minimum, set to shutter closed wavelength
     if MonoWavelength <= WavelengthMin then begin
        MonoWavelength := LightSource.ShutterClosedWavelength ;
        end ;

     // Set monochromator centre wavelength
     MonoWavelength := Max(Min(MonoWavelength,WavelengthMax),WavelengthMin) ;
     if Abs(Wavelength2 - Wavelength1) > 0.1 then begin
          VScale := (Voltage2 - Voltage1) / (Wavelength2 - Wavelength1) ;
          VMono := VScale*(MonoWavelength - Wavelength1) +  + Voltage1 ;
          end
     else VMono := 0.0 ;

     // Set wavelength
     iResource := MainFrm.IOConfig.LSControlLine[GratingDAC] ;
     if MainFRm.IOResourceAvailable(iResource) then begin
        LabIO.Resource[iResource].V := VMono ;
        LabIO.Resource[iResource].Delay := 0.0 ;
        end ;

    // Set laser control voltages

    for i := 0 to High(LaserOn) do begin
        iResource := MainFrm.IOConfig.LSControlLine[Laser1DAC+i] ;
        if MainFRm.IOResourceAvailable(iResource) then begin
           // Laser delay
           LabIO.Resource[iResource].Delay := LaserDelay[i] ;
           // Set laser voltage
           if LaserOn[i] then LabIO.Resource[iResource].V := LaserVoltage( i, LaserIntensity[Laser1DAC+i] )
                         else LabIO.Resource[iResource].V := LaserVoltage( i, 0.0 ) ;

           end ;
        end ;

     end;


procedure TLightSource.Lambda10FilterNumToVoltage(
          FilterNum : Integer
          ) ;
// ---------------------------------------
// Get Lambd10 filter # selection voltages
// ---------------------------------------
var
     iBit : Integer ;
     i,iResource : Integer ;
     V : single ;
begin

     // Set value
     iBit := 1 ;
     for i := 0 to NumControlLines-1 do begin
         iResource := MainFrm.IOConfig.LSControlLine[i] ;
         if MainFRm.IOResourceAvailable(iResource) then begin
            if (FilterNum and iBit) <> 0 then V := 4.9
                                         else V := 0.0 ;
            LabIO.Resource[iResource].V := V ;
            LabIO.Resource[iResource].Delay := 0.0 ;
            end;
         iBit := iBit*2 ;
         end ;

     end;


procedure TLightSource.LEDFilterNumToVoltage(
          FilterNum : Integer
          ) ;
// ---------------------------------------
// Get LED filter # selection voltages
// ---------------------------------------
const
    MaxLEDs = 8 ;
var
    i,iResource : Integer ;
begin

     for i := 0 to NumControlLines-1 do begin
         iResource := MainFrm.IOConfig.LSControlLine[i] ;
         if MainFRm.IOResourceAvailable(iResource) then begin
            LabIO.Resource[iResource].V := LEDOffVoltage ;
            LabIO.Resource[iResource].Delay := 0.0 ;
            if i = FilterNum then begin
               LabIO.Resource[iResource].V := (LEDMaxVoltage - LEDOffVoltage)*LaserIntensity[FilterNum]*0.01;
               end;
            end;
         end ;

     end;


procedure TLightSource.OptoScanWithLasersWavelengthToVoltage(
          Wavelength : Single ;
          BandWidth : Single
          ) ;
// -------------------------------------------------------------------------
// Return OptoScan & laser control voltages for selected wavelength & bandwidth
// -------------------------------------------------------------------------
const
    InputSlitDAC = 0 ;      // Optoscan
    GratingDAC = 1 ;        // Optoscan
    OutputSlitDAC = 2 ;      // Optoscan
    Laser1DAC = 3 ;          // Laser #1 Intensity control
    Laser2DAC = 4 ;          // Laser #2 Intensity control
    Laser3DAC = 5 ;          // Laser #3 Intensity control
    Laser1Shutter = 6 ;      // Laser #1 shutter control

    // Values added to wavelength to select laser
    Laser1Value = 1000.0 ;
    Laser2Value = 10000.0 ;
    Laser3Value = 100000.0 ;
    OptoScanWavelengthMin = 100.0 ;

var
     DValue,DWavelength : Double ;
     NumLines : DWord ;
     i : Integer ;
     OptoScanWavelength : Single ;
     LaserOn : Array[0..lsMaxLightSources-1] of Boolean ;
     iResource : Integer ;
begin

     // Set lines/mm of grating
     NumLines := 1200 ;
     os_Set_Grating_Lines( NumLines ) ;

     // Determine which lasers are on
     // Wavelength = Optoscan + Laser 1 + Laser 2 + Laser 3
     // Laser 1 = 1000
     // Laser 2 = 10000
     // Laser 3 = 100000

     OptoScanWavelength := Wavelength ;
     for i := 0 to High(LaserOn) do LaserOn[i] := False ;
     if OptoScanWavelength >= Laser3Value then begin
        LaserOn[2] := True ;
        OptoScanWavelength := OptoScanWavelength - Laser3Value ;
        end ;
     if OptoScanWavelength >= Laser2Value then begin
        LaserOn[1] := True ;
        OptoScanWavelength := OptoScanWavelength - Laser2Value ;
        end ;
     if OptoScanWavelength >= Laser1Value then begin
        LaserOn[0] := True ;
        OptoScanWavelength := OptoScanWavelength - Laser1Value ;
        end ;

     // If any lasers are on, set monochromator to off wavelength
     // and keep bandwidth same as before
     if LaserOn[0] or LaserOn[1] or LaserOn[2] then begin
        OptoScanWavelength := LightSource.ShutterClosedWavelength ;
        Bandwidth := LastOptoscanBandwidth ;
        end ;

     // Set input slit bandwidth
     iResource := MainFrm.IOConfig.LSControlLine[InputSlitDAC] ;
     if MainFRm.IOResourceAvailable(iResource) then begin
        LastOptoscanBandwidth := Bandwidth ;
        DValue := Bandwidth ;
        DWavelength := Wavelength ;
        os_In_Slit_Bandwidth_To_Width( DWavelength,@DValue ) ;
        os_In_Slit_Width_To_Voltage( @DValue ) ;
        LabIO.Resource[iResource].V := DValue ;
        LabIO.Resource[iResource].Delay := 0.0 ;
        end ;

     // Set centre wavelength
     iResource := MainFrm.IOConfig.LSControlLine[GratingDAC] ;
     if MainFRm.IOResourceAvailable(iResource) then begin
        DValue := OptoScanWavelength ;
        os_Wavelength_To_Voltage( @DValue ) ;
        LabIO.Resource[iResource].V := DValue ;
        LabIO.Resource[iResource].Delay := 0.0 ;
        end ;

     // Set output slit bandwidth
     iResource := MainFrm.IOConfig.LSControlLine[OutputSlitDAC] ;
     if MainFRm.IOResourceAvailable(iResource) then begin
        LastOptoscanBandwidth := Bandwidth ;
        DValue := Bandwidth ;
        DWavelength := Wavelength ;
        os_Out_Slit_Bandwidth_To_Width( DWavelength,@DValue ) ;
        os_Out_Slit_Width_To_Voltage( @DValue ) ;
        LabIO.Resource[iResource].V := DValue ;
        LabIO.Resource[iResource].Delay := 0.0 ;
        end ;

    // Set laser control voltages

    for i := 0 to High(LaserOn) do begin
        iResource := MainFrm.IOConfig.LSControlLine[Laser1DAC+i] ;
        if MainFRm.IOResourceAvailable(iResource) then begin
           // Laser delay
           LabIO.Resource[iResource].Delay := LaserDelay[i] ;
           // Set laser voltage
           if LaserOn[i] then LabIO.Resource[iResource].V := LaserVoltage( i, LaserIntensity[Laser1DAC+i] )
                         else LabIO.Resource[iResource].V := LaserVoltage( i, 0.0 ) ;
            end ;
        end ;

    // Open shutter for laser 1 if it has a non-zero intensity
    iResource := MainFrm.IOConfig.LSControlLine[Laser1Shutter] ;
    if MainFRm.IOResourceAvailable(iResource) then begin
       if LaserOn[0] then DValue := 5.0 else DValue := 0.0 ;
        LabIO.Resource[iResource].V := DValue ;
        LabIO.Resource[iResource].Delay := 0.0 ;
       end ;

     end;


function TLightSource.LaserVoltage(
         LaserNum : Integer ;         // Laser # 1 or 2
         PercentageIntensity : Single // % of maximum intensity
         ) : Single ;
// -------------------------------------------------------------
// Return laser intensity control voltage for select laser # and
// desired % intensity
// -------------------------------------------------------------
begin
     Result := LaserOffVoltage[LaserNum] +
               Min(Max(Abs(PercentageIntensity*0.01),0.0),1.0)*
               (LaserOnVoltage[LaserNum] - LaserOffVoltage[LaserNum]) ;
     end ;


procedure TLightSource.SutterDG4FilterNumToVoltage(
          FilterNum : Integer
          ) ;
// ---------------------------------------
// Get Sutter DG4 filter # selection voltages
// ---------------------------------------
// -1 = closed
// 0-3 = Filters 0-3
var
    iResource,iBit,i : Integer ;
    BitPattern : Word ;
    V : Single ;
begin

     if FilterNum >= 0 then BitPattern := (FilterNum mod 4) or 4
                       else BitPattern := 0 ;

     // Set value
     iBit := 1 ;
     for i := 0 to NumControlLines-1 do begin
         iResource := MainFrm.IOConfig.LSControlLine[i] ;
         if MainFRm.IOResourceAvailable(iResource) then begin
            if (BitPattern and iBit) <> 0 then V := 4.9 else V := 0.0 ;
            LabIO.Resource[iResource].V := V ;
            LabIO.Resource[iResource].Delay := 0.0 ;
            end;
         iBit := iBit*2 ;
         end ;

     end;


procedure TLightSource.CairnTIRFPositionToVoltage(
           GalvoPosition : Single  ) ;            // No. control channels
// ---------------------------------------------------
// Get Cairn TIRF galvo voltages for selected position
// ---------------------------------------------------
// 0 = All TIRF lasers off
// 1 = Laser #1 TIRF
// 2 = Laser #1 wide field
// 10 = Laser #2 TIRF
// 20 = Laser #2 wide field
// 11 = Laser #1 & #2 TIRF
// 12 = Laser #1 TIRF Laser #2 wide field
// 21 = Laser #2 TIRF Laser #1 wide field
// 22 = Laser #2 wide field Laser #1 wide field

Const
    Galvo1 = 0 ;
    Galvo2 = 1 ;
var
    iResource : Integer ;
    V : Array[0..lsMaxTIRFGalvos-1] of Single ;

begin

    // TIRF galvo #1
    case Round(GalvoPosition) mod 10 of
        1 : V[0] := TIRFOn[1] ;
        2 : V[0] := TIRFWF[1] ;
        else V[0] := TIRFOff[1] ;
        end ;

    // TIRF galvo #2
    case Round(GalvoPosition) div 10 of
        1 : V[1] := TIRFOn[2] ;
        2 : V[1] := TIRFWF[2] ;
        else V[1] := TIRFOff[2] ;
        end ;

     // Galvo #1
     iResource := MainFrm.IOConfig.LSControlLine[Galvo1] ;
     if MainFRm.IOResourceAvailable(iResource) then begin
        LabIO.Resource[iResource].V := V[0] ;
        LabIO.Resource[iResource].Delay := 0.0 ;
        end ;

     // Galvo #2
     iResource := MainFrm.IOConfig.LSControlLine[Galvo2] ;
     if MainFRm.IOResourceAvailable(iResource) then begin
        LabIO.Resource[iResource].V := V[1] ;
        LabIO.Resource[iResource].Delay := 0.0 ;
        end ;

     end ;


function TLightSource.VariableIntensitySource( iLine : Integer ) : Boolean ;
// ----------------------------------------------
// Return TRUE if light intensity can be adjusted
// ----------------------------------------------
var
    iResource : Integer ;
begin
     Result := False ;
     if (iLine < 0) or (iLine >= High(MainFrm.IOConfig.LSControlLine)) then Exit ;

     iResource := MainFrm.IOConfig.LSControlLine[iLine] ;
     if MainFRm.IOResourceAvailable(iResource) then begin
        if ANSIContainsText( ControlLineName(iLine), 'LED') and
           (LabIO.Resource[iResource].ResourceType = DACout) then Result := True ;
        end;
     end;


procedure TLightSource.SetDeviceType( Value : Integer ) ;
// ----------------------------
// Set light source device type
// ----------------------------
var
    FileName : ANSIstring ;
begin
    FDeviceType := Value ;
    case FDeviceType of
        lsOptoScan1200,lsOptoScan1800,lsOptoScan2000,lsOptoscanWithLasers : begin
          // Load default settings
          FileName := MainFrm.ProgramDirectory + 'oslibrary.ini' ;
          os_Load_Interface_Defaults( PANSIChar(FileName));
          end;
        end ;

   end;


procedure TLightSource.DataModuleCreate(Sender: TObject);
// --------------------------------------
// Initialisations when module is created
// --------------------------------------
var
    i : Integer ;
begin
     FDeviceType := lsNone ;
     for i := 0 to High(VControlInUse) do VControlInUse[i].V := 0.0 ;
     LastOptoscanBandwidth := 0.0 ;

     for i := 0 to High(LaserWavelength) do begin
        LaserWavelength[i] := 1000.0 ;
        LaserDelay[i] := 0.0 ;
        LaserOffVoltage[i] := 0.0 ;
        LaserOnVoltage[i] := 5.0 ;
        LaserIntensity[i] := 50.0 ;
        end;

     // Default laser settings
     LaserWavelength[0] := 488.0 ;
     LaserDelay[0] := 4E-4 ;
     LaserOffVoltage[0] := 0.0 ;
     LaserOnVoltage[0] := 10.0 ;
     LaserIntensity[0] := 50.0 ;

     LaserWavelength[1] := 561.0 ;
     LaserDelay[1] := 4E-4 ;
     LaserOffVoltage[1] := 0.0 ;
     LaserOnVoltage[1] := 10.0 ;
     LaserIntensity[1] := 50.0 ;

     LaserWavelength[2] := 561.0 ;
     LaserDelay[2] := 4E-4 ;
     LaserOffVoltage[2] := 0.0 ;
     LaserOnVoltage[2] := 10.0 ;
     LaserIntensity[2] := 50.0 ;

     LEDOffVoltage := 0.0 ;
     LEDMaxVoltage := 5.0 ;

     EMFilterChangeTime := 0.1 ;

     end;

end.
