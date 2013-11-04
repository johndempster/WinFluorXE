{$IFNDEF OSLIBRARY}

//Debug stuff
// os_Set_Grating_Lines (J.D. 14/9/5

procedure os_Set_Log(Strings: TStrings);stdcall; external 'osLibrary.dll';

//General osLibrary functions
function os_Library_Version: Double;stdcall; external 'osLibrary.dll';
function os_Library_Version_Str: PChar;stdcall; external 'osLibrary.dll';
function os_Wide_Error_String(ECode: DWord): lpWStr;stdcall; external 'osLibrary.dll';
function os_Char_Error_String(ECode: DWord): lpStr;stdcall; external 'osLibrary.dll';
function os_Wavelength_To_RGB(const Wavelength: Double; var R,G,B: Byte): DWord;stdcall; external 'osLibrary.dll';
function os_Wavelength_To_Voltage(Wavelength: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_In_Slit_Bandwidth_To_Width(Slit: PDouble): DWord;stdcall;  external 'osLibrary.dll';
function os_Out_Slit_Bandwidth_To_Width(Slit: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_In_Slit_Width_To_Voltage(Slit: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_Out_Slit_Width_To_Voltage(Slit: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_In_Slit_Width_To_Bandwidth(Slit: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_Out_Slit_Width_To_Bandwidth(Slit: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_Query_IO(Reserved: DWord; Data: lpStr; Size: DWord): DWord;stdcall; external 'osLibrary.dll';
function os_Open_IO(Reserved: DWord; Data: lpStr): DWord;stdcall; external 'osLibrary.dll';
function os_Switch_Control(Value: Bool): DWord;stdcall; external 'osLibrary.dll';
function os_Close_IO: DWord;stdcall; external 'osLibrary.dll';
function os_Valid_Params(Wavelength,InSlit,OutSlit: Double): DWord;stdcall; external 'osLibrary.dll';
function os_Set_Params(Wavelength,InSlit,OutSlit: Double): DWord;stdcall; external 'osLibrary.dll';
function os_Get_Ready(Wavelength,InSlit,OutSlit: PBool): DWord;stdcall; external 'osLibrary.dll';
function os_Shut_In_Slit(Value: Bool): DWord;stdcall; external 'osLibrary.dll';
function os_Shut_Grating(Value: Bool): DWord;stdcall; external 'osLibrary.dll';
function os_Shut_Out_Slit(Value: Bool): DWord;stdcall; external 'osLibrary.dll';
function os_Shut_Both_Slits(Value: Bool): DWord;stdcall; external 'osLibrary.dll';
function os_Num_Driver_Errors: DWord;stdcall; external 'osLibrary.dll';
function os_Last_Driver_Error: lpStr;stdcall; external 'osLibrary.dll';
function os_Slits_Driven(InSlit,OutSlit: PBool): DWord;stdcall;  external 'osLibrary.dll';
function os_Max_Wavelength(Wavelength: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_Min_Wavelength(Wavelength: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_Max_InSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_Min_InSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_Max_OutSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_Min_OutSlit(Wavelength: PDouble): DWord;stdcall; external 'osLibrary.dll';
function os_Grating_Lines(Lines: PDWord): DWord;stdcall; external 'osLibrary.dll';
function os_Set_Grating_Lines(Lines: DWord): DWord;stdcall; external 'osLibrary.dll';
function os_Load_Interface_Defaults(FileName: PChar): DWord;stdcall; external 'osLibrary.dll';
//function os_Load_DAC_Defaults(FileName: PChar): DWord;stdcall; external 'osLibrary.dll';
function os_Get_Handle(Handle: PDWord): DWord;stdcall; external 'osLibrary.dll';
function os_Set_Handle(Handle: DWord): DWord;stdcall; external 'osLibrary.dll';

{$ENDIF}
