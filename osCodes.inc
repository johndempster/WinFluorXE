type
  TSelect_Handle= function(Handle: LongWord): DWord;
  TQuery= function: DWord;
  TInit= function(Handle: DWord): DWord;
  TClose= function: DWord;
  TSet_Wavelength= function(Value: Double): DWord;
  TSet_InSlit= function(Value: Double): DWord;
  TSet_OutSlit= function(Value: Double): DWord;
  TGet_Ready= function(Wavelength,InSlit,OutSlit: PBool): DWord;
  TShut_In_Slit= function(Value: Bool): DWord;
  TShut_Grating= function(Value: Bool): DWord;
  TShut_Out_Slit= function(Value: Bool): DWord;
  TShut_Both_Slits= function(Value: Bool): DWord;
  TSwitch_Control= function(Value: Bool): DWord;
  TSlits_Driven= function (InSlit,OutSlit: PBool): DWord;
  TNum_Driver_Errors= function : DWord;
  TLast_Driver_Error= function : PChar;
  TSet_Params = function(InSlit,Wave,OutSlit: Double): DWord;


  PInterface=                           ^TInterface;
  TInterface=                           record
    Index:                              LongWord;
    Lib:                                THandle;
    Name:                               String;
    Open:                               Bool;
    MIN_WAVELENGTH:                     Double;//=300;  //Range settings
    MAX_WAVELENGTH:                     Double;//=800;
    MIN_IN_SLIT_WIDTH:                  Double;//=0;
    MAX_IN_SLIT_WIDTH:                  Double;//=30;
    MIN_OUT_SLIT_WIDTH:                 Double;//=0;
    MAX_OUT_SLIT_WIDTH:                 Double;//=30;
    InSlitDriven:                       Bool;//=True;
    OutSlitDriven:                      Bool;//=True;
    Grate_Offset:                       Double;//=0;    //Voltage offset for different gratings
    GratingLines:                       DWord;//=1200;
    GRATE_LINES:                        Double;
    TWO_COS_D:                          Double;
    MIRROR_DISTANCE:                    Double;
    D:                                  Double;
    a:                                  Double;
    b:                                  Double;
    IInit:                              TInit;
    IClose:                             TClose;
    ISwitch_Control:                    TSwitch_Control;
    ISet_Wavelength:                    TSet_Wavelength;
    ISet_InSlit:                        TSet_InSlit;
    ISet_OutSlit:                       TSet_OutSlit;
    IGet_Ready:                         TGet_Ready;
    IShut_In_Slit:                      TShut_In_Slit;
    IShut_Grating:                      TShut_Grating;
    IShut_Out_Slit:                     TShut_Out_Slit;
    IShut_Both_Slits:                   TShut_Both_Slits;
    INum_Driver_Errors:                 TNum_Driver_Errors;
    ILast_Driver_Error:                 TLast_Driver_Error;
    ISelect_Handle:                     TSelect_Handle;
    ISlits_Driven:                      TSlits_Driven;
    ISet_Params:                        TSet_Params;
    Tag:                                Integer;
  end;

const
//Error Codes
  os_OK                               =$00000000;
  os_UNKNOWN_ERROR                    =$FFFFFFFF;

//Controller box error codes
  os_PORT_ALREADY_OPEN	              =$00000100;
  os_PORT_ALREADY_CLOSED	      =$00000101;
  os_PORT_NOT_INITIALISED             =$00000102;
  os_PORT_NOT_OPEN                    =$00000103;
  os_RESPONSE_TIMEOUT                 =$00000104;
  os_UNABLE_TO_OPEN                   =$00000105;
  os_PORT_UNMODIFIABLE                =$00000106;
  os_UNABLE_TO_LOAD_DRIVER            =$00000107;
  os_COMM_CONTROL_NOT_ON              =$00000108;
  os_UNEXPECTED_RESONSE               =$00000109;
  os_FORTH_ERROR                      =$0000010A;

  os_SET_POS_ERROR                    =$00000201;
  os_SET_POS_ON_ERROR                 =$00000202;
  os_SET_POS_PARAMS_ERROR             =$00000203;
  os_WRITE_STRING_ERROR               =$00000204;
  os_SEND_COMMAND_ERROR               =$00000205;
  os_NO_DEFAULTS_ERROR                =$00000206;
  os_GO_EXTERNAL_ERROR                =$00000207;
  os_STOP_EXTERNAL_ERROR              =$00000208;
  os_SET_PROG_ERROR                   =$00000209;
  os_SET_PROG_EXTERN_ERROR            =$0000020A;
  os_NIL_POINTER_SENT                 =$0000020B;

  os_INVALID_POS_PARAMETER            =$00000301;
  os_INVALID_PROG_PARAMETER           =$00000302;
  os_INVALID_WAVELENGTH               =$00000303;
  os_INVALID_INPUT_SLIT               =$00000304;
  os_INVALID_OUTPUT_SLIT              =$00000305;
  os_INVALID_TIME                     =$00000306;
  os_INVALID_BAUD                     =$00000307;
  os_INVALID_PORT                     =$00000308;
  os_INVALID_STEP                     =$00000309;
//  os_SET_SCAN_ERROR                   =$00000401;

//  os_INVALID_SCAN_PROG_PARAMETER      =$00000501;
//  os_INVALID_SCAN_TIME                =$00000502;

  os_GET_POS_ERROR                    =$00000601;
  os_GET_WAVELENGTH_ERROR             =$00000602;
  os_GET_INSLIT_ERROR                 =$00000603;
  os_GET_EXSLIT_ERROR                 =$00000603;
  os_GET_TIME_ERROR                   =$00000604;
  os_GET_ENABLED_ERROR                =$00000605;

// DAC error codes
  os_UNSUPPORTED_IO_ID                =$00001100;
  os_INVALID_CARD_NAME                =$00001101;
  os_IO_NOT_INITIATED                 =$00001102;
  os_IO_CARD_SUPPORT_LIB_NOT_FOUND    =$00001103;
  os_INVALID_BUFFER                   =$00001104;
  os_QUERY_IO_ERROR                   =$00001105;
  os_UNABLE_TO_INIT                   =$00001106;
  os_FILE_NOT_FOUND                   =$00001107;
  os_DEFAULT_LOAD_FAIL                =$00001108;
  OS_INTERFACE_IN_USE                 =$00001109;

  os_NO_DA_FOUND                      =$00001200;
  os_DA_DATA_FLOW_ERROR               =$00001201;
  os_DA_RANGE_ERROR                   =$00001202;
  os_DA_CONFIG_ERROR                  =$00001203;
  os_DA_OUTPUT_ERROR                  =$00001204;

  os_NO_DIN_FOUND                     =$00001300;
  os_DIN_RESOLUTION_ERROR             =$00001301;
  os_DIN_DATA_FLOW_ERROR              =$00001302;
  os_DIN_NOTE_PROC_ERROR              =$00001303;
  os_DIN_CONFIG_ERROR                 =$00001304;
  os_DIN_START_ERROR                  =$00001305;
  os_DIN_INPUT_ERROR                  =$00001306;

  os_NO_DOUT_FOUND                    =$00001400;
  os_DOUT_RESOLUTION_ERROR            =$00001401;
  os_DOUT_DATA_FLOW_ERROR             =$00001402;
  os_DOUT_CONFIG_ERROR                =$00001403;
  os_DOUT_OUTPUT_ERROR                =$00001404;

  os_INVALID_DAC_WAVELENGTH           =$00001500;
  os_WAVELENGTH_TO_VOLT_ERROR         =$00001501;
  os_INVALID_SLIT_WIDTH               =$00001502;
  os_SLIT_BW_TO_WIDTH_ERROR           =$00001503;
  os_SLIT_WIDTH_TO_VOLT_ERROR         =$00001504;
  os_INVALID_GRATING_LINES            =$00001505;
  os_SLIT_WIDTH_TO_BW_ERROR           =$00001506;
  os_INVALID_HANDLE                   =$00001507;
