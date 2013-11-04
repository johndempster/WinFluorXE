unit WinFluor_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 09/07/2013 13:02:05 from Type Library described below.

// ************************************************************************  //
// Type Lib: F:\Delphi Projects\WinFluor\WinFluor.tlb (1)
// LIBID: {38EFB183-C837-4C53-BF40-5D6E5856820F}
// LCID: 0
// Helpfile: 
// HelpString: WinFluor Library
// DepndLst: 
//   (1) v2.0 stdole, (E:\WINDOWS\system32\STDOLE2.TLB)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  WinFluorMajorVersion = 1;
  WinFluorMinorVersion = 0;

  LIBID_WinFluor: TGUID = '{38EFB183-C837-4C53-BF40-5D6E5856820F}';

  IID_IAUTO: TGUID = '{E7B30E35-6C29-44D7-AFFB-BB93CA888A37}';
  CLASS_AUTO: TGUID = '{E161144A-A167-415E-852B-526C591FDD75}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IAUTO = interface;
  IAUTODisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  AUTO = IAUTO;


// *********************************************************************//
// Interface: IAUTO
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E7B30E35-6C29-44D7-AFFB-BB93CA888A37}
// *********************************************************************//
  IAUTO = interface(IDispatch)
    ['{E7B30E35-6C29-44D7-AFFB-BB93CA888A37}']
    procedure NewFile(FileName: OleVariant); safecall;
    procedure OpenFile(FileName: OleVariant); safecall;
    procedure CloseFile; safecall;
    procedure RecordCamera; safecall;
    procedure StopCamera; safecall;
    procedure RecordLSM(Duration: OleVariant; Num: OleVariant; Delay: OleVariant; 
                        LineScan: OleVariant); safecall;
    procedure StopLSM; safecall;
    procedure ImportImageFile(FileName: OleVariant); safecall;
    function Get_ExperimentID: OleVariant; safecall;
    procedure Set_ExperimentID(Value: OleVariant); safecall;
    function Get_FileName: OleVariant; safecall;
    procedure Set_FileName(Value: OleVariant); safecall;
    property ExperimentID: OleVariant read Get_ExperimentID write Set_ExperimentID;
    property FileName: OleVariant read Get_FileName write Set_FileName;
  end;

// *********************************************************************//
// DispIntf:  IAUTODisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E7B30E35-6C29-44D7-AFFB-BB93CA888A37}
// *********************************************************************//
  IAUTODisp = dispinterface
    ['{E7B30E35-6C29-44D7-AFFB-BB93CA888A37}']
    procedure NewFile(FileName: OleVariant); dispid 201;
    procedure OpenFile(FileName: OleVariant); dispid 202;
    procedure CloseFile; dispid 203;
    procedure RecordCamera; dispid 204;
    procedure StopCamera; dispid 205;
    procedure RecordLSM(Duration: OleVariant; Num: OleVariant; Delay: OleVariant; 
                        LineScan: OleVariant); dispid 206;
    procedure StopLSM; dispid 207;
    procedure ImportImageFile(FileName: OleVariant); dispid 208;
    property ExperimentID: OleVariant dispid 209;
    property FileName: OleVariant dispid 210;
  end;

// *********************************************************************//
// The Class CoAUTO provides a Create and CreateRemote method to          
// create instances of the default interface IAUTO exposed by              
// the CoClass AUTO. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAUTO = class
    class function Create: IAUTO;
    class function CreateRemote(const MachineName: string): IAUTO;
  end;

implementation

uses ComObj;

class function CoAUTO.Create: IAUTO;
begin
  Result := CreateComObject(CLASS_AUTO) as IAUTO;
end;

class function CoAUTO.CreateRemote(const MachineName: string): IAUTO;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AUTO) as IAUTO;
end;

end.
