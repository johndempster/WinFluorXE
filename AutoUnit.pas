unit AutoUnit;
// ----------------------------------------
// WinFluor - VBSCRIPT Automation Interface
// ----------------------------------------
// 05.07.04
// 10.06.05 Delay added to RecordLSM
// 02.08.05 Application.ProcessMessages removed (
//          (caused RECORDLSM to hang up when on 2nd monitors)
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, WinFluor_TLB, StdVcl, Forms, LogUnit ;

type
  TAUTO = class(TAutoObject, IAUTO)
  protected
    procedure NewFile(FileName: OleVariant); safecall;
    procedure CloseFile; safecall;
    procedure OpenFile(FileName: OleVariant); safecall;
    procedure RecordCamera; safecall;
    procedure RecordLSM(Duration, Num, Delay, LineScan: OleVariant); safecall;
    procedure StopCamera; safecall;
    procedure StopLSM; safecall;
    procedure ImportImageFile(FileName: OleVariant); safecall;
    function Get_ExperimentID: OleVariant; safecall;
    procedure Set_ExperimentID(Value: OleVariant); safecall;
    function Get_FileName: OleVariant; safecall;
    procedure Set_FileName(Value: OleVariant); safecall;

  end;

implementation

uses ComServ, Main,RecADCOnlyUnit ;

procedure TAUTO.NewFile(FileName: OleVariant);
// -------------------------------
// Create new .IDR image data file
// -------------------------------
begin

     // Close all windows (except record windows)
     MainFrm.CloseWindows ;

     // Close existing data files
     MainFrm.IDRFile.CloseFile ;

     FileName := MainFrm.CreateIndexedFileName( FileName ) ;
     MainFrm.IDRFile.CreateNewFile( FileName ) ;
     MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;
     // Update log
     LogFrm.AddLine( 'New file created: ' + MainFrm.IDRFile.FileName );

     MainFrm.UpdateRecentFilesList ;

     end;


procedure TAUTO.CloseFile;
// ------------------------------
// Close currently open data file
// ------------------------------
begin

     // Close all windows (except record windows)
     MainFrm.CloseWindows ;

     // Close existing data files
     MainFrm.IDRFile.CloseFile ;

     end;


procedure TAUTO.OpenFile(FileName: OleVariant);
// -------------------------------
// Open an existing .IDR data file
// -------------------------------
begin

     // Close windows
     MainFrm.CloseWindows ;

     // Close IDR data file
     MainFrm.IDRFile.CloseFile ;

     MainFrm.IDRFile.OpenFile( FileName ) ;
     // Display in program title bar
     MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;
     if MainFrm.IDRFile.NumFrames > 0 then begin
        MainFrm.UpdateRecentFilesList ;
        MainFrm.mnViewImages.Click ;
        end ;

     LogFrm.AddLine( MainFrm.IDRFile.FileName + ' opened.' );

     end;


procedure TAUTO.RecordCamera;
begin

end;

procedure TAUTO.RecordLSM(Duration,             // Recording duration
                          Num,                  // No. of frames or lines
                          Delay,                // Delay (s) before start
                          LineScan: OleVariant  // 0=frame / 1=line scan mode
                          );
var
    FormExists : Boolean ;
    FrameOrLineScanInterval : Single ;
    i : Integer ;
begin

     // Create analogue recording form if it does not already exist
     FormExists := False ;
     for i := 0 to MainFrm.MDIChildCount-1 do
         if MainFrm.MDIChildren[i].Name = 'RecADCOnlyFrm' then FormExists := True ;
     if not FormExists then begin
        MainFrm.mnRecord.Click ;
//        Application.ProcessMessages ;
        end ;

     // Start recording
     RecADCOnlyFrm.AutoRecordingMode := True ;

     FrameOrLineScanInterval := Duration / Num ;
     if LineScan = LSMXYSeriesFrameTriggerMode then begin
        // XY Series frame trigger mode
        if FrameOrLineScanInterval > RecADCOnlyFrm.Interval then
           RecADCOnlyFrm.Interval := FrameOrLineScanInterval ;
        Duration := RecADCOnlyFrm.Interval*Num ;
        end
     else begin
        RecADCOnlyFrm.Interval := FrameOrLineScanInterval ;
        end ;

     // Delay between trigger and start of imaging
     RecADCOnlyFrm.ImageStartDelay := Delay ;

     RecADCOnlyFrm.StartRecordingToDisk( Duration,
                                         Num,
                                         LineScan ) ;

     end;

procedure TAUTO.StopCamera;
begin

end;

procedure TAUTO.StopLSM;
begin

end;

procedure TAUTO.ImportImageFile(FileName: OleVariant);
// ----------------------------------------
// Import file of images into IDR data file
// ----------------------------------------
begin
     RecADCOnlyFrm.ImportImageFile( FileName ) ;
     end;


function TAUTO.Get_ExperimentID: OleVariant;
// ---------------------------------------------------
// Get experiment identification line of IDR data file
// ---------------------------------------------------
begin
    Result := MainFrm.IDRFile.Ident ;
    end;


procedure TAUTO.Set_ExperimentID(Value: OleVariant);
// ---------------------------------------------------
// Set experiment identification line of IDR data file
// ---------------------------------------------------
begin
    MainFrm.IDRFile.Ident := Value ;
    LogFrm.AddLine( MainFrm.IDRFile.Ident ) ;
    MainFrm.IdentChanged := True ;
    end;

    
function TAUTO.Get_FileName: OleVariant;
begin
     Result := MainFrm.IDRFile.FileName ;
     end;

procedure TAUTO.Set_FileName(Value: OleVariant);
begin

end;

initialization
  TAutoObjectFactory.Create(ComServer, TAUTO, Class_AUTO,
    ciMultiInstance, tmApartment);
end.
