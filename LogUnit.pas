unit LogUnit;
// --------------
// Experiment log
// --------------
// 08.07.04
// 21.06.05 ... Log file updated every time a line is added
// 25.10.13 ... Log text now written to TextFile

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, math ;

type
  TLogFrm = class(TForm)
    meLog: TMemo;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    LogFileName : String ;
    procedure AddLine( LineText : String ) ;
    procedure AddLineNoTime( LineText : String ) ;
    procedure SaveLogToFile ;
  end;

var
  LogFrm: TLogFrm;

implementation

{$R *.dfm}

uses MAIN;

procedure TLogFrm.AddLine( LineText : String );
// ---------------
// Add line to log
// ---------------
begin
    meLog.Lines.Add(TimeToStr(Time) + ' ' + LineText ) ;
    SaveLogToFile ;
    end;


procedure TLogFrm.AddLineNoTime( LineText : String );
// -------------------------------
// Add line to log (no time stamp)
// -------------------------------
begin
    meLog.Lines.Add( LineText ) ;
    SaveLogToFile ;
    end;



procedure TLogFrm.FormResize(Sender: TObject);
// ----------------------------------
// Update controls when form re-sized
// ----------------------------------
begin
     meLog.Width :=  Max( ClientWidth - meLog.Left - 10,2) ;
     meLog.Height := Max( ClientHeight - 2*meLog.Top, 2) ;
     end;


procedure TLogFrm.FormCreate(Sender: TObject);
// ------------------------------------
// Load from log file when form created
// ------------------------------------
var
    LogFile : TextFile ;
    s : string ;
    FirstLine : Boolean ;
begin

    // Re-size form to set up control sizes
    Resize ;

    {$IF CompilerVersion > 7.0} formatsettings.DateSeparator := '-';
     {$ELSE} DateSeparator := '-';
     {$IFEND}

    LogFileName := MainFrm.SettingsDirectory + 'WinFluor Log ' + DateToStr(Date) + '.log' ;
    Caption := 'Log: ' + LogFileName ;

    // Load text into memo control
    if FileExists(LogFileName) then begin
       AssignFile( LogFile, logFileName ) ;
       Reset( LogFile ) ;
       meLog.Clear ;
       FirstLine := True ;
       while not EOF(LogFile) do begin
          ReadLn( LogFile, s ) ;
          if FirstLine then begin
             meLog.Lines[0] := s ;
             FirstLine := False ;
             end
          else meLog.Lines.Add(s) ;
          end;
       CloseFile( LogFile ) ;
       end ;

    end;


procedure TLogFrm.FormDestroy(Sender: TObject);
// -----------------------------------
// Actions take when form is destroyed
// -----------------------------------
begin
    // Save log to file
    SaveLogToFile ;
    end ;


procedure TLogFrm.SaveLogToFile ;
// ---------------------
// Save data to log file
// ---------------------
var
    LogFile : TextFile ;
    i : Integer ;
begin

    // Create new log file
    if FileExists(LogFileName) then DeleteFile(PChar(LogFileName)) ;

    // Save text from memo control
    AssignFile( LogFile, logFileName ) ;
    ReWrite( LogFile ) ;
    for i := 0 to meLog.Lines.Count-1 do begin
       WriteLn( LogFile, meLog.Lines[i] ) ;
       end;
    CloseFile( LogFile ) ;

    end;


procedure TLogFrm.FormActivate(Sender: TObject);
begin
     meLog.SelStart := melog.GetTextLen-1 ;

     end;

procedure TLogFrm.FormShow(Sender: TObject);
begin
meLog.SelStart := melog.GetTextLen-1 ;
end;

end.
