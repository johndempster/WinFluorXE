unit PrintRec;
{ =================================================================
  WinEDR - Printer page/font setup form for printing signal records
  2/5/97 ... Calibration bar values only shown for enabled channels
  26/6/98 ... Text boxes updates now grouped together in UpdateSettings
  30/8/99 ... Display object (Scope or Chart) now updated by PrintRec
  03/12/05 ... Printer now displayed and can be selected from within dialog box
  =================================================================}
interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls,Printers,Shared, Grids, SysUtils, Spin,
  ValEdit,  ScopeDisplay, ChartDisplay, ValidatedEdit ;

type
  TDestination = (dePrinter,deClipboard) ;
  TPrintRecFrm = class(TForm)
    GroupBox2: TGroupBox;
    CalibrationBarTable: TStringGrid;
    GroupBox5: TGroupBox;
    ckShowZeroLevels: TCheckBox;
    ckShowLabels: TCheckBox;
    bPrint: TButton;
    bCancel: TButton;
    ckUseColor: TCheckBox;
    FontGrp: TGroupBox;
    Label7: TLabel;
    cbFontName: TComboBox;
    Page: TNotebook;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox4: TGroupBox;
    Label6: TLabel;
    Label8: TLabel;
    edWidth: TValidatedEdit;
    edHeight: TValidatedEdit;
    edLeftMargin: TValidatedEdit;
    edTopMargin: TValidatedEdit;
    edRightMargin: TValidatedEdit;
    edBottomMargin: TValidatedEdit;
    edFontSize: TValidatedEdit;
    Label5: TLabel;
    edLineThickness: TValidatedEdit;
    bDefaultSettings: TButton;
    GroupBox6: TGroupBox;
    bPrinterSetup: TButton;
    edPrinterName: TEdit;
    procedure bPrintClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CalibrationBarTableKeyPress(Sender: TObject; var Key: Char);
    procedure bDefaultSettingsClick(Sender: TObject);
    procedure bPrinterSetupClick(Sender: TObject);
  private
    { Private declarations }
    function GetCurrentPrinterName : string ;
  public
    { Public declarations }

    Destination : TDestination ;
    DisplayObj : TObject ;
  end;



var
  PrintRecFrm: TPrintRecFrm;

implementation

{$R *.DFM}
uses Main, maths ;

procedure TPrintRecFrm.FormShow(Sender: TObject);
{ --------------------------------
  Initialise Print Record(s) form
  -------------------------------}
var
   ch,Row : Integer ;
   BarSize : Single ;
begin

     ClientWidth := Page.Left + Page.Width + 5 ;
     ClientHeight := Page.Top + Page.Height + 5 ;

     { Select appropriate settings page }
     if Destination = dePrinter then begin
        Caption := ' Print ' ;
        edPrinterName.Text :=GetCurrentPrinterName ;
        bPrinterSetup.Enabled := True ;
        Page.PageIndex := 0 ;
        edFontSize.Units := 'pts' ;
        edLineThickness.Units := 'pts' ;
        end
     else begin
        Caption := ' Copy Image ' ;
        edPrinterName.Text := 'Windows Clipboard' ;
        bPrinterSetup.Enabled := False ;
        Page.PageIndex := 1 ;
        edFontSize.Units := 'pixels' ;
        edLineThickness.Units := 'pixels' ;
        end ;

     { Fill Fonts list with typefaces available to printer }
     cbFontName.items := printer.fonts ;

     { Set Column widths }
     CalibrationBarTable.colwidths[0] := Canvas.TextWidth('XXXXXXX') ;
     CalibrationBarTable.colwidths[1] := Canvas.TextWidth('XXXXXXXXXXX') ;

     CalibrationBarTable.options := [goEditing,goHorzLine,goVertLine] ;
     CalibrationBarTable.RowCount := TScopeDisplay(DisplayObj).NumChannels + 1 ;
     CalibrationBarTable.cells[0,0] := 'Time ' ;

      { Update text box settings }
      edLeftMargin.Value := MainFrm.PrinterLeftMargin ;
      edRightMargin.Value := MainFrm.PrinterRightMargin ;
      edTopMargin.Value := MainFrm.PrinterTopMargin ;
      edBottomMargin.Value := MainFrm.PrinterBottomMargin ;
      edWidth.Value := MainFrm.ClipboardBitMapWidth ;
      edHeight.Value := MainFrm.ClipboardBitMapHeight ;
      edFontSize.Value := MainFrm.PrinterFontSize ;
      edLineThickness.Value := MainFrm.PrinterLineThickness ;
      cbFontName.itemindex := cbFontName.items.indexof(MainFrm.PrinterFontName) ;
      if cbFontName.itemindex < 0 then  cbFontName.itemindex := 0 ;
      ckShowLabels.checked := MainFrm.PrinterShowLabels ;
      ckShowZeroLevels.checked := MainFrm.PrinterShowZeroLevels ;
      ckUseColor.checked := MainFrm.PrinterUseColor ;

      BarSize := (TScopeDisplay(DisplayObj).xMax - TScopeDisplay(DisplayObj).xMin)*0.1
                  * TScopeDisplay(DisplayObj).TScale ;

      CalibrationBarTable.cells[1,0] := Format( '%4g %s',
                                       [BarSize,TScopeDisplay(DisplayObj).TUnits])  ;

      { Channel calibration bars }
      Row := 1 ;
      for ch := 0 to TScopeDisplay(DisplayObj).NumChannels-1 do begin
          BarSize := (TScopeDisplay(DisplayObj).yMax[ch] -
                      TScopeDisplay(DisplayObj).yMin[ch])
                      *TScopeDisplay(DisplayObj).ChanScale[ch]*0.1 ;
         CalibrationBarTable.cells[0,Row] := TScopeDisplay(DisplayObj).ChanName[ch] ;
         CalibrationBarTable.cells[1,Row] := Format( '%.4g %s',
                                              [BarSize,
                                               TScopeDisplay(DisplayObj).ChanUnits[ch]]) ;
         Inc(Row) ;
         end ;

      { Ensure calibration bars have focus when dialog box opens }
      CalibrationBarTable.SetFocus ;

     end;


procedure TPrintRecFrm.bPrintClick(Sender: TObject);
{ -----------------------
  Update global settings
  ----------------------}
var
   ch : Integer ;
begin

      { Update settings from text boxes }
      MainFrm.PrinterLeftMargin := Round(edLeftMargin.Value) ;
      MainFrm.PrinterRightMargin := Round(edRightMargin.Value) ;
      MainFrm.PrinterTopMargin := Round(edTopMargin.Value) ;
      MainFrm.PrinterBottomMargin := Round(edBottomMargin.Value) ;
      MainFrm.ClipboardBitMapWidth := Round(edWidth.Value) ;
      MainFrm.ClipboardBitMapHeight := Round(edHeight.Value) ;
      MainFrm.PrinterFontSize := Round(edFontSize.Value) ;
      MainFrm.PrinterLineThickness := Round(edLineThickness.Value) ;
      MainFrm.PrinterFontName :=  cbFontName.text ;

      MainFrm.PrinterShowLabels := ckShowLabels.checked ;
      MainFrm.PrinterShowZeroLevels := ckShowZeroLevels.checked ;
      MainFrm.PrinterUseColor := ckUseColor.checked ;

      { Time calibration }

      { Copy data into display object }
      TScopeDisplay(DisplayObj).PrinterLeftMargin := Round(MainFrm.PrinterLeftMargin) ;
      TScopeDisplay(DisplayObj).PrinterRightMargin := Round(MainFrm.PrinterRightMargin) ;
      TScopeDisplay(DisplayObj).PrinterTopMargin := Round(MainFrm.PrinterTopMargin) ;
      TScopeDisplay(DisplayObj).PrinterBottomMargin := Round(MainFrm.PrinterBottomMargin) ;
      TScopeDisplay(DisplayObj).PrinterFontName := MainFrm.PrinterFontName ;
      TScopeDisplay(DisplayObj).PrinterFontSize := MainFrm.PrinterFontSize ;
      TScopeDisplay(DisplayObj).PrinterPenWidth := MainFrm.PrinterLineThickness ;
      TScopeDisplay(DisplayObj).MetafileWidth := MainFrm.ClipboardBitMapWidth ;
      TScopeDisplay(DisplayObj).MetafileHeight := MainFrm.ClipboardBitMapHeight ;
      TScopeDisplay(DisplayObj).PrinterShowZeroLevels := MainFrm.PrinterShowZeroLevels ;
      TScopeDisplay(DisplayObj).PrinterShowLabels := MainFrm.PrinterShowLabels ;
      TScopeDisplay(DisplayObj).PrinterDisableColor := not MainFrm.PrinterUseColor ;

      for ch := 0 to TScopeDisplay(DisplayObj).NumChannels-1 do
          TScopeDisplay(DisplayObj).ChanCalBar[ch] := ExtractFloat(
                                                     CalibrationBarTable.Cells[1,ch+1],10. ) ;
      TScopeDisplay(DisplayObj).TCalBar := ExtractFloat(CalibrationBarTable.cells[1,0], 1. )/
                                           TScopeDisplay(DisplayObj).TScale ;

     end ;


procedure TPrintRecFrm.CalibrationBarTableKeyPress(
          Sender: TObject;
          var Key: Char);
var
   Value : single ;
   ch,Row : Integer ;
begin
     if key = chr(13) then begin
         { Time calibration bar }
         Value := ExtractFloat( CalibrationBarTable.cells[1,0], 1. ) ;
         CalibrationBarTable.cells[1,0] := Format( '%.4g %s',
                                       [Value,MainFrm.TUnits])  ;

         { Channel calibration bars }
         Row := 1 ;
         for ch := 0 to TScopeDisplay(DisplayObj).NumChannels-1 do begin
             Value := ExtractFloat(CalibrationBarTable.Cells[1,Row],10. ) ;
             CalibrationBarTable.cells[0,Row] := TScopeDisplay(DisplayObj).ChanName[ch] ;
             CalibrationBarTable.cells[1,Row] := Format( '%.4g %s',
                                                 [Value,
                                                  TScopeDisplay(DisplayObj).ChanUnits[ch]]) ;
             Inc(Row) ;
             end ;
         end ;

     end;


procedure TPrintRecFrm.bDefaultSettingsClick(Sender: TObject);
// ---------------------------------------------------------
// Set calibration values to default settings (10% of range)
// ---------------------------------------------------------
var
     ch,Row : Integer ;
begin

      MainFrm.PrinterTimeCalibrationBar := (TScopeDisplay(DisplayObj).xMax -
                                            TScopeDisplay(DisplayObj).xMin)*
                                            TScopeDisplay(DisplayObj).TScale*0.1 ;

      CalibrationBarTable.cells[1,0] := Format( '%.4g %s',
                                       [MainFrm.PrinterTimeCalibrationBar,
                                        MainFrm.TUnits])  ;

      Row := 1 ;
      for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
         MainFrm.PrinterCalibrationBar[ch] := ((TScopeDisplay(DisplayObj).yMax[ch] -
                                                TScopeDisplay(DisplayObj).yMin[ch])
                                               *TScopeDisplay(DisplayObj).ChanScale[ch]*0.1) ;
         CalibrationBarTable.cells[0,Row] := TScopeDisplay(DisplayObj).ChanName[ch] ;
         CalibrationBarTable.cells[1,Row] := Format( '%.4g %s',
                                              [MainFrm.PrinterCalibrationBar[ch],
                                               TScopeDisplay(DisplayObj).ChanUnits[ch]]) ;
         Inc(Row) ;
         end ;
      end;


procedure TPrintRecFrm.bPrinterSetupClick(Sender: TObject);
// --------------------------------
// Display printer setup dialog box
// --------------------------------
begin
     MainFrm.PrinterSetupDialog.Execute ;
     edPrinterName.Text := GetCurrentPrinterName ;
     end;


function TPrintRecFrm.GetCurrentPrinterName : string ;
// --------------------------------------
// Get name of current default printer
// --------------------------------------
const
    MaxSize = 256 ;
var
   n,ch,Row : Integer ;
   DeviceName,DeviceDriver,Port : PChar ;
   DeviceMode : THandle ;
begin
        GetMem( DeviceName, MaxSize*SizeOf(Char) ) ;
        GetMem( DeviceDriver, MaxSize*SizeOf(Char) ) ;
        GetMem( Port, MaxSize*SizeOf(Char) ) ;
        {$IF CompilerVersion > 7.0}
        Printer.GetPrinter( DeviceName, DeviceDriver,Port,DeviceMode );
        {$ELSE}
        Printer.GetPrinter( DeviceName, DeviceDriver,Port, DeviceMode );
        {$IFEND}

        Result := String(DeviceName) ;
        FreeMem(DeviceName) ;
        FreeMem(DeviceDriver) ;
        FreeMem(Port) ;
        end ;



end.
