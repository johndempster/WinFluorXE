unit Printgra;
{ =======================================================
  Updates printer page settings in Settings.Plot
  21/2/00
  =======================================================}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin, Buttons, Printers,
  ValEdit, XMultiYPlot, XYPlotDisplay, ExtCtrls, ValidatedEdit ;

type
  TPrintGraphFrm = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    FontGrp: TGroupBox;
    Label7: TLabel;
    cbFontName: TComboBox;
    edFontSize: TValidatedEdit;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    edLineThickness: TValidatedEdit;
    Page: TNotebook;
    PrinterGrp: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edLeftMargin: TValidatedEdit;
    edRightMargin: TValidatedEdit;
    edTopMargin: TValidatedEdit;
    edBottomMargin: TValidatedEdit;
    MetafileGrp: TGroupBox;
    Label6: TLabel;
    Label9: TLabel;
    edBitmapWidth: TValidatedEdit;
    edBitmapHeight: TValidatedEdit;
    edMarkerSize: TValidatedEdit;
    Label8: TLabel;
    ckUseColor: TCheckBox;
    GroupBox6: TGroupBox;
    bPrinterSetup: TButton;
    edPrinterName: TEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bPrinterSetupClick(Sender: TObject);
  private
    { Private declarations }
    function GetCurrentPrinterName : string ;

  public
    { Public declarations }
    ToPrinter : Boolean ;
    MultiYPlot : Boolean ;
    Plot : TObject ;
    XYPlot : TXYPlotDisplay ;
    MXYPlot : TXMultiYPlot ;
  end;

var
  PrintGraphFrm: TPrintGraphFrm;

implementation

{$R *.DFM}

Uses Main ;


procedure TPrintGraphFrm.FormShow(Sender: TObject);
{ --------------------------------------------
  Initialise edit boxes when form is displayed
  -------------------------------------------- }
begin

     ClientWidth := Page.Left + Page.Width + 5 ;
     ClientHeight := bOK.Top + bOK.Height + 5 ;

     { Select appropriate page for printer page margins or
       metafile image size }
     if ToPrinter then begin
        Page.PageIndex := 0 ;
        Caption := ' Print ' ;
        edPrinterName.Text := GetCurrentPrinterName ;
        bPrinterSetup.Enabled := True ;
        edFontSize.Units := 'pts' ;
        edMarkerSize.Units := 'pts' ;
        edLineThickness.Units := 'pts' ;
        end
     else begin
        Page.PageIndex := 1 ;
        Caption := ' Copy Image ' ;
        edPrinterName.Text := 'Windows Clipboard' ;
        bPrinterSetup.Enabled := False ;
        edFontSize.Units := 'pixels' ;
        edMarkerSize.Units := 'pixels' ;
        edLineThickness.Units := 'pixels' ;
        end ;

     edTopMargin.Value := MainFrm.PrinterTopMargin ;
     edBottomMargin.Value := MainFrm.PrinterBottomMargin ;
     edLeftMargin.Value := MainFrm.PrinterLeftMargin ;
     edRightMargin.Value := MainFrm.PrinterRightMargin ;
     edBitMapWidth.Value := MainFrm.ClipboardBitMapWidth ;
     edBitMapHeight.Value := MainFrm.ClipboardBitMapHeight ;

     { Fill Fonts list with typefaces available to printer }
     cbFontName.items := printer.fonts ;
     edFontSize.Value := MainFrm.PrinterFontSize ;
     edLineThickness.Value := MainFrm.PrinterLineThickness ;
     edMarkerSize.Value := MainFrm.PrinterMarkerSize ;
     cbFontName.itemindex := cbFontName.items.indexof(MainFrm.PrinterFontName) ;
     if cbFontName.itemindex < 0 then  cbFontName.itemindex := 0 ;
     ckUseColor.checked := MainFrm.PrinterUseColor ;

     end;


procedure TPrintGraphFrm.bOKClick(Sender: TObject);
begin
     { Save new settings }
     MainFrm.PrinterTopMargin := Round(edTopMargin.Value) ;
     MainFrm.PrinterBottomMargin :=  Round(edBottomMargin.Value) ;
     MainFrm.PrinterLeftMargin:=  Round(edLeftMargin.Value)  ;
     MainFrm.PrinterRightMargin :=  Round(edRightMargin.Value) ;
     MainFrm.PrinterFontName := cbFontName.text ;
     MainFrm.PrinterFontSize := Round(edFontSize.Value) ;
     MainFrm.PrinterLineThickness := Round(edLineThickness.Value) ;
     MainFrm.PrinterMarkerSize := Round(edMarkerSize.Value) ;
     MainFrm.ClipboardBitMapWidth := Round(edBitMapWidth.Value)  ;
     MainFrm.ClipboardBitMapHeight := Round(edBitMapHeight.Value)  ;
     MainFrm.PrinterUseColor := ckUseColor.checked ;

     { Update settings in XYplot component }
     if  MultiYPlot then begin
         MXYPlot := TXMultiYPlot(Plot) ;
         MXYPlot.PrinterTopMargin := Round(edTopMargin.Value) ;
         MXYPlot.PrinterBottomMargin := Round(edBottomMargin.Value) ;
         MXYPlot.PrinterLeftMargin := Round(edLeftMargin.Value) ;
         MXYPlot.PrinterRightMargin := Round(edRightMargin.Value) ;
         MXYPlot.PrinterDisableColor := not ckUseColor.checked ;
         MXYPlot.PrinterFontSize := Round(edFontSize.Value) ;
         MXYPlot.PrinterFontName := cbFontName.text ;
         MXYPlot.PrinterLineWidth := Round(edLineThickness.Value) ;
         MainFrm.PrinterMarkerSize := Round(edMarkerSize.Value) ;
         MXYPlot.PrinterMarkerSize := Round(edMarkerSize.Value) ;
         MXYPlot.MetafileWidth := Round(edBitMapWidth.Value)  ;
         MainFrm.ClipboardBitMapHeight := Round(edBitMapHeight.Value)  ;
         MXYPlot.MetafileHeight := Round(edBitMapHeight.Value)  ;
         end
     else begin
         XYPlot := TXYPlotDisplay(Plot) ;
         XYPlot.PrinterTopMargin := Round(edTopMargin.Value) ;
         XYPlot.PrinterBottomMargin := Round(edBottomMargin.Value) ;
         XYPlot.PrinterLeftMargin := Round(edLeftMargin.Value) ;
         XYPlot.PrinterRightMargin := Round(edRightMargin.Value) ;
         XYPlot.PrinterDisableColor := not ckUseColor.checked ;
         XYPlot.PrinterFontSize := Round(edFontSize.Value) ;
         XYPlot.PrinterFontName := cbFontName.text ;
         XYPlot.PrinterLineWidth := Round(edLineThickness.Value) ;
         MainFrm.PrinterMarkerSize := Round(edMarkerSize.Value) ;
         XYPlot.PrinterMarkerSize := Round(edMarkerSize.Value) ;
         XYPlot.MetafileWidth := Round(edBitMapWidth.Value)  ;
         MainFrm.ClipboardBitMapHeight := Round(edBitMapHeight.Value)  ;
         XYPlot.MetafileHeight := Round(edBitMapHeight.Value)  ;
         end

     end;


procedure TPrintGraphFrm.bPrinterSetupClick(Sender: TObject);
// --------------------------------
// Display printer setup dialog box
// --------------------------------
begin
     MainFrm.PrinterSetupDialog.Execute ;
     edPrinterName.Text := GetCurrentPrinterName ;
     end;


function TPrintGraphFrm.GetCurrentPrinterName : string ;
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
        Printer.GetPrinter( DeviceName, DeviceDriver,Port,DeviceMode );
        Result := String(DeviceName) ;
        FreeMem(DeviceName) ;
        FreeMem(DeviceDriver) ;
        FreeMem(Port) ;
        end ;




end.
