unit SpectrumUnit;
// -----------------------------
// Spectral data analysis module
// -----------------------------
// 25.02.07
// 24.03.07 Time course can now handle 100 lines
//          tick spacing on spectrum now better
// 01.04.07 Min-Max range of spectra now set to all frame range

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, XYPlotDisplay, StdCtrls, RangeEdit, ComCtrls, ValidatedEdit, math, maths,
  HTMLLabel ;

type
  TSpectrumFrm = class(TForm)
    Page: TPageControl;
    SpectrumTab: TTabSheet;
    TimeCourseTab: TTabSheet;
    SpectrumGrp: TGroupBox;
    ControlGrp: TGroupBox;
    sbSpectrumNum: TScrollBar;
    edSpectrumNum: TRangeEdit;
    plSpectrum: TXYPlotDisplay;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    edSpectrumRange: TRangeEdit;
    Label3: TLabel;
    edSpectrumStepSize: TValidatedEdit;
    Label4: TLabel;
    edSpectrumBandwidth: TValidatedEdit;
    GroupBox2: TGroupBox;
    cbROI: TComboBox;
    Label5: TLabel;
    TimeCourseGrp: TGroupBox;
    GroupBox6: TGroupBox;
    plTimeCourse: TXYPlotDisplay;
    GroupBox3: TGroupBox;
    rbAllSpectra: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    bPlotTimeCourse: TButton;
    bSetAxes: TButton;
    lbTimeCourseCursor: THTMLLabel;
    GroupBox4: TGroupBox;
    rbAllWavelengths: TRadioButton;
    rbSingleWavelength: TRadioButton;
    cbTimeCourseWavelength: TComboBox;
    GroupBox5: TGroupBox;
    cbTimeCourseROI: TComboBox;
    Label10: TLabel;
    cbTimeCourseSubtractROI: TComboBox;
    Label6: TLabel;
    Label7: TLabel;
    cbSubtractROI: TComboBox;
    lbSpectrumFrame: TLabel;
    procedure FormShow(Sender: TObject);
    procedure sbSpectrumNumChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edSpectrumRangeKeyPress(Sender: TObject; var Key: Char);
    procedure edSpectrumStepSizeKeyPress(Sender: TObject; var Key: Char);
    procedure edSpectrumBandwidthKeyPress(Sender: TObject; var Key: Char);
    procedure bPlotTimeCourseClick(Sender: TObject);
    procedure bSetAxesClick(Sender: TObject);

    function GetPlotAvailable : Boolean ;
    procedure plTimeCourseCursorChange(Sender: TObject);
    procedure cbSubtractROIChange(Sender: TObject);
    procedure cbROIChange(Sender: TObject);

  private
    { Private declarations }
    ColorSequence : Array[0..9] of TColor ;     // Standard line colour sequence
  public
    { Public declarations }
    NumFramesPerSpectrum : Integer ;

    procedure PlotSpectrum ;
    procedure UpdateSpectrumSettings ;
    procedure CopyImageToClipboard ;
    procedure CopyDataToClipboard ;
    procedure Print ;

    property PlotAvailable : Boolean Read GetPlotAvailable ;

  end;

var
  SpectrumFrm: TSpectrumFrm;

implementation

uses Main, ViewPlotUnit, Printgra, PlotSETAXES;

const
  MaxReadoutLines = 10 ;

{$R *.dfm}

procedure TSpectrumFrm.FormShow(Sender: TObject);
begin

     Resize ;

     UpdateSpectrumSettings ;

     // Plot spectrum
     PlotSpectrum ;

    end ;

procedure TSpectrumFrm.UpdateSpectrumSettings ;
// -------------------------------------
// Initialisation when form is displayed
// -------------------------------------
var
    i,iROI,iSubROI : Integer ;
    Wavelength : Single ;
    Y : Single ;
begin

     edSpectrumRange.LoValue := MainFrm.IDRFile.SpectrumStartWavelength ;
     edSpectrumRange.HiValue := MainFrm.IDRFile.SpectrumEndWavelength ;
     edSpectrumStepSize.Value := MainFrm.IDRFile.SpectrumStepSize ;
     edSpectrumBandWidth.Value := MainFrm.IDRFile.SpectrumBandwidth ;
     NumFramesPerSpectrum := MainFrm.IDRFile.NumFramesPerSpectrum ;

     cbTimeCourseWavelength.Clear ;
     for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do begin
         cbTimeCourseWavelength.Items.AddObject(
         MainFrm.IDRFile.FrameType[i], TObject(i) ) ;
         end ;
     if cbTimeCourseWavelength.Items.Count > 0 then cbTimeCourseWavelength.ItemIndex := 0 ;

     // Fill regions of interest list
     cbROI.Clear ;
     cbSubtractROI.Clear ;
     cbSubtractROI.Items.AddObject( ' ', TObject(MainFrm.IDRFile.MaxROI+1)) ;
     cbTimeCourseROI.Clear ;
     cbTimeCourseSubtractROI.Clear ;
     cbTimeCourseSubtractROI.Items.AddObject( ' ', TObject(MainFrm.IDRFile.MaxROI+1)) ;
     for i := 1 to MainFrm.IDRFile.MaxROI do if  MainFrm.IDRFile.ROI[i].InUse then begin
         cbROI.Items.AddObject( format( 'ROI.%d',[i]), TObject(i)) ;
         cbSubtractROI.Items.AddObject( format( 'ROI.%d',[i]), TObject(i)) ;
         cbTimeCourseROI.Items.AddObject( format( 'ROI.%d',[i]), TObject(i)) ;
         cbTimeCourseSubtractROI.Items.AddObject( format( 'ROI.%d',[i]), TObject(i)) ;
         end ;

     if cbROI.Items.Count > 0 then begin
        cbROI.ItemIndex := 0 ;
        cbSubtractROI.ItemIndex := 0 ;
        cbTimeCourseROI.ItemIndex := 0 ;
        cbTimeCourseSubtractROI.ItemIndex := 0 ;
        end ;

     // Set spectrum selection slider & edit box
     sbSpectrumNum.Min := 1 ;
     sbSpectrumNum.Max := MainFrm.IDRFile.NumFrames div NumFramesPerSpectrum ;
     sbSpectrumNum.Position := 1 ;
     edSpectrumNum.LoLimit := sbSpectrumNum.Min ;
     edSpectrumNum.HiLimit := sbSpectrumNum.Max ;

     // Time course spectrum range
     edRange.LoLimit := 1.0 ;
     edRange.LoValue := 1.0 ;
     edRange.HiLimit := edSpectrumNum.HiLimit ;
     edRange.HiValue := edSpectrumNum.HiLimit ;

     ColorSequence[0] := clBlue ;
     ColorSequence[1] := clRed ;
     ColorSequence[2] := clGreen ;
     ColorSequence[3] := clYellow ;
     ColorSequence[4] := clGray ;
     ColorSequence[5] := clBlack ;
     ColorSequence[6] := clOlive ;
     ColorSequence[7] := clPurple ;
     ColorSequence[8] := clAqua ;
     ColorSequence[9] := clMaroon ;


     end;


procedure TSpectrumFrm.PlotSpectrum ;
// ---------------------------------------------------------
// Plot spectrum of selected region of interest & time point
// ---------------------------------------------------------
var
    X,Y : Single ;
    i,iFrame,iROI,iSubROI,iSpec : Integer ;
    YMin : Single ;
    YMax : Single ;
begin

     if NumFramesPerSpectrum < 2 then Exit ;
     if cbROI.Items.Count < 1 then Exit ;

     // Calculate min-max
     YMax := -1E30 ;
     YMin := 1E30 ;
     iROI := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
     iSubROI := Integer(cbSubtractROI.Items.Objects[cbSubtractROI.ItemIndex]) ;
     iSpec := 0 ;
     for iFrame := 1 to MainFrm.IDRFile.NumFrames do begin
         iSpec := (iFrame-1) mod NumFramesPerSpectrum ;
         Y := ViewPlotFrm.ROIIntensity( iROI,iFrame,iSpec ) ;
         if iSubROI <= MainFrm.IDRFile.MaxROI then begin
             Y := Y - ViewPlotFrm.ROIIntensity( iSubROI,iFrame,iSpec ) ;
             end ;
         if Y < YMin then YMin := Y ;
         if Y > YMax then YMax := Y ;
         end ;
     if YMax = YMin then YMax := YMin + 1.0 ;

     { Plot graph of currently selected variables }
     plSpectrum.xAxisAutoRange := False ;
     plSpectrum.xAxisMin := edSpectrumRange.LoValue ;
     plSpectrum.xAxisMax := edSpectrumRange.HiValue ;
     plSpectrum.xAxisTick := Max((plSpectrum.xAxisMax - plSpectrum.xAxisMin)*0.2,5.0) ;

     plSpectrum.yAxisAutoRange := False ;
     plSpectrum.yAxisMin := YMin ;
     plSpectrum.yAxisMax := YMax ;
     plSpectrum.yAxisTick := (YMax - YMin)*0.2 ;

     plSpectrum.MaxPointsPerLine := Max(NumFramesPerSpectrum,2) ;

     plSpectrum.xAxisLabel := 'Wavelength (nm)' ;
     plSpectrum.yAxisLabel := cbROI.Text ;
     if cbSubtractROI.ItemIndex > 0 then
        plSpectrum.yAxisLabel := plSpectrum.yAxisLabel + '-' + cbSubtractROI.Text ;
     // Readout cursor
     plSpectrum.ClearVerticalCursors ;
     plSpectrum.AddVerticalCursor( clBlue, '?r' , 0 ) ;

     // Clear data points line }
     plSpectrum.ClearAllLines ;
     plSpectrum.CreateLine( 0 , clBlue, msOpenSquare, psSolid ) ;

     // Plot spectrum
     X := edSpectrumRange.LoValue ;
     iROI := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
     iSubROI := Integer(cbSubtractROI.Items.Objects[cbSubtractROI.ItemIndex]) ;
     iFrame := ((sbSpectrumNum.Position-1)*NumFramesPerSpectrum) + 1 ;
     lbSpectrumFrame.Caption :=Format( 'Frame=%d (%8.2f s)',
                                    [iFrame,
                                     (iFrame-1)*MainFrm.IDRFile.FrameInterval
                                      ]) ;
     for iSpec := 0 to NumFramesPerSpectrum-1 do begin
         Y := ViewPlotFrm.ROIIntensity( iROI,iFrame+iSpec,iSpec ) ;
         if iSubROI <= MainFrm.IDRFile.MaxROI then begin
            Y := Y - ViewPlotFrm.ROIIntensity( iSubROI,iFrame+iSpec,iSpec) ;
            end ;
         plSpectrum.AddPoint(0, X, Y ) ;
         X := X + edSpectrumStepSize.Value ;
         Inc(iFrame) ;
         end ;

     // Place readout cursor in centre of graph
     plSpectrum.VerticalCursors[0] := 0.5*(plSpectrum.xAxisMin + plSpectrum.xAxisMax) ;

     edSpectrumNum.LoValue := sbSpectrumNum.Position ;
     edSpectrumNum.HiValue := edSpectrumNum.HiLimit ;

     end ;


procedure TSpectrumFrm.sbSpectrumNumChange(Sender: TObject);
//
// Spectrum sliderchanged
begin
     PlotSpectrum ;
     end;


procedure TSpectrumFrm.FormResize(Sender: TObject);
// --------------------------------------
// Resize controls when form size changes
// --------------------------------------
begin

     Page.Width := ClientWidth - Page.Left - 2 ;
     Page.Height := ClientHeight - Page.Top - 2 ;

     SpectrumGrp.Height := SpectrumTab.ClientHeight - SpectrumGrp.Top ;
     plSpectrum.Width := Max(SpectrumTab.ClientWidth - plSpectrum.Left - 2,2) ;
     plSpectrum.Height := Max(SpectrumTab.ClientHeight - plSpectrum.Top - 2,2) ;
     plSpectrum.Height := Max(SpectrumTab.ClientHeight - plSpectrum.Top -5,2) ;

     TimeCourseGrp.Height := TimeCourseTab.ClientHeight - TimeCourseGrp.Top ;
     plTimeCourse.Width := Max(TimeCourseTab.ClientWidth - plTimeCourse.Left -2,2) ;
     lbTimeCourseCursor.Top := TimeCourseTab.ClientHeight - lbTimeCourseCursor.Height - 2 ;
     plTimeCourse.Height := Max(lbTimeCourseCursor.Top - plTimeCourse.Top -2,2) ;

     end;

procedure TSpectrumFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ----------
// Close form
// ----------
begin
     Action := caFree ;
     end;


procedure TSpectrumFrm.edSpectrumRangeKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------
// Spectrum wavelength range changed
// ---------------------------------
begin
     if Key = #13 then begin
        MainFrm.IDRFile.SpectrumStartWavelength := edSpectrumRange.LoValue ;
        MainFrm.IDRFile.SpectrumEndWavelength := edSpectrumRange.HiValue ;
        UpdateSpectrumSettings ;
        end ;
     end;


procedure TSpectrumFrm.edSpectrumStepSizeKeyPress(Sender: TObject;
  var Key: Char);
// --------------------------
// Spectrum step size changed
// --------------------------
begin
     if Key = #13 then begin
        MainFrm.IDRFile.SpectrumStepSize := edSpectrumStepSize.Value ;
        UpdateSpectrumSettings ;
        end ;
     end;

procedure TSpectrumFrm.edSpectrumBandwidthKeyPress(Sender: TObject;
  var Key: Char);
// --------------------------
// Spectrum bandwidth changed
// --------------------------
begin
     if Key = #13 then begin
        MainFrm.IDRFile.SpectrumBandwidth := edSpectrumBandwidth.Value ;
        UpdateSpectrumSettings ;
        end ;
     end;


procedure TSpectrumFrm.CopyDataToClipBoard ;
{ ---------------------------------------------
  Copy the graph plot(s) data to the clipboard
  --------------------------------------------- }
var
    Plot : TXYPlotDisplay ;
begin

     // Select plot
     if Page.ActivePage = SpectrumTab then Plot := plSpectrum
                                      else Plot := plTimeCourse ;

     Plot.CopyDataToClipboard ;

     end ;


procedure TSpectrumFrm.Print ;
{ ------------------------------
  Print graph plot(s) on display
  ------------------------------ }
var
    Plot : TXYPlotDisplay ;
begin

     // Select plot
     if Page.ActivePage = SpectrumTab then Plot := plSpectrum
                                      else Plot := plTimeCourse ;

     PrintGraphFrm.MultiYPlot := False ;
     PrintGraphFrm.Plot := Plot ;
     PrintGraphFrm.ToPrinter := True ;
     PrintGraphFrm.MultiYPlot := False ;
     PrintGraphFrm.ShowModal ;
     if PrintGraphFrm.ModalResult = mrOK then begin
        Plot.ClearPrinterTitle ;
        Plot.AddPrinterTitleLine( ' File : ' + MainFrm.IDRFile.FileName ) ;
        Plot.AddPrinterTitleLine( ' ' + MainFrm.IDRFile.Ident ) ;
        Plot.Print ;
        end ;

     end ;


procedure TSpectrumFrm.CopyImageToClipboard ;
{ ------------------------------------------------------------
  Copy image of graph plot(s) to clipboard as Windows metafile
  ------------------------------------------------------------ }
var
    Plot : TXYPlotDisplay ;
begin

     // Select plot
     if Page.ActivePage = SpectrumTab then Plot := plSpectrum
                                      else Plot := plTimeCourse ;

     PrintGraphFrm.MultiYPlot := False ;
     PrintGraphFrm.Plot := Plot ;
     PrintGraphFrm.ToPrinter := False ;
     PrintGraphFrm.MultiYPlot := False ;
     PrintGraphFrm.ShowModal ;
     if PrintGraphFrm.ModalResult = mrOK then Plot.CopyImageToClipboard ;

     end ;


function TSpectrumFrm.GetPlotAvailable : Boolean ;
// -------------------------------------------------------
// Return TRUE if a plot is available for copying/printing
// -------------------------------------------------------
var
    Plot : TXYPlotDisplay ;
begin

     // Select plot
     if Page.ActivePage = SpectrumTab then Plot := plSpectrum
                                      else Plot := plTimeCourse ;
     Result := Plot.Available ;

     end ;


procedure TSpectrumFrm.bPlotTimeCourseClick(Sender: TObject);
// --------------------------------------------------------------
// Plot time course of selected wavelength and region of interest
// --------------------------------------------------------------
var
    X,Y : Single ;
    i,W,iFrame,iSpec,iWavelength,StartAt,EndAt : Integer ;
    iROI,iSubROI : Integer ;
    NumLines : Integer ;
begin

     if NumFramesPerSpectrum < 2 then Exit ;
     if cbTimeCourseROI.Items.Count < 1 then Exit ;

     { Plot graph of currently selected variables }
     plTimeCourse.xAxisAutoRange := True ;
     plTimeCourse.yAxisAutoRange := True ;

     plTimeCourse.MaxPointsPerLine := Max((MaxFrames div NumFramesPerSpectrum)+1,2) ;

     plTimeCourse.yAxisLabel := cbTimeCourseROI.Text ;
     if cbTimeCourseSubtractROI.ItemIndex > 0 then
        plTimeCourse.yAxisLabel := plTimeCourse.yAxisLabel + '-' + cbTimeCourseSubtractROI.Text ;
     if rbAllWavelengths.Checked then begin
         plTimeCourse.yAxisLabel := plTimeCourse.yAxisLabel
          + ' (' + cbTimeCourseWavelength.Items.Strings[0] + '-'
          + cbTimeCourseWavelength.Items.Strings[cbTimeCourseWavelength.Items.Count-1] + ')'  ;
        end
     else begin
        plTimeCourse.yAxisLabel := plTimeCourse.yAxisLabel
                                   + ' (' + cbTimeCourseWavelength.Text + ')' ;
        end ;
     plTimeCourse.xAxisLabel := 's' ;

     // Readout cursor
     plTimeCourse.ClearVerticalCursors ;
     plTimeCourse.AddVerticalCursor( clBlue, '', 0 ) ;

     // Clear data points line }
     plTimeCourse.ClearAllLines ;

     if rbAllSpectra.Checked then begin
        StartAt := 1 ;
        EndAt := Round(edRange.HiLimit) ;
        end
     else begin
        StartAt := Round(edRange.LoValue) ;
        EndAt := Round(edRange.HiValue) ;
        end ;

     // Regions of interest
     iROI := Integer(cbTimeCourseROI.Items.Objects[cbTimeCourseROI.ItemIndex]) ;
     iSubROI := Integer(cbTimeCourseSubtractROI.Items.Objects[cbTimeCourseSubtractROI.ItemIndex]) ;

     NumLines := 0 ;
     for W := 0 to cbTimeCourseWavelength.Items.Count-1 do
         if rbAllWavelengths.Checked or (W = cbTimeCourseWavelength.ItemIndex) then begin

         if NumLines >= XYPlotGraphMaxLines then Break ;

         // Create line }
         plTimeCourse.CreateLine( NumLines ,
                                  ColorSequence[NumLines mod High(ColorSequence)],
                                  msOpenSquare,
                                  psSolid ) ;

         // Plot spectrum
         iWavelength := Integer(cbTimeCourseWavelength.Items.Objects[W]) ;

         X := 0.0 ;
         for iSpec := StartAt to EndAt do begin
             iFrame := ((iSpec-1)*NumFramesPerSpectrum) + iWavelength + 1 ;
             X := MainFrm.IDRFile.FrameInterval*iFrame ;
             Y := ViewPlotFrm.ROIIntensity( iROI,iFrame+iWavelength,iWavelength ) ;
             if iSubROI <= MainFrm.IDRFile.MaxROI then begin
                Y := Y - ViewPlotFrm.ROIIntensity( iSubROI,
                                                   iFrame+iWavelength,
                                                   iWavelength ) ;
                end ;
             plTimeCourse.AddPoint(NumLines, X, Y ) ;
             X := X + MainFrm.IDRFile.FrameInterval*NumFramesPerSpectrum ;
             end ;

         Inc(NumLines) ;

         end ;

     // Place readout cursor in centre of graph
     plTimeCourse.VerticalCursors[0] := 0.5*(plTimeCourse.xAxisMin + plTimeCourse.xAxisMax) ;

     end ;


procedure TSpectrumFrm.bSetAxesClick(Sender: TObject);
//  --------------------------------------------
// Customise plot axes range, labels and styles
// --------------------------------------------
begin
     PlotSetAxesFrm.Plot := plTimeCourse ;
     PlotSetAxesFrm.ShowModal ;
     end;


procedure TSpectrumFrm.plTimeCourseCursorChange(Sender: TObject);
// --------------------------------
// Time course readout cursor moved
// --------------------------------
var
    L : Integer ;
    s,Wavelength : String ;
    x,y : Single ;
begin

     for L := 0 to Min(plTimeCourse.NumLines-1,MaxReadoutLines) do begin

         { Set readout cursor label }
         plTimeCourse.GetPoint( L,
                                plTimeCourse.FindNearestIndex(L,0),
                                x, y ) ;

         if rbAllWavelengths.Checked then begin
            Wavelength := cbTimeCourseWavelength.Items.Strings[L] ;
            end
         else begin
            Wavelength := cbTimeCourseWavelength.Text ;
            end ;

         if L = 0 then s := format('t=%.4g s',[x]) ;

         s := s + format('<br><font Color=%s>%s= %.4g</font>',
                         [HTMLColorString(ColorSequence[L]),
                          Wavelength,
                          y] ) ;
         end ;

     lbTimeCourseCursor.Caption := s ;

     lbTimeCourseCursor.Left := Min( plTimeCourse.Left
                                     + plTimeCourse.XToCanvasCoord( x )
                                     - (lbTimeCourseCursor.Width div 2),
                                     TimeCourseTab.ClientWidth - lbTimeCourseCursor.Width) ;


     end ;


procedure TSpectrumFrm.cbSubtractROIChange(Sender: TObject);
// -----------------------
// Subtraction ROI changed
// -----------------------
begin
     PlotSpectrum ;
     end;


procedure TSpectrumFrm.cbROIChange(Sender: TObject);
begin
     PlotSpectrum ;
     end;

end.
