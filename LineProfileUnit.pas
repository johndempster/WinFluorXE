unit LineProfileUnit;
// ---------------------------------------------------
// WinFluor - Line intensity profile display module
// ---------------------------------------------------
// (C) John Dempster, University of Strathclyde, 2002-03
// All Rights Reserved
// ---------------------------------------------------
// 23.03.05 Line profile now works correctly with multi-frame images
// 13.06.07 Calls to PrintGraphFrm no longer cause memory exceptions
//          after form is closed.
// 12.07.07 Cursor readout now displayed within plot
// 10.09.09 JD Edge tracking facility added
//             Pixel intensity now scaled by .IDRFile.IntensityScale & .IDRFile.IntensityOffset

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ExtCtrls, IDRFile, Maths, Math,
  XYPlotDisplay, ComCtrls, RangeEdit ;

type
  TLineProfileFrm = class(TForm)
    Timer: TTimer;
    Page: TPageControl;
    LineProfileTab: TTabSheet;
    EdgeTrackTab: TTabSheet;
    LPControlsGrp: TGroupBox;
    Label5: TLabel;
    Label1: TLabel;
    cbFrameType: TComboBox;
    cbROI: TComboBox;
    plPlot: TXYPlotDisplay;
    EPControlsGrp: TGroupBox;
    Label2: TLabel;
    Button1: TButton;
    cbEdge: TComboBox;
    plEdgePlot: TXYPlotDisplay;
    bDoEdgePlot: TButton;
    GroupBox1: TGroupBox;
    bSetAxes: TButton;
    rbAutoScale: TRadioButton;
    rbManual: TRadioButton;
    RangeGrp: TGroupBox;
    rbAllFrames: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    GroupBox2: TGroupBox;
    rbXAxisFrames: TRadioButton;
    rbXAxisTime: TRadioButton;
    GroupBox3: TGroupBox;
    edLineWidth: TValidatedEdit;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbFrameTypeChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbROIChange(Sender: TObject);
    procedure bSetAxesClick(Sender: TObject);
    procedure bDoEdgePlotClick(Sender: TObject);
    procedure edLineWidthKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    PImageSource : PIntArray ;
    FrameCounter : Array[0..MaxFrameType] of Integer ;
    PlotCursor : Integer ;
    ThresholdCursor : Integer ;
    ThresholdNotDefined : Boolean ;
    EdgePlotCursor : Integer ;
    FirstFrame : Integer ;
    LastFrame : Integer ;
    NextFrame : Integer ;
    TimerProcRunning : Boolean ;

    procedure PlotLine ;
    procedure AddToEdgeTrackPlot ;

  public
    { Public declarations }
    PlotAvailable : Boolean ;
    procedure UpdateSettings ;
    procedure CopyImageToClipboard ;
    procedure CopyDataToClipboard ;
    procedure Print ;

  end;

var
  LineProfileFrm: TLineProfileFrm;

implementation

uses Main, ViewUnit, PlotSetaxes, Printgra;

{$R *.dfm}

const
  TrackLeftEdge = 0 ;
  TrackRightEdge = 1 ;
  TrackWidth = 2 ;

procedure TLineProfileFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
begin

     plPlot.Height := LPControlsGrp.Height ;
     ClientHeight := LPControlsGrp.Top + LPControlsGrp.Height + 5 ;
     ClientWidth := ((ClientHeight*3) div 2) + 5 ;

     // Re-size controls appropriate to size of window
     Resize ;

     // Add readout cursor to plot
     plPlot.ClearVerticalCursors ;
     PlotCursor := plPlot.AddVerticalCursor( clGreen, '?r' , 0 ) ;

     // Add threshold cursor to plot
     plPlot.ClearHorizontalCursors ;
     ThresholdCursor := plPlot.AddHorizontalCursor( clGreen, 'threshold' ) ;
     ThresholdNotDefined := True ;
     plPlot.HorizontalCursors[ThresholdCursor] := 0 ;


     for i := 0 to High(FrameCounter) do FrameCounter[i] := 0 ;

     // Range of frames
     edRange.LoLimit := 1 ;
     edRange.HiLimit := MainFrm.IDRFile.NumFrames ;
     edRange.LoValue :=edRange.LoLimit ;
     edRange.HiValue :=edRange.HiLimit ;

     PlotAvailable := False ;
     plPlot.xAxisAutoRange := True ;
     plPlot.yAxisAutoRange := True ;

     cbEdge.Clear ;
     cbEdge.Items.AddObject('Left edge',TObject(TrackLeftEdge)) ;
     cbEdge.Items.AddObject('Right edge',TObject(TrackRightEdge)) ;
     cbEdge.Items.AddObject('Width',TObject(TrackWidth)) ;
     cbEdge.ItemIndex := 0 ;

     // Add readout cursor to plot
     plEdgePlot.ClearVerticalCursors ;
     EdgePlotCursor := plEdgePlot.AddVerticalCursor( clGreen, '?r', 0 ) ;
     TimerProcRunning := False ;

     Page.ActivePage := LineProfileTab ;

     end;


procedure TLineProfileFrm.UpdateSettings ;
// -------------------------------------------------------
// Update available regions of interest and other settings
// -------------------------------------------------------
var
    i,iOld : Integer ;
begin

    // Create list of line ROIs
    iOld := cbROI.ItemIndex ;
    cbROI.Clear ;
    for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then begin
        case MainFrm.IDRFile.ROI[i].Shape of
             LineROI,PolylineROI,PolygonROI : begin
                cbROI.Items.AddObject( format('ROI.%d',[i]),TObject(i)) ;
                end ;
             end ;
        end ;

    if cbROI.Items.Count > 0 then begin
       cbROI.ItemIndex := 0 ;
       for i := 0 to cbROI.Items.Count-1 do
           if cbROI.Items.Objects[i] = TObject(iOld) then cbROI.ItemIndex := i ;
       end ;

    // Update frame types
    iOld := cbFrameType.ItemIndex ;
    cbFrameType.Clear ;
    for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
        cbFrameType.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
    cbFrameType.ItemIndex := MaxInt( [iOld, 0 ]) ;

    PImageSource := ViewFrm.PImageBufs[cbFrameType.ItemIndex] ;

    end ;


procedure TLineProfileFrm.PlotLine ;
// -----------------
// Plot line profile
// -----------------
var
     i,iROI : Integer ;
     ROI : TROI ;
     XStart : Single ;
     XEnd : Single ;
     YStart : Single ;
     YEnd : Single ;
     XStep, YStep : Single ;
     X, Y : Single ; // line coordinates
     SlopeAngle : Single ; // Angle of slope of line (radians)
     D : Single ;    // Distance along line
     DStart,DEnd : Single ; // Length of line
     iPixel,iXPix,iYPix : Integer ;      // Pixel coord.
     iLine,nLines : Integer ;
     Sum,HalfPi : Single ;
     LineWidth : Integer ;
     AvgIntensity,MaxIntensity : Single ;
begin

     { New Y Axis on plot }
     PlotAvailable := False ;
     plPlot.ClearAllLines ;

     if (cbROI.ItemIndex < 0) then Exit ;

     // Region of interest
     iROI := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
     ROI := MainFrm.IDRFile.ROI[iROI] ;

     // Image source buffer
     PImageSource := ViewFrm.PImageBufs[cbFrameType.ItemIndex] ;

     // Create line on plot
     plPlot.CreateLine( 0,clBlue, msNone, psSolid ) ;

     plPlot.YAxisAutoRange := rbAutoScale.Checked ;
     plPlot.XAxisAutoRange := rbAutoScale.Checked ;
     plPlot.XAXisLabel := Mainfrm.IDRFIle.ResolutionUnits ;

     case ROI.Shape of
        PolylineROI,PolygonROI : nLines := ROI.NumPoints-1 ;
        else nLines := 1 ;
        end ;

     DStart := 0.0 ;
     MaxIntensity := 0.0 ;
     for iLine := 0 to nLines-1 do begin

        // Get line segment
        case ROI.Shape of
           PolylineROI,PolygonROI : begin
              XStart := ROI.XY[iLine].X ;
              YStart := ROI.XY[iLine].Y ;
              XEnd := ROI.XY[iLine+1].X ;
              YEnd := ROI.XY[iLine+1].Y ;
              end ;
           else begin
              XStart := ROI.TopLeft.X ;
              YStart := ROI.TopLeft.Y ;
              XEnd := ROI.BottomRight.X ;
              YEnd := ROI.BottomRight.Y ;
              end ;
           end ;

        // Plot line
        if XEnd <> XStart then SlopeAngle := ArcTan2( YEnd - YStart, XEnd - XStart )
                          else  SlopeAngle := 0.5*Pi ;

        X := XStart ;
        Y := YStart ;
        XStep := Cos(SlopeAngle) ;
        YStep := Sin(SlopeAngle) ;
        DEnd :=  Sqrt( (XEnd - XStart)*(XEnd - XStart)
                       + (YEnd - YStart)*(YEnd - YStart)) ;
        HalfPi := Pi/2.0 ;
        LineWidth := Round(edLineWidth.Value) ;
        if (LineWidth mod 2) = 0 then Inc(LineWidth) ;
        edLineWidth.Value := LineWidth ;
        Repeat

           // Distance along line profile
           D :=  Sqrt((X - XStart)*(X - XStart) + (Y - YStart)*(Y - YStart)) ;

           Sum := 0.0 ;
           for i := -(LineWidth div 2) to (LineWidth div 2) do begin
              // Nearest pixel
              iXPix := Min(Max(Round(X + i*Cos(SlopeAngle+HalfPi)),0),MainFrm.IDRFile.FrameWidth-1) ;
              iYPix := Min(Max(Round(Y + i*Sin(SlopeAngle+HalfPi)),0),MainFrm.IDRFile.FrameHeight-1) ;
              Sum := Sum + PImageSource^[iXPix + iYPix*MainFrm.IDRFile.FrameWidth] ;
              end ;

           AvgIntensity := MainFrm.IDRFile.IntensityScale*
                           ((Sum/LineWidth) - MainFrm.IDRFile.IntensityOffset) ;
           MaxIntensity := Max( MaxIntensity, AvgIntensity ) ;

           // Plot pixel intensity vs D
           plPlot.AddPoint( 0, (D+DStart)*MainFrm.IDRFile.XResolution, AvgIntensity ) ;

           // Increment to next pixel on line
           X := X + XStep ;
           Y := Y + YStep ;

           Until Round(D) >= Round(DEnd) ;

        DStart := DStart + DEnd ;

        end ;

     // Plot graph of currently selected variables }
 {    if plPlot.xAxisAutoRange then begin
        plPlot.xAxisAutoRange := False ;
        plPlot.XAxisMin := 0.0 ;
        plPlot.XAxisMax := DStart*Mainfrm.IDRFIle.XResolution ;
        plPlot.XAxisTick := (plPlot.XAxisMax - plPlot.XAxisMin) / 5.0 ;
        plPlot.XAXisLabel := Mainfrm.IDRFIle.ResolutionUnits ;
        end ;}

     { Create X and Y axes labels }
     plPlot.xAxisLabel := MainFrm.IDRFile.ResolutionUnits ;
     plPlot.yAxisLabel := format('%s (%s)',
                             [cbFrameType.Text,cbROI.Text]) ;

     plPlot.VerticalCursors[PlotCursor] := (plPlot.XAxisMin + plPlot.XAxisMax)*0.5 ;
     if ThresholdNotDefined then begin
        plPlot.HorizontalCursors[ThresholdCursor] := MaxIntensity*0.5 ;
        ThresholdNotDefined := False ;
        end ;

     PlotAvailable := True ;

     end ;


procedure TLineProfileFrm.AddToEdgeTrackPlot ;
// ------------------------
// Plot time course of edge
// ------------------------
var
     iROI : Integer ;
     ROI : TROI ;
     XStart, XEnd : Single ; // X limits of line
     YStart, YEnd : single ; // Y limits of line
     XStep, YStep : Single ;
     X, Y : Single ; // line coordinates
     Slope : Single ; // Slope of line
     D : PSingleArray ;                 // Distance along line
     PixelIntensity : PSingleArray ;    // Pixel intensity along line
     DStart,DEnd,DSeg : Single ;        // Length of line
     iXPix,iYPix : Integer ;            // Pixel coord.
     i,nPoints : Integer ;
     EdgeThreshold : Single ;
     t : Single ;
     Done : Boolean ;
     iLine,nLines : Integer ;
     Sum,HalfPi : Single ;
     LineWidth : Integer ;
     SlopeAngle : Single ;
     DLeftEdge,DRightEdge : Single ;
begin

     if (cbROI.ItemIndex < 0) then Exit ;
     if ViewFrm.FrameNumber <> NextFrame then Exit ;

     GetMem( D, Sizeof(Single)*MainFrm.IDRFile.NumPixelsPerFrame ) ;
     GetMem( PixelIntensity, Sizeof(Single)*MainFrm.IDRFile.NumPixelsPerFrame ) ;

     if FirstFrame = NextFrame then begin

        { New Y Axis on plot }
        PlotAvailable := False ;
        plEdgePlot.ClearAllLines ;

        // Create line on plot
        plEdgePlot.CreateLine( 0,clBlue, msNone, psSolid ) ;

        if rbXAxisTime.Checked then begin
           plEdgePlot.xAxisAutoRange := True ;
           end
        else begin
           plEdgePlot.XAxisMin := FirstFrame - (FirstFrame mod 10) ;
           plEdgePlot.XAxisMax := LastFrame + 10 - (LastFrame mod 10) ;
           plEdgePlot.XAxisTick := Max((LastFrame - FirstFrame) div 5,1) ;
           plEdgePlot.xAxisAutoRange := False ;
           end ;

        plEdgePlot.XAxisTick := (plEdgePlot.XAxisMax - plEdgePlot.XAxisMin) / 5.0 ;

        plEdgePlot.YAxisAutoRange := True ;

        { Create X and Y axes labels }
        plEdgePlot.xAxisLabel := 's' ;
        plEdgePlot.yAxisLabel := format('%s (%s) %s',
                             [cbFrameType.Text,cbROI.Text,Mainfrm.IDRFile.ResolutionUnits]) ;

        end ;

     // Region of interest
     iROI := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
     ROI := MainFrm.IDRFile.ROI[iROI] ;

     // Image source buffer
     PImageSource := ViewFrm.PImageBufs[cbFrameType.ItemIndex] ;

     case ROI.Shape of
        PolylineROI,PolygonROI : nLines := ROI.NumPoints-1 ;
        else nLines := 1 ;
        end ;

     DStart := 0.0 ;
     nPoints := 0 ;
     for iLine := 0 to nLines-1 do begin

        // Get line segment
        case ROI.Shape of
           PolylineROI,PolygonROI : begin
              XStart := ROI.XY[iLine].X ;
              YStart := ROI.XY[iLine].Y ;
              XEnd := ROI.XY[iLine+1].X ;
              YEnd := ROI.XY[iLine+1].Y ;
              end ;
           else begin
              XStart := ROI.TopLeft.X ;
              YStart := ROI.TopLeft.Y ;
              XEnd := ROI.BottomRight.X ;
              YEnd := ROI.BottomRight.Y ;
              end ;
           end ;

        // Plot line
        if XEnd <> XStart then SlopeAngle := ArcTan2( YEnd - YStart, XEnd - XStart )
                          else SlopeAngle := 0.5*Pi ;

        X := XStart ;
        Y := YStart ;
        XStep := Cos(SlopeAngle) ;
        YStep := Sin(SlopeAngle) ;
        DEnd :=  Sqrt( (XEnd - XStart)*(XEnd - XStart)
                       + (YEnd - YStart)*(YEnd - YStart)) ;
        HalfPi := Pi/2.0 ;
        LineWidth := Round(edLineWidth.Value) ;
        if (LineWidth mod 2) = 0 then Inc(LineWidth) ;
        edLineWidth.Value := LineWidth ;
        Repeat

           // Distance along line profile
           DSeg := Sqrt((X - XStart)*(X - XStart) + (Y - YStart)*(Y - YStart)) ;

           Sum := 0.0 ;
           for i := -(LineWidth div 2) to (LineWidth div 2) do begin
              // Nearest pixel
              iXPix := Min(Max(Round(X + i*Cos(SlopeAngle+HalfPi)),0),MainFrm.IDRFile.FrameWidth-1) ;
              iYPix := Min(Max(Round(Y + i*Sin(SlopeAngle+HalfPi)),0),MainFrm.IDRFile.FrameHeight-1) ;
              Sum := Sum + PImageSource^[iXPix + iYPix*MainFrm.IDRFile.FrameWidth] ;
              end ;

           // Average pixel intensity vs D
           D[nPoints] := (DStart + DSeg)*MainFrm.IDRFile.XResolution ;
           PixelIntensity[nPoints] := MainFrm.IDRFile.IntensityScale*
                                      ((Sum/LineWidth) - MainFrm.IDRFile.IntensityOffset) ;

           // Increment to next pixel on line
           X := X + XStep ;
           Y := Y + YStep ;
           Inc(nPoints) ;

           Until Round(DSeg) = Round(DEnd) ;

        DStart := DStart + DEnd ;

        end ;

     // Find edge
     // ---------

     // Find Left edge
     i := 0 ;
     DLeftEdge := -1 ;
     EdgeThreshold := plPlot.HorizontalCursors[ThresholdCursor] ;
     repeat
         if PixelIntensity[i] >= EdgeThreshold then begin
            DLeftEdge := D[i] ;
            if (PixelIntensity[i] <> PixelIntensity[Max(i-1,0)]) then begin
               DLeftEdge := DLeftEdge + Abs(D[i] - D[i-1])*
                                        (EdgeThreshold - PixelIntensity[i]) /
                                        Abs(PixelIntensity[i] - PixelIntensity[i-1]) ;
               end ;
            i := nPoints ;
            end ;
         inc(i) ;
         until i >= nPoints ;

     // Find Right edge
     i := nPoints - 1 ;
     DRightEdge := -1 ;
     EdgeThreshold := plPlot.HorizontalCursors[ThresholdCursor] ;
     repeat
         if PixelIntensity[i] >= EdgeThreshold then begin
            DRightEdge := D[i] ;
            if PixelIntensity[Min(i+1,nPoints-1)] <> PixelIntensity[i] then begin
               DRightEdge := DRightEdge - Abs(D[i] - D[i+1])*
                                          (EdgeThreshold - PixelIntensity[i]) /
                                          Abs(PixelIntensity[i] - PixelIntensity[i+1]) ;
               end ;
            i := 0 ;
            end ;
         Dec(i) ;
         until i <= 0 ;

     if rbXAxisTime.Checked then t := (NextFrame-FirstFrame)*MainFrm.IDRFile.FrameInterval
                            else t := NextFrame ;

     case cbEdge.ItemIndex of
        TrackLeftEdge : begin
            if DLeftEdge >= 0.0 then plEdgePlot.AddPoint( 0,t,DLeftEdge ) ;
            end ;
        TrackRightEdge : begin
            if DRightEdge >= 0.0 then plEdgePlot.AddPoint( 0,t,DRightEdge ) ;
            end ;
        TrackWidth : begin
            if (DLeftEdge >= 0.0) and (DRightEdge >= 0.0) then
                plEdgePlot.AddPoint( 0,t,DRightEdge - DLeftEdge ) ;
            end ;
        end ;

     PlotAvailable := True ;
     FreeMem(D) ;
     FreeMem(PixelIntensity) ;

     Inc(NextFrame) ;

     if NextFrame <= LastFrame then begin
        ViewFrm.FrameNumber := NextFrame ;
        MainFrm.StatusBar.SimpleText := format('Edge Track Plot: %d/%d',[NextFrame,LastFrame] ) ;

        end
     else begin
         bDoEdgePlot.Enabled := True ;
         plEdgePlot.VerticalCursors[EdgePlotCursor] := (plEdgePlot.XAxisMin + plEdgePlot.XAxisMax)*0.5 ;
         MainFrm.StatusBar.SimpleText := format('Edge Track Plot: %d-%d completed',
                                         [FirstFrame,LastFrame] ) ;
         end ;

     end ;


procedure TLineProfileFrm.FormResize(Sender: TObject);
// ----------------------------------------
// Re-size control when window size changes
// ----------------------------------------
begin

     Page.Width := Max( ClientWidth - Page.Left - 5, 2)  ;
     Page.Height := Max( ClientHeight - Page.Top - 5, 2)  ;

     LPControlsGrp.Height := LineProfileTab.ClientHeight - LPControlsGrp.Top - 5 ;
     plPlot.Width := Max(LineProfileTab.ClientWidth - plPlot.Left - 5,2) ;
     plPlot.Height := Max( LineProfileTab.ClientHeight - plPlot.Top - 5, 2) ;

     EPControlsGrp.Height := EdgeTrackTab.ClientHeight - EPControlsGrp.Top - 5 ;
     plEdgePlot.Width := Max(EdgeTrackTab.ClientWidth - plEdgePlot.Left - 5,2) ;
     plEdgePlot.Height := Max( EdgeTrackTab.ClientHeight - plEdgePlot.Top - 5, 2) ;

     end;


procedure TLineProfileFrm.cbFrameTypeChange(Sender: TObject);
// ------------------
// Frame type changed
// ------------------
begin
     PlotLine ;
     end;


procedure TLineProfileFrm.TimerTimer(Sender: TObject);
// ---------------------
// Scheduled event timer
// ---------------------
var
    i : Integer ;
    NumROIs : Integer ;
    NewSettings : Boolean ;
    NewPlot : Boolean ;
    SourceAvailable : Boolean ;
begin

    if TimerProcRunning then Exit ;
    TimerProcRunning := True ;
    NewSettings := False ;
    NewPlot := False ;

    // Close form if image source no longer exists
    if not MainFrm.FormExists( 'Viewfrm') then begin
       Close ;
       Exit ;
       end ;

    if not bDoEdgePlot.Enabled then AddToEdgeTrackPlot
    else begin

       // Update settings if ROIs changed
       NumROIs := 0 ;
       for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then begin
           case MainFrm.IDRFile.ROI[i].Shape of
             LineROI,PolylineROI,PolygonROI : Inc(NumROIs) ;
             end ;
           end ;
       if NumROIs <> cbROI.Items.Count then NewSettings := True ;

       // Update settings if frame types changed
       if (MainFrm.IDRFile.NumFrameTypes <> cbFrameType.Items.Count) then NewSettings := True
       else begin
           for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
              if MainFrm.IDRFile.FrameType[i] <> cbFrameType.Items[i] then
                 NewSettings := True ;
           end ;

       // Update settings (if required)
       if NewSettings then UpdateSettings ;

       // Update plot
       if ViewFrm.FrameCounter[cbFrameType.ItemIndex]
        <> FrameCounter[cbFrameType.ItemIndex] then NewPlot := True ;

       // Update plot (if required)
       if NewPlot then begin
          PlotLine ;
          FrameCounter[cbFrameType.ItemIndex] := ViewFrm.FrameCounter[cbFrameType.ItemIndex] ;
          end ;

       end ;

    TimerProcRunning := False ;
    Invalidate ;
    Application.HandleMessage ;

    end;


procedure TLineProfileFrm.cbROIChange(Sender: TObject);
// --------------------------
// Region of Interest changed
// --------------------------
begin
     PlotLine ;
     end;


procedure TLineProfileFrm.bSetAxesClick(Sender: TObject);
{ -----------------------------
  Customise Histogram plot axes
  -----------------------------}
begin
     PlotSetAxesFrm.Plot := plPlot ;
     PlotSetAxesFrm.Histogram := False ;
     PlotSetAxesFrm.ShowModal ;
     end;


procedure TLineProfileFrm.CopyDataToClipBoard ;
{ ---------------------------------------------
  Copy the intensity histogram to the clipboard
  --------------------------------------------- }
begin

     if Page.ActivePage = LineProfileTab then plPlot.CopyDataToClipboard
                                         else plEdgePlot.CopyDataToClipboard ;

     end ;


procedure TLineProfileFrm.Print ;
{ ----------------
  Print histogram
  --------------- }
begin

     // Display print dialog box
     PrintGraphFrm.MultiYPlot := False ;
     if Page.ActivePage = LineProfileTab then PrintGraphFrm.Plot := plPlot
                                         else PrintGraphFrm.Plot := plEdgePlot ;
     PrintGraphFrm.ShowModal ;

     if PrintGraphFrm.ModalResult = mrOK then CopyImageToClipboard ;

     if PrintGraphFrm.ModalResult = mrOK then begin
        { Add title information to plot }
        TXYPlotDisplay(PrintGraphFrm.Plot).ClearPrinterTitle ;
        TXYPlotDisplay(PrintGraphFrm.Plot).AddPrinterTitleLine( ' File : ' + MainFrm.IDRFile.FileName ) ;
        TXYPlotDisplay(PrintGraphFrm.Plot).AddPrinterTitleLine( ' ' + MainFrm.IDRFile.Ident ) ;
        { Plot graph to printer }
        TXYPlotDisplay(PrintGraphFrm.Plot).Print ;
        end ;
     end ;


procedure TLineProfileFrm.CopyImageToClipboard ;
{ -----------------------------------------------------
  Copy histogram image to clipboard as Windows metafile
  ----------------------------------------------------- }
begin

     PrintGraphFrm.MultiYPlot := False ;
     PrintGraphFrm.ToPrinter := False ;
     if Page.ActivePage = LineProfileTab then PrintGraphFrm.Plot := plPlot
                                         else PrintGraphFrm.Plot := plEdgePlot ;
     PrintGraphFrm.ShowModal ;
     if PrintGraphFrm.ModalResult = mrOK then
        TXYPlotDisplay(PrintGraphFrm.Plot).CopyImageToClipboard ;

     end ;


procedure TLineProfileFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
begin

     // Request destruction of form object
     Action := caFree ;
     end;


procedure TLineProfileFrm.bDoEdgePlotClick(Sender: TObject);
// ---------------
// Start edge plot
// ---------------
begin

     // Get frame range
     if rbAllFrames.Checked then begin
        FirstFrame := Round(edRange.LoLimit) ;
        LastFrame := Round(edRange.HiLimit) ;
        end
     else begin
        FirstFrame := Round(edRange.LoValue) ;
        LastFrame := Round(edRange.HiValue) ;
        end ;
     NextFrame := FirstFrame ;
     Viewfrm.FrameNumber := NextFrame ;

     bDoEdgePlot.Enabled := False ;

     end;


procedure TLineProfileFrm.edLineWidthKeyPress(Sender: TObject;
  var Key: Char);
// Line width setting changed
// --------------------------
begin
     if Key = #13 then PlotLine ;
     end;

procedure TLineProfileFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
     CanClose := bDoEdgePlot.Enabled ;
     bDoEdgePlot.Enabled := True ;
     end;

procedure TLineProfileFrm.Button1Click(Sender: TObject);
{ -----------------------------
  Customise Edge plot axes
  -----------------------------}
begin
     PlotSetAxesFrm.Plot := plEdgePlot ;
     PlotSetAxesFrm.Histogram := False ;
     PlotSetAxesFrm.ShowModal ;
     end;



end.


