unit HistogramUnit;
// ---------------------------------------------------
// WinFluor -Image intensity histogram display module
// ---------------------------------------------------
// (C) John Dempster, University of Strathclyde, 2002-03
// All Rights Reserved
// ---------------------------------------------------
// 1/3/2002
// 31/7/2003 ... Region of interest histograms added
//               Histogram now updates automatically when frame changed
// 3.8.03  Line ROI added
// 24.09.05 Histogram now limited to 1024 bins Histogram range removed
// 04.04.05 Problems with Binwidth scalibng fixed
// 03.05.06 Invalid memory access problem fixed
// 12.07.07 Cursor readout now displayed within plot

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  XYPlotDisplay, StdCtrls, ValEdit, RangeEdit, ExtCtrls,
  ValidatedEdit, IDRFile, strutils, snapunit ;

const
    MaxBins = 1024 ;
    srcRecordWindow = 0 ;
    srcLiveWindow = 1 ;
    srcImagesWindow = 2 ;
    srcLinesWindow = 3 ;

type
  THistogramFrm = class(TForm)
    HistGrp: TGroupBox;
    Label1: TLabel;
    cbSource: TComboBox;
    GroupBox1: TGroupBox;
    bFullRange: TButton;
    bAutoScale: TButton;
    GroupBox2: TGroupBox;
    meStatistics: TMemo;
    Timer: TTimer;
    cbFrameType: TComboBox;
    Label5: TLabel;
    ROIGrp: TGroupBox;
    rbWholeImage: TRadioButton;
    rbROI: TRadioButton;
    cbROI: TComboBox;
    bSetAxes: TButton;
    plPlot: TXYPlotDisplay;
    procedure bDoHistogramClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bFullRangeClick(Sender: TObject);
    procedure bAutoScaleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure cbSourceChange(Sender: TObject);
    procedure bSetAxesClick(Sender: TObject);
    procedure rbWholeImageClick(Sender: TObject);
    procedure cbROIChange(Sender: TObject);
    procedure rbROIClick(Sender: TObject);
    procedure cbFrameTypeChange(Sender: TObject);
  private
    { Private declarations }
    Histogram : Array[-MaxBins..MaxBins] of Integer ;
    BinWidth : Integer ;
    NumPixelsPerFrame : Integer ;    // No. pixels in image
    NumBytesPerPixel : Integer ;     // No. pixels per byte
    ReadoutCursor : Integer ;
    FrameCounter : Array[0..MaxFrameType] of Integer ;
    GreyMax : Integer ;
    AutoScaleXAxis : Boolean ;
    FullScaleXAxis : Boolean ;

    procedure DisplayHistogram ;
    procedure FindImageSources ;
    procedure NewSource ;
  public
    { Public declarations }

    NumFramesInBuffer : Integer ;
    LastFrameDisplayed : Integer ;


    MeanZ : Single ;
    StandardDevZ  : Single ;
    MinZ : Integer ;
    MaxZ : Integer ;
    PlotAvailable : Boolean ;
    procedure UpdateHistogram ;
    procedure CopyImageToClipboard ;
    procedure CopyDataToClipboard ;
    procedure Print ;
  end;

var
  HistogramFrm: THistogramFrm;

implementation

uses RecUnit, Main, Maths, Math, Printgra, ViewUnit, FileIOUnit ,
     PlotSETAXES, ViewLineUnit;

{$R *.DFM}

procedure THistogramFrm.FormCreate(Sender: TObject);
// ------------------------------------
// Initialisations when form is created
// ------------------------------------
begin
     // Initialise empty internal frame buffer
     NumPixelsPerFrame := 0 ;
     PlotAvailable := False ;
     end ;


procedure THistogramFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
begin

     ClientHeight := HistGrp.Top + HistGrp.Height + 5 ;

     // Re-size controls appropriate to size of window
     Resize ;

     // Add readout cursor to plot
     plPlot.ClearVerticalCursors ;
     ReadoutCursor := plPlot.AddVerticalCursor( clBlue, '?r' ,0 ) ;

     FindImageSources ;
     Application.ProcessMessages ;
     NewSource ;
     Application.ProcessMessages ;

     UpdateHistogram ;

     AutoScaleXAxis := True ;
     FullScaleXAxis := False ;

     bFullRange.Enabled := False ;
     bAutoScale.Enabled := False ;

     for i := 0 to High(FrameCounter) do FrameCounter[i] := 0 ;

     DisplayHistogram ;

     end;


procedure THistogramFrm.FindImageSources ;
// --------------------------------------
// Discover which forms can supply images
// --------------------------------------
var
     i,j : Integer ;
     OldSource : String ;
     OldROI : String ;
begin

     // Keep old histogram source
     if plPlot.Available then begin
        OldSource := cbSource.Text ;
        OldROI := cbROI.Text ;
        end ;

     cbSource.Clear ;
     cbROI.Clear ;
     for i := 0 to Mainfrm.MDIChildCount-1 do begin
         if ANSIContainsText(Mainfrm.MDIChildren[i].Name,'RecordFrm') then begin
            if TRecordFrm(MainFrm.MDIChildren[i]).ImageAvailable then
               cbSource.Items.AddObject('Record Window ',TObject(srcRecordWindow)) ;
            end
         else if ANSIContainsText(Mainfrm.MDIChildren[i].Name,'SnapFrm') then begin
            if TSnapFrm(MainFrm.MDIChildren[i]).ImageAvailable then
               cbSource.Items.AddObject('Live Window ',TObject(srcLiveWindow)) ;
            end
         else if ANSIContainsText(Mainfrm.MDIChildren[i].Name,'ViewFrm') then begin
            if TViewFrm(MainFrm.MDIChildren[i]).ImageAvailable then begin
               cbSource.Items.AddObject('Images Window ',TObject(srcImagesWindow)) ;
               // Create list of ROIs
               for j := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[j].InUse then
                   cbROI.Items.AddObject( format('ROI.%d',[j]),TObject(j)) ;
               end ;
            end
         else if ANSIContainsText(Mainfrm.MDIChildren[i].Name,'ViewLineFrm') then begin
            if TViewLineFrm(MainFrm.MDIChildren[i]).ImageAvailable then begin
               cbSource.Items.AddObject('Line Scans ',TObject(srcLinesWindow)) ;
               cbROI.Items.AddObject( 'Fluor',TObject(1)) ;
               end ;
            end ;
         end ;

     cbSource.ItemIndex := 0 ;
     for i := 0 to cbSource.Items.Count-1 do begin
         if OldSource = cbSource.Items[i] then cbSource.ItemIndex := i ;
         end ;

     cbROI.ItemIndex := 0 ;
     for i := 0 to cbROI.Items.Count-1 do begin
         if OldROI = cbROI.Items[i] then cbROI.ItemIndex := i ;
         end ;
     end ;


procedure THistogramFrm.bDoHistogramClick(Sender: TObject);
// ---------------------------------
// Compute image intensity histogram
// ---------------------------------
begin
    UpdateHistogram ;
    end ;


procedure THistogramFrm.UpdateHistogram ;
// ---------------------------------
// Update image intensity histogram
// ---------------------------------
var
    i,iStart,j,z,MaxCount,OldNumPixelsPerFrame : Integer ;
    xPix,yPix,ix : Integer ;
    Sum : Single ;
    ImageSource : Pointer ;
    ROI : TROI ;
    FrameWidth : Integer ;
    Asq, BSq, r : Single ;
    NumPixels : Integer ;
    XStart, XEnd : Single ; // X limits of line
    YStart, YEnd : single ; // Y limits of line
    XStep, YStep : Single ;
    X, Y : Single ; // line coordinates
    Slope : Single ; // Slope of line
    D : Single ;    // Distance along line
    DEnd : Single ; // Length of line
    iPixel : Integer ;      // Pixel coord.

    LeftEdge : Integer ;  // Rectangular bounds of current ROI
    RightEdge : Integer ;
    TopEdge : Integer ;
    BottomEdge : Integer ;
    iSource : Integer ;
    ImageAvailable : Boolean ;
begin

    ImageSource := Nil ;

    if cbSource.Items.Count < 1 then exit ;
    if cbSource.ItemIndex < 0 then exit ;

    // Clear histogram bins
    for z := Low(Histogram) to High(Histogram) do Histogram[z] := 0 ;

    iSource := Integer(cbSource.Items.Objects[cbSource.ItemIndex]) ;
    case iSource of

        srcRecordWindow : begin
          ImageAvailable := RecordFrm.ImageAvailable ;
          ImageSource := RecordFrm.PDisplayBufs[cbFrameType.ItemIndex] ;
          FrameWidth := RecordFrm.FrameWidth ;
          GreyMax := MainFrm.Cam1.GreyLevelMax ;
          NumBytesPerPixel := RecordFrm.NumBytesPerPixel ;
          NumPixelsPerFrame := RecordFrm.NumPixelsPerFrame ;
          iStart := 0 ;
          end ;

       srcLiveWindow : begin
          ImageAvailable := SnapFrm.ImageAvailable ;
          ImageSource := SnapFrm.PDisplayBuf ;
          FrameWidth := SnapFrm.FrameWidth ;
          GreyMax := MainFrm.Cam1.GreyLevelMax ;
          NumBytesPerPixel := SnapFrm.NumBytesPerPixel ;
          NumPixelsPerFrame := SnapFrm.NumPixelsPerFrame ;
          iStart := 0 ;
          end ;

       srcImagesWindow : begin
          ImageAvailable := ViewFrm.ImageAvailable ;
          ImageSource := ViewFrm.PImageBufs[cbFrameType.ItemIndex] ;
          FrameWidth := ViewFrm.FrameWidth ;
          NumBytesPerPixel := ViewFrm.NumBytesPerPixel ;
          NumPixelsPerFrame := ViewFrm.NumPixelsPerFrame ;
          GreyMax := MainFrm.IDRFile.GreyMax ;
          iStart := 0 ;
          end ;

       srcLinesWindow : begin
          ImageAvailable := ViewLineFrm.ImageAvailable ;
          ImageSource := ViewLineFrm.PLineScanBuf ;
          FrameWidth := ViewLineFrm.FrameWidth ;
          NumBytesPerPixel := ViewLineFrm.NumBytesPerPixel ;
          NumPixelsPerFrame := ViewLineFrm.NumPixelsPerFrame ;
          GreyMax := Mainfrm.IDRFile.GreyMax ;
          iStart := 0 ;
          end ;

       end ;

    if not ImageAvailable then Exit ;
    if ImageSource = Nil then Exit ;

    if iSource = srcLinesWindow then begin
       // Line scan images
       if MainFrm.IDRFile.NumBytesPerPixel > 1 then begin
          // 16 bit pixels
          for i := 0 to NumPixelsPerFrame-1 do begin
              z := (PWordArray(ImageSource)^[i]) div BinWidth ;
              z := Max(Min(z,High(Histogram)),Low(Histogram)) ;
              Inc(Histogram[z]) ;
              end ;
          end
       else begin
          // 8 bit pixels
          for i := 0 to NumPixelsPerFrame-1 do begin
              z := PByteArray(ImageSource)^[i] div BinWidth ;
              z := Max(Min(z,High(Histogram)),Low(Histogram)) ;
              Inc(Histogram[z]) ;
              end ;
          end ;
       end
    else if rbWholeImage.Checked then begin
       // Compile whole image histogram
       for i := 0 to NumPixelsPerFrame-1 do begin
           z := PIntArray(ImageSource)^[i] div BinWidth ;
           z := Max(Min(z,High(Histogram)),Low(Histogram)) ;
           Inc(Histogram[z]) ;
           end ;
       end
    else begin
       // Histogram of region of interest
       ROI := MainFrm.IDRFile.ROI[Integer(cbROI.Items.Objects[cbROI.ItemIndex])] ;
       LeftEdge := MinInt([ROI.TopLeft.X,ROI.BottomRight.X]) ;
       RightEdge := MaxInt([ROI.TopLeft.X,ROI.BottomRight.X]) ;
       TopEdge := MinInt([ROI.TopLeft.Y,ROI.BottomRight.y]) ;
       BottomEdge := MaxInt([ROI.TopLeft.Y,ROI.BottomRight.y]) ;

       case ROI.Shape of
             // Rectangular region of interest
            RectangleROI : begin
                for xPix := LeftEdge to RightEdge do
                    for yPix := TopEdge to BottomEdge do begin
                       i := xPix + FrameWidth*yPix ;
                       ix := Round(PIntArray(ImageSource)^[i]) div BinWidth ;
                       ix := Max(Min(ix,High(Histogram)),Low(Histogram)) ;
                       Histogram[ix] := Histogram[ix] + 1 ;
                       end ;
                end ;

            // Elliptical region of interest
            EllipseROI : begin
                aSq := (LeftEdge - ROI.Centre.x)*(LeftEdge - ROI.Centre.x) ;
                bSq := (ROI.TopLeft.y - ROI.Centre.y)*(ROI.TopLeft.y - ROI.Centre.y) ;
                for xPix := LeftEdge to RightEdge do
                    for yPix := TopEdge to BottomEdge do begin
                        r := ((xPix-ROI.Centre.x)*(xPix-ROI.Centre.x)/aSq) +
                             ((yPix-ROI.Centre.y)*(yPix-ROI.Centre.y)/bSq) ;
                        if  r <= 1.0 then begin
                            i := yPix*FrameWidth + xPix ;
                           ix := Round(PIntArray(ImageSource)^[i]) div BinWidth ;
                           ix := Max(Min(ix,High(Histogram)),Low(Histogram)) ;
                           Histogram[ix] := Histogram[ix] + 1 ;
                           end ;
                        end ;
                end ;

            LineROI : Begin

                XStart := ROI.TopLeft.X ;
                YStart := ROI.TopLeft.Y ;
                XEnd := ROI.BottomRight.X ;
                YEnd := ROI.BottomRight.Y ;

                // Plot line
                Slope := (YEnd - YStart)/(XEnd - XStart) ;
                X := XStart ;
                Y := YStart ;
                XStep := Sign(XEnd - XStart) ;
                YStep := Sign(YEnd - YStart) ;
                DEnd :=  Sqrt( (XEnd - XStart)*(XEnd - XStart)
                         + (YEnd - YStart)*(YEnd - YStart)) ;
                Repeat
                  // Distance along line profile
                  D :=  Sqrt((X - XStart)*(X - XStart) + (Y - YStart)*(Y - YStart)) ;
                  // Nearest pixel
                  iPixel := Round(X) + Round(Y)*MainFrm.IDRFile.FrameWidth ;
                  ix := Round(PIntArray(ImageSource)^[iPixel]) div BinWidth ;
                  ix := Max(Min(ix,High(Histogram)),Low(Histogram)) ;
                  Histogram[ix] := Histogram[ix] + 1 ;
                  // Increment to next pixel on line
                  if Abs(Slope) < 1.0 then begin
                      X := X + XStep ;
                      Y := (X - XStart)*Slope + YStart ;
                      end
                  else begin
                      Y := Y + YStep ;
                      X := (Y - YStart)/Slope + XStart ;
                      end ;
                  Until Round(D) = Round(DEnd) ;
                end ;

            end ;
       end ;

    // Calculate and display image statistics

    meStatistics.Clear ;
    // Mean intensity
    Sum := 0.0 ;
    NumPixels := 0 ;
    for z := Low(Histogram) to High(Histogram) do begin
        Sum := Sum + z*Histogram[z] ;
        NumPixels := NumPixels + Histogram[z] ;
        end ;
    if NumPixels < 2 then Exit ;

    MeanZ := Sum / NumPixels ;
    meStatistics.Lines.Add( format(' Mean = %.6g',[MeanZ*BinWidth])) ;

    // Intensity standard deviation
    Sum := 0.0 ;
    for z := Low(Histogram) to High(Histogram) do
        Sum := Sum + (z-MeanZ)*(z-MeanZ)*Histogram[z] ;
    StandardDevZ := Sqrt ( Sum /(NumPixels-1) ) ;
    meStatistics.Lines.Add( format(' S.D. = %.6g',[StandardDevZ*BinWidth])) ;

    // Minimum intensity
    MinZ := Low(Histogram) ;
    while (Histogram[MinZ] <= 0) and
          (MinZ < High(Histogram)) do Inc(MinZ) ;
    meStatistics.Lines.Add( format(' Min. = %d',[MinZ*BinWidth])) ;

    // Maximum intensity
    MaxZ := High(Histogram) ;
    while (Histogram[MaxZ] <= 0) and
          (MaxZ > Low(Histogram)) do Dec(MaxZ) ;
    meStatistics.Lines.Add( format(' Max. = %d',[MaxZ*BinWidth])) ;

    // Peak histogram count
    MaxCount := 0 ;
    for z := Low(Histogram) to High(Histogram) do
        if MaxCount < Histogram[z] then MaxCount := Histogram[z];

    // Display histogram
    DisplayHistogram ;

    bFullRange.Enabled := True ;
    bAutoScale.Enabled := True ;

    end;


procedure THistogramFrm.NewSource ;
// -------------------------------------------------
// Update internal settings when new source selected
// -------------------------------------------------
var
    i,OldNumPixelsPerFrame : Integer ;
    iSource : Integer ;
begin

    // Copy image from source to histogram buffer
    OldNumPixelsPerFrame := NumPixelsPerFrame ;

    iSource := Integer(cbSource.Items.Objects[cbSource.ItemIndex]) ;
    case iSource of
       srcRecordWindow : begin
          // Image recording module
          NumBytesPerPixel := RecordFrm.NumBytesPerPixel ;
          NumPixelsPerFrame := RecordFrm.NumPixelsPerFrame ;
          GreyMax := MainFrm.Cam1.GreyLevelMax ;
          cbFrameType.Clear ;
          for i := 0 to RecordFrm.NumFrameTypes-1 do begin
               cbFrameType.Items.Add(RecordFrm.FrameTypes[i]) ;
               end ;
          cbFrameType.ItemIndex := 0 ;
          rbWholeImage.Checked := True ;
          ROIGrp.Enabled := False ;
          end ;
       srcLiveWindow : begin
          // Live image recording module
          NumBytesPerPixel := SnapFrm.NumBytesPerPixel ;
          NumPixelsPerFrame := SnapFrm.NumPixelsPerFrame ;
          GreyMax := MainFrm.Cam1.GreyLevelMax ;
          cbFrameType.Clear ;
          cbFrameType.Items.Add(' ') ;
          cbFrameType.ItemIndex := 0 ;
          rbWholeImage.Checked := True ;
          ROIGrp.Enabled := False ;
          end ;

       srcImagesWindow : begin
           // Image display module
          NumBytesPerPixel := ViewFrm.NumBytesPerPixel ;
          NumPixelsPerFrame := ViewFrm.NumPixelsPerFrame ;
          GreyMax := MainFrm.IDRFile.GreyMax ;
          cbFrameType.Clear ;
          for i := 0 to ViewFrm.NumFrameTypes-1 do begin
             cbFrameType.Items.Add(ViewFrm.FrameTypes[i]) ;
             end ;
          cbFrameType.ItemIndex := 0 ;
          ROIGrp.Enabled := True ;
          end ;

        srcLinesWindow : begin
           // Line scan display module
          NumBytesPerPixel := ViewLineFrm.NumBytesPerPixel ;
          NumPixelsPerFrame := ViewLineFrm.NumPixelsPerFrame ;
          GreyMax := MainFrm.IDRFile.GreyMax ;
          cbFrameType.Clear ;
          cbFrameType.Items.Add('Line') ;
          cbFrameType.ItemIndex := 0 ;
          ROIGrp.Enabled := False ;
          end ;

        end ;

    // Set bin width
    BinWidth := Max(GreyMax div MaxBins,1) ;

    // Set histogram range to intensity range within image
    bAutoScale.Click ;

    end ;


procedure THistogramFrm.DisplayHistogram ;
// ------------------------
// Update histogram display
// ------------------------
var
    z,PlotNum,HistNum : Integer ;
    ZStart,ZEnd : Integer ;
    xLo,xMid,xHi,y,Temp : Single ;
    HalfBinWidth : Single ;
begin

    { New Y Axis on plot }
    plPlot.MaxPointsPerLine := GreyMax*2 ;

    // Clear data points line }
    plPlot.ClearAllLines ;
//    plPlot.CreateLine( 0 , clBlue, msNone, psSolid ) ;

    // Set up X axis

    if AutoScaleXAxis then begin
       plPlot.xAxisMin := (MinZ-1)*BinWidth ;
       plPlot.xAxisMax := (MaxZ+1)*BinWidth ;
       plPlot.xAxisTick := (plPlot.xAxisMax - plPlot.xAxisMin) / 5.0 ;
       end
    else if FullScaleXAxis then begin
       plPlot.xAxisMin := 0 ;
       plPlot.xAxisMax := GreyMax ;
       plPlot.xAxisTick := (plPlot.xAxisMax - plPlot.xAxisMin) / 5.0 ;
       end ;

    plPlot.xAxisAutoRange := False ;

    plPlot.xAxisLabel := 'Pixel Intensity' ;

    // Set up Y axis
    plPlot.yAxisMin := 0.0 ;
    plPlot.yAxisAutoRange := True ;
    plPlot.yAxisLabel := 'No. Pixels' ;

    plPlot.CreateHistogram(0) ;

    // Plot histogram

    HalfBinWidth := 0.5*BinWidth ;
    ZStart :=  Min(Max(Round(plPlot.xAxisMin/BinWidth),Low(Histogram)),High(Histogram))  ;
    ZEnd :=  Min(Max(Round(plPlot.xAxisMax/BinWidth),Low(Histogram)),High(Histogram)) ;
    for z := ZStart to ZEnd do begin
        xMid := z*BinWidth ;
        xLo := z*BinWidth - HalfBinWidth ;
        xHi := z*BiNWidth + HalfBinWidth ;
        y := Histogram[z] ;
        plPlot.AddBin( 0,xLo,xMid,xHi,y ) ;
        end ;

    if (plPlot.VerticalCursors[ReadoutCursor] < Round(plPlot.xAxisMin)) or
       (plPlot.VerticalCursors[ReadoutCursor] > Round(plPlot.xAxisMax)) then begin
       plPlot.VerticalCursors[ReadoutCursor] := (Round(plPlot.xAxisMax) + Round(plPlot.xAxisMin)) div 2 ;
       end ;

    PlotAvailable := True ;

    end ;


procedure THistogramFrm.bFullRangeClick(Sender: TObject);
// --------------------------------------------------------
// Re-display histogram over full range of intensity values
// --------------------------------------------------------
begin
     bFullRange.Font.Style := [fsUnderline,fsbold] ;
     bAutoScale.Font.Style := [fsbold] ;
     bSetAxes.Font.Style := [fsbold] ;
     if plPlot.Available then begin
        AutoScaleXAxis := False ;
        FullScaleXAxis := True ;
        DisplayHistogram ;
        end ;
     end;


procedure THistogramFrm.bAutoScaleClick(Sender: TObject);
// ------------------------------------------------------------
// Re-display histogram over range of non-zero intensity values
// ------------------------------------------------------------
begin
     bFullRange.Font.Style := [fsbold] ;
     bAutoScale.Font.Style := [fsUnderline,fsbold] ;
     bSetAxes.Font.Style := [fsbold] ;

     if plPlot.Available then begin
        AutoScaleXAxis := True ;
        FullScaleXAxis := False ;
        DisplayHistogram ;
        end ;
     end;


procedure THistogramFrm.FormResize(Sender: TObject);
// ----------------------------------------
// Re-size control when window size changes
// ----------------------------------------
begin
     HistGrp.Height := ClientHeight - HistGrp.Top - 5 ;
     plPlot.Width := Max(ClientWidth - plPlot.Left - 5,2) ;

     plPlot.Height := Max(ClientHeight - plPlot.Top - 5,2) ;
     end;


procedure THistogramFrm.CopyDataToClipBoard ;
{ ---------------------------------------------
  Copy the intensity histogram to the clipboard
  ---------------------------------------------
  }
begin
     plPlot.CopyDataToClipboard ;
     end ;


procedure THistogramFrm.Print ;
{ ----------------
  Print histogram
  --------------- }
begin
     PrintGraphFrm.MultiYPlot := False ;
     PrintGraphFrm.Plot := plPlot ;
     PrintGraphFrm.ToPrinter := True ;
     PrintGraphFrm.ShowModal ;
     if PrintGraphFrm.ModalResult = mrOK then begin
        { Add title information to plot }
        plPlot.ClearPrinterTitle ;
        plPlot.AddPrinterTitleLine( ' File : ' + MainFrm.IDRFile.FileName ) ;
        plPlot.AddPrinterTitleLine( ' ' + MainFrm.IDRFile.Ident ) ;
        { Plot graph to printer }
        plPlot.Print ;
        end ;
     end ;


procedure THistogramFrm.CopyImageToClipboard ;
{ -----------------------------------------------------
  Copy histogram image to clipboard as Windows metafile
  ----------------------------------------------------- }
begin
     PrintGraphFrm.MultiYPlot := False ;
     PrintGraphFrm.Plot := plPlot ;
     PrintGraphFrm.ToPrinter := False ;
     PrintGraphFrm.ShowModal ;
     if PrintGraphFrm.ModalResult = mrOK then plPlot.CopyImageToClipboard ;
     end ;


procedure THistogramFrm.TimerTimer(Sender: TObject);
// ----------------------
// Scheduled event timer
// ----------------------
var
    iSource : Integer ;
begin

     // Check to see if any images source have appeared or disappeared
     FindImageSources ;


     if cbSource.Items.Count < 0 then Exit ;
     if cbSource.ItemIndex < 0 then Exit ;

     if cbSource.Items.Count > 0 then bSetAxes.Enabled := True
                                 else bSetAxes.Enabled := False ;

    iSource := Integer(cbSource.Items.Objects[cbSource.ItemIndex]) ;
    case iSource of

        srcRecordWindow : begin
           if RecordFrm.FrameTypeCounter[cbFrameType.ItemIndex]
              <> FrameCounter[cbFrameType.ItemIndex] then begin
              UpdateHistogram ;
              FrameCounter[cbFrameType.ItemIndex] := RecordFrm.FrameTypeCounter[cbFrameType.ItemIndex] ;
              end ;
           end ;

        srcLiveWindow : begin
           UpdateHistogram ;
           end ;

        srcImagesWindow : begin
           if ViewFrm.FrameCounter[cbFrameType.ItemIndex]
              <> FrameCounter[cbFrameType.ItemIndex] then begin
              UpdateHistogram ;
              FrameCounter[cbFrameType.ItemIndex] := ViewFrm.FrameCounter[cbFrameType.ItemIndex] ;
              end ;
           end ;

        srcLinesWindow : begin
           if ViewLineFrm.FrameCounter <> FrameCounter[0] then begin
              UpdateHistogram ;
              FrameCounter[0] := ViewLineFrm.FrameCounter ;
              end ;
           end ;

        end ;

     end;


procedure THistogramFrm.cbSourceChange(Sender: TObject);
begin
     NewSource ;
     end;


procedure THistogramFrm.bSetAxesClick(Sender: TObject);
{ -----------------------------
  Customise Histogram plot axes
  -----------------------------}
begin

     bFullRange.Font.Style := [fsbold] ;
     bAutoScale.Font.Style := [fsbold] ;
     bSetAxes.Font.Style := [fsUnderline,fsbold] ;

     PlotSetAxesFrm.Plot := plPlot ;
     PlotSetAxesFrm.ShowModal ;

     AutoScaleXAxis := False ;
     FullScaleXAxis := False ;

     end;


procedure THistogramFrm.rbWholeImageClick(Sender: TObject);
begin
    UpdateHistogram ;
    end ;

procedure THistogramFrm.cbROIChange(Sender: TObject);
begin
    UpdateHistogram ;
    end ;

procedure THistogramFrm.rbROIClick(Sender: TObject);
begin
    UpdateHistogram ;
    end ;

procedure THistogramFrm.cbFrameTypeChange(Sender: TObject);
begin
    UpdateHistogram ;
    bAutoScale.Click ;
    end ;


procedure THistogramFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
begin

     // Request destruction of form object
     Action := caFree ;
     end;




end.
