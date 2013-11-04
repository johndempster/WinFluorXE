unit TimeCourseUnit;
// =============================================================================
// WinFluor - Time course analysis module
// (c) J. Dempster, University of Strathclyde, 2001-2003, All Rights Reserved
// =============================================================================
// 26.4.2002
// 2.9.2002 Computed ion concentration added
// 8.7.2003 Lines can be added to existing plots
// 22.7.03  Lines on same plot now drawn in different colours
// 29.7.03  Frames now loaded into 32 bit buffer
// 3.8.03  Line ROI added
// 11.8.03 2X errors in Ratio calculation corrected
// 22.10.03 Plots now uses min/max compression to limit lines to 3000 points
//          A/D channel, intensity and ratio plots now in separate procedure
// 6.11.03  Time can now be seconds or minutes
// 15.11.03 Frame times now defined as end of exposure time
// 04.03.04 Time axis for files with multiple frame types now correct
//          Intensity of Ratio images (produced by RatioUnit.pas) now plotted
//          as ratio rather than integer intensity
// 14.07.04 Pixel time courses from line scan images can now be plotted
//          ROI intensity now computed in ViewFrm or ViewLineFrm
// 21.03.05 Chart annotations now plotted on time course plots
// 04.05.06 Add All ROIs button added
// 05.08.08 Plots can be deleted individually
//          Line scans now correctly added to plots
// 10.09.09 ROI exclusion threshold removed
// 30.08.10 Fluorescence display type now selected using drop-down list
//          FP divide by zero error when Df/F0 plotted with F0=0 fixed.
// 25.08.12 Line is no longer truncated when more than 10000 frames/lines in plot
// 31.12.12 Fix issue with plotting different length fluorescence and ADC channels
//
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RangeEdit, XYPlotDisplay, ViewUnit, ExtCtrls, XMultiYPlot, FileIOUnit,
  HTMLLabel, ValidatedEdit, IDRFile, ComCtrls ;

const
     MaxPlotPoints = 10000 ;
     MaxLinesPerPlot = cMaxROIs ;
     MaxReadoutLines = 20 ;
     MaxPlots = 10 ;

type
  TPlotDescription = record
    InUse : Boolean ;
    Source : Integer ;
    Background : Integer ;
    end ;

  TTimeCourseFrm = class(TForm)
    ControlsGrp: TGroupBox;
    RangeGrp: TGroupBox;
    rbAllFrames: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    ROIGrp: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    cbSource: TComboBox;
    cbBackground: TComboBox;
    plPlot: TXMultiYPlot;
    TUnitsGrp: TGroupBox;
    rbSeconds: TRadioButton;
    rbMinutes: TRadioButton;
    ExcludePanel: TPanel;
    PlotGrp: TGroupBox;
    bAddLineToPlot: TButton;
    cbPlotNum: TComboBox;
    bAddAllROIs: TButton;
    bNewPlot: TButton;
    bClearPlots: TButton;
    cbClearPlot: TComboBox;
    bSetAxes: TButton;
    Shape1: TShape;
    FluorescenceDisplayGrp: TGroupBox;
    DisplayModePage: TPageControl;
    FTab: TTabSheet;
    Label3: TLabel;
    cbWavelength: TComboBox;
    dFTab: TTabSheet;
    Label6: TLabel;
    rbF0FromFrames: TRadioButton;
    edF0Range: TRangeEdit;
    rbF0Constant: TRadioButton;
    edF0Constant: TValidatedEdit;
    cbdFWavelength: TComboBox;
    GroupBox1: TGroupBox;
    rbDFOverF0: TRadioButton;
    rbFOverF0: TRadioButton;
    RatioTab: TTabSheet;
    Shape2: TShape;
    Label4: TLabel;
    Label5: TLabel;
    cbNumWave: TComboBox;
    cbDenomWave: TComboBox;
    edRatioExclusionThreshold: TValidatedEdit;
    GroupBox8: TGroupBox;
    edRatioRMax: TValidatedEdit;
    cbEquation: TComboBox;
    ckUseEquation: TCheckBox;
    ckDivideByRMax: TCheckBox;
    cbFluorescence: TComboBox;
    procedure bNewPlotClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure bClearPlotsClick(Sender: TObject);
    procedure bSetAxesClick(Sender: TObject);
    procedure bAddLineToPlotClick(Sender: TObject);
    procedure bAddAllROIsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edPixelExclusionThresholdKeyPress(Sender: TObject;
      var Key: Char);
    procedure ckExcludePixelsClick(Sender: TObject);
    procedure ckDivideByRMaxClick(Sender: TObject);
    procedure ckUseEquationClick(Sender: TObject);
    procedure cbFluorescenceChange(Sender: TObject);
    procedure cbSourceChange(Sender: TObject);
  private
    { Private declarations }
    Plots : Array[0..15] of TPlotDescription ;  // Plot description records
    LineColors : Array[0..MaxLinesPerPlot-1] of TColor ;       // Line colours
    ColorSequence : Array[0..9] of TColor ;     // Standard line colour sequence
    ReadoutCursor : Integer ;                   // Plot readout cursor number
    LineName : Array[0..MaxLinesPerPlot-1] of String ;         // Names of lines in plot
    LineUnits : Array[0..MaxLinesPerPlot-1] of String ;        // Units of data values in lines
    TScale : Single ;                           // Time units scaling factor
    TUnits : String ;                           // Time units
    PlotStartFrame : Integer ;                  // Starting frame of plot on display
    TInterval : Single ;                        // Inter-Frame /-Line time interval (s)
    StopPlot : Boolean ;

    procedure PlotLine ;


    procedure PlotROIIntensity(
              StartAtFrame : Integer ;
              EndAtFrame : Integer ;
              PlotNum : Integer ;
              LineNum : Integer
              ) ;

    procedure PlotLineScanIntensity(
              StartAtLine : Integer ;       // Start at frame #
              EndAtLine : Integer ;         // End at frame #
              PlotNum : Integer ;            // Plot on plot #
              LineNum : Integer              // Plot as line #
              ) ;

    procedure PlotADCChannel(
              StartAtFrame : Integer ;
              EndAtFrame : Integer ;
              PlotNum : Integer ;
              LineNum : Integer
              ) ;

    procedure FillPlotLists ;              

  public
    { Public declarations }
    PlotAvailable : Boolean ;
    procedure UpdateSettings ;
    procedure CopyImageToClipboard ;
    procedure CopyDataToClipboard ;
    procedure Print ;

  end;

var
  TimeCourseFrm: TTimeCourseFrm;

implementation

{$R *.DFM}

uses Main, Maths, Printgra, Setaxes, Math , ViewLineUnit, ViewPlotUnit;


procedure TTimeCourseFrm.FormShow(Sender: TObject);
// --------------------------------------------
// Initialisations when form is first displayed
// --------------------------------------------
var
     i : Integer ;
begin

     // Clear plots
     for i := 0 to High(Plots) do Plots[i].InUse := False ;

     // Create list of event detection channels
     cbFluorescence.Clear ;
     // Add fluorescence options
     cbFluorescence.Items.Add('Fluorescence') ;
     cbFluorescence.Items.Add('Fluorescence ratio') ;
     cbFluorescence.Items.Add('Fluorescence dF/F0') ;
     cbFluorescence.ItemIndex := 0 ;
     DisplayModePage.ActivePage := FTab ;

     if MainFrm.IDRFile.LineScan then begin
        // Line scan data file
        edRange.LoLimit := 1 ;
        edRange.HiLimit := MainFrm.IDRFile.FrameHeight ;
        TInterval := MainFrm.IDRFile.FrameInterval/MainFrm.IDRFile.FrameHeight ;
        ExcludePanel.Visible := False ;
        bAddAllROIs.Enabled := False ;
        end
     else begin
        // Image series data file
        edRange.LoLimit := 1 ;
        edRange.HiLimit := MainFrm.IDRFile.NumFrames ;
        TInterval := MainFrm.IDRFile.FrameInterval ;
        ExcludePanel.Visible := True ;
        bAddAllROIs.Enabled := True ;
        end ;
     edRange.LoValue := edRange.LoLimit ;
     edRange.HiValue := edRange.HiLimit ;

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

     // Update region of interest lists
     UpdateSettings ;

     // Add readout cursor to plot
     plPlot.ClearVerticalCursors ;
     ReadoutCursor := plPlot.AddVerticalCursor( clGreen, '?ri' ) ;

     plPlot.ShowMarkers := False ;
     plPlot.ShowLines := True ;

     ClientHeight := ControlsGrp.Top + ControlsGrp.Height + 5 ;

     Resize ;

     // No plots available for display
     PlotAvailable := False ;
     bSetAxes.Enabled := False ;
     bAddLineToPlot.Enabled := False ;
     DisplayModePage.ActivePage := FTab  ;

     cbClearPlot.Clear ;
     cbClearPlot.Items.AddObject('All',TObject(MaxPlots)) ;

     FluorescenceDisplayGrp.Visible := True ;

     end;


procedure TTimeCourseFrm.bNewPlotClick(Sender: TObject);
// ---------------------------------
// Add a new Y Axis and line to plot
// ---------------------------------
var
     PlotNum : Integer ;
     i : Integer ;
begin

      // Prevent plotting if time course is being calculated
      if MainFrm.FormExists( 'ViewPlotFrm' ) then begin
         if not ViewPlotFrm.FLTimeCourseAvailable then Exit ;
         end
      else if not MainFrm.FormExists( 'ViewLineFrm' ) then Exit ;

      // Disable button while line is plotted
      bNewPlot.Enabled := False ;
      ROIGrp.Enabled := False ;

     { Add new Y Axis to plot }
     PlotNum := plPlot.CreatePlot ;

     FillPlotLists ;

     // New line to plot
     PlotLine ;

     // Re-enable button
     bNewPlot.Enabled := True ;
     bAddLineToPlot.Enabled := True ;
     ROIGrp.Enabled := True ;

     end ;


procedure TTimeCourseFrm.PlotLine ;
// --------------------
// Add a line to a plot
// --------------------
var
     StartAtFrame,EndAtFrame : Integer ;
     PlotNum : Integer ;
     LineNum : Integer ;
     Col : TColor ;
     i : Integer ;
begin

     // Prevent aother plot operations while line is plotted
     PlotGrp.Enabled := False ;

     // Re-size readout label and plot
     plPlot.Height := ClientHeight - plPlot.Top - 5 ;

     // Ensure there is enough space allocated for line
     plPlot.MaxPointsPerLine := MaxPlotPoints*2 ;

     // Select range of frames to be plotted
     if rbAllFrames.Checked then begin
        StartAtFrame := Round(edRange.LoLimit) ;
        EndAtFrame := Round(edRange.HiLimit) ;
        end
     else begin
        StartAtFrame := Round(edRange.LoValue) ;
        EndAtFrame := Round(edRange.HiValue ) ;
        end ;
     PlotStartFrame := StartAtFrame ;

     // Seconds or minutes time units
     if rbSeconds.Checked then begin
        TScale := 1.0 ;
        TUnits := 's' ;
        end
     else begin
        TScale := 1.0 / 60.0 ;
        TUnits := 'min' ;
        end ;

     // Add`new line to plot
     Col := ColorSequence[plPlot.NumLinesInPlot[plPlot.PlotNum]] ;
     LineNum := plPlot.CreateLine( Col,
                                   msOpenSquare,
                                   psSolid ) ;
     LineColors[LineNum] := Col ;

     { Plot graph of currently selected variables }
     plPlot.xAxisAutoRange := False ;
     plPlot.XAxisMin := 0.0 ;
     plPlot.XAxisMax := (EndAtFrame-StartAtFrame)*TInterval*TScale ;
     plPlot.XAxisTick := (plPlot.XAxisMax - plPlot.XAxisMin) / 5.0 ;
     plPlot.yAxisAutoRange := True ;

     { Create X and Y axes labels }
     plPlot.xAxisLabel := '(' + TUnits + ')' ;

     if Integer(cbSource.Items.Objects[cbSource.ItemIndex])
        <= MainFrm.IDRFile.MaxROI then begin

        // Plot a region of interest within image
        // --------------------------------------
        if MainFrm.IDRFile.LineScan then begin
           PlotLineScanIntensity( StartAtFrame,
                                  EndAtFrame,
                                  PlotNum,
                                  LineNum ) ;
           end
        else begin
           PlotROIIntensity( StartAtFrame,
                             EndAtFrame,
                             PlotNum,
                             LineNum ) ;
           end ;
        end
     else begin
        // Plot analogue signal channel
        PlotADCChannel( StartAtFrame,
                        EndAtFrame,
                        PlotNum,
                        LineNum ) ;
        end ;

     // Add annotations to plot
     plPlot.ClearAnnotations ;
     for i := 0 to MainFrm.IDRFile.NumMarkers-1 do
         plPlot.AddAnnotation( MainFrm.IDRFile.MarkerTime[i],
                               MainFrm.IDRFile.MarkerText[i] ) ;

     plPlot.VerticalCursors[ReadoutCursor] := (plPlot.XAxisMax +  plPlot.XAxisMin) / 2.0 ;

     // Signal that a plot is available for copying/printing
     PlotAvailable := True ;
     bSetAxes.Enabled := True ;

     MainFrm.StatusBar.SimpleText :=
     format(' Time Course: Frames %d-%d plotted',
     [StartAtFrame,EndAtFrame]) ;

     // Re-enable plot operations
     PlotGrp.Enabled := True ;

     end;


procedure TTimeCourseFrm.PlotROIIntensity(
          StartAtFrame : Integer ;       // Start at frame #
          EndAtFrame : Integer ;         // End at frame #
          PlotNum : Integer ;            // Plot on plot #
          LineNum : Integer              // Plot as line #
          ) ;
// -------------------------------------------------------------
// Plot intensity ratio for of a region of interest within image
// -------------------------------------------------------------
const
    FMode = 0 ;
    DFMode = 1 ;
    RatioMode = 2 ;
var
     Frame,i,iROI,iBackg,j : Integer ;
     iChan : Integer ;
     z, t, dt, y : Single ;
     tStep : Single ;
     //zExclusionThresholdOffset : Integer ; // Measurement exclusion intensity level
     zBackg : Single ;                    // Background subtraction intensity level
     yNum : Single ;
     yDen : Single ;
     Ratio, tRatio : Single ;     // Frame ratio

     ReportInterval : Integer ;
     Concentration : Single ;             // Computed ion concentration
     RMin : Single ;                      // Minimum ratio
     RMax : Single ;                      // Maximum ratio
     KEff : Single ;                      // Binding coefficient

    // Compression block variables
    BlockCount : Integer ;           // No. of scans in block index
    NumFramesPerBlock : Integer ;     // No. image frame ratios in compression block
    NumPointsPerBlock : Integer ;    // No. of displayed points per compression block
    yMin : Single ;                  // Min. intensity value within block
    yMax : Single ;                  // Max. intensity value within block
    yMaxAt : Integer ;               // Blockcount index # of max intensity
    yMinAt : Integer ;               // Blockcount index of min intensity
    yThreshold : Single ;
    FrameTypeNum,FrameTypeDen : Integer ;

    NumPoints : Integer ;            // No. of points in plot

    yDenAvailable : Boolean ;
    yNumAvailable : Boolean ;

    F0 : Single ;
    F0Offset : Single ;
     Sum : Single ;
     nAvg : Integer ;
     StartF0, EndF0 : Integer ;

     OK : Boolean ;
     FrameOffset : Integer ;
     Col : TColor ;
     NewHeight : Integer ;
     Mode : Integer ;
begin

    // Get source and background region of interest selections
    iROI := Integer(cbSource.Items.Objects[cbSource.ItemIndex]) ;
    iBackg := Integer(cbBackground.Items.Objects[cbBackground.ItemIndex]) ;
    LineName[LineNum] := format('R.%d',[iROI]) ;
    if iBackg <= MainFrm.IDRFile.MaxROI then
       LineName[LineNum] := LineName[LineNum] + format('-R.%d',[iBackg]) ;

    if DisplayModePage.ActivePage = FTab then begin
       // Raw fluorescence plot
       FrameTypeNum := cbWavelength.ItemIndex ;
       plPlot.yAxisLabel := cbWavelength.Text ;
       yThreshold := 0.0 ;
       Mode := FMode ;
       end
    else if DisplayModePage.ActivePage = DFTab then begin
       // Relative fluorescence change plot
       FrameTypeNum := cbDFWavelength.ItemIndex ;
       if rbdFOverF0.Checked then begin
          plPlot.yAxisLabel := 'dF/F0' + '(' + cbWavelength.Text + ')' ;
          F0Offset := 1.0 ;
          end
       else begin
          plPlot.yAxisLabel := 'F/F0' + '(' + cbWavelength.Text + ')' ;
          F0Offset := 0.0 ;
          end ;
       yThreshold := 0.0 ;
       if rbF0Constant.Checked then begin
          // Set F0 to constant value
          F0 := edF0Constant.Value ;
          end
       else begin
          // Compute F0 from average intensity within range of frames
          Sum := 0.0 ;
          nAvg := 0 ;
          StartF0 := Round(edF0Range.LoValue) ;
          EndF0 := Min(Max(Round(edF0Range.HiValue),
                               StartF0 + MainFrm.IDRFile.NumFrameTypes - 1),
                               EndAtFrame) ;
          for Frame := StartF0 to EndF0 do begin

              // Compute background subtraction ROI (if available)
              if (iBackg >= 0) and (iBackg <= MainFrm.IDRFile.MaxROI) then begin
                 zBackg := ViewPlotFrm.ROIIntensity(iBackg,Frame,FrameTypeNum) ;
                 end
              else zBackg := 0 ;

              // Mean intensity within ROI
              Sum := Sum +
                     (ViewPlotFrm.ROIIntensity(iROI,Frame,FrameTypeNum)
                     - zBackg) ;

              Inc(nAvg) ;

              end ;
          if nAvg > 0 then F0 := Round( Sum / nAvg )
                      else F0 := 0 ;
          end ;

       Mode := DFMode ;
       end
    else begin
       // Fluorescence ratio plot
       FrameTypeNum := cbNumWave.ItemIndex ;
       FrameTypeDen := cbDenomWave.ItemIndex ;
       Mode := RatioMode ;

       if ckUseEquation.Checked then begin
          // Plot computed ion concentration
          LineUnits[LineNum] := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Units ;
          plPlot.yAxisLabel := format( '[%s] %s (%s)',
                               [MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Ion,
                                MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Units,
                                cbSource.Text]) ;
          RMax := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].RMax ;
          RMin := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].RMin ;
          KEff := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Keff ;
          end
       else begin
          // Plot ratio
          LineUnits[LineNum] := '' ;
          plPlot.yAxisLabel := format( '%s/%s',
                                       [cbNumWave.Text,
                                       cbDenomWave.Text]) ;
          if ckDivideByRMax.Checked then begin
             plPlot.yAxisLabel := plPlot.yAxisLabel + '/RMax' ;
             end ;
          end ;
       end ;

    // Size of plot compression block
    NumFramesPerBlock := (EndAtFrame - StartAtFrame + 1) div MaxPlotPoints ;
    NumFramesPerBlock := Max(NumFramesPerBlock div MainFrm.IDRFile.NumFrameTypes,1)*MainFrm.IDRFile.NumFrameTypes ;
    // No. of display points per compression block
    NumPointsPerBlock := Min(NumFramesPerBlock div MainFrm.IDRFile.NumFrameTypes,2) ;

    // Initialise counters
    BlockCount := NumFramesPerBlock ;
    NumPoints := 0 ;
    ReportInterval := Max((EndAtFrame - StartAtFrame) div 100,1);
    StopPlot := False ;

    yDenAvailable := False ;
    yNumAvailable := False ;
    yThreshold := Round(edRatioExclusionThreshold.Value) ;

    for Frame := StartAtFrame to EndAtFrame do begin

        // Time of acquisition
        t := (Frame-StartAtFrame)*MainFrm.IDRFile.FrameInterval ;

        // Compute background subtraction ROI (if available)
        if (iBackg >= 0) and (iBackg <= MainFrm.IDRFile.MaxROI) then begin
            zBackg := ViewPlotFrm.ROIIntensity( iBackg, Frame, FrameTypeNum ) ;
            end
        else zBackg := 0 ;

        // Get numerator

        yNum := ViewPlotFrm.ROIIntensity( iROI,
                                          Frame,
                                          FrameTypeNum ) - zBackg ;

        // Get denominator
        if Mode = RatioMode then begin
           yDen := ViewPlotFrm.ROIIntensity( iROI,
                                             Frame,
                                             FrameTypeDen )- zBackg ;
           end ;

        // Add to plot when numerator (and denominator in ratio mode) available
        if Mode = FMode then begin
           // Raw fluorescence
           //y := (yNum - MainFrm.IDRFile.IntensityOffset)*MainFrm.IDRFile.IntensityScale ;
           y := yNum ;
           end
        else if Mode = DFMode then begin
           // Relative change in fluorescence
           y := (yNum / F0) - F0Offset ;
           end
        else begin
           // Fluorescence ratio
           if yDen > yThreshold then Ratio := yNum / yDen
                                else Ratio := 0.0 ;
           if ckUseEquation.Checked then begin
              // Plot computed ion concentration
              if Ratio <= (RMax*0.99) then begin
                 y := ((Ratio - RMin)/(RMax - Ratio))*KEff ;
                 end ;
              end
           else begin
              // Plot ratio
              y := Ratio ;
              if ckDivideByRMax.Checked then y := y / edRatioRMax.Value ;
              end ;
           end ;

           // Initialise compression block
           if BlockCount >= NumFramesPerBlock then begin
              yMin := MainFrm.IDRFile.GreyMax ;
              yMax := -yMin -1 ;
              BlockCount := 0 ;
              end ;

           // Update min/max
           if y < yMin then begin
              yMin := y ;
              yMinAt := BlockCount ;
              end ;
           if y > yMax then begin
              yMax := y ;
              yMaxAt := BlockCount ;
              end ;

        Inc(BlockCount) ;

        // When block complete ... write min./max. to display buffer
        if BlockCount >= NumFramesPerBlock then begin

           // First point
           if yMaxAt <= yMinAt then plPlot.AddPoint(LineNum, t, yMax )
                               else plPlot.AddPoint(LineNum, t, yMin ) ;
           t := t + MainFrm.IDRFile.FrameInterval ;
           Inc(NumPoints) ;

           // Second point
           if BlockCount > MainFrm.IDRFile.NumFrameTypes then begin
              if yMaxAt >= yMinAt then plPlot.AddPoint(LineNum, t, yMax )
                                  else plPlot.AddPoint(LineNum, t, yMin ) ;
              Inc(NumPoints) ;
              end ;
           end ;

        // Report progress
        MainFrm.StatusBar.SimpleText :=
              format(' Time Course: Frame %d/%d',[Frame,EndAtFrame]) ;
        if ((Frame-StartAtFrame) mod ReportInterval) = 0 then Application.ProcessMessages ;

        if StopPlot then Break ;

        end ;

    LineName[LineNum] := LineName[LineNum] + ' ' + plPlot.yAxisLabel ;

    end ;


procedure TTimeCourseFrm.PlotLineScanIntensity(
          StartAtLine : Integer ;       // Start at frame #
          EndAtLine : Integer ;         // End at frame #
          PlotNum : Integer ;            // Plot on plot #
          LineNum : Integer              // Plot as line #
          ) ;
// -------------------------------------------------------------
// Plot intensity ratio for of a region of interest within image
// -------------------------------------------------------------
var
     i,j,L : Integer ;
     NumRatios : Integer ;
     iChan : Integer ;
     z, t, dt, y : Single ;
     tStep : Single ;
     yNumerator: Single ;
     yDenominator: Single ;
     Ratio : Single ;     // Frame ratio

     ReportInterval : Integer ;
     Concentration : Single ;             // Computed ion concentration
     RMin : Single ;                      // Minimum ratio
     RMax : Single ;                      // Maximum ratio
     KEff : Single ;                      // Binding coefficient

    // Compression block variables
    BlockCount : Integer ;           // No. of scans in block index
    NumLinesPerBlock : Integer ;     // No. image lines in compression block
    NumPointsPerBlock : Integer ;    // No. of displayed points per compression block
    yMin : Single ;                  // Min. intensity value within block
    yMax : Single ;                  // Max. intensity value within block
    yMaxAt : Integer ;               // Blockcount index # of max intensity
    yMinAt : Integer ;               // Blockcount index of min intensity
    yThreshold : Single ;

    NumPoints : Integer ;            // No. of points in plot
    AddPointToPlot : Boolean ;

     OK : Boolean ;
     FrameOffset : Integer ;
     Col : TColor ;
     NewHeight : Integer ;
     RatioMode : Boolean ;
     FrameTypeNum,FrameTypeDen : Integer ;
     nAvg : Integer ;
begin

    LineName[LineNum] := 'Fluor' ;

    if DisplayModePage.ActivePage = FTab then Begin
       // Raw fluorescence display
       LineUnits[LineNum] := '' ;
       plPlot.yAxisLabel := format( '%s',[cbWavelength.Text]) ;
       RatioMode := False ;
       yDenominator := 1.0 ;
       yThreshold := 0.0 ;
       FrameTypeNum := cbWavelength.ItemIndex ;
       end
    else if DisplayModePage.ActivePage = DFTab then begin
       // Relative change plot
       LineUnits[LineNum] := '' ;
       plPlot.yAxisLabel := format( 'dF/F0 (%s)/',[cbWavelength.Text]) ;
       RatioMode := False ;
       yThreshold := 0.0 ;
       FrameTypeNum := cbDFWavelength.ItemIndex ;
       if rbF0FromFrames.Checked then begin
           // Compute F0 from average intensity within range of frame
           yDenominator := 0.0 ;
           nAvg := 0 ;
           for L := Round(edF0Range.LoValue) to Round(edF0Range.HiValue) do begin
               // Add to summation
               yDenominator := yDenominator +
                               ViewLineFrm.TimeCourseIntensity( L,FrameTypeNum + 1)  ;
               Inc(nAvg) ;
               end ;
           if nAvg > 0 then yDenominator := Round( yDenominator / nAvg )
                       else yDenominator := 0 ;
           end
        else begin
           // Constant F0 value
           yDenominator := Round(edF0Constant.Value) ;

           end ;
       end
    else if DisplayModePage.ActivePage = RatioTab then begin
       // Wavelength ratio/equation plot
       RatioMode := True ;
       yThreshold := edRatioExclusionThreshold.Value ;
       FrameTypeNum := cbNumWave.ItemIndex ;
       FrameTypeDen := cbDenomWave.ItemIndex ;
       if ckUseEquation.Checked then begin
          // Plot computed ion concentration
          LineUnits[LineNum] := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Units ;
          plPlot.yAxisLabel := format( '[%s] %s (%s)',
                               [MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Ion,
                                MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Units,
                                cbSource.Text]) ;
          RMax := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].RMax ;
          RMin := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].RMin ;
          KEff := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Keff ;
          end
       else begin
          // Plot ratio
          LineUnits[LineNum] := '' ;
          plPlot.yAxisLabel := format( '%s/%s',
                                    [cbNumWave.Text,
                                     cbDenomWave.Text]) ;
          if ckDivideByRMax.Checked then begin
             plPlot.yAxisLabel := plPlot.yAxisLabel + '/RMax' ;
             end ;
          end ;
       end ;

    // Size of plot compression block
    NumLinesPerBlock := Max( (EndAtLine - StartAtLine + 1) div MaxPlotPoints,1) ;
    // No. of display points per compression block
    NumPointsPerBlock := Min(NumLinesPerBlock,2) ;

    // Initialise counters
    BlockCount := NumLinesPerBlock ;
    NumPoints := 0 ;
    // tStep := (TInterval*TScale*NumLinesPerBlock)/NumPointsPerBlock ;
    // The NumPoints correction factors seem to be appropriate for ADC channel
    // traces, rather than the line scan.
    tStep := TInterval*TScale;
    ReportInterval := Max((EndAtLine - StartAtLine) div 100,1);
    StopPlot := False ;
    for L := StartAtLine to EndAtLine do begin

        // Line intensities at TC line

        yNumerator := ViewLineFrm.TimeCourseIntensity( L,FrameTypeNum + 1) ;
        if DisplayModePage.ActivePage = FTab then begin
           // Raw fluorescence
           y := yNumerator ;
           end
        else if DisplayModePage.ActivePage = DFTab then begin
           // Relative change
           if yDenominator <> 0.0 then
              y := ViewLineFrm.TimeCourseIntensity( L,FrameTypeNum + 1) / yDenominator
           else Y := 0.0 ;
           if rbDFOverF0.Checked then y := y - 1.0 ;
           end
        else begin
           // Ratio
           yDenominator := ViewLineFrm.TimeCourseIntensity( L,FrameTypeDen + 1) ;

           if yDenominator > yThreshold then begin

              // Calculate ratio
              Ratio := (yNumerator) / yDenominator ;

              if ckUseEquation.Checked then begin
                 // Plot computed ion concentration
                 if Ratio <= (RMax*0.99) then begin
                    y := ((Ratio - RMin)/(RMax - Ratio))*KEff ;
                    end
                 else y := RMax ;
                 end
              else begin
                 // Plot ratio
                 y := Ratio ;
                 if ckDivideByRMax.Checked then y := y / edRatioRMax.Value ;
                 end ;
              end
           else y := 0.0 ;
           end ;

        // Add new point to plot

        // Initialise compression block
        if BlockCount >= NumLinesPerBlock then begin
           yMin := 1E30 ;
           yMax := -yMin -1 ;
           BlockCount := 0 ;
           end ;

        // Update min/max
        if y < yMin then begin
           yMin := y ;
           yMinAt := BlockCount ;
           end ;
        if y > yMax then begin
           yMax := y ;
           yMaxAt := BlockCount ;
           end ;
        Inc(BlockCount) ;

        // When block complete ... write min./max. to display buffer
        if BlockCount >= NumLinesPerBlock then begin

           // First point
           t := (L-StartAtLine)*TStep ;
           if yMaxAt <= yMinAt then plPlot.AddPoint(LineNum, t, yMax )
                               else plPlot.AddPoint(LineNum, t, yMin ) ;
           Inc(NumPoints) ;

           // Second point
           if BlockCount > 1 then begin
              t := ((L-StartAtLine) + 0.5)*TStep ;
              if yMaxAt >= yMinAt then plPlot.AddPoint(LineNum, t, yMax )
                                  else plPlot.AddPoint(LineNum, t, yMin ) ;
              Inc(NumPoints) ;
              end ;

           end ;

        // Report progress
        MainFrm.StatusBar.SimpleText :=
              format(' Time Course: Line %d/%d (no. points %d)',[L,EndAtLine,NumPoints]) ;
        if ((L-StartAtLine) mod ReportInterval) = 0 then Application.ProcessMessages ;

        if StopPlot then Break ;

        end ;

    LineName[LineNum] := LineName[LineNum] + ' ' + plPlot.yAxisLabel ;

    end ;


procedure TTimeCourseFrm.PlotADCChannel(
          StartAtFrame : Integer ;       // Start at frame #
          EndAtFrame : Integer ;         // End at frame #
          PlotNum : Integer ;            // Plot on plot #
          LineNum : Integer              // Plot as line #
          ) ;
// ----------------------------
// Plot analogue signal channel
// ----------------------------
const
    NumScansPerBuf = 512 ;
var
    iChan : Integer ;                // Channel # selected for plotting
    StartScan : Integer ;            // Start plot scan #
    EndScan : Integer ;              // End plot scan #
    NumScans : Integer ;             // No. of channel scans in plot
    iScan : Integer ;                // Scan index

    // Compression block variables
    BlockCount : Integer ;           // No. of scans in block index
    NumScansPerBlock : Integer ;     // No. scans in compression block
    NumPointsPerBlock : Integer ;    // No. of displayed points per compression block
    y : Single ;                    // Sample value
    yMin : Single ;                 // Min. sample value within block
    yMax : Single ;                 // Max. sample value within block
    yMaxAt : Integer ;               // Blockcount index # of max sample
    yMinAt : Integer ;               // Blockcount index of min sample

    NumPoints : Integer ;            // No. of points in plot

    Done : Boolean ;
    t : Single ;                    // Current time (s)
    dt : Single ;                   // Inter-scan time interval
    tStep : Single ;                // Inter-point time interval on plot
    // File read buffer
    BufStartScan : Integer ;
    NumScansRead : Integer ;
    NumSamplesRead : Integer ;
    iSample : Integer ;              // Sample index
    Buf : Array[0..NumScansPerBuf*(ADCChannelLimit+1)-1] of SmallInt ;

begin

     // A/D channel to be plotted
     iChan := Integer(cbSource.Items.Objects[cbSource.ItemIndex])
              - (MainFrm.IDRFile.MaxROI+1) ;

     // Inter-scan interval (s)
     dt :=  MainFrm.IDRFile.ADCScanInterval ;

     // Y axis label
     LineUnits[LineNum] := MainFrm.IDRFile.ADCChannel[iChan].ADCUnits ;
     LineName[LineNum] := MainFrm.IDRFile.ADCChannel[iChan].ADCName ;
     plPlot.yAxisLabel := MainFrm.IDRFile.ADCChannel[iChan].ADCName
                          + ' (' + MainFrm.IDRFile.ADCChannel[iChan].ADCUnits + ')' ;

     StartScan := Round( ((StartAtFrame-1)*TInterval)/dt) ;
     // EndScan := Round( (EndAtFrame*TInterval)/dt ) ;
     // Allow plotting of full ADC trace even if longer than line scan
     EndScan := Max(Round((EndAtFrame*TInterval)/dt ),
                    MainFrm.IDRFile.ADCNumScansInFile);

     // No. of multi-channel scans to be displayed
     NumScans := EndScan - StartScan + 1 ;

     // Size of display compression block
     NumScansPerBlock := Max( NumScans div MaxPlotPoints,1) ;
     // No. of display points per compression block
     NumPointsPerBlock := Min(NumScansPerBlock,2) ;

     // Initialise counters
     BlockCount := NumScansPerBlock ;
     NumScansRead := NumScansPerBuf ;
     iSample := NumScansRead*MainFrm.IDRFile.ADCNumChannels ;
     BufStartScan := StartScan ;
     iScan := StartScan ;
     NumPoints := 0 ;
     Done := False ;
     t := 0.0 ;
     tStep := (dt*NumScansPerBlock*TScale) / Max(NumPointsPerBlock,1) ;

     While not Done do begin

        // Load new buffer
        if iSample >= NumSamplesRead then begin
           NumScansRead := MainFrm.IDRFile.LoadADC( BufStartScan,
                                                    NumScansPerBuf,
                                                    Buf ) ;
           NumSamplesRead := NumScansRead*MainFrm.IDRFile.ADCNumChannels ;
           BufStartScan := BufStartScan + NumScansPerBuf ;
           iSample := MainFrm.IDRFile.ADCChannel[iChan].ChannelOffset ;
           if NumScansRead <= 0 then Break ;
           end ;

        // Initialise compression block
        if BlockCount >= NumScansPerBlock then begin
           yMin := 1E30 ;
           yMax := -1E30 ;
           BlockCount := 0 ;
           end ;

        // Get A/D sample and add to block
        y := (Buf[iSample] - MainFrm.IDRFile.ADCChannel[iChan].ADCZero)
              *MainFrm.IDRFile.ADCChannel[iChan].ADCScale ;
        if y < yMin then begin
           yMin := y ;
           yMinAt := BlockCount ;
           end ;
        if y > yMax then begin
           yMax := y ;
           yMaxAt := BlockCount ;
           end ;
        iSample := iSample + MainFrm.IDRFile.ADCNumChannels ;
        Inc(BlockCount) ;

        // When block complete ... write min./max. to display buffer
        if BlockCount >= NumScansPerBlock then begin

           // First point
           if yMaxAt <= yMinAt then plPlot.AddPoint(LineNum, t, yMax )
                               else plPlot.AddPoint(LineNum, t, yMin ) ;
           t := t + tStep ;
           Inc(NumPoints) ;

           // Second point
           if BlockCount > 1 then begin
              if yMaxAt >= yMinAt then plPlot.AddPoint(LineNum, t, yMax )
                                  else plPlot.AddPoint(LineNum, t, yMin ) ;
              t := t + tStep ;
              Inc(NumPoints) ;
              end ;

           end ;

        Inc(iScan) ;
        if (iScan > EndScan) or
           ( NumScansRead <= 0) or
           (NumPoints > (MaxPlotPoints*2)) then Done := True ;

        end ;
     end ;


procedure TTimeCourseFrm.FormResize(Sender: TObject);
// ----------------------------------------
// Adjusted controls when form is re-sized
// ----------------------------------------
begin

     // Plot display area
     plPlot.Height := Max( ClientHeight - plPlot.Top - 5,2) ;
     plPlot.Width := Max( ClientWidth - plPlot.Left - 5, 2) ;

     end;


procedure TTimeCourseFrm.UpdateSettings ;
// -------------------------------------------------------
// Update available regions of interest and other settings
// -------------------------------------------------------
var
    i,iOld,iOldWave,iOldNumWave,iOldDenomWave : Integer ;
    Wavelength : Single ;
begin

    // Create list of sources
    cbSource.Clear ;

    // Regions of interest
    if MainFrm.IDRFile.LineScan then begin
       // Line scan data file
       cbSource.Items.AddObject( 'Fluor',TObject(1)) ;
       rbF0FromFrames.Caption := 'F0 lines'
       end
    else begin
       // Image data files : list regions of interest
       for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then
           cbSource.Items.AddObject( format('ROI.%d',[i]),TObject(i)) ;
       rbF0FromFrames.Caption := 'F0 frames'
       end ;

    // A/D channels
    for i := 0 to MainFrm.IDRFile.ADCNumChannels-1 do
        cbSource.Items.AddObject( MainFrm.IDRFile.ADCChannel[i].ADCName,
                                  TObject(i+MainFrm.IDRFile.MaxROI+1)) ;
    cbSource.ItemIndex := 0 ;

    // Create list of background subtraction sources
    cbBackground.Clear ;
    cbBackground.Items.AddObject( ' ',TObject(MainFrm.IDRFile.MaxROI+1)) ;
    if MainFrm.IDRFile.LineScan then begin
       end
    else begin
       // Image series data file
       for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then
           cbBackground.Items.AddObject( format('ROI.%d',[i]),TObject(i)) ;
       end ;
    cbBackground.ItemIndex := 0 ;

    // Enabled plot creation buttons if sources available
    if cbSource.Items.Count > 0 then begin
       bNewPlot.Enabled := True ;
       bAddLineToPlot.Enabled := True ;
       end
    else begin
       bNewPlot.Enabled := False ;
       bAddLineToPlot.Enabled := False ;
       end ;

    // Update frame types
       // Other
    cbWavelength.Clear ;
    cbDfWavelength.Clear ;
    iOldWave := cbWavelength.ItemIndex ;
    iOldNumWave := cbNumWave.ItemIndex ;
    iOldDenomWave := cbDenomWave.ItemIndex ;

    for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
        cbWavelength.Items.Add(MainFrm.IDRFile.FrameType[i]) ;

    cbDfWavelength.Items.Assign(cbWavelength.Items);
    cbNumWave.Items.Assign(cbWavelength.Items);
    cbDenomWave.Items.Assign(cbWavelength.Items);

    cbWavelength.ItemIndex := Max( iOldWave,0 ) ;
    cbDfWavelength.ItemIndex := Max( iOldWave,0 ) ;
    cbNumWave.ItemIndex := Max( iOldNumWave,0 ) ;
    cbDenomWave.ItemIndex := Max( iOldDenomWave,0 ) ;

    if (cbDenomWave.ItemIndex = cbNumWave.ItemIndex) then begin
       for i := 0 to cbNumWave.Items.Count-1 do
           if i <> cbNumWave.ItemIndex then cbDenomWave.ItemIndex := i ;
       end ;

    // Single or frame ratio
    if MainFrm.IDRFile.NumFrameTypes = 1 then begin
       DisplayModePage.ActivePage := FTab ;
       end ;

    // Update available ion binding equations
    iOld := cbEquation.ItemIndex ;
    cbEquation.Clear ;
    for i := 0 to MainFrm.IDRFile.MaxEquations-1 do
        if MainFrm.IDRFile.Equation[i].InUse then
           cbEquation.Items.AddObject( MainFrm.IDRFile.Equation[i].Name,
                                       TObject(i) ) ;
    if cbEquation.Items.Count > 0 then cbEquation.Enabled := True
                                   else cbEquation.Enabled := False ;
    if iOld < cbEquation.Items.Count then cbEquation.ItemIndex := iOld ;
    if (iOld < 0) and (cbEquation.Items.Count > 0) then cbEquation.ItemIndex := 0 ;
    if cbEquation.Items.Count > 0 then ckUseEquation.Enabled := True
                                  else ckUseEquation.Enabled := False ;

    end;


procedure TTimeCourseFrm.bClearPlotsClick(Sender: TObject);
//  --------------------------
// Clear all plots on display
// --------------------------
begin

     if cbClearPlot.ItemIndex = 0 then begin
        plPlot.ClearAllPlots ;
        end
     else begin
        plPlot.ClearPlot( Integer(cbClearPlot.Items.Objects[cbClearPlot.ItemIndex])) ;
        end ;

     FillPlotLists ;

     end;

procedure TTimeCourseFrm.FillPlotLists ;
// -------------------------
// Fill plot selection lists
// -------------------------
var
    i : Integer ;
    PlotsAvailable : Boolean ;
begin

     cbPlotNum.Clear ;
     cbClearPlot.Clear ;
     cbClearPlot.Items.AddObject('All',TObject(MaxPlots));
     PlotsAvailable := False ;
     for i := 0 to MaxPlots-1 do if plPlot.PlotExists[i] then begin
         cbPlotNum.Items.AddObject(format('%d',[i+1]),TObject(i)) ;
         cbClearPlot.Items.AddObject(format('%d',[i+1]),TObject(i)) ;
         cbPlotNum.ItemIndex := 0 ;
         PlotsAvailable := True ;
         end ;

     cbClearPlot.ItemIndex := 0 ;
     PlotAvailable := PlotsAvailable ;
     bSetAxes.Enabled := PlotsAvailable ;
     bAddLineToPlot.Enabled := PlotsAvailable ;

     end ;


procedure TTimeCourseFrm.CopyDataToClipBoard ;
{ ---------------------------------------------
  Copy the graph plot(s) data to the clipboard
  --------------------------------------------- }
begin
     plPlot.CopyDataToClipboard ;
     end ;


procedure TTimeCourseFrm.Print ;
{ ------------------------------
  Print graph plot(s) on display
  ------------------------------ }
begin
     PrintGraphFrm.Plot := plPlot ;
     PrintGraphFrm.ToPrinter := True ;
     PrintGraphFrm.MultiYPlot := True ;
     PrintGraphFrm.ShowModal ;
     if PrintGraphFrm.ModalResult = mrOK then begin
        plPlot.ClearPrinterTitle ;
        plPlot.AddPrinterTitleLine( ' File : ' + MainFrm.IDRFile.FileName ) ;
        plPlot.AddPrinterTitleLine( ' ' + MainFrm.IDRFile.Ident ) ;
        plPlot.Print ;
        end ;
     end ;


procedure TTimeCourseFrm.CopyImageToClipboard ;
{ ------------------------------------------------------------
  Copy image of graph plot(s) to clipboard as Windows metafile
  ------------------------------------------------------------ }
begin
     PrintGraphFrm.Plot := plPlot ;
     PrintGraphFrm.ToPrinter := False ;
     PrintGraphFrm.MultiYPlot := True ;
     PrintGraphFrm.ShowModal ;
     if PrintGraphFrm.ModalResult = mrOK then plPlot.CopyImageToClipboard ;
     end ;


procedure TTimeCourseFrm.bSetAxesClick(Sender: TObject);
begin
     SetAxesFrm.Plot := plPlot ;
     SetAxesFrm.Show ;
     end;


procedure TTimeCourseFrm.bAddLineToPlotClick(Sender: TObject);
// ------------------------------------
// Add a new line to an existing Y Axis
// ------------------------------------
var
    i : Integer ;
begin

      // Prevent plotting if time course is being calculated
      if MainFrm.FormExists( 'ViewPlotFrm' ) then begin
         if not ViewPlotFrm.FLTimeCourseAvailable then Exit ;
         end
      else if not MainFrm.FormExists( 'ViewLineFrm' ) then Exit ;

     bAddLineToPlot.Enabled := False ;
     ROIGrp.Enabled := False ;

     { Add new line to selected plot }
     plPlot.PlotNum := Integer(cbPlotNum.Items.Objects[cbPlotNum.ItemIndex]) ;
     // Add line to plot
     PlotLine ;

     bAddLineToPlot.Enabled := True ;
     ROIGrp.Enabled := True ;

     end ;


procedure TTimeCourseFrm.bAddAllROIsClick(Sender: TObject);
// --------------------------
// Add all ROIs to a new plot
// --------------------------
var
    i,iROI,iBackg : Integer ;
    PlotNum : Integer ;
begin

     // Only use for image files
     if MainFrm.IDRFile.LineScan then Exit ;

      // Prevent plotting if time course is being calculated
      for i := 0 to MainFrm.MDIChildCount-1 do
          if MainFrm.MDIChildren[i].Name = 'ViewPlotFrm' then begin
            if not ViewPlotFrm.FLTimeCourseAvailable then Exit ;
            end ;

     // Disable button while line is plotted

     ROIGrp.Enabled := False ;
     bAddAllROIs.Enabled := False ;

     { Add new Y Axis to plot }
     PlotNum := plPlot.CreatePlot ;

     FillPlotLists ;
     cbPlotNum.ItemIndex := cbPlotNum.Items.Count-1 ;

     if iBackg >= 0 then begin
        iBackg := Integer(cbBackground.Items.Objects[cbBackground.ItemIndex]) ;
        end
     else iBackg := -1 ;

     // Add all ROIs (except background) to plot
     for i := 0 to cbSource.Items.Count-1 do begin
         iROI := Integer(cbSource.Items.Objects[i]) ;
         if (iROI <> iBackg) and (Pos('ROI',cbSource.Items.Strings[i]) > 0) then begin
            cbSource.ItemIndex := i ;
            PlotLine ;
            if StopPlot then Break ;
            end ;
         end ;

     // Re-enable button
     bAddAllROIs.Enabled := True ;
     ROIGrp.Enabled := True ;

     end;

procedure TTimeCourseFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
    i : Integer ;
begin

   {  if not PlotGrp.Enabled then begin
        CanClose := False ;
        StopPlot := True ;
        for i := 0 to MainFrm.MDIChildCount-1 do
            if MainFrm.MDIChildren[i].Name = 'ViewPlotFrm' then begin
               if TViewPlotFrm(MainFrm.MDIChildren[i]).ComputeFLTimeCourseRunning then
                  TViewPlotFrm(MainFrm.MDIChildren[i]).StopComputeFLTimeCourseRunning := True ;
               end ;
        end
     else CanClose := True ;}

     end;


procedure TTimeCourseFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// -----------------------------
// Clear up when form is closed
// -----------------------------
begin
     Action := caFree ;
     end;


procedure TTimeCourseFrm.edPixelExclusionThresholdKeyPress(Sender: TObject;
  var Key: Char);
begin

     // Require fluorescence time course to be re-calculated if
     // pixel exclusion threshold changed
     if MainFrm.FormExists('ViewPlotFrm') then ViewPlotFrm.NewFLTimeCourseRequired ;

     end;

procedure TTimeCourseFrm.ckExcludePixelsClick(Sender: TObject);
begin

     // Require fluorescence time course to be re-calculated if
     // pixel exclusion threshold changed

     if MainFrm.FormExists('ViewPlotFrm') then ViewPlotFrm.NewFLTimeCourseRequired ;

     end ;


procedure TTimeCourseFrm.ckDivideByRMaxClick(Sender: TObject);
begin
     if ckDivideByRMax.Checked then ckUseEquation.Checked := False ;
     end;

procedure TTimeCourseFrm.ckUseEquationClick(Sender: TObject);
begin
     if ckUseEquation.Checked then ckDivideByRMax.Checked := False ;
     end;

procedure TTimeCourseFrm.cbFluorescenceChange(Sender: TObject);
// ---------------------------------------
// Event fluorescence display mode changed
// ---------------------------------------
begin
      case cbFluorescence.ItemIndex of
          0 : DisplayModePage.ActivePage := FTab ;
          1 : DisplayModePage.ActivePage := RatioTab ;
          2 : DisplayModePage.ActivePage := dFTab ;
          end ;
       end;


procedure TTimeCourseFrm.cbSourceChange(Sender: TObject);
// --------------------
// Plot source changed
// --------------------
begin
      if Integer(cbSource.Items.Objects[cbSource.ItemIndex])
         <= MainFrm.IDRFile.MaxROI then FluorescenceDisplayGrp.Visible := True
                                   else FluorescenceDisplayGrp.Visible := False ;
      end;

end.
