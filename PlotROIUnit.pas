unit PlotROIUnit;
// ----------------------------------------------------------------
// PICViewer - Plot a graph of series of ROI intensity measurements
// c) J. Dempster, University of Strathclyde, 2004
// ----------------------------------------------------------------
// 26.04.04

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, RangeEdit, ExtCtrls, ValidatedEdit, ViewUnit;

type
  TPlotROIFrm = class(TForm)
    SourceGrp: TGroupBox;
    cbNumFile: TComboBox;
    lbNumSection: TLabel;
    edNumSection: TValidatedEdit;
    RatioPanel: TPanel;
    lbDenSection: TLabel;
    Shape1: TShape;
    cbDenFile: TComboBox;
    edDenSection: TValidatedEdit;
    ckRatio: TCheckBox;
    ROIGrp: TGroupBox;
    ckROI1: TCheckBox;
    CKROI2: TCheckBox;
    ckROI3: TCheckBox;
    ckROI4: TCheckBox;
    ckROI5: TCheckBox;
    ckROI6: TCheckBox;
    ckROI7: TCheckBox;
    ckROI8: TCheckBox;
    GroupBox3: TGroupBox;
    cbSubtractROI: TComboBox;
    ckSubtractROI: TCheckBox;
    RangeGrp: TGroupBox;
    rbAllFrames: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    bOK: TButton;
    bCancel: TButton;
    VariablesGrp: TGroupBox;
    cbXAxisVar: TComboBox;
    Label1: TLabel;
    Label3: TLabel;
    cbYAxisVar: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure cbNumFileChange(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure ckRatioClick(Sender: TObject);
    procedure ckROI1Click(Sender: TObject);
    procedure cbDenFileChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateOK ;
    procedure PlotROIMeasurements ;
    procedure PlotROIRatioMeasurements ;
    function MeanIntensity(
             var Runs : Array of TRunElement ; // Pixel runs defining region
             NumRuns : Integer ;           // No. of pixel runs
             FrameWidth : Integer ;        // Line width of image
             var Buf : Array of Integer    // Image buffer
             ) : Single ;

    function VarianceIntensity(
             var Runs : Array of TRunElement ; // Pixel runs defining region
             NumRuns : Integer ;           // No. of pixel runs
             FrameWidth : Integer ;        // Line width of image
             var Buf : Array of Integer    // Image buffer
             ) : Single ;

    function MaxIntensity(
          var Runs : Array of TRunElement ; // Pixel runs defining region
          NumRuns : Integer ;               // No. of pixel runs
          FrameWidth : Integer ;            // Line width of image
          var Buf : Array of Integer        // Image buffer
          ) : Single ;
    function MinIntensity(
          var Runs : Array of TRunElement ; // Pixel runs defining region
          NumRuns : Integer ;               // No. of pixel runs
          FrameWidth : Integer ;            // Line width of image
          var Buf : Array of Integer        // Image buffer
          ) : Single ;

    function PlotROI(
             iROI : Integer        // Region of interest #
             ) : Boolean ;

  public
    { Public declarations }
  end;

var
  PlotROIFrm: TPlotROIFrm;

implementation

uses PicViewMain ;

{$R *.dfm}

const
     NoFileSelected = 9999 ;

     // Plot variables
     vFrameNum = 0 ;
     vZDepth = 1 ;
     vTime = 2 ;
     vMean = 3 ;
     vStDev = 4 ;
     vVariance = 5 ;
     vMax = 6 ;
     vMin = 7 ;




procedure TPlotROIFrm.FormShow(Sender: TObject);
// -------------------------------------
// Initialisation when form is displayed
// -------------------------------------
var
     i : Integer ;
begin

     // Get list of available image stacks
     cbNumFile.Clear ;
     cbNumFile.Items.AddObject( ' ', TObject(NoFileSelected)) ;
     cbDenFile.Clear ;
     cbDenFile.Items.AddObject( ' ', TObject(NoFileSelected)) ;
     for i :=  0 to High(MainFrm.ViewFrmsInUse) do if MainFrm.ViewFrmsInUse[i] then begin
         cbNumFile.Items.AddObject( MainFrm.ViewFrms[i].FileName, TObject(i)) ;
         cbDenFile.Items.AddObject( MainFrm.ViewFrms[i].FileName, TObject(i)) ;
         end ;
     cbNumFile.ItemIndex := 0 ;
     cbDenFile.ItemIndex := 0 ;

     edNumSection.Value := 1 ;
     edDenSection.Value := 1 ;

     RatioPanel.Visible := ckRatio.Checked ;

     // Set X and Y axis variable selection boxes
     cbXAxisVar.Clear ;
     cbXAxisVar.Items.AddObject('Frame No.',TObject(vFrameNum)) ;
     cbXAxisVar.Items.AddObject('Z Depth',TObject(vZDepth)) ;
     cbXAxisVar.Items.AddObject('Time',TObject(vTime)) ;
     cbXAxisVar.Items.AddObject('Mean',TObject(vMean)) ;
     cbXAxisVar.Items.AddObject('St. Dev.',TObject(vStdev)) ;
     cbXAxisVar.Items.AddObject('Variance',TObject(vVariance)) ;
     cbXAxisVar.Items.AddObject('Max.',TObject(vMax)) ;
     cbXAxisVar.Items.AddObject('Min.',TObject(vMin)) ;
     cbXAxisVar.ItemIndex := cbXAxisVar.Items.IndexOfObject(Tobject(vFrameNum)) ;

     cbYAxisVar.Items.Assign(cbXAxisVar.Items) ;
     cbYAxisVar.ItemIndex := cbYAxisVar.Items.IndexOfObject(Tobject(vMean)) ;

     // Enable OK button if plot available
     UpdateOK ;

     end;



procedure TPlotROIFrm.cbNumFileChange(Sender: TObject);
// ----------------------------------
// Plot numerator source file changed
// ----------------------------------
var
     i : Integer ;
     iNum : Integer ;
begin

     iNum := Integer(cbNumFile.Items.Objects[cbNumFile.ItemIndex]) ;
     if iNum <> NoFileSelected then begin
        // Set ROI selection boxes
        ckROI1.Checked := MainFrm.ViewFrms[iNum].ROIsUnscaled[1].InUse ;
        ckROI1.Visible := ckROI1.Checked ;
        ckROI2.Checked := MainFrm.ViewFrms[iNum].ROIsUnscaled[2].InUse ;
        ckROI2.Visible := ckROI2.Checked ;
        ckROI3.Checked := MainFrm.ViewFrms[iNum].ROIsUnscaled[3].InUse ;
        ckROI3.Visible := ckROI3.Checked ;
        ckROI4.Checked := MainFrm.ViewFrms[iNum].ROIsUnscaled[4].InUse ;
        ckROI4.Visible := ckROI4.Checked ;
        ckROI5.Checked := MainFrm.ViewFrms[iNum].ROIsUnscaled[5].InUse ;
        ckROI5.Visible := ckROI5.Checked ;
        ckROI6.Checked := MainFrm.ViewFrms[iNum].ROIsUnscaled[6].InUse ;
        ckROI6.Visible := ckROI6.Checked ;
        ckROI7.Checked := MainFrm.ViewFrms[iNum].ROIsUnscaled[7].InUse ;
        ckROI7.Visible := ckROI7.Checked ;
        ckROI8.Checked := MainFrm.ViewFrms[iNum].ROIsUnscaled[8].InUse ;
        ckROI8.Visible := ckROI8.Checked ;

        // Fill subtraction ROI list
        cbSubtractROI.Clear ;
        cbSubtractROI.Items.AddObject(format(' ',[i]),TObject(MaxROIs+1)) ;
        for i := 1 to MaxROIs do if MainFrm.ViewFrms[iNum].ROIsUnscaled[i].InUse then
            cbSubtractROI.Items.AddObject(format('ROI#%d',[i]),TObject(i)) ;
        cbSubtractROI.ItemIndex := 0 ;

        // Set range of frames to be plotted
        if MainFrm.ViewFrms[iNum].NumSectionsPerStack > 1 then begin
           // 3D stack series
           lbNumSection.Visible := True ;
           edNumSection.Visible := lbNumSection.Visible ;
           lbDenSection.Visible := lbNumSection.Visible ;
           edDensection.Visible := lbNumSection.Visible ;
           RangeGrp.Caption := ' Stack Range ' ;
           rbAllFrames.Caption := 'All Stacks' ;
           edRange.HiLimit := MainFrm.ViewFrms[iNum].NumStacks ;
           edRange.HiValue := MainFrm.ViewFrms[iNum].NumStacks ;
           end
        else begin
           // 2D frame series
           lbNumSection.Visible := False ;
           edNumSection.Visible := lbNumSection.Visible ;
           lbDenSection.Visible := lbNumSection.Visible ;
           edDensection.Visible := lbNumSection.Visible ;
           RangeGrp.Caption := ' Frame Range ' ;
           rbAllFrames.Caption := 'All Frames' ;
           edRange.HiLimit := MainFrm.ViewFrms[iNum].NumFrames ;
           edRange.HiValue := MainFrm.ViewFrms[iNum].NumFrames ;
           end ;

        end ;

     // Enable OK button if plot possible
     UpdateOK ;

     end;


procedure TPlotROIFrm.UpdateOK ;
// ------------------------------------
// Enable OK button if plot is possible
// ------------------------------------
var
    iROI : Integer ;
begin

     bOK.Enabled := False ;

     // Exit (leaving OK disabled) if no source
     if Integer(cbNumFile.Items.Objects[cbNumFile.ItemIndex])
        = NoFileSelected then Exit ;

     // Exit (leaving OK disabled) if ratio selected and no denominator
     if ckRatio.Checked and
        (Integer(cbDenFile.Items.Objects[cbDenFile.ItemIndex])
        = NoFileSelected) then Exit ;

     // Enable OK if an ROI available
     for iROI := 1 to MaxROIs do begin
         if PlotROI(iROI) then bOK.Enabled := True ;
         end ;

     end ;


procedure TPlotROIFrm.PlotROIMeasurements ;
// --------------------------------------------------
// Plot ROI measurements
// --------------------------------------------------
type
    TRuns = Array[0..4096*4096] of TRunElement ;
    PRuns = ^TRuns ;

var
     FileName : String ;  // Source file name
     iNum : Integer ;     // Numerator source file index
     XVar : Integer ;    // X Axis variable
     YVar : Integer ;    // Y Axis variable
     StartAt : Integer ;   // Start plot at stack(4D)/frame(3D)
     EndAt : Integer ;     // End plot at stack(4D)/frame(3D)

     StartFrame : Integer ; // First frame in sequence to be plotted
     EndFrame : Integer ;   // End frame in sequence to be plotted
     FrameStep : Integer ;  // Frame increment

     i,j : Integer ;           // General counter
     iROI : Integer ;          // ROI counter
     iCol : Integer ;
     FrameNum : Integer ;    // Source frame counter

     FrameWidth : Cardinal ;
     FrameHeight : Cardinal ;
     NumFrames : Cardinal ;
     NumPixelsPerFrame : Cardinal ;
     NumComponentsPerPixel : Cardinal ;
     PixelDepth : Cardinal ;
     NumComponentsPerFrame : Cardinal ;

     XResolution : Single ;       // Pixel width
     YResolution : Single ;       // Pixel height
     PixelUnits : string ;       // Pixel width units

     OK : Boolean ;

     PWorkBuf : PIntArray ;         // Numerator frame buffer pointer
     Runs : PRuns ;                 // pointer to pixel runs array
     NumRuns : Integer ;            // No. of pixel runs available
     iSubROI : Integer ;            // ROI # to be subtracted
     SubRuns : PRuns ;              // pointer to subtraction pixel runs array
     NumSubRuns : Integer ;            // No. of subtraction pixel runs available

     x : Single ;
     y : Single ;
     GraphFrmNum : Integer ;
begin

     // Source of image stacks to be plotted
     iNum := Integer(cbNumFile.Items.Objects[cbNumFile.ItemIndex]) ;
     // ROI measurements to be plotted
     XVar := Integer(cbXAxisVar.Items.Objects[cbXAxisVar.ItemIndex]) ;
     YVar := Integer(cbYAxisVar.Items.Objects[cbYAxisVar.ItemIndex]) ;

     // Get range of stacks/frames to be plotted
     if rbAllFrames.Checked then begin
        StartAt := 1 ;
        EndAt := Round(edRange.HiLimit) ;
        end
     else begin
        StartAt := Round(edRange.LoValue) ;
        EndAt := Round(edRange.HiValue) ;
        end ;

     // Get range of frames to be plotted

     if MainFrm.ViewFrms[iNum].NumSectionsPerStack > 1 then begin
        // 4D (stack series) data file
        StartFrame := (StartAt-1)*MainFrm.ViewFrms[iNum].NumSectionsPerStack + Round(edNumSection.Value) ;
        EndFrame := (EndAt-1)*MainFrm.ViewFrms[iNum].NumSectionsPerStack + Round(edNumSection.Value) ;
        FrameStep := MainFrm.ViewFrms[iNum].NumSectionsPerStack ;
        end
     else begin
        // 3D (frame series) data file
        StartFrame := StartAt ;
        EndFrame := EndAt ;
        FrameStep := 1 ;
        end ;

     // Get image properties

     OK := True ;
     if (iNum = NoFileSelected) then begin
        MessageDlg( 'PLOT ROI: Source file required!', mtError, [mbOK], 0 ) ;
        Exit ;
        end ;

     // Create graph plot form
     GraphFrmNum := MainFrm.CreateNewGraphFrm( 'Plot: ' + MainFrm.ViewFrms[iNum].FileName ) ;

     FrameWidth := MainFrm.ViewFrms[iNum].FrameWidth ;
     FrameHeight := MainFrm.ViewFrms[iNum].FrameHeight ;
     NumFrames := MainFrm.ViewFrms[iNum].NumFrames ;
     XResolution := MainFrm.ViewFrms[iNum].XResolution ;

     NumPixelsPerFrame := FrameWidth*FrameHeight ;
     NumComponentsPerPixel := 1 ;
     PixelDepth := 16 ;
     NumComponentsPerFrame := NumPixelsPerFrame*NumComponentsPerPixel ;


     // Allocate frame buffers
     GetMem( PWorkBuf, NumPixelsPerFrame*4 ) ;
     GetMem( Runs, (NumPixelsPerFrame div 2)*SizeOf(TRunElement) ) ;
     GetMem( SubRuns, (NumPixelsPerFrame div 2)*SizeOf(TRunElement) ) ;

     try

        // Get subtraction ROI (if one is defined)
        iSubROI := Integer(cbSubtractROI.Items.Objects[cbSubtractROI.ItemIndex]) ;
        if (iSubROI >= 1) and (iSubROI <= MaxROIs) then begin
           MainFrm.ViewFrms[iNum].GetROIPixelRuns( iSubROI, SubRuns^, NumSubRuns ) ;
           end
        else NumSubRuns := 0 ;

        for iROI := 1 to MaxROIs do
           if PlotROI(iROI) and (iROI <> iSubROI) then begin

           // Set X and Y axis units
           case xVar of
                 vFrameNum,vMean,vStDev,vMin,vMax : MainFrm.GraphFrms[GraphFrmNum].XUnits := '' ;
                 vTime : MainFrm.GraphFrms[GraphFrmNum].XUnits := 's' ;
                 vZDepth : MainFrm.GraphFrms[GraphFrmNum].XUnits := MainFrm.ViewFrms[iNum].PixelUnits ;
                 end ;
           case yVar of
                 vFrameNum,vMean,vStDev,vMin,vMax : MainFrm.GraphFrms[GraphFrmNum].YUnits := '' ;
                 vTime : MainFrm.GraphFrms[GraphFrmNum].YUnits := 's' ;
                 vZDepth : MainFrm.GraphFrms[GraphFrmNum].YUnits := MainFrm.ViewFrms[iNum].PixelUnits ;
                 end ;

            // Create line on graph plot
            MainFrm.GraphFrms[GraphFrmNum].CreateLine( iROI ) ;

            // Set axes labels
            MainFrm.GraphFrms[GraphFrmNum].XAxisLabel := cbXAxisVar.Text + ' '
                                                         + MainFrm.GraphFrms[GraphFrmNum].XUnits ;
            MainFrm.GraphFrms[GraphFrmNum].YAxisLabel := cbYAxisVar.Text + ' '
                                                         + MainFrm.GraphFrms[GraphFrmNum].YUnits ;

            // Get pixel runs within ROI
            MainFrm.ViewFrms[iNum].GetROIPixelRuns( iROI, Runs^, NumRuns ) ;

            FrameNum := StartFrame ;
            While FrameNum <= EndFrame do begin

                // Load numerator & denominator frames
                MainFrm.ViewFrms[iNum].LoadFrame( FrameNum, PWorkBuf ) ;

                // X axis variable
                case xVar of
                     vFrameNum : x := FrameNum ;
                     vTime : x := (FrameNum-1)*MainFrm.ViewFrms[iNum].TResolution ;
                     vZDepth : x := ((FrameNum-1) mod MainFrm.ViewFrms[iNum].NumSectionsPerStack)
                                    *MainFrm.ViewFrms[iNum].ZResolution ;
                     vMean : x := MeanIntensity( Runs^, NumRuns, FrameWidth, PWorkBuf^ ) ;
                     vVariance : x := VarianceIntensity( Runs^, NumRuns, FrameWidth, PWorkBuf^ ) ;
                     vStDev : x := Sqrt(VarianceIntensity( Runs^, NumRuns, FrameWidth, PWorkBuf^ )) ;
                     vMax : x := MaxIntensity( Runs^, NumRuns, FrameWidth, PWorkBuf^ ) ;
                     vMin : x := MinIntensity( Runs^, NumRuns, FrameWidth, PWorkBuf^ ) ;
                     end ;

                // Y axis variable
                case yVar of
                     vFrameNum : y := FrameNum ;
                     vTime : y := (FrameNum-1)*MainFrm.ViewFrms[iNum].TResolution ;
                     vZDepth : y := ((FrameNum-1) mod MainFrm.ViewFrms[iNum].NumSectionsPerStack)
                                    *MainFrm.ViewFrms[iNum].ZResolution ;

                     vMean : begin
                          y := MeanIntensity( Runs^, NumRuns, FrameWidth, PWorkBuf^ ) ;
                          if NumSubRuns > 0 then
                             y := y - MeanIntensity( SubRuns^, NumSubRuns, FrameWidth, PWorkBuf^ ) ;
                          end ;

                     vVariance : begin
                          y := VarianceIntensity( Runs^, NumRuns, FrameWidth, PWorkBuf^ ) ;
                          if NumSubRuns > 0 then
                             y := y - VarianceIntensity( SubRuns^, NumSubRuns, FrameWidth, PWorkBuf^ ) ;
                          end ;

                     vStDev : begin
                          y := Sqrt(VarianceIntensity( Runs^, NumRuns, FrameWidth, PWorkBuf^ )) ;
                          if NumSubRuns > 0 then
                             y := y - Sqrt(VarianceIntensity( SubRuns^, NumSubRuns, FrameWidth, PWorkBuf^ )) ;
                          end ;

                     vMax : begin
                          y := MaxIntensity( Runs^, NumRuns, FrameWidth, PWorkBuf^ ) ;
                          if NumSubRuns > 0 then
                             y := y - MaxIntensity( SubRuns^, NumSubRuns, FrameWidth, PWorkBuf^ ) ;
                          end ;

                     vMin : begin
                          y := MinIntensity( Runs^, NumRuns, FrameWidth, PWorkBuf^ ) ;
                          if NumSubRuns > 0 then
                             y := y - MinIntensity( SubRuns^, NumSubRuns, FrameWidth, PWorkBuf^ ) ;
                          end ;

                     end ;

                // Add point to line
                MainFrm.GraphFrms[GraphFrmNum].AddPoint( iROI, x, y ) ;

                // Report progress
                MainFrm.StatusBar.SimpleText := format(
                'PLOT: Frame %d/%d',
                [FrameNum,NumFrames] ) ;

                // Increment to next frame
                FrameNum := FrameNum + FrameStep ;

                end ;

            end ;

        // Set graph cursors to default positions
        MainFrm.GraphFrms[GraphFrmNum].InitialiseCursors ;

        MainFrm.StatusBar.SimpleText := format(
        'PLOT: File: %s created',
        [FileName] ) ;

     finally
        FreeMem( PWorkBuf ) ;
        FreeMem( Runs ) ;
        FreeMem( SubRuns ) ;
        end ;

     end ;


procedure TPlotROIFrm.PlotROIRatioMeasurements ;
// ----------------------------
// Plot ROI measurement ratios
// ----------------------------
type
    TRuns = Array[0..4096*4096] of TRunElement ;
    PRuns = ^TRuns ;

var
     FileName : String ;  // Source file name
     iNum : Integer ;     // Numerator source file index
     iDen : Integer ;     // Denominator source file index
     XVar : Integer ;    // X Axis variable
     YVar : Integer ;    // Y Axis variable

     StartAt : Integer ;   // Start plot at stack(4D)/frame(3D)
     EndAt : Integer ;     // End plot at stack(4D)/frame(3D)

     StartFrame : Integer ; // First frame in sequence to be plotted
     EndFrame : Integer ;   // End frame in sequence to be plotted
     FrameStep : Integer ;  // Frame increment

     i,j : Integer ;           // General counter
     iROI : Integer ;          // ROI counter
     iCol : Integer ;
     FrameNum : Integer ;    // Source frame counter

     FrameWidth : Cardinal ;
     FrameHeight : Cardinal ;
     NumFrames : Cardinal ;
     NumPixelsPerFrame : Cardinal ;
     NumComponentsPerPixel : Cardinal ;
     PixelDepth : Cardinal ;
     NumComponentsPerFrame : Cardinal ;

     XResolution : Single ;       // Pixel width
     YResolution : Single ;       // Pixel height
     PixelUnits : string ;       // Pixel width units

     OK : Boolean ;

     PNumBuf : PIntArray ;         // Numerator frame buffer pointer
     PDenBuf : PIntArray ;         // Denominator frame buffer pointer
     Runs : PRuns ;                 // pointer to pixel runs array
     NumRuns : Integer ;            // No. of pixel runs available
     iSubROI : Integer ;            // ROI # to be subtracted
     SubRuns : PRuns ;              // pointer to subtraction pixel runs array
     NumSubRuns : Integer ;            // No. of subtraction pixel runs available

     Numer : Single ;               // Numerator value
     Denom : Single ;               // Denominator value
     x : Single ;
     y : Single ;
     GraphFrmNum : Integer ;
begin

     // Source of image stacks for numerator of ratio
     iNum := Integer(cbNumFile.Items.Objects[cbNumFile.ItemIndex]) ;
     // Source of image stacks for denominator of ratio
     iDen := Integer(cbNumFile.Items.Objects[cbNumFile.ItemIndex]) ;

     // ROI measurements to be plotted
     XVar := Integer(cbXAxisVar.Items.Objects[cbXAxisVar.ItemIndex]) ;
     YVar := Integer(cbYAxisVar.Items.Objects[cbYAxisVar.ItemIndex]) ;

     // Get range of stacks/frames to be plotted
     if rbAllFrames.Checked then begin
        StartAt := 1 ;
        EndAt := Round(edRange.HiLimit) ;
        end
     else begin
        StartAt := Round(edRange.LoValue) ;
        EndAt := Round(edRange.HiValue) ;
        end ;

     // Get range of frames to be plotted

     if MainFrm.ViewFrms[iNum].Display4DMode then begin
        // 4D (stack series) data file
        StartFrame := (StartAt-1)*MainFrm.ViewFrms[iNum].NumSectionsPerStack ;
        EndFrame := (EndAt-1)*MainFrm.ViewFrms[iNum].NumSectionsPerStack ;
        FrameStep := MainFrm.ViewFrms[iNum].NumSectionsPerStack ;
        end
     else begin
        // 3D (frame series) data file
        StartFrame := StartAt ;
        EndFrame := EndAt ;
        FrameStep := 1 ;
        end ;


     // Get image properties

     OK := True ;
     if (iNum = NoFileSelected) then begin
        MessageDlg( 'PLOT ROI: Numerator source file required!', mtError, [mbOK], 0 ) ;
        Exit ;
        end ;

     if (iDen = NoFileSelected) then begin
        MessageDlg( 'PLOT ROI: Denominator source file required!', mtError, [mbOK], 0 ) ;
        Exit ;
        end ;


     FrameWidth := MainFrm.ViewFrms[iNum].FrameWidth ;
     FrameHeight := MainFrm.ViewFrms[iNum].FrameHeight ;
     NumFrames := MainFrm.ViewFrms[iNum].NumFrames ;
     XResolution := MainFrm.ViewFrms[iNum].XResolution ;
     if (FrameWidth <> MainFrm.ViewFrms[iDen].FrameWidth ) or
        (FrameHeight <> MainFrm.ViewFrms[iDen].FrameHeight ) then begin
        MessageDlg( 'PLOT ROI: Files must contain images of the same size!', mtError, [mbOK], 0 ) ;
        Exit ;
        end ;
     if (NumFrames <> MainFrm.ViewFrms[iDen].NumFrames ) then begin
        MessageDlg( 'PLOT ROI: Files must contain same number of images!', mtError, [mbOK], 0 ) ;
        Exit ;
        end ;

     // Create graph plot form
     GraphFrmNum := MainFrm.CreateNewGraphFrm( 'Plot(ratio): ' + MainFrm.ViewFrms[iNum].FileName ) ;

     NumPixelsPerFrame := FrameWidth*FrameHeight ;
     NumComponentsPerPixel := 1 ;
     PixelDepth := 16 ;
     NumComponentsPerFrame := NumPixelsPerFrame*NumComponentsPerPixel ;


     // Allocate storage
     GetMem( PNumBuf, NumPixelsPerFrame*4 ) ;
     GetMem( PDenBuf, NumPixelsPerFrame*4 ) ;
     GetMem( Runs, (NumPixelsPerFrame div 2)*SizeOf(TRunElement) ) ;
     GetMem( SubRuns, (NumPixelsPerFrame div 2)*SizeOf(TRunElement) ) ;

     try

        // Get subtraction ROI (if one is defined)
        iSubROI := Integer(cbSubtractROI.Items.Objects[cbSubtractROI.ItemIndex]) ;
        if (iSubROI >= 1) and (iSubROI <= MaxROIs) then begin
           MainFrm.ViewFrms[iNum].GetROIPixelRuns( iSubROI, SubRuns^, NumSubRuns ) ;
           end
        else NumSubRuns := 0 ;

        for iROI := 1 to MaxROIs do
           if PlotROI(iROI) and (iROI <> iSubROI) then begin

           // Set X and Y axis units
           case xVar of
                 vFrameNum,vMean,vStDev,vMin,vMax : MainFrm.GraphFrms[GraphFrmNum].XUnits := '' ;
                 vTime : MainFrm.GraphFrms[GraphFrmNum].XUnits := 's' ;
                 vZDepth : MainFrm.GraphFrms[GraphFrmNum].XUnits := MainFrm.ViewFrms[iNum].PixelUnits ;
                 end ;
           case yVar of
                 vFrameNum,vMean,vStDev,vMin,vMax : MainFrm.GraphFrms[GraphFrmNum].YUnits := '' ;
                 vTime : MainFrm.GraphFrms[GraphFrmNum].YUnits := 's' ;
                 vZDepth : MainFrm.GraphFrms[GraphFrmNum].YUnits := MainFrm.ViewFrms[iNum].PixelUnits ;
                 end ;

            // Create line on graph plot
            MainFrm.GraphFrms[GraphFrmNum].CreateLine( iROI ) ;
            
            // Set axes labels
            MainFrm.GraphFrms[GraphFrmNum].XAxisLabel := cbXAxisVar.Text + ' '
                                                         + MainFrm.GraphFrms[GraphFrmNum].XUnits ;
            MainFrm.GraphFrms[GraphFrmNum].YAxisLabel := cbYAxisVar.Text + ' '
                                                         + MainFrm.GraphFrms[GraphFrmNum].YUnits ;


            // Get pixel runs within ROI
            MainFrm.ViewFrms[iNum].GetROIPixelRuns( iROI, Runs^, NumRuns ) ;

            FrameNum := StartFrame ;
            while FrameNum <= EndFrame do begin

                // Load numerator & denominator frames
                MainFrm.ViewFrms[iNum].LoadFrame( FrameNum + Round(edNumSection.Value), PNumBuf ) ;
                MainFrm.ViewFrms[iDen].LoadFrame( FrameNum + Round(edDenSection.Value), PDenBuf ) ;

                // X axis variable
                case xVar of
                     vFrameNum : x := FrameNum  + Round(edNumSection.Value) ;
                     vTime : x := (FrameNum-1)*MainFrm.ViewFrms[iNum].TResolution ;
                     vZDepth : x := ((FrameNum-1) mod MainFrm.ViewFrms[iNum].NumSectionsPerStack)
                                    *MainFrm.ViewFrms[iNum].ZResolution ;

                     vMean : begin
                         Numer := MeanIntensity( Runs^, NumRuns, FrameWidth, PNumBuf^ ) ;
                         Denom := MeanIntensity( Runs^, NumRuns, FrameWidth, PDenBuf^ ) ;
                         if Denom <> 0.0 then x := Numer / Denom ;
                         end ;

                     vVariance : begin
                         Numer := VarianceIntensity( Runs^, NumRuns, FrameWidth, PNumBuf^ ) ;
                         Denom := VarianceIntensity( Runs^, NumRuns, FrameWidth, PDenBuf^ ) ;
                         if Denom <> 0.0 then x := Numer / Denom ;
                         end ;

                     vStDev : begin
                         Numer := VarianceIntensity( Runs^, NumRuns, FrameWidth, PNumBuf^ ) ;
                         Denom := VarianceIntensity( Runs^, NumRuns, FrameWidth, PDenBuf^ ) ;
                         if Denom <> 0.0 then x := Sqrt( Numer / Denom ) ;
                         end ;

                     vMax : begin
                         Numer := MaxIntensity( Runs^, NumRuns, FrameWidth, PNumBuf^ ) ;
                         Denom := MaxIntensity( Runs^, NumRuns, FrameWidth, PDenBuf^ ) ;
                         if Denom <> 0.0 then x := Numer / Denom ;
                         end ;

                     vMin : begin
                         Numer := MinIntensity( Runs^, NumRuns, FrameWidth, PNumBuf^ ) ;
                         Denom := MinIntensity( Runs^, NumRuns, FrameWidth, PDenBuf^ ) ;
                         if Denom <> 0.0 then x := Numer / Denom ;
                         end ;

                     end ;

                // Y axis variable
                case yVar of

                     vFrameNum : y := FrameNum  + Round(edNumSection.Value) ;
                     vTime : y := (FrameNum-1)*MainFrm.ViewFrms[iNum].TResolution ;
                     vZDepth : y := ((FrameNum-1) mod MainFrm.ViewFrms[iNum].NumSectionsPerStack)
                                    *MainFrm.ViewFrms[iNum].ZResolution ;


                     vMean : begin
                         Numer := MeanIntensity( Runs^, NumRuns, FrameWidth, PNumBuf^ ) ;
                         Denom := MeanIntensity( Runs^, NumRuns, FrameWidth, PDenBuf^ ) ;
                          if NumSubRuns > 0 then begin
                             Numer := Numer - MeanIntensity( SubRuns^, NumSubRuns, FrameWidth, PNumBuf^ ) ;
                             Denom := Denom - MeanIntensity( SubRuns^, NumSubRuns, FrameWidth, PDenBuf^ ) ;
                             end ;
                         if Denom <> 0.0 then y := Numer / Denom ;
                          end ;

                     vVariance : begin
                         Numer := VarianceIntensity( Runs^, NumRuns, FrameWidth, PNumBuf^ ) ;
                         Denom := VarianceIntensity( Runs^, NumRuns, FrameWidth, PDenBuf^ ) ;
                          if NumSubRuns > 0 then begin
                             Numer := Numer - VarianceIntensity( SubRuns^, NumSubRuns, FrameWidth, PNumBuf^ ) ;
                             Denom := Denom - VarianceIntensity( SubRuns^, NumSubRuns, FrameWidth, PDenBuf^ ) ;
                             end ;
                         if Denom <> 0.0 then y := Numer / Denom ;
                          end ;

                     vStDev : begin
                         Numer := VarianceIntensity( Runs^, NumRuns, FrameWidth, PNumBuf^ ) ;
                         Denom := VarianceIntensity( Runs^, NumRuns, FrameWidth, PDenBuf^ ) ;
                         if NumSubRuns > 0 then begin
                             Numer := Numer - VarianceIntensity( SubRuns^, NumSubRuns, FrameWidth, PNumBuf^ ) ;
                             Denom := Denom - VarianceIntensity( SubRuns^, NumSubRuns, FrameWidth, PDenBuf^ ) ;
                             end ;
                         if Denom <> 0.0 then y := Sqrt( Numer / Denom ) ;
                         end ;

                     vMin : begin
                         Numer := MinIntensity( Runs^, NumRuns, FrameWidth, PNumBuf^ ) ;
                         Denom := MinIntensity( Runs^, NumRuns, FrameWidth, PDenBuf^ ) ;
                         if NumSubRuns > 0 then begin
                             Numer := Numer - MinIntensity( SubRuns^, NumSubRuns, FrameWidth, PNumBuf^ ) ;
                             Denom := Denom - MinIntensity( SubRuns^, NumSubRuns, FrameWidth, PDenBuf^ ) ;
                             end ;
                         if Denom <> 0.0 then y := Numer / Denom ;
                         end ;

                     vMax : begin
                         Numer := MaxIntensity( Runs^, NumRuns, FrameWidth, PNumBuf^ ) ;
                         Denom := MaxIntensity( Runs^, NumRuns, FrameWidth, PDenBuf^ ) ;
                         if NumSubRuns > 0 then begin
                             Numer := Numer - MaxIntensity( SubRuns^, NumSubRuns, FrameWidth,PNumBuf^ ) ;
                             Denom := Denom - MaxIntensity( SubRuns^, NumSubRuns, FrameWidth, PDenBuf^ ) ;
                             end ;
                         if Denom <> 0.0 then y := Numer / Denom ;
                         end ;

                     end ;

                // Add point to line
                if Denom <> 0.0 then MainFrm.GraphFrms[GraphFrmNum].AddPoint( iROI, x, y ) ;

                // Report progress
                MainFrm.StatusBar.SimpleText := format(
                'PLOT: Frame %d/%d',
                [FrameNum,NumFrames] ) ;

                FrameNum := FrameNum + FrameStep ;

                end ;

            end ;

        // Set graph cursors to default positions
        MainFrm.GraphFrms[GraphFrmNum].InitialiseCursors ;

        MainFrm.StatusBar.SimpleText := format(
        'PLOT: File: %s created',
        [FileName] ) ;

     finally
        FreeMem( PNumBuf ) ;
        FreeMem( PDenBuf ) ;
        FreeMem( Runs ) ;
        FreeMem( SubRuns ) ;
        end ;

     end ;


function TPlotROIFrm.PlotROI(
         iROI : Integer        // Region of interest #
         ) : Boolean ;
// -----------------------------------
// Return ROI plotting checkbox status
// -----------------------------------
begin
    case iROI of
        1 : Result := ckROI1.Visible and ckROI1.Checked ;
        2 : Result := ckROI2.Visible and ckROI2.Checked ;
        3 : Result := ckROI3.Visible and ckROI3.Checked ;
        4 : Result := ckROI4.Visible and ckROI4.Checked ;
        5 : Result := ckROI5.Visible and ckROI5.Checked ;
        6 : Result := ckROI6.Visible and ckROI6.Checked ;
        7 : Result := ckROI7.Visible and ckROI7.Checked ;
        8 : Result := ckROI8.Visible and ckROI8.Checked ;
        else Result := False ;
        end ;
    end ;


function TPlotROIFrm.MeanIntensity(
          var Runs : Array of TRunElement ; // Pixel runs defining region
          NumRuns : Integer ;           // No. of pixel runs
          FrameWidth : Integer ;        // Line width of image
          var Buf : Array of Integer    // Image buffer
          ) : Single ;
// ------------------------------------------------------------
// Calculate mean pixel intensity within region defined by Runs
// ------------------------------------------------------------
var
    iRun : Integer ;
    i : Integer ;
    iStart : Integer ;
    NumPixels : Integer ;
    Sum : Single ;
begin

    Sum := 0.0 ;
    NumPixels := 0 ;
    for iRun := 0 to NumRuns-1 do begin
        iStart := Runs[iRun].y*FrameWidth ;
        for i := Runs[iRun].x to Runs[iRun].x + Runs[iRun].Count - 1 do Sum := Sum + Buf[i+iStart] ;
        NumPixels := NumPixels + Runs[iRun].Count ;
        end ;
    if NumPixels > 0 then Result := Sum / NumPixels
                     else Result := 0.0 ;
    end ;


function TPlotROIFrm.VarianceIntensity(
          var Runs : Array of TRunElement ; // Pixel runs defining region
          NumRuns : Integer ;           // No. of pixel runs
          FrameWidth : Integer ;        // Line width of image
          var Buf : Array of Integer    // Image buffer
          ) : Single ;
// ------------------------------------------------------------
// Calculate variance of pixel intensity within region defined by Runs
// ------------------------------------------------------------
var
    iRun : Integer ;
    i : Integer ;
    iStart : Integer ;
    NumPixels : Integer ;
    Sum : Single ;
    Mean : Single ;
    Residual : Single ;
begin

    // Calculate mean
    Sum := 0.0 ;
    NumPixels := 0 ;
    for iRun := 0 to NumRuns-1 do begin
        iStart := Runs[iRun].y*FrameWidth ;
        for i := Runs[iRun].x to Runs[iRun].x + Runs[iRun].Count - 1 do Sum := Sum + Buf[i+iStart] ;
        NumPixels := NumPixels + Runs[iRun].Count ;
        end ;

    if NumPixels < 2 then begin
       Result := 0.0 ;
       Exit ;
       end ;

    // Calculate variance
    Sum := 0.0 ;
    for iRun := 0 to NumRuns-1 do begin
        iStart := Runs[iRun].y*FrameWidth ;
        for i := Runs[iRun].x to Runs[iRun].x + Runs[iRun].Count - 1 do begin
            Residual := Buf[i+iStart] - Mean ;
            Sum := Sum + Residual*Residual ;
            end ;
        end ;
    Result := Sum / (NumPixels - 1) ;

    end ;


function TPlotROIFrm.MaxIntensity(
          var Runs : Array of TRunElement ; // Pixel runs defining region
          NumRuns : Integer ;               // No. of pixel runs
          FrameWidth : Integer ;            // Line width of image
          var Buf : Array of Integer        // Image buffer
          ) : Single ;
// ------------------------------------------------------------
// Calculate maximum pixel intensity within region defined by Runs
// ------------------------------------------------------------
var
    iRun : Integer ;
    i : Integer ;
    iStart : Integer ;
    IMax : Integer ;
begin

    IMax := Low(IMax) ;
    for iRun := 0 to NumRuns-1 do begin
        iStart := Runs[iRun].y*FrameWidth ;
        for i := Runs[iRun].x to Runs[iRun].x + Runs[iRun].Count - 1 do
            if IMax < Buf[i+iStart] then IMax := Buf[i+iStart];
        end ;
    Result := IMax ;
    end ;


function TPlotROIFrm.MinIntensity(
          var Runs : Array of TRunElement ; // Pixel runs defining region
          NumRuns : Integer ;               // No. of pixel runs
          FrameWidth : Integer ;            // Line width of image
          var Buf : Array of Integer        // Image buffer
          ) : Single ;
// ------------------------------------------------------------
// Calculate minimum pixel intensity within region defined by Runs
// ------------------------------------------------------------
var
    iRun : Integer ;
    i : Integer ;
    iStart : Integer ;
    IMin : Integer ;
begin

    IMin := High(IMin) ;
    for iRun := 0 to NumRuns-1 do begin
        iStart := Runs[iRun].y*FrameWidth ;
        for i := Runs[iRun].x to Runs[iRun].x + Runs[iRun].Count - 1 do
            if IMin > Buf[i+iStart] then IMin := Buf[i+iStart];
        end ;
    Result := IMin ;
    end ;


procedure TPlotROIFrm.bOKClick(Sender: TObject);
// -----------------
// OK button pressed
// -----------------
begin
     if ckRatio.Checked then PlotROIRatioMeasurements
                        else PlotROIMeasurements;
     end;


procedure TPlotROIFrm.ckRatioClick(Sender: TObject);
// -----------------------
// Ratio check box clicked
// -----------------------
begin
    RatioPanel.Visible := ckRatio.Checked ;
    UpdateOK ;
    end;


procedure TPlotROIFrm.ckROI1Click(Sender: TObject);
// ---------------------
// ROI selection changed
// ---------------------
begin
     UpdateOK ;
     end;

procedure TPlotROIFrm.cbDenFileChange(Sender: TObject);
// ------------------------
// Denominator file changed
// ------------------------
begin
     // Enable OK button if plot possible
     UpdateOK ;
     end;

end.
