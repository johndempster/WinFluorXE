unit Setaxes;
{ -------------------------------------------------------------------
  WinFluor - Dialog box for manual setting of X/Y plot axes/style
  -------------------------------------------------------------------
  15/12/98 ... Now include histogram style options
  22.5.02 .... Modified for use in WinImage
  5/4/05 ..... Axes auto/manual options now set correctly when form displayed}

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, Spin, sysutils, shared, ExtCtrls, Dialogs,
  ValEdit, XMultiYPlot, ValidatedEdit ;

type
  TSetAxesFrm = class(TForm)
    XAxisGrp: TGroupBox;
    lbXMin: TLabel;
    lbxMax: TLabel;
    Label1: TLabel;
    cbXAxisType: TComboBox;
    YAxisGrp: TGroupBox;
    lbYMin: TLabel;
    lbYMax: TLabel;
    Label4: TLabel;
    cbYAxisType: TComboBox;
    bUpdate: TButton;
    LabelsGrp: TGroupBox;
    EdXLabel: TEdit;
    Label5: TLabel;
    edYLabel: TEdit;
    Label6: TLabel;
    rbXAutoRange: TRadioButton;
    rbXManual: TRadioButton;
    rbYAutoRange: TRadioButton;
    rbYManual: TRadioButton;
    nbPlotType: TNotebook;
    GroupBox1: TGroupBox;
    ColorBox: TShape;
    Label8: TLabel;
    ckBorders: TCheckBox;
    cbFillStyle: TComboBox;
    edXMin: TValidatedEdit;
    edXMax: TValidatedEdit;
    edXTick: TValidatedEdit;
    edYMin: TValidatedEdit;
    edYMax: TValidatedEdit;
    edYTick: TValidatedEdit;
    ColorDialog: TColorDialog;
    GroupBox2: TGroupBox;
    cbPlot: TComboBox;
    TraceGrp: TGroupBox;
    ckMarkers: TCheckBox;
    ckLines: TCheckBox;
    bUpdateAll: TButton;
    procedure bUpdateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbXAutoRangeClick(Sender: TObject);
    procedure rbXManualClick(Sender: TObject);
    procedure rbYAutoRangeClick(Sender: TObject);
    procedure rbYManualClick(Sender: TObject);
    procedure ColorBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbPlotChange(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure bUpdateAllClick(Sender: TObject);
  private
    { Private declarations }
    procedure CopyPlotToEditBoxes ;
    procedure CopyEditBoxesToPlot ;
  public
    { Public declarations }
    Plot : TXMultiYPlot ;
    Histogram : Boolean ;
  end;

var
  SetAxesFrm: TSetAxesFrm;

implementation

{$R *.DFM}


procedure TSetAxesFrm.FormShow(Sender: TObject);
{ -------------------
  Initialise controls
  -------------------}
var
     i : Integer ;
begin

     { Make appropriate special options page visible }
     if Histogram then begin
        nbPlotType.PageIndex := 1 ;
        Caption := ' Customise Histogram ' ;
        end
     else begin
        nbPlotType.PageIndex := 0 ;
        Caption := ' Customise Graph ' ;
        end ;

     cbFillStyle.Clear ;
     cbFillStyle.Items.AddObject( ' Empty ', TObject(bsClear) ) ;
     cbFillStyle.Items.AddObject( ' Solid ', TObject(bsSolid) ) ;
     cbFillStyle.Items.AddObject( ' Hatched ', TObject(bsBDiagonal) ) ;

     // List of plots
     cbPlot.Clear ;
     for i := 0 to Plot.NumPlots-1 do begin
         Plot.PlotNum := i ;
         cbPlot.Items.Add(format(' Plot %d (%s)',[i+1,Plot.YAxisLabel])) ;
         end ;
     cbPlot.ItemIndex := 0 ;
     Plot.PlotNum := cbPlot.ItemIndex ;

     // Update edit boxes with current plot axis settings
     CopyPlotToEditBoxes ;

     end;


procedure TSetAxesFrm.CopyPlotToEditBoxes ;
//  --------------------------------------------------
// Update edit boxes with current plot axes settings
// --------------------------------------------------
begin

     with Plot do begin
          edXMin.Value := XAxisMin ;
          edXMax.Value := XAxisMax ;
          edXTick.Value := XAxisTick ;
          edXLabel.Text := XAxisLabel ;

          case XAxisLaw of
               axLinear :  cbXAxisType.itemIndex := 0 ;
               axLog :  cbXAxisType.itemIndex := 1 ;
               axSquareRoot :  cbXAxisType.itemIndex := 2 ;
               end ;
          rbXAutoRange.Checked := XAxisAutoRange ;
          rbXManual.Checked := not XAxisAutoRange ;

          edYMin.Value := YAxisMin ;
          edYMax.Value := YAxisMax ;
          edYTick.Value := YAxisTick ;
          edYLabel.Text := YAxisLabel ;

          case YAxisLaw of
               axLinear :  cbYAxisType.itemIndex := 0 ;
               axLog :  cbYAxisType.itemIndex := 1 ;
               axSquareRoot :  cbYAxisType.itemIndex := 2 ;
               end ;

          rbYAutoRange.Checked := YAxisAutoRange ;
          rbYManual.Checked := not YAxisAutoRange ;

          ckLines.checked := ShowLines ;
          ckMarkers.checked := ShowMarkers ;

          cbFillStyle.ItemIndex := cbFillStyle.Items.IndexOfObject(
                                   TObject(HistogramFillStyle)) ;

          { Bin fill colour }
          ColorBox.brush.color := HistogramFillColor ;
          ckBorders.checked := HistogramFullBorders ;
          end ;
     end;


procedure TSetAxesFrm.CopyEditBoxesToPlot ;
//  -----------------------------------------------------------------
// Update current plot axes settings with new values from edit boxes
// ------------------------------------------------------------------
begin
     with Plot do begin
          XAxisMin := edXMin.Value ;
          XAxisMax  := edXMax.Value ;
          XAxisTick := edXTick.Value ;
          XAxisLabel := edXLabel.Text ;
          XAxisAutoRange := rbXAutoRange.Checked ;

          case cbXAxisType.itemIndex of
               0 : XAxisLaw := axLinear ;
               1 : XAxisLaw := axLog ;
               2 : XAxisLaw := axSquareRoot ;
               end ;

          YAxisMin := edYMin.Value ;
          YAxisMax  := edYMax.Value ;
          YAxisTick := edYTick.Value ;
          YAxisLabel := edYLabel.Text ;
          YAxisAutoRange := rbYAutoRange.Checked ;

          case cbYAxisType.itemIndex of
               0 : YAxisLaw := axLinear ;
               1 : YAxisLaw := axLog ;
               2 : YAxisLaw := axSquareRoot ;
               end ;

          ShowLines := ckLines.checked ;
          ShowMarkers := ckMarkers.checked ;

          HistogramFillStyle := TBrushStyle(
                                cbFillStyle.Items.Objects[cbFillStyle.ItemIndex]) ;

          { Bin fill colour }
          HistogramFillColor := ColorBox.brush.color ;
          HistogramFullBorders := ckBorders.checked ;
          end ;

     end ;


procedure TSetAxesFrm.bUpdateClick(Sender: TObject);
// -----------------------------------------
// Update currently selected plot on display
// -----------------------------------------
begin
     CopyEditBoxesToPlot ;
     end;


procedure TSetAxesFrm.rbXAutoRangeClick(Sender: TObject);
begin
     edXMin.Enabled := False ;
     edXMax.Enabled := False ;
     edXTick.Enabled := False ;
     end;


procedure TSetAxesFrm.rbXManualClick(Sender: TObject);
begin
     edXMin.Enabled := True ;
     edXMax.Enabled := True ;
     edXTick.Enabled := True ;
     end ;

procedure TSetAxesFrm.rbYAutoRangeClick(Sender: TObject);
begin
     edYMin.Enabled := False ;
     edYMax.Enabled := False ;
     edYTick.Enabled := False ;
     end;

procedure TSetAxesFrm.rbYManualClick(Sender: TObject);
begin
     edYMin.Enabled := True ;
     edYMax.Enabled := True ;
     edYTick.Enabled := True ;
     end;

procedure TSetAxesFrm.ColorBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     ColorDialog.Execute ;
     ColorBox.Brush.Color := ColorDialog.Color ;
     end;

procedure TSetAxesFrm.cbPlotChange(Sender: TObject);
// --------------------------------------------------
// Update edit boxes with settings from selected plot
// --------------------------------------------------
begin
     if Plot.PlotNum <> cbPlot.ItemIndex then begin
        Plot.PlotNum := cbPlot.ItemIndex ;
        CopyPlotToEditBoxes ;
        end;
     end ;


procedure TSetAxesFrm.bCloseClick(Sender: TObject);
// --------------------------------------
// Hide form when close button is clicked
// --------------------------------------
begin
     Hide ;
     end;

procedure TSetAxesFrm.bUpdateAllClick(Sender: TObject);
// ------------------
// Update all plots
// ------------------
var
     i : Integer ;
     YLabel : String ;
begin

     // Update current plot
     CopyEditBoxesToPlot ;

     for i := 0 to Plot.NumPlots-1 do begin
         // Select active plot
         Plot.PlotNum := i ;
         // Keep Y axis label
         YLabel := Plot.YAxisLabel ;
         // Update plot with settings from form
         CopyEditBoxesToPlot ;
         // Restore label
         Plot.YAxisLabel := YLabel ;
         end ;
     end;

end.
