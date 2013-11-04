unit PlotSetaxes;
{ -------------------------------------------------------------------
  WinFluor - Dialog box for manual setting of X/Y plot axes/style
  -------------------------------------------------------------------
  15/12/98 ... Now include histogram style options }

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, Spin, sysutils, shared, ExtCtrls, Dialogs,
  ValEdit, XYPlotDisplay, ValidatedEdit ;

type
  TPlotSetAxesFrm = class(TForm)
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
    bOK: TButton;
    bCancel: TButton;
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
    TraceGrp: TGroupBox;
    ckMarkers: TCheckBox;
    ckLines: TCheckBox;
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
    procedure bOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbXAutoRangeClick(Sender: TObject);
    procedure rbXManualClick(Sender: TObject);
    procedure rbYAutoRangeClick(Sender: TObject);
    procedure rbYManualClick(Sender: TObject);
    procedure ColorBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    Plot : TXYPlotDisplay ;
    Histogram : Boolean ;
  end;

var
  PlotSetAxesFrm: TPlotSetAxesFrm;

implementation

{$R *.DFM}


procedure TPlotSetAxesFrm.FormShow(Sender: TObject);
{ -------------------
  Initialise controls
  -------------------}
begin

     { Make appropriate special options page visible }
     if Histogram then nbPlotType.PageIndex := 1
                  else nbPlotType.PageIndex := 0 ;

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

          ckLines.checked := ShowLines ;
          ckMarkers.checked := ShowMarkers ;

          cbFillStyle.Clear ;
          cbFillStyle.Items.AddObject( ' Empty ', TObject(bsClear) ) ;
          cbFillStyle.Items.AddObject( ' Solid ', TObject(bsSolid) ) ;
          cbFillStyle.Items.AddObject( ' Hatched ', TObject(bsBDiagonal) ) ;
          cbFillStyle.ItemIndex := cbFillStyle.Items.IndexOfObject(
                                   TObject(HistogramFillStyle)) ;

          { Bin fill colour }
          ColorBox.brush.color := HistogramFillColor ;
          ckBorders.checked := HistogramFullBorders ;
          end ;

     end;



procedure TPlotSetAxesFrm.bOKClick(Sender: TObject);
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
     end;


procedure TPlotSetAxesFrm.rbXAutoRangeClick(Sender: TObject);
begin
     edXMin.Enabled := False ;
     edXMax.Enabled := False ;
     edXTick.Enabled := False ;
     end;

procedure TPlotSetAxesFrm.rbXManualClick(Sender: TObject);
begin
     edXMin.Enabled := True ;
     edXMax.Enabled := True ;
     edXTick.Enabled := True ;
     end ;

procedure TPlotSetAxesFrm.rbYAutoRangeClick(Sender: TObject);
begin
     edYMin.Enabled := False ;
     edYMax.Enabled := False ;
     edYTick.Enabled := False ;
     end;

procedure TPlotSetAxesFrm.rbYManualClick(Sender: TObject);
begin
     edYMin.Enabled := True ;
     edYMax.Enabled := True ;
     edYTick.Enabled := True ;
     end;

procedure TPlotSetAxesFrm.ColorBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     ColorDialog.Execute ;
     ColorBox.Brush.Color := ColorDialog.Color ;
     end;

end.
