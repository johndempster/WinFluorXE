unit EditROIUnit;
// ------------------------------
// Edit location and size of ROIs
// ------------------------------
// 17/9/9 JD Polygon and Polyline ROIs now displayed
//           Error when no ROIs defined fixed
// 26/7/11 JD Now displays 50th ROI when defined

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, IDRFile, shared ;

type
  TEditROIFrm = class(TForm)
    sgTable: TStringGrid;
    bOK: TButton;
    bCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditROIFrm: TEditROIFrm;

implementation

uses Main;

{$R *.dfm}

const
    NumCol = 0 ;
    ShapeCol = 1 ;
    XCol = 2 ;
    YCol = 3 ;
    WidthCol = 4 ;
    HeightCol = 5 ;

procedure TEditROIFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
    i,NumROis : Integer ;
begin
     NumROIs := 0 ;
     for i := 1 to MainFrm.IDRFile.MaxROI-1 do
         if MainFrm.IDRFile.ROI[i].InUse then Inc(NumROIs) ;
     if NumROIs <= 0 then begin
        //Hide ;
        Exit ;
        end ;

     // Set no. of rows in table
     sgTable.RowCount := 1 ;
     for i := 1 to MainFrm.IDRFile.MaxROI do
         if MainFrm.IDRFile.ROI[i].InUse then
            sgTable.RowCount := sgTable.RowCount + 1 ;
     sgTable.FixedRows := 1 ;

     // Column labels
     sgTable.Cells[ShapeCol,0] := 'Shape ' ;
     sgTable.Cells[XCol,0] := ' Centre (X) ' ;
     sgTable.Cells[YCol,0] := ' Centre (Y) ' ;
     sgTable.Cells[WidthCol,0] := ' Width ' ;
     sgTable.Cells[HeightCol,0] := ' Height ' ;

     // Column widths
     sgTable.ColWidths[NumCol] := sgTable.Canvas.TextWidth('ROI.X     ') ;
     sgTable.ColWidths[ShapeCol] := sgTable.Canvas.TextWidth(' Shape      ') ;
     sgTable.ColWidths[XCol] := sgTable.Canvas.TextWidth(' Centre (X)    ') ;
     sgTable.ColWidths[YCol] := sgTable.ColWidths[XCol] ;
     sgTable.ColWidths[WidthCol] := sgTable.ColWidths[XCol] ;
     sgTable.ColWidths[HeightCol] := sgTable.ColWidths[XCol] ;

     // Display ROIs in use
     for i := 1 to MainFrm.IDRFile.MaxROI do
       if MainFrm.IDRFile.ROI[i].InUse then begin
          sgTable.Cells[NumCol,i] := format('ROI.%d',[i]) ;
          case MainFrm.IDRFile.ROI[i].Shape of
              PointROI : sgTable.Cells[ShapeCol,i] := 'Point' ;
              RectangleROI : sgTable.Cells[ShapeCol,i] := 'Rect ' ;
              EllipseROI : sgTable.Cells[ShapeCol,i] := 'Ellip ' ;
              LineROI : sgTable.Cells[ShapeCol,i] := 'Line' ;
              PolylineROI : sgTable.Cells[ShapeCol,i] := 'Polyline' ;
              PolygonROI : sgTable.Cells[ShapeCol,i] := 'Polygon' ;
              end ;
          sgTable.Cells[XCol,i] := format('%d',[MainFrm.IDRFile.ROI[i].Centre.X]) ;
          sgTable.Cells[YCol,i] := format('%d',[MainFrm.IDRFile.ROI[i].Centre.Y]) ;
          sgTable.Cells[WidthCol,i] := format('%d',[MainFrm.IDRFile.ROI[i].Width]) ;
          sgTable.Cells[HeightCol,i] := format('%d',[MainFrm.IDRFile.ROI[i].Height]) ;
          end ;

     Resize ;

     end;


procedure TEditROIFrm.bOKClick(Sender: TObject);
// --------------------------
// OK pressed ... Update ROIs
// --------------------------
var
    i,ROINum : Integer ;
    ROI : TROI ;
begin

     // Display ROIs in use
     for i := 1 to sgTable.RowCount-1 do begin

         // Get ROI #
         ROINum := ExtractInt(sgTable.Cells[NumCol,i]) ;

         // Get ROI
         ROI := MainFrm.IDRFile.ROI[ROINum] ;

         case ROI.Shape of
            LineROI,RectangleROI,EllipseROI,PointROI : begin

               ROI.Centre.X := ExtractInt(sgTable.Cells[XCol,i]) ;
               ROI.Centre.Y := ExtractInt(sgTable.Cells[YCol,i]) ;
               ROI.Width := ExtractInt(sgTable.Cells[WidthCol,i]) ;
               ROI.Height := ExtractInt(sgTable.Cells[HeightCol,i]) ;

               ROI.TopLeft.X := ROI.Centre.X - (ROI.Width div 2) ;
               ROI.BottomRight.X := ROI.TopLeft.X + ROI.Width -1 ;
               ROI.TopLeft.Y := ROI.Centre.Y - (ROI.Height div 2) ;
               ROI.BottomRight.Y := ROI.TopLeft.Y + ROI.Height -1 ;

               // Update ROI in file headet
               MainFrm.IDRFile.ROI[ROINum] := ROI ;
               end ;
            end ;

          end ;

     Hide ;

     end;

procedure TEditROIFrm.FormResize(Sender: TObject);
begin
     sgTable.Width := ClientWidth - sgTable.Left - 5 ;

     bOK.Top := ClientHeight - bOK.Height - 5 ;
     bCancel.Top := bOK.Top ;
     sgTable.Height := bOK.Top - sgTable.Top - 5 ;

     end;

procedure TEditROIFrm.bCancelClick(Sender: TObject);
begin
    Hide ;
    end;

end.
