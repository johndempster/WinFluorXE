unit ImageUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, RangeEdit, ComCtrls;

type
  TImageFrm = class(TForm)
    ControlGrp: TGroupBox;
    DisplayGrp: TGroupBox;
    Label6: TLabel;
    cbDisplayZoom: TComboBox;
    cbPalette: TComboBox;
    ContrastPage: TPageControl;
    RangeTab: TTabSheet;
    bFullScale: TButton;
    bMaxContrast: TButton;
    edDisplayIntensityRange: TRangeEdit;
    ckContrast6SDOnly: TCheckBox;
    SlidersTab: TTabSheet;
    Label5: TLabel;
    Label10: TLabel;
    sbContrast: TScrollBar;
    sbBrightness: TScrollBar;
    SnapGrp: TGroupBox;
    bSnapImage: TButton;
    ImageGrp: TGroupBox;
    Image1: TImage;
    sbXScroll: TScrollBar;
    sbYScroll: TScrollBar;
    ROIPanel: TPanel;
    lbReadout: TLabel;
    ckDisplayCalBar: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImageFrm: TImageFrm;

implementation

{$R *.dfm}

procedure TImageFrm.FormCreate(Sender: TObject);
// ------------------------------------
// Initialisations when form is created
// ------------------------------------
var
     i : Integer ;
     Device : SmallInt ;
begin

     // Displayed image storage buffer pointers
     PDisplayBuf := Nil ;
     PWorkBuf := Nil ;

     // Internal image bitmaps
     BitMap := Nil ;

     end;

procedure TImageFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
begin

     FirstResize := True ;
     InitialisationComplete := False ;
     FormClosing := False ;

     // Set form at top left of MDI window
     Top := 20 ;
     Left := 20 ;


     // Set limits of display intensity range
     SelectedFrameType := 0 ;
     edDisplayIntensityRange.LoLimit := 0 ;
     edDisplayIntensityRange.HiLimit := MainFrm.Cam1.GreyLevelMax ;

     // Set brightness & contrast slider range and position
     sbContrast.Min := 0 ;
     sbContrast.Max := MainFrm.Cam1.GreyLevelMax ;
     sbContrast.SmallChange := 1 ;
     sbContrast.LargeChange := Max(sbContrast.Max div 50,1) ;
     sbBrightness.Min := 0 ;
     sbBrightness.Max := MainFrm.Cam1.GreyLevelMax ;
     sbBrightness.LargeChange := Max(sbBrightness.Max div 50,1) ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[SelectedFrameType],
                               MainFrm.GreyHi[SelectedFrameType] ) ;

    // Number of excitation wavelengths in use
    // (Each displayed as a separate image)
    NumFrameTypes := 1 ;

     // Intensity display palette
     cbPalette.Clear ;
     cbPalette.Items.AddObject(' Grey scale', TObject(palGrey)) ;
     cbPalette.Items.AddObject(' False colour', TObject(palFalseColor)) ;
     cbPalette.Items.AddObject(' Red scale', TObject(palRed)) ;
     cbPalette.Items.AddObject(' Green scale', TObject(palGreen)) ;
     cbPalette.Items.AddObject(' Blue scale', TObject(palBlue)) ;
     cbPalette.ItemIndex := cbPalette.Items.IndexOfObject(TObject(MainFrm.PaletteType)) ;

     // Display magnification factor
     cbDisplayZoom.Clear ;
     cbDisplayZoom.Items.AddObject( '  25% ', Tobject(25) ) ;
     cbDisplayZoom.Items.AddObject( '  50% ', Tobject(50) ) ;
     cbDisplayZoom.Items.AddObject( ' 100% ', Tobject(100)) ;
     cbDisplayZoom.Items.AddObject( ' 200% ', Tobject(200)) ;
     cbDisplayZoom.Items.AddObject( ' 300% ', Tobject(300)) ;
     cbDisplayZoom.Items.AddObject( ' 400% ', Tobject(400)) ;
     cbDisplayZoom.Items.AddObject( ' 500% ', Tobject(500)) ;
     cbDisplayZoom.Items.AddObject( ' 600% ', Tobject(600)) ;
     cbDisplayZoom.Items.AddObject( ' 700% ', Tobject(700)) ;
     cbDisplayZoom.Items.AddObject( ' 800% ', Tobject(800)) ;

     cbDisplayZoom.ItemIndex := Max(MainFrm.DisplayZoomIndex,0) ;
     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])*0.01 ;

     sbXScroll.Position := 0 ;
     sbYScroll.Position := 0 ;

     ImageAvailable := False ;
     MoveMode := mvNone ;
     OptimiseContrastNeeded := True ;

     InitialisationComplete := True ;

     ClientHeight := ControlGrp.Top + ControlGrp.Height + 5 ;

     end;


procedure TImageFrm.InitialiseImage ;
// ------------------------------------------------------
// Re-initialise size of memory buffers and image bitmaps
// ------------------------------------------------------
var
     i : Integer ;
     MaxBuffers : Integer ;
begin

    // No. of pixels per frame
    FrameWidth := MainFrm.Cam1.FrameWidth ;
    FrameHeight := MainFrm.Cam1.FrameHeight ;
    NumPixelsPerFrame := FrameWidth*FrameHeight ;

    // Number of excitation wavelengths in use
    NumFrameTypes := 1 ;

     // Dispose of existing display buffers and create new ones
     if PDisplayBuf <> Nil then begin
         Try
           FreeMem(PDisplayBuf) ;
           PDisplayBuf := Nil ;
         except
           outputdebugString(PChar('Error FreeMem(PDisplayBufs[i]')) ;
           PDisplayBuf := Nil ;
           end ;
         end ;
     GetMem( PDisplayBuf,NumPixelsPerFrame*SizeOf(Integer) ) ;

     // Create work buffer

     try
     if PWorkBuf <> Nil then FreeMem( PWorkBuf ) ;
     except
           outputdebugString(PChar('Error FreeMem( PWorkBuf )')) ;
           PWorkBuf := Nil ;
           end ;
     GetMem( PWorkBuf, NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel ) ;
     // Set size and location of image display panels
     SetImagePanels ;

     CCDAreaGrp.Caption := format( ' CCD Area (%d x %d) ',
                         [MainFrm.Cam1.FrameWidth,MainFrm.Cam1.FrameHeight] ) ;

     // Create display labels for each frame
     if MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].Width >= 0.0 then begin
        FrameTypes[0] := format('%d (%d)',
                         [MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].Centre,
                          MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].Width]) ;
        end
     else begin
       FrameTypes[0] := format('%dL',
                       [MainFrm.EXCWavelengths[MainFrm.EXCSingleWavelengthNum].Centre]) ;
        end ;

     // Indicate selected frame type selected for contrast update
     DisplayGrp.Caption := ' Contrast ' + FrameTypes[0] + ' ' ;

     // Set intensity range and sliders
     SetDisplayIntensityRange( MainFrm.GreyLo[0],MainFrm.GreyHi[0] ) ;

     // Update display look up tables
     MainFrm.UpdateLUT( 0, MainFrm.Cam1.GreyLevelMax );

     // Determine number of frame within circular buffer

     NumPixelsPerFrame := MainFrm.Cam1.FrameWidth*MainFrm.Cam1.FrameHeight ;
     NumBytesPerFrame := NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel ;

     MainFrm.Cam1.GetFrameBufferPointer( PFrameBuf ) ;

     // Set byte/word image flag
     NumBytesPerPixel := MainFrm.Cam1.NumBytesPerPixel ;
     if MainFrm.Cam1.NumBytesPerPixel = 1 then ByteImage := True
                                          else ByteImage := False ;

     end ;


procedure TImageFrm.SetImagePanels ;
// -------------------------------------------
// Set size and number of image display panels
// -------------------------------------------
const
    MarginPixels = 16 ;
var
     ch,i : Integer ;
     ROI : TROI ;
    ImageAreaHeight : Integer ;
    ImageAreaWidth : Integer ;
    ImageColumns : Integer ;
    ImageRows : Integer ;
    RightEdge : Integer ;
    BottomEdge : Integer ;
begin

     // Dispose of existing bit maps
     if BitMap <> Nil then begin
         BitMap.Free  ;
         BitMap := Nil ;
         end ;
     BitMap := TBitMap.Create ;

     // Set size and pen/brush characteristics of images in use

     DisplayZoom := Integer(cbDisplayZoom.Items.Objects[cbDisplayZoom.ItemIndex])*0.01 ;

     // Determine number of image columns and rows
     ImageRows := 1 ;
     ImageColumns := 1 ;

     ImageGrp.ClientWidth :=  Max( ClientWidth - ImageGrp.Left - 5, 2) ;
     ImageGrp.ClientHeight :=  Max( ClientHeight - ImageGrp.Top - 5, 2) ;

     ROIPanel.Top := ImageGrp.ClientHeight - ROIPanel.Height - 2 ;
     ROIPanel.Width := ImageGrp.ClientWidth - ROIPanel.Left - 5 ;

     ImageAreaWidth := Max( ImageGrp.ClientWidth - sbYScroll.Width - (2*MarginPixels),2) ;
     ImageAreaHeight := Max( ROIPanel.Top - sbXScroll.Height - (2*MarginPixels),2) ;

     BitMap.Width := Max(Min( ImageAreaWidth ,
                              Round(MainFrm.Cam1.FrameWidth*DisplayZoom)),2) ;
     BitMap.Height := Max(Min( ImageAreaHeight,
                               Round(MainFrm.Cam1.FrameHeight*DisplayZoom)),2) ;

     RightEdge := 0 ;
     BottomEdge := 0 ;

     MainFrm.SetPalette( BitMap, MainFrm.PaletteType ) ;

     Image1.Width := BitMap.Width ;
     Image1.Height := BitMap.Height ;

     Image1.Left := MarginPixels ;
     Image1.Top := MarginPixels ;

     Image1.Canvas.Pen.Color := clWhite ;
     Image1.Canvas.Brush.Style := bsClear ;
     Image1.Canvas.Font.Color := clWhite ;
     Image1.Canvas.TextFlags := 0 ;
     Image1.Canvas.Pen.Mode := pmXOR ;
     Image1.Canvas.Font.Name := 'Arial' ;
     Image1.Canvas.Font.Size := 8 ;
     Image1.Canvas.Font.Color := clBlue ;

     // Determine right/bottom edge of image area
     RightEdge := Image1.Left + Image1.Width + 1 ;
     BottomEdge := Image1.Top + Image1.Height + 1 ;

     // Position image scroll bars at right and bottom edges of image area
     sbXScroll.Top := BottomEdge ;
     sbXScroll.Left :=  Image1.Left ;
     sbXScroll.Width := RightEdge - sbXScroll.Left ;
     ROIPanel.Top := sbXScroll.Top + sbXScroll.Height + 2 ;

     sbYScroll.Left := RightEdge ;
     sbYScroll.Top := Image1.Top ;
     sbYScroll.Height := BottomEdge - sbYScroll.Top ;

     // Image scroll bar range
     sbXScroll.Max := Max(MainFrm.Cam1.FrameWidth - Round(Image1.Width/DisplayZoom),1);
     sbYScroll.Max := Max(MainFrm.Cam1.FrameHeight - Round(Image1.Height/DisplayZoom),1);

     CaptureRegion.Left := 0 ;
     CaptureRegion.Right := BitMap.Width - 1 ;
     CaptureRegion.Top := 0 ;
     CaptureRegion.Bottom := BitMap.Height - 1 ;
     FMoveCaptureRegion := False ;

     end ;


procedure TSnapFrm.SetDisplayIntensityRange(
          LoValue : Integer ;
          HiValue : Integer
          ) ;
// --------------------------------------
// Set display contrast range and sliders
// --------------------------------------
begin

     edDisplayIntensityRange.LoValue := LoValue  ;
     edDisplayIntensityRange.HiValue := HiValue  ;
     sbBrightness.Position := (LoValue + HiValue) div 2 ;
     sbContrast.Position := HiValue - LoValue ;

     end ;



end.
