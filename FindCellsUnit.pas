unit FindCellsUnit;
// ------------------------------------------------------------------------
// Automatically find location cells within image using intensity threshold
// ------------------------------------------------------------------------
// 16.04.26

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ValidatedEdit, IDRFile, ParticleLocator,
  Vcl.ExtCtrls ;

type
  TFindCellsFrm = class(TForm)
    GroupBox1: TGroupBox;
    cbBackgroundROI: TComboBox;
    lbBackgroundROI: TLabel;
    edThreshold: TValidatedEdit;
    lbThresholdLow: TLabel;
    bFIndCells: TButton;
    bCancel: TButton;
    cbFrameType: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    cbCellROI: TComboBox;
    lbBackgroundIntensity: TLabel;
    lbCellIntensity: TLabel;
    gpCellImits: TGroupBox;
    Label3: TLabel;
    edMinCellArea: TValidatedEdit;
    Label4: TLabel;
    edMaxCellArea: TValidatedEdit;
    rbMicrons: TRadioButton;
    rbPixels: TRadioButton;
    Image1: TImage;
    Label5: TLabel;
    edROISize: TValidatedEdit;
    Label6: TLabel;
    edErosionRadius: TValidatedEdit;
    edStatus: TEdit;
    bSaveAsROIs: TButton;
    procedure FormShow(Sender: TObject);
    procedure bFIndCellsClick(Sender: TObject);
    procedure cbFrameTypeChange(Sender: TObject);
    procedure cbCellROIChange(Sender: TObject);
    procedure cbBackgroundROIChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rbPixelsClick(Sender: TObject);
    procedure rbMicronsClick(Sender: TObject);
    procedure bSaveAsROIsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    pMaxIntensityImage : PIntArray ;               // Max. intensity projection image
    ThresholdedImage : TArray<TArray<UINT16>>;     // Thresholded image
    Particles: TParticleArray;                     // Particles found

   procedure GenerateMaxIntensityImage ;
   procedure DisplayMeanROIIntensities;
   procedure SaveCellsToROIList ;
  public
    { Public declarations }
  end;

var
  FindCellsFrm: TFindCellsFrm;

implementation

uses Main,ViewUnit, ViewPlotUnit, System.Generics.Collections, math ;


{$R *.dfm}


procedure TFindCellsFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
    i,iMin,iMax : Integer ;
    ROI : TROI ;
begin

     // List of frame type available
     cbFrameType.Items.Clear ;
     for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do cbFrameType.Items.Add( MainFrm.IDRFile.FrameType[i] );
     cbFrameType.ItemIndex := 0 ;

     // Delete all area ROIs

     for i := 1 to MainFrm.IDRFile.MaxROI do
         begin
         ROI := MainFrm.IDRFile.ROI[i] ;
         if ROI.Shape = AreaROI then ROI.InUse := False ;
          MainFrm.IDRFile.ROI[i] := ROI ;
         end ;

     // Fill Cell and Background ROI lists with available ROIs

     cbCellROI.Clear ;
     cbCellROI.Items.AddObject(' ',TObject(0)) ;
     for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then
        begin
        cbCellROI.Items.AddObject(format('ROI.%d',[i]),TObject(i)) ;
        end ;
     cbCellROI.ItemIndex := 0 ;

     cbBackgroundROI.Items.Assign( cbCellROI.Items ) ;
     cbBackgroundROI.ItemIndex := 0 ;

    // Create maximum intensity image from all frames of selected type in file
    GenerateMaxIntensityImage ;

    // Set initial value of thresholds

    iMin := MainFrm.IDRFile.GreyMax ; ;
    iMax := 0 ;
    for i := 0 to MainFrm.IDRFile.FrameWidth*MainFrm.IDRFile.FrameHeight-1 do
        begin
        If pMaxIntensityImage[i] < IMin then IMin := pMaxIntensityImage[i] ;
        If pMaxIntensityImage[i] > IMax then IMax := pMaxIntensityImage[i] ;
        end;
     edThreshold.Value := (Imax + iMin) div 2 ;


end;


procedure TFindCellsFrm.GenerateMaxIntensityImage ;
// ---------------------------------------------------------
// Compute max pixel intensity image for selected frame type
// ---------------------------------------------------------
var
    i,iFrame : Integer ;
    pImage : PIntArray ;
    OK : Boolean ;
begin

    // Create local image buffer
    pImage := AllocMem( MainFrm.IDRFile.NumPixelsPerFrame*SizeOf(Integer));

    // Create max. intensity image buffer
    if pMaxIntensityImage <> Nil then FreeMem( pMaxIntensityImage ) ;
    pMaxIntensityImage := AllocMem( MainFrm.IDRFile.NumPixelsPerFrame*SizeOf(Integer));


    // Create a max. intensity image from all frames of selected type in file

    for iFrame := 1 to MainFrm.IDRFile.NumFrames do
        begin
        if MainFrm.IDRFile.TypeOfFrame(iFrame) = cbFrameType.ItemIndex then
           begin
           // Get frame from data file
           OK := MainFrm.IDRFile.LoadFrame32( iFrame, pImage ) ;
           if OK then
              begin
              for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do
                  begin
                  if pMaxIntensityImage[i] < pImage[i] then pMaxIntensityImage[i] := pImage[i] ;
                  end ;
              end ;
           end;
        end;

    // Display mean intensity of selected cell and background ROIs
    DisplayMeanROIIntensities ;

    FreeMem( pImage ) ;

end;


procedure TFindCellsFrm.bFIndCellsClick(Sender: TObject);
// --------------------------------------------
// Find cells in selected image and assign ROIs
// --------------------------------------------
var
    NumPixels,i,iX,iY,iStart,P,iROI,iStep : Integer ;
    CellNum : Integer ;        // Cell counter
    Pixel: TPair<Integer, Integer>;
    CellsBitMap : TBitmap ;
    pScanLine : PByteArray ;
    Locator : TParticleLocator ;
    Particle : TParticle ;

begin

    // Create thresholded image

    SetLength(  ThresholdedImage, MainFrm.IDRFile.FrameHeight, MainFrm.IDRFile.FrameWidth ) ;
    for iX := 0 to MainFrm.IDRFile.FrameWidth-1 do
        for iY := 0 to MainFrm.IDRFile.FrameHeight-1 do
        begin
        i := iX +  MainFrm.IDRFile.FrameWidth*iY ;
        if (pMaxIntensityImage[i] >= Round(edThreshold.Value)) then ThresholdedImage[iY,iX] := 1
                                                               else ThresholdedImage[iY,iX] := 0 ;
        end;

    // Locate cells in thresholded max. intensity image

    Locator := TParticleLocator.Create( ThresholdedImage,  MainFrm.IDRFile.FrameWidth,  MainFrm.IDRFile.FrameHeight);

    try
      Locator.LocateParticles(Round(edErosionRadius.Value)); // Threshold = 1

      // Get list of cell locations and data foound
      Particles := Locator.GetParticles;

      // Create bitmap showing cells
      CellsBitMap := TBitmap.Create ;
      CellsBitMap.PixelFormat := pf8Bit ;
      CellsBitMap.Width := MainFrm.IDRFile.FrameWidth ;
      CellsBitMap.Height := MainFrm.IDRFile.FrameHeight ;
      CellsBitMap.Canvas.Brush.Color := clBlack ;
      CellsBitMap.Canvas.Pen.Color := clWhite ;

      // CLear bitmap
      for iy := 0 to CellsBitMap.Height-1 do
          begin
          pScanLine := CellsBitMap.ScanLine[iY] ;
          for ix := 0 to CellsBitMap.Width-1 do
              begin
              pScanLine[iX] := 0 ;
              end ;
          end;

      // Add cells within selected area limits to bitmap

      for P := 0 to High(Particles) do
      begin

        // Get Particle data
        Particle := Particles[P];

        if (Particle.Area >= Round(edMinCellArea.Value)) and (Particle.Area <= Round(edMaxCellArea.Value)) then
           begin

           // Set pixels for this particle in bitmap
           for Pixel in Particle.Pixels do
               begin
               CellsBitMap.Canvas.Pixels[Pixel.Key,Pixel.Value] := clWhite ;
               end;

           edStatus.Text := format( 'Cells Found: %d ',[Particle.ID] );
           Application.ProcessMessages ;

           end ;

      end;

    Image1.Picture.Bitmap.Assign(CellsBitMap);
    CellsBitMap.Free ;

    finally
        Locator.Free ;
    end;

end;


procedure TFindCellsFrm.SaveCellsToROIList ;
// ---------------------------------
// Save centres of cells to ROI list
// ---------------------------------
var
    i,P,iROI,iStep : Integer ;
    ROI : TROI ;
    Particle : TParticle ;
begin

    for P := 0 to High(Particles) do
        begin

        // Get Particle data
        Particle := Particles[P];

        if (Particle.Area >= Round(edMinCellArea.Value)) and (Particle.Area <= Round(edMaxCellArea.Value)) then
           begin

           // Get next free ROI array element
           iROI := -1 ;
           for i :=  1 to MainFrm.IDRFile.MaxROI do if not MainFrm.IDRFile.ROI[i].InUse then
               begin
               iROI := i ;
               Break ;
               end ;

           if iROI > 0 then
              begin
              ROI.Shape := AreaROI ;
              ROI.InUse := True ;
              ROI.Width := Round(edROISize.Value) ;
              ROI.Height := Round(edROISize.Value) ;
              ROI.Centre.X := Integer(Round(Particle.Centroid_X)) ;
              ROI.Centre.Y := Integer(Round(Particle.Centroid_Y)) ;

              ROI.TopLeft.X := ROI.Centre.X - ROI.Width div 2 ;
              ROI.TopLeft.Y := ROI.Centre.Y - ROI.Height div 2 ;
              ROI.BottomRight.X := ROI.TopLeft.X + ROI.Width - 1 ;
              ROI.BottomRight.Y := ROI.TopLeft.Y + ROI.Height - 1 ;
              ROI.NumPoints := 0 ;

              MainFrm.IDRFile.ROI[iROI] := ROI ;
              end;

           end ;

        end;

    // Refresh ROI list and request ROI time course recomputation
    ViewFrm.RefreshROILists ;

    // Close form
    Close ;

end;


procedure TFindCellsFrm.FormClose(Sender: TObject; var Action: TCloseAction);
// ----------------------
// Close and destroy form
// ----------------------
begin

    Action := caFree ;

    ViewFrm.bFindCells.Enabled := True ;

end;


procedure TFindCellsFrm.FormCreate(Sender: TObject);
// ---------------------------------
// Initialisations when form created
// ---------------------------------
begin

    pMaxIntensityImage := Nil ;
end;


procedure TFindCellsFrm.FormDestroy(Sender: TObject);
// -------------------------------------
// Deallocate memory when form destroyed
// -------------------------------------

begin
    if pMaxIntensityImage <> Nil then FreeMem(pMaxIntensityImage) ;
end;


procedure TFindCellsFrm.rbMicronsClick(Sender: TObject);
// ----------------------------------
// Cell area limits defined in pixels
// ----------------------------------
var
    MinArea,MaxArea : Single ;
begin
    MinArea := edMinCellArea.Value ;
    MaxArea := edMaxCellArea.Value ;
    edMinCellArea.Scale := MainFrm.IDRFile.XResolution ;
    edMinCellArea.Units :=  MainFrm.IDRFile.ResolutionUnits ;
    edMaxCellArea.Scale := MainFrm.IDRFile.XResolution ;
    edMaxCellArea.Units :=  MainFrm.IDRFile.ResolutionUnits ;
    edMinCellArea.Value := MinArea ;
    edMaxCellArea.Value := MaxArea ;
end;


procedure TFindCellsFrm.rbPixelsClick(Sender: TObject);
// ----------------------------------
// Cell area limits defined in pixels
// ----------------------------------
var
    MinArea,MaxArea : Single ;
begin
    MinArea := edMinCellArea.Value ;
    MaxArea := edMaxCellArea.Value ;
    edMinCellArea.Scale := 1.0 ;
    edMinCellArea.Units := '' ;
    edMaxCellArea.Scale := 1.0 ;
    edMaxCellArea.Units := '' ;
    edMinCellArea.Value := MinArea ;
    edMaxCellArea.Value := MaxArea ;
end;


procedure TFindCellsFrm.bSaveAsROIsClick(Sender: TObject);
// -------------------------------
// Save cell positions to ROI list
// -------------------------------
begin
    SaveCellsToROIList ;
end;


procedure TFindCellsFrm.cbBackgroundROIChange(Sender: TObject);
// ----------------------
// Background ROI changed
// ----------------------
begin
     DisplayMeanROIIntensities ;
end;


procedure TFindCellsFrm.cbCellROIChange(Sender: TObject);
// ----------------
// Cell ROI changed
// ----------------
begin
     DisplayMeanROIIntensities ;
end;


procedure TFindCellsFrm.cbFrameTypeChange(Sender: TObject);
// ------------------
// Frame type changed
// ------------------
begin
      DisplayMeanROIIntensities ;
end;


procedure TFindCellsFrm.DisplayMeanROIIntensities;
// -------------------------------------------------------------------
// Display mean intensities within Cell & Background ROIs
// ------------------------------------------------------------------
var
    iROI,IThreshold : Integer ;
    CellAvg,BackgroundAvg : Single ;
begin

     if pMaxIntensityImage = Nil then Exit ;

     // Cell
     if cbCellROI.ItemIndex > 0 then
        begin
        iROI := Integer(cbCellROI.Items.Objects[cbCellROI.ItemIndex]) ;
        CellAVg := ViewPlotFrm.MeanROIIntensity( iROI, pMaxIntensityImage )/ MainFrm.IDRFile.IntensityScale ;
        lbCellIntensity.Caption := format('Iavg := %.6g',[CellAvg]) ;
        end
     else
       begin
       lbCellIntensity.Caption := '' ;
       CellAVg := 0.0 ;
       end;

     // Background
     if cbBackgroundROI.ItemIndex > 0 then
        begin
        iROI := Integer(cbBackgroundROI.Items.Objects[cbBackgroundROI.ItemIndex]) ;
        BackgroundAvg := ViewPlotFrm.MeanROIIntensity( iROI, pMaxIntensityImage )/ MainFrm.IDRFile.IntensityScale ;
        lbBackgroundIntensity.Caption := format('Iavg := %.6g',[BackgroundAvg]) ;
        end
     else
        begin
        lbBackgroundIntensity.Caption := '' ;
        BackgroundAVg := 0.0 ;
        end;

     IThreshold := Round( (CellAvg + BackgroundAvg)*0.5 ) ;
     IThreshold := Max( IThreshold, Round( BackgroundAvg ) ) ;
     edThreshold.Value := IThreshold ;

end;


end.
