unit IntegrateUnit;
// ===================================================
// Integrated Image Capture Module
// (c) John Dempster, University of Strathclyde, 2002
// ===================================================
// 5/3/2002
// 7.7.03 .... Red,green,blue colour palettes added

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, RangeEdit, ValEdit, ValidatedEdit;

type
  TIntegrateFrm = class(TForm)
    ImageGrp: TGroupBox;
    Image: TImage;
    Timer: TTimer;
    ControlsGrp: TGroupBox;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbImageSize: TLabel;
    erXRange: TRangeEdit;
    erYRange: TRangeEdit;
    edBinFactor: TValidatedEdit;
    GroupBox3: TGroupBox;
    lbReadoutTime: TLabel;
    Label6: TLabel;
    edFrameInterval: TValidatedEdit;
    edNumFramesToAverage: TValidatedEdit;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    bFullScale: TButton;
    bMaxContrast: TButton;
    edDisplayIntensityRange: TRangeEdit;
    GroupBox5: TGroupBox;
    rbSingle: TRadioButton;
    bStart: TButton;
    GroupBox6: TGroupBox;
    Label1: TLabel;
    lbDuration: TLabel;
    edNumFramesRequired: TValidatedEdit;
    rbSequence: TRadioButton;
    bStop: TButton;
    bRecord: TButton;
    GroupBox7: TGroupBox;
    rbIntegrate: TRadioButton;
    rbAverage: TRadioButton;
    GroupBox8: TGroupBox;
    bLoadBackground: TButton;
    ckSubtractBackground: TCheckBox;
    cbPalette: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure bStartClick(Sender: TObject);
    procedure edNumFramesRequiredKeyPress(Sender: TObject; var Key: Char);
    procedure bStopClick(Sender: TObject);
    procedure edDisplayIntensityRangeKeyPress(Sender: TObject;
      var Key: Char);
    procedure edFrameIntervalKeyPress(Sender: TObject; var Key: Char);
    procedure bFullScaleClick(Sender: TObject);
    procedure bMaxContrastClick(Sender: TObject);
    procedure erXRangeKeyPress(Sender: TObject; var Key: Char);
    procedure rbSingleClick(Sender: TObject);
    procedure rbSequenceClick(Sender: TObject);
    procedure bRecordClick(Sender: TObject);
    procedure bLoadBackgroundClick(Sender: TObject);
    procedure ckSubtractBackgroundClick(Sender: TObject);
    procedure cbPaletteChange(Sender: TObject);
  private
    { Private declarations }
    PFrameBuf : Pointer ;
    BitMap : TBitMap ;
    FirstFrameInLowerBuffer : Integer ;
    LastFrameInLowerBuffer : Integer ;
    FirstFrameInUpperBuffer : Integer ;
    LastFrameInUpperBuffer : Integer ;
    SumImage : Array[0..1023*1023] of Integer ;
    AvgImage : Array[0..1023*1023] of SmallInt ;
    NewImage : Array[0..1023*1023] of SmallInt ;
    BackgImage  : Array[0..1023*1023] of SmallInt ;
    NumImagesAveraged : Integer ;

    SelectedFrameType : Integer ;
    NumBytesPerHalfFrameBuffer : Integer ;
    NumBytesPerFrame : Integer ;
    NumFramesDone : Integer ;
    NumFramesRequired : Integer ;
    Recording : Boolean ;
    LowerBufferFilling : Boolean ;

    TimerProcBusy : Boolean ;
    SetMaxDisplayContrast : Boolean ;

    procedure FillBufferWithEmptyFlags( StartAt : Integer ; EndAt : Integer ) ;
    procedure InitialiseImage ;
    procedure ResetAverageImage ;
    procedure SetSingleOrSequence ;

  public
    { Public declarations }
    { Public declarations }
    PAvgImage : Pointer ;
    FrameIndex : Integer ;
    NumFramesInBuffer : Integer ;
    NumPixelsPerFrame : Integer ;
    NumBytesPerPixel : Integer ;
    LastFrameDisplayed : Integer ;
    ByteImage : Boolean ;
    ImageAvailable : Boolean ;

  end;

var
  IntegrateFrm: TIntegrateFrm;

implementation

uses Main, mmsystem, pvcam, maths, LabIOUnit, SESCam, FileIOUnit ;

{$R *.DFM}

const
    ByteLoValue = 0 ;
    ByteHiValue = 255 ;
    WordLoValue = 0 ;
    WordHiValue = 32767 ;
    EmptyFlag = 32767 ;


procedure TIntegrateFrm.FormCreate(Sender: TObject);
//
// Initialisations when form is created
// ------------------------------------
begin
        BitMap := Nil ;
        end;


procedure TIntegrateFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
begin


     // Set camera to free run mode
     MainFrm.Cam1.TriggerMode := CamFreeRun ;

     // Inter-frame capture interval
     edFrameInterval.Value := MainFrm.Cam1.FrameInterval ;
     lbReadoutTime.Caption := format('Min.= %.3g ms',
                                      [SecsToMs*MainFrm.Cam1.ReadoutTime]) ;

     // No.frames per recording sequence
     edNumFramesRequired.Value :=  MainFrm.NumFramesRequired ;

     // Set limits of display intensity range
     SelectedFrameType := 0 ;
     edDisplayIntensityRange.LoLimit := 0 ;
     edDisplayIntensityRange.HiLimit := MainFrm.Cam1.GreyLevelMax ;
     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[SelectedFrameType] ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[SelectedFrameType] ;

     // Intensity display palette
     cbPalette.Clear ;
     cbPalette.Items.AddObject(' Grey scale', TObject(palGrey)) ;
     cbPalette.Items.AddObject(' False colour', TObject(palFalseColor)) ;
     cbPalette.Items.AddObject(' Red scale', TObject(palRed)) ;
     cbPalette.Items.AddObject(' Green scale', TObject(palGreen)) ;
     cbPalette.Items.AddObject(' Blue scale', TObject(palBlue)) ;
     cbPalette.ItemIndex := 0 ;

     // Enable/disable single image/sequence controls
     SetSingleOrSequence ;

     // Set image size
     InitialiseImage ;

     // Initialise background image
     for i := 0 to NumPixelsPerFrame-1 do BackgImage[i] := 0 ;

     Recording := False ;
     bRecord.Enabled := True ;
     bStop.Enabled := False ;
     bLoadBackground.Enabled := False ;
     ckSubtractBackground.Enabled := False ;
     TimerProcBusy := False ;
     SetMaxDisplayContrast := False ;
     ImageAvailable := False ;
     FrameIndex := 0 ;
     pAvgImage := @AvgImage ;
     NumBytesPerPixel := 2 ;

     // Start acquiring images
     MainFrm.Cam1.StartCapture ;

     end;


procedure TIntegrateFrm.InitialiseImage ;
// ------------------------------------------------------
// Re-initialise size of memory buffers and image bitmaps
// ------------------------------------------------------
begin

     // Dispose of existing bit map
     if BitMap <> Nil then BitMap.Free  ;

     // Create new one
     BitMap := TBitMap.Create ;

     // Set size
     BitMap.Width := MainFrm.Cam1.FrameWidth ;
     BitMap.Height := MainFrm.Cam1.FrameHeight ;

     Image.Width := MainFrm.Cam1.FrameWidth ;
     Image.Height := MainFrm.Cam1.FrameHeight ;
     ImageGrp.Width := 2*Image.Left + Image.Width ;
     ImageGrp.Height := 2*Image.Top + Image.Height ;

     ClientWidth := ImageGrp.Left + ImageGrp.Width + 5 ;

     ClientHeight := MaxInt([ ImageGrp.Top + ImageGrp.Height + 5,
                              ControlsGrp.Top + ControlsGrp.Height + 5 ]);

     BitMap.PixelFormat := pf8bit ;
     MainFrm.SetPalette( BitMap, MainFrm.PaletteType ) ;

     // Determine number of frame within circular buffer
     NumPixelsPerFrame := MainFrm.Cam1.FrameWidth*MainFrm.Cam1.FrameHeight ;
     MainFrm.Cam1.NumFramesInBuffer :=  (4194304 div
                                        (NumPixelsPerFrame*MainFrm.Cam1.NumBytesPerPixel))-1 ;
     MainFrm.Cam1.NumFramesInBuffer := (MainFrm.Cam1.NumFramesInBuffer div 2)*2 ;


     NumFramesInBuffer :=  MainFrm.Cam1.NumFramesInBuffer ;
     FirstFrameInLowerBuffer := 0 ;
     LastFrameInLowerBuffer := (NumFramesInBuffer div 2) - 1 ;
     FirstFrameInUpperBuffer := LastFrameInLowerBuffer + 1 ;
     LastFrameInUpperBuffer := NumFramesInBuffer  - 1 ;

     NumBytesPerHalfFrameBuffer := (NumFramesInBuffer div 2)*MainFrm.Cam1.NumBytesPerPixel*NumPixelsPerFrame      ;
     NumBytesPerFrame := MainFrm.IDRFile.NumBytesPerFrame ;
     MainFrm.Cam1.GetFrameBufferPointer( PFrameBuf ) ;

     // Set byte/word image flag
     if MainFrm.Cam1.NumBytesPerPixel = 1 then ByteImage := True
                                          else ByteImage := False ;

     // Add empty flag value to end of each frame
     FillBufferWithEmptyFlags( 0, NumFramesInBuffer-1 ) ;
     LowerBufferFilling := True ;

     // Initial setting of last frame displayed
     LastFrameDisplayed := NumFramesInBuffer - 1 ;

     lbDuration.caption := format( '(%.3g s)',[edNumFramesRequired.Value*
                                               edNumFramesToAverage.Value,
                                               MainFrm.Cam1.FrameInterval] ) ;

     erXRange.LoValue := MainFrm.Cam1.FrameLeft ;
     erXRange.HiValue := MainFrm.Cam1.FrameRight ;
     erYRange.LoValue := MainFrm.Cam1.FrameTop ;
     erYRange.HiValue := MainFrm.Cam1.FrameBottom ;
     edBinFactor.Value := MainFrm.Cam1.BinFactor ;
     lbImageSize.Caption := format('(x=%d,y=%d)',
                            [MainFrm.Cam1.FrameWidth,MainFrm.Cam1.FrameHeight]) ;

     // Add empty flag value to end of each frame
     FillBufferWithEmptyFlags( 0, NumFramesInBuffer-1 ) ;
     LowerBufferFilling := True ;

     // Reset averaged image
     ResetAverageImage ;

     end ;


procedure TIntegrateFrm.ResetAverageImage ;
// --------------------
// Reset averaged image
// --------------------
var
    i : Integer ;
begin
    for i := 0 to NumPixelsPerFrame-1 do begin
        SumImage[i] := 0 ;
        AvgImage[i] := 0 ;
        end ;
    NumImagesAveraged := 0 ;
    end ;


procedure TIntegrateFrm.FillBufferWithEmptyFlags(
          StartAt : Integer ;
          EndAt : Integer ) ;
// ----------------------------------------------
// Add empty flags to end of each frame in buffer
// ----------------------------------------------
var
    i,iFlag : Integer ;
begin

     for i := StartAt to EndAt do begin
         iFlag := (i+1)*NumPixelsPerFrame - 1 ;
         if ByteImage then begin
            PByteArray(PFrameBuf)^[iFlag] := ByteHiValue ;
            PByteArray(PFrameBuf)^[iFlag-1] := ByteLoValue ;
            end
         else begin
            PWordArray(PFrameBuf)^[iFlag] := WordHiValue ;
            PWordArray(PFrameBuf)^[iFlag-1] := WordLoValue ;
            end ;
         end ;
     end ;


procedure TIntegrateFrm.TimerTimer(Sender: TObject);
// ---------------------
// Scheduled timer event
// ---------------------

var
     i,j,x,y,t0,FrameNum,FrameCount,NextFrameToDisplay,iFlag : Integer ;
     FirstFrame,LastFrame,iStart : Integer ;
     PScanLine : PByteArray ;
     z,zMin,zMax : Word ;
     Done,BufferFull : Boolean ;
     PVStat : SmallInt ;
     PVCount,PVBCount : Cardinal ;
     LatestFrame : Pointer ;
begin

    if TimerProcBusy then Exit ;

    TimerProcBusy := True ;
    LatestFrame := Nil ;

     if {MainFrm.Cam1.CameraActive}True then begin

        // Find latest frame that has been acquired
        // ----------------------------------------

        FrameNum := LastFrameDisplayed + 1 ;
    //    pl_exp_check_cont_status( Session.Handle,PVStat,PVCount,PVBCount);
    //    PVCAM_DisplayErrorMessage('pl_exp_check_cont_status') ;

        NextFrameToDisplay := -1 ;
        FrameCount := 0 ;
        Done := False ;
        while not Done do begin
           // Keep frame within buffer
           if FrameNum >= NumFramesInBuffer then FrameNum := 0 ;
           // Get pointer to flag pixel
           iFlag :=  ((FrameNum+1)*NumPixelsPerFrame) - 1 ;
           // If image available for this frame, get frame#
           if ByteImage then begin
              if (PByteArray(PFrameBuf)^[iFlag] = ByteHiValue) and
                 (PByteArray(PFrameBuf)^[iFlag-1] = ByteLoValue) then Done := True
              else NextFrameToDisplay := FrameNum ;
              end
           else begin
              if (PWordArray(PFrameBuf)^[iFlag] = WordHiValue) and
                 (PWordArray(PFrameBuf)^[iFlag-1] = WordLoValue) then Done := True
              else NextFrameToDisplay := FrameNum ;
              end ;
           Inc(FrameNum) ;
           // Emergency exit when buffer overflow occurs
           Inc(FrameCount) ;
           if FrameCount >= NumFramesInBuffer then Done := True ;
           end ;

        // Display latest frame
        // --------------------


        if NextFrameToDisplay >= 0 then begin
           t0 := TimeGetTime ;


           // Update integration/average buffers
           // ----------------------------------

           if NumImagesAveraged < Round(edNumFramesToAverage.Value) then begin
              // Add latest frame to average
              // ---------------------------
              i := NextFrameToDisplay*NumPixelsPerFrame ;
              Inc(NumImagesAveraged) ;

              // Get latest image from circular buffer
              if ByteImage then begin
                 // 8 bit image
                 for j := 0 to NumPixelsPerFrame-1 do begin
                     NewImage[j] := PByteArray(PFrameBuf)^[i] ;
                     Inc(i) ;
                     end ;
                 end
              else begin
                 // 16 bit image
                 for j := 0 to NumPixelsPerFrame-1 do begin
                     NewImage[j] := PWordArray(PFrameBuf)^[i] ;
                     Inc(i) ;
                     end ;
                 end ;

              // Subtract background (if required)
              if ckSubtractBackground.Checked then begin
                 for j := 0 to NumPixelsPerFrame-1 do
                     NewImage[j] := NewImage[j] - BackgImage[j] ;
                 end
              else if rbAverage.Enabled then begin
                 // Enable load background since image is available
                 // and subtract background is disabled
                 bLoadBackground.Enabled := True ;
                 end ;

              if rbAverage.Checked then begin
                 // Average image
                 for j := 0 to NumPixelsPerFrame-1 do begin
                     SumImage[j] := SumImage[j] + NewImage[j] ;
                     AvgImage[j] :=  SumImage[j] div NumImagesAveraged ;
                     end ;
                 end
              else begin
                 // Integrate image
                 for j := 0 to NumPixelsPerFrame-1 do
                     AvgImage[j] := AvgImage[j] + NewImage[j] ;
                 end ;


              end
           else if rbSequence.Checked then begin
              // Save averaged image to file, if in recording mode
              // -------------------------------------------------
              if Recording then begin
                 // Save pixel data
                 FileWrite( MainFrm.IDRFile.IDRFileHandle, AvgImage,NumPixelsPerFrame*2) ;
                 // Increment frame counter
                 Inc(NumFramesDone) ;
                 // Check whether required number of frames have been collected
                 if Recording and (NumFramesDone >= Round(edNumFramesRequired.Value)) then begin
                    MainFrm.StatusBar.SimpleText := format( 'Recording Complete: %d Frames collected',
                                                    [NumFramesDone]) ;
                    Recording := False ;
                    bStop.Click ;
                    end
                 else begin
                    MainFrm.StatusBar.SimpleText := format( 'Recording: Frames %d/%d',
                                                          [NumFramesDone,Round(edNumFramesRequired.Value)]) ;
                    end ;
                 end ;

              // Clear averaged image buffers and counters
              ResetAverageImage ;

              end ;

           // Display average image
           // ---------------------

           i := 0 ;
           for y := 0 to MainFrm.Cam1.FrameHeight-1 do begin
               PScanLine := BitMap.ScanLine[y] ;
               for x := 0 to MainFrm.Cam1.FrameWidth-1 do begin
                      PScanLine[x] := MainFrm.LUTs[0] ;
                      Inc(i) ;
                      end ;
               end ;
           Image.Picture.Assign(BitMap) ;
           LastFrameDisplayed := NextFrameToDisplay ;
           ImageAvailable := True ;

           // Report progress
           if (rbSingle.Checked) or (not Recording) then begin
              MainFrm.StatusBar.SimpleText := format(
              ' Average/Integrated Image : %d/%d frames done',
              [NumImagesAveraged,Round(edNumFramesToAverage.Value)]) ;
              end
           else begin
              MainFrm.StatusBar.SimpleText := format(
              ' Average/Integrated Image : [RECORDING] Average %d/%d (%d/%d frames done)',
              [NumFramesDone+1,Round(edNumFramesRequired.Value),
               NumImagesAveraged,Round(edNumFramesToAverage.Value)]) ;
              end

           end ;

        // Adjust display LUT for maximum contrast (if requested)
        if SetMaxDisplayContrast then begin
           ZMin := High(ZMax) ;
           ZMax := 0 ;
           for j := 0 to NumPixelsPerFrame-1 do begin
               Z := AvgImage[j] ;
               if ZMin > Z then ZMin := Z ;
               if ZMax < Z then ZMax := Z ;
               end ;

           MainFrm.GreyLo[SelectedFrameType] := MaxInt([ZMin,0]) ;
           MainFrm.GreyHi[SelectedFrameType] := ZMax ;
           edDisplayIntensityRange.LoValue := MainFrm.GreyLo[SelectedFrameType] ;
           edDisplayIntensityRange.HiValue := MainFrm.GreyHi[SelectedFrameType] ;
           MainFrm.UpdateLUT(SelectedFrameType, MainFrm.Cam1.GreyLevelMax ) ;
           SetMaxDisplayContrast := False ;
           end ;

        // Restore empty frame flags / write to file
        // -----------------------------------------

        // Get position of frame empty flag at end of half-buffer
        if LowerBufferFilling then begin
           iFlag := ((LastFrameInLowerBuffer+1)*NumPixelsPerFrame) - 1 ;
           FirstFrame := FirstFrameInLowerBuffer ;
           LastFrame := LastFrameInLowerBuffer ;
           end
        else begin
           iFlag := (NumFramesInBuffer*NumPixelsPerFrame) - 1 ;
           FirstFrame := FirstFrameInUpperBuffer ;
           LastFrame := LastFrameInUpperBuffer ;
           end ;

        // Is half-buffer full yet?
        BufferFull := False ;
        if ByteImage then begin
           if (PByteArray(PFrameBuf)^[iFlag] <> ByteHiValue) or
              (PByteArray(PFrameBuf)^[iFlag-1] <> ByteLoValue) then BufferFull := True ;
           end
        else begin
           if (PWordArray(PFrameBuf)^[iFlag] <> WordHiValue) or
              (PWordArray(PFrameBuf)^[iFlag-1] <> WordLoValue) then BufferFull := True ;
           end ;


        // If half-buffer is full ...
        // update empty flags and write to disk if necessary
        //
        If BufferFull then begin

           // Reset empty frame flags
           FillBufferWithEmptyFlags( FirstFrame, LastFrame ) ;
           // Force last frame displayed to end of buffer
           if LastFrameDisplayed < LastFrame then LastFrameDisplayed := LastFrame ;
           // Toggle buffer in use flag
           LowerBufferFilling := not LowerBufferFilling ;
           end ;


        end ;

     TimerProcBusy := False ;

     end;


procedure TIntegrateFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
begin
     // Stop acquiring images
     MainFrm.Cam1.StopCapture ;


     // Request destruction of form
     Action := caFree ;

     end;


procedure TIntegrateFrm.FormDestroy(Sender: TObject);
begin
    if BitMap <> Nil then BitMap.Free ;
    end ;


procedure TIntegrateFrm.bStartClick(Sender: TObject);
// ------------------------------------------
// Clear averaged image and re-start capture
// ------------------------------------------
//
begin
     // Reset averaged image
     ResetAverageImage ;
     end;


procedure TIntegrateFrm.edNumFramesRequiredKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------
// No. of frames required updated
// ------------------------------
begin
     if key = #13 then begin
        lbDuration.caption := format( '(%.3g s)',
                              [edNumFramesRequired.Value*
                               edNumFramesToAverage.Value,
                                MainFrm.Cam1.FrameInterval] ) ;
        MainFrm.NumFramesRequired := Round(edNumFramesRequired.Value) ;                        
        end ;
     end;


procedure TIntegrateFrm.bStopClick(Sender: TObject);
//
// Stop recording images to disk
// -----------------------------
//
begin
     Recording := False ;
     bRecord.Enabled := True ;
     bStop.Enabled := False ;

     MainFrm.IDRFile.CloseFile ;
     MainFrm.IDRFile.OpenFile( MainFrm.IDRFile.FileName ) ;

     end;


procedure TIntegrateFrm.edDisplayIntensityRangeKeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------
// Update display intensity range
// ------------------------------
begin
     if key = chr(13) then begin
        MainFrm.GreyLo[SelectedFrameType] := Round(edDisplayIntensityRange.LoValue) ;
        MainFrm.GreyHi[SelectedFrameType] := Round(edDisplayIntensityRange.HiValue) ;
        MainFrm.UpdateLUT( SelectedFrameType, MainFrm.Cam1.GreyLevelMax ) ;
        end ;
     end;


procedure TIntegrateFrm.edFrameIntervalKeyPress(Sender: TObject;
  var Key: Char);
//
// Inter-frame capture time updated
// --------------------------------
begin
     if key = #13 then begin
         MainFrm.Cam1.StopCapture ;
         MainFrm.Cam1.FrameInterval := edFrameInterval.Value ;
         InitialiseImage ;
         MainFrm.Cam1.StartCapture ;
         edFrameInterval.Value := MainFrm.Cam1.FrameInterval ;
         end ;
     end;


procedure TIntegrateFrm.bFullScaleClick(Sender: TObject);
// --------------------------------------------------------
// Set display grey scale to full intensity range of camera
// --------------------------------------------------------
begin
     MainFrm.GreyLo[SelectedFrameType] := 0 ;
     MainFrm.GreyHi[SelectedFrameType] := MainFrm.Cam1.GreyLevelMax ;
     edDisplayIntensityRange.LoValue := MainFrm.GreyLo[SelectedFrameType] ;
     edDisplayIntensityRange.HiValue := MainFrm.GreyHi[SelectedFrameType] ;
     MainFrm.UpdateLUT( SelectedFrameType, MainFrm.Cam1.GreyLevelMax ) ;
     end;


procedure TIntegrateFrm.bMaxContrastClick(Sender: TObject);
// -------------------------------------------------------------
// Request display intensity range to be set for maximum contrast
// -------------------------------------------------------------
begin
     SetMaxDisplayContrast := True ;
     end;

procedure TIntegrateFrm.erXRangeKeyPress(Sender: TObject; var Key: Char);
// -------------------------------------------------------------
// Image capture horizontal/vertical range / bin factor updated
// -------------------------------------------------------------
begin
     if key = #13 then begin
        MainFrm.Cam1.StopCapture ;
        MainFrm.Cam1.FrameLeft := Round(erXRange.LoValue) ;
        MainFrm.Cam1.FrameRight := Round(erXRange.HiValue) ;
        MainFrm.Cam1.FrameTop := Round(erYRange.LoValue) ;
        MainFrm.Cam1.FrameBottom := Round(erYRange.HiValue) ;
        MainFrm.Cam1.BinFactor := Round(edBinFactor.Value) ;
        InitialiseImage ;
        MainFrm.Cam1.StartCapture ;
        end ;
     end;


procedure TIntegrateFrm.SetSingleOrSequence ;
// ----------------------------------------------------------
// Set up controls for single image or image sequence capture
// ----------------------------------------------------------
begin
     if rbSingle.Checked then begin
        bStart.Enabled := True ;
        bRecord.Enabled := False ;
        bStop.Enabled := False ;
        end
     else begin
        bStart.Enabled := False ;
        bRecord.Enabled := True ;
        bStop.Enabled := False ;
        end ;
     end ;

procedure TIntegrateFrm.rbSingleClick(Sender: TObject);
// --------------------------------
// Set up for single image capture
// --------------------------------
begin
     rbSequence.Checked := False ;
     SetSingleOrSequence ;
     end;

procedure TIntegrateFrm.rbSequenceClick(Sender: TObject);
// ----------------------------------
// Set up for image sequence capture
// ----------------------------------
begin
     rbSingle.Checked := False ;
     SetSingleOrSequence ;
     end;

procedure TIntegrateFrm.bRecordClick(Sender: TObject);
// -----------------------
// Start recording to file
// -----------------------
begin

     if MainFrm.IDRFile.IDRFileHandle >= 0 then begin

        // Move file pointer to end of data file
        FileSeek(MainFrm.IDRFile.IDRFileHandle,0,2) ;
        // Request frames to be stored on file
        Recording := True ;
        // Prevent multiple starts
        bRecord.Enabled := False ;
        bStop.Enabled := True ;
        // Set number of frames collected to zero
        NumFramesDone := 0 ;
        // Reset average image and buffers
        ResetAverageImage ;

        end
     else begin
        // Report failure to record
        MainFrm.StatusBar.SimpleText :=
        ' Record Sequence : Unable to record! No data file open! ' ;
        end ;

     end;

procedure TIntegrateFrm.bLoadBackgroundClick(Sender: TObject);
// ------------------------------------------------------
// Load background image with currently displayed average
// ------------------------------------------------------
var
     i : Integer ;
begin
     for i := 0 to NumPixelsPerFrame-1 do BackgImage[i] := AvgImage[i] ;
     ckSubtractBackground.Enabled := True ;
     end;


procedure TIntegrateFrm.ckSubtractBackgroundClick(Sender: TObject);
// -------------------------------------------------------
// Disable load backgound button if subtraction is enabled
// -------------------------------------------------------
begin
     if ckSubtractBackground.checked then bLoadBackground.Enabled := False ;
     end ;


procedure TIntegrateFrm.cbPaletteChange(Sender: TObject);
// ------------------------------
// Display colour palette changed
// ------------------------------
begin
     MainFrm.PaletteType := TPaletteType(cbPalette.Items.Objects[cbPalette.ItemIndex]) ;
     MainFrm.SetPalette( BitMap, MainFrm.PaletteType ) ;
     end;


end.

