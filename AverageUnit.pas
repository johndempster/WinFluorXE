unit AverageUnit;
// -----------------------------------------------
// WinFluor - Frame averaging module
// (c) J. Dempster, University of Strathclyde 2003
// -----------------------------------------------
// 29.7.03
// 1.8.03 ... FrameInterval now set correctly
// 7.3.07 ... A/D data now copied correctly when sub-range of frames selected
// 09.05.08 ... Dual-rate, multiwavelength support added
// 10.09.09 ... Frame differences added

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, RangeEdit, Main, IDRFile, StrUtils, math, excsetupunit ;

type
  TAverageFrm = class(TForm)
    GroupBox2: TGroupBox;
    rbAllFrames: TRadioButton;
    RadioButton1: TRadioButton;
    edRange: TRangeEdit;
    bOK: TButton;
    bCancel: TButton;
    IDRAvg: TIDRFile;
    GroupBox3: TGroupBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    rbAverage: TRadioButton;
    rbSkip: TRadioButton;
    edNumAverage: TValidatedEdit;
    rbAdd: TRadioButton;
    GroupBox4: TGroupBox;
    ckFrameDifferences: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
    PFrameBuf : PIntArray ;        // Pointer to image buffer
    PAvgBuf : Array[0..MaxFrameType] of PIntArray ;

    procedure AverageFrames ;
    procedure DifferentiateFrames ;

  public
    { Public declarations }
  end;

var
  AverageFrm: TAverageFrm;

implementation

uses FileIOUnit, LogUnit, ViewUnit;

{$R *.dfm}

procedure TAverageFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
     NumFast : Integer ;
begin

     edRange.LoLimit := 1 ;
     edRange.HiLimit := MainFrm.IDRFile.NumFrames ;
     edRange.LoValue := edRange.LoLimit ;
     edRange.HiValue := edRange.HiLimit ;

     // Allocate buffers
     GetMem( PFrameBuf, MainFrm.IDRFile.NumPixelsPerFrame*4 ) ;
     for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
         GetMem( PAvgBuf[i], MainFrm.IDRFile.NumPixelsPerFrame*4 ) ;

     NumFast := 0 ;
     for i := 0 to Mainfrm.IDRFile.NumFrameTypes-1 do
         if Mainfrm.IDRFile.FrameTypeDivideFactor[i] = 1.0 then Inc(NumFast) ;

     edNumAverage.LoLimit := (Mainfrm.IDRFile.FrameTypeCycleLength -
                             (Mainfrm.IDRFile.NumFrameTypes - NumFast)) div NumFast ;
     edNumAverage.Value := edNumAverage.LoLimit ;
     end;


procedure TAverageFrm.bOKClick(Sender: TObject);
begin

    if ckFrameDifferences.Checked then DifferentiateFrames
                                  else AverageFrames ;
    end ;


procedure TAverageFrm.AverageFrames ;
// ------------------------------------
// Create averaged/reduced frame series
// ------------------------------------

var

    StartAtFrame : Integer ;
    EndAtFrame : Integer ;
    Frame : Integer ;
    OutFrame : Integer ;
    ftype : Integer ;
    NumAveraged : Array[0..MaxFrameType] of Integer ;

    Done : Boolean ;
    OK : Boolean ;
    i : Integer ;
    iEnd : Integer ;
    FileName : String ;
    s : String ;
    StartScan, NumScans : Integer ;
    TStart,TEnd : Single ;
    ADCBuf : PSmallIntArray ;
    NumFramesTotal : Integer ;
begin

     bOK.Enabled := False ;

     // Select range of frames to be processed
     if rbAllFrames.Checked then begin
        StartAtFrame := Round(edRange.LoLimit) ;
        EndAtFrame := Round(edRange.HiLimit) ;
        end
     else begin
        StartAtFrame := Round(edRange.LoValue) ;
        EndAtFrame := Round(edRange.HiValue ) ;
        end ;

     // Create averages file
     FileName := ANSIReplaceText( MainFrm.IDRFile.FileName, '.idr',
                                 format('[avg%d][%d-%d].idr',
                                 [Round(edNumAverage.Value),StartAtFrame,EndAtFrame])) ;

     IDRAvg.CreateFileFrom( FileName, MainFrm.IDRFile, False ) ;

     for i := 0 to IDRAvg.NumFrameTypes-1 do IDRAvg.FrameTypeDivideFactor[i] := 1 ;

     // Ensure at least one frame of each type is available to average
     EndAtFrame := Max(EndAtFrame, StartAtFrame + Mainfrm.IDRFile.FrameTypeCycleLength -1) ;

     Frame := StartAtFrame ;

     // Clear averaging buffers
     for ftype := 0 to MainFrm.IDRFile.NumFrameTypes -1 do begin
         for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do PAvgBuf[ftype]^[i] := 0 ;
         NumAveraged[ftype] := 0 ;
         end ;

     Done := False ;
     OutFrame := 0 ;
     NumFramesTotal := 0 ;
     While not Done do begin

         // Add frame type sequence to average

         // Frame type
         ftype := MainFrm.IDRFile.TypeOfFrame(Frame) ;

         // Get frame from data file
         OK := MainFrm.IDRFile.LoadFrame32( Frame, PFrameBuf ) ;

         // Add frame to summation buffer
         // (Note. Only first frame added when using skip option)
         if OK or (not rbSkip.Checked) or (NumAveraged[ftype] = 0) then begin
            for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do
                PAvgBuf[ftype]^[i] := PAvgBuf[ftype]^[i] + PFrameBuf^[i] ;
            end ;

         Inc(Frame) ;
         Inc(NumFramesTotal) ;

         NumAveraged[ftype] := NumAveraged[ftype] + 1 ;

         // Compute average when required no. of frames done
         if NumAveraged[ftype] >= Round(edNumAverage.Value) then begin

            // Average and write to file each frame type
            for ftype := 0 to MainFrm.IDRFile.NumFrameTypes -1 do begin

                // If summation or frame skip selected, don't average
                if not rbAverage.Checked then NumAveraged[ftype] := 1 ;

                // Calculate average
                for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do
                    PAvgBuf[ftype]^[i] := PAvgBuf[ftype]^[i] div NumAveraged[ftype] ;

                // Save frame to data file
                Inc(OutFrame) ;
                IDRAvg.SaveFrame32( OutFrame, PAvgBuf[ftype] ) ;

                // Clear averages
                for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do PAvgBuf[ftype]^[i] := 0 ;
                NumAveraged[ftype] := 0 ;

                end ;
             end ;

         MainFrm.StatusBar.SimpleText := format(' Average: Frame %d/%d',[Frame,EndAtFrame]) ;
         Application.ProcessMessages ;

         if (Frame > EndAtFrame) or bOK.Enabled then Done := True ;

         end ;

     IDRAvg.FrameInterval := (MainFrm.IDRFile.FrameInterval*NumFramesTotal)/IDRAvg.NumFrames ;
     IDRAvg.ADCNumScansPerFrame := Round( (IDRAvg.ADCNumScansPerFrame*IDRAvg.FrameInterval) /
                                          MainFrm.IDRFile.FrameInterval ) ;

     // Copy A/D data (if available)
     if MainFrm.IDRFile.ADCNumChannels > 0 then begin
        TStart := (StartAtFrame-1)*MainFrm.IDRFile.FrameInterval ;
        TEnd := (EndAtFrame)*MainFrm.IDRFile.FrameInterval ;
        StartScan := Round(TStart/MainFrm.IDRFile.ADCScanInterval) ;
        NumScans := Min( Round((TEnd-TStart)/MainFrm.IDRFile.ADCScanInterval),
                         MainFrm.IDRFile.ADCNumScansInFile ) ;
        // Create A/D buffer
        GetMem( ADCBuf, NumScans*MainFrm.IDRFile.ADCNumChannels*2 ) ;
        // Copy A/D samples
        MainFrm.IDRFile.LoadADC( StartScan, NumScans, ADCBuf^ ) ;
        IDRAvg.SaveADC( 0, NumScans, ADCBuf^ ) ;
        FreeMem( ADCBuf ) ;
        // Update output file
        IDRAvg.ADCNumScansInFile :=  NumScans ;
        IDRAvg.ADCNumScansPerFrame := MainFrm.IDRFile.ADCNumScansPerFrame div IDRAvg.NumFrames ;
        end ;

     // Close files
     IDRAvg.CloseFile ;
     MainFrm.IDRFile.CloseFile ;

     s := format('Average: %d-%d (averages of %d frame) created in file %s',
                 [StartAtFrame,EndAtFrame,Round(edNumAverage.Value),FileName]) ;
     MainFrm.StatusBar.SimpleText := s ;
     LogFrm.AddLine(s);

    // Close view window
    if MainFrm.FormExists('Viewfrm') then ViewFrm.Close ;
    Application.ProcessMessages ;

     // Open and display averaged file
     MainFrm.IDRFile.OpenFile( FileName ) ;
     LogFrm.AddLine( 'File opened: ' + FileName );

     MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;
     if MainFrm.IDRFile.NumFrames > 0 then begin
        MainFrm.UpdateRecentFilesList ;
        MainFrm.mnViewImages.Click ;
        end ;

     Close ;

     end;


procedure TAverageFrm.DifferentiateFrames ;
// ----------------------------------------------------------
// Create series of the differences between successive images
// ----------------------------------------------------------

var

    StartAtFrame : Integer ;
    EndAtFrame : Integer ;
    Frame : Integer ;
    OutFrame : Integer ;
    ftype : Integer ;

    Done : Boolean ;
    OK : Boolean ;
    i : Integer ;
    iEnd,iTemp : Integer ;
    FileName : String ;
    s : String ;
    StartScan, NumScans : Integer ;
    TStart,TEnd : Single ;
    ADCBuf : PSmallIntArray ;
begin

     bOK.Enabled := False ;

     // Select range of frames to be processed
     if rbAllFrames.Checked then begin
        StartAtFrame := Round(edRange.LoLimit) ;
        EndAtFrame := Round(edRange.HiLimit) ;
        end
     else begin
        StartAtFrame := Round(edRange.LoValue) ;
        EndAtFrame := Round(edRange.HiValue ) ;
        end ;

     // Ensure frames start and end on beginning and end of multi-frame set
     StartAtFrame := StartAtFrame - ((StartAtFrame-1) mod MainFrm.IDRFile.NumFrameTypes) ;
     EndAtFrame := EndAtFrame - ((EndAtFrame-1) mod MainFrm.IDRFile.NumFrameTypes)
                              + (MainFrm.IDRFile.NumFrameTypes-1) ;

     // Create averages file
     FileName := ANSIReplaceText( MainFrm.IDRFile.FileName, '.idr',
                                 format('[diff][%d-%d].idr',
                                 [StartAtFrame,EndAtFrame])) ;

     IDRAvg.CreateFileFrom( FileName, MainFrm.IDRFile, False ) ;

     // Note differentiated pixel intensity is divided by 2 and offset
     // into middle of intensity range to accommodate negative values

     IDRAvg.IntensityOffset := MainFrm.IDRFile.GreyMax div 2 ;
     IDRAvg.IntensityScale := 2.0 ;

     // Load first frames
     for ftype := 0 to MainFrm.IDRFile.NumFrameTypes-1 do begin
         MainFrm.IDRFile.LoadFrame32( StartAtFrame+ftype, PAvgBuf[ftype] ) ;
         end ;

     for i := 0 to IDRAvg.NumFrameTypes-1 do IDRAvg.FrameTypeDivideFactor[i] := 1 ;


     OutFrame := 0 ;
     for Frame := StartAtFrame + MainFrm.IDRFile.NumFrameTypes to EndAtFrame do begin

         // Add frame type sequence to average

         // Frame type
         ftype := MainFrm.IDRFile.TypeOfFrame(Frame) ;

         // Get frame from data file
         OK := MainFrm.IDRFile.LoadFrame32( Frame, PFrameBuf ) ;
         if not OK then break ;

         // Subtract previous frame
         for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do begin
             iTemp := PFrameBuf^[i] ;
             PFrameBuf^[i] := Round( ((PFrameBuf^[i] - PAvgBuf[ftype]^[i])/IDRAvg.IntensityScale)
                                     + IDRAvg.IntensityOffset );
             PAvgBuf[ftype]^[i] := iTemp ;
             end ;

         // Save frame to data file
         Inc(OutFrame) ;
         IDRAvg.SaveFrame32( OutFrame, PFrameBuf ) ;

         MainFrm.StatusBar.SimpleText := format('Subtracting Frames: Frame %d/%d',[Frame,EndAtFrame]) ;
         Application.ProcessMessages ;

         end ;

     // Copy A/D data (if available)
     if MainFrm.IDRFile.ADCNumChannels > 0 then begin
        TStart := (StartAtFrame + MainFrm.IDRFile.NumFrameTypes-1)*MainFrm.IDRFile.FrameInterval ;
        TEnd := (EndAtFrame)*MainFrm.IDRFile.FrameInterval ;
        StartScan := Round(TStart/MainFrm.IDRFile.ADCScanInterval) ;
        NumScans := Min( Round((TEnd-TStart)/MainFrm.IDRFile.ADCScanInterval),
                         MainFrm.IDRFile.ADCNumScansInFile ) ;
        // Create A/D buffer
        GetMem( ADCBuf, NumScans*MainFrm.IDRFile.ADCNumChannels*2 ) ;
        // Copy A/D samples
        MainFrm.IDRFile.LoadADC( StartScan, NumScans, ADCBuf^ ) ;
        IDRAvg.SaveADC( 0, NumScans, ADCBuf^ ) ;
        FreeMem( ADCBuf ) ;
        // Update output file
        IDRAvg.ADCNumScansInFile :=  NumScans ;
        IDRAvg.ADCNumScansPerFrame := MainFrm.IDRFile.ADCNumScansPerFrame div IDRAvg.NumFrames ;
        end ;

     // Close files
     IDRAvg.CloseFile ;
     MainFrm.IDRFile.CloseFile ;

     s := format('Differentiated Fra: %d-%d created in file %s',
                 [StartAtFrame,EndAtFrame,FileName]) ;
     MainFrm.StatusBar.SimpleText := s ;
     LogFrm.AddLine(s);

    // Close view window
    if MainFrm.FormExists('Viewfrm') then ViewFrm.Close ;
    Application.ProcessMessages ;

     // Open and display averaged file
     MainFrm.IDRFile.OpenFile( FileName ) ;
     LogFrm.AddLine( 'File opened: ' + FileName );

     MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;
     if MainFrm.IDRFile.NumFrames > 0 then begin
        MainFrm.UpdateRecentFilesList ;
        MainFrm.mnViewImages.Click ;
        end ;

     Close ;

     end;



procedure TAverageFrm.FormClose(Sender: TObject; var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
var
     i : Integer ;
begin

     // Allocate buffers
     FreeMem( PFrameBuf ) ;
     for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do FreeMem( PAvgBuf[i] ) ;

     Action := caFree ;

     end;

procedure TAverageFrm.bCancelClick(Sender: TObject);
// -------------------------------
// Cancel averaging and close form
// -------------------------------
begin
     if bOK.Enabled then begin
        // Exit form if no averaging in progress
        Close ;
        end
     else begin
        // Enable OK button ... this terminates averaging
        bOK.Enabled := True ;
        end ;
     end;

end.
