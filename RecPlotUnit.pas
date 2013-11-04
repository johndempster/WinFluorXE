unit RecPlotUnit;
// =======================================================
// WinFluor - Live image ROI time course plotting module
// =======================================================
// 21.03.06
// 29.01.07 DisplayGrid property added
// 31.01.07 ADCMaxBlocksDisplayed now limited to MaxDisplayScans
// 20.02.07 ZoomOutAll method added
// 20.07.07 Additional ROIs can now be added to plot
//          Ratio display added
// 17.08.07 Clearing of time course displays at end of sweep now internal to unit
// 14.01.08 Live regions of interest can now be averaged as square blocks of pixels
// 16.09.08 Live regions of interest size now saved in INI file
// 14.04.09 JD Resize now perform by Update procedures
// 29.07.10 JD Ratio plot no longer runs at double speed when in time lapse mode
// 20.10.10 JD Display max. duration limit now 20000s
// 23.07.12 JD

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ExtCtrls, HTMLLabel, ScopeDisplay, math, IDRFile, strutils ;

Const
    GreyLevelLimit = $FFFF ;
    MaxDisplayScans = 2000 ;
    MaxADCChannels = 8 ;
    MaxFrames = 100000 ;
    MaxROIsRecPlot = 100 ;

type


  TRecPlotFrm = class(TForm)
    ckFixZeroLevels: TCheckBox;
    TDisplayPanel: TPanel;
    edTDisplay: TValidatedEdit;
    rbTDisplayUnitMins: TRadioButton;
    rbTDisplayUnitsSecs: TRadioButton;
    bTDisplayDouble: TButton;
    bTDisplayHalf: TButton;
    ROIGrp: TGroupBox;
    lbADCDisplay: THTMLLabel;
    lbFLDisplay: THTMLLabel;
    FluorGrp: TGroupBox;
    cbROI: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    cbSubROI: TComboBox;
    ckDisplayFluorescence: TCheckBox;
    ADCGrp: TGroupBox;
    ckDisplayADC: TCheckBox;
    RatioGrp: TGroupBox;
    Label3: TLabel;
    Shape1: TShape;
    ckDisplayR: TCheckBox;
    cbDenominator: TComboBox;
    cbNumerator: TComboBox;
    edRDisplayMax: TValidatedEdit;
    lbRDisplay: THTMLLabel;
    Label5: TLabel;
    edRatioExclusionThreshold: TValidatedEdit;
    edROISize: TValidatedEdit;
    Label4: TLabel;
    scFLDisplay: TScopeDisplay;
    scRDisplay: TScopeDisplay;
    scADCDisplay: TScopeDisplay;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure edTDisplayKeyPress(Sender: TObject; var Key: Char);
    procedure bTDisplayDoubleClick(Sender: TObject);
    procedure bTDisplayHalfClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ckDisplayFluorescenceClick(Sender: TObject);
    procedure ckDisplayADCClick(Sender: TObject);
    procedure scADCDisplayCursorChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cbROIChange(Sender: TObject);
    procedure cbSubROIChange(Sender: TObject);
    procedure ckDisplayRClick(Sender: TObject);
    procedure edRDisplayMaxKeyPress(Sender: TObject; var Key: Char);
    procedure edRatioExclusionThresholdKeyPress(Sender: TObject;
      var Key: Char);
    procedure cbNumeratorChange(Sender: TObject);
    procedure cbDenominatorChange(Sender: TObject);
    procedure edROISizeKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }


    pFLDisplayBuf : Pointer ;   // Pointer to fluorescence time course data buffer
    FLLatestValues : Array[0..MaxFrameType] of Integer ;

    TimeLapseMode : Boolean ;   // Time lapse mode flag
    NumFrameTypes : Integer ;  // No. of excitation light frames types in use
    FrameTypes : Array[0..MaxFrameType] of string ;      // Frame wavelengths

    pADCBuf : Pointer ;
    ADCMaxValue : Integer ;
    DACUpdateInterval : Single ;

    ADCDisplayBuf : Array[0..((MaxADCChannels)*MaxDisplayScans*4)-1] of SmallInt ;

    ADCDisplayScansPerPoint : Single ;
    ADCDisplayCursorUpdated : Boolean ;

    ADCNumScansPerBlock : Integer ;  // No. of A/D channel scans per display block
    ADCNumPointsPerBlock : Integer ; // No. of A/D samples per display block
    ADCMaxBlocksDisplayed : Integer ; // Max. no. of blocks in display
    ADCNumBlocksDisplayed : Integer ; // Current no. of A/D min-max blocks displayed
    ADCBlockCount : Integer ;
    ADCDispPointer : Integer ;

    // A/D sample block Min./max. buffers
    ADCyMin : Array[0..MaxADCChannels-1] of Integer ;
    ADCyMax : Array[0..MaxADCChannels-1] of Integer ;
    ADCyMinAt : Array[0..MaxADCChannels-1] of Integer ;
    ADCyMaxAt : Array[0..MaxADCChannels-1] of Integer ;

    // Fluorescence ratio time course buffers
    RDisplayBuf : Array[0..99999] of Integer ;    // Image time course buffer
    RatioExclusionThreshold : Single ;

    // Display counters
//    NumScans : Integer ;
    NumScansPerBlock : Integer ;
    NumSamplesPerBlock : Integer ;
    NumPointsPerBlock : Integer ;

    ResizeCounter : Integer ;

    function GetFLYMax( i : Integer ) : Single ;
    procedure SetFLYMax( i : Integer ; Value : single ) ;
    function GetFLYMin( i : Integer ) : Single ;
    procedure SetFLYMin( i : Integer ; Value : single ) ;
    function GetADCYMax( i : Integer ) : Single ;
    procedure SetADCYMax( i : Integer ; Value : single ) ;
    function GetADCYMin( i : Integer ) : Single ;
    procedure SetADCYMin( i : Integer ; Value : single ) ;
    procedure SetDisplayUnits ;
    function GetDisplayGrid : Boolean ;
    procedure SetDisplayGrid( Value : Boolean ) ;
    function GetROISize : Integer ;
    procedure ResizeControls ;

  public
    { Public declarations }
    PlotAvailable : Boolean ;
    CurrentFrame : Integer ;       // Current frame on display

    AllowClose : Boolean ;           // Set TRUE to allow window to close
    ADCDisplayFull : Boolean ;
    FLDisplayFull : Boolean ;

    SelectedROI : Integer ;          // ROI selected for plotting
    SelectedSubROI : Integer ;       // ROI selected for subtraction

    FLDisplayPointer : Integer ;
    procedure MagnifyChannelDisplay( ChanNum : Integer ) ;
    procedure ReduceChannelDisplay( ChanNum : Integer ) ;
    procedure ADCInitialiseDisplay(
              pADCBufIn : Pointer ;
              ADCMaxValueIn : Integer ;
              DACUpdateIntervalIn : Single
              ) ;

    procedure ADCUpdateDisplay(
          ADCBuf : Array of SmallInt ;
          ADCNumSamplesInBuffer : Integer ;
          var ADCOldestScan : Integer ;
          var ADCLatestScan : Integer
          ) ;

    procedure ClearDisplays ;

    procedure FLInitialiseDisplay(
              pFLDisplayBufIn : Pointer ;
              TimeLapseModeIn : Boolean ;
              FrameTypesIn : Array of String ;
              NumFrameTypesIn : Integer ;
              UpdateYRange : Boolean
              ) ;
    function FLUpdateDisplay(
             SpectrumMode : Boolean ) : Boolean ;

    procedure AddMarker( MarkerText : String ) ;

    procedure ZoomOutAll ;

    procedure ClearROILists ;
    procedure AddToROILists ;

    Property FLDisplayYMax[ i : Integer ] : Single read GetFLYMax write SetFLYMax ;
    Property FLDisplayYMin[ i : Integer ] : Single read GetFLYMin write SetFLYMin ;
    Property ADCDisplayYMax[ i : Integer ] : Single read GetADCYMax write SetADCYMax ;
    Property ADCDisplayYMin[ i : Integer ] : Single read GetADCYMin write SetADCYMin ;
    Property DisplayGrid : Boolean read GetDisplayGrid write SetDisplayGrid ;
    Property ROISize : Integer read GetROISize ;
//    Property TDisplay : Single read SetTDisplay write GetTDisplay ;

  end;

var
  RecPlotFrm: TRecPlotFrm;

implementation

uses Main, LabIOUnit, Recunit, SealTest ;

type
    TIntArray = Array[0..999999] of Integer ;
    pIntArray = ^TIntArray ;

{$R *.dfm}

procedure TRecPlotFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
    i : Integer ;
begin

     // Duration of analogue signals display window
     edTDisplay.Value := MainFrm.ADCDisplayWindow ;

     edROISize.Value := MainFrm.ROISize ;

     Top := 20 ;
     Left := MainFrm.ClientWidth - Width - 10 ;

     // Clear latest values buffer
     for i := 0 to High(FLLatestValues) do FLLatestValues[i] := 0 ;
     lbFLDisplay.Caption := '' ;
     lbRDisplay.Caption := '' ;
     lbADCDisplay.Caption := '' ;

     ckDisplayFluorescence.Checked := True ;
     ckDisplayADC.Checked := True ;
     ckDisplayR.Checked := False ;
     RatioExclusionThreshold := 1.0 ;

     FLDisplayFull := False ;
     ADCDisplayFull := False ;
     AllowClose := False ;

     ClearROILists ;

     // Request a resize on first Update
     ResizeCounter := 1 ;

     end;


procedure TRecPlotFrm.FormResize(Sender: TObject);
// -----------------------------------------------
// Re-size/locate controls when form size changes
// -----------------------------------------------
begin
    ResizeCounter := 5 ;
    end ;


procedure TRecPlotFrm.ResizeControls ;
// -----------------------------------------------
// Re-size/locate controls on form
// -----------------------------------------------
var
    DisplayHeight : Integer ;
    NumDisplays : Integer ;
    iTop : Integer ;
begin

     if ResizeCounter > 1 then Dec(ResizeCounter) ;
     if ResizeCounter < 1 then Exit ;
     ResizeCounter := 0 ;

     ROIGrp.Height := ClientHeight - ROIGrp.Top - 5 ;

     // Set height of display cursor readout
     lbFLDisplay.Height := (scFLDisplay.NumChannels + 1)*15 + 2 ;
     if not lbFLDisplay.Visible then lbFLDisplay.Height := 2 ;

     lbRDisplay.Height := 15 + 2 ;
     if not lbRDisplay.Visible then lbFLDisplay.Height := 2 ;

     lbADCDisplay.Height := (scADCDisplay.NumChannels + 1)*15 ;
     if not lbADCDisplay.Visible then lbADCDisplay.Height := 2 ;

     lbRDisplay.Top := lbFLDisplay.Top + lbFLDisplay.Height + 2 ;
     lbADCDisplay.Top := lbRDisplay.Top + lbRDisplay.Height + 2 ;

     // Control panel
     TDisplayPanel.Top := ClientHeight
                           - TDisplayPanel.Height - 5 ;
     ckFixZeroLevels.Top := TDisplayPanel.Top ;
     ckFixZeroLevels.Left := scFLDisplay.Left ;

     // Set size & location of time course displays

     if MainFrm.ADCNumChannels <= 0 then begin
        ckDisplayADC.Visible := False ;
        ckDisplayADC.Checked := False ;
        end ;

     scFLDisplay.Visible := ckDisplayFluorescence.Checked ;
     scADCDisplay.Visible := ckDisplayADC.Checked ;
     scRDisplay.Visible := ckDisplayR.Checked ;

     NumDisplays := 0 ;
     if scFLDisplay.Visible then Inc(NumDisplays) ;
     if scADCDisplay.Visible then Inc(NumDisplays) ;
     if scRDisplay.Visible then Inc(NumDisplays) ;
     DisplayHeight := Max( TDisplayPanel.Top - scFLDisplay.Top - 1, 2 ) div Max(NumDisplays,1) ;

     iTop := scFLDisplay.Top ;

     // Fluorescence display
     if scFLDisplay.Visible then begin
        scFLDisplay.Height := Max( DisplayHeight - 5, 2) ;
        iTop := iTop + DisplayHeight ;
        end
     else scFLDisplay.Height := 2 ;

     // Ratio display
     scRDisplay.Top := iTop ;
     if scRDisplay.Visible then begin
        scRDisplay.Height := Max( DisplayHeight - 5, 2) ;
        iTop := iTop + DisplayHeight ;
        end
     else scRDisplay.Height := 2 ;

     // A/D display
     scADCDisplay.Top := iTop ;
     if scADCDisplay.Visible then begin
        scADCDisplay.Height := Max( DisplayHeight - 5, 2) ;
        iTop := iTop + DisplayHeight ;
        end
     else scADCDisplay.Height := 2 ;

     scFLDisplay.Width := Max( ClientWidth - scADCDisplay.Left - 5,2 ) ;
     scRDisplay.Width := scFLDisplay.Width ;
     scADCDisplay.Width := scFLDisplay.Width ;

     TDisplayPanel.Left := scADCDisplay.Left + scADCDisplay.Width - TDisplayPanel.Width ;

     end;


procedure TRecPlotFrm.SetDisplayUnits ;
// ----------------------
// Set display time units
// ----------------------
var
    TFrameGroupInterval : Single ;
begin

    if rbTDisplayUnitsSecs.Checked then begin
       edTDisplay.Units := 's' ;
       edTDisplay.Scale := 1.0 ;

       end
    else begin
       edTDisplay.Units := 'm' ;
       edTDisplay.Scale := 1.0/60.0 ;
       end ;

    if TimeLapseMode then begin
       // Time lapse mode
        TFrameGroupInterval := MainFrm.TimeLapseInterval  / NumFrameTypes ; // / scFLDisplay.NumChannels 29/7/10;
       end
    else begin
       // Continuous recording
       TFrameGroupInterval := MainFrm.Cam1.FrameInterval ;
       end ;
    if TFrameGroupInterval <= 0.0 then Exit ;

    scFLDisplay.TScale := TFrameGroupInterval*edTDisplay.Scale ;
    scFLDisplay.TUnits := edTDisplay.Units ;

    // Determine no. of points in display
    scFLDisplay.MaxPoints := Round( edTDisplay.Value/TFrameGroupInterval ) + 1 ;
    edTDisplay.Value := (scFLDisplay.MaxPoints-1)*TFrameGroupInterval ;
    scFLDisplay.XMax := scFLDisplay.MaxPoints ;

    scFLDisplay.Invalidate ;

    // Set A/D channel display
    if (MainFrm.ADCNumChannels > 0) and (ADCNumPointsPerBlock > 0) then begin
       scADCDisplay.TScale := (MainFrm.ADCScanInterval*ADCNumScansPerBlock*edTDisplay.Scale)
                         /ADCNumPointsPerBlock ;
       scADCDisplay.TUnits := edTDisplay.Units ;
       scADCDisplay.Invalidate ;
       end ;

    end ;


procedure TRecPlotFrm.ADCInitialiseDisplay(
          pADCBufIn : Pointer ;
          ADCMaxValueIn : Integer ;
          DACUpdateIntervalIn : Single
          ) ;
// ------------------------------
// Initialise analogue inputs display
// ------------------------------
var
     i : Integer ;
     NumScans : Integer ;
     ch : Integer ;
     NumSamplesPerBlock : Integer ;

begin

     if (MainFrm.ADCNumChannels <= 0) or (pADCBufIn = Nil) then Exit ;
     if DACUpdateIntervalIn <= 0.0 then Exit ;

     pADCBuf := pADCBufIn ;
     ADCMaxValue := ADCMaxValueIn ;
     DACUpdateInterval := DACUpdateIntervalIn ;

     // Set up A/D signals display window
     scADCDisplay.MaxADCValue := ADCMaxValue ;
     scADCDisplay.MinADCValue := -ADCMaxValue -1 ;

     scADCDisplay.NumChannels := MainFrm.ADCNumChannels ;

     // No. of multi-channel scans to be displayed
     NumScans := Max( Round(edTDisplay.Value/DACUpdateInterval),2 ) ;
     edTDisplay.Value := NumScans*DACUpdateInterval ;

     // Size of display compression block
     ADCNumScansPerBlock := Max( NumScans div MaxDisplayScans,1) ;
     NumSamplesPerBlock := ADCNumScansPerBlock*MainFrm.ADCNumChannels ;

     // No. of displayed points per block
     ADCNumPointsPerBlock := Min(ADCNumScansPerBlock,2) ;

     // Max. number of points in display
     ADCMaxBlocksDisplayed := Min( (NumScans div ADCNumScansPerBlock),
                                   MaxDisplayScans )  ;
     scADCDisplay.MaxPoints := ADCMaxBlocksDisplayed*ADCNumPointsPerBlock ;

     // Add zero level cursors
     scADCDisplay.ClearHorizontalCursors ;
     for ch :=  0 to scADCDisplay.NumChannels-1 do
         scADCDisplay.AddHorizontalCursor( ch, clBlue, True, 'z' ) ;

     { Set channel information }
     for ch := 0 to scADCDisplay.NumChannels-1 do begin
            scADCDisplay.ChanOffsets[ch] := MainFrm.ADCChannel[Ch].ChannelOffset ;
            scADCDisplay.ChanUnits[ch] := MainFrm.ADCChannel[Ch].ADCUnits ;
            scADCDisplay.ChanName[ch] := MainFrm.ADCChannel[Ch].ADCName ;
            scADCDisplay.ChanScale[ch] := MainFrm.ADCChannel[ch].ADCScale ;
            scADCDisplay.yMin[ch] := MainFrm.ADCChannel[Ch].yMin ;
            scADCDisplay.yMax[ch] := MainFrm.ADCChannel[Ch].yMax ;
            scADCDisplay.HorizontalCursors[ch] := MainFrm.ADCChannel[Ch].ADCZero ;
            scADCDisplay.ChanVisible[ch] := MainFrm.ADCChannel[Ch].InUse ;
            end ;

     // Set display time scaling
     SetDisplayUnits ;

     scADCDisplay.xMin := 0 ;
     scADCDisplay.xMax := scADCDisplay.MaxPoints-1 ;
        // Enable/disable display calibration grid
        //scADCDisplay.DisplayGrid := MainFrm.mnDisplayGrid.Checked ;
     scADCDisplay.SetDataBuf( @ADCDisplayBuf ) ;

     scADCDisplay.NumPoints := 0 ;
     ADCNumBlocksDisplayed := 0 ;
     ADCBlockCount := ADCNumScansPerBlock ;
     ADCDispPointer := 0 ;
     ADCDisplayFull := False ;

     scADCDisplay.Invalidate ;

     // Clear markers on display
     scADCDisplay.ClearMarkers ;

     ResizeCounter := 1 ;

     end ;


procedure TRecPlotFrm.ADCUpdateDisplay(
          ADCBuf : Array of SmallInt ;
          ADCNumSamplesInBuffer : Integer ;
          var ADCOldestScan : Integer ;
          var ADCLatestScan : Integer
          ) ;
// ----------------------------
// Update A/D signals display
// ----------------------------
var
    Done : Boolean ;
    ch : Integer ;
    y : Integer ;
    s : String ;
begin

     ResizeControls ;

     if ADCOldestScan = ADCLatestScan then Done := True
                                      else Done := False ;
     While not Done do begin

        // Initialise block
        if ADCBlockCount >= ADCNumScansPerBlock then begin
           for ch := 0 to MainFrm.ADCNumChannels-1 do begin
               ADCyMin[ch] := ADCMaxValue ;
               ADCyMax[ch] := -ADCMaxValue -1 ;
               end ;
           ADCBlockCount := 0 ;
           end ;

        // Determine min. / max. value & order of samples within compression block
        for ch := 0 to MainFrm.ADCNumChannels-1 do begin

            y := ADCBuf[ADCOldestScan+ch] ;

            if y < ADCyMin[ch] then begin
               ADCyMin[ch] := y ;
               ADCyMinAt[ch] := ADCBlockCount ;
               end ;
            if y > ADCyMax[ch] then begin
               ADCyMax[ch] := y ;
               ADCyMaxAt[ch] := ADCBlockCount ;
               end ;
            end ;
        Inc(ADCBlockCount) ;

        // When block complete ... write min./max. to display buffer
        if ADCBlockCount >= ADCNumScansPerBlock then begin

           // First point
           for ch := 0 to MainFrm.ADCNumChannels-1 do begin
               if ADCyMaxAt[ch] <= ADCyMinAt[ch] then
                  ADCDisplayBuf[ADCDispPointer] := ADCyMax[ch]
               else ADCDisplayBuf[ADCDispPointer] := ADCyMin[ch] ;
               ADCDispPointer := ADCDispPointer + 1 ;
               end ;

           // Second point
           if ADCBlockCount > 1 then begin
              for ch := 0 to MainFrm.ADCNumChannels-1 do begin
                  if ADCyMaxAt[ch] >= ADCyMinAt[ch] then
                     ADCDisplayBuf[ADCDispPointer] := ADCyMax[ch]
                  else ADCDisplayBuf[ADCDispPointer] := ADCyMin[ch] ;
                  ADCDispPointer := ADCDispPointer + 1 ;
                  end ;
              end ;

           Inc(ADCNumBlocksDisplayed) ;
           end ;

        // Increment pointer to next available scan
        ADCOldestScan := ADCOldestScan + MainFrm.ADCNumChannels ;
        if ADCOldestScan >= ADCNumSamplesInBuffer then
           ADCOldestScan := ADCOldestScan - ADCNumSamplesInBuffer ;
        if (ADCOldestScan = ADCLatestScan) or
           (ADCNumBlocksDisplayed >= ADCMaxBlocksDisplayed) then Done := True ;

        end ;

     // Display latest points added to display buffer
     if  ADCNumBlocksDisplayed > 0 then begin
         scADCDisplay.DisplayNewPoints( ADCDispPointer div MainFrm.ADCNumChannels );
         end ;

     if ADCNumBlocksDisplayed >= ADCMaxBlocksDisplayed then ADCDisplayFull := True
                                                       else ADCDisplayFull := False ;

     // Update numerical readout of A/D channel signals
     s := '' ;
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
         s := s + format('%s=%7.4g %s<br>',
         [MainFrm.ADCChannel[ch].ADCName,
         ( ADCBuf[ADCLatestScan + MainFrm.ADCChannel[ch].ChannelOffset]
           - scADCDisplay.HorizontalCursors[ch] )*MainFrm.ADCChannel[ch].ADCScale,
         MainFrm.ADCChannel[ch].ADCUnits]) ;
         end ;
     lbADCDisplay.Caption := s ;

     end ;


procedure TRecPlotFrm.ClearDisplays ;
// --------------------------------
// Clear time course display graphs
// --------------------------------
begin

    FLInitialiseDisplay( pFLDisplayBuf, TimeLapseMode, FrameTypes, NumFrameTypes, False ) ;
    ADCInitialiseDisplay( pADCBuf,ADCMaxValue,DACUpdateInterval) ;
    SetDisplayUnits ;

    end ;


procedure TRecPlotFrm.FLInitialiseDisplay(
          pFLDisplayBufIn : Pointer ;
          TimeLapseModeIn : Boolean ;
          FrameTypesIn : Array of String ;
          NumFrameTypesIn : Integer ;
          UpdateYRange : Boolean
          ) ;
// --------------------------------------
// Clear fluorescence time course display
// --------------------------------------
var
    i,ch : Integer ;
begin

     if pFLDisplayBufIn = Nil then Exit ;

     pFLDisplayBuf := pFLDisplayBufIn ;
     TimeLapseMode := TimeLapseModeIn ;

     // Update frame types
     NumFrameTypes := NumFrameTypesIn ;
     for i := 0 to NumFrameTypesIn-1 do
         FrameTypes[i] := FrameTypesIn[i] ;

     // Set numerator and denominator wavelength lists
     cbNumerator.Clear ;
     for i := 0 to NumFrameTypes-1 do begin
          if FrameTypes[i] = '' then FrameTypes[i] := ' ' ;
          cbNumerator.Items.Add( FrameTypes[i] ) ;
          end ;
     cbDenominator.Items.Assign( cbNumerator.Items ) ;

     cbNumerator.ItemIndex := Min(MainFrm.TimeCourseRatioNumerator,
                                  cbNumerator.Items.Count-1) ;
     cbDenominator.ItemIndex := Min(MainFrm.TimeCourseRatioDenominator,
                                    cbDenominator.Items.Count-1) ;
     edRDisplayMax.Value := MainFrm.TimeCourseRatioDisplayMax ;
     edRatioExclusionThreshold.Value := MainFrm.TimeCourseRatioThreshold ;

    // Set up fluorescence signals display window
    scFLDisplay.MaxADCValue := MainFrm.Cam1.GreyLevelMax ;
    scFLDisplay.MinADCValue := MainFrm.Cam1.GreyLevelMin ; ;

    // No. of points in A/D window
    MainFrm.ADCDisplayWindow := edTDisplay.Value ;
    if TimeLapseMode then begin
       // Time lapse mode
       scFLDisplay.MaxPoints := Round( MainFrm.ADCDisplayWindow/MainFrm.TimeLapseInterval ) ;
       MainFrm.ADCDisplayWindow := scFLDisplay.MaxPoints*MainFrm.TimeLapseInterval ;
       scFLDisplay.TScale := MainFrm.TimeLapseInterval / NumFrameTypes ;
       end
    else begin
       // Continuous recording
       scFLDisplay.MaxPoints := Round( MainFrm.ADCDisplayWindow/(MainFrm.Cam1.FrameInterval) ) ;
       MainFrm.ADCDisplayWindow := scFLDisplay.MaxPoints*MainFrm.Cam1.FrameInterval ;
       scFLDisplay.TScale := MainFrm.Cam1.FrameInterval ;
       end ;
    edTDisplay.Value := MainFrm.ADCDisplayWindow ;

    FLDisplayPointer := 0 ;
    scFLDisplay.NumPoints := 0 ;
    scFLDisplay.ClearMarkers ;
    FLDisplayFull := False ;

     // Add zero level cursors
     scFLDisplay.ClearHorizontalCursors ;
     for ch :=  0 to NumFrameTypes-1 do
         scFLDisplay.AddHorizontalCursor( ch, clBlue, True, 'z' ) ;

    // Allocate a display channel for each wavelength in use
    scFLDisplay.NumChannels := NumFrameTypes ;
    scFLDisplay.xMax := scFLDisplay.MaxPoints-1 ;
    scFLDisplay.xMin := 0 ;

    for ch := 0 to scFLDisplay.NumChannels-1 do begin
        scFLDisplay.ChanOffsets[ch] := ch ;
        scFLDisplay.ChanUnits[ch] := '' ;
        scFLDisplay.ChanName[ch] := FrameTypes[ch] ;
        scFLDisplay.ChanScale[ch] := 1.0 ;
        if UpdateYRange then begin
           scFLDisplay.yMin[ch] := 0 ;
           scFLDisplay.yMax[ch] := Min( MainFrm.GreyHi[ch]*2.0, MainFrm.Cam1.GreyLevelMax ) ;
           end ;
        scFLDisplay.HorizontalCursors[ch] := 0 ;
        //scFLDisplay.ChanVisible[ch] := True ;
        end ;

    scFLDisplay.SetDataBuf( pFLDisplayBuf ) ;
    scFLDisplay.NumBytesPerSample := 4 ;

    // Set up fluorescence ratio display window

    scRDisplay.MaxADCValue := MainFrm.Cam1.GreyLevelMax ;
    scRDisplay.MinADCValue := MainFrm.Cam1.GreyLevelMin ; ;

    scRDisplay.MaxPoints := scFLDisplay.MaxPoints ;
    scRDisplay.MaxPoints := scFLDisplay.MaxPoints ;
    scRDisplay.NumPoints := 0 ;

    scRDisplay.TScale := scFLDisplay.TScale ;
    scRDisplay.TUnits := scFLDisplay.TUnits ;

    scRDisplay.ClearMarkers ;

     // Add zero level cursors
     scRDisplay.ClearHorizontalCursors ;
     scRDisplay.AddHorizontalCursor( 0, clBlue, True, 'z' ) ;

    // Allocate a display channel for each wavelength in use
    scRDisplay.NumChannels := 1 ;
    scRDisplay.xMax := scRDisplay.MaxPoints-1 ;
    scRDisplay.xMin := 0 ;

    scRDisplay.ChanOffsets[0] := 0 ;
    scRDisplay.ChanUnits[0] := '' ;
    scRDisplay.ChanName[0] := LeftStr(cbNumerator.Text,3) + '/' + LeftStr(cbDenominator.Text,3) ;
    scRDisplay.ChanScale[0] := edRDisplayMax.Value / scRDisplay.MaxADCValue ;
    if UpdateYRange then begin
       scRDisplay.yMin[0] := 0 ;
       scRDisplay.yMax[0] := MainFrm.Cam1.GreyLevelMax ;
       end ;
    scRDisplay.HorizontalCursors[0] := 0 ;
    //scRDisplay.ChanVisible[0] := True ;

    scRDisplay.SetDataBuf( @RDisplayBuf ) ;
    scRDisplay.NumBytesPerSample := 4 ;

    // Set display time scaling
    SetDisplayUnits ;

    // Update public ROI selection variables
    SelectedROI := cbROI.ItemIndex ;
    SelectedSubROI := cbSubROI.ItemIndex - 1 ;

    ResizeCounter := 1 ;

    end ;


function TRecPlotFrm.FLUpdateDisplay(
         SpectrumMode : Boolean
         )  : Boolean ;
// ----------------------------------------
// Update fluorescence time course display
// ----------------------------------------
var
    Done : Boolean ;
    s : String ;
    i,iFT,iFTNum,iFTDen,j : Integer ;
    yNum,yDen,YScale,R : Single ;
    y : single ;
begin

   // Resize controls on form (if required)
   ResizeControls ;

   //outputdebugString(PChar(format('FLDisplayPointer %d',[FLDisplayPointer]))) ;
     if FLDisplayPointer < (scFLDisplay.MaxPoints*NumFrameTypes) then begin
        scFLDisplay.DisplayNewPoints( (FLDisplayPointer div NumFrameTypes)-1 );
        FLDisplayFull := False ;
        Result := False ;
        end
     else begin
        ClearDisplays ;
        scFLDisplay.NumPoints := 0 ;
        FLDisplayPointer := 0 ;
        scFLDisplay.ClearMarkers ;
        FLDisplayFull := True ;
        scRDisplay.NumPoints := 0 ; // Also clear ratio display
        scRDisplay.ClearMarkers ;
        Result := True ;
        end ;

     // Calculate and update ratio display (if required)

     // Ratio display not allowed in spectrum mode
     if ckDisplayR.Checked and SpectrumMode then ckDisplayR.Checked := False ;

     if ckDisplayR.Checked then begin

        // Frame types to be ratioed
        iFTNum := cbNumerator.ItemIndex ;
        iFTDen := cbDenominator.ItemIndex ;

        if scRDisplay.ChanScale[0] > 0.0 then
           YScale := 1.0 / scRDisplay.ChanScale[0]
        else YScale := 1.0 ;
        scRDisplay.ChanScale[0] := 1.0 / YScale ;
        scRDisplay.ChanName[0] := LeftStr(cbNumerator.Text,3) + '/' + LeftStr(cbDenominator.Text,3) ;
        for i := scRDisplay.NumPoints-1 to scFLDisplay.NumPoints-1 do begin
            j := i*NumFrameTypes ;
            yNum := pIntArray(pFLDisplayBuf)^[j+iFTNum] ;
            yDen := pIntArray(pFLDisplayBuf)^[j+iFTDen] ;
            if yDen >= RatioExclusionThreshold then R := (yNum/yDen)
                                               else R := 0.0 ;
            RDisplayBuf[i] := Round( R*YScale ) ;
            end ;
        scRDisplay.DisplayNewPoints( scFLDisplay.NumPoints-1 );

        lbRDisplay.Caption := format('%s=%.3g',[scRDisplay.ChanName[0],R]) ;

        end ;

     // Update numerical readout

     for i := 1 to NumFrameTypes do begin
         j := FLDisplayPointer-i ;
         if j >= 0 then begin
            iFT := (FLDisplayPointer-i) mod NumFrameTypes ;
            FLLatestValues[iFT] := pIntArray(pFLDisplayBuf)^[FLDisplayPointer-i]
                                   - scFLDisplay.ChanZero[iFT] ;
            end ;
         end ;

     s := '' ;
     for iFT := 0 to NumFrameTypes-1 do begin
         s := s + format('%s=%d<br>',[FrameTypes[iFT],FLLatestValues[iFT]]) ;
         end ;
     lbFLDisplay.Caption := s ;

     end ;


function TRecPlotFrm.GetFLYMax( i : Integer ) : Single ;
// ------------------------------------
// Get upper limit of flourescence plot
// ------------------------------------
begin
    Result := scFLDisplay.YMax[i] ;
    end ;

procedure TRecPlotFrm.SetFLYMax( i : Integer ; Value : single ) ;
// ------------------------------------
// Set upper limit of flourescence plot
// ------------------------------------
begin
    scFLDisplay.YMax[i] := Min(Value,MainFrm.Cam1.GreyLevelMax) ;
    scFLDisplay.Invalidate ;
    end ;

function TRecPlotFrm.GetFLYMin( i : Integer ) : Single ;
// ------------------------------------
// Get lower limit of flourescence plot
// ------------------------------------

begin
    Result := scFLDisplay.YMin[i] ;

    end ;


procedure TRecPlotFrm.SetFLYMin( i : Integer ; Value : single ) ;
// ------------------------------------
// Set lower limit of flourescence plot
// ------------------------------------

begin
    scFLDisplay.YMin[i] := Value ;
    scFLDisplay.Invalidate ;
    end ;


function TRecPlotFrm.GetADCYMax( i : Integer ) : Single ;
// ------------------------------------
// Get upper limit of analogue channel plot
// ------------------------------------

begin
    Result := scADCDisplay.YMax[i] ;
    end ;

procedure TRecPlotFrm.SetADCYMax( i : Integer ; Value : single ) ;
// ------------------------------------
// Set upper limit of analogue channel plot
// ------------------------------------
begin
    scADCDisplay.YMax[i] := Value ;
    scADCDisplay.Invalidate ;
    end ;

function TRecPlotFrm.GetADCYMin( i : Integer ) : Single ;
// ------------------------------------
// Get lower limit of analogue channel plot
// ------------------------------------
begin
    Result := scADCDisplay.YMin[i] ;
    end ;


procedure TRecPlotFrm.SetADCYMin( i : Integer ; Value : single ) ;
// ------------------------------------
// Set lower limit of analogue channel plot
// ------------------------------------
begin
    scFLDisplay.YMin[i] := Value ;
    scFLDisplay.Invalidate ;
    end ;


procedure TRecPlotFrm.SetDisplayGrid( Value : Boolean ) ;
// ------------------------------------
// Set chart display grid on/off
// ------------------------------------
begin
    scFLDisplay.DisplayGrid := Value ;
    scADCDisplay.DisplayGrid := Value ;
    scRDisplay.DisplayGrid := Value ;
    end ;


function TRecPlotFrm.GetDisplayGrid : Boolean ;
// ------------------------------------
// Get chart display grid on/off state
// ------------------------------------
begin
    Result := scFLDisplay.DisplayGrid ;
    end ;


function TRecPlotFrm.GetROISize : Integer ;
// ------------------------------------
// Get size of live ROI area
// ------------------------------------
begin
    Result := Round( edROISize.Value ) ;
    end ;


procedure TRecPlotFrm.MagnifyChannelDisplay(
          ChanNum : Integer ) ;
// ------------------------------------
// Magnify selected A/D channel display
// ------------------------------------
begin
     if ChanNum >= MainFrm.ADCNumChannels then begin
       scFLDisplay.YZoom(ChanNum - MainFrm.ADCNumChannels, -50.0) ;
       end
     else scADCDisplay.YZoom(ChanNum, -50.0) ;
     end ;


procedure TRecPlotFrm.ReduceChannelDisplay( ChanNum : Integer ) ;
// ------------------------------------
// Reduce selected A/D channel display
// ------------------------------------
begin
     if ChanNum >= MainFrm.ADCNumChannels then begin
       scFLDisplay.YZoom(ChanNum - MainFrm.ADCNumChannels, 50.0) ;
       end
     else scADCDisplay.YZoom(ChanNum, 50.0) ;
     end ;



procedure TRecPlotFrm.edTDisplayKeyPress(Sender: TObject; var Key: Char);
// --------------------------------------------------------
// Request display update when A/D display duration changed
// --------------------------------------------------------
begin
     if key = #13 then begin
        MainFrm.ADCDisplayWindow := edTDisplay.Value ;
        ClearDisplays ;
        end ;
     end;

procedure TRecPlotFrm.bTDisplayDoubleClick(Sender: TObject);
// ----------------------------------------
// Double the duration of the display window
// ----------------------------------------
begin
     edTDisplay.Value := 2.0*edTDisplay.Value ;
     ClearDisplays ;
     end;


procedure TRecPlotFrm.bTDisplayHalfClick(Sender: TObject);
// ----------------------------------------
// Halve the duration of the display window
// ----------------------------------------
begin
     edTDisplay.Value := 0.5*edTDisplay.Value ;
     MainFrm.ADCDisplayWindow := edTDisplay.Value ;
     ClearDisplays ;
     end;


procedure TRecPlotFrm.AddMarker( MarkerText : String ) ;
// -------------------------------
// Add marker to fluorescence plot
// -------------------------------
begin
     scFLDisplay.AddMarker( FLDisplayPointer div scFLDisplay.NumChannels, MarkerText );
     end ;

     
procedure TRecPlotFrm.FormClose(Sender: TObject; var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
begin

     if AllowClose then Action := caFree
                   else Action := caMinimize ;

     MainFrm.ROISize := Round(edROISize.Value) ;

     end;

procedure TRecPlotFrm.ckDisplayFluorescenceClick(Sender: TObject);
// --------------------------------------------------
// Display fluorescence time course check box changed
// --------------------------------------------------
begin
     Resize ;
     end;

     
procedure TRecPlotFrm.ckDisplayADCClick(Sender: TObject);
// --------------------------------------------------
// Display A/D signal time course check box changed
// --------------------------------------------------
begin
     Resize ;
     end;


procedure TRecPlotFrm.scADCDisplayCursorChange(Sender: TObject);
// ----------------------------
// Display cursors/zoom changed
// ----------------------------
var
    ch : Integer ;
begin
    for ch := 0 to scADCDisplay.NumChannels-1 do begin
        MainFrm.ADCChannel[Ch].yMin := scADCDisplay.yMin[ch] ;
        MainFrm.ADCChannel[Ch].yMax := scADCDisplay.yMax[ch] ;
        if ckFixZeroLevels.Checked
           and (scADCDisplay.HorizontalCursors[ch] <> 0) then begin
           scADCDisplay.HorizontalCursors[ch] := 0 ;
           end ;
        MainFrm.ADCChannel[Ch].ADCZero := scADCDisplay.HorizontalCursors[ch] ;
        MainFrm.ADCChannel[Ch].InUse := scADCDisplay.ChanVisible[ch] ;
        end ;

    end;

procedure TRecPlotFrm.FormActivate(Sender: TObject);
// -----------------------
// Form has become active
// -----------------------
var
    i : Integer ;
begin

     // Stop seal test (if it is running)
     for i := 0 to MainFrm.MDIChildCount-1 do begin
         if (MainFrm.MDIChildren[i].Name = 'SealTestFrm') then begin
            TSealTestFrm(MainFrm.MDIChildren[i]).StopSealTest ;
            end ;
         end ;

     // Re-start camera (if necessary)
     for i := 0 to MainFrm.MDIChildCount-1 do begin
         if (MainFrm.MDIChildren[i].Name = 'RecordFrm') then begin
            if not TRecordFrm(MainFrm.MDIChildren[i]).CameraRunning then
               TRecordFrm(MainFrm.MDIChildren[i]).StartCamera ;
            end ;
         end ;

     end;


procedure TRecPlotFrm.ZoomOutAll ;
// --------------------------------------
// Set all plots to minimum magnification
// --------------------------------------
var
    i : Integer ;
begin

    // Set fluorescence plots
    for i := 0 to scFLDisplay.NumChannels-1 do begin
        scFLDisplay.YMax[i] := scFLDisplay.MaxADCValue ;
        scFLDisplay.YMin[i] := 0.0 ;
        end ;
    scFLDisplay.Invalidate ;

    // Set A/D plots
    scADCDisplay.ZoomOut ;

    end ;


procedure TRecPlotFrm.ClearROILists ;
// -------------------------------------
// Clear additional ROIs (leaving ROI.1)
// -------------------------------------
begin

    // Set ROI list to ROI 1
    cbROI.Items.Clear ;
    cbROI.Items.Add('ROI 1') ;
    cbROI.ItemIndex := 0 ;

    // Set subtraction list None and ROI 1
    cbSubROI.Items.Clear ;
    cbSubROI.Items.Add(' ') ;
    cbSubROI.Items.Add('ROI 1') ;
    cbSubROI.ItemIndex := 0 ;

    SelectedROI := cbROI.ItemIndex ;
    SelectedSubROI := cbSubROI.ItemIndex - 1 ;

    end ;


procedure TRecPlotFrm.AddToROILists ;
// ---------------
// Add ROI to list
// ---------------
begin

    cbROI.Items.Add(format('ROI %d',[cbROI.Items.Count+1])) ;
    cbROI.ItemIndex := cbROI.Items.Count-1 ;

    cbSubROI.Items.Add(format('ROI %d',[cbROI.Items.Count])) ;

    SelectedROI := cbROI.ItemIndex ;
    SelectedSubROI := cbSubROI.ItemIndex - 1 ;

    end ;


procedure TRecPlotFrm.cbROIChange(Sender: TObject);
// --------------------
// ROI selection changed
// --------------------
begin
     SelectedROI := cbROI.ItemIndex ;
     end ;

procedure TRecPlotFrm.cbSubROIChange(Sender: TObject);
// ---------------------------------
// Subtraction ROI selection changed
// ---------------------------------
begin
     SelectedSubROI := cbSubROI.ItemIndex - 1 ;
     end ;

procedure TRecPlotFrm.ckDisplayRClick(Sender: TObject);
begin

    Resize ;
    end;

procedure TRecPlotFrm.edRDisplayMaxKeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Ratio display maximum changed
// -----------------------------
begin
     if Key = #13 then begin
        scRDisplay.ChanScale[0] := edRDisplayMax.Value / scRDisplay.MaxADCValue ;
        MainFrm.TimeCourseRatioDisplayMax := edRDisplayMax.Value ;
        end ;
     end;

procedure TRecPlotFrm.edRatioExclusionThresholdKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------------
// Ratio exclusion threshold changed
// ---------------------------------
begin
     if Key = #13 then begin
        RatioExclusionThreshold := edRatioExclusionThreshold.Value ;
        MainFrm.TimeCourseRatioThreshold := edRatioExclusionThreshold.Value ;
        end ;
     end;

procedure TRecPlotFrm.cbNumeratorChange(Sender: TObject);
begin
     MainFrm.TimeCourseRatioNumerator := cbNumerator.ItemIndex ;
     end;

procedure TRecPlotFrm.cbDenominatorChange(Sender: TObject);
begin
     MainFrm.TimeCourseRatioDenominator := cbDenominator.ItemIndex ;
     end;

procedure TRecPlotFrm.edROISizeKeyPress(Sender: TObject; var Key: Char);
begin
     if Key = #13 then begin
        MainFrm.ROISize := Round(edROISize.Value) ;
        end ;
     end;

end.


