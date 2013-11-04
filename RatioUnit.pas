unit RatioUnit;
// -----------------------------------------------
// WinFluor - Ratio image computation module
// (c) J. Dempster, University of Strathclyde 2003
// -----------------------------------------------
// 07.03.07 ... Now copies A/D data correctly when frame sub-ranges selected
// 09.05.08 ... Dual-rate, multiwavelength support added
// 21.011000ssssCaimagenowworks

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, HTMLLabel, ValidatedEdit, ExtCtrls, RangeEdit, IDRFile;

type
  TRatioFrm = class(TForm)
    GroupBox2: TGroupBox;
    rbAllFrames: TRadioButton;
    RadioButton1: TRadioButton;
    edRange: TRangeEdit;
    GroupBox4: TGroupBox;
    lbDeltaFPlot: THTMLLabel;
    rbDeltaF: TRadioButton;
    rbFrameRatio: TRadioButton;
    bOK: TButton;
    bCancel: TButton;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    cbBackgroundROI: TComboBox;
    edInclusionThreshold: TValidatedEdit;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    edLowerLimit: TValidatedEdit;
    edUpperLimit: TValidatedEdit;
    Label6: TLabel;
    IDROut: TIDRFile;
    RatioGrp: TGroupBox;
    Shape1: TShape;
    Label2: TLabel;
    cbNumerator: TComboBox;
    cbDenominator: TComboBox;
    cbEquation: TComboBox;
    ckUseEquation: TCheckBox;
    DeltaFGrp: TGroupBox;
    Label1: TLabel;
    cbFrameType: TComboBox;
    GroupBox7: TGroupBox;
    rbF0FromFrames: TRadioButton;
    edF0Range: TRangeEdit;
    rbF0Constant: TRadioButton;
    edF0Constant: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rbDeltaFClick(Sender: TObject);
    procedure rbFrameRatioClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
    PFrameBuf : PIntArray ;  // Frame buffer
    PAvgBuf : PIntArray ;    // F0 averaging buffer
    PNumBuf : PIntArray ;    // Numerator frame buffer
    PDenomBuf : PIntArray ;  // Denominator frame buffer
    FrameList : pIntArray ;                        // Frame pointer array

    function MeanROIIntensity( ROI : TROI ;
                               FrameBuf : PIntArray ;
                               zExclusionThreshold : Integer
                               ) : Single ;

  public
    { Public declarations }
  end;

var
  RatioFrm: TRatioFrm;

implementation

uses Main, strUtils, maths, math, shared  ;

{$R *.dfm}

procedure TRatioFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i : Integer ;
begin


     edRange.LoLimit := 1 ;
     edRange.HiLimit := MainFrm.IDRFile.NumFrames ;
     edRange.LoValue := edRange.LoLimit ;
     edRange.HiValue := edRange.HiLimit ;

     // Allocate buffers
     GetMem( PFrameBuf, MainFrm.IDRFile.NumPixelsPerFrame*4 ) ;
     GetMem( PAvgBuf, MainFrm.IDRFile.NumPixelsPerFrame*4 ) ;
     GetMem( PNumBuf, MainFrm.IDRFile.NumPixelsPerFrame*4 ) ;
     GetMem( PDenomBuf, MainFrm.IDRFile.NumPixelsPerFrame*4 ) ;
     GetMem( FrameList,MainFrm.IDRFile.NumFrames*MainFrm.IDRFile.NumFrameTypes*4 ) ;

     // Create list of pointers to frames
     MainFrm.IDRFile.CreateFramePointerList(FrameList);

     // Create list of background ROIs
     cbBackgroundROI.Clear ;
     cbBackgroundROI.Items.AddObject( ' ',TObject(0)) ;
     for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then
         cbBackgroundROI.Items.AddObject( format('ROI.%d',[i]),TObject(i)) ;
     cbBackgroundROI.ItemIndex := 0 ;

    // Update frame types
    cbFrameType.Clear ;
    for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
        cbFrameType.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
    cbFrameType.ItemIndex := 0 ;

    cbNumerator.Clear ;
    for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
        cbNumerator.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
    cbNumerator.ItemIndex := 0 ;

    cbDenominator.Clear ;
    for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do
        cbDenominator.Items.Add(MainFrm.IDRFile.FrameType[i]) ;
    cbDenominator.ItemIndex := 0 ;
    // Ensure numerator and denomintor are different
    if (cbDenominator.ItemIndex = cbNumerator.ItemIndex) then begin
       for i := 0 to cbNumerator.Items.Count-1 do
           if i <> cbNumerator.ItemIndex then cbDenominator.ItemIndex := i ;
       end ;

    // Single or frame ratio
    if MainFrm.IDRFile.NumFrameTypes = 1 then begin
       rbDeltaF.Checked := True ;
       rbFrameRatio.Checked := not rbDeltaF.Checked ;
       RatioGrp.Enabled := False ;
       rbFrameRatio.Enabled := False ;
       cbFrameType.Enabled := False ;
       cbNumerator.Enabled := False ;
       cbDenominator.Enabled := False ;
       cbEquation.Enabled := False ;
       end
    else begin
       rbDeltaF.Checked := False ;
       rbFrameRatio.Checked := not rbDeltaF.Checked ;
       RatioGrp.Enabled := True ;
       RatioGrp.Enabled := True ;
       cbFrameType.Enabled := True ;
       cbNumerator.Enabled := True ;
       cbDenominator.Enabled := True ;
       cbEquation.Enabled := True ;
       end ;

    if rbDeltaF.Checked then begin
       edLowerLimit.Value := -50.0 ;
       edUpperLimit.Value := 500.0 ;
       DeltaFGrp.Visible := True ;
       RatioGrp.Visible := False ;
       end
    else begin
       edLowerLimit.Value := 0.0 ;
       edUpperLimit.Value := 10. ;
       DeltaFGrp.Visible := False ;
       RatioGrp.Visible := True ;
       end ;

    // Update available ion binding equations
    cbEquation.Clear ;
    for i := 0 to MainFrm.IDRFile.MaxEquations-1 do
        if MainFrm.IDRFile.Equation[i].InUse then
           cbEquation.Items.AddObject( MainFrm.IDRFile.Equation[i].Name,
                                       TObject(i) ) ;
    if cbEquation.Items.Count > 0 then begin
       cbEquation.Enabled := True ;
       ckUseEquation.Enabled := True ;
       cbEquation.ItemIndex := 0 ;
       end
    else begin
       cbEquation.Enabled := False ;
       ckUseEquation.Enabled := False ;
       end ;

     end;


procedure TRatioFrm.bOKClick(Sender: TObject);
// -------------------------------
// Compute ratio/ion conc. images
// -------------------------------
var

    StartAtFrame : Integer ;
    EndAtFrame : Integer ;
    Frame : Integer ;
    OutFrame : Integer ;
    NumAveraged : Integer ;
    Done : Boolean ;
    OK : Boolean ;
    i : Integer ;
    FileName : String ;
    FileNameTag : String ;

    Intensity : Integer ;     // Pixel intensity value
    INumerator : Integer ;    // Numerator pixel intensity value
    IDenominator : Integer ;  // Denominator pixel intensity value
    IThreshold : Integer ;    // Inclusion threshold pixel intensity value
    iBackg : Integer ;        // Background region of interest no.
    IBackground : Integer ;   // Background pixel intensity value
    INumBackground : Integer ;   // Background pixel intensity value for numerator image
    IDenomBackground : Integer ;   // Background pixel intensity value denominator image
    NumeratorAvailable : Boolean ;   // Numerator frame type available in buffer
    DenominatorAvailable : Boolean ; // Denominator frame type available in buffer
    dF : Single ;

    F0 : Single ;
    StartF0 : Integer ;             // Start of range of F0 frame averaged
    EndF0 : Integer ;              // End of range of F0 frame averaged
    nAvg : Integer ;                // No. of F0 frames averaged
    R : Single ;              // Intensity ratio
    RMin : Single ;           // Min. intensity ratio
    RMax : Single ;           // Max. intensity ratio
    Keff : Single ;           // Binding coeff.
    Concentration : Single ;  // Computed ion concentration
    FType : Integer ;         // Frame type
    MinValue : Single ;       // Output image min. value
    MaxValue : Single ;       // Output image max. value
    Scale : Single ;          // Output image pixel scaling factor
    ActualFrame : Integer ;

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

     // Tag added to end of output file
     if rbDeltaF.Checked then
          FileNameTag := 'df'
     else if ckUseEquation.Checked then
          FileNameTag := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Name
     else FileNameTag := 'ratio' ;

     FileName := ANSIReplaceText( MainFrm.IDRFile.FileName, '.idr',
                                 format('[%s][%d-%d].idr',
                                [FileNameTag,
                                 StartAtFrame,EndAtFrame])) ;

     IDROut.CreateFileFrom( FileName, MainFrm.IDRFile, False ) ;
//     IDROut.FrameInterval := MainFrm.IDRFile.FrameInterval*MainFrm.IDRFile.NumFrameTypes ;
//     IDROut.ADCNumScansPerFrame := IDROut.ADCNumScansPerFrame*MainFrm.IDRFile.NumFrameTypes ;
     IDROut.NumFrameTypes := 1 ;
     IDROut.FrameTypeDivideFactor[0] := 1 ;

     if rbDeltaF.Checked then begin
        IDROut.FrameType[0] := format('dF %d',
                               [ExtractInt(cbFrameType.Text)]) ;
        end
     else if ckUseEquation.Checked then begin
        IDROut.FrameType[0] := IDROut.Equation[cbEquation.ItemIndex].Name ;
        end
     else begin
        IDROut.FrameType[0] := format( '%d/%d',
                                        [ExtractInt(cbNumerator.Text),
                                         ExtractInt(cbDenominator.Text)]) ;
        end ;

     // Ensure start/end frame are at beginning of multiple frame type sequence
     StartAtFrame := ((StartAtFrame-1) div MainFrm.IDRFile.NumFrameTypes)*MainFrm.IDRFile.NumFrameTypes + 1 ;
     EndAtFrame := ((EndAtFrame-1) div MainFrm.IDRFile.NumFrameTypes)*MainFrm.IDRFile.NumFrameTypes + 1 ;

     // Upper and lower limits of computed image
     MinValue := Min( edLowerLimit.Value,edUpperLimit.Value ) ;
     MaxValue := Max( edLowerLimit.Value,edUpperLimit.Value ) ;
     if MaxValue = MinValue then MaxValue := MinValue + 1.0 ;
     Scale := MainFrm.IDRFile.GreyMax/(MaxValue - MinValue) ;
     IDROut.IntensityScale := 1.0 / Scale ;
     IDROut.IntensityOffset := MinValue ;

     // Background subtraction ROI
     iBackg := Integer(cbBackgroundROI.Items.Objects[cbBackgroundROI.ItemIndex]) ;

     // Intensity threshold level for including pixels in image
     // (pixels below this level appear as MinValue in output image)
     IThreshold := Round( edInclusionThreshold.Value ) ;

     if rbDeltaF.Checked then begin

        // Compute dF/F0 image
        // -------------------

        // Compute F0 frame
        // (From average of series of frames or used entered constant)

        if rbF0FromFrames.Checked then begin

           // Clear averaging buffer
           nAvg := 0 ;
           for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do PAvgBuf^[i] := 0 ;
           // Range of frames to be averaged
           StartF0 := Round(edF0Range.LoValue) ;
           EndF0 := Min( Max( Round(edF0Range.HiValue),
                              StartF0 + MainFrm.IDRFile.NumFrameTypes - 1),
                              EndAtFrame );
           // Add frames to average
           for Frame := StartF0 to EndF0 do begin
               ActualFrame := FrameList^[(Frame-1)*MainFrm.IDRFile.NumFrameTypes+cbFrameType.ItemIndex] ;
               OK := MainFrm.IDRFile.LoadFrame32( ActualFrame, PFrameBuf ) ;
               if OK then begin

                  // Background subtraction intensity
                  if iBackg > 0 then begin
                     IBackground := Round( MeanROIIntensity( MainFrm.IDRFile.ROI[iBackg],
                                                             PFrameBuf,
                                                             0 )) ;
                     end
                  else IBackground := 0 ;

                  // Add to average
                  for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do
                      PAvgBuf^[i] := PAvgBuf^[i] + PFrameBuf^[i] - IBackground ;
                  Inc(nAvg) ;
                  end ;
               end ;
                 //
           if nAvg > 0 then begin
              for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do begin
                  PAvgBuf^[i] := PAvgBuf^[i] div nAvg ;
                  end ;
              end
           else begin
              // Constant user-entered F0 value
              for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do begin
                  PAvgBuf^[i] := Round(edF0Constant.Value) ;
                 end ;
              end ;
           end ;

        // Compute dF/F0 frames

        Done := False ;
        OutFrame := 0 ;
        Frame := StartAtFrame ;
        While not Done do begin

             // Get frame from data file
             ActualFrame := FrameList^[(Frame-1)*MainFrm.IDRFile.NumFrameTypes+cbFrameType.ItemIndex] ;
             MainFrm.IDRFile.LoadFrame32( ActualFrame, PFrameBuf ) ;

             // Compute background intensity (if available)
             if iBackg > 0 then begin
                IBackground := Round( MeanROIIntensity( MainFrm.IDRFile.ROI[iBackg],
                                                        PFrameBuf,
                                                        0 )) ;
                end
             else IBackground := 0 ;

             // Compute image
             for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do begin
                 Intensity := PFrameBuf^[i] - IBackground ;
                 if (Intensity > IThreshold) and (PAvgBuf^[i] > IThreshold) then
                    dF := 100.0*((Intensity/PAvgBuf^[i]) - 1.0)
                 else dF := MinValue ;
                 dF :=  Max(Min(dF,MaxValue),MinValue) ;
                 PFrameBuf^[i] :=  Round(Scale*(dF - MinValue))
                 end ;

             // Save frame to data file
             Inc(OutFrame) ;
             IDROut.SaveFrame32( OutFrame, PFrameBuf ) ;

             Inc(Frame) ;
             if (Frame > EndAtFrame) or bOK.Enabled then Done := True ;
             MainFrm.StatusBar.SimpleText :=
             format(' Computing Images : Frame %d/%d',[Frame,EndAtFrame]) ;
             Application.ProcessMessages ;

             end ;
        end
     else begin

        // Compute ratio or ion concentration image
        // ----------------------------------------
        Done := False ;
        NumeratorAvailable := False ;
        DenominatorAvailable := False ;
        OutFrame := 0 ;
        Frame := StartAtFrame ;
        if ckUseEquation.Checked then begin
           RMax := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].RMax ;
           RMin := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].RMin ;
           KEff := MainFrm.IDRFile.Equation[cbEquation.ItemIndex].Keff ;
           end ;
        While not Done do begin

             // Load numerator frame
             ActualFrame := FrameList^[(Frame-1)*MainFrm.IDRFile.NumFrameTypes + cbNumerator.ItemIndex] ;
             NumeratorAvailable := MainFrm.IDRFile.LoadFrame32( ActualFrame, PNumBuf ) ;
             // Compute background intensity (if available)
             if iBackg > 0 then begin
                INumBackground := Round( MeanROIIntensity( MainFrm.IDRFile.ROI[iBackg],
                                                           PNumBuf,
                                                           0 )) ;
                end
             else INumBackground := 0 ;

             // Load Denominator frame
             ActualFrame := FrameList^[(Frame-1)*MainFrm.IDRFile.NumFrameTypes + cbDenominator.ItemIndex] ;
             DenominatorAvailable := MainFrm.IDRFile.LoadFrame32( ActualFrame, PDenomBuf ) ;
             // Compute background intensity (if available)
             if iBackg > 0 then begin
                IDenomBackground := Round( MeanROIIntensity( MainFrm.IDRFile.ROI[iBackg],
                                                             PDenomBuf,
                                                             0 )) ;
                end
             else IDenomBackground := 0 ;

             // Compute ratio image
             if NumeratorAvailable and DenominatorAvailable then begin

                for i := 0 to MainFrm.IDRFile.NumPixelsPerFrame-1 do begin

                    // Subtract background
                    INumerator := PNumBuf^[i] - INumBackground ;
                    IDenominator := PDenomBuf^[i] - IDenomBackground ;

                    if (INumerator  > IThreshold) and
                       (IDenominator  > IThreshold) then R := INumerator / IDenominator
                                                    else R := RMin ;

                    if ckUseEquation.Checked then begin
                       // Compute ion concentration using binding equation
                       if R < (RMax*0.99) then
                          Concentration := ((R - RMin)/(RMax - R))*KEff
                       else Concentration := MaxValue ;
                       PFrameBuf^[i] :=  Round(Scale*(Concentration - MinValue)) ;
                       end
                    else begin
                       // Display ratio
                       PFrameBuf^[i] :=  Round(Scale*(R - MinValue)) ;
                       end

                    end ;

                // Save frame to data file
                Inc(OutFrame) ;
                IDROut.SaveFrame32( OutFrame, PFrameBuf ) ;
                NumeratorAvailable := False ;
                DenominatorAvailable := False ;

                end ;

             Inc(Frame) ;
             if (Frame > EndAtFrame)  or bOK.Enabled then Done := True ;
             MainFrm.StatusBar.SimpleText :=
             format(' Computing Images : Frame %d/%d',[Frame,EndAtFrame]) ;
             Application.ProcessMessages ;

             end ;

        end ;

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
        IDROut.SaveADC( 0, NumScans, ADCBuf^ ) ;
        FreeMem( ADCBuf ) ;
        // Update output file
        IDROut.ADCNumScansInFile :=  NumScans ;
        IDROut.ADCNumScansPerFrame := MainFrm.IDRFile.ADCNumScansPerFrame div IDROut.NumFrames ;
        end ;

     // Close files
     IDROut.CloseFile ;
     MainFrm.IDRFile.CloseFile ;

    for i := 0 to MainFrm.MDIChildCount-1 do
     if MainFrm.MDIChildren[i].Name = 'ViewFrm' then MainFrm.MDIChildren[i].Close ;
     Application.ProcessMessages ;

     // Open and display averaged file
     MainFrm.IDRFile.OpenFile( FileName ) ;
     MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;
     if MainFrm.IDRFile.NumFrames > 0 then begin
        MainFrm.UpdateRecentFilesList ;
        MainFrm.mnViewImages.Click ;
        end ;

     Close ;

     end;


function TRatioFrm.MeanROIIntensity(
         ROI : TROI ;                     // Region of interest (in)
         FrameBuf : PIntArray ;           // Pointer to frame data (in)
         zExclusionThreshold : Integer    // Inclusion threshold (in)
          ) : Single ;
// --------------------------------------------------------
// Calculate mean pixel intensity within region of interest
// and exceeding exclusion limit
// --------------------------------------------------------
var
     xPix,yPix,i,ix,NumPixels : Integer ;
     z : Integer ;
     Sum : Single ;
     r,Asq,BSq : Single ;
     XStart, XEnd : Single ; // X limits of line
     YStart, YEnd : single ; // Y limits of line
     XStep, YStep : Single ;
     X, Y : Single ; // line coordinates
     Slope : Single ; // Slope of line
     D : Single ;    // Distance along line
     DEnd : Single ; // Length of line

     LeftEdge : Integer ;  // Rectangular bounds of current ROI
     RightEdge : Integer ;
     TopEdge : Integer ;
     BottomEdge : Integer ;

begin

     LeftEdge := MinInt([ROI.TopLeft.X,ROI.BottomRight.X]) ;
     RightEdge := MaxInt([ROI.TopLeft.X,ROI.BottomRight.X]) ;
     TopEdge := MinInt([ROI.TopLeft.Y,ROI.BottomRight.y]) ;
     BottomEdge := MaxInt([ROI.TopLeft.Y,ROI.BottomRight.y]) ;

     case ROI.Shape of

         // Cross-hairs region of interest
         PointROI : begin
            i := ROI.Centre.Y*MainFrm.IDRFile.FrameWidth + ROI.Centre.X ;
            Result := FrameBuf[i] ;
            end ;

         // Rectangular region of interest
         RectangleROI : begin
            Sum := 0.0 ;
            NumPixels := 0 ;
            for xPix := LeftEdge to RightEdge do
                for yPix := TopEdge to BottomEdge do begin
                    i := yPix*MainFrm.IDRFile.FrameWidth + xPix ;
                    z := FrameBuf[i] ;
                    if z >= zExclusionThreshold then begin
                       Sum := Sum + z ;
                       Inc(NumPixels) ;
                       end ;
                    end ;
           if NumPixels > 0 then Result := Sum / NumPixels
                            else Result := 0.0 ;
           end ;

         // Rectangular region of interest
         EllipseROI : begin
            Sum := 0.0 ;
            aSq := (LeftEdge - ROI.Centre.x)*(LeftEdge - ROI.Centre.x) ;
            bSq := (TopEdge - ROI.Centre.y)*(TopEdge - ROI.Centre.y) ;
            NumPixels := 0 ;
            for xPix := LeftEdge to RightEdge do
                for yPix := TopEdge to BottomEdge do begin
                    r := ((xPix-ROI.Centre.x)*(xPix-ROI.Centre.x)/aSq) +
                         ((yPix-ROI.Centre.y)*(yPix-ROI.Centre.y)/bSq) ;
                    if  r <= 1.0 then begin
                        i := yPix*MainFrm.IDRFile.FrameWidth + xPix ;
                        z := FrameBuf[i] ;
                       if z >= zExclusionThreshold then begin
                          Sum := Sum + z ;
                          Inc(NumPixels) ;
                          end ;
                        end ;
                    end ;
           if NumPixels > 0 then Result := Sum / NumPixels
                            else Result := 0.0 ;
           end ;

         LineROI : Begin

           XStart := ROI.TopLeft.X ;
           YStart := ROI.TopLeft.Y ;
           XEnd := ROI.BottomRight.X ;
           YEnd := ROI.BottomRight.Y ;

           // Mean intensity of pixels on line
           Slope := (YEnd - YStart)/(XEnd - XStart) ;
           X := XStart ;
           Y := YStart ;
           XStep := Sign(XEnd - XStart) ;
           YStep := Sign(YEnd - YStart) ;
           DEnd :=  Sqrt( (XEnd - XStart)*(XEnd - XStart)
                    + (YEnd - YStart)*(YEnd - YStart)) ;
           Sum := 0.0 ;
           NumPixels := 0 ;
           Repeat
               // Distance along line profile
               D :=  Sqrt((X - XStart)*(X - XStart) + (Y - YStart)*(Y - YStart)) ;
               // Nearest pixel
               i := Round(X) + Round(Y)*MainFrm.IDRFile.FrameWidth ;
               z := FrameBuf[i] ;
               if z >= zExclusionThreshold then begin
                  Sum := Sum + z ;
                  Inc(NumPixels) ;
                  end ;
               // Increment to next pixel on line
               if Abs(Slope) < 1.0 then begin
                  X := X + XStep ;
                  Y := (X - XStart)*Slope + YStart ;
                  end
               else begin
                  Y := Y + YStep ;
                  X := (Y - YStart)/Slope + XStart ;
                  end ;
               Until Round(D) = Round(DEnd) ;

           if NumPixels > 0 then Result := Sum / NumPixels
                            else Result := 0.0 ;

           end ;


         end ;

     end ;


procedure TRatioFrm.FormClose(Sender: TObject; var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
begin

     // Allocate buffers
     FreeMem( PFrameBuf ) ;
     FreeMem( PAvgBuf ) ;
     FreeMem( PNumBuf ) ;
     FreeMem( PDenomBuf ) ;
     FreeMem( FrameList ) ;

     Action := caFree ;

     end;

procedure TRatioFrm.rbDeltaFClick(Sender: TObject);
begin
     DeltaFGrp.Visible := True ;
     RatioGrp.Visible := False ;
     end;

procedure TRatioFrm.rbFrameRatioClick(Sender: TObject);
begin
     DeltaFGrp.Visible := False ;
     RatioGrp.Visible := True ;
     end;


procedure TRatioFrm.bCancelClick(Sender: TObject);
// -------------------------------
// Cancel ratio-ing and close form
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
