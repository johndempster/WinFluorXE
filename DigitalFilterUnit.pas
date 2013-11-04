unit DigitalFilterUnit;
// ---------------------------------------------
// Apply digital filter to analog signal channels
// ----------------------------------------------
// 21.10.10
// 27.07.12 Cancel now closes window

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ComCtrls, Main, math, strutils ;

type
  TDigitalFilterFrm = class(TForm)
    FilterGrp: TGroupBox;
    rbLowPass: TRadioButton;
    rbHighPass: TRadioButton;
    Filter: TPageControl;
    LPFilter: TTabSheet;
    Label1: TLabel;
    edLPCutOffFreq: TValidatedEdit;
    HPFilter: TTabSheet;
    Label2: TLabel;
    cbHPFilter: TComboBox;
    NFFilter: TTabSheet;
    Label4: TLabel;
    edNFCutOffFreq: TValidatedEdit;
    rbNotchFilter: TRadioButton;
    bOK: TButton;
    bCancel: TButton;
    ChannelsGrp: TGroupBox;
    ckInUse0: TCheckBox;
    ckInUse1: TCheckBox;
    ckInUse2: TCheckBox;
    ckInUse3: TCheckBox;
    ckInUse4: TCheckBox;
    ckInUse5: TCheckBox;
    ckInUse6: TCheckBox;
    ckInUse7: TCheckBox;
    rbInvert: TRadioButton;
    rbRestoreOriginal: TRadioButton;
    NoneTab: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rbNotchFilterClick(Sender: TObject);
    procedure rbLowPassClick(Sender: TObject);
    procedure rbHighPassClick(Sender: TObject);
    procedure rbInvertClick(Sender: TObject);
    procedure rbRestoreOriginalClick(Sender: TObject);
  private
    { Private declarations }
    UseChannel : Array[0..MaxADCChannels-1] of Boolean ;
    EDRFileName : String ;
    EDRFileNameBAK : String ;

    procedure SetChannelCheckBox(
              ckInUse : TCheckBox ;
              ChanNum : Integer
              ) ;
    procedure LowPassFilter ;
    procedure ButterworthHPFilter ;
    procedure NotchFilter ;
    procedure InvertSignal ;
    procedure RestoreOriginal ;

  public
    { Public declarations }
  end;

var
  DigitalFilterFrm: TDigitalFilterFrm;

implementation

uses ViewPlotUnit, LogUnit;

{$R *.dfm}
const
    MinSingle = 1.5E-45 ;
var
   Abort : Boolean ;


procedure TDigitalFilterFrm.FormShow(Sender: TObject);
{ -----------------------------------
  Initialise control settings on form
  -----------------------------------}
var
    NyquistFreq : Single ;
begin

     ClientWidth := ChannelsGrp.Width + ChannelsGrp.Left + 5 ;
     ClientHeight := bOK.Height + bOK.Top + 5 ;

     // Create EDR and back up file names
     EDRFileName := ChangeFileExt(MainFrm.IDRFile.FileName,'.edr') ;
     EDRFileNameBAK := ANSIReplaceText( EDRFileName,'.edr','[BAK].edr') ;

     { Set channel in use check boxes }
     SetChannelCheckBox( ckInUse0, 0 ) ;
     SetChannelCheckBox( ckInUse1, 1 ) ;
     SetChannelCheckBox( ckInUse2, 2 ) ;
     SetChannelCheckBox( ckInUse3, 3 ) ;
     SetChannelCheckBox( ckInUse4, 4 ) ;
     SetChannelCheckBox( ckInUse5, 5 ) ;
     SetChannelCheckBox( ckInUse6, 6 ) ;
     SetChannelCheckBox( ckInUse7, 7 ) ;

    { Set limits and initial value of LP cut-off frequency }
    edLPCutOffFreq.Scale := 1.0 / MainFrm.IDRFile.ADCSCanInterval ;
    edLPCutOffFreq.LoLimit := 0.132505/200.0 ;
    edLPCutOffFreq.HiLimit := 0.132505/0.5 ;
    edLPCutOffFreq.Value := 0.132505/2.5 ;

    // Set cut-off frequencies for HP filter
    NyquistFreq := 1.0 / (MainFrm.IDRFile.ADCSCanInterval*2.0) ;
    cbHPFilter.Clear ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.001*NyquistFreq]),TObject(1)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.002*NyquistFreq]),TObject(2)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.003*NyquistFreq]),TObject(3)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.004*NyquistFreq]),TObject(4)) ;
    cbHPFilter.Items.AddObject( format(' %.3g Hz',[0.005*NyquistFreq]),TObject(5)) ;
    cbHPFilter.ItemIndex := 0 ;

    { Set limits and initial value of NF cut-off frequency }
    edNFCutOffFreq.Scale := 1.0 / MainFrm.IDRFile.ADCSCanInterval ;
    edNFCutOffFreq.LoLimit := 0.132505/200.0 ;
    edNFCutOffFreq.HiLimit := 0.132505/0.5 ;
    edNFCutOffFreq.Value := 0.132505/2.5 ;

    rbLowPass.Checked := True ;
    Filter.ActivePage := LPFilter ;

    Abort := False ;

    end;


procedure TDigitalFilterFrm.bOKClick(Sender: TObject);
// ------------
// Apply filter
// ------------
var
    OK : Boolean ;
begin

    { Disable button }
    bOK.Enabled := False ;

    // Backup no longer needed since new file created for output of filter
    { Make a back up copy of original data file if one doesn't already exist }
    //Main.StatusBar.SimpleText :=
    // ' Digital Filter : Making back-up of original data file' ;
    //MakeBackupFile ;

    // Make backup copy of EDR file
    if not FileExists(EDRFileNameBAK) then begin
       MainFrm.IDRFile.CloseFile ;
       OK := CopyFile( PChar(EDRFileName), PChar(EDRFileNameBAK), False ) ;
       if not OK then ShowMessage('Unable to create backup') ;
       MainFrm.IDRFile.OpenFile( MainFrm.IDRFile.FileName ) ;
       end ;

    // Apply selected filter

    // Enable updates of files
    MainFrm.IDRFile.WriteEnabled := True ;

    if rbLowPass.Checked then LowPassFilter
    else if rbHighPass.Checked then ButterworthHPFilter
    else if rbInvert.Checked then InvertSignal
    else if rbNotchFilter.Checked then NotchFilter
    else RestoreOriginal ;

    // Disable updates
    MainFrm.IDRFile.WriteEnabled := False ;

    { Re-enable button }
    bOK.Enabled := True ;

    // Update signal time course display
    ViewPlotFrm.DisplayTimeCourse(ViewPlotFrm.CurrentFrame) ;

    Close ;

    end ;


procedure TDigitalFilterFrm.LowPassFilter ;
{ ----------------------------------------------------------------
  Gaussian low/high pass digital filter. (based on Sigworth, 1983)
  ---------------------------------------------------------------- }
const
     MaxCoeff = 128 ;
     MinBlocksPerBuffer = 256 ;
var
   a : Array[-MaxCoeff..MaxCoeff] of single ;
   InBuf : PSmallIntArray ;
   OutBuf : PSmallIntArray ;
   Work :  PSmallIntArray ;
   Temp,sum,sigma,aScale : single ;
   i,j,k,Ch,Coeff,Src,Dest : Integer ;
   BufSize : Integer ;             { No. of samples in file I/O buffer }
   NumBuffersToDo : Integer ; { No. of input buffers to be processed }
   NumBuffersRead : Integer ;  // No. of buffer still to be read
   NumBuffersWritten : Integer ;  // No. of buffer still to be written
   NumSamples : Integer ;          { Maximum sample in filter work buffer }
   NumCoeffs : Integer ;           { Maximum filter coefficient index }
   FirstBuffer : Boolean ;
   InBlockStart,OutBlockStart : Integer ;
   iStart : Integer ;
   FilterOp : String ;
   NumBlocksPerBuffer : Integer ;

begin

    // Type of filtering
    FilterOp := format('Low Pass Filter [%.4gHz]:',
                       [edLPCutOffFreq.Value*edLPCutOffFreq.Scale]) ;

    { Generate filter coefficients }
    sigma := 0.132505/(edLPCutOffFreq.Value) ;
    if sigma >= 0.62  then begin

       aScale := -1./(2.*sigma*sigma) ;
       NumCoeffs := 0 ;
       a[0] := 1.0 ;
       sum := 1.0 ;
       temp := 1.0 ;
       while (temp >= 10.0*MinSingle) and (NumCoeffs < MaxCoeff) do begin
          Inc(NumCoeffs) ;
          temp := exp( NumCoeffs*NumCoeffs*aScale ) ;
          a[NumCoeffs] := temp ;
          a[-NumCoeffs] := Temp ;
          sum := sum + 2.0*temp ;
          end ;

      { Normalise coefficients so that they summate to 1. }
      for i := -NumCoeffs to NumCoeffs do a[i] := a[i]/sum ;
      end
    else begin
      { Special case for very light filtering (See Colquhoun & Sigworth, 1983) }
      a[1] := (sigma*sigma)/2. ;
      a[-1] := a[1] ;
      a[0] := 1.0 - 2.0*a[1] ;
      NumCoeffs := 1 ;
      end ;

    // No. of blocks in I/P buffer (taking into account sample reduction factor
    NumBlocksPerBuffer := MinBlocksPerBuffer ;
    BufSize := NumBlocksPerBuffer*MainFrm.IDRFile.ADCNumChannels ;

    // Allocate buffers
    GetMem( InBuf, BufSize*2 ) ;
    GetMem( OutBuf, BufSize*2 ) ;
    GetMem( Work, BufSize*4 ) ;

    NumBuffersToDo := MainFrm.IDRFile.ADCNumScansInFile div NumBlocksPerBuffer ;
    NumBuffersRead := 0 ;
    NumBuffersWritten := 0 ;
    NumSamples := NumCoeffs*MainFrm.IDRFile.ADCNumChannels ;
    Src := BufSize ;
    Dest := 0 ;

    { Point to start of A/D data }
    InBlockStart := 0 ;
    // Fill output buffer with original unfiltered data
    // (in case some channels are not selected for filtering)
    OutBlockStart := 0 ;
    MainFrm.IDRFile.LoadADC( OutBlockStart,NumBlocksPerBuffer,OutBuf^ ) ;

    FirstBuffer := True ;
    while (NumBuffersWritten <= NumBuffersToDo) and (not Abort) do begin

       { Get data from input data file }
       if Src >= BufSize then begin
          if NumBuffersRead < NumBuffersToDo then begin
             MainFrm.IDRFile.LoadADC( InBlockStart,NumBlocksPerBuffer,InBuf^ ) ;
             InBlockStart := InBlockStart + NumBlocksPerBuffer ;
             end
          else begin
             { No more buffers, fill buffer with last sample block }
             ch := BufSize - MainFrm.IDRFile.ADCNumChannels ;
             for i := 0 to BufSize-1 do begin
                 InBuf[i] := InBuf[ch] ;
                 Inc(ch) ;
                 if ch >= BufSize then ch := BufSize - MainFrm.IDRFile.ADCNumChannels ;
                 end ;
             end ;
          Inc(NumBuffersRead) ;
          Src := 0 ;
          // Report progress
          MainFrm.StatusBar.SimpleText := format(
          '%s %.0f%% done.',
          [FilterOp,(NumBuffersWritten*100.0)/NumBuffersToDo]) ;
          end ;

       { If first buffer, fill working array }
       if FirstBuffer then begin
          for Coeff := -NumCoeffs to NumCoeffs do begin
              for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
                  i := Coeff*MainFrm.IDRFile.ADCNumChannels + ch ;
                  if i >= 0 then Work[i+BufSize] := InBuf[i]
                            else Work[i+BufSize] := InBuf[ch] ;
                  end ;
              end ;
          iStart := -NumSamples ;
          FirstBuffer := False ;
          Src := NumCoeffs*MainFrm.IDRFile.ADCNumChannels ;
          end ;

       { Apply gaussian filter to each channel }
       for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
           if UseChannel[ch] then begin
              Sum := 0.0 ;
              j := iStart + ch ;
              for Coeff := -NumCoeffs to NumCoeffs do begin
                  Sum := Sum + Work[j+BufSize]*a[Coeff] ;
                  j := j + MainFrm.IDRFile.ADCNumChannels ;
                  if j > NumSamples then j := -NumSamples + ch ;
                  end ;
              OutBuf[Dest] := Round(Sum) ;
              end ;
           Inc(Dest) ;
           end ;

       { Get next block of samples from input buffer }
       for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
           Work[iStart+ch+BufSize] := InBuf[Src] ;
           Inc(Src) ;
           end ;

       { Increment start pointer }
       iStart := iStart + MainFrm.IDRFile.ADCNumChannels ;
       if iStart > NumSamples then iStart := -NumSamples ;

       if Dest >= BufSize then begin

          // Write to file
          MainFrm.IDRFile.SaveADC( OutBlockStart,MinBlocksPerBuffer,OutBuf^ ) ;
          OutBlockStart := OutBlockStart + MinBlocksPerBuffer ;

          // Fill O/P buffer with old values
          MainFrm.IDRFile.LoadADC( OutBlockStart,NumBlocksPerBuffer,OutBuf^ ) ;
          Dest := 0 ;
          Inc(NumBuffersWritten) ;
          end ;

       end ;

    FreeMem(InBuf) ;
    FreeMem(OutBuf) ;
    FreeMem(Work) ;

    // Report progress
    MainFrm.StatusBar.SimpleText := format(' %s Completed.',[FilterOp]) ;
    LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;

    end ;


procedure TDigitalFilterFrm.ButterworthHPFilter ;
// ----------------------------------------------
// Forward-backward butterworth high pass filter
// ----------------------------------------------
const
    NumScansPerBuf = 256 ;
    NumCoeffs = 5 ;

   b001 : Array[0..4] of extended
   = ( 0.99590372213842,  -3.98361488855370,   5.97542233283055,
       -3.98361488855370, 0.99590372213842 ) ;
   a001 : Array[0..4] of extended
   = (1.00000000000000,  -3.99179062441447,   5.97540555338674,
      -3.97543915264442, 0.99182422376917 ) ;

   b002 : Array[0..4] of extended
   = ( 0.99182421200053,  -3.96729684800213,   5.95094527200320,
       -3.96729684800213, 0.99182421200053 ) ;
   a002 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.98358125865852,   5.95087842926670,
       -3.95101243657283, 0.98371526751048 ) ;

   b003 : Array[0..4] of extended
   = ( 0.98776138927683,  -3.95104555710730,   5.92656833566096,
       -3.95104555710730, 0.98776138927683 ) ;
   a003 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.97537191256092,   5.92641855596542,
       -3.92671919775679, 0.97567256214609 ) ;

   b004 : Array[0..4] of extended
   = ( 0.98371517412976,  -3.93486069651902,   5.90229104477854,
       -3.93486069651902, 0.98371517412976 ) ;
   a004 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.96716259594885,   5.90202586149088,
       -3.90255878482324, 0.96769554381314 ) ;

   b005 : Array[0..4] of extended
   = ( 0.97968548719040,  -3.91874194876162,   5.87811292314243,
       -3.91874194876162, 0.97968548719040 ) ;
   a005 : Array[0..4] of extended
   = ( 1.00000000000000,  -3.95895331864708,   5.87770027353615,
       -3.87853054905174, 0.95978365381150 ) ;

var
    i,j,ch : Integer ;
    iScan : Integer ;
    NumScansPerBlock : Integer ;
    NumScansRead : Integer ;
    Buf : Array[0..NumScansPerBuf*(MaxADCChannels)-1] of SmallInt ;
    FPSize : Integer ;
    x : extended ;
    y : Array[0..MaxADCChannels-1] of extended ;
    z : Array[0..MaxADCChannels-1,0..NumCoeffs-1] of extended ;
    a : Array[0..NumCoeffs-1] of extended ;
    b : Array[0..NumCoeffs-1] of extended ;
     TempPath : Array[0..100] of Char ;
     TempName : Array[0..100] of Char ;
     TempFileName1 : String ;
     TempHandle1 : Integer ;
     TempFileName2 : String ;
     TempHandle2 : Integer ;
     FilePointer : Integer ;
     iCutOff : Integer ;
     iBlock  : Integer ;
     FilterOp : String ;

begin

    // Get selected cut off frequency
    iCutOff := Integer(cbHPFilter.Items.Objects[cbHPFilter.ItemIndex]) ;

    // Type of filtering
    FilterOp := format('High Pass [%s]:',[cbHPFilter.Text]) ;

    // Select coefficients for selected cut-off frequency
    Case iCutOff of
       1 : Begin
          // 0.001
          for i := 0 to High(a) do a[i] := a001[i] ;
          for i := 0 to High(b) do b[i] := b001[i] ;
          end ;
       2 : Begin
          // 0.002
          for i := 0 to High(a) do a[i] := a002[i] ;
          for i := 0 to High(b) do b[i] := b002[i] ;
          end ;
       3 : Begin
          // 0.003
          for i := 0 to High(a) do a[i] := a003[i] ;
          for i := 0 to High(b) do b[i] := b003[i] ;
          end ;
       4 : Begin
          // 0.004
          for i := 0 to High(a) do a[i] := a004[i] ;
          for i := 0 to High(b) do b[i] := b004[i] ;
          end ;
       5 : Begin
          // 0.005
          for i := 0 to High(a) do a[i] := a005[i] ;
          for i := 0 to High(b) do b[i] := b005[i] ;
          end ;
       end ;

     FPSize := SizeOf(x) ;

     // Create temporary files
     GetTempPath( High(TempPath), TempPath )  ;
     GetTempFilename( TempPath, PChar('WinEDR'), 0, TempName ) ;
     TempFileName1 := String(TempName) ;
     TempHandle1 := FileCreate( TempFileName1 ) ;
     GetTempFilename( TempPath, PChar('WinEDR'), 0, TempName ) ;
     TempFileName2 := String(TempName) ;
     TempHandle2 := FileCreate( TempFileName2 ) ;

     // Copy samples to floating point temp file #1
     // -------------------------------------------

     FileSeek( TempHandle1, 0,0) ;
     iScan := 0 ;
     While iScan < (MainFrm.IDRFile.ADCNumScansInFile-1) do begin

         // Read A/D data from source file
         NumScansRead := MainFrm.IDRFile.LoadADC( iScan,NumScansPerBuf,Buf ) ;

         // Copy to temp file #1
         for j := 0 to NumScansRead*MainFrm.IDRFile.ADCNumChannels-1 do begin
             x := Buf[j] ;
             FileWrite( TempHandle1, x, FPSize ) ;
             end ;

         // Report progress
         MainFrm.StatusBar.SimpleText := format(
         '%s Reading source signal %.3g%%',
         [FilterOp,(100.0*iScan)/MainFrm.IDRFile.ADCNumScansInFile]) ;

         iScan := iScan + NumScansPerBuf ;

         end ;

     // Forward filter pass from to temp file #1 to #2
     // ----------------------------------------------

     // Initialise filter
     FileSeek( TempHandle1, 0, 0 ) ;
     for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
        FileRead( TempHandle1, x, FPSize ) ;
        y[ch] := 0.0 ;
        z[ch,NumCoeffs-1] := 0.0 ;
        for j := NumCoeffs-1 downto 1 do
            z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
        end ;

     FileSeek( TempHandle1, 0, 0 ) ;
     FileSeek( TempHandle2, 0, 0 ) ;
     for iScan := 0 to MainFrm.IDRFile.ADCNumScansInFile-1 do begin

          for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin

              // Read value
              FileRead( TempHandle1, x, FPSize ) ;

              y[ch] := b[0]*x + z[ch,0] ;
              z[ch,NumCoeffs-1] := 0.0 ;
              for j := 1 to NumCoeffs-1 do
                  z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;

              FileWrite( TempHandle2, y[ch], FPSize )

              end ;

          // Report progress
          if (iScan mod NumScansPerBuf) = 0 then
             MainFrm.StatusBar.SimpleText := format(
             '%s Applying forward filter %.3g%%',
             [FilterOp,(100.0*iScan)/MainFrm.IDRFile.ADCNumScansInFile]) ;

          end ;

     // Reverse filter pass from to temp file #2 to #1
     // ----------------------------------------------

     // Initialise filter
     iScan := MainFrm.IDRFile.ADCNumScansInFile - MainFrm.IDRFile.ADCNumChannels ;
     for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
         FilePointer := ((iScan*MainFrm.IDRFile.ADCNumChannels) + ch)*FPSize ;
         FileSeek( TempHandle2, FilePointer, 0 ) ;
         FileRead( TempHandle2, x, FPSize ) ;
         y[ch] := 0.0 ;
         z[ch,NumCoeffs-1] := 0.0 ;
         for j := NumCoeffs-1 downto 1 do
             z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
         end ;

     // Apply filter
     for iScan := MainFrm.IDRFile.ADCNumScansInFile - MainFrm.IDRFile.ADCNumChannels downto 0 do begin
         for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1do begin
             // Read value
             FilePointer := ((iScan*MainFrm.IDRFile.ADCNumChannels) + ch)*FPSize ;
             FileSeek( TempHandle2, FilePointer, 0 ) ;
             FileRead( TempHandle2, x, FPSize ) ;
             // Apply filter
             y[ch] := b[0]*x + z[ch,0] ;
             z[ch,NumCoeffs-1] := 0.0 ;
             for j := 1 to NumCoeffs-1 do
                 z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
             // Write value
             FileSeek( TempHandle1, FilePointer, 0 ) ;
             FileWrite( TempHandle1, y[ch], FPSize ) ;
             end ;

         // Report progress
         if (iScan mod NumScansPerBuf) = 0 then
            MainFrm.StatusBar.SimpleText := format(
            '%s Applying reverse filter %.3g%%',
            [FilterOp,(100.0*iScan)/MainFrm.IDRFile.ADCNumScansInFile]) ;

         end ;

     // Copy results to output EDR data file
     // ----------------------------------

     FileSeek( TempHandle1, 0,0) ;
     iScan := 0 ;
     While iScan < (MainFrm.IDRFile.ADCNumScansInFile-1) do begin

         // Read A/D data from source file
         NumScansRead := MainFrm.IDRFile.LoadADC( iScan,NumScansPerBuf,Buf ) ;
         if NumScansRead <= 0 then Break ;

         j := 0 ;
         for i := 0 to NumScansRead-1 do begin
             for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
                 FileRead( TempHandle1, x, FPSize ) ;
                 if UseChannel[ch] then Buf[j] := Round(x) ;
                 Inc(j) ;
                 end ;
             end ;

         // Write updated buffer back to file
         MainFrm.IDRFile.SaveADC( iScan,NumScansRead,Buf ) ;

         // Report progress
         MainFrm.StatusBar.SimpleText := format(
         '%s Writing to file %.3g%%',
         [FilterOp,(100.0*iScan)/MainFrm.IDRFile.ADCNumScansInFile]) ;

         iScan := iScan + NumScansPerBuf ;

         end ;

     // Close and delete temporary files
     FileClose( TempHandle1 ) ;
     DeleteFile( PChar(TempFileName1)) ;
     FileClose( TempHandle2 ) ;
     DeleteFile( PChar(TempFileName2)) ;

     // Report progress
     MainFrm.StatusBar.SimpleText := format('%s Complete.',[FilterOp]) ;
     LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;

     end ;


procedure TDigitalFilterFrm.NotchFilter ;
// ----------------------------------------------
// Forward-backward butterworth notch filter
// ----------------------------------------------
const
    NumScansPerBuf = 256 ;
    NumCoeffs = 3 ;

var
    i,j,ch,k : Integer ;
    iScan : Integer ;
    NumScansRead : Integer ;
    Buf : Array[0..NumScansPerBuf*MaxADCChannels-1] of SmallInt ;
    FBuf : Array[0..NumScansPerBuf*MaxADCChannels-1] of Extended ;
    FPSize : Integer ;
    x : extended ;
    y : Array[0..MaxADCChannels-1] of extended ;
    z : Array[0..MaxADCChannels-1,0..NumCoeffs-1] of extended ;
    a : Array[0..NumCoeffs-1] of extended ;
    b : Array[0..NumCoeffs-1] of extended ;
     TempPath : Array[0..100] of Char ;
     TempName : Array[0..100] of Char ;
     TempFileName1 : String ;
     TempHandle1 : Integer ;
     TempFileName2 : String ;
     TempHandle2 : Integer ;
     FilePointer : Integer ;
     iCutOff : Integer ;
     iBlock  : Integer ;
         NyquistFreq : Extended ;
    CentreFreq : Extended ;
    Bandwidth : Extended ;
    HalfPi : Extended ;
    D1,E1 : Extended ;
    FilterOp : String ;

begin

    // Type of filtering
    FilterOp := format('Notch Filter [%.4gHz]: ',[edNFCutOffFreq.Value*edNFCutOffFreq.Scale]) ;

    // Get selected cut off frequency
    NyquistFreq := 1.0 / (MainFrm.IDRFile.ADCSCanInterval*2.0) ;
    CentreFreq := edNFCutOffFreq.Value*2.0 ;
    Bandwidth := CentreFreq / 100.0 ;
    HalfPi := Pi/2.0 ;
    D1 := tan( HalfPi*BandWidth );
    E1 := (2.0*cos(HalfPi*CentreFreq*2.0)) / cos(HalfPi*BandWidth) ;

    b[0] := 1.0 / ( 1 + D1 ) ;
    b[1] := -E1 / ( 1 + D1 ) ;
    b[2] := b[0] ;

    a[0] := 1.0 ;
    a[1] := -E1 / ( 1 + D1 ) ;
    a[2] := (1 - D1) / ( 1 + D1 ) ;

     FPSize := SizeOf(x) ;

     // Create temporary files
     GetTempPath( High(TempPath), TempPath )  ;
     GetTempFilename( TempPath, PChar('WinEDR'), 0, TempName ) ;
     TempFileName1 := String(TempName) ;
     TempHandle1 := FileCreate( TempFileName1 ) ;
     GetTempFilename( TempPath, PChar('WinEDR'), 0, TempName ) ;
     TempFileName2 := String(TempName) ;
     TempHandle2 := FileCreate( TempFileName2 ) ;

     // Copy samples to floating point temp file #1
     // -------------------------------------------
     FileSeek( TempHandle1, 0,0) ;
     iScan := 0 ;
     While iScan < (MainFrm.IDRFile.ADCNumScansInFile-1) do begin

         // Read A/D data from source file
         NumScansRead := MainFrm.IDRFile.LoadADC( iScan,NumScansPerBuf,Buf ) ;
         if NumScansRead <= 0 then Break ;

         // Copy to floating point buffer
         for j := 0 to NumScansRead*MainFrm.IDRFile.ADCNumChannels-1 do begin
             FBuf[j] := Buf[j] ;
             end ;

         // Write to temp file #1
         FileWrite( TempHandle1, FBuf, NumScansRead*MainFrm.IDRFile.ADCNumChannels*FPSize ) ;

         // Report progress
         MainFrm.StatusBar.SimpleText := format(
         ' Notch Filter: Reading source signal %.3g%%',
         [(100.0*iScan)/MainFrm.IDRFile.ADCNumScansInFile]) ;

         Application.ProcessMessages ;
         if bOK.Enabled then begin
            FileClose( TempHandle1 ) ;
            FileClose( TempHandle2 ) ;
            Exit ;
            end ;

         iScan := iScan + NumScansPerBuf ;

         end ;

     // Forward filter pass from to temp file #1 to #2
     // ----------------------------------------------

     // Initialise filter
     FileSeek( TempHandle1, 0, 0 ) ;
     for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
        FileRead( TempHandle1, x, FPSize ) ;
        y[ch] := x ;
        //z[ch,NumCoeffs-1] := 0.0 ;
        z[ch,NumCoeffs-1] := y[ch] - b[NumCoeffs-1]*x ;
        for j := NumCoeffs-1 downto 1 do
            z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
        end ;


     FileSeek( TempHandle1, 0, 0 ) ;
     FileSeek( TempHandle2, 0, 0 ) ;
     for iScan := 0 to MainFrm.IDRFile.ADCNumScansInFile-1 do begin

          for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin

              // Read value
              FileRead( TempHandle1, x, FPSize ) ;

              y[ch] := b[0]*x + z[ch,0] ;
              z[ch,NumCoeffs-1] := 0.0 ;
              for j := 1 to NumCoeffs-1 do
                  z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;

              FileWrite( TempHandle2, y[ch], FPSize )

              end ;

          // Report progress
          if (iScan mod NumScansPerBuf) = 0 then begin
             MainFrm.StatusBar.SimpleText := format(
             '%s Applying forward filter %.3g%%',
             [FilterOp,(100.0*iScan)/MainFrm.IDRFile.ADCNumScansInFile]) ;
             Application.ProcessMessages ;
            if bOK.Enabled then begin
               FileClose( TempHandle1 ) ;
               FileClose( TempHandle2 ) ;
               Exit ;
               end ;

             end ;

          end ;

     // Reverse filter pass from to temp file #2 to #1
     // ----------------------------------------------

     // Initialise filter
     iScan := MainFrm.IDRFile.ADCNumScansInFile - MainFrm.IDRFile.ADCNumChannels ;
     for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
         FilePointer := ((iScan*MainFrm.IDRFile.ADCNumChannels) + ch)*FPSize ;
         FileSeek( TempHandle2, FilePointer, 0 ) ;
         FileRead( TempHandle2, x, FPSize ) ;
         y[ch] := x ;
         z[ch,NumCoeffs-1] := 0.0 ;
         for j := NumCoeffs-1 downto 1 do
             z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
         end ;

     // Apply filter
     for iScan := MainFrm.IDRFile.ADCNumScansInFile - MainFrm.IDRFile.ADCNumChannels downto 0 do begin
         for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1do begin
             // Read value
             FilePointer := ((iScan*MainFrm.IDRFile.ADCNumChannels) + ch)*FPSize ;
             FileSeek( TempHandle2, FilePointer, 0 ) ;
             FileRead( TempHandle2, x, FPSize ) ;
             // Apply filter
             y[ch] := b[0]*x + z[ch,0] ;
             z[ch,NumCoeffs-1] := 0.0 ;
             for j := 1 to NumCoeffs-1 do
                 z[ch,j-1] := b[j]*x + z[ch,j] - a[j]*y[ch] ;
             // Write value
             FileSeek( TempHandle1, FilePointer, 0 ) ;
             FileWrite( TempHandle1, y[ch], FPSize ) ;
             end ;

         // Report progress
         if (iScan mod NumScansPerBuf) = 0 then begin
            MainFrm.StatusBar.SimpleText := format(
            '%s Applying reverse filter %.3g%%',
            [FilterOp,(100.0*iScan)/MainFrm.IDRFile.ADCNumScansInFile]) ;
            Application.ProcessMessages ;
            if bOK.Enabled then begin
               FileClose( TempHandle1 ) ;
               FileClose( TempHandle2 ) ;
               Exit ;
            end ;

            end ;

          end ;

     // Copy results to back to source file
     // -----------------------------------

     FileSeek( TempHandle1, 0,0) ;
     iScan := 0 ;
     While iScan < (MainFrm.IDRFile.ADCNumScansInFile-1) do begin

         // Read A/D data from source file
         NumScansRead := MainFrm.IDRFile.LoadADC( iScan,NumScansPerBuf,Buf ) ;
         if NumScansRead <= 0 then Break ;

         j := 0 ;
         for i := 0 to NumScansRead-1 do begin
             for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
                 FileRead( TempHandle1, x, FPSize ) ;
                 if UseChannel[ch] then Buf[j] := Round(x) ;
                 Inc(j) ;
                 end ;
             end ;

         // Write updated buffer to O/P file
         MainFrm.IDRFile.SaveADC( iScan,NumScansRead,Buf ) ;

         // Report progress
         Mainfrm.StatusBar.SimpleText := format(
         '%s Writing to file %.3g%%',
         [FilterOp,(100.0*iScan)/MainFrm.IDRFile.ADCNumScansInFile]) ;

         iScan := iScan + NumScansPerBuf ;

         Application.ProcessMessages ;
         if bOK.Enabled then begin
            FileClose( TempHandle1 ) ;
            FileClose( TempHandle2 ) ;
            Exit ;
            end ;

         end ;

     // Close and delete temporary files
     FileClose( TempHandle1 ) ;
     DeleteFile( PChar(TempFileName1)) ;
     FileClose( TempHandle2 ) ;
     DeleteFile( PChar(TempFileName2)) ;

     // Report progress
     MainFrm.StatusBar.SimpleText := format('%s Complete.',[FilterOp]) ;
     LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;

     end ;


procedure TDigitalFilterFrm.InvertSignal ;
// ---------------------------------------
// Invert sign of selected signal channels
// ---------------------------------------
const
    NumScansPerBuf = 256 ;
var
    iScan,ch,j,i,NumScansRead : Integer ;
    Buf : Array[0..NumScansPerBuf*MaxADCChannels-1] of SmallInt ;
begin

     MainFrm.IDRFile.WriteEnabled := True ;

     iScan := 0 ;
     While iScan < (MainFrm.IDRFile.ADCNumScansInFile-1) do begin

         // Read A/D data from source file
         NumScansRead := MainFrm.IDRFile.LoadADC( iScan,NumScansPerBuf,Buf ) ;
         if NumScansRead <= 0 then Break ;

         // Invert selected channels

         for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do if UseChannel[ch] then begin
             j := MainFrm.IDRFile.ADCChannel[ch].ChannelOffset ;
             for i := 0 to NumScansRead-1 do begin
                 Buf[j] := -Buf[j] ;
                 j := j + MainFrm.IDRFile.ADCNumChannels ;
                 end ;
             end ;

         // Save to file
         MainFrm.IDRFile.SaveADC( iScan,NumScansRead,Buf ) ;

         iScan := iScan + NumScansPerBuf ;

         // Report progress
         Mainfrm.StatusBar.SimpleText := format(
         ' Invert: Inverting Signal %.3g%%',
         [(100.0*iScan)/MainFrm.IDRFile.ADCNumScansInFile]) ;

         end ;

    MainFrm.IDRFile.WriteEnabled := False ;

     // Report progress
     MainFrm.StatusBar.SimpleText := ' Invert: Complete' ;
     LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;


    end ;


procedure TDigitalFilterFrm.RestoreOriginal ;
// ---------------------
// Restore original data
// ----------------------
var
    OK : Boolean ;
begin

    // Make backup copy of EDR file
    if FileExists(EDRFileNameBAK) then begin
       MainFrm.IDRFile.CloseFile ;
       OK := CopyFile( PChar(EDRFileNameBAK), PChar(EDRFileName), False ) ;
       if not OK then ShowMessage('Unable to restore backup') ;
       MainFrm.IDRFile.OpenFile( MainFrm.IDRFile.FileName ) ;
       MainFrm.StatusBar.SimpleText := 'Restore: Original analogue signals recording restored.';
       LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;
       end ;

    end ;

procedure TDigitalFilterFrm.SetChannelCheckBox(
          ckInUse : TCheckBox ;          { Channel in use check box }
          ChanNum : Integer              { Channel Number }
          ) ;
{ ----------------------------
  Set channel in use check box
  ----------------------------}
begin
     if MainFrm.IDRFile.ADCNumChannels > ChanNum then begin
          ckInUse.caption := format('Ch.%d %s',
                             [ChanNum,MainFrm.IDRFile.ADCChannel[ChanNum].ADCName])  ;
          ckInUse.enabled := True ;
          ckInUse.visible := True ;
          ckInUse.checked := True ;

          end
     else begin
          ckInUse.enabled := False ;
          ckInUse.visible := False ;
          ckInUse.checked := False ;
          end ;

     // UseChannel is in channel scan sequence order
     UseChannel[MainFrm.IDRFile.ADCChannel[ChanNum].ChannelOffset] := ckInUse.checked ;

     end ;




procedure TDigitalFilterFrm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
     if not bOK.Enabled then CanClose := False ;
     end;

procedure TDigitalFilterFrm.bCancelClick(Sender: TObject);
begin
     Abort := True ;
     Close ;
     end;

procedure TDigitalFilterFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     Action := caFree ;
     end;

procedure TDigitalFilterFrm.rbNotchFilterClick(Sender: TObject);
// ----------------------
// Select notch filter
// ----------------------
begin
     Filter.ActivePage := NFFilter ;
     end;

     
procedure TDigitalFilterFrm.rbLowPassClick(Sender: TObject);
// ----------------------
// Select low pass filter
// ----------------------
begin
     Filter.ActivePage := LPFilter ;
     end;

procedure TDigitalFilterFrm.rbHighPassClick(Sender: TObject);
// ----------------------
// Select high pass filter
// ----------------------
begin
     Filter.ActivePage := HPFilter ;
     end;


procedure TDigitalFilterFrm.rbInvertClick(Sender: TObject);
// ----------------------
// Select invert option
// ----------------------
begin
     Filter.ActivePage := NoneTab ;
     end;

procedure TDigitalFilterFrm.rbRestoreOriginalClick(Sender: TObject);
// ----------------------
// Select restore option
// ----------------------
begin
     Filter.ActivePage := NoneTab ;
     end;

end.
