unit SaveAsFileUnit;
// ----------------------------------------------------
// Save all or part of WinFluor IDR to another IDR file
// ----------------------------------------------------
// 02.08.05
// 21.02.07 ... Updated to support 9 frame types
//              Error in number of frames exports when more than one frametype fixed
// 09.05.08 ... Dual-rate, multiwavelength support added

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IDRFile, StdCtrls, ValidatedEdit, RangeEdit, ExtCtrls, StrUtils, Math ;

type
  TSaveAsFileFrm = class(TForm)
    GroupBox1: TGroupBox;
    edFileName: TEdit;
    bChangeName: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Shape1: TShape;
    rbAllFrames: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    edSkip: TValidatedEdit;
    GroupBox6: TGroupBox;
    ckFrameType0: TCheckBox;
    ckFrameType1: TCheckBox;
    ckFrameType2: TCheckBox;
    ckFrameType3: TCheckBox;
    SaveDialog: TSaveDialog;
    bCancel: TButton;
    bOK: TButton;
    SaveIDRFile: TIDRFile;
    ChannelsGrp: TGroupBox;
    ckCh0: TCheckBox;
    ckCh1: TCheckBox;
    ckCh2: TCheckBox;
    ckCh3: TCheckBox;
    ckCh4: TCheckBox;
    ckCh5: TCheckBox;
    ckCh6: TCheckBox;
    ckCh7: TCheckBox;
    ckFrameType4: TCheckBox;
    ckFrameType5: TCheckBox;
    ckFrameType6: TCheckBox;
    ckFrameType7: TCheckBox;
    ckFrameType8: TCheckBox;
    procedure bChangeNameClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure rbAllFramesClick(Sender: TObject);
    procedure ckFrameType0Click(Sender: TObject);
    procedure rbRangeClick(Sender: TObject);
    procedure edRangeKeyPress(Sender: TObject; var Key: Char);
    procedure edSkipKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    procedure SetFrameType( CheckBox : TCheckBox ; ch : Integer ) ;
    procedure SetChannel( CheckBox : TCheckBox ; ch : Integer ) ;
    procedure SaveToFile ;
    procedure UpdateFileName ;
  public
    { Public declarations }
  end;

var
  SaveAsFileFrm: TSaveAsFileFrm;

implementation

uses Main, LogUnit;

{$R *.dfm}

const
    ChannelLimit = 7 ;

procedure TSaveAsFileFrm.FormShow(Sender: TObject);
// -------------------------------------
// Initialisation when form is displayed
// -------------------------------------
begin

     // Frame range
     edRange.LoLimit := 1 ;
     edRange.HiLimit := MainFrm.IDRFile.NumFrames ;
     edRange.LoValue := edRange.LoLimit ;
     edRange.HiValue := edRange.HiLimit ;

     // Display available frame types for selection
     SetFrameType( ckFrameType0, 0 ) ;
     SetFrameType( ckFrameType1, 1 ) ;
     SetFrameType( ckFrameType2, 2 ) ;
     SetFrameType( ckFrameType3, 3 ) ;
     SetFrameType( ckFrameType4, 4 ) ;
     SetFrameType( ckFrameType5, 5 ) ;
     SetFrameType( ckFrameType6, 6 ) ;
     SetFrameType( ckFrameType7, 7 ) ;
     SetFrameType( ckFrameType8, 8 ) ;

     // Display available A/D channels for selection
     SetChannel( ckCh0, 0 ) ;
     SetChannel( ckCh1, 1 ) ;
     SetChannel( ckCh2, 2 ) ;
     SetChannel( ckCh3, 3 ) ;
     SetChannel( ckCh4, 4 ) ;
     SetChannel( ckCh5, 5 ) ;
     SetChannel( ckCh6, 6 ) ;
     SetChannel( ckCh7, 7 ) ;

     // Update default save file name
     UpdateFileName ;

     end;


procedure TSaveAsFileFrm.SetChannel(
          CheckBox : TCheckBox ;
          ch : Integer
          ) ;
// ---------------------------
// Set channel selection state
// ---------------------------
begin
     if ch < MainFrm.IDRFile.ADCNumChannels then begin
        CheckBox.Visible := True ;
        CheckBox.Checked := MainFrm.IDRFile.ADCChannel[ch].InUse ;
        CheckBox.Caption := MainFrm.IDRFile.ADCChannel[ch].ADCName ;
        end
     else begin
        CheckBox.Visible := False ;
        CheckBox.Checked := False ;
        end ;

     end ;


procedure TSaveAsFileFrm.SetFrameType(
          CheckBox : TCheckBox ;
          ch : Integer
          ) ;
// ----------------------------
// Set frame type selection box
// ----------------------------
begin

     if MainFrm.IDRFile.NumFrameTypes > ch then begin
        CheckBox.Caption := MainFrm.IDRFile.FrameType[ch] ;
        CheckBox.Visible := True ;
        CheckBox.Checked := True ;
        end
     else begin
        CheckBox.Visible := False ;
        CheckBox.Checked := False ;
        end ;
     end ;


procedure TSaveAsFileFrm.bChangeNameClick(Sender: TObject);
// -----------------------------------
// Change name / folder of export file
// -----------------------------------
begin

     SaveDialog.InitialDir := ExtractFilePath( edFileName.Text ) ;
     SaveDialog.Title := 'Save As Data File ' ;
     SaveDialog.options := [ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.DefaultExt := ExtractFileExt( edFileName.Text ) ;
     SaveDialog.Filter := ' WinFluor (*.idr)|*.idr'  ;

     SaveDialog.FileName := edFileName.Text ;
     if SaveDialog.Execute then edFileName.Text := SaveDialog.FileName ;

     end ;


procedure TSaveAsFileFrm.UpdateFileName ;
// -------------------------------------------
// Update file name with frame range and types
// -------------------------------------------
var
    NumTypes : Integer ;
    s : String ;
begin

     // Add range
     if rbRange.Checked then begin
        edFileName.Text := ANSIReplaceText( MainFrm.IDRFile.FileName,
                                           '.idr',
                                           format('[%.0f-%.0f].idr',
                                           [edRange.LoValue,edRange.HiValue]) ) ;
        end
     else begin
        edFileName.Text := ANSIReplaceText( MainFrm.IDRFile.FileName,
                                           '.idr',
                                           format('[%.0f-%.0f].idr',
                                           [edRange.LoLimit,edRange.HiLimit]) ) ;
        end ;


     // Add frame types (if some have been excluded)
     s := '[' ;
     NumTypes := 0 ;
     if ckFrameType0.Checked then begin
        s := s + MainFrm.IDRFile.FrameType[0] + ' ' ;
        Inc(NumTypes) ;
        end ;
     if ckFrameType1.Checked then begin
        s := s + MainFrm.IDRFile.FrameType[1] + ' ' ;
        Inc(NumTypes) ;
        end ;
     if ckFrameType2.Checked then begin
        s := s + MainFrm.IDRFile.FrameType[2] + ' ' ;
        Inc(NumTypes) ;
        end ;
     if ckFrameType3.Checked then begin
        s := s + MainFrm.IDRFile.FrameType[3] + ' ' ;
        Inc(NumTypes) ;
        end ;
     if ckFrameType4.Checked then begin
        s := s + MainFrm.IDRFile.FrameType[4] + ' ' ;
        Inc(NumTypes) ;
        end ;
     if ckFrameType5.Checked then begin
        s := s + MainFrm.IDRFile.FrameType[5] + ' ' ;
        Inc(NumTypes) ;
        end ;
     if ckFrameType6.Checked then begin
        s := s + MainFrm.IDRFile.FrameType[6] + ' ' ;
        Inc(NumTypes) ;
        end ;
     if ckFrameType7.Checked then begin
        s := s + MainFrm.IDRFile.FrameType[7] + ' ' ;
        Inc(NumTypes) ;
        end ;
     if ckFrameType8.Checked then begin
        s := s + MainFrm.IDRFile.FrameType[8] + ' ' ;
        Inc(NumTypes) ;
        end ;
     s := s + ']' ;

     if NumTypes < MainFrm.IDRFile.NumFrameTypes then begin
        edFileName.Text := ANSIReplaceText( edFileName.Text,
                                           '.idr',
                                           s + '.idr' )
        end ;

    end ;


procedure TSaveAsFileFrm.SaveToFile ;
// --------------------------------------------
// Save images to another WinFluor data file
// --------------------------------------------
const
    NumScansPerBuf = 256 ;
var
    FrameNum : Integer ; // Frame counter
    PFrameBuf : Pointer ; // Image frame buffer pointer
    PixelDepth : Integer ;
    i,j : Integer ;
    Done : Boolean ;
    OK : Boolean ;
    iStartFrame : Integer ;   // First frame to save
    iEndFrame : Integer ;     // Last frame to save
    StartADCScan : Integer ;  // First A/D scan to save
    EndADCScan : Integer ;    // Last A/D scan
    InScan, OutScan, NumScansToCopy : Integer ;
    NumScansToRead, NumScansRead : Integer ;
    UseChannel : Array[0..MaxChannel] of Boolean ;
    UseFrameType : Array[0..MaxFrameType] of Boolean ;
    ADCChannel : TChannel ;
    iStep : Integer ;
    ch,chOut : Integer ;
    NumFramesExported : Integer ;
    FrameType : Integer ;
    MaxFramesOfType : Integer ;
    NumFramesOfType : Array[0..MaxFrameType] of Integer ;
    s : String ;
   InBuf : Array[0..NumScansPerBuf*(ChannelLimit+1)-1] of SmallInt ;
   OutBuf : Array[0..NumScansPerBuf*(ChannelLimit+1)-1] of SmallInt ;
begin

     // Prevent overwrite of current open data file
     if  LowerCase(edFileName.Text) = LowerCase(MainFrm.IDRFile.FileName) then begin
         ShowMessage( format( 'Save Aborted! Overwrite of %s not allowed!',
                              [MainFrm.IDRFile.FileName])) ;
         exit ;
         end ;

     // Let user cancel if file already exists
     if FileExists( edFileName.Text ) then begin
        if MessageDlg( format(
           'File %s already exists! Do you want to overwrite it? ',[edFileName.Text]),
           mtWarning,[mbYes,mbNo], 0 ) = mrNo then Exit
        end ;

     // Range of frames to be exported
     if rbAllFrames.Checked then begin
        iStartFrame := 1 ;
        iEndFrame := MainFrm.IDRFile.NumFrames ;
        end
     else begin
        // Ensure frame range starts on frame type 0
        iStartFrame := (((Round(edRange.LoValue) -1) div MainFrm.IDRFile.FrameTypeCycleLength)
                       *MainFrm.IDRFile.FrameTypeCycleLength) + 1 ;
        edRange.LoValue := iStartFrame ;
        iEndFrame := (((Round(edRange.HiValue) -1) div MainFrm.IDRFile.FrameTypeCycleLength)
                       *MainFrm.IDRFile.FrameTypeCycleLength) + MainFrm.IDRFile.FrameTypeCycleLength ;
        edRange.HiValue := iEndFrame ;
        end ;

     // Frame step interval
     if MainFrm.IDRFile.NumFrameTypes > 1 then edSkip.Value := 0.0 ;
     iStep := (Round(edSkip.Value) + 1) ;

     // Frame types to be exported
     UseFrameType[0] := ckFrameType0.Checked and ckFrameType0.Visible ;
     UseFrameType[1] := ckFrameType1.Checked and ckFrameType1.Visible ;
     UseFrameType[2] := ckFrameType2.Checked and ckFrameType2.Visible ;
     UseFrameType[3] := ckFrameType3.Checked and ckFrameType3.Visible ;
     UseFrameType[4] := ckFrameType4.Checked and ckFrameType4.Visible ;
     UseFrameType[5] := ckFrameType5.Checked and ckFrameType5.Visible ;
     UseFrameType[6] := ckFrameType6.Checked and ckFrameType6.Visible ;
     UseFrameType[7] := ckFrameType7.Checked and ckFrameType7.Visible ;
     UseFrameType[8] := ckFrameType8.Checked and ckFrameType8.Visible ;


     // Create export file (without copying A/D data
     if not SaveIDRFile.CreateFileFrom( edFileName.Text,MainFrm.IDRFile, False ) then begin
        MainFrm.StatusBar.SimpleText := 'Unable to create : ' + edFileName.Text ;
        Exit ;
        end ;

     // Check for disk space
     if {SaveIDRFile.DiskSpaceAvailable( (iEndFrame - iStartFrame + 1) div iStep )} false then begin
         ShowMessage( 'Save Aborted! Insufficient disk space!' ) ;
         SaveIDRFile.CloseFile ;
         Exit ;
         end ;

     // No. of frame types in o/p file
     SaveIDRFile.NumFrameTypes := 0 ;
     for i := 0 to MaxFrameType do if UseFrameType[i] then begin
         SaveIDRFile.NumFrameTypes := SaveIDRFile.NumFrameTypes + 1 ;
         SaveIDRFile.FrameType[SaveIDRFile.NumFrameTypes-1] := MainFrm.IDRFile.FrameType[i] ;
         SaveIDRFile.FrameTypeDivideFactor[SaveIDRFile.NumFrameTypes-1] := MainFrm.IDRFile.FrameTypeDivideFactor[i] ;
         end ;

     // Allocate frame buffer
     GetMem( PFrameBuf, MainFrm.IDRFile.NumBytesPerFrame ) ;

     // Copy image frames to file
     // -------------------------

     // Clear frame type counter
     for i := 0 to High(NumFramesOfType) do NumFramesOfType[i] := 0 ;

     FrameNum := iStartFrame ;
     NumFramesExported := 0 ;
     While FrameNum <= iEndFrame do begin

        // Type of frame
        FrameType := MainFrm.IDRFile.TypeOfFrame(FrameNum)  ;
        if UseFrameType[FrameType] then begin
           // Save frame to export file
           if MainFrm.IDRFile.LoadFrame( FrameNum, PFrameBuf ) then begin
              Inc(NumFramesExported) ;
              SaveIDRFile.SaveFrame( NumFramesExported, PFrameBuf ) ;
              MainFrm.StatusBar.SimpleText := format(
              'Save As: Saving %d/%d frames (%d)',[FrameNum,iEndFrame,NumFramesExported]) ;
             Application.ProcessMessages ;

              Inc(NumFramesOfType[FrameType]) ;

              end ;
           end ;

        FrameNum := FrameNum + iStep ;
        end ;

     MaxFramesOfType := 0 ;
     for i := 0 to MaxFrameType do if UseFrameType[i] then begin
         MaxFramesOfType := Max(NumFramesOfType[i],MaxFramesOfType) ;
         end ;

     // No. of frame types in o/p file
     SaveIDRFile.NumFrameTypes := 0 ;
     for i := 0 to MaxFrameType do if UseFrameType[i] then begin
         SaveIDRFile.NumFrameTypes := SaveIDRFile.NumFrameTypes + 1 ;
         SaveIDRFile.FrameType[SaveIDRFile.NumFrameTypes-1] := MainFrm.IDRFile.FrameType[i] ;
         SaveIDRFile.FrameTypeDivideFactor[SaveIDRFile.NumFrameTypes-1] := MaxFramesOfType div NumFramesOfType[i] ;
         end ;

     // Inter-frame interval
     SaveIDRFile.FrameInterval := (MainFrm.IDRFile.FrameInterval*NumFramesExported)/
                                   (iEndFrame - iStartFrame + 1) ;

     FreeMem( PFrameBuf ) ;

     // Final Report
     MainFrm.StatusBar.SimpleText := format(
     ' SaveAs: Save of frames %d-%d to %s complete.',
     [iEndFrame,iStartFrame,SaveIDRFile.FileName]) ;
     LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;

     if MainFrm.IDRFile.ADCNumScansInFile <= 0 then begin
        SaveIDRFile.CloseFile ;
        Exit ;
        end ;

     // Copy A/D channels (if available)

     StartADCScan := Round( ((iStartFrame-1)*MainFrm.IDRFile.FrameInterval)/
                       MainFrm.IDRFile.ADCScanInterval ) ;
     EndADCScan := Round( ((iEndFrame)*MainFrm.IDRFile.FrameInterval)/
                          MainFrm.IDRFile.ADCScanInterval ) ;

     // Channels to be exported
     UseChannel[0] :=  ckCh0.Checked ;
     UseChannel[1] :=  ckCh1.Checked ;
     UseChannel[2] :=  ckCh2.Checked ;
     UseChannel[3] :=  ckCh3.Checked ;
     UseChannel[4] :=  ckCh4.Checked ;
     UseChannel[5] :=  ckCh5.Checked ;
     UseChannel[6] :=  ckCh6.Checked ;
     UseChannel[7] :=  ckCh7.Checked ;

     chOut := 0 ;
     SaveIDRFile.ADCVoltageRange := MainFrm.IDRFile.ADCVoltageRange ;
     for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do if UseChannel[ch] then begin
         ADCChannel := MainFrm.IDRFile.ADCChannel[ch] ;
         ADCChannel.ChannelOffset := chOut ;
         SaveIDRFile.ADCChannel[chOut] := ADCChannel ;
         Inc(chOut) ;
         end ;
     SaveIDRFile.ADCNumChannels := chOut ;

     { Copy records }
     InScan := StartADCScan ;
     NumScansToCopy := EndADCScan - StartADCScan + 1 ;
     OutScan := 0 ;
     Done := False ;
     While not Done do begin

         // Read from buffer
         NumScansToRead := Min( NumScansToCopy,NumScansPerBuf ) ;
         NumScansRead := MainFrm.IDRFile.LoadADC( InScan, NumScansToRead, InBuf ) ;
         if NumScansRead <= 0 then Done := True ;

         // Copy required channels
         j := 0 ;
         for i := 0 to NumScansRead-1 do begin
             for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do if UseChannel[ch] then begin
                 OutBuf[j] := InBuf[i*MainFrm.IDRFile.ADCNumChannels +
                                    MainFrm.IDRFile.ADCChannel[ch].ChannelOffset] ;
                 Inc(j) ;
                 end ;
             end ;

         // Write to output file
         SaveIDRFile.SaveADC( OutScan, NumScansRead, OutBuf ) ;
         OutScan := OutScan + NumScansRead ;

         // Report progress
         MainFrm.StatusBar.SimpleText := format(
         'Save As: Saving A/D channel scans %d/%d to %s ',
         [StartADCScan,EndADCScan,SaveIDRFile.FileName]) ;

         InScan := InScan + NumScansRead ;
         NumScansToCopy := NumScansToCopy - NumScansRead ;
         if NumScansToCopy <= 0 then Done := True ;

         end ;

     // Close export data file
     SaveIDRFile.ADCNumScansInFile := OutScan ;
     SaveIDRFile.CloseFile ;

     end ;


procedure TSaveAsFileFrm.bOKClick(Sender: TObject);
begin
     // Save frames/signals to file
     SaveToFile ;
     end;

procedure TSaveAsFileFrm.rbAllFramesClick(Sender: TObject);
begin
     UpdateFileName ;
     end;

procedure TSaveAsFileFrm.ckFrameType0Click(Sender: TObject);
begin
     UpdateFileName ;
     end;


procedure TSaveAsFileFrm.rbRangeClick(Sender: TObject);
begin
     UpdateFileName ;
     end;

procedure TSaveAsFileFrm.edRangeKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then begin
        edRange.LoValue := (((Round(edRange.LoValue) -1) div MainFrm.IDRFile.FrameTypeCycleLength)
                            *MainFrm.IDRFile.FrameTypeCycleLength) + 1 ;
        edRange.HiValue := (((Round(edRange.HiValue) -1) div MainFrm.IDRFile.FrameTypeCycleLength)
                            *MainFrm.IDRFile.FrameTypeCycleLength) + MainFrm.IDRFile.FrameTypeCycleLength ;
        end ;
     UpdateFileName ;
     end;

procedure TSaveAsFileFrm.edSkipKeyPress(Sender: TObject; var Key: Char);
begin
    if Key = #13 then begin
        if MainFrm.IDRFile.NumFrameTypes > 1 then edSkip.Value := 1.0 ;
        end ;
    end;

end.
