unit ExportADCEventsUnit;
// ================================================================
//  WinFluor (c) J. Dempster, University of Strathclyde, 1998-99
//  Event analogue signal channel export module
//  ================================================================
//  08.06.05 Started
// 21.08.10 ... CFS file output added
// 20.10.10 JD ... Bug in export to WCP files fixed
// 29.11.12 JD ... WCPNumZeroAvg now set for WCP exports (avoids FP error in WinWCP)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ADCDataFile, StdCtrls, RangeEdit, Math, ScopeDisplay, strutils  ;

type
  TExportEventsFrm = class(TForm)
    GroupBox3: TGroupBox;
    bChangeName: TButton;
    GroupBox8: TGroupBox;
    rbAllRecords: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    GroupBox2: TGroupBox;
    rbABF: TRadioButton;
    rbASCII: TRadioButton;
    rbWCP: TRadioButton;
    ChannelsGrp: TGroupBox;
    ckCh0: TCheckBox;
    ckCh1: TCheckBox;
    ckCh2: TCheckBox;
    ckCh3: TCheckBox;
    ckCh4: TCheckBox;
    ckCh5: TCheckBox;
    ckCh6: TCheckBox;
    ckCh7: TCheckBox;
    bOK: TButton;
    bCancel: TButton;
    ExportFile: TADCDataFile;
    SaveDialog: TSaveDialog;
    edFileName: TEdit;
    rbIBW: TRadioButton;
    rbCFS: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bChangeNameClick(Sender: TObject);
    procedure rbABFClick(Sender: TObject);
    procedure rbCFSClick(Sender: TObject);
    procedure rbASCIIClick(Sender: TObject);
    procedure rbWCPClick(Sender: TObject);
    procedure ckCh0Click(Sender: TObject);
    procedure rbIBWClick(Sender: TObject);
  private
    { Private declarations }

    procedure SetChannel( CheckBox : TCheckBox ; ch : Integer ) ;
    procedure UpdateSettings ;
    procedure ExportAnalogueChannels ;
    procedure ExportFluorescenceChannel ;
    function RemoveInvalidFileNameCharacters(
             FileName : string ) : string ;



  public
    { Public declarations }
    ExportADC : Boolean ;
//    MaxADCValue : Integer ;
//    ScanInterval : Single ;
//    NumScansPerRecord : Integer ;
//    NumChannels : Integer ;
    Display : TScopeDisplay ;
  end;

var
  ExportEventsFrm: TExportEventsFrm;

implementation

uses Main, LogUnit, EventAnalysisUnit;

{$R *.dfm}

procedure TExportEventsFrm.FormShow(Sender: TObject);
// ------------------------------
// Initialise form when displayed
// ------------------------------
begin

     { Set block of EDR file to be exported }
     edRange.LoLimit := 1.0 ;
     edRange.LoValue := 1.0 ;
     edRange.HiLimit := EventAnalysisFrm.NumEvents ;
     edRange.HiValue := EventAnalysisFrm.NumEvents ;

     SetChannel( ckCh0, 0 ) ;
     SetChannel( ckCh1, 1 ) ;
     SetChannel( ckCh2, 2 ) ;
     SetChannel( ckCh3, 3 ) ;
     SetChannel( ckCh4, 4 ) ;
     SetChannel( ckCh5, 5 ) ;
     SetChannel( ckCh6, 6 ) ;
     SetChannel( ckCh7, 7 ) ;

     UpdateSettings ;
     ChannelsGrp.Enabled := True ;

     end;


procedure TExportEventsFrm.SetChannel(
          CheckBox : TCheckBox ;
          ch : Integer
          ) ;
// ---------------------------
// Set channel selection state
// ---------------------------
begin
     if ch < Display.NumChannels then begin
        CheckBox.Visible := True ;
        CheckBox.Checked := Display.ChanVisible[ch] ;
        CheckBox.Caption := Display.ChanName[ch] ;
        end
     else CheckBox.Visible := False ;
     end ;


procedure TExportEventsFrm.bOKClick(Sender: TObject);
// -------------
// Export Events
// -------------
begin
    if ExportADC then ExportAnalogueChannels
                 else ExportFluorescenceChannel ;

    end ;


procedure TExportEventsFrm.ExportAnalogueChannels ;
// -------------------------------------------------
// Export analogue channels of detected events
// -------------------------------------------------
var
   StartAt,EndAt,ch,i,j : Integer ;
   EventNum : Integer ;
   UseChannel : Array[0..ChannelLimit] of Boolean ;
   OutBuf : PSmallIntArray ;
   chOut : Integer ;
   NumRecordsExported : Integer ;
   ExportType : TADCDataFileType ;
   ExportFileName : string ;
begin

     if rbAllRecords.Checked then begin
        StartAt := 1 ;
        EndAt := Round(edRange.HiLimit) ;
        end
     else begin
        StartAt := Round(edRange.LoValue) ;
        EndAt := Round(edRange.HiValue) ;
        end ;

     // If destination file already exists, allow user to abort
     ExportFileName := edFileName.Text ;
     if FileExists( ExportFileName ) then begin
        if MessageDlg( ExportFileName + ' exists! Overwrite?!',
           mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit ;
         end ;

     // Channels to be exported
     UseChannel[0] :=  ckCh0.Checked ;
     UseChannel[1] :=  ckCh1.Checked ;
     UseChannel[2] :=  ckCh2.Checked ;
     UseChannel[3] :=  ckCh3.Checked ;
     UseChannel[4] :=  ckCh4.Checked ;
     UseChannel[5] :=  ckCh5.Checked ;
     UseChannel[6] :=  ckCh6.Checked ;
     UseChannel[7] :=  ckCh7.Checked ;

     // Export file type
     if rbABF.Checked then ExportType := ftAxonABF
     else if rbASCII.Checked then ExportType := ftASC
     else if rbIBW.Checked then ExportType := ftIBW
     else if rbCFS.Checked then ExportType := ftCFS
     else ExportType := ftWCP ;

     // Create empty export data file
     ExportFileName := edFileName.Text ;
     ExportFile.CreateDataFile( ExportFileName,
                                ExportType ) ;

     // Set file parameters
     ExportFile.NumChannelsPerScan := Display.NumChannels ;
     ExportFile.NumScansPerRecord := Display.MaxPoints ;
     // Ensure record sizes are multiples of 256 for export to WCP files
     if ExportType = ftWCP then begin
        ExportFile.NumScansPerRecord := Max(ExportFile.NumScansPerRecord div 256,1)*256 ;
        end ;
     ExportFile.MaxADCValue := Display.MaxADCValue ;
     ExportFile.MinADCValue := -Display.MaxADCValue - 1 ;
     ExportFile.ScanInterval := Display.TScale ;
     ExportFile.IdentLine := MainFrm.IDRFile.Ident ;
     ExportFile.RecordNum := 1 ;
     ExportFile.ABFAcquisitionMode := ftEpisodic ;

     chOut := 0 ;
     for ch := 0 to Display.NumChannels-1 do if UseChannel[ch] then begin
         ExportFile.ChannelOffset[chOut] := chOut ;
         ExportFile.ChannelADCVoltageRange[chOut] := MainFrm.IDRFile.ADCVoltageRange ;
         ExportFile.ChannelName[chOut] := Display.ChanName[ch] ;
         ExportFile.ChannelUnits[chOut] := Display.ChanUnits[ch] ;
         ExportFile.ChannelScale[chOut] := MainFrm.IDRFile.ADCChannel[ch].ADCSCale ;
         ExportFile.ChannelCalibrationFactor[chOut] := MainFrm.IDRFile.ADCChannel[ch].ADCCalibrationFactor ;
         ExportFile.ChannelGain[chOut] := MainFrm.IDRFile.ADCChannel[ch].ADCAmplifierGain ;
         Inc(chOut) ;
         end ;
     ExportFile.NumChannelsPerScan := chOut ;

     { Copy records }
     NumRecordsExported := 0 ;
     GetMem( OutBuf, MaxDisplayPoints*2 ) ;
     for EventNum := StartAt to EndAt do begin

         // Load event (in event analysis module)
         EventAnalysisFrm.DisplayEvent( EventNum ) ;

         // Copy required channels
         j := 0 ;
         for i := 0 to ExportFile.NumScansPerRecord-1 do begin
             for ch := 0 to Display.NumChannels-1 do if UseChannel[ch] then begin
                 OutBuf^[j] := EventAnalysisFrm.ADCBuf[i*Display.NumChannels +
                                                      Display.ChanOffsets[ch]] ;
                 Inc(j) ;
                 end ;
             end ;

         // Write to export file
         Inc(NumRecordsExported) ;
         ExportFile.RecordNum := NumRecordsExported ;
         ExportFile.WCPRecordAccepted := True ;
         ExportFile.WCPRecordType := 'TEST' ;
         ExportFile.WCPRecordNumber := EventNum ;
         ExportFile.WCPRecordTime :=  EventAnalysisFrm.EventTime ;
         ExportFile.WCPNumZeroAvg := Max(ExportFile.NumScansPerRecord div 20,1) ;
         ExportFile.SaveADCBuffer( 0, ExportFile.NumScansPerRecord, OutBuf^ ) ;

         // Report progress
         MainFrm.StatusBar.SimpleText := format(
         ' EXPORT EVENTS: Exporting event %d/%d to %s ',
         [EventNum,EndAt,ExportFileName]) ;

         end ;

     // Close export data file
     ExportFile.CloseDataFile ;

     // Final Report
     MainFrm.StatusBar.SimpleText := format(
     ' EXPORT EVENTS: %d events exported to %s ',
     [EndAt-StartAt+1,ExportFileName]) ;
     LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;

     FreeMem( OutBuf ) ;

     end;


procedure TExportEventsFrm.ExportFluorescenceChannel ;
// -------------------------------------------------
// Export fluorescence channel of detected events
// -------------------------------------------------
var
   StartAt,EndAt,ch,i,j : Integer ;
   EventNum : Integer ;
   UseChannel : Array[0..ChannelLimit] of Boolean ;
   OutBuf : PSmallIntArray ;
   chOut : Integer ;
   NumRecordsExported : Integer ;
   ExportType : TADCDataFileType ;
   ExportFileName : string ;
begin

     if rbAllRecords.Checked then begin
        StartAt := 1 ;
        EndAt := Round(edRange.HiLimit) ;
        end
     else begin
        StartAt := Round(edRange.LoValue) ;
        EndAt := Round(edRange.HiValue) ;
        end ;

     // If destination file already exists, allow user to abort
     ExportFileName := edFileName.Text ;
     if FileExists( ExportFileName ) then begin
        if MessageDlg( ExportFileName + ' exists! Overwrite?!',
           mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit ;
         end ;

     // Channels to be exported
     UseChannel[0] :=  ckCh0.Checked ;
     UseChannel[1] :=  ckCh1.Checked ;
     UseChannel[2] :=  ckCh2.Checked ;
     UseChannel[3] :=  ckCh3.Checked ;
     UseChannel[4] :=  ckCh4.Checked ;
     UseChannel[5] :=  ckCh5.Checked ;
     UseChannel[6] :=  ckCh6.Checked ;
     UseChannel[7] :=  ckCh7.Checked ;

     // Export file type
     if rbABF.Checked then ExportType := ftAxonABF
     else if rbASCII.Checked then ExportType := ftASC
     else if rbIBW.Checked then ExportType := ftIBW
     else ExportType := ftWCP ;

     // Create empty export data file
     ExportFile.CreateDataFile( ExportFileName,
                                ExportType ) ;

     // Set file parameters
     ExportFile.NumChannelsPerScan := Display.NumChannels ;
     ExportFile.NumScansPerRecord := Display.MaxPoints ;
     // Ensure record sizes are multiples of 256 for export to WCP files
     if ExportType = ftWCP then begin
        ExportFile.NumScansPerRecord := Max(ExportFile.NumScansPerRecord div 256,1)*256 ;
        end ;

     ExportFile.MaxADCValue := Display.MaxADCValue ;
     ExportFile.MinADCValue := -Display.MaxADCValue - 1 ;
     ExportFile.ScanInterval := Display.TScale ;
     ExportFile.IdentLine := MainFrm.IDRFile.Ident ;
     ExportFile.RecordNum := 1 ;
     ExportFile.ABFAcquisitionMode := ftEpisodic ;

     chOut := 0 ;

     for ch := 0 to Display.NumChannels-1 do if UseChannel[ch] then begin
         ExportFile.ChannelOffset[chOut] := chOut ;
         ExportFile.ChannelADCVoltageRange[chOut] := 10.0 ;
         ExportFile.ChannelName[chOut] := Display.ChanName[ch] ;
         ExportFile.ChannelUnits[chOut] := Display.ChanUnits[ch] ;
         ExportFile.ChannelScale[chOut] := Display.ChanScale[ch] ;
         ExportFile.ChannelGain[chOut] := 1.0 ;
         ExportFile.ChannelCalibrationFactor[chOut] :=
              ExportFile.ChannelADCVoltageRange[chOut] /
              (Display.ChanScale[ch]*ExportFile.ChannelGain[chOut]*
               (ExportFile.MaxADCValue+1)) ;
         Inc(chOut) ;
         end ;
     ExportFile.NumChannelsPerScan := chOut ;

     { Copy records }
     NumRecordsExported := 0 ;
     GetMem( OutBuf, MaxDisplayPoints*2 ) ;
     for EventNum := StartAt to EndAt do begin

         // Load event (in event analysis module)
         EventAnalysisFrm.DisplayEvent( EventNum ) ;

         // Copy required channels
         j := 0 ;
         for i := 0 to ExportFile.NumScansPerRecord-1 do begin
             for ch := 0 to Display.NumChannels-1 do if UseChannel[ch] then begin
                 OutBuf^[j] := EventAnalysisFrm.FLBuf[i*Display.NumChannels + ch] ;
                 Inc(j) ;
                 end ;
             end ;

         // Write to export file
         Inc(NumRecordsExported) ;
         ExportFile.RecordNum := NumRecordsExported ;
         ExportFile.WCPRecordAccepted := True ;
         ExportFile.WCPRecordType := 'TEST' ;
         ExportFile.WCPRecordNumber := EventNum ;
         ExportFile.WCPRecordTime :=  EventAnalysisFrm.EventTime ;
         ExportFile.WCPNumZeroAvg := Max(ExportFile.NumScansPerRecord div 20,1) ;

         ExportFile.SaveADCBuffer( 0, ExportFile.NumScansPerRecord, OutBuf^ ) ;

         // Report progress
         MainFrm.StatusBar.SimpleText := format(
         ' EXPORT EVENTS: Exporting event %d/%d to %s ',
         [EventNum,EndAt,ExportFileName]) ;

         end ;

     // Close export data file
     ExportFile.CloseDataFile ;

     // Final Report
     MainFrm.StatusBar.SimpleText := format(
     ' EXPORT EVENTS: %d events exported to %s ',
     [EndAt-StartAt+1,ExportFileName]) ;
     LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;

     FreeMem( OutBuf ) ;

     end;



procedure TExportEventsFrm.bChangeNameClick(Sender: TObject);
{ ------------------------------------------
  Change name/location of export destination
  ------------------------------------------ }
begin
     SaveDialog.DefaultExt := ExtractFileExt( edFileName.Text ) ;
     SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.Filter := ' Files (*' + SaveDialog.DefaultExt + ')|*' +
                            SaveDialog.DefaultExt + '|' ;

     SaveDialog.FileName := edFileName.Text ;
     SaveDialog.Title := 'Export File ' ;
     if MainFrm.DataDirectory <> '' then
        SaveDialog.InitialDir := MainFrm.DataDirectory ;

     if SaveDialog.Execute then edFileName.Text := SaveDialog.FileName ;
     end;


procedure TExportEventsFrm.rbABFClick(Sender: TObject);
// ---------------------------------
// Axon Binary File option selected
// ---------------------------------
begin
     UpdateSettings ;
     //ChannelsGrp.Enabled := False ;
     end;

procedure TExportEventsFrm.rbCFSClick(Sender: TObject);
// ---------------------------------
// CED Filing System option selected
// ---------------------------------
begin
     UpdateSettings ;
     ChannelsGrp.Enabled := False ;
     end;


procedure TExportEventsFrm.UpdateSettings ;
// ---------------------------------------------------
// Update control settings when export format changed
// ---------------------------------------------------
var
    FileTag : String ;
begin

     // List channels exported in file name
     FileTag := ' [' ;
     if ckCh0.Checked then FileTag := FileTag + ckCh0.Caption + ' ';
     if ckCh1.Checked then FileTag := FileTag + ckCh1.Caption + ' ' ;
     if ckCh2.Checked then FileTag := FileTag + ckCh2.Caption + ' ' ;
     if ckCh3.Checked then FileTag := FileTag + ckCh3.Caption + ' ' ;
     if ckCh4.Checked then FileTag := FileTag + ckCh4.Caption + ' ' ;
     if ckCh5.Checked then FileTag := FileTag + ckCh5.Caption + ' ' ;
     if ckCh6.Checked then FileTag := FileTag + ckCh6.Caption + ' ' ;
     if ckCh7.Checked then FileTag := FileTag + ckCh7.Caption + ' ' ;
     FileTag := LeftStr(FileTag,Length(FileTag)-1) + ']' ;

     edFileName.Text := ANSIReplaceText( MainFrm.IDRFile.FileName,'.idr',FileTag+'.idr') ;

     if rbABF.Checked then edFileName.Text := ChangeFileExt( edFileName.Text, '.abf' ) ;
     if rbASCII.Checked then edFileName.Text := ChangeFileExt( edFileName.Text, '.txt' ) ;
     if rbWCP.Checked then edFileName.Text := ChangeFileExt( edFileName.Text, '.wcp' ) ;
     if rbIBW.Checked then edFileName.Text := ChangeFileExt( edFileName.Text, '.ibw' ) ;
     if rbCFS.Checked then edFileName.Text := ChangeFileExt( edFileName.Text, '.cfs' ) ;
     edFileName.Text := RemoveInvalidFileNameCharacters( edFileName.Text ) ;

     end ;


function TExportEventsFrm.RemoveInvalidFileNameCharacters(
          FileName : string ) : String ;
begin
      Result := ANSIReplaceText( FileName, '/', '-' ) ;
      end ;


procedure TExportEventsFrm.rbASCIIClick(Sender: TObject);
// ---------------------------------
// ASCII text file option selected
// ---------------------------------
begin
     UpdateSettings ;
     //ChannelsGrp.Enabled := False ;
     end;

procedure TExportEventsFrm.rbWCPClick(Sender: TObject);
// ---------------------------------
// WinEDR file option selected
// ---------------------------------
begin
     UpdateSettings ;
     //ChannelsGrp.Enabled := False ;
     end;

procedure TExportEventsFrm.ckCh0Click(Sender: TObject);
// -------------------------
// Channel selection changed
// -------------------------
var
    NumChannels : Integer ;
begin

     NumChannels := 0 ;
     if ckCh0.Checked then Inc(NumChannels) ;
     if ckCh1.Checked then Inc(NumChannels) ;
     if ckCh2.Checked then Inc(NumChannels) ;
     if ckCh3.Checked then Inc(NumChannels) ;
     if ckCh4.Checked then Inc(NumChannels) ;
     if ckCh5.Checked then Inc(NumChannels) ;
     if ckCh6.Checked then Inc(NumChannels) ;
     if ckCh7.Checked then Inc(NumChannels) ;
     if NumChannels < 1 then TCheckBox(Sender).Checked := True ;

     // Ensure only one channel selected for IBW
     if rbIBW.Checked and TCheckBox(Sender).Checked then begin
        if ckCh0.Tag <> TCheckBox(Sender).Tag then ckCh0.Checked := False ;
        if ckCh1.Tag <> TCheckBox(Sender).Tag then ckCh1.Checked := False ;
        if ckCh2.Tag <> TCheckBox(Sender).Tag then ckCh2.Checked := False ;
        if ckCh3.Tag <> TCheckBox(Sender).Tag then ckCh3.Checked := False ;
        if ckCh4.Tag <> TCheckBox(Sender).Tag then ckCh4.Checked := False ;
        if ckCh5.Tag <> TCheckBox(Sender).Tag then ckCh5.Checked := False ;
        if ckCh6.Tag <> TCheckBox(Sender).Tag then ckCh6.Checked := False ;
        if ckCh7.Tag <> TCheckBox(Sender).Tag then ckCh7.Checked := False ;
        end ;

     UpdateSettings ;

     end;

procedure TExportEventsFrm.rbIBWClick(Sender: TObject);
var
    NumChannels : Integer ;
begin
     // Ensure only one channel selected for IBW
     if rbIBW.Checked then begin
        NumChannels := 0 ;
        if ckCh0.Checked then Inc(NumChannels) ;
        if ckCh1.Checked and (NumChannels > 0) then ckCh1.Checked := False
                                               else Inc(NumChannels) ;
        if ckCh2.Checked and (NumChannels > 0) then ckCh2.Checked := False
                                               else Inc(NumChannels) ;
        if ckCh3.Checked and (NumChannels > 0) then ckCh3.Checked := False
                                               else Inc(NumChannels) ;
        if ckCh4.Checked and (NumChannels > 0) then ckCh4.Checked := False
                                               else Inc(NumChannels) ;
        if ckCh5.Checked and (NumChannels > 0) then ckCh5.Checked := False
                                               else Inc(NumChannels) ;
        if ckCh6.Checked and (NumChannels > 0) then ckCh6.Checked := False
                                               else Inc(NumChannels) ;
        if ckCh7.Checked and (NumChannels > 0) then ckCh7.Checked := False
                                               else Inc(NumChannels) ;

        end ;

     UpdateSettings ;

     end;

end.
