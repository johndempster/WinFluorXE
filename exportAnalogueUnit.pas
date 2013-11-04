unit exportAnalogueUnit;
{ ================================================================
  WinEDR (c) J. Dempster, University of Strathclyde, 1998-99
  Data file export module
  ================================================================
  5/2/00
  28/2/00 ... Change File Name now works, channel captions updated
  9/2/02 ... V2.2.6 ASCII text export added
  26/2/02 ... V2.3.0 ASCII text export now works properly
              Progress reported to main status bar
  14/08/02 ... Bug which prevented export when output file did not exist fixed  (V2.3.4)
  1/12/03 .... Export now uses TADCDataFile component
  24/09/06 ... Fixing export to WCP files
  19/01/10 ... Export to Igor binary wave added
  }
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RangeEdit, ADCDataFile, maths, math, MATFileWriterUnit ;

type
  TExportAnalogueFrm = class(TForm)
    GroupBox8: TGroupBox;
    rbAllRecords: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    ChannelsGrp: TGroupBox;
    ckCh0: TCheckBox;
    ckCh1: TCheckBox;
    ckCh2: TCheckBox;
    ckCh3: TCheckBox;
    ckCh4: TCheckBox;
    ckCh5: TCheckBox;
    ckCh6: TCheckBox;
    ckCh7: TCheckBox;
    GroupBox3: TGroupBox;
    edFileName: TEdit;
    GroupBox2: TGroupBox;
    rbABF: TRadioButton;
    bChangeName: TButton;
    bOK: TButton;
    bCancel: TButton;
    SaveDialog: TSaveDialog;
    rbASCII: TRadioButton;
    rbEDR: TRadioButton;
    ExportFile: TADCDataFile;
    rbMAT: TRadioButton;
    rbIBW: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bChangeNameClick(Sender: TObject);
    procedure rbABFClick(Sender: TObject);
    procedure rbLDTClick(Sender: TObject);
    procedure rbCFSClick(Sender: TObject);
    procedure rbASCIIClick(Sender: TObject);
    procedure rbEDRClick(Sender: TObject);
    procedure rbMATClick(Sender: TObject);      // Added by NS 18 February 2009
    procedure bCancelClick(Sender: TObject);
    procedure ckCh0Click(Sender: TObject);
    procedure rbIBWClick(Sender: TObject);
  private
    { Private declarations }
    ExportFileName : string ;
    procedure ExportMATFile(Name: String;       // Added by NS 18 February 2009
                            StartAt: Integer;
                            EndAt: Integer;
                            UseChannel: Array of Boolean);
    procedure SetChannel( CheckBox : TCheckBox ; ch : Integer ) ;
    procedure UpdateSettings ;
  public
    { Public declarations }
  end;

var
  ExportAnalogueFrm: TExportAnalogueFrm;

implementation

uses Main , LogUnit, strutils ;

{$R *.DFM}


procedure TExportAnalogueFrm.FormShow(Sender: TObject);
// ------------------------------
// Initialise form when displayed
// ------------------------------
begin

     { Set block of EDR file to be exported }
     edRange.LoLimit := 0.0 ;
     edRange.LoValue := 0.0 ;
     edRange.HiLimit := MainFrm.IDRFile.ADCNumScansInFile*MainFrm.IDRFile.ADCSCanInterval ;
     edRange.HiValue := edRange.HiLimit ;

     SetChannel( ckCh0, 0 ) ;
     SetChannel( ckCh1, 1 ) ;
     SetChannel( ckCh2, 2 ) ;
     SetChannel( ckCh3, 3 ) ;
     SetChannel( ckCh4, 4 ) ;
     SetChannel( ckCh5, 5 ) ;
     SetChannel( ckCh6, 6 ) ;
     SetChannel( ckCh7, 7 ) ;

     { Update O/P file name channel selection options }
     ExportFileName := MainFrm.IDRFile.FileName ;
     UpdateSettings ;
     ChannelsGrp.Enabled := True ;

     end;


procedure TExportAnalogueFrm.SetChannel(
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
     else CheckBox.Visible := False ;
     end ;


procedure TExportAnalogueFrm.bOKClick(Sender: TObject);
// -------------------------------------------------
// Copy selected section of data file to export file
// -------------------------------------------------
const
   NumScansPerBuf = 256 ;
var
   StartAt,EndAt,ch,i,j : Integer ;
   UseChannel : Array[0..ChannelLimit] of Boolean ;
   InBuf : Array[0..NumScansPerBuf*(ChannelLimit+1)-1] of SmallInt ;
   OutBuf : Array[0..NumScansPerBuf*(ChannelLimit+1)-1] of SmallInt ;
   InScan : Integer ;
   OutScan : Integer ;
   NumScansToCopy : Integer ;
   NumScansToRead : Integer ;
   NumScansRead : Integer ;
   chOut : Integer ;
   Done : Boolean ;
   ExportType : TADCDataFileType ;
begin

     bOK.Enabled := False ;

     if rbAllRecords.Checked then begin
        StartAt := 0 ;
        EndAt := MainFrm.IDRFile.ADCNumScansInFile ;
        end
     else begin
        StartAt := Round(edRange.LoValue/MainFrm.IDRFile.ADCSCanInterval) ;
        EndAt := Round(edRange.HiValue/MainFrm.IDRFile.ADCSCanInterval) ;
        end ;

     // If destination file already exists, allow user to abort
     if FileExists( ExportFileName ) then begin
        if MessageDlg( ExportFileName + ' exists! Overwrite?!',
           mtConfirmation, [mbYes, mbNo], 0) <> mrYes then begin
           bOK.Enabled := True ;
           Exit ;
           end ;
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
     else ExportType := ftEDR ;

     // Check MAT-File export option
     // Added by NS 18 February 2009
     if rbMAT.Checked then
     begin
      //MessageDlg('Exporting to MAT-File format not yet implemented.', mtWarning, [mbOK], 0);
      ExportMATFile(ExportFileName, StartAt, EndAt, UseChannel);
      bOK.Enabled := True;
      Exit;
     end;

     // Create empty export data file
     ExportFile.CreateDataFile( ExportFileName,
                                ExportType ) ;

     // Set file parameters
     ExportFile.NumChannelsPerScan := MainFrm.IDRFile.ADCNumChannels ;
     ExportFile.NumScansPerRecord := EndAt - StartAt + 1 ;
     ExportFile.MaxADCValue := MainFrm.IDRFile.ADCMaxValue ;
     ExportFile.MinADCValue := -MainFrm.IDRFile.ADCMaxValue - 1 ;
     ExportFile.ScanInterval := MainFrm.IDRFile.ADCSCanInterval ;
     ExportFile.IdentLine := MainFrm.IDRFile.Ident ;
     ExportFile.RecordNum := 1 ;
     ExportFile.ABFAcquisitionMode := ftGapFree ;

     chOut := 0 ;
     for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do if UseChannel[ch] then begin
         ExportFile.ChannelOffset[chOut] := chOut ;
         ExportFile.ChannelADCVoltageRange[chOut] := MainFrm.IDRFile.ADCVoltageRange ;
         ExportFile.ChannelName[ch] := MainFrm.IDRFile.ADCChannel[chOut].ADCName ;
         ExportFile.ChannelUnits[ch] := MainFrm.IDRFile.ADCChannel[chOut].ADCUnits ;
         ExportFile.ChannelScale[ch] := MainFrm.IDRFile.ADCChannel[chOut].ADCSCale ;
         ExportFile.ChannelCalibrationFactor[chOut] := MainFrm.IDRFile.ADCChannel[ch].ADCCalibrationFactor ;
         ExportFile.ChannelGain[chOut] := MainFrm.IDRFile.ADCChannel[ch].ADCAmplifierGain ;
         Inc(chOut) ;
         end ;
     ExportFile.NumChannelsPerScan := chOut ;

     { Copy records }
     InScan := StartAt ;
     NumScansToCopy := EndAt - StartAt + 1 ;
     OutScan := 0 ;
     Done := False ;
     While (not Done) and (not bOK.Enabled) do begin

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

         // Write to export file
         ExportFile.SaveADCBuffer( OutScan, NumScansRead, OutBuf ) ;
         OutScan := OutScan + NumScansRead ;

         // Report progress
         MainFrm.StatusBar.SimpleText := format(
         ' EXPORT: Exporting scans %d/%d to %s ',
         [InScan,EndAt,ExportFileName]) ;

         InScan := InScan + NumScansRead ;
         NumScansToCopy := NumScansToCopy - NumScansRead ;
         if NumScansToCopy <= 0 then Done := True ;

         end ;

     // Close export data file
     ExportFile.CloseDataFile ;

     // Final Report
     MainFrm.StatusBar.SimpleText := format(
     ' EXPORT: %d scans exported to %s ',
     [EndAt-StartAt+1,ExportFileName]) ;
     LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;

     bOK.Enabled := True ;

     end;


procedure TExportAnalogueFrm.bChangeNameClick(Sender: TObject);
{ ------------------------------------------
  Change name/location of export destination
  ------------------------------------------ }
begin
     SaveDialog.DefaultExt := ExtractFileExt( ExportFileName ) ;
     SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.Filter := ' Files (*' + SaveDialog.DefaultExt + ')|*' +
                            SaveDialog.DefaultExt + '|' ;

     SaveDialog.FileName := ExportFileName ;
     SaveDialog.Title := 'Export File ' ;
     if MainFrm.DataDirectory <> '' then
        SaveDialog.InitialDir := MainFrm.DataDirectory ;

     if SaveDialog.Execute then ExportFileName := SaveDialog.FileName ;
     edFileName.text := ExportFileName ;
     end;


procedure TExportAnalogueFrm.rbABFClick(Sender: TObject);
// ---------------------------------
// Axon Binary File option selected
// ---------------------------------
begin
     UpdateSettings ;
     //ChannelsGrp.Enabled := False ;
     end;


procedure TExportAnalogueFrm.rbLDTClick(Sender: TObject);
// ---------------------------------
// Qub data file option selected
// ---------------------------------
begin
     UpdateSettings ;
     ChannelsGrp.Enabled := True ;
     end;


procedure TExportAnalogueFrm.rbCFSClick(Sender: TObject);
// ---------------------------------
// CED Filing System option selected
// ---------------------------------
begin
     UpdateSettings ;
     ChannelsGrp.Enabled := False ;
     end;


procedure TExportAnalogueFrm.UpdateSettings ;
// ---------------------------------------------------
// Update control settings when export format changed
// ---------------------------------------------------
var
    FileTag : String ;
    UseChannel : Array[0..ChannelLimit] of Boolean ;
    i, NumChannels : Integer ;
begin

     UseChannel[0] :=  ckCh0.Checked ;
     UseChannel[1] :=  ckCh1.Checked ;
     UseChannel[2] :=  ckCh2.Checked ;
     UseChannel[3] :=  ckCh3.Checked ;
     UseChannel[4] :=  ckCh4.Checked ;
     UseChannel[5] :=  ckCh5.Checked ;
     UseChannel[6] :=  ckCh6.Checked ;
     UseChannel[7] :=  ckCh7.Checked ;

     // Find no. of channels selected
     NumChannels := 0 ;
     for i := 0 to MainFrm.IDRFile.ADCNumChannels-1 do if UseChannel[i] then begin
         Inc(NumChannels) ;
         end ;

     if NumChannels <= 0 then begin
        // Ensure at least one channel selected
        UseChannel[0] := True ;
        end
     else if (NumChannels > 1) and rbIBW.Checked then begin
          // No more than one channel for IBW
          NumChannels := 0 ;
          for i := 0 to MainFrm.IDRFile.ADCNumChannels-1 do if UseChannel[i] then begin
              Inc(NumChannels) ;
              if NumChannels > 1 then UseChannel[i] := False ;
              end ;
         end ;

     FileTag := ' [' ;
     for i := 0 to MainFrm.IDRFile.ADCNumChannels-1 do if UseChannel[i] then begin
         FileTag := FileTag + MainFrm.IDRFile.ADCChannel[i].ADCName + ' ' ;
         end ;
     FileTag := LeftStr(FileTag,Length(FileTag)-1) + ']' ;

     ExportFileName := ANSIReplaceText( MainFrm.IDRFile.FileName,'.idr',FileTag+'.idr') ;

     if rbABF.Checked then ExportFileName := ChangeFileExt( ExportFileName, '.abf' ) ;
     if rbASCII.Checked then ExportFileName := ChangeFileExt( ExportFileName, '.txt' ) ;
     if rbIBW.Checked then ExportFileName := ChangeFileExt( ExportFileName, '.ibw' ) ;
     if rbEDR.Checked then begin
        ExportFileName := ChangeFileExt( ExportFileName, '.edr' ) ;
        if LowerCase(ExportFileName) = LowerCase(MainFrm.IDRFile.FileName) then begin
           ExportFileName := StringReplace( ExportFileName,
                                            '.edr',
                                            '[export].edr',
                                            [rfIgnoreCase] ) ;
           end ;
        end ;
     if rbMAT.Checked then ExportFileName := ChangeFileExt( ExportFileName, '.mat' ) ;  // Added by NS 18 February 2009
     edFileName.text := ExportFileName ;
     end ;


procedure TExportAnalogueFrm.rbASCIIClick(Sender: TObject);
// ---------------------------------
// ASCII text file option selected
// ---------------------------------
begin
     UpdateSettings ;
     //ChannelsGrp.Enabled := False ;
     end;


procedure TExportAnalogueFrm.rbEDRClick(Sender: TObject);
// ---------------------------------
// WinEDR file option selected
// ---------------------------------
begin
     UpdateSettings ;
     ChannelsGrp.Enabled := True ;
     end;


procedure TExportAnalogueFrm.rbMATClick(Sender: TObject);
// ---------------------------------
// MAT-File option selected
// Added by NS 18 February 2009
// ---------------------------------
begin
     UpdateSettings ;
     ChannelsGrp.Enabled := True ;
     end;


procedure TExportAnalogueFrm.bCancelClick(Sender: TObject);
// -------------
// Cancel export
// -------------
begin
     if not bOK.Enabled then Close
     else bOK.Enabled := True ;
     end;


procedure TExportAnalogueFrm.ExportMATFile(Name: String;                   // MAT file name
                                           StartAt: Integer;               // Start at scan
                                           EndAt: Integer;                 // End at scan
                                           UseChannel: Array of Boolean);  // Channels to export
// ----------------------------
// Export MAT-File
// Added by NS 18 February 2009
// ----------------------------
const
  NumScansPerBuf = 256;                                                 // Fixed number of scans per buffer
var
  ch: Integer;                                                          // Channel number
  Done: Boolean;                                                        // Loop for export while loop
  i: Integer;                                                           // Loop index
  InBuf: Array[0..NumScansPerBuf * (ChannelLimit + 1) - 1] of SmallInt; // Buffer for holding values from IDRFile
  InScan: Integer;                                                      // Total number of scans read in from IDRFile
  NumScansRead: Integer;                                                // Scans that were read per iteration
  NumScansToCopy: Integer;                                              // Total number of scans to be copied
  NumScansToRead: Integer;                                              // Scans to read per loop iteration
  OutBuf: Array[0..NumScansPerBuf * (ChannelLimit + 1) - 1] of Double;  // Buffer of values to be exported to MAT file
  Writer: TMATFileWriter;                                               // MAT-File writer
begin

  // Create new TMATFileWriter object
  Writer := TMATFileWriter.Create();

  // Create new file
  Writer.OpenMATFile(Name);

  // Write file header
  Writer.WriteFileHeader;


  // Total number of scans to copy
  NumScansToCopy := EndAt - StartAt + 1;

  // Check that total number of scans in file is not exceeded
  if EndAt >= MainFrm.IDRFile.ADCNumScansInFile then
  begin
    NumScansToCopy := NumScansToCopy - 1;
  end;

  // Write header for double matrix that holds time base
  Writer.WriteDoubleMatrixHeader('time_s', NumScansToCopy);

  // Create and write time base
  for i := 0 to NumScansToCopy - 1 do
  begin

    // Calculate time
    OutBuf[0] := (i + StartAt) * MainFrm.IDRFile.ADCSCanInterval;

    // Write values to output file
    Writer.WriteDoubleMatrixValues(OutBuf, 1);

  end;


  // Copy each channel to output file
  for ch := 0 to MainFrm.IDRFile.ADCNumChannels - 1 do
  begin
    if UseChannel[ch] then
    begin

      // Total number of scans to copy
      NumScansToCopy := EndAt - StartAt + 1;

      // Check that total number of scans in file is not exceeded
      if EndAt >= MainFrm.IDRFile.ADCNumScansInFile then
      begin
        NumScansToCopy := NumScansToCopy - 1;
      end;

      // Write header for double matrix
      Writer.WriteDoubleMatrixHeader(MainFrm.IDRFile.ADCChannel[ch].ADCName +
                                     '_' +
                                     MainFrm.IDRFile.ADCChannel[ch].ADCUnits,
                                     NumScansToCopy);

      // Set done flag
      Done := False;

      // Set scan number in input buffer
      InScan := StartAt;

      // Copy scans in intervals of NumScansPerBuf
      while (not Done) do
      begin

        // Determine number of scans to read
        NumScansToRead := Min(NumScansToCopy, NumScansPerBuf);

        // Read scans
        NumScansRead := MainFrm.IDRFile.LoadADC(InScan, NumScansToRead, InBuf);

        // Check if nothing was read
        if NumScansRead <= 0 then
        begin
          Done := True;
        end;

        // Copy scans to output buffer and convert to double format
        for i := 0 to NumScansRead - 1 do
        begin
          OutBuf[i] := InBuf[(i*MainFrm.IDRFile.ADCNumChannels) +
                             MainFrm.IDRFile.ADCChannel[ch].ChannelOffset] *
                       MainFrm.IDRFile.ADCChannel[ch].ADCSCale;
        end;

        // Write values to output file
        Writer.WriteDoubleMatrixValues(OutBuf, NumScansRead);

        // Report progress
        MainFrm.StatusBar.SimpleText := format(
        ' EXPORT: Exporting scans %d/%d of channel %s to %s ',
        [InScan, EndAt, MainFrm.IDRFile.ADCChannel[ch].ADCName, ExportFileName]);

        // Update position in input buffer
        InScan := InScan + NumScansRead;

        // Update number of scan left to copy
        NumScansToCopy := NumScansToCopy - NumScansRead;

        // Check if finished
        if NumScansToCopy <= 0 then
        begin
          Done := True;
        end;
        
      end;

    end;
  end;


  // Final Report
  MainFrm.StatusBar.SimpleText := format(
  ' EXPORT: %d scans exported to %s ',
  [EndAt - StartAt + 1, ExportFileName]);
  LogFrm.AddLine(MainFrm.StatusBar.SimpleText);


  // Close file
  Writer.CloseMATFile;

end;


procedure TExportAnalogueFrm.ckCh0Click(Sender: TObject);
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

procedure TExportAnalogueFrm.rbIBWClick(Sender: TObject);
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
