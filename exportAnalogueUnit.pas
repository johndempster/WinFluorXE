unit exportAnalogueUnit;
{ ================================================================
  WinFluor (c) J. Dempster, University of Strathclyde, 1998-2015
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
  13.11.12 ... .LOADADC() now uses 64 bit scan counter
  17.02.15 ... MAT file export now working correctly
  14.11.16 ... Multiple files can now be selected for export
  02.05.17 ... Select Files to Export button now works
              ViewFrm.NewFile now updated (if form exists) to ensure
              that time buffers are updated when files changed to
              avoid access violations.
  }
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RangeEdit, ADCDataFile, maths, math, MATFileWriterUnit, labIOUnit, UITYpes ;

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
    GroupBox2: TGroupBox;
    rbABF: TRadioButton;
    bOK: TButton;
    bCancel: TButton;
    SaveDialog: TSaveDialog;
    rbASCII: TRadioButton;
    rbEDR: TRadioButton;
    ExportFile: TADCDataFile;
    rbMAT: TRadioButton;
    rbIBW: TRadioButton;
    FilesToExportGrp: TGroupBox;
    bSelectFilesToExport: TButton;
    meFiles: TMemo;
    bClearList: TButton;
    OpenDialog: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure ckCh0Click(Sender: TObject);
    procedure rbIBWClick(Sender: TObject);
    procedure bSelectFilesToExportClick(Sender: TObject);
  private
    { Private declarations }
    ExportFileName : string ;
    FileOnDisplay : string ;
    procedure ExportToFile ;
    procedure SetChannel( CheckBox : TCheckBox ; ch : Integer ) ;
    function CreateExportFileName : string ;
  public
    { Public declarations }
  end;

var
  ExportAnalogueFrm: TExportAnalogueFrm;

implementation

uses Main , LogUnit, strutils, ViewUnit ;

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
     ExportFileName := CreateExportFileName ;
     ChannelsGrp.Enabled := True ;

     // Keep record of currently open file
     FileOnDisplay := MainFrm.IDRFile.FileName ;

     // Add currently open file to export list
     meFiles.Clear ;
     meFiles.Lines.Add(MainFrm.IDRFile.FileName) ;

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
// ------------
// Export files
// ------------
begin
    ExportToFile ;
    end;


procedure TExportAnalogueFrm.bSelectFilesToExportClick(Sender: TObject);
// ----------------------------
// Select files to be exported
// ----------------------------
var
    i : Integer ;
begin
     OpenDialog.InitialDir := ExtractFilePath( FileOnDisplay ) ;
     OpenDialog.Title := ' Files to be Exported ' ;
     OpenDialog.options := [ofHideReadOnly,ofPathMustExist,ofAllowMultiSelect] ;
     OpenDialog.DefaultExt := '.idr' ;
     OpenDialog.Filter := ' WinFluor (*.idr)|*.idr' ;

     OpenDialog.FileName := '' ;
     OpenDialog.Execute ;
     for i := 0 to OpenDialog.Files.Count-1 do
         meFiles.Lines.Add(OpenDialog.Files[i]);
     end ;


procedure TExportAnalogueFrm.ExportToFile ;
// -------------------------------------------------
// Copy selected section of data file to export file
// -------------------------------------------------
const
   NumScansPerBuf = 256 ;
var
   StartAt,EndAt,ch,i,j,ifrom,iTo,izero,iFile : Integer ;
   UseChannel : Array[0..ChannelLimit] of Boolean ;
   InBuf : Array[0..NumScansPerBuf*(ChannelLimit+1)-1] of SmallInt ;
   OutBuf : Array[0..NumScansPerBuf*(ChannelLimit+1)-1] of SmallInt ;
   YScale : single ;
   InScan : Int64 ;
   OutScan : Integer ;
   NumScansToCopy : Integer ;
   NumScansToRead : Integer ;
   NumScansRead : Integer ;
   nScansExported : Integer ;
   NumScansToExport : Integer ;
   nChannelsExported : Integer ;
   chOut : Integer ;
   Done : Boolean ;
   ExportType : TADCDataFileType ;
   TMat : PBigDoubleArray ;        // Time data array
   YMat : PBigDoubleArray ;        // Time data array
   Writer : TMATFileWriter ;
   FileName : string ;
begin

     bOK.Enabled := False ;

     for iFile := 0 to meFiles.Lines.Count-1 do begin

         // Close existing file
         MainFrm.IDRFile.CloseFile ;

         // Open file to export
         MainFrm.IDRFile.OpenFile( meFiles.Lines[iFile]) ;
         if MainFrm.FormExists( 'ViewFrm' ) then ViewFrm.NewFile ;

         ExportFileName := CreateExportFileName ;

         //Select range
         if rbAllRecords.Checked then
            begin
            StartAt := 0 ;
            EndAt := MainFrm.IDRFile.ADCNumScansInFile ;
            end
         else
            begin
            StartAt := Round(edRange.LoValue/MainFrm.IDRFile.ADCSCanInterval) ;
            EndAt := Round(edRange.HiValue/MainFrm.IDRFile.ADCSCanInterval) ;
            end ;

         // If destination file already exists, allow user to abort
         if FileExists( ExportFileName ) then begin
            if MessageDlg( ExportFileName + ' exists! Overwrite?!',
               mtConfirmation, [mbYes, mbNo], 0) <> mrYes then continue ;
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
             NumScansToExport := EndAt - StartAt + 1 ;
             GetMem( TMat, NumScansToExport*SizeOf(Double) ) ;
             GetMem( YMat, MainFrm.IDRFile.ADCNumChannels*NumScansToExport*SizeOf(Double) ) ;
             Writer := TMATFileWriter.Create();
             Writer.OpenMATFile( ExportFileName ) ;
             Writer.WriteFileHeader;
             end
         else
            begin
            // Create empty export data file
            ExportFile.CreateDataFile( ExportFileName, ExportType ) ;
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
            for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do if UseChannel[ch] then
                begin
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
            end ;

         { Copy records }
         InScan := StartAt ;
         NumScansToCopy := EndAt - StartAt + 1 ;
         OutScan := 0 ;
         nScansExported := 0 ;
         Done := False ;
         While (not Done) and (not bOK.Enabled) do
            begin
            // Read from buffer
            NumScansToRead := Min( NumScansToCopy,NumScansPerBuf ) ;
            NumScansRead := MainFrm.IDRFile.LoadADC( InScan, NumScansToRead, InBuf ) ;
            if NumScansRead <= 0 then Done := True ;

            // Copy required channels

            // Write to export file
            if rbMat.Checked then begin
               // Export to MAT file
               nChannelsExported :=  0 ;
               for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do if UseChannel[ch] then
                    begin
                    ifrom := Round(MainFrm.IDRFile.ADCChannel[ch].ChannelOffset) ;
                    iZero := Round(MainFrm.IDRFile.ADCChannel[ch].ADCZero) ;
                    YScale := MainFrm.IDRFile.ADCChannel[ch].ADCScale ;
                    iTo := NumScansToExport*nChannelsExported + nScansExported ;
                    for i := 0 to NumScansRead-1 do
                        begin
                        YMat^[iTo] := (InBuf[iFrom] - iZero)*YScale ;
                        inc(iTo) ;
                        iFrom := iFrom + MainFrm.IDRFile.ADCNumChannels ;
                        end ;
                    Inc(nChannelsExported) ;
                   end ;
               nScansExported := nScansExported + NumScansRead ;
               end
            else
               begin
               // All other formats
               j := 0 ;
               for i := 0 to NumScansRead-1 do
                   begin
                   for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do if UseChannel[ch] then
                       begin
                       OutBuf[j] := InBuf[i*MainFrm.IDRFile.ADCNumChannels +
                                    MainFrm.IDRFile.ADCChannel[ch].ChannelOffset] ;
                       Inc(j) ;
                       end ;
                   end ;
               ExportFile.SaveADCBuffer( OutScan, NumScansRead, OutBuf ) ;
               OutScan := OutScan + NumScansRead ;
               end ;

            // Report progress
            MainFrm.StatusBar.SimpleText := format(
            ' EXPORT: Exporting scans %d/%d to %s ',
            [InScan,EndAt,ExportFileName]) ;

            InScan := InScan + NumScansRead ;
            NumScansToCopy := NumScansToCopy - NumScansRead ;
            if NumScansToCopy <= 0 then Done := True ;
            end ;

        if rbMat.Checked then
           begin
           // Write to MAT file and close
           for i := 0 to nScansExported-1 do TMat^[i] := i*MainFrm.IDRFile.ADCSCanInterval ;
           Writer.WriteDoubleMatrixHeader('T',nScansExported,1);
           Writer.WriteDoubleMatrixValues( TMat^,nScansExported,1) ;
           Writer.WriteDoubleMatrixHeader('Y',nScansExported,nChannelsExported);
           Writer.WriteDoubleMatrixValues( YMat^, nScansExported,nChannelsExported) ;
           Writer.CloseMATFile;
           FreeMem(YMat) ;
           FreeMem(TMat) ;
           end
         else
           begin
           // Close export data file
           ExportFile.CloseDataFile ;
           end ;

         // Final Report
         MainFrm.StatusBar.SimpleText := format(
         ' EXPORT: %d scans exported to %s ',
         [EndAt-StartAt+1,ExportFileName]) ;
         LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;
         end;

     bOK.Enabled := True ;

     // Re-open file which was on display
     MainFrm.IDRFile.CloseFile ;
     MainFrm.IDRFile.OpenFile( FileOnDisplay ) ;
     if MainFrm.FormExists( 'ViewFrm' ) then ViewFrm.NewFile ;

     end;


function TExportAnalogueFrm.CreateExportFileName : string ;
// ---------------------------------------------------
// Update control settings when export format changed
// ---------------------------------------------------
var
    FileTag : String ;
    UseChannel : Array[0..ChannelLimit] of Boolean ;
    i, NumChannels : Integer ;
    ExportFileName : string ;
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

     Result := ExportFileName ;
     end ;


procedure TExportAnalogueFrm.bCancelClick(Sender: TObject);
// -------------
// Cancel export
// -------------
begin
     if not bOK.Enabled then Close
     else bOK.Enabled := True ;
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

     end;

end.
