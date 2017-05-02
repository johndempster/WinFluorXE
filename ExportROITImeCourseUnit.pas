unit ExportROITImeCourseUnit;
//
// Export ROI intensity time course
// --------------------------------
// 21-10-10 Time course for ROI#s > ROI1 no longer set to zero
// 22-7-11 Option to export all ROIs in a single operation added
// 27-7-11 CFS export added
// 17-2-15 MAT export added
// 09-11-16 Exported to ABF files as +/- 16 bit integers
// 14-11-16 Export of multiple file list now supported
// 02.05.17 ... ViewFrm.NewFile now updated (if form exists) to ensure
//              that time buffers are updated when files changed to
//              avoid access violations.


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ADCDataFile, StdCtrls, RangeEdit, ExtCtrls, math, labiounit,
  ValidatedEdit, UITYpes, MATFileWRiterUnit ;

type
  TExportROITimeCourseFrm = class(TForm)
    GroupBox8: TGroupBox;
    rbAllRecords: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    GroupBox2: TGroupBox;
    rbABF: TRadioButton;
    rbASCII: TRadioButton;
    rbEDR: TRadioButton;
    rbMAT: TRadioButton;
    bOK: TButton;
    bCancel: TButton;
    ExportFile: TADCDataFile;
    FluorGrp: TGroupBox;
    RatioGrp: TGroupBox;
    cbDenominator: TComboBox;
    cbNumerator: TComboBox;
    rbExportROI: TRadioButton;
    rbExportRatio: TRadioButton;
    SaveDialog: TSaveDialog;
    cbFrameType: TComboBox;
    Shape2: TShape;
    rbIBW: TRadioButton;
    edExclusionThreshold: TValidatedEdit;
    Label5: TLabel;
    ROIGrp: TGroupBox;
    rbAllROIs: TRadioButton;
    RadioButton2: TRadioButton;
    cbROI: TComboBox;
    Label2: TLabel;
    cbSubROI: TComboBox;
    rbCFS: TRadioButton;
    FilesToExportGrp: TGroupBox;
    bSelectFilesToExport: TButton;
    meFiles: TMemo;
    OpenDialog: TOpenDialog;
    bClearList: TButton;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bSelectFilesToExportClick(Sender: TObject);
    procedure bClearListClick(Sender: TObject);

  private
    { Private declarations }
    FileOnDisplay : string ;
    function AddFileNameTag( FileName : String ;
                             ROIRange : string
                             ) : String ;
    function SetExportFileExtension( FileName : String ) : String ;
    procedure ExportFiles ;
    procedure WriteToMATFile(
          FileName : string ;                     // Export file name
          yBuf : PBigSingleArray ;                // Data array
          nPoints : Integer ;                     // No. points in yBuf
          NumROIs : Integer ) ;
    procedure WriteToFile(
          FileName : string ;                     // Export file name
          ExportType : TADCDataFileType ;         // Export type
          yBuf : PBigSingleArray;                // Data array
          nPoints : Integer ;                     // No. points in yBuf
          NumROIs : Integer ;                     // No. ROIs in yBuf
          iROIStart : Integer                     // Starting ROI
          ) ;

  public
    { Public declarations }
  end;

var
  ExportROITimeCourseFrm: TExportROITimeCourseFrm;

implementation

uses Main, LogUnit , ViewPlotUnit, strutils, idrfile, ViewUnit ;

{$R *.dfm}

procedure TExportROITimeCourseFrm.FormShow(Sender: TObject);
// ------------------------------
// Initialise form when displayed
// ------------------------------
var
    i : Integer ;
begin

     // Update ROI list
     cbROI.Clear ;
     cbSubROI.Clear ;
     cbSubROI.Items.AddObject(' ',TObject(MainFrm.IDRFile.MaxROI+1)) ;
     for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then begin
        cbROI.Items.AddObject(format('ROI %d',[i]),TObject(i)) ;
        cbSubROI.Items.AddObject(format('ROI %d',[i]),TObject(i)) ;
        end ;
     cbROI.ItemIndex := 0 ;
     cbSubROI.ItemIndex := 0 ;

     // Set numerator and denominator wavelength lists
     cbFrameType.Clear ;
     for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do begin
          cbFrameType.Items.AddObject( MainFrm.IDRFile.FrameType[i],TObject(i)) ;
          end ;
     cbNumerator.Items.Assign( cbFrameType.Items ) ;
     cbDenominator.Items.Assign( cbFrameType.Items ) ;

     cbFrameType.ItemIndex := 0 ;
     cbNumerator.ItemIndex := Min(MainFrm.TimeCourseRatioNumerator,
                                  cbNumerator.Items.Count-1) ;
     cbDenominator.ItemIndex := Min(MainFrm.TimeCourseRatioDenominator,
                                    cbDenominator.Items.Count-1) ;

     { Set frames to be exported }
     edRange.LoLimit := 1 ;
     edRange.LoValue := edRange.LoLimit ;
     edRange.HiLimit := MainFrm.IDRFile.NumFrames ;
     edRange.HiValue := edRange.HiLimit ;

     // Keep record of currently open file
     FileOnDisplay := MainFrm.IDRFile.FileName ;

     // Add currently open file to export list
     meFiles.Clear ;
     meFiles.Lines.Add(MainFrm.IDRFile.FileName) ;

     end;


procedure TExportROITimeCourseFrm.bClearListClick(Sender: TObject);
// ---------------
// Clear file list
// ---------------
begin
    meFiles.Clear ;
    end;


procedure TExportROITimeCourseFrm.bOKClick(Sender: TObject);
// --------------------
// Export files in list
// --------------------
begin
    ExportFiles ;
    end;


procedure TExportROITimeCourseFrm.ExportFiles ;
// --------------------------------------------------------------------
// Copy selected ROI time courses of data files in list to export file
// --------------------------------------------------------------------
var
   StartAt,EndAt,ch,i,j,iTo,iFrom : Integer ;
   iROI,SubROI,SelFrameType,NumFrameType,DenFrameType : Integer ;
   NP,nPoints : Integer ;
   iFrame : Integer ;
   iList,iList0,iList1 : Integer ;
   yBuf : PBigSingleArray ;        // Y data array
   Done : Boolean ;
   ExportType : TADCDataFileType ;
   NumROIsExported : Integer ;
   ROIExportList : Array[0..cMaxROIs] of Integer ;
   NumROIs,MaxROIsPerFile : Integer ;
   y,yThreshold,yDenominator,yNumerator : Single ;
   ROIRange : String ;
   FileName : String ;
   iFile : Integer ;
   AllowWrite : Boolean ;
begin

     bOK.Enabled := False ;

     for iFile := 0 to meFiles.Lines.Count-1 do begin

         // Close existing file
         MainFrm.IDRFile.CloseFile ;

         // Open file to export
         MainFrm.IDRFile.OpenFile( meFiles.Lines[iFile]) ;
         if MainFrm.FormExists( 'ViewFrm' ) then ViewFrm.NewFile ;

         FileName := ChangeFileExt(MainFrm.IDRFile.FileName,'.xxx') ;

         if rbAllRecords.Checked then
            begin
            StartAt := 1 ;
            EndAt := MainFrm.IDRFile.NumFrames ;
            end
         else
            begin
            StartAt := Round(edRange.LoValue) ;
            EndAt := Round(edRange.HiValue) ;
            end ;

         // Frame range & subtraction ROI
         SubROI := Integer(cbSubROI.Items.Objects[cbSubROI.ItemIndex]) ;
         SelFrameType := Integer(cbFrameType.Items.Objects[cbFrameType.ItemIndex]) ;
         NumFrameType := Integer(cbNumerator.Items.Objects[cbNumerator.ItemIndex]) ;
         DenFrameType := Integer(cbDenominator.Items.Objects[cbDenominator.ItemIndex]) ;
         yThreshold := edExclusionThreshold.Value ;

         // Get ROIs to be exported
         if rbAllROIs.checked then
            begin
            // Export all ROIs
            NumROIsExported := 0 ;
            for i := 1 to MainFrm.IDRFile.MaxROI do
                if (MainFrm.IDRFile.ROI[i].InUse) and (i <> SubROI)then
                begin
                ROIExportList[NumROIsExported] := i ;
                Inc(NumROIsExported) ;
                end ;
            end
         else
           begin
           // Export selected ROI
           ROIExportList[0] := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
           NumROIsExported := 1 ;
           end ;

         if rbABF.Checked then
            begin
            ExportType := ftAxonABF ;
            MaxROIsPerFile := 16 ;
            end
         else if rbASCII.Checked then
            begin
            ExportType := ftASC ;
            MaxROIsPerFile := NumROIsExported ;
            end
         else if rbIBW.Checked then
            begin
            ExportType := ftIBW ;
            MaxROIsPerFile := 1 ;
            end
         else if rbCFS.Checked then
            begin
            ExportType := ftCFS ;
            MaxROIsPerFile := 16 ;
            end
         else
            begin
            ExportType := ftEDR ;
            MaxROIsPerFile := 16 ;
            end ;

         iList0 := 0 ;
         while iList0 < NumROIsExported do
            begin
            // Range of ROI's to be exported in this file
            iList1 := Min(iList0 + MaxROIsPerFile-1,NumROIsExported-1) ;
            NumROIs := iList1 - iList0 + 1 ;

            // Create ROI range file name tag
            if NumROIs > 1 then
               begin
               ROIRange := format('ROI%d-%d',[ROIExportList[iList0],ROIExportList[iList1]]) ;
               end
            else
               begin
               ROIRange := format('ROI%d',[ROIExportList[iList0]]) ;
               end ;

            if subROI < MainFrm.IDRFile.MaxROI then
               begin
               ROIRange := ROIRange + format('-ROI%d',[subROI]) ;
               end ;

            FileName := AddFileNameTag( FileName, ROIRange ) ;


            // Allocate buffers
            GetMem( yBuf, MainFrm.IDRFile.NumFrames*NumROIs*SizeOf(Double)) ;

            // Read ROIs into export buffer
            nPoints := 0 ;
            for iFrame := StartAt to EndAt do
                for iList := iList0 to iList1 do
                begin
                iROI := ROIExportList[iList] ;
                if rbExportROI.Checked then
                   begin
                   // Export fluorescence signal
                   y := ViewPlotFrm.ROIIntensity( iROI, iFrame, SelFrameType ) ;
                   if SubROI <= MainFrm.IDRFile.MaxROI then
                      y := y - ViewPlotFrm.ROIIntensity( SubROI, iFrame, SelFrameType ) ;
                   end
                else
                   begin
                   // Export ratio
                   yNumerator := ViewPlotFrm.ROIIntensity( iROI, iFrame, NumFrameType ) ;
                   if SubROI <= MainFrm.IDRFile.MaxROI then
                      yNumerator := yNumerator - ViewPlotFrm.ROIIntensity( SubROI,
                                                                           iFrame,
                                                                           NumFrameType ) ;

                    yDenominator := ViewPlotFrm.ROIIntensity( iROI, iFrame, DenFrameType ) ;
                    if SubROI <= MainFrm.IDRFile.MaxROI then
                       yDenominator := yDenominator - ViewPlotFrm.ROIIntensity( SubROI,
                                                                                iFrame,
                                                                                DenFrameType ) ;
                    // Numerator and denominator must exceed exclusion threshold
                    if (yNumerator >= yThreshold) and (yDenominator > yThreshold) then
                       begin
                       y := yNumerator / yDenominator ;
                       end
                    else y := 0.0 ;
                    end ;

                yBuf^[nPoints] := y ;
                Inc(nPoints) ;
                end ;

            // If destination file already exists, allow user to abort
            AllowWrite := True ;
            if FileExists( FileName ) then
               begin
               if MessageDlg( FileName + ' exists! Overwrite?!',
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then AllowWrite := False ;
               end ;
            if AllowWrite then
               begin
               if rbMat.Checked then WriteToMatFile( FileName,yBuf, nPoints,NumROIs )
               else WriteToFile( FileName, ExportType, yBuf, nPoints,NumROIs, ROIExportList[iList0]) ;
               // Report progress
               MainFrm.StatusBar.SimpleText := format(
                                            ' EXPORT: %d points exported to %s ',
                                            [EndAt-StartAt+1,FileName]) ;
               LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;
               end;

            FreeMem(yBuf) ;

            // Increment to next set of ROIs
            iList0 := iList0 + MaxROIsPerFile ;

            end ;
         end ;

     bOK.Enabled := True ;

     // Re-open file which was on display
     MainFrm.IDRFile.CloseFile ;
     MainFrm.IDRFile.OpenFile( FileOnDisplay ) ;
     if MainFrm.FormExists( 'ViewFrm' ) then ViewFrm.NewFile ;

     end ;

procedure TExportROITimeCourseFrm.WriteToMATFile(
          FileName : string ;
          yBuf : PBigSingleArray ;                // Data array
          nPoints : Integer ;                     // No. points in yBuf
          NumROIs : Integer ) ;                   // No. ROIs in yBuf
// ---------------------------------
// Write ROI time course to MAT file
// ---------------------------------

var
    NP,iTo,iFrom,iROI,i : Integer ;
    TMat : PBigDoubleArray ;        // Time data array
    YMat : PBigDoubleArray ;        // Time data array
    Writer : TMATFileWriter ;
begin

    // Open MAT file
    Writer := TMATFileWriter.Create();
    Writer.OpenMATFile( FileName ) ;
    Writer.WriteFileHeader;

    // *** Write to MAT file ***
    NP := nPoints div NumROIs ;
    GetMem( TMat, NP*SizeOf(Double) ) ;
    GetMem( YMat, nPoints*SizeOf(Double) ) ;

    // Copy to MAT buffers
    for iROI := 0 to NumROIs-1 do
        begin
        iTo := iROI*NP ;
        iFrom := iROI ;
        for i := 0 to NP-1 do
            begin
            TMat^[i] := i*MainFrm.IDRFile.FrameInterval ;
            YMat^[iTo] := Round(yBuf^[iFrom]) ;
            iTo := iTo + 1 ;
            iFrom := iFrom + NumROIs ;
            end;
        end ;

    // Write to MAT file
    Writer.WriteDoubleMatrixHeader('T',NP,1);
    Writer.WriteDoubleMatrixValues( TMat^,NP,1) ;
    Writer.WriteDoubleMatrixHeader('Y',NP,NumROIs);
    Writer.WriteDoubleMatrixValues( YMat^, NP,NumROIs) ;
    Writer.CloseMATFile;

    FreeMem(TMat) ;
    FreeMem(YMat) ;
    end;


procedure TExportROITimeCourseFrm.WriteToFile(
          FileName : string ;                     // Export file name
          ExportType : TADCDataFileType ;         // Export type
          yBuf : PBigSingleArray ;                // Data array
          nPoints : Integer ;                     // No. points in yBuf
          NumROIs : Integer ;                     // No. ROIs in yBuf
          iROIStart : Integer                    // Starting ROI
          ) ;
// ------------------------------
// Write ROI time course to file
// ------------------------------
var
   OutBuf : pSmallIntArray ;       // Integer data array
   i,ch : Integer ;
   YMax : single ;
begin

    // Create empty export data file
    ExportFile.CreateDataFile( FileName, ExportType ) ;
    // Set file parameters
    ExportFile.NumChannelsPerScan := NumROIs ;
    ExportFile.NumScansPerRecord := nPoints ;
    ExportFile.ScanInterval := MainFrm.IDRFile.FrameInterval ;
    ExportFile.IdentLine := MainFrm.IDRFile.Ident ;
    ExportFile.RecordNum := 1 ;
    ExportFile.ABFAcquisitionMode := ftGapFree ;
    ExportFile.MaxADCValue := 32767 ;
    ExportFile.MinADCValue := -ExportFile.MaxADCValue ;

    // Allocate buffer
    GetMem( OutBuf, MainFrm.IDRFile.NumFrames*NumROIs*SizeOf(SmallInt)) ;

    // Set upper limit of y data range
    if rbExportROI.Checked then YMax := MainFrm.IDRFile.GreyMax
    else
        begin
        // Determine upper limit of y range for ratio and Ca time courses
        YMax := 0.0 ;
        for i := 0 to nPoints-1 do if YMax < Abs(YBuf^[i]) then YMax := Abs(YBuf^[i]) ;
        YMax := YMax*1.5 ;
        end ;

   // Set export channel name, scaling and units
   for i := 0 to NumROIs-1 do
       begin
       ExportFile.ChannelOffset[i] := i ;
       ExportFile.ChannelADCVoltageRange[i] := 1.0 ;
       ExportFile.ChannelName[i] := format('ROI%d',[iROIStart+i]) ; ;
       ExportFile.ChannelUnits[i] := 'gsu' ;
       ExportFile.ChannelGain[i] := 1.0 ;
       ExportFile.ChannelScale[i] := YMax/ExportFile.MaxADCValue ;
       ExportFile.ChannelCalibrationFactor[i] := 1.0/(ExportFile.MaxADCValue*ExportFile.ChannelScale[i]) ;
       end ;

   // Scale to integer and copy to output buffer
   ch := 0 ;
   for i := 0 to nPoints-1 do
       begin
       OutBuf^[i] := Round(yBuf^[i]/ExportFile.ChannelScale[ch]) ;
       Inc(ch) ;
       if ch >= NumROIs then ch := 0 ;
       end ;

    // Write to export file
    ExportFile.SaveADCBuffer( 0, nPoints div NumROIs, OutBuf^ ) ;
    // Close export data file
    ExportFile.CloseDataFile ;

    FreeMem(OutBuf) ;

    end;


procedure TExportROITimeCourseFrm.bSelectFilesToExportClick(Sender: TObject);
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


function TExportROITimeCourseFrm.AddFileNameTag(
         FileName : String ;                      // Base file name
         ROIRange : string                        // ROI range
         ): String ;
// ---------------------------------------------------------
// Add frames/ROI tag information to end of export file name
// ---------------------------------------------------------
var
    FileTag : String ;
begin

     FileTag := '[' ;

     if rbAllRecords.Checked then begin
        FileTag := FileTag + format(' F1-%d',[Round(edRange.HiLimit)]) ;
        end
     else begin
        FileTag := FileTag + format(' F%d-%d',[Round(edRange.LoValue),Round(edRange.HiValue)]) ;
        end ;

     if rbExportROI.Checked then begin
        FileTag := FileTag + ' ' + cbFrameType.Text ;
        end
     else begin
        // Ratio
        FileTag := FileTag + ' ' + cbNumerator.Text + ' DIV ' + cbDenominator.Text ;
        end ;

     FileTag := FileTag + ' ' + ROIRange +']' ;

     FileName := ANSIReplaceText( FileName,'.xxx',FileTag+'.xxx') ;

     FileName := SetExportFileExtension( FileName ) ;

     Result := FileName ;

     end ;


function TExportROITimeCourseFrm.SetExportFileExtension(
         FileName : String
         ) : String ;
// ------------------------------------------------
// Set file extension for selected export file type
// ------------------------------------------------
begin
    if rbABF.Checked then FileName := ChangeFileExt( FileName, '.abf' ) ;
    if rbASCII.Checked then FileName := ChangeFileExt( FileName, '.txt' ) ;
    if rbIBW.Checked then FileName := ChangeFileExt( FileName, '.ibw' ) ;
    if rbCFS.Checked then FileName := ChangeFileExt( FileName, '.cfs' ) ;
    if rbEDR.Checked then begin
        FileName := ChangeFileExt( FileName, '.edr' ) ;
        if LowerCase(FileName) = LowerCase(MainFrm.IDRFile.FileName) then begin
           FileName := StringReplace( FileName,
                                            '.edr',
                                            '[export].edr',
                                            [rfIgnoreCase] ) ;
           end ;
        end ;
     if rbMAT.Checked then FileName := ChangeFileExt( FileName, '.mat' ) ;  // Added by NS 18 February 2009
     Result := FileName ;
     end ;

end.
