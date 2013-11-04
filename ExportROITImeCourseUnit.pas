unit ExportROITImeCourseUnit;
//
// Export ROI intensity time course
// --------------------------------
// 21-10-10 Time course for ROI#s > ROI1 no longer set to zero
// 22-7-11 Option to export all ROIs in a single operation added
// 27-7-11 CFS export added

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ADCDataFile, StdCtrls, RangeEdit, ExtCtrls, math, labiounit,
  ValidatedEdit ;

type
  TExportROITimeCourseFrm = class(TForm)
    GroupBox3: TGroupBox;
    edFileName: TEdit;
    bChangeName: TButton;
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
    procedure FormShow(Sender: TObject);
    procedure bChangeNameClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure rbABFClick(Sender: TObject);
    procedure rbASCIIClick(Sender: TObject);
    procedure rbEDRClick(Sender: TObject);
    procedure cbROIChange(Sender: TObject);
    procedure cbSubROIChange(Sender: TObject);
    procedure cbFrameTypeChange(Sender: TObject);
    procedure cbNumeratorChange(Sender: TObject);
    procedure cbDenominatorChange(Sender: TObject);
    procedure GroupBox2Click(Sender: TObject);
    procedure rbExportRatioClick(Sender: TObject);
    procedure rbAllROIsClick(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure rbExportROIClick(Sender: TObject);
    procedure rbRangeClick(Sender: TObject);
    procedure rbAllRecordsClick(Sender: TObject);
    procedure edRangeKeyPress(Sender: TObject; var Key: Char);

  private
    { Private declarations }
    ExportFileName : string ;
    function AddFileNameTag( FileName : String ) : String ;
    function SetExportFileExtension( FileName : String ) : String ;

  public
    { Public declarations }
  end;

var
  ExportROITimeCourseFrm: TExportROITimeCourseFrm;

implementation

uses Main, LogUnit , ViewPlotUnit, strutils, idrfile ;

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

     { Update O/P file name channel selection options }
     ExportFileName := ChangeFileExt(MainFrm.IDRFile.FileName,'.xxx') ;
     edFileName.Text := AddFileNameTag(ExportFileName) ;

     end;


procedure TExportROITimeCourseFrm.bChangeNameClick(Sender: TObject);
{ ------------------------------------------
  Change name/location of export destination
  ------------------------------------------ }
begin
     SaveDialog.FileName := SetExportFileExtension(ExportFileName) ;
     SaveDialog.DefaultExt := ExtractFileExt( SaveDialog.FileName ) ;
     SaveDialog.options := [ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.Filter := ' Files (*' + SaveDialog.DefaultExt + ')|*' +
                            SaveDialog.DefaultExt + '|' ;

     SaveDialog.Title := 'Export File ' ;
     if MainFrm.DataDirectory <> '' then
        SaveDialog.InitialDir := MainFrm.DataDirectory ;

     if SaveDialog.Execute then ExportFileName := SaveDialog.FileName ;

     ExportFileName := ChangeFileExt(ExportFileName,'.xxx') ;
     edFileName.Text := AddFileNameTag( ExportFileName ) ;

     end;


procedure TExportROITimeCourseFrm.bOKClick(Sender: TObject);
// -------------------------------------------------
// Copy selected section of data file to export file
// -------------------------------------------------
var
   StartAt,EndAt,ch,i,j : Integer ;
   iROI,SubROI,SelFrameType,NumFrameType,DenFrameType : Integer ;
   nPoints : Integer ;
   iFrame : Integer ;
   iList,iList0,iList1 : Integer ;
   OutBuf : pSmallIntArray ;
   yBuf : PBigSingleArray ;
   Done : Boolean ;
   ExportType : TADCDataFileType ;
   NumROIsExported : Integer ;
   ROIExportList : Array[0..cMaxROIs] of Integer ;
   NumROIsInFile,MaxROIsPerFile : Integer ;
   y,yThreshold,yDenominator,yNumerator,yMax : Single ;
   ROIRange : String ;
   FileName : String ;
begin

     bOK.Enabled := False ;

     if rbAllRecords.Checked then begin
        StartAt := 1 ;
        EndAt := MainFrm.IDRFile.NumFrames ;
        end
     else begin
        StartAt := Round(edRange.LoValue) ;
        EndAt := Round(edRange.HiValue) ;
        end ;


     // Export file type

     // Check MAT-File export option
     // Added by NS 18 February 2009
     if rbMAT.Checked then
     begin
      //MessageDlg('Exporting to MAT-File format not yet implemented.', mtWarning, [mbOK], 0);
      //ExportMATFile(ExportFileName, StartAt, EndAt, UseChannel);
      bOK.Enabled := True;
      Exit;
     end;

     // Frame range & subtraction ROI
     SubROI := Integer(cbSubROI.Items.Objects[cbSubROI.ItemIndex]) ;
     SelFrameType := Integer(cbFrameType.Items.Objects[cbFrameType.ItemIndex]) ;
     NumFrameType := Integer(cbNumerator.Items.Objects[cbNumerator.ItemIndex]) ;
     DenFrameType := Integer(cbDenominator.Items.Objects[cbDenominator.ItemIndex]) ;
     yThreshold := edExclusionThreshold.Value ;

     // Get ROIs to be exported
     if rbAllROIs.checked then begin
        // Export all ROIs
        NumROIsExported := 0 ;
        for i := 1 to MainFrm.IDRFile.MaxROI do
            if (MainFrm.IDRFile.ROI[i].InUse) and (i <> SubROI) then begin
            ROIExportList[NumROIsExported] := i ;
            Inc(NumROIsExported) ;
            end ;
        end
     else begin
        // Export selected ROI
        ROIExportList[0] := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
        NumROIsExported := 1 ;
        end ;

     if rbABF.Checked then begin
        ExportType := ftAxonABF ;
        MaxROIsPerFile := 16 ;
        end
     else if rbASCII.Checked then begin
        ExportType := ftASC ;
        MaxROIsPerFile := NumROIsExported ;
        end
     else if rbIBW.Checked then begin
        ExportType := ftIBW ;
        MaxROIsPerFile := 1 ;
        end
     else if rbCFS.Checked then begin
        ExportType := ftCFS ;
        MaxROIsPerFile := 16 ;
        end
     else begin
        ExportType := ftEDR ;
        MaxROIsPerFile := 16 ;
        end ;

     iList0 := 0 ;
     while iList0 < NumROIsExported do begin

         // Range og ROI's to be exported in this file
         iList1 := Min(iList0 + MaxROIsPerFile-1,NumROIsExported-1) ;
         NumROIsInFile := iList1 - iList0 + 1 ;

         // Create ROI range file name tag
         if NumROIsInFile > 1 then begin
            ROIRange := format('ROI%d-%d',[ROIExportList[iList0],ROIExportList[iList1]]) ;
            end
         else begin
            ROIRange := format('ROI%d',[ROIExportList[iList0]]) ;
            end ;

         if subROI < MainFrm.IDRFile.MaxROI then begin
            ROIRange := ROIRange + format('-ROI%d',[subROI]) ;
            end ;
            
         FileName := AddFileNameTag( ExportFileName ) ;
         FileName := ANSIReplaceText( FileName,'ROI?',ROIRange ) ;

         // If destination file already exists, allow user to abort
         if FileExists( FileName ) then begin
            if MessageDlg( FileName + ' exists! Overwrite?!',
               mtConfirmation, [mbYes, mbNo], 0) <> mrYes then begin
               bOK.Enabled := True ;
               Exit ;
               end ;
            end ;

         // Create empty export data file
         ExportFile.CreateDataFile( FileName, ExportType ) ;

         // Set file parameters

         ExportFile.NumChannelsPerScan := NumROIsInFile ;
         ExportFile.NumScansPerRecord := EndAt - StartAt + 1 ;
         ExportFile.ScanInterval := MainFrm.IDRFile.FrameInterval ;
         ExportFile.IdentLine := MainFrm.IDRFile.Ident ;
         ExportFile.RecordNum := 1 ;
         ExportFile.ABFAcquisitionMode := ftGapFree ;
         ExportFile.MaxADCValue := Min(MainFrm.IDRFile.GreyMax,32767) ;
         ExportFile.MinADCValue := -MainFrm.IDRFile.GreyMax - 1 ;

         // Allocate buffers
         GetMem( OutBuf, MainFrm.IDRFile.NumFrames*NumROIsInFile*SizeOf(SmallInt)) ;
         GetMem( yBuf, MainFrm.IDRFile.NumFrames*NumROIsInFile*SizeOf(Single)) ;

         // Read ROIs into export buffer

         nPoints := 0 ;
         for iFrame := StartAt to EndAt do
            for iList := iList0 to iList1 do begin
            iROI := ROIExportList[iList] ;
            if rbExportROI.Checked then begin
               // Export fluorescence signal
               y := ViewPlotFrm.ROIIntensity( iROI, iFrame, SelFrameType ) ;
               if SubROI <= MainFrm.IDRFile.MaxROI then
                  y := y - ViewPlotFrm.ROIIntensity( SubROI, iFrame, SelFrameType ) ;
               end
            else begin
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
                if (yNumerator >= yThreshold) and (yDenominator > yThreshold) then begin
                   y := yNumerator / yDenominator ;
                   end
                else y := 0.0 ;
                end ;

            yBuf^[nPoints] := y ;
            Inc(nPoints) ;
            end ;

         if rbExportROI.Checked then begin
            // ROI intensity at single frame type
            YMax := MainFrm.IDRFile.GreyMax ;
            end
         else begin
            YMax := 0.0 ;
            for i := 0 to nPoints-1 do if YMax < Abs(YBuf^[i]) then YMax := Abs(YBuf^[i]) ;
            YMax := YMax*1.5 ;
            end ;

         // Set export channel name, scaling and units
         for i := 0 to NumROIsInFile-1 do begin
             ExportFile.ChannelOffset[i] := i ;
             ExportFile.ChannelADCVoltageRange[i] := 1.0 ;
             ExportFile.ChannelName[i] := format('ROI%d',[ROIExportList[iList0+i]]) ; ;
             ExportFile.ChannelUnits[i] := '' ;
             ExportFile.ChannelGain[i] := 1.0 ;
             ExportFile.ChannelScale[i] := YMax/ExportFile.MaxADCValue ;
             ExportFile.ChannelCalibrationFactor[i] := 1.0/(ExportFile.MaxADCValue*ExportFile.ChannelScale[i]) ;
             end ;

         // Scale to integer and copy to output buffer
         ch := 0 ;
         for i := 0 to nPoints-1 do begin
             OutBuf^[i] := Round(yBuf^[i]/ExportFile.ChannelScale[ch]) ;
             Inc(ch) ;
             if ch >= NumROIsInFile then ch := 0 ;
             end ;

         // Write to export file
         ExportFile.SaveADCBuffer( 0, nPoints div NumROIsInFile, OutBuf^ ) ;

         // Close export data file
         ExportFile.CloseDataFile ;

         // Final Report
         MainFrm.StatusBar.SimpleText := format(
         ' EXPORT: %d scans exported to %s ',
         [EndAt-StartAt+1,FileName]) ;
         LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;

         FreeMem(OutBuf) ;
         FreeMem(yBuf) ;

         // Increment to next set of ROIs
         iList0 := iList0 + MaxROIsPerFile ;

         end ;


     bOK.Enabled := True ;

     end ;


function TExportROITimeCourseFrm.AddFileNameTag(
         FileName : String
         ): String ;
// ---------------------------------------------------------
// Add frames/ROI tag information to end of export file name
// ---------------------------------------------------------
var
    FileTag : String ;
    i,iFirst,iLast : Integer ;
    ROIRange : String ;
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

     FileTag := FileTag + ' ROI?]' ;

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
    if rbIBW.Checked then FileName := ChangeFileExt( FileName, '.cfs' ) ;
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


procedure TExportROITimeCourseFrm.rbABFClick(Sender: TObject);
begin
    edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;

procedure TExportROITimeCourseFrm.rbASCIIClick(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
  end;

procedure TExportROITimeCourseFrm.rbEDRClick(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
  end;

procedure TExportROITimeCourseFrm.cbROIChange(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;

procedure TExportROITimeCourseFrm.cbSubROIChange(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;


procedure TExportROITimeCourseFrm.cbFrameTypeChange(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;


procedure TExportROITimeCourseFrm.cbNumeratorChange(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;


procedure TExportROITimeCourseFrm.cbDenominatorChange(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;


procedure TExportROITimeCourseFrm.GroupBox2Click(Sender: TObject);
// ---------------------------
// Select Export ROI Intensity
// ---------------------------
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;

procedure TExportROITimeCourseFrm.rbExportRatioClick(Sender: TObject);
// ---------------------------------
// Select Export ROI Intensity ratio
// ---------------------------------
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;

procedure TExportROITimeCourseFrm.rbAllROIsClick(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;

procedure TExportROITimeCourseFrm.RadioButton2Click(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;

procedure TExportROITimeCourseFrm.rbExportROIClick(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;

procedure TExportROITimeCourseFrm.rbRangeClick(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;

procedure TExportROITimeCourseFrm.rbAllRecordsClick(Sender: TObject);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;

procedure TExportROITimeCourseFrm.edRangeKeyPress(Sender: TObject;
  var Key: Char);
begin
  edFileName.Text := AddFileNameTag( ExportFileName ) ;
    end;

end.
