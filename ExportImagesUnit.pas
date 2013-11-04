unit ExportImagesUnit;
// --------------------------------------------
// Export images to other image data file types
// --------------------------------------------
// 23.03.05
// 05.04.05 Frames can now be skipped
// 26.06.05 ... Disk space now checked before file is written
// 11.10.05 ... Multi-frame files now exported correctly
// 06.04.06 ... Panels replaced with simpletext
// 16.01
// 21.02.07 ... Updated to support 9 frame types
// 02.03.07 ... Exports all ROIs and all frames types to separate files in a single operation
// 15.03.07 ... Wavelengths from spectral data files can now be exported
//              Only single frame types or all frame types can now be exported
// 30.03.07 ... Export of individual frames now works correctly
// 09.05.08 ... Dual-rate, multiwavelength support added
// 19.05.09 ... '/' replaced in export file names with '-' to avoid
//              "Unable to create" errors when exporting ratio image files

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, RangeEdit, ImageFile, ExtCtrls, ValidatedEdit, IDRFile, strutils, math ;

type
  TExportImagesFrm = class(TForm)
    GroupBox1: TGroupBox;
    edFileName: TEdit;
    bChangeName: TButton;
    GroupBox2: TGroupBox;
    rbAllFrames: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    FrameTypeGrp: TGroupBox;
    GroupBox3: TGroupBox;
    rbBioRad: TRadioButton;
    rbMetaMorph: TRadioButton;
    rbTIFF: TRadioButton;
    bOK: TButton;
    bCancel: TButton;
    ImageFile: TImageFile;
    SaveDialog: TSaveDialog;
    Label1: TLabel;
    edSkip: TValidatedEdit;
    Shape1: TShape;
    GroupBox4: TGroupBox;
    rbWholeImage: TRadioButton;
    rbROI: TRadioButton;
    cbROINum: TComboBox;
    rbAllROIs: TRadioButton;
    rbAllFrameTypes: TRadioButton;
    GroupBox5: TGroupBox;
    rbInterleaved: TRadioButton;
    rbSeparate: TRadioButton;
    rbSingleFrameType: TRadioButton;
    cbFrameType: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure rbBioRadClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bChangeNameClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure edRangeKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    ExportFileExtension : String ;
    function UpdateFileExtension( FileName : String ) : String ;

    procedure ExportFile ;
  public
    { Public declarations }
  end;

var
  ExportImagesFrm: TExportImagesFrm;

implementation

uses Main, LogUnit;

{$R *.dfm}

procedure TExportImagesFrm.FormShow(Sender: TObject);
// -----------------------------------
// Initialisations when form displayed
// -----------------------------------
var
    i : Integer ;
begin

     // Frame range
     edRange.LoLimit := 1 ;
     edRange.HiLimit := MainFrm.IDRFile.NumFrames ;
     edRange.LoValue := edRange.LoLimit ;
     edRange.HiValue := edRange.HiLimit ;

     // Set regions of interest list
     cbROINum.Clear ;
     for i := 1 to MainFrm.IDRFile.MaxROI do if MainFrm.IDRFile.ROI[i].InUse then begin
        cbROINum.Items.AddObject( format('ROI.%d',[i]),TObject(i)) ;
        end ;
    if cbROINum.Items.Count > 0 then cbROINum.ItemIndex := 0 ;

    // Frame types
    rbAllFrameTypes.Caption := 'All wavelengths' ;
    rbSingleFrameType.Caption := 'Single wavelength' ;
    cbFrameType.Clear ;
    for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do begin
        cbFrameType.Items.Add( MainFrm.IDRFile.FrameType[i] ) ;
        end ;
    if cbFrameType.Items.Count > 0 then cbFrameType.ItemIndex := 0 ;

     // Export file name
     edFileName.Text := UpdateFileExtension(MainFrm.IDRFile.FileName) ;
     bOK.Enabled := True ;

     end;


function TExportImagesFrm.UpdateFileExtension(
         FileName : String ) : String ;
// -----------------------
// Update export file extension
// -----------------------
begin

     // Export format
     if rbBioRad.Checked then ExportFileExtension := '.PIC'
     else if rbMetaMorph.Checked then ExportFileExtension := '.STK'
     else ExportFileExtension := '.TIF' ;

     Result := ChangeFileExt( FileName, ExportFileExtension ) ;

     end ;


procedure TExportImagesFrm.ExportFile ;
// --------------------------------------------
// Export images to another image file format
// --------------------------------------------
var
    FrameNum : Integer ; // Frame counter
    PFrameBuf : PIntArray ; // Image frame buffer pointer
    PixelDepth : Integer ;
    FrameWidth : Integer ;  // Export frame width (pixels)
    FrameHeight : Integer ; // Export frame height (pixels)
    ROINum : Integer ;      // ROI number in use
    i,x,y : Integer ;
    iStartFrame : Integer ;
    iEndFrame : Integer ;
    iStep : Integer ;
    NumFramesExported : Integer ;
    FrameType,NumFrameTypeFiles : Integer ;
    s : String ;
    ROIList : Array[0..cMaxROIs+MaxFrameType+1] of Integer ;
    FrameTypeList : Array[0..MaxFrameType+1] of String ;
    FrameTypeListNum : Array[0..MaxFrameType+1] of Integer ;
    iROI,NumROIFiles : Integer ;
    iFT : Integer ;
    FileName : String ;
begin

     // Range of frames to be exported
     if rbAllFrames.Checked then begin
        iStartFrame := 1 ;
        iEndFrame := MainFrm.IDRFile.NumFrames ;
        end
     else begin
        iStartFrame := Round(edRange.LoValue) ;
        // Ensure frame range starts on frame type 0
        iStartFrame := (((iStartFrame -1) div MainFrm.IDRFile.FrameTypeCycleLength)*
                       MainFrm.IDRFile.FrameTypeCycleLength) + 1 ;
        iEndFrame := Round(edRange.HiValue) ;
        iEndFrame := (((iEndFrame -1) div MainFrm.IDRFile.FrameTypeCycleLength)*
                     MainFrm.IDRFile.FrameTypeCycleLength)
                     + MainFrm.IDRFile.FrameTypeCycleLength ;
        end ;

     // Frame step interval
     iStep := (Round(edSkip.Value) + 1) ;

     // Determine if sufficient disk space available
 //    NumFramesExported := 0 ;
 //    for i := 0 to NumFrameTypes-1 do if UseFrameType(i) then begin
 //        NumFramesExported := NumFramesExported + ((iEndFrame - iStartFrame +1) div iStep ) ;
 //        end ;

//     Disabled because not working reliably 24/9/6
//     if not MainFrm.IDRFile.DiskSpaceAvailable( NumFramesExported ) then begin
//        ShowMessage( 'Insufficient disk space!' ) ;
//        Exit ;
//        end ;

     // Create export file

     // Get width of output image frame
     if rbWholeImage.Checked then begin
        // Whole image
        ROIList[0] := -1 ;
        NumROIFiles := 1 ;
        end
     else if rbAllROIs.Checked then begin
        // All ROIs
        NumROIFiles := 0 ;
        for i := 1 to MainFrm.IDRFile.MaxROI do
            if MainFrm.IDRFile.ROI[i].InUse then begin
            ROIList[NumROIFiles] := i ;
            Inc(NumROIFiles) ;
            end ;
        end
     else begin
        // Selected ROI
        ROIList[0] := Integer(cbROINum.Items.Objects[cbROINum.ItemIndex]) ;
        NumROIFiles := 1 ;
        end ;

     // Number of frame type files to be output
     // (1 when frames types interleaved, 1 per frame type when separate file)
     if rbInterleaved.Checked then begin
        // All frame types (interleaved in single file)
        FrameTypeList[0] := '' ;
        for i:= 0 to cbFrameType.Items.Count-1 do begin
           if i <> 0 then FrameTypeList[0] := FrameTypeList[0] + '-' ;
           FrameTypeList[0] := FrameTypeList[0] + cbFrameType.Items[i] ;
           FrameTypeList[0] := ANSIReplaceText(FrameTypeList[0],' nm','') ;
           FrameTypeList[0] := ANSIReplaceText(FrameTypeList[0],' ','') ;
           end ;
        FrameTypeListNum[0] := 0 ;
        NumFrameTypeFiles := 1 ;
        end
     else if rbSingleFrameType.Checked then begin
        // Single frame type
        FrameTypeList[0] := cbFrameType.Text ;
        FrameTypeList[0] := ANSIReplaceText(FrameTypeList[0],' nm','') ;
        FrameTypeList[0] := ANSIReplaceText(FrameTypeList[0],' ','') ;
        FrameTypeListNum[0] := cbFrameType.ItemIndex ;
        NumFrameTypeFiles := 1 ;
        end
     else begin
       // All frame types (separate files)
       for i:= 0 to cbFrameType.Items.Count-1 do begin
           FrameTypeList[i] := cbFrameType.Items[i] ;
           FrameTypeListNum[i] := i ;
           FrameTypeList[i] := ANSIReplaceText(FrameTypeList[i],' nm','') ;
           FrameTypeList[i] := ANSIReplaceText(FrameTypeList[i],' ','') ;
           end ;
       NumFrameTypeFiles := cbFrameType.Items.Count ;
       end ;

     // Allocate frame buffer
     GetMem( PFrameBuf, MainFrm.IDRFile.NumPixelsPerFrame*4 ) ;

     // Copy frames to file
     bOK.Enabled := False ;

     for iROI := 0 to NumROIFiles-1 do begin

         for iFT := 0 to NumFrameTypeFiles-1 do begin

             if bOK.Enabled then Break ;

             // ROI to be output
             ROINum := ROIList[iROI] ;

             // Get size of image
             if ROINum < 0 then begin
                // Whole image
                FrameWidth := MainFrm.IDRFile.FrameWidth ;
                FrameHeight := MainFrm.IDRFile.FrameHeight ;
                end
             else begin
                // Region of interest
                FrameWidth := MainFrm.IDRFile.ROI[ROINum].BottomRight.X -
                              MainFrm.IDRFile.ROI[ROINum].TopLeft.X + 1 ;
                FrameHeight := MainFrm.IDRFile.ROI[ROINum].BottomRight.Y -
                               MainFrm.IDRFile.ROI[ROINum].TopLeft.Y + 1 ;
                end ;

             s := format('[%d-%d_',[iStartFrame,iEndFrame]) ;

             // Frame types used
             s := s + FrameTypeList[iFT] ;

             if not rbWholeImage.Checked then s := s + format('_ROI%d',[ROINum]) ;

             // Add to end of export file name
             for i := 1 to 2 do s := ANSIReplaceText( s, '/', '-' ) ;
             s := s + ']' + ExportFileExtension ;
             FileName := ANSIReplaceText( edFileName.text, ExportFileExtension, s ) ;

             // Let user cancel if file already exists
             if FileExists( FileName ) then begin
             if MessageDlg( format(
                'File %s already exists! Do you want to overwrite it? ',[edFileName.Text]),
                mtWarning,[mbYes,mbNo], 0 ) = mrNo then Break ;
                end ;

             // Create file
             if not ImageFile.CreateFile( FileName,
                                          FrameWidth,
                                          FrameHeight,
                                          MainFrm.IDRFile.NumBytesPerPixel*8,
                                          1,
                                          False ) then begin
                MainFrm.StatusBar.SimpleText := 'Unable to create : ' + FileName ;
                Break ;
                end ;

             FrameNum := iStartFrame ;
             NumFramesExported := 0 ;
             While FrameNum <= iEndFrame do begin

                 // Export selected frames
                 if rbInterleaved.Checked or (MainFrm.IDRFile.TypeOfFrame(FrameNum) = FrameTypeListNum[iFT]) then begin

                    // Load frame
                    if not MainFrm.IDRFile.LoadFrame32(FrameNum,PFrameBuf) then break ;

                    // If region of interest selected, extract it
                    if ROINum >= 0 then begin
                       i := 0 ;
                       for y := MainFrm.IDRFile.ROI[ROINum].TopLeft.Y to
                           MainFrm.IDRFile.ROI[ROINum].BottomRight.Y do
                           for x := MainFrm.IDRFile.ROI[ROINum].TopLeft.X to
                               MainFrm.IDRFile.ROI[ROINum].BottomRight.X do begin
                               PFrameBuf[i] := PFrameBuf[y*MainFrm.IDRFile.FrameWidth + x] ;
                               Inc(i) ;
                               end ;
                       end ;

                    // Save frame
                    Inc(NumFramesExported) ;
                    ImageFile.SaveFrame32( NumFramesExported, PFrameBuf ) ;

                    MainFrm.StatusBar.SimpleText := format(
                    'Exporting %d/%d frames (%d) to %s',
                    [FrameNum,iEndFrame,NumFramesExported,FileName]) ;
                    Application.ProcessMessages ;

                    if bOK.Enabled then Break ;

                    end ;

                 FrameNum := FrameNum + iStep ;

                 end ;

             // Close file
             ImageFile.CloseFile ;

             // Report
             s := format( 'Export: %d frames %d-%d exported to %s',
                         [NumFramesExported,iStartFrame,iEndFrame,FileName]) ;
             MainFrm.StatusBar.SimpleText := s ;
             LogFrm.AddLine(s) ;

             end ;
         end ;

    FreeMem( PFrameBuf ) ;

    bOK.Enabled := True ;

    end ;


procedure TExportImagesFrm.rbBioRadClick(Sender: TObject);
// --------------------------
// Export file format changed
// --------------------------
begin
     edFileName.Text := UpdateFileExtension(edFileName.Text) ;
     end;


procedure TExportImagesFrm.bOKClick(Sender: TObject);
// --------------------
// Export image to file
// --------------------
begin
     ExportFile ;
     end;


procedure TExportImagesFrm.bChangeNameClick(Sender: TObject);
// -----------------------------------
// Change name / folder of export file
// -----------------------------------
begin

     SaveDialog.InitialDir := ExtractFilePath( edFileName.Text ) ;
     SaveDialog.Title := 'Export File ' ;
     SaveDialog.options := [ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.DefaultExt := ExtractFileExt( edFileName.Text ) ;
     SaveDialog.Filter := ' BioRad PIC (*.pic)|*.pic|' +
                          ' MetaMorph STK (*.stk)|*.stk|' +
                          ' TIFF (*.tif)|*.tif' ;

     if rbBioRad.Checked then  SaveDialog.FilterIndex := 1
     else if rbMetaMorph.Checked then  SaveDialog.FilterIndex := 2
     else SaveDialog.FilterIndex := 3 ;

     SaveDialog.FileName := '' ;
     if SaveDialog.Execute then begin
        edFileName.Text := UpdateFileExtension(SaveDialog.FileName) ;
        end ;

     end ;

procedure TExportImagesFrm.bCancelClick(Sender: TObject);
begin
     bOK.Enabled := True ;
     end ;

procedure TExportImagesFrm.edRangeKeyPress(Sender: TObject; var Key: Char);
begin
    If Key = #13 then begin
        // Ensure frame range starts on frame type 0
        edRange.LoValue := (((Round(edRange.LoValue) -1) div MainFrm.IDRFile.FrameTypeCycleLength)*
                           MainFrm.IDRFile.FrameTypeCycleLength) + 1 ;
        edRange.HiValue := (((Round(edRange.HiValue) -1) div MainFrm.IDRFile.FrameTypeCycleLength)*
                           MainFrm.IDRFile.FrameTypeCycleLength)
                           + MainFrm.IDRFile.FrameTypeCycleLength ;
        end ;
    end;


end.


