unit ViewPlotThread;
// ========================================================================
// Compute fluorescence time course data buffer from image buffers and ROIs
// ========================================================================
// 22.03.21

interface

uses
  System.Classes;

type
  TViewPlotThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

implementation

uses Main,ViewPlotUnit,math,labiounit, LightSourceUnit,excsetupunit,Windows, Messages, SysUtils,IDRFile,StrUtils ;


procedure TViewPlotThread.Execute;
const
    EmptyFlag = -65536.0 ;
var
    i,j,iROI,TDone,iFrameType,iFrame,NumFrames,NumFrameTypes,np  : Integer ;
    Done,ROIsAvailable : Boolean ;
    LatestValue : Array[0..MaxLightSourceCycleLength+1] of Single ;
    FileHandle : THandle ;
    yBuf : PSingleArray ;    // Time course data buffer
    ImageBuf : PIntArray ;   // Image buffer
    ROITCSpacing : Integer ; // Time point spacing
    yBufSize : Cardinal ;    // No. bytes in time course data buffer
    s,FileName : string ;
    yTable : TStringList ;
    dt : single ;
begin

    if MainFrm.IDRFile.SpectralDataFile then NumFrameTypes := 1
                                        else NumFrameTypes := MainFrm.IDRFile.NumFrameTypes ;
    NumFrames :=  MainFrm.IDRFile.NumFrames ;
    ROITCSpacing := NumFrameTypes*NumFrames ;

    // Exit if no ROIs available
    ROIsAvailable := False ;
    for iROI := 1 to MainFrm.IDRFile.MaxROIInUse do
        ROIsAvailable := ROIsAvailable or MainFrm.IDRFile.ROI[iROI].InUse ;
    if not ROIsAvailable then Exit ;

    Synchronize( procedure
                 begin
                 ViewPlotFrm.CompDone := False ;
                 end);

    // Initialise empty buffer
    np := MainFrm.IDRFile.NumFrames*MainFrm.IDRFile.NumFrameTypes*Max(MainFrm.IDRFile.MaxROIInUse+1,1) ;
    yBufSize := np*SizeOf(Single) ;
    yBuf := AllocMem( yBufSize );
    for i := 0 to np-1 do yBuf^[i] := EmptyFlag ;
    ImageBuf := AllocMem( MainFrm.IDRFile.NumPixelsPerFrame*SizeOf(Integer)) ;

    // Create time course for each frame type
    iFrame := 1 ;
    while (not Terminated) and (iFrame <= MainFrm.IDRFile.NumFrames) do
        begin

        // Load image from file
        if not MainFrm.IDRFile.LoadFrame32( iFrame, ImageBuf ) then begin
           Break ;
           end ;

        // Frame type for this frame
        iFrameType := MainFrm.IDRFile.TypeOfFrame(iFrame) ;

        // Compute mean intensity
        for iROI := 1 to MainFrm.IDRFile.MaxROIInUse do
            if MainFrm.IDRFile.ROI[iROI].InUse then begin

            // Save in buffer
            j := (ROITCSpacing*(iROI-1)) + ((iFrame-1)*NumFrameTypes) + iFrameType ;
            yBuf^[j] := ViewPlotFrm.MeanROIIntensity( iROI, ImageBuf )/ MainFrm.IDRFile.IntensityScale ;

            end ;

        if ((iFrame mod 50) = 0) or (iFrame >= MainFrm.IDRFile.NumFrames) then
           begin
          // Report progress
          Synchronize( procedure
                     begin
                     MainFrm.StatusBar.SimpleText := format(
                                    'ROI: Computing Intensity time course %d/%d',
                                    [iFrame,MainFrm.IDRFile.NumFrames]) ;
                     end );

            Sleep(1) ;
            end ;

        Inc(iFrame) ;

        end ;

     // Time course complete : Fill in remaining time points with most recent values

     for iROI := 1 to MainFrm.IDRFile.MaxROIInUse do
         if MainFrm.IDRFile.ROI[iROI].InUse then
            begin

            for iFrameType := 0 to NumFrameTypes-1 do
                begin
                j := (ROITCSpacing*(iROI-1)) + (iFrameType*NumFrameTypes) + iFrameType ;
                LatestValue[iFrameType] := yBuf^[j] ;
                end ;

            // Update remaining empty entries with latest available frame
           for iFrameType := 0 to NumFrameTypes-1 do
               begin
               for iFrame := 0 to NumFrames-1 do
                   begin
                   j := (ROITCSpacing*(iROI-1)) + (iFrame*NumFrameTypes) + iFrameType ;
                   if yBuf^[j] <> EmptyFlag then LatestValue[iFrameType] := yBuf^[j]
                                            else yBuf^[j] := LatestValue[iFrameType] ;
                   end ;
               end ;

           // Set first set of points equal to second
           for i := 0 to NumFrameTypes-1 do
               begin
               j := ROITCSpacing*(iROI-1) + i ;
               yBuf[j] := yBuf[j+NumFrameTypes] ;
               end ;

           end ;

     // Save plot to .TCB storage file
     FileHandle := FileCreate( ChangeFileExt(Mainfrm.IDRFile.FileName,'.TCB'));
     if NativeInt(FileHandle) <> -1 then
        begin
        FileWrite( FileHandle,yBuf^,yBufSize);
        FileClose( FileHandle) ;
        end;

     // Save plot to .csv file

     // Column labels
     s := '"Time",' ;
     for iROI := 1 to MainFrm.IDRFile.MaxROIInUse do
         if MainFrm.IDRFile.ROI[iROI].InUse then s := s + format('"ROI.%d",',[iROI]) ;
     s := LeftStr(s,Length(s)-1);

     // Create string list
     YTable := TStringList.Create ;
     YTable.Add(s) ;

     // Add time + rows of ROI data
     dt := MainFrm.IDRFile.FrameInterval*NumFrameTypes ;
     for iFrame := 0 to NumFrames-1 do
         begin
         s := format('"%.6g",',[iFrame*dt]);
         for iROI := 1 to MainFrm.IDRFile.MaxROIInUse do
             if MainFrm.IDRFile.ROI[iROI].InUse then
             begin
             j := (ROITCSpacing*(iROI-1)) + (iFrame*NumFrameTypes) + iFrameType ;
             s := s + format('"%.6g",',[YBuf[j]]) ;
             end ;
         YTable.Add(s) ;
         end ;
    FileName := ChangeFileExt(Mainfrm.IDRFile.FileName,'.csv');
    FileName := ReplaceText( FileName, '.csv', '.roi.time.course.csv') ;
    YTable.SaveToFile( FileName ) ;
    YTable.Free ;

    FreeMem( yBuf ) ;
    FreeMem( ImageBuf ) ;

    Synchronize( procedure
                 begin
                 ViewPlotFrm.CompDone := True ;
                 end);
end;

end.
