unit FilePropsUnit;
// -----------------------------------------
// WinFluor - File Properties Viewer/Editor
// -----------------------------------------
// 21-6-05 .. Started
// 02-03-06 No. bytes in file header added to properties
// 13-07-10 File header text added to properties
// 30.01.13 Z stack properties added

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ComCtrls, ValidatedEdit, ExtCtrls;

type
  TFilePropsFrm = class(TForm)
    Page: TPageControl;
    PropTab: TTabSheet;
    ADCTab: TTabSheet;
    MarkerTab: TTabSheet;
    ChannelsGrp: TGroupBox;
    ChannelTable: TStringGrid;
    meProperties: TMemo;
    GroupBox1: TGroupBox;
    panADCScanInterval: TPanel;
    edADCScanInterval: TValidatedEdit;
    bUpdateChannelProps: TButton;
    EditablePropsGrp: TGroupBox;
    panInterval: TPanel;
    edInterval: TValidatedEdit;
    Panel2: TPanel;
    edPixelSize: TValidatedEdit;
    Panel3: TPanel;
    edPixelUnits: TEdit;
    bUpdateFileProps: TButton;
    MarkersGrp: TGroupBox;
    MarkerTable: TStringGrid;
    bUpdateMarkers: TButton;
    Panel4: TPanel;
    edImageStartDelay: TValidatedEdit;
    FrameTypesTab: TTabSheet;
    FrameTypesGrp: TGroupBox;
    FrameTypeTable: TStringGrid;
    bUpdateFrameTypes: TButton;
    ckSpectralDataFile: TCheckBox;
    HeaderTab: TTabSheet;
    meFileHeader: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bUpdateFilePropsClick(Sender: TObject);
    procedure bUpdateChannelPropsClick(Sender: TObject);
    procedure edPixelUnitsKeyPress(Sender: TObject; var Key: Char);
    procedure bUpdateMarkersClick(Sender: TObject);
    procedure bUpdateFrameTypesClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateChannelEditTable ;
    procedure GetChannelsFromEditTable ;
    procedure FillMarkersTable ;
    procedure ReadMarkersTable ;
    procedure FillFrameTypesTable ;
    procedure ReadFrameTypesTable ;
  public
    { Public declarations }
  end;

var
  FilePropsFrm: TFilePropsFrm;

implementation

uses Main, IDRFile, Maths ;

const
     // Channel calibration table column definitions
     ChNum = 0 ;
     ChName = 1 ;
     ChCal = 2 ;
     ChUnits = 3 ;

     MkTime = 0 ;
     MkText = 1 ;

     FTNum = 0 ;
     FTName = 1 ;

{$R *.dfm}

procedure TFilePropsFrm.FormShow(Sender: TObject);
// ------------------------------------------
// Initialise controls when form is displayed
// ------------------------------------------
begin

     // Set control sizes
     Resize ;

     meProperties.Clear ;
     meProperties.lines.Add( 'File name: ' + MainFrm.IDRFile.FileName ) ;

     meProperties.lines.Add( format('Created (dd/mm/yyyy): %d/%d/%d',
                             [ MainFrm.IDRFile.Day,
                               MainFrm.IDRFile.Month,
                               MainFrm.IDRFile.Year ])) ;

     meProperties.lines.Add( MainFrm.IDRFile.Ident ) ;

     if MainFrm.IDRFile.LineScan then begin
        meProperties.lines.Add( 'File Type: Line scan' ) ;
        end
     else begin
        meProperties.lines.Add( 'File Type: 2D Image' ) ;
        end ;

     meProperties.lines.Add( format('No. frames: %d',[MainFrm.IDRFile.NumFrames])) ;
     meProperties.lines.Add( format('Frame width:  %d',[MainFrm.IDRFile.FrameWidth])) ;
     meProperties.lines.Add( format('Frame height:  %d',[MainFrm.IDRFile.FrameHeight])) ;
     meProperties.lines.Add( format('Pixel depth:  %d bits',[MainFrm.IDRFile.PixelDepth])) ;
     meProperties.lines.Add( format('Pixel grey scale range:  0 - %d',
                             [MainFrm.IDRFile.GreyMax])) ;
     meProperties.lines.Add( format('Pixel size:  %.4g %s',
                             [MainFrm.IDRFile.XResolution,
                              MainFrm.IDRFile.ResolutionUnits])) ;

    meProperties.lines.Add( format('No. Z sections: %d',[MainFrm.IDRFile.NumZSections])) ;
    meProperties.lines.Add( format('Z spacing %.4g %s',
                            [MainFrm.IDRFile.ZSpacing,MainFrm.IDRFile.ResolutionUnits])) ;
    meProperties.lines.Add( format('Z position of first section %.4g %s',
                            [MainFrm.IDRFile.ZStart,MainFrm.IDRFile.ResolutionUnits])) ;

     meProperties.lines.Add( format('File header size (bytes):  %d',
                                    [MainFrm.IDRFile.NumIDRHeaderBytes])) ;

     if MainFrm.IDRFile.LineScan then begin
        // Line scans
         panInterval.Caption := 'Inter-line interval ' ;
         edInterval.Value := MainFrm.IDRFile.FrameInterval / MainFrm.IDRFile.FrameHeight ;
        end
     else begin
        // Images
        panInterval.Caption := 'Inter-frame interval ' ;
        edInterval.Value := MainFrm.IDRFile.FrameInterval ;
        end ;

     edImageStartDelay.Value := MainFrm.IDRFile.ImageStartDelay ;
     edPixelSize.Value := MainFrm.IDRFile.XResolution ;
     edPixelUnits.Text := MainFrm.IDRFile.ResolutionUnits ;
     edPixelSize.Units := edPixelUnits.Text ;

     // Analogue signal channels

     meProperties.lines.Add( ' ' ) ;

     if MainFrm.IDRFile.ADCNumChannels <= 0 then begin
        meProperties.lines.Add( 'No analogue signal channels' ) ;
        end
     else begin
        meProperties.lines.Add( format('No. of analogue channels:  %d',
                             [MainFrm.IDRFile.ADCNumChannels] )) ;
        meProperties.lines.Add( format('No. of analogue time points:  %d (%.4gs)',
                             [MainFrm.IDRFile.ADCNumScansInFile,
                              MainFrm.IDRFile.ADCNumScansInFile*
                              MainFrm.IDRFile.ADCScanInterval])) ;
        meProperties.lines.Add( format('Analogue sampling interval:  %.4gs',
                             [MainFrm.IDRFile.ADCScanInterval])) ;
        meProperties.lines.Add( format('Analogue value range:  %d - %d',
                             [-MainFrm.IDRFile.ADCMaxValue-1,
                              MainFrm.IDRFile.ADCMaxValue])) ;
        meProperties.lines.Add( format('A/D Converter voltage range:  +/-%.4gV',
                             [MainFrm.IDRFile.ADCVoltageRange])) ;
        end ;


     // Fill A/D channel properties table
     UpdateChannelEditTable ;

     // Fill markers table
     FillMarkersTable ;

     // Fill frame types table
     FillFrameTypesTable ;

     ckSpectralDataFile.Checked := MainFrm.IDRFile.SpectralDataFile ;

     // Display file header

     //FileSeek( MainFrm.IDRFile.IDRFileHandle, 0,0) ;
//     FillChar( Header, Sizeof(Header), 0) ;
//     FileRead( MainFrm.IDRFile.IDRFileHandle, Header, SizeOf(Header) ) ;
     meFileHeader.Lines.Clear ;
     meFileHeader.Lines.Text := MainFrm.IDRFile.FileHeader ;

     Page.ActivePage := PropTab ;

     end;

procedure TFilePropsFrm.FormResize(Sender: TObject);
// --------------------------------------
// Resize controls when form size changed
// --------------------------------------
begin

    Page.Width := ClientWidth - Page.Left - 5 ;
    Page.Height := ClientHeight - Page.Top - 5 ;

    EditablePropsGrp.Top := PropTab.ClientHeight - EditablePropsGrp.Height - 5 ;
    EditablePropsGrp.Width := PropTab.ClientWidth - EditablePropsGrp.Left - 5 ;

    meProperties.Width := PropTab.ClientWidth - meProperties.Left - 5 ;
    meProperties.Height := EditablePropsGrp.Top - meProperties.Top - 2 ;

    ChannelsGrp.Width := ADCTab.ClientWidth - ChannelsGrp.Left - 5 ;
    ChannelsGrp.Height := ADCTab.ClientHeight - ChannelsGrp.Top - 5 ;

    bUpdateChannelProps.Top := ChannelsGrp.ClientHeight -  bUpdateChannelProps.Height - 5 ;
    panADCScanInterval.Top := bUpdateChannelProps.Top - panADCScanInterval.Height - 2 ;
    ChannelTable.Width := ChannelsGrp.ClientWidth - ChannelTable.Left - 5 ;
    ChannelTable.Height := panADCScanInterval.Top - ChannelTable.Top - 5 ;

    MarkersGrp.Width := MarkerTab.ClientWidth - MarkersGrp.Left - 5 ;
    MarkersGrp.Height := MarkerTab.ClientHeight - MarkersGrp.Top - 5 ;

    MarkerTable.Width := MarkersGrp.ClientWidth - MarkerTable.Left - 5 ;
    bUpdateMarkers.Top := MarkersGrp.ClientHeight - bUpdateMarkers.Height - 5 ;
    MarkerTable.Height := bUpdateMarkers.Top - MarkerTable.Top - 5 ;

    FrameTypesGrp.Width := FrameTypesTab.ClientWidth - FrameTypesGrp.Left - 5 ;
    FrameTypesGrp.Height := FrameTypesTab.ClientHeight - FrameTypesGrp.Top - 5 ;

    FrameTypeTable.Width := FrameTypesGrp.ClientWidth - FrameTypeTable.Left - 5 ;
    bUpdateFrameTypes.Top := FrameTypesGrp.ClientHeight - bUpdateFrameTypes.Height - 5 ;
    FrameTypeTable.Height := bUpdateFrameTypes.Top - FrameTypeTable.Top - 5 ;
    ckSpectralDataFile.Top := bUpdateFrameTypes.Top ;

    meFileHeader.Height := HeaderTab.ClientHeight - meFileHeader.Top - 5 ;
    meFileHeader.Width := HeaderTab.ClientWidth - meFileHeader.Left - 5 ;

    end;


procedure TFilePropsFrm.UpdateChannelEditTable ;
// ----------------------------
// Update channel editing table
// ----------------------------
var
     ch : Integer ;
begin

     if MainFrm.IDRFile.ADCNumChannels <= 0 then begin
        ChannelTable.RowCount := 1 ;
        Exit ;
        end ;

     { Set A/D input channel calibration table }
     ChannelTable.cells[ChNum,0] := 'Ch.' ;
     ChannelTable.colwidths[ChNum] := ChannelTable.DefaultColWidth div 2 ;
     ChannelTable.cells[ChName,0] := 'Name' ;
     ChannelTable.colwidths[ChName] := ChannelTable.DefaultColWidth ;
     ChannelTable.cells[ChCal,0] := 'V/Units' ;
     ChannelTable.colwidths[ChCal] := (5*ChannelTable.DefaultColWidth) div 4 ;
     ChannelTable.cells[ChUnits,0] := 'Units' ;
     ChannelTable.colwidths[ChUnits] := ChannelTable.DefaultColWidth ;
     ChannelTable.RowCount := MainFrm.IDRFile.ADCNumChannels + 1;
     ChannelTable.options := [goEditing,goHorzLine,goVertLine] ;

     for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
         ChannelTable.cells[ChNum,ch+1] := IntToStr(ch) ;
         ChannelTable.cells[ChName,ch+1] := MainFrm.IDRFile.ADCChannel[ch].ADCName ;
         ChannelTable.cells[ChCal,ch+1] := Format( '%5.4g',
                                           [MainFrm.IDRFile.ADCChannel[ch].ADCCalibrationFactor] ) ;
         ChannelTable.cells[ChUnits,ch+1] := MainFrm.IDRFile.ADCChannel[ch].ADCUnits ;
         end ;

     edADCScanInterval.Value := MainFrm.IDRFile.ADCScanInterval ;

     end ;


procedure TFilePropsFrm.GetChannelsFromEditTable ;
// --------------------------------------------
// Get channel calibration data from edit table
// --------------------------------------------
var
    ch : Integer ;
    Channel : TChannel ;
begin

    for ch := 0 to MainFrm.IDRFile.ADCNumChannels-1 do begin
       Channel := MainFrm.IDRFile.ADCChannel[ch] ;
       Channel.ADCName := ChannelTable.cells[ChName,ch+1] ;
       Channel.ADCCalibrationFactor := ExtractFloat(
                                             ChannelTable.cells[ChCal,ch+1],
                                             Channel.ADCCalibrationFactor);
       Channel.ADCUnits := ChannelTable.cells[ChUnits,ch+1] ;
       MainFrm.IDRFile.ADCChannel[ch] := Channel ;
       end ;

    MainFrm.IDRFile.ADCScanInterval := edADCScanInterval.Value ;
    end ;


procedure TFilePropsFrm.FillMarkersTable ;
// ----------------------------
// Fill recording markers table
// ----------------------------
var
     i : Integer ;
begin

     MarkerTable.cells[MkTime,0] := 'Time' ;
     MarkerTable.colwidths[MKTime] := MarkerTable.DefaultColWidth div 2 ;
     MarkerTable.cells[MkText,0] := 'Marker Text' ;
     MarkerTable.colwidths[ChName] := MarkerTable.DefaultColWidth ;
     MarkerTable.options := [goEditing,goHorzLine,goVertLine] ;

     MarkerTable.RowCount := MainFrm.IDRFile.NumMarkers+1 ;

     for i := 0 to MainFrm.IDRFile.NumMarkers-1 do begin
         MarkerTable.cells[mkTime,i+1] := format(' %.4g s',
                                           [MainFrm.IDRFile.MarkerTime[i]]) ;
         MarkerTable.cells[mkText,i+1] := MainFrm.IDRFile.MarkerText[i] ;
         end ;

     end ;


procedure TFilePropsFrm.ReadMarkersTable ;
// ----------------------------
// Read recording markers table
// ----------------------------
var
     i : Integer ;
begin

     for i := 0 to MainFrm.IDRFile.NumMarkers-1 do begin
         MainFrm.IDRFile.MarkerTime[i] := ExtractFloat(MarkerTable.cells[mkTime,i+1],0.0);
         MainFrm.IDRFile.MarkerText[i] := MarkerTable.cells[mkText,i+1] ;
         end ;
     end ;


procedure TFilePropsFrm.FillFrameTypesTable ;
// ----------------------------
// Fill frame types table
// ----------------------------
var
     i : Integer ;
begin

     FrameTypeTable.cells[ftNum,0] := 'No. ' ;
     FrameTypeTable.colwidths[ftNum] := FrameTypeTable.DefaultColWidth ;
     FrameTypeTable.cells[ftName,0] := 'Name ' ;
     FrameTypeTable.colwidths[ftName] := FrameTypeTable.DefaultColWidth*5 ;
     FrameTypeTable.options := [goEditing,goHorzLine,goVertLine] ;

     FrameTypeTable.RowCount := MainFrm.IDRFile.NumFrameTypes+1 ;

     for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do begin
         FrameTypeTable.cells[ftNum,i+1] := format('%d',[i]) ;
         FrameTypeTable.cells[ftName,i+1] := MainFrm.IDRFile.FrameType[i] ;
         end ;

     end ;


procedure TFilePropsFrm.ReadFrameTypesTable ;
// ----------------------------
// Read recording Frame Types table
// ----------------------------
var
     i : Integer ;
begin

     for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do begin
         MainFrm.IDRFile.FrameType[i] := FrameTypeTable.cells[ftName,i+1] ;
         end ;

     end ;

procedure TFilePropsFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ----------------------
// Close and destroy form
// ----------------------
begin
     Action := caFree ;
     end;

procedure TFilePropsFrm.bUpdateFilePropsClick(Sender: TObject);
// -------------------------------
// Update editable file properties
// -------------------------------
begin
     if MainFrm.IDRFile.LineScan then begin
        // Line scans
        MainFrm.IDRFile.FrameInterval := edInterval.Value*MainFrm.IDRFile.FrameHeight ;
        end
     else begin
        // Images
        MainFrm.IDRFile.FrameInterval := edInterval.Value ;
        end ;
     MainFrm.IDRFile.ImageStartDelay := edImageStartDelay.Value ;
     MainFrm.IDRFile.XResolution := edPixelSize.Value ;
     MainFrm.IDRFile.ResolutionUnits := edPixelUnits.Text ;

     end;

procedure TFilePropsFrm.bUpdateChannelPropsClick(Sender: TObject);
// -----------------------------
// Update A/D channel properties
// -----------------------------
begin
     GetChannelsFromEditTable ;
     end;

procedure TFilePropsFrm.edPixelUnitsKeyPress(Sender: TObject;
  var Key: Char);
// -------------------
// Pixel units changed
// -------------------
begin
     if key = #13 then edPixelSize.Units := edPixelUnits.Text ;
     end;

procedure TFilePropsFrm.bUpdateMarkersClick(Sender: TObject);
// ---------------------
// Update markers table
// ---------------------
begin
     ReadMarkersTable ;
     end;

procedure TFilePropsFrm.bUpdateFrameTypesClick(Sender: TObject);
// ---------------------
// Update markers table
// ---------------------
begin
     ReadFrameTypesTable ;
     MainFrm.IDRFile.SpectralDataFile := ckSpectralDataFile.Checked ;
     end;

end.
