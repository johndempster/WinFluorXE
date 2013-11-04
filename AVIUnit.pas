unit AVIUnit;
// ----------------------------------------------
// Create an AVI movie from a WinFluor data file
// ----------------------------------------------
// 2.8.2002
// 22.10.2002 ... A/D channels can now be included in AVIs
// 22.3.2003 .... Updated to work when compiled with Delphi 7
// 7.7.03 .... Red,green,blue colour palettes added
// 29.7.03....
// 05.04.05 .. AVI file name now set using text box
//             Many bugs fixed
// 20.02.07 .. ROI time courses can now be plotted with plots selected from list
//             % Area allocated to plots can be set by user
//             AVIFileInit call fixed (was calling AVIFileExit)
//             Temporary file no longer used
// 03.03.07 .. 24 bit pixel format AVIs now supported
//             Subtraction ROI added to ROI plot
// 19.03.07 ..
// 30.07.07 .. Fluorescence ratio plots can now be display in movie
// 08.05.08 .. Dual-rate multi-wavelength frames now supported
// 10.09.09 .. JD Pixel Exclusion facility removed from MeanROIIntensity
{$O-}
interface

uses
  Windows, Graphics, Messages, SysUtils, Classes,  Controls, Forms, Dialogs,
  StdCtrls, RangeEdit, ActiveX, ValEdit, SetupUnit, ValidatedEdit, IDRFile,
  ExtCtrls, math, Grids, mmsystem, strutils ;

// TAVIFileInfo dwFlag values
const
    AVIF_HASINDEX		= $00000010;
    AVIF_MUSTUSEINDEX	= $00000020;
    AVIF_ISINTERLEAVED	= $00000100;
    AVIF_WASCAPTUREFILE	= $00010000;
    AVIF_COPYRIGHTED	= $00020000;
    AVIF_KNOWN_FLAGS	= $00030130;

    AVIERR_UNSUPPORTED              = $80044065; // MAKE_AVIERR(101)
    AVIERR_BADFORMAT                = $80044066; // MAKE_AVIERR(102)
    AVIERR_MEMORY                   = $80044067; // MAKE_AVIERR(103)
    AVIERR_INTERNAL                 = $80044068; // MAKE_AVIERR(104)
    AVIERR_BADFLAGS                 = $80044069; // MAKE_AVIERR(105)
    AVIERR_BADPARAM                 = $8004406A; // MAKE_AVIERR(106)
    AVIERR_BADSIZE                  = $8004406B; // MAKE_AVIERR(107)
    AVIERR_BADHANDLE                = $8004406C; // MAKE_AVIERR(108)
    AVIERR_FILEREAD                 = $8004406D; // MAKE_AVIERR(109)
    AVIERR_FILEWRITE                = $8004406E; // MAKE_AVIERR(110)
    AVIERR_FILEOPEN                 = $8004406F; // MAKE_AVIERR(111)
    AVIERR_COMPRESSOR               = $80044070; // MAKE_AVIERR(112)
    AVIERR_NOCOMPRESSOR             = $80044071; // MAKE_AVIERR(113)
    AVIERR_READONLY                 = $80044072; // MAKE_AVIERR(114)
    AVIERR_NODATA                   = $80044073; // MAKE_AVIERR(115)
    AVIERR_BUFFERTOOSMALL           = $80044074; // MAKE_AVIERR(116)
    AVIERR_CANTCOMPRESS             = $80044075; // MAKE_AVIERR(117)
    AVIERR_USERABORT                = $800440C6; // MAKE_AVIERR(198)
    AVIERR_ERROR                    = $800440C7; // MAKE_AVIERR(199)

// TAVIStreamInfo dwFlag values
  MaxPlots = 108 ;
  AVISF_DISABLED	= $00000001;
  AVISF_VIDEO_PALCHANGES= $00010000;
  AVISF_KNOWN_FLAGS	= $00010001;

// Palette change data record
  RIFF_PaletteChange: DWORD = 1668293411;

  AVIERR_OK       = 0;

  AVIIF_LIST      = $01;
  AVIIF_TWOCC	  = $02;
  AVIIF_KEYFRAME = $10;

  streamtypeVIDEO = $73646976; // DWORD( 'v', 'i', 'd', 's' )
  streamtypeAUDIO = $73647561; // DWORD( 'a', 'u', 'd', 's' )



type

    TSmallIntBuf = Array[0..9999999] of SmallInt ;
    PSmallIntBuf = ^TSmallIntBuf ;


  TAVIFileInfoW = record
    dwMaxBytesPerSec,	// max. transfer rate
    dwFlags,		// the ever-present flags
    dwCaps,
    dwStreams,
    dwSuggestedBufferSize,

    dwWidth,
    dwHeight,

    dwScale,
    dwRate,	// dwRate / dwScale == samples/second
    dwLength,

    dwEditCount: DWORD;

    szFileType: array[0..63] of WideChar;		// descriptive string for file type?
    end;
  PAVIFileInfoW = ^TAVIFileInfoW;

  TAVIStreamInfoA = record
    fccType,
    fccHandler,
    dwFlags,        // Contains AVITF_* flags
    dwCaps: DWORD;
    wPriority,
    wLanguage: WORD;
    dwScale,
    dwRate, // dwRate / dwScale == samples/second
    dwStart,
    dwLength, // In units above...
    dwInitialFrames,
    dwSuggestedBufferSize,
    dwQuality,
    dwSampleSize: DWORD;
    rcFrame: TRect;
    dwEditCount,
    dwFormatChangeCount: DWORD;
    szName:  array[0..63] of AnsiChar;
    end;
  TAVIStreamInfo = TAVIStreamInfoA;
  PAVIStreamInfo = ^TAVIStreamInfo;

  { TAVIStreamInfoW record }

  TAVIStreamInfoW = record
    fccType,
    fccHandler,
    dwFlags,        // Contains AVITF_* flags
    dwCaps: DWORD;
    wPriority,
    wLanguage: WORD;
    dwScale,
    dwRate, // dwRate / dwScale == samples/second
    dwStart,
    dwLength, // In units above...
    dwInitialFrames,
    dwSuggestedBufferSize,
    dwQuality,
    dwSampleSize: DWORD;
    rcFrame: TRect;
    dwEditCount,
    dwFormatChangeCount: DWORD;
    szName:  array[0..63] of WideChar;
    end;

  PAVIStream = pointer;
  PAVIFile = pointer;
  TAVIStreamList = array[0..0] of PAVIStream;
  PAVIStreamList = ^TAVIStreamList;


  TAVICompressOptions = packed record
    fccType		: DWORD;
    fccHandler		: DWORD;
    dwKeyFrameEvery	: DWORD;
    dwQuality		: DWORD;
    dwBytesPerSecond	: DWORD;
    dwFlags		: DWORD;
    lpFormat		: pointer;
    cbFormat		: DWORD;
    lpParms		: pointer;
    cbParms		: DWORD;
    dwInterleaveEvery	: DWORD;
  end;
  PAVICompressOptions = ^TAVICompressOptions;

  TPlotList = record
      Name : String ;
      Units : String ;
      ROIPlot : Boolean ;
      Index : Integer ;
      Channel : Integer ;
      FrameType : Integer ;
      YMin : Single ;
      YMax : Single ;
      Color : TColor ;
      end ;

  TAVIPalChange = packed record
    bFirstEntry		: byte;
    bNumEntries		: byte;
    wFlags		: WORD;
    peNew		: array[byte] of TPaletteEntry;
  end;
  PAVIPalChange = ^TAVIPalChange;

  APAVISTREAM          = array[0..1] of PAVISTREAM;
  APAVICompressOptions = array[0..1] of PAVICompressOptions;
  TAVISaveCallback = function (nPercent: integer): LongInt; stdcall;

type
  TAVIFrm = class(TForm)
    bOK: TButton;
    GroupBox2: TGroupBox;
    rbAllFrames: TRadioButton;
    rbRange: TRadioButton;
    edRange: TRangeEdit;
    SaveDialog: TSaveDialog;
    GroupBox3: TGroupBox;
    edPlayBackRate: TValidatedEdit;
    bCancel: TButton;
    FilenameGrp: TGroupBox;
    edFileName: TEdit;
    bChangeName: TButton;
    FontDialog: TFontDialog;
    Label1: TLabel;
    edFrameStep: TValidatedEdit;
    Shape1: TShape;
    PlotChannelGrp: TGroupBox;
    GroupBox6: TGroupBox;
    bClearPlots: TButton;
    Label2: TLabel;
    GroupBox8: TGroupBox;
    cbSubtractROI: TComboBox;
    CalTableGrp: TGroupBox;
    sgCalTable: TStringGrid;
    bAddPlot: TButton;
    cbROI: TComboBox;
    mePlotList: TMemo;
    TextGrp: TGroupBox;
    edFont: TEdit;
    bSetFont: TButton;
    Label4: TLabel;
    edPlotArea: TValidatedEdit;
    rbFLuorescence: TRadioButton;
    rbRatio: TRadioButton;
    FLPanel: TPanel;
    ckFrameType0: TCheckBox;
    ckFrameType1: TCheckBox;
    ckFrameType2: TCheckBox;
    ckFrameType3: TCheckBox;
    ckFrameType4: TCheckBox;
    ckFrameType5: TCheckBox;
    ckFrameType6: TCheckBox;
    ckFrameType7: TCheckBox;
    ckFrameType8: TCheckBox;
    RatioPanel: TPanel;
    Label5: TLabel;
    Shape2: TShape;
    cbNumWave: TComboBox;
    cbDenWave: TComboBox;
    edRatioExclusionThreshold: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCancelClick(Sender: TObject);
    procedure bChangeNameClick(Sender: TObject);
    procedure bSetFontClick(Sender: TObject);
    procedure bAddPlotClick(Sender: TObject);
    procedure bClearPlotsClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbSubtractROIChange(Sender: TObject);
    procedure rbFLuorescenceClick(Sender: TObject);
    procedure rbRatioClick(Sender: TObject);
  private
    { Private declarations }
    NumROIsAdded : Integer ;
    NumPlots : Integer ;
    PlotList : Array[0..MaxPlots-1] of TPlotList ;
    PlotStarted : Array[0..MaxPlots-1] of Boolean ;
    X : Array[0..MaxPlots-1] of Integer ;
    Y : Array[0..MaxPlots-1] of Integer ;
    ROIColor : Array[0..3] of TColor ;
    LeftMargin : Integer ;
    NumFrameTypes : Integer ;

    CombinedBitMap : TBitMap ;
    ImageBitMap : TBitMap ;

    function NumADCPlots : Integer ;
    function NumROIPlots : Integer ;
    procedure UpdateFLBitMap(
              FLBM : TBitMap ;
              PlotTop : Integer ;
              PlotHeight : Integer ;
              Frame : Integer ;
              StartFrame : Integer ;
              EndFrame : Integer
              ) ;
    procedure FindFLRange(
              iFrameType : Integer ;
              iROI : Integer ;
              var YMin : Single ;
              var YMax : Single
              ) ;
    procedure FindRatioRange(
              iROI : Integer ;
              var YMin : Single ;
              var YMax : Single
              ) ;

procedure UpdateRatioBitMap(
          RBM : TBitMap ;
          PlotTop : Integer ;
          PlotHeight : Integer ;
          Frame : Integer ;
          StartFrame : Integer ;
          EndFrame : Integer
          ) ;

    procedure UpdateADCBitMap(
              ADCBM : TBitMap ;
              PlotTop : Integer ;
              PlotHeight : Integer ;
              Frame : Integer ;
              StartFrame : Integer ;
              EndFrame : Integer ;
              FrameStep : Integer ) ;

    procedure FindADCRange(
          iChan : Integer ;
          var YMin : Single ;
          var YMax : Single
          ) ;

    procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer;
              var ImageSize: longInt; PixelFormat: TPixelFormat);
    function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
             var BitmapInfo; var Bits; PixelFormat: TPixelFormat): Boolean;
    procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var Info: TBitmapInfoHeader;
              PixelFormat: TPixelFormat);

    function AlignBit(Bits, BitsPerPixel, Alignment: Cardinal): Cardinal;

    procedure UpdatePlotCalibrationTable ;

    procedure Wait( Time : Single ) ;

    procedure SetFrameType(
              CheckBox : TCheckBox ;
              ch : Integer
              ) ;
    function FrameTypeSelected(
             FrameNum : Integer ) : Boolean ;


  public
    { Public declarations }
  end;

var
  AVIFrm: TAVIFrm;
    procedure AVIFileInit; stdcall ; external 'avifil32.dll' name 'AVIFileInit';
    procedure AVIFileExit; stdcall ; external 'avifil32.dll' name 'AVIFileExit';
    function AVIFileOpen(var ppfile: PAVIFile; szFile: PChar; uMode: UINT; lpHandler: pointer): HResult;
         stdcall; external 'avifil32.dll' name 'AVIFileOpenA';
function AVIFileCreateStream(pfile: PAVIFile; var ppavi: PAVISTREAM; var psi: TAVIStreamInfo): HResult;
         stdcall ; external 'avifil32.dll' name 'AVIFileCreateStreamA';
function AVIStreamSetFormat(pavi: PAVIStream; lPos: LongInt; lpFormat: pointer; cbFormat: LongInt): HResult;
         stdcall; external 'avifil32.dll' name 'AVIStreamSetFormat';
function AVIStreamReadFormat(pavi: PAVIStream; lPos: LongInt; lpFormat: pointer; var cbFormat: LongInt): HResult;
         stdcall; external 'avifil32.dll' name 'AVIStreamReadFormat';
function AVIStreamWrite(pavi: PAVIStream; lStart, lSamples: LongInt; lpBuffer: pointer; cbBuffer: LongInt; dwFlags: DWORD; var plSampWritten: LongInt; var plBytesWritten: LongInt): HResult;
         stdcall; external 'avifil32.dll' name 'AVIStreamWrite';
function AVIStreamRelease(pavi: PAVISTREAM): ULONG;
         stdcall; external 'avifil32.dll' name 'AVIStreamRelease';
function AVIFileRelease(pfile: PAVIFile): ULONG;
         stdcall; external 'avifil32.dll' name 'AVIFileRelease';
function AVIFileGetStream(pfile: PAVIFile; var ppavi: PAVISTREAM; fccType: DWORD; lParam: LongInt): HResult;
         stdcall; external 'avifil32.dll' name 'AVIFileGetStream';
function CreateEditableStream(var ppsEditable: PAVISTREAM; psSource: PAVISTREAM): HResult;
         stdcall;external 'avifil32.dll' name 'CreateEditableStream';

function AVISaveV(szFile: PChar; pclsidHandler: PCLSID; lpfnCallback: TAVISaveCallback;
         nStreams: integer; pavi: APAVISTREAM; lpOptions: APAVICompressOptions): HResult;
         stdcall; external 'avifil32.dll' name 'AVISaveV';



implementation

uses FileIOUnit, Main, maths , ViewUnit, ViewPlotUnit;

{$R *.DFM}

const
    AVIFileExtension = '.AVI' ;
    CalNameCol = 0 ;
    CalLowCol = 1 ;
    CalHighCol = 2 ;
    CalUnitsCol = 3 ;


function  TAVIFrm.InternalGetDIB(
          Bitmap: HBITMAP;          // Bitmap	The handle of the source bitmap.
          Palette: HPALETTE;        // Pal		The handle of the source palette.
          var BitmapInfo;
          var Bits;                 // Bits		The buffer that will receive the DIB's pixel data.
          PixelFormat: TPixelFormat // PixelFormat	The pixel format of the destination DIB.
          ): Boolean;
// --------------
// InternalGetDIB
// --------------
// From graphics.pas, "optimized" for our use
// Converts a bitmap to a DIB of a specified PixelFormat.
// Note: The InternalGetDIBSizes function can be used to calculate the
// nescessary sizes of the BitmapInfo and Bits buffers.
var
  OldPal	: HPALETTE;
  DC		: HDC;
  nLines : Integer ;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), PixelFormat);
 // TBitmapInfo(BitmapInfo).bmiColors[0].rgbBlue := 0 ;
 // TBitmapInfo(BitmapInfo).bmiColors[0].rgbRed := 0 ;
 // TBitmapInfo(BitmapInfo).bmiColors[0].rgbGreen := 0 ;
 //   TBitmapInfo(BitmapInfo).bmiColors[0].rgbReserved := 0 ;
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if (Palette <> 0) then begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
      end;
    nLines := GetDIBits(DC, Bitmap, 0, abs(TBitmapInfoHeader(BitmapInfo).biHeight),
               @Bits, TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) ;
    if nLines = 0 then Result := False
                  else Result := True ;
  finally
    if (OldPal <> 0) then SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
    end;
  end;


// -------------------
// InternalGetDIBSizes
// -------------------
// Calculates the buffer sizes nescessary for convertion of a bitmap to a DIB
// of a specified PixelFormat.
// See the GetDIBSizes API function for more info.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// InfoHeaderSize
//		The returned size of a buffer that will receive the DIB's
//		TBitmapInfo structure.
// ImageSize	The returned size of a buffer that will receive the DIB's
//		pixel data.
// PixelFormat	The pixel format of the destination DIB.
//
procedure  TAVIFrm.InternalGetDIBSizes(
           Bitmap: HBITMAP;
           var InfoHeaderSize: Integer;
           var ImageSize: longInt;
           PixelFormat: TPixelFormat);
// From graphics.pas, "optimized" for our use
var
  Info		: TBitmapInfoHeader;
begin

  InitializeBitmapInfoHeader(Bitmap, Info, PixelFormat);

  // Check for palette device format
  if (Info.biBitCount > 8) then begin
    // Header but no palette
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if ((Info.biCompression and BI_BITFIELDS) <> 0) then Inc(InfoHeaderSize, 12);
    end
  else
    // Header and palette
    InfoHeaderSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl Info.biBitCount);
  ImageSize := Info.biSizeImage;
  end;

// --------------------------
// InitializeBitmapInfoHeader
// --------------------------
// Fills a TBitmapInfoHeader with the values of a bitmap when converted to a
// DIB of a specified PixelFormat.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// Info		The TBitmapInfoHeader buffer that will receive the values.
// PixelFormat	The pixel format of the destination DIB.
//


procedure  TAVIFrm.InitializeBitmapInfoHeader(
           Bitmap: HBITMAP;
           var Info: TBitmapInfoHeader;
           PixelFormat: TPixelFormat
           );
// From graphics.pas, "optimized" for our use
var
  DIB	: TDIBSection;
  Bytes	: Integer;

begin
  DIB.dsbmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DIB), @DIB);
  if (Bytes = 0) then raise Exception.Create('Invalid bitmap');
//    Error(sInvalidBitmap);

  if (Bytes >= (sizeof(DIB.dsbm) + sizeof(DIB.dsbmih))) and
     (DIB.dsbmih.biSize >= sizeof(DIB.dsbmih)) then
     Info := DIB.dsbmih
  else begin
     FillChar(Info, sizeof(Info), 0);
     with Info, DIB.dsbm do begin
        biSize := SizeOf(Info);
        biWidth := bmWidth;
        biHeight := bmHeight;
        end;
     end;

  case PixelFormat of
    pf1bit: Info.biBitCount := 1;
    pf4bit: Info.biBitCount := 4;
    pf8bit: Info.biBitCount := 8;
    pf24bit: Info.biBitCount := 24;
  else
//    Error(sInvalidPixelFormat);
    raise Exception.Create('Invalid pixel foramt');
    // Info.biBitCount := DIB.dsbm.bmBitsPixel * DIB.dsbm.bmPlanes;
    end;
  Info.biPlanes := 1;
  Info.biCompression := BI_RGB; // Always return data in RGB format
  Info.biSizeImage := AlignBit(Info.biWidth, Info.biBitCount, 32) * Cardinal(abs(Info.biHeight));
  end;


function TAVIFrm.AlignBit(Bits, BitsPerPixel, Alignment: Cardinal): Cardinal;
begin
    Dec(Alignment);
    Result := ((Bits * BitsPerPixel) + Alignment) and not Alignment;
    Result := Result SHR 3;
  end;



function TAVIFrm.NumROIPlots : Integer ;
// ------------------------------------
// Are any ROIs to be displayed?
// ------------------------------------
var
    i,NumChannels : Integer ;
begin

    NumChannels := 0 ;
    for i := 0 to NumPlots-1 do if PlotList[i].ROIPlot then Inc(NumChannels) ;
    if NumChannels > 0 then Result := NumFrameTypes
                       else Result := 0 ;
    end ;


function TAVIFrm.NumADCPlots : Integer ;
// ------------------------------------
// Are any A/D channels to be displayed?
// ------------------------------------
var
    i,NumChannels : Integer ;
begin

    NumChannels := 0 ;
    for i := 0 to NumPlots-1 do if not PlotList[i].ROIPlot then Inc(NumChannels) ;

    Result := NumChannels ;
    end ;


procedure TAVIFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
var
     i,ch : Integer ;
begin

     // Get number and types of frames in use
     if MainFrm.IDRFile.SpectralDataFile then NumFrameTypes := 1
     else NumFrameTypes := MainFrm.IDRFile.NumFrameTypes ;

     // Export file name
     edFileName.Text := ChangeFileExt(MainFrm.IDRFile.FileName,AVIFileExtension) ;

     // Frame range
     edRange.LoLimit := 1 ;
     edRange.HiLimit := MainFrm.IDRFile.NumFrames ;
     edRange.LoValue := edRange.LoLimit ;
     edRange.HiValue := edRange.HiLimit ;

     // Font
     edFont.Text := format( '%s %d pts.',
                            [FontDialog.Font.Name,FontDialog.Font.Size] ) ;
     //edFont.Font.Assign(FontDialog.Font) ;

     // Add A/D channels and ROIs available for plotting to list
     cbROI.Clear ;
     NumPlots := 0 ;
     NumROIsAdded := 0 ;
     for i := 0 to MainFrm.IDRFile.ADCNumChannels-1 do
         cbROI.Items.AddObject( MainFrm.IDRFile.ADCChannel[i].ADCName, TObject(i) ) ;
     for i := 1 to MainFrm.IDRFile.MaxROI do if  MainFrm.IDRFile.ROI[i].InUse then
         cbROI.Items.AddObject( format( 'ROI.%d',[i]), TObject(i)) ;
     if cbROI.Items.Count > 0 then cbROI.ItemIndex := 0 ;

     // Add ROIs to subtraction list
     cbSubtractROI.Clear ;
     cbSubtractROI.Items.AddObject( ' ', TObject(MainFrm.IDRFile.MaxROI+1) ) ;
     for i := 1 to MainFrm.IDRFile.MaxROI do if  MainFrm.IDRFile.ROI[i].InUse then
         cbSubtractROI.Items.AddObject( format( 'ROI.%d',[i]), TObject(i)) ;
     if cbSubtractROI.Items.Count > 0 then cbSubtractROI.ItemIndex := 0 ;

     // ROI colours
     ROIColor[0] := clWhite ;
     ROIColor[1] := clRed ;
     ROIColor[2] := clGreen ;
     ROIColor[3] := clBlue ;

     sgCalTable.Cells[CalLowCol,0] := 'Low' ;
     sgCalTable.Cells[CalHighCol,0] := 'High' ;
     sgCalTable.Cells[CalUnitsCol,0] := 'Units' ;
     sgCalTable.RowCount := 1 ;

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

     // Set numerator and denominator wavelength lists
     cbNumWave.Clear ;
     for i := 0 to MainFrm.IDRFile.NumFrameTypes-1 do begin
          cbNumWave.Items.Add( MainFrm.IDRFile.FrameType[i] ) ;
          end ;
     cbDenWave.Items.Assign( cbNumWave.Items ) ;
     cbNumWave.ItemIndex := 0 ;
     cbDenWave.ItemIndex := Min(1,cbDenWave.Items.Count-1) ;

     Resize ;

     // Update table of calibration labels
     UpdatePlotCalibrationTable ;

     end;


procedure TAVIFrm.SetFrameType(
          CheckBox : TCheckBox ;
          ch : Integer
          ) ;
// ----------------------------
// Set frame type selection box
// ----------------------------
begin

     if NumFrameTypes > ch then begin
        CheckBox.Caption := MainFrm.IDRFile.FrameType[ch] ;
        CheckBox.Visible := True ;
        CheckBox.Checked := True ;
        end
     else begin
        CheckBox.Visible := False ;
        CheckBox.Checked := False ;
        end ;
     end ;


function TAVIFrm.FrameTypeSelected(
          FrameNum : Integer ) : Boolean ;
//
// Return TRUE if frame type box checked
//
begin
    case FrameNum of
      0 : Result := ckFrameType0.checked and ckFrameType0.Visible ;
      1 : Result := ckFrameType1.checked and ckFrameType1.Visible ;
      2 : Result := ckFrameType2.checked and ckFrameType2.Visible ;
      3 : Result := ckFrameType3.checked and ckFrameType3.Visible ;
      4 : Result := ckFrameType4.checked and ckFrameType4.Visible ;
      5 : Result := ckFrameType5.checked and ckFrameType5.Visible ;
      6 : Result := ckFrameType6.checked and ckFrameType6.Visible ;
      7 : Result := ckFrameType7.checked and ckFrameType7.Visible ;
      8 : Result := ckFrameType8.checked and ckFrameType8.Visible ;
      Else Result := False ;
      end ;
    end ;


procedure TAVIFrm.UpdateFLBitMap(
          FLBM : TBitMap ;            // Fluorescence display bitmap [In/Out]
          PlotTop : Integer ;
          PlotHeight : Integer ;
          Frame : Integer ;            // Current frame
          StartFrame : Integer ;       // First frame in display sequence [IN]
          EndFrame : Integer          // Last frame in display sequence [IN]
          ) ;
// ----------------------------------------------------
// Add A/D samples for this frame to A/D display bitmap
// ----------------------------------------------------
var
     XScale,ROIIntensity : Single ;
     i,ft,YBot,iPlot : Integer ;
     YScale : Array[0..MaxFrameType] of Single ;
     YName : Array[0..MaxFrameType] of String ;
     YMinLab : Array[0..MaxFrameType] of String ;
     YMaxLab : Array[0..MaxFrameType] of String ;
     YMin : Array[0..MaxFrameType] of Single ;
     YMax : Array[0..MaxFrameType] of Single ;
     YBottom : Array[0..MaxFrameType] of Integer ;
     YTop : Array[0..MaxFrameType] of Integer ;
     ChannelDisplayHeight : Integer ;
     ChannelDisplaySpacing : Integer ;
     iFrameType : Integer ;
     iSubROI : Integer ;
     Row : Integer ;
begin

     //if (EndFrame mod 2) = 0 then EndFrame := EndFrame + 1 ;

     // Set plot X scale factor
     XScale := (FLBM.Width - LeftMargin) /
               ((EndFrame - StartFrame + 1)*MainFrm.IDRFile.FrameInterval) ;

     // Get number of displayed channels
     ChannelDisplaySpacing := PlotHeight div NumFrameTypes ;
     ChannelDisplayHeight := ChannelDisplaySpacing - 2 ;

     // Set plot Y scale factors
     YBot := PlotTop + PlotHeight ;
     for FT := 0 to NumFrameTypes-1 do begin

        // Find row in calibration table
        Row := 0 ;
        for i := 1 to sgCalTable.RowCount do begin
            if Pos(MainFrm.IDRFile.FrameType[ft],sgCalTable.Cells[0,i]) > 0 then Row := i ;
            end ;

        YName[ft] := sgCalTable.Cells[CalNameCol,Row] ;

        // Set min-max plot range
        YMinLab[ft] := sgCalTable.Cells[CalLowCol,Row] ;
        YMin[ft] := ExtractFloat(YMinLab[ft],0.0)/MainFrm.IDRFile.IntensityScale ;
        YMaxLab[ft] := sgCalTable.Cells[CalHighCol,Row] ;
        YMax[ft] := ExtractFloat(YMaxLab[ft],0.0)/MainFrm.IDRFile.IntensityScale ;
        if YMin[ft] = YMax[ft] then YMax[ft] := YMin[ft] + 1.0 ;

        YBottom[ft] := YBot ;
        YTop[ft] := YBot - ChannelDisplayHeight ;
        YScale[ft] := ChannelDisplayHeight/(YMax[ft] - YMin[ft]) ;
        YBot := YBot - ChannelDisplaySpacing ;

        end ;

     // Initialise display area
     if Frame = StartFrame then begin
        // Set background colour
        FLBM.Canvas.Brush.Color := clBlack ;
        //FLBM.Canvas.FillRect( FLBM.Canvas.ClipRect ) ;

        // Set line colour
        FLBM.Canvas.Pen.Color := clWhite ;

        // Set font
        FLBM.Canvas.Font.Assign(FontDialog.Font) ;
        if FLBM.Canvas.Font.Color = FLBM.Canvas.Brush.Color then
           FLBM.Canvas.Font.Color := FLBM.Canvas.Pen.Color ;

        // Channel name
        for ft := 0 to NumFrameTypes-1 do begin

            // Add calibration text
            FLBM.Canvas.TextOut( LeftMargin - FLBM.Canvas.TextWidth(YMaxLab[ft]) - 3,
                                 YTop[ft],
                                 YMaxLab[ft]) ;
            FLBM.Canvas.TextOut( LeftMargin - FLBM.Canvas.TextWidth(YMinLab[ft]) - 3,
                                 YBottom[ft] - FLBM.Canvas.TextHeight('X') - 1,
                                 YMinLab[ft]) ;
            FLBM.Canvas.TextOut( 0,
                                (YTop[ft] + YBottom[ft] - FLBM.Canvas.TextHeight('X')) div 2,
                                 YName[ft] ) ;

            // Draw calibration bar
            FLBM.Canvas.MoveTo( LeftMargin - 5, YTop[ft] ) ;
            FLBM.Canvas.LineTo( LeftMargin - 2, YTop[ft] ) ;
            FLBM.Canvas.LineTo( LeftMargin - 2, YBottom[ft] ) ;
            FLBM.Canvas.LineTo( LeftMargin - 5, YBottom[ft] ) ;

            end ;

        end ;

     // Plot points

     for iPlot := 0 to NumPlots-1 do
         if PlotList[iPlot].ROIPlot then begin

         iFrameType := PlotList[iPlot].FrameType ;

         // Get ROI intensity measurement
         ROIIntensity := ViewPlotFrm.ROIIntensity( PlotList[iPlot].Index,Frame,iFrameType ) ;
         iSubROI := Integer(cbSubtractROI.Items.Objects[cbSubtractROI.ItemIndex]) ;
         if  iSubROI <= MainFrm.IDRFile.MaxROI then begin
             ROIIntensity := ROIIntensity - ViewPlotFrm.ROIIntensity( iSubROI,Frame,iFrameType ) ;
             end ;
         ROIIntensity := Max( Min( ROIIntensity,YMax[iFrameType] ),YMin[iFrameType] );

         if PlotStarted[iPlot] then FLBM.Canvas.MoveTo( X[iPlot], Y[iPlot] ) ;

         Y[iPlot] := YBottom[iFrameType]
                     - Round((ROIIntensity - YMin[iFrameType])*YScale[iFrameType]) ;
         X[iPlot] := LeftMargin +
                         Round(XScale*(Frame-StartFrame)*MainFrm.IDRFile.FrameInterval) ;

         if (X[iPlot] < FLBM.Width) and PlotStarted[iPlot] then begin
            FLBM.Canvas.Pen.Color := PlotList[iPlot].Color ;
            FLBM.Canvas.LineTo( X[iPlot], Y[iPlot] ) ;
            FLBM.Canvas.Pen.Color := clWhite ;
            end ;

         PlotStarted[iPlot] := True ;

         end ;

     end ;


procedure TAVIFrm.FindFLRange(
          iFrameType : Integer ;
          iROI : Integer ;
          var YMin : Single ;
          var YMax : Single
          ) ;
// --------------------------------------------------
// Find min/max range of fluorescence ROI time course
// --------------------------------------------------
var
   ROIIntensity : Single ;
   iFrame : Integer ;
   iSubROI : Integer ;
begin

      iFrame := 1 ;
      YMin := 1E30 ;
      YMax := -1E30 ;
      While iFrame <= MainFrm.IDRFile.NumFrames do begin
         // Get ROI intensity measurement
         ROIIntensity := ViewPlotFrm.ROIIntensity( iROI,iFrame,iFrameType ) ;
         iSubROI := Integer(cbSubtractROI.Items.Objects[cbSubtractROI.ItemIndex]) ;
         if  iSubROI <= MainFrm.IDRFile.MaxROI then begin
            ROIIntensity := ROIIntensity - ViewPlotFrm.ROIIntensity( iSubROI,iFrame,iFrameType ) ;
            end ;

         if ROIIntensity > YMax then YMax := ROIIntensity ;
         if ROIIntensity < YMin then YMin := ROIIntensity ;

         iFrame := iFrame + 1 ;

         end ;

      YMin := YMin*MainFrm.IDRFile.IntensityScale ;
      YMax := YMax*MainFrm.IDRFile.IntensityScale ;

      end ;


procedure TAVIFrm.UpdateRatioBitMap(
          RBM : TBitMap ;            // Fluorescence display bitmap [In/Out]
          PlotTop : Integer ;
          PlotHeight : Integer ;
          Frame : Integer ;            // Current frame
          StartFrame : Integer ;       // First frame in display sequence [IN]
          EndFrame : Integer          // Last frame in display sequence [IN]
          ) ;
// ----------------------------------------------------
// Add fluorescence ratio for this frame to FL display bitmap
// ----------------------------------------------------
var
     XScale : Single ;
     i,YBot,iPlot : Integer ;
     yNum,yDen,R,YScale,yThreshold : Single ;
     YName : String ;
     YMinLab : String ;
     YMaxLab : String ;
     YMin : Single ;
     YMax : Single ;
     YBottom : Integer ;
     YTop : Integer ;
     ChannelDisplaySpacing,ChannelDisplayHeight : Integer ;
     FrameType : Integer ;
     iSubROI : Integer ;
     Row : Integer ;
begin

     //if (EndFrame mod 2) = 0 then EndFrame := EndFrame + 1 ;

     // Set plot X scale factor
     XScale := (RBM.Width - LeftMargin) /
               ((EndFrame - StartFrame + 1)*MainFrm.IDRFile.FrameInterval) ;

     // Get number of displayed channels
     ChannelDisplaySpacing := PlotHeight ;
     ChannelDisplayHeight := ChannelDisplaySpacing - 2 ;

     // Set plot Y scale factors
     YBot := PlotTop + PlotHeight ;

     // Find row in calibration table
     Row := 0 ;
     for i := 1 to sgCalTable.RowCount do begin
         if Pos('Ratio',sgCalTable.Cells[0,i]) > 0 then Row := i ;
         end ;

     YName := ANSILeftStr(cbNumWave.Text,3) +'/'+ ANSILeftStr(cbDenWave.Text,3);

     // Set min-max plot range
     YMinLab := sgCalTable.Cells[CalLowCol,Row] ;
     YMin := ExtractFloat(YMinLab,0.0)/MainFrm.IDRFile.IntensityScale ;
     YMaxLab := sgCalTable.Cells[CalHighCol,Row] ;
     YMax := ExtractFloat(YMaxLab,0.0)/MainFrm.IDRFile.IntensityScale ;
     if YMin = YMax then YMax := YMin + 1.0 ;

     YBottom := YBot ;
     YTop := YBot - ChannelDisplayHeight ;
     YScale := ChannelDisplayHeight/(YMax - YMin) ;
     YBot := YBot - ChannelDisplaySpacing ;

     // Initialise display area
     if Frame = StartFrame then begin
        // Set background colour
        RBM.Canvas.Brush.Color := clBlack ;
        //RBM.Canvas.FillRect( RBM.Canvas.ClipRect ) ;

        // Set line colour
        RBM.Canvas.Pen.Color := clWhite ;

        // Set font
        RBM.Canvas.Font.Assign(FontDialog.Font) ;
        if RBM.Canvas.Font.Color = RBM.Canvas.Brush.Color then
           RBM.Canvas.Font.Color := RBM.Canvas.Pen.Color ;

        // Channel name

        // Add calibration text
        RBM.Canvas.TextOut( LeftMargin - RBM.Canvas.TextWidth(YMaxLab) - 3,
                            YTop,
                            YMaxLab) ;
        RBM.Canvas.TextOut( LeftMargin - RBM.Canvas.TextWidth(YMinLab) - 3,
                            YBottom - RBM.Canvas.TextHeight('X') - 1,
                            YMinLab) ;
        RBM.Canvas.TextOut( 0,
                            (YTop + YBottom - RBM.Canvas.TextHeight('X')) div 2,
                            YName ) ;

        // Draw calibration bar
        RBM.Canvas.MoveTo( LeftMargin - 5, YTop ) ;
        RBM.Canvas.LineTo( LeftMargin - 2, YTop ) ;
        RBM.Canvas.LineTo( LeftMargin - 2, YBottom ) ;
        RBM.Canvas.LineTo( LeftMargin - 5, YBottom ) ;
        end ;

     // Plot points

     iSubROI := Integer(cbSubtractROI.Items.Objects[cbSubtractROI.ItemIndex]) ;
     yThreshold := edRatioExclusionThreshold.Value ;
     for iPlot := 0 to NumPlots-1 do if PlotList[iPlot].ROIPlot then begin

         // Get numerator intensity
         yNum := ViewPlotFrm.ROIIntensity( PlotList[iPlot].Index,Frame,cbNumWave.ItemIndex ) ;
         if  iSubROI <= MainFrm.IDRFile.MaxROI then begin
            yNum := yNum - ViewPlotFrm.ROIIntensity( iSubROI,Frame,cbNumWave.ItemIndex ) ;
            end ;

         // Get denominator intensity
         yDen := ViewPlotFrm.ROIIntensity( PlotList[iPlot].Index,Frame,cbDenWave.ItemIndex ) ;
         if  iSubROI <= MainFrm.IDRFile.MaxROI then begin
            yDen := yDen - ViewPlotFrm.ROIIntensity( iSubROI,Frame,cbDenWave.ItemIndex ) ;
            end ;

         if yDen > yThreshold then r :=  yNum / yDen
                              else r := 0.0 ;

         R := Max( Min( R, YMax ),YMin );

         if PlotStarted[iPlot] then RBM.Canvas.MoveTo( X[iPlot], Y[iPlot] ) ;

         Y[iPlot] := YBottom
                     - Round((R - YMin)*YScale) ;
         X[iPlot] := LeftMargin +
                     Round(XScale*(Frame-StartFrame)*MainFrm.IDRFile.FrameInterval) ;

         if (X[iPlot] < RBM.Width) and PlotStarted[iPlot] then begin
            RBM.Canvas.Pen.Color := PlotList[iPlot].Color ;
            RBM.Canvas.LineTo( X[iPlot], Y[iPlot] ) ;
            RBM.Canvas.Pen.Color := clWhite ;
            end ;

         PlotStarted[iPlot] := True ;

         end ;

     end ;


procedure TAVIFrm.FindRatioRange(
          iROI : Integer ;
          var YMin : Single ;
          var YMax : Single
          ) ;
// --------------------------------------------------
// Find min/max range of fluorescence ROI time course
// --------------------------------------------------
var
   yNum,yDen,R,yThreshold : Single ;
   iSubROI,iFrame : Integer ;
begin

      YMin := 1E30 ;
      YMax := -1E30 ;

      iSubROI := Integer(cbSubtractROI.Items.Objects[cbSubtractROI.ItemIndex]) ;
      yThreshold := edRatioExclusionThreshold.Value ;

      for iFrame := 1 to MainFrm.IDRFile.NumFrames do begin

         // Get numerator intensity
         yNum := ViewPlotFrm.ROIIntensity( iROI,iFrame,cbNumWave.ItemIndex ) ;
         if  iSubROI <= MainFrm.IDRFile.MaxROI then begin
            yNum := yNum - ViewPlotFrm.ROIIntensity( iSubROI,iFrame,cbNumWave.ItemIndex ) ;
            end ;

         // Get denominator intensity
         yDen := ViewPlotFrm.ROIIntensity( iROI,iFrame,cbDenWave.ItemIndex ) ;
         if  iSubROI <= MainFrm.IDRFile.MaxROI then begin
            yDen := yDen - ViewPlotFrm.ROIIntensity( iSubROI,iFrame,cbDenWave.ItemIndex ) ;
            end ;

         if yDen > yThreshold then r :=  yNum / yDen
                              else r := 0.0 ;

         if R > YMax then YMax := R ;
         if R < YMin then YMin := R ;

         end ;

      end ;


procedure TAVIFrm.UpdateADCBitMap(
          ADCBM : TBitMap ;            // A/D display bitmap [In/Out]
          PlotTop : Integer ;
          PlotHeight : Integer ;
          Frame : Integer ;            // Current frame
          StartFrame : Integer ;       // First frame in display sequence [IN]
          EndFrame : Integer ;         // Last frame in display sequence [IN]
          FrameStep : Integer          // Spacing between frames [In]
          ) ;
// ----------------------------------------------------
// Add A/D samples for this frame to A/D display bitmap
// ----------------------------------------------------
var
     XScale,T : Single ;
     YBot,ch : Integer ;
     YScale : Array[0..MaxPlots-1] of Single ;
     YMin : Array[0..MaxPlots-1] of Single ;
     YMax : Array[0..MaxPlots-1] of Single ;
     YMinLab : Array[0..MaxPlots-1] of String ;
     YMaxLab : Array[0..MaxPlots-1] of String ;
     YName : Array[0..MaxPlots-1] of String ;
     YTop : Array[0..MaxPlots-1] of Integer ;
     YBottom : Array[0..MaxPlots-1] of Integer ;
     i,j,NumScansPerFrame : Integer ;
     iPlot : Integer ;
     ADCBuf : PSmallIntBuf ;
     ChannelDisplayHeight,ChannelDisplaySpacing : Integer ;
     NumScansToPlot : Integer ;
     Row : Integer ;
     s : String ;
begin

     //if (EndFrame mod 2) = 0 then EndFrame := EndFrame + 1 ;

     XScale := (ADCBM.Width - LeftMargin) / ((EndFrame - StartFrame + 1)*MainFrm.IDRFile.FrameInterval) ;

     ChannelDisplaySpacing := (PlotHeight div NumADCPlots) ;
     ChannelDisplayHeight := ChannelDisplaySpacing - 2 ;

     YBot := PlotTop + PlotHeight -1 ;
     for iPlot := 0 to NumPlots-1 do if not PlotList[iPlot].ROIPlot then begin

         // Find row in calibration table
         ch := PlotList[iPlot].Channel ;
         Row := 0 ;
         for i := 1 to sgCalTable.RowCount do begin
            if Pos( MainFrm.IDRFile.ADCChannel[ch].ADCName,
                    sgCalTable.Cells[0,i]) > 0 then Row := i ;
            end ;

         YName[iPlot] := sgCalTable.Cells[CalNameCol,Row] ;

         // Set min-max limits of plot
         YMinLab[iPlot] := sgCalTable.Cells[CalLowCol,Row] ;
         YMin[iPlot] := Round( ExtractFloat(YMinLab[iPlot],0.0)/
                               MainFrm.IDRFile.ADCChannel[ch].ADCScale ) ;
         YMaxLab[iPlot] := sgCalTable.Cells[CalHighCol,Row] ;
         YMax[iPlot] := Round( ExtractFloat(YMaxLab[iPlot],0.0)/
                               MainFrm.IDRFile.ADCChannel[ch].ADCScale ) ;
         if YMin[iPlot] = YMax[iPlot] then YMax[iPlot] := YMin[iPlot] + 1 ;

         // Plot scaling factor
         YScale[iPlot] := (ChannelDisplayHeight -1)/(YMax[iPlot] - YMin[iPlot]) ;

         YBottom[iPlot] := YBot ;
         YTop[iPlot] := YBot - ChannelDisplayHeight ;

         YBot := YBot - ChannelDisplaySpacing ;

         end ;

     // No. of A/D scans
     NumScansPerFrame := Round(MainFrm.IDRFile.FrameInterval/MainFrm.IDRFile.ADCScanInterval) ;
     NumScansToPlot := NumScansPerFrame*FrameStep ;
     // Allocate buffer
     GetMem( ADCBuf, NumScansToPlot*MainFrm.IDRFile.ADCNumChannels*2 ) ;
     // Load buffer
     MainFrm.IDRFile.LoadADC( Frame*NumScansPerFrame,
                              NumScansToPlot,
                              ADCBuf^ ) ;

     // Initialise A/D display area
     if Frame = StartFrame then begin
        // Set background colour
        ADCBM.Canvas.Brush.Color := clBlack ;
//        ADCBM.Canvas.FillRect( ADCBM.Canvas.ClipRect ) ;

        // Set line colour
        ADCBM.Canvas.Pen.Color := clWhite ;

        // Set font
        ADCBM.Canvas.Font.Assign(FontDialog.Font) ;
        if ADCBM.Canvas.Font.Color = ADCBM.Canvas.Brush.Color then
           ADCBM.Canvas.Font.Color := ADCBM.Canvas.Pen.Color ;

        // Set labels
        for iPlot := 0 to NumPlots-1 do if not PlotList[iPlot].ROIPlot then begin

            // Add calibration text
            ADCBM.Canvas.TextOut( LeftMargin - ADCBM.Canvas.TextWidth(YMaxLab[iPlot]) - 3,
                                  YTop[iPlot],
                                  YMaxLab[iPlot]) ;
            ADCBM.Canvas.TextOut( LeftMargin - ADCBM.Canvas.TextWidth(YMinLab[iPlot]) - 3,
                                  YBottom[iPlot] - ADCBM.Canvas.TextHeight('X') - 1,
                                  YMinLab[iPlot]) ;
            ADCBM.Canvas.TextOut( 0,
                                 (YTop[iPlot] + YBottom[iPlot]
                                  - ADCBM.Canvas.TextHeight('X')) div 2,
                                  YName[iPlot] ) ;

            // Draw calibratin bar
            ADCBM.Canvas.MoveTo( LeftMargin - 5, YTop[iPlot] ) ;
            ADCBM.Canvas.LineTo( LeftMargin - 2, YTop[iPlot] ) ;
            ADCBM.Canvas.LineTo( LeftMargin - 2, YBottom[iPlot] ) ;
            ADCBM.Canvas.LineTo( LeftMargin - 5, YBottom[iPlot] ) ;

            end ;

        end ;

     // Plot points associated with this frame
     for iPlot := 0 to NumPlots-1 do if not PlotList[iPlot].ROIPlot then begin

         // A/D channel
         ch := PlotList[iPlot].Channel ;

         T := (Frame-StartFrame)*MainFrm.IDRFile.FrameInterval ;

         if PlotStarted[iPlot] then ADCBM.Canvas.MoveTo( X[iPlot], Y[iPlot] ) ;

         for i := 0 to NumScansPerFrame-1 do begin
             j := i*MainFrm.IDRFile.ADCNumChannels + MainFrm.IDRFile.ADCChannel[ch].ChannelOffset ;
             X[iPlot] := LeftMargin +
                         Round(XScale*(T + i*MainFrm.IDRFile.ADCScanInterval)) ;
             Y[iPlot] := YBottom[iPlot] - Round((ADCBuf^[j] - YMin[iPlot])*YScale[iPlot]) ;
             if (X[iPlot] < ADCBM.Width) and
                (Y[iPlot] < ADCBM.Height) and
                (Y[iPlot] >= 0) and
                PlotStarted[iPlot] then ADCBM.Canvas.LineTo( X[iPlot], Y[iPlot] )
                                   else ADCBM.Canvas.MoveTo( X[iPlot], Y[iPlot] ) ;
             PlotStarted[iPlot] := True ;
             end ;
         end ;

     // Free buffer
     FreeMem(ADCBuf) ;

     end ;


procedure TAVIFrm.FindADCRange(
          iChan : Integer ;
          var YMin : Single ;
          var YMax : Single
          ) ;
// --------------------------------------------------
// Find min/max range of fluorescence ROI time course
// --------------------------------------------------
var
   i,j : Integer ;
   Y : Single ;
   ADCBuf : PSmallIntBuf ;
begin

     // Allocate buffer
     GetMem( ADCBuf, MainFrm.IDRFile.ADCNumScansInFile*MainFrm.IDRFile.ADCNumChannels*2 ) ;

     // Load A/D data buffer
     MainFrm.IDRFile.LoadADC( 0,
                              MainFrm.IDRFile.ADCNumScansInFile,
                              ADCBuf^ ) ;


      YMin := 1E30 ;
      YMax := -1E30 ;
      j := MainFrm.IDRFile.ADCChannel[iChan].ChannelOffset ;
      for i := 0 to MainFrm.IDRFile.ADCNumScansInFile-1 do begin
          Y := ADCBuf^[j] ;
          if Y > YMax then YMax := Y ;
          if Y < YMin then YMin := Y ;
          j := j + MainFrm.IDRFile.ADCNumChannels ;
          end ;

      YMin := YMin*MainFrm.IDRFile.ADCChannel[iChan].ADCScale ;
      YMax := YMax*MainFrm.IDRFile.ADCChannel[iChan].ADCScale ;

      end ;


procedure TAVIFrm.UpdatePlotCalibrationTable ;
var
    i,iPlot,Row,iChan : Integer ;
    RowExists : Boolean ;
    YMin : Array[1..20] of Single ;
    YMax : Array[1..20] of Single ;
begin

     sgCalTable.RowCount := 1 ;
     for i := 1 to High(YMax) do begin
         YMax[i] := -1E30 ;
         YMin[i] := 1E30 ;
         end ;

     for iPlot := 0 to NumPlots-1 do begin

         // Does entry for this plot already exist?
         Row := -1 ;
         for i := 1 to sgCalTable.RowCount-1 do if
             Pos(PlotList[iPlot].Name,sgCalTable.Cells[CalNameCol,i]) > 0 then Row := i ;

         // Add new entry
         if Row <= 0 then begin
            Row := sgCalTable.RowCount ;
            sgCalTable.RowCount := sgCalTable.RowCount + 1 ;
            sgCalTable.FixedRows := 1 ;
            sgCalTable.Cells[CalNameCol,Row] := PlotList[iPlot].Name + ' '
                                                + PlotList[iPlot].Units ;
            end ;

         // Update min-max range
         YMin[Row] := Min( YMin[Row],PlotList[iPlot].YMin ) ;
         YMax[Row] := Max( YMax[Row],PlotList[iPlot].YMax ) ;

         sgCalTable.Cells[CalLowCol,Row] := format( '%.5g',[YMin[Row]]) ;
         sgCalTable.Cells[CalHighCol,Row] := format( '%.5g',[YMax[Row]]) ;

         end ;

     end ;


procedure TAVIFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     Action := caFree ;
     end;


procedure TAVIFrm.bCancelClick(Sender: TObject);
// ---------------------------------
// Abort AVI production / close form
// ---------------------------------
begin
     if bOK.Enabled then begin
        Close ;
        end
     else begin
        bOK.Enabled := True ;
        end ;

     end ;


procedure TAVIFrm.bChangeNameClick(Sender: TObject);
// -----------------------------------
// Change name / folder of AVI file
// -----------------------------------
begin

     SaveDialog.InitialDir := ExtractFilePath( edFileName.Text ) ;
     SaveDialog.Title := 'AVI File ' ;
     SaveDialog.options := [ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.DefaultExt := ExtractFileExt( edFileName.Text ) ;
     SaveDialog.Filter := ' AVI Movie (*.avi)|*.avi|' ;
     if MainFrm.DataDirectory <> '' then SaveDialog.InitialDir := MainFrm.DataDirectory ;

     SaveDialog.FileName := edFileName.Text ;
     if SaveDialog.Execute then edFileName.Text := SaveDialog.FileName ;

     end ;



procedure TAVIFrm.bSetFontClick(Sender: TObject);
// --------------------
// Change AVI text font
// --------------------
begin
     FontDialog.Execute ;

     edFont.Text := format( '%s %d pts.',
                            [FontDialog.Font.Name,FontDialog.Font.Size] ) ;
     edFont.Font.Assign(FontDialog.Font) ;
     end;


procedure TAVIFrm.bAddPlotClick(Sender: TObject);
// ------------------------------
// Add time course plot to movie
// ------------------------------
var
    i : Integer ;
begin

    if cbROI.Items.Count < 1 then Exit ;
    if NumPlots >= MaxPlots then Exit ;
    if NumROIsAdded >= 4 then Exit ;

    // Exit if item already in plot
    for i:= 0 to mePlotList.Lines.Count-1 do
        if cbROI.text = mePlotList.Lines[i] then Exit ;

    bAddPlot.Enabled := False ;

    if Pos( 'ROI', cbROI.text ) > 0 then begin
       // ROI plots
       if rbFluorescence.Checked then begin
          // Raw fluorescence plots
          for i := 0 to NumFrameTypes-1 do if FrameTypeSelected(i) then begin
              PlotList[NumPlots].ROIPlot := True ;
              PlotList[NumPlots].FrameType := i ;
              PlotList[NumPlots].Name :=  MainFrm.IDRFile.FrameType[i] ;
              PlotList[NumPlots].Units := '' ;
              PlotList[NumPlots].Index := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
              PlotList[NumPlots].Color := ROIColor[NumROIsAdded] ;
              FindFLRange( PlotList[NumPlots].FrameType,
                           PlotList[NumPlots].Index,
                           PlotList[NumPlots].YMin,
                           PlotList[NumPlots].YMax ) ;
              Inc(NumPlots) ;
              end ;
           end
       else begin
           // Fluorescence ratio plot
           PlotList[NumPlots].ROIPlot := True ;
           PlotList[NumPlots].FrameType := i ;
           PlotList[NumPlots].Name :=  'Ratio' ;
           PlotList[NumPlots].Units := '' ;
           PlotList[NumPlots].Index := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
           PlotList[NumPlots].Color := ROIColor[NumROIsAdded] ;
           FindRatioRange( PlotList[NumPlots].Index,
                              PlotList[NumPlots].YMin,
                              PlotList[NumPlots].YMax ) ;
           Inc(NumPlots) ;
           end ;

       Inc(NumROIsAdded) ;
       end
    else begin
       // A/D channel plots
       PlotList[NumPlots].ROIPlot := False ;
       PlotList[NumPlots].Channel := Integer(cbROI.Items.Objects[cbROI.ItemIndex]) ;
       PlotList[NumPlots].Name := MainFrm.IDRFile.ADCChannel[PlotList[NumPlots].Channel].ADCName ;
       PlotList[NumPlots].Units := MainFrm.IDRFile.ADCChannel[PlotList[NumPlots].Channel].ADCUnits ;
       FindADCRange( PlotList[NumPlots].Channel,
                     PlotList[NumPlots].YMin,
                     PlotList[NumPlots].YMax ) ;
       Inc(NumPlots) ;
       end ;

    mePlotList.Lines.Add( cbROI.text ) ;

    // Update table of calibration labels
    UpdatePlotCalibrationTable ;

    bAddPlot.Enabled := True ;

    end;


procedure TAVIFrm.bClearPlotsClick(Sender: TObject);
// --------------------------
// Clear all plots from movie
// --------------------------
begin

     NumROIsAdded := 0 ;
     NumPlots := 0 ;
     mePlotList.Clear ;
     sgCalTable.RowCount := 1 ;

     // Update table of calibration labels
     UpdatePlotCalibrationTable ;

     end;


procedure TAVIFrm.cbSubtractROIChange(Sender: TObject);
// ------------------------
// Subtraction ROI changed
// ------------------------
var
    iPlot : Integer ;
begin

     for iPlot := 0 to NumPlots-1 do if PlotList[iPlot].ROIPlot then begin
         // Find min-max data range
         if rbFluorescence.Checked then begin
              FindFLRange( PlotList[NumPlots].FrameType,
                           PlotList[NumPlots].Index,
                           PlotList[NumPlots].YMin,
                           PlotList[NumPlots].YMax ) ;
              end
         else begin
              FindRatioRange( PlotList[NumPlots].Index,
                              PlotList[NumPlots].YMin,
                              PlotList[NumPlots].YMax ) ;
              end ;
         // Update table of calibration labels
         UpdatePlotCalibrationTable ;
         end ;

     end;


procedure TAVIFrm.FormResize(Sender: TObject);
begin
     FileNameGrp.Width := ClientWidth - FileNameGrp.Left - 5 ;
     edFileName.Width := FileNameGrp.ClientWidth - edFileName.Left - 5 ;
     PlotChannelGrp.Width := ClientWidth - PlotChannelGrp.Left - 5 ;
     CalTableGrp.Width := PlotChannelGrp.ClientWidth - CalTableGrp.Left - 5 ;
     TextGrp.Width := PlotChannelGrp.ClientWidth - TextGrp.Left - 5 ;
     edFont.Width := TextGrp.ClientWidth - edFont.Left - 5 ;

     RatioPanel.Visible := not rbFluorescence.Checked ;
     FLPanel.Visible := rbFluorescence.Checked ;

     end;

procedure TAVIFrm.Wait( Time : Single ) ;
//
// Wait for fixed period
//
var
    WaitTill : Cardinal ;
begin
    WaitTill := TimeGetTime + Round(Time*1000) ;
    repeat
        Application.ProcessMessages ;
        until TimeGetTime >= WaitTill ;
    end ;

    
procedure TAVIFrm.bOKClick(Sender: TObject);
// ---------------------------------
// Write selected frames to AVI file
// ---------------------------------
const
    MarginPixels = 2 ;
var
     Err,i,l,j,k : Integer ;
     pLine : Pointer ;
     Frame,FrameStep,iFrameType : Integer ;
     StartAtFrame,EndAtFrame : Integer ;
     FileName : String ;          // AVI file name

     AVIFIle : Pointer ;          // Pointer to AVI file

     BM : TBitMap ;
//     NumFrameTypes : Integer ;                 // No. of frame types in use
     xLeft,yTop : Integer ;
     Pstream : PAVISTREAM;
     StreamInfo		: TAVIStreamInfo;
     BitmapInfo		: PBitmapInfoHeader;
     BitmapInfoSize	: DWORD ;
     BitmapSize		: DWORD ;
     BitmapBits		: pointer;
     Samples_Written       : longInt;
     Bytes_Written         : longInt;
     nstreams              : integer;
     Streams : APAVISTREAM;
     CompOptions : APAVICompressOptions;
     refcount  : integer;
     VideoStream    : PAVISTREAM;
     AudioStream    : PAVISTREAM;

     AVIERR  : integer;
     AVIFrame : integer;
     Flags : Cardinal ;
     Done : Boolean ;
     FirstFrame : Boolean ;
     ImageRows : Integer ;
     ImageColumns : Integer ;
     PlotHeight : Integer ;
     s : string ;
     ROI : TROI ;
     NumROIs : Integer ;
     iPlot : Integer ;
     xTimeLabel,yTimeLabel : Integer ;
     PixelFormat : TPixelFormat ;
     Row,Col : Integer ;
     AVIWidth,AVIHeight : Integer ;
     FLPlotTop,FLPlotHeight : Integer ;
     ADCPlotTop,ADCPlotHeight : Integer ;
     NumPlotsTotal : Integer ;
begin

     // Let user cancel if file already exists
     if FileExists( edFileName.Text ) then begin
        if MessageDlg( format(
           'File %s already exists! DO you want to overwrite it? ',[edFileName.Text]),
           mtWarning,[mbYes,mbNo], 0 ) = mrNo then Exit ;
        end ;

     FileName := edFileName.text ;
     if FileExists( Filename ) then begin
        if not DeleteFile(PChar(FileName)) then begin
           ShowMessage('Unable to delete ' + FileName);
           Close ;
           Exit ;
           end ;

        Wait(1.0) ;
        end ;

     bOK.Enabled := False ;
     Application.ProcessMessages ;

     AudioStream := nil;
     VideoStream := nil;
     BitmapInfo := nil;
     BitmapBits := nil;
     CombinedBitMap := Nil ;
     ImageBitMap := Nil ;

     // Select range of frames to be plotted
     if rbAllFrames.Checked then begin
        StartAtFrame := Round(edRange.LoLimit) ;
        EndAtFrame := Round(edRange.HiLimit) ;
        end
     else begin
        StartAtFrame := Round(edRange.LoValue) ;
        EndAtFrame := Round(edRange.HiValue ) ;
        end ;
     if ((EndAtFrame - StartAtFrame) mod 2) = 1 then Dec(EndAtFrame) ;

     // Initialise AVI file DLL
     AVIFileInit;

     // Open AVI file for write
     if (AVIFileOpen(AVIFIle, pchar(FileName),
        OF_WRITE or OF_CREATE OR OF_SHARE_EXCLUSIVE, nil) <> AVIERR_OK) then begin
        ShowMessage('Create Movie: Failed to create file: ' + FileName ) ;
        Exit ;
        end ;

     // Create image bitmap
     ImageBitmap := TBitmap.create ;
     MainFrm.SetPalette( ImageBitmap, MainFrm.PaletteType ) ;
     ImageBitmap.Width := ViewFrm.BitMaps[0].Width ;
     ImageBitmap.Height := ViewFrm.BitMaps[0].Height ;
     ImageBitmap.Canvas.Font.Assign(FontDialog.Font) ;
     ImageBitmap.Canvas.Font.Color := clWhite ;
     ImageBitmap.Canvas.Brush.Style := bsClear ;
     ImageBitMap.Canvas.Pen.Mode := pmXOR ;
     ImageBitmap.Canvas.TextFlags := 0 ;
     ImageBitmap.HandleType := bmDIB ;
     ImageBitmap.PixelFormat :=pf8bit ;
     ImageBitmap.Dormant ;

     // Determine number of image columns and rows
     ImageRows := Round(Sqrt(NumFrameTypes)) ;
     ImageColumns := ImageRows ;
     if (ImageRows*ImageColumns) < NumFrameTypes then Inc(ImageColumns) ;

     AVIWidth := ImageBitmap.Width*ImageColumns ;
     AVIHeight := ImageBitmap.Height*ImageRows + 2 ;

     // Set height of plotting area
     if (NumROIPlots > 0) or (NumADCPlots > 0) then begin
        PlotHeight := Round(AVIHeight*edPlotArea.Value) ;
        NumPlotsTotal := NumROIPlots + NumADCPlots ;
        PlotHeight := Max( PlotHeight, NumPlotsTotal*2 ) ;

        if NumADCPlots > 0 then begin
           ADCPlotHeight := (PlotHeight div NumPlotsTotal)*NumADCPlots ;
           ADCPlotTop := AVIHeight ;
           AVIHeight := AVIHeight + ADCPlotHeight + 1;
           end ;

        if NumROIPlots > 0 then begin
           FLPlotHeight := (PlotHeight div NumPlotsTotal)*NumFrameTypes ;
           FLPlotTop := AVIHeight ;
           AVIHeight := AVIHeight + FLPlotHeight + 1;
           end ;

        // Add space at left for calibration bars
        LeftMargin := 0 ;
        for Row := 1 to sgCalTable.RowCount do
            for Col := 0 to sgCalTable.ColCount do
               LeftMargin := Max( LeftMargin,
                                  ImageBitmap.Canvas.TextWidth(sgCalTable.Cells[Col,Row]) ) ;
        LeftMargin := LeftMargin + ImageBitmap.Canvas.TextWidth('X') + 5 ;
        AVIWidth := AVIWidth + LeftMargin ;
        end ;

     // Add space for time counter label
     yTimeLabel := AVIHeight + 1 ;
     xTimeLabel := AVIWidth - ImageBitmap.Canvas.TextWidth('9')*8 ;
     AVIHeight := yTimeLabel + ImageBitmap.Canvas.TextHeight('9')+1 ;

     // Create combined images/plots bitmap
     CombinedBitmap := TBitmap.create ;
     CombinedBitmap.PixelFormat :=pf24bit ;
     CombinedBitmap.Width := AVIWidth ;
     CombinedBitmap.Height := AVIHeight ;
     CombinedBitmap.Canvas.Font.Assign(FontDialog.Font) ;
     CombinedBitmap.Canvas.Font.Color := clWhite ;
     CombinedBitmap.Canvas.Brush.Style := bsClear ;
     CombinedBitmap.Canvas.Brush.Color := clBlack ;
     CombinedBitmap.Canvas.TextFlags := 0 ;
     CombinedBitmap.HandleType := bmDIB ;
     CombinedBitmap.Dormant ;

     // Erase bit map to black
     CombinedBitMap.canvas.Brush.Color := clBlack ;
     CombinedBitMap.canvas.Brush.Style := bsSolid ;
     CombinedBitMap.canvas.Pen.Color := clWhite ;
     CombinedBitMap.Canvas.FillRect( CombinedBitMap.Canvas.ClipRect ) ;
     CombinedBitmap.Canvas.Brush.Style := bsClear ;

     // Determine size of DIB
     GetDIBSizes( CombinedBitmap.Handle,
                  BitmapInfoSize,
                  BitmapSize);

     // Create DIB header and pixel buffers
     GetMem(BitmapInfo, BitmapInfoSize);
     GetMem(BitmapBits, BitmapSize);

     if (BitmapInfoSize = 0) then begin
        ShowMessage('Create Movie: Failed to retrieve bitmap info') ;
        end ;

     // Set frame rate and scale
     FillChar(StreamInfo, sizeof(StreamInfo), 0);
     StreamInfo.dwRate := 1000;
     StreamInfo.dwScale := Round(1000.0/edPlayBackRate.Value) ;
     StreamInfo.fccType := streamtypeVIDEO;
     StreamInfo.fccHandler := 0;
     StreamInfo.dwFlags := 0;
     StreamInfo.dwSuggestedBufferSize := 0;
     StreamInfo.rcFrame.Right := CombinedBitmap.width-1;
     StreamInfo.rcFrame.Bottom := CombinedBitmap.height-1;

     // Open AVI data stream
     if (AVIFileCreateStream(AVIFIle, pStream, StreamInfo) <> AVIERR_OK) then begin
        ShowMessage('Create Movie: Failed to create AVI video stream') ;
        end ;

     // Clear time course position counters
     for i := 0 to MaxPlots-1 do begin
         X[i] := 0 ;
         Y[i] := 0 ;
         PlotStarted[i] := False ;
         end ;

     Frame := StartAtFrame ;
     Done := False ;
     FirstFrame := True ;
     FrameStep := Round(edFrameStep.Value+1) ;
     AVIFrame :=  0 ;
     while not Done do begin

         // Select frame(s) to added to AVI
         ViewFrm.FrameNumber := Frame ;

         // Wait for frames to be displays
         Repeat
            Application.ProcessMessages ;
            until ViewFrm.FrameNumber = Frame ;

         for iFrameType := 0 to NumFrameTypes-1 do begin

            // Copy bitmap from display form
            MainFrm.SetPalette( ImageBitMap, MainFrm.PaletteType ) ;
            ImageBitMap.Canvas.Draw(0,0,ViewFrm.BitMaps[iFrameType]) ;

            // Add ROIs
            ImageBitMap.Canvas.Brush.Style := bsClear ;
            for iPlot := 0 to NumPlots-1 do if PlotList[iPlot].ROIPlot {and
                (PlotList[iPlot].FrameType = iFrameType)} then begin
                ImageBitMap.Canvas.Pen.Color := PlotList[iPlot].Color ;
                ImageBitMap.Canvas.Font.Color := PlotList[iPlot].Color ;
                ViewFrm.ScaleROI( MainFrm.IDRFile.ROI[PlotList[iPlot].Index], ROI ) ;
                ViewFrm.DrawROI(ROI,PlotList[iPlot].Index,ImageBitMap.Canvas);
                end ;
            ImageBitMap.Canvas.Pen.Color := clWhite ;
            ImageBitMap.Canvas.Font.Color := clWhite ;

            // Add frame type label
            ImageBitMap.Canvas.TextOut( 0,0, MainFrm.IDRFile.FrameType[iFrameType] ) ;

            // Add bitmap to AVI bitmap
            xLeft := LeftMargin +
                     (iFrameType mod ImageColumns)*
                     (ImageBitMap.Width + MarginPixels) ;
            yTop := (iFrameType div ImageColumns)*
                    (ImageBitMap.Height + MarginPixels) ;
            CombinedBitmap.Canvas.Draw( xLeft, yTop, ImageBitMap ) ;

            // Plot ROI time course
            if NumROIPlots > 0 then begin
               if rbFluorescence.Checked then begin
                  UpdateFLBitMap( CombinedBitMap,
                                  FLPlotTop,
                                  FLPlotHeight,
                                  Frame+iFrameType,
                                  StartAtFrame,
                                  EndAtFrame ) ;
                  end
               else begin
                  UpdateRatioBitMap( CombinedBitMap,
                                     FLPlotTop,
                                     FLPlotHeight,
                                     Frame+iFrameType,
                                     StartAtFrame,
                                     EndAtFrame ) ;
                  end ;
               end ;

            end ;

         // Plot A/D time course
         if NumADCPlots > 0 then begin
            UpdateADCBitMap( CombinedBitMap,
                             ADCPlotTop,
                             ADCPlotHeight,
                             Frame,
                             StartAtFrame,
                             EndAtFrame,
                             FrameStep ) ;
            end ;

         // Display time counter
         s := format(' %7.2f s',[Frame*MainFrm.IDRFile.FrameInterval]) ;
         xTimeLabel := AVIWidth - ImageBitmap.Canvas.TextWidth('9')*10 ;
         CombinedBitmap.Canvas.Brush.Color := clBlack ;
         CombinedBitmap.Canvas.Font.Color := clWhite ;
         CombinedBitmap.Canvas.TextOut( xTimeLabel,yTimeLabel, s ) ;

         // Get bitmap info header
         InitializeBitmapInfoHeader( CombinedBitmap.Handle,
                                     BitmapInfo^,
                                     CombinedBitmap.PixelFormat ) ;

         // Copy bitmap data
         l := 0 ;
         for i := CombinedBitmap.Height-1 downto 0 do begin
             pLine := CombinedBitmap.ScanLine[i] ;
             k := l*AlignBit(CombinedBitmap.Width, 24, 32);
             for j := 0 to CombinedBitmap.Width*3-1 do begin
                 PByteArray(BitmapBits)^[k] := PByteArray(pLine)^[j] ;
                 Inc(k) ;
                 end ;
             Inc(l) ;
             end ;

         // On the first time through, set the stream format.
         if FirstFrame then begin

            if (AVIStreamSetFormat(pStream, 0, BitmapInfo, BitmapInfoSize) <> AVIERR_OK) then begin
               ShowMessage('Create Movie: Failed to set stream format') ;
               Done := True ;
               end ;
            FirstFrame := False ;
            end ;

         // Write frame to the video stream
         if (AVIFrame Mod 100) = 0 then Flags := AVIIF_KEYFRAME
                                   else Flags := 0 ;

         if AVIStreamWrite( pStream,
                             AVIFrame,
                             1,
                             BitmapBits,
                             BitmapSize,
                             AVIIF_KEYFRAME,
                             Samples_Written,
                             Bytes_Written) <> AVIERR_OK then begin
             ShowMessage('Create Movie: Failed to add frame to AVI file stream') ;
             Done := True ;
             end ;
         Inc(AVIFrame) ;

         // Report progress
         MainFrm.StatusBar.SimpleText :=
         format(' Create Movie: Creating frame %d/%d',[Frame,EndAtFrame]) ;

         Frame := Frame + FrameStep ;
         if (Frame > EndAtFrame) or bOK.Enabled then Done := True ;

         end ;

     // Create the editable VideoStream from pStream.
     if CreateEditableStream(VideoStream,pStream) <> AVIERR_OK then begin
        ShowMessage('Create Movie: Could not create Video Stream') ;
        end ;

     AviStreamRelease(pStream);

     MainFrm.StatusBar.SimpleText := format('Create Movie:  Writing to AVI file %s',[FileName]) ;

     if assigned(VideoStream) then AviStreamRelease(VideoStream);
     if assigned(AudioStream) then AviStreamRelease(AudioStream);

     repeat refcount := AviFileRelease(AVIFile) until refcount <= 0;

     if CombinedBitmap <> nil then CombinedBitmap.Free;
     if ImageBitmap <> nil then ImageBitmap.Free;
     if (BitmapInfo <> nil) then FreeMem(BitmapInfo);
     if (BitmapBits <> nil) then FreeMem(BitmapBits);

     AviFileExit;     MainFrm.StatusBar.SimpleText := ' Create Movie: File ' + FileName + ' created.' ;


     Close ;

     end;


procedure TAVIFrm.rbFLuorescenceClick(Sender: TObject);
// ------------------------------------------
// Fluoresence selected for plotting in movie
// ------------------------------------------
begin
     FLPanel.Visible := rbFLuorescence.Checked ;
     RatioPanel.Visible := not FLPanel.Visible ;
     end;

procedure TAVIFrm.rbRatioClick(Sender: TObject);
// ------------------------------------------
// Fluoresence selected for plotting in movie
// ------------------------------------------
begin
     FLPanel.Visible := rbFLuorescence.Checked ;
     RatioPanel.Visible := not FLPanel.Visible ;
     end;


end.
