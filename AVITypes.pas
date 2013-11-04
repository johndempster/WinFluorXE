unit AVITypes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ActiveX,
  ExtCtrls ;

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
  AVIIF_KEYFRAME  = $10;

  streamtypeVIDEO = $73646976; // DWORD( 'v', 'i', 'd', 's' )
  streamtypeAUDIO = $73647561; // DWORD( 'a', 'u', 'd', 's' )



type

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
      ROIPlot : Boolean ;
      Index : Integer ;
      Channel : Integer ;
      FrameType : Integer ;
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




//type
// TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit,
//                     pf24bit, pf32bit, pfCustom);


implementation

end.
