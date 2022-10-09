unit UltimaUnit;
// --------------------------------------------------------
// Praire Technologie Ultima 2P microscope interface module
// --------------------------------------------------------
// (c) J. Dempster, University of Strathclyde, 2005
// 17.11.05 V1.0
// 27.06.06 V1.1 Now supports multi-channel line scans (Prairie-View 2.1.0.3 and later)
// 25.06.07 LSD file now opened AFTER end of line scan. Temporary IDR file used to
//          hold data until final file name determined from LSD file after end of scan
//          (Praire-View V2.5)
// 27.12.07 Linescan data now obtained from XML record when available
// 08.01.08 Now imports repeated line scans
// 09.06.08 Now works with Prairie-View V3.0
// 05.08.08 LineRotation now stored in radian units
// 14.06.08 NS - Now works with Prairie-View V3.3.7.16
// 21.11.12 JD Updated to work with Prairie-View 4.3.2
//          Cycle number in file name had been increased from 3 to 5 digits. WinFluor now determines number of digits
//          in cycle number from SEQUENTIAL_ON message sent over TCP/IP by PraireView.
//          PraireView log file is now named prairieview.log rather than prarieview.log as in pre V4.3 versions
//          Old name is used if unable to open new name
// 29.01.14 Updated to Compile under both 32/64 bits (File handle now THandle)

interface                              

uses
  SysUtils, Classes, Sockets, ExtCtrls, mmsystem, strutils,
  ImageFile, dialogs, math, xmldom, XMLIntf, msxmldom, XMLDoc, windows, idrfile ;

type
  TUltima = class(TDataModule)
    TCPClient: TTcpClient;
    Timer: TTimer;
    ImageFile: TImageFile;
    XMLDOC: TXMLDocument;
    XMLState: TXMLDocument;
    XMLSingleImage: TXMLDocument;
    procedure TimerTimer(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    ImportImageTime : Cardinal ;
    TimerMethodExecuting : Boolean ;   // TRUE = Timer method currently executing
    NumLineScanSweeps : Integer ;
    ReadyToRead : Boolean ;
    NumCycleDigits : Integer ;

    procedure Wait( Delay : Single ) ;
    function GetLSDFileParameters(
             LSDFileName : String ;
             var Buf : Array of ANSIChar
              ) : Integer ;

   function GetNumberOfFramesInSequenceXML(
            iSequence : Integer
             ) : Integer;
   function GetNumberOfSequencesXML : Integer;

   function GetPrairieViewVersion : Single ;


   function GetFileNames(
            iSelectedFrame : Integer ;
            var FileNames : Array of String ; // List of file names
            var PMTChannels : Array of Integer ;
            var NumFiles : Integer            // No. of files
           ) : Boolean ;

   function GetFileNamesInFrame(
         iSequence : Integer ;
         iFrame : Integer ;
         var FileNames : Array of String ; // List of file names
         var PMTChannels : Array of Integer ;
         var NumFiles : Integer            // No. of files
         ) : Boolean ;


   function GetKey(
         iSequence : Integer ;
         iFrame : Integer ;
         KeyName : String
         ) : Single ;

    function FindXMLNode(
             const ParentNode : IXMLNode ;
             NodeName : String ;
             var ChildNode : IXMLNode
             ) : Boolean ;

    function GetStateXMLKey(
             var Node : IXMLNode ;
             KeyName : String ) : String ;


    function ImportImageFileLSD : Boolean ;           // LSD file version
    function ImportImageFileXML_V3 : Boolean ;           // XML file version
    function ImportImageFileXML_V2_5 : Boolean ;           // XML file version

    procedure ProcessNewImage ; // Process a NEWIMAGE command from PrairieView

    // Read a file backwards, line by line
    procedure Readback (const f: integer; var Line: String; var _bof: boolean);

    function CycleNumber( iNum : Integer ) : string ;

    procedure ReadFloat(
            const Source : array of ANSIChar;
            Keyword : ANSIstring ;
            var Value : Single ) ;

    function ExtractFloat (
             CBuf : ANSIstring ;
             Default : Single
             ) : extended ;


  procedure ReadInt(
            const Source : array of ANSIChar;
            Keyword : ANSIstring ;
            var Value : LongInt
            ) ;

  procedure ReadString(
            const Source : Array of ANSIChar;
            Keyword : ANSIstring ;
            var Value : string
            ) ;

  procedure FindParameter( const Source : array of ANSIChar ;
                               Keyword : ANSIstring ;
                               var Parameter : ANSIstring ) ;

  Function ExtractInt ( CBuf : ANSIstring ) : LongInt ;

  public
    { Public declarations }

    DataDirectory : String ;          // Prairie-View line scan data storage folder
    XMLFileName : String ;           // Prairie-View XML file name
    StateXMLFileName : String ;
    BaseFileName : String ;           // Prairie-View Line scan base file name
    TempFileName : String ;           // Temporary IDR/EDR file name for line scan data
    LineScanStarted : Boolean ;       // Prairie-View line scan started flag
    ImportImageRequested : Boolean ;  // Prairie-View Line scan image import requested flag
    NewLineScanNumber : Integer ;

    XMicronsPerPixel : Single ;           // Current Pixel resolution
    LineRotation : Single ;              // Angle of rotation of line
    opticalZoom : Single ;               // Optical magnification
    LineScanReferenceFileName : String ; // Name of reference image for line
    SourceImageWidth : Integer ;
    NewImageCompleted : Boolean ;

    procedure Connect ;                      // Establish TCP/IP connection to Ultima
    procedure Disconnect ;                   // Disconnect from Ultima
    procedure SendMessage( Msg : String ) ;  // Send message to Ultima
    procedure StartADCRecording ;            // Start A/D recording
    function NewDataFileName : String ;      // Create IDR file name for line scan data
    procedure CreateTempDataFile ;            // Create temporary IDR data file
    function ImportImageFile : Boolean ;              // Import Ultima line scan image into IDR file

    procedure GetStateXMLSettings(
              var SingleFileDirectory : String ;
              var XMicronsPerPixel : single
              ) ;

    procedure SendSnap ; // Send SNAP command to Ultima

    function IsUltimaActive : Boolean ;

  end;

var
  Ultima: TUltima;

implementation

uses logunit, Main , RecADCOnlyUnit;

{$R *.dfm}

const
    LSDFileBufSize = 50000 ;
    MaxUltimaChannels = 4 ;

procedure TUltima.DataModuleCreate(Sender: TObject);
// -----------------------------------
// Initialisations when module created
// -----------------------------------
begin
    DataDirectory := '' ;
    XMLFileName := '' ;
    BaseFileName := '' ;
    TempFileName := '' ;
    LineScanStarted := False ;
    Timer.Enabled := False ;
    TimerMethodExecuting := False ;
    ImportImageRequested := False ;
    NewLineScanNumber := 0 ;
    NumLineScanSweeps := 0 ;
    ReadyToRead := False ;

    // Prairie-View current state file
    StateXMLFileName := 'C:\Program Files\Prairie\Prairie View\Configuration Files\state.xml' ;

    XMicronsPerPixel := 0.0 ;
    LineRotation := 0.0 ;
    LineScanReferenceFileName := '' ;

    end;


procedure TUltima.Connect ;
// -------------------------------------
// Connect to Ultima Praire-View program
// -------------------------------------
begin

     // Set remote IP # to local host
     TCPClient.RemoteHost := TCPClient.LocalHostAddr ;

     // Open TCP/IP connection
     TCPClient.Open ;

     if not TCPClient.Active then begin
        MainFrm.StatusBar.SimpleText := 'Unable to establish TCP/IP connection with Prairie-View' ;
        LogFrm.AddLine(MainFrm.StatusBar.SimpleText) ;
        Exit ;
        end ;

     // Leave time for connection to be established
     Wait( 0.1 ) ;

     // Send connect command
     SendMessage('CONNECT|');
     SendMessage('VCLAMP|');

     MainFrm.StatusBar.SimpleText := format('TCP/IP connection with Prairie-View established (%s:%s)',
                                     [TCPClient.RemoteHost,TCPClient.RemotePort]) ;
     LogFrm.AddLine(MainFrm.StatusBar.SimpleText) ;

     Timer.Enabled := True ;

     end ;


procedure TUltima.Disconnect ;
// -------------------------------------
// Disconnect from Ultima Praire-View program
// -------------------------------------
begin

     if not TCPClient.Active then Exit ;

     // Send connect command
     SendMessage('EXIT|') ;

     // Give time for message to be processed
     Wait(0.1) ;

     // Close TCP/IP connection
     TCPClient.Close ;

     Timer.Enabled := False ;
     ImportImageRequested := False ;

     end ;


procedure TUltima.SendMessage( Msg : String ) ;
// ----------------------
// Send message to ultima
// ----------------------
var
  Buf : Array[0..255] of char ;
  i : integer ;
begin

     if not TCPClient.Active then Exit ;

     // Send command string to Ultima
     for i := 1 to Length(Msg) do Buf[i-1] := Msg[i] ;
     if MainFrm.PhotoStim.CmdTermZero then
       Buf[Length(Msg)] := #0
     else
       Buf[Length(Msg)] := #1;
     TCPClient.SendBuf(Buf,Length(Msg)+1,0) ;
     LogFrm.AddLine('TCP:Sent '+Msg) ;
     //outputdebugString(PChar(Msg)) ;

     end ;


procedure TUltima.TimerTimer(Sender: TObject);
// ---------------------------------------------
// Monitor communication with Prairie Ultima LSM
// ---------------------------------------------
var
    InBuf : Array[0..511] of Char ;
    Msg,s,Key,sCycle, tok : String ;
    n, i,Code : Integer ;
    pos, j, numTok : Integer ;
begin

     if not TCPClient.Active then Exit ;

     // Ensure only one instance executing at a time
     if TimerMethodExecuting then Exit ;
     TimerMethodExecuting := True ;

     // Get messages from Ultima
     n := TCPClient.ReceiveBuf(InBuf,SizeOf(InBuf),0) ;
     Msg := '' ;
     for i := 0 to n-1 do if InBuf[i] <> #0 then Msg := Msg + InBuf[i] ;
     //outputdebugString(PChar(Msg)) ;

     if Msg <> '' then LogFrm.Addline( 'TPC Rcvd: ' + Msg ) ;

     // Respond to GET_UNCAGING_GALVOS
     Key := 'GET_UNCAGING_GALVOS';
     if AnsiPos(Key, Msg) > 0 then begin
        SendMessage('UNCAGING_GALVOS|FALSE') ;
     end;

     // Praire-View Line scan sweep started
     Key := 'SEQUENTIAL_ON~' ;
     if AnsiPos(Key, Msg ) > 0 then begin
        // Get current line scan data folder
        s := AnsiMidStr( Msg, AnsiPos(Key,Msg)+Length(Key), Length(Msg) ) ;
        DataDirectory := AnsiLeftStr( s, AnsiPos('~',s)-1 ) ;

        LogFrm.AddLine('Praire-View Data folder: ' + DataDirectory );

        // XML file is same as folder name
        XMLFileName := DataDirectory + '\' + ExtractFileName(DataDirectory) + '.xml' ;

        //BaseFileName := AnsiLeftStr( s, AnsiPos('~',s)-1 ) ;
        BaseFileName := DataDirectory + '\' + ExtractFileName(DataDirectory) ;
        LogFrm.AddLine('Praire-View base file name: ' + BaseFileName );

        // Get current line cycle number from cycle file name (if available)
        if AnsiPos('Cycle',s) > 0 then begin
           // Get cycle number from file name (if available)
           i := AnsiPos('Cycle',s) + 5 ;
           sCycle := '' ;
           NumCycleDigits := 0 ;
           while (s[i] <> '~') and (i <= Length(s)) do begin
              sCycle := sCycle + s[i] ;
              inc(NumCycleDigits) ;
              Inc(i) ;
              end ;
           //sCycle := AnsiMidStr( s, AnsiPos('Cycle',s)+5, 3 ) ;
           Val(sCycle,NumLineScanSweeps,Code) ;
           NumLineScanSweeps := NumLineScanSweeps - 1 ;
           end
        else begin
           NumLineScanSweeps := 0 ;
           end ;
        LineScanStarted := False ;
        ImportImageRequested := False ;

        MainFrm.StatusBar.SimpleText := MainFrm.StatusBar.SimpleText +
                                        ' [Line scan mode ON]' ;

        end ;

     // SNAP command completed and Prairie-View aquired new image
     Key := 'NEWIMAGE' ;
     if AnsiPos(Key, Msg) > 0 then begin
        ProcessNewImage;
     end;

     // CHANNEL_LIST command
     Key := 'CHANNEL_LIST~' ;
     if AnsiPos(Key, Msg) > 0 then begin

        // Get rid of leading commands
        s := AnsiMidStr(Msg, AnsiPos(Key,Msg)+Length(Key), Length(Msg));

        // Find end of comman symbol
        j := Length(s);
        pos := 0;
        for i := 1 to j do
        begin
          if (Ord(s[i]) = 19) then
          begin
            pos := i;
            break;
          end;
        end;

        // Get command substring
        if pos > 0 then
        begin
          s := AnsiMidStr(s, 1, pos-1);
        end;

        // Parse channel names
        j := Length(s);
        pos := AnsiPos('~', s);
        tok := AnsiMidStr(s, 1, pos-1);
        s := AnsiMidStr(s, pos+1, Length(s));
        Val(tok, numTok, Code);
        for i := 1 to numTok do
        begin
          pos := AnsiPos('~', s);
          if pos = 0 then
            tok := AnsiMidStr(s, 1, Length(s))
          else
            tok := AnsiMidStr(s, 1, pos-1);
          MainFrm.PhotoStim.PMTChannelNames[i] := tok;
          s := AnsiMidStr(s, pos+1, Length(s));
        end;

     end;

     // Prairie-View Line scan sweep ended
     Key := 'TTL_TRIGGER_OFF' ;
     if AnsiPos(Key, Msg) > 0 then begin

        // Set flag indicating that XML is written
        ReadyToRead := True;

     end;

     // Start of A/D recording request from Ultima
     if (AnsiPos('ACQUIRE', Msg ) > 0) and
        (AnsiPos('ANALYZE_ACQUIRED', Msg ) = 0)  then begin
        MainFrm.StatusBar.SimpleText := MainFrm.StatusBar.SimpleText +
                                           ' [ACQUIRE requested]' ;

        // Increment no. of sweeps (since kast SEQUENTIAL ON)
        Inc(NumLineScanSweeps) ;

        StartADCRecording ;
        ImportImageRequested := False ;
        end ;

     // Wait for A/D recording to complete, then tell Ultima
     if LineScanStarted then begin
        // When A/D recording is complete, import image and inform Praire-View
        if not MainFrm.Recording then begin
           ImportImageRequested := True ;
           ImportImageTime := TimeGetTime + 1000 ;
           SendMessage('ACQUIRE_COMPLETE|') ;
           MainFrm.StatusBar.SimpleText := MainFrm.StatusBar.SimpleText +
                                           ' [ACQUIRE completed]' ;
           LineScanStarted := False ;
           end ;
        end ;

     // On completion of A/D recording, wait 1 second then import line scan from Ultima
     if ImportImageRequested and (TimeGetTime > ImportImageTime) then begin
        if ReadyToRead then begin
          if ImportImageFile then begin
             ReadyToRead := False ;
             ImportImageRequested := False ;
             // Close A/D form
             RecADCOnlyFrm.Close ;
             // Display line scan
             if MainFrm.IDRFile.NumFrames > 0 then MainFrm.mnViewImages.Click ;
          end
          else begin
             // Wait 1 second before trying again
             ImportImageTime := TimeGetTime + 1000 ;
          end ;
        end ;
     end ;

     TimerMethodExecuting := False ;

     end;


procedure TUltima.StartADCRecording ;
// -------------------
// Start A/D recording
// -------------------
begin

    // Create analogue recording form if it does not already exist
    if not MainFrm.FormExists('RecADCOnlyFrm') then MainFrm.mnRecord.Click ;

    // Create temporary WinFluor data file to hold line scan
    CreateTempDataFile ;

    // Start recording
    RecADCOnlyFrm.AutoRecordingMode := True ;
    RecADCOnlyFrm.StartRecordingToDisk( -1.0, 1,LSMLineScanMode) ;
    // Note. Negative recording time indicates that duration set to
    // WinFluor should be used

    // Tell Ultima to start line scan
    SendMessage('ACQUIRE_READY|') ;
    LineScanStarted := True ;

    end ;


procedure TUltima.CreateTempDataFile ;
// -------------------------------
// Create temporary WinFluor data file
// -------------------------------
begin

     //Inc(NumLineScanSweeps) ;
     // Create beginning of new WinFluor data file name
     TempFileName := format( '%s\TempDataFile_Sweep_%d.idr',
                             [DataDirectory,NumLineScanSweeps]) ;

     if FileExists( TempFileName ) then DeleteFile( PChar(TempFileName) ) ;

     // Close all windows (except record windows)
     MainFrm.CloseWindows ;

     // Close existing data files
     MainFrm.IDRFile.CloseFile ;

     MainFrm.IDRFile.CreateNewFile( TempFileName ) ;

     // Get ident line
     MainFrm.IDRFile.Ident := RecADCOnlyFrm.Ident ;

     MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;

     // Update log
     LogFrm.AddLine( 'Temp file created: ' + MainFrm.IDRFile.FileName );

     end ;


function TUltima.NewDataFileName : String ;
// --------------------------------------
// Create name for new WinFluor data file
// --------------------------------------
var
   Buf : array[0..LSDFileBufSize-1] of ANSIchar ;
   ch,n : Integer ;
   NumExistingLines : Integer ;
   FileName : String ;
   LSDFileName : String ;
   LSDFileFound : Boolean ;
begin

     LSDFileFound := False ;

     // Create beginning of new WinFluor data file name
     FileName := format( '%s\%s_LS_Ch',[DataDirectory,BaseFileName]) ;

     // Check for single-channel line scan LSD files (Prairie-View V2.1.)
     LSDFileName := DataDirectory + '\' + BaseFileName + '.lsd' ;
     n := GetLSDFileParameters( LSDFileName, Buf ) ;
     if n > 0 then LSDFileFound := True ;

     // Check for LSD file names ending with channel #
     for ch := 1 to MaxUltimaChannels do begin
         LSDFileName := DataDirectory + '\' + BaseFileName + format('_Ch%d.lsd',[ch]) ;
         n := GetLSDFileParameters( LSDFileName, Buf ) ;
         if n > 0 then begin
           FileName := FileName + format('%d',[ch]) ;
           if n > 0 then LSDFileFound := True ;
           end ;
         end ;

     if not LSDFileFound then begin
        LSDFileName := DataDirectory + '\' + BaseFileName + 'xxx.lsd' ;
        LogFrm.AddLine('Ultima: ' + LSDFileName + 'does not exist!' );
        Exit ;
        end ;

     NumExistingLines := 0 ;
     ReadInt( Buf, 'Number Lines=', NumExistingLines ) ;

     NewLineScanNumber := NumExistingLines ;

     // Add line number
     FileName := FileName + format( '_%d.idr',[NewLineScanNumber]) ;

     Result := FileName ;

     end ;


function TUltima.GetLSDFileParameters(
         LSDFileName : String ;
         var Buf : Array of ANSIChar ) : Integer ;
// -------------------------------
// Return parameters in LSD in Buf
// Note Buf is left unchsnged if LSDFileName does not exists
// -------------------------------
var
   FileHandle : THandle ;
   NumBytesInFile : Integer ;
   i : Integer ;
begin

     Result := 0 ;

     // Open LSD file
     if not FileExists( LSDFileName ) then Exit ;

     FileHandle := FileOpen( LSDFileName, fmOpenRead ) ;
     if FileHandle < 0 then begin
        LogFrm.AddLine('Ultima: Unable to open ' + LSDFileName );
        Exit ;
        end ;

     // Seek to end of file to get no. bytes
     NumBytesInFile := FileSeek( FileHandle, 0, 2 ) ;
     if NumBytesInFile >= (High(Buf)+1) then begin
        ShowMessage('Ultima: LSD file exceeds WinFluor buffer size!') ;
        LogFrm.AddLine('Ultima: LSD file exceeds WinFluor buffer size!') ;
        end ;

     // Clear buffer
     for i := 0 to High(Buf) do Buf[i] := #0 ;

     // Read data from LSD file
     FileSeek( FileHandle, 0, 0 ) ;
     Result := FileRead( FileHandle, Buf, Min(NumBytesInFile,High(Buf)+1) ) ;

     // Close file
     FileClose( FileHandle ) ;

     end ;


function TUltima.ImportImageFile : Boolean ;
// ---------------------------
// Import line scan image data
// ---------------------------
var
    FileHandle : THandle ;
begin

    if FileExists( XMLFileName ) then begin

       // Import from PV versions which use XML file

       // If unable to get exclusive access to XML document quit
       FileHandle := FileOpen( XMLFileName, fmOpenReadWrite ) ;
       if FileHandle < 0 then begin
          // Make certain to return False if the file can't be opened.
          Result := False;
          Exit;
       end
       else FileClose(FileHandle) ;

       // Load XML document
       XMLDoc.LoadFromFile(XMLFileName) ;
       XMLDoc.Active := True ;

       // Use appropriate import function
       if GetPrairieViewVersion >= 3.0 then Result := ImportImageFileXML_V3
                                       else Result := ImportImageFileXML_V2_5 ;

       XMLDoc.Active := False ;

       end
    else begin
       // Pre PV V2.5 Load using LSD file
       Result := ImportImageFileLSD ;
       end ;

    NumLineScanSweeps := 0 ;

    end ;


function TUltima.ImportImageFileLSD : Boolean ;
// -----------------------------
// Import images from image file
// -----------------------------
var
    Buf : Array[0..LSDFileBufSize-1] of ANSIChar ;
    PFrameBuf : Pointer ; // Image frame buffer pointer
    ch,n,NumLines : Integer ;
    FileNum : Integer ;    // Index into OpenDialog.Files list
    iFrame : Integer ;     // Frame counter
    NumFramesImported : Integer ;
    NumFiles : Integer ;
    ImportFileName : String ;
    InterLineTime : Single ;
    fValue : Single ;
    LSDFileNames : Array[0..MaxUltimaChannels] of String ;
    NewName : string ;
    NewNameEDR : string ;
    TempFileNameEDR : string ;
begin

    Result := False ;

    // Create list of possible LSD file names
    NumFiles := 0 ;
    LSDFileNames[NumFiles] := DataDirectory + '\' + BaseFileName + '.lsd' ;
    Inc(NumFiles) ;
    for ch := 1 to MaxUltimaChannels do begin
        LSDFileNames[NumFiles] := DataDirectory + '\' + BaseFileName + format('_Ch%d.lsd',[ch]) ;
        Inc(NumFiles) ;
        end ;

    // Import frames from all files in list
    NumFramesImported := 0 ;
    for FileNum := 0 to NumFiles-1 do begin

        // Read LSD file (if it exists)
        n := GetLSDFileParameters( LSDFileNames[FileNum], Buf ) ;
        if n <= 0 then Continue ;

        ReadInt( Buf, 'Number Lines=', NumLines ) ;
        if NumLines <= 0 then begin
           MainFrm.StatusBar.SimpleText := 'Ultima: No line scan data available!' ;
           LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;
           Continue ;
           end ;

        if NumLines <> NewLineScanNumber then Continue ;

        // Get name of latest line scan data file
        ImportFileName := '' ;
        ReadString( Buf, format('Line%d Image=',[NewLineScanNumber]), ImportFileName ) ;
        if ImportFileName = '' then begin
           MainFrm.StatusBar.SimpleText := 'Ultima: Line scan data file name missing!' ;
           LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;
           Continue ;
           end ;
        ImportFileName := DataDirectory + '\' + ImportFileName ;

        // Open file
        if not ImageFile.OpenFile( ImportFileName ) then begin
           MainFrm.StatusBar.SimpleText := 'Unable to open : ' + ImportFileName ;
           LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;
           Continue ;
           end ;

        // Get image properties from first import file
        if NumFramesImported = 0 then begin
           MainFrm.IDRFile.FrameWidth := ImageFile.FrameWidth ;
           MainFrm.IDRFile.FrameHeight := ImageFile.FrameHeight ;
           //MainFrm.IDRFile.PixelDepth := ImageFile.PixelDepth ;

           // Get camera pixel size
           MainFrm.IDRFile.XResolution := ImageFile.XResolution ;
           MainFrm.IDRFile.ResolutionUnits := ImageFile.ResolutionUnit ;

           MainFrm.IDRFile.NumFrameTypes := 1 ;
           MainFrm.IDRFile.FrameInterval := ImageFile.TResolution ;
           MainFrm.IDRFile.ADCScanInterval := MainFrm.ADCScanInterval ;
           MainFrm.IDRFile.ADCNumChannels := MainFrm.ADCNumChannels ;

           // Set inter-frame interval
           InterLineTime := 1.0 ;
           ReadFloat( Buf, Format('Line%d Inter-Line Time=',[NumLines]), InterLineTime ) ;
           MainFrm.IDRFile.FrameInterval := (InterLineTime*1E-3)*ImageFile.FrameHeight ;

           // Set pixel size
           ReadFloat( Buf, 'X Microns Per Pixel=', fValue ) ;
           MainFrm.IDRFile.XResolution := fValue ;
           MainFrm.IDRFile.ResolutionUnits := 'um' ;

           end ;

        // Terminate import if frame size changed
        if (MainFrm.IDRFile.FrameWidth <> ImageFile.FrameWidth) or
           (MainFrm.IDRFile.FrameHeight <> ImageFile.FrameHeight) then begin
           MainFrm.StatusBar.SimpleText := 'Ultima Import: Aborted at ' + ImportFileName ;
           LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;
           ImageFile.CloseFile ;
           Break ;
           end ;

        LogFrm.AddLine( 'Importing File: ' + ImportFileName ) ;

        // Allocate frame buffer
        GetMem( PFrameBuf,MainFrm.IDRFile.NumBytesPerFrame ) ;

        // Import frames from this file
        for iFrame := 1 to ImageFile.NumFrames do begin
           if ImageFile.LoadFrame( iFrame, PFrameBuf ) then begin
              Inc(NumFramesImported) ;
              MainFrm.IDRFile.SaveFrame( NumFramesImported, PFrameBuf ) ;
              MainFrm.StatusBar.SimpleText := format(
              'Ultima: Importing %d/%d frames from %s',
              [iFrame,ImageFile.NumFrames,ImportFileName]) ;
              end ;
           end ;

        // Close this import file
        ImageFile.CloseFile ;
        // Free buffer
        FreeMem( PFrameBuf ) ;

        end ;

     if NumFramesImported <= 0 then LogFrm.AddLine('No files imported!!!') ;

     // Get ident line
     MainFrm.IDRFile.Ident := RecADCOnlyFrm.Ident ;
     LogFrm.AddLine( MainFrm.IDRFile.Ident ) ;

     // Close temporary IDR/EDR files
     MainFrm.IDRFile.CloseFile ;

     // Rename temporary IDR file
     NewName := NewDataFileName ;
     if not RenameFile( TempFileName, NewName ) then
        LogFrm.AddLine( 'Unable to rename ' + TempFileName + ' to ' + NewName ) ;

     // Rename temporary IDR file
     NewNameEDR := ChangeFileExt( NewName, '.edr') ;
     TempFileNameEDR := ChangeFileExt(TempFileName, '.edr') ;
     if not RenameFile( TempFileNameEDR, NewNameEDR ) then
        LogFrm.AddLine( 'Unable to rename ' + TempFileNameEDR + ' to ' + NewNameEDR ) ;

     // Re-open file under new name
     MainFrm.IDRFile.OpenFile( NewName ) ;
     MainFrm.UpdateRecentFilesList ;
     LogFrm.AddLine( 'File opened: ' + MainFrm.IDRFile.FileName );
     // Display in program title bar
     MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;

     Result := True ;

     end ;


function TUltima.ImportImageFileXML_V3 : Boolean ;
// -------------------------------------------------------------
// Import images from image file using data from PVV3.X XML file
// -------------------------------------------------------------
const
    MaxChannels = 4 ;
var
    ImportFileName : String ;
    PFrameBuf,PBufPointer : Pointer ; // Image frame buffer pointer
    PImageBuf : PIntArray ; // Image frame buffer pointer
    FileNum : Integer ;    // Index into OpenDialog.Files list
    iFrame : Integer ;     // Frame counter
    imgFrame : Integer ;
    NumFramesImported : Integer ;
    NumFiles : Integer ;
    InterLineTime : Single ;
    PMTChannels : Array[0..MaxUltimaChannels] of Integer ;
    NewNameIDR : string ;
    FirstNewNameIDR : string ;
    NewNameEDR : string ;
    TempFileNameEDR : string ;
    TempFileName : String ;
    FileHandle : THandle ;
    NumCycles,NumChannels,NumFrames,NumLines  : Integer ;
    iCycle,iChan,ch : Integer ;
    FrameWidth : Integer ;
    i,iWrite : Integer ;
    Done : Boolean ;
    FileName : String ;
    ChanName : Array[0..MaxChannels-1] of String ;
    ChanNum : Array[0..MaxChannels-1] of Integer ;
    SourceFileName : String ;
begin

    Result := False ;

    // Find number of channels
    NumChannels := 0 ;
    for ch := 1 to MaxChannels do begin
        FileName := BaseFileName + '_' + CycleNumber(1) +
                    format('_CurrentSettings_Ch%d_%.6d.tif',[ch,1]) ;
        if FileExists(FileName) then begin
           ChanName[NumChannels] := format('Ch.%d',[ch]) ;
           ChanNum[NumChannels] := ch ;
           Inc(NumChannels) ;
           end ;
        end ;

    // Find number of cycles
    iCycle := 1 ;
    Done := False ;
    repeat
        FileName := BaseFileName + '_' + CycleNumber(iCycle) +
                    format('_CurrentSettings_Ch%d_%.6d.tif',[ChanNum[0],1]) ;
//        FileName := format('%s_Cycle%.3d_CurrentSettings_Ch%d_%.6d.tif',
//                    [BaseFileName,iCycle,ChanNum[0],1]) ;
        if FileExists(FileName) then begin

           NumCycles := iCycle ;
           Inc(iCycle) ;
           end
        else Done := True ;
        until Done ;

    // Reference file name
    if NumCycles >= 1 then begin
        LineScanReferenceFileName := BaseFileName + '-' + CycleNumber(iCycle) + '-LineScanReference.tif' ;
       //LineScanReferenceFileName := format('%s-Cycle001-LineScanReference.tif',[BaseFileName]) ;
       SourceFileName := BaseFileName + '-' + CycleNumber(iCycle) +
                         format('_Ch%dSource.tif',[ChanNum[0]]) ;
       //SourceFileName := format('%s-Cycle001_Ch%dSource.tif',[BaseFileName,ChanNum[0]]) ;
       ImageFile.OpenFile(SourceFileName) ;
       SourceImageWidth :=  ImageFile.FrameWidth ;
       ImageFile.CloseFile ;
       end
    else LineScanReferenceFileName := '' ;


    // Find number of frames and total number of lines in scan
    iFrame := 1 ;
    Done := False ;
    NumLines := 0 ;
    repeat
        FileName := BaseFileName + '_' + CycleNumber(ChanNum[0]) +
                    format('_CurrentSettings_Ch%d_%.6d.tif',[1,iFrame]) ;
//        FileName := format('%s_Cycle%.3d_CurrentSettings_Ch%d_%.6d.tif',
//                    [BaseFileName,ChanNum[0],1,iFrame]) ;
        if FileExists(FileName) then begin
           NumFrames := iFrame ;
           ImageFile.OpenFile( FileName ) ;
           FrameWidth := ImageFile.FrameWidth ;
           NumLines := ImageFile.FrameHeight + NumLines ;
           ImageFile.CloseFile ;
           Inc(iFrame) ;
           end
        else Done := True ;
        until Done ;

    FirstNewNameIDR := '' ;

    for iCycle := 1 to NumCycles do begin

        // Open temporary file for this frame
        TempFileName := format( '%s\TempDataFile_Sweep_%d.idr',
                                [DataDirectory,iCycle]) ;

        // Skip if temp. file name does not exists (because this is an appended scan)
        if not FileExists( TempFileName ) then Continue ;

        MainFrm.IDRFile.CloseFile ;
        MainFrm.IDRFile.OpenFile( TempFileName ) ;
        MainFrm.IDRFile.WriteEnabled := True ;

        MainFrm.IDRFile.FrameWidth := FrameWidth ;
        MainFrm.IDRFile.FrameHeight := NumLines ;

        // V3.0+ intensities vary up to 32767, V2.x up to 4095
        //if GetPrairieViewVersion >= 3.0 then MainFrm.IDRFile.PixelDepth := 15
        //else
        MainFrm.IDRFile.PixelDepth := 12 ;

        MainFrm.IDRFile.NumFrameTypes := NumChannels ;
        MainFrm.IDRFile.ADCScanInterval := MainFrm.ADCScanInterval ;
        MainFrm.IDRFile.ADCNumChannels := MainFrm.ADCNumChannels ;

        // Set inter-frame interval
        InterLineTime := GetKey( 1, 1, 'scanlinePeriod') ;
        MainFrm.IDRFile.FrameInterval := InterLineTime*NumLines ;

        // Set pixel size
        MainFrm.IDRFile.XResolution := GetKey( 1, 1, 'micronsPerPixel_XAxis') ;
        XMicronsPerPixel := MainFrm.IDRFile.XResolution ;
        MainFrm.IDRFile.ResolutionUnits := 'um' ;

        // Line rotation angle (convert degree -> radians)
        LineRotation := GetKey( 1, 1, 'rotation')*((2.0*Pi)/360.0) ;

        // Optical zoom
        OpticalZoom := GetKey( 1, 1, 'opticalZoom') ;

        // Allocate frame buffer
        GetMem( PFrameBuf, MainFrm.IDRFile.NumPixelsPerFrame*4 ) ;


        for iChan := 0 to NumChannels-1 do begin

            // Import frames from all files in list
            iWrite := 0 ;
            PBufpointer := PFrameBuf ;
            for iFrame := 1 to NumFrames do begin

                 ImportFileName := BaseFileName + '_' + CycleNumber(iCycle) +
                                   format('_CurrentSettings_Ch%d_%.6d.tif',[ChanNum[iChan],iFrame]) ;

//                ImportFileName := format('%s_Cycle%.3d_CurrentSettings_Ch%d_%.6d.tif',
//                                   [BaseFileName,iCycle,ChanNum[iChan],iFrame]) ;

                if not ImageFile.OpenFile( ImportFileName ) then begin
                   MainFrm.StatusBar.SimpleText := 'Unable to open : ' + ImportFileName ;
                   LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;
                   Continue ;
                   end ;

                GetMem( PImageBuf, ImageFile.NumPixelsPerFrame*4 ) ;

                // Get camera pixel size
                MainFrm.IDRFile.XResolution := ImageFile.XResolution ;
                MainFrm.IDRFile.ResolutionUnits := ImageFile.ResolutionUnit ;

                // PMT channel number
                MainFrm.IDRFile.FrameType[iChan] := ChanName[iChan] ;

                // Terminate import if frame size changed
                if (MainFrm.IDRFile.FrameWidth <> ImageFile.FrameWidth) then begin
                   MainFrm.StatusBar.SimpleText := 'Ultima Import: Aborted at ' + ImportFileName ;
                   LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;
                   ImageFile.CloseFile ;
                   Break ;
                   end ;

                LogFrm.AddLine( 'Importing File: ' + ImportFileName ) ;

                // Import frames from this file

                if ImageFile.LoadFrame( 1, PBufpointer ) then begin
                   PBufpointer := Pointer( NativeInt(PBufpointer)
                                           + (ImageFile.NumPixelsPerFrame*2) ) ;
                   MainFrm.StatusBar.SimpleText := format(
                   'Ultima: Importing: %s',
                   [ExtractFileName(ImportFileName)]) ;
                   end ;

                // Close this import file
                ImageFile.CloseFile ;

                FreeMem(PImageBuf) ;

                end ;

            MainFrm.IDRFile.SaveFrame( iChan+1, PFrameBuf ) ;

            end ;

        // Free buffer
        FreeMem( PFrameBuf ) ;
        PFrameBuf := Nil ;

        // Get ident line
        MainFrm.IDRFile.Ident := RecADCOnlyFrm.Ident ;
        LogFrm.AddLine( MainFrm.IDRFile.Ident ) ;

        // Close temporary IDR/EDR files
        MainFrm.IDRFile.CloseFile ;

        // Rename temporary IDR file
        NewNameIDR := ANSIReplaceText( ChangeFileExt( XMLFileName, '.idr' ),
                                       '.idr',
                                       format('_%d.idr',[iCycle]) ) ;
        if FirstNewNameIDR = '' then FirstNewNameIDR := NewNameIDR ;

        if not RenameFile( TempFileName, NewNameIDR ) then
           LogFrm.AddLine( 'Unable to rename ' + TempFileName + ' to ' + NewNameIDR ) ;

        // Rename temporary EDR file
        NewNameEDR := ChangeFileExt( NewNameIDR, '.edr') ;
        TempFileNameEDR := ChangeFileExt(TempFileName, '.edr') ;
        if not RenameFile( TempFileNameEDR, NewNameEDR ) then
           LogFrm.AddLine( 'Unable to rename ' + TempFileNameEDR + ' to ' + NewNameEDR ) ;

           

        LogFrm.AddLine( 'File created: ' + NewNameIDR );

        end ;

     // Re-open first file under new name
     MainFrm.IDRFile.CloseFile ;
     MainFrm.IDRFile.OpenFile( FirstNewNameIDR ) ;
     MainFrm.UpdateRecentFilesList ;
     MainFrm.DataDirectory := DataDirectory ;

     // Display in program title bar
     MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;

     Result := True ;

     end ;

function TUltima.CycleNumber( iNum : Integer ) : string ;
// ---------------------------------------------------------
// Create cycle number text with number of digits used by PV
// ---------------------------------------------------------
begin
    Result := '' ;
    case NumCycleDigits of
        1 : Result := format('Cycle%.1d',[iNum]) ;
        2 : Result := format('Cycle%.2d',[iNum]) ;
        3 : Result := format('Cycle%.3d',[iNum]) ;
        4 : Result := format('Cycle%.4d',[iNum]) ;
        5 : Result := format('Cycle%.5d',[iNum]) ;
        6 : Result := format('Cycle%.6d',[iNum]) ;
        7 : Result := format('Cycle%.7d',[iNum]) ;
        8 : Result := format('Cycle%.8d',[iNum]) ;
        else Result := format('Cycle%.5d',[iNum]) ;
        end ;
    end ;

function TUltima.ImportImageFileXML_V2_5 : Boolean ;
// --------------------------------------------------------------
// Import images from image file using data from PV V2.5 XML file
// --------------------------------------------------------------
var
    ImportFileName : String ;
    PFrameBuf,PBufPointer : Pointer ; // Image frame buffer pointer
    PImageBuf : PIntArray ; // Image frame buffer pointer
    iFrame : Integer ;     // Frame counter
    InterLineTime : Single ;
    NewNameIDR : string ;
    FirstNewNameIDR : string ;
    NewNameEDR : string ;
    TempFileNameEDR : string ;
    TempFileName : String ;
    NumCycles,NumChannels,NumFrames,NumLines  : Integer ;
    iCycle,iChan : Integer ;
    FrameWidth : Integer ;
    iWrite : Integer ;
    Done : Boolean ;
    FileName : String ;
begin

    Result := False ;

    // Find number of cycles
    iCycle := 1 ;
    Done := False ;
    repeat
        FileName := format('%s_Cycle%.3d_CurrentSettings_Ch%d_%.6d.tif',
                    [BaseFileName,iCycle,1,1]) ;
        if FileExists(FileName) then begin
           NumCycles := iCycle ;
           Inc(iCycle) ;
           end
        else Done := True ;
        until Done ;

    // Find number of channels
    iChan := 1 ;
    Done := False ;
    repeat
        FileName := format('%s_Cycle%.3d_CurrentSettings_Ch%d_%.6d.tif',
                    [BaseFileName,1,iChan,1]) ;
        if FileExists(FileName) then begin
           NumChannels := iChan ;
           Inc(iChan) ;
           end
        else Done := True ;
        until Done ;

    // Find number of frames and total number of lines in scan
    iFrame := 1 ;
    Done := False ;
    NumLines := 0 ;
    repeat
        FileName := format('%s_Cycle%.3d_CurrentSettings_Ch%d_%.6d.tif',
                    [BaseFileName,1,1,iFrame]) ;
        if FileExists(FileName) then begin
           NumFrames := iFrame ;
           ImageFile.OpenFile( FileName ) ;
           FrameWidth := ImageFile.FrameWidth ;
           NumLines := ImageFile.FrameHeight + NumLines ;
           ImageFile.CloseFile ;
           Inc(iFrame) ;
           end
        else Done := True ;
        until Done ;

    FirstNewNameIDR := '' ;

    for iCycle := 1 to NumCycles do begin

        // Open temporary file for this frame
        TempFileName := format( '%s\TempDataFile_Sweep_%d.idr',
                                [DataDirectory,iCycle]) ;

        // Skip if temp. file name does not exists (because this is an appended scan)
        if not FileExists( TempFileName ) then Continue ;

        MainFrm.IDRFile.CloseFile ;
        MainFrm.IDRFile.OpenFile( TempFileName ) ;
        MainFrm.IDRFile.WriteEnabled := True ;

        MainFrm.IDRFile.FrameWidth := FrameWidth ;
        MainFrm.IDRFile.FrameHeight := NumLines ;

        // V3.0+ intensities vary up to 32767, V2.x up to 4095
        //if GetPrairieViewVersion >= 3.0 then MainFrm.IDRFile.PixelDepth := 15
        //else
        MainFrm.IDRFile.PixelDepth := 12 ;

        MainFrm.IDRFile.NumFrameTypes := NumChannels ;
        MainFrm.IDRFile.ADCScanInterval := MainFrm.ADCScanInterval ;
        MainFrm.IDRFile.ADCNumChannels := MainFrm.ADCNumChannels ;

        // Set inter-frame interval
        InterLineTime := GetKey( 1, 1, 'scanlinePeriod') ;
        MainFrm.IDRFile.FrameInterval := InterLineTime*NumLines ;

        // Set pixel size
        MainFrm.IDRFile.XResolution := GetKey( 1, 1, 'micronsPerPixel_XAxis') ;
        MainFrm.IDRFile.ResolutionUnits := 'um' ;

        // Allocate frame buffer
        GetMem( PFrameBuf, MainFrm.IDRFile.NumPixelsPerFrame*4 ) ;

        iFrame := 1 ;
        for iChan := 1 to NumChannels do begin

            // Import frames from all files in list
            iWrite := 0 ;
            PBufpointer := PFrameBuf ;

            ImportFileName := format('%s_Cycle%.3d_CurrentSettings_Ch%d_%.6d.tif',
                                   [BaseFileName,iCycle,iChan,iFrame]) ;

            if not ImageFile.OpenFile( ImportFileName ) then begin
               MainFrm.StatusBar.SimpleText := 'Unable to open : ' + ImportFileName ;
               LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;
               Continue ;
               end ;

            GetMem( PImageBuf, ImageFile.NumPixelsPerFrame*4 ) ;

            // Get camera pixel size
            MainFrm.IDRFile.XResolution := ImageFile.XResolution ;
            MainFrm.IDRFile.ResolutionUnits := ImageFile.ResolutionUnit ;

            // PMT channel number
            MainFrm.IDRFile.FrameType[iChan-1] := format('Ch%d',[iChan]) ;

            // Terminate import if frame size changed
            if (MainFrm.IDRFile.FrameWidth <> ImageFile.FrameWidth) then begin
               MainFrm.StatusBar.SimpleText := 'Ultima Import: Aborted at ' + ImportFileName ;
               LogFrm.AddLine( MainFrm.StatusBar.SimpleText ) ;
               ImageFile.CloseFile ;
               Break ;
               end ;

            LogFrm.AddLine( 'Importing File: ' + ImportFileName ) ;

            // Import frames from this file

            if ImageFile.LoadFrame( 1, PFrameBuf ) then begin
                   MainFrm.StatusBar.SimpleText := format(
                   'Ultima: Importing: %s',
                   [ExtractFileName(ImportFileName)]) ;
                   end ;

            // Close this import file
            ImageFile.CloseFile ;

            FreeMem(PImageBuf) ;

            MainFrm.IDRFile.SaveFrame( iChan, PFrameBuf ) ;

            end ;

        // Free buffer
        FreeMem( PFrameBuf ) ;
        PFrameBuf := Nil ;

        // Get ident line
        MainFrm.IDRFile.Ident := RecADCOnlyFrm.Ident ;
        LogFrm.AddLine( MainFrm.IDRFile.Ident ) ;

        // Close temporary IDR/EDR files
        MainFrm.IDRFile.CloseFile ;

        // Rename temporary IDR file
        NewNameIDR := ANSIReplaceText( ChangeFileExt( XMLFileName, '.idr' ),
                                       '.idr',
                                       format('_%d.idr',[iCycle]) ) ;
        if FirstNewNameIDR = '' then FirstNewNameIDR := NewNameIDR ;

        if not RenameFile( TempFileName, NewNameIDR ) then
           LogFrm.AddLine( 'Unable to rename ' + TempFileName + ' to ' + NewNameIDR ) ;

        // Rename temporary EDR file
        NewNameEDR := ChangeFileExt( NewNameIDR, '.edr') ;
        TempFileNameEDR := ChangeFileExt(TempFileName, '.edr') ;
        if not RenameFile( TempFileNameEDR, NewNameEDR ) then
           LogFrm.AddLine( 'Unable to rename ' + TempFileNameEDR + ' to ' + NewNameEDR ) ;

        LogFrm.AddLine( 'File created: ' + NewNameIDR );

        end ;

     // Re-open first file under new name
     MainFrm.IDRFile.CloseFile ;
     MainFrm.IDRFile.OpenFile( FirstNewNameIDR ) ;
     MainFrm.UpdateRecentFilesList ;
     MainFrm.DataDirectory := DataDirectory ;

     // Display in program title bar
     MainFrm.Caption := 'WinFluor : ' + MainFrm.IDRFile.FileName ;

     Result := True ;

     end ;


procedure TUltima.Wait( Delay : Single ) ;
var
    TExit : Cardinal ;
begin
    TExit := TimeGetTime + Round(Delay*1000) ;
    Repeat Until (TimeGetTime >= TExit) ;
    end ;


function TUltima.GetNumberOfFramesInSequenceXML(
         iSequence : Integer
         ) : Integer;
// ------------------------------------------
// Get number of frames within sequence
// ------------------------------------------
var
    SeqNode : IXMLNode ;
    i : Integer ;
    NumFrames : Integer ;
    Found : Boolean ;
begin

    Result := 0 ;
    Found := False ;

    // Find selected sequence node
    for i := 0 to XMLDOC.DocumentElement.ChildNodes.Count-1 do begin
      if XMLDOC.DocumentElement.ChildNodes[i].NodeName = 'Sequence' then begin
         SeqNode := XMLDOC.DocumentElement.ChildNodes[i] ;
         if (i+1) = iSequence then begin
            Found := True ;
            Break ;
            end ;
         end ;
      end ;

    if not Found then Exit ;

    // Count number of frame nodes
    NumFrames := 0 ;
    for i := 0 to SeqNode.ChildNodes.Count-1 do begin
        if SeqNode.ChildNodes[i].NodeName = 'Frame' then Inc(NumFrames) ;
        end ;

    Result := NumFrames ;

    end ;


function TUltima.GetNumberOfSequencesXML : Integer;
// ------------------------
// Get number of sequences
// ------------------------
var
    i : Integer ;
    NumSequences : Integer ;
begin

    Result := 0 ;

    // Count number of sequence nodes
    NumSequences := 0 ;
    for i := 0 to XMLDOC.DocumentElement.ChildNodes.Count-1 do begin
        if XMLDOC.DocumentElement.ChildNodes[i].NodeName = 'Sequence' then Inc(NumSequences) ;
        end ;

    Result := NumSequences ;

    end ;



function TUltima.GetPrairieViewVersion : Single ;
// ------------------------------------------
// Get Prairie-View version number
// ------------------------------------------
var
    Code : Integer ;
    sVer : string ;
    VMajor,VMin1,VMin2,VMin3 : Single ;
begin

    VMajor := 0 ;
    VMin1 := 0 ;
    VMin2 := 0 ;
    VMin3 := 0 ;

    sVer := XMLDOC.DocumentElement.Attributes['version'] ;
    if sVer <> '' then begin
       Val(sVer[1],VMajor,Code) ;
       Val(sVer[3],VMin1,Code) ;
       Val(sVer[5],VMin2,Code) ;
       Val(sVer[7],VMin3,Code) ;
       end ;

    Result := VMajor + 0.1*VMin1 + 0.01*VMin2 + 0.001*VMin3 ;

    end ;


function TUltima.GetFileNamesInFrame(
         iSequence : Integer ;
         iFrame : Integer ;
         var FileNames : Array of String ; // List of file names
         var PMTChannels : Array of Integer ;
         var NumFiles : Integer            // No. of files
         ) : Boolean ;
// ------------------------------------------
// Get names TIF data files with Frame record
// ------------------------------------------
var
    SeqNode,FRNode : IXMLNode ;
    i : Integer ;
    FrameNum : Integer ;
    Code : Integer ;
begin

    NumFiles := 0 ;
    Result := False ;

    // Find selected sequence node
    for i := 0 to XMLDOC.DocumentElement.ChildNodes.Count-1 do begin
      if XMLDOC.DocumentElement.ChildNodes[i].NodeName = 'Sequence' then begin
         SeqNode := XMLDOC.DocumentElement.ChildNodes[i] ;
         if (i+1) = iSequence then begin
            Result := True ;
            Break ;
            end ;
         end ;
      end ;

    if not Result then Exit ;

    // Find selected frame node
    for i := 0 to SeqNode.ChildNodes.Count-1 do begin
      if SeqNode.ChildNodes[i].NodeName = 'Frame' then begin
         FrNode := SeqNode.ChildNodes[i] ;
         Val(FRNode.Attributes['index'],FrameNum,Code) ;
         if FrameNum = iFrame then begin
            Result := True ;
            Break ;
            end ;
         end ;
      end ;

    if not Result then Exit ;

    // Find and return all file names in frame node
    for i := 0 to FRNode.ChildNodes.Count-1 do begin
        if FRNode.ChildNodes[i].NodeName = 'File' then begin
           // File name
           FileNames[NumFiles] := FRNode.ChildNodes[i].Attributes['filename'] ;
           // PMT channel number
           Val(FRNode.ChildNodes[i].Attributes['channel'],PMTChannels[i],Code) ;
           Inc(NumFiles) ;
           if NumFiles > High(FileNames) then Exit ;
           end ;
        end ;

    end ;


function TUltima.GetFileNames(
         iSelectedFrame : Integer ;
         var FileNames : Array of String ; // List of file names
         var PMTChannels : Array of Integer ;
         var NumFiles : Integer            // No. of files
         ) : Boolean ;
// ------------------------------------------
// Get names TIF data files with Frame record
// ------------------------------------------
var
    SeqNode,FRNode : IXMLNode ;
    i : Integer ;
    FrameNum : Integer ;
    Code : Integer ;
begin

    NumFiles := 0 ;
    Result := False ;

    if not FindXMLNode( XMLDOC.DocumentElement, 'Sequence', SeqNode ) then Exit ;

    // Find selected frame node
    for i := 0 to SeqNode.ChildNodes.Count-1 do begin
      if SeqNode.ChildNodes[i].NodeName = 'Frame' then begin
         FrNode := SeqNode.ChildNodes[i] ;
         Val(FRNode.Attributes['index'],FrameNum,Code) ;
         if iSelectedFrame = FrameNum then begin
            Result := True ;
            Break ;
            end ;
         end ;
      end ;

    if not Result then Exit ;

    // Find and return all file names in frame node
    for i := 0 to FRNode.ChildNodes.Count-1 do begin
        if FRNode.ChildNodes[i].NodeName = 'File' then begin
           // File name
           FileNames[NumFiles] := FRNode.ChildNodes[i].Attributes['filename'] ;
           // PMT channel number
           Val(FRNode.ChildNodes[i].Attributes['channel'],PMTChannels[i],Code) ;
           Inc(NumFiles) ;
           if NumFiles > High(FileNames) then Exit ;
           end ;
        end ;

    end ;


procedure TUltima.GetStateXMLSettings(
         var SingleFileDirectory : String ;
         var XMicronsPerPixel : single
         ) ;
// ------------------------------------------
// Get latest Prairie-View single image folder
// ------------------------------------------
var
    FileHandle : THandle ;
    PVStateNode : IXMLNode ;
    KeyValue : String ;
    FilePath : String ;
    COde : Integer ;
begin

       // If unable to get exclusive access to XML document quit
       FileHandle := FileOpen( StateXMLFileName, fmOpenReadWrite ) ;
       if FileHandle < 0 then Exit
       else FileClose(FileHandle) ;

       // Load XML document
       xmldoc.LoadFromFile(StateXMLFileName) ;
       xmldoc.Active := True ;

       if not FindXMLNode( xmldoc.DocumentElement,
                           'PVStateShard',
                           PVStateNode ) then begin
          xmldoc.Active := False ;
          Exit ;
          end ;

       // Find base directory
       FilePath := GetStateXMLKey( PVStateNode, 'directory_Base' ) ;

       // Find single file directory name
       KeyValue := GetStateXMLKey( PVStateNode, 'directory_SingleImage' ) ;
       SingleFileDirectory := FilePath + '\' + KeyValue ;

      // Find single file directory name
      KeyValue := GetStateXMLKey( PVStateNode, 'micronsPerPixel_XAxis' ) ;
      if KeyValue <> '' then begin
         Val( KeyValue, XMicronsPerPixel, Code ) ;
         end
      else XMicronsPerPixel := 1.0 ;

      XMLState.Active := False ;

      end ;


function TUltima.GetStateXMLKey(
         var Node : IXMLNode ;
         KeyName : String ) : String ;
// -------------------------------------
// Get XML Key value from STATE.XML file
// -------------------------------------
var
    i : Integer ;
begin
       Result := '' ;
       for i := 0 to Node.ChildNodes.Count-1 do
           if Node.ChildNodes[i].NodeName = 'Key' then begin
           if KeyName = Node.ChildNodes[i].Attributes['key'] then begin
              Result := Node.ChildNodes[i].Attributes['value'] ;
              Break ;
              end ;
           end ;
       end ;


function TUltima.GetKey(
         iSequence : Integer ;
         iFrame : Integer ;
         KeyName : String
         ) : Single ;
// -----------------------------------
// Return value of selected "key" node
// -----------------------------------
var
    PVStateNode,SeqNode,FRNode : IXMLNode ;
    KeyValue : String ;
    Value : Single ;
    BadCharCode : Integer ;
    i : Integer ;
    FrameNum : Integer ;
    Code : Integer ;
    Found : Boolean ;
    CycleNum : Integer ;
begin

    Result := 0.0 ;
    Found := False ;

    // Find selected sequence node
    for i := 0 to XMLDOC.DocumentElement.ChildNodes.Count-1 do begin
      if XMLDOC.DocumentElement.ChildNodes[i].NodeName = 'Sequence' then begin
         Val(XMLDOC.DocumentElement.ChildNodes[i].Attributes['cycle'],CycleNum,Code) ;
         if CycleNum = iSequence then begin
            SeqNode := XMLDOC.DocumentElement.ChildNodes[i] ;
            Found := True ;
            Break ;
            end ;
         end ;
      end ;

    if not Found then Exit ;

    // Find selected frame node
    for i := 0 to SeqNode.ChildNodes.Count-1 do begin
      if SeqNode.ChildNodes[i].NodeName = 'Frame' then begin
         FrNode := SeqNode.ChildNodes[i] ;
         Val(FRNode.Attributes['index'],FrameNum,Code) ;
         if FrameNum = iFrame then begin
            Found := True ;
            Break ;
            end ;
         end ;
      end ;

    if not Found then Exit ;

    // Get PVStateShard node
    if not FindXMLNode( FRNode, 'PVStateShard', PVStateNode ) then Exit ;

    // Find selected key node
    for i := 0 to PVStateNode.ChildNodes.Count-1 do
        if PVStateNode.ChildNodes[i].NodeName = 'Key' then begin
           if KeyName = PVStateNode.ChildNodes[i].Attributes['key'] then begin
              KeyValue := PVStateNode.ChildNodes[i].Attributes['value'] ;
              Break ;
              end ;
           end ;

    if KeyValue <> '' then begin
       Val( KeyValue, Value, BadCharCode ) ;
       if BadCharCode = 0 then Result := Value ;
       end ;

    end ;


function TUltima.FindXMLNode(
         const ParentNode : IXMLNode ;
         NodeName : String ;
         var ChildNode : IXMLNode
         ) : Boolean ;
var
    i : Integer ;
begin

    Result := False ;
    for i := 0 to ParentNode.ChildNodes.Count-1 do begin
      if ParentNode.ChildNodes[i].NodeName = NodeName then begin
         Result := True ;
         ChildNode := ParentNode.ChildNodes[i] ;
         Break ;
         end ;
      end ;
    end ;


procedure TUltima.SendSnap ;
// -----------------------------------------------
// Send SNAP command to Ultima Praire-View program
// -----------------------------------------------
begin

     if not TCPClient.Active then Exit ;

     // Set flag to False
     // This is set to True at end of TUltima.ProcessNewImage procedure
     NewImageCompleted := False;

     // Send connect command
     SendMessage('SNAP|') ;

     // Give time for message to be processed
     Wait(0.1) ;

     //Ultima.NewImageCompleted := True;

  end ;


procedure TUltima.ProcessNewImage ;
// -----------------------------------------
// Process NEWIMAGE command from PrairieView
// -----------------------------------------
const
  Needle = 'aquired data saved to';
var
  FileName : String;
  f : Integer;
  pos : Integer;
  Line : String;
  BeginOfFile : Boolean;
  DataFound : Boolean;
  FullPath : String;
  FilePath : String;
  SeqNode : IXMLNode;
  FrNode : IXMLNode;
  PVNode : IXMLNode;
  Result : Boolean;
  Code : Integer;
  FrameNum : Integer;
  s : String;
  i : Integer;
  NumFiles : Integer;
  micronsX : Single;
  micronsY : Single;
  zoom : Single;
  rotation : Single;
  imgWidth : Integer;
  imgHeight : Integer;
  maxVoltageX : Double;
  maxVoltageY : Double;
  minVoltageX : Double;
  minVoltageY : Double;
begin

  DataFound := False;

  // Prairie log file name
  FileName := MainFrm.PhotoStim.PVLogFile;

  // Try to open Prairie log file

  f := FileOpen(FileName, fmShareDenyNone);
  if f > -1 then begin
    LogFrm.AddLine('Process NEWIMAGE - Prairie log opened: ' + FileName);
    end
  else begin
    // Note: Prairie's log file name is incorrectly spelled in V4.X versions before V4.3!!!
    // Try to open again with incorrect spelling
    FileName := ANSIReplaceText( FileName, 'prairieview.log', 'prarieview.log' ) ;
    f := FileOpen(FileName, fmShareDenyNone);
    if f > -1 then begin
       LogFrm.AddLine('Process NEWIMAGE - Error opening Prairie log: ' + FileName);
       Exit;
       end ;
    end;


  // Move to end of file
  FileSeek(f, 0, 2);


  // Read file, backwards until proper entry is found or BOF reached
  repeat

    // Read line
    Readback(f, Line, BeginOfFile);

    // Check if line contains data we're looking for
    if AnsiContainsStr(Line, Needle) then
    begin
      DataFound := True;
    end;

  until BeginOfFile or DataFound;


  // Check if found proper entry in log file
  if DataFound then
  begin

    // Get dataset path
    pos := AnsiPos(Needle, Line);
    FullPath := AnsiRightStr(Line, Length(Line) - (pos + Length(Needle)));
    FilePath := ExtractFileName(FullPath);

  end
  else
  begin

    // Can't find entry
    LogFrm.AddLine('Process NEWIMAGE - Path not found.');

    // Close file
    FileClose(f);

    // Exit procedure
    Exit;

  end;


  // Close file
  FileClose(f);


  // If unable to get exclusive access to XML document quit
  f := FileOpen(FullPath + '\' + FilePath + '.xml', fmOpenReadWrite);
  if f < 0 then begin
    LogFrm.AddLine('Process NEWIMAGE - Can not open XML file!');
    Exit;
  end
  else begin
    FileClose(f);
  end;

  // Load XML file
  XMLSingleImage.LoadFromFile(FullPath + '\' + FilePath + '.xml');

  // Mark active
  XMLSingleImage.Active := True;

  // Find 'Sequence' node
  if not FindXMLNode(XMLSingleImage.DocumentElement, 'Sequence', SeqNode) then Exit;

  // Make certain this is a single scan
  s := SeqNode.Attributes['type'];
  if strcomp(Addr(s[1]), 'Single') <> 0 then
  begin
    LogFrm.AddLine('Process NEWIMAGE - Single scan entry not found in XML file.');
    Exit;
  end;

  // Find frame node for frame 1
  for i := 0 to SeqNode.ChildNodes.Count-1 do begin
    if SeqNode.ChildNodes[i].NodeName = 'Frame' then begin
      FrNode := SeqNode.ChildNodes[i];
      Val(FRNode.Attributes['index'], FrameNum, Code);
      if FrameNum = 1 then begin
        Result := True;
        Break;
      end;
    end;
  end;

  // Check that frame 1 was found
  if not Result then Exit;

  // Initialize number of files found
  NumFiles := 0;

  // Find all file names in frame node
  for i := 0 to FRNode.ChildNodes.Count-1 do begin
    if FRNode.ChildNodes[i].NodeName = 'File' then begin
      // File name
      MainFrm.PhotoStim.PMTFileNames[NumFiles] := FullPath + '\' + FRNode.ChildNodes[i].Attributes['filename'];
      // PMT channel number
      Val(FRNode.ChildNodes[i].Attributes['channel'], MainFrm.PhotoStim.PMTChannels[i], Code);
      Inc(NumFiles);
      MainFrm.PhotoStim.NumPMTFiles := NumFiles;
      if NumFiles > High(MainFrm.PhotoStim.PMTFileNames) then Exit;
    end;
  end;

  // Find PVStateShared node
  for i := 0 to FRNode.ChildNodes.Count-1 do begin
    Result := False;
    if FRNode.ChildNodes[i].NodeName = 'PVStateShard' then begin
      PVNode := FRNode.ChildNodes[i];
      Result := True;
      Break;
    end;
  end;

  // Could not find PVStateShard node
  if not Result Then Exit;

  // Extract um/pixel and zoom factor
  for i := 0 to PVNode.ChildNodes.Count-1 do begin
    if PVNode.ChildNodes[i].NodeName = 'Key' then begin
      if PVNode.ChildNodes[i].Attributes['key'] = 'micronsPerPixel_XAxis' then
      begin
        Val(PVNode.ChildNodes[i].Attributes['value'], micronsX, Code);
      end;
      if PVNode.ChildNodes[i].Attributes['key'] = 'micronsPerPixel_YAxis' then
      begin
        Val(PVNode.ChildNodes[i].Attributes['value'], micronsY, Code);
      end;
      if PVNode.ChildNodes[i].Attributes['key'] = 'opticalZoom' then
      begin
        Val(PVNode.ChildNodes[i].Attributes['value'], zoom, Code);
      end;
      if PVNode.ChildNodes[i].Attributes['key'] = 'rotation' then
      begin
        Val(PVNode.ChildNodes[i].Attributes['value'], rotation, Code);
      end;
      if PVNode.ChildNodes[i].Attributes['key'] = 'pixelsPerLine' then
      begin
        Val(PVNode.ChildNodes[i].Attributes['value'], imgWidth, Code);
      end;
      if PVNode.ChildNodes[i].Attributes['key'] = 'linesPerFrame' then
      begin
        Val(PVNode.ChildNodes[i].Attributes['value'], imgHeight, Code);
      end;
      if PVNode.ChildNodes[i].Attributes['key'] = 'minVoltage_XAxis' then
      begin
        Val(PVNode.ChildNodes[i].Attributes['value'], minVoltageX, Code);
      end;
      if PVNode.ChildNodes[i].Attributes['key'] = 'minVoltage_YAxis' then
      begin
        Val(PVNode.ChildNodes[i].Attributes['value'], minVoltageY, Code);
      end;
      if PVNode.ChildNodes[i].Attributes['key'] = 'maxVoltage_XAxis' then
      begin
        Val(PVNode.ChildNodes[i].Attributes['value'], maxVoltageX, Code);
      end;
      if PVNode.ChildNodes[i].Attributes['key'] = 'maxVoltage_YAxis' then
      begin
        Val(PVNode.ChildNodes[i].Attributes['value'], maxVoltageY, Code);
      end;
    end;
  end;

  // Correct um/pixel for zoom factor
  micronsX := micronsX / zoom;
  micronsY := micronsY / zoom;

  // Save um/pixel data
  MainFrm.PhotoStim.XMicronsPerPixel := micronsX;
  MainFrm.PhotoStim.YMicronsPerPixel := micronsY;

  // Save image dimensions
  //MainFrm.PhotoStim.ImageWidth := imgWidth;
  //MainFrm.PhotoStim.ImageHeight := imgHeight;

  // Save rotation data in radians
  MainFrm.PhotoStim.ImageRotation := rotation * 0.0174532925;

  // Calculate center offsets for ROI
  // The factor of 4.2 accounts for resistors on BNC boxes from Prairie
  // that limit the voltage sent to the galvos.
  MainFrm.PhotoStim.ROIXVoltageOffset :=
    (((maxVoltageX - minVoltageX) / 2) + minVoltageX) * 4.2;
  MainFrm.PhotoStim.ROIYVoltageOffset :=
    (((maxVoltageY - minVoltageY) / 2) + minVoltageY) * 4.2;

  // Mark inactive
  XMLSingleImage.Active := False;


  // Set completed to True
  // Without this, PhotoStimUnit will not update correctly.
  NewImageCompleted := True;

end ;


function TUltima.IsUltimaActive : Boolean;
begin
  Result := TCPClient.Active;
end;


procedure TUltima.Readback (const f: integer; var Line: String; var _bof: boolean);
// -----------------------------------------
// Read a file backwards
// -----------------------------------------
const
  MAXLINELENGTH = 512;
var
  curr,
  Before : Longint;
  Buffer : array [0..MAXLINELENGTH] of char;
  p      : PChar;
begin
  // scan backwards to the last CR-LF
  curr := FileSeek (f, 0, 1);
  Before := curr - MAXLINELENGTH;
  if Before < 0 then
    Before := 0;
  FileSeek (f, Before, 0);
  FileRead (f, Buffer, curr - Before);
  Buffer[curr - Before] := #0;
  p := StrRScan (Buffer, #10);
  if p = Nil then
  begin
    Line := StrPas (Buffer);
    FileSeek (f, 0, 0);
    _bof := True
  end
  else
  begin
    Line := StrPas (p + 1);
    FileSeek (f, Before + Longint (p) - Longint (@Buffer), 0);
    _bof := False
  end;

  // this will also work with Unix files (#10 only, no #13)
  if length (Line) > 0 then
    if Line[length (Line)] = #13 then
    begin
      SetLength (Line, length (Line) - 1)
    end
end;


procedure TUltima.ReadInt( const Source : Array of ANSIChar; Keyword : ANSIstring ; var Value : LongInt ) ;
var
   Parameter : ANSIstring ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if Parameter <> '' then Value := ExtractInt( Parameter ) ;
     end ;


procedure TUltima.ReadString(
          const Source : Array of ANSIChar;
          Keyword : ANSIstring ;
          var Value : string ) ;
var
   Parameter : ANSIstring ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if Parameter <> '' then Value := String(Parameter)  ;
     end ;


procedure TUltima.ReadFloat( const Source : Array of ANSIChar; Keyword : ANSIstring ; var Value : Single ) ;
var
   Parameter : ANSIstring ;
begin
     FindParameter( Source, Keyword, Parameter ) ;
     if Parameter <> '' then Value := ExtractFloat( Parameter, 1. ) ;
     end ;


function TUltima.ExtractFloat (
         CBuf : ANSIstring ;     { ASCII text to be processed }
         Default : Single    { Default value if text is not valid }
         ) : extended ;
{ -------------------------------------------------------------------
  Extract a floating point number from a string which
  may contain additional non-numeric text
  28/10/99 ... Now handles both comma and period as decimal separator
  -------------------------------------------------------------------}

var
   CNum,dsep : ANSIstring ;
   i : integer ;
   Done,NumberFound : Boolean ;
begin
     { Extract number from othr text which may be around it }
     CNum := '' ;
     Done := False ;
     NumberFound := False ;
     i := 1 ;
     repeat

         if CBuf[i] in ['0'..'9', 'E', 'e', '+', '-', '.', ',' ] then begin
            CNum := CNum + CBuf[i] ;
            NumberFound := True ;
            end
         else if NumberFound then Done := True ;
         Inc(i) ;
         if i > Length(CBuf) then Done := True ;
         until Done ;

     { Correct for use of comma/period as decimal separator }
     {$IF CompilerVersion > 7.0} dsep := formatsettings.DECIMALSEPARATOR ;
     {$ELSE} dsep := DECIMALSEPARATOR ;
     {$IFEND}
     if dsep = '.' then CNum := ANSIReplaceText(CNum ,',',dsep);
     if dsep = ',' then CNum := ANSIReplaceText(CNum, '.',dsep);

     { Convert number from ASCII to real }
     try
        if Length(CNum)>0 then Result := StrToFloat( String(CNum) )
                          else Result := Default ;
     except
        on E : EConvertError do Result := Default ;
        end ;
end ;




procedure TUltima.FindParameter( const Source : array of ANSIChar ;
                               Keyword : ANSIstring ;
                               var Parameter : ANSIstring ) ;
var
s,k : integer ;
Found : boolean ;
begin

     { Search for the string 'keyword' within the
       array 'Source' }

     s := 0 ;
     k := 1 ;
     Found := False ;
     while (not Found) and (s < High(Source)) do
     begin
          if Source[s] = Keyword[k] then
          begin
               k := k + 1 ;
               if k > length(Keyword) then Found := True
               end
               else k := 1;
         s := s + 1;
         end ;

    { Copy parameter value into string 'Parameter'
      to be returned to calling routine }

    Parameter := '' ;
    if Found then
    begin
        while (Source[s] <> #13) and (s < High(Source)) do
        begin
             Parameter := Parameter + Source[s] ;
             s := s + 1
             end ;
        end ;
    end ;


function TUltima.ExtractInt ( CBuf : ANSIstring ) : longint ;
{ ---------------------------------------------------
  Extract a 32 bit integer number from a string which
  may contain additional non-numeric text
  ---------------------------------------------------}

Type
    TState = (RemoveLeadingWhiteSpace, ReadNumber) ;
var CNum : ANSIstring ;
    i : integer ;
    Quit : Boolean ;
    State : TState ;

begin
     CNum := '' ;
     i := 1;
     Quit := False ;
     State := RemoveLeadingWhiteSpace ;
     while not Quit do begin

           case State of

                { Ignore all non-numeric characters before number }
                RemoveLeadingWhiteSpace : begin
                   if CBuf[i] in ['0'..'9','+','-'] then State := ReadNumber
                                                    else i := i + 1 ;
                   end ;

                { Copy number into string CNum }
                ReadNumber : begin
                    {End copying when a non-numeric character
                    or the end of the string is encountered }
                    if CBuf[i] in ['0'..'9','E','e','+','-','.'] then begin
                       CNum := CNum + CBuf[i] ;
                       i := i + 1 ;
                       end
                    else Quit := True ;
                    end ;
                else end ;

           if i > Length(CBuf) then Quit := True ;
           end ;
     try


        ExtractInt := StrToInt( CNum ) ;
     except
        ExtractInt := 1 ;
        end ;
     end ;



end.

