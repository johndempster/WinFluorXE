unit Wavgen;
{ =====================================================================
  WinWCP -Analogue & Digital Waveform Editor
  (c) J. Dempster, University of Strathclyde, 1996-97. All Rights Reserved.
  22/5/97 Bug in incrementing duration dig. pulse fixed.
          Minor offset error between dig & voltage pulse fixed.
  12/12/97 DAC update interval can now go down to 0.1 ms
           and is adjusted automatically based on pulse width
           and repeat period
  24/3/98  wvStep1 : Amplitude increment now updated correctly when
           when changed
  3/4/98   Data points in user-defined waveforms now automatically reduced
           in number to fit available space. Inter-record interval also
           adjusted if it is too long to be supported by D/A update interval
  2/7/99   V3.0 32 bit version
  11/12/99 Stimulus Protocol list in Record To Disk window now updated
           if that window is open
  20/7/01 Num Leaks and Leak scale labels now visible
  20/12/01 Updated to use SESLabIO.DACMaxValue for scaling display
  18.7.02  RecordInterval now automatically updated to ensure that it is
           5% (or 300ms) greater than RecordDuration
  5/2/04 ..... Normal/Inverted trigger pulse setting added
  30/3/04 .... User-defined waveforms can now contain negative voltages
  2/11/04 .... Two DAC channels now supported
               Waveform display now much faster
  16/12/04 ... Display now ignores zero duration elements
               Changes to waveform elements now trigger protocol save
  20/05.05 ... (V3.5.6) Number of pulse increments for each waveform element
               now stored in Prog.NumPulses array (except for pulse trains)
               So number of increments in pulse families no longer set to 1
               when additional single pulse elements added
  24.05.06 ... Holding voltage no longer defined in stimulus protocol
               Segment of last step now always displayed
  23.06.06 ... RecordFrm.UpdateStimProgramList & RecADCOnlyFrm.UpdateStimProgramList
               now called when this form is closed

  11.10.06 .. Series of pulse trains protocol added.
  26.01.07 .. Stimulus protocols can now be linked together
              Stimulus period, repeated stimulus check box now in separate group
  15.08.07 .. Digital pulse with incremented delay added
  28.08.07 .. Ptrain1 (Incrementing series of pulse trains removed
  14.01.08 .. Digital outputs via DACs now work
  04.10.09 .. NS Added third DAC channel, Vout2
  06.07.10 .. JD Waveform display channels now only appear when
              a stimulus protocol waveform exists in that channel.
              Divide factor box for Vout #2 added.
              Display form made more compact.
              Digital waveforms can now be erased by dragging blank TTL waveform
              on to protocol.
  09.09.10 .. JD Included VHold in calculation of upper and lower limits of
                 stimulus amplitude display
  31.01.13 .. DE Allow existing waveforms to be dragged and dropped
  24.04.13 .. JD SelectDirectory() dialog now used to select protocol folder
  =====================================================================}
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Grids, SHared, ClipBrd,
  maths, Spin, ScopeDisplay, ValEdit, ValidatedEdit, LabIOUnit, math, strutils,
  FileCtrl  ;

const
    NumVChannels = 3 ;
    NumDigChannels = 8 ;
    NumDisplayChannels = NumVChannels + NumDigChannels ;
    // Plot channels
    VChan0 = 0 ;
    VPlotStart = 0 ;
    VPlotEnd = 2 ;
    VChan1 = 1 ;
    VChan2 = 2 ;
    DigPlotStart = 3 ;
    DigPlotEnd = 10 ;

    MaxProtocolShapes = 38 ;
    // VChan0 shape #s
    VChan0Start = 0 ;
    VChan0End = 9 ;
    // Digital shape #s
    DigStart = 10 ;
    DigEnd = 17 ;
    // VChan1 shape #s
    VChan1Start = 18 ;
    VChan1End = 27 ;
    // VChan2 shape #s
    VChan2Start = 28 ;
    VChan2End = 37 ;

type

  TDisplayChannel = record
      X : Single ;
      Y : Single ;
      XStart : Single ;
      YStart : Single ;
      XScale : Single ;
      YScale : Single ;
      XZero : Single ;
      YZero : Single ;
      end ;

  TWavGenFrm = class(TForm)
    ControlGrp: TGroupBox;
    bOpen: TButton;
    bSave: TButton;
    bNew: TButton;
    ProtocolGrp: TGroupBox;
    Prot1: TImage;
    Prot2: TImage;
    Prot3: TImage;
    Prot4: TImage;
    Prot5: TImage;
    Prot6: TImage;
    Prot7: TImage;
    Prot8: TImage;
    Prot9: TImage;
    Prot0: TImage;
    TableGrp: TGroupBox;
    Table: TStringGrid;
    ToolBoxGrp: TGroupBox;
    Step1: TImage;
    Step2: TImage;
    pTrain: TImage;
    Ramp: TImage;
    Wave: TImage;
    Step0: TImage;
    None: TImage;
    Label1: TLabel;
    dig0: TImage;
    Dig1: TImage;
    Dig2: TImage;
    Dig3: TImage;
    Dig4: TImage;
    Dig5: TImage;
    Dig6: TImage;
    Dig7: TImage;
    lbDig0: TLabel;
    DigStep0: TImage;
    DigStep1: TImage;
    DigTrain: TImage;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    DACGrp: TGroupBox;
    Label4: TLabel;
    shSelected: TShape;
    bPasteFromClipboard: TButton;
    Timer1: TTimer;
    edVDivide0: TValidatedEdit;
    lbDig1: TLabel;
    lbDig2: TLabel;
    lbDig3: TLabel;
    lbDig4: TLabel;
    lbDig5: TLabel;
    lbDig6: TLabel;
    lbDig7: TLabel;
    DigNone: TImage;
    Label2: TLabel;
    Label6: TLabel;
    DisplayGrp: TGroupBox;
    pbDisplay: TPaintBox;
    Label7: TLabel;
    Prot18: TImage;
    Prot19: TImage;
    Prot20: TImage;
    Prot21: TImage;
    Prot22: TImage;
    Prot23: TImage;
    Prot24: TImage;
    Prot25: TImage;
    Prot26: TImage;
    Prot27: TImage;
    edVDivide1: TValidatedEdit;
    Label8: TLabel;
    PeriodGrp: TGroupBox;
    ckRepeatedStim: TCheckBox;
    cbNextProgram: TComboBox;
    Label3: TLabel;
    edStimPeriod: TValidatedEdit;
    Label5: TLabel;
    bSetSubFolder: TButton;
    edFolder: TEdit;
    Label9: TLabel;
    DigDelay: TImage;
    Label10: TLabel;
    Prot28: TImage;
    Prot29: TImage;
    Prot30: TImage;
    Prot31: TImage;
    Prot32: TImage;
    Prot33: TImage;
    Prot34: TImage;
    Prot35: TImage;
    Prot36: TImage;
    Prot37: TImage;
    Label11: TLabel;
    edVDivide2: TValidatedEdit;
    procedure Prot0DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Prot0DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Step0MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Prot0MouseDown(Sender: TObject; Button: TMouseButton;
                             Shift: TShiftState; X, Y: Integer);
    procedure Prot0Click(Sender: TObject);
    procedure DigStep0MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DigProt0MouseDown(Sender: TObject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure bNewClick(Sender: TObject);
    procedure TableKeyPress(Sender: TObject; var Key: Char);
    procedure bSaveClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure dig0DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure dig0Click(Sender: TObject);
    procedure dig0DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure bPasteFromClipboardClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TableExit(Sender: TObject);
    procedure pbDisplayPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure edVDivide0KeyPress(Sender: TObject; var Key: Char);
    procedure edVDivide1KeyPress(Sender: TObject; var Key: Char);
    procedure edVDivide2KeyPress(Sender: TObject; var Key: Char);    
    procedure edStimPeriodKeyPress(Sender: TObject; var Key: Char);
    procedure ckRepeatedStimClick(Sender: TObject);
    procedure cbNextProgramChange(Sender: TObject);
    procedure bSetSubFolderClick(Sender: TObject);
    procedure DigNoneMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  private
    { Private declarations }
   PlotChannel : Array[0..MaxProtocolShapes-1] of Integer ;
   DisplayChannel : Array[0..31] of TDisplayChannel ;

   // Stimulus channel in use flags.
   VChanInUse : Array[0..2] of Boolean ;
   DigChanInUse : Boolean ;


    Procedure SetCursor( SetDrag : Boolean ) ;
    procedure FillParameterTable( Num : Integer ; FirstTime : Boolean ) ;

    procedure NewWaveform ;
    procedure LoadWaveform( FileName : string ) ;
    procedure SaveWaveform( FileName : string ) ;
    procedure UpdateIcons ;

    {procedure CheckWaveformDuration ;}

    procedure UpdateEntry( const SourceText : string ;
                       var Destination : single ;
                       ConversionFactor : single ;
                       PositiveOnly : boolean ) ;
    procedure UpdateWaveform ;

    procedure PlotVStep(
              ChanNum : Integer ;
              Amplitude : Single ;
              Time : Single
              ) ;
    procedure PlotVLine(
              ChanNum : Integer ;
              Amplitude : Single ;
              Time : Single
              ) ;
    procedure PlotDigStep(
              ChanNum : Integer ;
              Polarity : Boolean ;
              Time : Single
              ) ;

   procedure DisplayStimulusProtocol ;
   function ProtocolDuration : single ;
  end;

var
  WavGenFrm: TWavGenFrm;

implementation

uses Main, StimModule, RecUnit, RecADCOnlyUnit ;

{$R *.DFM}



Type
    TCurrentTool = record
                 ToolType : TWaveShape ;
                 Digital : Boolean ;
                 IsCopy: Boolean; // Copying an existing waveform's parameters
                 CopyingFrom: Integer; // Tag of element copied from
                 end ;


var
   CurrentTool : TCurrentTool ;
   CurrentWaveformNum : Integer ;
   ImageIndex : Array[0..22] of Integer ;
   TableChanged : boolean ;


procedure TWavGenFrm.FormShow(Sender: TObject);
{ ----------------------------------
  Initial setup when form is opened
  ---------------------------------}
var
   i : Integer ;
begin

        Dig4.visible := True ;
        Dig5.visible := True ;
        Dig6.visible := True ;
        Dig7.visible := True ;

     // Clear stimulus channel in use flags
     for i := 0 to High(VChanInUse) do VChanInUse[i] := False ;
     DigChanInUse := False  ;

     Stimulator.Prog.RecordDuration := 0.5 ;
     Stimulator.Prog.RecordInterval := 1. ;
     Stimulator.Prog.HoldingVoltage[0] := 0. ;
     Stimulator.Prog.HoldingVoltage[1] := 0. ;
     Stimulator.Prog.HoldingVoltage[2] := 0. ;

     for i := 0 to High(Stimulator.Prog.Shape) do Stimulator.Prog.Shape[i] := wvNone ;

     { Set tag and hint properties of tool components }
     { Note. tags contain the waveform type used in the TWaveform record }
     Step0.Tag := Ord( wvStep0 ) ;
     ImageIndex[Step0.Tag] := Step0.ComponentIndex ;

     Step1.Tag := Ord( wvStep1 ) ;
     ImageIndex[Step1.Tag] := Step1.ComponentIndex ;

     Step2.Tag := Ord( wvStep2 ) ;
     ImageIndex[Step2.Tag] := Step2.ComponentIndex ;

     Ramp.Tag := Ord( wvRamp ) ;
     ImageIndex[Ramp.Tag] := Ramp.ComponentIndex ;

     PTRain.Tag := Ord( wvPTrain ) ;
     ImageIndex[PTrain.Tag] := PTrain.ComponentIndex ;

     Wave.Tag := Ord( wvWave ) ;
     ImageIndex[Wave.Tag] := Wave.ComponentIndex ;

     None.Tag := Ord(wvNone) ;
     None.hint := 'No DAC waveform' ;
     ImageIndex[None.Tag] := None.ComponentIndex ;

     DigStep0.tag := Ord(wvDigStep0) ;
     ImageIndex[DigStep0.Tag] := DigStep0.ComponentIndex ;

     DigStep1.tag := Ord(wvDigStep1) ;
     ImageIndex[DigStep1.Tag] := DigStep1.ComponentIndex ;

     DigTrain.tag := Ord(wvDigTrain) ;
     ImageIndex[DigTrain.Tag] := DigTrain.ComponentIndex ;

     DigDelay.tag := Ord(wvDigDelay) ;
     ImageIndex[DigDelay.Tag] := DigDelay.ComponentIndex ;

     DigNone.Tag := Ord(wvDigNone) ;
     DigNone.hint := 'No digital waveform' ;
     ImageIndex[DigNone.Tag] := DigNone.ComponentIndex ;

     { Display general recording sweep parameter table}

     //  Note the following two values are stored as part of
     //  the settings data stored in the INI file not the .VPR file

     // Patch/voltage clamp command voltage divide factor
     edVDivide0.Value := MainFrm.VCommand[0].DivideFactor ;
     edVDivide1.Value := MainFrm.VCommand[1].DivideFactor ;
     edVDivide2.Value := MainFrm.VCommand[2].DivideFactor ;

     // Define shape-> plot channel mappings
     for i := VChan0Start to VChan0End do PlotChannel[i] := VChan0 ;
     for i := VChan1Start to VChan1End do PlotChannel[i] := VChan1 ;
     for i := VChan2Start to VChan2End do PlotChannel[i] := VChan2 ;
     for i := DigStart to DigEnd do PlotChannel[i] :=  DigPlotStart + i - DigStart ;

     { Load the voltage protocol in current use (if any) }
     if (MainFrm.StimFileName <> '' ) and FileExists(MainFrm.StimFileName) then
        LoadWaveform(MainFrm.StimFileName)
     else NewWaveform ;

     Timer1.enabled := True ;
     TableChanged := False ;

     edFolder.Text := MainFrm.VProtDirectory ;

     ClientWidth := ProtocolGrp.Left + ProtocolGrp.Width + 5 ;
     ClientHeight := ProtocolGrp.Top + ProtocolGrp.Height + 5 ;

     end ;


procedure TWavGenFrm.LoadWaveform(
          FileName : string
          ) ;
{ ------------------------------
  Load an existing waveform file
  ------------------------------ }
begin

     // Clear all waveform elements in protocol
     Stimulator.ClearWaveformElements ;

     FileName := ChangeFileExt( FileName, '.vpr' ) ;
     Caption := 'Stimulus Protocol : ' + ExtractFileNameOnly(FileName) ;
     
     { Get voltage program from VPR file }
     Stimulator.LoadProgram( FileName ) ;

     UpdateIcons ;

     // Display first protocol
     shSelected.brush.style := bsClear ;
     shSelected.Left := Prot0.Left ;
     shSelected.Top := Prot0.Top + Prot0.Height ;
     shSelected.Width := Prot0.Width ;
     FillParameterTable(TIMage(Prot0).Tag,False) ;

     Stimulator.Prog.Saved := True ;

     end ;


procedure TWavGenFrm.NewWaveform ;
{ --------------------------
  Create a new waveform file
  -------------------------- }
begin

     // Clear all waveform elements in protocol
     Stimulator.ClearWaveformElements ;

     { Update waveform icons }
     UpdateIcons ;

     Stimulator.Prog.NumRepeats := 1 ;
     Stimulator.Prog.NumSteps := 1 ;
     Stimulator.Prog.RecordDuration := 0.5 ;
     Stimulator.Prog.RecordInterval := 1. ;
     Stimulator.Prog.HoldingVoltage[0] := 0. ;
     Stimulator.Prog.HoldingVoltage[1] := 0. ;
     Stimulator.Prog.HoldingVoltage[2] := 0. ;     
     Stimulator.Prog.NumLeaks := 0 ;
     Stimulator.Prog.DigitalInUse := False ;
     Stimulator.FileName := MainFrm.VProtDirectory + 'Untitled.vpr' ;

     Stimulator.Prog.RecordIntervalChanged := False ;
     Stimulator.Prog.ExtEndofData := -1 ;
     Stimulator.Prog.DACdt := 0.001 ;
     Stimulator.Prog.ExtDACdt := 0.001 ;
     FillParameterTable(-1,False) ;
{     shSelected.BoundsRect := RecSweep.BoundsRect ;}
     shSelected.Height := 2 ;
     shSelected.Width := Prot0.Width ;
     shSelected.Top := Prot0.Top + Prot0.Height ;

     pbDisplay.Invalidate ;

     Stimulator.Prog.Saved := False ;

     // Update list of stimulus programs
     Stimulator.CreateProgramList( cbNextProgram ) ;

     end;


procedure TWavGenFrm.SaveWaveform(
          FileName : string
          ) ;
{ -------------------------
  Save waveform to VPR file
  ------------------------- }
begin

     // Update settings
     UpdateWaveform ;

     FileName := ChangeFileExt( FileName, '.vpr' ) ;
     Caption := 'Stimulus Protocol : ' + ExtractFileNameOnly(FileName) ;
     Stimulator.SaveProgram( FileName ) ;

     // Update list of stimulus programs
     Stimulator.CreateProgramList( cbNextProgram ) ;

     end ;


procedure TWavGenFrm.UpdateIcons ;
{ -----------------------------------
  Update waveform shape display icons
  ----------------------------------- }
var
    i : Integer ;
begin

      Caption := 'Stimulus Protocol : ' + ExtractFileNameOnly(Stimulator.FileName) ;

      // VChan1 waveform element
      Prot0.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[0])]]).Picture ;
      Prot1.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[1])]]).Picture ;
      Prot2.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[2])]]).Picture ;
      Prot3.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[3])]]).Picture ;
      Prot4.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[4])]]).Picture ;
      Prot5.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[5])]]).Picture ;
      Prot6.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[6])]]).Picture ;
      Prot7.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[7])]]).Picture ;
      Prot8.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[8])]]).Picture ;
      Prot9.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[9])]]).Picture ;

      // Ensure digital waveform elements have correct empty icon
      for i := DigStart to DigEnd do if Stimulator.Prog.Shape[i] = wvNone then
          Stimulator.Prog.Shape[i] := wvDigNone ;

      // Digital waveform elements
      Dig0.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[10])]]).Picture ;
      Dig1.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[11])]]).Picture ;
      Dig2.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[12])]]).Picture ;
      Dig3.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[13])]]).Picture ;
      Dig4.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[14])]]).Picture ;
      Dig5.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[15])]]).Picture ;
      Dig6.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[16])]]).Picture ;
      Dig7.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[17])]]).Picture ;

      // VChan1 waveform element
      Prot18.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[18])]]).Picture ;
      Prot19.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[19])]]).Picture ;
      Prot20.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[20])]]).Picture ;
      Prot21.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[21])]]).Picture ;
      Prot22.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[22])]]).Picture ;
      Prot23.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[23])]]).Picture ;
      Prot24.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[24])]]).Picture ;
      Prot25.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[25])]]).Picture ;
      Prot26.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[26])]]).Picture ;
      Prot27.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[27])]]).Picture ;

      // VChan2 waveform element
      Prot28.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[28])]]).Picture ;
      Prot29.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[29])]]).Picture ;
      Prot30.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[30])]]).Picture ;
      Prot31.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[31])]]).Picture ;
      Prot32.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[32])]]).Picture ;
      Prot33.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[33])]]).Picture ;
      Prot34.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[34])]]).Picture ;
      Prot35.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[35])]]).Picture ;
      Prot36.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[36])]]).Picture ;
      Prot37.Picture := TIMage(Components[ImageIndex[Ord(Stimulator.Prog.Shape[37])]]).Picture ;

      end ;


Procedure TWavGenFrm.SetCursor( SetDrag : Boolean ) ;
begin
     if SetDrag then begin
        Cursor := crDrag ;
        Step1.Cursor := crDrag ;
        ProtocolGrp.Cursor := crDrag ;
        ToolBoxGrp.Cursor := crDrag ;
        end
     else begin
          Cursor := crDefault ;
          Step1.Cursor :=crDefault ;
          ProtocolGrp.Cursor := crDefault ;
          ToolBoxGrp.Cursor := crDefault ;
          end ;
     end ;


{ -------------------------------------------------
  Mouse down events when a protocol tool is selected
  --------------------------------------------------}

procedure TWavGenFrm.Step0MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     CurrentTool.ToolType := TWaveShape( TImage(Sender).Tag ) ;
             CurrentTool.Digital := False ;
     CurrentTool.IsCopy := False;
     TIMage(Sender).BeginDrag( False ) ;
     end        ;

procedure TWavGenFrm.Prot0MouseDown(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
begin
  CurrentTool.ToolType := TWaveShape(Stimulator.Prog.Shape[TImage(Sender).Tag]);
  CurrentTool.Digital := False;
  CurrentTool.IsCopy := True;
  CurrentTool.CopyingFrom := TImage(Sender).Tag;
  TImage(Sender).BeginDrag(False);
end;

procedure TWavGenFrm.DigStep0MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     CurrentTool.ToolType := TWaveShape( TImage(Sender).Tag ) ;
     CurrentTool.Digital := True ;
     CurrentTool.IsCopy := False;
     TIMage(Sender).BeginDrag( False ) ;
     end;

procedure TWavGenFrm.DigProt0MouseDown(Sender: TObject; Button: TMouseButton;
                                       Shift: TShiftState; X, Y: Integer);
begin
  CurrentTool.ToolType := TWaveShape(Stimulator.Prog.Shape[TImage(Sender).Tag]);
  CurrentTool.Digital := True;
  CurrentTool.IsCopy := True;
  CurrentTool.CopyingFrom := TImage(Sender).Tag;
  TImage(Sender).BeginDrag(False);
end;


{ -----------------------------------------------------------------
  When a protocol block is clicked its parameter table is displayed
  -----------------------------------------------------------------}

procedure TWavGenFrm.Prot0Click(Sender: TObject);
// -----------------------------------------
// Voltage waveform protocol element clicked
// -----------------------------------------
begin
     if TableChanged then UpdateWaveform ;
     FillParameterTable(TIMage(Sender).Tag,False) ;
     shSelected.Left := TIMage(Sender).Left ;
     shSelected.Top := TIMage(Sender).Top + TIMage(Sender).Height ;
     shSelected.Width := TIMage(Sender).Width ;
     end;


procedure TWavGenFrm.dig0Click(Sender: TObject);
// -----------------------------------------
// Digital waveform protocol element clicked
// -----------------------------------------
begin
     if TableChanged then UpdateWaveform ;
     FillParameterTable(TIMage(Sender).Tag,False) ;
     shSelected.Left := TIMage(Sender).Left ;
     shSelected.Top := TIMage(Sender).Top + TIMage(Sender).Height ;
     shSelected.Width := TIMage(Sender).Width ;
     end;


{ ---------------------------------------------------------------
  Procedures when a waveform shape is dropped on a protocol block
  ---------------------------------------------------------------}

procedure TWavGenFrm.Prot0DragDrop(Sender, Source: TObject; X, Y: Integer);
{ ------------------------------------------------------------
  Accept analogue waveform tool dropped on to waveform icon
  ------------------------------------------------------------}
var
   iTag : Integer ;
   FromTag: Integer;
begin
     iTag := TImage(Sender).Tag ;
     if  (not CurrentTool.Digital) then begin
        TIMage(Sender).Picture := TImage(Source).Picture ;
        if (CurrentTool.IsCopy) then
        begin
          FromTag := CurrentTool.CopyingFrom;
          Stimulator.Prog.Shape[iTag] := Stimulator.Prog.Shape[FromTag];
          Stimulator.Prog.Amplitude[iTag] := Stimulator.Prog.Amplitude[FromTag];
          Stimulator.Prog.Duration[iTag] := Stimulator.Prog.Duration[FromTag];
          Stimulator.Prog.Increment[iTag] := Stimulator.Prog.Increment[FromTag];
          Stimulator.Prog.RampStart[iTag] := Stimulator.Prog.RampStart[FromTag];
          Stimulator.Prog.RampEnd[iTag] := Stimulator.Prog.RampEnd[FromTag];
          Stimulator.Prog.PulseInterval[iTag] := Stimulator.Prog.PulseInterval[FromTag];
          Stimulator.Prog.NumPulses[iTag] := Stimulator.Prog.NumPulses[FromTag];
          Stimulator.Prog.Period[iTag] := Stimulator.Prog.Period[FromTag];
          Stimulator.Prog.Delay[iTag] := Stimulator.Prog.Delay[FromTag];
          Stimulator.Prog.Invert[iTag] := Stimulator.Prog.Invert[FromTag];
        end else
        begin
          Stimulator.Prog.Shape[iTag] := TWaveShape( TImage(Source).Tag ) ;
        end;
        end ;
     SetCursor( False ) ;
     FillParameterTable(iTag, not CurrentTool.IsCopy) ;
     shSelected.Left := TIMage(Sender).Left ;
     shSelected.Top := TIMage(Sender).Top + TIMage(Sender).Height ;
     shSelected.Width := TIMage(Sender).Width ;
     Stimulator.Prog.Saved := False ;
     pbDisplay.invalidate ;
     end;


procedure TWavGenFrm.dig0DragDrop(Sender, Source: TObject; X, Y: Integer);
{ ------------------------------------------------------------
  Accept digital waveform tool dropped on to digital port icon
  ------------------------------------------------------------}
var
   iTag : Integer ;
   FromTag: Integer;
begin
     { Tag property of image icon provides the index into Shape array }
     iTag := TImage(Sender).Tag ;
     { Update the port icon with the digital port tool's image }
     // if CurrentTool.Digital or (TWaveShape(TImage(Source).Tag) = wvNone) then begin
     if CurrentTool.Digital then begin
        TIMage(Sender).Picture := TImage(Source).Picture ;
        if (CurrentTool.IsCopy) then
        begin
          FromTag := CurrentTool.CopyingFrom;
          Stimulator.Prog.Shape[iTag] := Stimulator.Prog.Shape[FromTag];
          Stimulator.Prog.Amplitude[iTag] := Stimulator.Prog.Amplitude[FromTag];
          Stimulator.Prog.Duration[iTag] := Stimulator.Prog.Duration[FromTag];
          Stimulator.Prog.Increment[iTag] := Stimulator.Prog.Increment[FromTag];
          Stimulator.Prog.RampStart[iTag] := Stimulator.Prog.RampStart[FromTag];
          Stimulator.Prog.RampEnd[iTag] := Stimulator.Prog.RampEnd[FromTag];
          Stimulator.Prog.PulseInterval[iTag] := Stimulator.Prog.PulseInterval[FromTag];
          Stimulator.Prog.NumPulses[iTag] := Stimulator.Prog.NumPulses[FromTag];
          Stimulator.Prog.Period[iTag] := Stimulator.Prog.Period[FromTag];
          Stimulator.Prog.Delay[iTag] := Stimulator.Prog.Delay[FromTag];
          Stimulator.Prog.Invert[iTag] := Stimulator.Prog.Invert[FromTag];
        end else
        begin
          Stimulator.Prog.Shape[iTag] := TWaveShape( TImage(Source).Tag ) ;
        end;
        end ;
     { Update waveform parameter table }
     SetCursor( False ) ;
     FillParameterTable(iTag, not CurrentTool.IsCopy) ;
     shSelected.Left := TIMage(Sender).Left ;
     shSelected.Top := TIMage(Sender).Top + TIMage(Sender).Height ;
     shSelected.Width := TIMage(Sender).Width ;
     Stimulator.Prog.Saved := False ;
     pbDisplay.invalidate ;
     end;


procedure TWavGenFrm.Prot0DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
{ -----------------------------------------------------------------
  If the dragged object is compatible with this waveform component
  change the cursor to acceptable (Note this procedure used by all
  10 protocol items
  ----------------------------------------------------------------}
begin
     if (Source is TBitmap) and (not CurrentTool.Digital) then accept := True ;
     end;


procedure TWavGenFrm.dig0DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
{ -----------------------------------------------------------------
  If the dragged object is compatible with a digital port icon
  change the cursor to acceptable (Note this procedure used by all
  8 digital ports
  ----------------------------------------------------------------}
begin
     if (Source is TBitmap) and CurrentTool.Digital then accept := True ;
     end;


procedure TWavgenFrm.FillParameterTable( Num : Integer ; FirstTime : Boolean ) ;
{ -------------------------------------------
  Fill waveform parameter table with settings
  -------------------------------------------}
var
   i,MaxWidth,RowHeight : Integer ;
   NamePart : String ;
begin

     CurrentWaveformNum := Num ;

     Table.options := [goFixedVertLine,goFixedHorzLine,
                       goVertLine,goHorzLine,goEditing] ;

     { Disable the Paste button (unless in user-entered waveform option) }
     bPasteFromClipboard.enabled := false ;

     edStimPeriod.Value := Stimulator.Prog.RecordInterval ;
     if Stimulator.Prog.NumRepeats > 1 then ckRepeatedStim.Checked := True
                                       else ckRepeatedStim.Checked := False ;

     // Update list of stimulus programs

     Stimulator.CreateProgramList( cbNextProgram ) ;
     if Stimulator.Prog.NextProtocolFileName <> '' then begin
        // Find name within list
        NamePart := AnsiReplaceStr(LowerCase(
                    ExtractFileName(Stimulator.Prog.NextProtocolFileName)),'.vpr', '' ) ;
        cbNextProgram.ItemIndex := cbNextProgram.Items.IndexOf(NamePart) ;
        end
     else begin
        cbNextProgram.ItemIndex := 0 ;
        end ;

     if Num >= 0 then begin
        case Stimulator.Prog.Shape[Num] of
        wvNone, wvDigNone : begin
             { No waveform }
             Table.RowCount := 1 ;
             Table.ColCount := 2 ;
             TableGrp.caption := ' No waveform ' ;
             Table.cells[0,0] := '' ;
             Table.cells[1,0] := '' ;
             end ;
        wvStep0 : begin
             if FirstTime then begin
                if Num = 0 then Stimulator.Prog.Delay[Num] := Stimulator.Prog.RecordDuration * 0.1
                           else Stimulator.Prog.Delay[Num] := 0. ;
                Stimulator.Prog.Amplitude[Num] := 0.01 ;
                Stimulator.Prog.Duration[Num] := Stimulator.Prog.RecordDuration * 0.8 ;
                Stimulator.Prog.NumSteps := Max( Stimulator.Prog.NumSteps,1 ) ;
                end ;
             { Rectangular step (fixed size) }
             Table.RowCount := 3 ;
             Table.ColCount := 2 ;
             TableGrp.caption := ' Rectangular pulse (fixed size) ' ;
             Table.cells[0,0] := ' Delay ' ;
             Table.cells[1,0] := format('%.4f %s',
                                 [Stimulator.Prog.Delay[Num]*Mainfrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,1] := ' Amplitude ' ;
             Table.cells[1,1] := format( '%.2f mV', [Stimulator.Prog.Amplitude[Num]*VoltsTomV] ) ;
             Table.cells[0,2] := ' Duration ' ;
             Table.cells[1,2] := format('%.4f %s',
                                 [Stimulator.Prog.Duration[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Stimulator.Prog.NumPulses[Num] := 1 ;
             end ;

        wvStep1 : begin
             { Family of steps varying in amplitude }
             if FirstTime then begin
                if Num = 0 then Stimulator.Prog.Delay[Num] := Stimulator.Prog.RecordDuration * 0.1
                           else Stimulator.Prog.Delay[Num] := 0. ;
                Stimulator.Prog.Amplitude[Num] := 0.01 ;
                Stimulator.Prog.Increment[Num] := 0.01 ;
                Stimulator.Prog.Duration[Num] := Stimulator.Prog.RecordDuration * 0.8 ;
                Stimulator.Prog.NumSteps := 10 ;
                Stimulator.Prog.NumPulses[Num] := 10 ;
                end ;

             TableGrp.caption := ' Family of rectangular pulses (varying in amplitude) ' ;
             Table.RowCount := 5 ;
             Table.ColCount := 2 ;
             Table.cells[0,0] := ' Delay ' ;
             Table.cells[1,0] := format('%.4f %s',
                                 [Stimulator.Prog.Delay[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,1] := ' Initial Amplitude ' ;
             Table.cells[1,1] := format( '%.2f mV', [Stimulator.Prog.Amplitude[Num]*VoltsTomV] ) ;
             Table.cells[0,2] := ' Increment by ' ;
             Table.cells[1,2] := format( '%.2f mV', [Stimulator.Prog.Increment[Num]*VoltsTomV] ) ;
             Table.cells[0,3] := ' Number of increments ' ;
             // Get value from NumSteps to maintain compatibility with pre V3.5.6 protocol files
             Stimulator.Prog.NumPulses[Num] := Stimulator.Prog.NumSteps ;
             Table.cells[1,3] := format( '%5d', [Stimulator.Prog.NumPulses[Num]] ) ;
             Table.cells[0,4] := ' Duration ' ;
             Table.cells[1,4] := format('%.4f %s',
                                 [Stimulator.Prog.Duration[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             end ;

        wvStep2 : begin
             { Family of steps varying in duration }
             if FirstTime then begin
                if Num = 0 then Stimulator.Prog.Delay[Num] := Stimulator.Prog.RecordDuration * 0.1
                           else Stimulator.Prog.Delay[Num] := 0. ;
                Stimulator.Prog.Amplitude[Num] := 0.01 ;
                Stimulator.Prog.Duration[Num] := Stimulator.Prog.RecordDuration * 0.8 ;
                Stimulator.Prog.Increment[Num] := Stimulator.Prog.Duration[Num] * 0.1  ;
                Stimulator.Prog.NumSteps := 10 ;
                end ;
             TableGrp.caption := ' Family of rectangular pulses (varying in duration) ' ;
             Table.RowCount := 5 ;
             Table.ColCount := 2 ;
             Table.cells[0,0] := ' Delay ' ;
             Table.cells[1,0] := format('%.4f %s',
                                 [Stimulator.Prog.Delay[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,1] := ' Amplitude ' ;
             Table.cells[1,1] := format( '%.2f mV',[Stimulator.Prog.Amplitude[Num]*VoltsTomV] ) ;
             Table.cells[0,2] := ' Initial Duration ' ;
             Table.cells[1,2] := format('%.4f %s',
                                 [Stimulator.Prog.Duration[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,3] := ' Increment by ' ;
             Table.cells[1,3] := format('%.4f %s',
                                 [Stimulator.Prog.Increment[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,4] := ' Number of increments ' ;
             // Get value from NumSteps to maintain compatibility with pre V3.5.6 protocol files
             Stimulator.Prog.NumPulses[Num] := Stimulator.Prog.NumSteps ;
             Table.cells[1,4] := format( '%5d', [Stimulator.Prog.NumPulses[Num]] ) ;
             end ;

          wvRamp : begin
             { Voltage ramp }
             if FirstTime then begin
                if Num = 0 then Stimulator.Prog.Delay[Num] := Stimulator.Prog.RecordDuration * 0.1
                           else Stimulator.Prog.Delay[Num] := 0. ;
                Stimulator.Prog.RampStart[Num] := -0.1 ;
                Stimulator.Prog.RampEnd[Num] := 0.1 ;
                Stimulator.Prog.Duration[Num] := Stimulator.Prog.RecordDuration * 0.8 ;
                Stimulator.Prog.NumSteps := Max( Stimulator.Prog.NumSteps,1 ) ;
                end ;
             TableGrp.caption := ' Voltage ramp ' ;
             Table.RowCount := 4;
             Table.ColCount := 2 ;
             Table.cells[0,0] := ' Delay ' ;
             Table.cells[1,0] := format('%.4f %s',[Stimulator.Prog.Delay[Num]*MainFrm.TScale,
                                                   MainFrm.TUnits] ) ;
             Table.cells[0,1] := ' Initial Amplitude ' ;
             Table.cells[1,1] := format( '%.2f mV', [Stimulator.Prog.RampStart[Num]*VoltsTomV] ) ;
             Table.cells[0,2] := ' Final Amplitude ' ;
             Table.cells[1,2] := format( '%.2f mV', [Stimulator.Prog.RampEnd[Num]*VoltsTomV] ) ;
             Table.cells[0,3] := ' Duration ' ;
             Table.cells[1,3] := format('%.4f %s',
                                 [Stimulator.Prog.Duration[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             end ;

          wvPTrain : begin
             { Train of pulses }
             if FirstTime then begin

                if Num = 0 then Stimulator.Prog.Delay[Num] := Stimulator.Prog.RecordDuration * 0.1
                           else Stimulator.Prog.Delay[Num] := 0. ;
                Stimulator.Prog.Amplitude[Num] := 0.01 ;
                Stimulator.Prog.Duration[Num] := 0.01 ;
                Stimulator.Prog.PulseInterval[Num] := 0.1 ;
                Stimulator.Prog.NumPulses[Num] := 2 ;
                Stimulator.Prog.NumSteps := Max( Stimulator.Prog.NumSteps,1 ) ;
                end ;

             TableGrp.caption := ' Pulse train ' ;
             Table.RowCount := 5 ;
             Table.ColCount := 2 ;
             Table.cells[0,0] := ' Delay ' ;
             Table.cells[1,0] := format( '%.4f %s',
                                 [Stimulator.Prog.Delay[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,1] := ' Amplitude ' ;
             Table.cells[1,1] := format( '%.2f mV', [Stimulator.Prog.Amplitude[Num]*VoltsTomV] ) ;
             Table.cells[0,2] := ' Duration ' ;
             Table.cells[1,2] := format( '%.4f %s',
                                 [Stimulator.Prog.Duration[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,3] := ' Pulse interval ' ;
             Table.cells[1,3] := format( '%.4f %s',
                                 [Stimulator.Prog.PulseInterval[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,4] := ' Number of pulses ' ;
             Table.cells[1,4] := format( '%5d', [Stimulator.Prog.NumPulses[Num]] ) ;

             end ;

          wvWave : begin
             { Voltage waveform from file }
             TableGrp.caption := ' Waveform from file ' ;
             Table.RowCount := Stimulator.Prog.ExtEndOfData + 2 ;
             Table.ColCount := 2 ;
             Table.cells[0,0] := ' Delay ' ;
             Table.cells[1,0] := format('%.3g %s',
                                 [Stimulator.Prog.Delay[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             for i := 0 to Stimulator.Prog.ExtEndofData do begin
                 Table.cells[0,i+1] := format( '%.3g %s',
                                       [i*Stimulator.Prog.ExtDACdt*MainFrm.TScale,MainFrm.TUnits]) ;
                 Table.cells[1,i+1] := format( '%.3g mV',
                                              [Stimulator.Prog.ExtWaveData[i]*VoltsTomV]) ;
                 end ;
             Stimulator.Prog.NumSteps := Max( Stimulator.Prog.NumSteps,1 ) ;
             bPasteFromClipboard.enabled := True ;
             end ;

          wvDigStep0 : begin
             { Single digital pulse (with incrementable delay) }
             if FirstTime then begin
                Stimulator.Prog.Delay[Num] := Stimulator.Prog.RecordDuration * 0.1 ;
                Stimulator.Prog.Duration[Num] := 0.1 ;
                Stimulator.Prog.Increment[Num] := 0. ;
                Stimulator.Prog.Invert[Num] := False ;
                Stimulator.Prog.NumSteps := Max( Stimulator.Prog.NumSteps,1 ) ;
                end ;

             TableGrp.caption := ' Digital pulse ' ;
             Table.RowCount := 3 ;
             Table.ColCount := 2 ;
             Table.cells[0,0] := ' Delay ' ;
             Table.cells[1,0] := format( '%.4f %s',
                                 [Stimulator.Prog.Delay[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,1] := ' Duration ' ;
             Table.cells[1,1] := format( '%.4f %s',
                                 [Stimulator.Prog.Duration[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,2] := ' Invert Signal (Yes/No) ' ;
             if Stimulator.Prog.Invert[Num] then Table.Cells[1,2] := 'Yes'
                                            else Table.Cells[1,2] := 'No' ;

             end ;

          wvDigStep1 : begin
             { Family of digital pulse steps varying in duration }
             if FirstTime then begin
                Stimulator.Prog.Delay[Num] := Stimulator.Prog.RecordDuration * 0.1 ;
                Stimulator.Prog.Duration[Num] := 0.01 ;
                Stimulator.Prog.Increment[Num] := 0.01 ;
                Stimulator.Prog.NumSteps := 10 ;
                Stimulator.Prog.Invert[Num] := False ;
                end ;

             TableGrp.caption := ' Family of digital pulses (varying in duration) ' ;
             Table.RowCount := 5 ;
             Table.ColCount := 2 ;
             Table.cells[0,0] := ' Delay ' ;
             Table.cells[1,0] := format( '%.4f %s',
                                 [Stimulator.Prog.Delay[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,1] := ' Initial Duration ' ;
             Table.cells[1,1] := format( '%.4f %s',
                                 [Stimulator.Prog.Duration[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,2] := ' Increment by ' ;
             Table.cells[1,2] := format( '%.4f %s',
                                 [Stimulator.Prog.Increment[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,3] := ' Number of increments ' ;
             // Get value from NumSteps to maintain compatibility with pre V3.5.6 protocol files
             Stimulator.Prog.NumPulses[Num] := Stimulator.Prog.NumSteps ;
             Table.cells[1,3] := format( '%5d', [Stimulator.Prog.NumPulses[Num]] ) ;
             Table.cells[0,4] := ' Invert Signal (Yes/No) ' ;
             if Stimulator.Prog.Invert[Num] then Table.Cells[1,4] := 'Yes'
                                      else Table.Cells[1,4] := 'No'
             end ;

          wvDigTrain : begin
             { Digital pulse train }
             if FirstTime then begin
                Stimulator.Prog.Delay[Num] := Stimulator.Prog.RecordDuration * 0.1  ;
                Stimulator.Prog.Duration[Num] := 0.01 ;
                Stimulator.Prog.PulseInterval[Num] := 0.1 ;
                Stimulator.Prog.NumPulses[Num] := 2 ;
                Stimulator.Prog.Invert[Num] := False ;
                Stimulator.Prog.NumSteps := Max( Stimulator.Prog.NumSteps,1 ) ;
                end ;

             TableGrp.caption := ' Series of digital pulses ' ;
             Table.RowCount := 5 ;
             Table.ColCount := 2 ;
             Table.cells[0,0] := ' Delay ' ;
             Table.cells[1,0] := format( '%.4f %s',
                                 [Stimulator.Prog.Delay[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,1] := ' Duration ' ;
             Table.cells[1,1] := format( '%.4f %s',
                                 [Stimulator.Prog.Duration[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,2] := ' Inter-pulse interval ' ;
             Table.cells[1,2] := format( '%.4f %s',
                                 [Stimulator.Prog.PulseInterval[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,3] := ' Number of pulses ' ;
             Table.cells[1,3] := format( '%5d', [Stimulator.Prog.NumPulses[Num]] ) ;
             Table.cells[0,4] := ' Invert Signal (Yes/No) ' ;
             if Stimulator.Prog.Invert[Num] then Table.Cells[1,4] := 'Yes'
                                      else Table.Cells[1,4] := 'No'
             end ;

          wvDigDelay : begin
             { Family of digital pulses with incrementing delay }
             if FirstTime then begin
                Stimulator.Prog.Delay[Num] := Stimulator.Prog.RecordDuration * 0.1 ;
                Stimulator.Prog.Duration[Num] := 0.01 ;
                Stimulator.Prog.Increment[Num] := 0.01 ;
                Stimulator.Prog.NumSteps := 10 ;
                Stimulator.Prog.Invert[Num] := False ;
                end ;

             TableGrp.caption := ' Family of digital pulses (varying delay) ' ;
             Table.RowCount := 5 ;
             Table.ColCount := 2 ;
             Table.cells[0,0] := ' Delay ' ;
             Table.cells[1,0] := format( '%.4f %s',
                                 [Stimulator.Prog.Delay[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,1] := ' Duration ' ;
             Table.cells[1,1] := format( '%.4f %s',
                                 [Stimulator.Prog.Duration[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,2] := ' Increment delay by ' ;
             Table.cells[1,2] := format( '%.4f %s',
                                 [Stimulator.Prog.Increment[Num]*MainFrm.TScale,MainFrm.TUnits] ) ;
             Table.cells[0,3] := ' Number of increments ' ;
             // Get value from NumSteps to maintain compatibility with pre V3.5.6 protocol files
             Stimulator.Prog.NumPulses[Num] := Stimulator.Prog.NumSteps ;
             Table.cells[1,3] := format( '%5d', [Stimulator.Prog.NumPulses[Num]] ) ;
             Table.cells[0,4] := ' Invert Signal (Yes/No) ' ;
             if Stimulator.Prog.Invert[Num] then Table.Cells[1,4] := 'Yes'
                                      else Table.Cells[1,4] := 'No'
             end ;

          end ;




          end ;

        MaxWidth := 0 ;
        RowHeight := (4*Table.canvas.TextHeight(Table.cells[0,0])) div 3 ;
        for i := 0 to Table.RowCount-1 do begin
            Table.RowHeights[i] := RowHeight ;
            if Table.canvas.TextWidth(Table.cells[0,i]+' ') > MaxWidth then
               MaxWidth := Table.canvas.TextWidth(Table.cells[0,i]+' ') ;
            end ;
        Table.ColWidths[0] := MaxWidth + 10 ;


        end ;


procedure TWavGenFrm.bNewClick(Sender: TObject);
{ ----------------------------
  Create a new voltage program
  ---------------------------- }
begin
     { Create a new voltage program }
     NewWaveform ;
     end;


procedure TWavGenFrm.TableKeyPress(Sender: TObject; var Key: Char);
// ----------------------------------------------
// Update waveform parameters when CR key pressed
// ----------------------------------------------
begin
     TableChanged := True ;
     if key = #13 then begin
        UpdateWaveform ;
        FillParameterTable(CurrentWaveformNum,False) ;
        end ;
     end ;


procedure TWavGenFrm.UpdateWaveform ;
{ ----------------------------------------------
  Update appropriate variable in waveform record
  when user makes a change to parameter table
  ---------------------------------------------- }
var
   Num, i {, iStart, iEnd} : Integer ;
   NumSteps : Integer ;
   IncrementedProtocol : Boolean ;
begin

     // Copy no. of waveform increments into internal buffer
     // so each waveshape can be changed individually
     NumSteps := Stimulator.Prog.NumSteps ;

     Stimulator.Prog.RecordInterval  := edStimPeriod.Value ;
     if ckRepeatedStim.Checked then begin
        // Repeated stimulus pulses
        Stimulator.Prog.NumRepeats := 2 ;
        Stimulator.Prog.NextProtocolFileName := '' ;
        end
     else begin
        // Single stimulus pulse
        Stimulator.Prog.NumRepeats := 1 ;
        if cbNextProgram.ItemIndex <= 0 then Stimulator.Prog.NextProtocolFileName := ''
        else Stimulator.Prog.NextProtocolFileName := Mainfrm.VProtDirectory
                                                     + cbNextProgram.Text + '.vpr' ;
        end ;

     Num := CurrentWaveformNum ;
     if Num >= 0 then begin
         case Stimulator.Prog.Shape[Num] of
             wvStep0 : begin
                  { Rectangular step (fixed size) }
                  UpdateEntry(Table.Cells[1,0],Stimulator.Prog.Delay[Num],MainFrm.TScale,True) ;
                  UpdateEntry(Table.Cells[1,1],Stimulator.Prog.Amplitude[Num],VoltsTomV,False) ;
                  UpdateEntry(Table.Cells[1,2],Stimulator.Prog.Duration[Num],MainFrm.TScale,True) ;
                  end ;
             wvStep1 : begin
                  { Family of steps varying in amplitude }
                  UpdateEntry(Table.Cells[1,0],Stimulator.Prog.Delay[Num],MainFrm.TScale,True);
                  UpdateEntry(Table.Cells[1,1],Stimulator.Prog.Amplitude[Num],VoltsTomV,False);
                  UpdateEntry(Table.Cells[1,2],Stimulator.Prog.Increment[Num],VoltsTomV,False) ;
                  NumSteps := Max( ExtractInt(Table.Cells[1,3]),1 ) ;
                  UpdateEntry(Table.Cells[1,4],Stimulator.Prog.Duration[Num],MainFrm.TScale,True) ;
                  end ;
             wvStep2 : begin
                  { Family of steps varying in duration }
                  UpdateEntry(Table.Cells[1,0],Stimulator.Prog.Delay[Num],MainFrm.TScale,True);
                  UpdateEntry(Table.Cells[1,1],Stimulator.Prog.Amplitude[Num],VoltsTomV,False);
                  UpdateEntry(Table.Cells[1,2],Stimulator.Prog.Duration[Num],MainFrm.TScale,True);
                  UpdateEntry(Table.Cells[1,3],Stimulator.Prog.Increment[Num],MainFrm.TScale,False);
                  NumSteps := Max( ExtractInt(Table.Cells[1,4]),1 ) ;
                  end ;
             wvRamp : begin
                  { Voltage ramp }
                  UpdateEntry(Table.Cells[1,0],Stimulator.Prog.Delay[Num],MainFrm.TScale,True);
                  UpdateEntry(Table.Cells[1,1],Stimulator.Prog.RampStart[Num],VoltsTomV,False);
                  UpdateEntry(Table.Cells[1,2],Stimulator.Prog.RampEnd[Num],VoltsTomV,False);
                  UpdateEntry(Table.Cells[1,3],Stimulator.Prog.Duration[Num],MainFrm.TScale,True) ;
                  end ;
             wvPTrain : begin
                  { Train of pulses }
                  UpdateEntry(Table.Cells[1,0],Stimulator.Prog.Delay[Num],MainFrm.TScale,True);
                  UpdateEntry(Table.Cells[1,1],Stimulator.Prog.Amplitude[Num],VoltsTomV,False);
                  UpdateEntry(Table.Cells[1,2],Stimulator.Prog.Duration[Num],MainFrm.TScale,True);
                  UpdateEntry(Table.Cells[1,3],Stimulator.Prog.PulseInterval[Num],MainFrm.TScale,True);
                  Stimulator.Prog.NumPulses[Num] := Max( ExtractInt(Table.Cells[1,4]),1 ) ;
                  end ;

             wvWave : begin
                  UpdateEntry(Table.Cells[1,0],Stimulator.Prog.Delay[Num],MainFrm.TScale,True);
                  UpdateEntry(Table.Cells[1,1],Stimulator.Prog.ExtWaveData[Table.Row-2],
                                  VoltstomV,False) ;
                  end ;

             wvDigStep0 : begin
                  { Single digital pulse }
                  UpdateEntry(Table.Cells[1,0],Stimulator.Prog.Delay[Num],MainFrm.TScale,true);
                  UpdateEntry(Table.Cells[1,1],Stimulator.Prog.Duration[Num],MainFrm.TScale,True);
                  if Pos('Y',UpperCase(Table.Cells[1,2])) > 0 then
                       Stimulator.Prog.Invert[Num] := True
                  else Stimulator.Prog.Invert[Num] := False ;
                  end ;

               wvDigStep1 : begin
                  { Variable duration digital pulse }
                  UpdateEntry(Table.Cells[1,0],Stimulator.Prog.Delay[Num],MainFrm.TScale,true);
                  UpdateEntry(Table.Cells[1,1],Stimulator.Prog.Duration[Num],MainFrm.TScale,True);
                  UpdateEntry(Table.Cells[1,2],Stimulator.Prog.Increment[Num],MainFrm.TScale,False);
                  NumSteps := Max( ExtractInt(Table.Cells[1,3]),1 ) ;
                  if Pos('Y',UpperCase(Table.Cells[1,4])) > 0 then
                       Stimulator.Prog.Invert[Num] := True
                  else Stimulator.Prog.Invert[Num] := False ;
                  end ;
             wvDigTrain : begin
                  { Digital pulse train }
                  UpdateEntry(Table.Cells[1,0],Stimulator.Prog.Delay[Num],MainFrm.TScale,true);
                  UpdateEntry(Table.Cells[1,1],Stimulator.Prog.Duration[Num],MainFrm.TScale,True);
                  UpdateEntry(Table.Cells[1,2],Stimulator.Prog.PulseInterval[Num],MainFrm.TScale,True);
                  Stimulator.Prog.NumPulses[Num] := Max( ExtractInt(Table.Cells[1,3]),1 ) ;
                  if Pos('Y',UpperCase(Table.Cells[1,4])) > 0 then
                       Stimulator.Prog.Invert[Num] := True
                  else Stimulator.Prog.Invert[Num] := False ;
                  end ;
               wvDigDelay : begin
                  { Variable duration digital pulse }
                  UpdateEntry(Table.Cells[1,0],Stimulator.Prog.Delay[Num],MainFrm.TScale,true);
                  UpdateEntry(Table.Cells[1,1],Stimulator.Prog.Duration[Num],MainFrm.TScale,True);
                  UpdateEntry(Table.Cells[1,2],Stimulator.Prog.Increment[Num],MainFrm.TScale,False);
                  NumSteps := Max( ExtractInt(Table.Cells[1,3]),1 ) ;
                  if Pos('Y',UpperCase(Table.Cells[1,4])) > 0 then
                       Stimulator.Prog.Invert[Num] := True
                  else Stimulator.Prog.Invert[Num] := False ;
                  end ;
             end;

         // Update number of steps in waveform
         IncrementedProtocol := False ;
         for i := 0 to MaxWaveShapes-1 do begin
            case Stimulator.Prog.Shape[i] of
                wvStep1,wvStep2,wvDigStep0,wvDigDelay : begin
                    Stimulator.Prog.NumSteps := NumSteps ;
                    IncrementedProtocol := True ;
                    end ;
                end ;
            end ;
         if not IncrementedProtocol then Stimulator.Prog.NumSteps := 1 ;
         end ;

     { Mark waveform as needing to be saved }
     Stimulator.Prog.Saved := False ;
     TableChanged := False ;
     pbDisplay.invalidate ;
     end ;


procedure TWavGenFrm.bSaveClick(Sender: TObject);
{ -----------------------------
  Save voltage protocol to file
  -----------------------------}
begin

     SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     //SaveDialog.FileName := ExtractFileName( SaveDialog.FileName ) ;
     SaveDialog.InitialDir := MainFrm.VProtDirectory ;
     SaveDialog.Title := 'Save Stimulus Protocol' ;
     if FileExists(Stimulator.FileName) then
        SaveDialog.FileName := ExtractFileName(Stimulator.FileName)
     else SaveDialog.FileName := '*.vpr' ;

     if SaveDialog.execute then SaveWaveform(SaveDialog.FileName) ;

     end ;


procedure TWavGenFrm.bOpenClick(Sender: TObject);
{ ------------------------------------------------------------
  When Open File button clicked load voltage program from file
  ------------------------------------------------------------}
begin

     { If the current waveform has been edited ... offer to save it to file }
     if not Stimulator.Prog.Saved then begin
        if MessageDlg( ' Waveform Changed! Save it to ' + Stimulator.FileName,
           mtConfirmation,[mbYes,mbNo], 0 ) = mrYes then begin
           SaveWaveform( Stimulator.FileName ) ;
           end ;
        end ;

     OpenDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     OpenDialog.FileName := ExtractFileName( SaveDialog.FileName ) ;
     OpenDialog.InitialDir := MainFrm.VProtDirectory ;
     OpenDialog.Title := 'Load Stimulus Protocol' ;
     if OpenDialog.execute then begin
        LoadWaveform( OpenDialog.FileName ) ;
        end ;

     pbDisplay.invalidate ;

     end ;


{procedure TWavGenFrm.CheckWaveformDuration ;}
{ --------------------------------------------------------
  Warn user if recording interval has had to be increased
  --------------------------------------------------------}
{begin

     if Stimulator.Prog.RecordIntervalChanged then begin
        Stimulator.Prog.StepCounter := 0 ;
        Stimulator.CreateWaveform( DAC^, DigBuf^ ) ;
        MessageDlg( format('Interval between records too small! Changed to %.0f %s',
                    [Stimulator.Prog.RecordInterval*MainFrm.TScale,MainFrm.TUnits]),
                              mtWarning, [mbOK], 0 ) ;
        Stimulator.Prog.RecordIntervalChanged := False ;
        Stimulator.Prog.Saved := False ;
        end ;
     end ;}


procedure TWavGenFrm.UpdateEntry(
          const SourceText : string ;
          var Destination : single ;
          ConversionFactor : single ;
          PositiveOnly : boolean
          ) ;
// -----------------------------------------------------------------------
// Update the variable "Destination" with the ASCII number in "SourceText"
// after applying the conversion factor "ConversionFactor".
// -----------------------------------------------------------------------
var
   val : single ;
begin
     Val := ExtractFloat(SourceText,Destination*ConversionFactor) ;
     if PositiveOnly and (Val < 0. ) then Val := Destination*ConversionFactor ;
     Destination := Val / ConversionFactor ;
     end ;


procedure TWavGenFrm.bPasteFromClipboardClick(Sender: TObject);
{ ---------------------------------------------------------------
  Paste voltage waveform data from clipboard into waveform record
  ---------------------------------------------------------------}
const
     BufSize = 30000 ;
var
   T,TOld : single ;
   nValues,i,iSkip : Integer ;
   DataList : TStringList ;
   Values : Array[0..2] of single ;
   Buf0 : PChar ;
begin

     try
        { Allocate buffers }
        DataList := TStringList.create ;
        Buf0 := StrAlloc( BufSize ) ;
        { Get ASCII waveform text list from from clipboard }
        {NumChars := }ClipBoard.GetTextBuf( Buf0, BufSize ) ;
        { Copy list into string list }
        DataList.SetText( Buf0 ) ;

        { Convert to floating point and store in waveform record}

        { Determine how many data points must be skipped to make
          the waveform fit into buffer }
        iSkip := (DataList.Count + High(Stimulator.Prog.ExtWaveData)) div
                 (High(Stimulator.Prog.ExtWaveData)+1) ;
        if iSkip > 1 then
               MessageDlg( format('Too many point in pasted waveform, 1/%d used.',
                           [iSkip]),mtWarning, [mbOK], 0 ) ;
        i := 0 ;
        Stimulator.Prog.ExtEndofData := -1 ;
        TOld := 0.0 ;
        repeat
            { Extract number from row }
            nValues := ExtractListOfFloats( DataList[i], Values, False ) ;
            if nValues >= 2 then begin
               Inc(Stimulator.Prog.ExtEndofData) ;
               TOld := T ;
               T := Values[0] ;
               if Stimulator.Prog.ExtEndofData = 1 then
                  Stimulator.Prog.ExtDACdt := (T - TOld)/MainFrm.TScale ;
               { Read voltage value }
               Stimulator.Prog.ExtWaveData[Stimulator.Prog.ExtEndofData] := Values[1]*mVtoVolts ;
               end
            else Stimulator.Prog.ExtWaveData[Stimulator.Prog.ExtEndofData] := 0.0 ;
            i := i + iSkip ;
            until i >= DataList.Count ;

            { If the required D/A output interval cannot support the
              currently selected inter-record interval, reduce the interval
              to the maximum that can be supported }

{            MaxRecordInterval := ((Stimulator.Prog.ExtDACdt*(MaxTBuf-1))/2.0) ;
            if Stimulator.Prog.RecordInterval > MaxRecordInterval then begin
               Stimulator.Prog.RecordInterval := MaxRecordInterval ;
               MessageDlg( format(' Recording interval changed to %.3g s.',
                           [MaxRecordInterval]),mtWarning, [mbOK], 0 ) ;
               end ;
            if Stimulator.Prog.RecordDuration > MaxRecordInterval then
               Stimulator.Prog.RecordDuration := MaxRecordInterval - 0.1 ;}
     finally
        StrDispose( Buf0 ) ;
        DataList.Free ;
        end ;

     { Update table with waveform data }
     FillParameterTable( CurrentWaveformNum ,True) ;
     pbDisplay.invalidate ;

     end;


procedure TWavGenFrm.FormClose(Sender: TObject; var Action: TCloseAction);
{ -------------------------------------
  Close form and update global settings
  -------------------------------------}
var
    i : Integer ;
begin

     // Command voltage pulse divide factor
     MainFrm.VCommand[0].DivideFactor := edVDivide0.Value ;
     MainFrm.VCommand[1].DivideFactor := edVDivide1.Value ;
     MainFrm.VCommand[2].DivideFactor := edVDivide2.Value ;

     { If the current waveform has been edited ... offer to save it to file }
     if not Stimulator.Prog.Saved then begin
        if MessageDlg( ' Stimulus Changed! Save it to ' + Stimulator.FileName,
           mtConfirmation,[mbYes,mbNo], 0 ) = mrYes then begin
           SaveWaveform(Stimulator.FileName) ;
           end ;
        end ;
     MainFrm.StimFileName := Stimulator.FileName ;

     // Update any forms which make use of stimulus programs
     for i := 0 to MainFrm.MDIChildCount-1 do begin
         if (MainFrm.MDIChildren[i].Name = 'RecordFrm') then
            TRecordFrm(MainFrm.MDIChildren[i]).UpdateStimProgramList
         else if (MainFrm.MDIChildren[i].Name = 'RecADCOnlyFrm') then
            TRecADCOnlyFrm(MainFrm.MDIChildren[i]).UpdateStimProgramList ;
         end ;

     Action := caFree ;
     end;


procedure TWavGenFrm.FormActivate(Sender: TObject);
// ---------------------------------------
// Update display when form becomes active
// ---------------------------------------
begin
     pbDisplay.invalidate ;
     end;


procedure TWavGenFrm.FormResize(Sender: TObject);
// ------------------------------------
// Adjust controls on form for new size
// ------------------------------------
begin

     ProtocolGrp.Width := Max( ClientWidth - ProtocolGrp.Left - 5, 2) ;
     ProtocolGrp.Top := Max( ClientHeight - ProtocolGrp.Height - 5, 2) ;

     DisplayGrp.Width := ProtocolGrp.Width ;
     DisplayGrp.Height := Max( ProtocolGrp.Top - DisplayGrp.Top - 10,2) ;
     pbDisplay.Width := Max( DisplayGrp.ClientWidth - pbDisplay.Left - 5,2) ;
     pbDisplay.Height := Max( DisplayGrp.ClientHeight - pbDisplay.Top - 5,2) ;

     edFolder.Width := DisplayGrp.Width + DisplayGrp.Left - edFolder.Left ;

     DACGrp.Top := ClientHeight - DacGrp.Height - 5 ;

     TableGrp.Width := ProtocolGrp.ClientWidth - TableGrp.Left - 4 ;
     PeriodGrp.Left := TableGrp.ClientWidth - PeriodGrp.Width - 4 ;
     Table.Width := PeriodGrp.Left - Table.Left - 4 ;

     bPasteFromClipboard.Left := PeriodGrp.Left ;

     pbDisplay.Invalidate ;

     FillParameterTable(CurrentWaveformNum,False) ;

     end;


procedure TWavGenFrm.TableExit(Sender: TObject);
// -----------------------------------------------
// Focus has moved away from parameter entry table
// -----------------------------------------------
begin
     if TableChanged then UpdateWaveform ;
     end;


procedure TWavGenFrm.pbDisplayPaint(Sender: TObject);
{ -----------------------------
  Update paintbox areas on form
  -----------------------------}
begin

     DisplayStimulusProtocol ;

     // Update parameter table
     if CurrentWaveformNum = -1 then FillParameterTable( CurrentWaveformNum ,False) ;

     end ;


procedure TWavGenFrm.DisplayStimulusProtocol ;
// -------------------------
// Display stimulus protocol
// -------------------------
const
    DisplayVoltageRange = 10.0 ;
var
    Top :Integer ;
    Left :Integer ;
    i,j,sh :Integer ;
    Shape :Integer ;
    ChannelSpacing : Integer ;
    ChannelHeight : Integer ;
    ChannelHalfHeight : Integer ;
    DigChannelHeight :Integer ;
    YZero :Integer ;

    DisplayDuration : Single ;

    // Stimulus channels in use flags
    NumChansInUse : Integer ;
    // Y Axes range and labels
    VoltsMax : Array[0..NumVChannels-1] of Single ;
    VoltsMin : Array[0..NumVChannels-1] of Single ;
    VoltsMaxLabel : Array[0..NumVChannels-1] of String ;
    VoltsMinLabel : Array[0..NumVChannels-1] of String ;
    DACChannelName : String ;
    //
    DigChannelName : String ;

    Step : Integer ;
    TStartLabel : String ;      // Start of protocol time label
    TEndLabel : String ;        // End of protocol time label
    TLabelsTop : Integer ;      // Location of top of time labels
    RecordLabel : String ;      // Recording bar label
    PlotWidth : Integer ;       // Width of plotting area within display (pixels)
    Prog : TWaveform ;
    Loop : Integer ;
    NumLoops : Integer ;
    Amplitude : Single ;
    RampStart : Single ;
    RampEnd : Single ;
    VHold : Single ;
    MaxAmp : Single ;
begin

     // Make internal copy of stimulator protocol
     Prog := Stimulator.Prog ;

     pbDisplay.canvas.Font.Size := 8 ;

     { Erase various parts of voltage protocol display }
     pbDisplay.canvas.brush.color := clWhite ;
     pbDisplay.canvas.FillRect( Rect( 0,0,pbDisplay.Width-1,pbDisplay.Height-1) ) ;

     Top := 2 ;
     DisplayDuration := Stimulator.Prog.RecordInterval ;


     // Determine maximum amplitude in analog output protocol and
     // scale display accordingly - NS 16 July 2010

     // Reset maximum amplitude
     MaxAmp := 0.0 ;

     // Looking for maximum amplitude
     for Shape := 0 to 29 do
     begin

        // Get current holding voltage for this waveform element
        VHold := MainFrm.VCommand[Min(PlotChannel[Shape],High(MainFrm.VCommand))].HoldingVoltage ;

        // Reset amplitude
        Amplitude := 0.0;

        // Get current amplitude for this waveform element and
        // add holding voltage to it
        case Prog.Shape[Shape] of
           wvStep0 : Begin
              Amplitude := Prog.Amplitude[Shape] + VHold ;
           end;
           wvStep1 : Begin
              for Step := 0 to Min(Prog.NumSteps-1, 1000) do
              begin
                 Amplitude := Max(Amplitude,
                    VHold + Prog.Amplitude[Shape] + (Prog.Increment[Shape] * Step));
              end;
           end;
           wvStep2 : Begin
              Amplitude := Prog.Amplitude[Shape] + VHold;
           end;
           wvRamp : Begin
              RampStart := VHold + Prog.RampStart[Shape] ;
              RampEnd := VHold + Prog.RampEnd[Shape] ;
              Amplitude := Max(Abs(RampStart), Abs(RampEnd));
           end;
           wvPTrain : Begin
              Amplitude := Prog.Amplitude[Shape] + VHold;
           end;
           wvWave : Begin
              for i := 0 to Prog.ExtEndofData do
              begin
                 Amplitude :=
                    Max(Abs(VHold + Prog.ExtWaveData[i]), Abs(Amplitude));
              end;
           end;
        end;

        // Determine maximum amplitude
        if Abs(Amplitude) > Abs(MaxAmp) then MaxAmp := Abs(Amplitude) ;
        MaxAmp := Max(MaxAmp,Abs(VHold)) ;

     end; // End Looking for maximum amplitude


     // Set DAC display labels
     for i := 0 to 2 do begin
           //VoltsMax[i] := DisplayVoltageRange / MainFrm.VCommand[i].DivideFactor ;
           VoltsMax[i] := MaxAmp ;
           if VoltsMax[i] < 1.0 then VoltsMaxLabel[i] := format( '%.0f mV ',[VoltsMax[i]*1000.0])
                                else VoltsMaxLabel[i] := format( '%.0f V ',[VoltsMax[i]]) ;
           VoltsMin[i] := -VoltsMax[i] ;
           if Abs(VoltsMin[i]) < 1.0 then VoltsMinLabel[i] := format( '%.0f mV ',[VoltsMin[i]*1000.0])
                                     else VoltsMinLabel[i] := format( '%.0f V ',[VoltsMin[i]]) ;
           end ;

    Left := 5 + MaxInt( [ pbDisplay.Canvas.TextWidth('VChan0 '),
                          pbDisplay.Canvas.TextWidth(VoltsMaxLabel[0]),
                          pbDisplay.Canvas.TextWidth(VoltsMinLabel[0]),
                          pbDisplay.Canvas.TextWidth(VoltsMaxLabel[1]),
                          pbDisplay.Canvas.TextWidth(VoltsMinLabel[1])
                          ]) ;
    PlotWidth := pbDisplay.Width - Left - 2 ;

    // Display protocol duration labels
    TStartLabel := '0.0' ;
    TLabelsTop := pbDisplay.Height
                - pbDisplay.Canvas.TextHeight(TStartLabel) - 2 ;
    pbDisplay.Canvas.TextOut( Left, TLabelsTop, TStartLabel ) ;
    if DisplayDuration > 0.9 then TEndLabel := format( '%.3g s',[ DisplayDuration] )
                             else TEndLabel := format( '%.3g ms',[1000.0*DisplayDuration] ) ;
    pbDisplay.Canvas.TextOut( pbDisplay.Width
                              - pbDisplay.Canvas.TextWidth(TEndLabel) - 1,
                              TLabelsTop,
                              TEndLabel ) ;

    pbDisplay.canvas.brush.color := clWhite ;

    // Determine stimulus channels in use
    for i := 0 to High(VChanInUse) do VChanInUse[i] := False ;
    DigChanInUse := False ;
    for sh := 0 to High(Prog.Shape) do begin
        if (Prog.Shape[sh] <> wvNone) and
           (Prog.Shape[sh] <> wvDigNone) then begin
           case sh of
                0..9 : VChanInUse[0] := True ;
                10..17 : DigChanInUse := True ;
                18..27 : VChanInUse[1] := True ;
                28..37 : VChanInUse[2] := True ;
                end ;
           end ;
        end ;

    // Determine no. of stimulus channels in use
    NumChansInUse := 0 ;
    for i := 0 to High(VChanInUse) do
        if VChanInUse[i] = True then Inc(NumChansInUse) ;
    if DigChanInUse then Inc(NumChansInUse) ;
    NumChansInUse := Max(NumChansInUse,1) ;   // Avoid div by zero errors when no channels

    ChannelSpacing := Max((TLabelsTop - Top - 2) div NumChansInUse,2);
    ChannelHeight := Max(ChannelSpacing - 2,2) ;
    ChannelHalfHeight := ChannelHeight div 2 ;
    DigChannelHeight := Max(ChannelHeight div 8,2) ;

     // Set DAC channels display area
     YZero := ChannelHalfHeight + Top ;
     for i := 0 to NumVChannels-1 do if VChanInUse[i] then begin

         VHold := MainFrm.VCommand[i].HoldingVoltage ;

         DisplayChannel[VChan0+i].YZero := YZero ;

         //DisplayChannel[VChan0+i].YScale := (ChannelHeight*MainFrm.VCommand[i].DivideFactor) /
         //                                 (2.0*DisplayVoltageRange) ;
         DisplayChannel[VChan0+i].YScale := (ChannelHeight) / (2.0*MaxAmp) ;

         DisplayChannel[VChan0+i].XScale := (pbDisplay.Width - Left - 2) / DisplayDuration ;

         DisplayChannel[VChan0+i].XStart := Left ;
         DisplayChannel[VChan0+i].YStart := DisplayChannel[VChan0+i].YZero
                                            - (VHold*DisplayChannel[VChan0+i].YScale) ;

         // Display voltage range labels
         DACChannelName := format( 'Vout %d ', [i] ) ;
         pbDisplay.Canvas.TextOut( Left - pbDisplay.Canvas.TextWidth(DACChannelName),
                                   Round(DisplayChannel[VChan0+i].YZero)
                                   - (pbDisplay.Canvas.TextHeight(DACChannelName) div 2),
                                   DACChannelName ) ;

         pbDisplay.Canvas.TextOut( Left - pbDisplay.Canvas.TextWidth(VoltsMaxLabel[i]),
                                   Round(DisplayChannel[VChan0+i].YZero) - ChannelHalfHeight,
                                   VoltsMaxLabel[i] ) ;
         pbDisplay.Canvas.TextOut( Left - pbDisplay.Canvas.TextWidth(VoltsMinLabel[i]),
                                   Round(DisplayChannel[VChan0+i].YZero)
                                   + ChannelHalfHeight
                                   - pbDisplay.Canvas.TextHeight(VoltsMinLabel[i]),
                                   VoltsMinLabel[i] ) ;

         // Display zero level as dotted line
         pbDisplay.Canvas.Pen.Style := psDot ;
         pbDisplay.Canvas.MoveTo( Left, YZero ) ;
         pbDisplay.Canvas.LineTo( Left + PlotWidth -1, YZero ) ;
         pbDisplay.Canvas.Pen.Style := psSolid ;

         YZero := YZero + ChannelSpacing ;

         end ;



     if DigChanInUse then begin

        // Initialise digital channels display
        YZero := YZero - ChannelHalfHeight + DigChannelHeight ;
        for i := DigPlotStart to DigPlotEnd do begin
            DisplayChannel[i].XStart := Left ;
            DisplayChannel[i].XScale := (pbDisplay.Width - Left - 2) / DisplayDuration ;
            DisplayChannel[i].YZero := YZero ;
            DisplayChannel[i].YStart := YZero ;
            DisplayChannel[i].YScale := DigChannelHeight - 1 ;
            YZero := YZero + DigChannelHeight ;
            end ;

        // Digital channel names
        DigChannelName := 'Dig. 0-7' ;
        pbDisplay.Canvas.TextOut( Left - pbDisplay.Canvas.TextWidth(DigChannelName),
                                   Round(DisplayChannel[DigPlotStart].YZero)
                                   - DigChannelHeight,
                                   DigChannelName ) ;
        end ;

     // Two display loops if P/N leak subtraction enabled
     NumLoops := 1 ;
     Prog.DigitalInUse := False ;
     for Loop := 1 to NumLoops do begin

         // Plot shapes within stimulus protocol
         for Step := 0 to Min(Prog.NumSteps-1,1000) do begin

             // Reset display channels
             for i := 0 to NumDisplayChannels-1 do begin
                 DisplayChannel[i].X := DisplayChannel[i].XStart ;
                 DisplayChannel[i].Y := DisplayChannel[i].YStart ;
                 pbDisplay.Canvas.MoveTo( Round(DisplayChannel[i].X),
                                          Round(DisplayChannel[i].Y )) ;
                 end ;

             for Shape := 0 to High(Prog.Shape) do begin

                 // Get current holding voltage for this waveform element
                 VHold := MainFrm.VCommand[Min(PlotChannel[Shape],High(MainFrm.VCommand))].HoldingVoltage ;

                 case Prog.Shape[Shape] of
                     wvStep0 : Begin
                         Amplitude := Prog.Amplitude[Shape] ;
                         PlotVStep( PlotChannel[Shape],
                                    VHold,
                                    Prog.Delay[Shape] ) ;
                         PlotVStep(PlotChannel[Shape],
                                    VHold + Amplitude,
                                    Prog.Duration[Shape] ) ;
                         end ;
                      wvStep1 : Begin
                         Amplitude := Prog.Amplitude[Shape] + Prog.Increment[Shape]*Step ;
                         PlotVStep( PlotChannel[Shape],
                                     VHold,
                                     Prog.Delay[Shape] ) ;
                         PlotVStep( PlotChannel[Shape],
                                    VHold + Amplitude,
                                    Prog.Duration[Shape] ) ;
                         end ;

                       wvStep2 : Begin
                         { Family of steps incrementing in duration }
                         Amplitude := Prog.Amplitude[Shape] ;
                         PlotVStep( PlotChannel[Shape],
                                    VHold,
                                    Prog.Delay[Shape] ) ;
                         PlotVStep( PlotChannel[Shape],
                                    VHold + Amplitude,
                                    Prog.Duration[Shape]
                                    + Prog.Increment[Shape]*Step ) ;
                         end ;

                       wvRamp : Begin
                         { Voltage ramp }
                         RampStart := VHold + Prog.RampStart[Shape] ;
                         RampEnd := VHold + Prog.RampEnd[Shape] ;
                         PlotVStep( PlotChannel[Shape],
                                    VHold,
                                    Prog.Delay[Shape] ) ;
                         PlotVStep( PlotChannel[Shape],
                                    RampStart,
                                    0.0 ) ;
                         PlotVLine( PlotChannel[Shape],
                                    RampEnd,
                                    Prog.Duration[Shape] ) ;
                         PlotVStep( PlotChannel[Shape],
                                    VHold,
                                    0.0 ) ;
                         end ;

                       wvPTrain : Begin
                         { Pulse train }
                         PlotVStep( PlotChannel[Shape],
                                    VHold,
                                    Prog.Delay[Shape] ) ;
                         for i := 1 to Prog.NumPulses[Shape] do begin
                             PlotVStep( PlotChannel[Shape],
                                        VHold + Prog.Amplitude[Shape],
                                        Prog.Duration[Shape] ) ;
                             PlotVStep( PlotChannel[Shape],
                                        VHold,
                                        Prog.PulseInterval[Shape] - Prog.Duration[Shape] ) ;
                             end ;
                         end ;

                       wvWave : Begin
                         { Waveform defined from an external file }
                         PlotVStep( PlotChannel[Shape],
                                    VHold,
                                    Prog.Delay[Shape] ) ;

                         for i := 0 to Prog.ExtEndofData do
                             PlotVLine( PlotChannel[Shape],
                                        VHold + Prog.ExtWaveData[i],
                                        Prog.ExtDACdt ) ;
                         end ;

                       wvDigStep0 : Begin
                         { Digital pulse }
                         Prog.DigitalInUse := True ;
                         PlotDigStep( PlotChannel[Shape],
                                      Prog.Invert[Shape],
                                      Prog.Delay[Shape] ) ;
                         PlotDigStep( PlotChannel[Shape],
                                      not Prog.Invert[Shape],
                                      Prog.Duration[Shape] ) ;
                         end ;

                       wvDigStep1 : Begin
                         { Digital pulse (incrementing width)}
                         Prog.DigitalInUse := True ;
                         PlotDigStep( PlotChannel[Shape],
                                      Prog.Invert[Shape],
                                      Prog.Delay[Shape] ) ;
                         PlotDigStep( PlotChannel[Shape],
                                      not Prog.Invert[Shape],
                                      Prog.Duration[Shape] +
                                      Prog.Increment[Shape]*Step ) ;
                         end ;

                       wvDigTrain : begin
                         { Pulse train }
                         Prog.DigitalInUse := True ;
                         PlotDigStep( PlotChannel[Shape],
                                      Prog.Invert[Shape],
                                      Prog.Delay[Shape] ) ;
                         for i := 1 to Prog.NumPulses[Shape] do begin
                             { Pulse duration }
                             PlotDigStep( PlotChannel[Shape],
                                          not Prog.Invert[Shape],
                                          Prog.Duration[Shape] ) ;
                             PlotDigStep( PlotChannel[Shape],
                                          Prog.Invert[Shape],
                                          Prog.PulseInterval[Shape]
                                          - Prog.Duration[Shape] ) ;
                             end ;
                         end ;

                       wvDigDelay : Begin
                         { Digital pulse (incrementing delay)}
                         Prog.DigitalInUse := True ;
                         PlotDigStep( PlotChannel[Shape],
                                      Prog.Invert[Shape],
                                      Prog.Delay[Shape] +
                                      Prog.Increment[Shape]*Step) ;
                         PlotDigStep( PlotChannel[Shape],
                                      not Prog.Invert[Shape],
                                      Prog.Duration[Shape] ) ;
                         end ;

                     end ;
                 end ;

             // Extend line to end stimulus protocol (DACs)
             for i := 0 to VPlotEnd-VPlotStart do begin
                 PlotVStep( i,
                            MainFrm.VCommand[i].HoldingVoltage,
                            DisplayDuration) ;
                 end ;

             // Extend line to end stimulus protocol (Digital O/Ps)
             for i := DigStart to DigEnd do begin
                 PlotDigStep( DigPlotStart + i - DigStart,
                              Prog.Invert[i],
                              DisplayDuration ) ;
                 end ;
             end ;

         end ;

     end ;


procedure TWavGenFrm.PlotVStep(
          ChanNum : Integer ;
          Amplitude : Single ;
          Time : Single
          ) ;
// ----------------------------------
// Plot a DAC voltage step on display
// ----------------------------------
begin
      //if Time = 0.0 then Exit ;

      if not VChanInUse[ChanNum] then Exit ;

      pbDisplay.Canvas.MoveTo( Round(DisplayChannel[ChanNum].X),
                               Round(DisplayChannel[ChanNum].Y) ) ;
      DisplayChannel[ChanNum].Y := DisplayChannel[ChanNum].YZero
                                   - DisplayChannel[ChanNum].YScale*Amplitude ;
      pbDisplay.Canvas.LineTo( Round(DisplayChannel[ChanNum].X),
                               Round(DisplayChannel[ChanNum].Y) ) ;
      DisplayChannel[ChanNum].X := DisplayChannel[ChanNum].X
                                   + DisplayChannel[ChanNum].XScale*Time ;
      pbDisplay.Canvas.LineTo( Round(DisplayChannel[ChanNum].X),
                               Round(DisplayChannel[ChanNum].Y) ) ;

      end ;


procedure TWavGenFrm.PlotVLine(
          ChanNum : Integer ;
          Amplitude : Single ;
          Time : Single
          ) ;
// ----------------------------------
// Plot a DAC voltage line on display
// ----------------------------------
begin

      if not VChanInUse[ChanNum] then Exit ;

      pbDisplay.Canvas.MoveTo( Round(DisplayChannel[ChanNum].X),
                               Round(DisplayChannel[ChanNum].Y) ) ;
      DisplayChannel[ChanNum].Y := DisplayChannel[ChanNum].YZero
                                   - DisplayChannel[ChanNum].YScale*Amplitude ;
      DisplayChannel[ChanNum].X :=  DisplayChannel[ChanNum].X
                                    + DisplayChannel[ChanNum].XScale*Time ;
      pbDisplay.Canvas.LineTo( Round(DisplayChannel[ChanNum].X),
                               Round(DisplayChannel[ChanNum].Y) ) ;

      end ;


procedure TWavGenFrm.PlotDigStep(
          ChanNum : Integer ;
          Polarity : Boolean ;
          Time : Single
          ) ;
// ----------------------------------
// Plot a TTL digital step on display
// ----------------------------------
var
    DigLevel : Integer ;
begin


      if Time = 0.0 then Exit ;
      if not DigChanInUse then Exit ;

      if Polarity then DigLevel := 1
                  else DigLevel := 0 ;
      pbDisplay.Canvas.MoveTo( Round(DisplayChannel[ChanNum].X),
                               Round(DisplayChannel[ChanNum].Y) ) ;
      DisplayChannel[ChanNum].Y := DisplayChannel[ChanNum].YZero
                                   - DisplayChannel[ChanNum].YScale*DigLevel ;
      pbDisplay.Canvas.LineTo( Round(DisplayChannel[ChanNum].X),
                               Round(DisplayChannel[ChanNum].Y) ) ;
      DisplayChannel[ChanNum].X := DisplayChannel[ChanNum].X
                                   + DisplayChannel[ChanNum].XScale*Time ;
      pbDisplay.Canvas.LineTo( Round(DisplayChannel[ChanNum].X),
                               Round(DisplayChannel[ChanNum].Y) ) ;

      end ;


function TWavGenFrm.ProtocolDuration : Single ;
{ --------------------------------------------
  Calculate total duration of voltage protocol
  --------------------------------------------}

var
   i,NumExtra : Integer ;
   TSum : Array[0..MaxProtocolShapes-1] of Single ;
   TMax : Single ;
begin

     for i := 0 to MaxProtocolShapes-1 do TSum[i] := 0.0 ;

     { Add up time taken by all active waveform shapes }
     NumExtra := Stimulator.Prog.NumSteps - 1 ;

     for i := 0 to High(Stimulator.Prog.Shape) do begin
         case Stimulator.Prog.Shape[i] of
              { Analogue voltage waveforms. Occur one after the other
                   on DAC O/P channel 0) }
              wvStep0 :   begin
                  TSum[PlotChannel[i]] := TSum[PlotChannel[i]]
                                          + Stimulator.Prog.Duration[i] + Stimulator.Prog.Delay[i] ;
                  end ;
              wvStep1 : begin
                  TSum[PlotChannel[i]] := TSum[PlotChannel[i]]
                                          + Stimulator.Prog.Duration[i] + Stimulator.Prog.Delay[i] ;
                  end ;
              wvStep2 : begin
                  TSum[PlotChannel[i]] := TSum[PlotChannel[i]]
                                          + Stimulator.Prog.Duration[i] + Stimulator.Prog.Delay[i]
                                          + MaxFlt([Stimulator.Prog.Increment[i]*NumExtra,0. ]) ;
                  end ;
              wvRamp : begin
                  TSum[PlotChannel[i]] := TSum[PlotChannel[i]]
                                          + Stimulator.Prog.Duration[i] + Stimulator.Prog.Delay[i] ;
                  end ;
              wvPTrain : begin
                 TSum[PlotChannel[i]] := TSum[PlotChannel[i]]
                                         + (Stimulator.Prog.Delay[i] +
                                         + Stimulator.Prog.NumPulses[i]*Stimulator.Prog.PulseInterval[i]) ;
                 end ;
              wvWave : begin
                 TSum[PlotChannel[i]] := TSum[PlotChannel[i]]
                                         + (Stimulator.Prog.ExtEndOfData+1)*Stimulator.Prog.DACdt ;
                 end ;

              { Digital waveforms. Each waveform output simultaneously (on bits 0..7) }
              wvDigStep0 : begin
                 TSum[PlotChannel[i]] := TSum[PlotChannel[i]] +
                                         Stimulator.Prog.Delay[i] + Stimulator.Prog.Duration[i] ;
                 end ;
              wvDigStep1 : begin
                 TSum[PlotChannel[i]] := TSum[PlotChannel[i]] +
                                         Stimulator.Prog.Delay[i] + Stimulator.Prog.Duration[i] +
                                         (Stimulator.Prog.Increment[i]*NumExtra) ;
                end ;
              wvDigTrain : begin
                 TSum[PlotChannel[i]] := TSum[PlotChannel[i]] +
                                         Stimulator.Prog.Delay[i] +
                                         (Stimulator.Prog.NumPulses[i]*Stimulator.Prog.PulseInterval[i]) ;
                end ;
              wvDigDelay : begin
                 TSum[PlotChannel[i]] := TSum[PlotChannel[i]] +
                                         Stimulator.Prog.Delay[i] + Stimulator.Prog.Duration[i] +
                                         (Stimulator.Prog.Increment[i]*NumExtra) ;
                end ;

              end ;
         end ;

     TMax := Stimulator.Prog.RecordInterval ;
     for i := 0 to NumDisplayChannels-1 do if TMax < TSum[i] then TMax := TSum[i] ;

     Result := TMax ;
     end ;



procedure TWavGenFrm.Timer1Timer(Sender: TObject);
{ -----------------------------------------------------
  Ensure table row width and column heights are correct
  -----------------------------------------------------}
var
   MaxWidth,RowHeight,i : Integer ;
begin
     MaxWidth := 0 ;
        RowHeight := (4*Table.canvas.TextHeight(Table.cells[0,0])) div 3  ;
        for i := 0 to Table.RowCount-1 do begin
            Table.RowHeights[i] := RowHeight ;
            if Table.canvas.TextWidth(Table.cells[0,i]+' ') > MaxWidth then
               MaxWidth := Table.canvas.TextWidth(Table.cells[0,i]+' ') ;
            end ;
        Table.ColWidths[0] := MaxWidth + 10 ;

     Timer1.enabled := false ;
     end;


procedure TWavGenFrm.edVDivide0KeyPress(Sender: TObject; var Key: Char);
{ ------------------------------------
  Update command voltage #0 divide factor
  ------------------------------------}
begin
     if key = #13 then MainFrm.VCommand[0].DivideFactor := edVDivide0.Value ;
     end;

procedure TWavGenFrm.edVDivide1KeyPress(Sender: TObject; var Key: Char);
{ ------------------------------------
  Update command voltage #1 divide factor
  ------------------------------------}
begin
     if key = #13 then MainFrm.VCommand[1].DivideFactor := edVDivide1.Value ;
     end;


procedure TWavGenFrm.edVDivide2KeyPress(Sender: TObject; var Key: Char);
{ ------------------------------------
  Update command voltage #2 divide factor
  ------------------------------------}
begin
     if key = #13 then MainFrm.VCommand[2].DivideFactor := edVDivide2.Value ;
     end;


procedure TWavGenFrm.edStimPeriodKeyPress(Sender: TObject; var Key: Char);
begin
     if Key = #13 then UpdateWaveform ;
     Stimulator.Prog.Saved := False ;
     end;

procedure TWavGenFrm.ckRepeatedStimClick(Sender: TObject);
// --------------------------------------
// Repeated stimulation check box changed
// --------------------------------------
begin
     Stimulator.Prog.Saved := False ;
     if ckRepeatedStim.Checked then cbNextProgram.ItemIndex := 0 ;
     end;

procedure TWavGenFrm.cbNextProgramChange(Sender: TObject);
// -----------------------------
// Next stimulus program changed
// -----------------------------
begin
    Stimulator.Prog.Saved := False ;
    end;

procedure TWavGenFrm.bSetSubFolderClick(Sender: TObject);
// ---------------------------------
//  Set voltage protocol file folder
// ---------------------------------
begin

    SelectDirectory(MainFrm.VProtDirectory, [sdAllowCreate, sdPerformCreate, sdPrompt],0) ;
    MainFrm.VProtDirectory := MainFrm.VProtDirectory + '\' ;
    edFolder.Text := MainFrm.VProtDirectory ;

    end ;


procedure TWavGenFrm.DigNoneMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     CurrentTool.ToolType := TWaveShape( TImage(Sender).Tag ) ;
     CurrentTool.Digital := True ;
     TIMage(Sender).BeginDrag( False ) ;
     end;

Initialization

end.
