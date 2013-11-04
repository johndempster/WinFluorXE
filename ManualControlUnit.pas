unit ManualControlUnit;
// =============================================================================
// WinImage - Windows Imaging Program - Manual digital line control module
// (c) J. Dempster, University of Strathclyde, 2001-2002, All Rights Reserved
// =============================================================================
// 16.5.2002

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TManualControlFrm = class(TForm)
    Excitation: TGroupBox;
    GroupBox1: TGroupBox;
    rbEXCShutterOpen: TRadioButton;
    rbEXCShutterClosed: TRadioButton;
    Label2: TLabel;
    cbWavelength: TComboBox;
    rbIntensifierActive: TRadioButton;
    rbIntensifierInactive: TRadioButton;
    DigitalOutGrp: TGroupBox;
    Dig0: TGroupBox;
    rbDig0On: TRadioButton;
    rbDig0Off: TRadioButton;
    Label1: TLabel;
    GroupBox3: TGroupBox;
    Label3: TLabel;
    rbDig1On: TRadioButton;
    rbDig1Off: TRadioButton;
    GroupBox4: TGroupBox;
    Label4: TLabel;
    rbDig2On: TRadioButton;
    rbDig2Off: TRadioButton;
    GroupBox5: TGroupBox;
    Label5: TLabel;
    rbDig3On: TRadioButton;
    rbDig3Off: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure rbEXCShutterOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    Initialising : Boolean ;
    procedure UpdateDigitalControlLines ;
  public
    { Public declarations }
  end;

var
  ManualControlFrm: TManualControlFrm;

implementation

uses Main, LabIOUnit, mmsystem ;

{$R *.DFM}


procedure TManualControlFrm.FormShow(Sender: TObject);
// ------------------------------------
// Initialise form when it is displayed
// ------------------------------------
var
     i : Integer ;
begin

     // Set initialisg flag to prevented repeated digital updates
     Initialising := True ;

     // Excitation light shutter
     if Word(MainFrm.DigControlWord and EXCShutterBitOn)
        = Word(EXCShutterBitOn) then rbEXCShutterOpen.Checked := True
                                else rbEXCShutterOpen.Checked := False ;
     rbEXCShutterClosed.Checked := not rbEXCShutterOpen.Checked ;

     // Intensifier active/inactive state
     if Word(MainFrm.DigControlWord and MCPShutterBitOn)
        = Word(MCPShutterBitOn) then rbIntensifierActive.Checked := True
                                else rbIntensifierActive.Checked := False ;
     rbIntensifierInactive.Checked := not rbIntensifierActive.Checked ;

     // Digital stimulus control line 0
     if Word(MainFrm.DigControlWord and StimPulse0On)
        = Word(StimPulse0On) then rbDig0On.Checked := True
                             else rbDig0On.Checked := False ;
     rbDig0Off.Checked := not rbDig0On.Checked ;

     // Digital stimulus control line 1
     if Word(MainFrm.DigControlWord and StimPulse1On)
        = Word(StimPulse1On) then rbDig1On.Checked := True
                             else rbDig1On.Checked := False ;
     rbDig1Off.Checked := not rbDig1On.Checked ;

     // Digital stimulus control line 2
     if Word(MainFrm.DigControlWord and StimPulse2On)
        = Word(StimPulse2On) then rbDig2On.Checked := True
                             else rbDig2On.Checked := False ;
     rbDig2Off.Checked := not rbDig2On.Checked ;

     // Digital stimulus control line 3
     if Word(MainFrm.DigControlWord and StimPulse3On)
        = Word(StimPulse3On) then rbDig3On.Checked := True
                             else rbDig3On.Checked := False ;
     rbDig3Off.Checked := not rbDig3On.Checked ;

     // Create list of excitation wavelengths
     cbWaveLength.Clear ;
     for i := 0 to High(MainFrm.EXCWavelengths) do begin
         cbWaveLength.Items.Add( format('%d: %d (%d) nm',
         [i+1,
          MainFrm.EXCWavelengths[i].Centre,
          MainFrm.EXCWavelengths[i].Width] )) ;
         end ;
     cbWaveLength.ItemIndex := 0 ;

     ClientHeight := DigitalOutGrp.Top
                     + DigitalOutGrp.Height + 5 ;
     ClientWidth := DigitalOutGrp.Left
                     + DigitalOutGrp.Width + 5 ;

     // Clear initialising flag to allow digital updates
     Initialising := False ;
     UpdateDigitalControlLines ;

     end;


procedure TManualControlFrm.UpdateDigitalControlLines ;
// -----------------------------------
// Update digital system control lines
// -----------------------------------
var
     NewValue : Word ;
begin

     // Excitation light shutter
     if rbEXCShutterOpen.Checked then NewValue := Word(EXCShutterBitOn)
                                 else NewValue := Word(EXCShutterBitOff) ;
     MainFrm.DigControlWord :=
         (MainFrm.DigControlWord and EXCShutterMask) or NewValue ;

     // Intensifier active/inactive state
     if rbIntensifierActive.Checked then NewValue := Word(MCPShutterBitOn)
                                    else NewValue := Word(MCPShutterBitOff) ;
     MainFrm.DigControlWord :=
         (MainFrm.DigControlWord and MCPShutterMask) or NewValue ;

     // Digital stimulus control line 0
     if rbDig0On.Checked then NewValue := Word(StimPulse0On)
                         else NewValue := Word(StimPulse0Off) ;
     MainFrm.DigControlWord :=
         (MainFrm.DigControlWord and StimPulse0Mask) or NewValue ;

     // Digital stimulus control line 1
     if rbDig1On.Checked then NewValue := Word(StimPulse1On)
                         else NewValue := Word(StimPulse1Off) ;
     MainFrm.DigControlWord :=
         (MainFrm.DigControlWord and StimPulse1Mask) or NewValue ;

     // Digital stimulus control line 2
     if rbDig2On.Checked then NewValue := Word(StimPulse2On)
                         else NewValue := Word(StimPulse2Off) ;
     MainFrm.DigControlWord :=
         (MainFrm.DigControlWord and StimPulse2Mask) or NewValue ;

     // Digital stimulus control line 3
     if rbDig3On.Checked then NewValue := Word(StimPulse3On)
                         else NewValue := Word(StimPulse3Off) ;
     MainFrm.DigControlWord :=
         (MainFrm.DigControlWord and StimPulse3Mask) or NewValue ;

     // Set excitation wavelength
     NewValue := Word(cbWaveLength.ItemIndex*WaveLengthLSB) ;
     MainFrm.DigControlWord :=
         (MainFrm.DigControlWord and WavelengthMask) or NewValue ;

     // Update control line settings
     MainFrm.SetDigitalControlLines ;

     end ;


procedure TManualControlFrm.rbEXCShutterOpenClick(Sender: TObject);
begin
     if not Initialising then UpdateDigitalControlLines ;
     end;


procedure TManualControlFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ----------------------
// Close and destroy form
// ----------------------
begin
     Action := caFree ;
     MainFrm.mnManualControl.Enabled := True ;
     end;

end.
