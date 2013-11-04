unit SetupADCUnit;
// ---------------------------------------------
// WinFluor - Analogue Input Channels Setup Form
// ---------------------------------------------
// 8.10.3 ... Created
// 2.09.05 .. MainFrm.ADCNumChannels and MainFrm.ADCScanInterval
//            now updated directly
// 09.09.05 .. Channel settings can now be set when Amplifier=None
// 12.06.07 .. Channel settings can now be set when Amplifier=Manual Gain Entry
// 05.08.08 .. Amplifiers now listed in more rational order (amp #s stored as combo box objects)
//             Gain and mode telegraphs now updated correctly
//             Two amplifiers supported
// 09.09.10 .. Cell capacity calculation setup added

interface


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, ValidatedEdit, IDRFile, ExtCtrls, math ;

//const
//     MaxADCChannel = 7 ;

type
  TSetupADCFrm = class(TForm)
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edNumChannels: TValidatedEdit;
    edScanInterval: TValidatedEdit;
    cbADCVoltageRange: TComboBox;
    ChannelsGrp: TGroupBox;
    ChannelTable: TStringGrid;
    GroupBox18: TGroupBox;
    cbAmplifier1: TComboBox;
    bOK: TButton;
    bCancel: TButton;
    edVDivide0: TValidatedEdit;
    Label15: TLabel;
    GainTelPanel1: TPanel;
    lbTelegraphChannel: TLabel;
    edGainTelegraphChannel1: TValidatedEdit;
    ModeTelPanel1: TPanel;
    Label4: TLabel;
    edModeTelegraphChannel1: TValidatedEdit;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    cbAmplifier2: TComboBox;
    edVDivide1: TValidatedEdit;
    GainTelPanel2: TPanel;
    Label7: TLabel;
    edGainTelegraphChannel2: TValidatedEdit;
    ModeTelPanel2: TPanel;
    Label8: TLabel;
    edModeTelegraphChannel2: TValidatedEdit;
    CapGrp: TGroupBox;
    GroupBox12: TGroupBox;
    Label29: TLabel;
    Label32: TLabel;
    edCapRSeriesComp: TValidatedEdit;
    edCapCellCapacityComp: TValidatedEdit;
    ckCapacityCompensationInUse: TCheckBox;
    GroupBox7: TGroupBox;
    Label19: TLabel;
    Label20: TLabel;
    edCapFrequency: TValidatedEdit;
    edCapVRev: TValidatedEdit;
    GroupBox6: TGroupBox;
    Label9: TLabel;
    Label17: TLabel;
    Label21: TLabel;
    edCapCmDisplayMax: TValidatedEdit;
    edCapGmDisplayMax: TValidatedEdit;
    edCapGsDisplayMax: TValidatedEdit;
    ckCapEnabled: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edNumChannelsKeyPress(Sender: TObject; var Key: Char);
    procedure bCancelClick(Sender: TObject);
    procedure cbAmplifier1Change(Sender: TObject);
    procedure edVDivide0KeyPress(Sender: TObject; var Key: Char);
    procedure ckCapEnabledClick(Sender: TObject);
  private
    { Private declarations }
    Device : SmallInt ;
    procedure UpdateChannelEditTable ;
    procedure GetChannelsFromEditTable ;
    procedure UpdateAmplifierSettings ;
    procedure UpdateCapacityChannels ;

  public
    { Public declarations }
  end;

var
  SetupADCFrm: TSetupADCFrm;

implementation

uses Main, AmpModule, LabIOUnit, maths , RecADCOnlyUnit, RecUnit;

{$R *.dfm}

const
     // Channel calibration table column definitions
     ChNum = 0 ;
     ChName = 1 ;
     ChCal = 2 ;
     ChUnits = 3 ;



procedure TSetupADCFrm.FormShow(Sender: TObject);
// ---------------------------------------------
// Initial control settings when form is opened
// ---------------------------------------------
var
     i :Integer ;
begin

     ClientWidth := CapGrp.Left + CapGrp.Width + 5  ;
     ClientHeight := ChannelsGrp.Top + ChannelsGrp.Height + 5 ;

     // Get A/D converter device
     if (MainFrm.IOConfig.ADCIn >= 0) and
        (MainFrm.IOConfig.ADCIn <= MaxResources) then
        Device := LabIO.Resource[MainFrm.IOConfig.ADCIn].Device
     else Device := 0 ;

     // Disable menu option
     MainFrm.mnSetupADC.Enabled := False ;

     // Close any form which might be acquiring images
     if MainFrm.FormExists('RecADCOnlyFrm') then RecADCOnlyFrm.Close ;
     if MainFrm.FormExists('RecordFrm') then RecordFrm.Close ;

     // Analogue input settings
     edNumChannels.Value := MainFrm.ADCNumChannels ;
     edScanInterval.Value := MainFrm.ADCScanInterval ;

     { Set up A/D Converter voltage range selection box }
     cbADCVoltageRange.clear ;
     if Device > 0 then begin
        // Get ranges supported by interface
        for i := 0 to LabIO.NumADCVoltageRanges[Device]-1 do begin
            cbADCVoltageRange.items.add(
            format(' +/- %.3g V ',[LabIO.ADCVoltageRanges[Device,i]] )) ;
            if Abs((MainFrm.ADCVoltageRange/LabIO.ADCVoltageRanges[Device,i])-1.0)
               < 1E-2 then cbADCVoltageRange.ItemIndex := i ;
            if Abs((MainFrm.ADCVoltageRange/LabIO.ADCVoltageRanges[Device,i])-1.0)
               < 1E-2 then cbADCVoltageRange.ItemIndex := i ;
            end ;
        end
     else begin
        cbADCVoltageRange.items.add(
        format(' +/- %.3g V ',[MainFrm.ADCVoltageRange] )) ;
        cbADCVoltageRange.ItemIndex ;
        end ;

     // Setup Amplifier #1 telegraph options
     Amplifier.GetList( cbAmplifier1.Items ) ;
     cbAmplifier1.ItemIndex := cbAmplifier1.Items.IndexofObject(TObject(Amplifier.AmplifierType[1])) ;
     Amplifier.GetList( cbAmplifier2.Items ) ;
     cbAmplifier2.ItemIndex := cbAmplifier2.Items.IndexofObject(TObject(Amplifier.AmplifierType[2])) ;

     edGainTelegraphChannel1.Value := Amplifier.GainTelegraphChannel[1] ;
     edModeTelegraphChannel1.Value := Amplifier.ModeTelegraphChannel[1] ;
     edGainTelegraphChannel2.Value := Amplifier.GainTelegraphChannel[2] ;
     edModeTelegraphChannel2.Value := Amplifier.ModeTelegraphChannel[2] ;

     // Command voltage divide factor
     edVDivide0.Value := MainFrm.VCommand[0].DivideFactor ;
     edVDivide1.Value := MainFrm.VCommand[1].DivideFactor ;

     // Get channel settings
     UpdateAmplifierSettings ;
     UpdateChannelEditTable ;

     // Capacity calculation
     ckCapEnabled.Checked := MainFrm.Cap.Enabled ;
     edCapFrequency.Value := MainFrm.Cap.Frequency ;
     edCapVRev.Value :=  MainFrm.Cap.VRev ;
     ckCapacityCompensationInUse.Checked := MainFrm.Cap.CompensationInUse ;
     edCapRSeriesComp.Value :=  MainFrm.Cap.RSeriesComp ;
     edCapCellCapacityComp.Value := MainFrm.Cap.CellCapacityComp ;
     edCapGmDisplayMax.Value :=  MainFrm.Cap.GmDisplayMax ;
     edCapGsDisplayMax.Value :=  MainFrm.Cap.GsDisplayMax ;
     edCapCmDisplayMax.Value :=  MainFrm.Cap.CmDisplayMax ;

     end ;


procedure TSetupADCFrm.UpdateChannelEditTable ;
// ----------------------------
// Update channel editing table
// ----------------------------
var
     ch : Integer ;
begin

     { Set A/D input channel calibration table }
     ChannelTable.cells[ChNum,0] := 'Ch.' ;
     ChannelTable.colwidths[ChNum] := ChannelTable.DefaultColWidth div 2 ;
     ChannelTable.cells[ChName,0] := 'Name' ;
     ChannelTable.colwidths[ChName] := ChannelTable.DefaultColWidth ;
     ChannelTable.cells[ChCal,0] := 'V/Units' ;
     ChannelTable.colwidths[ChCal] := (5*ChannelTable.DefaultColWidth) div 4 ;
     ChannelTable.cells[ChUnits,0] := 'Units' ;
     ChannelTable.colwidths[ChUnits] := ChannelTable.DefaultColWidth ;
     ChannelTable.RowCount := MainFrm.ADCNumChannels + 1;
     ChannelTable.options := [goEditing,goHorzLine,goVertLine] ;

     for ch := 0 to MainFrm.ADCNumChannels-1 do begin
         ChannelTable.cells[ChNum,ch+1] := IntToStr(ch) ;
         ChannelTable.cells[ChName,ch+1] := MainFrm.ADCChannel[ch].ADCName ;
         ChannelTable.cells[ChCal,ch+1] := Format( '%5.4g',
                                           [MainFrm.ADCChannel[ch].ADCCalibrationFactor] ) ;
         ChannelTable.cells[ChUnits,ch+1] := MainFrm.ADCChannel[ch].ADCUnits ;
         end ;

     end ;


procedure TSetupADCFrm.GetChannelsFromEditTable ;
// --------------------------------------------
// Get channel calibration data from edit table
// --------------------------------------------
var
    ch : Integer ;
begin

    for ch := 0 to MainFrm.ADCNumChannels-1 do begin
       MainFrm.ADCChannel[ch].ADCName := ChannelTable.cells[ChName,ch+1] ;
       MainFrm.ADCChannel[ch].ADCCalibrationFactor := ExtractFloat(
                                             ChannelTable.cells[ChCal,ch+1],
                                             MainFrm.ADCChannel[ch].ADCCalibrationFactor);
       MainFrm.ADCChannel[ch].ADCUnits := ChannelTable.cells[ChUnits,ch+1] ;
       end ;
    end ;


procedure TSetupADCFrm.bOKClick(Sender: TObject);
// ------------------------------------
// Update settings and close setup form
// ------------------------------------
begin

    // Get settings from channel table
    GetChannelsFromEditTable ;

    // Ensure amplifier settings are up to date
    UpdateAmplifierSettings ;

    // Analogue input settings
    MainFrm.ADCNumChannels := Round(edNumChannels.Value)  ;

    // Channel scanning interval
    MainFrm.ADCScanInterval := edScanInterval.Value ;

    // Update A/D input voltage range
    if Device > 0 then begin
       MainFrm.ADCVoltageRange := LabIO.ADCVoltageRanges[ Device,
                                                          cbADCVoltageRange.ItemIndex] ;
       MainFrm.ADCVoltageRange := LabIO.ADCVoltageRanges[ Device,
                                                          cbADCVoltageRange.ItemIndex] ;
       end ;

    // Setup Amplifier telegraph options
    Amplifier.AmplifierType[1] := Integer(cbAmplifier1.Items.Objects[cbAmplifier1.ItemIndex]) ;
    Amplifier.GainTelegraphChannel[1] := Round(edGainTelegraphChannel1.Value) ;
    Amplifier.ModeTelegraphChannel[1] := Round(edModeTelegraphChannel1.Value) ;
    Amplifier.AmplifierType[2] := Integer(cbAmplifier2.Items.Objects[cbAmplifier2.ItemIndex]) ;
    Amplifier.GainTelegraphChannel[2] := Round(edGainTelegraphChannel2.Value) ;
    Amplifier.ModeTelegraphChannel[2] := Round(edModeTelegraphChannel2.Value) ;

    // Get command voltage divide factor
    // (If an amplifier is defined, it over-rides user entered setting
    MainFrm.VCommand[0].DivideFactor := edVDivide0.Value ;
    Amplifier.GetCommandVoltageDivideFactor(1,MainFrm.VCommand[0].DivideFactor) ;
    MainFrm.VCommand[1].DivideFactor := edVDivide1.Value ;
    Amplifier.GetCommandVoltageDivideFactor(2,MainFrm.VCommand[1].DivideFactor) ;

    // Capacity calculation
    MainFrm.Cap.Enabled := ckCapEnabled.Checked ;
    MainFrm.Cap.Frequency := edCapFrequency.Value ;
    MainFrm.Cap.VRev := edCapVRev.Value ;
    MainFrm.Cap.CompensationInUse := ckCapacityCompensationInUse.Checked ;
    MainFrm.Cap.RSeriesComp := edCapRSeriesComp.Value ;
    MainFrm.Cap.CellCapacityComp := edCapCellCapacityComp.Value ;
    MainFrm.Cap.GmDisplayMax := edCapGmDisplayMax.Value ;
    MainFrm.Cap.GsDisplayMax := edCapGsDisplayMax.Value ;
    MainFrm.Cap.CmDisplayMax := edCapCmDisplayMax.Value ;

    Close ;

    end;


procedure TSetupADCFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// -----------------------
// Close and destroy form
// -----------------------
begin
     MainFrm.mnSetupADC.Enabled := True ;
     Action := caFree ;
     end;


procedure TSetupADCFrm.edNumChannelsKeyPress(Sender: TObject;
  var Key: Char);
// ---------------------------
// No. of A/D channels changed
// ---------------------------
begin
     if key = #13 then begin
        GetChannelsFromEditTable ;
        MainFrm.ADCNumChannels :=  Round(edNumChannels.Value) ;
        UpdateChannelEditTable ;
        end ;
     end ;


procedure TSetupADCFrm.UpdateAmplifierSettings ;
// ----------------------------------------------
// Update channel settings when amplifier changed
// ----------------------------------------------
var
    ch : Integer ;

begin

     Amplifier.AmplifierType[1] := Integer(cbAmplifier1.Items.Objects[cbAmplifier1.ItemIndex]) ;
     GainTelPanel1.Visible := Amplifier.NeedsGainTelegraphChannel[1] ;
     ModeTelPanel1.Visible := Amplifier.NeedsModeTelegraphChannel[1] ;

     Amplifier.AmplifierType[2] := Integer(cbAmplifier2.Items.Objects[cbAmplifier2.ItemIndex]) ;
     GainTelPanel2.Visible := Amplifier.NeedsGainTelegraphChannel[2] ;
     ModeTelPanel2.Visible := Amplifier.NeedsModeTelegraphChannel[2] ;

     // Update channels 0 & 1 settings which depend upon amplifier
     for ch := 0 to MainFrm.ADCNumChannels-1 do begin

         Amplifier.GetChannelSettings( ch,
                                       MainFrm.ADCChannel[ch].ADCName,
                                       MainFrm.ADCChannel[ch].ADCUnits,
                                       MainFrm.ADCChannel[ch].ADCCalibrationFactor,
                                       MainFrm.ADCChannel[ch].ADCAmplifierGain ) ;

        end ;

     // Get command voltage divide factor
     // (If an amplifier is defined, it over-rides user entered setting
     MainFrm.VCommand[0].DivideFactor := edVDivide0.Value ;
     Amplifier.GetCommandVoltageDivideFactor(1,MainFrm.VCommand[0].DivideFactor) ;
     edVDivide0.Value := MainFrm.VCommand[0].DivideFactor  ;
     MainFrm.VCommand[1].DivideFactor := edVDivide1.Value ;
     Amplifier.GetCommandVoltageDivideFactor(2,MainFrm.VCommand[1].DivideFactor) ;
     edVDivide1.Value := MainFrm.VCommand[1].DivideFactor  ;

     end ;


procedure TSetupADCFrm.bCancelClick(Sender: TObject);
// ----------------------------
// Close dialog without changes
// ----------------------------
begin
     Close ;
     end;

procedure TSetupADCFrm.cbAmplifier1Change(Sender: TObject);
// ----------------------
// Amplifier type changed
// ----------------------
begin
     UpdateAmplifierSettings ;
     end ;


procedure TSetupADCFrm.edVDivide0KeyPress(Sender: TObject; var Key: Char);
// ------------------------------
// Command voltage factor changed
// ------------------------------
begin
     if Key = #13 then begin
        UpdateAmplifierSettings ;
        end ;
     end;

procedure TSetupADCFrm.UpdateCapacityChannels ;
// ---------------------------------------------
// Update channel settings for capacity analysis
// ---------------------------------------------
begin

    // Ensure there are at least 7 channels
    GetChannelsFromEditTable ;
    edNumChannels.Value := Max( Round(edNumChannels.Value),7) ;
    MainFrm.ADCNumChannels := Round(edNumChannels.Value) ;
    UpdateChannelEditTable ;

    // Get A/D voltage
    MainFrm.ADCVoltageRange := LabIO.ADCVoltageRanges[ Device,
                                                       cbADCVoltageRange.ItemIndex] ;

    // Get Display ranges
    MainFrm.Cap.GmDisplayMax := edCapGmDisplayMax.Value ;
    MainFrm.Cap.GsDisplayMax := edCapGsDisplayMax.Value ;
    MainFrm.Cap.CmDisplayMax := edCapCmDisplayMax.Value ;


    // Define channels

    Mainfrm.Cap.ImChan := 0 ;
    MainFrm.ADCChannel[Mainfrm.Cap.ImChan].ADCName := 'Im' ;
    MainFrm.ADCChannel[Mainfrm.Cap.ImChan].ADCUnits := 'pA' ;

    Mainfrm.Cap.VmChan := 1 ;
    MainFrm.ADCChannel[Mainfrm.Cap.VmChan].ADCName := 'Vm' ;
    MainFrm.ADCChannel[Mainfrm.Cap.VmChan].ADCUnits := 'mV' ;

    Mainfrm.Cap.GRChan := 2 ;
    MainFrm.ADCChannel[Mainfrm.Cap.GRChan].ADCName := 'Greal' ;
    MainFrm.ADCChannel[Mainfrm.Cap.GRChan].ADCUnits := 'nS' ;

    Mainfrm.Cap.GIChan := 3 ;
    MainFrm.ADCChannel[Mainfrm.Cap.GIChan].ADCName := 'Gimag' ;
    MainFrm.ADCChannel[Mainfrm.Cap.GIChan].ADCUnits := 'nS' ;

    Mainfrm.Cap.GsChan := 4 ;
    MainFrm.ADCChannel[Mainfrm.Cap.GsChan].ADCName := 'Gs' ;
    MainFrm.ADCChannel[Mainfrm.Cap.GsChan].ADCUnits := 'nS' ;
    MainFrm.ADCChannel[Mainfrm.Cap.GsChan].ADCAmplifierGain := 1.0 ;
    MainFrm.ADCChannel[Mainfrm.Cap.GsChan].ADCCalibrationFactor :=
        MainFrm.ADCVoltageRange / MainFrm.Cap.GsDisplayMax ;

    Mainfrm.Cap.GmChan := 5 ;
    MainFrm.ADCChannel[Mainfrm.Cap.GmChan].ADCName := 'Gm' ;
    MainFrm.ADCChannel[Mainfrm.Cap.GmChan].ADCUnits := 'nS' ;
    MainFrm.ADCChannel[Mainfrm.Cap.GmChan].ADCAmplifierGain := 1.0 ;
    MainFrm.ADCChannel[Mainfrm.Cap.GmChan].ADCCalibrationFactor :=
        MainFrm.ADCVoltageRange / MainFrm.Cap.GmDisplayMax ;

    Mainfrm.Cap.CmChan := 6 ;
    MainFrm.ADCChannel[Mainfrm.Cap.CmChan].ADCName := 'Cm' ;
    MainFrm.ADCChannel[Mainfrm.Cap.CmChan].ADCUnits := 'pF' ;
    MainFrm.ADCChannel[Mainfrm.Cap.CmChan].ADCAmplifierGain := 1.0 ;
    MainFrm.ADCChannel[Mainfrm.Cap.CmChan].ADCCalibrationFactor :=
        MainFrm.ADCVoltageRange / MainFrm.Cap.CmDisplayMax ;

    // Update channel table
    UpdateChannelEditTable ;

    end ;


procedure TSetupADCFrm.ckCapEnabledClick(Sender: TObject);
// -----------------------------------
// Enable/disable capacity calculation
// -----------------------------------
begin
     if ckCapEnabled.Checked then UpdateCapacityChannels ;
     end;

end.
