unit DirectControlUnit;
// =============================================
// Device Control Outputs - Direct Control Panel
// =============================================
// (c) J. Dempster, University of Strathclyde
// 20.12.2010
// 25.08.14 Digital output bits now controlled correctly
// 08.10.15 Track bar removed. Both digital and analog outputs updated using text box voltages

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ValidatedEdit, ExtCtrls, lightsourceunit, strutils, math  ;

type
  TDirectControlFrm = class(TForm)
    LSControlGrp: TGroupBox;
    LightSourcePanel0: TPanel;
    Label0: TLabel;
    ValidatedEdit0: TValidatedEdit;
    VStimGrp: TGroupBox;
    VStimPanel0: TPanel;
    Label20: TLabel;
    validatededit20: TValidatedEdit;
    DigStimGrp: TGroupBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    VStimPanel1: TPanel;
    Label7: TLabel;
    ValidatedEdit21: TValidatedEdit;
    VStimPanel2: TPanel;
    Label8: TLabel;
    ValidatedEdit22: TValidatedEdit;
    Splitter3: TSplitter;
    PhotoStimGrp: TGroupBox;
    PhotoStimPanel0: TPanel;
    Label40: TLabel;
    ValidatedEdit40: TValidatedEdit;
    PhotoStimPanel1: TPanel;
    Label41: TLabel;
    ValidatedEdit41: TValidatedEdit;
    PhotoStimPanel2: TPanel;
    Label42: TLabel;
    ValidatedEdit42: TValidatedEdit;
    PhotoStimPanel4: TPanel;
    Label144: TLabel;
    ValidatedEdit44: TValidatedEdit;
    PhotoStimPanel3: TPanel;
    Label143: TLabel;
    ValidatedEdit43: TValidatedEdit;
    Splitter4: TSplitter;
    CameraTriggerGrp: TGroupBox;
    LightSourcePanel1: TPanel;
    Label1: TLabel;
    ValidatedEdit1: TValidatedEdit;
    LightSourcePanel2: TPanel;
    Label2: TLabel;
    ValidatedEdit2: TValidatedEdit;
    LightSourcePanel3: TPanel;
    Label4: TLabel;
    ValidatedEdit4: TValidatedEdit;
    LightSourcePanel4: TPanel;
    Label5: TLabel;
    ValidatedEdit5: TValidatedEdit;
    LightSourcePanel5: TPanel;
    Label6: TLabel;
    ValidatedEdit6: TValidatedEdit;
    LightSourcePanel6: TPanel;
    Label3: TLabel;
    ValidatedEdit3: TValidatedEdit;
    LightSourcePanel7: TPanel;
    Label10: TLabel;
    ValidatedEdit7: TValidatedEdit;
    ShutterPanel: TPanel;
    Label9: TLabel;
    ValidatedEdit8: TValidatedEdit;
    DigStimPanel0: TPanel;
    Label11: TLabel;
    ValidatedEdit9: TValidatedEdit;
    DigStimPanel1: TPanel;
    Label12: TLabel;
    ValidatedEdit10: TValidatedEdit;
    DigStimPanel2: TPanel;
    Label13: TLabel;
    ValidatedEdit11: TValidatedEdit;
    DigStimPanel3: TPanel;
    Label16: TLabel;
    ValidatedEdit12: TValidatedEdit;
    DigStimPanel4: TPanel;
    Label17: TLabel;
    ValidatedEdit13: TValidatedEdit;
    DigStimPanel5: TPanel;
    Label18: TLabel;
    ValidatedEdit14: TValidatedEdit;
    DigStimPanel6: TPanel;
    Label19: TLabel;
    ValidatedEdit15: TValidatedEdit;
    DigStimPanel7: TPanel;
    Label21: TLabel;
    ValidatedEdit16: TValidatedEdit;
    PhotoStimPanel5: TPanel;
    Label22: TLabel;
    ValidatedEdit18: TValidatedEdit;
    CameraTriggerPanel: TPanel;
    Label15: TLabel;
    ValidatedEdit19: TValidatedEdit;
    Button1: TButton;
    bUpdateOutputs: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ValidatedEdit0KeyPress(Sender: TObject; var Key: Char);
    procedure bUpdateOutputsClick(Sender: TObject);

  private
    { Private declarations }

    procedure SetControlPanel(
          ControlPanel : TPanel ;
          Name : string ;
          iResource : Integer ;
          var PanelHeight : Integer );

    procedure GetControlPanel(
          ControlPanel : TPanel ;
          iResource : Integer
          ) ;

   procedure UpdateControls ;
   procedure UpdateOutputs ;


  public
    { Public declarations }
  end;

var
  DirectControlFrm: TDirectControlFrm;

implementation

uses LabIOUnit, Main;

{$R *.dfm}

const
    TrackBarVMax = 10.0 ;

procedure TDirectControlFrm.bUpdateOutputsClick(Sender: TObject);
begin
    UpdateOutputs ;
    UpdateControls ;
    end;

procedure TDirectControlFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     Action := caFree ;
     end;

procedure TDirectControlFrm.FormShow(Sender: TObject);
// ------------------------------
// Initialise form when displayed
// ------------------------------
begin

     // Get voltages for shutter closed condition to populate
     // VControl with name information
     LightSource.ShutterClosedVoltages ;

     UpdateControls ;

     ClientWidth := 350 ;
     ClientHeight := CameraTriggerGrp.Top + 100 ;

     end ;


procedure TDirectControlFrm.UpdateControls ;
// -----------------------
// Update control settings
// -----------------------
var
    H,i : Integer ;
    iLine : Integer ;
    OutputLines : Array[0..7] of Integer ;
begin
     H := 50 ;
     SetControlPanel( LightSourcePanel0,
                      LightSource.ControlLineName(0),
                      MainFrm.IOConfig.LSControlLine[0],
                      H ) ;
     SetControlPanel( LightSourcePanel1,
                      LightSource.ControlLineName(1),
                      MainFrm.IOConfig.LSControlLine[1],
                      H ) ;
     SetControlPanel( LightSourcePanel2,
                      LightSource.ControlLineName(2),
                      MainFrm.IOConfig.LSControlLine[2],
                      H ) ;
     SetControlPanel( LightSourcePanel3,
                      LightSource.ControlLineName(3),
                      MainFrm.IOConfig.LSControlLine[3],
                      H ) ;
     SetControlPanel( LightSourcePanel4,
                      LightSource.ControlLineName(4),
                      MainFrm.IOConfig.LSControlLine[4],
                      H ) ;
     SetControlPanel( LightSourcePanel5,
                      LightSource.ControlLineName(5),
                      MainFrm.IOConfig.LSControlLine[5],
                      H ) ;
     SetControlPanel( LightSourcePanel6,
                      LightSource.ControlLineName(6),
                      MainFrm.IOConfig.LSControlLine[6],
                      H ) ;

     SetControlPanel( LightSourcePanel7,
                      LightSource.ControlLineName(7),
                      MainFrm.IOConfig.LSControlLine[7],
                      H ) ;

     SetControlPanel( ShutterPanel,
                      'Light Source Shutter Control Output: ',
                      MainFrm.IOConfig.LSShutter,
                      H ) ;

     LSControlGrp.Height := H ;

//   Set Voltage stimulus outputs
     H := 50 ;
     SetControlPanel( VStimPanel0,'Analogue stimulus Vout.0',MainFrm.IOConfig.VCommand[0],H);
     SetControlPanel( VStimPanel1,'Analogue stimulus Vout.1',MainFrm.IOConfig.VCommand[1],H);
     SetControlPanel( VStimPanel2,'Analogue stimulus Vout.2',MainFrm.IOConfig.VCommand[2],H);
     VStimGrp.Height := H ;

//   Set digital stimulus outputs

     H := 50 ;
     for i := 0 to High(OutputLines) do begin
         OutputLines[i] := MaxResources ;
         iLine := MainFrm.IOConfig.DigitalStimStart+i ;
         if (iLine <= MainFrm.IOConfig.DigitalStimEnd) and
            (iLine <= MaxResources) then OutputLines[i] := iLine ;
         end ;

     SetControlPanel(  DigStimPanel0, 'Digital stimulus Dig.0', OutputLines[0],H);
     SetControlPanel(  DigStimPanel1, 'Digital stimulus Dig.1', OutputLines[1],H);
     SetControlPanel(  DigStimPanel2, 'Digital stimulus Dig.2', OutputLines[2],H);
     SetControlPanel(  DigStimPanel3, 'Digital stimulus Dig.3', OutputLines[3],H);
     SetControlPanel(  DigStimPanel4, 'Digital stimulus Dig.4', OutputLines[4],H);
     SetControlPanel(  DigStimPanel5, 'Digital stimulus Dig.5', OutputLines[5],H);
     SetControlPanel(  DigStimPanel6, 'Digital stimulus Dig.6', OutputLines[6],H);
     SetControlPanel(  DigStimPanel7, 'Digital stimulus Dig.7', OutputLines[7],H);

     DigStimGrp.Height := H ;

//   Set photo stimulator channels

     H := 50 ;
     SetControlPanel( PhotoStimPanel0,'PhotoStim: X galvo: ',MainFrm.IOConfig.PhotoStimX,H);
     SetControlPanel( PhotoStimPanel1,'PhotoStim: Y galvo: ',MainFrm.IOConfig.PhotoStimY,H);
     SetControlPanel( PhotoStimPanel2,'PhotoStim: Attenuator #1: ',MainFrm.IOConfig.PhotoStimI1,H);
     SetControlPanel( PhotoStimPanel3,'PhotoStim: Attenuator #2: ',MainFrm.IOConfig.PhotoStimI2,H);
     SetControlPanel( PhotoStimPanel4,'PhotoStim: Attenuator #3: ',MainFrm.IOConfig.PhotoStimI3,H);
     SetControlPanel( PhotoStimPanel5,'PhotoStim: Shutter: ',MainFrm.IOConfig.PhotoStimShutter,H);
     PhotoStimGrp.Height := H ;

//   Camera trigger output
     SetControlPanel( CameraTriggerPanel,'Camera Trigger Output: ',MainFrm.IOConfig.CameraStart,H);

     CameraTriggerGrp.Height := bUpdateOutputs.Top + bUpdateOutputs.Height + 10 ;


     end;


procedure TDirectControlFrm.SetControlPanel(
          ControlPanel : TPanel ;
          Name : string ;
          iResource : Integer ;
          var PanelHeight : Integer
          ) ;
// --------------------------------
// Set light source control outputs
// --------------------------------
var
    Lab : TLabel ;
    EditBox : TValidatedEdit ;
    i,Dev,Chan : Integer ;
    Bit : DWord ;
begin

    if (Name = '') or (iResource >= MaxResources) then begin
       ControlPanel.Visible := False ;
       Exit ;
       end
    else begin
       ControlPanel.Visible := True ;
       PanelHeight := Max( PanelHeight, ControlPanel.Height + ControlPanel.Top + 10 ) ;
       end ;

    // Identify controls on panel
    Lab := Nil ;
    EditBox := Nil ;
    for i := 0 to ControlPanel.ControlCount-1 do begin
        if AnsiContainsText(ControlPanel.Controls[i].Name,'label') then begin
           Lab := Tlabel(ControlPanel.Controls[i]) ;
           end
        else if AnsiContainsText(ControlPanel.Controls[i].Name,'validatededit') then begin
           EditBox := TValidatedEdit(ControlPanel.Controls[i]) ;
           end ;
        end ;

    if (Lab = Nil) or (EditBox = Nil) then Exit ;

    // Update controls
     if MainFRm.IOResourceAvailable(iResource) then begin
        Dev := LabIO.Resource[iResource].Device ;
        Chan := LabIO.Resource[iResource].StartChannel ;
        if LabIO.Resource[iResource].ResourceType = DACOut then begin
           Lab.Caption := format('%s Dev%d:AO%d',[Name,Dev,Chan]);
           EditBox.Value := LabIO.DACOutState[Dev][Chan] ;
           end
        else begin
           Lab.Caption := format('%s Dev%d:P0.%d',[Name,Dev,Chan]);
           Bit := 1 shl Chan ;
           if (LabIO.DigOutState[Dev] and Bit) <> 0 then EditBox.Value := 5.0
                                                   else EditBox.Value := 0.0
           end ;
        end;
      end ;

procedure TDirectControlFrm.GetControlPanel(
          ControlPanel : TPanel ;
          iResource : Integer
          ) ;
// ---------------------------------------
// Get light source control output setting
// ---------------------------------------
var
    EditBox : TValidatedEdit ;
    i,Dev,Chan : Integer ;
    Bit,BitMask : DWord ;
begin

    if iResource >= MaxResources then Exit ;

    // Identify controls on panel
    EditBox := Nil ;
    for i := 0 to ControlPanel.ControlCount-1 do
        if AnsiContainsText(ControlPanel.Controls[i].Name,'validatededit') then begin
           EditBox := TValidatedEdit(ControlPanel.Controls[i]) ;
           end ;
    if EditBox = Nil then Exit ;

    // Update controls
     if MainFRm.IOResourceAvailable(iResource) then begin
        Dev := LabIO.Resource[iResource].Device ;
        Chan := LabIO.Resource[iResource].StartChannel ;
        if LabIO.Resource[iResource].ResourceType = DACOut then begin
           LabIO.DACOutState[Dev][Chan] := EditBox.Value ;
           end
        else begin
           BitMask := not (1 shl Chan) ;
           if EditBox.Value > 0.0 then Bit := 1 shl Chan
                                  else Bit := 0 ;
           LabIO.DigOutState[Dev] := (LabIO.DigOutState[Dev] and BitMask) or Bit ;
           end ;
        end ;
     end;


procedure TDirectControlFrm.ValidatedEdit0KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
     if Key = #13 then begin
        UpdateOutputs ;
        UpdateControls ;
        end;
     end;


procedure TDirectControlFrm.UpdateOutputs ;
// ------------------------------------------
// Update all digital and DAC control outputs
// ------------------------------------------
var
    Dev,iDAC,iLine,i : Integer ;
    OutputLines : Array[0..7] of Integer ;
begin

     // Get Light source controls
     GetControlPanel(LightSourcePanel0,MainFrm.IOConfig.LSControlLine[0]) ;
     GetControlPanel(LightSourcePanel1,MainFrm.IOConfig.LSControlLine[1]) ;
     GetControlPanel(LightSourcePanel2,MainFrm.IOConfig.LSControlLine[2]) ;
     GetControlPanel(LightSourcePanel3,MainFrm.IOConfig.LSControlLine[3]) ;
     GetControlPanel(LightSourcePanel4,MainFrm.IOConfig.LSControlLine[4]) ;
     GetControlPanel(LightSourcePanel5,MainFrm.IOConfig.LSControlLine[5]) ;
     GetControlPanel(LightSourcePanel6,MainFrm.IOConfig.LSControlLine[6]) ;
     GetControlPanel(LightSourcePanel7,MainFrm.IOConfig.LSControlLine[7]) ;

     GetControlPanel(ShutterPanel,MainFrm.IOConfig.LSShutter) ;

     GetControlPanel( VStimPanel0,MainFrm.IOConfig.VCommand[0]);
     GetControlPanel( VStimPanel1,MainFrm.IOConfig.VCommand[1]);
     GetControlPanel( VStimPanel2,MainFrm.IOConfig.VCommand[2]);

//   Get digital stimulus outputs

     for i := 0 to High(OutputLines) do begin
         OutputLines[i] := MaxResources ;
         iLine := MainFrm.IOConfig.DigitalStimStart+i ;
         if (iLine <= MainFrm.IOConfig.DigitalStimEnd) and
            (iLine <= MaxResources) then OutputLines[i] := iLine ;
         end ;

     GetControlPanel(  DigStimPanel0,  OutputLines[0]);
     GetControlPanel(  DigStimPanel1,  OutputLines[1]);
     GetControlPanel(  DigStimPanel2,  OutputLines[2]);
     GetControlPanel(  DigStimPanel3,  OutputLines[3]);
     GetControlPanel(  DigStimPanel4,  OutputLines[4]);
     GetControlPanel(  DigStimPanel5,  OutputLines[5]);
     GetControlPanel(  DigStimPanel6,  OutputLines[6]);
     GetControlPanel(  DigStimPanel7,  OutputLines[7]);


//   Set photo stimulator channels

     GetControlPanel( PhotoStimPanel0,MainFrm.IOConfig.PhotoStimX);
     GetControlPanel( PhotoStimPanel1,MainFrm.IOConfig.PhotoStimY);
     GetControlPanel( PhotoStimPanel2,MainFrm.IOConfig.PhotoStimI1);
     GetControlPanel( PhotoStimPanel3,MainFrm.IOConfig.PhotoStimI2);
     GetControlPanel( PhotoStimPanel4,MainFrm.IOConfig.PhotoStimI3);
     GetControlPanel( PhotoStimPanel5,MainFrm.IOConfig.PhotoStimShutter);

//   Camera trigger output
     GetControlPanel( CameraTriggerPanel,MainFrm.IOConfig.CameraStart);


     // Update control outputs for all interface cards
     for Dev := 1 to LabIO.NumDevices do begin

         // Update DAC outputs
         if not LabIO.DACActive[Dev] then begin
            for iDAC := 0 to LabIO.NumDACs[Dev]-1 do begin
                LabIO.WriteDAC( Dev, LabIO.DACOutState[Dev][iDAC],iDAC) ;
                end ;
            end ;

         // Update digital outputs
         if not LabIO.DIGActive[Dev] then begin
            LabIO.WriteToDigitalOutPutPort( Dev, LabIO.DigOutState[Dev] ) ;
            end ;

        end ;


     end ;


end.
