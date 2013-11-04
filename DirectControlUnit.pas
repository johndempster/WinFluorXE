unit DirectControlUnit;
// =============================================
// Device Control Outputs - Direct Control Panel
// =============================================
// (c) J. Dempster, University of Strathclyde
// 20.12.2010

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ValidatedEdit, ExtCtrls, lightsourceunit, strutils, math  ;

type
  TDirectControlFrm = class(TForm)
    LSControlGrp: TGroupBox;
    LightSourcePanel0: TPanel;
    Label0: TLabel;
    Trackbar0: TTrackBar;
    ValidatedEdit0: TValidatedEdit;
    VStimGrp: TGroupBox;
    VStimPanel0: TPanel;
    Label20: TLabel;
    Trackbar20: TTrackBar;
    validatededit20: TValidatedEdit;
    DigStimGrp: TGroupBox;
    DigStimPanel0: TPanel;
    Label30: TLabel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    combobox30: TComboBox;
    DigStimPanel1: TPanel;
    Label31: TLabel;
    combobox31: TComboBox;
    DigStimPanel2: TPanel;
    Label32: TLabel;
    combobox32: TComboBox;
    DigStimPanel3: TPanel;
    Label33: TLabel;
    combobox33: TComboBox;
    DigStimPanel4: TPanel;
    Label34: TLabel;
    combobox34: TComboBox;
    DigStimPanel5: TPanel;
    Label35: TLabel;
    combobox35: TComboBox;
    DigStimPanel6: TPanel;
    label36: TLabel;
    combobox36: TComboBox;
    DigStimPanel7: TPanel;
    Label37: TLabel;
    combobox37: TComboBox;
    LightSourcePanel1: TPanel;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    ValidatedEdit1: TValidatedEdit;
    LightSourcePanel2: TPanel;
    Label2: TLabel;
    TrackBar2: TTrackBar;
    ValidatedEdit2: TValidatedEdit;
    LightSourcePanel3: TPanel;
    Label3: TLabel;
    TrackBar3: TTrackBar;
    ValidatedEdit3: TValidatedEdit;
    LightSourcePanel4: TPanel;
    Label5: TLabel;
    TrackBar4: TTrackBar;
    ValidatedEdit4: TValidatedEdit;
    LightSourcePanel5: TPanel;
    Label6: TLabel;
    TrackBar5: TTrackBar;
    ValidatedEdit5: TValidatedEdit;
    VStimPanel1: TPanel;
    Label7: TLabel;
    TrackBar21: TTrackBar;
    ValidatedEdit21: TValidatedEdit;
    VStimPanel2: TPanel;
    Label8: TLabel;
    TrackBar22: TTrackBar;
    ValidatedEdit22: TValidatedEdit;
    ShutterPanel: TPanel;
    Label9: TLabel;
    ComboBox1: TComboBox;
    Splitter3: TSplitter;
    PhotoStimGrp: TGroupBox;
    PhotoStimPanel0: TPanel;
    Label40: TLabel;
    TrackBar40: TTrackBar;
    ValidatedEdit40: TValidatedEdit;
    PhotoStimPanel1: TPanel;
    Label41: TLabel;
    TrackBar41: TTrackBar;
    ValidatedEdit41: TValidatedEdit;
    PhotoStimPanel2: TPanel;
    Label42: TLabel;
    TrackBar42: TTrackBar;
    ValidatedEdit42: TValidatedEdit;
    PhotoStimPanel4: TPanel;
    Label144: TLabel;
    TrackBar44: TTrackBar;
    ValidatedEdit44: TValidatedEdit;
    PhotoStimPanel3: TPanel;
    Label143: TLabel;
    TrackBar43: TTrackBar;
    ValidatedEdit43: TValidatedEdit;
    PhotoStimPanel5: TPanel;
    Label14: TLabel;
    ComboBox2: TComboBox;
    Splitter4: TSplitter;
    CameraTriggerGrp: TGroupBox;
    CameraTriggerPanel: TPanel;
    Label15: TLabel;
    ComboBox3: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ValidatedEdit0KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit1KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit2KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit3KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit4KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit5KeyPress(Sender: TObject; var Key: Char);
    procedure Trackbar0Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure validatededit20KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit21KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit22KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit40KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit41KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit42KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit43KeyPress(Sender: TObject; var Key: Char);
    procedure ValidatedEdit44KeyPress(Sender: TObject; var Key: Char);
    procedure Trackbar20Change(Sender: TObject);
    procedure TrackBar21Change(Sender: TObject);
    procedure TrackBar22Change(Sender: TObject);
    procedure TrackBar40Change(Sender: TObject);
    procedure TrackBar41Change(Sender: TObject);
    procedure TrackBar42Change(Sender: TObject);
    procedure TrackBar43Change(Sender: TObject);
    procedure TrackBar44Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure combobox30Change(Sender: TObject);
    procedure combobox31Change(Sender: TObject);
    procedure combobox32Change(Sender: TObject);
    procedure combobox33Change(Sender: TObject);
    procedure combobox34Change(Sender: TObject);
    procedure combobox35Change(Sender: TObject);
    procedure combobox36Change(Sender: TObject);
    procedure combobox37Change(Sender: TObject);
   
  private
    { Private declarations }
    VControl : Array[0..lsMaxVControl] of TLSVControl ;
    NumVControls : Integer ;

    procedure SetControlPanel(
          ControlPanel : TPanel ;
          iVControl : Integer ;
          var VControl : Array of TLSVControl ;
          NumVControls : Integer ;
          var PanelHeight : Integer
          ) ;

    procedure SetVStimChannel(
              ControlPanel : TPanel ;
              VChan : Integer ;
              var PanelHeight : Integer
              ) ;

    procedure SetDigStimChannel(
              ControlPanel : TPanel ;
              iChan : Integer ;
              var PanelHeight : Integer
              ) ;

   procedure SetPhotoStimChannel(
             ControlPanel : TPanel ;
             iResource : Integer ;
             Name : String ;
             var PanelHeight : Integer
             ) ;

   procedure SetDigControl(
             ControlPanel : TPanel ;
             iResource : Integer ;
             Name : String ;
             var PanelHeight : Integer
             ) ;

   procedure UpdateOutputs ;

   procedure SetDigOutputState(
          iResource : Integer ;  // Output channel resource
          iChan : Integer ;      // Output channel with resource
          iState : Integer ) ;   // Output state (1/0)



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

procedure TDirectControlFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     Action := caFree ;
     end;

procedure TDirectControlFrm.FormShow(Sender: TObject);
// ------------------------------
// Initialise form when displayed
// ------------------------------
var
    iV,Dev,H : Integer ;
begin

     // Get voltages for shutter closed condition to populate
     // VControl with name information
     LightSource.ShutterClosedVoltages( VControl, NumVControls ) ;

     // Get current default state of control lines

     for iV := 0 to NumVControls-1 do begin
         Dev := VControl[iV].Device ;
         VControl[iV].V := LabIO.DACOutState[Dev][VControl[iV].Chan] ;
         end ;

     H := 50 ;
     SetControlPanel( LightSourcePanel0, 0, VControl, NumVControls, H ) ;
     SetControlPanel( LightSourcePanel1, 1, VControl, NumVControls, H ) ;
     SetControlPanel( LightSourcePanel2, 2, VControl, NumVControls, H ) ;
     SetControlPanel( LightSourcePanel3, 3, VControl, NumVControls, H ) ;
     SetControlPanel( LightSourcePanel4, 4, VControl, NumVControls, H ) ;
     SetControlPanel( LightSourcePanel5, 5, VControl, NumVControls, H ) ;
     ShutterPanel.Top := H ;
     SetDigControl( ShutterPanel,
                    MainFrm.IOConfig.LSShutter,
                    'Light Source Shutter Control Output: ',
                    H ) ;
     LSControlGrp.Height := H ;


//   Set Voltage stimulus outputs
     H := 50 ;
     SetVStimChannel( VStimPanel0, 0, H ) ;
     SetVStimChannel( VStimPanel1, 1, H ) ;
     SetVStimChannel( VStimPanel2, 2, H ) ;
     VStimGrp.Height := H ;

//   Set digital stimulus outputs

     H := 50 ;
     SetDigStimChannel( DigStimPanel0, 0, H ) ;
     SetDigStimChannel( DigStimPanel1, 1, H ) ;
     SetDigStimChannel( DigStimPanel2, 2, H ) ;
     SetDigStimChannel( DigStimPanel3, 3, H ) ;
     SetDigStimChannel( DigStimPanel4, 4, H ) ;
     SetDigStimChannel( DigStimPanel5, 5, H ) ;
     SetDigStimChannel( DigStimPanel6, 6, H ) ;
     SetDigStimChannel( DigStimPanel7, 7, H ) ;
     DigStimGrp.Height := H ;

//   Set photo stimulator channels

     H := 50 ;
     SetPhotoStimChannel( PhotoStimPanel0,
                          MainFrm.IOConfig.PhotoStimX,
                          'PhotoStim: X galvo: ',
                          H ) ;
     SetPhotoStimChannel( PhotoStimPanel1,
                          MainFrm.IOConfig.PhotoStimY,
                          'PhotoStim: Y galvo: ',
                          H ) ;
     SetPhotoStimChannel( PhotoStimPanel2,
                          MainFrm.IOConfig.PhotoStimI1,
                          'PhotoStim: Attenuator #1: ',
                          H ) ;
     SetPhotoStimChannel( PhotoStimPanel3,
                          MainFrm.IOConfig.PhotoStimI2,
                          'PhotoStim: Attenuator #2: ',
                          H ) ;
     SetPhotoStimChannel( PhotoStimPanel4,
                          MainFrm.IOConfig.PhotoStimI3,
                          'PhotoStim: Attenuator #3: ',
                          H ) ;

     SetDigControl( PhotoStimPanel5,
                    MainFrm.IOConfig.PhotoStimShutter,
                    'PhotoStim: Shutter: ',
                    H ) ;
     PhotoStimGrp.Height := H ;

     SetDigControl( CameraTriggerPanel,
                    MainFrm.IOConfig.CameraStart,
                    'Camera Trigger Output: ',
                    H ) ;
     CameraTriggerGrp.Height := H ;

     ClientWidth := 450 ;
     ClientHeight := CameraTriggerGrp.Top + CameraTriggerGrp.Height + 50 ;  ;

     end;


procedure TDirectControlFrm.SetControlPanel(
          ControlPanel : TPanel ;
          iVControl : Integer ;
          var VControl : Array of TLSVControl ;
          NumVControls : Integer ;
          var PanelHeight : Integer
          ) ;
// --------------------------------
// Set light source control outputs
// --------------------------------
var
    Lab : TLabel ;
    TrackBar : TTrackBar ;
    EditBox : TValidatedEdit ;
    i : Integer ;
begin

    if iVControl < NumVControls then begin
       ControlPanel.Visible := True ;
       PanelHeight := Max( PanelHeight, ControlPanel.Height + ControlPanel.Top + 10 ) ;
       end
    else begin
       ControlPanel.Visible := False ;
       Exit ;
       end ;

    // Identify controls on panel
    for i := 0 to ControlPanel.ControlCount-1 do begin
        if AnsiContainsText(ControlPanel.Controls[i].Name,'label') then begin
           Lab := Tlabel(ControlPanel.Controls[i]) ;
           end
        else if AnsiContainsText(ControlPanel.Controls[i].Name,'trackbar') then begin
           TrackBar := TTrackbar(ControlPanel.Controls[i]) ;
           end
        else if AnsiContainsText(ControlPanel.Controls[i].Name,'validatededit') then begin
           EditBox := TValidatedEdit(ControlPanel.Controls[i]) ;
           end ;
        end ;

    // Update controls
     Lab.Caption := Format('%s: ',[VControl[iVControl].Name]) ;

     EditBox.Value := LabIO.DACOutState[VControl[iVControl].Device][VControl[iVControl].Chan] ;

    TrackBar.Position := Round((VControl[iVControl].V/TrackBarVMax)*TrackBar.Max) ;

    end ;

procedure TDirectControlFrm.SetVStimChannel(
          ControlPanel : TPanel ;
          VChan : Integer ;
          var PanelHeight : Integer
          ) ;
// -----------------------------------
// Set voltage stimulus output channel
// -----------------------------------
var
    Lab : TLabel ;
    TrackBar : TTrackBar ;
    EditBox : TValidatedEdit ;
    i : Integer ;
    DACDev,DACChan : Integer ;
begin

     // Skip if channel is not configured
     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[VChan]) then begin
        ControlPanel.Visible := False ;
        Exit ;
        end
     else begin
       ControlPanel.Visible := True ;
       PanelHeight := Max( PanelHeight, ControlPanel.Height + ControlPanel.Top + 10 ) ;
       end ;

    // Identify controls on panel
    for i := 0 to ControlPanel.ControlCount-1 do begin
        if AnsiContainsText(ControlPanel.Controls[i].Name,'label') then begin
           Lab := Tlabel(ControlPanel.Controls[i]) ;
           end
        else if AnsiContainsText(ControlPanel.Controls[i].Name,'trackbar') then begin
           TrackBar := TTrackbar(ControlPanel.Controls[i]) ;
           end
        else if AnsiContainsText(ControlPanel.Controls[i].Name,'validatededit') then begin
           EditBox := TValidatedEdit(ControlPanel.Controls[i]) ;
           end ;
        end ;

     DACDev := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].Device ;
     DACCHan := LabIO.Resource[MainFrm.IOConfig.VCommand[VChan]].StartChannel ;

     Lab.Caption := Format('Voltage Stimulus Output #%d: Dev%d:AO%d',
                    [VChan+1,DACDev,DACChan]) ;

     EditBox.Value := LabIO.DACOutState[DACDev][DACChan] ;
     Trackbar.Position := Round((EditBox.Value/TrackBarVMax)*Trackbar.Max) ;

     end ;


procedure TDirectControlFrm.SetDigStimChannel(
          ControlPanel : TPanel ;
          iChan : Integer ;
          var PanelHeight : Integer
          ) ;
// -----------------------------------
// Set digital stimulus output channel
// -----------------------------------
var
    Lab : TLabel ;
    ComboBox : TComboBox ;
    i : Integer ;
    Dev,DigStart,DigEnd : Integer ;
    OutputName : String ;
    Bit : Cardinal ;
begin

     // Skip if channel is not configured
     if (not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart)) then begin
        ControlPanel.Visible := False ;
        Exit ;
        end
     else begin
       ControlPanel.Visible := True ;
       end ;


    // Identify controls on panel
    for i := 0 to ControlPanel.ControlCount-1 do begin
        if AnsiContainsText(ControlPanel.Controls[i].Name,'label') then begin
           Lab := Tlabel(ControlPanel.Controls[i]) ;
           end
        else if AnsiContainsText(ControlPanel.Controls[i].Name,'combo') then begin
           ComboBox := TComboBox(ControlPanel.Controls[i]) ;
           end ;
        end ;

     Dev := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device ;
     DigStart := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel ;
     DigEnd := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel ;

     if iChan > (DigEnd - DigStart) then begin
        ControlPanel.Visible := False ;
        Exit ;
        end ;

     PanelHeight := Max( PanelHeight, ControlPanel.Height + ControlPanel.Top + 10 ) ;

     if LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].ResourceType = DACOut then begin
        OutputName := 'AO' ;
        if LabIO.DACOutState[Dev][DigStart+iChan] > 2.5 then ComboBox.ItemIndex := 1
                                                        else ComboBox.ItemIndex :=  0 ;
        end
     else begin
        OutputName := 'DO' ;
        Bit := 1 shl (DigStart + iChan) ;
        if (LabIO.DigOutState[Dev] or Bit) <> 0 then ComboBox.ItemIndex := 1
                                                else ComboBox.ItemIndex :=  0 ;
        end ;

     Lab.Caption := Format('Digital Stimulus Output #%d: Dev%d:%s%d',
                    [DigStart+iChan,Dev,OutputName,iChan+DigStart]) ;


     end ;


procedure TDirectControlFrm.SetDigControl(
          ControlPanel : TPanel ;
          iResource : Integer ;
          Name : String ;
          var PanelHeight : Integer
          ) ;
// ---------------------------------------
// Set digital (OV/5V) control output
// ---------------------------------------
var
    Lab : TLabel ;
    ComboBox : TComboBox ;
    i : Integer ;
    Dev,DigChan,DigStart,DigEnd : Integer ;
    OutputName : String ;
begin

     // Skip if channel is not configured
     if (not MainFrm.IOResourceAvailable(iResource)) then begin
        ControlPanel.Visible := False ;
        Exit ;
        end
     else begin
        ControlPanel.Visible := True ;
        PanelHeight := Max( PanelHeight, ControlPanel.Height + ControlPanel.Top + 10 ) ;
        end ;

    // Identify controls on panel
    for i := 0 to ControlPanel.ControlCount-1 do begin
        if AnsiContainsText(ControlPanel.Controls[i].Name,'label') then begin
           Lab := Tlabel(ControlPanel.Controls[i]) ;
           end
        else if AnsiContainsText(ControlPanel.Controls[i].Name,'combo') then begin
           ComboBox := TComboBox(ControlPanel.Controls[i]) ;
           end ;
        end ;

     Dev := LabIO.Resource[iResource].Device ;
     DigStart := LabIO.Resource[iResource].StartChannel ;

     if LabIO.Resource[iResource].ResourceType = DACOut then OutputName := 'AO'
                                                        else OutputName := 'DO' ;

     Lab.Caption := Format('%s: Dev%d:%s%d',
                    [Name,Dev,OutputName,DigStart]) ;

//     if MainFrm.DefaultDigitalOutputs or

//     cbControlValue.ItemIndex := LabIO.DACOutState[DACDev][DACChan] ;

     end ;

procedure TDirectControlFrm.SetPhotoStimChannel(
          ControlPanel : TPanel ;
          iResource : Integer ;
          Name : String ;
          var PanelHeight : Integer
          ) ;
// -----------------------------------
// Set photo stimulus output channel
// -----------------------------------
var
    Lab : TLabel ;
    TrackBar : TTrackBar ;
    EditBox : TValidatedEdit ;
    i : Integer ;
    DACDev,DACChan : Integer ;
begin

     // Skip if channel is not configured
     if not MainFrm.IOResourceAvailable(iResource) then begin
        ControlPanel.Visible := False ;
        Exit ;
        end
     else begin
       ControlPanel.Visible := True ;
       PanelHeight := Max( PanelHeight, ControlPanel.Height + ControlPanel.Top + 10 ) ;
       end ;

    // Identify controls on panel
    for i := 0 to ControlPanel.ControlCount-1 do begin
        if AnsiContainsText(ControlPanel.Controls[i].Name,'label') then begin
           Lab := Tlabel(ControlPanel.Controls[i]) ;
           end
        else if AnsiContainsText(ControlPanel.Controls[i].Name,'trackbar') then begin
           TrackBar := TTrackbar(ControlPanel.Controls[i]) ;
           end
        else if AnsiContainsText(ControlPanel.Controls[i].Name,'validatededit') then begin
           EditBox := TValidatedEdit(ControlPanel.Controls[i]) ;
           end ;
        end ;

     DACDev := LabIO.Resource[iResource].Device ;
     DACCHan := LabIO.Resource[iResource].StartChannel ;

     Lab.Caption := Format('%s: Dev%d:AO%d',
                    [Name,DACDev,DACChan]) ;

     EditBox.Value := LabIO.DACOutState[DACDev][DACChan] ;
     Trackbar.Position := Round((EditBox.Value/TrackBarVMax)*Trackbar.Max) ;

     end ;



procedure TDirectControlFrm.ValidatedEdit0KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
     if Key = #13 then begin
        LabIO.DACOutState[VControl[0].Device][VControl[0].Chan] := ValidatedEdit0.Value ;
        TrackBar0.Position := Round((ValidatedEdit0.Value/TrackBarVMax)*TrackBar0.Max) ;
        end ;
     end;


procedure TDirectControlFrm.ValidatedEdit1KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
     if Key = #13 then begin
        LabIO.DACOutState[VControl[1].Device][VControl[1].Chan] := ValidatedEdit1.Value ;
        TrackBar1.Position := Round((ValidatedEdit1.Value/TrackBarVMax)*TrackBar1.Max) ;
        end ;
     end;


procedure TDirectControlFrm.ValidatedEdit2KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
     if Key = #13 then begin
        LabIO.DACOutState[VControl[2].Device][VControl[2].Chan] := ValidatedEdit2.Value ;
        TrackBar2.Position := Round((ValidatedEdit2.Value/TrackBarVMax)*TrackBar2.Max) ;
        end ;
     end;


procedure TDirectControlFrm.ValidatedEdit3KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
     if Key = #13 then begin
        LabIO.DACOutState[VControl[3].Device][VControl[3].Chan] := ValidatedEdit3.Value ;
        TrackBar3.Position := Round((ValidatedEdit3.Value/TrackBarVMax)*TrackBar3.Max) ;
        end ;
     end;


procedure TDirectControlFrm.ValidatedEdit4KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
     if Key = #13 then begin
        LabIO.DACOutState[VControl[4].Device][VControl[4].Chan] := ValidatedEdit4.Value ;
        TrackBar4.Position := Round((ValidatedEdit4.Value/TrackBarVMax)*TrackBar4.Max) ;
        end ;
     end;


procedure TDirectControlFrm.ValidatedEdit5KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
     if Key = #13 then begin
        LabIO.DACOutState[VControl[5].Device][VControl[5].Chan] := ValidatedEdit5.Value ;
        TrackBar5.Position := Round((ValidatedEdit5.Value/TrackBarVMax)*TrackBar5.Max) ;
        end ;
     end;


procedure TDirectControlFrm.UpdateOutputs ;
// ------------------------------------------
// Update all digital and DAC control outputs
// ------------------------------------------
var
    Dev,iDAC : Integer ;
begin

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
     //       LabIO.WriteToDigitalOutPutPort( Dev, LabIO.DigOutState[Dev] ) ;
            end ;

        end ;


     end ;


procedure TDirectControlFrm.Trackbar0Change(Sender: TObject);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
    if NumVControls > 0 then begin
       ValidatedEdit0.Value := (TrackBar0.Position*TrackBarVMax)/TrackBar0.Max ;
       LabIO.DACOutState[VControl[0].Device][VControl[0].Chan] := ValidatedEdit0.Value ;
       UpdateOutputs ;
       end ;

    end;

procedure TDirectControlFrm.TrackBar1Change(Sender: TObject);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
    if NumVControls > 1 then begin
       ValidatedEdit1.Value := (TrackBar1.Position*TrackBarVMax)/TrackBar1.Max ;
       LabIO.DACOutState[VControl[1].Device][VControl[1].Chan] := ValidatedEdit1.Value ;
       UpdateOutputs ;
       end ;

    end;



procedure TDirectControlFrm.TrackBar2Change(Sender: TObject);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
    if NumVControls > 2 then begin
       ValidatedEdit2.Value := (TrackBar2.Position*TrackBarVMax)/TrackBar2.Max ;
       LabIO.DACOutState[VControl[2].Device][VControl[2].Chan] := ValidatedEdit2.Value ;
       UpdateOutputs ;
       end ;

    end;


procedure TDirectControlFrm.TrackBar3Change(Sender: TObject);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
    if NumVControls > 3 then begin
       ValidatedEdit3.Value := (TrackBar3.Position*TrackBarVMax)/TrackBar3.Max ;
       LabIO.DACOutState[VControl[3].Device][VControl[3].Chan] := ValidatedEdit3.Value ;
       UpdateOutputs ;
       end ;

    end;



procedure TDirectControlFrm.TrackBar4Change(Sender: TObject);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
    if NumVControls > 4 then begin
       ValidatedEdit4.Value := (TrackBar4.Position*TrackBarVMax)/TrackBar4.Max ;
       LabIO.DACOutState[VControl[4].Device][VControl[4].Chan] := ValidatedEdit4.Value ;
       UpdateOutputs ;
       end ;

    end;


procedure TDirectControlFrm.TrackBar5Change(Sender: TObject);
// -----------------------------
// Update output channel voltage
// -----------------------------
begin
    if NumVControls > 5 then begin
       ValidatedEdit5.Value := (TrackBar5.Position*TrackBarVMax)/TrackBar5.Max ;
       LabIO.DACOutState[VControl[5].Device][VControl[5].Chan] := ValidatedEdit5.Value ;
       UpdateOutputs ;
       end ;

    end;


procedure TDirectControlFrm.validatededit20KeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------------
// Update voltage stimulus ch. 0 output
// ------------------------------------
var
    Dev,DACChan: Integer ;
begin
     if Key = #13 then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.VCommand[0]].Device ;
        DACCHan := LabIO.Resource[MainFrm.IOConfig.VCommand[0]].StartChannel ;
        LabIO.DACOutState[Dev][DACChan] := ValidatedEdit20.Value ;
        TrackBar20.Position := Round((ValidatedEdit20.Value/TrackBarVMax)*TrackBar20.Max) ;
        end ;
     end;




procedure TDirectControlFrm.ValidatedEdit21KeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------------
// Update voltage stimulus ch. 1 output
// ------------------------------------
var
    Dev,DACChan: Integer ;
begin
     if Key = #13 then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.VCommand[1]].Device ;
        DACCHan := LabIO.Resource[MainFrm.IOConfig.VCommand[1]].StartChannel ;
        LabIO.DACOutState[Dev][DACChan] := ValidatedEdit21.Value ;
        TrackBar21.Position := Round((ValidatedEdit21.Value/TrackBarVMax)*TrackBar21.Max) ;
        end ;
     end;



procedure TDirectControlFrm.ValidatedEdit22KeyPress(Sender: TObject;
  var Key: Char);
// ------------------------------------
// Update voltage stimulus ch. 2 output
// ------------------------------------
var
    Dev,DACChan: Integer ;
begin
     if Key = #13 then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.VCommand[2]].Device ;
        DACCHan := LabIO.Resource[MainFrm.IOConfig.VCommand[2]].StartChannel ;
        LabIO.DACOutState[Dev][DACChan] := ValidatedEdit22.Value ;
        TrackBar22.Position := Round((ValidatedEdit22.Value/TrackBarVMax)*TrackBar22.Max) ;
        end ;
     end;



procedure TDirectControlFrm.ValidatedEdit40KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------------------
// Update photo stim output channel voltage MainFrm.IOConfig.PhotoStimX
// -----------------------------------------
var
    Dev,DACchan : Integer ;
begin
     if Key = #13 then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.PhotoStimX].Device ;
        DACchan := LabIO.Resource[MainFrm.IOConfig.PhotoStimX].StartChannel ;
        LabIO.DACOutState[Dev,DACchan] := ValidatedEdit40.Value ;
        TrackBar40.Position := Round((ValidatedEdit40.Value/TrackBarVMax)*TrackBar40.Max) ;
        end ;
     end;




procedure TDirectControlFrm.ValidatedEdit41KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------------------
// Update photo stim output channel voltage MainFrm.IOConfig.PhotoStimY
// -----------------------------------------
var
    Dev,DACchan : Integer ;
begin
     if Key = #13 then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.PhotoStimY].Device ;
        DACchan := LabIO.Resource[MainFrm.IOConfig.PhotoStimY].StartChannel ;
        LabIO.DACOutState[Dev,DACchan] := ValidatedEdit41.Value ;
        TrackBar41.Position := Round((ValidatedEdit41.Value/TrackBarVMax)*TrackBar41.Max) ;
        end ;
     end;


procedure TDirectControlFrm.ValidatedEdit42KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------------------
// Update photo stim output channel voltage MainFrm.IOConfig.PhotoStimI1
// -----------------------------------------
var
    Dev,DACchan : Integer ;
begin
     if Key = #13 then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.PhotoStimI1].Device ;
        DACchan := LabIO.Resource[MainFrm.IOConfig.PhotoStimI1].StartChannel ;
        LabIO.DACOutState[Dev,DACchan] := ValidatedEdit42.Value ;
        TrackBar42.Position := Round((ValidatedEdit42.Value/TrackBarVMax)*TrackBar42.Max) ;
        end ;
     end;


procedure TDirectControlFrm.ValidatedEdit43KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------------------
// Update photo stim output channel voltage MainFrm.IOConfig.PhotoStimI2
// -----------------------------------------
var
    Dev,DACchan : Integer ;
begin
     if Key = #13 then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.PhotoStimI2].Device ;
        DACchan := LabIO.Resource[MainFrm.IOConfig.PhotoStimI2].StartChannel ;
        LabIO.DACOutState[Dev,DACchan] := ValidatedEdit43.Value ;
        TrackBar43.Position := Round((ValidatedEdit43.Value/TrackBarVMax)*TrackBar43.Max) ;
        end ;
     end;

procedure TDirectControlFrm.ValidatedEdit44KeyPress(Sender: TObject;
  var Key: Char);
// -----------------------------------------
// Update photo stim output channel voltage MainFrm.IOConfig.PhotoStimI3
// -----------------------------------------
var
    Dev,DACchan : Integer ;
begin
     if Key = #13 then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.PhotoStimI3].Device ;
        DACchan := LabIO.Resource[MainFrm.IOConfig.PhotoStimI3].StartChannel ;
        LabIO.DACOutState[Dev,DACchan] := ValidatedEdit43.Value ;
        TrackBar43.Position := Round((ValidatedEdit43.Value/TrackBarVMax)*TrackBar43.Max) ;
        end ;
     end;

procedure TDirectControlFrm.Trackbar20Change(Sender: TObject);
// -----------------------------
// Update Vstim Ch.0 output
// -----------------------------
var
    Dev,DACchan : Integer ;
begin

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[0]) then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.VCommand[0]].Device ;
        DACChan := LabIO.Resource[MainFrm.IOConfig.VCommand[0]].StartChannel ;
        ValidatedEdit20.Value := (TrackBar20.Position*TrackBarVMax)/TrackBar20.Max ;
        LabIO.DACOutState[Dev,DACChan] := ValidatedEdit20.Value ;
        UpdateOutputs ;
        end ;

    end;


procedure TDirectControlFrm.TrackBar21Change(Sender: TObject);
// -----------------------------
// Update Vstim Ch.1 output
// -----------------------------
var
    Dev,DACchan : Integer ;
begin

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[1]) then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.VCommand[1]].Device ;
        DACChan := LabIO.Resource[MainFrm.IOConfig.VCommand[1]].StartChannel ;
        ValidatedEdit21.Value := (TrackBar21.Position*TrackBarVMax)/TrackBar21.Max ;
        LabIO.DACOutState[Dev,DACChan] := ValidatedEdit21.Value ;
        UpdateOutputs ;
        end ;

    end;


procedure TDirectControlFrm.TrackBar22Change(Sender: TObject);
// -----------------------------
// Update Vstim Ch.2 output
// -----------------------------
var
    Dev,DACchan : Integer ;
begin

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[2]) then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.VCommand[2]].Device ;
        DACChan := LabIO.Resource[MainFrm.IOConfig.VCommand[2]].StartChannel ;
        ValidatedEdit22.Value := (TrackBar21.Position*TrackBarVMax)/TrackBar22.Max ;
        LabIO.DACOutState[Dev,DACChan] := ValidatedEdit22.Value ;
        UpdateOutputs ;
        end ;

    end;


procedure TDirectControlFrm.TrackBar40Change(Sender: TObject);
// --------------------------------
// Update Photo stim X galvo output
// --------------------------------
var
    Dev,DACchan : Integer ;
begin

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimX) then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.PhotoStimX].Device ;
        DACChan := LabIO.Resource[MainFrm.IOConfig.PhotoStimX].StartChannel ;
        ValidatedEdit40.Value := (TrackBar40.Position*TrackBarVMax)/TrackBar40.Max ;
        LabIO.DACOutState[Dev,DACChan] := ValidatedEdit40.Value ;
        UpdateOutputs ;
        end ;

    end;


procedure TDirectControlFrm.TrackBar41Change(Sender: TObject);
// --------------------------------
// Update Photo stim Y galvo output
// --------------------------------
var
    Dev,DACchan : Integer ;
begin

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimY) then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.PhotoStimY].Device ;
        DACChan := LabIO.Resource[MainFrm.IOConfig.PhotoStimY].StartChannel ;
        ValidatedEdit41.Value := (TrackBar41.Position*TrackBarVMax)/TrackBar41.Max ;
        LabIO.DACOutState[Dev,DACChan] := ValidatedEdit41.Value ;
        UpdateOutputs ;
        end ;

    end;


procedure TDirectControlFrm.TrackBar42Change(Sender: TObject);
// --------------------------------
// Update Photo stim Attenuator#1 galvo output
// --------------------------------
var
    Dev,DACchan : Integer ;
begin

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI1) then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.PhotoStimI1].Device ;
        DACChan := LabIO.Resource[MainFrm.IOConfig.PhotoStimI1].StartChannel ;
        ValidatedEdit42.Value := (TrackBar42.Position*TrackBarVMax)/TrackBar42.Max ;
        LabIO.DACOutState[Dev,DACChan] := ValidatedEdit42.Value ;
        UpdateOutputs ;
        end ;

    end;


procedure TDirectControlFrm.TrackBar43Change(Sender: TObject);
// --------------------------------
// Update Photo stim Attenuator #2 galvo output
// --------------------------------
var
    Dev,DACchan : Integer ;
begin

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI2) then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.PhotoStimI2].Device ;
        DACChan := LabIO.Resource[MainFrm.IOConfig.PhotoStimI2].StartChannel ;
        ValidatedEdit43.Value := (TrackBar43.Position*TrackBarVMax)/TrackBar43.Max ;
        LabIO.DACOutState[Dev,DACChan] := ValidatedEdit43.Value ;
        UpdateOutputs ;
        end ;

    end;


procedure TDirectControlFrm.TrackBar44Change(Sender: TObject);
// --------------------------------
// Update Photo stim Attenuator #2 galvo output
// --------------------------------
var
    Dev,DACchan : Integer ;
begin

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.PhotoStimI3) then begin
        Dev := LabIO.Resource[MainFrm.IOConfig.PhotoStimI3].Device ;
        DACChan := LabIO.Resource[MainFrm.IOConfig.PhotoStimI3].StartChannel ;
        ValidatedEdit44.Value := (TrackBar44.Position*TrackBarVMax)/TrackBar44.Max ;
        LabIO.DACOutState[Dev,DACChan] := ValidatedEdit44.Value ;
        UpdateOutputs ;
        end ;

    end;


procedure TDirectControlFrm.ComboBox1Change(Sender: TObject);
// --------------------------------------
// Set light source shutter control output
// --------------------------------------
//
begin
     SetDigOutputState( MainFrm.IOConfig.LSShutter,
                        0, TComboBox(Sender).ItemIndex
                        ) ;
     end ;


procedure TDirectControlFrm.SetDigOutputState(
          iResource : Integer ;  // Output channel resource
          iChan : Integer ;      // Output channel with resource
          iState : Integer ) ;   // Output state (1/0)
// ------------------------
// Set digital output state
// ------------------------
var
    Dev,StartChan,iBit,iMask : Integer ;
begin

     if MainFrm.IOResourceAvailable(iResource) then begin
        Dev := LabIO.Resource[iResource].Device ;
        StartChan := LabIO.Resource[iResource].StartChannel ;
        if LabIO.Resource[iResource].ResourceType = DACOut then begin
           // DAC output
           LabIO.DACOutState[Dev,StartChan] := iState*5.0 ;
           end
        else begin
           // Digital output
             iBit := 1 shl StartChan ;
             iMask := not iBit ;
             LabIO.DigOutState[Dev] := (LabIO.DigOutState[Dev] and iMask) or iBit ;
             end ;
        end ;
     end ;


procedure TDirectControlFrm.ComboBox2Change(Sender: TObject);
// --------------------------------------
// Set photo stim shutter control output
// --------------------------------------
//
begin
     SetDigOutputState( MainFrm.IOConfig.PhotoStimShutter,
                        0,
                        TComboBox(Sender).ItemIndex
                        ) ;
     end ;


procedure TDirectControlFrm.ComboBox3Change(Sender: TObject);
// --------------------------------------
// Set Camera trigger control output
// --------------------------------------
//
begin
     SetDigOutputState( MainFrm.IOConfig.CameraStart,
                        0,
                        TComboBox(Sender).ItemIndex
                        ) ;
     end ;

procedure TDirectControlFrm.combobox30Change(Sender: TObject);
// --------------------------------------
// Set digital stimulus output #0
// --------------------------------------
//
var
    iChan,NumChans : Integer ;
begin

     iChan := 0 ;

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) and
        MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then begin
        NumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel -
                    LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel + 1 ;
        end
     else NumChans := 0 ;

     if iChan < NumChans then begin
        SetDigOutputState( MainFrm.IOConfig.DigitalStimStart,
                           iChan,
                           TComboBox(Sender).ItemIndex
                           ) ;
        end ;
     end ;

procedure TDirectControlFrm.combobox31Change(Sender: TObject);
// --------------------------------------
// Set digital stimulus output #0
// --------------------------------------
//
var
    iChan,NumChans : Integer ;
begin

     iChan := 1 ;

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) and
        MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then begin
        NumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel -
                    LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel + 1 ;
        end
     else NumChans := 0 ;

     if iChan < NumChans then begin
        SetDigOutputState( MainFrm.IOConfig.DigitalStimStart,
                           iChan,
                           TComboBox(Sender).ItemIndex
                           ) ;
        end ;
     end ;


procedure TDirectControlFrm.combobox32Change(Sender: TObject);
// --------------------------------------
// Set digital stimulus output #0
// --------------------------------------
//
var
    iChan,NumChans : Integer ;
begin

     iChan := 2 ;

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) and
        MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then begin
        NumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel -
                    LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel + 1 ;
        end
     else NumChans := 0 ;

     if iChan < NumChans then begin
        SetDigOutputState( MainFrm.IOConfig.DigitalStimStart,
                           iChan,
                           TComboBox(Sender).ItemIndex
                           ) ;
        end ;
     end ;


procedure TDirectControlFrm.combobox33Change(Sender: TObject);
// --------------------------------------
// Set digital stimulus output #0
// --------------------------------------
//
var
    iChan,NumChans : Integer ;
begin

     iChan := 3 ;

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) and
        MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then begin
        NumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel -
                    LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel + 1 ;
        end
     else NumChans := 0 ;

     if iChan < NumChans then begin
        SetDigOutputState( MainFrm.IOConfig.DigitalStimStart,
                           iChan,
                           TComboBox(Sender).ItemIndex
                           ) ;
        end ;
     end ;


procedure TDirectControlFrm.combobox34Change(Sender: TObject);
// --------------------------------------
// Set digital stimulus output #0
// --------------------------------------
//
var
    iChan,NumChans : Integer ;
begin

     iChan := 4 ;

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) and
        MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then begin
        NumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel -
                    LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel + 1 ;
        end
     else NumChans := 0 ;

     if iChan < NumChans then begin
        SetDigOutputState( MainFrm.IOConfig.DigitalStimStart,
                           iChan,
                           TComboBox(Sender).ItemIndex
                           ) ;
        end ;
     end ;


procedure TDirectControlFrm.combobox35Change(Sender: TObject);
// --------------------------------------
// Set digital stimulus output #0
// --------------------------------------
//
var
    iChan,NumChans : Integer ;
begin

     iChan := 5 ;

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) and
        MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then begin
        NumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel -
                    LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel + 1 ;
        end
     else NumChans := 0 ;

     if iChan < NumChans then begin
        SetDigOutputState( MainFrm.IOConfig.DigitalStimStart,
                           iChan,
                           TComboBox(Sender).ItemIndex
                           ) ;
        end ;
     end ;


procedure TDirectControlFrm.combobox36Change(Sender: TObject);
// --------------------------------------
// Set digital stimulus output #0
// --------------------------------------
//
var
    iChan,NumChans : Integer ;
begin

     iChan := 6 ;

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) and
        MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then begin
        NumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel -
                    LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel + 1 ;
        end
     else NumChans := 0 ;

     if iChan < NumChans then begin
        SetDigOutputState( MainFrm.IOConfig.DigitalStimStart,
                           iChan,
                           TComboBox(Sender).ItemIndex
                           ) ;
        end ;
     end ;


procedure TDirectControlFrm.combobox37Change(Sender: TObject);
// --------------------------------------
// Set digital stimulus output #0
// --------------------------------------
//
var
    iChan,NumChans : Integer ;
begin

     iChan := 7 ;

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) and
        MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then begin
        NumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel -
                    LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel + 1 ;
        end
     else NumChans := 0 ;

     if iChan < NumChans then begin
        SetDigOutputState( MainFrm.IOConfig.DigitalStimStart,
                           iChan,
                           TComboBox(Sender).ItemIndex
                           ) ;
        end ;
     end ;


end.
