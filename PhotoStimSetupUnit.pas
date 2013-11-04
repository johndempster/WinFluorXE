unit PhotoStimSetupUnit;
//
// 21.11.12 PrarieView.log changed to PrairieView.log to work with PV 4.3
//          PraireView log file is now named prairieview.log rather than prarieview.log as in pre V4.3 versions

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ValidatedEdit, ComCtrls, FileCtrl;

type
  TPhotoStimSetupFrm = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    SetupTab: TPageControl;
    Attenuator1Tab: TTabSheet;
    Label30: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Shape2: TShape;
    rbPockelsCellA1: TRadioButton;
    Panel3: TPanel;
    Label50: TLabel;
    Label49: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label48: TLabel;
    Label47: TLabel;
    Panel2: TPanel;
    rbConoptics302A1: TRadioButton;
    rbConoptics302RMA1: TRadioButton;
    Panel1: TPanel;
    rbPolarizationCrossA1: TRadioButton;
    rbPolarizationParallelA1: TRadioButton;
    edBiasSettingA1: TValidatedEdit;
    edVoltagePiA1: TValidatedEdit;
    edPowerMaximumA1: TValidatedEdit;
    edPowerMinimumA1: TValidatedEdit;
    rbLinearRampA1: TRadioButton;
    Panel4: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edLinearPowerMinimumA1: TValidatedEdit;
    edLinearPowerMaximumA1: TValidatedEdit;
    edLinearVoltageMinimumA1: TValidatedEdit;
    edLinearVoltageMaximumA1: TValidatedEdit;
    cbEnableShutterA1: TCheckBox;
    edPhotoStimXCenter1: TValidatedEdit;
    edPhotoStimYCenter1: TValidatedEdit;
    edPhotoStimXScale1: TValidatedEdit;
    edPhotoStimYScale1: TValidatedEdit;
    Attenuator2Tab: TTabSheet;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Shape3: TShape;
    rbPockelsCellA2: TRadioButton;
    Panel5: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label14: TLabel;
    Panel6: TPanel;
    rbConoptics302A2: TRadioButton;
    rbConoptics302RMA2: TRadioButton;
    Panel7: TPanel;
    rbPolarizationCrossA2: TRadioButton;
    rbPolarizationParallelA2: TRadioButton;
    edBiasSettingA2: TValidatedEdit;
    edVoltagePiA2: TValidatedEdit;
    edPowerMaximumA2: TValidatedEdit;
    edPowerMinimumA2: TValidatedEdit;
    rbLinearRampA2: TRadioButton;
    Panel8: TPanel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    edLinearPowerMinimumA2: TValidatedEdit;
    edLinearPowerMaximumA2: TValidatedEdit;
    edLinearVoltageMinimumA2: TValidatedEdit;
    edLinearVoltageMaximumA2: TValidatedEdit;
    cbEnableShutterA2: TCheckBox;
    edPhotoStimXCenter2: TValidatedEdit;
    edPhotoStimYCenter2: TValidatedEdit;
    edPhotoStimXScale2: TValidatedEdit;
    edPhotoStimYScale2: TValidatedEdit;
    Attenuator3Tab: TTabSheet;
    Label19: TLabel;
    Shape4: TShape;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    rbPockelsCellA3: TRadioButton;
    rbLinearRampA3: TRadioButton;
    Panel9: TPanel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Panel10: TPanel;
    rbConoptics302A3: TRadioButton;
    rbConoptics302RMA3: TRadioButton;
    Panel11: TPanel;
    rbPolarizationCrossA3: TRadioButton;
    rbPolarizationParallelA3: TRadioButton;
    edBiasSettingA3: TValidatedEdit;
    edVoltagePiA3: TValidatedEdit;
    edPowerMaximumA3: TValidatedEdit;
    edPowerMinimumA3: TValidatedEdit;
    Panel12: TPanel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    edLinearPowerMinimumA3: TValidatedEdit;
    edLinearPowerMaximumA3: TValidatedEdit;
    edLinearVoltageMinimumA3: TValidatedEdit;
    edLinearVoltageMaximumA3: TValidatedEdit;
    cbEnableShutterA3: TCheckBox;
    edPhotoStimXCenter3: TValidatedEdit;
    edPhotoStimYCenter3: TValidatedEdit;
    edPhotoStimXScale3: TValidatedEdit;
    edPhotoStimYScale3: TValidatedEdit;
    PrairieView: TTabSheet;
    Label31: TLabel;
    lblPath: TLabel;
    btnBrowse: TButton;
    lblTerminator: TLabel;
    rbTerm0: TRadioButton;
    rbTerm1: TRadioButton;
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure rbPockelsCellA1Click(Sender: TObject);
    procedure rbPockelsCellA2Click(Sender: TObject);
    procedure rbPockelsCellA3Click(Sender: TObject);
    procedure rbLinearRampA1Click(Sender: TObject);
    procedure rbLinearRampA2Click(Sender: TObject);
    procedure rbLinearRampA3Click(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    procedure DisablePockelsCellA1;
    procedure DisablePockelsCellA2;
    procedure DisablePockelsCellA3;
    procedure DisableLinearRampA1;
    procedure DisableLinearRampA2;
    procedure DisableLinearRampA3;
    procedure EnablePockelsCellA1;
    procedure EnablePockelsCellA2;
    procedure EnablePockelsCellA3;
    procedure EnableLinearRampA1;
    procedure EnableLinearRampA2;
    procedure EnableLinearRampA3;
  public
    { Public declarations }
  private
    { Private declarations}
    PVFile : String;  // PrairieView log file path
  end;

var
  PhotoStimSetupFrm: TPhotoStimSetupFrm;

implementation

uses Main;

{$R *.dfm}

procedure TPhotoStimSetupFrm.bOKClick(Sender: TObject);
begin

  // Update gavlo 1 calibration
  MainFrm.PhotoStim.XCenter[1] := edPhotoStimXCenter1.Value;
  MainFrm.PhotoStim.YCenter[1] := edPhotoStimYCenter1.Value;
  MainFrm.PhotoStim.XScale[1] := edPhotoStimXScale1.Value;
  MainFrm.PhotoStim.YScale[1] := edPhotoStimYScale1.Value;

  // Update gavlo 2 calibration
  MainFrm.PhotoStim.XCenter[2] := edPhotoStimXCenter2.Value;
  MainFrm.PhotoStim.YCenter[2] := edPhotoStimYCenter2.Value;
  MainFrm.PhotoStim.XScale[2] := edPhotoStimXScale2.Value;
  MainFrm.PhotoStim.YScale[2] := edPhotoStimYScale2.Value;

  // Update gavlo 3 calibration
  MainFrm.PhotoStim.XCenter[3] := edPhotoStimXCenter3.Value;
  MainFrm.PhotoStim.YCenter[3] := edPhotoStimYCenter3.Value;
  MainFrm.PhotoStim.XScale[3] := edPhotoStimXScale3.Value;
  MainFrm.PhotoStim.YScale[3] := edPhotoStimYScale3.Value;

  // Update attenuator 1 settings in MainFrm
  if rbPockelsCellA1.Checked then MainFrm.PhotoStim.PCEnable[1] := True;
  if rbLinearRampA1.Checked then MainFrm.PhotoStim.PCEnable[1] := False;
  MainFrm.PhotoStim.PCPowerMin[1] := edPowerMinimumA1.Value;
  MainFrm.PhotoStim.PCPowerMax[1] := edPowerMaximumA1.Value;
  MainFrm.PhotoStim.PCBias[1] := edBiasSettingA1.Value;
  MainFrm.PhotoStim.PCVoltagePi[1] := edVoltagePiA1.Value;
  if rbPolarizationCrossA1.Checked then
    MainFrm.PhotoStim.PCPolarizationCross[1] := True ;
  if rbPolarizationParallelA1.Checked then
    MainFrm.PhotoStim.PCPolarizationCross[1] := False ;
  if rbConoptics302A1.Checked then
    MainFrm.PhotoStim.PCConoptics302[1] := True ;
  if rbConoptics302RMA1.Checked then
    MainFrm.PhotoStim.PCConoptics302[1] := False ;
  MainFrm.PhotoStim.LinearPowerMin[1] := edLinearPowerMinimumA1.Value;
  MainFrm.PhotoStim.LinearPowerMax[1] := edLinearPowerMaximumA1.Value;
  MainFrm.PhotoStim.LinearVoltageMin[1] := edLinearVoltageMinimumA1.Value;
  MainFrm.PhotoStim.LinearVoltageMax[1] := edLinearVoltageMaximumA1.Value;
  MainFrm.PhotoStim.EnableShutter[1] := cbEnableShutterA1.Checked;

  // Update attenuator 2 settings in MainFrm
  if rbPockelsCellA2.Checked then MainFrm.PhotoStim.PCEnable[2] := True;
  if rbLinearRampA2.Checked then MainFrm.PhotoStim.PCEnable[2] := False;
  MainFrm.PhotoStim.PCPowerMin[2] := edPowerMinimumA2.Value;
  MainFrm.PhotoStim.PCPowerMax[2] := edPowerMaximumA2.Value;
  MainFrm.PhotoStim.PCBias[2] := edBiasSettingA2.Value;
  MainFrm.PhotoStim.PCVoltagePi[2] := edVoltagePiA2.Value;
  if rbPolarizationCrossA2.Checked then
    MainFrm.PhotoStim.PCPolarizationCross[2] := True ;
  if rbPolarizationParallelA2.Checked then
    MainFrm.PhotoStim.PCPolarizationCross[2] := False ;
  if rbConoptics302A2.Checked then
    MainFrm.PhotoStim.PCConoptics302[2] := True ;
  if rbConoptics302RMA2.Checked then
    MainFrm.PhotoStim.PCConoptics302[2] := False ;
  MainFrm.PhotoStim.LinearPowerMin[2] := edLinearPowerMinimumA2.Value;
  MainFrm.PhotoStim.LinearPowerMax[2] := edLinearPowerMaximumA2.Value;
  MainFrm.PhotoStim.LinearVoltageMin[2] := edLinearVoltageMinimumA2.Value;
  MainFrm.PhotoStim.LinearVoltageMax[2] := edLinearVoltageMaximumA2.Value;
  MainFrm.PhotoStim.EnableShutter[2] := cbEnableShutterA2.Checked;

  // Update attenuator 3 settings in MainFrm
  if rbPockelsCellA3.Checked then MainFrm.PhotoStim.PCEnable[3] := True;
  if rbLinearRampA3.Checked then MainFrm.PhotoStim.PCEnable[3] := False;
  MainFrm.PhotoStim.PCPowerMin[3] := edPowerMinimumA3.Value;
  MainFrm.PhotoStim.PCPowerMax[3] := edPowerMaximumA3.Value;
  MainFrm.PhotoStim.PCBias[3] := edBiasSettingA3.Value;
  MainFrm.PhotoStim.PCVoltagePi[3] := edVoltagePiA3.Value;
  if rbPolarizationCrossA3.Checked then
    MainFrm.PhotoStim.PCPolarizationCross[3] := True ;
  if rbPolarizationParallelA3.Checked then
    MainFrm.PhotoStim.PCPolarizationCross[3] := False ;
  if rbConoptics302A3.Checked then
    MainFrm.PhotoStim.PCConoptics302[3] := True ;
  if rbConoptics302RMA3.Checked then
    MainFrm.PhotoStim.PCConoptics302[3] := False ;
  MainFrm.PhotoStim.LinearPowerMin[3] := edLinearPowerMinimumA3.Value;
  MainFrm.PhotoStim.LinearPowerMax[3] := edLinearPowerMaximumA3.Value;
  MainFrm.PhotoStim.LinearVoltageMin[3] := edLinearVoltageMinimumA3.Value;
  MainFrm.PhotoStim.LinearVoltageMax[3] := edLinearVoltageMaximumA3.Value;
  MainFrm.PhotoStim.EnableShutter[3] := cbEnableShutterA3.Checked;

  // Update PrairieView log file
  MainFrm.PhotoStim.PVLogFile := PVFile;

  // Update Ultima command terminator
  if rbTerm0.Checked then
    MainFrm.PhotoStim.CmdTermZero := True
  else
    MainFrm.PhotoStim.CmdTermZero := False;

  // Close form
  Close;

end;

procedure TPhotoStimSetupFrm.bCancelClick(Sender: TObject);
begin

  // Close form
  Close;

end;

procedure TPhotoStimSetupFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin

  // Really close the form
  Action := caFree;

end;

procedure TPhotoStimSetupFrm.FormShow(Sender: TObject);
begin

  // Update gavlo 1 calibration
  edPhotoStimXCenter1.Value := MainFrm.PhotoStim.XCenter[1];
  edPhotoStimYCenter1.Value := MainFrm.PhotoStim.YCenter[1];
  edPhotoStimXScale1.Value := MainFrm.PhotoStim.XScale[1];
  edPhotoStimYScale1.Value := MainFrm.PhotoStim.YScale[1];

  // Update gavlo 2 calibration
  edPhotoStimXCenter2.Value := MainFrm.PhotoStim.XCenter[2];
  edPhotoStimYCenter2.Value := MainFrm.PhotoStim.YCenter[2];
  edPhotoStimXScale2.Value := MainFrm.PhotoStim.XScale[2];
  edPhotoStimYScale2.Value := MainFrm.PhotoStim.YScale[2];

  // Update gavlo 3 calibration
  edPhotoStimXCenter3.Value := MainFrm.PhotoStim.XCenter[3];
  edPhotoStimYCenter3.Value := MainFrm.PhotoStim.YCenter[3];
  edPhotoStimXScale3.Value := MainFrm.PhotoStim.XScale[3];
  edPhotoStimYScale3.Value := MainFrm.PhotoStim.YScale[3];

  // Update attenuator 1 settings in MainFrm
  rbPockelsCellA1.Checked := MainFrm.PhotoStim.PCEnable[1];
  rbLinearRampA1.Checked := Not MainFrm.PhotoStim.PCEnable[1];
  edPowerMinimumA1.Value := MainFrm.PhotoStim.PCPowerMin[1];
  edPowerMaximumA1.Value := MainFrm.PhotoStim.PCPowerMax[1];
  edBiasSettingA1.Value := MainFrm.PhotoStim.PCBias[1];
  edVoltagePiA1.Value := MainFrm.PhotoStim.PCVoltagePi[1];
  rbPolarizationCrossA1.Checked := MainFrm.PhotoStim.PCPolarizationCross[1];
  rbPolarizationParallelA1.Checked :=
    Not MainFrm.PhotoStim.PCPolarizationCross[1];
  rbConoptics302A1.Checked := MainFrm.PhotoStim.PCConoptics302[1];
  rbConoptics302RMA1.Checked := Not MainFrm.PhotoStim.PCConoptics302[1];
  edLinearPowerMinimumA1.Value := MainFrm.PhotoStim.LinearPowerMin[1];
  edLinearPowerMaximumA1.Value := MainFrm.PhotoStim.LinearPowerMax[1];
  edLinearVoltageMinimumA1.Value := MainFrm.PhotoStim.LinearVoltageMin[1];
  edLinearVoltageMaximumA1.Value := MainFrm.PhotoStim.LinearVoltageMax[1];
  cbEnableShutterA1.Checked := MainFrm.PhotoStim.EnableShutter[1];

  // Update attenuator 2 settings in MainFrm
  rbPockelsCellA2.Checked := MainFrm.PhotoStim.PCEnable[2];
  rbLinearRampA2.Checked := Not MainFrm.PhotoStim.PCEnable[2];
  edPowerMinimumA2.Value := MainFrm.PhotoStim.PCPowerMin[2];
  edPowerMaximumA2.Value := MainFrm.PhotoStim.PCPowerMax[2];
  edBiasSettingA2.Value := MainFrm.PhotoStim.PCBias[2];
  edVoltagePiA2.Value := MainFrm.PhotoStim.PCVoltagePi[2];
  rbPolarizationCrossA2.Checked := MainFrm.PhotoStim.PCPolarizationCross[2];
  rbPolarizationParallelA2.Checked :=
    Not MainFrm.PhotoStim.PCPolarizationCross[2];
  rbConoptics302A2.Checked := MainFrm.PhotoStim.PCConoptics302[2];
  rbConoptics302RMA2.Checked := Not MainFrm.PhotoStim.PCConoptics302[2];
  edLinearPowerMinimumA2.Value := MainFrm.PhotoStim.LinearPowerMin[2];
  edLinearPowerMaximumA2.Value := MainFrm.PhotoStim.LinearPowerMax[2];
  edLinearVoltageMinimumA2.Value := MainFrm.PhotoStim.LinearVoltageMin[2];
  edLinearVoltageMaximumA2.Value := MainFrm.PhotoStim.LinearVoltageMax[2];
  cbEnableShutterA2.Checked := MainFrm.PhotoStim.EnableShutter[2];

  // Update attenuator 3 settings in MainFrm
  rbPockelsCellA3.Checked := MainFrm.PhotoStim.PCEnable[3];
  rbLinearRampA3.Checked := Not MainFrm.PhotoStim.PCEnable[3];
  edPowerMinimumA3.Value := MainFrm.PhotoStim.PCPowerMin[3];
  edPowerMaximumA3.Value := MainFrm.PhotoStim.PCPowerMax[3];
  edBiasSettingA3.Value := MainFrm.PhotoStim.PCBias[3];
  edVoltagePiA3.Value := MainFrm.PhotoStim.PCVoltagePi[3];
  rbPolarizationCrossA3.Checked := MainFrm.PhotoStim.PCPolarizationCross[3];
  rbPolarizationParallelA3.Checked :=
    Not MainFrm.PhotoStim.PCPolarizationCross[3];
  rbConoptics302A3.Checked := MainFrm.PhotoStim.PCConoptics302[3];
  rbConoptics302RMA3.Checked := Not MainFrm.PhotoStim.PCConoptics302[3];
  edLinearPowerMinimumA3.Value := MainFrm.PhotoStim.LinearPowerMin[3];
  edLinearPowerMaximumA3.Value := MainFrm.PhotoStim.LinearPowerMax[3];
  edLinearVoltageMinimumA3.Value := MainFrm.PhotoStim.LinearVoltageMin[3];
  edLinearVoltageMaximumA3.Value := MainFrm.PhotoStim.LinearVoltageMax[3];
  cbEnableShutterA3.Checked := MainFrm.PhotoStim.EnableShutter[3];

  // Update PrairieView log file
  PVFile := MainFrm.PhotoStim.PVLogFile;
  lblPath.Caption := PVFile;

  // Update Ultima command terminator
  rbTerm0.Checked := MainFrm.PhotoStim.CmdTermZero;
  rbTerm1.Checked := not MainFrm.PhotoStim.CmdTermZero;

end;


procedure TPhotoStimSetupFrm.DisablePockelsCellA1;
begin

  edPowerMinimumA1.Enabled := False;
  edPowerMaximumA1.Enabled := False;
  edVoltagePiA1.Enabled := False;
  edBiasSettingA1.Enabled := False;
  rbPolarizationParallelA1.Enabled := False;
  rbPolarizationCrossA1.Enabled := False;

end;


procedure TPhotoStimSetupFrm.DisablePockelsCellA2;
begin

  edPowerMinimumA2.Enabled := False;
  edPowerMaximumA2.Enabled := False;
  edVoltagePiA2.Enabled := False;
  edBiasSettingA2.Enabled := False;
  rbPolarizationParallelA2.Enabled := False;
  rbPolarizationCrossA2.Enabled := False;

end;


procedure TPhotoStimSetupFrm.DisablePockelsCellA3;
begin

  edPowerMinimumA3.Enabled := False;
  edPowerMaximumA3.Enabled := False;
  edVoltagePiA3.Enabled := False;
  edBiasSettingA3.Enabled := False;
  rbPolarizationParallelA3.Enabled := False;
  rbPolarizationCrossA3.Enabled := False;

end;


procedure TPhotoStimSetupFrm.DisableLinearRampA1;
begin

  edLinearPowerMinimumA1.Enabled := False;
  edLinearPowerMaximumA1.Enabled := False;
  edLinearVoltageMinimumA1.Enabled := False;
  edLinearVoltageMaximumA1.Enabled := False;

end;


procedure TPhotoStimSetupFrm.DisableLinearRampA2;
begin

  edLinearPowerMinimumA2.Enabled := False;
  edLinearPowerMaximumA2.Enabled := False;
  edLinearVoltageMinimumA2.Enabled := False;
  edLinearVoltageMaximumA2.Enabled := False;

end;


procedure TPhotoStimSetupFrm.DisableLinearRampA3;
begin

  edLinearPowerMinimumA3.Enabled := False;
  edLinearPowerMaximumA3.Enabled := False;
  edLinearVoltageMinimumA3.Enabled := False;
  edLinearVoltageMaximumA3.Enabled := False;

end;


procedure TPhotoStimSetupFrm.EnablePockelsCellA1;
begin

  edPowerMinimumA1.Enabled := True;
  edPowerMaximumA1.Enabled := True;
  edVoltagePiA1.Enabled := True;
  edBiasSettingA1.Enabled := True;
  rbPolarizationParallelA1.Enabled := True;
  rbPolarizationCrossA1.Enabled := True;

end;


procedure TPhotoStimSetupFrm.EnablePockelsCellA2;
begin

  edPowerMinimumA2.Enabled := True;
  edPowerMaximumA2.Enabled := True;
  edVoltagePiA2.Enabled := True;
  edBiasSettingA2.Enabled := True;
  rbPolarizationParallelA2.Enabled := True;
  rbPolarizationCrossA2.Enabled := True;

end;


procedure TPhotoStimSetupFrm.EnablePockelsCellA3;
begin

  edPowerMinimumA3.Enabled := True;
  edPowerMaximumA3.Enabled := True;
  edVoltagePiA3.Enabled := True;
  edBiasSettingA3.Enabled := True;
  rbPolarizationParallelA3.Enabled := True;
  rbPolarizationCrossA3.Enabled := True;

end;


procedure TPhotoStimSetupFrm.EnableLinearRampA1;
begin

  edLinearPowerMinimumA1.Enabled := True;
  edLinearPowerMaximumA1.Enabled := True;
  edLinearVoltageMinimumA1.Enabled := True;
  edLinearVoltageMaximumA1.Enabled := True;

end;


procedure TPhotoStimSetupFrm.EnableLinearRampA2;
begin

  edLinearPowerMinimumA2.Enabled := True;
  edLinearPowerMaximumA2.Enabled := True;
  edLinearVoltageMinimumA2.Enabled := True;
  edLinearVoltageMaximumA2.Enabled := True;

end;


procedure TPhotoStimSetupFrm.EnableLinearRampA3;
begin

  edLinearPowerMinimumA3.Enabled := True;
  edLinearPowerMaximumA3.Enabled := True;
  edLinearVoltageMinimumA3.Enabled := True;
  edLinearVoltageMaximumA3.Enabled := True;

end;


procedure TPhotoStimSetupFrm.rbPockelsCellA1Click(Sender: TObject);
begin

  // Enable Pockels Cell controls
  EnablePockelsCellA1;

  // Disable Linear Ramp controls
  DisableLinearRampA1;

end;

procedure TPhotoStimSetupFrm.rbPockelsCellA2Click(Sender: TObject);
begin

  // Enable Pockels Cell controls
  EnablePockelsCellA2;

  // Disable Linear Ramp controls
  DisableLinearRampA2;

end;

procedure TPhotoStimSetupFrm.rbPockelsCellA3Click(Sender: TObject);
begin

  // Enable Pockels Cell controls
  EnablePockelsCellA3;

  // Disable Linear Ramp controls
  DisableLinearRampA3;

end;

procedure TPhotoStimSetupFrm.rbLinearRampA1Click(Sender: TObject);
begin

  // Disable Pockels Cell controls
  DisablePockelsCellA1;

  // Enable Linear Ramp controls
  EnableLinearRampA1;

end;

procedure TPhotoStimSetupFrm.rbLinearRampA2Click(Sender: TObject);
begin

  // Disable Pockels Cell controls
  DisablePockelsCellA2;

  // Enable Linear Ramp controls
  EnableLinearRampA2;

end;

procedure TPhotoStimSetupFrm.rbLinearRampA3Click(Sender: TObject);
begin

  // Disable Pockels Cell controls
  DisablePockelsCellA3;

  // Enable Linear Ramp controls
  EnableLinearRampA3;

end;

procedure TPhotoStimSetupFrm.btnBrowseClick(Sender: TObject);
var
  dir : String;
begin

  // Show directory picker
  if SelectDirectory('Select a directory', 'C:\', dir) then
  begin

    // Update directory
    PVFile := dir + '\PrairieView.log';

    // Update label
    lblPath.Caption := PVFile;
    
  end;

end;

end.
