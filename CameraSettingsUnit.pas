unit CameraSettingsUnit;

// --------------------------
// Additional Camera Settings
// --------------------------
// 20-5-9
// 16.08

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, math;

type
  TCameraSettingsFrm = class(TForm)
    CCDCoolingGrp: TGroupBox;
    Label41: TLabel;
    edTemperatureSetPoint: TValidatedEdit;
    ckCameraCooling: TCheckBox;
    bOK: TButton;
    bCancel: TButton;
    cbFanMode: TComboBox;
    Label1: TLabel;
    SpecialGrp: TGroupBox;
    cbADCGain: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    cbCCDVerticalShiftSpeed: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CameraSettingsFrm: TCameraSettingsFrm;

implementation

uses Main, RecUnit, SnapUnit;

{$R *.dfm}

procedure TCameraSettingsFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// ---------------------------
// Tidy up when form is closed
// ---------------------------
begin
     Action := caFree ;
     end;

procedure TCameraSettingsFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
begin

    // CCD cooling temperature set point
    edTemperatureSetPoint.Value := MainFrm.Cam1.CameraTemperatureSetPoint ;
    // Camera Peltier cooling
    ckCameraCooling.Checked := MainFrm.Cam1.CameraCoolingOn ;

    // Camera fan
    cbFanMode.Clear ;
    cbFanMode.Items.Add('Off') ;
    cbFanMode.Items.Add('Low') ;
    cbFanMode.Items.Add('High') ;
    cbFanMode.ItemIndex := Min(Max(MainFrm.Cam1.CameraFanMode,0),cbFanMode.Items.Count-1) ;

    MainFrm.Cam1.GetADCGainList( cbADCGain.Items ) ;
    cbADCGain.ItemIndex := Min(Max(MainFrm.Cam1.ADCGain,0),cbADCGain.Items.Count-1) ;

    MainFrm.Cam1.GetCCDVerticalShiftSpeedList(cbCCDVerticalShiftSpeed.Items );
    cbCCDVerticalShiftSpeed.ItemIndex := Min(Max(MainFrm.Cam1.CCDVerticalShiftSpeed,0),
                                         cbCCDVerticalShiftSpeed.Items.Count-1) ;

    ClientWidth := SpecialGrp.Left + SpecialGrp.Width + 5 ;
    ClientHeight := bOK.Top + bOK.Height + 5 ;

    end;

procedure TCameraSettingsFrm.bOKClick(Sender: TObject);
// ----------------------
// Update camera settings
// ----------------------
begin

    // Update camera settings
    MainFrm.Cam1.CameraTemperatureSetPoint := edTemperatureSetPoint.Value ;
    MainFrm.Cam1.CameraCoolingOn := ckCameraCooling.Checked ;
    MainFrm.Cam1.CameraFanMode := cbFanMode.ItemIndex ;
    MainFrm.Cam1.ADCGain := cbADCGain.ItemIndex ;
    MainFrm.Cam1.CCDVerticalShiftSpeed := cbCCDVerticalShiftSpeed.ItemIndex ;

    // Request a timing cycle change
    if MainFrm.FormExists( 'RecordFrm') then begin
       if RecordFrm.CameraRunning then RecordFrm.RestartCamera ;
       end ;
    if MainFrm.FormExists( 'SnapFrm') then begin
       if SnapFrm.CameraRunning then SnapFrm.RestartCamera ;
       end ;

    close ;

    end;

procedure TCameraSettingsFrm.bCancelClick(Sender: TObject);
begin
  Close;
end;

end.

