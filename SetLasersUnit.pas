unit SetLasersUnit;
// ---------------------------------------------------------
// Set intensity of lasers (Optoscan + Lasers light source
// ---------------------------------------------------------
// 10.03.09 Laser intensity can now be set from live image window
// 31.5.13 JD Only slider from available laser now visible
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ExtCtrls;

type
  TSetLasersFrm = class(TForm)
    LasersGrp: TGroupBox;
    bOK: TButton;
    bCancel: TButton;
    panLaser1: TPanel;
    Label31: TLabel;
    sbLaser1: TScrollBar;
    edLaser1Intensity: TValidatedEdit;
    panLaser3: TPanel;
    Label1: TLabel;
    sbLaser2: TScrollBar;
    edLaser2Intensity: TValidatedEdit;
    panLaser2: TPanel;
    Label2: TLabel;
    sbLaser3: TScrollBar;
    edLaser3Intensity: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure sbLaser1Change(Sender: TObject);
    procedure sbLaser2Change(Sender: TObject);
    procedure sbLaser3Change(Sender: TObject);
    procedure edLaser1IntensityKeyPress(Sender: TObject; var Key: Char);
    procedure edLaser2IntensityKeyPress(Sender: TObject; var Key: Char);
    procedure edLaser3IntensityKeyPress(Sender: TObject; var Key: Char);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SetLasersFrm: TSetLasersFrm;

implementation

uses LightSourceUnit, Main, RecUnit, SnapUnit;

{$R *.dfm}

procedure TSetLasersFrm.FormShow(Sender: TObject);
// ---------------------------------------
// Initialise controls when form displayed
// ---------------------------------------
begin

     panLaser1.Visible :=  LightSource.LaserAvailable[1] ;
     panLaser2.Visible :=  LightSource.LaserAvailable[2] ;
     panLaser3.Visible :=  LightSource.LaserAvailable[3] ;

     sbLaser1.Position := Round(LightSource.LaserIntensity[1]) ;
     edLaser1Intensity.Value := sbLaser1.Position ;
     sbLaser2.Position := Round(LightSource.LaserIntensity[2]) ;
     edLaser2Intensity.Value := sbLaser2.Position ;
     sbLaser3.Position := Round(LightSource.LaserIntensity[3]) ;
     edLaser3Intensity.Value := sbLaser3.Position ;

     ClientWidth := LasersGrp.Left + LasersGrp.Width + 5 ;
     ClientHeight := bOK.Top + bOK.Height + 5 ;

     end;

procedure TSetLasersFrm.bOKClick(Sender: TObject);
// -----------------
// OK button pressed
// -----------------
begin

     LightSource.LaserIntensity[1] := edLaser1Intensity.Value ;
     LightSource.LaserIntensity[2] := edLaser2Intensity.Value ;
     LightSource.LaserIntensity[3] := edLaser3Intensity.Value ;

     if Mainfrm.FormExists('RecordFrm') then begin
        // Restart recording
        if RecordFrm.CameraRunning then begin
           RecordFrm.StopCamera ;
           RecordFrm.StartCamera ;
           end ;
        end ;

     if Mainfrm.FormExists('SnapFrm') then begin
        // Restart recording
        if SnapFrm.CameraRunning then begin
           SnapFrm.StopCamera ;
           SnapFrm.StartCamera ;
           end ;
        end ;

     Hide ;

     end;

     
procedure TSetLasersFrm.sbLaser1Change(Sender: TObject);
begin
     edLaser1Intensity.Value := sbLaser1.Position ;
     end;

procedure TSetLasersFrm.sbLaser2Change(Sender: TObject);
begin
     edLaser2Intensity.Value := sbLaser2.Position ;
     end;


procedure TSetLasersFrm.sbLaser3Change(Sender: TObject);
begin
     edLaser3Intensity.Value := sbLaser3.Position ;
     end;


procedure TSetLasersFrm.edLaser1IntensityKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then begin
        sbLaser1.Position := Round(edLaser1Intensity.Value) ;
        end ;
     end;

procedure TSetLasersFrm.edLaser2IntensityKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then begin
        sbLaser2.Position := Round(edLaser2Intensity.Value) ;
        end ;
     end;


procedure TSetLasersFrm.edLaser3IntensityKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then begin
        sbLaser3.Position := Round(edLaser3Intensity.Value) ;
        end ;
     end;


procedure TSetLasersFrm.bCancelClick(Sender: TObject);
// ---------------------
// Cancel button pressed
// ---------------------
begin
     Hide ;
     end;

end.
