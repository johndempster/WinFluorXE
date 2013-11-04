unit SetCCDReadoutUnit;
// ---------------------------------------
// Enter coordinates of CCD readout region
// ---------------------------------------
// 20.02.07 Limits of range now set correctly
// 22.05.13 Cancel now closes form rather than hides (avoiding form error)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, RangeEdit;

type
  TSetCCDReadoutFrm = class(TForm)
    AreaGrp: TGroupBox;
    edXRange: TRangeEdit;
    Label1: TLabel;
    Label2: TLabel;
    edYRange: TRangeEdit;
    bOK: TButton;
    bCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    CalledBy : String ;
  end;

var
  SetCCDReadoutFrm: TSetCCDReadoutFrm;

implementation

uses Main, RecUnit, SnapUnit;

{$R *.dfm}

procedure TSetCCDReadoutFrm.FormShow(Sender: TObject);
// --------------------------------------
// Initialisations when form is displayed
// --------------------------------------
begin

     edXRange.HiLimit := (MainFrm.Cam1.FrameWidthMax)-1 ;
     edYRange.HiLimit := (MainFrm.Cam1.FrameHeightMax)-1 ;

     edXRange.LoValue := MainFrm.Cam1.FrameLeft ;
     edXRange.HiValue := MainFrm.Cam1.FrameRight ;
     edYRange.LoValue := MainFrm.Cam1.FrameTop ;
     edYRange.HiValue := MainFrm.Cam1.FrameBottom ;

     ClientWidth := AreaGrp.Left + AreaGrp.Width + 5 ;
     ClientHeight := bOK.Top + bOK.Height + 5 ;

     end;

procedure TSetCCDReadoutFrm.bOKClick(Sender: TObject);
// -----------------
// OK button pressed
// -----------------
begin

    if (CalledBy = 'RecordFrm') and MainFrm.FormExists(CalledBy) then begin
        RecordFrm.StopCamera ;
        MainFrm.Cam1.SetCCDArea( Round(edXRange.LoValue),
                                 Round(edYRange.LoValue),
                                 Round(edXRange.HiValue),
                                 Round(edYRange.HiValue));
        RecordFrm.StartCamera ;
        end
    else if (CalledBy = 'SnapFrm') and MainFrm.FormExists(CalledBy) then begin
        SnapFrm.StopCamera ;
        MainFrm.Cam1.SetCCDArea( Round(edXRange.LoValue),
                              Round(edYRange.LoValue),
                              Round(edXRange.HiValue),
                              Round(edYRange.HiValue));
        SnapFrm.StartCamera ;
        end ;

     Close ;

     end;

procedure TSetCCDReadoutFrm.bCancelClick(Sender: TObject);
// ---------------------
// Cancel button pressed
// ---------------------
begin
     Close
     end;

procedure TSetCCDReadoutFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     Action := caFree ;
     end;

end.
