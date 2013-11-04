unit DigDiagnosticsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TDigDiagnosticsFrm = class(TForm)
    DIGGroup: TGroupBox;
    Label4: TLabel;
    Label7: TLabel;
    GroupBox5: TGroupBox;
    rbOn1: TRadioButton;
    rbOff1: TRadioButton;
    GroupBox6: TGroupBox;
    rbOn3: TRadioButton;
    rboff3: TRadioButton;
    GroupBox7: TGroupBox;
    rbon4: TRadioButton;
    rboff4: TRadioButton;
    GroupBox8: TGroupBox;
    rbon5: TRadioButton;
    rboff5: TRadioButton;
    GroupBox9: TGroupBox;
    rbon6: TRadioButton;
    rboff6: TRadioButton;
    GroupBox10: TGroupBox;
    rbon7: TRadioButton;
    rboff7: TRadioButton;
    GroupBox11: TGroupBox;
    rbOn2: TRadioButton;
    rbOff2: TRadioButton;
    Dig0: TGroupBox;
    rbOn0: TRadioButton;
    rbOff0: TRadioButton;
    GroupBox12: TGroupBox;
    rbon8: TRadioButton;
    rboff8: TRadioButton;
    GroupBox13: TGroupBox;
    rbon9: TRadioButton;
    rboff9: TRadioButton;
    bSetDigitalOut: TButton;
    bOK: TButton;
    bCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bSetDigitalOutClick(Sender: TObject);
  private
    { Private declarations }
    BitSettings : Word ;    
  public
    { Public declarations }
  end;

var
  DigDiagnosticsFrm: TDigDiagnosticsFrm;

implementation

uses Main, LabIOUnit;

{$R *.dfm}

procedure TDigDiagnosticsFrm.FormShow(Sender: TObject);
// ---------------------------------------------
// Initial control settings when form is opened
// ---------------------------------------------
var
     i,ch :Integer ;

     Bit : Word ;
begin

     MainFrm.mnDigDiagnostics.Enabled := False ;

     // Close any form which might be acquiring images
     for i := 0 to Mainfrm.MDIChildCount-1 do begin
         if MainFrm.MDIChildren[i].Name = 'IntegrateFrm' then
            MainFrm.MDIChildren[i].Close ;
         if MainFrm.MDIChildren[i].Name = 'RecordFrm' then
            MainFrm.MDIChildren[i].Close ;
         end ;

     Bit := BitSettings and 1 ;
     if Bit <> 0 then rbOn0.checked := true
                 else rbOff0.checked := true ;
     Bit := BitSettings and 2 ;
     if Bit <> 0 then rbOn1.checked := true
                 else rbOff1.checked := true  ;
     Bit := BitSettings and 4 ;
     if Bit <> 0 then rbOn2.checked := true
                 else rbOff2.checked := true  ;
     Bit := BitSettings and 8 ;
     if Bit <> 0 then rbOn3.checked := true
                 else rbOff3.checked := true  ;
     Bit := BitSettings and 16 ;
     if Bit <> 0 then rbOn4.checked := true
                 else rbOff4.checked := true  ;
     Bit := BitSettings and 32 ;
     if Bit <> 0 then rbOn5.checked := true
                 else rbOff5.checked := true  ;
     Bit := BitSettings and 64 ;
     if Bit <> 0 then rbOn6.checked := true
                 else rbOff6.checked := true  ;
     Bit := BitSettings and 128 ;
     if Bit <> 0 then rbOn7.checked := true
                 else rbOff7.checked := true  ;
     Bit := BitSettings and 256 ;
     if Bit <> 0 then rbOn8.checked := true
                 else rbOff8.checked := true  ;
     Bit := BitSettings and 512 ;
     if Bit <> 0 then rbOn9.checked := true
                 else rbOff9.checked := true  ;


     end ;




procedure TDigDiagnosticsFrm.bOKClick(Sender: TObject);
// -------------------------------------
// Exit without updating master settings
// -------------------------------------
begin
    Close ;
    end;

procedure TDigDiagnosticsFrm.bCancelClick(Sender: TObject);
// -------------------------------------
// Exit without updating master settings
// -------------------------------------
begin
    Close ;
    end;

procedure TDigDiagnosticsFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// -----------------------
// Close and destroy form
// -----------------------
begin
     MainFrm.mnDigDiagnostics.Enabled := True ;
     Action := caFree ;
     end;

procedure TDigDiagnosticsFrm.bSetDigitalOutClick(Sender: TObject);
// --------------------
// Update digital lines
// --------------------
var
     Value : Word ;
begin
     Value := 0 ;
     if rbOn0.checked then Value := Value or 1 ;
     if rbOn1.checked then Value := Value or 2 ;
     if rbOn2.checked then Value := Value or 4 ;
     if rbOn3.checked then Value := Value or 8 ;
     if rbOn4.checked then Value := Value or 16 ;
     if rbOn5.checked then Value := Value or 32 ;
     if rbOn6.checked then Value := Value or 64 ;
     if rbOn7.checked then Value := Value or 128 ;
     if rbOn8.checked then Value := Value or 256 ;
     if rbOn9.checked then Value := Value or 512 ;
     LabIO.SetDigitalOutputs(Value) ;
     end;


end.
