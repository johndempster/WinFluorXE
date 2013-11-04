unit StimulusDefaultsUnit;
// -------------------------------------------------
// Set default DAC and digital stimlus output values
// -------------------------------------------------
// 28.08.06
// 21.12.07 Digital default settungs now limited to digital stimulus channels
// 11.01.08 Default digital output states now held in LabIO.DigOutState[Device]
// 04.10.09 NS .. Added third DAC channel, Vout2

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ExtCtrls;

type
  TStimulusDefaultsFrm = class(TForm)
    DIGGroup: TGroupBox;
    Label4: TLabel;
    Label7: TLabel;
    Dig1: TGroupBox;
    rbOn1: TRadioButton;
    rbOff1: TRadioButton;
    Dig3: TGroupBox;
    rbOn3: TRadioButton;
    rboff3: TRadioButton;
    Dig4: TGroupBox;
    rbon4: TRadioButton;
    rboff4: TRadioButton;
    Dig5: TGroupBox;
    rbon5: TRadioButton;
    rboff5: TRadioButton;
    Dig6: TGroupBox;
    rbon6: TRadioButton;
    rboff6: TRadioButton;
    Dig7: TGroupBox;
    rbon7: TRadioButton;
    rboff7: TRadioButton;
    Dig2: TGroupBox;
    rbOn2: TRadioButton;
    rbOff2: TRadioButton;
    Dig0: TGroupBox;
    rbOn0: TRadioButton;
    rbOff0: TRadioButton;
    bOK: TButton;
    bCancel: TButton;
    DACGroup: TGroupBox;
    VCommand0Panel: TPanel;
    Label1: TLabel;
    edVCommand0: TValidatedEdit;
    VCommand1Panel: TPanel;
    Label2: TLabel;
    edVCommand1: TValidatedEdit;
    VCommand2Panel: TPanel;
    Label3: TLabel;
    edVCommand2: TValidatedEdit;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    procedure SetDigRadioButton(
          RadioGroup : TGroupBox ;
          var RadioButton : TRadioButton ;
          BitNum : Integer  ) ;
    procedure GetDigRadioButton(
              RadioButton : TRadioButton ;
              BitNum : Integer  ) ;


  public
    { Public declarations }
  end;

var
  StimulusDefaultsFrm: TStimulusDefaultsFrm;

implementation

uses Main, LabIOUnit;

{$R *.dfm}

procedure TStimulusDefaultsFrm.FormShow(Sender: TObject);
// ---------------------------------------------
// Initial control settings when form is opened
// ---------------------------------------------
begin

     // DAC Outputs
     VCommand0Panel.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[0]) ;
     edVCommand0.Value := MainFrm.VCommand[0].HoldingVoltage ;

     VCommand1Panel.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[1]) ;
     edVCommand1.Value := MainFrm.VCommand[1].HoldingVoltage ;

     VCommand2Panel.Visible := MainFrm.IOResourceAvailable(MainFrm.IOConfig.VCommand[2]) ;
     edVCommand2.Value := MainFrm.VCommand[2].HoldingVoltage ;

     SetDigRadioButton( Dig0,rbOn0, 0 ) ;
     SetDigRadioButton( Dig1,rbOn1, 1 ) ;
     SetDigRadioButton( Dig2,rbOn2, 2 ) ;
     SetDigRadioButton( Dig3,rbOn3, 3 ) ;
     SetDigRadioButton( Dig4,rbOn4, 4 ) ;
     SetDigRadioButton( Dig5,rbOn5, 5 ) ;
     SetDigRadioButton( Dig6,rbOn6, 6 ) ;
     SetDigRadioButton( Dig7,rbOn7, 7 ) ;

     ClientWidth := DIGGroup.Left + DIGGroup.Width + 5 ;
     ClientHeight := bOK.Top + bOK.Height + 5 ;

     end ;


procedure TStimulusDefaultsFrm.SetDigRadioButton(
          RadioGroup : TGroupBox ;
          var RadioButton : TRadioButton ;
          BitNum : Integer ) ;
// ----------------------
// Set radio button state
// ----------------------
var
    DStartChan,DNumChans,Device : Integer ;
begin
     RadioGroup.Visible := False ;

     // Skip if channel is not configured
     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) then Exit ;
     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then Exit ;

     // Get I/O device properties (skip if not set up)
     Device := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device ;

     // Get starting output bit and number of bits available
     DStartChan := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel ;
     DNumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel
                     -DStartChan + 1 ;

     if BitNum >= DNumChans then Exit ;

     RadioGroup.Visible := True ;

     // Set bit
     if (LabIO.DigOutState[Device] and
         LabIO.BitMask(DStartChan + BitNum)) <> 0 then  RadioButton.Checked := True
                                                   else RadioButton.Checked := False ;

     end ;


procedure TStimulusDefaultsFrm.GetDigRadioButton(
          RadioButton : TRadioButton ;
          BitNum : Integer ) ;
// -------------------------------------------
// Set BitSettings bit to radio button setting
// -------------------------------------------
var
    DStartChan,DNumChans,Device : Integer ;
begin

     // Skip if channel is not configured
     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) then Exit ;
     if not MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimEnd) then Exit ;

     // Get I/O device properties (skip if not set up)
     Device := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device ;

     // Get starting output bit and number of bits available
     DStartChan := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel ;
     DNumChans := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel
                     -DStartChan + 1 ;

      LabIO.DigOutState[Device] := LabIO.DigOutState[Device] and
                                   (not LabIO.BitMask(DStartChan + BitNum)) ;
      if RadioButton.Checked then
         LabIO.DigOutState[Device] := LabIO.DigOutState[Device] or LabIO.BitMask(DStartChan + BitNum) ;

      end ;


procedure TStimulusDefaultsFrm.bOKClick(Sender: TObject);
// -------------------------------------
// Exit and update default settings
// -------------------------------------
var
     Device : Integer ;
     DStart,DEnd,DChan : Integer ;
begin

     MainFrm.VCommand[0].HoldingVoltage := edVCommand0.Value ;
     MainFrm.VCommand[1].HoldingVoltage := edVCommand1.Value ;
     MainFrm.VCommand[2].HoldingVoltage := edVCommand2.Value ;     

     GetDigRadioButton( rbOn0, 0 ) ;
     GetDigRadioButton( rbOn1, 1 ) ;
     GetDigRadioButton( rbOn2, 2 ) ;
     GetDigRadioButton( rbOn3, 3 ) ;
     GetDigRadioButton( rbOn4, 4 ) ;
     GetDigRadioButton( rbOn5, 5 ) ;
     GetDigRadioButton( rbOn6, 6 ) ;
     GetDigRadioButton( rbOn7, 7 ) ;

     // Update digital outputs (if in use)

     if MainFrm.IOResourceAvailable(MainFrm.IOConfig.DigitalStimStart) then begin

        // Start/end bits
        DStart := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel ;
        DEnd := LabIO.Resource[MainFrm.IOConfig.DigitalStimEnd].StartChannel ;

        // Output device
        Device := LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device ;

        if (LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].ResourceType = DACOut) then begin
            // Update DAC output buffer (if a DAC channel is being used for digital output)
            LabIO.StopDAC(Device) ;
            for DChan := DStart to DEnd do begin
                if DChan >= LabIO.NumDACs[Device] then Break ;
               // Calculate DAC output level value (5V or 0V)
               if (LabIO.DigOutState[Device] and LabIO.BitMask(DChan)) <> 0 then
                  LabIO.DACOutState[Device,DChan] := 5.0
               else LabIO.DACOutState[Device,DChan] := 0.0 ;
               LabIO.WriteDAC( Device, LabIO.DACOutState[Device,DChan], DChan );
               end ;
            end
        else begin
            // Stop digital waveform output on device
            LabIO.StopDIG(Device) ;
            LabIO.WriteToDigitalOutPutPort( Device, LabIO.DigOutState[Device] ) ;
            end ;
        end ;

    Close ;
    end;


procedure TStimulusDefaultsFrm.bCancelClick(Sender: TObject);
// -------------------------------------
// Exit without updating master settings
// -------------------------------------
begin
    Close ;
    end;


procedure TStimulusDefaultsFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// -----------------------
// Close and destroy form
// -----------------------
begin
     Action := caFree ;
     end;

end.
