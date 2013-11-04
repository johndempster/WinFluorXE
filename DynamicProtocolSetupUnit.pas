unit DynamicProtocolSetupUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, ExtCtrls, Math, StrUtils;

type
  TDynamicProtocolSetupFrm = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    cbChannel: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    cbDirection: TComboBox;
    edThreshold: TValidatedEdit;
    Label3: TLabel;
    edDuration: TValidatedEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    rbEPRestart: TRadioButton;
    rbEPStop: TRadioButton;
    Panel2: TPanel;
    rbPSRestart: TRadioButton;
    rbPSStop: TRadioButton;
    cbStimProgram: TComboBox;
    cbPhotoStimProgram: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCancelClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbPSRestartClick(Sender: TObject);
    procedure rbEPRestartClick(Sender: TObject);    
    procedure rbEPStopClick(Sender: TObject);
    procedure rbPSStopClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateStimProgramList;
  end;

var
  DynamicProtocolSetupFrm: TDynamicProtocolSetupFrm;

implementation

uses Main, StimModule, PhotoStimModule;

{$R *.dfm}

procedure TDynamicProtocolSetupFrm.FormShow(Sender: TObject);
var
     i : Integer;  // Loop index
begin

     // Add channels to channel selection box
     cbChannel.Clear;
     for i := 0 to MainFrm.ADCNumChannels-1 do begin
        cbChannel.Items.Add(MainFrm.ADCChannel[i].ADCName);
     end;
     cbChannel.ItemIndex := MainFrm.DynamicProtocol.SelectedChannel;

     // Add options to direction combo box
     cbDirection.Clear;
     cbDirection.Items.Add('Above');
     cbDirection.Items.Add('Below');
     cbDirection.ItemIndex := MainFrm.DynamicProtocol.Direction;

     // Load threshold
     edThreshold.Value := MainFrm.DynamicProtocol.Threshold;

     // Load duration
     edDuration.Value := MainFrm.DynamicProtocol.Duration;

     // Load radio button values
     rbEPRestart.Checked := MainFrm.DynamicProtocol.EPRestart;
     rbPSRestart.Checked := MainFrm.DynamicProtocol.PSRestart;

     // Load list of electro-physiology stimulus protocols
     UpdateStimProgramList;

     // Enable/disable stimulus combo box based on state of start/stop buttons
     cbStimProgram.Enabled := MainFrm.DynamicProtocol.EPRestart;
     cbPhotoStimProgram.Enabled := MainFrm.DynamicProtocol.PSRestart;

end;

procedure TDynamicProtocolSetupFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     Action := caFree ;
end;

procedure TDynamicProtocolSetupFrm.bOKClick(Sender: TObject);
begin

    // Save form parameters
    MainFrm.DynamicProtocol.SelectedChannel := cbChannel.ItemIndex ;
    MainFrm.DynamicProtocol.Direction := cbDirection.ItemIndex ;
    MainFrm.DynamicProtocol.Threshold := edThreshold.Value ;
    MainFrm.DynamicProtocol.Duration := edDuration.Value ;
    MainFrm.DynamicProtocol.EPRestart := rbEPRestart.Checked ;
    MainFrm.DynamicProtocol.PSRestart := rbPSRestart.Checked ;
    MainFrm.DynamicProtocol.EPStimFileName :=
      Mainfrm.VProtDirectory + cbStimProgram.Text + '.vpr';
    MainFrm.DynamicProtocol.EPStimIndex := cbStimProgram.ItemIndex;
    MainFrm.DynamicProtocol.PSStimFileName :=
      Mainfrm.PProtDirectory + cbPhotoStimProgram.Text + '.ppr';
    MainFrm.DynamicProtocol.PSStimIndex := cbPhotoStimProgram.ItemIndex;

    // Close window
    Close;

end;

procedure TDynamicProtocolSetupFrm.bCancelClick(Sender: TObject);
begin

    // Close window without saving changes
    Close;

end;

procedure TDynamicProtocolSetupFrm.UpdateStimProgramList ;
var
    NamePart : String ;
begin

    // Load list of stimulus programs
    Stimulator.CreateProgramList(cbStimProgram);
    if MainFrm.DynamicProtocol.EPStimFileName <> '' then begin
      // Find name within list
      NamePart :=
        AnsiReplaceText(ExtractFileName(MainFrm.DynamicProtocol.EPStimFileName),
                                        '.vpr',
                                        '');
      cbStimProgram.ItemIndex := Min(Max(cbStimProgram.Items.IndexOf(NamePart),
                                     0), cbStimProgram.Items.Count-1);
    end
    else
    begin
      cbStimProgram.ItemIndex := 0;
    end ;

    // Load list of photo-stimulus programs
    PhotoStimulator.CreateProgramList(cbPhotoStimProgram);
    if MainFrm.DynamicProtocol.PSStimFileName <> '' then begin
      // Find name within list
      NamePart :=
        AnsiReplaceText(ExtractFileName(MainFrm.DynamicProtocol.PSStimFileName),
                                        '.ppr',
                                        '');
      cbPhotoStimProgram.ItemIndex := Min(Max(cbPhotoStimProgram.Items.IndexOf(NamePart),
                                          0), cbPhotoStimProgram.Items.Count-1);
    end
    else
    begin
      cbPhotoStimProgram.ItemIndex := 0;
    end ;

end ;

procedure TDynamicProtocolSetupFrm.rbEPRestartClick(Sender: TObject);
begin
    cbStimProgram.Enabled := True;
end;

procedure TDynamicProtocolSetupFrm.rbEPStopClick(Sender: TObject);
begin
    cbStimProgram.Enabled := False;
end;

procedure TDynamicProtocolSetupFrm.rbPSRestartClick(Sender: TObject);
begin
    cbPhotoStimProgram.Enabled := True;
end;

procedure TDynamicProtocolSetupFrm.rbPSStopClick(Sender: TObject);
begin
    cbPhotoStimProgram.Enabled := False;
end;

end.
