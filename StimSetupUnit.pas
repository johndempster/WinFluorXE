unit StimSetupUnit;
// -------------------------------
// Stimulus pulse setup dialog box
// -------------------------------
// 11/4/02
// 30.8.02 Stimulus repeat period and delay now in seconds.
// 26.9.03 Voltage pulse stimulus added
// 29.01.14 Updated to Compile under both 32/64 bits (File handle now THandle)

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ValEdit, FileCtrl, ValidatedEdit;

const
  NumStimulusPulses = 4 ;
  TextBufSize = 1024 ;
  FileExtension = 'STM' ;
  StimSubFolder = 'stim\' ;

type

  TStimulus = record
         InUse : Boolean ;
         Delay : Single ;
         Width : Single ;
         NumPulses : Integer ;
         Interval : Single ;
         ActiveHigh : Boolean ;
         Bit : Word ;
         end ;


  TStimProgram = record
        VDelay : Single ;
        VPrePulseWidth : Single ;
        VPrePulseAmplitude : Single ;
        VPrimaryPulseWidthIncrement : Single ;
        VPrimaryPulseAmplitudeIncrement : Single ;
        VPrimaryPulseWidth : Single ;
        VPrimaryPulseAmplitude : Single ;
        VNumPulses : Integer ;
        VPulseInterval : Single ;
        Pulses : Array[0..NumStimulusPulses-1] of TStimulus ;
        Period : Single ;
        SingleStimulus : Boolean ;
        Duration : Single ;
        end ;

  TStimSetupFrm = class(TForm)
    DigitalPulseGrp: TGroupBox;
    Pulse1Grp: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbInterval1: TLabel;
    edDelay1: TValidatedEdit;
    edWidth1: TValidatedEdit;
    edNumPulses1: TValidatedEdit;
    edInterval1: TValidatedEdit;
    ckUsePulse1: TCheckBox;
    bOK: TButton;
    bCancel: TButton;
    GroupBox7: TGroupBox;
    rbRepeat: TRadioButton;
    rbSingle: TRadioButton;
    edRepeatPeriod: TValidatedEdit;
    GroupBox8: TGroupBox;
    edFileName: TEdit;
    bLoadFile: TButton;
    bSaveFile: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    VoltagePulseGrp: TGroupBox;
    GroupBox9: TGroupBox;
    Label4: TLabel;
    edPrePulseAmplitude: TValidatedEdit;
    Label8: TLabel;
    edPrePulseWidth: TValidatedEdit;
    GroupBox10: TGroupBox;
    Label12: TLabel;
    Label16: TLabel;
    edPrimaryPulseAmplitude: TValidatedEdit;
    edPrimaryPulseWidth: TValidatedEdit;
    Label17: TLabel;
    edPrimaryPulseAmplitudeIncrement: TValidatedEdit;
    Label18: TLabel;
    edPrimaryPulseWidthIncrement: TValidatedEdit;
    GroupBox11: TGroupBox;
    Label19: TLabel;
    Label20: TLabel;
    edNumPulses: TValidatedEdit;
    edPulseInterval: TValidatedEdit;
    Label21: TLabel;
    Label22: TLabel;
    edVDelay: TValidatedEdit;
    GroupBox12: TGroupBox;
    rbActiveHigh1: TRadioButton;
    rbActiveLow1: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCancelClick(Sender: TObject);
    procedure ckUsePulse1Click(Sender: TObject);
    procedure ckUsePulse2Click(Sender: TObject);
    procedure bLoadFileClick(Sender: TObject);
    procedure bSaveFileClick(Sender: TObject);
    procedure edNumPulses1KeyPress(Sender: TObject; var Key: Char);
    procedure edPrePulseAmplitudeKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    FileName : String ;
    StimProg : TStimProgram ;


    procedure StimProgToEditBoxes ;
    procedure InitPulseSettings(
          PulseNum : Integer ;
          ckUsePulse : TCheckBox ;
          edDelay : TValidatedEdit ;
          edWidth : TValidatedEdit ;
          edNumPulses : TValidatedEdit ;
          lbInterval : TLabel ;
          edInterval : TValidatedEdit ;
          rbActiveHigh : TRadioButton ;
          rbActiveLow  : TRadioButton ) ;

    procedure EditBoxesToStimProg ;
    procedure ReadPulseSettings(
          PulseNum : Integer ;
          ckUsePulse : TCheckBox ;
          edDelay : TValidatedEdit ;
          edWidth : TValidatedEdit ;
          edNumPulses : TValidatedEdit ;
          lbInterval : TLabel ;          
          edInterval : TValidatedEdit ;
          rbActiveHigh : TRadioButton ) ;

  public
    { Public declarations }


    procedure LoadStimulusProgram(
          var StimProg : TStimProgram ;
          FileName : String ) ;
    procedure SaveStimulusProgram(
          var StimProg : TStimProgram ;
          FileName : String ) ;

    procedure CreateProgramList(
              var cbList : TComboBox
              ) ;

    function StimulusProgramDuration(
             StimProgram : TStimProgram
             ) : Single ;

  end;

var
  StimSetupFrm: TStimSetupFrm;

implementation

uses Main, SysUtils, shared, winprocs, maths, RecADCOnlyUnit ,
     LabIOUnit, RecUnit ;

{$R *.DFM}

procedure TStimSetupFrm.FormShow(Sender: TObject);
// --------------------------------------------
// Initialisations when form is first displayed
// --------------------------------------------
var
    StimDirectory : String ;
    Path : string ;
begin

     ClientHeight := bOK.Top + bOK.Height + 5 ;
     ClientWidth := VoltagePulseGrp.Left + VoltagePulseGrp.Width + 5 ;

     // Stimulus files held in \stim sub-folder within program folder
     StimDirectory := MainFrm.ProgramDirectory + StimSubFolder ;
     if not DirectoryExists(StimDirectory) then CreateDir(StimDirectory) ;
     OpenDialog.InitialDir :=  StimDirectory ;
     SaveDialog.InitialDir :=  StimDirectory ;

     // Stimulus files held in \stim sub-folder within program folder
     Path := StimDirectory + MainFrm.StimFileName + '.' + FileExtension ;

     if FileExists(Path) then begin
        // Get name of current stimulus program
        FileName := MainFrm.StimFileName ;

        // Load current stimulus progam
        LoadStimulusProgram( StimProg, FileName ) ;

        end
     else FileName := '' ;

     // Command voltage channel
     if (MainFrm.IOConfig.VCommand[1] >= 0) and
        (MainFrm.IOConfig.VCommand[1] < MaxResources) then begin
        VoltagePulseGrp.Caption := format(' Command Voltage Waveform (Device %d, DAC%d)',
                                   [LabIO.Resource[MainFrm.IOConfig.VCommand[1]].Device,
                                    LabIO.Resource[MainFrm.IOConfig.VCommand[1]].StartChannel]) ;
        end
     else VoltagePulseGrp.Caption := ' Command Voltage Waveform (Not Available)' ;

     // Command voltage channel
     if (MainFrm.IOConfig.DigitalStimStart >= 0) and
        (MainFrm.IOConfig.DigitalStimStart < MaxResources) then begin
        DigitalPulseGrp.Caption := format(' Digital Pulse Waveform(Device %d, DAC%d)',
                                   [LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].Device,
                                    LabIO.Resource[MainFrm.IOConfig.DigitalStimStart].StartChannel]) ;
        end
     else DigitalPulseGrp.Caption := ' Digital Pulse Waveform (Not Available)' ;

     // Update edit boxes on form
     StimProgToEditBoxes ;

     // Update file name
     edFileName.Text := FileName ;


     end;


procedure TStimSetupFrm.StimProgToEditBoxes ;
// ------------------------------------------
// Write stimulus program to form entry boxes
// ------------------------------------------
//
begin

     // Voltage pulse
     edVDelay.Value := StimProg.VDelay ;
     edPrePulseWidth.Value := StimProg.VPrePulseWidth ;
     edPrePulseAmplitude.Value := StimProg.VPrePulseAmplitude ;
     edPrimaryPulseWidth.Value := StimProg.VPrimaryPulseWidth ;
     edPrimaryPulseAmplitude.Value := StimProg.VPrimaryPulseAmplitude ;
     edPrimaryPulseWidthIncrement.Value := StimProg.VPrimaryPulseWidthIncrement;
     edPrimaryPulseAmplitudeIncrement.Value := StimProg.VPrimaryPulseAmplitudeIncrement ;
     edNumPulses.Value := StimProg.VNumPulses ;
     edPulseInterval.Value := StimProg.VPulseInterval ;

     // Digital pulses

     InitPulseSettings( 1,
                        ckUsePulse1,
                        edDelay1,
                        edWidth1,
                        edNumPulses1,
                        lbInterval1,
                        edInterval1,
                        rbActiveHigh1,
                        rbActiveLow1 ) ;

     edRepeatPeriod.Value := StimProg.Period  ;

     rbSingle.Checked := StimProg.SingleStimulus ;
     rbRepeat.Checked := not StimProg.SingleStimulus ;

     end ;


procedure TStimSetupFrm.InitPulseSettings(
          PulseNum : Integer ;
          ckUsePulse : TCheckBox ;
          edDelay : TValidatedEdit ;
          edWidth : TValidatedEdit ;
          edNumPulses : TValidatedEdit ;
          lbInterval : TLabel ;
          edInterval : TValidatedEdit ;
          rbActiveHigh : TRadioButton ;
          rbActiveLow : TRadioButton) ;
// --------------------------------------
// Initialise controls for stimulus pulse
// --------------------------------------
begin
     // Stimulus pulse 1
     ckUsePulse.Checked := StimProg.Pulses[PulseNum-1].InUse ;
     edDelay.Value := StimProg.Pulses[PulseNum-1].Delay ;
     edWidth.Value := StimProg.Pulses[PulseNum-1].Width ;
     edNumPulses.Value := StimProg.Pulses[PulseNum-1].NumPulses ;
     edInterval.Value := StimProg.Pulses[PulseNum-1].Interval ;
     rbActiveHigh.Checked := StimProg.Pulses[PulseNum-1].ActiveHigh ;
     rbActiveLow.Checked := not StimProg.Pulses[PulseNum-1].ActiveHigh ;

     if edNumPulses.Value > 1 then begin
        edInterval.Visible := True ;
        lbInterval.Visible := True ;
        end
     else begin
        edInterval.Visible := False ;
        lbInterval.Visible := False ;
        end ;

     end ;


procedure TStimSetupFrm.EditBoxesToStimProg ;
// ------------------------------------------
// Write stimulus program to form entry boxes
// ------------------------------------------
var
     P : Integer ;
     PulseDuration : Single ;
begin

     // Voltage pulse
     StimProg.VDelay := edVDelay.Value ;
     StimProg.VPrePulseWidth := edPrePulseWidth.Value ;
     StimProg.VPrePulseAmplitude := edPrePulseAmplitude.Value ;
     StimProg.VPrimaryPulseWidth := edPrimaryPulseWidth.Value ;
     StimProg.VPrimaryPulseAmplitude := edPrimaryPulseAmplitude.Value ;
     StimProg.VPrimaryPulseWidthIncrement := edPrimaryPulseWidthIncrement.Value ;
     StimProg.VPrimaryPulseAmplitudeIncrement := edPrimaryPulseAmplitudeIncrement.Value ;
     StimProg.VNumPulses := Round(edNumPulses.Value) ;
     StimProg.VPulseInterval := edPulseInterval.Value ;

     // Ensure pulse interval > pulse widths
     PulseDuration := StimProg.VPrePulseWidth +
                      StimProg.VPrimaryPulseWidth +
                      (StimProg.VNumPulses-1)*StimProg.VPrimaryPulseWidthIncrement ;
     if StimProg.VPulseInterval < PulseDuration then begin
        StimProg.VPulseInterval := PulseDuration ;
        edPulseInterval.Value := StimProg.VPulseInterval ;
        end ;

     // Determine total duration of voltage stimuli
     StimProg.Duration := StimProg.VDelay +
                          (StimProg.VPulseInterval*StimProg.VNumPulses) ;

     ReadPulseSettings( 1,
                        ckUsePulse1,
                        edDelay1,
                        edWidth1,
                        edNumPulses1,
                        lbInterval1,
                        edInterval1,
                        rbActiveHigh1 ) ;

     // Determine duration of digital pulse stimuli

     for P := 0 to High(StimProg.Pulses) do
         if StimProg.Pulses[P].InUse then begin

            // Ensure inter-pulse interval >= pulse width
            if StimProg.Pulses[P].Interval < StimProg.Pulses[P].Width then
               StimProg.Pulses[P].Interval := StimProg.Pulses[P].Width ;

            StimProg.Duration := MaxFlt( [ StimProg.Duration,
                                 StimProg.Pulses[P].Delay +
                                 (StimProg.Pulses[P].NumPulses)*StimProg.Pulses[P].Interval
                                 ]) ;
            end ;

     StimProg.Period := edRepeatPeriod.Value ;
     if StimProg.Period < StimProg.Duration then StimProg.Period := StimProg.Duration ;
     edRepeatPeriod.Value := StimProg.Period ;

     StimProg.SingleStimulus := rbSingle.Checked ;

     end ;


procedure TStimSetupFrm.ReadPulseSettings(
          PulseNum : Integer ;
          ckUsePulse : TCheckBox ;
          edDelay : TValidatedEdit ;
          edWidth : TValidatedEdit ;
          edNumPulses : TValidatedEdit ;
          lbInterval : TLabel ;
          edInterval : TValidatedEdit ;
          rbActiveHigh : TRadioButton ) ;
// ------------------------------------------
// Read stimulus pulse settings from controls
// ------------------------------------------
begin

     StimProg.Pulses[PulseNum-1].InUse := ckUsePulse.Checked ;
     StimProg.Pulses[PulseNum-1].Delay := edDelay.Value  ;
     StimProg.Pulses[PulseNum-1].Width  := edWidth.Value ;
     StimProg.Pulses[PulseNum-1].NumPulses := Round(edNumPulses.Value) ;
     StimProg.Pulses[PulseNum-1].Interval := edInterval.Value ;
     StimProg.Pulses[PulseNum-1].ActiveHigh := rbActiveHigh.Checked ;

     if edNumPulses.Value > 1 then begin
        edInterval.Visible := True ;
        lbInterval.Visible := True ;
        end
     else begin
        edInterval.Visible := False ;
        lbInterval.Visible := False ;
        end ;

     end ;



procedure TStimSetupFrm.bOKClick(Sender: TObject);
// --------------------------------------------
// User has pressed the OK button to close form
// --------------------------------------------
begin

     // Save changes
     SaveStimulusProgram( StimProg, FileName ) ;

     // Update name of stimulus file
     MainFrm.StimFileName := FileName ;

     Close ;

     end;


procedure TStimSetupFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// -----------------------------
// Operations when form is closed
// ------------------------------
var
    i : Integer ;
begin

     // Update any forms which make use of stimulus programs
     for i := 0 to MainFrm.MDIChildCount-1 do begin
         if (MainFrm.MDIChildren[i].Name = 'RecordFrm') then
            TRecordFrm(MainFrm.MDIChildren[i]).UpdateStimProgramList
         else if (MainFrm.MDIChildren[i].Name = 'RecADCOnlyFrm') then
            TRecADCOnlyFrm(MainFrm.MDIChildren[i]).UpdateStimProgramList ;
         end ;

    Action := caFree ;
    
    end;

procedure TStimSetupFrm.bCancelClick(Sender: TObject);
begin
     Close ;
     end;

procedure TStimSetupFrm.ckUsePulse1Click(Sender: TObject);
begin
     //Pulse1Grp.Enabled :=  ckUsePulse1.Checked
     end;

procedure TStimSetupFrm.ckUsePulse2Click(Sender: TObject);
begin
     //Pulse2Grp.Enabled :=  ckUsePulse2.Checked
     end;


procedure TStimSetupFrm.LoadStimulusProgram(
          var StimProg : TStimProgram ;  // Stimulus program to be loaded (OUT)
          FileName : String                  // Name of file containing program (IN)
          ) ;
// -------------------------------
// Load stimulus program from file
// -------------------------------
var
     i : Integer ;
     FileHandle : THandle ;
     TextBuf : Array[1..TextBufSize] of ANSIchar ;
     Path : string ;
begin

     // Stimulus files held in \stim sub-folder within program folder
     Path := MainFrm.ProgramDirectory + StimSubFolder + FileName + '.' + FileExtension ;

     FileHandle := FileOpen( Path, fmOpenReadWrite ) ;
     if FileHandle <= 0 then begin
        MessageDlg( 'Could not open file ' + Path, mtWarning, [mbOK], 0 ) ;
        Exit ;
        end ;

     if FileRead( FileHandle, TextBuf, Sizeof(TextBuf) ) = Sizeof(TextBuf) then begin

        ReadFloat( TextBuf, 'STPER=', StimProg.Period ) ;
        ReadLogical( TextBuf, 'STSP=', StimProg.SingleStimulus ) ;

        // Voltage pulse settings
        ReadFloat( TextBuf, 'STVDEL=', StimProg.VDelay ) ;
        ReadFloat( TextBuf, 'STVPRW=', StimProg.VPrePulseWidth ) ;
        ReadFloat( TextBuf, 'STVPRA=', StimProg.VPrePulseAmplitude ) ;
        ReadFloat( TextBuf, 'STVPPW=', StimProg.VPrimaryPulseWidth ) ;
        ReadFloat( TextBuf, 'STVPPA=', StimProg.VPrimaryPulseAmplitude ) ;
        ReadFloat( TextBuf, 'STVPPWI=', StimProg.VPrimaryPulseWidthIncrement ) ;
        ReadFloat( TextBuf, 'STVPPAI=', StimProg.VPrimaryPulseAmplitudeIncrement ) ;
        ReadInt( TextBuf, 'STVNP=', StimProg.VNumPulses ) ;
        ReadFloat( TextBuf, 'STVPI=', StimProg.VPulseInterval ) ;

        // Stimulus pulse settings
        for i := 0 to High(StimProg.Pulses) do begin
            ReadLogical( TextBuf, format('STUSE%d=',[i]), StimProg.Pulses[i].InUse) ;
            ReadFloat( TextBuf, format('STSTA%d=',[i]), StimProg.Pulses[i].Delay) ;
            ReadFloat( TextBuf, format('STWID%d=',[i]), StimProg.Pulses[i].Width) ;
            ReadInt( TextBuf, format('STNP%d=',[i]),StimProg.Pulses[i].NumPulses) ;
            ReadFloat( TextBuf, format('STINT%d=',[i]), StimProg.Pulses[i].Interval) ;
            ReadLogical( TextBuf, format('STACH%d=',[i]), StimProg.Pulses[i].ActiveHigh) ;
            end ;
        end ;

     ReadFloat( TextBuf, 'STDUR=', StimProg.Duration ) ;

     if FileHandle >= 0 then FileClose( FileHandle ) ;


     end ;


procedure TStimSetupFrm.SaveStimulusProgram(
          var StimProg : TStimProgram ;  // Stimulus program to be saved (IN)
          FileName : String              // Name of file containing program (IN)
          ) ;
// -------------------------------
// Save stimulus program to file
// -------------------------------
var
     i : Integer ;
     FileHandle : THandle ;
     TextBuf : Array[1..TextBufSize] of ANSIchar ;
     Path : String ;
begin


     // Transfer latest data from edit boxes to StimProg
     EditBoxesToStimProg ;

     // Stimulus files held in \stim sub-folder within program folder
     Path := MainFrm.ProgramDirectory + StimSubFolder + FileName + '.' + FileExtension ;

     FileHandle := FileCreate( Path ) ;
     if FileHandle <= 0 then begin
        ShowMessage( ' Could not create file ' ) ;
        Exit ;
        end ;

     // Initialise empty buffer with zero bytes
     for i := 1 to High(TextBuf) do TextBuf[i] := chr(0) ;

     AppendFloat( TextBuf, 'STPER=', StimProg.Period ) ;
     AppendLogical( TextBuf, 'STSP=', StimProg.SingleStimulus ) ;

     // Voltage pulse settings
     AppendFloat( TextBuf, 'STVDEL=', StimProg.VDelay ) ;
     AppendFloat( TextBuf, 'STVPRW=', StimProg.VPrePulseWidth ) ;
     AppendFloat( TextBuf, 'STVPRA=', StimProg.VPrePulseAmplitude ) ;
     AppendFloat( TextBuf, 'STVPPW=', StimProg.VPrimaryPulseWidth ) ;
     AppendFloat( TextBuf, 'STVPPA=', StimProg.VPrimaryPulseAmplitude ) ;
     AppendFloat( TextBuf, 'STVPPWI=', StimProg.VPrimaryPulseWidthIncrement ) ;
     AppendFloat( TextBuf, 'STVPPAI=', StimProg.VPrimaryPulseAmplitudeIncrement ) ;
     AppendFloat( TextBuf, 'STVPPW=', StimProg.VPrePulseWidth ) ;
     AppendInt( TextBuf, 'STVNP=', StimProg.VNumPulses ) ;
     AppendFloat( TextBuf, 'STVPI=', StimProg.VPulseInterval ) ;


     // Stimulus pulse settings
     for i := 0 to High(StimProg.Pulses) do begin
         AppendLogical( TextBuf, format('STUSE%d=',[i]), StimProg.Pulses[i].InUse) ;
         AppendFloat( TextBuf, format('STSTA%d=',[i]), StimProg.Pulses[i].Delay) ;
         AppendFloat( TextBuf, format('STWID%d=',[i]), StimProg.Pulses[i].Width) ;
         AppendInt( TextBuf, format('STNP%d=',[i]),StimProg.Pulses[i].NumPulses) ;
         AppendFloat( TextBuf, format('STINT%d=',[i]), StimProg.Pulses[i].Interval) ;
         AppendLogical( TextBuf, format('STACH%d=',[i]), StimProg.Pulses[i].ActiveHigh) ;
         end ;

     AppendFloat( TextBuf, 'STDUR=', StimProg.Duration ) ;

     if FileWrite( FileHandle, TextBuf, Sizeof(TextBuf) ) <> Sizeof(TextBuf) then
        ShowMessage( ' Write to ' + FileName + ' failed!' ) ;

     if FileHandle >= 0 then FileClose( FileHandle ) ;

     // Update file name
     edFileName.Text := FileName ;

     for i := 0 to MainFrm.MDIChildCount-1 do
         if MainFrm.MDIChildren[I].Name = 'RecADCOnlyFrm' then begin
            TRecADCOnlyFrm(MainFrm.MDIChildren[I]).UpdateStimProgramList ;
            end ;

     end ;


procedure TStimSetupFrm.bLoadFileClick(Sender: TObject);
// ---------------------------------
// Load a stimulus program from file
// ---------------------------------
begin

     OpenDialog.options := [ofPathMustExist] ;
     OpenDialog.DefaultExt := FileExtension ;
     OpenDialog.Filter := format(' %s Files (*.%s)|*.%s',
                          [FileExtension,FileExtension,FileExtension]);
     OpenDialog.Title := 'Load Stimulus Program  ' ;


     if OpenDialog.execute then begin
        FileName := ExtractFilenameOnly(OpenDialog.FileName) ;
        LoadStimulusProgram( StimProg, FileName ) ;
        // Update edit boxes on form
        StimProgToEditBoxes ;

        // Update file name
        edFileName.Text := FileName ;

        end ;

     end;


procedure TStimSetupFrm.bSaveFileClick(Sender: TObject);
// ---------------------------------
// Save a stimulus program to file
// ---------------------------------
begin

     // Present user with standard Save File dialog box
     SaveDialog.options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.DefaultExt := FileExtension ;
     SaveDialog.FileName := FileName ;
     SaveDialog.Filter := format(' %s Files (*.%s)|*.%s',
                          [FileExtension,FileExtension,FileExtension]);
     SaveDialog.Title := 'Save Stimulus Program' ;

     if SaveDialog.execute then begin
        FileName := ExtractFileNameOnly(SaveDialog.FileName) ;
        SaveStimulusProgram( StimProg, FileName ) ;
        edFileName.Text := FileName ;
        end ;

     end;


procedure TStimSetupFrm.CreateProgramList(
          var cbList : TComboBox
          ) ;
{ --------------------------------------------
  Compile a list of stimulus files in the directory \winf\vprot
  and put the file names into a combo box
  --------------------------------------------------------------}
var
   SearchRec : TSearchRec ;
   First : Boolean ;
   FileFound : Integer ;
   StimDirectory : string ;
begin

     // Stimulus program folder
     StimDirectory := MainFrm.ProgramDirectory + 'stim\' ;

     First := True ;
     cbList.Clear ;
     cbList.items.add( 'None' ) ;
     repeat
        { Find file }
        if First then
           FileFound := FindFirst( StimDirectory + '*' + FileExtension,
                                   faAnyFile,
                                   SearchRec )
        else
           FileFound := FindNext( SearchRec ) ;

        { Add file name (no extension or path) to list }
        if FileFound = 0 then cbList.items.Add(ExtractFileNameOnly(SearchRec.Name))
                         else FindClose(SearchRec.FindHandle) ;
        First := False ;
        Until FileFound <> 0 ;

     end ;

procedure TStimSetupFrm.edNumPulses1KeyPress(Sender: TObject;
  var Key: Char);
begin
     if key = #13 then EditBoxesToStimProg ;
     end;

procedure TStimSetupFrm.edPrePulseAmplitudeKeyPress(Sender: TObject;
  var Key: Char);
begin
     if Key = #13 then EditBoxesToStimProg ;
     end;


function TStimSetupFrm.StimulusProgramDuration(
         StimProgram : TStimProgram
         ) : Single ;
// ------------------------------------------
// Return duration of stimulus within program
// ------------------------------------------
var
    P : Integer ;
    TotalTime : Single ;

begin

     TotalTime := StimProgram.VPrePulseWidth + StimProgram.VPrimaryPulseWidth + StimProgram.VDelay ;
     TotalTime := MaxFlt(  [ TotalTime, StimProgram.VNumPulses*StimProgram.VPulseInterval] ) ;

     for P := 0 to High(StimProgram.Pulses) do
         if StimProgram.Pulses[P].InUse then begin
            TotalTime := MaxFlt( [TotalTime,
                         StimProgram.Pulses[P].Delay
                         + (StimProgram.Pulses[P].NumPulses*StimProgram.Pulses[P].Width)
                         + (StimProgram.Pulses[P].NumPulses-1)*StimProgram.Pulses[P].Interval
                         ]) ;
            end ;
     Result := TotalTime ;
     end ;

end.
