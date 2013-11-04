unit ExcSetupUnit;
// =============================================================================
// WinFluor - Windows Fluorescence Imaging Program - Excitation wavelength setup module
// (c) J. Dempster, University of Strathclyde, 2001-2002, All Rights Reserved
// =============================================================================
// 16.5.2002
// 31.8.2006 Wavelengths now numbered 0-9
// 09.09.2006 Excitation spectrum setup added
// 22.04.08 No. wavelengths increased to 20
//          Multi-wavelength sequence now multi-rate
// 21.01
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ValEdit, Grids, ValidatedEdit, RangeEdit, math, idrfile,
  ComCtrls ;

const
   WavelengthTableSize = 20 ;
   MaxSequences = 10 ;
   EXSFileSize = 2048 ;
   EXSFileExtension = 'exs' ;

type

  TEXCSequence = record
         WavelengthNum : Integer ;
         DivideFactor : Integer ;
         end ;

  TExcSetupFrm = class(TForm)
    TableGrp: TGroupBox;
    WaveTable: TStringGrid;
    bOK: TButton;
    bCancel: TButton;
    GroupBox1: TGroupBox;
    meSequence: TMemo;
    SequenceGrp: TGroupBox;
    bAddWavelength: TButton;
    cbAddWavelength: TComboBox;
    SpectrumGrp: TGroupBox;
    edSpectrumRange: TRangeEdit;
    Label1: TLabel;
    edSpectrumStepSize: TValidatedEdit;
    Label2: TLabel;
    Label3: TLabel;
    edSpectrumBandwidth: TValidatedEdit;
    ClearCycle: TButton;
    Label4: TLabel;
    edDivideFactor: TValidatedEdit;
    edSequenceName: TEdit;
    cbSequence: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure ClearCycleClick(Sender: TObject);
    procedure bAddWavelengthClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCancelClick(Sender: TObject);
    procedure WaveTableKeyPress(Sender: TObject; var Key: Char);
    procedure edSequenceNameChange(Sender: TObject);
    procedure cbSequenceChange(Sender: TObject);
  private
    { Private declarations }
    NumWavelengths : Array[0..MaxSequences-1] of Integer ;
    Sequence : Array[0..MaxFrameType,0..MaxSequences-1] of TEXCSequence ;
    SequenceName : Array[0..MaxSequences-1] of String ;
    CentreWavelengths : Array[0..WavelengthTableSize-1] of Integer ;
    WavelengthWidths : Array[0..WavelengthTableSize-1] of Integer ;
    procedure DisplaySequence ;
    procedure UpdateWavelengthsTable ;
  public
    { Public declarations }
  end;

var
  ExcSetupFrm: TExcSetupFrm;

implementation

uses Main, maths , RecUnit, SnapUnit, shared , LogUnit;

{$R *.DFM}

const

     // Excitation wavelength table
     WvNum = 0 ;
     WvCentre = 1 ;
     WvWidth = 2 ;


procedure TExcSetupFrm.FormShow(Sender: TObject);
// ---------------------------------------------
// Initial control settings when form is opened
// ---------------------------------------------
var
     i,iSeq :Integer ;
begin

    // Create sequence selection list
    cbSequence.Clear ;
    for iSeq := 0 to MaxSequences-1 do begin
        cbSequence.Items.Add(MainFrm.EXCSequenceName[iSeq]) ;
        end ;
    cbSequence.ItemIndex := Min(Max(MainFrm.EXCSequenceNum,0),cbSequence.Items.Count-1) ;

     // Get list of excitations wavelengths/widths from master table
     for i := 0 to WavelengthTableSize-1 do begin
         CentreWavelengths[i] := MainFrm.EXCWavelengths[i].Centre ;
         WavelengthWidths[i] := MainFrm.EXCWavelengths[i].Width ;
         end ;
     UpdateWavelengthsTable ;

     // Copy current excitation settings into interval work variables
     for iSeq := 0 to MaxEXCSequences-1 do begin
         NumWavelengths[iSeq] := MainFrm.EXCNumWavelengths[iSeq] ;
         SequenceName[iSeq] := MainFrm.EXCSequenceName[iSeq] ;
         for i := 0 to NumWavelengths[iSeq]-1 do
             Sequence[i,iSeq] := MainFrm.EXCSequence[i,iSeq] ;
         end ;

     // Initial Add Wavelength number
     cbAddWavelength.Clear ;
     for i := 0 to WavelengthTableSize-1 do
         cbAddWavelength.Items.Add(format('%d',[i])) ;
     cbAddWavelength.ItemIndex := 0 ;

     // Display excitation wavelength sequence
     DisplaySequence ;

     // Excitation spectrum options
     edSpectrumRange.LoValue := MainFrm.EXCSpectrumStartWavelength ;
     edSpectrumRange.HiValue := MainFrm.EXCSpectrumEndWavelength ;
     edSpectrumBandwidth.Value := MainFrm.EXCSpectrumBandwidth ;
     edSpectrumStepSize.Value := MainFrm.EXCSpectrumStepSize ;

     ClientWidth := TableGrp.Left + TableGrp.Width + 5 ;
     ClientHeight :=  bOK.Top + bOK.Height + 10 ;

//     if NumWavelengths > MaxFrameType then bAddWavelength.Enabled := False
//                                      else bAddWavelength.Enabled := True ;

     end;

procedure TExcSetupFrm.UpdateWavelengthsTable ;
// -------------------------------------------------------------------
// Update table of available wavelengths for multi-wavelength sequence
// -------------------------------------------------------------------
var
    i : Integer ;
begin
     { Set excitation wavelength table }
     WaveTable.cells[WvNum,0] := 'No.' ;
     WaveTable.colwidths[WvNum] := WaveTable.DefaultColWidth div 2 ;
     WaveTable.cells[WvCentre,0] := ' Wavelength ' ;
     WaveTable.colwidths[WvCentre] := {WaveTable.Canvas.TextWidth(
                                     WaveTable.cells[WvCentre,0])}70 ;
     WaveTable.cells[WvWidth,0] := ' Bandwidth ' ;
     WaveTable.colwidths[WvWidth] := {WaveTable.Canvas.TextWidth(
                                     WaveTable.cells[WvWidth,0])}70 ;
     WaveTable.options := [goEditing,goHorzLine,goVertLine] ;
     WaveTable.RowCount := WavelengthTableSize+1 ;
     for i := 0 to WavelengthTableSize-1 do begin
         WaveTable.cells[WvNum,i+1] := IntToStr(i) ;
         WaveTable.cells[WvCentre,i+1] := format( ' %d nm',[CentreWavelengths[i]]) ;
         if WavelengthWidths[i] >= 0.0 then begin
            WaveTable.cells[WvWidth,i+1] := format( ' %d nm',[WavelengthWidths[i]]) ;
            end
         else begin
            WaveTable.cells[WvWidth,i+1] := format( ' %d %%',[WavelengthWidths[i]]) ;
            end ;
         end ;

     end ;

procedure TExcSetupFrm.DisplaySequence ;
// -------------------------
// Display waveform sequence
// -------------------------
var
    i,Current,Last,Num : Integer ;
    MaxDivideFactor : Integer ;
    Temp : TEXCSequence ;
    s : String ;
begin

     // Sequence name
     edSequenceName.Text := SequenceName[cbSequence.ItemIndex] ;

     // Find maximum divide factor
     MaxDivideFactor := 1 ;
     for i := 0 to NumWavelengths[cbSequence.ItemIndex]-1 do begin
         MaxDivideFactor := Max(Sequence[i,cbSequence.ItemIndex].DivideFactor,MaxDivideFactor) ;
         end ;

     // Ensure there are two divide factors in use ( 1 and MaxDivideFactor)
     for i := 0 to NumWavelengths[cbSequence.ItemIndex]-1 do begin
         if Sequence[i,cbSequence.ItemIndex].DivideFactor <> 1 then
            Sequence[i,cbSequence.ItemIndex].DivideFactor := MaxDivideFactor ;
         end ;

     // Sort into ascending order
     for Last := (NumWavelengths[cbSequence.ItemIndex]-1) DownTo 1 do begin
         for Current := 0 to Last-1 do begin
             if Sequence[Current,cbSequence.ItemIndex].DivideFactor <
               Sequence[Current+1,cbSequence.ItemIndex].DivideFactor then begin
                Temp := Sequence[Current,cbSequence.ItemIndex] ;
                Sequence[Current,cbSequence.ItemIndex] := Sequence[Current+1,cbSequence.ItemIndex] ;
                Sequence[Current+1,cbSequence.ItemIndex] := Temp ;
                end ;
             end ;
         end ;

     // Create list of excitation wavelengths
     meSequence.Clear ;
     for i := 0 to NumWavelengths[cbSequence.ItemIndex]-1 do begin
         Num := Sequence[i,cbSequence.ItemIndex].WavelengthNum ;
         if WavelengthWidths[Num] < 1000.0 then begin
            s := format('%d: %d (%d) nm /%d',
                 [Num,
                  CentreWavelengths[Num],
                  WavelengthWidths[Num],
                  Sequence[i,cbSequence.ItemIndex].DivideFactor] ) ;
            end
         else begin
            s := format('%d: %d nm (Laser) /%d',
                 [Num,
                  CentreWavelengths[Num],
                  Sequence[i,cbSequence.ItemIndex].DivideFactor] ) ;
            end ;

         meSequence.Lines.Add(s) ;

         end ;

     end ;


procedure TExcSetupFrm.ClearCycleClick(Sender: TObject);
// ------------------------------------
// Clear excitation wavelength sequence
// ------------------------------------
begin

     NumWavelengths[cbSequence.ItemIndex] := 0 ;
     bAddWavelength.Enabled := True ;

     DisplaySequence ;

     end ;


procedure TExcSetupFrm.bAddWavelengthClick(Sender: TObject);
// -------------------------------------
// Add wavelength to excitation sequence
// -------------------------------------
var
     Num : Integer ;
begin

     if NumWavelengths[cbSequence.ItemIndex] > High(Sequence) then begin
        bAddWavelength.Enabled := False ;
        Exit ;
        end ;

     Inc(NumWavelengths[cbSequence.ItemIndex]) ;
     Num := cbAddWavelength.ItemIndex ;
     Sequence[NumWavelengths[cbSequence.ItemIndex]-1,cbSequence.ItemIndex].WavelengthNum := Num ;
     Sequence[NumWavelengths[cbSequence.ItemIndex]-1,cbSequence.ItemIndex].DivideFactor := Round(edDivideFactor.Value) ;

     DisplaySequence ;

     end;


procedure TExcSetupFrm.bOKClick(Sender: TObject);
// -----------------------------------------------
// OK press - Close dialog box and update settings
// -----------------------------------------------
var
     i,iSeq : Integer ;
begin
     // Copy updated excitation settings to main settings

     // If no wavelength sequence defined revert to single wavelength
     if NumWavelengths[cbSequence.ItemIndex] <= 0 then MainFrm.EXCSingleWaveLength := True ;

     for iSeq := 0 to MaxEXCSequences-1 do begin
         MainFrm.EXCSequenceName[iSeq] := SequenceName[iSeq] ;
         MainFrm.EXCNumWavelengths[iSeq] := NumWavelengths[iSeq] ;
         for i := 0 to NumWavelengths[iSeq]-1 do
             MainFrm.EXCSequence[i,iSeq] := Sequence[i,iSeq] ;
         end ;

     // Update wavelengths table
     for i := 0 to WavelengthTableSize-1 do begin
         MainFrm.EXCWavelengths[i].Centre := Round(
             ExtractFloat(WaveTable.cells[WvCentre,i+1],0.0)) ;
         MainFrm.EXCWavelengths[i].Width := Round(
             ExtractFloat(WaveTable.cells[WvWidth,i+1],0.0));
         end ;

     // Excitation spectrum options
     MainFrm.EXCSpectrumStartWavelength := Min( edSpectrumRange.LoValue,
                                                edSpectrumRange.HiValue ) ;
     MainFrm.EXCSpectrumEndWavelength := Max( edSpectrumRange.LoValue,
                                              edSpectrumRange.HiValue ) ;
     MainFrm.EXCSpectrumBandwidth := edSpectrumBandwidth.Value ;
     MainFrm.EXCSpectrumStepSize := Round(edSpectrumStepSize.Value) ;

     // Request a timing cycle change
     if MainFrm.FormExists( 'RecordFrm') then begin
        if RecordFrm.CameraRunning then RecordFrm.RestartCamera ;
        end ;
     if MainFrm.FormExists( 'SnapFrm') then begin
        if SnapFrm.CameraRunning then SnapFrm.RestartCamera ;
        end ;

     Close ;

     end;


procedure TExcSetupFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
     Action := caFree ;
     end;

procedure TExcSetupFrm.bCancelClick(Sender: TObject);
begin
     Close ;
     end;

procedure TExcSetupFrm.WaveTableKeyPress(Sender: TObject; var Key: Char);
var
     i,Num : Integer ;
begin
     if Key = #13 then begin
        // Update wavelengths table
        for i := 0 to WavelengthTableSize-1 do begin
            CentreWavelengths[i] := Round(ExtractFloat(WaveTable.cells[WvCentre,i+1],0.0)) ;
            WaveTable.cells[WvCentre,i+1] := format( ' %d nm',[CentreWavelengths[i]]) ;
            WavelengthWidths[i] := Round(ExtractFloat(WaveTable.cells[WvWidth,i+1],0.0));
            WaveTable.cells[WvWidth,i+1] := format( ' %d nm',[WavelengthWidths[i]]) ;
            end ;

        // Create list of excitation wavelengths
        meSequence.Clear ;
        for i := 0 to NumWavelengths[cbSequence.ItemIndex]-1 do begin
            Num := Sequence[i,cbSequence.ItemIndex].WavelengthNum ;
            meSequence.Lines.Add( format('%d: %d (%d) nm /%d',
            [Num,
             CentreWavelengths[Num],
             WavelengthWidths[Num],
             Sequence[i,cbSequence.ItemIndex].DivideFactor] )) ;
            end ;

         end ;

     end;



procedure TExcSetupFrm.edSequenceNameChange(Sender: TObject);
// ---------------------
// Sequence name changed
// ---------------------
begin
    SequenceName[cbSequence.ItemIndex] := edSequenceName.Text ;
    end;

procedure TExcSetupFrm.cbSequenceChange(Sender: TObject);
// -------------------------
// Selected sequence changed
// -------------------------
begin
     DisplaySequence ;
     end;

end.
