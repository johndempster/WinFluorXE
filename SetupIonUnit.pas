unit SetupIonUnit;
// ------------------------------------------
// WinFluor - Ion binding equations Setup Form
// ------------------------------------------
// (c) J. Dempster, University of Strathclyde 2002, All Rights Reserved
// 8.10.3 ... Created
// 18.08.06 ... RMax / Keff boxes labels now
// 23.08.06 ... Equations in use now saved to IDR file correctly
// 30.08.10 ... Access violation when equation with blank name added fixed


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ValidatedEdit, IDRFile ;

const
     MaxBindingEquations = Maxeqn ;
  

type
  TSetupIonFrm = class(TForm)
    GroupBox15: TGroupBox;
    GroupBox16: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    edRMax: TValidatedEdit;
    edRMin: TValidatedEdit;
    edKEff: TValidatedEdit;
    edIon: TEdit;
    edUnits: TEdit;
    GroupBox17: TGroupBox;
    Label8: TLabel;
    edNewEqnName: TEdit;
    bNewTable: TButton;
    bDeleteTable: TButton;
    bOK: TButton;
    bCancel: TButton;
    Label10: TLabel;
    cbBindingEquation: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure cbBindingEquationChange(Sender: TObject);
    procedure cbBindingEquationEnter(Sender: TObject);
    procedure bNewTableClick(Sender: TObject);
    procedure bDeleteTableClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BindingEquations : Array[0..MaxBindingEquations-1] of TBindingEquation ;
    procedure BindingEqnTableToEditBoxes ;
    procedure BindingEqnEditBoxesToTable ;

  end;

var
  SetupIonFrm: TSetupIonFrm;

implementation

uses Main, TimeCourseUnit ;

{$R *.dfm}

procedure TSetupIonFrm.FormShow(Sender: TObject);
// ---------------------------------------------
// Initial control settings when form is opened
// ---------------------------------------------
var
     i :Integer ;
begin

     MainFrm.mnSetupIon.Enabled := False ;

     // Close any form which might be acquiring images
     for i := 0 to Mainfrm.MDIChildCount-1 do begin
         if MainFrm.MDIChildren[i].Name = 'IntegrateFrm' then
            MainFrm.MDIChildren[i].Close ;
         if MainFrm.MDIChildren[i].Name = 'RecordFrm' then
            MainFrm.MDIChildren[i].Close ;
         end ;

     // Create list of available equations

     cbBindingEquation.Clear ;
     cbBindingEquation.ItemIndex := -1 ;
     for i := 0 to High(BindingEquations) do begin

         // Copy default equation set (stored in winfluor.ini)
         BindingEquations[i] := MainFrm.BindingEquations[i] ;

         // Copy equation from IDR file it is available
         if (MainFrm.IDRFile.Open) and
            (MainFrm.IDRFile.Equation[i].InUse) then begin
            // Copy main settings to working set
            BindingEquations[i] := MainFrm.IDRFile.Equation[i] ;
            end ;

         // Add to selection list
         if BindingEquations[i].InUse then begin
            cbBindingEquation.Items.AddObject( BindingEquations[i].Name,
                                               TObject(i)) ;
            cbBindingEquation.ItemIndex := cbBindingEquation.Items.Count-1 ;
            end ;

         end ;

     // Select first equation (if available)
     if  cbBindingEquation.ItemIndex >= 0 then
         cbBindingEquation.ItemIndex := 0 ;

     // Enable delete button if equations available
     if cbBindingEquation.Items.Count > 0 then
        bDeleteTable.Enabled := True
     else bDeleteTable.Enabled := False ;

     BindingEqnTableToEditBoxes ;

     end ;


procedure TSetupIonFrm.bOKClick(Sender: TObject);
// ------------------------------------
// Update settings and close setup form
// ------------------------------------
var
    i,MDIChild : Integer ;
begin

    // Update binding eqn. table with settings from edit boxes
    BindingEqnEditBoxesToTable ;

    // Update master settings with local working set
    for i := 0 to High(BindingEquations) do begin

        // Copy default equation set (stored in winfluor.ini)
        MainFrm.BindingEquations[i] := BindingEquations[i] ;

        // Copy equation to IDR file it is available
        if MainFrm.IDRFile.Open and
           (BindingEquations[i].InUse) then begin
           MainFrm.IDRFile.Equation[i] := BindingEquations[i] ;
           end ;

        end ;

    // Update relevant forms if they are active
    for MDIChild := 0 to MainFrm.MDIChildCount-1 do begin
        if MainFrm.MDIChildren[MDIChild].Name = 'TimeCourseFrm' then
           TTimeCourseFrm(MainFrm.MDIChildren[MDIChild]).UpdateSettings ;
        end ;

    Close ;

    end;


procedure TSetupIonFrm.BindingEqnTableToEditBoxes ;
//  --------------------------------------------
// Update binding equation parameter edit boxes
// --------------------------------------------
var
     iEqn : Integer ;
begin

     if cbBindingEquation.ItemIndex >= 0 then begin

        iEqn := Integer(
                cbBindingEquation.Items.Objects[cbBindingEquation.ItemIndex]) ;

        edIon.Text := BindingEquations[iEqn].Ion ;
        edIon.Enabled := True ;
        edUnits.Text := BindingEquations[iEqn].Units ;
        edUnits.Enabled := True ;
        edRMax.Value :=  BindingEquations[iEqn].RMax ;
        edRMax.Enabled := True ;
        edRMin.Value :=  BindingEquations[iEqn].RMin ;
        edRMin.Enabled := True ;
        edKEff.Value :=  BindingEquations[iEqn].KEff ;
        edKEff.Enabled := True ;

        end
     else begin
        // No binding equations defined
        edIon.Text :=  '' ;
        edIon.Enabled := False ;
        edUnits.Text := '' ;
        edUnits.Enabled := False ;
        edRMax.Value :=  0.0 ;
        edRMax.Enabled := False ;
        edRMin.Value :=  0.0 ;
        edRMin.Enabled :=  False ;
        edKEff.Value :=  0.0 ;
        edKEff.Enabled :=  False ;
        end ;
     end ;


procedure TSetupIonFrm.BindingEqnEditBoxesToTable ;
//  -------------------------------------------------
// Copy equation parameters from edit boxes to table
// -------------------------------------------------
var
    iEqn : Integer ;
begin

    if cbBindingEquation.ItemIndex >= 0 then begin
       // Update binding eqn. table with settings from edit boxes
       iEqn := Integer(
               cbBindingEquation.Items.Objects[cbBindingEquation.ItemIndex]) ;
       if iEqn <= High(BindingEquations) then begin
          BindingEquations[iEqn].Ion := edIon.Text ;
          BindingEquations[iEqn].Units := edUnits.Text ;
          BindingEquations[iEqn].RMax := edRMax.Value ;
          BindingEquations[iEqn].RMin := edRMin.Value ;
          BindingEquations[iEqn].KEff := edKEff.Value ;
          end ;
       end ;

    end ;


procedure TSetupIonFrm.cbBindingEquationChange(Sender: TObject);
// ----------------------------------------------------------
// Update binding eqn. parameter table when new eqn. selected
// ----------------------------------------------------------
begin
     BindingEqnTableToEditBoxes ;
     end;


procedure TSetupIonFrm.cbBindingEquationEnter(Sender: TObject);
// --------------------------------------------------------------------
// Save current edit boxes to table when equation list selected by user
// --------------------------------------------------------------------
begin
    BindingEqnEditBoxesToTable ;
    end;


procedure TSetupIonFrm.bNewTableClick(Sender: TObject);
//  --------------------------------
// Add new binding equation to list
// ---------------------------------
var
     iEqn : Integer ;
begin

    // Update currently selected table entry
    if cbBindingEquation.Items.Count > 0 then BindingEqnEditBoxesToTable ;

     // Find first empty slot in table
     iEqn := 0 ;
     While BindingEquations[iEqn].InUse
           and (iEqn < High(BindingEquations)) do Inc(iEqn) ;

     // Add new equation to table
     if not BindingEquations[iEqn].InUse then begin

        BindingEquations[iEqn].InUse := True ;

        if edNewEqnName.Text = '' then BindingEquations[iEqn].Name :=  format('Eqn%d',[iEqn])
                                  else BindingEquations[iEqn].Name := edNewEqnName.Text ;

        cbBindingEquation.Items.AddObject( BindingEquations[iEqn].Name,
                                           TObject(iEqn)) ;
       cbBindingEquation.ItemIndex := cbBindingEquation.Items.Count-1 ;

        // Initial binding equation parameters
        edIon.Text :=  '??' ;
        edIon.Enabled := True ;
        edUnits.Text := 'uM' ;
        edUnits.Enabled := True ;
        edRMax.Value :=  0.0 ;
        edRMax.Enabled := True ;
        edRMin.Value :=  0.0 ;
        edRMin.Enabled :=  True ;
        edKEff.Value :=  0.0 ;
        edKEff.Enabled :=  True ;

        // Enable delete button
        bDeleteTable.Enabled := True ;

        end ;

     end;


procedure TSetupIonFrm.bDeleteTableClick(Sender: TObject);
// ---------------------------------
// Delete binding equation from list
// ---------------------------------
var
     iEqn : Integer ;
begin
     if cbBindingEquation.ItemIndex >= 0 then begin

        // Delete selected equation
        iEqn := Integer(
                cbBindingEquation.Items.Objects[cbBindingEquation.ItemIndex]) ;

        cbBindingEquation.Items.Delete(cbBindingEquation.ItemIndex) ;
        BindingEquations[iEqn].InUse := False ;
        cbBindingEquation.ItemIndex := cbBindingEquation.Items.Count - 1 ;
        BindingEqnTableToEditBoxes ;

        // Disable if no equations left
        if cbBindingEquation.Items.Count > 0 then
           bDeleteTable.Enabled := True
        else begin
           bDeleteTable.Enabled := False ;
           cbBindingEquation.DroppedDown := True ;
           cbBindingEquation.DroppedDown := False ;
           end ;

        end ;

     end;


procedure TSetupIonFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
// -----------------------
// Close and destroy form
// -----------------------
begin
     MainFrm.mnSetupIon.Enabled := True ;
     Action := caFree ;
     end;

procedure TSetupIonFrm.bCancelClick(Sender: TObject);
// -------------------------------------
// Exit without updating master settings
// -------------------------------------
begin
    Close ;
    end;


end.
