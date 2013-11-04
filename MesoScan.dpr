program MesoScan;

uses
  Forms,
  MainUnit in '..\MesoScan\MainUnit.pas' {MainFrm},
  LabIOUnit in '..\MesoScan\LabIOUnit.pas' {LabIO: TDataModule},
  LogUnit in '..\MesoScan\LogUnit.pas' {LogFrm},
  nidaqmxlib in '..\MesoScan\nidaqmxlib.pas',
  nidaqlib in '..\MesoScan\nidaqlib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.CreateForm(TLabIO, LabIO);
  Application.CreateForm(TLogFrm, LogFrm);
  Application.Run;
end.
