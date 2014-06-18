unit About;

// 18.06.14 Product name caption now updated with latest version number

interface

uses Windows, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, SysUtils;

type
  TAboutBox = class(TForm)
    Panel: TPanel;
    OKButton: TButton;
    ProductName: TLabel;
    meCameraInfo: TMemo;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

uses Main;

{$R *.DFM}

procedure TAboutBox.FormShow(Sender: TObject);
begin
     ProductName.Caption :=  MainFrm.ProgramName ;
     MainFrm.Cam1.GetCameraInfo( meCameraInfo.Lines ) ;
     end;

end.

