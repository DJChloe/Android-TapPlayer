program TapPlayer;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {TabbedForm},
  UnitFileFrame in 'UnitFileFrame.pas' {frameFile: TFrame},
  UnitTapFiles in 'UnitTapFiles.pas',
  UnitTapFilesList in 'UnitTapFilesList.pas',
  UnitWavs in 'UnitWavs.pas',
  UnitOpenDiag in 'UnitOpenDiag.pas' {FormOpenFile},
  UnitProgressDialog in 'UnitProgressDialog.pas' {FormProgressDialog},
  UnitAudio in 'UnitAudio.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTabbedForm, TabbedForm);
  Application.CreateForm(TFormOpenFile, FormOpenFile);
  Application.CreateForm(TFormProgressDialog, FormProgressDialog);
  Application.Run;
end.
