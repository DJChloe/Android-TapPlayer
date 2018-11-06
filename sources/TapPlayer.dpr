program TapPlayer;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {MainForm},
  UnitFileFrame in 'UnitFileFrame.pas' {frameFile: TFrame},
  UnitTapFiles in 'UnitTapFiles.pas',
  UnitTapFilesList in 'UnitTapFilesList.pas',
  UnitOpenDiag in 'UnitOpenDiag.pas' {FormOpenFile},
  UnitProgressDialog in 'UnitProgressDialog.pas' {FormProgressDialog},
  UnitAudio in 'UnitAudio.pas',
  UnitPermissions in 'UnitPermissions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormOpenFile, FormOpenFile);
  Application.CreateForm(TFormProgressDialog, FormProgressDialog);
  Application.Run;
end.
