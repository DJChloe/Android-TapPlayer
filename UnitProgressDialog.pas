unit UnitProgressDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TFormProgressDialog = class(TForm)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Rectangle1: TRectangle;
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
  end;

var
  FormProgressDialog: TFormProgressDialog;

implementation

{$R *.fmx}
uses main;

end.
