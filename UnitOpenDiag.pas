unit UnitOpenDiag;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FMX.Objects,
  System.ImageList, FMX.ImgList;

type
  TFormOpenFile = class(TForm)
    ToolBarLabel: TLabel;
    HeaderToolBar: TToolBar;
    ToolBar1: TToolBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    LabelSelText: TLabel;
    Panel1: TPanel;
    LabelSelection: TLabel;
    ListBox1: TListBox;
    LabelPath: TLabel;
    ListBoxItem1: TListBoxItem;
    Rectangle1: TRectangle;
    ImageList1: TImageList;
    procedure ListBox1ItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Déclarations privées }
    fCurrentPath:string;
    fSelectedFile:string;
    procedure SetCurrentPath(const Value: string);
    procedure SetSelectedFile(const Value: string);
    procedure AddListItem(list: array of string; itype: string);
    procedure TotalWork(path_tr: string; clear: boolean);
  public
    { Déclarations publiques }
    property CurrentPath:string read fCurrentPath write SetCurrentPath;
    property SelectedFile:string read fSelectedFile write SetSelectedFile;
  end;

var
  FormOpenFile: TFormOpenFile;

implementation

{$R *.fmx}
uses
  System.IOUtils, System.Generics.Collections, Generics.Defaults,
  FMX.Helpers.Android, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Webkit,main;

function CompareLowerStr(const Left, Right: string): Integer;
begin
  Result := CompareStr(AnsiLowerCase(Left), AnsiLowerCase(Right));
end;

procedure TFormOpenFile.SetCurrentPath(const Value: string);
begin
  if TDirectory.Exists(value) then
  begin
      fCurrentPath:=value;
      LabelPath.Text:=fCurrentPath;
      TotalWork(fCurrentPath, True);
  end else fCurrentPath:='-';
end;

procedure TFormOpenFile.SetSelectedFile(const Value: string);
begin
  if fSelectedFile=value then Exit;
  fSelectedFile:=value;
  LabelSelection.Text:=ExtractFileName(fSelectedFile);
end;

procedure TFormOpenFile.SpeedButton1Click(Sender: TObject);
begin
  Hide;
  Application.ProcessMessages;
  if FileExists(FormOpenFile.SelectedFile) then
  TabbedForm.OpenFile(FormOpenFile.SelectedFile);
end;

procedure TFormOpenFile.SpeedButton2Click(Sender: TObject);
begin
  Close;
end;

procedure TFormOpenFile.FormCreate(Sender: TObject);
begin
  CurrentPath := TPath.GetSharedDocumentsPath;
end;

procedure TFormOpenFile.FormShow(Sender: TObject);
begin
  TotalWork(fCurrentPath, True);
end;

procedure TFormOpenFile.ListBox1ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var
  LItemPath:string;
begin
  LItemPath := Item.ItemData.Detail;

  if Item.TagString = 'folder' then
  begin
    CurrentPath:=LItemPath;
  end
  else if Item.TagString = 'file' then
  begin
    try
      if TFile.Exists(LItemPath) then SelectedFile:=LItemPath;
    except
      ShowMessage('Impossible d''ouvrir le fichier!');
    end;
  end;
end;

procedure TFormOpenFile.AddListItem(list: array of string; itype: string);
var
  c: integer;
  LItem: TListBoxItem;
begin

  ListBox1.BeginUpdate;

  for c := 0 to Length(list) - 1 do
  begin
    LItem := TListBoxItem.Create(ListBox1);
    LItem.ItemData.Text := TPath.GetFileName(list[c]);
    LItem.ItemData.Detail := list[c];
    if itype='folder'
    then LItem.ImageIndex:=0
    else LItem.ImageIndex:=1;
    LItem.TagString := itype;
    ListBox1.AddObject(LItem);
  end;

  ListBox1.EndUpdate;
end;

procedure TFormOpenFile.TotalWork(path_tr: string; clear: boolean);
var
  folders, files: TStringDynArray;
  LItem: TListBoxItem;
  parentdir:string;
begin
  if clear then
  begin
      ListBox1.Clear;
      parentdir:='';
      try
      parentdir:=TDirectory.GetParent(path_tr);
      if parentdir<>'' then
      begin
        LItem := TListBoxItem.Create(ListBox1);
        LItem.ItemData.Text := '..';
        LItem.ItemData.Detail := parentdir;
        LItem.TagString := 'folder';
        ListBox1.AddObject(LItem);
      end;
      finally

      end;

  end;
  folders := TDirectory.GetDirectories(path_tr);
  TArray.Sort<string>(folders, TComparer<string>.Construct(CompareLowerStr));
  AddListItem(folders, 'folder');

  files := TDirectory.GetFiles(path_tr,'*.tap');
  TArray.Sort<string>(files, TComparer<string>.Construct(CompareLowerStr));
  AddListItem(files, 'file');
end;

end.
