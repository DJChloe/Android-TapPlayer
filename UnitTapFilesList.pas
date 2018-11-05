unit UnitTapFilesList;

interface
uses
  Sysutils, Classes, System.Generics.collections, unitTapFiles;

type
  TLoadingMethod=(lmMergeRawData,lmDistinctRawData);
  TOricTape = class
  private
    FList: TObjectList<TOricFile>;
    FFileName: string;
    FContainsRawData: boolean;
    function GetItem(Index: Integer): TOricFile;
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure SetItem(Index: Integer; const Value: TOricFile);
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
    function GetOwnsObjects: boolean;
    procedure SetOwnsObjects(const Value: boolean);
    procedure SetFileName(const Value: string);
    function GetTotalWavLength: int64;
    function GetTotalSize: int64;
    procedure SetContainsRawData(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadfromFile(Filename:string;LoadingMethod:TLoadingMethod);
    procedure SaveToFileAS(Filename:string);
    procedure SaveToFile;
    function Add(Item: TOricFile): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TOricTape;
    function Extract(ItemIndex: integer): TOricFile;
    function First: TOricFile;
    function IndexOf(Item: TOricFile): Integer;
    procedure Insert(Index: Integer; Item: TOricFile);
    function Last: TOricFile;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TOricFile): Integer;
    procedure Pack;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TOricFile read GetItem write SetItem; default;
    property OwnsObjects: boolean read GetOwnsObjects write SetOwnsObjects;
    property FileName:string read FFileName write SetFileName;
    property TotalSize:int64 read GetTotalSize;
    property TotalWavLength:int64 read GetTotalWavLength;
    property ContainsRawData:boolean read FContainsRawData write SetContainsRawData;
  end;
implementation
{ TOricTape }

uses System.IOUtils;

function TOricTape.Add(Item: TOricFile): Integer;
begin
  Result := FList.Add(Item);
end;

procedure TOricTape.Clear;
begin
  FList.Clear;
  ContainsRawData:=false;
end;

constructor TOricTape.Create;
begin
  FList := TObjectList<TOricFile>.Create;
  ContainsRawData:=false;
end;

procedure TOricTape.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TOricTape.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TOricTape.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TOricTape.Expand: TOricTape;
begin
  Result := TOricTape(FList.Expand);
end;

function TOricTape.Extract(ItemIndex: integer): TOricFile;
begin
 // Result := FList.Extract(Item);
  result:=Items[ItemIndex];
  flist.Items[ItemIndex]:=nil;
  flist.Pack;
end;

function TOricTape.First: TOricFile;
begin
  Result := TOricFile(FList.First);
end;

function TOricTape.GetItem(Index: Integer): TOricFile;
begin
  Result := TObject(FList[Index]) as TOricFile;
end;

function TOricTape.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TOricTape.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TOricTape.IndexOf(Item: TOricFile): Integer;
begin
  Result := FList.IndexOf(Item);
end;

procedure TOricTape.Insert(Index: Integer; Item: TOricFile);
begin
  FList.Insert(Index, Item);
end;

function TOricTape.Last: TOricFile;
begin
  Result := TOricFile(FList.Last);
end;

procedure TOricTape.LoadFromFile(Filename:string;LoadingMethod:TLoadingMethod);
var i:integer;
    fs:TFileStream;
    AnOricFile:TOricFile;
    b,bb,index:byte;
    count16:int64;
    firstraw,sync_ok:boolean;
    rawwriting:boolean;
begin
  if FileName<>'' then
  begin
    Clear;
    index:=0;
    AnOricFile:=nil;
    if not FileExists(FileName) then exit;
    FFileName:=Filename;
    fs:=TFileStream.Create(FFileName,fmOpenRead);
    try
    fs.Position:=0;
    while (fs.Position<fs.size) do
    begin
      b:=0;
      count16:=0;
      sync_ok:=false;
      firstraw:=true;
      rawwriting:=false;
      while (not sync_ok) and (fs.Position<fs.size) do
      begin
        fs.Read(b,1);
        if b=$16 then inc(count16)
        else begin
               if (count16>2) and (b=$24)
               then begin
                      sync_ok:=true;
                      if rawwriting then
                      begin
                        AnOricFile.TapeIndex:=index;
                        Add(AnOricFile);
                        inc(index);
                        count16:=0;
                        firstraw:=true;
                        rawwriting:=false;
                      end;
                    end
               else begin
                      if firstraw
                      then begin
                             if LoadingMethod=lmDistinctRawData then
                                AnOricFile:=TOricFile.Create;
                             AnOricFile.RawData:=true;
                             ContainsRawData:=true;
                             firstraw:=false;
                             rawwriting:=true;
                           end;
                      for i := 1 to count16 do
                      begin
                        bb:=$16;
                        AnOricFile.Data.Write(bb,1);
                      end;
                      AnOricFile.Data.Write(b,1);
                      count16:=0;
               end;
              end;

      end;
      //while (b<>$16) do fs.Read(b,1);

      //b:=$16;
      if sync_OK then
      begin
      AnOricFile:=TOricFile.Create;

     // while (b=$16) do begin
     //                    fs.Read(b,1); // read synchro (0x24 included)
     //                  end;
      b:=$16;
      AnOricFile.Header.Write(b,1);
      AnOricFile.Header.Write(b,1);
      AnOricFile.Header.Write(b,1);
      b:=$24;
      AnOricFile.Header.Write(b,1);
      if (fs.Position>=fs.size) then break;

      AnOricFile.TapeIndex:=index;
      //header
      for i:=0 to 8 do
      begin
         fs.Read(b,1);
         AnOricFile.Header.Write(b,1);
      end;
      //Name

      repeat
        i:=fs.Read(b,1);
        AnOricFile.Header.Write(b,1);
      until ((b=0) or (i=0));

      AnOricFile.StreamToProps;
      //data

      //if (fs.Position+AnOricFile.size+1)<fs.size then
      begin
        AnOricFile.Data.CopyFrom(fs,AnOricFile.size);
        if LoadingMethod=lmDistinctRawData then Add(AnOricFile);
      end;
      inc(index);
      end else
      if rawwriting then
      begin
        AnOricFile.TapeIndex:=index;
        if LoadingMethod=lmDistinctRawData then Add(AnOricFile);
      end;
    end;
    if LoadingMethod=lmMergeRawData then  Add(AnOricFile);
    finally
      fs.Free;
    end;
  end;
end;

procedure TOricTape.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TOricTape.Pack;
begin
  FList.Pack;
end;

procedure TOricTape.SetItem(Index: Integer; const Value: TOricFile);
begin
  FList.Items[Index] := Value;
end;

function TOricTape.Remove(Item: TOricFile): Integer;
begin
  Result := FList.Remove(Item);
end;

procedure TOricTape.SaveToFile;
var i:integer;
    fs:TFileStream;
begin
  if FFileName<>'' then
  begin
    if Tfile.Exists(FFileName) then TFile.Delete(FFileName);
    fs:=TFileStream.Create(FFileName,fmCreate);
    try
    for I := 0 to Count - 1 do
      begin
        Items[i].SaveToStream(fs);
      end;
    finally
      fs.Free;
    end;
  end;
end;

procedure TOricTape.SaveToFileAS(Filename:string);
begin
  FFileName:=Filename;
  SaveToFile;
end;

procedure TOricTape.SetCapacity(const Value: Integer);
begin
  FList.Capacity := Value;
end;

procedure TOricTape.SetContainsRawData(const Value: boolean);
begin
  FContainsRawData := Value;
end;

procedure TOricTape.SetCount(const Value: Integer);
begin
  FList.Count := Value;
end;

procedure TOricTape.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

function TOricTape.GetOwnsObjects: boolean;
begin
  Result := FList.OwnsObjects;
end;

function TOricTape.GetTotalSize: int64;
var i:integer;
begin
  result:=0;
  for i := 0 to Count - 1 do
    result:=Result+Items[i].data.size;
end;

function TOricTape.GetTotalWavLength: int64;
var i:integer;
begin
  result:=0;
  for i := 0 to Count - 1 do
    result:=Result+Items[i].WavLength;
end;

procedure TOricTape.SetOwnsObjects(const Value: boolean);
begin
  FList.OwnsObjects := Value;
end;

end.

