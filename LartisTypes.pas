unit LartisTypes;

interface

type
  TJSONFile = Record
    Name: String;
    Size: Integer;
    Hash: String;
    Time: Integer;
  End;
  TJSONFileArray = Array of TJSONFile;

  TJSONMeta = Record
    Content: String;
    Size: Integer;
    Version: String;
  End;

  TJSONContent = Record
    Name: String;
    Desc: String;
    Inst: String;
    Items: Integer;
    Size: Integer;
    Data: TJSONFileArray;
  End;
  TJSONContentArray = Array of TJSONContent;

  TJSONFileList = Record
    Meta: TJSONMeta;
    Content: TJSONContentArray;
  End;


implementation

end.
