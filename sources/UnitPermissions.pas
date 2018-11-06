unit UnitPermissions;

interface

type
  TAndroidPermissions = class(TObject)
  public
    const CAMERA = 'android.permission.CAMERA';
    const RECORD_AUDIO = 'android.permission.RECORD_AUDIO';
    const READ_CONTACTS = 'android.permission.READ_CONTACTS';
    const READ_PHONE_STATE = 'android.permission.READ_PHONE_STATE';
  public
    class function CheckPermission(const APermissionName: string): Boolean; static;
  end;

implementation

uses
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge, Androidapi.Helpers;

type
  JPermissionCheckerClass = interface(JObjectClass)
    ['{FC8D7A0B-078A-4DBB-B115-17E6F0E7CA18}']
    {class} function _GetPERMISSION_DENIED: Integer; cdecl;
    {class} function _GetPERMISSION_DENIED_APP_OP: Integer; cdecl;
    {class} function _GetPERMISSION_GRANTED: Integer; cdecl;
    {class} function checkCallingOrSelfPermission(P1: JContext; P2: JString): Integer; cdecl;
    {class} function checkCallingPermission(P1: JContext; P2: JString; P3: JString): Integer; cdecl;
    {class} function checkPermission(P1: JContext; P2: JString; P3: Integer; P4: Integer; P5: JString): Integer; cdecl;
    {class} function checkSelfPermission(P1: JContext; P2: JString): Integer; cdecl;
    {class} property PERMISSION_DENIED: Integer read _GetPERMISSION_DENIED;
    {class} property PERMISSION_DENIED_APP_OP: Integer read _GetPERMISSION_DENIED_APP_OP;
    {class} property PERMISSION_GRANTED: Integer read _GetPERMISSION_GRANTED;
  end;

  [JavaSignature('android/support/v4/content/PermissionChecker')]
  JPermissionChecker = interface(JObject)
    ['{AF30C4BE-CF3B-4AE0-A274-D96FBC08728E}']
  end;
  TJPermissionChecker = class(TJavaGenericImport<JPermissionCheckerClass, JPermissionChecker>) end;

{ TAndroidPermissions }

class function TAndroidPermissions.CheckPermission(const APermissionName: string): Boolean;
begin
  Result := TJPermissionChecker.JavaClass.checkSelfPermission(TAndroidHelper.Context, StringToJString(APermissionName))
    = TJPermissionChecker.JavaClass.PERMISSION_GRANTED;
end;

end.