unit admin;
//For Win95,98,ME all users are administrators. However, with WinNT/XP this is not the case
//non-Administrators do not have access to c;|program files, therefore we will save ini files in their home folder
interface

function IsAdmin: Boolean;
function AppDataFolder: string;
implementation

uses
  SysUtils, Windows,shlobj;


function AppDataFolder: string;  //uses shlobj
var
Path : pchar;
idList : PItemIDList;
begin
GetMem(Path, MAX_PATH);
SHGetSpecialFolderLocation(0, CSIDL_APPDATA {CSIDL_PERSONAL}, idList);
SHGetPathFromIDList(idList, Path);
Result := string(Path);
FreeMem(Path);
end;

function IsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS     = $00000220;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
  begin
    Result := True;
    exit;
  end;

  Result := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True,
    hAccessToken);
  if not bSuccess then
  begin
    if GetLastError = ERROR_NO_TOKEN then
    bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
      hAccessToken);
  end;
  if bSuccess then
  begin
    GetMem(ptgGroups, 1024);
    bSuccess := GetTokenInformation(hAccessToken, TokenGroups,
      ptgGroups, 1024, dwInfoBufferSize);
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
        0, 0, 0, 0, 0, 0, psidAdministrators);
      {$R-}
      for x := 0 to ptgGroups.GroupCount - 1 do
        if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
        begin
          Result := True;
          break;
        end;
      {$R+}
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
end;

end.