// Disk Space Kludge for Delphi 3 for > 2 GB disk drives
// Works with Windows 95 OSR2 or later, Windows 98 or Windows 4.0 or later
// See http://msdn.microsoft.com/library/sdkdoc/winbase/filesio_8bso.htm
//
// Use DiskSpaceKludge.DiskFree and DiskSpaceKludge.DiskSize now in D3 and
// replace with equivalent SysUtils.DiskFree and SysUtils.DiskSize in D4

// efg, April 1999

UNIT DiskSpaceKludge;

INTERFACE

  USES
    Windows;    // GetDiskFreeSpace, BOOL

  TYPE
    TInteger8 = Comp;  // 8-byte integer, since disk sizes may be > 2 GB
    pInteger8 = ^TInteger8;

  // Use Delphi 4 trick from D4 SysUtils.PAS
  VAR
    GetDiskFreeSpaceEx:  FUNCTION (DirectoryName:  pChar;
                               FreeBytesAvailableToCaller:  pInteger8;
                               TotalNumberOfBytes        :  pInteger8;
                               TotalNumberOfFreeBytes    :  pInteger8):  BOOL
                               StDCall = NIL;

  FUNCTION GetDiskFreeSpaceExA (DirectoryName:  pChar;
                               FreeBytesAvailableToCaller:  pInteger8;
                               TotalNumberOfBytes        :  pInteger8;
                               TotalNumberOfFreeBytes    :  pInteger8):  BOOL;
                               StDCall;

  FUNCTION DiskFreeA(Drive: BYTE): TInteger8;
  FUNCTION DiskSize(Drive: BYTE): TInteger8;
  FUNCTION DiskFreeStr(DriveStr: String): TInteger8;
  FUNCTION DiskFreeEx (DriveStr: String): Integer;


IMPLEMENTATION

  USES
    SysUtils;   // StrCopy
function DiskFreeEx (DriveStr: String): Integer;
var  lOutDisk: Integer;  lDiskDir : string;  lSize8: Tinteger8;begin     lOutDisk := ord(upcase(DriveStr[1]))+1-ord('A');     if (lOutDisk >= ord('A')) and (lOutDisk <= ord('Z')) then begin        DiskFreeEx := DiskFree(lOutDisk);     end else begin         lDiskDir :=(ExtractFileDrive(DriveStr))+'\';         lSize8 := DiskFreeStr (lDiskDir);         if lSize8 > MaxINt then DiskFreeEx := MaxInt         else DiskFreeEx := round(lSize8);     end;end;

  ///////////////////////////////////////////////////////////////////////////

  FUNCTION GetDiskFreeSpaceExA; EXTERNAL KERNEL32 NAME 'GetDiskFreeSpaceExA';


  ///////////////////////////////////////////////////////////////////////////

  // Borland's DiskFree and DiskSize in D3 SysUtils only return a 4-byte integer.
  // Use Integer8 here so values are meaningful on large disk drives.
  // These routines may be replaced in D4 with the same name functions that
  // return Int64 values.

  // DiskFree returns the number of free bytes on the specified drive number,
  // where 0 = Current, 1 = A, 2 = B, etc. DiskFree returns -1 if the drive
  // number is invalid. }

  FUNCTION DiskFreeA(Drive: BYTE): TInteger8;
    VAR
      FreeBytesAvailableToCaller:  TInteger8;
      RootPath         :  ARRAY[0..4] OF CHAR;
      RootPtr          :  pChar;
      TotalNumberOfBytes    :  TInteger8;
  BEGIN
    RootPtr := NIL;
    IF   Drive > 0
    THEN BEGIN
      StrCopy(RootPath, 'A:\');
      RootPath[0] := CHR(Drive + ORD('A') - 1);
      StrCopy(RootPath, 'C:\');
      RootPtr     := RootPath
    END;

    // Use NIL as third parameter, just like in D4 InternalGetDiskSpace routine
    IF   GetDiskFreeSpaceEx(RootPtr,
                            @FreeBytesAvailableToCaller,
                            @TotalNumberOfBytes,
                            NIL)
    THEN RESULT := FreeBytesAvailableToCaller
    ELSE RESULT := -1
  END {DiskFree};

  FUNCTION DiskFreeStr(DriveStr: String): TInteger8;
    VAR
      FreeBytesAvailableToCaller:  TInteger8;
      RootPath         :  ARRAY[0..255] OF CHAR;
      RootPtr          :  pChar;
      TotalNumberOfBytes    :  TInteger8;
  BEGIN
//    RootPtr := NIL;
//      StrCopy(RootPath, DriveStr);
{      RootPath[0] := CHR(Drive + ORD('A') - 1);
      StrCopy(RootPath, 'C:\');}
      RootPtr     := RootPath;
      StrPCopy(RootPtr,DriveStr);

    // Use NIL as third parameter, just like in D4 InternalGetDiskSpace routine
    IF   GetDiskFreeSpaceEx(RootPtr,
                            @FreeBytesAvailableToCaller,
                            @TotalNumberOfBytes,
                            NIL)
    THEN RESULT := FreeBytesAvailableToCaller
    ELSE RESULT := -1
  END {DiskFree};


  // DiskSize returns the size in bytes of the specified drive number, where
  // 0 = Current, 1 = A, 2 = B, etc. DiskSize returns -1 if the drive number
  // is invalid. }

  FUNCTION DiskSize(Drive: BYTE): TInteger8;
     VAR
      FreeBytesAvailableToCaller:  TInteger8;
      RootPath         :  ARRAY[0..4] OF CHAR;
      RootPtr          :  pChar;
      TotalNumberOfBytes    :  TInteger8;
  BEGIN
    RootPtr := NIL;
    IF   Drive > 0
    THEN BEGIN
      StrCopy(RootPath, 'A:\');
      RootPath[0] := CHR(Drive + ORD('A') - 1);
      RootPtr     := RootPath
    END;

    // Use NIL as third parameter, just like in D4 InternalGetDiskSpace routine
    IF   GetDiskFreeSpaceEx(RootPtr,
                            @FreeBytesAvailableToCaller,
                            @TotalNumberOfBytes,
                            NIL)
    THEN RESULT := TotalNumberOfBytes
    ELSE RESULT := -1
  END {DiskSize};


  ///////////////////////////////////////////////////////////////////////////

  // Equivalent to Delphi 4 SysUtils.PAS routines
  FUNCTION BackfillGetDiskFreeSpaceEx(Directory:  pChar;
    VAR FreeAvailable, TotalSpace:  TInteger8;
    TotalFree:  pInteger8):  BOOL;  StdCall;

    VAR
      BytesPerSector   :  DWORD;
      Dir              :  pChar;
      FreeClusters     :  DWORD;
      SectorsPerCluster:  DWORD;
      Temp             :  TInteger8;
      TotalClusters    :  DWORD;
  BEGIN
    IF   Directory <> NIL
    THEN Dir := Directory
    ELSE Dir := NIL;

    RESULT := GetDiskFreeSpaceA(Dir, SectorsPerCluster, BytesPerSector,
                                FreeClusters, TotalClusters);
    Temp := SectorsPerCluster * BytesPerSector;
    FreeAvailable := Temp * FreeClusters;
    TotalSpace := Temp * TotalClusters
  END {BackfillGetDiskFreeSpaceEx};


  PROCEDURE InitializeDriveSpacePointer;
    VAR
      Kernel:  THandle;
  BEGIN
    Kernel := GetModuleHandle(Windows.Kernel32);

    IF   Kernel <> 0
    THEN @GetDiskFreeSpaceEx := GetProcAddress(Kernel, 'GetDiskFreeSpaceExA');

    IF  NOT Assigned(GetDiskFreespaceEx)
    THEN GetDiskFreeSpaceEx := @BackFillGetDiskFreeSpaceEx
  END {InitializeDriveSpacePointer};

INITIALIZATION
  InitializeDriveSpacePointer
END.
