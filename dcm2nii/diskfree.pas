unit diskfree;

{
  Copyright 2001, by Michael Hopper

  This unit provides generic functions for finding the number of bytes and
  available bytes for a given drive volume or path.  These functions work for
  both Linux and Windows systems.

  Note:  In the near future, getmntent should be incorporated into the Linux
  version of the code rather than reading the /etc/mtab file directly.  This
  is for future compatibility.
}

interface

uses
  SysUtils;

{$IFDEF LINUX}

{ This function is included (necessary) only in the Linux version because the
  Windows version is a little more intuitive, i.e., 1=A:, 2=B:, etc.
}
function GetVolumePath (Volume : integer) : TFileName;
{$ENDIF}

{ This overloaded function returns the number of available bytes for the
  specified path or volume.  I've mimicked DiskFree in that Volume=0 indicates
  the current directory (or pwd), while values greater than 0 return a value for
  the drives A: through Z: on Windows systems.

  For Linux systems, the /etc/mtab file is read, and only those devices mounted
  under /dev are alphabetized and included.  This means that a value of 1 would
  return the free space on /dev/fd0 if a floppy is mounted, or /dev/hda1 if a
  floppy is not mounted.  Hence the need for the GetVolumePath function to
  verify that you're getting the right path.
}
function GetAvailableFreeSpace(path : TFilename) : int64; overload;
function GetAvailableFreeSpace(Volume : byte) : int64; overload;

{  This overloaded function returns the total number of bytes for the volume or
   path specified.  Specification of the path or volume follows the same pattern
   as for the GetAvailableFreeSpace function.
}
function GetVolumeBytes(path : TFilename) : int64; overload;
function GetVolumeBytes(Volume : byte) : int64; overload;

implementation

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

{$IFDEF LINUX}
uses
  Classes,libc;

const
  MAX_PATH = 65536;
{$ENDIF}

{$IFDEF MSWINDOWS}
function oldWindows : boolean;

{
  Use GetVersionEx windows call as defined in SysUtils and explained in the
  Win32 help file to return true if this is a version of Win95 that is pre-OSR2.
  Otherwise, return false.
}

var
  MyVersionInfo : _OSVERSIONINFOA;

begin

// Get Windows version information
getversionex(MyVersionInfo);

// Report TRUE only if Platform is Win9x and BuildNumber is < 1000 (pre-OSR2).
with MyVersionInfo do
  begin
  result := ((dwPlatformID = VER_PLATFORM_WIN32_WINDOWS) and
            ((dwBuildNumber and $0000FFFF) < 1000))
  end;
end;
{$ENDIF}



{$IFDEF LINUX}
function GetVolumePath (Volume : integer) : TFileName;

{
  Return the path of the Volume specified.  If Volume = 0, return the present
  present working directory.
}

var
  loop          : integer;
  tempstr       : string;
  mountedDrives : TStringlist;


begin
// return the present working directory if Volume = 0.
if Volume = 0 then
  result := getenvironmentvariable('PWD')
else
  begin
  // Get information about currently mounted drives.
  mountedDrives := tstringlist.create;
  mountedDrives.LoadFromFile('/etc/mtab');

  // Alphabetize
  mountedDrives.Sort;

  loop := 0;

  // Loop to remove entries that are not drives
  while loop < mountedDrives.Count do
    begin
    if pos('/dev/',mountedDrives.Strings[loop]) <> 1 then
      mountedDrives.Delete(loop)
    else
      inc(loop);
    end;

  // If there are not enough mounted drives, then return a null string.
  if Volume > mountedDrives.Count then
    result := ''
  else // parse the entry to return the mounted path.
    begin
    tempstr := mountedDrives.Strings[Volume-1];
    tempstr := copy(tempstr,pos(' ',tempstr) + 1,length(tempstr));
    tempstr := copy(tempstr,1,pos(' ',tempstr)-1);
    result  := tempstr;
    end;
  end;
end;
{$ENDIF}



function CalculateVolumeSpace(path : pchar; var AvailableBytes : int64; var TotalBytes : int64) : boolean;

{
  This function calculates and returns the number of available bytes and total
  bytes for a given path.  It also returns a true value if successful, or false
  if unsuccessful at determining the correct values.  If the function returns
  false, then the AvailableBytes and TotalBytes values should be treated as
  though they are undefined.
}

var
{$IFDEF MSWINDOWS}
  SectorsPerCluster,
  BytesPerSector,
  NumberOfFreeClusters,
  TotalNumberOfClusters : cardinal;
{$ENDIF}
{$IFDEF LINUX}
  myStatFs              : Tstatfs;
{$ENDIF}

begin
{$IFDEF MSWINDOWS}

// Check for pre-OSR2 Win95 because the GetDiskFreeSpace function was not
// available.
if not oldWindows then
  result := GetDiskFreeSpaceEx(path,AvailableBytes,TotalBytes,nil)

// If pre-OSR2 Win95, then calculate the free space.
else if GetDiskFreeSpace(path,SectorsPerCluster,BytesPerSector,NumberOfFreeClusters,TotalNumberOfClusters) then
  begin
  result := true;
  AvailableBytes := BytesPerSector * SectorsPerCluster * NumberOfFreeClusters;
  TotalBytes := BytesPerSector * SectorsPerCluster * TotalNumberOfClusters;
  end

// If unsuccessful at either of the previous attempts, report failure.
else
  begin
  result := false;
  AvailableBytes := -1;
  TotalBytes := -1;
  end;
{$ENDIF}

{$IFDEF LINUX}

// Read information about the volume.
if statfs(path,myStatFS) = 0 then
  begin

  // Calculate AvailableBytes and TotalBytes.
  AvailableBytes := int64(myStatFs.f_bAvail) * int64(myStatFs.f_bsize);
  TotalBytes := int64(myStatFs.f_blocks) * int64(myStatFs.f_bsize);

  // Report success.
  result := true;
  end

else // report failure to get information
  begin
  AvailableBytes := -1;
  TotalBytes := -1;
  result := false;
  end;
{$ENDIF}
end;



function GetAvailableFreeSpace(path : TFilename) : int64; overload;

{
  Use the CalculateVolumeSpace function to find the free space for the path
  specified.
}

var
  TotalBytes      : int64;
  pathPchar       : pchar;

begin
{$IFDEF MSWINDOWS}
if ByteType(path,1) = mbSingleByte then
  getmem(pathPchar,length(path) + 1)
else
  getmem(pathPchar,2 * (length(path) + 1));
strpcopy(pathpchar,path);
if not CalculateVolumeSpace(pathPchar,result,TotalBytes) then
  result := -1;
{$ENDIF}

{$IFDEF LINUX}
getmem(pathPchar,(length(path) + 1) * sizeof(char));
strpcopy(pathPchar,path);
if not CalculateVolumeSpace(pathPchar,result,TotalBytes) then
  result := -1; // stub
{$ENDIF}
end;


function GetAvailableFreeSpace(Volume : byte) : int64; overload;

{
  Use the CalculateVolumeSpace function to find the free space for the Volume
  specified.
}

var
  volumePath  : array [1..MAX_PATH + 1] of char;
  volumePchar : pchar;
  TotalSpace : int64;


begin
{$IFDEF MSWINDOWS}
volumePchar := @volumePath;
if Volume = 0 then
  getcurrentdirectory(MAX_PATH + 1,volumePchar)
else
  strPCopy(volumePchar,chr(Volume - 1 + ord('A')) + ':\');
if not CalculateVolumeSpace(volumePchar, result, TotalSpace) then
  result := -1;
{$ENDIF}

{$IFDEF LINUX}
volumePchar := @volumePath;
if Volume = 0 then
  strPCopy(volumePchar,getEnvironmentVariable('PWD'))
else
  strPCopy(volumePChar,GetVolumePath(Volume));
if not CalculateVolumeSpace(volumePchar, result, TotalSpace) then
  result := -1;
{$ENDIF}
end;



function GetVolumeBytes(path : TFilename) : int64; overload;

{
  Use the CalculateVolumeSpace function to find the total space for the path
  specified.
}

var
  AvailableBytes  : int64;
  pathPchar       : pchar;

begin
{$IFDEF MSWINDOWS}
if ByteType(path,1) = mbSingleByte then
  getmem(pathPchar,length(path) + 1)
else
  getmem(pathPchar,2 * (length(path) + 1));
strpcopy(pathpchar,path);
if not CalculateVolumeSpace(pathPchar,AvailableBytes,result) then
  result := -1;
{$ENDIF}

{$IFDEF LINUX}
getmem(pathPchar,(length(path) + 1) * sizeof(char));
strpcopy(pathPchar,path);
if not CalculateVolumeSpace(pathPchar,AvailableBytes, result) then
  result := -1;
{$ENDIF}
end;



function GetVolumeBytes(Volume : byte) : int64; overload;

{
  Use the CalculateVolumeSpace function to find the total space for the Volume
  specified.
}

var
  volumePath     : array [1..MAX_PATH + 1] of char;
  volumePchar    : pchar;
  AvailableBytes : int64;


begin
{$IFDEF MSWINDOWS}
volumePchar := @volumePath;
if Volume = 0 then
  getcurrentdirectory(MAX_PATH + 1,volumePchar)
else
  strPCopy(volumePchar,chr(Volume - 1 + ord('A')) + ':\');
if not CalculateVolumeSpace(volumePchar, AvailableBytes, result) then
  result := -1;
{$ENDIF}

{$IFDEF LINUX}
volumePchar := @volumePath;
if Volume = 0 then
  strPCopy(volumePchar,getEnvironmentVariable('PWD'))
else
  strPCopy(volumePChar,GetVolumePath(Volume));
if not CalculateVolumeSpace(volumePchar, AvailableBytes, result) then
  result := -1;
{$ENDIF}
end;

end.
