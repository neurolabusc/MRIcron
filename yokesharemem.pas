unit yokesharemem;
{$mode objfpc}{$H+}
interface
//http://community.freepascal.org:10000/docs-html/rtl/ipc/shmctl
// call  CreateSharedMem when an application is created and  CloseSharedMem when a program is closed
// along with NInstances, these functions return the number of concurrent instances.
// if a program crashes, the values may not be reset until the next reboot

uses
  forms, classes,
  {$IFDEF UNIX}
  BaseUnix, SysUtils, ipc,dialogs;
 {$ELSE}
   winmemmap;
 {$ENDIF}
 function CreateSharedMem (lApp: TComponent): integer; //returns number of instances after including this one...
 function CloseSharedMem: integer; //returns number of instances after after this one closes
 function NInstances: integer; //returns number of instances
 function SetShareFloats(lXmm,lYmm,lZmm: single): boolean;
 function GetShareFloats(var lXmm,lYmm,lZmm: single): boolean;

implementation

type
  TShareMem =  record
   Instances: integer;
   Xmm,Ymm,Zmm: single;
  end;
  PIntBuffer = ^TShareMem;
var
   gShareIntBuf: PIntBuffer;
   gPrevShare : TShareMem;

function NInstances: integer;
begin
      result := gShareIntBuf^.Instances;
end;

function SetShareFloats(lXmm,lYmm,lZmm: single): boolean;
begin
        gShareIntBuf^.Xmm := lXmm;
        gShareIntBuf^.Ymm := lYmm;
        gShareIntBuf^.Zmm := lZmm;
        gPrevShare :=  gShareIntBuf^;
end;

function GetShareFloats(var lXmm,lYmm,lZmm: single): boolean;
begin
        lXmm := gShareIntBuf^.Xmm;
        lYmm := gShareIntBuf^.Ymm;
        lZmm := gShareIntBuf^.Zmm;
        if (lXmm = gPrevShare.Xmm) and (lYmm = gPrevShare.Ymm) and(lZmm = gPrevShare.Zmm) then
           result := false
        else
            result := true;
        gPrevShare :=  gShareIntBuf^;
end;
{$IFNDEF UNIX} //Windows implementation
var
EMemMap : TEMemMap;

function CreateSharedMem (lApp: TComponent): integer; //returns number of instances after including this one...
var
 I: integer;
begin
  EMemMap:=TEMemMap.Create(lApp{Self});
  EMemMap.CreateMutex('MRICROMUTEX3');
  If NOT EMemMap.MapExisting('MRICROMAP3',SizeOf(TShareMem)) then begin
	gPrevShare.Xmm:=0;
	gPrevShare.Ymm:=0;
        gPrevShare.Zmm:=0;
        gPrevShare.Instances:=0;
        If NOT EMemMap.CreateMemMap('MRICROMAP2',SizeOf(TShareMem),gPrevShare) then
		EMemMap.RaiseMappingException;
         gShareIntBuf := PINtBuffer(EMemMap.MemMap);
  end else
      gShareIntBuf^.Instances := gShareIntBuf^.Instances + 1;
end;


function CloseSharedMem: integer; //returns number of instances after after this one closesb
begin
    EMemMap.Free;
end;

{$ELSE}
var
 fshmid: longint;
 segptr  : Pointer;

function CreateSharedMem (lApp: TComponent): integer; //returns number of instances after including this one...
 var
    key : Tkey;
    new: boolean;
    const ftokpath = '.'#0;
 begin
   key := ftok (pchar(@ftokpath[1]),ord('S'));
  fshmid := shmget(key,SizeOf(TShareMem) {segsize},IPC_CREAT or IPC_EXCL or 438);
  If fshmid=-1 then begin
    //showmessage('Loading existing memory.');
    new := false;
    fshmid := shmget(key,SizeOf(TShareMem){segsize},0);
    If fshmid = -1 then begin
      showmessage ('Shared memory : Error !'+inttostr(fpgeterrno));
      halt(1);
      end
    end
  else begin
    new := true;
    //showmessage ('Creating new shared memory segment.');
  end;
  segptr:=shmat(fshmid,nil,0);
  gShareIntBuf := segptr;
  if new then
     gShareIntBuf^.Instances := 1
  else
      gShareIntBuf^.Instances :=gShareIntBuf^.Instances + 1;
 result :=  gShareIntBuf^.Instances;
end;

function CloseSharedMem: integer;
//returns number of instances after this application quits
begin
  gShareIntBuf^.Instances := gShareIntBuf^.Instances -1;
  result := gShareIntBuf^.Instances;
  if Assigned (segptr) then
    shmdt (segptr);
  if result < 1 then begin //last running instance - close shared memory
    if shmctl (FShmId, IPC_RMID, nil) = -1 then
      Showmessage('unable to release shared memory');
  end;
end;
{$ENDIF}
end.

