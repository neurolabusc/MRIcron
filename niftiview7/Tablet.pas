unit Tablet;

interface
uses
  Windows,  SysUtils,Forms,WinTab32;

//procedure InitializeTablet;
//procedure FinalizeTablet;
procedure TabletState(var lPressurePct: integer; var lErase: boolean);
function TabletAvailable: boolean;

implementation
var
    FTablet: HCTX;
    FMaxNPressure: integer;

function TabletAvailable: boolean;
begin
     result := false;
  if FTablet = 0 then
     exit;
  if not IsWinTab32Available  then
     exit;
     result := true;
end;

procedure TabletState(var lPressurePct: integer; var lErase: boolean);
var
  buf: array[0..31] of PACKET;
  n,p,i: integer;
begin
  lPressurePct := -1;
  lErase := false;
  if FTablet = 0 then
     exit;
  if not IsWinTab32Available  then
     exit;
  n := WTPacketsGet(FTablet, 32, @buf);
  if n = 0 then
     exit;

repeat
      i :=  n-1;
      p := (buf[i].pkNormalPressure );
      lErase := (2 = buf[i].pkCursor);
  n := WTPacketsGet(FTablet, 32, @buf);
until n = 0;
    lPressurePct := round(p /FMaxNPressure * 100);
end;

procedure FinalizeTablet;
begin
  if FTablet <> 0 then begin
    WTClose(FTablet);
    FTablet := 0;
  end;
end;

procedure InitializeTablet;
var
  lc: LOGCONTEXT;
  npAxis: AXIS;
begin
  if not IsWinTab32Available then Exit;
  // get default
  WTInfo(WTI_DEFSYSCTX, 0, @lc);
  // modify the digitizing region
  StrCopy(lc.lcName, PChar('PaintWindow '+IntToHex(HInstance, 8)));
  lc.lcOptions := lc.lcOptions or CXO_SYSTEM;
  lc.lcMsgBase := WT_DEFBASE;
  lc.lcPktData := PACKETDATA;
  lc.lcPktMode := PACKETMODE;
  lc.lcMoveMask := PACKETDATA;
  lc.lcBtnUpMask := lc.lcBtnDnMask;
  lc.lcOutExtX := lc.lcOutExtX * 10;
  lc.lcOutExtY := lc.lcOutExtY * 10;
  FTablet := WTOpen(Application.Handle, lc, TRUE);
  WTInfo(WTI_DEVICES + lc.lcDevice, DVC_NPRESSURE, @npAxis);
  FMaxNPressure := npAxis.axMax;
end;

initialization
  InitializeTablet;
finalization
  FinalizeTablet;
end.
