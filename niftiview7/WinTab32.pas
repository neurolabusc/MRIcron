unit WinTab32;
{
  WinTab interface for delphi

  WinTab is standardized programming interface to digitizing tablets,
  three dimensional position sensors, and other pointing devices
  by a group of leading digitizer manufacturers and applications developers.

  converted from wintab.h, pktdef.h from www.pointing.com

  The manager part is omitted for not being widely used or supported.
  Dynamic link is used so as not to prevent programs to run without a tablet installed.
  Detailed documents can be downloaded from www.pointing.com.

  Note: Modify definations of PACKETDATA and PACKET to define your own data format.

  by LI Qingrui
  emailto: the3i@sohu.com

  This file is supplied "AS IS", without warranty of any kind.
  Feel free to use and modify for any purpose.
  Enjoy yourself.
}

interface

uses Windows, Messages;

const // Message constants
  WT_DEFBASE = $7FF0;
  WT_MAXOFFSET = $F;
  WT_PACKET = WT_DEFBASE + 0;
  WT_CTXOPEN = WT_DEFBASE + 1;
  WT_CTXCLOSE = WT_DEFBASE + 2;
  WT_CTXUPDATE = WT_DEFBASE + 3;
  WT_CTXOVERLAP = WT_DEFBASE + 4;
  WT_PROXIMITY = WT_DEFBASE + 5;
  WT_INFOCHANGE = WT_DEFBASE + 6;
  WT_CSRCHANGE = WT_DEFBASE + 7;
  WT_MAX = WT_DEFBASE + WT_MAXOFFSET;

// Common data types

type
  HCTX = THandle; // context handle
  WTPKT = Longword; // packet mask

const  // Packet constants
       // PACKET DEFINITION
       // The following definition controls what data items are requested from the Tablet during the
       // "WTOpen" command. Note that while some tablets will open with all data items requested
       // (i.e. X, Y, Z, and Pressure information), some tablets will not open if they do not support
       // a particular data item. For example, the GTCO Sketchmaster driver will fail on "WTOpen" if
       // you request Z data or Pressure data. However, the SummaSketch driver will succeed on open
       // even though Z and Pressure are not supported by this tablet. In this case, 0 is returned for
       // the Z and Pressure data, as you might expect.

  PK_CONTEXT            = $1; // reporting context
  PK_STATUS             = $2; // status bits
  PK_TIME               = $4; // time stamp
  PK_CHANGED            = $8; // change bit vector
  PK_SERIAL_NUMBER      = $10; // packet serial number
  PK_CURSOR             = $20; // reporting cursor
  PK_BUTTONS            = $40; // button information
  PK_X                  = $80; // x axis
  PK_Y                  = $100; // y axis
  PK_Z                  = $200; // z axis
  PK_NORMAL_PRESSURE    = $400; // normal or tip pressure
  PK_TANGENT_PRESSURE   = $800; // tangential or barrel pressure
  PK_ORIENTATION        = $1000; // orientation info: tilts
  PK_ROTATION           = $2000; // rotation info; 1.1

  // this constant is used to define PACKET record
  PACKETDATA = PK_CURSOR or {PK_X or PK_Y or} PK_NORMAL_PRESSURE;

  // this constant is used to define PACKET record
  PACKETMODE = 0; //This means all values are reported "absoulte"

type

  // Modify this to suit your needs.
  PACKET = record
//    pkContext: HCTX; // PK_CONTEXT
//    pkStatus: Cardinal; // PK_STATUS
//    pkTime: Longword; // PK_TIME
//    pkChanged: WTPKT; // PK_CHANGED
//    pkSerialNumber: cardinal; // PK_SERIAL_NUMBER
    pkCursor: cardinal; // PK_CURSOR
//    pkButtons: Longword; // PK_BUTTONS
//    pkX: LongInt; // PK_X
//    pkY: LongInt; // PK_Y
//    pkZ: LongInt; // PK_Z
    pkNormalPressure: integer; // PK_NORMAL_PRESSURE
//    pkTangentPressure: integer; // PK_TANGENT_PRESSURE
//    pkOrientation: ORIENTATION; // PK_ORIENTATION
//    pkRotation: ROTATION; // PK_ROTATION  Ver 1.1
  end;

type FIX32 = Longword;


// Info data defs

const  // unit specifiers
  TU_NONE = 0;
  TU_INCHES = 1;
  TU_CENTIMETERS = 2;
  TU_CIRCLE = 3;

type
  AXIS = record
    axMin: LongInt;
    axMax: LongInt;
    axUnits: Cardinal;
    axResolution: FIX32;
  end;
  PAXIS = ^AXIS;

const // system button assignment values
  SBN_NONE	      =	$00;
  SBN_LCLICK	      =	$01;
  SBN_LDBLCLICK	      = $02;
  SBN_LDRAG	      =	$03;
  SBN_RCLICK	      =	$04;
  SBN_RDBLCLICK	      = $05;
  SBN_RDRAG	      =	$06;
  SBN_MCLICK	      = $07;
  SBN_MDBLCLICK       = $08;
  SBN_MDRAG	      =	$09;

const // hardware capabilities
  HWC_INTEGRATED      =	$0001;
  HWC_TOUCH	      =	$0002;
  HWC_HARDPROX	      =	$0004;
  HWC_PHYSID_CURSORS  =	$0008; // 1.1

const // cursor capabilities
  CRC_MULTIMODE	      = $0001; // 1.1
  CRC_AGGREGATE	      = $0002; // 1.1
  CRC_INVERT	      =	$0004; // 1.1

const // info categories
  WTI_INTERFACE	      =	1;
  IFC_WINTABID	      =	1;
  IFC_SPECVERSION     =	2;
  IFC_IMPLVERSION     =	3;
  IFC_NDEVICES	      =	4;
  IFC_NCURSORS	      =	5;
  IFC_NCONTEXTS	      =	6;
  IFC_CTXOPTIONS      =	7;
  IFC_CTXSAVESIZE     =	8;
  IFC_NEXTENSIONS     =	9;
  IFC_NMANAGERS	      =	10;
  IFC_MAX	      =	10;

  WTI_STATUS	      =	2;
  STA_CONTEXTS	      =	1;
  STA_SYSCTXS	      =	2;
  STA_PKTRATE	      =	3;
  STA_PKTDATA	      =	4;
  STA_MANAGERS	      =	5;
  STA_SYSTEM	      =	6;
  STA_BUTTONUSE	      =	7;
  STA_SYSBTNUSE	      =	8;
  STA_MAX	      =	8;

  WTI_DEFCONTEXT      =	3;
  WTI_DEFSYSCTX	      = 4;
  WTI_DDCTXS	      =	400; // 1.1
  WTI_DSCTXS	      =	500; // 1.1
  CTX_NAME	      =	1;
  CTX_OPTIONS	      =	2;
  CTX_STATUS	      =	3;
  CTX_LOCKS	      =	4;
  CTX_MSGBASE	      =	5;
  CTX_DEVICE	      =	6;
  CTX_PKTRATE	      =	7;
  CTX_PKTDATA	      =	8;
  CTX_PKTMODE	      =	9;
  CTX_MOVEMASK	      = 10;
  CTX_BTNDNMASK	      = 11;
  CTX_BTNUPMASK	      = 12;
  CTX_INORGX	      =	13;
  CTX_INORGY	      =	14;
  CTX_INORGZ	      =	15;
  CTX_INEXTX	      =	16;
  CTX_INEXTY	      =	17;
  CTX_INEXTZ	      =	18;
  CTX_OUTORGX	      =	19;
  CTX_OUTORGY	      =	20;
  CTX_OUTORGZ	      =	21;
  CTX_OUTEXTX	      =	22;
  CTX_OUTEXTY	      =	23;
  CTX_OUTEXTZ	      =	24;
  CTX_SENSX	      =	25;
  CTX_SENSY	      =	26;
  CTX_SENSZ	      =	27;
  CTX_SYSMODE	      =	28;
  CTX_SYSORGX	      =	29;
  CTX_SYSORGY	      =	30;
  CTX_SYSEXTX	      =	31;
  CTX_SYSEXTY	      =	32;
  CTX_SYSSENSX	      = 33;
  CTX_SYSSENSY	      = 34;
  CTX_MAX	      =	34;

  WTI_DEVICES	      =	100;
  DVC_NAME	      =	1;
  DVC_HARDWARE	      =	2;
  DVC_NCSRTYPES	      =	3;
  DVC_FIRSTCSR	      =	4;
  DVC_PKTRATE	      =	5;
  DVC_PKTDATA	      =	6;
  DVC_PKTMODE	      =	7;
  DVC_CSRDATA	      =	8;
  DVC_XMARGIN	      =	9;
  DVC_YMARGIN	      =	10;
  DVC_ZMARGIN	      =	11;
  DVC_X		      =	12;
  DVC_Y		      =	13;
  DVC_Z		      =	14;
  DVC_NPRESSURE	      =	15;
  DVC_TPRESSURE	      =	16;
  DVC_ORIENTATION     =	17;
  DVC_ROTATION	      =	18; // 1.1
  DVC_PNPID	      =	19; // 1.1
  DVC_MAX	      =	19;

  WTI_CURSORS	      =	200;
  CSR_NAME	      =	1;
  CSR_ACTIVE	      =	2;
  CSR_PKTDATA	      =	3;
  CSR_BUTTONS	      =	4;
  CSR_BUTTONBITS      =	5;
  CSR_BTNNAMES	      =	6;
  CSR_BUTTONMAP	      =	7;
  CSR_SYSBTNMAP	      =	8;
  CSR_NPBUTTON	      =	9;
  CSR_NPBTNMARKS      =	10;
  CSR_NPRESPONSE      =	11;
  CSR_TPBUTTON	      =	12;
  CSR_TPBTNMARKS      =	13;
  CSR_TPRESPONSE      =	14;
  CSR_PHYSID	      =	15; // 1.1
  CSR_MODE	      =	16; // 1.1
  CSR_MINPKTDATA      =	17; // 1.1
  CSR_MINBUTTONS      =	18; // 1.1
  CSR_CAPABILITIES    =	19; // 1.1
  CSR_MAX	      =	19;

  WTI_EXTENSIONS      =	300;
  EXT_NAME	      =	1;
  EXT_TAG	      =	2;
  EXT_MASK	      =	3;
  EXT_SIZE	      =	4;
  EXT_AXES	      =	5;
  EXT_DEFAULT	      =	6;
  EXT_DEFCONTEXT      =	7;
  EXT_DEFSYSCTX	      = 8;
  EXT_CURSORS	      =	9;
  EXT_MAX	      =	109; // Allow 100 cursors


// Context data defs

const
  LCNAMELEN = 40;
  LC_NAMELEN = 40;

  // context option values
  CXO_SYSTEM	      =	$0001; // the context is a system cursor context
  CXO_PEN	      = $0002; // the context is a PenWin context, also a system cursor context
  CXO_MESSAGES	      = $0004; // the context sends WT_PACKET messages to its owner
  CXO_MARGIN	      =	$8000; // the margin is an area outside the input area where events will be mapped to the edge of the input area
  CXO_MGNINSIDE	      = $4000; // the margin will be inside the specified context
  CXO_CSRMESSAGES     =	$0008; // 1.1 sends WT_CSRCHANGE messages
  // context status values
  CXS_DISABLED	      = $0001;
  CXS_OBSCURED	      = $0002;
  CXS_ONTOP	      = $0004;
  // context lock values
  CXL_INSIZE	      =	$0001; // the context's input size cannot be changed
  CXL_INASPECT	      = $0002; // the context's input aspect ratio cannot be changed
  CXL_SENSITIVITY     =	$0004; // the context's sensitivity settings for x, y, and z cannot be changed
  CXL_MARGIN	      =	$0008; // the context's margin options cannot be changed
  CXL_SYSOUT	      =	$0010; // If the context is a system cursor context, the value specifies that the system pointing control variables of the context cannot be changed

type
  LOGCONTEXT = record
    lcName: array[0..LCNAMELEN-1] of char; // context name string
    lcOptions, // unsupported option will cause WTOpen to fail
    lcStatus,
    lcLocks, // specify attributes of the context that cannot be changed once the context has been opened
    lcMsgBase,
    lcDevice, // device whose input the context processes
    lcPktRate: cardinal; // desired packet report rate in Hertz. returns the actual report rate.
    lcPktData, // which optional data items will be in packets. unsupported items will cause WTOpen to fail.
    lcPktMode, // whether the packet data items will be returned in absolute or relative mode
    lcMoveMask: WTPKT; // which packet data items can generate move events in the context
    lcBtnDnMask, // buttons for which button press events will be processed in the context
    lcBtnUpMask: Longword; // buttons for which button release events will be processed in the context
    lcInOrgX,
    lcInOrgY,
    lcInOrgZ, // origin of the context's input area in the tablet's native coordinates
    lcInExtX,
    lcInExtY,
    lcInExtZ, // extent of the context's input area in the tablet's native coordinates
    lcOutOrgX,
    lcOutOrgY,
    lcOutOrgZ, // origin of the context's output area in context output coordinates, absolute mode only
    lcOutExtX,
    lcOutExtY,
    lcOutExtZ: LongInt; // extent of the context's output area in context output coordinates, absolute mode only
    lcSensX,
    lcSensY,
    lcSensZ: FIX32; // specifies the relative-mode sensitivity factor
    lcSysMode: LongBool; // system cursor tracking mode. Zero specifies absolute; non-zero means relative
    lcSysOrgX,
    lcSysOrgY, // the origin of the screen mapping area for system cursor tracking, in screen coordinates
    lcSysExtX,
    lcSysExtY: integer; // the extent of the screen mapping area for system cursor tracking, in screen coordinates
    lcSysSensX,
    lcSysSensY: FIX32; // specifies the system-cursor relative-mode sensitivity factor for the x and y axes
  end;
  PLOGCONTEXT = ^LOGCONTEXT;


// Event data defs
const // packet status values
  TPS_PROXIMITY         = $0001;
  TPS_QUEUE_ERR		= $0002;
  TPS_MARGIN		= $0004;
  TPS_GRAB		= $0008;
  TPS_INVERT		= $0010; // 1.1

type
  ORIENTATION = record
    orAzimuth: integer;
    orAltitude: integer;
    orTwist: integer;
  end;
  PORIENTATION = ^ORIENTATION;

  ROTATION = record // 1.1
    roPitch: integer;
    roRoll: integer;
    roYaw: integer;
  end;
  PROTATION = ^ROTATION;

const // relative buttons
  TBN_NONE = 0;
  TBN_UP = 1;
  TBN_DOWN = 2;


// device config constants

const
  WTDC_NONE = 0;
  WTDC_CANCEL = 1;
  WTDC_OK = 2;
  WTDC_RESTART = 3;


// PREFERENCE FUNCTION CONSTANTS

const
  WTP_LPDEFAULT: Pointer = Pointer(-1);
  WTP_DWDEFAULT: Longword = Longword(-1);


// functions
var
  // Used to read various pieces of information about the tablet.
  WTInfo: function (wCategory, nIndex: Cardinal; lpOutput: Pointer): Cardinal; stdcall;

  // Used to begin accessing the Tablet.
  WTOpen: function (hw: HWnd; var lc: LOGCONTEXT; fEnable: LongBool): HCTX; stdcall;

  // Fills the supplied structure with the current context attributes for the passed handle.
  WTGet: function (hc: HCTX; var lc: LOGCONTEXT): LongBool; stdcall;

  // Allows some of the context's attributes to be changed on the fly.
  WTSet: function (hc: HCTX; const lc: LOGCONTEXT): LongBool; stdcall;

  // Used to end accessing the Tablet.
  WTClose: function (hc: HCTX): LongBool; stdcall;

  // Used to poll the Tablet for input.
  WTPacketsGet: function (hc: HCTX; cMaxPackets: Integer; lpPkts: Pointer): Integer; stdcall;

  // Similar to WTPacketsGet but is used in a window function.
  WTPacket: function (hc: HCTX; wSerial: Cardinal; lpPkts: Pointer): LongBool; stdcall;

  // Visibility Functions

  // Enables and Disables a Tablet Context, temporarily turning on or off the processing of packets.
  WTEnable: function (hc: HCTX; fEnable: LongBool): LongBool; stdcall;

  // Sends a tablet context to the top or bottom of the order of overlapping tablet contexts.
  WTOverlap: function (hc: HCTX; fToTop: LongBool): LongBool; stdcall;

  // Context Editing Functions

  // Used to call a requestor which aids in configuring the Tablet
  WTConfig: function (hc: HCTX; hw: HWnd): LongBool; stdcall;

  WTExtGet: function (hc: HCTX; wExt: cardinal; lpData: Pointer): LongBool; stdcall;

  WTExtSet: function (hc: HCTX; wExt: cardinal; lpData: Pointer): LongBool; stdcall;

  // Fills the supplied buffer with binary save information that can be used to restore the equivalent context in a subsequent Windows session.
  WTSave: function (hc: HCTX; lpSaveInfo: Pointer): LongBool; stdcall;

  // Creates a tablet context from the save information returned from the WTSave function.
  WTRestore: function (hw: HWnd; lpSaveInfo: Pointer; fEnable: LongBool): HCTX; stdcall;

  // Advanced Packet and Queue Functions

  WTPacketsPeek: function (hc: HCTX; cMaxPackets: Integer; lpPkts: Pointer): Integer; stdcall;
  WTDataGet: function (hc: HCTX; wBegin, wEnding: cardinal; cMaxPackets: Integer; lpPkts: Pointer; var lpNPkts: Integer): Integer; stdcall;
  WTDataPeek: function (hc: HCTX; wBegin, wEnding: cardinal; cMaxPackets: Integer; lpPkts: Pointer; var lpNPkts: Integer): Integer; stdcall;
  // Returns the serial numbers of the oldest and newest packets currently in the queue.
  WTQueuePacketsEx: function (hc: HCTX; var lpOld, lpNew: cardinal): LongBool; stdcall;
  WTQueueSizeGet: function (hc: HCTX): Integer; stdcall;
  WTQueueSizeSet: function (hc: HCTX; nPkts: Integer): LongBool; stdcall;

function IsWinTab32Available: boolean;

implementation

var lib: THandle;

function IsWinTab32Available: boolean;
begin
  result := lib <> 0;
end;

function LoadWinTab32: boolean;
begin
  result := true;
  if lib <> 0 then Exit;
  lib := LoadLibrary('wintab32.dll');
  if lib = 0 then
  begin
    result := false;
    Exit;
  end;
  WTInfo := GetProcAddress(lib, 'WTInfoA');
  WTOpen := GetProcAddress(lib, 'WTOpenA');
  WTClose := GetProcAddress(lib, 'WTClose');
  WTPacketsGet := GetProcAddress(lib, 'WTPacketsGet');
  WTPacket := GetProcAddress(lib, 'WTPacket');
  WTEnable := GetProcAddress(lib, 'WTEnable');
  WTOverlap := GetProcAddress(lib, 'WTOverlap');
  WTConfig := GetProcAddress(lib, 'WTConfig');
  WTGet := GetProcAddress(lib, 'WTGetA');
  WTSet := GetProcAddress(lib, 'WTSetA');
  WTExtGet := GetProcAddress(lib, 'WTExtGet');
  WTExtSet := GetProcAddress(lib, 'WTExtSet');
  WTSave := GetProcAddress(lib, 'WTSave');
  WTRestore := GetProcAddress(lib, 'WTRestore');
  WTPacketsPeek := GetProcAddress(lib, 'WTPacketsPeek');
  WTDataGet := GetProcAddress(lib, 'WTDataGet');
  WTDataPeek := GetProcAddress(lib, 'WTDataPeek');
  WTQueuePacketsEx := GetProcAddress(lib, 'WTQueuePacketsEx');
  WTQueueSizeGet := GetProcAddress(lib, 'WTQueueSizeGet');
  WTQueueSizeSet := GetProcAddress(lib, 'WTQueueSizeSet');
end;

procedure UnloadWinTab32;
begin
  if lib <> 0 then
  begin
    FreeLibrary(lib);
    lib := 0;
  WTInfo := nil;
  WTOpen := nil;
  WTClose := nil;
  WTPacketsGet := nil;
  WTPacket := nil;
  WTEnable := nil;
  WTOverlap := nil;
  WTConfig := nil;
  WTGet := nil;
  WTSet := nil;
  WTExtGet := nil;
  WTExtSet := nil;
  WTSave := nil;
  WTRestore := nil;
  WTPacketsPeek := nil;
  WTDataGet := nil;
  WTDataPeek := nil;
  WTQueuePacketsEx := nil;
  WTQueueSizeGet := nil;
  WTQueueSizeSet := nil;
  end;
end;

initialization
  LoadWinTab32;
finalization
  UnloadWinTab32;
end.
