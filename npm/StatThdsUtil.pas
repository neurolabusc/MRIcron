unit StatThdsUtil;
interface
uses
 //ComCtrls,Graphics, ExtCtrls,
 Classes,  define_types,dialogsx;
const 
        kMaxThreads = 16;
 kSh = 10; //bits to shift
	kMaxImages = 1024;
        kMaxPermute = 4000;
        //kPlankMB : integer = 512;

var
gnCPUThreads, gThreadsRunning: Integer;
        kPlankSz : int64;// =1024 {bytes/kb} * 1024 {bytes/mb} * kPlankMB; //e.g. 512 MB
        gDataTypeRA: array [0..kMaxImages] of integer;
	gOffsetRA,gScaleRA,gInterceptRA: array [0..kMaxImages] of single;
        gnVoxTestedRA : array [0..kMaxThreads] of integer;
        gPermuteMinT,gPermuteMaxT,gPermuteMinBM,gPermuteMaxBM : array [0..kMaxThreads,0..kMaxPermute ] of double;
procedure ClearThreadData(lnThreads,lnPermute: integer);
function SumThreadDataLite (lnThreads: integer): integer;
function SumThreadData (lnThreads,lnPermute: integer;lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP): integer;
procedure ClearThreadDataPvals (lnThreads,lnPermute: integer);

implementation

procedure ClearThreadDataPvals (lnThreads,lnPermute: integer);
var lT,lP: integer;
begin
     if lnThreads < 1 then exit;
     if lnPermute > kMaxPermute then
        ShowMsg('Error: recompile with larger kMaxPermute');
     for lT := 1 to lnThreads do
         gnVoxTestedRA[lT] := 0;
     if lnPermute < 1 then exit;
     for lT := 1 to lnThreads do begin
         for lP := 1 to lnPermute do begin
             gPermuteMinT[lT,lP] := 10;
             gPermuteMaxT[lT,lP] := -10;
             gPermuteMinBM[lT,lP] := 10;
             gPermuteMaxBM[lT,lP] := -10;
         end;
     end;
end;


procedure ClearThreadData (lnThreads,lnPermute: integer);
var lT,lP: integer;
begin
     if lnThreads < 1 then exit;
     if lnPermute > kMaxPermute then
        ShowMsg('Error: recompile with larger kMaxPermute');
     for lT := 1 to lnThreads do
         gnVoxTestedRA[lT] := 0;
     if lnPermute < 1 then exit;
     for lT := 1 to lnThreads do begin
         for lP := 1 to lnPermute do begin
             gPermuteMinT[lT,lP] := 0;
             gPermuteMaxT[lT,lP] := 0;
             gPermuteMinBM[lT,lP] := 0;
             gPermuteMaxBM[lT,lP] := 0;
         end;
     end;
end;

function SumThreadDataLite (lnThreads: integer): integer;
var lT: integer;
begin
     result := 0;
     if lnThreads < 1 then exit;
     for lT := 1 to lnThreads do
         result := result + gnVoxTestedRA[lT];
end;

function SumThreadData (lnThreads,lnPermute: integer;lPermuteMaxT, lPermuteMinT,lPermuteMaxBM, lPermuteMinBM: singleP): integer;
var lT,lP: integer;
begin
     result := 0;
     if lnThreads < 1 then exit;
     for lT := 1 to lnThreads do
         result := result + gnVoxTestedRA[lT];
     if lnPermute < 1 then exit;
     for lP := 1 to lnPermute do begin
             lPermuteMinT^[lP] :=  gPermuteMinT[1,lP];
             lPermuteMaxT^[lP] :=  gPermuteMaxT[1,lP];
             lPermuteMinBM^[lP] :=  gPermuteMinBM[1,lP];
             lPermuteMaxBM^[lP] :=  gPermuteMaxBM[1,lP];
     end;
     if lnThreads < 2 then exit;
     for lT := 2 to lnThreads do begin
         for lP := 1 to lnPermute do begin
             if lPermuteMinT^[lP] > gPermuteMinT[lT,lP]  then
                lPermuteMinT^[lP] := gPermuteMinT[lT,lP];
             if lPermuteMinBM^[lP] > gPermuteMinBM[lT,lP]  then
                lPermuteMinBM^[lP] := gPermuteMinBM[lT,lP];
             if lPermuteMaxT^[lP] < gPermuteMaxT[lT,lP]  then
                lPermuteMaxT^[lP] := gPermuteMaxT[lT,lP];
             if lPermuteMaxBM^[lP] < gPermuteMaxBM[lT,lP]  then
                lPermuteMaxBM^[lP] := gPermuteMaxBM[lT,lP];

         end;
     end;
end; //SumThreadData


end.