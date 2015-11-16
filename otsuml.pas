unit otsuml;
//Multilevel Otsu's Method
//Otsu N (1979) A threshold selection method from gray-level histograms. IEEE Trans. Sys., Man., Cyber. 9: 62-66.
//Lookup Tables as suggested by Liao, Chen and Chung (2001) A fast algorithm for multilevel thresholding
//note that my "otsu.pas" is slightly faster and much simpler if you only want bi-level output

interface
uses define_types, sysutils;

function FindOtsu2 (var Img: Bytep; nVox: integer): byte;
//function ApplyOtsu2 (var Img: Bytep; nVox: integer): byte;
//function ApplyOtsu3 (var Img: Bytep; nVox: integer): byte;
//function ApplyOtsu4 (var Img: Bytep; nVox: integer): byte;
procedure ApplyOtsu (var Img: Bytep; nVox, levels: integer);//levels: 2=black/white, 3=3tone, 4=4tone
procedure ApplyOtsuBinary (var Img: Bytep; nVox,levels: integer);

implementation

Type
HistoRA = array [0..255] of longint;
HistoRAd = array [0..255] of double;
Histo2D = array [0..255] of HistoRAd;

Function OtsuLUT(H: HistoRA): Histo2D;
var
  Sum,Prob: double;
  v,u: integer;//column/rom index
  P,S: Histo2D;
begin
     Sum := 0;
     for v := 0 to 255 do
       Sum := Sum + H[v];
     if Sum <= 0 then
        exit;
     P[0][0] := H[0];
     S[0][0] := H[0];
     for v := 1 to 255 do begin
         prob := H[v]/Sum;
         P[0][v] := P[0][v-1]+prob;
         S[0][V] := S[0][v-1]+(v+1)*prob;
     end;
     for u := 1 to 255 do begin
         for v := u to 255 do begin
             P[u][v] := P[0][v]-P[0][u-1];
             S[u][v] := S[0][v]-S[0][u-1];
         end
     end;
     //result is eq 29 from Liao
     for u := 0 to 255 do begin
         for v := u to 255 do begin
             if S[u][v] = 0 then  //avoid divide by zero errors...
                result[u][v] := 0
             else
                 result[u][v] := sqr(S[u][v]) /P[u][v];
         end
     end;
end;

Function OtsuCostFunc(H: HistoRA): integer;
//Otsu N (1979) A threshold selection method from gray-level histograms". IEEE Trans. Sys., Man., Cyber. 9: 62-66.
//http://en.wikipedia.org/wiki/Otsu's_method
//http://www.labbookpages.co.uk/software/imgProc/otsuThreshold.html
//returns threshold for binarizing an image
// all voxel <=Threshold are background
// all voxel >Threshold are object
const
  kMaxBin = 255;
var
   t,total: integer;
   wB,wF,Sum,SumB,mF,mB,varBetween,varMax: double;
begin
     result := 0;
     wB := 0;
     wF := 0;
     SumB := 0;
  	 Sum := 0;
     Total := 0;
     varMax := 0;
     for t := 0 to kMaxBin do
     	 Total := Total + H[t];
     if Total = 0 then exit;
     for t := 0 to kMaxBin do
     	 Sum := Sum + (t*H[t]);
	 for t :=0 to kMaxBin do begin
   	 	 wB :=  wB + H[t];               // Weight Background
   		 if (wB = 0) then continue;
		 wF := Total - wB;                 // Weight Foreground
   		 if (wF = 0) then break;
   		 sumB := sumB+(t * H[t]);
         mB := sumB / wB;            // Mean Background
   		 mF := (sum - sumB) / wF;    // Mean Foreground
         // Calculate Between Class Variance
   		 varBetween := (wB/Total) * (wF/Total) * sqr(mB - mF);
         // Check if new maximum found
   		 if (t=0) or (varBetween > varMax) then begin
      	 	varMax := varBetween;
      		result := t;
   		 end;
     end;
end;

//OtsuCostFunc2 provides same answer as OtsuCostFunc, but is slightly slower and requires more RAM
function OtsuCostFunc2(lHisto: HistoRA): integer;
var
  v,max: double;
  h2d: Histo2D;
  n: integer;
begin
  h2d := OtsuLUT(lHisto);
  //default solution
  n := 128;
  max := h2d[0,n]+h2d[n+1,255];
  result := n;
  //exhaustively search
  for n := 0 to (255-1) do begin
      v := h2d[0,n]+h2d[n+1,255];
      if v > max then begin
         result := n;
         max := v;
      end; //new max
  end; //for n
end; //bilevel OtsuCostFunc2

procedure OtsuCostFunc3(lHisto: HistoRA; var Lo,Hi: integer);
var
  v,max: double;
  l,h: integer;
  h2d: Histo2D;
begin
  h2d := OtsuLUT(lHisto);
  //default solution
  lo := 85;
  hi := 170;
  max := h2d[0,lo]+h2d[lo+1,Hi]+h2d[Hi+1,255];
  //exhaustively search
  for l := 0 to (255-2) do begin
      for h := l+1 to (255-1) do begin
          v := h2d[0,l]+h2d[l+1,h]+h2d[h+1,255];
          if v > max then begin
             lo := l;
             hi := h;
             max := v;
          end; //new max
      end;//for h -> hi
  end; //for l -> low
end; //trilevel OtsuCostFunc3

procedure OtsuCostFunc4(lHisto: HistoRA; var Lo,Mid,Hi: integer);
var
  v,max: double;
  l,m,h: integer;
  h2d: Histo2D;
begin
  h2d := OtsuLUT(lHisto);
  //default solution
  lo := 64;
  mid := 128;
  hi := 192;
  max := h2d[0,lo]+h2d[lo+1,mid]+h2d[mid+1,hi]+h2d[Hi+1,255];
  //exhaustively search
  for l := 0 to (255-3) do begin
      for m := l+1 to (255-2) do begin
        for h := m+1 to (255-1) do begin
          v := h2d[0,l]+h2d[l+1,m]+h2d[m+1,h]+h2d[h+1,255];
          if v > max then begin
             lo := l;
             mid := m;
             hi := h;
             max := v;
          end; //new max
        end;//for h -> hi
      end; //for mid
  end; //for l -> low
end; //quad OtsuCostFunc4

function FindOtsu2 (var Img: Bytep; nVox: integer): byte;
var
  n: integer;
  lHisto: HistoRA;
begin
  result := 128;
  if nVox < 1 then exit;
  //create histogram
  for n := 0 to 255 do
    lHisto[n] := 0;
  for n := 0 to nVox do
    inc(lHisto[Img^[n]]);
  //now find minimum intraclass variance....
  //result := OtsuCostFunc(lHisto);
  result := OtsuCostFunc2(lHisto); //same answer, just slower and more memory
end;

procedure FindOtsu3 (var Img: Bytep; nVox: integer; var lo, hi: integer);
var
  n: integer;
  lHisto: HistoRA;
begin
  lo := 85;
  hi := 170;
  if nVox < 1 then exit;
  //create histogram
  for n := 0 to 255 do
    lHisto[n] := 0;
  for n := 0 to nVox do
    inc(lHisto[Img^[n]]);
  //now find minimum intraclass variance....
  OtsuCostFunc3(lHisto,lo,hi);
end;

procedure FindOtsu4 (var Img: Bytep; nVox: integer; var lo, med, hi: integer);
var
  n: integer;
  lHisto: HistoRA;
begin
  lo := 64;
  med := 128;
  hi := 192;
  if nVox < 1 then exit;
  //create histogram
  for n := 0 to 255 do
    lHisto[n] := 0;
  for n := 0 to nVox do
    inc(lHisto[Img^[n]]);
  //now find minimum intraclass variance....
  OtsuCostFunc4(lHisto,lo,med,hi);
end;

function ApplyOtsu2 (var Img: Bytep; nVox: integer): byte;
var
  n: integer;
begin
  result := 128;
  if nVox < 1 then exit;
  result := FindOtsu2(Img,nVox);
  for n := 1 to nVox do
    if Img^[n] > result then
      Img^[n] := 255
    else
      Img^[n] := 0;
end;

procedure ApplyOtsu3 (var Img: Bytep; nVox: integer);
var
  n,lo,hi: integer;
  h: histora;
begin
  if nVox < 1 then exit;
  FindOtsu3(Img,nVox,lo,hi);
  for n := 0 to 255 do
    if n <= Lo then
       H[n] := 0
    else if n <= hi then
       h[n] := 128
    else
        h[n] := 255;
  for n := 1 to nVox do
      Img^[n] := H[Img^[n]];
end;

procedure ApplyOtsu4 (var Img: Bytep; nVox: integer);
var
  n,lo,med,hi: integer;
  h: histora;
begin
  if nVox < 1 then exit;
  FindOtsu4(Img,nVox,lo,med,hi);
  for n := 0 to 255 do
    if n <= Lo then
       H[n] := 0
    else if n <= med then
       h[n] := 85
    else if n <= hi then
       h[n] := 170
    else
        h[n] := 255;
  for n := 1 to nVox do
      Img^[n] := H[Img^[n]];
end;

procedure ApplyOtsu (var Img: Bytep; nVox,levels: integer);
begin
     if levels <= 2 then
        ApplyOtsu2(Img,nVox)
     else if levels = 3 then
          ApplyOtsu3(Img,nVox)
     else
         ApplyOtsu4(Img,nVox);
end;

procedure ApplyOtsuBinary (var Img: Bytep; nVox,levels: integer);
//1=1/4, 2=1/3, 3=1/2, 4=2/3, 5=3/4
var
  n: integer;
  h: histora;
begin
     if nVox < 1 then exit;
     if (levels <= 1) or (levels >= 5) then
        ApplyOtsu4(Img,nVox)
     else if (levels = 2) or (levels = 4) then
          ApplyOtsu3(Img,nVox)
     else //level = 3
         ApplyOtsu2(Img,nVox);
     if levels <= 3 then begin //make dark: all except 255 equal 0
        for n := 0 to 254 do
            H[n] := 0;
        H[255] := 255;
     end else begin //make bright: all except 0 equal 255
       H[0] := 0;
       for n := 1 to 255 do
           H[n] := 255;
     end;
     for n := 1 to nVox do
            Img^[n] := H[Img^[n]];

end;


end.
 