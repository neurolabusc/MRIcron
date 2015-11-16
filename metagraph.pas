unit metagraph;
interface

uses
  {$IFNDEF Unix}Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ToolWin, ComCtrls,define_types, ExtCtrls, StdCtrls,  Menus,ClipBrd;

const
     kMaxCond = 6;
     knMaxRow = 20;  //Niftiimgvie
     kMaxLines = kMaxCond* knMaxRow;
     kClrRA: array [1..kMaxCond] of TColor = (clRed,clGreen,clBlue,clTeal,clAqua,clSilver);
     kPenStyleRA: array[1..knMaxRow] of TPenStyle = (psSolid,psDot,psDash,psDashDot,psDashDotDot,psSolid,psDot,psDash,psDashDot,psDashDotDot,
     psSolid,psDot,psDash,psDashDot,psDashDotDot,psSolid,psDot,psDash,psDashDot,psDashDotDot);
type
    TEventOnset =  RECORD
      Events: integer;
      ELabel: string[16];
      EventRA,DurRA: SingleP;
    END;

    T4DTrace =  RECORD
      //Title: string[16];
      //Samples: integer;
      HorzMin,HorzWidPerBin,SampleMin,SampleMax,SamplePlotMin,SamplePlotMax
      {SampleMean,SampleSD,SampleSE,SampleVar}: Double;
      //SampleRA: SingleP;
      Lines: array [1..kMaxLines] of TEventOnset;
      Conditions: array [1..kMaxLines] of TEventOnset;
      //DurationRA: array [1..kMaxLines] of SingleP;
    END;
procedure Create4DTrace (var l4DTrace: T4DTrace);
procedure Init4DTrace(lnSample,lnLines: integer; var l4DTrace: T4DTrace; lErrorBars: boolean);
procedure Close4DTrace (var l4DTrace: T4DTrace; lCloseCond: boolean);
procedure MinMax4DTrace(var l4DTrace: T4DTrace);
procedure CorePlot4DTrace(var l4DTrace: T4DTrace; lImage: TImage; lStartSample,HSpeed,lnColors: integer;lTR,lVertMin,lVertMax: single; lErrorBars: boolean);
procedure GraphResize(lImage: TImage);
procedure CloseCond (var l4DTrace: T4DTrace; lCond: integer);
procedure InitCond (var l4DTrace: T4DTrace; lCond, lnEvents: integer);


{$IFNDEF FPC} var   gWmf: TMetafile; {$ENDIF}
implementation

procedure GraphResize(lImage: TImage);
var
  TempBitmap: TBitmap;
  lx,ly: integer;
begin

  lx := lImage.Width;
  ly := lImage.Height;
  if (lx < 1) or (ly < 1) then exit;
  TempBitmap := TBitmap.Create;
  TempBitmap.Width := lx;
  TempBitmap.Height := ly;
  //Draw32Bitmap(TempBitmap.Canvas.Handle, lx, ly,lBuff {Self});
  lImage.Picture.Bitmap := TempBitmap;
  lImage.Width := lx;//delphi
  lImage.Height := ly;//delphi
  TempBitmap.Free;
end;

function RealToStr(lR: double {was extended}; lDec: integer): string;
begin
     if lR > 99999 then
        RealTOStr := FloatToStrF(lR, ffExponent	,lDec,7)
     else
         RealTOStr := FloatToStrF(lR, ffFixed,7,lDec);
end;

procedure Create4DTrace (var l4DTrace: T4DTrace);
var
   lLine: integer;
begin
     with l4DTrace do begin
        for lLine := 1 to kMaxLines do begin
          Lines[lLine].events := 0;
          Lines[lLine].elabel := '';
          Conditions[lLine].events := 0;
          Conditions[lLine].elabel := '';

        end;
     end; //with trace
end;

procedure Init4DTrace(lnSample,lnLines: integer; var l4DTrace: T4DTrace; lErrorBars: boolean);
var
   lLine: integer;
begin
     Close4DTrace(l4DTrace,lErrorBars);
     if (lnSample < 1) or (lnLines < 1) then
        exit;
     with l4DTrace do begin
          HorzMin := 0;
          HorzWidPerBin := 1;
          for lLine := 1 to lnLines do begin
               //getmem(DurationRA[lLine],lnSample*sizeof(single));
               //fx(lLine,lnSample);

            getmem(Lines[lLine].EventRA,lnSample*sizeof(single));
            Lines[lLine].events := lnSample;
            if lErrorBars then begin
               getmem(Conditions[lLine].EventRA,lnSample*sizeof(single));
               getmem(Conditions[lLine].DurRA,lnSample*sizeof(single));
               Conditions[lLine].events := lnSample;
            end;
          end; //for each line
     end; //with trace
end;

procedure Close4DTrace (var l4DTrace: T4DTrace; lCloseCond: boolean);
var
   lLine: integer;
begin
     with l4DTrace do begin
        for lLine := 1 to kMaxLines do begin
          if Lines[lLine].events > 0  then begin

             freemem(Lines[lLine].EventRA);
          end;
          Lines[lLine].events := 0;
          if lCloseCond then begin
             if Conditions[lLine].events > 0 then begin
                freemem(Conditions[lLine].EventRA);
                freemem(Conditions[lLine].DurRA); //1/1/2008
             end;
             Conditions[lLine].events := 0;
          end;
        end; //for each Line
     end; //with trace
end;

procedure CloseCond (var l4DTrace: T4DTrace; lCond: integer);
begin
     if (lCond < 1) or (lCond > kMaxLines) then
        exit;
     if l4DTrace.Conditions[lCond].events > 0 then begin
        freemem(l4DTrace.Conditions[lCond].EventRA);
        freemem(l4DTrace.Conditions[lCond].DurRA);
      end;
     l4DTrace.Conditions[lCond].events := 0;
end;

procedure InitCond (var l4DTrace: T4DTrace; lCond, lnEvents: integer);
begin
     if (lCond < 1) or (lCond > kMaxLines) then
        exit;
     CloseCond (l4DTrace, lCond);
     if lnEvents > 0 then begin
        getmem(l4DTrace.Conditions[lCond].EventRA, lnEvents * sizeof(single));
        //getmem(l4DTrace.DurationRA[lCond],lnEvents*sizeof(single));
        getmem(l4DTrace.Conditions[lCond].DurRA,lnEvents*sizeof(single));
        //fx(lLine,lnSample);
     end;
     l4DTrace.Conditions[lCond].events := lnEvents;
end;

procedure MinMax4DTrace(var l4DTrace: T4DTrace);
var lPos,lLine: integer;
l1stLine :boolean;
begin
  l1stLine := true;
  with l4DTrace do begin
     for lLine := 1 to kMaxLines do begin
         if Lines[lLine].events > 0  then begin
            if l1stLine then begin
                   SampleMin := Lines[lLine].EventRA^[1];
                   SampleMax:= Lines[lLine].EventRA^[1];
            end;
            l1stLine := false;

            for lPos := 1 to Lines[lLine].events do begin
                if Lines[lLine].EventRA^[lPos] > SampleMax then
                   SampleMax := Lines[lLine].EventRA^[lPos];
                if Lines[lLine].EventRA^[lPos] < SampleMin then
                   SampleMin := Lines[lLine].EventRA^[lPos];
            end; //for each event
         end; //if events > 0
     end; //for each line
     SamplePlotMin := SampleMin-0.1*abs(SampleMax-SampleMin);
     SamplePlotMax := SampleMax+0.1*abs(SampleMax-SampleMin);
  end; //with trace
end;

{$IFDEF FPC}
procedure HText(lImage: TImage; lX,lY,lDec: integer; lVal: single);
{$ELSE}
procedure HText(lImage: TMetafileCanvas; lX,lY,lDec: integer; lVal: single);
{$ENDIF}
var
   lStr: string;
begin
    if lDec >= 0 then
       lStr := realtostr(round(lVal),0)
    else
        lStr := realtostr(lVal,abs(lDec));
{$IFDEF FPC}
    lImage.Canvas.TextOut(lX-(lImage.Canvas.TextWidth(lStr) shr 1),lY,lStr);
{$ELSE}
    lImage.TextOut(lX-(lImage.TextWidth(lStr) shr 1),lY,lStr);
{$ENDIF}
end;

{$IFDEF FPC}
procedure VText(lImage: TImage; lX,lY,lDec: integer; lVal: single);
{$ELSE}
procedure VText(lImage: TMetafileCanvas; lX,lY,lDec: integer; lVal: single);
{$ENDIF}
var
   lStr: string;
begin
    if lDec >= 0 then
       lStr := realtostr(round(lVal),0)
    else
        lStr := realtostr(lVal,abs(lDec));
{$IFDEF FPC}
    lImage.Canvas.TextOut(lX-lImage.Canvas.TextWidth(lStr) ,lY,lStr);
{$ELSE}
    lImage.TextOut(lX-lImage.TextWidth(lStr) ,lY,lStr);
{$ENDIF}

end;

{$IFDEF FPC}
procedure VTextLeftJustified(lImage: TIMage; lX,lY,lDec: integer; lVal: single);
{$ELSE}
procedure VTextLeftJustified(lImage: TMetafileCanvas; lX,lY,lDec: integer; lVal: single);
{$ENDIF}
var
   lStr: string;
begin
    if lDec >= 0 then
       lStr := inttostr(round(lVal))
    else
        lStr := realtostr(lVal,abs(lDec));
{$IFDEF FPC}
    lImage.Canvas.TextOut(lX ,lY,lStr);
{$ELSE}
    lImage.TextOut(lX ,lY,lStr);
{$ENDIF}

end;

{$IFDEF FPC}
procedure ShowRange(lImage: TImage; lMin,lMax: single; lL,lT,lR,lB,lPosition: integer);
{$ELSE}
procedure ShowRange(lImage: TMetafileCanvas; lMin,lMax: single; lL,lT,lR,lB,lPosition: integer);
{$ENDIF}
//position 1=L,2=T,3=R,4=B
var
   lRangeR,lRange,lD,lV: double;
   lDecimals,lPos,lLo,lHi,lHPos,lOffset : integer;
begin
{$IFDEF FPC}
with lImage.Canvas do begin
{$ELSE}
with lImage do begin
{$ENDIF}
                Font.color := clBlack;
    lRange := abs(lMax-lMin);
    lRangeR := lRange;
    lDecimals := 0;
    lD := 1;
    if lRangeR = 0 then
       exit;
    while lRangeR > 10 do begin//get range 1..10
          lRangeR := lRangeR / 10;
          inc(lDecimals);
          lD := lD * 10;
    end;
    while lRangeR < 1 do begin//get range 1..10
          lRangeR := lRangeR * 10;
          dec(lDecimals);
          lD := lD / 10;
    end;
    lLo := round((lMin + (lD/2)) / lD);
    lHi := trunc((lMax + (lD/20) ) / lD);//2007
    //lHi := trunc((lMax ) / lD);
    if lHi <= (lLo+2) then begin
        lD := lD /2;
        if lDecimals <= 0 then
           dec(lDecimals)
        else
           inc(lDecimals);
        lLo := round((lMin + (lD/2)) / lD);
        lHi := trunc((lMax + (lD/20) ) / lD);//2007
    end;
    if (lPosition = 2{T}) or (lPosition = 4{B}) then begin
       lOffset :=  TextHeight('0');
       for lPos := lLo to lHi do begin
        lV := lPos * lD;
        lHPos := lL+ round( ((lV-lMin) / lRange)* abs(lR-lL));
        if (lPosition = 2{T}) then
           HText(lImage, lHPos,lT- lOffset,lDecimals,lV)
        else
            HText(lImage, lHPos,lB+1,lDecimals,lV);
       end;
    end else if (lPosition = 1{L}) or (lPosition = 3{R}) then begin //vertical values
       lOffset :=  TextHeight('0') div 2; //2007
       for lPos := lLo to lHi do begin
        lV := lPos * lD;
        {lHPos := lB- round( ((lV-lMin) / lRange)* abs(lT-lB));
        lImage.MoveTo(1,lHPos);
        lImage.LineTo(1000,lHPos);}
        lHPos := lB- round( ((lV-lMin) / lRange)* abs(lT-lB))-lOffset;

        if (lPosition = 1{L}) then
           VText(lImage, lL-1,lHPos,lDecimals,lV)
        else
            VTextLeftJustified(lImage, lR+1,lHPos,lDecimals,lV);
       end;

    end; //if vertical
  end; //with limage
end;

{$IFDEF FPC}
function ShowLegend(var l4DTrace: T4DTrace; lImage: TImage; lL,lT: integer): integer;
{$ELSE}
function ShowLegend(var l4DTrace: T4DTrace; lImage: TMetafileCanvas; lL,lT: integer): integer;
{$ENDIF}
var
   lC,lLegendLeft: integer;
begin
{$IFDEF FPC}
with lImage.Canvas do begin
{$ELSE}
with lImage do begin
{$ENDIF}
  lLegendLeft := lL;
  font.color := clBlack;
  for lC := 1 to kMaxCond do begin
      //lImage.canvas.pen.color := kClrRA[lC];
      font.color := kClrRA[lC] ;
      if (l4DTrace.Conditions[lC].events > 0)  then begin
        TextOut(lLegendLeft,lT,l4DTrace.Conditions[lC].ELabel);
        lLegendLeft := lLegendLeft + TextWidth(l4DTrace.Conditions[lC].ELabel)+5;
      end; //for each tevent
  end; //if cond has events
  result := lLegendLeft;
end; //with limage
end; //for each cond

function n4DTrace(var l4DTrace: T4DTrace;var  lSamples: integer; lErrorBars: boolean): integer;
var lLine: integer;
l1stLine :boolean;
begin
  lSamples:= 0;
  result := 0;
  l1stLine := true;
  with l4DTrace do begin
     for lLine := 1 to kMaxLines do begin
         if Lines[lLine].events > 0  then begin
            if l1stLine then
               lSamples := Lines[lLine].events;
            l1stLine := false;
            if  (lErrorBars) and (Lines[lLine].events <> lSamples) then
               exit; //all lines must have same number of samples
            inc(result);
         end; //if events > 0
     end; //for each line
  end; //with trace
end;

{$IFDEF FPC}
function SetColorStyle (lImage: TImage; lLine,lnColors: integer): TPenStyle;
{$ELSE}
function SetColorStyle (lImage: TMetafileCanvas; lLine,lnColors: integer): TPenStyle;
{$ENDIF}
var
   lC: integer;
begin
{$IFDEF FPC}
with lImage.Canvas do begin
{$ELSE}
with lImage do begin
{$ENDIF}
     if lnColors < 1 then begin
        pen.color := clBlack;//clRed
        pen.style := kPenStyleRA[lLine];
        result := kPenStyleRA[lLine];
        exit;
     end;
     lC := lLine mod lnColors;
     if lC = 0 then
        lC := lnColors;
     pen.color := kClrRA[lC];
     lC := ((lLine-1) div lnColors)+1;
     pen.style := kPenStyleRA[lC];
     result := kPenStyleRA[lC];
end; //with lImage.
end;

{$IFDEF FPC}
procedure ShowLineLegend(var l4DTrace: T4DTrace; lImage: TImage;  lL, lT,lnLines,lnColors: integer);
{$ELSE}
procedure ShowLineLegend(var l4DTrace: T4DTrace; lImage: TMetafileCanvas;  lL, lT,lnLines,lnColors: integer);
{$ENDIF}
var
   lLineTop,lStyle,lnStyles,lLegendLeft: integer;
begin
  if lnColors < 1 then
     lnStyles := lnLines
  else
      lnStyles := lnLines div lnColors;
  if lnStyles < 1 then
     lnStyles := 1;
{$IFDEF FPC}
with lImage.Canvas do begin
{$ELSE}
with lImage do begin
{$ENDIF}

  font.color := clBlack;
  pen.color := clBlack;
  lLegendLeft := lL;
  lLineTop := lT+(TextHeight('X') div 2);
  for lStyle := 1 to lnStyles do begin
      pen.style := kPenStyleRA[lStyle];
      MoveTo(lLegendLeft,lLineTop);
      lLegendLeft := lLegendLeft +40;
      LineTo(lLegendLeft,lLineTop);
      lLegendLeft := lLegendLeft + 2;
      TextOut(lLegendLeft,lT,l4DTrace.Lines[lStyle].ELabel);
      lLegendLeft := lLegendLeft + TextWidth(l4DTrace.Lines[lStyle].ELabel)+5;
  end;
  pen.style := psSolid;
end;//with lImage.
end;

{$IFDEF FPC}
procedure ShowPlot(var l4DTrace: T4DTrace; lImage: TImage; lL,lT,lR,lB,lStartSample,lHSpeedIn,lScalePos,lnColors: integer; lSecPerSample,lVertMin,lVertMax: single; lShowHRange,lErrorBars: boolean);
{$ELSE}
procedure ShowPlot(var l4DTrace: T4DTrace; lImage: TMetafileCanvas; lL,lT,lR,lB,lStartSample,lHSpeedIn,lScalePos,lnColors: integer; lSecPerSample,lVertMin,lVertMax: single; lShowHRange,lErrorBars: boolean);
{$ENDIF}
const
     kMinMax = 0;
     k2SD = 1;
     k12bit = 2;
     kMaxPt = 16000;
type
    TPtRA= array [1..kMaxPt] of TPoint;
var
   lnPt,lnLines,lLine,lnSamples,lC,lStartSamp,lEndSamp,lEndPix,lPos,lI: integer;
   lVert,lHorz,lVMax,lVMin,lScale,lHSpeed: single;
   lPenStyle: TPenStyle;
   lPtRA: TPtRA;
begin
  lnLines := n4DTrace(l4DTrace,lnSamples,lErrorBars);
  if (lnLines < 1) or (lnSamples < 2) or (lB <= lT) then exit;
  lStartSamp := lStartSample;
  if  (lStartSamp > lnSamples) then
     exit;
  if lStartSamp < 1 then
     lStartSamp := 1;
  lHSpeed := lHSpeedIn;
  if lHSpeed < 1 then begin
     lStartSamp := 1;
     lHSpeed := (lnSamples-1)/(lR-lL);
  end;
{$IFDEF FPC}
with lImage.Canvas do begin
{$ELSE}
with lImage do begin
{$ENDIF}
  lEndSamp := trunc(lStartSamp + ((lR-lL)*lHSpeed))+1;
     ShowRange(lImage, l4DTrace.HorzMin+((lStartSamp-1)*l4DTrace.HorzWidPerBin),l4DTrace.HorzMin+((lEndSamp-1)*l4DTrace.HorzWidPerBin),lL,lT,lR,lB,4);
  if lShowHRange then
     ShowRange(lImage, l4DTrace.HorzMin+((lStartSamp-1)*l4DTrace.HorzWidPerBin),l4DTrace.HorzMin+((lEndSamp-1)*l4DTrace.HorzWidPerBin),lL,lT,lR,lB,4);

  lI := ShowLegend(l4DTrace,lImage, lL,5);
  ShowLineLegend(l4DTrace, lImage, lI+10, 5,lnLines,lnColors);
  //next show event onsets
  if not lErrorBars then
  for lC := 1 to kMaxCond do begin
      pen.color := kClrRA[lC];
      if (l4DTrace.Conditions[lC].events > 0) and (lSecPerSample > 0) then begin
        //canvas.TextOut(lLegendLeft,lT-canvas.TextHeight('X')-2,l4DTrace.Conditions[lC].ELabel);
        //lLegendLeft := lLegendLeft + canvas.TextWidth(l4DTrace.Conditions[lC].ELabel)+5;
        for lPos := 1 to l4DTrace.Conditions[lC].events do begin
          lHorz := l4DTrace.Conditions[lC].EventRA^[lPos] / lSecPerSample;
          if (lHorz < lEndSamp)  and (lHorz > lStartSamp) then begin
           lVert := ((lHorz - lStartSamp) / lHSpeed)+lL;
           moveto(round(lVert),lT);
           lineto(round(lVert),lB);
          end; //if event in range
        end; //for each tevent
      end; //if cond has events
  end; //for each cond
  if (lEndSamp > lnSamples) then begin
     lEndSamp := lnSamples;
     lEndPix := lL+trunc((lnSamples-lStartSamp) / lHSpeed);
  end else
      lEndPix := lR;
  lVMax := lVertMax;
  lVMin := lVertMin;
  if (lVMax <= lVMin) then begin
     lVMax := l4DTrace.SamplePlotMax;
     lVMin :=  l4DTrace.SamplePlotMin;
  end;
  if (lVMax <  l4DTrace.SampleMin) or (lVMin >  l4DTrace.SampleMax)  then begin
     lVMax := l4DTrace.SamplePlotMax;
     lVMin :=  l4DTrace.SamplePlotMin;
  end;

  ShowRange(lImage,lVMin,lVMax,lL,lT,lR,lB,lScalePos);
  moveto(lL,lT);
  if lVMax <= lVMin then
     lScale := 1
  else
      lScale := (lB-lT)/ (lVMax-lVMin);
  if lHSpeed < 1 then begin
   //lHSpeed := (l4DTrace.Samples-1)/(lR-lL);
   for lLine := 1 to lnLines do begin
     lPenStyle := SetColorStyle (lImage, lLine,lnColors);
     lnPt := 0;
     for lPos := lStartSamp to lEndSamp do begin
      lVert := l4DTrace.Lines[lLine].EventRA^[lPos];
      if lVert > lVMax then
         lVert := lVMax
      else if lVert < lVMin then
           lVert := lVMin;
      lVert := round((lVert-lVMin)*lScale);
      lVert := lB-lVert;
      lHorz := lL+round((lPos-lStartSamp)/lHSpeed);
      inc(lnPt);
      if lnPt < kMaxPt then
         lPtRA[lnPt] := Point(round(lHorz),round(lVert));
      if lErrorBars then begin
         pen.style := psSolid;
         moveto(round(lHorz),round(lVert-(l4DTrace.Conditions[lLine].EventRA^[lPos]*lScale)));
         lineto(round(lHorz) ,round(lVert+(l4DTrace.Conditions[lLine].EventRA^[lPos]*lScale)));
         moveto(round(lHorz) ,round(lVert));
         pen.style := lPenStyle;
      end;
     end; //for lPos
      if lnPt > kMaxPt then
         lnPt := kMaxPt;
      if lnPt > 0 then
         PolyLine( Slice(lPtRA, lnPt));

   end; //for each line
  end else begin //HSpeed >=1 so every pixel unique
     for lLine := 1 to lnLines do begin
     lPenStyle := SetColorStyle (lImage, lLine,lnColors);
   lI := lStartSamp;
   lnPt := 0;
   for lPos := lL to lEndPix do begin
      lVert := l4DTrace.Lines[lLine].EventRA^[lI];
      if lVert > lVMax then
         lVert := lVMax
      else if lVert < lVMin then
           lVert := lVMin;
      lVert := round((lVert-lVMin)*lScale);
      //lVert := lVert + lT;
      lVert := lB-lVert;
      inc(lnPt);
      if lnPt < kMaxPt then
         lPtRA[lnPt] := Point(lPos,round(lVert));

      if lErrorBars then begin
           pen.style := psSolid;
         moveto(lPos,round(lVert-(l4DTrace.Conditions[lLine].EventRA^[lPos]*lScale)));
         lineto(lPos ,round(lVert+(l4DTrace.Conditions[lLine].EventRA^[lPos]*lScale)));
         moveto(lPos ,round(lVert));
         pen.style := lPenStyle;
      end;
      lI := round( lStartSamp+((lPos-lL)*lHSpeed) );
      if lI < 1 then
         lI := 1;
      if lI > lEndSamp then
         lI := lEndSamp;
    end; //for lPos
      if lnPt > kMaxPt then
         lnPt := kMaxPt;
      if lnPt > 0 then
         PolyLine( Slice(lPtRA, lnPt))

      end; //for each line
   end; //hspeed >= 1
   pen.style := psSolid;
end;//with .lImage
end;



procedure DrawBMP( lx, ly: integer;  {lBuff: RGBQuadp;} var lImage: TImage);
var
  TempBitmap: TBitmap;
begin
  TempBitmap := TBitmap.Create;
    TempBitmap.Width := lx;
    TempBitmap.Height := ly;
    //Draw32Bitmap(TempBitmap.Canvas.Handle, lx, ly,lBuff {Self});
    lImage.Picture.Bitmap := TempBitmap;
  lImage.Width := lx;//delphi
  lImage.Height := ly;//delphi
    TempBitmap.Free;
end;


 {$IFDEF FPC}
procedure PrepPlot(var lImage: TIMage; lL,lT,lR,lB,lWid,lHt,lFontSize: integer);
{$ELSE}
procedure PrepPlot(var lImage: TMetafileCanvas; lL,lT,lR,lB,lWid,lHt,lFontSize: integer);
{$ENDIF}
begin
{$IFDEF FPC}
   with lImage.Canvas do begin
{$ELSE}
   with lImage do begin
{$ENDIF}
     Font.Name := 'Arial';
     Font.Size := 12;
     pen.color := clBlack;
     Font.color := clBlack;
     Brush.Style := bsSolid;
     Brush.color := clWhite;
     Rectangle(1,1,lWid,lHt);
     Rectangle(lL,lT,lR,lB);
   end;
end;

procedure CorePlot4DTrace(var l4DTrace: T4DTrace; lImage: TImage; lStartSample,HSpeed,lnColors: integer;lTR,lVertMin,lVertMax: single; lErrorBars: boolean);
var
   lWid,lHt,lBorder,lL,lT,lR,lB,lFontSize: integer;
{$IFDEF FPC}
  //WmfCanvas: TCanvas;

{$ELSE}
  WmfCanvas: TMetafileCanvas;
{$ENDIF}
begin
     lWid := lImage.Width;
     lHt := lImage.Height;
     lFontSize := 12;
     lBorder := lFontSize * 4;
     if (lWid <= (2*lBorder)) or (lHt <= (2*lBorder)) then
        exit;

     lL := round(1.3*lBorder);
     lT :=lFontSize*2;
     lR := lWid - lBorder;
     lB := lHt-(lFontSize*2);
{$IFDEF FPC}
     //WmfCanvas := TCanvas.Create;
          PrepPlot(lImage,lL,lT,lR,lB,lWid,lHt,lFontSize);
          ShowPlot(l4DTrace,lImage,lL,lT,lR,lB,lStartSample,HSpeed,1,lnColors, lTR,lVertMin,lVertMax,true,lErrorBars);
//abba lImage.Canvas.Draw (0, 0, WmfCanvas);
     //WmfCanvas.Free;
{$ELSE}
     gWmf.clear;
     gWmf.Width := lWid;
     gWmf.Height := lHt;
     WmfCanvas := TMetafileCanvas.CreateWithComment(gWmf, 0, 'mricron', 'plot metafile');
     try
          PrepPlot(WmfCanvas,lL,lT,lR,lB,lWid,lHt,lFontSize);
          ShowPlot(l4DTrace,WmfCanvas,lL,lT,lR,lB,lStartSample,HSpeed,1,lnColors, lTR,lVertMin,lVertMax,true,lErrorBars);
     finally
            WmfCanvas.Free;
     end;//finally
     lImage.Canvas.Draw (0, 0, gWmf);
{$ENDIF}
end;


initialization
begin
{$IFDEF FPC}
{$ELSE}
  gWmf := TMetafile.Create;
  gWmf.Enhanced := True;
{$ENDIF}
  //   Create4DTrace(g4Ddata);
end;

finalization
begin
 //Close4DTrace(g4Ddata);
{$IFDEF FPC}
{$ELSE}
     gWmf.free;
{$ENDIF}

end;


end.
