unit RenderThds;
interface
{$include isthreaded.inc}
  {$mode delphi}
uses
{$IFDEF UNIX}
lclintf,//critical sections
{$ELSE}
  Windows,
{$ENDIF}
 ComCtrls,Classes, Graphics, ExtCtrls, define_types,GraphicsMathLibrary
 ,sysutils;
const
 kSh = 10; //bits to shift - precision for integers to simulate floats
var
  ThreadsRunning: Integer = 0;

 type

  TRotateVals = record
             InSliceSz,ZDimStart,ZDimEnd,YDimStart,YDimEnd,OutPivot,OutDim,OutSliceSz: integer;
             XPivotInU2,YDimIN,YPivotInU2,ZDimIN,ZPivotInU2,XDimIN: integer;
             XPivotIn,YPivotIn,ZPivotIn: integer;
             Xxra,Xyra,Xzra: longintp;
             //RenderCutout: boolean;
  end;


  TRenderThread = class(TThread)
  private
    lBarX: TProgressBar;
    lRV: TRotateVals;
    lMx : TMatrix;
    lThreadX: integer;
    lRenderCutoutX: boolean;
    lBuffInX,lBuffOutX: ByteP;
    lPosX: integer;
        procedure DoVisualSwap;
  protected
    procedure Execute; override;
    procedure VisualProg(lPos: Integer);
    procedure Rotate(lThread: integer; l: TRotateVals; var lM: TMatrix; lRenderCutout: boolean; var lBuffIn,lBuffOut: ByteP); virtual; abstract;
  public

    constructor Create(lBar: TProgressBar; lThread: integer; l: TRotateVals; var lM: TMatrix; lRenderCutout: boolean; var lBuffIn,lBuffOut: ByteP);
  end;

{ NearestNeighbor }

  TNNRender = class(TRenderThread)
  protected
    procedure Rotate(lThread: integer;l: TRotateVals; var lM: TMatrix; lRenderCutout: boolean; var lBuffIn,lBuffOut: ByteP);  override;
  end;

{ Trilinear }

  TTriRender = class(TRenderThread)
  protected
    procedure Rotate(lThread: integer;l: TRotateVals; var lM: TMatrix; lRenderCutout: boolean; var lBuffIn,lBuffOut: ByteP);  override;
  end;

implementation

uses Render;

var
  {$IFDEF UNIX}
     {$ifdef cpux86_64}
     CritSect : QWord;
     {$else}
     CritSect : LongWord;
     {$endif}
  {$ELSE}
     CritSect : TRTLCriticalSection;
  {$ENDIF}

procedure ThreadDone;
begin
              EnterCriticalSection(CritSect);
	Dec(ThreadsRunning);
              LeaveCriticalSection(CritSect);

end;

procedure TRenderThread.DoVisualSwap;
begin
  {$IFDEF SHOWPROG}
  lBarX.Position := lPosX;
  {$ENDIF}
end;

procedure TRenderThread.VisualProg(lPos: Integer);
begin
  lPosX := lPos;
  {$IFDEF SHOWPROG}
  	{$IFDEF FPC}
  	Synchronize(DoVisualSwap);
  	{$ELSE}
  	Synchronize(DoVisualSwap);
  	{$ENDIF}
  {$ENDIF}

end;

constructor TRenderThread.Create(lBar: TProgressBar;lThread: integer;l: TRotateVals; var lM: TMatrix; lRenderCutout: boolean; var lBuffIn,lBuffOut: ByteP);
begin
  lBarX := lBar;
  lRV := l;
  lMx := lM;
  lRenderCutoutX := lRenderCutout;
  lBuffInX := lBuffIn;
  lBuffOutX := lBuffOut;
  lThreadX := lThread;
  FreeOnTerminate := True;
  inherited Create(False);
end;

// The Execute method is called when the thread starts

procedure TRenderThread.Execute;
begin
  Rotate(lThreadX,lRV,lMx,lRenderCutoutX, lBuffInX,lBuffOutX);
end;

procedure FindXBounds (var lXMax,lXMin: integer;
lXDimIN,lYxiZxi,lXPivotInU2,lYDimIN,lYyiZyi,lYPivotInU2,lZDimIN,lYziZzi,lZPivotInU2,lOutDim:integer;
 lXxra,lXyra,lXzra : LongIntP);
var
 lXo,lYo,lZo,Xo_at_one,Xo_at_two,Xo_grad,Xo_offs,lShiftedOne : integer;
 when_it_is_zero, when_it_is_max: double;
 lReallySmall {, debugx0, debugx1, debugy0, debugy1, debugz0, debugz1}: double;
 l2: integer;
begin
		  lXMax := lOutDim;
		  lXMin := 1;
		  l2 := 2;
		  lShiftedOne := 1 shl ksh;
		  lReallySmall := 1e-6;
		  Xo_at_one := lXxRA^[1] +lYxiZxi + (lXPivotInU2 shl kSh);
		  Xo_at_two := lXxRA^[l2] +lYxiZxi + (lXPivotInU2 shl kSh);
		  Xo_grad := Xo_at_two - Xo_at_one; Xo_offs := Xo_at_one - Xo_grad;
		  if Abs(Xo_grad) > lReallySmall then begin
			 when_it_is_zero := (lShiftedOne-Xo_offs) / Xo_grad;
			 when_it_is_max := ((lXDimIn shl kSh)-Xo_offs) / Xo_grad;
			 //debugx0 := when_it_is_zero; debugx1 := when_it_is_max;
			 if (when_it_is_zero < when_it_is_max) then begin
			   if when_it_is_zero > lXMin then lXMin := Round(when_it_is_zero+0.5);
			   if when_it_is_max < lXMax then lXMax := Round(when_it_is_max-0.5);

			 end else begin
				if when_it_is_max > lXMin then lXMin := Round(when_it_is_max+0.5);
				if when_it_is_zero < lXMax then lXMax := Round(when_it_is_zero-0.5);
			 end;
		  end;
		  Xo_at_one := lXyRA^[1]  +lYyiZyi + (lYPivotInU2 shl kSh);
		  Xo_at_two := lXyRA^[l2] +lYyiZyi + (lYPivotInU2 shl kSh);
		  Xo_grad := Xo_at_two - Xo_at_one; Xo_offs := Xo_at_one - Xo_grad;
		  if Abs(Xo_grad) > lReallySmall then begin
			 when_it_is_zero := (lShiftedOne-Xo_offs) / Xo_grad;
			 when_it_is_max := ((lYDimIn shl kSh)-Xo_offs) / Xo_grad;
			 //debugy0 := when_it_is_zero; debugy1 := when_it_is_max;
			 if (when_it_is_zero < when_it_is_max) then begin
				if when_it_is_zero > lXMin then lXMin := Round(when_it_is_zero+0.5);
				if when_it_is_max < lXMax then lXMax := Round(when_it_is_max-0.5);

			 end else begin
				if when_it_is_max > lXMin then lXMin := Round(when_it_is_max+0.5);
				if when_it_is_zero < lXMax then lXMax := Round(when_it_is_zero-0.5);
			 end;
		  end;
		  Xo_at_one := lXzRA^[1]  +lYziZzi + (lZPivotInU2 shl kSh);
		  Xo_at_two := lXzRA^[l2] +lYziZzi + (lZPivotInU2 shl kSh);
		  Xo_grad := Xo_at_two - Xo_at_one; Xo_offs := Xo_at_one - Xo_grad;
		  if Abs(Xo_grad) > lReallySmall then begin
			 when_it_is_zero := (lShiftedOne-Xo_offs) / Xo_grad;
			 when_it_is_max := ((lZDimIn shl kSh)-Xo_offs) / Xo_grad;
			 //debugz0 := when_it_is_zero; debugz1 := when_it_is_max;
			 if (when_it_is_zero < when_it_is_max) then begin
				if when_it_is_zero > lXMin then lXMin := Round(when_it_is_zero+0.5);
				if when_it_is_max < lXMax then lXMax := Round(when_it_is_max-0.5);
			 end else begin
				if when_it_is_max > lXMin then lXMin := Round(when_it_is_max+0.5);
				if when_it_is_zero < lXMax then lXMax := Round(when_it_is_zero-0.5);
			 end;
		  end;
		  // even with all the care about rounding, it's possible that we've got the
		  // edges wrong in ultra-high-gradient cases
		  if lXMin < lXMax then begin
			 while true do begin
				lXo :=  ((lXxRA^[lXMin] +lYxiZxi) shr kSh)+lXPivotInU2;
				lYo :=  ((lXyRA^[lXMin] +lYyiZyi) shr kSh)+lYPivotInU2;
				lZo :=  ((lXzRA^[lXMin] +lYziZzi) shr kSh)+lZPivotInU2;
				if (lXMin < lXMax) and ((lXo<1) or (lXo>lXDimIn) or (lYo<1) or (lYo>lYDimIn) or (lZo<1) or (lZo>lZDimIn)) then begin
				   lXMin := 1+lXMin;
				end else
				   break;
			 end;
			 while true do begin
				lXo :=  ((lXxRA^[lXMax] +lYxiZxi) shr kSh)+lXPivotInU2;
				lYo :=  ((lXyRA^[lXMax] +lYyiZyi) shr kSh)+lYPivotInU2;
				lZo :=  ((lXzRA^[lXMax] +lYziZzi) shr kSh)+lZPivotInU2;
				if (lXMax > lXMin) and ((lXo<1) or (lXo>lXDimIn) or (lYo<1) or (lYo>lYDimIn) or (lZo<1) or (lZo>lZDimIn)) then begin
				  lXMax := lXMax-1;
				end else
				   break;
			 end;
		  end;
end;//proc findXBounds

//Nearest Neighbor
procedure TNNRender.Rotate (lThread: integer;l: TRotateVals; var lM: TMatrix; lRenderCutout: boolean; var lBuffIn,lBuffOut: ByteP);
const kshx = ksh shr 1;
var

   lZxi,lZyi,lZzi,lYxiZxi,lYyiZyi,lYziZzi,lZ,lY,lX,lOutPos,
   lMaxX,lMinX,lXo,lYo,lZo: integer;
begin
		for lZ := l.ZDimStart to l.ZDimEnd do begin
		 lZxi := round(lZ*lM.matrix[1,3]* (1 shl kSh)  );
		 lZyi := round(lZ*lM.matrix[2,3]* (1 shl kSh)  );
		 lZzi := round(lZ*lM.matrix[3,3]* (1 shl kSh)  );
	         if {(RenderForm.RenderRefreshTimer.enabled) or} (Terminated) then begin
                    ThreadDone;
                    exit;
                 end;
                   {$IFDEF SHOWPROG}  //flicker with lazarus
                 if (lThread = 1) and ((lZ mod 30)=0) then
                    VisualProg(lZ);
                 {$ENDIF}
		 //ImgForm.ProgressBar1.Position := lZ;
		 for lY := l.YDimStart to l.YDimEnd do begin
		  lYxiZxi := round(lY * lM.matrix[1,2]* (1 shl kSh)  )+lZxi;
		  lYyiZyi := round(lY * lM.matrix[2,2]* (1 shl kSh)  )+lZyi;
		  lYziZzi := round(lY * lM.matrix[3,2]* (1 shl kSh)  )+lZzi;
		  lOutPos := ((lZ+l.OutPivot-1)*l.OutSliceSz)+((lY+l.OutPivot-1)*l.Outdim);
		  //if gAbortRender > 0 then goto 345;
		  FindXBounds (lMaxX,lMinX,l.XDimIN,lYxiZxi,l.XPivotInU2,l.YDimIN,lYyiZyi,l.YPivotInU2,l.ZDimIN,lYziZzi,l.ZPivotInU2,l.OutDim,l.Xxra,l.Xyra,l.Xzra);
			 if lMaxX > lMinX then
				for lX := lMinX to lMaxX do begin
					lXo :=  ((l.XxRA^[lX] +lYxiZxi) shr kSh)+l.XPivotInU2;
					lYo :=  ((l.XyRA^[lX] +lYyiZyi) shr kSh)+l.YPivotInU2;
					lZo :=  ((l.XzRA^[lX] +lYziZzi) shr kSh)+l.ZPivotInU2;
                                        {lXo := (lXo shr 1) + 1;
                                        lYo := lYo shr 1;
                                        lZo := lZo shr 1;}
					   lBuffOut[lX+lOutPos] := lBuffIn[(lXo)+((lYo-1)*l.XdimIn)+((lZo-1)*l.InSliceSz)]
				end;
		 end; //for y
		end; //for z
                ThreadDone;
end;

procedure TTriRender.Rotate (lThread: integer;l: TRotateVals; var lM: TMatrix; lRenderCutout: boolean; var lBuffIn,lBuffOut: ByteP);
//Trilinear - this uses integer math, and on CoreDuo CPUs is 30% faster than Floating Point
//For precision, integers are multiplied by kSh (~2^10 bits) to simulate floats
// However, we will use 32-bit integers and the image intensity is 8 bit values,
// with the final interpolation multiplying X*Y*Z*intensity
// Therefore, this final interpolation adjusts kSh to be 2^8, avoiding overflow
var
   lMi: TMatrixi;
   lXr,lYr,lZr,lYxi,lYyi,lYzi,lXxi,lXyi,lXzi,lZxi,lZyi,lZzi,
   lYxiZxi,lYyiZyi,lYziZzi,lZ,lY,lX,lOutPos,
   lXPiv,lYPiv,lZPiv,lXrM1i,lYrM1i,lZrM1i,
   lShr,lShl,lShlTo8,lShl8,
   lMinZ,lMaxZ,lMinY,lMaxY,lMaxX,lMinX,lXo,lYo,lZo: integer;
begin
     lShl := 1 shl kSh;
     lShl8 := 1 shl 8; //8bit precision
     lShlTo8 := (kSh - 8); //shr the kSh precision by this to get 8-bit precision
     lShr := 24;//24-bits * 8 bit intensity = 32 bits
     lXPiv := l.XPivotIn * lShl;
     lYPiv := l.YPivotIn * lShl;
     lZPiv := l.ZPivotIn * lShl;
     for lX := 1 to 3 do
         for lY := 1 to 3 do
             lMi.matrix[lX,lY] := round(lM.matrix[lX,lY] * lShl);
     if (lRenderCutout )  then begin //only separated to unroll IF rendercutout
        for lZ := l.ZDimStart to l.ZDimEnd do begin
		 lZxi := (lZ*lMi.matrix[1,3]  );
		 lZyi := (lZ*lMi.matrix[2,3]  );
		 lZzi := (lZ*lMi.matrix[3,3]  );
	  if {(RenderForm.RenderRefreshTimer.enabled) or} (Terminated) then begin
             ThreadDone;
             exit;
          end;
                   {$IFDEF SHOWPROG}  //flicker with lazarus
                 if (lThread = 1) and ((lZ mod 30)=0) then
                    VisualProg(lZ);
                 {$ENDIF}
	  for lY := l.YDimStart to l.YDimEnd do begin
		  lYxi := lY * lMi.matrix[1,2];
		  lYyi := lY * lMi.matrix[2,2];
		  lYzi := lY * lMi.matrix[3,2];
		  lYxiZxi := (lY * lMi.matrix[1,2]  )+lZxi;
		  lYyiZyi := (lY * lMi.matrix[2,2] )+lZyi;
		  lYziZzi := (lY * lMi.matrix[3,2]  )+lZzi;
		  FindXBounds (lMaxX,lMinX,l.XDimIN,lYxiZxi,l.XPivotInU2,l.YDimIN,lYyiZyi,l.YPivotInU2,l.ZDimIN,lYziZzi,l.ZPivotInU2,l.OutDim,l.Xxra,l.Xyra,l.Xzra);
		  lMaxX := lMaxX - l.OutPivot -1 ;
		  lMinX := lMinX - l.OutPivot+1;
		  if lMaxX > lMinX then
			for lX := lMinX to lMaxX do begin
			  lXr := ( (lX*lMi.matrix[1,1])+lYxi+lZxi)+lXPiv;
			  lYr := ((lX*lMi.matrix[2,1])+lYyi+lZyi)+lYPiv;
			  lZr := ( (lX*lMi.matrix[3,1])+lYzi+lZzi)+lZPiv;
			  lXo := (lXr shr kSh);
			  lYo := (lYr shr kSh);
			  lZo := (lZr shr kSh);
			  if (lXo > 0) and (lXo < l.XDimIn)
			  and (lYo > 0) and (lYo < l.YDimIn) and
			  (lZo > 0) and (lZo < l.ZDimIn) then begin
			   lXr := (lXr- (lXo  * lShl)) shr lShlTo8;
			   lYr := (lYr- (lYo * lShl)) shr lShlTo8;
			   lZr := (lZr- (lZo * lShl)) shr lShlTo8;
			   lXrM1i := lShl8-lXr;
			   lYrM1i := lShl8-lYr;
			   lZrM1i := lShl8-lZr;
			   lMinY := ((lYo-1)*l.XdimIn);
			   lMinZ := ((lZo-1)*l.InSliceSz);
			   lMaxY := ((lYo)*l.XdimIn);
			   lMaxZ := ((lZo)*l.InSliceSz);
		           lOutPos := ((lZ+l.OutPivot-1)*l.OutSliceSz)+((lY+l.OutPivot-1)*l.Outdim);
			   if {(lRenderCutout )  and} ((lBuffIn^[lXo+lMinY+lMinZ]=255) or (lBuffIn^[lXo+1+lMinY+lMinZ]=255)
			   or (lBuffIn^[lXo+lMaxY+lMinZ]=255) or (lBuffIn^[lXo+1+lMaxY+lMinZ]=255)
			   or (lBuffIn^[lXo+lMinY+lMaxZ]=255) or (lBuffIn^[lXo+1+lMinY+lMaxZ]=255)
			   or (lBuffIn^[lXo+lMaxY+lMaxZ]=255) or (lBuffIn^[lXo+1+lMaxY+lMaxZ]=255))
				then lBuffOut^[lX+l.OutPivot+lOutPos] := 255
			   else
                               lBuffOut^[lX+l.OutPivot+lOutPos] :=  (
			    (lXrM1i*lYrM1i*lZrM1i *lBuffIn^[lXo+lMinY+lMinZ] )
			   +(lXr*lYrM1i*lZrM1i *lBuffIn^[lXo+1+lMinY+lMinZ])
			   +(lXrM1i*lYr*lZrM1i *lBuffIn^[lXo+lMaxY+lMinZ] )
			   +(lXrM1i*lYrM1i*lZr *lBuffIn^[lXo+lMinY+lMaxZ] )
			   +(lXr*lYr*lZrM1i *lBuffIn^[lXo+1+lMaxY+lMinZ] )
			   +(lXr*lYrM1i*lZr *lBuffIn^[lXo+1+lMinY+lMaxZ] )
			   +(lXrM1i*lYr*lZr *lBuffIn^[lXo+lMaxY+lMaxZ])
			   +(lXr*lYr*lZr *lBuffIn^[lXo+1+lMaxY+lMaxZ] )
                           ) shr lShr;
			  end; //values in range
		  end; //for x
	   end; //for y
        end; //for z
        ThreadDone;
        exit;
     end; //if RenderCutout
     for lZ := l.ZDimStart to l.ZDimEnd do begin
         lZxi := (lZ*lMi.matrix[1,3]  );
         lZyi := (lZ*lMi.matrix[2,3]  );
         lZzi := (lZ*lMi.matrix[3,3]  );
         if {(RenderForm.RenderRefreshTimer.enabled) or} (Terminated) then begin
             ThreadDone;
             exit;
          end;
                   {$IFDEF SHOWPROG}  //flicker with lazarus
                 if (lThread = 1) and ((lZ mod 30)=0) then
                    VisualProg(lZ);
                 {$ENDIF}
	  for lY := l.YDimStart to l.YDimEnd do begin
		  lYxi := lY * lMi.matrix[1,2];
		  lYyi := lY * lMi.matrix[2,2];
		  lYzi := lY * lMi.matrix[3,2];
		  lYxiZxi := (lY * lMi.matrix[1,2]  )+lZxi;
		  lYyiZyi := (lY * lMi.matrix[2,2] )+lZyi;
		  lYziZzi := (lY * lMi.matrix[3,2]  )+lZzi;
		  FindXBounds (lMaxX,lMinX,l.XDimIN,lYxiZxi,l.XPivotInU2,l.YDimIN,lYyiZyi,l.YPivotInU2,l.ZDimIN,lYziZzi,l.ZPivotInU2,l.OutDim,l.Xxra,l.Xyra,l.Xzra);
		  lMaxX := lMaxX - l.OutPivot -1 ;
		  lMinX := lMinX - l.OutPivot+1;
		  if lMaxX > lMinX then
			for lX := lMinX to lMaxX do begin
			  lXr := ( (lX*lMi.matrix[1,1])+lYxi+lZxi)+lXPiv;
			  lYr := ((lX*lMi.matrix[2,1])+lYyi+lZyi)+lYPiv;
			  lZr := ( (lX*lMi.matrix[3,1])+lYzi+lZzi)+lZPiv;
			  lXo := (lXr shr kSh);
			  lYo := (lYr shr kSh);
			  lZo := (lZr shr kSh);
			  if (lXo > 0) and (lXo < l.XDimIn)
			  and (lYo > 0) and (lYo < l.YDimIn) and
			  (lZo > 0) and (lZo < l.ZDimIn) then begin
			   lXr := (lXr- (lXo  * lShl)) shr lShlTo8;
			   lYr := (lYr- (lYo * lShl)) shr lShlTo8;
			   lZr := (lZr- (lZo * lShl)) shr lShlTo8;
			   lXrM1i := lShl8-lXr;
			   lYrM1i := lShl8-lYr;
			   lZrM1i := lShl8-lZr;
			   lMinY := ((lYo-1)*l.XdimIn);
			   lMinZ := ((lZo-1)*l.InSliceSz);
			   lMaxY := ((lYo)*l.XdimIn);
			   lMaxZ := ((lZo)*l.InSliceSz);
		           lOutPos := ((lZ+l.OutPivot-1)*l.OutSliceSz)+((lY+l.OutPivot-1)*l.Outdim);
                           lBuffOut^[lX+l.OutPivot+lOutPos] :=(
			    (lXrM1i*lYrM1i*lZrM1i *lBuffIn^[lXo+lMinY+lMinZ] )
			   +(lXr*lYrM1i*lZrM1i *lBuffIn^[lXo+1+lMinY+lMinZ])
			   +(lXrM1i*lYr*lZrM1i *lBuffIn^[lXo+lMaxY+lMinZ] )
			   +(lXrM1i*lYrM1i*lZr *lBuffIn^[lXo+lMinY+lMaxZ] )
			   +(lXr*lYr*lZrM1i *lBuffIn^[lXo+1+lMaxY+lMinZ] )
			   +(lXr*lYrM1i*lZr *lBuffIn^[lXo+1+lMinY+lMaxZ] )
			   +(lXrM1i*lYr*lZr *lBuffIn^[lXo+lMaxY+lMaxZ])
			   +(lXr*lYr*lZr *lBuffIn^[lXo+1+lMaxY+lMaxZ] )
                           ) shr lShr;
			  end; //values in range
		  end; //for x
	   end; //for y
	  end; //for z
          ThreadDone;
end;

(*
// floating point version of the same algorithm...
procedure TTriRender.Rotate (lThread: integer;l: TRotateVals; var lM: TMatrix; lRenderCutout: boolean; var lBuffIn,lBuffOut: ByteP);
var
   lXreal,lYreal,lZreal,lZx,lZy,lZz,lYx,lYy,lYz,lXrM1,lYrM1,lZrM1: single;
   lXxi,lXyi,lXzi,lZxi,lZyi,lZzi,lYxiZxi,lYyiZyi,lYziZzi,lZ,lY,lX,lOutPos,
   lMinZ,lMaxZ,lMinY,lMaxY,lMaxX,lMinX,lXo,lYo,lZo: integer;
begin
if (lRenderCutout )  then begin

     for lZ := l.ZDimStart to l.ZDimEnd do begin
	  lZx := lZ*lM.matrix[1,3];
	  lZy := lZ*lM.matrix[2,3];
	  lZz := lZ*lM.matrix[3,3];
		 lZxi := round(lZ*lM.matrix[1,3]* (1 shl kSh)  );
		 lZyi := round(lZ*lM.matrix[2,3]* (1 shl kSh)  );
		 lZzi := round(lZ*lM.matrix[3,3]* (1 shl kSh)  );
	  if RenderForm.RenderRefreshTimer.enabled then exit;//abort
          if Terminated then exit; //goto 345;//abort
          if (lThread = 1) and ((lZ mod 10)=0) then
             VisualProg(lZ);
	  for lY := l.YDimStart to l.YDimEnd do begin
		  lYx := lY * lM.matrix[1,2];
		  lYy := lY * lM.matrix[2,2];
		  lYz := lY * lM.matrix[3,2];
		  lOutPos := ((lZ+l.OutPivot-1)*l.OutSliceSz)+((lY+l.OutPivot-1)*l.Outdim);
		  lYxiZxi := round(lY * lM.matrix[1,2]* (1 shl kSh)  )+lZxi;
		  lYyiZyi := round(lY * lM.matrix[2,2]* (1 shl kSh)  )+lZyi;
		  lYziZzi := round(lY * lM.matrix[3,2]* (1 shl kSh)  )+lZzi;
		  FindXBounds (lMaxX,lMinX,l.XDimIN,lYxiZxi,l.XPivotInU2,l.YDimIN,lYyiZyi,l.YPivotInU2,l.ZDimIN,lYziZzi,l.ZPivotInU2,l.OutDim,l.Xxra,l.Xyra,l.Xzra);
		  lMaxX := lMaxX - l.OutPivot -1 ;
		  lMinX := lMinX - l.OutPivot+1;
		  if lMaxX > lMinX then
			for lX := lMinX to lMaxX do begin
			  lXreal := ( (lX*lM.matrix[1,1])+lYx+lZx)+l.XPivotIn;
			  lYreal := ( (lX*lM.matrix[2,1])+lYy+lZy)+l.YPivotIn;
			  lZreal := ( (lX*lM.matrix[3,1])+lYz+lZz)+l.ZPivotIn;
			  lXo := trunc(lXreal);
			  lYo := trunc(lYreal);
			  lZo := trunc(lZreal);
			  if (lXo > 0) and (lXo < l.XDimIn)
			  and (lYo > 0) and (lYo < l.YDimIn) and
			  (lZo > 0) and (lZo < l.ZDimIn) then begin
			   lXreal := lXreal-lXo;
			   lYreal := lYreal-lYo;
			   lZreal := lZreal-lZo;
			   lXrM1 := 1-lXreal;
			   lYrM1 := 1-lYreal;
			   lZrM1 := 1-lZreal;
			   lMinY := ((lYo-1)*l.XdimIn);
			   lMinZ := ((lZo-1)*l.InSliceSz);
			   lMaxY := ((lYo)*l.XdimIn);
			   lMaxZ := ((lZo)*l.InSliceSz);
			   if {(l.RenderCutout )  and} ((lBuffIn^[lXo+lMinY+lMinZ]=255) or (lBuffIn^[lXo+1+lMinY+lMinZ]=255)
			   or (lBuffIn^[lXo+lMaxY+lMinZ]=255) or (lBuffIn^[lXo+1+lMaxY+lMinZ]=255)
			   or (lBuffIn^[lXo+lMinY+lMaxZ]=255) or (lBuffIn^[lXo+1+lMinY+lMaxZ]=255)
			   or (lBuffIn^[lXo+lMaxY+lMaxZ]=255) or (lBuffIn^[lXo+1+lMaxY+lMaxZ]=255))
				then lBuffOut^[lX+l.OutPivot+lOutPos] := 255
			   else
				   lBuffOut^[lX+l.OutPivot+lOutPos] :=  round (
			   {all min} ( (lXrM1*lYrM1*lZrM1)*lBuffIn^[lXo+lMinY+lMinZ])
			   {x+1}+((lXreal*lYrM1*lZrM1)*lBuffIn^[lXo+1+lMinY+lMinZ])
			   {y+1}+((lXrM1*lYreal*lZrM1)*lBuffIn^[lXo+lMaxY+lMinZ])
			   {z+1}+((lXrM1*lYrM1*lZreal)*lBuffIn^[lXo+lMinY+lMaxZ])
			   {x+1,y+1}+((lXreal*lYreal*lZrM1)*lBuffIn^[lXo+1+lMaxY+lMinZ])
			   {x+1,z+1}+((lXreal*lYrM1*lZreal)*lBuffIn^[lXo+1+lMinY+lMaxZ])
			   {y+1,z+1}+((lXrM1*lYreal*lZreal)*lBuffIn^[lXo+lMaxY+lMaxZ])
			   {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*lBuffIn^[lXo+1+lMaxY+lMaxZ]) );
			  end; //values in range
		  end; //for x
	   end; //for y
	  end; //for z
  ThreadDone;
exit;
end; //rendercutout
     for lZ := l.ZDimStart to l.ZDimEnd do begin
	  lZx := lZ*lM.matrix[1,3];
	  lZy := lZ*lM.matrix[2,3];
	  lZz := lZ*lM.matrix[3,3];
		 lZxi := round(lZ*lM.matrix[1,3]* (1 shl kSh)  );
		 lZyi := round(lZ*lM.matrix[2,3]* (1 shl kSh)  );
		 lZzi := round(lZ*lM.matrix[3,3]* (1 shl kSh)  );
	  if RenderForm.RenderRefreshTimer.enabled then exit;//abort
          if Terminated then exit; //goto 345;//abort
          if (lThread = 1) and ((lZ mod 10)=0) then
             VisualProg(lZ);
	  for lY := l.YDimStart to l.YDimEnd do begin
		  lYx := lY * lM.matrix[1,2];
		  lYy := lY * lM.matrix[2,2];
		  lYz := lY * lM.matrix[3,2];
		  lOutPos := ((lZ+l.OutPivot-1)*l.OutSliceSz)+((lY+l.OutPivot-1)*l.Outdim);
		  lYxiZxi := round(lY * lM.matrix[1,2]* (1 shl kSh)  )+lZxi;
		  lYyiZyi := round(lY * lM.matrix[2,2]* (1 shl kSh)  )+lZyi;
		  lYziZzi := round(lY * lM.matrix[3,2]* (1 shl kSh)  )+lZzi;
		  FindXBounds (lMaxX,lMinX,l.XDimIN,lYxiZxi,l.XPivotInU2,l.YDimIN,lYyiZyi,l.YPivotInU2,l.ZDimIN,lYziZzi,l.ZPivotInU2,l.OutDim,l.Xxra,l.Xyra,l.Xzra);
		  lMaxX := lMaxX - l.OutPivot -1 ;
		  lMinX := lMinX - l.OutPivot+1;
		  if lMaxX > lMinX then
			for lX := lMinX to lMaxX do begin
			  lXreal := ( (lX*lM.matrix[1,1])+lYx+lZx)+l.XPivotIn;
			  lYreal := ((lX*lM.matrix[2,1])+lYy+lZy)+l.YPivotIn;
			  lZreal := ( (lX*lM.matrix[3,1])+lYz+lZz)+l.ZPivotIn;
			  lXo := trunc(lXreal);
			  lYo := trunc(lYreal);
			  lZo := trunc(lZreal);
			  if (lXo > 0) and (lXo < l.XDimIn)
			  and (lYo > 0) and (lYo < l.YDimIn) and
			  (lZo > 0) and (lZo < l.ZDimIn) then begin
			   lXreal := lXreal-lXo;
			   lYreal := lYreal-lYo;
			   lZreal := lZreal-lZo;
			   lXrM1 := 1-lXreal;
			   lYrM1 := 1-lYreal;
			   lZrM1 := 1-lZreal;
			   lMinY := ((lYo-1)*l.XdimIn);
			   lMinZ := ((lZo-1)*l.InSliceSz);
			   lMaxY := ((lYo)*l.XdimIn);
			   lMaxZ := ((lZo)*l.InSliceSz);

                           lBuffOut^[lX+l.OutPivot+lOutPos] :=
                           round (
			   {all min} ( (lXrM1*lYrM1*lZrM1)*lBuffIn^[lXo+lMinY+lMinZ])
			   {x+1}+((lXreal*lYrM1*lZrM1)*lBuffIn^[lXo+1+lMinY+lMinZ])
			   {y+1}+((lXrM1*lYreal*lZrM1)*lBuffIn^[lXo+lMaxY+lMinZ])
			   {z+1}+((lXrM1*lYrM1*lZreal)*lBuffIn^[lXo+lMinY+lMaxZ])
			   {x+1,y+1}+((lXreal*lYreal*lZrM1)*lBuffIn^[lXo+1+lMaxY+lMinZ])
			   {x+1,z+1}+((lXreal*lYrM1*lZreal)*lBuffIn^[lXo+1+lMinY+lMaxZ])
			   {y+1,z+1}+((lXrM1*lYreal*lZreal)*lBuffIn^[lXo+lMaxY+lMaxZ])
			   {x+1,y+1,z+1}+((lXreal*lYreal*lZreal)*lBuffIn^[lXo+1+lMaxY+lMaxZ]) );
			  end; //values in range
		  end; //for x
	   end; //for y
	  end; //for z
          // if Terminated then Exit;
          ThreadDone;
end; *)


initialization
  InitializeCriticalSection(CritSect);


finalization
  DeleteCriticalSection(CritSect);
end.