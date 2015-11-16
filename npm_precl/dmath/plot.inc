{ **********************************************************************
  *                               PLOT.INC                             *
  **********************************************************************
         Variables and routines common to PLOT.PAS and WINPLOT.PAS
  ********************************************************************** }

var
  XminPixel, YminPixel : Integer;  { Pixel coord. of upper left corner }
  XmaxPixel, YmaxPixel : Integer;  { Pixel coord. of lower right corner }
  FactX, FactY         : Float;    { Scaling factors }

  function Xpixel(X : Float) : Integer;
  var
    P : Float;
  begin
    P := FactX * (X - XAxis.Min);
    if Abs(P) > 30000 then
      Xpixel := 30000
    else
      Xpixel := Round(P) + XminPixel;
  end;

  function Ypixel(Y : Float) : Integer;
  var
    P : Float;
  begin
    P := FactY * (YAxis.Max - Y);
    if Abs(P) > 30000 then
      Ypixel := 30000
    else
      Ypixel := Round(P) + YminPixel;
  end;

  function Xuser(X : Integer) : Float;
  begin
    Xuser := XAxis.Min + (X - XminPixel) / FactX;
  end;

  function Yuser(Y : Integer) : Float;
  begin
    Yuser := YAxis.Max - (Y - YminPixel) / FactY;
  end;

  procedure Interval(X1, X2 : Float; MinDiv, MaxDiv : Integer;
                     var Min, Max, Step : Float);
  var
    H, R, K : Float;
  begin
    if X1 >= X2 then Exit;
    H := X2 - X1;
    R := Int(Log10(H));
    if H < 1.0 then R := R - 1.0;
    Step := Exp10(R);

    repeat
      K := Int(H / Step);
      if K < MinDiv then Step := 0.5 * Step;
      if K > MaxDiv then Step := 2.0 * Step;
    until (K >= MinDiv) and (K <= MaxDiv);

    Min := Step * Int(X1 / Step);
    Max := Step * Int(X2 / Step);
    while Min > X1 do Min := Min - Step;
    while Max < X2 do Max := Max + Step;
  end;

  procedure AutoScale(Z : PVector; Lbound, Ubound : Integer;
                      var Axis : TAxis);
  var
    I                  : Integer;
    Zmin, Zmax, Z1, Z2 : Float;
  begin
    if Axis.Scale = LIN_SCALE then
      Interval(Min(Z, Lbound, Ubound), Max(Z, Lbound, Ubound),
               2, 6, Axis.Min, Axis.Max, Axis.Step)
    else
      begin
        Zmin := MAXNUM; Zmax := 0.0;
        for I := Lbound to Ubound do
          if Z^[I] > 0.0 then
            if Z^[I] < Zmin then
              Zmin := Z^[I]
            else if Z^[I] > Zmax then
              Zmax := Z^[I];
        Z1 := Int(Log10(Zmin));
        Z2 := Int(Log10(Zmax));
        if Zmin < 1.0 then Z1 := Z1 - 1.0;
        if Zmax > 1.0 then Z2 := Z2 + 1.0;
        Axis.Min := Z1;
        Axis.Max := Z2;
        Axis.Step := 1.0;
      end;
  end;

