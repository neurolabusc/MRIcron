{ ******************************************************************
  This programs tests the accuracy of the special functions.
  The data file SPECFUNC.DAT has been modified from the
  'Numerical Recipes' example file.
  ****************************************************************** }

program specfunc;

uses
  crt, tpmath;

const
  FileName = 'specfunc.dat';
  Blank    = '    ';

  procedure Pause;
  var
    Ch : Char;
  begin
    Writeln;
    Write('Press a key to continue');
    Ch := ReadKey;
    Writeln;
    Writeln;
  end;

  procedure Test_Fact;
  var
    I, M, N : Integer;
    Y, Ref, R : Float;
    F : Text;
    S : String;
  begin
    Assign(F, FileName);
    Reset(F);
    repeat
      ReadLn(F, S);
    until S = 'N-factorial';
    ReadLn(F, M);
    WriteLn('  X                        Fact(N)                     Reference     Rel.Error');
    WriteLn('------------------------------------------------------------------------------');
    for I := 1 to M do
      begin
        ReadLn(F, N, Ref);
        Y := Fact(N);
        R := (Y - Ref) / Ref;
        WriteLn(N:4, Blank, Y:26, Blank, Ref:26, Blank, R:10);
      end;
    Close(F);
    Pause;
  end;

  procedure Test_Binomial;
  var
    I, M, N, K : Integer;
    Y, Ref, R : Float;
    F : Text;
    S : String;
  begin
    Assign(F, FileName);
    Reset(F);
    repeat
      ReadLn(F, S);
    until S = 'Binomial Coefficients';
    ReadLn(F, M);
    WriteLn(' N    K    Binomial(N, K)         Reference           Rel.Error');
    WriteLn('---------------------------------------------------------------');
    for I := 1 to M do
      begin
        ReadLn(F, N, K, Ref);
        Y := Binomial(N, K);
        R := (Y - Ref) / Ref;
        WriteLn(N:2, K:5, '     ', Y:13:0, '     ', Ref:13:0, '          ', R:10);
      end;
    Close(F);
    Pause;
  end;

  procedure Test_Gamma;
  var
    I, M : Integer;
    X, Y, Ref, R : Float;
    F : Text;
    S : String;
  begin
    Assign(F, FileName);
    Reset(F);
    repeat
      ReadLn(F, S);
    until S = 'Gamma Function';
    ReadLn(F, M);
    WriteLn('  X                       Gamma(X)                     Reference     Rel.Error');
    WriteLn('------------------------------------------------------------------------------');
    for I := 1 to M do
      begin
        ReadLn(F, X, Ref);
        Y := Gamma(X);                        { To test Gamma }
      { Y := SgnGamma(X) * Exp(LnGamma(X)); } { To test LnGamma }
        R := (Y - Ref) / Ref;
        WriteLn(X:4:1, Blank, Y:26, Blank, Ref:26, Blank, R:10);
      end;
    Close(F);
    Pause;
  end;

  procedure Test_IGamma;
  var
    I, M : Integer;
    A, X, Y, R, Ref : Float;
    F : Text;
    S : String;
  begin
    Assign(F, FileName);
    Reset(F);
    repeat
      ReadLn(F, S);
    until S = 'Incomplete Gamma Function';
    ReadLn(F, M);
    WriteLn('  A       X                   IGamma(A, X)                 Reference  Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to M do
      begin
        ReadLn(F, A, X, Ref);
        Y := IGamma(A, X);
        R := (Y - Ref) / Ref;
        WriteLn(A:4:1, X:12:8, Y:26, Ref:26, ' ', R:10);
      end;
    Close(F);
    Pause;
  end;

  procedure Test_Beta;
  var
    I, M : Integer;
    X, Y, Z, R, Ref : Float;
    F : Text;
    S : String;
  begin
    Assign(F, FileName);
    Reset(F);
    repeat
      ReadLn(F, S);
    until S = 'Beta Function';
    ReadLn(F, M);
    WriteLn('  X     Y                   Beta(X, Y)                   Reference    Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to M do
      begin
        ReadLn(F, X, Y, Ref);
        Z := Beta(X, Y);
        R := (Z - Ref) / Ref;
        WriteLn(X:4:1, '  ', Y:4:1, '  ', Z:26, '  ', Ref:26, '   ', R:10);
      end;
    Close(F);
    Pause;
  end;

  procedure Test_IBeta;
  var
    I, M : Integer;
    A, B, X, Y, R, Ref : Float;
    F : Text;
    S : String;
  begin
    Assign(F, FileName);
    Reset(F);
    repeat
      ReadLn(F, S);
    until S = 'Incomplete Beta Function';
    ReadLn(F, M);
    WriteLn('  A    B   X               IBeta(A, B, X)                  Reference  Rel.Error');
    WriteLn('-------------------------------------------------------------------------------');
    for I := 1 to M do
      begin
        ReadLn(F, A, B, X, Ref);
        Y := IBeta(A, B, X);
        R := (Y - Ref) / Ref;
        WriteLn(A:4:1, ' ', B:4:1, ' ', X:4:2, ' ', Y:26, ' ', Ref:26, ' ', R:10);
      end;
    Close(F);
    Pause;
  end;

  procedure Test_Erf;
  var
    I, M : Integer;
    X, Y, R, Ref : Float;
    F : Text;
    S : String;
  begin
    Assign(F, FileName);
    Reset(F);
    repeat
      ReadLn(F, S);
    until S = 'Error Function';
    ReadLn(F, M);
    WriteLn('  X                         Erf(X)                     Reference     Rel.Error');
    WriteLn('------------------------------------------------------------------------------');
    for I := 1 to M do
      begin
        ReadLn(F, X, Ref);
        Y := Erf(X);
        R := (Y - Ref) / Ref;
        WriteLn(X:4:1, Blank, Y:26, Blank, Ref:26, Blank, R:10);
      end;
    Close(F);
    Pause;
  end;

  procedure Test_DiGamma;
  var
    I, M : Integer;
    X, Y, Ref, R : Float;
    F : Text;
    S : String;
  begin
    Assign(F, FileName);
    Reset(F);
    repeat
      ReadLn(F, S);
    until S = 'DiGamma Function';
    ReadLn(F, M);
    WriteLn('  X                     DiGamma(X)                     Reference     Rel.Error');
    WriteLn('------------------------------------------------------------------------------');
    for I := 1 to M do
      begin
        ReadLn(F, X, Ref);
        Y := DiGamma(X);
        R := (Y - Ref) / Ref;
        WriteLn(X:6:2, Blank, Y:25, Blank, Ref:25, Blank, R:10);
      end;
    Close(F);
    Pause;
  end;

  procedure Test_TriGamma;
  var
    I, M : Integer;
    X, Y, Ref, R : Float;
    F : Text;
    S : String;
  begin
    Assign(F, FileName);
    Reset(F);
    repeat
      ReadLn(F, S);
    until S = 'TriGamma Function';
    ReadLn(F, M);
    WriteLn('  X                    TriGamma(X)                     Reference     Rel.Error');
    WriteLn('------------------------------------------------------------------------------');
    for I := 1 to M do
      begin
        ReadLn(F, X, Ref);
        Y := TriGamma(X);
        R := (Y - Ref) / Ref;
        WriteLn(X:6:2, Blank, Y:25, Blank, Ref:25, Blank, R:10);
      end;
    Close(F);
    Pause;
  end;

begin
  Test_Fact;
  Test_Binomial;
  Test_Gamma;
  Test_IGamma;
  Test_Beta;
  Test_IBeta;
  Test_Erf;
  Test_DiGamma;
  Test_TriGamma;
end.
