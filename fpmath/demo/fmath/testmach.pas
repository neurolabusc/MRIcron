{ ******************************************************************
  This program displays the floating point type and the machine-
  dependent constants.
  ****************************************************************** }

program testmach;

uses
  tpmath;

var
  N : Byte;

begin
  writeln;
  writeln('Integer type        = Integer (', sizeof(Integer), ' bytes)');
  writeln('Long Integer type   = LongInt (', sizeof(LongInt), ' bytes)');

  N := sizeof(Float);
  write('Floating point type = ');
  if N = sizeof(Single) then
    write('Single')
  else if N = sizeof(Double) then
    write('Double')
  else if N = sizeof(Extended) then
    write('Extended')
  else
    write('Real');

  writeln(' (', N, ' bytes)');

  writeln('Complex type        = Complex (', sizeof(Complex), ' bytes)');

  writeln;
  writeln('MachEp          = ', MachEp);

  writeln;
  writeln('MinNum          = ', MinNum);
  writeln('Exp(MinLog)     = ', Exp(MinLog));

  writeln;
  writeln('MinLog          = ', MinLog);
  writeln('Ln(MinNum)      = ', Ln(MinNum));

  writeln;
  writeln('MaxNum          = ', MaxNum);
  writeln('Exp(MaxLog)     = ', Exp(MaxLog));

  writeln;
  writeln('MaxLog          = ', MaxLog);
  writeln('Ln(MaxNum)      = ', Ln(MaxNum));

  writeln;
  writeln('MaxFac          =  ', MaxFac);
  writeln('Fact(MaxFac)    = ', Fact(MaxFac));

  writeln;
  writeln('MaxGam          = ', MaxGam);
  writeln('Gamma(MaxGam)   = ', Gamma(MaxGam));

  writeln;
  writeln('MaxLgm          = ', MaxLgm);
  writeln('LnGamma(MaxLgm) = ', LnGamma(MaxLgm));
end.

