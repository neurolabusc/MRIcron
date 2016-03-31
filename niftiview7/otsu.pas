unit otsu;
interface
uses define_types;

function DoOtsu (var Img: Bytep; nVox: integer): boolean;

implementation

function DoOtsu (var Img: Bytep; nVox: integer): boolean;
var
  n: integer;
  lHisto: HistoRA;
begin
  result := false;
  if nVox < 1 then exit;
  //create histogram
  for n := 0 to 255 do
    lHisto[n] := 0;
  for n := 0 to nVox do
    inc(lHisto[Img^[n]]);
  //now find minimum intraclass variance....
  (*min :=  (wbk*varbk)+(wfg*varfg);
  for n=1 to 255 do begin
    [wbk,varbk]=calculate(1,i);
    [wfg,varfg]=calculate(i+1,255);
    % After calculating the weights and the variance, the final computation is stored in the array ‘result’.
    result(i+1)=(wbk*varbk)+(wfg*varfg);
  end;*)

  result := true;
end;

Function OtsuWeightedVar(MinIndex,MaxIndex: integer): double;
var
 Weight, Variance: double;
begin
(*  //Weight Calculation
  weight=sum(H(m:n))/sum(H);
  //Mean Calculation
value=H(m:n).*Index(m:n);
total=sum(value);
mean=total/sum(H(m:n));
if(isnan(mean)==1)
	mean=0;
end

%Variance calculation.
value2=(Index(m:n)-mean).^2;
numer=sum(value2.*H(m:n));
var=numer/sum(H(m:n));
if(isnan(var)==1)
    var=0;
end*)
end;


end.
