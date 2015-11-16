unit fdr;
interface

uses define_types;
procedure EstimateFDR2(lnTests: integer; var Ps: SingleP; var lFDR05, lFDR01,lnegFDR05, lnegFDR01: double);
procedure qsort(lower, upper : integer; var Data:SingleP);
implementation

procedure qsort(lower, upper : integer; var Data:SingleP);
//40ms - very recursive...
var
       left, right : integer;
       pivot,lswap: single;
begin
     pivot:=Data^[(lower+upper) div 2];
     left:=lower;
     right:=upper;
     while left<=right do begin
             while Data^[left]  < pivot do left:=left+1;  { Parting for left }
             while Data^[right] > pivot do right:=right-1;{ Parting for right}
             if left<=right then begin   { Validate the change }
                 lswap := Data^[left];
                 Data^[left] := Data^[right];
                 Data^[right] := lswap;
                 left:=left+1;
                 right:=right-1;
             end; //validate
     end;//while left <=right
     if right>lower then qsort(lower,right,Data); { Sort the LEFT  part }
     if upper>left  then qsort(left ,upper,data); { Sort the RIGHT part }
end;

procedure EstimateFDR2(lnTests: integer; var Ps: SingleP; var lFDR05, lFDR01,lnegFDR05, lnegFDR01: double);
var
	lInc: integer;
	lrPs,Qs: SingleP;
begin
	//rank Pvalues
        //ShaQuickSort(lnTests,Singlep0(Ps[1]));
        qSort(1,lnTests,Ps);
	//qcksrt(1,lnTests,Ps);
	GetMem(Qs,lnTests*sizeof(single));
	//next findcrit FDR05
	for lInc := 1 to lnTests do
		Qs^[lInc] := (0.05*lInc)/lnTests;
	lFDR05 := 0;
	for lInc := 1 to lnTests do
		if Ps^[lInc] <= Qs^[lInc] then
				lFDR05 := Ps^[lInc];
	//next findcrit FDR01
	for lInc := 1 to lnTests do
		Qs^[lInc] := (0.01*lInc)/lnTests;
	lFDR01 := 0;
	for lInc := 1 to lnTests do
		if Ps^[lInc] <= Qs^[lInc] then
				lFDR01 := Ps^[lInc];
        //reverse
        GetMem(lrPs,lnTests*sizeof(single));
	for lInc := 1 to lnTests do
		lrPs^[lInc] := 1- Ps^[lnTests-lInc+1];
	for lInc := 1 to lnTests do
		Qs^[lInc] := (0.05*lInc)/lnTests;
	lnegFDR05 := 0;
	for lInc := 1 to lnTests do
		if lrPs^[lInc] <= Qs^[lInc] then
				lnegFDR05 := lrPs^[lInc];
	//next findcrit FDR01
	for lInc := 1 to lnTests do
		Qs^[lInc] := (0.01*lInc)/lnTests;
	lnegFDR01 := 0;
	for lInc := 1 to lnTests do
		if lrPs^[lInc] <= Qs^[lInc] then
				lnegFDR01 := lrPs^[lInc];
        FreeMem(lrPs);
	Freemem(Qs);
end;

end.
