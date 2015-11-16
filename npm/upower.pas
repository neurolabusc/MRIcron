unit upower;
interface

uses define_types, statcr, distr, dialogsx;
function Sum2Power(lOutImgSum: SingleP; lVolVox,lnTotal,lnDeficit: integer; lBinomial: boolean): boolean;
function Sum2PowerCont(lOutImgSum: SingleP; lVolVox,lnTotal: integer): boolean;
function Sum2PowerBinom(lOutImgSum: SingleP; lVolVox,lnTotal,lnDeficit: integer): boolean;
function k_out_n (k,n: integer): double; //total possible permutations

implementation

function k_out_n (k,n: integer): double; //total possible permutations
//k= smaller group, n=sum of both groups
begin
	if not gFactRAready then InitFact;
	result := round(gFactRA[n] / (gFactRA[k]*gFactRA[n-k]  )  );
// k out n = n!/(k!*(n-k)! which is equal to the PROD(i=k; 1){(n-i+1)/i}
end; //k_out_n

function Sum2Power(lOutImgSum: SingleP; lVolVox,lnTotal,lnDeficit: integer; lBinomial: boolean): boolean;
begin
     if lBinomial then
        result :=  Sum2PowerBinom(lOutImgSum, lVolVox,lnTotal,lnDeficit)
     else
        result :=  Sum2PowerCont(lOutImgSum, lVolVox,lnTotal)
end;

function Sum2PowerCont(lOutImgSum: SingleP; lVolVox,lnTotal: integer): boolean;
//convert Sum image to power map showing maximum possible effect size
//'Cont' version is for continuous data
var
   lDensity,lN,lRank: integer;
   lDensityPowerRA: singleP;
begin
    result := false;
    if (lnTotal < 2)  or (lVolVox < 1) then
       exit;
    getmem(lDensityPowerRA,lnTotal* sizeof(single));
    //no need to compute power for [lnTotal] and [0] - no variability when everyone or no one has a lesion
    //lDensityPowerRA[lnTotal] := 0; //everyone has a lesion = no variability
    lRank := 0;
    for lN := 1 to (lnTotal -1) do begin
        //most power when all participants with a lesion have most extreme behavioural data
        //therefore, they will have the lowest ranks: rank 1,2,3,4
        lRank := lRank + lN;
        if (lnTotal > 360) then //cannot calculate values this large...
            lDensityPowerRA^[lN] := 0
        else if (lN > 10) and (lnTotal > 64) then  //avoid overflow...
            lDensityPowerRA^[lN] := pNormalInv ( 1/(k_out_n(10,lnTotal)) )
        else begin
            lDensityPowerRA^[lN] := 1/(k_out_n(lN,lnTotal)); //compute Wilcoxon probability
            lDensityPowerRA^[lN] := pNormalInv (lDensityPowerRA^[lN]);//convert p to z-score
        end;
        //max power when every possible person with a lesion has a defict, and everyone w/o lesion does not...
        //lDensityPowerRA[lN] := Liebermeister (lLD,lnoLD,lLnoD,lnoLnoD); //probability of this observation
        //lDensityPowerRA[lN] := pNormalInv (lDensityPowerRA[lN]);//convert p to z-score
        //fx(lDensityPowerRA[lN]);
    end;
    //now use lookup table to convert overlay density to effective power
    for lN := 1 to lVolVox do begin
        lDensity := round( lOutImgSum^[lN]);
        if (lDensity > 0) and (lDensity < lnTotal) then
           lOutImgSum^[lN] := lDensityPowerRA^[lDensity]
        else
            lOutImgSum^[lN] := 0;
    end; //for each voxel
    freemem(lDensityPowerRA);
    result := true;
end;

function Sum2PowerBinom(lOutImgSum: SingleP; lVolVox,lnTotal,lnDeficit: integer): boolean;
//convert Sum image to power map showing maximum possible effect size
var
   lDensity,lN,lLD,lLnoD,lnoLD,lnoLnoD: integer;
   lDensityPowerRA: singleP;
begin
    result := false;
    if (lnTotal < 2) or (lnDeficit < 1) or (lVolVox < 1) then
       exit;
    if(lnDeficit >= lnTotal) then begin
       ShowMsg('Sum2Power error: people with deficit must be less than sample size');
       exit;
    end;
    getmem(lDensityPowerRA,lnTotal* sizeof(single));
    //no need to compute power for lnTotal and 0 - no variability when everyone or no one has a lesion
    //lDensityPowerRA[lnTotal] := 0; //everyone has a lesion = no variability
    for lN := 1 to (lnTotal -1) do begin
        //max power when every possible person with a lesion has a defict, and everyone w/o lesion does not...
        if lN > lnDeficit then begin
            lLD := lnDeficit;
            lLnoD := lN - lnDeficit;
        end else begin
            lLD := lN;
            lLnoD := 0;
        end;
        lnoLD := lnDeficit-lLD; //number of people with deficit who do not have a lesion - as close to zero as possible
        lnoLnoD := lnTotal-lnoLD-lLnoD-lLD;
        lDensityPowerRA^[lN] := Liebermeister (lLD,lnoLD,lLnoD,lnoLnoD); //probability of this observation
        lDensityPowerRA^[lN] := pNormalInv (lDensityPowerRA^[lN]);//convert p to z-score
        //fx(lLD,lnoLD,lLnoD,lnoLnoD,lDensityPowerRA[lN]);
    end;
    //now use lookup table to convert overlay density to effective power
    for lN := 1 to lVolVox do begin
        lDensity := round( lOutImgSum^[lN]);
        if (lDensity > 0) and (lDensity < lnTotal) then
           lOutImgSum^[lN] := lDensityPowerRA^[lDensity]
        else
            lOutImgSum^[lN] := 0;

    end; //for each voxel
    freemem(lDensityPowerRA);
    result := true;
end;


end.