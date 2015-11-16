unit firth;

interface
uses
   {$Include ..\common\isgui.inc}
  //ComCtrls,Classes, Graphics, ExtCtrls,
   //{$IFDEF FPC}ComCtrls, {$ENDIF}
   {$IFDEF GUI} ComCtrls,{$ENDIF}  //progressbar
   classes,define_types,{stats,}StatThdsUtil,lesion_pattern,Mat,Math,Distr,Vector,dialogsx, unpm, SysUtils;

procedure FirthAnalyzeNoThread(lnCond, lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount : integer; lPlankImg: bytep;lOutImgMn,lSymptomRA: SingleP;lOutImg: SingleRAp);

implementation

procedure VisualProg(lPos,lTestNumber: Integer);
begin
   NPMTitleMsg(inttostr(lTestNumber));
     NPMProgressBar( lPos);
end;

var
   finalloglik: SingleP0;
   KxKA1,KxKB1,KxKA,KxKB :TMatrix;
Kvec,Kvec1 : TVector;
Kveci,kVeci1 : TVectori;
   betak,xbeta,yin,pi,ustar,
   XXx,XXXW2,XXFisher,XXcovs,XXXWPrime,
   deltahalfs,deltat,delta,covs,x,Fisher,XW2,W,XWprime,Hprime,H,ustarmat,negx: TMatrix;
    lBarX: TProgressBar;
    lnCondx,lnCritx,lBarPosX,lnPermuteX,lThreadx,lThreadStartx,lThreadEndx,lStartVoxx,lVoxPerPlankx,lImagesCountx : integer;
    lPlankImgx: byteP;lOutImgMnx,lSymptomRAx: SingleP;
    lOutImgX: SingleRAp;


procedure logistfx (xin: SingleP; var lZvals: SingleP0; numSubj,numCond: integer; lComputeIntercept: boolean);
//todo zero output incase exit
//yin = 1..numSubj binary 0/1 values
//xin = numSubj*numCond predictors
//Chivals = 0..numCond p-values - the 0th Khi-value is the intercept
//  [0th value will not be computed if ; lComputeIntercept= false]
label
     123,666;
const
   maxit = 25;
   maxhs = 5;
   epsilon = 0.0001;
   maxstep = 10;
var
   SumY0,SumY1,mx, beta0,loglik,loglikold: double;
   sumy, n, i,j, k, iter,halfs,lCond,dropCond: integer;
   variability,firth: boolean;
procedure crossprodustar;
var
   inc,row: integer;
begin
     for row := 1 to k do begin
         ustarmat[row,1] := 0;
         for inc := 1 to ustar.r do
             ustarmat[row,1] := ustarmat[row,1] + (x[row,inc]*ustar[inc,1]);
     end;
end;
procedure Diag2Vec;
var
   inc: integer;
begin
     for inc := 1 to pi.r do
         ustar[inc,1] := ustar[inc,1]+ H[inc,inc]*(0.5-pi[inc,1]);
end; //nested DiagP2
procedure DiagP2 (var W, P: TMatrix);
var
   inc: integer;
begin
     W.Zero;
     for inc := 1 to P.r do
             W[inc,inc] := Power((P[inc,1] * (1-P[inc,1])),0.5) ;
end; //nested DiagP2
procedure ComputeFisher;
begin
    DiagP2(W,pi);
    XW2.mult(x,W);
    //XWPrime.copy( XW2);
    //XWPrime.transpose;
    XWPrime.transpose(XW2);
    Fisher.mult(XW2,XWPrime);
    covs.copy( Fisher);
    covs.Invert2(KxKA,KxKB,Kvec,Kveci)
end; //nested computeFisher

procedure  computedropdelta;
var
   jinc,iinc,ii,jj: integer;
begin
       DiagP2(W,pi);
       XXXW2.mult(XXx,W);
       //XXXWPrime.copy( XXXW2);
       //XXXWPrime.transpose;
       XXXWPrime.transpose(XXXW2);
       XXFisher.mult(XXXW2,XXXWPrime);
       XXcovs.copy( XXFisher);
       //XXcovs.Invert;
       XXcovs.Invert2(KxKA1,KxKB1,Kvec1,Kveci1);
       covs.Zero;
       ii := 0;
       for iinc := 1 to (k) do begin
           if iinc <> (dropCond+1) then begin //leave the specified column zeros...
                inc(ii);
                jj := 0;
                for jinc := 1 to (k) do begin
                    if jinc <> (dropCond+1) then begin
                       inc(jj);
                       covs[iinc,jinc] := xxCovs[ii,jj];
                    end;
                end;
           end;
       end;
end;
function firthpenalty: double;
begin
    ComputeFisher;
    //result := 0.5 * ln(abs(Fisher.det));
    result := 0.5 * ln(abs(Fisher.Det2(KxKA,kVeci,kVec)));
end; //nested firthpenalty
function ComputeLogLik: double;
var
   inc: integer;
   lDenom: double;
begin
    xbeta.mult(betak,negx);
    for inc := 1 to n do begin
        lDenom := (1 + exp( xbeta[inc,1]));
        if lDenom = 0 then
           showMsg('yikes')
        else
            pi[inc,1] := 1/lDenom;
    end;
     result := 0;
     for inc := 1 to n do
         if yin[inc,1] = 1 then
            //if pi[inc,1] <> 1 then
            result := result+ln(pi[inc,1]);
     for inc := 1 to n do
         if yin[inc,1] = 0 then
            //if pi[inc,1] <> 1 then
               result := result+ln(1-pi[inc,1]);
     if firth then
        result := result + firthpenalty;
end;//nested ComputeLogLik
begin
   for i := 0 to (numCond) do
       lZVals^[i] := 0; //
   if (numSubj < 2) or (numCond < 1) then
      exit;
      //ensure there is some variability in the input data...
   variability := false;
   i := 1;
   repeat
         inc(i);
         if xin^[i] <> xin^[1] then
            variability := true;
   until (i= (numSubj*numCond)) or (variability);
   if not variability then
      exit; //no variance in the regressors...
   variability := false;
   i := 1;
   repeat
         inc(i);
         if yin[i,1] <> yin[1,1] then
            variability := true;
   until (i= (numSubj)) or (variability);
   if not variability then
      exit; //no variance in the dependent variable...
   dropCond := -1; //initially compute full model, then compute effect of removing individual conditions
   firth := true;
   n := numSubj;
   k := numCond + 1;
   //get memory
    //beta := TMatrix.Create(n,1);
   //design our model
   //first row = 1: ell samples have equal weight
   for i := 1 to n do
        x.M[1,i] := 1;
   //next load model into x
   iter := 0;
   for j := 2 to k do
       for i := 1 to n do begin
           inc(iter);
           x.M[j,i] := xin^[iter];
       end;
   //WriteMatrix('Observations',y);
   //WriteMatrix('Model',x);
   //negx is just sing-swapped - we will generate this as we use it a lot...
   for j := 1 to k do
       for i := 1 to n do begin
           negx.M[j,i] := -x.M[j,i];
       end;
    //now start computations
    sumy := 0;
    for i := 1 to n do
        sumy := sumy + round(yin[i,1]);
    if (sumy <= 0) or (sumy >= n) then begin
        //serious error: no variability. This should have been detected earlier in the procedure when yin was tested for variability
        goto 666;
    end;
    beta0 := ln((sumy/n)/(1 - sumy/n));//initial estimate
123:    //go here for each dropcond
    if DropCond >= 0 then begin
       betak.Ones;
       betak.mult( 0) //start with a null model... does not really make sense
    end else begin
       betak.zero;
       betak[1,1] := (beta0);
    end;
    iter :=  0;
    if DropCond >= 0 then begin //drop one of the factors...
       if dropCond <> 0 then begin//include intercept
          for i := 1 to n do
              XXx.M[1,i] := 1;
              lCond := 1;
       end else
           lCond := 0;
       for j := 1 to NumCond do begin
           if j <> DropCond then begin
              inc(lCond);
              for i := 1 to n do
                  XXx.M[lCond,i] := x.M[j+1,i];
           end; //if j <> dropCond
       end;
    end;//if lDropCond >= 0
    loglik  := ComputeLogLik;
    repeat
          inc(iter);
          ComputeFisher;
          HPrime.mult(XWPrime,covs);
          H.mult(HPrime,XW2);
          //WriteMatrix(covs);
          ustar.Sub(yin,pi);
          if firth then
             Diag2Vec;
          crossprodustar;
          if dropCond >= 0 then // model with dropped factor
             computedropdelta;
          deltat.mult(covs,ustarmat);
          delta.transpose(deltat);
          mx := delta.MatAbsMax/MaxStep;
          if mx > 1 then
             delta.mult(mx);//scale delta
          betak.add(delta);
          loglikold := loglik;
          halfs := 1;
          while halfs <= maxhs do begin // Half-Steps
            //fx(iter,halfs,loglik);
            loglik  := ComputeLogLik;
            deltahalfs.mult(delta,power(2,-halfs));
            betak.sub(deltahalfs);
            if (loglik > loglikold) then
                break;
            inc(halfs);
          end;
          if  delta.MatAbsMax <= epsilon then break;
    until (iter >= maxit);
    //fx(DropCond,loglik);
    //done with this model - record model fit
    if DropCond < 0 then
       finalloglik^[k] := loglik  //full model
    else begin
        finalloglik^[DropCond] := loglik; //model with a factor removed
    end;
    if DropCond < numCond then begin
       inc(DropCond);
       if (DropCond = 0) and (not lComputeIntercept) then  //only compute intercept model if requested
          inc(DropCond);
       goto 123;

    end;
    //finally - results

    //ResultsForm.Memo1.lines.add (inttostr(j)+' cases have Y=0, '+inttostr(n-j)+' cases have Y=1');
    if lComputeIntercept then
       J := 0
    else
        J := 1;
    for i := J to (k-1) do begin
        lZVals^[i] := abs(2*(finalloglik^[i]-finalloglik^[k]));
        //find direction of effect - does a larger value of the IV predict more zeros or ones
        lZVals^[i] := pNormalInv(ChiSq(lZVals^[i],1));
        //we have now computed a Z scores - but Chi is one tailed, so all Z > 0... lets check direction
        Sumy0 := 0;
        Sumy1 := 0;
        for iter := 1 to n do begin
            if yin[iter,1] = 0 then
               Sumy0 := Sumy0 + x.M[i+1,iter] //+1: M indexed from 1, ZVal indexed from 0
            else
                Sumy1 := Sumy1 + x.M[i+1,iter]; //+1 M indexed from 1
        end;
        //compute means
        Sumy1 := Sumy1/sumy;
        Sumy0 := Sumy0/(n-sumy);
        if Sumy0 < Sumy1 then //negative z-scores: damage here predicts performance is BETTER
           lZVals^[i] := -lZVals^[i];
    end;
    (*if lComputeIntercept then //intercept is the 0th value
       lChiVals[0] := abs(2*(finalloglik[0]-finalloglik[k]));
    for i := 1 to (k-1) do  //k-1 as this is indexed from 0
        lChiVals[i] := abs(2*(finalloglik[i]-finalloglik[k])); *)

666:
end;

//FirthAnalyzeNoThread (lnCond,lnCrit, lnPermute,1,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImages.Count,lPlankImg,lOutImgSum,lSymptomRA,lOutImg);

procedure FirthAnalyzeNoThread(lnCond, lnCrit,lnPermute,lThread,lThreadStart,lThreadEnd,lStartVox,lVoxPerPlank,lImagesCount : integer; lPlankImg: bytep;lOutImgMn,lSymptomRA: SingleP;lOutImg: SingleRAp);
//calls logistf (yin,xin: SingleP; var lChivals: SingleP0; numSubj,numCond: integer);
label
666;
const
     knPrevPattern = 10;
var
   lPrevPatternRA: array[1..knPrevPattern] of TLesionPattern;
   lPattern: TLesionPattern;
   lObs: Bytep;
   lPrevZVals: array [1..knPrevPattern] of SingleP0;
   lZVals: SingleP0;
   lPatternPos,lC,lnLesion,lPosPct,lPos,lPos2,lPos2Offset,lnCritLocal,n,k: integer;
(*   myFile : TextFile;
procedure Hdr2File;
var
   myI: integer;
begin
     for myI := 1 to (lImagesCount*lnCond ) do begin
         write(myFile, floattostr(lSymptomRA^[myI])+kTab);
     end;
     WriteLn(myFile, '');
end;

procedure Data2File;
var
   myI: integer;
begin
     for myI := 1 to (lImagesCount ) do begin
         write(myFile, inttostr(round(yIn[myI,1]))+kTab);
     end;
     WriteLn(myFile, '');
end; *)

begin //statthread
    (*Assign(myFile,'/Users/rorden/logreg.txt');
     ReWrite(myFile);
       Hdr2File; *)
    lnCritLocal := lnCrit;
   if lnCritLocal < 1 then
      lnCritLocal := 1;
   Getmem(lObs,lImagesCount*sizeof(byte));
   Getmem(lZVals,(lnCond+1)*sizeof(single));
   for lPos := 1 to knPrevPattern do
       Getmem(lPrevZVals[lPos],(lnCond+1)*sizeof(single));
   n := lImagesCount;
   k := lnCond + 1;
   yin:= TMatrix.Create(n,1);
   GetMem(finalloglik,(k+1)*sizeof(single));//finalloglik := TVector.Create(k+1);
   x := TMatrix.Create (k, n);
betak:=TMatrix.Create(1,k);
covs:=TMatrix.Create(k,k);
delta:=TMatrix.Create(1,k);
deltahalfs:=TMatrix.Create(1,k);
deltat:=TMatrix.Create(k,1);
Fisher:=TMatrix.Create(k,k);
H:=TMatrix.Create(n,n);
HPrime:=TMatrix.Create(n,k);
negx:=TMatrix.Create(k,n);
pi:=TMatrix.Create(n,1);
ustar:=TMatrix.Create(n,1);
ustarmat:=TMatrix.create(k,1);
W:=TMatrix.Create(n,n);
xbeta:=TMatrix.Create(1,n);
XW2:=TMatrix.Create(k,n);
//XWPrime:=TMatrix.Create(k,n);
XWPrime:=TMatrix.Create(n,k);
XXcovs:=TMatrix.Create(k-1,k-1);
XXFisher:=TMatrix.Create(k-1,k-1);
XXx:=TMatrix.Create(k-1,n);
XXXW2:=TMatrix.Create(k-1,n);
//XXXWPrime:=TMatrix.Create(k-1,n);
XXXWPrime := TMatrix.Create ( n, k-1);
KxKA := TMatrix.Create(k,k);
KxKB := TMatrix.Create(k,k);
Kvec := TVector.Create(k);
Kveci := TVectori.Create(k);
KxKA1 := TMatrix.Create(k-1,k-1);
KxKB1 := TMatrix.Create(k-1,k-1);
Kvec1 := TVector.Create(k-1);
Kveci1 := TVectori.Create(k-1);

   lPosPct := (lThreadEnd-lThreadStart) div 100;
  for lPatternPos := 1 to knPrevPattern do
   lPrevPatternRA[lPatternPos] := EmptyOrder;
  lPatternPos := 1;
  for lPos2 := lThreadStart to lThreadEnd do begin
       if (lThread = 1) and ((lPos2 mod lPosPct) = 0) or  ((gnVoxTestedRA[lThread] mod 100) = 0) then
           VisualProg(round((lPos2/(lThreadEnd-lThreadStart))*100), gnVoxTestedRA[lThread]);
       lPos2Offset := lPos2+lStartVox-1;
       lnLesion := 0;

       for lPos := 1 to lImagesCount do begin
            if lPlankImg^[((lPos-1)* lVoxPerPlank)+lPos2] = 0 then begin
               //no lesion
               yin[lPos,1] := 0;
               lObs^[lPos] := 0;
            end else begin
                //lesion
                inc(lnLesion);
                lObs^[lPos] := 1;
                yin[lPos,1] := 1; //note: lObs indexed from zero!
            end;
        end;
        lOutImgMn^[lPos2Offset] := lnLesion;///lImages.Count;
           if (lnLesion >= lnCritLocal) and (lnLesion < lImagesCount) then begin
              lPattern := SetOrderX (lObs,lImagesCount);
              lPos := 1;
              while (lPos <= knPrevPattern) and not (SameOrder(lPattern,lPrevPatternRA[lPos],lImagesCount)) do
                    inc(lPos);
              if SameOrder(lPattern,lPrevPatternRA[lPos],lImagesCount) then begin

                  //logistfx(lObs,lSymptomRA, lZvals, lImagesCount,lnCond,false);
                  for lC := 1 to lnCond do
                      lOutImg^[lC]^[lPos2Offset] := lPrevZvals[lPos]^[lC];
              end else begin //new pattern - need to compute
                  inc(gnVoxTestedRA[lThread]);
                  //logistfx(lSymptomRA, lZvals, lImagesCount,lnCond,false);
                  for lC := 1 to lnCond do
                      lOutImg^[lC]^[lPos2Offset] := lZvals^[lC];
                  lPrevPatternRA[lPatternPos] := lPattern;
                  for lC := 1 to lnCond do
                      lPrevZVals[lPatternPos]^[lC] := lZvals^[lC];
                  inc(lPatternPos);
                  if lPatternPos > knPrevPattern then
                     lPatternPos := 1;

              end; //new pattern
           end; //nlesion > nCritical

    end; //for each voxel
    //gMat := false;
666:
    freemem(lObs);
   for lPos := 1 to knPrevPattern do
    freemem(lPrevZVals[lPos]);
    freemem(lZVals);

yin.free;
x.free;
betak.free;
covs.free;
delta.free;
deltahalfs.free;
deltat.free;
Fisher.free;
H.free;
HPrime.free;
negx.free;
pi.free;
ustar.free;
ustarmat.Free;
W.free;
xbeta.free;
XW2.free;
XWPrime.free;
XXcovs.free;
XXFisher.free;
XXx.free;
XXXW2.free;
XXXWPrime.free;
KxKA.free;
KxKB.free;
Kvec.free;
Kveci.free;
KxKA1.free;
KxKB1.free;
Kvec1.free;
Kveci1.free;

    freemem(finalloglik);

end;

end.
 