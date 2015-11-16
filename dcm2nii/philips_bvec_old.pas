unit philips_bvec;
{$ifdef fpc}{$mode delphi}{$endif}
{$H+}
interface
uses
  //StrUtils,
  Classes, SysUtils, define_types, dicomtypes, dialogsx,GraphicsMathLibrary,dialogs_msg;
//{$DEFINE VERBOSE_BVEC}


procedure PhilipsCorrectBvecs(var lDICOMdata:dicomdata; var lDTIra: TDTIRA; nVec: integer);

implementation

procedure ReportMatrix(s: string; q: TMatrix);
begin
     dcmmsg(s+Format('=[ %g %g %g %g; %g %g %g %g; %g %g %g %g; 0 0 0 1]',  [
     q.matrix[1,1],q.matrix[1,2],q.matrix[1,3],q.matrix[1,4]  ,
     q.matrix[2,1],q.matrix[2,2],q.matrix[2,3],q.matrix[2,4] ,
     q.matrix[3,1],q.matrix[3,2],q.matrix[3,3],q.matrix[3,4]]));
end;


//Next routines for PhilipsBVec
  FUNCTION Vector2D  (CONST xValue, yValue, zValue:  DOUBLE):  TVector;
  BEGIN
    WITH RESULT DO
    BEGIN
      x    := xValue;
      y    := yValue;
      z    := zValue;
      size := size2D
    END
  END; //Vector2D

    // Assume vector contains 'extra' homogeneous coordinate -- ignore it.
  procedure NormalizeVector2D(var u:  TVector);
  var
     lSum: double;
  BEGIN
      lSum := sqrt((u.x*u.x)+(u.y*u.y)+(u.z*u.z));
      if lSum <> 0 then
         u := Vector2D( u.x/lSum,
                         u.y/lSum,
                          u.z/lSum)
  END; //NormalizeVector2D

FUNCTION revMat  (CONST Input:TMatrix):  TMatrix;//Transpose Matrix
var
   i,j: integer;
begin
 result.size := Input.size;
 for i := 1 to Input.size do
     for j := 1 to Input.size do
         result.matrix[i,j] := input.matrix[j,i];
end;


 FUNCTION VecMatMult (CONST u:  TVector; CONST a:  TMatrix):  TVector;
    VAR
      i,k :  TIndex;
      temp:  DOUBLE;
  BEGIN
    RESULT.size := a.size;
    IF  a.size = u.size
    THEN BEGIN
      FOR i := 1 TO a.size DO
      BEGIN
        temp := 0.0;
        FOR k := 1 TO a.size DO
        BEGIN
          temp := temp + u.vector[k]*a.matrix[i,k];
        END;
        RESULT.vector[i] := Defuzz(temp)
      END;
    END
    ELSE raise EMatrixError.Create('VecMatMult error')
  END;//VecMatMult


procedure PhilipsCorrectBvecs(var lDICOMdata:dicomdata; var lDTIra: TDTIRA; nVec: integer);
//Test lIn.x := 0.499997615814209; lIn.y :=  0.499997615814209; lIn.z := 0.707110166549683;
//Philips DICOM data stored in patient (LPH) space, regardless of settings in Philips user interface
//algorithm inspired by CATNAP http://godzilla.kennedykrieger.org/~jfarrell/software_web.htm
//http://iacl.ece.jhu.edu/~bennett/catnap/catnap.shtml
//0018,5100. patient orientation - 'HFS'
//2001,100B Philips slice orientation (TRANSVERSAL, AXIAL, SAGITTAL)
//2005,1071. Philips AP angulation : -8.74086
//2005,1072. Philips FH angulation : -3.53147
//2005,1073. Philips RL angulation -0.387372
(* 3/2008: updated to correct for a bug in the Johns Hopkins code:
% July 20, 2007 | I corrected a small bug with the rotation matrices for
% slice angulation.  I had multiplied 3 matrices in the incorrect order.

% A colleague (Harsh Agarwal) pointed this out while aligning different
% MRI contrasts using the angulation parameters and the transformation
% matrices given in the Philips document.
%I originally had Tang = Tfh*Tap*Trl
%    which is now Tang = Trl*Tap*Tfh;
%I originally had rev_Tang = rev_Trl*rev_Tap*rev_Tfh;
%which is now     rev_Tang = rev_Tfh*rev_Tap*rev_Trl;
% I double checked the Philips code and this seems to be correct now.
% I also double checked the impact on fiber tracking. The fiber tracking
% looks good in both instances (even though the gradient tables are
% slightly different).  If 2 angulation values are zero (i.e. [AP,FH,RL]
=
% [0,0,20], then the old and new equations give the same result.  Only
if
% two or more elements are non zero is the result different.  I did some
% testing with very large angulations of 20 degrees [20,20,0], [20,0,20]

% and [0,20,20]and found that the fiber tracking results were almost
% indistinguishable. THIS FIX ONLY affects yes overplus and
% user-defined gradient tables. No overplus tables are not subject to
% slice angulation changes
*)

var
   lIn,lOut: TVector;
   ltpp,lrev_tpp,ltpom,lrev_tpom,ltpo,lrev_tpo,ltrl,ltap,ltfh,
   lmtemp1,lmtemp2 ,ltang,lrev_tang,
   lrev_trl, lrev_tap, lrev_tfh,
   lrev_tsom,ldtiextra: TMatrix;
   lI: Integer;
   lap,lfh,lrl: double;
begin

    if nVec < 1 then exit;
    //require HFS - head first supine. See Catnap for alternate body orientations
    //   and (lDicomData.PatientPos[1] = 'H') and (lDicomData.PatientPos[2] = 'F') and (lDicomData.PatientPos[3] = 'S') then
    if (length(lDicomData.PatientPos) < 3) then begin
       //HFS = head-first supine
          dcmMsg('DTI vector error: Position is not head first supine');
          exit;
     end;
     if (lDicomData.PatientPos[1] = 'F') and (lDicomData.PatientPos[2] = 'F') then begin//strcmpi(patient_position,'ff')
       ltpp := Matrix2D (0,-1,0, -1,0,0, 0,0,1);
       //rev_Tpp = [0,-1,0;-1,0,0;0,0,-1];
    end else if (lDicomData.PatientPos[1] = 'H') and (lDicomData.PatientPos[2] = 'F') then begin//strcmpi(patient_position,'hf')
           ltpp := Matrix2D (0,1,0,-1,0,0, 0,0,-1);
           //rev_Tpp = [0,-1,0;1,0,0;0,0,-1];
    end else begin
       dcmMsg('DTI vector error: images must be HF or FF (head or feet first) '+lDicomData.PatientPos);
       exit;
     end;
    lrev_tpp := revMat(ltpp);

(* http://www.dabsoft.ch/dicom/3/C.7.3.1.1.2/
see matlab code http://godzilla.kennedykrieger.org/~jfarrell/software_web.htm#PARtoNRRD
HFP = Head First-Prone
HFS = Head First-Supine
HFDR = Head First-Decubitus Right
HFDL = Head First-Decubitus Left
FFDR = Feet First-Decubitus Right
FFDL = Feet First-Decubitus Left
FFP = Feet First-Prone
FFS = Feet First-Supine
*)
     if lDicomData.PatientPos[3] = 'S' then begin//supine
        ltpo := Matrix2D (1,0,0, 0,1,0, 0,0,1);
        //rev_Tpo = [1,0,0;0,1,0;0,0,1];
     end else if lDicomData.PatientPos[3] = 'P'    then begin //prone
          ltpo := Matrix2D (-1,0,0, 0,-1,0, 0,0,1);
          //rev_Tpo = [-1,0,0;0,-1,0;0,0,1];
     end else if (length(lDicomData.PatientPos) > 3) and (lDicomData.PatientPos[3] = 'D') and (lDicomData.PatientPos[4] = 'R') then begin   //rd
          ltpo := Matrix2D (0,-1,0, 1,0,0, 0,0,1);
          //rev_Tpo = [0,1,0;-1,0,0;0,0,1];
     end else if (length(lDicomData.PatientPos) > 3) and (lDicomData.PatientPos[3] = 'D') and (lDicomData.PatientPos[4] = 'L')   then begin  //ld
          ltpo := Matrix2D (0,1,0, -1,0,0, 0,0,1);
          //rev_Tpo = [0,-1,0;1,0,0;0,0,1];
     end else begin
       dcmMsg('DTI vector error: Position is not HFS,HFP,HFDR,HFDL,FFS,FFP,FFDR, or FFDL: '+lDicomData.PatientPos);
       exit;
     end;
     lrev_tpo := revMat(ltpo);
     dcmMsg('Reorienting vectors for patient position ('+lDicomData.PatientPos+'). Please validate if you conduct DTI processing.');

     (*
    //Assume supine
    ltpo := Matrix2D (1,0,0,  0,1,0, 0,0,1  );
    lrev_tpo := revMat(ltpo);
    //Assume head first
    ltpp := Matrix2D (0,1,0,  -1,0,0, 0,0,-1);
    lrev_tpp := revMat(ltpp);  *)
    ltpom := MultiplyMatrices( ltpo, ltpp);
    lrev_tpom := MultiplyMatrices( lrev_tpp,lrev_tpo  );
    lap := lDicomData.AngulationAP  * PI /180;
    lfh := lDicomData.AngulationFH  * PI /180;
    lrl := lDicomData.AngulationRL  * PI /180;
    {$IFDEF  VERBOSE_BVEC}
    dcmmsg('ap/fh/rl'+kTab+floattostr(lDicomData.AngulationAP)+kTab+floattostr(lDicomData.AngulationFH)+kTab+floattostr(lDicomData.AngulationRL));
    for lI := 1 to nVec do
      dcmmsg(inttostr(lI)+ kTab+floattostr(lDTIra[lI].bval)+kTab+floattostr(lDTIra[lI].v1)+kTab+floattostr(lDTIra[lI].v2)+kTab+floattostr(lDTIra[lI].v3));

    {$ENDIF}
    //lAP:=-0.152557;  lFH:=-0.0616358;  lRL := -0.00676092;
     //dcmmsg('ap/fh/rl'+kTab+floattostr(lap)+kTab+floattostr(lfh)+kTab+floattostr(lrl));

    ltrl := Matrix2D (1,0,0,                0,cos(lrl),-sin(lrl),  0,sin(lrl),cos(lrl));
    ltap := Matrix2D (cos(lap),0,sin(lap),  0,1,0,                 -sin(lap),0,cos(lap));
    ltfh := Matrix2D (cos(lfh),-sin(lfh),0, sin(lfh),cos(lfh),0,    0,0,1);
    lrev_trl := revMat(ltrl);
    lrev_tap := revMat(ltap);
    lrev_tfh := revMat(ltfh);
    lmtemp1 := MultiplyMatrices( ltrl, ltap );

    ltang := MultiplyMatrices( lmtemp1, ltfh );
    lmtemp1 := MultiplyMatrices( lrev_tfh, lrev_tap );
    lrev_tang := MultiplyMatrices( lmtemp1, lrev_trl );

    if (lDicomData.PhilipsSliceOrient[1] = 'S') then //SAGITTAL
       lrev_tsom := Matrix2D (0,0,1,  0,-1,0, -1,0,0 )
    else if (lDicomData.PhilipsSliceOrient[1] = 'C') then //CORONAL
         lrev_tsom := Matrix2D (0,0,1,  -1,0,0, 0,1,0 )
    else //TRANSVERSAL = AXIAL
        lrev_tsom := Matrix2D (0,-1,0,  -1,0,0, 0,0,1 );
    ldtiextra := Matrix2D (0,-1,0,  -1,0,0, 0,0,1 );
    lmtemp2 := MultiplyMatrices( ldtiextra, lrev_tsom );
    lmtemp1 := MultiplyMatrices (lmtemp2, lrev_tang);
    ReportMatrix('lmtemp1',lmtemp1);

    for lI := 1 to nVec do begin
      {$IFDEF  VERBOSE_BVEC}
      //dcmmsg(realtostr(lDTIra[lI].v1,5)+kTab+realtostr(lDTIra[lI].v2,5)+kTab+realtostr(lDTIra[lI].v3,5) );

      {$ENDIF}
        if (lDTIra[lI].bval <= 0) or  ((lDTIra[lI].v1 = 0) and (lDTIra[lI].v2 = 0) and (lDTIra[lI].v3 = 0)) then begin
            lDTIra[lI].v1 := 0;
            lDTIra[lI].v2 := 0;
            lDTIra[lI].v3 := 0;
        end else begin
            //lIn := Vector2D(0.7071, -0.7071, -0.0000);
            lIn := Vector2D(-lDTIra[lI].v1,-lDTIra[lI].v2,-lDTIra[lI].v3);
            NormalizeVector2D(lIn);
            lout := VecMatMult (lin,lmtemp1);
            NormalizeVector(lout);
            lDTIra[lI].v1 := lOut.x;
            lDTIra[lI].v2 := lOut.y;
            {2014: dcm2nii flips physically images in AP direction, so do not change sign
            if lOut.y = 0 then
                lDTIra[lI].v2 := lOut.y //people dislike seeing -0
            else
                lDTIra[lI].v2 := -lOut.y; //flip Y component
            }
            lDTIra[lI].v3 := lOut.z;
        end;
    end; //for each vector
    dcmmsg('Note: dcm2nii since 2014 flips sign of DTI y-component for FSL tools. Please validate for yur system.');
end;

end.

