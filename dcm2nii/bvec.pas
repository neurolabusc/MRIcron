unit bvec;
{$ifdef fpc}{$mode delphi}{$endif}
{$H+}
interface
uses
  //StrUtils,
  Classes, SysUtils, define_types, dicomtypes, dialogsx,GraphicsMathLibrary,dialogs_msg;
//{$DEFINE VERBOSE_BVEC}


procedure siemensPhilipsCorrectBvecs (var lDICOMdata:dicomdata; var lDTIra: TDTIra; nVec: integer; lSliceOrientMosaicNegativeDeterminant: boolean);

implementation

(*function VV (lLabel: string; var lV: TVector): string;
begin
     result := lLabel +' =['+ floattostr(lV.x)+','+floattostr(lV.y)+','+ floattostr(lV.z)+']'';';
end;

procedure VTX (var bvecs_old,slice_dir,read_dir,phase_dir: TVector);
var  lStr : string;
begin
      lStr := '';
      lStr := lStr + VV('bvecs_old',bvecs_old);
      lStr := lStr + VV('slice_dir',slice_dir);
      lStr := lStr + VV('read_dir',read_dir);
      lStr := lStr + VV('phase_dir',phase_dir);
      dcmMsg(lStr);
end;  *)

procedure siemensPhilipsCorrectBvecs (var lDICOMdata:dicomdata; var lDTIra: TDTIra; nVec: integer; lSliceOrientMosaicNegativeDeterminant: boolean);
    //see Matthew Robson's  http://users.fmrib.ox.ac.uk/~robson/internal/Dicom2Nifti111.m
    //convert DTI vectors from scanner coordinates to image frame of reference
    //Uses 6 orient values from ImageOrientationPatient  (0020,0037)
    // requires PatientPosition 0018,5100 is HFS (head first supine)
var
   lI: integer;
   read_vector ,phase_vector,slice_vector,bvecs_old,bvecs_new: TVector;
begin
     if nVec < 1 then exit;
     if (length(lDicomData.PatientPos) >= 3) and (lDicomData.PatientPos[1] = 'H') and (lDicomData.PatientPos[2] = 'F') and (lDicomData.PatientPos[3] = 'S') then
     else begin
          dcmMsg('DTI vector error: Position is not head first supine');
          exit;
     end;
    read_vector := Vector3D(lDICOMData.Orient[1],lDICOMData.Orient[2],lDICOMData.Orient[3]);
    phase_vector := Vector3D(lDICOMData.Orient[4],lDICOMData.Orient[5],lDICOMData.Orient[6]);
    slice_vector := CrossProduct(read_vector ,phase_vector);

    NormalizeVector(read_vector);
    NormalizeVector(phase_vector);
    NormalizeVector(slice_vector);
    for lI := 1 to nVec do begin
        //afx('test',lDTIra[lI],lI);
        if (lDTIra[lI].bval <= 0) or  ((lDTIra[lI].v1 = 0) and (lDTIra[lI].v2 = 0) and (lDTIra[lI].v3 = 0)) then begin
            lDTIra[lI].v1 := 0;
            lDTIra[lI].v2 := 0;
            lDTIra[lI].v3 := 0;
        end else begin
            bvecs_old := Vector3D(lDTIra[lI].v1,lDTIra[lI].v2,lDTIra[lI].v3);
            //VTX (bvecs_old,slice_vector,read_vector,phase_vector );
            bvecs_new :=Vector3D(DotProduct(bvecs_old,read_vector),DotProduct(bvecs_old,phase_vector),DotProduct(bvecs_old,slice_vector) );
            bvecs_new.y := - bvecs_new.y;
            NormalizeVector(bvecs_new);
            lDTIra[lI].v1 := bvecs_new.x;
            if  lSliceOrientMosaicNegativeDeterminant then
                lDTIra[lI].v2 := -bvecs_new.y
            else
                lDTIra[lI].v2 := bvecs_new.y;
            lDTIra[lI].v3 := bvecs_new.z;
        end;
    end;//for each bvec
    if  lSliceOrientMosaicNegativeDeterminant then
        dcmmsg('WARNING: please validate DTI vectors (matrix had a negative determinant, perhaps Siemens sagittal).');

end;//PhilipsSiemensCorrectBvecs

end.

