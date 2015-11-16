{ ******************************************************************
  Eigenvalues and eigenvectors of a general square matrix
  ****************************************************************** }

unit ueigvec;

interface

uses
  utypes, ubalance, uelmhes, ueltran, uhqr2, ubalbak;

procedure EigenVect(A      : PMatrix;
                    Lb, Ub : Integer;
                    Lambda : PCompVector;
                    V      : PMatrix);

implementation

procedure EigenVect(A      : PMatrix;
                    Lb, Ub : Integer;
                    Lambda : PCompVector;
                    V      : PMatrix);
  var
    I_low, I_igh : Integer;
    Scale        : PVector;
    I_Int        : PIntVector;
  begin
    DimVector(Scale, Ub);
    DimIntVector(I_Int, Ub);

    Balance(A, Lb, Ub, I_low, I_igh, Scale);
    ElmHes(A, Lb, Ub, I_low, I_igh, I_int);
    Eltran(A, Lb, Ub, I_low, I_igh, I_int, V);
    Hqr2(A, Lb, Ub, I_low, I_igh, Lambda, V);

    if MathErr = 0 then
      BalBak(V, Lb, Ub, I_low, I_igh, Scale, Ub);

    DelVector(Scale, Ub);
    DelIntVector(I_Int, Ub);
  end;

end.
