{ ******************************************************************
  Eigenvalues of a general square matrix
  ****************************************************************** }

unit ueigval;

interface

uses
  utypes, ubalance, uelmhes, uhqr;

procedure EigenVals(A      : PMatrix;
                    Lb, Ub : Integer;
                    Lambda : PCompVector);

implementation

procedure EigenVals(A      : PMatrix;
                    Lb, Ub : Integer;
                    Lambda : PCompVector);
  var
    I_low, I_igh : Integer;
    Scale        : PVector;
    I_int        : PIntVector;
  begin
    DimVector(Scale, Ub);
    DimIntVector(I_Int, Ub);

    Balance(A, Lb, Ub, I_low, I_igh, Scale);
    ElmHes(A, Lb, Ub, I_low, I_igh, I_int);
    Hqr(A, Lb, Ub, I_low, I_igh, Lambda);

    DelVector(Scale, Ub);
    DelIntVector(I_Int, Ub);
  end;

end.
