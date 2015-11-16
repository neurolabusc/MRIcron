unit Mat;

{ Basic Matrix Unit for Delphi, May 1996. Implemented using original iMAP C matrix library }
{ Use this instead of matrix }


interface

Uses SysUtils, Classes, Vector,dialogsx;

//var gMat: boolean = false;
type EMatrixError     = class (Exception);
     EMatrixSizeError = class (EMatrixError);
     ESingularMatrix  = class (EMatrixError);
     ENonSquareMatrix = class (EMatrixError);

     TMatError = (Singular, NonSingular, NonSquare);



     { A Matrix is made up of a set of rows of type TRow, pTRow is
     a pointer to a single row and a matrix is a row of pTRows, this
     allows arrays larger then 65K to be built, the max size of
     a matrix is roughly 4096 MBytes }
     MatRA = array [1..1] of Double;
     Matp = ^MatRA;

     { forward declare the Matrix class }
     TMatrix = class;

     { Used by svdfit, supplies basis functions at x }
     BasisProc = procedure (x : TMatElement; var BasisFunc : TVector);

     { Define a dynamic matrix type for holding doubles }
     TMatrix = class (TObject)
                private

                   nr, nc : integer;
                   mx   :matp;//: pTRowList;  { pointer to a list of rows }
                   procedure SetSize (ri, ci : integer);
                   procedure FreeSpace;
                public
                   constructor  create (r, c : integer); overload; virtual;
                   constructor  create (n : integer); overload; virtual;
                   constructor  create (c : integer; d : array of TMatElement); overload; virtual;
                   destructor   destroy; override;
                   procedure Setval (ri, ci : integer; v : TMatElement);
                   function  Getval (ri, ci : integer) : TMatElement;
                   property  M[x, y : Integer] : TMatElement read GetVal write SetVal; default;
                   property  r : integer read nr;
                   property  c : integer read nc;
                   function  IsSquare : boolean;
                   function  SameDimensions (m1, m2 : TMatrix) : boolean;
                   function  Identity : TMatrix;
                   function  Diagonal (k : TMatElement) : TMatrix; overload;
                   function  Diagonal (v : TVector) : TMatrix; overload;
                   function  Zero : TMatrix;
                   function  Ones : TMatrix;
                   function  L (ci :integer; d : array of TMatElement) : TMatrix;
                   function  transpose : TMatrix; overload;
                   function  transpose (m1 : TMatrix) : TMatrix; overload;
                   function  add    (m1, m2 : TMatrix) : TMatrix; overload;
                   function  add    (m1 : TMatrix) : TMatrix; overload;
                   function  sub    (m1, m2 : TMatrix) : TMatrix; overload;
                   function  sub    (m1 : TMatrix) : TMatrix; overload;
                   function  mult   (m1 : TMatrix; k : TMatElement) : TMatrix; overload;
                   function  mult   (k : TMatElement) : TMatrix; overload;
                   function  mult   (m1, m2 : TMatrix) : TMatrix; overload;
                   function  copy   (m1 : TMatrix) : TMatrix;
                   procedure ExtractColumn (var v : TVector; cc : integer);
                   procedure ExtractRow (var v : TVector; rr : integer);
                   function  ExchangeRows (r1, r2 : integer) : TMatrix;
                   function  ExchangeCols (c1, c2 : integer) : TMatrix;
                   function  Rank (echelon : TMatrix; eps : double) : integer;
                   procedure Invert (inv : TMatrix); overload;
                   procedure Invert; overload;
                   procedure Invert2 (var dest, src : TMatrix; var col: TVector; var index : TVectori);
                   function Det2 (m1 : TMatrix; var index : TVectori; var v : TVector): double;
                   procedure SolveLinear (v, b : TVector; SelfToInv : boolean);
                   procedure LUSolve (index : TVectori; b : TVector);
                   procedure LUDecomp (m1 : TMatrix; index : TVectori);
                   procedure LUDecomp2 (var m1 : TMatrix; var index : TVectori; var v : TVector);
                   function MatMax: double;
                   function MatAbsMax: double;
                   function  Det : double;
                   procedure NullSpace (var NullVectors : TMatrix; var BasisSize : integer;
                                 var Echelon : TMatrix; var TheRank : integer);

                   procedure svd (var u : TMatrix; var w : TVector; var v : TMatrix);
                   procedure svd2 (var u : TMatrix; var w : TVector; var v : TMatrix);
                   procedure svdSolve (var u : TMatrix; var w : TVector; var v : TMatrix;
                                b : TVector; var x : TVector);
                   function  svdfit (x, y, yerr : TVector; var fit : TVector;
                             var u, v : TMatrix; var w : TVector; funcs : BasisProc): TMatElement;
                   procedure svdCovar (v : TMatrix; w : TVector; alpha : TMatrix);

                   procedure eliminate_cms (S, Tk1 : TMatrix; var cr, N : integer);
                   procedure ElementaryModes (D : TVectori; var mf, mb, C1, k : integer; Tk : TMatrix);
                   class procedure Tableau (N, R1 : integer; var mf, mb, C1, k : integer; Tk, Tk1 : TMatrix);
                   class function  grecodiv_of_vector (N, R1 : integer; vec : TVector) : integer;
                   class function  grecodiv(P, Rest: integer) : integer;
                   procedure Conserve(st : TMatrix);
                end;


{ ------------------------------------------------------------------------- }

implementation

const MATERROR = 'Matrix Operation Error:';



{ ------------------------------------------------------------------------- }
{                         START OF MATRIX IMPLEMETATION                     }
{ ------------------------------------------------------------------------- }


{ -------------------------  Constructors first  ---------------------------- }


{ ******************************************************************** }
{ Usage:  A := TMatrix.create (3, 2);                                  }
{ ******************************************************************** }
constructor TMatrix.create (r, c : integer);
begin
  Inherited Create; nr := 0; nc := 0; mx := Nil;
  Self.SetSize (r, c);
  
end;




{ ******************************************************************** }
{ Create an identity matrix                                            }
{                                                                      }
{ Usage:   A := TMatrix.createI (3);                                   }
{ ******************************************************************** }
constructor TMatrix.create (n : integer);
var i : integer;
begin
  Inherited Create; nr := 0; nc := 0; mx := Nil;
  Self.SetSize (n, n);
  for i := 1 to n do Self[i,i] := 1.0;
end;


{ ******************************************************************** }
{ Create a matrix filled with values from array d given that the       }
{ number of columns equals c.                                          }
{                                                                      }
{ Usage:  A := TMatrix.createLit (2, [1, 2, 3, 4]);                    }
{         Creates a 2 by 2 array                                       }
{ ******************************************************************** }
constructor  TMatrix.create (c : integer; d : array of TMatElement);
var i, j, ri, count : integer;
begin
  Inherited Create; nr := 0; nc := 0; mx := Nil;
  ri := (High(d)+1) div c;
  Self.SetSize (ri, c);
  count := 0;
  for i := 1 to ri do
      for j := 1 to c do
          begin
          Self[i,j] := d[count];
          inc (count);
          end;
end;


{ ******************************************************************** }
{ Usage:    A.destroy, use a.free in a program                         }
{ ******************************************************************** }
destructor TMatrix.destroy;
begin
  FreeSpace;
  Inherited Destroy;
end;



{ Free the data space but not the object }
procedure TMatrix.FreeSpace;
//var i : integer;
begin
  if mx <> Nil then
     begin
     FreeMem (mx); mx := Nil;
     end;
end;


{ Internal routine used set size of matrix and allocate space }
procedure TMatrix.SetSize (ri, ci : integer);
//var i : integer;
begin
  if (mx <> Nil) and ((ri*ci)= (nr*nc)  ) then begin
     nr := ri; nc := ci;
     exit;
  end;
  //if gMat then beep;
  FreeSpace;
  nr := ri; nc := ci;
  //if gMat then beep;
  Getmem(mx,ri*ci*sizeof(TMatElement));//AllocMem (sizeof (pTRowList) * (nr+1));  { r+1 so that I can index from 1 }
end;


{ ---------------------------------------------------------------------------- }
{                               BASIC ROUTINES                                 }
{ ---------------------------------------------------------------------------- }


{ ******************************************************************** }
{ Used internally but is also accessible from the outside              }
{                                                                      }
{ Normal Usage:  A[2, 3] := 1.2;                                       }
{                                                                      }
{ ******************************************************************** }
procedure TMatrix.Setval (ri, ci : integer; v : TMatElement);
begin
  if ri > r then
     raise EMatrixSizeError.Create ('ri index out of range: ' + inttostr (ri));

  if ci > c then
     raise EMatrixSizeError.Create ('ci index out of range: ' + inttostr (ci));

  mx^[ri + ((ci-1)* r )] := v;
end;


{ ******************************************************************** }
{ Used internally but is also accessible from the outside              }
{                                                                      }
{ Normal Usage:  d := A[2, 3];                                         }
{                                                                      }
{ ******************************************************************** }
function TMatrix.Getval (ri, ci : integer) : TMatElement;
begin
  result := mx^[ri + ((ci-1)* r )];
end;




{ ******************************************************************** }
{ Fill an existing matrix with the array d of numbers. ci equals       }
{ the number of columns.                                               }
{                                                                      }
{ Usage:   A.L(3, [1, 2, 3, 4, 5, 6, 7, 8, 9]);                        }
{                                                                      }
{ ******************************************************************** }
function TMatrix.L (ci :integer; d : array of TMatElement) : TMatrix;
var i, j, ri, count : integer;
begin
  ri := (High(d)+1) div ci;
  FreeMem (mx, sizeof (TMatElement) * nr * nc);
  Self.SetSize (ri, ci);
  count := 0;
  for i := 1 to ri do
      for j := 1 to ci do
          begin
          Self[i,j] := d[count];
          inc (count);
          end;
  result := Self;
end;
{ ******************************************************************** }
{ Set all elements to one                                                }
{                                                                      }
{ Usage: A.Ones;                                                       }
{                                                                      }
{ ******************************************************************** }

function TMatrix.Ones : TMatrix;
var i, j : integer;
begin
  for i := 1 to r do
      for j := 1 to c do
          Self[i,j] := 1.0;
  result := Self;
end;
{ ******************************************************************** }
{ Zero the Self matrix                                                 }
{                                                                      }
{ Usage: A.Zero;                                                       }
{                                                                      }
{ ******************************************************************** }
function TMatrix.Zero : TMatrix;
var i, j : integer;
begin
  for i := 1 to r do
      for j := 1 to c do
          Self[i,j] := 0.0;
  result := Self;
end;


{ ******************************************************************** }
{ Returns true if matrices m1 and m2 have the same dimensions          }
{                                                                      }
{ Usage: if SameDimensions (A, B) then                                 }
{                                                                      }
{ ******************************************************************** }
function TMatrix.SameDimensions (m1, m2 : TMatrix) : boolean;
begin
  result := (m1.nr = m2.nr) and (m1.nc = m2.nc);  { use nr, nc for direct access }
end;


{ ******************************************************************** }
{ Returns true if matrix m is square                                   }
{                                                                      }
{ Usage: if IsSquare then                                              }
{                                                                      }
{ ******************************************************************** }
function TMatrix.IsSquare : boolean;
begin
  result := Self.nr = Self.nc;
end;


{ ******************************************************************** }
{ Turn the matrix Self into an identify matrix                         }
{                                                                      }
{ Usage:  A.Identity                                                   }
{                                                                      }
{ ******************************************************************** }
function TMatrix.Identity : TMatrix;
var i : integer;
begin
  if Self.IsSquare then
     begin
     Self.Zero;
     for i := 1 to r do Self[i,i] := 1.0;
     result := Self;
     end
  else
     raise EMatrixSizeError.Create ('An identity matrix can only be formed from a square matrix');
end;


{ ******************************************************************** }
{ Make the matrix object a diagonal matrix with the value, k           }
{                                                                      }
{ Usage: A.Diagonal (3.1415);                                          }
{                                                                      }
{ ******************************************************************** }
function TMatrix.Diagonal (k : TMatElement) : TMatrix;
var i : integer;
begin
  if Self.IsSquare then
     begin
     Self.Zero;
     for i := 1 to r do Self[i,i] := k;
     result := Self;
     end
  else
     raise EMatrixSizeError.Create ('Can only form a diagonal matrix from a square matrix');
end;


{ ******************************************************************** }
{ This forms a diagonal matrix from the elements of vector v.          }
{                                                                      }
{ Usage: A.Diagonal (v)                                                }
{                                                                      }
{ ******************************************************************** }
function TMatrix.Diagonal (v : TVector) : TMatrix;
var i : integer;
begin
  if Self.IsSquare then
     begin
     if v.size = Self.nr then
        begin
        Self.zero;
        for i := 1 to r do Self[i,i] := v[i];
        result := Self;
        end
     else
        raise EMatrixSizeError.Create ('Vector must be same size as matrix in DiagonalV');
     end
  else
     raise EMatrixSizeError.Create ('Can only form a diagonal matrix from a square matrix');
end;


{ ******************************************************************** }
{ Transpose matrix 'Self', Self is thus destroyed and replaced         }
{                                                                      }
{ Usage:  A.transpose                                                  }
{                                                                      }
{ ******************************************************************** }
function TMatrix.Transpose : TMatrix;
var i, j : integer; tmp : TMatrix;
begin
  if (r=1) or (c=1) then begin
      i := nr;
      nr := nc;
      nc := i;
      exit;
  end;
  tmp := TMatrix.create (c, r);
  try
    for i := 1 to r do
        for j := 1 to c do
            tmp [j,i] := Self[i,j];
    Self.FreeSpace; Self.SetSize (tmp.nr, tmp.nc);
    { move data from transpose to Self }
    Self.Copy (tmp);
  finally
    tmp.Destroy;
  end;
  result := Self;
end;


{ ******************************************************************** }
{ Transpose the matrix 'm' into Self                                   }
{                                                                      }
{ Usage:  T.transpose (A);   Tranposes A and puts result into T       }
{ Will also accept T.transpose (T)                                    }
{ ******************************************************************** }
function TMatrix.Transpose (m1 : TMatrix) : TMatrix;
var i, j : integer; t : TMatrix;
begin
  if (m1.r <> Self.c) and (m1.c <> Self.r) then
     raise EMatrixSizeError.Create ('Destination matrix has incorrect dimensions for transpose');
  { If the user is trying to transpose itself.... }
  if Self = m1 then
     begin
     t := TMatrix.Create (r, c);
     try
       t.Copy (m1);
       for i := 1 to m1.r do
           for j := 1 to m1.c do
               Self[j,i] := t[i,j];
     finally
       t.free;
       result := Self;
     end;
     exit;
     end;

  for i := 1 to m1.r do
      for j := 1 to m1.c do
          Self[j,i] := m1[i,j];
  result := Self;
end;


{ ******************************************************************** }
{ Copy matrix 'm' to Self, Self must exist and is overwritten          }
{ in the process. This procedure does a fast deep copy of the matrix.  }
{                                                                      }
{ Usage:  B.Copy (A);   performs the operation:  B = A  with deep copy }
{                                                                      }
{ ******************************************************************** }
function TMatrix.Copy (m1 : TMatrix) : TMatrix;
begin

  if ( r<> m1.r) or (c <> m1.c) then begin
     (*if r <> m.r then
        raise EMatrixSizeError.Create (MATERROR + #13#10'Cannot copy matrices with different sized rows: dest<'
            + inttostr (r) + '> src<' + inttostr (m.r) + '>')
     else
        raise EMatrixSizeError.Create (MATERROR + #13#10'Cannot copy matrices with different sized columns: dest<'
            + inttostr (c) + '> src<' + inttostr (m.c) + '>'); *)
     SetSize (self.r, self.c);
  end;
  { Copy a whole row at a time using move }
  //for i := 1 to r do move (m.mx^[i]^, Self.mx^[i]^, sizeof(TMatElement) * (c+1));
  move(m1.mx^,self.mx^,r*c*sizeof(double));
  // Copy over column and row names, clear destination first then copy
  result := Self;
end;


{ ******************************************************************** }
{ Extract column cc from the Self matrix and return it as a TVector    }
{                                                                      }
{ Usage: m.ExtractColumn (v, 1)  extract column 1 from m and place in v}
{                                                                      }
{ ******************************************************************** }
procedure TMatrix.ExtractColumn (var v : TVector; cc : integer);
var i : integer;
begin
  v.freeSpace; v.SetSize (Self.r); { Create result vector of appropriate size }
  for i := 1 to Self.r do v[i] := Self[i, cc];
end;


{ ******************************************************************** }
{ Extract rwo rr from the Self matrix and return it as a TVector       }
{                                                                      }
{ Usage: m.ExtractRow (v, 1)  extract row 1 from m and place in v      }
{                                                                      }
{ ******************************************************************** }
procedure TMatrix.ExtractRow (var v : TVector; rr : integer);
var i : integer;
begin
  v.freespace; v.SetSize (Self.c);
  for i := 1 to Self.c do v[i] := Self[rr, i];
end;


{ ******************************************************************** }
{ Add matrix 'm' to Self, giving a new Self                            }
{                                                                      }
{ Usage:  A.addU (B);   add B to A, giving A                           }
{                                                                      }
{ ******************************************************************** }
function TMatrix.add (m1 : TMatrix) : TMatrix;
var i, j : integer;
begin
  if Not SameDimensions (m1, Self) then
     raise EMatrixSizeError.Create ('Incorrectly sized result matrix for matrix addition');

  for i := 1 to r do
      for j := 1 to c do
          Self[i,j] := Self[i,j] + m1[i,j];
  result := Self;
end;


{ ******************************************************************** }
{ Add matrix 'm1' and 'm2' and assign to Self                          }
{                                                                      }
{ Usage: A.add (A1, A2);  add A1 to A2 giving A                        }
{                                                                      }
{ ******************************************************************** }
function TMatrix.add (m1, m2 : TMatrix) : TMatrix;
var i, j : integer;
begin
  if Not SameDimensions (m1, m2) then
     raise EMatrixSizeError.Create ('Incompatible matrix operands to add');

  if Not SameDimensions (m1, Self) then
     raise EMatrixSizeError.Create ('Incorrectly sized result matrix for matrix addition');

  for i := 1 to r do
      for j := 1 to c do
          Self[i,j] := m1[i,j] + m2[i,j];
  result := Self;
end;



{ ******************************************************************** }
{ Subtract matrix m from Self giving a new Self                        }
{                                                                      }
{ Usage:  A.subU (B);  subtract B from A giving A                      }
{                                                                      }
{ ******************************************************************** }
function TMatrix.sub (m1 : TMatrix) : TMatrix;
var i, j : integer;
begin
  if Not SameDimensions (m1, Self) then
     raise EMatrixSizeError.Create ('Incorrecly sized result matrix for matrix subtraction');

  for i := 1 to r do
      for j := 1 to c do
          Self[i,j] := Self[i,j] - m1[i,j];
  result := Self;
end;



{ ******************************************************************** }
{ Subtract m2 from m1 giving Self                                      }
{                                                                      }
{ Usage:  A.sub (A1, A2);  subtract A2 from A1 giving A (A = A2 - A1)  }
{                                                                      }
{ ******************************************************************** }
function TMatrix.sub (m1, m2 : TMatrix) : TMatrix;
var i, j : integer;
begin
  if Not SameDimensions (m1, m2) then
     raise EMatrixSizeError.Create ('Incompatible matrix operands to subtract');

  if Not SameDimensions (m1, Self) then
     raise EMatrixSizeError.Create ('Incorrectly sized result matrix for matrix subtraction');

  for i := 1 to r do
      for j := 1 to c do
          Self[i,j] := m1[i,j] - m2[i,j];
  result := Self;
end;


{ ******************************************************************** }
{ Multiply a matrix 'm' by scalar constant k and assign result to Self }
{                                                                      }
{ Usage: A.multk (B, 0.5);  multiply scalar, 0.5 by B giving A         }
{                                                                      }
{ ******************************************************************** }
function TMatrix.mult (m1 : TMatrix; k : TMatElement) : TMatrix;
var i, j : integer;
begin
  for i := 1 to m1.r do
      for j := 1 to m1.c do
          Self[i, j] := m1[i,j] * k;
  result := Self;
end;


{ ******************************************************************** }
{ Multiply the Self matrix by the scalar constant k                    }
{                                                                      }
{ Usage:  A.multKU (0.5);  multiply scalar 0.5 by A giving A           }
{                                                                      }
{ ******************************************************************** }
function TMatrix.mult (k : TMatElement) : TMatrix;
var i, j : integer;
begin
  for i := 1 to r do
      for j := 1 to c do
          Self[i, j] := Self[i,j] * k;
  result := Self;
end;



{ ******************************************************************** }
{ Multiply matrix 'm1' by 'm2' to give result in Self                  }
{                                                                      }
{ Usage:  A.mult (A1, A2); multiply A1 by A2 giving A                  }
{                                                                      }
{ ******************************************************************** }
function TMatrix.mult (m1, m2 : Tmatrix) : TMatrix;
var i, j, k, m1_Col : integer; sum : TMatElement;
begin
  if m1.c = m2.r then
     begin
      m1_col := m1.c;
      for i := 1 to Self.r do
          for j := 1 to Self.c do
              begin
              sum := 0.0;
              for k := 1 to m1_Col do
                  sum := sum + m1[i, k]* m2[k, j];
              Self[i,j] := sum;
              end;
      result := Self;
    end
  else
     raise EMatrixSizeError.Create ('Incompatible matrix operands to multiply');
end;


{ ******************************************************************** }
{ LU Solve. Solve the linear system represented by m and right-hand    }
{ side b m is assumed have have been decomposed by LUDecomp            }
{                                                                      }
{ Usage: m.LUSolve (index, b)                                          }
{                                                                      }
{ ******************************************************************** }
procedure TMatrix.LUSolve (index : TVectori; b : TVector);
var i, j, ii, ip, nRows : integer; sum : TMatElement;
begin
   ii := 0;
   nRows := r;
   for i := 1 to nRows do
       begin
       ip := index[i];
       sum := b[ip];
       b[ip] := b[i];
       if ii <> 0 then
          for j := ii TO i-1 do sum := sum - Self[i,j]*b[j]
       else if sum <> 0.0 then ii := i;
       b[i] := sum;
       end;
    for i := nRows downto 1 do
        begin
        sum := b[i];
        if i < nRows then
           for j := i+1 to nRows do sum := sum - Self[i,j]*b[j];
       b[i] := sum/Self[i,i];
    end
end;


{ ******************************************************************** }
{ Form LU decomposition of Self matrix. Result goes into m             }
{                                                                      }
{ Usage: m.LUDecomp(result, index);                                    }
{                                                                      }
{ ******************************************************************** }
procedure TMatrix.LUDecomp (m1 : TMatrix; index : TVectori);
var v : TVector; i, k, j, imax, nRows : integer; sum, big, tmp : TMatElement;
begin
  if Self.r = m1.c then
     begin
     m1.Copy (Self);
     v := TVector.Create (m1.r);
     try
     { Find the largest element in every row, and store its reciprocal in v[i] }
     nRows := m1.r;
     for i := 1 to nRows do
         begin
         big := 0.0; { needed to test for singularity }
         { Although we're working across columns we can use nRows since m1 is square }
         for j := 1 to nRows do if (abs(m[i,j]) > big) then big := abs(m[i,j]);
         if big = 0.0 then raise ESingularMatrix.Create ('LUDecomp: Singular matrix in LUDecomp, found row of zeros');
         v[i] := 1.0/big
         end;

     for j := 1 TO nRows do
         begin
         { Form beta = aij - sum_k=1^i-1 aik * bkj }
         for i := 1 TO j-1 do
             begin
             sum := m[i,j];
             for k := 1 to i-1 do sum := sum - m[i,k]*m[k,j];
             m[i,j] := sum
             end;
         big := 0.0;
         for i := j to nRows do
             begin
             sum := m[i,j];
             for k := 1 to j-1 do sum := sum - m[i,k]*m[k,j];
             m[i,j] := sum;
             if v[i]*abs(sum) >= big then
                begin
                big := v[i]*abs(sum);
                imax := i
                end
             end;

         { Interchange rows if necessary }
         if j <> imax then
            begin
            { Swap row names aswell }
            for k := 1 to nRows do
                begin
                tmp := m[imax,k];
                m[imax,k] := m[j,k];
                m[j,k] := tmp
                end;
             v[imax] := v[j]
             end;
          index[j] := imax;
          { Get ready to divide by pivot element }
          if m[j,j] = 0.0 then
             raise ESingularMatrix.Create ('LUDecomp: Singular Matrix, pivot value is zero');
          if j <> nRows then
             begin
             tmp := 1.0/m[j,j];
             for i := j+1 to nRows do m[i,j] := m[i,j]*tmp
             end
         end;
     finally
       v.destroy;
     end;
     end
  else
     raise ENonSquareMatrix.Create ('LUDecomp: Matrix must be square');
end;

//return max value in a matrix
function TMatrix.MatMax : double;
var i,j : integer;
begin
  if (r < 1) or (c<1) then begin
     result := 0;
     exit;
  end;
  result := m[1,1];                              
  for i := 1 to r do
      for j := 1 to c do
          if m[i, j] > result then
             result := m[i,j];
end;

//return max value in a matrix
function TMatrix.MatAbsMax : double;
var i,j : integer;
begin
  if (r < 1) or (c<1) then begin
     result := 0;
     exit;
  end;
  result := abs(m[1,1]);
  for i := 1 to r do
      for j := 1 to c do
          if abs(m[i, j]) > result then
             result := abs(m[i,j]);
end;
{ ******************************************************************** }
{ Find determinant of matrix                                           }
{                                                                      }
{ Usage: d := m.det                                                    }
{                                                                      }
{ ******************************************************************** }
function TMatrix.Det : double;
var m1 : TMatrix; index : TVectori; i : integer;
begin
  result := 1;
  if r = c then
     begin
     index := TVectori.Create (r);
     m1 := TMatrix.Create (r,r);
     try
       m1.copy (Self);
       Self.LUDecomp (m1, index);
       for i := 1 to r do result := result * m1[i,i];
     finally
       m1.free; index.free;
     end;
     end
  else
     raise ENonSquareMatrix.Create ('Determinant: Matrix must be square');
end;

(*procedure wMatrix( lTitle: string; A : TMatrix);
var
   lR,lC: integer;
   lStr: string;
begin
  if (A.r < 1) or (A.c < 1) then
     exit;
  lStr := (lTitle)+chr($0D)+chr($0A);

  for lR := 1 to (A.r) do begin
      for lC := 1 to (A.c) do
          lStr := lStr + floattostr(A.Getval(lr, lc))+'  ';
      lStr := lStr +  chr($0D)+chr($0A);
  end; //each row
  showmessage(lStr);
end;*)


procedure TMatrix.LUDecomp2 (var m1 : TMatrix; var index : TVectori; var v : TVector);
var  i, k, j, imax, nRows : integer; sum, big, tmp : TMatElement;
begin
  if Self.r = m1.c then
     begin
     m1.Copy (Self);
     //wmatrix('m1',m1);
     //v := TVector.Create (m.r);
     try
     { Find the largest element in every row, and store its reciprocal in v[i] }
     nRows := m1.r;
     for i := 1 to nRows do
         begin
         big := 0.0; { needed to test for singularity }
         { Although we're working across columns we can use nRows since m1 is square }
         for j := 1 to nRows do if (abs(m1[i,j]) > big) then big := abs(m1[i,j]);
         if big = 0.0 then raise ESingularMatrix.Create ('LUDecomp: Singular matrix in LUDecomp, found row of zeros');
         v[i] := 1.0/big
         end;

     for j := 1 TO nRows do
         begin
         { Form beta = aij - sum_k=1^i-1 aik * bkj }
         for i := 1 TO j-1 do
             begin
             sum := m1[i,j];
             for k := 1 to i-1 do sum := sum - m1[i,k]*m1[k,j];
             m1[i,j] := sum
             end;
         big := 0.0;
         for i := j to nRows do
             begin
             sum := m1[i,j];
             for k := 1 to j-1 do sum := sum - m1[i,k]*m1[k,j];
             m1[i,j] := sum;
             if v[i]*abs(sum) >= big then
                begin
                big := v[i]*abs(sum);
                imax := i
                end
             end;

         { Interchange rows if necessary }
         if j <> imax then
            begin
            { Swap row names aswell }
            for k := 1 to nRows do
                begin
                tmp := m1[imax,k];
                m1[imax,k] := m1[j,k];
                m1[j,k] := tmp
                end;
             v[imax] := v[j]
             end;
          index[j] := imax;
          { Get ready to divide by pivot element }
          if m1[j,j] = 0.0 then
             raise ESingularMatrix.Create ('LUDecomp: Singular Matrix, pivot value is zero');
          if j <> nRows then
             begin
             tmp := 1.0/m1[j,j];
             for i := j+1 to nRows do m1[i,j] := m1[i,j]*tmp
             end
         end;
     finally
       //v.destroy;
     end;
     end
  else
     raise ENonSquareMatrix.Create ('LUDecomp: Matrix must be square');
end;

function TMatrix.Det2 (m1 : TMatrix; var index : TVectori; var v : TVector): double;
var   i : integer;
begin
  result := 1;
  if r = c then
     begin
     //index := TVectori.Create (r);
     //m := TMatrix.Create (r,r);
     try
       m1.copy (Self);
       Self.LUDecomp2 (m1, index,v);
       for i := 1 to r do result := result * m1[i,i];
     finally
       //m.free; index.free;
     end;
     end
  else
     raise ENonSquareMatrix.Create ('Determinant: Matrix must be square');
end;

{ ******************************************************************** }
{ Solve a linear system of equations: Self.v = b, i.e solve for v      }
{                                                                      }
{ Usage: A.SolveLinear (v, b, t);                                      }
{        Solution in v                                                 }
{ If the boolean t is true then self is replaced by the inverse        }
{ ******************************************************************** }
procedure TMatrix.SolveLinear (v, b : TVector; SelfToInv : boolean);
var n, i, j : integer;
    indx : TVectori; col : TVector;
    dest, src : TMatrix;
begin
  if Self.r = Self.c then
     begin
     n := Self.r;
     { Make a copy and work on the copy }
     dest := TMatrix.Create (n, n);
     src  := TMatrix.Create (n, n);
     indx := TVectori.Create (n);
     try
       src.Copy (Self);
       for i := 1 to n do v[i] := b[i];
       src.LUDecomp (dest, indx);
       dest.LUSolve (indx, v);
       if SelfToInv then
          begin
          col := TVector.Create (n);
          try
            for j := 1 to n do
                begin
                for i := 1 to n do col[i] := 0.0;
                col[j] := 1.0;
                dest.LUSolve (indx, col);
                for i := 1 to n do Self[i,j] := col[i];
                end;
          finally
            col.free;
          end;
          end;
     finally
     indx.destroy; dest.destroy; src.destroy;
     end;
     end
  else
     raise ENonSquareMatrix.Create ('SolveLinear: Matrix must be square');
end;




{ ******************************************************************** }
{ Fast method for inverting a matrix (Self)                            }
{ Result in inv                                                        }
{                                                                      }
{ Usage:  A.Invert (inv);                                              }
{ ******************************************************************** }
procedure TMatrix.Invert2 (var dest, src : TMatrix; var col: TVector; var index : TVectori);
var  n, i, j : integer;
begin
  n := Self.r;
  try
    src.Copy (Self);
    try
      //wmatrix('w1',src);
      src.LUDecomp2 (dest, index,col);
      //wmatrix('w2',src);
    except
      on ESingularMatrix do
         raise ESingularMatrix.Create ('Invert: Singular Matrix');
    end;
    for j := 1 to n do
        begin
        for i := 1 to n do col[i] := 0.0;
        col[j] := 1.0;
        dest.LUSolve (index, col);
        for i := 1 to n do Self[i,j] := col[i];
        end;
  finally
    //col.destroy; dest.destroy; src.destroy; index.destroy;
  end;
end;

procedure TMatrix.Invert (inv : TMatrix);
var col : TVector; n, i, j : integer;
    dest, src : TMatrix; indx : TVectori;
begin
  n := Self.r;
  col := TVector.Create (n);
  dest := TMatrix.Create (n, n);
  src  := TMatrix.Create (n, n);
  indx := TVectori.Create (n);
  try
    src.Copy (Self);
    try
      src.LUDecomp (dest, indx);
    except
      on ESingularMatrix do
         raise ESingularMatrix.Create ('Invert: Singular Matrix');
    end;
    for j := 1 to n do
        begin
        for i := 1 to n do col[i] := 0.0;
        col[j] := 1.0;
        dest.LUSolve (indx, col);
        for i := 1 to n do inv[i,j] := col[i];
        end;
  finally
    col.destroy; dest.destroy; src.destroy; indx.destroy;
  end;
end;


{ ******************************************************************** }
{ Fast method for inverting a matrix (Self)                            }
{ Result in Self                                                       }
{                                                                      }
{ Usage:  A.Invert                                                     }
{ ******************************************************************** }

procedure TMatrix.Invert;
var col : TVector; n, i, j : integer;
    dest, src : TMatrix; index : TVectori;
begin
  n := Self.r;
  col   := TVector.Create (n);
  dest  := TMatrix.Create (n, n);
  src   := TMatrix.Create (n, n);
  index := TVectori.Create (n);
  try
    src.Copy (Self);
    try
      src.LUDecomp (dest, index);
    except
      on ESingularMatrix do
         raise ESingularMatrix.Create ('Invert: Singular Matrix');
    end;
    for j := 1 to n do
        begin
        for i := 1 to n do col[i] := 0.0;
        col[j] := 1.0;
        dest.LUSolve (index, col);
        for i := 1 to n do Self[i,j] := col[i];
        end;
  finally
    col.destroy; dest.destroy; src.destroy; index.destroy;
  end;
end;


{ Internal routine that sets any values less than eps to 0.0 }
procedure CleanUpMatrix (m : TMatrix; eps : double);
var i, j, ri, ci : integer;
begin
  { Removes all numbers close to zero, i.e between -eps and +eps }
  ri := m.r; ci := m.c;
  for i := 1 to ri do
      for j := 1 to ci do
          if abs (m [i, j]) < eps then m [i, j] := 0.0;
end;


{ Internal routine to work out the rank of a matrix given the reduced row-echelon }
function ComputeRank (m : TMatrix; eps : double) : integer;
var i, j, ri, ci, rank : integer;
begin
  ri := m.r; ci := m.c;
  { find the rank - brute force algorithm }
  rank := 0;
  { search row by row  for zero rows }
  for i := 1 to ri do
      begin
      { search along the row looking for nonzero entry }
      for j := 1 to ci do
          if abs (m [i, j]) > eps then
             begin
             inc (rank);
             break;
             end;

      end;
 result := rank;
end;


{ ******************************************************************** }
{ Routine to exchange two rows, r1 and r2 in matrix Self               }
{                                                                      }
{ Usage:  A.exchangeRows (1, 2);                                       }
{                                                                      }
{ ******************************************************************** }
function TMatrix.ExchangeRows (r1, r2 : integer) : TMatrix;
var ci, i : integer; t : double;
begin
  if (r1 > 0) and (r1 <= Self.r) and (r2 > 0) and (r2 <= Self.r) then
     begin
     ci := Self.c;
     for i := 1 to ci do
         begin
         t := Self[r1, i];
         Self[r1, i] := Self[r2, i];
         Self[r2, i] := t;
         end;
     result := Self;
     end
  else
     raise EMatrixSizeError.Create ('Rows not in range for exchange');
end;



{ ******************************************************************** }
{ Routine to exchange two columns, c1 and c2 in matrix Self            }
{                                                                      }
{ Usage:  A.exchangeCols (1, 2);                                       }
{                                                                      }
{ ******************************************************************** }
function TMatrix.ExchangeCols (c1, c2 : integer) : TMatrix;
var ri, i : integer; t : double;
begin
  if (c1 > 0) and (c1 <= Self.c) and (c2 > 0) and (c2 <= Self.c) then
     begin
     ri := Self.r;
     for i := 1 to ri do
         begin
         t := Self[c1, i];
         Self[c1, i] := Self[c2, i];
         Self[c2, i] := t;
         end;
     result := Self;
     end
  else
     raise EMatrixSizeError.Create ('Columns not in range for exchange');
end;



{ ******************************************************************** }
{ Find the rank r, of the matrix Self, The reduced Row                 }
{ echelon is returned in mat. eps is the magnitude of                  }
{ the largest number before it is assumed to be zero.                  }
{                                                                      }
{ Usage:  r := A.Rank (echelon, 1e-8)                                  }
{         Find the rank of A, place echelon in echelon                 }
{                                                                      }
{ ******************************************************************** }
function TMatrix.Rank (echelon : TMatrix; eps : double) : integer;
var Arow, Acol, i, j, n, m1, RowScan : integer;
    factor : double;
begin
  echelon.copy (Self);    { we work on mat, not Self }

  if (eps = 0.0) then eps := 1.0E-14;

  n := echelon.r; m1 := echelon.c;

  Arow := 1; Acol := 1;
  repeat
    { locate a nonzero column }
    if abs(echelon [Arow, Acol]) <= eps then  { i.e equals zero }
       begin
       { First entry was zero, therefore work our way down the matrix
       looking for a nonzero entry, when found, swap it for Arow }
       RowScan := Arow;
       repeat
         { next row }
         inc (RowScan);
         { have we reached the end of the rows but we've still got columns left to scan }
         if (RowScan > n) and (Acol < m1) then
           begin
           { reset row counter back to where it was and try next column }
           RowScan := Arow; inc (Acol);
           end;

         { If we've scanned the whole matrix, so lets get out... }
         if (RowScan > n) then
            begin
            CleanUpMatrix (echelon, eps);
            result := ComputeRank (echelon, eps);
            exit;
            end;
       until abs (echelon [RowScan, Acol]) > eps;   { keep searching until non-zero entry found }

       { We've found a nonzero row entry so swap it with
       'Arow' which did have a zero as its entry }
       echelon.exchangeRows (Arow, RowScan);
       end;
    { Arow now holds the row of interest }
    factor := 1.0/echelon [Arow, Acol];
    { reduce all the entries along the column by the factor }
    for i := Acol to m1 do echelon[Arow,i] := echelon[Arow, i] * factor;

    { now eliminate all entries above and below Arow, this generates the reduced form }
    for i := 1 to n do
        { miss out Arow itself }
        if (i <> Arow) and (abs (echelon [i, Acol]) > eps) then
           begin
           factor := echelon [i, Acol];
           { work your way along the column doing the same operation }
           for j := Acol to m1 do
               echelon[i,j] := echelon [i, j] - factor * echelon [Arow, j];
           end;

    inc (Arow); inc (Acol);
 until (Arow > n) or (Acol > m1);
 CleanUpMatrix (echelon, eps);
 result := ComputeRank (echelon, eps);   { This is just a patch for the moment }
end;


(*
                           Algorithm

    1. Reduce matrix to reduced echelon form
    2. There will be as many null space vectors as there are
       non-leading columns. Select one of these non-leading columns.
    3. Select the ith non-leading column and place a 1 at the ith
       position in the growing null space vector
    4. Consider the remaining non-leading columns, say j,k,l...
       and place zero's at positions j,k,l... in the growing null
       vector.
    5. Consider now the column positions of the leading columns, say
       l,m,n... The equivalent entries in the growing null space
       are what remains to be filled in. Select each of these leading
       columns in turn, say the lth first. Record which row the
       leading one is in, say r. Then place at position l in the
       growing null space vector, the element -1 * element (r, i)
       where i is the original ith non-leading column selected in
       step 3. Continue for leading columns m,n... until the growing
       null space vector is complete.
    6. Go back to step 2 and pick another non-leading column to
       compute the next null space vector.

Does not disturb the matrix Self. Null space to be found in NullVectors, size of
the basis in BasisSize, the reduced row-echelon in Echelon and the rank in TheRank }

 Usage:   A.NullSpace (N, b, Echelon, r);
*)
procedure TMatrix.NullSpace (var NullVectors : TMatrix; var BasisSize : integer;
                     var Echelon : TMatrix; var TheRank : integer);
var eps, x: double;
    i, j, k : integer;
    mask    : TVectori;
    tmpNullVectors : TMatrix;
    VectorCounter, maskcount : integer;
    minus999, minus888, EchelonCols : integer;
begin
  try
  eps := 0.000000001;
  minus999 := -999;     { leading column }
  minus888 := -888;     { non-leading column }

  if NullVectors <> Nil then NullVectors.free;
  if Echelon <> Nil then Echelon.free;

  tmpNullVectors := TMatrix.Create (Self.c, Self.c);
  Echelon     := TMatrix.Create (Self.r, Self.c);
  EchelonCols := Echelon.c;
  mask := TVectori.create (EchelonCols);

  // STEP 1
  k := Self.Rank (Echelon, eps);
  TheRank := k;

  k := Self.c - TheRank;
  BasisSize := k;
  if BasisSize > 0 then
     begin
     for i := 1 to EchelonCols do mask [i] := minus888;

     for i := 1 to Echelon.r do
         begin
         { scan along columns looking for a leading one }
         j := 1;
         repeat
           x := Echelon[i, j];
           if (x > -eps) and (x < eps) then   { check if its practically zero }
              Echelon [i, j] := 0.0;

           if (x > 1.0-eps) and (x < 1.0+eps) then  { x is then = 1.0 }
              begin
              mask [j] := minus999;        { tag as leading column }
              j := 0;   { exit signal }
              end
           else
              j := j + 1;

         until (j = 0) or (j > EchelonCols);

         end;  { end row scan }
     { Find non-leading columns }
     VectorCounter := 1;
     i := 1;  { i = column counter, check all columns }
     repeat
       for j := 1 to EchelonCols do tmpNullVectors[j, VectorCounter] := minus888;

       { STEP 5 }
       { remember, all minus888's in mask = non-leading columns }
       if mask [i] = minus888 then  { found a non-leading column }
          begin
          j := 1;
          { move down mask }
          for maskcount := 1 to EchelonCols do
               if (mask [maskcount] = minus999) then
                  begin
                  tmpNullVectors[maskcount, VectorCounter] := -Echelon[j, i];
                  inc (j);
                  end;

          { STEP 4 }
          { zero all -888 (free) entries }
          for j := 1 to EchelonCols do
              if tmpNullVectors[j, VectorCounter] = minus888 then
                 tmpNullVectors[j, VectorCounter] := 0.0;

          { STEP 2 AND 3 }
          { mark free variable }
          tmpNullVectors[i, VectorCounter] := 1.0;
          VectorCounter := VectorCounter + 1;
          end;
       inc (i);
     until i > EchelonCols;
     end
  else
     begin
     BasisSize := 0;
     NullVectors := Nil;
     end;
  finally
     if BasisSize > 0 then
        begin
        NullVectors := TMatrix.Create (Self.c, BasisSize);
        for i := 1 to Self.c do
            for j := 1 to BasisSize do
                NullVectors[i,j] := tmpNullVectors[i,j];
        end;
     mask.free;
     tmpNullVectors.free;
  end;
end;

               
function sign (a, b : TMatElement) : TMatElement;
begin
  if b >= 0.0 then
     result := abs (a)
  else
     result := -abs(a);
end;


function max (a, b : TMatElement) : TMatElement;
begin
  if a > b then
     result := a
  else
     result := b;
end;


{ Compute sqrt (a^2 + b^2) using numerically more stable method. If x = sqrt(a^2 + b^2),
then, x/a^2 = 1/a^2 sqrt (a^2 + b^2), mult both sides by sqrt(..), so
x/a^2 * sqrt (a^2 + b^2) = 1/a^2 (a^2 + b^2) or
x/a^2 * sqrt (a^2 + b^2) = 1 + (b/a)^2 but on left side 1/a^2 sqrt(a^2 + b^2) equals
x/a^2, therefore x * x/a^2 = 1 + (b/a) ^2, take square roots on both side yields:
x/a := sqrt (1+(b/a)^2), or FINALLY: x := a sqrt (1 + (b/a)^2) }

function pythag (a, b : TMatElement) : TMatElement;
var at, bt, ct : TMatElement;
begin
  result := sqrt (a*a + b*b);
  exit;
  at := abs (a); bt := abs (b);
  if at > bt then
     begin
     ct := bt/at;
     result := at*sqrt (1 + ct*ct);
     end
  else
     begin
     if bt > 0 then
        begin
        ct := at/bt;
        result := bt*sqrt (1 + ct*ct);
        end
     else
        result := 0.0;
     end;
end;

   function MyAbs (x : TMatElement) : TMatElement;
   begin
     if x < 0.0 then x := -x;
     result := x;
   end;


{procedure TMatrix.svd2 (var u : TMatrix; var w : TVector; var v : TMatrix);}
procedure TMatrix.svd2 (var u : TMatrix; var w : TVector; var v : TMatrix);
LABEL 1,2,3;
CONST
   nmax=100;
VAR
   n, m1, nm, l1, k, j, jj, its, i : integer;
   z, y, x, scale, s, h, g, f, cc, anorm : real;
   rv1 : TVector; //Aug : TMatrix;
   AugMatrix : boolean;

   function sign(a,b: TMatElement): TMatElement;
   begin
      if (b >= 0.0) then sign := abs(a) else sign := -abs(a)
   end;

   function max(a,b: TMatElement): TMatElement;
   begin
      if (a > b) then max := a else max := b
   end;

begin
  m1 := r; n := c; AugMatrix := false;
  (*if m < n then
     begin
     { More parameters than data ! Change structure of Self by augmenting
     Self with additional rows (entries set to zero) so that m = n, don't change m or n though }
     {Aug := TMatrix.Create (n, n); Aug.zero;
     try
       for i := 1 to m do
           for j := 1 to n do
               Aug[i,j] := Self[i,j];
       u.FreeSpace; u.SetSize (n, n); u.Copy (Aug);
       AugMatrix := true;
     finally
       Aug.free;
     end;
     end
  else*)
     u.Copy(Self); { Work on U, don't destroy Self }


  if AugMatrix then
     rv1 := TVector.Create (n)  { Make enough room }
  else
     rv1 := TVector.Create (m1); { Save some space }
   g := 0.0;
   scale := 0.0;
   anorm := 0.0;
   FOR i := 1 TO n DO BEGIN
      l1 := i+1;
      rv1[i] := scale*g;
      g := 0.0;
      s := 0.0;
      scale := 0.0;
      IF (i <= m1) THEN BEGIN
         FOR k := i TO m1 DO scale := scale + Myabs(u[k,i]);
         IF (Myabs(scale) > 1e-12) THEN BEGIN
         {IF (scale <> 0.0) THEN BEGIN}
            for k := i to m1 do
                begin
                u[k,i] := u[k,i]/scale;
                s := s + u[k,i]*u[k,i]
                end;
            f := u[i,i];
            g := -sign(sqrt(s),f);
            h := f*g-s;
            u[i,i] := f-g;
            if (i <> n) then
               begin
               for j := l1 to n do
                   begin
                   s := 0.0;
                   for k := i to m1 do s := s + u[k,i]*u[k,j];
                   f := s/h;
                   for k := i to m1 do u[k,j] := u[k,j] + f*u[k,i];
                   end
               end;
            for k := i to m1 do u[k,i] := scale*u[k,i]
         END
      END;
      w[i] := scale*g;
      g := 0.0;
      s := 0.0;
      scale := 0.0;
      IF ((i <= m1) AND (i <> n)) THEN BEGIN
         for k := l1 to n do scale := scale + Myabs(u[i,k]);
         if (Myabs(scale) > 1e-12) then begin
         {if (scale <> 0.0) then begin}
            for k := l1 to n do
                begin
                u[i,k] := u[i,k]/scale;
                s := s + u[i,k]*u[i,k]
                end;
            f := u[i,l1];
            g := -sign(sqrt(s),f);
            h := f*g-s;
            u[i,l1] := f-g;
            for k := l1 to n do rv1[k] := u[i,k]/h;
            if (i <> m1) then
               begin
               for j := l1 to m1 do
                   begin
                   s := 0.0;
                   for k := l1 to n do s := s + u[j,k]*u[i,k];
                   for k := l1 to n do u[j,k] := u[j,k] + s*rv1[k];
                   end
               end;
            for k := l1 to n do u[i,k] := scale*u[i,k];
         END
      END;
      anorm := max(anorm,(Myabs(w[i]) + Myabs(rv1[i])))
   END;

   FOR i := n DOWNTO 1 DO BEGIN
      IF (i < n) THEN BEGIN
         if (Myabs(g) > 1e-12) then
         {IF (g <> 0.0) THEN}
            begin
            for j := l1 to n do v[j,i] := (u[i,j]/u[i,l1])/g;
            for j := l1 to n do
                begin
                s := 0.0;
                for k := l1 to n do s := s + u[i,k]*v[k,j];
                for k := l1 to n do v[k,j] := v[k,j] + s*v[k,i]
                end
            end;
         for j := l1 to n do
             begin
             v[i,j] := 0.0;
             v[j,i] := 0.0;
             end
      END;
      v[i,i] := 1.0;
      g := rv1[i];
      l1 := i
   end;
   FOR i := n DOWNTO 1 DO BEGIN
      l1 := i+1;
      g := w[i];
      if (i < n) then for j := l1 to n do u[i,j] := 0.0;
      if (Myabs(g) > 1e-12) then
      {IF (g <> 0.0) THEN}
         begin
         g := 1.0/g;
         IF (i <> n) THEN
            begin
            for j := l1 to n do
                begin
                s := 0.0;
                for k := l1 to m1 do s := s + u[k,i]*u[k,j];
                f := (s/u[i,i])*g;
                for k := i to m1 do u[k,j] := u[k,j] + f*u[k,i];
                end
            end;
         for j := i to m1 do u[j,i] := u[j,i]*g;
      end else
         begin
         for j := i to m1 do u[j,i] := 0.0;
         end;
      u[i,i] := u[i,i]+1.0
   END;
   FOR k := n DOWNTO 1 DO BEGIN
      FOR its := 1 TO 30 DO BEGIN
         for l1 := k downto 1 do
             begin
             nm := l1-1;
             if ((Myabs(rv1[l1]) + anorm) - anorm < 1e-12) then goto 2;
             {if ((Myabs(rv1[l]) + anorm) = anorm) then goto 2;}
             if ((Myabs(w[nm]) + anorm) - anorm < 1e-12) then goto 1
             {if ((Myabs(w[nm]) + anorm) = anorm) then goto 1}
             end;
1:         cc := 0.0;
         s := 1.0;
         for i := l1 to k do
             begin
             f := s*rv1[i];
             if ((Myabs(f) + anorm) - anorm > 1e-12) then
             {if ((Myabs(f)+anorm) <> anorm) then}
                begin
                g := w[i];
                h := sqrt(f*f+g*g);
                w[i] := h;
                h := 1.0/h;
                cc := (g*h);
                s := -(f*h);
                for j := 1 to m1 do
                    begin
                    y := u[j,nm];
                    z := u[j,i];
                    u[j,nm] := (y*cc)+(z*s);
                    u[j,i] := -(y*s)+(z*cc)
                    end
                end
             end;
2:       z := w[k];
         if (l1 = k) then
             begin
             if (z < 0.0) then
               begin
               w[k] := -z;
               for j := 1 to n do v[j,k] := -v[j,k];
               end;
            GOTO 3
            end;
         if (its = 30) then writeln ('no convergence in 30 SVDCMP iterations');
         x := w[l1];
         nm := k-1;
         y := w[nm];
         g := rv1[nm];
         h := rv1[k];
         f := ((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
         g := sqrt(f*f+1.0);
         f := ((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x;
         cc := 1.0;
         s := 1.0;
         for j := l1 to nm do
             begin
             i := j+1;
             g := rv1[i];
             y := w[i];
             h := s*g;
             g := cc*g;
             z := sqrt(f*f+h*h);
             rv1[j] := z;
             cc := f/z;
             s := h/z;
             f := (x*cc)+(g*s);
             g := -(x*s)+(g*cc);
             h := y*s;
             y := y*cc;
             for jj := 1 to n do
                 begin
                 x := v[jj,j];
                 z := v[jj,i];
                 v[jj,j] := (x*cc)+(z*s);
                 v[jj,i] := -(x*s)+(z*cc)
                 end;
             z := sqrt(f*f+h*h);
             w[j] := z;
             if (Myabs(z) > 1e-12) then
             {if (z <> 0.0) then}
                begin
                z := 1.0/z;
                cc := f*z;
                s := h*z
                end;
             f := (cc*g)+(s*y);
             x := -(s*g)+(cc*y);
             for jj := 1 to m1 do
                 begin
                 y := u[jj,j];
                 z := u[jj,i];
                 u[jj,j] := (y*cc)+(z*s);
                 u[jj,i] := -(y*s)+(z*cc)
                 end
             end;
         rv1[l1] := 0.0;
         rv1[k] := f;
         w[k] := x
      END;
3:   END


END;


{ Perform a Singular Value Decompostion on self, returning u, w, and v, modified
from Numerical Recipes and Forsythe et al 1977, Computer methods for Math Calc }
procedure TMatrix.svd (var u : TMatrix; var w : TVector; var v : TMatrix);
label 3;
var i, j, k, l1, n, m1, its, flag, nm, jj : integer; rv1 : TVector;
    scale, g, h, f, anorm, s, cc, x, y, z : TMatElement; Aug : TMatrix;
    AugMatrix : boolean;
begin
  m1:= r; n := c; AugMatrix := false;
  if m1 < n then
     begin
     { More parameters than data ! Change structure of Self by augmenting
     Self with additional rows (entries set to zero) so that m = n, don't change m or n though }
     Aug := TMatrix.Create (n, n); Aug.zero;
     try
       for i := 1 to m1 do
           for j := 1 to n do
               Aug[i,j] := Self[i,j];
       u.FreeSpace; u.SetSize (n, n); u.Copy (Aug);
       AugMatrix := true;
     finally
       Aug.free;
     end;
     end
  else
     u.Copy(Self); { Work on U, don't destroy Self }

  scale := 0.0; g := 0.0; anorm := 0.0;
  if AugMatrix then
     rv1 := TVector.Create (n)  { Make enough room }
  else
     rv1 := TVector.Create (m1); { Save some space }

  try
  for i := 1 to n do
      begin
      l1 := i + 1;
      rv1[i] := scale * g;
      g := 0.0; s := 0.0; scale := 0.0;
      if i <= m1 then
         begin
         for k := i to m1 do scale := scale + abs (u[k,i]);
         if scale <> 0.0 then
            begin
            for k := i to m1 do
                begin
                u[k, i] := u[k, i] / scale;
                s := s + u[k,i]*u[k,i];
                end;
                f := u[i,i];
                g := -sign (sqrt (s), f);
                h := f*g - s;
                u[i,i] := f - g;
                if i <> n then
                   begin
                   for j := l1 to n do
                       begin
                       s := 0.0;
                       for k := i to m1 do s := s + u[k,i]*u[k,j];
                       f := s/h;
                       for k := i to m1 do u[k,j] := u[k,j] + f*u[k,i];
                       end;
                   end;
            for k := i to m1 do u[k,i] := u[k,i] * scale;
            end;
         end;
      w[i] := scale * g;
      g := 0.0; s := 0.0; scale := 0.0;
      if (i <= m1) and (i <> n) then
         begin
         for k := l1 to n do scale := scale + abs (u[i,k]);
         if scale <> 0.0 then
            begin
            for k := l1 to n do
                begin
                u[i,k] := u[i,k] / scale;
                s := s + u[i,k]*u[i,k];
                end;
            f := u[i,l1];
            g := -sign(sqrt (s), f);
            h := f*g - s;
            u[i,l1] := f - g;
            for k := l1 to n do rv1[k] := u[i,k]/h;
            if i <> m1 then
               begin
               for j := l1 to m1 do
                   begin
                   s := 0.0;
                   for k := l1 to n do s := s + u[j,k]*u[i,k];
                   for k := l1 to n do u[j,k] := u[j,k] + s*rv1[k];
                   end;
               end;
            for k := l1 to n do u[i,k] := u[i,k] * scale;
            end;
         end;
      anorm := max (anorm, abs(w[i]) + abs(rv1[i]));
      end;

  { ------------------------------------------ }
  { Accumulation of right-hand transformations }
  for i := n downto 1 do
      begin
      if i < n then
         begin
         if g <> 0.0 then
            begin
            for j := l1 to n do v[j,i] := (u[i,j]/u[i,l1])/g;
            for j := l1 to n do
                begin
                s := 0.0;
                for k := l1 to n do s := s + u[i,k]*v[k,j];
                for k := l1 to n do v[k,j] := v[k,j] + s*v[k,i];
                end;
            end;
         for j := l1 to n do begin v[i,j] := 0.0; v[j,i] := 0.0; end;
         end;
      v[i,i] := 1.0;
      g := rv1[i];
      l1 := i;
      end;

  { ------------------------------------------ }
  { Accumulation of left-hand transformations  }
  for i := n downto 1 do
      begin
      l1 := i + 1;
      g := w[i];
      if i < n then  for j := l1 to n do u[i,j] := 0.0;
         if g <> 0.0 then
            begin
            g := 1.0/g;
            if i <> n then
               begin
               for j := l1 to n do
                   begin
                   s := 0.0;
                   for k := l1 to m1 do s := s + u[k,i]*u[k,j];
                   f := (s/u[i,i])*g;
                   for k := i to m1 do u[k,j] := u[k,j] + f*u[k,i];
                   end;
               end;
            for j := i to m1 do u[j,i] := u[j,i] * g;
            end
         else
            begin
            for j := i to m1 do u[j,i] := 0.0;
            end;
      u[i,i] := u[i,i] + 1.0;
      end;

  { --------------------------------------------- }
  { Diagonalization of the bidiagonal form        }
  for k := n downto 1 do
      begin
      for its := 1 to 30 do
          begin
          flag := 1;
          for l1 := k downto 1 do
              begin
              nm := l1 - 1;
              if abs (rv1[l1] + anorm) = anorm then
                 begin
                 flag := 0;
                 break;
                 end;
              if abs (w[nm] + anorm) = anorm then break;
              end;
          if flag <> 0 then
             begin
             cc := 0.0; s := 1.0;
             for i := l1 to k do
                 begin
                 f := s * rv1[i];
                 if (abs (f) + anorm) <> anorm then
                    begin
                    g := w[i];
                    h := pythag (f, g);
                    w[i] := h;
                    h := 1.0/h;
                    cc := g*h;
                    s := -f*h;
                    for j := 1 to m1 do
                        begin
                        y := u[j,nm];
                        z := u[j, i];
                        u[j,nm] := y*cc + z*s;
                        u[j,i]  := z*cc - y*s;
                        end;
                    end;
                 end;
             end;
          z := w[k];
          if l1 = k then
             begin
             if z < 0.0 then
                begin
                w[k] := -z;
                for j := 1 to n do v[j,k] := -v[j,k];
                end;
             {break;} goto 3;
             end;
      if (its = 30) then raise Exception.Create ('Exceeded iterations in SVD routine');
      x := w[l1];
      nm := k - 1;
      y := w[nm]; g := rv1[nm];
      h := rv1[k];
      f := ((y - z)*(y + z) + (g - h)*(g + h))/(2.0*h*y);
      g := pythag (f, 1.0);
      f := ((x - z) * (x + z) + h*((y/(f + sign(g, f))) - h))/x;

      cc := 1.0; s := 1.0;
      for j := l1 to nm do
          begin
          i := j + 1;
          g := rv1[i];
          y := w[i]; h := s*g;
          g := cc*g;
          z := pythag (f, h);
          rv1[j] := z;
          cc := f/z; s := h/z;
          f := x*cc + g*s; g := g*cc - x*s;
          h := y*s;
          y := y*cc;
          for jj := 1 to n do
              begin
              x := v[jj,j]; z := v[jj,i];
              v[jj,j] := x*cc + z*s;
              v[jj,i] := z*cc - x*s;
              end;
          z := pythag (f, h);
          w[j] := z;
          if z <> 0 then
             begin
             z := 1.0/z; cc := f*z; s := h*z;
             end;
          f := (cc*g) + (s*y);
          x := (cc*y) - (s*g);
          for jj := 1 to m1 do
              begin
              y := u[jj,j]; z := u[jj,i];
              u[jj,j] := y*cc + z*s;
              u[jj,i] := z*cc - y*s;
              end;
          end;
      rv1[l1] := 0.0;
      rv1[k] := f;
      w[k] := x;
3:      end;
  end;
  finally
    rv1.free;
  end;

  if AugMatrix then
     begin
     { This means that originally m < n, therefore u has some junk rows, remove them here }
     Aug := TMatrix.Create (m1, n);
     try
       for i := 1 to m1 do
           for j := 1 to n do
               Aug[i,j] := u[i,j];
       u.FreeSpace; u.SetSize (m1, n); u.Copy (Aug);
     finally
       Aug.free;
     end;
     end;
end;



{ Call this after having called svd, computes x = V [diag (1/wj)]. U^t.b }
procedure TMatrix.svdSolve (var u : TMatrix; var w : TVector; var v : TMatrix;
                            b : TVector; var x : TVector);
var j, i, n, m1 : integer; s: TMatElement; tmp: TVector;
begin
  m1 := u.r; n := u.c;
  tmp := TVector.Create (u.c);
  try
    { Compute diag (1/wj) . U^t . b }
    for j := 1 to n do
        begin
        s := 0.0;
        if (w[j] <> 0.0) then
           begin
           for i := 1 to m1 do s := s + u[i,j]*b[i];
           s := s/w[j]
           end;
        tmp[j] := s
        end;
    { ...mult by V to get solution vector x }
    for i := 1 to n do
        begin
        s := 0.0;
        for j := 1 to w.size do s := s + v[i,j]*tmp[j];
        x[i] := s
        end;
  finally
    tmp.free;
  end;
end;


{ Solves the equation: (A.a - b)^2 = 0 for a. Where, A is the 'design matrix',
Aij = Xj(xi)/sigi, where Xj is the value of the jth basis function; b is the set
of weighted observed y values, b = yi/sigi; and a is the set of fitting coefficients
for the basis functions. Thus A.a - b expresses predicted - observed }

{ BasisProc is a procedure which must return in an array the values for the
basis functions at a particular value of xi, i.e it computes, Xj(xi) }

function TMatrix.svdfit (x, y, yerr : TVector; var fit : TVector;
        var u, v : TMatrix; var w : TVector; funcs : BasisProc): TMatElement;
const
   tol=1.0e-5;
var
   i, j : integer; wmax, weight, thresh, sum: TMatElement;
   BasisVal, b : TVector; A : TMatrix;
begin
  BasisVal := TVector.Create (fit.size); b := TVector.Create (x.size);
  A := TMatrix.Create (x.size, fit.size);
  try
    { Form the A matrix }
    for i := 1 to x.size do
        begin
        funcs(x[i], BasisVal);
        weight := 1.0/yerr[i];
        for j := 1 to fit.size do A[i,j] := BasisVal[j]*weight;
        b[i] := y[i]*weight
        end;
    A.svd (u, w, v);

    wmax := 0.0;
    for j := 1 to fit.size do if (w[j] > wmax) then wmax := w[j];
    thresh := tol*wmax;
    for j := 1 to fit.size do if (w[j] < thresh) then w[j] := 0.0;

    svdSolve (u, w, v, b, fit);

    result := 0.0;  { chisqr set to zero ready to accumulate }
    for i := 1 to x.size do
        begin
        funcs(x[i], BasisVal);
        sum := 0.0;
        for j := 1 to fit.size do sum := sum + fit[j]*BasisVal[j];
        result := result + sqr((y[i]-sum)/yerr[i]); { Accumulate chisqr }
        end;
  finally
    BasisVal.free; A.free; b.free;
  end;
end;


procedure TMatrix.svdCovar (v : TMatrix; w : TVector; alpha : TMatrix);
var i, j, k : integer; wti : TVector; sum : TMatElement;
begin
  wti := TVector.Create (w.size);
  try
    for i := 1 to w.size do
        begin
        wti[i] := 0.0;
        if w[i] > 0.0 then wti[i] := 1.0/(w[i]*w[i]);
        end;
    for i := 1 to w.size do
        begin
        for j := 1 to i do
            begin
            sum := 0.0;
            for k := 1 to w.size do sum := sum + v[i,k]*v[j,k]*wti[k];
            alpha[j,i] := sum; alpha[i,j] := alpha[j,i];
            end;
        end;
  finally
    wti.free;
  end;
end;


procedure TMatrix.eliminate_cms (S, Tk1 : TMatrix; var cr, N : integer);            (* eliminating conserved moieties *)
var
  i,j,x,y,crc,old_cr : byte;
begin
  x := 0; cr := 0;                    (* cr - conservation relations *)
  for i := 1 to N do
      begin
      old_cr := cr;
      for j := i+1 to N do
          begin
          crc := 0;                                 (* crc - cr counter *)
          // S.c = number of reactions
          for y := 1 to S.c do crc := crc + trunc (abs(S[i,y]+S[j,y]));
          if crc = 0 then cr := cr+1;
          end;
      if cr = old_cr then
         begin
         x := x+1;
         for y := 1 to S.c do
             Tk1[x,y] := S[i,y];
         end;
      end;
end;


procedure TMatrix.ElementaryModes (D : TVectori; var mf, mb, C1, k : integer; Tk : TMatrix);
var i, j, cr, N, k1 : integer; Tk1 : TMatrix; hlpRow : TVector;
begin
  N := Self.r;
  Tk1 := TMatrix.Create (Self.r, Self.c);
  hlpRow := TVector.Create (Self.c);
  try
    {eliminate_cms;                       (* also transscribing S into  Tk1 *)
     N := N-cr;

     for i := 1 to R do
         begin
         for j := 1 to N do Tk[i,j] := Tk1[j,i];       (* transposing matrix  *)
         for j:=N+1 to N+R do
             if i=j-N then
                Tk[i,j]:=1       (* appending..  *)
             else Tk[i,j]:=0;                                         (*..unity matrix*)
         end;
                                    (* (preliminary) fund. rows to the top *)
    i := 0;                         (*          splitting indices into F/B *)
    for j := 1 TO R DO
    begin
      if (D[j] <> 0) then
         begin
         i := i+1;
         hlprow := Tk[i];
         Tk[i] := Tk[j];
         Tk[j] := hlprow;
         end;
    end;
    mf := i;                                     (* no. of fundamental rows *)
    mb := R-mf;}



    eliminate_cms (Self, Tk1, cr, N);          (* also transscribing S into  Tk1 *)
    N := N-cr;

    for i := 1 to Self.c do
        begin
        for j := 1 to N do Tk[i,j] := Tk1[j,i];     (* transposing matrix  *)
        for j := N+1 to N+Self.c do
            if i=j-N then
               Tk[i,j] := 1       (* appending..  *)
            else Tk[i,j] := 0;    (*..unity matrix*)
        end;
                                  (* (preliminary) fund. rows to the top *)
    i := 0;                       (*          splitting indices into F/B *)
    for j := 1 TO Self.c DO
    begin
      if (D[j] <>  0) then
         begin
         i := i+1;
         for k1 := 1 to Self.c do hlprow[k1] := Tk[i,k1];
         for k1 := 1 to Self.c do Tk[i,k1] := Tk[j,k1];
         for k1 := 1 to Self.c do Tk[j,k1] := hlprow[k1];
         //hlprow := Tk[i];
         //Tk[i] := Tk[j];
         //Tk[j] := hlprow;
         end;
    end;
    mf := i;                                     (* no. of fundamental rows *)
    mb := Self.c-mf;                                  (* no. of basis rows       *)

    Tableau (N, Self.c, mf, mb, C1, k, Tk, Tk1);
  finally
    hlpRow.Free;
    Tk1.Free;
  end;
end;


class function TMatrix.grecodiv(P, Rest: integer) : integer;
var
  old_Rest : integer;
begin
  grecodiv := 1;
  if (Rest*P <> 0) then
      begin
      if ABS(P) < ABS(Rest) then
         begin
         old_Rest := Rest;
         Rest := P;
         P := old_Rest;    (* swap P 'n' R *)
         end;

      repeat                                     (* Euclidean Algorithm: *)
        old_Rest := Rest;
	Rest := P mod old_Rest;
	P := old_Rest;
      until (Rest = 0);
      grecodiv := P;
      end
  else
      if (P = 0) then
         begin
         if (Rest = 0) then grecodiv := 1 else grecodiv := Rest;
         end
      else grecodiv := P;
end;


class function TMatrix.grecodiv_of_vector (N, R1 : integer; vec : TVector) : integer;
var
  x      : byte;
  coeff  : integer;
begin
  coeff := trunc (vec[1]);
  for x := 2 to (N+R1) do
      begin
      if (vec[x] <> 0) then coeff := grecodiv(trunc (vec[x]), coeff);
      end;
  grecodiv_of_vector := coeff;
end;





class procedure TMatrix.Tableau (N, R1 : integer; var mf, mb, C1, k : integer; Tk, Tk1 : TMatrix);
var
  i,j,k1,x,xa,y,m1      : integer;
  cf,dir,ifrom,iend      : integer;
  index,bool,allow_comb  : boolean;
  l1 : integer;
  vec : TVector;
begin
  C1 := R1;                            (* C: number of rows of the tableau *)
  k := 0;                                            (* k: tableau index *)
  vec := TVector.Create (Tk1.c);

  repeat

    //output;                                        (* HELPFUL MONITORING*)
    //write(' k =  ');writeln(k);writeln('cf=',cf);           (* OF TABLEAU STEPS   *)
    {write(' Press <ENTER> to continue.'); readln;}

    l1 := 1;                         (* l: row index in the tableau k+1 *)
    cf := 0;                        (* counter for f-rows in the tableau k+1*)
    for dir :=1 to 2 do
        BEGIN
        IF dir=1 THEN
           BEGIN
           ifrom:=1; iend:=mf;
           END
        ELSE
           BEGIN
           ifrom:=mf+1; iend:=c1
           END;
           FOR i := ifrom TO iend DO
               BEGIN
               IF Tk[i,k+1] = 0 THEN                     (*  copying rows that  *)
                  BEGIN                                  (* have a zero element *)
                  for k1 := 1 to Tk1.c do
                      Tk1[l1, k1] := Tk[i, k1];             (*      already        *)
                  //Tk1[l] := Tk[i];                     (*      already        *)
                  l1 := l1+1;
                  IF i <= mf THEN cf := cf+1;
                  END
               END;
               FOR i:=ifrom TO iend DO
                   BEGIN
                   IF Tk[i,k+1]<>0 THEN
                      BEGIN
                      FOR j := i+1 TO C1 DO
                          BEGIN
                          IF Tk[j,k+1] <> 0 THEN
                             BEGIN
                             IF Tk[i,k+1]*Tk[j,k+1] > 0 THEN
                                BEGIN             (* not for f-rows with *)
                                IF j <= mf THEN
                                   allow_comb := false    (*      same signum    *)
                                ELSE
                                   BEGIN
                                   FOR y := 1 TO N+R1 DO Tk[j,y] := -1 * Tk[j,y]; (* invert b-row *)
                                   allow_comb := true;
                                   END;
                                END
                             ELSE allow_comb := true;
                             IF allow_comb THEN
                                BEGIN
                                index:=true;                   (* first simplicity (S) test: *)
                                IF (l1>1) THEN
                                   BEGIN
                                   IF dir=1 THEN x:=0
                                   ELSE x:=cf;
	                           WHILE (x<l1-1) AND (INDEX) DO
                                         BEGIN
	                                 x:=x+1;
	                                 y:=n;
	                                 bool:=true;
	                                 REPEAT
	                                   y:=y+1;
	                                   IF ((Tk[i,y] = 0) and (Tk[j,y] = 0)) THEN
	                                      IF Tk1[x,y] <> Tk[i,y] THEN bool:=false;
	                                 UNTIL (y=n+r1)or NOT bool;
	                                 IF (y=n+r1)and bool THEN index:=false;
                                         END;
                                   END;
	                           IF index THEN
                                      BEGIN              (* combine rows *)
	                              FOR y:=1 TO R1+N DO
	                                  Tk1[l1,y]:=abs(Tk[i,k+1])*Tk[j,y]+abs(Tk[j,k+1])*Tk[i,y];

                                      for k1 := 1 to Tk1.c do
                                          vec[i] := Tk1[l1,k1];

                                      m1:= Grecodiv_of_vector(N, R1, vec) ;
                                      //m:= Grecodiv_of_vector(Tk1[l]) ;
                                      IF (ABS(m1)<>1) AND (m1<>0) THEN FOR y:=1 to R1+N DO
                                      Tk1[l1,y]:= trunc (Tk1[l1,y]) DIV ABS(m1);
                                      l1:= l1+1;
                                      IF i <= mf THEN cf := cf+1;
                                          (* second simplicity (S) test: *)
                                      IF dir=1 THEN x:=0
                                      ELSE x:=cf;
                                      bool:=true;
	                              WHILE (X<L1-2) AND (bool=true) DO
                                            BEGIN
	                                    x:=x+1;
	                                    y:=n;
	                                    bool:=false;
	                                    REPEAT
	                                      y:=y+1;
	                                      IF Tk1[x,y]=0 THEN
	                                         IF (Tk1[x,y]<>Tk[i,y]) OR (Tk1[x,y]<>Tk[j,y])
                                                    THEN bool:=true;
	                                    UNTIL (y=n+r1)or bool;
	                                    IF (y=n+r1)and NOT(bool) THEN
                                               BEGIN
                                               {writeln('Jetzt hat folgende Zeile:');
                                               FOR Y:=n+1 to n+r DO
                                                  write(Tk1[x,y]:3);
                                               writeln; writeln('x=',x);
                                               writeln; writeln('l-1=',l-1);
                                               writeln('verloren gegen folgende Zeilen:');
                                               FOR Y:=n+1 to n+r DO
                                                   write(Tk[i,y]:3);
                                               writeln; writeln('i=',i);
                                               FOR Y:=n+1 to n+r DO
                                                   write(Tk[j,y]:3);
                                               writeln; writeln('j=',j); writeln;
                                               writeln(x,'+1te Zeile:');
                                               FOR Y:=n+1 to n+r DO
                                               write(Tk1[x+1,y]:3);
                                               writeln;}
                                               FOR xa:=x TO l1-2 DO
                                                   BEGIN
                                                   FOR y:=1 TO n+r1 DO
                                                       Tk1[xa,y]:=Tk1[xa+1,y];
                                                   END;
                                               l1:=l1-1;
                                               IF x<=cf THEN cf:=cf-1;
                                               END;
                                            END;
	                              END;
                                   END;
                                END;
                             END;
                          END;
                      END;
                   END;

        C1 := l1-1;                                      (* new no. of rows   *)
        mf := cf;
        mb := C1-mf;

       k := k+1;                                      (*  next tableau        *)
       for i := 1 to C1 do
           begin
           for k1 := 1 to Tk.c do
               Tk[i, k1] := Tk1[i, k1];            (*  restarting with Tk1 *)
           end;
       //for i := 1 to C do Tk[i] := Tk1[i];            (*  restarting with Tk1 *)

  until (k = N) or ((mb = 0) and (mf = 0));

  //if ((mb = 0) and (mf = 0)) then
  //    writeln(' There exist neither irreversible nor reversible flux modes.')
  //else
  //   output;
  vec.Free;
end;


// Evaluate conservation relations, uses the algorthim: tr(ns(tr(m)))
procedure TMatrix.Conserve(st : TMatrix);
var tmp, ns, echelon : TMatrix; b, r1 : integer;
begin
  tmp := TMatrix.Create (st.c, st.r);
  ns  := TMatrix.Create (1,1);
  echelon := TMatrix.Create (1,1);
  try
    tmp.Transpose (st);
    tmp.NullSpace (ns, b, Echelon, r1);
    Self.SetSize (ns.c, ns.r);
    Self.Transpose (ns);
  finally
    ns.free;
    echelon.free;
    tmp.free;
  end;
end;


end.