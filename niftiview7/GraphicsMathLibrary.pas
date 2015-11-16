// Graphics Math Library
//
// Copyright (C) 1982, 1985, 1992, 1995-1998 Earl F. Glynn, Overland Park, KS.
// All Rights Reserved.  E-Mail Address:  EarlGlynn@att.net

UNIT GraphicsMathLibrary;  // Matrix/Vector Operations for 2D/3D Graphics}

INTERFACE

  USES
    SysUtils,dialogs;  {Exception}

  CONST
    sizeUndefined = 1;
    size2D        = 3;   // 'size' of 2D homogeneous vector or transform matrix
	size3D        = 4;   // 'size' of 3D homogeneous vector or transform matrix

  TYPE
    EVectorError = CLASS(Exception);
    EMatrixError = CLASS(Exception);

    TAxis       = (axisX, axisY, axisZ);
    TCoordinate = (coordCartesian, coordSpherical, coordCylindrical);
    TDimension  = (dimen2D, dimen3D);  // two- or three-dimensional TYPE
    TIndex      = 1..4;      // index of 'TMatrix' and 'TVector' TYPEs

    TMatrix     =            // transformation 'matrix'
      RECORD
        size:    TIndex;
        matrix:  ARRAY[TIndex,TIndex] OF single //azx DOUBLE
      END;
    TMatrixI     =            // transformation 'matrix'
      RECORD
        size:    TIndex;
        matrix:  ARRAY[TIndex,TIndex] OF longint
      END;

    Trotation   = (rotateClockwise, rotateCounterClockwise);

    // Normally the TVector TYPE is used to define 2D/3D homogenous
    // cartesian coordinates for graphics, i.e., (x,y,1) for 2D and
    // (x,y,z,1) for 3D.
    //
    // Cartesian coordinates can be converted to spherical (r, theta, phi),
    // or cylindrical coordinates (r,theta, z).  Spherical or cylindrical
    // coordinates can be converted back to cartesian coordinates.
    TVector     =
      RECORD
        size: TIndex;
        CASE INTEGER OF
          0:  (vector:  ARRAY[TIndex] OF single);
          1:  (x:  single;
               y:  single;
               z:  single;   // contains 'h' for 2D cartesian vector
               h:  single)
        END;

    TIntVector     =
      RECORD
        size: TIndex;
        CASE INTEGER OF
          0:  (vector:  ARRAY[TIndex] OF integer);
          1:  (x:  integer;
               y:  integer;
               z:  integer;   // contains 'h' for 2D cartesian vector
               h:  integer)
        END;
                                                 // Vector Operations

//  FUNCTION Vector2D  (CONST xValue, yValue:          DOUBLE):  TVector;
  FUNCTION Vector3D  (CONST xValue, yValue, zValue:  DOUBLE):  TVector;
  Function SameVec (const u,v: TVector): boolean;
  FUNCTION Transform (CONST u:  TVector; CONST a:  TMatrix):  TVector;
  (*  FUNCTION AddVectors (CONST u,v:  TVector):  TVector;
//  FUNCTION Transform (CONST u:  TVector; CONST a:  TMatrix):  TVector;
   *)
  //FUNCTION DotProduct  (CONST u,v:  TVector):  DOUBLE;
  //FUNCTION CrossProduct(CONST u,v:  TVector):  TVector;


                                                 // Basic Matrix Operations

  FUNCTION Matrix2D (CONST m11,m12,m13,          // 2D "graphics" matrix
                           m21,m22,m23,
                           m31,m32,m33:  DOUBLE):  TMatrix;

  FUNCTION Matrix3D (CONST m11,m12,m13,m14,      // 3D "graphics" matrix
						   m21,m22,m23,m24,
						   m31,m32,m33,m34,
						   m41,m42,m43,m44:  DOUBLE):  TMatrix;

  FUNCTION MultiplyMatrices (CONST a,b:  TMatrix):  TMatrix;
  FUNCTION InvertMatrix3D  (CONST Input:TMatrix):  TMatrix;
  Function Eye3D: TMatrix; //returns identity matrix
  FUNCTION InvertMatrix  (CONST a,b:  TMatrix; VAR determinant:  DOUBLE):  TMatrix;
  FUNCTION InvertMatrix1  (CONST a:  TMatrix; VAR determinant:  DOUBLE):  TMatrix;

                                                 // Transformation Matrices
procedure RotatePitch(lAngleDeg: double;  var lM:  TMatrix);
procedure RotateRoll(lAngleDeg: double;  var lM:  TMatrix);
procedure RotateYaw(lAngleDeg: double;  var lM:  TMatrix);

  FUNCTION RotateMatrix     (CONST dimension:  TDimension;
                             CONST xyz      :  TAxis;
                             CONST angle    :  DOUBLE;
                             CONST rotation :  Trotation):  TMatrix;

//  FUNCTION ScaleMatrix      (CONST s:  TVector):  TMatrix;

//  FUNCTION TranslateMatrix  (CONST t:  TVector):  TMatrix;

  FUNCTION ViewTransformMatrix (CONST coordinate:  TCoordinate;
       CONST azimuth {or x}, elevation {or y}, distance {or z}:  DOUBLE;
       CONST ScreenX, ScreenY, ScreenDistance:  DOUBLE):  TMatrix;


                                                 // conversions

//  FUNCTION FromCartesian (CONST ToCoordinate:  TCoordinate; CONST u:  TVector):  TVector;
//  FUNCTION ToCartesian   (CONST FromCoordinate:  TCoordinate; CONST u:  TVector):  TVector;

  //FUNCTION ToDegrees(CONST angle {radians}:  DOUBLE):  DOUBLE {degrees};
  FUNCTION ToRadians(CONST angle {degrees}:  DOUBLE):  DOUBLE {radians};


                                                 // miscellaneous

  FUNCTION  Defuzz(CONST x:  DOUBLE):  DOUBLE;
{  FUNCTION  GetFuzz:  DOUBLE;
  PROCEDURE SetFuzz(CONST x:  DOUBLE);
 }

IMPLEMENTATION

Function Eye3D: TMatrix; //returns identity matrix
begin
     result := Matrix3D   (1,0,0,0,
                         0,1,0,0,
                         0,0,1,0,
                         0,0,0,1);
  end;

procedure RotatePitch(lAngleDeg: double;  var lM:  TMatrix);
var
   lRads,lCos,lSin: double;
   lRot: TMatrix;
begin
   lRads := ToRadians(lAngleDeg);
   //lRads :=//showmessage(floattostr(lRads));
   lCos := cos(lRads);
   lSin := sin(lRads);
     lRot := Matrix3D   (1,0,0,0,
                         0,lcos,-lsin,0,
                         0,lsin,lcos,0,
                         0,0,0,1);
   lM := MultiplyMatrices(lM,lRot);
end;

procedure RotateRoll(lAngleDeg: double;  var lM:  TMatrix);
var
   lRads,lCos,lSin: double;
   lRot: TMatrix;
begin
   lRads := ToRadians(lAngleDeg);
   lCos := cos(lRads);
   lSin := sin(lRads);
     lRot := Matrix3D   (lcos,0,lsin,0,
                         0,1,0,0,
                         -lsin,0,lcos,0,
                         0,0,0,1);
   lM := MultiplyMatrices(lM,lRot);
end;

procedure RotateYaw(lAngleDeg: double;  var lM:  TMatrix);
var
   lRads,lCos,lSin: double;
   lRot: TMatrix;
begin
   lRads := ToRadians(lAngleDeg);
   lCos := cos(lRads);
   lSin := sin(lRads);
     lRot := Matrix3D   (lcos,-lsin,0,0,
                         lsin,lcos,0,0,
                         0,0,1,0,
                         0,0,0,1);
   lM := MultiplyMatrices(lM,lRot);
end;

  VAR
    fuzz : DOUBLE;


// *************************  Vector Operations *************************

  // This procedure defines two-dimensional homogeneous coordinates (x,y,1)
  // as a single 'vector' data element 'u'.  The 'size' of a two-dimensional
  // homogenous vector is 3.
 (* FUNCTION Vector2D  (CONST xValue, yValue:  DOUBLE):  TVector;

  BEGIN
    WITH RESULT DO
    BEGIN
      x    := xValue;
      y    := yValue;
      z    := 1.0;      // should be 'h' but let's use the 'z' slot to keep
      size := size2D    // subscripting possible
    END
  END {Vector2D};
   *)

   Function SameVec (const u,v: TVector): boolean;
   begin
       if (u.x=v.x) and (u.y=v.y) and (u.z=v.z) then
          result := true
       else
           result := false;

   end;
  // This procedure defines three-dimensional homogeneous coordinates
  // (x,y,z,1) as a single 'vector' data element 'u'.  The 'size' of a
  // three-dimensional homogenous vector is 4.
  FUNCTION Vector3D  (CONST xValue, yValue, zValue:  DOUBLE):  TVector;
  BEGIN
    WITH RESULT DO
    BEGIN
      x    := xValue;
      y    := yValue;
      z    := zValue;
      h    := 1.0;       // homogeneous coordinate
      size := size3D
    END
  END {Vector3D};


  // AddVectors adds two vectors defined with homogeneous coordinates.
  FUNCTION AddVectors (CONST u,v:  TVector):  TVector;
    VAR
      i: TIndex;
  BEGIN
    IF  (u.size IN [size2D..size3D])  AND
        (v.size IN [size2D..size3D])  AND
        (u.size = v.size)
    THEN BEGIN
      RESULT.size := u.size;
      FOR i := 1 TO u.size-1 DO     {2D + 2D = 2D  or  3D + 3D = 3D}
      BEGIN
        RESULT.vector[i] := u.vector[i] + v.vector[i]
      END;
      RESULT.vector[u.size] := 1.0   {homogeneous coordinate}
    END
    ELSE raise EVectorError.Create('Vector Addition Mismatch')
  END {AddVectors};


  // 'Transform' multiplies a row 'vector' by a transformation 'matrix'
  // resulting in a new row 'vector'.  The 'size' of the 'vector' and 'matrix'
  // must agree.  To save execution time, the vectors are assumed to contain
  // a homogeneous coordinate.
  FUNCTION Transform (CONST u:  TVector; CONST a:  TMatrix):  TVector;
    VAR
      i,k :  TIndex;
      temp:  DOUBLE;
  BEGIN
    RESULT.size := a.size;
    IF  a.size = u.size
    THEN BEGIN
      FOR i := 1 TO a.size-1 DO
      BEGIN
        temp := 0.0;
        FOR k := 1 TO a.size DO
        BEGIN
          temp := temp + u.vector[k]*a.matrix[k,i];
        END;
        RESULT.vector[i] := Defuzz(temp)
      END;
      RESULT.vector[a.size] := 1.0 {assume homogeneous coordinate}
    END
    ELSE raise EMatrixError.Create('Transform multiply error')
  END {Transform};


  // Assume vector contains 'extra' homogeneous coordinate -- ignore it.
  FUNCTION DotProduct  (CONST u,v:  TVector):  DOUBLE;
    VAR
      i:  INTEGER;
  BEGIN
    IF  (u.size = v.size)
    THEN BEGIN
      RESULT := 0.0;
      FOR i := 1 TO u.size-1 DO
      BEGIN
        RESULT := RESULT + u.vector[i] * v.vector[i];
      END;
    END
    ELSE RAISE EMatrixError.Create('Vector dot product error')
  END; {DotProduct}


  // Assume vector contains 'extra' homogeneous coordinate -- ignore it.
  FUNCTION CrossProduct(CONST u,v:  TVector):  TVector;
  BEGIN
    IF  (u.size = v.size) AND (u.size = size3D)
    THEN BEGIN
      RESULT := Vector3D( u.y*v.z - v.y*u.z,
                         -u.x*v.z + v.x*u.z,
                          u.x*v.y - v.x*u.y)
    END
    ELSE RAISE EMatrixError.Create('Vector cross product error')
  END; {CrossProduct}


// *********************** Basic Matrix Operations **********************

  FUNCTION Matrix2D (CONST m11,m12,m13, m21,m22,m23, m31,m32,m33:  DOUBLE):
                     TMatrix;
  BEGIN
    WITH RESULT DO
    BEGIN
      matrix[1,1] := m11; matrix[1,2] := m12; matrix[1,3] := m13;
      matrix[2,1] := m21; matrix[2,2] := m22; matrix[2,3] := m23;
      matrix[3,1] := m31; matrix[3,2] := m32; matrix[3,3] := m33;
      size := size2D
    END
  END {Matrix2D};


  FUNCTION Matrix3D (CONST m11,m12,m13,m14, m21,m22,m23,m24,
                           m31,m32,m33,m34, m41,m42,m43,m44:  DOUBLE):  TMatrix;
  BEGIN
    WITH RESULT DO
    BEGIN
      matrix[1,1] := m11; matrix[1,2] := m12;
      matrix[1,3] := m13; matrix[1,4] := m14;

      matrix[2,1] := m21; matrix[2,2] := m22;
      matrix[2,3] := m23; matrix[2,4] := m24;

      matrix[3,1] := m31; matrix[3,2] := m32;
      matrix[3,3] := m33; matrix[3,4] := m34;

      matrix[4,1] := m41; matrix[4,2] := m42;
      matrix[4,3] := m43; matrix[4,4] := m44;
      size := size3D
    END
  END {Matrix3D};


  // Compound geometric transformation matrices can be formed by multiplying
  // simple transformation matrices.  This procedure only multiplies together
  // matrices for two- or three-dimensional transformations, i.e., 3x3 or 4x4
  // matrices.  The multiplier and multiplicand must be of the same dimension.
 FUNCTION MultiplyMatrices (CONST a,b:  TMatrix):  TMatrix;
    VAR
      i,j,k:  TIndex;
      temp :  DOUBLE;
  BEGIN
	RESULT.size := a.size;
    IF  a.size = b.size
    THEN

      FOR i := 1 TO a.size DO
      BEGIN
        FOR j := 1 TO a.size DO
        BEGIN

          temp := 0.0;
          FOR k := 1 TO a.size DO
          BEGIN
            temp := temp + a.matrix[i,k]*b.matrix[k,j];
          END;
          RESULT.matrix[i,j] := Defuzz(temp)

        END
      END
	ELSE Showmessage('shit'+inttostr(a.size)+'x'+inttostr(b.size));
    //ELSE EMatrixError.Create('MultiplyMatrices error')
  END {MultiplyMatrices};



PROCEDURE lubksb(a: {glnpbynp}TMatrix; n: integer; indx: TIntVector; VAR b: TVector);
VAR
   j,ip,ii,i: integer;
   sum: double;
BEGIN
   ii := 0;
   FOR i := 1 TO n DO BEGIN
      ip := indx.vector[i];
      sum := b.vector[ip];
      b.vector[ip] := b.vector[i];
      IF  (ii <> 0) THEN BEGIN
         FOR j := ii TO i-1 DO BEGIN
            sum := sum-a.matrix[i,j]*b.vector[j]
         END
      END ELSE IF (sum <> 0.0) THEN BEGIN
         ii := i
      END;
      b.vector[i] := sum
   END;
   FOR i := n DOWNTO 1 DO BEGIN
      sum := b.vector[i];
      IF (i < n) THEN BEGIN
         FOR j := i+1 TO n DO BEGIN
            sum := sum-a.matrix[i,j]*b.vector[j]
         END
      END;
      b.vector[i] := sum/a.matrix[i,i]
   END
end;

  PROCEDURE ludcmp(VAR a: TMatrix;  n: integer;
       VAR indx: TIntVector; VAR d: double);
CONST
   tiny=1.0e-20;
VAR
   k,j,imax,i: integer;
   sum,dum,big: real;
   vv: TVector;
BEGIN
   d := 1.0;
   FOR i := 1 TO n DO BEGIN
      big := 0.0;
      FOR j := 1 TO n DO IF (abs(a.matrix[i,j]) > big) THEN big := abs(a.matrix[i,j]);
      IF (big = 0.0) THEN BEGIN
         writeln('pause in LUDCMP - singular matrix'); readln
      END;
      vv.vector[i] := 1.0/big
   END;
   FOR j := 1 TO n DO BEGIN
      FOR i := 1 TO j-1 DO BEGIN
         sum := a.matrix[i,j];
         FOR k := 1 TO i-1 DO BEGIN
            sum := sum-a.matrix[i,k]*a.matrix[k,j]
         END;
         a.matrix[i,j] := sum
      END;
      big := 0.0;
      FOR i := j TO n DO BEGIN
         sum := a.matrix[i,j];
         FOR k := 1 TO j-1 DO BEGIN
            sum := sum-a.matrix[i,k]*a.matrix[k,j]
         END;
         a.matrix[i,j] := sum;
         dum := vv.vector[i]*abs(sum);
         IF (dum > big) THEN BEGIN
            big := dum;
            imax := i
         END
      END;
      IF (j <> imax) THEN BEGIN
         FOR k := 1 TO n DO BEGIN
            dum := a.matrix[imax,k];
            a.matrix[imax,k] := a.matrix[j,k];
            a.matrix[j,k] := dum
         END;
         d := -d;
         vv.vector[imax] := vv.vector[j]
      END;
      indx.vector[j] := imax;
      IF (a.matrix[j,j] = 0.0) THEN a.matrix[j,j] := tiny;
      IF (j <> n) THEN BEGIN
         dum := 1.0/a.matrix[j,j];
         FOR i := j+1 TO n DO BEGIN
            a.matrix[i,j] := a.matrix[i,j]*dum
         END
      END
   END;
END;

 FUNCTION InvertMatrix3D  (CONST Input:TMatrix):  TMatrix;
 var
    n,i,j: integer;
    d: double;
    indx: tIntVector;
    col: tvector;
    a,y: TMatrix;
 begin
 a:= Input;
 n := 3;
  y.size := size3D;
 ludcmp(a,n,indx,d);
 for j := 1 to n do begin
     for i := 1 to n do col.vector[i] := 0;
     col.vector[j] := 1.0;
     lubksb(a,n,indx,col);
     for i := 1 to n do y.matrix[i,j] := col.vector[i];
 end;
 result := y;
 end;

  // This procedure inverts a general transformation matrix.  The user need
  // not form an inverse geometric transformation by keeping a product of
  // the inverses of simple geometric transformations:  translations, rotations
  // and scaling.  A determinant of zero indicates no inverse is possible for
  // a singular matrix.
  FUNCTION InvertMatrix  (CONST a,b:  TMatrix; VAR determinant:  DOUBLE):  TMatrix;
    VAR
      c        :  TMatrix;
      i,i_pivot:  TIndex;
      i_flag   :  ARRAY[TIndex] OF BOOLEAN;
      j,j_pivot:  TIndex;
      j_flag   :  ARRAY[TIndex] OF BOOLEAN;
      modulus  :  DOUBLE;
      n        :  TIndex;
      pivot    :  DOUBLE;
      pivot_col:  ARRAY[TIndex] OF TIndex;
      pivot_row:  ARRAY[TIndex] OF TIndex;
      temporary:  DOUBLE;
  BEGIN
    c := a;                         // The matrix inversion algorithm used here
    WITH c DO                       // is similar to the "maximum pivot strategy"
    BEGIN                           // described in "Applied Numerical Methods"
      FOR i := 1 TO size DO         // by Carnahan, Luther and Wilkes,
      BEGIN                         // pp. 282-284.
        i_flag[i] := TRUE;
        j_flag[i] := TRUE
      END;
      modulus := 1.0;
      i_pivot := 1;  // avoid initialization warning
      j_pivot := 1;  // avoid initialization warning

      FOR n := 1 TO size DO
      BEGIN
        pivot := 0.0;
        IF   ABS(modulus) > 0.0
        THEN BEGIN
          FOR i := 1 TO size DO
            IF  i_flag[i]
            THEN

              FOR j := 1 TO size DO
                IF   j_flag[j]
                THEN
                  IF   ABS(matrix[i,j]) > ABS(pivot)
                  THEN BEGIN
                    pivot := matrix[i,j];   // largest value on which to pivot
                    i_pivot := i;           // indices of pivot element
                    j_pivot := j
                  END;

          IF   Defuzz(pivot) = 0    // If pivot is too small, consider
          THEN modulus := 0         // the matrix to be singular
          ELSE BEGIN
            pivot_row[n] := i_pivot;
            pivot_col[n] := j_pivot;
            i_flag[i_pivot] := FALSE;
            j_flag[j_pivot] := FALSE;
            FOR i := 1 TO size DO
              IF   i <> i_pivot
              THEN
                FOR j := 1 TO size DO  // pivot column unchanged for elements
                  IF   j <> j_pivot    // not in pivot row or column ...
                  THEN matrix[i,j] := (matrix[i,j]*matrix[i_pivot,j_pivot] -
                                    matrix[i_pivot,j]*matrix[i,j_pivot])
                                    / modulus;  // 2x2 minor / modulus
            FOR j := 1 TO size DO
              IF   j <> j_pivot        // change signs of elements in pivot row
              THEN matrix[i_pivot,j] := -matrix[i_pivot,j];
            temporary := modulus;      // exchange pivot element and modulus
            modulus := matrix[i_pivot,j_pivot];
            matrix[i_pivot,j_pivot] := temporary
          END
        END
      END {FOR n}
    END {WITH};
    determinant := Defuzz(modulus);
    IF  determinant <> 0
    THEN BEGIN
      RESULT.size := c.size;       // The matrix inverse must be unscrambled
      FOR i := 1 TO c.size DO      // if pivoting was not along main diagonal.
        FOR j := 1 TO c.size DO
          RESULT.matrix[pivot_row[i],pivot_col[j]] := Defuzz(c.matrix[i,j]/determinant)
    END
    ELSE EMatrixError.Create('InvertMatrix error')

  END {InvertMatrix};

   FUNCTION InvertMatrix1  (CONST a:  TMatrix; VAR determinant:  DOUBLE):  TMatrix;
    VAR
      c        :  TMatrix;
      i,i_pivot:  TIndex;
      i_flag   :  ARRAY[TIndex] OF BOOLEAN;
      j,j_pivot:  TIndex;
      j_flag   :  ARRAY[TIndex] OF BOOLEAN;
      modulus  :  DOUBLE;
      n        :  TIndex;
      pivot    :  DOUBLE;
      pivot_col:  ARRAY[TIndex] OF TIndex;
      pivot_row:  ARRAY[TIndex] OF TIndex;
      temporary:  DOUBLE;
  BEGIN
    c := a;                         // The matrix inversion algorithm used here
    WITH c DO                       // is similar to the "maximum pivot strategy"
    BEGIN                           // described in "Applied Numerical Methods"
      FOR i := 1 TO size DO         // by Carnahan, Luther and Wilkes,
      BEGIN                         // pp. 282-284.
        i_flag[i] := TRUE;
        j_flag[i] := TRUE
      END;
      modulus := 1.0;
      i_pivot := 1;  // avoid initialization warning
      j_pivot := 1;  // avoid initialization warning

      FOR n := 1 TO size DO
      BEGIN
        pivot := 0.0;
        IF   ABS(modulus) > 0.0
        THEN BEGIN
          FOR i := 1 TO size DO
            IF  i_flag[i]
            THEN

              FOR j := 1 TO size DO
                IF   j_flag[j]
                THEN
                  IF   ABS(matrix[i,j]) > ABS(pivot)
                  THEN BEGIN
                    pivot := matrix[i,j];   // largest value on which to pivot
                    i_pivot := i;           // indices of pivot element
                    j_pivot := j
                  END;

          IF   Defuzz(pivot) = 0    // If pivot is too small, consider
          THEN modulus := 0         // the matrix to be singular
          ELSE BEGIN
            pivot_row[n] := i_pivot;
            pivot_col[n] := j_pivot;
            i_flag[i_pivot] := FALSE;
            j_flag[j_pivot] := FALSE;
            FOR i := 1 TO size DO
              IF   i <> i_pivot
              THEN
                FOR j := 1 TO size DO  // pivot column unchanged for elements
                  IF   j <> j_pivot    // not in pivot row or column ...
                  THEN matrix[i,j] := (matrix[i,j]*matrix[i_pivot,j_pivot] -
                                    matrix[i_pivot,j]*matrix[i,j_pivot])
                                    / modulus;  // 2x2 minor / modulus
            FOR j := 1 TO size DO
              IF   j <> j_pivot        // change signs of elements in pivot row
              THEN matrix[i_pivot,j] := -matrix[i_pivot,j];
            temporary := modulus;      // exchange pivot element and modulus
            modulus := matrix[i_pivot,j_pivot];
            matrix[i_pivot,j_pivot] := temporary
          END
        END
      END {FOR n}
    END {WITH};

    determinant := Defuzz(modulus);
    IF  determinant <> 0
    THEN BEGIN
      RESULT.size := c.size;       // The matrix inverse must be unscrambled
      FOR i := 1 TO c.size DO      // if pivoting was not along main diagonal.
        FOR j := 1 TO c.size DO
          RESULT.matrix[pivot_row[i],pivot_col[j]] := Defuzz(c.matrix[i,j]/determinant)
    END
    ELSE EMatrixError.Create('InvertMatrix error')

  END {InvertMatrix};

(*  FUNCTION InvertMatrix1  (CONST a:  TMatrix; VAR determinant:  DOUBLE):  TMatrix;
    VAR
      c        :  TMatrix;
      i,i_pivot:  TIndex;
      i_flag   :  ARRAY[TIndex] OF BOOLEAN;
      j,j_pivot:  TIndex;
      j_flag   :  ARRAY[TIndex] OF BOOLEAN;
      modulus  :  DOUBLE;
      n        :  TIndex;
      pivot    :  DOUBLE;
      pivot_col:  ARRAY[TIndex] OF TIndex;
      pivot_row:  ARRAY[TIndex] OF TIndex;
      temporary:  DOUBLE;
  BEGIN
    c := a;                         // The matrix inversion algorithm used here
    WITH c DO                       // is similar to the "maximum pivot strategy"
    BEGIN                           // described in "Applied Numerical Methods"
      FOR i := 1 TO size DO         // by Carnahan, Luther and Wilkes,
      BEGIN                         // pp. 282-284.
        i_flag[i] := TRUE;
        j_flag[i] := TRUE
      END;
      modulus := 1.0;
      i_pivot := 1;  // avoid initialization warning
      j_pivot := 1;  // avoid initialization warning

      FOR n := 1 TO size DO
      BEGIN
        pivot := 0.0;
        IF   ABS(modulus) > 0.0
        THEN BEGIN
          FOR i := 1 TO size DO
            IF  i_flag[i]
            THEN

              FOR j := 1 TO size DO
                IF   j_flag[j]
                THEN
                  IF   ABS(matrix[i,j]) > ABS(pivot)
                  THEN BEGIN
                    pivot := matrix[i,j];   // largest value on which to pivot
                    i_pivot := i;           // indices of pivot element
                    j_pivot := j
                  END;

          IF   Defuzz(pivot) = 0    // If pivot is too small, consider
          THEN modulus := 0         // the matrix to be singular
          ELSE BEGIN
            pivot_row[n] := i_pivot;
            pivot_col[n] := j_pivot;
            i_flag[i_pivot] := FALSE;
            j_flag[j_pivot] := FALSE;
            FOR i := 1 TO size DO
              IF   i <> i_pivot
              THEN
                FOR j := 1 TO size DO  // pivot column unchanged for elements
                  IF   j <> j_pivot    // not in pivot row or column ...
                  THEN matrix[i,j] := (matrix[i,j]*matrix[i_pivot,j_pivot] -
                                    matrix[i_pivot,j]*matrix[i,j_pivot])
                                    / modulus;  // 2x2 minor / modulus
            FOR j := 1 TO size DO
              IF   j <> j_pivot        // change signs of elements in pivot row
              THEN matrix[i_pivot,j] := -matrix[i_pivot,j];
            temporary := modulus;      // exchange pivot element and modulus
            modulus := matrix[i_pivot,j_pivot];
            matrix[i_pivot,j_pivot] := temporary
          END
        END
      END {FOR n}
    END {WITH};
    determinant := Defuzz(modulus);
    IF  determinant <> 0
    THEN BEGIN
      RESULT.size := c.size;       // The matrix inverse must be unscrambled
      FOR i := 1 TO c.size DO      // if pivoting was not along main diagonal.
        FOR j := 1 TO c.size DO
          RESULT.matrix[pivot_row[i],pivot_col[j]] := Defuzz(c.matrix[i,j]/determinant)
    END
    
    ELSE EMatrixError.Create('InvertMatrix error')

  END {InvertMatrix};    *)

// ***********************  Transformation Matrices  ********************


  // This procedure defines a matrix for a two- or three-dimensional rotation.
  // To avoid possible confusion in the sense of the rotation, 'rotateClockwise'
  // or 'roCounterlcockwise' must always be specified along with the axis
  // of rotation. Two-dimensional rotations are assumed to be about the z-axis
  // in the x-y plane.
  //
  // A rotation about an arbitrary axis can be performed with the following
  // steps:
  //   (1) Translate the object into a new coordinate system where (x,y,z)
  //       maps into the origin (0,0,0).
  //   (2) Perform appropriate rotations about the x and y axes of the
  //       coordinate system so that the unit vector (a,b,c) is mapped into
  //       the unit vector along the z axis.
  //   (3) Perform the desired rotation about the z-axis of the new
  //       coordinate system.
  //   (4) Apply the inverse of step (2).
  //   (5) Apply the inverse of step (1).
  FUNCTION RotateMatrix     (CONST dimension:  TDimension;
                             CONST xyz      :  TAxis;
                             CONST angle    :  DOUBLE;
                             CONST rotation :  Trotation):  TMatrix;
    VAR
      cosx     :  DOUBLE;
      sinx     :  DOUBLE;
      TempAngle:  DOUBLE;

  BEGIN
    TempAngle := angle;  // Use TempAngle since "angle" is CONST parameter

    IF  rotation = rotateCounterClockwise
    THEN TempAngle := -TempAngle;

    cosx := Defuzz( COS(TempAngle) );
    sinx := Defuzz( SIN(TempAngle) );

    CASE dimension OF
      dimen2D:
        CASE xyz OF
          axisX,axisY:  EMatrixError.Create('Invalid 2D rotation matrix.  Specify axisZ');

          axisZ:  RESULT := Matrix2D ( cosx, -sinx,     0,
                                       sinx,  cosx,     0,
                                          0,     0,     1)
        END;

      dimen3D:
        CASE xyz OF
          axisX:  RESULT := Matrix3D (    1,     0,     0, 0,
                                          0,  cosx, -sinx, 0,
                                          0,  sinx,  cosx, 0,
                                          0,     0,     0, 1);

          axisY:  RESULT := Matrix3D ( cosx,     0,  sinx, 0,
                                          0,     1,     0, 0,
                                      -sinx,     0,  cosx, 0,
                                          0,     0,     0, 1);

          axisZ:  RESULT := Matrix3D ( cosx, -sinx,     0, 0,
                                       sinx,  cosx,     0, 0,
                                          0,     0,     1, 0,
                                          0,     0,     0, 1);
        END
    END
  END {RotateMatrix};


  // 'ScaleMatrix' accepts a 'vector' containing the scaling factors for
  //  each of the dimensions and creates a scaling matrix.  The size
  //  of the vector dictates the size of the resulting matrix.
  FUNCTION ScaleMatrix      (CONST s:  TVector):  TMatrix;
  BEGIN
    CASE s.size OF
      size2D: RESULT := Matrix2D (s.x,   0,   0,
                                    0, s.y,   0,
                                    0,   0,   1);

      size3D: RESULT := Matrix3D (s.x,   0,   0,  0,
                                    0, s.y,   0,  0,
                                    0,   0, s.z,  0,
                                    0,   0,   0,  1)
    END
  END {ScaleMatrix};
  // 'TranslateMatrix' defines a translation transformation matrix.  The
  // components of the vector 't' determine the translation components.
  // (Note:  'Translate' here is from kinematics in physics.)
  FUNCTION TranslateMatrix  (CONST t:  TVector):  TMatrix;
  BEGIN
    CASE t.size OF
      size2D: RESULT := Matrix2D (  1,   0, 0,
                                    0,   1, 0,
                                  t.x, t.y, 1);

      size3D: RESULT := Matrix3D (  1,   0,   0,  0,
                                    0,   1,   0,  0,
                                    0,   0,   1,  0,
                                  t.x, t.y, t.z,  1)
    END
  END {TranslateMatrix};
  // 'ViewTransformMatrix' creates a transformation matrix for changing
  // from world coordinates to eye coordinates. The location of the 'eye'
  // from the 'object' is given in spherical (azimuth,elevation,distance)
  // coordinates or Cartesian (x,y,z) coordinates.  The size of the screen
  // is 'ScreenX' units horizontally and 'ScreenY' units vertically.  The
  // eye is 'ScreenDistance' units from the viewing screen.  A large ratio
  // 'ScreenDistance/ScreenX (or ScreenY)' specifies a narrow aperature
  // -- a telephoto view.  Conversely, a small ratio specifies a large
  // aperature -- a wide-angle view.  This view transform matrix is very
  // useful as the default three-dimensional transformation matrix.  Once
  // set, all points are automatically transformed.
  FUNCTION ViewTransformMatrix (CONST coordinate:  TCoordinate;
       CONST azimuth {or x}, elevation {or y}, distance {or z}:  DOUBLE;
       CONST ScreenX, ScreenY, ScreenDistance:  DOUBLE):  TMatrix;

    CONST
      HalfPI   =  PI / 2.0;

    VAR
      a         :  TMatrix;
      b         :  TMatrix;
      cosm      :  DOUBLE;        // COS(-angle)
      hypotenuse:  DOUBLE;
      sinm      :  DOUBLE;        // SIN(-angle)
      temporary :  DOUBLE;
      u         :  TVector;
      x         :  DOUBLE  ABSOLUTE azimuth;     // x and azimuth are synonyms
      y         :  DOUBLE  ABSOLUTE elevation;   // synonyms
      z         :  DOUBLE  ABSOLUTE distance;    // synonyms

  BEGIN
    CASE coordinate OF
      coordCartesian:  u := Vector3D (-x, -y, -z);

      coordSpherical:
        BEGIN
          temporary := -distance * COS(elevation);
          u := Vector3D (temporary * COS(azimuth - HalfPI),
                         temporary * SIN(azimuth - HalfPI),
                        -distance  * SIN(elevation));
        END
    END;
    a := TranslateMatrix(u);      // translate origin to 'eye'
    b := RotateMatrix (dimen3D, axisX, HalfPI, rotateClockwise);
    a := MultiplyMatrices(a,b);

    CASE coordinate OF
      coordCartesian:
        BEGIN
          temporary := SQR(x) + SQR(y);
          hypotenuse := SQRT(temporary);
          if hypotenuse <> 0 then begin
          cosm := -y/hypotenuse;
          sinm :=  x/hypotenuse;
          end else begin
              cosm := 1;//abba
              sinm := 0;
          end;

          b := Matrix3D ( cosm, 0, sinm, 0,
                             0, 1,    0, 0,
                         -sinm, 0, cosm, 0,
                             0, 0,    0, 1);

          a := MultiplyMatrices (a,b);
          cosm := hypotenuse;
          hypotenuse := SQRT(temporary + SQR(z));
          cosm := cosm/hypotenuse;
          sinm := -z/hypotenuse;

          b := Matrix3D (    1,    0,     0,  0,
                             0, cosm, -sinm,  0,
                             0, sinm,  cosm,  0,
                             0,    0,     0,  1)
        END;
      coordSpherical:
        BEGIN
          b := RotateMatrix (dimen3D,axisY,-azimuth,rotateCounterClockwise);
          a := MultiplyMatrices(a,b);
          b := RotateMatrix (dimen3D,axisX,elevation,rotateCounterClockwise);
        END
    END {CASE};

    a := MultiplyMatrices (a,b);
    u := Vector3D (ScreenDistance/(0.5*ScreenX),
              ScreenDistance/(0.5*ScreenY),-1.0);
    b := ScaleMatrix (u);  // reverse sense of z-axis; screen transformation

    RESULT := MultiplyMatrices (a,b);

  END {ViewTransformMatrix};

// ***************************   Conversions   **************************
  // This function converts the vector parameter from Cartesian
  // coordinates to the specified type of coordinates.
  FUNCTION FromCartesian (CONST ToCoordinate:  TCoordinate; CONST u:  TVector):  TVector;
    VAR
      phi  :  DOUBLE;
      r    :  DOUBLE;
      temp :  DOUBLE;
      theta:  DOUBLE;

  BEGIN
    IF  ToCoordinate = coordCartesian
    THEN RESULT := u
    ELSE BEGIN
      RESULT.size := u.size;

      IF   (u.size = size3D) AND
           (ToCoordinate = coordSpherical)
      THEN BEGIN                    // spherical 3D
        temp := SQR(u.x)+SQR(u.y);  // (x,y,z) -> (r,theta,phi)
        r := SQRT(temp+SQR(u.z));
        IF   Defuzz(u.x) = 0.0
        THEN theta := PI/4
        ELSE theta := ARCTAN(u.y/u.x);
        IF   Defuzz(u.z) = 0.0
        THEN phi := PI/4
        ELSE phi := ARCTAN(SQRT(temp)/u.z);
        RESULT.x := r;
        RESULT.y := theta;
        RESULT.z := phi
      END
      ELSE BEGIN              // cylindrical 2D/3D or spherical 2D
                              // (x,y) -> (r,theta)  or  (x,y,z) -> (r,theta,z)
        r := SQRT( SQR(u.x) + SQR(u.y) );
        IF   Defuzz(u.x) = 0.0
        THEN theta := PI/4
        ELSE theta := ARCTAN(u.y/u.x);
        RESULT.x := r;
        RESULT.y := theta
      END

    END
  END {FromCartesian};


  // This function converts the vector parameter from specified coordinates
  // into Cartesian coordinates.
  FUNCTION ToCartesian   (CONST FromCoordinate:  TCoordinate; CONST u:  TVector):  TVector;
    VAR
      phi   :  DOUBLE;
      r     :  DOUBLE;
      sinphi:  DOUBLE;
      theta :  DOUBLE;

  BEGIN
    RESULT := u;

    IF  FromCoordinate = coordCartesian
    THEN RESULT := u
    ELSE BEGIN
      RESULT.size := u.size;

      IF   (u.size = size3D) AND
           (FromCoordinate = coordSpherical)
      THEN BEGIN       // spherical 3D
        r :=  u.x;     //  (r,theta,phi) -> (x,y,z)
        theta := u.y;
        phi := u.z;
        sinphi := SIN(phi);
        RESULT.x := r * COS(theta) * sinphi;
        RESULT.y := r * SIN(theta) * sinphi;
        RESULT.z := r * COS(phi)
      END
      ELSE BEGIN       // cylindrical 2D/3D or spherical 2D
        r :=  u.x;     // (r,theta) -> (x,y)  or  (r,theta,z) -> (x,y,z)
        theta := u.y;
        RESULT.x := r * COS(theta);
        RESULT.y := r * SIN(theta)
      END
    END
  END {ToCartesian};

  // Convert angle in radians to degrees.
  (*FUNCTION ToDegrees(CONST angle {radians}:  DOUBLE):  DOUBLE {degrees};
  BEGIN
    RESULT := 180.0/PI * angle
  END ;//ToDegrees*)


  // Convert angle in degrees to radians.
  FUNCTION ToRadians (CONST angle:  DOUBLE):  DOUBLE;
  BEGIN
    RESULT := PI/180.0 * angle
  END; {ToRadians}


// ***************************  Miscellaneous  **************************

  // 'Defuzz' is used for comparisons and to avoid propagation of 'fuzzy',
  //  nearly-zero values.  DOUBLE calculations often result in 'fuzzy' values.
  //  The term 'fuzz' was adapted from the APL language.
 FUNCTION  Defuzz(CONST x:  DOUBLE):  DOUBLE;
  BEGIN
    IF  ABS(x) < fuzz
    THEN RESULT := 0.0
    ELSE RESULT := x
  END {Defuzz};

(*  FUNCTION  GetFuzz:   DOUBLE;
  BEGIN
    RESULT := fuzz
  END {GetFuzz};
  *)

  {PROCEDURE SetFuzz(CONST x:  DOUBLE);
  BEGIN
    fuzz := x
  END; {SetFuzz}

INITIALIZATION
 fuzz := 1.0E-6;
 //SetFuzz(1.0E-6)
END {GraphicsMath UNIT}.
