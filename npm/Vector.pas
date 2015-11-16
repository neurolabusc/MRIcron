unit Vector;
{$Include ..\common\isgui.inc}
interface

uses SysUtils;


//var gMat: boolean = false;

type
     EVectorSizeError = class (Exception);
          TMatElement = double; //extended;
     { The 1000 in the array types below does not impose a limit at runtime!
     If you compile with range checking on then the compiled code will impose
     an effective limit of 1000, but with range checking off the size of
     vector is limited to 64K under 16bit OS or *much* greater under 32bit OS }
     TArrayd = array[1..1000] of TMatElement;  pTArrayd = ^TArrayd;
     TArrayi = array[1..1000] of integer; pTArrayi = ^TArrayi;

     { Define a dynamic array type for holding integers }
     TVectori = class (TObject)
                 private
                    s  : integer;   { size of vector }
                    vx : pTArrayi; { pointer to the data }
                 private
                   procedure   SetSize (NewSize : integer);
                 public
                   constructor create (i : integer); virtual;
                   destructor  destroy; override;
                   procedure   EnlargeBy (n : integer);
                   procedure   ReduceBy (n : integer);
                   procedure   Enlarge;
                   procedure   Reduce;
                   procedure   Zero;
                   procedure   Clear;
                   procedure   Assign (v : TVectori);
                   procedure   Setval (i : integer; v : integer);
                   function    Getval (i : integer) : integer;
                   function    GetSize : integer;
                   property    Elem[x : Integer] : integer read GetVal write SetVal; default;
                   property    Size : integer read s;
                end;


     { Define a dynamic array type for holding Extendeds }
     TVector = class (TObject)
                 private
                    s  : integer;   { size of vector }
                    vx : pTArrayd;  { pointer to the data }
                    Tmp : boolean;  { set to true if temporary }
                 public
                   { Declare as a class method, saves having a self variable }
                   class function Dot (u, v : TVector) : TMatElement;

                   constructor create (i : integer); virtual;
                   constructor createTmp (i : integer);
                   destructor  destroy; override;
                   procedure   FreeSpace;
                   procedure   SetSize (i : integer);
                   procedure   EnlargeBy (n : integer);
                   procedure   ReduceBy (n : integer);
                   procedure   Enlarge;
                   procedure   Reduce;
                   procedure   Zero;
                   procedure   Clear;
                   procedure   Setval (i : integer; v : TMatElement);
                   function    Getval (i : integer) : TMatElement;
                   property    Elem[x : Integer] : TMatElement read GetVal write SetVal; default;
                   property    Size : integer read s;
                   procedure   Assign (v : TVector);
                   function    Add (v, u : TVector) : TVector;
                   function    Sub (v, u : TVector) : TVector;
                   class function xAdd (v, u : TVector) : TVector;
                   class function xSub (v, u : TVector) : TVector;
                   function    DotU (v : TVector) : TMatElement;
                   function    CrossU (v : TVector) : TVector;
                   function    Cross (v1, v2 : TVector) : TVector;
                   function    Sum  : TMatElement;
                   function    Mean : TMatElement;
                   function    SumofSquares : TMatElement;
                   function    Norm : TMatElement;
                   function    StdDev : TMatElement;
                   procedure   Scale (factor : TMatElement);
                end;


implementation


// -------------------------------------------------------------------------
//                         START OF VECTOR TYPE IMPLEMETATION
// -------------------------------------------------------------------------


{ The data space which holds the data for a vector is typed as [1..x] so that
indexing autmatically starts at one, therefore there is no need in the
following code to add 1 to the size of the vector when creating or destroying it }

{ Create a vector of size i }
constructor TVector.create(i : integer);
begin
  Inherited Create;
  s := 0; vx := Nil;  { vx set to Nil to indicate empty vector, used by SetSize }
  if i > 0 then Self.SetSize (i);
end;

constructor TVector.createTmp (i : integer);
begin
  Inherited Create;
  s := 0; vx := Nil;  { vx set to Nil to indicate empty vector, used by SetSize }
  if i > 0 then Self.SetSize (i);
  Tmp := true;
end;


destructor TVector.destroy;
begin
  FreeSpace;
  Inherited Destroy;
end;


{ Private internal procedure }
procedure TVector.FreeSpace;
begin
  if vx <> Nil then FreeMem (vx, sizeof (TMatElement) * s); vx := Nil; s := 0;
end;


{ Internal routine to allocate space. If space already exists then it frees it first }
procedure TVector.SetSize (i : integer);
begin
  if vx <> Nil then FreeMem (vx, sizeof (TMatElement) * s);
  s := i; vx := AllocMem (sizeof (TMatElement) * s);
  //if gMat then beep;
end;



{ Increase the size of the vector without destroying and existing data }
procedure TVector.EnLargeBy (n : integer);
begin
  if n < 0 then raise EVectorSizeError.Create ('Argument to EnLargeBy must be positive');
  ReAllocMem (vx, sizeof (TMatElement)*(s+n)); inc (s,n); { Modified for D2 }
end;


{ Reduce the size of the vector }
procedure TVector.ReduceBy (n : integer);
begin
  if n >= s then
     raise EVectorSizeError.Create ('Can''t reduce size of vector to below zero elements');
  ReAllocMem (vx, sizeof (TMatElement)*(s-n)); dec (s,n); { modified for D2 }
end;


{ Enlarge the vector by one element without destroying any existing data }
procedure TVector.Enlarge;
begin
  ReAllocMem (vx, sizeof (TMatElement)*(s+1)); inc (s); { Modified for D2 }
end;


{ Reduce the vector by one element, the top most element is destroyed }
procedure TVector.Reduce;
begin
  ReAllocMem (vx, sizeof (TMatElement)*(s-1)); dec (s); { Modified for D2 }
end;


{ Clears the vector, sets all elements to zero }
procedure TVector.Zero;
var i : integer;
begin
  for i := 1 to s do vx^[i] := 0.0;
end;


{ Clears the vector, sets all elements to zero }
procedure TVector.Clear;
begin
  Zero;
end;



{ used internally but is also accessible from the outside }
procedure TVector.Setval (i : integer; v : TMatElement);
begin
  vx^[i] := v;
end;


{ used internally but is also accessible from the outside }
function  TVector.Getval (i : integer) : TMatElement;
begin
  result := vx^[i];
end;


// -------------------------------------------------------------------------
// Copies vector v, including contects to self. If self is not the same
// size as v then self is resized

//  Copy v to u:
//  Usage: u.Assign (v)
// -------------------------------------------------------------------------
procedure TVector.Assign (v : TVector);
begin
  v.Tmp := False;  { just in case its a temporary variable }
  if v.s <> Self.s then Self.SetSize (v.s);
  move (v.vx^, Self.vx^, sizeof(TMatElement) * s)
end;


// -------------------------------------------------------------------------
// Add the vectors, 'v' and 'u' together to produce Self. Error if v and u are
// the the same size. If Self is not sized correctly, then Add will resize Self

// Usage: w.Add (u, v)
// Add u to v giving result w
// -------------------------------------------------------------------------

function TVector.Add (v, u : TVector) : TVector;
var i : integer;
begin
  if v.s <> u.s then
     raise EVectorSizeError.Create ('Vectors must be the same size to sum them');
  if Self.s <> v.s then Self.SetSize (v.s);
  for i := 1 to v.s do Self[i] := v[i] + u[i];
  if v.tmp then v.free; if u.tmp then u.free;
  result := Self;
end;


// -------------------------------------------------------------------------
// Add the vectors, 'v' and 'u' together and RETURN the result. An Error
// occurs if v and u are the the same size. xAdd returns the result to the
// caller therefore it is the responsibility of the caller to dispose of the
// memory allocated by xSub. Note, the variable which is used to store the
// returned result must not have been previously allocated, otherwise you'll
// get memory leak!

// w must be unallocated
// Usage: w := Add (u, v)
// Add u to v giving result w
// -------------------------------------------------------------------------

class function TVector.xAdd (v, u : TVector) : TVector;
var i : integer; t : TVector;
begin
  if v.s <> u.s then
     raise EVectorSizeError.Create ('Vectors must be the same size to sum them');
  t := TVector.CreateTmp (v.s);
  for i := 1 to v.s do t[i] := v[i] + u[i];
  result := t;
end;


// -------------------------------------------------------------------------
// Subtract the vectors, 'v' and 'u' together to produce Self. Error if v and u are
// the the same size. If Self is not sized correctly, then Add will resize Self

// Usage: w.Sub (u, v)
// Add u to v giving result w
// -------------------------------------------------------------------------

function TVector.Sub (v, u : TVector) : TVector;
var i : integer;
begin
  if v.s <> u.s then
     raise EVectorSizeError.Create ('Vectors must be the same size to subtract them');
  if Self.s <> v.s then Self.SetSize (v.s);
  for i := 1 to v.s do Self[i] := v[i] - u[i];
  if v.tmp then v.free; if u.tmp then u.free;
  result := Self;
end;


// -------------------------------------------------------------------------
// Subtract the vectors, 'v' and 'u' together and RETURN the result. An Error
// occurs if v and u are the the same size. xSub returns the result to the
// caller therefore it is the responsibility of the caller to dispose of the
// memory allocated by xSub. Note, the variable which is used to store the
// returned result must not have been previously allocated, otherwise you'll
// get memory leak!

// w must be unallocated
// Usage: w := Sub (u, v)
// Add u to v giving result w
// -------------------------------------------------------------------------


class function TVector.xSub (v, u : TVector) : TVector;
var i : integer; t : TVector;
begin
  if v.s <> u.s then
     raise EVectorSizeError.Create ('Vectors must be the same size to subtract them');
  t := TVector.CreateTmp (v.s);
  for i := 1 to v.s do t[i] := v[i] - u[i];
  result := t;
end;


// -------------------------------------------------------------------------
// Compute the dot product of vectors 'u' and 'v'
// Usage: d := dot (u, v);
// -------------------------------------------------------------------------
class function TVector.Dot (u, v : TVector) : TMatElement;
var i : integer;
begin
  if u.Size <> v.Size then
     raise EVectorSizeError.Create ('Vectors must be of the same size to compute dot product');

  result := 0.0;
  for i := 1 to u.Size do result := result + u[i]*v[i];
end;


// -------------------------------------------------------------------------
// Apply a dot product to Self and argument, 'v'
// Usage: d := u.dotU (v);
// -------------------------------------------------------------------------
function TVector.DotU (v : TVector) : TMatElement;
var i : integer;
begin
  if Self.Size <> v.Size then
     raise EVectorSizeError.Create ('Vectors must be of the same size to compute dot product');

  result := 0.0;
  for i := 1 to Self.Size do
      result := result + Self[i]*v[i];
end;


// -------------------------------------------------------------------------
// Compute the cross product of Self and vector 'v', replacing Self
// Usage: v.CrossU (u)
// -------------------------------------------------------------------------
function TVector.CrossU (v : TVector) : TVector;
begin
  if (v.Size = 3) and (Self.Size = 3) then
     begin
     Self[1] := Self[2]*v[3] - Self[3]*v[2];
     Self[2] := Self[3]*v[1] - Self[1]*v[3];
     Self[3] := Self[1]*v[2] - Self[2]*v[1];
     result := Self;
     end
  else
     raise EVectorSizeError.Create ('Cross product can only be calculated for vectors in 3D');
end;


// -------------------------------------------------------------------------
// Compute the cross product of 'v1' and vector 'v2' giving Self
// Usage: v.Cross (v1, v2)
// -------------------------------------------------------------------------
function TVector.Cross (v1, v2 : TVector) : TVector;
begin
  if (v1.Size = 3) and (v2.Size = 3) and (Self.Size = 3) then
     begin
     Self[1] := v1[2]*v2[3] - v1[3]*v2[2];
     Self[2] := v1[3]*v2[1] - v1[1]*v2[3];
     Self[3] := v1[1]*v2[2] - v1[2]*v2[1];
     result := Self;
     end
  else
     raise EVectorSizeError.Create ('Cross product can only be calculated for vectors in 3D');
end;


// -------------------------------------------------------------------------
// Returns the sum of values in the vector
// Usage: total := v.sum
// -------------------------------------------------------------------------
function TVector.Sum : TMatElement;
var i : integer;
begin
  result := 0.0;
  for i := 1 to s do result := result + vx^[i];
end;

// -------------------------------------------------------------------------
// Returns the mean of the elements of the vector
// Usage: average := v.mean;
// -------------------------------------------------------------------------
function TVector.Mean : TMatElement;
begin
  if s > 0 then result := sum / s
  else raise Exception.Create ('Vector must have at least one element to compute mean');
end;


// -------------------------------------------------------------------------
// Returns the sum of the squares of values in Data
// Usage: s := v.SumOfSquares;
// -------------------------------------------------------------------------
function TVector.SumOfSquares : TMatElement;
var i : integer;
begin
  result := 0.0;
  for i := 1 to s do result := result + sqr(vx^[i]);
end;


// -------------------------------------------------------------------------
// Returns the Euclidean norm of the Self vector
// -------------------------------------------------------------------------
function TVector.Norm : TMatElement;
begin
  result := sqrt (Self.SumOfSquares);
end;


// -------------------------------------------------------------------------
// Returns the sample standard deviation
// Usage: sd := v.StdDev;
// -------------------------------------------------------------------------
function TVector.StdDev : TMatElement;
var sq, total : TMatElement; i : integer;
begin
  sq := 0; total := 0;
  if s > 1 then
     begin
     for i := 1 to s do
         begin sq := sq + sqr(vx^[i]); total := total + vx^[i]; end;
     result := sqrt ((sq - sqr(total)/s)/(s-1));
     // The following code is easier to read but slightly slower in execution:
     // result := sqrt ((SumOfSquares - sqr (sum)/s)/(s-1));}
     end
  else
     raise Exception.Create ('Can''t calculate stddev for vector with one or no elements');
end;


// -------------------------------------------------------------------------
// Scale the vector by factor
// Usage: v.Scale (2)   Multiplies all elements by 2
// -------------------------------------------------------------------------
procedure TVector.Scale (factor : TMatElement);
var i : integer;
begin
  for i := 1 to s do vx^[i] := vx^[i]*factor;
end;


{ ------------------------------------------------------------------------- }
{                         START OF INTEGER VECTOR IMPLEMETATION             }
{ ------------------------------------------------------------------------- }


{ Create a vector of size i }
constructor TVectori.create(i : integer);
begin
  Inherited Create; vx := Nil;
  Self.SetSize (i);
end;


destructor TVectori.destroy;
begin
  if vx <> Nil then FreeMem (vx, sizeof (integer) * s);
  Inherited Destroy;
end;


{ Internal routine used by define }
procedure TVectori.SetSize (NewSize : integer);
begin
  if vx <> Nil then FreeMem (vx, sizeof (integer) * s);
  s := NewSize; vx := AllocMem (sizeof (integer) * NewSize);
end;

procedure TVectori.EnLargeBy (n : integer);
begin
  ReAllocMem (vx, sizeof (integer)*(s+n)); inc (s,n); { Modified for D2 }
end;


procedure TVectori.ReduceBy (n : integer);
begin
  if n >= s then
     raise EVectorSizeError.Create ('Can''t reduce size of vector to below zero elements');
  ReAllocMem (vx, sizeof (integer)*(s-n)); dec (s,n);  { Modified for D2 }
end;


{ Enlarge the vector by one element without destroying any existing data }
procedure TVectori.Enlarge;
begin
  ReAllocMem (vx, sizeof (integer)*(s+1)); inc (s); { Modified for D2 }
end;


{ Reduce the vector by one element, the top most element is destroyed }
procedure TVectori.Reduce;
begin
  ReAllocMem (vx, sizeof (integer)*(s-1)); dec (s); { Modified for D2 }
end;


{ Clear the vector, sets all elements to zero }
procedure TVectori.Zero;
var i : integer;
begin
  for i := 1 to s do vx^[i] := 0;
end;


{ Clear the vector, sets all elements to zero }
procedure TVectori.Clear;
begin
  Zero;
end;


procedure TVectori.Assign (v : TVectori);
begin
  if v.s <> Self.s then Self.SetSize (v.s);
  move (v.vx^, Self.vx^, sizeof(integer) * s)
end;


{ used internally but is also accessible from the outside }
procedure TVectori.Setval (i : integer; v : integer);
begin
  vx^[i] := v;
end;


{ used internally but is also accessible from the outside }
function  TVectori.Getval (i : integer) : integer;
begin
  result := vx^[i];
end;


function TVectori.GetSize : integer;
begin
  result := s;
end;


end.