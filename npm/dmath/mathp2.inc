{ **********************************************************************
  *                             MATHP2.INC                             *
  **********************************************************************
                    Mathematical functions for TPMATH
             (Assembler version for Pentium II/III with FPC)
  ********************************************************************** }


{ Bibliotheque mathematique pour utilisation du coprocesseur flottant
  JD GAYRARD Sept. 95

  ----------------------------------------------------------------------
  Unite d'origine : MATH387.PAS, disponible dans MATHLIB2.ZIP
  (http://wcarchive.cdrom.com/pub/delphi_www/)
  Adapte aux pentiums II/III et complete par P. NOGARET (2000)
  ---------------------------------------------------------------------- }



{***********************************************************************}
{*              function fexp(x : Float): Float;assembler;             *}
{***********************************************************************}
{*   Fonction d‚velopp‚e … partir du document de Agner Fog             *}
{*   www.agner.org/assem                                               *}
{***********************************************************************}
{*   retourne e^x, par la methode e^x = 2^(x.log2(e))                  *}
{*   2^z = 2^f.2^i avec f = frac(z) and i = int(z)                     *}
{*   2^f is computed with F2XM1,                                       *}
{*   2^i pourrait ˆtre calcul‚ avec FSCALE mais cette instruction      *}
{*   est trŠs lente 56 micro-ops sur un pentium II                     *}
{*   pour la m‚thode utilis‚ pour calculer 2^i voir Agner Fog          *}
{***********************************************************************}
{*                  st(0)                      st(1)                   *}
{*                 log2(e)                       -                     *}
{*                   x                        log2(e)                  *}
{*                 z:=x.log2(e)                  -                     *}
{*                   z                           -                     *}
{*                z - round(z)                   -                     *}
{*             2^(z - round(z)) - 1              -                     *}
{*                   1                  2^(z - round(z)) - 1           *}
{*             2^(z - round(z))                  -                     *}
{*               temp:=2^i              2^f:=2^(z - round(z))          *}
{*                  e^x                          -                     *}
{***********************************************************************}
  function fexp(x : Float): Float;assembler;
  var
    round_z : dword;
    temp    : extended;
  asm                       
    FLDL2E
    FLD x                              
    FMULP
    FIST round_z                       
    MOV DWORD PTR [temp],  00000000H   
    MOV DWORD PTR [temp+4],80000000H   
    FISUB round_z
    MOV EAX, round_z                  
    ADD EAX, 00003FFFH
    MOV DWORD PTR [temp+8],EAX        
    F2XM1                              
    FLD1
    FADDP                              
    FLD TBYTE PTR [temp]               
    FMULP                              
  end ['eax'];


{***********************************************************************}
{*            function fexp2(x : Float): Float; assembler;             *}
{***********************************************************************}
{*   Fonction d‚velopp‚e … partir du document de Agner Fog             *}
{*   www.agner.org/assem                                               *}
{***********************************************************************}
{*   retourne 2^x par la methode 2^z = 2^f.2^i                         *}
{*   avec f = frac(z) and i = int(z)                                   *}
{*   2^f is computed with F2XM1,                                       *}
{*   2^i pourrait ˆtre calcul‚ avec FSCALE mais cette instruction      *}
{*   est trŠs lente 56 micro-ops sur un pentium II                     *}
{*   pour la m‚thode utilis‚ pour calculer 2^i voir Agner Fog          *}
{***********************************************************************}
{*                  st(0)                      st(1)                   *}
{*                   x                           -                     *}
{*                  z:=x                         -                     *}
{*                   z                           -                     *}
{*                z - round(z)                   -                     *}
{*             2^(z - round(z)) - 1              -                     *}
{*                   1                  2^(z - round(z)) - 1           *}
{*             2^(z - round(z))                  -                     *}
{*               temp:=2^i              2^f:=2^(z - round(z))          *}
{*                  e^x                          -                     *}
{***********************************************************************}
  function fexp2(x : Float): Float; assembler;
  var
    round_z : dword;
    temp    : extended;
  asm
    FLD x                   
    FIST round_z            
    MOV DWORD PTR [temp],  00000000H
    MOV DWORD PTR [temp+4],80000000H
    FISUB round_z           
    MOV EAX, round_z   { round_zmax := 16384 }
    ADD EAX, 00003FFFH
    MOV DWORD PTR [temp+8],EAX
    F2XM1                   
    FLD1                    
    FADDP                   
    FLD TBYTE PTR [temp]     
    FMULP                    
  end ['EAX'];

{***********************************************************************}
{*            function fexp10(x : Float): Float; assembler;            *}
{***********************************************************************}
{*   Fonction d‚velopp‚e … partir du document de Agner Fog             *}
{*   www.agner.org/assem                                               *}
{***********************************************************************}
{*   retourne 10^x, par la methode 10^x = 2^(x.log2(10))               *}
{*   2^z = 2^f.2^i with f = frac(z) and i = int(z)                     *}
{*   2^f is computed with F2XM1                                        *}
{*   2^i pourrait ˆtre calcul‚ avec FSCALE mais cette instruction      *}
{*   est trŠs lente 56 micro-ops sur un pentium II                     *}
{*   pour la m‚thode utilis‚ pour calculer 2^i voir Agner Fog          *}
{***********************************************************************}
{*                  st(0)                      st(1)                   *}
{*                 log2(10)                      -                     *}
{*                   x                        log2(10)                 *}
{*                 z:=x.log2(10)                 -                     *}
{*                   z                           -                     *}
{*                z - round(z)                   -                     *}
{*             2^(z - round(z)) - 1              -                     *}
{*                   1                  2^(z - round(z)) - 1           *}
{*             2^(z - round(z))                  -                     *}
{*               temp:=2^i              2^f:=2^(z - round(z))          *}
{*                  10^x                         -                     *}
{***********************************************************************}
  function fexp10(x : Float): Float; assembler;
  var
    round_z : dword;
    temp    : extended;
  asm
   FLDL2T             
   FLD X              
   FMULP              
   FIST round_z            
   MOV DWORD PTR [temp],  00000000H
   MOV DWORD PTR [temp+4],80000000H
   FISUB round_z           
   MOV EAX, round_z   
   ADD EAX, 00003FFFH
   MOV DWORD PTR [temp+8],EAX
   F2XM1                   
   FLD1
   FADDP                   
   FLD TBYTE PTR [temp]     
   FMULP                   
  end ['EAX'];

function fln(x : Float): Float; assembler;
{ retourne le logarithme naturel de x, utilise
 la methode loge(x) = loge(2).log2(x) }
{ pas de verification du domaine de definition (x < 0) }
asm             {  ST(0)          ST(1)  }
   FLDLN2       { ln(2)            -     }
   FLD X        {   x             ln(2)  }
   FYL2X        { ln(2).log2(x)    -     }
end;

function flog2(x : Float): Float; assembler;
{ retourne le logarithme de base 2 de x }
{ pas de verification du domaine de definition (x < 0) }
asm             {  ST(0)          ST(1)  }
   FLD1         {   1               -    }
   FLD X        {   x               1    }
   FYL2X        { log2(x)           -    }
end;

{***********************************************************************}
{*                function flog10(X : Float) : Float;                  *}
{***********************************************************************}
{* Compute a common (base 10) logarithm. If X is near 1.0, then we     *}
{* use the FYL2XP1 instruction instead of FYL2X. "Near" means between  *}
{* 1.0 and 1+Sqrt(2)/2. We use an approximation for Sqrt(2)/2, so we   *}
{* don't have to compute it. The exact value isn't important, since    *}
{* FYL2X works fine for values near the transition.                    *}
{***********************************************************************}
  function flog10(x : Float): Float; assembler;
  const
    HalfSqrt2p1: Extended = 1.7071;
  asm
     fldlg2          { push Log2 }
     fld X           { push X }
     fld1            { push 1.0 }
     fcomp ST(1)     { if (X < 1.0) }
     jl @@1          {    goto @@1 }
     fld HalfSqrt2p1 { push 1.707 }
     fcomp ST(1)     { if (X > 1.707) }
     jg @@1          {    goto @@1 }
     fld1            { X is small, so subtract 1.0 }
     fsubrp          { X := X - 1.0 }
     fyl2xp1         { Log10(2) * Log2(X+1) }
     jmp @@2
   @@1:              { X is not near 1.0 }
     fyl2x           { Log10(2) * Log2(X) }
   @@2:
  end;

{***********************************************************************}
{*              function fsin(X : Float) : Float;                      *}
{***********************************************************************}
{*    if x < pi.2^62, then C2 is set to 0 and ST = sin(x)              *}
{*                       else C2 is set to 1 and ST = x                *}
{*    no check range validity is performed in this function            *}
{***********************************************************************}
  function fsin(X : Float) : Float; assembler;
  asm
    FLD x
    fsin
  end;

{***********************************************************************}
{*              function fcos(X : Float) : Float;                      *}
{***********************************************************************}
  function fcos(X : Float) : Float; assembler;
  asm
    FLD x
    fcos
  end;

{***********************************************************************}
{*              function ftan(X : Float) : Float;assembler;            *}
{***********************************************************************}
  function ftan(X : Float) : Float; assembler;
  asm             { ST(0)    ST(1) }
    FLD x         {  x        -    }
    FPTAN         {  1      tan(x) }
    FSTP ST(0)    { tan(x)    -    }
  end;

{***********************************************************************}
{*                function farctan(X : Float) : Float;                 *}
{***********************************************************************}
  function farctan(x : Float): Float; assembler;
  asm              { ST(0)    ST(1) }
     FLD x         {  x         -   }
     FLD1          {  1         x   }
     FPATAN        { atan(x/1)  -   }
  end;

{***********************************************************************}
{*             function farctan2(Y, X : Float) : Float;                *}
{***********************************************************************}
function farctan2(y, x : Float): Float; assembler;
{ retourne arctan (y / x) }
asm              { ST(0)    ST(1) }
   FLD y         {  y         -   }
   FLD x         {  x         y   }
   FPATAN        { atan(y/x)  -   }
end;

{***********************************************************************}
{*                function farcsin(X : Float) : Float;                 *}
{***********************************************************************}
{* retourne l'arcsin de x                                              *}
{*  methode :                ________                                  *}
{*  arcsin(x) = arctan( x / V 1 - x.x )                                *}
{*  no range validity check is performed in this function |x| > 1      *}
{***********************************************************************}
{*               ST(0)     ST(1)     ST(2)                             *}
{*                x         -         -                                *}
{*                x         x         -                                *}
{*               x.x        x         -                                *}
{*                1        x.x        x                                *}
{*              1 - xý      x         -                                *}
{*             sqrt(1-xý)   x         -                                *}
{*             arcsin(x)    -         -                                *}
{***********************************************************************}
function farcsin(x : Float): Float; assembler;
asm                 
   FLD X            
   FLD ST(0)        
   FMUL ST(0), ST   
   FLD1             
   FSUBRP ST(1), ST 
   FSQRT            
   FPATAN           
end;

{***********************************************************************}
{*            function farccos(x : Float): Float; assembler;           *}
{***********************************************************************}
{* retourne l'arccos de x                                              *}
{*  methode :            ________                                      *}
{*  arccos(x) = arctan( V 1 - x.x / x)                                 *}
{*  pas de controle de domaine de definition |x| > 1                   *}
{***********************************************************************}
{*               ST(0)     ST(1)     ST(2)                             *}
{*                x         -         -                                *}
{*                x         x         -                                *}
{*               x.x        x         -                                *}
{*                1        x.x        x                                *}
{*              1 - xý      x         -                                *}
{*             sqrt(1-xý)   x         -                                *}
{*                x         z         -                                *}
{*             arccos(x)    -         -                                *}
{***********************************************************************}
function farccos(x : Float): Float; assembler;
asm                 
   FLD X            
   FLD ST(0)        
   FMUL ST(0), ST   
   FLD1             
   FSUBRP ST(1), ST 
   FSQRT
   FXCH            
   FPATAN          
end;

{***********************************************************************}
{*            function fsinh(X : Float) : Float;                       *}
{***********************************************************************}
{*      retourne le sinus hyperbolique de l'argument                   *}
{*      sh(x) = [exp(x) - exp(-x)] / 2                                 *}
{*      methode : z = exp(x), ch(x) = 1/2 (z - 1/z)                    *}
{*          z = 2^y, y = x.log2(e),                                    *}
{*          z = 2^f.2^i, f = frac(y), i = int(y)                       *}
{*      2^f est calcul‚ avec F2XM1, 2^i sans FSCALE                    *}
{***********************************************************************}
{*            ST(0)                      ST(1)                 ST(2)   *}
{*           log2(e)                       -                     -     *}
{*             x                        log2(e)                  -     *}
{*           z:=x.log2(e)                  -                     -     *}
{*             z                           -                     -     *}
{*          z - round(z)                   -                     -     *}
{*       2^(z - round(z)) - 1              -                     -     *}
{*             1                  2^(z - round(z)) - 1           -     *}
{*       2^(z - round(z))                  -                     -     *}
{*         temp:=2^i              2^f:=2^(z - round(z))          -     *}
{*            e^x                          -                     -     *}
{*            e^x                         e^x                    -     *}
{*             1                           z                     z     *}
{*            1/z                          z                     -     *}
{*           z-1/z                         -                     -     *}
{*            0.5                        z-1/z                   -     *}
{*           sh(x)                         -                     -     *}
{***********************************************************************}
function fsinh(x : float): float; assembler;
const
  one_half : float = 0.5;
var
  round_z : dword;
  temp    : extended;
asm
  FLDL2E                  
  FLD x                   
  FMULP                   
  FIST round_z            
  MOV DWORD PTR [temp],  00000000H
  MOV DWORD PTR [temp+4],80000000H
  FISUB round_z           
  MOV EAX, round_z   
  ADD EAX, 00003FFFH
  MOV DWORD PTR [temp+8],EAX
  F2XM1                   
  FLD1                    
  FADDP                   
  FLD TBYTE PTR [temp]     
  FMULP                   
  FST ST(1)          
  FLD1               
  FDIVRP ST(1), ST   
  FSUBP ST(1), ST
  FLD one_half       
  FMULP ST(1), ST    
end;

{***********************************************************************}
{*                  function fcosh(X : Float) : Float;                 *}
{***********************************************************************}
{*      retourne le cosinus hyperbolique de l'argument                 *}
{*      ch(x) = [exp(x) + exp(-x)] / 2                                 *}
{*      methode : z = exp(x), ch(x) = 1/2 (z + 1/z)                    *}
{*          z = 2^y, y = x.log2(e),                                    *}
{*          z = 2^f.2^i, f = frac(y), i = int(y)                       *}
{*      2^f est calcul‚ avec F2XM1, 2^i sans FSCALE                    *}
{***********************************************************************}
{*         st(0)                      st(1)               st(2)        *}
{*        log2(e)                       -                              *}
{*          x                        log2(e)                           *}
{*        z:=x.log2(e)                  -                              *}
{*          z                           -                              *}
{*       z - round(z)                   -                              *}
{*    2^(z - round(z)) - 1              -                              *}
{*          1                  2^(z - round(z)) - 1                    *}
{*    2^(z - round(z))                  -                              *}
{*     temp:=2^i              2^f:=2^(z - round(z))                    *}
{*         e^x                          -                              *}
{*         e^x                         e^x                 -           *}
{*          1                           z                  z           *}
{*         1/z                          z                  -           *}
{*        z+1/z                         -                  -           *}
{*         0.5                        z+1/z                -           *}
{*        ch(x)                         -                  -           *}
{***********************************************************************}
function fcosh(x : float): float; assembler;
const
  one_half : float = 0.5;
var
  round_z : dword;
  temp    : extended;
asm
  FLDL2E                  
  FLD x                   
  FMULP                   
  FIST round_z            
  MOV DWORD PTR [temp],  00000000H
  MOV DWORD PTR [temp+4],80000000H
  FISUB round_z           
  MOV EAX, round_z   
  ADD EAX, 00003FFFH
  MOV DWORD PTR [temp+8],EAX
  F2XM1                   
  FLD1                    
  FADDP                   
  FLD TBYTE PTR [temp]     
  FMULP                   
  FST ST(1)          
  FLD1               
  FDIVRP ST(1), ST
  FADDP ST(1), ST
  FLD one_half       
  FMULP ST(1), ST    
end;

{***********************************************************************}
{*               function ftanh(X : Float) : Float;                    *}
{***********************************************************************}
{*      retourne la tangente hyperbolique de l'argument                *}
{*      th(x) = sh(x) / ch(x) *)                                       *}
{*      th(x) = [exp(x) - exp(-x)] / [exp(x) + exp(-x)]                *}
{*      methode : z = exp(x), ch(x) = (z - 1/z) / (z + 1/z)            *}
{*          z = 2^y, y = x.log2(e),                                    *}
{*          z = 2^f.2^i, f = frac(y), i = int(y)                       *}
{*      2^f est calcul‚ avec F2XM1, 2^i sans FSCALE                    *}
{***********************************************************************}
{*         st(0)                      st(1)               st(2)        *}
{*        log2(e)                       -                              *}
{*          x                        log2(e)                           *}
{*        z:=x.log2(e)                  -                              *}
{*          z                           -                              *}
{*       z - round(z)                   -                              *}
{*    2^(z - round(z)) - 1              -                              *}
{*          1                  2^(z - round(z)) - 1                    *}
{*    2^(z - round(z))                  -                              *}
{*      temp:=2^i              2^f:=2^(z - round(z))                   *}
{*         e^x                          -                              *}
{*         e^x                         e^x                -            *}
{*          1                           z                 z            *}
{*         1/z                          z                 z            *}
{*         1/z                          z               z-1/z          *}
{*        z+1/z                       z-1/z               -            *}
{*        th(x)                         -                 -            *}
{***********************************************************************}
function ftanh(x : float): float; assembler;
const
  one_half : float = 0.5;
var
  round_z : dword;
  temp    : extended;
asm                       
  FLDL2E                  
  FLD x                   
  FMULP                   
  FIST round_z            
  MOV DWORD PTR [temp],  00000000H
  MOV DWORD PTR [temp+4],80000000H
  FISUB round_z           
  MOV EAX, round_z   
  ADD EAX, 00003FFFH
  MOV DWORD PTR [temp+8],EAX
  F2XM1                   
  FLD1                    
  FADDP                   
  FLD TBYTE PTR [temp]     
  FMULP                   
  FST ST(1)          
  FLD1               
  FDIV  ST, ST(1)
  FSUB  ST(2), ST    
  FADDP ST(1), ST    
  FDIVP ST(1), ST    
end;

{***********************************************************************}
{*              function farcsinh(X : Float) : Float;                  *}
{***********************************************************************}
{* retourne l'arc sinus hyperbolique de l'argument                     *}
{*                       _________                                     *}
{* arg sh(x) = ln ( x + V x.x + 1 )                                    *}
{***********************************************************************}
{*       ST(0)            ST(1)             ST(2)          ST(3)       *}
{*       ln(2)              -                 -              -         *}
{*         x              ln(2)               -              -         *}
{*         x                x                ln(2)           -         *}
{*        x.x               x                ln(2)           -         *}
{*         1               x.x                x             ln(2)      *}
{*      x.x + 1             x                ln(2)           -         *}
{*      sqrt(x.x+1)         x                ln(2)           -         *}
{*       x + z            ln(2)               -              -         *}
{*      arg_sh(x)           -                 -              -         *}
{***********************************************************************}
function farcsinh(x : float): float; assembler;
asm                 
   FLDLN2           
   FLD X            
   FLD ST(0)        
   FMUL ST(0), ST   
   FLD1             
   FADDP ST(1), ST  
   FSQRT            
   FADDP ST(1), ST  
   FYL2X            
end;

{***********************************************************************}
{*             function farccosh(X : Float) : Float;                   *}
{***********************************************************************}
{* retourne l'arc cosinus hyperbolique de l'argument                   *}
{*                       ________                                      *}
{* arg ch(x) = ln ( x + V x.x - 1 )  x >=1                             *}
{***********************************************************************}
{*        ST(0)            ST(1)            ST(2)         ST(3)        *}
{*        ln(2)             -                -             -           *}
{*         x               ln(2)             -             -           *}
{*         x                x               ln(2)          -           *}
{*        x.x               x               ln(2)          -           *}
{*         1               x.x               x            ln(2)        *}
{*       x.x - 1            x               ln(2)          -           *}
{*      sqrt(x2-1)          x               ln(2)          -           *}
{*       x + z             ln(2)             -             -           *}
{*      arg_ch(x)           -                -             -           *}
{***********************************************************************}
function farccosh(x : float): float; assembler;
asm                 
   FLDLN2
   FLD X            
   FLD ST(0)        
   FMUL ST(0), ST  
   FLD1            
   FSUBP ST(1), ST  
   FSQRT            
   FADDP ST(1), ST  
   FYL2X            
end;

{***********************************************************************}
{*            function farctanh(X : Float) : Float;                    *}
{***********************************************************************}
{*      retourne l'arc tangente hyperbolique de l'argument             *}
{*      arg th(x) = 1/2 ln [ (1 + x) / (1 - x) ]                       *}
{***********************************************************************}
{*      ST(0)             ST(1)             ST(2)          ST(3)       *}
{*      ln(2)              -                  -              -         *}
{*       x                ln(2)               -              -         *}
{*       x                 x                ln(2)            -         *}
{*       1                 x                  x            ln(2)       *}
{*       1                 x                1 + x          ln(2)       *}
{*     1 - x             1 + x              ln(2)            -         *}
{*    1+x/1-x             ln(2)               -              -         *}
{*     ln(z)               -                  -              -         *}
{***********************************************************************}
function farctanh(x : float): float; assembler;
asm                 
   FLDLN2                
   FLD X                 
   FLD ST(0)             
   FLD1
   FADD ST(2),ST
   FSUBRP ST(1),ST
   FDIVP ST(1),ST
   FYL2X
end;
