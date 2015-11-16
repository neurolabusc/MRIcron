{ **********************************************************************
  *                             MATH387.INC                            *
  **********************************************************************
                    Mathematical functions for TPMATH
       (Assembler version for 387/486/Pentium with BP7 and Delphi1)
  ********************************************************************** }


(* Bibliotheque mathematique pour utilisation du coprocesseur flottant
  JD GAYRARD Sept. 95

  ----------------------------------------------------------------------
  Unite d'origine : MATH387.PAS, disponible dans MATHLIB2.ZIP
  (http://wcarchive.cdrom.com/pub/delphi_www/)
  Convertie en fichier Include par J. DEBORD, Juin 97
  avec ajout des fonctions fexp2 et flog2
  ----------------------------------------------------------------------

 la bibliotheque est batie … partir des fonctions du coprocesseur
 du type 386, elle fournit les fonctions suivantes:
      fsin, fcos, ftan, farctan, farctan2,
      farcsin, farccos, fmod, mod_2PI,
      ften_to, fy_to_x, fexp, fexp2, fln, flog, flog2...

Aucune verification du domaine de definition des fonctions n'est faite,
pas plus qu'un controle de la validite des operandes. Il est conseille
d'utiliser cette bibliotheque pour les types single et double exclusivement *)

{ table opcode du 387 non comprise par turbo pascal V7 }
{ FSIN    : D9 FE
  FCOS    : D9 FF
  FSINCOS : D9 FB
  FPREM1  : D9 F5 }

(* use only with 80387, 80486 or pentium for type single, double and extended,
no check of definition domain of the function or range (FPU limitation).
The f prefix avoids function redefinition of system runtime library *)

function fsin(x : Float): Float; assembler;
{if x < pi.2^62, then C2 is set to 0 and ST = sin(x)
                 else C2 is set to 1 and ST = x }
{no check range validity is performed in this function}
asm
   FLD x           { load x }
   DB $D9, $FE     { opcode for FSIN }
end;

function fcos(x : Float): Float; assembler;
{ if x < pi.2^62, then C2 is set to 0 and ST = sin(x)
                  else C2 is set to 1 and ST = x }
{no range validity check is performed in this function}
asm
   FLD x         { load angle }
   DB $D9, $FF   { opcode for FCOS }
end;
(*
procedure dsincos(x : Float; var sinus, cosinus : double); assembler;
{ retourne sinus et cosinus(x), utilisable uniquement
  avec 80387, 80468 et pentium et type double  }
asm                          { ST(0)     ST(1) }
     FLD x                   {  x          -   }
     DB $D9, $FB             { cos(x)   sin(x) }
     LES DI,cosinus          {                 }
     FSTP ES:QWORD PTR [DI]  { sin(x)      -   }
     LES DI,sinus            {                 }
     FSTP ES:QWORD PTR [DI]  {  -          -   }
end;

procedure ssincos(x : Float; var sinus, cosinus : single); assembler;
{ retourne sinus et cosinus(x), utilisable uniquement
  avec 80387, 80468 et pentium et type single  }
asm                          { ST(0)     ST(1) }
     FLD x                   {  x          -   }
     DB $D9, $FB             { cos(x)   sin(x) }
     LES DI,cosinus          {                 }
     FSTP ES:DWORD PTR [DI]  { sin(x)      -   }
     LES DI,sinus            {                 }
     FSTP ES:DWORD PTR [DI]  {  -          -   }
end;

procedure fsincos(x : Float; var sinus, cosinus : Float);
{ retourne sinus et cosinus(x), utilisable uniquement
  avec 80387, 80486 et pentium }
var lcos, lsin : Float;
begin
     asm                 { ST(0)     ST(1) }
          FLD x          {  x          -   }
          DB $D9, $FB    { cos(x)   sin(x) }
          FSTP lcos      { sin(x)      -   }
          FSTP lsin      {  -          -   }
     end;
cosinus := lcos;
sinus := lsin
end;
*)
function ftan(x : Float): Float; assembler;
{ if x < pi.2^62, then C2 is set to 0 and ST = 1 and ST(1) = tan(x)
                  else C2 is set to 1 and ST = x }
{no range validity check is performed in this function}
asm              { ST(0)    ST(1) }
   FLD x         {  x        -    }
   FPTAN         {  1      tan(x) }
   FSTP ST(0)    { tan(x)    -    }
end;

function farcsin(x : Float): Float; assembler;
(* retourne l'arcsin de x *)
{  methode :                ________
   arcsin(x) = arctan( x / V 1 - x.x ) }
{no range validity check is performed in this function |x| > 1 }
asm                 { ST(0)     ST(1)     ST(2) }
   FLD X            {  x         -         -    }
   FLD ST(0)        {  x         x         -    }
   FMUL ST(0), ST   { x.x        x         -    }
   FLD1             {  1         x.x       x    }
   FSUBRP ST(1), ST { 1 - xý      x        -    }
   FSQRT            { sqrt(1-xý)  x        -    }
   FPATAN           { arcsin(x)   -        -    }
end;

function farccos(x : Float): Float; assembler;
{ retourne arccos(x)
   methode :            ________
   arcsin(x) = arctan( V 1 - x.x / x ) }
{ pas de controle de domaine de definition |x| > 1 }
asm                 { ST(0)     ST(1)     ST(2) }
   FLD X            {  x         -         -    }
   FLD ST(0)        {  x         x         -    }
   FMUL ST(0), ST   { x.x        x         -    }
   FLD1             {  1         x.x       x    }
   FSUBRP ST(1), ST { 1 - xý      x        -    }
   FSQRT            { sqrt(1-xý)  x        -    }
   FXCH             {   x         z        -    }
   FPATAN           { arccos(x)   -        -    }
end;

function farctan(x : Float): Float; assembler;
asm              { ST(0)    ST(1) }
   FLD x         {  x         -   }
   FLD1          {  1         x   }
   FPATAN        { atan(x/1)  -   }
end;

function farctan2(y, x : Float): Float; assembler;
{ retourne arctan (y / x) }
asm              { ST(0)    ST(1) }
   FLD y         {  y         -   }
   FLD x         {  x         y   }
   FPATAN        { atan(y/x)  -   }
end;
(*
function fmod(x, y : Float): Float; assembler;
{ retourne x mod y }
asm                 { ST(0)    ST(1) }
   FLD Y            {  y        -    }
   FLD X            {  x        y    }
@repeat_mod:
   FPREM            { x mod y   y    }
   FSTSW AX
   SAHF
   JP @repeat_mod
   FSTP ST(1)       { x mod y   -    }
end;

function fmod_2PI( x : Float): Float; assembler;
{ retourne x mod 2.pi }
asm                   { ST(0)    ST(1) }
   FLDPI              {  pi        -   }
   FADD ST, ST        { 2.pi       -   }
   FLD x              {  x        2.pi }
@unit_circle:
   FPREM              { x mod 2pi  2pi }
   FSTSW AX
   SAHF
   JP @unit_circle
   FSTP ST(1)         { x mod 2pi   -  }
end;
*)
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

function flog10(x : Float): Float; assembler;
{ retourne le logarithme base 10 de x, utilise
 la methode log10(x) = log10(2).log2(x) }
{ pas de verification du domaine de definition (x < 0) }
asm             {  ST(0)          ST(1)  }
   FLDLG2       { log10(2)         -     }
   FLD X        {   x           log10(2) }
   FYL2X        {log2(x).log10(2)  -     }
end;

function fexp(x : Float): Float; assembler;
{ retourne e^x, par la methode e^x = 2^(x.log2(e)) }
{ 2^z = 2^f.2^i with f = frac(z) and i = int(z) }
{ 2^f is computed with F2XM1, 2^i with FSCALE }
const round_down : word = $177F;
var control_ww : word;
asm                   { ST(0)     ST(1)     ST(2) }
   FLD X              {  x         -         -    }
   FLDL2E             { log2(e)    x         -    }
   FMULP ST(1), ST    { x.log2(e)  -         -    }
   FSTCW control_ww
   FLDCW round_down
   FLD ST(0)          {  z         z          -   }
   FRNDINT            { int(z)     z          -   }
   FLDCW control_ww
   FXCH               {  z         i          -   }
   FSUB ST, ST(1)     {  f         i          -   }
   F2XM1              { 2^f-1      i          -   }
   FLD1               {  1        2^f-1       i   }
   FADDP ST(1), ST    { 2^f        i          -   }
   FSCALE             { 2^f.2^i    i          -   }
   FSTP ST(1)         { e^x        -          -   }
end;

function fexp2(x : Float): Float; assembler;
{ retourne 2^x par la methode 2^z = 2^f.2^i }
{ with f = frac(z) and i = int(z) }
{ 2^f is computed with F2XM1, 2^i with FSCALE }
const round_down : word = $177F;
var control_ww : word;
asm                   { ST(0)     ST(1)     ST(2) }
   FLD X              {  x         -         -    }
   FSTCW control_ww
   FLDCW round_down
   FLD ST(0)          {  x         x          -   }
   FRNDINT            { int(x)     x          -   }
   FLDCW control_ww
   FXCH               {  x         i          -   }
   FSUB ST, ST(1)     {  f         i          -   }
   F2XM1              { 2^f-1      i          -   }
   FLD1               {  1        2^f-1       i   }
   FADDP ST(1), ST    { 2^f        i          -   }
   FSCALE             { 2^f.2^i    i          -   }
   FSTP ST(1)         { 2^x        -          -   }
end;

function fexp10(x : Float): Float; assembler;
{ retourne 10^x, par la methode 10^x = 2^(x.log2(10))
{ 2^z = 2^f.2^i with f = frac(z) and i = int(z)
{ 2^f is computed with F2XM1, 2^i with FSCALE }
const round_down : word = $177F;
var control_ww : word;
asm                   { ST(0)     ST(1)     ST(2) }
   FLD X              {  x         -         -    }
   FLDL2T             { log2(10)   x         -    }
   FMULP ST(1), ST    { x.log2(10) -         -    }
   FSTCW control_ww
   FLDCW round_down
   FLD ST(0)          {  z         z          -   }
   FRNDINT            { int(z)     z          -   }
   FLDCW control_ww
   FXCH               {  z         i          -   }
   FSUB ST, ST(1)     {  f         i          -   }
   F2XM1              { 2^f-1      i          -   }
   FLD1               {  1        2^f-1       i   }
   FADDP ST(1), ST    { 2^f        i          -   }
   FSCALE             { 2^f.2^i    i          -   }
   FSTP ST(1)         { 10^x       -          -   }
end;
(*
function fpower(y, x : Float): Float; assembler;
{ retourne y^x, par la methode y^x = 2^(y.log2(y))
{no range validity check is performed in this function (y > 0) }
{ 2^z = 2^f.2^i with f = frac(z) and i = int(z)
{ 2^f is computed with F2XM1, 2^i with FSCALE }
const round_down : word = $177F;
var   control_ww : word;
asm                   { ST(0)     ST(1)     ST(2) }
   FLD Y              {  y         -         -    }
   FLD X              {  x         y         -    }
   FYL2X              { x.log2(y)  -         -    }
   FSTCW control_ww
   FLDCW round_down
   FLD ST(0)          {  z         z          -   }
   FRNDINT            { int(z)     z          -   }
   FLDCW control_ww
   FXCH               {  z         i          -   }
   FSUB ST, ST(1)     {  f         i          -   }
   F2XM1              { 2^f-1      i          -   }
   FLD1               {  1        2^f-1       i   }
   FADDP ST(1), ST    { 2^f        i          -   }
   FSCALE             { 2^f.2^i    i          -   }
   FSTP ST(1)         { y^x        -          -   }
end;

function module(x, y : Float): Float; assembler;
{ retourne le module du complexe (x,y) }
asm                  { ST(0)     ST(1) }
   FLD Y             {  y         -    }
   FMUL ST(0), ST    { y.y        -    }
   FLD X             {  x         y.y  }
   FMUL ST(0), ST    { x.x        y.y  }
   FADDP ST(1), ST   { d.d        -    }
   FSQRT             {  d         -    }
end;
*)

