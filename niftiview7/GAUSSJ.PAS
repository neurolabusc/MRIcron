PROCEDURE gaussj(VAR a: glnpbynp; n,np: integer;
       VAR b: glnpbymp; m,mp: integer);
(* Programs using GAUSSJ must define the types
TYPE
   glnpbynp = ARRAY [1..np,1..np] OF real;
   glnpbymp = ARRAY [1..np,1..mp] OF real;
   glnp = ARRAY [1..np] OF integer;
in the main routine. *)
VAR
   big,dum,pivinv: real;
   i,icol,irow,j,k,l,ll: integer;
   indxc,indxr,ipiv: glnp;
BEGIN
   FOR j := 1 TO n DO BEGIN
      ipiv[j] := 0
   END;
   FOR i := 1 TO n DO BEGIN
      big := 0.0;
      FOR j := 1 TO n DO BEGIN
         IF (ipiv[j] <> 1) THEN BEGIN
            FOR k := 1 TO n DO BEGIN
               IF (ipiv[k] = 0) THEN BEGIN
                  IF (abs(a[j,k]) >= big) THEN BEGIN
                     big := abs(a[j,k]);
                     irow := j;
                     icol := k
                  END
               END ELSE IF (ipiv[k] > 1) THEN BEGIN
                  writeln('pause 1 in GAUSSJ - singular matrix'); readln
               END
            END
         END
      END;
      ipiv[icol] := ipiv[icol]+1;
      IF (irow <> icol) THEN BEGIN
         FOR l := 1 TO n DO BEGIN
            dum := a[irow,l];
            a[irow,l] := a[icol,l];
            a[icol,l] := dum
         END;
         FOR l := 1 TO m DO BEGIN
            dum := b[irow,l];
            b[irow,l] := b[icol,l];
            b[icol,l] := dum
         END
      END;
      indxr[i] := irow;
      indxc[i] := icol;
      IF (a[icol,icol] = 0.0) THEN BEGIN
         writeln('pause 2 in GAUSSJ - singular matrix'); readln
      END;
      pivinv := 1.0/a[icol,icol];
      a[icol,icol] := 1.0;
      FOR l := 1 TO n DO BEGIN
         a[icol,l] := a[icol,l]*pivinv
      END;
      FOR l := 1 TO m DO BEGIN
         b[icol,l] := b[icol,l]*pivinv
      END;
      FOR ll := 1 TO n DO BEGIN
         IF (ll <> icol) THEN BEGIN
            dum := a[ll,icol];
            a[ll,icol] := 0.0;
            FOR l := 1 TO n DO BEGIN
               a[ll,l] := a[ll,l]-a[icol,l]*dum
            END;
            FOR l := 1 TO m DO BEGIN
               b[ll,l] := b[ll,l]-b[icol,l]*dum
            END
         END
      END
   END;
   FOR l := n DOWNTO 1 DO BEGIN
      IF (indxr[l] <> indxc[l]) THEN BEGIN
         FOR k := 1 TO n DO BEGIN
            dum := a[k,indxr[l]];
            a[k,indxr[l]] := a[k,indxc[l]];
            a[k,indxc[l]] := dum
         END
      END
   END
END;
