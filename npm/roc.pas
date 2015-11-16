unit roc;
interface
//demonstrates the ROC tests that are in the Brunner.pas file
uses
  define_types,SysUtils,
part,StatThds,statcr,StatThdsUtil,Brunner,//Brunner,nifti_img, DISTR
   Messages,  Classes, Graphics, Controls, Forms, Dialogsx,
StdCtrls,  ComCtrls,ExtCtrls,Menus, //overlap,ReadInt,stats,LesionStatThds,nifti_hdr,

{$IFDEF FPC} LResources,gzio2,
{$ELSE} gziod,associate,{$ENDIF}   //must be in search path, e.g. C:\pas\mricron\npm\math
{$IFNDEF UNIX} Windows, {$ENDIF}
upower,IniFiles,cpucount,userdir,math,
regmult,utypes;
procedure testROC;
procedure testROC2;
function AUCbinomcont (lBinomdataRA,lContdataRA: singlep; lnSubj :integer): double;
function AUCcontcont (ldatara1,ldatara2: singlep; lnSubj :integer): double;
implementation

uses npmform;

function readCSV2 (lFilename: string; lCol1,lCol2: integer;  var lnObservations : integer; var ldataRA1,ldataRA2: singlep): boolean;
const
     kHdrRow = 0;//1;
     kHdrCol = 0;//1;
var
   lNumStr: string;
   F: TextFile;
   lTempFloat: double;
   lCh: char;
   lnFactors,MaxC,R,C:integer;
   lError: boolean;

begin
     lError := false;
     result := false;
	 if not fileexists(lFilename) then begin
            ShowMsg('Can not find '+lFilename);
            exit;
         end;
	 AssignFile(F, lFilename);
	 FileMode := 0;  //Set file access to read only
	 //First pass: determine column height/width
	 Reset(F);
	 C := 0;
	 MaxC := 0;
	 R := 0;
	 while not Eof(F) do begin
		//read next line
		//read next line
		Read(F, lCh);
		if lCh = '#' then
		   while not (lCh in [#10,#13]) do
			   Read(F, lCh)
		else if not (lCh in [#10,#13,#9,',']) then begin
		   lNumStr := lNumStr + lCh;
		end else if lNumStr <> '' then begin
			lNumStr := '';
			inc(C);
			if C > MaxC then
				MaxC := C;
			if (lCh in [#10,#13]) then begin
				C := 0;
			   inc(R);

			end; //eoln
		end; //if lNumStr <> '' and not tab
	 end;
	 if lNumStr <> '' then  //july06- read data immediately prior to EOF
        inc(R);

     if (R <= (kHdrRow+1)) or (MaxC < (kHdrCol+lCol1)) or (MaxC < (kHdrCol+lCol2)) then begin
         ShowMsg('problems reading CSV - not enough columns/rows '+inttostr(lCol1)+'  '+inttostr(lCol2));
         exit;
     end;

     lnObservations := R -kHdrRow ; //-1: first row is header....
     lnFactors := MaxC-1;// -1: first column is Y values
     //fx(lnObservations,lnFactors);

     //exit;
     getmem(ldataRA1,lnObservations*sizeof(single));
     getmem(ldataRA2,lnObservations*sizeof(single));

     //second pass
	 Reset(F);
	 C := 1;
	 MaxC := 0;
	 R := 1;
	 lNumStr := '';
     lTempfloat := 0;
	 while not Eof(F) do begin
		//read next line
		Read(F, lCh);
		if lCh = '#' then
		   while not (lCh in [#10,#13]) do
			   Read(F, lCh)
		else if not (lCh in [#10,#13,#9,',']) then begin
		   lNumStr := lNumStr + lCh;
		end else if lNumStr <> '' then begin
            if (R > kHdrRow) and (C > kHdrCol) then begin
             if ((C-kHdrCol) = lCol1) or ((C-kHdrCol) = lCol2) then begin
               if lNumStr = '-' then begin
                  lTempFloat := 0;
               end else begin //number
                try
                   lTempFloat := strtofloat(lNumStr);
                except
                                    on EConvertError do begin
                                       if not lError then
                                          ShowMsg('Empty cells? Error reading CSV file row:'+inttostr(R)+' col:'+inttostr(C)+' - Unable to convert the string '+lNumStr+' to a number');
                                       lError := true;
                                       lTempFloat := nan;
                                    end;
                end;//except
                //showmessage(lNumStr);
                if (C-kHdrCol) = lCol1 then
                   ldataRA1^[R-kHdrRow] := lTempFloat
                else if (C-kHdrCol) = lCol2 then
                    ldataRA2^[R-kHdrRow] := lTempFloat;
               end; //number
             end; //col1 or col2
            end;// else //R > 1

			lNumStr := '';
			inc(C);
			if C > MaxC then
				MaxC := C;
			if (lCh in [#10,#13]) then begin
				C := 1;
			   inc(R);
			end; //eoln
		end; //if lNumStr <> '' and not tab
	 end;
     if (lNumStr <> '') and (C = lnFactors) then begin  //unterminated string
        try
           lTempFloat := strtofloat(lNumStr);
        except
              on EConvertError do begin
                 if not lError then
                    ShowMsg('Empty cells? Error reading CSV file row:'+inttostr(R)+' col:'+inttostr(C)+' - Unable to convert the string '+lNumStr+' to a number');
                 lError := true;
                 lTempFloat := nan;
              end;
        end;//except
        ldataRA2^[R-1] := lTempFloat;
     end;//unterminated string
     //read finel item
	 CloseFile(F);
	 FileMode := 2;  //Set file access to read/write
     result := true;
end;

function AUCcontcont (ldatara1,ldatara2: singlep; lnSubj :integer): double;
var
   lIn,lInDX : DoubleP0;
   lnGroup0,lnGroup1,lI: integer;
begin
   result := 0.5;
   if lnSubj < 1 then
      exit;
   Getmem(lIn,lnSubj*sizeof(double));
   Getmem(lInDX,lnSubj*sizeof(double));
   for lI := 1 to lnSubj do begin
       lIn^[lI-1] := ldatara2^[lI];
       lInDX^[lI-1] := ldatara1^[lI];
   end;
   result := continROC2 (lnSubj, lIn, lInDX);
   freemem(lIn);
   freemem(lInDX);
end;

function AUCbinomcont (lBinomdataRA,lContdataRA: singlep; lnSubj :integer): double;
var
   lIn : DoubleP0;
   lnGroup0,lnGroup1,lI: integer;
begin
   result := 0.5;
   if lnSubj < 1 then
      exit;
   Getmem(lIn,lnSubj*sizeof(double));
   lnGroup0 := 0;
   lnGroup1 := 0;
   for lI := 1 to lnSubj do begin
       if lBinomdataRA^[lI] = 0 then begin
          lIn^[lnGroup0] := lContdataRA^[lI];
          inc (lnGroup0);
       end else begin
          inc (lnGroup1);
          lIn^[lnSubj-lnGroup1] := lContdataRA^[lI];

       end;
   end;
   result := continROC (lnSubj, lnGroup0, lIn);
   freemem(lIn);
end;

procedure testROC;
var
   lROC : single;
   lI,lnSubj,lnGroup0: integer;
   //lIn : DoubleP0;
   //csv
   lnGroup1,lCol1,lCol2: integer;  var lnObservations : integer; var ldataRA1,ldataRA2: singlep ;
begin
         npmform.MainForm.memo1.lines.clear;
     npmform.MainForm.memo1.lines.add('ROC analysis requires CSV format text file.');
     npmform.MainForm.memo1.lines.add('First column is the filename (ignored).');
     npmform.MainForm.memo1.lines.add('Second column is 0 [deficit present] or 1 [no deficit].');
     npmform.MainForm.memo1.lines.add('Third column is number of voxels injured in ROI [0 or greater]:');
        npmform.MainForm.memo1.lines.add('Example file:');
   //npmform.MainForm.memo1.lines.add('deficit, voxels');
   npmform.MainForm.memo1.lines.add('c:\c01.voi,0, 121');
   npmform.MainForm.memo1.lines.add('c:\c02.voi,1, 33');
   npmform.MainForm.memo1.lines.add('c:\c03.voi,0, 222');
   npmform.MainForm.memo1.lines.add('c:\c04.voi,1, 56');
   npmform.MainForm.memo1.lines.add('c:\c05.voi,1, 96');
   npmform.MainForm.memo1.lines.add('c:\c06.voi,0, 100');
   //get csv
   npmform.MainForm.memo1.lines.add('  ...requesting CSV file');

   if not MainForm.OpenDialogExecute('Select comma separated filenames ',false,false,kTxtFilter) then
         exit;
   npmform.MainForm.memo1.lines.add('  ...reading CSV file');
   if not readCSV2 (MainForm.OpenHdrDlg.Filename, 2,3, lnObservations, ldataRA1,ldataRA2) then
        exit;
   npmform.MainForm.memo1.lines.add('  ...observations: '+inttostr(lnObservations));
   if lnObservations < 3 then begin
       ShowMsg('At least 3 subjects required.');
       exit;
   end;
   lnSubj := lnObservations;
   lnGroup0 := 0;
   for lI := 1 to lnSubj do
       if ldatara1^[lI] = 0 then
          inc (lnGroup0);
   npmform.MainForm.memo1.lines.add('  ...observations with deficit [0]: '+inttostr(lnGroup0));
   if (lnGroup0 = lnSubj) or (lnGroup0 = 0) then begin
       ShowMsg('Some values in the first column must be zero, some must be non-zero.');
       exit;
   end;
   lROC := AUCbinomcont (ldatara1,ldatara2, lnSubj);
   (*Getmem(lIn,lnSubj*sizeof(double));
   lnGroup0 := 0;
   lnGroup1 := 0;
   for lI := 1 to lnSubj do begin
       if ldatara1[lI] = 0 then begin
          lIn[lnGroup0] := ldatara2[lI];
          inc (lnGroup0);
       end else begin
          inc (lnGroup1);
          lIn[lnSubj-lnGroup1] := ldatara2[lI];

       end;
   end;
   lROC := continROC (lnSubj, lnGroup0, lIn);
   freemem(lIn);
     *)

   freemem(ldataRA1);
   freemem(ldataRA2);
   //now analyze
   npmform.MainForm.memo1.lines.add('ROC = '+floattostr(lROC));
   //fx(lROC);
end;


procedure testROC2;
var
   //lDouble: double;
   lVariable: boolean;
   lF,lROC : single;
   lI,lnSubj: integer;
   lIn,lInDX : DoubleP0;
   //csv
   lnGroup1,lCol1,lCol2: integer;  var lnObservations : integer; var ldataRA1,ldataRA2: singlep ;
begin
     npmform.MainForm.memo1.lines.clear;
     npmform.MainForm.memo1.lines.add('ROC analysis requires CSV format text file.');
     npmform.MainForm.memo1.lines.add('First column is the filename (ignored).');
     npmform.MainForm.memo1.lines.add('Second column is degree of deficit [lower value = more impaired].');
     npmform.MainForm.memo1.lines.add('Third column is number of voxels injured in ROI [0 or greater]:');
        npmform.MainForm.memo1.lines.add('Example file:');
   //npmform.MainForm.memo1.lines.add('deficit, voxels');
   npmform.MainForm.memo1.lines.add('c:\c01.voi,0.3, 121');
   npmform.MainForm.memo1.lines.add('c:\c02.voi,0.1, 33');
   npmform.MainForm.memo1.lines.add('c:\c03.voi,0.2, 222');
   npmform.MainForm.memo1.lines.add('c:\c04.voi,1.3, 56');
   npmform.MainForm.memo1.lines.add('c:\c05.voi,1.7, 96');
   npmform.MainForm.memo1.lines.add('c:\c06.voi,1.5, 100');
   //get csv
   npmform.MainForm.memo1.lines.add('  ...requesting CSV file');

   if not MainForm.OpenDialogExecute('Select comma separated filenames ',false,false,kTxtFilter) then
         exit;
   npmform.MainForm.memo1.lines.add('  ...reading CSV file');
   if not readCSV2 (MainForm.OpenHdrDlg.Filename, 2,3, lnObservations, ldataRA1,ldataRA2) then
        exit;
   npmform.MainForm.memo1.lines.add('  ...observations: '+inttostr(lnObservations));
   if lnObservations < 3 then begin
       ShowMsg('At least 3 subjects required.');
       exit;
   end;
   lnSubj := lnObservations;
   lF := ldatara1^[1];
   lVariable := false;
   for lI := 1 to lnSubj do
       if ldatara1^[lI] <> lF then
          lVariable := true;
   if (not lVariable) then begin
       ShowMsg('The columns must have some variability.');
       exit;
   end;
   Getmem(lIn,lnSubj*sizeof(double));
   Getmem(lInDX,lnSubj*sizeof(double));
   for lI := 1 to lnSubj do begin
       lIn^[lI-1] := ldatara2^[lI];
       lInDX^[lI-1] := ldatara1^[lI];
   end;
   freemem(ldataRA1);
   freemem(ldataRA2);
   //now analyze
   (*lnSubj := 10;
   lnGroup0 := 5;
   Getmem(lIn,lnSubj*sizeof(double));
   for lI := 0 to (lnSubj-1) do
       lIn[lI] := -lI;//random(99);  *)
   lROC := continROC2 (lnSubj, lIn, lInDX);
   npmform.MainForm.memo1.lines.add('ROC = '+floattostr(lROC));
   freemem(lIn);
   freemem(lInDX);

end;

(*procedure testROC;
var
   lROC : single;
   lI,lnSubj,lnGroup0: integer;
   lIn : DoubleP0;
begin
   lnSubj := 10;
   lnGroup0 := 5;
   Getmem(lIn,lnSubj*sizeof(double));
   for lI := 0 to (lnSubj-1) do
       lIn[lI] := -lI;//random(99);
   lROC := continROC (lnSubj, lnGroup0, lIn);
   npmform.MainForm.memo1.lines.add('ROC = '+floattostr(lROC));
   //fx(lROC);
   freemem(lIn);

end;      *)


end.