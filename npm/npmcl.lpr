program npmcl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  //cthreads,
  {$ENDIF}{$ENDIF}
   cthreads,

  Classes, SysUtils, CustApp,prefs, unpm, userdir,StatThdsUtil, cpucount, define_types, turbolesion;

type
  TNPMcl = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetOptionValueInt(lCmd: string; lDefault: integer): integer;
    //procedure WriteHelp; virtual;
    procedure ThreadDone(Sender: TObject);
  end;

var
  Application: TNPMcl;

procedure msg(s: string);
begin
  { add your help code here }
  writeln(s);
end;




procedure ShowOptions (lTestInt: integer; lMaskFilename,lOutFilename: string);
begin
  msg(' -c : CPU threads, Default : '+inttostr(gnCPUThreads));
  msg(' -m : mask name. Default "' +lMaskFilename+'"');
  msg(' -n : neighbors for TFCE, 0 for none. Default ' +inttostr(gNPMprefs.TFCE));
  msg(' -o : output name. Default "' +lOutFilename+'"');
  msg(' -p : Permutations, 0 for none. Default '+inttostr(gNPMprefs.nPermute));
  msg(' -r : RAM for processing (Mb). Default '+inttostr(kPlankMB));
  msg(' -t : test (0=continuous,1=binomial,2=regress,3=multiregress). Default '+inttostr(lTestInt));

end;

procedure WriteHelp ;
begin
     msg(GetKVers);
     msg(' usage: '+ExtractFileName(FileNameNoExt(paramstr(0)))+' [options] [-t test] [valfilename]' );
     msg('Examples:');
     msg(' '+ ExtractFileName(FileNameNoExt(paramstr(0)))+' -t 0 test.val');
     msg(' '+ ExtractFileName(FileNameNoExt(paramstr(0)))+' -r 1024 -p 1000 -m mymask.nii -t 0 test.val');
     msg('Options:');
     msg(' -h : Help displayed');
end;

procedure TNPMcl.ThreadDone(Sender: TObject);
begin
     Dec(gThreadsRunning);
end;
function TNPMcl.GetOptionValueInt(lCmd: string; lDefault: integer): integer;
var
  lResp : string;
begin
     lResp := GetOptionValue(lCmd);
     if length(lResp) < 1 then result := lDefault;
     try
        result := strtoint(lResp);
     except
       Writeln('Error '+(lResp)+' is not a valid integer.');
       result := lDefault;
    end;


end;

procedure doVLSM(lBinomial: boolean; VALFilename, lMaskFilename,lOutFilename: string);
 var
        lPrefs: TLDMPrefs ;
begin
     lPrefs.NULP := gNPMPrefs.NULP;
     if (not lBinomial) then begin //continuous
             lPrefs.BMtest :=   true;
             lPrefs.Ttest := true;
             lPrefs.Ltest:= false;
     end else begin //binomial
             lPrefs.BMtest := false;
             lPrefs.Ttest := false;
             lPrefs.Ltest:= true;
     end;
     lPrefs.CritPct := -1;
     lPrefs.nPermute := gNPMprefs.nPermute;
     lPrefs.Run := 0;{0 except for montecarlo}
     lPrefs.VALFilename := VALFilename;
     lPrefs.OutName := lOutFilename;
    lPrefs.ExplicitMaskName := lMaskFilename;
    DoLesion (lPrefs);
end;



procedure TNPMcl.DoRun;
label
  666;
var
   lTestInt: integer = 0;
   lMaskFilename : string = '';
   lValFilename : string = '';
   lOutFilename : string = '';
begin
  gnCPUThreads := GetLogicalCpuCount;
  ReadIniFile;
  // parse parameters
  if (HasOption('h','help')) or (ParamCount = 0) then begin
    WriteHelp;
    ShowOptions(lTestInt, lMaskFilename, lOutFilename);
    goto 666;
  end;
  if (HasOption('c')) then gnCPUThreads := GetOptionValueInt('c', gnCPUThreads);
  if (HasOption('m')) then begin
     lMaskFilename := GetOptionValue('m');
     if not (not FileExistsEX(lMaskFilename)) then begin
        writeln('Can not fine masking image '+ lMaskFilename);
        goto 666;
     end;
  end;
  if (HasOption('n')) then gnCPUThreads := GetOptionValueInt('n', gNPMprefs.TFCE);
  if (HasOption('o')) then begin
     lOutFilename := GetOptionValue('o');
  end;
  if (HasOption('p')) then gNPMprefs.nPermute := GetOptionValueInt('p', gNPMprefs.nPermute);
  if (HasOption('r')) then begin
     kPlankMB := GetOptionValueInt('r', kPlankMB);
     ComputePlankSize(kPlankMB);
  end;
  if (HasOption('t')) then lTestInt := GetOptionValueInt('t', lTestInt);


  lValFilename := (paramstr(ParamCount));
  if (UpCaseExt(lValFilename) <> '.VAL') or (not FileExistsEX(lValFilename)) then begin
     Writeln('Error: final option should be an existing file with the .val extension');
     goto 666;
  end;

  if (lOutFilename = '') then begin
     lOutFilename := ChangeFileExtX( lValFilename,'');
  end;
  //show settings
  ShowOptions(lTestInt,lMaskFilename,lOutFilename);
  Writeln('VAL File: '+lValFilename);
  //run test
  case lTestInt of
       0: doVLSM(false, lVALFilename, lMaskFilename,lOutFilename);//continuous : t-test
       1: doVLSM(true, lVALFilename, lMaskFilename,lOutFilename);//binomial: Liebermeister
       2: NPMSingleRegress ( lVALFilename, lMaskFilename,lOutFilename);
       3: NPMMultipleRegressClick( lVALFilename, lMaskFilename,lOutFilename);
  end;


  WriteIniFile;
  // stop program loop
  666:
  Terminate;
end;

constructor TNPMcl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TNPMcl.Destroy;
begin
  inherited Destroy;
end;


begin
  Application:=TNPMcl.Create(nil);
  Application.Title:='NPMcl';
  Application.Run;
  Application.Free;
end.

