Unit gziod;//GZip input/output for delphi
interface

uses define_types,gzio,Windows,sysutils;

procedure UnGZip2 (var lFname: string; var lBuf: ByteP{}; lOffset,lMaxSz: integer; Skip: int64); //unzip
procedure UnGZip (var lFname: string; var lBuf: ByteP{}; lOffset,lMaxSz: integer); //unzip
procedure UnGZipCore (var infile : gzFile; var lBuf: ByteP; lReadBytes: integer; lWrite: boolean);
function Gunzip (var FFileSource,FFileDestination: string): integer;
procedure GZipBuffer(var lFilename: String;lxInBuffer: byteP;lInSize: Integer; lOverwritewarn: boolean);
procedure GZipFile(lSrcName,lDestName: String;lDeleteSrc: boolean);
procedure UnGZipFile (var lFname,lOUtname: string); //unzip
implementation

{$include isgui.inc}
{$IFDEF GUI}uses dialogs;{$ELSE} uses dialogsx;{$ENDIF}

function gz_compressBuffer (lxInBuffer: ByteP;lInSize: integer;outfile:gzFile): integer;
var
  len   : Integer;
  lInBufferPos,ioerr : integer;
  buf  : packed array [0..Z_BUFSIZE-1] of byte; { Global uses BSS instead of stack }
  //lInBufPtr,lOutbufPtr: pointer;
  errorcode : byte;
  //fsize, lensize : DWord;
function blocktransfer(var lInBuffer: ByteP;  lSizeRequested: integer; var lSizeTransferred:integer): integer;
begin
	result := 0;
	if  lInBufferPos > lInSize then begin
		result := 666;
		exit;
	end else if (lInBufferPos + lSizeRequested) <= lInSize then
		lSizeTransferred := lSizeRequested
	else
		lSizeTransferred := lInSize-lInBufferPos;
	//for lC := 1 to lSizeTransferred do
	//	buf[lC-1] := lInBuffer[lInBufferPos+lC] ;
	move(lInbuffer[lInBufferPos+1],buf,lSizeTransferred);
	//move(src,dest,count);

	lInBufferPos := lInBufferPos+lSizeTransferred;
end;
begin
//showmessage(inttostr(Z_BUFSIZE));
  lInBufferPos := 0;
  errorcode := 0;
  //Progress := 0;
  //fsize := lInSize;
  //lensize := 0;
  //if FProgressStep > 0 then DoOnProgress;
  while true do begin
	//lll{$I-}blockread (infile, buf, Z_BUFSIZE, len);{$I+}
	ioerr := blocktransfer(lxInBuffer,Z_BUFSIZE, len);
	if (ioerr <> 0) then begin
	  errorcode := 1;
	  break
	end;
	if (len = 0) then break;
	{$WARNINGS OFF}{Comparing signed and unsigned types}
	if (gzwrite (outfile, @buf, len) <> len) then begin
	{$WARNINGS OFF}
	  errorcode := 2;
	  break
	end;
  end; {WHILE}
  if (gzclose (outfile) <> 0{Z_OK}) then errorcode := 3;
  result := errorcode;
end;

procedure GZipBuffer(var lFilename: String;lxInBuffer: byteP;lInSize: Integer; lOverwritewarn: boolean);
var
   FFileDestination,FGzipComments ,outmode,s : string;
	infile  : file;
	outfile : gzFile;
	FCompressionLevel{,errorcode} : integer;
	flags : uInt;
	stream : gz_streamp;
	p : PChar;
begin
FGzipComments := '';
FFileDestination := lFilename;
//if not GzExt(FFileDestination) then
//   FFileDestination := FFileDestination + '.gz';
FCompressionLevel := 6;//MainForm.CompressEdit.value;
if (FCompressionLevel > 9) or (FCompressionLevel<0) then FCompressionLevel := 6;
 if lOverwritewarn and fileexists(FFileDestination) then begin
        {$IFDEF GUI}
		case MessageDlg('Overwrite the file '+FFileDestination+'?', mtConfirmation,[mbYes, mbAbort], 0) of	{ produce the message dialog box }
			 id_Abort: exit;
		end;
        {$ELSE}
		case MsgDlg('Overwrite the file '+FFileDestination+'?', mtConfirmation,[mbYes, mbAbort], 0) of	{ produce the message dialog box }
			 id_Abort: exit;
		end;

        {$ENDIF}
 end;
	  //w adds .gz extension-> outmode := 'w  ';
	  outmode := 'w  ';
	  s := IntToStr(FCompressionLevel);
	  outmode[2] := s[1];
		  outmode[3] := ' ';
		  flags := ORIG_NAME;
	  //if (comment  in FGzipHeader) then flags := flags + COMMENT_;
	  outfile := gzopenZ (FFileDestination, outmode, flags);
	  //showmessage(FFileDestination);
	  if (outfile = NIL) then begin
		 //if FWindowOnError then
         {$IFDEF GUI}
			  MessageDlg('Can''t open: '+FFileDestination, mtError, [mbAbort], 0);
         {$ELSE}
         MsgDlg('Can''t open: '+FFileDestination, mtError, [mbAbort], 0);
         {$ENDIF}
		 close( infile);
		 //errorcode := 2
				 exit;
	  end
	  else begin
		 { if flags are set then write them }
		 stream := gz_streamp(outfile);
		 if {(zfilename in FGzipHeader)} true then begin
                    //s := ExtractFilename(lInFileName);
			//s := ExtractFilename(FGzipFilename);
                        s := ExtractFilename(changefileext(FFileDestination,''));
			p := PChar(s);
			blockWrite( stream^.gzfile, p[0], length(s)+1);
			stream^.startpos := stream^.startpos + length(s) + 1
		 end;
		 gz_compressBuffer (lxInBuffer,lInSize,outfile);
	  end
end;

function gz_uncompress (infile:gzFile; var outfile:file;fsize:DWord{LongWord}) : integer;
var
  len     : integer;
  written : uInt;
  buf  : packed array [0..Z_BUFSIZE-1] of byte; { Global uses BSS instead of stack }
  errorcode : byte;
  lensize : DWord{LongWord};
begin
  errorcode := 0;
  //FProgress := 0;
  lensize := 0;
  //if FProgressStep > 0 then DoOnProgress;
  while true do begin
	len := gzread (infile, @buf, Z_BUFSIZE);
	if (len < 0) then begin
	   errorcode := 1;
	   break
	end;
	if (len = 0)
	  then break;
	{$I-}
	blockwrite (outfile, buf, len, written);
	{$I+}
	{$WARNINGS OFF}{Comparing signed and unsigned types}
	if (written <> len) then begin
	{$WARNINGS ON}
	   errorcode := 2;
	   break
	end;
	(*if FProgressStep > 0 then begin
	   {$WARNINGS OFF}
	   lensize := lensize + len;
	   if ((lensize / fsize) * 100 >= FProgress + FProgressStep)
						or (lensize = fsize) then begin
		  FProgress := Trunc((lensize / fsize) * 100);
		  DoOnProgress
	   end
	   {$WARNINGS ON}
	end *)
  end; {WHILE}
  if (gzclose (infile) <> 0{Z_OK}) then begin
  {$IFDEF GUI}
		MessageDlg('gzclose Error.', mtError, [mbAbort], 0);
   {$ELSE}
		MsgDlg('gzclose Error.', mtError, [mbAbort], 0);
   {$ENDIF}
	 errorcode := 3
  end;
  gz_uncompress := errorcode
end;


function Gunzip (var FFileSource,FFileDestination: string): integer;
var
	infile : gzFile;
	outfile : file;
	ioerr : integer;
	errorcode : integer;
	fsize : DWord{LongWord};
	s : gz_streamp;
begin
  errorcode := 0;

  infile := gzopenZ (FFileSource, 'r', 0);
  if (infile = NIL) then begin
	//if FWindowOnError then
    {$IFDEF GUI}
	   MessageDlg('Can''t open: '+FFileSource, mtError, [mbAbort], 0);
    {$ELSE}
	   MsgDlg('Can''t open: '+FFileSource, mtError, [mbAbort], 0);
    {$ENDIF}
	errorcode := 1
  end
  else begin
	s := gz_streamp(infile);
	fsize := FileSize( s^.gzfile);

	AssignFile (outfile, FFileDestination);
	{$I-}
	Rewrite (outfile,1);
	{$I+}
	ioerr := IOResult;
	if (ioerr <> 0) then begin
		//if FWindowOnError then
    {$IFDEF GUI}
		   MessageDlg('Can''t create: '+FFileDestination, mtError, [mbAbort], 0);
    {$ELSE}
		   MsgDlg('Can''t create: '+FFileDestination, mtError, [mbAbort], 0);
    {$ENDIF}

		errorcode := 2
	end
	else begin
		{ We could open all files, so time for uncompressing }
		gz_uncompress (infile, outfile, fsize);
		//if FDeleteSource then DeleteFile(FFileSource);
	   {$I-}
	   close (outfile);
	   {$I+}
	   ioerr := IOResult;
	   if (ioerr <> 0) then begin
		  //if FWindowOnError then
  {$IFDEF GUI}
			 MessageDlg('Can''t close file '+FFileDestination, mtError, [mbAbort], 0);
   {$ELSE}
			 MsgDlg('Can''t close file '+FFileDestination, mtError, [mbAbort], 0);
   {$ENDIF}

		  halt(1)
	   end
	end
  end;

  Gunzip := errorcode
end;

procedure UnGZipCore (var infile : gzFile; var lBuf: ByteP; lReadBytes: integer; lWrite: boolean);
const
     BUFLEN = 16384;
var
  buf  : packed array [0..BUFLEN-1] of byte; { Global uses BSS instead of stack }
  len,lI,written : integer;
begin
  written := 0;
  if lReadBytes < 1 then exit;
  Len := lReadBytes div BUFLEN;
  if Len > 0 then
        for lI := 1 to Len do begin
            gzread (infile, @buf, BUFLEN {1388});
            if lWrite then
               Move(buf,lbuf[Written+1],BUFLEN);
            Written := Written + BUFLEN;
        end;
  Len := lReadBytes mod BUFLEN;
  if Len = 0 then exit;
  gzread (infile, @buf, Len);
  if lWrite then
        Move(buf,lbuf[Written+1],len);
end; //ungzipCore

procedure UnGZip2 (var lFname: string; var lBuf: ByteP; lOffset,lMaxSz: integer; Skip: int64);
const
BUFLEN = 16384;
var
	infile : gzFile;
  lbufsz,len,lI     : integer;
  written : integer;
  buf  : packed array [0..BUFLEN-1] of byte; { Global uses BSS instead of stack }
begin
  infile := gzopenZskip (lFName, 'r', 0, Skip);
  written := 0;
  if lOffset > 0 then begin
     Len := lOffset div BUFLEN;
     if Len > 0 then
        for lI := 1 to Len do
            gzread (infile, @buf, BUFLEN {1388});
     Len := lOffset mod BUFLEN;
     gzread (infile, @buf, Len);
  end;
  lbufsz := BUFLEN;
  if lMaxSz < BUFLEN then
     lbufsz := lMaxSz;
  while true do begin
	len := gzread (infile, @buf, lbufsz);
	if (len < 0) then begin
	   break
	end;
	if (len = 0)
	  then break;
        if (Written+len) > lMaxSz then begin
             if Written < lMaxSz then
              Move(buf,lbuf[Written+1],lMaxSz-Written); //cr2007
             break;
        end;
		Move(buf,lbuf[Written+1],len);
		Written := Written + len;
  end; {WHILE}
  gzclose (infile);
end;

procedure UnGZip (var lFname: string; var lBuf: ByteP; lOffset,lMaxSz: integer);
begin
  UnGZip2 (lFname, lBuf, lOffset,lMaxSz,0);
end;
(*procedure UnGZip (var lFname: string; var lBuf: ByteP; lOffset,lMaxSz: integer);
const
BUFLEN = 16384;
var
	infile : gzFile;
  lbufsz,len,lI     : integer;
  written : integer;
  buf  : packed array [0..BUFLEN-1] of byte; { Global uses BSS instead of stack }
begin
  infile := gzopenZ (lFName, 'r', 0);
  written := 0;
  if lOffset > 0 then begin
     Len := lOffset div BUFLEN;
     if Len > 0 then
        for lI := 1 to Len do
            gzread (infile, @buf, BUFLEN {1388});
     Len := lOffset mod BUFLEN;
     gzread (infile, @buf, Len);
  end;
  lbufsz := BUFLEN;
  if lMaxSz < BUFLEN then
     lbufsz := lMaxSz;
  while true do begin
	len := gzread (infile, @buf, lbufsz);
	if (len < 0) then begin
	   break
	end;
	if (len = 0)
	  then break;
        if (Written+len) > lMaxSz then begin
             if Written < lMaxSz then
              Move(buf,lbuf[Written+1],lMaxSz-Written); //cr2007
             break;
        end;
		Move(buf,lbuf[Written+1],len);
		Written := Written + len;
  end; {WHILE}
  gzclose (infile);
end;*)

procedure UnGZipFile (var lFname,lOUtname: string); //unzip
//1417z- offset
const
bufsz = 16384;
var
	infile : gzFile;
  len,lI     : integer;
  //written : integer;
  	lF: File;
  buf  : packed array [0..bufsz-1] of byte; { Global uses BSS instead of stack }
begin
  infile := gzopenZ (lFName, 'r', 0);
  //written := 0;
  //lbufsz := BUFLEN;
     Filemode := 1;
     AssignFile(lF, lOUtname);
     Rewrite(lF,1);
  while true do begin
	len := gzread (infile, @buf, bufsz);
	if (len < 0) then begin
	   break
	end;
	if (len = 0)
	  then break;
        BlockWrite(lF,buf, len);
               //Move(buf,lbuf[Written+1],len);
		//Written := Written + len;
  end; {WHILE}
  gzclose (infile);
  CloseFile(lF);
  Filemode := 2; //1366
end;


function gz_compress (var infile:file; outfile:gzFile): integer;
var
  len   : uInt;
  ioerr : integer;
  buf  : packed array [0..Z_BUFSIZE-1] of byte; { Global uses BSS instead of stack }
  errorcode : byte;
 // fsize, lensize : DWord;
(*{$IFDEF VER100, VER90}
  fsize, lensize : DWord;
{$ELSE}
       fsize, lensize : LongWord;
{$ENDIF} *)
begin
  errorcode := 0;
  //Progress := 0;
  //fsize := FileSize(infile);
  //lensize := 0;
  //if FProgressStep > 0 then DoOnProgress;
  while true do begin
	{$I-}
	blockread (infile, buf, Z_BUFSIZE, len);
	{$I+}

	ioerr := IOResult;
	if (ioerr <> 0) then begin
	  errorcode := 1;
	  break
	end;
	if (len = 0) then break;
	{$WARNINGS OFF}{Comparing signed and unsigned types}
	if (gzwrite (outfile, @buf, len) <> len) then begin
	{$WARNINGS OFF}
	  errorcode := 2;
	  break
	end;
	(*if FProgressStep > 0 then begin
	   {$WARNINGS OFF}{Calculate progress and raise event}
	   lensize := lensize + len;
	   if ((lensize / fsize) * 100 >= FProgress + FProgressStep)
						or (lensize = fsize) then begin
		  FProgress := Trunc((lensize / fsize) * 100);
		  DoOnProgress
	   end
	   {$WARNINGS ON}
	end *)
  end; {WHILE}

  closeFile (infile);
  if (gzclose (outfile) <> 0{Z_OK}) then errorcode := 3;

  gz_compress := errorcode;
end;


procedure GZipFile(lSrcName,lDestName: String;lDeleteSrc: boolean);
var
   //FGzipHeader : THeader;
   //FCompressionLevel,FProgress,Progress: integer;
   FGzipFilename : string;
   FGzipComments : string;
   outmode : string;
	s : string;
	infile  : file;
	outfile : gzFile;
	FCompressionLevel{,errorcode} : integer;
	flags : uInt;
	stream : gz_streamp;
	p : PChar;
        //lProceed: TModalResult;
	ioerr : integer;
begin
//FGzipHeader := [zFilename];
FGzipFilename:= lSrcName;
FGzipComments := '';
//FProgress := 0;
FCompressionLevel := 6;
if (FCompressionLevel > 9) or (FCompressionLevel<0) then FCompressionLevel := 6;
//MainForm.ProgressBar1.position :=1;
//Gzip (lFile,lMulti);
 //FFileDestination := lSourceFile+'.gz';
 //result := 2; //return error if user aborts
 if fileexists(lDestName) then begin
  {$IFDEF GUI}
        case MessageDlg('Overwrite the file '+lDestName+'?', mtConfirmation,[mbYes, mbAbort], 0) of	{ produce the message dialog box }
             id_Abort: exit;
        end;
   {$ELSE}
        case MsgDlg('Overwrite the file '+lDestName+'?', mtConfirmation,[mbYes, mbAbort], 0) of	{ produce the message dialog box }
             id_Abort: exit;
        end;
   {$ENDIF}
 end;
  AssignFile (infile, lSrcName);
  {$I-}
  Reset (infile,1);
  {$I+}
  ioerr := IOResult;
  if (ioerr <> 0) then begin
	//if FWindowOnError then
	//	 MessageDlg('Can''t open: '+lSourceFile, mtError, [mbAbort], 0);
	//errorcode := 1
  end
  else begin
	  outmode := 'w  ';
	  s := IntToStr(FCompressionLevel);
	  outmode[2] := s[1];
          outmode[3] := ' ';
	  (*case FCompressionType of
		   Standard    : outmode[3] := ' ';
		   HuffmanOnly : outmode[3] := 'h';
		   Filtered    : outmode[3] := 'f';
	  end;*)

	  //flags := 0;
	  //if (zfilename in FGzipHeader) then
          flags := ORIG_NAME;
	  //if (comment  in FGzipHeader) then flags := flags + COMMENT_;
	  outfile := gzopenZ (lDestName, outmode, flags);
	  if (outfile = NIL) then begin
		 //if FWindowOnError then
		 //	  MessageDlg('Can''t open: '+FFileDestination, mtError, [mbAbort], 0);
		 close( infile);
		 //errorcode := 2
                 exit;
	  end
	  else begin
		 { if flags are set then write them }
		 stream := gz_streamp(outfile);
		 if {(zfilename in FGzipHeader)} true then begin
                        s := ExtractFilename(lSrcName);
			p := PChar(s);
			blockWrite( stream^.gzfile, p[0], length(s)+1);
			stream^.startpos := stream^.startpos + length(s) + 1
		 end;
		 {if (zcomment  in FGzipHeader) then begin
			p := PChar(FGzipComments);
			blockWrite( stream^.gzfile, p[0], length(FGzipComments)+1);
			stream^.startpos := stream^.startpos + length(FGzipComments) + 1
		 end; }
		 {errorcode :=} gz_compress(infile, outfile);
		 {if errorcode <> 0 then errorcode := errorcode+100
		 else
			if FDeleteSource then erase (infile);}
	  end
   end;
   if lDeleteSrc then erase (infile);
end;

end.
