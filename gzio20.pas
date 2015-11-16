Unit gzio2;

{
  Pascal unit based on gzio.c -- IO on .gz files
  Copyright (C) 1995-1998 Jean-loup Gailly.

  Define NO_DEFLATE to compile this file without the compression code

  Pascal tranlastion based on code contributed by Francisco Javier Crespo
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I zconf.inc}

uses
  {$ifdef MSDOS}
  dos, strings,
  {$else}
  SysUtils,
  {$endif}
  zutil, zbase, gzcrc, zdeflate, zinflate, define_types,dialogs;

type gzFile = voidp;
type z_off_t = long;

procedure GZipBuffer(var FGzipFilename,FFileDestination: String;lxInBuffer: byteP;lInSize: Integer; lOverwritewarn: boolean);
procedure UnGZip (var lInFname: string; var lBuf: ByteP; lOffset,lMaxSz: integer); //unzip
procedure UnGZipCore (var infile : gzFile; var lBuf: ByteP; lReadBytes: integer; lWrite: boolean);

function gzopen  (path:ansistring; mode:string) : gzFile;
function gzsetparams (f:gzfile; level:int; strategy:int) : int;
function gzread  (f:gzFile; buf:voidp; len:uInt) : int;
function gzgetc  (f:gzfile) : int;
function gzgets  (f:gzfile; buf:PChar; len:int) : PChar;

{$ifndef NO_DEFLATE}
function gzwrite (f:gzFile; buf:voidp; len:uInt) : int;
function gzputc  (f:gzfile; c:char) : int;
function gzputs  (f:gzfile; s:PChar) : int;
function gzflush (f:gzFile; flush:int)           : int;
  {$ifdef GZ_FORMAT_STRING}
  function gzprintf (zfile : gzFile;
                     const format : string;
                     a : array of int);    { doesn't compile }
  {$endif}
{$endif}

function gzseek   (f:gzfile; offset:z_off_t; whence:int) : z_off_t;
function gzrewind (f:gzFile) : int;
function gztell   (f:gzfile) : z_off_t;
function gzeof    (f:gzfile) : boolean;
function gzclose  (f:gzFile)                      : int;
function gzerror  (f:gzFile; var errnum:Int)      : string;

const
  SEEK_SET {: z_off_t} = 0; { seek from beginning of file }
  SEEK_CUR {: z_off_t} = 1; { seek from current position }
  SEEK_END {: z_off_t} = 2;

implementation

const
  Z_EOF = -1;         { same value as in STDIO.H }
  Z_BUFSIZE = 16384;
  { Z_PRINTF_BUFSIZE = 4096; }


  gz_magic : array[0..1] of byte = ($1F, $8B); { gzip magic header }

  { gzip flag byte }

  ASCII_FLAG  = $01; { bit 0 set: file probably ascii text }
  HEAD_CRC    = $02; { bit 1 set: header CRC present }
  EXTRA_FIELD = $04; { bit 2 set: extra field present }
  ORIG_NAME   = $08; { bit 3 set: original file name present }
  COMMENT     = $10; { bit 4 set: file comment present }
  RESERVED    = $E0; { bits 5..7: reserved }

type gz_stream = record
  stream      : z_stream;
  z_err       : int;      { error code for last stream operation }
  z_eof       : boolean;  { set if end of input file }
  gzfile      : file;     { .gz file }
  inbuf       : pBytef;   { input buffer }
  outbuf      : pBytef;   { output buffer }
  crc         : uLong;    { crc32 of uncompressed data }
  msg,                    { error message - limit 79 chars }
  path        : string[79];//: ansistring;   { path name for debugging only - limit 79 chars }
  transparent : boolean;  { true if input file is not a .gz file }
  mode        : char;     { 'w' or 'r' }
  startpos    : long;     { start of compressed data in file (header skipped) }
end;

type gz_streamp = ^gz_stream;

function destroy (var s:gz_streamp) : int; forward;
procedure check_header(s:gz_streamp); forward;






{ GZOPEN ====================================================================

  Opens a gzip (.gz) file for reading or writing. As Pascal does not use
  file descriptors, the code has been changed to accept only path names.

  The mode parameter defaults to BINARY read or write operations ('r' or 'w')
  but can also include a compression level ('w9') or a strategy: Z_FILTERED
  as in 'w6f' or Z_HUFFMAN_ONLY as in 'w1h'. (See the description of
  deflateInit2 for more information about the strategy parameter.)

  gzopen can be used to open a file which is not in gzip format; in this
  case, gzread will directly read from the file without decompression.

  gzopen returns NIL if the file could not be opened (non-zero IOResult)
  or if there was insufficient memory to allocate the (de)compression state
  (zlib error is Z_MEM_ERROR).

============================================================================}

function gzopen (path:ansistring; mode:string) : gzFile;

var

  i        : uInt;
  err      : int;
  level    : int;        { compression level }
  strategy : int;        { compression strategy }
  s        : gz_streamp;
{$IFDEF MSDOS}
  attr     : word;       { file attributes }
{$ENDIF}

{$IFNDEF NO_DEFLATE}
  gzheader : array [0..9] of byte;
{$ENDIF}

begin

  if (path='') or (mode='') then begin
    gzopen := Z_NULL;
    exit;
  end;

  GetMem (s,sizeof(gz_stream));
  if not Assigned (s) then begin
    gzopen := Z_NULL;
    exit;
  end;

  level := Z_DEFAULT_COMPRESSION;
  strategy := Z_DEFAULT_STRATEGY;

  s^.stream.zalloc := NIL;     { (alloc_func)0 }
  s^.stream.zfree := NIL;      { (free_func)0 }
  s^.stream.opaque := NIL;     { (voidpf)0 }
  s^.stream.next_in := Z_NULL;
  s^.stream.next_out := Z_NULL;
  s^.stream.avail_in := 0;
  s^.stream.avail_out := 0;
  s^.z_err := Z_OK;
  s^.z_eof := false;
  s^.inbuf := Z_NULL;
  s^.outbuf := Z_NULL;
  s^.crc := crc32(0, Z_NULL, 0);
  s^.msg := '';
  s^.transparent := false;

  s^.path := path; { limit to 255 chars }

  s^.mode := chr(0);
  for i:=1 to Length(mode) do begin
    case mode[i] of
      'r'      : s^.mode := 'r';
      'w'      : s^.mode := 'w';
      '0'..'9' : level := Ord(mode[i])-Ord('0');
      'f'      : strategy := Z_FILTERED;
      'h'      : strategy := Z_HUFFMAN_ONLY;
    end;
  end;
  if (s^.mode=chr(0)) then begin
    destroy(s);
    gzopen := gzFile(Z_NULL);
    exit;
  end;

  if (s^.mode='w') then begin
{$IFDEF NO_DEFLATE}
    err := Z_STREAM_ERROR;
{$ELSE}
    err := deflateInit2 (s^.stream, level, Z_DEFLATED, -MAX_WBITS,
                         DEF_MEM_LEVEL, strategy);
        { windowBits is passed < 0 to suppress zlib header }

    GetMem (s^.outbuf, Z_BUFSIZE);
    s^.stream.next_out := s^.outbuf;
{$ENDIF}
    if (err <> Z_OK) or (s^.outbuf = Z_NULL) then begin
      destroy(s);
      gzopen := gzFile(Z_NULL);
      exit;
    end;
  end

  else begin
    GetMem (s^.inbuf, Z_BUFSIZE);
    s^.stream.next_in := s^.inbuf;

    err := inflateInit2_ (s^.stream, -MAX_WBITS, ZLIB_VERSION, sizeof(z_stream));
        { windowBits is passed < 0 to tell that there is no zlib header }

    if (err <> Z_OK) or (s^.inbuf = Z_NULL) then begin
      destroy(s);
      gzopen := gzFile(Z_NULL);
      exit;
    end;
  end;

  s^.stream.avail_out := Z_BUFSIZE;

  {$IFOPT I+} {$I-} {$define IOcheck} {$ENDIF}
  Assign (s^.gzfile, s^.path);
  {$ifdef MSDOS}
  GetFAttr(s^.gzfile, Attr);
  if (DosError <> 0) and (s^.mode='w') then
    ReWrite (s^.gzfile,1)
  else
    Reset (s^.gzfile,1);
  {$else}
  if (not FileExists(s^.path)) and (s^.mode='w') then
    ReWrite (s^.gzfile,1)
  else
    Reset (s^.gzfile,1);
  {$endif}
  {$IFDEF IOCheck} {$I+} {$ENDIF}
  if (IOResult <> 0) then begin
    destroy(s);
    gzopen := gzFile(Z_NULL);
    exit;
  end;

  if (s^.mode = 'w') then begin { Write a very simple .gz header }
{$IFNDEF NO_DEFLATE}
    gzheader [0] := gz_magic [0];
    gzheader [1] := gz_magic [1];
    gzheader [2] := Z_DEFLATED;   { method }
    gzheader [3] := 0;            { flags }
    gzheader [4] := 0;            { time[0] }
    gzheader [5] := 0;            { time[1] }
    gzheader [6] := 0;            { time[2] }
    gzheader [7] := 0;            { time[3] }
    gzheader [8] := 0;            { xflags }
    gzheader [9] := 0;            { OS code = MS-DOS }
    blockwrite (s^.gzfile, gzheader, 10);
    s^.startpos := LONG(10);
{$ENDIF}
  end
  else begin
    check_header(s); { skip the .gz header }
    s^.startpos := FilePos(s^.gzfile) - s^.stream.avail_in;
  end;

  gzopen := gzFile(s);
end;


{ GZSETPARAMS ===============================================================

  Update the compression level and strategy.

============================================================================}

function gzsetparams (f:gzfile; level:int; strategy:int) : int;

var

  s : gz_streamp;
  written: integer;

begin

  s := gz_streamp(f);

  if (s = NIL) or (s^.mode <> 'w') then begin
    gzsetparams := Z_STREAM_ERROR;
    exit;
  end;

  { Make room to allow flushing }
  if (s^.stream.avail_out = 0) then begin
    s^.stream.next_out := s^.outbuf;
    blockwrite(s^.gzfile, s^.outbuf^, Z_BUFSIZE, written);
    if (written <> Z_BUFSIZE) then s^.z_err := Z_ERRNO;
    s^.stream.avail_out := Z_BUFSIZE;
  end;

  gzsetparams := deflateParams (s^.stream, level, strategy);
end;


{ GET_BYTE ==================================================================

  Read a byte from a gz_stream. Updates next_in and avail_in.
  Returns EOF for end of file.
  IN assertion: the stream s has been sucessfully opened for reading.

============================================================================}

function get_byte (s:gz_streamp) : int;

begin

  if (s^.z_eof = true) then begin
    get_byte := Z_EOF;
    exit;
  end;

  if (s^.stream.avail_in = 0) then begin
    {$I-}
    blockread (s^.gzfile, s^.inbuf^, Z_BUFSIZE, Int(s^.stream.avail_in));
    {$I+}
    if (s^.stream.avail_in = 0) then begin
      s^.z_eof := true;
      if (IOResult <> 0) then s^.z_err := Z_ERRNO;
      get_byte := Z_EOF;
      exit;
    end;
    s^.stream.next_in := s^.inbuf;
  end;

  Dec(s^.stream.avail_in);
  get_byte := s^.stream.next_in^;
  Inc(s^.stream.next_in);

end;


{ GETLONG ===================================================================

   Reads a Longint in LSB order from the given gz_stream.

============================================================================}
{
function getLong (s:gz_streamp) : uLong;
var
  x  : array [0..3] of byte;
  i  : byte;
  c  : int;
  n1 : longint;
  n2 : longint;
begin

  for i:=0 to 3 do begin
    c := get_byte(s);
    if (c = Z_EOF) then s^.z_err := Z_DATA_ERROR;
    x[i] := (c and $FF)
  end;
  n1 := (ush(x[3] shl 8)) or x[2];
  n2 := (ush(x[1] shl 8)) or x[0];
  getlong := (n1 shl 16) or n2;
end;
}
function getLong(s : gz_streamp) : uLong;
var
  x : packed array [0..3] of byte;
  c : int;
begin
  { x := uLong(get_byte(s));  - you can't do this with TP, no unsigned long }
  { the following assumes a little endian machine and TP }
  x[0] := Byte(get_byte(s));
  x[1] := Byte(get_byte(s));
  x[2] := Byte(get_byte(s));
  c := get_byte(s);
  x[3] := Byte(c);
  if (c = Z_EOF) then
    s^.z_err := Z_DATA_ERROR;
  GetLong := uLong(longint(x));
end;


{ CHECK_HEADER ==============================================================

  Check the gzip header of a gz_stream opened for reading.
  Set the stream mode to transparent if the gzip magic header is not present.
  Set s^.err  to Z_DATA_ERROR if the magic header is present but the rest of
  the header is incorrect.

  IN assertion: the stream s has already been created sucessfully;
  s^.stream.avail_in is zero for the first time, but may be non-zero
  for concatenated .gz files

============================================================================}

procedure check_header (s:gz_streamp);

var

  method : int;  { method byte }
  flags  : int;  { flags byte }
  len    : uInt;
  c      : int;

begin

  { Check the gzip magic header }
  for len := 0 to 1 do begin
    c := get_byte(s);
    if (c <> gz_magic[len]) then begin
      if (len <> 0) then begin
        Inc(s^.stream.avail_in);
        Dec(s^.stream.next_in);
      end;
      if (c <> Z_EOF) then begin
        Inc(s^.stream.avail_in);
        Dec(s^.stream.next_in);
        s^.transparent := TRUE;
      end;
      if (s^.stream.avail_in <> 0) then s^.z_err := Z_OK
      else s^.z_err := Z_STREAM_END;
      exit;
    end;
  end;

  method := get_byte(s);
  flags := get_byte(s);
  if (method <> Z_DEFLATED) or ((flags and RESERVED) <> 0) then begin
    s^.z_err := Z_DATA_ERROR;
    exit;
  end;

  for len := 0 to 5 do get_byte(s); { Discard time, xflags and OS code }

  if ((flags and EXTRA_FIELD) <> 0) then begin { skip the extra field }
    len := uInt(get_byte(s));
    len := len + (uInt(get_byte(s)) shr 8);
    { len is garbage if EOF but the loop below will quit anyway }
    while (len <> 0) and (get_byte(s) <> Z_EOF) do Dec(len);
  end;

  if ((flags and ORIG_NAME) <> 0) then begin { skip the original file name }
    repeat
      c := get_byte(s);
    until (c = 0) or (c = Z_EOF);
  end;

  if ((flags and COMMENT) <> 0) then begin { skip the .gz file comment }
    repeat
      c := get_byte(s);
    until (c = 0) or (c = Z_EOF);
  end;

  if ((flags and HEAD_CRC) <> 0) then begin { skip the header crc }
    get_byte(s);
    get_byte(s);
  end;

  if (s^.z_eof = true) then
    s^.z_err := Z_DATA_ERROR
  else
    s^.z_err := Z_OK;

end;


{ DESTROY ===================================================================

  Cleanup then free the given gz_stream. Return a zlib error code.
  Try freeing in the reverse order of allocations.

============================================================================}

function destroy (var s:gz_streamp) : int;

begin

  destroy := Z_OK;

  if not Assigned (s) then begin
    destroy := Z_STREAM_ERROR;
    exit;
  end;

  if (s^.stream.state <> NIL) then begin
    if (s^.mode = 'w') then begin
{$IFDEF NO_DEFLATE}
      destroy := Z_STREAM_ERROR;
{$ELSE}
      destroy := deflateEnd(s^.stream);
{$ENDIF}
    end
    else if (s^.mode = 'r') then begin
      destroy := inflateEnd(s^.stream);
    end;
  end;

  if (s^.path <> '') then begin
    {$I-}
    close(s^.gzfile);
    {$I+}
    if (IOResult <> 0) then destroy := Z_ERRNO;
  end;

  if (s^.z_err < 0) then destroy := s^.z_err;

  if Assigned (s^.inbuf) then
    FreeMem(s^.inbuf, Z_BUFSIZE);
  if Assigned (s^.outbuf) then
    FreeMem(s^.outbuf, Z_BUFSIZE);
  FreeMem(s, sizeof(gz_stream));

end;


{ GZREAD ====================================================================

  Reads the given number of uncompressed bytes from the compressed file.
  If the input file was not in gzip format, gzread copies the given number
  of bytes into the buffer.

  gzread returns the number of uncompressed bytes actually read
  (0 for end of file, -1 for error).

============================================================================}

function gzread (f:gzFile; buf:voidp; len:uInt) : int;

var

  s         : gz_streamp;
  start     : pBytef;
  next_out  : pBytef;
  n         : uInt;
  crclen    : uInt;  { Buffer length to update CRC32 }
  filecrc   : uLong; { CRC32 stored in GZIP'ed file }
  filelen   : uLong; { Total lenght of uncompressed file }
  bytes     : integer;  { bytes actually read in I/O blockread }
  total_in  : uLong;
  total_out : uLong;

begin

  s := gz_streamp(f);
  start := pBytef(buf); { starting point for crc computation }

  if (s = NIL) or (s^.mode <> 'r') then begin
    gzread := Z_STREAM_ERROR;
    exit;
  end;

  if (s^.z_err = Z_DATA_ERROR) or (s^.z_err = Z_ERRNO) then begin
    gzread := -1;
    exit;
  end;

  if (s^.z_err = Z_STREAM_END) then begin
    gzread := 0;  { EOF }
    exit;
  end;

  s^.stream.next_out := pBytef(buf);
  s^.stream.avail_out := len;

  while (s^.stream.avail_out <> 0) do begin

    if (s^.transparent = true) then begin
      { Copy first the lookahead bytes: }
      n := s^.stream.avail_in;
      if (n > s^.stream.avail_out) then n := s^.stream.avail_out;
      if (n > 0) then begin
        zmemcpy(s^.stream.next_out, s^.stream.next_in, n);
        inc (s^.stream.next_out, n);
        inc (s^.stream.next_in, n);
        dec (s^.stream.avail_out, n);
        dec (s^.stream.avail_in, n);
      end;
      if (s^.stream.avail_out > 0) then begin
        blockread (s^.gzfile, s^.stream.next_out^, s^.stream.avail_out, bytes);
        dec (s^.stream.avail_out, uInt(bytes));
      end;
      dec (len, s^.stream.avail_out);
      inc (s^.stream.total_in, uLong(len));
      inc (s^.stream.total_out, uLong(len));
      gzread := int(len);
      exit;
    end; { IF transparent }

    if (s^.stream.avail_in = 0) and (s^.z_eof = false) then begin
      {$I-}
      blockread (s^.gzfile, s^.inbuf^, Z_BUFSIZE, Int(s^.stream.avail_in));
      {$I+}
      if (s^.stream.avail_in = 0) then begin
        s^.z_eof := true;
        if (IOResult <> 0) then begin
          s^.z_err := Z_ERRNO;
          break;
        end;
      end;
      s^.stream.next_in := s^.inbuf;
    end;

    s^.z_err := inflate(s^.stream, Z_NO_FLUSH);

    if (s^.z_err = Z_STREAM_END) then begin
      crclen := 0;
      next_out := s^.stream.next_out;
      while (next_out <> start ) do begin
        dec (next_out);
        inc (crclen);   { Hack because Pascal cannot substract pointers }
      end;
      { Check CRC and original size }
      s^.crc := crc32(s^.crc, start, crclen);
      start := s^.stream.next_out;

      filecrc := getLong (s);
      filelen := getLong (s);

      if (s^.crc <> filecrc) or (s^.stream.total_out <> filelen)
        then s^.z_err := Z_DATA_ERROR
        else begin
          { Check for concatenated .gz files: }
          check_header(s);
          if (s^.z_err = Z_OK) then begin
            total_in := s^.stream.total_in;
            total_out := s^.stream.total_out;

            inflateReset (s^.stream);
            s^.stream.total_in := total_in;
            s^.stream.total_out := total_out;
            s^.crc := crc32 (0, Z_NULL, 0);
          end;
      end; {IF-THEN-ELSE}
    end;

    if (s^.z_err <> Z_OK) or (s^.z_eof = true) then break;

  end; {WHILE}

  crclen := 0;
  next_out := s^.stream.next_out;
  while (next_out <> start ) do begin
    dec (next_out);
    inc (crclen);   { Hack because Pascal cannot substract pointers }
  end;
  s^.crc := crc32 (s^.crc, start, crclen);

  gzread := int(len - s^.stream.avail_out);

end;


{ GZGETC ====================================================================

  Reads one byte from the compressed file.
  gzgetc returns this byte or -1 in case of end of file or error.

============================================================================}

function gzgetc (f:gzfile) : int;

var c:byte;

begin

  if (gzread (f,@c,1) = 1) then gzgetc := c else gzgetc := -1;

end;


{ GZGETS ====================================================================

  Reads bytes from the compressed file until len-1 characters are read,
  or a newline character is read and transferred to buf, or an end-of-file
  condition is encountered. The string is then Null-terminated.

  gzgets returns buf, or Z_NULL in case of error.
  The current implementation is not optimized at all.

============================================================================}

function gzgets (f:gzfile; buf:PChar; len:int) : PChar;

var

  b      : PChar; { start of buffer }
  bytes  : Int;   { number of bytes read by gzread }
  gzchar : char;  { char read by gzread }

begin

    if (buf = Z_NULL) or (len <= 0) then begin
      gzgets := Z_NULL;
      exit;
    end;

    b := buf;
    repeat
      dec (len);
      bytes := gzread (f, buf, 1);
      gzchar := buf^;
      inc (buf);
    until (len = 0) or (bytes <> 1) or (gzchar = Chr(13));

    buf^ := Chr(0);
    if (b = buf) and (len > 0) then gzgets := Z_NULL else gzgets := b;

end;


{$IFNDEF NO_DEFLATE}

{ GZWRITE ===================================================================

  Writes the given number of uncompressed bytes into the compressed file.
  gzwrite returns the number of uncompressed bytes actually written
  (0 in case of error).

============================================================================}

function gzwrite (f:gzfile; buf:voidp; len:uInt) : int;

var

  s : gz_streamp;
  written : integer;

begin

    s := gz_streamp(f);

    if (s = NIL) or (s^.mode <> 'w') then begin
      gzwrite := Z_STREAM_ERROR;
      exit;
    end;

    s^.stream.next_in := pBytef(buf);
    s^.stream.avail_in := len;

    while (s^.stream.avail_in <> 0) do begin

      if (s^.stream.avail_out = 0) then begin
        s^.stream.next_out := s^.outbuf;
        blockwrite (s^.gzfile, s^.outbuf^, Z_BUFSIZE, written);
        if (written <> Z_BUFSIZE) then begin
          s^.z_err := Z_ERRNO;
          break;
        end;
        s^.stream.avail_out := Z_BUFSIZE;
      end;

      s^.z_err := deflate(s^.stream, Z_NO_FLUSH);
      if (s^.z_err <> Z_OK) then break;

    end; {WHILE}

    s^.crc := crc32(s^.crc, buf, len);
    gzwrite := int(len - s^.stream.avail_in);

end;


{ ===========================================================================
   Converts, formats, and writes the args to the compressed file under
   control of the format string, as in fprintf. gzprintf returns the number of
   uncompressed bytes actually written (0 in case of error).
}

{$IFDEF GZ_FORMAT_STRING}
function gzprintf (zfile : gzFile;
                   const format : string;
                   a : array of int) : int;
var
  buf : array[0..Z_PRINTF_BUFSIZE-1] of char;
  len : int;
begin
{$ifdef HAS_snprintf}
    snprintf(buf, sizeof(buf), format, a1, a2, a3, a4, a5, a6, a7, a8,
             a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
{$else}
    sprintf(buf, format, a1, a2, a3, a4, a5, a6, a7, a8,
            a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
{$endif}
    len := strlen(buf); { old sprintf doesn't return the nb of bytes written }
    if (len <= 0) return 0;

    gzprintf := gzwrite(file, buf, len);
end;
{$ENDIF}


{ GZPUTC ====================================================================

  Writes c, converted to an unsigned char, into the compressed file.
  gzputc returns the value that was written, or -1 in case of error.

============================================================================}

function gzputc (f:gzfile; c:char) : int;
begin
  if (gzwrite (f,@c,1) = 1) then
  {$IFDEF FPC}
    gzputc := int(ord(c))
  {$ELSE}
    gzputc := int(c)
  {$ENDIF}
  else
    gzputc := -1;
end;


{ GZPUTS ====================================================================

  Writes the given null-terminated string to the compressed file, excluding
  the terminating null character.
  gzputs returns the number of characters written, or -1 in case of error.

============================================================================}

function gzputs (f:gzfile; s:PChar) : int;
begin
  gzputs := gzwrite (f, voidp(s), strlen(s));
end;


{ DO_FLUSH ==================================================================

  Flushes all pending output into the compressed file.
  The parameter flush is as in the zdeflate() function.

============================================================================}

function do_flush (f:gzfile; flush:int) : int;
var
  len     : uInt;
  done    : boolean;
  s       : gz_streamp;
  written : integer;
begin
  done := false;
  s := gz_streamp(f);

  if (s = NIL) or (s^.mode <> 'w') then begin
    do_flush := Z_STREAM_ERROR;
    exit;
  end;

  s^.stream.avail_in := 0; { should be zero already anyway }

  while true do begin

    len := Z_BUFSIZE - s^.stream.avail_out;

    if (len <> 0) then begin
      {$I-}
      blockwrite(s^.gzfile, s^.outbuf^, len, written);
      {$I+}
      if (written <> len) then begin
        s^.z_err := Z_ERRNO;
        do_flush := Z_ERRNO;
        exit;
      end;
      s^.stream.next_out := s^.outbuf;
      s^.stream.avail_out := Z_BUFSIZE;
    end;

    if (done = true) then break;
    s^.z_err := deflate(s^.stream, flush);

    { Ignore the second of two consecutive flushes: }
    if (len = 0) and (s^.z_err = Z_BUF_ERROR) then s^.z_err := Z_OK;

    { deflate has finished flushing only when it hasn't used up
      all the available space in the output buffer: }

    done := (s^.stream.avail_out <> 0) or (s^.z_err = Z_STREAM_END);
    if (s^.z_err <> Z_OK) and (s^.z_err <> Z_STREAM_END) then break;

  end; {WHILE}

  if (s^.z_err = Z_STREAM_END) then do_flush:=Z_OK else do_flush:=s^.z_err;
end;

{ GZFLUSH ===================================================================

  Flushes all pending output into the compressed file.
  The parameter flush is as in the zdeflate() function.

  The return value is the zlib error number (see function gzerror below).
  gzflush returns Z_OK if the flush parameter is Z_FINISH and all output
  could be flushed.

  gzflush should be called only when strictly necessary because it can
  degrade compression.

============================================================================}

function gzflush (f:gzfile; flush:int) : int;
var
  err : int;
  s   : gz_streamp;
begin
  s := gz_streamp(f);
  err := do_flush (f, flush);

  if (err <> 0) then begin
    gzflush := err;
    exit;
  end;

  if (s^.z_err = Z_STREAM_END) then gzflush := Z_OK else gzflush := s^.z_err;
end;

{$ENDIF} (* NO DEFLATE *)


{ GZREWIND ==================================================================

  Rewinds input file.

============================================================================}

function gzrewind (f:gzFile) : int;
var
  s:gz_streamp;
begin
  s := gz_streamp(f);

  if (s = NIL) or (s^.mode <> 'r') then begin
    gzrewind := -1;
    exit;
  end;

  s^.z_err := Z_OK;
  s^.z_eof := false;
  s^.stream.avail_in := 0;
  s^.stream.next_in := s^.inbuf;

  if (s^.startpos = 0) then begin { not a compressed file }
    {$I-}
    seek (s^.gzfile, 0);
    {$I+}
    gzrewind := 0;
    exit;
  end;

  inflateReset(s^.stream);
  {$I-}
  seek (s^.gzfile, s^.startpos);
  {$I+}
  gzrewind := int(IOResult);
  exit;
end;


{ GZSEEK ====================================================================

  Sets the starting position for the next gzread or gzwrite on the given
  compressed file. The offset represents a number of bytes from the beginning
  of the uncompressed stream.

  gzseek returns the resulting offset, or -1 in case of error.
  SEEK_END is not implemented, returns error.
  In this version of the library, gzseek can be extremely slow.

============================================================================}

function gzseek (f:gzfile; offset:z_off_t; whence:int) : z_off_t;
var
  s : gz_streamp;
  size : uInt;
begin
  s := gz_streamp(f);

  if (s = NIL) or (whence = SEEK_END) or (s^.z_err = Z_ERRNO)
  or (s^.z_err = Z_DATA_ERROR) then begin
    gzseek := z_off_t(-1);
    exit;
  end;

  if (s^.mode = 'w') then begin
{$IFDEF NO_DEFLATE}
    gzseek := z_off_t(-1);
    exit;
{$ELSE}
    if (whence = SEEK_SET) then dec(offset, s^.stream.total_out);
    if (offset < 0) then begin;
      gzseek := z_off_t(-1);
      exit;
    end;

    { At this point, offset is the number of zero bytes to write. }
    if (s^.inbuf = Z_NULL) then begin
      GetMem (s^.inbuf, Z_BUFSIZE);
      zmemzero(s^.inbuf, Z_BUFSIZE);
    end;

    while (offset > 0) do begin
      size := Z_BUFSIZE;
      if (offset < Z_BUFSIZE) then size := uInt(offset);

      size := gzwrite(f, s^.inbuf, size);
      if (size = 0) then begin
        gzseek := z_off_t(-1);
        exit;
      end;

      dec (offset,size);
    end;

    gzseek := z_off_t(s^.stream.total_in);
    exit;
{$ENDIF}
  end;
  { Rest of function is for reading only }

  { compute absolute position }
  if (whence = SEEK_CUR) then inc (offset, s^.stream.total_out);
  if (offset < 0) then begin
    gzseek := z_off_t(-1);
    exit;
  end;

  if (s^.transparent = true) then begin
    s^.stream.avail_in := 0;
    s^.stream.next_in := s^.inbuf;
    {$I-}
    seek (s^.gzfile, offset);
    {$I+}
    if (IOResult <> 0) then begin
      gzseek := z_off_t(-1);
      exit;
    end;

    s^.stream.total_in := uLong(offset);
    s^.stream.total_out := uLong(offset);
    gzseek := z_off_t(offset);
    exit;
  end;

  { For a negative seek, rewind and use positive seek }
  if (uLong(offset) >= s^.stream.total_out)
    then dec (offset, s^.stream.total_out)
    else if (gzrewind(f) <> 0) then begin
      gzseek := z_off_t(-1);
      exit;
  end;
  { offset is now the number of bytes to skip. }

  if (offset <> 0) and (s^.outbuf = Z_NULL)
  then GetMem (s^.outbuf, Z_BUFSIZE);

  while (offset > 0) do begin
    size := Z_BUFSIZE;
    if (offset < Z_BUFSIZE) then size := int(offset);

    size := gzread (f, s^.outbuf, size);
    if (size <= 0) then begin
      gzseek := z_off_t(-1);
      exit;
    end;
    dec(offset, size);
  end;

  gzseek := z_off_t(s^.stream.total_out);
end;


{ GZTELL ====================================================================

  Returns the starting position for the next gzread or gzwrite on the
  given compressed file. This position represents a number of bytes in the
  uncompressed data stream.

============================================================================}

function gztell (f:gzfile) : z_off_t;
begin
  gztell := gzseek (f, 0, SEEK_CUR);
end;


{ GZEOF =====================================================================

  Returns TRUE when EOF has previously been detected reading the given
  input stream, otherwise FALSE.

============================================================================}

function gzeof (f:gzfile) : boolean;
var
  s:gz_streamp;
begin
  s := gz_streamp(f);

  if (s=NIL) or (s^.mode<>'r') then
    gzeof := false
  else
    gzeof := s^.z_eof;
end;


{ PUTLONG ===================================================================

  Outputs a Longint in LSB order to the given file

============================================================================}

procedure putLong (var f:file; x:uLong);
var
  n : int;
  c : byte;
begin
  for n:=0 to 3 do begin
    c := x and $FF;
    blockwrite (f, c, 1);
    x := x shr 8;
  end;
end;


{ GZCLOSE ===================================================================

  Flushes all pending output if necessary, closes the compressed file
  and deallocates all the (de)compression state.

  The return value is the zlib error number (see function gzerror below).

============================================================================}

function gzclose (f:gzFile) : int;
var
  err : int;
  s   : gz_streamp;
begin
  s := gz_streamp(f);
  if (s = NIL) then begin
    gzclose := Z_STREAM_ERROR;
    exit;
  end;

  if (s^.mode = 'w') then begin
{$IFDEF NO_DEFLATE}
    gzclose := Z_STREAM_ERROR;
    exit;
{$ELSE}
    err := do_flush (f, Z_FINISH);
    if (err <> Z_OK) then begin
      gzclose := destroy (gz_streamp(f));
      exit;
    end;

    putLong (s^.gzfile, s^.crc);
    putLong (s^.gzfile, s^.stream.total_in);
{$ENDIF}
  end;

  gzclose := destroy (gz_streamp(f));
end;


{ GZERROR ===================================================================

  Returns the error message for the last error which occured on the
   given compressed file. errnum is set to zlib error number. If an
   error occured in the file system and not in the compression library,
   errnum is set to Z_ERRNO and the application may consult errno
   to get the exact error code.

============================================================================}

function gzerror (f:gzfile; var errnum:int) : string;
var
 m : string;
 s : gz_streamp;
begin
  s := gz_streamp(f);
  if (s = NIL) then begin
    errnum := Z_STREAM_ERROR;
    gzerror := zError(Z_STREAM_ERROR);
    end;

  errnum := s^.z_err;
  if (errnum = Z_OK) then begin
    gzerror := zError(Z_OK);
    exit;
  end;

  m := s^.stream.msg;
  if (errnum = Z_ERRNO) then m := '';
  if (m = '') then m := zError(s^.z_err);

  s^.msg := s^.path+': '+m;
  gzerror := s^.msg;
end;

procedure UnGZip (var lInFname: string; var lBuf: ByteP; lOffset,lMaxSz: integer); //unzip
const
BUFLEN = 16384;
var
	infile : gzFile;
 lFname : ansistring;
  lbufsz,len,lI     : integer;
  written : integer;
  buf  : packed array [0..BUFLEN-1] of byte; { Global uses BSS instead of stack }
begin
     lFName := lInFName;
  //filemode := 1;
  //if lFName = 'z' then
   //showmessage('unzip');
  //ImgForm.Caption := 'gz';
  //ReadIntForm.GetInt('Multi-volume file, please select volume to view.',1,1,3);
  //infile := gzopenZ (lFName, 'r', 0);
  infile := gzopen (lFName, 'r');
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
           break;
        end;
		Move(buf,lbuf^[Written+1],len);
		Written := Written + len;
  end; {WHILE}
  gzclose (infile);
  //filemode := 2;
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

function gz_compress (var infile:file; outfile:gzFile): integer;
var
  len   : cardinal;
  ioerr : integer;
  buf  : packed array [0..Z_BUFSIZE-1] of byte; { Global uses BSS instead of stack }
  errorcode : byte;
  fsize, lensize : DWord;
begin
  errorcode := 0;
  //Progress := 0;
  fsize := FileSize(infile);
  lensize := 0;
  //if FProgressStep > 0 then DoOnProgress;
  while true do begin
	{$I-}blockread (infile, buf, Z_BUFSIZE, len);{$I+}
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
  end;
  closeFile (infile);
  if (gzclose (outfile) <> 0{Z_OK}) then errorcode := 3;
  gz_compress := errorcode;
end; // proc gz_compress

procedure GZipFile(lSrcName,lDestName: String);
var
   FGzipFilename : string;
   FGzipComments : string;
   outmode : string;
	s,FFileDestination : string;
	infile  : file;
	outfile : gzFile;
	FCompressionLevel{,errorcode} : integer;
	flags : Integer;
	stream : gz_streamp;
	//p : PChar;
	ioerr : integer;
begin
//FGzipHeader := [zFilename];
FGzipFilename:= lSrcName;
FGzipComments := '';
 FCompressionLevel := 6;
//MainForm.ProgressBar1.position :=1;
//Gzip (lFile,lMulti);
 FFileDestination := lDestName;
 //result := 2; //return error if user aborts
(* if fileexists(FFileDestination) then begin
        case MessageDlg('Overwrite the file '+FFileDestination+'?', mtConfirmation,[mbYes, mbAbort], 0) of	{ produce the message dialog box }
             mrAbort: exit;
        end;
 end;*)
  AssignFile (infile, lSrcName);
  {$I-}
  Reset (infile,1);
  {$I+}
  ioerr := IOResult;
  if (ioerr <> 0) then begin
	//	 Showmessage('Can''t open: '+lSrcName);
	//errorcode := 1
  end
  else begin
	  outmode := 'w  ';
	  //s := IntToStr(FCompressionLevel);
	  outmode[2] := '6';//s[1];
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
	  outfile := gzopen (lSrcName, outmode);
	  if (outfile = NIL) then begin
	     //Showmessage('Can''t open: '+lSrcName);
	     close( infile);
             exit;
	  end
	  else begin
		 { if flags are set then write them }
		 stream := gz_streamp(outfile);
		 if {(zfilename in FGzipHeader)} true then begin
                        s := lSrcName;//999 ExtractFilename(lSrcName);
			//p := PChar(s);
			blockWrite( stream^.gzfile, {p[0]}s, length(s)+1);
			stream^.startpos := stream^.startpos + length(s) + 1
		 end;
                 gz_compress(infile, outfile);
	  end
   end;
end;

procedure file_compress2 (filename,outname:string);
var
  infile  : file;
  outfile : gzFile;
  ioerr   : integer;
  mode : string;
begin
  mode := 'w6 ';
  Assign (infile, filename);
  {$I-}
  Reset (infile,1);
  {$I+}
  ioerr := IOResult;
  if (ioerr <> 0) then begin
    writeln ('open error: ',ioerr);
    halt(1);
  end;
  outfile := gzopen (outname, mode);
  if (outfile = NIL) then begin
    //999 showmessage(' can''t gzopen '+outname);
    halt(1);
  end;

  gz_compress(infile, outfile);
  erase (infile);
end;


procedure GZipBuffer(var FGzipFilename,FFileDestination: String;lxInBuffer: byteP;lInSize: Integer; lOverwritewarn: boolean);
var
   lTempName: string;
    lFdata: file;
begin
 (*if lOverwritewarn and fileexists(FFileDestination) then begin
    case MessageDlg('Overwrite the file '+FFileDestination+'?', mtConfirmation,[mbYes, mbAbort], 0) of	{ produce the message dialog box }
         mrAbort: exit;
    end;
  end; //if overwrite  *)
  lTempName := changefileext(FFileDestination,'.tmp');
  assignfile(lFdata,lTempName );
  filemode := 2;
  rewrite(lFdata,1);
  BlockWrite(lFdata,lxInBuffer^,lInSize);
  closefile(lFdata);
  file_compress2 (lTempName,FFileDestination );

  //GZipFile(lTempName,FFileDestination);
  //deletefile(lTempname);
end;//GZipBuffer

end.
