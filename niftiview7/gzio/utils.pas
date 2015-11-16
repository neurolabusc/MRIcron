unit utils;

interface

const VERSION = '0.2.99';
      BUFLEN = 16384;

type
  TUtils = class
  private
	{ Private declarations }
  protected
	{ Protected declarations }
  public
	{ Public declarations }
	function HexToStr(w : longint) : string;
	function CreateAboutMsg(componentName : string) : string;
  published
	{ Published declarations }
  end;

implementation

function TUtils.HexToStr(w : longint) : string;
const
   ByteToChar : array[0..$F] of char ='0123456789ABCDEF';
var s : string;
	i : integer;
	x : longint;
begin
   s := '';
   x := w;
   for i := 0 to 3 do
   begin
	  s := ByteToChar[Byte(x) shr 4] + ByteToChar[Byte(x) and $F] + s;
	  x := x shr 8;
   end;
   HexToStr := s;
end;

function TUtils.CreateAboutMsg(componentName : string) : string;
const CR = chr(13);
var Msg : string;
begin
  Msg := componentName+' component '+version+CR+CR;
  Msg:=Msg+'Copyright © 2000 Vincent Nikkelen.'+CR+CR;
  Msg:=Msg+'Do not thank me for this component, please'+CR;
  Msg:=Msg+'thank Jean-loup Gailly and Mark Adler for'+CR;
  Msg:=Msg+'the zlib-library and Jacques Nomssi Nzali'+CR;
  Msg:=Msg+'for the Pascal translation.'+CR+CR;
  Msg:=Msg+'Pleas read the README.TXT that comes with'+CR;
  Msg:=Msg+'this component.'+CR+CR;
  Msg:=Msg+'http://www.cdrom.com/pub/infozip/zlib/'+CR;
  Msg:=Msg+'http://www.tu-chemnitz.de/~nomssi/paszlib.html'+CR;
  Msg:=Msg+'http://www.stack.nl/~vincentn/delphizlib/';
  CreateAboutMsg := Msg
end;

end.
