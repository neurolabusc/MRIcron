unit filename;

interface
{$H+}

function LegitFilename(var lInName: string; lIndex: integer): string;


implementation
uses SysUtils;

function LegitFilename(var lInName: string; lIndex: integer): string;
var
   I: integer;
begin
    if length(lInName) < 1 then begin
        result := inttostr(lIndex);
        exit;
    end;
    result := '';
    for I := 1 to length(lInName) do
        if lInName[I] in [ '0'..'9','a'..'z','A'..'Z'] then
           result := result + lInName[I];
    if length(result) < 1 then
        result := inttostr(lIndex);
end;

end.
