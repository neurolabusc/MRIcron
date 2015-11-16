unit associate;
interface
uses Windows,registry,Forms,dialogs,SysUtils;

function registerfiletype(inft,inkey,desc,icon:string): boolean;

implementation

function registerfiletype(inft,inkey,desc,icon:string): boolean;
var myreg : treginifile;
    ct : integer;
    ft,key: string;
begin
     result := true;
     ft := inft;
     key := inkey;
     ct := pos('.',ft);
     while ct > 0 do begin
           delete(ft,ct,1);
           ct := pos('.',ft);
     end;
     if (ft = '') or (Application.ExeName = '') then exit; //not a valid file-ext or ass. app
     ft := '.'+ft;
     myreg := treginifile.create('');
     try
        myreg.rootkey := hkey_classes_root; // where all file-types are described
        if key = '' then key := copy(ft,2,maxint)+'_auto_file'; // if no key-name is given, create one
        myreg.writestring(ft,'',key); // set a pointer to the description-key
        myreg.writestring(key,'',desc); // write the description
        myreg.writestring(key+'\DefaultIcon','',icon); // write the def-icon if given
        //showmessage(key);
        myreg.writestring(key+'\shell\open\command','',Application.ExeName+' %1'); //association
     except
           result := false;
           showmessage('Only administrators can change file associations. You are currently logged in as a restricted user.');
     end;
     //finally
            myreg.free;
     //end;
end;

end.