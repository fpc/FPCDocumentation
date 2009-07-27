program compilelatexchm;
// FPC script to compile latex manuals html to chm.
// (C) Marco van de Voort 2009
// should get indexing and toc in the future. (output of relink?)

uses SysUtils,Classes,chmfilewriter;

procedure scandir(filespec:string;recursive:boolean;fn:TStrings);

var d : TSearchRec;
    lfilespec : string;
begin
  filespec:=excludetrailingpathdelimiter(filespec);
  lfilespec:=filespec+'/';
  filespec:=includetrailingpathdelimiter(filespec);
  if findfirst(filespec+'*',faanyfile and fadirectory,d)=0 then
    begin
      repeat
        if (d.attr and fadirectory = fadirectory)  then
        begin
          // if recursive
	  writeln('skipping '+d.name);
	end
        else
         begin
          fn.add(lfilespec+d.name);
          writeln(filespec+d.name);
         end;
      until findnext(d)<>0;
     findclose(d);
    end;
end;

procedure usage;

begin
  Writeln('CHMCreate [prefix] "[Description]"'#13#10'   where prefix is prog,ref or user'#13#10);
  halt;
end; 

var x : TCHMProject;
    f : TFileStream;
    prefix : string;
    prefixpath :string;
    description:string;
begin
  if paramcount<2 then
    usage;
  prefix:=paramstr(1);
  description:=paramstr(2);
  if not directoryexists(prefix) then
    usage;
  prefixpath:=includetrailingpathdelimiter(prefix);
   
  x := TCHMProject.create;

  x.OutputFilename:=prefix+'.chm';
  x.Defaultpage:=prefixpath+prefix+'.html';
  x.Title:=description;
  x.IndexFileName:='default.hhk';

//  x.projectdir:='.';
  scandir(prefix,false,x.files);
// xml stuff doesn't seme to work ?
  x.savetofile(prefix+'proj.xml');
  f:=TFileStream.Create(prefix+'.chm',fmcreate);
  x.writechm(f);
  x.free;
  f.free;
end.