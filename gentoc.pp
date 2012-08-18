program gentoc;
// FPC script to generate a simple overall TOC chm from a bunch of chms
// for use in the textmode ide.
// (C) Marco van de Voort 2009 BSD license (no advocacy)

{$ifdef fpc}
{$mode delphi}
{$else}
{$apptype console}
{$endif}
{$info only works properly with 2.3.1+ of july 2009 or newer }
{$ifdef ver2_2}
   Die.
{$endif}

Uses {$ifdef unix}cthreads, {$endif} chmreader,chmfilewriter,sysutils,classes,dom_html,xmlwrite,htmwrite8859,chmbase,chmwriter,chmsitemap;

{ Index generation }

Type 
     TContextClass = class
			  Description : string;
                          defaultpage : string;
			  tocpage     : string;
			end;

// main part, recursive is not yet implemented
procedure scandir(filespec:string;recursive:boolean;fn:TStrings);

var d        : TSearchRec;
    ctxt     : TContextClass;
begin
  filespec:=includetrailingpathdelimiter(filespec);
  writeln(filespec);
  if findfirst(filespec+'*.chm',faanyfile and not fadirectory,d)=0 then
    begin
      repeat
        if (d.attr and fadirectory = fadirectory)  then
          begin
            // if recursive this needs to be fixed. E.g. for multiple chms in one.
 	        writeln('skipping '+d.name);
	      end
        else
         begin
          if d.name<>'toc.chm' THen
            begin
	      ctxt:=TContextClass.Create;
              fn.addObject(d.name,ctxt);
            end;
         end;
      until findnext(d)<>0;
     findclose(d);
    end;
end;

procedure scanchms(chmspath:string;flz:TStringlist);

var r    : TChmReader;
    fs   : TFileStream;    
    i    : integer;
    ctxt : TContextClass;

begin
  for i:=0 to flz.count-1 do
    begin
      fs:=TFileStream.create(chmspath+flz[i],fmOpenRead);
//      writeln('Reading ',chmspath+flz[i]);

        r:=TChmReader.Create(fs,True);
      try
        ctxt:=TContextClass(flz.objects[i]);
        ctxt.tocpage:=r.tocfile;
        ctxt.defaultpage:=r.defaultpage;
//        writeln(r.tocfile,' ',r.defaultpage);

      finally
        r.free;
       end;
    end;
end;

const   
    KnownNames : array [0..7] of string = ('ref',
					   'prog',
					   'user',
					   'rtl',
					   'fcl',
					   'lcl',
					   'fpdoc',
					   'lazutils'
				          );
    Descriptions  : array [0..7] of string = (
                 'Language reference Manual contents',
                 'Programmer''s guide contents',
                 'User''s guide contents',
                 'Run-Time Library (RTL) Manual contents',
                 'Free Component Library (FCL) Manual contents',
		 'Lazarus Component Library (LCL) Manual contents',
		 'FPDoc Documentation tool contents',
		 'Lazarus unils library (LazUtils) Manual contents');
    Preamble  = '<html><head></head><body><h1> Free Pascal/Lazarus documentation overview</h1><ol>';
    postamble=  '</ol></body></html>';

 
procedure gendescription(files:Tstringlist);
var
    i,j    : integer;
    ctxt : TContextClass;
    fn   : string;
begin
  for i:=0 to files.count-1 do
    begin
      fn:=changefileext(files[i],'');
      ctxt:=TContextClass(files.objects[i]);
      j:=0;
      while (j<=high(Knownnames)) and (fn<>knownnames[j]) do inc(j);  
      if j<=high(knownnames) then
        ctxt.description:=Descriptions[j]
      else
        ctxt.description:=fn+' Contents';
    end;
end;

procedure  genfile(fn:string;files:TStringList);

var f : text;
    i,j : integer;
    ctxt : TContextClass;

begin
  writeln('writing:',fn);
  assignfile(f,fn);
  rewrite(f);
  writeln(f,preamble);
  for i:=0 to Files.count-1 do
   begin
     ctxt:=TContextClass(files.objects[i]);
     writeln(f,'<li><a href="ms-its:',files[i],'::',ctxt.defaultpage,'">&nbsp;',ctxt.Description,'</a></li>');
   end;
  writeln(f,postamble);
  closefile(f);
end;

procedure usage;

begin
  Writeln('CHMgentoc "[chmspath]" "[tocchmpath]"'#13#10'   where chmspath is the dir to scan for chms, and tocchmpath is the place to write the generate CHM'#13#10);
  halt;
end; 

var chmspath, 
    tocchmpath : string;
    x 	       : TCHMProject;
    f 	       : TFileStream;
    files      : TStringList; 
    i 	       : integer;
    tocpath,
    tmppath    : String;
begin
 chmspath:='.';
 tocchmpath:='.'; 
  if paramcount>0 then
    chmspath:=paramstr(1);
  if paramcount>1 then
    tocchmpath:=paramstr(2);
  tocchmpath:=expandfilename(tocchmpath);
  if (chmspath<>'') and not directoryexists(chmspath) then
    usage;
 
  tmppath:=includetrailingpathdelimiter(gettempdir(false));
  
  tocpath:=tmppath+'toc';
  forcedirectories(tocpath);
  tocpath:=includetrailingpathdelimiter(tocpath);
   
  tocchmpath:=includetrailingpathdelimiter(tocchmpath);
  chmspath:=includetrailingpathdelimiter(chmspath);
  files:=TStringList.create;
  scandir(chmspath,false,files);  // make list of chms.
  scanchms(chmspath,files);       // scan them for defaultpage/tocfile path/name
  gendescription(files);
  chdir(tmppath);
  genfile(tocpath+'toc.html',files);
  x := TCHMProject.create;
  x.MakeSearchable:=true;
  x.OutputFilename:=tocchmpath+'toc.chm';
  x.Defaultpage:='toc/toc.html';
  x.Title:='Table of Contents';
  x.files.add('toc/toc.html');
// xml stuff doesn't seem to work ?
//  x.savetofile(tocchmpath+'proj.xml');
    
  f:=TFileStream.Create(tocchmpath+'toc.chm',fmcreate);
  x.writechm(f);
  x.free;
  f.free;
end.