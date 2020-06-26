program compilelatexchm;
// FPC script to compile latex manuals html to chm.
// (C) Marco van de Voort 2009 BSD license (no advocacy)
//
//   Requires Sergei's work on dom_html, which means trunk from after (mainly r13357
//   but maybe also one of the slightly later commits)
//  Rule of thumb: use version 2.3.1 of the same date or newer as the last commit to this file
//
//  1.0  2009-07-27 initial version
//  1.1  2009-07-28 Initial TOC support and fulltextsearch


{$ifdef fpc}
{$mode delphi}
{$else}
{$apptype console}
{$endif}
{$info only works properly with 2.3.1+ of july 2009 or newer }
{$ifdef ver2_2}
   Die.
{$endif}

Uses {$ifdef unix}cwstring,cthreads,{$endif} chmfilewriter,Sax_HTML,dom,sysutils,classes,dom_html,xmlwrite,htmwrite8859,chmbase,chmwriter,chmsitemap,strutils;

{ Index generation }

const
      attribstringsconstants : Array [0..3] of domstring =
                       ('sectionToc','appendixToc','subsectionToc','chapterToc');
      attriblevels           : array [0..3] of integer   =
                        (1,0,2,0);

var
    ChSitemap  : Array[0..3] of TCHMSiteMapItem =(nil,nil,nil,nil);
    toc        : TChmSiteMap;
    curlevel   : integer =-1;
    prefix     : string;
    prefixpath : string;
    kwdfile    : string ='';
    description: string;
    placeholdervalue : integer =1;

    lastsectionheading : string ='';
    scanningfile : string = '';
    labellist : Tstringlist;

Type

{ TStringClass }

 TStringClass = class
                      fstr : string;
                      constructor create(const astr:string);
                      end;

 { TStringClass }

 constructor TStringClass.create(const astr: string);
 begin
   fstr:=astr;
 end;

function sectiontype (const s:string):integer;
var i : integer;

begin
  i:=0;
  while (i<=high(attribstringsconstants)) and (attribstringsconstants[i]<>s) do
   inc(i);
  if i>high(attribstringsconstants) then
    result:=-1
  else
    result:=attriblevels[i];
end;

procedure recursivelygettext(p:TDomNode;var s:domstring);

var p2:TDomNode;
begin
  if not assigned(p) then exit;
  s:=s+p.nodevalue;
  if p.haschildnodes then
    begin
      p2:=p.firstchild;
      while assigned(p2) do
        begin
          recursivelygettext(p2,s);
          p2:=p2.nextsibling;
        end;
    end;
end;

function parsechildren(p:TDomNode;var href:domstring;var name:domstring):DOMString;

var p2 : tdomnode;
begin
  result:='';  href:=''; name:='';
  if assigned(p.FirstChild) then
    begin
      result:=p.firstchild.NodeValue;
      p2:=p.FirstChild.NextSibling;
      if assigned(p2) then
        begin
          href:=tdomelement(p2).attribstrings['href'];
          recursivelygettext(p2,name);
          {if assigned(p2.firstchild) then
            name:=tdomelement(p2.firstchild).nodevalue;}
        end;
    end;
end;

procedure scannodesections(p:TDomNode;lvl:integer;lst:tstringlist);

var c:TDomNode;
    s,title,classnm:domstring;

begin
  if p is TDomElement then s:=TDomElement(p).tagname else s:='';

  // cache last section header.
  if (s='h1') or (s='h2')  or (s='h3')  or (s='h4') then
    begin
      classnm:=tdomelement(p).attribstrings['class'];
      if endstext('Head',classnm) then
        begin
          c:=p.FirstChild;
          while assigned(c) do
            begin
              if c is TDomElement then s:=TDomElement(c).tagname else s:='';
              if s='a' then
                begin
                 recursivelygettext(p,title);
                 if startstext('Chapter',title) then
                    delete(title,1,8);
                 title:=TrimLeftSet(title,['0'..'9','.',' ']);

                 if title<>'' then
                   lastsectionheading:=title;
                end;
              c:=c.NextSibling;
            end;
         end;
     end;
  // label defined ? -> store it in a stringlist as <filename#tagname, sectionname>
  if s='a' then
     begin
      classnm:=tdomelement(p).attribstrings['name'];
      if StartsText('keyword_',classnm) then
         labellist.AddObject(lowercase(scanningfile+'#'+classnm),TStringClass.create(lastsectionheading));
     end;

  if s='img' then
     begin
       classnm:=tdomelement(p).attribstrings['src'];
       if classnm<>'' then
          lst.add(classnm);
     end;
  c:=p.FirstChild;
  while assigned(c) do
    begin
      scannodesections(c,lvl+1,lst);
      c:=c.NextSibling;
    end;
end;


procedure fixpicdoubleslash(fn:string);
var m,m2: TStringStream;
    wholefile : string;
begin
  m:=TStringStream.create;
  m.LoadFromFile(fn);
  wholefile:=stringreplace(m.datastring,'pics//','pics/',[rfReplaceAll]);
  m.free;
  m:=TStringStream.create(wholefile);
  m.SaveToFile(fn);
  m.free;

end;

procedure scanfileforlabels(fn:string;imglist:tstringlist);
var sx:THtmlDocument;

begin
  sx:=THtmlDocument.create;
  ReadHtmlFile(sx,fn);
  scanningfile:=extractfilename(fn);
  scannodesections(sx,0,imglist);
  sx.free;
end;


procedure loadcur(SC:integer);
var j: integer;
begin
  j:=0;
  while j<=sc do
    begin
      if not assigned(chsitemap[j]) then
            begin
                  if j=0 then
           chsitemap[j]:=TOC.Items.NewItem
          else
           chsitemap[j]:=chsitemap[j-1].children.NewItem;
                  chsitemap[j].text:='Placeholder '+inttostr(placeholdervalue);
                  inc(placeholdervalue);
                end;
          inc(j);
    end;
end;

function makeforwardslash(const s : string):string;

begin
  result:=StringReplace(s,'\','/',[rfReplaceAll]);
end;

procedure print(p:TDomNode;lvl:integer);

var c:TDomNode;
    name,s,href:domstring;
    sc :integer;
    cur : TCHMSitemapitem;
begin
  if p is TDomElement then s:=TDomElement(p).tagname else s:='';

  if s='span' then
    begin
      sc:=sectiontype(tdomelement(p).attribstrings['class']);
      if sc<>-1 then
        begin
          if sc=0 then
            chsitemap[sc]:=TOC.Items.NewItem
          else
            if not assigned(chsitemap[sc-1]) then
              loadcur(sc)
            else
              chsitemap[sc]:=chsitemap[sc-1].children.NewItem;
          cur:=chsitemap[sc];
          s:=parsechildren(p,href,name);
          {$ifdef chmdebug}
            writeln(curlevel:5,sc:5,' ',s);
          {$endif}
          cur.addname(s+' '+name);
          cur.addlocal(makeforwardslash(prefixpath+href));
          curlevel:=sc;
        end;
        end;
  c:=p.FirstChild;
  while assigned(c) do
    begin
      print(c,lvl+1);
      c:=c.NextSibling;
    end;
end;

procedure readindex(fn:string;tocfilename:string);
var
    sx : THTMLDocument;
    I : integer;
      f : TFileStream;

begin
  Toc := TChmSiteMap.Create(stTOC);
  chsitemap[0]:=TOC.Items.NewItem;
  chsitemap[0].addname('Contents');
  chsitemap[0].addlocal(makeforwardslash(prefixpath+prefix+'.html'));
  f:=TFileStream.Create(tocfilename,fmcreate);
  sx:=THtmlDocument.create;
  ReadHtmlFile(sx,fn);
  print(sx,0);
  toc.savetostream(f);
  toc.free;
  sx.free;
  f.free;
end;

// main part
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
            // if recursive this needs to be fixed. E.g. for multiple chms in one.
                writeln('skipping '+d.name);
              end
        else
         begin
          fn.add(lfilespec+d.name);
//          writeln(filespec+d.name);
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

function TOCSort(Item1, Item2: TChmSiteMapItem): Integer;
begin
  Result := CompareText(LowerCase(Item1.Text), LowerCase(Item2.Text));
end;

function searchlabel(fn : string; val : integer):string;
var j : integer;
begin
   j:=labellist.IndexOf(lowercase(fn));
   if j<>-1 then
     result:=TStringClass(labellist.objects[j]).fstr
   else
    result:=inttostr(val);
end;

procedure processkwd(x: TCHMProject;kwdfilename:string);
var t      : TStringList;
    i,j    : integer;
    Index  : TChmSiteMap;
    Stream : TFileStream;
    ChdItem,
    TmpItem: TChmSiteMapItem;
    s,s2   : String;
    f : text;
    chd : TStringList;

begin
  if fileexists(kwdfilename) then
    begin
      t:=TStringlist.create;
      t.sorted:=true;
      assignfile(f,kwdfilename);
      reset(f);
      while not EOF(F) do
        begin
          readln(f,s);
          i:=pos('=',s);
          s2:=copy(s,i+1,length(s)-i);
          delete(s,i,length(s)-i+1);
          i:=t.indexof(s);
          if i<>-1 then
            begin
              chd:=TStringList(t.objects[i]);
              chd.add(s2);
            end
          else
            begin
              chd:=TStringList.create;
              chd.sorted:=true;  chd.duplicates:=dupignore;
              chd.add(s2);
              t.addobject(s,chd);
            end;
         end;
     closefile(f);

      Index := TChmSiteMap.Create(stIndex);
      Stream := TFilestream.Create(x.indexfilename,fmcreate);
      for i:=0 to t.count-1 do
        begin
          TmpItem := Index.Items.NewItem;
          TmpItem.addname(t[i]);
          chd:=TStringList(t.objects[i]);
          tmpitem.addlocal(makeforwardslash(prefixpath+chd[0]));
          if chd.count>1 then
            begin
              for j:=0 to chd.count-1 do
                begin
                  chditem:=TmpItem.Children.NewItem;
                  chditem.addname(searchlabel(chd[j],j+1));
                  chditem.addlocal(makeforwardslash(prefixpath+chd[j]));
                end;
            end;
        end;
      Index.Items.Sort(TListSortCompare(@TOCSort));
      Index.SaveToStream(Stream);
      Index.Free;
      Stream.Free;
  end;
end;

function CopyFile(const SrcFilename, DestFilename: string;
  {Flags: TCopyFileFlags; }ExceptionOnError: Boolean): boolean;
var
  SrcHandle: THandle;
  DestHandle: THandle;
  Buffer: array[1..4096] of byte;
  ReadCount, WriteCount, TryCount: LongInt;
begin
  Result:=false;
  TryCount := 0;
  While TryCount <> 3 Do Begin
    SrcHandle := FileOpen(SrcFilename, fmOpenRead or fmShareDenyWrite);
    if THandle(SrcHandle)=feInvalidHandle then Begin
      Inc(TryCount);
      Sleep(10);
    End
    Else Begin
      TryCount := 0;
      Break;
    End;
  End;
  If TryCount > 0 Then
  begin
    if ExceptionOnError then
      raise EFOpenError.CreateFmt({SFOpenError}'Unable to open file "%s"', [SrcFilename])
    else
      exit;
  end;
try
  DestHandle := FileCreate(DestFileName);
  if (THandle(DestHandle)=feInvalidHandle) then
  begin
    if ExceptionOnError then
      raise EFCreateError.CreateFmt({SFCreateError}'Unable to create file "%s"',[DestFileName])
    else
      Exit;
  end;
  try
    repeat
      ReadCount:=FileRead(SrcHandle,Buffer[1],High(Buffer));
      if ReadCount<=0 then break;
      WriteCount:=FileWrite(DestHandle,Buffer[1],ReadCount);
      if WriteCount<ReadCount then
      begin
        if ExceptionOnError then
          raise EWriteError.CreateFmt({SFCreateError}'Unable to write to file "%s"',[DestFileName])
        else
          Exit;
      end;
    until false;
  finally
    FileClose(DestHandle);
  end;
  Result := True;
finally
  FileClose(SrcHandle);
end;
end;

procedure dupcopyfile(prefix,fn:string);
begin

  writeln('* copying ',fn ,' to ', prefix,'/',fn);
  forcedirectories(prefix+'/'+ ExtractFileDir(fn));
  copyfile(fn,prefix+'/'+ fn,true);
end;

var x : TCHMProject;
    f : TFileStream;
    i : integer;
    fn,ext : string;
    imglist : TStringList;
begin
  if paramcount<2 then
    usage;
  prefix:=paramstr(1);
  description:=paramstr(2);
  if paramcount>=3 then
   kwdfile:=paramstr(3);
  if not directoryexists(prefix) then
    usage;
  imglist :=TStringList.create;
  imglist.Sorted:=true;
  imglist.Duplicates:=dupIgnore;
  prefixpath:=includetrailingpathdelimiter(prefix);

  labellist :=Tstringlist.create; labellist.sorted:=true; labellist.OwnsObjects:=true;

  x := TCHMProject.create;
  x.MakeBinaryToc:=True;
  x.MakeSearchable:=true;
  x.OutputFilename:=prefix+'.chm';
  x.Defaultpage:=prefixpath+prefix+'.html';
  x.Title:=description;
  x.IndexFileName:=prefixpath+'default.hhk';
  x.TableOfContentsFileName:=prefixpath+'default.hhc';
  if fileexists(x.TableOfContentsFileName) then deletefile(x.TableOfContentsFileName);
  scandir(prefix,false,x.files);
  flush(output);
  writeln('Fixing pics//');
  for i:=0 to x.files.count-1 do
     fixpicdoubleslash(x.files[i]);
  writeln('Scanning for images');
  for i:=0 to x.files.count-1 do
   begin
     ext:=(extractfileext(x.files[i]));
     if startstext('.htm',ext) then
       begin
         writeln('scanning ',x.files[i],' for labels');
         scanfileforlabels(x.files[i],imglist);
       end
     else
       writeln('Data file ', x.files[i]);
   end;
  writeln('Copying misplaced pictures');
  for i:=0 to imglist.Count-1 do
    begin
      fn:=StringReplace(imglist[i],'//','/',[rfReplaceAll]);
      if not startstext(prefix,fn) then
        dupcopyfile(prefix,fn);
     x.files.add(prefix+'/'+fn);
    end;


  writeln('--- Convert toc html file to toc hhc file');
  readindex(x.defaultpage,x.TableOfContentsFileName);
  writeln('--- Convert kwd file to index');
  if kwdfile<>'' then
     processkwd(x,kwdfile);
  writeln('--- write out settings as project');

// xml stuff doesn't seme to work ?
  x.savetofile(prefix+'proj.xml');

  writeln('--- write chm');
  f:=TFileStream.Create(prefix+'.chm',fmcreate);
  x.writechm(f);
  x.free;
  f.free;
  labellist.free;
  writeln('--- Done!');
end.
