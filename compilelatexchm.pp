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

Uses cwstring,cthreads,chmfilewriter,Sax_HTML,dom,sysutils,classes,dom_html,xmlwrite,htmwrite8859,chmbase,chmwriter,chmsitemap;

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

function sectiontype (s:string):integer;
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
          cur.text:=s+' '+name;
          cur.local:=prefixpath+href;
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
  chsitemap[0].text:='Contents';
  chsitemap[0].local:=prefixpath+prefix+'.html';
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

function TOCSort(Item1, Item2: TChmSiteMapItem): Integer;
begin
  Result := CompareText(LowerCase(Item1.Text), LowerCase(Item2.Text));
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
          TmpItem.Text := t[i];
	  chd:=TStringList(t.objects[i]);  
          tmpitem.local:=prefixpath+chd[0];
          if chd.count>1 then
            begin 
              for j:=0 to chd.count-1 do
                begin
                  chditem:=TmpItem.Children.NewItem;  
		  chditem.Text:=inttostr(j+1);
		  chditem.local:=prefixpath+chd[j];
                end;
            end;
        end; 
      Index.Items.Sort(TListSortCompare(@TOCSort));
      Index.SaveToStream(Stream);
      Index.Free;
      Stream.Free;
  end;
end;

var x : TCHMProject;
    f : TFileStream;
     
begin
  if paramcount<2 then
    usage;
  prefix:=paramstr(1);
  description:=paramstr(2);
  if paramcount>=3 then
   kwdfile:=paramstr(3);
  if not directoryexists(prefix) then
    usage;
  prefixpath:=includetrailingpathdelimiter(prefix);
   
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
  readindex(x.defaultpage,x.TableOfContentsFileName);
  if kwdfile<>'' then
     processkwd(x,kwdfile);
// xml stuff doesn't seme to work ?
  x.savetofile(prefix+'proj.xml');
    
  f:=TFileStream.Create(prefix+'.chm',fmcreate);
  x.writechm(f);
  x.free;
  f.free;
end.