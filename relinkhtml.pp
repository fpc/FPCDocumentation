unit relinkhtml;
// hackety script using DOM_HTML to fix the URLs in FPC doc.
// Note: needs dom fixes in fcl-xml (2.3.1 of after july 2009)
//
// Warning: looking at this code might damage your eyes!
//

interface
{$ifdef fpc}
{$mode delphi}
{$endif}

{$define debugoutput}
{define printattr}
{define printchildren}
uses typinfo,classes,
     dom,SAX_Html,dom_html,xmlutils,htmwrite8859;

Type
        TNavButton = (NavNone,NavNext,NavUp,NavTail,NavPrev,NavPrevTail,navfront );
        TNavArray  = array [TNavButton] of TDomNode;
        THtmlDocFile = class // container object for file read from disk
                          public
                      title,                                     // title of paragraph/section
                      redtitle : string;                         // title with some substitutions done (appendix/chapter removed)
                      Filename:string;                           // html filename.
                      dom : THTMLDocument;
                      numbers: array[0..4] of string;            // doc hierachy splitted on the dots. ('1','1','1','' = section 1.1.1 etc)
                      padnumbers: array[0..4] of string;         // same as numbers but padded with zeroes to 5 chars. for ordering purposes.

                      children : TStringlist;                    // child nodes
                      navtable : array[0..1] of TNavArray;       // nodes in navigation table. 0=top, 1=bottom
                      Localnavtable: array[TNavButton] of THtmldocfile; // Calculated up/prev/next
                      constructor create;
                      procedure read(filenamewithpath,fn:string);
                      procedure findtitle;
                      procedure parsetitle;
                      procedure scan_nav_table(scanforward:boolean;var navtab:TNavArray);
                      function  walk(prev,up:THtmlDocFile):THtmlDocFile;
                      function  walkback(next:THtmlDocFile):THtmlDocFile;
                      function  scantablenode(root:tdomnode;var resnode:Tdomnode):TNavButton;
                      procedure navcheck;
                      procedure patch;
                     end;
        TIndexes = class     // Collection of files + indexes.
                    indexes: array[0..4] of TStringlist; // files sorted according to group
                    htmls  : TStringList;                // all files.
                    prefix  :string;                     // prog, user etc. set before running
                    procedure walk;
                    procedure walkback;
                    constructor create;
                    procedure readfiles(basedir:string);
                    procedure linkfiles;
                    procedure writehtmls(basedir:string);
                    procedure patch;
                    property chapters     :Tstringlist read indexes[0];
                    property sections     :Tstringlist read indexes[1];
                    property subsections  :Tstringlist read indexes[2];
//                    property appendices   :Tstringlist read indexes[5];
                    property toc          :Tstringlist read indexes[3];
                    property rest         :Tstringlist read indexes[4];
        end;

const NavButtonCaptions : array[TNavButton] of String =('NONE!','next','up','tail','prev','prev-tail','front');

function A_NodeToURL(node:tdomnode):String;

implementation

uses sysutils,strutils;

// Helper funcs.

function A_NodeToURL(node:tdomnode):String;
var n :TDomNode;
begin
  result:='';
  if not assigned(node.Attributes) then exit;

  n:=node.attributes.GetNamedItem('href');
  if assigned(n) then
    result:=n.NodeValue;
end;

procedure setnode_tourl(node:TDomNode;value:string);
var n :TDomNode;
begin
  if not assigned(node.Attributes) then exit;

  n:=node.attributes.GetNamedItem('href');
  if assigned(n) then
    n.NodeValue:=value;
  assert(n.NodeValue=value);
end;

procedure printattributes(node:TDomNode;title:string='');
var i :integer;
    Attributes:TDOMNamedNodeMap;
begin
  if assigned(node) then
    begin
      Attributes:=node.Attributes;
      write(title,' ');
      if assigned(attributes) and (attributes.length>0) then
        for i := 0 to Attributes.Length - 1 do
          write(Attributes.item[i].nodevalue,' ');
      writeln;
    end;
end;


function searchtag(prnt:TDomNode;tag:string):TDomNode;
// Seach first matching tag in siblings
var chld: TDomNode;
begin
  result:=nil;
  if assigned(prnt )  then
    begin
      chld:=prnt.firstchild;
      while assigned(chld) do
        begin
          if (chld is TDomElement) then
            begin
              if (TDomElement(chld).tagname=tag) then
                begin
                  result:=chld;
                  exit;
                 end;
            end;
          chld:=chld.nextsibling;
        end;
    end;
end;

function searchtagback(prnt:TDomNode;tag:string):TDomNode;
// Seach last matching tag in siblings

var chld: TDomNode;
begin
  result:=nil;
  if assigned(prnt )  then
    begin
      chld:=prnt.LastChild;
      while assigned(chld) do
        begin
          if (chld is TDomElement) then
            begin
              if (TDomElement(chld).tagname=tag) then
                begin
                  result:=chld;
                  exit;
                 end;
            end;
          chld:=chld.PreviousSibling;
        end;
    end;
end;

function reducefilename(name:string):string;
var i,j : integer;
begin
  i:=1; j:=length(name);
  {while not (name[i] in ['0'..'9']) do inc(i);
 } result:=ChangeFileExt(copy(name,i,j-i+1),'');
  while length(result)<5 do result:='0'+result;
end;

function padzero(s:string):string;

begin
 result:=s;
 while length(result)<5 do result:='0'+result;
end;

procedure printchildren(root:tdomnode);
var
    node :TDomNode;
    children:TDOMNodeList;
    i: integer;
begin
   children:=root.ChildNodes;
   for i:= 0 to children.Length- 1 do
     begin
       node:=children.Item[i];
       write(node.classname,'"',node.NodeName,'" ',node.Nodevalue,' - ' );
     end;
   writeln;
end;

{ THtmlDocFile }

constructor THtmlDocFile.create;
begin
  fillchar(navtable,sizeof(navtable),#0);
  dom:=THTMLDocument.create;
  children:=TStringList.Create;
  children.Sorted:=true;
end;

procedure THtmlDocFile.findtitle;
// Tries to find object(section,chapter etc) true title. Not necessarily title tag.
procedure trytitle(node: TDomNode);
// finds most section's title.
var
    span,txt: TDomNode;
begin
  if assigned(node) then
    begin
      span:=searchtag(node,'span');
      if assigned(span) then
        begin
           txt:=span.firstchild;
           while assigned(txt) do
             begin
               if txt is TDomText then
                   title:=title+trim(tDomText(txt).nodevalue);
               txt:=txt.nextsibling;
             end;
        end;
    end;
end;

procedure examinenode(txt:TDomNode;var title:ansistring);
var s:ansistring;
begin
  while assigned(txt) do
    begin
      if txt is TDomText then
        begin
          s:=trim(tDomText(txt).nodevalue);
          s:=ansireplacestr(s,chr(160),' ');
          title:=trim(title)+' '+s;
        end
      else
        examinenode(txt.firstchild,title);
      txt:=txt.nextsibling;
    end;
end;

procedure try_hx_a(nr:integer);
// finds some sections with clickable titles
var
    node: TDomNode;
    span,txt: TDomNode;
begin
  node:=searchtag(dom.body,'h'+inttostr(nr));
  if assigned(node) then
    begin
      examinenode(node.firstchild,title);
  (*    span:=searchtag(node,'a');
//      printchildren(span);
      if assigned(span) then
        begin
           examinenode(span,title);
           printchildren(span);
    //       examinenode(span.firstchild,title);
        end;
*)
    end; 
end;
var node: TDomNode;
    l,
    i : integer;
begin
  title:='';
  for i := 1 to 4 do
    begin
      if title='' then
        begin
          node:=searchtag(dom.body,'h'+inttostr(i));
          trytitle(node);
        end;
    end;
    // alternate locations:
  if title='' then
    try_hx_a(2);
  if title='' then
   try_hx_a(3);

  if title='' then
   try_hx_a(4);

  {$ifdef debugtitle}
  if title='' then
     writeln('no title:',filename)
  else
     writeln('title:',title,' in ',filename);
  {$endif}
  // strip appendix,chapter from title for easy indexing.
  redtitle:=title;
  if ansiStartsText('Chapter',redtitle) then
     delete(redtitle,1,8);
  if ansiStartsText('Appendix',redtitle) then
    delete(redtitle,1,9);
  l:=length(redtitle);
  redtitle:=trim(redtitle);
  i:=1;
  while (i<l) and (redtitle[i] in ['0'..'9','.']) do
    inc(i); 
  if i<>l then
   dec(i);
  setlength(redtitle,i);
  {$ifdef debugtitle}
  writeln('redtitle:"',redtitle,'"');
  {$endif}
end;

procedure THtmlDocFile.navcheck;
//unused debugproc to do a sanity  check if top and bottom nodes don't match
var isfirst:boolean;
    n :TNavButton;
begin
  isfirst:=true;
  for n := low(n) to high(n) do
    if assigned(navtable[0][n]) and assigned(navtable[1][n]) and (navtable[0][n]<>navtable[1][n]) then
      begin
        if isfirst then
          write(':',title,'  ');
        write(getenumname(typeinfo(tnavbutton),ord(n)),' ' );
        isfirst:=false;
      end;
  if not isfirst then writeln;
end;

procedure THtmlDocFile.parsetitle;
var i,j,k : Integer;
   s2:ansistring;
   code : longint;
begin
 i:=1;
 j:=length(title); k:=0;
 if title<>'' then
 while (i<=j) and (k<=high(numbers)) do
   begin
     s2:='';
     while (i<=j) and (title[i] in ['0'..'9','A'..'Z']) do
       begin
         s2:=s2+title[i];
         inc(i);
       end;
     if length(s2)>0 then
       begin
         numbers[k]:=s2;  padnumbers[k]:=padzero(s2);
          inc(k);
//       val(s2,numbers[k],code); inc(k);
       end
     else
       i:=j;
     inc(i);
   end;
end;

procedure THtmlDocFile.patch;
// tries to patch up/down/next nodes etc.
var n : TNavButton;
    fn : string;
begin
  for n:= succ(low(n)) to high(n) do
  begin
    if assigned(Localnavtable[n]) then
      begin
        fn:=localnavtable[n].Filename;
        if assigned(navtable[0][n]) then
           setnode_tourl(navtable[0][n],fn);
        if assigned(navtable[1][n]) then
           setnode_tourl(navtable[1][n],fn);
      end;
      if n=NavTail then
          begin
            if assigned(navtable[0][n]) then
              setnode_tourl(navtable[0][n],'#tail'+filename);
            if assigned(navtable[1][n]) then
              setnode_tourl(navtable[1][n],'#tail'+filename);
          end;
      if (n=NavPrevTail) and assigned(localnavtable[navprev]) then
          begin
            fn:=localnavtable[navprev].Filename;
            if assigned(navtable[0][n]) then
              setnode_tourl(navtable[0][n],fn+'#'+fn);
            if assigned(navtable[1][n]) then
              setnode_tourl(navtable[1][n],fn+'#'+fn);
          end;
      if n=navfront then
          begin
            if assigned(navtable[0][n]) then
              setnode_tourl(navtable[0][n],filename);
            if assigned(navtable[1][n]) then
              setnode_tourl(navtable[1][n],filename);
          end;
  end;
end;

procedure THtmlDocFile.read(filenamewithpath,fn: string);
begin
  ReadHtmlFile(dom,filenamewithpath);
  filename:=fn;
  findtitle;
  parsetitle;
  scan_nav_table(true,navtable[0]);
  scan_nav_table(false,navtable[1]);
end;

function THtmlDocFile.scantablenode(root: tdomnode;var resnode:Tdomnode):TNavbutton;
var
    children: TDOMNodeList;
    i       : integer;
    j       : TNavButton;
    node    : TDomNode;
    s       : domstring;
begin
   result:=NavNone;
   children:=root.ChildNodes;
   {$ifdef printattributes}
     writeln('root:', root.nodevalue);
     printattributes(root,'');
   {$endif}  
   for i:= 0 to children.Length- 1 do
     begin
      node:=children.Item[i];
      s:=node.nodevalue;
      if s='' then
        begin
          result:=scantablenode(node,resnode);
          if result<>navnone then 
           begin
             exit(result);
           end;
        end;
     {$ifdef printattributes}
      writeln('navchild:',node.nodename,' ',s,' * ');
      writeln('attr:');
      printattributes(node,'');  
      printchildren(node);
     {$endif}
      j:=low(NavButtonCaptions);
     {$R-}
      while (j<=high(NavButtonCaptions)) and (navbuttoncaptions[j]<>s) do inc(j);
      if j<=high(navbuttoncaptions) then
        begin
          resnode:=node;
          exit(j);
        end;
     {$R+}
     end;
end;

procedure THtmlDocFile.scan_nav_table(scanforward:boolean;var navtab:TNavArray);
var resnode,divnode,pnode,node :TDomNode;
    Attributes:TDOMNamedNodeMap;
    children:TDOMNodeList;
    i : integer;
    but : tnavbutton;
begin
   if scanforward then
     divnode:=searchtag(dom.body,'div')
    else
     divnode:=searchtagback(dom.body,'div');
  if assigned(divnode) then
    begin
      //printchildren(divnode);
      pnode:=searchtag(divnode,'p');
      if assigned(pnode) then
        begin
        {$ifdef sdebugoutput}
        writeln(title,' ');
        {$endif}
        children:=pnode.ChildNodes;
         for i:= 0 to children.Length- 1 do
          begin
            node:=children.Item[i];
            if node.NodeName='a' then
              begin
                 resnode:=nil; 
                 but:=scantablenode(node,resnode);
                 if but<>navnone then
                  begin
                 {$ifdef printchildren}
                   writeln('found',but,' ',resnode.nodename,' ',resnode.nodevalue);
                 {$endif}
                   navtab[but]:=node;
                  end;
              end;
          {$ifdef printchildren}
            write('   "',children.Item[i].NodeName,'" ',children.Item[i].Nodevalue,' - ' );
            writeln;
          {$endif}
          end;
        end;
      {$ifdef printattributes}
        printattributes(pnode,title);  
      {$endif}
    end;
end;

function THtmlDocFile.walk(prev,up: THtmlDocFile): THtmlDocFile;
var cur : THtmlDocFile;
    i   : Integer;
begin
  localnavtable[NavUp]:=up;
  localnavtable[Navprev]:=prev;
  prev:=self;
  up:=self;
  for i := 0 to children.Count - 1 do
    begin
      cur:=THtmlDocFile(children.Objects[i]);
      prev:=cur.walk(prev,up)
    end;
  result:=prev;
end;

function THtmlDocFile.walkback(next: THtmlDocFile): THtmlDocFile;
var cur : THtmlDocFile;
    i   : Integer;
begin
  for i := children.Count - 1 downto 0 do
    begin
      cur:=THtmlDocFile(children.Objects[i]);
      next:=cur.walkback(next)
    end;
  localnavtable[Navnext]:=next;
  result:=self;
end;

{ TIndexes }

constructor TIndexes.create;
var i:integer;
begin
 htmls:=TStringList.Create; htmls.sorted:=true;
 for i := 0 to high(indexes) do
   begin
    indexes[i]:=Tstringlist.Create; indexes[i].Sorted:=true;
   end;
end;

procedure TIndexes.linkfiles;
var
    i,j : integer;
    sect,sect2,chap :  THtmlDocFile;
    s : string;
begin
  // collection chapters is zeropadded, rest not.
  // all "children" must be zero padded, since it is final ordering.
  for  i:= 0 to sections.Count - 1 do
    begin
      sect:=THtmlDocFile(sections.Objects[i]);
      s:=sect.padnumbers[0];
      j:=chapters.IndexOf(s);
      if j<>-1 then
        begin
          chap:=THtmlDocFile(chapters.Objects[j]);
          chap.children.AddObject(sect.padnumbers[1],sect);
          {$ifdef debugoutput}
          writeln('linking ',sect.redtitle,' to ',chap.redtitle);
          {$endif}
       end
      else
        writeln(sect.redtitle,' not found');
    end;
  {$ifdef debugoutput}
  writeln('finished linking sections');
  {$endif}
  for  i:= 0 to subsections.Count - 1 do
    begin
      sect:=THtmlDocFile(subsections.Objects[i]);
      s:=sect.padnumbers[0];
      j:=chapters.IndexOf(s);
      if j<>-1 then
        begin
          chap:=THtmlDocFile(chapters.Objects[j]);
          s:=sect.padnumbers[1];
          j:=chap.children.IndexOf(s);
          if j<>-1 then
            begin
              sect2:=THtmlDocFile(chap.children.Objects[j]);
              sect2.children.addobject(sect.padnumbers[2],sect);
              {$ifdef debugoutput}
              writeln('linking ',sect.redtitle,' to ',sect2.redtitle);
              {$endif}
            end
          else
            writeln('Section for ',sect.redtitle,' not found');
       end
      else
        writeln('Chapter for ',sect.redtitle,' not found');
    end;

end;

procedure TIndexes.patch;
var i : integer;
begin
 for i := 0 to htmls.Count - 1 do
   THtmlDocFile(htmls.Objects[i]).patch;
end;

procedure TIndexes.readfiles(basedir:string);
// load files, and sort them into an index of node types based on filename.
var
  d :  TSearchRec;
  v :  THtmlDocFile;
  redname : string;

begin
  if basedir<>'' then
    basedir:=IncludeTrailingPathDelimiter(basedir);
  if findfirst(basedir+'*.html',faanyfile,d)=0 then
    begin
      repeat
        v:=THtmlDocFile.create;
        v.read(basedir+d.Name,d.name); 
        {$ifdef debugoutput}
          Writeln('  ',d.name);
        {$endif}
        delete(d.Name,1,length(prefix));
        redname:=v.redtitle;                 // redtitle is paragraph title minus appendix/chapter etc.
        if ansiStartsText('ch',d.Name) then
         begin
           writeln('chap:',redname);
          chapters.AddObject(padzero(redName),v) // must be sorted from the start, so padded.
         end
        else
          if ansiStartsText('se',d.Name) then
            sections.AddObject(redName,v)
          else
            if ansiStartsText('su',d.Name) then
              subsections.AddObject(redName,v)
            else
              if ansiStartsText('ap',d.Name) then
                chapters.AddObject(padzero(redName),v)    // treat appendix as chapter. "A" sorts after numbers
              else
                if ansiStartsText('li',d.Name) then
                  toc.AddObject(redName,v)
                else
                  rest.AddObject(redName,v);
        htmls.addobject(d.name,v);
      until findnext(d)<>0;
      findclose(d);
    end;
end;

procedure TIndexes.walk;
// walk all chapters recursively and generate a prev, up nodes.
var i : integer;
    up,prev: THtmlDocFile;
    cur : THtmlDocFile;
begin
  prev:=nil;
  up:=nil; // should init to index or so.
  for i := 0 to chapters.Count - 1 do
    begin
      cur:=THtmlDocFile(chapters.Objects[i]);
      prev:=cur.walk(prev,up);
    end;
end;

procedure TIndexes.walkback;
// walk all chapters backwards recursively and generate a next nodes.
var i : integer;
    next: THtmlDocFile;
    cur : THtmlDocFile;
begin
  next:=nil;
  for i := chapters.Count - 1 downto 0 do
    begin
      cur:=THtmlDocFile(chapters.Objects[i]);
      next:=cur.walkback(next);
    end;
end;

procedure TIndexes.writehtmls(basedir: string);
var I : integer;
    sx :THTMLDocument;
    fnhtml:string;
begin
  for i:=0 to htmls.count-1 do
   begin
     fnhtml:=basedir+pathdelim+prefix+htmls[i];
     sx:=THtmlDocFile(htmls.objects[i]).dom;
     writehtml(sx,fnhtml);
   end;
end;

end.