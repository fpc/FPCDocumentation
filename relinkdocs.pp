program relinkdocs;

{
  Quick script that corrects tex4ht output (FPC documentation)
     (C) Marco van de Voort

  Requires Sergei's work on dom_html, which means trunk from after (mainly r13357
  but maybe also one of the slightly later commits)
, and maybe mods to chmlib.

  Rule of thumb: use version 2.3.1 of the same date or newer as the last commit to the relink* files
  

  Doesn't deallocate, but needs <64MB

}

{$ifdef fpc}
{$mode delphi}
{$else}
{$apptype console}
{$stringchecks on}
{$endif}
{$info only works properly with 2.3.1+ of july 2009 or newer }
{$ifdef ver2_2}
   Die.
{$endif}

Uses {$ifdef unix}cwstring,cthreads,{$endif}strutils,typinfo,Sax_HTML,sysutils,classes,dom_html,xmlwrite,htmwrite8859,relinkhtml;


procedure fixdir(readdir,writedir,prefix:string);
var
    ind : TIndexes;

begin
  ForceDirectories(writedir);

  ind:=TIndexes.create;
  ind.prefix:=prefix;
  ind.readfiles(readdir);
  ind.linkfiles;
  ind.walk;
  ind.walkback;
  ind.patch;
  ind.writehtmls(writedir);
end;

Var
  S : String;

begin
  If ParamCount=0 then
    begin
    fixdir('prog','prog-fixed/','prog');  // input=output overwrites, but should work.
    fixdir('user','user-fixed/','user');  // input=output overwrites, but should work.
    fixdir('ref','ref-fixed/','ref');  // input=output overwrites, but should work.
    end
  else
    begin
    S:=ParamStr(1);
    fixdir(S,S,S);
    end;
end.

