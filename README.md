# FPC documentation sources

This is the README for the Free Pascal documentation.

All documentation is stored here, in LaTeX format and in fpdoc format.
it uses special style files (fpc*.sty) which are also in the directory.

Building the LaTeX documentation on Windows is not supported.
It needs various common unix utilities to work.
You can probably make it work if you install cygwin and try to build
within the cygwin environment, but even that is not guaranteed.

It is possible to generate the HTML reference documentation on Windows.
See below for instructions on how to do this.

## Available Options

Before you start, make sure the FPC sources are in the correct location.
They are needed to build the reference documentation.

See the section "General settings" for the assumptions made about default
locations, and how to configure make if the sources are in another location.

To see a list of all available "target" output formats, run the following
command (you need GNU make installed on your system):
```sh
make help
```

* do a `make dvi` to produce the dvi format of the docs.
* a `make html` will produce the html version (using tex4ht).
* a `make ps` will produce PostScript documents.
* a `make pdf` will produce PDF (Portable Document Format) documents.
* a `make txt` will produce plain text documents.
* a `make chm` will produce window CHM documents.

If you only want to generate the RTL and FCL reference documentation
in HTML format, starting from the fpdoc XML descriptions, then the
following 2 commands should be enough:

```sh
make rtl.chk
make fcl.chk
```

The html target output format has some variations as well.
The two commands above will generate the standard html reference
documentation in 2 sub-directories named rtl and fcl.

Two variations are both html output, but in a single file format.

## CHM - Compressed HTML Help

When the HTMLFMT environment variable is set to "chm", make html (fpdoc)
will compress the html files to .chm files.  The environment variable
CSSFILE can be used to override the style file (.css).

A complete release building script "fixdocs.sh" is added that should
build the .chms on *nix, provided a recent FPC is installed (not just fpdoc,
needs 2.4.0/2.5.1 fcl-xml too).

Note that the script does not do a great job cleaning yet, so if you have
odd problems, try to manually cleanup the fpcdocs/ checkout first.

See also [Below](#chm-considerations) for more information about CHM.

## IPF - OS/2 Information Presentation Facility

When the HTMLFMT environment variable is set to "ipf", make html (fpdoc)
will generate a single .ipf file that contains all the documentation. This
.ipf file can then be compiled with the IPFC compiler (external tool) to
generate the final .INF help file.

```sh
make rtl.chk HTMLFMT=ipf FPCSRCDIR=/path/to/fpc/source
```

## General Settings
The reference documentation and the list of compiler messages are
created from the FPC sources. For this to work correctly, the
documentation should be in the same directory as the rest of the
FPC source tree.

Meaning a directory layout as
```text
/FPC/compiler
    /rtl
    /packages
    /docs
```

or, alternatively, the directory structure as created when checking out
the build SVN-repository:
```
/FPC/fpcsrc/compiler
           /rtl
           /packages
    /fpcdocs
```

If you have the sources in another location, you can specify their
location through the FPCSRCDIR makefile variable, e.g like in:

```sh
make html FPCSRCDIR=/path/to/fpc/sources
```

or an example of generating HTML reference documentation only:

```sh
make rtl.chk fcl.chk FPCSRCDIR=/path/to/fpc/sources
```

If you want to produce dos docs, you can do a 'make htm' this will convert
the .html files to .htm files (including all references), suitable for a 8:3
format.

The rest of this document is only interesting if you want to write docs.
Otherwise, you can bail out now.

## THE DOCS


### Why LaTeX ?

* because I like a printed copy of the manuals, HTML just isn't good enough
  for this.
* I know LaTeX very well :) (mind you : html also !)
* It converts to many other formats.
* many other reasons.

In order to translate the user manuals to HTML, I use tex4ht.


### Why fpdoc ?

* Because it always creates up-to-date documentation.
* The documentation is separate from the units contrary to many other
  documentation tools which require comments in the sources, which makes
  the source unreadable.
* It's written in FPC.

### Then how to proceed ?

If you just want to write general latex docs, just use fpc.sty.
fpc.sty.doc describes what fpc.sty does. (one day I'll integrate them using
the doc package, but I need some time for it)

If you want to document units, use fpdoc. It is documented fairly complete,
and you can have a look at the many .xml units for examples on how to use
it.

## CHM considerations

CHM support based on the great work of Andrew is now also available in the
textmode IDE.  This archive also contains the .xct and .kwd files that are
needed for crosslinking this archive with other CHM files. These files are
not required for viewing (but are only about 1% of the total size)

toc.chm is a workaround for help systems that don't provide a list of
helpfiles. In such system it provides access to the default pages of the
various CHMs.

### How to install the CHMs in Lazarus

- Install the chmhelp package
- Copy the chm, xct, or kwd files to docs/chm in the Lazarus directory.

Note that Lazarus does not load ref.* at this moment, so (CHM) help
on keywords does not work yet.
Note that toc.chm is not loaded by the LHelp CHM viewer at the present time.

### How to install the CHMs into the textmode IDE.

1. extract the archive somewhere, the archive already has an "help/" path
    built in. (on windows/dos: c:\fpc, under unix e.g. /usr/share/fpc/help)
2. add the files to the textmode IDE using the "help->files->new" button.
	Add toc.chm first, then add the other files in random order.
3. it is safe to restart the IDE before testing. (in case something goes
   wrong at least your help config will be saved)

The helpconfiguration is stored, with paths in fp.ini, and might look like
this:
```ini
[Help]
Files="/fpc/fpcdocs/toc.chm;/fpc/fpcdocs/fcl.chm;/fpc/fpcdocs/ref.chm;/fpc/fpcdocs/rtl.chm;/fpc/fpcdocs/prog.chm;/fpc/fpcdocs/user.chm;/fpc/fpcdocs/fclres.chm"
```

### Troubleshooting

If you use Windows XPsp2 or later, and an helpfile won't view, go into the
file explorer, and bring up the properties of the CHM file. Then click
"unblock" there. Apparantly, in some cases Windows thinks the helpfiles are
downloaded content, and prevents access.

To do this programatically, FPC 2.4.3+ has this functionality built into the
chmls utility (chmls unblock <chmfile>)

### What is CHM?

CHM is an archive format specially made for HTML based help by Microsoft. It
is also known as HTMLHELP, but note that HTMLHELP2 (and in the future 3) are
not related.  Besides being an archive format optimized to quickly extract a
single file, there is also a TOC, an Index and fulltext search dictionaries
in each CHM.

The html in a CHM is basically unmodified except for links from one chm file
to the other, these use a ms-its://file.chm/path/to/htmlfile.html like URL
syntax. Some extra tables with references might be generated.

The format is sometimes related to security problems, but as far as I have
been able to verify, this is more an Internet Explorer viewer application
problem, not a problem of the format itself

### What OSes support CHM natively?

Windows versions after windows 98 can open CHMs. Due to new security
measures in XPsp2 and later, sometimes unlocking the CHMs is necessary (see
troubleshooting paragraph).

On Linux (and mostly *nix in general) there are at least four packages
to process CHMs in addition to FPC/Lazarus' own:

* the xCHM viewer
* the KCHMViewer viewer
* the GNOCHM viewer
* the chmlib packages (sometimes called extract_chmlib) that allows to unpack
     chm files.

Older kchmviewers (<=4.0) are known to have a problem with the indexes of
the larger chm files (rtl.chm, and lcl.chm from Lazarus), and only show the
first couple of hundred entries.  The author has been notified and can't
reproduce it with his latest builds anymore.

kchmviewer versions (even as new as in Fedora 13) seem to have problems with
links that don't have a leading / either ( ? ). I reported it to kchmviewer,
but the maintainer said it was a problem of QT (and indeed, switching
khtmlpart resolves it)

Gnochm is extremely slow with the larger files, and development seems to
have stalled.

xCHM in the recent version seems to work fine and reasonably fast. (lcl.chm
in a few secs).  However its TOC system is not a tree, and some versions
won't load some nodes (same problem as kchmviewer ?). The most recent
version seems to have fixed it.

Of course the lazarus and FPC textmode IDE now also support CHM.
