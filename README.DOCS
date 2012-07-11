This is the README for the Free Pascal documentation.

All documentation is stored here, in LaTeX format and in fpdoc format.
it uses special style files (fpc*.sty) which are also in the directory.

Building the LaTeX documentation on Windows is not supported.
It needs various common unix utilities to work.
You can probably make it work if you install cygwin and try to build
within the cygwin environment, but even that is not guaranteed.

It is possible to generate the HTML reference documentation on Windows.
See below for instructions on how to do this.

Available Options
-----------------

Before you start, make sure the FPC sources are in the correct location. 
They are needed to build the reference documentation.

See the section "General settings" for the assumptions made about default 
locations, and how to configure make if the sources are in another location.

To see a list of all available "target" output formats, run the following
command (you need GNU make installed on your system):

make help

do a 'make dvi' to produce the dvi format of the docs.
a 'make html' will produce the html version (using tex4ht).
a 'make ps' will produce PostScript documents.
a 'make pdf' will produce PDF (Portable Document Format) documents.
a 'make txt' will produce plain text documents.

If you only want to generate the RTL and FCL reference documentation
in HTML format, starting from the fpdoc XML descriptions, then the 
following 2 commands should be enough:

make rtl.chk 
make fcl.chk

The html target output format has some variations as well. 
The two commands above will generate the standard html reference
documentation in 2 sub-directories named rtl and fcl. 

Two variations are both html output, but in a single file format.


CHM - Compressed HTML Help
--------------------------
When the HTMLFMT environment variable is set to "chm", make html (fpdoc)
will compress the html files to .chm files.  The environment variable 
CSSFILE can be used to override the style file (.css).

A complete release building script "fixdocs.sh" is added that should 
build the .chms on *nix, provided a recent FPC is installed (not just fpdoc,
needs 2.4.0/2.5.1 fcl-xml too). 

Note that the script does not do a great job cleaning yet, so if you have
odd problems, try to manually cleanup the fpcdocs/ checkout first.


IPF - OS/2 Information Presentation Facility
--------------------------------------------
When the HTMLFMT environment variable is set to "ipf", make html (fpdoc)
will generate a single .ipf file that contains all the documentation. This
.ipf file can then be compiled with the IPFC compiler (external tool) to
generate the final .INF help file.

make rtl.chk HTMLFMT=ipf FPCSRCDIR=/path/to/fpc/source


General Settings
----------------
The reference documentation and the list of compiler messages are 
created from the FPC sources. For this to work correctly, the 
documentation should be in the same directory as the rest of the 
FPC source tree.

Meaning a directory layout as

/FPC/compiler
    /rtl
    /packages
    /docs

or, alternatively, the directory structure as created when checking out
the build SVN-repository:

/FPC/fpcsrc/compiler
           /rtl
           /packages
    /fpcdocs

If you have the sources in another location, you can specify their
location through the FPCSRCDIR makefile variable, e.g like in:

make html FPCSRCDIR=/path/to/fpc/sources

or an example of generating HTML reference documentation only:

make rtl.chk fcl.chk FPCSRCDIR=/path/to/fpc/sources

If you want to produce dos docs, you can do a 'make htm' this will convert
the .html files to .htm files (including all references), suitable for a 8:3
format.

The rest of this document is only interesting if you want to write docs.
Otherwise, you can bail out now.

THE DOCS...


Why LaTeX ?
-----------
- because I like a printed copy of the manuals, HTML just isn't good enough 
  for this.
- I know LaTeX very well :) (mind you : html also !)
- It converts to many other formats.
- many other reasons.

In order to translate the user manuals to HTML, I use tex4ht.


Why fpdoc ?
-----------
- Because it always creates up-to-date documentation.
- The documentation is separate from the units contrary to many other
  documentation tools which require comments in the sources, which makes
  the source unreadable.
- It's written in FPC.


Then how to proceed ?
---------------------
If you just want to write general latex docs, just use fpc.sty. 
fpc.sty.doc describes what fpc.sty does. (one day I'll integrate them using
the doc package, but I need some time for it)

If you want to document units, use fpdoc. It is documented fairly complete,
and you can have a look at the many .xml units for examples on how to use
it.

Happy TeXing,

Michael.
