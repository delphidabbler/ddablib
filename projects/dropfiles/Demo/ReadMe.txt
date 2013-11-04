Demo Programs for the Drop Files Components
===========================================

There are two demo projects, in the Demos\1 and Demos\2 directories.

Compiling either project requires Delphi 7 or later. The projects are compatible
with both the Delphi 32 and 64 bit compilers.

Different project files are provided to suit different compilers, as follows:

  * Delphi 7
    Open the .dpr file and compile.

  * Delphi 2005 and 2006
    Open the .bdsproj file and compile.

  * Delphi 2007
    Delete the .dproj file then remove the .2007 extension from the .dproj.2007
    file to create a new .dproj file. Open this new .dproj file and compile.

  * Delphi 2009, 2010 and XE
    Open the .dproj file and compile.

  * Delphi XE2, XE3 and XE4
    Open the .dproj file, choose either the 32-bit Windows or 64-bit Windows
    target as required and compile.

Demo 1
------

This project is named DropFilesDemo and it thoroughly exercises the
TPJDropFiles, TPJFormDropFiles and TPJCtrlDropFiles components along with the
TPJWildCardFilter and TPJExtFileFilter filter components. It also shows how to
use the OnFileFilter event exposed by all the drop files components.

The main form is divided into different areas where files can be dropped: the
form, some blue panels, a pink RTF control and a green shape. Check boxes are
provided to control the various properties of the drop files controls in each
of the areas. Filters for three of the four areas can be customised or switched
off.

Demo 2
------

This project is named FrameDemo and is provided to show how to catch files
dropped on frames using TPJCtrlDropFiles.

There are two frames on the form, each of which can catch dropped files. Both
frames have a TPJExtFileFilter component for filtering dropped files and an edit
box where the required extensions can be specified. Files can be dropped
directly on the main form.

Files dropped on the frames are filtered and files that pass the filter are
listed in the frame. The full file path of all (unfiltered) dropped files is
displayed in the main form. Dropping files outside any frame simply lists them
in the main form.

A bar at the bottom of the form displays the name of the control on which the
files were dropped.