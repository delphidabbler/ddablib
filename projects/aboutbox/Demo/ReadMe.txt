This demo exercises the DelphiDabbler About Box Component.

The demo may compile with Delphi 6 and earlier, but this has not been tested.

Delphi 7 users, use the .dpr project file.

Delphi 2006 users (and Delphi 2005?), open the .bdsproj file.

Delphi 2007 users will need to delete the .dproj file and load the .dpr file.

Delphi 2009 and later users load the .dproj file.

Note 1:

TSpinEdit from "Samples" palette is required to build the demo. If this
component is not present the demo will not compile.

Note 2:

You may need to add the path to the package containing the About Box Component
binaries to the project options.

Note 3:

The About Box Component is 64 bit compatible. To compile the demo as a 64 bit
add a Windows 64 bit target to the project options, set the path to the 64 bit
package containing the About Box component.

Also in project options make sure that Delphi does not generate version
information so that the program retains its own version information - this is
used in the test.