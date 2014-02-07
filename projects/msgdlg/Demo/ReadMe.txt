This demo exercises the various components provided in PJMessageDialog.pas.

To use, select the tab for the component you wish to try out and then fill in
the properties from the controls on the page. The controls for properties that
are not available for the selected component are disabled. Once all properties
have been entered press the Store Properties & Execute button.

The controls representing the various properties fall into distinct types as
follows:

* All controls for properties with enumerated values are drop-down lists
  containing available values.
* Set properties are represented using check list boxes - simply check the
  values that are to be included in the property.
* Numeric properties are represented by edit boxes that will only take numeric
  input.
* Text properties are usually represented by single or multi line edit boxes. An
  exception is that the control used for the IconResource property is a
  drop-down list of resource names that are contained in the demo executable.
  The empty string and an unknown resource are also provided.

If you want to test the operation of any help button that appears in the
dialogue boxes the easiest thing to do is to leave the "Use OnHelp event
handler" check box checked. When this is the case clicking a help button results
in a dialogue box being displayed that echoes the help file and context number
being requested.

Clearing the check box then clicking the dialogue's help button will cause the
component to try to find the requested help context in the specified help file
using Delphi's internal help system. If such a file does not exist an error will
be reported. To use this option you will need to create a valid help file
containing suitable help contexts. Furthermore, if the file is not in the
obsolete WinHelp format then you will also need to modify the demo to register
your required help file type. How to do this is beyond the scope of this
document.

For further information about the components and their properties see the online
documentation at http://delphidabbler.com/url/msgdlg-docs

--------------------------------------------------------------------------------
Any copyright in this file is dedicated to the Public Domain.
http://creativecommons.org/publicdomain/zero/1.0/

$Rev$
$Date$
--------------------------------------------------------------------------------
