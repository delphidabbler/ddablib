; ------------------------------------------------------------------------------
; This Source Code Form is subject to the terms of the Mozilla Public License,
; v. 2.0. If a copy of the MPL was not distributed with this file, You can
; obtain one at http://mozilla.org/MPL/2.0/
;
; Copyright (C) 2014-2016, Peter Johnson (www.delphidabbler.com).
;
; Change Log for Hot Label Component
; ------------------------------------------------------------------------------

Release v2.2.1 of 14 February 2016
+ Fixed memory leak introduced in v2.2.0 because of a failure to free a font object.

Release v2.2.0 of 05 February 2014
+ The component can now display the text of "visited" URLs in a different style. Three new properties have been added to support this feature: Visited, VisitedFont and TrackVisits.
+ The default URL used by the component has changed from "http://localhost" to "http://example.com".
+ Fixed some bugs:
  - Fixed problem with compiler directives that was causing compilation to fail on Delphi XE5.
  - Changing the highlight font while the label is highlighted now updates the display immediately. Previously the effect of the change was delayed until the label was next highlighted.
  - An error in detecting one of the possible error codes when executing a URL has been fixed.
  - The component no longer has to have a parent form. Previously an exception would be raised when the label was clicked if there was no parent form.
+ Unit names in uses clauses are now qualified with their namespace when compiled with Delphi XE2 and later.
+ Changes to demo projects:
  - Modified to demonstrate component's ability to track and display "visited" URLS.
  - Form's appearance changed: it is no longer scaled, the font is now "Arial", and the form now appears at the centre of the desktop.
  - The form file is now stored in text format.
+ WinHelp help files removed from project.
+ Updated documentation:
  - The project read-me file has updated re the changes.
  - The documentation internet short-cut's URL has been changed. The short-cut has also been renamed.
  - The MPL text file's contents have been changed from the Mozilla Public License v1.1 to v2.0 and the file has been renamed accordingly.
+ License changes:
  - Component source license changed to Mozilla Public License v2.0.
  - Main documentation files now also licensed under Mozilla Public License v2.0.
  - Demo source code placed in the public domain (Creative Commons CC0 1.0 Universal).

Release v2.1.1 of 12 October 2010
+ Removed support for Delphi 2 and 3. Unit now generates error when compiled with these compilers.
+ Removed custom hand-point cursor.
+ Modified to compile without warnings on Delphi 2010.
+ Updated help file re changes.
+ Revised documentation and included short-cut file that links to component Wiki.

Release v2.1 of 17 March 2007
+ Fixed design time bug that could change font colour if mouse was moved over label in designer.
+ Added .als keyword file for use with Delphi 6 and 7.
+ Changed name of cursor resource.
+ Update copyright message in help file.
+ Some minor re-factorings.
+ Changed to use Mozilla Public License for component's source code.
+ Added HotLabelDemo demo program.

Release v2.0 of 02 November 2003
+ Added separate URL property to enable caption to be different to URL accessed.
+ Added ability to highlight label text when cursor passes over it.
+ Added ability to display URL in hint or to display custom hints set by handling the new OnCustomHint event.
+ Now uses built in hand point cursor for Delphi 4 and higher while still loading a custom cursor from resources for earlier compilers.
+ Moved error messages to resource strings for Delphi 3 and later while using constants for Delphi 2.
+ Now recognises the https:// protocol as valid.
+ Changed default URL to http://localhost/.
+ Rewrote help file to be compatible with Delphi 3 and later IDEs. Help file is no longer compatible with Delphi 2 IDE.

Release v1.0.1 of 28 November 1999
+ Updated HTML documentation to include installation notes for Delphi 3/4 and added update history.

Release v1.0 of 25 October 1999
+ Original version.
