{ ##
  @PROJECT_NAME             DropFilesDemo
  @PROJECT_DESC             Demonstrates use of Drop Files components and
                            filters.
  @FILE                     DropFilesDemo.dpr
  @COMMENTS                 Demo program project file.
  @LEGAL_NOTICE             This demo program is distributed under the Mozilla
                            Public License - see below for details.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/02/2005
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 12/04/2004
      @COMMENTS             Added title.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 19/03/2006
      @COMMENTS             Modified title.
    )
  )
}


{
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is DropFilesDemo.dpr.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2006 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


program DropFilesDemo;

uses
  Forms,
  FmDropFilesDemo in 'FmDropFilesDemo.pas' {DropFilesDemoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'DelphiDabbler Drop Files Components Demo';
  Application.CreateForm(TDropFilesDemoForm, DropFilesDemoForm);
  Application.Run;
end.
