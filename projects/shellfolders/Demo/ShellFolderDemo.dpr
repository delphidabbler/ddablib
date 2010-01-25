{ ##
  @PROJECT_NAME             ShellFolderDemo
  @PROJECT_DESC             Demonstrates shell folders unit.
  @FILE                     ShellFolderDemo.dpr
  @COMMENTS                 Demo project file.
  @DEPENDENCIES             None.
  @LICENSE                  The demo is released under the Mozilla public
                            license (see below).
  @COPYRIGHT                Copyright (c) 2003-2005, Peter D Johnson.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 15/06/2003
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 22/12/2005
      @COMMENTS             + Set Application.Title.
                            + Changed application's icon to DelphDabbler
                              standard icon.
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
 * The Original Code is ShellFolderDemo.dpr.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2003-2005 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


program ShellFolderDemo;

uses
  Forms,
  ShellFolderDemoForm in 'ShellFolderDemoForm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Shell Folders Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
