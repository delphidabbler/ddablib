{ ##
  @PROJECT_NAME             PJMessageDialogDemo
  @PROJECT_DESC             Message dialog demo program.
  @FILE                     PJMessageDialogDemo.dpr
  @COMMENTS                 Project file for demo program.
  @LEGAL_NOTICE             This demo program is distributed under the Mozilla
                            Public License - see below.
  @AUTHOR                   Peter D Johnson, LLANARTH, Ceredigion, Wales, UK.
  @OWNER                    DelphiDabbler
  @EMAIL                    peter.johnson@openlink.org
  @WEBSITE                  http://www.delphidabbler.com/
  @COPYRIGHT                © Peter D Johnson, 2003-2005.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 05/10/2003
      @COMMENTS             Original version.
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
 * The Original Code is PJMessageDialogDemo.dpr.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2003-2005 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


program PJMessageDialogDemo;

uses
  Forms,
  FmPJMessageDialogDemo in 'FmPJMessageDialogDemo.pas' {DemoForm};

{$R Icons.res}
{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Message Dialog Demo';
  Application.CreateForm(TDemoForm, DemoForm);
  Application.Run;
end.
