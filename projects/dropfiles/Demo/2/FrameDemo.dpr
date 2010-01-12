{ 
 * FrameDemo.dpr
 *
 * Project file for demo program that demonstrates use of Drop Files Components
 * with frames.
 *
 * $Rev$
 * $Date$
 *
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
 * The Original Code is FrameDemo.dpr.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


program FrameDemo;

uses
  Forms,
  FmFrameDemo in 'FmFrameDemo.pas' {FmMain},
  FrDemo in 'FrDemo.pas' {Frame1: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DelphiDabbler Frame Drop Files Demo';
  Application.CreateForm(TFmMain, FmMain);
  Application.Run;
end.
