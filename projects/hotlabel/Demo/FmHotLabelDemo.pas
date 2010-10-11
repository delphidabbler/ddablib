{
 * FmHotLabelDemo.pas
 *
 * Main form for Hot Label Component Demo.
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
 * The Original Code is FmHotLabelDemo.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmHotLabelDemo;

interface


uses
  // Delphi
  Classes, Controls, StdCtrls, Forms,
  // Hot label component
  PJHotLabel;


type
  {
  TDemoForm:
    Main demo form.
  }
  TDemoForm = class(TForm)
    hlIndex: TPJHotLabel;
    hlArticles: TPJHotLabel;
    hlArticle1: TPJHotLabel;
    hlArticle2: TPJHotLabel;
    hlArticle3: TPJHotLabel;
    hlArticle4: TPJHotLabel;
    hlArticle5: TPJHotLabel;
    hlArticle6: TPJHotLabel;
    hlArticle7: TPJHotLabel;
    hlArticle8: TPJHotLabel;
    hlArticle9: TPJHotLabel;
    lblIndex: TLabel;
    procedure HotLabelArticleHint(Sender: TObject; var HintStr: String);
    procedure HotLabelArticleClick(Sender: TObject);
  private
    fArticleVisited: array[1..9] of Boolean;
      {Records whether each article's link has been visited}
  end;


var
  DemoForm: TDemoForm;


implementation


uses
  // Delphi
  SysUtils, Graphics;


{$R *.DFM}

function GetArticleNumFromURL(const URL: string): Integer;
  {Extracts article number from URL that references article.
    @param URL [in] URL containing article number.
  }
begin
  Result := StrToInt(Copy(URL, Length(URL), 1));
end;

procedure TDemoForm.HotLabelArticleClick(Sender: TObject);
  {Updates article label when clicked for first time.
    @param Sender [in] Hot label that was clicked.
  }
var
  ArtNum: Integer;  // article number referenced by label's URL
  HL: TPJHotLabel;  // reference to clicked hot label
begin
  ArtNum := GetArticleNumFromURL((Sender as TPJHotLabel).URL);
  if not fArticleVisited[ArtNum] then
  begin
    // first time visited: record visit and update label colours
    fArticleVisited[ArtNum] := True;
    HL := Sender as TPJHotLabel;
    HL.Font.Color := clMaroon;
    HL.HighlightFont.Color := HL.Font.Color;
  end;
end;

procedure TDemoForm.HotLabelArticleHint(Sender: TObject;
  var HintStr: String);
  {Handles OnHint event for Article label.
    @param Sender [in] Hot label generating hint.
    @param HintStr [in/out] Set to required hint.
  }
var
  ArtNum: Integer;  // article number referenced by label's URL
begin
  // We set hint differently depending on if link visited or not
  ArtNum := GetArticleNumFromURL((Sender as TPJHotLabel).URL);
  if not fArticleVisited[ArtNum] then
    HintStr := Format('Click to view article #%d', [ArtNum])
  else
    HintStr := Format('Click to revisit article #%d', [ArtNum]);
end;

end.
