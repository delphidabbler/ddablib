{
 * FmHotLabelDemo.pas
 *
 * Main form for demo program that demonstrates use of the Hot Label Component.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
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
  if not (Sender as TPJHotLabel).Visited then
    HintStr := Format('Click to view article #%d', [ArtNum])
  else
    HintStr := Format('Click to revisit article #%d', [ArtNum]);
end;

end.
