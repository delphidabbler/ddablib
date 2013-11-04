{
 * FmDropFilesDemo.pas
 *
 * Main form file for demo program that demonstrates use of Drop Files
 * Components and filters.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}


unit FmDropFilesDemo;

interface

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and later
    {$WARN SYMBOL_DEPRECATED OFF}
  {$IFEND}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, PJDropFiles, ExtCtrls;

type
  TDropFilesDemoForm = class(TForm)
    dfForm: TPJFormDropFiles;
    dfPanel: TPJDropFiles;
    dfRTF: TPJCtrlDropFiles;
    RichEdit1: TRichEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    gbFormDropFiles: TGroupBox;
    chkFormIncFiles: TCheckBox;
    chkFormIncFolders: TCheckBox;
    chkFormRecurseFolders: TCheckBox;
    gbDropFiles: TGroupBox;
    chkPanelIncFiles: TCheckBox;
    chkPanelIncFolders: TCheckBox;
    chkPanelRecurseFolders: TCheckBox;
    chkPanelPassThru: TCheckBox;
    gbCtrlDropFiles: TGroupBox;
    chkRTFIncFiles: TCheckBox;
    chkRTFIncFolders: TCheckBox;
    chkRTFRecurseFolders: TCheckBox;
    chkRTFPassThru: TCheckBox;
    chkPanelEnabled: TCheckBox;
    chkRTFEnabled: TCheckBox;
    chkFormEnabled: TCheckBox;
    edExtensions: TEdit;
    Label1: TLabel;
    PanelExtFilter: TPJExtFileFilter;
    Label2: TLabel;
    edWildcard: TEdit;
    RTFWildcardFilter: TPJWildCardFileFilter;
    Label3: TLabel;
    DateTimePicker1: TDateTimePicker;
    Label4: TLabel;
    chkFormEnableFilter: TCheckBox;
    chkFormShowRejections: TCheckBox;
    chkPanelEnableFilter: TCheckBox;
    chkRTFEnableFilter: TCheckBox;
    dfShape: TPJDropFiles;
    Shape1: TShape;
    GroupBox1: TGroupBox;
    chkShapeIncFiles: TCheckBox;
    chkShapeIncFolders: TCheckBox;
    chkShapeRecurseFolders: TCheckBox;
    chkShapePassThru: TCheckBox;
    chkShapeEnabled: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormOptionsClick(Sender: TObject);
    procedure PanelOptionsClick(Sender: TObject);
    procedure RTFOptionsClick(Sender: TObject);
    procedure chkPanelPassThruClick(Sender: TObject);
    procedure chkRTFPassThruClick(Sender: TObject);
    procedure dfFormDropFiles(Sender: TObject);
    procedure dfPanelDropFiles(Sender: TObject);
    procedure dfRTFDropFiles(Sender: TObject);
    procedure chkFormEnabledClick(Sender: TObject);
    procedure chkPanelEnabledClick(Sender: TObject);
    procedure chkRTFEnabledClick(Sender: TObject);
    procedure edExtensionsChange(Sender: TObject);
    procedure edWildcardChange(Sender: TObject);
    procedure dfFormFileFilter(Sender: TObject; const FileName: String;
      const IsFolder: Boolean; var Accept: Boolean);
    procedure chkFormEnableFilterClick(Sender: TObject);
    procedure chkPanelEnableFilterClick(Sender: TObject);
    procedure chkRTFEnableFilterClick(Sender: TObject);
    procedure dfShapeDropFiles(Sender: TObject);
    procedure ShapeOptionsClick(Sender: TObject);
    procedure chkShapePassThruClick(Sender: TObject);
    procedure chkShapeEnabledClick(Sender: TObject);
  private
    procedure DisplayItem(C: TColor; S: string);
    function DropControlInfo(Ctrl: TControl; Point: TPoint): string;
  end;

var
  DropFilesDemoForm: TDropFilesDemoForm;

implementation

{$R *.DFM}

procedure TDropFilesDemoForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  chkFormIncFiles.Tag := Ord(dfoIncFiles);
  chkPanelIncFiles.Tag := Ord(dfoIncFiles);
  chkRTFIncFiles.Tag := Ord(dfoIncFiles);
  chkShapeIncFiles.Tag := Ord(dfoIncFiles);
  chkFormIncFolders.Tag := Ord(dfoIncFolders);
  chkPanelIncFolders.Tag := Ord(dfoIncFolders);
  chkRTFIncFolders.Tag := Ord(dfoIncFolders);
  chkShapeIncFolders.Tag := Ord(dfoIncFolders);
  chkFormRecurseFolders.Tag := Ord(dfoRecurseFolders);
  chkPanelRecurseFolders.Tag := Ord(dfoRecurseFolders);
  chkRTFRecurseFolders.Tag := Ord(dfoRecurseFolders);
  chkShapeRecurseFolders.Tag := Ord(dfoRecurseFolders);
  chkFormIncFiles.Checked := dfoIncFiles in dfForm.Options;
  chkFormIncFolders.Checked := dfoIncFolders in dfForm.Options;
  chkFormRecurseFolders.Checked := dfoRecurseFolders in dfForm.Options;
  chkFormEnabled.Checked := dfForm.Enabled;
  chkPanelIncFiles.Checked := dfoIncFiles in dfPanel.Options;
  chkPanelIncFolders.Checked := dfoIncFolders in dfPanel.Options;
  chkPanelRecurseFolders.Checked := dfoRecurseFolders in dfPanel.Options;
  chkPanelPassThru.Checked := dfPanel.PassThrough;
  chkPanelEnabled.Checked := dfPanel.Enabled;
  chkRTFIncFiles.Checked := dfoIncFiles in dfRTF.Options;
  chkRTFIncFolders.Checked := dfoIncFolders in dfRTF.Options;
  chkRTFRecurseFolders.Checked := dfoRecurseFolders in dfRTF.Options;
  chkRTFPassThru.Checked := dfRTF.PassThrough;
  chkRTFEnabled.Checked := dfRTF.Enabled;
  chkShapeIncFiles.Checked := dfoIncFiles in dfShape.Options;
  chkShapeIncFolders.Checked := dfoIncFolders in dfShape.Options;
  chkShapeRecurseFolders.Checked := dfoRecurseFolders in dfShape.Options;
  chkShapePassThru.Checked := dfShape.PassThrough;
  chkShapeEnabled.Checked := dfShape.Enabled;
  edExtensions.Text := PanelExtFilter.Extensions;
  edWildcard.Text := RTFWildcardFilter.WildCard;
  chkFormEnableFilter.Checked := Assigned(dfForm.OnFileFilter);
  chkPanelEnableFilter.Checked := Assigned(dfPanel.Filter);
  chkRTFEnableFilter.Checked := Assigned(dfRTF.Filter);
  chkFormShowRejections.Checked := False;
  // TPanel's ParentBackground seems to default to False in earlier Delphis and
  // True in later Delphis if there's no property defined in form file. So, we
  // set it to the required value here.
  Panel1.ParentBackground := False;
end;

procedure TDropFilesDemoForm.FormOptionsClick(Sender: TObject);
var
  CB: TCheckBox;
begin
  CB := Sender as TCheckBox;
  if CB.Checked then
    dfForm.Options := dfForm.Options + [TPJDropFilesOption(CB.Tag)]
  else
    dfForm.Options := dfForm.Options - [TPJDropFilesOption(CB.Tag)];
end;

procedure TDropFilesDemoForm.PanelOptionsClick(Sender: TObject);
var
  CB: TCheckBox;
begin
  CB := Sender as TCheckBox;
  if CB.Checked then
    dfPanel.Options := dfPanel.Options + [TPJDropFilesOption(CB.Tag)]
  else
    dfPanel.Options := dfPanel.Options - [TPJDropFilesOption(CB.Tag)];
end;

procedure TDropFilesDemoForm.RTFOptionsClick(Sender: TObject);
var
  CB: TCheckBox;
begin
  CB := Sender as TCheckBox;
  if CB.Checked then
    dfRTF.Options := dfRTF.Options + [TPJDropFilesOption(CB.Tag)]
  else
    dfRTF.Options := dfRTF.Options - [TPJDropFilesOption(CB.Tag)];
end;

procedure TDropFilesDemoForm.ShapeOptionsClick(Sender: TObject);
var
  CB: TCheckBox;
begin
  CB := Sender as TCheckBox;
  if CB.Checked then
    dfShape.Options := dfShape.Options + [TPJDropFilesOption(CB.Tag)]
  else
    dfShape.Options := dfShape.Options - [TPJDropFilesOption(CB.Tag)];
end;

procedure TDropFilesDemoForm.chkPanelPassThruClick(Sender: TObject);
begin
  dfPanel.PassThrough := chkPanelPassThru.Checked;
end;

procedure TDropFilesDemoForm.chkRTFPassThruClick(Sender: TObject);
begin
  dfRTF.PassThrough := chkRTFPassThru.Checked;
end;

procedure TDropFilesDemoForm.chkShapePassThruClick(Sender: TObject);
begin
  dfShape.PassThrough := chkShapePassThru.Checked;
end;

procedure TDropFilesDemoForm.chkFormEnabledClick(Sender: TObject);
begin
  dfForm.Enabled := chkFormEnabled.Checked;
end;

procedure TDropFilesDemoForm.chkPanelEnabledClick(Sender: TObject);
begin
  dfPanel.Enabled := chkPanelEnabled.Checked;
end;

procedure TDropFilesDemoForm.chkRTFEnabledClick(Sender: TObject);
begin
  dfRTF.Enabled := chkRTFEnabled.Checked;
end;

procedure TDropFilesDemoForm.chkShapeEnabledClick(Sender: TObject);
begin
  dfShape.Enabled := chkShapeEnabled.Checked;
end;

procedure TDropFilesDemoForm.dfFormDropFiles(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Pred(dfForm.Count) do
    DisplayItem(clBlack, 'File: ' + dfForm.Files[I]);
  DisplayItem(clBlack,
    'Drop info: ' + DropControlInfo(dfForm.DropControl, dfForm.DropPoint));
end;

procedure TDropFilesDemoForm.dfPanelDropFiles(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Pred(dfPanel.Count) do
    DisplayItem(clBlue, 'File: ' + dfPanel.Files[I]);
  DisplayItem(clBlue,
    'Drop info: ' + DropControlInfo(dfPanel.DropControl, dfPanel.DropPoint));
end;

procedure TDropFilesDemoForm.dfRTFDropFiles(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Pred(dfRTF.Count) do
    DisplayItem(clRed, 'File: ' + dfRTF.Files[I]);
  DisplayItem(clRed,
    'Drop info: ' + DropControlInfo(dfRTF.DropControl, dfRTF.DropPoint));
end;

procedure TDropFilesDemoForm.dfShapeDropFiles(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Pred(dfShape.Count) do
    DisplayItem(clGreen, 'File: ' + dfShape.Files[I]);
  DisplayItem(clGreen,
    'Drop info: ' + DropControlInfo(dfShape.DropControl, dfShape.DropPoint));
end;

procedure TDropFilesDemoForm.DisplayItem(C: TColor; S: string);
var
  SelStart: Integer;
begin
  SelStart := Length(RichEdit1.Text);
  RichEdit1.Lines.Add(S);
  RichEdit1.SelStart := SelStart;
  RichEdit1.SelLength := Length(S);
  RichEdit1.SelAttributes.Color := C;
  RichEdit1.SelStart := SelStart + Length(S) + 2;
  RichEdit1.SelLength := 0;
end;

function TDropFilesDemoForm.DropControlInfo(Ctrl: TControl;
  Point: TPoint): string;
begin
  Result := Format('%s at (%d.%d)', [Ctrl.Name, Point.X, Point.Y]);
end;

procedure TDropFilesDemoForm.edExtensionsChange(Sender: TObject);
begin
  PanelExtFilter.Extensions := edExtensions.Text;
end;

procedure TDropFilesDemoForm.edWildcardChange(Sender: TObject);
begin
  RTFWildcardFilter.WildCard := edWildcard.Text;
end;

procedure TDropFilesDemoForm.dfFormFileFilter(Sender: TObject;
  const FileName: String; const IsFolder: Boolean; var Accept: Boolean);
var
  FileDate: Integer;
  FilterDateTime: TDate;//Time;
  FilterDate: Integer;
begin
  FileDate := FileAge(FileName);
  FilterDateTime := Trunc(DateTimePicker1.Date);  // make midnight
  FilterDate := DateTimeToFileDate(FilterDateTime);
  Accept := (FileDate < FilterDate);
  if not Accept and chkFormShowRejections.Checked then
    DisplayItem(
      clBlack,
      'Rejected: ' + FileName + ' - Date: '
      + DateTimeToStr(FileDateToDateTime(FileDate))
    );
end;

procedure TDropFilesDemoForm.chkFormEnableFilterClick(Sender: TObject);
begin
  if chkFormEnableFilter.Checked then
    dfForm.OnFileFilter := dfFormFileFilter
  else
    dfForm.OnFileFilter := nil;
end;

procedure TDropFilesDemoForm.chkPanelEnableFilterClick(Sender: TObject);
begin
  if chkPanelEnableFilter.Checked then
    dfPanel.Filter := PanelExtFilter
  else
    dfPanel.Filter := nil;
end;

procedure TDropFilesDemoForm.chkRTFEnableFilterClick(Sender: TObject);
begin
  if chkRTFEnableFilter.Checked then
    dfRTF.Filter := RTFWildcardFilter
  else
    dfRTF.Filter := nil;
end;

end.
