{
 * FmMDIChild.pas
 *
 * MDI child form for the Window State Components MDIChild demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

unit FmMDIChild;

interface

uses
  Classes, Controls, StdCtrls, Forms,
  PJWdwState, ExtCtrls;

type
  TForm2 = class(TForm)
    PJWdwState1: TPJWdwState;
    Label1: TLabel;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.

