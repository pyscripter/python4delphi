// Copyright 2000 by Sigve Tjora and others as specified in PythonEngine.pas

// For information about me and my projects see:
// See http://www.tjora.no or mail me at public@tjora.no

// This file makes the PyObjectAsVariant work for all kinds of PyObjects via
// TPythonAtom

(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)


unit AtomPythonEngine;

{$I Definition.Inc}

interface

uses
{$IFDEF DELPHI6_OR_HIGHER}
  Variants,
{$ENDIF}
  PythonEngine;

type
  TAtomPythonEngine = class(TPythonEngine)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    function PyObjectAsVariant( obj : PPyObject ) : Variant; override;
  published
    { Published declarations }
  end {$IFDEF DELPHI6_OR_HIGHER}deprecated{$ENDIF};

procedure Register;

implementation

uses
  Classes, SysUtils, PythonAtom;

{ TAtomPythonEngine }

function TAtomPythonEngine.PyObjectAsVariant( obj : PPyObject ) : Variant;
begin
  Result := inherited PyObjectAsVariant( obj );
  if VarIsNull(Result) and (obj <> Py_None) then
    Result := getAtom(obj);
end;



procedure Register;
begin
{$WARNINGS OFF}
  RegisterComponents('Python', [TAtomPythonEngine]);
{$WARNINGS ON}
end;

end.
