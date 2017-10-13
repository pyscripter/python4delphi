(**************************************************************************)
(*                                                                        *)
(* Module:   'Definition.Inc'          Copyright (c) 1998                 *)
(*                                                                        *)
(* Version: 1.5                        Dr. Dietmar Budelsky               *)
(*                                     budelsky@ibs.bm.eunet.de           *)
(*                                     IBS Schillings GmbH & Co KG        *)
(*                                     Ein Unternehmen der KROHNE-Gruppe  *)
(*                                     Heisenbergstr. 18                  *)
(*                                     50169 Kerpen-Türnich               *)
(*                                     Phone: (49)22 37/97 44-0           *)
(*                                                                        *)
(**************************************************************************)
(*  Changes for Delphi 4 or higher are made for Compilers higher          *)
(*  than Version 10.0.                                                     *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free, as long as this  *)
(* header and its copyright text is intact.                               *)
(* Dr. Dietmar Budelsky, 1998-01-07                                       *)
(**************************************************************************)

/////////////////////////////////////////////////////////////////////////////
// Select a Python version by commenting out the corresponding line, or
// modify the default Python version below, in the Python versions section.
/////////////////////////////////////////////////////////////////////////////

//{$DEFINE PYTHON23}
//{$DEFINE PYTHON24}
//{$DEFINE PYTHON25}
//{$DEFINE PYTHON26}
//{$DEFINE PYTHON27}
//{$DEFINE PYTHON30}
//{$DEFINE PYTHON31}
//{$DEFINE PYTHON32}
//{$DEFINE PYTHON33}
//{$DEFINE PYTHON34}
//{$DEFINE PYTHON35}

/////////////////////////////////////////////////////////////////////////////
// Python for Delphi settings. Comment out the setting you want to enable.
/////////////////////////////////////////////////////////////////////////////

{$DEFINE PREFER_UNICODE} // this will convert a variant containing an OleStr to a UniCode Python string.

/////////////////////////////////////////////////////////////////////////////
// OS symbols. Note that MSWINDOWS is defined from Delphi6/Kylix.
/////////////////////////////////////////////////////////////////////////////

{$IFNDEF MSWINDOWS}
  {$IFDEF WIN32}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

/////////////////////////////////////////////////////////////////////////////
// Delphi, C++ Builder and Kylix versions
/////////////////////////////////////////////////////////////////////////////

{$IFDEF VER90} // Delphi 2.0
  {$DEFINE DELPHI2}
{$ENDIF}
{$IFDEF VER93} // C++ Builder 1.0
  {$DEFINE CBUILDER1}
{$ENDIF}
{$IFDEF VER100} // Delphi 3.0
  {$DEFINE DELPHI3}
{$ENDIF}
{$IFDEF VER110} // C++ Builder 3.0
  {$DEFINE CBUILDER3}
  {$DEFINE DELPHI3_OR_HIGHER}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER120} // Delphi 4.0
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI4_OR_HIGHER}
{$ENDIF}
{$IFDEF VER125} // C++ Builder 4.0
  {$DEFINE CBUILDER4}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER130} // Delphi 5.0
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$IFDEF BCB}  { Borland C++Builder 5.0 }
    {$DEFINE CBUILDER3}
    {$DEFINE CBUILDER4}
    {$DEFINE CBUILDER5}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}
{$IFDEF VER140} // Delphi 6.0
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$IFDEF BCB}  { Borland C++Builder 6.0 }
    {$DEFINE CBUILDER3}
    {$DEFINE CBUILDER4}
    {$DEFINE CBUILDER5}
    {$DEFINE CBUILDER6}
    {$ObjExportAll On}
  {$ENDIF} 
{$ENDIF}
{$IFDEF VER150} // Delphi 7.0
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
{$ENDIF}
{$IFDEF VER160} // Delphi 8
  {$DEFINE DELPHI8}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
{$ENDIF}
{$IFDEF VER170} // Delphi 2005
  {$DEFINE DELPHI2005}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
{$ENDIF}
{$IFDEF VER180} // Delphi 2006
  {$DEFINE DELPHI2006}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
{$ENDIF}
{$IFDEF VER190} // Delphi 2007
  {$DEFINE DELPHI2007}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
{$ENDIF}
{$IFDEF VER200} // Delphi 2009
  {$DEFINE DELPHI2009}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
{$ENDIF}
{$IFDEF VER210} // Delphi 2010
  {$DEFINE DELPHI2010}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
{$ENDIF}
{$IFDEF VER220} // Delphi XE
  {$DEFINE DELPHIXE}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
{$ENDIF}
{$IFDEF VER230} // Delphi XE2
  {$DEFINE DELPHIXE2}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
{$ENDIF}
{$IFDEF VER240} // Delphi XE3
  {$DEFINE DELPHIXE3}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
{$ENDIF}
{$IFDEF VER250} // Delphi XE4
  {$DEFINE DELPHIXE4}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
{$ENDIF}
{$IFDEF VER260} // Delphi XE5
  {$DEFINE DELPHIXE5}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
{$ENDIF}
{$IFDEF VER270} // Delphi XE6
  {$DEFINE DELPHIXE6}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
{$ENDIF}
{$IFDEF VER280} // Delphi XE7
  {$DEFINE DELPHIXE7}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
{$ENDIF}
{$IFDEF VER290} // Delphi XE8
  {$DEFINE DELPHIXE8}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$DEFINE DELPHI2007_OR_HIGHER}
  {$DEFINE DELPHI2009_OR_HIGHER}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
  {$DEFINE DELPHIXE8_OR_HIGHER}
{$ENDIF}
/////////////////////////////////////////////////////////////////////////////
// Python versions
/////////////////////////////////////////////////////////////////////////////

// Here we select a default Python version, if no version was explicitely specified.
// Note that the installer will let the user specify its default Python version, and
// thus will edit this file.

{$IFNDEF PYTHON35}
  {$IFNDEF PYTHON34}
    {$IFNDEF PYTHON33}
      {$IFNDEF PYTHON32}
        {$IFNDEF PYTHON31}
          {$IFNDEF PYTHON30}
            {$IFNDEF PYTHON27}
              {$IFNDEF PYTHON26}
                {$IFNDEF PYTHON25}
                  {$IFNDEF PYTHON24}
                    {$IFNDEF PYTHON23}
              {---<START OF DEFAULT PYTHON VERSION>---}
                  {$DEFINE PYTHON27}
              {---<END OF DEFAULT PYTHON VERSION>---}
                    {$ENDIF}
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


{$IFDEF PYTHON23}
  {$DEFINE PYTHON23_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON24}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON25}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON26}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON27}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON27_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON30}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON31}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
  {$DEFINE PYTHON31_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON32}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
  {$DEFINE PYTHON31_OR_HIGHER}
  {$DEFINE PYTHON32_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON33}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
  {$DEFINE PYTHON31_OR_HIGHER}
  {$DEFINE PYTHON32_OR_HIGHER}
  {$DEFINE PYTHON33_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON34}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
  {$DEFINE PYTHON31_OR_HIGHER}
  {$DEFINE PYTHON32_OR_HIGHER}
  {$DEFINE PYTHON33_OR_HIGHER}
  {$DEFINE PYTHON34_OR_HIGHER}
{$ENDIF}

/////////////////////////////////////////////////////////////////////////////
// Misc
/////////////////////////////////////////////////////////////////////////////

{$IFDEF WIN32}
  {$DEFINE CPUX86}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$IFDEF CPU64}
    {$DEFINE CPUX64}
  {$ENDIF CPU64}
  {$IFDEF CPU32}
    {$DEFINE CPUX86}
  {$ENDIF CPU32}
  {$IFDEF DARWIN}
    {$DEFINE MACOS}
    {$DEFINE ALIGN_STACK}
    {$IFDEF CPU32}
      {$DEFINE MACOS32}
    {$ENDIF CPU32}
  {$ENDIF DARWIN}
  {$H+}
{$ENDIF FPC}


