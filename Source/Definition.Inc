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
// Delphi, C++ Builder versions
/////////////////////////////////////////////////////////////////////////////

{$IFDEF VER210} // Delphi 2010
  {$DEFINE DELPHI2010}
  {$DEFINE DELPHI2010_OR_HIGHER}
{$ENDIF}
{$IFDEF VER220} // Delphi XE
  {$DEFINE DELPHIXE}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
{$ENDIF}
{$IFDEF VER230} // Delphi XE2
  {$DEFINE DELPHIXE2}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
{$ENDIF}
{$IFDEF VER240} // Delphi XE3
  {$DEFINE DELPHIXE3}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
{$ENDIF}
{$IFDEF VER250} // Delphi XE4
  {$DEFINE DELPHIXE4}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
{$ENDIF}
{$IFDEF VER260} // Delphi XE5
  {$DEFINE DELPHIXE5}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
{$ENDIF}
{$IFDEF VER270} // Delphi XE6
  {$DEFINE DELPHIXE6}
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
{$IFDEF VER300} // Delphi 10
  {$DEFINE DELPHI10}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
  {$DEFINE DELPHIXE8_OR_HIGHER}
  {$DEFINE DELPHIX10_OR_HIGHER}
{$ENDIF}
{$IFDEF VER310} // Delphi 10.1
  {$DEFINE DELPHI10_1}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
  {$DEFINE DELPHIXE8_OR_HIGHER}
  {$DEFINE DELPHI10_OR_HIGHER}
  {$DEFINE DELPHI10_1_OR_HIGHER}
{$ENDIF}
{$IFDEF VER320} // Delphi 10.2
  {$DEFINE DELPHI10_2}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
  {$DEFINE DELPHIXE8_OR_HIGHER}
  {$DEFINE DELPHI10_OR_HIGHER}
  {$DEFINE DELPHI10_1_OR_HIGHER}
  {$DEFINE DELPHI10_2_OR_HIGHER}
{$ENDIF}
{$IFDEF VER330} // Delphi 10.3
  {$DEFINE DELPHI10_3}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
  {$DEFINE DELPHIXE8_OR_HIGHER}
  {$DEFINE DELPHI10_OR_HIGHER}
  {$DEFINE DELPHI10_1_OR_HIGHER}
  {$DEFINE DELPHI10_2_OR_HIGHER}
  {$DEFINE DELPHI10_3_OR_HIGHER}
{$ENDIF}
{$IFDEF VER340} // Delphi 10.4
  {$DEFINE DELPHI10_4}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
  {$DEFINE DELPHIXE8_OR_HIGHER}
  {$DEFINE DELPHI10_OR_HIGHER}
  {$DEFINE DELPHI10_1_OR_HIGHER}
  {$DEFINE DELPHI10_2_OR_HIGHER}
  {$DEFINE DELPHI10_3_OR_HIGHER}
  {$DEFINE DELPHI10_4_OR_HIGHER}
  {$IF RTLVERSION1042}
    {$DEFINE DELPHI10_4_2} //Delphi 10.4.2
  {$ENDIF RTLVERSION1042}
{$ENDIF}
{$IFDEF VER350}  // Delphi 11
  {$DEFINE DELPHI11}
  {$DEFINE DELPHI2010_OR_HIGHER}
  {$DEFINE DELPHIXE_OR_HIGHER}
  {$DEFINE DELPHIXE2_OR_HIGHER}
  {$DEFINE DELPHIXE3_OR_HIGHER}
  {$DEFINE DELPHIXE4_OR_HIGHER}
  {$DEFINE DELPHIXE5_OR_HIGHER}
  {$DEFINE DELPHIXE6_OR_HIGHER}
  {$DEFINE DELPHIXE7_OR_HIGHER}
  {$DEFINE DELPHIXE8_OR_HIGHER}
  {$DEFINE DELPHI10_OR_HIGHER}
  {$DEFINE DELPHI10_1_OR_HIGHER}
  {$DEFINE DELPHI10_2_OR_HIGHER}
  {$DEFINE DELPHI10_3_OR_HIGHER}
  {$DEFINE DELPHI10_4_OR_HIGHER}
  {$DEFINE DELPHI11_OR_HIGHER}
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
    {$DEFINE CPU64BITS}
  {$ENDIF CPU64}

  {$IFDEF CPU86}
    {$DEFINE CPUX86}
  {$ENDIF CPU86}

  {$IFDEF DARWIN}
    {$DEFINE OSX}
    {$DEFINE MACOS}
    {$IFDEF CPU32}
      {$DEFINE MACOS32}
    {$ENDIF CPU32}
    {$IFDEF CPU64}
      {$DEFINE MACOS64}
      {$DEFINE OSX64}
    {$ENDIF CPU64}
  {$ENDIF DARWIN}

  {$IFDEF CPUARM}
    {$DEFINE CPUARM32}
  {$ENDIF}

  {$IFDEF CPUAARCH64}
    {$DEFINE CPUARM}
    {$DEFINE CPUARM64}
  {$ENDIF}
  
  {$IFDEF UNIX}
    {$DEFINE POSIX}
  {$ENDIF UNIX}

  {$IFDEF ANDROID}
    {$DEFINE POSIX}
  {$ENDIF ANDROID}
{$ENDIF FPC}

{$IFDEF DELPHIXE_OR_HIGHER}
  {$DEFINE EXTENDED_RTTI}
{$ENDIF DELPHIXE_OR_HIGHER}

{$IFDEF DELPHIXE4_OR_HIGHER}
  {$LEGACYIFEND ON}
{$ENDIF DELPHIXE4_OR_HIGHER}

{$IFDEF MACOS}
  {$DEFINE DARWIN}
{$ENDIF MACOS}
