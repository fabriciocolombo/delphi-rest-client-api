(* Full	Delphi Version Compiler Directives list could be found here: https://gist.github.com/2485849 *)

{$DEFINE USE_INDY}
{$DEFINE USE_WIN_HTTP}
{$DEFINE USE_WIN_INET}
{$DEFINE USE_SUPER_OBJECT}

(*
- Feature Directives

  The features directives are used to test if the compiler supports specific
  features, such as method overloading, and adjust the sources accordingly. Use
  of these directives is preferred over the use of the DELPHI and COMPILER
  directives.

  Directive              Description
  ------------------------------------------------------------------------------
  SUPPORTS_GENERICS              Compiler supports generic implementations (Delphi 2009+)
  SUPPORTS_ANONYMOUS_METHODS     Anonymous methods is available (D2009+)
  HAS_UNIT_RTTI                  Unit RTTI is available (Delphi 2010+)
  HAS_UNIT_JSON                  Unit JSON is available (Delphi XE6+)
  HAS_UNIT_NETENCODING           Unit NetEncoding is available (Delphi XE7+)
  HAS_ENOTSUPPORTED              Exception class ENotSupportedException is available (Delphi 2010+)
  HAS_ENOTIMPLEMENTED            Exception class ENotImplemented is available (Delphi XE+)
  HAS_TTIMEZONE                  TTimeZone class is available (Delphi XE+)
  HAS_WININET_INTERNET_OPTION    WinINet Internet Options is declared (Delphi XE2+)
  HAS_NS                         Namespace is available (Delphi XE2+)
*)

//Basic Version of Compiler Supported
{$IFDEF CONDITIONALEXPRESSIONS}  //Started being defined with Delphi 6
  {$IF Declared(CompilerVersion) and (CompilerVersion < 15.0) } // Before Delphi 7
    {$DEFINE UNSUPPORTED_COMPILER_VERSION}
  {$IFEND}
{$ELSE}
  {$DEFINE UNSUPPORTED_COMPILER_VERSION}
{$ENDIF}

{$IFDEF UNSUPPORTED_COMPILER_VERSION}
  {$Message Fatal 'Unsupported Compiler Version. Delphi 7 or later required!'}
{$ENDIF}

{------------------------------------------------------------------------------}
{ DELPHI VERSIONS                                                              }
{------------------------------------------------------------------------------}

{$IFDEF VER150} // Delphi 7
  {$DEFINE DELPHI_7}
  {$DEFINE DELPHI_7_UP}
{$ENDIF}

{$IFDEF VER185} //Delphi 2007
  {$DEFINE DELPHI_2007}
  {$DEFINE DELPHI_2007_UP}
{$ENDIF VER185}

{$IFDEF VER200} // RAD Studio 2009
  {$DEFINE DELPHI_2009}
  {$DEFINE DELPHI_2009_UP}
{$ENDIF VER200}

{$IFDEF VER210} // RAD Studio 2010
  {$DEFINE DELPHI_2010}
  {$DEFINE DELPHI_2010_UP}
{$ENDIF VER210}

{$IFDEF VER220} // RAD Studio XE
  {$DEFINE DELPHI_XE}
  {$DEFINE DELPHI_XE_UP}
{$ENDIF VER220}

{$IFDEF VER230} // RAD Studio XE2
  {$DEFINE DELPHI_XE2}
  {$DEFINE DELPHI_XE2_UP}
{$ENDIF VER230}

{$IFDEF VER240} // RAD Studio XE3
  {$DEFINE DELPHI_XE3}
  {$DEFINE DELPHI_XE3_UP}
{$ENDIF VER240}

{$IFDEF VER250} // RAD Studio XE4
  {$DEFINE DELPHI_XE4}
  {$DEFINE DELPHI_XE4_UP}
{$ENDIF VER250}

{$IFDEF VER260} // RAD Studio XE5
  {$DEFINE DELPHI_XE5}
  {$DEFINE DELPHI_XE5_UP}
{$ENDIF VER260}

{$IFDEF VER270} // RAD Studio XE6
  {$DEFINE DELPHI_XE6}
  {$DEFINE DELPHI_XE6_UP}
{$ENDIF VER270}

{$IFDEF VER280} // RAD Studio XE7
  {$DEFINE DELPHI_XE7}
  {$DEFINE DELPHI_XE7_UP}
{$ENDIF VER280}

{$IFDEF VER290} // RAD Studio XE8
  {$DEFINE DELPHI_XE8}
  {$DEFINE DELPHI_XE8_UP}
{$ENDIF VER290}

{$IFDEF VER300}  // RAD Studio 10 Seattle
  {$DEFINE DELPHI_10SEATTLE}
  {$DEFINE DELPHI_10SEATTLE_UP}
{$ENDIF}

{$IFDEF VER310}  // RAD Studio 10 Berlin
  {$DEFINE DELPHI_10BERLIN}
  {$DEFINE DELPHI_10BERLIN_UP}
{$ENDIF}

{$IFDEF VER320}  // RAD Studio 10 Tokyo
  {$DEFINE DELPHI_10TOKYO}
  {$DEFINE DELPHI_10TOKYO_UP}
{$ENDIF}


//Provide compatibility for the next version
{$IF Declared(CompilerVersion) and (CompilerVersion > 30.0)} // Delphi 10 Seattle
  {$DEFINE DELPHI_10SEATTLE}
  {$DEFINE DELPHI_10SEATTLE_UP}
{$IFEND}

{------------------------------------------------------------------------------}
{ DELPHI_**_UP VERSIONS                                                          }
{------------------------------------------------------------------------------}

{$IFDEF DELPHI_10TOKYO_UP}
  {$DEFINE DELPHI_10BERLIN_UP}
{$ENDIF}

{$IFDEF DELPHI_10BERLIN_UP}
  {$DEFINE DELPHI_10SEATTLE_UP}
{$ENDIF}

{$IFDEF DELPHI_10SEATTLE_UP}
  {$DEFINE DELPHI_XE8_UP}
{$ENDIF}

{$IFDEF DELPHI_XE8_UP}
  {$DEFINE DELPHI_XE7_UP}
{$ENDIF}

{$IFDEF DELPHI_XE7_UP}
  {$DEFINE DELPHI_XE6_UP}
{$ENDIF}

{$IFDEF DELPHI_XE6_UP}
  {$DEFINE DELPHI_XE5_UP}
{$ENDIF}

{$IFDEF DELPHI_XE5_UP}
  {$DEFINE DELPHI_XE4_UP}
{$ENDIF}

{$IFDEF DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE3_UP}
{$ENDIF}

{$IFDEF DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE2_UP}
{$ENDIF}

{$IFDEF DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE_UP}
{$ENDIF}

{$IFDEF DELPHI_XE_UP}
  {$DEFINE DELPHI_2010_UP}
{$ENDIF}

{$IFDEF DELPHI_2010_UP}
  {$DEFINE DELPHI_2009_UP}
{$ENDIF}

{$IFDEF DELPHI_2009_UP}
  {$DEFINE DELPHI_2007_UP}
{$ENDIF}

{$IFDEF DELPHI_2007_UP}
  {$DEFINE DELPHI_7_UP}
{$ENDIF}

{------------------------------------------------------------------------------}
{ FEATURE DIRECTIVES                                                   }
{------------------------------------------------------------------------------}

{$IFDEF DELPHI_2009_UP}
  {$DEFINE SUPPORTS_GENERICS}
  {$DEFINE SUPPORTS_ANONYMOUS_METHODS}
{$ENDIF}

{$IFDEF DELPHI_2010_UP}
  {$DEFINE HAS_UNIT_RTTI}
  {$DEFINE HAS_ENOTSUPPORTED}
{$ENDIF}

{$IFDEF DELPHI_XE_UP}
  {$DEFINE HAS_ENOTIMPLEMENTED}
  {$DEFINE HAS_TTIMEZONE}
{$ENDIF}

{$IFDEF DELPHI_XE2_UP}
  {$DEFINE HAS_NS}
  {$DEFINE HAS_WININET_INTERNET_OPTION}
{$ENDIF}

{$IFDEF DELPHI_XE6_UP}
  {$DEFINE HAS_UNIT_JSON}
{$ENDIF}

{$IFDEF DELPHI_XE7_UP}
  {$DEFINE HAS_UNIT_NETENCODING}
{$ENDIF}

{$IFDEF MACOS}
  {$UNDEF USE_SUPER_OBJECT}
  {$UNDEF USE_WIN_HTTP}
  {$UNDEF USE_WIN_INET}  
{$ENDIF}

