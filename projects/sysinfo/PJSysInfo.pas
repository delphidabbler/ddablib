{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2001-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * This unit contains various static classes, constant and type definitions and
 * global variables for use in providing information about the computer and the
 * operating system.
 *
 * NOTE 1: When compiled with old versions of Delphi that do not support setting
 * registry access flags via the TRegistry object, some of this code may not
 * work correctly when running as a 32 bit process on 64 bit Windows.
 *
 * NOTE 2: The code has not been tested when compiled using the Delphi 64 bit
 * compiler, but is expected to require some changes before it will work
 * correctly.
 *
 * Thanks to the following who have contributed to this project:
 *
 *   Guillermo Fazzolari (bug fix in v2.0.1)
 *   Laurent Pierre (PRODUCT_* constants and suggested GetProductInfo API code
 *     used in v3.0)
 *   Rich Habedank (bug fix in revision 228)
 *
 * ***** END LICENSE BLOCK *****
}


unit PJSysInfo;


// Conditional defines

// Assume all required facilities available
{$DEFINE REGOPENREADONLY}     // TRegistry.OpenKeyReadOnly available
{$DEFINE REGACCESSFLAGS}      // TRegistry access flags available
{$DEFINE WARNDIRS}            // $WARN compiler directives available
{$DEFINE EXCLUDETRAILING}     // SysUtils.ExcludeTrailingPathDelimiter available
{$DEFINE HASLONGWORD}         // LongWord type defined

// Undefine facilities not available in earlier compilers
// Note: Delphi 1/2 is not included since the code will not compile on these
// compilers
{$IFDEF VER100} // Delphi 3
  {$UNDEF REGOPENREADONLY}
  {$UNDEF REGACCESSFLAGS}
  {$UNDEF WARNDIRS}
  {$UNDEF EXCLUDETRAILING}
  {$UNDEF HASLONGWORD}
{$ENDIF}
{$IFDEF VER120} // Delphi 4
  {$UNDEF REGACCESSFLAGS}
  {$UNDEF WARNDIRS}
  {$UNDEF EXCLUDETRAILING}
{$ENDIF}
{$IFDEF VER130} // Delphi 5
  {$UNDEF REGACCESSFLAGS}
  {$UNDEF WARNDIRS}
  {$UNDEF EXCLUDETRAILING}  // ** fix by Rich Habedank
{$ENDIF}
{$IFDEF VER140} // Delphi 6
  {$UNDEF WARNDIRS}
{$ENDIF}

// Switch off "unsafe" warnings for this unit
{$IFDEF WARNDIRS}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
{$ENDIF}


interface


uses
  // Delphi
  SysUtils, Classes, Windows;


type
  // Windows types not defined in all supported Delphi VCLs

  // ANSI versions of the Win API OSVERSIONINFOEX structure and pointers
  _OSVERSIONINFOEXA = packed record
    dwOSVersionInfoSize: DWORD;               // size of structure
    dwMajorVersion: DWORD;                    // major OS version number
    dwMinorVersion: DWORD;                    // minor OS version number
    dwBuildNumber: DWORD;                     // OS build number
    dwPlatformId: DWORD;                      // OS platform identifier
    szCSDVersion: array[0..127] of AnsiChar;  // service pack or extra info
    wServicePackMajor: WORD;                  // service pack major version no.
    wServicePackMinor: WORD;                  // service pack minor version no.
    wSuiteMask: WORD;                         // bitmask that stores OS suite(s)
    wProductType: Byte;                       // additional info about system
    wReserved: Byte;                          // reserved for future use
  end;
  OSVERSIONINFOEXA = _OSVERSIONINFOEXA;
  TOSVersionInfoExA = _OSVERSIONINFOEXA;
  POSVersionInfoExA = ^TOSVersionInfoExA;

  // Unicode versions of the Win API OSVERSIONINFOEX structure and pointers
  _OSVERSIONINFOEXW = packed record
    dwOSVersionInfoSize: DWORD;               // size of structure
    dwMajorVersion: DWORD;                    // major OS version number
    dwMinorVersion: DWORD;                    // minor OS version number
    dwBuildNumber: DWORD;                     // OS build number
    dwPlatformId: DWORD;                      // OS platform identifier
    szCSDVersion: array[0..127] of WideChar;  // service pack or extra info
    wServicePackMajor: WORD;                  // service pack major version no.
    wServicePackMinor: WORD;                  // service pack minor version no.
    wSuiteMask: WORD;                         // bitmask that stores OS suite(s)
    wProductType: Byte;                       // additional info about system
    wReserved: Byte;                          // reserved for future use
  end;
  OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
  TOSVersionInfoExW = _OSVERSIONINFOEXW;
  POSVersionInfoExW = ^TOSVersionInfoExW;

  // Default version of the Win API OSVERSIONINFOEX structure.
  // UNICODE is defined when the Unicode API is used, so we use this to decide
  // which structure to use as default.
  {$IFDEF UNICODE}
  _OSVERSIONINFOEX = _OSVERSIONINFOEXW;
  OSVERSIONINFOEX = OSVERSIONINFOEXW;
  TOSVersionInfoEx = TOSVersionInfoExW;
  POSVersionInfoEx = POSVersionInfoExW;
  {$ELSE}
  _OSVERSIONINFOEX = _OSVERSIONINFOEXA;
  OSVERSIONINFOEX = OSVERSIONINFOEXA;
  TOSVersionInfoEx = TOSVersionInfoExA;
  POSVersionInfoEx = POSVersionInfoExA;
  {$ENDIF}

const
  // Windows constants possibly not defined in all supported Delphi VCLs

  // These Windows-defined constants are required for use with TOSVersionInfoEx
  // NT Product types
  VER_NT_WORKSTATION                          = 1;
  VER_NT_DOMAIN_CONTROLLER                    = 2;
  VER_NT_SERVER                               = 3;
  // Mask representing NT product suites
  VER_SUITE_SMALLBUSINESS                     = $00000001;
  VER_SUITE_ENTERPRISE                        = $00000002;
  VER_SUITE_BACKOFFICE                        = $00000004;
  VER_SUITE_COMMUNICATIONS                    = $00000008;
  VER_SUITE_TERMINAL                          = $00000010;
  VER_SUITE_SMALLBUSINESS_RESTRICTED          = $00000020;
  VER_SUITE_EMBEDDEDNT                        = $00000040;
  VER_SUITE_DATACENTER                        = $00000080;
  VER_SUITE_SINGLEUSERTS                      = $00000100;
  VER_SUITE_PERSONAL                          = $00000200;
  VER_SUITE_SERVERAPPLIANCE                   = $00000400;
  VER_SUITE_BLADE                             = VER_SUITE_SERVERAPPLIANCE;
  VER_SUITE_EMBEDDED_RESTRICTED               = $00000800;
  VER_SUITE_SECURITY_APPLIANCE                = $00001000;
  VER_SUITE_STORAGE_SERVER                    = $00002000;
  VER_SUITE_COMPUTE_SERVER                    = $00004000;
  VER_SUITE_WH_SERVER                         = $00008000;

  // These Windows-defined constants are required for use with the
  // GetProductInfo API call (Vista and later)
  // ** Thanks to Laurent Pierre for providing these defintions.
  // ** Additional definitions were obtained from
  //    http://msdn.microsoft.com/en-us/library/ms724358
  PRODUCT_BUSINESS                            = $00000006;
  PRODUCT_BUSINESS_N                          = $00000010;
  PRODUCT_CLUSTER_SERVER                      = $00000012;
  PRODUCT_CLUSTER_SERVER_V                    = $00000040;
  PRODUCT_CORE                                = $00000065;
  PRODUCT_CORE_N                              = $00000062;
  PRODUCT_CORE_COUNTRYSPECIFIC                = $00000063;
  PRODUCT_CORE_SINGLELANGUAGE                 = $00000064;
  PRODUCT_DATACENTER_EVALUATION_SERVER        = $00000050;
  PRODUCT_DATACENTER_SERVER                   = $00000008;
  PRODUCT_DATACENTER_SERVER_CORE              = $0000000C;
  PRODUCT_DATACENTER_SERVER_CORE_V            = $00000027;
  PRODUCT_DATACENTER_SERVER_V                 = $00000025;
  PRODUCT_ENTERPRISE                          = $00000004;
  PRODUCT_ENTERPRISE_E                        = $00000046;
  PRODUCT_ENTERPRISE_N_EVALUATION             = $00000054;
  PRODUCT_ENTERPRISE_N                        = $0000001B;
  PRODUCT_ENTERPRISE_EVALUATION               = $00000048;
  PRODUCT_ENTERPRISE_SERVER                   = $0000000A;
  PRODUCT_ENTERPRISE_SERVER_CORE              = $0000000E;
  PRODUCT_ENTERPRISE_SERVER_CORE_V            = $00000029;
  PRODUCT_ENTERPRISE_SERVER_IA64              = $0000000F;
  PRODUCT_ENTERPRISE_SERVER_V                 = $00000026;
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT       = $0000003B;
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL       = $0000003C;
  PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC    = $0000003D;
  PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC    = $0000003E;
  PRODUCT_HOME_BASIC                          = $00000002;
  PRODUCT_HOME_BASIC_E                        = $00000043;
  PRODUCT_HOME_BASIC_N                        = $00000005;
  PRODUCT_HOME_PREMIUM                        = $00000003;
  PRODUCT_HOME_PREMIUM_E                      = $00000044;
  PRODUCT_HOME_PREMIUM_N                      = $0000001A;
  PRODUCT_HOME_PREMIUM_SERVER                 = $00000022;
  PRODUCT_HOME_SERVER                         = $00000013;
  PRODUCT_HYPERV                              = $0000002A;
  PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT    = $0000001E;
  PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING     = $00000020;
  PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY      = $0000001F;
  PRODUCT_MULTIPOINT_STANDARD_SERVER          = $0000004C;
  PRODUCT_MULTIPOINT_PREMIUM_SERVER           = $0000004D;
  PRODUCT_PROFESSIONAL                        = $00000030;
  PRODUCT_PROFESSIONAL_E                      = $00000045;
  PRODUCT_PROFESSIONAL_N                      = $00000031;
  PRODUCT_PROFESSIONAL_WMC                    = $00000067;
  PRODUCT_SB_SOLUTION_SERVER_EM               = $00000036;
  PRODUCT_SERVER_FOR_SB_SOLUTIONS             = $00000033;
  PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM          = $00000037;
  PRODUCT_SERVER_FOR_SMALLBUSINESS            = $00000018;
  PRODUCT_SERVER_FOR_SMALLBUSINESS_V          = $00000023;
  PRODUCT_SERVER_FOUNDATION                   = $00000021;
  PRODUCT_SB_SOLUTION_SERVER                  = $00000032;
  PRODUCT_SMALLBUSINESS_SERVER                = $00000009;
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM        = $00000019;
  PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE   = $0000003F;
  PRODUCT_SOLUTION_EMBEDDEDSERVER             = $00000038;
  PRODUCT_STANDARD_EVALUATION_SERVER          = $0000004F;
  PRODUCT_STANDARD_SERVER                     = $00000007;
  PRODUCT_STANDARD_SERVER_CORE                = $0000000D;
  PRODUCT_STANDARD_SERVER_V                   = $00000024;
  PRODUCT_STANDARD_SERVER_CORE_V              = $00000028;
  PRODUCT_STANDARD_SERVER_SOLUTIONS           = $00000034;
  PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE      = $00000035;
  PRODUCT_STARTER                             = $0000000B;
  PRODUCT_STARTER_E                           = $00000042;
  PRODUCT_STARTER_N                           = $0000002F;
  PRODUCT_STORAGE_ENTERPRISE_SERVER           = $00000017;
  PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE      = $0000002E;
  PRODUCT_STORAGE_EXPRESS_SERVER              = $00000014;
  PRODUCT_STORAGE_EXPRESS_SERVER_CORE         = $0000002B;
  PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER  = $00000060;
  PRODUCT_STORAGE_STANDARD_SERVER             = $00000015;
  PRODUCT_STORAGE_STANDARD_SERVER_CORE        = $0000002C;
  PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER = $0000005F;
  PRODUCT_STORAGE_WORKGROUP_SERVER            = $00000016;
  PRODUCT_STORAGE_WORKGROUP_SERVER_CORE       = $0000002D;
  PRODUCT_UNDEFINED                           = $00000000;
  PRODUCT_ULTIMATE                            = $00000001;
  PRODUCT_ULTIMATE_E                          = $00000047;
  PRODUCT_ULTIMATE_N                          = $0000001C;
  PRODUCT_WEB_SERVER                          = $00000011;
  PRODUCT_WEB_SERVER_CORE                     = $0000001D;
  PRODUCT_UNLICENSED                          = $ABCDABCD;

  // These constants are required for use with GetSystemMetrics to detect
  // certain editions. GetSystemMetrics returns non-zero when passed these flags
  // if the associated edition is present.
  // Obtained from http://msdn.microsoft.com/en-us/library/ms724385
  SM_TABLETPC       = 86;     // Detects XP Tablet Edition
  SM_MEDIACENTER    = 87;     // Detects XP Media Center Edition
  SM_STARTER        = 88;     // Detects XP Starter Edition
  SM_SERVERR2       = 89;     // Detects Windows Server 2003 R2
  SM_REMOTESESSION  = $1000;  // Detects a remote terminal server session

  // These constants are required when examining the
  // TSystemInfo.wProcessorArchitecture member.
  // Only constants marked * are defined in the MS 2008 SDK
  PROCESSOR_ARCHITECTURE_UNKNOWN    = $FFFF; // Unknown architecture.
  PROCESSOR_ARCHITECTURE_INTEL          = 0; // x86 *
  PROCESSOR_ARCHITECTURE_MIPS           = 1; // MIPS architecture
  PROCESSOR_ARCHITECTURE_ALPHA          = 2; // Alpha architecture
  PROCESSOR_ARCHITECTURE_PPC            = 3; // PPC architecture
  PROCESSOR_ARCHITECTURE_SHX            = 4; // SHX architecture
  PROCESSOR_ARCHITECTURE_ARM            = 5; // ARM  architecture
  PROCESSOR_ARCHITECTURE_IA64           = 6; // Intel Itanium Processor Family *
  PROCESSOR_ARCHITECTURE_ALPHA64        = 7; // Alpha64 architecture
  PROCESSOR_ARCHITECTURE_MSIL           = 8; // MSIL architecture
  PROCESSOR_ARCHITECTURE_AMD64          = 9; // x64 (AMD or Intel) *
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10; // IA32 on Win64 architecture

  // These constants are provided in case the obsolete
  // TSystemInfo.dwProcessorType needs to be used.
  // Constants marked Windows CE are only used on Windows mobile and are only
  // provided here for completeness.
  // Only constants marked * are defined in MS SDK 6.1
  PROCESSOR_INTEL_386     = 386;   // Intel i386 processor *
  PROCESSOR_INTEL_486     = 486;   // Intel i486 processor *
  PROCESSOR_INTEL_PENTIUM = 586;   // Intel Pentium processor *
  PROCESSOR_INTEL_IA64    = 2200;  // Intel IA64 processor *
  PROCESSOR_AMD_X8664     = 8664;  // AMD X86 64 processor *
  PROCESSOR_MIPS_R4000    = 4000;  // MIPS R4000, R4101, R3910 processor
  PROCESSOR_ALPHA_21064   = 21064; // Alpha 210 64 processor
  PROCESSOR_PPC_601       = 601;   // PPC 601 processor
  PROCESSOR_PPC_603       = 603;   // PPC 603 processor
  PROCESSOR_PPC_604       = 604;   // PPC 604 processor
  PROCESSOR_PPC_620       = 620;   // PPC 620 processor
  PROCESSOR_HITACHI_SH3   = 10003; // Hitachi SH3 processor (Windows CE)
  PROCESSOR_HITACHI_SH3E  = 10004; // Hitachi SH3E processor (Windows CE)
  PROCESSOR_HITACHI_SH4   = 10005; // Hitachi SH4 processor (Windows CE)
  PROCESSOR_MOTOROLA_821  = 821;   // Motorola 821 processor (Windows CE)
  PROCESSOR_SHx_SH3       = 103;   // SHx SH3 processor (Windows CE)
  PROCESSOR_SHx_SH4       = 104;   // SHx SH4 processor (Windows CE)
  PROCESSOR_STRONGARM     = 2577;  // StrongARM processor (Windows CE)
  PROCESSOR_ARM720        = 1824;  // ARM 720 processor (Windows CE)
  PROCESSOR_ARM820        = 2080;  // ARM 820 processor (Windows CE)
  PROCESSOR_ARM920        = 2336;  // ARM 920 processor (Windows CE)
  PROCESSOR_ARM_7TDMI     = 70001; // ARM 7TDMI processor (Windows CE)
  PROCESSOR_OPTIL         = $494F; // MSIL processor


type
  ///  <summary>Enumeration of OS platforms.</summary>
  TPJOSPlatform = (
    ospWinNT,               // Windows NT platform
    ospWin9x,               // Windows 9x platform
    ospWin32s               // Win32s platform
  );

type
  ///  <summary>Enumeration identifying OS product.</summary>
  ///  <remarks>New values are always appended to the end of the enumeration so
  ///  as not to destroy any existing code that depends on the ordinal value of
  ///  the existing values.</remarks>
  TPJOSProduct = (
    osUnknownWinNT,         // Unknown Windows NT OS
    osWinNT,                // Windows NT (up to v4)
    osWin2K,                // Windows 2000
    osWinXP,                // Windows XP
    osUnknownWin9x,         // Unknown Windows 9x OS
    osWin95,                // Windows 95
    osWin98,                // Windows 98
    osWinMe,                // Windows Me
    osUnknownWin32s,        // Unknown OS running Win32s
    osWinSvr2003,           // Windows Server 2003
    osUnknown,              // Completely unknown Windows
    osWinVista,             // Windows Vista
    osWinSvr2003R2,         // Windows Server 2003 R2
    osWinSvr2008,           // Windows Server 2008
    osWinLater,             // A later version of Windows than v6.1
    osWin7,                 // Windows 7
    osWinSvr2008R2,         // Windows Server 2008 R2
    osWin8,                 // Windows 8
    osWinSvr2012            // Windows Server 2012
  );

type
  ///  <summary>Enumeration identifying processor architecture.</summary>
  TPJProcessorArchitecture = (
    paUnknown,              // Unknown architecture
    paX64,                  // X64 (AMD or Intel)
    paIA64,                 // Intel Itanium processor family (IPF)
    paX86                   // Intel 32 bit
  );

type
  ///  <summary>Enumeration identifying system boot modes.</summary>
  TPJBootMode = (
    bmUnknown,              // Unknown boot mode
    bmNormal,               // Normal boot
    bmSafeMode,             // Booted in safe mode
    bmSafeModeNetwork       // Booted in safe node with networking
  );

type
  ///  <summary>Class of exception raised by code in this unit.</summary>
  EPJSysInfo = class(Exception);

type
  ///  <summary>Static class that provides operating system version information.
  ///  </summary>
  TPJOSInfo = class(TObject)
  private
    ///  <summary>Gets description of OS product edition from value returned
    ///  from GetProductInfo API.</summary>
    class function EditionFromProductInfo: string;

    ///  <summary>Checks if a given suite is installed on an NT system.
    ///  </summary>
    ///  <param name="Suite">Integer [in] One of the VER_SUITE_* flags.</param>
    ///  <returns>True if suite is installed, False if not installed or not an
    ///  NT platform OS.</returns>
    class function CheckSuite(const Suite: Integer): Boolean;

    ///  <summary>Gets product edition from registry.</summary>
    ///  <remarks>Needed to get edition for NT4 pre SP6.</remarks>
    class function EditionFromReg: string;

    ///  <summary>Checks registry to see if NT4 Service Pack 6a is installed.
    ///  </summary>
    class function IsNT4SP6a: Boolean;

    ///  <summary>Gets code describing product type from registry.</summary>
    ///  <remarks>Used to get product type for NT4 SP5 and earlier.</remarks>
    class function ProductTypeFromReg: string;

  public
    ///  <summary>Checks if the OS is on the Windows 9x platform.</summary>
    class function IsWin9x: Boolean;

    ///  <summary>Checks if the OS is on the Windows NT platform.</summary>
    class function IsWinNT: Boolean;

    ///  <summary>Checks if the program is hosted on Win32s.</summary>
    ///  <remarks>This is unlikely to ever return True since Delphi does not run
    ///  on Win32s.</remarks>
    class function IsWin32s: Boolean;

    ///  <summary>Checks if a 32 bit program is running under WOW64 on a 64 bit
    ///  operating system.</summary>
    class function IsWow64: Boolean;

    ///  <summary>Checks if the program is running on a server operating system.
    ///  </summary>
    class function IsServer: Boolean;

    ///  <summary>Checks if Windows Media Center is installed.</summary>
    class function IsMediaCenter: Boolean;

    ///  <summary>Checks if the program is running on a tablet PC OS.</summary>
    class function IsTabletPC: Boolean;

    ///  <summary>Checks if the program is running under Windows Terminal Server
    ///  as a client session.</summary>
    class function IsRemoteSession: Boolean;

    ///  <summary>Checks of the host operating system has pen extensions
    ///  installed.</summary>
    class function HasPenExtensions: Boolean;

    ///  <summary>Returns the host OS platform identifier.</summary>
    class function Platform: TPJOSPlatform;

    ///  <summary>Returns the host OS product identifier.</summary>
    class function Product: TPJOSProduct;

    ///  <summary>Returns the product name of the host OS.</summary>
    class function ProductName: string;

    ///  <summary>Returns the major version number of the host OS.</summary>
    class function MajorVersion: Integer;

    ///  <summary>Returns the minor version number of the host OS.</summary>
    class function MinorVersion: Integer;

    ///  <summary>Returns the host OS's build number.</summary>
    class function BuildNumber: Integer;

    ///  <summary>Returns the name of any installed OS service pack.</summary>
    class function ServicePack: string;

    ///  <summary>Returns the major version number of any NT platform service
    ///  pack.</summary>
    ///  <remarks>0 is returned in no service pack is installed, if the host OS
    ///  is not on the NT platform.</remarks>
    class function ServicePackMajor: Integer;

    ///  <summary>Returns the minor version number of any NT platform service
    ///  pack.</summary>
    ///  <remarks>Invalid is ServicePackMinor returns 0.</remarks>
    class function ServicePackMinor: Integer;

    ///  <summary>Returns the production edition for an NT platform OS.
    ///  </summary>
    ///  <remarks>The empty string is returned if the OS is not on the NT
    ///  platform.</remarks>
    class function Edition: string;

    ///  <summary>Returns a full descriptuon of the host OS.</summary>
    class function Description: string;

    ///  <summary>Returns the Windows product ID of the host OS.</summary>
    class function ProductID: string;

    ///  <summary>Organisation to which Windows is registered, if any.</summary>
    class function RegisteredOrganisation: string;

    ///  <summary>Owner to which Windows is registered.</summary>
    class function RegisteredOwner: string;
  end;

type
  ///  <summary>Static class that provides information about the host computer.
  ///  </summary>
  TPJComputerInfo = class(TObject)
  public
    ///  <summary>Returns name of host computer.</summary>
    class function ComputerName: string;

    ///  <summary>Returns name of currently logged on user.</summary>
    class function UserName: string;

    ///  <summary>Returns MAC address of 1st ethernet adapter on host computer.
    ///  or empty string if no such adapter is found.
    ///  </summary>
    ///  <remarks>**WARNING** may be unreliable - see comments in
    ///  implementation. </remarks>
    class function MACAddress: string;

    ///  <summary>Returns processor architecture of host computer.</summary>
    class function Processor: TPJProcessorArchitecture;

    ///  <summary>Returns number of processors (or cores) in the host computer.
    ///  </summary>
    class function ProcessorCount: Cardinal;

    ///  <summary>Returns the identifier of the computer's processor.</summary>
    ///  <remarks>On multi-processor systems this is the identifier of the 1st
    ///  processor.</remarks>
    class function ProcessorIdentifier: string;

    ///  <summary>Returns the name of the computer's processor.</summary>
    ///  <remarks>On multi-processor systems this is the name of the 1st
    ///  processor.</remarks>
    class function ProcessorName: string;

    ///  <summary>Checks if the host computer has a 64 bit processor.</summary>
    class function Is64Bit: Boolean;

    ///  <summary>Checks if a network is present on host computer.</summary>
    class function IsNetworkPresent: Boolean;

    ///  <summary>Returns the OS mode used when host computer was last booted.
    ///  </summary>
    class function BootMode: TPJBootMode;

    ///  <summary>Checks if the current user has administrator privileges.
    ///  </summary>
    ///  <remarks>
    ///  <para>Always returns True on the Windows 9x platform.</para>
    ///  <para>WARNING: True is also returned when running in Windows 9x
    ///  compatibility mode on a Windows NT platform system, regardless of
    ///  whether the user has admin privileges or not.</para>
    ///  <para>Based on code at http://edn.embarcadero.com/article/26752</para>
    ///  </remarks>
    class function IsAdmin: Boolean;

    ///  <summary>Checks if UAC is active on the computer.</summary>
    ///  <remarks>
    ///  <para>UAC requires Windows Vista or later. Returns False on earlier
    ///  operating systems.</para>
    ///  <para>WARNING: False is also returned when running in Windows XP or
    ///  earlier compatibility mode on Windows Vista or later, regardless of
    ///  whether UAC is enabled or not.</para>
    ///  <para>Based on code on Stack Overflow, answer by norgepaul, at
    ///  http://tinyurl.com/avlztmg</para>
    ///  </remarks>
    class function IsUACActive: Boolean;

    ///  <summary>Returns the name of the computer's BIOS vendor.</summary>
    class function BiosVendor: string;

    ///  <summary>Returns the name of the computer's manufacturer.</summary>
    class function SystemManufacturer: string;

    ///  <summary>Returns the computer's product or model name.</summary>
    class function SystemProductName: string;

  end;

type
  ///  <summary>Static class that provides the paths of the system's standard
  ///  folders.</summary>
  TPJSystemFolders = class(TObject)
  public
    ///  <summary>Returns the fully qualified name of the Common Files folder.
    ///  </summary>
    class function CommonFiles: string;

    ///  <summary>Returns the fully qualified name of the Common Files x86
    ///  folder.</summary>
    ///  <remarks>
    ///  <para>Returns the empty string on 32 bit Windows since there is no
    ///  such folder.</para>
    ///  <para>This folder is used common files for 32 bit programs on 64 bit
    ///  Windows systems.</para>
    ///  </remarks>
    class function CommonFilesX86: string;

    ///  <summary>Returns the fully qualified name of the Common Files folder
    ///  according to whether the host program and operating system are 32 or 64
    ///  bit.</summary>
    ///  <remarks>For a 32 bit program on 32 bit Windows or 64 bit program on
    ///  64 bit windows the return value is the same as CommonFiles. For a 32
    ///  bit program running on 64 bit Windows the return value is the same as
    ///  CommonFilesX86.</remarks>
    class function CommonFilesRedirect: string;

    ///  <summary>Returns the fully qualified name of the Program Files folder.
    ///  </summary>
    class function ProgramFiles: string;

    ///  <summary>Returns the fully qualified name of the Program Files x86
    ///  folder, if present.</summary>
    ///  <remarks>
    ///  <para>Returns the empty string on 32 bit Windows since there is no
    ///  such folder.</para>
    ///  <para>This folder is used to install 32 bit programs on 64 bit
    ///  Windows systems.</para>
    ///  </remarks>
    class function ProgramFilesX86: string;

    ///  <summary>Returns the fully qualified name of the Program Files folder
    ///  according to whether the host program and operating system are 32 or 64
    ///  bit.</summary>
    ///  <remarks>For a 32 bit program on 32 bit Windows or 64 bit program on
    ///  64 bit windows the return value is the same as ProgramFiles. For a 32
    ///  bit program running on 64 bit Windows the return value is the same as
    ///  ProgramFilesX86.</remarks>
    class function ProgramFilesRedirect: string;

    ///  <summary>Returns the fully qualified name of the Windows folder.
    ///  </summary>
    class function Windows: string;

    ///  <summary>Returns the fully qualified name of the Windows System folder.
    ///  </summary>
    class function System: string;

    ///  <summary>Returns the fully qualified name of the folder used to store
    ///  shared 32 bit code on 64 bit Windows.</summary>
    class function SystemWow64: string;

    ///  <summary>Returns the fully qualified name of the system's temporary
    ///  folder.</summary>
    class function Temp: string;
  end;

{$IFNDEF HASLONGWORD}
type
  // Define LongWord for compilers that don't have it
  LongWord = Windows.DWORD;
{$ENDIF}

var
  // Global variables providing extended information about the OS version

  // Flag that indicates if extended version information is available.
  Win32HaveExInfo: Boolean = False;
  // Major version number of the latest Service Pack installed on the system. If
  // no service pack has been installed the value is 0. Invalid if
  // Win32HaveExInfo is False.
  Win32ServicePackMajor: Integer = 0;
  // Minor version number of the latest Service Pack installed on the system.
  // Invalid if Win32HaveExInfo is False.
  Win32ServicePackMinor: Integer = 0;
  // Bit flags that identify the product suites available on the system. Value
  // is a combination of the VER_SUITE_* flags defined above. Invalid if
  // Win32HaveExInfo is False.
  Win32SuiteMask: Integer = 0;
  // Additional information about the system. Value is one of the VER_NT_* flags
  // defined above. Invalid if Win32HaveExInfo is False.
  Win32ProductType: Integer = 0;

  // Flag that indicates if product information is available on the OS, i.e. if
  // the GetProductInfo API function is available.
  Win32HaveProductInfo: Boolean = False;
  // Product info for the operating system. Set to 0 if Win32HaveProductInfo
  // is False.
  Win32ProductInfo: LongWord = 0;


implementation


uses
  // Delphi
  Registry, Nb30;


resourcestring
  // Error messages
  sUnknownPlatform = 'Unrecognized operating system platform';
  sUnknownProduct = 'Unrecognised operating system product';
  sBadRegType =  'Unsupported registry type';
  sBadProcHandle = 'Bad process handle';

const
  // Map of product codes per GetProductInfo API to product names
  // ** Laurent Pierre supplied original code on which this map is based
  //    It has been modified and extended using MSDN documentation at
  //    http://msdn.microsoft.com/en-us/library/ms724358
  cProductMap: array[1..83] of record
    Id: Cardinal; // product ID
    Name: string; // product name
  end = (
    (Id: PRODUCT_BUSINESS;
      Name: 'Business Edition';),
    (Id: PRODUCT_BUSINESS_N;
      Name: 'Business N Edition';),
    (Id: PRODUCT_CLUSTER_SERVER;
      Name: 'Cluster Server Edition';),
    (Id: PRODUCT_CLUSTER_SERVER_V;
      Name: 'Server Hyper Core V';),
    (Id: PRODUCT_CORE;
      Name: 'Windows 8';),
    (Id: PRODUCT_CORE_N;
      Name: 'Windows 8N';),
    (Id: PRODUCT_CORE_COUNTRYSPECIFIC;
      Name: 'Windows 8 China';),
    (Id: PRODUCT_CORE_SINGLELANGUAGE;
      Name: 'Windows 8 Single Language';),
    (Id: PRODUCT_DATACENTER_EVALUATION_SERVER;
      Name: 'Server Datacenter (evaluation installation)';),
    (Id: PRODUCT_DATACENTER_SERVER;
      Name: 'Server Datacenter Edition (full installation)';),
    (Id: PRODUCT_DATACENTER_SERVER_CORE;
      Name: 'Server Datacenter Edition (core installation)';),
    (Id: PRODUCT_DATACENTER_SERVER_CORE_V;
      Name: 'Server Datacenter Edition without Hyper-V (core installation)';),
    (Id: PRODUCT_DATACENTER_SERVER_V;
      Name: 'Server Datacenter Edition without Hyper-V (full installation)';),
    (Id: PRODUCT_ENTERPRISE;
      Name: 'Enterprise Edition';),
    (Id: PRODUCT_ENTERPRISE_E;
      Name: 'Enterprise E Edition';),
    (Id: PRODUCT_ENTERPRISE_N_EVALUATION;
      Name: 'Enterprise N (evaluation installation)';),
    (Id: PRODUCT_ENTERPRISE_N;
      Name: 'Enterprise N Edition';),
    (Id: PRODUCT_ENTERPRISE_EVALUATION;
      Name: 'Server Enterprise (evaluation installation)';),
    (Id: PRODUCT_ENTERPRISE_SERVER;
      Name: 'Server Enterprise Edition (full installation)';),
    (Id: PRODUCT_ENTERPRISE_SERVER_CORE;
      Name: 'Server Enterprise Edition (core installation)';),
    (Id: PRODUCT_ENTERPRISE_SERVER_CORE_V;
      Name: 'Server Enterprise Edition without Hyper-V (core installation)';),
    (Id: PRODUCT_ENTERPRISE_SERVER_IA64;
      Name: 'Server Enterprise Edition for Itanium-based Systems';),
    (Id: PRODUCT_ENTERPRISE_SERVER_V;
      Name: 'Server Enterprise Edition without Hyper-V (full installation)';),
    (Id: PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT;
      Name: 'Windows Essential Server Solution Management'),
    (Id: PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL;
      Name: 'Windows Essential Server Solution Additional'),
    (Id: PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC;
      Name: 'Windows Essential Server Solution Management SVC'),
    (Id: PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC;
      Name: 'Windows Essential Server Solution Additional SVC'),
    (Id: PRODUCT_HOME_BASIC;
      Name: 'Home Basic Edition';),
    (Id: PRODUCT_HOME_BASIC_E;
      Name: 'Home Basic E Edition';),
    (Id: PRODUCT_HOME_BASIC_N;
      Name: 'Home Basic N Edition';),
    (Id: PRODUCT_HOME_PREMIUM;
      Name: 'Home Premium Edition';),
    (Id: PRODUCT_HOME_PREMIUM_E;
      Name: 'Home Premium E Edition';),
    (Id: PRODUCT_HOME_PREMIUM_N;
      Name: 'Home Premium N Edition';),
    (Id: PRODUCT_HOME_PREMIUM_SERVER;
      Name: 'Windows Home Server 2011';),
    (Id: PRODUCT_HOME_SERVER;
      Name: 'Home Server Edition';),
    (Id: PRODUCT_HYPERV;
      Name: 'Microsoft Hyper-V Server'),
    (Id: PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT;
      Name: 'Windows Essential Business Server Management Server';),
    (Id: PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING;
      Name: 'Windows Essential Business Server Messaging Server';),
    (Id: PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY;
      Name: 'Windows Essential Business Server Security Server';),
    (Id: PRODUCT_MULTIPOINT_STANDARD_SERVER;
      Name: 'Windows MultiPoint Server Standard (full installation)';),
    (Id: PRODUCT_MULTIPOINT_PREMIUM_SERVER;
      Name: 'Windows MultiPoint Server Premium (full installation)';),
    (Id: PRODUCT_PROFESSIONAL;
      Name: 'Professional Edition';),
    (Id: PRODUCT_PROFESSIONAL_E;
      Name: 'Professional E Edition';),
    (Id: PRODUCT_PROFESSIONAL_N;
      Name: 'Professional N Edition';),
    (Id: PRODUCT_PROFESSIONAL_WMC;
      Name: 'Professional with Media Center';),
    (Id: PRODUCT_SB_SOLUTION_SERVER_EM;
      Name: 'Server For SB Solutions EM';),
    (Id: PRODUCT_SERVER_FOR_SB_SOLUTIONS;
      Name: 'Server For SB Solutions';),
    (Id: PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM;
      Name: 'Server For SB Solutions EM';),
    (Id: PRODUCT_SERVER_FOR_SMALLBUSINESS;
      Name: 'Server for Small Business Edition';),
    (Id: PRODUCT_SERVER_FOR_SMALLBUSINESS_V;
      Name: 'Windows Server 2008 without Hyper-V for Windows Essential Server '
        + 'Solutions';),
    (Id: PRODUCT_SERVER_FOUNDATION;
      Name: 'Server Foundation';),
    (Id: PRODUCT_SB_SOLUTION_SERVER;
      Name: 'Windows Small Business Server 2011 Essentials';),
    (Id: PRODUCT_SMALLBUSINESS_SERVER;
      Name: 'Small Business Server';),
    (Id: PRODUCT_SMALLBUSINESS_SERVER_PREMIUM;
      Name: 'Small Business Server Premium Edition';),
    (Id: PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE;
      Name: 'Small Business Server Premium (core installation)';),
    (Id: PRODUCT_SOLUTION_EMBEDDEDSERVER;
      Name: 'Windows MultiPoint Server';),
    (Id: PRODUCT_STANDARD_EVALUATION_SERVER;
      Name: 'Server Standard (evaluation installation)';),
    (Id: PRODUCT_STANDARD_SERVER;
      Name: 'Server Standard Edition (full installation)';),
    (Id: PRODUCT_STANDARD_SERVER_CORE;
      Name: 'Server Standard Edition (core installation)';),
    (Id: PRODUCT_STANDARD_SERVER_CORE_V;
      Name: 'Server Standard Edition without Hyper-V (core installation)';),
    (Id: PRODUCT_STANDARD_SERVER_V;
      Name: 'Server Standard Edition without Hyper-V (full installation)';),
    (Id: PRODUCT_STANDARD_SERVER_SOLUTIONS;
      Name: 'Server Solutions Premium';),
    (Id: PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE;
      Name: 'Server Solutions Premium (core installation)';),
    (Id: PRODUCT_STARTER;
      Name: 'Starter Edition';),
    (Id: PRODUCT_STARTER_E;
      Name: 'Starter E Edition';),
    (Id: PRODUCT_STARTER_N;
      Name: 'Starter N Edition';),
    (Id: PRODUCT_STORAGE_ENTERPRISE_SERVER;
      Name: 'Storage Server Enterprise Edition';),
    (Id: PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE;
      Name: 'Storage Server Enterprise (core installation)';),
    (Id: PRODUCT_STORAGE_EXPRESS_SERVER;
      Name: 'Storage Server Express Edition';),
    (Id: PRODUCT_STORAGE_EXPRESS_SERVER_CORE;
      Name: 'Storage Server Express (core installation)';),
    (Id: PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER;
      Name: 'Storage Server Standard (evaluation installation)';),
    (Id: PRODUCT_STORAGE_STANDARD_SERVER;
      Name: 'Storage Server Standard Edition';),
    (Id: PRODUCT_STORAGE_STANDARD_SERVER_CORE;
      Name: 'Storage Server Standard (core installation)';),
    (Id: PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER;
      Name: 'Storage Server Workgroup (evaluation installation)';),
    (Id: PRODUCT_STORAGE_WORKGROUP_SERVER;
      Name: 'Storage Server Workgroup Edition';),
    (Id: PRODUCT_STORAGE_WORKGROUP_SERVER_CORE;
      Name: 'Storage Server Workgroup (core installation)';),
    (Id: PRODUCT_UNDEFINED;
      Name: 'An unknown product';),
    (Id: PRODUCT_ULTIMATE;
      Name: 'Ultimate Edition';),
    (Id: PRODUCT_ULTIMATE_E;
      Name: 'Ultimate E Edition';),
    (Id: PRODUCT_ULTIMATE_N;
      Name: 'Ultimate N Edition';),
    (Id: PRODUCT_WEB_SERVER;
      Name: 'Web Server Edition';),
    (Id: PRODUCT_WEB_SERVER_CORE;
      Name: 'Web Server Edition (core installation)';),
    (Id: Cardinal(PRODUCT_UNLICENSED);
      Name: 'Unlicensed product';)
  );

const
  // Array of "current version" registry sub-keys that vary with platform.
  // "False" value is for Window 9x and "True" value is for Windows NT.
  CurrentVersionRegKeys: array[Boolean] of string = (
    'Software\Microsoft\Windows\CurrentVersion',
    'Software\Microsoft\Windows NT\CurrentVersion'
  );

var
  // Records processor architecture information
  pvtProcessorArchitecture: Word = 0;

// Flag required when opening registry with specified access flags
{$IFDEF REGACCESSFLAGS}
const
  KEY_WOW64_64KEY = $0100;  // registry access flag not defined in all Delphis
{$ENDIF}

// Loads a function from the OS kernel.
function LoadKernelFunc(const FuncName: string): Pointer;
const
  cKernel = 'kernel32.dll'; // kernel DLL
begin
  Result := GetProcAddress(GetModuleHandle(cKernel), PChar(FuncName));
end;

// Initialise global variables with extended OS version information if possible.
procedure InitPlatformIdEx;
type
  // Function type of the GetProductInfo API function
  TGetProductInfo = function(OSMajor, OSMinor, SPMajor, SPMinor: DWORD;
    out ProductType: DWORD): BOOL; stdcall;
  // Function type of the GetNativeSystemInfo and GetSystemInfo functions
  TGetSystemInfo = procedure(var lpSystemInfo: TSystemInfo); stdcall;
var
  OSVI: TOSVersionInfoEx;           // extended OS version info structure
  POSVI: POSVersionInfo;            // pointer to OS version info structure
  GetProductInfo: TGetProductInfo;  // pointer to GetProductInfo API function
  GetSystemInfoFn: TGetSystemInfo;  // function used to get system info
  SI: TSystemInfo;                  // structure from GetSystemInfo API call
begin
  // Clear the structure
  FillChar(OSVI, SizeOf(OSVI), 0);
  // Get pointer to structure of non-extended type (GetVersionEx
  // requires a non-extended structure and we need this pointer to get
  // it to accept our extended structure!!)
  {$TYPEDADDRESS OFF}
  POSVI := @OSVI;
  {$TYPEDADDRESS ON}
  // Try to get exended information
  OSVI.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
  Win32HaveExInfo := GetVersionEx(POSVI^);
  if Win32HaveExInfo then
  begin
    // We have extended info: store details in global vars
    Win32ServicePackMajor := OSVI.wServicePackMajor;
    Win32ServicePackMinor := OSVI.wServicePackMinor;
    Win32SuiteMask := OSVI.wSuiteMask;
    Win32ProductType := OSVI.wProductType;
    // Try to get product info
    GetProductInfo := LoadKernelFunc('GetProductInfo');
    Win32HaveProductInfo := Assigned(GetProductInfo);
    if Win32HaveProductInfo then
    begin
      if not GetProductInfo(
        Win32MajorVersion, Win32MinorVersion,
        Win32ServicePackMajor, Win32ServicePackMinor,
        Win32ProductInfo
      ) then
        Win32ProductInfo := PRODUCT_UNDEFINED;
    end;
  end;
  // Get processor architecture. Prefer to use GetNativeSystemInfo() API call if
  // available, otherwise use GetSystemInfo()
  GetSystemInfoFn := LoadKernelFunc('GetNativeSystemInfo');
  if not Assigned(GetSystemInfoFn) then
    GetSystemInfoFn := Windows.GetSystemInfo;
  GetSystemInfoFn(SI);
  pvtProcessorArchitecture := SI.wProcessorArchitecture;
end;

{$IFNDEF EXCLUDETRAILING}
// Removes any trailing '\' from given directory or path. Used for versions of
// Delphi that don't implement this routine in SysUtils.
function ExcludeTrailingPathDelimiter(const DirOrPath: string) : string;
begin
  Result := DirOrPath;
  while (Result <> '') and (Result[Length(Result)] = '\') do
    Result := Copy(Result, 1, Length(Result) - 1);
end;
{$ENDIF}

// Returns the value of the given environment variable.
function GetEnvVar(const VarName: string): string;
var
  BufSize: Integer;  // size (in chars) of value + terminal #0
begin
  BufSize := GetEnvironmentVariable(PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariable(PChar(VarName), PChar(Result), BufSize);
  end
  else
    Result := '';
end;

// Creates a read only TRegistry instance. On versions of Delphi that don't
// support passing access flags to TRegistry constructor, registry is opened
// normally for read/write access.
function RegCreate: TRegistry;
begin
  {$IFDEF REGACCESSFLAGS}
  //! fix for issue #14 (http://bit.ly/eWkw9X) suggested by Steffen Schaff
  Result := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
  {$ELSE}
  Result := TRegistry.Create;
  {$ENDIF}
end;

// Uses registry object to open a key as read only. On versions of Delphi that
// can't open keys as read only the key is opened normally.
function RegOpenKeyReadOnly(const Reg: TRegistry; const Key: string): Boolean;
begin
  {$IFDEF REGACCESSFLAGS}
  //! Fix for problem with OpenKeyReadOnly on 64 bit Windows
  //! requires Reg has (KEY_READ or KEY_WOW64_64KEY) access flags
  Result := Reg.OpenKey(Key, False);
  {$ELSE}
  // Can't fix Win 64 problem since this version of Delphi does not support
  // customisation of registry access flags.
  {$IFDEF REGOPENREADONLY}
  Result := Reg.OpenKeyReadOnly(Key);
  {$ELSE}
  Result := Reg.OpenKey(Key, False);
  {$ENDIF}
  {$ENDIF}
end;

// Gets a string value from the given registry sub-key and value within the
// given root key (hive).
function GetRegistryString(const RootKey: HKEY;
  const SubKey, Name: string): string;
var
  Reg: TRegistry;          // registry access object
  ValueInfo: TRegDataInfo; // info about registry value
begin
  Result := '';
  // Open registry at required root key
  Reg := RegCreate;
  try
    Reg.RootKey := RootKey;
    // Open registry key and check value exists
    if RegOpenKeyReadOnly(Reg, SubKey)
      and Reg.ValueExists(Name) then
    begin
      // Check if registry value is string or integer
      Reg.GetDataInfo(Name, ValueInfo);
      case ValueInfo.RegData of
        rdString, rdExpandString:
          // string value: just return it
          Result := Reg.ReadString(Name);
        rdInteger:
          // integer value: convert to string
          Result := IntToStr(Reg.ReadInteger(Name));
        else
          // unsupported value: raise exception
          raise EPJSysInfo.Create(sBadRegType);
      end;
    end;
  finally
    // Close registry
    Reg.CloseKey;
    Reg.Free;
  end;
end;

// Gets string info for given value from Windows current version key in
// registry.
function GetCurrentVersionRegStr(ValName: string): string;
const
  // required registry string
  cWdwCurrentVer = '\Software\Microsoft\Windows\CurrentVersion';
begin
  Result := GetRegistryString(
    Windows.HKEY_LOCAL_MACHINE, cWdwCurrentVer, ValName
  );
end;

{ TPJOSInfo }

class function TPJOSInfo.BuildNumber: Integer;
begin
  Result := Win32BuildNumber;
end;

class function TPJOSInfo.CheckSuite(const Suite: Integer): Boolean;
begin
  Result := Win32SuiteMask and Suite <> 0;
end;

class function TPJOSInfo.Description: string;

  // Adds a non-empty string to end of result, preceeded by space.
  procedure AppendToResult(const Str: string);
  begin
    if Str <> '' then
      Result := Result + ' ' + Str;
  end;

begin
  // Start with product name
  Result := ProductName;
  case Platform of
    ospWinNT:
    begin
      // We have an NT OS
      // append any product type
      if Product = osWinNT then
      begin
        // For NT3/4 append version number after product
        AppendToResult(Format('%d.%d', [MajorVersion, MinorVersion]));
        AppendToResult(Edition);
        AppendToResult(ServicePack);  // does nothing if no service pack
        AppendToResult(Format('(Build %d)', [BuildNumber]));
      end
      else
      begin
        // Windows 2000 and later: don't include version number
        AppendToResult(Edition);
        AppendToResult(ServicePack);  // does nothing if no service pack
        AppendToResult(Format('(Build %d)', [BuildNumber]));
      end;
    end;
    ospWin9x:
      // We have a Win 95 line OS: append service pack
      AppendToResult(ServicePack);
  end;
end;

class function TPJOSInfo.Edition: string;
begin
  // This method is based on sample C++ code from MSDN
  Result := '';
  case Product of
    osWinVista, osWinSvr2008,
    osWin7, osWinSvr2008R2,
    osWin8, osWinSvr2012:
    begin
      // For v6.0 and later we ignore the suite mask and use the new
      // PRODUCT_ flags from the GetProductInfo() function to determine the
      // edition
      Result := EditionFromProductInfo;
      // append 64-bit if 64 bit system
      if pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64 then
        Result := Result + ' (64-bit)';
      // can detect 32-bit if required by checking if
      // pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_INTEL then
    end;
    osWinSvr2003, osWinSvr2003R2:
    begin
      // We check different processor architectures and act accordingly
      // This code closely based on MS's sample code found at
      // http://msdn2.microsoft.com/en-us/library/ms724429
      if pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA64 then
      begin
        if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter Edition for Itanium-based Systems'
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Enterprise Edition for Itanium-based Systems';
      end
      else if (pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
      begin
        if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter x64 Edition'
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Enterprise x64 Edition'
        else
          Result := 'Standard x64 Edition';
      end
      else
      begin
        if CheckSuite(VER_SUITE_COMPUTE_SERVER) then
          Result := 'Compute Cluster Edition'
        else if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter Edition'
        else if CheckSuite(VER_SUITE_BLADE) then
          Result := 'Web Edition'
        else if CheckSuite(VER_SUITE_STORAGE_SERVER) then
          Result := 'Storage Server'
        // According to MSDN we can't rely on VER_SUITE_SMALLBUSINESS since it
        // is not removed when upgrading to standard or enterprises editions.
        // When installing Small Business edition both VER_SUITE_SMALLBUSINESS
        // and VER_SUITE_SMALLBUSINESS_RESTRICTED are set. When installed
        // standard edition VER_SUITE_SMALLBUSINESS_RESTRICTED gets unset while
        // VER_SUITE_SMALLBUSINESS remains. So, we first check for the
        // Enterprise edition and exclude Small Business if we find that.
        // Since there is no flag for Standard Edition we check for both
        // VER_SUITE_SMALLBUSINESS and VER_SUITE_SMALLBUSINESS_RESTRICTED and
        // assume Small Business if we find both, otherwise we assume Standard
        // edition.
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Enterprise Edition'
        else if CheckSuite(VER_SUITE_SMALLBUSINESS) and
          CheckSuite(VER_SUITE_SMALLBUSINESS_RESTRICTED) then
          Result := 'Small Business Edition'
        else
          Result := 'Standard Edition';
      end;
    end;
    osWinXP:
    begin
      if GetSystemMetrics(SM_STARTER) <> 0 then
        Result := 'Starter Edition'
      else if (Win32MajorVersion = 5) and (Win32MinorVersion = 2) and
        not IsServer and  // XP Pro 64 has version 5.2 not 5.1!
        (pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
        Result := 'Professional x64 Edition'
      else if CheckSuite(VER_SUITE_PERSONAL) then
        Result := 'Home Edition'
      else
        Result := 'Professional';
    end;
    osWin2K:
    begin
      if IsServer then
      begin
        if CheckSuite(VER_SUITE_DATACENTER) then
          Result := 'Datacenter Server'
        else if CheckSuite(VER_SUITE_ENTERPRISE) then
          Result := 'Advanced Server'
        else
          Result := 'Server';
      end
      else
        Result := 'Professional';
    end;
    osWinNT:
    begin
      if Win32HaveExInfo then
      begin
        // This is NT SP6 or later: got info from OS
        if IsServer then
        begin
          if CheckSuite(VER_SUITE_ENTERPRISE) then
            Result := 'Enterprise Edition'
          else
            Result := 'Server';
        end
        else
          Result := 'Workstation'
      end
      else
        // NT before SP6: we read required info from registry
        Result := EditionFromReg;
    end;
  end;
end;

class function TPJOSInfo.EditionFromProductInfo: string;
var
  Idx: Integer; // loops through entries in cProductMap[]
begin
  Result := '';
  for Idx := Low(cProductMap) to High(cProductMap) do
  begin
    if cProductMap[Idx].Id = Win32ProductInfo then
    begin
      Result := cProductMap[Idx].Name;
      Exit;
    end;
  end;
end;

class function TPJOSInfo.EditionFromReg: string;
var
  EditionCode: string;  // OS product edition code stored in registry
begin
  EditionCode := ProductTypeFromReg;
  if CompareText(EditionCode, 'WINNT') = 0 then
    Result := 'WorkStation'
  else if CompareText(EditionCode, 'LANMANNT') = 0 then
    Result := 'Server'
  else if CompareText(EditionCode, 'SERVERNT') = 0 then
    Result := 'Advanced Server';
  Result := Result + Format(
    ' %d.%d', [Win32MajorVersion, Win32MinorVersion]
  );
end;

class function TPJOSInfo.HasPenExtensions: Boolean;
begin
  Result := GetSystemMetrics(SM_PENWINDOWS) <> 0;
end;

class function TPJOSInfo.IsMediaCenter: Boolean;
begin
  Result := GetSystemMetrics(SM_MEDIACENTER) <> 0;
end;

class function TPJOSInfo.IsNT4SP6a: Boolean;
var
  Reg: TRegistry; // registry access object
begin
  if (Product = osWinNT)
    and (Win32MajorVersion = 4)
    and (CompareText(Win32CSDVersion, 'Service Pack 6') = 0) then
  begin
    // System is reporting NT4 SP6
    // we have SP 6a if particular registry key exists
    Reg := RegCreate;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      Result := Reg.KeyExists(
        'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009'
      );
    finally
      Reg.Free;
    end;
  end
  else
    // System not reporting NT4 SP6, so not SP6a!
    Result := False;
end;

class function TPJOSInfo.IsRemoteSession: Boolean;
begin
  Result := GetSystemMetrics(SM_REMOTESESSION) <> 0;
end;

class function TPJOSInfo.IsServer: Boolean;
begin
  if Win32HaveExInfo then
    // Check product type from extended OS info
    Result := Win32ProductType <> VER_NT_WORKSTATION
  else
    // Check product type stored in registry
    Result := CompareText(ProductTypeFromReg, 'WINNT') <> 0;;
end;

class function TPJOSInfo.IsTabletPC: Boolean;
begin
  Result := GetSystemMetrics(SM_TABLETPC) <> 0;
end;

class function TPJOSInfo.IsWin32s: Boolean;
begin
  Result := Platform = ospWin32s;
end;

class function TPJOSInfo.IsWin9x: Boolean;
begin
  Result := Platform = ospWin9x;
end;

class function TPJOSInfo.IsWinNT: Boolean;
begin
  Result := Platform = ospWinNT;
end;

class function TPJOSInfo.IsWow64: Boolean;
type
  TIsWow64Process = function( // Type of IsWow64Process API fn
    Handle: THandle;
    var Res: BOOL
  ): BOOL; stdcall;
var
  IsWow64Result: BOOL;              // result from IsWow64Process
  IsWow64Process: TIsWow64Process;  // IsWow64Process fn reference
begin
  // Try to load required function from kernel32
  IsWow64Process := LoadKernelFunc('IsWow64Process');
  if Assigned(IsWow64Process) then
  begin
    // Function is implemented: call it
    if not IsWow64Process(GetCurrentProcess, IsWow64Result) then
      raise Exception.Create(sBadProcHandle);
    // Return result of function
    Result := IsWow64Result;
  end
  else
    // Function not implemented: can't be running on Wow64
    Result := False;
end;

class function TPJOSInfo.MajorVersion: Integer;
begin
  Result := Win32MajorVersion;
end;

class function TPJOSInfo.MinorVersion: Integer;
begin
  Result := Win32MinorVersion;
end;

class function TPJOSInfo.Platform: TPJOSPlatform;
begin
  case Win32Platform of
    VER_PLATFORM_WIN32_NT: Result := ospWinNT;
    VER_PLATFORM_WIN32_WINDOWS: Result := ospWin9x;
    VER_PLATFORM_WIN32s: Result := ospWin32s;
    else raise EPJSysInfo.Create(sUnknownPlatform);
  end;
end;

class function TPJOSInfo.Product: TPJOSProduct;
begin
  Result := osUnknown;
  case Platform of
    ospWin9x:
    begin
      // Win 9x platform: only major version is 4
      Result := osUnknownWin9x;
      case Win32MajorVersion of
        4:
        begin
          case Win32MinorVersion of
            0: Result := osWin95;
            10: Result := osWin98;
            90: Result := osWinMe;
          end;
        end;
      end;
    end;
    ospWinNT:
    begin
      // NT platform OS
      Result := osUnknownWinNT;
      case Win32MajorVersion of
        3, 4:
        begin
          // NT 3 or 4
          case Win32MinorVersion of
            0: Result := osWinNT;
          end;
        end;
        5:
        begin
          // Windows 2000 or XP
          case Win32MinorVersion of
            0:
              Result := osWin2K;
            1:
              Result := osWinXP;
            2:
            begin
              if GetSystemMetrics(SM_SERVERR2) <> 0 then
                Result := osWinSvr2003R2
              else
              begin
                if not IsServer and
                  (pvtProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
                  Result := osWinXP // XP Pro X64
                else
                  Result := osWinSvr2003
              end
            end;
          end;
        end;
        6:
        begin
          case Win32MinorVersion of
            0:
              if not IsServer then
                Result := osWinVista
              else
                Result := osWinSvr2008;
            1:
              if not IsServer then
                Result := osWin7
              else
                Result := osWinSvr2008R2;
            2:
              if not IsServer then
                Result := osWin8
              else
                Result := osWinSvr2012;
            else
              // Higher minor version: must be an unknown later OS
              Result := osWinLater
          end;
        end;
        else
          // Higher major version: must be an unknown later OS
          Result := osWinLater;
      end;
    end;
    ospWin32s:
      // Windows 32s: probably won't ever get this
      Result := osUnknownWin32s;
  end;
end;

class function TPJOSInfo.ProductID: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE, CurrentVersionRegKeys[IsWinNT], 'ProductID'
  );
end;

class function TPJOSInfo.ProductName: string;
begin
  case Product of
    osUnknownWinNT, osUnknownWin9x, osUnknownWin32s: Result := '';
    osWinNT: Result := 'Windows NT';
    osWin2K: Result := 'Windows 2000';
    osWinXP: Result := 'Windows XP';
    osWinVista: Result := 'Windows Vista';
    osWinSvr2008: Result := 'Windows Server 2008';
    osWin95: Result := 'Windows 95';
    osWin98: Result := 'Windows 98';
    osWinMe: Result := 'Windows Me';
    osWinSvr2003: Result := 'Windows Server 2003';
    osWinSvr2003R2: Result := 'Windows Server 2003 R2';
    osWinLater: Result := Format(
      'Windows Version %d.%d', [Win32MajorVersion, Win32MinorVersion]
    );
    osWin7: Result := 'Windows 7';
    osWinSvr2008R2: Result := 'Windows Server 2008 R2';
    osWin8: Result := 'Windows 8';
    osWinSvr2012: Result := 'Windows Server 2012';
    else
      raise EPJSysInfo.Create(sUnknownProduct);
  end;
end;

class function TPJOSInfo.ProductTypeFromReg: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'SYSTEM\CurrentControlSet\Control\ProductOptions',
    'ProductType'
  );
end;

class function TPJOSInfo.RegisteredOrganisation: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE, CurrentVersionRegKeys[IsWinNT], 'RegisteredOrganisation'
  );
end;

class function TPJOSInfo.RegisteredOwner: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE, CurrentVersionRegKeys[IsWinNT], 'RegisteredOwner'
  );
end;

class function TPJOSInfo.ServicePack: string;
begin
  // Assume to service pack
  Result := '';
  case Platform of
    ospWin9x:
      // On the Windows 9x platform we decode the service pack info
      if Win32CSDVersion <> '' then
      begin
        case Product of
          osWin95:
            {$IFDEF UNICODE}
            if CharInSet(Win32CSDVersion[1], ['B', 'b', 'C', 'c']) then
            {$ELSE}
            if Win32CSDVersion[1] in ['B', 'b', 'C', 'c'] then
            {$ENDIF}
              Result := 'OSR2';
          osWin98:
            {$IFDEF UNICODE}
            if CharInSet(Win32CSDVersion[1], ['A', 'a']) then
            {$ELSE}
            if Win32CSDVersion[1] in ['A', 'a'] then
            {$ENDIF}
              Result := 'SE';
        end;
      end;
    ospWinNT:
      // On Windows NT we return service pack string, unless NT4 SP6 when we
      // need to check whether actually SP6 or SP6a
      if IsNT4SP6a then
        Result := 'Service Pack 6a' // do not localize
      else
        Result := Win32CSDVersion;
  end;
end;

class function TPJOSInfo.ServicePackMajor: Integer;
begin
  Result := Win32ServicePackMajor;
end;

class function TPJOSInfo.ServicePackMinor: Integer;
begin
  Result := Win32ServicePackMinor;
end;

{ TPJComputerInfo }

class function TPJComputerInfo.BiosVendor: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\Bios\',
    'BIOSVendor'
  );
end;

class function TPJComputerInfo.BootMode: TPJBootMode;
begin
  case GetSystemMetrics(SM_CLEANBOOT) of
    0: Result := bmNormal;
    1: Result := bmSafeMode;
    2: Result := bmSafeModeNetwork;
    else Result := bmUnknown;
  end;
end;

class function TPJComputerInfo.ComputerName: string;
var
  PComputerName:  // buffer for name returned from API
    array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;    // size of name buffer
begin
  Size := MAX_COMPUTERNAME_LENGTH;
  if Windows.GetComputerName(PComputerName, Size) then
    Result := PComputerName
  else
    Result := '';
end;

class function TPJComputerInfo.Is64Bit: Boolean;
begin
  Result := Processor in [paX64, paIA64];
end;

class function TPJComputerInfo.IsAdmin: Boolean;
const
  // SID related constants
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
var
  AccessToken: THandle;           // process access token
  TokenGroupsInfo: PTokenGroups;  // token groups
  InfoBufferSize: DWORD;          // token info buffer size
  AdmininstratorsSID: PSID;       // administrators SID
  I: Integer;                     // loops thru token groups
  Success: BOOL;                  // API function success results
begin
  if not TPJOSInfo.IsWinNT then
  begin
    // Admin mode is a foreign concept to Windows 9x - everyone is an admin
    Result := True;
    Exit;
  end;
  Result := False;
  Success := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, AccessToken);
  if not Success then
  begin
    if GetLastError = ERROR_NO_TOKEN then
      Success := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, AccessToken);
  end;
  if Success then
  begin
    GetMem(TokenGroupsInfo, 1024);
    Success := GetTokenInformation(
      AccessToken, TokenGroups, TokenGroupsInfo, 1024, InfoBufferSize
    );
    CloseHandle(AccessToken);
    if Success then
    begin
      AllocateAndInitializeSid(
        SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID,
        DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, AdmininstratorsSID
      );
      {$IFOPT R+}
        {$DEFINE RANGECHECKSWEREON}
        {$R-}
      {$ELSE}
        {$UNDEF RANGECHECKSWEREON}
      {$ENDIF}
      for I := 0 to TokenGroupsInfo.GroupCount - 1 do
        if EqualSid(AdmininstratorsSID, TokenGroupsInfo.Groups[I].Sid) then
        begin
          Result := True;
          Break;
        end;
      {$IFDEF RANGECHECKSWEREON}
        {$R+}
      {$ENDIF}
      FreeSid(AdmininstratorsSID);
    end;
    if Assigned(TokenGroupsInfo) then
      FreeMem(TokenGroupsInfo);
  end;
end;

class function TPJComputerInfo.IsNetworkPresent: Boolean;
begin
  Result := GetSystemMetrics(SM_NETWORK) and 1 = 1;
end;

class function TPJComputerInfo.IsUACActive: Boolean;
var
  Reg: TRegistry;   // registry access object
begin
  Result := False;
  if not TPJOSInfo.IsWinNT or (TPJOSInfo.MajorVersion < 6) then
    // UAC only available on Vista or later
    Exit;
  Reg := RegCreate;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if RegOpenKeyReadOnly(
      Reg, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System'
    ) then
      Result := Reg.ValueExists('EnableLUA') and Reg.ReadBool('EnableLUA');
  finally
    Reg.Free;
  end;
end;

class function TPJComputerInfo.MACAddress: string;
type
  // Based on code at MSDN knowledge base Q118623 article at
  // http://support.microsoft.com/kb/q118623/}
  // According to MSDN this method should fail on Windows 6.0 (Vista) and later.
  // It has been known to fail on Vista, but works on Vista Home Premium SP1!
  // It would seem that the call will succeed if there's an active network with
  // Netbios over TCP enabled.
  // Again according to Microsoft, the function is unreliable on Windows 95, 98
  // and Me.

  // This type is defined in MSDN sample code, but tests have found this is
  // not needed (on XP Pro) and Adapter can be of type TAdapterStatus. This
  // method uses the type in case other OSs require it
  TAStat = packed record
    Adapt: TAdapterStatus;
    NameBuff: array [0..29] of TNameBuffer;
  end;
var
  Adapter: TAStat;          // info about a network adapter
  AdapterList: TLanaEnum;   // numbers for current LAN adapters
  Ncb: TNCB;                // network control block descriptor
  I: Integer;               // loops thru all adapters in list

  // Examines given NetBios API call return value to check if call succeeded.
  function NetBiosSucceeded(const RetCode: AnsiChar): Boolean;
  begin
    Result := UCHAR(RetCode) = NRC_GOODRET;
  end;

begin
  // Assume not adapter
  Result := '';
  // Get list of adapters
  FillChar(Ncb, SizeOf(Ncb), 0);
  Ncb.ncb_command := AnsiChar(NCBENUM);
  Ncb.ncb_buffer := PAnsiChar(@AdapterList);
  Ncb.ncb_length := SizeOf(AdapterList);
  if not NetBiosSucceeded(Netbios(@Ncb)) then
    Exit;
  // Get status of each adapter, exiting when first valid one reached
  // MSDN cautions us not to assume lana[0] is valid
  for I := 0 to Pred(Integer(AdapterList.length)) do
  begin
    // reset the adapter
    FillChar(Ncb, SizeOf(Ncb), 0);
    Ncb.ncb_command := AnsiChar(NCBRESET);
    Ncb.ncb_lana_num := AdapterList.lana[I];
    if not NetBiosSucceeded(Netbios(@Ncb)) then
      Exit;
    // get status of adapter
    FillChar(Ncb, SizeOf(Ncb), 0);
    Ncb.ncb_command := AnsiChar(NCBASTAT);
    Ncb.ncb_lana_num := AdapterList.lana[i];
    Ncb.ncb_callname := '*               ';
    Ncb.ncb_buffer := PAnsiChar(@Adapter);
    Ncb.ncb_length := SizeOf(Adapter);
    if NetBiosSucceeded(Netbios(@Ncb)) then
    begin
      // we have a MAC address: return it
      with Adapter.Adapt do
        Result := Format(
          '%.2x-%.2x-%.2x-%.2x-%.2x-%.2x',
          [
            Ord(adapter_address[0]),
            Ord(adapter_address[1]),
            Ord(adapter_address[2]),
            Ord(adapter_address[3]),
            Ord(adapter_address[4]),
            Ord(adapter_address[5])
          ]
        );
      Exit;
    end;
  end;
end;

class function TPJComputerInfo.Processor: TPJProcessorArchitecture;
begin
  case pvtProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL: Result := paX86;
    PROCESSOR_ARCHITECTURE_AMD64: Result := paX64;
    PROCESSOR_ARCHITECTURE_IA64:  Result := paIA64;
    else Result := paUnknown;
  end;
end;

class function TPJComputerInfo.ProcessorCount: Cardinal;
var
  SI: TSystemInfo;  // contains system information
begin
  GetSystemInfo(SI);
  Result := SI.dwNumberOfProcessors;
end;

class function TPJComputerInfo.ProcessorIdentifier: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\CentralProcessor\0\',
    'Identifier'
  );
end;

class function TPJComputerInfo.ProcessorName: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\CentralProcessor\0\',
    'ProcessorNameString'
  );
end;

class function TPJComputerInfo.SystemManufacturer: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\Bios\',
    'SystemManufacturer'
  );
end;

class function TPJComputerInfo.SystemProductName: string;
begin
  Result := GetRegistryString(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\Bios\',
    'SystemProductName'
  );
end;

class function TPJComputerInfo.UserName: string;
const
  UNLEN = 256;  // max size of user name buffer (per MS SDK docs)
var
  PUserName: array[0..UNLEN] of Char; // buffer for name returned from API
  Size: DWORD;                        // size of name buffer
begin
  Size := UNLEN;
  if Windows.GetUserName(PUserName, Size) then
    Result := PUserName
  else
    Result := '';
end;

{ TPJSystemFolders }

class function TPJSystemFolders.CommonFiles: string;
begin
  Result :=  ExcludeTrailingPathDelimiter(
    GetCurrentVersionRegStr('CommonFilesDir')
  );
end;

class function TPJSystemFolders.CommonFilesRedirect: string;
begin
  Result := GetEnvVar('COMMONPROGRAMFILES');
end;

class function TPJSystemFolders.CommonFilesX86: string;
begin
  Result :=  ExcludeTrailingPathDelimiter(
    GetCurrentVersionRegStr('CommonFilesDir (x86)')
  );
end;

class function TPJSystemFolders.ProgramFiles: string;
begin
  Result :=  ExcludeTrailingPathDelimiter(
    GetCurrentVersionRegStr('ProgramFilesDir')
  );
end;

class function TPJSystemFolders.ProgramFilesRedirect: string;
begin
  Result := GetEnvVar('PROGRAMFILES');
end;

class function TPJSystemFolders.ProgramFilesX86: string;
begin
  Result :=  ExcludeTrailingPathDelimiter(
    GetCurrentVersionRegStr('ProgramFilesDir (x86)')
  );
end;

class function TPJSystemFolders.System: string;
var
  PFolder: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
begin
  if GetSystemDirectory(PFolder, MAX_PATH) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PFolder)
  else
    Result := '';
end;

class function TPJSystemFolders.SystemWow64: string;
type
  // type of GetSystemWow64DirectoryFn API function
  TGetSystemWow64Directory = function(lpBuffer: PChar; uSize: UINT): UINT;
    stdcall;
var
  PFolder: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
  GetSystemWow64Directory: TGetSystemWow64Directory;  // API function
begin
  Result := '';
  {$IFDEF UNICODE}
  GetSystemWow64Directory := LoadKernelFunc('GetSystemWow64DirectoryW');
  {$ELSE}
  GetSystemWow64Directory := LoadKernelFunc('GetSystemWow64DirectoryA');
  {$ENDIF}
  if not Assigned(GetSystemWow64Directory) then
    Exit;
  if GetSystemWow64Directory(PFolder, MAX_PATH) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PFolder);
end;

class function TPJSystemFolders.Temp: string;
var
  PathBuf: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
begin
  if GetTempPath(MAX_PATH, PathBuf) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PathBuf)
  else
    Result := '';
end;

class function TPJSystemFolders.Windows: string;
var
  PFolder: array[0..MAX_PATH] of Char;  // buffer to hold name returned from API
begin
  if GetWindowsDirectory(PFolder, MAX_PATH) <> 0 then
    Result := ExcludeTrailingPathDelimiter(PFolder)
  else
    Result := '';
end;

initialization

// Initialize global variables from extended OS and product info
InitPlatformIdEx;

end.

