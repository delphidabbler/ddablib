================================================================================

Console Application Runner Demo Programs Read Me

================================================================================

** IMPORTANT: Please read this file before attempting to compile any of the demo
projects.

--------------------------------------------------------------------------------
Contents of Demos directory
--------------------------------------------------------------------------------

The Demos directory and its sub-directories contain twelve small programs that
demonstrate how to use the PJConsoleApp unit.

A VCL based version of each demo program is in its own sub-directory -
Demos\Demo1 through to Demos\Demo12.

Demos 4, 8 and 11 are also available in versions that uses the FireMonkey 2
framework instead of the VCL. These projects are in the Demos\Demo4\FMX2,
Demos\Demo8\FMX2 and Demos\Demo11\FMX2 directories respectively.

In addition there are two example console applications that the demos use. They
are supplied in source form and need to be built. The project files are in
Demos\TestApps\Echoer and Demos\TestApps\Timed.

The Demos directory itself contains .bpg format project group file that includes
all the VCL demo projects and the example console applications in a single
project group. FireMonkey versions of any demo project are NOT included in the
project group and must be opened separately.

Finally there's an Demos\IOUtils sub-directory (explained in Dependencies below)
and a Demos\TestData directory which stores any data files required by the demo
programs.

--------------------------------------------------------------------------------
Information about the Demos
--------------------------------------------------------------------------------

The 12 demos are:

1: ExecAndWait.
    Implements the traditional "ExecAndWait" routine to run a child console
    application using TPJConsoleApp.

2: A better ExecAndWait.
    Implements an improved "ExecAndWait" routine that lets the GUI program
    remain interactive while the child console application is running.

3: Indicating progress.
    Shows how to display a progress meter in the GUI while a child console
    application is running.

4: Timing out.
    How to set a maximum time for a child console application to run and how to
    handle time-outs.

5: Terminating an application.
    Shows how to forcible terminate a child console application.

6: Redirecting standard i/o using files.
    How to redirect a child console application's standard input and output from
    and to files.

7: Redirecting standard i/o using pipes.
    How to redirect a child console application's standard input and output from
    and to pipes.

8: Capturing console output in a GUI.
    Demonstrates how to capture a child console application's output and display
    it in a GUI in "real time".

9: Sub-classing TPJConsoleApp.
    Show how to write your own sub class of TPJCustomConsoleApp to specialise
    its behaviour.

10: Using TPJConsoleApp from console applications.
    Demonstrates how to use TPJConsoleApp to enable a console application to
    spawn child processes.

11: Customising the appearance of the console.
    Shows how to customise the appearance of any console that is used to display
    the output of a child console application.

12: Handling Unicode output from console applications.
    How to handle output from child console applications that output Unicode
    text instead of ANSI or ASCII text.

For further information go to
http://wiki.delphidabbler.com/index.php/Docs/ConsoleAppExamples where you will
find a set of examples based on the demo programs. The number of each example
relates to the number of the associated demo project.

--------------------------------------------------------------------------------
Compatibility
--------------------------------------------------------------------------------

Delphi 7 or later are required to build the VCL based demo projects as 32 bit
Windows applications. To build 64 bit versions of the demos, Delphi XE2 or
later is required.

The FireMonkey 2 based demos require Delphi XE3 or later.

--------------------------------------------------------------------------------
Dependencies
--------------------------------------------------------------------------------

All the demo programs require that PJConsoleApp.pas is located in the parent
directory of the Demos folder, i.e. Demos\..

Demos 6, 7, 8, 9 and 12 also require one or more units from the I/O Utility
Classes project. You will need to download this project and place the
PJPipe.pas, PJFileHandle.pas and PJPipeFilters.pas files in the Demos\IOUtils
directory. A zip file containing the I/O Utility Classes project can be
downloaded from http://delphidabbler.com/software/ioutils/download.

The example console applications have no external dependencies.

--------------------------------------------------------------------------------
How to Compile
--------------------------------------------------------------------------------

First ensure all the required files listed in "Dependencies" above are in place
for the demos you want to build.

** Note for Delphi 2007 users.
   The supplied .dproj project files are not compatible with Delphi 2007.
   Alternative files, with the .dproj.2007 extension have been provided for use
   with Delphi 2007. A batch file, Prepare2007.bat, is provided in the Demos
   directory that will replace existing .dproj files with the content of the
   .proj.2007 files. To run this batch file, open a command window and make the
   Demos directory current. Now simply run the batch file by typing its name.

From your Delphi IDE open Demos\PJConsoleAppDemos.bpg. The VCL demo projects and
the example console application projects should all be listed in the IDE's
Project Manager window.

You can now build selected demos or compile them all in one go using the
Projects | Build All Projects menu option.

To build the FireMonkey 2 demos you must open their .dproj files one at a time
and build them.

Delphi may update the project or project group files. If this happens you may
need to restore the original files from before re-compiling with a different
version of Delphi.

Each demo project is configured to compile as a 32 bit Windows application. They
will all compile successfully as 64 bit Windows applications with Delphi XE2 and
later, but you will need to manually add 64 bit targets to each project
individually.

Note: some versions of Delphi may generate hints, or possibly warning, depending
on your set-up. These should not cause a problem.

--------------------------------------------------------------------------------
Disclaimer
--------------------------------------------------------------------------------

All the demo project files are for illustration purposes only. Although you can
use them as you wish, you should not use any part of them in production code
without thorough testing. The code distributed on an "AS IS" basis, WITHOUT
WARRANTY OF ANY KIND, either express or implied.

--------------------------------------------------------------------------------
$Rev$
$Date$
--------------------------------------------------------------------------------
