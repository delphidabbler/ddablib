{
 * PJConsoleApp.pas
 *
 * Class class that encapsulates and executes a command line application and
 * optionally redirects the application's standard input, output and error.
 *
 * v1.0 of 04 Oct 2007  - Original version.
 * v1.1 of 30 Mar 2008  - Made application error constants public.
 *                      - Modified code that sets application errors.
 *
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
 * The Original Code is PJConsoleApp.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit PJConsoleApp;


interface


uses
  // Delphi
  Classes, Windows;


const
  // Constants for working in milliseconds
  cOneSecInMS = 1000;               // one second in milliseconds
  cOneMinInMS = 60 * cOneSecInMS;   // one minute in milliseconds

  // Default values for some TPJConsoleApp properties
  cDefTimeSlice = 50;               // default time slice allocated to app
  cDefMaxExecTime = cOneMinInMS;    // maximum execution time of app

  // Mask that is ORd with application error codes: according to Windows API
  // docs, error codes with bit 29 set are reserved for application use.
  // Test for an app error code by and-ing the error with this mask, e.g.
  // IsAppError = (ErrorCode and cAppErrorMask) <> 0
  cAppErrorMask = 1 shl 29;

  // Application errors
  cAppErrorTimeOut = 1 or cAppErrorMask;      // application timed out
  cAppErrorTerminated = 2 or cAppErrorMask;   // application was terminated


type

  {
  TPJConsoleAppPriority:
    Enumeration of possible priorties for console application.
  }
  TPJConsoleAppPriority = (
    cpDefault,    // use default priority (see Win API docs for details)
    cpHigh,       // use for time-critical tasks: processor intensive
    cpNormal,     // normal process with no specific scheduling needs
    cpIdle,       // run only when system idle
    cpRealTime    // highest possible priority: preempts all threads inc OS
  );

  {
  TPJCustomConsoleApp:
    Base class that encapsulates and executes a command line application and
    optionally redirects the application's standard input, output and error. The
    application is excuted in time slices and the class triggers an event
    between time slices. All properties declared protected. Descendant classes
    can make required properties public.
  }
  TPJCustomConsoleApp = class(TObject)
  private
    fOnWork: TNotifyEvent;
      {References OnWork event handler}
    fOnComplete: TNotifyEvent;
      {Reference to OnComplete event handler}
    fStdIn: THandle;
      {Handle of Console app's redirected standard input or 0 if not redirected}
    fStdOut: THandle;
      {Handle of Console app's redirected standard output or 0 if not
      redirected}
    fStdErr: THandle;
      {Handle of Console app's redirected standard error or 0 if not redirected}
    fExitCode: LongWord;
      {Exit code returned from console app}
    fMaxExecTime: LongWord;
      {Maximum execution time of console app}
    fErrorMessage: string;
      {Description of any error that occured while trying to execute
      application}
    fErrorCode: LongWord;
      {Code of any error that occured while trying to execute application}
    fVisible: Boolean;
      {Whether application is to be visible or hidden}
    fTimeSlice: LongWord;
      {Time to let application run before generating each OnWork event}
    fKillTimedOutProcess: Boolean;
      {Whether to terminate a timed out or force-terminated process}
    fTimeToLive: LongWord;
      {Time application has to execute in milliseconds}
    fElapsedTime: LongWord;
      {Time elapased in milliseconds since program started executing}
    fRequestTerminate: Boolean;
      {Flag set true by Terminate to request that application is terminated}
    fProcessAttrs: PSecurityAttributes;
      {Pointer to process security and inheritance attributes}
    fThreadAttrs: PSecurityAttributes;
      {Pointer to thread security and inheritance attributes}
    fUseNewConsole: Boolean;
      {Whether application should be started in a new console window}
    fConsoleTitle: string;
      {Title to be displayed in a new console. '' => default title used}
    fEnvironment: Pointer;
      {Pointer to environment block to be passed to console application}
    fProcessInfo: TProcessInformation;
      {Stores information about running process. Zeroed when no process running}
    fPriority: TPJConsoleAppPriority;
      {Priority with which console application is started}
    function MonitorProcess: Boolean;
      {Monitors a running process, triggering event at end of each timeslice and
      when completed.
        @return True on successful completion or false if application times out
          or is forced to terminate.
      }
    function SetExitCode: Boolean;
      {Sets ExitCode property to value returned from application. Sets error
      code if we fail to retrieve exit code.
        @return True if exit code retrieved OK and False if we fail to retrieve
          it.
      }
    procedure SetMaxExecTime(const Value: LongWord);
      {Sets MaxExecTime property.
        @param Value [in] Required time in milliseconds. If 0 then property's
          default value is used.
      }
    procedure SetTimeSlice(const Value: LongWord);
      {Sets TimeSlice property.
        @param Value [in] Required time in milliseconds. If 0 then property's
          default value is used.
      }
    procedure ZeroProcessInfo;
      {Zeros the process information structure.
      }
    function GetProcessHandle: THandle;
      {Gets process handle from process info structure.
        @return Required process handle.
      }
    procedure UpdateSecurityAttrs(var OldValue: PSecurityAttributes;
      const NewValue: PSecurityAttributes);
      {Updates security attributes new new value.
        @param OldValue [in/out] Pointer to value to be updated. On return set
          to nil if NewValue is nil, otherwise set to point at copy of NewValue.
        @param NewValue [in] Pointer to new security attributes. May be nil.
      }
    procedure FreeSecurityAttrs(var Attrs: PSecurityAttributes);
      {Frees memory for and nils a security attributes structure.
        @param Attrs [in/out] Pointer to the structure to be freed (may be nil).
          Set to nil on return.
      }
    procedure SetProcessAttrs(const Value: PSecurityAttributes);
      {Write access method for ProcessAttrs property. Makes a copy of new value
      if non nil.
        @param Values [in] New value. If nil ProcessAttrs is set nil otherwise
          ProcessAttrs points to a copy of structure pointed to by Value.
      }
    procedure SetThreadAttrs(const Value: PSecurityAttributes);
      {Write access method for ThreadAttrs property. Makes a copy of new value
      if non nil.
        @param Values [in] New value. If nil ThreadAttrs is set nil otherwise
          ThreadAttrs points to a copy of structure pointed to by Value.
      }
  protected
    function StartProcess(const CmdLine, CurrentDir: string;
      out ProcessInfo: TProcessInformation): Boolean;
      {Starts a process and gets information about it from OS.
        @param CmdLine [in] Command line to be executed.
        @param CurrentDir [in] Application's current directory. Pass '' to use
          same current directory as parent application.
        @param ProcessInfo [out] Passes OS's process info back to caller.
        @return True if process created OK and false if process couldn't be
          started.
      }
    procedure DoWork; virtual;
      {Method called between program timeslices and after completion but never
      called if TimeSlice=INFINITE. Triggers the OnWork event.
      }
    procedure DoComplete; virtual;
      {Method called after completion. Triggers the OnComplete event.
      }
    procedure RecordAppError(const Code: LongWord; const Msg: string);
      {Set error code and message to class-defined error.
        @param Code [in] Required error code. Must have bit 29 set.
        @param Msg [in] Required error message.
      }
    procedure RecordWin32Error;
      {Set error code message to the last-reported Windows error.
      }
    procedure ResetError;
      {Resets error code and message to "no error" values of 0 and empty string.
      }
    property StdIn: THandle read fStdIn write fStdIn default 0;
      {Handle of console app's redirected standard input. Leave as 0 if standard
      input is not to be redirected. Ensure handle is inheritable}
    property StdOut: THandle read fStdOut write fStdOut default 0;
      {Handle of console app's redirected standard output. Leave as 0 if
      standard output not to be redirected. Ensure handle is inheritable}
    property StdErr: THandle read fStdErr write fStdErr default 0;
      {Handle of console app's redirected standard error. Leave as 0 if standard
      error is not to be redirected. Ensure handle is inheritable}
    property Visible: Boolean read fVisible write fVisible default False;
      {Determines whether console app is to be displayed of not}
    property MaxExecTime: LongWord read fMaxExecTime write SetMaxExecTime
      default cDefMaxExecTime;
      {Maximum execution time of console app in milliseconds. Set to INFINITE if
      no execution time limit is required (not recommended)}
    property TimeSlice: LongWord read fTimeSlice write SetTimeSlice
      default cDefTimeSlice;
      {Timeslice allocated to console app in milliseconds. The app is paused at
      end of each time slice and OnWork event is triggered. If TimeSlice is set
      to INFINITE (not recommended) then the app is never paused and OnWork is
      never triggered}
    property KillTimedOutProcess: Boolean
      read fKillTimedOutProcess write fKillTimedOutProcess
      default True;
      {When true any console app that times out will be killed off. When false a
      timeout causes the Execute method to return leaving the timed out console
      to run to completion}
    property ProcessAttrs: PSecurityAttributes
      read fProcessAttrs write SetProcessAttrs default nil;
      {Security and inheritance attributes for console process. Determines
      whether the process handle can be inherited by child processes. Setting to
      nil means the process handle can't be inherited. Setting to non-nil makes
      a copy of the provided structure}
    property ThreadAttrs: PSecurityAttributes
      read fThreadAttrs write SetThreadAttrs default nil;
      {Security and inheritance attributes for console's primary thread.
      Determines whether the thread handle can be inherited by child processes.
      Setting to nil means the thread handle can't be inherited. Setting to
      non-nil makes a copy of the provided structure}
    property UseNewConsole: Boolean
      read fUseNewConsole write fUseNewConsole default False;
      {When true causes console application to open a new console window. If
      false the console application uses any existing console}
    property ConsoleTitle: string
      read fConsoleTitle write fConsoleTitle;
      {Title to be displayed in any new console window. If left as default ''
      default title is used. If console app shares a console this property is
      ignored}
    property Environment: Pointer
      read fEnvironment write fEnvironment;
      {Pointer to environment block used by console application. The caller is
      responsible for allocating and freeing this memory, which must remain
      allocated while the console application is executing}
    property Priority: TPJConsoleAppPriority
      read fPriority write fPriority default cpDefault;
      {Priority with which console application is started}
    property TimeToLive: LongWord
      read fTimeToLive;
      {The amount of time in milliseconds a console app has left before timing
      out. This value is INFINITE if MaxExecTime is INFINITE}
    property ElapsedTime: LongWord
      read fElapsedTime;
      {Time in milliseconds since a console app began execution. The clock stops
      when the app completes or times out}
    property ProcessInfo: TProcessInformation
      read fProcessInfo;
      {Information about process including process and main thread handle. All
      fields zero when process not executing}
    property ExitCode: LongWord
      read fExitCode;
      {Exit code set by console app. Not valid if ErrorCode is non zero. Refer
      to console application documentation for meaning of these codes}
    property ErrorCode: LongWord
      read fErrorCode;
      {Zero if application executes successfully and non-zero if there was an
      error executing the application (e.g. if it timed out). Error codes either
      correspond to Windows error or are set by the class. Class generated error
      codes have bit 29 set}
    property ErrorMessage: string
      read fErrorMessage;
      {Error message corresponding to ErrorCode. '' if ErrorCode = 0}
    property OnWork: TNotifyEvent
      read fOnWork write fOnWork;
      {Event triggered each time console application signals the class.
      Frequency of these events depends on TimeSlice. If TimeSlice is INFINITE
      then OnWork is never triggered. Process handle is available}
    property OnComplete: TNotifyEvent
      read fOnComplete write fOnComplete;
      {Event triggered when application completes or times out. Always called.
      Called after last OnWork event. Use to tidy up after process completed.
      ProcessHandle is available. Use ErrorCode to check how application
      terminated}
  public
    constructor Create;
      {Class constructor. Instantiates object and sets default properties.
      }
    destructor Destroy; override;
      {Class destructor. Tidies up object.
      }
    function Execute(const CmdLine: string;
      const CurrentDir: string = ''): Boolean;
      {Executes the console application.
        @param CmdLine [in] Command line to execute. Includes program name and
          any parameters. Paths containing spaces must be quoted.
        @param CurrentDir [in] Application's current directory. Pass '' to use
          same current directory as parent application.
        @return True if command line application is run successfully or false if
          it fails to run. Note that this has nothing to do with application's
          exit code.
      }
    procedure Terminate;
      {Attempts to terminate the process. Calling this method causes the Execute
      method to return after the next OnWork event. If KillTimedOutProcess is
      true the console application will be halted. The method has no effect when
      TimeSlice=INFINITE.
      }
  end;

  {
  TPJConsoleApp:
    Class that encapsulates and executes a command line application and
    optionally redirects the application's standard input, output and error. The
    application is excuted in time slices and the class triggers an event
    between time slices.
  }
  TPJConsoleApp = class(TPJCustomConsoleApp)
  public
    // Published inherited properties
    property StdIn;
    property StdOut;
    property StdErr;
    property Visible;
    property MaxExecTime;
    property TimeSlice;
    property KillTimedOutProcess;
    property ProcessAttrs;
    property ThreadAttrs;
    property UseNewConsole;
    property ConsoleTitle;
    property Environment;
    property Priority;
    property TimeToLive;
    property ElapsedTime;
    property ProcessInfo;
    property ExitCode;
    property ErrorCode;
    property ErrorMessage;
    property OnWork;
    property OnComplete;
  end;


implementation


uses
  // Delphi
  SysUtils, DateUtils, Messages;


resourcestring
  // Error message
  sErrTimeout = 'Application timed out';
  sTerminated = 'Application terminated';

{ TPJCustomConsoleApp }

constructor TPJCustomConsoleApp.Create;
  {Class constructor. Instantiates object and sets default properties.
  }
begin
  inherited Create;
  // Set default property values
  fMaxExecTime := cDefMaxExecTime;
  fTimeSlice := cDefTimeSlice;
  fVisible := False;
  fStdIn := 0;
  fStdOut := 0;
  fStdErr := 0;
  fKillTimedOutProcess := True;
  ZeroProcessInfo;
  fProcessAttrs := nil;
  fThreadAttrs := nil;
  fUseNewConsole := False;
  fConsoleTitle := '';
  fEnvironment := nil;
  fPriority := cpDefault;
end;

destructor TPJCustomConsoleApp.Destroy;
  {Class destructor. Tidies up object.
  }
begin
  FreeSecurityAttrs(fProcessAttrs);
  FreeSecurityAttrs(fThreadAttrs);
  inherited;
end;

procedure TPJCustomConsoleApp.DoComplete;
  {Method called after completion. Triggers the OnComplete event.
  }
begin
  if Assigned(fOnComplete) then
    fOnComplete(Self);
end;

procedure TPJCustomConsoleApp.DoWork;
  {Method called between program timeslices and after completion but never
  called if TimeSlice=INFINITE. Triggers the OnWork event.
  }
begin
  if Assigned(fOnWork) then
    fOnWork(Self);
end;

function TPJCustomConsoleApp.Execute(const CmdLine, CurrentDir: string): Boolean;
  {Executes the console application.
    @param CmdLine [in] Command line to execute. Includes program name and any
      parameters. Paths containing spaces must be quoted.
    @param CurrentDir [in] Application's current directory. Pass '' to use same
      current directory as parent application.
    @return True if command line application is run successfully or false if it
      fails to run. Note that this has nothing to do with application's exit
      code.
  }
var
  ProcessInfo: TProcessInformation; // information about process
begin
  fExitCode := 0;
  ResetError;
  ZeroProcessInfo;
  Result := StartProcess(CmdLine, CurrentDir, ProcessInfo);
  if Result then
  begin
    // Process started: monitor its progress
    try
      fProcessInfo := ProcessInfo;
      Result := MonitorProcess and SetExitCode;
    finally
      // Process ended: tidy up
      ZeroProcessInfo;
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  end
  else
  begin
    // Couldn't start process: error
    RecordWin32Error;
    ZeroProcessInfo;
  end;
end;

procedure TPJCustomConsoleApp.FreeSecurityAttrs(var Attrs: PSecurityAttributes);
  {Frees memory for and nils a security attributes structure.
    @param Attrs [in/out] Pointer to the structure to be freed (may be nil). Set
      to nil on return.
  }
begin
  if Assigned(Attrs) then
  begin
    FreeMem(Attrs);
    Attrs := nil;
  end;
end;

function TPJCustomConsoleApp.GetProcessHandle: THandle;
  {Gets process handle from process info structure.
    @return Required process handle.
  }
begin
  Result := fProcessInfo.hProcess;
end;

function TPJCustomConsoleApp.MonitorProcess: Boolean;
  {Monitors a running process, triggering event at end of each timeslice and
  when completed.
    @return True on successful completion or false if application times out or
      is forced to terminate.
  }
var
  AppState: DWORD;        // State of app after last wait
  StartTime: TDateTime;   // Time application starts
begin
  Result := True;
  StartTime := Now;
  fRequestTerminate := False;
  fTimeToLive := fMaxExecTime;
  fElapsedTime := 0;
  repeat
    // Pause and wait for app - length determined by TimeSlice property
    AppState := WaitForSingleObject(GetProcessHandle, fTimeSlice);
    fElapsedTime := Int64Rec(DateUtils.MilliSecondsBetween(StartTime, Now)).Lo;
    if fMaxExecTime <> INFINITE then
      if fElapsedTime >= fMaxExecTime then
        fTimeToLive := 0
      else
        fTimeToLive := fMaxExecTime - fElapsedTime;
    if AppState = WAIT_FAILED then
    begin
      RecordWin32Error;
      Result := False;
    end
    else if fTimeSlice <> INFINITE then
      // All OK: do inter-timeslice processing
      DoWork;
  until (AppState <> WAIT_TIMEOUT) or (fTimeToLive <= 0) or fRequestTerminate;
  fTimeToLive := 0;
  // App halted or timed out: check which
  if (AppState = WAIT_TIMEOUT) or fRequestTerminate then
  begin
    if fRequestTerminate then
      RecordAppError(cAppErrorTerminated, sTerminated)
    else
      RecordAppError(cAppErrorTimeOut, sErrTimeout);
    DoComplete;   // call do complete before possibly terminating process
    Result := False;
    if KillTimedOutProcess then
      TerminateProcess(GetProcessHandle, fErrorCode);
  end
  else
    DoComplete;
end;

procedure TPJCustomConsoleApp.RecordAppError(const Code: LongWord;
  const Msg: string);
  {Set error code and message to class-defined error.
    @param Code [in] Required error code. Must have bit 29 set.
    @param Msg [in] Required error message.
  }
begin
  Assert(Code and cAppErrorMask <> 0);
  fErrorCode := Code;
  fErrorMessage := Msg;
end;

procedure TPJCustomConsoleApp.RecordWin32Error;
  {Set error code message to the last-reported Windows error.
  }
begin
  fErrorCode := GetLastError;
  fErrorMessage := SysErrorMessage(fErrorCode);
end;

procedure TPJCustomConsoleApp.ResetError;
  {Resets error code and message to "no error" values of 0 and empty string.
  }
begin
  fErrorCode := 0;
  fErrorMessage := '';
end;

function TPJCustomConsoleApp.SetExitCode: Boolean;
  {Sets ExitCode property to value returned from application. Sets error code
  if we fail to retrieve exit code.
    @return True if exit code retrieved OK and False if we fail to retrieve it.
  }
begin
  Result := GetExitCodeProcess(GetProcessHandle, fExitCode);
  if not Result then
    RecordWin32Error;
end;

procedure TPJCustomConsoleApp.SetMaxExecTime(const Value: LongWord);
  {Sets MaxExecTime property.
    @param Value [in] Required time in milliseconds. If 0 then property's
      default value is used.
  }
begin
  if Value = 0 then
    fMaxExecTime := cDefMaxExecTime
  else
    fMaxExecTime := Value;
end;

procedure TPJCustomConsoleApp.SetProcessAttrs(const Value: PSecurityAttributes);
  {Write access method for ProcessAttrs property. Makes a copy of new value if
  non nil.
    @param Values [in] New value. If nil ProcessAttrs is set nil otherwise
      ProcessAttrs points to a copy of structure pointed to by Value.
  }
begin
  UpdateSecurityAttrs(fProcessAttrs, Value);
end;

procedure TPJCustomConsoleApp.SetThreadAttrs(const Value: PSecurityAttributes);
  {Write access method for ThreadAttrs property. Makes a copy of new value if
  non nil.
    @param Values [in] New value. If nil ThreadAttrs is set nil otherwise
      ThreadAttrs points to a copy of structure pointed to by Value.
  }
begin
  UpdateSecurityAttrs(fThreadAttrs, Value);
end;

procedure TPJCustomConsoleApp.SetTimeSlice(const Value: LongWord);
  {Sets TimeSlice property.
    @param Value [in] Required time in milliseconds. If 0 then property's
      default value is used.
  }
begin
  if Value > 0 then
    fTimeSlice := Value
  else
    fTimeSlice := cDefTimeSlice;
end;

function TPJCustomConsoleApp.StartProcess(const CmdLine, CurrentDir: string;
  out ProcessInfo: TProcessInformation): Boolean;
  {Starts a process and gets information about it from OS.
    @param CmdLine [in] Command line to be executed.
    @param CurrentDir [in] Application's current directory. Pass '' to use same
      current directory as parent application.
    @param ProcessInfo [out] Passes OS's process info back to caller.
    @return True if process created OK and false if process couldn't be started.
  }
const
  // Maps Visible property to required wondows flags
  cShowFlags: array[Boolean] of Integer = (SW_HIDE, SW_SHOW);
  // Maps Priority property to creation flags
  cPriorityFlags: array[TPJConsoleAppPriority] of DWORD = (
    0, HIGH_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, IDLE_PRIORITY_CLASS,
    REALTIME_PRIORITY_CLASS
  );
var
  StartInfo: TStartupInfo;  // information about process from OS
  CurDir: PChar;            // stores current directory
  CreateFlags: DWORD;       // creation flags
begin
  // Set up startup information structure
  FillChar(StartInfo, Sizeof(StartInfo),#0);
  with StartInfo do
  begin
    cb := SizeOf(StartInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    if (fStdIn <> 0) or (fStdOut <> 0) or (fStdErr <> 0) then
      dwFlags := dwFlags or STARTF_USESTDHANDLES;          // we are redirecting
    if fConsoleTitle <> '' then
      lpTitle := PChar(fConsoleTitle);
    hStdInput := fStdIn;                   // std handles (non-zero => redirect)
    hStdOutput := fStdOut;
    hStdError := fStdErr;
    wShowWindow := cShowFlags[fVisible];                  // show or hide window
  end;

  // Set up process info structure
  ZeroProcessInfo;

  // Set creation flags
  CreateFlags := cPriorityFlags[fPriority];
  if fUseNewConsole then
    CreateFlags := CreateFlags or CREATE_NEW_CONSOLE;

  // Set current directory
  CurDir := nil;
  if CurrentDir <> '' then
    CurDir := PChar(CurrentDir);

  // Try to create the process
  Result := CreateProcess(
    nil,                  // no application name: we use command line instead
    PChar(CmdLine),       // command line
    fProcessAttrs,        // security attributes for process
    fThreadAttrs,         // security attributes for thread
    True,                 // we inherit inheritable handles from calling process
    CreateFlags,          // creation flags
    fEnvironment,         // environment block for new process
    CurDir,               // current directory
    StartInfo,            // informs how new process' window should appear
    ProcessInfo           // receives info about new process
  );
end;

procedure TPJCustomConsoleApp.Terminate;
  {Attempts to terminate the process. Calling this method causes the Execute
  method to return after the next OnWork event. If KillTimedOutProcess is true
  the console application will be halted. The method has no effect when
  TimeSlice=INFINITE.
  }
begin
  fRequestTerminate := True;
end;

procedure TPJCustomConsoleApp.UpdateSecurityAttrs(
  var OldValue: PSecurityAttributes; const NewValue: PSecurityAttributes);
  {Updates security attributes new new value.
    @param OldValue [in/out] Pointer to value to be updated. On return set to
      nil if NewValue is nil, otherwise set to point at copy of NewValue.
    @param NewValue [in] Pointer to new security attributes. May be nil.
  }
begin
  if Assigned(NewValue) then
  begin
    if not Assigned(OldValue) then
      GetMem(OldValue, SizeOf(TSecurityAttributes));
    OldValue^ := NewValue^;
  end
  else
    FreeSecurityAttrs(OldValue);
end;

procedure TPJCustomConsoleApp.ZeroProcessInfo;
  {Zeros the process information structure.
  }
begin
  FillChar(fProcessInfo, SizeOf(fProcessInfo), 0);
(*
    hProcess: THandle;
    hThread: THandle;
    dwProcessId: DWORD;
    dwThreadId: DWORD;
  fProcessInfo.hProcess := 0;
  fProcessInfo.hThread := 0;
  fProcessInfo.dwProcessId := 0;
  fProcessInfo.dwThreadId := 0;
*)
end;

end.

