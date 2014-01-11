{
 * PJResFile.pas
 *
 * Defines classes that encapsulate 32 bit binary resource files and the
 * individual resources within them. Also provides supporting routines and
 * constants.
 *
 * $Rev$
 * $Date$
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
 * The Original Code is PJResFile.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2004-2014 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit PJResFile;


{
  NOTES
  =====

  This unit defines classes that encapsulate the Windows 32 bit resource file
  format.

  BINARY RESOURCE FILE FORMAT
  ---------------------------

  A 32 bit resource file is comprised as follows:
    +--------------+
    | File header  |
    +--------------+
    | Resource 1   |
    +--------------+
    | Padding      |
    +--------------+
    | Resource 2   |
    +--------------+
    | Padding      |
    +--------------+
    ...          ...
    +--------------+
    | Resource N   |
    +--------------+
    | Padding      |
    +--------------+

  The File header is a "pseudo-resource" that identifies the file as a 32 bit
  resource file (rather than a 16 bit file). This is a 32 byte structure, the
  first 8 bytes of which are $00, $00, $00, $00, $20, $00, $00, $00.

  Each resource is made up of a variable length header record followed by the
  resource data.

  A resource file header is made up of the following fields:

    DataSize: DWORD;            // size of resource data (excl end padding)
    HeaderSize: DWORD;          // size of resource data header
    Type: Unicode or Ordinal;   // type of resource
    Name: Unicode or Ordinal;   // name of resource
    [Padding: Word];            // padding to DWORD boundary, if needed
    DataVersion: DWORD;         // version of the data resource
    MemoryFlags: Word;          // describes the state of the resource
    LanguageId: Word;           // language for the resource
    Version: DWORD;             // user defined resource version
    Characteristics: DWORD;     // user defined info about resource

  The resource name and type can either be a #0#0 terminated Unicode string or
  can be an ordinal word value (preceeded by a $FFFF word). If Type or Name is a
  Unicode string then an additional padding word may be needed after the Name
  "field" to ensure the following field start on a DWORD boundary. (The name
  field doesn't have to be DWORD aligned so there is no padding between the Type
  and Name "fields").

  Each resource starts on a DWORD boundary, so there may be padding bytes
  following the resource data if it is not a multiple of 4 bytes in length.

  IMPLEMENTATION NOTES
  --------------------

  Although the word "file" is used in these notes, this term also covers binary
  resource data stored in a stream.

  Two classes are used to encapsulate a resource file:

    + TPJResourceFile
      Encapsulates the whole file and has methods to load and save resource
      files, to add and delete resources and to find out information about the
      resources contained in the file.

    + TPJResourceEntry
      Encapsulates a single resource with a resource file. It exposes properties
      that give access to all the fields of the resource header and provides a
      stream onto the resource's data. Methods to check whether the resource
      matches certain criteria are also provided.

  While TPJResourceFile is a concrete class, TPJResourceEntry is abstract - it
  is used as an interface to actual concrete resource entry instances maintained
  internally by TPJResourceFile. This approach is used because instances of
  TPJResourceEntry must not be directly instantiated: all resources are "owned"
  by a resource file object. New instances of resource entry objects are created
  internally by TPJResourceFile in response to methods and constructors.

  Since the resource type and name identifiers are variable length we can't use
  a standard Pascal record to represent a resource header. Instead we use two
  fixed length packed records:
    TResEntryHdrPefix:  the DataSize and HeaderSize fields.
    TResEntryHdrSuffix: the DataVersion thru to Characteristics fields.
  We handle the resource type and name "fields" (and any padding) separately.

  When interogating or accessing Windows resources using the Windows API
  resource types and names are specified either as #0 terminated strings
  or as ordinal values as returned from the MakeIntResource "macro". This
  convention is also used by the TPJResourceFile and TPJResourceEntry classes -
  the methods that take resource identifiers as parameters all expect them to be
  in this form. Note that 32 bit resource files use the Unicode or Ordinal
  format described in the Binary Resource File Format section above. The classes
  convert from the resource file format to and from the API format on saving and
  loading resource files.
}


{$UNDEF UseAnsiStrIComp}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and later
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and later
    {$DEFINE UseAnsiStrIComp}
  {$IFEND}
{$ENDIF}


interface


uses
  // Delphi
  SysUtils, Classes, Windows;


const
  // Memory flags constants - used in MemoryFlags field of resource header
  // These flags can be ORd together as a bitmask
  RES_MF_MOVEABLE     = $0010;                // can move res in memory
  RES_MF_PURE         = $0020;                // res data is DWORD aligned
  RES_MF_PRELOAD      = $0040;                // must load after app loads
  RES_MF_DISCARDABLE  = $1000;                // can be unloaded if mem low
  // These flags can be ANDed with bitmask to remove complementary flag
  RES_MF_FIXED        = not RES_MF_MOVEABLE;  // can't move res in memory
  RES_MF_IMPURE       = not RES_MF_PURE;      // data not aligned: needs padding
  RES_MF_LOADONCALL   = not RES_MF_PRELOAD;   // load only when app accesses
  // NOTE: RES_MF_MOVEABLE, RES_MF_IMPURE and RES_MF_PRELOAD ignored by Win NT

  // System resource types not defined in Delphi Windows unit for some supported
  // Delphis
  RT_HTML             = MakeIntResource(23);  // HTML resources
  RT_MANIFEST         = MakeIntResource(24);  // XP manifest resource


type

  TPJResourceEntry = class;

  {
  TPJResourceFile:
    Class that encapsulates a 32 bit binary resource file and exposes the
    entries within it. This class allows reading, creation and editing of
    resource files.
  }
  TPJResourceFile = class(TObject)
  private
    fEntries: TList;  // Maintains list of all resource entries.
    function GetEntry(Idx: Integer): TPJResourceEntry;
      {Read accessor for Entries[] property.
        @param Idx [in] Index of wanted entry.
        @return Wanted entry.
      }
    function GetEntryCount: Integer;
      {Read accessor for EntryCount property.
        @return Number of entries in resource.
      }
  public
    constructor Create;
      {Creates a new empty resource file object.
      }
    destructor Destroy; override;
      {Destroys resource file object.
      }
    procedure Clear;
      {Clears all resources from file object.
      }
    function DeleteEntry(const Entry: TPJResourceEntry): Boolean;
      {Deletes an entry from the resource file object if it exists.
        @param Entry [in] Resource entry object to delete.
        @return True if entry deleted, False if entry not in resource file.
      }
    function IndexOfEntry(const Entry: TPJResourceEntry): Integer;
      {Returns index number of a resource entry in resource file.
        @param Entry [in] Resource entry object to find.
        @return Index of entry in resource file object or -1 if not found.
      }
    procedure LoadFromFile(const FileName: TFileName);
      {Loads resource file from given file
        @param FileName [in] Resource file name.
        @except Raised if file doesn't exist.
        @except Raised if resource file invalid.
      }
    procedure LoadFromStream(const Stm: TStream);
      {Loads resource data from given stream.
        @param Stm [in] Stream containing the resource file.
        @except Raised if resource file invalid.
      }
    procedure SaveToFile(const FileName: TFileName);
      {Saves resource to given file.
        @param FileName [in] File where resource is to be saved.
      }
    procedure SaveToStream(const Stm: TStream);
      {Saves resource data to given stream.
        @param Stm [in] Stream where resource data will be saved.
      }
    class function IsValidResourceStream(const Stm: TStream): Boolean;
      {Checks if the given stream contains a valid resource file at the current
      location.
        @return True if stream contains valid 32 bit resource file header at
          current location, False if not.
      }
    function AddEntry(const ResType, ResName: PChar;
      const LangID: Word = 0): TPJResourceEntry; overload;
      {Adds new empty resource entry to resource file that has given type, name
      and language and returns reference to it.
        @param ResType [in] Resource type (ordinal or string).
        @param ResName [in] Resource name (ordinal or string).
        @param LangID [in] Pptional language ID - 0 (language neutral) used by
          default.
        @return Reference to new entry.
        @except Raised if an entry already exists with same type, name and
          language id.
      }
    function AddEntry(const Entry: TPJResourceEntry; const ResName: PChar;
      const LangID: Word = 0): TPJResourceEntry; overload;
      {Adds a copy of the given resource entry of the same type with the given
      name and language ID.
        @param Entry [in] Resource entry to be copied.
        @param ResName [in] Name of the new resource.
        @param LangID [in] Optional language ID of the new entry.
        @return Reference to new resource entry.
        @except Raised if an entry already exists with same type, name and
          language id.
      }
    function FindEntry(const ResType, ResName: PChar;
      const LangID: Word = $FFFF): TPJResourceEntry;
      {Finds a resource entry with the given type, name and language id and
      returns a reference to it. Search can ignore resource name and or language
      id in which case first entry that matches is returned.
        @param ResType [in] Resource type (ordinal or string) - required.
        @param ResName [in] Resource name (ordinal or string) - if ResName is
          nil it is not used in match.
        @param LangID [in] Optional language ID of entry to find - if not
          supplied (or is $FFFF) it is not used in match.
        @return Reference to found resource entry or nil if there is no match.
      }
    function FindEntryIndex(const ResType, ResName: PChar;
      const LangID: Word = $FFFF): Integer;
      {Finds a resource entry with the given type, name and language id and
      returns the index of the entry in the Entries[] property. Search can
      ignore resource name or language id in which case first entry that matches
      is returned.
        @param ResType [in] Resource type (ordinal or string) - required.
        @param ResName [in] Resource name (ordinal or string) - if ResName is
          nil it is not used in match.
        @param LangID [in] Pptional language ID of entry to find - if not
          supplied (or is $FFFF) it is not used in match.
        @return Index of entry in Entries[] or -1 if no matching entry.
      }
    function EntryExists(const ResType, ResName: PChar;
      const LangID: Word = $FFFF): Boolean;
      {Returns whether a resource entry matching given search criteria exists.
        @param ResType [in] Resource type (ordinal or string) - required.
        @param ResName [in] Resource name (ordinal or string) - if ResName is
          nil it is not used in match.
        @param LangID [in] Optional language ID of entry to find - if not
          supplied (or is $FFFF) it is not used in match.
        @return True if a matching entry exists in the resource file.
      }
    property EntryCount: Integer read GetEntryCount;
      {Number of resource entries in the resource file}
    property Entries[Idx: Integer]: TPJResourceEntry read GetEntry;
      {Array of resource entries in resource file}
  end;


  {
  TPJResourceEntry:
    Abstract class that encapsulates an entry in a resource file.
    Implementations enable resource name, type and other properties to be set
    and allow resource's data to be read & written. The resource data is treated
    as raw binary bytes and it is for the user to interpret the meaning of the
    data. This class should not be directly instantiated but should be used to
    reference resource entry objects created internally by TPJResourceFile.
  }
  TPJResourceEntry = class(TObject)
  protected // abstract property access methods
    function GetCharacteristics: DWORD; virtual; abstract;
    procedure SetCharacteristics(const Value: DWORD); virtual; abstract;
    function GetData: TStream; virtual; abstract;
    function GetDataSize: DWORD; virtual; abstract;
    function GetDataVersion: DWORD; virtual; abstract;
    procedure SetDataVersion(const Value: DWORD); virtual; abstract;
    function GetHeaderSize: DWORD; virtual; abstract;
    function GetLanguageID: Word; virtual; abstract;
    function GetMemoryFlags: Word; virtual; abstract;
    procedure SetMemoryFlags(const Value: Word); virtual; abstract;
    function GetResName: PChar; virtual; abstract;
    function GetResType: PChar; virtual; abstract;
    function GetVersion: DWORD; virtual; abstract;
    procedure SetVersion(const Value: DWORD); virtual; abstract;
  public
    function IsMatching(const ResType, ResName: PChar;
      const LangID: Word = $FFFF): Boolean; overload; virtual; abstract;
      {Returns true if the resource entry has the given type, name and language
      id. Name and language ID can be omitted from the match but resource Type
      is required.
        @param ResType [in] Matching resource type.
        @param ResName [in] Matching resource name (ignored if nil).
        @param LangID [in] Matching language id (ignored if $FFFF).
        @return True if a match is found, False otherwise.
      }
    function IsMatching(const Entry: TPJResourceEntry): Boolean; overload;
      virtual; abstract;
      {Returns true if the resource entry has the same resource type, name and
      language Id as the given enrty.
        @param Entry [in] Resource entry to match
        @return True if the entries match, False otherwise.
      }
    property DataSize: DWORD
      read GetDataSize;
      {Size of resource data, excluding padding}
    property HeaderSize: DWORD
      read GetHeaderSize;
      {Size of resource header record including internal padding}
    property DataVersion: DWORD
      read GetDataVersion write SetDataVersion;
      {Predefined data resource version information}
    property MemoryFlags: Word
      read GetMemoryFlags write SetMemoryFlags;
      {Attribute bitmask specifying state of resource}
    property LanguageID: Word
      read GetLanguageID;
      {Language of resource (0 => language neutral)}
    property Version: DWORD
      read GetVersion write SetVersion;
      {User specified version number for resource data}
    property Characteristics: DWORD
      read GetCharacteristics write SetCharacteristics;
      {Further user specified data information}
    property ResName: PChar
      read GetResName;
      {Name of resource (string or ordinal value)}
    property ResType: PChar
      read GetResType;
      {Type of resource (string or ordinal value)}
    property Data: TStream
      read GetData;
      {Stream containing raw resource data (excludes padding)}
  end;


  {
  EPJResourceFile:
    Class of exception raised by objects in this unit.
  }
  EPJResourceFile = class(Exception);


function IsIntResource(const ResID: PChar): Boolean;
  {Informs if a resource id (name or type) is numeric.
    @param ResID [in] Id to test.
    @return True if id is numeric, False if it is a string value.
  }

function IsEqualResID(const R1, R2: PChar): Boolean;
  {Checks whether two resource ids are equal.
    @param R1 [in] First resource id to test.
    @param R2 [in] Second resource id to test.
    @return True if R1 and R2 are same type (ordinal or string) and have same
      value (case insensitive for string types).
  }

function ResIDToStr(const ResID: PChar): string;
  {Converts a resource ID into its string representation.
    @param ResID [in] Resource ID to be converted.
    @return String representation of ResID - if resource id is an ordinal the
      ordinal number preceeded by '#' is returned, otherwise the string itself
      is returned.
  }


implementation


type
  {
  TResEntryHdrPrefix:
    Record that stores the fixed length fields that preceed the variable length
    type and name records in a resource header.
  }
  TResEntryHdrPrefix = packed record
    DataSize: DWORD;
    HeaderSize: DWORD;
  end;

  {
  TResEntryHdrSuffix:
    Record that stores the fixed length fields that follow the variable length
    type and name records in a resource header.
  }
  TResEntryHdrSuffix = packed record
    DataVersion: DWORD;
    MemoryFlags: Word;
    LanguageID: Word;
    Version: DWORD;
    Characteristics: DWORD;
  end;

  {
  TInternalResEntry:
    Implementation of the abstract TPJResourceEntry class that encapsulates an
    entry in a resource file. Enables resource name, type and other properties
    to be set and allows resource's data to be read & written. The resource data
    is treated as raw binary bytes.
  }
  TInternalResEntry = class(TPJResourceEntry)
  private
    fResName: PChar;
      {Stores or points to resource name}
    fResType: PChar;
      {Stores or points to resource type}
    fHdrSuffix: TResEntryHdrSuffix;
      {Fixed length fields that follow variable size fields of resource header}
    fDataStream: TStream;
      {Stream that stores entry's raw resource data}
    fOwner: TPJResourceFile;
      {Resource file instance that owns this entry}
    procedure Init(const Owner: TPJResourceFile);
      {Helper method for constructors. Initialises new resource entry object.
        @param Owner [in] Owning TPJResourceFile instance.
      }
    procedure FinaliseResID(var ResID: PChar);
      {Finalises a resource identifier and sets it to nil. If the identifier
      points to a string the string's memory is first released.
        @param Owner [in] TPJResourceFile instance that owns new resource entry.
      }
    procedure CopyResID(var Dest: PChar; const Src: PChar);
      {Copies one resource identifier to another taking care of memory
      allocations.
        @param Dest [in] Identifier receiving the resource identifier. If the
          identifier is ordinal Dest is set to its value but if Dest is a string
          then the string is copied and Dest is set to point to it.
        @param Src [in] Identifier to be copied.
      }
  protected
    function GetCharacteristics: DWORD; override;
      {Gets user defined resource characteristics.
        @return Value of Characteristics field from resource header.
      }
    procedure SetCharacteristics(const Value: DWORD); override;
      {Sets value of Characteristics field of resource header.
        @param Value the new Characteristics value.
      }
    function GetData: TStream; override;
      {Gets reference to raw data stream.
        @return Reference to stream used to store raw resource data.
      }
    function GetDataSize: DWORD; override;
      {Gets size of resource data (excluding any padding).
        @return Value of DataSize field from resource header.
      }
    function GetDataVersion: DWORD; override;
      {Gets predefined data resource version number.
        @return Value of DataVersion field from resource header.
      }
    procedure SetDataVersion(const Value: DWORD); override;
      {Sets value of DataVersion field of header.
        @param Value the new DataVersion number.
      }
    function GetHeaderSize: DWORD; override;
      {Calculates size of variable length resource header.
         @return Size of resource header in bytes.
      }
    function GetLanguageID: Word; override;
      {Gets resource's Language ID.
        @return Value of LanguageID field of resource header.
      }
    function GetMemoryFlags: Word; override;
      {Gets bitmask of attributes giving state of resource.
        @return MemoryFlags field of resource header.
      }
    procedure SetMemoryFlags(const Value: Word); override;
      {Sets value MemoryFlags field of resource header.
        @param Value the new MemoryFlags bitmask.
      }
    function GetResName: PChar; override;
      {Gets resource name.
        @return Either pointer to resource name string or its ordinal value.
      }
    function GetResType: PChar; override;
      {Gets resource type.
        @return Either pointer to resource name string or its ordinal value.
      }
    function GetVersion: DWORD; override;
      {Gets user specified version number for resource data.
        @return Value of Version field of resource header.
      }
    procedure SetVersion(const Value: DWORD); override;
      {Sets value of Version field of resource header.
        @param Value the new Version number.
      }
  public
    constructor Create(const Owner: TPJResourceFile;
      const ResType, ResName: PChar; LangID: Word); overload;
      {Class constructor. Called by owning TPJResourceFile instance to create a
      new empty resource entry.
        @param Owner [in] Owning TPJResourceFile instance.
        @param ResType [in] New entry's resource type.
        @param ResName [in] New entry's resource name.
        @param LangID [in] New entry's language id.
      }
    constructor Create(const Owner: TPJResourceFile;
      const Stm: TStream); overload;
      {Class constructor. Called by owning TPJResourceFile instance to create a
      new resource entry from the data in a stream.
        @param Owner [in] Owning TPJResourceFile instance.
        @param Stm [in] Stream containing the binary resource entry data.
      }
    destructor Destroy; override;
      {Class destructor.
      }
    procedure WriteToStream(Stm: TStream);
      {Writes resource entry to a stream.
        @param Stm [in] Stream where the entry is written.
      }
    function IsMatching(const ResType, ResName: PChar;
      const LangID: Word = $FFFF): Boolean; overload; override;
      {Checks if a resource entry has a required type, name and language id.
      Name and LangID can be omitted from the match. ResType is required.
        @param ResType [in] Matching resource type.
        @param ResName [in] Matching resource name (ignored if nil).
        @param LangID [in] Matching language id (ignored if $FFFF).
        @return True if a match is found, False otherwise.
      }
    function IsMatching(const Entry: TPJResourceEntry): Boolean; overload;
      override;
      {Checks if a resource entry has the same resource type, name and language
      Id as another entry.
        @param Entry [in] Resource entry to match
        @return True if the entries match, False otherwise.
      }
  end;

resourcestring
  // Error messages
  sErrBadResFile      = 'Invalid 32 bit resource file';
  sErrDupResEntry     = 'Duplicate entry: can''t add to resource file';
  sErrEndOfStream     = 'Unexpected end of stream when reading resource entry';
  sErrCorruptHeader   = 'Encountered corrupt header size field when reading '
                      + 'resource header';
  sErrHeaderCalc      = 'Error calculating header size while writing resource '
                      + 'entry';
  sErrResWrite        = 'Error writing resource data to stream';


{ Public helper routines }

function IsIntResource(const ResID: PChar): Boolean;
begin
  Result := (HiWord(DWORD(ResID)) = 0);
end;

function ResIDToStr(const ResID: PChar): string;
begin
  if IsIntResource(ResID) then
    Result := '#' + IntToStr(LoWord(DWORD(ResID)))
  else
    Result := ResID;
end;

function IsEqualResID(const R1, R2: PChar): Boolean;
begin
  if IsIntResource(R1) then
    // R1 is ordinal: R2 must also be ordinal with same value in lo word
    Result := IsIntResource(R2) and (LoWord(DWORD(R1)) = LoWord(DWORD(R2)))
  else
    // R1 is string pointer: R2 must be same string (ignoring case)
    Result := not IsIntResource(R2) and (StrIComp(R1, R2) = 0);
end;


{ TInternalResEntry }

procedure TInternalResEntry.CopyResID(var Dest: PChar; const Src: PChar);
begin
  // Clear up the old destination identifier
  FinaliseResID(Dest);  // Dest is set to nil here
  if IsIntResource(Src) then
    // Ordinal value: store in Dest
    Dest := Src
  else
    // String value: make Dest point to copy of string
    Dest := StrNew(Src);
end;

constructor TInternalResEntry.Create(const Owner: TPJResourceFile;
  const Stm: TStream);

  // ---------------------------------------------------------------------------
  procedure Read(out Value; const Size: Integer; var BytesRead: Integer);
    {Reads a value from the stream and raises exception if all required bytes
    can't be read. Updates count of total bytes read.
      @param Value [out] Value to be read from stream.
      @param Size [in] Size of the value to be read.
      @param BytesRead [in/out] Updated total of all bytes read.
    }
  begin
    // Read stream and check all expected bytes read
    if Stm.Read(Value, Size) <> Size then
      raise EPJResourceFile.Create(sErrEndOfStream);
    // Update count of total bytes read
    Inc(BytesRead, Size);
  end;

  procedure SkipToBoundary(var BytesRead: Integer);
    {Reads bytes from the stream as necessary to ensure the number of bytes read
    is a multiple of the size of a DWORD.
      @param BytesRead [in/out] Updated number of bytes read.
    }
  var
    SkipBytes: Integer; // number of bytes to skip
    Dummy: DWORD;       // temp store for bytes read
  begin
    if BytesRead mod SizeOf(DWORD) <> 0 then
    begin
      SkipBytes := SizeOf(DWORD) - BytesRead mod SizeOf(DWORD);
      Read(Dummy, SkipBytes, BytesRead);
    end;
  end;

  procedure ReadResID(out ResID: PChar; var BytesRead: Integer);
    {Reads a resource identifier from the stream and updates total bytes read.
      @param ResID [out] Resource identifier read from stream.
      @param BytesRead [in/out] Updated total of bytes read.
    }
  var
    Ch: WideChar;     // store wide chars read from stream
    Str: WideString;  // string resource id
  begin
    Assert(SizeOf(Word) = SizeOf(WideChar));
    // Read first WideChar: determines type of resource id
    Read(Ch, SizeOf(Ch), BytesRead);
    if Ord(Ch) = $FFFF then
    begin
      // First char is $FFFF so this is ordinal resource id
      // next character contains resource id: stored in out parameter
      Read(Ch, SizeOf(Ch), BytesRead);
      CopyResID(ResID, MakeIntResource(Ord(Ch)));
    end
    else
    begin
      // First char not $FFFF so this is string resource id
      // we read each character into string until zero char encountered
      Str := Ch;
      Read(Ch, SizeOf(Ch), BytesRead);
      while Ord(Ch) <> 0 do
      begin
        Str := Str + Ch;
        Read(Ch, SizeOf(Ch), BytesRead);
      end;
      // we now copy resource string, converted to string, to resource id
      // *** there would be a shorter way than this when string = UnicodeString,
      //     but this is needed for string = AnsiString and works for both.
      CopyResID(ResID, PChar(WideCharToString(PWideChar(Str))));
    end;
  end;
  // ---------------------------------------------------------------------------

var
  BytesRead: Integer;             // total # of bytes read from stream
  HdrPrefix: TResEntryHdrPrefix;  // fixed size resource header prefix
begin
  // Initialise new object
  Init(Owner);
  // Read header
  // start counting bytes
  BytesRead := 0;
  // read fixed header prefix
  Read(HdrPrefix, SizeOf(HdrPrefix), BytesRead);
  // read variable type and name resource ids then skip to DWORD boundary
  ReadResID(fResType, BytesRead);
  ReadResID(fResName, BytesRead);
  SkipToBoundary(BytesRead);
  // read fixed header suffix
  Read(fHdrSuffix, SizeOf(fHdrSuffix), BytesRead);
  // check header length was as expected
  if Int64(BytesRead) <> Int64(HdrPrefix.HeaderSize) then
    raise EPJResourceFile.Create(sErrCorruptHeader);
  // Read any resource data into data stream
  if HdrPrefix.DataSize > 0 then
  begin
    // check stream is large enough for expected data
    if Stm.Size < Stm.Position + Int64(HdrPrefix.DataSize) then
      raise EPJResourceFile.Create(sErrEndOfStream);
    // copy data from input stream into resource data stream & reset it
    fDataStream.CopyFrom(Stm, HdrPrefix.DataSize);
    fDataStream.Position := 0;
    Inc(BytesRead, HdrPrefix.DataSize);
    // skip any padding bytes following resource data
    SkipToBoundary(BytesRead);
  end;
end;

constructor TInternalResEntry.Create(const Owner: TPJResourceFile;
  const ResType, ResName: PChar; LangID: Word);
begin
  // Initialise new object
  Init(Owner);
  // Store type and name resource ids
  CopyResID(fResType, ResType);
  CopyResID(fResName, ResName);
  // Record language id
  fHdrSuffix.LanguageID := LangID;
end;

destructor TInternalResEntry.Destroy;
begin
  // Free resource identifier storage
  FinaliseResID(fResType);
  FinaliseResID(fResName);
  // Free resource data stream
  fDataStream.Free;
  // Delete from owner list
  if Assigned(fOwner) then
    fOwner.DeleteEntry(Self);
  inherited;
end;

procedure TInternalResEntry.FinaliseResID(var ResID: PChar);
begin
  // Check resource id not already finalised
  if Assigned(ResID) then
  begin
    if not IsIntResource(ResID) then
      // This is string resource: free the string's memory
      StrDispose(ResID);
    // Zero the identifier
    ResID := nil;
  end;
end;

function TInternalResEntry.GetCharacteristics: DWORD;
begin
  Result := fHdrSuffix.Characteristics;
end;

function TInternalResEntry.GetData: TStream;
begin
  Result := fDataStream;
end;

function TInternalResEntry.GetDataSize: DWORD;
begin
  Result := fDataStream.Size;
end;

function TInternalResEntry.GetDataVersion: DWORD;
begin
  Result := fHdrSuffix.DataVersion;
end;

function TInternalResEntry.GetHeaderSize: DWORD;

  // ---------------------------------------------------------------------------
  function ResIDSize(const ResID: PChar): Integer;
    {Calculates size of a resource identifier when stored.
      @param ResID [in] Resource identifier we want size of.
      @return The required size.
    }
  begin
    if IsIntResource(ResID) then
      // Ordinal resource id: want size of a DWORD
      Result := SizeOf(DWORD)
    else
      // String resource id: want length of string in WideChars + terminating
      // zero WideChar
      Result := (StrLen(ResID) + 1) * SizeOf(WideChar);
  end;
  // ---------------------------------------------------------------------------

begin
  Assert(SizeOf(WideChar) = SizeOf(Word));
  // Add up size of fixed and variable parts of header
  Result := SizeOf(TResEntryHdrPrefix) + SizeOf(TResEntryHdrSuffix) +
    ResIDSize(fResType) + ResIDSize(fResName);
  // Round up to multiple of DWORD if required
  Assert(Result mod SizeOf(Word) = 0);
  if Result mod SizeOf(DWORD) <> 0 then
    Inc(Result, SizeOf(Word));
end;

function TInternalResEntry.GetLanguageID: Word;
begin
  Result := fHdrSuffix.LanguageID;
end;

function TInternalResEntry.GetMemoryFlags: Word;
begin
  Result := fHdrSuffix.MemoryFlags;
end;

function TInternalResEntry.GetResName: PChar;
begin
  Result := fResName;
end;

function TInternalResEntry.GetResType: PChar;
begin
  Result := fResType;
end;

function TInternalResEntry.GetVersion: DWORD;
begin
  Result := fHdrSuffix.Version;
end;

procedure TInternalResEntry.Init(const Owner: TPJResourceFile);
begin
  Assert(Assigned(Owner));
  inherited Create;
  // Record owner
  fOwner := Owner;
  // Create stream to hold resource data
  fDataStream := TMemoryStream.Create;
  // Clear all field in resource header suffix
  FillChar(fHdrSuffix, SizeOf(fHdrSuffix), 0);
end;

function TInternalResEntry.IsMatching(const ResType, ResName: PChar;
  const LangID: Word = $FFFF): Boolean;
begin
  // Check if types are same
  Result := IsEqualResID(ResType, Self.ResType);
  if Assigned(ResName) then
    // ResName is assigned so check names are same
    Result := Result and IsEqualResID(ResName, Self.ResName);
  if LangID <> $FFFF then
    // Language ID is provided so check languages are same
    Result := Result and (LangID = Self.LanguageID);
end;

function TInternalResEntry.IsMatching(const Entry: TPJResourceEntry): Boolean;
begin
  // Check that entry's resource type & name and language matches ours
  Result := IsMatching(Entry.ResType, Entry.ResName, Entry.LanguageID);
end;

procedure TInternalResEntry.SetCharacteristics(const Value: DWORD);
begin
  fHdrSuffix.Characteristics := Value;
end;

procedure TInternalResEntry.SetDataVersion(const Value: DWORD);
begin
  fHdrSuffix.DataVersion := Value;
end;

procedure TInternalResEntry.SetMemoryFlags(const Value: Word);
begin
  fHdrSuffix.MemoryFlags := Value;
end;

procedure TInternalResEntry.SetVersion(const Value: DWORD);
begin
  fHdrSuffix.Version := Value;
end;

procedure TInternalResEntry.WriteToStream(Stm: TStream);

  // ---------------------------------------------------------------------------
  procedure Write(const Value; const Size: Integer; var BytesWritten: Integer);
    {Writes a value to the stream. Raises exception if all required bytes can't
    be written. Updates count of bytes written.
      @param Value [in] Value to be written.
      @param Size [in] Size of the value to be written.
      @param BytesWritten [in/out] Updated total of all bytes written.
    }
  begin
    // Write stream and check all expected bytes read
    if Stm.Write(Value, Size) <> Size then
      raise EPJResourceFile.Create('Error writing resource entry to stream');
    // Update count of bytes written
    Inc(BytesWritten, Size);
  end;

  procedure WriteResID(ResID: PChar; var BytesWritten: Integer);
    {Writes a resource identifier to the stream and updates total of bytes
    written.
      @param ResID [in] Resource identifier to be written.
      @param BytesWritten [in/out] Updated total of bytes written to date.
    }
  var
    OrdValue: DWORD;        // resource id ordinal value
    StrValue: WideString;   // resource id string value
  begin
    if IsIntResource(ResID) then
    begin
      // This is ordinal: create and write out required DWORD
      OrdValue := $0000FFFF or (DWORD(LoWord(ResID)) shl 16);
      Write(OrdValue, SizeOf(OrdValue), BytesWritten);
    end
    else
    begin
      // This is string: create and write out required wide string
      StrValue := WideString(string(ResID));
      Write(
        StrValue[1], (Length(StrValue) + 1) * SizeOf(WideChar), BytesWritten
      );
    end;
  end;

  procedure WriteToBoundary(var BytesWritten: Integer);
    {Write bytes to the stream to ensure the number of bytes written is a
    multiple of the size of a DWORD.
      @param BytesWritten [in/out] Updated number of bytes written.
    }
  const
    cPadding: DWORD = 0;  // stores zero bytes for writing out as padding
  var
    PadBytes: Integer;    // number of padding bytes needed
  begin
    if BytesWritten mod SizeOf(DWORD) <> 0 then
    begin
      PadBytes := SizeOf(DWORD) - BytesWritten mod SizeOf(DWORD);
      Write(cPadding, PadBytes, BytesWritten);
    end;
  end;
  // ---------------------------------------------------------------------------

var
  BytesWritten: Integer;          // count of bytes written to stream
  HdrPrefix: TResEntryHdrPrefix;  // fixed size resource header prefix
begin
  // Initialise number of bytes written
  BytesWritten := 0;
  // Write header
  // write data size and header size
  HdrPrefix.DataSize := GetDataSize;
  HdrPrefix.HeaderSize := GetHeaderSize;
  Write(HdrPrefix, SizeOf(HdrPrefix), BytesWritten);
  // write type and name resource ids, padded to DWORD boundary
  WriteResID(fResType, BytesWritten);
  WriteResID(fResName, BytesWritten);
  WriteToBoundary(BytesWritten);
  // write fixed header suffix (updated via properties)
  Write(fHdrSuffix, SizeOf(fHdrSuffix), BytesWritten);
  // check correct size header written
  if Int64(BytesWritten) <> Int64(HdrPrefix.HeaderSize) then
    raise EPJResourceFile.Create(sErrHeaderCalc);
  // Write any resource data
  if HdrPrefix.DataSize > 0 then
  begin
    // copy whole of resource data stream to output stream
    fDataStream.Position := 0;
    try
      Stm.CopyFrom(fDataStream, HdrPrefix.DataSize);
      Inc(BytesWritten, HdrPrefix.DataSize);
    except
      // convert any write error to EPJResourceFile error
      raise EPJResourceFile.Create(sErrResWrite);
    end;
    // rewind resource data stream
    fDataStream.Position := 0;
    // write out any required padding bytes to make length multiple of DWORD
    WriteToBoundary(BytesWritten);
  end;
end;

{ TPJResourceFile }

function TPJResourceFile.AddEntry(const ResType, ResName: PChar;
  const LangID: Word = 0): TPJResourceEntry;
begin
  // Check matching entry not already in file
  if Assigned(FindEntry(ResType, ResName, LangID)) then
    raise EPJResourceFile.Create(sErrDupResEntry);
  // Create new resource entry and add to list
  Result := TInternalResEntry.Create(Self, ResType, ResName, LangID);
  fEntries.Add(Result);
end;

function TPJResourceFile.AddEntry(const Entry: TPJResourceEntry;
  const ResName: PChar; const LangID: Word): TPJResourceEntry;
var
  OldPos: Longint;  // position in entry to be copied data stream
begin
  // Create new empty entry
  Result := AddEntry(Entry.ResType, ResName, LangID);
  // Copy read/write ordinal properties
  Result.DataVersion := Entry.DataVersion;
  Result.MemoryFlags := Entry.MemoryFlags;
  Result.Version := Entry.Version;
  Result.Characteristics := Entry.Characteristics;
  // Copy given entry's data to new entry, preserving position in stream
  OldPos := Entry.Data.Position;
  Entry.Data.Position := 0;
  Result.Data.CopyFrom(Entry.Data, Entry.Data.Size);
  Entry.Data.Position := OldPos;
  // Reset new entry's stream position
  Result.Data.Position := 0;
end;

procedure TPJResourceFile.Clear;
var
  Idx: Integer; // loops thru all entries
begin
  // Free all resource entry instances
  for Idx := Pred(EntryCount) downto 0 do
    Entries[Idx].Free;  // this unlinks entry from list
end;

constructor TPJResourceFile.Create;
begin
  inherited;
  // Create list to store resource entries
  fEntries := TList.Create;
end;

function TPJResourceFile.DeleteEntry(const Entry: TPJResourceEntry): Boolean;
var
  Idx: Integer; // index of entry in list
begin
  // Find index of entry in list, if exists
  Idx := IndexOfEntry(Entry);
  Result := Idx > -1;
  if Result then
    // Delete found entry from list
    fEntries.Delete(Idx);
end;

destructor TPJResourceFile.Destroy;
begin
  Clear;
  fEntries.Free;
  inherited;
end;

function TPJResourceFile.EntryExists(const ResType, ResName: PChar;
  const LangID: Word = $FFFF): Boolean;
begin
  Result := Assigned(FindEntry(ResType, ResName, LangID));
end;

function TPJResourceFile.FindEntry(const ResType,
  ResName: PChar; const LangID: Word = $FFFF): TPJResourceEntry;
var
  Idx: Integer; // loops thru all resource entries in file
begin
  // Loop thru entries checking if they match type, name and language id
  Result := nil;
  for Idx := 0 to Pred(EntryCount) do
    if Entries[Idx].IsMatching(ResType, ResName, LangID) then
    begin
      Result := Entries[Idx];
      Break;
    end;
end;

function TPJResourceFile.FindEntryIndex(const ResType, ResName: PChar;
  const LangID: Word = $FFFF): Integer;
var
  Entry: TPJResourceEntry;  // matching resource entry instance
begin
  // Try to find resource entry matching type, name and language
  Entry := FindEntry(ResType, ResName, LangID);
  if Assigned(Entry) then
    // Found entry: get index in list
    Result := IndexOfEntry(Entry)
  else
    // No matching entry
    Result := -1;
end;

function TPJResourceFile.GetEntry(Idx: Integer): TPJResourceEntry;
begin
  Result := TInternalResEntry(fEntries[Idx]);
end;

function TPJResourceFile.GetEntryCount: Integer;
begin
  Result := fEntries.Count;
end;

function TPJResourceFile.IndexOfEntry(const Entry: TPJResourceEntry): Integer;
begin
  Result := fEntries.IndexOf(Entry);
end;

class function TPJResourceFile.IsValidResourceStream(
  const Stm: TStream): Boolean;
const
  // Expected bytes in the header record that introduces a 32 bit resource file
  DummyHeader: array[0..7] of Byte = ($00, $00, $00, $00, $20, $00, $00, $00);
var
  HeaderBuf: array[0..31] of Byte;  // stores introductory header
begin
  if Stm.Read(HeaderBuf, SizeOf(HeaderBuf)) = SizeOf(HeaderBuf) then
    // Check if header is equivalent to dummy header that starts resource files
    Result := CompareMem(@HeaderBuf, @DummyHeader, SizeOf(DummyHeader))
  else
    // Couldn't read header
    Result := False;
end;

procedure TPJResourceFile.LoadFromFile(const FileName: TFileName);
var
  Stm: TFileStream; // stream onto file
begin
  Stm := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stm);
  finally
    Stm.Free;
  end;
end;

procedure TPJResourceFile.LoadFromStream(const Stm: TStream);
begin
  // Clear any previous resource entries
  Clear;
  // Test for header of 32 bit resource file: exception if invalid
  if not IsValidResourceStream(Stm) then
    raise EPJResourceFile.Create(sErrBadResFile);
  // This is valid 32 bit resource file. We've passed header: read the resources
  while Stm.Position < Stm.Size do
    fEntries.Add(TInternalResEntry.Create(Self, Stm));  // increments stream pos
end;

procedure TPJResourceFile.SaveToFile(const FileName: TFileName);
var
  Stm: TFileStream; // stream onto file
begin
  Stm := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stm);
  finally
    Stm.Free;
  end;
end;

procedure TPJResourceFile.SaveToStream(const Stm: TStream);
var
  Idx: Integer; // loops thru all entries in resource
begin
  // Write header record to stream
  with TInternalResEntry.Create(
    Self, MakeIntResource(0), MakeIntResource(0), $0000
  ) do
    try
      WriteToStream(Stm);
    finally
      Free;
    end;
  // Write actual resource entries
  for Idx := 0 to Pred(EntryCount) do
    (Entries[Idx] as TInternalResEntry).WriteToStream(Stm);
end;

end.

