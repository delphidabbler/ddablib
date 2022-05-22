# ddablib: DelphiDabbler Code Library

A library of assorted Delphi components, units and IDE extensions by [DelphiDabbler](https://gravatar.com/delphidabbler).

> ## IMPORTANT NOTE
>
> This repository is no longer in use for code development.
>
> The various sub-projects within the library have been split out into their own repositories within a new GitHub organization called [ddablib](https://github.com/ddablib). Active development has transferred to there.
>
> Once everything has been transferred out this repository will be retained as a placeholder, but will be archived. There will be several commits to `main` before the project is archived. The start of the archiving process is tagged with the [`begin-arching-process`](https://github.com/delphidabbler/ddablib/commits/begin-archiving-process) tag

## Library Contents

The library contains the following (sub-)projects, each of which has its own web page (linked):

* [About Box Component](https://delphidabbler.com/software/aboutbox) †
* [Clipboard Viewer Component](https://delphidabbler.com/software/cbview) ‡
* [Console Application Runner Classes](https://delphidabbler.com/software/consoleapp)
* [Drop Files Components](https://delphidabbler.com/software/dropfiles)
* [Environment Variable Unit](https://delphidabbler.com/software/envvars)
* [Extended String Property Editor](https://delphidabbler.com/software/stringpe)
* [Fractions Unit](https://delphidabbler.com/software/fractions)
* [Hot Label Component](https://delphidabbler.com/software/hotlabel)
* [I/O Utility Classes](https://delphidabbler.com/software/ioutils)
* [MD5 Message Digest Class](https://delphidabbler.com/software/md5)
* [Message Dialogue Box Components](https://delphidabbler.com/software/msgdlg)
* [Resource File Unit](https://delphidabbler.com/software/resfile)
* [Shell Folders Unit](https://delphidabbler.com/software/shellfolders)
* [Stream Extensions Classes](https://delphidabbler.com/software/streams)
* [System Information Unit](https://delphidabbler.com/software/sysinfo)
* [Version Information Unit](https://delphidabbler.com/software/verinfo)
* [Windows State Components](https://delphidabbler.com/software/wdwstate)

> † The About Box Component depends on the Version Information Unit, which must be installed first.
>
> ‡ Some of the Console Application Runner Classes demo programs make use of code from the I/O Utility Classes.

The linked web pages provide information about where to find the relevant source code repository, releases, issues page and documentation.

## Releases

Each sub-project is released separately.

The last pre-split release of every (sub-)project and any subsequent releases are available from the relevant GitHub repository pages over at the [ddablib](https://github.com/ddablib) organization.

Furthermore ***All*** releases to date can be found on SourceForge in the [DDabLib Files page](https://sourceforge.net/projects/ddablib/files/), where each (sub-)project has its own folder.

## Documentation

Each sub-project of the library is fully documented in the separate [delphidabbler/ddab-lib-docs](https://github.com/delphidabbler/ddab-lib-docs) GitHub project.

Some sub-projects also have an [FAQ page](https://github.com/delphidabbler/ddab-lib-docs/blob/master/FAQs/FAQs.md).

## Bugs & Feature Requests

Please report any bugs or request new features on the relevant (sub-)project repository's _Issues_ page over at the [ddablib](https://github.com/ddablib) organization.

> **DO NOT** open an issue on this repository - the _Issues_ tab has only been retained until existing issues are resolved and may then be removed.
>
> The old ticketing system on SourceForge has been removed.

## History of the library

This library goes back a long way. When it was started the code was not under version control. It was then moved to a Subversion repository on the long defunct Google Code. When Google Code closed the Subversion repo was transferred over to SourceForge.

On 2022/01/16 the library was converted to Git and moved from SourceForge to GitHub.

The library has never been released as a single entity - each component of the library has always been maintained as a separate sub-project and receives it's own releases. Therefore, on 2022/05/21, the long overdue decision was taken to split the Git repo into 17 different repositories, one for each sub-project. A new GitHub organization was set up and all the new repositories were created there.

### Conversion from Subversion

Until 2022/01/16 this library was maintained in a [Subversion repository](https://sourceforge.net/p/ddablib/code/HEAD/tree/) on SourceForge. On that date the contents of that repository's `trunk` were converted to Git. The Subversion repo's `tags` were not imported, but Git tags were added at the closest commit before each sub-project's release date.

All Subversion commits have been applied to the Git repo's `main` branch. That practise will change with the next release when `main` will begin to contain only the commits that relate to the latest release. All development work will be carried out in the `develop` branch.

The last commit that relates to code imported from the Subversion repo is [f25d049](https://github.com/delphidabbler/ddablib/commit/f25d049465a486bd6f70d73e8b061915e4b56e7c).

The Subversion repo remains in place, in read only mode, for historical reasons.

### Splitting into separate repositories

The Git `filter-branch` command was used to split the repositories. This flattened the `develop` branch into the `main` branch so that all changes since the conversion from Subversion on 2022/01/16 that had been made in the `develop` branch now appear in `main`.

Tags were preserved (although some errors were corrected). Each release tag was renamed from `<projectname>-v9.9.9` format to `version-9.9.9` format. A replacement `svn-import` tag was added to each repository at the last commit before conversion to Git. Finally, a new `ddablib-split` tag was added at the last commit made before the monolithic Git repository was divided.