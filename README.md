# ddablib: DelphiDabbler Code Library

A library of assorted Delphi components, units and IDE extensions by [DelphiDabbler](https://gravatar.com/delphidabbler).

> ## IMPORTANT NOTE
>
> This repository is no longer in use for code development.
>
> The various sub-projects within the library have been split out into their own repositories within a new GitHub organization called [ddablib](https://github.com/ddablib). Active development has transferred to there.
>
> Once everything has been transferred out this repository will be retained as a placeholder, but will be archived. There will be several commits to `main` before the project is archived. The start of the archiving process is tagged with the [`begin-archiving-process`](https://github.com/delphidabbler/ddablib/commits/begin-archiving-process) tag.

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

Furthermore ***all*** †† releases can be found on SourceForge in the [DDabLib Files page](https://sourceforge.net/projects/ddablib/files/), where each (sub-)project has its own folder.

> †† Well, all releases made under version control are available - see [History of the library](#history-of-the-library) below for details.

## Documentation

Each sub-project of the library is fully documented in the separate [delphidabbler/ddab-lib-docs](https://github.com/delphidabbler/ddab-lib-docs) GitHub project.

Some sub-projects also have an [FAQ page](https://github.com/delphidabbler/ddab-lib-docs/blob/master/FAQs/FAQs.md).

> This documentation _may_ also move into a new [ddablib](https://github.com/ddablib) project in due course.

## Bugs & Feature Requests

Please report any bugs or request new features on the relevant (sub-)project repository's _Issues_ page over at the [ddablib](https://github.com/ddablib) organization.

> **DO NOT** open an issue on this repository - the _Issues_ tab has only been retained until existing issues are resolved and may then be removed.
>
> The old ticketing system on SourceForge has been removed.

## History of the library

### Pre-history

This library goes back a long way. When it was started the code was not under version control. However, reasonably complete records of changes and releases were kept, either change logs in source files or in separate documents. A summary of all the documented changes still exists in a file named `PreSVNHistory.txt` in each sub-project's `Docs` directory.

At this time each (what was to become a) sub-project was separately maintained in its own directory and received its own releases.

### The monolithic library

In 2009 the decision was taken to combine all the different source code library projects into a single monolithic library project. The intention was to start to release the whole library instead of making individual releases of each sub-project. This never actually happened!

#### Subversion

A new Subversion repository was created. Gradually the sub-project's were imported, starting with the _System Information Unit_ on 2009-07-04 and ending with the _Clipboard Viewer Component_ on 2010-10-13. Each sub-project had its own sub-directory in `trunk/projects`. There was also a `trunk/common` directory for tools and anything else that could be shared amongst all sub-projects.

Originally the repository was maintained locally but was eventually moved to GoogleCode as the`ddab-lib` project. When Google announced the forthcoming closure of GoogleCode the repository was moved to SourceForge, now named [DDabLib](https://sourceforge.net/p/ddablib/code/HEAD/tree/). Dates when these moves took place are not known.

The repository remained on SourceForge until 2022.

#### Git

By 2022 the library was the last of my projects still maintained in Subversion, everything else was using Git. There had been one or two abortive attempts already to convert the repo to Git, but finally, on 2022-01-16, the move was made.

Only the contents of the Subversion repository's `trunk` were exported. The `tags` were not exported but suitable tags were added at the closest commit before each sub-project's release date. There were no Subversion branches to export.

> Because there was _some_ loss of information in the conversion, the [SourceForge repository](https://sourceforge.net/p/ddablib/code/HEAD/tree/) has been retained for archive purposes.

All Subversion commits have been applied to the Git repo's `main` branch. 
The last commit that relates to code imported from the Subversion repo hss been tagged as [`svn-import`](https://github.com/delphidabbler/ddablib/commits/svn-import).

After the conversion was complete the library was uploaded to GitHub as the [delphidabbler/ddablib](https://github.com/delphidabbler/ddablib) project.

New development on the Git repository was carried out on the `develop` branch, with `main` being updated only after each release.

### The monolith shatters

As has been mentioned, the intention to release the library as a whole was never realised. Instead, individual sub-projects continued to be released separately.

Keeping all the sub-projects in one repo meant that GitHub's release system couldn't be used properly and releases were still having to be uploaded to SourceForge. It was clear that it would be more logical to split the library into its component parts.

Therefore, on 2022-05-21, the long overdue decision was taken to split the Git repo into 17 different repositories, one for each sub-project. A new [GitHub organization](https://github.com/ddablib) was set up and all the new repositories were created there. 

The Git `filter-branch` command was used to split the repositories. This flattened the `develop` branch into the `main` branch so that all changes since the conversion from Subversion that had been made in the `develop` branch now appeared in `main`.

Tags were preserved (although some errors were corrected). Each release tag was renamed from `<projectname>-v9.9.9` format to `version-9.9.9` format. A replacement `svn-import` tag was added to each repository at the last commit before conversion to Git. Finally, a new `ddablib-split` tag was added at the last commit made before the monolithic Git repository was divided.

Each library project's latest release had a GitHub release created for it. Earlier releases remain available on [SourceForge](https://sourceforge.net/projects/ddablib/files/).

Following the split, commits to [delphidabbler/ddablib](https://github.com/delphidabbler/ddablib) are once again being made directly to `main`. Such commits are only for the purpose of archiving this repository. The [`begin-archiving-process`](https://github.com/delphidabbler/ddablib/commits/begin-archiving-process) tag marks the start of this process.

The [new repositories](https://github.com/ddablib) are being developed using the GitFlow methodology.

