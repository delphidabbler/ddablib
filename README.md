# :no_entry: ddablib: DelphiDabbler Code Library

This is the former home of a library of assorted Delphi components, units and IDE extensions by [DelphiDabbler](https://gravatar.com/delphidabbler).

Active development of the library now takes place in a group of repositories in the [ddablib](https://github.com/ddablib) GitHub organisation.

 :warning: **WARNING:** This repository is now archived. The code is frozen and out of date.

## Library Contents

At the time the library was transferred to [ddablib](https://github.com/ddablib), it contained the projects listed in the table below. Links are provided to each project's web page and to its [ddablib](https://github.com/ddablib) GitHub repository.

| Project Web Page | GitHub Repository |
|------------------|-------------------|
| [About Box Component](https://delphidabbler.com/software/aboutbox) † | [ddablib/about](https://github.com/ddablib/aboutbox) |
| [Clipboard Viewer Component](https://delphidabbler.com/software/cbview) ‡ | [ddablib/cbview](https://github.com/ddablib/cbview) |
| [Console Application Runner Classes](https://delphidabbler.com/software/consoleapp) | [ddablib/consoleapp](https://github.com/ddablib/consoleapp) |
| [Drop Files Components](https://delphidabbler.com/software/dropfiles) | [ddablib/dropfiles](https://github.com/ddablib/dropfiles) |
| [Environment Variables Unit](https://delphidabbler.com/software/envvars) | [ddablib/envvars](https://github.com/ddablib/envvars) |
| [Extended String Property Editor](https://delphidabbler.com/software/stringpe) | [ddablib/stringpe](https://github.com/ddablib/stringpe) |
| [Fractions Unit](https://delphidabbler.com/software/fractions) | [ddablib/factions](https://github.com/ddablib/fractions) |
| [Hot Label Component](https://delphidabbler.com/software/hotlabel) | [ddablib/hotlabel](https://github.com/ddablib/hotlabel) |
| [I/O Utility Classes](https://delphidabbler.com/software/ioutils) | [ddablib/ioutils](https://github.com/ddablib/ioutils) |
| [MD5 Message Digest Unit](https://delphidabbler.com/software/md5) | [ddablib/md5](https://github.com/ddablib/md5) |
| [Message Dialogue Components](https://delphidabbler.com/software/msgdlg) | [ddablib/msgdlg](https://github.com/ddablib/msgdlg) |
| [Resource File Unit](https://delphidabbler.com/software/resfile) | [ddablib/resfile](https://github.com/ddablib/resfile) |
| [Shell Folders Unit](https://delphidabbler.com/software/shellfolders) | [ddablib/shellfolders](https://github.com/ddablib/shellfolders) |
| [Stream Extensions Classes](https://delphidabbler.com/software/streams) | [ddablib/streams](https://github.com/ddablib/streams) |
| [System Information Unit](https://delphidabbler.com/software/sysinfo) | [ddablib/sysinfo](https://github.com/ddablib/sysinfo) |
| [Version Information Component](https://delphidabbler.com/software/verinfo) | [ddablib/verinfo](https://github.com/ddablib/verinfo) |
| [Windows State Components](https://delphidabbler.com/software/wdwstate) | [ddablib/wdwstate](https://github.com/ddablib/wdwstate) |

> † The About Box Component depends on the Version Information Unit, which must be installed first.
>
> ‡ Some of the Console Application Runner Classes demo programs make use of code from I/O Utility Classes.

## Releases

The latest release of each project is available from the relevant [ddablib](https://github.com/ddablib) project page.

Many more releases can be found archived on SourceForge in the [DDabLib Files page](https://sourceforge.net/projects/ddablib/files/), where each project has its own folder.

## Documentation

Each sub-project of the library is fully documented. You can access the latest version of the documentation via: <https://delphidabbler.com/url/ddablib-docs> (:arrow_right: *redirect*).

Some sub-projects also have an FAQ page that can be accessed via <https://delphidabbler.com/url/ddablib-faqs> (:arrow_right: *redirect*).

## Bugs & Feature Requests

Please report any bugs or request new features on the relevant [ddablib](https://github.com/ddablib) repository's _Issues_ page.

## History of the library

### Pre-history

This library goes back a long way. When it was first created the code was not under version control. However, reasonably complete records of changes and releases were kept, either as change logs in source files or in separate documents. A file named `PreSVNHistory.txt`, containing a summary of the project's history to date, was added to each library project.

At this time each library project was separately maintained in its own directory and received its own releases.

### A monolith is created

In 2009 the decision was taken to combine all the library projects into a single, monolithic, library. The intention was to release the whole library instead of making separate releases of each individual project.

This never actually happened!

#### Subversion

A new Subversion repository was created to maintain the whole library. Gradually, the existing  library projects were imported as sub-projects, starting with the _System Information Unit_ on 2009-07-04 and ending with the _Clipboard Viewer Component_ on 2010-10-13. (Full details of import dates can be found in [`/common/Docs/svn-initial-import-dates.md`](https://github.com/delphidabbler/ddablib/blob/main/common/Docs/svn-initial-import-dates.md)). Each sub-project had its own sub-directory in `trunk/projects`. There was also a `trunk/common` directory for tools and anything else that could be shared amongst all sub-projects.

Originally the repository was maintained locally but eventually it was moved to GoogleCode as the `ddab-lib` project. When the closure of GoogleCode was announced the repository was moved to SourceForge, now named [DDabLib](https://sourceforge.net/p/ddablib/code/HEAD/tree/). Dates when this move took place are not known.

The repository remained on SourceForge until 2022. During that time several new sub-projects were begun. These sub-projects do not have the aforementioned `PreSVNHistory.txt` file.

#### Git

By 2022 the library was the last of my projects still being maintained in Subversion: everything else was using Git. There had already been one or two abortive attempts to convert the repo to Git, but finally, on 2022-01-16, the changeover was made.

Only the contents of the Subversion repository's `trunk` were exported. While the Subversion release `tags` were not exported, equivalent Git tags were added at the last commit before each sub-project's release date. These tags had the form `<projectname>-v9.9.9`. There were no Subversion branches.

> Because there was _some_ loss of information in the conversion, the [SourceForge repository](https://sourceforge.net/p/ddablib/code/HEAD/tree/) has been retained for archive purposes.

All Subversion commits were applied to the Git repo's `main` branch. The last commit relating to code imported from the Subversion repo was tagged as [`svn-import`](https://github.com/delphidabbler/ddablib/commits/svn-import).

After the conversion was complete the library was uploaded to GitHub as the [delphidabbler/ddablib](https://github.com/delphidabbler/ddablib) repository.

New development on the Git repository was carried out on the `develop` branch, with `main` being updated and tagged only at each sub-project release.

### The monolith shatters

As has been mentioned, the intention to release the library as a whole was never realised. Instead, individual sub-projects continued to be released separately.

Keeping all the sub-projects in one repo meant that GitHub's tag-based release system couldn't be used effectively. Consequently, releases continued to be uploaded to SourceForge. It was clear that it would be more logical to split the library back into its constituent projects.

Therefore, on 2022-05-21, the long overdue decision was taken to split the monolithic Git repository into 17 different repositories, one for each sub-project. A new [GitHub organization](https://github.com/ddablib) was set up and all the new repositories were uploaded there. 

The Git `filter-branch` command was used to extract the individual library project repositories from the monolith. This flattened the `develop` branch into the `main` branch so that all changes that had been made in `develop` now appeared in `main`.

Tags were preserved, although some errors were corrected. Each release tag was renamed from `<projectname>-v9.9.9` format to `version-9.9.9` format. A replacement `svn-import` tag was added to each repository at the last commit before conversion to Git. Finally, a new `ddablib-split` tag was added at the last commit made before the monolithic repository was divided.

A release was created on GitHub for the latest version of each project at the time of the split. Subsequent releases were finally being made available on GitHub as well as SourceForge. Earlier releases remain available on [SourceForge](https://sourceforge.net/projects/ddablib/files/).

Following the split, development on [delphidabbler/ddablib](https://github.com/delphidabbler/ddablib) ceased. Further commits _were_ made to `main`, but these were only for the purpose of archiving the repository. The [`begin-archiving-process`](https://github.com/delphidabbler/ddablib/commits/begin-archiving-process) tag marks the start of this process.

The [new repositories](https://github.com/ddablib) adopted the [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/) development methodology.
