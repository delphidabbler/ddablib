# ddablib: DelphiDabbler Code Library

A library of assorted Delphi components, units and IDE extensions by DelphiDabbler.

> Until 2022/01/16 this library was maintained in a Subversion repository on SourceForge. On that date the contents of that repository's `trunk` converted to Git.
>
> All Subversion commits have been applied to the Git repo's `main` branch. That practise will change with the next release when `main` will begin to contain only the commits that relate to the latest release. All development work will be carried out in the `develop` branch.
>
> The last commit that relates to code imported from the Subversion repo is [f25d049](https://github.com/delphidabbler/ddablib/commit/f25d049465a486bd6f70d73e8b061915e4b56e7c).

## Library Contents

The library contains the following sub-projects †

* About Box Component ‡
* Clipboard Viewer Component
* Console Application Runner Classes
* Drop Files Components
* Environment Variable Unit
* Extended String Property Editor
* Fractions Unit
* Hot Label Component
* I/O Utility Classes
* MD5 Message Digest Class
* Message Dialogue Box Components
* Resource File Unit
* Shell Folders Unit
* Stream Extensions Classes
* System Information Unit
* Version Information Unit
* Windows State Components

> † Other than ‡ there are no interdependencies between the sub-projects.
>
> ‡ The About Box Components depend on the Version Information Unit, which must be installed first.

The projects are summarised [here](https://github.com/delphidabbler/ddab-lib-docs/blob/master/Docs/Welcome.md).

## Releases

The whole library has never been released as a single entity - each sub-project has received its own releases, has its own changelog and has a Git tag for each release made under version control.

The practise of making separate releases for each sub-project has its origins in the days of dial-up internet, when it was important to reduce file sizes. Things may change following the move to GitHub. We'll see. All releases to date can be found on SourceForge in the [DDabLib Files page](https://sourceforge.net/projects/ddablib/files/), where each sub-project has its own folder.

## Bugs & Feature Requests

Please report any bugs or request new features using this project's [Issues tab](https://github.com/delphidabbler/ddablib/issues).

Before reporting a bug or requesting a feature please make sure you:

* Are using the latest version of the project or sub-project.
* Have read through existing issues to make sure that no-one has reported / requested the same thing already. If they have then please add a comment if you can provide more information, or to add a +1.

When opening a new issue please:

* State which sub-project you are talking about (**essential**).
* Be as clear as you can.
* Provide as much information and context as you can.
* If reporting a bug please provide some source code that reproduces the bug.

An issue heading on its own, with no further information is unlikely to get much love!

Even better, if you can provide your own fix then open a pull request for it.

> **Note:** The old ticket system on SourceForge has now been closed and is no longer monitored.

## Documentation

Each sub-project of the library is fully documented in the separate [delphidabbler/ddab-lib-docs](https://github.com/delphidabbler/ddab-lib-docs) GitHub project.
