Open in Git X
=============
=============


PREREQUISITES
-------------
* You must have a recent version of GitX installed to either
  /Applications/GitX.app or ~/Applications/GitX.app

I would recommend brotherbard's fork of GitX as it is stable and more
feature rich:

https://github.com/brotherbard/gitx/downloads

* You must open GitX manually (by double-clicking the application) at
  least once, to overcome the "This application was downloaded from
  the internet..." dialog.



INSTALLATION
============

1) Place the "Open in Git X" application in "/Applications/Finder
Droplets" (or something similar... you'll have to create the folder)

2) Once moved, drag the icon to your finder toolbar (with the toolbar
expanded). It should stick up there.

3) Profit!



TROUBLESHOOTING
===============

GitX bounces in my dock forever but does not open
-------------------------------------------------
It's probably because MacOSX wants to prompt you if it's OK to open an
app downloaded from the web, but the prompt is unseen due to the
method we are launching GitX. Follow the prerequisite step by opening
GitX manually once.


CHANGES
================

1.2 (2010-02-06) - Tell screen wrapper to ignore ~/.screenrc, since a .screenrc configuration file can the behavior of screen such that the a command provided from the command-line isn't run.

1.1 (2010-12-09) - Fix