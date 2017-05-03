# emacs-init

This is the personal Emacs configuration of Aaron Harris.  Feel free
to take a look around!  I try as much as possible to make my
configuration modular, so odds are good that if you find something
useful here, you can make use of it as-is.

Particular things that might be of interest to the general Emacs user:

* The `umbra` module defines a minor mode for personal keybindings (so
  that you shadow, rather than overwrite, the default bindings).
  Support for mode-specific bindings is provided.

* The `deck` module defines some useful window-manipulation functions
  that view windows as containing "decks" of buffers.

* The `vizier` module defines some macros building on Emacs' advice
  facility (specifically, the newer kind of advice found in
  `nadvice`).  These let you apply advice in a precisely-targeted
  fashion without a lot of hassle.

* The `proctor` module defines macros for use as `ert` test fixtures,
  covering various scenarios common to Emacs code.  The `proctor-*`
  family defines similar macros for use with specific packages.

As you look around, you should be aware of my naming conventions.
Modules prefixed with `init-` are primarily concerned with actual
configuration (setting up keybindings and hooks, etc.); most of the
general-purpose code is elsewhere.  Modules prefixed with `aph-` tend
to contain simple extensions or tweaks to functionality found in
existing Emacs modules.  Every other module is supposed to be designed
around a particular, largely independent, element of functionality.

One final caveat: While I've put this up here in the hopes that
someone other than myself will find this useful, this is first and
foremost my own Emacs configuration.  I try to keep everything in
working order, but I do break things occasionally; I only code for my
own setup (typically running on Windows or Cygwin, in a GUI window);
and I don't make much of an effort to keep interfaces stable.  Use at
your own risk.
