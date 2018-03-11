*Author:* Noah Peart <noah.v.peart@gmail.com><br>
*URL:* [https://github.com/nverno/macrostep-make](https://github.com/nverno/macrostep-make)<br>

Description:

Expands make variables using macrostep-expand interface.

- creates an association list of macro -> value
- currently is able to parse included makefiles and add their macro
  definitions. If `macrostep-make-use-shell` is non-nil, then included
  makefiles that require the $(shell) command can be found by calling
  `shell-file-name`.
- macrostep interface:
	+ macrostep-sexp-bounds-function
	+ macrostep-sexp-at-point-function
	+ macrostep-environment-at-point-function
	+ macrostep-expand-1-function
	+ macrostep-print-function

Hacks:
- variables are updated when ':' and '=' pressed -- these actions that are
  defined in `make-mode` to keep track of macros/targets. Therefore, it is
  quite possible that variable names can change without tables being updated.

  **To sync variables with their definitions**, call
  `makefile-pickup-everthing` (defined in `make-mode`) to update all targets
  and macros defined in the file. This won't update variables from included
  files however.

TODO:
- Deal with '+=', also possibly '?=' by adding new cell to table
  to determine what to do when mergin tables
- support 'ifeq', 'ifneq'
- Either do variable substitution when storing values or do multi-level
  macroexpansion, like elisp would be pretty sweet.
- deal with builtin defaults somehow?? eg. $(RM) => 'rm -r'
- option to expand external commands -- $(shell ...)
- or show results of commands like patsubst, dir, etc.

Code:


---
Converted from `macrostep-make.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
