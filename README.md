*Author:* Noah Peart <noah.v.peart@gmail.com><br>
*URL:* [https://github.com/nverno/make-tools](https://github.com/nverno/make-tools)<br>

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

PROBLEMS:
- when expanding variable with blank definition, it isn't replaced
  with original variable

TODO:
- Deal with '+='
- Either do variable substitution when storing values or do multi-level
  macroexpansion, like elisp.


---
Converted from `macrostep-make.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
