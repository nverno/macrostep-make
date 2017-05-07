*Author:* Noah Peart <noah.v.peart@gmail.com><br>
*URL:* [https://github.com/nverno/make-tools](https://github.com/nverno/make-tools)<br>

Description:

Expands make variables using macrostep-expand interface.

- creates an association list of macro -> value
- currently is able to parse included makefiles and add their macro
  definitions so long as there aren't any make functions involved, like 'shell'
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
