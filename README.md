[![Build Status](https://travis-ci.org/Chris00/weberizer.svg?branch=master)](https://travis-ci.org/Chris00/weberizer)
[![Build status](https://ci.appveyor.com/api/projects/status/6danpd8mm0rh7qet?svg=true)](https://ci.appveyor.com/project/Chris00/weberizer)

Weberizer
=========

Weberizer is a simple templating engine for OCaml.  It compiles the
template to an OCaml module, providing an easy way to set the
variables and render the template.  String values are automatically
escaped according to the context of the template in which they appear.
You can add you own functions to the generated module — for example to
set several related variables at once (you can also hide those
variables from the interface if desired).

This approach will enable to easily add some security features if
desired — like forcing several variables to be set before the template
can be rendered.

Installation
------------

The simplest way to install this library and program is to use
[opam](https://opam.ocaml.org/):

    opam install weberizer

The program to transform HTML files is called `weberizer`.  Issue
`weberizer templ.html` to generate `templ.ml` and `templ.mli`.  The
templating “variables” are described at the beginnin of
[weberizer.mli](lib/weberizer.mli).  See also the [example](demo/).

Licence
-------

This library is released under the LGPL-3.0 with the OCaml linking
exception.
