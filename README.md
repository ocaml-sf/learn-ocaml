Learn-OCaml Editor
==================

[![Build Status](https://travis-ci.com/pfitaxel/learn-ocaml-editor.svg?branch=develop)](https://travis-ci.com/pfitaxel/learn-ocaml-editor)
[![HitCount](http://hits.dwyl.io/pfitaxel/learn-ocaml-editor.svg)](http://hits.dwyl.io/pfitaxel/learn-ocaml-editor)
[![GitHub license](https://img.shields.io/github/license/pfitaxel/learn-ocaml-editor.svg)](https://github.com/pfitaxel/learn-ocaml-editor/blob/master/LICENSE)
[![GitHub tag](https://img.shields.io/github/tag/pfitaxel/learn-ocaml-editor.svg)](https://GitHub.com/pfitaxel/learn-ocaml-editor/tags/)

This is Learn-OCaml Editor, an online editor for teachers using the
[Learn-OCaml](https://github.com/ocaml-sf/learn-ocaml) Web application
for learning the OCaml language.

This software is still under development, no public announcement has been made yet.

A demo is available at: <https://pfitaxel.github.io/pfitaxel-demo/>

Installing the dependencies
---------------------------

Clone this Git repo,
install [opam 2](http://opam.ocaml.org/doc/Install.html), then run:

```bash
cd …/learn-ocaml-editor
opam switch create . --deps-only --locked
eval $(opam env)
opam install opam-installer merlin
```

Building learn-ocaml-editor
---------------------------

To build and locally run learn-ocaml-editor, you should just have to type:

```bash
make
make opaminstall
learn-ocaml build --repo=./demo-repository
learn-ocaml serve -p 8080
```

As learn-ocaml-editor is only a client-side (JavaScript) application,
you can also replace the last command above `learn-ocaml serve …` with

```bash
( cd www ; python3 -m http.server 8080 )
```

Howtos
------

* [How to setup an environment to develop exercises?](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-setup-exercise-development-environment.md)
* [How to write exercises?](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-write-exercises.md)
* [How to submit an exercise to the global corpus?](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-submit-an-exercise.md)
* [How to deploy an instance of Learn OCaml?](https://github.com/ocaml-sf/learn-ocaml/blob/master/docs/howto-deploy-a-learn-ocaml-instance.md)

License and copyright
---------------------

Unless explicitly written below or in the files themselves, the source
code for the app, images, static files, course content and exercises
are placed under the GNU Affero General Public License version 3. This
practically means that any instance of the app must provide its source
code to its users.  See <https://www.gnu.org/licenses/agpl-3.0.html>.

Lightly modified third party components ACE and ppx_metaquot are
included, under their original licenses (respectively BSD and MIT).

The OCamlPro logo images are (c) OCamPro. Redistribution is permitted,
alteration requires prior written authorization by OCamlPro.

The OCaml / ocaml.org logo is released under the very liberal UNLICENSE.
See <https://github.com/ocaml/ocaml.org/blob/master/LICENSE.md>.

The Inconsolata font is released under the Open Font License.
See <http://www.levien.com/type/myfonts/inconsolata.html>.

The Biolinum font is licensed under the GNU General Public License with
a the 'Font-Exception'.
See <http://www.linuxlibertine.org>.

The public instance of Learn OCaml uses the Fontin font instead of
Biolinum. This font is licensed under the exljbris Font Foundry Free
Font License Agreement, which, to our understanding, does not allow us
to redistribute it. See <http://www.exljbris.com/eula.html>. You will
optionally have to procure the files by yourself while building the
app. If not, the CSS provides a reasonable fallback font.

Contributions to this repository are placed under the BSD
license. This means that we can merge them with the same license as
the rest of the codebase, while you keep all the rights on your code.
And we will not have to bother you with any future license update.
See <https://opensource.org/licenses/BSD-3-Clause>.

Authors and Acknowledgements
----------------------------

Learn OCaml is a software by OCamlPro.

 * The main authors are Benjamin Canou, Çağdaş Bozman and Grégoire Henry.

 * It builds on the previous experience of Try OCaml by Çağdaş Bozman.

 * We heavily use js_of_ocaml, so thanks to the Ocsigen team.

 * The text editing component is a customized version of ACE.

 * We also include a derivative of ppx_metaquot by Alain Frisch.
