Learn-OCaml
===========

This is Learn-OCaml, a platform for learning the OCaml language,
featuring a Web toplevel, an exercise environment, and a directory of
lessons and tutorials.

A demo is available at: [http://learn-ocaml.hackojo.org/](http://learn-ocaml.hackojo.org/).

[![CI](https://github.com/ocaml-sf/learn-ocaml/workflows/CI/badge.svg?branch=master)](https://github.com/ocaml-sf/learn-ocaml/actions?query=workflow%3ACI)
[![macOS](https://github.com/ocaml-sf/learn-ocaml/workflows/macOS/badge.svg?branch=master)](https://github.com/ocaml-sf/learn-ocaml/actions?query=workflow%3AmacOS)
[![learn-ocaml](https://img.shields.io/badge/docker-ocamlsf%2Flearn--ocaml-blue.svg)](https://hub.docker.com/r/ocamlsf/learn-ocaml "Docker image of learn-ocaml")
[![learn-ocaml-client](https://img.shields.io/badge/docker-ocamlsf%2Flearn--ocaml--client-blue.svg)](https://hub.docker.com/r/ocamlsf/learn-ocaml-client "Docker image of learn-ocaml-client")

Howtos
------

* [How to set up an environment to develop exercises?](./howto-setup-exercise-development-environment.md)
* [How to write exercises?](./howto-write-exercises.md)
* [How to submit an exercise to the global corpus?](./howto-submit-an-exercise.md)
* [How to deploy an instance of Learn OCaml?](./howto-deploy-a-learn-ocaml-instance.md)

Contacts
--------

To ask any question about how to use Learn-OCaml, subscribe to
the mailing-list [learn-ocaml-club](https://sympa.inria.fr/sympa/subscribe/learn-ocaml-club).

To discuss about the development of Learn-OCaml, subscribe to
the mailing-list [learn-ocaml-dev](https://sympa.inria.fr/sympa/subscribe/learn-ocaml-dev).

License and copyright
---------------------

Unless explicitly written below or in the files themselves, the source
code for the app, images, static files, course content and exercises
are placed under the MIT license.

Lightly modified third party components ACE and ppx_metaquot are
included, under their original licenses (respectively BSD and MIT).

The OCamlPro logo images are (c) OCamlPro. Redistribution is
permitted, alteration requires prior written authorization by
OCamlPro.

The OCaml / ocaml.org logo is released under the very liberal UNLICENSE.
See [https://github.com/ocaml/ocaml.org/blob/master/LICENSE.md](https://github.com/ocaml/ocaml.org/blob/master/LICENSE.md).

The Inconsolata font is released under the Open Font License.
See [http://www.levien.com/type/myfonts/inconsolata.html](http://www.levien.com/type/myfonts/inconsolata.html).

The Biolinum font is licensed under the GNU General Public License with
a the 'Font-Exception'.
See [http://www.linuxlibertine.org](http://www.linuxlibertine.org).

The public instance of Learn OCaml uses the Fontin font instead of
Biolinum. This font is licensed under the exljbris Font Foundry Free
Font License Agreement, which, to our understanding, does not allow us
to redistribute it. See [http://www.exljbris.com/eula.html](http://www.exljbris.com/eula.html). You will
optionally have to procure the files by yourself while building the
app. If not, the CSS provides a reasonable fallback font.

Contributions to this repository are placed under the BSD
license. This means that we can merge them with the same license as
the rest of the codebase, while you keep all the rights on your code.
And we will not have to bother you with any future license update.
See [https://opensource.org/licenses/BSD-3-Clause](https://opensource.org/licenses/BSD-3-Clause).

Authors and Acknowledgements
----------------------------

Learn-OCaml is a free software by the [OCaml Software Foundation](https://ocaml-sf.org).

 * The main authors are Benjamin Canou, Çağdaş Bozman and Grégoire Henry.

 * It builds on the previous experience of Try OCaml by Çağdaş Bozman.

 * We heavily use js_of_ocaml, so thanks to the Ocsigen team.

 * The text editing component is a customized version of ACE.

 * We also include a derivative of ppx_metaquot by Alain Frisch.
