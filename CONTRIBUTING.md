# Contribution Guide for Learn-OCaml

Thanks for considering contributing to the Learn-OCaml project!

The guidelines below summarize the main conventions involved in the
development of Learn-OCaml.

## Issues

Bug reports and feature requests are very welcome. They are tracked as
GitHub issues, using various [labels](https://github.com/ocaml-sf/learn-ocaml/labels).

First, search if a related issue already exists in the
[`ocaml-sf/learn-ocaml` bug tracker](https://github.com/ocaml-sf/learn-ocaml/issues).

Otherwise, you can open a new issue using one of the
[issue forms](https://github.com/ocaml-sf/learn-ocaml/issues/new/choose).

## Pull Requests

In the sequel, we assume you are familiar with [Git](https://git-scm.com/docs/).

We use pull requests to review bug fixes and new features.

If the underlying bug or feature request has not been reported
beforehand, it can be a good idea to open an issue first (unless it is
a minor one).

Then, you can state in this issue that you are working on a fix and/or
discuss the design of the implementation with Learn-OCaml maintainers.

Next, you may want to read the documentation regarding
[How to set up your development environment](https://ocaml-sf.org/learn-ocaml/howto-setup-exercise-development-environment).

### Branches Conventions

Pull Requests should be created from a feature branch (≠ `master`),
typically from a fork of
[`ocaml-sf/learn-ocaml`](https://github.com/ocaml-sf/learn-ocaml), and
target the `master` branch.

If you need to fix merge conflicts, we generally prefer that you
rebase your branch on
[`master`](https://github.com/ocaml-sf/learn-ocaml/tree/master),
rather than creating a merge commit.

### Atomic Commits

Borrowing some suggestions of the
[Git Style Guide](https://github.com/agis/git-style-guide#commits), ideally:

> * Each commit should be a single *logical change*.
>   Don't make several *logical changes* in one commit.
>   For example, if a patch fixes a bug and optimizes the performance of a feature, split it into two separate commits.
> 
> * Don't split a single *logical change* into several commits.
>   For example, the implementation of a feature and the corresponding tests should be in the same commit.

In particular, please avoid to reformat lines you need not touch for a
given *logical change*, in order to make code review easier.

Thus, the Learn-OCaml maintainers may suggest you adapt the commits of your PR
(using [`git rebase -i`](https://git-scm.com/docs/git-rebase#Documentation/git-rebase.txt--i) and `git push -f`)
to better comply with these suggestions as well as with the [Conventional Commits](#conventional-commits) guidelines below.

However, these conventions can be somewhat lifted if the Pull Request
is intended to be "squashed" in a single commit, as the maintainer
could refine the squashed-commit message in this case (cf. the
[Learn-OCaml maintainers wiki](https://github.com/ocaml-sf/learn-ocaml/wiki/Checklist-for-testing-and-merging-a-PR#merging-a-pr)).

### Conventional Commits

The commits messages should follow the [Conventional Commits specification](https://www.conventionalcommits.org/en/v1.0.0/).

This is necessary as Learn-OCaml relies on the
[`release-please`](https://github.com/googleapis/release-please) tool to
automatically generate the [`CHANGELOG.md`](./CHANGELOG.md) and the
[Release Notes](https://github.com/ocaml-sf/learn-ocaml/releases),
assuming [Semantic Versioning](https://semver.org/).

To sum up, each commit message contains a header, a body, and a footer, with the following structure:

<pre>
<i><b>type</b>[optional <b>scope</b>]</i><b>: <i>description</i></b>
<i>blank_line</i>
<i>[optional <b>body</b>]</i>
<i>blank_line</i>
<i>[optional <b>footers</b>]</i>
</pre>

where:

* ***`type`*** is one of the <a href="#conventional-commits-types">conventional commits types</a> (`feat`, `fix`, …); an exclamation mark after the type denotes a non-backward-compatible change (e.g., <code>refactor<b>!</b>: Use ocamlformat</code>)
* ***`scope`*** is a keyword *between parentheses* providing more context on the impacted part of the project (e.g.: API, grader, UI, dune, make, opam, docker, GHA, or just a given *filename.ml*)
* ***`description`*** is a mandatory summary (typically starting with a verb in imperative, present tense), with no period in the end; it should be short but informative, just like an e-mail subject line.
* ***`body`*** is an optional body, useful to explain *why* the change is necessary or has been implemented this way.
* several kinds of ***`footers`*** can be provided:
  * References that auto-close issues → `Fix #1` or `Close #1` [(online doc)](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword)
  * References to commits or issues → `Related: #1` or mere URLs [(online doc)](https://docs.github.com/en/github/writing-on-github/working-with-advanced-formatting/autolinked-references-and-urls#issues-and-pull-requests)
  * A Conventional Commits footer → <code>BREAKING CHANGE: <i><b>description</b></i></code>  
  (which plays the same role as the <code><b><i>type</i>!</b></code> suffix mentioned above)
  * A co-authorship footer → `Co-authored-by: First Last <address@example.com>` [(online doc)](https://github.blog/2018-01-29-commit-together-with-co-authors/#how-it-works)
  * Or any footer following the usual [Git trailers convention](https://git-scm.com/docs/git-interpret-trailers).

### Conventional Commits Examples

Here are two example commit messages:

* [`7e389ef`](https://github.com/ocaml-sf/learn-ocaml/commit/7e389ef22842e95e1b3f4364c19cf657a53ebf01) (this commit was obtained after squash-merging [PR #434](https://github.com/ocaml-sf/learn-ocaml/pull/434))
  ```
  feat(release.yml): Add a (3 jobs)-based GHA using release-please
  
  * Use var `OPAM_RELEASE` (GitHub PAC)
  
  * Use `expect` to workaround the fact that the feature wish
    https://github.com/ocaml-opam/opam-publish/issues/132
    is not yet available.
  
  Co-Authored-By: Yann Régis-Gianas <yann@regis-gianas.org>
  Co-Authored-By: Erik Martin-Dorel <erik@martin-dorel.org>
  ```
* [`35941b5`](https://github.com/ocaml-sf/learn-ocaml/commit/35941b5ebe8cb2b947cd6010118050a79c6e36f8), reworded (this commit was part of [PR #448](https://github.com/ocaml-sf/learn-ocaml/pull/448))
  ```
  fix(grader): Display negative numbers with mandatory parens
  
  * Thanks @letouzey for reporting this issue and suggesting a fix
  
  * Update some learn-ocaml-client tests accordingly
  
  Fix #440
  ```

### Conventional Commits Types

As specified in commit [`87bb9b5`](https://github.com/ocaml-sf/learn-ocaml/commit/87bb9b5e838badf872b8d08228e6768ce45710b5), the table below summarizes the commit types (lowercase prefixes before a colon) that are recognized by [the `release-please` GitHub Action](https://github.com/ocaml-sf/learn-ocaml/blob/master/.github/workflows/release.yml):

| commit type | `CHANGELOG` section title | Comments |
|-------------|---------------------------|----------|
| `feat`      | Features                  | Add a new feature (use `feat!` if it is non-backward compatible) |
| `fix`       | Bug Fixes                 | Patch a bug |
| `revert`    | Reverts                   | Revert commit (include that commit header, SHA1, and motivation) |
| `perf`      | Performance Improvements  | Change code to improve performance |
| `refactor`  | Code Refactoring          | Change code without adding a feature nor fixing a bug |
| `deps`      | Dependencies              | Change external dependencies (e.g., for scopes opam, docker) |
| `build`     | Build System              | Change the build system (e.g., for scopes dune, make, docker) |
| `test`      | Tests                     | Add missing tests or correct existing tests |
| `ci`        | CI/CD                     | Change the CI/CD configuration |
| `docs`      | Documentation             | Change documentation only |
| `style`     | Style                     | Change code without affecting its meaning (white-space, formatting, semi-colons or so) |
| `chore`     | Miscellaneous Chores      | (hidden by default) Change files unrelated to code, tests, docs, build or ci config |

See also:

* [Conventional Commits v1.0.0 Summary](https://www.conventionalcommits.org/en/v1.0.0/#summary)
* [Angular's Commit Message Format](https://github.com/angular/angular/blob/master/CONTRIBUTING.md#commit)
* [Semantic Versioning](https://semver.org/)

## Documentation and tests

All contributed code and comments should be written in English.

If you change `.mli` interface files, make sure you also update the
[`(** ocamldoc comments *)`](https://ocaml.org/manual/ocamldoc.html#s:ocamldoc-comments)
accordingly (see also [this page](https://ocaml.org/manual/doccomments.html#s:doc-comments)).

If your contribution contains user-facing changes, it can be a good
idea to also update the Learn-OCaml documentation stored in the
[docs/](https://github.com/ocaml-sf/learn-ocaml/tree/master/docs)
directory.

If your contribution (whether a fix or an enhancement) impacts the
grading feature, make sure you also update the test suite stored in the
[tests/](https://github.com/ocaml-sf/learn-ocaml/tree/master/tests#readme)
directory.

## Licensing

Contributions to this repository are placed under the [MIT](https://spdx.org/licenses/MIT.html) license.
