# Changelog

## [0.16.0](https://github.com/ocaml-sf/learn-ocaml/compare/v0.15.0...v0.16.0) (2023-11-03)


### Features

* **UI:** Add possibility to choose exercise display order ([25780ba](https://github.com/ocaml-sf/learn-ocaml/commit/25780ba2ff2bbe50d7ad74d9ac6fb3097759ed03))
* **UI:** Rework of the exercise index ([91f827b](https://github.com/ocaml-sf/learn-ocaml/commit/91f827b3b78b4466093da781d627db3979d11943))
* **UI:** Add exercise sorting by focus skill ([4f9766b](https://github.com/ocaml-sf/learn-ocaml/commit/4f9766ba0db73eacaef8f02b9562cd01a0a37e27))


### Bug Fixes

* **translations:** Add missing fr.po.header ([f7ffc6f](https://github.com/ocaml-sf/learn-ocaml/commit/f7ffc6fd14c7a4618aba940cdb0003d24150083d)), closes [#555](https://github.com/ocaml-sf/learn-ocaml/issues/555)
* **teacher_tab:** Display (Open/Close)GloballyInconsistent exos and fix them ([10c9fc3](https://github.com/ocaml-sf/learn-ocaml/commit/10c9fc30e391036b0847e7d4ff2bc88e2be25e55))
* **teacher_tab:** partly fix Open/Close handling w.r.t. Assignments ([6c41457](https://github.com/ocaml-sf/learn-ocaml/commit/6c414578a70c5529387f7cb3266b9ea1e85cc97d)), closes [#534](https://github.com/ocaml-sf/learn-ocaml/issues/534) [#558](https://github.com/ocaml-sf/learn-ocaml/issues/558)
* **teacher_tab:** Fix Open/Closed handling for update_exercise_assignments ([49d82e4](https://github.com/ocaml-sf/learn-ocaml/commit/49d82e49c900000c9e04da845f1f47579f974332)), closes [#558](https://github.com/ocaml-sf/learn-ocaml/issues/558)


### Code Refactoring

* **translations:** gettext: Use CLI option `--no-wrap` ([ea4f2bc](https://github.com/ocaml-sf/learn-ocaml/commit/ea4f2bc3538ba1efaca3e388ab7fc5e65821081b))
* **teacher_tab:** Move critical code apart in update_exercise_assignments ([bf6a931](https://github.com/ocaml-sf/learn-ocaml/commit/bf6a931f122d41e8d6afb0a59ecc4a96a30d9b1a))


### Dependencies

* **opam:** learn-ocaml 0.x does not build with asak 0.4 ([#570](https://github.com/ocaml-sf/learn-ocaml/issues/570)) ([9176975](https://github.com/ocaml-sf/learn-ocaml/commit/9176975ab1df493ab0cecab8711223e1a692ab76))


### Tests

* **Learnocaml_data:** Add support for ppx_expect & ppx_inline_test ([3a0ceb4](https://github.com/ocaml-sf/learn-ocaml/commit/3a0ceb469d9f60979d15a889454fd2965c7fa72f))
* **Learnocaml_data:** Add ppx_expect tests for update_exercise_assignments ([c18da2a](https://github.com/ocaml-sf/learn-ocaml/commit/c18da2a89a88ebfdbed512f4dc813c63c0648d73))
* **Learnocaml_data:** Refactor ppx_expect tests to display more details in the trace ([569d536](https://github.com/ocaml-sf/learn-ocaml/commit/569d536d8ed5889bafa8bd88fa8d21b89f60810e))


### CI/CD

* ***.yml:** Move opam-publish in a separate workflow to enable testing ([#571](https://github.com/ocaml-sf/learn-ocaml/issues/571)) ([b84132e](https://github.com/ocaml-sf/learn-ocaml/commit/b84132ee7328fdf132743a17722c5e26b391b2e7))


### Documentation

* **opam:** Cite Louis Gesbert in the Learn-OCaml maintainers team ([c9a833b](https://github.com/ocaml-sf/learn-ocaml/commit/c9a833be624b8bda7d2f4a310ccf832fc10cae7f))

## [0.15.0](https://github.com/ocaml-sf/learn-ocaml/compare/v0.14.1...v0.15.0) (2023-08-23)


### Features

* **partition-view:** Add a selector to show (tokens, nicks, or anon IDs) ([#540](https://github.com/ocaml-sf/learn-ocaml/issues/540)) ([58b3644](https://github.com/ocaml-sf/learn-ocaml/commit/58b3644a6d5a3f43cf8d7cb21ebbe40b588f176f)), closes [#528](https://github.com/ocaml-sf/learn-ocaml/issues/528)
* **teacher-tab:** Add some inline documentation to the teacher tab ([651456a](https://github.com/ocaml-sf/learn-ocaml/commit/651456a159963e979c1258ab698903aeb220599a))
* **teacher-tab:** Allow name input on teacher token creation ([c341fca](https://github.com/ocaml-sf/learn-ocaml/commit/c341fcaf6d3da48edceaf37131869d9137277464))
* **teacher-tab:** Allow partial CSV export ([d5d82fc](https://github.com/ocaml-sf/learn-ocaml/commit/d5d82fccb497385e2da1798a158f7581d12d64fc))
* **teacher-tab**: Display last synced student's draft (≠ graded code) ([#548](https://github.com/ocaml-sf/learn-ocaml/issues/548)) ([48583ba](https://github.com/ocaml-sf/learn-ocaml/commit/48583ba4d1376ad006227d7b5ee1b9cfaf9ad8f7)), closes [#527](https://github.com/ocaml-sf/learn-ocaml/issues/527)
* **teacher-tab:** Highlight the "apply" button on unsaved changes ([b20ce4e](https://github.com/ocaml-sf/learn-ocaml/commit/b20ce4ee9955cd5854e3a6ee97f509770452d966))
* **js_utils:** Add HTMLElement.title support ([3a7c42e](https://github.com/ocaml-sf/learn-ocaml/commit/3a7c42e41ff2636a8022ac82906c4fda499f956f))
* **ui:** Add feedback button with internationalized tooltip ([6b7b226](https://github.com/ocaml-sf/learn-ocaml/commit/6b7b226cc7ca7ad80e3301b15f62028790b3d147)), closes [#525](https://github.com/ocaml-sf/learn-ocaml/issues/525)
* **ui:** better string input dialog ([03669eb](https://github.com/ocaml-sf/learn-ocaml/commit/03669eb2980fbb42575707e91e50c5be96e4c2dc))


### Bug Fixes

* **i18n:** fix escaping issue in i18n ([e060517](https://github.com/ocaml-sf/learn-ocaml/commit/e060517daecc1f3c5a2fc58223f1d318bd40dd54))
* **partition-view:** Use Ctrl-click or ⌘-click (instead of middle-click) ([#516](https://github.com/ocaml-sf/learn-ocaml/issues/516)) ([a6e4c5e](https://github.com/ocaml-sf/learn-ocaml/commit/a6e4c5e61362f569d6279d5d990705d3ac1d4f94)), closes [#500](https://github.com/ocaml-sf/learn-ocaml/issues/500)
* **teacher-tab:** show different status for open and closed assigned exercises ([8c2c639](https://github.com/ocaml-sf/learn-ocaml/commit/8c2c639490cd4986c1d45a5abe30de3cedfc00c7))
* **ui:** Update fr translation ([1b88a18](https://github.com/ocaml-sf/learn-ocaml/commit/1b88a18d41188e747b166af3e9705a5b91d49d46))
* **ui:** Use proper URL fragments (`"#tab=…"`, not `"#tab%3D…"`) ([#557](https://github.com/ocaml-sf/learn-ocaml/issues/557)) ([7f2b6cf](https://github.com/ocaml-sf/learn-ocaml/commit/7f2b6cfc5e1c7ce25c074cef281fd6b33ebae1ca)), closes [#539](https://github.com/ocaml-sf/learn-ocaml/issues/539)
* **html:** Fix `process_html_file` w.r.t. `base_url` ([84bc393](https://github.com/ocaml-sf/learn-ocaml/commit/84bc393e8cc6e1f9ec3a5f25822d565333ed7d57))
* **vuln:** percent-decode before path-splitting ([#560](https://github.com/ocaml-sf/learn-ocaml/issues/560)) ([1a0c2ef](https://github.com/ocaml-sf/learn-ocaml/commit/1a0c2efaedf6f7eab5bdf10a2347276827d7c06a))


### Code Refactoring

* **partition-view:** Move adhoc CSS code to `learnocaml_partition_view.css` ([8d86f01](https://github.com/ocaml-sf/learn-ocaml/commit/8d86f018ede1666983cdc36fcab5e7c93d434be4))


### CI/CD

* ***.yml:** Use actions/checkout@v3 ([6cb8165](https://github.com/ocaml-sf/learn-ocaml/commit/6cb8165e871ce182851302ab7ccc8098dcbbb394))
* **docker:** Fix build-args syntax (docker/build-push-action@v4) ([046087d](https://github.com/ocaml-sf/learn-ocaml/commit/046087d596c35a8918e7d6a51b54c6c64b5a2397))
* **docker:** Fix GHA input name: s/build_args/build-args/ ([a31ff2b](https://github.com/ocaml-sf/learn-ocaml/commit/a31ff2b1f33499e4b9ee14a75f00e21f5307fa87))
* **docker:** Replace `LABEL` Dockerfile commands with `labels:` (GHA) ([#551](https://github.com/ocaml-sf/learn-ocaml/issues/551)) ([ce8c006](https://github.com/ocaml-sf/learn-ocaml/commit/ce8c006a9c178dcd815903ac0c20fe67c9d91cf1)), closes [#545](https://github.com/ocaml-sf/learn-ocaml/issues/545)
* **docker:** Use docker/build-push-action@v4 ([#544](https://github.com/ocaml-sf/learn-ocaml/issues/544)) ([69006f8](https://github.com/ocaml-sf/learn-ocaml/commit/69006f818dce579511d8da0a4beb456dbfe83fc8))
* **macos:** Fix `brew upgrade` failure ([a99c61b](https://github.com/ocaml-sf/learn-ocaml/commit/a99c61b5e43f57c2b84ba75778b286daa7d9b566))
* **macos:** Fix `brew upgrade` failure, bis ([#518](https://github.com/ocaml-sf/learn-ocaml/issues/518)) ([68221ba](https://github.com/ocaml-sf/learn-ocaml/commit/68221ba119514dbb3ab48950b6198e294cf39af2))
* **macos:** Fix `brew upgrade` failure, ter ([#536](https://github.com/ocaml-sf/learn-ocaml/issues/536)) ([af83c70](https://github.com/ocaml-sf/learn-ocaml/commit/af83c701230ed8582c362090c6eede66019d244e))
* **macos:** Run the `macOS` workflow as well in the weekly CI build ([84b7863](https://github.com/ocaml-sf/learn-ocaml/commit/84b7863df98392aeb0d190e880c2fbc261099a82))


### Documentation

* Fix date in README.md and Copyright headers ([#517](https://github.com/ocaml-sf/learn-ocaml/issues/517)) ([04bb39f](https://github.com/ocaml-sf/learn-ocaml/commit/04bb39faa16d34efd4857ba5e6a04f0a1359acb7))
* Refactor `ISSUE_TEMPLATE`s to use GitHub issue forms ([#509](https://github.com/ocaml-sf/learn-ocaml/issues/509)) ([7205c39](https://github.com/ocaml-sf/learn-ocaml/commit/7205c391d0860a7448c84a0ff7521b9dc7a821e2))

## [0.14.1](https://github.com/ocaml-sf/learn-ocaml/compare/v0.14.0...v0.14.1) (2023-01-06)


### Bug Fixes

* **learnocaml_report.css:** Display multiple spaces in code excerpts ([#503](https://github.com/ocaml-sf/learn-ocaml/issues/503)) ([c1054ab](https://github.com/ocaml-sf/learn-ocaml/commit/c1054abeafc2defd14963c50a9f62d036674fca9)), closes [#502](https://github.com/ocaml-sf/learn-ocaml/issues/502)
* **web-app:** Ask token (not secret) when opening exercise url ([#489](https://github.com/ocaml-sf/learn-ocaml/issues/489)) ([b91050e](https://github.com/ocaml-sf/learn-ocaml/commit/b91050e1caaf46fb6c3abb6f3120dd784af701f7)), closes [#488](https://github.com/ocaml-sf/learn-ocaml/issues/488)
* **web-app:** Disable the Sync button at load time ([1c068dd](https://github.com/ocaml-sf/learn-ocaml/commit/1c068ddd5d933c102ccc4c889a8ea62c240b0b6e))
* **web-app:** Disable Sync after local save for a static deployment ([dd69f3c](https://github.com/ocaml-sf/learn-ocaml/commit/dd69f3c9b0995817f789d9f77ada6cd84ca40075))
* **web-app:** Remove automatic dialog (cf. Mechanism-2 of PR [#372](https://github.com/ocaml-sf/learn-ocaml/issues/372)) ([7ea03f1](https://github.com/ocaml-sf/learn-ocaml/commit/7ea03f14444373fbbc036b4a392c5933f9af466b))
* **web-app:** Add Reload button that replaces Mechanism-2 of PR [#372](https://github.com/ocaml-sf/learn-ocaml/issues/372) ([918b79e](https://github.com/ocaml-sf/learn-ocaml/commit/918b79e875d80451b53a0f215da05e9e7223f177)), closes [#493](https://github.com/ocaml-sf/learn-ocaml/issues/493) [#505](https://github.com/ocaml-sf/learn-ocaml/issues/505)
* **learnocaml_common.ml:** Remove unused var (leftover of PR [#489](https://github.com/ocaml-sf/learn-ocaml/issues/489)) ([dc5c89c](https://github.com/ocaml-sf/learn-ocaml/commit/dc5c89ccfd9a5f3fe234c9cbdefeb017a66e82d8))


### Documentation

* **index.md:** Update index.md ([2fdc853](https://github.com/ocaml-sf/learn-ocaml/commit/2fdc853ba4095147698f8f77d39aa64d3fdf155e))
* **README.md:** Update/Fix README.md ([7174547](https://github.com/ocaml-sf/learn-ocaml/commit/7174547ff0c97f4feaabbb43c8a49b0e522693ad))
* **src/main/*:** Document exit codes in man pages forall cmd/subcmd ([c6ae2ca](https://github.com/ocaml-sf/learn-ocaml/commit/c6ae2cad5e68b7f4459c488a36e8900a402b3b9e))
* **src/main/*:** Move server options to ad-hoc section in man pages ([7c14d4c](https://github.com/ocaml-sf/learn-ocaml/commit/7c14d4c0d67d79b16d4c8b9bc06d6bd35c63ad70))


### Build System

* **static-binaries:** Fix building of static binaries on CI ([#497](https://github.com/ocaml-sf/learn-ocaml/issues/497)) ([bcb0ff7](https://github.com/ocaml-sf/learn-ocaml/commit/bcb0ff73123f6e48102e328893d6d94365d8036d)), closes [#496](https://github.com/ocaml-sf/learn-ocaml/issues/496)


### Dependencies

* **opam:** Bump ssl (0.5.10 -&gt; 0.5.12) ([#499](https://github.com/ocaml-sf/learn-ocaml/issues/499)) ([9bc28a6](https://github.com/ocaml-sf/learn-ocaml/commit/9bc28a63ab13986dab36c9843142094b05ab5856)), closes [#498](https://github.com/ocaml-sf/learn-ocaml/issues/498)
* **opam:** Upgrade cmdliner version to 1.1.1 & Improve man pages ([590034f](https://github.com/ocaml-sf/learn-ocaml/commit/590034f9fa6b38a6f7e0032c58cfc257fc253ea8))


### CI/CD

* **check-update-index.yml:** Add a workflow to check index.md ([24c56b3](https://github.com/ocaml-sf/learn-ocaml/commit/24c56b3912bc678e4e90d819ba10a02dcf2714f6)), closes [#490](https://github.com/ocaml-sf/learn-ocaml/issues/490)
* **pin-artifacts.yml:** Add build artifacts links within upstream commits status ([#501](https://github.com/ocaml-sf/learn-ocaml/issues/501)) ([e55cf61](https://github.com/ocaml-sf/learn-ocaml/commit/e55cf61bfb001a8aeeadb3dcd751825f1064e96e)), closes [#475](https://github.com/ocaml-sf/learn-ocaml/issues/475)
* **update-index.sh:** Add a script to update docs/index.md ([c538200](https://github.com/ocaml-sf/learn-ocaml/commit/c5382005c25d706f6ae2b0458c7fabb9aa96adc1)), closes [#490](https://github.com/ocaml-sf/learn-ocaml/issues/490)

## [0.14.0](https://www.github.com/ocaml-sf/learn-ocaml/compare/v0.13.1...v0.14.0) (2022-03-06)


### Features

* Build `learn-ocaml-www.zip` (`learn-ocaml --contents-dir=www`) ([#469](https://www.github.com/ocaml-sf/learn-ocaml/issues/469)) ([e36d874](https://www.github.com/ocaml-sf/learn-ocaml/commit/e36d874551f453231142fdabf8971ea708437687))
* Offer better protections against solution overwriting ([#372](https://www.github.com/ocaml-sf/learn-ocaml/issues/372)) ([5c12539](https://www.github.com/ocaml-sf/learn-ocaml/commit/5c125390d0197bd6ad39b30bedf9018570e12bb5)), closes [#316](https://www.github.com/ocaml-sf/learn-ocaml/issues/316) [#467](https://www.github.com/ocaml-sf/learn-ocaml/issues/467)
* Rename Try_OCaml→Tutorials, --enable-tryocaml→--enable-tutorials ([#451](https://www.github.com/ocaml-sf/learn-ocaml/issues/451)) ([9651160](https://www.github.com/ocaml-sf/learn-ocaml/commit/9651160201df17e09907e113027abaf13788f738))


### Bug Fixes

* Change `learn-ocaml-www.zip` inner dir (`www` → `learn-ocaml-www`) ([#474](https://www.github.com/ocaml-sf/learn-ocaml/issues/474)) ([a602ba2](https://www.github.com/ocaml-sf/learn-ocaml/commit/a602ba2609a06bebc49d8c6c8b4f80e4e851099d))
* **docker:** Add missing libs for partition-view/… using compiler-libs ([#459](https://www.github.com/ocaml-sf/learn-ocaml/issues/459)) ([c2399b0](https://www.github.com/ocaml-sf/learn-ocaml/commit/c2399b0e40e08e9605ccce3d7fa151fbd930ea8a)), closes [#438](https://www.github.com/ocaml-sf/learn-ocaml/issues/438)
* **grader:** Workaround issue [#457](https://www.github.com/ocaml-sf/learn-ocaml/issues/457) about the `Introspection` module ([#461](https://www.github.com/ocaml-sf/learn-ocaml/issues/461)) ([f5d66c6](https://www.github.com/ocaml-sf/learn-ocaml/commit/f5d66c678bf970f37bf71b54ac9104e4c877b08d))
* **UI:** Restore UX of the welcome message "Choose an activity." ([#455](https://www.github.com/ocaml-sf/learn-ocaml/issues/455)) ([9d5c7de](https://www.github.com/ocaml-sf/learn-ocaml/commit/9d5c7de495201ea18af6482875caa3d8dff1f3f1))


### Documentation

* Add `CONTRIBUTING.md` file ([#452](https://www.github.com/ocaml-sf/learn-ocaml/issues/452)) ([6e3fa0f](https://www.github.com/ocaml-sf/learn-ocaml/commit/6e3fa0f8731a383f70006abf127bbf3c8a6218ee))
* Improve `ISSUE_TEMPLATE`s & Add `PULL_REQUEST_TEMPLATE` ([#453](https://github.com/ocaml-sf/learn-ocaml/pull/453)) ([d511d58](https://github.com/ocaml-sf/learn-ocaml/commit/d511d58a53778cede681b71f9a9f45922202b371))
* Improve `{ISSUE,PULL_REQUEST}_TEMPLATE`s, slightly ([#464](https://www.github.com/ocaml-sf/learn-ocaml/issues/464)) ([e3ff435](https://www.github.com/ocaml-sf/learn-ocaml/commit/e3ff435dd32cd452ecd8a3a85f7c45102da77f71))


### Code Refactoring

* Remove unneeded `hooks/` directory ([#465](https://www.github.com/ocaml-sf/learn-ocaml/issues/465)) ([79ce07e](https://www.github.com/ocaml-sf/learn-ocaml/commit/79ce07ed290165e839f528fb7571c59900738b44))


### Build System

* **docker:** Refine the `.dockerignore` as a whitelist ([#471](https://www.github.com/ocaml-sf/learn-ocaml/issues/471)) ([41706ea](https://www.github.com/ocaml-sf/learn-ocaml/commit/41706ea09fc497ab81d7e2a28b18dbcda750e1e6))


### Tests

* Improve `make detect-libs` & Use it (`Dockerfile.test-server`) ([7c81d7f](https://www.github.com/ocaml-sf/learn-ocaml/commit/7c81d7fc69f04693ced35c76c44fe36dd02a7004))


### Dependencies

* **opam:** Add missing package (ocaml-migrate-parsetree v1.8.0) ([38809a5](https://www.github.com/ocaml-sf/learn-ocaml/commit/38809a56d45c388a74ad07d0255c1adf3ac45e5a))
* **opam:** Bump `ssl` version ([9d6da39](https://www.github.com/ocaml-sf/learn-ocaml/commit/9d6da397d02d5340ec2a4cf60e04297f831a6313))

### [0.13.1](https://www.github.com/ocaml-sf/learn-ocaml/compare/v0.13.0...v0.13.1) (2021-10-16)


### Bug Fixes

* ***.opam:** Make common deps constraints uniform & Add missing deps ([26a50ef](https://www.github.com/ocaml-sf/learn-ocaml/commit/26a50ef1ef4c37ddb990f9d7ebb71da6896cfc1a))
* **API:** Fix listing exercise status when the status list hasn't been initialized; Fix [#314](https://www.github.com/ocaml-sf/learn-ocaml/issues/314) ([3c781cb](https://www.github.com/ocaml-sf/learn-ocaml/commit/3c781cb3f9605176c915da825be877c70cfceb41))
* **grader:** Display negative numbers with mandatory parens; Fix [#440](https://www.github.com/ocaml-sf/learn-ocaml/issues/440) ([35941b5](https://www.github.com/ocaml-sf/learn-ocaml/commit/35941b5ebe8cb2b947cd6010118050a79c6e36f8))
* **UI:** Cleanup duplicate, inconsistent camel logos ([03d871a](https://www.github.com/ocaml-sf/learn-ocaml/commit/03d871ad0a70c4c50849166dd2b2f3d95ae3913a))
* **UI:** Display Actions=teacher_menubar properly in responsive mode; Fix [#444](https://www.github.com/ocaml-sf/learn-ocaml/issues/444) ([#450](https://www.github.com/ocaml-sf/learn-ocaml/issues/450)) ([b6d44db](https://www.github.com/ocaml-sf/learn-ocaml/commit/b6d44db7504e9b74169cbb69e9ae08b60a1aed01))
* **UI:** Fix CSS bug regarding the loading animation ([#445](https://www.github.com/ocaml-sf/learn-ocaml/issues/445)) ([881982a](https://www.github.com/ocaml-sf/learn-ocaml/commit/881982a8f0ec5b03b4f8c85d8ea38790ac804012))
* **UI:** Increase timeout during grading ([3cb9dd1](https://www.github.com/ocaml-sf/learn-ocaml/commit/3cb9dd1f0460b9c5d91e8c42c3bba9b23091c0e7))
* **UI:** Update one fr translation ([06a71ae](https://www.github.com/ocaml-sf/learn-ocaml/commit/06a71aec5af76e421c5e654d213fb77450eae82a))


### Dependencies

* Fix version of dune package (w.r.t. that of dune-project file) ([7c11083](https://www.github.com/ocaml-sf/learn-ocaml/commit/7c110834d9647beb80ce59d976a18efe712d5393))


### Documentation

* **tests/README.md:** Add hint for test case generation ([19477a5](https://www.github.com/ocaml-sf/learn-ocaml/commit/19477a51327ed749bc0f7cf0ad9195e1b401c5f8))


### Tests

* Add Dockefile.test-server to repro issue with learn-ocaml.opam ([6d86ce6](https://www.github.com/ocaml-sf/learn-ocaml/commit/6d86ce6a7c40b3098336315d3f4cbd4b89ace9c8))
* Add test to repro issue [#440](https://www.github.com/ocaml-sf/learn-ocaml/issues/440) ([07033c9](https://www.github.com/ocaml-sf/learn-ocaml/commit/07033c98b3e73630a4472125673dc386687bc2b1))


### CI/CD

* **build-and-test.yml:** Add ocamlsf/learn-ocaml:0.13.0 in client test matrix ([6c21d9c](https://www.github.com/ocaml-sf/learn-ocaml/commit/6c21d9c58a36364f52ac731b28b9e1622a1348dc))
* Ensure release-please triggers docker/build-push-action jobs ([#443](https://www.github.com/ocaml-sf/learn-ocaml/issues/443)) ([71c3590](https://www.github.com/ocaml-sf/learn-ocaml/commit/71c3590e46ad48269834dbaf1a3b634ec35b91b4))
* Ensure the CD-related workflows won't run on forks ([#446](https://www.github.com/ocaml-sf/learn-ocaml/issues/446)) ([6b8c49b](https://www.github.com/ocaml-sf/learn-ocaml/commit/6b8c49b36a8e2c2bd729cd0acc8f9cefde38ec2a))

## 0.13.0 (2021-10-05)

### Features

* Port to OCaml 4.12 ([#408](https://www.github.com/ocaml-sf/learn-ocaml/issues/408)) ([34f04af](https://github.com/ocaml-sf/learn-ocaml/commit/34f04af5e0725e524d73d34d6b2b83b25d2777ed))
* Relax the client/server coupling & Add support for API versioning ([#426](https://www.github.com/ocaml-sf/learn-ocaml/issues/426)) ([3113861](https://www.github.com/ocaml-sf/learn-ocaml/commit/31138615d1566f5cd7b7d63484bfe5434a47258c))
* Make server version available to front-ends (`learn-ocaml-client` & `learn-ocaml.el`) ([#429](https://github.com/ocaml-sf/learn-ocaml/pull/429)) ([d607281](https://github.com/ocaml-sf/learn-ocaml/commit/d607281bc7ee6f85fb86c1f242a70a2dc6c7020a))
* Generate static binaries directly from a GitHub Action (Linux, macOS) ([#391](https://github.com/ocaml-sf/learn-ocaml/pull/391)) ([ff9f091](https://github.com/ocaml-sf/learn-ocaml/commit/ff9f091d4af8110bed76c02afcfba66d13f17374))
* Make the prelude available in description page ([#393](https://www.github.com/ocaml-sf/learn-ocaml/issues/393)) ([51ed717](https://www.github.com/ocaml-sf/learn-ocaml/commit/51ed717fb5224aeb53c55b666019d10ceaeb68b6))
* Improve description entrypoint ([#423](https://www.github.com/ocaml-sf/learn-ocaml/issues/423)) ([a825d7e](https://www.github.com/ocaml-sf/learn-ocaml/commit/a825d7ed515fc9cc0dbd74513e902c5f8f828fc1))
* Add a ppx facility to test expressions ([#403](https://github.com/ocaml-sf/learn-ocaml/pull/403)) ([526bc07](https://github.com/ocaml-sf/learn-ocaml/commit/526bc07f47f88d777145c5cb52c97b0456bc4e67))
* Make `find_binding` match annotated declarations ([#425](https://github.com/ocaml-sf/learn-ocaml/pull/425)) ([b277b38](https://github.com/ocaml-sf/learn-ocaml/commit/b277b38167c079f506e3801e7fea36b61f63f710))
* **UI:** Discourage redundant token creation from users ([#410](https://github.com/ocaml-sf/learn-ocaml/pull/410)) ([83630d5](https://github.com/ocaml-sf/learn-ocaml/commit/83630d5fc82f63e1c533a1d5b48e1e854deb7918))
* **UI:** Move the toplevel buttons to the bottom-right ([#409](https://github.com/ocaml-sf/learn-ocaml/pull/409)) ([90066d0](https://github.com/ocaml-sf/learn-ocaml/commit/90066d0fb2ec468d0a211b371f44c59b7ebe4108))
* **UI:** Reset the toplevel every time before evaluating code from the editor ([#411](https://github.com/ocaml-sf/learn-ocaml/pull/411)) ([65ce0c42](https://github.com/ocaml-sf/learn-ocaml/commit/65ce0c42ef2075458f82c5063254b4fe68b641d0))
* **UI:** Improve UX for clickable logo ([#389](https://github.com/ocaml-sf/learn-ocaml/pull/389)) ([c406219](https://www.github.com/ocaml-sf/learn-ocaml/commit/c4062194ce23b14b3fb87198dff3ac6ef53a8ece))
* Add support for exercise code reuse ([#320](https://github.com/ocaml-sf/learn-ocaml/pull/320)) ([42d8127](https://github.com/ocaml-sf/learn-ocaml/commit/42d81279916c70528b42cf0af7eec01cf1adcbcb))
* Add VG in toplevel and exercises ([#352](https://github.com/ocaml-sf/learn-ocaml/pull/352)) ([1add7ba](https://github.com/ocaml-sf/learn-ocaml/commit/1add7babae8a899d066b61122bfe08212bcf4a28)) 


### Bug Fixes

* **CI:** Use OCaml 4.12.1 ([d5ece9b](https://www.github.com/ocaml-sf/learn-ocaml/commit/d5ece9b933bbd0b3dce879f397e1d9929a92bdfb))
* Blacklist jsoo version 3.10.0 ([#420](https://github.com/ocaml-sf/learn-ocaml/pull/420)) ([2696f86](https://github.com/ocaml-sf/learn-ocaml/commit/2696f86f4dad94d6b8645ee1ea2eb416f35e1a80))
* **.ci-macosx.sh:** Avoid `{ set -e; c1 && c2; }` bug & Improve script ([17a6703](https://www.github.com/ocaml-sf/learn-ocaml/commit/17a6703c1d4d2a59d4c0dcb78328f29cfd057add))
* **.ci-macosx.sh:** Install openssl ([0a30b4f](https://www.github.com/ocaml-sf/learn-ocaml/commit/0a30b4fe982bd16131c48b555a2072fbd8f2b277))
* **dune:** Avoid ANSI char issues in `opam show … > VERSION` ([#394](https://github.com/ocaml-sf/learn-ocaml/pull/394)) ([b72fa6c](https://www.github.com/ocaml-sf/learn-ocaml/commit/b72fa6c3047e997bdc513c9957951458cd702e39))
* **UI:** Fix the display of UTF-8 characters in the code editor ([#412](https://github.com/ocaml-sf/learn-ocaml/pull/412)) ([debb635](https://github.com/ocaml-sf/learn-ocaml/commit/debb635f216016372a44b4e18d97b72296038691))
* **omd:** Update the markdown-to-html transformation so links open a new tab ([#392](https://github.com/ocaml-sf/learn-ocaml/pull/392)) ([e0e3f2f](https://github.com/ocaml-sf/learn-ocaml/commit/e0e3f2fe13884bd2482bbe26f78b69048f910532))
* **omd:** Update `override`_url to avoid escape characters problems in URL ([1e87190](https://www.github.com/ocaml-sf/learn-ocaml/commit/1e87190a9de73136dca0b7032267e0f8c2c066e5))
* Fix static deployment ([#356](https://github.com/ocaml-sf/learn-ocaml/pull/356)) ([1332c6e](https://github.com/ocaml-sf/learn-ocaml/commit/1332c6ecb693dc737f835ea201adb7079b5350af))
* Fix & Document static deployment CLI support ([#368](https://github.com/ocaml-sf/learn-ocaml/pull/368)) ([3cc2533](https://github.com/ocaml-sf/learn-ocaml/commit/3cc2533986a608c12108f4feb5fa1754fa9e5a89))
* Fix learn-ocaml-client regression after PR [#320](https://github.com/ocaml-sf/learn-ocaml/pull/320) ([#397](https://github.com/ocaml-sf/learn-ocaml/pull/397)) ([5b16c92](https://github.com/ocaml-sf/learn-ocaml/commit/5b16c92392ac587e2aea5c0f9fb70bb16fe9ba68))
* Better error message for `X-TOKEN` registration ([#406](https://github.com/ocaml-sf/learn-ocaml/pull/406)) ([e27415c](https://www.github.com/ocaml-sf/learn-ocaml/commit/e27415cf67ba4c976f96067fd1d201999b7692af))
* **CI:** Add custom, curl-based `wait_for_it` ([d910a6e](https://www.github.com/ocaml-sf/learn-ocaml/commit/d910a6e5779133c82a0aebb1cf16c11a019eeef0))
* **Docker:** Fix runtime error in Docker packaging ([a697edf](https://www.github.com/ocaml-sf/learn-ocaml/commit/a697edfd44b6d8fdbfc935ad04533de5627022ab))
* **CI:** Bump alpine version & Use `ocaml/opam` instead of `ocaml/opam2` ([7766698](https://www.github.com/ocaml-sf/learn-ocaml/commit/7766698707759739f3893173aa3b876540c344c4))
* **CI:** Workaround the static-bin-macos failure ([#431](https://www.github.com/ocaml-sf/learn-ocaml/issues/431)) ([064dafd](https://www.github.com/ocaml-sf/learn-ocaml/commit/064dafd20d2ec12042f7ddcddaf8549b14176fd6))
* Use HTML5 Doctype ([fe0fe8b](https://www.github.com/ocaml-sf/learn-ocaml/commit/fe0fe8b983b722c685c34aa3f238e46a30dfb17b))


### Performance Improvements

* Remove redundant build step ([2b3f25b](https://www.github.com/ocaml-sf/learn-ocaml/commit/2b3f25b1a68fc21321c3cc044b334717304475e5))
* **Learnocaml_main:** Remove unneeded `Markup.pretty_print` ([e5ce820](https://www.github.com/ocaml-sf/learn-ocaml/commit/e5ce82078f2f5d2663022d9a2dae682601399dda))


### Tests

* **runtests.sh:** Improve the script & Fix all shellcheck issues ([5f4a6ac](https://www.github.com/ocaml-sf/learn-ocaml/commit/5f4a6ac1ea8ec8b0120896a2e89afb88dadb91cb))


### Continuous Integration

* CI/CD using GitHub Actions ([#361](https://github.com/ocaml-sf/learn-ocaml/pull/361)) ([bcecadd](https://github.com/ocaml-sf/learn-ocaml/commit/bcecadd9c01e3ee058a4c944e71af31019e48c78))
* **GHA:** Add a weekly build `https://crontab.guru/#0_8_*_*_6` ([63903f2](https://github.com/ocaml-sf/learn-ocaml/commit/63903f277e6eaa1b08580cbf4e0bb289996f85de))
* **GHA:** Enable cron build for `static-builds.yml` ([6478085](https://github.com/ocaml-sf/learn-ocaml/commit/6478085d72e575e368be50d4b45fc12ab4fa25a2))
* **GHA:** Deploy `ocamlsf/emacs-learn-ocaml-client:{master,$tags}` ([#433](https://github.com/ocaml-sf/learn-ocaml/pull/433)) ([dce8f00](https://github.com/ocaml-sf/learn-ocaml/commit/dce8f00ffcb42f2ac9d6bae08400c8b3ba6872d1))
* **GHA:** Ensure the uploaded artifacts are single `.tar.gz` files ([#432](https://github.com/ocaml-sf/learn-ocaml/pull/432)) ([adbb063](https://github.com/ocaml-sf/learn-ocaml/commit/adbb063e3087ff2d7c3cc04ec26d1b9bf5acc0dd))
* **release.yml:** Add a (3 jobs)-based GHA using release-please ([#434](https://www.github.com/ocaml-sf/learn-ocaml/issues/434)) ([7e389ef](https://www.github.com/ocaml-sf/learn-ocaml/commit/7e389ef22842e95e1b3f4364c19cf657a53ebf01))


### Documentation

* Document student answers classification ([#421](https://github.com/ocaml-sf/learn-ocaml/pull/421)) ([9b1a551](https://github.com/ocaml-sf/learn-ocaml/commit/9b1a55160b5732d0d0125035c841f979286ae7cd))
* Fix docs issue [#399](https://www.github.com/ocaml-sf/learn-ocaml/issues/399) and unify the markdown syntax ([276dac9](https://www.github.com/ocaml-sf/learn-ocaml/commit/276dac96c8d538a97e628e77b162c49642ae519a))
* Make accessible the documentation generated by `odoc` ([#404](https://github.com/ocaml-sf/learn-ocaml/pull/404)) ([e10975f](https://github.com/ocaml-sf/learn-ocaml/commit/e10975fdf5a4f31594f946e0fd00fc6995ba630b))
* **docs/index.md:** Update docker badges & ocaml-sf URL ([7fce8a1](https://www.github.com/ocaml-sf/learn-ocaml/commit/7fce8a1a93deeef69495bdde98a8a504ccb54b17))
* **readme:** s/HTTP/HTTPS/ ([193df99](https://www.github.com/ocaml-sf/learn-ocaml/commit/193df99160ea834b46bafbf82efceb7f6cf64a2e))
* **UI:** Improve text details ([#428](https://github.com/ocaml-sf/learn-ocaml/pull/428)) ([017f049](https://github.com/ocaml-sf/learn-ocaml/commit/017f04960fc1cf7b11f641869082f13556a7c809))

### Miscellaneous Chores

* release 0.13.0 ([f6b3b26](https://www.github.com/ocaml-sf/learn-ocaml/commit/f6b3b2652c8b8cfdc30bb86514f532f5901c660f))
