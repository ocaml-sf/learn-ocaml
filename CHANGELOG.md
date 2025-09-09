# Changelog

## [1.1.0](https://github.com/ocaml-sf/learn-ocaml/compare/v1.0.0...v1.1.0) (2025-09-09)


### Features

* **build:** Migrate ocplib-i18n to ppxlib ([e34b5f7](https://github.com/ocaml-sf/learn-ocaml/commit/e34b5f77e358e8f5289c25f41c0bbb5d5c9a8848))
* bump version to 1.1.0 ([9d757cd](https://github.com/ocaml-sf/learn-ocaml/commit/9d757cd29889e36eb0b62eb418ac5b8865023ed7))
* **CLI:** Add option `learn-ocaml build serve --serve-during-build` ([a47f8dd](https://github.com/ocaml-sf/learn-ocaml/commit/a47f8ddef2762ad20c2bb168f05d706d6fa31dd5)), closes [#594](https://github.com/ocaml-sf/learn-ocaml/issues/594)
* complete porting to OCaml 5.1 and jsoo 5.8 ([5f5dbb7](https://github.com/ocaml-sf/learn-ocaml/commit/5f5dbb7be1307adda6223838847a6b92806de643))
* Reenable asak (using pin-depends for compat patch with OCaml 5) ([046d7dd](https://github.com/ocaml-sf/learn-ocaml/commit/046d7dd92fee318f17540fcca9ddaf86b215cecb))


### Bug Fixes

* Actually bind the `--dry-run` flag ([1a76ddb](https://github.com/ocaml-sf/learn-ocaml/commit/1a76ddba19acf32d35f91554e9ece3a355b33b83))
* **build:** Fix static build on macOS w.r.t. libzstd ([3e8afe8](https://github.com/ocaml-sf/learn-ocaml/commit/3e8afe8bce87abe0b165d3bb7d1b866e22094015))
* **build:** Fix/disable some warnings ([49c7208](https://github.com/ocaml-sf/learn-ocaml/commit/49c72080c7e412a047eed2326b150b48bd024ba0))
* **build:** Update linking flags (needed for POSIX & macOS static binaries) ([6b0bc56](https://github.com/ocaml-sf/learn-ocaml/commit/6b0bc56bfbe46b2ea86dcc1562c78ea90d1d3a07))
* **CI:** fix base Alpine version for final Docker images ([e721ad4](https://github.com/ocaml-sf/learn-ocaml/commit/e721ad4cd1dd292dbe2288712f9a9768459e3e0a))
* **CI:** fix Dockerfile and client dependencies ([8012c6b](https://github.com/ocaml-sf/learn-ocaml/commit/8012c6b3bc6ebbb9692437063abf5b0620435abb))
* **CI:** fix static builds and some specific deps ([283456d](https://github.com/ocaml-sf/learn-ocaml/commit/283456d52c164d106b29914e735ac3899209f7ef))
* **CI:** Use macos-13 for static-builds ([#603](https://github.com/ocaml-sf/learn-ocaml/issues/603)) ([d2c8d65](https://github.com/ocaml-sf/learn-ocaml/commit/d2c8d65b12c2512bd35204305d59c5eb486d8a96))
* **CI:** use older dependency releases in lockfile ([a5b4abf](https://github.com/ocaml-sf/learn-ocaml/commit/a5b4abf91285f7076320acd67fb348a01566d57b))
* **docker:** Add missing alpine package (lsof) ([cc6caa3](https://github.com/ocaml-sf/learn-ocaml/commit/cc6caa34947000e6e60c175b607f5b0437609dc9))
* font size of code fragments in descr ([#487](https://github.com/ocaml-sf/learn-ocaml/issues/487)) ([b8ebfcc](https://github.com/ocaml-sf/learn-ocaml/commit/b8ebfccee5753bf9fd8ab3cbb733b1aaa12c9cfb)), closes [#214](https://github.com/ocaml-sf/learn-ocaml/issues/214)
* **server:** Add missing flush in Learnocaml_server.kill_running ([3b14462](https://github.com/ocaml-sf/learn-ocaml/commit/3b14462aebd7e9a3a0c9a109442182fbb3af2671))


### Performance Improvements

* **GHA:** Perform more caching, as the Dockerfile does multi-stage builds ([#632](https://github.com/ocaml-sf/learn-ocaml/issues/632)) ([b28115a](https://github.com/ocaml-sf/learn-ocaml/commit/b28115aad2377279eade202b68500b5d47c02cd7)), closes [#627](https://github.com/ocaml-sf/learn-ocaml/issues/627)


### Dependencies

* **opam:** Bump conf-libssl ([2081069](https://github.com/ocaml-sf/learn-ocaml/commit/20810690c73b1ae6996985fb037267818558d62d))
* **opam:** Bump conf-pkg-config ([c50b77d](https://github.com/ocaml-sf/learn-ocaml/commit/c50b77d212699935f56fb735702059900fba19b2))
* **opam:** Require js_of_ocaml &lt; 6.0.0 ([dcb59ce](https://github.com/ocaml-sf/learn-ocaml/commit/dcb59ce20ddc667fec7db5dc4a2b2582d9ebf489))
* **opam:** Skip conduit-lwt-unix's latest release for now ([a636b26](https://github.com/ocaml-sf/learn-ocaml/commit/a636b26c14eac8203c406d9a66c7261848d9a924))


### Build System

* add `opam update` to static-build.sh ([a1f5d6e](https://github.com/ocaml-sf/learn-ocaml/commit/a1f5d6ee80060bbd075e9c031818cb95b6b8dddc))
* **deps:** bump dawidd6/action-download-artifact from 2 to 6 in /.github/workflows ([#605](https://github.com/ocaml-sf/learn-ocaml/issues/605)) ([5780d3a](https://github.com/ocaml-sf/learn-ocaml/commit/5780d3ab58961b0d386d89bf608c8fa8798ae1b1))
* **deps:** bump dawidd6/action-download-artifact in /.github/workflows ([5780d3a](https://github.com/ocaml-sf/learn-ocaml/commit/5780d3ab58961b0d386d89bf608c8fa8798ae1b1))
* **js_of_ocaml:** Remove `--no-source-map` from root dune file ([6f6be0f](https://github.com/ocaml-sf/learn-ocaml/commit/6f6be0f6c80fa27a002c0268d087fe0f37965f2f))
* lower upper bound of js_of_ocaml ([3e839c6](https://github.com/ocaml-sf/learn-ocaml/commit/3e839c61d591859a09e237fc13723394b0b9187c))
* update pinned package versions ([b21586f](https://github.com/ocaml-sf/learn-ocaml/commit/b21586f484292ef020e8932c33ae6a7f22a7af3f))
* update pinned package versions ([#622](https://github.com/ocaml-sf/learn-ocaml/issues/622)) ([ce843c5](https://github.com/ocaml-sf/learn-ocaml/commit/ce843c546d7c39331e1021deb58ee06111769b31))


### CI/CD

* Bump several GitHub Actions tags to latest major version ([8b1bc50](https://github.com/ocaml-sf/learn-ocaml/commit/8b1bc508af7e4e003e5eec8cbd22320e29313ae0))
* **deploy-master.yml:** Add missing GHA `docker/setup-qemu-action` ([7ba32cd](https://github.com/ocaml-sf/learn-ocaml/commit/7ba32cd7515cd50e5ccdf640e1b47c9fc36b91f1))
* **deploy-master.yml:** Build and push images for amd64 and arm64/v8 ([af5b77a](https://github.com/ocaml-sf/learn-ocaml/commit/af5b77a4240e899d9bb98f992462ad245c24d0bf))
* **deploy-master.yml:** Use the docker-container driver for multi-platform builds ([3431025](https://github.com/ocaml-sf/learn-ocaml/commit/343102570fb043e59e21a0df231f94ae8b39ab3d))
* **gha:** Enable gha cache (mode=min) in `docker/build-push-action` ([d7f78c7](https://github.com/ocaml-sf/learn-ocaml/commit/d7f78c700a0113b60ef20e1b28e15987e0216e2a))
* **GHA:** Make deployment workflows up-to-date ([#630](https://github.com/ocaml-sf/learn-ocaml/issues/630)) ([d3bd293](https://github.com/ocaml-sf/learn-ocaml/commit/d3bd293abf266337711d41c71975502f3d2e982e)), closes [#595](https://github.com/ocaml-sf/learn-ocaml/issues/595)
* **GHA:** Use actions/checkout@v4 ([#599](https://github.com/ocaml-sf/learn-ocaml/issues/599)) ([b7ed29c](https://github.com/ocaml-sf/learn-ocaml/commit/b7ed29c4748ab2d8dfe6969ac1479f0ced68eb80))
* **gha:** Use actions/upload-artifact@v4 & dawidd6/action-download-artifact@v8 ([02476fe](https://github.com/ocaml-sf/learn-ocaml/commit/02476fed50fafb014bbddb1b014b304a42b0ebca))
* **macos:** s/macos-latest/macos-14/ ([#598](https://github.com/ocaml-sf/learn-ocaml/issues/598)) ([0a69394](https://github.com/ocaml-sf/learn-ocaml/commit/0a69394bd4a1462fe0891b375e6efd08938f0c8c))
* **publish-artifacts.yml:** Bump GHA versions ([61ee0cd](https://github.com/ocaml-sf/learn-ocaml/commit/61ee0cd11cf8bf1940ee38ea031cec5cef4278d0))
* **release.yml:** Build and push images for amd64 and arm64/v8 ([396b615](https://github.com/ocaml-sf/learn-ocaml/commit/396b615f7d4d6555b007ca91a523b3895c6f24b4)), closes [#626](https://github.com/ocaml-sf/learn-ocaml/issues/626)
* **release.yml:** Build and push images for amd64 and arm64/v8 ([#628](https://github.com/ocaml-sf/learn-ocaml/issues/628)) ([1641137](https://github.com/ocaml-sf/learn-ocaml/commit/16411373dda3a9f2fca484a4b1a8d9c2f175c343)), closes [#626](https://github.com/ocaml-sf/learn-ocaml/issues/626)


### Documentation

* **CONTRIBUTING.md:** minor update ([f318b95](https://github.com/ocaml-sf/learn-ocaml/commit/f318b958dae945d7f7e7a7b8fe091f51eb22263e))
* Document how to include local images in exercises ([#612](https://github.com/ocaml-sf/learn-ocaml/issues/612)) ([d3bc210](https://github.com/ocaml-sf/learn-ocaml/commit/d3bc210f616bc5c27b329c24efd6e863d4ec1d62)), closes [#128](https://github.com/ocaml-sf/learn-ocaml/issues/128)
* Update index.md ([25af64f](https://github.com/ocaml-sf/learn-ocaml/commit/25af64fab5938cea1a70052820d3b3ee5d56d71b))
* Update index.md & publish-artifacts.yml ([#615](https://github.com/ocaml-sf/learn-ocaml/issues/615)) ([0a50c15](https://github.com/ocaml-sf/learn-ocaml/commit/0a50c15a4b96ad11643d48e9bc8488d12993f775))


### Style

* Address warnings in Dockerfiles and GHA workflows ([#634](https://github.com/ocaml-sf/learn-ocaml/issues/634)) ([5ffd748](https://github.com/ocaml-sf/learn-ocaml/commit/5ffd748f6d68e4eabe6e4a351a5fa65bd2087fb5))
* Fix displayed version to adapt to git tags naming convention ([2784290](https://github.com/ocaml-sf/learn-ocaml/commit/2784290e758a2fcfef7807cce5fd4e9c5e44e59f))

## [1.0.0](https://github.com/ocaml-sf/learn-ocaml/compare/v0.16.0...v1.0.0) (2024-02-12)


### ⚠ BREAKING CHANGES

* Implement pre-compilation of exercises and graders.  
  Prefix **pre-compilation** indicates the related commits below (sorted in chronological order).
* Remove doc tutorial on `depend.txt` (it will need rewriting; relying on the new server engine).


### Features

* **pre-compilation:** Implement pre-compilation of exercises and graders ([b03bdfe](https://github.com/ocaml-sf/learn-ocaml/commit/b03bdfe5a7c9ed5d4677b1ee2eb11d37e953cbe3))
* **pre-compilation:** Enable downloading for only the relevant artifacts (bc or js) ([47d5a06](https://github.com/ocaml-sf/learn-ocaml/commit/47d5a0614f82e313520c79739922616b99d8c868))
* **pre-compilation:** Include Prelude/Prepare and shadow them ([787840b](https://github.com/ocaml-sf/learn-ocaml/commit/787840bda9701ae932ce5f9d56c5929dbb889e17))
* **pre-compilation: ppx-metaquot:** Add transformation introducing the `register_sampler` calls ([3cd75f5](https://github.com/ocaml-sf/learn-ocaml/commit/3cd75f5fbf580ab7ec388ef36976a0b32ca118ce))
* **pre-compilation:** Restore compatibility with static deployment ([f0e8346](https://github.com/ocaml-sf/learn-ocaml/commit/f0e8346450a826a9955a71b69aa4504865d2d56a))
* **pre-compilation:** Add support for a `test_libs.txt` file in exercises ([d22a788](https://github.com/ocaml-sf/learn-ocaml/commit/d22a78822b007e92f576288713dfa5ed6be2b1de))
* **pre-compilation:** Preprocessing and typing of samplers and printers ([e768616](https://github.com/ocaml-sf/learn-ocaml/commit/e7686163f9b23acc6140cabd406f2999f228fb50))
* **pre-compilation:** Provide lib to compile grader helper libraries ([3fc41ca](https://github.com/ocaml-sf/learn-ocaml/commit/3fc41caf632ede074476d760b892ebd61b790b9e))
* **server:** add a `--replace` option ([82d9bea](https://github.com/ocaml-sf/learn-ocaml/commit/82d9bea4db4034857747b4da2e7b3121bd6c2b28))
* **grader:** Show a status line on what is being built ([995a79d](https://github.com/ocaml-sf/learn-ocaml/commit/995a79d5f02b787981eb29bffb012d6993e3c63f))
* **CI: static-binaries:** Deploy artifacts to GitHub Pages ([01eae90^..9cf5486](https://github.com/ocaml-sf/learn-ocaml/compare/6cee13c160aba5db8aba0968df16d069e02e8fda...9cf5486b02d098f556a72d5bd639196701bee633)), closes [#575](https://github.com/ocaml-sf/learn-ocaml/issues/575)
* **pre-compilation: CLI:** Add CLI option `learn-ocaml build --build-dir=[./_learn-ocaml-build]` to increase compatibility with existing workflows ([#585](https://github.com/ocaml-sf/learn-ocaml/issues/585)) ([6535692](https://github.com/ocaml-sf/learn-ocaml/commit/6535692bc77eba471a97bb671658b4db4c86b4f4))


### Bug Fixes

* **grading:** avoid failing on sampling arrays with unique elements ([6a3ce07](https://github.com/ocaml-sf/learn-ocaml/commit/6a3ce077e1ce0946e9e10324b5e1e1b51669e62c))
* **pre-compilation:** Fix a small race condition in builder ([87ee902](https://github.com/ocaml-sf/learn-ocaml/commit/87ee902e1c60fd3d5b6fcb40e68b563062b70662))
* **pre-compilation:** Properly type samplers ([a97f813](https://github.com/ocaml-sf/learn-ocaml/commit/a97f81367bcce970e6474f39d3f5940df77cb880))
* **pre-compilation:** Avoid double-printing of internal grader errors ([7422ca4](https://github.com/ocaml-sf/learn-ocaml/commit/7422ca439fb450af6dab362ccc95e094f4297b41))
* **pre-compilation:** Fix segfault on graders using samplers returning newly defined exceptions ([c61a4d0](https://github.com/ocaml-sf/learn-ocaml/commit/c61a4d06715180069e9c6625f9c9559af476054d))
* **pre-compilation:** Be more precise on the definition and lookup of samplers ([7825a6b](https://github.com/ocaml-sf/learn-ocaml/commit/7825a6b6d15b213b1297d7878e8fb36057ec7b81))
* **pre-compilation:** Fix printer registration in the grader ([7d27523](https://github.com/ocaml-sf/learn-ocaml/commit/7d2752392ec7bf74a21d1604e2f35a221377e3f1))
* **pre-compilation:** Do some cleanup & Fix `mutation_testing` test lib ([c432909](https://github.com/ocaml-sf/learn-ocaml/commit/c43290947491dc3e1de76a5df3581531971332aa))
* **pre-compilation:** Allow printer registration in prepare/prelude & Fix print callbacks' usage ([1ec3af6](https://github.com/ocaml-sf/learn-ocaml/commit/1ec3af6ebec857f79f440706779190541636ce68))
* **pre-compilation: dune:** Fix dune dependency glitch on recompilation of `mutation_test` ([32ad13e](https://github.com/ocaml-sf/learn-ocaml/commit/32ad13e1915239af563f88fa673b73649d685f28))
* **pre-compilation: docker:** Include jsoo in Dockerfile, which is now needed ([466e80c](https://github.com/ocaml-sf/learn-ocaml/commit/466e80ca8e5ea1ab99590d4795f7913188dd0333))
* **pre-compilation: CI:** Fix permission issues ([fa2cd23](https://github.com/ocaml-sf/learn-ocaml/commit/fa2cd23babafcea0ff7a2d685993a838af65eada))
* **pre-compilation:** Expose `prepare.ml` file ([365cbb7](https://github.com/ocaml-sf/learn-ocaml/commit/365cbb719a5048a6f422278fc986f822ad17770b))
* **pre-compilation: partition-view:** Reactivate the feature ([57ca10b](https://github.com/ocaml-sf/learn-ocaml/commit/57ca10b0a40157e6b97d974c76c9963a1e00a0aa))
* **pre-compilation: CLI:** Report JSON parse error origin and locations ([ee57ac1](https://github.com/ocaml-sf/learn-ocaml/commit/ee57ac18dc7d395108defaa38079affb13f6ccaf))
* **pre-compilation: grader:**: Add a safeguard against grading workers going haywire ([cb417d1](https://github.com/ocaml-sf/learn-ocaml/commit/cb417d186a32b2b0d11aea4228c264642b64bf34))
* **pre-compilation: grader:** allow exercises to use vg, gg ([ead187e](https://github.com/ocaml-sf/learn-ocaml/commit/ead187e387d5794d3f16bb420a77e643b95f4b5a))
* **pre-compilation: partition-view:** use newer asak compatible with precompilation ([942edc2](https://github.com/ocaml-sf/learn-ocaml/commit/942edc2fb1b30336f45caf9027d18f2bd5221ea9))
* **pre-compilation: build:** update lockfiles ([f1abb7d](https://github.com/ocaml-sf/learn-ocaml/commit/f1abb7d48e00e19edd3d922a043d309767b4c339))
* **pre-compilation: CI:** attempt to fix running the docker image on the corpus ([b94f053](https://github.com/ocaml-sf/learn-ocaml/commit/b94f05368a02038e5efb978976d8f7e20154fcc6))
* **pre-compilation: CI:** disable compat tests with 0.12, 0.13 ([91a418e](https://github.com/ocaml-sf/learn-ocaml/commit/91a418eeadf3be73e716a83ae16153332d7d19e7))
* **pre-compilation: docker:** install more libs in server image ([6ce797f](https://github.com/ocaml-sf/learn-ocaml/commit/6ce797f818766047c85d543188767fb4d3609352))
* **pre-compilation: grader:** avoid errors with too many open files on parallel builds ([6583af4](https://github.com/ocaml-sf/learn-ocaml/commit/6583af4bbb3547b302963922279e0516edd9d6b4))
* **server:** Avoid using `lsof -Q` which is only available from lsof 4.95.0 ([a242084](https://github.com/ocaml-sf/learn-ocaml/commit/a242084cde9eaf4ab25b205b71c244c23279704f)), closes [#580](https://github.com/ocaml-sf/learn-ocaml/issues/580)
* **UI:** Small CSS fix for exercise lists on small screens ([3c9c123](https://github.com/ocaml-sf/learn-ocaml/commit/3c9c1237f5e2565cc173e7f57b864866d190a83d)), closes [#574](https://github.com/ocaml-sf/learn-ocaml/issues/574)
* **server:** Do exercise recompilation correctly with `--replace` ([#584](https://github.com/ocaml-sf/learn-ocaml/issues/584)) ([fe2a806](https://github.com/ocaml-sf/learn-ocaml/commit/fe2a806fa306a46b1c978fe47fbd3c26170ee52c)), closes [#583](https://github.com/ocaml-sf/learn-ocaml/issues/583)


### Performance Improvements

* **pre-compilation:** Make `learn-ocaml build` parallel by default ([eaad14c](https://github.com/ocaml-sf/learn-ocaml/commit/eaad14cfe1d693081c43277c71dca8a74bd5a5a7))
* **pre-compilation:** Dump the cmis for grading only once ([e63359e](https://github.com/ocaml-sf/learn-ocaml/commit/e63359e38760ee052b62d6ccf3e46ad1db46e988))


### Code Refactoring

* **pre-compilation:** Get rid of the pseudo-cipher ([2792faf](https://github.com/ocaml-sf/learn-ocaml/commit/2792faf8f49b7b874ae9a854ccafb2c4a2922383))
* **pre-compilation:** Rename and generalise `recorder` to `ppx_autoregister` ([99e913d](https://github.com/ocaml-sf/learn-ocaml/commit/99e913d847c54e021669c7068ca991ae24de89f2))
* **pre-compilation:** Generalize sampler typing ([264db4c](https://github.com/ocaml-sf/learn-ocaml/commit/264db4c0436f581c5fc68f222305e8f8770fd674))
* **pre-compilation:** Disable debug flags ([54851dd](https://github.com/ocaml-sf/learn-ocaml/commit/54851dd368e1c8a5635a2a193751aec1e46f1d97))


### Build System

* **pre-compilation:** Make `make testrun` parallel ([46631d8](https://github.com/ocaml-sf/learn-ocaml/commit/46631d8e62385d934012ae16d756ce6f4bee4139))


### CI/CD

* **release.yml:** Replace `hub` (not installed anymore) with `gh` ([cad060f](https://github.com/ocaml-sf/learn-ocaml/commit/cad060f801dba21976cffc03f2fcb4119a3dec75))
* **release.yml:** Next release version will be 1.0.0 ([6e9cd2b](https://github.com/ocaml-sf/learn-ocaml/commit/6e9cd2bbdb4a695397a7eea0d2560bb8e694cf37))


### Documentation

* **pre-compilation: translations:** Update French translation ([f028b75](https://github.com/ocaml-sf/learn-ocaml/commit/f028b75b09676120669ea6f4b9e0beff686c9302))
* **pre-compilation:** Remove doc tutorial on `depend.txt` (it will need rewriting) ([9155145](https://github.com/ocaml-sf/learn-ocaml/commit/915514524c501317d79c6e40ee8a948f6a3e5af1))
* **pre-compilation:** Update doc for pre-compiled exercises + `test_libs.txt` ([2c89d9e](https://github.com/ocaml-sf/learn-ocaml/commit/2c89d9e0935cfce90031502c85cfb6ffa7ca000e))
* **pre-compilation:** Add/Update copyright headers ([5b4e0ab](https://github.com/ocaml-sf/learn-ocaml/commit/5b4e0abaf1c16c1f1964014ab0b4feecc6bcdef8))
* **pre-compilation:** Update index.md ([f572990](https://github.com/ocaml-sf/learn-ocaml/commit/f572990b4a25363da2b907f08b1f5aa0065273f4))


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
