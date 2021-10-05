# Changelog

## 0.13.0 (2021-10-05)


### ⚠ BREAKING CHANGES

* 

### Features

* Add "learn-ocaml-client server-version" command ([0e6cf0a](https://www.github.com/ocaml-sf/learn-ocaml/commit/0e6cf0abf9dffde1586bd099dfa86b7ac5834e87))
* Add automated extra push for "_:latest" ([3009d8c](https://www.github.com/ocaml-sf/learn-ocaml/commit/3009d8c1873056eb3c671dad83d1e41df95b39d3))
* add entrypoint for just have the exercise information ([83507c8](https://www.github.com/ocaml-sf/learn-ocaml/commit/83507c883762a02b7ea3082c2e725af2aa364513))
* Add LEARNOCAML_ROOT_URL env var ([a24dd1c](https://www.github.com/ocaml-sf/learn-ocaml/commit/a24dd1c23a47d8ed016a0a2c770dad8bc9cca94a))
* Add metadata labels in the Docker image using a dedicated hook ([b5e1115](https://www.github.com/ocaml-sf/learn-ocaml/commit/b5e111592861bbac25524375d658ac00850b858c))
* annotation in the dependencies ([01daa94](https://www.github.com/ocaml-sf/learn-ocaml/commit/01daa9433c76731c6ba307dcfdb4e81316928ab2))
* **docker build:** Add --target parameter & Push two images at once ([10e47dd](https://www.github.com/ocaml-sf/learn-ocaml/commit/10e47dd4afe2d3bff47ffdbf73c114be264a62fc))
* **docker build:** Add flag --pull ([878ed2d](https://www.github.com/ocaml-sf/learn-ocaml/commit/878ed2d2d240aa3dc65eeef03da42991347b66a2))
* Implement ppx extension node [%prot: int -> int] ([f5ed5c4](https://www.github.com/ocaml-sf/learn-ocaml/commit/f5ed5c46b227b0973473e75be86fcbca03cd0163))
* import vg display in toplevel ([1add7ba](https://www.github.com/ocaml-sf/learn-ocaml/commit/1add7babae8a899d066b61122bfe08212bcf4a28))
* import vg in opam deps ([a70d4b6](https://www.github.com/ocaml-sf/learn-ocaml/commit/a70d4b6c992e709a7bdaf526106f6447bd0e8bf3))
* Improve description entrypoint ([#423](https://www.github.com/ocaml-sf/learn-ocaml/issues/423)) ([a825d7e](https://www.github.com/ocaml-sf/learn-ocaml/commit/a825d7ed515fc9cc0dbd74513e902c5f8f828fc1))
* Improve the "description" page to make it responsive ([3d0c76a](https://www.github.com/ocaml-sf/learn-ocaml/commit/3d0c76aaec4be1bf06074a93e22e4dd36a9bbe4e))
* introduce vg pretty printer ([8781622](https://www.github.com/ocaml-sf/learn-ocaml/commit/8781622283b8d126f67c684c971d3fc8321903f6))
* Make the prelude available in description page ([#393](https://www.github.com/ocaml-sf/learn-ocaml/issues/393)) ([51ed717](https://www.github.com/ocaml-sf/learn-ocaml/commit/51ed717fb5224aeb53c55b666019d10ceaeb68b6))
* Relax the client/server coupling & Add support for API versioning ([#426](https://www.github.com/ocaml-sf/learn-ocaml/issues/426)) ([3113861](https://www.github.com/ocaml-sf/learn-ocaml/commit/31138615d1566f5cd7b7d63484bfe5434a47258c))
* **release.yml:** Add a (3 jobs)-based GHA using release-please ([#434](https://www.github.com/ocaml-sf/learn-ocaml/issues/434)) ([7e389ef](https://www.github.com/ocaml-sf/learn-ocaml/commit/7e389ef22842e95e1b3f4364c19cf657a53ebf01))
* update the transformation from markdown to html which allow to open in a new tab when clicking on a link ([0a9cc43](https://www.github.com/ocaml-sf/learn-ocaml/commit/0a9cc433fe24346558dab6e6df476baa6c7d5297))


### Bug Fixes

* **.ci-macosx.sh:** Avoid { set -e; c1 && c2; } bug & Improve script ([17a6703](https://www.github.com/ocaml-sf/learn-ocaml/commit/17a6703c1d4d2a59d4c0dcb78328f29cfd057add))
* **.ci-macosx.sh:** Install openssl ([0a30b4f](https://www.github.com/ocaml-sf/learn-ocaml/commit/0a30b4fe982bd16131c48b555a2072fbd8f2b277))
* (forward/backward/…) links were broken in description pages ([09b828c](https://www.github.com/ocaml-sf/learn-ocaml/commit/09b828c89f0278617b878468631d887ffa908ad4))
* 244 ([52b800f](https://www.github.com/ocaml-sf/learn-ocaml/commit/52b800ff02890554845b412b1ae8f5a693d9dd8d))
* 244 for osx ([4676001](https://www.github.com/ocaml-sf/learn-ocaml/commit/46760015683be51fb9eaeed5e3292599fd594291))
* Add custom, curl-based wait_for_it ([d910a6e](https://www.github.com/ocaml-sf/learn-ocaml/commit/d910a6e5779133c82a0aebb1cf16c11a019eeef0))
* Address review comments by [@yurug](https://www.github.com/yurug) ([711102a](https://www.github.com/ocaml-sf/learn-ocaml/commit/711102abf9cb794baef4c677d268a5199b78b508))
* algorithm to extract the token ([e1ec5cd](https://www.github.com/ocaml-sf/learn-ocaml/commit/e1ec5cdb50eb0f1c2fadd6a252833e7fc1781cc5))
* arguments of find command ([5b16c92](https://www.github.com/ocaml-sf/learn-ocaml/commit/5b16c92392ac587e2aea5c0f9fb70bb16fe9ba68))
* better error msg for X-token registration ([e27415c](https://www.github.com/ocaml-sf/learn-ocaml/commit/e27415cf67ba4c976f96067fd1d201999b7692af))
* Bump alpine version ([7766698](https://www.github.com/ocaml-sf/learn-ocaml/commit/7766698707759739f3893173aa3b876540c344c4))
* CI ([d5ece9b](https://www.github.com/ocaml-sf/learn-ocaml/commit/d5ece9b933bbd0b3dce879f397e1d9929a92bdfb))
* **client,main:** Add -lcurses if (= %{ocaml-config:system} macosx) ([18e1d50](https://www.github.com/ocaml-sf/learn-ocaml/commit/18e1d505a5ad170c2a3edc1440cd5eb4dc4c9cc7))
* dead links in docs ([36f6272](https://www.github.com/ocaml-sf/learn-ocaml/commit/36f6272e6cf339e26fda2cce210399cff30abc3f))
* **detail:** Add missing colon in learnocaml_common.ml ([26df32a](https://www.github.com/ocaml-sf/learn-ocaml/commit/26df32a44261c78ec3a8f76a96677c86b1bf3ab9))
* details ([e41d8a1](https://www.github.com/ocaml-sf/learn-ocaml/commit/e41d8a1930c5b28d24144a85b7b8f0060f14ca16))
* doc of LEARNOCAML_BASE_URL ([af0b57e](https://www.github.com/ocaml-sf/learn-ocaml/commit/af0b57ee85f4a73a2bb19d2ccc9e1174a69d24af))
* **docs/index.md:** Update docker badges & ocaml-sf URL ([7fce8a1](https://www.github.com/ocaml-sf/learn-ocaml/commit/7fce8a1a93deeef69495bdde98a8a504ccb54b17))
* **docs:** should fix [#399](https://www.github.com/ocaml-sf/learn-ocaml/issues/399) and unify the markdown syntax ([276dac9](https://www.github.com/ocaml-sf/learn-ocaml/commit/276dac96c8d538a97e628e77b162c49642ae519a))
* don't cache exercise requests ([efa08b0](https://www.github.com/ocaml-sf/learn-ocaml/commit/efa08b02a032159e9948c4c2a63ce63f5d8d94ab))
* **dune-project:** learn-ocaml version ([93d81f7](https://www.github.com/ocaml-sf/learn-ocaml/commit/93d81f7fd08dfce00c8dbc176976ed25382c6f7c))
* exercises are always Open for teachers ([5bc9c25](https://www.github.com/ocaml-sf/learn-ocaml/commit/5bc9c2541a890414bc67b45ccb56e0113a38e538))
* fix the links present on the metadata part ([5c7a004](https://www.github.com/ocaml-sf/learn-ocaml/commit/5c7a0040badb86ef0f9efe36b7cfcaa2e956bb96))
* Improve the way the initial teacher token is read ([15a7386](https://www.github.com/ocaml-sf/learn-ocaml/commit/15a7386d7678fc7e5e3d9e21de9af32cc77de39e))
* improve UX for clickable logo ([36df963](https://www.github.com/ocaml-sf/learn-ocaml/commit/36df9638351072d15f529b02c63fb0cecd47fbe7))
* **learn-ocaml-client server-version:** No need for a cookie file ([c200d97](https://www.github.com/ocaml-sf/learn-ocaml/commit/c200d973faad93dbe099635be9df668eb8d485ec))
* **learnocaml_partition_create.ml:** non exhaustive pattern-matchings ([0454274](https://www.github.com/ocaml-sf/learn-ocaml/commit/0454274161e6f4ded279802870ab309e4aaa16c1))
* **learnocaml_server:** Make the server also aware of the root_url ([1cf718f](https://www.github.com/ocaml-sf/learn-ocaml/commit/1cf718f85b3972b416e60b92af845dea96841677))
* macOS build ([13d4766](https://www.github.com/ocaml-sf/learn-ocaml/commit/13d4766126bfaf050e2fdeba3a3a7b1daee69fbc))
* Make fields added in PR [#320](https://www.github.com/ocaml-sf/learn-ocaml/issues/320) optional ([c24fef8](https://www.github.com/ocaml-sf/learn-ocaml/commit/c24fef8d89e95c9690c7d0c0afb994f9d8f2a5a8))
* printing of BASE_URL ([a5fda1d](https://www.github.com/ocaml-sf/learn-ocaml/commit/a5fda1d0a537795d60a1880fbfbdbfef34c029c6))
* **readme:** s/HTTP/HTTPS/ ([193df99](https://www.github.com/ocaml-sf/learn-ocaml/commit/193df99160ea834b46bafbf82efceb7f6cf64a2e))
* Remove redundant build step ([2b3f25b](https://www.github.com/ocaml-sf/learn-ocaml/commit/2b3f25b1a68fc21321c3cc044b334717304475e5))
* **runtests.sh:** Improve the script & Fix all shellcheck issues ([5f4a6ac](https://www.github.com/ocaml-sf/learn-ocaml/commit/5f4a6ac1ea8ec8b0120896a2e89afb88dadb91cb))
* runtime error in Docker packaging ([a697edf](https://www.github.com/ocaml-sf/learn-ocaml/commit/a697edfd44b6d8fdbfc935ad04533de5627022ab))
* s/ROOT_URL/BASE_URL/ ([bf4b096](https://www.github.com/ocaml-sf/learn-ocaml/commit/bf4b0964116383d284e055b47dd548df2093594d))
* small issue with `make detect-libs` on macOS ([7276fab](https://www.github.com/ocaml-sf/learn-ocaml/commit/7276fab2936c6fff0ec6766ecd1b3113d383ff69))
* **static:** path of /icons/tryocaml_loading_*.gif ([8673786](https://www.github.com/ocaml-sf/learn-ocaml/commit/8673786bfed84cf0ca6596af1e902d1fdda35b4f))
* update dune file to avoid problems with ANSI char with opam run show command ([b72fa6c](https://www.github.com/ocaml-sf/learn-ocaml/commit/b72fa6c3047e997bdc513c9957951458cd702e39))
* update override_url to handle internal links ([2b071d1](https://www.github.com/ocaml-sf/learn-ocaml/commit/2b071d1e97eec3fc274304de83ecea3343d650aa))
* update overrideURL to handle escape characters problems in URL ([1e87190](https://www.github.com/ocaml-sf/learn-ocaml/commit/1e87190a9de73136dca0b7032267e0f8c2c066e5))
* Use HTML5 Doctype ([fe0fe8b](https://www.github.com/ocaml-sf/learn-ocaml/commit/fe0fe8b983b722c685c34aa3f238e46a30dfb17b))
* Workaround the static-bin-macos failure ([#431](https://www.github.com/ocaml-sf/learn-ocaml/issues/431)) ([064dafd](https://www.github.com/ocaml-sf/learn-ocaml/commit/064dafd20d2ec12042f7ddcddaf8549b14176fd6))


### Performance Improvements

* **learn-ocaml-client server-version:** Trigger only 1 GET /version ([25311fa](https://www.github.com/ocaml-sf/learn-ocaml/commit/25311fa8a86a32b8395a7c7f1457735eeabc713a))
* **Learnocaml_main:** Remove unneeded Markup.pretty_print ([e5ce820](https://www.github.com/ocaml-sf/learn-ocaml/commit/e5ce82078f2f5d2663022d9a2dae682601399dda))


### Code Refactoring

* Rename one CLI option (s/--root/--root-url/) ([13b1f83](https://www.github.com/ocaml-sf/learn-ocaml/commit/13b1f83bbfa4e609eb2c3e6ddd0a8b929c34c102))


### Miscellaneous Chores

* release 0.13.0 ([f6b3b26](https://www.github.com/ocaml-sf/learn-ocaml/commit/f6b3b2652c8b8cfdc30bb86514f532f5901c660f))
