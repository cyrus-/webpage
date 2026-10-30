# Vendored packages

These packages are copied from Anil Madhavapeddy's `monopam-myspace`
monorepo (https://tangled.org/anil.recoil.org/monopam-myspace), commit
`4e9c14ab9e9201cc8bb1f4654fca9a82d591b9d5` (2026-01-29). All are ISC
licensed; see each package's `LICENSE.md` (or `LICENSE-monopam-myspace.md`
for `arod` and `ocaml-tomlt`, which don't ship their own).

| Directory           | Package           | Purpose                                   |
|---------------------|-------------------|-------------------------------------------|
| `arod`              | arod              | Site renderer / webserver for Bushel data |
| `ocaml-bushel`      | bushel            | Typed Markdown+frontmatter content store  |
| `ocaml-frontmatter` | frontmatter       | YAML frontmatter parsing                  |
| `ocaml-tomlt`       | tomlt             | TOML codecs (config)                      |
| `ocaml-yamlrw`      | yamlrw            | YAML 1.2 reader/writer                    |
| `ocaml-yamlt`       | yamlt             | YAML codecs over jsont                    |
| `ocaml-bytesrw-eio` | bytesrw-eio       | Eio adapters for bytesrw                  |
| `sortal`            | sortal            | Contact records (`@handle` links)         |
| `srcsetter`         | srcsetter         | Responsive image generation               |
| `xdge`              | xdge              | XDG base directories                      |

Removed when copying, since the site doesn't use them: test, fuzz,
benchmark, example and doc directories; command-line `bin/` directories
other than `arod/bin`; and `bushel`'s `lib_sync` (HTTP thumbnail/video
fetching) and `lib_typesense` (search indexing) subpackages.

Local changes to these packages should be kept small and noted here.
