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

## Local changes

- `arod/lib/arod_footer.ml`: the footer no longer hard-codes Anil's social
  links. `Arod_footer.footer` is now a function returning a replaceable
  value (`Arod_footer.set`); the default shows only the Atom/JSON feed
  links. Call sites in `arod_entries.ml`, `arod_ideas.ml`,
  `arod_projects.ml` and `arod_page.ml` updated accordingly. `site/main.ml`
  sets the footer from the site author's contact card.
- `arod/lib/arod_page.ml`: the page frame (header, content area, footer)
  is replaceable via `Arod_page.set_frame`; the default is unchanged.
  `site/main.ml` uses it for the two-column layout with a tab bar.
- `arod/lib/arod_feed.ml`: the Atom feeds no longer carry Anil's copyright
  as their `rights`; a feed's `updated` is its newest note (the epoch when
  it has none, which used to raise); and each feed's self link is its own
  URL rather than always `/news.xml`.
- `arod/lib/arod_view.ml`: sidenote triggers (`[:slug]`, `[@@handle]`) are
  plain links to the entry, showing its title when the link has no text of
  its own, so they work without arod's sidenote script.
- `arod/lib/arod_notes.ml`: a note's page starts with its title and date.
