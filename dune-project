(lang dune 3.16)

(name ppx_deriving_jsonschema)

(generate_opam_files true)

(source
 (github ahrefs/ppx_deriving_jsonschema))

(authors "ahrefs")

(maintainers "ahrefs")

(license MIT)

(documentation https://github.com/ahrefs/ppx_deriving_jsonschema)

(package
 (name ppx_deriving_jsonschema)
 (synopsis "jsonschema generator for ppx_deriving")
 (description
  "ppx_deriving_jsonschema is a ppx rewriter that generates jsonschema from ocaml types")
 (depends ocaml dune ppxlib)
 (tags
  (jsonschema "org:ahrefs" syntax)))
