(lang dune 3.16)

(name ppx_deriving_jsonschema)

(generate_opam_files true)

(source
 (github ahrefs/ppx_deriving_jsonschema))

(authors
 "Louis Roché <louis.roche@ahrefs.com>"
 "Ahrefs <github@ahrefs.com>")

(maintainers
  "Louis Roché <louis.roche@ahrefs.com>"
  "Koonwen Lee <koonwen.lee@ahrefs.com>"
  "Ahrefs <github@ahrefs.com>")

(license MIT)

(documentation "https://ahrefs.github.io/ppx_deriving_jsonschema/")

(package
 (name ppx_deriving_jsonschema)
 (synopsis "Jsonschema generator for ppx_deriving")
 (description
  "ppx_deriving_jsonschema is a ppx rewriter that generates jsonschema from ocaml types")
 (depends
  (ocaml
   (>= "4.08.0"))
  dune
  (ppxlib
   (>= "0.24.0"))
  (melange-json :with-test)
  (melange-json-native :with-test)
  (yojson :with-test)
  (ppx_expect :with-test)
  (ocamlformat :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup))
 (tags
  (jsonschema "org:ahrefs" syntax)))
