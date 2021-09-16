(lang dune 2.0)

(name current-deployer)





(license ISC)

(authors
  "Lucas Pluvinage")

(maintainers
  "Lucas Pluvinage")

(generate_opam_files true)

(package
 (name current-deployer)
 (synopsis "A short, but powerful statement about your project")
 (description "A short, but powerful statement about your project")
 (depends
  (ocaml (>= 4.08.0))
  dune
  (odoc :with-doc)))
