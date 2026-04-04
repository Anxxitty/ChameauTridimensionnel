(lang dune 3.21)

(name ChameauTridimensionnel)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ChameauTridimensionnel)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
    ocaml
    tgls
    glfw-ocaml
    stb_image)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
