(lang dune 3.3)

(name prettree)

(generate_opam_files true)

(source (github art-w/prettree))

(authors "Arthur Wendling")

(maintainers "art.wendling@gmail.com")

(documentation "https://art-w.github.io/prettree/prettree/Prettree")

(license MIT)

(package
 (name prettree)
 (synopsis "Pretty tree layout")
 (depends
   (ocaml (>= 4.0.8))
   dune
   (graphics :with-test)
  ))
