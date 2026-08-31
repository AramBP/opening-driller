(lang dune 3.4)
(name opening-driller)
(generate_opam_files true)
(authors "Aram Brouwer")
(package
  (name opening-driller)
  (depends 
    (ppx_deriving (>= 6.1.3))
    (ppx_blob (>= 0.9.0))
    (core (>= 0.17))
    (raylib (>= 2.2.1))
  ))
