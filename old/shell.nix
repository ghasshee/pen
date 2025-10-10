with import <nixpkgs> {}; 
stdenv.mkDerivation rec {
  name = "OCaml" ; 
  nativeBuildInputs = [gmp zlib pkg-config]; 
  buildInputs = with ocamlPackages; 
  [ 
    lolcat figlet
    gcc gnumake cmake makeWrapper gnum4 glibc pkg-config

    opam camlp4

    gtk2 tk tcl # labltk lablgtk
    zlib gmp    # cryptokit 
  ];
  shellHook = ''
    figlet -tk "OPAM    World" | lolcat; 
    unset OCAMLFIND_DESTDIR; 
    opam switch 4.06.1
    opam install cryptokit rope rpclib batteries hex menhir
    make 
    exit
  '';
}
