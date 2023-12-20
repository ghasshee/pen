with import <nixpkgs> {}; 
stdenv.mkDerivation rec {
  name = "OCaml" ; 
  nativeBuildInputs = [gmp zlib pkgconfig]; 
  buildInputs = with ocamlPackages; 
  [ 
    lolcat figlet
    gcc gnumake cmake makeWrapper gnum4 pkgconfig glibc

    opam camlp4

    gtk2 tk tcl # labltk lablgtk
    zlib gmp    # cryptokit 
  ];
  shellHook = ''
    figlet -tk "OPAM    World" | lolcat; 
    unset OCAMLFIND_DESTDIR; 
    opam switch 4.06.1
    opam install cryptokit rope rpclib batteries hex 
    exit
  '';
}
