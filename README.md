# projetPFA
//lance d'aboard
opam switch 4.07.1
eval $(opam env)
cd Versions/version_final

//executer le programe
ocamlc graphics.cma ig.ml board.ml lire.ml grtest1.ml -o grtest1
./grtest1
