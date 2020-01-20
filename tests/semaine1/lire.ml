let lire fichier =
  let ic = open_in fichier in
  let line = input_line ic in
  close_in ic;
  line;;