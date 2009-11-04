open Printf

let content_of_file file =
  let fh = open_in file in
  let buf = Buffer.create 500 in
  try
    while true do
      Buffer.add_string buf (input_line fh);
      Buffer.add_char buf '\n'
    done;
    assert false
  with End_of_file ->
    close_in fh;
    Buffer.contents buf

(* Return the content of [file] if it exists or [None] otherwise. *)
let maybe_content file =
  if Sys.file_exists file then Some(content_of_file file) else None

let () =
  if Array.length Sys.argv <= 1 then (
    eprintf "You must provide one or more files to compile.\n";
    exit 1;
  );
  let files = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in

  Array.iter (fun f ->
                let trailer_ml = maybe_content (f ^ ".ml") in
                let trailer_mli = maybe_content (f ^ ".mli") in
                Template.compile ?trailer_ml ?trailer_mli f;
                printf "Compiled %s\n%!" f
             ) files
