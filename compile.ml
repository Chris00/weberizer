open Printf


let () =
  if Array.length Sys.argv <= 1 then (
    eprintf "You must provide one or more files to compile.\n";
    exit 1;
  );
  let files = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in

  Array.iter (fun f ->
                Template.compile f;
                printf "Compiled %s\n%!" f
             ) files
