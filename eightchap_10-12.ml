(*8-10*) 
let print_int2 a = output_string stdout (string_of_int a);;
print_int2 12;;

(*8-11*) 
let display_file filename =
  let channel = open_in filename in
  let linenum = ref 0 in
  try
    while true do
      let linestr = input_line channel in
      linenum := !linenum + 1;
      print_string (string_of_int !linenum);
      print_string " ";
      print_endline linestr
    done
  with End_of_file -> close_in channel;;

(*8-12*) 
let cp fname1 fname2 =
  let fc1 = open_in fname1 and fc2 = open_out fname2 in
  try
    while true do
      output_string fc2 (input_line fc1 ^ "/n")
    done
  with End_of_file -> ();
    close_in fc1; close_out fc2;;
