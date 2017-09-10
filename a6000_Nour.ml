(*Langage A6000 : Echantillon

main(integer x) (
     var integer i;
     var integer j;
     var boolean continue;

     continue := 1 < 2;
     i := 0;

     while continue (
       j := 0;
       while ((i*i) + (j*j)) < (x*x) (
         print(46);
         print(32);
         j := j+1;
       );
       continue := 0 < j;
       while j < (x+1) (
         print(35);
         print(32);
         j := j+1;
       );
       print(10);
       i := i+1;
     );
   )


*)

type token =
	|MAIN 
	|BEGIN |END (*(,)*)
	|IDENT of string
	|INT of int 
	|VAR 
	|INTERGER |BOOLEAN 
	|SEMI (* ; *)
	|PRINT |WHILE 
	|SET (* := *)
	|ADD |MULT |LT (* < *)
	|EOF |BOF (* fin de fichier, début de fichier *)

type input_buffer = {
    input: string;
    length: int;
    mutable start_pos: int;
    mutable next_pos: int;
 }

 let init_buffer s = {
     input = s;
     length = String.length s;
     start_pos = 0;
     next_pos = 0;
 }

(*Lire le caractère courant*)
 let next_char b = 
     if b.next_pos < b.length
     then b.input.[b.next_pos]
     else '\000' (* Fin du fichier *)
 
 (* Faire avancer le curseur *)
 let shift b = b.next_pos <- b.next_pos + 1

  (* Marquer le lexème en cours d'analyse *)
  let init_pos b = b.start_pos <- b.next_pos

  (* Obtenir le lexème analysé *)
  let current_word b = 
      String.sub b.input b.start_pos (b.next_pos - b.start_pos)

  (* L'automate *)
  let rec read_token b =
      match next_char with 
      (* les cas faciles, un seul caractère *)
          |'(' -> shift b; BEGIN
          |')' -> shift b; END
          |'+' -> shift b; ADD
          |'*' -> shift b; MULT
          |'<' -> shift b; LT
          |';' -> shift b; SEMI
      (* Cas particuliers de la fin de fichier *)
          |'\000' -> EOF (* On ne shift plus, c'est la fin *)
          
