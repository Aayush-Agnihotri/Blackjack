module Person_Gen = struct
  let print_large_stick_figure_smile () =
    print_endline "   O   O      ";
    print_endline "   \\___/      ";
    print_endline "     "

  let print_large_stick_figure_nosmile () =
    print_endline "   O   O  ";
    print_endline "    ___    ";
    print_endline "      "

  let print_money num =
    print_endline "*************************************";
    print_endline "* $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ *";
    print_endline ("* $" ^ num ^ "                             " ^ num ^ "$ *");
    print_endline ("* $             " ^ "*****" ^ "           $ *");
    print_endline "* $            *******            $ *";
    print_endline "* $           *********           $ *";
    print_endline ("* $               " ^ num ^ "             $ *");
    print_endline "* $           *********           $ *";
    print_endline "* $            *******            $ *";
    print_endline
      ("* $             " ^ num ^ num ^ num ^ num ^ num ^ "             $ *");
    print_endline ("* $" ^ num ^ "                             " ^ num ^ "$ *");
    print_endline "* $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ *";
    print_endline "*************************************"

  let print_winner () =
    print_endline " W     W   IIIII  N    N  N    N  EEEEE  RRRR   RRRR ";
    print_endline " W     W     I    NN   N  NN   N  E      R   R  R   R";
    print_endline " W  W  W     I    N N  N  N N  N  EEEE   RRRR   RRRR ";
    print_endline " W W W W     I    N  N N  N  N N  E      R  R   R  R ";
    print_endline "  W   W    IIIII  N   NN  N   NN  EEEEE  R   R  R   R"

  let print_loser () =
    print_endline " $$$$   @@@@  rrrr  rrrr  y   y ";
    print_endline "$    $  @  @  r  r  r  r   y  y ";
    print_endline " $$$$   @  @  rrrr  rrrr    y  ";
    print_endline "    $   @  @  r  r  r  r    y  ";
    print_endline " $$$$   @@@@  r  r  r  r    y  "
end
