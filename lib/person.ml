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
    print_endline ("* $             " ^ "******" ^ "             $ *");
    print_endline ("* $" ^ num ^ "                             " ^ num ^ "$ *");
    print_endline "* $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ *";
    print_endline "*************************************"

  let print_winner () =
    print_endline "  CCCC   OOO   N   N   GGGG  RRRR    A    TTTTT  SSSS ";
    print_endline " C      O   O  NN  N  G      R   R  A A     T    S    ";
    print_endline " C      O   O  N N N  G  GG  RRRR   AAAA    T     SSS ";
    print_endline " C      O   O  N  NN  G   G  R  R  A   A    T        S";
    print_endline "  CCCC   OOO   N   N   GGG   R   R  A   A   T    SSSS ";
    print_endline "";
    print_endline " Y   Y   oooo   u    u ";
    print_endline "  Y Y   o   o  u    u ";
    print_endline "   Y    o   o  u    u ";
    print_endline "   Y    o   o  u    u ";
    print_endline "   Y     oooo   uuuuu  ";
    print_endline "";
    print_endline " A    RRRR   EEEEE";
    print_endline "A A   R   R  E    ";
    print_endline "AAA   RRRR   EEEE ";
    print_endline "A A   R  R   E    ";
    print_endline "A A   R   R  EEEEE";
    print_endline "";
    print_endline "   A   ";
    print_endline "  A A  ";
    print_endline " AAAA ";
    print_endline " A   A";
    print_endline "A   A";
    print_endline "";
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
    print_endline " $$$$   @@@@  r  r  r  r    y  ";
    print_endline "";
    print_endline " Y   Y   oooo   u    u ";
    print_endline "  Y Y   o   o  u    u ";
    print_endline "   Y    o   o  u    u ";
    print_endline "   Y    o   o  u    u ";
    print_endline "   Y     oooo   uuuuu  ";
    print_endline "";
    print_endline "L     OOO   SSSS  TTTTT";
    print_endline "L     O   O  S       T  ";
    print_endline "L     O   O   SSS    T  ";
    print_endline "L     O   O      S   T  ";
    print_endline "LLLL   OOO   SSSS    T  "
end
