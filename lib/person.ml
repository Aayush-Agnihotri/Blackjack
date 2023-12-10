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
    print_endline
      ("* $             " ^ num ^ num ^ num ^ num ^ num ^ "             $ *");
    print_endline "* $            *******            $ *";
    print_endline "* $           *********           $ *";
    print_endline "* $          ***********          $ *";
    print_endline "* $           *********           $ *";
    print_endline "* $            *******            $ *";
    print_endline
      ("* $             " ^ num ^ num ^ num ^ num ^ num ^ "             $ *");
    print_endline ("* $" ^ num ^ "                             " ^ num ^ "$ *");
    print_endline "* $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ *";
    print_endline "*************************************"
end
