open Player 
open Cpu

module Player1= Player
module Cpu1 = CPU
module Cpu2 = CPU
module Cpu3 = CPU
module Cpu4 = CPU 


let suminit= Player1.sum
let suminitCpu = Cpu1.sum
module Game = struct
let rec addToPlayer1 () =
  if Player1.get_boolean_input () then
    begin
      Player1.add_to_sum 4;
      addToPlayer1 ()
    end

let rec addToCpu1 () =
  if Cpu1.check_more then
    begin
      Cpu1.add_to_sum 4;
        addToCpu1 ()
    end

let rec addToCpu2 () =
  if Cpu2.check_more then
    begin
      Cpu2.add_to_sum 4;
        addToCpu2 ()
    end

let rec addToCpu3 () =
  if Cpu3.check_more then
    begin
      Cpu3.add_to_sum 4;
        addToCpu3 ()
    end

let rec addToCpu4 () =
  if Cpu4.check_more then
    begin
      Cpu4.add_to_sum 4;
        addToCpu4 ()
    end
  end