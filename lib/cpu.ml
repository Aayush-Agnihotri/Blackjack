module type CPUprofile = sig
  val sum : int ref
  val add_to_sum : int -> unit
  val check_more : bool
end

module CPU : CPUprofile = struct
  let sum = ref 0
  let add_to_sum (num : int) = sum := !sum + num
  let check_more = if !sum < 17 then true else false
end
