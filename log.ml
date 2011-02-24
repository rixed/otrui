let ch = open_out "otrui.log"

let p fmt = Printf.fprintf ch (fmt^^"\n%!")

