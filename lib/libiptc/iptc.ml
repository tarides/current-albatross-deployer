open Ctypes
open Foreign

type handle_t

let handle_t : handle_t structure typ = structure "iptc_handle_t"

let init = foreign "iptc_init" (string @-> returning handle_t) 

let strerror = foreign "iptc_strerror" (int @->  returning string)

let first_chain = foreign "iptc_first_chain" (ptr handle_t @-> returning string_opt)

let next_chain = foreign "iptc_next_chain" (ptr handle_t @-> returning string_opt)

let is_chain = foreign "iptc_is_chain" (string @-> ptr handle_t @-> returning int)

let builtin = foreign "iptc_builtin" (string @-> ptr handle_t @-> returning int)

type in_addr
let in_addr : in_addr structure typ = structure "in_addr"
let s_addr = field in_addr "s_addr" uint32_t
let () = seal in_addr

let ifnamsize = 16 (* Value of IFNAMSIZE taken here: https://code.woboq.org/qt5/include/net/if.h.html *)

type ipt_ip
let ipt_ip : ipt_ip structure typ = structure "ipt_ip"
let src = field ipt_ip "src" in_addr
let dst = field ipt_ip "dst" in_addr
let smsk = field ipt_ip "smsk" in_addr
let dmsk = field ipt_ip "dmsk" in_addr
let iniface = field ipt_ip "iniface" (array ifnamsize char) 
let outiface = field ipt_ip "outiface" (array ifnamsize char)
let iniface_mask = field ipt_ip "iniface_mask" (array ifnamsize uchar)
let outiface_mask = field ipt_ip "outiface_mask" (array ifnamsize uchar)
let proto = field ipt_ip "proto" uint16_t
let flags = field ipt_ip "flags" uint8_t
let invflags = field ipt_ip "invflags" uint8_t
let () = seal ipt_ip

type ipt_counters
let ipt_counters : ipt_counters structure typ = structure "ipt_counters"
let pcnt = field ipt_counters "pcnt" uint64_t
let bcnt = field ipt_counters "bcnt" uint64_t
let () = seal ipt_counters

type ipt_entry
let ipt_entry : ipt_entry structure typ = structure "ipt_entry"
let ip = field ipt_entry "ip" ipt_ip
let nfcache = field ipt_entry "nfcache" uint
let target_offset = field ipt_entry "target_offset" uint16_t
let next_offset = field ipt_entry "next_offset" uint16_t
let comefrom = field ipt_entry "comefrom" uint
let counters = field ipt_entry "counters" ipt_counters
