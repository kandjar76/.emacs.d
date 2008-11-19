;;;; spu-tooltip.el -- Popup a tooltip above the opcode.
;;
;; Author: Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: spu highlight minor mode opcode
;; Description: File to highlight registers of the current instruction in the spu mode.
;; Tested with: GNU Emacs 21.x and GNU Emacs 22.x
;;
;; This file is *NOT* part of GNU Emacs.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Display a popup window above the current opcode when the cursor is above
;; Press F1 to display the popup
;;
;; The SPU instruction documentation is based on IBM docs which can be found at
;; the following address: 
;;
;;   http://www-01.ibm.com/chips/techlib/techlib.nsf/techdocs/76CA6C7304210F398
;;   7257060006F2C44/$file/SPU_ISA_v1.2_27Jan2007_pub.pdf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tooltip-help)

;;
;;
;; Documentation data:
;;
;;

(setq spu-opcode-help (list (list "nop"
				  "NOP"
				  "1"
				  "??"
				  "nop instruction")
			    (list "lnop"
				  "LNOP"
				  "1"
				  "??"
				  "nop instruction")
			    (list "br"
				  "BR brTo"
				  "4"
				  "BR"
				  "goto label addresses in words")
			    (list "brsl"
				  "BRSL	rt, brTo"
				  "4"
				  "BR"
				  "gosub (rt.w[0] set with return address, rest cleared)")
			    (list "brhnz"
				  "BRHNZ rt, brTo"
				  "4"
				  "BR"
				  "branch if rt.h[1] != 0")
			    (list "brhz"
				  "BRHZ	rt, brTo"
				  "4"
				  "BR"
				  "branch if rt.h[1] == 0")
			    (list "brnz"
				  "BRNZ	rt, brTo"
				  "4"
				  "BR"
				  "branch if rt.w[0] != 0")
			    (list "brz"
				  "BRZ rt, brTo"
				  "4"
				  "BR"
				  "branch if rt.w[0] == 0")
			    (list "hbrr"
				  "HBRR	brFrom, brTo"
				  "15"
				  "BR"
				  "branch hint for any BRxxx type branch")
			    (list "bi"
				  "BI rt"
				  "?"
				  "BR"
				  "goto rt  register addresses in bytes")
			    (list "bisl"
				  "BISL	rt, ra"
				  "4"
				  "BR"
				  "gosub ra (rt.w[0] set with return address, rest cleared)")
			    (list "bisled"
				  "BISLED rt, ra"
				  "4"
				  "BR"
				  "gosub ra if channel zero is non-zero\n(rt.w[0] set with return address, rest cleared)")
			    (list "bihnz"
				  "BIHNZ rt, ra"
				  "?"
				  "BR"
				  "branch to ra.w[0] if rt.h[1] != 0")
			    (list "bihz"
				  "BIHZ	rt, ra"
				  "?"
				  "BR"
				  "branch to ra.w[0] if rt.h[1] == 0")
			    (list "binz"
				  "BINZ	rt, ra"
				  "?"
				  "BR"
				  "branch to ra.w[0] if rt.w[0] != 0")
			    (list "biz"
				  "BIZ rt, ra"
				  "?"
				  "BR"
				  "branch to ra.w[0] if rt.w[0] == 0")
			    (list "hbr"
				  "HBR brFrom, ra"
				  "15"
				  "BR"
				  "branch hint for any BIxxx type branch")
			    (list "hbrp"
				  "HBRP	label, ra"
				  "15"
				  "BR"
				  "inline prefetch?  What?  label and rt are ignored.")
			    (list "bra"
				  "BRA brTo"
				  ""
				  "BR"
				  "goto label addresses in words")
			    (list "brasl"
				  "BRASL rt, brTo"
				  "4"
				  "BR"
				  "gosub (rt.w[0] set with return address, rest cleared)")
			    (list "hbra"
				  "HBRA	brFrom, brTo"
				  "15"
				  "BR"
				  "branch hint for any BRAxx type branch")
			    (list "heq"
				  "HEQ rt, ra"
				  "2"
				  "FX"
				  "halt if rt == ra")
			    (list "heqi"
				  "HEQI	rt, s10"
				  "2"
				  "FX"
				  "halt if rt == ext(s10)")
			    (list "hgt"
				  "HGT rt, ra"
				  "2"
				  "FX"
				  "halt if rt > ra -- signed")
			    (list "hgti"
				  "HGTI	rt, s10"
				  "2"
				  "FX"
				  "halt if rt > ext(s10) -- signed")
			    (list "hlgt"
				  "HLGT	rt, ra"
				  "2"
				  "FX"
				  "halt if rt > ra -- unsigned ('Logical')")
			    (list "hlgti"
				  "HLGTI rt, s10"
				  "2"
				  "FX"
				  "halt if rt > ext(s10) -- unsigned, even though sign-extended")
			    (list "stop"
				  "STOP"
				  "1"
				  "BR"
				  "stop instruction")
			    (list "fa"
				  "FA a, b, c"
				  "6"
				  "SP"
				  "a.f[n] = b.f[n] + c.f[n]")
			    (list "fs"
				  "FS a, b, c"
				  "6"
				  "SP"
				  "a.f[n] = b.f[n] - c.f[n]")
			    (list "fm"
				  "FM a, b, c"
				  "6"
				  "SP"
				  "a.f[n] = b.f[n] * c.f[n]")
			    (list "fma"
				  "FMA a, b, c, d"
				  "6"
				  "SP"
				  "a.f[n] = b.f[n] * c.f[n] + d.f[n]")
			    (list "fms"
				  "FMS a, b, c, d"
				  "6"
				  "SP"
				  "a.f[n] = b.f[n] * c.f[n] - d.f[n]")
			    (list "fnms"
				  "FNMS a, b, c, d"
				  "6"
				  "SP"
				  "a.f[n] = -(b.f[n] * c.f[n] - d.f[n])\nSubtract then Negate")
			    (list "dfa"
				  "DFA a, b, c"
				  "?"
				  "SP"
				  "a = b + c")
			    (list "dfs"
				  "DFS a, b, c"
				  "?"
				  "SP"
				  "a = b - c")
			    (list "dfm"
				  "DFM a, b, c"
				  "?"
				  "SP"
				  "a = b * c")
			    (list "dfma"
				  "DFMA a, b, c"
				  "?"
				  "SP"
				  "a = a + b * c")
			    (list "dfms"
				  "DFMS a, b, c"
				  "?"
				  "SP"
				  "a = -a + b * c")
			    (list "dfnma"
				  "DFNMA a, b, c"
				  "?"
				  "SP"
				  "a = -( a + b * c )\nAdd then Negate")
			    (list "dfnms"
				  "DFNMS a, b, c"
				  "?"
				  "SP"
				  "a = -(-a + b * c )\nSubtract then Negate")
			    (list "lqa"
				  "LQA	rt, label18"
				  "6"
				  "LS"
				  "addr = label18 & ~3\nrt = *addr\n
The value label18 (with the 2 least significant bits cleared) is used as the
local storage address. The 16 bytes at the local storage address are loaded 
into register RT.")
			    (list "lqd"
				  "LQD rt, s14(RA)"
				  "6"
				  "LS"
				  "addr = s14 & ~15 + ra.w[0]\nrt = *addr\n
The local storage address is computed by adding the signed value s14, with the 
4 least significant bits cleared, to the value in the preferred slot of 
register RA and forcing the rightmost 4 bits of the sum to zero. The 16 bytes 
at the local storage address are placed into register RT.")
			    (list "lqr"
				  "LQR rt, label18"
				  "6"
				  "LS"
				  "addr = label18 & ~3 + PC\nrt = *addr\n
The value label, with the 2 least significant bits cleared, is added to the 
program counter (PC) to form the local storage address. The 16 bytes at the
local storage address are loaded into register RT.")
			    (list "lqx"
				  "LQX rt, ra, rb"
				  "6"
				  "LS"
				  "addr = ra.w[0] + rb.w[0]\nrt = *addr\n
The local storage address is computed by adding the value in the preferred slot
of register RA to the value in the preferred slot of register RB and forcing 
the rightmost 4 bits of the sum to zero. The 16 bytes at the local storage 
address are placed into register RT.")
			    (list "stqa"
				  "STQA rt, label18"
				  "6"
				  "LS"
				  "addr = label18 & ~3\n*addr = rt\n
The value label18 (with the 2 least significant bits cleared) is used as the
local storage address. The contents of register RT are stored at the location 
given by the local storage address.")
			    (list "stqd"
				  "STQD rt, s14(ra)"
				  "6"
				  "LS"
				  "addr = s14 & ~15 + ra.w[0]\n*addr = rt\n
The local storage address is computed by adding the signed value s14, with the
4 least significant bits cleared, to the value in the preferred slot of 
register RA and forcing the rightmost 4 bits of the sum to zero. The contents 
of register RT are stored at the local storage address.")
			    (list "stqr"
				  "STQR rt, label18"
				  "6"
				  "LS"
				  "addr = label18 & ~3 + PC\n*addr = rt\n
The value label, with the 2 least significant bits cleared, is added to the 
program counter (PC) to form the local storage address. The contents of 
register RT are stored at the location given by the local storage address.")
			    (list "stqx"
				  "STQX rt, ra, rb"
				  "6"
				  "LS"
				  "addr = ra.w[0] + rb.w[0]\n*addr = rt\n
The local storage address is computed by adding the value in the preferred slot
of register RA to the value in the preferred slot of register RB and forcing
the rightmost 4 bits of the sum to zero. The contents of register RT are stored
at the local storage address.")
			    (list "cntb"
				  "CNTB	rt, ra"
				  "4"
				  "BO"
				  "rt.b[n] = numOneBits( ra.b[n] )\n
For each of 16 byte slots:
. The number of bits in register RA whose value is '1' is computed.
. The result is placed in register RT.

The result placed in register RT satisfies 0 <= RT <= 8. The value in register
RT is zero, for example, if the value in RA is zero. The value in RT is 8 if 
the value in RA is -1.

for ra = 0 to 15
   c = 0
   b = RA.b[ra]
   For m = 0 to 7
     If b.bit[m] = 1 then c = c + 1
   end
   RT.b[ra] = c
end")
			    (list "avgb"
				  "AVGB	rt, ra, rb"
				  "4"
				  "BO"
				  "rt.b[n] = ( ra.b[n] + rb.b[n] + 1 ) / 2\n
For each of 16 byte slots:
. The operand from register RA is added to the operand from register RB, and 
'1' is added to the result. These additions are done without loss of 
precision.
. That result is shifted to the right by 1 bit and placed in register RT.")
			    (list "absdb"
				  "ABSDB rt, ra, rb"
				  "4"
				  "BO"
				  "rt.b[n] = abs( ra.b[n] - rb.b[n] )\n
For each of 16 byte slots:
. The operand in register RA is subtracted from the operand in register RB.
. The absolute value of the result is placed in register RT.")
			    (list "sumb"
				  "SUMB	rt, ra, rb"
				  "4"
				  "BO"
				  "rt.h[n] = ( n & 1 == 0 ) ? sum(ra.b[2n..2n+3]) : sum(rb.b[2n-2..2n+1])\n
For each of four word slots:
. The 4 bytes in register RB are added, and the 16-bit result is placed in 
bytes 0 and 1 of register RT.
. The 4 bytes in register RA are added, and the 16-bit result is placed in
bytes 2 and 3 of register RT.")
			    (list "shlh"
				  "SHLH	rt, ra, rb"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] << ( rb.h[n] & 0x1F )")
			    (list "shlhi"
				  "SHLHI rt, ra, imm"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] << ( imm & 0x1F )")
			    (list "shl"
				  "SHL rt, ra, rb"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] << ( rb.w[n] & 0x3F )")
			    (list "shli"
				  "SHLI rt, ra, imm"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] << ( imm & 0x3F )")
			    (list "roth"
				  "ROTH	rt, ra, rb"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] <^ ( rb.h[n] & 0x0F )\n<^ is an idiosyncratic symbol for rotate")
			    (list "rothi"
				  "ROTHI rt, ra, imm"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] <^ ( imm & 0x0F )\n<^ is an idiosyncratic symbol for rotate")
			    (list "rot"
				  "ROT rt, ra, rb"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] <^ ( rb.w[n] & 0x1F )\n<^ is an idiosyncratic symbol for rotate")
			    (list "roti"
				  "ROTI rt, ra, imm"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] <^ ( imm & 0x1F )\n<^ is an idiosyncratic symbol for rotate")
			    (list "rothm"
				  "ROTHM rt, ra, rb"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] >> ( -rb.h[n] & 0x1F )\nshift right logical")
			    (list "rothmi"
				  "ROTHMI rt, ra, imm"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] >> ( -imm & 0x1F )")
			    (list "rotm"
				  "ROTM rt, ra, rb"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] >> ( -rb.w[n] & 0x3F )")
			    (list "rotmi"
				  "ROTMI rt, ra, imm"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] >> ( -imm & 0x3F )")
			    (list "rotmah"
				  "ROTMAH rt, ra, rb"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] >> ( -rb.h[n] & 0x1F )\nshift right arithmetic")
			    (list "rotmahi"
				  "ROTMAHI rt, ra, imm"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] >> ( -imm & 0x1F )")
			    (list "rotma"
				  "ROTMA rt, ra, rb"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] >> ( -rb.w[n] & 0x3F )")
			    (list "rotmai"
				  "ROTMAI rt, ra, imm"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] >> ( -imm & 0x3F )")
			    (list "mpy"
				  "MPY rt, ra, rb"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n+1] * rb.h[2n+1]\nmultiply lower halves signed\n
For each of four word slots:
. The value in the rightmost 16 bits of register RA is multiplied by the value
in the rightmost 16 bits of register RB.
. The 32-bit product is placed in register RT.
. The leftmost 16 bits of each operand are ignored.")
			    (list "mpyi"
				  "MPYI rt, ra, s10"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n+1] * ext(s10)\nmultiply lower halves signed immediate\n
For each of four word slots:
. The signed value s10 is multiplied by the value in the rightmost 16 bits
of register RA.
. The resulting product is placed in register RT.")
			    (list "mpyu"
				  "MPYU rt, ra, rb"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n+1] * rb.h[2n+1]\nmultiply lower halves unsigned\n
For each of four word slots:
. The rightmost 16 bits of register RA are multiplied by the rightmost 16 bits 
of register RB, treating both operands as unsigned.
. The 32-bit product is placed in register RT.")
			    (list "mpyui"
				  "MPYUI rt, ra, s10"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n+1] * ext(s10)\nmultiply lower halves unsigned immediate\n(even though immediate is sign-extended)\n
For each of four word slots:
. The signed value s10 is extended to 16 bits by replicating the leftmost bit. 
The resulting value is multiplied by the rightmost 16 bits of register RA, 
treating both operands as unsigned.
. The resulting product is placed in register RT.")
			    (list "mpya"
				  "MPYA rt, ra, rb, rc"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n+1] * rb.h[2n+1] + rc.w[n]\nmultiply lower halves, add word\n
For each of four word slots:
. The value in register RA is treated as a 16-bit signed integer and multiplied
by the 16-bit signed value in register RB. The resulting product is added to 
the value in register RC.
. The result is placed in register RT.
. Overflows and carries are not detected.")
			    (list "mpys"
				  "MPYS rt, ra, rb"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n+1] * rb.h[2n+1] >> 16\nmultiply lower halves, shift result down 16 with sign extend\n
For each of four word slots:
. The value in the rightmost 16 bits of register RA is multiplied by the value
in the rightmost 16 bits of register RB.
. The leftmost 16 bits of the 32-bit product are placed in the rightmost 16 
bits of register RT, with the sign bit replicated into the left 16 bits of the
register.")
			    (list "mpyh"
				  "MPYH rt, ra, rb"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n] * rb.h[2n+1] << 16\nmultiply upper half ra by lower half rb, shift up 16\n
For each of four word slots:
. The leftmost 16 bits of the value in register RA are shifted right by 16 bits
and multiplied by the 16-bit value in register RB.
. The product is shifted left by 16 bits and placed in register RT. Bits 
shifted out at the left are discarded. Zeros are shifted in at the right.

This instruction can be used in conjunction with mpyu and Add Word (a) to
perform a 32-bit multiply. A 32-bit multiply instruction, mpy32 rt,ra,rb, can 
be emulated with the following instruction sequence:
    mpyh t1,ra,rb
    mpyh t2,rb,ra
    mpyu t3,ra,rb
    a    rt,t1,t2
    a    rt,rt,t3")
			    (list "mpyhh"
				  "MPYHH rt, ra, rb"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n] * rb.h[2n]\nmultiply upper halves signed\n
For each of four word slots:
. The leftmost 16 bits in register RA are multiplied by the leftmost 16 bits
in register RB.
. The 32-bit product is placed in register RT.")
			    (list "mpyhhu"
				  "MPYHHU rt, ra, rb"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n] * rb.h[2n]\nmultiply upper halves unsigned\n
For each of four word slots:
. The leftmost 16 bits in register RA are multiplied by the leftmost 16 bits in
register RB, treating both operands as unsigned.
. The 32-bit product is placed in register RT.")
			    (list "mpyhha"
				  "MPYHHA rt, ra, rb"
				  "7"
				  "FI"
				  "rt.w[n] += ra.h[2n] * rb.h[2n]\nmultiply/accumulate upper halves\n
For each of four word slots:
. The leftmost 16 bits in register RA are multiplied by the leftmost 16 bits in
register RB. The product is added to the value in register RT.
. The sum is placed in register RT.")
			    (list "mpyhhau"
				  "MPYHHAU rt, ra, rb"
				  "7"
				  "FI"
				  "rt.w[n] += ra.h[2n] * rb.h[2n]\nmultiply/accumulate upper halves unsigned\n
For each of four word slots:
. The leftmost 16 bits in register RA are multiplied by the leftmost 16 bits in
register RB, treating both operands as unsigned. The product is added to the 
value in register RT.
. The sum is placed in register RT.")
			    (list "fi"
				  "FI a, b, c"
				  "7"
				  "FI"
				  "use after FREST or FRSQEST")
			    (list "cuflt"
				  "CUFLT a, ra, precis"
				  "7"
				  "FI"
				  "unsigned int to float, specify precision")
			    (list "csflt"
				  "CSFLT a, ra, precis"
				  "7"
				  "FI"
				  "signed int to float, specify precision")
			    (list "cfltu"
				  "CFLTU rt, b, precis"
				  "7"
				  "FI"
				  "float to unsigned int, specify precision")
			    (list "cflts"
				  "CFLTS rt, b, precis"
				  "7"
				  "FI"
				  "float to signed int, specify precision")
			    (list "fesd"
				  "FESD	a, b"
				  "?"
				  "FI"
				  "a.d[n] = b.w[2n+1]\nsingle to double")
			    (list "frds"
				  "FRDS	a, b"
				  "?"
				  "FI"
				  "a.w[2n] = b.d[n]; a.w[2n+1] = 0\ndouble to single")
			    (list "ceqb"
				  "CEQB	rt, ra, rb"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] == rb.b[n] ) ? FF : 00")
			    (list "ceqbi"
				  "CEQBI rt, ra, su8"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] == su8 ) ? FF : 00")
			    (list "cgtb"
				  "CGTB rt, ra, rb"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] > rb.b[n] ) ? FF : 00\nsigned")
			    (list "cgtbi"
				  "CGTBI rt, ra, su8"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] > su8 ) ? FF : 00")
			    (list "clgtb"
				  "CLGTB rt, ra, rb"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] > rb.b[n] ) ? FF : 00\nunsigned")
			    (list "clgtbi"
				  "CLGTBI rt, ra, su8"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] > su8 ) ? FF : 00")
			    (list "ceqh"
				  "CEQH rt, ra, rb"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] == rb.h[n] ) ? FFFF : 0000")
			    (list "ceqhi"
				  "CEQHI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] == ext(s10) ) ? FFFF : 0000")
			    (list "cgth"
				  "CGTH rt, ra, rb"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] > rb.h[n] ) ? FFFF : 0000\nsigned")
			    (list "cgthi"
				  "CGTHI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] > ext(s10) ) ? FFFF : 0000")
			    (list "clgth"
				  "CLGTH rt, ra, rb"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] > rb.h[n] ) ? FFFF : 0000\nunsigned")
			    (list "clgthi"
				  "CLGTHI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] > ext(s10) ) ? FFFF : 0000")
			    (list "ceq"
				  "CEQ rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] == rb.w[n] ) ? FFFFFFFF : 00000000")
			    (list "ceqi"
				  "CEQI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] == ext(s10) ) ? FFFFFFFF : 00000000")
			    (list "cgt"
				  "CGT rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] > rb.w[n] ) ? FFFFFFFF : 00000000\nsigned")
			    (list "cgti"
				  "CGTI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] > ext(s10) ) ? FFFFFFFF : 00000000")
			    (list "clgt"
				  "CLGT rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] > rb.w[n] ) ? FFFFFFFF : 00000000\nunsigned")
			    (list "clgti"
				  "CLGTI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] > ext(s10) ) ? FFFFFFFF : 00000000")
			    (list "fceq"
				  "FCEQ	rt, b, c"
				  "2"
				  "FX"
				  "rt.w[n] = ( b[n] == c[n] ) ? FFFFFFFF : 00000000")
			    (list "fcmeq"
				  "FCMEQ rt, b, c"
				  "2"
				  "FX"
				  "rt.w[n] = ( abs(b[n]) == abs(c[n]) ) ? FFFFFFFF : 00000000")
			    (list "fcgt"
				  "FCGT	rt, b, c"
				  "2"
				  "FX"
				  "rt.w[n] = ( b[n] > c[n] ) ? FFFFFFFF : 00000000")
			    (list "fcmgt"
				  "FCMGT rt, b, c"
				  "2"
				  "FX"
				  "rt.w[n] = ( abs(b[n]) > abs(c[n]) ) ? FFFFFFFF : 00000000")
			    (list "and"
				  "AND rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ra & rb\n
The values in register RA and register RB are logically ANDed. The result is 
placed in register RT.")
			    (list "nand"
				  "NAND rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ~(ra & rb)\n
For each of four word slots:
. The complement of the AND of the bit in register RA and the bit in register
RB is placed in register RT.")
			    (list "andc"
				  "ANDC rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ra & ~rb\n
The value in register RA is logically ANDed with the complement of the value
in register RB. The result is placed in register RT.")
			    (list "andbi"
				  "ANDBI rt, ra, u8"
				  "2"
				  "FX"
				  "rt.b[n] = ra.b[n] & u8\n
For each of 16 byte slots, the 8 bits of value u8 are ANDed with the value in 
register RA. The result is placed in register RT.")
			    (list "andhi"
				  "ANDHI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.h[n] = ra.h[n] & ext(s10)\n
For each of eight halfword slots:
. The value s10 is extended to 16 bits by replicating its leftmost bit. The 
result is ANDed with the value in register RA.
. The 16-bit result is placed in register RT.")
			    (list "andi"
				  "ANDI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.w[n] = ra.w[n] & ext(s10)\n
For each of four word slots:
. The value s10 is extended to 32 bits by replicating its leftmost bit. The
result is ANDed with the contents of register RA.
. The result is placed in register RT.")
			    (list "or"
				  "OR rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ra | rb\n
The values in register RA and register RB are logically ORed. The result is
placed in register RT.")
			    (list "nor"
				  "NOR rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ~(ra | rb)\n
For each of four word slots:
. The values in register RA and register RB are logically ORed.
. The result is complemented and placed in register RT.")
			    (list "orc"
				  "ORC rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ra | ~rb\n
The value in register RA is ORed with the complement of the value in register 
RB. The result is placed in register RT.")
			    (list "orbi"
				  "ORBI rt, ra, u8"
				  "2"
				  "FX"
				  "rt.b[n] = ra.b[n] | u8\n
For each of 16 byte slots:
. The rightmost 8 bits of the value u8 are ORed with the value in register RA.
. The result is placed in register RT.")
			    (list "orhi"
				  "ORHI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.h[n] = ra.h[n] | ext(s10)\n
For each of eight halfword slots:
. The value s10 is extended to 16 bits by replicating its leftmost bit. The 
result is ORed with the value in register RA.
. The result is placed in register RT.")
			    (list "ori"
				  "ORI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.w[n] = ra.w[n] | ext(s10)\n
For each of four word slots:
. The value s10 is sign-extended to 32 bits and ORed with the contents of
register RA.
. The result is placed in register RT.")
			    (list "xor"
				  "XOR rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ra ^ rb\n
The values in register RA and register RB are logically XORed. The result
is placed in register RT.")
			    (list "eqv"
				  "EQV rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ~(ra ^ rb)\nEQV is 'NXOR'\nAlso, the missing 'XORC' would give the same result as EQV
For each of four word slots:
. If the bit in register RA and register RB are the same, the result is '1'; otherwise, the result is '0'.
. The result is placed in register RT.")
			    (list "xorbi"
				  "XORBI   rt, ra, u8"
				  "2"
				  "FX"
				  "rt.b[n] = ra.b[n] ^ u8\n
For each of 16 byte slots:
. The rightmost 8 bits of the value u8 are XORed with the value in register RA.
. The result is placed in register RT.")
			    (list "xorhi"
				  "XORHI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.h[n] = ra.h[n] ^ ext(s10)\n
For each of eight halfword slots:
. The value s10 is extended to 16 bits by replicating the leftmost bit. The 
resulting value is XORed with the value in register RA.
. The 16-bit result is placed in register RT.")
			    (list "xori"
				  "XORI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.w[n] = ra.w[n] ^ ext(s10)\n
For each of four word slots:
. The I10 field is sign-extended to 32 bits and XORed with the contents of register RA.
. The 32-bit result is placed in register RT.")
			    (list "il"
				  "IL rt, s16"
				  "2"
				  "FX"
				  "rt.w[n] = ext(s16)\n
For each of four word slots:
. The value s16 is expanded to 32 bits by replicating the leftmost bit.
. The resulting value is placed in register RT.")
			    (list "ilh"
				  "ILH rt, s16"
				  "2"
				  "FX"
				  "rt.h[n] = s16\n
For each of eight halfword slots:
. The value s16 is placed in register RT.")
			    (list "ila"
				  "ILA rt, u18"
				  "2"
				  "FX"
				  "rt.w[n] = u18\n
For each of four word slots:
. The value u18 is placed unchanged in the rightmost 18 bits of register RT.
. The remaining bits of register RT are set to zero.")
			    (list "ilhu"
				  "ILHU	rt, s16"
				  "2"
				  "FX"
				  "rt.w[n] = s16 << 16\n
For each of four word slots:
. The value s16 is placed in the leftmost 16 bits of the word.
. The remaining bits of the word are set to zero.")
			    (list "iohl"
				  "IOHL	rt, u16"
				  "2"
				  "FX"
				  "rt.w[n] |= u16\n
For each of four word slots:
. The value u16 is prefaced with zeros and ORed with the value in register RT.
. The result is placed into register RT.")
			    (list "ah"
				  "AH rt, ra, rb"
				  "2"
				  "FX"
				  "rt.h[n] = ra.h[n] + rb.h[n]\n
For each of eight halfword slots:
. The operand from register RA is added to the operand from register RB.
. The 16-bit result is placed in RT.
. Overflows and carries are not detected.")
			    (list "ahi"
				  "AHI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.h[n] = ra.h[n] + ext(s10)\n
For each of eight halfword slots:
. The signed value s10 is added to the value in register RA.
. The 16-bit result is placed in RT.
. Overflows and carries are not detected.")
			    (list "a"
				  "A rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ra.w[n] + rb.w[n]\n
For each of four word slots:
. The operand from register RA is added to the operand from register RB.
. The 32-bit result is placed in register RT.
. Overflows and carries are not detected.")
			    (list "ai"
				  "AI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.w[n] = ra.w[n] + ext(s10)\n
For each of four word slots:
. The signed value s10 is added to the operand in register RA.
. The 32-bit result is placed in register RT.
. Overflows and carries are not detected.")
			    (list "sfh"
				  "SFH rt, ra, rb"
				  "2"
				  "FX"
				  "rt.h[n] = -ra.h[n] + rb.h[n]\n
For each of eight halfword slots:
. The value in register RA is subtracted from the value in RB.
. The 16-bit result is placed in register RT.
. Overflows and carries are not detected.")
			    (list "sfhi"
				  "SFHI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.h[n] = -ra.h[n] + ext(s10)\n
For each of eight halfword slots:
. The value in register RA is subtracted from the signed value s10.
. The 16-bit result is placed in register RT.
. Overflows are not detected.")
			    (list "sf"
				  "SF rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = -ra.w[n] + rb.w[n]\n
For each of four word slots:
. The value in register RA is subtracted from the value in register RB.
. The result is placed in register RT.
. Overflows and carries are not detected.")
			    (list "sfi"
				  "SFI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.w[n] = -ra.w[n] + ext(s10)\n
For each of four word slots:
. The value in register RA is subtracted from the value s10.
. The result is placed in register RT.
. Overflows and carries are not detected.")
			    (list "bg"
				  "BG rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( -ra.w[n] + rb.w[n] ) < 0 ? 0 : 1\ngenerate borrow bit\n
For each of four word slots:
. If the unsigned value of RA is greater than the unsigned value of RB, then 
'0' is placed in register RT. Otherwise, '1' is placed in register RT.")
			    (list "bgx"
				  "BGX rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( -ra.w[n] + rb.w[n] + ( rt.w[n] & 1 ) - 1 ) < 0 ? 0 : 1\ngenerate borrow bit with borrow\n
For each of four word slots:
. The operand from register RA is subtracted from the operand from register RB.
An additional '1' is subtracted from the result if the least-significant bit of
RT is '0'. If the result is less than zero, a '0' is placed in register RT. 
Otherwise, register RT is set to '1'. 
. Bits 0 to 30 of the RT input are reserved and should be zero.")
			    (list "sfx"
				  "SFX rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( -ra.w[n] + rb.w[n] + ( rt.w[n] & 1 ) - 1 )\nsubtract with borrow bit\n
For each of four word slots:
. The operand from register RA is subtracted from the operand from register RB.
An additional '1' is subtracted from the result if the least-significant bit of
RT is '0'.
. The 32-bit result is placed in register RT. 
. Bits 0 to 30 of the RT input are reserved and should be zero.")
			    (list "cg"
				  "CG rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] + rb.w[n] ) > 0xFFFFFFFF ? 1 : 0\ngenerate carry bit\n
For each of four word slots:
. The operand from register RA is added to the operand from register RB.
. The carry-out is placed in the least-significant bit of register RT.
. The remaining bits of RT are set to zero.")
			    (list "cgx"
				  "CGX rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] + rb.w[n] + ( rt.w[n] & 1 ) ) > 0xFFFFFFFF ? 1 : 0\ngenerate carry bit with carry\n
For each of four word slots:
. The operand from register RA is added to the operand from register RB and the
least-significant bit of register RT.
. The carry-out is placed in the least-significant bit of register RT.
. The remaining bits of RT are set to zero. 
. Bits 0 to 30 of the RT input are reserved and should be zero.")
			    (list "addx"
				  "ADDX	rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] + rb.w[n] + ( rt.w[n] & 1 ) )\nadd with carry bit\n
For each of four word slots:
. The operand from register RA is added to the operand from register RB and the
least-significant bit of the operand from register RT.
. The 32-bit result is placed in register RT. 
. Bits 0 to 30 of the RT input are reserved and should be zero.")
			    (list "xsbh"
				  "XSBH	rt, ra"
				  "2"
				  "FX"
				  "rt.h[n] = ext( ra.h[n] & 0xFF )\n
For each of eight halfword slots:
. The sign of the byte in the right byte of the operand in register RA is 
propagated to the left byte.
. The resulting 16-bit integer is stored in register RT.")
			    (list "xshw"
				  "XSHW rt, ra"
				  "2"
				  "FX"
				  "rt.w[n] = ext( ra.w[n] & 0xFFFF )\n
For each of four word slots:
. The sign of the halfword in the right half of the operand in register RA is
propagated to the left halfword.
. The resulting 32-bit integer is placed in register RT.")
			    (list "xswd"
				  "XSWD rt, ra"
				  "2"
				  "FX"
				  "rt.d[n] = ext( ra.d[n] & 0xFFFFFFFF )\n
For each of two doubleword slots:
. The sign of the word in the right slot is propagated to the left word.
. The resulting 64-bit integer is stored in register RT.")
			    (list "clz"
				  "CLZ rt, ra"
				  "2"
				  "FX"
				  "rt.w[n] = leadingZeroCount(ra.w[n])\n
For each of four word slots:
. The number of zero bits to the left of the first '1' bit in the operand
in register RA is computed.
. The result is placed in register RT. If register RA is zero, 
the result is 32.

The result placed in register RT satisfies 0 <= RT <= 32. The value in 
register RT is zero, for example, if the corresponding slot in RA is a 
negative integer. The value in register RT is 32 if the corresponding slot in
register RA is zero.")
			    (list "selb"
				  "SELB	rt, ra, rb, rc"
				  "2"
				  "FX"
				  "rt = ( rc ? rb : ra )\n
A result is formed by using bits from RC to choose corresponding bits 
either from RA or RB.
. If the bit in register RC is '0', then select the bit from register 
RA; otherwise, select the bit from register RB.
. The selected bits are placed in register RT.")
			    (list "shlqby"
				  "SHLQBY rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra << ( ( rb.b[3] & 0x1F ) << 3 )")
			    (list "shlqbyi"
				  "SHLQBYI rt, ra, imm"
				  "4"
				  "SH"
				  "rt = ra << ( ( imm & 0x1F ) << 3 )")
			    (list "shlqbybi"
				  "SHLQBYBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra << ( rb.b[3] & 0xF8 )")
			    (list "shlqbi"
				  "SHLQBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra << ( rb.b[3] & 0x07 )")
			    (list "shlqbii"
				  "SHLQBII rt, ra, imm"
				  "4"
				  "SH"
				  "rt = ra << ( imm & 0x07 )")
			    (list "rotqby"
				  "ROTQBY rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra <^ ( ( rb.b[3] & 0x0F ) << 3 )")
			    (list "rotqbyi"
				  "ROTQBYI rt, ra, imm"
				  "4"
				  "SH"
				  "rt = ra <^ ( ( imm & 0x0F ) << 3 )")
			    (list "rotqbybi"
				  "ROTQBYBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra <^ ( rb.b[3] & 0x78 )")
			    (list "rotqbi"
				  "ROTQBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra <^ ( rb.b[3] & 0x07 )")
			    (list "rotqbii"
				  "ROTQBII rt, ra, imm"
				  "4"
				  "SH"
				  "rt = ra <^ ( imm & 0x07 )")
			    (list "rotqmby"
				  "ROTQMBY rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra >> ( ( -rb.b[3] & 0x1F ) << 3 )")
			    (list "rotqmbyi"
				  "ROTQMBYI rt, ra, imm"
				  "4"
				  "SH"
				  "rt = ra >> ( ( -imm & 0x1F ) << 3 )")
			    (list "rotqmbybi"
				  "ROTQMBYBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra >> ( -( rb.b[3] & 0xF8 ) & 0xF8 )\n\n  #bits to	bits in b	b.low = -shr\n  shift right	high  low	b.hi  = -(shr & 0xF8)\n  0		00000 000\n  1		00000 111\n  2		00000 110\n  3		00000 101\n  4		00000 100\n  5		00000 011\n  6		00000 010\n  7		00000 001\n  8		11111 000\n  9		11111 111\n  . . .\n  16		11110 000\n")
			    (list "rotqmbi"
				  "ROTQMBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra >> ( -rb.b[3] & 0x07 )")
			    (list "rotqmbii"
				  "ROTQMBII rt, ra, imm"
				  "4"
				  "SH"
				  "rt = ra >> ( -imm & 0x07 )")
			    (list "orx"
				  "ORX rt, ra"
				  "4"
				  "SH"
				  "rt.w[0] = ( ra.w[0] | ra.w[1] | ra.w[2] | ra.w[3] )\nrt.w[1] = rt.w[2] = rt.w[3] = 0\n
The four words of RA are logically ORed. The result is placed in the preferred
slot of register RT. The other three slots of the register are written with 
zeros.")
			    (list "shufb"
				  "SHUFB rt, ra, rb, rc"
				  "4"
				  "SH"
				  "x = rc.b[n]\nx in 0x00..0x7F: rt.b[n] = x & 0x10 ? rb.b[x & 0x0F] : ra.b[x & 0x0F]\nx in 0x80..0xBF: rt.b[n] = 0x00\nx in 0xC0..0xDF: rt.b[n] = 0xFF\nx in 0xE0..0xFF: rt.b[n] = 0x80\n
Registers RA and RB are logically concatenated with the least-significant bit
of RA adjacent to the most-significant bit of RB. The bytes of the resulting 
value are considered to be numbered from 0 to 31.
For each byte slot in registers RC and RT:
. The value in register RC is examined, and a result byte is produced as shown
 in Table below.
. The result byte is inserted into register RT.

+-----------+--------------------------------------------------------+
| 10xxxxxx  | 0x00                                                   |
| 110xxxxx  | 0xFF                                                   |
| 111xxxxx  | 0x80                                                   |
| Otherwise | The byte of the concatenated register addressed by the |
|           | rightmost 5 bits of register RC                        |
+-----------+--------------------------------------------------------+")
			    (list "cbd"
				  "CBD rt, i7(ra)"
				  "4"
				  "SH"
				  "rt = generate shuffleb mask((ra.b[3]+ i7) & 0x0F)\nCBD 0 = s_Dbcdefghijklmnop, not s_Abcd...\n
A 4-bit address is computed by adding the value in the signed I7 field to the 
value in the preferred slot of register RA. The address is used to determine
the position of the addressed byte within a quadword. Based on the position, 
a mask is generated that can be used with the Shuffle Bytes (shufb) instruction
to insert a byte at the indicated position within a (previously loaded) 
quadword. The byte is taken from the rightmost byte position of the preferred 
slot of the RA operand of the shufb instruction. ")
			    (list "cbx"
				  "CBX rt, ra, rb"
				  "4"
				  "SH"
				  "rt = generate shuffleb mask((ra.b[3]+rb.b[3]) & 0x0F)\n\n0 => s_Dbcdefghijklmnop, not s_Abcd...\n
A 4-bit address is computed by adding the value in the preferred slot of 
register RA to the value in the preferred slot of register RB. The address is
used to determine the position of the addressed byte within a quadword. Based
on the position, a mask is generated that can be used with the shufb 
instruction to insert a byte at the indicated position within a (previously 
loaded) quadword. The byte is taken from the rightmost byte position of the 
preferred slot of the RA operand of the shufb instruction. ")
			    (list "chd"
				  "CHD rt, i7(ra)"
				  "4"
				  "SH"
				  "rt = generate shuffleh mask((ra.b[3]+ i7) & 0x0E)\nCHD 0 = s_Bbcdefgh, not s_Abcd...\n
A 4-bit address is computed by adding the value in the signed I7 field to the
value in the preferred slot of register RA and forcing the least-significant
bit to zero. The address is used to determine the position of an aligned 
halfword within a quadword. Based on the position, a mask is generated that 
can be used with the shufb instruction to insert a halfword at the indicated
position within a quadword. The halfword is taken from the rightmost 2 bytes
of the preferred slot of the RA operand of the shufb instruction.")
			    (list "chx"
				  "CHX rt, ra, rb"
				  "4"
				  "SH"
				  "rt = generate shuffleh mask((ra.b[3]+rb.b[3]) & 0x0E)\n0 => s_Bbcdefgh, not s_Abcd...\n
A 4-bit address is computed by adding the value in the preferred slot of 
register RA to the value in the preferred slot of register RB and forcing the
least-significant bit to zero. The address is used to determine the position of
an aligned halfword within a quadword. Based on the position, a mask is 
generated that can be used with the shufb instruction to insert a halfword at
the indicated position within a quadword. The halfword is taken from the 
rightmost 2 bytes of the preferred slot of the RA operand of the shufb 
instruction.")
			    (list "cwd"
				  "CWD rt, i7(ra)"
				  "4"
				  "SH"
				  "rt = generate shufflew mask((ra.b[3]+ i7) & 0x0C)\nCWD 0 = s_Abcd\n
A 4-bit address is computed by adding the value in the signed I7 field to the 
value in the preferred slot of register RA and forcing the least-significant 2
bits to zero. The address is used to determine the position of an aligned word
within a quadword. Based on the position, a mask is generated that can be used
with the shufb instruction to insert a word at the indicated position within a
quadword. The word is taken from the preferred slot of the RA operand of the 
shufb instruction.")
			    (list "cwx"
				  "CWX rt, ra, rb"
				  "4"
				  "SH"
				  "rt = generate shufflew mask(ra.b[3]+rb.b[3] & 0x0C)\n
A 4-bit address is computed by adding the value in the preferred slot of 
register RA to the value in the preferred slot of register RB and forcing the
least-significant 2 bits to zero. The address is used to determine the position
of an aligned word within a quadword. Based on the position, a mask is 
generated that can be used with the shufb instruction to insert a word at the
indicated position within a quadword. The word is taken from the preferred slot
of the RA operand of the shufb instruction.")
			    (list "cdd"
				  "CDD rt, i7(ra)"
				  "4"
				  "SH"
				  "rt = generate shuffled mask((ra.b[3]+ i7) & 0x08)\nCDD 0 = s_Ab\n
A 4-bit address is computed by adding the value in the signed I7 field to the 
value in the preferred slot of register RA and forcing the least-significant 3
bits to zero. The address is used to determine the position of an aligned 
doubleword within a quadword. Based on the position, a mask is generated that 
can be used with the shufb instruction to insert a doubleword at the indicated
position within a quadword. The doubleword is taken from the leftmost 8 bytes 
of the RA operand of the shufb instruction.")
			    (list "cdx"
				  "CDX rt, ra, rb"
				  "4"
				  "SH"
				  "rt = generate shuffled mask((ra.b[3]+rb.b[3]) & 0x08)\n
A 4-bit address is computed by adding the value in the preferred slot of 
register RA to the value in the preferred slot of register RB and forcing the
least-significant 3 bits to zero. The address is used to determine the position
of the addressed doubleword within a quadword. Based on the position, a mask is
generated that can be used with the shufb instruction to insert a doubleword at
the indicated position within a quadword. The quadword is taken from the 
leftmost 8 bytes of the RA operand of the shufb instruction.")
			    (list "fsmbi"
				  "FSMBI rt, u16"
				  "4"
				  "SH"
				  "rt.b[n] = ( ( u16 << n ) & 0x8000 ) ? 0xFF : 0x00\nexpand 16 bits to 16 bytes\n
The u16 is used to create a mask in register RT by making eight copies of each 
bit. Bits in the operand are related to bytes in the result in a left-to-right
correspondence.

s = u16
for j = 0 to 15
  If sj == 0 then r.b[j] = 0x00
             else r.b[j] = 0xFF
end
rt = r")
			    (list "fsmb"
				  "FSMB	rt, ra"
				  "4"
				  "SH"
				  "rt.b[n] = ( ( ra.h[1] << n ) & 0x8000 ) ? 0xFF : 0x00
The rightmost 16 bits of the preferred slot of register RA are used to create a 
mask in register RT by replicating each bit eight times. Bits in the operand 
are related to bytes in the result in a left-to-right correspondence.

s = ra.h[1]
for j = 0 to 15
  If sj == 0 then r.b[j] = 0x00
             else r.b[j] = 0xFF
end
rt = r")
			    (list "fsmh"
				  "FSMH	rt, ra"
				  "4"
				  "SH"
				  "rt.b[n] = ( ( ra.b[3] << n ) & 0x80 ) ? 0xFFFF : 0x0000\nexpand 8 bits to 8 half words\n
The rightmost 8 bits of the preferred slot of register RA are used to create a 
mask in register RT by replicating each bit 16 times. Bits in the operand are 
related to halfwords in the result, in a left-to-right correspondence.

s = RA.b[3]
for j = 0 to 7
  If sj = 0 then r.h[j] = 0x0000
            else r.h[j] = 0xFFFF
end
RT = r")
			    (list "fsm"
				  "FSM rt, ra"
				  "4"
				  "SH"
				  "rt.b[n] = ( ( ra.b[3] << n ) & 0x8 ) ? 0xFFFFFFFF : 0x00000000\nexpand 4 bits to 4 words\n
The rightmost 4 bits of the preferred slot of register RA are used to create a
mask in register RT by replicating each bit 32 times. Bits in the operand are
related to words in the result in a left-to-right correspondence.

s = RA.b[3]
for j = 0 to 3
  If sj = 0 then r.w[j] = 0x00000000
            else r.w[j] = 0xFFFFFFFF
end
RT = r")
			    (list "gbb"
				  "GBB rt, ra"
				  "4"
				  "SH"
				  "rt = 0;\nfor( n = 0; n < 16; n++ )\n  rt.w[0] = (rt.w[0]<<1) | ( ra.b[n] & 1 )\ngather lowest bit of 16 bytes into preferred word\n
A 16-bit quantity is formed in the right half of the preferred slot of register
RT by concatenating the rightmost bit in each byte of register RA. The leftmost
16 bits of register RT are set to zero, as are the remaining slots of register 
RT.")
			    (list "gbh"
				  "GBH rt, ra"
				  "4"
				  "SH"
				  "rt = 0;\nfor( n = 0; n < 8; n++ )\n  rt.w[0] = (rt.w[0]<<1) | ( ra.h[n] & 1 )\ngather lowest bit of 8 halfwords into preferred word\n
An 8-bit quantity is formed in the rightmost byte of the preferred slot of 
register RT by concatenating the rightmost bit in each halfword of register RA.
The leftmost 24 bits of the preferred slot of register RT are set to zero, as 
are the remaining slots of register RT.")
			    (list "gb"
				  "GB rt, ra"
				  "4"
				  "SH"
				  "rt = 0;\nfor( n = 0; n < 4; n++ )\n  rt.w[0] = (rt.w[0]<<1) | ( ra.w[n] & 1 )\ngather lowest bit of 4 words into preferred word\n
A 4-bit quantity is formed in the rightmost 4 bits of register RT by 
concatenating the rightmost bit in each word of register RA. The leftmost 28 
bits of register RT are set to zero, as are the remaining slots of register
RT.")
			    (list "frest"
				  "FREST a, b"
				  "4"
				  "SH"
				  "Form Reciprocal ESTimate for 1/b.f[n]\nmust complete with FI instruction\n\n  1/x\n		FREST a, x\n	FI	b, x, a		b is good to 12 bits precision\n	FNMS	c, b, x, One\n	FMA	b, c, b, b      now b is good to 24 bits precision")
			    (list "frsqest"
				  "FRSQEST a, b"
				  "4"
				  "SH"
				  "Form Reciprocal SQuare root ESTimate for 1/sqrt(b.f[n])\nmust complete with FI instruction\n\n  1/sqrt(x)\n		FRSQEST a, x\n	FI	b, x, a		b is good to 12 bits precision\n	FM	c, b, x		(b and a can share register)\n	FM	d, b, OneHalf	(c and x can share register)\n	FNMS	c, c, b, One\n	FMA	b, d, c, b      now b is good to 24 bits precision")
			    (list "sync"
				  "SYNC"
				  "?"
				  "??"
				  "wait for data stores to finish")
			    (list "syncc"
				  "SYNCC chn?"
				  "?"
				  "??"
				  "synchronize channel, then do SYNC\n(SPU doc does not specify chn)")
			    (list "dsync"
				  "DSYNC"
				  "?"
				  "??"
				  "wait for data loads to finish")
			    (list "fscrrd"
				  "FSCRRD a"
				  "?"
				  "??"
				  "read floating point status flags")
			    (list "fscrwr"
				  "FSCRWR a"
				  "?"
				  "??"
				  "write ...")
			    (list "mfspr"
				  "MFSPR a, spr"
				  "?"
				  "??"
				  "move from special purpose register")
			    (list "mtspr"
				  "MTSPR spr, a"
				  "?"
				  "??"
				  "move to special purpose register")
			    (list "iretd"
				  "IRETD"
				  "?"
				  "??"
				  "return from interrupt, disable interrupts")
			    (list "irete"
				  "IRETE"
				  "?"
				  "??"
				  "return from interrupt, enable interrupts")
			    (list "iret"
				  "IRET"
				  "?"
				  "??"
				  "return from interrupt")
			    (list "rchcnt"
				  "RCHCNT a, chn"
				  "?"
				  "??"
				  "read channel chn count")
			    (list "rdch"
				  "RDCH	a, chn"
				  "?"
				  "??"
				  "read from channel chn")
			    (list "wrch"
				  "WRCH	chn, a"
				  "?"
				  "??"
				  "write to channel chn")
			    (list "stop"
				  "STOP	imm"
				  "?"
				  "??"
				  "stop and signal")
			    (list "stopd"
				  "STOPD a, b, c"
				  "?"
				  "??"
				  "stop and signal when registers a,b,c dependencies clear")
			    ))


(defun spu-tooltip-string (opcode)
  (let ((data (assoc opcode spu-opcode-help)))
    (if data
	(concat "" (nth 1 data) "\n[class: " (nth 3 data) "; latency: " (nth 2 data) " cycles]\n\n"
		(nth 4 data)))))

;;(spu-tooltip-string "shufb")
	
(defun spu-find-register-definition(reg)
  "Search for the definition of the register, and returns the comment at the end of line if any (after ';')"
  (save-excursion 
    (save-match-data
      (goto-char (point-min))
      ;;(let ((regdef (search-forward-regexp (concat "^[\t ]*\\.reg[\t ]*" reg) nil t)))
      (let ((regdef (search-forward-regexp (concat "^\\.reg[\t ]*\\<" reg "\\>") nil t)))
	(when regdef
	  (goto-char regdef)
	  (let* ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
		 (cmt (string-match ";" line)))
	    (if cmt
		(substring line cmt))))))))

(defun th-spu-mode-handler ()
  (or (spu-tooltip-string (current-word))
      (spu-find-register-definition (current-word)) 
      ""))


(provide 'spu-tooltip)