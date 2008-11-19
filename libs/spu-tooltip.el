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
;;   http://www-01.ibm.com/chips/techlib/techlib.nsf/techdocs/76CA6C7304210F3987257060006F2C44/$file/SPU_ISA_v1.2_27Jan2007_pub.pdf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tooltip-help)

;;
;;
;; Documentation data:
;;
;;




;;
;; Memory Load/Store Instructions:
;;


(setq spu-opcode-help-mops (list 
			    (list "lqd"
				  "LQD rt, s14(RA)"
				  "6"
				  "LS"
				  "addr = s14 & ~15 + ra.w[0]\nrt = *addr\n
The local storage address is computed by adding the signed value s14, with the 
4 least significant bits cleared, to the value in the preferred slot of 
register RA and forcing the rightmost 4 bits of the sum to zero. The 16 bytes 
at the local storage address are placed into register RT.")
			    (list "lqx"
				  "LQX rt, ra, rb"
				  "6"
				  "LS"
				  "addr = ra.w[0] + rb.w[0]\nrt = *addr\n
The local storage address is computed by adding the value in the preferred slot
of register RA to the value in the preferred slot of register RB and forcing 
the rightmost 4 bits of the sum to zero. The 16 bytes at the local storage 
address are placed into register RT.")
			    (list "lqa"
				  "LQA	rt, label18"
				  "6"
				  "LS"
				  "addr = label18 & ~3\nrt = *addr\n
The value label18 (with the 2 least significant bits cleared) is used as the
local storage address. The 16 bytes at the local storage address are loaded 
into register RT.")
			    (list "lqr"
				  "LQR rt, label18"
				  "6"
				  "LS"
				  "addr = label18 & ~3 + PC\nrt = *addr\n
The value label, with the 2 least significant bits cleared, is added to the 
program counter (PC) to form the local storage address. The 16 bytes at the
local storage address are loaded into register RT.")
			    (list "stqd"
				  "STQD rt, s14(ra)"
				  "6"
				  "LS"
				  "addr = s14 & ~15 + ra.w[0]\n*addr = rt\n
The local storage address is computed by adding the signed value s14, with the
4 least significant bits cleared, to the value in the preferred slot of 
register RA and forcing the rightmost 4 bits of the sum to zero. The contents 
of register RT are stored at the local storage address.")
			    (list "stqx"
				  "STQX rt, ra, rb"
				  "6"
				  "LS"
				  "addr = ra.w[0] + rb.w[0]\n*addr = rt\n
The local storage address is computed by adding the value in the preferred slot
of register RA to the value in the preferred slot of register RB and forcing
the rightmost 4 bits of the sum to zero. The contents of register RT are stored
at the local storage address.")
			    (list "stqa"
				  "STQA rt, label18"
				  "6"
				  "LS"
				  "addr = label18 & ~3\n*addr = rt\n
The value label18 (with the 2 least significant bits cleared) is used as the
local storage address. The contents of register RT are stored at the location 
given by the local storage address.")
			    (list "stqr"
				  "STQR rt, label18"
				  "6"
				  "LS"
				  "addr = label18 & ~3 + PC\n*addr = rt\n
The value label, with the 2 least significant bits cleared, is added to the 
program counter (PC) to form the local storage address. The contents of 
register RT are stored at the location given by the local storage address.")
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
leftmost 8 bytes of the RA operand of the shufb instruction.")))


;;
;; Constant-Formation Instructions:
;;


(setq spu-opcode-help-cfin (list
			    (list "ilh"
				  "ILH rt, s16"
				  "2"
				  "FX"
				  "rt.h[n] = s16\n
For each of eight halfword slots:
. The value s16 is placed in register RT.")
			    (list "ilhu"
				  "ILHU	rt, s16"
				  "2"
				  "FX"
				  "rt.w[n] = s16 << 16\n
For each of four word slots:
. The value s16 is placed in the leftmost 16 bits of the word.
. The remaining bits of the word are set to zero.")
			    (list "il"
				  "IL rt, s16"
				  "2"
				  "FX"
				  "rt.w[n] = ext(s16)\n
For each of four word slots:
. The value s16 is expanded to 32 bits by replicating the leftmost bit.
. The resulting value is placed in register RT.")
			    (list "ila"
				  "ILA rt, u18"
				  "2"
				  "FX"
				  "rt.w[n] = u18\n
For each of four word slots:
. The value u18 is placed unchanged in the rightmost 18 bits of register RT.
. The remaining bits of register RT are set to zero.")
			    (list "iohl"
				  "IOHL	rt, u16"
				  "2"
				  "FX"
				  "rt.w[n] |= u16\n
For each of four word slots:
. The value u16 is prefaced with zeros and ORed with the value in register RT.
. The result is placed into register RT.")
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
rt = r")))


;;
;; Integer and Logical Instructions:
;;

(setq spu-opcode-help-ilin (list
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
			    (list "mpyu"
				  "MPYU rt, ra, rb"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n+1] * rb.h[2n+1]\nmultiply lower halves unsigned\n
For each of four word slots:
. The rightmost 16 bits of register RA are multiplied by the rightmost 16 bits 
of register RB, treating both operands as unsigned.
. The 32-bit product is placed in register RT.")
			    (list "mpyi"
				  "MPYI rt, ra, s10"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n+1] * ext(s10)\nmultiply lower halves signed immediate\n
For each of four word slots:
. The signed value s10 is multiplied by the value in the rightmost 16 bits
of register RA.
. The resulting product is placed in register RT.")
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
			    (list "mpyhh"
				  "MPYHH rt, ra, rb"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n] * rb.h[2n]\nmultiply upper halves signed\n
For each of four word slots:
. The leftmost 16 bits in register RA are multiplied by the leftmost 16 bits
in register RB.
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
			    (list "mpyhhu"
				  "MPYHHU rt, ra, rb"
				  "7"
				  "FI"
				  "rt.w[n] = ra.h[2n] * rb.h[2n]\nmultiply upper halves unsigned\n
For each of four word slots:
. The leftmost 16 bits in register RA are multiplied by the leftmost 16 bits in
register RB, treating both operands as unsigned.
. The 32-bit product is placed in register RT.")
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
			    (list "fsmb"
				  "FSMB	rt, ra"
				  "4"
				  "SH"
				  "rt.b[n] = ( ( ra.h[1] << n ) & 0x8000 ) ? 0xFF : 0x00\n
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
			    (list "and"
				  "AND rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ra & rb\n
The values in register RA and register RB are logically ANDed. The result is 
placed in register RT.")
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
			    (list "orx"
				  "ORX rt, ra"
				  "4"
				  "SH"
				  "rt.w[0] = ( ra.w[0] | ra.w[1] | ra.w[2] | ra.w[3] )\nrt.w[1] = rt.w[2] = rt.w[3] = 0\n
The four words of RA are logically ORed. The result is placed in the preferred
slot of register RT. The other three slots of the register are written with 
zeros.")
			    (list "xor"
				  "XOR rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ra ^ rb\n
The values in register RA and register RB are logically XORed. The result
is placed in register RT.")
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
			    (list "nand"
				  "NAND rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ~(ra & rb)\n
For each of four word slots:
. The complement of the AND of the bit in register RA and the bit in register
RB is placed in register RT.")
			    (list "nor"
				  "NOR rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ~(ra | rb)\n
For each of four word slots:
. The values in register RA and register RB are logically ORed.
. The result is complemented and placed in register RT.")
			    (list "eqv"
				  "EQV rt, ra, rb"
				  "2"
				  "FX"
				  "rt = ~(ra ^ rb)\nEQV is 'NXOR'\nAlso, the missing 'XORC' would give the same result as EQV\n
For each of four word slots:
. If the bit in register RA and register RB are the same, the result is '1'; otherwise, the result is '0'.
. The result is placed in register RT.")
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
+-----------+--------------------------------------------------------+")))


;;
;; Shift and Rotate Instructions:
;;


(setq spu-opcode-help-srin (list
			    (list "shlh"
				  "SHLH	rt, ra, rb"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] << rb.h[n]\n
For each of eight halfword slots:
. The contents of register RA are shifted to the left according to the count in
bits 11 to 15 of register RB.
. The result is placed in register RT.
. If the count is zero, the contents of register RA are copied unchanged into 
register RT. If the count is greater than 15, the result is zero.
. Bits shifted out of the left end of the halfword are discarded; zeros are 
shifted in at the right.")
			    (list "shlhi"
				  "SHLHI rt, ra, u7"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] << u7\n
For each of eight halfword slots:
. The contents of register RA are shifted to the left according to the count in
the immediate value u7.
. The result is placed in register RT.
. If the count is zero, the contents of register RA are copied unchanged into
register RT. If the count is greater than 15, the result is zero.
. Bits shifted out of the left end of the halfword are discarded; zeros are 
shifted in at the right.")
			    (list "shl"
				  "SHL rt, ra, rb"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] << rb.w[n]\n
For each of four word slots:
. The contents of register RA are shifted to the left according to the count in
bits 26 to 31 of register RB.
. The result is placed in register RT.
. If the count is zero, the contents of register RA are copied unchanged into 
register RT. If the count is greater than 31, the result is zero.
. Bits shifted out of the left end of the word are discarded; zeros are shifted
in at the right.")
			    (list "shli"
				  "SHLI rt, ra, u7"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] << u7\n
For each of four word slots:
. The contents of register RA are shifted to the left according to the count in
the immediate value u7.
. The result is placed in register RT.
. If the count is zero, the contents of register RA are copied unchanged into
register RT. If the count is greater than 31, the result is zero.
. Bits shifted out of the left end of the word are discarded; zeros are shifted
in at the right.")
			    (list "shlqbi"
				  "SHLQBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra << ( rb.b[3] & 0x07 )\n
The contents of register RA are shifted to the left according to the count in
bits 29 to 31 of the preferred slot of register RB. The result is placed in 
register RT. A shift of up to 7 bit positions is possible.
If the count is zero, the contents of register RA are copied unchanged into 
register RT. Bits shifted out of the left end of the register are discarded, 
and zeros are shifted in at the right.")
			    (list "shlqbii"
				  "SHLQBII rt, ra, u7"
				  "4"
				  "SH"
				  "rt = ra << ( imm & 0x07 )\n
The contents of register RA are shifted to the left according to the count in
the last 3 bits of the U7 value. The result is placed in register RT. A shift
of up to 7 bit positions is possible.
If the count is zero, the contents of register RA are copied unchanged into 
register RT. Bits shifted out of the left end of the register are discarded, 
and zeros are shifted in at the right.")
			    (list "shlqby"
				  "SHLQBY rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra << ( ( rb.b[3] & 0x1F ) << 3 )\n
The bytes of register RA are shifted to the left according to the count in bits
27 to 31 of the preferred slot of register RB. The result is placed in register
RT. If the count is zero, the contents of register RA are copied unchanged into
register RT. If the count is greater than 15, the result is zero. Bytes shifted
out of the left end of the register are discarded, and bytes of zeros are 
shifted in at the right.")
			    (list "shlqbyi"
				  "SHLQBYI rt, ra, u7"
				  "4"
				  "SH"
				  "rt = ra << ( ( u7 & 0x1F ) << 3 )\n
The bytes of register RA are shifted to the left according to the count in the
last 4 bits of the U7 value. The result is placed in register RT. If the count
is zero, the contents of register RA are copied unchanged into register RT. If
the count is greater than 15, the result is zero. Bytes shifted out of the left
end of the register are discarded, and zero bytes are shifted in at the right.")
			    (list "shlqbybi"
				  "SHLQBYBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra << ( rb.b[3] & 0xF8 )\n
The bytes of register RA are shifted to the left according to the count in bits
24 to 28 of the preferred slot of register RB. The result is placed in register
RT. If the count is zero, the contents of register RA are copied unchanged into
register RT. If the count is greater than 15, the result is zero. Bytes shifted
out of the left end of the register are discarded, and bytes of zeros are 
shifted in at the right.")
			    (list "roth"
				  "ROTH	rt, ra, rb"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] <^ ( rb.h[n] & 0x0F )\n<^ is an idiosyncratic symbol for rotate\n
For each of eight halfword slots:
. The contents of register RA are rotated to the left according to the count in
bits 12 to 15 of register RB.
. The result is placed in register RT.
. If the count is zero, the contents of register RA are copied unchanged into 
register RT.
. Bits rotated out of the left end of the halfword are rotated in at the right 
end.")
			    (list "rothi"
				  "ROTHI rt, ra, u7"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] <^ ( u7 & 0x0F )\n<^ is an idiosyncratic symbol for rotate\n
For each of eight halfword slots:
. The contents of register RA are rotated to the left according to the count
in the last 4 bits of the U7 value.
. The result is placed in register RT.
. If the count is zero, the contents of register RA are copied unchanged into
register RT.
. Bits rotated out of the left end of the halfword are rotated in at the right
end.")
			    (list "rot"
				  "ROT rt, ra, rb"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] <^ ( rb.w[n] & 0x1F )\n<^ is an idiosyncratic symbol for rotate\n
For each of four word slots:
. The contents of register RA are rotated to the left according to the count in
bits 27 to 31 of register RB.
. The result is placed in register RT.
. If the count is zero, the contents of register RA are copied unchanged into 
register RT.
. Bits rotated out of the left end of the word are rotated in at the right end.")
			    (list "roti"
				  "ROTI rt, ra, u7"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] <^ ( u7 & 0x1F )\n<^ is an idiosyncratic symbol for rotate\n
For each of four word slots:
. The contents of register RA are rotated to the left according to the count in
the last four bits of the value u7.
. The result is placed in register RT.
. If the count is zero, the contents of register RA are copied unchanged into 
register RT.
. Bits rotated out of the left end of the word are rotated in at the right end.")
			    (list "rotqby"
				  "ROTQBY rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra <^ ( ( rb.b[3] & 0x0F ) << 3 )\n
The bytes in register RA are rotated to the left according to the count in the
rightmost 4 bits of the preferred slot of register RB. The result is placed in
register RT. Rotation of up to 15 byte positions is possible.
If the count is zero, the contents of register RA are copied unchanged into 
register RT. Bytes rotated out of the left end of the register are rotated in
at the right.")
			    (list "rotqbyi"
				  "ROTQBYI rt, ra, u7"
				  "4"
				  "SH"
				  "rt = ra <^ ( ( u7 & 0x0F ) << 3 )\n
The bytes in register RA are rotated to the left according to the count in the
rightmost 4 bits of the value u7. The result is placed in register RT. Rotation
of up to 15 byte positions is possible. If the count is zero, the contents of
register RA are copied unchanged into register RT. Bytes rotated out of the 
left end of the register are rotated in at the right.")
			    (list "rotqbybi"
				  "ROTQBYBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra <^ ( rb.b[3] & 0x78 )\n
The bytes of register RA are rotated to the left according to the count in bits
25 to 28 of the preferred slot of register RB. The result is placed in register
RT. If the count is zero, the contents of register RA are copied unchanged into
register RT. Bytes rotated out of the left end of the register are rotated in 
at the right.")
			    (list "rotqbi"
				  "ROTQBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra <^ ( rb.b[3] & 0x07 )\n
The contents of register RA are rotated to the left according to the count in
bits 29 to 31 of the preferred slot of register RB. The result is placed in
register RT. Rotation of up to 7 bit positions is possible.
If the count is zero, the contents of register RA are copied unchanged into 
register RT. Bits rotated out at the left end of the register are rotated in at
the right.")
			    (list "rotqbii"
				  "ROTQBII rt, ra, u7"
				  "4"
				  "SH"
				  "rt = ra <^ ( u7 & 0x07 )\n
The contents of register RA are rotated to the left according to the count in
the rightmost 3 bits of the value u7. The result is placed in register RT. 
Rotation of up to 7 bit positions is possible. If the count is zero, the 
contents of register RA are copied unchanged into register RT.
Bits rotated out at the left end of the register are rotated in at the right.")
			    (list "rothm"
				  "ROTHM rt, ra, rb"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] >> ( -rb.h[n] & 0x1F )\nshift right logical\n
For each of eight halfword slots:
. The shift_count is (0 - RB) modulo 32.
. If the shift_count is less than 16, then RT is set to the contents of RA 
shifted right shift_count bits, with zero fill at the left.
. Otherwise, RT is set to zero.

The Rotate and Mask instructions provide support for a logical right shift, and
the Rotate and Mask Algebraic instructions provide support for an algebraic 
right shift. They differ from a conventional right logical or algebraic shift 
in that the shift amount accepted by the instructions is the two's complement
of the right shift amount. 
Thus, to shift right logically the contents of R2 by the number of bits given 
in R1, the following sequence could be used:
   sfi r3,r1,0    Form two's complement
   rotm r4,r2,r3  Rotate, then mask
For the immediate forms of these instructions, the formation of the two's 
complement shift quantity can be performed during assembly or compilation.")
			    (list "rothmi"
				  "ROTHMI rt, ra, u7"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] >> ( -u7 & 0x1F )\n
For each of eight halfword slots:
. The shift_count is (0 - U7) modulo 32.
. If the shift_count is less than 16, then RT is set to the contents of RA
shifted right shift_count bits, with zero fill at the left.
. Otherwise, RT is set to zero.

The Rotate and Mask instructions provide support for a logical right shift, and
the Rotate and Mask Algebraic instructions provide support for an algebraic 
right shift. They differ from a conventional right logical or algebraic shift 
in that the shift amount accepted by the instructions is the two's complement
of the right shift amount. 
Thus, to shift right logically the contents of R2 by the number of bits given 
in R1, the following sequence could be used:
   sfi r3,r1,0    Form two's complement
   rotm r4,r2,r3  Rotate, then mask
For the immediate forms of these instructions, the formation of the two's 
complement shift quantity can be performed during assembly or compilation.")
			    (list "rotm"
				  "ROTM rt, ra, rb"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] >> ( -rb.w[n] & 0x3F )\n
For each of four word slots:
. The shift_count is (0 - RB) modulo 64.
. If the shift_count is less than 32, then RT is set to the contents of RA
shifted right shift_count bits, with zero fill at the left.
. Otherwise, RT is set to zero.

The Rotate and Mask instructions provide support for a logical right shift, and
the Rotate and Mask Algebraic instructions provide support for an algebraic 
right shift. They differ from a conventional right logical or algebraic shift 
in that the shift amount accepted by the instructions is the two's complement
of the right shift amount. 
Thus, to shift right logically the contents of R2 by the number of bits given 
in R1, the following sequence could be used:
   sfi r3,r1,0    Form two's complement
   rotm r4,r2,r3  Rotate, then mask
For the immediate forms of these instructions, the formation of the two's 
complement shift quantity can be performed during assembly or compilation.")
			    (list "rotmi"
				  "ROTMI rt, ra, u7"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] >> ( -u7 & 0x3F )\n
For each of four word slots:
. The shift_count is (0 - U7) modulo 64.
. If the shift_count is less than 32, then RT is set to the contents of RA
shifted right shift_count bits, with zero fill at the left.
. Otherwise, RT is set to zero.

The Rotate and Mask instructions provide support for a logical right shift, and
the Rotate and Mask Algebraic instructions provide support for an algebraic 
right shift. They differ from a conventional right logical or algebraic shift 
in that the shift amount accepted by the instructions is the two's complement
of the right shift amount. 
Thus, to shift right logically the contents of R2 by the number of bits given 
in R1, the following sequence could be used:
   sfi r3,r1,0    Form two's complement
   rotm r4,r2,r3  Rotate, then mask
For the immediate forms of these instructions, the formation of the two's 
complement shift quantity can be performed during assembly or compilation.")
			    (list "rotqmby"
				  "ROTQMBY rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra >> ( ( -rb.b[3] & 0x1F ) << 3 )\n
The shift_count is (0 - the preferred word of RB) modulo 32. If the shift_count
is less than 16, then RT is set to the contents of RA shifted right shift_count
bytes, filling at the left with 0x00 bytes. Otherwise, RT is set to zero.")
			    (list "rotqmbyi"
				  "ROTQMBYI rt, ra, u7"
				  "4"
				  "SH"
				  "rt = ra >> ( ( -u7 & 0x1F ) << 3 )\n
The shift_count is (0 - U7) modulo 32. If the shift_count is less than 16, then
RT is set to the contents of RA shifted right shift_count bytes, filling at the
left with 0x00 bytes. Otherwise, all bytes of RT are set to 0x00.")
			    (list "rotqmbybi"
				  "ROTQMBYBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra >> ( -( rb.b[3] & 0xF8 ) & 0xF8 )\n\n  #bits to	bits in b	b.low = -shr\n  shift right	high  low	b.hi  = -(shr & 0xF8)\n  0		00000 000\n  1		00000 111\n  2		00000 110\n  3		00000 101\n  4		00000 100\n  5		00000 011\n  6		00000 010\n  7		00000 001\n  8		11111 000\n  9		11111 111\n  . . .\n  16		11110 000\n
The shift_count is (0 minus bits 24 to 28 of RB) modulo 32. If the shift_count
is less than 16, then RT is set to the contents of RA, which is shifted right 
shift_count bytes, and filled at the left with 0x00 bytes. Otherwise, all 
bytes of RT are set to 0x00.")
			    (list "rotqmbi"
				  "ROTQMBI rt, ra, rb"
				  "4"
				  "SH"
				  "rt = ra >> ( -rb.b[3] & 0x07 )\n
The shift_count is (0 - the preferred word of RB) modulo 8. RT is set to the 
contents of RA, shifted right by shift_count bits, filling at the left with
zero bits.")
			    (list "rotqmbii"
				  "ROTQMBII rt, ra, u7"
				  "4"
				  "SH"
				  "rt = ra >> ( -u7 & 0x07 )\n
The shift_count is (0 - U7) modulo 8. RT is set to the contents of RA, shifted
right by shift_count bits, filling at the left with zero bits.")
			    (list "rotmah"
				  "ROTMAH rt, ra, rb"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] >> ( -rb.h[n] & 0x1F )\nshift right arithmetic\n
For each of eight halfword slots:
. The shift_count is (0 - RB) modulo 32.
. If the shift_count is less than 16, then RT is set to the contents of RA 
shifted right shift_count bits, replicating bit 0 (of the halfword) at the 
left.
. Otherwise, all bits of this halfword of RT are set to bit 0 of this 
halfword of RA.")
			    (list "rotmahi"
				  "ROTMAHI rt, ra, u7"
				  "4"
				  "WS"
				  "rt.h[n] = ra.h[n] >> ( -u7 & 0x1F )\n
For each of eight halfword slots:
. The shift_count is (0 - U7) modulo 32.
. If the shift_count is less than 16, then RT is set to the contents of RA
shifted right shift_count bits, replicating bit 0 (of the halfword) at the
left.
. Otherwise, all bits of this halfword of RT are set to bit 0 of this 
halfword of RA.")
			    (list "rotma"
				  "ROTMA rt, ra, rb"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] >> ( -rb.w[n] & 0x3F )\n
For each of four word slots:
. The shift_count is (0 - RB) modulo 64.
. If the shift_count is less than 32, then RT is set to the contents of RA 
shifted right shift_count bits, replicating bit 0 (of the word) at the left.
. Otherwise, all bits of this word of RT are set to bit 0 of this word of RA.")
			    (list "rotmai"
				  "ROTMAI rt, ra, u7"
				  "4"
				  "WS"
				  "rt.w[n] = ra.w[n] >> ( -u7 & 0x3F )\n
For each of four word slots:
. The shift_count is (0 - U7) modulo 64.
. If the shift_count is less than 32, then RT is set to the contents of RA
shifted right shift_count bits, replicating bit 0 (of the word) at the left.
. Otherwise, all bits of this word of RT are set to bit 0 of this word of RA.")))


;;
;; Compare, Branch, and Halt Instructions:
;;

(setq spu-opcode-help-cbhi (list 
			    (list "heq"
				  "HEQ ra, rb"
				  "2"
				  "FX"
				  "halt if ra == rb\n
The value in the preferred slot of register RA is compared with the value in 
the preferred slot of register RB. If the values are equal, execution of the
program stops at or after the halt.")
			    (list "heqi"
				  "HEQI	ra, s10"
				  "2"
				  "FX"
				  "halt if ra == ext(s10)\n
The value s10 is extended to 32 bits by replicating the leftmost bit. The 
result is compared to the value in the preferred slot of register RA. If the
value from register RA is equal to the immediate value, execution of the SPU
program stops at or after the halt instruction.")
			    (list "hgt"
				  "HGT ra, rb"
				  "2"
				  "FX"
				  "halt if ra > rb -- signed\n
The value in the preferred slot of register RA is algebraically compared with 
the value in the preferred slot of register RB. If the value from register RA
is greater than the RB value, execution of the SPU program stops at or after 
the halt instruction.")
			    (list "hgti"
				  "HGTI	ra, s10"
				  "2"
				  "FX"
				  "halt if ra > ext(s10) -- signed\n
The value s10 is extended to 32 bits by replicating the leftmost bit. The 
result is algebraically compared to the value in the preferred slot of 
register RA. If the value from register RA is greater than the immediate value,
execution of the SPU program stops at or after the halt instruction.")
			    (list "hlgt"
				  "HLGT	ra, rb"
				  "2"
				  "FX"
				  "halt if ra > rb -- unsigned ('Logical')\n
The value in the preferred slot of register RA is logically compared with the 
value in the preferred slot of register RB. If the value from register RA is
greater than the value from register RB, execution of the SPU program stops at
or after the halt instruction.")
			    (list "hlgti"
				  "HLGTI ra, s10"
				  "2"
				  "FX"
				  "halt if ra > ext(s10) -- unsigned, even though sign-extended\n
The value s10 is extended to 32 bits by replicating the leftmost bit. The 
result is logically compared to the value in the preferred slot of register RA.
If the value from register RA is logically greater than the immediate value, 
execution of the SPU program stops at or after the halt instruction.")
			    (list "ceqb"
				  "CEQB	rt, ra, rb"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] == rb.b[n] ) ? FF : 00\n
For each of 16 byte slots:
. The operand from register RA is compared with the operand from register RB. 
If the operands are equal, a result of all one bits (true) is produced. If they
are unequal, a result of all zero bits (false) is produced.
. The 8-bit result is placed in register RT.")
			    (list "ceqbi"
				  "CEQBI rt, ra, u8"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] == u8 ) ? FF : 00\n
For each of 16 byte slots:
. The value in the rightmost 8 bits of the u8 value is compared with the value
in register RA. If the two values are equal, a result of all one bits (true) is
produced. If they are unequal, a result of all zero bits (false) is produced.
. The 8-bit result is placed in register RT.")
			    (list "ceqh"
				  "CEQH rt, ra, rb"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] == rb.h[n] ) ? FFFF : 0000\n
For each of 8 halfword slots:
. The operand from register RA is compared with the operand from register RB. 
If the operands are equal, a result of all one bits (true) is produced. If they
are unequal, a result of all zero bits (false) is produced.
. The 16-bit result is placed in register RT.")
			    (list "ceqhi"
				  "CEQHI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] == ext(s10) ) ? FFFF : 0000\n
For each of eight halfword slots:
. The value in the s10 value is extended to 16 bits by replicating its leftmost
bit and compared with the value in register RA. If the two values are equal, a
result of all one bits (true) is produced. If they are unequal, a result of all
zero bits (false) is produced.
. The 16-bit result is placed in register RT.")
			    (list "ceq"
				  "CEQ rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] == rb.w[n] ) ? FFFFFFFF : 00000000\n
For each of four word slots:
. The operand from register RA is compared with the operand from register RB. 
If the operands are equal, a result of all one bits (true) is produced. If they
are unequal, a result of all zero bits (false) is produced.
. The 32-bit result is placed in register RT.")
			    (list "ceqi"
				  "CEQI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] == ext(s10) ) ? FFFFFFFF : 00000000\n
For each of four word slots:
. The s10 value is extended to 32 bits by replicating its leftmost bit and 
comparing it with the value in register RA. If the two values are equal, a
result of all one bits (true) is produced. If they are unequal, a result of
all zero bits (false) is produced.
. The 32-bit result is placed in register RT.")
			    (list "cgtb"
				  "CGTB rt, ra, rb"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] > rb.b[n] ) ? FF : 00\nsigned\n
For each of 16 byte slots:
. The operand from register RA is algebraically compared with the operand from 
register RB. If the operand in register RA is greater than the operand in 
register RB, a result of all one bits (true) is produced. Otherwise, a result
of all zero bits (false) is produced.
. The 8-bit result is placed in register RT.")
			    (list "cgtbi"
				  "CGTBI rt, ra, u10"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] > (u10&0xFF) ) ? FF : 00\n
For each of 16 byte slots:
. The value in the rightmost 8 bits of the u10 value is algebraically compared 
with the value in register RA. If the value in register RA is greater, a result
of all one bits (true) is produced. Otherwise, a result of all zero bits 
(false) is produced.
. The 8-bit result is placed in register RT.")
			    (list "cgth"
				  "CGTH rt, ra, rb"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] > rb.h[n] ) ? FFFF : 0000\nsigned\n
For each of 8 halfword slots:
. The operand from register RA is algebraically compared with the operand from
register RB. If the operand in register RA is greater than the operand in 
register RB, a result of all one bits (true) is produced. Otherwise, a result 
of all zero bits (false) is produced.
. The 16-bit result is placed in register RT.")
			    (list "cgthi"
				  "CGTHI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] > ext(s10) ) ? FFFF : 0000\n
For each of eight halfword slots:
. The value s10 is extended to 16 bits and algebraically compared with the 
value in register RA. If the value in register RA is greater than the s10 
value, a result of all one bits (true) is produced. Otherwise, a result of
all zero bits (false) is produced.
. The 16-bit result is placed in register RT.")
			    (list "cgt"
				  "CGT rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] > rb.w[n] ) ? FFFFFFFF : 00000000\nsigned\n
For each of four word slots:
. The operand from register RA is algebraically compared with the operand from
register RB. If the operand in register RA is greater than the operand in 
register RB, a result of all one bits (true) is produced. Otherwise, a result 
of all zero bits (false) is produced.
. The 32-bit result is placed in register RT.")
			    (list "cgti"
				  "CGTI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] > ext(s10) ) ? FFFFFFFF : 00000000\n
For each of four word slots:
. The value s10 is extended to 32 bits by sign extension and algebraically 
compared with the value in register RA. If the value in register RA is greater
than the s10 value, a result of all one bits (true) is produced. Otherwise, a
result of all zero bits (false) is produced.
. The 32-bit result is placed in register RT.")
			    (list "clgtb"
				  "CLGTB rt, ra, rb"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] > rb.b[n] ) ? FF : 00\nunsigned\n
For each of 16 byte slots:
. The operand from register RA is logically compared with the operand from 
register RB. If the operand in register RA is logically greater than the 
operand in register RB, a result of all one bits (true) is produced. Otherwise,
a result of all zero bits (false) is produced.
. The 8-bit result is placed in register RT.")
			    (list "clgtbi"
				  "CLGTBI rt, ra, u10"
				  "2"
				  "FX"
				  "rt.b[n] = ( ra.b[n] > (u10 & 0xFF) ) ? FF : 00\n
For each of 16 byte slots:
. The value in the rightmost 8 bits of the u10 value is logically compared with
the value in register RA. If the value in register RA is logically greater, a
result of all one bits (true) is produced. Otherwise, a result of all zero 
(false) bits is produced.
. The 8-bit result is placed in register RT.")
			    (list "clgth"
				  "CLGTH rt, ra, rb"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] > rb.h[n] ) ? FFFF : 0000\nunsigned\n
For each of eight halfword slots:
. The operand from register RA is logically compared with the operand from
register RB. If the operand in register RA is logically greater than the 
operand in register RB, a result of all one bits (true) is produced. Otherwise,
a result of all zero bits (false) is produced.
. The 16-bit result is placed in register RT.")
			    (list "clgthi"
				  "CLGTHI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.h[n] = ( ra.h[n] > ext(s10) ) ? FFFF : 0000\n
For each of eight halfword slots:
. The value s10 is extended to 16 bits by replicating the leftmost bit and 
logically compared with the value in register RA. If the value in register RA 
is logically greater than the I10 value, a result of all one bits (true) is 
produced. Otherwise, a result of all zero bits (false) is produced.
. The 16-bit result is placed in register RT.")
			    (list "clgt"
				  "CLGT rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] > rb.w[n] ) ? FFFFFFFF : 00000000\nunsigned\n
For each of four word slots:
. The operand from register RA is logically compared with the operand from 
register RB. If the operand in register RA is logically greater than the 
operand in register RB, a result of all one bits (true) is produced. Otherwise,
 a result of all zero bits (false) is produced.
. The 32-bit result is placed in register RT.")
			    (list "clgti"
				  "CLGTI rt, ra, s10"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra.w[n] > ext(s10) ) ? FFFFFFFF : 00000000\n
For each of four word slots:
. The value in the I10 field is extended to 32 bits by sign extension and 
logically compared with the value in register RA. If the value in register RA 
is logically greater than the I10 value, a result of all one bits (true) is 
produced. Otherwise, a result of all zero bits (false) is produced.
. The 32-bit result is placed in register RT.")
			    (list "br"
				  "BR brTo"
				  "4"
				  "BR"
				  "goto label addresses in words\n
Branch Relative instruction: Execution proceed with the target instruction")
			    (list "bra"
				  "BRA brTo"
				  ""
				  "BR"
				  "goto label addresses in words\n
Execution proceeds with the target instruction.")
			    (list "brsl"
				  "BRSL	rt, brTo"
				  "4"
				  "BR"
				  "gosub (rt.w[0] set with return address, rest cleared)\n
Execution proceeds with the target instruction. 
In addition, a link register is set.
The preferred slot of register RT is set to the address of the byte following 
the Branch Relative and Set Link instruction. The remaining slots of register
RT are set to zero.")
			    (list "brasl"
				  "BRASL rt, brTo"
				  "4"
				  "BR"
				  "gosub (rt.w[0] set with return address, rest cleared)\n
Execution proceeds with the target instruction. 
In addition, a link register is set.
The preferred slot of register RT is set to the address of the byte following 
the Branch Absolute and Set Link instruction. The remaining slots of register 
RT are set to zero.")
			    (list "bi"
				  "BI rt"
				  "?"
				  "BR"
				  "goto rt  register addresses in bytes\n
Execution proceeds with the instruction addressed by the preferred slot of 
register RA. The rightmost 2 bits of the value in register RA are ignored and
assumed to be zero.
Interrupts can be enabled or disabled with the E or D feature bits.")
			    (list "bie"
				  "BI rt"
				  "?"
				  "BR"
				  "goto rt  register addresses in bytes\n
Execution proceeds with the instruction addressed by the preferred slot of 
register RA. The rightmost 2 bits of the value in register RA are ignored and
assumed to be zero. Interrupts are enabled.")
			    (list "bid"
				  "BI rt"
				  "?"
				  "BR"
				  "goto rt  register addresses in bytes\n
Execution proceeds with the instruction addressed by the preferred slot of 
register RA. The rightmost 2 bits of the value in register RA are ignored and
assumed to be zero. Interrupts are disabled.")
			    (list "iret"
				  "IRET"
				  "?"
				  "??"
				  "return from interrupt\n
Execution proceeds with the instruction addressed by SRR0. RA is considered to
be a valid source whose value is ignored. 
Interrupts can be enabled or disabled with the E or D feature bits.")
			    (list "iretd"
				  "IRETD"
				  "?"
				  "??"
				  "return from interrupt, disable interrupts\n
Execution proceeds with the instruction addressed by SRR0. RA is considered to
be a valid source whose value is ignored. Interrupts are disabled.")
			    (list "irete"
				  "IRETE"
				  "?"
				  "??"
				  "return from interrupt, enable interrupts\n
Execution proceeds with the instruction addressed by SRR0. RA is considered to
be a valid source whose value is ignored. Interrupts are enabled.")
			    (list "bisled"
				  "BISLED rt, ra"
				  "4"
				  "BR"
				  "gosub ra if channel zero is non-zero\n(rt.w[0] set with return address, rest cleared)\n
The external condition is examined. If it is false, execution continues with 
the next sequential instruction. If the external condition is true, the 
effective address of the next instruction is taken from the preferred word slot
of register RA.
The address of the instruction following the bisled instruction is placed into 
the preferred word slot of register RT; the remainder of register RT is set to
zero.
If the branch is taken, interrupts can be enabled or disabled with the E or D 
feature bits.")
			    (list "bisl"
				  "BISL	rt, ra"
				  "4"
				  "BR"
				  "gosub ra (rt.w[0] set with return address, rest cleared)\n
The effective address of the next instruction is taken from the preferred word
slot of register RA, with the rightmost 2 bits assumed to be zero. The address
of the instruction following the bisl instruction is placed into the preferred
word slot of register RT. The remainder of register RT is set to zero.
Interrupts can be enabled or disabled with the E or D feature bits.")
			    (list "bisle"
				  "BISL	rt, ra"
				  "4"
				  "BR"
				  "gosub ra (rt.w[0] set with return address, rest cleared)\n
The effective address of the next instruction is taken from the preferred word
slot of register RA, with the rightmost 2 bits assumed to be zero. The address
of the instruction following the bisl instruction is placed into the preferred
word slot of register RT. The remainder of register RT is set to zero. 
Interrupts are enabled.")
			    (list "bisld"
				  "BISL	rt, ra"
				  "4"
				  "BR"
				  "gosub ra (rt.w[0] set with return address, rest cleared)\n
The effective address of the next instruction is taken from the preferred word
slot of register RA, with the rightmost 2 bits assumed to be zero. The address
of the instruction following the bisl instruction is placed into the preferred
word slot of register RT. The remainder of register RT is set to zero. 
Interrupts are disabled.")
			    (list "brnz"
				  "BRNZ	rt, brTo"
				  "4"
				  "BR"
				  "branch if rt.w[0] != 0\n
Examine the preferred slot; if it is not zero, proceed with the branch target.
Otherwise, proceed with the next instruction.")
			    (list "brz"
				  "BRZ rt, brTo"
				  "4"
				  "BR"
				  "branch if rt.w[0] == 0\n
Examine the preferred slot. If it is zero, proceed with the branch target. 
Otherwise, proceed with the next instruction.")
			    (list "brhnz"
				  "BRHNZ rt, brTo"
				  "4"
				  "BR"
				  "branch if rt.h[1] != 0\n
Examine the preferred slot. If the rightmost halfword is not zero, proceed with
the branch target. Otherwise, proceed with the next instruction.")
			    (list "brhz"
				  "BRHZ	rt, brTo"
				  "4"
				  "BR"
				  "branch if rt.h[1] == 0\n
Examine the preferred slot. If the rightmost halfword is zero, proceed with the
branch target. Otherwise, proceed with the next instruction.")
			    (list "biz"
				  "BIZ rt, ra"
				  "?"
				  "BR"
				  "branch to ra.w[0] if rt.w[0] == 0\n
If the preferred slot of register RT is not zero, execution proceeds with the 
next sequential instruction. Otherwise, execution proceeds at the address in 
the preferred slot of register RA, treating the rightmost 2 bits as zero. If 
the branch is taken, interrupts can be enabled or disabled with the E or D 
feature bits.")
			    (list "binz"
				  "BINZ	rt, ra"
				  "?"
				  "BR"
				  "branch to ra.w[0] if rt.w[0] != 0\n
If the preferred slot of register RT is zero, execution proceeds with the next
sequential instruction. Otherwise, execution proceeds at the address in the 
preferred slot of register RA, treating the rightmost 2 bits as zero. If the 
branch is taken, interrupts can be enabled or disabled with the E or D feature
bits.")
			    (list "bihz"
				  "BIHZ	rt, ra"
				  "?"
				  "BR"
				  "branch to ra.w[0] if rt.h[1] == 0\n
If the rightmost halfword of the preferred slot of register RT is not zero, 
execution proceeds with the next sequential instruction. Otherwise, execution 
proceeds at the address in the preferred slot of register RA, treating the 
rightmost 2 bits as zero. If the branch is taken, interrupts can be enabled or
disabled with the E or D feature bits.")
			    (list "bihnz"
				  "BIHNZ rt, ra"
				  "?"
				  "BR"
				  "branch to ra.w[0] if rt.h[1] != 0\n
If the rightmost halfword of the preferred slot of register RT is zero, 
execution proceeds with the next sequential instruction. Otherwise, execution
proceeds at the address in the preferred slot of register RA, treating the 
rightmost 2 bits as zero. If the branch is taken, interrupts can be enabled or
disabled with the E or D feature bits.")))


;;
;; Hint-for-Branch Instructions:
;;


(setq spu-opcode-help-hfbi (list
			    (list "hbr"
				  "HBR brFrom, ra"
				  "15"
				  "BR"
				  "branch hint for any BIxxx type branch\n
The address of the branch target is given by the contents of the preferred slot
of register RA. The brFrom value gives the signed word offset from the hbr 
instruction to the branch instruction.")
			    (list "hbrp"
				  "HBRP	label, ra"
				  "15"
				  "BR"
				  "inline prefetch?  What?  label and rt are ignored.\n
hbrp does not hint a branch. Instead, it hints that this is the proper 
implementation-specific moment to perform inline prefetching. Inline 
prefetching is the instruction fetch function necessary to run linearly 
sequential program text. To obtain optimal performance, some implementations of
the SPU may require help scheduling these inline prefetches of local storage 
when the program is also doing loads and stores. 
The instruction ignores the value of RA and the relative offset label must be 
set to zero.")
			    (list "hbra"
				  "HBRA	brFrom, brTo18"
				  "15"
				  "BR"
				  "branch hint for any BRAxx type branch\n
The address of the branch target is specified by an address in the 18 bitfield:
brTo16. The righmost 2 bits are cleared before used.")
			    (list "hbrr"
				  "HBRR	brFrom, brTo16"
				  "15"
				  "BR"
				  "branch hint for any BRxxx type branch\n
The address of the branch target is specified by a word offset given in the 
brTo18 value. The signed brTo16 field is added to the address of the hbrr 
instruction to determine the absolute address of the branch target.")))



;;
;; Floating-Point Instructions:
;;

;; General Floating Point Information:
;;   Smax: Maximum Positive Magnitude ( 0x 0080 0000 )
;;   Smin: Minimum Positive Magnitude ( 0x 7FFF FFFF )
;;   NaN:  is not supported as an operand and is not produced as a result.
;;   Inf:  is not supported. An operation that produces a magnitude greater than 
;;         the largest number representable in the target floating-point format 
;;         instead produces a number with the appropriate sign, the largest biased
;;         exponent, and a magnitude of all (binary) ones. It is important to note
;;         that the representation of Inf, which conforms to the IEEE standard, is
;;         interpreted by the SPU as a number that is smaller than the largest 
;;         number used on the SPU.
;;   Denorms are not supported and are treated as zero. Thus, an operation that 
;;         would generate a denorm under IEEE rules instead generates a positive 
;;         zero. If a denorm is used as an operand, it is treated as a zero.
;;   The only supported rounding mode is truncation (toward zero).


(setq spu-opcode-help-fpin (list
			    (list "fa"
				  "FA rt, ra, rb"
				  "6"
				  "SP"
				  "rt.f[n] = ra.f[n] + rb.f[n]\n
For each of the four word slots:
. The operand from register RA is added to the operand from register RB.
. The result is placed in register RT.
. If the magnitude of the result is greater than Smax, then Smax (with the 
correct sign) is produced as the result. If the magnitude of the result is less
than Smin, then zero is produced.

General Floating Point Information:
  Smax: Maximum Positive Magnitude ( 0x 0080 0000 )
  Smin: Minimum Positive Magnitude ( 0x 7FFF FFFF )")
			    (list "dfa"
				  "DFA rt, ra, rb"
				  "?"
				  "SP"
				  "rt = ra + rb\n
For each of two doubleword slots:
. The operand from register RA is added to the operand from register RB.
. The result is placed in register RT.")
			    (list "fs"
				  "FS rt, ra, rb"
				  "6"
				  "SP"
				  "rt.f[n] = ra.f[n] - rb.f[n]\n
For each of the four word slots:
. The operand from register RB is subtracted from the operand from register RA.
. The result is placed in register RT.
. If the magnitude of the result is greater than Smax, then Smax (with the 
correct sign) is produced as the result. If the magnitude of the result is less
than Smin, then zero is produced.

General Floating Point Information:
  Smax: Maximum Positive Magnitude ( 0x 0080 0000 )
  Smin: Minimum Positive Magnitude ( 0x 7FFF FFFF )")
			    (list "dfs"
				  "DFS rt, ra, rb"
				  "?"
				  "SP"
				  "rt = ra - rb\n
For each of two doubleword slots:
. The operand from register RB is subtracted from the operand from register RA.
. The result is placed in register RT.")
			    (list "fm"
				  "FM rt, ra, rb"
				  "6"
				  "SP"
				  "rt.f[n] = ra.f[n] * rb.f[n]\n
For each of the four word slots:
. The operand from register RA is multiplied by the operand from register RB.
. The result is placed in register RT.
. If the magnitude of the result is greater than Smax, then Smax (with the 
correct sign) is produced. If the magnitude of the result is less than Smin, 
then zero is produced.

General Floating Point Information:
  Smax: Maximum Positive Magnitude ( 0x 0080 0000 )
  Smin: Minimum Positive Magnitude ( 0x 7FFF FFFF )")
			    (list "dfm"
				  "DFM rt, ra, rb"
				  "?"
				  "SP"
				  "rt = ra * rb\n
For each of two doubleword slots:
. The operand from register RA is multiplied by the operand from register RB.
. The result is placed in register RT.")
			    (list "fma"
				  "FMA rt, ra, rb, rc"
				  "6"
				  "SP"
				  "rt.f[n] = ra.f[n] * rb.f[n] + rc.f[n]\n
For each of the four word slots:
. The operand from register RA is multiplied by the operand from register RB
and added to the operand from register RC. The multiplication is exact and not
subject to limits on its range.
. The result is placed in register RT.
. If the magnitude of the result of the addition is greater than Smax, then 
Smax (with the correct sign) is produced. If the magnitude of the result is 
less than Smin, then zero is produced.

General Floating Point Information:
  Smax: Maximum Positive Magnitude ( 0x 0080 0000 )
  Smin: Minimum Positive Magnitude ( 0x 7FFF FFFF )")
			    (list "dfma"
				  "DFMA rt, ra, rb"
				  "?"
				  "SP"
				  "rt = rt + ra * rb\n
For each of two doubleword slots:
. The operand from register RA is multiplied by the operand from register RB 
and added to the operand from register RT. The multiplication is exact and not
subject to limits on its range.
. The result is placed in register RT.")
			    (list "fnms"
				  "FNMS rt, ra, rb, rc"
				  "6"
				  "SP"
				  "rt.f[n] = -(ra.f[n] * rb.f[n] - rc.f[n])\nSubtract then Negate\n
For each of the four word slots:
. The operand from register RA is multiplied by the operand from register RB, 
and the product is subtracted from the operand from register RC. The result of
the multiplication is exact and not subject to limits on its range.
. The result is placed in register RT.
. If the magnitude of the result of the subtraction is greater than Smax, then
Smax (with the correct sign) is produced. If the magnitude of the result of the
subtraction is less than Smin, then zero is produced.

General Floating Point Information:
  Smax: Maximum Positive Magnitude ( 0x 0080 0000 )
  Smin: Minimum Positive Magnitude ( 0x 7FFF FFFF )")
			    (list "dfnms"
				  "DFNMS rt, ra, rb"
				  "?"
				  "SP"
				  "rt = -(-rt + ra * rb )\nSubtract then Negate\n
For each of two doubleword slots:
. The operand from register RA is multiplied by the operand from register RB. 
The operand from register RT is subtracted from the product. The result, which 
is placed in register RT, is usually obtained by negating the rounded result of
this multiply subtract operation. There is one exception: If the result is a 
QNaN, the sign bit of the result is zero.
. This instruction produces the same result as would be obtained by using the
Double Floating Multiply and Subtract instruction and then negates any result 
that is not a NaN.
. The multiplication is exact and not subject to limits on its range.")
			    (list "fms"
				  "FMS rt, ra, rb, rc"
				  "6"
				  "SP"
				  "rt.f[n] = ra.f[n] * rb.f[n] - rc.f[n]\n
For each of the four word slots:
. The operand from register RA is multiplied by the operand from register RB. 
The result of the multiplication is exact and not subject to limits on its 
range. The operand from register RC is subtracted from the product.
. The result is placed in register RT.
. If the magnitude of the result of the subtraction is greater than Smax, then
Smax (with the correct sign) is produced. If the magnitude of the result of the
subtraction is less than Smin, then zero is produced.

General Floating Point Information:
  Smax: Maximum Positive Magnitude ( 0x 0080 0000 )
  Smin: Minimum Positive Magnitude ( 0x 7FFF FFFF )")
			    (list "dfms"
				  "DFMS rt, ra, rb"
				  "?"
				  "SP"
				  "rt = -rt + ra * rb\n
For each of two doubleword slots:
. The operand from register RA is multiplied by the operand from register RB. 
The multiplication is exact and not subject to limits on its range. The operand
from register RT is subtracted from the product.
. The result is placed in register RT.")
			    (list "dfnma"
				  "DFNMA rt, ra, rb"
				  "?"
				  "SP"
				  "rt = -( rt + ra * rb )\nAdd then Negate\n
For each of two doubleword slots:
. The operand from register RA is multiplied by the operand from register RB 
and added to the operand from register RT. The multiplication is exact and not
subject to limits on its range. The result, which is placed in register RT, is
usually obtained by negating the rounded result of this multiply add operation.
. There is one exception: If the result is a QNaN, the sign bit of the result 
is 0.
. This instruction produces the same result as would be obtained by using the
Double Floating Multiply and Add instruction and then negating any result that
is not a NaN.")
			    (list "frest"
				  "FREST rt, ra"
				  "4"
				  "SH"
				  "Form Reciprocal ESTimate for 1/ra.f[n]\nmust complete with FI instruction\n
  1/x:	  FREST rt, x          ; table-lookup
          FI	ra, x, rt      ; interpolation . ra is good to 12bits precision
          FNMS	rb, ra, x, One ; rb = - (x * ra - 1.0)
          FMA	ra, rb, ra, ra ; now ra is good to 24 bits precision

For each of four word slots:
. The operand in register RA is used to compute a base and a step for 
estimating the reciprocal of the operand. The result, in the form shown below, is placed in register RT. 
S is the sign bit of the base result.
     | sgn | biased-exp | Base fraction | Step Fraction |
     |  0  |  1  ->  8  |     9 -> 21   |   22 -> 31    |
. The base result is expressed as a floating-point number with 13 bits in the 
fraction, rather than the usual 23 bits. The remaining 10 bits of the fraction
are used to encode the magnitude of the step as a 10-bit denormal fraction; the
exponent is that of the base.
. Let x be the initial value in register RA. The result placed in RT, which is
interpreted as a regular IEEE number, provides an estimate of the reciprocal of
a nonzero x.")
			    (list "frsqest"
				  "FRSQEST rt, ra"
				  "4"
				  "SH"
				  "Form Reciprocal SQuare root ESTimate for 1/sqrt(ra.f[n])\nmust complete with FI instruction\n
  1/sqrt(x):    FRSQEST rt, x           ; table-lookup
                FI	ra, x, rt       ; ra is good to 12 bits precision
                FM	rb, ra, x       ; (ra and rt can share register)
                FM	rc, ra, OneHalf	; (rb and x can share register)
                FNMS	rb, rb, ra, One ;
                FMA	ra, rc, rb, ra  ; now ra is good to 24 bits precision

For each of four word slots:
. The operand in register RA is used to compute a base and step for estimating
the reciprocal of the square root of the absolute value of the operand. The 
result is placed in register RT. The sign bit (S) will be zero.
. see also: freq")
			    (list "fi"
				  "FI rt, ra, rb"
				  "7"
				  "FI"
				  "use after FREST or FRSQEST\n
For each of four word slots:
. The operand in register RB is disassembled to produce a floating-point base 
and step according to the format described in Floating Reciprocal ESTimate ; 
that is, a sign, biased exponent, base fraction, and step fraction. (cf: FREST)
. Bits 13 to 31 of register RA are taken to represent a fraction, Y, whose 
binary point is to the left of bit 13; that is, Y = 0.RA13:31.
The result is computed by the following equation:
RT = (-1)^S * (1.BaseFraction - 0.000StepFraction * Y) * 2^(BiasedExponent-127)

If the operand in register RB is the result of an frest or frsqest instruction
with the operand from register RA, then the result of the fi instruction placed
in register RT provides a more accurate estimation.")
			    (list "csflt"
				  "CSFLT rt, ra, scale8"
				  "7"
				  "FI"
				  "signed int to float, specify precision\n
For each of four word slots:
. The signed 32-bit integer value in register RA is converted to an 
extended-range, single-precision, floating-point value.
. The result is divided by 2^scale and placed in register RT. The factor scale
is an 8-bit unsigned integer and must be in the range of 0 to 127, if not the 
result of the operation is undefined.
. The scale factor describes the number of bit positions between the binary 
point of the magnitude and the right end of register RA. A scale factor of zero
means that the register RA value is an unscaled integer.")
			    (list "cflts"
				  "CFLTS rt, ra, scale8"
				  "7"
				  "FI"
				  "float to signed int, specify precision\n
For each of four word slots:
. The extended-range, single-precision, floating-point value in register RA is
multiplied by 2^scale. The factor scale is an 8-bit unsigned integer and must 
be in the range 0 to 127 otherwise the result of the operation is undefined.
. The product is converted to a signed 32-bit integer. If the intermediate 
result is greater than (2^31 - 1), it saturates to (2^31 - 1); if it is less 
than -2^31, it saturates to -2^31. The resulting signed integer is placed in 
register RT.
. The scale factor is the location of the binary point of the result, expressed
as the number of bit positions from the right end of the register RT. A scale
factor of zero means that the value in register RT is an unscaled integer.")
			    (list "cuflt"
				  "CUFLT rt, ra, precis"
				  "7"
				  "FI"
				  "unsigned int to float, specify precision\n
For each of four word slots:
. The unsigned 32-bit integer value in register RA is converted to an 
extended-range, single-precision, floating-point value.
. The result is divided by 2^scale and placed in register RT. The factor scale
is an 8-bit unsigned integer and must be in the range of 0 to 127, if not the 
result of the operation is undefined.
. The scale factor describes the number of bit positions between the binary 
point of the magnitude and the right end of register RA. A scale factor of zero
means that the register RA value is an unscaled integer.")
			    (list "cfltu"
				  "CFLTU rt, ra, precis"
				  "7"
				  "FI"
				  "float to unsigned int, specify precision\n
For each of four word slots:
. The extended-range, single-precision, floating-point value in register RA is
multiplied by 2^scale. The factor scale is an 8-bit unsigned integer and must 
be in the range 0 to 127 otherwise the result of the operation is undefined.
. The product is converted to an unsigned 32-bit integer. If the intermediate
result is greater than (2^32 - 1) it saturates to (2^32 - 1). If the product is
negative, it saturates to zero. The resulting unsigned integer is placed in 
register RT.
. The scale factor is the location of the binary point of the result, expressed
as the number of bit positions from the right end of the register RT. A scale
factor of zero means that the value in register RT is an unscaled integer.")
			    (list "frds"
				  "FRDS	rt, ra"
				  "?"
				  "FI"
				  "rt.w[2n] = ra.rc[n]; rt.w[2n+1] = 0\ndouble to single\n
For each of two doubleword slots:
. The double-precision value in register RA is rounded to a single-precision, 
floating-point value and placed in the left word slot. Zeros are placed in the
right word slot.
. The rounding is performed in accordance with the rounding mode specified in
the Floating-Point Status Register. Double-precision exceptions are detected 
and accumulated in the Floating-Point Unit (FPU) Status Register.")
			    (list "fesd"
				  "FESD	rt, ra"
				  "?"
				  "FI"
				  "rt.rc[n] = ra.w[2n+1]\nsingle to double\n
For each of two doubleword slots:
. The single-precision value in the left slot of register RA is converted to a
double-precision, floating-point value and placed in register RT. The contents
of the right word slot are ignored.
. Double-precision exceptions are detected and accumulated in the FPU Status
Register.")
			    (list "fceq"
				  "FCEQ	rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra[n] == rb[n] ) ? FFFFFFFF : 00000000\n
For each of four word slots:
. The floating-point value from register RA is compared with the floating-point
value from register RB. If the values are equal, a result of all ones (true) is
produced in register RT. Otherwise, a result of zero (false) is produced in 
register RT. Two zeros always compare equal independent of their fractions and
signs.
. This instruction is always executed in extended-range mode and ignores the 
setting of the mode bit.")
			    (list "fcmeq"
				  "FCMEQ rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( abs(ra[n]) == abs(rb[n]) ) ? FFFFFFFF : 00000000\n
For each of four word slots:
. The absolute value of the floating-point number in register RA is compared 
with the absolute value of the floating-point number in register RB. If the
absolute values are equal, a result of all ones (true) is produced in 
register RT. Otherwise, a result of zero (false) is produced in register RT. 
Two zeros always compare equal independent of their fractions and signs.
. This instruction is always executed in extended-range mode and ignores the
setting of the mode bit.")
			    (list "fcgt"
				  "FCGT	rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( ra[n] > rb[n] ) ? FFFFFFFF : 00000000\n
For each of four word slots:
. The floating-point value in register RA is compared with the floating-point
value in register RB. If the value in RA is greater than the value in RB, a
result of all ones (true) is produced in register RT. Otherwise, a result of
zero (false) is produced in register RT. Two zeros never compare greater than
independent of their sign bits and fractions.
. This instruction is always executed in extended-range mode, and ignores the
setting of the mode bit.")
			    (list "fcmgt"
				  "FCMGT rt, ra, rb"
				  "2"
				  "FX"
				  "rt.w[n] = ( abs(ra[n]) > abs(rb[n]) ) ? FFFFFFFF : 00000000\n
For each of four word slots:
. The absolute value of the floating-point number in register RA is compared 
with the absolute value of the floating-point number in register RB. If the 
absolute value of the value from register RA is greater than the absolute value
of the value from register RB, a result of all ones (true) is produced in 
register RT. Otherwise, a result of zero (false) is produced in register RT. 
Two zeros never compare greater than, independent of their fractions and signs.
. This instruction is always executed in extended-range mode, and ignores the
setting of the mode bit.")
			    (list "fscrwr"
				  "FSCRWR rt"
				  "?"
				  "??"
				  "write ...\n
The 128-bit value of register RA is written into the FPSCR. The value of the 
unused bits in the FPSCR is undefined. RT is a false target. Implementations 
can schedule instructions as though this instruction produces a value into RT.
Programs can avoid unnecessary delay by programming RT so as not to appear to
source data for nearby subsequent instructions. False targets are not written.")
			    (list "fscrrd"
				  "FSCRRD rt"
				  "?"
				  "??"
				  "read floating point status flags\n
This instruction reads the value of the FPSCR. In the result, the unused bits 
of the FPSCR are forced to zero. The result is placed in the register RT.")))


;;
;; Control Instructions:
;;


(setq spu-opcode-help-ctrl (list
			    (list "lnop"
				  "LNOP"
				  "1"
				  "??"
				  "nop instruction\n
This instruction has no effect on the execution of the program. It exists to 
provide implementation-defined control of instruction issuance.")
			    (list "stop"
				  "STOP"
				  "1"
				  "BR"
				  "stop instruction\n
Execution of the program in the SPU stops, and the external environment is 
signaled. No further instructions are executed.")
			    (list "stop"
				  "STOP	u14"
				  "?"
				  "??"
				  "stop and signal\n
Execution of the program in the SPU stops, and the external environment is 
signaled. No further instructions are executed.
The value u14 is written in the SPU status register and the an interrupt is
sent to the PPU.")
			    (list "stopd"
				  "STOPD rt, ra, rb"
				  "?"
				  "??"
				  "stop and signal when registers rt,ra,rb dependencies clear\n
Execution of the program in the SPU stops.
This instruction differs from stop only in that, in typical implementations, 
instructions with dependencies can be replaced with stopd to create a 
breakpoint without affecting the instruction timings.")
			    (list "nop"
				  "NOP"
				  "1"
				  "??"
				  "nop instruction\n
This instruction has no effect on the execution of the program. It exists to
provide implementation-defined control of instruction issuance.")
			    (list "nop"
				  "NOP rt"
				  "1"
				  "??"
				  "nop instruction\n
This instruction has no effect on the execution of the program. It exists to
provide implementation-defined control of instruction issuance. RT is a false 
target. Implementations can schedule instructions as though this instruction 
produces a value into RT. Programs can avoid unnecessary delay by programming
RT so as not to appear to source data for nearby subsequent instructions. False
targets are not written.")
			    (list "sync"
				  "SYNC"
				  "?"
				  "??"
				  "wait for data stores to finish\n
This instruction has no effect on the execution of the program other than to 
cause the processor to wait until all pending store instructions have completed
before fetching the next sequential instruction. This instruction must be used
following a store instruction that modifies the instruction stream.")
			    (list "syncc"
				  "SYNCC chn?"
				  "?"
				  "??"
				  "synchronize channel, then do SYNC\n(SPU doc does not specify chn)\n
This instruction has no effect on the execution of the program other than to
cause the processor to wait until all pending store instructions have completed
before fetching the next sequential instruction. This instruction must be used
following a store instruction that modifies the instruction stream.
The C feature bit causes channel synchronization to occur before instruction 
synchronization occurs. Channel synchronization allows an SPU state modified 
through channel instructions to affect execution.")
			    (list "dsync"
				  "DSYNC"
				  "?"
				  "??"
				  "wait for data loads to finish\n
This instruction forces all earlier load, store, and channel instructions to 
complete before proceeding. No subsequent load, store, or channel instructions
can start until the previous instructions complete. The dsync instruction 
allows SPU software to ensure that the local storage data would be consistent 
if it were observed by another entity. This instruction does not affect any
prefetching of instructions that the processor might have done.")
			    (list "mfspr"
				  "MFSPR rt, spr"
				  "?"
				  "??"
				  "move from special purpose register\n
Special-Purpose Register SA is copied into register RT. If SPR SA is not 
defined, zeros are supplied.")
			    (list "mtspr"
				  "MTSPR spr, rt"
				  "?"
				  "??"
				  "move to special purpose register\n
The contents of register RT is written to Special-Purpose Register SA. If SPR
SA is not defined, no operation is performed.")))


;;
;; Channel Instructions:
;;

(setq spu-opcode-help-chin (list 
			    (list "rdch"
				  "RDCH	rt, ca"
				  "?"
				  "??"
				  "read from channel ca\n
The SPU waits for data to become available in channel CA (capacity is 
available). When data is available to the channel, it is moved from the channel
and placed into register RT.
If the channel designated by the CA field is not a valid, readable channel, the
SPU will stop on or after the rdch instruction.
Note: The SPU ISA defines the rdch and wrch instructions as 128-bit operations.
An implementation might define 32-bit wide channels. In that case, the 32-bit
value occupies the preferred slot; the other slots return zeros.")
			    (list "rchcnt"
				  "RCHCNT rt, ca"
				  "?"
				  "??"
				  "read channel ca count\n
The channel capacity of channel CA is placed into the preferred slot of 
register RT. The channel capacity of unimplemented channels is zero.")
			    (list "wrch"
				  "WRCH	ca, rt"
				  "?"
				  "??"
				  "write to channel ca\n
The SPU waits for capacity to become available in channel CA before executing 
the wrch instruction. When capacity is available in the channel, the contents 
of register RT are placed into channel CA. Channel writes targeting channels 
that are not valid writable channels cause the SPU to stop on or after the wrch
instruction.")))

;;
;; Control Instructions:
;;

(setq spu-opcode-help 
      (append spu-opcode-help-mops
	      spu-opcode-help-cfin
	      spu-opcode-help-ilin
	      spu-opcode-help-srin
	      spu-opcode-help-cbhi
	      spu-opcode-help-hfbi
	      spu-opcode-help-fpin
	      spu-opcode-help-ctrl
	      spu-opcode-help-chin))





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