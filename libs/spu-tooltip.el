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
				  "BR	brTo"
				  "4"
				  "BR"
				  "goto label addresses in words")
			    (list "brsl"
				  "BRSL	i, brTo"
				  "4"
				  "BR"
				  "gosub (i.w[0] set with return address, rest cleared)")
			    (list "brhnz"
				  "BRHNZ	i, brTo"
				  "4"
				  "BR"
				  "branch if i.h[1] != 0")
			    (list "brhz"
				  "BRHZ	i, brTo"
				  "4"
				  "BR"
				  "branch if i.h[1] == 0")
			    (list "brnz"
				  "BRNZ	i, brTo"
				  "4"
				  "BR"
				  "branch if i.w[0] != 0")
			    (list "brz"
				  "BRZ	i, brTo"
				  "4"
				  "BR"
				  "branch if i.w[0] == 0")
			    (list "hbrr"
				  "HBRR	brFrom, brTo"
				  "15"
				  "BR"
				  "branch hint for any BRxxx type branch")
			    (list "bi"
				  "BI	i"
				  "?"
				  "BR"
				  "goto i  register addresses in bytes")
			    (list "bisl"
				  "BISL	i, j"
				  "4"
				  "BR"
				  "gosub j (i.w[0] set with return address, rest cleared)")
			    (list "bisled"
				  "BISLED i, j"
				  "4"
				  "BR"
				  "gosub j if channel zero is non-zero\n(i.w[0] set with return address, rest cleared)")
			    (list "bihnz"
				  "BIHNZ	i, j"
				  "?"
				  "BR"
				  "branch to j.w[0] if i.h[1] != 0")
			    (list "bihz"
				  "BIHZ	i, j"
				  "?"
				  "BR"
				  "branch to j.w[0] if i.h[1] == 0")
			    (list "binz"
				  "BINZ	i, j"
				  "?"
				  "BR"
				  "branch to j.w[0] if i.w[0] != 0")
			    (list "biz"
				  "BIZ	i, j"
				  "?"
				  "BR"
				  "branch to j.w[0] if i.w[0] == 0")
			    (list "hbr"
				  "HBR	brFrom, j"
				  "15"
				  "BR"
				  "branch hint for any BIxxx type branch")
			    (list "hbrp"
				  "HBRP	label, j"
				  "15"
				  "BR"
				  "inline prefetch?  What?  label and i are ignored.")
			    (list "bra"
				  "BRA	brTo"
				  ""
				  "BR"
				  "goto label addresses in words")
			    (list "brasl"
				  "BRASL	i, brTo"
				  "4"
				  "BR"
				  "gosub (i.w[0] set with return address, rest cleared)")
			    (list "hbra"
				  "HBRA	brFrom, brTo"
				  "15"
				  "BR"
				  "branch hint for any BRAxx type branch")
			    (list "heq"
				  "HEQ	i, j"
				  "2"
				  "FX"
				  "halt if i == j")
			    (list "heqi"
				  "HEQI	i, s10"
				  "2"
				  "FX"
				  "halt if i == ext(s10)")
			    (list "hgt"
				  "HGT	i, j"
				  "2"
				  "FX"
				  "halt if i > j -- signed")
			    (list "hgti"
				  "HGTI	i, s10"
				  "2"
				  "FX"
				  "halt if i > ext(s10) -- signed")
			    (list "hlgt"
				  "HLGT	i, j"
				  "2"
				  "FX"
				  "halt if i > j -- unsigned ('Logical')")
			    (list "hlgti"
				  "HLGTI	i, s10"
				  "2"
				  "FX"
				  "halt if i > ext(s10) -- unsigned, even though sign-extended")
			    (list "stop"
				  "STOP"
				  "1"
				  "BR"
				  "stop instruction")
			    (list "fa"
				  "FA      a, b, c"
				  "6"
				  "SP"
				  "a.f[n] = b.f[n] + c.f[n]")
			    (list "fs"
				  "FS      a, b, c"
				  "6"
				  "SP"
				  "a.f[n] = b.f[n] - c.f[n]")
			    (list "fm"
				  "FM	a, b, c"
				  "6"
				  "SP"
				  "a.f[n] = b.f[n] * c.f[n]")
			    (list "fma"
				  "FMA     a, b, c, d"
				  "6"
				  "SP"
				  "a.f[n] = b.f[n] * c.f[n] + d.f[n]")
			    (list "fms"
				  "FMS     a, b, c, d"
				  "6"
				  "SP"
				  "a.f[n] = b.f[n] * c.f[n] - d.f[n]")
			    (list "fnms"
				  "FNMS    a, b, c, d"
				  "6"
				  "SP"
				  "a.f[n] = -(b.f[n] * c.f[n] - d.f[n])\nSubtract then Negate")
			    (list "dfa"
				  "DFA	a, b, c"
				  "?"
				  "SP"
				  "a = b + c")
			    (list "dfs"
				  "DFS     a, b, c"
				  "?"
				  "SP"
				  "a = b - c")
			    (list "dfm"
				  "DFM     a, b, c"
				  "?"
				  "SP"
				  "a = b * c")
			    (list "dfma"
				  "DFMA    a, b, c"
				  "?"
				  "SP"
				  "a = a + b * c")
			    (list "dfms"
				  "DFMS    a, b, c"
				  "?"
				  "SP"
				  "a = -a + b * c")
			    (list "dfnma"
				  "DFNMA   a, b, c"
				  "?"
				  "SP"
				  "a = -( a + b * c )\nAdd then Negate")
			    (list "dfnms"
				  "DFNMS   a, b, c"
				  "?"
				  "SP"
				  "a = -(-a + b * c )\nSubtract then Negate")
			    (list "lqa"
				  "LQA	i, label18"
				  "6"
				  "LS"
				  "addr = label18")
			    (list "lqd"
				  "LQD	i, quad(j)"
				  "6"
				  "LS"
				  "addr = quad * 16 + j.w[0]")
			    (list "lqr"
				  "LQR	i, label14"
				  "6"
				  "LS"
				  "addr = ext(label14) + PC")
			    (list "lqx"
				  "LQX	i, j, k"
				  "6"
				  "LS"
				  "addr = j.w[0] + k.w[0]")
			    (list "stqa"
				  "STQA  i, label18"
				  "6"
				  "LS"
				  "addr = label18 * 4")
			    (list "stqd"
				  "STQD  i, quad(j)"
				  "6"
				  "LS"
				  "addr = quad * 16 + j.w[0]")
			    (list "stqr"
				  "STQR  i, label14"
				  "6"
				  "LS"
				  "addr = ext(label14) + PC")
			    (list "stqx"
				  "STQX  i, j, k"
				  "6"
				  "LS"
				  "addr = j.w[0] + k.w[0]")
			    (list "cntb"
				  "CNTB	i, j"
				  "4"
				  "BO"
				  "i.b[n] = numOneBits( j.b[n] )")
			    (list "avgb"
				  "AVGB	i, j, k"
				  "4"
				  "BO"
				  "i.b[n] = ( j.b[n] + k.b[n] + 1 ) / 2")
			    (list "absdb"
				  "ABSDB	i, j, k"
				  "4"
				  "BO"
				  "i.b[n] = abs( j.b[n] - k.b[n] )")
			    (list "sumb"
				  "SUMB	i, j, k"
				  "4"
				  "BO"
				  "i.H[n] = ( n & 1 == 0 ) ? sum(k.b[2n..2n+3]) : sum(j.b[2n-2..2n+1])")
			    (list "shlh"
				  "SHLH	i, j, k"
				  "4"
				  "WS"
				  "i.h[n] = j.h[n] << ( k.h[n] & 0x1F )")
			    (list "shlhi"
				  "SHLHI	i, j, imm"
				  "4"
				  "WS"
				  "i.h[n] = j.h[n] << ( imm & 0x1F )")
			    (list "shl"
				  "SHL     i, j, k"
				  "4"
				  "WS"
				  "i.w[n] = j.w[n] << ( k.w[n] & 0x3F )")
			    (list "shli"
				  "SHLI    i, j, imm"
				  "4"
				  "WS"
				  "i.w[n] = j.w[n] << ( imm & 0x3F )")
			    (list "roth"
				  "ROTH	i, j, k"
				  "4"
				  "WS"
				  "i.h[n] = j.h[n] <^ ( k.h[n] & 0x0F )\n<^ is an idiosyncratic symbol for rotate")
			    (list "rothi"
				  "ROTHI	i, j, imm"
				  "4"
				  "WS"
				  "i.h[n] = j.h[n] <^ ( imm & 0x0F )\n<^ is an idiosyncratic symbol for rotate")
			    (list "rot"
				  "ROT     i, j, k"
				  "4"
				  "WS"
				  "i.w[n] = j.w[n] <^ ( k.w[n] & 0x1F )\n<^ is an idiosyncratic symbol for rotate")
			    (list "roti"
				  "ROTI    i, j, imm"
				  "4"
				  "WS"
				  "i.w[n] = j.w[n] <^ ( imm & 0x1F )\n<^ is an idiosyncratic symbol for rotate")
			    (list "rothm"
				  "ROTHM	i, j, k"
				  "4"
				  "WS"
				  "i.h[n] = j.h[n] >> ( -k.h[n] & 0x1F )\nshift right logical")
			    (list "rothmi"
				  "ROTHMI	i, j, imm"
				  "4"
				  "WS"
				  "i.h[n] = j.h[n] >> ( -imm & 0x1F )")
			    (list "rotm"
				  "ROTM    i, j, k"
				  "4"
				  "WS"
				  "i.w[n] = j.w[n] >> ( -k.w[n] & 0x3F )")
			    (list "rotmi"
				  "ROTMI   i, j, imm"
				  "4"
				  "WS"
				  "i.w[n] = j.w[n] >> ( -imm & 0x3F )")
			    (list "rotmah"
				  "ROTMAH	i, j, k"
				  "4"
				  "WS"
				  "i.h[n] = j.h[n] >> ( -k.h[n] & 0x1F )\nshift right arithmetic")
			    (list "rotmahi"
				  "ROTMAHI	i, j, imm"
				  "4"
				  "WS"
				  "i.h[n] = j.h[n] >> ( -imm & 0x1F )")
			    (list "rotma"
				  "ROTMA   i, j, k"
				  "4"
				  "WS"
				  "i.w[n] = j.w[n] >> ( -k.w[n] & 0x3F )")
			    (list "rotmai"
				  "ROTMAI  i, j, imm"
				  "4"
				  "WS"
				  "i.w[n] = j.w[n] >> ( -imm & 0x3F )")
			    (list "mpy"
				  "MPY	i, j, k"
				  "7"
				  "FI"
				  "i.w[n] = j.h[2n+1] * k.h[2n+1]\nmultiply lower halves signed")
			    (list "mpyi"
				  "MPYI    i, j, s10"
				  "7"
				  "FI"
				  "i.w[n] = j.h[2n+1] * ext(s10)\nmultiply lower halves signed immediate")
			    (list "mpyu"
				  "MPYU    i, j, k"
				  "7"
				  "FI"
				  "i.w[n] = j.h[2n+1] * k.h[2n+1]\nmultiply lower halves unsigned")
			    (list "mpyui"
				  "MPYUI   i, j, s10"
				  "7"
				  "FI"
				  "i.w[n] = j.h[2n+1] * ext(s10)\nmultiply lower halves unsigned immediate\n(even though immediate is sign-extended)")
			    (list "mpya"
				  "MPYA    i, j, k, l"
				  "7"
				  "FI"
				  "i.w[n] = j.h[2n+1] * k.h[2n+1] + l.w[n]	multiply lower halves, add word")
			    (list "mpys"
				  "MPYS    i, j, k"
				  "7"
				  "FI"
				  "i.w[n] = j.h[2n+1] * k.h[2n+1] >> 16\nmultiply lower halves, shift result down 16 with sign extend")
			    (list "mpyh"
				  "MPYH    i, j, k"
				  "7"
				  "FI"
				  "i.w[n] = j.h[2n] * k.h[2n+1] << 16\nmultiply upper half j by lower half k, shift up 16\n(verified; SPU_assembly_language_0.6.pfd is wrong)")
			    (list "mpyhh"
				  "MPYHH   i, j, k"
				  "7"
				  "FI"
				  "i.w[n] = j.h[2n] * k.h[2n]\nmultiply upper halves signed")
			    (list "mpyhhu"
				  "MPYHHU  i, j, k"
				  "7"
				  "FI"
				  "i.w[n] = j.h[2n] * k.h[2n]\nmultiply upper halves unsigned")
			    (list "mpyhha"
				  "MPYHHA  i, j, k"
				  "7"
				  "FI"
				  "i.w[n] += j.h[2n] * k.h[2n]\nmultiply/accumulate upper halves")
			    (list "mpyhhau"
				  "MPYHHAU i, j, k"
				  "7"
				  "FI"
				  "i.w[n] += j.h[2n] * k.h[2n]\nmultiply/accumulate upper halves unsigned")
			    (list "fi"
				  "FI	a, b, c"
				  "7"
				  "FI"
				  "use after FREST or FRSQEST")
			    (list "cuflt"
				  "CUFLT	a, j, precis"
				  "7"
				  "FI"
				  "unsigned int to float, specify precision")
			    (list "csflt"
				  "CSFLT	a, j, precis"
				  "7"
				  "FI"
				  "signed int to float, specify precision")
			    (list "cfltu"
				  "CFLTU   i, b, precis"
				  "7"
				  "FI"
				  "float to unsigned int, specify precision")
			    (list "cflts"
				  "CFLTS   i, b, precis"
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
				  "CEQB	i, j, k"
				  "2"
				  "FX"
				  "i.b[n] = ( j.b[n] == k.b[n] ) ? FF : 00")
			    (list "ceqbi"
				  "CEQBI	i, j, su8"
				  "2"
				  "FX"
				  "i.b[n] = ( j.b[n] == su8 ) ? FF : 00")
			    (list "cgtb"
				  "CGTB    i, j, k"
				  "2"
				  "FX"
				  "i.b[n] = ( j.b[n] > k.b[n] ) ? FF : 00\nsigned")
			    (list "cgtbi"
				  "CGTBI   i, j, su8"
				  "2"
				  "FX"
				  "i.b[n] = ( j.b[n] > su8 ) ? FF : 00")
			    (list "clgtb"
				  "CLGTB   i, j, k"
				  "2"
				  "FX"
				  "i.b[n] = ( j.b[n] > k.b[n] ) ? FF : 00\nunsigned")
			    (list "clgtbi"
				  "CLGTBI  i, j, su8"
				  "2"
				  "FX"
				  "i.b[n] = ( j.b[n] > su8 ) ? FF : 00")
			    (list "ceqh"
				  "CEQH    i, j, k"
				  "2"
				  "FX"
				  "i.h[n] = ( j.h[n] == k.h[n] ) ? FFFF : 0000")
			    (list "ceqhi"
				  "CEQHI   i, j, s10"
				  "2"
				  "FX"
				  "i.h[n] = ( j.h[n] == ext(s10) ) ? FFFF : 0000")
			    (list "cgth"
				  "CGTH    i, j, k"
				  "2"
				  "FX"
				  "i.h[n] = ( j.h[n] > k.h[n] ) ? FFFF : 0000\nsigned")
			    (list "cgthi"
				  "CGTHI   i, j, s10"
				  "2"
				  "FX"
				  "i.h[n] = ( j.h[n] > ext(s10) ) ? FFFF : 0000")
			    (list "clgth"
				  "CLGTH   i, j, k"
				  "2"
				  "FX"
				  "i.h[n] = ( j.h[n] > k.h[n] ) ? FFFF : 0000\nunsigned")
			    (list "clgthi"
				  "CLGTHI  i, j, s10"
				  "2"
				  "FX"
				  "i.h[n] = ( j.h[n] > ext(s10) ) ? FFFF : 0000")
			    (list "ceq"
				  "CEQ     i, j, k"
				  "2"
				  "FX"
				  "i.w[n] = ( j.w[n] == k.w[n] ) ? FFFFFFFF : 00000000")
			    (list "ceqi"
				  "CEQI    i, j, s10"
				  "2"
				  "FX"
				  "i.w[n] = ( j.w[n] == ext(s10) ) ? FFFFFFFF : 00000000")
			    (list "cgt"
				  "CGT     i, j, k"
				  "2"
				  "FX"
				  "i.w[n] = ( j.w[n] > k.w[n] ) ? FFFFFFFF : 00000000\nsigned")
			    (list "cgti"
				  "CGTI    i, j, s10"
				  "2"
				  "FX"
				  "i.w[n] = ( j.w[n] > ext(s10) ) ? FFFFFFFF : 00000000")
			    (list "clgt"
				  "CLGT    i, j, k"
				  "2"
				  "FX"
				  "i.w[n] = ( j.w[n] > k.w[n] ) ? FFFFFFFF : 00000000\nunsigned")
			    (list "clgti"
				  "CLGTI   i, j, s10"
				  "2"
				  "FX"
				  "i.w[n] = ( j.w[n] > ext(s10) ) ? FFFFFFFF : 00000000")
			    (list "fceq"
				  "FCEQ	i, b, c"
				  "2"
				  "FX"
				  "i.w[n] = ( b[n] == c[n] ) ? FFFFFFFF : 00000000")
			    (list "fcmeq"
				  "FCMEQ	i, b, c"
				  "2"
				  "FX"
				  "i.w[n] = ( abs(b[n]) == abs(c[n]) ) ? FFFFFFFF : 00000000")
			    (list "fcgt"
				  "FCGT	i, b, c"
				  "2"
				  "FX"
				  "i.w[n] = ( b[n] > c[n] ) ? FFFFFFFF : 00000000")
			    (list "fcmgt"
				  "FCMGT	i, b, c"
				  "2"
				  "FX"
				  "i.w[n] = ( abs(b[n]) > abs(c[n]) ) ? FFFFFFFF : 00000000")
			    (list "and"
				  "AND	i, j, k"
				  "2"
				  "FX"
				  "i = j & k")
			    (list "nand"
				  "NAND    i, j, k"
				  "2"
				  "FX"
				  "i = ~(j & k)")
			    (list "andc"
				  "ANDC    i, j, k"
				  "2"
				  "FX"
				  "i = j & ~k")
			    (list "andbi"
				  "ANDBI   i, j, u8"
				  "2"
				  "FX"
				  "i.b[n] = j.b[n] & u8")
			    (list "andhi"
				  "ANDHI   i, j, s10"
				  "2"
				  "FX"
				  "i.h[n] = j.h[n] & ext(s10)")
			    (list "andi"
				  "ANDI    i, j, s10"
				  "2"
				  "FX"
				  "i.w[n] = j.w[n] & ext(s10)")
			    (list "or"
				  "OR      i, j, k"
				  "2"
				  "FX"
				  "i = j | k")
			    (list "nor"
				  "NOR     i, j, k"
				  "2"
				  "FX"
				  "i = ~(j | k)")
			    (list "orc"
				  "ORC     i, j, k"
				  "2"
				  "FX"
				  "i = j | ~j")
			    (list "orbi"
				  "ORBI    i, j, u8"
				  "2"
				  "FX"
				  "i.b[n] = j.b[n] | u8")
			    (list "orhi"
				  "ORHI    i, j, s10"
				  "2"
				  "FX"
				  "i.h[n] = j.h[n] | ext(s10)")
			    (list "ori"
				  "ORI     i, j, s10"
				  "2"
				  "FX"
				  "i.w[n] = j.w[n] | ext(s10)")
			    (list "xor"
				  "XOR     i, j, k"
				  "2"
				  "FX"
				  "i = j ^ k")
			    (list "eqv"
				  "EQV     i, j, k"
				  "2"
				  "FX"
				  "i = ~(j ^ k)\nEQV is 'NXOR'\nAlso, the missing 'XORC' would give the same result as EQV")
			    (list "xorbi"
				  "XORBI   i, j, u8"
				  "2"
				  "FX"
				  "i.b[n] = j.b[n] ^ u8")
			    (list "xorhi"
				  "XORHI   i, j, s10"
				  "2"
				  "FX"
				  "i.h[n] = j.h[n] ^ ext(s10)")
			    (list "xori"
				  "XORI    i, j, s10"
				  "2"
				  "FX"
				  "i.w[n] = j.w[n] ^ ext(s10)")
			    (list "il"
				  "IL	i, s16"
				  "2"
				  "FX"
				  "i.w[n] = ext(s16)")
			    (list "ilh"
				  "ILH	i, u16"
				  "2"
				  "FX"
				  "i.h[n] = u16")
			    (list "ila"
				  "ILA	i, u18"
				  "2"
				  "FX"
				  "i.w[n] = u18")
			    (list "ilhu"
				  "ILHU	i, u16"
				  "2"
				  "FX"
				  "i.w[n] = u16 << 16")
			    (list "iohl"
				  "IOHL	i, u16"
				  "2"
				  "FX"
				  "i.w[n] |= u16")
			    (list "ah"
				  "AH	i, j, k"
				  "2"
				  "FX"
				  "i.h[n] = j.h[n] + k.h[n]")
			    (list "ahi"
				  "AHI     i, j, s10"
				  "2"
				  "FX"
				  "i.h[n] = j.h[n] + ext(s10)")
			    (list "a"
				  "A       i, j, k"
				  "2"
				  "FX"
				  "i.w[n] = j.w[n] + k.w[n]")
			    (list "ai"
				  "AI      i, j, s10"
				  "2"
				  "FX"
				  "i.w[n] = j.w[n] + ext(s10)")
			    (list "sfh"
				  "SFH     i, j, k"
				  "2"
				  "FX"
				  "i.h[n] = -j.h[n] + k.h[n]")
			    (list "sfhi"
				  "SFHI    i, j, s10"
				  "2"
				  "FX"
				  "i.h[n] = -j.h[n] + ext(s10)")
			    (list "sf"
				  "SF      i, j, k"
				  "2"
				  "FX"
				  "i.w[n] = -j.w[n] + k.w[n]")
			    (list "sfi"
				  "SFI     i, j, s10"
				  "2"
				  "FX"
				  "i.w[n] = -j.w[n] + ext(s10)")
			    (list "bg"
				  "BG	i, j, k"
				  "2"
				  "FX"
				  "i.w[n] = ( -j.w[n] + k.w[n] ) < 0 ? 0 : 1\ngenerate borrow bit")
			    (list "bgx"
				  "BGX	i, j, k"
				  "2"
				  "FX"
				  "i.w[n] = ( -j.w[n] + k.w[n] + ( i.w[n] & 1 ) - 1 ) < 0 ? 0 : 1\ngenerate borrow bit with borrow")
			    (list "sfx"
				  "SFX	i, j, k"
				  "2"
				  "FX"
				  "i.w[n] = ( -j.w[n] + k.w[n] + ( i.w[n] & 1 ) - 1 )\nsubtract with borrow bit")
			    (list "cg"
				  "CG	i, j, k"
				  "2"
				  "FX"
				  "i.w[n] = ( j.w[n] + k.w[n] ) > 0xFFFFFFFF ? 1 : 0\ngenerate carry bit")
			    (list "cgx"
				  "CGX	i, j, k"
				  "2"
				  "FX"
				  "i.w[n] = ( j.w[n] + k.w[n] + ( i.w[n] & 1 ) ) > 0xFFFFFFFF ? 1 : 0\ngenerate carry bit with carry")
			    (list "addx"
				  "ADDX	i, j, k"
				  "2"
				  "FX"
				  "i.w[n] = ( j.w[n] + k.w[n] + ( i.w[n] & 1 ) )\nadd with carry bit")
			    (list "xsbh"
				  "XSBH	i, j"
				  "2"
				  "FX"
				  "i.h[n] = ext( i.h[n] & 0xFF )")
			    (list "xshw"
				  "XSHW    i, j"
				  "2"
				  "FX"
				  "i.w[n] = ext( i.w[n] & 0xFFFF )")
			    (list "xswd"
				  "XSWD    i, j"
				  "2"
				  "FX"
				  "i.d[n] = ext( i.d[n] & 0xFFFFFFFF )")
			    (list "clz"
				  "CLZ	i, j"
				  "2"
				  "FX"
				  "i.w[n] = leadingZeroCount(j.w[n])")
			    (list "selb"
				  "SELB	i, j, k, l"
				  "2"
				  "FX"
				  "i = ( l ? k : j )")
			    (list "shlqby"
				  "SHLQBY   i, j, k"
				  "4"
				  "SH"
				  "i = j << ( ( k.b[3] & 0x1F ) << 3 )")
			    (list "shlqbyi"
				  "SHLQBYI  i, j, imm"
				  "4"
				  "SH"
				  "i = j << ( ( imm & 0x1F ) << 3 )")
			    (list "shlqbybi"
				  "SHLQBYBI i, j, k"
				  "4"
				  "SH"
				  "i = j << ( k.b[3] & 0xF8 )")
			    (list "shlqbi"
				  "SHLQBI   i, j, k"
				  "4"
				  "SH"
				  "i = j << ( k.b[3] & 0x07 )")
			    (list "shlqbii"
				  "SHLQBII  i, j, imm"
				  "4"
				  "SH"
				  "i = j << ( imm & 0x07 )")
			    (list "rotqby"
				  "ROTQBY   i, j, k"
				  "4"
				  "SH"
				  "i = j <^ ( ( k.b[3] & 0x0F ) << 3 )")
			    (list "rotqbyi"
				  "ROTQBYI  i, j, imm"
				  "4"
				  "SH"
				  "i = j <^ ( ( imm & 0x0F ) << 3 )")
			    (list "rotqbybi"
				  "ROTQBYBI i, j, k"
				  "4"
				  "SH"
				  "i = j <^ ( k.b[3] & 0x78 )")
			    (list "rotqbi"
				  "ROTQBI   i, j, k"
				  "4"
				  "SH"
				  "i = j <^ ( k.b[3] & 0x07 )")
			    (list "rotqbii"
				  "ROTQBII  i, j, imm"
				  "4"
				  "SH"
				  "i = j <^ ( imm & 0x07 )")
			    (list "rotqmby"
				  "ROTQMBY   i, j, k"
				  "4"
				  "SH"
				  "i = j >> ( ( -k.b[3] & 0x1F ) << 3 )")
			    (list "rotqmbyi"
				  "ROTQMBYI  i, j, imm"
				  "4"
				  "SH"
				  "i = j >> ( ( -imm & 0x1F ) << 3 )")
			    (list "rotqmbybi"
				  "ROTQMBYBI i, j, k"
				  "4"
				  "SH"
				  "i = j >> ( -( k.b[3] & 0xF8 ) & 0xF8 )\n\n  #bits to	bits in b	b.low = -shr\n  shift right	high  low	b.hi  = -(shr & 0xF8)\n  0		00000 000\n  1		00000 111\n  2		00000 110\n  3		00000 101\n  4		00000 100\n  5		00000 011\n  6		00000 010\n  7		00000 001\n  8		11111 000\n  9		11111 111\n  . . .\n  16		11110 000\n")
			    (list "rotqmbi"
				  "ROTQMBI   i, j, k"
				  "4"
				  "SH"
				  "i = j >> ( -k.b[3] & 0x07 )")
			    (list "rotqmbii"
				  "ROTQMBII  i, j, imm"
				  "4"
				  "SH"
				  "i = j >> ( -imm & 0x07 )")
			    (list "orx"
				  "ORX	i, j"
				  "4"
				  "SH"
				  "i.w[0] = ( j.w[0] | j.w[1] | j.w[2] | j.w[3] )\ni.w[1] = i.w[2] = i.w[3] = 0")
			    (list "shufb"
				  "SHUFB	i, j, k, l"
				  "4"
				  "SH"
				  "x = l.b[n]\nx in 0x00..0x7F: i.b[n] = x & 0x10 ? k.b[x & 0x0F] : j.b[x & 0x0F]\nx in 0x80..0xBF: i.b[n] = 0x00\nx in 0xC0..0xDF: i.b[n] = 0xFF\nx in 0xE0..0xFF: i.b[n] = 0x80")
			    (list "cbd"
				  "CBD	i, imm(j)"
				  "4"
				  "SH"
				  "i = shuffleb(j.b[3]+ imm   & 0x0F)\nCBD 0 = s_Dbcdefghijklmnop, not s_Abcd...")
			    (list "cbx"
				  "CBX	i, j, k"
				  "4"
				  "SH"
				  "i = shuffleb(j.b[3]+k.b[3] & 0x0F)")
			    (list "chd"
				  "CHD   i, imm(j)"
				  "4"
				  "SH"
				  "i = shuffleh(j.b[3]+ imm   & 0x0E)\nCHD 0 = s_Bbcdefgh, not s_Abcd...")
			    (list "chx"
				  "CHX   i, j, k"
				  "4"
				  "SH"
				  "i = shuffleh(j.b[3]+k.b[3] & 0x0E)")
			    (list "cwd"
				  "CWD   i, imm(j)"
				  "4"
				  "SH"
				  "i = shufflew(j.b[3]+ imm   & 0x0C)\nCWD 0 = s_Abcd")
			    (list "cwx"
				  "CWX   i, j, k"
				  "4"
				  "SH"
				  "i = shufflew(j.b[3]+k.b[3] & 0x0C)")
			    (list "cdd"
				  "CDD   i, imm(j)"
				  "4"
				  "SH"
				  "i = shuffled(j.b[3]+ imm   & 0x08)\nCDD 0 = s_Ab")
			    (list "cdx"
				  "CDX   i, j, k"
				  "4"
				  "SH"
				  "i = shuffled(j.b[3]+k.b[3] & 0x08)")
			    (list "fsmbi"
				  "FSMBI	i, u16"
				  "4"
				  "SH"
				  "i.b[n] = ( ( u16 << n ) & 0x8000 ) ? 0xFF : 0x00\nexpand 16 bits to 16 bytes")
			    (list "fsmb"
				  "FSMB	i, j"
				  "4"
				  "SH"
				  "i.b[n] = ( ( i.h[1] << n ) & 0x8000 ) ? 0xFF : 0x00")
			    (list "fsmh"
				  "FSMH	i, j"
				  "4"
				  "SH"
				  "i.b[n] = ( ( i.b[3] << n ) & 0x80 ) ? 0xFFFF : 0x0000\nexpand 8 bits to 8 half words")
			    (list "fsm"
				  "FSM	i, j"
				  "4"
				  "SH"
				  "i.b[n] = ( ( i.b[3] << n ) & 0x8 ) ? 0xFFFFFFFF : 0x00000000\nexpand 4 bits to 4 words")
			    (list "gbb"
				  "GBB	i, j"
				  "4"
				  "SH"
				  "i = 0; for( n = 0; n < 16; n++ ) i.w[0] = (i.w[0]<<1) | ( j.b[n] & 1 )\ngather lowest bit of 16 bytes into preferred word")
			    (list "gbh"
				  "GBH	i, j"
				  "4"
				  "SH"
				  "i = 0; for( n = 0; n < 8; n++ ) i.w[0] = (i.w[0]<<1) | ( j.h[n] & 1 )\ngather lowest bit of 8 halfwords into preferred word")
			    (list "gb"
				  "GB    i, j"
				  "4"
				  "SH"
				  "i = 0; for( n = 0; n < 4; n++ ) i.w[0] = (i.w[0]<<1) | ( j.w[n] & 1 )\ngather lowest bit of 4 words into preferred word")
			    (list "frest"
				  "FREST	   a, b"
				  "4"
				  "SH"
				  "Form Reciprocal ESTimate for 1/b.f[n]\nmust complete with FI instruction\n\n  1/x\n		FREST a, x\n	FI	b, x, a		b is good to 12 bits precision\n	FNMS	c, b, x, One\n	FMA	b, c, b, b      now b is good to 24 bits precision")
			    (list "frsqest"
				  "FRSQEST  a, b"
				  "4"
				  "SH"
				  "Form Reciprocal SQuare root ESTimate for 1/sqrt(b.f[n])\nmust complete with FI instruction\n\n  1/sqrt(x)\n		FRSQEST a, x\n	FI	b, x, a		b is good to 12 bits precision\n	FM	c, b, x		(b and a can share register)\n	FM	d, b, OneHalf	(c and x can share register)\n	FNMS	c, c, b, One\n	FMA	b, d, c, b      now b is good to 24 bits precision")
			    (list "sync"
				  "SYNC"
				  "?"
				  "??"
				  "wait for data stores to finish")
			    (list "syncc"
				  "SYNCC	chn?"
				  "?"
				  "??"
				  "synchronize channel, then do SYNC\n(SPU doc does not specify chn)")
			    (list "dsync"
				  "DSYNC"
				  "?"
				  "??"
				  "wait for data loads to finish")
			    (list "fscrrd"
				  "FSCRRD  a"
				  "?"
				  "??"
				  "read floating point status flags")
			    (list "fscrwr"
				  "FSCRWR	a"
				  "?"
				  "??"
				  "write ...")
			    (list "mfspr"
				  "MFSPR	a, spr"
				  "?"
				  "??"
				  "move from special purpose register")
			    (list "mtspr"
				  "MTSPR	spr, a"
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
				  "RCHCNT	a, chn"
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
				  "STOPD	a, b, c"
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
	

(defun th-spu-mode-handler ()
  (or (spu-tooltip-string (current-word)) ""))



;;(defun temp-extract-doc()
;;  (interactive)
;;  (if (is-region-active)
;;      (let ((start (region-beginning))
;;	    (end   (region-end))
;;	    opcode
;;	    fctn
;;	    latency
;;	    category
;;	    comment
;;	    beg
;;	    instr-list
;;	    item)
;;	(if ( > start end )
;;	    (let (tmp) (setq tmp end end start start tmp)))
;;	(deactivate-mark)
;;	(save-excursion
;;	  (goto-char start)
;;	  (beginning-of-line)
;;	  (while (and (bolp)
;;		      (not (bobp))
;;		      (< (point) end))
;;	    (setq opcode (current-word))
;;	    (setq beg (point))
;;	    (move-to-column 24)
;;	    (setq fctn (clear-spaces (buffer-substring-no-properties beg (point))))
;;	    (setq beg (point))
;;	    (move-to-column 27) 
;;	    (setq latency (clear-spaces (buffer-substring-no-properties beg (point))))
;;	    (setq beg (point))
;;	    (move-to-column 32)
;;	    (setq category (clear-spaces (buffer-substring-no-properties beg (point))))
;;	    (setq comment (clear-spaces (buffer-substring-no-properties (point) (point-at-eol))))
;;	    (setq instr-list (cons (list (downcase opcode) fctn latency category comment) instr-list))
;;	    (forward-line 1)
;;	    (beginning-of-line)))
;;	(goto-char 1437)
;;	(setq instr-list (reverse instr-list))
;;	(insert "(setq spu-opcode-help (list ")
;;	(setq item (pop instr-list))
;;	(while item
;;	  (insert (concat "(list \"" (car item) "\""))
;;	  (newline)
;;	  (lisp-indent-line)
;;	  (insert (concat "\""       (cadr item) "\""))
;;	  (newline)
;;	  (lisp-indent-line)
;;	  (insert (concat "\""       (caddr item) "\""))
;;	  (newline)
;;	  (lisp-indent-line)
;;	  (insert (concat "\""    (cadddr item) "\""))
;;	  (newline)
;;	  (lisp-indent-line)
;;	  (insert (concat "\""    (cadddr (cdr item)) "\")"))
;;	  (newline)
;;	  (lisp-indent-line)
;;	  (setq item (pop instr-list)))
;;	(insert "))"))))

(provide 'spu-tooltip)