; RUN: llc -mtriple=mos -mcpu=mos45gs02 -verify-machineinstrs < %s | FileCheck %s
; RUN: llc -mtriple=mos -mcpu=mos6502 -verify-machineinstrs < %s | FileCheck %s

; Verify that i32 values can be passed via the 'r' inline asm constraint,
; placing them in contiguous zero-page Imag32 registers.

define void @use_i32_reg(i32 %addr) {
; CHECK-LABEL: use_i32_reg:
; CHECK: ;APP
; CHECK-NEXT: ;; addr at [[REG:__rc[0-9]+]]
; CHECK-NEXT: ;NO_APP
  call void asm sideeffect ";; addr at $0", "r"(i32 %addr)
  ret void
}

define i32 @roundtrip_i32(i32 %x) {
; CHECK-LABEL: roundtrip_i32:
; CHECK: ;APP
; CHECK-NEXT: ;; use [[REG:__rc[0-9]+]]
; CHECK-NEXT: ;NO_APP
  %result = call i32 asm sideeffect ";; use $0", "=r,0"(i32 %x)
  ret i32 %result
}

; Output-only: exercises the unmerge (s32 → 4×s8) path.
define i32 @output_only_i32() {
; CHECK-LABEL: output_only_i32:
; CHECK: ;APP
; CHECK-NEXT: ;; produce [[REG:__rc[0-9]+]]
; CHECK-NEXT: ;NO_APP
  %result = call i32 asm sideeffect ";; produce $0", "=r"()
  ret i32 %result
}

; Multiple i32 operands: two Imag32 allocations coexist.
define void @two_i32_regs(i32 %a, i32 %b) {
; CHECK-LABEL: two_i32_regs:
; CHECK: ;APP
; CHECK-NEXT: ;; a=[[REGA:__rc[0-9]+]] b=[[REGB:__rc[0-9]+]]
; CHECK-NEXT: ;NO_APP
  call void asm sideeffect ";; a=$0 b=$1", "r,r"(i32 %a, i32 %b)
  ret void
}

; Mixed sizes: i8, i16, and i32 inline asm operands together.
define void @mixed_sizes(i8 %a, i16 %b, i32 %c) {
; CHECK-LABEL: mixed_sizes:
; CHECK: ;APP
; CHECK-NEXT: ;; i8=[[R8:__rc[0-9]+]] i16=[[R16:__rc[0-9]+]] i32=[[R32:__rc[0-9]+]]
; CHECK-NEXT: ;NO_APP
  call void asm sideeffect ";; i8=$0 i16=$1 i32=$2", "r,r,r"(i8 %a, i16 %b, i32 %c)
  ret void
}

; Constant i32: verifies merge of 4 immediate bytes into Imag32.
define void @const_i32() {
; CHECK-LABEL: const_i32:
; CHECK: ;APP
; CHECK-NEXT: ;; val=[[REG:__rc[0-9]+]]
; CHECK-NEXT: ;NO_APP
  call void asm sideeffect ";; val=$0", "r"(i32 305419896) ; 0x12345678
  ret void
}
