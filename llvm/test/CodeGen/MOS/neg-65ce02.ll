; RUN: llc -mtriple=mos -mcpu=mos65ce02 -verify-machineinstrs < %s | FileCheck %s
; RUN: llc -mtriple=mos -mcpu=mos45gs02 -verify-machineinstrs < %s | FileCheck %s
; RUN: llc -mtriple=mos -mcpu=mos45gs02 -verify-machineinstrs < %s | FileCheck %s --check-prefix=45GS
; RUN: llc -mtriple=mos -mcpu=mos4510 -verify-machineinstrs < %s | FileCheck %s --check-prefix=4510
; RUN: llc -mtriple=mos -mcpu=mos45gs02 -filetype=obj -verify-machineinstrs < %s -o %t.o
; RUN: llvm-objdump -d --mcpu=mos45gs02 %t.o | FileCheck %s --check-prefix=45GS-OBJ

target datalayout = "e-m:e-p:16:8-p1:8:8-i16:8-i32:8-i64:8-f32:8-f64:8-a:8-Fi8-n8"

define i8 @neg_i8(i8 %a) {
; CHECK-LABEL: neg_i8:
; CHECK:       ; %bb.0:
; CHECK-NEXT:    neg
; CHECK-NEXT:    rts
;
  %neg = sub i8 0, %a
  ret i8 %neg
}

define i8 @sub_nonzero_lhs(i8 %a) {
; CHECK-LABEL: sub_nonzero_lhs:
; CHECK:       ; %bb.0:
; CHECK-NOT:   {{^[[:space:]]*neg$}}
; CHECK:       rts
;
  %sub = sub i8 5, %a
  ret i8 %sub
}

define i16 @neg_i16(i16 %a) {
; CHECK-LABEL: neg_i16:
; CHECK-NOT:   {{^[[:space:]]*neg$}}
; CHECK:       rts
  %neg = sub i16 0, %a
  ret i16 %neg
}

define i8 @neg_twice_inline_asm(i8 %a) {
; 45GS-LABEL: neg_twice_inline_asm:
; 45GS:       neg
; 45GS:       nop
; 45GS-NEXT:  neg
; 45GS-NEXT:  lsr
; 45GS-NEXT:  rts
;
; 4510-LABEL: neg_twice_inline_asm:
; 4510:       neg
; 4510:       ;APP
; 4510-NEXT:  ;NO_APP
; 4510-NEXT:  neg
; 4510-NEXT:  lsr
; 4510-NEXT:  rts
;
; 45GS-OBJ-LABEL: <neg_twice_inline_asm>:
; 45GS-OBJ:       {{[[:space:]]+neg$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+nop$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+neg$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+lsr$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+rts$}}
  %first = sub i8 0, %a
  %tied = call i8 asm sideeffect "", "=a,0"(i8 %first)
  %second = sub i8 0, %tied
  %result = lshr i8 %second, 1
  ret i8 %result
}

define i8 @neg_after_inline_asm(i8 %a) {
; 45GS-LABEL: neg_after_inline_asm:
; 45GS:       neg
; 45GS:       nop
; 45GS-NEXT:  neg
; 45GS-NEXT:  lsr
; 45GS-NEXT:  rts
;
; 45GS-OBJ-LABEL: <neg_after_inline_asm>:
; 45GS-OBJ:       {{[[:space:]]+neg$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+nop$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+neg$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+lsr$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+rts$}}
  %tied = call i8 asm sideeffect "neg", "=a,0"(i8 %a)
  %second = sub i8 0, %tied
  %result = lshr i8 %second, 1
  ret i8 %result
}

define i8 @neg_before_inline_asm(i8 %a) {
; 45GS-LABEL: neg_before_inline_asm:
; 45GS:       neg
; 45GS:       ;APP
; 45GS-NEXT:  nop
; 45GS-NEXT:  neg
; 45GS-NEXT:  lsr
; 45GS-NEXT:  ;NO_APP
; 45GS-NEXT:  rts
;
; 45GS-OBJ-LABEL: <neg_before_inline_asm>:
; 45GS-OBJ:       {{[[:space:]]+neg$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+nop$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+neg$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+lsr$}}
; 45GS-OBJ-NEXT:  {{[[:space:]]+rts$}}
  %first = sub i8 0, %a
  %tied = call i8 asm sideeffect "neg\0A\09lsr", "=a,0"(i8 %first)
  ret i8 %tied
}
