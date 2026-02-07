; RUN: llc -mcpu=mos45gs02 -verify-machineinstrs < %s | FileCheck %s

target datalayout = "e-m:e-p:16:8-p1:8:8-i16:8-i32:8-i64:8-f32:8-f64:8-a:8-Fi8-n8"
target triple = "mos"

define i8 @ashr_i8_1(i8 %a) {
; CHECK-LABEL: ashr_i8_1:
; CHECK:       ; %bb.0: ; %entry
; CHECK-NEXT:    asr
; CHECK-NEXT:    rts
entry:
  %0 = ashr i8 %a, 1
  ret i8 %0
}

define i8 @ashr_i8_2(i8 %a) {
; CHECK-LABEL: ashr_i8_2:
; CHECK:       ; %bb.0: ; %entry
; CHECK-NEXT:    asr
; CHECK-NEXT:    asr
; CHECK-NEXT:    rts
entry:
  %0 = ashr i8 %a, 2
  ret i8 %0
}

; Only the highest byte needs sign preservation; lower bytes propagate carry.
define i16 @ashr_i16_1(i16 %a) {
; CHECK-LABEL: ashr_i16_1:
; CHECK:       ; %bb.0: ; %entry
; CHECK-NEXT:    stx __rc2
; CHECK-NEXT:    asr __rc2
; CHECK-NEXT:    ror
; CHECK-NEXT:    ldx __rc2
; CHECK-NEXT:    rts
entry:
  %0 = ashr i16 %a, 1
  ret i16 %0
}

define i32 @ashr_i32_1(i32 %a) {
; CHECK-LABEL: ashr_i32_1:
; CHECK:       ; %bb.0: ; %entry
; CHECK-NEXT:    stx __rc4
; CHECK-NEXT:    asr __rc3
; CHECK-NEXT:    ror __rc2
; CHECK-NEXT:    ror __rc4
; CHECK-NEXT:    ror
; CHECK-NEXT:    ldx __rc4
; CHECK-NEXT:    rts
entry:
  %0 = ashr i32 %a, 1
  ret i32 %0
}

; Each shift-by-1 iteration must independently preserve the sign bit.
define i8 @ashr_i8_3(i8 %a) {
; CHECK-LABEL: ashr_i8_3:
; CHECK:       ; %bb.0: ; %entry
; CHECK-NEXT:    asr
; CHECK-NEXT:    asr
; CHECK-NEXT:    asr
; CHECK-NEXT:    rts
entry:
  %0 = ashr i8 %a, 3
  ret i8 %0
}

; ASR preserves the sign bit, so it must not be used for unsigned shifts.
define i8 @lshr_i8_1(i8 %a) {
; CHECK-LABEL: lshr_i8_1:
; CHECK:       ; %bb.0: ; %entry
; CHECK-NEXT:    lsr
; CHECK-NEXT:    rts
entry:
  %0 = lshr i8 %a, 1
  ret i8 %0
}

; Left shifts are unrelated to ASR and must remain unchanged.
define i16 @shl_i16_1(i16 %a) {
; CHECK-LABEL: shl_i16_1:
; CHECK:       ; %bb.0: ; %entry
; CHECK-NEXT:    stx __rc2
; CHECK-NEXT:    asl
; CHECK-NEXT:    rol __rc2
; CHECK-NEXT:    ldx __rc2
; CHECK-NEXT:    rts
entry:
  %0 = shl i16 %a, 1
  ret i16 %0
}
