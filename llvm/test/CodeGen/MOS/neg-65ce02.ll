; RUN: llc -mtriple=mos -mcpu=mos65ce02 -verify-machineinstrs < %s | FileCheck %s
; RUN: llc -mtriple=mos -mcpu=mos45gs02 -verify-machineinstrs < %s | FileCheck %s
; RUN: llc -mtriple=mos -mcpu=mos6502 -verify-machineinstrs < %s | FileCheck %s --check-prefix=NO-NEG

target datalayout = "e-m:e-p:16:8-p1:8:8-i16:8-i32:8-i64:8-f32:8-f64:8-a:8-Fi8-n8"

define i8 @neg_i8(i8 %a) {
; CHECK-LABEL: neg_i8:
; CHECK:       ; %bb.0:
; CHECK-NEXT:    neg
; CHECK-NEXT:    rts
;
; NO-NEG-LABEL: neg_i8:
; NO-NEG:       ; %bb.0:
; NO-NEG-NOT:   neg
; NO-NEG:       rts
  %neg = sub i8 0, %a
  ret i8 %neg
}

define i8 @sub_nonzero_lhs(i8 %a) {
; CHECK-LABEL: sub_nonzero_lhs:
; CHECK:       ; %bb.0:
; CHECK-NOT:   neg
; CHECK:       rts
;
; NO-NEG-LABEL: sub_nonzero_lhs:
; NO-NEG:       ; %bb.0:
; NO-NEG-NOT:   neg
; NO-NEG:       rts
  %sub = sub i8 5, %a
  ret i8 %sub
}
