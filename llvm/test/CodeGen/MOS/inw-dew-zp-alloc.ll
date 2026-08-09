; RUN: llc -O2 -mcpu=mos65ce02 -zp-avail=224 -verify-machineinstrs < %s | FileCheck %s

target datalayout = "e-m:e-p:16:8-p1:8:8-i16:8-i32:8-i64:8-f32:8-f64:8-a:8-Fi8-n8"
target triple = "mos"

@word = global i16 undef, align 1
@source = global i16 0, align 1

define void @seed_word() {
entry:
  %v = load i16, ptr @source
  store i16 %v, ptr @word
  ret void
}

define void @inc_word() {
; CHECK-LABEL: inc_word:
; CHECK:       inw mos8(word)
entry:
  %v = load i16, ptr @word
  %inc = add i16 %v, 1
  store i16 %inc, ptr @word
  ret void
}
