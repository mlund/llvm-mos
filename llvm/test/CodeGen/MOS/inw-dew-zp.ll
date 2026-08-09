; RUN: llc -mcpu=mos65ce02 -verify-machineinstrs < %s | FileCheck %s --check-prefix=CE02
; RUN: llc -mcpu=mos6502 -verify-machineinstrs < %s | FileCheck %s --check-prefix=6502

target datalayout = "e-m:e-p:16:8-p1:8:8-i16:8-i32:8-i64:8-f32:8-f64:8-a:8-Fi8-n8"
target triple = "mos"

@zp = addrspace(1) global i16 0
@zp32 = addrspace(1) global i32 0
@abs = global i16 0

define void @inc_zp() {
; CE02-LABEL: inc_zp:
; CE02:         inw mos8(zp)
; CE02-NEXT:    rts
; 6502-LABEL: inc_zp:
; 6502:         inc mos8(zp)
; 6502-NEXT:    bne .LBB0_2
; 6502:         inc mos8(zp+1)
; 6502-NEXT:  .LBB0_2:
; 6502-NEXT:    rts
entry:
  %v = load i16, ptr addrspace(1) @zp
  %inc = add i16 %v, 1
  store i16 %inc, ptr addrspace(1) @zp
  ret void
}

define void @dec_zp() {
; CE02-LABEL: dec_zp:
; CE02:         dew mos8(zp)
; CE02-NEXT:    rts
; 6502-LABEL: dec_zp:
; 6502:         ldx #255
; 6502-NEXT:    dec mos8(zp)
; 6502-NEXT:    cpx mos8(zp)
; 6502-NEXT:    bne .LBB1_2
; 6502:         dec mos8(zp+1)
; 6502-NEXT:  .LBB1_2:
; 6502-NEXT:    rts
entry:
  %v = load i16, ptr addrspace(1) @zp
  %dec = add i16 %v, -1
  store i16 %dec, ptr addrspace(1) @zp
  ret void
}

define void @inc_abs() {
; CE02-LABEL: inc_abs:
; CE02-NOT:     inw
; CE02:         inc abs
; CE02:         inc abs+1
entry:
  %v = load i16, ptr @abs
  %inc = add i16 %v, 1
  store i16 %inc, ptr @abs
  ret void
}

define void @inc_zp_volatile() {
; CE02-LABEL: inc_zp_volatile:
; CE02-NOT:     inw
; CE02:         ldx mos8(zp)
; CE02:         stx mos8(zp)
entry:
  %v = load volatile i16, ptr addrspace(1) @zp
  %inc = add i16 %v, 1
  store volatile i16 %inc, ptr addrspace(1) @zp
  ret void
}

define void @dec_zp32() {
; CE02-LABEL: dec_zp32:
; CE02:         dec mos8(zp32)
; CE02:         dec mos8(zp32+1)
; CE02:         dew mos8(zp32+2)
entry:
  %v = load i32, ptr addrspace(1) @zp32
  %dec = add i32 %v, -1
  store i32 %dec, ptr addrspace(1) @zp32
  ret void
}

define void @inc_zp32() {
; CE02-LABEL: inc_zp32:
; CE02:         inw mos8(zp32)
; CE02:         inw mos8(zp32+2)
entry:
  %v = load i32, ptr addrspace(1) @zp32
  %inc = add i32 %v, 1
  store i32 %inc, ptr addrspace(1) @zp32
  ret void
}
