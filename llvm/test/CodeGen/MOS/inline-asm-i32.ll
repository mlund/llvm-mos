; RUN: llc -mtriple=mos -mcpu=mos6502 -verify-machineinstrs < %s | FileCheck %s
; RUN: llc -mtriple=mos -mcpu=mos45gs02 -verify-machineinstrs < %s | FileCheck %s

define void @input(i32 %value) {
; CHECK-LABEL: input:
; CHECK:       ;APP
; CHECK-NEXT:  ; value = [[REG:__rc(4|12|20|24)]]
; CHECK-NEXT:  ;NO_APP
  call void asm sideeffect "; value = $0", "r"(i32 %value)
  ret void
}

define i32 @output() {
; CHECK-LABEL: output:
; CHECK:       ;APP
; CHECK-NEXT:  ; result = [[REG:__rc(4|12|20|24)]]
; CHECK-NEXT:  ;NO_APP
  %result = call i32 asm sideeffect "; result = $0", "=r"()
  ret i32 %result
}

define i32 @tied(i32 %value) {
; CHECK-LABEL: tied:
; CHECK:       ;APP
; CHECK-NEXT:  ; value = [[REG:__rc(4|12|20|24)]]
; CHECK-NEXT:  ;NO_APP
  %result = call i32 asm sideeffect "; value = $0", "=r,0"(i32 %value)
  ret i32 %result
}

define void @constant() {
; CHECK-LABEL: constant:
; CHECK:       ;APP
; CHECK-NEXT:  ; constant = [[REG:__rc(4|12|20|24)]]
; CHECK-NEXT:  ;NO_APP
  call void asm sideeffect "; constant = $0", "r"(i32 305419896)
  ret void
}

define void @mixed(i8 %byte, i16 %word, i32 %long) {
; CHECK-LABEL: mixed:
; CHECK:       ;APP
; CHECK-NEXT:  ; mixed = __rc{{[0-9]+}}, __rc{{[0-9]+}}, __rc{{(4|12|20|24)}}
; CHECK-NEXT:  ;NO_APP
  call void asm sideeffect "; mixed = $0, $1, $2",
                           "r,r,r"(i8 %byte, i16 %word, i32 %long)
  ret void
}

define void @four(i32 %a, i32 %b, i32 %c, i32 %d) {
; CHECK-LABEL: four:
; CHECK:       ;APP
; CHECK-NEXT:  ; four = __rc{{(4|12|20|24)}}, __rc{{(4|12|20|24)}}, __rc{{(4|12|20|24)}}, __rc{{(4|12|20|24)}}
; CHECK-NEXT:  ;NO_APP
  call void asm sideeffect "; four = $0, $1, $2, $3",
                           "r,r,r,r"(i32 %a, i32 %b, i32 %c, i32 %d)
  ret void
}

define void @separate_outputs() {
; CHECK-LABEL: separate_outputs:
; CHECK:       ; define __rc
; CHECK:       ; use __rc
  %a = call i32 asm sideeffect "; define $0", "=r"()
  %b = call i32 asm sideeffect "; define $0", "=r"()
  call void asm sideeffect "; use $0", "r"(i32 %a)
  call void asm sideeffect "; use $0", "r"(i32 %b)
  ret void
}
