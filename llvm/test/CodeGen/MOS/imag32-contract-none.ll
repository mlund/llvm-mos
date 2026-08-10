; RUN: llc -mtriple=mos -filetype=obj < %s -o %t.o
; RUN: llvm-readelf -r -s %t.o | FileCheck %s

; CHECK-NOT: __mos_imag32_contiguous

define void @no_imag32() {
  ret void
}
