; RUN: llc -mtriple=mos -filetype=obj < %s -o %t.o
; RUN: llvm-readelf -r -s %t.o | FileCheck %s --check-prefix=OBJECT

; OBJECT: R_MOS_ADDR8
; OBJECT-SAME: __mos_imag32_contiguous
; OBJECT: GLOBAL DEFAULT UND __mos_imag32_contiguous

define void @require_imag32() {
  %unused = call i32 asm sideeffect "; $0", "=r"()
  ret void
}
