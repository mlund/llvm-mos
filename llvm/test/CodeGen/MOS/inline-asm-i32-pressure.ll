; RUN: not llc -mtriple=mos -O2 -o /dev/null < %s 2>&1 | FileCheck %s

; CHECK: ran out of registers during register allocation in function 'five'

define void @five(i32 %a, i32 %b, i32 %c, i32 %d, i32 %e) {
  call void asm sideeffect "; $0 $1 $2 $3 $4", "r,r,r,r,r"(
      i32 %a, i32 %b, i32 %c, i32 %d, i32 %e)
  ret void
}
