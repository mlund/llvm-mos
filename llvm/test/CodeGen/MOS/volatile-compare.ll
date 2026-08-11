; RUN: llc -mtriple=mos -verify-machineinstrs < %s | FileCheck %s

define zeroext i1 @cmp_abs() {
; CHECK-LABEL: cmp_abs:
; CHECK:       ldx 53266
; CHECK-NEXT:  cpx 53267
entry:
  %lhs = load volatile i8, ptr inttoptr (i16 53266 to ptr)
  %rhs = load volatile i8, ptr inttoptr (i16 53267 to ptr)
  %cmp = icmp eq i8 %lhs, %rhs
  ret i1 %cmp
}

define i8 @cmp_abs_multiple_users() {
; CHECK-LABEL: cmp_abs_multiple_users:
; CHECK:       ldx 53266
; CHECK-NEXT:  lda 53267
; CHECK-NEXT:  sta [[RHS:__rc[0-9]+]]
; CHECK-NEXT:  cpx [[RHS]]
entry:
  %lhs = load volatile i8, ptr inttoptr (i16 53266 to ptr)
  %rhs = load volatile i8, ptr inttoptr (i16 53267 to ptr)
  %cmp = icmp eq i8 %lhs, %rhs
  %ext = zext i1 %cmp to i8
  %result = add i8 %rhs, %ext
  ret i8 %result
}

define zeroext i1 @cmp_indir(ptr %lhs.ptr, ptr %rhs.ptr) {
; CHECK-LABEL: cmp_indir:
; CHECK:       cmp ({{__rc[0-9]+}}),y
entry:
  %lhs = load volatile i8, ptr %lhs.ptr
  %rhs = load volatile i8, ptr %rhs.ptr
  %cmp = icmp eq i8 %lhs, %rhs
  ret i1 %cmp
}

define zeroext i1 @cmp_abs_ult() {
; CHECK-LABEL: cmp_abs_ult:
; CHECK:       ldx 53266
; CHECK-NEXT:  cpx 53267
entry:
  %lhs = load volatile i8, ptr inttoptr (i16 53266 to ptr)
  %rhs = load volatile i8, ptr inttoptr (i16 53267 to ptr)
  %cmp = icmp ult i8 %lhs, %rhs
  ret i1 %cmp
}

@g = global i8 0

define void @cmp_abs_ordered(i8 %x, i8 %y) {
; CHECK-LABEL: cmp_abs_ordered:
; CHECK:       ldy 53266
; CHECK:       stx g
; CHECK:       cmp
entry:
  %rhs = load volatile i8, ptr inttoptr (i16 53266 to ptr)
  %cmp = icmp eq i8 %x, %rhs
  store volatile i8 %y, ptr @g
  br i1 %cmp, label %equal, label %exit

equal:
  store volatile i8 2, ptr @g
  br label %exit

exit:
  ret void
}
