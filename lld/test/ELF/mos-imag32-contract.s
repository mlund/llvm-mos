# REQUIRES: mos
# RUN: llvm-mc -filetype=obj -triple=mos %s -o %t.o
# RUN: not ld.lld --gc-sections %t.o -o /dev/null 2>&1 | FileCheck %s
# RUN: ld.lld --gc-sections --defsym=__mos_imag32_contiguous=0 %t.o -o /dev/null

# CHECK: error: undefined symbol: __mos_imag32_contiguous

.section .mos.imag32,"aR",@progbits
  .byte __mos_imag32_contiguous
