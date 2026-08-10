//===-- MOSMarkImag32.h - Mark 32-bit imaginary register use ----*- C++ -*-===//
//
// Part of LLVM-MOS, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MOS_MOSMARKIMAG32_H
#define LLVM_LIB_TARGET_MOS_MOSMARKIMAG32_H

#include "llvm/CodeGen/MachineFunctionPass.h"

namespace llvm {

MachineFunctionPass *createMOSMarkImag32Pass();

} // namespace llvm

#endif // LLVM_LIB_TARGET_MOS_MOSMARKIMAG32_H
