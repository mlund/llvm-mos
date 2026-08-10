//===-- MOSMarkImag32.cpp - Mark 32-bit imaginary register use ------------===//
//
// Part of LLVM-MOS, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MOSMarkImag32.h"

#include "MOS.h"
#include "MOSMachineFunctionInfo.h"
#include "MOSRegisterInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/InitializePasses.h"

#define DEBUG_TYPE "mos-mark-imag32"

using namespace llvm;

namespace {

class MOSMarkImag32 : public MachineFunctionPass {
public:
  static char ID;

  MOSMarkImag32() : MachineFunctionPass(ID) {
    initializeMOSMarkImag32Pass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;
};

bool MOSMarkImag32::runOnMachineFunction(MachineFunction &MF) {
  auto &FuncInfo = *MF.getInfo<MOSFunctionInfo>();
  for (const MachineBasicBlock &MBB : MF) {
    for (const MachineInstr &MI : MBB) {
      for (const MachineOperand &MO : MI.operands()) {
        if (MO.isReg() && MO.getReg().isPhysical() &&
            MOS::Imag32AllRegClass.contains(MO.getReg())) {
          // Later pseudo expansion erases the quad assignment.
          FuncInfo.UsesImag32 = true;
          return false;
        }
      }
    }
  }
  return false;
}

} // namespace

char MOSMarkImag32::ID = 0;

INITIALIZE_PASS(MOSMarkImag32, DEBUG_TYPE,
                "Mark 32-bit imaginary register use", false, false)

MachineFunctionPass *llvm::createMOSMarkImag32Pass() {
  return new MOSMarkImag32();
}
