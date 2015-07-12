//   _                    _           
//  | |__   ___ _ __   __| | ___ _ __ 
//  | '_ \ / _ \ '_ \ / _` |/ _ \ '__|
//  | |_) |  __/ | | | (_| |  __/ |   
//  |_.__/ \___|_| |_|\__,_|\___|_|
//
// bender -  a 6502/2A03 emulator written in x86 assembly
//			with plenty of help from others
//			eli dayan 2012-2013

#ifndef _BENDER_DEBUG_H_
#define _BENDER_DEBUG_H_

#include <stdint.h>

void debug_init (void);
void debug_cleanup (void);

void debug_disassemble (void);
void debug_dump_registers (void);
void debug_print_inst_count(void);

#endif // _BENDER_DEBUG_H_
