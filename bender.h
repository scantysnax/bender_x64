//   _                    _           
//  | |__   ___ _ __   __| | ___ _ __ 
//  | '_ \ / _ \ '_ \ / _` |/ _ \ '__|
//  | |_) |  __/ | | | (_| |  __/ |   
//  |_.__/ \___|_| |_|\__,_|\___|_|
//
// bender - a 6502/2A03 emulator written in x64 assembly
//			with plenty of help from others
//			eli dayan 2013-2014

#ifndef _BENDER_H_
#define _BENDER_H_

#include <stdint.h>

#define MAX_PAGES 	64

#ifdef __cplusplus
	extern "C" {
#endif

extern uint64_t rPC;
extern uint8_t rA;
extern uint8_t rX;
extern uint8_t rY;
extern uint8_t rP;
extern uint8_t rS;

extern uint8_t *bender_memory[MAX_PAGES];
extern uint8_t bender_needs_interrupt;
extern uint8_t bender_is_jammed;
extern int64_t bender_remaining_cycles;
extern int64_t bender_total_cycles;
extern int64_t bender_cycles_to_eat;

extern uint8_t (*bender_read_hook)(uint64_t address);
extern void (*bender_write_hook)(uint64_t address, uint8_t data);
extern void (*bender_jam_hook)(void);

extern void bender_abort (void);
extern void bender_clearirq (void);
extern uint8_t bender_dma (uint64_t address);
extern void bender_eatcycles (int64_t cycles);
extern uint64_t bender_elapsedcycles (uint8_t reset);
extern void bender_init (void *read_hook, void *write_hook, void *jam_hook);
extern void bender_irq (void);
extern void bender_nmi (void);
extern void bender_reset (void);
extern int64_t bender_run (uint64_t cycles);
extern void bender_step (void);

#ifdef __cplusplus
}
#endif

#endif // _BENDER_H_

