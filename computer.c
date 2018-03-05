#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
    //int x = 0;
    //while(++x < 10){
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

        /* 
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

        /* 
	 * Perform computation needed to execute d, returning computed value 
	 * in val 
	 */
        val = Execute(&d, &rVals);

	    UpdatePC(&d,val);

        /* 
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem, 
	 * otherwise put -1 in *changedMem. 
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);

        /* 
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        PrintInfo (changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch. 
 */
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

/* Decode instr, returning decoded instruction. */
// Use bit manipulation and masking to retrieve information from instruction
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    if(mips.debugging)
        printf("Decode\n");

    // opcode: 31 - 26
    d->op = instr >> 26;

    if(mips.debugging)
        printf("opcode %d\n", d->op);
    
    // opcode: 0 = R, 2/3 = J, rest is I
    switch(d->op){
        // R-format always 0 opcode
        // opcode   rs      rt      rd      shamt   func
        // 31-26    25-21   20-16   15-11   10-6    6-0
        case 0:
            if(mips.debugging)
                printf("R Format\n");
            d->type = R;
            d->regs.r.funct = instr & 0b111111;
            if(mips.debugging)
                printf("decode func %d", d->regs.r.funct);
            d->regs.r.shamt = (instr & 0b11111000000) >> 6;
            // set the registers
            d->regs.r.rd = (instr & 0b1111100000000000) >> 11;
            d->regs.r.rt = (instr & 0b111110000000000000000) >> 16;
            d->regs.r.rs = (instr & 0b11111000000000000000000000) >> 21;
            // set the register values
            rVals->R_rs = mips.registers[d->regs.r.rs];
            rVals->R_rt = mips.registers[d->regs.r.rt];
            rVals->R_rd = mips.registers[d->regs.r.rd];
            
            break;

        // J-format
        // opcode   address
        // 31-26    26-0
        case 2:
            // j
        case 3:
            // jal
            if(mips.debugging)
                printf("J Format\n");
            d->type = J;
            d->regs.j.target = instr & 0b11111111111111111111111111;
            break;

        // I-format
        // opcode   rs      rt      imm
        // 31-26    25-21   20-16   15-0
        default:
            if(mips.debugging)
                printf("I Format\n");

            d->type = I;
            d->regs.i.addr_or_immed = (short)(instr & 0b1111111111111111);    // short for 16bit int
            // set the registers
            d->regs.i.rt = (instr & 0b111110000000000000000) >> 16;
            d->regs.i.rs = (instr & 0b11111000000000000000000000) >> 21;
            // set the register values
            rVals->R_rs = mips.registers[d->regs.i.rs];
            rVals->R_rt = mips.registers[d->regs.i.rt];

            break;
    }

}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction (DecodedInstr* d) {
    if(mips.debugging)
        printf("PrintInstruction\n");
    
    // opcode/Funct referenced from green sheet
    char* instr;
    
    // Find instruction
    switch(d->op){
        
        // R-type: look at func
        case 0:
            switch((funct)(d->regs.r.funct)){
                case add:    //0x20 = 32
                    instr = "add";  break;
                case addu:    //0x21 = 33
                    if(mips.debugging)
                        printf("addu func %d\n", d->regs.r.funct);
                    instr = "addu"; break;
                case and:    //0x24 = 36
                    instr = "and"; break;
                case jr:
                    if(mips.debugging)
                        printf("hello");
                    instr = "jr";
                    //printf("%s\t$%d\n", instr, d->regs.r.rs);
                    break;
                case or:
                    instr = "or"; break;
                case slt:    // 0x2a = 42
                    instr = "slt"; break;
                // case sltu:
                //     instr = "sltu"; break;
                case sll:
                    instr = "sll"; break;
                case srl:
                    instr = "srl"; break;
                case sub:
                    instr = "sub"; break;
                case subu:
                    instr = "subu"; break;
                default:
                    if(mips.debugging)
                        printf("Uknown Instruction R-type case: %d\n", d->regs.r.funct);
                    exit(1);
            } 
            break;
        // case addi:
        //    printf("Oops");
        //    instr = "addi"; break;
        case addiu:
            instr = "addiu"; break;
        case andi:
            instr = "andi"; break;
        case beq:
            instr = "beq"; break;
        case bne:
            instr = "bne"; break;
        case j:
            instr = "j"; break;
        case jal:
            instr = "jal"; break;
        case lui:
            instr = "lui"; break;
        case lw:
            instr = "lw"; break;
        case ori:
            instr = "ori"; break;
        case slti:
            instr = "slti"; break;
        // case sltiu:
        //    instr = "sltiu"; break;
        case sw:
            instr = "sw"; break;
        default:
            if(mips.debugging)
                printf("Uknown instruction %d\n", d->op);
            exit(1);
    }

    // union allows us to use d->regs.r.rd/s/t even for i format
    int rs = d->regs.r.rs;
    int rt = d->regs.r.rt;
    int rd = d->regs.r.rd;
    short imm = d->regs.i.addr_or_immed; // short for 16bit int

    switch(d->type){
        case R:
            if(mips.debugging)
                printf("prnt R Format\n");
            // Print format based on funct
            switch((funct)(d->regs.r.funct)){
                // jr   $rs
                case jr:
                    printf("%s\t$%d\n", instr, rs); break;
                // srl/sll  $rd, $rs, shmat
                case srl:
                case sll:
                    printf("%s\t$%d, $%d, %d\n", instr, rd, rt, d->regs.r.shamt);   break;
                // and/etc  $rd, $rs, $rt
                default:
                    printf("%s\t$%d, $%d, $%d\n", instr, rd, rs, rt); break;
            }
            break;
        case I:
            if(mips.debugging)
                printf("print I format\n");
            // Print format based on opcode
            switch((opcode)(d->op)){
                // Print an address
                case beq:
                case bne:
                    if(mips.debugging)
                        printf("branching\n");
                    // always relative to next instruction (mips.pc + 4)
                    // word align the imm by multiplying it by 4 (shift by 2 to left)
                    printf("%s\t$%d, $%d, 0x%8.8x\n", instr, rs, rt, mips.pc + 4 + (imm << 2)); break;
                // Print immediate in decimal
                case addiu:
                    if(mips.debugging)
                        printf("print imm not addre\n");
                    printf("%s\t$%d, $%d, %i\n", instr, rt, rs, imm);   break;
                // lw/sw $rt, imm($rs)
                case lw:
                case sw:
                    printf("%s\t$%d, %d($%d)\n", instr, rt, imm, rs); break;
                // Print immediate in hex
                // immediate is zero extended
                case andi:
                case ori:
                case lui:
                    switch(imm){
                        // Print 0x0 if imm is 0
                        case 0:
                            printf("%s\t$%d, $%d, 0x%x\n", instr, rt, rs, imm); break;
                        // Otherwise print in hex
                        default:
                            printf("%s\t$%d, $%d, 0x%4.4x\n", instr, rt, rs, (unsigned short)(imm)); break;
                    } break;
                default:
                    if(mips.debugging)
                        printf("Oops Uknown opcode to print\n");
                    exit(1);
            }
            break;
        case J:
            // to get the address, concatenate the first four bits of PC
            // to the left of our jump address shifted 2 to the left
            printf("%s\t0x%8.8x\n", instr, (mips.pc & 0xf0000000) | (d->regs.j.target << 2)); break;
        default:
            if(mips.debugging)
                printf("Oops unknown type\n");
            exit(1);
    }
}

/* Perform computation needed to execute d, returning computed value */
/* Reference green sheet */
int Execute ( DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    // Find instruction

    unsigned short imm = d->regs.i.addr_or_immed;

    switch((opcode)(d->op)){
        // R format: look at funct
        case 0:
            switch((funct)(d->regs.r.funct)){
                case jr:
                    return rVals->R_rs;
                case srl:
                    return rVals->R_rt >> d->regs.r.shamt;
                case sll:
                    return rVals->R_rt << d->regs.r.shamt;
                case addu:
                    if(mips.debugging)
                        printf("rs: %d\trt: %d\n", rVals->R_rs, rVals->R_rt);
                    return rVals->R_rs + rVals->R_rt;
                case subu:
                    return rVals->R_rs - rVals->R_rt;
                case and:
                    return rVals->R_rs & rVals->R_rt;
                case or:
                    return rVals->R_rs || rVals->R_rt;
                case slt:
                    return rVals->R_rs < rVals->R_rt;
                default:
                    if(mips.debugging)
                        printf("Oops Execute Unknown funct\n"); 
                    exit(1);
            } break;
        // I format
        case addiu:
            return rVals->R_rs + d->regs.i.addr_or_immed;
        case beq:
            return rVals->R_rs == rVals->R_rt;
        case bne:
            return rVals->R_rs != rVals->R_rt;
        case lw:
        case sw:
            return rVals->R_rs + d->regs.i.addr_or_immed;
        case andi:
            return rVals->R_rs & imm;
        case ori:
            return rVals->R_rs || imm;
        // load the immediate into the upper 16 bits by shifting it 16 left  
        case lui:
            return imm << 16;
        // J format
        case jal:
            return mips.pc + 4;
        // no need for calculations
        case j:
            return;
        default:
            if(mips.debugging)
                printf("Oops Execute Unknown opcode\n");
            exit(1);
    }
  return 0;
}

/* 
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {
    switch((opcode)(d->op)){
        // R format: look at funct
        case 0:
            switch((funct)(d->regs.r.funct)){
                case jr:
                    mips.pc = val; return;
                default:
                    mips.pc+=4; return;
            }
            break;
        // I format
        case beq:
        case bne:
            if(val){
                // always relative to next instruction (mips.pc + 4)
                // word align the imm by multiplying it by 4 (shift by 2 to left)
                mips.pc = mips.pc + 4 + ((short)(d->regs.i.addr_or_immed) << 2);  // short for 16bit
                return;
            }
            else{
                mips.pc += 4; return;
            }
        // J format
        case j:
        case jal:
            // to get the address, concatenate the first four bits of PC
            // to the left of our jump address shifted 2 to the left
            mips.pc = (mips.pc & 0xf0000000) | (d->regs.j.target << 2);
            return;
        default:
            mips.pc += 4;
            return;
    }
}

/*
 * Perform memory load or store. Place the address of any updated memory 
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value 
 * that is read, otherwise return -1. 
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory 
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1] 
 * with address 0x00400004, and so forth.
 *
 */
int Mem( DecodedInstr* d, int val, int *changedMem) {
    // val is the mem address to access
    // if val is 0x00400000, then we want to access mips.memory[0]
    // divide by 4 (shift right 2) since data is stored in bytes
    // so access mips.memory[(val - 0x00400000) >> 2]

    // default don't change mem
    changedMem = -1;

    switch((opcode)(d->op)){
        case lw:
            // mips.registers[d->regs.i.rt] = mips.memory[(val - 0x00400000) >> 2]; mem stage, don't edit reg values here
            // just return the value read from memory
            return mips.memory[(val - 0x00400000) >> 2];
        case sw:
            // place the address of any updated memory in *changedMem
            *changedMem = val;
            // store the value at rt into mem
            mips.memory[(val - 0x00400000) >> 2] = mips.registers[d->regs.i.rt];
    }
    return val;
}

/* 
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    if(mips.debugging){
        printf("RegWrite val: %d", val);
    }

    // default don't change register
    *changedReg = -1;

    switch((opcode)d->op){
        // R format
        case 0:
            switch((funct)d->regs.r.funct){
                case addu:
                case and:
                case or:
                case slt:
                case sll:
                case srl:
                case subu:
                    // rd is always modified in R format
                    *changedReg = d->regs.r.rd;
                    mips.registers[d->regs.r.rd] = val;
                    return;
                // added default to get rid of warning prints
                // but caused program to terminate before printInfo
                 default:
                     return;
            }
        // I format
        case addiu:
        case andi:
        case lui:
        case lw:
        case ori:
            // rt is always in I format
            *changedReg = d->regs.i.rt;
            mips.registers[d->regs.i.rt] = val;
            return;
        // J format
        case jal:
            // $ra is reg 31
            *changedReg = 31;
            mips.registers[31] = val;
            return;
    }
}
