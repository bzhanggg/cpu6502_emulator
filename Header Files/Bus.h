//
// Created by Brian Zhang on 8/28/23.
//
#pragma once
#include <cstdint> // rename the standard types (int, short, char) into explicit types
#include "olc6502.h"
#include <array>

#ifndef CPU6502_EMULATOR_BUS_H
#define CPU6502_EMULATOR_BUS_H


class Bus {
public:
    Bus();
    ~Bus();

public: // Devices on Bus
    olc6502 cpu;

    // fake RAM
    std::array<uint8_t, 64 * 1024> ram{};

public: // Bus read and write
    void write(uint16_t addr, uint8_t data);
    uint8_t read(uint16_t addr, bool bReadOnly = false);
};

#endif //CPU6502_EMULATOR_BUS_H
