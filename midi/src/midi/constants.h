#pragma once

#include <cstdint>

namespace midi
{

constexpr size_t MESSAGE_SIZE_EXPECTED = 3;
constexpr size_t MESSAGE_IDX_TYPE      = 0;
constexpr size_t MESSAGE_IDX_VAL0      = 1;
constexpr size_t MESSAGE_IDX_VAL1      = 2;

constexpr uint64_t MESSAGE_VAL_MAX = 128;

constexpr uint8_t MESSAGE_VOICE_TYPE_NOTE_OFF = 0x80;
constexpr uint8_t MESSAGE_VOICE_TYPE_NOTE_ON  = 0x90;

}

