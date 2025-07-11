#pragma once

#include <cstdint>

#include "interface.h"

extern "C"
{

uint64_t ffi_fibonacci(const uint64_t n);

midi::Interface* ffi_midi_interface_new();
void ffi_midi_interface_free(midi::Interface* midi);

void ffi_midi_interface_set_port(midi::Interface* midi, const uint64_t port);
midi::Device* ffi_midi_interface_get_device(const midi::Interface* midi);

uint64_t ffi_midi_device_get_keys_low(const midi::Device* device);
uint64_t ffi_midi_device_get_keys_high(const midi::Device* device);

}

