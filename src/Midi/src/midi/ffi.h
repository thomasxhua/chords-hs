#pragma once

#include <cstdint>

#include "interface.h"

extern "C"
{

midi::Interface* ffi_midi_interface_new();
void ffi_midi_interface_free(midi::Interface* midi);

void ffi_midi_interface_set_port(midi::Interface* midi, const uint64_t port);
uint64_t ffi_midi_interface_get_port_count(const midi::Interface* midi);
const char* ffi_midi_interface_get_port_name(const midi::Interface* midi, const uint64_t i);
midi::Device* ffi_midi_interface_get_device(const midi::Interface* midi);

midi::Device::Keys* ffi_midi_device_get_keys(midi::Device* device);

}

