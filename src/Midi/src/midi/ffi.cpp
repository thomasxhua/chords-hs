#include "ffi.h"

#include "constants.h"

midi::Interface* ffi_midi_interface_new()
{
    auto* midi = new midi::Interface();
    return midi;
}

void ffi_midi_interface_free(midi::Interface* midi)
{
    if (!midi)
        return;
    delete midi;
}

void ffi_midi_interface_set_port(midi::Interface* midi, const uint64_t port)
{
    if (!midi)
        return;
    midi->set_port(port);
}

midi::Device* ffi_midi_interface_get_device(const midi::Interface* midi)
{
    if (!midi)
        return nullptr;
    return midi->get_device();
}


uint64_t ffi_midi_interface_get_port_count(const midi::Interface* midi)
{
    if (!midi)
        return 0ULL;
    return midi->get_port_count();
}

const char* ffi_midi_interface_get_port_name(const midi::Interface* midi, const uint64_t i)
{
    if (!midi)
        return "";
    return midi->get_port_name(i).c_str();
}

midi::Device::Keys* ffi_midi_device_get_keys(midi::Device* device)
{
    if (!device)
        return nullptr;
    return device->get_keys_ptr();
}

