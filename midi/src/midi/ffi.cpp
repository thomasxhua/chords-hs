#include "ffi.h"

#include "constants.h"

uint64_t ffi_fibonacci(const uint64_t n)
{
    if (n <= 1)
        return 1ULL;
    uint64_t f_1=1, f_2=1;
    for (uint64_t i=2; i<=n; ++i)
    {
        const uint64_t f_0 = f_1+f_2;
        f_2 = f_1;
        f_1 = f_0;
    }
    return f_1;
}

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

uint64_t ffi_midi_device_get_keys_low(const midi::Device* device)
{
    if (!device)
        return 0ULL;
    const auto keys = device->get_keys();
    uint64_t temp = 0;
    for (uint64_t i=0ULL; i<64ULL; ++i)
        if (keys[i])
            temp |= (1ULL << i);
    return temp;
}

uint64_t ffi_midi_device_get_keys_high(const midi::Device* device)
{
    if (!device)
        return 0ULL;
    const auto keys = device->get_keys();
    uint64_t temp = 0;
    for (uint64_t i=0ULL; i<64ULL; ++i)
        if (keys[i+64ULL])
            temp |= (1ULL << i);
    return temp;
}

