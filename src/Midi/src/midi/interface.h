#pragma once

#include <cstdint>
#include <memory>

#include "../../ext/rtmidi/RtMidi.h"

#include "device.h"

namespace midi
{

/*
TODO:
- handle reconnects
- ignoreTypes
*/

class Interface
{
protected:
    std::unique_ptr<RtMidiIn> rtmidi_in = nullptr;
    uint64_t rtmidi_port_number;
    bool rtmidi_ignore_sysex = false;
    bool rtmidi_ignore_time  = true;
    bool rtmidi_ignore_sense = true;

    void rtmidi_init();

    std::unique_ptr<midi::Device> device = nullptr;

public:
    Interface();
    void set_port(const uint64_t port_number);

    uint64_t get_port_count() const;
    std::string get_port_name(const uint64_t i) const;

    midi::Device* get_device() const;
};

}

