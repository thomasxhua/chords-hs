#include "interface.h"

midi::Interface::Interface()
    : rtmidi_in(std::make_unique<RtMidiIn>())
    , rtmidi_port_number(0)
{
    device = std::make_unique<midi::Device>();
    rtmidi_init();
}

void midi::Interface::set_port(const uint64_t port_number)
{
    rtmidi_port_number = port_number;
    rtmidi_in->openPort(rtmidi_port_number);
}

void midi::Interface::dialog_select_port()
{}

void midi::Interface::rtmidi_init()
{
    rtmidi_in->ignoreTypes(rtmidi_ignore_sysex,rtmidi_ignore_time,rtmidi_ignore_sense);
    rtmidi_in->setCallback(&device->callback, static_cast<void*>(get_device()));
}

midi::Device* midi::Interface::get_device() const
{
    return device.get();
}
