#include "interface_cl.h"

#include <iostream>
#include <cstdint>

midi::InterfaceCL::InterfaceCL()
    : Interface()
{}

void midi::InterfaceCL::dialog_select_port()
{
    std::string port_name;
    const uint64_t number_of_ports = rtmidi_in->getPortCount();
    if (!number_of_ports)
    {
        std::cout << "No MIDI devices found!" << std::endl;
        return;
    }
    std::cout << "Choose one of the following (" << number_of_ports << ") MIDI devices:\n";
    for (uint64_t i=0; i<number_of_ports; ++i)
    {
        try
        {
            port_name = rtmidi_in->getPortName(i);
        }
        catch (const RtMidiError& error)
        {
            error.printMessage();
        }
        std::cout << "(" << i << ") " << port_name << "\n";
    }
    uint64_t port_number;
    std::cout << "Enter port number: ";
    std::cin >> port_number;
    set_port(port_number);
}

