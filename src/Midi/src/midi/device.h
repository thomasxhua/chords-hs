#pragma once

#include <vector>
#include <cstdint>

namespace midi
{

class Device
{
public:
    struct Keys
    {
        uint64_t low,high;
    };

private:
    Keys keys;

public:
    static void callback(double delta_time, std::vector<uint8_t>* message, void* user_data);
    void handle_message(const double delta_time, const std::vector<uint8_t>& message);

    Device();

    const midi::Device::Keys& get_keys() const;
    midi::Device::Keys* get_keys_ptr();
};

}

