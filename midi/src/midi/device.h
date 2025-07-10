#pragma once

#include <vector>
#include <cstdint>

namespace midi
{

class Device
{
private:
    std::vector<bool> keys;

public:
    static void callback(double delta_time, std::vector<uint8_t>* message, void* user_data);
    void handle_message(const double delta_time, const std::vector<uint8_t>& message);

    Device();

    const std::vector<bool>& get_keys() const;
};

}

