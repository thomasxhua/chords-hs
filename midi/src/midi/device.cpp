#include "device.h"

#include "constants.h"

midi::Device::Device()
    : keys({0ULL,0ULL})
{}

const midi::Device::Keys& midi::Device::get_keys() const
{
    return keys;
}

midi::Device::Keys* midi::Device::get_keys_ptr()
{
    return &keys;
}

void midi::Device::callback(
    [[maybe_unused]] double delta_time,
    std::vector<uint8_t>* message,
    void* user_data)
{
    static_cast<midi::Device*>(user_data)->handle_message(delta_time, *message);
}

void midi::Device::handle_message(
    [[maybe_unused]] const double delta_time,
    const std::vector<uint8_t>& message)
{
    if (message.size() != midi::MESSAGE_SIZE_EXPECTED)
        return;
    const uint8_t type = message[midi::MESSAGE_IDX_TYPE];
    if (type != midi::MESSAGE_VOICE_TYPE_NOTE_OFF
        && type != midi::MESSAGE_VOICE_TYPE_NOTE_ON)
        return;
    const uint8_t key = message[midi::MESSAGE_IDX_VAL0];
    if (key < midi::MESSAGE_VAL_LOW_MAX)
    {
        if (type == midi::MESSAGE_VOICE_TYPE_NOTE_ON)
            keys.low |= 1ULL << key;
        else
            keys.low &= ~(1ULL << key);
    }
    else
    {
        if (type == midi::MESSAGE_VOICE_TYPE_NOTE_ON)
            keys.high |= 1ULL << (key - midi::MESSAGE_VAL_LOW_MAX);
        else
            keys.high &= ~(1ULL << (key - midi::MESSAGE_VAL_LOW_MAX));
    }
}

