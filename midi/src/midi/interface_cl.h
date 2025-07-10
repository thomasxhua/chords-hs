#pragma once

#include "interface.h"

namespace midi
{

class InterfaceCL : public Interface
{
public:
    InterfaceCL();
    void dialog_select_port() override;
};

}

