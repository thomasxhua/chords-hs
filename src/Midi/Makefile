CXX = g++
CPPFLAGS = -D__WINDOWS_MM__
CXXFLAGS = -std=c++17 -Wall -Wextra -pedantic -static -c
LDFLAGS  = -lwinmm -static-libgcc -static-libstdc++

EXT_RTMIDI = \
	ext/rtmidi/RtMidi.cpp \

SRC_MIDI = \
	$(EXT_RTMIDI) \
	src/midi/device.cpp \
	src/midi/interface.cpp \
	src/midi/interface_cl.cpp \
	src/midi/ffi.cpp \

SRC = \
	$(SRC_MIDI) \

OBJDIR = build

OBJ = $(SRC:%.cpp=$(OBJDIR)/%.o)

LIB = midi.dll

all: $(LIB)

$(LIB): $(OBJ)
	$(CXX) -shared -o $@ $^ $(LDFLAGS)

$(OBJDIR)/%.o: %.cpp
	if not exist "$(dir $@)" @mkdir "$(dir $@)"
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $<

clean:
	del /Q $(OBJDIR)\*.o
	del /Q $(LIB)

