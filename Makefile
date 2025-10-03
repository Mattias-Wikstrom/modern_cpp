# ----------------- Configurable compiler -----------------
# Set CXX to g++ or clang++ when calling make, e.g.
#   make CXX=clang++
CXX ?= g++
CXXFLAGS ?= -std=c++20 -Wall -Wextra -O2

# ----------------- Source files -----------------
SOURCES := $(wildcard *.cpp)
# Executables have the same name as source files without .cpp
EXES := $(SOURCES:.cpp=)

# Default target: build all executables
all: $(EXES)

# Rule to compile each .cpp into executable
%: %.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

# Clean up
clean:
	rm -f $(EXES)

.PHONY: all clean

