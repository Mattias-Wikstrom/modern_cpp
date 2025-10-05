# ----------------- Compilers -----------------
CXX ?= g++
CXXFLAGS ?= -std=c++20 -Wall -Wextra -O2

GHC ?= ghc
RUSTC ?= rustc
SCALA ?= scala-cli compile
GO ?= go

# ----------------- Source files -----------------
CPP_SOURCES := $(wildcard *.cpp)
HS_SOURCES := $(wildcard *.hs)
RS_SOURCES := $(wildcard *.rs)
SCALA_SOURCES := $(wildcard *.scala)
GO_SOURCES := $(wildcard *.go)

# Executables
CPP_EXES := $(CPP_SOURCES:.cpp=)
HS_EXES := $(HS_SOURCES:.hs=)
RS_EXES := $(RS_SOURCES:.rs=)
SCALA_EXES := $(SCALA_SOURCES:.scala=)
GO_EXES := $(GO_SOURCES:.go=)

EXES := $(CPP_EXES) $(HS_EXES) $(RS_EXES) $(SCALA_EXES) $(GO_EXES)

# ----------------- Default target -----------------
all: $(EXES)

# ----------------- Build rules -----------------
# C++
%: %.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

# Haskell
%: %.hs
	$(GHC) $< -o $@

# Rust
%: %.rs
	$(RUSTC) $< -o $@

# Scala (using scala-cli)
%: %.scala
	$(SCALA) $<

# Go
%: %.go
	$(GO) build -o $@ $<

# ----------------- Clean -----------------
clean:
	rm -f $(EXES) *.o *.hi *.hi-boot *.class
	rm -rf .bsp/ .scala-build/

.PHONY: all clean

