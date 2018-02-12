BUILDDIR ?= build
CFG      ?= debug
NAME     ?= c4
SRCDIR   ?= src

all:

-include config/$(CFG).cfg

# Be verbose about the build.
Q ?= @

BINDIR := $(BUILDDIR)/$(CFG)
BIN    := $(BINDIR)/$(NAME)
SRC    := $(sort $(wildcard $(SRCDIR)/*.cpp))
OBJ    := $(SRC:$(SRCDIR)/%.cpp=$(BINDIR)/%.o)
DEP    := $(OBJ:%.o=%.d)

ifneq ("$(wildcard llvm/install/bin/llvm-config)","")
	LLVM_CONFIG  ?= llvm/install/bin/llvm-config
else
	ifneq ("$(wildcard /llvm/install/bin/llvm-config)","")
		LLVM_CONFIG  ?= /llvm/install/bin/llvm-config
	else
		LLVM_CONFIG  ?= llvm-config
	endif
endif

LLVM_CFLAGS  := $(shell $(LLVM_CONFIG) --cppflags)
LLVM_LDFLAGS := $(shell $(LLVM_CONFIG) --ldflags --libs --system-libs)

CFLAGS   := $(LLVM_CFLAGS) -Wall -W $(CFLAGS)
CXXFLAGS += $(CFLAGS) -std=c++11
LDFLAGS  += $(LLVM_LDFLAGS)

#CFLAGS-Optimizer.o += -fno-rtti
#CFLAGS-InlinePass.o += -fno-rtti
#CFLAGS-OptimizerPass.o += -fno-rtti
#CFLAGS-OptimizerUtils.o += -fno-rtti
#CFLAGS-DecompilerPass.o += -fno-rtti

DUMMY := $(shell mkdir -p $(sort $(dir $(OBJ))))

.PHONY: all clean

all: $(BIN)

-include $(DEP)

clean:
	@echo "===> CLEAN"
	$(Q)rm -fr $(BINDIR)

$(BIN): $(OBJ)
	@echo "===> LD $@"
	$(Q)$(CXX) -o $(BIN) $(OBJ) $(LDFLAGS)

$(BINDIR)/%.o: $(SRCDIR)/%.cpp
	@echo "===> CXX $(CFLAGS-$(notdir $@)) $<"
	$(Q)$(CXX) $(CXXFLAGS) $(CFLAGS-$(notdir $@)) -MMD -c -o $@ $<
