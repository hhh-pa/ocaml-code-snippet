# Out-of-kit build for the vendored inscount tool. PIN_ROOT must point
# at the extracted kit root (the directory holding the `pin`
# launcher); the kit's own Config makefiles do the heavy lifting.
ifdef PIN_ROOT
CONFIG_ROOT := $(PIN_ROOT)/source/tools/Config
else
CONFIG_ROOT := ../Config
endif
include $(CONFIG_ROOT)/makefile.config
include makefile.rules
include $(TOOLS_ROOT)/Config/makefile.default.rules
