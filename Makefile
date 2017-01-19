# Library projects
LIBS =            \
	libmuxml      \
	libmutools    \
	libmucfgcheck \
	libmucfgvcpu  \
	liballoc      \
	libpaging

# Library projects with no unit test suite
LIBS_NO_TESTS = \
	libmulog    \
	libtest

# Tool projects
TOOLS =            \
	mucfgalloc     \
	mucfgexpand    \
	mucfgmemhashes \
	mucfgmerge     \
	mucfgvalidate  \
	mucgenspec     \
	mucheckelf     \
	mucheckstack   \
	mugenacpi      \
	mugeniobm      \
	mugenmsrbm     \
	mugenmsrstore  \
	mugenpt        \
	mugensinfo     \
	mugenspec      \
	mugenvtd       \
	mugenzp        \
	mulnxbzpatch   \
	mupack

# Projects to build
PROJECTS =   \
	$(LIBS)  \
	$(TOOLS) \

# Projects to test
TESTS =      \
	$(LIBS)  \
	$(TOOLS)

# Projects to clean
CLEAN =              \
	$(LIBS)          \
	$(LIBS_NO_TESTS) \
	$(TOOLS)

all: projects

projects:
	@for prj in $(PROJECTS); do $(MAKE) -C $$prj || exit 1; done

tests:
	@for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

install:
	@for prj in $(TOOLS); do $(MAKE) $@ -C $$prj PREFIX=$(PREFIX) || exit 1; done

clean:
	@for prj in $(CLEAN); do $(MAKE) $@ -C $$prj || exit 1; done
