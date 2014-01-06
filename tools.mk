include ../../Makeconf

all: $(TOOL)

tests: test_$(TOOL)
	@obj/tests/test_runner

$(TOOL):
	@gprbuild $(BUILD_OPTS) -P$@

test_$(TOOL):
	@gprbuild $(BUILD_OPTS) -P$@ -XBUILD=tests

clean:
	@rm -rf bin obj
