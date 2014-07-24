include ../../Makeconf

GNATTEST_RUNNER = $(OBJ_DIR)/tests/gnattest/harness/test_runner
GNATTEST_DRIVER = $(OBJ_DIR)/tests/gnattest/harness/test_driver
TESTS_DIR       = $(CURDIR)/tests

SRC_FILES  = $(wildcard $(SRC_DIR)/*)
SRC_FILES += $(wildcard $(TESTS_DIR)/additional/*)

all: $(COMPONENT)

$(DEPENDS) $(TDEPENDS):
	@$(MAKE) -s -C $(TOP_DIR)/tools/$@

$(COMPONENT): $(DEPENDS) $(COMPONENT_TARGETS)
	@gprbuild $(BUILD_OPTS) -P$@

$(OBJ_DIR)/.harness_stamp: $(SRC_FILES)
	@mkdir -p $(OBJ_DIR)/tests
	gnattest --tests-dir=$(TESTS_DIR) -Pgnattest_$(COMPONENT)
	@touch $@

build_tests: $(DEPENDS) $(TDEPENDS) $(TEST_TARGETS) $(OBJ_DIR)/.harness_stamp
	gprbuild $(BUILD_OPTS) -P$(GNATTEST_DRIVER) -XBUILD=tests \
		-cargs -ftest-coverage -fprofile-arcs \
		-largs -fprofile-generate

tests: build_tests
	$(GNATTEST_RUNNER)

install: $(COMPONENT)
	@install -m 755 -D bin/$(COMPONENT) $(PREFIX)/bin/$(COMPONENT)

clean:
	@rm -rf bin obj $(ADDITIONAL_CLEAN)
