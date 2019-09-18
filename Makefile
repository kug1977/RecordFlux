VERBOSE ?= @

build-dir := build
noprefix-dir := build/noprefix

project := test
test-bin := $(build-dir)/test
test-files := $(wildcard generated/rflx-*.ad? tests/*.ad? tests/*.raw test.gpr)

ifneq ($(NOPREFIX),)
project := $(noprefix-dir)/test
test-bin := $(noprefix-dir)/$(build-dir)/test
test-files := $(addprefix $(noprefix-dir)/, $(subst /rflx-,/,$(test-files)))
endif

.PHONY: test test_python test_spark prove_spark prove_spark_ci clean

test: test_python test_spark prove_spark

test_python:
	coverage run --branch --source=rflx -m unittest -b
	mypy bin/*.py rflx/*.py tests/*.py
	pylint bin/*.py rflx/*.py tests/*.py
	flake8 bin/*.py rflx/*.py tests/*.py
	isort -c -w 100 bin/*.py rflx/*.py tests/*.py

test_spark: $(test-files)
	gprbuild -P$(project)
	$(test-bin)

prove_spark: $(test-files)
	gnatprove -P$(project) $(GNATPROVE_ARGS)

prove_spark_ci: $(test-files)
	gnatprove -P$(project) -Xaunit=no --report=statistics $(GNATPROVE_ARGS)

clean:
	gprclean -Ptest
	gnatprove -Ptest --clean
	test -d $(noprefix-dir) && rm -r $(noprefix-dir) || true
	rmdir $(build-dir)

remove-prefix = $(VERBOSE) \
	mkdir -p $(dir $@) && \
	sed 's/RFLX.//g' $< > $@.tmp && \
	mv $@.tmp $@

$(noprefix-dir)/generated/%: generated/rflx-%
	$(remove-prefix)

$(noprefix-dir)/tests/%: tests/rflx-%
	$(remove-prefix)

$(noprefix-dir)/%: %
	$(remove-prefix)
