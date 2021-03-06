# Test Structure

All Python test files have the suffix `_test.py` (e.g., `generator_test.py`). The top level directory `tests` does not contain any test files. Utility functions used by multiple test files are collected in `tests/utils.py`.

## Tests

### Unit Tests (`tests/unit`)

A unit test verifies the functionality of a single Python module. The result of a unit test mainly depends on the functionality of the to be tested module, but may involve data structures of different modules (counterexample: validating errors which are actually detected in another module). A unit test is simple, fast and generic (counterexample: testing multiple complex examples). Each test file corresponds to a module in `rflx`.

### Integration Tests (`tests/integration`)

Integration tests verify the correct interaction between multiple modules. Complex tests comprising just a single module may also be included in this directory.

### Property Tests (`tests/property`)

Property-based testing based on [Hypothesis](https://hypothesis.readthedocs.io/).

### SPARK Tests (`tests/spark`)

The SPARK tests verify the correctness of the generated SPARK code. A [test suite](https://docs.adacore.com/live/wave/aunit/html/aunit_cb/aunit_cb.html) and [formal verification](https://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/en/gnatprove.html) is used to ensure functional correctness and prove absence of runtime errors. The to be proven code is contained in `tests/spark/generated`. This code is also used by integration tests for regression testing.

### IDE Tests (`tests/ide`)

IDE tests allow to verify the functionality of the IDE plugin. These tests are not executed automatically.

## Test Data (`tests/data`)

- Binary test message (`tests/data/captured`)
- Test fixtures (`tests/data/fixtures`)
- Test specifications (`tests/data/specs`)

Test data should be kept as local as possible. If test data is required for a single, very specific test case, it should be considered to define the test data locally instead of making the data globally available in one of the directories mentioned above.
