#!/bin/bash

# Integration Tests
echo ""
echo "========================================"
echo "Integration tests with ecukes"
cask exec ecukes

# Unit Tests
echo ""
echo "========================================"
echo "Unit tests with ert"
cd test || exit
emacs -batch -Q -L . -l commify-test.el -f ert-run-tests-batch
