# Makefile for Ada Prolog Interpreter
# Copyright (C) 2026 Ulrik Hørlyk Hjort

# Compiler settings
GNATMAKE = gnatmake
GNATCLEAN = gnatclean

# Directories
SRC_DIR = src
BUILD_DIR = build
BIN_DIR = bin

# Compiler flags
GNAT_FLAGS = -g -O2 -gnatwa

# Output executable
TARGET = prolog
MAIN = prolog_main

.PHONY: all clean run debug release directories

all: directories
	cd $(SRC_DIR) && $(GNATMAKE) $(GNAT_FLAGS) $(MAIN).adb -o ../$(BIN_DIR)/$(TARGET) -D ../$(BUILD_DIR)

directories:
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(BIN_DIR)

# Debug build with extra checks
debug: GNAT_FLAGS = -g -O0 -gnatwa -gnata -gnato -fstack-check -gnatVa
debug: directories
	cd $(SRC_DIR) && $(GNATMAKE) $(GNAT_FLAGS) $(MAIN).adb -o ../$(BIN_DIR)/$(TARGET) -D ../$(BUILD_DIR)

# Release build with optimizations
release: GNAT_FLAGS = -O3 -gnatn -gnatp
release: directories
	cd $(SRC_DIR) && $(GNATMAKE) $(GNAT_FLAGS) $(MAIN).adb -o ../$(BIN_DIR)/$(TARGET) -D ../$(BUILD_DIR)

# Run the interpreter
run: all
	./$(BIN_DIR)/$(TARGET)

# Run the test suite
test: all
	@./run_tests.sh

# Clean build artifacts
clean:
	@rm -rf $(BUILD_DIR)
	@rm -rf $(BIN_DIR)
	@rm -f $(SRC_DIR)/*.ali $(SRC_DIR)/*.o $(SRC_DIR)/b~*
	@rm -f *.ali *.o
	@echo "Clean complete."

# Install to /usr/local/bin (requires sudo)
install: all
	install -m 755 $(BIN_DIR)/$(TARGET) /usr/local/bin/

# Uninstall
uninstall:
	rm -f /usr/local/bin/$(TARGET)

# Help
help:
	@echo "Ada Prolog Interpreter - Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all      - Build the interpreter (default)"
	@echo "  debug    - Build with debug flags and runtime checks"
	@echo "  release  - Build with optimizations"
	@echo "  run      - Build and run the interpreter"
	@echo "  test     - Run the test suite"
	@echo "  clean    - Remove build artifacts"
	@echo "  install  - Install to /usr/local/bin"
	@echo "  uninstall- Remove from /usr/local/bin"
	@echo "  help     - Show this help message"
