# Makefile

# Variables
SML_FILES = main.sml
EXECUTABLE = main

# Default target
all: $(EXECUTABLE)

# Build rules
$(EXECUTABLE): $(SML_FILES)
	mlton -output $(EXECUTABLE) $(SML_FILES)

# Clean target
clean:
	rm -f $(EXECUTABLE)
