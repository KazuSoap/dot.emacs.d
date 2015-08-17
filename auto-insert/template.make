# ------------------------------------
# Makefile
# ------------------------------------

# target
TARGET = $(shell basename `pwd`)

# directory
OUT_DIR = build
SRC_DIR = src
HEADER_DIR = include
PROGRAM_DIR = $(OUT_DIR)/bin
OBJ_DIR = $(OUT_DIR)/obj
DEPEND_DIR = $(OUT_DIR)/depend

# src files
SRCS = $(notdir $(wildcard $(SRC_DIR)/*.cpp))

# generated files
DEPENDS = $(addprefix $(DEPEND_DIR)/,$(SRCS:.cpp=.depend))

# compiler
CXX = g++ -std=c++11
CXXFLAGS = -I./$(HEADER_DIR) `pkg-config --cflags opencv`
LDFLAGS  = `pkg-config --libs opencv`

# dummy target
.PHONY : release debug all clean

# release
release: CXX += -s -O2
release: STATICFLAG = -static
release: CXXFLAGS = -I./$(HEADER_DIR) `pkg-config --cflags opencv_static`
release: LDFLAGS  = `pkg-config --libs opencv_static`
release: all

# debug
debug: CXX += -g -O0
debug: all

# all
all: $(DEPENDS) $(TARGET)
$(TARGET): $(addprefix $(OBJ_DIR)/,$(SRCS:.cpp=.o))
	@mkdir -p $(PROGRAM_DIR)
	$(CXX) $(STATICFLAG) $^ $(LDFLAGS) -o $(PROGRAM_DIR)/$@

$(DEPEND_DIR)/%.depend: $(SRC_DIR)/%.cpp $(wildcard $(HEADER_DIR)/*.h)
	@echo "generating $@"
	@mkdir -p $(DEPEND_DIR)
	@$(CXX) -I./$(HEADER_DIR) -MM -MP $< > $@

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -c $^ -o $@

ifneq "$(MAKECMDGOALS)" "clean"
-include $(DEPENDS)
endif

# clean
clean:
	$(RM) -r $(OUT_DIR)
