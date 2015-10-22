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

# input files
SRCS = $(notdir $(wildcard $(SRC_DIR)/*.cpp))
OBJS = $(addprefix $(OBJ_DIR)/,$(SRCS:.cpp=.o))
DEPS = $(addprefix $(OBJ_DIR)/,$(SRCS:.cpp=.d))

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
all: $(TARGET)
$(TARGET): $(OBJS)
	@mkdir -p $(PROGRAM_DIR)
	@if [ ! -f $(PROGRAM_DIR)/opencv_ffmpeg300_64.dll ]; then \
		cp -p /mingw64/local/opencv-3.0.0/bin/opencv_ffmpeg300_64.dll $(PROGRAM_DIR); \
	fi
	$(CXX) $(STATICFLAG) $^ $(LDFLAGS) -o $(PROGRAM_DIR)/$@

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(OBJ_DIR)
	$(CXX) $(CXXFLAGS) -c -MMD -MP -MF $(@:%.o=%.d) $< -o $@

# コマンドラインから与えた最終ターゲットが "clean" 以外
ifneq "$(MAKECMDGOALS)" "clean"
-include $(DEPS)
endif

# clean
clean:
	$(RM) -r $(OUT_DIR)
