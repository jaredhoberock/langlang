CXX := clang++-19

LANGLANG_SOURCES = ../interpreter.hpp \
		   ../main.cpp \
		   ../parser.hpp \
		   ../source_location.hpp \
		   ../syntax.hpp \
		   ../token_stream.hpp

TEST_SRCS = 0_hello.lox

# this rule runs a test silently
%.test : %.lox langlang
	./langlang $< > /dev/null

# a list of test programs with their file extension replaced with .test
all-test-programs : $(addsuffix .test, $(basename $(wildcard *.lox)))

langlang: $(LANGLANG_SOURCES)
	$(CXX) -std=c++23 -I.. -Wall -Wextra -Werror ../main.cpp -lstdc++ -lfmt -g -o langlang

all: all-test-programs

clean:
	rm -rf *.o langlang

# Phony Targets
.PHONY: test all clean

