CXX := clang++-19

LANGLANG_SOURCES = ../interpreter.hpp \
		   ../main.cpp \
		   ../parsers.hpp \
		   ../source_location.hpp \
		   ../syntax.hpp \
		   ../token_range.hpp

# this rule runs a test foo.lox and captures its output to foo.out
%.out : %.lox langlang
	./langlang $< > $@

# this rule compares a test's output to its expected result
%.test : %.expected %.out
	diff $^

# a list of test programs with their file extension replaced with .test
all-test-programs : $(addsuffix .test, $(basename $(wildcard *.lox)))

langlang: $(LANGLANG_SOURCES)
	$(CXX) -std=c++23 -I.. -Wall -Wextra -Werror ../main.cpp -lstdc++ -lfmt -g -o langlang

all: all-test-programs

clean:
	rm -rf *.o *.out langlang

# Phony Targets
.PHONY: all-test-programs all clean

