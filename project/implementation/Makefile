CC    ?= clang
CXX   ?= clang++

LEX     = flex
YACC    = /usr/local/bin/bison 

EXE = intergdbc

CDEBUG = -g -Wall

CXXDEBUG = -g -Wall

CSTD = -std=c99
CXXSTD = -std=c++11

CFLAGS = -O0  $(CDEBUG) $(CSTD) 
CXXFLAGS = -O0  $(CXXDEBUG) $(CXXSTD) 

CPPOBJ = Main Driver SchemaNode AttrNode ConfNode VertexNode EdgeNode GraphNode
SOBJ =  Parser Scanner

FILES = $(addsuffix .cpp, $(CPPOBJ))

OBJS  = $(addsuffix .o, $(CPPOBJ))

CLEANLIST =  $(addsuffix .o, $(OBJ)) $(OBJS) \
			 Parser.tab.cc Parser.tab.hh \
				 location.hh position.hh \
			    stack.hh Parser.output Parser.o \
				 Scanner.o Scanner.yy.cc $(EXE)\

.PHONY: all
all: intergdbc

intergdbc: $(FILES)
	$(MAKE) $(SOBJ)
	$(MAKE) $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(EXE) $(OBJS) Parser.o Scanner.o $(LIBS)


Parser: Parser.yy
	$(YACC) -d -v Parser.yy
	$(CXX) $(CXXFLAGS)  -c -o Parser.o Parser.tab.cc

Scanner: Scanner.l
	flex --outfile=Scanner.yy.cc  $<
	$(CXX)  $(CXXFLAGS)  -c Scanner.yy.cc -o Scanner.o


.PHONY: clean
clean:
	rm -rf $(CLEANLIST) *~
