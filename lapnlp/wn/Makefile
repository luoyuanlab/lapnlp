# yluo - 07/09/2010 creation
# Makefile for wordnet.c
SRC = .
INC = $(WNHOME)/include
LIB = $(WNHOME)/lib
OBJ = obj
BIN = .
CC = gcc
CFLAGS = -fPIC -c -g -Wall
CLDFLAGS = -shared -g


INCLUDES =\
${INC}/wn.h

OBJECTS =\
${OBJ}/wordnet.o

wnet: ${SRC}/main.c wordnet.h libwordnet.so
		${CC} -g -Wall -I${INC} -L. \
		${SRC}/main.c -lwordnet -o ${BIN}/wnet

libwordnet.so: ${OBJECTS} ${LIB}/libWN.a
		${CC} ${CLDFLAGS} -Wl,-soname,libwordnet.so \
		${OBJECTS} ${LIB}/libWN.a -o ${BIN}/libwordnet.so  

${OBJ}/wordnet.o: ${SRC}/wordnet.c ${INCLUDES}
		${CC} ${CFLAGS} -I${INC} ${SRC}/wordnet.c \
		-o ${OBJ}/wordnet.o

clean:
	/bin/rm ${OBJ}/*.o
	/bin/rm ${BIN}/libwordnet.so
	/bin/rm ${BIN}/wnet