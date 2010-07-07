GCC=gcc
LDFLAGS=-shared -fpic
SRCS=`ls *.c`
OUTPUT=test_drv.so
CFLAGS=-o ${OUTPUT}
INCLUDE=-I /usr/local/lib/erlang/erts-5.8/include/

all:
	${GCC} ${SRCS} ${LDFLAGS} ${INCLUDE} ${CFLAGS}

clean:
	rm -rf ${OUTPUT}
