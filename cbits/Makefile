CC = clang
CCFLAGS = -fPIC -DAUGURCPU -O3
CLIB = -L/usr/local/lib/ -lgslcblas -lgsl -lm 
CINCLUDE = -I/usr/local/include/

NVCC = nvcc
NVCCFLAGS = -arch=sm_35 -Xcompiler -fPIC -O3

OBJ = augur_blkop.o augur_dist.o augur_math.o augur_matop.o augur_rtmem.o augur_rtval.o augur_util.o augur_vecop.o 

libaugur_util_cpu.dylib: $(OBJ)
	$(CC) -shared -o $@ $^ $(CLIB)

test: $(OBJ)
	$(CC) -DAUGURCPU $(CINCLUDE) test.c $(OBJ) $(CLIB)

%.o : %.c
	$(CC) $(CINCLUDE) $(CCFLAGS) -o $@ -c $<

.PHONY: clean

clean:
	rm -f $(OBJ) libaugur_util_cpu.dylib
