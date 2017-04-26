CC = clang
CCFLAGS = -fPIC -DAUGURCPU -O3
CLIB = -L/usr/local/lib/ -lgslcblas -lgsl -lm 
CINCLUDE = -I/usr/local/include/
CPUDIR = cpu

NVCC = nvcc
NVCCFLAGS = -arch=sm_35 -Xcompiler -fPIC -O3
GPUDIR = gpu


OBJ = augur_blkop.o augur_blkstk.o augur_dist.o augur_math.o augur_matop.o augur_rtmem.o augur_rtval.o augur_util.o augur_vecop.o 

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	CPULIB := libaugur_util_cpu.so
endif
ifeq ($(UNAME_S),Darwin)
	CPULIB := libaugur_util_cpu.dylib
endif

libcpu : $(addprefix $(CPUDIR)/,$(OBJ))
	$(CC) -shared -o $(CPULIB) $^ $(CLIB)

test : $(OBJ)
	$(CC) -DAUGURCPU $(CINCLUDE) test.c $(OBJ) $(CLIB)

$(CPUDIR)/%.o : %.c $(CPUDIR)
	$(CC) $(CINCLUDE) $(CCFLAGS) -o $@ -c $<

$(CPUDIR):
	mkdir $@

libgpu: $(addprefix $(GPUDIR)/,$(OBJ)) ;

$(GPUDIR)/%.o : %.cu $(GPUDIR)
	$(NVCC) $(CINCLUDE) $(NVCCFLAGS) -o $@ -c $< -dc

$(GPUDIR):
	mkdir $@

%.cu : %.c
	cp $< $@

.PHONY: clean

clean:
	rm -f $(addprefix $(CPUDIR)/,$(OBJ)) $(addprefix $(GPUDIR)/,$(OBJ)) $(CPULIB)
