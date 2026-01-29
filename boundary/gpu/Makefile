CUDA_HOME ?= /usr/local/cuda
CC = gcc
CFLAGS = -shared -fPIC -O2 -Wall -I$(CUDA_HOME)/include
LDFLAGS = -L$(CUDA_HOME)/lib64 -lcuda -lnvrtc

TARGET = libfold_gpu.so

all: $(TARGET)

$(TARGET): bridge.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

clean:
	rm -f $(TARGET)

.PHONY: all clean
