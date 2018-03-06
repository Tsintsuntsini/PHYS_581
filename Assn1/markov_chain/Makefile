FILES = distributions.f95 random_distributions.f95 main.f95

all:
	gfortran -o main.o $(FILES)
	./main.o
	python3 plot.py
