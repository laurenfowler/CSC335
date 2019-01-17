#!/bin/bash
gfortran -c module.f90
gfortran ddpop.f90 module.o 
