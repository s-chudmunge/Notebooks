import numpy as NP
import matplotlib.pyplot as PP
E=NP.loadtxt('E.dat')
NP.shape(E)
PP.imshow(E,cmap='jet',origin='lower',extent=[0.,1.,0.,1.])
PP.colorbar()
