import uhal
import matplotlib.pyplot as plt
import numpy as np
from scipy.fftpack import fft

def twos_comp(val, bits):
    """compute the 2's complement of int value val"""
    if (val & (1 << (bits - 1))) != 0: # if sign bit is set e.g., 8bit: 128-255
        val = val - (1 << bits)        # compute negative value
    return val 

if __name__ == "__main__":

   # Code for the connection with the board
   manager = uhal.ConnectionManager("file://arty7_connection.xml")
   hw = manager.getDevice("arty7")

   size = hw.getNode("size_reg").read() 
   hw.dispatch() # Send IPbus transactions
   size = int(size)

   Ts = 1e-8

   regs = hw.getNode("sq_regs").readBlock(size)    
   hw.dispatch() # Send IPbus transactions
   
   t = np.zeros(size)
   i = 0
   values = np.zeros(size)    
   for x in regs:
      t[i] = i*Ts
      values[i] = twos_comp(x,32)
      i = i+1
     
   regs_fir = hw.getNode("fir_regs").readBlock(size) 
   hw.dispatch()  # Send IPbus transactions
   
   i = 0
   values_fir = np.zeros(size)    
   for x in regs_fir:
      values_fir[i] = twos_comp(x,32)     
      i = i+1

   fft_sig = fft(values) 
   f1 = np.arange(len(values)); f2 = len(values)*Ts; freq_sig = f1/f2;
   fft_fir_sig = fft(values_fir) 
   f1 = np.arange(len(values_fir)); f2 = len(values_fir)*Ts; freq_fir_sig = f1/f2;

   fig = plt.figure()
   ax1 = fig.add_subplot(211)   
   ax1.plot(t,values,t,values_fir)
   ax1.set_xlabel('T [s]')
   ax1.grid()
   ax1.set_xlim(0, max(t))
   ax2 = fig.add_subplot(212)
   ax2.plot(freq_sig[0:(len(fft_sig)/2 -1)],20*np.log10(abs(fft_sig[0:(len(fft_sig)/2 -1)])),freq_fir_sig[0:(len(fft_fir_sig)/2 -1)],20*np.log10(abs(fft_fir_sig[0:(len(fft_fir_sig)/2 -1)])),'r')
   ax2.set_xlabel('F [Hz]')
   ax2.grid()
   ax2.set_xlim(1e6, 5e7)
   fig.savefig('square.png')

   plt.grid()
   plt.show()
