from scipy import signal
from scipy.signal import freqz
import numpy as np
import matplotlib.pyplot as plt

if __name__ == "__main__": 

   fs = 100000000.0 # Hz -- Arty7 board frequency oscillator
   b = signal.firwin(5, 0.1, window='boxcar')   # 5 in the number of the coefficients 
                                                # 0.1 is the cutoff frequency
   w, h = signal.freqz(b)
   print "coefficients : "
   print b

   fig = plt.figure()
   fig.set_size_inches(16, 12, forward=True)
   ax1 = fig.add_subplot(211)   
   ax1.plot(w/3.14, 20*np.log10(abs(h))) 
   ax1.set_xlabel('w [Frequency [rad/samples]')
   ax1.set_ylabel('Amplitude [dB]')
   ax1.set_title('Digital Frequency Filter Response')
   ax1.grid()
   ax2 = fig.add_subplot(212)
   ax2.plot(w/3.14*fs/2, 20*np.log10(abs(h))) 
   ax2.set_xlim(min(w/3.14*fs/2), max(w/3.14*fs/2))
   ax2.set_xlabel('f [Frequency [Hz]')
   ax2.set_ylabel('Amplitude [dB]')
   ax2.grid()
   fig.savefig('f_response.png')

