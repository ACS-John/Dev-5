9000   option retain 
9020   def library fntestretain(value)
9040   ! library : fntestretainBLABLALBA
9060     fntestretain=fn_testretainBLABLALBA(value)
9080   fnend
9100   def library fntestretainBLABLALBA(value)
9120     fntestretainBLABLALBA=fn_testretainBLABLALBA(value)
9140   fnend
9160   def fn_testretainBLABLALBA(value)
9180     localvalue+=value
9200     fn_testretainBLABLALBA=localvalue
9220   fnend 
