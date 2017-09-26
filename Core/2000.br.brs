00010 ! Replace S:\Core\2000.br
00020 ! (previously fn2000)   converts mmddyy (of x) to ccyymmdd and returns it as the value of fndate_mmddyy_to_ccyymmdd
00030 ! ______________________________________________________________________
00040   def library fndate_mmddyy_to_ccyymmdd(x)
00042     let fndate_mmddyy_to_ccyymmdd=date(days(x,'mmddyy'),'ccyymmdd')
00210   fnend 
