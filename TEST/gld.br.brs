00010 ! Replace Test\GLD
00020 ! test the fnGLD$ function
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fngld$,fntop,fntos,fnacs,fncombof,fncmdset,fncno
00050 ! ______________________________________________________________________
00060   dim resp$(10)*80
00070 ! ______________________________________________________________________
00080   let fncno(cno)
00090   let fntos(sn$='alpha-GLD') !:
        let lc=0
00100   let fncombof('gld-alpha',lc+=1,1,0,env$('Q')&"\CLmstr\GLmstr.h"&str$(cno),13,20,1,12,env$('Q')&"\CLmstr\GLIndx2.h"&str$(cno),1) !:
        let resp$(1)=fngld$('0   700  0')
00110   let fncmdset(1)
00120   let fnacs(sn$,0,mat resp$,ckey)
00130   print resp$(1)
00140   print fngld$('0   375  0')
00150   print 'first 0 should be in pos 24 - first blank in 22'
00160   stop 
