00010 ! Replace Test\GLD
00020 ! test the fnGLD$ function
00030 !
00040   library 'S:\Core\Library': fngld$,fntop,fnTos,fnAcs,fncombof,fnCmdSet,fncno
00050 !
00060   dim resp$(10)*80
00070 !
00080   fncno(cno)
00090   fnTos(sn$='alpha-GLD') !:
        lc=0
00100   fncombof('gld-alpha',lc+=1,1,0,"[Q]\CLmstr\GLmstr.h[cno]",13,20,1,12,"[Q]\CLmstr\GLIndx2.h[cno]",1) !:
        resp$(1)=fngld$('0   700  0')
00110   fnCmdSet(1)
00120   fnAcs(sn$,0,mat resp$,ckey)
00130   pr resp$(1)
00140   pr fngld$('0   375  0')
00150   pr 'first 0 should be in pos 24 - first blank in 22'
00160   stop 
