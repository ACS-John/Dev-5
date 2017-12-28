00020   library 'S:\Core\Library': fntop,fnChk,fnTos,fnAcs,fnCmdSet
00030   dim response$(3)*80,cap$*128
00040   fntop(program$,cap$='Test Checkboxes')
00050   fnTos(sn$="check_test")
00065   fnChk(2,40,"Check #1 And then some (align=1)",myalign:=1,0) : response$(1)='True'
00075   fnChk(4,40,"Check #2 And then some (align=0)",myalign:=0,0) : response$(2)='True'
00085   fnChk(6,40,"Check #3 And then some (align=1) this one is longer",myalign:=1,0) : response$(3)='True'
00150   fnCmdSet(2)
00152   fnAcs(sn$,win,mat response$,ckey)
00160   pr mat response$
