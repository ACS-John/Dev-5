00020   library 'S:\Core\Library': fntop,fnchk,fntos,fnacs,fncmdset
00030   dim response$(3)*80,cap$*128
00040   fntop(program$,cap$='Test Checkboxes')
00050   fntos(sn$="check_test")
00065   fnchk(2,40,"Check #1 And then some (align=1)",myalign:=1,0) : let response$(1)='True'
00075   fnchk(4,40,"Check #2 And then some (align=0)",myalign:=0,0) : let response$(2)='True'
00085   fnchk(6,40,"Check #3 And then some (align=1) this one is longer",myalign:=1,0) : let response$(3)='True'
00150   fncmdset(2)
00152   fnacs(sn$,win,mat response$,ckey)
00160   pr mat response$
