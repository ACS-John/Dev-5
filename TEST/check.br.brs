autoLibrary
dim response$(3)*80,cap$*128
fnTop(program$,cap$='Test Checkboxes')
fnTos(sn$="check_test")
fnChk(2,40,"Check #1 And then some (align=1)",myalign:=1,0) : response$(1)='True'
fnChk(4,40,"Check #2 And then some (align=0)",myalign:=0,0) : response$(2)='True'
fnChk(6,40,"Check #3 And then some (align=1) this one is longer",myalign:=1,0) : response$(3)='True'
fnCmdSet(2)
fnAcs2(mat response$,ckey)
pr mat response$
