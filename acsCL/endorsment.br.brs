00010 ! REPLACE S:\acsCL\Endorsment
00020 ! pr endorsment on back of check
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnopenprn,fncloseprn,fntos,fnlbl,fnacs,fntxt,fncmdkey
00050   fntop(program$,"Endorse Checks")
00080   on error goto ERTN
00090 ENDORSE_CHECKS: ! 
00100   fntos(sn$="Endorse")
00110   fnlbl(1,1,"Number of Endorsements:",25,1,0)
00120   fntxt(1,28,6,0,0,"30",0,"You can guess. Too many will only cause you to have to cancel print.") 
00122   resp$(1)=""
00130   fnlbl(2,1,"Bank Account:",25,1,0)
00140   fntxt(2,28,15,0,0,"30",0,"Enter your bank account number if you want it shown on the back of the check.") 
00142   resp$(2)=""
00150   fncmdkey("&Next",1,1,0,"Proceed with printing.")
00160   fncmdkey("&Cancel",5,0,1,"Cancel printing any check endorsments.")
00170   fnacs(sn$,0,mat resp$,ckey) ! endorse check
00180   if ckey=5 then goto XIT
00190   endorsements=val(resp$(1))
00200   bank=val(resp$(2))
00210   fnopenprn
00220   for j=1 to endorsements
00230     pr #255,using "Form pos 1,cc 30,skip 1,cc 30": "For Deposit Only",env$('cnam')(1:30)
00240     if bank>0 then pr #255,using "Form pos 1,cc 30": "Account: "&str$(bank)
00250     pr #255: newpage
00260   next j
00270   fncloseprn
00280   goto XIT
00290 XIT: fnxit
78000 ! <updateable region: ertn>
78020 ERTN: fnerror(program$,err,line,act$,"xit")
78040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
78060   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
78080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
78100 ERTN_EXEC_ACT: execute act$ : goto ERTN
78120 ! </updateable region: ertn>
