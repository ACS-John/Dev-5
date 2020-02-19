00010 ! Replace S:\acsUB\conversion\fixfinalbilling
00020 ! this program will createa final billiing core based on the * in front of the alpha sort name.
00030 !
00040   library 'S:\Core\Library': fnxit,fnerror,fnpause
00050   on error goto Ertn
00060   dim b(11),a(7),d(15),alpha$*7,f2$*12,extra(23),extra$(11)*30,ba(12)
00070   dim custname$*30,badr(2)
00080   dim z$*10,e$(4)*30,f$(3)*12,c(4),g(12),adr(2),alp$*7,gb(10)
00090   dim x$*10,p$*10
00100 !
00120   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\uBIndex.h[cno]",internal,outIn,keyed 
00130 L130: read #1,using L140: alp$,final eof L190
00140 L140: form pos 354,c 7,pos 1821,n 1
00150   if alp$(1:1)="*" and final=0 then final=1 ! cnange any zeros to ones
00160   rewrite #1,using L170: final
00170 L170: form pos 1821,n 1
00180   goto L130
00190 L190: close #1: 
00200   execute "Index [Q]\UBmstr\Customer.h[cno]"&' '&"[Q]\UBmstr\UBIndx2.h[cno] 354 7 Replace DupKeys"
00210 XIT: fnxit
00220 ! __________________________________________________
00230 ! <Updateable Region: ERTN>
00240 ERTN: fnerror(program$,err,line,act$,"xit")
00250   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00260   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00270   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00280 ERTN_EXEC_ACT: execute act$ : goto ERTN
00290 ! /region
