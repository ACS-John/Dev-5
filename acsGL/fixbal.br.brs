00010 ! Replace S:\acsGL\FixBal ! moves the beginning balance to current balance if current balance is 0 used for new client who entered beginning balance and no current balance
00020 ! we have a new menu option that does a better job - could be deleted
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror
00050   on error goto ERTN
00060   fntop(program$,"CHANGE_ME")
00070 ! ______________________________________________________________________
00080   dim io1$(12),gln(3,3),fin(3),ta(2),ac(18),te$*1
00090   dim d$*50,bc(13),bp(13),bm(13),rf(6),dn$*3,an$*6,sn$*3,glk$*12,fsk$*5
00100 ! ______________________________________________________________________
00110   fncno(cno)
00120 ! 
00130   open #glmstr=1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed 
00140 READ_GLMSTR: ! 
00150   read #glmstr,using 'Form POS 1,N 3,N 6,N 3,C 50,6*PD 3,42*PD 6.2,2*PD 3': dno,ano,sno,d$,mat rf,bb,cb eof END1
00160   if cb=0 then cb=bb else goto READ_GLMSTR
00170   rewrite #glmstr,using 'Form POS 1,N 3,N 6,N 3,C 50,6*PD 3,42*PD 6.2,2*PD 3': dno,ano,sno,d$,mat rf,bb,cb
00180   goto READ_GLMSTR
00190 ! ______________________________________________________________________
00200 END1: ! 
00210   close #glmstr: 
00220 XIT: stop 
00230 ! ______________________________________________________________________
00240 ! <Updateable Region: ERTN>
00250 ERTN: fnerror(program$,err,line,act$,"xit")
00260   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00270   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00280   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00290 ERTN_EXEC_ACT: execute act$ : goto ERTN
00300 ! /region
00310 ! ______________________________________________________________________
