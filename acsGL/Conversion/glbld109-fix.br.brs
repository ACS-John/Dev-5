00020 ! (C) COPYRIGHT - 2003 - ADVANCED COMPUTER SERVICES, INC.
00030   library 'S:\Core\Library': fnxit,fntop
00040   open #20: "Name=CNO.H"&wsid$,internal,input,relative 
00050   fntop(program$,"CHANGE_ME")
00060   read #20,using L70,rec=1: cno,cnam$,dat$,cp,nw,process
00070 L70: form pos 1,n 2,c 40,x 20,c 20,pos 89,2*n 1,pos 141,n 1
00080   form c 9,skip 0
00090 !:
        ! 00100  dim vn$*8,nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,holdvn$*8,vcode$*8
00110   dim cnam$*40,dat$*20,adr(2),id1$*25
00120   dim rn$*12,de$*30,adr(2),tvn$*8
00130   open #1: "Name="&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno),internal,outin,keyed 
00140 L140: form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11,2*pd 3
00150 L150: read #1,using L140: vn$,nam$,ad1$,ad2$,csz$ eof L210
00160   let x=val(vn$) conv L190
00170   if x<1 or x>99999999 then goto L190
00180   goto L150
00190 L190: delete #1: 
00200   goto L150
00210 L210: let fnxit
