00010 ! Formerly S:\acsGL\CloseMonth
00020 ! GL Month End Closing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnprocess,fnactpd, fntos,fnlbl,fntxt,fncmdset,fnacs, fnconsole,fngethandle,fnindex_it
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim bc(13),tr(7),tr$*12,td$*30
00080 ! ______________________________________________________________________
00090   let fntop(program$,cap$="Close Month")
00100   let fnconsole(off=0)
00110   let fncno(cno)
00120   if fnprocess=1 then goto GET_GOING
00130 SCREEN1: ! 
00140   let fntos(sn$='Close_Month')
00142   let lc=0 : let mylen=22 : let mypos=mylen+2
00150   let fnlbl(lc+=1,1,"Closing Period Number:",mylen,right)
00160   let fntxt(lc,mypos,2,0,0,'number')
00162   let resp$(1)=str$(fnactpd)
00170   let fncmdset(2)
00180   let fnacs(sn$,0,mat resp$,ckey)
00190   if ckey=5 then goto XIT
00200   actpd=val(resp$(1))
00210   if actpd<1 or actpd>13 then goto SCREEN1
00220 ! ______________________________________________________________________
00230 GET_GOING: ! 
02000   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative ioerr L440
02020   read #1,using 'Form pos 384,n 2,POS 406,C 1,POS 417,N 1',rec=1: nap,actrcde$,reccode
02040   close #1: 
02060   if actrcde$="0" or actrcde$="N" then goto OPEN_GLMSTR
02080   let fn_current_to_accumlated_trans
02100 OPEN_GLMSTR: ! 
02120   open #h_glmstr:=1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLINDEX.h"&str$(cno),internal,outin,keyed 
02140 ! fnwait - "Closing Month..."
02160   do 
02180     read #h_glmstr,using 'Form POS 87,14*PD 6.2': cb,mat bc eof EO_GLMSTR
02200     bc(actpd)=cb
02220     bb=cb
02240     rewrite #h_glmstr,using 'Form POS 81,PD 6.2,POS 93,13*PD 6.2,POS 333,2*PD 3': bb,mat bc,0,0
02260   loop 
02280 EO_GLMSTR: ! 
02300   close #h_glmstr: 
02322   let lmu=actpd
02324   actpd=actpd+1
02326   if actpd>nap then actpd=1
02340   let fnactpd(actpd)
02343   open #21: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative 
02344   rewrite #21,using 'Form pos 296,n 2',rec=1: lmu
02345   close #21: 
02360   open #1: "Name="&env$('Q')&"\GLmstr\GLTrans.h"&str$(cno)&",Size=0,RecL=73,Replace",internal,output 
02380   write #1,using 'Form POS 1,N 3,N 6,N 3,N 6,PD 6.2,2*N 2,C 12,C 30,PD 3': 0,0,0,0,0,0,0," "," ",1
02400   close #1: 
02420 L440: ! Open #1: "Name="&env$('Q')&"\GLmstr\acprcks.h"&STR$(CNO)&",SIZE=0,RecL=110,Replace",Internal,Output Ioerr 470
02440 ! WRITE #1,USING 'Form POS 87,14*PD 6.2': 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
02460 ! Close #1:
02480 ! Open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRINDEX.h"&STR$(CNO),Internal,Outin,Keyed Ioerr 520
02500 ! Read #1,Using 'Form POS 271,2*N 5': N1,N2 Eof 510
02520 ! Rewrite #1,Using 'Form POS 271,2*N 5': 0,0
02540 ! Goto 480
02560 ! Close #1:
02580   if reccode=0 then goto GLBREC_DROP
02600   open #h_glbrec:=1: "Name="&env$('Q')&"\GLmstr\GLBRec.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLRecIdx.h"&str$(cno),internal,outin,keyed ioerr GLBREC_DROP
02620   do 
02640     read #h_glbrec,using 'Form POS 63,PD 5.2,POS 68,N 1': a2,a3 eof FINIS
02660     if a3=1 or a2=0 then delete #h_glbrec: 
02680   loop 
02700 FINIS: ! 
02720   close #h_glbrec: 
02740   execute "Copy "&env$('Q')&"\GLmstr\GLBRec.h"&str$(cno)&' '&env$('Q')&"\GLmstr\GLBRec.X -D -n"
02760   execute "Copy  "&env$('Q')&"\GLmstr\GLBRec.X,"&env$('Q')&"\GLmstr\GLBRec.h"&str$(cno)&" -n"
02780   execute "Free "&env$('Q')&"\GLmstr\GLBRec.X -n"
02800   fnindex_it(env$('Q')&"\GLmstr\GLBREC.h"&env$('cno'),env$('Q')&"\GLmstr\GLRecIdx.h"&env$('cno'),"1 24")
02820   goto XIT
02840 GLBREC_DROP: ! 
02860   open #h_glbrec:=1: "Name="&env$('Q')&"\GLmstr\GLBRec.h"&str$(cno)&",SIZE=0,RecL=68,Replace",internal,outin 
02880   goto FINIS
02900 ! ______________________________________________________________________
02920   def fn_current_to_accumlated_trans
02940 ! fnwait - "Transferring Current Transactions to Accumulated Trans..."
02960     open #1: "Name="&env$('Q')&"\GLmstr\ACTRANS.h"&str$(cno),internal,output ioerr L680
02980     goto L690
03000 L680: open #1: "Name="&env$('Q')&"\GLmstr\ACTRANS.h"&str$(cno)&",Size=0,RecL=72",internal,output 
03020 L690: open #2: "Name="&env$('Q')&"\GLmstr\GLTRANS.h"&str$(cno),internal,input 
03040 L700: read #2,using 'Form POS 1,N 3,N 6,N 3,N 6,PD 6.2,2*N 2,C 12,C 30,N 2': mat tr,tr$,td$ eof L740
03060     if tr(1)+tr(2)+tr(3)=0 then goto L700
03080     write #1,using 'Form POS 1,N 3,N 6,N 3,N 6,PD 6.2,2*N 2,C 12,C 30,N 2': mat tr,tr$,td$,actpd
03100     goto L700
03120 L740: close #1: 
03140     close #2: 
03160     let fnindex_it(env$('Q')&"\GLmstr\ACTRANS.h"&str$(cno),env$('Q')&"\GLmstr\ACTRIDX.h"&str$(cno),"1/71/17/13 12/2/2/4")
03180   fnend 
03200 ! ______________________________________________________________________
50830 XIT: let fnxit
50840 ! ______________________________________________________________________
50850 ! <Updateable Region: ERTN>
50860 ERTN: let fnerror(program$,err,line,act$,"NO")
50870   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
50880   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50890   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
50900 ERTN_EXEC_ACT: execute act$ : goto ERTN
50910 ! /region
50920 ! ______________________________________________________________________
