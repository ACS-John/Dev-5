00010 ! Replace R:\acsGL\AcGLAcTB
00020 ! Print Accumulated Trial Balance
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnprocess,fngl_number_use_dept,fnpedat$,fntos,fnfra,fnopt,fnlbl,fnqgl,fncmdset,fnacs,fnagl$,fnchk,fntxt,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,d$*50,tr(7),tr$*12,td$*30,n$*12,t$*12,x$*3,cap$*128
00080   dim resp$(20)*50,bp(13)
00090   dim a$(9)*3,cogl$(3)*12,u$*12
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Print Accumulated Trial Balance")
00120   let fnconsole(off=0)
00130   if exists("petro.txt") then let petro_opt=1 else let petro_opt=0
00140   let fncno(cno,cnam$)
00150 ! 
00160   open #20: "Name=Q:\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative 
00170   read #20,using 'Form Pos 152,3*C 12',rec=1: mat cogl$
00180   close #20: 
00190   open #1: "Name=Q:\GLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative 
00200   read #1,using "Form pos 296,N 2",rec=1: lmu
00210   close #1: 
00220   open #20: "Name=Q:\GLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative 
00230   read #20,using 'Form Pos 384,n 2',rec=1: nap
00240   close #20: 
00250   let m2=87
00260   if nap=13 then let m1=171-6 else let m1=171-12 ! 171 was 249
00270   let last=val(cogl$(3)(4:9))
00280   data "C/D","C/R","ADJ","A/P","PR","A/R","S/J","P/J"," "
00290   read mat a$
00300   open #h_glmstr:=1: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",KFName=Q:\GLmstr\GLIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00310   open #2: "Name=Q:\GLmstr\AcTrans.h"&str$(cno)&",KFName=Q:\GLmstr\AcTrIdx.h"&str$(cno)&",Shr",internal,input,keyed 
00320   let fnopenprn
00330   if file$(255)(1:4)<>"PRN:" then let redir=1 else let redir=0
00340   if process=1 then let s1=1 : goto L710
00350   let cnam$=rtrm$(cnam$)
00355 L300: print newpage
00360   let fntos(sn$="Acglactb")
00365   let mylen=52: let mypos=mylen+3 : let right=1
00370   let fnlbl(1,1,'General ledger number for the last "Capital" account:',mylen,right)
00375   let fnqgl(1,mypos,0,2)
00380   let resp$(1)=""
00385   let fnchk(2,mypos,"Print Ending Balance on First Line:",1,0)
00390   let resp$(2)=""
00395   let fnlbl(3,1,"Period Code to print (blank for all):",mylen,right)
00400   let fntxt(3,mypos,2,0,1,"30",0,"You can print any month or the entire year.")
00405   let resp$(3)="" ! STR$(LMU)
00410   let fnfra(5,1,3,73,"Selection Type"," ",0)
00415   let fnopt(1,3,"Print All GL Accounts",0,1)
00420   let resp$(4)="True"
00425   let fnopt(2,3,"Print Selected GL Accounts",0,1)
00430   let resp$(5)="False"
00435   let fnopt(3,3,"Print a Range of Accounts",0,1)
00440   let resp$(6)="False"
00445   let fnfra(10,1,3,73,"Accounts to Print"," ",0)
00450   let mylen=30 : let mypos=mylen+3
00455   let fnlbl(1,1,'First account number to print:',mylen,right,0,2)
00460   let fnqgl(1,mypos,2,2)
00465   let resp$(7)=""
00470   let fnlbl(2,1,'Last account number to print:',mylen,right,0,2)
00475   let fnqgl(2,mypos,2,2)
00480   let resp$(8)=""
00485   let fncmdset(2)
00490   let fnacs(sn$,0,mat resp$,ckey)
00495   if ckey=5 then goto XIT
00500   let cogl$(3)=fnagl$(resp$(1))
00505   if resp$(2)="True" then let petro_opt$="Y" else let petro_opt$="N"
00510   let spc=val(resp$(3)) ! period code to print
00515   if resp$(4)="True" then let s1=1 ! method of selecting
00520   if resp$(5)="True" then let s1=2
00525   if resp$(6)="True" then let s1=3
00530   let n1$=fnagl$(resp$(7))
00535   let n2$=fnagl$(resp$(8))
00540   if petro_opt$="Y" then let petro_opt=1 else let petro_opt=0
00545   if spc>1 then let m1=spc*6+81
00550   let m2=spc*6+87
00555   if spc><1 then goto L640
00560   if nap=13 then let m1=171-6 else let m1=171-12 ! 171 was 249
00640 L640: if s1=3 and n2$<n1$ then goto MSGBOX1
00650 ! Read #h_Q:\GLmstr,Using 880,Key=N1$: N$,D$,BB,CB Nokey 670
00660   if f1=1 then goto L920
00670   gosub HDR
00680   let f1=1
00690   goto L720
00700 ! ______________________________________________________________________
00710 L710: gosub HDR
00720 L720: if fngl_number_use_dept=0 or fnprocess=1 then goto L840
00730 L730: let fntos(sn$="Acglactb2")
00740   let mylen=38: let mypos=mylen+3 : let right=1
00750   let fnlbl(1,1,'Fund number to print (blank for all):',mylen,right,0,0)
00760   let fntxt(1,mypos,2,0,1,"30",0,"Enter a fund number if you wish to print only one fund.")
00770   let resp$(1)=""
00780   let fncmdset(2)
00790   let fnacs(sn$,0,mat resp$,ckey)
00800   if ckey=5 then goto XIT
00810   let costcent=val(resp$(1))
00820   let n$=cnvrt$("N 3",costcent)&"         "
00830   restore #h_glmstr,key>=n$: nokey L730
00840   on pageoflow goto PGOF
00850   on fkey 5 goto TOTALS
00855 L840: ! If fngl_number_use_dept=0 OR FNPROCESS=1 Then Goto READ_Q:\GLmstr Else Goto 910
00860 READ_GLMSTR: ! 
00865   if s1=2 then gosub SELECT_ACCOUNT : goto L890
00870 L870: ! 
00880   read #h_glmstr,using F_GLMSTR: n$,d$,bb,cb,mat bp eof TOTALS
00890   let bb=bp(nap)
00900 F_GLMSTR: form pos 1,c 12,c 50,pos m1,pd 6.2,pos m2,pd 6.2,pos 171,13*pd 6.2
00905 L890: if s1=3 and n$<n1$ then goto L870
00910   if s1=3 and n$>n2$ then goto TOTALS
00915   if costcent><0 and val(n$(1:3))><costcent then goto TOTALS
00920 L920: let dno=val(n$(1:3))
00930   let ano=val(n$(4:9))
00940   let sno=val(n$(10:12))
00950   if spc=0 and ano>val(cogl$(3)(4:9)) then let bb=0
00960   if spc><1 then goto L960
00970   if ano>val(cogl$(3)(4:9)) then let bb=0
00975 L960: if petro_opt=1 then 
00980     print #255,using L1380: dno,ano,sno,d$,bb,cb
00985   else 
00990     print #255,using L1390: dno,ano,sno,d$,bb
00995   end if 
01000   restore #2,key>=n$&cnvrt$("N 2",spc)&"      ": nokey END_OF_TRANS
01005   let t9=0
01007 L990: gosub READ_TR
01009   if t9=9 then goto END_OF_TRANS
01011   gosub PRINT_A_TRANS
01013   goto L990
01015 ! ______________________________________________________________________
01017 END_OF_TRANS: ! 
01019   gosub PRINT_CB_OR_SUMTR
01021   goto READ_GLMSTR
01023 TOTALS: ! EOF ON MASTER FILE
01025   print #255: ""
01027   print #255,using L1100: "Trial Balance Proof Totals",begbal,trtotal,curbal
01100 L1100: form pos 1,cr 78,pos 80,pic(zz,zzz,zzz.## cr),pic(z,zzz,zzz.## cr),pic(z,zzz,zzz.## cr)
01110   close #h_glmstr: 
01120   close #2: 
01130   let fncloseprn
01140   goto XIT
01150 ! ______________________________________________________________________
01160 XIT: let fnxit
01170 ! ______________________________________________________________________
01180 PGOF: ! 
01190   print #255: newpage
01200   gosub HDR
01210   continue 
01220 ! ______________________________________________________________________
01230 NWPG_AND_HDR: ! 
01240   print #255: newpage
01250   goto HDR
01260 ! ______________________________________________________________________
01270 HDR: ! 
01280   print #255,using L1270: cnam$,date$('mm/dd/yy')
01290   print #255,using L1270: 'Accumulated Trial Balance', time$
01300   print #255,using L1270: fnpedat$,"Page "&str$(p1+=1)
01305 L1270: form pos 21,cc 80,cr 21
01310   print #255: ""
01315   print #255: "      Account";
01320   print #255: tab(70);"Reference";tab(84);"Beginning";tab(99);"Current";
01325   print #255: tab(115);"Ending"
01330   print #255,using L1330: "Number","Account Name/Transaction Description","Date  Source","Number","Balance","Activity","Balance"
01332 L1330: form pos 6,c 6,pos 17,c 36,pos 54,c 13,pos 71,c 6,pos 85,c 7,pos 99,c 8,pos 115,c 7
01334   print #255,using L1350: "__________","____________________________________","____","______","___________","_________","__________","_________"
01350 L1350: form pos 4,c 10,pos 17,c 36,pos 54,c 4,pos 60,c 6,pos 69,c 11,pos 84,c 9,pos 98,c 10,pos 114,c 10
01360   return 
01370 ! ______________________________________________________________________
01380 L1380: form pos 1,pic(zzz),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(--,---,--z.## cr),pos 110,pic(zz,zzz,zzz.## cr)
01390 L1390: form pos 1,pic(zzz),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(--,---,--z.## cr)
01400 ! ______________________________________________________________________
01410 READ_TR: ! 
01420   read #2,using L1430: t$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pcde eof T9_9
01430 L1430: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,n 2
01440   if t$><n$ then goto T9_9
01450   if spc=0 then goto L1470
01460   if spc><pcde then goto T9_9
01470 L1470: if tr(5)=0 then goto READ_TR
01480   if tr(6)><0 then goto L1490 else let tr(6)=9
01490 L1490: return 
01500 ! ______________________________________________________________________
01510 T9_9: ! 
01520   let t9=9
01530   return 
01540 ! ______________________________________________________________________
01550 PRINT_A_TRANS: ! 
01560   let x$=a$(tr(6))
01570   if val(cogl$(1)(4:9))=0 or val(cogl$(2)(4:9))=0 then goto PRINT_TRANS
01580   if t$>=cogl$(1) and t$<=cogl$(2) then goto L1760
01590   if tr$="999999999999" then let tr$=" "
01600 PRINT_TRANS: ! 
01610 ! 
01620   print #255,using L1610: td$,tr(4),x$,lpad$(rtrm$(tr$),12),tr(5)
01625 L1610: form pos 21,c 30,pos 52,pic(zz/zz/zz),pos 62,c 3,pos 67,c 12,pos 95,pic(zz,zzz,zzz.## cr)
01630   let trtotal=trtotal+tr(5)
01635   let u$=t$
01637 L1630: return 
01639 ! ______________________________________________________________________
01641 PRINT_CB_OR_SUMTR: ! 
01643   if u0=0 then goto PRINT_EDING_BAL
01645   if u$<cogl$(1) or u$>cogl$(2) then goto L1700
01647   print #255,using L1690: "Summary Transaction",u0
01690 L1690: form pos 21,c 30,pos 95,pic(zz,zzz,zz#.## cr)
01700 L1700: let u0=0
01710 PRINT_EDING_BAL: ! 
01720   if petro_opt<>1 then 
01730     print #255,using L1720: cb
01740   end if 
01745 L1720: form pos 110,pic(zz,zzz,zzz.## cr)
01750   let curbal=curbal+cb
01755   let begbal=begbal+bb
01760   return 
01765 ! ______________________________________________________________________
01767 L1760: if tr(5)>0 then goto PRINT_TRANS
01769   let u0=u0+tr(5)
01771   let trtotal=trtotal+tr(5)
01773   let u$=t$
01775   goto L1630
01777 ! ______________________________________________________________________
01779 ! <Updateable Region: ERTN>
01781 ERTN: let fnerror(cap$,err,line,act$,"xit")
01783   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01785   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01787   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01789 ERTN_EXEC_ACT: execute act$ : goto ERTN
01791 ! /region
01793 ! ______________________________________________________________________
01795 MSGBOX1: ! 
01797   goto L300
01799 BAD_ACCOUNT: ! 
01801 MSGBOX2: ! invalid account #
01803   goto L300
01805 SELECT_ACCOUNT: ! 
01807   let fntos(sn$="Acglactb3")
01809   let mylen=38: let mypos=mylen+3 : let right=1
01811   let fnlbl(1,1,'General ledger # to print:',mylen,right,0,0)
01813   let fnqgl(1,mypos,0,2)
01815   let resp$(1)=""
01817   let fncmdset(2)
01819   let fnacs(sn$,0,mat resp$,ckey)
01821   if ckey=5 then goto TOTALS
01823   let n$=fnagl$(resp$(1))
01825   read #h_glmstr,using F_GLMSTR,key=n$: n$,d$,bb,cb,mat bp nokey SELECT_ACCOUNT
01827   let bb=bp(nap)
01829   return 
