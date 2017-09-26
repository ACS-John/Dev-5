00010 ! Replace S:\acsGL\glCkRec
00020 ! Bank Reconciliation Report  (prints the actual bank reconciliation form the general ledger system
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fndat,fndate_mmddyy_to_ccyymmdd,fnwait,fntos,fnlbl,fnacs,fncmdset,fnqgl,fnagl$,fntxt,fnrgl$,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim flo2$(3),io1$(6),sc$(9),sd$(7),se$(7)*30,sf$(4),cnam$*40,dat$*20
00080   dim gl$*12,c$*12,p$*30,s$*2,a(3),dcode$*24,glc$*24,holdgc$*24,currgl$*12
00090   dim cap$*128,resp$(10)*80
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Bank Reconciliation Report")
00120   let fnconsole(off=0)
00130   let fncno(cno,cnam$)
00140   let fndat(dat$)
00150   let right=1 : let center=2 : let pointtwo$="32"
00160   let fntos(sn$="glCkRec") !:
        let lc=0 : let mylen=40 : let mypos=mylen+2
00170   let fnlbl(lc+=1,1,"General Ledger Bank Account Number:",mylen,right)
00180   let fnqgl(lc,mypos) !:
        let resp$(1)=fnrgl$(resp$(1))
00190 ! Let IO1$(1)="2,43,Nz 3,UT,N" : Let IO1$(2)="2,47,Nz 6,UT,N" !:
        ! Let IO1$(3)="2,54,Nz 3,UT,N" : Let IO1$(4)="3,43,c 20,UT,N" !:
        ! Let IO1$(5)="4,43,Nz 12.2,UT,N" : Let IO1$(6)="5,43,Nz 6,UT,N"
00200 ! Print Fields "15,29,C 10,B,1": "Print (F1)" !:
        ! Print Fields "15,41,C 09,B,5": "Exit (F5)"
00210   let fnlbl(lc+=1,1,"Report Heading Date:",mylen,right)
00220   let fntxt(lc,mypos,20) !:
        let resp$(2)=dat$
00230   let fnlbl(lc+=1,1,"Balance per Bank Statement:",mylen,right)
00240   let fntxt(lc,mypos,12,0,0,'PointTwo') !:
        let resp$(3)=str$(bankbal)
00250   let fnlbl(lc+=1,1,"Last Check Date for Reconciliation:",mylen,right)
00260   let fntxt(lc,mypos,0,0,0,'CCYYMMDD') !:
        let resp$(4)=str$(lcd)
00270   let fncmdset(3)
00280 ! Rinput #101,Fields MAT IO1$: GL1,GL2,GL3,DAT$,BANKBAL,LCD !:
        let fnacs(sn$,0,mat resp$,ckey)
00290   if ckey=5 then goto XIT
00300   let resp$(1)=fnagl$(resp$(1)) !:
        let gl1=val(resp$(1)(1:3)) !:
        let gl2=val(resp$(1)(4:9)) !:
        let gl3=val(resp$(1)(10:12)) !:
        let dat$=resp$(2) !:
        let bankbal=val(resp$(3)) !:
        let lcd=val(resp$(4))
00310   let currgl$=resp$(1)
00320   let fnwait(0,cap$,"Printing: Please wait...",1) !:
        on fkey 5 goto DONE
00330   open #glbrec=1: "Name="&env$('Q')&"\GLmstr\glbrec.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\glrecidx.h"&str$(cno)&",Shr",internal,input,keyed ioerr XIT
00340   read #glbrec,using 'Form POS 1,C 12,C 12,C 30,C 2,N 6,PD 5.2,N 1',key>=currgl$&"            ": gl$,c$,p$,s$,mat a nokey DONE
00350   let fnopenprn
00360   if currgl$<>gl$ then goto DONE
00370   gosub HDR !:
        print #255,using 'Form POS 20,C 35,N 16.2': "* Balance Per Bank Statement *",bankbal !:
        print #255: ""
00380   goto RD_NXT
00390 ! ______________________________________________________________________
00400 READ_GLBREC: ! 
00410   read #glbrec,using 'Form POS 1,C 12,C 12,C 30,C 2,N 6,PD 5.2,N 1': gl$,c$,p$,s$,mat a eof TOTAL
00420   if currgl$<>gl$ then goto TOTAL
00430   if fndate_mmddyy_to_ccyymmdd(a(1))>lcd then goto READ_GLBREC
00440 RD_NXT: ! 
00450   if a(3)<>0 then goto READ_GLBREC
00460   print #255,using 'Form POS 1,C 12,POS 15,C 30,POS 48,PIC(ZZ/ZZ/ZZ),POS 58,N 13.2': c$,p$,a(1),a(2) pageoflow PGOF
00470   let tot+=a(2)
00480   goto READ_GLBREC
00490 ! ______________________________________________________________________
00500 PGOF: print #255: newpage !:
        gosub HDR !:
        continue 
00510 ! ______________________________________________________________________
00520 TOTAL: ! 
00530   print #255,using 'Form POS 59,C 12': "------------" !:
        print #255,using 'Form POS 57,N 14.2': bankbal-tot !:
        print #255,using 'Form POS 59,C 12': "============"
00540   goto DONE
00550 ! ______________________________________________________________________
00560 HDR: ! 
00570   print #255,using 'Form POS 1,C 8,Cc 56': date$('mm/dd/yy'),cnam$
00580   print #255,using 'Form POS 1,C 8,Cc 56': time$,cap$
00590   print #255,using 'Form POS 29,C 3,X 1,C 6,X 1,C 3': currgl$(1:3),currgl$(4:9),currgl$(10:12)
00600   print #255,using 'Form POS 1,Cc 72': dat$
00610   print #255: ""
00620   print #255,using 'Form POS 1,C 12,POS 15,C 5,POS 50,C 4,POS 65,C 6': "Check Number","Payee","Date","Amount"
00630   print #255: ""
00640   return 
00650 ! ______________________________________________________________________
00660 DONE: let fncloseprn
00670   close #glbrec: 
00680 XIT: let fnxit
00690 ! ______________________________________________________________________
00700 ! <Updateable Region: ERTN>
00710 ERTN: let fnerror(program$,err,line,act$,"xit")
00720   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00730   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00740   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00750 ERTN_EXEC_ACT: execute act$ : goto ERTN
00760 ! /region
00770 ! ______________________________________________________________________
