00010 ! Replace S:\acsUB\ubCass2
00020 ! -- Place Certified File Back on PC
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fnxit,fnwait,fnlbl,fntos,fncomboa,fnacs,fncmdset,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim nam$*30,sta$*30,city$*23,csz$*30,opt1$(4),cap$*128,txt$*100
00071   dim a$*5,b$*4,c$*3,bc$*12,cr$*4,d$(2)
00080 ! ______________________________________________________________________
00090   let fncno(cno) !:
        ! 
00100   let fntop("S:\acsUB\ubCass2",cap$="Place Certified File Back on PC")
00110 ! ______________________________________________________________________
00120   let sn$="ubCass2" !:
        let fntos(sn$) !:
        let respc = 0
00130   let fnlbl(1,1,"Path to Returned Postal Diskette:",33,1)
00140   let opt1$(1)="A:\" !:
        let opt1$(2)="C:\" !:
        let opt1$(3)="E:\" !:
        let opt1$(4)="F:\" !:
        let fncomboa("AB",1,35,mat opt1$) !:
        let resp$(respc+=1)=opt1$(1)
00150   let fnlbl(3,1,"This program prints:")
00160   let fnlbl(4,1,"Listing of Customer Addresses that could not be certified",58,2)
00170   let fncmdset(2)
00180 L180: let fnacs(sn$,0,mat resp$,ckey)
00190   if ckey=5 then goto XIT
00200   let dv$=resp$(1)
00210 ! ______________________________________________________________________
00220 ! Open #1: "Name="&DV$&"Cass1.Dat,RecL=223",External,Input Ioerr 180
00221   open #1: "Name="&dv$&"Cass1.Dat,RecL=113",external,input ioerr L180
00230   open #2: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",RecL=112,Replace",internal,output 
00240   open #3: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00250   open #4: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",Shr",internal,outin,relative 
00260   let fnopenprn(cp,0,0,process)
00270   print #255: "\qc {\b "&cap$ !:
        print #255: "Listing of Customer Addresses that could not be certified" !:
        print #255: date$("mm/dd/ccyy")&"}" !:
        print #255: "\ql "
00280 READ_A: ! 
00290 ! Read #1,Using 300: Z$,NAM$,STA$,CITY$,STATE$,A$,B$,C$,CR$ Eof END1
00291   read #1,using L301: z$,nam$,sta$,city$,state$,a$,b$,d$,cr$ eof END1
00292 ! Let BC$=A$&B$&C$  when return batch total
00293   let bc$=a$&b$&d$ ! when have to generate barcode
00300   form pos 1,c 10,pos 12,c 30,pos 41,c 30,pos 106,c 23,pos 151,c 2,pos 166,c 5,pos 172,c 4,pos 192,c 3,pos 202,c 4
00301 L301: form pos 1,c 10,pos 12,c 30,pos 41,c 30,pos 71,c 23,pos 94,c 2,pos 96,c 5,c 4,c 2,pos 107,c 4
00310   if rtrm$(bc$(6:9))="" then !:
          print #255,using L320: z$,nam$,sta$,city$,state$,bc$ : goto READ_A
00320 L320: form pos 1,c 12,2*c 32,c 25,c 4,c 12
00330 ! Gosub CREATE_CHECK_DIGIT  ! already done by melissa read bc$ as c 12 instead of c 11
00331   gosub CREATE_CHECK_DIGIT ! melissa returned only 11 digits
00340   write #2,using L350: z$,nam$,sta$,city$,state$,bc$,cr$
00350 L350: form pos 1,c 10,2*c 30,c 23,c 2,c 12,c 4
00360   gosub L580
00370   goto READ_A
00380 ! ______________________________________________________________________
00390 END1: ! 
00400   close #1: 
00410   close #2: 
00420   execute "Index "&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&","&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",1,10,Replace,DupKeys -n"
00430   let fncloseprn
00440 XIT: let fnxit
00450 ! ______________________________________________________________________
00460 CREATE_CHECK_DIGIT: ! 
00470   let bc$=rtrm$(bc$)
00480   if bc$="" then goto L560
00490   let c1=0
00500   for j=1 to len(bc$)
00510     let c1=c1+val(bc$(j:j)) conv L520
00520 L520: next j
00530   let c1$=str$(c1) !:
        let l1=len(c1$) !:
        let c2=val(c1$(l1:l1))
00540   if c2=0 then let cd$="0" else let cd$=str$(10-c2)
00550   let bc$=bc$&cd$
00560 L560: return 
00570 ! ______________________________________________________________________
00580 L580: let csz$=rtrm$(city$)&", "&state$&" "&bc$(1:5)
00585   goto L640 ! don't update any addresses
00590   read #3,using "Form POS 385,PD 3",key=z$: aba nokey L640
00600   if aba=0 then goto L630
00610   rewrite #4,using "Form POS 41,2*C 30",rec=aba: sta$,csz$ norec L630
00620   goto L640
00630 L630: rewrite #3,using "Form POS 71,2*C 30",key=z$: sta$,csz$
00640 L640: return 
00650 ! ______________________________________________________________________
00660 ! <Updateable Region: ERTN>
00670 ERTN: let fnerror(program$,err,line,act$,"xit")
00680   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00690   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00700   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00710 ERTN_EXEC_ACT: execute act$ : goto ERTN
00720 ! /region
