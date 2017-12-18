00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fncno,fnerror,fnwait,fnopenprn,fncloseprn,fnacs,fnflexadd1,fnflexinit1,fntos,fncustomer_search,fnlbl,fntxt,fnopt,fnmsgbox,fncomboa,fnbutton,fnpic,fnfra,fnchk,fndat,fncmbact,fncombof,fncmbrt2,fnxit,fncmdset,fncmdkey,fntop,fndate_mmddyy_to_ccyymmdd,fnpause,fngethandle
00040   library 'S:\Core\Library': fnLastBillingDate
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim rate(18,20),usage(18,20),cde(20),d(12),t(18,2),a(4),message$*40
00080   dim usagtot(18,20),ratetot(18,20),customer(18,20),cap$*128
00090   dim fa$(5),sa$(4),fb$(1),fc$(1),sb$(1)*38,fd$(1),z$(4)*11,srvc$*11
00100   dim io2$(38),cnam$*40,code$(4),a(7),d(15),g(10),e$(4)*30,f$(3)*12
00110 ! ______________________________________________________________________
00120   fncno(cno,cnam$)
00130   fnLastBillingDate(bdate)
00140   fntop(program$,cap$="Analyze Charges")
00150 MAIN: ! 
00160   fntos(sn$:="UBAnalyze") !:
        mylen=20 !:
        mypos=mylen+2
00170   fnlbl(1,1,"Billing Date:",mylen,1)
00180   fntxt(1,mypos,8,8,0,"1") !:
        resp$(1)=str$(bdate)
00190   fnlbl(2,1,"Type of Service:",mylen,1)
00200   code$(1)="Water" !:
        code$(2)="Sewer" !:
        code$(3)="Electric" !:
        code$(4)="Gas" !:
        fncomboa("Service",2,mylen+3,mat code$,"",16)
00210   fnlbl(3,1,"Rate Code",mylen,1)
00220   fntxt(3,mypos,3,3,0,"30") !:
        resp$(3)=""
00230   fncmdset(3): fnacs(sn$,0,mat resp$,ck)
00240   if ck=5 then goto XIT
00250   bdate= val(resp$(1))
00260   if resp$(2)="Water" then !:
          srvc=1 : srvc$=resp$(2)
00270   if resp$(2)="Sewer" then !:
          srvc=2 : srvc$=resp$(2)
00280   if resp$(2)="Electric" then !:
          srvc=3 : srvc$=resp$(2)
00290   if resp$(2)="Gas" then !:
          srvc=4 : srvc$=resp$(2)
00300   rcode=val(resp$(3))
00310   fnopenprn
00320   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00330 ! 
00340   on fkey 5 goto DONE
00350   fnopenprn(cp,58,220,process)
00360 ! Read #1,Using 370: MAT A,MAT D,F Eof 400
00370 ! Form POS 143,4*PD 2,POS 217,12*PD 5,POS 296,PD 4
00380 ! If F<>BDATE Then Goto 360
00390 ! If A(SVCE)<>RATCODE Then Goto 360
00400   gosub PRINTIT
00410 DONE: close #1: ioerr L420
00420 L420: fncloseprn
00430 XIT: fnxit
00440 ! ______________________________________________________________________
00450 ERTN: fnerror(program$,err,line,act$,"xit")
00460   if uprc$(act$)<>"PAUSE" then goto L490
00470   execute "list -"&str$(line) !:
        pause  !:
        goto L490
00480   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
00490 L490: execute act$
00500   goto ERTN
00510 ! ______________________________________________________________________
00520 PRINTIT: ! 
00530   gosub HDR
00540 L540: read #1,using L610: z$,mat e$,mat a,mat f$,mat d,mat g eof L810 ! READ MASTER RECORD
00550   if a(srvc)=0 then goto L540 ! no service
00560   usage=0
00570   if srvc=1 then usage=d(3): amount=g(1): meter$=f$(1) ! water
00580   if srvc=2 then usage=d(3): amount=g(2): meter$="" ! sewer
00590   if srvc=3 then usage=d(7): amount=g(3): meter$=f$(2) ! electric
00600   if srvc=4 then usage=d(11): amount=g(4): meter$=f$(3) ! gas
00610 L610: form pos 1,c 10,pos 11,4*c 30,pos 143,7*pd 2,pos 131,c 12,pos 361,c 12,pos 373,c 12,pos 217,15*pd 5,pos 300,10*pd 4.2
00620   if a(srvc)=tc or tc=0 then goto L630 else goto L540
00630 L630: pr #255,using L660: z$,e$(2),e$(1),meter$,usage,amount pageoflow PGOF
00640   totusage=totusage+usage
00650   totamount=totamount+amount
00660 L660: form x 5,c 10,x 5,c 30,x 7,c 30,x 2,c 12,x 2,pic(zzzzzzzzz),x 2,n 12.2,skip 2
00670   goto L540
00680 ! ______________________________________________________________________
00690 PGOF: pr #255: newpage
00700   gosub HDR
00710   continue 
00720 ! ______________________________________________________________________
00730 HDR: ! 
00740   p2=p2+1
00750   pr #255,using "Form POS 1,CC 80": cnam$ !:
        pr #255,using "Form POS 1,CC 80": "Consumption List - "&srvc$ ! "                       pr #255,Using " Form POS 1,CC 80": "Rate Code "&STR$(SRVC) !:
        pr #255,using "Form POS 90,C 5,PIC(ZZZ)": "Page ",p2 !:
        pr #255: ""
00760   if tc<>0 then pr #255,using L770: srvc$&" Code ",tc
00770 L770: form pos 41,c 9,n 2,skip 2
00780   pr #255: tab(7);"Customer #";tab(21);"Name";tab(58);"Meter Address";tab(90);"   Merer #    Consumption  Dollar Amt"
00790   pr #255: tab(7);"__________";tab(21);"________________________________";tab(58);"______________________________  ____________  ___________  __________"
00800   return 
00810 L810: pr #255,using "Form POS 101,C 28": "____________  ____________" !:
        pr #255,using "Form POS 101,N 12,X 2,N 12.2": totusage,totamount !:
        pr #255,using "Form POS 101,C 28": "============  ============"
00820   close #1: 
00830   return 
