00010 ! Replace S:\acsCL\CpInvLst
00020 ! checkbook Accounts Payable Listing (Any Time)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fnxit,fntop,fntos,fnlbl,fntxt,fncomboa,fncmdset,fnacs,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,vnam$*30,de$*30,cap$*128,sq1$*1,item1$(2)*15
00080 ! ______________________________________________________________________
00090   fncno(cno,cnam$)
00100   fntop(program$,cap$="Payables Listing (Any Time)")
00120   cancel=99
00130   fntos("cpinvlst") !:
        respc=0
00140   fnlbl(1,1,"Period Ending Date:",23,1)
00150   fntxt(1,25,10,0,1,"3") !:
        resp$(respc+=1)=str$(ped)
00160   fnlbl(2,1,"Print Order:",23,1)
00170   item1$(1)="General Ledger" !:
        item1$(2)="Vendor"
00180   fncomboa("ubnamlst-srt",2,25,mat item1$,tt$) !:
        resp$(respc+=1)=item1$(1)
00190   fnlbl(3,1,"Fund Number to Print:",23,1)
00200   fntxt(3,25,3,0,1,"30") !:
        resp$(respc+=1)=str$(fund)
00210   fncmdset(2): fnacs(sn$,0,mat resp$,ckey)
00220   if ckey=5 then goto XIT
00230   ped=val(resp$(1))
00240   sq1$=resp$(2)(1:1)
00250   let fund=val(resp$(3))
00260 ! ___________________________
00270 ! ___________________________
00280   open #paymstr=13: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00290   open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00300   open #tralloc=3: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&str$(cno)&",Version=2,KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.h"&str$(cno)&",Shr",internal,input,keyed 
00310   open #paytrans=4: "Name="&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.h"&str$(cno)&",Shr",internal,input,keyed 
00320   open #unpdaloc=6: "Name="&env$('Q')&"\CLmstr\UnPdAloc.h"&str$(cno)&",Version=2,KFName="&env$('Q')&"\CLmstr\UAIdx2.h"&str$(cno)&",Shr",internal,input,keyed 
00330   open #work=5: "Name="&env$('Temp')&"\Work,Size=0,RecL=66,Replace",internal,output 
00340 READ_PAYTRANS: ! 
00350   read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6',release: vn$,iv$,ivd,dd eof END1
00360   if fndate_mmddyy_to_ccyymmdd(ivd)>ped then goto READ_PAYTRANS
00370   de$=""
00380   read #paymstr,using 'Form POS 9,C 30',key=vn$: de$ nokey RESTORE_UNPDALOC
00390 RESTORE_UNPDALOC: ! 
00400   restore #unpdaloc,key>=vn$&iv$: 
00410 READ_UNPDALOC: ! 
00420   read #unpdaloc,using 'Form pos 1,c 8,c 12,C 12,PD 5.2': alocvn$,alociv$,gl$,upa norec READ_PAYTRANS eof READ_PAYTRANS
00430   if alocvn$<>vn$ or alociv$<>iv$ then goto READ_PAYTRANS
00440   if fund>0 and fund<>val(gl$(1:3)) then goto READ_UNPDALOC
00450   if upa=0 then goto L470 ! don't pr 0 breakdowns
00460   write #work,using 'Form POS 1,C 12,N 6,C 8,C 30,N 10.2': gl$,ivd,vn$,ltrm$(rtrm$(iv$))&" "&de$(1:17),upa
00470 L470: goto READ_UNPDALOC
00480 ! ______________________________________________________________________
00490 END1: close #paytrans: 
00500 READ_TRMSTR: ! 
00510   read #trmstr,using 'Form pos 1,n 2,n 1,c 8,N 6,POS 28,C 8,POS 36,C 30,POS 78,N 1': bank_code,tcde,tr$(1),pd,vn$,de$,scd eof END2
00520   if scd=4 then goto READ_TRMSTR
00530   if tcde>1 then goto READ_TRMSTR
00540   if ped=>fndate_mmddyy_to_ccyymmdd(pd) then goto READ_TRMSTR
00550   adr=ta(1)
00560 READ_TRALLOC: ! 
00570   key$=cnvrt$("Pic(zz)",bank_code)&str$(tcde)&tr$(1) !:
        restore #tralloc,key=key$: nokey READ_TRMSTR
00580 L580: read #tralloc,using 'Form POS 1,C 11,C 12,PD 5.2,X 30,N 6': newkey$,gl$,amt,ivd eof READ_TRMSTR !:
        if key$<>newkey$ then goto READ_TRMSTR
00590   if ped<fndate_mmddyy_to_ccyymmdd(ivd) or ivd=0 or (fund>0 and fund<>val(gl$(1:3))) then goto EO_LOOP
00600   if amt=0 then goto EO_LOOP ! don't allow 0 amounts to show
00610   write #work,using 'Form POS 1,C 12,N 6,C 8,C 30,N 10.2': gl$,ivd,vn$,de$,amt
00620 EO_LOOP: goto L580
00630 ! ______________________________________________________________________
00640 END2: close #1: 
00650   close #tralloc: 
00660   if lrec(work)=0 then goto XIT
00670   close #work: 
00680   open #tmp=1: "Name="&env$('Temp')&"\Control,Size=0,RecL=128,Replace",internal,output 
00690   write #tmp,using 'Form POS 1,C 128': "! Sorting Accounts Payable Invoice List..."
00700   write #tmp,using 'Form POS 1,C 128': "File "&env$('Temp')&"\Work,,,"&env$('Temp')&"\Addr,,,,,A,N"
00710   if sq1$="G" then !:
          write #tmp,using 'Form POS 1,C 128': "Mask 1,26,C,A"
00720   if sq1$="V" then !:
          write #tmp,using 'Form POS 1,C 128': "Mask 19,8,C,A,13,6,C,A"
00730   close #tmp: 
00740   execute "FREE "&env$('Temp')&"\ADDR" ioerr L750
00750 L750: execute "Sort "&env$('Temp')&"\Control"
00760   open #addr=1: "Name="&env$('Temp')&"\ADDR",internal,input ioerr XIT
00770   open #work=5: "Name="&env$('Temp')&"\WORK",internal,input,relative 
00780   fnopenprn
00790   gosub HDR
00800 READ_ADDR: ! 
00810   read #addr,using 'Form POS 1,PD 3': r5 eof ENDALL
00820   read #work,using 'Form POS 1,C 12,N 6,C 8,C 30,N 10.2',rec=r5: gl$,ivd,vn$,de$,amt norec READ_ADDR
00830   if t1=0 then goto PRINT_LINE
00840   if sq1$="G" then goto L890
00850   if vn$=hvn$ then goto PRINT_LINE
00860   pr #255: "                                  ______________________________  __________"
00870   pr #255,using 'Form POS 35,CR 30,N 12.2': "Vendor #: "&ltrm$(hvn$)&" Total",t1
00880   goto L930
00890 L890: if hgl$=gl$ then goto PRINT_LINE
00900   if t1=0 then goto PRINT_LINE
00910   pr #255: "                                  ______________________________  __________"
00920   pr #255,using 'Form POS 35,CR 30,N 12.2': "GL # "&hgl$&" Total",t1
00930 L930: pr #255: "                                  ______________________________  __________" pageoflow NEWPGE
00940   t1=0
00950   if amt=0 then goto READ_ADDR
00960 PRINT_LINE: ! 
00970   pr #255,using 'Form POS 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,C 10,C 30,N 12.2': gl$,ivd,vn$,de$,amt pageoflow NEWPGE
00980   t1+=amt : t2+=amt : hgl$=gl$ : hvn$=vn$
00990   goto READ_ADDR
01000 ! ______________________________________________________________________
01010 NEWPGE: pr #255: newpage: gosub HDR : continue 
01020 ! ______________________________________________________________________
01030 HDR: ! 
01040   pr #255,using 'Form POS 1,C 8,CC 78': date$,cnam$
01050   pr #255,using 'Form POS 1,C 8,POS 32,C 40': time$,cap$
01060   pr #255,using 'Form POS 1,C 4,N 4,POS 38,C 40': "Page",pg+=1,"as of "&cnvrt$("PIC(zzZZ/ZZ/ZZ)",ped) !:
        pr #255: ""
01070   pr #255: "               Invoice   Vendor                                            "
01080   pr #255: " G/L Number     Date     Number   Description                         Amount  "
01090   pr #255: "____________  ________  ________  ______________________________  __________"
01100   return 
01110 ! ______________________________________________________________________
01120 ENDALL: ! 
01130   pr #255: "                                  ______________________________  __________"
01140   if sq1$="G" then !:
          pr #255,using 'Form POS 35,CR 30,N 12.2': "GL # "&hgl$&" Total",t1
01150   if sq1$="V" then !:
          pr #255,using 'Form POS 35,CR 30,N 12.2': "  Vendor: "&ltrm$(hvn$)&" Total",t1
01160   pr #255: "                                  ______________________________  __________" pageoflow NEWPGE
01170   pr #255,using 'Form POS 35,CR 30,N 12.2': "Final Total",t2
01180   pr #255: "                                  =========================================="
01190   fncloseprn
01200 XIT: fnxit
01210 ! ______________________________________________________________________
01220 ! <Updateable Region: ERTN>
01230 ERTN: fnerror(program$,err,line,act$,"xit")
01240   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01250   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01260   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01270 ERTN_EXEC_ACT: execute act$ : goto ERTN
01280 ! /region
01290 ! ______________________________________________________________________
