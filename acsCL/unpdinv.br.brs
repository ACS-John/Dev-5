00010 ! Replace S:\acsCL\UnPdInv
00020 ! Unpaid Invoice Listing (Current)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fndat,fntos,fnlbl,fncomboa,fncmdset,fnacs,fnwait
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim dat$*20,cnam$*40,vnam$*30,de$*50,fd$*30,ft(3),aa(2),gl(3),ade$*50
00080   dim cap$*128,io1$(2),wrd1$(2),item1$(2)*15
00090   dim udf$*256
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Unpaid Invoice Listing")
00120   let udf$=env$('temp')&'\'
00130   let cancel=99
00140   let fncno(cno,cnam$) !:
        let fndat (dat$)
00150   let fntos(sn$="unpdinv") !:
        let respc=0
00160   let fnlbl(1,40,"",1,1)
00170   let fnlbl(1,1,"Order for Printing:",20,1)
00180   let item1$(1)="Payee" !:
        let item1$(2)="Fund Number" !:
        let fncomboa("unpdinv",1,22,mat item1$,tt$) !:
        let resp$(respc+=1)=item1$(1)
00190   let fncmdset(2) !:
        let fnacs(sn$,0,mat resp$,ck)
00200   if ck=5 then goto XIT else !:
          if resp$(1)=item1$(1) then let fund=1 else !:
            let fund=2
00210 ! FNWAIT
00220   open #paytrans=4: "Name="&env$('Q')&"\CLmstr\PayTrans.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.H"&str$(cno)&",Shr",internal,input,keyed 
00230   open #unpdaloc=8: "Name="&env$('Q')&"\CLmstr\UnPdAloc.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\Uaidx2.h"&str$(cno)&",Shr",internal,input,keyed 
00240   open #clwork=10: "Name="&env$('Q')&"\CLmstr\CLWORK"&wsid$&".h"&str$(cno)&", Size=0, RecL=97, Replace",internal,outin 
00250 READ_PAYTRANS: ! 
00260   read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,pos 107,n 8': vn$,iv$,ivd,dd,po$,de$,upa,cde,ddate eof L350
00270   let ivnum+=1 ! UNIQUE Number FOR EACH INVOICE
00280   restore #unpdaloc,key>=vn$&iv$: nokey READ_PAYTRANS
00290 READ_UNPDALOC: ! 
00300   read #unpdaloc,using 'Form pos 1,c 8,c 12,N 3,N 6,N 3,PD 5.2,C 30': unvn$,univ$,mat gl,amt,ade$ eof READ_PAYTRANS
00310   if vn$<>unun$ and univ$<>iv$ then goto READ_PAYTRANS
00320   if fund=2 then let de$=ade$(1:18)
00330   write #clwork,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,N 3,N 6,N 3,N 4,n 8': vn$,iv$,ivd,dd,po$,de$(1:18),amt,cde,mat gl,ivnum,ddate
00340   goto READ_UNPDALOC
00350 L350: close #paytrans: : close #unpdaloc: : close #clwork: 
00360   let upa=0 ! sort ok, sorts a work file
00370   open #9: "Name="&udf$&"CONTROL,SIZE=0,RecL=128,Replace",internal,output 
00380   write #9,using 'Form POS 1,C 128': "FILE CLWORK"&wsid$&".H"&str$(cno)&","&env$('Q')&"\CLmstr,,"&udf$&"ADDR,,,,,A,N"
00390   if fund=2 then !:
          write #9,using 'Form POS 1,C 128': "MASK 74,3,N,A,1,20,C,A,86,4,N,A"
00400   if fund<>2 then !:
          write #9,using 'Form POS 1,C 128': "MASK 1,20,C,A,86,4,N,A"
00410   close #9: 
00420   execute "FREE "&udf$&"ADDR" ioerr L430
00430 L430: execute "SORT "&udf$&"CONTROL"
00440   open #9: "Name="&udf$&"ADDR",internal,input 
00450   open #paymstr=13: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&str$(cno)&",Shr",internal,input,keyed 
00460   open #clwork=10: "Name="&env$('Q')&"\CLmstr\CLWORK"&wsid$&".h"&str$(cno)&",Shr",internal,input,relative 
00470   open #glmstr=5: "Name="&env$('Q')&"\CLmstr\GLmstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\GLIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00480   open #work=6: "Name="&udf$&"WORK,SIZE=0,RecL=22,Replace",internal,output 
00490   close #work: 
00500   execute "FREE INDX."&wsid$ ioerr L510
00510 L510: execute "INDEX "&udf$&"WORK,"&udf$&"INDX,1,12,Replace"
00520   open #work=6: "Name="&udf$&"WORK,KFName="&udf$&"INDX",internal,outin,keyed 
00530   open #fundmstr=7: "Name="&env$('Q')&"\CLmstr\FundMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\FundIdx1.h"&str$(cno)&",Shr",internal,input,keyed 
00540   let fnopenprn
00550   let vn$="": let iv$=""
00560 L560: read #9,using 'FORM POS 1,PD 3': r4 eof END1
00570   if r4=0 then goto L560
00580   read #clwork,using 'Form Pos 86,N 4',rec=r4: ivnum
00590   if ivnum=hivnum or hivnum=0 then goto L600 else goto L670
00600 L600: read #clwork,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,N 3,N 6,N 3,N 4,n 8',rec=r4: vn$,iv$,ivd,dd,po$,ade$,amt,cde,mat gl,ivnum,ddate
00610   let upa+=amt
00620   let hivnum=ivnum
00630   let gl$=cnvrt$("PIC(ZZ#)",gl(1))&cnvrt$("PIC(ZZZZZZ)",gl(2))&cnvrt$("PIC(ZZ#)",gl(3))
00640   if f1=0 then gosub L990
00650   gosub L890 ! ACCUMULATE G/L TOTALS
00660   goto L560
00670 L670: if vn$=hvn$ then goto L730
00680   if vc<2 then goto L710
00690   print #255: tab(97);"__________  __________  __________"
00700   print #255,using 'FORM POS 77,C 18,3*N 12.2': "Vendor Total",v1,v2,v3
00710 L710: let vc=v1=v2=v3=0
00720   print #255: pageoflow NEWPGE
00730 L730: let vc+=1
00740   let hvn$=vn$
00750   if fund<>2 then goto L770
00760   if fund$><gl$(1:3) then gosub TOTALS : print #255: newpage : gosub L990
00770 L770: if cde=1 then let p1=97: let v1=v1+upa: let t1=t1+upa : let ft(1)=ft(1)+upa else let p1=109: let v2=v2+upa : let t2=t2+upa : let ft(2)=ft(2)+upa
00780   let v3=v3+upa
00790   let t3=t3+upa : let ft(3)=ft(3)+upa
00800   let vnam$=""
00810   read #paymstr,using L820,key=vn$,release: vnam$ nokey L830
00820 L820: form pos 9,c 30
00830 L830: let discdate$=cnvrt$("pic(########)",ddate)
00840   let ddate=val(discdate$(5:8))*100+val(discdate$(3:4))
00850   print #255,using 'FORM POS 1,C 10,C 22,C 12,3*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,POS P1,N 10.2,POS 119,N 12.2': vn$,vnam$(1:20),iv$,ivd,dd,ddate,ade$(1:18),upa,upa pageoflow NEWPGE
00860   let upa=0
00870   if endcode=1 then goto L1180
00880   goto L600
00890 L890: if gl$(3:3)=" " then let gl$(3:3)="0"
00900   if gl$(12:12)=" " then let gl$(12:12)="0"
00910   read #work,using 'FORM POS 13,N 10.2',key=gl$: gla nokey L950
00920   let gla+=amt
00930   rewrite #work,using 'FORM POS 13,N 10.2': gla
00940   goto L960
00950 L950: write #work,using 'FORM POS 1,C 12,N 10.2': gl$,amt
00960 L960: return 
00970 ! ______________________________________________________________________
00980 NEWPGE: print #255: newpage: gosub HDR : continue 
00990 L990: let fd$=""
01000   let fund$=gl$(1:3)
01010   read #fundmstr,using 'FORM POS 4,C 25',key=fund$: fd$ nokey HDR
01020 HDR: let f1=1
01030   print #255,using 'FORM POS 1,C 8,Cc 86': date$,cnam$
01040   print #255,using 'FORM POS 1,C 8,cc 86': time$,"Unpaid Invoice Listing"
01050   print #255,using 'FORM POS 1,C 4,N 4,Cc 86': "Page",pg+=1,dat$
01060   if fund<>2 then let fd$=""
01070   print #255,using 'FORM POS 1,Cc 86': fd$ ! ; pr #255: ""
01080   print #255: "                                              Invoice     Due     Discount                       Pay Now     Pay Later      Total"
01090   print #255: "Payee  #  Payee Name            Invoice Numb    Date      Date      Date    Description           Amount      Amount         Due"
01100   print #255: "________  ____________________  ____________  ________  ________  ________  __________________  __________  __________  __________"
01110   let f1=1
01120   return 
01130 ! ______________________________________________________________________
01140 END1: ! 
01150   if r4=0 then goto XIT
01160   let endcode=1
01170   goto L670
01180 L1180: if vc<2 then goto L1210
01190   print #255: tab(97);"__________  __________  __________"
01200   print #255,using 'FORM POS 77,C 18,3*N 12.2': "Vendor Total",v1,v2,v3
01210 L1210: gosub TOTALS
01220   print #255: tab(97);"__________  __________  __________"
01230   print #255,using 'FORM POS 77,C 18,3*N 12.2': "Final Total",t1,t2,t3
01240   print #255: tab(97);"=================================="
01250   restore #work,key>="            ": nokey EO_WORK
01260 READ_WORK: ! 
01270   read #work,using 'FORM POS 1,C 12,N 10.2': gl$,gla eof EO_WORK
01280   if hf$="" or hf$=gl$(1:3) then goto L1300
01290   gosub TOTF1
01300 L1300: let hf$=gl$(1:3)
01310   let de$=""
01320   read #glmstr,using 'FORM POS 13,C 50',key=gl$: de$ nokey L1330
01330 L1330: print #255,using 'FORM POS 12,C 14,C 52,N 10.2': gl$,de$,gla pageoflow NEWPGE
01340   let tf1=tf1+gla
01350   goto READ_WORK
01360 TOTF1: print #255: tab(78);"__________"
01370   if val(hf$)>0 then let fd$="Total for Fund #: "&ltrm$(hf$) else let fd$="TOTAL"
01380   print #255,using 'FORM POS 12,C 14,C 52,N 10.2': "",fd$,tf1
01390   print #255: pageoflow NEWPGE
01400   let tf1=0
01410   return 
01420 ! ______________________________________________________________________
01430 EO_WORK: gosub TOTF1
01440   let fncloseprn
01450   close #work,free: ioerr XIT
01460 XIT: let fnxit
01470 ! ______________________________________________________________________
01480 TOTALS: ! 
01490   if fund=2 then !:
          print #255: tab(97);"__________  __________  __________" !:
          print #255,using 'FORM POS 77,C 18,3*N 12.2': "Fund   Total",mat ft
01500   mat ft=(0)
01510   return 
01520 ! ______________________________________________________________________
01530 ! <Updateable Region: ERTN>
01540 ERTN: let fnerror(program$,err,line,act$,"xit")
01550   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01560   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01570   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01580 ERTN_EXEC_ACT: execute act$ : goto ERTN
01590 ! /region
01600 ! ______________________________________________________________________
