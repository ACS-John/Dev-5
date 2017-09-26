00010 ! Replace S:\acsCL\AgedAP
00020 ! Print Aged AP Listing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnxit,fntop,fntos,fnlbl,fntxt,fncmdset,fnacs, fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,vnam$*30,de$*50,fd$*30,ft(3)
00080   dim io1$(7)*30,bk(3,2),t1(5),t2(5),cap$*128
00090   dim mo(13)
00100 ! ______________________________________________________________________
00110   let fncno(cno,cnam$)
00120   let fntop(program$,cap$="Accounts Payable Listing (Aged)")
00130   let cancel=99
00140   let mo(01)=000 : let mo(02)=031 : let mo(03)=059 : let mo(04)=090 !:
        let mo(05)=120 : let mo(06)=151 : let mo(07)=181 : let mo(08)=212 !:
        let mo(09)=243 : let mo(10)=273 : let mo(11)=304 : let mo(12)=334 !:
        let mo(13)=365
00150 ! ______________________________________________________________________
00160   def fnjd(x)
00170     let jd0=mo(int(x*.0001))+(int(x*.01)-int(x*.0001)*100)+int(fndate_mmddyy_to_ccyymmdd(x)*.0001)*365+int(int(fndate_mmddyy_to_ccyymmdd(x)*.0001)/4)
00180     if int(fndate_mmddyy_to_ccyymmdd(x*.0001))/4=int(int(fndate_mmddyy_to_ccyymmdd(x*.0001))/4) and int(x*.0001)<3 then let jd0=jd0-1
00190     let fnjd=jd0
00200   fnend 
00210 ! ______________________________________________________________________
00220   let fntos(sn$="agedap") !:
        let respc=0
00230   let fnlbl(1,38,"",1,1)
00240   let fnlbl(1,1,"Aging Date:",23,1)
00250   let fntxt(1,25,10,0,1,"1001") !:
        let resp$(respc+=1)=str$(d1)
00260   let fnlbl(3,1,"Aging Break 1:",23,1)
00270   let fntxt(3,25,3,0,1,"30",0,"Aging break 1 is the maximum age of an invoice (in days) to be grouped in the first category") !:
        let resp$(respc+=1)="30"
00280   let fnlbl(4,1,"Aging Break 2:",23,1)
00290   let fntxt(4,25,3,0,1,"30") !:
        let resp$(respc+=1)="60"
00300   let fnlbl(5,1,"Aging Break 3:",23,1)
00310   let fntxt(5,25,3,0,1,"30") !:
        let resp$(respc+=1)="90"
00320   let fncmdset(2): let fnacs(sn$,0,mat resp$,ckey)
00330   if ckey=5 then goto XIT
00340   let d1=val(resp$(1))
00350   let bk(1,2)=val(resp$(2))
00360   let bk(2,2)=val(resp$(3))
00370   let bk(3,2)=val(resp$(4))
00380   let bk(1,1)=0
00390   if bk(2,2)>0 then let bk(2,1)=bk(1,2)+1
00400   if bk(3,2)>0 then let bk(3,1)=bk(2,2)+1
00410   let fnopenprn
00420   open #paytrans=4: "Name="&env$('Q')&"\CLmstr\PayTrans.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00430   open #paymstr=2: "Name="&env$('Q')&"\CLmstr\PayMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.H"&str$(cno)&",Shr",internal,input,keyed 
00440   gosub HDR
00450 READ_PAYTRANS: ! 
00460   read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1': vn$,iv$,ivd,dd,po$,de$,upa,cde eof END1
00470   if upa=0 then goto READ_PAYTRANS
00480   if hvn$<>"" and hvn$><vn$ then gosub VNTOT
00490   let hvn$=vn$
00500   gosub AGE2
00510   print #255,using 'Form POS 1,C 10,C 12,2*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,POS P1,N 12.2,POS 111,N 12.2': vn$,iv$,ivd,dd,de$,upa,upa pageoflow NEWPGE
00520   goto READ_PAYTRANS
00530 ! ______________________________________________________________________
00540 NEWPGE: print #255: newpage: gosub HDR : continue 
00550 ! ______________________________________________________________________
00560 HDR: let f1=1
00570   print #255,using 'Form POS 1,C 8,Cc 86': date$,cnam$
00580   print #255,using 'Form POS 1,C 8,Cc 86': time$,cap$
00590   print #255,using 'Form POS 1,C 4,N 4,POS 9,Cc 86': "Page",pg+=1,"As of "&cnvrt$("pic(zz/zz/zz)",d1) !:
        print #255: ""
00600   print #255,using 'Form POS 1,C 62,N 8,"-",N 3,N 8,"-",N 3,N 8,"-",N 3,2*C 12': "Payee  #  Invoice Numb    Date      Date    Description       ",mat bk,"    Over "&str$(bk(3,2)),"     Total"
00610   gosub PRINT_
00620   return 
00630 ! ______________________________________________________________________
00640 PRINT_: ! 
00650   print #255: "________  ____________  ________  ________  __________________  __________  __________  __________  __________  __________" pageoflow NEWPGE
00660   return 
00670 PRINT2_: ! 
00680   print #255: "                                                                __________  __________  __________  __________  __________" pageoflow NEWPGE
00690   return 
00700 ! ______________________________________________________________________
00710 END1: ! 
00720   gosub VNTOT
00730   print #255,using 'Form POS 33,C 30,5*N 12.2': "Final Total",mat t2
00740   print #255: tab(63);rpt$("  ==========",5)
00750   let fncloseprn
00760   goto XIT
00770 ! ______________________________________________________________________
00780 XIT: let fnxit
00790 ! ______________________________________________________________________
00800 VNTOT: ! 
00810   let vnam$=""
00820   if hvn$<>"" then !:
          read #paymstr,using 'Form POS 9,C 30',key=hvn$: vnam$ nokey L830
00830 L830: print #255: tab(63);rpt$("  ----------",5)
00840   print #255,using 'Form POS 33,C 30,5*N 12.2': vnam$,mat t1
00850   gosub PRINT2_
00860   mat t2=t2+t1 : mat t1=(0)
00870   return 
00880 ! ______________________________________________________________________
00890 AGE2: ! 
00895   if ivd=0 then goto L950
00900   let das=max(0,int(fnjd(d1)-fnjd(ivd)))
00910   for j=1 to 3
00920     if das>=bk(j,1) and das<=bk(j,2) then goto L940
00930   next j
00940 L940: let p1=j*12+51 : let t1(j)+=upa : let t1(5)+=upa
00950 L950: return 
00960 ! ______________________________________________________________________
00970 ! <Updateable Region: ERTN>
00980 ERTN: let fnerror(program$,err,line,act$,"xit")
00990   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01000   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01010   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01020 ERTN_EXEC_ACT: execute act$ : goto ERTN
01030 ! /region
01040 ! ______________________________________________________________________
