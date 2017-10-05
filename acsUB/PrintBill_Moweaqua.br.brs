00010 ! Replace S:\acsUB\ubprtbl1_mow
00020 ! pr bills for Village of Moweaqua
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fncmbrt2,fncombof,fnchk,fnerror,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpa_txt,fnpa_finis,fnpa_open,fnpa_font,fnpa_fontbold,fnpa_fontsize,fnpa_newpage,fnpa_line
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*40,txt$*45,mg$(3)*30,cap$*128
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
00100 ! ______________________________________________________________________
00110   fncno(cno,cnam$)
00120   fnd1(d1)
00130   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input 
00140   read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
00150   close #21: 
00160   at$(1)=cnam$
00170   z=21
00180   at$(1)=trim$(at$(1))(1:z)
00190   x=len(at$(1)) : y=z-x
00200   at$(1)=rpt$(" ",int(y/2))&at$(1)
00210   z=26
00220   for j=2 to udim(at$)
00230     at$(j)=trim$(at$(j))(1:z)
00240     x=len(at$(j)) : y=z-x
00250     at$(j)=rpt$(" ",int(y/2))&at$(j)
00260   next j
00270 ! linelength=62
00280 ! 
00290 ! 
00310   fn_bulksort
00320   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.H"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00330   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.H"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00340 ! ______________________________________________________________________
00350 SCREEN1: ! 
00360   a$="" : prtbkno=0
00370   fntos(sn$="UBPrtBl1-1")
00380   pf=26 : ll=24
00390   respc=0
00400   fnlbl(3,1,"Penalty Due Date:",ll,1)
00410   fntxt(3,pf,8,8,1,"1",0,tt$)
00420   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00430   fnlbl(4,1,"Message on Bill:",ll,1)
00440   fntxt(4,pf,30,30)
00450   resp$(respc+=1)=mg$(1)
00460   fntxt(5,pf,30,30)
00470   resp$(respc+=1)=mg$(2)
00480   fntxt(6,pf,30,30)
00490   resp$(respc+=1)=mg$(3)
00500   fnlbl(7,1,"Date of Billing:",ll,1)
00510   fntxt(7,pf,8,8,1,"1")
00520   resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00530   fnlbl(8,1,"Starting Account:",ll,1)
00540   fe$="ubm-act-nam"
00550   datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno)
00560   indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno)
00570   kp=1741: kl=9 : dp=41 : dl=30
00580   fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2)
00590   resp$(respc+=1)="[All]"
00600   fnlbl(9,1,"Route Number:",ll,1)
00610   fncmbrt2(9,pf)
00620   resp$(respc+=1)="[All]"
00630   fnchk(10,pf,"Select Accounts to Print",1)
00640   resp$(respc+=1)="False"
00650   fncmdset(3)
00660   fnacs(sn$,0,mat resp$,ck)
00670   if ck=5 then goto XIT
00680   d1=val(resp$(5))
00690   d4=val(resp$(1))
00700   mg$(1)=resp$(2)
00710   mg$(2)=resp$(3)
00720   mg$(3)=resp$(4)
00730   if resp$(6)="[All]" then a$="" else a$=lpad$(trim$(resp$(6)(1:9)),9)
00740   if resp$(7)="[All]" then prtbkno=0 else prtbkno=val(resp$(7))
00750   if resp$(8)="True" then sl1=1: z$="" else sl1=0
00760   if trim$(a$)<>"" then 
00770     read #2,using L460,key=a$: z$,route,sequence nokey SCREEN1
00780     st1=1
00790     st1$=z$
00800   end if 
00805 L460: form pos 1,c 10,pos 1741,n 2,n 7
00810   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00815   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00820   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00825 ! ______________________________________________________________________
00830   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.H"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.H"&str$(cno)&",Shr",internal,input,keyed 
00835   fnpa_open("Landscape")
00840   lyne=3
00857 NEXT_CUSTOMER: ! 
00858   if sl1=1 then goto SCR_ASK_CUSTOMER
00859 READ_SORT_FILE: ! 
00860   read #6,using 'form pos 22,c 10': z$ eof RELEASE_PRINT
00863   read #1,using F_CUSTOMER,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,sequence nokey READ_SORT_FILE
00865 F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1743,n 7
00867   if prtbkno=0 then goto L620
00869   if prtbkno><route then goto RELEASE_PRINT
00871 L620: if f><d1 then goto NEXT_CUSTOMER
00873   if st1=0 then goto AFTER_CUSTOMER_READ
00875   if st1$=z$ then st1=0 else goto NEXT_CUSTOMER
00877 AFTER_CUSTOMER_READ: ! 
00879 ! r: read alternate billing address
00881   read #3,using 'form pos 11,4*c 30',key=z$: mat ba$ nokey L750
00885   e1=0 : mat pe$=("")
00887   for j=1 to 4
00889     if rtrm$(ba$(j))<>"" then e1=e1+1 : pe$(e1)=ba$(j)
00891   next j
00893   goto PRINT_IT
00895 ! ______________________________________________________________________
00897 L750: ! 
00898   e1=0 : mat pe$=("")
00899   for j=2 to 4
00901     if rtrm$(e$(j))<>"" then e1=e1+1 : pe$(e1)=e$(j)
00903   next j
00905   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00907   goto PRINT_IT
00909 ! /r
00911 RELEASE_PRINT: ! 
00913   close #1: ioerr ignore
00915   close #3: ioerr ignore
00917   fnpa_finis
00919   goto ENDSCR
00921 ! ______________________________________________________________________
00923 PRINT_IT: ! r:
00924   if bal<>0 then 
00925     pb=bal-g(11)
00927     if bal<=0 then g(10)=0 ! don't show penalty if balance 0 or less
00929 ! ______________print bill routine______________________________________
00931     fn_vbprint
00933 ! _____________end of pr routine______________________________________
00935     bct(2)=bct(2)+1 ! accumulate totals
00936   end if 
00937   goto NEXT_CUSTOMER ! /r
00939 ! ______________________________________________________________________
00941 SCR_ASK_CUSTOMER: ! r:
00943   sn$="UBPrtBl1-2"
00945   fntos(sn$)
00947   txt$="Account (blank to stop)"
00949   fnlbl(1,1,txt$,31,1)
00951 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
00953   if trim$(z$)<>"" then 
00957     fnlbl(3,1,"Last Account entered was "&z$,44,1)
00965   end if 
00967   fncmbact(1,17) ! 
00969   resp$(1)=a$
00971   fncmdset(3)
00972   fnacs(sn$,0,mat resp$,ck)
00973   a$=lpad$(trim$(resp$(1)(1:10)),10)
00975   if trim$(a$)="" then goto RELEASE_PRINT
00977   if ck=5 then goto RELEASE_PRINT
00979   read #1,using F_CUSTOMER,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,sequence nokey SCR_ASK_CUSTOMER
00981   goto AFTER_CUSTOMER_READ ! /r
01390 ENDSCR: ! r: pr totals screen
01400   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01410   fntos(sn$="Bills-Total")
01420   mylen=23 : mypos=mylen+2
01430   respc=0
01440   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01450   fntxt(1,mypos,8,0,1,"",1)
01460   resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01580   fncmdset(52)
01590   fnacs(sn$,0,mat resp$,ck)
01592   goto XIT ! /r
01600 XIT: fnxit
01670   def fn_vbprint
01675 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
01680     checkcounter+=1
01685     if checkcounter=1 then xmargin=0 : ymargin=0
01690     if checkcounter=2 then xmargin=139 : ymargin=0
01695     if checkcounter=3 then xmargin=0 : ymargin=108
01700     if checkcounter=4 then xmargin=139 : ymargin=108 : checkcounter=0
01710 ! ______________________________________________________________________
01715     fnpa_line(xmargin+5,ymargin+2,55,lyne*3+3,1)
01720     fnpa_fontbold(1)
01725     fnpa_fontsize(12)
01730     fnpa_font
01735     fnpa_txt("Village of Moweaqua",xmargin+8,lyne*1-1+ymargin)
01740     fnpa_font("Lucida Console")
01745     fnpa_fontsize
01750     fnpa_fontbold
01755     fnpa_txt("    122 North Main  ",xmargin+6,lyne*2+1+ymargin-.2)
01760     fnpa_txt("  Moweaqua, IL 62550    ",xmargin+6,lyne*3+1+ymargin)
01765     fnpa_txt("#"&trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
01770     fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
01775     fnpa_txt('From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3),xmargin+2,lyne*7+ymargin)
01780     fnpa_txt("Is due now and payable.",xmargin+2,lyne*8+ymargin)
01785     fnpa_txt('Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1),xmargin+2,lyne*11+ymargin)
01790     fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
01795     fnpa_txt("Reading",xmargin+10,lyne*13+ymargin)
01800     fnpa_txt("Usage",xmargin+33,lyne*13+ymargin)
01805     fnpa_txt("Charge",xmargin+50,lyne*13+ymargin)
01810 ! ______________________________________________________________________
01815 ! PRINTGRID:
01816     meter=14
01820     fnpa_fontsize(8)
01825     if g(1) then 
01830       fnpa_txt("WTR",xmargin+1,lyne*(meter+=1)+ymargin)
01835       fnpa_txt(fnformnumb$(d(1),0,9),xmargin+6,lyne*meter+ymargin)
01840       fnpa_txt(fnformnumb$(d(3),0,9),xmargin+25,lyne*meter+ymargin)
01845       fnpa_txt(fnformnumb$(g(1),2,9),xmargin+45,lyne*meter+ymargin)
01850     end if 
01855     if g(2) then 
01860       fnpa_txt("SWR",xmargin+1,lyne*(meter+=1)+ymargin)
01865       fnpa_txt(fnformnumb$(g(2),2,9),xmargin+45,lyne*meter+ymargin)
01870     end if 
01875     if g(3) then 
01880       fnpa_txt("Water Plant Improvements",xmargin+1,lyne*(meter+=1)+ymargin)
01895       fnpa_txt(fnformnumb$(g(3),2,9),xmargin+45,lyne*meter+ymargin)
01900     end if 
02020 ! L2020: !
02030     if a4=1 then 
02040       gcode$="RSGS"
02050     else if a4=2 then 
02060       gcode$="CMGS"
02070     else if a4=3 then 
02080       gcode$="INGS"
02090     else 
02100       gcode$="GAS"
02110     end if 
02120     if g(4) then 
02130       fnpa_txt(gcode$,xmargin+1,lyne*(meter+=1)+ymargin)
02140       fnpa_txt(fnformnumb$(d(9),0,9),xmargin+6,lyne*meter+ymargin)
02150       fnpa_txt(fnformnumb$(d(11),0,9),xmargin+25,lyne*meter+ymargin)
02160       fnpa_txt(fnformnumb$(g(4),2,9),xmargin+45,lyne*meter+ymargin)
02170     end if 
02180     if g(5) then 
02190       fnpa_txt("SAN",xmargin+1,lyne*(meter+=1)+ymargin)
02200       fnpa_txt(fnformnumb$(g(5),2,9),xmargin+45,lyne*meter+ymargin)
02210     end if 
02220     if g(6) then 
02230       fnpa_txt("FP",xmargin+1,lyne*(meter+=1)+ymargin)
02240       fnpa_txt(fnformnumb$(g(6),2,9),xmargin+45,lyne*meter+ymargin)
02250     end if 
02260     if g(7) then 
02270       fnpa_txt("FEUL ADJ",xmargin+1,lyne*(meter+=1)+ymargin)
02280       fnpa_txt(fnformnumb$(g(7),2,9),xmargin+45,lyne*meter+ymargin)
02290     end if 
02300     if g(8) then 
02310       fnpa_txt("MISC",xmargin+1,lyne*(meter+=1)+ymargin)
02320       fnpa_txt(fnformnumb$(g(8),2,9),xmargin+45,lyne*meter+ymargin)
02330     end if 
02340     if g(9) then 
02350       fnpa_txt("TAX",xmargin+1,lyne*(meter+=1)+ymargin)
02360       fnpa_txt(fnformnumb$(g(9),2,9),xmargin+45,lyne*meter+ymargin)
02370     end if 
02380     if pb then 
02390       fnpa_txt("Previous Balance",xmargin+1,lyne*(meter+=1)+ymargin)
02400       fnpa_txt(fnformnumb$(pb,2,9),xmargin+45,lyne*meter+ymargin)
02410     end if 
02420     fnpa_fontsize
02430 ! ______________________________________________________________________
02440     fnpa_line(xmargin+1,lyne*23+1+ymargin,63,0)
02450     fnpa_txt('Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+1,lyne*24+ymargin)
02460     fnpa_txt(fnformnumb$(bal,2,9),xmargin+42,lyne*24+ymargin)
02470     fnpa_txt('Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+1,lyne*25+ymargin)
02480     fnpa_txt(fnformnumb$(bal+g(10),2,9),xmargin+42,lyne*25+ymargin)
02490     fnpa_line(xmargin+1,lyne*26+1+ymargin,63,0)
02500     fnpa_txt("Phone: 217-768-3435",xmargin+1,lyne*27+ymargin)
02510 ! ______________________________________________________________________
02520 !     special=28
02530 ! ______________________________________________________________________
02540     fnpa_fontsize(7)
02550     fnpa_line(xmargin+97,ymargin+0,29,lyne*5+2,1)
02560     fnpa_line(xmargin+90,ymargin+0,7,0)
02570     fnpa_line(xmargin+90,ymargin+2.8,7,0)
02580     fnpa_line(xmargin+90,ymargin+5.6,7,0)
02590     fnpa_line(xmargin+90,ymargin+8.4,7,0)
02600     fnpa_line(xmargin+90,ymargin+11.2,7,0)
02610     fnpa_line(xmargin+90,ymargin+14,7,0)
02620     fnpa_line(xmargin+90,ymargin+17,7,0)
02630     fnpa_txt("   Pre-Sorted",xmargin+100,lyne*1-1+ymargin)
02640     fnpa_txt("First Class Mail",xmargin+100,lyne*2-1+ymargin)
02650     fnpa_txt("  U.S. Postage  ",xmargin+100,lyne*3-1+ymargin)
02660     fnpa_txt("      Paid",xmargin+100,lyne*4-1+ymargin)
02670     fnpa_txt("  Permit No 38",xmargin+100,lyne*5-1+ymargin)
02680     fnpa_fontsize(9)
02690 ! fnpa_txt("Address Service Requested",XMARGIN+68LYNE*7+YMARGIN-6)
02700     fnpa_txt("Please return this",xmargin+68,lyne*7+ymargin)
02710     fnpa_txt("side with payment to:",xmargin+68,lyne*8+ymargin)
02720     fnpa_txt("Village of Moweaqua",xmargin+68,lyne*9+ymargin)
02730     fnpa_fontsize
02740     fnpa_txt('Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*11+ymargin)
02750     fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*11+ymargin)
02760     fnpa_txt('After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*12+ymargin)
02770     fnpa_txt(fnformnumb$(bal+g(10),2,9),xmargin+106,lyne*12+ymargin)
02780     fnpa_fontsize(9)
02790     addy=14
02800     fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
02810     fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
02820     fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
02830     addy+=1
02840     fnpa_fontsize
02850     if df$="Y" then 
02860       fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
02870     end if 
02880     if c4>0 then 
02890       fnpa_txt("Final Bill",xmargin+1,lyne*(addy+=1)+ymargin)
02900     end if 
02910     if d(10)=1 then 
02920       fnpa_txt("Bill Estimated",xmargin+1,lyne*(addy+=1)+ymargin)
02930     end if 
02940     fnpa_txt("#"&trim$(z$)&' '&bulk$,xmargin+68,lyne*(addy+=1)+ymargin)
02950     fnpa_txt(pe$(1),xmargin+68,lyne*(addy+=1)+ymargin)
02960     fnpa_txt(pe$(2),xmargin+68,lyne*(addy+=1)+ymargin)
02970     fnpa_txt(pe$(3),xmargin+68,lyne*(addy+=1)+ymargin)
02980     fnpa_txt(pe$(4),xmargin+68,lyne*(addy+=1)+ymargin)
02982     fnpa_txt("Address Service Requested",xmargin+68,lyne*(addy+=2)+ymargin)
02990     if checkcounter=1 then checkx=1.375 : checky=3.6875
03000     if checkcounter=2 then checkx=6.75 : checky=3.6875
03010     if checkcounter=3 then checkx=1.375 : checky=7.9375
03020     if checkcounter=0 then checkx=6.75 : checky=7.9375
03050     if checkcounter=0 then let fnpa_newpage
03060   fnend 
03070 ! ______________________________________________________________________
03080   def fn_bulksort ! bulk sort order
03090     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
03100     open #6: "Name="&env$('Temp')&"\Temp."&session$&",Replace,RecL=31",internal,output 
03105 L2730: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2760
03110     write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
03115     goto L2730
03117 L2760: close #1: ioerr ignore
03119     close #6: ioerr ignore
03121     execute "Index "&env$('Temp')&"\Temp."&session$&" "&env$('temp')&"\Tempidx."&session$&" 1,19,Replace,DupKeys -n"
03123     open #6: "Name="&env$('Temp')&"\Temp."&session$&",KFName="&env$('temp')&"\Tempidx."&session$,internal,input,keyed 
03125 ! 
03127   fnend 
76000 IGNORE: continue 
76020 ! <updateable region: ertn>
76040 ERTN: fnerror(program$,err,line,act$,"xit")
76060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76120 ERTN_EXEC_ACT: execute act$ : goto ERTN
76140 ! </updateable region: ertn>
