00010 ! formerly S:\acsGL\acglinc
00020 ! -- Print Income Statement !:
        ! FOR 8 1/2 * 11 PAPER WITHOUT PERCENTAGES
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fnprocess,fnchain,fnUseDeptNo,fnpedat$,fnps,fnpriorcd,fnfscode,fnactpd$,fncch$,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs,fnactpd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,cch$*20,by(13),bp(13),cap$*128,form$*200
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132
00090   dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2),sc1$(2)*20
00100 ! ______________________________________________________________________
00110   let fntop(program$)
00140   let actpd$=fnactpd$
00142   let actpd=fnactpd
00147   ! fscode=fnfscode
00148   ! priorcd=fnpriorcd
00150   if fnglfs=5 then goto XIT ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00155   let fscode=fnfscode 
00156   let priorcd=fnpriorcd 
00160   let pors=1
00170   let mp1=69
00200   if fnps=2 then 
00202     let mp1=mp1+3
00204     let mp2=78
00206     let fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSJ.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSJINDX.h"&env$('cno')&",Shr"
00208   else 
00210     let mp2=75
00212     let fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\FNSIINDX.h"&env$('cno')&",Shr"
00214   end if 
00216   open #h_acglfnx:=1: fl1$,internal,input,keyed 
00220   if fnprocess=1 or fnUseDeptNo=0 then goto L320
00230   let fntos(sn$="GLInput")
00232   let mylen=30: let mypos=mylen+3 : let right=1
00240   let fnlbl(1,1,"Cost Center or Department #:",mylen,right)
00250   let fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to print only one department, else leave blank for all.",0 )
00252   let resp$(1)=""
00260   let fnlbl(2,1,"(Blank for all Departments)",mylen,right)
00270   let fncmdkey("&Next",1,1,0,"Prints the financial statement.")
00280   let fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00290   let fnacs(sn$,0,mat resp$,ckey)
00300   if ckey=5 then goto XIT
00310   let costcntr=val(resp$(1))
00320 L320: ! 
00322   let fnopenprn
00330   if fnps=2 then ! secondary
00332     execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&env$('temp')&"\fsindex.H"&env$('cno')&" 72 3 Replace DupKeys -N"
00334   else 
00336     execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&env$('temp')&"\fsindex.H"&env$('cno')&" 69 3 Replace DupKeys -N"
00338   end if 
00370   open #h_glmstr:=3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('temp')&"\fsindex.h"&env$('cno')&",Shr",internal,input,keyed 
00372 F_GLMSTR_A: form pos mp1,pd 3,pos 81,2*pd 6.2
00374 F_GLMSTR_B: form pos 1,c 12,pos mp1,pd 3,pos mp2,pd 3,pos 81,41*pd 6.2
00380   let report$="Statement of Income and Expenses"
00390 ! GOSUB BLDPCT1 ! BUILD % BASED ON REF # IN PRIMARY FUND # IN G/L ACCOUNT
00400 READ_ACGLFNS: ! 
00402   read #h_acglfnx,using 'form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3': r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1650
00404  ! if trim$(d$(1:sp2))='INCOME TAX' then debug_this=1 else debug_this=0 ! pr #255: 'just read the d$' : pause
00410   if ltrm$(r$)="" or ltrm$(r$)="0" then goto READ_ACGLFNS
00420   if costcntr=0 then goto L450
00430   if fc=0 and te$="F" then goto L460 ! 5/8/89
00440   if costcntr><fc then goto READ_ACGLFNS
00450 L450: ! 
00460 L460: if te$="S" or te$="F" then goto L480
00470   if heading=0 and te$><"R" then gosub HDR_REPORT
00480 L480: on pos ("RFHDTS",te$,1) goto L1070,L1110,L490,L540,L970,L1070 none READ_ACGLFNS
00490 L490: print #255,using L500: d$(1:40)
00500 L500: form pos sp,c 40,skip 1
00510   gosub HDR_COLUMN_A
00520   gosub RESET_ACCUM_ARRAY
00530   goto READ_ACGLFNS
00540 L540: if notrans=1 then goto L750
00550   if ir=val(r$) and val(r$)><0 then goto L680
00560   if ir>val(r$) then goto L680
00570 L570: ! read gl master file for amounts
00580 L580: read #h_glmstr,using F_GLMSTR_B: gl_number$,ir,pcr,bb,cb,mat by,mat bp eof L740
00590   if ir=0 then goto L580 ! skip any gl accounts not pointed to ic
00610   if fscode=0 or (fscode=actpd and priorcd=1) then goto L680
00620   if fscode<1 or fscode>13 then let fscode=1
00630   if priorcd=1 then let cb=by(fscode) else let cb=bp(fscode)
00640   if priorcd=2 then goto L670
00650   if fscode>1 then let bb=by(fscode-1) else let bb=0
00660   goto L680
00670 L670: ! 
00672   if fscode>1 then let bb=bp(fscode-1) else let bb=0
00680 L680: ! 
00682   if ir=val(r$) then 
00683 !  if debug_this then pr gl_number$;'  total(';total;')+=(cb(';cb;')-bb(';bb;'))'
00684     let total=total+(cb-bb)
00685 !  if debug_this then pr gl_number$;'  so now total=';total: pause
00686   else 
00688     goto L720
00690   end if 
00692   let total2=total2+cb
00700   let k$=cnvrt$("N 5",pcr)
00710   goto L570
00720 L720: if ir<val(r$) then goto L570
00730   if ir>val(r$) then goto L750
00740 L740: let notrans=1
00750 L750: ! 
00752   for j=1 to 9
00760     if ac(j)=9 then goto L790 ! 10/14/87
00770     let accum(j,1)=accum(j,1)+total
00780     let accum(j,2)=accum(j,2)+total2
00790 L790: ! 
00792   next j
00800   if rs=1 then 
00802     let total=-total
00804   else 
00806     goto L820
00808   end if 
00810   let total2=-total2
00820 L820: ! 
00822   if ds=1 then 
00824     let dollar$="$"
00826   else 
00828     let dollar$=" "
00830   end if 
00832   if total><0 or total2><0 then goto L850
00840   if ls+ul+ds+ic>0 then goto L850 else goto READ_ACGLFNS
00850 L850: let sp2=49-sp-1
00852 ! If DS=1 Then Let DOLLAR$="": Let FORM$="Form POS SP,C SP2,POS 50,C 1,C 5,PIC($$$$,$$$,$$$.##),C 1,POS 74,C 1,C 5,PIC($$$$$$,$$$,$$$.##),C 1,skip 1" Else Let FORM$="Form POS SP,C SP2,POS 50,C 1,C 5,PIC(----,---,---.##),C 1,POS 74,C 1,C 5,PIC(------,---,---.##),C 1,skip 1"
00853 ! if debug_this then pr #255: '***'
00854   if ul=1 then 
00856     print #255,using L856: d$(1:sp2),dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}" pageoflow PGOF
00858 L856: form pos sp,c sp2,pos 50,c 1,c 5,pic(----,---,---.##),c 1,pos 74,c 1,c 5,pic(------,---,---.##),c 1,skip 1
00860   else 
00862     print #255,using L870: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow PGOF
00864 L870: form pos sp,c sp2,pos 49,c 1,pic(-----,---,---.##),pos 67,c 1,pic(-------,---,---.##),skip 1
00866   end if 
00868 ! if debug_this then pr #255: '***'
00880   if pc0=1 then gosub BLDPCT2
00890   if pc3>0 or pc4>0 then print #255,using L900: pc3,pc4
00900 L900: form pos 63,n 4,pos 82,n 4,skip 1
00910   let total=0
00920   let total2=0
00930   gosub RESET_ACCUM_ARRAY
00935   if ul=1 then goto L950
00940   gosub L1410
00950 L950: ! 
00952   gosub HDR_COLUMN_A
00960   goto READ_ACGLFNS
00970 L970: if ap=0 then let ap=1
00980   if rs=1 then let accum1=-accum(ap,1) else let accum1=accum(ap,1)
00990   if rs=1 then let accum2=-accum(ap,2) else let accum2=accum(ap,2)
01000   if ds=1 then let dollar$="$" else let dollar$=" "
01002   let sp2=49-sp-1
01004   if ds=1 then 
01006     let dollar$=""
01008     let form$="Form POS SP,C SP2,POS 50,C 1,C 5,PIC($---,---,---.##),C 1,POS 74,C 1,C 5,PIC($-----,---,---.##),C 1,skip 1"
01010   else 
01012     let form$="Form POS SP,C SP2,POS 50,C 1,C 5,PIC(----,---,---.##),C 1,POS 74,C 1,C 5,PIC(------,---,---.##),C 1,skip 1"
01014   end if 
01015 ! print some sub total like thingies
01016   if ul=1 then 
01018     print #255,using L856: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}" pageoflow PGOF
01026   else 
01028     print #255,using L870: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow PGOF
01030   end if 
01032   gosub RESET_ACCUM_ARRAY
01035   if ul=1 then goto L1050
01040   gosub L1410
01050 L1050: ! 
01052   gosub HDR_COLUMN_A
01060   goto READ_ACGLFNS
01070 L1070: ! r:
01072   if te$="R" then let report$=d$
01080   if te$="S" then let secondr$=d$
01090   gosub HDR_COLUMN_A
01100   goto READ_ACGLFNS ! /r
01110 L1110: ! 
01112   if foot1=1 then goto L1160
01120   let tabnote=sp
01130   let foot1=1
01140   let foot$=d$
01150   goto READ_ACGLFNS
01160 L1160: ! 
01162   let foot$=rtrm$(foot$)&d$
01170   goto READ_ACGLFNS
01180 RESET_ACCUM_ARRAY: ! r:
01182   for j=1 to 9
01190     if ac(j)=0 or ac(j)=9 then goto L1220 ! 10/14/87
01200     let accum(j,1)=0
01210     let accum(j,2)=0
01220 L1220: ! 
01222   next j
01230   return  ! /r
01240 HDR_COLUMN_A: ! r:
01242   if ls=0 then goto HDR_COLUMN_XIT
01250   if ls=99 then goto HDR_COLUMN_B
01260   print #255,using L1270: " "
01270 L1270: form pos 1,c 1,skip ls
01280   goto HDR_COLUMN_XIT
01290 HDR_COLUMN_B: ! 
01292   let fnpglen(pglen)
01300 ! If PGLEN<>42 Then Let PGLEN=58
01310   let sk=pglen-krec(255): let fl=len(rtrm$(foot$))
01320 ! If PGLEN=42 Then Let SK=SK+1
01330   print #255,using L1340: rtrm$(foot$),"Page "&str$(pt1)
01340 L1340: form skip sk,pos tabnote,c fl,pos 80,c 8,skip 1
01350   if eofcode=1 then goto HDR_COLUMN_XIT
01360   print #255: newpage
01370   gosub HDR_REPORT
01380 HDR_COLUMN_XIT: ! 
01382   return  ! /r
01390 PGOF: ! r:
01392   gosub HDR_COLUMN_B
01400   continue  ! /r
01410 L1410: ! r:
01412   if ul=0 then goto L1500
01420   if ul=1 then goto L1470
01440   print #255,using 'form pos 49,c 17,pos 67,c 19': "=================","==================="
01460   goto L1500
01470 L1470: ! 
01480   print #255: ''
01482   print #255,using 'form pos 49,c 18,pos 67,c 19': "_________________","___________________"
01500 L1500: ! 
01520   return  ! /r
01530 HDR_REPORT: ! r:
01532   let heading=1
01540   if pt1=0 then let pt1=1 else let pt1=pt1+1
01550   print #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
01560   print #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
01570   if trim$(secondr$)<>"" then print #255: "\qc  {\f181 \fs24 \b "&trim$(secondr$)&"}"
01580   print #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
01590   print #255: "\ql "
01600   print #255: 
01610   print #255,using L1620: lpad$(rtrm$(fncch$),20),"Year To Date"
01620 L1620: form pos 45,c 20,pos 73,c 12,skip 2
01630   return  ! /r
01640 ! ______________________________________________________________________
01650 L1650: ! r:
01652   let eofcode=1
01660   gosub HDR_COLUMN_B
01675   let fnfscode(actpd)
01676   let fnpriorcd(1)
01680   let fncloseprn
01690   goto XIT ! /r
01700 ! ______________________________________________________________________
01710 BLDPCT1: ! r:
01712   open #10: "Name="&env$('temp')&"\Work."&session$&",KFName="&env$('Temp')&"\Addr."&session$&",Replace,RecL=17,KPS=1,KLN=5",internal,outin,keyed 
01720   for j=1 to lrec(3)
01730     read #h_glmstr,using F_GLMSTR_A,rec=j: pc1,bb,cb norec L1830
01750     let k$=cnvrt$("N 5",pc1)
01760     read #10,using L1770,key=k$: pc1,pc2,yt2 nokey L1820
01770 L1770: form pos 1,g 5,2*pd 6.2
01780     let pc2=pc2+cb-bb
01790     let yt2=yt2+cb
01800     rewrite #10,using L1770: pc1,pc2,yt2
01810     goto L1830
01820 L1820: write #10,using L1770: pc1,cb-bb,cb
01830 L1830: next j
01840   let pc0=1
01850   return  ! /r
01860 ! ______________________________________________________________________
01870 BLDPCT2: ! r:
01880   let pc3=pc4=0
01890   if val(k$)=0 then goto L1970
01900   read #10,using L1770,key=k$: pc1,pc2,yt2 nokey L1970
01910   if total=0 then goto L1940
01920   let pc3=round(((total-pc2)/total)*100,0)
01930   if pc3<-999 or pc3>9999 then let pc3=0
01940 L1940: if total2=0 then goto L1970
01950   let pc4=round(((total2-yt2)/total2)*100,0)
01960   if pc4<-999 or pc4>9999 then let pc4=0
01970 L1970: ! 
01972   return  ! /r
01980 ! ______________________________________________________________________
01990 XIT: let fnxit
02000 ! ______________________________________________________________________
02010 ! <Updateable Region: ERTN>
02020 ERTN: let fnerror(program$,err,line,act$,"xit")
02030   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02040   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02050   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
02060 ERTN_EXEC_ACT: execute act$ : goto ERTN
02070 ! /region
