14000 ! formerly S:\acsUB\BkDraft
14020 ! r: setup
14040   library 'S:\Core\Library': fntop,fnxit, fnLastBillingDate,fnAcs,fnLbl,fnTxt,fnTos,fnopenprn,fncloseprn,fnerror,fnmsgbox,fnCmdSet,fnChk,fndate_mmddyy_to_ccyymmdd,fnureg_write,fnureg_read,fnget_services
14060   on error goto ERTN
14080 ! r: dims
14100   dim pth$*128
14140   dim idn$*23,ion$*23,alloc(10)
14160   dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),alp$*7
14180   dim resp$(5)*128,msgline$(5)*80
14200 ! /r
18020   fntop(program$)
18030   fnLastBillingDate(d1)
18100   fnget_services(mat srvname$)
18120   for j=1 to 10
18140     if trim$(srvname$(j))>"" then order(j)=1 : sz1+=1
18160   next j
18180   fnureg_read('Bank Draft File',pth$,env$('Desktop')&'\bkdraft.dat')
18200   open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],Shr",internal,outIn,relative 
18220   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
18240   close #22: ioerr ignore
18260 goto SCREEN1 ! /r
22000 SCREEN1: ! r:
22020   fnTos(sn$="BKDraft")
22040   mylen=13 
22060   mypos=mylen+2
22080   respc=0
22100   fnLbl(1,1,"Billing Date:",mylen,1)
22120   fnTxt(1,mypos,8,0,1,"1")
22140   resp$(respc+=1)=str$(d1)
22160   fnLbl(2,1,"Payment Date:",mylen,1)
22180   fnTxt(2,mypos,8,0,1,"1")
22200   resp$(respc+=1)=""
22220   fnLbl(3,1,"File:",mylen,1)
22240   fnTxt(3,mypos,30,128,0,"70",0,"Destination and file name.",0)
22260   resp$(respc_bankDraftFile:=respc+=1)=pth$
22280   fnChk(4,mypos,"Post Collections:",1)
22300   fnCmdSet(2)
22320   fnAcs(sn$,0,mat resp$,ck)
22340   if ck=5 then goto XIT
22360   d1=val(resp$(1))
22380   d2=val(resp$(2))
22400   pth$=trim$(resp$(respc_bankDraftFile))
22420   fnureg_write('Bank Draft File',pth$)
22440   if resp$(4)="True" then postub=1
22460 goto initialization ! /r
26000 initialization: ! r: initialization
26020   open #22: "Name="&env$('temp')&"\BkDraft_Tmp_22."&session$&",RecL=94,Replace",display,output
26040   if postub=1 then 
26060     open #6: "Name=[Q]\UBmstr\Collections-"&env$('acsUserId')&".h[cno],RecL=91,Replace", internal,outIn,relative 
26080   end if 
26100   open #7: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,outIn,keyed 
26120   fnopenprn
26140   gosub HDRP1
26160   gosub HDR1
26180 goto READ_CUSTOMER ! /r
28000 READ_CUSTOMER: ! r: main loop
28020   read #1,using L440: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),mat gb,df$,dr$,bc,da$ eof END1
28040   L440: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 354,c 7,2*c 12,pos 388,10*pd 5.2,pos 1712,c 1,c 9,n 2,c 17
28060   if d1><f then goto READ_CUSTOMER
28080   if bal<=0 then goto READ_CUSTOMER
28100   if uprc$(df$)="Y" then gosub DETAIL1
28120 goto READ_CUSTOMER ! /r
32000 HDR1: ! r: ! FILE HEADER RECORD
32020   pcde=01 ! PRIORITY CODE
32040   ! will need simular lines for anyone using this option; actually need to be moved to company information
32080   if env$('client')="Monticello" then imd$=" 071121963" ! IMMEDIATE DESTINATION (party to which delivered - routing # of ach operator)
32100   if env$('client')="Franklinton" then imd$=" 061000146" ! IMMEDIATE DESTINATION (party to which delivered - routing # of ach operator)0610001
32120   if env$('client')="Billings" then imd$=" 071000301" ! IMMEDIATE DESTINATION (party to which delivered - routing # of ach operator)0610001
32140   if env$('client')="Thomasboro" then imd$="          " ! IMMEDIATE DESTINATION (party to which delivered - routing # of ach operator) (don't know on Thomasboro
32160   ! if env$('client')="Ashland" then imd$=" 084000039" ! IMMEDIATE DESTINATION (party to which delivered - routing # of ach operator) (don't know on Thomasboro
32200   if env$('client')="Monticello" then imo$=" 071121963" ! IMMEDIATE ORIGIN (routing number of the bank  which the city uses)
32220   if env$('client')="Franklinton" then imo$=" 065201611" ! IMMEDIATE ORIGIN (routing number of the bank  which the city uses)
32240   if env$('client')="Billings" then imo$=" 071000301" ! IMMEDIATE ORIGIN (routing number of the bank  which the city uses)
32260   if env$('client')="Thomasboro" then imo$=" 071113175" ! IMMEDIATE ORIGIN (routing number of the bank  which the city uses)
32280   ! if env$('client')="Ashland" then imo$=" 084201676" ! IMMEDIATE ORIGIN (routing number of the bank  which the city uses)
32300   fcd$=date$(1:2)&date$(4:5)&date$(7:8) ! FILE CREATION DATE
32320   if env$('client')="Depoe Bay" then fcd$=str$(d2)
32340   if len(fcd$)=5 then fcd$="0"&fcd$
32360   fct$=time$(1:2)&time$(4:5) ! FILE CREATION TIME
32380   fidm$="A" ! FILE ID MODIFIER
32400   rsz$="094" ! RECORD SIZE
32420   bf$="10" ! BLOCKING FACTOR
32440   if env$('client')="Billings" then ion$="                      " ! (23) IMMEDIATE ORIGIN NAME  (name of bank the city uses)
32460   if env$('client')="Thomasboro" then ion$="Gifford State Bank    " ! (23) IMMEDIATE ORIGIN NAME  (name of bank the city uses)
32480   ! if env$('client')="Ashland" then ion$="Merchants & Farmers    " ! (23) IMMEDIATE ORIGIN NAME  (name of bank the city uses)
32500   fc$="1" ! FORMAT CODE
32520   idn$="                       " ! (23) IMMEDIATE DESTINATION NAME
32560   if env$('client')="Monticello" then ion$="First State Bank       " ! (23) IMMEDIATE ORIGIN NAME  (name of bank the city uses)
32580   if env$('client')="Franklinton" then ion$="Parish National Bank  " ! (23) IMMEDIATE ORIGIN NAME  (name of bank the city uses)
32600   rc$="0000000" ! REFERENCE CODE
32620   pr #22,using L690: 1,pcde,imd$,imo$,fcd$,fct$,fidm$,rsz$,bf$,fc$,idn$,ion$,rc$,"0"
32640   L690: form pos 1,g 1,pic(##),c 10,c 10,g 6,g 4,c 1,c 3,c 2,c 1,c 23,c 23,c 7,c 1
32660   ! COMPANY/BATCH HEADER RECORD
32680   scc=225 ! SERVICE CLASS CODE
32700   cdd$="" ! COMPANY DISCRETIONARY DATA
32760   if env$('client')="Monticello" then cid$="1376001815" ! COMPANY IDENTIFICATION
32780   if env$('client')="Franklinton" then cid$="Franklinto" ! COMPANY IDENTIFICATION
32800   if env$('client')="Billings" then cid$=" 430903099" ! COMPANY IDENTIFICATION
32820   if env$('client')="Thomasboro" then cid$=" 376000521" ! COMPANY IDENTIFICATION
32840   ! if env$('client')="Ashland" then cid$="1646018046" ! COMPANY IDENTIFICATION
32860   ecc$="PPD" ! STANDARD ENTRY CLASS CODE
32880   if env$('client')="Billings" then ecc$='CCD' ! Corporate
32900   ced$="UTILITIES" ! COMPANY ENTRY DESCRIPTIVE
32920   eed$=date$(1:2)&date$(4:5)&date$(7:8) ! EFFECTIVE ENTRY DATE
32940   osc$="1" ! ORIGINATOR STATUS CODE
33000   if env$('client')="Monticello" then odi$="010251" ! ORIGINATING DFI IDENTIFICATION  (bank's account)
33020   if env$('client')="Franklinton" then odi$="06520161" ! ORIGINATING DFI IDENTIFICATION  (bank's account)
33040   if env$('client')="Billings" then odi$="08150596" ! ORIGINATING DFI IDENTIFICATION  (bank's account)
33060   if env$('client')="Thomasboro" then odi$=" 2000412" ! ORIGINATING DFI IDENTIFICATION  (bank's account)
33080   ! if env$('client')="Ashland" then odi$="08420167" ! ORIGINATING DFI IDENTIFICATION  (bank's account)
33100   bn=1 !  BN=BATCH NUMBER
33120   if env$('client')="Depoe Bay" then 
33140     pr #22,using L850: 5,scc,env$('cnam')(1:16),cdd$,cid$,ecc$,ced$,fcd$,fcd$,"",osc$,odi$,bn
33160   else 
33180     pr #22,using L850: 5,scc,env$('cnam')(1:16),cdd$,cid$,ecc$,ced$,fncd(d2),eed$,"",osc$,odi$,bn
33200   end if 
33220   L850: form pos 1,g 1,pic(###),c 16,c 20,c 10,c 3,c 10,pic(######),g 6,g 3,g 1,c 8,pic(#######)
33240 return  ! /r
38000 DETAIL1: ! r:
38020   pr #255,using L900: z$,e$(2),d2,bal pageoflow NEWPGE
38040   L900: form pos 1,c 12,c 32,pic(zz/zz/zz),n 13.2,skip 1
38060   t1=t1+bal
38080   tc=bc ! TRANSACTION CODE
38100   ! l1=LEN(RTRM$(BA$))
38120   ! cD=VAL(BA$(L1:L1)) ! CHECK DIGIT
38140   ari=0 ! ADDENDA RECORD INDICATOR
38160   tn1=tn1+1
38180   tn$=br$&cnvrt$("PIC(#######)",tn1) ! TRACE NUMBER
38200   pr #22,using L1000: 6,tc,dr$,da$,bal*100,z$,e$(2)(1:22),"",ari,odi$&tn$
38220   if postub=1 then gosub WRITE_POSTING_ENTRIES
38240   L1000: form pos 1,g 1,g 2,c 9,c 17,pic(##########),c 15,c 22,g 2,n 1,c 15
38260   td1=td1+bal
38280   if env$('client')<>"Billings" then tc1=tc1+bal
38300   eh=eh+val(dr$(1:8)) ! ENTRY HASH
38320 return  ! /r
42000 CTRL1: ! r: COMPANY/BATCH CONTROL RECORD
42020   scc=225 ! SERVICE CLASS CODE
42040   eac=tn1 ! ENTRY ADDENDA COUNT
42060   n=d2: m=bal: o(1)=3: rcpt$= "BkDraft"
42080   eh$=str$(eh): x=len(eh$): eh=val(eh$(max(1,x-9):x))
42100   ! TD1=TOTAL DEBIT AMOUNT
42120   ! TC1=TOTAL CREDIT AMOUNT
42140   ! CID$=COMPANY IDENTIFICATION
42160   tn1=tn1+1 : tn$=br$&cnvrt$("PIC(#######)",tn1) ! TRACE NUMBER
42180   if env$('client')="Billings" then 
42200     tn1-=1
42220   else 
42240     pr #22,using L1126: 6,22,imo$(2:9),imo$(10:10),trim$(odi$),td1*100,"",env$('cnam')(1:22),"",0,odi$&tn$ ! putting total deposit in city's bank account ! Billings does not offset the withdrawls with a deposit.  The banker does that manually.
42260     L1126: form pos 1,g 1,g 2,c 8,c 1,c 17,pic(##########),c 15,c 22,g 2,n 1,c 15
42280   end if 
42300   pr #22,using L1140: 8,scc,eac,eh,td1*100,tc1*100,cid$,mac$,"",odi$,bn
42320   L1140: form pos 1,g 1,pic(###),pic(######),pic(##########),2*pic(############),c 10,c 19,c 6,c 8,pic(#######)
42340   ! FILE CONTROL RECORD
42360   bactr=1 ! BATCH COUNT
42380   tn2=tn1+4 ! total # records (all 6 records plus the 1&5 plus 8&9)
42400   if fp(tn2/10)>0 then blctr=int(tn2/10)+1: bkfactor=blctr*10-tn2 ! block counter and block factor
42420   if fp(tn2/10)=0 then blctr=int(tn2/10): bkfactor=0
42440   eac=tn1 ! entry/adgenda count (number of 6 records)
42460   ! EH=ENTRY HASH
42480   pr #22,using L1230: 9,bactr,blctr,eac,eh,td1*100,tc1*100,"                                      "," "
42500   L1230: form pos 1,g 1,2*pic(######),pic(########),pic(##########),2*pic(############),c 38,c 1
42520   if bkfactor=0 then goto L1280
42540   for j=1 to bkfactor
42560     pr #22,using "Form POS 1,C 94": "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"
42580   next j
42600   L1280: ! 
42620 return  ! /r
46000 END1: ! r:
46020   gosub CTRL1
46040   pr #255: "                                                    {\ul            }"
46060   pr #255,using L900: "  Total","",0,t1
46080   fncloseprn
46100   gosub L1500
46120 if postub=1 then goto L1370 else goto XIT
46140 ! /r
48000 L1370: ! r:
48020   mat ml$(3) 
48040   ml$(1)="You have indicated you want to post the drafts" 
48060   ml$(2)="as collections to each customers account." 
48080   ml$(3)="Click OK to continue else Cancel to skip posting." 
48100   fnmsgbox(mat ml$,resp$,'',1)
48120 if resp$="OK" then gosub MERGE else goto XIT ! /r
50000 XIT: fnxit
51000 NEWPGE: pr #255: newpage : gosub HDRP1: continue 
52000 HDRP1: ! r:
52020   pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
52040   pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
52060   pr #255: "\qc  {\f181 \fs22 \b "&date$("Month DD, CCYY")&"}"
52080   pr #255: "{\ul Account No}  {\ul Customer Name                 }  {\ul Pay Date}  {\ul     Amount}"
52100 return  ! /r
54000 L1500: ! r:
54020   dim a$*94,srvname$(10)*20,ml$(3)*80,o(2),bd2(5)
54040   close #24: ioerr ignore
54060   open #24: "Name="&env$('temp')&"\BkDraft_Tmp_24."&session$&",RecL=96,EOL=None,Replace",external,output 
54080   close #22: ioerr ignore
54100   open #22: "Name="&env$('temp')&"\BkDraft_Tmp_22."&session$&",RecL=94",display,input 
54120   do 
54140     linput #22: a$ eof L1590
54160     if a$(94:94)="X" then a$(94:94)=""
54180     write #24,using "Form POS 1,C 94,C 1,c 1": rpad$(a$,94),chr$(13),chr$(10)
54200   loop 
54220   L1590: ! 
56000   close #24: 
56020   close #22: 
56040   COPY_TO_DESTINATION: ! 
56060   execute 'Copy "'&env$('temp')&"\BkDraft_Tmp_24."&session$&'" "'&pth$&'" -n' ioerr L1640
56080   execute 'Free "'&env$('temp')&"\BkDraft_Tmp_24."&session$&'"'
56100 return  ! /r
58000 L1640: ! r:
58020   mat msgline$(3)
58040   msgline$(1)="Unable to create file: "
58060   msgline$(2)=pth$
58080   msgline$(3)='Select OK to retry.'
58100   fnmsgbox(mat msgline$,resp$,'',65)
58120   if resp$="OK" then goto COPY_TO_DESTINATION
58140 goto XIT  ! /r
60000 ! <Updateable Region: ERTN>
60020 ERTN: fnerror(program$,err,line,act$,"xit")
60040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
60060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
60080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
60100 ERTN_EXEC_ACT: execute act$ : goto ERTN
60120 ! /region
60140 IGNORE: continue 
64000 WRITE_POSTING_ENTRIES: ! r:
64020   mat alloc(sz1)
64040   x=0
64060   for j=1 to 10
64080     if order(j)=1 then alloc(x+=1)=gb(j)
64100   next j
64120   m=bal : n=d2 : o(1)=3
64140   write #6,using "Form POS 1,C 10,PD 4.2,PD 4,2*N 1,POS 24,C 9,SZ1*PD 4.2,5*PD 3,PD 4.2": z$,m,n,mat o,rcpt$,mat alloc,mat bd2
64160 return ! /r
68000 MERGE: ! r:
68020   r6=0
68040   do
68060     L1860: !
68080     r6+=1
68100     if r6>lrec(6) then goto L2120 ! prevent stopping to deleted record and quit when finished
68120     read #6,using L1890,rec=r6: z$,m,n,mat o,rcpt$,mat alloc,mat bd2 noRec L1860
68140     L1890: form pos 1,c 10,pd 4.2,pd 4,2*n 1,pos 24,c 9,sz1*pd 4.2,5*pd 3,pd 4.2
68160     if p$(1:2)="  " and m=0 then goto L1860
68180     read #1,using 'Form POS 292,PD 4.2,POS 388,10*PD 5.2,pos 1859,pd 5.2',key=z$: bal,mat gb nokey L1860
68200     if o(1)=3 then tcode=3 ! collection
68220     if o(1)=4 then tcode=4 ! credit memo
68240     if o(1)=5 then tcode=5 ! debit memo
68260     if o(1)=5 then bal+=m else bal-=m
68280     tmp=fndate_mmddyy_to_ccyymmdd(n)
68300     mat tg=(0): x=0
68320     for j=1 to 10
68340       if trim$(srvname$(j))<>"" then tg(j)=alloc(x+=1)
68360     next j
68380     write #7,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,tmp,tcode,m,mat tg,0,0,0,0,0,0,bal,pcode
68400     j2=0
68420     for j=1 to 10
68440       if trim$(srvname$(j))<>'' then
68460         j2=j2+1
68480         if o(1)=5 then 
68500           gb(j)=gb(j)+alloc(j2) 
68520         else 
68540           gb(j)=gb(j)-alloc(j2)
68560         end if
68580       end if
68600     next j
68620     rewrite #1,using 'Form POS 292,PD 4.2,POS 388,10*PD 5.2,pos 1859,pd 5.2',key=z$: bal,mat gb
68640     o(2)=9
68660     rewrite #6,using "Form POS 19,2*N 1",rec=r6: mat o
68680   loop
68700   L2120: !
68720   close #6,free: 
68740 return  ! /r
70000 def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01) ! /r
