10000 ! REPLACE S:\acsTM\arinput_import_csv.br
10100   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnd1,fnsearch
10200   library 'S:\Core\Library': fnerror,fnopenprn,fncloseprn,fnacs,fnflexadd1,fnflexinit1,fntos,fncustomer_search,fnlbl,fntxt,fnmsgbox,fnbutton,fnfra
10300   library 'S:\Core\Library': fndat,fncmbact,fncombof,fncmbrt2,fnd1,fncmdset,fncmdkey,fntop,fngethandle
10400   library 'S:\Core\Library': fntransfile
10500   let fntop(program$,cap$="Import Transactions from Mint CSV to CL")
10600   dim cr$*1,lf$*1,crlf$*2,line$*2048,item$(1)*1024
10700   cr$=chr$(13) : let lf$=chr$(10)
10800   crlf$=cr$&lf$
11100   dim fl1$(7),flo1$(11),sc3$(5),pt(6),f3$*255,flo3$(6),name$*25,cap$*128
11200   dim p$*5,iv$*12,tr(6),id$*20,sc1$(5),sc2$(9),hd$(2)*50
11300   dim flo4$(5),sc4$(5),ot4$(5),fli4$(5),q(3),gln1(3),gln2(3),otgl$(3)
11400   dim gl(10,4),fli1$(49),ot1$(49),pgl(3)
11500   let fn_get_old_setup
11600   open #h_clmstr:=9: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&env$('cno')&",Shr",internal,input,keyed ioerr ERR_FILE
11700   open #11: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\TMmstr\CLIndx2.h"&env$('cno')&",Shr",internal,input,keyed ioerr ERR_FILE
11800   open #h_addr:=3: "Name="&env$('Temp')&"\Addr."&session$&",RecL=239,Replace",internal,outin,relative ioerr ERR_FILE
11900 SCREEN_1: ! r:
12000 ! exec 'config dimonly'
12100   dim file_import$*256,filter_date(2)
12200   dim label$(3)*40,resp$(3)*256
12300   let label$(1)='Starting Date:'
12400   let label$(2)='Ending Date:'
12500   let label$(3)='Mint Transaction CSV File To Import:'
12600   let filter_date(1)=20120101
12700   let filter_date(2)=20121231
12800   let file_import$=env$('userprofile')&'\Downloads\transactions.csv'
12900   let fntos(sn$="ask_fdd"&str$(udim(mat label$))&'_dates')
13000   let respc=0 : ad_line=0 : col1_len=36 : col2_pos=col1_len+2
13100   let fnlbl(ad_line+=1,1,label$(ad_line),col1_len,align_right:=1)
13200   let fntxt(ad_line,col2_pos,8,0,1,"3")
13300   let resp$(respc+=1)=str$(filter_date(ad_line))
13400   let fnlbl(ad_line+=1,1,label$(ad_line),col1_len,align_right)
13500   let fntxt(ad_line,col2_pos,8,0,1,"3")
13600   let resp$(respc+=1)=str$(filter_date(ad_line))
13700   let fnlbl(ad_line+=1,1,label$(ad_line),col1_len,align_right)
13800   let fntxt(ad_line,col2_pos,40,256,1,"70")
13900   let resp$(respc+=1)=file_import$
14000   let fncmdset(3)
14100   let fnacs(sn$,0,mat resp$,ckey)
14200   if ckey=5 then 
14300     let fkey(99)
14400   else 
14500     let filter_date(1)=val(srep$(resp$(1),'/',''))
14600     let filter_date(2)=val(srep$(resp$(2),'/',''))
14700     let file_import$=resp$(3)
14800   end if 
14900   let fn_import_it(file_import$)
15000   end  ! pr newpage
15100 ! pr fields mat fl1$: mat sc1$,"A/R Input Selection Menu","Selection:"
15200 L630: ! 
15300 ! 
15400   input fields "13,29,n 1,eu,n": transaction_type conv L630
15500   if transaction_type=0 then let vf=1 : goto SCREEN_PROOF_TOTALS
15600   if transaction_type<1 or transaction_type>4 then goto L630
15700 ! /r
15800 SCREENS_TRANS_ENTRY_A: ! r: old
15900   if transaction_type=4 or transaction_type=3 then let sc2$(7)="G/L # to Credit" else let sc2$(7)="G/L # to Debit"
16000   if transaction_type=3 then let sc2$(6)="Discount Amount" else let sc2$(6)=""
16100   if gx=0 then let sc2$(7)=" "
16200 SCREENS_TRANS_ENTRY_B: ! 
16300   pr newpage
16400   pr fields mat flo1$: mat sc2$,"A/R Input "&sc1$(transaction_type+1)(5:18),"Client Number as 0 to stop"
16500   let ps1=0
16600   if vf=0 then goto L790
16700   if gx><0 then goto L780
16800 L760: ! 
16900   pr fields mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2)
17000   goto L790
17100 L780: ! 
17200   pr fields mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl
17300 L790: ! 
17400   pr fields "5,30,pic(zzzzzz)": tr(1)
17500   pr fields "24,20,C 50,N": "F1 Continue   F2 verify name    F4 Search"
17600   if gx><0 then goto L910
17700 L820: ! 
17800   input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2) conv L870
17900   if cmdkey=4 then let fn_tmsrch : goto L760
18000   let p$=uprc$(lpad$(rtrm$(p$),5))
18100   if ce>0 then let fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
18200   ce=0
18300   goto L1280
18400 L870: ! 
18500   if ce>0 then let fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
18600   ce=cnt+1
18700   let fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,"U","RC")
18800   goto L820
18900 L910: ! 
19000   if ps1=1 or vf=1 then goto L1060
19100 L920: ! 
19200   rinput fields "3,30,C 5,EU,n": p$ conv L920
19300   if cmdkey=4 then let fn_tmsrch : goto L920
19400   let p$=uprc$(lpad$(rtrm$(p$),5))
19500   if ltrm$(p$)="-1" then pr fields mat otgl$: mat gln1 else pr fields mat otgl$: mat gln2
19600   if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto SCREEN_1
19700   if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto L1630
19800   if ltrm$(p$)="-1" then let name$="CASH SALE" else goto L990
19900   goto L1050
20000 L990: ! 
20100   read #h_clmstr,using 'form pos 6,c 25',key=p$,release: name$ nokey L1020 ioerr ERR_FILE
20200   goto L1050
20300 L1020: ! 
20400   let name$="INVALID CLIENT NUMBER"
20500   pr fields "3,40,C 25,R,N": name$
20600   goto L920
20700 L1050: ! 
20800   pr fields "3,40,C 25,N": name$
20900 L1060: ! 
21000   let fli1$(4)="6,30,n 11.2,ut,n"
21100   if r1>0 then goto L1180
21200   if transaction_type=3 then let fli1$(4)="6,30,n 11.2,ue,n"
21300   input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl conv L1240
21400   if cmdkey=2 then goto L920
21500   if transaction_type<>3 then goto L1200
21600   let fli1$(4)="6,30,n 11.2,ut,n"
21700   if sz=4 then let gl(1,2)=gln1(2): let gl(1,1)=gln1(1): let gl(1,3)=tr(3)
21800   if sz=3 then let gl(1,1)=gln1(2): let gl(1,2)=gln1(3): let gl(1,3)=tr(3)
21900   if sz=2 then let gl(1,2)=gln1(2): let gl(1,1)=gln1(1): let gl(1,3)=gln1(3): let gl(1,4)=tr(3)
22000   if sz=5 then let gl(1,1)=gln1(2): let gl(1,2)=tr(3)
22100 L1180: ! 
22200   rinput fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl conv L1240
22300   if cmdkey=2 then goto L920
22400 L1200: ! 
22500   let p$=uprc$(lpad$(rtrm$(p$),5))
22600   if ce>0 then let fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
22700   ce=0
22800   goto L1280
22900 L1240: ! 
23000   if ce>0 then let fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
23100   ce=cnt+1
23200   let fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,"U","RC")
23300   if cnt<=4 then goto L1060 else goto L1180
23400 L1280: ! 
23500   if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto SCREEN_1
23600   if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto L1630
23700   let ps1=1
23800   if tr(1)<10100 or tr(1)>123199 then goto L1320 else goto L1340
23900 L1320: ! 
24000   pr fields "5,48,c 20": "INVALID DATE"
24100   goto L790
24200 L1340: ! 
24300   if tr(3)><0 then goto L1370
24400   pr fields "6,48,c 20": "NO AMOUNT ENTERED"
24500   goto L790
24600 L1370: ! 
24700   if gx=0 then goto L1520
24800   if pgl(gpx)>0 then goto L1410
24900   pr fields "9,45,c 30": "G/L # REQUIRED"
25000   goto L790
25100 L1410: ! 
25200   let gla=0
25300   for j=1 to 10
25400     if gl(j,gx)=0 then goto L1460
25500     let gla=gla+gl(j,gx)
25600   next j
25700 L1460: ! 
25800   if transaction_type=3 then let gla=gla-tr(2)
25900   if gla=tr(3) then goto L1520
26000   pr fields "11,2,c 75,h,n": " G/L ALLOCATIONS DO NOT AGREE WITH TOTAL AMOUNT.  PRESS ENTER TO CONTINUE."
26100   input fields "11,78,c 1,EU,n": pause$
26200   pr fields "11,2,c 75,n,n": " "
26300   goto L790
26400 L1520: ! 
26500   if ltrm$(p$)="-1" then goto L1540
26600   let pt(1)=pt(1)+val(p$) conv L1540
26700 L1540: ! 
26800   let pt(transaction_type+1)=pt(transaction_type+1)+tr(3)
26900   if transaction_type=3 then let tdt=tdt+tr(2)
27000   if ltrm$(p$)="-1" then let pt(6)=pt(6)+tr(3)
27100   if vf=1 then goto L1670
27200   let r3=r3+1
27300   let tr(5)=transaction_type
27400   write #h_addr,using f3$,rec=r3: p$,iv$,mat tr,id$,mat pgl,mat gl
27500   let p$=""
27600   let q2=0
27700   goto SCREENS_TRANS_ENTRY_B
27800 L1630: ! 
27900   let iv$=" "
28000   mat tr=(0)
28100   let id$=" "
28200   mat gl=(0)
28300 L1670: ! 
28400   rewrite #h_addr,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl
28500   let p$=""
28600   goto SCREEN_ASK_REF_TO_FIX ! /r
28700 SCREEN_PROOF_TOTALS: ! r: old
28800   pr newpage
28900   pr fields mat fl1$: mat sc3$,"A/R Input Proof Totals",""
29000   pr fields "11,5,C 20": "Total Cash Sales"
29100   pr fields "12,5,C 22": "Total Discounts Taken"
29200   pr fields mat flo3$: mat pt
29300   pr fields "12,26,n 11.2": tdt
29400   pr fields "18,1,C 70,H,N": "1=Merge; 2=Corrections; 3=Proof List: 5=Stop Without Posting"
29500 L1790: ! 
29600   input fields "18,61,n 1,eu,n": j conv L1790
29700   if j=3 then let fn_print_proof_list : goto L1790
29800   if j=5 then goto XIT
29900   if j=1 then goto CHAIN_ARMERGE
30000   goto L1790
30100 ! /r
30200   def fn_print_proof_list
30300     let r=0
30400     let fnopenprn
30500     pr newpage
30600     on fkey 5 goto L2040
30700     pr newpage
30800     pr #255,using L1910: date$,env$('cnam'),time$,"Input Edit List"
30900 L1910: form pos 1,c 8,pos namtab,c 50,skip 1,pos 1,c 8,pos 58,c 50
31000     pr fields "10,20,C 40,N": "Input Edit Listing In Process"
31100     pr fields "23,2,C 30,N": "Press F5 To Stop"
31200     pr #255: "Ref #  Cl #  Invoice #";
31300     pr #255: tab(34);"Date     Amount             Description           Discount          Tr Code"
31400 L1960: ! 
31500     let r=r+1
31600     read #h_addr,using L2110,rec=r: p$,iv$,mat tr,id$ eof L2040,norec L2040 ioerr ERR_FILE
31700 L2110: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20
31800     if ltrm$(p$)="0" or ltrm$(p$)="" then goto L1960
31900     let name$=""
32000     read #h_clmstr,using 'form pos 6,c 25',key=p$,release: name$ nokey L2010
32100 L2010: ! 
32200     pr #255,using L2020: r,p$,iv$,tr(1),tr(3),tr(4),name$(1:22),tr(2),tr(5)
32300 L2020: form pos 1,n 4,x 2,c 5,x 2,c 18,n 6,n 11.2,pic(zzzzzz),x 7,c 22,n 12.2,n 12
32400     goto L1960
32500 L2040: ! 
32600     let fncloseprn
32700     on fkey 5 ignore 
32800   fnend 
32900 SCREEN_ASK_REF_TO_FIX: ! r:
33000   pr newpage
33100   pr fields "10,10,c 60": "Ref Number To Correct; (0 when Completed)"
33200 L2080: input fields "10,61,n 4,eu,n": r1 conv L2080
33300   if r1=0 then goto SCREEN_ASK_ADD_MORE
33400   read #h_addr,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl norec SCREEN_ASK_REF_TO_FIX ioerr ERR_FILE
33500   if ltrm$(p$)="0" or ltrm$(p$)="" then goto SCREEN_ASK_REF_TO_FIX
33600   let transaction_type=tr(5)
33700   if p><-1 then let pt(1)=pt(1)-val(p$) conv L2150
33800 L2150: let pt(transaction_type+1)=pt(transaction_type+1)-tr(3)
33900   if ltrm$(p$)="-1" then let pt(6)=pt(6)-tr(3)
34000   if transaction_type=3 then let tdt=tdt-tr(2)
34100   let hd$(1)="A/R Correct "&sc1$(transaction_type+1)(5:18)
34200   let hd$(2)="Enter Client # As 0 To Delete This Entry"
34300   let vf=1
34400   goto SCREENS_TRANS_ENTRY_A
34500 ! /r
34600 SCREEN_ASK_ADD_MORE: ! r:
34700   pr newpage
34800   let vf=0
34900   pr fields "10,10,c 50": "ENTER 1 TO MAKE ADDITIONAL ENTRIES; ELSE ENTER 2"
35000 L2250: ! 
35100   input fields "10,61,N 1,EU,N": j conv L2250
35200   on j goto SCREEN_1,SCREEN_PROOF_TOTALS none L2250
35300 ! /r
35400 CHAIN_ARMERGE: chain "S:\acsTM\ARMerge"
35500 XIT: ! 
35600   let fnxit
35700 ERR_FILE: ! r:
35800   if err=61 then pr fields "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2310
35900   goto L2350
36000 L2310: pr newpage
36100   if err=4148 then pr fields "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2340
36200   goto L2350
36300 L2340: pr fields "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
36400 L2350: pr fields "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
36500   input fields "24,60,C 1,N": quitcode$
36600   if err=61 and rtrm$(uprc$(quitcode$))="Q" then goto SCREEN_1 else goto L2410
36700   pr fields "23,3,C 78,N": ""
36800   pr fields "24,3,C 78,N": ""
36900   retry 
37000 L2410: goto XIT
37100 ! /r
37200   def fn_tmsrch !  search for customer #
37300     dim heading$*70,form$*80,numeric_format$*20,selection$*70
37400     let file_num=11 ! alpha index on clients
37500     let form$="form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2"
37600     let numeric_format$='pic($$$,$$$.##)'
37700     let key_length=5
37800     let heading$="Acct #-Name--------------------Address--------Balance"
37900     let fnsearch(cap$,file_num,heading$,form$,numeric_format$,selection$,key_length)
38000     let p$=selection$ ! pull key from first field in search line
38100     ano=0
38200     ano=val(selection$) conv L4910
38300 L4910: ! 
38400   fnend 
38500   def fn_get_old_setup
38600     open #h_company:=1: "Name="&env$('Q')&"\TMmstr\Company.h"&env$('cno')&",Shr",internal,input ioerr ERR_FILE
38700     read #h_company,using L130: i3,i4,i5,mat gln1,mat gln2 ioerr ERR_FILE
38800 ! Let I3=1 ! ENTER G/L #'S
38900 L130: form pos 161,3*n 1,pos 178,n 3,n 6,n 3,n 3,n 6,n 3
39000     close #h_company: 
39100     let namtab=66-int(len(rtrm$(env$('cnam')))/2)
39200     let otgl$(1)="9,30,pic(zzz)"
39300     let otgl$(2)="9,34,pic(zzzzzz)"
39400     let otgl$(3)="9,41,pic(zzz)"
39500     if i3=0 then goto L490
39600     if i4=1 and i5=1 then goto L300
39700     if i4=0 and i5=1 then goto L350
39800     if i4=1 and i5=0 then goto L420
39900 ! NO DEPT    NO SUBACCOUNT
40000     let sz=5
40100     let gx=2
40200     mat gl(10,2)=(0)
40300     mat pgl(1)=(0)
40400     let gpx=1
40500     goto L510
40600 L300: ! YES DEPT   YES SUBACCOUNT
40700     let sz=2
40800     let gx=4
40900     let gpx=2
41000     goto L510
41100 L350: ! NO DEPT    YES SUBACCOUNT
41200     let sz=3
41300     let gx=3
41400     mat gl(10,3)=(0)
41500     mat pgl(2)=(0)
41600     let gpx=1
41700     goto L510
41800 L420: ! YES DEPT    NO SUB ACCOUNT
41900     let sz=4
42000     let gx=3
42100     mat gl(10,3)=(0)
42200     mat pgl(2)=(0)
42300     let gpx=2
42400     goto L510
42500 L490: ! NO GL TO BE ENTERED
42600     let sz=6
42700 L510: ! 
42800 !        if sz=5 then
42900 !         let f3$='FORM POS 1,C 5,C 12,N 6,2*PD 5.2,PD 2,2*N 1,C 20,x 3,n 6,x 3,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2'
43000 !         let sc1$(1)="0 = Completed"
43100 !         let sc1$(2)="1 = Invoices"
43200 !         let sc1$(3)="2 = Debit Memos"
43300 !         let sc1$(4)="3 = Collections"
43400 !         let sc1$(5)="4 = Credit Memos"
43500 !      !
43600 !         let fl1$(1)='6,5,c 20'
43700 !         let fl1$(2)='7,5,c 20'
43800 !         let fl1$(3)='8,5,c 20'
43900 !         let fl1$(4)='9,5,c 20'
44000 !         let fl1$(5)='10,5,c 20'
44100 !         let fl1$(6)='3,10,c 50,h,n'
44200 !         let fl1$(7)='13,10,c 50,h,n'
44300 !      !
44400 !         let sc3$(1)="TOTAL ACCOUNT #'S"
44500 !         let sc3$(2)="TOTAL INVOICES"
44600 !         let sc3$(3)="TOTAL DEBIT MEMOS"
44700 !         let sc3$(4)="TOTAL COLLECTIONS"
44800 !         let sc3$(5)="TOTAL CREDIT MEMOS"
44900 !         let flo3$(1)='6,26,n 11.2'
45000 !         let flo3$(2)='7,26,n 11.2'
45100 !         let flo3$(3)='8,26,n 11.2'
45200 !         let flo3$(4)='9,26,n 11.2'
45300 !         let flo3$(5)='10,26,n 11.2'
45400 !         let flo3$(6)='11,26,n 11.2'
45500 !      !
45600 !         let flo1$(1)='3,5,c 20'
45700 !         let flo1$(2)='4,5,c 20'
45800 !         let flo1$(3)='5,5,c 20'
45900 !         let flo1$(4)='6,5,c 20'
46000 !         let flo1$(5)='7,5,c 20'
46100 !         let flo1$(6)='8,5,c 20'
46200 !         let flo1$(7)='9,5,c 20'
46300 !         let flo1$(8)='11,20,c 20'
46400 !         let flo1$(9)='11,40,c 20'
46500 !         let flo1$(10)='1,15,c 40,h,n'
46600 !         let flo1$(11)='2,5,c 45,h,n'
46700 !      !
46800 !         let ot1$(01)='3,30,C 5,ut,n'
46900 !         let ot1$(02)='4,30,c 12,ut,n'
47000 !         let ot1$(03)='5,30,n 6,ut,n'
47100 !         let ot1$(04)='6,30,n 11.2,ut,n'
47200 !         let ot1$(05)='7,30,c 20,ut,n'
47300 !         let ot1$(06)='8,30,n 11.2,ut,n'
47400 !         let ot1$(07)='9,34,n 6,ut,n'
47500 !         let ot1$(08)='12,24,n 6,ut,n'
47600 !         let ot1$(09)='12,40,n 11.2,ut,n'
47700 !         let ot1$(10)='13,24,n 6,ut,n'
47800 !         let ot1$(11)='13,40,n 11.2,ut,n'
47900 !         let ot1$(12)='14,24,n 6,ut,n'
48000 !         let ot1$(13)='14,40,n 11.2,ut,n'
48100 !         let ot1$(14)='15,24,n 6,ut,n'
48200 !         let ot1$(15)='15,40,n 11.2,ut,n'
48300 !         let ot1$(16)='16,24,n 6,ut,n'
48400 !         let ot1$(17)='16,40,n 11.2,ut,n'
48500 !         let ot1$(18)='17,24,n 6,ut,n'
48600 !         let ot1$(19)='17,40,n 11.2,ut,n'
48700 !         let ot1$(20)='18,24,n 6,ut,n'
48800 !         let ot1$(21)='18,40,n 11.2,ut,n'
48900 !         let ot1$(22)='19,24,n 6,ut,n'
49000 !         let ot1$(23)='19,40,n 11.2,ut,n'
49100 !         let ot1$(24)='20,24,n 6,ut,n'
49200 !         let ot1$(25)='20,40,n 11.2,ut,n'
49300 !         let ot1$(26)='21,24,n 6,ut,n'
49400 !         let ot1$(27)='21,40,n 11.2,ut,n'
49500 !      !
49600 !         let fli1$(01)='3,30,C 5,ut,n'
49700 !         let fli1$(02)='4,30,c 12,cu,n'
49800 !         let fli1$(03)='5,30,n 6,ut,n'
49900 !         let fli1$(04)='6,30,n 11.2,ut,n'
50000 !         let fli1$(05)='7,30,c 20,ut,n'
50100 !         let fli1$(06)='8,30,n 11.2,ut,n'
50200 !         let fli1$(07)='9,34,n 6,ut,n'
50300 !         let fli1$(08)='12,24,n 6,ut,n'
50400 !         let fli1$(09)='12,40,n 11.2,ut,n'
50500 !         let fli1$(10)='13,24,n 6,ut,n'
50600 !         let fli1$(11)='13,40,n 11.2,ut,n'
50700 !         let fli1$(12)='14,24,n 6,ut,n'
50800 !         let fli1$(13)='14,40,n 11.2,ut,n'
50900 !         let fli1$(14)='15,24,n 6,ut,n'
51000 !         let fli1$(15)='15,40,n 11.2,ut,n'
51100 !         let fli1$(16)='16,24,n 6,ut,n'
51200 !         let fli1$(17)='16,40,n 11.2,ut,n'
51300 !         let fli1$(18)='17,24,n 6,ut,n'
51400 !         let fli1$(19)='17,40,n 11.2,ut,n'
51500 !         let fli1$(20)='18,24,n 6,ut,n'
51600 !         let fli1$(21)='18,40,n 11.2,ut,n'
51700 !         let fli1$(22)='19,24,n 6,ut,n'
51800 !         let fli1$(23)='19,40,n 11.2,ut,n'
51900 !         let fli1$(24)='20,24,n 6,ut,n'
52000 !         let fli1$(25)='20,40,n 11.2,ut,n'
52100 !         let fli1$(26)='21,24,n 6,ut,n'
52200 !         let fli1$(27)='21,40,n 11.2,ut,n'
52300 !      !
52400 !         let sc2$(1)='Client #'
52500 !         let sc2$(2)='Invoice #'
52600 !         let sc2$(3)='Date'
52700 !         let sc2$(4)='Amount'
52800 !         let sc2$(5)='Description'
52900 !         let sc2$(6)='Cost Of Goods'
53000 !         let sc2$(7)=''
53100 !         let sc2$(8)='G/L Account #'
53200 !         let sc2$(9)='Amount'
53300 !        else 
53400     open #1: "Name=S:\acsTM\TMSCRN.CL,Shr",internal,input,relative ioerr ERR_FILE
53500     read #1,using 'form pos 1,c 255,142*c 18',rec=sz: ioerr ERR_FILE
53600     close #1: 
53700 !        end if
53800   fnend 
53900   def fn_get_next_line(&line$)
54000     dim gnl_block$*512
54100     dim gnl_buffer$*32767
54200     do until pos(gnl_buffer$,lf$)>0 or gnl_eof
54300       let gnl_block$=''
54400       read #h_in,using 'form pos 1,C 100': gnl_block$ ioerr GNL_H_IN_READ_IOERR
54500       let gnl_buffer$=gnl_buffer$&gnl_block$
54600     loop 
54700     let pos_crlf=pos(gnl_buffer$,lf$)
54800     let line$=gnl_buffer$(1:pos_crlf)
54900     let gnl_buffer$(1:pos_crlf+1)=''
55000 ! line$=srep$(line$,cr$,'^') : line$=srep$(line$,lf$,'~')
55100 ! pr 'line='&line$ : pause
55200     goto GNL_XIT
55300 GNL_H_IN_READ_IOERR: ! 
55400     let gnl_block$=gnl_block$&lf$
55500     let gnl_eof=1
55600     continue  ! gnl_h_in_read_ioerr
55700 GNL_XIT: ! 
55800   fnend  ! fn_get_next_line
55900   def fn_import_it(file_import$*256)
56000     open #h_in:=fngethandle: 'Name='&file_import$&',RecL=100,Shr',external,input 
56100     let fnopenprn
56200     pr #255,using FORM_PRN_HEAD: 'date','client','time','cat','month','desc','rate'
56300 FORM_OUT: form pos 1,n 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,c 12,pd 3,c 30
56400 FORM_PRN: form pos 1,pic(####d##d##),x 1,n 10.2,x 2,c 40,6*(skip 1,x 10,c 80)
56500 FORM_PRN_HEAD: form pos 1,cc 8,x 1,5*cr 10,x 1,c 30,cr 7
56600 ! r: headings
56700     let fn_get_next_line(line$) : let line_count+=1
56800     let str2mat(line$,mat item$,',',"QUOTES:TRIM")
56900     csv_date=srch(mat item$,"Date")
57000     csv_desc=srch(mat item$,"Description")
57100     csv_odesc=srch(mat item$,"Original Description")
57200     csv_amt=srch(mat item$,"Amount")
57300     csv_type=srch(mat item$,"Transaction Type")
57400     csv_cat=srch(mat item$,"Category")
57500     csv_acct=srch(mat item$,"Account Name")
57600     csv_labels=srch(mat item$,"Labels")
57700     csv_notes=srch(mat item$,"Notes"&lf$) ! pr csv_notes : pause
57800 ! /r
57900     do 
58000       let fn_get_next_line(line$) : let line_count+=1
58100       if line$<>'' then 
58200         let str2mat(line$,mat item$,',',"QUOTES:TRIM")
58300         if item$(csv_date)<>'' then let the_date=fn_get_the_date(item$(csv_date))
58400         if the_date=>filter_date(1) and the_date<=filter_date(2) then 
58500           pr the_date,val(item$(csv_amt)),item$(csv_desc),item$(csv_odesc)
58600 !       r: translate from csv to acs tranaction thing
58700           if item$(csv_type)='credit' then 
58800             let transaction_type=2 ! deposit
58900           else if item$(csv_type)='debit' then 
59000             let transaction_type=1 ! check
59100           else 
59200             pr 'unhandled transaction_type!  item$(csv_type)='&item$(csv_type)
59300             pause 
59400           end if 
59500           if item$(csv_acct)='Free Business Checking' then 
59600             bank_account=1
59700           else 
59800             pr 'unhan bank account!  item$(csv_acct)='&item$(csv_acct)
59900             pause 
60000           end if 
60300 !       /r
60400 !    let fn_write_out(the_date,val(item$(4)),val(item$(7)),val(item$(9)),val(item$(10)),item$(11)(1:30))
60500           pr #255,using FORM_PRN: the_date,val(item$(csv_amt)),item$(csv_type),item$(csv_desc),item$(csv_odesc),item$(csv_cat),item$(csv_acct),item$(csv_labels),item$(csv_notes)
60900         end if  ! the_date=>filter_date(1) and <=filter_date(2)
61000       end if  ! line$<>''
61100     loop until line$=''
61200 THE_END: ! 
61300     close #h_in: 
61400     let fncloseprn
61500   fnend 
61600   def fn_get_the_date(the_date$)
61700     let the_date=date(days(the_date$,'m/dd/ccyy'),'ccyymmdd')
61800     let fn_get_the_date=the_date
61900   fnend 
62000   def fn_write_out(wo_date,wo_client,wo_time,wo_cat,wo_month,wo_desc$*30)
62100     dim des$*30,inp(7)
62200     let inp(1)=wo_client
62300     let inp(2)=1 ! employee number
62400     let inp(3)=wo_time
62500     let inp(4)=150.00 ! hourly rate
62600     let inp(5)=wo_time*inp(4)
62700     let inp(6)=date(days(wo_date,'ccyymmdd'),'mmddyy') ! mmddyy
62800     let inp(7)=wo_cat
62900     b6=0 ! ???
63000     b7=1 ! ???
63100     b8=wo_month
63200     if wo_cat=6 then 
63300       let sc=601
63400     else if wo_cat=2 then 
63500       let sc=201
63600     else if wo_cat=11 then 
63700       let sc=1101
63800     else if wo_cat=23 then 
63900       let sc=2300
64000     else 
64100       pr #255: '!!! wo_cat ('&str$(wo_cat)&') is unrecognized - enhance code'
64200 !   pr 'wo_cat (';wo_cat;') is unrecognized - enhance code' : pause
64300     end if 
64400     write #h_out,using FORM_OUT: mat inp,b6,b7,b8,sc,'',0,wo_desc$
64500   fnend  ! fn_write_out
