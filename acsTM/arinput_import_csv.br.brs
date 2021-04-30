! REPLACE S:\acsTM\arinput_import_csv.br
autoLibrary
fnTop(program$,cap$="Import Transactions from Mint CSV to CL")
dim cr$*1,lf$*1,crlf$*2,line$*2048,item$(1)*1024
cr$=chr$(13) : lf$=chr$(10)
crlf$=cr$&lf$
dim fl1$(7),flo1$(11),sc3$(5),pt(6),f3$*255,flo3$(6),name$*25,cap$*128
dim p$*5,iv$*12,tr(6),id$*20,sc1$(5),sc2$(9),hd$(2)*50
dim flo4$(5),sc4$(5),ot4$(5),fli4$(5),q(3),gln1(3),gln2(3),otgl$(3)
dim gl(10,4),fli1$(49),ot1$(49),pgl(3)
fn_get_old_setup
open #h_clmstr:=9: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,input,keyed ioerr ERR_FILE
open #11: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr",internal,input,keyed ioerr ERR_FILE
open #h_addr:=3: "Name=[Temp]\Addr."&session$&",RecL=239,Replace",internal,outIn,relative ioerr ERR_FILE
SCREEN_1: ! r:
! exec 'config dimonly'
	dim file_import$*256,filter_date(2)
	dim label$(3)*40,resp$(3)*256
	label$(1)='Starting Date:'
	label$(2)='Ending Date:'
	label$(3)='Mint Transaction CSV File To Import:'
	filter_date(1)=20120101
	filter_date(2)=20121231
	file_import$=env$('userprofile')&'\Downloads\transactions.csv'
	fnTos(sn$="ask_fdd"&str$(udim(mat label$))&'_dates')
	respc=0 : ad_line=0 : col1_len=36 : col2_pos=col1_len+2
	fnLbl(ad_line+=1,1,label$(ad_line),col1_len,align_right:=1)
	fnTxt(ad_line,col2_pos,8,0,1,"3")
	resp$(respc+=1)=str$(filter_date(ad_line))
	fnLbl(ad_line+=1,1,label$(ad_line),col1_len,align_right)
	fnTxt(ad_line,col2_pos,8,0,1,"3")
	resp$(respc+=1)=str$(filter_date(ad_line))
	fnLbl(ad_line+=1,1,label$(ad_line),col1_len,align_right)
	fnTxt(ad_line,col2_pos,40,256,1,"70")
	resp$(respc+=1)=file_import$
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		fkey(99)
	else
		filter_date(1)=val(srep$(resp$(1),'/',''))
		filter_date(2)=val(srep$(resp$(2),'/',''))
		file_import$=resp$(3)
	end if
	fn_import_it(file_import$)
	end  ! pr newpage
! pr f mat fl1$: mat sc1$,"A/R Input Selection Menu","Selection:"
L630: !
!
	input fields "13,29,n 1,eu,n": transaction_type conv L630
	if transaction_type=0 then vf=1 : goto SCREEN_PROOF_TOTALS
	if transaction_type<1 or transaction_type>4 then goto L630
! /r
SCREENS_TRANS_ENTRY_A: ! r: old
	if transaction_type=4 or transaction_type=3 then sc2$(7)="G/L # to Credit" else sc2$(7)="G/L # to Debit"
	if transaction_type=3 then sc2$(6)="Discount Amount" else sc2$(6)=""
	if gx=0 then sc2$(7)=" "
SCREENS_TRANS_ENTRY_B: !
	pr newpage
	pr f mat flo1$: mat sc2$,"A/R Input "&sc1$(transaction_type+1)(5:18),"Client Number as 0 to stop"
	ps1=0
	if vf=0 then goto L790
	if gx><0 then goto L780
L760: !
	pr f mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2)
	goto L790
L780: !
	pr f mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl
L790: !
	pr f "5,30,pic(zzzzzz)": tr(1)
	pr f "24,20,C 50,N": "F1 Continue   F2 verify name    F4 Search"
	if gx><0 then goto L910
L820: !
	input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2) conv L870
	if cmdkey=4 then let fn_tmsrch : goto L760
	p$=uprc$(lpad$(rtrm$(p$),5))
	if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
	ce=0
	goto L1280
L870: !
	if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
	ce=cnt+1
	fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,"U","RC")
	goto L820
L910: !
	if ps1=1 or vf=1 then goto L1060
L920: !
	rinput fields "3,30,C 5,EU,n": p$ conv L920
	if cmdkey=4 then let fn_tmsrch : goto L920
	p$=uprc$(lpad$(rtrm$(p$),5))
	if ltrm$(p$)="-1" then pr f mat otgl$: mat gln1 else pr f mat otgl$: mat gln2
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto SCREEN_1
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto L1630
	if ltrm$(p$)="-1" then name$="CASH SALE" else goto L990
	goto L1050
L990: !
	read #h_clmstr,using 'form pos 6,c 25',key=p$,release: name$ nokey L1020 ioerr ERR_FILE
	goto L1050
L1020: !
	name$="INVALID CLIENT NUMBER"
	pr f "3,40,C 25,R,N": name$
	goto L920
L1050: !
	pr f "3,40,C 25,N": name$
L1060: !
	fli1$(4)="6,30,n 11.2,ut,n"
	if r1>0 then goto L1180
	if transaction_type=3 then fli1$(4)="6,30,n 11.2,ue,n"
	input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl conv L1240
	if cmdkey=2 then goto L920
	if transaction_type<>3 then goto L1200
	fli1$(4)="6,30,n 11.2,ut,n"
	if sz=4 then gl(1,2)=gln1(2): gl(1,1)=gln1(1): gl(1,3)=tr(3)
	if sz=3 then gl(1,1)=gln1(2): gl(1,2)=gln1(3): gl(1,3)=tr(3)
	if sz=2 then gl(1,2)=gln1(2): gl(1,1)=gln1(1): gl(1,3)=gln1(3): gl(1,4)=tr(3)
	if sz=5 then gl(1,1)=gln1(2): gl(1,2)=tr(3)
L1180: !
	rinput fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl conv L1240
	if cmdkey=2 then goto L920
L1200: !
	p$=uprc$(lpad$(rtrm$(p$),5))
	if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
	ce=0
	goto L1280
L1240: !
	if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
	ce=cnt+1
	fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,"U","RC")
	if cnt<=4 then goto L1060 else goto L1180
L1280: !
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto SCREEN_1
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto L1630
	ps1=1
	if tr(1)<10100 or tr(1)>123199 then goto L1320 else goto L1340
L1320: !
	pr f "5,48,c 20": "INVALID DATE"
	goto L790
L1340: !
	if tr(3)><0 then goto L1370
	pr f "6,48,c 20": "NO AMOUNT ENTERED"
	goto L790
L1370: !
	if gx=0 then goto L1520
	if pgl(gpx)>0 then goto L1410
	pr f "9,45,c 30": "G/L # REQUIRED"
	goto L790
L1410: !
	gla=0
	for j=1 to 10
		if gl(j,gx)=0 then goto L1460
		gla=gla+gl(j,gx)
	next j
L1460: !
	if transaction_type=3 then gla=gla-tr(2)
	if gla=tr(3) then goto L1520
	pr f "11,2,c 75,h,n": " G/L ALLOCATIONS DO NOT AGREE WITH TOTAL AMOUNT.  PRESS ENTER TO CONTINUE."
	input fields "11,78,c 1,EU,n": pause$
	pr f "11,2,c 75,n,n": " "
	goto L790
L1520: !
	if ltrm$(p$)="-1" then goto L1540
	pt(1)=pt(1)+val(p$) conv L1540
L1540: !
	pt(transaction_type+1)=pt(transaction_type+1)+tr(3)
	if transaction_type=3 then tdt=tdt+tr(2)
	if ltrm$(p$)="-1" then pt(6)=pt(6)+tr(3)
	if vf=1 then goto L1670
	r3=r3+1
	tr(5)=transaction_type
	write #h_addr,using f3$,rec=r3: p$,iv$,mat tr,id$,mat pgl,mat gl
	p$=""
	q2=0
	goto SCREENS_TRANS_ENTRY_B
L1630: !
	iv$=" "
	mat tr=(0)
	id$=" "
	mat gl=(0)
L1670: !
	rewrite #h_addr,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl
	p$=""
	goto SCREEN_ASK_REF_TO_FIX ! /r
SCREEN_PROOF_TOTALS: ! r: old
	pr newpage
	pr f mat fl1$: mat sc3$,"A/R Input Proof Totals",""
	pr f "11,5,C 20": "Total Cash Sales"
	pr f "12,5,C 22": "Total Discounts Taken"
	pr f mat flo3$: mat pt
	pr f "12,26,n 11.2": tdt
	pr f "18,1,C 70,H,N": "1=Merge; 2=Corrections; 3=Proof List: 5=Stop Without Posting"
L1790: !
	input fields "18,61,n 1,eu,n": j conv L1790
	if j=3 then let fn_print_proof_list : goto L1790
	if j=5 then goto Xit
	if j=1 then goto CHAIN_ARMERGE
	goto L1790
! /r
def fn_print_proof_list
	r=0
	fnopenprn
	pr newpage
	on fkey 5 goto L2040
	pr newpage
	pr #255,using L1910: date$,env$('cnam'),time$,"Input Edit List"
	L1910: form pos 1,c 8,pos namtab,c 50,skip 1,pos 1,c 8,pos 58,c 50
	pr f "10,20,C 40,N": "Input Edit Listing In Process"
	pr f "23,2,C 30,N": "Press F5 To Stop"
	pr #255: "Ref #  Cl #  Invoice #";
	pr #255: tab(34);"Date     Amount             Description           Discount          Tr Code"
	L1960: !
	r=r+1
	read #h_addr,using L2110,rec=r: p$,iv$,mat tr,id$ eof L2040,noRec L2040 ioerr ERR_FILE
	L2110: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20
	if ltrm$(p$)="0" or ltrm$(p$)="" then goto L1960
	name$=""
	read #h_clmstr,using 'form pos 6,c 25',key=p$,release: name$ nokey L2010
	L2010: !
	pr #255,using L2020: r,p$,iv$,tr(1),tr(3),tr(4),name$(1:22),tr(2),tr(5)
	L2020: form pos 1,n 4,x 2,c 5,x 2,c 18,n 6,n 11.2,pic(zzzzzz),x 7,c 22,n 12.2,n 12
	goto L1960
	L2040: !
	fncloseprn
	on fkey 5 ignore
fnend
SCREEN_ASK_REF_TO_FIX: ! r:
	pr newpage
	pr f "10,10,c 60": "Ref Number To Correct; (0 when Completed)"
L2080: input fields "10,61,n 4,eu,n": r1 conv L2080
	if r1=0 then goto SCREEN_ASK_ADD_MORE
	read #h_addr,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl noRec SCREEN_ASK_REF_TO_FIX ioerr ERR_FILE
	if ltrm$(p$)="0" or ltrm$(p$)="" then goto SCREEN_ASK_REF_TO_FIX
	transaction_type=tr(5)
	if p><-1 then pt(1)=pt(1)-val(p$) conv L2150
L2150: pt(transaction_type+1)=pt(transaction_type+1)-tr(3)
	if ltrm$(p$)="-1" then pt(6)=pt(6)-tr(3)
	if transaction_type=3 then tdt=tdt-tr(2)
	hd$(1)="A/R Correct "&sc1$(transaction_type+1)(5:18)
	hd$(2)="Enter Client # As 0 To Delete This Entry"
	vf=1
	goto SCREENS_TRANS_ENTRY_A
! /r
SCREEN_ASK_ADD_MORE: ! r:
	pr newpage
	vf=0
	pr f "10,10,c 50": "ENTER 1 TO MAKE ADDITIONAL ENTRIES; ELSE ENTER 2"
L2250: !
	input fields "10,61,N 1,EU,N": j conv L2250
	on j goto SCREEN_1,SCREEN_PROOF_TOTALS none L2250
! /r
CHAIN_ARMERGE: chain "S:\acsTM\ARMerge"
Xit: !
	fnXit
ERR_FILE: ! r:
	if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2310
	goto L2350
L2310: pr newpage
	if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2340
	goto L2350
L2340: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
L2350: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
	input fields "24,60,C 1,N": quitcode$
	if err=61 and rtrm$(uprc$(quitcode$))="Q" then goto SCREEN_1 else goto L2410
	pr f "23,3,C 78,N": ""
	pr f "24,3,C 78,N": ""
	retry
L2410: goto Xit
! /r
	def fn_tmsrch !  search for customer #
		dim heading$*70,form$*80,numeric_format$*20,selection$*70
		file_num=11 ! alpha index on clients
		form$="form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2"
		numeric_format$='pic($$$,$$$.##)'
		key_length=5
		heading$="Acct #-Name--------------------Address--------Balance"
		fnsearch(cap$,file_num,heading$,form$,numeric_format$,selection$,key_length)
		p$=selection$ ! pull key from first field in search line
		ano=0
		ano=val(selection$) conv L4910
		L4910: !
	fnend
def fn_get_old_setup
	open #h_company:=1: "Name=S:\Core\Data\acsllc\Company.h[cno],Shr",internal,input ioerr ERR_FILE
	read #h_company,using L130: i3,i4,i5,mat gln1,mat gln2 ioerr ERR_FILE
	! i3=1 ! ENTER G/L #'S
	L130: form pos 161,3*n 1,pos 178,n 3,n 6,n 3,n 3,n 6,n 3
	close #h_company:
	namtab=66-int(len(rtrm$(env$('cnam')))/2)
	otgl$(1)="9,30,pic(zzz)"
	otgl$(2)="9,34,pic(zzzzzz)"
	otgl$(3)="9,41,pic(zzz)"
	if i3=0 then goto L490
	if i4=1 and i5=1 then goto L300
	if i4=0 and i5=1 then goto L350
	if i4=1 and i5=0 then goto L420
	! NO DEPT    NO SUBACCOUNT
	sz=5
	gx=2
	mat gl(10,2)=(0)
	mat pgl(1)=(0)
	gpx=1
	goto L510
	L300: ! YES DEPT   YES SUBACCOUNT
	sz=2
	gx=4
	gpx=2
	goto L510
	L350: ! NO DEPT    YES SUBACCOUNT
	sz=3
	gx=3
	mat gl(10,3)=(0)
	mat pgl(2)=(0)
	gpx=1
	goto L510
	L420: ! YES DEPT    NO SUB ACCOUNT
	sz=4
	gx=3
	mat gl(10,3)=(0)
	mat pgl(2)=(0)
	gpx=2
	goto L510
	L490: ! NO GL TO BE ENTERED
	sz=6
	L510: !
	open #1: "Name=S:\acsTM\TMSCRN.CL,Shr",internal,input,relative ioerr ERR_FILE
	read #1,using 'form pos 1,c 255,142*c 18',rec=sz: ioerr ERR_FILE
	close #1:
fnend
def fn_get_next_line(&line$)
	dim gnl_block$*512
	dim gnl_buffer$*32767
	do until pos(gnl_buffer$,lf$)>0 or gnl_eof
		gnl_block$=''
		read #h_in,using 'form pos 1,C 100': gnl_block$ ioerr GNL_H_IN_READ_IOERR
		gnl_buffer$=gnl_buffer$&gnl_block$
	loop
	pos_crlf=pos(gnl_buffer$,lf$)
	line$=gnl_buffer$(1:pos_crlf)
	gnl_buffer$(1:pos_crlf+1)=''
	! line$=srep$(line$,cr$,'^') : line$=srep$(line$,lf$,'~')
	! pr 'line='&line$ : pause
	goto GNL_XIT
	GNL_H_IN_READ_IOERR: !
		gnl_block$=gnl_block$&lf$
		gnl_eof=1
	continue  ! gnl_h_in_read_ioerr
	GNL_XIT: !
fnend  ! fn_get_next_line
def fn_import_it(file_import$*256)
	open #h_in=fnH: 'Name='&file_import$&',RecL=100,Shr',external,input
	fnopenprn
	pr #255,using FORM_PRN_HEAD: 'date','client','time','cat','month','desc','rate'
	FORM_OUT: form pos 1,n 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,c 12,pd 3,c 30
	FORM_PRN: form pos 1,pic(####d##d##),x 1,n 10.2,x 2,c 40,6*(skip 1,x 10,c 80)
	FORM_PRN_HEAD: form pos 1,cc 8,x 1,5*cr 10,x 1,c 30,cr 7
	! r: headings
	fn_get_next_line(line$) : line_count+=1
	str2mat(line$,mat item$,',',"QUOTES:TRIM")
	csv_date=srch(mat item$,"Date")
	csv_desc=srch(mat item$,"Description")
	csv_odesc=srch(mat item$,"Original Description")
	csv_amt=srch(mat item$,"Amount")
	csv_type=srch(mat item$,"Transaction Type")
	csv_cat=srch(mat item$,"Category")
	csv_acct=srch(mat item$,"Account Name")
	csv_labels=srch(mat item$,"Labels")
	csv_notes=srch(mat item$,"Notes"&lf$) ! pr csv_notes : pause
	! /r
	do
		fn_get_next_line(line$) : line_count+=1
		if line$<>'' then
			str2mat(line$,mat item$,',',"QUOTES:TRIM")
			if item$(csv_date)<>'' then the_date=fn_get_the_date(item$(csv_date))
			if the_date=>filter_date(1) and the_date<=filter_date(2) then
				pr the_date,val(item$(csv_amt)),item$(csv_desc),item$(csv_odesc)
				! r: translate from csv to acs tranaction thing
				if item$(csv_type)='credit' then
					transaction_type=2 ! deposit
				else if item$(csv_type)='debit' then
					transaction_type=1 ! check
				else
					pr 'unhandled transaction_type!  item$(csv_type)='&item$(csv_type)
					pause
				end if
				if item$(csv_acct)='Free Business Checking' then
					bank_account=1
				else
					pr 'unhan bank account!  item$(csv_acct)='&item$(csv_acct)
					pause
				end if
				! /r
				pr #255,using FORM_PRN: the_date,val(item$(csv_amt)),item$(csv_type),item$(csv_desc),item$(csv_odesc),item$(csv_cat),item$(csv_acct),item$(csv_labels),item$(csv_notes)
			end if  ! the_date=>filter_date(1) and <=filter_date(2)
		end if  ! line$<>''
	loop until line$=''
	THE_END: !
	close #h_in:
	fncloseprn
fnend
def fn_get_the_date(the_date$)
	the_date=date(days(the_date$,'m/dd/ccyy'),'ccyymmdd')
	fn_get_the_date=the_date
fnend
def fn_write_out(wo_date,wo_client,wo_time,wo_cat,wo_month,wo_desc$*30)
	dim des$*30,inp(7)
	inp(1)=wo_client
	inp(2)=1 ! employee number
	inp(3)=wo_time
	inp(4)=150.00 ! hourly rate
	inp(5)=wo_time*inp(4)
	inp(6)=date(days(wo_date,'ccyymmdd'),'mmddyy') ! mmddyy
	inp(7)=wo_cat
	b6=0 ! ???
	b7=1 ! ???
	b8=wo_month
	if wo_cat=6 then
		sc=601
	else if wo_cat=2 then
		sc=201
	else if wo_cat=11 then
		sc=1101
	else if wo_cat=23 then
		sc=2300
	else
		pr #255: '!!! wo_cat ('&str$(wo_cat)&') is unrecognized - enhance code'
	end if
	write #h_out,using FORM_OUT: mat inp,b6,b7,b8,sc,'',0,wo_desc$
fnend  ! fn_write_out
