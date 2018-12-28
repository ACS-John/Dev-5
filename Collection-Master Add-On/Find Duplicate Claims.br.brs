fn_setup
fntop(program$)
dim Selmenu$(0)*128
fnAddOneC(mat Selmenu$,'Duplicate Forwarder+Forwarder File Number')
fnAddOneC(mat Selmenu$,'Duplicate Forwarder+Infinity.*TrakFNo')
pk$='A-F'

 fnget_form("Master",mat master_data$,mat master_data,mat master_fieldsc$,mat master_fieldsn$,master_formc$,master_formn$)
 fnunpack$(master_formc$,master_formn$)
 master_formall$=fnget_formall$
 gosub DEFINE_MASTER
 open #main_read:=fngethandle: 'name=master//6,kfname=masterx//6,shr',internal,outin,keyed
 open #h_internal:=fngethandle: "NAME=INTERNAL//6,KFNAME=INTERNAL.IDX//6,SHR",internal,outin,keyed


choice=fnMenu(env$('program_caption'),Mat Selmenu$, pk$,'John Bowman Services LLC') ! ,0,1) ! ; &Pk$, Title$*80, Footer$*80, Nnp, Autonumber,Mstart,Mat Custom_Menubar$, Mat Custom_Menuprg$, Mat Custom_Menustatus$,&Menu_Startup$,Menu_Offset,M_Timeout,M_Trust$*30,M_F93_Enable)


if choice=1 then 
 gosub FFFN
else if choice=2 then
 gosub FFIT
end if
goto Xit



dim dupeSearchValue$(0)*128
dim reportFile$*256
FFFN: ! r:
	delim$=','
	reportFile$='n:CM-Duplicates-ForwarderFileNo.csv'
	if env$('client_server')='Yes' then
		reportFile$(0:0)='@:'
	end if
	limitRecordsProcessed=0 ! 50000
	timeStart=fnStime(time$)
	Fffn_AskFile: !
	open #hOut:=fngethandle: "Name=SAVE:"&reportFile$&",RecL=1024,replace",display,output ioerr SAVE_AS_OPEN_ERR
	reportFile$=os_filename$(file$(hOut))
	Fnlist_Print('creating '&reportFile$, 0,1,'Status') ! env$('program_caption'))
	pr #hOut: 'FileNo'&delim$&'Matches'&delim$&'Forwarder'&delim$&'Forwarder File Number'

	! MAIN LOOP
	mat fileno$(0)
	mat dupeSearchValue$(0)


	readCount=0
	dupeCount=0
	Fnlist_Print('gathering data from Open Claims', 0,1)
	if limitRecordsProcessed>0 then 
		lastRecord=limitRecordsProcessed
	else 
		lastRecord=lrec(main_read)
	end if
	for mRec=1 to lastRecord
		! read #1,using master_formall$,key=tfileno$,release: mat tmaster_data$,mat tmaster_data nokey 55370
		read #main_read,using master_formall$,rec=mRec: mat master_data$,mat master_data noRec Fffn_NextMasterRecord
		fncom(mRec,lastRecord,6)
		fnfix_bh(mat master_data)
		! read_recno=rec( main_read)
		! fnmast2_int_cache(mat master_data$,mat master_data,mat master_fieldsc$,mat master_fieldsn$,mat extra_data$,mat extra_data,mat extra_fieldsc$,mat extra_fieldsn$)
		master_data$(master_docket_no)=fnget_docket$(master_data$(1))
		master_data$(master_forw_refno)=fnget_edi_refno$(master_data$(1))
		master_data$(master_jmt_no)=fnget_jmt_no$(master_data$(1))
		master_data$(master_forw_fileno)=fnget_fofile$(master_data$(1))
		
		readCount+=1
		fnAddOneC(mat fileno$,master_data$(master_fileno))
		fnAddOneC(mat dupeSearchValue$,trim$(str$(master_data(master_FORW_NO)))&'.'&trim$(master_data$(master_FORW_FILENO)))
		Fffn_NextMasterRecord: !
	next mRec
	close #main_read:
	Fnlist_Print('analyzing '&str$(udim(mat dupeSearchValue$))&' records for duplicates...', 0,1)
	for fffn_mItem=1 to udim(mat fileno$)
		fncom(fffn_mItem,udim(mat fileno$),8)
		howMany=fnCountMatchesC(mat dupeSearchValue$,dupeSearchValue$(fffn_mItem))
		if howMany>1 and dupeSearchValue$(fffn_mItem)(len(dupeSearchValue$(fffn_mItem)):len(dupeSearchValue$(fffn_mItem)))<>'.' then 
			pr #hOut: fileno$(fffn_mItem)&delim$;
			pr #hOut: str$(howMany-1)&delim$;
			pr #hOut: srep$(dupeSearchValue$(fffn_mItem),'.',delim$)
			dupeCount+=1
		end if
	nex fffn_mItem
return ! /r



FFIT: ! r:
	delim$=','
	reportFile$='n:CM-Duplicates-TrakFNo.csv'
	if env$('client_server')='Yes' then
		reportFile$(0:0)='@:'
	end if
	limitRecordsProcessed=0 ! 50000
	timeStart=fnStime(time$)
	!open #hOut:=fngethandle: 'Name='&reportFile$&',RecL=1024,Replace',display,output 
	open #hInf:=fngethandle: "name=infinity.int//6,kfname=infinity.idx//6,shr",internal,outin,keyed
	Ffit_AskFile: !
	open #hOut:=fngethandle: "Name=SAVE:"&reportFile$&",RecL=1024,replace",display,output ioerr SAVE_AS_OPEN_ERR
	reportFile$=os_filename$(file$(hOut))
	Fnlist_Print('creating '&reportFile$, 0,1,'Status') ! env$('program_caption'))
	pr #hOut: 'FileNo'&delim$&'Matches'&delim$&'Forwarder'&delim$&'*TrakNo'

	mat fileno$(0)
	mat dupeSearchValue$(0)
	dim trakFno$*256


	readCount=0
	dupeCount=0
	Fnlist_Print('gathering data from Open Claims', 0,1)
	if limitRecordsProcessed>0 then 
		lastRecord=limitRecordsProcessed
	else 
		lastRecord=lrec(main_read)
	end if
	for mRec=1 to lastRecord
		! read #1,using master_formall$,key=tfileno$,release: mat tmaster_data$,mat tmaster_data nokey 55370
		read #main_read,using master_formall$,rec=mRec: mat master_data$,mat master_data norec Ffit_NextMasterRecord
		fncom(mRec,lastRecord,6)
		fnfix_bh(mat master_data)
		master_data$(master_docket_no)=fnget_docket$(master_data$(1))
		master_data$(master_forw_refno)=fnget_edi_refno$(master_data$(1))
		master_data$(master_jmt_no)=fnget_jmt_no$(master_data$(1))
		master_data$(master_forw_fileno)=fnget_fofile$(master_data$(1))
		fngetinf$(hInf,master_data$(master_fileno)&'*TrakFNo',inf_value,trakFno$)
		readCount+=1
		fnAddOneC(mat fileno$,master_data$(master_fileno))
		fnAddOneC(mat dupeSearchValue$,trim$(str$(master_data(master_FORW_NO)))&'.'&trim$(trakFno$))
		Ffit_NextMasterRecord: !
	next mRec
	close #main_read:
	Fnlist_Print('analyzing '&str$(udim(mat dupeSearchValue$))&' records for duplicates...', 0,1)
	for mItem=1 to udim(mat fileno$)
		fncom(mItem,udim(mat fileno$),8)
		howMany=fnCountMatchesC(mat dupeSearchValue$,dupeSearchValue$(mItem))
		if howMany>1 and dupeSearchValue$(mItem)(len(dupeSearchValue$(mItem)):len(dupeSearchValue$(mItem)))<>'.' then 
			pr #hOut: fileno$(mItem)&delim$;
			pr #hOut: str$(howMany-1)&delim$;
			pr #hOut: srep$(dupeSearchValue$(mItem),'.',delim$)
			dupeCount+=1
		end if
	nex mItem
	close #hInf:
return ! /r



Xit: !
close #hOut:
close #h_internal:
timeStop=fnStime(time$)
dim message$*2048
message$='readCount='&str$(readCount)&'\n'
message$(inf:inf)='dupeCount='&str$(dupeCount)&'\n'
message$(inf:inf)='elapsed time='&fnStime$(timeStop-timeStart)&'\n'
message$(inf:inf)='Output File Created:\n'
message$(inf:inf)=reportFile$
fnMessageBox(message$, Mbx_Type,env$('program_caption'))
execute 'Proc=Run'
SAVE_AS_OPEN_ERR: ! r: there was a problem opening the file.
 if err<>622 then
  dim ml$(0)*256
  mat ml$(2)
  ml$(1)='Select a different file name.'
  ml$(2)='Error: '&str$(err)
  fnmsgbox(mat ml$)
  pr "Err:";err;" Line:";line
  if choice=1 then 
   goto Fffn_AskFile
  else if choice=2 then
   goto Ffit_AskFile
  else
   pr 'dont know where to go.'
   pause
  end if
 else
  goto Ertn
 end if 
! /r
! def fnsql_main_read(fileno$)
!   mat master_data$=("")
!   mat master_data=(0)
!   let sql_main_read=0
!   let fileno$=rpad$(fileno$,8)(1:8)
!   read #main_read,using master_formall$,key=fileno$,release: mat master_data$,mat master_data nokey 17330
!   let sql_main_read=1
!   let fnsql_main_read=sql_main_read
! fnend
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library.br': fngethandle
		library 'S:\Core\Library.br': fnCountMatchesC
		library 'S:\Core\Library.br': fnMsgBox
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fntop

		library "library\CLSUtil.wb": fnGetInf$
		library "library\CLSUtil.wb": fncom
		library "library\CLSUtil.wb": fnget_formall$,fnget_formarr
		library "library\CLSUtil.wb": fnreport_path$,fnclaim_path$
		library "library\CLSUtil.wb": fnget_claimfiles,fnclaim_scroll
		library "library\CLSUtil.wb": fnrange_to_array,fnarray_to_range$
		library "library\CLSUtil.wb": fnfix_bh,fnask_payref
		library "library\CLSUtil.wb": fnget_form
		library "library\CLSUtil.wb": fnunpack$
		library "library\CLSUtil.wb": fnStime,fnStime$
		library "library\CLSUtil.wb": fnMessageBox
		library "library\CLSUtil.wb": Fnlist_Print
		library "Prog2\Mast2.wb": fnsql_read

		library "library\CLSUtil.wb": fnfix_bh
		library "Prog2\Mast_SQL.wb": fnmast2_int_cache
		library "library\CLSUtil.wb": fnAsk_file1
		! LIBRARY "Toolbar/Library": Fnsubmenu
		library "library\CLSUtil.wb": fnmenu

		! exe 'con sub [acsPath] C:\Brumbaugh\clsinc'
		! if env$('status.files.drives.[s]')='' then 
		!  exe 'con Drive S,F:,X,\'
		!  pause
		! end if

		dim master_data$(1)*60,master_data(1)
		dim master_fieldsc$(1)*20,master_fieldsn$(1)*20,master_formc$*1024,master_formn$*1024
		dim master_before$(1)*60,master_before(1)
		dim tmaster_data(1),tmaster_data$(1)
		dim master_before_data$(1)*60,master_formall$*2048
	end if
fnend
DEFINE_MASTER: ! r:
	master_fileno=1 : master_date_recd=2 : master_forw_file_12=3 : master_law_list=4 : master_comm=5 : master_sfee=6 : master_jmt_no=7 : master_d1_name=8 : master_d1_street=9 : master_d1_cs=10 : master_d1_zip=11 : master_d1_phone=12 : master_cred_name=13 : master_cred_xline=14 : master_filler_254=15 : master_date_debt=16 : master_suit_date=17 : master_jmt_date=18 : master_docket_no=19 : master_stat1_date=20 : master_stat1_code=21 : master_plaintiff_1=22 : master_plaintiff_2=23 : master_defendant_1=24 : master_defendant_2=25
	master_defendant_3=26 : master_defendant_4=27 : master_d2_name=28 : master_d2_street=29 : master_d2_csz=30 : master_d3_name=31 : master_d3_street=32 : master_d3_csz=33 : master_misc_asset1=34 : master_misc_asset2=35 : master_misc_asset3=36 : master_d1_ssn=37 : master_filler_730=38 : master_stat2_date=39 : master_stat2_code=40 : master_bank_acct_no=41 : master_int_date=42 : master_closed_yy=43 : master_closed_mm=44 : master_closed_code1=45 : master_closed_code2=46 : master_filler_adva=47 : master_ll_date=48 : master_ll_code4=49
	master_coco_fileno=50 : master_filler_1040=51 : master_filler_1049=52 : master_d1_alias=53 : master_filler_cred_xline=54 : master_filler_1100=55 : master_forw_comm=56 : master_forw_sfee=57 : master_coco_comm=58 : master_coco_sfee=59 : master_rfile_no=60 : master_ll_code8=61 : master_lpaymnt_date=62 : master_forw_fileno=63 : master_d1_fax=64 : master_no_debtors=65 : master_link_primary=66 : master_d2_phone=67 : master_d3_phone=68 : master_asset_phone=69 : master_p_type=70 : master_p_dcode=71 : master_forw_refno=72 : master_b2_accno=73
	master_b3_accno=74 : master_emp1_acc=75 : master_emp2_acc=76 : master_emp3_acc=77 : master_d2_ssn=78 : master_d3_ssn=79 : master_c_priority=80 : master_ctype=81 : master_d2_adva_file=82 : master_d3_adva_file=83 : master_claim_time=84 : master_d1_dl=85 : master_d2_dl=86 : master_d3_dl=87 : master_d1_birth_date=88 : master_d2_birth_date=89 : master_d3_birth_date=90 : master_d1_returned=91 : master_d2_returned=92 : master_d3_returned=93 : master_cred_phone=94 : master_cred_fax=95 : master_cred_email=96 : master_filler_2048=97
	master_def_state=98 : master_collord=99 : master_edi_status_code=100 : master_edi_status_date=101 : master_cl_status_code=102 : master_cl_status_date=103 : master_closed_date=104 : master_filler_2375=105 : master_contract_flag=106 : master_jmt_flags=107 : master_cred_street=108 : master_cred_cs=109 : master_cred_zip=110 : master_jmt_type=111
	master_forw_no=1 : master_venue1_no=2 : master_pre_j_rate=3 : master_post_j_rate=4 : master_stored_int=5 : master_contract_fee=6 : master_orig_claim=7 : master_suit_amt=8 : master_stat_fee=9 : master_jmt_amt=10 : master_balance=11 : master_cost_bal=12 : master_coll_hold=13 : master_prin_coll=14 : master_int_coll=15 : master_merch_bef=16 : master_merch_post=17 : master_cash_bef=18 : master_cash_post=19 : master_cost_received=20 : master_cost_ret=21 : master_cost_exp=22 : master_cost_recovered=23 : master_comm_earn=24 : master_sfee_earn=25
	master_stat_earn=26 : master_sher_no=27 : master_atty=28 : master_para=29 : master_recs_01=30 : master_recs_02=31 : master_recs_19=32 : master_recs_03=33 : master_recs_04=34 : master_recs_05=35 : master_recs_20=36 : master_recs_06=37 : master_recs_07=38 : master_recs_08=39 : master_recs_21=40 : master_recs_09=41 : master_recs_10=42 : master_recs_11=43 : master_recs_22=44 : master_recs_12=45 : master_recs_13=46 : master_recs_14=47 : master_recs_23=48 : master_recs_15=49 : master_recs_16=50 : master_recs_17=51 : master_recs_24=52
	master_recs_18=53 : master_bank_no=54 : master_coco_no=55 : master_cost_post_judg=56 : master_affidavit=57 : master_paid_post_judg=58 : master_non_rcvrd_cost=59 : master_tot_exp_cost=60 : master_dp_pre_suit_nf=61 : master_dp_post_suit_nf=62 : master_dp_post_judg_nf=63 : master_merch_post_judg=64 : master_exp_cost=65 : master_dp_pre_suit=66 : master_dp_post_suit=67 : master_dp_post_judg=68 : master_accr_int_bef=69 : master_tax_rebate=70 : master_agent_befscoll=71 : master_agent_pscoll=72 : master_agent_pjcoll=73 : master_agent_cost=74
	master_sales_tax=75 : master_scode_pos=76 : master_lpaymnt_refno=77 : master_lpaymnt_amt=78 : master_client_no=79 : master_l_sfcode=80 : master_p_1st_date=81 : master_p_amort=82 : master_p_period=83 : master_p_first=84 : master_p_payment=85 : master_p_last=86 : master_p_amount=87 : master_emp_no=88 : master_c_adjust=89 : master_compr_amt=90 : master_overpay_amt=91 : master_bank2=92 : master_bank3=93 : master_emp2=94 : master_emp3=95 : master_collector=96 : master_d2_adva=97 : master_d3_adva=98 : master_sales_no=99 : master_coco_comm_earn=100
	master_coco_sfee_earn=101 : master_principal_coll=102 : master_contract_coll=103 : master_fees_coll=104 : master_charges_coll=105 : master_other_coll=106 : master_other_fees_coll=107 : master_pend_coll=108 : master_nr_cost_coll=109 : master_rec_costs_coll=110 : master_firm_costs_coll=111 : master_other_cost_coll=112 : master_frec37=113 : master_frec38=114 : master_frec39=115 : master_frec40=116 : master_opened_amt=117 : master_services_amt=118 : master_charge_off=119 : master_purchase_amt=120 : master_pre_paid_amt=121 : master_filler_1934=122
	master_filler_1940=123 : master_filler_1946=124 : master_filler_1958=125 : master_l_rpt_int_rate=126 : master_opened_date=127 : master_services_date=128 : master_charge_off_date=129 : master_purchase_date=130 : master_pre_paid_date=131 : master_jmt_exp_date=132 : master_filler_1988=133 : master_filler_1992=134 : master_filler_1996=135 : master_l_rpt_int_date=136 : master_statute_date=137 : master_rev_date=138 : master_rev_time=139 : master_rev_user=140 : master_edi_date=141 : master_edi_time=142 : master_edi_user=143 : master_batch_date=144
	master_batch_time=145 : master_batch_user=146 : master_batch_enet_no=147 : master_per_diem_date=148 : master_per_diem_amt=149 : master_per_diem_rate=150 : master_per_diem_base=151 : master_per_diem_int=152 : master_jmt_cost=153 : master_jmt_stat=154 : master_jmt_contract=155 : master_jmt_prin=156 : master_jmt_awarded=157 : master_exp_m_cost=158 : master_exp_d_cost=159 : master_exp_p_cost=160 : master_exp_r_cost=161 : master_wo_rec_costs=162 : master_wo_nr_costs=163 : master_wo_fees=164 : master_frec58=165 : master_frec59=166 : master_frec60=167
	master_frec61=168 : master_frec62=169 : master_frec63=170 : master_frec64=171 : master_frec65=172 : master_i_1100_ir_j=173 : master_i_3100_ir_j=174 : master_i_4100_ir_j=175 : master_i_5100_ir_j=176 : master_i_6100_ir_j=177 : master_i_9100_ir_j=178 : master_filler_ir_j_72=179 : master_filler_ir_j_73=180 : master_filler_ir_j_74=181 : master_filler_ir_j_75=182 : master_filler_ir_j_76=183 : master_filler_ir_j_77=184 : master_filler_ir_j_78=185 : master_filler_ir_j_79=186 : master_filler_ir_j_80=187 : master_adva=188 : master_filler_2350=189
	master_filler_2353=190 : master_pay_subcode=191 : master_p_entry_date=192 : master_filler_2363=193 : master_filler_2367=194 : master_filler_2371=195
return  ! /r


def fnget_docket$*30(docket_fileno$)
	dim docket_no$*30
	read #h_internal,using "FORM POS 27,C 30",key=docket_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",2)&"MAIN      ",release: docket_no$ nokey L17420 
	let fnget_docket$=docket_no$ 
	! if trim$(docket_no$)="" and trim$(master_data$(master_docket_no))<>"" then 
	!  let fnget_docket$=docket_no$=master_data$(master_docket_no) 
	!  let fnmessagebox("Warning, Internal Docket Number is missing\nUsing MASTER.DOCKET_NO instead\n"&docket_no$,64,"Problem with Docket/Case #") 
	!  rewrite #h_internal,using "FORM POS 27,C 30",key=docket_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",2)&"MAIN      ",release: docket_no$ nokey L17420
	! end if
	L17420: !
fnend 
def fnget_jmt_no$*30(jmt_fileno$)
	dim jmt_no$*30
	read #h_internal,using "FORM POS 27,C 30",key=jmt_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",2)&"JUDGMENT  ",release: jmt_no$ nokey JMN_XIT
	let fnget_jmt_no$=jmt_no$
	! if trim$(jmt_no$)="" and trim$(master_data$(master_jmt_no))<>"" then 
	!  let fnget_jmt_no$=jmt_no$=master_data$(master_jmt_no)
	!  let fnmessagebox("Warning, Internal Judgment Number is missing\nUsing MASTER.JMT_NO instead\n"&jmt_no$,mb_information+mb_okonly,"Problem with Judgment Number")
	!  rewrite #h_internal,using "FORM POS 27,C 30",key=jmt_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",2)&"JUDGMENT  ",release: jmt_no$ nokey JMN_XIT
	! end if  ! jmt_no$="" and master.jmt_no<>""
	JMN_XIT: ! 
fnend  ! fnget_jmt_no$
def fnget_edi_refno$*30(edi_fileno$)
	dim edi_refno_no$*30
	read #h_internal,using "FORM POS 27,C 30",key=edi_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",28)&"MAIN      ",release: edi_refno_no$ nokey L17480 
	let fnget_edi_refno$=edi_refno_no$ 
	! if trim$(edi_refno_no$)="" and trim$(master_data$(master_forw_refno))<>"" then 
	!  let fnget_edi_refno$=edi_refno_no$=master_data$(master_forw_refno) 
	!  let fnmessagebox("Warning, Internal EDI_REFNO # is missing\nUsing MASTER.FORW_REFNO instead\n"&edi_refno_no$,64,"Problem with FORW_REFNO/Edi Ref #") 
	!  rewrite #h_internal,using "FORM POS 27,C 30",key=edi_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",28)&"MAIN      ",release: edi_refno_no$ nokey L17480
	! end if
	L17480: !
fnend 
def fnget_fofile$*30(fofile_fileno$)
	dim fofile$*30
	read #h_internal,using "FORM POS 27,C 30",key=fofile_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",1)&"MAIN      ",release: fofile$ nokey L17496 
	let fnget_fofile$=fofile$ 
	! if trim$(fofile$)="" and trim$(master_data$(master_forw_fileno))<>"" then 
	!  let fnget_fofile$=fofile$=master_data$(master_forw_fileno) 
	!  let fnmessagebox("Warning, Internal Forw File Number is missing\nUsing MASTER.FORW_FILENO instead\n"&fofile$,64,"Problem with Forw Fileno #") 
	!  rewrite #h_internal,using "FORM POS 27,C 30",key=fofile_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",1)&"MAIN      ",release: fofile$ nokey L17496
	! end if
	L17496: !
fnend 
include: ertn