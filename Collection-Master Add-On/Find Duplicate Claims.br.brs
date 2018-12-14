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
FFFN: !
 delim$=','
 reportFile$='@:n:CM-Duplicates-ForwarderFileNo.csv'
 limitRecordsProcessed=0 ! 50000
 timeStart=fnStime(time$)
 Fffn_AskFile: !
 open #hOut:=fngethandle: "Name=SAVE:"&reportFile$&",RecL=1024,replace",display,output ioerr SAVE_AS_OPEN_ERR
 reportFile$=os_filename$(file$(hOut))
 Fnlist_Print('creating '&reportFile$, 0,1,'Status') ! env$('program_caption'))
 pr #hOut: 'FileNo'&delim$&'Matches'&delim$&'Forwarder'&delim$&'Forwarder File Number'

 ! r: MAIN LOOP
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
  read #main_read,using master_formall$,rec=mRec: mat master_data$,mat master_data
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
return ! /r



FFIT: ! r:
 delim$=','
 reportFile$='@:n:CM-Duplicates-TrakFNo.csv'
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
  read #main_read,using master_formall$,rec=mRec: mat master_data$,mat master_data
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
 let master_fileno=1: let master_date_recd=2: let master_forw_file_12=3: let master_law_list=4: let master_comm=5: let master_sfee=6: let master_jmt_no=7: let master_d1_name=8: let master_d1_street=9: let master_d1_cs=10: let master_d1_zip=11: let master_d1_phone=12: let master_cred_name=13: let master_cred_xline=14: let master_filler_254=15: let master_date_debt=16: let master_suit_date=17: let master_jmt_date=18: let master_docket_no=19: let master_stat1_date=20: let master_stat1_code=21: let master_plaintiff_1=22: let master_plaintiff_2=23: let master_defendant_1=24: let master_defendant_2=25
 let master_defendant_3=26: let master_defendant_4=27: let master_d2_name=28: let master_d2_street=29: let master_d2_csz=30: let master_d3_name=31: let master_d3_street=32: let master_d3_csz=33: let master_misc_asset1=34: let master_misc_asset2=35: let master_misc_asset3=36: let master_d1_ssn=37: let master_filler_730=38: let master_stat2_date=39: let master_stat2_code=40: let master_bank_acct_no=41: let master_int_date=42: let master_closed_yy=43: let master_closed_mm=44: let master_closed_code1=45: let master_closed_code2=46: let master_filler_adva=47: let master_ll_date=48: let master_ll_code4=49
 let master_coco_fileno=50: let master_filler_1040=51: let master_filler_1049=52: let master_d1_alias=53: let master_filler_cred_xline=54: let master_filler_1100=55: let master_forw_comm=56: let master_forw_sfee=57: let master_coco_comm=58: let master_coco_sfee=59: let master_rfile_no=60: let master_ll_code8=61: let master_lpaymnt_date=62: let master_forw_fileno=63: let master_d1_fax=64: let master_no_debtors=65: let master_link_primary=66: let master_d2_phone=67: let master_d3_phone=68: let master_asset_phone=69: let master_p_type=70: let master_p_dcode=71: let master_forw_refno=72: let master_b2_accno=73
 let master_b3_accno=74: let master_emp1_acc=75: let master_emp2_acc=76: let master_emp3_acc=77: let master_d2_ssn=78: let master_d3_ssn=79: let master_c_priority=80: let master_ctype=81: let master_d2_adva_file=82: let master_d3_adva_file=83: let master_claim_time=84: let master_d1_dl=85: let master_d2_dl=86: let master_d3_dl=87: let master_d1_birth_date=88: let master_d2_birth_date=89: let master_d3_birth_date=90: let master_d1_returned=91: let master_d2_returned=92: let master_d3_returned=93: let master_cred_phone=94: let master_cred_fax=95: let master_cred_email=96: let master_filler_2048=97
 let master_def_state=98: let master_collord=99: let master_edi_status_code=100: let master_edi_status_date=101: let master_cl_status_code=102: let master_cl_status_date=103: let master_closed_date=104: let master_filler_2375=105: let master_contract_flag=106: let master_jmt_flags=107: let master_cred_street=108: let master_cred_cs=109: let master_cred_zip=110: let master_jmt_type=111
 let master_forw_no=1: let master_venue1_no=2: let master_pre_j_rate=3: let master_post_j_rate=4: let master_stored_int=5: let master_contract_fee=6: let master_orig_claim=7: let master_suit_amt=8: let master_stat_fee=9: let master_jmt_amt=10: let master_balance=11: let master_cost_bal=12: let master_coll_hold=13: let master_prin_coll=14: let master_int_coll=15: let master_merch_bef=16: let master_merch_post=17: let master_cash_bef=18: let master_cash_post=19: let master_cost_received=20: let master_cost_ret=21: let master_cost_exp=22: let master_cost_recovered=23: let master_comm_earn=24: let master_sfee_earn=25
 let master_stat_earn=26: let master_sher_no=27: let master_atty=28: let master_para=29: let master_recs_01=30: let master_recs_02=31: let master_recs_19=32: let master_recs_03=33: let master_recs_04=34: let master_recs_05=35: let master_recs_20=36: let master_recs_06=37: let master_recs_07=38: let master_recs_08=39: let master_recs_21=40: let master_recs_09=41: let master_recs_10=42: let master_recs_11=43: let master_recs_22=44: let master_recs_12=45: let master_recs_13=46: let master_recs_14=47: let master_recs_23=48: let master_recs_15=49: let master_recs_16=50: let master_recs_17=51: let master_recs_24=52
 let master_recs_18=53: let master_bank_no=54: let master_coco_no=55: let master_cost_post_judg=56: let master_affidavit=57: let master_paid_post_judg=58: let master_non_rcvrd_cost=59: let master_tot_exp_cost=60: let master_dp_pre_suit_nf=61: let master_dp_post_suit_nf=62: let master_dp_post_judg_nf=63: let master_merch_post_judg=64: let master_exp_cost=65: let master_dp_pre_suit=66: let master_dp_post_suit=67: let master_dp_post_judg=68: let master_accr_int_bef=69: let master_tax_rebate=70: let master_agent_befscoll=71: let master_agent_pscoll=72: let master_agent_pjcoll=73: let master_agent_cost=74
 let master_sales_tax=75: let master_scode_pos=76: let master_lpaymnt_refno=77: let master_lpaymnt_amt=78: let master_client_no=79: let master_l_sfcode=80: let master_p_1st_date=81: let master_p_amort=82: let master_p_period=83: let master_p_first=84: let master_p_payment=85: let master_p_last=86: let master_p_amount=87: let master_emp_no=88: let master_c_adjust=89: let master_compr_amt=90: let master_overpay_amt=91: let master_bank2=92: let master_bank3=93: let master_emp2=94: let master_emp3=95: let master_collector=96: let master_d2_adva=97: let master_d3_adva=98: let master_sales_no=99: let master_coco_comm_earn=100
 let master_coco_sfee_earn=101: let master_principal_coll=102: let master_contract_coll=103: let master_fees_coll=104: let master_charges_coll=105: let master_other_coll=106: let master_other_fees_coll=107: let master_pend_coll=108: let master_nr_cost_coll=109: let master_rec_costs_coll=110: let master_firm_costs_coll=111: let master_other_cost_coll=112: let master_frec37=113: let master_frec38=114: let master_frec39=115: let master_frec40=116: let master_opened_amt=117: let master_services_amt=118: let master_charge_off=119: let master_purchase_amt=120: let master_pre_paid_amt=121: let master_filler_1934=122
 let master_filler_1940=123: let master_filler_1946=124: let master_filler_1958=125: let master_l_rpt_int_rate=126: let master_opened_date=127: let master_services_date=128: let master_charge_off_date=129: let master_purchase_date=130: let master_pre_paid_date=131: let master_jmt_exp_date=132: let master_filler_1988=133: let master_filler_1992=134: let master_filler_1996=135: let master_l_rpt_int_date=136: let master_statute_date=137: let master_rev_date=138: let master_rev_time=139: let master_rev_user=140: let master_edi_date=141: let master_edi_time=142: let master_edi_user=143: let master_batch_date=144
 let master_batch_time=145: let master_batch_user=146: let master_batch_enet_no=147: let master_per_diem_date=148: let master_per_diem_amt=149: let master_per_diem_rate=150: let master_per_diem_base=151: let master_per_diem_int=152: let master_jmt_cost=153: let master_jmt_stat=154: let master_jmt_contract=155: let master_jmt_prin=156: let master_jmt_awarded=157: let master_exp_m_cost=158: let master_exp_d_cost=159: let master_exp_p_cost=160: let master_exp_r_cost=161: let master_wo_rec_costs=162: let master_wo_nr_costs=163: let master_wo_fees=164: let master_frec58=165: let master_frec59=166: let master_frec60=167
 let master_frec61=168: let master_frec62=169: let master_frec63=170: let master_frec64=171: let master_frec65=172: let master_i_1100_ir_j=173: let master_i_3100_ir_j=174: let master_i_4100_ir_j=175: let master_i_5100_ir_j=176: let master_i_6100_ir_j=177: let master_i_9100_ir_j=178: let master_filler_ir_j_72=179: let master_filler_ir_j_73=180: let master_filler_ir_j_74=181: let master_filler_ir_j_75=182: let master_filler_ir_j_76=183: let master_filler_ir_j_77=184: let master_filler_ir_j_78=185: let master_filler_ir_j_79=186: let master_filler_ir_j_80=187: let master_adva=188: let master_filler_2350=189
 let master_filler_2353=190: let master_pay_subcode=191: let master_p_entry_date=192: let master_filler_2363=193: let master_filler_2367=194: let master_filler_2371=195
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