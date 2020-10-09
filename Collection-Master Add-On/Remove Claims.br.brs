on error goto Ertn
fn_setup
fngui_push_on

print newpage
goto MAIN_BODY


BY_FILENO: ! r:
	_by_fileno=fnsecurity("MENU:4-1-S-5-7-1","User does not have rights to this option.",0,user_init$)
	if _by_fileno<>1 then 
		goto DONE_BY_FILENO
	end if
	h_master=h_masterx
	print #1,fields "3,1,CR 24,[w];5,1,CR 24,[w]": "Date Received:","File #:"
	INPUT_FILENO: !
	rinput #1,fields "3,26,C 10,[D]SAE;5,26,C 8,[D]SA": inp_dayt$,fileno$
	if cmdkey=99 then 
		goto DONE_BY_FILENO
	end if
	inp_dayt$=fndate_dis10$(fndate_inp10$(inp_dayt$)) 
	print #1,fields "3,26,C 10": inp_dayt$
	if trim$(inp_dayt$)="" then 
		curfld(1) 
		goto INPUT_FILENO 
	else if trim$(fileno$)="" then 
		curfld(2) 
		goto INPUT_FILENO
	end if
	dayt$=fndate_c8$(fndate_inp10$(inp_dayt$))
	read #h_master,using R10,key=fileno$: f$,date_recd$,d$,fow,r$,cost_balance,collect_hold,creditor$,forw_fileno$,forw_refno$,jmt_date$,jmt_amt,prin_coll,int_coll nokey DONE_BY_FILENO
	if fn_warningMsg then 
		_deleted=fn_delete(fileno$,r$,dayt$,1)
	end if
goto DONE_BY_FILENO ! /r



BY_RANGE: ! r:
	_by_range=fnsecurity("MENU:4-1-S-5-7-2","User does not have rights to this option.",0,user_init$)
	if _by_range<>1 then 
		goto DONE_BY_RANGE
	end if
	h_master=h_masterdate
	print #1,fields "3,1,CR 24,[w];5,1,CR 24,[w];6,1,CR 24,[w]": "Date Received:","Starting File #:","Ending File #:"
	INPUT_RANGE: rinput #1,fields "3,26,C 10,[D]SAE;5,26,C 8,[D]SA;6,26,C 8,[D]SA": inp_dayt$,start_fileno$,end_fileno$
	if cmdkey=99 then 
		goto DONE_BY_RANGE
	end if
	inp_dayt$=fndate_dis10$(fndate_inp10$(inp_dayt$)) 
	print #1,fields "3,26,C 10": inp_dayt$
	if trim$(inp_dayt$)="" then 
		curfld(1) 
		goto INPUT_RANGE 
	else if trim$(start_fileno$)="" or trim$(end_fileno$)="" then 
		curfld(2) 
		goto INPUT_RANGE
	end if
	dayt$=fndate_c8$(fndate_inp10$(inp_dayt$))
	if fn_warningMsg then 
		read #h_master,using R10,key=dayt$: f$,date_recd$,d$,fow,r$,cost_balance,collect_hold,creditor$,forw_fileno$,forw_refno$,jmt_date$,jmt_amt,prin_coll,int_coll nokey DONE_BY_RANGE
		do 
			if r$<>dayt$ then 
				exit do 
			end if
			if f$<start_fileno$ or f$>end_fileno$ then 
				goto NEXT_CLAIM
			end if
			fn_delete(f$,r$,dayt$,0)
			NEXT_CLAIM: read #h_master,using R10: f$,date_recd$,d$,fow,r$,cost_balance,collect_hold,creditor$,forw_fileno$,forw_refno$,jmt_date$,jmt_amt,prin_coll,int_coll eof DONE_BY_RANGE
		loop 
	end if  ! fn_warningMsg
	DONE_BY_RANGE: !
goto DONE ! /r
CLEARVAR: ! r:
	dayt$=fileno$=start_fileno$=end_fileno$=message$=cfile$="" 
	inp_dayt$=date$("MM/DD/CCYY") 
	h_delete_report=h_reject_report=0
return ! /r

def library fnDeleteClaim(delete_fileno$; date_recd$,delete_date_recd$,no_report)
	fnDeleteClaim=fn_delete(delete_fileno$,date_recd$,delete_date_recd$,no_report)
fnend 
def fn_delete(delete_fileno$,date_recd$;delete_date_recd$,no_report) 
	! Set No_Report to 1 to suppress deletion or rejection reports (IE from mast2 where you know what file you are trying to delete without a report)
	if ~setup then 
		fn_setup
	end if
	delete_fileno$=rpad$(delete_fileno$,8)(1:8)
	if trim$(delete_date_recd$)="" then 
		h_master=h_masterx 
		read #h_master,using R10,key=delete_fileno$: f$,date_recd$,d$,fow,r$,cost_balance,collect_hold,creditor$,forw_fileno$,forw_refno$,jmt_date$,jmt_amt,prin_coll,int_coll 
		! This program supplies Delete_Date_Recd$, if it's blank we came from somewhere else, so MASTER needs to be read
	end if
	holding_place=rec(h_master)
	if cost_balance>0 then 
		reject_reason$="Cost Balance remains on file" 
	else if collect_hold<>0 then 
		reject_reason$="Claim has money in Coll&Hold" 
	else if delete_date_recd$<>date_recd$ then 
		reject_reason$="Date Recd doesn't match Fileno" 
	else if trim$(jmt_date$)<>"" or jmt_amt>0 then 
		reject_reason$="Claim has Judgment" 
	else if prin_coll>0 or int_coll>0 then 
		reject_reason$="Claim has had payments"
	end if
	if trim$(reject_reason$)<>"" then 
		if ~no_report then 
			gosub REJECT_REPORT 
			reject_reason$="" 
		else 
			fnmessagebox("Claim cannot be deleted!"&crlf$&reject_reason$,16,"Claim Not Deleted")
		end if
		goto DONE_DELETING
	end if 
	print #h_removed: delete_fileno$;" ";d$;" ";date_recd$;" ";fow
	print delete_fileno$;" ";d$;" ";date_recd$;" ";fow
	delete #h_master: 
	fn_delete=1 
	c+=1 
	fnlog("Log\Undo_Claims.log",user_init$&tab$&delete_fileno$&tab$&d$&tab$&fndate_rpt10$(date_recd$)&tab$&str$(fow))
	gosub DELETE_DEBTOR 
	gosub DELETE_INTERNAL 
	gosub DELETE_CAPTION 
	gosub DELETE_PROPERTY 
	gosub DELETE_INFINITY 
	gosub DELETE_CHANGES 
	gosub DELETE_BOOKMARK
	gosub DELETE_DCHANGES 
	gosub DELETE_TAGS 
	gosub DELETE_DPHONE
	gosub DELETE_AGENT_PF 
	gosub DELETE_CLIENT_PF 
	gosub DELETE_PAY_CALC
	gosub DELETE_FINAN_EDI 
	gosub DELETE_CALL_LOG 
	gosub DELETE_CALL_RESULT 
	gosub DELETE_CALL_LINK 
	gosub DELETE_AOD_DOCS 
	gosub DELETE_AOD_REQ 
	gosub DELETE_AOD_NOTES
	gosub DELETE_BUCKETS_EDI 
	gosub DELETE_CREDIT_FRM 
	gosub DELETE_PACTIV 
	gosub DELETE_DOCUMENT
	L60450: !
	read #h_diaryfil,using DIARY_FORM,search>=delete_fileno$: tdelete_fileno$ nokey L60460 
	if tdelete_fileno$=delete_fileno$ then 
		delete #h_diaryfil: 
		goto L60450
	end if
	L60460: read #h_extra,key=delete_fileno$: nokey L60470 : delete #h_extra: 
	L60470: read #h_active,using R20,key=delete_fileno$: fa$ nokey L60530
	goto L60500
	L60490: read #h_active,using R20: fa$ eof L60530
	L60500: if fa$<>delete_fileno$ then goto L60530
	delete #h_active: 
	goto L60490
	L60530: read #h_invoice,using R20,search>=delete_fileno$: fa$ nokey L60590
	goto L60560
	L60550: read #h_invoice,using R20: fa$ eof L60590
	L60560: if fa$<>delete_fileno$ then goto L60590
	delete #h_invoice: 
	goto L60550
	L60590: read #h_finan,using R20,search>=delete_fileno$: fa$ nokey L60610
	if fa$=delete_fileno$ then 
		delete #h_finan: 
		goto L60590
	end if
	L60610: read #h_buckets,using R20,search>=delete_fileno$: fa$ nokey DONE_SEARCHING
	if fa$=delete_fileno$ then 
		delete #h_buckets: 
		goto L60610
	end if 
	DONE_SEARCHING: !
	if ~no_report then 
		gosub PRINT_DELETE_REPORT
	end if
	DONE_DELETING: !
fnend 
def fn_warningMsg
	_warning=fnmessagebox("WARNING: This will completely remove the specificed file(s).\nThere is NO UNDO for this.\nDo you wish to proceed?",4+256+48,"Warning")
	if _warning=6 then 
		fn_warningMsg=1 
		! Yes 
	else 
		fn_warningMsg=0
	end if
fnend 
def fn_setup
	library "CLSUTIL/LIBRARY": fnuser_init$,fnsecurity,fngui_push_on,fngethandle,fndate_inpdis10$,fndate_inp10$,fndate_dis10$,fndate_c8$,fnlog,fnmake_dir,fndate_rpt10$,fnmenu,fnmessagebox,fnopen$,fnprogram_top,fnget_form,fnget_formarr,fnget_var$,fnget_formall$,fngenerate_buttons,fncnv$
	library "Print/Library": fnsel_lib,fnget_prnlegacyvar,fnget_prnvar$,fnclose,fnpage, fnprint_xlate,fnget_printers
	library "openfile/library": fnopen_dchanges,fnopen_tags,fnopen_dphone,fnopen_client_pf,fnopen_agent_pf,fnopen_pay_calc,fnopen_property,fnopen_master,fnopen_invoice,fnopen_internal_backbone,fnopen_infinity,fnopen_finan,fnopen_extra,fnopen_debtor,fnopen_diaryint,fnopen_changes,fnopen_caption,fnopen_buckets,fnopen_bookmark,fnopen_active
	library "openfile/library": fnopen_finan_edi,fnopen_call_log,fnopen_call_result,fnopen_call_link,fnopen_aod_docs,fnopen_aod_req,fnopen_aod_notes,fnopen_credit_frm,fnopen_buckets_edi,fnopen_pactiv
	library "openfile/library": fnopen_document,fnopen_invoice
	library "MAST_SQL/PROG2": fnopen_debt
	gosub OPEN_FILES
	gosub SETUP_SQL
	fnmake_dir('log')
	tab$=chr$(9) 
	crlf$=chr$(13)&chr$(10)
	user_init$=fnuser_init$
	go=fnsecurity("EDI:UNDOMW","User does not have rights to this program.",0,user_init$,-1) 
	if go<>1 then chain "M/prog1"
	setup=1
	fnget_printers(mat printer_choices$)
	gosub SetupPrint
	dim d$*30,_menu$(0)*60,cfile$*256,menu_startup$*80,buff$*30,line$(2)*15,creditor$*40,forw_fileno$*30,reject_reason$*60,reject_printer$*30,delete_printer$*30,printer_choices$(1)*80,box_agedtop$*40,box_agedtop_delete$*40,message$*512,heading$*512,headings$(1)*256
fnend 



SETUP_SQL: ! r: Setup SQL Info
	! Setup Master Fields
	dim master_data$(1)*60,master_data(1),master_fieldsc$(1)*20,master_fieldsn$(1)*20,master_formc$*2048,master_formn$*2048,master_formall$*2048
	dim master_des_c$(1)*80,master_des_n$(1)*80,master_seq$(1)*80,master_valid$(1)*80,master_fc$(1,3)*80,master_fn$(1,3)*80
	fnget_form("master",mat master_data$,mat master_data,mat master_fieldsc$,mat master_fieldsn$,master_formc$,master_formn$)
	fnget_formarr("master",mat master_data$,mat master_data,mat master_fieldsc$,mat master_fieldsn$,mat master_fc$,mat master_fn$,mat master_des_c$,mat master_des_n$,mat master_seq$,mat master_valid$) 
	execute "*SubProc "&fnget_var$("Master", mat master_fieldsc$, mat master_fieldsn$) 
	master_formall$=fnget_formall$
return ! /r
REJECT_REPORT: ! r:
	if ~h_reject_report or trim$(file$(h_reject_report))="" then 
		open #h_reject_report:=fngethandle: "NAME=N:NOT_DELETED_"&date$("MMDDCCYY")&srep$(time$,":","")&".CSV,replace",display,output  
		print #h_reject_report: "FILENO,RECD,REASON"
	end if
	print #h_reject_report: f$&","&fndate_rpt10$(r$)&","&reject_reason$
	read #h_master,same,release: 
	! Release previous locks, record not deleted, but keep last place
return ! /r
PRINT_DELETE_REPORT: ! r:
	if ~h_delete_report or trim$(file$(h_delete_report))="" then 
		open #h_delete_report:=fngethandle: "NAME=N:DELETED_"&date$("MMDDCCYY")&srep$(time$,":","")&".CSV,replace",display,output  
		print #h_delete_report: "FILENO,RECD"
	end if
	print #h_delete_report: delete_fileno$&","&fndate_rpt10$(date_recd$)
return ! /r
OPEN_FILES: ! r:
	open #h_removed:=fngethandle: "name=removed.mw/temp,replace,recl=100",display,output 
	! OPEN #H_Extra:=fngethandle: "NAME=EXTRA//6,KFNAME=EXTRAI//6,SHR",INTERNAL,OUTIN,KEYED 
	h_extra=fnopen_extra(mat extra_handles)
	! OPEN #H_Masterdate:=fngethandle: "NAME=MASTER//6,KFNAME=dateidxb//6,SHR",INTERNAL,OUTIN,KEYED  
	! OPEN #H_Masterx:=fngethandle: "NAME=MASTER//6,KFNAME=MASTERX//6,SHR",INTERNAL,OUTIN,KEYED 
	h_masterx=fnopen_master(mat master_handles) 
	h_masterdate=master_handles(7)
	h_invoice=fnopen_invoice(mat invoice_handles)
	! OPEN #H_Active:=fngethandle: "NAME=ACTIVE.INT//6,KFNAME=ACTIVE.IDX//6,SHR",INTERNAL,OUTIN,KEYED
	! OPEN #H_Activedte:=fngethandle: "NAME=ACTIVE.INT//6,KFNAME=ACTIVE.DTE//6,SHR",INTERNAL,OUTIN,KEYED 
	h_active=fnopen_active(mat active_handles)
	! OPEN #H_Finan:=fngethandle: "NAME=FINAN.INT//6,KFNAME=FINAN.IDX//6,shr",INTERNAL,OUTIN,KEYED 
	h_finan=fnopen_finan(mat finan_handles)
	! OPEN #H_Buckets:=fngethandle: "NAME=BUCKETS.INT//6,shr,KFNAME=BUCKETS.IDX//6",INTERNAL,OUTIN,KEYED 
	h_buckets=fnopen_buckets(mat buckets_handles)
	! OPEN #H_Diarydat:=fngethandle: "name=diary.int//6,kfname=diary.dat//6,shr",INTERNAL,OUTIN,KEYED IOERR 20100
	! OPEN #H_Diaryfil:=fngethandle: "name=diary.int//6,kfname=diary.fil//6,shr",INTERNAL,OUTIN,KEYED IOERR 20110
	! OPEN #H_Diaryque:=fngethandle: "name=diary.int//6,kfname=diary.que//6,shr",INTERNAL,INPUT,KEYED IOERR 20120
	h_diaryfil=fnopen_diaryint(mat diary_handles)
	! OPEN #H_Internal:=fngethandle: "NAME=INTERNAL//6,KFNAME=INTERNAL.IDX//6,SHR",INTERNAL,OUTIN,KEYED  
	! OPEN #H_Internalrel:=fngethandle: "NAME=INTERNAL//6,KFNAME=INTERNAL.REL//6,SHR",INTERNAL,OUTIN,KEYED 
	h_internal=fnopen_internal_backbone(mat internal_handles)
	! OPEN #H_Debtor:=fngethandle: "NAME=DEBTOR//6,KFNAME=DEBTOR.IDX//6,SHR",INTERNAL,OUTIN,KEYED  
	! Fnopen_Debt("INTERNAL//6") 
	! Fnopen_Debt("DEBTOR//6") 
	h_debtor=fnopen_debtor(mat debtor_handles)
	! OPEN #H_Caption:=fngethandle: "NAME=CAPTION//6,KFNAME=CAPTION.IDX//6,SHR",INTERNAL,OUTIN,KEYED 
	h_caption=fnopen_caption(mat caption_handles)
	! OPEN #H_Property:=fngethandle: "NAME=PROPERTY//6,KFNAME=PROPERTY.IDX//6,SHR",INTERNAL,OUTIN,KEYED 
	h_property=fnopen_property(mat property_handles)
	! OPEN #H_Infinity:=fngethandle: "Name=infinity.int//6,kfname=infinity.idx//6,shr",INTERNAL,OUTIN,KEYED  
	! OPEN #H_Infinitycdx:=fngethandle: "Name=infinity.int//6,kfname=infinity.cdx//6,shr",INTERNAL,OUTIN,KEYED  
	! OPEN #H_Infinitydax:=fngethandle: "Name=infinity.int//6,kfname=infinity.dax//6,shr",INTERNAL,OUTIN,KEYED 
	h_infinity=fnopen_infinity(mat infinity_handles)
	! OPEN #H_Changes:=fngethandle: "Name=changes.int//6,kfname=changes.idx//6,shr",INTERNAL,OUTIN,KEYED  
	! OPEN #H_Changesrec:=fngethandle: "Name=changes.int//6,kfname=changes.rec//6,shr",INTERNAL,OUTIN,KEYED 
	h_changes=fnopen_changes(mat changes_handles)
	! OPEN #H_Bookmark:=fngethandle: "Name=Bookmark.Int//6,KFName=Bookmark.Idx//6,Shr",INTERNAL,OUTIN,KEYED 
	h_bookmark=fnopen_bookmark(mat bookmark_handles)
	h_dchanges=fnopen_dchanges(mat dchanges)
	h_tags=fnopen_tags(mat tags) 
	h_dphone=fnopen_dphone(mat dphone) 
	h_agent_pf=fnopen_agent_pf(mat agent_pf) 
	h_client_pf=fnopen_client_pf(mat client_pf) 
	h_pay_calc=fnopen_pay_calc(mat pay_calc)
	h_finan_edi=fnopen_finan_edi(mat finan_edi) 
	h_call_log=fnopen_call_log(mat call_log) 
	h_call_result=fnopen_call_result(mat call_result) 
	h_call_link=fnopen_call_link(mat call_link) 
	h_aod_docs=fnopen_aod_docs(mat aod_docs) 
	h_aod_req=fnopen_aod_req(mat aod_req) 
	h_aod_notes=fnopen_aod_notes(mat aod_notes)
	h_buckets_edi=fnopen_buckets_edi(mat buckets_edi) 
	h_credit_frm=fnopen_credit_frm(mat credit_frm) 
	h_pactiv=fnopen_pactiv(mat pactiv_handles) 
	h_document=fnopen_document(mat docuemnt_handles)
	open #h_undolog:=fngethandle: "Name=Undomwlog.int//6,KFName=Undomwlog.idx//6,RECL=182,KPS=1,KLN=8,USE,shr",internal,output,keyed 
return ! /r
DELETE_DEBTOR: ! r: DELETE ALL DEBTOR INFORMATION FOR THE PARTICULAR CLAIM
	DEBTOR_FORM: form pos 1,c 8,n 3
	do 
		READ_DEBTOR: !
		read #h_debtor,using DEBTOR_FORM,search>=f$: fa$,debtor_no nokey END_DELETE_DEBTOR
		if fa$<>f$ then goto END_DELETE_DEBTOR
		delete #h_debtor,key=f$&cnvrt$('N 3',debtor_no): nokey READ_DEBTOR
	loop 
END_DELETE_DEBTOR: ! 
return ! /r
DELETE_INTERNAL: ! r: DELETE ALL INTERNAL INFORMATION FOR THE PARTICULAR CLAIM
INTERNAL_FORM: form pos 1,c 8
do 
	read #h_internal,using INTERNAL_FORM,search>=f$: fa$ nokey END_DELETE_INTERNAL
	if fa$<>f$ then goto END_DELETE_INTERNAL
	delete #h_internal: 
loop 
END_DELETE_INTERNAL: ! 
return ! /r
DELETE_CAPTION: ! r: DELETE ALL CAPTION INFORMATION FOR THE PARTICULAR CLAIM
CAPTION_FORM: form pos 1,c 8
do 
	read #h_caption,using CAPTION_FORM,search>=f$: fa$ nokey END_DELETE_CAPTION
	if fa$<>f$ then goto END_DELETE_CAPTION
	delete #h_caption: 
loop 
END_DELETE_CAPTION: ! 
return ! /r
DELETE_PROPERTY: ! r: delete all property information for the particular claim
PROPERTY_FORM: form pos 1,c 8
do 
	read #h_property,using PROPERTY_FORM,search>=f$: fa$ nokey END_DELETE_PROPERTY
	if fa$<>f$ then goto END_DELETE_PROPERTY
	delete #h_property: 
loop 
END_DELETE_PROPERTY: ! 
return ! /r
DELETE_INFINITY: ! r: delete all INFINITY information for the particular claim
INF_FORM: form pos 1,c 8
do 
	read #h_infinity,using INF_FORM,search>=f$: fa$ nokey END_DELETE_INF
	if fa$<>f$ then goto END_DELETE_INF
	delete #h_infinity: 
loop 
END_DELETE_INF: ! 
return ! /r
DELETE_CHANGES: ! r: delete all CHANGES information for the particular claim
CHANGES_FORM: form pos 1,c 8
do 
	read #h_changes,using CHANGES_FORM,search>=f$: fa$ nokey END_DELETE_CHANGES
	if fa$<>f$ then goto END_DELETE_CHANGES
	delete #h_changes: 
loop 
END_DELETE_CHANGES: ! 
return ! /r
DELETE_DCHANGES: ! r:
	do 
		read #h_dchanges,using CHANGES_FORM,search>=f$: fa$ nokey DELETE_DCHANGES_XIT
		if fa$<>f$ then goto DELETE_DCHANGES_XIT
		delete #h_dchanges: 
	loop 
	DELETE_DCHANGES_XIT: ! 
return ! /r
DELETE_BOOKMARK: ! r: DELETE ALL BOOKMARK INFORMATION FOR THE PARTICULAR CLAIM
BOOK_FORM: form pos 1,c 1,c 3,c 8,c 20
dim myfa$*24,myf$*20
myf$=lpad$(f$,12)
do 
	read #h_bookmark,using BOOK_FORM,search>=myf$: stat$,init$,cat$,myfa$ nokey XIT_DEL_BOOKMARK
	if rpad$(myfa$ (1:8),8)<>rpad$(f$,8) then goto Xit_DEL_BOOKMARK
	delete #h_bookmark: 
loop 
XIT_DEL_BOOKMARK: ! 
! PAUSE
return ! /r
DELETE_TAGS: ! r: DELETE ALL TAGS INFORMATION FOR THE PARTICULAR CLAIM
TAGS_FORM: form pos 1,c 8,n 3
do 
	read #h_tags,using TAGS_FORM,search>=f$: fa$,debtor_no nokey XIT_DEL_TAGS
	if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_TAGS
	delete #h_tags: 
loop 
XIT_DEL_TAGS: ! 
! PAUSE
return ! /r
DELETE_DPHONE: ! r: DELETE ALL DPhone INFORMATION FOR THE PARTICULAR CLAIM
DPHONE_FORM: form pos 1,c 8,n 3
do 
	read #h_dphone,using DPHONE_FORM,search>=f$: fa$,debtor_no nokey XIT_DEL_DPHONE
	if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_DPHONE
	delete #h_dphone: 
loop 
XIT_DEL_DPHONE: ! 
return ! /r
DELETE_AGENT_PF: ! r: DELETE ALL AGENT_PF INFORMATION FOR THE PARTICULAR CLAIM
	AGENT_PF_FORM: form pos 1,c 8
	do 
		read #h_agent_pf,using AGENT_PF_FORM,search>=f$: fa$ nokey XIT_DEL_AGENT_PF
		if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_AGENT_PF
		delete #h_agent_pf: 
	loop 
	XIT_DEL_AGENT_PF: ! 
return ! /r
DELETE_CLIENT_PF: !  r: DELETE ALL CLIENT_PF INFORMATION FOR THE PARTICULAR CLAIM
	CLIENT_PF_FORM: form pos 1,c 8
	do 
		read #h_client_pf,using CLIENT_PF_FORM,search>=f$: fa$ nokey XIT_DEL_CLIENT_PF
		if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_CLIENT_PF
		delete #h_client_pf: 
	loop 
	XIT_DEL_CLIENT_PF: ! 
return ! /r
DELETE_PAY_CALC: !  r: DELETE ALL PAY_CALC INFORMATION FOR THE PARTICULAR CLAIM
	PAY_CALC_FORM: form pos 1,c 8
	do 
		read #h_pay_calc,using PAY_CALC_FORM,search>=f$: fa$ nokey XIT_DEL_PAY_CALC
		if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_PAY_CALC
		delete #h_pay_calc: 
	loop 
	XIT_DEL_PAY_CALC: ! 
return ! /r
DELETE_FINAN_EDI: ! r: DELETE ALL FINAN_EDI INFORMATION FOR THE PARTICULAR CLAIM
FINAN_EDI_FORM: form pos 1,c 8
do 
	read #h_finan_edi,using FINAN_EDI_FORM,search>=f$: fa$ nokey XIT_DEL_FINAN_EDI
	if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_FINAN_EDI
	delete #h_finan_edi: 
loop 
XIT_DEL_FINAN_EDI: ! 
return ! /r
DELETE_CALL_LOG: ! r: DELETE ALL CALL_LOG INFORMATION FOR THE PARTICULAR CLAIM
CALL_LOG_FORM: form pos 1,c 8
do 
	read #h_call_log,using CALL_LOG_FORM,search>=f$: fa$ nokey XIT_DEL_CALL_LOG
	if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_CALL_LOG
	delete #h_call_log: 
loop 
XIT_DEL_CALL_LOG: ! 
return ! /r
DELETE_CALL_RESULT: ! r: DELETE ALL CALL_RESULT INFORMATION FOR THE PARTICULAR CLAIM
CALL_RESULT_FORM: form pos 1,c 8
do 
	read #h_call_result,using CALL_RESULT_FORM,search>=f$: fa$ nokey XIT_DEL_CALL_RESULT
	if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_CALL_RESULT
	delete #h_call_result: 
loop 
XIT_DEL_CALL_RESULT: ! 
return ! /r
DELETE_CALL_LINK: ! r: DELETE ALL CALL_LINK INFORMATION FOR THE PARTICULAR CLAIM
CALL_LINK_FORM: form pos 1,c 8
do 
	read #h_call_link,using CALL_LINK_FORM,search>=f$: fa$ nokey XIT_DEL_CALL_LINK
	if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_CALL_LINK
	delete #h_call_link: 
loop 
XIT_DEL_CALL_LINK: ! 
return ! /r
DELETE_AOD_REQ: ! r: DELETE ALL AOD_REQ INFORMATION FOR THE PARTICULAR CLAIM
AOD_REQ_FORM: form pos 1,c 8
do 
	read #h_aod_req,using AOD_REQ_FORM,search>=f$: fa$ nokey XIT_DEL_AOD_REQ
	if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_AOD_REQ
	delete #h_aod_req: 
loop 
XIT_DEL_AOD_REQ: ! 
return ! /r
DELETE_AOD_DOCS: ! r:  DELETE ALL AOD_DOCS INFORMATION FOR THE PARTICULAR CLAIM
	AOD_DOCS_FORM: form pos 1,c 8
	do 
		read #h_aod_docs,using AOD_DOCS_FORM,search>=f$: fa$ nokey XIT_DEL_AOD_DOCS
		if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_AOD_DOCS
		delete #h_aod_docs: 
	loop 
	XIT_DEL_AOD_DOCS: ! 
return ! /r
DELETE_AOD_NOTES: ! r:  DELETE ALL AOD_NOTES INFORMATION FOR THE PARTICULAR CLAIM
	AOD_NOTES_FORM: form pos 1,c 8
	do 
		read #h_aod_notes,using AOD_NOTES_FORM,search>=f$: fa$ nokey XIT_DEL_AOD_NOTES
		if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_AOD_NOTES
		delete #h_aod_notes: 
	loop 
	XIT_DEL_AOD_NOTES: ! 
return ! /r
DELETE_BUCKETS_EDI: ! r:  DELETE ALL BUCKETS_EDI INFORMATION FOR THE PARTICULAR CLAIM
	BUCKETS_EDI_FORM: form pos 1,c 8
	do 
		read #h_buckets_edi,using BUCKETS_EDI_FORM,search>=f$: fa$ nokey XIT_DEL_BUCKETS_EDI
		if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_BUCKETS_EDI
		delete #h_buckets_edi: 
	loop 
	XIT_DEL_BUCKETS_EDI: ! 
return ! /r
DELETE_CREDIT_FRM: ! r: DELETE ALL CREDIT_FRM INFORMATION FOR THE PARTICULAR CLAIM
	do 
		read #h_credit_frm,using 'form pos 1,c 8',search>=f$: fa$ nokey XIT_DEL_CREDIT_FRM
		if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_CREDIT_FRM
		delete #h_credit_frm: 
	loop 
	XIT_DEL_CREDIT_FRM: ! 
return ! /r
DELETE_PACTIV: ! r: DELETE ALL pactiv INFORMATION FOR THE PARTICULAR CLAIM
	do 
		read #h_pactiv,using 'form pos 1,c 8',search>=f$: fa$ nokey XIT_DEL_PACTIV
		if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_PACTIV
		delete #h_pactiv: 
	loop 
	XIT_DEL_PACTIV: ! 
return ! /r
DELETE_DOCUMENT: ! r: DELETE ALL document INFORMATION FOR THE PARTICULAR CLAIM
	do 
		read #h_document,using 'form pos 1,c 8',search>=f$: fa$ nokey XIT_DEL_DOCUMENT
		if rpad$(f$(1:8),8)<>rpad$(fa$,8) then goto Xit_DEL_DOCUMENT
		delete #h_document: 
	loop 
	XIT_DEL_DOCUMENT: ! 
return ! /r




MAIN_BODY: ! r:
R10: form pos 1,2*c 8,pos 63,c 30,pos 17,bh 3,pos 9,c 8,pos 820,pd 6.2,pos 826,pd 6.2,pos 174,c 40,pos 1279,c 20,pos 1431,c 10,pos 276,c 8,pos 808,pd 6.2,pos 832,2*pd 6.2
R20: form pos 1,c 8
DIARY_FORM: form pos 1,c 8,x 1,b 3,2*c 8,c 20,c 8,c 5,cr 3
	mat _menu$(4) 
	_menu$(1)="1. Single Claim" 
	_menu$(2)="2. Claim Range" 
	_menu$(3)="3. List of Claims"
	_menu$(udim(mat _menu$))="0. Return to Main Menu"
	fngenerate_buttons("0,99","OK,Exit",2,0,1)
	inp_dayt$=date$("MM/DD/CCYY")
	MAIN_MENU: !
	choose=fnmenu("Remove Claim Utility",mat _menu$, pk$, title$,footer$, nnp, autonumber=0,mstart,mat empty$, mat empty$, mat empty$,menu_startup$,0,freq)
	fnprogram_top("Remove Claim Utility",pk$,m_pk$) 
	m_pk$=pk$="4-1-S-5-7"
	open #1: "SCOL=15,SROW=5,ECOL=65,EROW=15,BORDER=D",display,outin  
	print #1,fields "8,1,CC 50,[S]": "WARNING:" 
	print #1,fields "9,1,CC 50,[S]": "Deletion of files cannot be undone!" 
	print #1,fields "10,1,CC 50,[S]": "Use extreme caution!"
	on choose gosub BY_FILENO,BY_RANGE,BY_LIST,Xit
goto MAIN_MENU ! /r


BY_LIST: ! r:
	_by_list=fnsecurity("MENU:4-1-S-5-7-3","User does not have rights to this option.",0,user_init$)
	if _by_list<>1 then 
		goto DONE_BY_LIST
	end if
	h_master=h_masterx
	print #1,fields "3,1,CR 10,[W];3,48,P 1/2,[Button],B5": "Text List:","Icons\Open.gif:isotropic"
	INPUT_LIST: rinput #1,fields "3,12,35/C 256,[D]SAE",attr '[A]': cfile$
	if cmdkey=99 then 
		goto DONE_BY_LIST
	end if
	if cmdkey=5 or (curfld=2 and (pos(cfile$,"*")>0 or pos(cfile$,"?")))>0 or exists(cfile$)=1 then 
		cfile$=fnopen$(trim$(cfile$)) 
		goto INPUT_LIST
	end if
	if fn_warningMsg then 
		if exists(cfile$)>1 then 
			open #h_list_input:=fngethandle: "NAME="&cfile$&",shr",display,input 
			linput #h_list_input: heading$ eof DONE_BY_LIST
			str2mat(heading$,mat headings$,tab$) 
			_fileno=srch(mat headings$,"FILENO") 
			_date_recd=srch(mat headings$,"RECD")
			if _fileno<=0 or _date_recd<=0 then 
				fnmessagebox("Invalid headers in file.\nFile needs FILENO (File Number) and\nRECD (Date Received) headers.",16,"Invalid File") 
				goto DONE_BY_LIST
			end if
			do 
				linput #h_list_input: buff$ eof DONE_BY_LIST
				str2mat(buff$,mat line$,tab$) 
				! Each line must contain Fileno and Date Received
				if udim(mat line$)<2 or _fileno<=0 or _date_recd<=0 then 
					reject_reason$="Not enough data given in list" 
					gosub REJECT_REPORT 
					reject_reason$=""
				else 
					read #h_master,using R10,key=rpad$(line$(_fileno),8)(1:8): f$,date_recd$,d$,fow,r$,cost_balance,collect_hold,creditor$,forw_fileno$,forw_refno$,jmt_date$,jmt_amt,prin_coll,int_coll nokey NEXT_LIST_FILENO
					dayt$=fndate_c8$(fndate_inp10$(line$(_date_recd))) 
					fn_delete(f$,r$,dayt$,0)
				end if 
				NEXT_LIST_FILENO: !
			loop 
		else 
			fnmessagebox("Invalid file.  Please try again.",16,"Invalid File")
			goto INPUT_LIST
		end if  ! Check file exists
	end if  ! fn_warningMsg
DONE_BY_LIST: !
close #h_list_input: ioerr ignore 
goto DONE ! /r
DONE: ! r:
	if h_delete_report and h_reject_report then 
		message$=file$(h_delete_report)&crlf$&file$(h_reject_report) 
	else if h_reject_report then 
		message$=file$(h_reject_report) 
	else if h_delete_report then 
		message$=file$(h_delete_report)
	end if
	if trim$(message$)<>"" then 
		fnmessagebox("The following files were created:\n"&message$,64,"Files Created")
	end if
	gosub CLEARVAR 
	fnclose(h_reject_report) 
	fnclose(h_delete_report) 
	fnclose(1)
return ! /r
Xit: ! 
	if trim$(return$)<>"" then chain return$ 
	library 'S:\Core\Library.br': fnXit
fnXit


DONE_BY_FILENO: ! r:
	if _deleted then 
		fnmessagebox("File Successfully Deleted",64,"File Deleted") 
		_deleted=0
	end if
goto DONE ! /r

include: cm\print
include: ertn
