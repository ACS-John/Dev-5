fn_setup
fnTop(program$)
dim Selmenu$(0)*128
fnAddOneC(mat Selmenu$,'Duplicate Forwarder+Forwarder File Number')
fnAddOneC(mat Selmenu$,'Duplicate Forwarder+Infinity.*TrakFNo')
pk$='A-F'

 fnget_form("Master",mat master_data$,mat master_data,mat master_fieldsc$,mat master_fieldsn$,master_formc$,master_formn$)
 fnunpack$(master_formc$,master_formn$)
 master_formall$=fnget_formall$
 gosub enumMaster
 open #main_read:=fngethandle: 'name=master//6,kfname=masterx//6,shr',i,outIn,k
 open #h_internal:=fngethandle: "NAME=INTERNAL//6,KFNAME=INTERNAL.IDX//6,SHR",i,outIn,k


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
	if env$('BR_MODEL')='CLIENT/SERVER' then
		reportFile$(0:0)='@:'
	end if
	limitRecordsProcessed=0 ! 50000
	timeStart=fnStime(time$)
	Fffn_AskFile: !
	open #hOut:=fngethandle: "Name=SAVE:"&reportFile$&",RecL=1024,replace",d,o ioerr SAVE_AS_OPEN_ERR
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
		master_data$(master_docket_no)=fn_getDocket$(master_data$(1))
		master_data$(master_forw_refno)=fn_getEdiRefno$(master_data$(1))
		master_data$(master_jmt_no)=fn_getJmtNo$(master_data$(1))
		master_data$(master_forw_fileno)=fn_getFoFile$(master_data$(1))
		
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
	!open #hOut:=fngethandle: 'Name='&reportFile$&',RecL=1024,Replace',d,o 
	open #hInf:=fngethandle: "name=infinity.int//6,kfname=infinity.idx//6,shr",i,outIn,k
	Ffit_AskFile: !
	open #hOut:=fngethandle: "Name=SAVE:"&reportFile$&",RecL=1024,replace",d,o ioerr SAVE_AS_OPEN_ERR
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
		master_data$(master_docket_no)=fn_getDocket$(master_data$(1))
		master_data$(master_forw_refno)=fn_getEdiRefno$(master_data$(1))
		master_data$(master_jmt_no)=fn_getJmtNo$(master_data$(1))
		master_data$(master_forw_fileno)=fn_getFoFile$(master_data$(1))
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
message$&='dupeCount='&str$(dupeCount)&'\n'
message$&='elapsed time='&fnStime$(timeStop-timeStart)&'\n'
message$&='Output File Created:\n'
message$&=reportFile$
fnMessageBox(message$, Mbx_Type,env$('program_caption'))
fnXit
SAVE_AS_OPEN_ERR: ! r: there was a problem opening the file.
 if err<>622 then
  dim ml$(0)*256
  mat ml$(2)
  ml$(1)='Select a different file name.'
  ml$(2)='Error: '&str$(err)
  fnMsgBox(mat ml$)
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
		library 'S:\Core\Library.br': fnTop
		library 'S:\Core\Library.br': fnXit

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

def fn_getDocket$*30(docket_fileno$)
	dim docket_no$*30
	read #h_internal,using "form pos 27,C 30",key=docket_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",2)&"MAIN      ",release: docket_no$ nokey L17420 
	let fn_getDocket$=docket_no$ 
	! if trim$(docket_no$)="" and trim$(master_data$(master_docket_no))<>"" then 
	!  let fn_getDocket$=docket_no$=master_data$(master_docket_no) 
	!  let fnmessagebox("Warning, Internal Docket Number is missing\nUsing MASTER.DOCKET_NO instead\n"&docket_no$,64,"Problem with Docket/Case #") 
	!  rewrite #h_internal,using "form pos 27,C 30",key=docket_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",2)&"MAIN      ",release: docket_no$ nokey L17420
	! end if
	L17420: !
fnend 
def fn_getJmtNo$*30(jmt_fileno$)
	dim jmt_no$*30
	read #h_internal,using "form pos 27,C 30",key=jmt_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",2)&"JUDGMENT  ",release: jmt_no$ nokey JMN_XIT
	let fn_getJmtNo$=jmt_no$
	! if trim$(jmt_no$)="" and trim$(master_data$(master_jmt_no))<>"" then 
	!  let fn_getJmtNo$=jmt_no$=master_data$(master_jmt_no)
	!  let fnmessagebox("Warning, Internal Judgment Number is missing\nUsing MASTER.JMT_NO instead\n"&jmt_no$,mb_information+mb_okonly,"Problem with Judgment Number")
	!  rewrite #h_internal,using "form pos 27,C 30",key=jmt_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",2)&"JUDGMENT  ",release: jmt_no$ nokey JMN_XIT
	! end if  ! jmt_no$="" and master.jmt_no<>""
	JMN_XIT: ! 
fnend  ! fn_getJmtNo$
def fn_getEdiRefno$*30(edi_fileno$)
	dim edi_refno_no$*30
	read #h_internal,using "form pos 27,C 30",key=edi_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",28)&"MAIN      ",release: edi_refno_no$ nokey L17480 
	let fn_getEdiRefno$=edi_refno_no$ 
	! if trim$(edi_refno_no$)="" and trim$(master_data$(master_forw_refno))<>"" then 
	!  let fn_getEdiRefno$=edi_refno_no$=master_data$(master_forw_refno) 
	!  let fnmessagebox("Warning, Internal EDI_REFNO # is missing\nUsing MASTER.FORW_REFNO instead\n"&edi_refno_no$,64,"Problem with FORW_REFNO/Edi Ref #") 
	!  rewrite #h_internal,using "form pos 27,C 30",key=edi_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",28)&"MAIN      ",release: edi_refno_no$ nokey L17480
	! end if
	L17480: !
fnend 
def fn_getFoFile$*30(fofile_fileno$)
	dim fofile$*30
	read #h_internal,using "form pos 27,C 30",key=fofile_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",1)&"MAIN      ",release: fofile$ nokey L17496 
	let fn_getFoFile$=fofile$ 
	! if trim$(fofile$)="" and trim$(master_data$(master_forw_fileno))<>"" then 
	!  let fn_getFoFile$=fofile$=master_data$(master_forw_fileno) 
	!  let fnmessagebox("Warning, Internal Forw File Number is missing\nUsing MASTER.FORW_FILENO instead\n"&fofile$,64,"Problem with Forw Fileno #") 
	!  rewrite #h_internal,using "form pos 27,C 30",key=fofile_fileno$&cnvrt$("N 3",0)&cnvrt$("N 2",1)&"MAIN      ",release: fofile$ nokey L17496
	! end if
	L17496: !
fnend 
include: ertn
include: cm\enum\master