enableOpen=1
enableClosed=1
enablePaperlessScan=1
enableMatchingAccountsOnly=1
! Trak SOC Project Report
! 
! email nick, trevor, angie, jacob and kirk, and tina, greg
! 
! I NEED THIS LIST OF ACCOUNTS. 
! There is no reason you should not be able to 
! 
! process open and closed
! 
! (list any account in their files that we didn't list)
! (identify which account we list that are not in their files)
! 
! identify accounts where you failed to 
! identify that an SOC needed to be filed; 
! 	SOC = Substitution of Council
! 	any account (open) with a TRAK Forwarder (fowarder - any in list:  "W:\OLD_F_DRIVE\TRAK\PERFORMANCE MANAGEMENT\Forwarder #'s\TRAK-Forwarder #'s.txt")
! 	
! 	master.suit_date >0
! 	master.suit_date - 
! 	
! 	Served Date:
! 		If 2 debtors then use D2.service_date
! 		If 1 debtors then use D1.service_date
! 	if there's a date there - they got served.
! 		
! 	in paperless - scan for:
! 		has "pkmsad"
! 		has "xuts"
! 	if they have "xuts" then
! 		they did not get served.
! 	
! 	and date dismissed > master.suit_date
! 	
! 	debtor(1,2).dsmis_date
! 	debtor(1,2).jmt_date
! 	master.jmt_date
! 	
! 	Requires Refiling (column)
! 		Yes if date_dismmissed>suit_date
! 
! 
! then from there identify the accounts that were dismissed '
! and required refiling
! 
! as well as the accounts they did not require a dismissal
!  because the court allowed a motion to set aside. 
!  
! TRAKAmerica is extremely concerned 
! about this for all of our clients not just Midland. 
! 
! I need a list of all accounts impacted, 
! if they were dismissed,
!  if the court allowed a motion to set aside, 
!  if the dismissal remained and accounts was closed 
!  and what client. 
! 
! Please put together and get 
! me a spreadsheet with all of this information by the 
! end of the week. 
! This has been escalated to our executive team and 
! they want answers. 
! This is a huge risk.
! Attached are accounts that were part of the 
! Berman transfer if it helps you identify. 


on error goto Error_Hanler
fn_setup
fnTop(program$)
! r: Set Defaults and Filters
	mat forwarderFilter$(0)
	Fnasci('Collection-Master Add-On\Forwarder Lists\TRAK.txt',mat forwarderFilter$)
	! Fnasci("W:\OLD_F_DRIVE\TRAK\PERFORMANCE MANAGEMENT\Forwarder #'s\TRAK-Forwarder #'s.txt",mat forwarderFilter$)
	! dim forwarderFilter$*256
	! mat v(udim(mat forwarderFilter$))
	! for x=1 to udim(mat forwarderFilter$)
	! 	v(x)=val(forwarderFilter$(x))
	! nex x
	! forwarderFilter$=fnArray_to_range$(mat v)
	
	mat CodeToFind$(0)
	fnAddOneC(mat CodeToFind$,'PKMSAD')
	! fnAddOneC(mat CodeToFind$,'XPKMSAD') !  just for testing
	! fnAddOneC(mat CodeToFind$,'*PKMSAD') !  just for testing
	fnAddOneC(mat CodeToFind$,'XUTS')
	fnAddOneC(mat CodeToFind$,'XDWOP')
	fnAddOneC(mat CodeToFind$,'PKDIS')
	fnAddOneC(mat CodeToFind$,'PMDIS')
	fnAddOneC(mat CodeToFind$,'PSUBATKS')
	fnAddOneC(mat CodeToFind$,'PKENTRY')
	fnAddOneC(mat CodeToFind$,'PKSSUBD')
	fnAddOneC(mat CodeToFind$,'PMOSUBD')
	fnAddOneC(mat CodeToFind$,'PSUBATMO')
	fnAddOneC(mat CodeToFind$,'XSUT')
! /r

! r: Screens
	fnTos(sn$='tspr')
	dim resp$(64)*256
	fnChk(2,2,'enable Open Claims')
	if enableOpen then resp$(1)='True' else resp$(1)='False'
	fnChk(3,2,'enable Closed Claims')
	if enableClosed then resp$(2)='True' else resp$(2)='False'
	fnChk(4,2,'enable Paperless Scan')
	if enablePaperlessScan then resp$(3)='True' else resp$(3)='False'
	fnChk(6,2,'Include matching accounts only')
	if enableMatchingAccountsOnly then resp$(4)='True' else resp$(4)='False'
	! fnLbl(2,2,'Forwarder Number(s):',20,1)
	! fnTxt(2,24,40,256)
	! resp$(1)=forwarderFilter$
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto Xit
	if resp$(1)='True' then enableOpen=1 else enableOpen=0
	if resp$(2)='True' then enableClosed=1 else enableClosed=0
	if resp$(3)='True' then enablePaperlessScan=1 else enablePaperlessScan=0
	if resp$(4)='True' then enableMatchingAccountsOnly=1 else enableMatchingAccountsOnly=0
	
	fnSel(1024, 'Select Output for '&env$('cap') ,255, 'Cancel','HTML',env$('cap'))
	if fkey=93 or fkey=99 then goto Xit
	
! /r

! r: (onetime only) Header
	pr #255: '</pre>'
	pr #255: '<table align="Center">'
	pr #255: '<tr>'
	pr #255: '<td align="Center"><h2>'&env$('program_caption')&'</h2></td>'
	pr #255: '</tr>'
	pr #255: '<tr>'
	pr #255: '<td  align="Center">As of '&fnDate_rpt10$(Date$)&'.</td>'
	if enableOpen and enableClosed then
		pr #255: '<td  align="Center">Includes both Open and Closed claims.</td>'
	else if enableOpen and ~enableClosed then
		pr #255: '<td  align="Center">Includes only Open claims.</td>'
	else if ~enableOpen and enableClosed then
		pr #255: '<td  align="Center">Includes only Closed claims.</td>'
	else if ~enableOpen and ~enableClosed then
		pr #255: '<td  align="Center">Includes neither Open nor Closed claims.</td>'
	end if
	if ~enablePaperlessScan then
		pr #255: '<td  align="Center">Paperless Scan disabled.</td>'
	end if
	pr #255: '</tr>'
	if forwarderFilter$<>'' then
		pr #255: '<tr>'
		pr #255: '  <td align="Center">Forwarder Filter: '&forwarderFilter$&'</td>'
		pr #255: '</tr>'
	end if
	pr #255: '</table>'
	! /r
! r: Accumulate that Data
	! masterKey$=  "forwarder number here"
	dim listForwFileno$        	(0)*64
	dim listDiary$              	(0)*1024
	
	
	mat listFileno$            	(0)
	mat listForwFileno$        	(0)
	mat listForwarder$         	(0)
	mat listOpenedDate$       	(0)
	mat listBalance            	(0)
	mat listClosedDate$       	(0)
	mat listDiary$             	(0)
	mat listOC$                 	(0)
	mat listProvided$          	(0)
	mat listMasterSuitDate$   	(0)
	mat listD1HearingDate$    	(0)
	mat listD2HearingDate$    	(0)
	mat listD1ServiceDate$    	(0)
	mat listD2ServiceDate$    	(0)
	mat listD1DismissDate$    	(0)
	mat listD2DismissDate$    	(0)
	mat listD2RequiresRefile$ 	(0)
	if enableClosed then ocTop=2 else ocTop=1
	if enableOpen then ocBottom=1 else ocBottom=2
	for oc=ocBottom to ocTop
		if oc=1 then
			open #hM:=fngethandle: 'name=master//6,kfname=masterx//6,shr',internal,input,keyed
			open #hDebtor:=fngethandle: 'name=debtor//6,kfname=debtor.idx//6,shr',internal,input,keyed
		else if oc=2 then
			open #hM:=fngethandle: 'name=history//1,kfname=historyx//1,shr',internal,input,keyed
			open #hDebtor:=fngethandle: 'name=debtor//1,kfname=debtor.idx//1,shr',internal,input,keyed
		end if
		open #hInf:=fngethandle: 'name=infinity.int//6,kfname=infinity.idx//6,shr',internal,input,keyed
		! r: gather claims and their data
		fn_listPrint('Stage '&str$(stageCount+=1)&' - gather '&oc$(oc)&' claims')
		restore #hM: ! ,key=>masterKey$:
		comLine=5


		do
			read #hM,using mFormAll$: mat masterData$,mat masterDataN eof EoMaster
			countReadMaster+=1
			! pr f '24,2,c 70,[T]': 'countReadMaster='&str$(countReadMaster)
			fncom(countReadMaster,lrec(hM), comLine)
			dim fileno$*8
			fileno$=masterData$(master_fileno)
			forwN=masterDataN(master_forw_no)
			forw$=str$(forwN)
			dim forwFileno$*64
			forwFileno$=''
			fnget_inf_claim(hInf,fileno$,Mat Inf_Code$,Mat Inf_Type$,Mat Inf_Value$,Mat Inf_Value)
			whichTrak=srch(mat Inf_Code$,uprc$('*TrakFNo'))
			if whichTrak>0 then
			forwFileno$=Inf_Value$(whichTrak)
			end if
			trim$(masterData$(master_forw_fileno))
			! if fileno$='14899402' then pr 'fileno$='&fileno$ : pause
			if srch(mat forwarderFilter$,forw$)>0 and (~enableMatchingAccountsOnly or fn_yesIfInList$(forwFileno$)='Yes')  then
			! if srch(mat forwarderFilter$,forw$)>0 and fn_yesIfInList$(forwFileno$)='Yes'  then
				countMatchMaster+=1
				if countMatchMaster/1000=int(countMatchMaster/1000) then
					pr f '2,2,c 70,[T]': 'countMatchMaster='&str$(countMatchMaster)
				end if
				! r: master
					fnAddOneC(mat listFileno$,fileno$)
					fnAddOneC(mat listForwFileno$,forwFileno$)
					fnAddOneC(mat listForwarder$,forw$)
					fnAddOneC(mat listOpenedDate$,str$(masterDataN(master_opened_date)))
					fnAddOneN(mat listBalance,masterDataN(master_balance))
					fnAddOneC(mat listClosedDate$,masterData$(master_closed_date))
					fnAddOneC(mat listDiary$,'')
					fnAddOneC(mat listOC$              	,oc$(oc))
					fnAddOneC(mat listProvided$       	,fn_yesIfInList$(forwFileno$))
					fnAddOneC(mat listMasterSuitDate$ 	,str$(masterDataN(master_suit_date)))
				! /r
				! r: Debtor
					dim allDebtor$(0,0)*256,allDebtorN(0,0)
					debtorCount=fnAllDebtors(fileno$,hDebtor,Mat allDebtor$,Mat allDebtorN)
					! allDebtor$(dno,debtor_*[enum])
					if debtorCount=>1 then let fnAddOneC(mat listD1HearingDate$     		,str$(allDebtorN(1,debtor_hearing_date)))         else let fnAddOneC(mat listD1HearingDate$     	,'')
					if debtorCount=>2 then let fnAddOneC(mat listD2HearingDate$     		,str$(allDebtorN(2,debtor_hearing_date)))         else let fnAddOneC(mat listD2HearingDate$     	,'')
					if debtorCount=>1 then let fnAddOneC(mat listD1ServiceDate$  	,str$(allDebtorN(1,debtor_service_date)))    else let fnAddOneC(mat listD1ServiceDate$  	,'')
					if debtorCount=>2 then let fnAddOneC(mat listD2ServiceDate$  	,str$(allDebtorN(2,debtor_service_date)))    else let fnAddOneC(mat listD2ServiceDate$  	,'')
					if debtorCount=>1 then let fnAddOneC(mat listD1DismissDate$  	,str$(allDebtorN(1,debtor_dsmis_date)))      else let fnAddOneC(mat listD1DismissDate$  	,'')
					if debtorCount=>2 then let fnAddOneC(mat listD2DismissDate$  	,str$(allDebtorN(2,debtor_dsmis_date)))      else let fnAddOneC(mat listD2DismissDate$  	,'')
				! /r
				if date_dismmissed>suit_date then
					fnAddOneC(mat listD2RequiresRefile$,'Yes')
				else
					fnAddOneC(mat listD2RequiresRefile$,'No' )
				end if
			end if
		loop
		EoMaster: !
		close #hM:
		close #hDebtor:
		close #hInf:
		fncom(lrec(hM),lrec(hM), comLine)
		! /r
		! r: Stage 2 - scan paperless - RESTORE FILE for each claim approach - should be faster
		if enablePaperlessScan then
			fn_listPrint('Stage '&str$(stageCount+=1)&' - scan '&oc$(oc)&' paperless')
			comLine+=1
			if oc=1 then
				open #hActive:=fngethandle: "Name=Active.int//6,KFName=Active.idx//6,Shr",internal,input,keyed ! RecL=92,KPs=1,KLn=8,Shr
			else if oc=2 then
				open #hActive:=fngethandle: "Name=Active.int//1,KFName=Active.idx//1,Shr",internal,input,keyed ! RecL=92,KPs=1,KLn=8,Shr
			end if
			for claim=1 to udim(mat listFileno$)
				fncom(claim,udim(mat listFileno$), comLine)
				if listOC$(claim)=oc$(oc) then
					fileno$=listFileno$(claim)
					restore #hActive,key=>rpad$(fileno$,kln(hActive)): noKey S2b_nextClaim
					do
						read #hActive,using active_FormAll$: mat active_data$,mat active_data eof S2b_nextClaim
						countReadActive+=1
						if fileno$=trim$(active_data$(active_fileno)) then
							whichCode=srch(mat CodeToFind$,trim$(active_data$(active_code)))
							if whichCode>0 then
								whichListItem=srch(mat listFileno$,active_data$(active_fileno))
								listDiary$(whichListItem)=listDiary$(whichListItem)&'<br>'&CodeToFind$(whichCode)&' ('&str$(active_data(active_trans_date))&')'
								if listDiary$(whichListItem)(1:4)='<br>' then listDiary$(whichListItem)(1:4)=''
								! if listDiary$(whichListItem)(len(listDiary$(whichListItem)):len(listDiary$(whichListItem)))='<br>' then listDiary$(whichListItem)(len(listDiary$(whichListItem)):len(listDiary$(whichListItem)))=''
							end if
			
						end if
					loop while fileno$=trim$(active_data$(active_fileno))
				end if
				S2b_nextClaim: !
				fncom(claim,udim(mat listFileno$), comLine)
			nex claim
			fncom(udim(mat listFileno$),udim(mat listFileno$), comLine)
			close #hActive:
		end if
		! /r
	next oc
	close #hCoco:
! /r
! r: Print the Accumulated Data in Excel
	fn_listPrint('Stage  '&str$(stageCount+=1)&' - produce report')
	comLine+=1
	! pr #255: '<table cellpadding=10 border=1>'
	! pr #255: '  <tr><th>Category</th><th>Total</th></tr>'
	! fn_finisAddRow('Master Read Count',str$(countReadMaster))
	! fn_finisAddRow('Master Match Count',str$(countMatchMaster))
	! fn_finisAddRow('Active Read Count',str$(countReadActive))
	! fn_finisAddRow('Active Match Count',str$(countMatchActive))
	! pr #255: '</table>'
	
	fn_listPrint(' Master Read Count: '&str$(countReadMaster))
	fn_listPrint('Master Match Count: '&str$(countMatchMaster))
	fn_listPrint(' Active Read Count: '&str$(countReadActive))
	fn_listPrint('Active Match Count: '&str$(countMatchActive))
	
	
	
	pr #255: '<table cellpadding=10 border=1>'
	! r: Pr Header
		pr #255: '<tr>'
		pr #255: '  <th>Fileno                    </th>'
		pr #255: '  <th>Forwarder<br>File No      </th>'
		pr #255: '  <th>Forwarder                 </th>'
		pr #255: '  <th>Opened<br>Date            </th>'
		pr #255: '  <th>Balance                   </th>'
		pr #255: '  <th>Closed<br>Date            </th>'
		pr #255: '  <th>Diary                     </th>'
		pr #255: '  <th>OC                        </th>'
		pr #255: '  <th>Provided                  </th>'
		pr #255: '  <th>Master<br>Suit Date       </th>'
		pr #255: '  <th>D1<br>Hearing Date        </th>'
		pr #255: '  <th>D2<br>Hearing Date        </th>'
		pr #255: '  <th>D1<br>Service Date        </th>'
		pr #255: '  <th>D2<br>Service Date        </th>'
		pr #255: '  <th>D1<br>Dismiss Date        </th>'
		pr #255: '  <th>D2<br>Dismiss Date        </th>'
		pr #255: '  <th>D2<br>Requires Refile     </th>'
	! /r
	
	For item=1 to udim(mat listFileno$)
		fncom(item,udim(mat listFileno$),comLine)
		! r: pr Line
		pr #255: '  <tr> ';
		pr #255: '    <td align="right">'&listFileno$           (item)&'</td>';
		pr #255: '    <td align="right">'&listForwFileno$       (item)&'</td>';
		pr #255: '    <td align="right">'&listForwarder$        (item)&'</td>';
		pr #255: '    <td align="right">'&fn_dateFormat$(listOpenedDate$         		(item))&'</td>';
		pr #255: '    <td align="right">'&cnvrt$('pic(---,---,---,--z.zz)',listBalance(item))&'</td>';
		pr #255: '    <td align="right">'&fn_dateFormat$(listClosedDate$     	(item))&'</td>';
		pr #255: '    <td align="right">'&listDiary$            (item)&'</td>';
		pr #255: '    <td align="right">'&listOC$               (item)&'</td>';
		pr #255: '    <td align="right">'&listProvided$         (item)&'</td>';
		pr #255: '    <td align="right">'&fn_dateFormat$(listMasterSuitDate$ 	(item))&'</td>';
		pr #255: '    <td align="right">'&fn_dateFormat$(listD1HearingDate$     	(item))&'</td>';
		pr #255: '    <td align="right">'&fn_dateFormat$(listD2HearingDate$     	(item))&'</td>';
		pr #255: '    <td align="right">'&fn_dateFormat$(listD1ServiceDate$  	(item))&'</td>';
		pr #255: '    <td align="right">'&fn_dateFormat$(listD2ServiceDate$  	(item))&'</td>';
		pr #255: '    <td align="right">'&fn_dateFormat$(listD1DismissDate$  	(item))&'</td>';
		pr #255: '    <td align="right">'&fn_dateFormat$(listD2DismissDate$  	(item))&'</td>';
		pr #255: '    <td align="right">'&listD2RequiresRefile$(item)&'</td>';
		pr #255: '  </tr> '
		! /r
	nex item
	pr #255: '</table>'
! /r
fnClose
fncom(udim(mat listFileno$),udim(mat listFileno$),comLine)
goto Xit ! /r
def fn_dateFormat$(ccyymmdd$; ___,return$)
	if ccyymmdd$='0' or ccyymmdd$='' then
		return$=''
	else
		return$=ccyymmdd$ ! date$(days(ccyymmdd$,'ccyymmdd'),'mm/dd/ccyy')
	end if
	fn_dateFormat$=return$
fnend
def fn_yesIfInList$(forwFileNo$*128; ___,return$)
	if ~yiilSetup then
		yiilSetup=1
		! r: build mat yiiList$
			dim yiiList$(0)*12
			mat yiiList$(0)
			fnAddOneC(mat yiiList$,'A106508')
			fnAddOneC(mat yiiList$,'A114864')
			fnAddOneC(mat yiiList$,'A116760')
			fnAddOneC(mat yiiList$,'A126989')
			fnAddOneC(mat yiiList$,'A130959')
			fnAddOneC(mat yiiList$,'A135896')
			fnAddOneC(mat yiiList$,'A137219')
			fnAddOneC(mat yiiList$,'A142467')
			fnAddOneC(mat yiiList$,'A196292')
			fnAddOneC(mat yiiList$,'A196594')
			fnAddOneC(mat yiiList$,'A203416')
			fnAddOneC(mat yiiList$,'A212646')
			fnAddOneC(mat yiiList$,'A253007')
			fnAddOneC(mat yiiList$,'A253016')
			fnAddOneC(mat yiiList$,'A253898')
			fnAddOneC(mat yiiList$,'A253996')
			fnAddOneC(mat yiiList$,'A256296')
			fnAddOneC(mat yiiList$,'A260803')
			fnAddOneC(mat yiiList$,'A271417')
			fnAddOneC(mat yiiList$,'A277805')
			fnAddOneC(mat yiiList$,'A281136')
			fnAddOneC(mat yiiList$,'A287516')
			fnAddOneC(mat yiiList$,'A289541')
			fnAddOneC(mat yiiList$,'A299170')
			fnAddOneC(mat yiiList$,'A309851')
			fnAddOneC(mat yiiList$,'A310868')
			fnAddOneC(mat yiiList$,'A326179')
			fnAddOneC(mat yiiList$,'A346338')
			fnAddOneC(mat yiiList$,'A356236')
			fnAddOneC(mat yiiList$,'A362624')
			fnAddOneC(mat yiiList$,'A370287')
			fnAddOneC(mat yiiList$,'A374435')
			fnAddOneC(mat yiiList$,'A391564')
			fnAddOneC(mat yiiList$,'A401012A')
			fnAddOneC(mat yiiList$,'A412150')
			fnAddOneC(mat yiiList$,'A412542')
			fnAddOneC(mat yiiList$,'A425129')
			fnAddOneC(mat yiiList$,'A425262')
			fnAddOneC(mat yiiList$,'A94359')
			fnAddOneC(mat yiiList$,'A94360')
			fnAddOneC(mat yiiList$,'A94463')
			fnAddOneC(mat yiiList$,'B128916')
			fnAddOneC(mat yiiList$,'B196279')
			fnAddOneC(mat yiiList$,'B199679')
			fnAddOneC(mat yiiList$,'B203264')
			fnAddOneC(mat yiiList$,'B211746A')
			fnAddOneC(mat yiiList$,'B212068')
			fnAddOneC(mat yiiList$,'B212170')
			fnAddOneC(mat yiiList$,'B212508')
			fnAddOneC(mat yiiList$,'B234603')
			fnAddOneC(mat yiiList$,'B247007')
			fnAddOneC(mat yiiList$,'B252095')
			fnAddOneC(mat yiiList$,'B255356')
			fnAddOneC(mat yiiList$,'B262272')
			fnAddOneC(mat yiiList$,'B262517')
			fnAddOneC(mat yiiList$,'B262683')
			fnAddOneC(mat yiiList$,'B262790')
			fnAddOneC(mat yiiList$,'B303763')
			fnAddOneC(mat yiiList$,'B309531')
			fnAddOneC(mat yiiList$,'B317115')
			fnAddOneC(mat yiiList$,'B321030')
			fnAddOneC(mat yiiList$,'B347520')
			fnAddOneC(mat yiiList$,'B378455')
			fnAddOneC(mat yiiList$,'B386810')
			fnAddOneC(mat yiiList$,'B391923')
			fnAddOneC(mat yiiList$,'B394539')
			fnAddOneC(mat yiiList$,'B404263')
			fnAddOneC(mat yiiList$,'B426446')
			fnAddOneC(mat yiiList$,'B471413')
			fnAddOneC(mat yiiList$,'B483603')
			fnAddOneC(mat yiiList$,'B483630')
			fnAddOneC(mat yiiList$,'B484453')
			fnAddOneC(mat yiiList$,'B484877')
			fnAddOneC(mat yiiList$,'B484878')
			fnAddOneC(mat yiiList$,'B484916')
			fnAddOneC(mat yiiList$,'B484985')
			fnAddOneC(mat yiiList$,'B484995')
			fnAddOneC(mat yiiList$,'B485041')
			fnAddOneC(mat yiiList$,'B485134')
			fnAddOneC(mat yiiList$,'B485161')
			fnAddOneC(mat yiiList$,'B485850')
			fnAddOneC(mat yiiList$,'B486108')
			fnAddOneC(mat yiiList$,'B486174')
			fnAddOneC(mat yiiList$,'B487267')
			fnAddOneC(mat yiiList$,'B492179')
			fnAddOneC(mat yiiList$,'B492279')
			fnAddOneC(mat yiiList$,'B507845')
			fnAddOneC(mat yiiList$,'B515270')
			fnAddOneC(mat yiiList$,'B520536')
			fnAddOneC(mat yiiList$,'B520631')
			fnAddOneC(mat yiiList$,'B531684')
			fnAddOneC(mat yiiList$,'B533879')
			fnAddOneC(mat yiiList$,'B540667')
			fnAddOneC(mat yiiList$,'B540669')
			fnAddOneC(mat yiiList$,'B540676')
			fnAddOneC(mat yiiList$,'B540714')
			fnAddOneC(mat yiiList$,'B552631')
			fnAddOneC(mat yiiList$,'B560999')
			fnAddOneC(mat yiiList$,'B578286')
			fnAddOneC(mat yiiList$,'B591057')
			fnAddOneC(mat yiiList$,'B622150')
			fnAddOneC(mat yiiList$,'B627656')
			fnAddOneC(mat yiiList$,'B627667')
			fnAddOneC(mat yiiList$,'B632825')
			fnAddOneC(mat yiiList$,'B638086')
			fnAddOneC(mat yiiList$,'B664358')
			fnAddOneC(mat yiiList$,'B666326')
			fnAddOneC(mat yiiList$,'B676493')
			fnAddOneC(mat yiiList$,'B694302')
			fnAddOneC(mat yiiList$,'B695261')
			fnAddOneC(mat yiiList$,'B698584')
			fnAddOneC(mat yiiList$,'B703385')
			fnAddOneC(mat yiiList$,'B710087')
			fnAddOneC(mat yiiList$,'B717385')
			fnAddOneC(mat yiiList$,'B717501')
			fnAddOneC(mat yiiList$,'B722286')
			fnAddOneC(mat yiiList$,'B756519')
			fnAddOneC(mat yiiList$,'B765787')
			fnAddOneC(mat yiiList$,'B766633')
			fnAddOneC(mat yiiList$,'B789496')
			fnAddOneC(mat yiiList$,'B789938')
			fnAddOneC(mat yiiList$,'B790064')
			fnAddOneC(mat yiiList$,'B790398')
			fnAddOneC(mat yiiList$,'B790458')
			fnAddOneC(mat yiiList$,'B797886A')
			fnAddOneC(mat yiiList$,'B816780')
			fnAddOneC(mat yiiList$,'B816819')
			fnAddOneC(mat yiiList$,'B816839')
			fnAddOneC(mat yiiList$,'B816954')
			fnAddOneC(mat yiiList$,'B817027')
			fnAddOneC(mat yiiList$,'B817077')
			fnAddOneC(mat yiiList$,'B817172')
			fnAddOneC(mat yiiList$,'B817177')
			fnAddOneC(mat yiiList$,'B817180')
			fnAddOneC(mat yiiList$,'C123732')
			fnAddOneC(mat yiiList$,'C134827')
			fnAddOneC(mat yiiList$,'C158388')
			fnAddOneC(mat yiiList$,'C163709')
			fnAddOneC(mat yiiList$,'C166415')
			fnAddOneC(mat yiiList$,'C183359')
			fnAddOneC(mat yiiList$,'C194878')
			fnAddOneC(mat yiiList$,'C209166')
			fnAddOneC(mat yiiList$,'C216197')
			fnAddOneC(mat yiiList$,'C216509')
			fnAddOneC(mat yiiList$,'C235702')
			fnAddOneC(mat yiiList$,'C235768')
			fnAddOneC(mat yiiList$,'C235899')
			fnAddOneC(mat yiiList$,'C268844')
			fnAddOneC(mat yiiList$,'C271598')
			fnAddOneC(mat yiiList$,'C276954')
			fnAddOneC(mat yiiList$,'C282177')
			fnAddOneC(mat yiiList$,'C286193')
			fnAddOneC(mat yiiList$,'C289037')
			fnAddOneC(mat yiiList$,'C314168')
			fnAddOneC(mat yiiList$,'C345796')
			fnAddOneC(mat yiiList$,'C357560')
			fnAddOneC(mat yiiList$,'C362689')
			fnAddOneC(mat yiiList$,'C401164')
			fnAddOneC(mat yiiList$,'C416611')
			fnAddOneC(mat yiiList$,'C435634')
			fnAddOneC(mat yiiList$,'C435943')
			fnAddOneC(mat yiiList$,'C445415')
			fnAddOneC(mat yiiList$,'C448656')
			fnAddOneC(mat yiiList$,'C448749')
			fnAddOneC(mat yiiList$,'C451334')
			fnAddOneC(mat yiiList$,'C463625')
			fnAddOneC(mat yiiList$,'C479171')
			fnAddOneC(mat yiiList$,'C481907')
			fnAddOneC(mat yiiList$,'C487926')
			fnAddOneC(mat yiiList$,'C496703')
			fnAddOneC(mat yiiList$,'C500633')
			fnAddOneC(mat yiiList$,'C500750')
			fnAddOneC(mat yiiList$,'C500758')
			fnAddOneC(mat yiiList$,'C500775')
			fnAddOneC(mat yiiList$,'C500865')
			fnAddOneC(mat yiiList$,'C500878')
			fnAddOneC(mat yiiList$,'C500913')
			fnAddOneC(mat yiiList$,'C501039')
			fnAddOneC(mat yiiList$,'C515333')
			fnAddOneC(mat yiiList$,'C548157')
			fnAddOneC(mat yiiList$,'C551987')
			fnAddOneC(mat yiiList$,'C583249')
			fnAddOneC(mat yiiList$,'C593968')
			fnAddOneC(mat yiiList$,'C603122')
			fnAddOneC(mat yiiList$,'C619372')
			fnAddOneC(mat yiiList$,'C647363')
			fnAddOneC(mat yiiList$,'C650854')
			fnAddOneC(mat yiiList$,'C650892')
			fnAddOneC(mat yiiList$,'C651047')
			fnAddOneC(mat yiiList$,'C656231')
			fnAddOneC(mat yiiList$,'C657141')
			fnAddOneC(mat yiiList$,'C667923')
			fnAddOneC(mat yiiList$,'C696780A')
			fnAddOneC(mat yiiList$,'C712085')
			fnAddOneC(mat yiiList$,'C712093')
			fnAddOneC(mat yiiList$,'C712100')
			fnAddOneC(mat yiiList$,'C712101')
			fnAddOneC(mat yiiList$,'C712988')
			fnAddOneC(mat yiiList$,'C734000')
			fnAddOneC(mat yiiList$,'C734034')
			fnAddOneC(mat yiiList$,'C734275')
			fnAddOneC(mat yiiList$,'C734404')
			fnAddOneC(mat yiiList$,'C734587')
			fnAddOneC(mat yiiList$,'C734594')
			fnAddOneC(mat yiiList$,'C734619')
			fnAddOneC(mat yiiList$,'C734902')
			fnAddOneC(mat yiiList$,'C735165')
			fnAddOneC(mat yiiList$,'C735281')
			fnAddOneC(mat yiiList$,'C735632')
			fnAddOneC(mat yiiList$,'C741421')
			fnAddOneC(mat yiiList$,'C758583')
			fnAddOneC(mat yiiList$,'C758634')
			fnAddOneC(mat yiiList$,'C758692')
			fnAddOneC(mat yiiList$,'C758892')
			fnAddOneC(mat yiiList$,'C758899')
			fnAddOneC(mat yiiList$,'D135804')
			fnAddOneC(mat yiiList$,'D136005')
			fnAddOneC(mat yiiList$,'D136073')
			fnAddOneC(mat yiiList$,'D161643')
			fnAddOneC(mat yiiList$,'D164287')
			fnAddOneC(mat yiiList$,'D175480')
			fnAddOneC(mat yiiList$,'D179870')
			fnAddOneC(mat yiiList$,'D206188')
			fnAddOneC(mat yiiList$,'D218372')
			fnAddOneC(mat yiiList$,'D218472')
			fnAddOneC(mat yiiList$,'D219823')
			fnAddOneC(mat yiiList$,'D224244')
			fnAddOneC(mat yiiList$,'D266047')
			fnAddOneC(mat yiiList$,'D275933')
			fnAddOneC(mat yiiList$,'D276012')
			fnAddOneC(mat yiiList$,'D276757')
			fnAddOneC(mat yiiList$,'D281773')
			fnAddOneC(mat yiiList$,'D288817')
			fnAddOneC(mat yiiList$,'D292899')
			fnAddOneC(mat yiiList$,'D292938')
			fnAddOneC(mat yiiList$,'D307121')
			fnAddOneC(mat yiiList$,'D307251')
			fnAddOneC(mat yiiList$,'D337433')
			fnAddOneC(mat yiiList$,'D342874')
			fnAddOneC(mat yiiList$,'D353683')
			fnAddOneC(mat yiiList$,'D355962')
			fnAddOneC(mat yiiList$,'D357808')
			fnAddOneC(mat yiiList$,'D373232')
			fnAddOneC(mat yiiList$,'D384057')
			fnAddOneC(mat yiiList$,'D396471')
			fnAddOneC(mat yiiList$,'D399632')
			fnAddOneC(mat yiiList$,'D401722')
			fnAddOneC(mat yiiList$,'D402255')
			fnAddOneC(mat yiiList$,'D408535')
			fnAddOneC(mat yiiList$,'D410661')
			fnAddOneC(mat yiiList$,'D423153')
			fnAddOneC(mat yiiList$,'D447677')
			fnAddOneC(mat yiiList$,'D447704')
			fnAddOneC(mat yiiList$,'D447757')
			fnAddOneC(mat yiiList$,'D447851')
			fnAddOneC(mat yiiList$,'D447852')
			fnAddOneC(mat yiiList$,'D448108')
			fnAddOneC(mat yiiList$,'D462974')
			fnAddOneC(mat yiiList$,'D462980')
			fnAddOneC(mat yiiList$,'D463061')
			fnAddOneC(mat yiiList$,'D463125')
			fnAddOneC(mat yiiList$,'D463150')
			fnAddOneC(mat yiiList$,'D85872')
			fnAddOneC(mat yiiList$,'E106788')
			fnAddOneC(mat yiiList$,'E114898')
			fnAddOneC(mat yiiList$,'E122125')
			fnAddOneC(mat yiiList$,'E126812')
			fnAddOneC(mat yiiList$,'E131843')
			fnAddOneC(mat yiiList$,'E155534')
			fnAddOneC(mat yiiList$,'E160245')
			fnAddOneC(mat yiiList$,'E179409')
			fnAddOneC(mat yiiList$,'E185195')
			fnAddOneC(mat yiiList$,'E185197')
			fnAddOneC(mat yiiList$,'E185203')
			fnAddOneC(mat yiiList$,'E185230')
			fnAddOneC(mat yiiList$,'E185234')
			fnAddOneC(mat yiiList$,'E185243')
			fnAddOneC(mat yiiList$,'E185259')
			fnAddOneC(mat yiiList$,'E185272')
			fnAddOneC(mat yiiList$,'E185273')
			fnAddOneC(mat yiiList$,'E185278')
			fnAddOneC(mat yiiList$,'E32577')
			fnAddOneC(mat yiiList$,'E46256')
			fnAddOneC(mat yiiList$,'E69073')
			fnAddOneC(mat yiiList$,'E69150')
			fnAddOneC(mat yiiList$,'F120465')
			fnAddOneC(mat yiiList$,'F121989')
			fnAddOneC(mat yiiList$,'F178183')
			fnAddOneC(mat yiiList$,'F185580')
			fnAddOneC(mat yiiList$,'F193548')
			fnAddOneC(mat yiiList$,'F193572')
			fnAddOneC(mat yiiList$,'F193612')
			fnAddOneC(mat yiiList$,'F193784')
			fnAddOneC(mat yiiList$,'F194445')
			fnAddOneC(mat yiiList$,'F194447')
			fnAddOneC(mat yiiList$,'F194723')
			fnAddOneC(mat yiiList$,'F204149')
			fnAddOneC(mat yiiList$,'F212833')
			fnAddOneC(mat yiiList$,'F215590')
			fnAddOneC(mat yiiList$,'F215600')
			fnAddOneC(mat yiiList$,'F215687')
			fnAddOneC(mat yiiList$,'F217574')
			fnAddOneC(mat yiiList$,'F226291')
			fnAddOneC(mat yiiList$,'F237013')
			fnAddOneC(mat yiiList$,'F248204')
			fnAddOneC(mat yiiList$,'F269706')
			fnAddOneC(mat yiiList$,'F285416')
			fnAddOneC(mat yiiList$,'F285684')
			fnAddOneC(mat yiiList$,'F300735')
			fnAddOneC(mat yiiList$,'F304505')
			fnAddOneC(mat yiiList$,'F313733')
			fnAddOneC(mat yiiList$,'F314041')
			fnAddOneC(mat yiiList$,'F314092')
			fnAddOneC(mat yiiList$,'F314276')
			fnAddOneC(mat yiiList$,'F314489')
			fnAddOneC(mat yiiList$,'F314602')
			fnAddOneC(mat yiiList$,'F324477')
			fnAddOneC(mat yiiList$,'F324529')
			fnAddOneC(mat yiiList$,'F42112')
			fnAddOneC(mat yiiList$,'F88123')
			fnAddOneC(mat yiiList$,'G119767')
			fnAddOneC(mat yiiList$,'G121921')
			fnAddOneC(mat yiiList$,'G132888')
			fnAddOneC(mat yiiList$,'G147562')
			fnAddOneC(mat yiiList$,'G147808')
			fnAddOneC(mat yiiList$,'G151346')
			fnAddOneC(mat yiiList$,'G175770')
			fnAddOneC(mat yiiList$,'G175771')
			fnAddOneC(mat yiiList$,'G178060')
			fnAddOneC(mat yiiList$,'G186874')
			fnAddOneC(mat yiiList$,'G186879A')
			fnAddOneC(mat yiiList$,'G195886')
			fnAddOneC(mat yiiList$,'G240985')
			fnAddOneC(mat yiiList$,'G244362')
			fnAddOneC(mat yiiList$,'G246965')
			fnAddOneC(mat yiiList$,'G248932')
			fnAddOneC(mat yiiList$,'G250021')
			fnAddOneC(mat yiiList$,'G303974A')
			fnAddOneC(mat yiiList$,'G308591A')
			fnAddOneC(mat yiiList$,'G308745')
			fnAddOneC(mat yiiList$,'G313182')
			fnAddOneC(mat yiiList$,'G315415A')
			fnAddOneC(mat yiiList$,'G315486')
			fnAddOneC(mat yiiList$,'G315507')
			fnAddOneC(mat yiiList$,'G315520')
			fnAddOneC(mat yiiList$,'G315592')
			fnAddOneC(mat yiiList$,'G315650')
			fnAddOneC(mat yiiList$,'G320665')
			fnAddOneC(mat yiiList$,'G326253')
			fnAddOneC(mat yiiList$,'G326304')
			fnAddOneC(mat yiiList$,'G337499')
			fnAddOneC(mat yiiList$,'G343944')
			fnAddOneC(mat yiiList$,'G353312')
			fnAddOneC(mat yiiList$,'G388235')
			fnAddOneC(mat yiiList$,'G396883')
			fnAddOneC(mat yiiList$,'G416987')
			fnAddOneC(mat yiiList$,'G417098')
			fnAddOneC(mat yiiList$,'G452020')
			fnAddOneC(mat yiiList$,'G461973')
			fnAddOneC(mat yiiList$,'G465814')
			fnAddOneC(mat yiiList$,'G469845')
			fnAddOneC(mat yiiList$,'G478455')
			fnAddOneC(mat yiiList$,'G506107A')
			fnAddOneC(mat yiiList$,'G520288')
			fnAddOneC(mat yiiList$,'G520414')
			fnAddOneC(mat yiiList$,'G525266')
			fnAddOneC(mat yiiList$,'G537045')
			fnAddOneC(mat yiiList$,'G537049')
			fnAddOneC(mat yiiList$,'G537140')
			fnAddOneC(mat yiiList$,'G537207')
			fnAddOneC(mat yiiList$,'G537238')
			fnAddOneC(mat yiiList$,'G537270')
			fnAddOneC(mat yiiList$,'H154484')
			fnAddOneC(mat yiiList$,'H171140')
			fnAddOneC(mat yiiList$,'H176357')
			fnAddOneC(mat yiiList$,'H176483')
			fnAddOneC(mat yiiList$,'H176668')
			fnAddOneC(mat yiiList$,'H176724')
			fnAddOneC(mat yiiList$,'H176754')
			fnAddOneC(mat yiiList$,'H176956')
			fnAddOneC(mat yiiList$,'H190692')
			fnAddOneC(mat yiiList$,'H203964')
			fnAddOneC(mat yiiList$,'H220774')
			fnAddOneC(mat yiiList$,'H233754')
			fnAddOneC(mat yiiList$,'H236387')
			fnAddOneC(mat yiiList$,'H239949')
			fnAddOneC(mat yiiList$,'H244004')
			fnAddOneC(mat yiiList$,'H295076')
			fnAddOneC(mat yiiList$,'H373135')
			fnAddOneC(mat yiiList$,'H375729')
			fnAddOneC(mat yiiList$,'H375807')
			fnAddOneC(mat yiiList$,'H375827')
			fnAddOneC(mat yiiList$,'H375927')
			fnAddOneC(mat yiiList$,'H375948')
			fnAddOneC(mat yiiList$,'H376433')
			fnAddOneC(mat yiiList$,'H376965')
			fnAddOneC(mat yiiList$,'H376983')
			fnAddOneC(mat yiiList$,'H376994')
			fnAddOneC(mat yiiList$,'H377685')
			fnAddOneC(mat yiiList$,'H377755')
			fnAddOneC(mat yiiList$,'H377859')
			fnAddOneC(mat yiiList$,'H379043')
			fnAddOneC(mat yiiList$,'H381473')
			fnAddOneC(mat yiiList$,'H381553')
			fnAddOneC(mat yiiList$,'H394024')
			fnAddOneC(mat yiiList$,'H394095')
			fnAddOneC(mat yiiList$,'H401752')
			fnAddOneC(mat yiiList$,'H404156')
			fnAddOneC(mat yiiList$,'H409171')
			fnAddOneC(mat yiiList$,'H414589')
			fnAddOneC(mat yiiList$,'H416310')
			fnAddOneC(mat yiiList$,'H416433')
			fnAddOneC(mat yiiList$,'H416577')
			fnAddOneC(mat yiiList$,'H419724')
			fnAddOneC(mat yiiList$,'H419920')
			fnAddOneC(mat yiiList$,'H420005')
			fnAddOneC(mat yiiList$,'H420035')
			fnAddOneC(mat yiiList$,'H424218')
			fnAddOneC(mat yiiList$,'H437593')
			fnAddOneC(mat yiiList$,'H447310A')
			fnAddOneC(mat yiiList$,'H452860')
			fnAddOneC(mat yiiList$,'H460063')
			fnAddOneC(mat yiiList$,'H462150')
			fnAddOneC(mat yiiList$,'H480014')
			fnAddOneC(mat yiiList$,'H487631')
			fnAddOneC(mat yiiList$,'H488504')
			fnAddOneC(mat yiiList$,'H488967')
			fnAddOneC(mat yiiList$,'H518162')
			fnAddOneC(mat yiiList$,'H527568A')
			fnAddOneC(mat yiiList$,'H527585')
			fnAddOneC(mat yiiList$,'H532632')
			fnAddOneC(mat yiiList$,'H541643')
			fnAddOneC(mat yiiList$,'H544976')
			fnAddOneC(mat yiiList$,'H563106')
			fnAddOneC(mat yiiList$,'H563380')
			fnAddOneC(mat yiiList$,'H563405')
			fnAddOneC(mat yiiList$,'H617418')
			fnAddOneC(mat yiiList$,'H617626')
			fnAddOneC(mat yiiList$,'H618146')
			fnAddOneC(mat yiiList$,'H618177')
			fnAddOneC(mat yiiList$,'H618213')
			fnAddOneC(mat yiiList$,'H618261')
			fnAddOneC(mat yiiList$,'H618279')
			fnAddOneC(mat yiiList$,'H618351')
			fnAddOneC(mat yiiList$,'H618448')
			fnAddOneC(mat yiiList$,'H618458')
			fnAddOneC(mat yiiList$,'H618552')
			fnAddOneC(mat yiiList$,'H618696')
			fnAddOneC(mat yiiList$,'H618841')
			fnAddOneC(mat yiiList$,'H618842')
			fnAddOneC(mat yiiList$,'H618852')
			fnAddOneC(mat yiiList$,'H619106')
			fnAddOneC(mat yiiList$,'H638839')
			fnAddOneC(mat yiiList$,'H638870')
			fnAddOneC(mat yiiList$,'H638896')
			fnAddOneC(mat yiiList$,'H638930')
			fnAddOneC(mat yiiList$,'H638994')
			fnAddOneC(mat yiiList$,'I19619')
			fnAddOneC(mat yiiList$,'I21903A')
			fnAddOneC(mat yiiList$,'I35360')
			fnAddOneC(mat yiiList$,'I48304')
			fnAddOneC(mat yiiList$,'I49749')
			fnAddOneC(mat yiiList$,'J109589')
			fnAddOneC(mat yiiList$,'J117677')
			fnAddOneC(mat yiiList$,'J119794')
			fnAddOneC(mat yiiList$,'J119825')
			fnAddOneC(mat yiiList$,'J138477')
			fnAddOneC(mat yiiList$,'J152928')
			fnAddOneC(mat yiiList$,'J188184')
			fnAddOneC(mat yiiList$,'J188429')
			fnAddOneC(mat yiiList$,'J188765')
			fnAddOneC(mat yiiList$,'J194201')
			fnAddOneC(mat yiiList$,'J201386')
			fnAddOneC(mat yiiList$,'J209060')
			fnAddOneC(mat yiiList$,'J209102')
			fnAddOneC(mat yiiList$,'J209120')
			fnAddOneC(mat yiiList$,'J209126')
			fnAddOneC(mat yiiList$,'J209130')
			fnAddOneC(mat yiiList$,'J209163')
			fnAddOneC(mat yiiList$,'J221526')
			fnAddOneC(mat yiiList$,'J241730')
			fnAddOneC(mat yiiList$,'J242730')
			fnAddOneC(mat yiiList$,'J242804')
			fnAddOneC(mat yiiList$,'J246724')
			fnAddOneC(mat yiiList$,'J263581')
			fnAddOneC(mat yiiList$,'J264737')
			fnAddOneC(mat yiiList$,'J268106')
			fnAddOneC(mat yiiList$,'J268134')
			fnAddOneC(mat yiiList$,'J277770')
			fnAddOneC(mat yiiList$,'J277804')
			fnAddOneC(mat yiiList$,'J279073')
			fnAddOneC(mat yiiList$,'J279145A')
			fnAddOneC(mat yiiList$,'J295045')
			fnAddOneC(mat yiiList$,'J295582')
			fnAddOneC(mat yiiList$,'J304349')
			fnAddOneC(mat yiiList$,'J304351')
			fnAddOneC(mat yiiList$,'J304394')
			fnAddOneC(mat yiiList$,'J304448')
			fnAddOneC(mat yiiList$,'J304516')
			fnAddOneC(mat yiiList$,'J304521')
			fnAddOneC(mat yiiList$,'J304550')
			fnAddOneC(mat yiiList$,'J304894')
			fnAddOneC(mat yiiList$,'J314267')
			fnAddOneC(mat yiiList$,'J314341')
			fnAddOneC(mat yiiList$,'J314356')
			fnAddOneC(mat yiiList$,'J50172')
			fnAddOneC(mat yiiList$,'J78472')
			fnAddOneC(mat yiiList$,'J83702')
			fnAddOneC(mat yiiList$,'J83767')
			fnAddOneC(mat yiiList$,'K113047')
			fnAddOneC(mat yiiList$,'K174195')
			fnAddOneC(mat yiiList$,'K176462')
			fnAddOneC(mat yiiList$,'K177270')
			fnAddOneC(mat yiiList$,'K180204')
			fnAddOneC(mat yiiList$,'K197001')
			fnAddOneC(mat yiiList$,'K197049')
			fnAddOneC(mat yiiList$,'K215249')
			fnAddOneC(mat yiiList$,'K217160')
			fnAddOneC(mat yiiList$,'K236484')
			fnAddOneC(mat yiiList$,'K249122A')
			fnAddOneC(mat yiiList$,'K256799')
			fnAddOneC(mat yiiList$,'K261926')
			fnAddOneC(mat yiiList$,'K288704')
			fnAddOneC(mat yiiList$,'K288765')
			fnAddOneC(mat yiiList$,'K288803')
			fnAddOneC(mat yiiList$,'K289521')
			fnAddOneC(mat yiiList$,'K299341')
			fnAddOneC(mat yiiList$,'K299382')
			fnAddOneC(mat yiiList$,'K299384')
			fnAddOneC(mat yiiList$,'K299456')
			fnAddOneC(mat yiiList$,'K299457')
			fnAddOneC(mat yiiList$,'K55387')
			fnAddOneC(mat yiiList$,'K74874')
			fnAddOneC(mat yiiList$,'K79591')
			fnAddOneC(mat yiiList$,'K84834')
			fnAddOneC(mat yiiList$,'K86907')
			fnAddOneC(mat yiiList$,'K91260')
			fnAddOneC(mat yiiList$,'K93360')
			fnAddOneC(mat yiiList$,'L126576')
			fnAddOneC(mat yiiList$,'L126585')
			fnAddOneC(mat yiiList$,'L126726')
			fnAddOneC(mat yiiList$,'L126782')
			fnAddOneC(mat yiiList$,'L142577')
			fnAddOneC(mat yiiList$,'L165527')
			fnAddOneC(mat yiiList$,'L165691')
			fnAddOneC(mat yiiList$,'L165865')
			fnAddOneC(mat yiiList$,'L169836')
			fnAddOneC(mat yiiList$,'L202951')
			fnAddOneC(mat yiiList$,'L211314')
			fnAddOneC(mat yiiList$,'L213688')
			fnAddOneC(mat yiiList$,'L253908')
			fnAddOneC(mat yiiList$,'L259700')
			fnAddOneC(mat yiiList$,'L269639')
			fnAddOneC(mat yiiList$,'L288665')
			fnAddOneC(mat yiiList$,'L289883')
			fnAddOneC(mat yiiList$,'L298783')
			fnAddOneC(mat yiiList$,'L303153')
			fnAddOneC(mat yiiList$,'L305201')
			fnAddOneC(mat yiiList$,'L323555')
			fnAddOneC(mat yiiList$,'L326407')
			fnAddOneC(mat yiiList$,'L349634')
			fnAddOneC(mat yiiList$,'L352839')
			fnAddOneC(mat yiiList$,'L358223')
			fnAddOneC(mat yiiList$,'L371952')
			fnAddOneC(mat yiiList$,'L377391')
			fnAddOneC(mat yiiList$,'L390216')
			fnAddOneC(mat yiiList$,'L390227')
			fnAddOneC(mat yiiList$,'L395071')
			fnAddOneC(mat yiiList$,'L400194')
			fnAddOneC(mat yiiList$,'L437659')
			fnAddOneC(mat yiiList$,'L437678')
			fnAddOneC(mat yiiList$,'L437945')
			fnAddOneC(mat yiiList$,'L438060')
			fnAddOneC(mat yiiList$,'L438749')
			fnAddOneC(mat yiiList$,'L453143')
			fnAddOneC(mat yiiList$,'L453150')
			fnAddOneC(mat yiiList$,'L453166')
			fnAddOneC(mat yiiList$,'L453337')
			fnAddOneC(mat yiiList$,'M117361')
			fnAddOneC(mat yiiList$,'M117364')
			fnAddOneC(mat yiiList$,'M117417')
			fnAddOneC(mat yiiList$,'M205407')
			fnAddOneC(mat yiiList$,'M219938')
			fnAddOneC(mat yiiList$,'M237771')
			fnAddOneC(mat yiiList$,'M263543')
			fnAddOneC(mat yiiList$,'M287452')
			fnAddOneC(mat yiiList$,'M296731')
			fnAddOneC(mat yiiList$,'M296968')
			fnAddOneC(mat yiiList$,'M301199')
			fnAddOneC(mat yiiList$,'M329381')
			fnAddOneC(mat yiiList$,'M345041')
			fnAddOneC(mat yiiList$,'M361544')
			fnAddOneC(mat yiiList$,'M417774')
			fnAddOneC(mat yiiList$,'M429302')
			fnAddOneC(mat yiiList$,'M433237')
			fnAddOneC(mat yiiList$,'M436886')
			fnAddOneC(mat yiiList$,'M446399')
			fnAddOneC(mat yiiList$,'M459160')
			fnAddOneC(mat yiiList$,'M537981')
			fnAddOneC(mat yiiList$,'M549348')
			fnAddOneC(mat yiiList$,'M549506')
			fnAddOneC(mat yiiList$,'M549749')
			fnAddOneC(mat yiiList$,'M549754')
			fnAddOneC(mat yiiList$,'M549811')
			fnAddOneC(mat yiiList$,'M551060')
			fnAddOneC(mat yiiList$,'M551908')
			fnAddOneC(mat yiiList$,'M552594')
			fnAddOneC(mat yiiList$,'M554008')
			fnAddOneC(mat yiiList$,'M561264')
			fnAddOneC(mat yiiList$,'M568051')
			fnAddOneC(mat yiiList$,'M584302')
			fnAddOneC(mat yiiList$,'M590397')
			fnAddOneC(mat yiiList$,'M593269')
			fnAddOneC(mat yiiList$,'M597856')
			fnAddOneC(mat yiiList$,'M606103')
			fnAddOneC(mat yiiList$,'M613705')
			fnAddOneC(mat yiiList$,'M613824')
			fnAddOneC(mat yiiList$,'M613872')
			fnAddOneC(mat yiiList$,'M613928')
			fnAddOneC(mat yiiList$,'M619704')
			fnAddOneC(mat yiiList$,'M632281')
			fnAddOneC(mat yiiList$,'M637735')
			fnAddOneC(mat yiiList$,'M672227')
			fnAddOneC(mat yiiList$,'M675984')
			fnAddOneC(mat yiiList$,'M676508')
			fnAddOneC(mat yiiList$,'M676940')
			fnAddOneC(mat yiiList$,'M676951')
			fnAddOneC(mat yiiList$,'M677308')
			fnAddOneC(mat yiiList$,'M714292')
			fnAddOneC(mat yiiList$,'M714293')
			fnAddOneC(mat yiiList$,'M714838')
			fnAddOneC(mat yiiList$,'M727697')
			fnAddOneC(mat yiiList$,'M727818')
			fnAddOneC(mat yiiList$,'M727863')
			fnAddOneC(mat yiiList$,'M758813')
			fnAddOneC(mat yiiList$,'M777410')
			fnAddOneC(mat yiiList$,'M792609')
			fnAddOneC(mat yiiList$,'M796999')
			fnAddOneC(mat yiiList$,'M800095')
			fnAddOneC(mat yiiList$,'M800134')
			fnAddOneC(mat yiiList$,'M803148')
			fnAddOneC(mat yiiList$,'M803159')
			fnAddOneC(mat yiiList$,'M803172')
			fnAddOneC(mat yiiList$,'M803196')
			fnAddOneC(mat yiiList$,'M803199')
			fnAddOneC(mat yiiList$,'M803281')
			fnAddOneC(mat yiiList$,'M803295')
			fnAddOneC(mat yiiList$,'M817669')
			fnAddOneC(mat yiiList$,'M822871')
			fnAddOneC(mat yiiList$,'M825135')
			fnAddOneC(mat yiiList$,'M871416')
			fnAddOneC(mat yiiList$,'M871577')
			fnAddOneC(mat yiiList$,'M871580')
			fnAddOneC(mat yiiList$,'M898583')
			fnAddOneC(mat yiiList$,'M898659')
			fnAddOneC(mat yiiList$,'M898853')
			fnAddOneC(mat yiiList$,'M898869')
			fnAddOneC(mat yiiList$,'M898938')
			fnAddOneC(mat yiiList$,'M899019')
			fnAddOneC(mat yiiList$,'M899058')
			fnAddOneC(mat yiiList$,'M899337')
			fnAddOneC(mat yiiList$,'M899340')
			fnAddOneC(mat yiiList$,'M899384')
			fnAddOneC(mat yiiList$,'M900098')
			fnAddOneC(mat yiiList$,'M900419')
			fnAddOneC(mat yiiList$,'M900549')
			fnAddOneC(mat yiiList$,'M928538')
			fnAddOneC(mat yiiList$,'M928587')
			fnAddOneC(mat yiiList$,'M928591')
			fnAddOneC(mat yiiList$,'M928625')
			fnAddOneC(mat yiiList$,'M928668')
			fnAddOneC(mat yiiList$,'M928806')
			fnAddOneC(mat yiiList$,'M928879')
			fnAddOneC(mat yiiList$,'M928919')
			fnAddOneC(mat yiiList$,'M942376A')
			fnAddOneC(mat yiiList$,'N110491')
			fnAddOneC(mat yiiList$,'N118577')
			fnAddOneC(mat yiiList$,'N143109')
			fnAddOneC(mat yiiList$,'N165649')
			fnAddOneC(mat yiiList$,'N165650')
			fnAddOneC(mat yiiList$,'N176008')
			fnAddOneC(mat yiiList$,'N181172')
			fnAddOneC(mat yiiList$,'N181569')
			fnAddOneC(mat yiiList$,'N181659')
			fnAddOneC(mat yiiList$,'N187398')
			fnAddOneC(mat yiiList$,'N187468')
			fnAddOneC(mat yiiList$,'N39996')
			fnAddOneC(mat yiiList$,'N55353')
			fnAddOneC(mat yiiList$,'N56423')
			fnAddOneC(mat yiiList$,'N72173')
			fnAddOneC(mat yiiList$,'O114587')
			fnAddOneC(mat yiiList$,'O116896')
			fnAddOneC(mat yiiList$,'O127863')
			fnAddOneC(mat yiiList$,'O134342')
			fnAddOneC(mat yiiList$,'O134654')
			fnAddOneC(mat yiiList$,'O146442')
			fnAddOneC(mat yiiList$,'O146497')
			fnAddOneC(mat yiiList$,'O151168')
			fnAddOneC(mat yiiList$,'O151223')
			fnAddOneC(mat yiiList$,'O27093')
			fnAddOneC(mat yiiList$,'O32023')
			fnAddOneC(mat yiiList$,'O38793')
			fnAddOneC(mat yiiList$,'O43734')
			fnAddOneC(mat yiiList$,'O46640')
			fnAddOneC(mat yiiList$,'O88772')
			fnAddOneC(mat yiiList$,'O88774')
			fnAddOneC(mat yiiList$,'O89019')
			fnAddOneC(mat yiiList$,'O93005')
			fnAddOneC(mat yiiList$,'O96993')
			fnAddOneC(mat yiiList$,'P112653')
			fnAddOneC(mat yiiList$,'P112665')
			fnAddOneC(mat yiiList$,'P124255')
			fnAddOneC(mat yiiList$,'P124456')
			fnAddOneC(mat yiiList$,'P124514')
			fnAddOneC(mat yiiList$,'P127112')
			fnAddOneC(mat yiiList$,'P131411')
			fnAddOneC(mat yiiList$,'P137934')
			fnAddOneC(mat yiiList$,'P150208')
			fnAddOneC(mat yiiList$,'P184624')
			fnAddOneC(mat yiiList$,'P186691A')
			fnAddOneC(mat yiiList$,'P210838')
			fnAddOneC(mat yiiList$,'P223897')
			fnAddOneC(mat yiiList$,'P234607')
			fnAddOneC(mat yiiList$,'P236091')
			fnAddOneC(mat yiiList$,'P238997')
			fnAddOneC(mat yiiList$,'P276356')
			fnAddOneC(mat yiiList$,'P286807')
			fnAddOneC(mat yiiList$,'P286909')
			fnAddOneC(mat yiiList$,'P286986')
			fnAddOneC(mat yiiList$,'P288151')
			fnAddOneC(mat yiiList$,'P296516')
			fnAddOneC(mat yiiList$,'P296587')
			fnAddOneC(mat yiiList$,'P300600')
			fnAddOneC(mat yiiList$,'P304995')
			fnAddOneC(mat yiiList$,'P316627')
			fnAddOneC(mat yiiList$,'P317813')
			fnAddOneC(mat yiiList$,'P320568')
			fnAddOneC(mat yiiList$,'P320737')
			fnAddOneC(mat yiiList$,'P351729')
			fnAddOneC(mat yiiList$,'P353709')
			fnAddOneC(mat yiiList$,'P353735')
			fnAddOneC(mat yiiList$,'P404552')
			fnAddOneC(mat yiiList$,'P411881')
			fnAddOneC(mat yiiList$,'P431333')
			fnAddOneC(mat yiiList$,'P449795')
			fnAddOneC(mat yiiList$,'P457475')
			fnAddOneC(mat yiiList$,'P470910')
			fnAddOneC(mat yiiList$,'P471083')
			fnAddOneC(mat yiiList$,'P471412')
			fnAddOneC(mat yiiList$,'P471425')
			fnAddOneC(mat yiiList$,'P471907')
			fnAddOneC(mat yiiList$,'P472032')
			fnAddOneC(mat yiiList$,'P472090')
			fnAddOneC(mat yiiList$,'P472302')
			fnAddOneC(mat yiiList$,'P487466')
			fnAddOneC(mat yiiList$,'P487509')
			fnAddOneC(mat yiiList$,'P487510')
			fnAddOneC(mat yiiList$,'P487543')
			fnAddOneC(mat yiiList$,'P487569')
			fnAddOneC(mat yiiList$,'P61754')
			fnAddOneC(mat yiiList$,'P84058')
			fnAddOneC(mat yiiList$,'Q18728')
			fnAddOneC(mat yiiList$,'Q22286')
			fnAddOneC(mat yiiList$,'Q22455')
			fnAddOneC(mat yiiList$,'Q22980')
			fnAddOneC(mat yiiList$,'Q6621')
			fnAddOneC(mat yiiList$,'Q8045')
			fnAddOneC(mat yiiList$,'R115884')
			fnAddOneC(mat yiiList$,'R118745')
			fnAddOneC(mat yiiList$,'R133984')
			fnAddOneC(mat yiiList$,'R134026')
			fnAddOneC(mat yiiList$,'R143250')
			fnAddOneC(mat yiiList$,'R151647')
			fnAddOneC(mat yiiList$,'R159443')
			fnAddOneC(mat yiiList$,'R159526')
			fnAddOneC(mat yiiList$,'R159602')
			fnAddOneC(mat yiiList$,'R160022')
			fnAddOneC(mat yiiList$,'R160037')
			fnAddOneC(mat yiiList$,'R174557')
			fnAddOneC(mat yiiList$,'R222276')
			fnAddOneC(mat yiiList$,'R257948')
			fnAddOneC(mat yiiList$,'R263168')
			fnAddOneC(mat yiiList$,'R312922')
			fnAddOneC(mat yiiList$,'R337042')
			fnAddOneC(mat yiiList$,'R337073')
			fnAddOneC(mat yiiList$,'R337951')
			fnAddOneC(mat yiiList$,'R338246')
			fnAddOneC(mat yiiList$,'R338290')
			fnAddOneC(mat yiiList$,'R338796')
			fnAddOneC(mat yiiList$,'R338898')
			fnAddOneC(mat yiiList$,'R339492')
			fnAddOneC(mat yiiList$,'R358345')
			fnAddOneC(mat yiiList$,'R369987')
			fnAddOneC(mat yiiList$,'R371803')
			fnAddOneC(mat yiiList$,'R376541')
			fnAddOneC(mat yiiList$,'R376712')
			fnAddOneC(mat yiiList$,'R376735')
			fnAddOneC(mat yiiList$,'R376787')
			fnAddOneC(mat yiiList$,'R377190')
			fnAddOneC(mat yiiList$,'R380426')
			fnAddOneC(mat yiiList$,'R395430')
			fnAddOneC(mat yiiList$,'R402050A')
			fnAddOneC(mat yiiList$,'R404620')
			fnAddOneC(mat yiiList$,'R413090')
			fnAddOneC(mat yiiList$,'R415283')
			fnAddOneC(mat yiiList$,'R416792')
			fnAddOneC(mat yiiList$,'R432173')
			fnAddOneC(mat yiiList$,'R443858')
			fnAddOneC(mat yiiList$,'R465796')
			fnAddOneC(mat yiiList$,'R468311A')
			fnAddOneC(mat yiiList$,'R468370')
			fnAddOneC(mat yiiList$,'R485487')
			fnAddOneC(mat yiiList$,'R485511')
			fnAddOneC(mat yiiList$,'R485538')
			fnAddOneC(mat yiiList$,'R493112A')
			fnAddOneC(mat yiiList$,'R494874')
			fnAddOneC(mat yiiList$,'R496844')
			fnAddOneC(mat yiiList$,'R501117')
			fnAddOneC(mat yiiList$,'R506407')
			fnAddOneC(mat yiiList$,'R507409')
			fnAddOneC(mat yiiList$,'R512707')
			fnAddOneC(mat yiiList$,'R538039')
			fnAddOneC(mat yiiList$,'R538045')
			fnAddOneC(mat yiiList$,'R540374')
			fnAddOneC(mat yiiList$,'R540382')
			fnAddOneC(mat yiiList$,'R553881')
			fnAddOneC(mat yiiList$,'R554324')
			fnAddOneC(mat yiiList$,'R554565')
			fnAddOneC(mat yiiList$,'R554944')
			fnAddOneC(mat yiiList$,'R559020')
			fnAddOneC(mat yiiList$,'R571975')
			fnAddOneC(mat yiiList$,'R572018')
			fnAddOneC(mat yiiList$,'R572022')
			fnAddOneC(mat yiiList$,'R572149')
			fnAddOneC(mat yiiList$,'R572179')
			fnAddOneC(mat yiiList$,'R77478')
			fnAddOneC(mat yiiList$,'R98609')
			fnAddOneC(mat yiiList$,'S109648')
			fnAddOneC(mat yiiList$,'S112029')
			fnAddOneC(mat yiiList$,'S125006')
			fnAddOneC(mat yiiList$,'S160173')
			fnAddOneC(mat yiiList$,'S196261')
			fnAddOneC(mat yiiList$,'S214815')
			fnAddOneC(mat yiiList$,'S214907')
			fnAddOneC(mat yiiList$,'S218258')
			fnAddOneC(mat yiiList$,'S218363')
			fnAddOneC(mat yiiList$,'S224428')
			fnAddOneC(mat yiiList$,'S230154')
			fnAddOneC(mat yiiList$,'S231376')
			fnAddOneC(mat yiiList$,'S231531')
			fnAddOneC(mat yiiList$,'S231670')
			fnAddOneC(mat yiiList$,'S231703')
			fnAddOneC(mat yiiList$,'S238498A')
			fnAddOneC(mat yiiList$,'S248268')
			fnAddOneC(mat yiiList$,'S255939')
			fnAddOneC(mat yiiList$,'S256366')
			fnAddOneC(mat yiiList$,'S275599')
			fnAddOneC(mat yiiList$,'S276593')
			fnAddOneC(mat yiiList$,'S279342')
			fnAddOneC(mat yiiList$,'S279364')
			fnAddOneC(mat yiiList$,'S279367')
			fnAddOneC(mat yiiList$,'S279520')
			fnAddOneC(mat yiiList$,'S297651')
			fnAddOneC(mat yiiList$,'S318554')
			fnAddOneC(mat yiiList$,'S321448')
			fnAddOneC(mat yiiList$,'S342090')
			fnAddOneC(mat yiiList$,'S342095')
			fnAddOneC(mat yiiList$,'S358347')
			fnAddOneC(mat yiiList$,'S407074')
			fnAddOneC(mat yiiList$,'S408585')
			fnAddOneC(mat yiiList$,'S415699')
			fnAddOneC(mat yiiList$,'S417342')
			fnAddOneC(mat yiiList$,'S417347')
			fnAddOneC(mat yiiList$,'S435961')
			fnAddOneC(mat yiiList$,'S438804')
			fnAddOneC(mat yiiList$,'S444101')
			fnAddOneC(mat yiiList$,'S483486')
			fnAddOneC(mat yiiList$,'S491527')
			fnAddOneC(mat yiiList$,'S517538')
			fnAddOneC(mat yiiList$,'S532394')
			fnAddOneC(mat yiiList$,'S532395')
			fnAddOneC(mat yiiList$,'S532409')
			fnAddOneC(mat yiiList$,'S532410')
			fnAddOneC(mat yiiList$,'S532564')
			fnAddOneC(mat yiiList$,'S532698')
			fnAddOneC(mat yiiList$,'S534104')
			fnAddOneC(mat yiiList$,'S534963')
			fnAddOneC(mat yiiList$,'S534983')
			fnAddOneC(mat yiiList$,'S534985')
			fnAddOneC(mat yiiList$,'S535995')
			fnAddOneC(mat yiiList$,'S540431')
			fnAddOneC(mat yiiList$,'S550226')
			fnAddOneC(mat yiiList$,'S553320')
			fnAddOneC(mat yiiList$,'S558666')
			fnAddOneC(mat yiiList$,'S591633')
			fnAddOneC(mat yiiList$,'S594196')
			fnAddOneC(mat yiiList$,'S594214')
			fnAddOneC(mat yiiList$,'S594307')
			fnAddOneC(mat yiiList$,'S594331')
			fnAddOneC(mat yiiList$,'S594498')
			fnAddOneC(mat yiiList$,'S594540')
			fnAddOneC(mat yiiList$,'S594649')
			fnAddOneC(mat yiiList$,'S617516')
			fnAddOneC(mat yiiList$,'S627804')
			fnAddOneC(mat yiiList$,'S654725')
			fnAddOneC(mat yiiList$,'S655099')
			fnAddOneC(mat yiiList$,'S665838')
			fnAddOneC(mat yiiList$,'S691238')
			fnAddOneC(mat yiiList$,'S691258')
			fnAddOneC(mat yiiList$,'S705685')
			fnAddOneC(mat yiiList$,'S746470')
			fnAddOneC(mat yiiList$,'S751738')
			fnAddOneC(mat yiiList$,'S767706')
			fnAddOneC(mat yiiList$,'S770649')
			fnAddOneC(mat yiiList$,'S770778')
			fnAddOneC(mat yiiList$,'S796720')
			fnAddOneC(mat yiiList$,'S796746')
			fnAddOneC(mat yiiList$,'S844203A')
			fnAddOneC(mat yiiList$,'S844218')
			fnAddOneC(mat yiiList$,'S845172')
			fnAddOneC(mat yiiList$,'S870038')
			fnAddOneC(mat yiiList$,'S870233')
			fnAddOneC(mat yiiList$,'S870415')
			fnAddOneC(mat yiiList$,'S870916')
			fnAddOneC(mat yiiList$,'S871198')
			fnAddOneC(mat yiiList$,'S871293')
			fnAddOneC(mat yiiList$,'S871666')
			fnAddOneC(mat yiiList$,'S871815')
			fnAddOneC(mat yiiList$,'S871923')
			fnAddOneC(mat yiiList$,'S879328')
			fnAddOneC(mat yiiList$,'S879403')
			fnAddOneC(mat yiiList$,'S900096')
			fnAddOneC(mat yiiList$,'S900202')
			fnAddOneC(mat yiiList$,'S900297')
			fnAddOneC(mat yiiList$,'S900353')
			fnAddOneC(mat yiiList$,'S900392')
			fnAddOneC(mat yiiList$,'S900405')
			fnAddOneC(mat yiiList$,'S900460')
			fnAddOneC(mat yiiList$,'S900467')
			fnAddOneC(mat yiiList$,'S900489')
			fnAddOneC(mat yiiList$,'S900492')
			fnAddOneC(mat yiiList$,'T107388')
			fnAddOneC(mat yiiList$,'T111158')
			fnAddOneC(mat yiiList$,'T111254')
			fnAddOneC(mat yiiList$,'T114881')
			fnAddOneC(mat yiiList$,'T115131')
			fnAddOneC(mat yiiList$,'T117433')
			fnAddOneC(mat yiiList$,'T138398')
			fnAddOneC(mat yiiList$,'T138847')
			fnAddOneC(mat yiiList$,'T150606')
			fnAddOneC(mat yiiList$,'T169191')
			fnAddOneC(mat yiiList$,'T169620')
			fnAddOneC(mat yiiList$,'T180392')
			fnAddOneC(mat yiiList$,'T198980')
			fnAddOneC(mat yiiList$,'T205539')
			fnAddOneC(mat yiiList$,'T214923')
			fnAddOneC(mat yiiList$,'T214943')
			fnAddOneC(mat yiiList$,'T230744')
			fnAddOneC(mat yiiList$,'T233520')
			fnAddOneC(mat yiiList$,'T236662')
			fnAddOneC(mat yiiList$,'T239765')
			fnAddOneC(mat yiiList$,'T239781')
			fnAddOneC(mat yiiList$,'T239833')
			fnAddOneC(mat yiiList$,'T251951')
			fnAddOneC(mat yiiList$,'T253195')
			fnAddOneC(mat yiiList$,'T279068')
			fnAddOneC(mat yiiList$,'T296237')
			fnAddOneC(mat yiiList$,'T310064')
			fnAddOneC(mat yiiList$,'T311291')
			fnAddOneC(mat yiiList$,'T313592')
			fnAddOneC(mat yiiList$,'T322326')
			fnAddOneC(mat yiiList$,'T341465')
			fnAddOneC(mat yiiList$,'T351717')
			fnAddOneC(mat yiiList$,'T351768')
			fnAddOneC(mat yiiList$,'T351867')
			fnAddOneC(mat yiiList$,'T351910')
			fnAddOneC(mat yiiList$,'T363759')
			fnAddOneC(mat yiiList$,'T73941')
			fnAddOneC(mat yiiList$,'T77522')
			fnAddOneC(mat yiiList$,'U19362')
			fnAddOneC(mat yiiList$,'U20355')
			fnAddOneC(mat yiiList$,'U22928')
			fnAddOneC(mat yiiList$,'U25701')
			fnAddOneC(mat yiiList$,'U6610')
			fnAddOneC(mat yiiList$,'U6612')
			fnAddOneC(mat yiiList$,'U9837')
			fnAddOneC(mat yiiList$,'V101721')
			fnAddOneC(mat yiiList$,'V121732')
			fnAddOneC(mat yiiList$,'V122543')
			fnAddOneC(mat yiiList$,'V127585')
			fnAddOneC(mat yiiList$,'V148178')
			fnAddOneC(mat yiiList$,'V157146')
			fnAddOneC(mat yiiList$,'V165318')
			fnAddOneC(mat yiiList$,'W113459')
			fnAddOneC(mat yiiList$,'W117100')
			fnAddOneC(mat yiiList$,'W135588')
			fnAddOneC(mat yiiList$,'W149935')
			fnAddOneC(mat yiiList$,'W149992')
			fnAddOneC(mat yiiList$,'W150098')
			fnAddOneC(mat yiiList$,'W150364')
			fnAddOneC(mat yiiList$,'W150653')
			fnAddOneC(mat yiiList$,'W150802')
			fnAddOneC(mat yiiList$,'W150979')
			fnAddOneC(mat yiiList$,'W164029')
			fnAddOneC(mat yiiList$,'W179977')
			fnAddOneC(mat yiiList$,'W198974')
			fnAddOneC(mat yiiList$,'W206132')
			fnAddOneC(mat yiiList$,'W209719')
			fnAddOneC(mat yiiList$,'W218325')
			fnAddOneC(mat yiiList$,'W243895')
			fnAddOneC(mat yiiList$,'W247228')
			fnAddOneC(mat yiiList$,'W247796')
			fnAddOneC(mat yiiList$,'W255773')
			fnAddOneC(mat yiiList$,'W304346A')
			fnAddOneC(mat yiiList$,'W313240')
			fnAddOneC(mat yiiList$,'W313307')
			fnAddOneC(mat yiiList$,'W313350')
			fnAddOneC(mat yiiList$,'W313376')
			fnAddOneC(mat yiiList$,'W313474')
			fnAddOneC(mat yiiList$,'W313497')
			fnAddOneC(mat yiiList$,'W313616')
			fnAddOneC(mat yiiList$,'W313991')
			fnAddOneC(mat yiiList$,'W314480')
			fnAddOneC(mat yiiList$,'W315632')
			fnAddOneC(mat yiiList$,'W320389')
			fnAddOneC(mat yiiList$,'W323829')
			fnAddOneC(mat yiiList$,'W336415')
			fnAddOneC(mat yiiList$,'W338047')
			fnAddOneC(mat yiiList$,'W340517')
			fnAddOneC(mat yiiList$,'W341410')
			fnAddOneC(mat yiiList$,'W344807')
			fnAddOneC(mat yiiList$,'W349295')
			fnAddOneC(mat yiiList$,'W349299')
			fnAddOneC(mat yiiList$,'W349397')
			fnAddOneC(mat yiiList$,'W349535')
			fnAddOneC(mat yiiList$,'W349632')
			fnAddOneC(mat yiiList$,'W359949')
			fnAddOneC(mat yiiList$,'W365657')
			fnAddOneC(mat yiiList$,'W367758A')
			fnAddOneC(mat yiiList$,'W379998')
			fnAddOneC(mat yiiList$,'W380407')
			fnAddOneC(mat yiiList$,'W381533')
			fnAddOneC(mat yiiList$,'W383727')
			fnAddOneC(mat yiiList$,'W396171')
			fnAddOneC(mat yiiList$,'W396199')
			fnAddOneC(mat yiiList$,'W410253')
			fnAddOneC(mat yiiList$,'W426803')
			fnAddOneC(mat yiiList$,'W434286')
			fnAddOneC(mat yiiList$,'W445459')
			fnAddOneC(mat yiiList$,'W449946')
			fnAddOneC(mat yiiList$,'W451477')
			fnAddOneC(mat yiiList$,'W454160')
			fnAddOneC(mat yiiList$,'W455811')
			fnAddOneC(mat yiiList$,'W460584')
			fnAddOneC(mat yiiList$,'W464062')
			fnAddOneC(mat yiiList$,'W464497')
			fnAddOneC(mat yiiList$,'W507628')
			fnAddOneC(mat yiiList$,'W507804')
			fnAddOneC(mat yiiList$,'W508049')
			fnAddOneC(mat yiiList$,'W508065')
			fnAddOneC(mat yiiList$,'W508678')
			fnAddOneC(mat yiiList$,'W508695')
			fnAddOneC(mat yiiList$,'W524917')
			fnAddOneC(mat yiiList$,'W524948')
			fnAddOneC(mat yiiList$,'W524958')
			fnAddOneC(mat yiiList$,'W525173')
			fnAddOneC(mat yiiList$,'W525187')
			fnAddOneC(mat yiiList$,'W525207')
			fnAddOneC(mat yiiList$,'W525220')
			fnAddOneC(mat yiiList$,'W63288')
			fnAddOneC(mat yiiList$,'Y26614')
			fnAddOneC(mat yiiList$,'Y28263')
			fnAddOneC(mat yiiList$,'Y35033')
			fnAddOneC(mat yiiList$,'Y38989')
			fnAddOneC(mat yiiList$,'Y51586')
			fnAddOneC(mat yiiList$,'Y59425')
			fnAddOneC(mat yiiList$,'Z25000')
			fnAddOneC(mat yiiList$,'Z47438')
		! /r
	end if
	if srch(mat yiiList$,trim$(forwFileNo$))>0 then
		return$='Yes'
	else
		return$='No'
	end if
	fn_yesIfInList$=return$
fnend
def fn_finisAddRow(label$*256,value$*256)
	pr #255: '  <tr>'
	pr #255: '    <td>'&label$&'</td>'
	pr #255: '    <td align="right">'&value$&'</td>'
	pr #255: '  </tr>'
fnend
PgOf: ! r:
	pr #255: newpage
continue ! /r
Xit: fnXit
def fn_setup
	if ~setup then
		setup=1
		
		LIBRARY 'Library\OpenFile.wb': Fnopen_Active
		
		library 'library\clsUtil.wb': fnget_inf_claim
		library 'library\clsUtil.wb': fnAsci
		library 'library\clsUtil.wb': fnDate_rpt10$
		library 'library\clsUtil.wb': fnAllDebtors
		library 'library\clsUtil.wb': fnArray_to_range$,fnRange_to_array
		library 'prog2\intermnt.wb': fnInternal_data


		library 'S:\Core\Library.br': fnXit
		library 'S:\Core\Library.br': fnTos
		library 'S:\Core\Library.br': fnChk
		library 'S:\Core\Library.br': fnLbl,fnTxt
		library 'S:\Core\Library.br': fnCmdSet,fnAcs
		library 'S:\Core\Library.br': fnGetHandle
		! library "CLSUtil/Library": fnGetHandle
		library 'S:\Core\Library.br': fnMsgBox
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnAddOneN
		library 'S:\Core\Library.br': fntop

		library 'library\CLSUtil.wb': fnGetInf$
		library 'library\CLSUtil.wb': fncom
		library 'library\CLSUtil.wb': fnreport_path$,fnclaim_path$
		library 'library\CLSUtil.wb': fnget_claimfiles,fnclaim_scroll
		library 'library\CLSUtil.wb': fnrange_to_array,fnarray_to_range$
		library 'library\CLSUtil.wb': fnfix_bh,fnask_payref
		library 'library\CLSUtil.wb': fnunpack$
		library 'library\CLSUtil.wb': fnStime,fnStime$
		library 'library\CLSUtil.wb': fnMessageBox
		library 'library\CLSUtil.wb': fnList_Print
		library 'library\CLSUtil.wb': fncom
		library 'Prog2\Mast2.wb': fnsql_read

		library 'library\CLSUtil.wb': fnfix_bh
		library 'Prog2\Mast_SQL.wb': fnmast2_int_cache
		library 'library\CLSUtil.wb': fnAsk_file1
		library 'library\CLSUtil.wb': fnremove_arrayitem$,fnremove_arrayitem
		library 'library\CLSUtil.wb': fnget_file
		library 'library\CLSUtil.wb': fnmessagebox
		library 'library\CLSUtil.wb': fndisplay_top
		library 'Library\GridIO.wb': fnmulti_select,fnconfirm,fnconfirm_delete
		library 'Theme\Theme.wb': fnsection_divider
		library 'Library\SQL.wb': fnopen_sql_file
		library 'Library\SQL.wb': fnsql_setup$

		gosub Enum
		gosub SetupPrint

		dim masterData$(0)*60,masterDataN(0)
		dim masterFieldsc$(0)*20,masterFieldsN$(0)*20
		dim mFormAll$*2048
		fnsql_setup$('master',mat masterData$,mat masterDataN,mat masterFieldsc$,mat masterFieldsN$,mFormAll$)
		gosub EnumMaster
		
		gosub enumDebtor
		
		dim active_data$(0)*60,active_data(0)
		dim active_fieldsc$(0)*20,active_fieldsn$(0)*20
		dim active_formall$*2048
		execute "*SubProc "&fnsql_setup$('active',mat active_data$,mat active_data,mat active_fieldsc$,mat active_fieldsn$,active_formall$)

		dim oc$(2)*6
		oc$(1)='Open'
		oc$(2)='Closed'

	end if
fnend
def fn_listPrint(item$*2048)
	if ~listPrintSetup then
		listPrintSetup=1
		!                 if env$('Session_Rows')='' then let setenv('Session_Rows',24)
		!                 if env$('Session_Cols')='' then let setenv('Session_Cols',80)
		!                 
		!                 listPrint_sRow=int(val(env$('Session_Rows'))/2)
		!                 listPrint_Rows=val(env$('Session_Rows'))-listPrint_sRow-2
		!                 
		!                 listPrint_sCol=3
		!                 listPrint_Cols=val(env$('Session_Cols'))-listPrint_sCol-2
		! pr 'listPrint_sRow=';listPrint_sRow
		! pr 'listPrint_Rows=';listPrint_Rows
		! pr 'listPrint_sCol=';listPrint_sCol
		! pr 'listPrint_Cols=';listPrint_Cols
		! pause
	end if
	
	! pr f '3,2,c': item$
	Fnlist_Print(item$, handle,Lp_Cache_Disable, Cap_Etc$,listPrint_sRow,listPrint_sCol,listPrint_Rows,listPrint_Cols,listPrint_Border$)
fnend

include: cm\enum\common
include: cm\enum\master
include: cm\enum\debtor
include: cm\err
include: cm\print
