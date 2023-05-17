pr 'this program ('&program$&') is not intended to be run directly.'
end

def library fnHamsterFio(fileid$*64)
	if ~setup then fn_setup
	dim hfData$(0)*2048
	dim hfDataN(0)
	dim hfDataAll$(0)*2048
	hFile=fn_openFio(fileid$,mat hfData$,mat hfDataN)
	if hFile then
		dim hfLabel$(0)*128
		fn_hfLayoutRead(fnFileIoLayoutPath$(fileid$),mat hfDataAll$,mat hfLabel$,mat hfFieldType$,mat hfStorageLen,mat hfMask$,mat hfFieldLen)
		fnHamster2b(fileid$,mat hfLabel$,mat hfFieldLen,hFile,mat hfDataAll$,mat hfFieldType$,mat hfStorageLen,mat hfMask$,mat startingPosition,mat comboBox$)
	end if
fnend

def library fnFioLayoutRead(fileio$*64,mat dataAll$,mat label$,mat fieldType$,mat storageLen,mat mask$,mat fieldLen)
	if ~setup then fn_setup
	fnFioLayoutRead=fn_hfLayoutRead(fnFileIoLayoutPath$(fileio$),mat dataAll$,mat label$,mat fieldType$,mat storageLen,mat mask$,mat fieldLen)
fnend

def fn_hfLayoutRead(hfLayoutFilename$*256,mat hfDataAll$,mat hfLabel$,mat hfFieldType$,mat hfStorageLen,mat hfMask$,mat hfFieldLen; ___,limit_to_list$,posComma,posComboFio,posComboF,posComboA,hamsterColumn,hfItem,line$*1024,past_header)
	dim hfItem$(0)*1024
	open #hLay=fnH: 'name='&hfLayoutFilename$,d,i
	mat hfDataAll$(0) : mat hfLabel$(0) : mat hfFieldType$(0) : mat hfStorageLen(0) : mat hfMask$(0) : mat hfFieldLen(0)
	dim comboBox$(0,9)*256 ! comboBox$(0,8)*256<-worked most of the time.    comboBox$(60,64)*256
	mat comboBox$(0,udim(mat comboBox$,2)) ! mat comboBox$=('') !
	do
		linput #hLay: line$
		if line$(1:6)='======' then past_header=1
	loop until past_header
	do
		linput #hLay: line$ eof hfEofhLay
		if trim$(line$)(1:1)<>'!' and trim$(line$)<>'' and trim$(line$)(1:1)<>'#' then
			line$=srep$(line$,chr$(9),'  ')
			! r: basic stuff like file layout and text
			str2mat(line$,mat hfItem$,',')
			hfItem+=1
			fnAddOneC(mat hfDataAll$,'')
			fnAddOneC(mat hfLabel$,trim$(hfItem$(2)))
			hfItem$(3)=trim$(hfItem$(3))
			posSpace=pos(hfItem$(3),' ')
			fnAddOneC(mat hfFieldType$,uprc$(trim$(hfItem$(3)(1:posSpace-1))))
			hfItem$(3)(1:posSpace-1)=''
			hfItem$(3)=trim$(hfItem$(3))
			fnAddOneN(mat hfStorageLen,val(hfItem$(3)))
			fnAddOneC(mat hfMask$,'') ! accumulated last
			tmp=int(hfStorageLen(hfItem))
			if uprc$(hfFieldType$(hfItem))='PD' then tmp=tmp*2-1
			if uprc$(hfFieldType$(hfItem))='V' then hfFieldType$(hfItem)='C'
			fnAddOneN(mat hfFieldLen,tmp)
			! /r
			if udim(mat hfItem$)=> 4 then ! r: If at least 4 columns than try to process the 5th(or 4th, if no 5th)

				if udim(mat hfItem$)=>5 then hamsterColumn=5 else hamsterColumn=4

				if pos(lwrc$(hfItem$(hamsterColumn)),' required=true')>0 then hfMask$(hfitem)=str$(val(hfMask$(hfitem))+1000)
				do while pos(hfItem$(hamsterColumn),'= ')>0
					hfItem$(hamsterColumn)=srep$(hfItem$(hamsterColumn),'= ','=')
				loop
				hfItem$(hamsterColumn)=trim$(hfItem$(hamsterColumn))
				if hfItem$(hamsterColumn)(1:1)='!' then hfItem$(hamsterColumn)(1:1)=''
				hfItem$(hamsterColumn)=' '&trim$(hfItem$(hamsterColumn))&' '
				posMask=pos(lwrc$(hfItem$(hamsterColumn)),' mask=')
				posComboF=pos(lwrc$(hfItem$(hamsterColumn)),' combof(')
				posComboA=pos(lwrc$(hfItem$(hamsterColumn)),' comboa(')
				posComboFio=pos(lwrc$(hfItem$(hamsterColumn)),' combofio(')
				posGlAccount=pos(lwrc$(hfItem$(hamsterColumn)),' glaccount')


				if hamsterColumn=5 and hfItem$(hamsterColumn-1)='date(mmddyy)' then
					! todo: seems like i should set a date format for the grid here
				else if posMask>0 then ! glaccount selection box
					if mask$='glaccount' then
					else
						! r: masked text box (except glaccount)
						! pr hfItem$(hamsterColumn) : pause
						posSpaceAfter=pos(hfItem$(hamsterColumn),' ',posMask+1)
						! if posSpaceAfter<=0 then pr 'XYXY123' : pause
						! pr 'posSpaceAfter=';posSpaceAfter : pause
						mask$=lwrc$(hfItem$(hamsterColumn)(posMask+6:posSpaceAfter-1))
						if mask$='currency' or mask$='pointtwo' then
							tmp=32
						! else if mask$='glaccount' then !   old unused format: mask=glnumber
						! 	tmp=53          !  this could also need to be 50, 51 or 52  jb 6/7/2021
						! 	fnGetUseDeptAndSub(useDept,useSub)
						! 	if ~useDept and ~useSub then
						! 		tmp=50
						! 	else if useDept and ~useSub then
						! 		tmp=51
						! 	else if ~useDept and useSub then
						! 		tmp=52
						! 	else if useDept and useSub then
						! 		tmp=53
						! 	end if
						! 	! see fnAgl$ logic in
						! 	! C:\ACS\Dev-5\Core\fn\agl$.br.brs
						! 	! pr 'found it -aaa in hamster fio-  mask$='&mask$ : pause
						else if mask$='mmddyy' then
							tmp=1
						else if mask$='ccyymmdd' then
							tmp=3
						else if mask$='number' then
							tmp=30
						else
							tmp=val(hfItem$(hamsterColumn)(posMask+6:posSpaceAfter))
						end if
						if tmp=0 then
							if hfFieldType$(hfItem)='N' or hfFieldType$(hfItem)='PD' or hfFieldType$(hfItem)='G' and fp(hfStorageLen(hfItem))=2 then
								tmp=32
							end if
						end if
						hfMask$(hfitem)=str$(val(hfMask$(hfitem))+tmp)
						! /r
					end if
				else if posGlAccount then
				
					posSpaceAfter=pos(hfItem$(hamsterColumn),' ',posMask+1)
					mask$=lwrc$(hfItem$(hamsterColumn)(posMask+6:posSpaceAfter-1))
				
					hfMask$(hfitem)='glaccount'
					line$='comboFio(GL Account)'
					posComboFio=1
					mat comboBox$(hfItem,udim(mat comboBox$,2))
					goto ToComboFio
				else if posComboF>0 or posComboA>0 or posComboFio>0 then
				! if env$('acsDeveloper')<>'' then debugCombo=1
					! r: comoboboxes
					mat comboBox$(hfItem,udim(mat comboBox$,2))
					posComboA=pos(lwrc$(line$),'comboa(')
					posComboF=pos(lwrc$(line$),'combof(')
					posComboFio=pos(lwrc$(line$),'combofio(')
					if posComboF>0 then ! r:
						posComboX=posComboF
						comboBox$(hfItem,1)='ComboF'                 ! 'ComboF'
						! pr "comboBox$(hfItem,1)='ComboF'"
						! /r
					else if posComboA then ! r:
						posComboX=posComboA
						comboBox$(hfItem,1)='ComboA'                 ! 'ComboA'
						! pr "comboBox$(";hfItem;",1)='ComboA'"
						! /r
					else if posComboFio>0 then ! r:  support for comboFio(CO Table)   and   comboFio(CO Table,2) (adds [All] option)
						ToComboFio: !
						line$=srep$(line$,'ComboFio','combofio')
						line$=srep$(line$,'comboFio','combofio')
						line$=srep$(line$,'COMBOFIO','combofio')
						line$=srep$(line$,'Combofio','combofio')
						dim cfTable$*256
						cfTable$=line$(posComboFio+len('comboFio('):pos(line$,')',posComboFio)-1)
						posComma=pos(cfTable$,',')
						if posComma>0 then
							limit_to_list$=trim$(cfTable$(posComma+1:inf))
							cfTable$(posComma:inf)=''
							! pr 'cfTable$='&cfTable$
							! pr 'limit_to_list$='&limit_to_list$
							! pause
						end if
						comboBox$(hfItem,1)='ComboF'
						! r: set mat comboBox$(hfItem,2-8) based on cfTable$ (fileio layout id)
							! fn_addToMatComboBox(x,'ComboF,data_file$,key_pos,key_len,desc_pos,desc_len,index_file$,limit_to_list)
							! r: comboBox$(x,y) Legend for ComoboF
								! X= item number
								! Y=
								!  comboBox$(hfItem,2)  =  data_file$
								!  comboBox$(hfItem,3)  =  key_pos
								!  comboBox$(hfItem,4)  =  key_len ! Key Length
								!  comboBox$(hfItem,5)  =  desc_pos
								!  comboBox$(hfItem,6)  =  desc_len ! Description Length
								!  comboBox$(hfItem,7)  =  index_file$
								!  comboBox$(hfItem,8)  =  limit_to_list
							! /r
							! r: S:\Core\Data
								! S:\Core\Data
								if fn_amc('CO TransactionCode'              	,'S:\Core\Data\TransactionCode.dat    ,1, 1, 2,18,S:\Core\Data\TimTransactionCode.idx ') then goto CfGotIt
								if fn_amc('GL TransactionType'              	,'S:\Core\Data\GL TransactionType.dat ,1, 1, 2,18,S:\Core\Data\GL TransactionType.idx ') then goto CfGotIt
								if fn_amc('CO Billing Method'               	,'S:\Core\Data\Billing Method.dat     ,1, 1, 2,40,S:\Core\Data\Billing Method.idx     ') then goto CfGotIt
								! s:\Core\Data\acsllc
								if fn_amc('CO Client'                        	,'S:\Core\Data\acsllc\Client.h[cno]   ,1, 5, 6,30,S:\Core\Data\acsllc\Client-Idx.h[cno]') then goto CfGotIt
								if fn_amc('CO Provider'                     	,'S:\Core\Data\acsllc\Provider.h[cno] ,1,11,12,64,S:\Core\Data\acsllc\Provider-Idx.h[cno]') then goto CfGotIt
								if fn_amc('CO Systems'                      	,'S:\Core\Data\acsllc\Systems.h420    ,1, 2, 3,50,S:\Core\Data\acsllc\Systems-Idx.h420') then goto CfGotIt
								if fn_amc('CO Systems 2'                    	,'S:\Core\Data\acsllc\acsSystems.dat  ,1,16,17,64,S:\Core\Data\acsllc\acsSystems.idx') then goto CfGotIt
								if fn_amc('Client Billing Time Frame'      	,'S:\Core\Data\acsllc\TimeFrame.h[cno],1, 2, 3,50,S:\Core\Data\acsllc\TimeFrame-Idx.h[cno]') then goto CfGotIt
								if fn_amc('Client Billing Category'        	,'S:\Core\Data\acsllc\Category.h[cno] ,1, 3, 4,30,S:\Core\Data\acsllc\Category_Id_Idx.h[cno]') then goto CfGotIt
							! /r
							! r: [Q]
								! [Q]\Data
								if fn_amc('CO City State Zip' 	,'[Q]\Data\CityStZip.dat              ,1,30, 0, 0,[Q]\Data\CityStZip.idx') then goto CfGotIt
								! [Q]\GLmstr
								if fn_amc('GL Account'         	,'[Q]\GLmstr\glMstr.h[cno]            ,1,12,13,50,[Q]\GLmstr\GLIndex.h[cno]') then goto CfGotIt
								if fn_amc('GL Period Names'    	,'[Q]\GLmstr\Period.h[cno]            ,1, 2, 3,30,[Q]\GLmstr\Period-Idx.h[cno]') then goto CfGotIt
								! [Q]\UBmstr
								if fn_amc('UB Customer'        	,'[Q]\UBmstr\Customer.h[cno]          ,1,10,41,30,[Q]\UBmstr\ubIndex.h[cno]') then goto CfGotIt
								if fn_amc('U4 Meter Type'      	,'[Q]\UBmstr\MeterType.h[cno]         ,1, 5, 6,40,[Q]\UBmstr\MeterTypeIdx.h[cno]') then goto CfGotIt
								! [Q]\PRmstr
								if fn_amc('PR Employee'        	,'[Q]\PRmstr\Employee.h[cno]          ,1, 8, 9,30,[Q]\PRmstr\EmployeeIdx-no.h[cno]') then goto CfGotIt
								
							! /r
							pr 'unrecognized table passed to ComboFio'
							pr 'unrecognized table='&cfTable$
							pr 'more code suggested'
							pr 'somewhere around line 200 in '
							pr 'C:\ACS\Dev-5\Core\HamsterFio.br.brs'
							pause
							CfGotIt: !
							
							! for x=1 to 8
							! 	pr 'comboBox$(hfItem,x)='&comboBox$(hfItem,x)
							! nex x
							! pause

						! /r
						! /r
					else
						pr 'unexpected comboBox err in hamsterFio line$="'&line$&'"' : pause
					end if
					if posComboA or posComboF then
						! r: build mat comboBox$ for ComboA and ComboF
						dim cfItem$(0)*128
						mat cfItem$(0)
						str2mat(line$(posComboX+len('comboX('):pos(line$,')',posComboX)-1),mat cfItem$,',')
						if debugCombo then pr line$ : pr mat cfItem$ : pause
						! r: comboBox$(x,y) Legend for ComoboF
							! X= item number
							! Y= 1  (id)
							!    2  data_file$
							!    3  key_pos
							!    4  key_len ! Key Length
							!    5  desc_pos
							!    6  desc_len ! Description Length
							!    7  index_file$
							!    8  limit_to_list
						! /r
						for cbIndex=2 to udim(mat cfItem$)
							cfItem$(cbIndex)=trim$(cfItem$(cbIndex),"'")
							cfItem$(cbIndex)=trim$(cfItem$(cbIndex),'"')
							! cfItem$(cbIndex)=srep$(cfItem$(cbIndex),"env$('Q')",env$('Q'))
							! cfItem$(cbIndex)=srep$(cfItem$(cbIndex),"env$('cno')",env$('cno'))
							! cfItem$(cbIndex)=srep$(cfItem$(cbIndex),'[Q]',env$('Q'))
							! cfItem$(cbIndex)=srep$(cfItem$(cbIndex),'[cno]',env$('cno'))
							cfItem$(cbIndex)=fnSrepEnv$(cfItem$(cbIndex))

							! use mask=glaccount instead    if pos(cfItem$(cbIndex),'*custom:GL Account*')>0 then
							!                               	debugCombo=1
							!                               	pause
							!                               end if

							if pos(cfItem$(cbIndex),'*custom:UB ServiceCodes*')>0 then
								dim serviceName$(10)*20
								dim serviceCode$(10)*2
								fnGetServices(mat serviceName$,mat serviceCode$)
								dim tmpList$*256
								tmpList$=''
								for scItem=1 to udim(mat serviceCode$)
									if trim$(serviceCode$(scItem))<>'' then
										if scItem>1 then tmpList$&=','
										tmpList$&=serviceCode$(scItem)
									end if
								nex scItem
								cfItem$(cbIndex)=srep$(cfItem$(cbIndex),'*custom:UB ServiceCodes*',tmpList$)
							else if pos(cfItem$(cbIndex),'*custom:UB ServiceCodes Metered*')>0 then
								fnGetServiceCodesMetered(mat serviceCodeMetered$)
								mat2str(mat serviceCodeMetered$,tmpList$,',')
								cfItem$(cbIndex)=srep$(cfItem$(cbIndex),'*custom:UB ServiceCodes Metered*',tmpList$)
							else if pos(cfItem$(cbIndex),'*custom:U4 Devices Enabled*')>0 then
								dim deviceName$(0)*20
								fnHandHeldList(mat deviceName$)
								mat2str(mat deviceName$,tmpList$,',')
								cfItem$(cbIndex)=srep$(cfItem$(cbIndex),'*custom:U4 Devices Enabled*',tmpList$)

							end if
							comboBox$(hfItem,cbIndex)=cfItem$(cbIndex)
							if debugCombo then pr 'comboBox$(';hfItem;',';cbIndex;') to "'&cfItem$(cbIndex)&'"'
						nex cbIndex
						if debugCombo then  pause
						! /r
					end if
					! /r
				end if
			end if
			! /r
		end if
	loop
	hfEofhLay: !

	! r: build mat startingPosition from mat hfStorageLen
		startingPosition(1)=1
		mat startingPosition(udim(mat hfStorageLen))
		for item=2 to udim(mat startingPosition)
			startingPosition(item)=startingPosition(item-1)+int(hfStorageLen(item-1))
		nex item
	! /r


	close #hLay:
fnend
	def fn_amc(table$*128,csvList$*1024; ___,returnN) ! uses local: hfItem,limit_to_list$,cfTable$
		if cfTable$=table$ then
			if limit_to_list$='' then limit_to_list$='0'
			fn_addToMatComboBox(hfItem,'comboF,'&csvList$&','&limit_to_list$)
			returnN=1
		end if
		fn_amc=returnN
	fnend
		def fn_addToMatComboBox(x,csvList$*1024; ___,y)
			dim atmItem$(0)*512
			mat atmItem$(0)
			str2mat(csvList$,mat atmItem$,',')
			for y=1 to 8
				comboBox$(x,y)=rtrm$(atmItem$(y))
			nex y
		fnend
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
	end if
fnend
include: fn_open
include: ertn