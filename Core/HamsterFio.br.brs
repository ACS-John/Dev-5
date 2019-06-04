def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnAddOneC,fnOpenFile,fnAddOneN,fnHamster,fngethandle,fnerror,fnSrepEnv$
		dim form$(0)*256
		dim hfLabel$(0)*128
	end if
fnend
def library fnHamsterFio(fileid$*64)
	if ~setup then let fn_setup
	dim defaultFileLayoutPath$*256
	
	if env$('client')='Brumbaugh' then
		defaultFileLayoutExtension$=''
		defaultFileLayoutPath$="S:\FileLay\"
	else
		defaultFileLayoutExtension$='.fio'
		defaultFileLayoutPath$="S:\Core\FileIO\Layout\"
	end if
	dim hfData$(0)*2048
	dim hfDataN(0)
	dim hfDataAll$(0)*2048
	hFile=fn_open(fileid$,mat hfData$,mat hfDataN,mat form$)
	if hFile then
		fn_hfLayoutRead(defaultFileLayoutPath$&fileid$&defaultFileLayoutExtension$,mat hfDataAll$,mat hfLabel$,mat hfFieldType$,mat hfStorageLen,mat hfMask,mat hfFieldLen)
		fnHamster(fileid$,mat hfLabel$,mat hfFieldLen,hFile,mat hfDataAll$,mat hfFieldType$,mat hfStorageLen,mat hfMask,mat startingPosition,mat comboBox$)
	end if
fnend
def fn_hfLayoutRead(hfLayoutFilename$*256,mat hfDataAll$,mat hfLabel$,mat hfFieldType$,mat hfStorageLen,mat hfMask,mat hfFieldLen)
	dim line$*1024,hfItem$(0)*1024
	open #hLay:=fngethandle: 'name='&hfLayoutFilename$,d,i
	past_header=0
	hfItem=0
	mat hfDataAll$(0) : mat hfLabel$(0) : mat hfFieldType$(0) : mat hfStorageLen(0) : mat hfMask(0) : mat hfFieldLen(0) 
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
			fnAddOneN(mat hfMask,0) ! accumulated last
			tmp=int(hfStorageLen(hfItem))
			if uprc$(hfFieldType$(hfItem))='PD' then tmp=tmp*2-1
			fnAddOneN(mat hfFieldLen,tmp)
			! /r
			if udim(mat hfItem$)=> 4 then
				if pos(lwrc$(hfItem$(4)),' required=true')>0 then hfMask(hfitem)+=1000
				do while pos(hfItem$(4),'= ')>0
					hfItem$(4)=srep$(hfItem$(4),'= ','=')
				loop 
				hfItem$(4)=trim$(hfItem$(4))
				if hfItem$(4)(1:1)='!' then hfItem$(4)(1:1)=''
				hfItem$(4)=' '&trim$(hfItem$(4))&' '
				posMask=pos(lwrc$(hfItem$(4)),' mask=')
				posComboF=pos(lwrc$(hfItem$(4)),' combof(') 
				posComboA=pos(lwrc$(hfItem$(4)),' comboa(') 
				if posMask>0 then 
					! r: masked text box
					posSpaceAfter=pos(hfItem$(4),' ',posMask+1)
					! pr hfItem$(4) : pause
					mask$=lwrc$(hfItem$(4)(posMask+6:posSpaceAfter-1))
					if mask$='currency' or mask$='pointtwo' then
						tmp=32
					else if mask$='glnumber' then
						tmp=53
					else if mask$='mmddyy' then
						tmp=1
					else if mask$='ccyymmdd' then
						tmp=3
					else if mask$='number' then
						tmp=30
					else
						tmp=val(hfItem$(4)(posMask+6:posSpaceAfter))
					end if
					if tmp=0 then
						if hfFieldType$(hfItem)='N' or hfFieldType$(hfItem)='PD' or hfFieldType$(hfItem)='G' and fp(hfStorageLen(hfItem))=2 then
							tmp=32
						end if
					end if
					hfMask(hfitem)+=tmp
					! /r
				else if posComboF>0 or posComboA>0 then
				! if env$('acsDeveloper')<>'' then debugCombo=1
					! r: comoboboxes
					mat comboBox$(hfItem,udim(mat comboBox$,2))
					posComboA=pos(lwrc$(line$),'comboa(')
					posComboF=pos(lwrc$(line$),'combof(')
					if posComboF>0 then 
						posComboX=posComboF 
						comboBox$(hfItem,1)='ComboF'                 ! 'ComboF'
						! pr "comboBox$(hfItem,1)='ComboF'"
					else if posComboA then
						posComboX=posComboA
						comboBox$(hfItem,1)='ComboA'                 ! 'ComboA'
						! pr "comboBox$(";hfItem;",1)='ComboA'"
					else 
						pr 'unexpected comboBox err in hamsterFio line$="'&line$&'"' : pause
					end if
					dim cfItem$(0)*128
					mat cfItem$(0)
					str2mat(line$(posComboX+len('comboX('):pos(line$,')',posComboX)-1),mat cfItem$,',')
					if debugCombo then pr line$ : pr mat cfItem$ : pause
					! r: comboBox$(x,y) Legend for ComoboF
						! X= item number
						! Y=
						!    data_file$ 
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
						if pos(cfItem$(cbIndex),'*custom:UB ServiceCodes*')>0 then
							library 'S:\Core\Library': fnget_services
							dim serviceName$(10)*20
							dim serviceCode$(10)*2
							fnget_services(mat serviceName$,mat serviceCode$)
							dim serviceCsv$*256
							serviceCsv$=''
							for scItem=1 to udim(mat serviceCode$)
								if trim$(serviceCode$(scItem))<>'' then
									if scItem>1 then serviceCsv$(inf:inf)=','
									serviceCsv$(inf:inf)=serviceCode$(scItem)
								end if
							nex scItem
							cfItem$(cbIndex)=srep$(cfItem$(cbIndex),'*custom:UB ServiceCodes*',serviceCsv$)
						else if pos(cfItem$(cbIndex),'*custom:UB ServiceCodes Metered*')>0 then
							library 'S:\Core\Library': fnGetServiceCodesMetered
							fnGetServiceCodesMetered(mat serviceCodeMetered$)
							mat2str(mat serviceCodeMetered$,serviceCsv$,',')
							cfItem$(cbIndex)=srep$(cfItem$(cbIndex),'*custom:UB ServiceCodes Metered*',serviceCsv$)
						end if
						comboBox$(hfItem,cbIndex)=cfItem$(cbIndex) 
						if debugCombo then pr 'comboBox$(';hfItem;',';cbIndex;') to "'&cfItem$(cbIndex)&'"'
					nex cbIndex
					if debugCombo then  pause
				end if 
			end if
			! /r
		end if
	loop
	hfEofhLay: !
	close #hLay:
fnend
include: fn_open
include: ertn