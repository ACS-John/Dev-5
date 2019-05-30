
! todo:   FinSum  (a trust and a cost) - see:    C:\ACS\Dev-5\Collection-Master Add-On\Import Finsum.br.brs

! first run Get images for claims from DB3 and put them into DB1


fn_setup
on error goto Error_Hanler
fnTop(program$)
enableMoveFiles=0
dim pathFrom$*256
pathFrom$='\\192.168.111.44\Data1\CLS_LOCAL\CMSS' ! test Db3
! pathFrom$='\\192.168.111.44\Data\CLSINC2' ! real db3
dim imagePathFrom$*128
imagePathFrom$='F:\IMASTER2\IMAGES\CM\'
dim imagePathTo$*128
imagePathTo$  ='S:\CM\'

dim filenoList$(0)*8
mat filenoList$(0)
fnasci(env$('at')&'claimsToGetFromDB3.txt',mat filenoList$)
! fnAddOneC(mat filenoList$,'firstOne')
! fnAddOneC(mat filenoList$,'secondUn')
mat hitCount(udim(mat filenoList$))
mat hitCount=(0)

oc_first=1 : oc_last=2 !  both open and closed
for oc=oc_first to oc_last
	if oc=1 then ! open
		open #hImagesFrom:=fngethandle: 'NAME='&pathFrom$&'\data\IMAGES.INT,kfname='&pathFrom$&'\data\IMAGES.IDX,shr',internal,input,keyed
		open   #hImagesTo:=fngethandle: "NAME=IMAGES.INT//6,kfname=IMAGES.IDX//6,shr",internal,outin,keyed
		! kps=1/9/13/76 kln=8/4/4/4
	else if oc=2 then ! closed
		open #hImagesFrom:=fngethandle: 'NAME='&pathFrom$&'\history\IMAGES.INT,kfname='&pathFrom$&'\history\IMAGES.IDX,shr',internal,input,keyed
		open   #hImagesTo:=fngethandle: "NAME=IMAGES.INT//1,kfname=IMAGES.IDX//1,shr",internal,outin,keyed 
	end if
	readCount=0
	for filenoItem=1 to udim(mat filenoList$)
		fileno$=filenoList$(filenoItem)
		restore #hImagesFrom,search=>fileno$:
		do
			read  #hImagesFrom,using images_formall$: mat images_data$,mat images_data eof EoImagesFrom
			readCount+=1
			! pr 'just read "'&images_data$(images_fileno)&'" when looking for "'&fileno$&'"' : pause
			if rtrm$(images_data$(images_fileno))=rtrm$(fileno$) then
			! filenoItem=srch(mat filenoList$,trim$(images_data$(images_fileno)))
			! if filenoItem>0 then
				images_data$(images_npath)=srep$(images_data$(images_npath),imagePathFrom$,imagePathTo$)
				write #hImagesTo,using images_formall$: mat images_data$,mat images_data
				hitCount(filenoItem)+=1
				if enableMoveFiles then
					pr 'move files is not yet written   [npath]\[nfile]*.*   '
					pause
				end if
					! no need to move the actual image files at this time.
			end if
		loop while rtrm$(images_data$(images_fileno))=rtrm$(fileno$)
	nex filenoItem
	EoImagesFrom: !
	close #hImagesFrom:
	close #hImagesTo:
	pr 'Stage: '&oc$(oc)
	pr '  read count: '&str$(readCount) : readCount=0
	pr '  Claims searhed for: '&str$(udim(mat filenoList$))
	pr '  total hits: '&str$(sum(mat hitCount)) : mat hitCount=(0)
nex oc
pr 'Type GO and hit Enter to continue.'
pause
goto Xit
Xit: fnXit
def fn_setup
	if ~setup then
		setup=1
		
		library 'library\clsUtil.wb': fnget_inf_claim
		library 'library\clsUtil.wb': fnAsci
		library 'library\clsUtil.wb': fnDate_rpt10$
		library 'library\clsUtil.wb': fnAllDebtors
		! library 'library\clsUtil.wb': fnArray_to_range$
		library 'prog2\intermnt.wb': fnInternal_data


		library 'S:\Core\Library.br': fnXit
		library 'S:\Core\Library.br': fnTos
		library 'S:\Core\Library.br': fnChk
		library 'S:\Core\Library.br': fnLbl,fnTxt
		library 'S:\Core\Library.br': fnCmdSet,fnAcs
		library 'S:\Core\Library.br': fnGetHandle
		library 'S:\Core\Library.br': fnMsgBox
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnAddOneN
		library 'S:\Core\Library.br': fntop

		library 'library\CLSUtil.wb': fnList_Print
		library 'library\CLSUtil.wb': fncom

		library 'Library\SQL.wb': fnsql_setup$

		dim images_data$(0)*128,images_data(0)
		dim images_fieldsc$(0)*20,images_fieldsn$(0)*20
		dim images_formall$*2048
		execute "*SubProc "&fnsql_setup$('images',mat images_data$,mat images_data,mat images_fieldsc$,mat images_fieldsn$,images_formall$)

		dim oc$(2)*6
		oc$(1)='Open'
		oc$(2)='Closed'
	end if
fnend
include: cm\err